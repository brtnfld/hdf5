/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: Appended signature verification for HDF5 plugins
 *
 *          Implements verification of plugins using RSA signatures appended
 *          to the end of the binary file. This approach:
 *            - Works on all platforms (Linux, Windows, macOS)
 *            - No ELF/PE parsing required
 *            - No external tools needed
 *            - Simple and secure
 */

/****************/
/* Module Setup */
/****************/

#include "H5PLmodule.h" /* This source code file is part of the H5PL module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions            */
#include "H5Eprivate.h"  /* Error handling               */
#include "H5PLpkg.h"     /* Plugin                       */
#include "H5PLsig.h"     /* Signature format             */
#include "H5MMprivate.h" /* Memory management            */
#include "H5encode.h"    /* Endianness conversion        */

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
#include <openssl/err.h>

/*******************/
/* Local Variables */
/*******************/

/* Cached public key for signature verification
 * The public key is parsed from PEM string once and reused across all plugin verifications.
 * This provides significant performance improvement when loading multiple plugins.
 */
static EVP_PKEY *H5PL_cached_public_key_g      = NULL;
static bool      H5PL_public_key_initialized_g = false;

/*-------------------------------------------------------------------------
 * Function:    H5PL__read_file_data
 *
 * Purpose:     Portable file read with EINTR retry and safe chunking
 *
 *              Follows HDF5's established pattern from H5FDsec2.c for safe,
 *              portable I/O that handles:
 *                - EINTR interruptions (retry)
 *                - Partial reads (loop until complete)
 *                - Platform I/O size limits (H5_POSIX_MAX_IO_BYTES)
 *                - pread support (when available for better concurrency)
 *                - Detailed error messages with errno info
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__read_file_data(int fd, HDoff_t offset, void *buf, size_t size, const char *filename)
{
    size_t         left_to_read = size;
    unsigned char *read_ptr     = (unsigned char *)buf;
    herr_t         ret_value    = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(buf);
    assert(filename);

#ifndef H5_HAVE_PREADWRITE
    /* Seek to the correct location (if we don't have pread) */
    if (HDlseek(fd, offset, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_SEEKERROR, FAIL, "unable to seek to offset %llu in plugin file '%s'",
                    (unsigned long long)offset, filename);
#endif /* H5_HAVE_PREADWRITE */

    /* Read data in chunks, following HDF5's established I/O pattern from H5FDsec2.c */
    while (left_to_read > 0) {
        h5_posix_io_t     bytes_in   = 0;
        h5_posix_io_ret_t bytes_read = -1;

        /* Respect platform I/O size limits to avoid undefined behavior */
        if (left_to_read > H5_POSIX_MAX_IO_BYTES)
            bytes_in = H5_POSIX_MAX_IO_BYTES;
        else
            bytes_in = (h5_posix_io_t)left_to_read;

        /* Retry on EINTR (interrupted system call), use pread if available */
        do {
#ifdef H5_HAVE_PREADWRITE
            bytes_read = HDpread(fd, read_ptr, bytes_in, offset);
            if (bytes_read > 0)
                offset += bytes_read;
#else
            bytes_read = HDread(fd, read_ptr, bytes_in);
            if (bytes_read > 0)
                offset += bytes_read;
#endif /* H5_HAVE_PREADWRITE */
        } while (-1 == bytes_read && EINTR == errno);

        if (bytes_read < 0) {
            int    myerrno = errno;
            time_t mytime  = time(NULL);
            char   timebuf[26]; /* Buffer for thread-safe ctime_r (POSIX requires 26 bytes) */

            ctime_r(&mytime, timebuf);

            HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL,
                        "plugin file read failed: time = %s, filename = '%s', file descriptor = %d, "
                        "errno = %d, error message = '%s', buf = %p, total read size = %llu, "
                        "bytes this sub-read = %llu, offset = %llu",
                        timebuf, filename, fd, myerrno, strerror(myerrno), (void *)buf,
                        (unsigned long long)size, (unsigned long long)bytes_in, (unsigned long long)offset);
        }

        if (0 == bytes_read)
            HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL,
                        "unexpected end of file while reading plugin '%s' at offset %llu", filename,
                        (unsigned long long)offset);

        assert(bytes_read >= 0);
        assert((size_t)bytes_read <= left_to_read);

        left_to_read -= (size_t)bytes_read;
        read_ptr += bytes_read;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__read_file_data() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__create_public_RSA_from_string
 *
 * Purpose:     Create EVP public key from hardcoded PEM string
 *              Uses modern OpenSSL 3.0+ EVP API instead of deprecated RSA API
 *
 * Return:      Success: Pointer to EVP_PKEY
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static EVP_PKEY *
H5PL__create_public_RSA_from_string(const char *key_string)
{
    BIO      *key_bio   = NULL;
    EVP_PKEY *pkey      = NULL;
    EVP_PKEY *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(key_string);

    /* Create BIO from string */
    if (NULL == (key_bio = BIO_new_mem_buf(key_string, -1))) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, NULL, "cannot create BIO from key string: %s", err_buf);
    }

    /* Read public key using modern EVP API */
    if (NULL == (pkey = PEM_read_bio_PUBKEY(key_bio, NULL, NULL, NULL))) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "cannot read public key from BIO: %s", err_buf);
    }

    ret_value = pkey;
    pkey      = NULL; /* Prevent cleanup */

done:
    if (key_bio)
        BIO_free(key_bio);
    if (pkey)
        EVP_PKEY_free(pkey);

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__create_public_RSA_from_string() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__verify_signature_appended
 *
 * Purpose:     Verify appended RSA signature on plugin binary
 *
 *              File format:
 *                [ Binary Data ] [ RSA Signature ] [ Footer ]
 *
 *              The footer contains signature length and magic number.
 *              This function:
 *                1. Opens the plugin file
 *                2. Reads footer from end
 *                3. Validates magic number
 *                4. Reads signature
 *                5. Hashes the binary portion
 *                6. Verifies signature using hardcoded public key
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__verify_signature_appended(const char *plugin_path)
{
    int               fd = -1;
    h5_stat_t         st;
    HDoff_t           file_size = 0;
    H5PL_sig_footer_t footer;
    unsigned char    *signature     = NULL;
    unsigned char    *binary_data   = NULL;
    size_t            binary_size   = 0;
    EVP_PKEY         *public_key    = NULL;
    EVP_MD_CTX       *mdctx         = NULL;
    EVP_PKEY_CTX     *pkey_ctx      = NULL;
    int               verify_result = 0;
    herr_t            ret_value     = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(plugin_path);

    /* Open plugin file for reading (HDopen handles O_BINARY automatically on Windows) */
    if ((fd = HDopen(plugin_path, O_RDONLY, 0)) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open plugin file");

    /* Get file size using portable stat */
    if (HDfstat(fd, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot get file size");

    file_size = (HDoff_t)st.st_size;

    /* File must be large enough for footer */
    if (file_size < (HDoff_t)sizeof(H5PL_sig_footer_t))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "file too small to contain signature footer");

    /* Read footer using safe I/O from end of file */
    if (H5PL__read_file_data(fd, file_size - (HDoff_t)sizeof(H5PL_sig_footer_t), &footer,
                             sizeof(H5PL_sig_footer_t), plugin_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature footer");

    /* Convert footer from little-endian (file format) to native byte order */
    {
        uint8_t *p = (uint8_t *)&footer;
        uint32_t signature_length_le, magic_le;

        UINT32DECODE(p, signature_length_le);
        UINT32DECODE(p, magic_le);

        footer.signature_length = signature_length_le;
        footer.magic            = magic_le;
    }

    /* Validate magic number */
    if (footer.magic != H5PL_SIG_MAGIC)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "invalid signature magic number (not a signed HDF5 plugin or corrupted)");

    /* Validate signature length */
    if (footer.signature_length == 0 || footer.signature_length > 8192)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "invalid signature length");

    /* Validate file is large enough for signature and footer */
    if (file_size < (HDoff_t)(footer.signature_length + sizeof(H5PL_sig_footer_t)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "file too small to contain claimed signature and footer");

    /* Calculate binary data size (file - signature - footer) */
    {
        HDoff_t binary_size_off =
            file_size - (HDoff_t)footer.signature_length - (HDoff_t)sizeof(H5PL_sig_footer_t);

        /* Practical size limit: 1GB for plugin files (prevents unreasonable allocations) */
#define H5PL_MAX_PLUGIN_SIZE ((HDoff_t)(1024 * 1024 * 1024))
        if (binary_size_off > H5PL_MAX_PLUGIN_SIZE)
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                        "plugin binary size %llu exceeds maximum allowed size (%llu bytes) - "
                        "file too large to verify",
                        (unsigned long long)binary_size_off, (unsigned long long)H5PL_MAX_PLUGIN_SIZE);

        /* Check for overflow when casting to size_t (critical on 32-bit systems with LFS) */
        if (binary_size_off > (HDoff_t)SIZE_MAX)
            HGOTO_ERROR(
                H5E_PLUGIN, H5E_BADVALUE, FAIL,
                "plugin binary size %llu exceeds SIZE_MAX - file too large to verify on this platform",
                (unsigned long long)binary_size_off);

        binary_size = (size_t)binary_size_off;
#undef H5PL_MAX_PLUGIN_SIZE
    }

    /* Allocate signature buffer */
    if (NULL == (signature = (unsigned char *)H5MM_malloc(footer.signature_length)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate signature buffer");

    /* Read signature using safe I/O with EINTR retry and chunking */
    if (H5PL__read_file_data(fd, (HDoff_t)binary_size, signature, footer.signature_length, plugin_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature data");

    /* Get public key from cache or create it on first use
     * This lazy initialization pattern avoids parsing the PEM string on every plugin load,
     * providing significant performance improvement when verifying multiple plugins.
     */
    if (!H5PL_public_key_initialized_g) {
        H5PL_cached_public_key_g      = H5PL__create_public_RSA_from_string(H5PL_PUBLIC_KEY_PEM);
        H5PL_public_key_initialized_g = true;
    }
    if (NULL == H5PL_cached_public_key_g)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "cannot create public key");

    /* Use the cached public key (no need to free it in this function) */
    public_key = H5PL_cached_public_key_g;

    /* Create message digest context for signature verification */
    if (NULL == (mdctx = EVP_MD_CTX_new())) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "cannot create message digest context: %s", err_buf);
    }

    if (1 != EVP_DigestVerifyInit(mdctx, &pkey_ctx, EVP_sha256(), NULL, public_key)) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "cannot initialize signature verification: %s", err_buf);
    }

    /* Streaming hash computation: read and hash binary in chunks to avoid large memory allocation
     * This prevents loading entire plugin (potentially 1GB) into memory at once.
     * Instead, we read 64KB chunks and update the hash incrementally.
     */
#define H5PL_HASH_CHUNK_SIZE ((size_t)(64 * 1024)) /* 64KB chunks */

    /* Allocate small chunk buffer (reused for all reads) */
    if (NULL == (binary_data = (unsigned char *)H5MM_malloc(H5PL_HASH_CHUNK_SIZE)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate hash chunk buffer");

    /* Process binary data in chunks */
    {
        size_t  remaining      = binary_size;
        HDoff_t current_offset = 0;

        while (remaining > 0) {
            size_t chunk_size = (remaining > H5PL_HASH_CHUNK_SIZE) ? H5PL_HASH_CHUNK_SIZE : remaining;

            /* Read chunk from file */
            if (H5PL__read_file_data(fd, current_offset, binary_data, chunk_size, plugin_path) < 0)
                HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read binary chunk at offset %llu",
                            (unsigned long long)current_offset);

            /* Update hash with chunk data */
            if (1 != EVP_DigestVerifyUpdate(mdctx, binary_data, chunk_size)) {
                unsigned long ssl_err = ERR_get_error();
                char          err_buf[256];
                ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot update signature verification: %s",
                            err_buf);
            }

            remaining -= chunk_size;
            current_offset += (HDoff_t)chunk_size;
        }
    }

#undef H5PL_HASH_CHUNK_SIZE

    /* Close file now that we're done reading */
    HDclose(fd);
    fd = -1;

    verify_result = EVP_DigestVerifyFinal(mdctx, signature, (size_t)footer.signature_length);

    if (verify_result != 1) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "plugin signature verification failed - signature is not authentic: %s", err_buf);
    }

done:
    if (fd >= 0)
        HDclose(fd);
    if (signature)
        H5MM_xfree(signature);
    if (binary_data)
        H5MM_xfree(binary_data);
    if (mdctx)
        EVP_MD_CTX_free(mdctx);
    /* Note: public_key points to the cached key, so we don't free it here.
     * It will be freed in H5PL__cleanup_signature_cache() during package termination.
     */

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__verify_signature_appended() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__cleanup_signature_cache
 *
 * Purpose:     Clean up cached signature verification resources
 *              Called during H5PL package termination to free the cached public key
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__cleanup_signature_cache(void)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Free the cached public key if it was initialized */
    if (H5PL_public_key_initialized_g) {
        if (H5PL_cached_public_key_g) {
            EVP_PKEY_free(H5PL_cached_public_key_g);
            H5PL_cached_public_key_g = NULL;
        }
        H5PL_public_key_initialized_g = false;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5PL__cleanup_signature_cache() */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
