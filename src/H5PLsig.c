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
#include "H5private.h"    /* Generic Functions            */
#include "H5Eprivate.h"   /* Error handling               */
#include "H5PLpkg.h"      /* Plugin                       */
#include "H5PLsig.h"      /* Signature format             */
#include "H5MMprivate.h"  /* Memory management            */

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

#include <openssl/rsa.h>
#include <openssl/pem.h>
#include <openssl/sha.h>
#include <openssl/bio.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef H5_HAVE_WIN32_API
#include <io.h>
#define open  _open
#define close _close
#define read  _read
#define lseek _lseeki64
#else
#include <unistd.h>
/* O_BINARY is Windows-specific; not defined on POSIX systems */
#ifndef O_BINARY
#define O_BINARY 0
#endif
#endif

/*-------------------------------------------------------------------------
 * Function:    H5PL__create_public_RSA_from_string
 *
 * Purpose:     Create RSA public key from hardcoded PEM string
 *
 * Return:      Success: Pointer to RSA key
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static RSA *
H5PL__create_public_RSA_from_string(const char *key_string)
{
    BIO *key_bio   = NULL;
    RSA *rsa       = NULL;
    RSA *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(key_string);

    /* Create BIO from string */
    if (NULL == (key_bio = BIO_new_mem_buf(key_string, -1)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, NULL, "cannot create BIO from key string");

    /* Read public key */
    if (NULL == (rsa = PEM_read_bio_RSA_PUBKEY(key_bio, NULL, NULL, NULL)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "cannot read RSA public key from BIO");

    ret_value = rsa;
    rsa       = NULL; /* Prevent cleanup */

done:
    if (key_bio)
        BIO_free(key_bio);
    if (rsa)
        RSA_free(rsa);

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
    int                  fd            = -1;
    struct stat          st;
    off_t                file_size     = 0;
    H5PL_sig_footer_t    footer;
    unsigned char       *signature     = NULL;
    unsigned char       *binary_data   = NULL;
    size_t               binary_size   = 0;
    unsigned char        hash[SHA256_DIGEST_LENGTH];
    RSA                 *public_key    = NULL;
    int                  verify_result = 0;
    herr_t               ret_value     = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(plugin_path);

    /* Open plugin file for reading */
    if ((fd = open(plugin_path, O_RDONLY | O_BINARY)) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open plugin file");

    /* Get file size */
    if (fstat(fd, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot get file size");

    file_size = st.st_size;

    /* File must be large enough for footer */
    if (file_size < (off_t)sizeof(H5PL_sig_footer_t))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "file too small to contain signature footer");

    /* Seek to footer position (end of file - footer size) */
    if (lseek(fd, -(off_t)sizeof(H5PL_sig_footer_t), SEEK_END) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTSEEK, FAIL, "cannot seek to footer");

    /* Read footer */
    if (read(fd, &footer, sizeof(H5PL_sig_footer_t)) != (ssize_t)sizeof(H5PL_sig_footer_t))
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature footer");

    /* Validate magic number */
    if (footer.magic != H5PL_SIG_MAGIC)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "invalid signature magic number (not a signed HDF5 plugin or corrupted)");

    /* Validate signature length */
    if (footer.signature_length == 0 || footer.signature_length > 8192)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "invalid signature length");

    /* Calculate binary data size (file - signature - footer) */
    binary_size = (size_t)(file_size - footer.signature_length - sizeof(H5PL_sig_footer_t));

    /* Allocate signature buffer */
    if (NULL == (signature = (unsigned char *)H5MM_malloc(footer.signature_length)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate signature buffer");

    /* Seek to signature position (binary_size offset from start) */
    if (lseek(fd, (off_t)binary_size, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTSEEK, FAIL, "cannot seek to signature");

    /* Read signature */
    if (read(fd, signature, footer.signature_length) != (ssize_t)footer.signature_length)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature data");

    /* Allocate buffer for binary data */
    if (NULL == (binary_data = (unsigned char *)H5MM_malloc(binary_size)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate binary data buffer");

    /* Seek to start of file */
    if (lseek(fd, 0, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTSEEK, FAIL, "cannot seek to file start");

    /* Read binary data */
    if (read(fd, binary_data, binary_size) != (ssize_t)binary_size)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read binary data");

    /* Close file (we're done reading) */
    close(fd);
    fd = -1;

    /* Calculate SHA256 hash of binary data */
    SHA256(binary_data, binary_size, hash);

    /* Create RSA public key from hardcoded PEM string */
    if (NULL == (public_key = H5PL__create_public_RSA_from_string(H5PL_PUBLIC_KEY_PEM)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "cannot create RSA public key");

    /* Verify signature */
    verify_result = RSA_verify(NID_sha256, hash, SHA256_DIGEST_LENGTH,
                                signature, footer.signature_length, public_key);

    if (verify_result != 1)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "plugin signature verification failed - signature is not authentic");

done:
    if (fd >= 0)
        close(fd);
    if (signature)
        H5MM_xfree(signature);
    if (binary_data)
        H5MM_xfree(binary_data);
    if (public_key)
        RSA_free(public_key);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__verify_signature_appended() */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
