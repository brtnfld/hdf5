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

/* For directory operations */
#ifndef H5_HAVE_WIN32_API
#include <dirent.h>
#endif

/*******************/
/* Local Variables */
/*******************/

/* KeyStore entry for storing multiple trusted public keys */
typedef struct H5PL_keystore_entry_t {
    EVP_PKEY *key;    /* OpenSSL public key object */
    char     *source; /* Key source (filename or "embedded") for debugging */
} H5PL_keystore_entry_t;

/* KeyStore for signature verification
 * Supports multiple trusted keys from different organizations (LLNL, ANL, HDFGroup, etc.)
 * Keys are loaded from:
 *   1. Environment variable HDF5_PLUGIN_KEYSTORE (highest priority)
 *   2. CMake-configured HDF5_PLUGIN_KEYSTORE_DIR
 *   3. Compile-time embedded H5PL_PUBLIC_KEY_PEM (backward compatibility)
 */
static H5PL_keystore_entry_t *H5PL_keystore_g             = NULL;
static size_t                 H5PL_keystore_count_g       = 0;
static size_t                 H5PL_keystore_capacity_g    = 0;
static bool                   H5PL_keystore_initialized_g = false;

/* Initial capacity for keystore array */
#define H5PL_KEYSTORE_INITIAL_CAPACITY 4

/*-------------------------------------------------------------------------
 * Function:    H5PL__read_file_data
 *
 * Purpose:     Wrapper around H5_read_safe() for plugin signature reading
 *
 *              Uses the centralized safe I/O implementation from H5system.c
 *              which handles EINTR retry, partial reads, chunking, and pread.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__read_file_data(int fd, HDoff_t offset, void *buf, size_t size, const char *filename)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(buf);
    assert(filename);

    /* Use centralized safe I/O routine */
    if (H5_read_safe(fd, offset, buf, size, filename) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "failed to read plugin file data");

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
 * Function:    H5PL__add_key_to_keystore
 *
 * Purpose:     Add a public key to the keystore with source tracking
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__add_key_to_keystore(EVP_PKEY *key, const char *source)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(key);
    assert(source);

    /* Expand keystore if needed */
    if (H5PL_keystore_count_g >= H5PL_keystore_capacity_g) {
        size_t new_capacity =
            H5PL_keystore_capacity_g == 0 ? H5PL_KEYSTORE_INITIAL_CAPACITY : H5PL_keystore_capacity_g * 2;
        H5PL_keystore_entry_t *new_keystore = (H5PL_keystore_entry_t *)H5MM_realloc(
            H5PL_keystore_g, new_capacity * sizeof(H5PL_keystore_entry_t));

        if (NULL == new_keystore)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot expand keystore array");

        H5PL_keystore_g          = new_keystore;
        H5PL_keystore_capacity_g = new_capacity;
    }

    /* Add key to keystore */
    H5PL_keystore_g[H5PL_keystore_count_g].key = key;

    /* Duplicate source string for storage */
    if (NULL == (H5PL_keystore_g[H5PL_keystore_count_g].source = strdup(source)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot duplicate key source string");

    H5PL_keystore_count_g++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__add_key_to_keystore() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__create_public_RSA_from_file
 *
 * Purpose:     Create EVP public key from PEM file
 *
 * Return:      Success: Pointer to EVP_PKEY
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static EVP_PKEY *
H5PL__create_public_RSA_from_file(const char *file_path)
{
    FILE     *key_file  = NULL;
    EVP_PKEY *pkey      = NULL;
    EVP_PKEY *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(file_path);

    /* Open key file */
    if (NULL == (key_file = fopen(file_path, "r"))) {
        /* Don't error - just skip invalid files */
        goto done;
    }

    /* Read public key using modern EVP API */
    if (NULL == (pkey = PEM_read_PUBKEY(key_file, NULL, NULL, NULL))) {
        /* Don't error - just skip invalid PEM files */
        goto done;
    }

    ret_value = pkey;
    pkey      = NULL; /* Prevent cleanup */

done:
    if (key_file)
        fclose(key_file);
    if (pkey)
        EVP_PKEY_free(pkey);

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__create_public_RSA_from_file() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__validate_directory_permissions
 *
 * Purpose:     Validate that a directory is not world-writable
 *              This prevents unprivileged users from adding malicious keys
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__validate_directory_permissions(const char *dir_path)
{
    h5_stat_t st;
    herr_t    ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dir_path);

    /* Check if directory exists and get permissions */
    if (HDstat(dir_path, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot stat keystore directory: %s", dir_path);

    /* Verify it's a directory */
    if (!S_ISDIR(st.st_mode))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore path is not a directory: %s", dir_path);

#ifndef H5_HAVE_WIN32_API
    /* SECURITY: Reject world-writable directories (Unix/Linux only)
     * This prevents unprivileged users from adding malicious keys
     */
    if (st.st_mode & S_IWOTH)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "SECURITY ERROR: keystore directory is world-writable (mode %o): %s\n"
                    "This allows unprivileged users to add malicious keys.\n"
                    "Fix with: chmod o-w %s",
                    (unsigned)(st.st_mode & 0777), dir_path, dir_path);
#else
    /* SECURITY WARNING for Windows: We cannot easily check if the directory
     * is writable by non-administrators due to complex ACL semantics.
     *
     * IMPORTANT: Administrators must ensure the KeyStore directory is:
     *   - Located in a system-protected path (e.g., C:\Program Files\)
     *   - NOT in user-writable locations (e.g., C:\Temp, %APPDATA%)
     *   - Has ACLs that restrict write access to Administrators only
     *
     * Recommended KeyStore locations on Windows:
     *   - C:\Program Files\HDF_Group\HDF5\trusted_keys
     *   - C:\ProgramData\HDF_Group\trusted_keys (if ACLs properly configured)
     *
     * AVOID user-writable locations like:
     *   - C:\Temp
     *   - C:\Users\<username>\AppData
     *   - Any folder with "Everyone: Full Control" ACL
     */

    /* Warn if KeyStore is in obviously dangerous locations on Windows */
    {
        char   dir_upper[MAX_PATH];
        size_t i;

        /* Convert to uppercase for case-insensitive comparison */
        strncpy(dir_upper, dir_path, sizeof(dir_upper) - 1);
        dir_upper[sizeof(dir_upper) - 1] = '\0';
        for (i = 0; dir_upper[i]; i++)
            dir_upper[i] = (char)toupper((unsigned char)dir_upper[i]);

        /* Check for obviously dangerous locations */
        if (strstr(dir_upper, "\\TEMP") || strstr(dir_upper, "\\TMP") || strstr(dir_upper, "\\APPDATA") ||
            strstr(dir_upper, "\\USERS\\")) {
            /* Issue warning but don't fail - admin might have locked it down */
            fprintf(stderr,
                    "WARNING: KeyStore directory may be in user-writable location: %s\n"
                    "  This could allow unprivileged users to inject malicious keys.\n"
                    "  Recommended: Use system-protected paths like:\n"
                    "    C:\\Program Files\\HDF_Group\\HDF5\\trusted_keys\n"
                    "  Verify directory ACLs restrict write access to Administrators only.\n",
                    dir_path);
        }
    }
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__validate_directory_permissions() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__load_keys_from_directory
 *
 * Purpose:     Load all .pem files from a directory into the keystore
 *
 * Return:      SUCCEED/FAIL (fails if directory invalid, but skips bad files)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__load_keys_from_directory(const char *dir_path)
{
    H5PL_HANDLE dir_handle = NULL;
    herr_t      ret_value  = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dir_path);

    /* Validate directory permissions */
    if (H5PL__validate_directory_permissions(dir_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore directory validation failed");

#ifdef H5_HAVE_WIN32_API
    {
        WIN32_FIND_DATAA find_data;
        char             search_pattern[MAX_PATH];

        /* Build search pattern: dir\*.pem */
        snprintf(search_pattern, sizeof(search_pattern), "%s\\*.pem", dir_path);

        dir_handle = FindFirstFileA(search_pattern, &find_data);
        if (INVALID_HANDLE_VALUE == dir_handle) {
            /* Empty directory is OK */
            goto done;
        }

        do {
            char      file_path[MAX_PATH];
            EVP_PKEY *key = NULL;

            /* Skip directories */
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                continue;

            /* Build full path */
            snprintf(file_path, sizeof(file_path), "%s\\%s", dir_path, find_data.cFileName);

            /* SECURITY: Skip symlinks and reparse points to prevent privilege escalation
             * An unprivileged user could create a symlink or junction in a trusted directory
             * pointing to a file they control, bypassing the world-writable check.
             */
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)
                continue; /* Skip reparse points (symlinks, junctions, etc.) */

            /* Try to load key */
            if (NULL != (key = H5PL__create_public_RSA_from_file(file_path))) {
                /* Add to keystore */
                if (H5PL__add_key_to_keystore(key, file_path) < 0) {
                    EVP_PKEY_free(key);
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add key to keystore");
                }
                /* Key ownership transferred to keystore */
            }
            /* Skip files that fail to load (invalid PEM, etc.) */

        } while (FindNextFileA(dir_handle, &find_data) != 0);
    }
#else
    {
        DIR           *dir    = NULL;
        struct dirent *entry  = NULL;
        size_t         dirlen = 0;

        /* Open directory */
        if (NULL == (dir = opendir(dir_path))) {
            /* Non-existent directory is an error */
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open keystore directory: %s", dir_path);
        }

        dirlen = strlen(dir_path);

        /* Iterate through directory entries */
        while (NULL != (entry = readdir(dir))) {
            char      file_path[4096];
            EVP_PKEY *key     = NULL;
            size_t    namelen = strlen(entry->d_name);

            /* Skip . and .. */
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;

            /* Only process .pem files */
            if (namelen < 5 || strcmp(entry->d_name + namelen - 4, ".pem") != 0)
                continue;

            /* Build full path */
            if (dirlen + namelen + 2 > sizeof(file_path))
                continue; /* Path too long, skip */

            snprintf(file_path, sizeof(file_path), "%s/%s", dir_path, entry->d_name);

            /* SECURITY: Skip symlinks to prevent privilege escalation
             * An unprivileged user could create a symlink in a trusted directory
             * pointing to a file they control, bypassing the world-writable check.
             * Using lstat (not stat) to detect symlinks before following them.
             */
            {
                h5_stat_t file_stat;
                if (HDlstat(file_path, &file_stat) < 0)
                    continue; /* Cannot stat, skip file */

                if (S_ISLNK(file_stat.st_mode)) {
                    /* Skip symbolic links for security */
                    continue;
                }
            }

            /* Try to load key */
            if (NULL != (key = H5PL__create_public_RSA_from_file(file_path))) {
                /* Add to keystore */
                if (H5PL__add_key_to_keystore(key, file_path) < 0) {
                    EVP_PKEY_free(key);
                    closedir(dir);
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add key to keystore");
                }
                /* Key ownership transferred to keystore */
            }
            /* Skip files that fail to load (invalid PEM, etc.) */
        }

        closedir(dir);
    }
#endif

done:
#ifdef H5_HAVE_WIN32_API
    if (dir_handle != NULL && dir_handle != INVALID_HANDLE_VALUE)
        FindClose(dir_handle);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__load_keys_from_directory() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__init_keystore
 *
 * Purpose:     Initialize keystore by loading keys from configured locations
 *
 *              Priority order:
 *                1. Environment variable HDF5_PLUGIN_KEYSTORE
 *                2. CMake-configured HDF5_PLUGIN_KEYSTORE_DIR
 *                3. Compile-time embedded H5PL_PUBLIC_KEY_PEM
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__init_keystore(void)
{
    const char *env_keystore = NULL;
    bool        keys_loaded  = false;
    herr_t      ret_value    = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Already initialized? */
    if (H5PL_keystore_initialized_g)
        HGOTO_DONE(SUCCEED);

    /* Initialize keystore */
    H5PL_keystore_g             = NULL;
    H5PL_keystore_count_g       = 0;
    H5PL_keystore_capacity_g    = 0;
    H5PL_keystore_initialized_g = true;

    /* 1. Check environment variable (highest priority) */
    if (NULL != (env_keystore = getenv("HDF5_PLUGIN_KEYSTORE"))) {
        if (H5PL__load_keys_from_directory(env_keystore) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, FAIL, "failed to load keys from HDF5_PLUGIN_KEYSTORE: %s",
                        env_keystore);
        keys_loaded = true;
    }

/* 2. Check CMake-configured directory */
#ifdef H5PL_KEYSTORE_DIR
    if (!keys_loaded) {
        /* Only try if directory was configured */
        h5_stat_t st;
        if (HDstat(H5PL_KEYSTORE_DIR, &st) == 0) {
            /* Directory exists, try to load */
            if (H5PL__load_keys_from_directory(H5PL_KEYSTORE_DIR) < 0) {
                /* Not a fatal error - fall through to embedded key */
            }
            else {
                keys_loaded = true;
            }
        }
    }
#endif

/* 3. Fallback to compile-time embedded key (backward compatibility) */
#ifdef H5PL_PUBLIC_KEY_PEM
    if (!keys_loaded) {
        EVP_PKEY *embedded_key = H5PL__create_public_RSA_from_string(H5PL_PUBLIC_KEY_PEM);
        if (NULL != embedded_key) {
            if (H5PL__add_key_to_keystore(embedded_key, "embedded") < 0) {
                EVP_PKEY_free(embedded_key);
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add embedded key to keystore");
            }
            keys_loaded = true;
        }
    }
#endif

    /* Must have at least one key */
    if (!keys_loaded || H5PL_keystore_count_g == 0) {
        const char *attempted_source = env_keystore ? env_keystore :
#ifdef H5PL_KEYSTORE_DIR
                                                    H5PL_KEYSTORE_DIR
#else
                                                    "(none configured)"
#endif
            ;

        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "no valid public keys found for plugin signature verification\n"
                    "  Attempted to load from: %s\n"
                    "  Keys found: 0\n"
                    "\n"
                    "Configure keys via:\n"
                    "  - Environment: export HDF5_PLUGIN_KEYSTORE=/path/to/keys\n"
                    "  - CMake: -DHDF5_PLUGIN_KEYSTORE_DIR=/path/to/keys\n"
                    "  - Compile-time: -DHDF5_PLUGIN_PUBLIC_KEY_FILE=key.pem\n"
                    "\n"
                    "Verify:\n"
                    "  - Directory exists and is readable\n"
                    "  - Directory contains .pem files\n"
                    "  - .pem files are valid RSA public keys",
                    attempted_source);
    }

#ifdef H5PL_DEBUG_KEYSTORE
    /* Optional debug output (enable via compile-time flag) */
    if (H5PL_keystore_count_g > 0) {
        fprintf(stderr, "HDF5 Plugin KeyStore initialized:\n");
        fprintf(stderr, "  Keys loaded: %zu\n", H5PL_keystore_count_g);
        for (size_t i = 0; i < H5PL_keystore_count_g; i++) {
            fprintf(stderr, "  [%zu] %s\n", i + 1, H5PL_keystore_g[i].source);
        }
    }
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__init_keystore() */

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
        if (binary_size_off < 0 || (uint64_t)binary_size_off > (uint64_t)SIZE_MAX)
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

    /* Initialize keystore on first use (lazy initialization)
     * Loads keys from environment, CMake-configured directory, or embedded key
     */
    if (!H5PL_keystore_initialized_g) {
        if (H5PL__init_keystore() < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "cannot initialize keystore");
    }

    /* Must have at least one key */
    if (H5PL_keystore_count_g == 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore is empty - no keys available for verification");

    /* Try verifying with each key in keystore (OR logic - first match wins) */
    {
        size_t key_idx;
        bool   verified = false;

        for (key_idx = 0; key_idx < H5PL_keystore_count_g; key_idx++) {
            public_key = H5PL_keystore_g[key_idx].key;

            /* Create fresh message digest context for this key */
            if (NULL == (mdctx = EVP_MD_CTX_new())) {
                unsigned long ssl_err = ERR_get_error();
                char          err_buf[256];
                ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "cannot create message digest context: %s",
                            err_buf);
            }

            if (1 != EVP_DigestVerifyInit(mdctx, &pkey_ctx, EVP_sha256(), NULL, public_key)) {
                unsigned long ssl_err = ERR_get_error();
                char          err_buf[256];
                ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));

                /* Clean up and try next key */
                EVP_MD_CTX_free(mdctx);
                mdctx = NULL;
                ERR_clear_error();
                continue;
            }

            /* Streaming hash computation: read and hash binary in chunks to avoid large memory allocation
             * This prevents loading entire plugin (potentially 1GB) into memory at once.
             * Instead, we read 64KB chunks and update the hash incrementally.
             */
#define H5PL_HASH_CHUNK_SIZE ((size_t)(64 * 1024)) /* 64KB chunks */

            /* Allocate small chunk buffer (reused for all reads) */
            if (binary_data == NULL) {
                if (NULL == (binary_data = (unsigned char *)H5MM_malloc(H5PL_HASH_CHUNK_SIZE)))
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate hash chunk buffer");
            }

            /* Process binary data in chunks */
            {
                size_t  remaining      = binary_size;
                HDoff_t current_offset = 0;
                bool    hash_ok        = true;

                while (remaining > 0) {
                    size_t chunk_size = (remaining > H5PL_HASH_CHUNK_SIZE) ? H5PL_HASH_CHUNK_SIZE : remaining;

                    /* Read chunk from file */
                    if (H5PL__read_file_data(fd, current_offset, binary_data, chunk_size, plugin_path) < 0)
                        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL,
                                    "cannot read binary chunk at offset %llu",
                                    (unsigned long long)current_offset);

                    /* Update hash with chunk data */
                    if (1 != EVP_DigestVerifyUpdate(mdctx, binary_data, chunk_size)) {
                        hash_ok = false;
                        break;
                    }

                    remaining -= chunk_size;
                    current_offset += (HDoff_t)chunk_size;
                }

                if (!hash_ok) {
                    /* Clean up and try next key */
                    EVP_MD_CTX_free(mdctx);
                    mdctx = NULL;
                    ERR_clear_error();
                    continue;
                }
            }

            /* Finalize verification */
            verify_result = EVP_DigestVerifyFinal(mdctx, signature, (size_t)footer.signature_length);

            /* Clean up context for this iteration */
            EVP_MD_CTX_free(mdctx);
            mdctx = NULL;

            if (verify_result == 1) {
                /* SUCCESS! Signature verified with this key */
                verified = true;
                break;
            }

            /* Clear OpenSSL errors before trying next key */
            ERR_clear_error();
        }

#undef H5PL_HASH_CHUNK_SIZE

        /* Close file now that we're done reading */
        HDclose(fd);
        fd = -1;

        /* Check if any key verified successfully */
        if (!verified) {
            /* Build informative error message with key sources for debugging */
            char   key_sources[1024] = "";
            char   temp[256];
            size_t msg_len = 0;

            for (size_t i = 0; i < H5PL_keystore_count_g && msg_len < sizeof(key_sources) - 50; i++) {
                const char *source  = H5PL_keystore_g[i].source ? H5PL_keystore_g[i].source : "unknown";
                int         written = snprintf(temp, sizeof(temp), "%s%s", (i > 0 ? ", " : ""), source);

                if (written > 0 && msg_len + written < sizeof(key_sources) - 1) {
                    strcat(key_sources, temp);
                    msg_len += written;
                }
            }

            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                        "plugin signature verification failed\n"
                        "  Plugin: %s\n"
                        "  Tried %zu key%s: [%s]\n"
                        "  No matching signature found\n"
                        "  Possible causes:\n"
                        "    - Plugin signed with different key\n"
                        "    - Plugin signature corrupted\n"
                        "    - Wrong KeyStore directory configured",
                        plugin_path, H5PL_keystore_count_g, (H5PL_keystore_count_g == 1 ? "" : "s"),
                        key_sources);
        }
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
    /* Note: public_key points to a key in the keystore, so we don't free it here.
     * All keystore keys will be freed in H5PL__cleanup_signature_cache() during package termination.
     */

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__verify_signature_appended() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__cleanup_signature_cache
 *
 * Purpose:     Clean up keystore signature verification resources
 *              Called during H5PL package termination to free all cached keys
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__cleanup_signature_cache(void)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Free all keys in the keystore */
    if (H5PL_keystore_initialized_g) {
        if (H5PL_keystore_g) {
            size_t i;
            for (i = 0; i < H5PL_keystore_count_g; i++) {
                if (H5PL_keystore_g[i].key)
                    EVP_PKEY_free(H5PL_keystore_g[i].key);
                if (H5PL_keystore_g[i].source)
                    free(H5PL_keystore_g[i].source);
            }
            H5MM_xfree(H5PL_keystore_g);
            H5PL_keystore_g = NULL;
        }
        H5PL_keystore_count_g       = 0;
        H5PL_keystore_capacity_g    = 0;
        H5PL_keystore_initialized_g = false;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5PL__cleanup_signature_cache() */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
