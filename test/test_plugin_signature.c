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
 * Purpose:    Comprehensive tests for HDF5 plugin signature verification
 *
 *             This test suite verifies that the plugin signature verification
 *             system correctly handles:
 *             1. Valid signed plugins (should load successfully)
 *             2. Unsigned plugins (should be rejected)
 *             3. Tampered plugins (should be rejected)
 *             4. Plugins with invalid signatures (should be rejected)
 */

#include "h5test.h"
#include "H5srcdir.h"

/*
 * This file needs to access private datatypes from the H5PL package.
 */
#define H5PL_FRIEND
#include "H5PLpkg.h"
#include "H5PLsig.h"

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

#include <sys/stat.h>
#include <fcntl.h>

/* Test filter ID */
#define TEST_SIGNATURE_FILTER_ID 260

/* Test files */
static const char *PLUGIN_DIR        = "test_plugin_signature_dir";
static const char *SIGNED_PLUGIN     = "libh5test_sig_filter.so";
static const char *UNSIGNED_PLUGIN   = "libh5test_sig_filter_unsigned.so";
static const char *TAMPERED_PLUGIN   = "libh5test_sig_filter_tampered.so";
static const char *BAD_SIG_PLUGIN    = "libh5test_sig_filter_badsig.so";
static const char *NO_FOOTER_PLUGIN  = "libh5test_sig_filter_nofooter.so";
static const char *CORRUPT_MAGIC_PLUGIN = "libh5test_sig_filter_badmagic.so";

/* Test key paths (set via environment or compile-time) */
static char test_private_key[1024] = "";
static char test_public_key[1024]  = "";

/*-------------------------------------------------------------------------
 * Function:    create_dummy_plugin
 *
 * Purpose:     Create a minimal valid plugin binary for testing
 *              This creates a simple binary file that can be used as
 *              a base for signature testing.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
create_dummy_plugin(const char *path)
{
    int    fd;
    herr_t ret_value = SUCCEED;

    /* Create minimal plugin file - just some dummy binary data */
    const unsigned char dummy_data[] = {
        /* ELF header magic for shared library (simplified) */
        0x7f, 'E', 'L', 'F',              /* Magic number */
        0x02, 0x01, 0x01, 0x00,           /* 64-bit, little-endian, current version */
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* Padding */
        /* Some dummy content to make it a reasonable size */
        'T', 'E', 'S', 'T', ' ', 'P', 'L', 'U', 'G', 'I', 'N', '\0'
    };

    if ((fd = HDopen(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0) {
        fprintf(stderr, "Failed to create plugin file: %s\n", path);
        return FAIL;
    }

    if (HDwrite(fd, dummy_data, sizeof(dummy_data)) < 0) {
        fprintf(stderr, "Failed to write plugin data: %s\n", path);
        HDclose(fd);
        return FAIL;
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    sign_plugin_file
 *
 * Purpose:     Sign a plugin file using the Python signing script
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
sign_plugin_file(const char *plugin_path, const char *private_key_path)
{
    char   cmd[2048];
    int    result;
    herr_t ret_value = SUCCEED;

    /* Build command to sign the plugin */
    snprintf(cmd, sizeof(cmd), "python3 %s/bin/sign-hdf5-plugin.py %s %s 2>&1",
             H5_get_srcdir(), plugin_path, private_key_path);

    result = system(cmd);
    if (result != 0) {
        fprintf(stderr, "Failed to sign plugin: %s\n", plugin_path);
        return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    append_bad_signature
 *
 * Purpose:     Append an invalid signature to a plugin file
 *              This creates a plugin that has a signature footer but
 *              with an incorrect signature value.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
append_bad_signature(const char *plugin_path)
{
    int                fd;
    H5PL_sig_footer_t  footer;
    unsigned char      bad_signature[256];
    size_t             i;
    herr_t             ret_value = SUCCEED;

    /* Create a dummy bad signature (just random bytes) */
    for (i = 0; i < sizeof(bad_signature); i++)
        bad_signature[i] = (unsigned char)(i * 7 + 13);  /* Arbitrary pattern */

    /* Open plugin file in append mode */
    if ((fd = HDopen(plugin_path, O_WRONLY | O_APPEND, 0)) < 0) {
        fprintf(stderr, "Failed to open plugin for bad signature: %s\n", plugin_path);
        return FAIL;
    }

    /* Write bad signature */
    if (HDwrite(fd, bad_signature, sizeof(bad_signature)) < 0) {
        fprintf(stderr, "Failed to write bad signature\n");
        HDclose(fd);
        return FAIL;
    }

    /* Write footer with correct format but pointing to bad signature */
    footer.signature_length = sizeof(bad_signature);
    footer.magic            = H5PL_SIG_MAGIC;

    /* Encode footer in little-endian (as expected by verification code) */
    {
        unsigned char footer_bytes[8];
        unsigned char *p = footer_bytes;

        UINT32ENCODE(p, footer.signature_length);
        UINT32ENCODE(p, footer.magic);

        if (HDwrite(fd, footer_bytes, sizeof(footer_bytes)) < 0) {
            fprintf(stderr, "Failed to write footer\n");
            HDclose(fd);
            return FAIL;
        }
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    append_corrupt_footer
 *
 * Purpose:     Append a footer with corrupted magic number
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
append_corrupt_footer(const char *plugin_path)
{
    int           fd;
    unsigned char footer_bytes[8];
    unsigned char *p = footer_bytes;
    herr_t        ret_value = SUCCEED;

    if ((fd = HDopen(plugin_path, O_WRONLY | O_APPEND, 0)) < 0) {
        fprintf(stderr, "Failed to open plugin for corrupt footer: %s\n", plugin_path);
        return FAIL;
    }

    /* Write footer with wrong magic number */
    UINT32ENCODE(p, 256);         /* Signature length */
    UINT32ENCODE(p, 0xDEADBEEF);  /* Wrong magic */

    if (HDwrite(fd, footer_bytes, sizeof(footer_bytes)) < 0) {
        fprintf(stderr, "Failed to write corrupt footer\n");
        HDclose(fd);
        return FAIL;
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    tamper_with_plugin
 *
 * Purpose:     Modify a signed plugin to invalidate its signature
 *              This simulates an attacker tampering with a signed plugin.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
tamper_with_plugin(const char *plugin_path)
{
    int    fd;
    char   byte;
    herr_t ret_value = SUCCEED;

    /* Open plugin and modify the first byte of content */
    if ((fd = HDopen(plugin_path, O_RDWR, 0)) < 0) {
        fprintf(stderr, "Failed to open plugin for tampering: %s\n", plugin_path);
        return FAIL;
    }

    /* Read first byte */
    if (HDread(fd, &byte, 1) < 0) {
        fprintf(stderr, "Failed to read plugin byte\n");
        HDclose(fd);
        return FAIL;
    }

    /* Modify it */
    byte ^= 0xFF;

    /* Seek back and write modified byte */
    if (HDlseek(fd, 0, SEEK_SET) < 0) {
        fprintf(stderr, "Failed to seek plugin\n");
        HDclose(fd);
        return FAIL;
    }

    if (HDwrite(fd, &byte, 1) < 0) {
        fprintf(stderr, "Failed to write modified byte\n");
        HDclose(fd);
        return FAIL;
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    setup_test_environment
 *
 * Purpose:     Set up the test environment with various test plugins
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
setup_test_environment(void)
{
    char plugin_path[1024];
    char temp_path[1024];

    /* Create plugin directory */
    HDmkdir(PLUGIN_DIR, 0755);

    /* Get test keys from environment or use defaults */
    if (getenv("HDF5_TEST_PRIVATE_KEY")) {
        strncpy(test_private_key, getenv("HDF5_TEST_PRIVATE_KEY"), sizeof(test_private_key) - 1);
    }
    else {
        /* Try to find test keys in common locations */
        snprintf(test_private_key, sizeof(test_private_key), "%s/ci-test-private.pem", H5_get_srcdir());
    }

    if (getenv("HDF5_TEST_PUBLIC_KEY")) {
        strncpy(test_public_key, getenv("HDF5_TEST_PUBLIC_KEY"), sizeof(test_public_key) - 1);
    }
    else {
        snprintf(test_public_key, sizeof(test_public_key), "%s/ci-test-public.pem", H5_get_srcdir());
    }

    /* Verify keys exist */
    if (access(test_private_key, R_OK) != 0) {
        fprintf(stderr, "Test private key not found: %s\n", test_private_key);
        fprintf(stderr, "Set HDF5_TEST_PRIVATE_KEY environment variable or generate keys\n");
        return FAIL;
    }

    /* 1. Create and sign a valid plugin */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, SIGNED_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (sign_plugin_file(plugin_path, test_private_key) < 0)
        return FAIL;

    /* 2. Create an unsigned plugin */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, UNSIGNED_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;

    /* 3. Create a signed plugin then tamper with it */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, TAMPERED_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (sign_plugin_file(plugin_path, test_private_key) < 0)
        return FAIL;
    if (tamper_with_plugin(plugin_path) < 0)
        return FAIL;

    /* 4. Create plugin with bad signature */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, BAD_SIG_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (append_bad_signature(plugin_path) < 0)
        return FAIL;

    /* 5. Create plugin with no footer */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, NO_FOOTER_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;

    /* 6. Create plugin with corrupted magic number */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, CORRUPT_MAGIC_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (append_corrupt_footer(plugin_path) < 0)
        return FAIL;

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    cleanup_test_environment
 *
 * Purpose:     Clean up test files
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
cleanup_test_environment(void)
{
    char cmd[1024];

    /* Remove plugin directory */
    snprintf(cmd, sizeof(cmd), "rm -rf %s", PLUGIN_DIR);
    system(cmd);

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_valid_signed_plugin
 *
 * Purpose:     Test that a properly signed plugin is accepted
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_valid_signed_plugin(void)
{
    char   plugin_path[1024];
    herr_t ret_value = SUCCEED;

    TESTING("valid signed plugin verification");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, SIGNED_PLUGIN);

    /* Verify the signed plugin */
    if (H5PL__verify_signature_appended(plugin_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "    Valid signed plugin was rejected\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_unsigned_plugin_rejected
 *
 * Purpose:     Test that an unsigned plugin is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_unsigned_plugin_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("unsigned plugin rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, UNSIGNED_PLUGIN);

    /* Verification should fail for unsigned plugin */
    H5E_BEGIN_TRY {
        status = H5PL__verify_signature_appended(plugin_path);
    } H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Unsigned plugin was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_tampered_plugin_rejected
 *
 * Purpose:     Test that a tampered plugin is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_tampered_plugin_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("tampered plugin rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, TAMPERED_PLUGIN);

    /* Verification should fail for tampered plugin */
    H5E_BEGIN_TRY {
        status = H5PL__verify_signature_appended(plugin_path);
    } H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Tampered plugin was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_bad_signature_rejected
 *
 * Purpose:     Test that a plugin with wrong signature is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_bad_signature_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("plugin with invalid signature rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, BAD_SIG_PLUGIN);

    /* Verification should fail for plugin with bad signature */
    H5E_BEGIN_TRY {
        status = H5PL__verify_signature_appended(plugin_path);
    } H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Plugin with bad signature was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_no_footer_rejected
 *
 * Purpose:     Test that a plugin without signature footer is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_no_footer_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("plugin without signature footer rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, NO_FOOTER_PLUGIN);

    /* Verification should fail for plugin without footer */
    H5E_BEGIN_TRY {
        status = H5PL__verify_signature_appended(plugin_path);
    } H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Plugin without footer was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_corrupt_magic_rejected
 *
 * Purpose:     Test that a plugin with corrupted magic number is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_corrupt_magic_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("plugin with corrupt magic number rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, CORRUPT_MAGIC_PLUGIN);

    /* Verification should fail for plugin with corrupt magic */
    H5E_BEGIN_TRY {
        status = H5PL__verify_signature_appended(plugin_path);
    } H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Plugin with corrupt magic was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run plugin signature verification tests
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    printf("Testing HDF5 Plugin Signature Verification\n");
    printf("==========================================\n\n");

    /* Set up test environment */
    if (setup_test_environment() < 0) {
        fprintf(stderr, "Failed to set up test environment\n");
        return EXIT_FAILURE;
    }

    /* Run tests */
    nerrors += test_valid_signed_plugin() < 0       ? 1 : 0;
    nerrors += test_unsigned_plugin_rejected() < 0  ? 1 : 0;
    nerrors += test_tampered_plugin_rejected() < 0  ? 1 : 0;
    nerrors += test_bad_signature_rejected() < 0    ? 1 : 0;
    nerrors += test_no_footer_rejected() < 0        ? 1 : 0;
    nerrors += test_corrupt_magic_rejected() < 0    ? 1 : 0;

    /* Clean up */
    cleanup_test_environment();

    /* Report results */
    if (nerrors) {
        printf("\n***** %d PLUGIN SIGNATURE VERIFICATION TEST%s FAILED *****\n",
               nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    printf("\nAll plugin signature verification tests passed.\n");
    return EXIT_SUCCESS;
}

#else /* H5_REQUIRE_DIGITAL_SIGNATURE */

int
main(void)
{
    printf("Plugin signature verification is not enabled.\n");
    printf("Reconfigure with -DHDF5_REQUIRE_SIGNED_PLUGINS=ON to enable these tests.\n");
    return EXIT_SUCCESS;  /* Not a failure - feature not enabled */
}

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
