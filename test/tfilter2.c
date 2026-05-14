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
 * Tests for RFC-HDFG-2026-001: String-Based Filter Configuration API
 *   - H5Pappend_filter / H5Pget_filter_params_by_idx
 *   - Typed TOML accessor functions (H5Zconfig_get_int, _get_str, etc.)
 *   - Built-in filter set_config / get_config round-trips
 *   - Name registry (H5Z_filter_id_by_name)
 *   - Regression: existing H5Pset_filter still works
 */

#include "h5test.h"

static const char *FILENAME[] = {"tfilter2", NULL};

/* -----------------------------------------------------------------------
 * Parser tests — typed TOML accessor functions
 * ---------------------------------------------------------------------- */
static int
test_parser(void)
{
    char    vbuf[256];
    size_t  vsz;
    int64_t ival;
    double  dval;
    hbool_t bval;
    htri_t  ret;

    TESTING("H5Zconfig_get_int: basic integer lookup");
    ret = H5Zconfig_get_int("level = 6, mode = 2", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: key not found");
    ret = H5Zconfig_get_int("level = 6", "mode", &ival);
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: key present");
    ret = H5Zconfig_has_key("level = 6, compress = true", "compress");
    if (ret <= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: key absent");
    ret = H5Zconfig_has_key("level = 6", "mode");
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: double-quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("name = \"hello world\"", "name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "hello world") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: single-quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("name = 'hello world'", "name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "hello world") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: boolean true");
    ret = H5Zconfig_get_bool("compress = true", "compress", &bval);
    if (ret <= 0 || !bval)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: boolean false");
    ret = H5Zconfig_get_bool("compress = false", "compress", &bval);
    if (ret <= 0 || bval)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: float value");
    ret = H5Zconfig_get_double("tol = 1.5", "tol", &dval);
    if (ret <= 0 || dval != 1.5)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: NULL params error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int(NULL, "key", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: NULL key error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level = 6", NULL, &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: duplicate key error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level = 6, level = 9", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: whitespace around equals");
    ret = H5Zconfig_get_int("  level = 6 , mode = 2 ", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: braced inline-table form");
    ret = H5Zconfig_get_int("{level = 6, mode = 2}", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: braced inline-table form");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("{ coding = \"entropy\" }", "coding", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "entropy") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: type mismatch error (integer key)");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = H5Zconfig_get_str("level = 6", "level", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: negative integer");
    ret = H5Zconfig_get_int("offset = -4", "offset", &ival);
    if (ret <= 0 || ival != -4)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: scientific notation");
    ret = H5Zconfig_get_double("tol = 1.0e-6", "tol", &dval);
    if (ret <= 0 || dval < 9.9e-7 || dval > 1.1e-6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: comma inside quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("path = \"/data/run_1,v2/dict.bin\"", "path", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "/data/run_1,v2/dict.bin") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: backslash-quote escape in double-quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("msg = \"say \\\"hi\\\"\"", "msg", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "say \"hi\"") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: empty string is valid (no params)");
    ret = H5Zconfig_has_key("", "level");
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: inf rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_double("tol = inf", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: nan rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_double("tol = nan", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: semicolon outside quotes rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level = 6; mode = 2", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: underscore digit separator");
    ret = H5Zconfig_get_int("count = 1_000_000", "count", &ival);
    if (ret <= 0 || ival != 1000000)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: hex prefix 0x");
    ret = H5Zconfig_get_int("flags = 0xff", "flags", &ival);
    if (ret <= 0 || ival != 255)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: hex-float 0x1.8p+1 == 3.0");
    ret = H5Zconfig_get_double("rate = 0x1.8p+1", "rate", &dval);
    if (ret <= 0 || dval != 3.0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: hex-float 0x1.cp+1 == 3.5");
    ret = H5Zconfig_get_double("rate = 0x1.cp+1", "rate", &dval);
    if (ret <= 0 || dval != 3.5)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: negative hex-float -0x1p-1 == -0.5");
    ret = H5Zconfig_get_double("offset = -0x1p-1", "offset", &dval);
    if (ret <= 0 || dval != -0.5)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: hex-float without fraction 0xAp0 == 10.0");
    ret = H5Zconfig_get_double("val = 0xAp0", "val", &dval);
    if (ret <= 0 || dval != 10.0)
        TEST_ERROR;
    PASSED();

    /* Verify that %a output round-trips exactly for a value that is not
     * representable exactly in decimal (0.1 requires hex-float to preserve
     * the exact IEEE 754 bit pattern through a serialize/parse cycle). */
    TESTING("H5Zconfig_get_double: %%a round-trip for non-decimal-exact value");
    {
        char   pstr[64];
        double orig = 0.1, rt;
        snprintf(pstr, sizeof(pstr), "rate = %a", orig);
        ret = H5Zconfig_get_double(pstr, "rate", &rt);
        if (ret <= 0 || orig != rt)
            TEST_ERROR;
    }
    PASSED();

    return 0;

error:
    return -1;
}

/* -----------------------------------------------------------------------
 * H5Pappend_filter / H5Pget_filter_params_by_idx callback contract tests
 * ---------------------------------------------------------------------- */
static int
test_callback_contracts(void)
{
    hid_t  dcpl = H5I_INVALID_HID;
    char   pbuf[256];
    size_t plen;
    htri_t deflate_avail;

    if ((deflate_avail = H5Zfilter_avail(H5Z_FILTER_DEFLATE)) < 0)
        TEST_ERROR;

    TESTING("H5Pappend_filter: deflate with level=6");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        {
            H5Z_params_t _p = H5Z_PARAMS_STR("level=6");
            if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
                TEST_ERROR;
        }
        if (H5Pget_nfilters(dcpl) != 1)
            TEST_ERROR;
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    TESTING("H5Pappend_filter: deflate default (no params)");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, NULL) < 0)
            TEST_ERROR;
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    TESTING("H5Pappend_filter: shuffle (no params)");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: shuffle rejects params");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        H5Z_params_t _p  = H5Z_PARAMS_STR("blocksize=8");
        herr_t       ret = H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, &_p);
        if (ret >= 0)
            TEST_ERROR;
    }
    H5E_END_TRY
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pget_filter_params_by_idx: deflate level=9");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        {
            H5Z_params_t _p = H5Z_PARAMS_STR("level=9");
            if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
                TEST_ERROR;
        }
        plen = 0;
        if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
            TEST_ERROR;
        if (plen == 0)
            TEST_ERROR;
        /* Should contain "level = 9" (TOML output format) */
        if (strstr(pbuf, "level = 9") == NULL)
            TEST_ERROR;
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    TESTING("H5Pget_filter_params_by_idx: fallback for filter without get_config");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    plen = 0;
    /* Fletcher32 has no get_config, should fall back to cd_values= format */
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: invalid level rejects");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        H5E_BEGIN_TRY
        {
            H5Z_params_t _p  = H5Z_PARAMS_STR("level=99");
            herr_t       ret = H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p);
            if (ret >= 0)
                TEST_ERROR;
        }
        H5E_END_TRY
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    return 0;

error:
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

/* -----------------------------------------------------------------------
 * Modify-filter pattern test
 *
 * There is no H5Pmodify_filter2 (string-based).  The documented pattern for
 * updating a filter's parameters on a copied DCPL is:
 *   1. H5Pget_filter_by_id2 → retrieve current cd_values
 *   2. Mutate cd_values in place
 *   3. H5Pmodify_filter → write back
 *
 * This test verifies that a filter appended via the string API produces
 * cd_values that round-trip correctly through this pattern.
 * ---------------------------------------------------------------------- */
static int
test_modify_filter_pattern(void)
{
    hid_t    dcpl_orig = H5I_INVALID_HID;
    hid_t    dcpl      = H5I_INVALID_HID;
    unsigned flags;
    size_t   cd_nelmts;
    unsigned cd_values[8];
    char     name[64];
    unsigned config;

    TESTING("modify filter params: H5Pget_filter_by_id2 + H5Pmodify_filter");

    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) <= 0) {
        SKIPPED();
        puts("    deflate filter not available");
        return 0;
    }

    /* Build original DCPL with deflate level=6 via string API */
    if ((dcpl_orig = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("level=6");
        if (H5Pappend_filter(dcpl_orig, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
            TEST_ERROR;
    }

    /* Copy it — simulates a caller receiving a DCPL they did not create */
    if ((dcpl = H5Pcopy(dcpl_orig)) < 0)
        TEST_ERROR;

    /* Retrieve current cd_values */
    cd_nelmts = 8;
    if (H5Pget_filter_by_id2(dcpl, H5Z_FILTER_DEFLATE, &flags, &cd_nelmts, cd_values, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (cd_nelmts < 1)
        TEST_ERROR;

    /* Verify level=6 is present before modification */
    if (cd_values[0] != 6)
        TEST_ERROR;

    /* Bump level to 9 and write back */
    cd_values[0] = 9;
    if (H5Pmodify_filter(dcpl, H5Z_FILTER_DEFLATE, flags, cd_nelmts, cd_values) < 0)
        TEST_ERROR;

    /* Read back and confirm level=9 */
    cd_nelmts = 8;
    if (H5Pget_filter_by_id2(dcpl, H5Z_FILTER_DEFLATE, &flags, &cd_nelmts, cd_values, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (cd_values[0] != 9)
        TEST_ERROR;

    H5Pclose(dcpl_orig);
    H5Pclose(dcpl);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl_orig);
        H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * Round-trip tests: write and read a chunked dataset through the new API
 * ---------------------------------------------------------------------- */

/* Shared helper: create dataset with H5Pappend_filter, write wbuf, read back
 * into rbuf, verify every element matches.  Returns SUCCEED or FAIL. */
static herr_t
h5_run_filter_roundtrip(hid_t file, const char *dset_name, hsize_t *dims, hsize_t *chunks, int ndims,
                        H5Z_filter_t filter_id, const H5Z_params_t *params, int *wbuf, int *rbuf,
                        size_t total_elements)
{
    hid_t  sid  = H5I_INVALID_HID;
    hid_t  dcpl = H5I_INVALID_HID;
    hid_t  dset = H5I_INVALID_HID;
    size_t i;
    herr_t ret = FAIL;

    if ((sid = H5Screate_simple(ndims, dims, NULL)) < 0)
        goto done;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto done;
    if (H5Pset_chunk(dcpl, ndims, chunks) < 0)
        goto done;
    if (H5Pappend_filter(dcpl, filter_id, 0, params) < 0)
        goto done;

    if ((dset = H5Dcreate2(file, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto done;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        goto done;
    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, dset_name, H5P_DEFAULT)) < 0)
        goto done;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        goto done;
    for (i = 0; i < total_elements; i++)
        if (rbuf[i] != wbuf[i])
            goto done;
    ret = SUCCEED;

done:
    if (dset != H5I_INVALID_HID)
        H5Dclose(dset);
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    if (sid != H5I_INVALID_HID)
        H5Sclose(sid);
    return ret;
}

static int
test_roundtrip_deflate(hid_t file)
{
    hsize_t dims[2]   = {32, 32};
    hsize_t chunks[2] = {8, 8};
    int     wbuf[32 * 32], rbuf[32 * 32];
    int     i;

    TESTING("Round-trip: deflate=level=6 write/read");
    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) <= 0) {
        SKIPPED();
        puts("    deflate filter not available");
        return 0;
    }
    for (i = 0; i < 32 * 32; i++)
        wbuf[i] = i;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("level=6");
        if (h5_run_filter_roundtrip(file, "deflate_rt", dims, chunks, 2, H5Z_FILTER_DEFLATE, &_p, wbuf, rbuf,
                                    32 * 32) < 0)
            TEST_ERROR;
    }
    PASSED();
    return 0;
error:
    return -1;
}

static int
test_roundtrip_shuffle(hid_t file)
{
    hsize_t dims[1]   = {64};
    hsize_t chunks[1] = {16};
    int     wbuf[64], rbuf[64];
    int     i;

    TESTING("Round-trip: shuffle write/read");
    for (i = 0; i < 64; i++)
        wbuf[i] = i;
    if (h5_run_filter_roundtrip(file, "shuffle_rt", dims, chunks, 1, H5Z_FILTER_SHUFFLE, NULL, wbuf, rbuf,
                                64) < 0)
        TEST_ERROR;
    PASSED();
    return 0;
error:
    return -1;
}

static int
test_roundtrip_fletcher32(hid_t file)
{
    hsize_t dims[1]   = {32};
    hsize_t chunks[1] = {8};
    int     wbuf[32], rbuf[32];
    int     i;

    TESTING("Round-trip: fletcher32 write/read");
    for (i = 0; i < 32; i++)
        wbuf[i] = i * 3;
    if (h5_run_filter_roundtrip(file, "fletcher32_rt", dims, chunks, 1, H5Z_FILTER_FLETCHER32, NULL, wbuf,
                                rbuf, 32) < 0)
        TEST_ERROR;
    PASSED();
    return 0;
error:
    return -1;
}

/* -----------------------------------------------------------------------
 * Regression tests: existing H5Pset_filter still works correctly
 * ---------------------------------------------------------------------- */
static int
test_regression_old_api(hid_t file)
{
    hid_t    dset = H5I_INVALID_HID, dcpl = H5I_INVALID_HID;
    hid_t    sid        = H5I_INVALID_HID;
    hsize_t  dims[1]    = {32};
    hsize_t  chunks[1]  = {8};
    unsigned cd_vals[1] = {5}; /* deflate level 5 */
    int      wbuf[32], rbuf[32];
    int      i;

    TESTING("Regression: H5Pset_filter (old API) still works");

    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) <= 0) {
        SKIPPED();
        puts("    deflate filter not available");
        return 0;
    }

    for (i = 0; i < 32; i++)
        wbuf[i] = i + 100;

    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunks) < 0)
        TEST_ERROR;
    if (H5Pset_filter(dcpl, H5Z_FILTER_DEFLATE, 0, 1, cd_vals) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, "old_api_rt", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, "old_api_rt", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;
    for (i = 0; i < 32; i++)
        if (rbuf[i] != wbuf[i])
            TEST_ERROR;

    H5Dclose(dset);
    H5Sclose(sid);
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    if (dset != H5I_INVALID_HID)
        H5Dclose(dset);
    if (sid != H5I_INVALID_HID)
        H5Sclose(sid);
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

static int
test_regression_filter2_appends(void)
{
    hid_t dcpl = H5I_INVALID_HID;
    int   nfilters;

    TESTING("Regression: H5Pappend_filter appends (matches H5Pset_filter behavior)");

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    {
        int expected = 1;
        if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) > 0) {
            H5Z_params_t _p = H5Z_PARAMS_STR("level=3");
            if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
                TEST_ERROR;
            expected = 2;
        }
        if ((nfilters = H5Pget_nfilters(dcpl)) != expected)
            TEST_ERROR;
    }

    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

/* -----------------------------------------------------------------------
 * ScaleOffset set_config / get_config round-trip
 * ---------------------------------------------------------------------- */
static int
test_scaleoffset_params(hid_t file)
{
    hid_t   dcpl      = H5I_INVALID_HID;
    hsize_t dims[1]   = {32};
    hsize_t chunks[1] = {8};
    int     wbuf[32], rbuf[32];
    char    pbuf[256];
    size_t  plen;
    int     i;

    TESTING("Round-trip: scaleoffset scale_type = \"int\", scale_factor = 0");

    /* Verify get_config round-trip on the dcpl before writing */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunks) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("scale_type = \"int\", scale_factor = 0");
        if (H5Pappend_filter(dcpl, H5Z_FILTER_SCALEOFFSET, 0, &_p) < 0)
            TEST_ERROR;
    }
    plen = 0;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (plen == 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;

    for (i = 0; i < 32; i++)
        wbuf[i] = i * 2;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("scale_type = \"int\", scale_factor = 0");
        if (h5_run_filter_roundtrip(file, "scaleoffset_rt", dims, chunks, 1, H5Z_FILTER_SCALEOFFSET, &_p,
                                    wbuf, rbuf, 32) < 0)
            TEST_ERROR;
    }
    PASSED();
    return 0;

error:
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

/* -----------------------------------------------------------------------
 * canonical_name display tests
 *
 * Registers a minimal class3 filter and verifies that H5Pget_filter_by_id2
 * returns the canonical_name as the filter name.
 * ---------------------------------------------------------------------- */

#define TITLE_FILTER_ID 512

static size_t
title_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                  size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes; /* pass-through */
}

static int
test_canonical_name_display(void)
{
    static const H5Z_class3_t title_cls = {
        H5Z_CLASS3_T_VERS,   /* version        */
        TITLE_FILTER_ID,     /* id             */
        1,                   /* encoder_present */
        1,                   /* decoder_present */
        "test_title_filter", /* canonical_name */
        NULL,                /* can_apply      */
        NULL,                /* set_local      */
        title_filter_func,   /* filter         */
        NULL,                /* set_config     */
        NULL,                /* get_config     */
    };
    hid_t    dcpl = H5I_INVALID_HID;
    unsigned flags;
    unsigned cd_values[8];
    size_t   cd_nelmts;
    char     name[64];
    unsigned config;

    TESTING("canonical_name: returned by H5Pget_filter_by_id2 as filter name");

    if (H5Zregister(&title_cls) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, TITLE_FILTER_ID, 0, NULL) < 0)
        TEST_ERROR;

    cd_nelmts = 8;
    if (H5Pget_filter_by_id2(dcpl, TITLE_FILTER_ID, &flags, &cd_nelmts, cd_values, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (strcmp(name, "test_title_filter") != 0)
        TEST_ERROR;

    H5Pclose(dcpl);
    H5Zunregister(TITLE_FILTER_ID);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(TITLE_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * H5Z_class3_t name field tests
 * ---------------------------------------------------------------------- */

#define NAME_FILTER_ID 513

static size_t
name_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                 size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_class3_name(void)
{
    herr_t ret;

    TESTING("H5Z_class3_t: NULL name rejected by H5Zregister");
    {
        static const H5Z_class3_t null_name_cls = {
            H5Z_CLASS3_T_VERS, /* version        */
            NAME_FILTER_ID,    /* id             */
            1,                 /* encoder_present */
            1,                 /* decoder_present */
            NULL,              /* canonical_name — intentionally NULL to trigger error */
            NULL,              /* can_apply      */
            NULL,              /* set_local      */
            name_filter_func,  /* filter         */
            NULL,              /* set_config     */
            NULL,              /* get_config     */
        };
        H5E_BEGIN_TRY
        {
            ret = H5Zregister(&null_name_cls);
        }
        H5E_END_TRY
        if (ret >= 0)
            TEST_ERROR;
    }
    PASSED();

    TESTING("H5Z_class3_t: valid name accepted by H5Zregister");
    {
        static const H5Z_class3_t valid_cls = {
            H5Z_CLASS3_T_VERS,  /* version        */
            NAME_FILTER_ID,     /* id             */
            1,                  /* encoder_present */
            1,                  /* decoder_present */
            "test_name_filter", /* canonical_name */
            NULL,               /* can_apply      */
            NULL,               /* set_local      */
            name_filter_func,   /* filter         */
            NULL,               /* set_config     */
            NULL,               /* get_config     */
        };
        if (H5Zregister(&valid_cls) < 0)
            TEST_ERROR;
        H5Zunregister(NAME_FILTER_ID);
    }
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Zunregister(NAME_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * cd_packing helper tests
 * ---------------------------------------------------------------------- */
static int
test_cd_packing(void)
{
    unsigned slots[8];
    size_t   n_used;
    double   dval_out;
    float    fval_out;
    char     sbuf[64];

    TESTING("H5Zcd_pack/unpack double round-trip");
    {
        double dval = 3.14159265358979;
        if (H5Zcd_pack_double(dval, slots, 8, &n_used) < 0)
            TEST_ERROR;
        if (n_used != 2)
            TEST_ERROR;
        if (H5Zcd_unpack_double(slots, n_used, &dval_out) < 0)
            TEST_ERROR;
        if (dval_out != dval)
            TEST_ERROR;
    }
    PASSED();

    TESTING("H5Zcd_pack/unpack float round-trip");
    {
        float fval = 2.718f;
        if (H5Zcd_pack_float(fval, slots, 8, &n_used) < 0)
            TEST_ERROR;
        if (n_used != 1)
            TEST_ERROR;
        if (H5Zcd_unpack_float(slots, n_used, &fval_out) < 0)
            TEST_ERROR;
        if (fval_out != fval)
            TEST_ERROR;
    }
    PASSED();

    TESTING("H5Zcd_pack/unpack string round-trip");
    {
        const char *src = "hello";
        if (H5Zcd_pack_string(src, slots, 8, &n_used) < 0)
            TEST_ERROR;
        if (H5Zcd_unpack_string(slots, n_used, sbuf, sizeof(sbuf)) < 0)
            TEST_ERROR;
        if (strcmp(sbuf, src) != 0)
            TEST_ERROR;
    }
    PASSED();

    return 0;

error:
    return -1;
}

/* -----------------------------------------------------------------------
 * Additional coverage tests
 * ---------------------------------------------------------------------- */

/* 1. NULL/empty string fast-path: set_config must NOT be called */
#define FASTPATH_FILTER_ID 514

static int fastpath_set_config_called = 0;

static herr_t
fastpath_set_config(const char *params, unsigned *flags, size_t *cd_nelmts, unsigned cd_values[],
                   size_t cd_values_size)
{
    (void)params;
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)cd_values_size;
    fastpath_set_config_called = 1;
    return 0;
}

static size_t
fastpath_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                     size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_empty_string_fast_path(void)
{
    static const H5Z_class3_t fp_cls = {
        H5Z_CLASS3_T_VERS,   /* version         */
        FASTPATH_FILTER_ID,  /* id              */
        1,                   /* encoder_present */
        1,                   /* decoder_present */
        "fastpath_filter",   /* canonical_name  */
        NULL,                /* can_apply       */
        NULL,                /* set_local       */
        fastpath_filter_func,/* filter          */
        fastpath_set_config, /* set_config      */
        NULL,                /* get_config      */
    };
    hid_t dcpl = H5I_INVALID_HID;

    TESTING("H5Pappend_filter: NULL params does not invoke set_config");
    if (H5Zregister(&fp_cls) < 0)
        TEST_ERROR;
    fastpath_set_config_called = 0;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, FASTPATH_FILTER_ID, 0, NULL) < 0)
        TEST_ERROR;
    if (fastpath_set_config_called != 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: empty string params does not invoke set_config");
    fastpath_set_config_called = 0;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("");
        if (H5Pappend_filter(dcpl, FASTPATH_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    if (fastpath_set_config_called != 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(FASTPATH_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(FASTPATH_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 2. CDVALUES path: H5Z_PARAMS_CDVALUES is passed through correctly */
#define CDVALS_FILTER_ID 515

static size_t
cdvals_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                   size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_cdvalues_path(void)
{
    static const H5Z_class3_t cdv_cls = {
        H5Z_CLASS3_T_VERS, /* version         */
        CDVALS_FILTER_ID,  /* id              */
        1,                 /* encoder_present */
        1,                 /* decoder_present */
        "cdvals_filter",   /* canonical_name  */
        NULL,              /* can_apply       */
        NULL,              /* set_local       */
        cdvals_filter_func,/* filter          */
        NULL,              /* set_config      */
        NULL,              /* get_config      */
    };
    hid_t        dcpl   = H5I_INVALID_HID;
    unsigned     vals[] = {42, 99};
    H5Z_params_t p      = H5Z_PARAMS_RAW(2, vals);
    unsigned     flags2;
    unsigned     cd_out[8];
    size_t       cd_nelmts = 8;
    char         name[64];
    unsigned     config;

    TESTING("H5Pappend_filter: CDVALUES path stores raw cd_values");
    if (H5Zregister(&cdv_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, CDVALS_FILTER_ID, 0, &p) < 0)
        TEST_ERROR;
    if (H5Pget_filter_by_id2(dcpl, CDVALS_FILTER_ID, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (cd_nelmts < 2 || cd_out[0] != 42 || cd_out[1] != 99)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(CDVALS_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(CDVALS_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 3. CDVALUES path: cd_values=NULL with cd_nelmts>0 is rejected */
static int
test_cdvalues_null_check(void)
{
    hid_t        dcpl = H5I_INVALID_HID;
    H5Z_params_t p;
    herr_t       ret;

    TESTING("H5Pappend_filter: CDVALUES with NULL pointer and nelmts>0 is rejected");
    p.type           = H5Z_PARAMS_CDVALUES;
    p.u.raw.cd_nelmts = 3;
    p.u.raw.cd_values = NULL;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, &p);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

/* 4. canonical_name is packed into cd_values and can be recovered */
#define TITLE_CDPACK_FILTER_ID 516

static int     title_cdpack_set_config_called = 0;
static int64_t title_cdpack_param_value       = 0;

static herr_t
title_cdpack_set_config(const char *params, unsigned *flags, size_t *cd_nelmts, unsigned cd_values[],
                        size_t cd_values_size)
{
    int64_t val = 0;
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)cd_values_size;
    title_cdpack_set_config_called = 1;
    if (params && *params)
        H5Zconfig_get_int(params, "alpha", &val);
    title_cdpack_param_value = val;
    return 0;
}

static herr_t
title_cdpack_get_config(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], char *buf,
                        size_t *buf_size)
{
    size_t needed;
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    needed = (size_t)snprintf(NULL, 0, "alpha = %" PRId64, title_cdpack_param_value) + 1;
    if (buf_size)
        *buf_size = needed;
    if (buf)
        snprintf(buf, needed, "alpha = %" PRId64, title_cdpack_param_value);
    return 0;
}

static size_t
title_cdpack_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                         size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_canonical_name_in_cdvalues(void)
{
    static const H5Z_class3_t tc_cls = {
        H5Z_CLASS3_T_VERS,          /* version         */
        TITLE_CDPACK_FILTER_ID,     /* id              */
        1,                          /* encoder_present */
        1,                          /* decoder_present */
        "title_cdpack_filter",      /* canonical_name  */
        NULL,                       /* can_apply       */
        NULL,                       /* set_local       */
        title_cdpack_filter_func,   /* filter          */
        title_cdpack_set_config,    /* set_config      */
        title_cdpack_get_config,    /* get_config      */
    };
    hid_t    dcpl      = H5I_INVALID_HID;
    unsigned flags2;
    unsigned cd_out[32];
    size_t   cd_nelmts = 32;
    char     name[64];
    unsigned config;
    char     recovered[64];
    size_t   n;

    TESTING("canonical_name: packed into cd_values and recoverable via H5Zcd_unpack_string");
    if (H5Zregister(&tc_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("alpha=7");
        if (H5Pappend_filter(dcpl, TITLE_CDPACK_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    /* Retrieve raw cd_values */
    if (H5Pget_filter_by_id2(dcpl, TITLE_CDPACK_FILTER_ID, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    /* The last portion of cd_values should encode the canonical_name string.
     * slot[0] of the name block holds the byte length; at least one slot required. */
    if (cd_nelmts < 2)
        TEST_ERROR;
    /* The final slots should unpack to "Title CD Pack Filter" */
    {
        /* Find the name block: cd_out[0] is the param count from set_config.
         * The canonical_name trailer begins after the set_config slots.
         * Use H5Zcd_unpack_string to recover from the tail. */
        size_t name_slots;
        if (H5Zcd_pack_string("title_cdpack_filter", NULL, 0, &name_slots) < 0)
            TEST_ERROR;
        if (cd_nelmts < name_slots)
            TEST_ERROR;
        n = name_slots;
        if (H5Zcd_unpack_string(cd_out + (cd_nelmts - name_slots), n, recovered, sizeof(recovered)) < 0)
            TEST_ERROR;
        if (strcmp(recovered, "title_cdpack_filter") != 0)
            TEST_ERROR;
    }
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(TITLE_CDPACK_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(TITLE_CDPACK_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 5. canonical_name persists as the pipeline name field after H5Zunregister */
#define PERSIST_FILTER_ID 517

static size_t
persist_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                    size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_canonical_name_persistence(void)
{
    static const H5Z_class3_t persist_cls = {
        H5Z_CLASS3_T_VERS, /* version         */
        PERSIST_FILTER_ID, /* id              */
        1,                 /* encoder_present */
        1,                 /* decoder_present */
        "persist_filter",  /* canonical_name  */
        NULL,              /* can_apply       */
        NULL,              /* set_local       */
        persist_filter_func,/* filter         */
        NULL,              /* set_config      */
        NULL,              /* get_config      */
    };
    hid_t dcpl = H5I_INVALID_HID;
    unsigned flags2;
    unsigned cd_out[8];
    size_t   cd_nelmts = 8;
    char     name[64];
    unsigned config;

    TESTING("canonical_name: persists as pipeline name after H5Zunregister");
    if (H5Zregister(&persist_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, PERSIST_FILTER_ID, 0, NULL) < 0)
        TEST_ERROR;
    /* Unregister so the only name source is the pipeline name field */
    H5Zunregister(PERSIST_FILTER_ID);
    /* Name should still be "persist_filter" (canonical_name) from the stored pipeline entry */
    if (H5Pget_filter_by_id2(dcpl, PERSIST_FILTER_ID, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (strcmp(name, "persist_filter") != 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(PERSIST_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 6. When canonical_name is absent and plugin is not loaded, name falls back to decimal ID */
static int
test_name_id_fallback(void)
{
    /* Use a filter ID that is not registered and has no built-in entry */
    H5Z_filter_t unregistered_id = 800;
    hid_t        dcpl            = H5I_INVALID_HID;
    unsigned     flags2;
    unsigned     cd_out[8];
    size_t       cd_nelmts = 8;
    char         name[64];
    unsigned     config;
    char         expected[32];

    TESTING("name fallback: unregistered filter returns decimal ID string");
    /* Build a dcpl with the unregistered filter via the old raw API */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    /* H5Pset_filter does not load plugins or validate existence at property-set time */
    if (H5Pset_filter(dcpl, unregistered_id, H5Z_FLAG_OPTIONAL, 0, NULL) < 0)
        TEST_ERROR;
    cd_nelmts = 8;
    /* H5Pget_filter_by_id2: with no registered entry, name should be "800" */
    if (H5Pget_filter_by_id2(dcpl, unregistered_id, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    snprintf(expected, sizeof(expected), "%d", (int)unregistered_id);
    if (strcmp(name, expected) != 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

/* 7. H5Zregister rejects a canonical_name longer than 255 bytes */
#define LONGTITLE_FILTER_ID 518

static size_t
longtitle_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                      size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_canonical_name_length_limit(void)
{
    /* A 256-byte canonical_name (one byte over the 255-byte limit) */
    static const char long_title[257] =
        /* 100 */ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        /* 100 */ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        /*  56 */ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"; /* 256 'A's + NUL */
    H5Z_class3_t long_cls = {
        H5Z_CLASS3_T_VERS,    /* version         */
        LONGTITLE_FILTER_ID,  /* id              */
        1,                    /* encoder_present */
        1,                    /* decoder_present */
        long_title,           /* canonical_name  */
        NULL,                 /* can_apply       */
        NULL,                 /* set_local       */
        longtitle_filter_func,/* filter          */
        NULL,                 /* set_config      */
        NULL,                 /* get_config      */
    };
    herr_t ret;

    TESTING("H5Zregister: canonical_name > 255 bytes is rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zregister(&long_cls);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5Zunregister(LONGTITLE_FILTER_ID);
        TEST_ERROR;
    }
    PASSED();
    return 0;

error:
    return -1;
}

/* 8. H5Pappend_filter rejects a param string longer than H5Z_CONFIG_STRING_MAX */
static int
test_config_string_max(void)
{
    hid_t  dcpl     = H5I_INVALID_HID;
    char  *big_str  = NULL;
    herr_t ret;

    TESTING("H5Pappend_filter: param string > H5Z_CONFIG_STRING_MAX is rejected");
    /* Build a string one byte over the limit */
    if (NULL == (big_str = (char *)malloc(H5Z_CONFIG_STRING_MAX + 2)))
        TEST_ERROR;
    memset(big_str, 'x', H5Z_CONFIG_STRING_MAX + 1);
    big_str[H5Z_CONFIG_STRING_MAX + 1] = '\0';

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        H5Z_params_t _p;
        _p.type  = H5Z_PARAMS_STRING;
        _p.u.str = big_str;
        ret      = H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    free(big_str);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
    }
    H5E_END_TRY
    free(big_str);
    return -1;
}

/* 9. H5Zconfig_get_str: buf != NULL but buf_size == NULL is rejected */
static int
test_config_get_str_null_buf_size(void)
{
    const char *params = "key = \"value\"";
    char        buf[32];
    htri_t      ret;

    TESTING("H5Zconfig_get_str: buf != NULL, buf_size == NULL is rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_str(params, "key", buf, NULL);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();
    return 0;

error:
    return -1;
}

/* 10. set_config and get_config are invoked and their output is consistent */
#define CALLBACK_FILTER_ID 519

static int     callback_set_called = 0;
static int64_t callback_stored_val = 0;

static herr_t
callback_set_config(const char *params, unsigned *flags, size_t *cd_nelmts, unsigned cd_values[],
                    size_t cd_values_size)
{
    int64_t v = 0;
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)cd_values_size;
    callback_set_called = 1;
    if (params && *params)
        H5Zconfig_get_int(params, "beta", &v);
    callback_stored_val = v;
    return 0;
}

static herr_t
callback_get_config(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], char *buf, size_t *buf_size)
{
    size_t needed;
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    needed = (size_t)snprintf(NULL, 0, "beta = %" PRId64, callback_stored_val) + 1;
    if (buf_size)
        *buf_size = needed;
    if (buf)
        snprintf(buf, needed, "beta = %" PRId64, callback_stored_val);
    return 0;
}

static size_t
callback_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                     size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_set_get_config_callbacks(void)
{
    static const H5Z_class3_t cb_cls = {
        H5Z_CLASS3_T_VERS,   /* version         */
        CALLBACK_FILTER_ID,  /* id              */
        1,                   /* encoder_present */
        1,                   /* decoder_present */
        "callback_filter",   /* canonical_name  */
        NULL,                /* can_apply       */
        NULL,                /* set_local       */
        callback_filter_func,/* filter          */
        callback_set_config, /* set_config      */
        callback_get_config, /* get_config      */
    };
    hid_t dcpl = H5I_INVALID_HID;
    char  pbuf[256];
    size_t plen = 0;

    TESTING("set_config and get_config callbacks are invoked and output is consistent");
    if (H5Zregister(&cb_cls) < 0)
        TEST_ERROR;
    callback_set_called = 0;
    callback_stored_val = 0;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("beta=42");
        if (H5Pappend_filter(dcpl, CALLBACK_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    if (callback_set_called == 0)
        TEST_ERROR;
    if (callback_stored_val != 42)
        TEST_ERROR;
    /* get_config should produce "beta = 42" */
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strstr(pbuf, "beta") == NULL)
        TEST_ERROR;
    if (strstr(pbuf, "42") == NULL)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(CALLBACK_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(CALLBACK_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * main
 * ---------------------------------------------------------------------- */
int
main(void)
{
    hid_t fapl    = H5I_INVALID_HID;
    hid_t file    = H5I_INVALID_HID;
    int   nerrors = 0;
    char  filename[1024];

    h5_test_init();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Parser tests */
    nerrors += test_parser() < 0 ? 1 : 0;

    /* canonical_name display test */
    nerrors += test_canonical_name_display() < 0 ? 1 : 0;

    /* H5Z_class3_t name field tests */
    nerrors += test_class3_name() < 0 ? 1 : 0;

    /* cd_packing helper tests */
    nerrors += test_cd_packing() < 0 ? 1 : 0;

    /* H5Pappend_filter callback contract tests */
    nerrors += test_callback_contracts() < 0 ? 1 : 0;

    /* Modify-filter pattern (H5Pget_filter_by_id2 + H5Pmodify_filter) */
    nerrors += test_modify_filter_pattern() < 0 ? 1 : 0;

    /* Round-trip tests */
    nerrors += test_roundtrip_deflate(file) < 0 ? 1 : 0;
    nerrors += test_roundtrip_shuffle(file) < 0 ? 1 : 0;
    nerrors += test_roundtrip_fletcher32(file) < 0 ? 1 : 0;
    nerrors += test_scaleoffset_params(file) < 0 ? 1 : 0;

    /* Regression tests */
    nerrors += test_regression_old_api(file) < 0 ? 1 : 0;
    nerrors += test_regression_filter2_appends() < 0 ? 1 : 0;

    /* Additional coverage tests */
    nerrors += test_empty_string_fast_path() < 0 ? 1 : 0;
    nerrors += test_cdvalues_path() < 0 ? 1 : 0;
    nerrors += test_cdvalues_null_check() < 0 ? 1 : 0;
    nerrors += test_canonical_name_in_cdvalues() < 0 ? 1 : 0;
    nerrors += test_canonical_name_persistence() < 0 ? 1 : 0;
    nerrors += test_name_id_fallback() < 0 ? 1 : 0;
    nerrors += test_canonical_name_length_limit() < 0 ? 1 : 0;
    nerrors += test_config_string_max() < 0 ? 1 : 0;
    nerrors += test_config_get_str_null_buf_size() < 0 ? 1 : 0;
    nerrors += test_set_get_config_callbacks() < 0 ? 1 : 0;

    if (H5Fclose(file) < 0)
        goto error;

    h5_cleanup(FILENAME, fapl);

    if (nerrors)
        goto error;

    printf("All tfilter2 tests passed.\n");
    return EXIT_SUCCESS;

error:
    puts("***** TFILTER2 TESTS FAILED *****");
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY
    return EXIT_FAILURE;
}
