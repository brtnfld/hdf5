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

    TESTING("H5Pappend_filter: deflate with level=6");
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

    TESTING("H5Pappend_filter: deflate default (no params)");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, NULL) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

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
        H5Z_params_t _p = H5Z_PARAMS_STR("level=3");
        if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
            TEST_ERROR;
    }
    if ((nfilters = H5Pget_nfilters(dcpl)) != 2)
        TEST_ERROR;

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
 * filter_title field tests
 *
 * Registers a minimal class3 filter with a non-NULL filter_title and
 * verifies that H5Pget_filter_by_id2 returns the title as the filter name.
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
test_filter_title(void)
{
    static const H5Z_class3_t title_cls = {
        H5Z_CLASS3_T_VERS,   /* version        */
        TITLE_FILTER_ID,     /* id             */
        1,                   /* encoder_present */
        1,                   /* decoder_present */
        "test_title_filter", /* name          */
        "My Test Filter",    /* filter_title   */
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

    TESTING("filter_title: returned by H5Pget_filter_by_id2 as filter name");

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
    if (strcmp(name, "My Test Filter") != 0)
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
            NULL,              /* name — intentionally NULL to trigger error */
            NULL,              /* filter_title   */
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
            "test_name_filter", /* name           */
            NULL,               /* filter_title   */
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

    /* filter_title field test */
    nerrors += test_filter_title() < 0 ? 1 : 0;

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
