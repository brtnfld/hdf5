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
 *   - H5Pset_filter2 / H5Pget_filter_params_by_idx
 *   - H5Zconfig_get_param parser
 *   - Built-in filter set_config / get_config round-trips
 *   - Name registry (H5Z_filter_id_by_name)
 *   - Regression: existing H5Pset_filter still works
 */

#include "h5test.h"

static const char *FILENAME[] = {"tfilter2", NULL};

/* -----------------------------------------------------------------------
 * Parser tests (H5Zconfig_get_param)
 * ---------------------------------------------------------------------- */
static int
test_parser(void)
{
    char   vbuf[256];
    size_t vsz;
    htri_t ret;

    TESTING("H5Zconfig_get_param basic lookup");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_param("level=6,mode=fast", "level", vbuf, &vsz);
    if (ret <= 0)
        TEST_ERROR;
    if (strcmp(vbuf, "6") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_param key not found");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_param("level=6", "mode", vbuf, &vsz);
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_param bare key (no value)");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_param("verbose", "verbose", vbuf, &vsz);
    if (ret <= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_param quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_param("name=\"hello world\"", "name", vbuf, &vsz);
    if (ret <= 0)
        TEST_ERROR;
    if (strcmp(vbuf, "hello world") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_param NULL params error");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = H5Zconfig_get_param(NULL, "key", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_param NULL key error");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = H5Zconfig_get_param("level=6", NULL, vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_param duplicate key error");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = H5Zconfig_get_param("level=6,level=9", "level", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_param whitespace stripping");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_param("  level = 6 , mode = fast ", "level", vbuf, &vsz);
    if (ret <= 0)
        TEST_ERROR;
    if (strcmp(vbuf, "6") != 0)
        TEST_ERROR;
    PASSED();

    return 0;

error:
    return -1;
}

/* -----------------------------------------------------------------------
 * H5Pset_filter2 / H5Pget_filter_params_by_idx callback contract tests
 * ---------------------------------------------------------------------- */
static int
test_callback_contracts(void)
{
    hid_t  dcpl = H5I_INVALID_HID;
    char   pbuf[256];
    size_t plen;

    TESTING("H5Pset_filter2: deflate with level=6");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_DEFLATE, 0, "level=6") < 0)
        TEST_ERROR;
    if (H5Pget_nfilters(dcpl) != 1)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pset_filter2: deflate default (no params)");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_DEFLATE, 0, NULL) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pset_filter2: shuffle (no params)");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pset_filter2: shuffle rejects params");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        herr_t ret = H5Pset_filter2(dcpl, H5Z_FILTER_SHUFFLE, 0, "blocksize=8");
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
    if (H5Pset_filter2(dcpl, H5Z_FILTER_DEFLATE, 0, "level=9") < 0)
        TEST_ERROR;
    plen = 0;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (plen == 0)
        TEST_ERROR;
    /* Should contain "level=9" */
    if (strstr(pbuf, "level=9") == NULL)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pget_filter_params_by_idx: fallback for filter without get_config");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    plen = 0;
    /* Fletcher32 has no get_config, should fall back to cd_values= format */
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pset_filter2: invalid level rejects");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        herr_t ret = H5Pset_filter2(dcpl, H5Z_FILTER_DEFLATE, 0, "level=99");
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
 * Round-trip tests: write and read a chunked dataset through the new API
 * ---------------------------------------------------------------------- */
static int
test_roundtrip_deflate(hid_t file)
{
    hid_t   dset = H5I_INVALID_HID, dcpl = H5I_INVALID_HID;
    hid_t   sid       = H5I_INVALID_HID;
    hsize_t dims[2]   = {32, 32};
    hsize_t chunks[2] = {8, 8};
    int     wbuf[32][32], rbuf[32][32];
    int     i, j;

    TESTING("Round-trip: deflate=level=6 write/read");

    for (i = 0; i < 32; i++)
        for (j = 0; j < 32; j++)
            wbuf[i][j] = i * 32 + j;

    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunks) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_DEFLATE, 0, "level=6") < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, "deflate_rt", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, "deflate_rt", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;
    for (i = 0; i < 32; i++)
        for (j = 0; j < 32; j++)
            if (rbuf[i][j] != wbuf[i][j])
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
test_roundtrip_shuffle(hid_t file)
{
    hid_t   dset = H5I_INVALID_HID, dcpl = H5I_INVALID_HID;
    hid_t   sid       = H5I_INVALID_HID;
    hsize_t dims[1]   = {64};
    hsize_t chunks[1] = {16};
    int     wbuf[64], rbuf[64];
    int     i;

    TESTING("Round-trip: shuffle write/read");

    for (i = 0; i < 64; i++)
        wbuf[i] = i;

    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunks) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, "shuffle_rt", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, "shuffle_rt", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;
    for (i = 0; i < 64; i++)
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
test_roundtrip_fletcher32(hid_t file)
{
    hid_t   dset = H5I_INVALID_HID, dcpl = H5I_INVALID_HID;
    hid_t   sid       = H5I_INVALID_HID;
    hsize_t dims[1]   = {32};
    hsize_t chunks[1] = {8};
    int     wbuf[32], rbuf[32];
    int     i;

    TESTING("Round-trip: fletcher32 write/read");

    for (i = 0; i < 32; i++)
        wbuf[i] = i * 3;

    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunks) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_FLETCHER32, 0, NULL) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, "fletcher32_rt", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, "fletcher32_rt", H5P_DEFAULT)) < 0)
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

    TESTING("Regression: H5Pset_filter2 appends (matches H5Pset_filter behavior)");

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_DEFLATE, 0, "level=3") < 0)
        TEST_ERROR;
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
    hid_t   dset = H5I_INVALID_HID, dcpl = H5I_INVALID_HID;
    hid_t   sid       = H5I_INVALID_HID;
    hsize_t dims[1]   = {32};
    hsize_t chunks[1] = {8};
    int     wbuf[32], rbuf[32];
    char    pbuf[256];
    size_t  plen;
    int     i;

    TESTING("Round-trip: scaleoffset scale_type=int,scale_factor=0");

    for (i = 0; i < 32; i++)
        wbuf[i] = i * 2;

    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunks) < 0)
        TEST_ERROR;
    if (H5Pset_filter2(dcpl, H5Z_FILTER_SCALEOFFSET, 0, "scale_type=int,scale_factor=0") < 0)
        TEST_ERROR;

    /* Check get_config round-trip */
    plen = 0;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (plen == 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, "scaleoffset_rt", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, "scaleoffset_rt", H5P_DEFAULT)) < 0)
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

    /* cd_packing helper tests */
    nerrors += test_cd_packing() < 0 ? 1 : 0;

    /* H5Pset_filter2 callback contract tests */
    nerrors += test_callback_contracts() < 0 ? 1 : 0;

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
