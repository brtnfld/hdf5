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

package test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Z {
    @Rule
    public TestName testname = new TestName();

    @Before
    public void checkOpenIDs()
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName()
    {
        System.out.println();
    }

    @Test
    public void testH5Zfilter_avail()
    {
        try {
            int filter_found;

            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_FLETCHER32);
            assertTrue("H5.H5Zfilter_avail_FLETCHER32", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_NBIT);
            assertTrue("H5.H5Zfilter_avail_NBIT", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SCALEOFFSET);
            assertTrue("H5.H5Zfilter_avail_SCALEOFFSET", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5.H5Zfilter_avail_SHUFFLE", filter_found > 0);

            // Just make sure H5Zfilter_avail() doesn't fail with szip/zlib
            // since there is no way for us to determine if they should be present
            // or not.
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SZIP);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Zfilter_avail " + err);
        }
    }

    @Test
    public void testH5Zget_filter_info()
    {
        try {
            int filter_flag;

            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_FLETCHER32);
            assertTrue("H5.H5Zget_filter_info_FLETCHER32_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_FLETCHER32_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_NBIT);
            assertTrue("H5.H5Zget_filter_info_NBIT_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_NBIT_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SCALEOFFSET);
            assertTrue("H5.H5Zget_filter_info_SCALEOFFSET_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_SCALEOFFSET_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5.H5Zget_filter_info_DECODE_SHUFFLE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_ENCODE_SHUFFLE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_DEFLATE);
                assertTrue("H5.H5Zget_filter_info_DEFLATE_DECODE_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
                assertTrue("H5.H5Zget_filter_info_DEFLATE_ENCODE_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            }

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SZIP)) {
                filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SZIP);
                // Decode should always be available, but we have no way of determining
                // if encode is so don't assert on that.
                assertTrue("H5.H5Zget_filter_info_DECODE_SZIP_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Zget_filter_info " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Zunregister_predefined() throws Throwable
    {
        int filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SHUFFLE);
        assertTrue("H5.H5Zfilter_avail", filter_found > 0);

        H5.H5Zunregister(HDF5Constants.H5Z_FILTER_SHUFFLE);
    }

    @Test
    public void testH5Pappend_filter_shuffle()
    {
        long dcpl = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl >= 0);

            // Raw cd_values variant (empty array = no params)
            int ret = H5.H5Pappend_filter(dcpl, HDF5Constants.H5Z_FILTER_SHUFFLE, 0, new int[0]);
            assertTrue("H5Pappend_filter(shuffle,raw)", ret >= 0);
            assertTrue("getNfilters after append", H5.H5Pget_nfilters(dcpl) == 1);
            System.out.print(" PASSED");
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pappend_filter_shuffle: " + err);
        }
        finally {
            if (dcpl != HDF5Constants.H5I_INVALID_HID)
                try { H5.H5Pclose(dcpl); } catch (Exception e) { /* ignore */ }
        }
    }

    @Test
    public void testH5Pappend_filter_deflate_string()
    {
        try {
            if (H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE) <= 0) {
                System.out.print(" SKIPPED");
                return;
            }
        }
        catch (Throwable err) {
            fail("H5Zfilter_avail: " + err);
        }

        long dcpl = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl >= 0);

            // String params variant
            int ret = H5.H5Pappend_filter(dcpl, HDF5Constants.H5Z_FILTER_DEFLATE, 0, "level=6");
            assertTrue("H5Pappend_filter(deflate,str)", ret >= 0);
            assertTrue("getNfilters after append", H5.H5Pget_nfilters(dcpl) == 1);

            String[] params = new String[1];
            ret = H5.H5Pget_filter_params_by_idx(dcpl, 0, params);
            assertTrue("H5Pget_filter_params_by_idx", ret >= 0);
            assertTrue("params not empty", params[0] != null && !params[0].isEmpty());
            System.out.print(" PASSED");
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pappend_filter_deflate_string: " + err);
        }
        finally {
            if (dcpl != HDF5Constants.H5I_INVALID_HID)
                try { H5.H5Pclose(dcpl); } catch (Exception e) { /* ignore */ }
        }
    }

    @Test
    public void testH5Zconfig_get_param_int()
    {
        try {
            long[] out = new long[1];
            int ret = H5.H5Zconfig_get_param("level = 6, mode = 2", "level", out);
            assertTrue("H5Zconfig_get_param(int) found", ret > 0);
            assertTrue("H5Zconfig_get_param(int) value", out[0] == 6L);
            System.out.print(" PASSED");
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Zconfig_get_param_int: " + err);
        }
    }
}
