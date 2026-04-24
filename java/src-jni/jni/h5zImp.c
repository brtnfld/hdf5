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

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "hdf5.h"
#include <jni.h>
#include <stdlib.h>
#include "h5jni.h"
#include "h5zImp.h"

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zunregister
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zunregister(JNIEnv *env, jclass clss, jint filter)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Zunregister((H5Z_filter_t)filter)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Zunregister */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zfilter_avail
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zfilter_1avail(JNIEnv *env, jclass clss, jint filter)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Zfilter_avail((H5Z_filter_t)filter)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Zfilter_1avail */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zget_filter_info
 * Signature: (I)I
 */

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zget_1filter_1info(JNIEnv *env, jclass clss, jint filter)
{
    unsigned int flags = 0;

    UNUSED(clss);

    if (H5Zget_filter_info((H5Z_filter_t)filter, (unsigned *)&flags) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)flags;
} /* end Java_hdf_hdf5lib_H5_H5Zget_1filter_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_param
 * Signature: (Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param(JNIEnv *env, jclass clss, jstring params, jstring key,
                                          jobjectArray value)
{
    const char *c_params = NULL;
    const char *c_key    = NULL;
    jboolean    isCopy1, isCopy2;
    char       *c_valbuf = NULL;
    size_t      buf_size = H5Z_CONFIG_STRING_MAX;
    jstring     str;
    htri_t      status = -1;

    UNUSED(clss);

    if (NULL == params)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: params string is NULL");
    if (NULL == key)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: key string is NULL");

    PIN_JAVA_STRING(ENVONLY, params, c_params, &isCopy1, "H5Zconfig_get_param: params not pinned");
    PIN_JAVA_STRING(ENVONLY, key, c_key, &isCopy2, "H5Zconfig_get_param: key not pinned");

    if (NULL == (c_valbuf = (char *)malloc(buf_size + 1)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Zconfig_get_param: malloc failed");

    if ((status = H5Zconfig_get_param(c_params, c_key, c_valbuf, &buf_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    c_valbuf[buf_size < H5Z_CONFIG_STRING_MAX ? buf_size : H5Z_CONFIG_STRING_MAX] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, status > 0 ? c_valbuf : ""))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Zconfig_get_param: could not create string");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, value, 0, str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (c_valbuf)
        free(c_valbuf);
    if (c_key)
        UNPIN_JAVA_STRING(ENVONLY, key, c_key);
    if (c_params)
        UNPIN_JAVA_STRING(ENVONLY, params, c_params);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
