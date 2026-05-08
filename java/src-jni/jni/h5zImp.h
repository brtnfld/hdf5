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

#include <jni.h>
/* Header for class hdf_hdf5lib_H5_H5Z */

#ifndef Included_hdf_hdf5lib_H5_H5Z
#define Included_hdf_hdf5lib_H5_H5Z

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zunregister
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zunregister(JNIEnv *, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zfilter_avail
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zfilter_1avail(JNIEnv *, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zget_filter_info
 * Signature: (I)I
 */

JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zget_1filter_1info(JNIEnv *, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_has_key
 * Signature: (Ljava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zconfig_1has_1key(JNIEnv *, jclass, jstring, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_int
 * Signature: (Ljava/lang/String;Ljava/lang/String;[J)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zconfig_1get_1int(JNIEnv *, jclass, jstring, jstring,
                                                               jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_double
 * Signature: (Ljava/lang/String;Ljava/lang/String;[D)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zconfig_1get_1double(JNIEnv *, jclass, jstring, jstring,
                                                                  jdoubleArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_bool
 * Signature: (Ljava/lang/String;Ljava/lang/String;[Z)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zconfig_1get_1bool(JNIEnv *, jclass, jstring, jstring,
                                                                jbooleanArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_str
 * Signature: (Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Zconfig_1get_1str(JNIEnv *, jclass, jstring, jstring,
                                                               jobjectArray);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* Included_hdf_hdf5lib_H5_H5Z */
