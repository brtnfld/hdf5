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

#if !(defined H5Z_FRIEND || defined H5Z_MODULE)
#error "Do not include this file outside the H5Z package!"
#endif

#ifndef H5Zpkg_H
#define H5Zpkg_H

/* Include private header file */
#include "H5Zprivate.h" /* Filter functions                */

/*
 * Internal filter table entry.  All H5Z_class2_t fields come first so that
 * a pointer to H5Z_entry_t may be safely cast to H5Z_class2_t * where the
 * existing API expects it.  V3-specific fields are zero-initialised for
 * filters registered via H5Z_class2_t or H5Z_class1_t.
 */
typedef struct H5Z_entry_t {
    /* --- H5Z_class2_t compatible fields (must stay first) --- */
    int                  version; /* H5Z_CLASS_T_VERS or H5Z_CLASS3_T_VERS */
    H5Z_filter_t         id;
    unsigned             encoder_present;
    unsigned             decoder_present;
    const char          *name; /* canonical name (or debug comment for v2) */
    H5Z_can_apply_func_t can_apply;
    H5Z_set_local_func_t set_local;
    H5Z_func_t           filter;
    /* --- V3 extensions (NULL for v1/v2 plugins) --- */
    const char           *filter_title; /* human-readable label packed into cd_values; may be NULL */
    H5Z_set_config_func_t set_config;
    H5Z_get_config_func_t get_config;
} H5Z_entry_t;

/********************/
/* Internal filters */
/********************/

/* Shuffle filter */
H5_ATTR_VISIBILITY_HIDDEN extern const H5Z_class3_t H5Z_SHUFFLE[1];

/* Fletcher32 filter */
H5_ATTR_VISIBILITY_HIDDEN extern const H5Z_class3_t H5Z_FLETCHER32[1];

/* n-bit filter */
H5_ATTR_VISIBILITY_HIDDEN extern H5Z_class3_t H5Z_NBIT[1];

/* Scale/offset filter */
H5_ATTR_VISIBILITY_HIDDEN extern H5Z_class3_t H5Z_SCALEOFFSET[1];

/********************/
/* External filters */
/********************/

/* Deflate filter */
#ifdef H5_HAVE_FILTER_DEFLATE
H5_ATTR_VISIBILITY_HIDDEN extern const H5Z_class3_t H5Z_DEFLATE[1];
#endif /* H5_HAVE_FILTER_DEFLATE */

/* szip filter */
#ifdef H5_HAVE_FILTER_SZIP
H5_ATTR_VISIBILITY_HIDDEN extern H5Z_class3_t H5Z_SZIP[1];
#endif /* H5_HAVE_FILTER_SZIP */

/* Package internal routines */
H5_DLL herr_t H5Z__reregister_deflate(void);
H5_DLL herr_t H5Z__unregister(H5Z_filter_t filter_id);
H5_DLL herr_t H5Z__config_validate_keys(const char *params, const char *const *known_keys);
H5_DLL htri_t H5Z__config_get_int(const char *params, const char *key, int64_t *out);
H5_DLL htri_t H5Z__config_get_str(const char *params, const char *key, char *buf, size_t *buf_size);

#endif /* H5Zpkg_H */
