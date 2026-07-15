/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5test.h"
#include "genall5.h"

/* The default, do-nothing implementation of the zoo_create_hook_g callback,
 * which is called after each create_zoo() step. Individual tests override
 * zoo_create_hook_g itself (see genall5.h) rather than redefining this
 * function.
 */
static void
zoo_create_hook_default(hid_t H5_ATTR_UNUSED fid)
{
    return;
}

zoo_create_hook_t zoo_create_hook_g = zoo_create_hook_default;
