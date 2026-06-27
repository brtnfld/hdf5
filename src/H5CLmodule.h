/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Lifeboat, LLC                                                *
 * All rights reserved.                                                      *
 *                                                                           *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the COPYING file, which can be found  *
 * at the root of the source code distribution tree.                         *
 * If you do not have access to either file, you may request a copy from     *
 * help@lifeboat.llc                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: This file contains declarations which define macros for the
 *          H5FDcl package.  Including this header means that the source file
 *          is part of the H5CL package.
 */
#ifndef H5CL_module_H
#define H5CL_module_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5CL_MODULE
#define H5_MY_PKG      H5CL
#define H5_MY_PKG_ERR  H5E_VFL
#define H5_MY_PKG_INIT NO
/**
 * \defgroup H5VFDCL VFD SWMR Configuration Language Features
 * \ingroup H5VFD
 *
 */

#endif /* H5CL_module_H */
