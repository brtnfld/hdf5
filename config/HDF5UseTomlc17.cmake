#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# -----------------------------------------------------------------------------
# HDF5 CMake tomlc17 Support Configuration
# -----------------------------------------------------------------------------
# The tomlc17 TOML parser is always compiled from the vendored copy in
# src/tomlc17/.  H5_HAVE_TOMLC17 is set unconditionally so the tomlc17
# path in H5Zconfig.c is always active.
# -----------------------------------------------------------------------------

set (H5_HAVE_TOMLC17 1)
message (STATUS "tomlc17: using bundled source from ${HDF5_SRC_DIR}/tomlc17")
