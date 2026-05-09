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
# Configures tomlc17 TOML parser support for the H5Z filter parameter API
# (RFC-HDFG-2026-001).
#
# When HDF5_ENABLE_TOMLC17_SUPPORT is ON the bundled copy of tomlc17 in
# src/tomlc17/ is compiled directly into libhdf5 (no external library).
# H5_HAVE_TOMLC17 is set so the #ifdef path in H5Zconfig.c is active.
# -----------------------------------------------------------------------------

option (HDF5_ENABLE_TOMLC17_SUPPORT "Enable tomlc17 TOML parser for H5Z filter parameter strings" OFF)

if (HDF5_ENABLE_TOMLC17_SUPPORT)
  if (EXISTS "${HDF5_SRC_DIR}/tomlc17/tomlc17.h")
    message (STATUS "tomlc17: using bundled source from ${HDF5_SRC_DIR}/tomlc17")
    set (H5_HAVE_TOMLC17 1)
    set (TOMLC17_INCLUDE_DIR "${HDF5_SRC_DIR}/tomlc17")
  else ()
    message (WARNING
      "HDF5_ENABLE_TOMLC17_SUPPORT=ON but bundled tomlc17 not found in "
      "src/tomlc17/.  Disabling tomlc17 support."
    )
    set (H5_HAVE_TOMLC17 0)
  endif ()
endif ()
