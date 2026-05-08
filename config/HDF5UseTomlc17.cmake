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
# When HDF5_ENABLE_TOMLC17_SUPPORT is ON:
#   1. A system-installed tomlc17 is preferred (found via find_path/find_library).
#   2. If not found on the system, the bundled copy in src/tomlc17/ is used.
#   3. H5_HAVE_TOMLC17 is set and tomlc17 is compiled into libhdf5.
#
# The bundled copy of tomlc17 is located in ${HDF5_SRC_DIR}/tomlc17/ and
# is NOT a permanent vendor: it is fetched separately from the HDF5 source
# tree and is optional.
# -----------------------------------------------------------------------------

option (HDF5_ENABLE_TOMLC17_SUPPORT "Enable tomlc17 TOML parser for H5Z filter parameter strings" OFF)

if (HDF5_ENABLE_TOMLC17_SUPPORT)

  # --- 1. Try to find a system-installed tomlc17 ----------------------------
  find_path (TOMLC17_INCLUDE_DIR
    NAMES tomlc17.h
    PATH_SUFFIXES tomlc17
    DOC "Path to directory containing tomlc17.h"
  )
  find_library (TOMLC17_LIBRARY
    NAMES tomlc17
    DOC "Path to tomlc17 shared/static library"
  )
  mark_as_advanced (TOMLC17_INCLUDE_DIR TOMLC17_LIBRARY)

  if (TOMLC17_INCLUDE_DIR AND TOMLC17_LIBRARY)
    message (STATUS "Found system tomlc17: ${TOMLC17_LIBRARY}")
    set (H5_HAVE_TOMLC17 1)
    set (TOMLC17_USE_BUNDLED FALSE)
    set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${TOMLC17_LIBRARY})

  # --- 2. Fall back to the bundled copy in src/tomlc17/ --------------------
  elseif (EXISTS "${HDF5_SRC_DIR}/tomlc17/tomlc17.h")
    message (STATUS "Using bundled tomlc17 from ${HDF5_SRC_DIR}/tomlc17")
    set (H5_HAVE_TOMLC17 1)
    set (TOMLC17_USE_BUNDLED TRUE)
    set (TOMLC17_INCLUDE_DIR "${HDF5_SRC_DIR}/tomlc17")

  else ()
    message (WARNING
      "HDF5_ENABLE_TOMLC17_SUPPORT=ON but tomlc17 not found "
      "(neither system-installed nor bundled in src/tomlc17/). "
      "Disabling tomlc17 support."
    )
    set (H5_HAVE_TOMLC17 0)
  endif ()

endif ()
