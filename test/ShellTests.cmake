
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

##############################################################################
##############################################################################
###           T E S T I N G  S H E L L  S C R I P T S                      ###
##############################################################################

find_program (PWSH NAMES pwsh powershell)
mark_as_advanced (PWSH)
if (PWSH)
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/use_cases_test")
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/swmr_test")
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/vds_swmr_test")

    set (srcdir ${HDF5_TEST_SOURCE_DIR})
    set (H5_UTILS_TEST_BUILDDIR ${CMAKE_TEST_OUTPUT_DIRECTORY})
    set (H5_TEST_BUILDDIR ${HDF5_TEST_BINARY_DIR}/H5TEST)
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_swmr.pwsh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_swmr.ps1 @ONLY)
    # test commented out as currently the programs are not allowing another access to the data file
    #add_test (H5SHELL-testswmr ${PWSH} ${HDF5_TEST_BINARY_DIR}/H5TEST/testswmr.ps1)
    #set_tests_properties (H5SHELL-testswmr PROPERTIES
    #        ENVIRONMENT "PATH=$ENV{PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    #        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    #)
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_vds_swmr.pwsh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_vds_swmr.ps1 @ONLY)
    # test commented out as currently the programs are not allowing another access to the data file
    #add_test (H5SHELL-testvdsswmr ${PWSH} ${HDF5_TEST_BINARY_DIR}/H5TEST/testvdsswmr.ps1)
    #set_tests_properties (H5SHELL-testvdsswmr PROPERTIES
    #        ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    #        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    #)
elseif (UNIX)
  find_program (SH_PROGRAM bash)
  mark_as_advanced (SH_PROGRAM)
  if (SH_PROGRAM)
    set (srcdir ${HDF5_TEST_SOURCE_DIR})
    set (H5_UTILS_TEST_BUILDDIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    set (H5_TEST_BUILDDIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    ##############################################################################
    #  configure scripts to test dir
    ##############################################################################
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_use_cases.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_use_cases.sh @ONLY)
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_swmr.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_swmr.sh @ONLY)
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_vds_swmr.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_vds_swmr.sh @ONLY)
    if (TARGET aux_process)
      set (AUX_PROCESS "yes")
    else ()
      set (AUX_PROCESS "no")
    endif ()
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_vfd_swmr.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_vfd_swmr.sh @ONLY)

    ##############################################################################
    #  copy test programs to test dir
    ##############################################################################
    add_custom_command (
        TARGET     accum_swmr_reader
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "${HDF5_SOURCE_DIR}/bin/output_filter.sh" "${HDF5_TEST_BINARY_DIR}/H5TEST/bin/output_filter.sh"
    )

    ##############################################################################
    ##############################################################################
    ###           A D D I T I O N A L   T E S T S                              ###
    ##############################################################################
    ##############################################################################
    # H5_CHECK_TESTS
    #---------------
    #    atomic_writer
    #    atomic_reader
    #    filenotclosed
    #    del_many_dense_attrs
    #    flushrefresh
    ##############################################################################
    # autotools script tests
    # error_test and err_compat are built at the same time as the other tests, but executed by test_error.sh
    # NOT CONVERTED accum_swmr_reader is used by accum.c
    # NOT CONVERTED atomic_writer and atomic_reader are stand-alone programs
    # links_env is used by test_links_env.sh
    # filenotclosed and del_many_dense_attrs are used by test_abort_fail.sh
    # NOT CONVERTED flushrefresh is used by test_flush_refresh.sh.
    # NOT CONVERTED use_append_chunk, use_append_mchunks and use_disable_mdc_flushes are used by test_use_cases.sh
    # NOT CONVERTED swmr_* files (besides swmr.c) are used by test_swmr.sh.
    # NOT CONVERTED vds_swmr_* files are used by test_vds_swmr.sh
    # NOT CONVERTED 'make check' doesn't run them directly, so they are not included in TEST_PROG.
    # NOT CONVERTED Also build testmeta, which is used for timing test. It builds quickly
    # NOT CONVERTED and this lets automake keep all its test programs in one place.
    ##############################################################################

    ##############################################################################
    ###    S W M R  T E S T S
    ##############################################################################
    #       test_flush_refresh.sh: flushrefresh
    #       test_use_cases.sh: use_append_chunk, use_append_mchunks, use_disable_mdc_flushes
    #       test_swmr.sh: swmr*
    #       test_vds_swmr.sh: vds_swmr*
    if (H5_PERL_FOUND)
      configure_file(${HDF5_TEST_SOURCE_DIR}/test_flush_refresh.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_flush_refresh.sh @ONLY)
      add_test (H5SHELL-test_flush_refresh ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/test_flush_refresh.sh)
      set_tests_properties (H5SHELL-test_flush_refresh PROPERTIES
              ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
              WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
      if ("H5SHELL-test_flush_refresh" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5SHELL-test_flush_refresh PROPERTIES DISABLED true)
      endif ()
    endif ()

    add_test (H5SHELL-test_use_cases ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/test_use_cases.sh)
    set_tests_properties (H5SHELL-test_use_cases PROPERTIES
            ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    if ("H5SHELL-test_use_cases" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5SHELL-test_use_cases PROPERTIES DISABLED true)
    endif ()

    add_test (H5SHELL-test_swmr ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/test_swmr.sh)
    set_tests_properties (H5SHELL-test_swmr PROPERTIES
            ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    if ("H5SHELL-test_swmr" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5SHELL-test_swmr PROPERTIES DISABLED true)
    endif ()

    add_test (H5SHELL-test_vds_swmr ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/test_vds_swmr.sh)
    set_tests_properties (H5SHELL-test_vds_swmr PROPERTIES
            ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    if ("H5SHELL-test_vds_swmr" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5SHELL-test_vds_swmr PROPERTIES DISABLED true)
    endif ()

    # test_vfd_swmr.sh runs a set of independent scenarios (few_big, zoo,
    # groups, sparse, ...); it accepts a scenario name as an argument to run
    # just that one (see its all_tests handling). Register each scenario as
    # its own ctest test so CI reports/parallelizes them individually instead
    # of hiding them all behind one monolithic pass/fail. The scenarios all
    # run in the same working directory and several of them communicate over a
    # fixed socket port (DEFAULT_PORT in vfd_swmr_common.c) and share message/
    # data filenames, so they must never run concurrently with each other --
    # a shared RESOURCE_LOCK serializes them among themselves while still
    # letting the rest of the suite run in parallel.
    #
    # These are the scenarios test_vfd_swmr.sh makes available at every
    # test-express level (its default all_tests list).
    set (H5_VFD_SWMR_SHELL_TESTS
        generator expand shrink expand_shrink sparse vlstr_null vlstr_oob
        zoo groups groups_attrs groups_ops few_big many_small
    )
    # Additional scenarios the script only enables for an exhaustive
    # (HDF_TEST_EXPRESS=0) run -- registering them at other levels would make
    # the script reject the argument as an "Unknown test".
    if (HDF_TEST_EXPRESS EQUAL 0)
      list (APPEND H5_VFD_SWMR_SHELL_TESTS
          attrdset dsetops dsetops_ref dsetchks
          os_groups_attrs os_groups_ops os_groups_seg independ_wr
          gfail_entry_length gfail_checksum gfail_page_size gfail_index_space
      )
    endif ()

    foreach (vfd_swmr_scenario ${H5_VFD_SWMR_SHELL_TESTS})
      # test_vfd_swmr.sh's own 'shrink' scenario does not regenerate its
      # prerequisite .h5 file the way every other scenario does (see its
      # "depends on the .h5 file left behind by the 'expand' test" check) --
      # it is only ever meant to be invoked together with 'expand' in the
      # same script run. RESOURCE_LOCK below only prevents these ctest
      # tests from overlapping in time, it does not order them, so running
      # 'shrink' with just its own name (as every other scenario here is)
      # fails deterministically. Pass both scenario names to this one
      # invocation instead, matching the script's own documented
      # precondition, rather than relying on ctest scheduling order.
      if (vfd_swmr_scenario STREQUAL "shrink")
        set (vfd_swmr_scenario_args expand shrink)
      else ()
        set (vfd_swmr_scenario_args ${vfd_swmr_scenario})
      endif ()
      add_test (H5SHELL-test_vfd_swmr-${vfd_swmr_scenario}
          ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/test_vfd_swmr.sh ${vfd_swmr_scenario_args})
      set_tests_properties (H5SHELL-test_vfd_swmr-${vfd_swmr_scenario} PROPERTIES
              # Forward the configured test-express level so the script honors it
              # (it otherwise defaults to 1 = a heavier run than a HDF_TEST_EXPRESS=3
              # build intends). Matches how the reference/autotools harness runs it.
              ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY};HDF5TestExpress=${HDF_TEST_EXPRESS}"
              WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
              TIMEOUT ${CTEST_VERY_LONG_TIMEOUT}
              RESOURCE_LOCK vfd_swmr_h5test_dir
      )
      if ("H5SHELL-test_vfd_swmr-${vfd_swmr_scenario}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5SHELL-test_vfd_swmr-${vfd_swmr_scenario} PROPERTIES DISABLED true)
      endif ()
    endforeach ()
  endif ()
endif ()
