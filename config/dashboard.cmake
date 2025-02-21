# Dashboard script for CDash reporting
set(CTEST_SOURCE_DIRECTORY "$ENV{GITHUB_WORKSPACE}/hdf5")
set(CTEST_BINARY_DIRECTORY "$ENV{GITHUB_WORKSPACE}/hdf5/build")
set(CTEST_SITE "$ENV{CTEST_SITE}")
set(CTEST_BUILD_NAME "$ENV{CTEST_BUILD_NAME}")
set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
set(CTEST_CONFIGURATION_TYPE "${CTEST_BUILD_CONFIGURATION}")

# Start the submission process
ctest_start("${CTEST_TEST_MODEL}" TRACK "${CTEST_TRACK_TYPE}")

# Configure step
ctest_configure()

# Build step 
ctest_build(FLAGS "${CTEST_BUILD_FLAGS}")

# Test step
ctest_test(RETURN_VALUE test_ret)

# Submit results to CDash
ctest_submit()

