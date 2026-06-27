# VFD SWMR Utilities

aux_process.c:
==============
The `aux_process` utility applies a sequence of updater files to generate a locally maintained copy of the VFD SWMR metadata file.

This utility is primarily intended for use on the reader system in NFS-based workflows, where direct access to up-to-date metadata produced by the writer may be delayed.

The updater files are expected to be generated incrementally during writer execution and are processed in order to reconstruct the latest available metadata state.

**Usage:** 
```bash
aux_process [options] <md_file> <ud_path>
```

**Where:**  
  - `<md_file>`  
  The path to the metadata file. Must be on a POSIX file system. Note that the file may not exist yet.  
  - `<ud_path>`   
  The path of the updater files including the directory. For example, updater files named `updater_file.0`, `updater_file.1`, ..., `updater_file.n` should be specified as `/path/to/updater_file`. This will typically be in an NFS mounted file system.

**Options:**  
  - `-a --skip_aux`  
  Exit if VDS across multiple file is being enabled (to be implemented in the future).  
  - `-l --log_file`  
  Path to the log file. Default: no log file.  
  - `-m --md_chksum_path`  
  Path to the file containing the checksum values for testing purpose.  
  - `-p --polls_per_tick`  
  Number of times to poll for a new updater file per tick. Default: 10.  
  - `-s --stats`  
  Display stats on exit.  
  - `-t --tick_len`  
  Integer value indicating the tick length in tenths of a second.  
  - `-v --verbose`  
  Write log entries to stdout.

**Example:**
```bash
aux_process --verbose my_md_file /path/to/updater_file
```

**Note:**  
The `--log_file` option may need to be reworked. Errors are currently written to `stderr`, and selected command-line options are written to `stdout`; neither is written to the specified log file.

recovery_tool.c:
================
The `recovery_tool` applies a sequence of updater files to an HDF5 file in order to reconstruct a consistent metadata state after interruption.

This process restores the file to a state where it can be safely reopened using standard HDF5 APIs.

**Usage:** 
```bash
recovery_tool [options] <h5_file> <ud_path>
```

**Where:**
  - `<h5_file>`  
  Path to the HDF5 file.
  - `<ud_path>`  
  Path prefix of the updater files, including the directory. For example, updater files named `updater_file.0`, `updater_file.1`, ..., `updater_file.n` should be specified as `/path/to/updater_file`. 

**Options:**
  - `-h --help`  
  Print the usage message and exit.  
  - `-p --posix`  
  Indicate that the HDF5 file is on POSIX file system; HDF5 file will be kept open during the sequence of the metadata modifications. (Currently, only POSIX-compliant systems are supported).  
  - `-v --verbose`  
  Prints detailed information about each updater file being processed, including headers, change lists, and data operations, to stdout.  
  - `-l --log_file <log_file>`  
  Specify path of a log file for log entries. (Will ignore verbose option) 

**Example:**
```bash
recovery_tool --verbose path/to/h5_file.h5 /path/to/updater_file
```

**Requirement:**  
The `h5clear` utility must be available to this program. The path to `h5clear` must either be present in the system `PATH` or specified through the `H5CLEAR_PATH` environment variable.

### Notes:
#### Platform support
This tool currently supports POSIX-compliant systems only and is not expected to function on Windows.

#### Logging behavior
The `--log_file` option is incomplete: some output is still written to `stdout` or `stderr` regardless of this setting, and logging behavior is not fully consistent with the `--verbose` option.


crasher.c
=========
This utility is used to test the recovery tool by deliberately terminating a running process. It executes the provided command and then sends a SIGKILL (kill -9) signal after a specified delay. This makes it possible to interrupt a VFD SWMR writer at any point during HDF5 file creation and evaluate how recoverable the resulting file is.

By default, the command’s standard output is redirected to \<command\>.out. This behavior can be overridden with the `-p` option to print output directly to the console.

**Usage:**
```bash
crasher [options] <delay> <command> [args]
```

**Where:**  
  - `<delay>`  
  Time in seconds to wait before crashing the process.  
  Decimal values are supported (e.g., 1.5, 0.25).  
  Maximum precision: 6 decimal places. 
  - `<command> [args]`  
  The command to execute. Any additional arguments are passed directly to the command.

**Options:**
  - `-h`  
  Print the usage message and exit.  
  - `-v`  
  Prints detailed information.  
  - `-p`  
  Prints command's output to console instead of redirecting to \<command\>.out.

**Example:**
```bash
crasher -v -p 5 ./my_program arg1 arg2
```
<!-- 
The test scripts have been commented out because they are not quite ready for user testing.
The instructions are also likely to be out of date too.
                                -- Cody S. 6/15/26
 -->
<!-- test_crash_recovery.sh
======================
This script tests the recovery tool's ability to recover HDF5 files that have been corrupted by simulated 
crashes during the write process. It uses the crasher utility to kill VFD SWMR writer programs at 
incrementing time intervals (starting from 0.0 seconds and increasing by 0.1 seconds each iteration), 
then attempts to recover the resulting files using the recovery_tool utility and verifies that the 
recovered files are valid.

Usage: test_crash_recovery.sh [options] [test1 test2 ...]

Options:
    -h: Show help message and exit
    -v: Enable verbose output
    -k: Keep output files from each crash iteration (generates many files, useful for debugging)

Output Files (when using -k option):
    <test>_recovery.out.<count>         - Recovery tool output and error messages
    <test>_h5clear_pre.out.<count>      - H5clear output before recovery
    <test>_h5clear_post.out.<count>     - H5clear output after recovery
    <test>_validation_pre.out.<count>   - Validation output before recovery
    <test>_validation_post.out.<count>  - Validation output after recovery
    <writer_name>.out.<count>           - Writer tool output
Where <test> is the test name, <count> is the iteration number, and <writer_name> is the actual name of the writer program. 
Note that a single test may run dozens of iterations, so many files will be created with the -k option.

Tests:
    standard  - Test vfd_swmr_writer crash recovery
    bigset    - Test vfd_swmr_bigset_writer crash recovery
    sparse    - Test vfd_swmr_sparse_writer crash recovery
    remove    - Test vfd_swmr_remove_writer crash recovery

If no tests are specified, all tests will be run.

All output files are placed in a newly made crash_test/ directory inside the current working directory.

Test Process (for each iteration):
1. Generate initial HDF5 file (if required by the test)
2. Run the writer tool and crash it after <delay> seconds using crasher utility
3. Run h5clear to reset status flags on the crashed file
4. Validate the file before recovery using h5ls and h5dump (might fail might not)
5. Apply updater files using recovery_tool to recover the HDF5 file
6. Test recovery using h5ls and h5dump (expected to succeed)
7. Clean up temporary files (unless -k flag is used or HDF5_NOCLEANUP is set)

The test continues incrementing the crash delay until the writer tool completes normally without being 
crashed, at which point the test for that configuration ends.

Environment Variables:
    HDF5TestExpress - Controls test thoroughness (0=exhaustive, 1=default, 2+=quick)
    HDF5_NOCLEANUP  - If set, prevents cleanup of output files
    H5CLEAR_PATH    - Path to h5clear utility (set automatically by script)

Examples:
    ./test_crash_recovery.sh                    # Run all tests
    ./test_crash_recovery.sh -v -k bigset       # Run bigset test with verbose output and keep files
    ./test_crash_recovery.sh bigset sparse      # Run only bigset and sparse tests

Note: The script should automatically select the correct project dir, but will fail if you move relevant files 
from their expected spots.
ALSO NOTE: Only the 'remove' test currently works.


exec_local_socket_test.sh
=========================
The local version of exec_nfs_socket_test, which tests the 'attrdset', 'bigset', 'dsetchks', 
'dsetops', 'gfail', 'group', and 'zoo' VFD SWMR programs' socket communication ability with 
options found in test/test_vfd_swmr.sh. The script allows you to select the tests individually,
or to run multiple tests at once. All files will be placed in a local_socket_test/ directory in 
the directory that you call the script from.

Usage: exec_local_socket_test <test> <role> [md_dir]
    <test>: The VFD SWMR test program that we want to test.
            Can be one of the following:
            'all', 'attrdset', 'bigset', 'gfail', 'group',
            'group_basic', 'group_attrs', 'os_group_attrs', or 'zoo'.
            Note: 'all' runs all tests, 'group' runs all group-related
            tests.
    <role>: 'reader' or 'writer' to indicate which role to run.
            Also accepts just 'r' or 'w'.

This script sets up and runs SWMR tests using local sockets. It assumes that
you will run the writer and reader roles in separate terminal sessions. The
writer role should be started before reader role, to allow the socket
connection to establish correctly.

exec_nfs_socket_test.sh
=======================
Tests the 'attrdset', 'bigset', 'dsetchks', 'dsetops', 'gfail', 'group', and 'zoo' VFD SWMR programs' socket 
communication ability over a networked environment with options found in test/test_vfd_swmr.sh. The script has
been configured to pass an IP address to each of the reader programs to establish a socket connection with
the writer, and has slightly increase delays in some of the options to account for NFS-mount latency. The 
script allows you to select the tests individually, or to run multiple tests at once. All files will be 
placed in an nfs_socket_test/ directory in the directory that you call the script from.

Usage: $0 <test> <role> [md_dir]
    <test>: The VFD SWMR test program that we want to test.
            Can be one of the following:
            'all', 'attrdset', 'bigset', 'gfail', 'group',
            'group_basic', 'group_attrs', 'os_group_attrs', or 'zoo'.
            Note: 'all' runs all tests, 'group' runs all group-related 
            tests.
    <role>: 'reader' or 'writer' to indicate which role to run.
            Also accepts just 'r' or 'w'.
    [md_dir]: Optional directory path to place mdfile 
                (only for bigset test). 

This script sets up and runs SWMR tests using sockets. It assumes 
that you will run the writer and reader roles on separate devices, 
using an NFS mount as the current working directory when running 
this script. The writer role should be started before the reader 
role, to allow the socket connection to establish correctly.

Note: Since the bigset test requires the auxiliary process to 
run with access to a valid POSIX file system, the [md_dir] 
argument MUST be set to a valid local posix path on the reader 
device. The writer doesn't need this argument. -->