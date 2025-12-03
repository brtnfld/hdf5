# VFD SWMR Utilities

aux_process.c:
==============
To support NFS file system, this utility applies the updater files to the copy of the metadata file.

Usage: aux_process [options] <md_file> <ud_path>

Where: <md_file> is the path to the metadata file. Must be on a POSIX file system. Note that the file may not exist yet.
       <ud_path> is the path of the updater files including the directory. This will typically be in an NFS mounted file system.

Options:
    -a --skip_aux:       Exit if VDS across multiple file is being enabled (to be implemented in the future).
    -c --vfd_config:     Quoted string containing the configuration string for the VFD stack to be used. Default: sec2
    -l --log_file:       Path to the log file. Default: no log file.
    -m --md_chksum_path: Path to the file containing the checksum values for testing purpose.
    -p --polls_per_tick: Number of times to poll for a new updater file per tick. Default: 10.
    -s --stats:          Display stats on exit.
    -t --tick_len:       Integer value indicating the tick length in tenths of a second.
    -v --verbose:        Write log entries to stdout.


recovery_tool.c:
================
To enable recovery of HDF5 files, this utility applies the updater files directly to the HDF5 file metadata.

Usage: recovery_tool [options] <h5_file> <ud_path>

Where: <h5_file> is the path to the HDF5 file. Must be on a POSIX file system.
       <ud_path> is  the path of the updater files including the directory.

Options:
    -h --help:     Prints a usage message for the program.
    -p --posix:    Indicate that the HDF5 file is on POSIX file system; HDF5 file will be kept open during the 
                   sequence of the metadata modifications.
    -v --verbose:  Prints detailed information about each updater file being processed, including headers, 
                   change lists, and data operations, to stdout.
    -l --log_file <log_file>: 
                   Specify path of a log file for log entries. (Will ignore verbose option)

Note: 
    The different prints need to be reworked, possibly in aux_process.c as well. None of the critical errors 
    get printed to the logging file, and some things are printed to stdout regardless of the verbose flag, 
    but not printed to the logging file.


crasher.c
=========
To allow for testing the recovery tool, this utility runs and crashes (sends a KILL -9 signal to) the 
command you provide it after <delay> seconds. This allows us to crash a VFD SWMR writer at any time during 
HDF5 file creation, and test how recoverable that file is.

Usage: crasher [options] <delay> <command> [args]

Where:  <delay> is the time in seconds to wait before crashing (decimals allowed, e.g., 1.5 or 0.25, 
            max precision 6 decimal places)
        <command> [args] is the command to execute and then crash. Any arguments after the command 
            are passed to it.

Options:
    -h: Prints a usage message for the program.
    -v: Prints detailed information.
    -p: Prints command's output to console instead of redirecting to <command>.out.

Example:
    crasher -v -p 5 ./my_program arg1 arg2


test_crash_recovery.sh
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
All output files are placed in the crasher_test/ directory:
    <test>_recovery.out.<count>         - Recovery tool output and error messages
    <test>_h5clear_pre.out.<count>      - H5clear output before recovery
    <test>_h5clear_post.out.<count>     - H5clear output after recovery
    <test>_validation_pre.out.<count>   - Validation output before recovery
    <test>_validation_post.out.<count>  - Validation output after recovery
    <writer_name>.out.<count>           - Writer tool output

Where <test> is the test name and <count> is the iteration number. Note that a single test may run 
dozens of iterations, so many files will be created with the -k option.
Tests:
    standard  - Test vfd_swmr_writer crash recovery
    bigset    - Test vfd_swmr_bigset_writer crash recovery (currently the primary test)
    sparse    - Test vfd_swmr_sparse_writer crash recovery
    remove    - Test vfd_swmr_remove_writer crash recovery

If no tests are specified, all tests will be run.

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

Note: The script should automatically select the correct project dir, but will fail if you move relavant files from their expected spots.
ALSO NOTE: Only the 'remove' test currently works.
