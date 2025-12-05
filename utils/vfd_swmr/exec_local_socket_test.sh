#!/usr/bin/env bash

# Gets the directory of ../.. relative to this script, which should be the root directory of the project
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WAIT_TIME=3
nerrors=0

# Parse arguments
chosen_test="$1"
test_role="${2:0:1}"

# Change to test directory
if [[ "$test_role" == "w" ]]; then
    rm -rf local_socket_test
    mkdir -p local_socket_test
fi
cd local_socket_test || { echo "Failed to change directory to local_socket_test"; exit 1; }

echo "Test directory: $(pwd)"

# Declare usage function
usage() {
    echo "Usage: $0 <test> <role> [md_dir]"
    echo "    <test>: The VFD SWMR test program that we want to test." 
    echo "            Can be one of the following:"
    echo "            'all', 'attrdset', 'bigset', 'gfail', 'group',"
    echo "            'group_basic', 'group_attrs', 'os_group_attrs', or 'zoo'."
    echo "            Note: 'all' runs all tests, 'group' runs all group-related"
    echo "            tests."
    echo "    <role>: 'reader' or 'writer' to indicate which role to run."
    echo "            Also accepts just 'r' or 'w'."
    echo ""
    echo " This script sets up and runs SWMR tests using local sockets. It assumes that" 
    echo " you will run the writer and reader roles in separate terminal sessions. The"
    echo " writer role should be started before reader role, to allow the socket"
    echo " connection to establish correctly."
} # usage

# Validate arguments
if [ -z "$1" ] || [ -z "$2" ] || ( [ "$test_role" != "r" ] && [ "$test_role" != "w" ] ); then
    usage
    exit 1
fi

###############################################################################
# HDF5TestExpress variable controls how exhaustive the tests are.
## 0:  Exhaustive run: Tests take a long time to run.
## 1:  Default run.
## 2+: Quick run (not implemented into this script)
###############################################################################
if [[ -z $HDF5TestExpress ]]; then    # Set to default when not set
    HDF5TestExpress=1
fi

###############################################################################
# configure_test_env() <test_name>
#   Arguments:
#     $1 - test name (attrdset, bigset, dsetops, dsetchks, gfail, group_basic, 
#           group_attrs, os_group_attrs, zoo)
# Configure environment variables to run each test
################################################################################
configure_test_env() {
    case "$1" in
        attrdset)
            # Reconstructed options from test_vfd_swmr.sh's "attrdset" tests
            swmr_shared_opts=("-g -a 8 -v -m -d 8 -c 3 -u 5 -q")
            if [[ "$HDF5TestExpress" -eq 0 ]] ; then        # exhaustive run
                swmr_shared_opts=(
                    "-p -g -a 10 -v -m -d 10 -c 3 -u 5 -q"
                    "-k -a 20 -v -m -d 5 -q"
                )
            fi

            # Set options
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")

            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_attrdset_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_attrdset_reader"
            aux_proc_cmd="" # No aux process for attrdset test
            generated_files="vfd_swmr_attrdset.h5 attrdset-shadow"
            ;;
        bigset)
            # Reconstructed options from test_vfd_swmr.sh's "bigset" tests
            swmr_shared_opts=(
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -F"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 2 -l 10"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 2 -F -l 10"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -t"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -t -F"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -t -R"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -V"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -M"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -V -F"
                "-n 25 -s 50 -e 1 -r 16 -c 16 -q -d 1 -M -F"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -F"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 2 -l 10"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 2 -F -l 10"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -l 10"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -F -l 10"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -R"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -V"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -M"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -V -F"
                "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -M -F"
            ) 
            if [[ "$HDF5TestExpress" -eq 0 ]] ; then
                swmr_shared_opts=(
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -F"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 2 -l 10"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 2 -F -l 10"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -t"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -t -F"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -t -R"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -V"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -M"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -V -F"
                    "-n 100 -s 100 -e 1 -r 16 -c 16 -q -d 1 -M -F"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -F"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 2 -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 2 -F -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -F -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -R"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -V"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -M"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -V -F"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -M -F"
                )
            elif [[ "$HDF5TestExpress" -gt 1 ]] ; then
                swmr_shared_opts=(
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -F"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 2 -l 10"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 2 -F -l 10"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -t"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -t -F"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -t -R"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -V"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -M"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -V -F"
                    "-n 10 -s 25 -e 1 -r 16 -c 16 -q -d 1 -M -F"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -F"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 2 -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 2 -F -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -F -l 10"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -t -R"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -V"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -M"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -V -F"
                    "-n 25 -s 50 -e 8 -r 256 -c 256 -q -d 1 -M -F"
                )
            fi

            # Set options
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")

            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_bigset_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_bigset_reader"

            aux_proc_cmd="$PROJECT_DIR/utils/vfd_swmr/aux_process mdfile bigset_updater"
            Maux_proc_cmd="$PROJECT_DIR/utils/vfd_swmr/aux_process -a mdfile bigset_updater" # For -M option
            generated_files="bigset_updater.* vfd_swmr_bigset.h5 step_file.tmp mdfile"
            ;;
        dsetchks)
            swmr_shared_opts=(
                "-s -m 8 -n 3 -g 1 -q"
                "-i -o -g 3 -q"
                "-f -p 4 -q"
                "-e -m 3 -n 5 -t 1 -q"
                "-r -m 11 -n 5 -l 7 -q"
                "-f -x 5 -y 2 -q"
                "-s -m 8 -n 3 -g 1 -q -U" # Same options as above but with -U appended (from test_vfd_swmr.sh)
                "-i -o -g 3 -q -U"
                "-f -p 4 -q -U"
                "-e -m 3 -n 5 -t 1 -q -U"
                "-r -m 11 -n 5 -l 7 -q -U"
                "-f -x 5 -y 2 -q -U"
            )
            # Set options
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")

            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_dsetchks_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_dsetchks_reader"
            aux_proc_cmd="" # No aux process for dsetchks test
            generated_files="vfd_swmr_dsetchks.h5 dsetchks-shadow"
            ;;
        dsetops)
            swmr_shared_opts=(
                "-p -e 20 -t -q" # Start of options for "dsetops" test in test_vfd_swmr.sh
                "-g -m 5 -n 2 -s 10 -w 7 -q"
                "-k -m 10 -n 5 -r 5 -l 10 -q"
                "-p -e 20 -t -q -U"
                "-g -m 5 -n 2 -s 10 -w 7 -q -U"
                "-k -m 10 -n 5 -r 5 -l 10 -q -U"
                "-p -e 20 -t -g -q -O"       # Start of options for "dsetops_ref" test in test_vfd_swmr.sh
                "-g -m 5 -n 2 -s 10 -w 7 -q -O"
                "-k -m 10 -n 5 -r 5 -l 10 -q -O"
                "-p -e 20 -t -g -q -R"
                "-g -m 5 -n 2 -s 10 -w 7 -q -R"
                "-k -m 10 -n 5 -r 5 -l 10 -q -R"
                "-p -e 20 -t -g -q -O -R"
                "-g -m 5 -n 2 -s 10 -w 7 -q -O -R"
                "-k -m 10 -n 5 -r 5 -l 10 -q -O -R"
            )

            # Set options
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")

            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_dsetops_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_dsetops_reader"
            aux_proc_cmd="" # No aux process for dsetops test
            generated_files="vfd_swmr_dsetops.h5 dsetops-shadow"
            ;;
        gfail)
            swmr_shared_opts=(
                "-m 10 -n 340000 -q"
                "-m 50 -t 10 -n 4000000 -q"
                "-m 10 -B 8192 -s 8192 -n 320000 -q" # Will fail sometimes and succeed other times.
                "-m 50 -t 10 -n 1000000 -q"
            )
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")
  
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_gfail_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_gfail_reader"
            aux_proc_cmd="" # No aux process for gfail test
            generated_files="vfd_swmr_group.h5 group-shadow"
            ;;
        
        group_basic) 
            # "groups" test from test_vfd_swmr.sh uses vfd_swmr_group_{writer,reader}
            
            swmr_shared_opts=(
                "-q -c 10 -n 20"
            )
            if [[ "$HDF5TestExpress" -eq 0 ]] ; then        # exhaustive run
                swmr_shared_opts=(
                    "-q -c 10 -n 400"
                )
            elif [[ "$HDF5TestExpress" -gt 1 ]] ; then
                swmr_shared_opts=(
                    "-q -c 10 -n 10"
                )
            fi

            # Set options
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")
            reader_opts[0]="${reader_opts[0]} -u 5"  # Add update argument to reader option set (copying from test_vfd_swmr.sh)

            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_group_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_group_reader"
            aux_proc_cmd="" # No aux process for group test
            generated_files="vfd_swmr_group.h5 group-shadow"
            ;;
        group_attrs) 
            # "groups_attrs" test from test_vfd_swmr.sh uses vfd_swmr_group_{writer,reader}
            swmr_shared_opts=(
                "-q -c 1 -n 1 -a 1 -A dense-del-to-compact"
                "-q -c 1 -n 1 -a 1 -A modify"
                "-q -c 1 -n 1 -a 1 -A remove-vstr"
                "-q -c 1 -n 1 -a 1 -A modify-vstr"
                "-q -c 1 -n 1 -a 1 -A del-ohr-block"
            )
            if [[ "$HDF5TestExpress" -eq 0 ]] ; then        # exhaustive run
                swmr_shared_opts=(
                    "-q -c 1 -n 2 -a 1 -A compact"
                    "-q -c 1 -n 2 -a 1 -A dense"
                    "-q -c 1 -n 2 -a 1 -A compact-del"
                    "-q -c 1 -n 2 -a 1 -A dense-del"
                    "-q -c 1 -n 2 -a 1 -A compact-add-to-dense"
                    "-q -c 1 -n 2 -a 1 -A dense-del-to-compact"
                    "-q -c 1 -n 2 -a 1 -A modify"
                    "-q -c 1 -n 2 -a 1 -A add-vstr"
                    "-q -c 1 -n 2 -a 1 -A remove-vstr"
                    "-q -c 1 -n 2 -a 1 -A modify-vstr"
                    "-q -c 1 -n 2 -a 1 -A add-ohr-block"
                    "-q -c 1 -n 2 -a 1 -A del-ohr-block"
                )
            elif [[ "$HDF5TestExpress" -gt 1 ]] ; then
                swmr_shared_opts=(
                    "-q -c 1 -n 1 -a 1 -A dense"
                    "-q -c 1 -n 1 -a 1 -A modify"
                    "-q -c 1 -n 1 -a 1 -A remove-vstr"
                    "-q -c 1 -n 1 -a 1 -A modify-vstr"
                    "-q -c 1 -n 1 -a 1 -A del-ohr-block"
                )
            fi
            # Set options
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")
            
            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_group_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_group_reader"
            aux_proc_cmd="" # No aux process for group test
            generated_files="vfd_swmr_group.h5 group-shadow"
            ;;
        os_group_attrs)
            # "os_group_attrs" test from test_vfd_swmr.sh uses vfd_swmr_group_{writer,reader}
            swmr_shared_opts=(
                "-q -G -c 1 -n 1 -a 1 -A compact"
                "-q -G -c 1 -n 1 -a 1 -A compact-del"
                "-q -G -c 1 -n 1 -a 1 -A modify"
                "-q -G -c 1 -n 1 -a 1 -A add-vstr"
                "-q -G -c 1 -n 1 -a 1 -A remove-vstr"
                "-q -G -c 1 -n 1 -a 1 -A modify-vstr"
                "-q -G -c 1 -n 1 -a 1 -A add-ohr-block"
                "-q -G -c 1 -n 1 -a 1 -A del-ohr-block"
            )
            if [[ "$HDF5TestExpress" -eq 0 ]] ; then        # exhaustive run
                swmr_shared_opts=(
                "-q -G -c 1 -n 2 -a 1 -A compact"
                "-q -G -c 1 -n 2 -a 1 -A compact-del"
                "-q -G -c 1 -n 2 -a 1 -A modify"
                "-q -G -c 1 -n 2 -a 1 -A add-vstr"
                "-q -G -c 1 -n 2 -a 1 -A remove-vstr"
                "-q -G -c 1 -n 2 -a 1 -A modify-vstr"
                "-q -G -c 1 -n 2 -a 1 -A add-ohr-block"
                "-q -G -c 1 -n 2 -a 1 -A del-ohr-block"
                )
            fi
            # Set options
            writer_opts=("${swmr_shared_opts[@]}")
            reader_opts=("${swmr_shared_opts[@]}")

            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_group_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_group_reader"
            aux_proc_cmd="" # No aux process for group test
            generated_files="vfd_swmr_group.h5 group-shadow"
            ;;
        zoo)
            # Set options
            writer_opts=("-q")
            reader_opts=("-l 4 -q")

            # Configure basic command paths
            writer_cmd="$PROJECT_DIR/test/vfd_swmr_zoo_writer"
            reader_cmd="$PROJECT_DIR/test/vfd_swmr_zoo_reader"
            aux_proc_cmd="" # No aux process for zoo test
            generated_files="vfd_swmr_zoo.h5 zoo-shadow"
            ;;
        *)
            echo "Unknown test type: $1"
            usage
            exit 1
            ;;
    esac
}

###############################################################################
# reader_signal_writer_and_wait()
#
# Signal writer to proceed to next iteration, then wait for writer to 
# acknowledge the signal before continuing. Communication is done by echoing
# specific values into a temporary file and using grep to check for those 
# values. Will exit at 300 retries to avoid infinite loops.
#
# If grep errors, then exit.
###############################################################################
reader_signal_writer_and_wait() {
    local retry_count=0 # To not get stuck in infinite loop
    
    # Signal writer to proceed
    echo 1 > socket_test.tmp

    printf "\nREADER: Signaled writer to proceed to next iteration.\n"
    printf "Waiting for writer to acknowledge... "

    # Wait for the writer to acknowledge the signal
    while true; do
        # Acknowledgment check (the file contains "2")
        grep -q "2" socket_test.tmp
        rc=$?
        if [[ $rc -eq 0 ]]; then # grep succeeded
            break
        elif [[ $rc -eq 2 ]]; then # grep encountered IO error. Exit
            echo "Encountered error trying to access socket_test.tmp, exiting..."
            exit 1
        fi

        # Increment retry count and check for max retries
        retry_count=$((retry_count+1))
        if [[ $retry_count -ge 300 ]]; then
            echo "Exceeded maximum retries waiting for reader signal, exiting..."
            exit 1
        fi

        sleep 1 # Wait for writer to acknowledge
    done

    printf "Acknowledgment received. Reader proceeding after $WAIT_TIME seconds.\n"
    sleep $WAIT_TIME
    echo 0 > socket_test.tmp # Reset for next iteration
} # reader_signal_writer_and_wait

###############################################################################
# writer_wait_for_reader_and_acknowledge()
#
# Wait for reader's signal to proceed to next iteration, then acknowledge the 
# signal before continuing. Communication is done by echoing specific values 
# into a temporary file and using grep to check for those  values. Will exit 
# at 300 retries to avoid infinite loops.
#
# Ignores grep errors, since the file may not exist if the reader hasn't
# signaled yet.
###############################################################################
writer_wait_for_reader_and_acknowledge() {
    local retry_count=0 # To not get stuck in infinite loop

    printf "\nWRITER: Waiting for reader signal... "

    # Wait for the reader to signal
    while true; do
        # Check for reader signal.
        # Ignore grep errors, since file may not exist yet
        grep -q "1" socket_test.tmp 2>/dev/null
        rc=$?
        if [[ $rc -eq 0 ]]; then # grep succeeded. Proceed to acknowledgement
            break
        fi 

        # Increment retry count and check for max retries
        retry_count=$((retry_count+1))
        if [[ $retry_count -ge 300 ]]; then
            echo "Exceeded maximum retries waiting for reader signal, exiting..."
            exit 1
        fi

        sleep 1 # Wait for reader signal
    done

    printf "Signal received. Sent acknowledgment to Reader.\n"
    echo 2 > socket_test.tmp
    
} # writer_wait_for_reader_and_acknowledge



###############################################################################
# run_test() <test_name>
#   Args:
#       test_name: The name of the test to run (e.g., "attrdset", "bigset", etc.)
#
# This is where the actual test execution takes place.
# It configures the test environment based on the chosen test, and then runs all
# specified option sets for whichever role (reader or writer) was chosen.
###############################################################################
run_test() {
    configure_test_env $1
    if [[ "$test_role" == "w" ]]; then
        for opt in "${writer_opts[@]}"; do
            # Clean up generated files from previous runs
            rm -f $generated_files
            
            # Run writer command
            printf "\nRUNNING WRITER CMD:\n  %s\n    %s\n" "$writer_cmd" "$opt"
            $writer_cmd $opt
            rc=$?
            
            if [[ $rc -ne 0 ]]; then
                echo "ERROR: Writer command returned exit code: $rc"
                nerrors=$((nerrors+1))
            else
                # Print completion message
                echo "WRITER SUCCEEDED."
            fi
    
            # If there are multiple option sets, wait for reader acknowledgment between iterations
            if [[ ${#writer_opts[@]} -gt 1 || -v tests_to_run ]]; then
                writer_wait_for_reader_and_acknowledge
            fi
        done
    elif [[ "$test_role" == "r" ]]; then
        for opt in "${reader_opts[@]}"; do # Loop through the different option configurations for the chosen test
    
            # Start auxiliary process if needed
            if [[ -n "$aux_proc_cmd" ]]; then
                printf "\nRUNNING AUXILIARY CMD:\n"
                if [[ "$opt" == *"-M"* ]]; then
                    echo "  $Maux_proc_cmd" ; $Maux_proc_cmd & # Need to change aux command when bigset test is run with -M option
                else
                    echo "  $aux_proc_cmd" ; $aux_proc_cmd & 
                fi
                aux_pid=$! # Get PID of auxiliary process
                echo "Waiting for $WAIT_TIME seconds to allow the auxiliary process to start."
                sleep $WAIT_TIME
            fi
    
            # Run reader command
            printf "\nRUNNING READER CMD:\n  %s\n    %s\n" "$reader_cmd" "$opt"
            $reader_cmd $opt
            rc=$?

            if [[ $rc -ne 0 ]]; then
                echo "ERROR: Reader command returned exit code: $rc"
                nerrors=$((nerrors+1))
            else
                # Print completion message
                echo "READER SUCCEEDED."
            fi
    
            # Handle auxiliary process completion if it was started
            if [[ -n "$aux_proc_cmd" ]]; then
                wait $aux_pid 
            fi

            # If there are multiple option sets, signal writer to proceed between iterations
            if [[ ${#reader_opts[@]} -gt 1 || -v tests_to_run ]]; then
                reader_signal_writer_and_wait
            fi
        done
    else
        echo "Invalid role specified: $test_role"
        usage
        exit 1
    fi
}

################################################################################
# main()
################################################################################
main () {
    # Determine which tests to run based on user input
    if [[ "$chosen_test" == "all" || "$chosen_test" == "group" ]]; then
        if [[ "$chosen_test" == "all" ]]; then
            tests_to_run=("attrdset" "bigset" "dsetchks" "dsetops" "gfail" "group_basic" "group_attrs" "os_group_attrs" "zoo")
        else
            tests_to_run=("group_basic" "group_attrs" "os_group_attrs")
        fi

        for test in "${tests_to_run[@]}"; do
            run_test $test
        done
    else # Run a single specified test
        unset tests_to_run # Clear tests_to_run to indicate single test
        run_test $chosen_test
    fi
    echo "All tests completed with $nerrors errors."

    exit 0
}

# Invoke
main
