/*-------------------------------------------------------------------------
 *
 * Created:     crasher.c
 *              Cody Sloan, 08/06/2025
 *
 * Purpose:     Run a command as a forked process and then kill it with 
 *              SIGKILL after a specified delay.
 * 
 *              This program is intended to simulate real crashes in VFD
 *              SWMR writer programs to test recovery of the HDF5 file.
 *      
 *              Only works on POSIX systems.
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/
#include "H5private.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>  
#include <signal.h>   
#include <fcntl.h>
#include <string.h>    
#include <stdlib.h>    

#define FILE_NAME_LEN                  1024

/*-------------------------------------------------------------------------
 * Function: run_command_with_crash
 *
 * Purpose:  Run a command with the given arguments, redirecting its output
 *    (stdout and stderr) to a file. Crashes the command after a specified
 *    delay.
 * Description: This function forks a child process to run the command. The
 *    child process redirects its output (stdout and stderr) to a file named
 *    "<outbase>.out".
 * 
 * Return:   Success:       exit status of the command
 *           Failure:      -1 
 *-------------------------------------------------------------------------
 */
static int
run_command_with_crash(const char *outbase, char *const cmd_argv[], double delay, int verbose, int print_to_console)
{
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork failed");
        goto error;
    } else if (pid == 0) { /* Child process */
        /*If print_to_console is false, redirect stdout and stderr to an 
        output file instead of printing to the console. */
        if (!print_to_console){
            /* Prepare output file names */
            char out_file[FILE_NAME_LEN];
            snprintf(out_file, sizeof(out_file), "%s.out", outbase);

            /* Open output file */
            int out_fd = open(out_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (out_fd < 0) {
                perror("open output file failed");
                exit(EXIT_FAILURE);
            }

            /* Redirect stdout and stderr to the output file */
            if (dup2(out_fd, STDOUT_FILENO) < 0 || dup2(out_fd, STDERR_FILENO) < 0) {
                perror("dup2 failed");
                close(out_fd);
                exit(EXIT_FAILURE);
            }

            close(out_fd); /* Close the file descriptor after duplicating */
        }

        /* Execute the command */
        execvp(cmd_argv[0], cmd_argv);

        /* If execvp returns, there was an error */
        perror("execvp failed");
        exit(EXIT_FAILURE);
    } else { /* Parent process */
        /* Start the crash timer */
        if (delay <= 0) {
            /* If delay is 0, crash immediately */
            kill(pid, SIGKILL);
        } else {
            /* Otherwise, wait for the specified delay before crashing */
            usleep((useconds_t)(delay * 1000000));
            kill(pid, SIGKILL);  /* Always kill, regardless of state */
        }

        /* Wait for the child process to finish */
        int status;
        if (waitpid(pid, &status, 0) == -1) {
            perror("waitpid failed");
            goto error;
        }


        /* Write return code to file and handle status in one go */
        char rc_file[FILE_NAME_LEN];
        snprintf(rc_file, sizeof(rc_file), "%s.rc", outbase);
        FILE *rc_fp = fopen(rc_file, "w");
        
        int return_code;
        if (WIFEXITED(status)) {
            return_code = WEXITSTATUS(status);
            if (verbose)
                HDfprintf(stdout, "Command exited with status %d\n", return_code);
        } else if (WIFSIGNALED(status)) {
            return_code = 128 + WTERMSIG(status);
            if (verbose)
                HDfprintf(stdout, "Command was killed by signal %d\n", WTERMSIG(status));
        } else {
            return_code = -1;
            if (verbose)
                HDfprintf(stdout, "Command terminated abnormally (status=0x%x)\n", status);
        }

        /* Write the return code to file */
        if (rc_fp) {
            fprintf(rc_fp, "%d\n", return_code);
            fclose(rc_fp);
        } else {
            perror("fopen rc file failed");
            goto error;
        }
        return return_code;
    }

error:
    return -1; /* Indicate failure */
} /* run_command_with_crash() */

static void
usage(void)
{
    HDprintf("\nUsage: crasher [options] <delay> <command> [args...]\n");
    HDprintf("\n");
    HDprintf("Executes the specified command as a forked process and then terminates it with\n");
    HDprintf("SIGKILL after <delay> seconds. If <delay> is 0, the command is killed\n");
    HDprintf("immediately.\n");
    HDprintf("\n");
    HDprintf("   Options:\n");
    HDprintf("     -h : Show this help message, then exit.\n");
    HDprintf("     -v : Print verbose output.\n");
    HDprintf("     -p : Print the command's output to console instead of redirecting to\n");
    HDprintf("          <command>.out.\n");
    HDprintf("\n");
    HDprintf("   Required Arguments:\n");
    HDprintf("     <delay>             : Time in seconds to wait before crashing (decimals allowed, \n");
    HDprintf("                           e.g., 1.5 or 0.25, max precision 6 decimal places).\n");
    HDprintf("     <command> [args...] : Command to execute and then crash. Any arguments after\n");
    HDprintf("                           the command are passed to it.\n");
    HDprintf("\nExample:\n");
    HDprintf("  crasher -v 5 ./my_program arg1 arg2\n");
    HDprintf("\n");
}

int main(int argc, char *argv[]) {
    double  delay           = -1.0;
    int    verbose          = 0;
    int    print_to_console = 0;
    int    i                = 1;
    char  *endptr           = NULL;
    char  **cmd_argv        = NULL;  /* Command and its arguments */
    char  *cmd_name         = NULL;  /* Name used for output files */

    /* Parse options first */
    while (i < argc && argv[i][0] == '-') {
        if (strcmp(argv[i], "-h") == 0) {
            usage();
            exit(0);
        } else if (strcmp(argv[i], "-v") == 0) {
            verbose = 1;
            i++;
        } else if (strcmp(argv[i], "-p") == 0) {  
            print_to_console = 1;
            i++;
        } else {
            HDprintf("Unknown option: %s\n", argv[i]);
            usage();
            exit(1);
        }
    }

    /* Check that delay argument is present */
    if (i >= argc) {
        HDprintf("Error: Missing required <delay> and <command> arguments\n");
        usage();
        exit(1);
    }

    /* Parse delay value */
    delay = HDstrtod(argv[i], &endptr);
    if (endptr == argv[i] || *endptr != '\0') {
        HDprintf("Error: Invalid delay value '%s'\n", argv[i]);
        usage();
        exit(1);
    }
    if (delay < 0) {
        HDprintf("Error: Delay must be non-negative\n");
        usage();
        exit(1);
    }
    i++; /* Move to the next argument after delay */

    /* Check that command argument is present */
    if (i >= argc) {
        HDprintf("Error: Missing required <command> argument\n");
        usage();
        exit(1);
    }

    /* Remaining arguments are the command and its arguments */
    cmd_argv = &argv[i];

    /* Grab the command full path */
    cmd_name = cmd_argv[0];

    /* Strip path from command path to get basename for output files */
    char *slash = strrchr(cmd_argv[0], '/');
    if (slash) {
        cmd_name = slash + 1;  /* Use basename only */
    }

    /* Sanity check - this should never happen if cmd_argv[0] exists */
    if (cmd_name == NULL || cmd_name[0] == '\0') {
        HDprintf("Error: Command name is empty\n");
        usage();
        exit(1);
    }

    if (verbose) {
        HDprintf("Running and crashing command after %.1f second delay: %s", delay, cmd_argv[0]);
        for (int j = 1; cmd_argv[j] != NULL; j++) {
            HDprintf(" %s", cmd_argv[j]);
        }
        HDprintf("\n");
        HDprintf("Output will be saved to: %s.out\n", cmd_name);
    }

    /* Run the command with the specified delay */
    return run_command_with_crash(cmd_name, cmd_argv, delay, verbose, print_to_console);
} /* main() */