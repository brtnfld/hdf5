/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "H5private.h" /* Generic Functions -- must be included before other private headers, see H5Fsuper.c */

#include "hdf5.h"

#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define READER_WAIT_TICKS 3
#define VS_ATTR_NAME_LEN  27

typedef struct {
    hid_t        file, filetype, one_by_one_sid;
    char         filename[PATH_MAX];
    char         progname[PATH_MAX];
    unsigned int asteps;
    unsigned int csteps;
    unsigned int nsteps;
    unsigned int update_interval;
    hbool_t      use_vfd_swmr;
    hbool_t      old_style_grp;
    hbool_t      use_communication;
    char         grp_op_pattern;
    hbool_t      grp_op_test;
    char         at_pattern;
    hbool_t      attr_test;
    uint32_t     max_lag;
    uint32_t     tick_len;
    uint32_t     ps;
    uint32_t     pbs;
#ifdef H5_USE_SOCKETS
    socket_state_t sock;
#else  /* H5_USE_SOCKETS */
    int np_fd_w_to_r;
    int np_fd_r_to_w;
    int np_notify;
    int np_verify;
#endif /* H5_USE_SOCKETS */
} state_t;

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s [-S] [-G] [-a steps] [-b] [-c] [-n iterations]\n"
            "    [-N] [-q] [-u numb_ticks] [-A at_pattern] [-O grp_op_pattern]\n"
            "    [-t tick_len] [-m max_lag][-B pbs] [-s ps] \n"
            "\n"
            "-S:             do not use VFD SWMR\n"
            "-G:             old-style type of group\n"
            "-a steps:       `steps` between adding attributes\n"
            "-b:             write data in big-endian byte order\n"
            "-c steps:       `steps` between communication between the writer and reader\n"
#ifdef H5_USE_SOCKETS
            "-i --ip_addr ip_address: \n"
#endif /* H5_USE_SOCKETS */
            "                IP address for socket communication (default is localhost)\n"
            "-n ngroups:     the number of groups\n"
            "-N:             do not use communication between reader and writer,\n"
            "                mainly to run them separately\n"
            "-u numb_ticks:  `numb_ticks` for the reader to wait before verification\n"
            "-t tick_len:    length of a tick in tenths of a second.\n"
            "-m max_lag:     maximum expected lag(in ticks) between writer and readers\n"
            "-B pbs:         page buffer size in bytes:\n"
            "                The default value is 4K(4096).\n"
            "-s ps:          page size used by page aggregation, page buffer and \n"
            "                the metadata file. The default value is 4K(4096).\n"
            "-A at_pattern:  `at_pattern' for different attribute tests\n"
            "              The value of `at_pattern` is one of the following:\n"
            "              `compact`              - Attributes added in compact storage\n"
            "              `dense`                - An attribute added in dense storage\n"
            "              `compact-del`          - Attributes added and then one\n"
            "                                       attribute deleted, in compact \n"
            "              `dense-del`            - Attributes added until the storage\n"
            "                                       is dense then an attribute deleted\n"
            "                                       the storage still in dense\n"
            "              `compact-add-to-dense` - Attributes added first in compact\n"
            "                                       then in dense storage\n"
            "              `dense-del-to-compact` - Attributes added until the storage\n"
            "                                       is dense, then several attributes \n"
            "                                       deleted, the storage changed to\n"
            "                                       compact\n"
            "              `modify`               - An attribute added then modified\n",
            progname);
    /* Need to split this print function to remove compiler warnings (print too large) */
    fprintf(stderr, "              `add-vstr`             - A VL string attribute added\n"
                    "              `remove-vstr`          - A VL string attribute added then\n"
                    "                                       deleted\n"
                    "              `modify-vstr`          - A VL string attribute added then \n"
                    "                                       modified \n"
                    "              `add-ohr-block`        - An attribute is added and this forces\n"
                    "                                       the creation of object header\n"
                    "                                       continuation block \n"
                    "              `del-ohr-block`        - An attribute is added and this forces\n"
                    "                                       the creation of object header\n"
                    "                                       continuation block and then this \n"
                    "                                       attribute is deleted so the \n"
                    "                                       object header continuation block is \n"
                    "                                       removed. \n"
                    "-O grp_op_pattern:  `grp_op_pattern' for different group operation tests\n"
                    "              The value of `grp_op_pattern` is one of the following:\n"
                    "              `grp-creation`         - A group is created.\n"
                    "              `grp-deletion`         - An existing group is deleted.\n"
                    "              `grp-move`             - A group is moved to become \n"
                    "                                       another group. \n"
                    "              `grp-ins-links`        - Links are inserted, including\n"
                    "                                       both hard and soft links. \n"
                    "              `grp-del-links`        - Links are deleted, including\n"
                    "                                       both hard ans soft links. \n"
                    "              `grp-compact-t-dense`  - Links are inserted to the group.\n"
                    "                                       The link storage of this group \n"
                    "                                       changed from compact to dense. \n"
                    "                                       The links include both hard and\n"
                    "                                       soft links.                    \n"
                    "              `grp-dense-t-compact`  - Links are inserted to the group\n"
                    "                                       The link storage of this group \n"
                    "                                       changed from compact to dense. \n"
                    "                                       Then several links are deleted.\n"
                    "                                       The link storage changed from  \n"
                    "                                       dense to compact again.        \n"
                    "                                       The links include both hard and\n"
                    "                                       soft links.                    \n"
                    "-q:             silence printouts, few messages\n"
                    "\n");
    exit(EXIT_FAILURE);
}

#ifdef H5_USE_SOCKETS
static hbool_t
state_init(state_t *s, int argc, char **argv)
{
    unsigned long          tmp;
    int                    opt;
    const hsize_t          dims  = 1;
    char                  *tfile = NULL;
    char                  *end;
    const char            *s_opts   = "SGa:bc:i:n:Nqu:t:m:B:s:A:O:";
    struct h5_long_options l_opts[] = {{"ip_addr", require_arg, 'i'}, {NULL, 0, '\0'}};

    s->file           = H5I_INVALID_HID;
    s->one_by_one_sid = H5I_INVALID_HID;

    /* Force full library initialization before the first use of an
     * H5T_NATIVE_* macro below.  Without this, the very first evaluation
     * of H5T_NATIVE_UINT32 in this process can observe an unpopulated
     * H5T_NATIVE_UINT32_g (reads back as H5I_INVALID_HID), which later
     * makes H5Tget_native_type() fail with "not a data type" in
     * add_attr().  H5open() is always safe to call even if the library
     * is already open.
     */
    if (H5open() < 0) {
        printf("H5open failed\n");
        TEST_ERROR;
    }

    s->filetype          = H5T_NATIVE_UINT32;
    s->asteps            = 10;
    s->csteps            = 10;
    s->nsteps            = 100;
    s->update_interval   = READER_WAIT_TICKS;
    s->use_vfd_swmr      = true;
    s->old_style_grp     = false;
    s->use_communication = true;
    s->grp_op_pattern    = ' ';
    s->grp_op_test       = false;
    s->at_pattern        = ' ';
    s->attr_test         = false;
    s->tick_len          = 4;
    s->max_lag           = 7;
    s->ps                = 4096;
    s->pbs               = 4096;

    if (!socket_init(&s->sock)) {
        printf("socket_init failed\n");
        TEST_ERROR;
    }

    memset(s->filename, 0, PATH_MAX);
    memset(s->progname, 0, PATH_MAX);

    if (H5_basename(argv[0], &tfile) < 0) {
        printf("H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile) {
        free(tfile);
        tfile = NULL;
    }

    while ((opt = H5_get_option(argc, (const char *const *)argv, s_opts, l_opts)) != EOF) {
        switch (opt) {
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'G':
                s->old_style_grp = true;
                break;
            case 'a':
            case 'c':
            case 'n':
            case 'u':
            case 't':
            case 'm':
            case 'B':
            case 's':
                errno = 0;
                tmp   = strtoul(H5_optarg, &end, 0);
                if (end == H5_optarg || *end != '\0') {
                    printf("couldn't parse `-%c` argument `%s`\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    printf("couldn't parse `-%c` argument `%s`\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    printf("`-%c` argument `%lu` too large\n", opt, tmp);
                    TEST_ERROR;
                }

                if (opt == 'a')
                    s->asteps = (unsigned)tmp;
                else if (opt == 'c')
                    s->csteps = (unsigned)tmp;
                else if (opt == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (opt == 'u')
                    s->update_interval = (unsigned)tmp;
                else if (opt == 't')
                    s->tick_len = (unsigned)tmp;
                else if (opt == 'm')
                    s->max_lag = (unsigned)tmp;
                else if (opt == 'B')
                    s->pbs = (unsigned)tmp;
                else if (opt == 's')
                    s->ps = (unsigned)tmp;
                break;
            case 'b':
                s->filetype = H5T_STD_U32BE;
                break;
            case 'i':
                if (strlen(H5_optarg) >= MAX_IP_ADDR_LEN) {
                    fprintf(stderr, "-i,--ip_addr argument %s is too long\n", H5_optarg);
                    TEST_ERROR;
                }
                s->sock.ip_address = H5_optarg;
                break;
            case 'N':
                s->use_communication = false;
                break;
            case 'O':
                if (strcmp(H5_optarg, "grp-creation") == 0)
                    s->grp_op_pattern = 'c';
                else if (strcmp(H5_optarg, "grp-deletion") == 0)
                    s->grp_op_pattern = 'd';
                else if (strcmp(H5_optarg, "grp-move") == 0)
                    s->grp_op_pattern = 'm';
                else if (strcmp(H5_optarg, "grp-ins-links") == 0)
                    s->grp_op_pattern = 'i';
                else if (strcmp(H5_optarg, "grp-del-links") == 0)
                    s->grp_op_pattern = 'D';
                else if (strcmp(H5_optarg, "grp-compact-t-dense") == 0)
                    s->grp_op_pattern = 't';
                else if (strcmp(H5_optarg, "grp-dense-t-compact") == 0)
                    s->grp_op_pattern = 'T';
                else {
                    printf("Invalid -O argument \"%s\"", H5_optarg);
                    TEST_ERROR;
                }
                break;
            case 'A':
                if (strcmp(H5_optarg, "compact") == 0)
                    s->at_pattern = 'c';
                else if (strcmp(H5_optarg, "dense") == 0)
                    s->at_pattern = 'd';
                else if (strcmp(H5_optarg, "compact-add-to-dense") == 0)
                    s->at_pattern = 't';
                else if (strcmp(H5_optarg, "compact-del") == 0)
                    s->at_pattern = 'C';
                else if (strcmp(H5_optarg, "dense-del") == 0)
                    s->at_pattern = 'D';
                else if (strcmp(H5_optarg, "dense-del-to-compact") == 0)
                    s->at_pattern = 'T';
                else if (strcmp(H5_optarg, "modify") == 0)
                    s->at_pattern = 'M';
                else if (strcmp(H5_optarg, "add-vstr") == 0)
                    s->at_pattern = 'v';
                else if (strcmp(H5_optarg, "remove-vstr") == 0)
                    s->at_pattern = 'r';
                else if (strcmp(H5_optarg, "modify-vstr") == 0)
                    s->at_pattern = 'm';
                else if (strcmp(H5_optarg, "add-ohr-block") == 0)
                    s->at_pattern = 'a';
                else if (strcmp(H5_optarg, "del-ohr-block") == 0)
                    s->at_pattern = 'R';
                else {
                    printf("Invalid -A argument \"%s\"", H5_optarg);
                    TEST_ERROR;
                }
                break;
            case 'q':
                verbosity = 0;
                break;
            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= H5_optind;
    argv += H5_optind;

    if (s->grp_op_pattern != ' ')
        s->grp_op_test = true;
    if (s->at_pattern != ' ')
        s->attr_test = true;

    if (!s->grp_op_test) {
        if (s->asteps < 1 || s->asteps > s->nsteps) {
            printf("attribute interval is out of bounds\n");
            TEST_ERROR;
        }
    }

    if (s->grp_op_test && s->attr_test) {
        printf("Cannot test both group operation and attribute tests!\n");
        printf("Attribute tests are ignored.\n");
    }

    if (s->csteps < 1 || s->csteps > s->nsteps) {
        printf("communication interval is out of bounds\n");
        TEST_ERROR;
    }

    if (s->pbs < s->ps) {
        printf("Page buffer size cannot be smaller than the page size.s\n");
        TEST_ERROR;
    }
    if (argc > 0) {
        printf("unexpected command-line arguments\n");
        TEST_ERROR;
    }

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        printf("H5Screate_simple failed\n");
        TEST_ERROR;
    }

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");

    return true;

error:
    if (tfile)
        free(tfile);
    return false;
} /* state_init() */

/* Socket Subroutine: sock_wr_send_receive
 * Description:
 *   The writer sends a message to the reader,
 *   then waits for max_lag ticks,
 *   then checks the returned message from the reader.
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above.
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent
 *         by the reader.
 */
static hbool_t
sock_wr_send_receive(state_t *s)
{

    unsigned int i;
    /* Bump up the value of notify to notice the reader to start to read */
    s->sock.notify++;
    if (send(s->sock.comm_fd, &(s->sock.notify), sizeof(int), 0) < 0) {
        printf("send failed\n");
        TEST_ERROR;
    }

    /* During the wait, writer makes repeated HDF5 API calls
     * to trigger EOT at approximately the correct time */
    for (i = 0; i < s->max_lag + 1; i++) {
        decisleep(s->tick_len);
        H5E_BEGIN_TRY
        {
            H5Aexists(s->file, "nonexistent");
        }
        H5E_END_TRY;
    }

    /* Receive the same value from the reader and verify it before
     * going to the next step */
    (s->sock.verify)++;
    if (recv(s->sock.comm_fd, &(s->sock.notify), sizeof(int), 0) < 0) {
        printf("recv failed\n");
        TEST_ERROR;
    }

    if (s->sock.notify == -1) {
        printf("reader failed to verify group or attribute operation.\n");
        TEST_ERROR;
    }

    if (s->sock.notify != s->sock.verify) {
        printf("received message %d, expecting %d\n", s->sock.notify, s->sock.verify);
        TEST_ERROR;
    }

    return true;

error:
    return false;
} /* sock_wr_send_receive() */

/* Socket Subroutine: sock_rd_receive
 * Description:
 *   The reader receives a message from the writer,
 *   then checks if the notification number from
 *   the writer is expected.
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above.
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent
 *         by the writer.
 */
static hbool_t
sock_rd_receive(state_t *s)
{

    /* The writer should have bumped up the value of notify.
     * Do the same with verify and confirm it */
    s->sock.verify++;

    /* Receive the notify that the writer bumped up the value */
    if (recv(s->sock.comm_fd, &(s->sock.notify), sizeof(int), 0) < 0) {
        printf("recv failed\n");
        TEST_ERROR;
    }

    if (s->sock.notify == -1) {
        printf("writer failed to create group or carry out an attribute operation.\n");
        TEST_ERROR;
    }

    if (s->sock.notify != s->sock.verify) {
        printf("received message %d, expecting %d\n", s->sock.notify, s->sock.verify);
        TEST_ERROR;
    }

    return true;

error:
    return false;
} /* sock_rd_receive() */

/* Socket Subroutine: sock_rd_send
 * Description:
 *   The reader sends an acknowledgement to the writer
 * Return:
 *   True  if succeed
 *   False if an error occurs in sending the message.
 */
static hbool_t
sock_rd_send(state_t *s)
{

    if (send(s->sock.comm_fd, &(s->sock.notify), sizeof(int), 0) < 0) {
        H5_FAILED();
        AT();
        printf("send failed\n");
        return false;
    }
    else {
        return true;
    }
} /* sock_rd_send() */

/* Socket Subroutine: sock_send_error
 * Description:
 *   An error (notification number is 1) message is sent
 *   either from the reader or the writer.
 *   A boolean input parameter is used to choose
 *   either reader or writer.
 * Return:
 *   True  if succeed
 *   False if an error occurs in sending the message.
 */
static bool
sock_send_error(state_t *s)
{
    s->sock.notify = -1;

    if (send(s->sock.comm_fd, &(s->sock.notify), sizeof(int), 0) < 0) {
        H5_FAILED();
        AT();
        printf("write failed\n");
        return false;
    }
    else {
        return true;
    }
}

#else /* H5_USE_SOCKETS */
static hbool_t
state_init(state_t *s, int argc, char **argv)
{
    unsigned long          tmp;
    int                    opt;
    const hsize_t          dims  = 1;
    char                  *tfile = NULL;
    char                  *end;
    const char            *s_opts   = "SGa:bc:i:n:Nqu:t:m:B:s:A:O:";
    struct h5_long_options l_opts[] = {{"ip_addr", require_arg, 'i'}, {NULL, 0, '\0'}};

    s->file              = H5I_INVALID_HID;
    s->one_by_one_sid    = H5I_INVALID_HID;
    s->filetype          = H5T_NATIVE_UINT32;
    s->asteps            = 10;
    s->csteps            = 10;
    s->nsteps            = 100;
    s->update_interval   = READER_WAIT_TICKS;
    s->use_vfd_swmr      = true;
    s->old_style_grp     = false;
    s->use_communication = true;
    s->grp_op_pattern    = ' ';
    s->grp_op_test       = false;
    s->at_pattern        = ' ';
    s->attr_test         = false;
    s->tick_len          = 4;
    s->max_lag           = 7;
    s->ps                = 4096;
    s->pbs               = 4096;
    s->np_fd_w_to_r      = -1;
    s->np_fd_r_to_w      = -1;
    s->np_notify         = 0;
    s->np_verify         = 0;

    memset(s->filename, 0, PATH_MAX);
    memset(s->progname, 0, PATH_MAX);

    if (H5_basename(argv[0], &tfile) < 0) {
        printf("H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile) {
        free(tfile);
        tfile = NULL;
    }

    while ((opt = H5_get_option(argc, (const char *const *)argv, s_opts, l_opts)) != EOF) {
        switch (opt) {
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'G':
                s->old_style_grp = true;
                break;
            case 'a':
            case 'c':
            case 'n':
            case 'u':
            case 't':
            case 'm':
            case 'B':
            case 's':
                errno = 0;
                tmp   = strtoul(H5_optarg, &end, 0);
                if (end == H5_optarg || *end != '\0') {
                    printf("couldn't parse `-%c` argument `%s`\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    printf("couldn't parse `-%c` argument `%s`\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    printf("`-%c` argument `%lu` too large\n", opt, tmp);
                    TEST_ERROR;
                }

                if (opt == 'a')
                    s->asteps = (unsigned)tmp;
                else if (opt == 'c')
                    s->csteps = (unsigned)tmp;
                else if (opt == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (opt == 'u')
                    s->update_interval = (unsigned)tmp;
                else if (opt == 't')
                    s->tick_len = (unsigned)tmp;
                else if (opt == 'm')
                    s->max_lag = (unsigned)tmp;
                else if (opt == 'B')
                    s->pbs = (unsigned)tmp;
                else if (opt == 's')
                    s->ps = (unsigned)tmp;
                break;
            case 'b':
                s->filetype = H5T_STD_U32BE;
                break;
            case 'N':
                s->use_communication = false;
                break;
            case 'O':
                if (strcmp(H5_optarg, "grp-creation") == 0)
                    s->grp_op_pattern = 'c';
                else if (strcmp(H5_optarg, "grp-deletion") == 0)
                    s->grp_op_pattern = 'd';
                else if (strcmp(H5_optarg, "grp-move") == 0)
                    s->grp_op_pattern = 'm';
                else if (strcmp(H5_optarg, "grp-ins-links") == 0)
                    s->grp_op_pattern = 'i';
                else if (strcmp(H5_optarg, "grp-del-links") == 0)
                    s->grp_op_pattern = 'D';
                else if (strcmp(H5_optarg, "grp-compact-t-dense") == 0)
                    s->grp_op_pattern = 't';
                else if (strcmp(H5_optarg, "grp-dense-t-compact") == 0)
                    s->grp_op_pattern = 'T';
                else {
                    printf("Invalid -O argument \"%s\"", H5_optarg);
                    TEST_ERROR;
                }
                break;
            case 'A':
                if (strcmp(H5_optarg, "compact") == 0)
                    s->at_pattern = 'c';
                else if (strcmp(H5_optarg, "dense") == 0)
                    s->at_pattern = 'd';
                else if (strcmp(H5_optarg, "compact-add-to-dense") == 0)
                    s->at_pattern = 't';
                else if (strcmp(H5_optarg, "compact-del") == 0)
                    s->at_pattern = 'C';
                else if (strcmp(H5_optarg, "dense-del") == 0)
                    s->at_pattern = 'D';
                else if (strcmp(H5_optarg, "dense-del-to-compact") == 0)
                    s->at_pattern = 'T';
                else if (strcmp(H5_optarg, "modify") == 0)
                    s->at_pattern = 'M';
                else if (strcmp(H5_optarg, "add-vstr") == 0)
                    s->at_pattern = 'v';
                else if (strcmp(H5_optarg, "remove-vstr") == 0)
                    s->at_pattern = 'r';
                else if (strcmp(H5_optarg, "modify-vstr") == 0)
                    s->at_pattern = 'm';
                else if (strcmp(H5_optarg, "add-ohr-block") == 0)
                    s->at_pattern = 'a';
                else if (strcmp(H5_optarg, "del-ohr-block") == 0)
                    s->at_pattern = 'R';
                else {
                    printf("Invalid -A argument \"%s\"", H5_optarg);
                    TEST_ERROR;
                }
                break;
            case 'q':
                verbosity = 0;
                break;
            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= H5_optind;
    argv += H5_optind;

    if (s->grp_op_pattern != ' ')
        s->grp_op_test = true;
    if (s->at_pattern != ' ')
        s->attr_test = true;

    if (!s->grp_op_test) {
        if (s->asteps < 1 || s->asteps > s->nsteps) {
            printf("attribute interval is out of bounds\n");
            TEST_ERROR;
        }
    }

    if (s->grp_op_test && s->attr_test) {
        printf("Cannot test both group operation and attribute tests!\n");
        printf("Attribute tests are ignored.\n");
    }

    if (s->csteps < 1 || s->csteps > s->nsteps) {
        printf("communication interval is out of bounds\n");
        TEST_ERROR;
    }

    if (s->pbs < s->ps) {
        printf("Page buffer size cannot be smaller than the page size.s\n");
        TEST_ERROR;
    }
    if (argc > 0) {
        printf("unexpected command-line arguments\n");
        TEST_ERROR;
    }

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        printf("H5Screate_simple failed\n");
        TEST_ERROR;
    }

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");

    return true;

error:
    if (tfile)
        free(tfile);
    return false;
} /* state_init() */

/* Named Pipe Subroutine: np_wr_send_receive
 * Description:
 *   The writer sends a message to the reader,
 *   then waits for max_lag ticks,
 *   then checks the returned message from the reader.
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above.
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent
 *         by the reader.
 */
static hbool_t
np_wr_send_receive(state_t *s)
{

    unsigned int i;
    /* Bump up the value of notify to notice the reader to start to read */
    s->np_notify++;
    if (write(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0) {
        printf("write failed\n");
        TEST_ERROR;
    }

    /* During the wait, writer makes repeated HDF5 API calls
     * to trigger EOT at approximately the correct time */
    for (i = 0; i < s->max_lag + 1; i++) {
        decisleep(s->tick_len);
        H5E_BEGIN_TRY
        {
            H5Aexists(s->file, "nonexistent");
        }
        H5E_END_TRY;
    }

    /* Receive the same value from the reader and verify it before
     * going to the next step */
    (s->np_verify)++;
    if (read(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0) {
        printf("read failed\n");
        TEST_ERROR;
    }

    if (s->np_notify == -1) {
        printf("reader failed to verify group or attribute operation.\n");
        TEST_ERROR;
    }

    if (s->np_notify != s->np_verify) {
        printf("received message %d, expecting %d\n", s->np_notify, s->np_verify);
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

/* Named Pipe Subroutine: np_rd_receive
 * Description:
 *   The reader receives a message from the writer,
 *   then checks if the notification number from
 *   the writer is expected.
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above.
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent
 *         by the writer.
 */
static hbool_t
np_rd_receive(state_t *s)
{

    /* The writer should have bumped up the value of notify.
     * Do the same with verify and confirm it */
    s->np_verify++;

    /* Receive the notify that the writer bumped up the value */
    if (read(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0) {
        printf("read failed\n");
        TEST_ERROR;
    }

    if (s->np_notify == -1) {
        printf("writer failed to create group or carry out an attribute operation.\n");
        TEST_ERROR;
    }

    if (s->np_notify != s->np_verify) {
        printf("received message %d, expecting %d\n", s->np_notify, s->np_verify);
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

/* Named Pipe Subroutine: np_rd_send
 * Description:
 *   The reader sends an acknowledgement to the writer
 * Return:
 *   True  if succeed
 *   False if an error occurs in sending the message.
 */
static hbool_t
np_rd_send(state_t *s)
{

    if (write(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0) {
        H5_FAILED();
        AT();
        printf("write failed\n");
        return false;
    }
    else
        return true;
}

/* Named Pipe Subroutine: np_send_error
 * Description:
 *   An error (notification number is 1) message is sent
 *   either from the reader or the writer.
 *   A boolean input parameter is used to choose
 *   either reader or writer.
 * Return:
 *   True  if succeed
 *   False if an error occurs in sending the message.
 */
static bool
np_send_error(state_t *s, bool writer)
{
    int fd = writer ? s->np_fd_w_to_r : s->np_fd_r_to_w;

    s->np_notify = -1;

    if (write(fd, &(s->np_notify), sizeof(int)) < 0) {
        H5_FAILED();
        AT();
        printf("write failed\n");
        return false;
    }
    else
        return true;
}

#endif /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    check_ohr_num_chunk
 *
 * Purpose:     Check if the number of object header chunks is as expected.
 *
 * Parameters:  hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hbool_t one_chunk_ohr
 *              flag to indicate if the object header chunk is 1 or greater
 *              1: true
 *              greater than 1: false
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
check_ohr_num_chunk(hid_t g, hbool_t one_chunk_ohr)
{

    H5O_native_info_t ninfo;

    /* Get the object information */
    if (H5Oget_native_info(g, &ninfo, H5O_NATIVE_INFO_HDR) < 0) {
        printf("H5Oget_native_info failed\n");
        TEST_ERROR;
    }

    if (true == one_chunk_ohr) {
        if (ninfo.hdr.nchunks != 1) {
            printf("Object header should have only one chunk,but it is not.\n");
            TEST_ERROR;
        }
    }
    else {
        if (ninfo.hdr.nchunks <= 1) {
            printf("Object header should have more than one chunk,but it is not.\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    add_attr
 *
 * Purpose:     Add attributes to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *              unsigned num_attrs
 *              The number of attributes to be created
 *
 *              const char*aname_fmt
 *              The attribute name template used to create unique attribute names.
 *
 *              unsigned int g_which
 *              This parameter is used to generate correct group name in a key
 *              debugging message.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
add_attr(state_t *s, hid_t oid, unsigned int which, unsigned num_attrs, const char *aname_fmt,
         unsigned int g_which)
{

    char     attrname[VS_ATTR_NAME_LEN];
    unsigned u;
    unsigned attr_value;
    hid_t    aid    = H5I_INVALID_HID;
    hid_t    amtype = H5I_INVALID_HID;
    hid_t    atype  = s->filetype;
    hid_t    sid    = s->one_by_one_sid;

    /* Need to obtain native datatype for H5Aread */
    if ((amtype = H5Tget_native_type(atype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    for (u = 0; u < num_attrs; u++) {

        /* Create attribute */
        /* Construct attribute name like attr-0-0 */
        sprintf(attrname, aname_fmt, which, u);
        if ((aid = H5Acreate2(oid, attrname, atype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            printf("H5Acreate2 failed\n");
            TEST_ERROR;
        }

        attr_value = u + which;

        dbgf(1, "setting attribute %s on group %u to %u\n", attrname, g_which, u + which);

        /* Write data into the attribute */
        if (H5Awrite(aid, amtype, &attr_value) < 0) {
            printf("H5Awrite failed\n");
            TEST_ERROR;
        }

        /* Close attribute */
        if (H5Aclose(aid) < 0) {
            printf("H5Aclose failed\n");
            TEST_ERROR;
        }

        /* If coming to an "object header continuation block" test,
         * we need to check if this test behaves as expected. */
        if (s->at_pattern == 'a' || s->at_pattern == 'R') {
            if (false == check_ohr_num_chunk(oid, false)) {
                printf("An object header continuation block should be created. \n");
                printf("But it is not.\n");
                TEST_ERROR;
            }
        }

        /* Writer sends a message to reader: an attribute is successfully generated.
           then wait for the reader to verify and send an acknowledgement message back.*/
        if (s->use_communication && s->attr_test == true) {
            dbgf(2, "writer: write attr - ready to send/receive message: %d\n", s->sock.notify + 1);
            if (sock_wr_send_receive(s) == false) {
                H5_FAILED();
                AT();
                dbgf(2, "writer: write attr - verification failed.\n");
                /* Note: This is (mostly) because the verification failure message
                 *       from the reader. So don't send the error message back to
                 *       the reader. Just stop the test. */
                goto error2;
            }
        }

    } /* end for */

    if (H5Tclose(amtype) < 0) {
        TEST_ERROR;
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(amtype);
    }
    H5E_END_TRY;

    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    add_attr
 *
 * Purpose:     Add attributes to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *              unsigned num_attrs
 *              The number of attributes to be created
 *
 *              const char*aname_fmt
 *              The attribute name template used to create unique attribute names.
 *
 *              unsigned int g_which
 *              This parameter is used to generate correct group name in a key
 *              debugging message.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
add_attr(state_t *s, hid_t oid, unsigned int which, unsigned num_attrs, const char *aname_fmt,
         unsigned int g_which)
{

    char     attrname[VS_ATTR_NAME_LEN];
    unsigned u;
    unsigned attr_value;
    hid_t    aid    = H5I_INVALID_HID;
    hid_t    amtype = H5I_INVALID_HID;
    hid_t    atype  = s->filetype;
    hid_t    sid    = s->one_by_one_sid;

    /* Need to obtain native datatype for H5Aread */
    if ((amtype = H5Tget_native_type(atype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    for (u = 0; u < num_attrs; u++) {

        /* Create attribute */
        /* Construct attribute name like attr-0-0 */
        sprintf(attrname, aname_fmt, which, u);
        if ((aid = H5Acreate2(oid, attrname, atype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            printf("H5Acreate2 failed\n");
            TEST_ERROR;
        }

        attr_value = u + which;

        dbgf(1, "setting attribute %s on group %u to %u\n", attrname, g_which, u + which);

        /* Write data into the attribute */
        if (H5Awrite(aid, amtype, &attr_value) < 0) {
            printf("H5Awrite failed\n");
            TEST_ERROR;
        }

        /* Close attribute */
        if (H5Aclose(aid) < 0) {
            printf("H5Aclose failed\n");
            TEST_ERROR;
        }

        /* If coming to an "object header continuation block" test,
         * we need to check if this test behaves as expected. */
        if (s->at_pattern == 'a' || s->at_pattern == 'R') {
            if (false == check_ohr_num_chunk(oid, false)) {
                printf("An object header continuation block should be created. \n");
                printf("But it is not.\n");
                TEST_ERROR;
            }
        }

        /* Writer sends a message to reader: an attribute is successfully generated.
           then wait for the reader to verify and send an acknowledgement message back.*/
        if (s->use_communication && s->attr_test == true) {
            dbgf(2, "writer: write attr - ready to send/receive message: %d\n", s->np_notify + 1);
            if (np_wr_send_receive(s) == false) {
                H5_FAILED();
                AT();
                dbgf(2, "writer: write attr - verification failed.\n");
                /* Note: This is (mostly) because the verification failure message
                 *       from the reader. So don't send the error message back to
                 *       the reader. Just stop the test. */
                goto error2;
            }
        }

    } /* end for */

    if (H5Tclose(amtype) < 0) {
        TEST_ERROR;
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);

error2:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(amtype);
    }
    H5E_END_TRY;

    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    add_default_group_attr
 *
 * Purpose:     Add an attribute to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "dense" storage test.
 *              It is also used by the group-only, "add-ohr-block"
 *              and "del-ohr-block" tests.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_default_group_attr(state_t *s, hid_t g, unsigned int which)
{

    const char *aname_format = "attr-%u";

    /* Note: Since we only add one attribute, the parameter for
     *        the number of attributes is 1. */
    return add_attr(s, g, which, 1, aname_format, which);
}
#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    add_vlstr_attr
 *
 * Purpose:     Add a variable length string  attribute to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "vstr" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;
    hid_t sid      = s->one_by_one_sid;

    /* Allocate buffer for the VL string value */
    astr_val = malloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        printf("Allocate memory for VL string failed.\n");
        TEST_ERROR;
    }

    /* Assign the VL string value and the attribute name.. */
    sprintf(astr_val, "%u", which);
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    /* Generate the VL string attribute.*/
    if ((aid = H5Acreate2(g, name, atype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        printf("H5Acreate2 failed.\n");
        TEST_ERROR;
    }

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        printf("H5Awrite failed.\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose() failed\n");
        TEST_ERROR;
    }
    if (H5Aclose(aid) < 0) {
        printf("H5Aclose() failed\n");
        TEST_ERROR;
    }

    free(astr_val);

    /* Writer sends a message to reader: a VL string attribute is successfully generated.
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: write attr - ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;

    if (astr_val)
        free(astr_val);

error2:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    del_one_attr
 *
 * Purpose:     delete one attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hbool_t is_dense
 *              if the deleted attribute is for checking the dense storage
 *
 *              hbool_t is_vl_or_ohrc
 *              if the deleted attribute is a VL string or for object header
 *              continuation check test
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute names
 *              according to if this attribute is a VL string or for checking
 *              the dense storage or the storage transition from dense to
 *              compact.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
del_one_attr(state_t *s, hid_t obj_id, hbool_t is_dense, hbool_t is_vl_or_ohrc, unsigned int which)
{

    char attrname[VS_ATTR_NAME_LEN];

    /*attribute name template for the dense storage related deletion operation */
    const char *aname_format_d = "attr-d-%u-%u";

    /*attribute name template used for general attribute deletion operation */
    const char *aname_format = "attr-%u-%u";

    /*attribute name template used for VL string attribute deletion
     * or object header continuation check operations */
    const char *aname_format_vl = "attr-%u";

    dbgf(2, "writer: coming to delete the attribute.\n");

    /* Construct the attribute name */
    if (is_dense == true)
        sprintf(attrname, aname_format_d, which, 0);
    else if (is_vl_or_ohrc == true)
        sprintf(attrname, aname_format_vl, which, 0);
    else
        sprintf(attrname, aname_format, which, 0);

    /* Delete the attribute */
    if (H5Adelete(obj_id, attrname) < 0) {
        printf("H5Adelete() failed\n");
        TEST_ERROR;
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(obj_id, true)) {
            printf("The object header chunk should not continue. \n");
            TEST_ERROR;
        }
    }
    /* Writer sends a message to reader: an attribute is successfully generated.
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: delete attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    if (s->use_communication && s->attr_test == true)
        if (!sock_send_error(s))
            fprintf(stderr, "Sending error message failed");

error2:
    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    add_vlstr_attr
 *
 * Purpose:     Add a variable length string  attribute to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "vstr" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;
    hid_t sid      = s->one_by_one_sid;

    /* Allocate buffer for the VL string value */
    astr_val = malloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        printf("Allocate memory for VL string failed.\n");
        TEST_ERROR;
    }

    /* Assign the VL string value and the attribute name.. */
    sprintf(astr_val, "%u", which);
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    /* Generate the VL string attribute.*/
    if ((aid = H5Acreate2(g, name, atype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        printf("H5Acreate2 failed.\n");
        TEST_ERROR;
    }

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        printf("H5Awrite failed.\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose() failed\n");
        TEST_ERROR;
    }
    if (H5Aclose(aid) < 0) {
        printf("H5Aclose() failed\n");
        TEST_ERROR;
    }

    free(astr_val);

    /* Writer sends a message to reader: a VL string attribute is successfully generated.
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: write attr - ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;

    if (astr_val)
        free(astr_val);

error2:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    del_one_attr
 *
 * Purpose:     delete one attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hbool_t is_dense
 *              if the deleted attribute is for checking the dense storage
 *
 *              hbool_t is_vl_or_ohrc
 *              if the deleted attribute is a VL string or for object header
 *              continuation check test
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute names
 *              according to if this attribute is a VL string or for checking
 *              the dense storage or the storage transition from dense to
 *              compact.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
del_one_attr(state_t *s, hid_t obj_id, hbool_t is_dense, hbool_t is_vl_or_ohrc, unsigned int which)
{

    char attrname[VS_ATTR_NAME_LEN];

    /*attribute name template for the dense storage related deletion operation */
    const char *aname_format_d = "attr-d-%u-%u";

    /*attribute name template used for general attribute deletion operation */
    const char *aname_format = "attr-%u-%u";

    /*attribute name template used for VL string attribute deletion
     * or object header continuation check operations */
    const char *aname_format_vl = "attr-%u";

    dbgf(2, "writer: coming to delete the attribute.\n");

    /* Construct the attribute name */
    if (is_dense == true)
        sprintf(attrname, aname_format_d, which, 0);
    else if (is_vl_or_ohrc == true)
        sprintf(attrname, aname_format_vl, which, 0);
    else
        sprintf(attrname, aname_format, which, 0);

    /* Delete the attribute */
    if (H5Adelete(obj_id, attrname) < 0) {
        printf("H5Adelete() failed\n");
        TEST_ERROR;
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(obj_id, true)) {
            printf("The object header chunk should not continue. \n");
            TEST_ERROR;
        }
    }
    /* Writer sends a message to reader: an attribute is successfully generated.
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: delete attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    if (s->use_communication && s->attr_test == true)
        if (!np_send_error(s, true))
            fprintf(stderr, "Sending error message failed");

error2:
    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    add_del_vlstr_attr
 *
 * Purpose:     Add a variable length string attribute
 *              then delete this attribute in this a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "remove-vstr" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_del_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t ret_value = false;

    /* Add a VL string attribute then delete it. */
    ret_value = add_vlstr_attr(s, g, which);
    if (ret_value == true)
        ret_value = del_one_attr(s, g, false, true, which);

    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    modify_attr
 *
 * Purpose:     Modify the value of an attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              const char*aname_fmt
 *              The attribute name template used to create unique attribute names.
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
modify_attr(state_t *s, hid_t g, const char *aname_fmt, unsigned int which)
{

    char         attrname[VS_ATTR_NAME_LEN];
    hid_t        aid    = H5I_INVALID_HID;
    hid_t        amtype = H5I_INVALID_HID;
    unsigned int modify_value;

    sprintf(attrname, aname_fmt, which, 0);
    if ((aid = H5Aopen(g, attrname, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed\n");
        TEST_ERROR;
    }

    if ((amtype = H5Tget_native_type(s->filetype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    /* Make a large number to verify the change easily */
    modify_value = which + 10000;

    if (H5Awrite(aid, amtype, &modify_value) < 0) {
        printf("H5Awrite failed\n");
        TEST_ERROR;
    }
    if (H5Tclose(amtype) < 0) {
        printf("H5Tclose failed\n");
        TEST_ERROR;
    }
    if (H5Aclose(aid) < 0) {
        printf("H5Aclose failed\n");
        TEST_ERROR;
    }

    /* Writer sends a message to reader: an attribute is successfully modified.
           then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: modify attr - ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }

    return true;
error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(aid);
    }
    H5E_END_TRY;

error2:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    modify_vlstr_attr
 *
 * Purpose:     Modify the value of an VL string attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
modify_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;

    astr_val = malloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        printf("Allocate memory for VL string failed.\n");
        TEST_ERROR;
    }

    /* Change the VL string value and create the attribute name. */
    sprintf(astr_val, "%u%c", which, 'A');
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    /* Open this attribute. */
    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed.\n");
        TEST_ERROR;
    }

    dbgf(1, "The modified VL string value  is  %s \n", astr_val);

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        printf("H5Awrite failed.\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose() failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        printf("H5Aclose() failed\n");
        TEST_ERROR;
    }

    free(astr_val);

    /* Writer sends a message to reader: a VL string attribute is successfully generated.
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: modify vl attr - ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;

    if (astr_val)
        free(astr_val);

    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:
    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    modify_attr
 *
 * Purpose:     Modify the value of an attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              const char*aname_fmt
 *              The attribute name template used to create unique attribute names.
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
modify_attr(state_t *s, hid_t g, const char *aname_fmt, unsigned int which)
{

    char         attrname[VS_ATTR_NAME_LEN];
    hid_t        aid    = H5I_INVALID_HID;
    hid_t        amtype = H5I_INVALID_HID;
    unsigned int modify_value;

    sprintf(attrname, aname_fmt, which, 0);
    if ((aid = H5Aopen(g, attrname, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed\n");
        TEST_ERROR;
    }

    if ((amtype = H5Tget_native_type(s->filetype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    /* Make a large number to verify the change easily */
    modify_value = which + 10000;

    if (H5Awrite(aid, amtype, &modify_value) < 0) {
        printf("H5Awrite failed\n");
        TEST_ERROR;
    }
    if (H5Tclose(amtype) < 0) {
        printf("H5Tclose failed\n");
        TEST_ERROR;
    }
    if (H5Aclose(aid) < 0) {
        printf("H5Aclose failed\n");
        TEST_ERROR;
    }

    /* Writer sends a message to reader: an attribute is successfully modified.
           then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: modify attr - ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }

    return true;
error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(aid);
    }
    H5E_END_TRY;

error2:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    modify_vlstr_attr
 *
 * Purpose:     Modify the value of an VL string attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
modify_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;

    astr_val = malloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        printf("Allocate memory for VL string failed.\n");
        TEST_ERROR;
    }

    /* Change the VL string value and create the attribute name. */
    sprintf(astr_val, "%u%c", which, 'A');
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    /* Open this attribute. */
    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed.\n");
        TEST_ERROR;
    }

    dbgf(1, "The modified VL string value  is  %s \n", astr_val);

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        printf("H5Awrite failed.\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose() failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        printf("H5Aclose() failed\n");
        TEST_ERROR;
    }

    free(astr_val);

    /* Writer sends a message to reader: a VL string attribute is successfully generated.
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: modify vl attr - ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;

    if (astr_val)
        free(astr_val);

    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);

error2:
    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    add_modify_vlstr_attr
 *
 * Purpose:     Add a variable length string attribute
 *              then modify this attribute in this a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "modify-vstr" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_modify_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t ret_value = false;
    ret_value         = add_vlstr_attr(s, g, which);
    if (true == ret_value)
        ret_value = modify_vlstr_attr(s, g, which);

    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    add_attrs_compact
 *
 * Purpose:     Add some attributes to the group.
 *              the number of attributes should be the maximal number of
 *              attributes that the compact storage can hold
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "modify-vstr" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    unsigned    max_compact  = 0;
    unsigned    min_dense    = 0;
    const char *aname_format = "attr-%u-%u";

    if (s->old_style_grp)
        max_compact = 2;
    else {
        /* Obtain the maximal number of attributes to be stored in compact
         * storage and the minimal number of attributes to be stored in
         * dense storage. */
        if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
            printf("H5Pget_attr_phase_change() failed\n");
            TEST_ERROR;
        }
    }

    /* Add max_compact attributes, these attributes are stored in
     * compact storage. */
    return add_attr(s, g, which, max_compact, aname_format, which);

error:
    if (s->use_communication && s->attr_test == true) {
        sock_send_error(s);
    }
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_attrs_compact_dense
 *
 * Purpose:     Add some attributes to the group.
 *              First, the number of attributes should be the maximal number
 *              of attributes that the compact storage can hold.
 *              Then,  add another attribute, the storage becomes dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "compact-to-dense" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    unsigned    max_compact  = 0;
    unsigned    min_dense    = 0;
    const char *aname_format = "attr-d-%u-%u";
    hbool_t     ret_value    = false;

    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }

    /* Add attributes, until just before converting to dense storage */
    ret_value = add_attrs_compact(s, g, gcpl, which);

    /* Add another attribute, the storage becomes dense. */
    if (ret_value == true)
        ret_value = add_attr(s, g, which + max_compact, 1, aname_format, which);

    return ret_value;

error:
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    del_attrs_compact_dense_compact
 *
 * Purpose:     delete some attributes in the group.
 *              The number of attributes are deleted in such a way
 *              that the attribute storage changes from compact to
 *              dense then to compact again.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is an internal function used by the
 *              "dense-del-to-compact" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
del_attrs_compact_dense_compact(state_t *s, hid_t obj_id, hid_t gcpl, unsigned int which)
{

    unsigned max_compact = 0;
    unsigned min_dense   = 0;
    unsigned u           = 0;

    char        attrname[VS_ATTR_NAME_LEN];
    const char *aname_format  = "attr-%u-%u";
    const char *adname_format = "attr-d-%u-%u";

    /* Obtain the maximal number of attributes to be stored in compact
     * storage and the minimal number of attributes to be stored in
     * dense storage. */
    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }
    u = max_compact + 1;

    /* delete a number of attributes so that the attribute storage just becomes dense.*/
    for (u--; u >= (min_dense - 1); u--) {
        sprintf(attrname, aname_format, which, max_compact - u);
        if (H5Adelete(obj_id, attrname) < 0) {
            printf("H5Adelete failed\n");
            TEST_ERROR;
        }

        /* For each attribute deletion, we want to ensure the verification
         * from the reader.
         * So writer sends a message to reader: an attribute is successfully deleted.
           then wait for reader to verify and send an acknowledgement message back. */
        if (s->use_communication && s->attr_test == true) {
            dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->sock.notify + 1);
            if (sock_wr_send_receive(s) == false) {
                H5_FAILED();
                AT();
                dbgf(2, "writer: delete attr - verification failed.\n");
                goto error2;
            }
        }
    }

    /* The writer deletes another attribute, the storage is
     * still dense. However, the attribute to be deleted
     * doesn't follow the previous for loop. It may be
     * in different location in the object header. Just add
     * a litter variation to check if this operation is successful.
     * The attribute name to be deleted is attr-max_compact+which-0
     */

    sprintf(attrname, adname_format, max_compact + which, 0);
    if (H5Adelete(obj_id, attrname) < 0) {
        printf("H5Adelete failed\n");
        TEST_ERROR;
    }
    /* Again we need to notify the reader via Named pipe. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: delete attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:
    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    add_attrs_compact
 *
 * Purpose:     Add some attributes to the group.
 *              the number of attributes should be the maximal number of
 *              attributes that the compact storage can hold
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "modify-vstr" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    unsigned    max_compact  = 0;
    unsigned    min_dense    = 0;
    const char *aname_format = "attr-%u-%u";

    if (s->old_style_grp)
        max_compact = 2;
    else {
        /* Obtain the maximal number of attributes to be stored in compact
         * storage and the minimal number of attributes to be stored in
         * dense storage. */
        if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
            printf("H5Pget_attr_phase_change() failed\n");
            TEST_ERROR;
        }
    }

    /* Add max_compact attributes, these attributes are stored in
     * compact storage. */
    return add_attr(s, g, which, max_compact, aname_format, which);

error:
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_attrs_compact_dense
 *
 * Purpose:     Add some attributes to the group.
 *              First, the number of attributes should be the maximal number
 *              of attributes that the compact storage can hold.
 *              Then,  add another attribute, the storage becomes dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "compact-to-dense" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    unsigned    max_compact  = 0;
    unsigned    min_dense    = 0;
    const char *aname_format = "attr-d-%u-%u";
    hbool_t     ret_value    = false;

    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }

    /* Add attributes, until just before converting to dense storage */
    ret_value = add_attrs_compact(s, g, gcpl, which);

    /* Add another attribute, the storage becomes dense. */
    if (ret_value == true)
        ret_value = add_attr(s, g, which + max_compact, 1, aname_format, which);

    return ret_value;

error:
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    del_attrs_compact_dense_compact
 *
 * Purpose:     delete some attributes in the group.
 *              The number of attributes are deleted in such a way
 *              that the attribute storage changes from compact to
 *              dense then to compact again.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is an internal function used by the
 *              "dense-del-to-compact" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
del_attrs_compact_dense_compact(state_t *s, hid_t obj_id, hid_t gcpl, unsigned int which)
{

    unsigned max_compact = 0;
    unsigned min_dense   = 0;
    unsigned u           = 0;

    char        attrname[VS_ATTR_NAME_LEN];
    const char *aname_format  = "attr-%u-%u";
    const char *adname_format = "attr-d-%u-%u";

    /* Obtain the maximal number of attributes to be stored in compact
     * storage and the minimal number of attributes to be stored in
     * dense storage. */
    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }
    u = max_compact + 1;

    /* delete a number of attributes so that the attribute storage just becomes dense.*/
    for (u--; u >= (min_dense - 1); u--) {
        sprintf(attrname, aname_format, which, max_compact - u);
        if (H5Adelete(obj_id, attrname) < 0) {
            printf("H5Adelete failed\n");
            TEST_ERROR;
        }

        /* For each attribute deletion, we want to ensure the verification
         * from the reader.
         * So writer sends a message to reader: an attribute is successfully deleted.
           then wait for reader to verify and send an acknowledgement message back. */
        if (s->use_communication && s->attr_test == true) {
            dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify + 1);
            if (np_wr_send_receive(s) == false) {
                H5_FAILED();
                AT();
                dbgf(2, "writer: delete attr - verification failed.\n");
                goto error2;
            }
        }
    }

    /* The writer deletes another attribute, the storage is
     * still dense. However, the attribute to be deleted
     * doesn't follow the previous for loop. It may be
     * in different location in the object header. Just add
     * a litter variation to check if this operation is successful.
     * The attribute name to be deleted is attr-max_compact+which-0
     */

    sprintf(attrname, adname_format, max_compact + which, 0);
    if (H5Adelete(obj_id, attrname) < 0) {
        printf("H5Adelete failed\n");
        TEST_ERROR;
    }
    /* Again we need to notify the reader via Named pipe. */
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            dbgf(2, "writer: delete attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);

error2:
    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    add_del_attrs_compact
 *
 * Purpose:     Add some attributes to the group and then delete one attribute.
 *              First, the number of attributes to be added should be the
 *              maximal number of attributes that the compact storage can hold.
 *              Then,  delete one attribute, the storage is still compact.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "compact-del" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_del_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    hbool_t ret_value = false;
    ret_value         = add_attrs_compact(s, g, gcpl, which);
    if (ret_value == true) {
        dbgf(2, "writer: before deleting the attribute.\n");
        ret_value = del_one_attr(s, g, false, false, which);
    }

    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    add_del_attrs_compact_dense
 *
 * Purpose:     Add some attributes to the group and then delete one attribute.
 *              First, the number of attributes to be added exceeds
 *              the maximal number of attributes that the compact storage can hold.
 *              The storage changes from compact to dense.
 *              Then,  delete one attribute, the storage is still dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "dense-del" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_del_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    hbool_t  ret_value   = false;
    unsigned max_compact = 0;
    unsigned min_dense   = 0;

    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }

    ret_value = add_attrs_compact_dense(s, g, gcpl, which);
    if (ret_value == true)
        ret_value = del_one_attr(s, g, true, false, which + max_compact);

    return ret_value;

error:
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);
    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    add_del_attrs_compact_dense
 *
 * Purpose:     Add some attributes to the group and then delete one attribute.
 *              First, the number of attributes to be added exceeds
 *              the maximal number of attributes that the compact storage can hold.
 *              The storage changes from compact to dense.
 *              Then,  delete one attribute, the storage is still dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "dense-del" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_del_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    hbool_t  ret_value   = false;
    unsigned max_compact = 0;
    unsigned min_dense   = 0;

    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }

    ret_value = add_attrs_compact_dense(s, g, gcpl, which);
    if (ret_value == true)
        ret_value = del_one_attr(s, g, true, false, which + max_compact);

    return ret_value;

error:
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);
    return false;
}
#endif /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    add_del_attrs_compact_dense_compact
 *
 * Purpose:     Add attributes to the group and then delete some of them.
 *              First, the number of attributes to be added exceeds
 *              the maximal number of attributes that the compact storage can hold.
 *              The storage changes from compact to dense.
 *              Then,  delete some attributes, the storage changes from
 *              dense to compact again.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "dense-del-to-compact" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_del_attrs_compact_dense_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    hbool_t ret_value = false;
    ret_value         = add_attrs_compact_dense(s, g, gcpl, which);
    if (ret_value == true)
        ret_value = del_attrs_compact_dense_compact(s, g, gcpl, which);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    add_modify_default_group_attr
 *
 * Purpose:     Add an attribute then modify the value to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "modify" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_modify_default_group_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t     ret_value    = false;
    const char *aname_format = "attr-%u";
    ret_value                = add_default_group_attr(s, g, which);
    if (ret_value == true)
        ret_value = modify_attr(s, g, aname_format, which);
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    del_ohr_block_attr
 *
 * Purpose:     Add an attribute to force creation of object header
 *              continuation block and remove this attribute to delete
 *              the object header continuation block
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the
 *              "deletion of object header continuation block" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
del_ohr_block_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t ret_value = false;
    ret_value         = add_default_group_attr(s, g, which);
    if (ret_value == true)
        ret_value = del_one_attr(s, g, false, true, which);
    return ret_value;
}
/*-------------------------------------------------------------------------
 * Function:    add_group_attribute
 *
 * Purpose:     Check the attribute test pattern and then call the
 *              corresponding test function..
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the write_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
add_group_attribute(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    hbool_t ret_value    = false;
    char    test_pattern = s->at_pattern;

    switch (test_pattern) {
        case 'c':
            ret_value = add_attrs_compact(s, g, gcpl, which);
            break;
        case 't':
            ret_value = add_attrs_compact_dense(s, g, gcpl, which);
            break;
        case 'C':
            ret_value = add_del_attrs_compact(s, g, gcpl, which);
            break;
        case 'D':
            ret_value = add_del_attrs_compact_dense(s, g, gcpl, which);
            break;
        case 'T':
            ret_value = add_del_attrs_compact_dense_compact(s, g, gcpl, which);
            break;
        case 'M':
            ret_value = add_modify_default_group_attr(s, g, which);
            break;
        case 'v':
            ret_value = add_vlstr_attr(s, g, which);
            break;
        case 'r':
            ret_value = add_del_vlstr_attr(s, g, which);
            break;
        case 'm':
            ret_value = add_modify_vlstr_attr(s, g, which);
            break;
        case 'R':
            ret_value = del_ohr_block_attr(s, g, which);
            break;
        case 'a':
        case 'd':
        case ' ':
        default:
            ret_value = add_default_group_attr(s, g, which);
            break;
    }
    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    write_group
 *
 * Purpose:     Create a group and carry out attribute operations(add,delete etc.)
 *              according to the attribute test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
write_group(state_t *s, unsigned int which)
{
    char       name[sizeof("/group-9999999999")];
    hid_t      g       = H5I_INVALID_HID;
    hid_t      dummy_d = H5I_INVALID_HID;
    hid_t      gcpl    = H5I_INVALID_HID;
    hbool_t    result  = true;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if (s->old_style_grp)
        gcpl = H5P_DEFAULT;
    else {
        gcpl = H5Pcreate(H5P_GROUP_CREATE);
        if (gcpl < 0) {
            printf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        /* If we test the dense storage, change the attribute phase. */
        if (s->at_pattern == 'd') {
            if (H5Pset_attr_phase_change(gcpl, 0, 0) < 0) {
                printf("H5Pset_attr_phase_change failed for the dense storage.\n");
                TEST_ERROR;
            }
        }
    }

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    /* We need to create a dummy dataset for the object header continuation block test. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if ((dummy_d = H5Dcreate2(g, "Dataset", H5T_NATIVE_INT, s->one_by_one_sid, H5P_DEFAULT, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 failed\n");
            TEST_ERROR;
        }
    }
    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Old-styled group test: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the new-style.\n");
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, true)) {
            printf("An object header continuation block should NOT be created. \n");
            printf("But it is created.\n");
            TEST_ERROR;
        }
    }

    /* If an attribute test is turned on and named pipes are used,
     * the writer should send and receive messages after the group creation.
     * This will distinguish an attribute operation error from an
     * group creation error.
     * Writer sends a message to reader: an attribute is successfully generated.
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }

    /* Then carry out the attribute operation. */
    if (s->asteps != 0 && which % s->asteps == 0)
        result = add_group_attribute(s, g, gcpl, which);

    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (H5Dclose(dummy_d) < 0) {
            printf("H5Dclose failed\n");
            TEST_ERROR;
        }
    }
    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    if (!s->old_style_grp && H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    return result;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:

    H5E_BEGIN_TRY
    {
        if (s->at_pattern == 'a' || s->at_pattern == 'R')
            H5Dclose(dummy_d);
        H5Gclose(g);
        if (!s->old_style_grp)
            H5Pclose(gcpl);
    }
    H5E_END_TRY;

    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    write_group
 *
 * Purpose:     Create a group and carry out attribute operations(add,delete etc.)
 *              according to the attribute test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
write_group(state_t *s, unsigned int which)
{
    char       name[sizeof("/group-9999999999")];
    hid_t      g       = H5I_INVALID_HID;
    hid_t      dummy_d = H5I_INVALID_HID;
    hid_t      gcpl    = H5I_INVALID_HID;
    hbool_t    result  = true;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if (s->old_style_grp)
        gcpl = H5P_DEFAULT;
    else {
        gcpl = H5Pcreate(H5P_GROUP_CREATE);
        if (gcpl < 0) {
            printf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        /* If we test the dense storage, change the attribute phase. */
        if (s->at_pattern == 'd') {
            if (H5Pset_attr_phase_change(gcpl, 0, 0) < 0) {
                printf("H5Pset_attr_phase_change failed for the dense storage.\n");
                TEST_ERROR;
            }
        }
    }

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    /* We need to create a dummy dataset for the object header continuation block test. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if ((dummy_d = H5Dcreate2(g, "Dataset", H5T_NATIVE_INT, s->one_by_one_sid, H5P_DEFAULT, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 failed\n");
            TEST_ERROR;
        }
    }
    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Old-styled group test: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the new-style.\n");
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, true)) {
            printf("An object header continuation block should NOT be created. \n");
            printf("But it is created.\n");
            TEST_ERROR;
        }
    }

    /* If an attribute test is turned on and named pipes are used,
     * the writer should send and receive messages after the group creation.
     * This will distinguish an attribute operation error from an
     * group creation error.
     * Writer sends a message to reader: an attribute is successfully generated.
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->attr_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }

    /* Then carry out the attribute operation. */
    if (s->asteps != 0 && which % s->asteps == 0)
        result = add_group_attribute(s, g, gcpl, which);

    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (H5Dclose(dummy_d) < 0) {
            printf("H5Dclose failed\n");
            TEST_ERROR;
        }
    }
    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    if (!s->old_style_grp && H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    return result;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, true);

error2:

    H5E_BEGIN_TRY
    {
        if (s->at_pattern == 'a' || s->at_pattern == 'R')
            H5Dclose(dummy_d);
        H5Gclose(g);
        if (!s->old_style_grp)
            H5Pclose(gcpl);
    }
    H5E_END_TRY;

    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    check_attr_storage_type
 *
 * Purpose:     Check if the attribute storage type is correct
 *
 * Parameters:  hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hbool_t is_compact
 *              true if the attribute is stored in compact storage
 *              false if the attribute is stored in dense storage
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
check_attr_storage_type(hid_t g, hbool_t is_compact)
{

    H5O_native_info_t ninfo;

    /* Get the object information */
    if (H5Oget_native_info(g, &ninfo, H5O_NATIVE_INFO_HDR | H5O_NATIVE_INFO_META_SIZE) < 0) {
        printf("H5Oget_native_info failed\n");
        TEST_ERROR;
    }

    if (is_compact) {
        if (ninfo.meta_size.attr.index_size != 0 || ninfo.meta_size.attr.heap_size != 0) {
            printf("Should be in compact storage,but it is not.\n");
            TEST_ERROR;
        }
    }
    else {
        if (ninfo.meta_size.attr.index_size == 0 || ninfo.meta_size.attr.heap_size == 0) {
            printf("Should be in dense storage,but it is not.\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    vrfy_attr
 *
 * Purpose:     Verify is a group attribute value is as expected.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *              const char*aname
 *              The attribute name
 *
 *              unsigned int g_which
 *              This parameter is used to generate correct group name in a key
 *              debugging message.
 *
 *              hbool_t check_storage
 *              a flag to indicate if the storage check is on
 *
 *              hbool_t is_compact
 *              true if the attribute storage should be in compact
 *              false if the attribute storage should be in dense
 *              Note: this parameter is not used if the check_storage
 *              is set to false.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_attr(state_t *s, hid_t g, unsigned int which, const char *aname, unsigned int g_which,
          hbool_t check_storage, hbool_t is_compact)
{

    unsigned int read_which;
    hid_t        aid    = H5I_INVALID_HID;
    hid_t        amtype = H5I_INVALID_HID;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if receiving an error
     * message.
     */
    if (s->use_communication && true == s->attr_test) {
        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            /* Since receiving the error message from the writer,
             * just stop the test. */
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n", s->sock.notify);
    }

    /* Go ahead to read the attribute. */
    dbgf(1, "verifying attribute %s on group %u equals %u\n", aname, g_which, which);

    if ((amtype = H5Tget_native_type(s->filetype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    if ((aid = H5Aopen(g, aname, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed\n");
        TEST_ERROR;
    }

    if (H5Aread(aid, amtype, &read_which) < 0) {
        printf("H5Aread failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        printf("H5Aclose failed\n");
        TEST_ERROR;
    }

    if (read_which != which) {
        printf("reader: the add_attribute verification failed,expected value is  %d\n", which);
        printf("reader: the add_attribute verification failed, the value is %d\n", read_which);
        printf("The add_attribute verification failed\n");
        TEST_ERROR;
    }

    if (!s->old_style_grp && check_storage == true) {
        if (false == check_attr_storage_type(g, is_compact)) {
            printf("The attribute storage type is wrong. \n");
            TEST_ERROR;
        }
        dbgf(2, "reader: finish checking the storage type: %d\n", s->sock.notify);
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, false)) {
            printf("An object header continuation block should be created. \n");
            printf("But it is not.\n");
            printf("Verification of 'object header continuation block test' failed.\n");
            TEST_ERROR;
        }
    }

    /* If the read value is expected, send back an OK message to the writer. */
    if (s->use_communication && s->attr_test == true) {
        if (sock_rd_send(s) == false) {
            printf("socket reader send message error\n");
            TEST_ERROR;
        }
        dbgf(2, "reader: finish sending back the message: %d\n", s->sock.notify);
    }
    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(amtype);
        H5Aclose(aid);
    }
    H5E_END_TRY;

    /* Send back an error message to the writer so that the writer can stop. */
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);
error2:
    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    vrfy_attr
 *
 * Purpose:     Verify is a group attribute value is as expected.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *              const char*aname
 *              The attribute name
 *
 *              unsigned int g_which
 *              This parameter is used to generate correct group name in a key
 *              debugging message.
 *
 *              hbool_t check_storage
 *              a flag to indicate if the storage check is on
 *
 *              hbool_t is_compact
 *              true if the attribute storage should be in compact
 *              false if the attribute storage should be in dense
 *              Note: this parameter is not used if the check_storage
 *              is set to false.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_attr(state_t *s, hid_t g, unsigned int which, const char *aname, unsigned int g_which,
          hbool_t check_storage, hbool_t is_compact)
{

    unsigned int read_which;
    hid_t        aid    = H5I_INVALID_HID;
    hid_t        amtype = H5I_INVALID_HID;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if receiving an error
     * message.
     */
    if (s->use_communication && true == s->attr_test) {
        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            /* Since receiving the error message from the writer,
             * just stop the test. */
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n", s->np_notify);
    }

    /* Go ahead to read the attribute. */
    dbgf(1, "verifying attribute %s on group %u equals %u\n", aname, g_which, which);

    if ((amtype = H5Tget_native_type(s->filetype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    if ((aid = H5Aopen(g, aname, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed\n");
        TEST_ERROR;
    }

    if (H5Aread(aid, amtype, &read_which) < 0) {
        printf("H5Aread failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        printf("H5Aclose failed\n");
        TEST_ERROR;
    }

    if (read_which != which) {
        printf("reader: the add_attribute verification failed,expected value is  %d\n", which);
        printf("reader: the add_attribute verification failed, the value is %d\n", read_which);
        printf("The add_attribute verification failed\n");
        TEST_ERROR;
    }

    if (!s->old_style_grp && check_storage == true) {
        if (false == check_attr_storage_type(g, is_compact)) {
            printf("The attribute storage type is wrong. \n");
            TEST_ERROR;
        }
        dbgf(2, "reader: finish checking the storage type: %d\n", s->np_notify);
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, false)) {
            printf("An object header continuation block should be created. \n");
            printf("But it is not.\n");
            printf("Verification of 'object header continuation block test' failed.\n");
            TEST_ERROR;
        }
    }

    /* If the read value is expected, send back an OK message to the writer. */
    if (s->use_communication && s->attr_test == true) {
        if (np_rd_send(s) == false) {
            printf("named pipe reader send message error\n");
            TEST_ERROR;
        }
        dbgf(2, "reader: finish sending back the message: %d\n", s->np_notify);
    }
    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(amtype);
        H5Aclose(aid);
    }
    H5E_END_TRY;

    /* Send back an error message to the writer so that the writer can stop. */
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, false);
error2:
    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    verify_default_group_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of a
 *              group attribute corrected by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The expected attribute value. It is also used to construct the
 *              group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "dense" storage test.
 *              It is also used by the group-only, "add-ohr-block"
 *              and "del-ohr-block" tests.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_default_group_attr(state_t *s, hid_t g, unsigned int which)
{
    char        attrname[VS_ATTR_NAME_LEN];
    const char *aname_format = "attr-%u";
    sprintf(attrname, aname_format, which);
    return vrfy_attr(s, g, which, attrname, which, false, true);
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    verify_modify_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of
 *              an attribute in a group, first the original value then
 *              the modified value.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The expected attribute value. It is also used to construct the
 *              group name. The modified attribute value can be derived from
 *              the expected attribute value.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "modified" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_modify_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t      ret       = false;
    const char  *aname_fmt = "attr-%u";
    unsigned int read_which;
    hid_t        aid    = H5I_INVALID_HID;
    hid_t        amtype = H5I_INVALID_HID;
    char         attrname[VS_ATTR_NAME_LEN];

    /* First verify the original attribute value */
    ret = verify_default_group_attr(s, g, which);

    /* Then the modified value */
    if (ret == true) {

        /* The reader receives a message from the writer.Then sleep
         * for a few ticks or stop the test if receiving an error
         * message.
         */
        if (s->use_communication && true == s->attr_test) {
            if (false == sock_rd_receive(s)) {
                H5_FAILED();
                AT();
                goto error2;
            }
            decisleep(s->tick_len * s->update_interval);
            dbgf(1, "Reader: finish reading the message: %d\n", s->sock.notify);
        }

        /* Go ahead to read the attribute. */
        esnprintf(attrname, sizeof(attrname), aname_fmt, which);
        if ((amtype = H5Tget_native_type(s->filetype, H5T_DIR_ASCEND)) < 0) {
            printf("H5Tget_native_type failed\n");
            TEST_ERROR;
        }

        if ((aid = H5Aopen(g, attrname, H5P_DEFAULT)) < 0) {
            printf("H5Aopen failed\n");
            TEST_ERROR;
        }

        if (H5Aread(aid, amtype, &read_which) < 0) {
            printf("H5Aread failed\n");
            TEST_ERROR;
        }

        if (H5Tclose(amtype) < 0) {
            printf("H5Tclose failed.\n");
            TEST_ERROR;
        }

        if (H5Aclose(aid) < 0) {
            printf("H5Aclose failed\n");
            TEST_ERROR;
        }

        /* verify the modified value */
        if (read_which != (which + 10000)) {
            printf("reader: the modified_attr() expected value is  %d\n", which + 10000);
            printf("reader: the modified_attr() actual value is %d\n", read_which);
            printf("The modify_attribute verification failed.\n");
            TEST_ERROR;
        }

        /* The reader sends an OK message back to the writer. */
        if (s->use_communication && s->attr_test == true) {
            if (sock_rd_send(s) == false)
                goto error2;
            dbgf(2, "reader: modify_attr finish sending back the message: %d\n", s->sock.notify);
        }
        return true;
    }
    return false;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(amtype);
    }
    H5E_END_TRY;

    /* The reader needs to send an error message back to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_group_vlstr_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of
 *              a variable length string attribute created by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value. It is also used
 *              to construct the group name.
 *
 *              hbool_t vrfy_mod
 *              true if this function is used for the modified VL string test.
 *              false if this function is just used for the VL string test.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is an internal function used by
 *              both the "vlstr" and the "modify-vstr" tests.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_group_vlstr_attr(state_t *s, hid_t g, unsigned int which, hbool_t vrfy_mod)
{
    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];

    char *astr_val_exp;
    char *astr_val = NULL;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->attr_test) {
        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n", s->sock.notify);
    }

    /* Go ahead to read the VL string attribute. */
    astr_val_exp = malloc(VS_ATTR_NAME_LEN);
    if (astr_val_exp == NULL) {
        printf("Allocate memory for expected buffer failed.\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "attr-%u", which);

    /* Construct the expected VL string value,depending if
     * it is the modified value or the original value. */
    if (vrfy_mod == true)
        sprintf(astr_val_exp, "%u%c", which, 'A');
    else
        sprintf(astr_val_exp, "%u", which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which, which);

    dbgf(1, "expected vl attr is= %s\n", astr_val_exp);

    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed\n");
        TEST_ERROR;
    }

    /* Create a VL string datatype  */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Aread(aid, atype, &astr_val) < 0) {
        printf("Cannot read the attribute.\n");
        TEST_ERROR;
    }

    dbgf(1, "read attr is= %s\n", astr_val);
    if (strcmp(astr_val, astr_val_exp) != 0) {
        printf("reader: the vl add_attribute verification failed,expected value is  %s\n", astr_val_exp);
        printf("reader: the vl add_attribute verification failed, the value is %s\n", astr_val);
        printf("The vl add_attribute verification failed\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose failed.\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        printf("H5Aclose failed.\n");
        TEST_ERROR;
    }

    H5free_memory(astr_val);
    free(astr_val_exp);

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->attr_test == true) {
        if (sock_rd_send(s) == false)
            goto error2;
        dbgf(2, "reader: finish sending back the message: %d\n", s->sock.notify);
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;
    if (astr_val)
        H5free_memory(astr_val);
    if (astr_val_exp)
        free(astr_val_exp);

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_one_attr
 *
 * Purpose:     Verify if an attribute is successfully deleted.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              const char* aname
 *              The name of the attribute to be deleted.
 *
 *              hbool_t check_storage
 *              a flag to indicate if the storage check is on
 *
 *              hbool_t is_compact
 *              true if after this attribute is deleted,
 *              the storage should still be in compact, false
 *              if the storage should be in dense.
 *              Note: this parameter is not used if the check_storage
 *              is set to false.

 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is an internal function used by "remove-vlstr",
 *              "compact-del","dense-del",dense-del-to-compact"  tests.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_del_one_attr(state_t *s, hid_t g, const char *aname, hbool_t check_storage, hbool_t is_compact)
{

    htri_t attr_exists = false;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->attr_test) {
        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n", s->sock.notify);
    }

    /* Check if the deleted attribute still exists. */
    attr_exists = H5Aexists_by_name(g, ".", aname, H5P_DEFAULT);
    if (attr_exists == false) {
        dbgf(1, "verify_del_attrs_compact() test: \n");
        dbgf(1, "  attribute %s is successfully deleted. \n", aname);
    }
    else if (attr_exists == true) {
        printf("The supposed deleted attribute %s still exists \n", aname);
        printf("verify_del_attrs_compact() test failed \n");
        TEST_ERROR;
    }
    else {
        printf("H5Aexists_by_name failed \n");
        TEST_ERROR;
    }

    if (!s->old_style_grp && check_storage == true) {
        if (false == check_attr_storage_type(g, is_compact)) {
            printf("The attribute storage type is wrong. \n");
            TEST_ERROR;
        }
        dbgf(2, "reader: finish checking the storage type: %d\n", s->sock.notify);
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, true)) {
            printf("An object header continuation block should be removed. \n");
            printf("But it is NOT.\n");
            printf("Verification of an 'object header continuation block test' failed.\n");
            TEST_ERROR;
        }
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->attr_test == true) {
        if (sock_rd_send(s) == false)
            TEST_ERROR;
        dbgf(2, "reader: finish sending back the message: %d\n", s->sock.notify);
    }

    return true;
error:
    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:
    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    verify_modify_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of
 *              an attribute in a group, first the original value then
 *              the modified value.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The expected attribute value. It is also used to construct the
 *              group name. The modified attribute value can be derived from
 *              the expected attribute value.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "modified" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_modify_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t      ret       = false;
    const char  *aname_fmt = "attr-%u";
    unsigned int read_which;
    hid_t        aid    = H5I_INVALID_HID;
    hid_t        amtype = H5I_INVALID_HID;
    char         attrname[VS_ATTR_NAME_LEN];

    /* First verify the original attribute value */
    ret = verify_default_group_attr(s, g, which);

    /* Then the modified value */
    if (ret == true) {

        /* The reader receives a message from the writer.Then sleep
         * for a few ticks or stop the test if receiving an error
         * message.
         */
        if (s->use_communication && true == s->attr_test) {
            if (false == np_rd_receive(s)) {
                H5_FAILED();
                AT();
                goto error2;
            }
            decisleep(s->tick_len * s->update_interval);
            dbgf(1, "Reader: finish reading the message: %d\n", s->np_notify);
        }

        /* Go ahead to read the attribute. */
        esnprintf(attrname, sizeof(attrname), aname_fmt, which);
        if ((amtype = H5Tget_native_type(s->filetype, H5T_DIR_ASCEND)) < 0) {
            printf("H5Tget_native_type failed\n");
            TEST_ERROR;
        }

        if ((aid = H5Aopen(g, attrname, H5P_DEFAULT)) < 0) {
            printf("H5Aopen failed\n");
            TEST_ERROR;
        }

        if (H5Aread(aid, amtype, &read_which) < 0) {
            printf("H5Aread failed\n");
            TEST_ERROR;
        }

        if (H5Tclose(amtype) < 0) {
            printf("H5Tclose failed.\n");
            TEST_ERROR;
        }

        if (H5Aclose(aid) < 0) {
            printf("H5Aclose failed\n");
            TEST_ERROR;
        }

        /* verify the modified value */
        if (read_which != (which + 10000)) {
            printf("reader: the modified_attr() expected value is  %d\n", which + 10000);
            printf("reader: the modified_attr() actual value is %d\n", read_which);
            printf("The modify_attribute verification failed.\n");
            TEST_ERROR;
        }

        /* The reader sends an OK message back to the writer. */
        if (s->use_communication && s->attr_test == true) {
            if (np_rd_send(s) == false)
                goto error2;
            dbgf(2, "reader: modify_attr finish sending back the message: %d\n", s->np_notify);
        }
        return true;
    }
    return false;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(amtype);
    }
    H5E_END_TRY;

    /* The reader needs to send an error message back to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, false);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_group_vlstr_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of
 *              a variable length string attribute created by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value. It is also used
 *              to construct the group name.
 *
 *              hbool_t vrfy_mod
 *              true if this function is used for the modified VL string test.
 *              false if this function is just used for the VL string test.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is an internal function used by
 *              both the "vlstr" and the "modify-vstr" tests.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_group_vlstr_attr(state_t *s, hid_t g, unsigned int which, hbool_t vrfy_mod)
{
    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];

    char *astr_val_exp;
    char *astr_val = NULL;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->attr_test) {
        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n", s->np_notify);
    }

    /* Go ahead to read the VL string attribute. */
    astr_val_exp = malloc(VS_ATTR_NAME_LEN);
    if (astr_val_exp == NULL) {
        printf("Allocate memory for expected buffer failed.\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "attr-%u", which);

    /* Construct the expected VL string value,depending if
     * it is the modified value or the original value. */
    if (vrfy_mod == true)
        sprintf(astr_val_exp, "%u%c", which, 'A');
    else
        sprintf(astr_val_exp, "%u", which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which, which);

    dbgf(1, "expected vl attr is= %s\n", astr_val_exp);

    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed\n");
        TEST_ERROR;
    }

    /* Create a VL string datatype  */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Aread(aid, atype, &astr_val) < 0) {
        printf("Cannot read the attribute.\n");
        TEST_ERROR;
    }

    dbgf(1, "read attr is= %s\n", astr_val);
    if (strcmp(astr_val, astr_val_exp) != 0) {
        printf("reader: the vl add_attribute verification failed,expected value is  %s\n", astr_val_exp);
        printf("reader: the vl add_attribute verification failed, the value is %s\n", astr_val);
        printf("The vl add_attribute verification failed\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose failed.\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        printf("H5Aclose failed.\n");
        TEST_ERROR;
    }

    H5free_memory(astr_val);
    free(astr_val_exp);

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->attr_test == true) {
        if (np_rd_send(s) == false)
            goto error2;
        dbgf(2, "reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;
    if (astr_val)
        H5free_memory(astr_val);
    if (astr_val_exp)
        free(astr_val_exp);

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, false);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_one_attr
 *
 * Purpose:     Verify if an attribute is successfully deleted.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              const char* aname
 *              The name of the attribute to be deleted.
 *
 *              hbool_t check_storage
 *              a flag to indicate if the storage check is on
 *
 *              hbool_t is_compact
 *              true if after this attribute is deleted,
 *              the storage should still be in compact, false
 *              if the storage should be in dense.
 *              Note: this parameter is not used if the check_storage
 *              is set to false.

 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is an internal function used by "remove-vlstr",
 *              "compact-del","dense-del",dense-del-to-compact"  tests.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_del_one_attr(state_t *s, hid_t g, const char *aname, hbool_t check_storage, hbool_t is_compact)
{

    htri_t attr_exists = false;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->attr_test) {
        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n", s->np_notify);
    }

    /* Check if the deleted attribute still exists. */
    attr_exists = H5Aexists_by_name(g, ".", aname, H5P_DEFAULT);
    if (attr_exists == false) {
        dbgf(1, "verify_del_attrs_compact() test: \n");
        dbgf(1, "  attribute %s is successfully deleted. \n", aname);
    }
    else if (attr_exists == true) {
        printf("The supposed deleted attribute %s still exists \n", aname);
        printf("verify_del_attrs_compact() test failed \n");
        TEST_ERROR;
    }
    else {
        printf("H5Aexists_by_name failed \n");
        TEST_ERROR;
    }

    if (!s->old_style_grp && check_storage == true) {
        if (false == check_attr_storage_type(g, is_compact)) {
            printf("The attribute storage type is wrong. \n");
            TEST_ERROR;
        }
        dbgf(2, "reader: finish checking the storage type: %d\n", s->np_notify);
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, true)) {
            printf("An object header continuation block should be removed. \n");
            printf("But it is NOT.\n");
            printf("Verification of an 'object header continuation block test' failed.\n");
            TEST_ERROR;
        }
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->attr_test == true) {
        if (np_rd_send(s) == false)
            TEST_ERROR;
        dbgf(2, "reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;
error:
    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, false);

error2:
    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    verify_remove_vlstr_attr
 *
 * Purpose:     Verify if an variable length string attribute is
 *              successfully deleted by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added
 *              by the writer. It is also used to construct
 *              the attribute name.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is for the "remove-vstr" test.
 *              Also note this function first verifies if
 *              a variable length attribute is added then
 *              it verifies if it is deleted successfully.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_remove_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{
    hbool_t     ret = false;
    char        attrname[VS_ATTR_NAME_LEN];
    const char *aname_format = "attr-%u";

    ret = verify_group_vlstr_attr(s, g, which, false);
    if (ret == true) {
        sprintf(attrname, aname_format, which);
        ret = verify_del_one_attr(s, g, attrname, false, false);
    }
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_modify_vlstr_attr
 *
 * Purpose:     Verify if an variable length string attribute is
 *              successfully modified by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added
 *              by the writer. It is also used to construct
 *              the attribute name.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is for the "modify-vstr" test.
 *              Also note this function first verifies if
 *              a variable length attribute is added then
 *              it verifies if it is modified successfully.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_modify_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t ret = false;

    ret = verify_group_vlstr_attr(s, g, which, false);
    if (ret == true)
        ret = verify_group_vlstr_attr(s, g, which, true);
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_attrs_compact
 *
 * Purpose:     Verify if attributes are successfully added for the compact
 *              storage.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added
 *              by the writer. It is also used to construct the
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "compact" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_attrs_compact(state_t *s, hid_t g, unsigned max_c, unsigned int which)
{

    unsigned    u;
    hbool_t     ret          = true;
    const char *aname_format = "attr-%u-%u";
    char        attrname[VS_ATTR_NAME_LEN];

    /* Need to verify the added attribute one by one. */
    for (u = 0; u < max_c; u++) {

        sprintf(attrname, aname_format, which, u);
        if (false == vrfy_attr(s, g, u + which, attrname, which, true, true)) {
            ret = false;
            break;
        }
    }
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_attrs_compact_dense
 *
 * Purpose:     Verify if attributes are successfully added first in the
 *              compact storage then in the dense storage.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "compact-dense" test.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_attrs_compact_dense(state_t *s, hid_t g, unsigned max_c, unsigned int which)
{

    const char *aname_format = "attr-d-%u-%u";
    char        attrname[VS_ATTR_NAME_LEN];

    hbool_t ret = verify_attrs_compact(s, g, max_c, which);

    if (ret == true) {

        /* Now the storage is in dense. Verify if the
         * retrieved value is correct. */
        sprintf(attrname, aname_format, max_c + which, 0);
        ret = vrfy_attr(s, g, which + max_c, attrname, which, true, false);
    }
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_attrs_compact
 *
 * Purpose:     Verify if an attribute in compact storage is successfully
 *              deleted.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "compact-del" test.
 *              Also note this function first verifies if
 *              attributes are successfully added in compact storage then
 *              it verifies if one added attribute is deleted  successfully.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_del_attrs_compact(state_t *s, hid_t g, unsigned max_c, unsigned int which)
{

    const char *aname_format = "attr-%u-%u";
    char        attrname[VS_ATTR_NAME_LEN];

    hbool_t ret = verify_attrs_compact(s, g, max_c, which);

    if (ret == true) {
        /* The writer only deletes the attribute attr-which-0 */
        sprintf(attrname, aname_format, which, 0);
        ret = verify_del_one_attr(s, g, attrname, true, true);
    }

    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_attrs_compact_dense
 *
 * Purpose:     Verify if an attribute in dense storage is successfully
 *              deleted.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "dense-del" test.
 *              Also note this function first verifies if
 *              attributes are successfully added in compact storage then
 *              in dense storage. Afterwards,
 *              it verifies if one added attribute is deleted successfully.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_del_attrs_compact_dense(state_t *s, hid_t g, unsigned max_c, unsigned int which)
{

    const char *aname_format = "attr-d-%u-%u";
    char        attrname[VS_ATTR_NAME_LEN];

    hbool_t ret = verify_attrs_compact_dense(s, g, max_c, which);

    if (ret == true) {
        /* The writer only deletes the attribute attr-d-which-0 */
        sprintf(attrname, aname_format, max_c + which, 0);
        ret = verify_del_one_attr(s, g, attrname, true, false);
    }

    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_attrs_compact_dense_compact
 *
 * Purpose:     verify that the attributes are deleted successfully
 *              even the attribute storage changes from dense to
 *              compact.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned min_d
 *              The minimal number of attributes to be stored in
 *              dense storage
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "dense-del-to-compact" test.
 *              Also note this function first verifies if
 *              attributes are successfully added in compact storage then
 *              in dense storage. Afterwards,
 *              it verifies if some added attributes are deleted successfully
 *              until the storage changes from dense to compact.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_del_attrs_compact_dense_compact(state_t *s, hid_t g, unsigned max_c, unsigned min_d,
                                       unsigned int which)
{

    unsigned    u;
    const char *aname_format = "attr-%u-%u";
    char        attrname[VS_ATTR_NAME_LEN];

    /* Verify the attributes are added correctly from
     * compact to dense storage*/
    hbool_t ret = verify_attrs_compact_dense(s, g, max_c, which);

    if (ret == true) {

        /* Then verify the deletion of attributes
         * from dense to compact.
         */
        u = max_c + 1;
        for (u--; u >= (min_d - 1); u--) {
            sprintf(attrname, aname_format, which, max_c - u);
            if (u == (min_d - 1))
                ret = verify_del_one_attr(s, g, attrname, true, true);
            else
                ret = verify_del_one_attr(s, g, attrname, true, false);
        }

        /* Just verify one more deleted attribute by the writer.
           The storage is still compact. */
        sprintf(attrname, aname_format, max_c + which, 0);
        ret = verify_del_one_attr(s, g, attrname, true, true);
    }

    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_ohr_block_attr
 *
 * Purpose:     Verify that an attribute is added to force creation of
 *              object header continuation block and remove this attribute
 *              to delete the object header continuation block

 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              group and attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_del_ohr_block_attr(state_t *s, hid_t g, unsigned int which)
{

    hbool_t     ret_value = false;
    char        attrname[VS_ATTR_NAME_LEN];
    const char *aname_format = "attr-%u";

    ret_value = verify_default_group_attr(s, g, which);
    if (ret_value == true) {
        sprintf(attrname, aname_format, which);
        ret_value = verify_del_one_attr(s, g, attrname, false, true);
    }
    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    verify_group_attribute
 *
 * Purpose:     Check the attribute test pattern and then call the
 *              corresponding verification function.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              group and attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_group_attribute(state_t *s, hid_t g, unsigned int which)
{
    char     test_pattern = s->at_pattern;
    hbool_t  ret          = false;
    unsigned max_compact  = 0;
    unsigned min_dense    = 0;
    hid_t    gcpl         = H5I_INVALID_HID;

    /* For tests "compact","compact-to-dense","compact-del",
     *           "dense-del", "dense-del-to-compact",
     *     the maximal number of attributes for the compact storage
     *     and the minimal number of attributes for the dense storage
     *     are needed. So obtain them here
     * When testing the old-style group creation case, only max_compact
     * matters. To reduce the testing time, we set max_compact to 2.*/
    switch (test_pattern) {
        case 'c':
        case 't':
        case 'C':
        case 'D':
        case 'T':
            if (s->old_style_grp)
                max_compact = 2;
            else {
                if ((gcpl = H5Gget_create_plist(g)) < 0) {
                    printf("H5Gget_create_plist failed\n");
                    TEST_ERROR;
                }
                if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
                    printf("H5Pget_attr_phase_change failed\n");
                    TEST_ERROR;
                }
                if (H5Pclose(gcpl) < 0) {
                    printf("H5Pclose failed\n");
                    TEST_ERROR;
                }
            }
            break;
        case 'v':
        case 'd':
        case 'M':
        case 'm':
        case 'r':
        case 'a':
        case 'R':
        case ' ':
        default:
            break;
    }

    /* Distribute the verification test. */
    switch (test_pattern) {
        case 'c':
            ret = verify_attrs_compact(s, g, max_compact, which);
            break;
        case 't':
            ret = verify_attrs_compact_dense(s, g, max_compact, which);
            break;
        case 'C':
            ret = verify_del_attrs_compact(s, g, max_compact, which);
            break;
        case 'D':
            ret = verify_del_attrs_compact_dense(s, g, max_compact, which);
            break;
        case 'T':
            ret = verify_del_attrs_compact_dense_compact(s, g, max_compact, min_dense, which);
            break;
        case 'M':
            ret = verify_modify_attr(s, g, which);
            break;
        case 'v':
            ret = verify_group_vlstr_attr(s, g, which, false);
            break;
        case 'r':
            ret = verify_remove_vlstr_attr(s, g, which);
            break;
        case 'm':
            ret = verify_modify_vlstr_attr(s, g, which);
            break;
        case 'R':
            ret = verify_del_ohr_block_attr(s, g, which);
            break;
        case 'a':
        case 'd':
        case ' ':
        default:
            ret = verify_default_group_attr(s, g, which);
            break;
    }

    return ret;

error:
    /* Still to finish the handshaking */
    if (s->use_communication && s->attr_test == true) {

        sock_rd_receive(s);
        sock_send_error(s);
    }
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_group
 *
 * Purpose:     verify the success of group creation and
 *              carry out the test for attribute operations(add,delete etc.)
 *              according to the attribute test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_group(state_t *s, unsigned int which)
{
    char       name[sizeof("/group-9999999999")];
    hid_t      g      = H5I_INVALID_HID;
    hbool_t    result = true;
    H5G_info_t group_info;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->attr_test) {

        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->sock.notify);
    }

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if ((g = H5Gopen2(s->file, name, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);
    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - Old-styled group: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the new-style.\n");
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, true)) {
            printf("An object header continuation block should NOT be created. \n");
            printf("But it is created.\n");
            printf("Verification of an 'object header continuation block test' failed.\n");
            TEST_ERROR;
        }
    }

    /* Reader sends an OK message back to the writer */
    if (s->use_communication && s->attr_test == true) {

        if (sock_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->sock.notify);
    }

    /* Check if we need to skip the attribute test for this group. */
    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(s, g, which);
    else
        result = true;

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    return result;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        sock_send_error(s);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    create_group_id
 *
 * Purpose:     Create a group and return the group ID.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name.
 *
 *              hbool_t dense_to_compact
 *              true if this function is used to test the transition from dense to
 *              compact, false if the test is from compact to dense.
 *
 * Return:      Success:    the group ID
 *              Failure:    -1
 *
 * Note:        Only used by testing the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hid_t
create_group_id(state_t *s, unsigned int which, hbool_t dense_to_compact)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g    = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    if (gcpl < 0) {
        printf("H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (dense_to_compact) {
        if (H5Pset_link_phase_change(gcpl, 2, 2) < 0) {
            printf("H5Pset_link_phase_change failed for dense to compact.\n");
            TEST_ERROR;
        }
    }
    else {
        if (H5Pset_link_phase_change(gcpl, 1, 1) < 0) {
            printf("H5Pset_attr_phase_change failed for compact to dense.\n");
            TEST_ERROR;
        }
    }

    esnprintf(name, sizeof(name), "/group-%u", which);
    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    /* The storage type should always be compact when a group is created. */
    if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
        printf("New-style group link storage test:. \n");
        printf("    still be compact after group creation. \n");
        TEST_ERROR;
    }

    if (H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    /* If a grp_op_test is turned on and named pipes are used,
     * the writer should send and receive messages after the group creation.
     * Writer sends a message to reader: a group is successfully created.
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "Writer: ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }

    return g;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    H5E_BEGIN_TRY
    {
        H5Pclose(gcpl);
    }
    H5E_END_TRY;

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    close_group_id
 *
 * Purpose:     Verify is a group is closed successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              The ID of the group to be closed.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hbool_t
close_group_id(state_t *s, hid_t g)
{

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* If a grp_op_test is turned on and named pipes are used, for
     * link storage test,
     * Writer sends a message to reader: the group is successfully closed.
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "Writer: ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    create_group
 *
 * Purpose:     Create a group
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
create_group(state_t *s, unsigned int which)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g = H5I_INVALID_HID;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);
    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Old-styled group test: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the new-style.\n");
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Writer sends a message to reader,
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "Writer: ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    delete_one_link
 *
 * Purpose:     Delete a link(either hard/soft) in group operation tests.
 *              according to the group test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              The HDF5 object ID that the deleted link is attached to.
 *
 *              const char *name
 *              The name of the link to be deleted.
 *
 *              short link_storage
 *              <=0: link storage is ignored.
 *              1: link storage should be compact after link deletion..
 *              >1: link storage should be dense after link deletion.
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by delete_groups() and delete_links() functions.
 *-------------------------------------------------------------------------
 */

static hbool_t
delete_one_link(state_t *s, hid_t obj_id, const char *name, short link_storage, unsigned int which)
{

    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    if (H5Ldelete(obj_id, name, H5P_DEFAULT) < 0) {
        printf("H5Ldelete failed\n");
        TEST_ERROR;
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support the indexed storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {

            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {

            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    /* Writer sends a message to reader:
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    verify_group_attribute
 *
 * Purpose:     Check the attribute test pattern and then call the
 *              corresponding verification function.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              group and attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_group_attribute(state_t *s, hid_t g, unsigned int which)
{
    char     test_pattern = s->at_pattern;
    hbool_t  ret          = false;
    unsigned max_compact  = 0;
    unsigned min_dense    = 0;
    hid_t    gcpl         = H5I_INVALID_HID;

    /* For tests "compact","compact-to-dense","compact-del",
     *           "dense-del", "dense-del-to-compact",
     *     the maximal number of attributes for the compact storage
     *     and the minimal number of attributes for the dense storage
     *     are needed. So obtain them here
     * When testing the old-style group creation case, only max_compact
     * matters. To reduce the testing time, we set max_compact to 2.*/
    switch (test_pattern) {
        case 'c':
        case 't':
        case 'C':
        case 'D':
        case 'T':
            if (s->old_style_grp)
                max_compact = 2;
            else {
                if ((gcpl = H5Gget_create_plist(g)) < 0) {
                    printf("H5Gget_create_plist failed\n");
                    TEST_ERROR;
                }
                if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
                    printf("H5Pget_attr_phase_change failed\n");
                    TEST_ERROR;
                }
                if (H5Pclose(gcpl) < 0) {
                    printf("H5Pclose failed\n");
                    TEST_ERROR;
                }
            }
            break;
        case 'v':
        case 'd':
        case 'M':
        case 'm':
        case 'r':
        case 'a':
        case 'R':
        case ' ':
        default:
            break;
    }

    /* Distribute the verification test. */
    switch (test_pattern) {
        case 'c':
            ret = verify_attrs_compact(s, g, max_compact, which);
            break;
        case 't':
            ret = verify_attrs_compact_dense(s, g, max_compact, which);
            break;
        case 'C':
            ret = verify_del_attrs_compact(s, g, max_compact, which);
            break;
        case 'D':
            ret = verify_del_attrs_compact_dense(s, g, max_compact, which);
            break;
        case 'T':
            ret = verify_del_attrs_compact_dense_compact(s, g, max_compact, min_dense, which);
            break;
        case 'M':
            ret = verify_modify_attr(s, g, which);
            break;
        case 'v':
            ret = verify_group_vlstr_attr(s, g, which, false);
            break;
        case 'r':
            ret = verify_remove_vlstr_attr(s, g, which);
            break;
        case 'm':
            ret = verify_modify_vlstr_attr(s, g, which);
            break;
        case 'R':
            ret = verify_del_ohr_block_attr(s, g, which);
            break;
        case 'a':
        case 'd':
        case ' ':
        default:
            ret = verify_default_group_attr(s, g, which);
            break;
    }

    return ret;

error:
    /* Still to finish the handshaking */
    if (s->use_communication && s->attr_test == true) {
        np_rd_receive(s);
        np_send_error(s, false);
    }
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_group
 *
 * Purpose:     verify the success of group creation and
 *              carry out the test for attribute operations(add,delete etc.)
 *              according to the attribute test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_group(state_t *s, unsigned int which)
{
    char       name[sizeof("/group-9999999999")];
    hid_t      g      = H5I_INVALID_HID;
    hbool_t    result = true;
    H5G_info_t group_info;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->attr_test) {

        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->np_notify);
    }

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if ((g = H5Gopen2(s->file, name, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);
    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - Old-styled group: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the new-style.\n");
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, true)) {
            printf("An object header continuation block should NOT be created. \n");
            printf("But it is created.\n");
            printf("Verification of an 'object header continuation block test' failed.\n");
            TEST_ERROR;
        }
    }

    /* Reader sends an OK message back to the writer */
    if (s->use_communication && s->attr_test == true) {

        if (np_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->np_notify);
    }

    /* Check if we need to skip the attribute test for this group. */
    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(s, g, which);
    else
        result = true;

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    return result;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->attr_test == true)
        np_send_error(s, false);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    create_group_id
 *
 * Purpose:     Create a group and return the group ID.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name.
 *
 *              hbool_t dense_to_compact
 *              true if this function is used to test the transition from dense to
 *              compact, false if the test is from compact to dense.
 *
 * Return:      Success:    the group ID
 *              Failure:    -1
 *
 * Note:        Only used by testing the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hid_t
create_group_id(state_t *s, unsigned int which, hbool_t dense_to_compact)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g    = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    if (gcpl < 0) {
        printf("H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (dense_to_compact) {
        if (H5Pset_link_phase_change(gcpl, 2, 2) < 0) {
            printf("H5Pset_link_phase_change failed for dense to compact.\n");
            TEST_ERROR;
        }
    }
    else {
        if (H5Pset_link_phase_change(gcpl, 1, 1) < 0) {
            printf("H5Pset_attr_phase_change failed for compact to dense.\n");
            TEST_ERROR;
        }
    }

    esnprintf(name, sizeof(name), "/group-%u", which);
    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    /* The storage type should always be compact when a group is created. */
    if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
        printf("New-style group link storage test:. \n");
        printf("    still be compact after group creation. \n");
        TEST_ERROR;
    }

    if (H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    /* If a grp_op_test is turned on and named pipes are used,
     * the writer should send and receive messages after the group creation.
     * Writer sends a message to reader: a group is successfully created.
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "Writer: ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }

    return g;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, true);

error2:

    H5E_BEGIN_TRY
    {
        H5Pclose(gcpl);
    }
    H5E_END_TRY;

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    close_group_id
 *
 * Purpose:     Verify is a group is closed successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              The ID of the group to be closed.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hbool_t
close_group_id(state_t *s, hid_t g)
{

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* If a grp_op_test is turned on and named pipes are used, for
     * link storage test,
     * Writer sends a message to reader: the group is successfully closed.
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "Writer: ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, true);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    create_group
 *
 * Purpose:     Create a group
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
create_group(state_t *s, unsigned int which)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g = H5I_INVALID_HID;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);
    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Old-styled group test: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the new-style.\n");
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Writer sends a message to reader,
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "Writer: ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, true);

error2:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    delete_one_link
 *
 * Purpose:     Delete a link(either hard/soft) in group operation tests.
 *              according to the group test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              The HDF5 object ID that the deleted link is attached to.
 *
 *              const char *name
 *              The name of the link to be deleted.
 *
 *              short link_storage
 *              <=0: link storage is ignored.
 *              1: link storage should be compact after link deletion..
 *              >1: link storage should be dense after link deletion.
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by delete_groups() and delete_links() functions.
 *-------------------------------------------------------------------------
 */

static hbool_t
delete_one_link(state_t *s, hid_t obj_id, const char *name, short link_storage, unsigned int which)
{

    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    if (H5Ldelete(obj_id, name, H5P_DEFAULT) < 0) {
        printf("H5Ldelete failed\n");
        TEST_ERROR;
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support the indexed storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {

            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {

            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    /* Writer sends a message to reader:
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, true);

error2:

    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    delete_group
 *
 * Purpose:     Delete a group and carry out group operations(add,delete etc.)
 *              according to the group operation test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
delete_group(state_t *s, unsigned int which)
{

    char    name[sizeof("/group-9999999999")];
    hbool_t ret_value = create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        ret_value = delete_one_link(s, s->file, name, 0, which);
    }

    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    move_one_group
 *
 * Purpose:     A helper function used by the move_group operation.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this group is attached to
 *
 *              const char *name
 *              The original group name
 *
 *              const char *newname
 *              The new group name
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the move_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
move_one_group(state_t *s, hid_t obj_id, const char *name, const char *newname, unsigned int which)
{

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    if (H5Lmove(obj_id, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        printf("H5Ldelete failed\n");
        TEST_ERROR;
    }

    /* Writer sends a message to reader:
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    move_one_group
 *
 * Purpose:     A helper function used by the move_group operation.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this group is attached to
 *
 *              const char *name
 *              The original group name
 *
 *              const char *newname
 *              The new group name
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the move_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
move_one_group(state_t *s, hid_t obj_id, const char *name, const char *newname, unsigned int which)
{

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    if (H5Lmove(obj_id, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        printf("H5Ldelete failed\n");
        TEST_ERROR;
    }

    /* Writer sends a message to reader:
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, true);

error2:

    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    move_group
 *
 * Purpose:     Move a group to another group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
move_group(state_t *s, unsigned int which)
{

    char    name[sizeof("/group-9999999999")];
    char    new_name[sizeof("/new-group-9999999999")];
    hbool_t ret_value = create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        esnprintf(new_name, sizeof(new_name), "/new-group-%u", which);
        ret_value = move_one_group(s, s->file, name, new_name, which);
    }

    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    insert_one_link
 *
 * Purpose:     A helper function used to attach a link to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this link is attached to
 *
 *              const char *name
 *              The name of the target object used by creating links
 *
 *              const char *newname
 *              The name of the linked objects
 *
 *              hbool_t is_hard
 *              true if inserting a hard link
 *              false if inserting a soft link
 *
 *              short link_storage
 *              <=0: link storage is ignored.
 *              1: link storage should be compact.
 *              >1: link storage should be dense.

 *              unsigned int which
 *              The number of iterations for group creation
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the insert_links and link storage transit functions.
 *              For link storage, we test at both the writer and the reader.
 *-------------------------------------------------------------------------
*/

static hbool_t
insert_one_link(state_t *s, hid_t obj_id, const char *name, const char *newname, hbool_t is_hard,
                short link_storage, unsigned int which)
{

    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    /* For storage transit and insert_links cases, we
     * create links in different style, just add a little
     * variation of the tests.*/
    if (is_hard) {
        if (link_storage > 0) {
            if (H5Lcreate_hard(s->file, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_hard failed\n");
                TEST_ERROR;
            }
        }
        else {
            if (H5Lcreate_hard(obj_id, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_hard failed\n");
                TEST_ERROR;
            }
        }
    }
    else {
        if (link_storage > 0) {
            if (H5Lcreate_soft("/", obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_soft failed\n");
                TEST_ERROR;
            }
        }
        else {
            if (H5Lcreate_soft(name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_soft failed.\n");
                TEST_ERROR;
            }
        }
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support dense or compact storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {
            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {
            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    /* Writer sends a message to reader,
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->sock.notify + 1);
        if (sock_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    insert_one_link
 *
 * Purpose:     A helper function used to attach a link to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this link is attached to
 *
 *              const char *name
 *              The name of the target object used by creating links
 *
 *              const char *newname
 *              The name of the linked objects
 *
 *              hbool_t is_hard
 *              true if inserting a hard link
 *              false if inserting a soft link
 *
 *              short link_storage
 *              <=0: link storage is ignored.
 *              1: link storage should be compact.
 *              >1: link storage should be dense.

 *              unsigned int which
 *              The number of iterations for group creation
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the insert_links and link storage transit functions.
 *              For link storage, we test at both the writer and the reader.
 *-------------------------------------------------------------------------
*/

static hbool_t
insert_one_link(state_t *s, hid_t obj_id, const char *name, const char *newname, hbool_t is_hard,
                short link_storage, unsigned int which)
{

    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    /* For storage transit and insert_links cases, we
     * create links in different style, just add a little
     * variation of the tests.*/
    if (is_hard) {
        if (link_storage > 0) {
            if (H5Lcreate_hard(s->file, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_hard failed\n");
                TEST_ERROR;
            }
        }
        else {
            if (H5Lcreate_hard(obj_id, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_hard failed\n");
                TEST_ERROR;
            }
        }
    }
    else {
        if (link_storage > 0) {
            if (H5Lcreate_soft("/", obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_soft failed\n");
                TEST_ERROR;
            }
        }
        else {
            if (H5Lcreate_soft(name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_soft failed.\n");
                TEST_ERROR;
            }
        }
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support dense or compact storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {
            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {
            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    /* Writer sends a message to reader,
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_communication && s->grp_op_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->np_notify + 1);
        if (np_wr_send_receive(s) == false) {
            H5_FAILED();
            AT();
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, true);

error2:

    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    insert_links
 *
 * Purpose:     create links with a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
insert_links(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("/st-group-9999999999")];

    hbool_t ret_value = create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        esnprintf(hd_name, sizeof(hd_name), "/hd-group-%u", which);
        esnprintf(st_name, sizeof(st_name), "/st-group-%u", which);
        ret_value = insert_one_link(s, s->file, name, hd_name, true, 0, which);
        if (ret_value == true)
            ret_value = insert_one_link(s, s->file, name, st_name, false, 0, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    delete_links
 *
 * Purpose:     create links with a group and then delete them successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
delete_links(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("/st-group-9999999999")];

    hbool_t ret_value = insert_links(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        esnprintf(hd_name, sizeof(hd_name), "/hd-group-%u", which);
        esnprintf(st_name, sizeof(st_name), "/st-group-%u", which);
        ret_value = delete_one_link(s, s->file, hd_name, 0, which);
        if (ret_value == true)
            ret_value = delete_one_link(s, s->file, st_name, 0, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    transit_storage_compact_to_dense
 *
 * Purpose:     Add links so that the link storage transits from
 *              compact to dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
transit_storage_compact_to_dense(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("/st-group-9999999999")];

    hid_t g = create_group_id(s, which, false);
    if (g < 0) {
        printf("create_group_id failed\n");
        TEST_ERROR;
    }

    /* First insert a hard link, compact storage. */
    esnprintf(name, sizeof(name), "/group-%u", which);
    esnprintf(hd_name, sizeof(hd_name), "hd-group-%u", which);
    if (insert_one_link(s, g, name, hd_name, true, 1, which) == false) {
        printf("insert_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    /* Then insert a soft link, the storage becomes dense. */
    esnprintf(st_name, sizeof(st_name), "st-group-%u", which);
    if (insert_one_link(s, g, name, st_name, false, 2, which) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    if (close_group_id(s, g) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    transit_storage_dense_to_compact
 *
 * Purpose:     Add or delete links so that the link storage transits from
 *              compact to dense then to compact.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
transit_storage_dense_to_compact(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("st-group-9999999999")];
    char st2_name[sizeof("st2-group-9999999999")];

    hid_t g = create_group_id(s, which, true);
    if (g < 0) {
        printf("create_group_id failed\n");
        TEST_ERROR;
    }

    /* Insert a link, storage is compact. */
    esnprintf(name, sizeof(name), "/group-%u", which);
    esnprintf(hd_name, sizeof(hd_name), "hd-group-%u", which);
    if (insert_one_link(s, g, name, hd_name, true, 1, which) == false) {
        printf("insert_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    /* Insert a link, storage is still compact. */
    esnprintf(st_name, sizeof(st_name), "st-group-%u", which);
    if (insert_one_link(s, g, name, st_name, false, 1, which) == false) {
        printf("insert_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    /* Insert a link, storage becomes dense. */
    esnprintf(st2_name, sizeof(st2_name), "st2-group-%u", which);
    if (insert_one_link(s, g, name, st2_name, false, 2, which) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    /* Delete a link, storage is still dense */
    if (delete_one_link(s, g, st_name, 2, which) == false) {
        printf("delete_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    /* Delete another link, storage becomes compact */
    if (delete_one_link(s, g, st2_name, 1, which) == false) {
        printf("delete_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    if (close_group_id(s, g) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    group_operations
 *
 * Purpose:     Carry out group and attribute operations(add,delete etc.)
 *              according to the group operation and attribute test patterns.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function. The check of attribute
 *              operations is inside the write_group() function.
 *-------------------------------------------------------------------------
 */
static hbool_t
group_operations(state_t *s, unsigned int which)
{

    hbool_t ret_value    = false;
    char    test_pattern = s->grp_op_pattern;

    switch (test_pattern) {
        case 'c':
            ret_value = create_group(s, which);
            break;
        case 'd':
            ret_value = delete_group(s, which);
            break;
        case 'm':
            ret_value = move_group(s, which);
            break;
        case 'i':
            ret_value = insert_links(s, which);
            break;
        case 'D':
            ret_value = delete_links(s, which);
            break;
        case 't':
            ret_value = transit_storage_compact_to_dense(s, which);
            break;
        case 'T':
            ret_value = transit_storage_dense_to_compact(s, which);
            break;
        case ' ':
        default:
            ret_value = write_group(s, which);
            break;
    }
    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    vrfy_create_group
 *
 * Purpose:     Verify if a group can be created successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_create_group(state_t *s, unsigned int which)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g = H5I_INVALID_HID;
    H5G_info_t group_info;

    dbgf(2, "reader: ready to send the message: \n");

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->sock.notify);
    }

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if ((g = H5Gopen2(s->file, name, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);
    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - Old-styled group: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the new-style.\n");
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Reader sends an OK message back to the writer */
    if (s->use_communication && s->grp_op_test == true) {

        if (sock_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->sock.notify);
    }

    return true;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_create_group_id
 *
 * Purpose:     Verify if a group is created successfully and return the group
 *              ID.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 *              hbool_t dense_to_compact
 *              true if this function is used to test the transition from dense to
 *              compact, false if the test is from compact to dense.
 *
 * Return:      Success:    the group ID
 *              Failure:    -1
 *
 * Note:        This function is used by the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hid_t
vrfy_create_group_id(state_t *s, unsigned int which, hbool_t dense_to_compact)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g    = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t group_info;
    unsigned   max_compact = 0;
    unsigned   min_dense   = 0;

    dbgf(2, "reader: ready to receive a message: \n");
    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->sock.notify);
    }

    if (which >= s->nsteps) {
        printf("Number of the created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if ((g = H5Gopen2(s->file, name, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if ((gcpl = H5Gget_create_plist(g)) < 0) {
        printf("H5Gget_create_plist failed\n");
        TEST_ERROR;
    }

    if (H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_link_phase_change failed\n");
        TEST_ERROR;
    }

    if (dense_to_compact) {
        if (max_compact != 2) {
            printf("For storage check from dense to compact:\n");
            printf("    The max_compact should be 2.\n");
            printf("    But the actual value is %d.\n", max_compact);
            TEST_ERROR;
        }
        else if (min_dense != 2) {
            printf("For storage check from dense to compact:\n");
            printf("    The min_dense should be 2.\n");
            printf("    But the actual value is %d.\n", min_dense);
            TEST_ERROR;
        }
    }
    else {
        if (max_compact != 1) {
            printf("For storage check from dense to compact:\n");
            printf("    The max_compact should be 1.\n");
            printf("    But the actual value is %d.\n", max_compact);
            TEST_ERROR;
        }
        else if (min_dense != 1) {
            printf("For storage check from dense to compact:\n");
            printf("    The min_dense should be 1.\n");
            printf("    But the actual value is %d.\n", min_dense);
            TEST_ERROR;
        }
    }

    if (H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    /* When the group is created, the storage type is always compact. */
    if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
        printf("Old-styled group test: but the group is not in old-style. \n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {

        if (sock_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->sock.notify);
    }

    return g;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
        H5Pclose(gcpl);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_close_group_id
 *
 * Purpose:     Verify if a group is closed successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              The ID of the group to be closed.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_close_group_id(state_t *s, hid_t g)
{

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->sock.notify);
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {

        if (sock_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->sock.notify);
    }

    return true;

error:

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_one_link_exist
 *
 * Purpose:     Verify if a link exists or not. The link storage is
 *              also checked.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              The ID of the object the link is attached to
 *
 *              hbool_t expect_exist
 *              A flag that indicates if this link is expected to exist
 *
 *              short link_storage
 *              <=0: link storage check is ignored.
 *              1: link storage is expected to be compact.
 *              >1: link storage is expected to be dense.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        Helper function to check if links are inserted or deleted.
 *              The link storage is also checked.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_one_link_exist(state_t *s, hid_t obj_id, const char *name, hbool_t expect_exist, short link_storage)
{

    int        link_exists = 0;
    H5G_info_t group_info;

    dbgf(2, "reader: ready to send the message: \n");
    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->sock.notify);
    }

    link_exists = H5Lexists(obj_id, name, H5P_DEFAULT);

    if (link_exists < 0) {
        printf("H5Lexists error\n");
        TEST_ERROR;
    }
    else if (link_exists == 1) {
        if (expect_exist == false) {
            printf("This link should be moved or deleted but it still exists.\n");
            TEST_ERROR;
        }
    }
    else if (link_exists == 0) {
        if (expect_exist == true) {
            printf("This link should exist but it is moved or deleted.\n");
            TEST_ERROR;
        }
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support the indexed storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {
            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {
            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {
        if (sock_rd_send(s) == false) {
            TEST_ERROR;
        }
        dbgf(1, "Reader: finish sending back the message: %d\n", s->sock.notify);
    }

    return true;

error:

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    vrfy_create_group
 *
 * Purpose:     Verify if a group can be created successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_create_group(state_t *s, unsigned int which)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g = H5I_INVALID_HID;
    H5G_info_t group_info;

    dbgf(2, "reader: ready to send the message: \n");

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->np_notify);
    }

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if ((g = H5Gopen2(s->file, name, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);
    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - Old-styled group: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the new-style.\n");
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Reader sends an OK message back to the writer */
    if (s->use_communication && s->grp_op_test == true) {

        if (np_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, false);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_create_group_id
 *
 * Purpose:     Verify if a group is created successfully and return the group
 *              ID.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 *              hbool_t dense_to_compact
 *              true if this function is used to test the transition from dense to
 *              compact, false if the test is from compact to dense.
 *
 * Return:      Success:    the group ID
 *              Failure:    -1
 *
 * Note:        This function is used by the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hid_t
vrfy_create_group_id(state_t *s, unsigned int which, hbool_t dense_to_compact)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g    = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t group_info;
    unsigned   max_compact = 0;
    unsigned   min_dense   = 0;

    dbgf(2, "reader: ready to receive a message: \n");
    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->np_notify);
    }

    if (which >= s->nsteps) {
        printf("Number of the created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if ((g = H5Gopen2(s->file, name, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if ((gcpl = H5Gget_create_plist(g)) < 0) {
        printf("H5Gget_create_plist failed\n");
        TEST_ERROR;
    }

    if (H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_link_phase_change failed\n");
        TEST_ERROR;
    }

    if (dense_to_compact) {
        if (max_compact != 2) {
            printf("For storage check from dense to compact:\n");
            printf("    The max_compact should be 2.\n");
            printf("    But the actual value is %d.\n", max_compact);
            TEST_ERROR;
        }
        else if (min_dense != 2) {
            printf("For storage check from dense to compact:\n");
            printf("    The min_dense should be 2.\n");
            printf("    But the actual value is %d.\n", min_dense);
            TEST_ERROR;
        }
    }
    else {
        if (max_compact != 1) {
            printf("For storage check from dense to compact:\n");
            printf("    The max_compact should be 1.\n");
            printf("    But the actual value is %d.\n", max_compact);
            TEST_ERROR;
        }
        else if (min_dense != 1) {
            printf("For storage check from dense to compact:\n");
            printf("    The min_dense should be 1.\n");
            printf("    But the actual value is %d.\n", min_dense);
            TEST_ERROR;
        }
    }

    if (H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    /* When the group is created, the storage type is always compact. */
    if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
        printf("Old-styled group test: but the group is not in old-style. \n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {

        if (np_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->np_notify);
    }

    return g;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
        H5Pclose(gcpl);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, false);

error2:

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_close_group_id
 *
 * Purpose:     Verify if a group is closed successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              The ID of the group to be closed.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_close_group_id(state_t *s, hid_t g)
{

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->np_notify);
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {

        if (np_rd_send(s) == false)
            TEST_ERROR;
        dbgf(1, "Reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;

error:

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, false);

error2:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_one_link_exist
 *
 * Purpose:     Verify if a link exists or not. The link storage is
 *              also checked.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              The ID of the object the link is attached to
 *
 *              hbool_t expect_exist
 *              A flag that indicates if this link is expected to exist
 *
 *              short link_storage
 *              <=0: link storage check is ignored.
 *              1: link storage is expected to be compact.
 *              >1: link storage is expected to be dense.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        Helper function to check if links are inserted or deleted.
 *              The link storage is also checked.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_one_link_exist(state_t *s, hid_t obj_id, const char *name, hbool_t expect_exist, short link_storage)
{

    int        link_exists = 0;
    H5G_info_t group_info;

    dbgf(2, "reader: ready to send the message: \n");
    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->np_notify);
    }

    link_exists = H5Lexists(obj_id, name, H5P_DEFAULT);

    if (link_exists < 0) {
        printf("H5Lexists error\n");
        TEST_ERROR;
    }
    else if (link_exists == 1) {
        if (expect_exist == false) {
            printf("This link should be moved or deleted but it still exists.\n");
            TEST_ERROR;
        }
    }
    else if (link_exists == 0) {
        if (expect_exist == true) {
            printf("This link should exist but it is moved or deleted.\n");
            TEST_ERROR;
        }
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support the indexed storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {
            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {
            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {
        if (np_rd_send(s) == false) {
            TEST_ERROR;
        }
        dbgf(1, "Reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;

error:

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, false);

error2:

    return false;
}
#endif /* H5_USE_SOCKETS */

/*-------------------------------------------------------------------------
 * Function:    vrfy_delete_group
 *
 * Purpose:     Verify if a group can be deleted successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_delete_group(state_t *s, unsigned int which)
{

    hbool_t ret_value = false;
    char    name[sizeof("/group-9999999999")];

    ret_value = vrfy_create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        ret_value = vrfy_one_link_exist(s, s->file, name, false, 0);
    }

    return ret_value;
}

#ifdef H5_USE_SOCKETS
/*-------------------------------------------------------------------------
 * Function:    vrfy_move_one_group
 *
 * Purpose:     A helper function to verify the move_group operation successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this group is attached to
 *
 *              const char *name
 *              The original group name
 *
 *              const char *newname
 *              The new group name
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_move_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_move_one_group(state_t *s, hid_t obj_id, const char *name, const char *newname, unsigned int which)
{

    hid_t      g = H5I_INVALID_HID;
    H5G_info_t group_info;
    int        link_exists = 0;

    dbgf(2, "reader: ready to send the message: \n");
    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == sock_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->sock.notify);
    }

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    link_exists = H5Lexists(obj_id, name, H5P_DEFAULT);
    if (link_exists < 0) {
        printf("H5Lexists error\n");
        TEST_ERROR;
    }
    else if (link_exists == 1) {
        printf("This link should be moved but it still exists.\n");
        TEST_ERROR;
    }

    if ((g = H5Gopen2(obj_id, newname, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);
    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - Old-styled group: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the new-style.\n");
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {

        if (sock_rd_send(s) == false) {
            TEST_ERROR;
        }
        dbgf(1, "Reader: finish sending back the message: %d\n", s->sock.notify);
    }

    return true;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        sock_send_error(s);

error2:

    return false;
}
#else  /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    vrfy_move_one_group
 *
 * Purpose:     A helper function to verify the move_group operation successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this group is attached to
 *
 *              const char *name
 *              The original group name
 *
 *              const char *newname
 *              The new group name
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_move_group() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_move_one_group(state_t *s, hid_t obj_id, const char *name, const char *newname, unsigned int which)
{

    hid_t      g = H5I_INVALID_HID;
    H5G_info_t group_info;
    int        link_exists = 0;

    dbgf(2, "reader: ready to send the message: \n");
    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message
     * is an error message.
     */
    if (s->use_communication && true == s->grp_op_test) {

        if (false == np_rd_receive(s)) {
            H5_FAILED();
            AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n", s->np_notify);
    }

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    link_exists = H5Lexists(obj_id, name, H5P_DEFAULT);
    if (link_exists < 0) {
        printf("H5Lexists error\n");
        TEST_ERROR;
    }
    else if (link_exists == 1) {
        printf("This link should be moved but it still exists.\n");
        TEST_ERROR;
    }

    if ((g = H5Gopen2(obj_id, newname, H5P_DEFAULT)) < 0) {
        printf("H5Gopen2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    dbgf(2, "Storage info is %d\n", group_info.storage_type);
    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - Old-styled group: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Reader - The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Reader: verify that the group is created with the new-style.\n");
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    /* Reader sends an OK message back to the reader */
    if (s->use_communication && s->grp_op_test == true) {

        if (np_rd_send(s) == false) {
            TEST_ERROR;
        }
        dbgf(1, "Reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if (s->use_communication && s->grp_op_test == true)
        np_send_error(s, false);

error2:

    return false;
}
#endif /* H5_USE_SOCKETS */
/*-------------------------------------------------------------------------
 * Function:    vrfy_move_group
 *
 * Purpose:     Verify if a group can be moved successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_move_group(state_t *s, unsigned int which)
{

    char    name[sizeof("/group-9999999999")];
    char    new_name[sizeof("/new-group-9999999999")];
    hbool_t ret_value = vrfy_create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        esnprintf(new_name, sizeof(new_name), "/new-group-%u", which);
        ret_value = vrfy_move_one_group(s, s->file, name, new_name, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_insert_links
 *
 * Purpose:     Verify if the links are inserted successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_insert_links(state_t *s, unsigned int which)
{

    hbool_t ret_value = false;
    char    hd_name[sizeof("/hd-group-9999999999")];
    char    st_name[sizeof("/st-group-9999999999")];

    ret_value = vrfy_create_group(s, which);

    if (ret_value == true) {
        esnprintf(hd_name, sizeof(hd_name), "/hd-group-%u", which);
        esnprintf(st_name, sizeof(st_name), "/st-group-%u", which);
        ret_value = vrfy_one_link_exist(s, s->file, hd_name, true, 0);
        if (ret_value == true)
            ret_value = vrfy_one_link_exist(s, s->file, st_name, true, 0);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_delete_links
 *
 * Purpose:     Verify if the links created with a group are deleted successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_delete_links(state_t *s, unsigned int which)
{

    hbool_t ret_value = false;
    char    hd_name[sizeof("/hd-group-9999999999")];
    char    st_name[sizeof("/st-group-9999999999")];

    /* First verify if the links are inserted correctly */
    ret_value = vrfy_insert_links(s, which);

    /* Then if these links are deleted correctly. */
    if (ret_value == true) {
        esnprintf(hd_name, sizeof(hd_name), "/hd-group-%u", which);
        esnprintf(st_name, sizeof(st_name), "/st-group-%u", which);
        ret_value = vrfy_one_link_exist(s, s->file, hd_name, false, 0);
        if (ret_value == true)
            ret_value = vrfy_one_link_exist(s, s->file, st_name, false, 0);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_transit_storage_compact_to_dense
 *
 * Purpose:     Verify if the link storage successfully transits from
 *              compact to dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_transit_storage_compact_to_dense(state_t *s, unsigned int which)
{

    hid_t g = H5I_INVALID_HID;
    char  hd_name[sizeof("hd-group-9999999999")];
    char  st_name[sizeof("st-group-9999999999")];

    g = vrfy_create_group_id(s, which, false);
    if (g < 0) {
        printf("verify create_group_id failed\n");
        TEST_ERROR;
    }

    esnprintf(hd_name, sizeof(hd_name), "hd-group-%u", which);
    if (vrfy_one_link_exist(s, g, hd_name, true, 1) == false) {
        printf("verify the compact storage failed for 'link compact to dense' test\n");
        TEST_ERROR;
    }

    esnprintf(st_name, sizeof(st_name), "st-group-%u", which);
    if (vrfy_one_link_exist(s, g, st_name, true, 2) == false) {
        printf("verify the dense link storage failed for 'link compact to dense' test\n");
        TEST_ERROR;
    }

    if (vrfy_close_group_id(s, g) == false) {
        printf("verify the group close for 'link compact to dense' test\n");
        TEST_ERROR;
    }
    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    vrfy_transit_storage_dense_to_compact
 *
 * Purpose:     Verify if the link storage successfully transits from
 *              compact to dense then to compact.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group_operations() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
vrfy_transit_storage_dense_to_compact(state_t *s, unsigned int which)
{

    hid_t g = H5I_INVALID_HID;
    char  hd_name[sizeof("hd-group-9999999999")];
    char  st_name[sizeof("st-group-9999999999")];
    char  st2_name[sizeof("st2-group-9999999999")];

    g = vrfy_create_group_id(s, which, true);
    if (g < 0) {
        printf("verify create_group_id failed\n");
        TEST_ERROR;
    }

    /* Add a link, verify it is still the compact storage */
    esnprintf(hd_name, sizeof(hd_name), "hd-group-%u", which);
    if (vrfy_one_link_exist(s, g, hd_name, true, 1) == false) {
        printf("verify the compact storage failed for 'link compact to dense' test\n");
        TEST_ERROR;
    }

    /* Add another link, verify it is still the compact storage */
    esnprintf(st_name, sizeof(st_name), "st-group-%u", which);
    if (vrfy_one_link_exist(s, g, st_name, true, 1) == false) {
        printf("verify the compact link storage failed for 'link compact to dense' test\n");
        TEST_ERROR;
    }

    /* Add the third link, verify it becomes the dense storage */
    esnprintf(st2_name, sizeof(st2_name), "st2-group-%u", which);
    if (vrfy_one_link_exist(s, g, st2_name, true, 2) == false) {
        printf("verify the dense link storage failed for 'link compact to dense' test\n");
        TEST_ERROR;
    }

    /* Remove a link, verify the link doesn't exist and still dense storage */
    if (vrfy_one_link_exist(s, g, st_name, false, 2) == false) {
        printf("verify the dense link storage failed for 'link compact to dense' test\n");
        TEST_ERROR;
    }

    /* Remove a link, verify the link doesn't exist and it becomes compact storage */
    if (vrfy_one_link_exist(s, g, st2_name, false, 1) == false) {
        printf("verify the compact link storage failed for 'link compact to dense' test\n");
        TEST_ERROR;
    }

    if (vrfy_close_group_id(s, g) == false) {
        printf("verify the group close for 'link compact to dense' test\n");
        TEST_ERROR;
    }
    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_group_operations
 *
 * Purpose:     verify the success of group creation and
 *              carry out the test for attribute operations(add,delete etc.)
 *              according to the attribute test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static hbool_t
verify_group_operations(state_t *s, unsigned int which)
{
    hbool_t ret_value    = false;
    char    test_pattern = s->grp_op_pattern;

    switch (test_pattern) {
        case 'c':
            ret_value = vrfy_create_group(s, which);
            break;
        case 'd':
            ret_value = vrfy_delete_group(s, which);
            break;
        case 'm':
            ret_value = vrfy_move_group(s, which);
            break;
        case 'i':
            ret_value = vrfy_insert_links(s, which);
            break;
        case 'D':
            ret_value = vrfy_delete_links(s, which);
            break;
        case 't':
            ret_value = vrfy_transit_storage_compact_to_dense(s, which);
            break;
        case 'T':
            ret_value = vrfy_transit_storage_dense_to_compact(s, which);
            break;
        case ' ':
        default:
            ret_value = verify_group(s, which);
            break;
    }
    return ret_value;
}

#ifdef H5_USE_SOCKETS
int
main(int argc, char **argv)
{
    hid_t                  fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID;
    unsigned               step;
    hbool_t                writer = false;
    state_t               *s      = NULL;
    const char            *personality;
    H5F_vfd_swmr_config_t *config = NULL;
    hbool_t                wg_ret = false;
    hbool_t                vg_ret = false;

    if (NULL == (s = calloc(1, sizeof(state_t))))
        TEST_ERROR;
    if (NULL == (config = calloc(1, sizeof(H5F_vfd_swmr_config_t))))
        TEST_ERROR;

    if (!state_init(s, argc, argv)) {
        printf("state_init failed\n");
        TEST_ERROR;
    }

    personality = strstr(s->progname, "vfd_swmr_group_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_group_reader") == 0)
        writer = false;
    else {
        printf("unknown personality, expected vfd_swmr_group_{reader,writer}\n");
        TEST_ERROR;
    }

    /* config, tick_len, max_lag, presume_posix_semantics, writer,
     * maintain_metadata_file, generate_updater_files, flush_raw_data, md_pages_reserved,
     * md_file_path, md_file_name, updater_file_path */
    init_vfd_swmr_config(config, s->tick_len, s->max_lag, false, writer, true, false, true, 128, "./",
                         "group-shadow", NULL);

    /* If old-style option is chosen, use the earliest file format(H5F_LIBVER_EARLIEST)
     * as the second parameter of H5Pset_libver_bound() that is called by
     * vfd_swmr_create_fapl. Otherwise, the latest file format(H5F_LIBVER_LATEST)
     * should be used as the second parameter of H5Pset_libver_bound().
     * Also pass the use_vfd_swmr, only_meta_page, page_buf_size, config to vfd_swmr_create_fapl().*/
    if ((fapl = vfd_swmr_create_fapl(!s->old_style_grp, s->use_vfd_swmr, true, s->pbs, config)) < 0) {
        printf("vfd_swmr_create_fapl failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, s->ps)) < 0) {
        printf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    if (writer)
        s->file = H5Fcreate(s->filename, H5F_ACC_TRUNC, fcpl, fapl);
    else {
        /* The writer and reader are launched concurrently with no ordering
         * guarantee (these scenarios have no WRITER_MESSAGE gating), so the
         * reader can reach this open before the writer has created the file.
         * Retry the open until the file appears rather than failing
         * immediately: a reader that died here would leave the writer blocked
         * forever waiting for it in the handshake below -- the root cause of
         * an observed multi-hour hang in the os_groups_* scenarios.
         */
        unsigned       open_ntries;
        const unsigned max_open_ntries = 300; /* ~30s at 0.1s each */

        for (open_ntries = 0; open_ntries < max_open_ntries; open_ntries++) {
            H5E_BEGIN_TRY
            {
                s->file = H5Fopen(s->filename, H5F_ACC_RDONLY, fapl);
            }
            H5E_END_TRY;

            if (s->file >= 0)
                break;

            decisleep(1); /* 0.1s */
        }
    }

    if (s->file < 0) {
        printf("H5Fcreate/open failed\n");
        TEST_ERROR;
    }

    /* Must be added between file creation/opening and socket connections */
    if (H5Fvfd_swmr_end_tick(s->file) < 0) {
        printf("H5Fvfd_swmr_end_tick failed\n");
        TEST_ERROR;
    }

    /* Establish socket connection */
    if (s->use_communication && !socket_connect(&s->sock, writer)) {
        printf("socket_connect failed\n");
        TEST_ERROR;
    }

    /* For attribute test, force the socket to communicate in every step.
     * This will avoid the fake verification error from the reader when using the socket.
     * If the socket is not forced to communicate in every step, the reader may go ahead
     * to verify the group and the attribute operations before the writer has a chance to
     * carry out the corresponding operations. */
    if (s->attr_test && s->use_communication)
        s->csteps = 1;

    /* For group operation test, force the socket to communicate in every step. */
    if (s->grp_op_test && s->use_communication)
        s->csteps = 1;

    if (writer) {
        for (step = 0; step < s->nsteps; step++) {
            dbgf(2, "writer: step %d\n", step);

            wg_ret = group_operations(s, step);

            if (wg_ret == false) {

                /* At communication interval, notifies the reader about the failure and quit */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0)
                    sock_send_error(s);
                printf("write_group failed at step %d\n", step);
                TEST_ERROR;
            }
            else {

                /* At communication interval, notifies the reader and waits for its response */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0) {

                    if (sock_wr_send_receive(s) == false) {
                        printf("writer: write group - verification failed.\n");
                        TEST_ERROR;
                    }
                }
            }
        }
    }
    else {
        for (step = 0; step < s->nsteps; step++) {
            dbgf(1, "reader: step %d\n", step);

            /* At communication interval, waits for the writer to finish creation before starting verification
             */
            if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                step % s->csteps == 0) {
                if (false == sock_rd_receive(s)) {
                    TEST_ERROR;
                }
            }

            /* For the default test, wait for a few ticks for the update to happen */
            if (s->use_communication && s->attr_test == false)
                decisleep(config->tick_len * s->update_interval);

            vg_ret = verify_group_operations(s, step);

            if (vg_ret == false) {

                printf("verify_group_operations failed\n");

                /* At communication interval, tell the writer about the failure and exit */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0)
                    sock_send_error(s);
                TEST_ERROR;
            }
            else {

                /* Send back the same notify value for acknowledgement to tell the writer
                 * move to the next step. */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0) {
                    if (sock_rd_send(s) == false) {
                        TEST_ERROR;
                    }
                }
            }
        }
    }

    if (H5Pclose(fapl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(s->one_by_one_sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Fclose(s->file) < 0) {
        printf("H5Fclose failed\n");
        TEST_ERROR;
    }

    if (s->use_communication) {
        socket_close(&s->sock);
    }

    free(config);
    free(s);

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Sclose(s->one_by_one_sid);
        H5Fclose(s->file);
    }
    H5E_END_TRY;

    if (s->use_communication) {
        socket_close(&s->sock);
    }

    free(config);
    free(s);

    return EXIT_FAILURE;
}
#else  /* H5_USE_SOCKETS */
int
main(int argc, char **argv)
{
    hid_t                  fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID;
    unsigned               step;
    hbool_t                writer = false;
    state_t               *s      = NULL;
    const char            *personality;
    H5F_vfd_swmr_config_t *config                = NULL;
    const char            *fifo_writer_to_reader = "./fifo_group_writer_to_reader";
    const char            *fifo_reader_to_writer = "./fifo_group_reader_to_writer";
    int                    fd_writer_to_reader = -1, fd_reader_to_writer = -1;
    int                    notify = 0, verify = 0;
    hbool_t                wg_ret = false;
    hbool_t                vg_ret = false;

    if (NULL == (s = calloc(1, sizeof(state_t))))
        TEST_ERROR;
    if (NULL == (config = calloc(1, sizeof(H5F_vfd_swmr_config_t))))
        TEST_ERROR;

    if (!state_init(s, argc, argv)) {
        printf("state_init failed\n");
        TEST_ERROR;
    }

    personality = strstr(s->progname, "vfd_swmr_group_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_group_reader") == 0)
        writer = false;
    else {
        printf("unknown personality, expected vfd_swmr_group_{reader,writer}\n");
        TEST_ERROR;
    }

    /* config, tick_len, max_lag, presume_posix_semantics, writer,
     * maintain_metadata_file, generate_updater_files, flush_raw_data, md_pages_reserved,
     * md_file_path, md_file_name, updater_file_path */
    init_vfd_swmr_config(config, s->tick_len, s->max_lag, false, writer, true, false, true, 128, "./",
                         "group-shadow", NULL);

    /* If old-style option is chosen, use the earliest file format(H5F_LIBVER_EARLIEST)
     * as the second parameter of H5Pset_libver_bound() that is called by
     * vfd_swmr_create_fapl. Otherwise, the latest file format(H5F_LIBVER_LATEST)
     * should be used as the second parameter of H5Pset_libver_bound().
     * Also pass the use_vfd_swmr, only_meta_page, page_buf_size, config to vfd_swmr_create_fapl().*/
    if ((fapl = vfd_swmr_create_fapl(!s->old_style_grp, s->use_vfd_swmr, true, s->pbs, config)) < 0) {
        printf("vfd_swmr_create_fapl failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, s->ps)) < 0) {
        printf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    if (writer)
        s->file = H5Fcreate(s->filename, H5F_ACC_TRUNC, fcpl, fapl);
    else {
        /* The writer and reader are launched concurrently with no ordering
         * guarantee (these scenarios have no WRITER_MESSAGE gating), so the
         * reader can reach this open before the writer has created the file.
         * Retry the open until the file appears rather than failing
         * immediately: a reader that died here would leave the writer blocked
         * forever waiting for it in the handshake below -- the root cause of
         * an observed multi-hour hang in the os_groups_* scenarios.
         */
        unsigned       open_ntries;
        const unsigned max_open_ntries = 300; /* ~30s at 0.1s each */

        for (open_ntries = 0; open_ntries < max_open_ntries; open_ntries++) {
            H5E_BEGIN_TRY
            {
                s->file = H5Fopen(s->filename, H5F_ACC_RDONLY, fapl);
            }
            H5E_END_TRY;

            if (s->file >= 0)
                break;

            decisleep(1); /* 0.1s */
        }
    }

    if (s->file < 0) {
        printf("H5Fcreate/open failed\n");
        TEST_ERROR;
    }

    /* Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication so that the two sides can move forward together.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.  */
    if (s->use_communication && writer) {
        /* Writer creates two named pipes(FIFO) */
        if (mkfifo(fifo_writer_to_reader, 0600) < 0) {
            printf("mkfifo failed\n");
            TEST_ERROR;
        }

        if (mkfifo(fifo_reader_to_writer, 0600) < 0) {
            printf("mkfifo failed\n");
            TEST_ERROR;
        }
    }

    /* Both the writer and reader open the pipes */
    if (s->use_communication && (fd_writer_to_reader = open(fifo_writer_to_reader, O_RDWR)) < 0) {
        printf("open failed\n");
        TEST_ERROR;
    }

    if (s->use_communication && (fd_reader_to_writer = open(fifo_reader_to_writer, O_RDWR)) < 0) {
        printf("open failed\n");
        TEST_ERROR;
    }

    /* Pass the named pipe information to the struct of state_t s, for attribute tests.*/
    if (s->use_communication) {
        s->np_fd_w_to_r = fd_writer_to_reader;
        s->np_fd_r_to_w = fd_reader_to_writer;
        s->np_notify    = notify;
        s->np_verify    = verify;
    }

    /* For attribute test, force the named pipe to communicate in every step.
     * This will avoid the fake verification error from the reader when using the named pipe.
     * If the named pipe is not forced to communicate in every step, the reader may go ahead
     * to verify the group and the attribute operations before the writer has a chance to
     * carry out the corresponding operations. */
    if (s->attr_test && s->use_communication)
        s->csteps = 1;

    /* For group operation test, force the named pipe to communicate in every step. */
    if (s->grp_op_test && s->use_communication)
        s->csteps = 1;

    if (writer) {
        for (step = 0; step < s->nsteps; step++) {
            dbgf(2, "writer: step %d\n", step);

            wg_ret = group_operations(s, step);

            if (wg_ret == false) {

                /* At communication interval, notifies the reader about the failure and quit */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0)
                    np_send_error(s, true);
                printf("write_group failed at step %d\n", step);
                TEST_ERROR;
            }
            else {

                /* At communication interval, notifies the reader and waits for its response */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0) {

                    if (np_wr_send_receive(s) == false) {
                        printf("writer: write group - verification failed.\n");
                        TEST_ERROR;
                    }
                }
            }
        }
    }
    else {
        for (step = 0; step < s->nsteps; step++) {
            dbgf(1, "reader: step %d\n", step);

            /* At communication interval, waits for the writer to finish creation before starting verification
             */
            if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                step % s->csteps == 0) {
                if (false == np_rd_receive(s)) {
                    TEST_ERROR;
                }
            }

            /* For the default test, wait for a few ticks for the update to happen */
            if (s->use_communication && s->attr_test == false)
                decisleep(config->tick_len * s->update_interval);

            vg_ret = verify_group_operations(s, step);

            if (vg_ret == false) {

                printf("verify_group_operations failed\n");

                /* At communication interval, tell the writer about the failure and exit */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0)
                    np_send_error(s, false);
                TEST_ERROR;
            }
            else {

                /* Send back the same notify value for acknowledgement to tell the writer
                 * move to the next step. */
                if (s->use_communication && s->attr_test != true && s->grp_op_test != true &&
                    step % s->csteps == 0) {
                    if (np_rd_send(s) == false) {
                        TEST_ERROR;
                    }
                }
            }
        }
    }

    if (H5Pclose(fapl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(s->one_by_one_sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Fclose(s->file) < 0) {
        printf("H5Fclose failed\n");
        TEST_ERROR;
    }

    /* Both the writer and reader close the named pipes */
    if (s->use_communication && close(fd_writer_to_reader) < 0) {
        printf("close failed\n");
        TEST_ERROR;
    }

    if (s->use_communication && close(fd_reader_to_writer) < 0) {
        printf("close failed\n");
        TEST_ERROR;
    }

    /* Reader finishes last and deletes the named pipes */
    if (s->use_communication && !writer) {
        if (remove(fifo_writer_to_reader) != 0) {
            printf("remove failed\n");
            TEST_ERROR;
        }

        if (remove(fifo_reader_to_writer) != 0) {
            printf("remove failed\n");
            TEST_ERROR;
        }
    }

    free(config);
    free(s);

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Sclose(s->one_by_one_sid);
        H5Fclose(s->file);
    }
    H5E_END_TRY;

    if (s->use_communication && fd_writer_to_reader >= 0)
        close(fd_writer_to_reader);

    if (s->use_communication && fd_reader_to_writer >= 0)
        close(fd_reader_to_writer);

    if (s->use_communication && !writer) {
        remove(fifo_writer_to_reader);
        remove(fifo_reader_to_writer);
    }

    free(config);
    free(s);

    return EXIT_FAILURE;
}
#endif /* H5_USE_SOCKETS */

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    fprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
}

#endif /* H5_HAVE_WIN32_API */
