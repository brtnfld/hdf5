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

/*
 * Utility functions for the VFD SWMR tests.
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "vfd_swmr_common.h"
#include "swmr_common.h"

/* For poll() on the listen socket (POSIX); this test's socket code is
 * POSIX-only, matching the guarded socket headers in vfd_swmr_common.h. */
#ifdef H5_HAVE_SYS_SOCKET_H
#include <poll.h>
#endif

/* Only need the pthread solution if sigtimedwait(2) isn't available.
 * There's currently no Windows solution, so ignore that for now.
 */
#if !defined(H5_HAVE_SIGTIMEDWAIT) && !defined(H5_HAVE_WIN32_API)
#include <pthread.h>
#endif

int verbosity = 2;

/* Return true no more than once in any `ival` interval of time,
 * as measured by the system's monotonically increasing timer, to
 * help rate-limit activities.
 *
 * Read the system's current time and compare it with the time stored in
 * `last`.  If the difference between `last` and the current time is
 * greater than the duration `ival`, then record the current time at
 * `last` and return true.  Otherwise, return false.
 */
hbool_t
below_speed_limit(struct timespec *last, const struct timespec *ival)
{
    struct timespec now;
    hbool_t         result;

    assert(0 <= last->tv_nsec && last->tv_nsec < 1000000000LL);
    assert(0 <= ival->tv_nsec && ival->tv_nsec < 1000000000LL);

    /* NOTE: timespec_get() is C11. This may need further tweaks. */
#if defined(H5_HAVE_TIMESPEC_GET)
    if (timespec_get(&now, TIME_UTC) != TIME_UTC) {
        fprintf(stderr, "%s: timespec_get", __func__);
        exit(EXIT_FAILURE);
    }
#elif defined(H5_HAVE_CLOCK_GETTIME)
    if (clock_gettime(CLOCK_MONOTONIC, &now) == -1) {
        fprintf(stderr, "%s: clock_gettime", __func__);
        exit(EXIT_FAILURE);
    }
#elif defined(H5_HAVE_WIN32_API)
    {
        /* GetSystemTimeAsFileTime: always available, no CRT version dependency */
        FILETIME       ft;
        ULARGE_INTEGER uli;
        GetSystemTimeAsFileTime(&ft);
        uli.LowPart  = ft.dwLowDateTime;
        uli.HighPart = ft.dwHighDateTime;
        /* Convert from 100-ns ticks since 1601-01-01 to Unix epoch */
        uli.QuadPart -= 116444736000000000ULL;
        now.tv_sec  = (time_t)(uli.QuadPart / 10000000ULL);
        now.tv_nsec = (long)((uli.QuadPart % 10000000ULL) * 100ULL);
    }
#else
#error "No suitable time function (timespec_get or clock_gettime) available"
#endif

    if ((uint64_t)now.tv_sec - (uint64_t)last->tv_sec > (uint64_t)ival->tv_sec)
        result = true;
    else if ((uint64_t)now.tv_sec - (uint64_t)last->tv_sec < (uint64_t)ival->tv_sec)
        result = false;
    else
        result = ((uint64_t)now.tv_nsec - (uint64_t)last->tv_nsec >= (uint64_t)ival->tv_nsec);

    if (result)
        *last = now;

    return result;
}

/* Sleep for `tenths` tenths of a second. */
void
decisleep(uint32_t tenths)
{
    uint64_t nsec = tenths * 100 * 1000 * 1000;

    H5_nanosleep(nsec);
}

/* Like vsnprintf(3), but abort the program with an error message on
 * `stderr` if the buffer is too small or some other error occurs.
 */
void
evsnprintf(char *buf, size_t bufsz, const char *fmt, va_list ap)
{
    int rc;

    rc = vsnprintf(buf, bufsz, fmt, ap);

    if (rc < 0) {
        fprintf(stderr, "%s: HDvsnprintf", __func__);
        exit(EXIT_FAILURE);
    }
    else if ((size_t)rc >= bufsz) {
        fprintf(stderr, "%s: buffer too small", __func__);
        exit(EXIT_FAILURE);
    }
}

/* Like snprintf(3), but abort the program with an error message on
 * `stderr` if the buffer is too small or some other error occurs.
 */
void
esnprintf(char *buf, size_t bufsz, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    evsnprintf(buf, bufsz, fmt, ap);
    va_end(ap);
}

void
dbgf(int level, const char *fmt, ...)
{
    va_list ap;

    if (verbosity < level)
        return;

    va_start(ap, fmt);
    (void)vfprintf(stderr, fmt, ap);
    va_end(ap);
}

/* Disable HDF5 error-stack printing and return the previous state
 * of error-stack printing.
 */
estack_state_t
disable_estack(void)
{
    estack_state_t es = estack_get_state();

    (void)H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    return es;
}

/* Return the current state of HDF5 error-stack printing. */
estack_state_t
estack_get_state(void)
{
    estack_state_t es;

    (void)H5Eget_auto2(H5E_DEFAULT, &es.efunc, &es.edata);

    return es;
}

/* Restore HDF5 error-stack printing to a state returned previously by
 * `disable_estack` or `estack_get_state`.
 */
void
restore_estack(estack_state_t es)
{
    (void)H5Eset_auto2(H5E_DEFAULT, es.efunc, es.edata);
}

#ifndef H5_HAVE_WIN32_API
/* Store the signal mask at `oldset` and then block all signals. */
void
block_signals(sigset_t *oldset)
{
    sigset_t fullset;

    if (sigfillset(&fullset) == -1) {
        fprintf(stderr, "%s.%d: could not initialize signal masks", __func__, __LINE__);
        exit(EXIT_FAILURE);
    }

    if (sigprocmask(SIG_BLOCK, &fullset, oldset) == -1) {
        fprintf(stderr, "%s.%d: sigprocmask", __func__, __LINE__);
        exit(EXIT_FAILURE);
    }
}

/* Restore the signal mask in `oldset`. */
void
restore_signals(sigset_t *oldset)
{
    if (sigprocmask(SIG_SETMASK, oldset, NULL) == -1) {
        fprintf(stderr, "%s.%d: sigprocmask", __func__, __LINE__);
        exit(EXIT_FAILURE);
    }
}

#ifndef H5_HAVE_SIGTIMEDWAIT

typedef struct timer_params_t {
    struct timespec *tick;
    hid_t            fid;
} timer_params_t;

pthread_mutex_t timer_mutex;
hbool_t         timer_stop = false;

static void *
timer_function(void *arg)
{
    timer_params_t *params = (timer_params_t *)arg;
    sigset_t        sleepset;
    hbool_t         done = false;

    /* Ignore any signals */
    sigfillset(&sleepset);
    pthread_sigmask(SIG_SETMASK, &sleepset, NULL);

    for (;;) {
        estack_state_t es;

        nanosleep(params->tick, NULL);

        /* Check the mutex */
        pthread_mutex_lock(&timer_mutex);
        done = timer_stop;
        pthread_mutex_unlock(&timer_mutex);
        if (done)
            break;

        /* Avoid deadlock with peer: periodically enter the API so that
         * tick processing occurs and data is flushed so that the peer
         * can see it.
         *
         * The call we make will fail, but that's ok,
         * so squelch errors.
         */
        es = disable_estack();
        (void)H5Aexists_by_name(params->fid, "nonexistent", "nonexistent", H5P_DEFAULT);
        restore_estack(es);
    }

    return NULL;
}
#endif /* H5_HAVE_SIGTIMEDWAIT */

/* Wait for any signal to occur and then return.  Wake periodically
 * during the wait to perform API calls: in this way, the
 * VFD SWMR tick number advances and recent changes do not languish
 * in HDF5 library buffers where readers cannot see them.
 */
void
await_signal(hid_t fid)
{
    struct timespec tick = {.tv_sec = 0, .tv_nsec = 1000000000LL / 100};
    sigset_t        sleepset;

    if (sigfillset(&sleepset) == -1) {
        fprintf(stderr, "%s.%d: could not initialize signal mask", __func__, __LINE__);
        exit(EXIT_FAILURE);
    }

    /* Avoid deadlock: flush the file before waiting for the reader's
     * message.
     */
    if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0) {
        fprintf(stderr, "%s: H5Fflush failed", __func__);
        exit(EXIT_FAILURE);
    }

    dbgf(1, "waiting for signal\n");

#ifndef H5_HAVE_SIGTIMEDWAIT
    {
        /* Use an alternative scheme for platforms like MacOS that do not have
         * sigtimedwait(2)
         */
        timer_params_t params;
        int            rc;
        pthread_t      timer;

        params.tick = &tick;
        params.fid  = fid;

        pthread_mutex_init(&timer_mutex, NULL);

        pthread_create(&timer, NULL, timer_function, &params);

        {
            int sig;
            rc = sigwait(&sleepset, &sig);
        }

        if (rc != -1) {
            fprintf(stderr, "Received signal, wrapping things up.\n");
            pthread_mutex_lock(&timer_mutex);
            timer_stop = true;
            pthread_mutex_unlock(&timer_mutex);
            pthread_join(timer, NULL);
        }
        else {
            fprintf(stderr, "%s: sigwait", __func__);
            exit(EXIT_FAILURE);
        }
    }
#else
    for (;;) {
        /* Linux and other systems */
        const int rc = sigtimedwait(&sleepset, NULL, &tick);

        if (rc != -1) {
            fprintf(stderr, "Received %s, wrapping things up.\n", strsignal(rc));
            break;
        }
        else if (rc == -1 && errno == EAGAIN) {
            estack_state_t es;

            /* Avoid deadlock with peer: periodically enter the API so that
             * tick processing occurs and data is flushed so that the peer
             * can see it.
             *
             * The call we make will fail, but that's ok,
             * so squelch errors.
             */
            es = disable_estack();
            (void)H5Aexists_by_name(fid, "nonexistent", "nonexistent", H5P_DEFAULT);
            restore_estack(es);
        }
        else if (rc == -1) {
            fprintf(stderr, "%s: sigtimedwait", __func__);
            exit(EXIT_FAILURE);
        }
    }
#endif /* H5_HAVE_SIGTIMEDWAIT */
}

#endif /* H5_HAVE_WIN32_API */

/*
 * Revised support routines that can be used for all VFD SWMR integration tests
 */
void
init_vfd_swmr_config(H5F_vfd_swmr_config_t *config, uint32_t tick_len, uint32_t max_lag,
                     hbool_t presume_posix_semantics, hbool_t writer, hbool_t maintain_metadata_file,
                     hbool_t generate_updater_files, hbool_t flush_raw_data, uint32_t md_pages_reserved,
                     const char *md_path_fmtstr, const char *md_file_fmtstr, const char *updater_path_fmtstr,
                     ...)
{
    va_list ap;

    memset(config, 0, sizeof(H5F_vfd_swmr_config_t));

    config->version                = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config->pb_expansion_threshold = 0;

    config->tick_len                = tick_len;
    config->max_lag                 = max_lag;
    config->presume_posix_semantics = presume_posix_semantics;
    config->writer                  = writer;
    config->maintain_metadata_file  = maintain_metadata_file;
    config->generate_updater_files  = generate_updater_files;
    config->flush_raw_data          = flush_raw_data;
    config->md_pages_reserved       = md_pages_reserved;

    if (md_path_fmtstr == NULL)
        config->md_file_path[0] = '\0';
    else {
        va_start(ap, updater_path_fmtstr);
        evsnprintf(config->md_file_path, sizeof(config->md_file_path), md_path_fmtstr, ap);
        va_end(ap);
    }

    if (md_file_fmtstr == NULL)
        config->md_file_name[0] = '\0';
    else {
        va_start(ap, updater_path_fmtstr);
        evsnprintf(config->md_file_name, sizeof(config->md_file_name), md_file_fmtstr, ap);
        va_end(ap);
    }

    if (config->generate_updater_files && updater_path_fmtstr != NULL) {
        va_start(ap, updater_path_fmtstr);
        evsnprintf(config->updater_file_path, sizeof(config->updater_file_path), updater_path_fmtstr, ap);
        va_end(ap);
    }

} /* init_vfd_swmr_config() */

/* Initialize the log file path in config, this function should be called after init_vfd_swmr_config.  */
void
init_vfd_swmr_log(H5F_vfd_swmr_config_t *config, const char *log_file_fmtstr, ...)
{
    va_list ap;

    va_start(ap, log_file_fmtstr);
    evsnprintf(config->log_file_path, sizeof(config->log_file_path), log_file_fmtstr, ap);
    va_end(ap);

} /* init_vfd_swmr_log() */

/* Perform common VFD SWMR configuration on the file-access property list:
 * configure page buffering, set reasonable VFD SWMR defaults.
 */
/* Set up the file-access property list:
 * --configure for latest format or not
 * --configure the page buffer size to page_buf_size
 * --configure page buffering with only_meta_pages or not
 * --configure for VFD SWMR or not
 */
hid_t
vfd_swmr_create_fapl(bool use_latest_format, bool use_vfd_swmr, bool only_meta_pages, size_t page_buf_size,
                     H5F_vfd_swmr_config_t *config)
{
    hid_t fapl = H5I_INVALID_HID;

    /* Create file access property list */
    if ((fapl = h5_fileaccess()) < 0)
        return H5I_INVALID_HID;

    if (use_latest_format) {
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            return H5I_INVALID_HID;
    }
    else { /* Currently this is used only for old-styled group implementation tests.*/
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
            return H5I_INVALID_HID;
    }

    /* Enable page buffering */
    if (H5Pset_page_buffer_size(fapl, page_buf_size, only_meta_pages ? 100 : 0, 0) < 0)
        return H5I_INVALID_HID;

    /*
     * Set up to open the file with VFD SWMR configured.
     */
    /* Enable VFD SWMR configuration */
    if (use_vfd_swmr && H5Pset_vfd_swmr_config(fapl, config) < 0)
        return H5I_INVALID_HID;

    return fapl;

} /* vfd_swmr_create_fapl() */

/* Create the file creation property list:
 * --Set the file space strategy to fs_strategy
 * --Set the file space page size to fs_page_size
 */
hid_t
vfd_swmr_create_fcpl(H5F_fspace_strategy_t fs_strategy, hsize_t fs_page_size)
{
    hid_t fcpl = H5I_INVALID_HID;

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        return H5I_INVALID_HID;

    if (H5Pset_file_space_strategy(fcpl, fs_strategy, false, 1) < 0)
        return H5I_INVALID_HID;

    if (H5Pset_file_space_page_size(fcpl, fs_page_size) < 0)
        return H5I_INVALID_HID;

    return fcpl;

} /* vfd_swmr_create_fcpl() */

/* Fetch a variable from the environment and parse it for unsigned long
 * content.  Return 0 if the variable is not present, -1 if it is present
 * but it does not parse and compare less than `limit`, 1 if it's present,
 * parses, and is in-bounds.
 */
int
fetch_env_ulong(const char *varname, unsigned long limit, unsigned long *valp)
{
    char         *end;
    unsigned long ul;
    char         *tmp;

    if ((tmp = getenv(varname)) == NULL)
        return 0;

    errno = 0;
    ul    = strtoul(tmp, &end, 0);
    if (ul == ULONG_MAX && errno != 0) {
        fprintf(stderr, "could not parse %s: %s\n", varname, strerror(errno));
        return -1;
    }
    if (end == tmp || *end != '\0') {
        fprintf(stderr, "could not parse %s\n", varname);
        return -1;
    }
    if (ul > limit) {
        fprintf(stderr, "%s (%lu) out of range\n", varname, ul);
        return -1;
    }
    *valp = ul;
    return 1;
}

/* Socket functions */

/* Initialize socket state values. Sets default IP address to localhost.
 * Should be run right after allocating socket_state_t structure. */
hbool_t
socket_init(socket_state_t *sock)
{
    if (sock == NULL) {
        fprintf(stderr, "socket state structure is NULL\n");
        return false;
    }

    sock->ip_address = "127.0.0.1";

    sock->comm_fd   = INVALID_SOCKET;
    sock->listen_fd = INVALID_SOCKET;

    sock->notify = 0;
    sock->verify = 0;

    return true;
}

/* Open sockets for communication between reader and writer.
 * If server is true, open a listening socket and wait for a connection.
 * If server is false, open a client socket and connect to the server at `ip_address`.
 * If `ip_address` is NULL, default to localhost (127.0.0.1).
 *
 * Note: Only supports IPv4 sockets, and only a single connection at a time.
 */
#ifndef H5_HAVE_WIN32_API
hbool_t
socket_connect(socket_state_t *sock, bool server)
{
    struct sockaddr_in servaddr;

    /* Initialize sock address structure memory */
    memset(&servaddr, 0, sizeof(servaddr));

    if (server) { /* Server Code */
        struct sockaddr_in client;

        /* Create listening socket */
        if (INVALID_SOCKET == (sock->listen_fd = socket(AF_INET, SOCK_STREAM, 0))) {
            fprintf(stderr, "error creating listen socket\n");
            goto error;
        }

        /* Configure server socket info */
        servaddr.sin_family      = AF_INET;
        servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
        servaddr.sin_port        = htons(DEFAULT_PORT);

        /* Make address reusable so rerunning this program won't error if the previous run left
         * our chosen Address:Port for the listen socket in a TIME_WAIT state */
        const int enable = 1;
        if (setsockopt(sock->listen_fd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int)) < 0) {
            fprintf(stderr, "error setting socket options\n");
            goto error;
        }

        /* Bind socket */
        if (bind(sock->listen_fd, (struct sockaddr *)&servaddr, sizeof(servaddr)) < 0) {
            fprintf(stderr, "error binding server socket\n");
            goto error;
        }

        /* Start listening on open socket */
        if (listen(sock->listen_fd, 1) < 0) {
            fprintf(stderr, "error listening to server socket\n");
            goto error;
        }

        /* Wait for a client connection with a timeout, then accept it. A bare
         * blocking accept() has no timeout, so a writer whose reader died (or
         * never connected) would block here forever -- the mechanism behind an
         * observed multi-hour hang. The reader-side retries (the H5Fopen retry
         * in the test's reader path and the connect() retry below) make a
         * connection arrive reliably within seconds; this timeout is a backstop
         * that converts any residual failure-to-connect into a prompt,
         * diagnosable error instead of an unbounded hang. */
        {
            struct pollfd pfd;
            int           poll_ret;
            const int     accept_timeout_ms = 120000; /* 2 minutes */

            pfd.fd     = sock->listen_fd;
            pfd.events = POLLIN;

            poll_ret = poll(&pfd, 1, accept_timeout_ms);
            if (poll_ret == 0) {
                fprintf(stderr, "timed out waiting for client connection\n");
                goto error;
            }
            else if (poll_ret < 0) {
                fprintf(stderr, "error polling listen socket\n");
                goto error;
            }
        }

        /* Accept a connection */
        socklen_t len = sizeof(client);
        sock->comm_fd = accept(sock->listen_fd, (struct sockaddr *)&client, &len);
        if (sock->comm_fd == INVALID_SOCKET) {
            fprintf(stderr, "error accepting client connection\n");
            goto error;
        }
#ifdef DEBUG_SOCKETS
        fprintf(stderr, "SERVER SOCKET: Accepted connection from client with IP: %s\n",
                inet_ntoa(client.sin_addr));
#endif

        /* Close the listening socket, we don't need it anymore */
        close(sock->listen_fd);
        sock->listen_fd = INVALID_SOCKET;
    }
    else { /* Client Code */
        int  attempt;
        bool connected = false;
        /* ~30s worth of 0.1s retries. The writer (server) and reader
         * (client) are launched with no ordering guarantee, so the client
         * can reach connect() before the server has reached listen(). A
         * single connect() attempt would then fail with ECONNREFUSED, the
         * reader would die, and the server would block forever in accept()
         * waiting for a client that already gave up -- a real, observed
         * hang for scenarios (e.g. "groups") that don't gate the reader
         * launch on a writer-ready message the way the bigset tests do.
         * Retrying the connect tolerates the startup race generically. */
        const int max_connect_attempts = 300;

        /* Set socket information */
        servaddr.sin_family = AF_INET;
        servaddr.sin_port   = htons(DEFAULT_PORT);

        /* Get binary address for server connection */
        if (inet_pton(AF_INET, sock->ip_address, &servaddr.sin_addr) <= 0) {
            fprintf(stderr, "socket communication inet_pton error\n");
            goto error;
        }

        /* Attempt server connection, retrying to tolerate the writer not
         * having reached listen() yet. A fresh socket is created for each
         * attempt, since a socket whose connect() failed must not be reused
         * for a subsequent connect(). */
        for (attempt = 0; attempt < max_connect_attempts; attempt++) {
            if (INVALID_SOCKET == (sock->comm_fd = socket(AF_INET, SOCK_STREAM, 0))) {
                fprintf(stderr, "error creating client socket\n");
                goto error;
            }

            if (connect(sock->comm_fd, (struct sockaddr *)&servaddr, sizeof(servaddr)) == 0) {
                connected = true;
                break;
            }

            /* This attempt failed; discard the socket and pause before retrying */
            close(sock->comm_fd);
            sock->comm_fd = INVALID_SOCKET;
            decisleep(1); /* 0.1s */
        }

        if (!connected) {
            fprintf(stderr, "socket communication connection error\n");
            goto error;
        }
#ifdef DEBUG_SOCKETS
        fprintf(stderr, "CLIENT SOCKET: Connected to server with IP: %s\n", sock->ip_address);
#endif
    }

    return true;

error:

    if (sock != NULL) {
        socket_close(sock);
    }

    return false;
} /* socket_connect() */

/* Safely close the sockets */
void
socket_close(socket_state_t *sock)
{
    if (sock == NULL) { /* Redundant check  */
        return;
    }

    if (sock->comm_fd != INVALID_SOCKET && sock->comm_fd > 2) {
        close(sock->comm_fd);
    }
    if (sock->listen_fd != INVALID_SOCKET && sock->listen_fd > 2) {
        close(sock->listen_fd);
    }

    sock->ip_address = NULL;
    sock->comm_fd    = INVALID_SOCKET;
    sock->listen_fd  = INVALID_SOCKET;
} /* socket_close() */

#else /* H5_HAVE_WIN32_API */

hbool_t
socket_connect(socket_state_t H5_ATTR_UNUSED *sock, bool H5_ATTR_UNUSED server)
{
    return false;
}

void
socket_close(socket_state_t H5_ATTR_UNUSED *sock)
{
}

#endif /* H5_HAVE_WIN32_API */