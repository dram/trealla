#define _XOPEN_SOURCE
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <spawn.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include "trealla.h"

#include "bifs.h"
#include "jela.h"

extern char **environ;

static int bif_posix_format_time_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_compound(term2);
	node *term3 = get_var(term3);

	if (strcmp(term_functor(term2), "time")) {
		return 0;
	}

	const char *format = VAL_S(term1);
	size_t length = LEN_S(term1);

	// XXX: Is this check reasonable? May strftime() return non-empty
	// result for empty format?

	if (length == 0) {
		return unify_const_atom(q, term3, term3_ctx, "");
	}

	struct tm tm;
	node *n = term_firstarg(term2); tm.tm_sec = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_min = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_hour = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_mday = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_mon = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_year = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_wday = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_yday = VAL_INT(subst(q, n, term2_ctx));
	n = term_next(n); tm.tm_isdst = VAL_INT(subst(q, n, term2_ctx));

	char *buffer = NULL;
	int tries = 0;
	const int max_tries = 5;

	while (++tries <= max_tries) {
		// make enough space for some long formats, e.g. `%c'
		length = 128 + length * 2;
		buffer = realloc(buffer, length);

		// FIXME: `0' returned by strftime() does not always indicate
		// an error, seems there is no easy way to check that.

		if (strftime(buffer, length, format, &tm) > 0) {
			return unify_atom(q, term3, term3_ctx, buffer);
		}
	}

	free(buffer);
	// XXX: maybe raise an error?
	return 0;
}

static int bif_posix_gmt_time_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_var(term2);

	time_t t = (time_t)term1->val_i;
	struct tm tm;

	if (gmtime_r(&t, &tm) == NULL) {
		return 0;
	} else {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("time"));
		term_append(tmp, make_int(tm.tm_sec));
		term_append(tmp, make_int(tm.tm_min));
		term_append(tmp, make_int(tm.tm_hour));
		term_append(tmp, make_int(tm.tm_mday));
		term_append(tmp, make_int(tm.tm_mon));
		term_append(tmp, make_int(tm.tm_year));
		term_append(tmp, make_int(tm.tm_wday));
		term_append(tmp, make_int(tm.tm_yday));
		term_append(tmp, make_int(tm.tm_isdst));

		unify(q, term2, term2_ctx, tmp, -1);
		return 1;
	}
}

static int bif_posix_local_time_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_var(term2);

	time_t t = (time_t)term1->val_i;
	struct tm tm;

	if (localtime_r(&t, &tm) == NULL) {
		return 0;
	} else {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("time"));
		term_append(tmp, make_int(tm.tm_sec));
		term_append(tmp, make_int(tm.tm_min));
		term_append(tmp, make_int(tm.tm_hour));
		term_append(tmp, make_int(tm.tm_mday));
		term_append(tmp, make_int(tm.tm_mon));
		term_append(tmp, make_int(tm.tm_year));
		term_append(tmp, make_int(tm.tm_wday));
		term_append(tmp, make_int(tm.tm_yday));
		term_append(tmp, make_int(tm.tm_isdst));

		unify(q, term2, term2_ctx, tmp, -1);
		return 1;
	}
}

// XXX: Is it desirable to also return corrected `tm'?
static int bif_posix_make_time_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_compound(term1);
	node *term2 = get_var(term2);

	if (strcmp(term_functor(term1), "time")) {
		return 0;
	}

	struct tm tm;
	node *n = term_firstarg(term1); tm.tm_sec = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_min = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_hour = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_mday = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_mon = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_year = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_wday = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_yday = VAL_INT(subst(q, n, term1_ctx));
	n = term_next(n); tm.tm_isdst = VAL_INT(subst(q, n, term1_ctx));

	time_t t = mktime(&tm);

	if (t == -1) {
		return 0;
	} else {
		return unify_int(q, term2, term2_ctx, t);
	}
}

// XXX: To be consistent with posix:format_time/3, order by arguments
// is different from strptime(), is this choice reasonable?
static int bif_posix_parse_time_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);

	struct tm tm;
	memset(&tm, 0, sizeof(struct tm));

	if (strptime(VAL_S(term2), VAL_S(term1), &tm) == NULL) {
		return 0;
	} else {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("time"));
		term_append(tmp, make_int(tm.tm_sec));
		term_append(tmp, make_int(tm.tm_min));
		term_append(tmp, make_int(tm.tm_hour));
		term_append(tmp, make_int(tm.tm_mday));
		term_append(tmp, make_int(tm.tm_mon));
		term_append(tmp, make_int(tm.tm_year));
		term_append(tmp, make_int(tm.tm_wday));
		term_append(tmp, make_int(tm.tm_yday));
		term_append(tmp, make_int(tm.tm_isdst));

		unify(q, term3, term3_ctx, tmp, -1);
		return 1;
	}
}

static int bif_posix_time_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);

	time_t t = time(NULL);

	if (t == -1) {
		return 0;
	} else {
		return unify_int(q, term1, term1_ctx, t);
	}
}

static int bif_posix_spawn_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_list(term2);
	node *term3 = get_var(term3);
	node *term4 = get_next_arg(q, &args);
	unsigned term4_ctx = q->latest_context;

	if (term4 != NULL) {
		if (!is_list(term4)
			&& !(is_atom(term4) && strcmp(VAL_S(term4), "[]"))) {
				QABORT(ABORT_INVALIDARGNOTLIST);
				return 0;
		}
	}

	int i = 0;
	int size = 128;
	const char **arguments = malloc(size);
	node *l = term2;
	while (is_list(l)) {
		node *head = term_firstarg(l);
		node *n = subst(q, head, term2_ctx);

		if (i + 1 >= size) {
			size *= 2;
			arguments = realloc(arguments, size);
		}

		arguments[i++] = VAL_S(n);

		node *tail = term_next(head);
		l = subst(q, tail, term2_ctx);
	}
	arguments[i] = NULL;

	posix_spawn_file_actions_t file_actions;
	posix_spawn_file_actions_init(&file_actions);

	l = term4;
	while (is_list(l)) {
		node *head = term_firstarg(l);
		node *option = subst(q, head, term4_ctx);

		if (!is_compound(option)) {
			// FIXME: raise error?
			continue;
		}

		// TODO: add `attributes` and `environment` option
		if (!strcmp(term_functor(option), "file_actions")) {
			node *l = subst(q, term_firstarg(option), term4_ctx);
			while (is_list(l)) {
				node *head = term_firstarg(l);
				node *action = subst(q, head, term4_ctx);

				if (!strcmp(term_functor(action), "close")) {
					node *n = term_firstarg(action);
					int fd = VAL_INT(subst(q, n, term4_ctx));
					posix_spawn_file_actions_addclose(&file_actions, fd);
				} else if (!strcmp(term_functor(action), "duplicate")) {
					node *n = term_firstarg(action);
					int fd1 = VAL_INT(subst(q, n, term4_ctx));
					n = term_next(n);
					int fd2 = VAL_INT(subst(q, n, term4_ctx));
					posix_spawn_file_actions_adddup2(&file_actions, fd1, fd2);
				} else if (!strcmp(term_functor(action), "open")) {
					node *n = term_firstarg(action);
					int fd = VAL_INT(subst(q, n, term4_ctx));
					n = term_next(n);
					n = subst(q, n, term4_ctx);
					const char *path = VAL_S(n);

					int flag = 0;
					n = term_next(n);
					node *l = subst(q, n, term4_ctx);
					while (is_list(l)) {
						node *head = term_firstarg(l);
						node *n = subst(q, head, term4_ctx);

						if (!is_atom(n)) {
							// FIXME: raise error
							continue;
						}

						// FIXME: incomplete
						if (!strcmp(VAL_S(n), "read_only")) {
							flag |= O_RDONLY;
						} else if (!strcmp(VAL_S(n), "write_only")) {
							flag |= O_WRONLY;
						} else if (!strcmp(VAL_S(n), "read_write")) {
							flag |= O_RDWR;
						} else if (!strcmp(VAL_S(n), "append")) {
							flag |= O_APPEND;
						} else if (!strcmp(VAL_S(n), "create")) {
							flag |= O_CREAT;
						} else if (!strcmp(VAL_S(n), "exclusive")) {
							flag |= O_EXCL;
						} else if (!strcmp(VAL_S(n), "truncate")) {
							flag |= O_TRUNC;
						}

						node *tail = term_next(head);
						l = subst(q, tail, term4_ctx);
					}

					n = term_next(n);
					int mode = VAL_INT(subst(q, n, term4_ctx));
					posix_spawn_file_actions_addopen(&file_actions, fd, path, flag, mode);
				}

				node *tail = term_next(head);
				l = subst(q, tail, term4_ctx);
			}
		}

		node *tail = term_next(head);
		l = subst(q, tail, term4_ctx);
	}

	pid_t pid;
	int r = posix_spawnp(&pid, VAL_S(term1), &file_actions, NULL, (char * const*)arguments, environ);
	posix_spawn_file_actions_destroy(&file_actions);
	free(arguments);
	if (r == 0) {
		return unify_int(q, term3, term3_ctx, pid);
	} else {
		// FIXME: raise error
		return 0;
	}
}

static int bif_posix_wait_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_compound_or_var(term2);
	node *term3 = get_atom_or_list(term3);

	idtype_t type;
	id_t id = 0;

	if (is_compound(term1) && !strcmp(term_functor(term1), "process")) {
		type = P_PID;
		node *n = term_firstarg(term1);
		id = VAL_INT(subst(q, n, term1_ctx));
	} else if (is_compound(term1) && !strcmp(term_functor(term1), "process_group")) {
		type = P_PGID;
		node *n = term_firstarg(term1);
		id = VAL_INT(subst(q, n, term1_ctx));
	} else if (is_atom(term1) && !strcmp(VAL_S(term1), "all")) {
		type = P_ALL;
	} else {
		// FIXME: Maybe raise an error?
		return 0;
	}

	int options = 0;
	node *l = term3;
	while (is_list(l)) {
		node *head = term_firstarg(l);
		node *n = subst(q, head, term3_ctx);

		if (is_atom(n) && !strcmp(VAL_S(n), "continued")) {
			options |= WCONTINUED;
		} else if (is_atom(n) && !strcmp(VAL_S(n), "exited")) {
			options |= WEXITED;
		} else if (is_atom(n) && !strcmp(VAL_S(n), "stopped")) {
			options |= WSTOPPED;
		} else if (is_atom(n) && !strcmp(VAL_S(n), "no_hang")) {
			options |= WNOHANG;
		} else if (is_atom(n) && !strcmp(VAL_S(n), "no_wait")) {
			options |= WNOWAIT;
		} else {
			// FIXME: Maybe raise an error?
			return 0;
		}

		node *tail = term_next(head);
		l = subst(q, tail, term3_ctx);
	}

	siginfo_t info;
	memset(&info, 0, sizeof(siginfo_t));
	if (waitid(type, id, &info, options) == 0) {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("wait"));

		node *process = make_compound();
		term_append(process, make_const_atom("process"));
		term_append(process, make_int(info.si_pid));
		term_append(tmp, process);

		node *status = make_compound();
		term_append(status, make_const_atom("status"));
		term_append(status, make_int(info.si_status));
		term_append(tmp, status);

		unify(q, term2, term2_ctx, tmp, -1);
		return 1;
	} else {
		// FIXME: raise an error
		return 0;
	}
}

static int bif_posix_scan_directory_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);

	const char *directory = VAL_S(term1);

	struct dirent **names;

	int n = scandir(directory, &names, NULL, NULL);

	if (n == -1) {
		// FIXME: raise an error
		return 0;
	} else {
		node *save_l = make_list();

		node *l = save_l;
		for (int i = 0; i < n; ++i) {
			node *entry = make_compound();
			term_append(entry, make_const_atom("file"));
			term_append(entry, make_int(names[i]->d_ino));
			term_append(entry, make_atom(strdup(names[i]->d_name)));
			term_append(l, entry);
			free(names[i]);

			if (i + 1 == n) {
				term_append(l, make_const_atom("[]"));
			} else {
				l = term_append(l, make_list());
			}
		}

		free(names);

		unify(q, term2, term2_ctx, save_l, -1);

		return 1;
	}
}

static int bif_posix_close_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);

	int fd = VAL_INT(subst(q, term1, term1_ctx));

	if (close(fd) == -1) {
		return 0;
	} else {
		return 1;
	}
}

static int bif_posix_open_file_descriptor_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	int fd = VAL_INT(term1);
	const char *mode = VAL_S(term2);
	const char *type = "text";
	char tmpbuf[40];
	strcpy(tmpbuf, !strcmp(mode, "append") ? "a" : !strcmp(mode, "update") ? "r+" : !strcmp(mode, "write") ? "w" : "r");

	FILE *fp = fdopen(fd, tmpbuf);

	if (!fp) {
		QABORT(ABORT_NOTEXISTFILE);
		return 0;
	}

	stream *sp = calloc(1, sizeof(stream));
	sp->fptr = fp;
	const int max_name_length = 32;
	char *name = malloc(max_name_length);
	snprintf(name, max_name_length, "fd(%d)", fd);
	sp->filename = name;
	sp->mode = strdup(mode);
	sp->type = strdup(type);
	node *n = make_stream(sp);
	n->flags |= FLAG_FILE;
	put_env(q, q->c.curr_frame + term3->slot, n, -1);
	term_heapcheck(n);
	return 1;
}

static int bif_posix_pipe_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_compound_or_var(term1);

	int fd[2];

	if (pipe(fd) == -1) {
		return 0;
	} else {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("-"));
		term_append(tmp, make_int(fd[0]));
		term_append(tmp, make_int(fd[1]));

		return unify(q, term1, term1_ctx, tmp, -1);
	}
}

void bifs_load_posix(void)
{
	DEFINE_BIF("posix:format_time", 3, bif_posix_format_time_3);
	DEFINE_BIF("posix:gmt_time", 2, bif_posix_gmt_time_2);
	DEFINE_BIF("posix:local_time", 2, bif_posix_local_time_2);
	DEFINE_BIF("posix:make_time", 2, bif_posix_make_time_2);
	DEFINE_BIF("posix:parse_time", 3, bif_posix_parse_time_3);
	DEFINE_BIF("posix:time", 1, bif_posix_time_1);

	DEFINE_BIF("posix:spawn", 3, bif_posix_spawn_3);
	DEFINE_BIF("posix:spawn", 4, bif_posix_spawn_3);

	// TODO Maybe add wait/2 with `options` defaults to `[exited]`?
	DEFINE_BIF("posix:wait", 3, bif_posix_wait_3);

	// TODO How to pass `select` and `compare` functions to `scandir`?
	DEFINE_BIF("posix:scan_directory", 2, bif_posix_scan_directory_2);

	DEFINE_BIF("posix:close", 1, bif_posix_close_1);
	DEFINE_BIF("posix:open_file_descriptor", 3, bif_posix_open_file_descriptor_3);
	DEFINE_BIF("posix:pipe", 1, bif_posix_pipe_1);
}
