#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <io.h>
#define fsync _commit
#define snprintf _snprintf
#else
#include <unistd.h>
#endif

#include "trealla.h"
#include "internal.h"
#include "bifs.h"
#include "jela.h"

#define ffsync(fp) fsync(fileno(fp))

int g_kvs_merge = 0;

static int kvs_load_file(trealla *pl, FILE *fp, const char *filename)
{
	tpl_query *q = trealla_create_query(pl);
	if (!q) return 0;
	lexer lex;
	lexer_init(&lex, pl);
	q->lex = &lex;
	q->pl->kvs_loading = 1;
	unsigned line_nbr = 0;
	char *line;

	while ((line = trealla_readline(fp)) != NULL)
	{
		line_nbr++;

		if (!isalpha(line[0]))
			continue;

		if (strncmp(line, "kvs:", 4) && strncmp(line, "sys:", 4))
			continue;

		if (!query_parse(q, line))
			printf("ERROR: '%s'\n", filename);
		else
			query_run(q);

		query_reset(q);
		free(line);
	}

	query_destroy(q);
	return 1;
}

static void kvs_load(trealla *pl, const char *name)
{
	char filename[1024];
	snprintf(filename, sizeof(filename), "%s.tmp.kvs", name);
	FILE *fp = fopen(filename, "rb");
	if (fp != NULL)
	{
		fclose(fp);
		remove(filename);
	}

	snprintf(filename, sizeof(filename), "%s.new.kvs", name);
	fp = fopen(filename, "rb");
	int any = 0;

	if (fp != NULL)
	{
		fclose(fp);
		char tmpname[1024];
		snprintf(tmpname, sizeof(tmpname), "%s.log.kvs", name);
		remove(tmpname);
		snprintf(tmpname, sizeof(tmpname), "%s.dat.kvs", name);
		remove(tmpname);
		rename(filename, tmpname);
	}

	// Load the database stream...

	snprintf(filename, sizeof(filename), "%s.dat.kvs", name);
	fp = fopen(filename, "rb");

	if (fp != NULL)
	{
		printf("Loading '%s'...", filename);
		any++;

		if (!strcmp(name, pl->db.name))
			pl->kvs_loaded = 1;

		kvs_load_file(pl, fp, filename);
		printf("%llu items\n", (unsigned long long)any);
		fclose(fp);
	}

	// Load the transaction stream...

	snprintf(filename, sizeof(filename), "%s.log.kvs", name);
	fp = fopen(filename, "rb");
	any = 0;

	if (fp != NULL)
	{
		printf("Loading '%s'... ", filename);
		any++;

		if (!strcmp(name, pl->db.name))
			pl->kvs_loaded = 1;

		kvs_load_file(pl, fp, filename);
		printf("%llu items\n", (unsigned long long)any);
		fclose(fp);
	}

	if (g_kvs_merge)
	{
		printf("Merge\n");
		g_kvs_merge = 0;
		pl->kvs_dirty = 1;
		pl->kvs_loaded = 0;
		kvs_save(pl);
		pl->kvs_loaded = 1;
	}
}

static void kvs_save_node(trealla *pl, FILE *fp, char **dstbuf, size_t *buflen, const char *key, node *n, int as_tran)
{
	char *dst = *dstbuf;
	*buflen -= 10;						// a bit of leeway
	char keybuf[KEY_SIZE*2];
	deescape(keybuf, key, '\'');

	if (n->flags & FLAG_KVS_DISCARD)
	{
		dst += snprintf(dst, *buflen, "kvs:erase('%s',_)", keybuf);
		sl_del(&pl->kvs, key, NULL);
		term_heapcheck(n);
	}
	else if (n->flags & FLAG_KVS_LPOP)
	{
		n->flags &= ~FLAG_DIRTY;
		dst += snprintf(dst, *buflen, "kvs:lpop('%s')", keybuf);
	}
	else if (n->flags & FLAG_KVS_LPUSH)
	{
		n->flags &= ~FLAG_DIRTY;
		dst += snprintf(dst, *buflen, "kvs:lpush('%s',", keybuf);
		dst += sprint2_term(dstbuf, buflen, &dst, pl, NULL, n, 1);
		*dst++ = ')';
	}
	else if (n->flags & FLAG_KVS_LERASE)
	{
		n->flags &= ~FLAG_DIRTY;
		dst += snprintf(dst, *buflen, "kvs:lerase('%s',", keybuf);
		dst += sprint2_term(dstbuf, buflen, &dst, pl, NULL, n, 1);
		*dst++ = ')';
	}
	else if (n->flags & FLAG_KVS_INC)
	{
		n->flags &= ~FLAG_DIRTY;
		dst += snprintf(dst, *buflen, "kvs:inc('%s',_,", keybuf);
		dst += sprint2_term(dstbuf, buflen, &dst, pl, NULL, n, 1);
		*dst++ = ')';
	}
	else
	{
		n->flags &= ~FLAG_DIRTY;
		dst += snprintf(dst, *buflen, "kvs:put('%s',", keybuf);
		dst += sprint2_term(dstbuf, buflen, &dst, pl, NULL, n, 1);
		*dst++ = ')';
	}

	*dst++ = as_tran?',':'.';
	if (!as_tran) *dst++ = '\n';
	*dst = '\0';
	fwrite(*dstbuf, 1, dst-*dstbuf, fp);
}

int kvs_save(trealla *pl)
{
	if (pl->kvsfp)
	{
		fflush(pl->kvsfp);
		ffsync(pl->kvsfp);
		fclose(pl->kvsfp);
		pl->kvsfp = NULL;
	}

	if (!pl->kvs_dirty)
		return 1;

	char filename[1024];
	snprintf(filename, sizeof(filename), "%s.%s", pl->db.name, !pl->kvs_loaded?"tmp.kvs":"log.kvs");
	FILE *fp = fopen(filename, !pl->kvs_loaded?"wb":"ab");
	if (fp == NULL) return 0;
	printf("Saving '%s'\n", filename);
	const char *key;
	node *n = NULL;
	size_t buflen = 1024*64;					// expandable
	char *tmpbuf = (char*)malloc(buflen+1);
	sl_start(&pl->kvs);

	while ((key = sl_next(&pl->kvs, (void**)&n)) != NULL)
	{
		if (pl->kvs_loaded && !(n->flags & FLAG_DIRTY))
			continue;

		kvs_save_node(pl, fp, &tmpbuf, &buflen, key, n, 0);
	}

	fflush(fp);
	ffsync(fp);
	fclose(fp);
	pl->kvs_dirty = 0;
	free(tmpbuf);

	if (pl->kvs_loaded)
		return 1;

	char tmpname[1024];
	snprintf(tmpname, sizeof(tmpname), "%s.new.kvs", pl->db.name);
	rename(filename, tmpname);
	strcpy(filename, tmpname);
	snprintf(tmpname, sizeof(tmpname), "%s.log.kvs", pl->db.name);
	remove(tmpname);
	snprintf(tmpname, sizeof(tmpname), "%s.dat.kvs", pl->db.name);
	remove(tmpname);
	rename(filename, tmpname);
	printf("Renamed '%s'\n", tmpname);
	return 1;
}

void kvs_done(skiplist *d)
{
	sl_start(d);
	node *n = NULL;

	while ((sl_next(d, (void**)&n)) != NULL)
		term_heapcheck(n);

	sl_done(d, NULL);
}

static int bif_kvs_lget(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);
	KVSUNLOCK(q);

	if (!exists)
		n = make_const_atom("[]", 0);
	else if (n->flags & FLAG_KVS_DISCARD)
		n = make_const_atom("[]", 0);
	else
		n = clone_term(q, n);

	int ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_kvs_lpush(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_nonvar(term2);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	if (strlen_utf8(key) > FUNCTOR_LEN)
		{ QABORT(ABORT_ARGTOOBIG); return 0; }

	if (q->kvs_tran)
	{
		node *tmp = clone_term(q, term2);
		tmp->flags |= FLAG_KVS_LPUSH;
		tmp->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, tmp);
		return 1;
	}

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);

	if (!exists)
		n = make_const_atom("[]", 0);
	else if (n->flags & FLAG_KVS_DISCARD)
		n = make_const_atom("[]", 0);
	else
		n = clone_term(q, n);

	node *l = make_list();
	NLIST_PUSH_BACK(&l->val_l, clone_term(q, term2));
	NLIST_PUSH_BACK(&l->val_l, n);

	if (exists)
	{
		node *tmp = NULL;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);
	}

	sl_set(&q->pl->kvs, strdup(key), l);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		l->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_lpop2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);

	if (!sl_get(&q->pl->kvs, key, (void**)&n))
	{
		KVSUNLOCK(q);
		return 0;
	}

	if (!is_list(n))
	{
		KVSUNLOCK(q);
		return 0;
	}

	node *n2 = NLIST_FRONT(&n->val_l);
	n2 = NLIST_NEXT(n2);					// head

	if (!unify_term(q, term2, n2, q->curr_frame))
	{
		KVSUNLOCK(q);
		return 0;
	}

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		node *tmp = make_const_atom("dummy", 1);
		tmp->flags |= FLAG_KVS_LPOP;
		tmp->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, tmp);
		return 1;
	}

	n2 = NLIST_NEXT(n2);					// tail
	node *l = clone_term(q, n2);
	node *tmp = NULL;

	if (sl_del(&q->pl->kvs, key, (void**)&tmp))
		term_heapcheck(tmp);

	sl_set(&q->pl->kvs, strdup(key), l);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		l->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_lpop1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);

	if (!sl_get(&q->pl->kvs, key, (void**)&n))
	{
		KVSUNLOCK(q);
		return 0;
	}

	if (!is_list(n))
	{
		KVSUNLOCK(q);
		return 0;
	}

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		node *tmp = make_const_atom("dummy", 1);
		tmp->flags |= FLAG_KVS_LPOP;
		tmp->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, tmp);
		return 1;
	}

	node *n2 = NLIST_FRONT(&n->val_l);
	n2 = NLIST_NEXT(n2);					// head
	n2 = NLIST_NEXT(n2);					// tail
	node *l = clone_term(q, n2);
	node *tmp = NULL;

	if (sl_del(&q->pl->kvs, key, (void**)&tmp))
		term_heapcheck(tmp);

	sl_set(&q->pl->kvs, strdup(key), l);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		l->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_lput(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	node *term3 = get_term(term3);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	if (strlen_utf8(key) > FUNCTOR_LEN)
		{ QABORT(ABORT_ARGTOOBIG); return 0; }

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);

	if (!exists)
		n = make_const_atom("[]", 0);
	else if (n->flags & FLAG_KVS_DISCARD)
		n = make_const_atom("[]", 0);
	else
		n = clone_term(q, n);

	if (!unify_term(q, term2, n, q->curr_frame))
	{
		KVSUNLOCK(q);
		term_heapcheck(n);
		return 0;
	}

	term_heapcheck(n);
	n = clone_term(q, term3);

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		n->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, n);
		return 1;
	}

	if (exists)
	{
		node *tmp = NULL;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);
	}

	sl_set(&q->pl->kvs, strdup(key), n);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		n->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_lerase(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);

	if (!exists)
	{
		KVSUNLOCK(q);
		return 1;
	}
	else if (n->flags & FLAG_KVS_DISCARD)
	{
		KVSUNLOCK(q);
		return 1;
	}

	node *save = n;
	node *l = make_list();
	node *tmp = l, *last = NULL;
	int matched = 0;

	while (is_list(n))
	{
		n = NLIST_FRONT(&n->val_l);
		n = NLIST_NEXT(n);

		if (!unify_term(q, term2, n, q->curr_frame))
		{
			NLIST_PUSH_BACK(&tmp->val_l, clone_term(q, n));
			if (is_atom(NLIST_NEXT(n))) break;
			node *tmp2;
			NLIST_PUSH_BACK(&tmp->val_l, tmp2=make_list());
			last = tmp;
			tmp = tmp2;
		}
		else
			matched = 1;

		n = NLIST_NEXT(n);
	}

	if (last && (NLIST_COUNT(&tmp->val_l) == 1))
	{
		n = NLIST_POP_BACK(&last->val_l);
		term_heapcheck(n);
		NLIST_PUSH_BACK(&last->val_l, make_const_atom("[]", 0));
	}
	else if (NLIST_COUNT(&tmp->val_l) == 1)
	{
		term_heapcheck(l);
		l = make_const_atom("[]", 0);
	}
	else
		NLIST_PUSH_BACK(&tmp->val_l, make_const_atom("[]", 0));

	if (!matched)
	{
		KVSUNLOCK(q);
		term_heapcheck(l);
		return 1;
	}

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		term_heapcheck(l);
		node *tmp = clone_term(q, term2);
		tmp->flags |= FLAG_KVS_LERASE;
		tmp->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, tmp);
		return 1;
	}

	if (exists)
		sl_del(&q->pl->kvs, key, NULL);

	sl_set(&q->pl->kvs, strdup(key), l);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		l->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	term_heapcheck(save);
	return 1;
}

static int bif_kvs_get(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);
	KVSUNLOCK(q);

	if (!exists)
		n = make_quick_int(0);
	else if (n->flags & FLAG_KVS_DISCARD)
		n = make_quick_int(0);
	else
		n = clone_term(q, n);

	int ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_kvs_put3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	node *term3 = get_term(term3);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	if (strlen_utf8(key) > FUNCTOR_LEN)
		{ QABORT(ABORT_ARGTOOBIG); return 0; }

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);

	if (!exists)
		n = make_quick_int(0);
	else if (n->flags & FLAG_KVS_DISCARD)
		n = make_quick_int(0);
	else
		n = clone_term(q, n);

	if (!unify_term(q, term2, n, q->curr_frame))
	{
		KVSUNLOCK(q);
		term_heapcheck(n);
		return 0;
	}

	term_heapcheck(n);
	n = clone_term(q, term3);

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		n->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, n);
		return 1;
	}

	if (exists)
	{
		node *tmp = NULL;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);
	}

	sl_set(&q->pl->kvs, strdup(key), n);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		n->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_put2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);

	if (strlen_utf8(key) > FUNCTOR_LEN)
		{ QABORT(ABORT_ARGTOOBIG); return 0; }

	node *n = clone_term(q, term2);

	if (q->kvs_tran)
	{
		n->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, n);
		return 1;
	}

	KVSLOCK(q);
	node *tmp = NULL;

	if (sl_del(&q->pl->kvs, key, (void**)&tmp))
		term_heapcheck(tmp);

	sl_set(&q->pl->kvs, strdup(key), n);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		n->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_inc(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_var(term2);
	node *term3 = get_term(term3);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);

	if (!exists)
		n = make_quick_int(0);
	else if (n->flags & FLAG_KVS_DISCARD)
		n = make_quick_int(0);
	else
		n = clone_term(q, n);

	if (!is_integer(n))
	{
		QABORT(ABORT_INVALIDARGNOTINT);
		KVSUNLOCK(q);
		term_heapcheck(n);
		return 0;
	}

	if (!unify_term(q, term2, n, q->curr_frame))
	{
		KVSUNLOCK(q);
		term_heapcheck(n);
		return 0;
	}

	term_heapcheck(n);

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		node *tmp = clone_term(q, term3);
		tmp->flags |= FLAG_KVS_INC;
		tmp->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, tmp);
		return 1;
	}

	n->val_i += term3->val_i;	// is Ok, we cloned

	if (exists)
	{
		node *tmp;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);
	}

	sl_set(&q->pl->kvs, strdup(key), n);

	if (!q->pl->kvs_loading && (key[0] != '$'))
	{
		n->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_erase2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);

	if (!exists)
		n = make_quick_int(0);
	else if (n->flags & FLAG_KVS_DISCARD)
		n = make_quick_int(0);
	else
		n = clone_term(q, n);

	if (!unify_term(q, term2, n, q->curr_frame))
	{
		KVSUNLOCK(q);
		term_heapcheck(n);
		return 0;
	}

	term_heapcheck(n);

	if (q->pl->kvs_loading)
	{
		node *tmp;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);

		KVSUNLOCK(q);
		return 1;
	}

	n->flags |= FLAG_KVS_DISCARD;

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		n->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, n);
		return 1;
	}

	if (key[0] != '$')
	{
		n->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}
	else
	{
		node *tmp;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_erase1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	node *n = NULL;

	KVSLOCK(q);
	int exists = sl_get(&q->pl->kvs, key, (void**)&n);

	if (!exists)
	{
		KVSUNLOCK(q);
		return 1;
	}

	if (q->pl->kvs_loading)
	{
		node *tmp;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);

		KVSUNLOCK(q);
		return 1;
	}

	n->flags |= FLAG_KVS_DISCARD;

	if (q->kvs_tran)
	{
		KVSUNLOCK(q);
		n->key = strdup(key);
		NLIST_PUSH_BACK(&q->pl->kvs_queue, n);
		return 1;
	}

	if (key[0] != '$')
	{
		n->flags |= FLAG_DIRTY;
		q->pl->kvs_dirty = 1;
	}
	else
	{
		node *tmp;

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);
	}

	KVSUNLOCK(q);
	return 1;
}

static int bif_kvs_load(tpl_query *q)
{
	if (q->kvs_tran) return 0;
	KVSLOCK(q);
	kvs_load(q->pl, q->pl->db.name);
	KVSUNLOCK(q);
	return 1;
}

static void kvs_bail(tpl_query *q)
{
	if (!q->kvs_tran)
		return;

	node *n = NULL;

	while ((n = NLIST_POP_FRONT(&q->pl->kvs_queue)) != NULL)
	{
		free(n->key);
		term_heapcheck(n);
	}

	q->kvs_tran = 0;
	KVSUNLOCK(q);
}

static int bif_kvs_begin(tpl_query *q)
{
	if (q->retry)
	{
		kvs_bail(q);
		return 0;
	}

	if (q->kvs_tran)
		return 0;

	allocate_frame(q);
	try_me_nofollow(q);
	KVSLOCK(q);
	q->kvs_tran = 1;
	return 1;
}

static void kvs_end(tpl_query *q, int do_sync)
{
	trust_me(q);

	if (!q->pl->kvsfp)
	{
		char filename[1024];
		snprintf(filename, sizeof(filename), "%s.log.kvs", q->pl->db.name);
		q->pl->kvsfp = fopen(filename, "ab");
	}

	size_t buflen = 1024*64;					// expandable
	char *tmpbuf = (char*)malloc(buflen+1);
	node *n = NULL;
	int any = 0;

	while ((n = NLIST_POP_FRONT(&q->pl->kvs_queue)) != NULL)
	{
		const char *key = n->key;

		if (key[0] != '$')
		{
			kvs_save_node(q->pl, q->pl->kvsfp, &tmpbuf, &buflen, key, n, 1);
			any++;
		}

		node *tmp = NULL;

		if (n->flags & FLAG_KVS_DISCARD)
		{
			if (sl_del(&q->pl->kvs, key, (void**)&tmp))
				term_heapcheck(tmp);

			free(n->key);
			term_heapcheck(n);
			continue;
		}

		if (n->flags & FLAG_KVS_LPOP)
		{
			node *v = NULL;

			if (!sl_del(&q->pl->kvs, key, (void**)&v))
			{
				free(n->key);
				term_heapcheck(n);
				continue;
			}

			free(n->key);
			term_heapcheck(n);

			if (!is_list(v))
				continue;

			node *tmp = NLIST_FRONT(&v->val_l);
			tmp = NLIST_NEXT(tmp);					// head
			n = NLIST_NEXT(tmp);					// tail
			n->refcnt++;
			term_heapcheck(v);
			sl_set(&q->pl->kvs, strdup(key), n);
			n->refcnt--;
			continue;
		}
		else if (n->flags & FLAG_KVS_LPUSH)
		{
			node *l = make_list();
			free(n->key);
			NLIST_PUSH_BACK(&l->val_l, n);			// new head
			node *v = NULL;

			if (!sl_del(&q->pl->kvs, key, (void**)&v))
				NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
			else
				NLIST_PUSH_BACK(&l->val_l, v);		// old list

			sl_set(&q->pl->kvs, strdup(key), l);
			continue;
		}
		else if (n->flags & FLAG_KVS_LERASE)
		{
			node *v;

			if (!sl_get(&q->pl->kvs, key, (void**)&v))
			{
				free(n->key);
				term_heapcheck(n);
				continue;
			}

			if (!is_list(v))
			{
				free(n->key);
				term_heapcheck(n);
				continue;
			}

			node *l = make_list();
			node *tmp = l, *last = NULL;

			while (is_list(v))
			{
				v = NLIST_FRONT(&v->val_l);
				v = NLIST_NEXT(v);

				if (!unify_term(q, v, n, q->curr_frame))
				{
					NLIST_PUSH_BACK(&tmp->val_l, clone_term(q, v));
					if (is_atom(NLIST_NEXT(v))) break;
					node *tmp2;
					NLIST_PUSH_BACK(&tmp->val_l, tmp2=make_list());
					last = tmp;
					tmp = tmp2;
				}

				v = NLIST_NEXT(v);
			}

			if (last && (NLIST_COUNT(&tmp->val_l) == 1))
			{
				v = NLIST_POP_BACK(&last->val_l);
				term_heapcheck(v);
				NLIST_PUSH_BACK(&last->val_l, make_const_atom("[]", 0));
			}
			else if (NLIST_COUNT(&tmp->val_l) == 1)
			{
				term_heapcheck(l);
				l = make_const_atom("[]", 0);
			}
			else
				NLIST_PUSH_BACK(&tmp->val_l, make_const_atom("[]", 0));

			sl_del(&q->pl->kvs, key, (void**)&v);
			term_heapcheck(v);
			sl_set(&q->pl->kvs, strdup(key), l);
			free(n->key);
			term_heapcheck(n);
			continue;
		}
		else if (n->flags & FLAG_KVS_INC)
		{
			node *v = NULL;

			if (sl_del(&q->pl->kvs, key, (void**)&v))
			{
				n->val_i += v->val_i;
				term_heapcheck(v);
			}

			free(n->key);
			sl_set(&q->pl->kvs, strdup(key), n);
			continue;
		}

		if (sl_del(&q->pl->kvs, key, (void**)&tmp))
			term_heapcheck(tmp);

		sl_set(&q->pl->kvs, strdup(key), n);
	}

	if (any)
	{
		const char *msg = "true.\n";
		fwrite(msg, 1, strlen(msg), q->pl->kvsfp);
		fflush(q->pl->kvsfp);
		if (do_sync) ffsync(q->pl->kvsfp);
	}

	q->kvs_tran = 0;
	KVSUNLOCK(q);
	free(tmpbuf);
}

static int bif_kvs_end0(tpl_query *q)
{
	if (!q->kvs_tran)
		return 0;

	kvs_end(q, 1);
	return 1;
}

static int bif_kvs_end1(tpl_query *q)
{
	if (!q->kvs_tran)
		return 0;

	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);
	int n = 0;

	if (is_integer(term1))
		n = term1->val_i;
	else
		n = !strcmp(term1->val_s, "true");

	kvs_end(q, n != 0);
	return 1;
}

void bifs_load_kvs(void)
{
	DEFINE_BIF("kvs:lpush", 2, bif_kvs_lpush);
	DEFINE_BIF("kvs:lpop", 2, bif_kvs_lpop2);
	DEFINE_BIF("kvs:lpop", 1, bif_kvs_lpop1);
	DEFINE_BIF("kvs:lget", 2, bif_kvs_lget);
	DEFINE_BIF("kvs:lput", 3, bif_kvs_lput);
	DEFINE_BIF("kvs:lerase", 2, bif_kvs_lerase);
	DEFINE_BIF("kvs:get", 2, bif_kvs_get);
	DEFINE_BIF("kvs:put", 3, bif_kvs_put3);
	DEFINE_BIF("kvs:put", 2, bif_kvs_put2);
	DEFINE_BIF("kvs:inc", 3, bif_kvs_inc);
	DEFINE_BIF("kvs:erase", 2, bif_kvs_erase2);
	DEFINE_BIF("kvs:erase", 1, bif_kvs_erase1);
	DEFINE_BIF("kvs:begin", 0, bif_kvs_begin);
	DEFINE_BIF("kvs:end", 0, bif_kvs_end0);
	DEFINE_BIF("kvs:end", 1, bif_kvs_end1);
	DEFINE_BIF("kvs:load", 0, bif_kvs_load);
}

