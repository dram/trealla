#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

#include "trealla.h"

#include "bifs.h"
#include "jela.h"

#if defined(DEBUG) && 0
#define DEBUGPRINT if (q->trace)
#else
#define DEBUGPRINT if (0)
#endif

#define TRACE(s)                                                                                                               \
	DEBUGPRINT                                                                                                                 \
	{                                                                                                                          \
		printf("###[ %s ] ==> frame=%d, size=%u, choice=%u, prev=%u, "                                                         \
		       "env=%u, "                                                                                                      \
		       "trail=%u, size=%u\n",                                                                                          \
		       s, q->c.curr_frame, q->c.frame_size, q->choice_point, q->c.prev_choice, q->c.env_point, q->c.trail_point,       \
		       q->c.trail_size);                                                                                               \
	}

int grow_environment(tpl_query *q)
{
	TRACE("grow_environment");

	//if (!q->parent)
	//	printf("*** ENV %lld = %lld\n", (long long)q->envs_possible, (long long)(sizeof(env)*q->envs_possible));

	int MULT = 2;

	if (((sizeof(env) * q->envs_possible / 1024 / 1024) * MULT) >= g_trealla_memlimit_mb) {
		QABORT(ABORT_MAXENVS);
		return 0;
	}

	if (q->def_env) {
		q->envs = (env *)calloc(1, sizeof(env) * q->envs_possible * MULT);
		memcpy(q->envs, q->env_stack, sizeof(env) * q->envs_possible);
		q->def_env = 0;
	}
	else {
		q->envs = (env *)realloc(q->envs, sizeof(env) * q->envs_possible * MULT);
		memset(&q->envs[q->envs_possible], 0, sizeof(env) * q->envs_possible * (MULT-1));
	}

	q->envs_possible *= MULT;
	return 1;
}

static int grow_choice(tpl_query *q)
{
	TRACE("grow_choice");

	//if (!q->parent)
	//	printf("*** CHOICE %lld = %lld\n", (long long)q->choices_possible, (long long)(sizeof(choice)*q->choices_possible));

	int MULT = 2;

	if (((sizeof(choice) * q->choices_possible) / 1024 / 1024) * MULT >= g_trealla_memlimit_mb) {
		QABORT(ABORT_MAXCHOICES);
		return 0;
	}

	if (q->def_choice) {
		q->choices = (choice *)calloc(1, sizeof(choice) * q->choices_possible * MULT);
		memcpy(q->choices, q->choice_stack, sizeof(choice) * q->choices_possible);
		q->def_choice = 0;
	}
	else
		q->choices = (choice *)realloc(q->choices, sizeof(choice) * q->choices_possible * MULT);

	q->choices_possible *= MULT;
	return 1;
}

static int grow_trail(tpl_query *q)
{
	TRACE("grow_trail");

	//if (!q->parent)
	//	printf("*** TRAIL %lld = %lld\n", (long long)q->trails_possible, (long long)(sizeof(trail)*q->trails_possible));

	int MULT = 2;

	if (((sizeof(trail) * q->trails_possible / 1024 / 1024) * MULT) >= g_trealla_memlimit_mb) {
		QABORT(ABORT_MAXTRAILS);
		return 0;
	}

	if (q->def_trail) {
		q->trails = (trail *)calloc(1, sizeof(trail) * q->trails_possible * MULT);
		memcpy(q->trails, q->trail_stack, sizeof(trail) * q->trails_possible);
		q->def_trail = 0;
	}
	else
		q->trails = (trail *)realloc(q->trails, sizeof(trail) * q->trails_possible * MULT);

	q->trails_possible *= MULT;
	return 1;
}

static void reclaim_trail(tpl_query *q)
{
	TRACE("reclaim_trail");

	for (unsigned i = 0; i < q->c.trail_size; i++) {
		env *e = &q->envs[q->trails[q->c.trail_point + i]];
		term_heapcheck(e->term);
		e->term = NULL;
		e->context = 0;
	}

	q->c.trail_size = 0;
}

void prepare_frame(tpl_query *q, unsigned frame_size)
{
	TRACE("prepare_frame");
	//q->curr_context = q->c.curr_frame;

	if ((q->c.env_point + frame_size) >= q->envs_used) {
		q->envs_used = q->c.env_point + frame_size;

		while ((q->c.env_point + frame_size) >= q->envs_possible) {
			if (!grow_environment(q))
				return;
		}
	}

	env *e = &q->envs[q->c.env_point];
	e->choices = 0;

	for (unsigned i = 0; i < frame_size; i++, e++) {
		term_heapcheck(e->term);
		e->term = NULL;
		e->context = 0;
	}
}

void allocate_frame(tpl_query *q)
{
	TRACE("allocate_frame");
	q->c.trail_size = 0;

	if ((q->c.trail_point + MAX_FRAME_SIZE) >= q->trails_used) {
		q->trails_used = q->c.trail_point + MAX_FRAME_SIZE;

		while ((q->c.trail_point + MAX_FRAME_SIZE) >= q->trails_possible) {
			if (!grow_trail(q))
				return;
		}
	}

	for (int i = 0; i < NBR_MASKS; i++)
		q->c.mask1[i] = q->c.mask2[i] = 0;

	env *e = &q->envs[q->c.curr_frame];
	mask_t bit = 1;

	for (unsigned i = 0, j = 0, k = 0; k < q->c.frame_size; k++, e++) {
		if (!e->context)
			q->c.mask1[j] |= bit;
		else if (!e->term) {
			env *e2 = e - e->binding;

			if (!e2->context)
				q->c.mask2[j] |= bit;
		}

		if (++i == MAX_MASK_SIZE) {
			j++;
			i = 0;
			bit = 1;
		}
		else
			bit <<= 1;
	}
}

void reallocate_frame(tpl_query *q)
{
	TRACE("reallocate_frame");
	env *e = &q->envs[q->c.curr_frame];
	mask_t bit = 1;

	for (unsigned i = 0, j = 0, k = 0; k < q->c.frame_size; k++, e++) {
		if ((q->c.mask1[j] & bit) && !(q->pins[j] & bit)) {
			term_heapcheck(e->term);
			e->term = NULL;
			e->context = 0;
		}
		else if ((q->c.mask2[j] & bit) && !(q->pins[j] & bit)) {
			env *e = get_env(q, q->c.curr_frame + k);
			term_heapcheck(e->term);
			e->term = NULL;
			e->context = 0;
		}

		if (++i == MAX_MASK_SIZE) {
			j++;
			i = 0;
			bit = 1;
		}
		else
			bit <<= 1;
	}

	reclaim_trail(q);
}

static int proceed(tpl_query *q)
{
	while (q->c.prev_choice) {
		choice *c = &q->choices[q->c.prev_choice];
		q->c.prev_choice = c->prev_choice;

		if (c->curr_term->flags & FLAG_NOFOLLOW)
			continue;

		if (!c->nofollow)
			q->c.curr_term = term_next(c->curr_term);

		for (int i = 0; i < NBR_MASKS; i++) {
			q->c.mask1[i] = c->mask1[i];
			q->c.mask2[i] = c->mask2[i];
		}

		q->c.curr_match = c->curr_match;
		q->curr_context = c->curr_frame;
		q->c.idx_iter = c->idx_iter;
		q->c.curr_db = c->curr_db;
		q->c.curr_frame = c->curr_frame;
		q->c.frame_size = c->frame_size;
		TRACE("proceed");

		if (!q->c.curr_term)
			continue;

		break;
	}

	// if (q->trace && q->c.curr_term)
	//	trace(q, 0, 0);

	return q->c.curr_term ? 1 : 0;
}

static int follow(tpl_query *q)
{
	if (q->c.curr_term->flags & FLAG_NOFOLLOW)
		return proceed(q);

	if (!(q->c.curr_term = term_next(q->c.curr_term)))
		return proceed(q);

	if (is_var(q->c.curr_term))
		q->c.curr_term = subst(q, q->c.curr_term, q->c.curr_frame);

	return 1;
}

void try_me2(tpl_query *q, int nofollow, int nochoice, int transparent)
{
	TRACE("try_me");
	choice *c = &q->choices[q->choice_point];
	*c = q->c;
	c->nofollow = nofollow;
	c->transparent = transparent;
	c->cut = nochoice;
	q->c.prev_choice = q->choice_point++;

#ifdef DEBUG
	g_choicepoints++;
#endif

	q->c.trail_point += q->c.trail_size;
	q->c.trail_size = 0;

	if (q->c.curr_match != NULL)
		if (term_next(q->c.curr_match) != NULL)
			q->envs[q->c.curr_frame].choices++;

	if (q->choice_point <= q->choices_used)
		return;

	if (q->choice_point >= q->choices_possible)
		grow_choice(q);

	q->choices_used = q->choice_point;
}

int retry_me(tpl_query *q)
{
	TRACE("retry_me");

	if (!q->choice_point)
		return 0;

	reclaim_trail(q);
	choice *c = &q->choices[--q->choice_point];

	if (c->curr_match != NULL)
		if (term_next(c->curr_match) != NULL)
			q->envs[c->curr_frame].choices--;

	q->c = *c;
	q->curr_context = q->c.curr_frame;
	reallocate_frame(q);

#ifdef DEBUG
	g_backtracks++;
#endif

	if (c->cut)
		return retry_me(q);

	return q->retry = 1;
}

void trust_me(tpl_query *q)
{
	TRACE("trust_me");
	q->choice_point = q->c.prev_choice + 1;
	choice *c = &q->choices[q->c.prev_choice];
	c->cut = 1;
	q->envs[c->curr_frame].choices = 0;

	if (c->transparent) {
		q->c.prev_choice = c->prev_choice;
		c = &q->choices[q->choice_point-1];
		c->cut = 1;
		return trust_me(q);
	}

#ifdef DEBUG
	g_cuts++;
#endif
}

static void execute_term(tpl_query *q, node *term, unsigned frame_size, unsigned alloc_size)
{
	TRACE("execute_term");
	q->curr_context = q->c.env_point;
	q->c.curr_frame = q->c.env_point;
	q->c.env_point += alloc_size;
	q->c.frame_size = frame_size;
	q->c.curr_term = term;
	q->is_det = 1;

#ifdef DEBUG
	g_executes++;
#endif
}

static void reexecute_term(tpl_query *q, node *term, unsigned frame_size, unsigned alloc_size)
{
	TRACE("reexecute_term");
	env *to = &q->envs[q->c.curr_frame];

	for (unsigned i = 0; i < q->c.frame_size; i++, to++) {
		term_heapcheck(to->term);
		to->term = NULL;
	}

	to = &q->envs[q->c.curr_frame];
	env *from = &q->envs[q->c.env_point];

	for (unsigned i = 0; i < frame_size; i++, to++, from++) {
		to->term = from->term;
		to->choices = from->choices;

		if (from->term != NULL)
			to->context = from->context;
		else if (from->binding != 0)
			; // to->binding = from->binding - q->c.frame_size;
		else
			to->context = 0;

		from->term = NULL;
	}

	q->c.env_point = q->c.curr_frame + alloc_size;
	q->c.frame_size = frame_size;
	q->c.curr_term = term;
	q->is_det = 1;

#ifdef DEBUG
	g_reexecutes++;
#endif
}

static int dynamic(tpl_query *q)
{
	node *n;
	int arity;

	if (is_compound(q->c.curr_term)) {
		node *args = get_args(q);
		n = args;
		arity = get_arity(q);
	}
	else {
		n = q->c.curr_term;
		arity = 0;
	}

	if (is_builtin(n)) {

#ifdef DEBUG
		g_s_resolves++;
#endif

		return n->bifptr(q);
	}

	if (!is_atom(n)) {
		QABORT(ABORT_NOTCALLABLE);
		return 0;
	}

	const char *src = strchr(VAL_S(n), ':');
	const char *functor = src ? src + 1 : VAL_S(n);
	rule *r = xref_term(q->lex, n, arity);

	if ((r == NULL) && functor[0]) {
		char tmpbuf[FUNCTOR_SIZE + 10];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor[0] ? functor : ":", arity);

		if (!sl_get(&q->c.curr_db->rules, tmpbuf, (void **)&r)) {
			printf("ERROR: UNKNOWN -> '%s'\n", tmpbuf);
			q->halt_code = 1;
			q->halt = ABORT_HALT;
			return 0;
		}
	}

	if (r == NULL) {
		printf("ERROR: NOT CALLABLE -> '%s/%d'\n", VAL_S(n), arity);
		q->halt_code = 1;
		q->halt = ABORT_HALT;
		return 0;
	}

#ifdef DEBUG
	g_u_resolves++;
#endif

	q->c.curr_term->match = r;
	return match(q);
}

int call(tpl_query *q)
{
	if (q->trace)
		trace(q, 0, 0);

	int status = 1;

	if (is_builtin(q->c.curr_term)) {
		status = q->c.curr_term->bifptr(q);
		q->latest_context = q->c.curr_frame;

#ifdef DEBUG
		g_s_resolves++;
#endif
	}
	else if (q->c.curr_term->match != NULL) {
		status = match(q);

#ifdef DEBUG
		g_u_resolves++;
#endif
	}
	else if (is_list(q->c.curr_term)) {
		node *head = term_first(q->c.curr_term);

		for (node *n = term_next(head); n != NULL; n = term_next(n)) {
			if (is_atom(n))
				status = trealla_consult_file(q->pl, VAL_S(n));

			n = term_next(n);

			if (is_list(n))
				n = term_first(n);
		}
	}
	else {
		status = dynamic(q);
	}

	q->retry = 0;
	return status;
}

void begin_query(tpl_query *q, node *term)
{
	DEBUGPRINT
	{
		printf("### begin_query ");
		term_print(q->pl, q, term, 1);
		printf("\n");
	}

	q->c.curr_term = term;
}

void run_me(tpl_query *q)
{
	q->is_running++;

	while (!g_abort) {
		if (!call(q)) {
			if (q->is_yielded || q->halt)
				break;

			if (q->trace)
				trace(q, 1, 0);

			if (!retry_me(q)) {
				q->ok = 0;
				break;
			}

			continue;
		}

		if (q->trace)
			trace(q, 0, 1);

		if (!follow(q))
			break;
	}

	q->is_running--;
}

void bind_vars(tpl_query *q, unsigned point1, unsigned point2)
{
	point1 -= q->envs[point1].binding;
	point2 -= q->envs[point2].binding;

	if (point2 >= point1) {
		q->envs[point2].binding = (signed)point2 - (signed)point1;

		if ((point2 < q->c.curr_frame) || (point2 >= (q->c.curr_frame + q->c.frame_size))) {
			q->trails[q->c.trail_point + q->c.trail_size++] = point2;
			q->is_det = 0;
		}
	}
	else {
		q->envs[point1].binding = (signed)point1 - (signed)point2;

		if ((point1 < q->c.curr_frame) || (point1 >= (q->c.curr_frame + q->c.frame_size))) {
			q->trails[q->c.trail_point + q->c.trail_size++] = point1;
			q->is_det = 0;
		}
	}
}

int unify_int(tpl_query *q, node *term, unsigned context, nbr_t v)
{
	node *n = make_quick_int(v);
	int ok = unify(q, term, context, n, -1);
	term_heapcheck(n);
	return ok;
}

int unify_float(tpl_query *q, node *term, unsigned context, flt_t v)
{
	node *n = make_float(v);
	int ok = unify(q, term, context, n, -1);
	term_heapcheck(n);
	return ok;
}

int unify_atom(tpl_query *q, node *term, unsigned context, char *v)
{
	node *n = make_atom(v);

	if (needs_quoting(v))
		n->flags |= FLAG_QUOTED;

	int ok = unify(q, term, context, n, -1);
	term_heapcheck(n);
	return ok;
}

int unify_const_atom(tpl_query *q, node *term, unsigned context, const char *v)
{
	node *n = make_const_atom(v);

	if (needs_quoting(v))
		n->flags |= FLAG_QUOTED;

	int ok = unify(q, term, context, n, -1);
	term_heapcheck(n);
	return ok;
}

static int unify_atomic(tpl_query *q, node *term1, unsigned context1, node *term2, unsigned context2)
{
	DEBUGPRINT
	{
		printf("### unify : ");
		term_print(q->pl, NULL, term1, 1);
		printf(" (%d) <==> (%d) ", context1, context2);
		term_print(q->pl, NULL, term2, 1);
		printf("\n");
	}

	if (is_var(term1)) {
		if (is_var(term2)) {
			bind_vars(q, context1 + term1->slot, context2 + term2->slot);
			return 1;
		}

		if (is_compound(term2))
			q->is_det = 0;

		put_env(q, context1 + term1->slot, term2, is_compound(term2) ? context2 : -1);
		return 1;
	}

	if (is_var(term2)) {
		if (is_compound(term1))
			q->is_det = 0;

		put_env(q, context2 + term2->slot, term1, is_compound(term1) ? context1 : -1);
		return 1;
	}

	if (is_integer(term1) && is_integer(term2))
		return term1->val_i == term2->val_i;

	if (is_float(term1) && is_float(term2))
		return term1->val_f == term2->val_f;

#if USE_SSL
	if (is_bignum(term1) && is_bignum(term2))
		return !BN_cmp(term1->val_bn, term2->val_bn);

	if (is_bignum(term1) && is_integer(term2))
		return BN_get_word(term1->val_bn) == term2->val_i;

	if (is_integer(term1) && is_bignum(term2))
		return term1->val_i == BN_get_word(term2->val_bn);
#endif

#ifndef ISO_ONLY
	if (is_blob(term1) && is_blob(term2)) {
		if (term1->val_len != term2->val_len)
			return 0;

		return !memcmp(term1->val_s, term2->val_s, term2->val_len);
	}
#endif

	if (is_atom(term1) && is_atom(term2)) {
		if (VAL_S(term1) == VAL_S(term2))
			return 1;

		return !strcmp(VAL_S(term1), VAL_S(term2));
	}

	return 0;
}

static int unify_compound(tpl_query *q, node *term1, unsigned context1, node *term2, unsigned context2)
{
	if (term_count(term1) != term_count(term2))
		return 0;

	term1 = term_first(term1);
	term2 = term_first(term2);

	if (VAL_S(term1) != VAL_S(term2))
		if (strcmp(VAL_S(term1), VAL_S(term2)))
			return 0;

	if (++q->unify_depth == MAX_UNIFY_DEPTH) {
		QABORT(ABORT_MAXDEPTH);
		return 0;
	}

	term1 = term_next(term1);
	term2 = term_next(term2);
	int ok = 1;

	while (term1 != NULL) {
		node *tmp1 = subst(q, term1, context1);
		int tmp1_ctx = q->latest_context;
		node *tmp2 = subst(q, term2, context2);
		int tmp2_ctx = q->latest_context;
		q->fail_arg++;

		if (is_compound(tmp1) && is_compound(tmp2))
			ok = unify_compound(q, tmp1, tmp1_ctx, tmp2, tmp2_ctx);
		else
			ok = unify_atomic(q, tmp1, tmp1_ctx, tmp2, tmp2_ctx);

		if (!ok)
			break;

		term1 = term_next(term1);
		term2 = term_next(term2);
	}

	q->unify_depth--;
	return ok;
}

int unify(tpl_query *q, node *term1, unsigned context1, node *term2, unsigned context2)
{
	if (is_compound(term1) && is_compound(term2))
		return unify_compound(q, term1, context1, term2, context2);
	else
		return unify_atomic(q, term1, context1, term2, context2);
}

int match(tpl_query *q)
{
	TRACE("match");
	rule *r = q->c.curr_term->match;

	if (r == NULL)
		return 0;

	if (NLIST_FRONT(&r->val_l) == NULL) {
		if (!r->dynamic)
			printf("ERROR: No rule for '%s'\n", r->functor);

		return 0;
	}

	node *save_match = q->c.curr_match;
	const void *key = NULL;
	int use_iter = 0;

	if (r->idx) {
		node *fa = term_firstarg(q->c.curr_term);
		node *fval = subst(q, fa, q->curr_context);

		if (!is_var(fval)) {
			use_iter = 1;

#ifndef ISO_ONLY
			if (r->numeric) {
				if (!is_integer(fval)) {
					printf("ERROR: index type mismatch\n");
					return 0;
				}

				key = (void *)(size_t)fval->val_i;
			}
			else
#endif
			{
				char tmpbuf[KEY_SIZE];
				key = make_key(q->pl, tmpbuf, fval);
			}

			if (!q->retry)
				q->c.idx_iter = sb_findkey(r->idx, key);

			if (!sb_nextkey(q->c.idx_iter, key, (void **)&q->c.curr_match))
				q->c.curr_match = NULL;
		}
		else if (q->retry) {
			save_match = q->c.curr_match;
			q->c.curr_match = term_next(q->c.curr_match);
		}
		else
			save_match = q->c.curr_match = NLIST_FRONT(&r->val_l);
	}
	else if (q->retry) {
		save_match = q->c.curr_match;
		q->c.curr_match = term_next(q->c.curr_match);
	}
	else
		save_match = q->c.curr_match = NLIST_FRONT(&r->val_l);

	if (!q->retry)
		allocate_frame(q);

	while ((q->c.curr_match != NULL) && !q->halt) {
		if (is_deleted(q->c.curr_match)) {
			if (use_iter) {
				if (!sb_nextkey(q->c.idx_iter, key, (void **)&q->c.curr_match))
					break;
			}
			else
				q->c.curr_match = term_next(q->c.curr_match);

			continue;
		}

		node *term = term_first(q->c.curr_match);
		node *head = term_next(term);

#ifndef ISO_ONLY
		node *save_head = head, *save_head2 = NULL;

		if (is_storage(q->c.curr_match)) {
			node *tmp_arg1 = term_firstarg(head);
			node *tmp_rest = term_next(tmp_arg1);
			save_head2 = dbs_read_entry(q->c.curr_db, tmp_rest->val_i);

			if (save_head2 == NULL) {
				printf("ERROR: accessing %s storage fpos=%lld", q->c.curr_db->name, (long long)tmp_rest->val_i);
				break;
			}

			head = term_firstarg(save_head2);
		}
#endif

		q->unify_depth = q->fail_arg = 0;
		const unsigned frame_size = q->c.curr_match->frame_size;
		unsigned alloc_size;

		if (q->c.curr_match->flags & FLAG_EXPANDABLE) {
			//printf("*** HERE\n");
			alloc_size = MAX_FRAME_SIZE;				// HACK
		}
		else
			alloc_size = frame_size;

		prepare_frame(q, alloc_size);

		DEBUGPRINT
		{
			printf("### match : ");
			term_print(q->pl, NULL, q->c.curr_term, 1);
			printf(" (%u) <==> (%u) ", q->curr_context, q->c.env_point);
			term_print(q->pl, NULL, head, 1);
			printf(" (size %u)\n", frame_size);
		}

		int ok = unify(q, q->c.curr_term, q->c.curr_frame, head, q->c.env_point);

#ifndef ISO_ONLY
		if (head != save_head) {
			term_heapcheck(save_head2);
			head = save_head;
		}
#endif

		if (!ok) {
			reallocate_frame(q);

			// Dynamic and the index doesn't match?

			if (r->dynamic && (q->fail_arg == 1))
				break;

			if (use_iter) {
				if (!sb_nextkey(q->c.idx_iter, key, (void **)&q->c.curr_match))
					q->c.curr_match = NULL;
			}
			else
				q->c.curr_match = term_next(q->c.curr_match);

			continue;
		}

		if (save_match && is_deleted(save_match)) {
			NLIST_REMOVE(&r->val_l, save_match);
			term_heapcheck(save_match);
		}

		node *body = term_next(head);
		int is_cut = body->flags & FLAG_ISCUT;
		int is_lastmatch = (term_next(q->c.curr_match) == NULL) || is_cut;
		int is_lastcall = term_next(q->c.curr_term) == NULL;
		int is_det = q->is_det;

		if ((q->optimize > 1) && is_lastcall && is_lastmatch && is_det) {
			int is_tco = is_tailrecursive(q->c.curr_term) && !q->envs[q->c.curr_frame].choices;

			if ((q->optimize > 2) && is_tco)
				reexecute_term(q, head, frame_size, alloc_size);
			else
				execute_term(q, head, frame_size, alloc_size);
		}
		else {
			try_me(q);
			execute_term(q, head, frame_size, alloc_size);
		}

		return 1;
	}

	if (save_match && is_deleted(save_match)) {
		NLIST_REMOVE(&r->val_l, save_match);
		term_heapcheck(save_match);
	}

	return 0;
}
