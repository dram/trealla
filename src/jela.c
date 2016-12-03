#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

#include "trealla.h"
#include "internal.h"
#include "bifs.h"
#include "jela.h"

#define DEBUG if (0)

#define get_index(q,slot) (slot - q->envs[slot].binding)

static void bind_arg(const tpl_query *q, unsigned slot1, unsigned slot2)
{
	unsigned index1 = get_index(q, slot1);
	unsigned index2 = get_index(q, slot2);

	if (index2 >= index1)
	{
		env *e = &q->envs[index2];
		e->binding = index2 - index1;
	}
	else
	{
		env *e = &q->envs[index1];
		e->binding = index1 - index2;
	}
}

int grow_env_stack(tpl_query *q)
{
	//printf("INFO: ### grow_env_stack\n");

	if ((sizeof(env)*q->envs_possible*2)>(1024ULL*1024*g_trealla_memlimit_mb))
		{ QABORT(ABORT_MAXENVS); return 0; }

	if (q->def_env)
	{
		q->envs = (env*)calloc(1, sizeof(env)*q->envs_possible*2);
		memcpy(q->envs, q->env_stack, sizeof(env)*q->envs_possible);
		q->def_env = 0;
	}
	else
	{
		q->envs = (env*)realloc(q->envs, sizeof(env)*q->envs_possible*2);
		memset(&q->envs[q->envs_possible], 0, sizeof(env)*q->envs_possible);
	}

	q->envs_possible *= 2;
	return 1;
}

static int grow_choice_stack(tpl_query *q)
{
	//printf("INFO: ### grow_choice_stack\n");

	if ((sizeof(choice)*q->choices_possible*2)>(1024ULL*1024*g_trealla_memlimit_mb))
		{ QABORT(ABORT_MAXCHOICES); return 0; }

	if (q->def_choice)
	{
		q->choices = (choice*)calloc(1, sizeof(choice)*q->choices_possible*2);
		memcpy(q->choices, q->choice_stack, sizeof(choice)*q->choices_possible);
		q->def_choice = 0;
	}
	else
		q->choices = (choice*)realloc(q->choices, sizeof(choice)*q->choices_possible*2);

	q->choices_possible *= 2;
	return 1;
}

void prepare_frame(tpl_query *q, unsigned frame_size)
{
	DEBUG printf("### prepare_frame size=%u, curr_choice=%u, env_point=%u\n", frame_size, q->curr_choice, q->env_point);

	q->curr_context = q->curr_frame;

	while ((q->env_point+frame_size) >= q->envs_possible)
	{
		if (!grow_env_stack(q))
			return;
	}

	for (unsigned i = 0; i < frame_size; i++)
	{
		env *e = &q->envs[q->env_point+i];

		if (e->term)
		{
			term_heapcheck(e->term);
			e->term = NULL;
		}

		e->binding = 0;
	}
}

#define TRACE(s) DEBUG printf("###[ %s ] ==> size=%d, choice_point=%u, curr_choice=%u, env_point=%u\n", s, q->frame_size, q->choice_point, q->curr_choice, q->env_point);

void allocate_frame(tpl_query *q)
{
	TRACE("allocate_frame");
	g_allocates++;
	choice *c = &q->choices[q->choice_point];
	mask_t *mptr = &c->mask;
	mask_t *mptr2 = &c->mask2;
	*mptr = *mptr2 = 0;
	mask_t mask = 1;

	for (unsigned i = 0; i < q->frame_size; i++)
	{
		env *e = get_env(q, q->curr_frame+i);

		if (!e->context)				// must be reset
			*mptr |= mask;

		e = &q->envs[q->curr_frame+i];

		if (!e->context)				// must be reset
			*mptr2 |= mask;

		mask <<= 1;
	}
}

void reallocate_frame(tpl_query *q)
{
	TRACE("reallocate_frame");
	g_reallocates++;
	choice *c = &q->choices[q->choice_point];
	mask_t *mptr = &c->mask;
	mask_t *mptr2 = &c->mask2;
	mask_t mask = 1;

	for (unsigned i = 0; i < q->frame_size; i++)
	{
		if (*mptr & mask)				// need to reset?
		{
			env *e = get_env(q, q->curr_frame+i);

			if (e->term)
			{
				term_heapcheck(e->term);
				e->term = NULL;
			}

			e->binding = 0;
		}

		if (*mptr2 & mask)				// need to reset?
		{
			env *e = &q->envs[q->curr_frame+i];
			e->binding = 0;
		}

		mask <<= 1;
	}
}

void deallocate_frame(tpl_query *q)
{
	TRACE("deallocate_frame");
	g_deallocates++;
	unsigned new_envs_point = q->curr_frame + q->frame_size;

	if (new_envs_point == q->env_point)
		return;

	for (unsigned i = 0; i < q->frame_size; i++)
	{
		env *e = get_env(q, q->curr_frame+i);

		// If context beyond this frame keep it

		if (e->context >= (signed)new_envs_point)
			return;
	}

	// Otherwise trim

	q->env_point = new_envs_point;
}

static int proceed(tpl_query *q)
{
	do
	{
		TRACE("proceed");

		if (!q->curr_choice)
			break;

		unsigned last_frame = q->curr_frame;
		choice *c = &q->choices[q->curr_choice];
		q->curr_choice = c->previous;
		q->curr_context = c->curr_frame;
		q->curr_match = c->curr_match;
		q->idx_iter = c->idx_iter;
		q->curr_db = c->curr_db;
		q->curr_term = NLIST_NEXT(c->curr_term);
		q->curr_frame = c->curr_frame;
		q->frame_size = c->frame_size;

		// Can trim the frame?

		if ((last_frame > (q->curr_frame+q->frame_size)) && !q->noopt)
			deallocate_frame(q);

		if (c->nofollow)
			q->curr_term = NULL;
	}
	 while (q->curr_term == NULL);

	return q->curr_term != NULL;
}

int follow(tpl_query *q)
{
	TRACE("follow");

	if (q->curr_term->flags & FLAG_NOFOLLOW)
		q->curr_term = NULL;
	else
		q->curr_term = NLIST_NEXT(q->curr_term);

	if (q->curr_term == NULL)
		return proceed(q);
	else if (!is_callable(q->curr_term))
		{ QABORT(ABORT_NOTCALLABLE); return 0; }


	return 1;
}

void try_me2(tpl_query *q, int nofollow, int nochoice)
{
	TRACE("try_me");
	choice *c = &q->choices[q->choice_point];
	c->previous = q->curr_choice;
	c->curr_match = q->curr_match;
	c->idx_iter = q->idx_iter;
	c->curr_db = q->curr_db;
	c->curr_term = q->curr_term;
	c->curr_frame = q->curr_frame;
	c->frame_size = q->frame_size;
	c->env_point = q->env_point;
	c->nofollow = nofollow;
	c->cut = nochoice;
	q->curr_choice = q->choice_point;
	q->is_det = 0;
	g_choicepoints++;

	if (++q->choice_point <= q->choices_used)
		return;

	if (q->choice_point == q->choices_possible)
		grow_choice_stack(q);

	q->choices_used = q->choice_point;
}

int retry_me(tpl_query *q)
{
	TRACE("retry_me");

	if (!q->choice_point)
		return 0;

	choice *c = &q->choices[--q->choice_point];
	if (c->cut) return retry_me(q);
	q->curr_context = c->curr_frame;
	q->curr_match = c->curr_match;
	q->idx_iter = c->idx_iter;
	q->curr_db = c->curr_db;
	q->curr_term = c->curr_term;
	q->curr_frame = c->curr_frame;
	q->frame_size = c->frame_size;
	q->env_point = c->env_point;
	q->curr_choice = c->previous;
	q->is_det = 0;

	reallocate_frame(q);
	g_backtracks++;
	return q->retry = 1;
}

void trust_me(tpl_query *q)
{
	TRACE("trust_me");
	q->choice_point = q->curr_choice+1;
	choice *c = &q->choices[q->curr_choice];
	c->cut = 1;
	g_cuts++;
}

void execute_term(tpl_query *q, node *term, unsigned frame_size)
{
	TRACE("execute_term");
	g_executes++;
	q->curr_term = term;
	q->curr_context = q->curr_frame = q->env_point;
	q->env_point += q->frame_size = frame_size;
	q->is_det = 1;

	if (q->env_point > q->envs_used)
		q->envs_used = q->env_point;
}

static void reexecute_term(tpl_query *q, node *term, unsigned frame_size)
{
	TRACE("reexecute_term");

	if (q->env_point != q->curr_frame)
	{
		env *e_from = &q->envs[q->env_point];
		env *e_to = &q->envs[q->curr_frame];

		for (unsigned i = 0; i < frame_size; i++)
		{
			if (e_to->term)
			{
				if (e_to->term)
					term_heapcheck(e_to->term);
			}

			if (e_from->term)
			{
				e_to->term = e_from->term;
				e_to->context = e_from->context;
				e_from->term = NULL;
			}
			else if (e_from->binding)
			{
				e_to->term = NULL;
				e_to->binding = e_from->binding - (q->env_point - q->curr_frame);
			}
			else
			{
				e_to->term = NULL;
				e_to->binding = 0;
			}

			e_from++;
			e_to++;
		}
	}

	q->curr_term = term;
	q->curr_context = q->curr_frame;
	q->frame_size = frame_size;
	q->env_point = q->curr_frame + frame_size;
	g_reexecutes++;

	if (q->env_point > q->envs_used)
		q->envs_used = q->env_point;
}

static int unify_compound(tpl_query *q, node *term1, node *term2, unsigned frame)
{
	//{ printf("### unify_compound : "); print_term(q->pl, q, term1, 1); print_term(q->pl, NULL, term2, 1); printf("\n"); }

	if (NLIST_COUNT(&term1->val_l) != NLIST_COUNT(&term2->val_l))
		return 0;

	node *it1 = NLIST_FRONT(&term1->val_l);
	node *it2 = NLIST_FRONT(&term2->val_l);

	if (term1->match)
	{
		it1 = NLIST_NEXT(it1);	// skip functor
		it2 = NLIST_NEXT(it2);	//	...
	}

	int this_context = q->curr_context;

	while (it1)
	{
		q->fail_arg++;
		node *tmp1 = get_arg(q, it1, this_context);
		q->curr_context = q->latest_context;
		node *tmp2 = get_arg(q, it2, frame);

		if (q->unify_depth++ > q->max_depth)
			q->max_depth = q->unify_depth;

		int ok = unify_term(q, tmp1, tmp2, q->latest_context);
		q->unify_depth--;
		if (!ok) return 0;

		it1 = NLIST_NEXT(it1);
		it2 = NLIST_NEXT(it2);
	}

	return 1;
}

int unify_term(tpl_query *q, node *term1, node *term2, unsigned frame)
{
	//{ printf("### unify_term : "); print_term(q->pl, q, term1, 1); printf(" <==> "); print_term(q->pl, NULL, term2, 1); printf("\n"); }

	if (q->unify_depth > MAX_UNIFY_DEPTH) { QABORT(ABORT_MAXDEPTH); return 0; }

	if (is_compound(term1) && is_compound(term2))
		return unify_compound(q, term1, term2, frame);

	if (is_var(term1))
	{
		if (!is_var(term2))
		{
			put_env(q, q->curr_context+term1->slot, term2, is_compound(term2)?frame:-1);
			return 1;
		}

		//printf("curr_context=%u, slot1=%u, frame=%u, slot2=%u\n", q->curr_context, term1->slot, frame, term2->slot);

		if (q->curr_context+term1->slot <= frame+term2->slot)
			bind_arg(q, q->curr_context+term1->slot, frame+term2->slot);
		else
			bind_arg(q, frame+term2->slot, q->curr_context+term1->slot);

		return 1;
	}

	if (is_var(term2))
	{
		put_env(q, frame+term2->slot, term1, is_compound(term1)?q->curr_context:-1);
		return 1;
	}

	if (is_integer(term1) && is_integer(term2))
		return term1->val_i == term2->val_i;

	if (is_float(term1) && is_float(term2))
		return term1->val_f == term2->val_f;

	if (is_atom(term1) && is_atom(term2))
	{
		if (term1->val_s == term2->val_s) return 1;
		return !strcmp(term1->val_s, term2->val_s);
	}

	return 0;
}

int match(tpl_query *q)
{
	rule *r = q->curr_term->match;
	if (!r) return 0;

	if (q->retry && q->curr_match)
	{
		if (r->dynamic)
		{
			node *term = NLIST_FRONT(&q->curr_term->val_l);		// Get the functor
			node *fa = NLIST_NEXT(term);						// ... first arg
			node *fval = get_arg(q, fa, q->curr_context);

			if (!is_var(fval))
			{
				node *match;

				if (!sl_nextx(&q->idx_iter, (void**)&match))
					return 0;

				q->curr_match = match;
			}
			else
				q->curr_match = NLIST_NEXT(q->curr_match);
		}
		else
			q->curr_match = NLIST_NEXT(q->curr_match);
	}
	else
	{
		if (r->dynamic)
		{
			node *term = NLIST_FRONT(&q->curr_term->val_l);		// Get the functor
			node *fa = NLIST_NEXT(term);						// ... first arg
			node *fval = get_arg(q, fa, q->curr_context);

			if (!is_var(fval))
			{
				char tmpbuf[KEY_SIZE];
				const char *key = make_key(q->pl, tmpbuf, fval);
				q->idx_iter = sl_findx(&r->idx, key);
				node *match;

				if (!sl_nextx(&q->idx_iter, (void**)&match))
					return 0;

				q->curr_match = match;
			}
			else
				q->curr_match = NLIST_FRONT(&r->clauses);
		}
		else
			q->curr_match = NLIST_FRONT(&r->clauses);

		allocate_frame(q);
	}

	while (q->curr_match && !q->halt)
	{
		if (q->curr_match->flags & FLAG_DELETED)
		{
			q->curr_match = NLIST_NEXT(q->curr_match);
			continue;
		}

		node *term = NLIST_FRONT(&q->curr_match->val_l);
		node *head = NLIST_NEXT(term);
		node *save_head = head;

#ifndef ISO_ONLY
		if (q->curr_match->flags & FLAG_DBS_STORAGE)
		{
			node *tmp_fa = NLIST_NEXT(NLIST_FRONT(&head->val_l));
			node *tmp_rest = NLIST_NEXT(tmp_fa);
			head = dbs_read_entry(q->curr_db, tmp_rest->val_i);
		}
#endif

		unsigned frame_size = q->curr_match->frame_size;
		prepare_frame(q, frame_size);

		//{ printf("### match : "); print_term(q->pl, q, q->curr_term, 1); printf(" <==> "); print_term(q->pl, NULL, head, 1); printf("\n"); }

		g_match_try++;
		q->max_depth = q->unify_depth = q->fail_arg = 0;

		if (!unify_term(q, q->curr_term, head, q->env_point))
		{
			if (head != save_head)
			{
				term_heapcheck(head);
				head = save_head;
			}

			if (!NLIST_NEXT(q->curr_match))
				break;

			// Dynamic and the index doesn't match?

			if (r->dynamic && (q->fail_arg == 1))
				break;

			q->curr_match = NLIST_NEXT(q->curr_match);
			reallocate_frame(q);
			continue;
		}

		if (head != save_head)
		{
			term_heapcheck(head);
			head = save_head;
		}

		node *body = NLIST_NEXT(head);
		g_match_ok++;
		int is_lastmatch = !NLIST_NEXT(q->curr_match);
		int is_lastcall = !NLIST_NEXT(q->curr_term) || (body->flags & FLAG_CUT);

		if (!is_lastcall || q->noopt || q->is_det)
			try_me(q);
		else if (!is_lastmatch)
			try_me_nofollow(q);

		if (is_lastcall && is_lastmatch && !q->noopt)
		{
			int is_complex = !q->is_det || (q->max_depth > 1);

			if (!is_complex &&
				((q->curr_frame+frame_size) >= q->env_point))
			{
				reexecute_term(q, head, frame_size);
				return 1;
			}
		}

		execute_term(q, head, frame_size);
		return 1;
	}

	return 0;
}

static int dynamic(tpl_query *q)
{
	int status = 0;
	node *n;
	int arity;

	if (is_compound(q->curr_term))
	{
		n = get_args(q);
		arity = get_arity(q);
	}
	else
	{
		n = q->curr_term;
		arity = 0;
	}

	const char *functor = n->val_s;
	char tmpbuf2[FUNCTOR_SIZE+10];
	const char *src = strchr(functor, ':');

	if (src)
	{
		memcpy(tmpbuf2, functor, src-functor);
		tmpbuf2[src-functor] = '\0';
		functor = src+1;
		sl_get(&q->pl->mods, tmpbuf2, (void**)&q->lex->db);
	}

	//printf("DEBUG: dynamic %s/%d\n", tmp->val_s, arity);

	rule *r = xref_term(q->lex, n, arity);

	if (is_builtin(n))
	{
		g_s_resolves++;
		return n->bifptr(q);
	}

	if (r == NULL)
	{
		char tmpbuf[FUNCTOR_SIZE+10];

		if (!strchr(functor, ARITY_CHAR))
		{
			snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);
			functor = tmpbuf;
		}

		if (!sl_get(&q->curr_db->rules, functor, (void**)&r))
		{
			printf("ERROR: UNKNOWN -> '%s'\n", functor);
			QABORT(ABORT_NOTDYNAMIC);
			return 0;
		}

		if (0 && !r->dynamic && !strchr(functor, ':'))
		{
			printf("ERROR: NOT DYNAMIC '%s'\n", functor);
			QABORT(ABORT_NOTDYNAMIC);
			return 0;
		}
	}

	if (r != NULL)
	{
		q->curr_term->match = r;
		status = match(q);
		g_u_resolves++;
	}

	return status;
}

int call(tpl_query *q)
{
	int status = 0;

	if (is_builtin(q->curr_term))
	{
		status = q->curr_term->bifptr(q);
		g_s_resolves++;
	}
	else if (q->curr_term->match)
	{
		status = match(q);
		g_u_resolves++;
	}
	else if (is_list(q->curr_term))
	{
		node *head = NLIST_FRONT(&q->curr_term->val_l);

		for (node *n = NLIST_NEXT(head); n; n = NLIST_NEXT(n))
		{
			if (is_atom(n))
				trealla_consult_file(q->pl, n->val_s);

			n = NLIST_NEXT(n);

			if (is_list(n))
				n = NLIST_FRONT(&n->val_l);
		}
	}
	else
	{
		status = dynamic(q);
	}

	q->retry = 0;
	return q->ok = status;
}

void begin_query(tpl_query *q, node *term)
{
	DEBUG { printf("### begin_query "); print_term(q->pl, q, term, 1); printf("\n"); }
	q->curr_term = term;
}

