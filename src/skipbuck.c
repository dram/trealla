/*
 * This version is unique (as far as I know) in that it adds 'buckets'
 * with binary search within. This allows many more keys to be stored:
 * about 400M 64-bit keys (bucket size 16) on an 8GB system vs 150M
 * with the original. A pathological case exists whereby keys are added
 * in descending order and it reverts to one key per bucket, as per the
 * original. Since buckets are fixed in size, this is bad. The case
 * though is unlikely and can be planned around. For both in-order and
 * random insertion space usage is optimal.
 *
 * Future enhancement: allocate variable sized buckets (requires
 * storing bucket allocation size in sbnode).
 *
 * Follow-on enhancement: when deleting from a bucket, if the number
 * drops below half the bucket size, reallocate bucket at half size.
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG
#include <assert.h>
#endif

#include "skipbuck.h"

typedef struct keyval_ keyval_t;
typedef struct sbnode_ sbnode;

struct keyval_ {
	void *key;
	void *val;
};

#ifndef SKIPBUCK_KEYS
#define SKIPBUCK_KEYS 16
#endif

struct sbnode_ {
	int nbr;
	keyval_t bkt[SKIPBUCK_KEYS];
	sbnode *forward[0];
};

struct skipbuck_ {
	sbnode *header;
	size_t count;
	int level;
	unsigned int seed;
	int (*compkey)(const void *, const void *);
	void *(*copykey)(const void *);
	void (*freekey)(void *);
	void *(*copyval)(const void *);
	void (*freeval)(void *);
};

struct sbiter_ {
	skipbuck *l;
	sbnode *p;
	int idx;
};

#define max_levels 32
#define max_level (max_levels - 1)
#define new_node_of_level(x) (sbnode *)malloc(sizeof(sbnode) + ((x) * sizeof(sbnode *)))

// Allows using integer values as keys...

static int default_compkey(const void *k1, const void *k2)
{
	if ((size_t)k1 < (size_t)k2)
		return -1;
	else if ((size_t)k1 == (size_t)k2)
		return 0;
	else
		return 1;
}

skipbuck *sb_create2(int (*compkey)(const void *, const void *), void *(*copykey)(const void *), void (*freekey)(void *),
                     void *(*copyval)(const void *), void (*freeval)(void *))
{
	if (compkey == NULL)
		compkey = default_compkey;

	skipbuck *l = (skipbuck *)malloc(sizeof(struct skipbuck_));
	l->header = new_node_of_level(max_levels);
	l->level = 1;
	l->seed = (unsigned int)(size_t)l;

	for (int i = 0; i < max_levels; i++)
		l->header->forward[i] = NULL;

	l->header->nbr = 1;
	l->header->bkt[0].key = NULL;
	l->compkey = compkey;
	l->copykey = copykey;
	l->freekey = freekey;
	l->copyval = copyval;
	l->freeval = freeval;
	l->count = 0;
	return l;
}

skipbuck *sb_create(int (*compkey)(const void *, const void *), void *(*copykey)(const void *), void (*freekey)(void *))
{
	return sb_create2(compkey, copykey, freekey, NULL, NULL);
}

void sb_destroy(skipbuck *l)
{
	sbnode *p, *q;

	if (!l || !l->header)
		return;

	p = l->header;
	q = p->forward[0];
	free(p);
	p = q;

	while (p != NULL) {
		q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (l->freekey)
				l->freekey(p->bkt[j].key);

			if (l->freeval)
				l->freeval(p->bkt[j].val);
		}

		free(p);
		p = q;
	}

	free(l);
}

size_t sb_count(const skipbuck *l) { return l->count; }

void sb_dump(const skipbuck *l)
{
	if (!l)
		return;

	sbnode *p, *q;

	if (!l || !l->header)
		return;

	p = l->header;
	p = p->forward[0];

	while (p != NULL) {
		q = p->forward[0];
		printf("%6d: ", p->nbr);

		for (int j = 0; j < p->nbr; j++)
			printf("%llu ", (unsigned long long)(size_t)p->bkt[j].key);
			//printf("%s ", (const char*)p->bkt[j].key);

		printf("\n");
		p = q;
	}

	printf("\n");
}

static int binary_search(const skipbuck *l, const keyval_t n[], const void *key, int imin, int imax)
{
	while (imax >= imin) {
		int imid = (imax + imin) / 2;

		if (l->compkey(n[imid].key, key) == 0)
			return imid;
		else if (l->compkey(n[imid].key, key) < 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	return -1;
}

// Modified binary search: return position where it is or ought to be

static int binary_search1(const skipbuck *l, const keyval_t n[], const void *key, int imin, int imax)
{
	int imid = 0;

	while (imax >= imin) {
		imid = (imax + imin) / 2;

		if (l->compkey(n[imid].key, key) < 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	if (l->compkey(n[imid].key, key) < 0)
		imid++;

	return imid;
}

// Modified binary search: return position where it is or ought to be

static int binary_search2(const skipbuck *l, const keyval_t n[], const void *key, int imin, int imax)
{
	int imid = 0;

	while (imax >= imin) {
		imid = (imax + imin) / 2;

		if (l->compkey(n[imid].key, key) <= 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	if (l->compkey(n[imid].key, key) <= 0)
		imid++;

	return imid;
}

#define frand(seedp) ((double)rand_r(seedp) / RAND_MAX)

static int random_level(unsigned int *seedp)
{
	const double P = 0.5;
	int lvl = (int)(log(frand(seedp)) / log(1. - P));
	return lvl < max_level ? lvl : max_level;
}

int sb_set(skipbuck *l, const void *key, const void *value)
{
#ifdef DEBUG
	assert(l);
#endif

	sbnode *update[max_levels];
	sbnode *p, *q;
	sbnode stash;
	stash.nbr = 0;
	int k;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[0].key, key) < 0))
			p = q;

		update[k] = p;
	}

	if (p != l->header) {
		int imid = binary_search2(l, p->bkt, key, 0, p->nbr - 1);

		if (p->nbr < SKIPBUCK_KEYS) {
			int j;

			// sb_dump(l);
			// printf("SHIFT @ %d\n", imid);

			for (j = p->nbr; j > imid; j--)
				p->bkt[j] = p->bkt[j - 1];

			if (l->copykey)
				p->bkt[j].key = l->copykey(key);
			else
				p->bkt[j].key = (void *)key;

			if (l->copyval)
				p->bkt[j].val = l->copyval(value);
			else
				p->bkt[j].val = (void *)value;

			p->nbr++;
			l->count++;
			// sb_dump(l); printf("\n");
			return 1;
		}

		// Don't drop this unless you are 100% sure:

		while ((imid < p->nbr) && (l->compkey(p->bkt[imid].key, key) == 0))
			imid++;

		if (imid <= SKIPBUCK_KEYS) {
			// sb_dump(l);
			// printf("SPLIT @ %d\n", imid);

			for (int j = imid; j < p->nbr; j++)
				stash.bkt[stash.nbr++] = p->bkt[j];

			p->nbr = imid;
			// sb_dump(l); printf("\n");
		}
	}

	k = random_level(&l->seed);

	if (k >= l->level) {
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k + 1);

	if (l->copykey)
		q->bkt[0].key = l->copykey(key);
	else
		q->bkt[0].key = (void *)key;

	if (l->copyval)
		q->bkt[0].val = l->copyval(value);
	else
		q->bkt[0].val = (void *)value;

	q->nbr = 1;
	l->count++;

	if (stash.nbr) {
		for (int i = 0; i < stash.nbr; i++, q->nbr++)
			q->bkt[q->nbr] = stash.bkt[i];
	}

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	// sb_dump(l); printf("\n");
	return 1;
}

int sb_app(skipbuck *l, const void *key, const void *value)
{
#ifdef DEBUG
	assert(l);
#endif

	sbnode *update[max_levels];
	sbnode *p, *q;
	sbnode stash;
	stash.nbr = 0;
	int k;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[0].key, key) <= 0))
			p = q;

		update[k] = p;
	}

	if (p != l->header) {
		int imid = binary_search2(l, p->bkt, key, 0, p->nbr - 1);

		if (p->nbr < SKIPBUCK_KEYS) {
			int j;

			// sb_dump(l);
			// printf("SHIFT @ %d\n", imid);

			for (j = p->nbr; j > imid; j--)
				p->bkt[j] = p->bkt[j - 1];

			if (l->copykey)
				p->bkt[j].key = l->copykey(key);
			else
				p->bkt[j].key = (void *)key;

			if (l->copyval)
				p->bkt[j].val = l->copyval(value);
			else
				p->bkt[j].val = (void *)value;

			p->nbr++;
			l->count++;
			// sb_dump(l); printf("\n");
			return 1;
		}

		// Don't drop this unless you are 100% sure:

		while ((imid < p->nbr) && (l->compkey(p->bkt[imid].key, key) == 0))
			imid++;

		if (imid <= SKIPBUCK_KEYS) {
			// sb_dump(l);
			// printf("SPLIT @ %d\n", imid);

			for (int j = imid; j < p->nbr; j++)
				stash.bkt[stash.nbr++] = p->bkt[j];

			p->nbr = imid;
			// sb_dump(l); printf("\n");
		}
	}

	k = random_level(&l->seed);

	if (k >= l->level) {
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k + 1);

	if (l->copykey)
		q->bkt[0].key = l->copykey(key);
	else
		q->bkt[0].key = (void *)key;

	if (l->copyval)
		q->bkt[0].val = l->copyval(value);
	else
		q->bkt[0].val = (void *)value;

	q->nbr = 1;
	l->count++;

	if (stash.nbr) {
		for (int i = 0; i < stash.nbr; i++, q->nbr++)
			q->bkt[q->nbr] = stash.bkt[i];
	}

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	// sb_dump(l); printf("\n");
	return 1;
}

int sb_get(const skipbuck *l, const void *key, const void **value)
{
#ifdef DEBUG
	assert(l);
#endif

	int k;
	sbnode *p, *q = 0;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key) < 0))
			p = q;
	}

	q = p->forward[0];

	if (q == NULL)
		return 0;

	int imid = binary_search(l, q->bkt, key, 0, q->nbr - 1);
	// printf("GET: %llu @ %d\n", (unsigned long long)key, imid);

	if (imid < 0)
		return 0;

	*value = q->bkt[imid].val;
	return 1;
}

int sb_del(skipbuck *l, const void *key)
{
#ifdef DEBUG
	assert(l);
#endif

	int k, m;
	sbnode *update[max_levels];
	sbnode *p, *q;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key) < 0))
			p = q;

		update[k] = p;
	}

	q = p->forward[0];

	if (q == NULL)
		return 0;

	int imid = binary_search(l, q->bkt, key, 0, q->nbr - 1);

	if (imid < 0)
		return 0;

	// printf("DEL: %llu @ %d\n", (unsigned long long)key, imid);
	// sb_dump(l); printf("\n");

	if (l->freekey)
		l->freekey(q->bkt[imid].key);

	if (l->freeval)
		l->freeval(q->bkt[imid].val);

	while (imid < (q->nbr - 1)) {
		q->bkt[imid] = q->bkt[imid + 1];
		imid++;
	}

	q->nbr--;
	l->count--;
	// sb_dump(l); printf("\n");

	if (q->nbr)
		return 1;

	// printf("DEL empty\n");

	m = l->level - 1;

	for (k = 0; k <= m; k++) {
		p = update[k];

		if ((p == NULL) || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	free(q);
	m = l->level - 1;

	while ((l->header->forward[m] == NULL) && (m > 0))
		m--;

	l->level = m + 1;
	return 1;
}

int sb_erase(skipbuck *l, const void *key, const void *value, int (*compkey)(const void *, const void *))
{
#ifdef DEBUG
	assert(l);
#endif

	if (!compkey)
		compkey = default_compkey;

	int k, m;
	sbnode *update[max_levels];
	sbnode *p, *q = NULL;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key) < 0))
			p = q;

		update[k] = p;
	}

	q = p->forward[0];

	if (q == NULL)
		return 0;

	int imid, done = 0;

	for (imid = 0; imid < q->nbr; imid++) {

		if (l->compkey(q->bkt[imid].key, key) < 0)
			continue;

		if (l->compkey(q->bkt[imid].key, key) == 0) {
			if (compkey(q->bkt[imid].val, value) != 0)
				continue;

			done = 1;
		}

		break;
	}

	if (!done)
		return 0;

	// printf("DEL: %llu @ %d\n", (unsigned long long)key, imid);
	// sb_dump(l); printf("\n");

	if (l->freekey)
		l->freekey(q->bkt[imid].key);

	if (l->freeval)
		l->freeval(q->bkt[imid].val);

	while (imid < (q->nbr - 1)) {
		q->bkt[imid] = q->bkt[imid + 1];
		imid++;
	}

	q->nbr--;
	l->count--;
	// sb_dump(l); printf("\n");

	if (q->nbr)
		return 1;

	// printf("DEL empty\n");

	m = l->level - 1;

	for (k = 0; k <= m; k++) {
		p = update[k];

		if ((p == NULL) || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	free(q);
	m = l->level - 1;

	while ((l->header->forward[m] == NULL) && (m > 0))
		m--;

	l->level = m + 1;
	return 1;
}

int sb_efface(skipbuck *l, const void *value, int (*compkey)(const void *, const void *))
{
#ifdef DEBUG
	assert(l);
#endif

	if (!compkey)
		compkey = default_compkey;

	int k, m, imid = -1;
	sbnode *update[max_levels];
	sbnode *p, *q;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		int done = 0;

		while ((q = p->forward[k]) != NULL) {
			int j;

			for (j = 0; j < q->nbr; j++) {
				if (compkey(q->bkt[j].val, value) == 0)
					done = 1;
			}

			if (done) {
				imid = j;
				break;
			}

			p = q;
		}

		update[k] = p;
	}

	q = p->forward[0];

	if (q == NULL)
		return 0;

	// printf("DEL: %llu @ %d\n", (unsigned long long)key, imid);
	// sb_dump(l); printf("\n");

	if (l->freekey)
		l->freekey(q->bkt[imid].key);

	if (l->freeval)
		l->freeval(q->bkt[imid].val);

	while (imid < (q->nbr - 1)) {
		q->bkt[imid] = q->bkt[imid + 1];
		imid++;
	}

	q->nbr--;
	l->count--;
	// sb_dump(l); printf("\n");

	if (q->nbr)
		return 1;

	// printf("DEL empty\n");

	m = l->level - 1;

	for (k = 0; k <= m; k++) {
		p = update[k];

		if ((p == NULL) || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	free(q);
	m = l->level - 1;

	while ((l->header->forward[m] == NULL) && (m > 0))
		m--;

	l->level = m + 1;
	return 1;
}

void sb_iter(const skipbuck *l, int (*f)(void *, const void *, const void *), void *p1)
{
#ifdef DEBUG
	assert(l && f);
#endif

	sbnode *p;
	p = l->header;
	p = p->forward[0];

	while (p != NULL) {
		sbnode *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (!f(p1, p->bkt[j].key, p->bkt[j].val))
				return;
		}

		p = q;
	}
}

void sb_find(const skipbuck *l, const void *key, int (*f)(void *, const void *, const void *), void *p1)
{
#ifdef DEBUG
	assert(l && f);
#endif

	if (!f)
		return;

	sbnode *p, *q = 0;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key) < 0))
			p = q;
	}

	q = p->forward[0];

	if (q == NULL)
		return;

	int imid = binary_search2(l, q->bkt, key, 0, q->nbr - 1);

	if (imid < 0)
		return;

	p = q;

	for (int j = imid; j < p->nbr; j++) {
		if (!f(p1, p->bkt[j].key, p->bkt[j].val))
			return;
	}

	while (p != NULL) {
		sbnode *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (!f(p1, p->bkt[j].key, p->bkt[j].val))
				return;
		}

		p = q;
	}
}

sbiter *sb_findkey(const skipbuck *l, const void *key)
{
#ifdef DEBUG
	assert(l && key);
#endif

	sbnode *p, *q = 0;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key) < 0))
			p = q;
	}

	q = p->forward[0];

	if (q == NULL)
		return NULL;

	int imid = binary_search1(l, q->bkt, key, 0, q->nbr - 1);

	if (imid < 0)
		return NULL;

	if (l->compkey(q->bkt[imid].key, key) != 0)
		return NULL;

	//printf("*** %s (%d/%d)\n", (const char*)key, imid, q->nbr);  sb_dump(l); printf("\n");

	sbiter *iter = malloc(sizeof(sbiter));
	iter->l = (skipbuck*)l;
	iter->p = q;
	iter->idx = imid;
	return iter;
}

int sb_nextkey(sbiter *iter, const void *key, void **value)
{
	if (!iter)
		return 0;

	if (!iter->p)
		return 0;

#ifdef DEBUG
	assert(key);
#endif

	if (iter->idx < iter->p->nbr) {
		if (iter->l->compkey(iter->p->bkt[iter->idx].key, key) != 0) {
			//free(iter);
			return 0;
		}

		*value = iter->p->bkt[iter->idx++].val;
		return 1;
	}

	iter->p = iter->p->forward[0];
	iter->idx = 0;

	if (iter->p)
		return sb_nextkey(iter, key, value);

	return 0;
}
