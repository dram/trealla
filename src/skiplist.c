#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef DEBUG
#include <assert.h>
#endif

#include "skiplist.h"

struct slnode_ {
	char *key;
	void *value;
	slnode *forward[0];
};

#define max_levels 16
#define max_level (max_levels - 1)
#define new_node_of_level(n) (slnode *)malloc(sizeof(slnode) + ((n) * sizeof(slnode *)))
#define frand(seedp) ((double)rand_r(seedp) / RAND_MAX)

static int random_level(unsigned int *seedp)
{
	const double P = 0.5;
	int lvl = (int)(log(frand(seedp)) / log(1.0 - P));
	return lvl < max_level ? lvl : max_level;
}

static int defcmp(const char *s1, const char *s2)
{
	if ((long long)(size_t)s1 < (long long)(size_t)s2)
		return -1;

	return (long long)(size_t)s1 > (long long)(size_t)s2;
}

int sl_count(skiplist *d)
{
#ifdef DEBUG
	assert(d);
#endif

	return d->cnt;
}

void sl_init(skiplist *d, int dups, int (*compare)(const char *, const char *), void (*deleter)(void *))
{
#ifdef DEBUG
	assert(d);
#endif

	d->header = new_node_of_level(max_levels);
	d->level = 1;
	d->dups = dups;
	d->compare = compare ? compare : defcmp;
	d->deleter = deleter;
	d->cnt = 0;
	d->iter = NULL;
	d->seed = (unsigned int)(size_t)(d + clock());

	for (int i = 0; i < max_levels; i++)
		d->header->forward[i] = NULL;

	d->header->key = NULL;
}

int sl_set(skiplist *d, const char *key, void *value)
{
#ifdef DEBUG
	assert(d);
#endif

	slnode *update[max_levels], *p = d->header, *q = NULL;
	int k;

	for (k = d->level; k >= 0; k--) {
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;

		update[k] = p;
	}

	if (!d->dups) {
		if (q && (d->compare(q->key, key) == 0)) {
			if (d->deleter)
				d->deleter((char *)key);

			return 0;
		}
	}

	k = random_level(&d->seed);

	if (k >= d->level) {
		d->level++;
		k = d->level - 1;
		update[k] = d->header;
	}

	q = new_node_of_level(k + 1);
	q->key = (char *)key;
	q->value = value;

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	d->cnt++;
	return 1;
}

int sl_del(skiplist *d, const char *key, void **value)
{
#ifdef DEBUG
	assert(d);
#endif

	slnode *update[max_levels], *p = d->header, *q = NULL;
	int k;

	for (k = d->level; k >= 0; k--) {
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;

		update[k] = p;
	}

	q = p->forward[0];

	if (!q)
		return 0;

	if (d->compare(q->key, key) != 0)
		return 0;

	if (d->deleter)
		d->deleter(q->key);

	if (value)
		*value = q->value;

	for (k = 0; k <= d->level; k++) {
		p = update[k];

		if (!p || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	int m = d->level;

	while (!d->header->forward[m] && (m > 0))
		m--;

	d->level = m;
	d->cnt--;
	free(q);
	return 1;
}

int sl_get(skiplist *d, const char *key, void **value)
{
#ifdef DEBUG
	assert(d);
#endif

	slnode *p = d->header, *q = NULL;

	for (int k = d->level; k >= 0; k--) {
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;
	}

	q = p->forward[0];

	if (!q)
		return 0;

	if (d->compare(q->key, key) != 0)
		return 0;

	if (value)
		*value = q->value;

	return 1;
}

void sl_start(skiplist *d)
{
#ifdef DEBUG
	assert(d);
#endif

	d->iter = d->header->forward[0];
}

void sl_find(skiplist *d, const char *key)
{
#ifdef DEBUG
	assert(d);
#endif

	d->iter = d->header->forward[0];
	slnode *p = d->header, *q = NULL;

	for (int k = d->level; k >= 0; k--) {
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;
	}

	q = p->forward[0];

	if (!q)
		return;

	if (d->compare(q->key, key) >= 0)
		d->iter = q;
	else
		d->iter = NULL;
}

const char *sl_next(skiplist *d, void **value)
{
#ifdef DEBUG
	assert(d);
#endif

	if (d->iter == NULL)
		return NULL;

	if (value)
		*value = d->iter->value;

	const char *key = d->iter->key;
	d->iter = d->iter->forward[0];
	return key;
}

slnode *sl_startx(skiplist *d)
{
#ifdef DEBUG
	assert(d);
#endif

	return d->header->forward[0];
}

const char *sl_nextx(slnode **iter, void **value)
{
	if (*iter == NULL)
		return NULL;

	if (value)
		*value = (*iter)->value;

	const char *key = (*iter)->key;
	*iter = (*iter)->forward[0];
	return key;
}

slnode *sl_findkey(skiplist *d, const char *key)
{
#ifdef DEBUG
	assert(d);
#endif

	slnode *p = d->header, *q = NULL;

	for (int k = d->level; k >= 0; k--) {
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;
	}

	q = p->forward[0];
	return q;
}

int sl_nextkey(slnode **iter, const char *key, void **value)
{
	if (*iter == NULL)
		return 0;

	if (strcmp((*iter)->key, key) != 0)
		return 0;

	if (value)
		*value = (*iter)->value;

	*iter = (*iter)->forward[0];
	return 1;
}

void sl_done(skiplist *d, void (*deleter)(void *))
{
	if (!d)
		return;

	if (!d->header)
		return;

	slnode *p = d->header, *q;
	q = p->forward[0];
	free(p);
	p = q;

	while (p != NULL) {
		q = p->forward[0];

		if (d->deleter)
			d->deleter(p->key);

		if (deleter)
			deleter(p->value);
		free(p);
		p = q;
	}

	d->header = NULL;
	d->cnt = 0;
}

void sl_clear(skiplist *d, void (*deleter)(void *))
{
	if (!d)
		return;

	sl_done(d, deleter);
	sl_init(d, d->dups, d->compare, d->deleter);
}
