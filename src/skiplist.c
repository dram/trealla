#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "skiplist.h"

struct slnode_
{
	char *key;
	void *value;
	slnode *forward[0];
};

#define max_levels 16
#define max_level (max_levels-1)
#define new_node_of_level(n) (slnode*)malloc(sizeof(slnode)+((n)*sizeof(slnode*)))
#define frand() ((double)rand()/RAND_MAX)

static int random_level(void)
{
	const double P = 0.5;
	int lvl = (int)(log(frand())/log(1.0-P));
	return lvl < max_level ? lvl : max_level;
}

static int defcmp(const char *s1, const char *s2)
{
	if ((long long)(size_t)s1 < (long long)(size_t)s2)
		return -1;

	return (long long)(size_t)s1 > (long long)(size_t)s2;
}

void sl_init(skiplist *d, int dups, int (*compare)(const char*, const char*), void (*deleter)(void*))
{
	d->header = new_node_of_level(max_levels);
	d->level = 1;
	d->dups = dups;
	d->compare = compare ? compare : defcmp;
	d->deleter = deleter;
	d->cnt = 0;
	d->iter = NULL;

	for (int i = 0; i < max_levels; i++)
		d->header->forward[i] = NULL;

	d->header->key = NULL;
}

int sl_set(skiplist *d, const char *key, void *value)
{
	slnode *update[max_levels], *p = d->header, *q = NULL;
	int k;

	for (k = d->level; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;

		update[k] = p;
	}

	if (!d->dups)
	{
		if (q && (d->compare(q->key, key) == 0))
			return 0;
	}

	k = random_level();

	if (k >= d->level)
	{
		d->level++;
		k = d->level - 1;
		update[k] = d->header;
	}

	q = new_node_of_level(k+1);
	q->key = (char*)key;
	q->value = value;

	for (int i = 0; i < k; i++)
		q->forward[i] = NULL;

	for (; k >= 0; k--)
	{
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	d->cnt++;
	return 1;
}

int sl_app(skiplist *d, const char *key, void *value)
{
	if (!d->dups)
		return 0;

	slnode *update[max_levels], *p = d->header, *q = NULL;
	int k;

	for (k = d->level; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (d->compare(q->key, key) <= 0))
			p = q;

		update[k] = p;
	}

	k = random_level();

	if (k >= d->level)
	{
		d->level++;
		k = d->level - 1;
		update[k] = d->header;
	}

	q = new_node_of_level(k+1);
	q->key = (char*)key;
	q->value = value;

	for (int i = 0; i < k; i++)
		q->forward[i] = NULL;

	for (; k >= 0; k--)
	{
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	d->cnt++;
	return 1;
}

int sl_del(skiplist *d, const char *key, void **value)
{
	slnode *update[max_levels], *p = d->header, *q = NULL;
	int k;

	for (k = d->level; k >= 0; k--)
	{
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

	for (k = 0; k <= d->level; k++)
	{
		p = update[k];

		if ((p == NULL) || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	int m = d->level;

	while ((d->header->forward[m] == NULL) && (m > 0))
		m--;

	d->level = m;
	d->cnt--;
	free(q);
	return 1;
}

int sl_rem(skiplist *d, const char *key, void *value)
{
	slnode *update[max_levels], *p = d->header, *q = NULL;
	int k;

	for (k = d->level; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;

		update[k] = p;
	}

	for (;;)
	{
		q = p->forward[0];

		if (!q)
			return 0;

		if (d->compare(q->key, key) != 0)
			return 0;

		if (q->value == value)
			break;

		p = q;
	}

	if (d->deleter)
		d->deleter(q->key);

	for (k = 0; k <= d->level; k++)
	{
		p = update[k];

		if ((p == NULL) || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	int m = d->level;

	while ((d->header->forward[m] == NULL) && (m > 0))
		m--;

	d->level = m;
	d->cnt--;
	free(q);
	return 1;
}

int sl_get(skiplist *d, const char *key, void **value)
{
	slnode *p = d->header, *q = NULL;

	for (int k = d->level; k >= 0; k--)
	{
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
	d->iter = d->header->forward[0];
}

void sl_find(skiplist *d, const char *key)
{
	d->iter = d->header->forward[0];
	slnode *p = d->header, *q = NULL;

	for (int k = d->level; k >= 0; k--)
	{
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
	if (d->iter == NULL) return NULL;
	if (value) *value = d->iter->value;
	const char *key = d->iter->key;
	d->iter = d->iter->forward[0];
	return key;
}

slnode *sl_startx(skiplist *d)
{
	return d->header->forward[0];
}

slnode *sl_findx(skiplist *d, const char *key)
{
	slnode *iter = d->header->forward[0];
	slnode *p = d->header, *q = NULL;

	for (int k = d->level; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (d->compare(q->key, key) < 0))
			p = q;
	}

	q = p->forward[0];

	if (!q)
		return NULL;

	if (d->compare(q->key, key) >= 0)
		iter = q;
	else
		iter = NULL;

	return iter;
}

const char *sl_nextx(slnode **iter, void **value)
{
	if (*iter == NULL) return NULL;
	if (value) *value = (*iter)->value;
	const char *key = (*iter)->key;
	*iter = (*iter)->forward[0];
	return key;
}

void sl_done(skiplist *d, void (*deleter)(void*))
{
	if (!d->header) return;
	slnode *p = d->header, *q;
	q = p->forward[0];
	free(p);
	p = q;

	while (p != NULL)
	{
		q = p->forward[0];
		if (d->deleter) d->deleter(p->key);
		if (deleter) deleter(p->value);
		free(p);
		p = q;
	}

	d->header = NULL;
	d->cnt = 0;
}

void sl_clear(skiplist *d, void (*deleter)(void*))
{
	sl_done(d, deleter);
	sl_init(d, d->dups, d->compare, d->deleter);
}
