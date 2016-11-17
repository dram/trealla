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
 * storing bucket allocation size in slnode).
 *
 * Follow-on enhancement: when deleting from a bucket, if the number
 * drops below half the bucket size, reallocate bucket at half size.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "skipbuck.h"

typedef struct keyval_ keyval_t;
typedef struct slnode_ slnode;

struct keyval_
{
	void*	key;
	void*	val;
};

#ifndef SKIPBUCK_KEYS
#define SKIPBUCK_KEYS 16
#endif

struct slnode_
{
	int			nbr;
	keyval_t	bkt[SKIPBUCK_KEYS];
	slnode		*forward[0];
};

struct skipbuck_
{
	slnode	*header;
	size_t	count;
	int		level;
	int		(*compare)(const void*, const void*);
	void*	(*copykey)(const void*);
	void	(*freekey)(void*);
	void*	(*copyval)(const void*);
	void	(*freeval)(void*);
};

#define max_levels 32
#define max_level (max_levels-1)
#define new_node_of_level(x) (slnode*)malloc(sizeof(slnode)+((x)*sizeof(slnode*)))

// Allows using integer values as keys...

static int default_compare(const void *k1, const void *k2)
{
	if ((size_t)k1 < (size_t)k2)
		return -1;
	else if ((size_t)k1 == (size_t)k2)
		return 0;
	else
		return 1;
}

skipbuck *sb_create2(int (*compare)(const void*, const void*), void *(*copykey)(const void*), void (*freekey)(void*), void *(*copyval)(const void*), void (*freeval)(void*))
{
	skipbuck *l;

	if (compare == NULL)
		compare = default_compare;

	l = (skipbuck*)malloc(sizeof(struct skipbuck_));
	l->level = 1;
	l->header = new_node_of_level(max_levels);

	for (int i = 0; i < max_levels; i++)
		l->header->forward[i] = NULL;

	l->header->nbr = 1;
	l->header->bkt[0].key = NULL;
	l->compare = compare;
	l->copykey = copykey;
	l->freekey = freekey;
	l->copyval = copyval;
	l->freeval = freeval;
	l->count = 0;
	return l;
}

skipbuck *sb_create(int (*compare)(const void*, const void*), void *(*copykey)(const void*), void (*freekey)(void*))
{
	return sb_create2(compare, copykey, freekey, NULL, NULL);
}

void sb_destroy(skipbuck *l)
{
	slnode *p, *q;

	if (!l || !l->header)
		return;

	p = l->header;
	q = p->forward[0];
	free(p);
	p = q;

	while (p != NULL)
	{
		q = p->forward[0];

		for (int j = 0; j < p->nbr; j++)
		{
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

size_t sb_count(const skipbuck *l)
{
	return l->count;
}

void sb_dump(const skipbuck *l)
{
	slnode *p, *q;

	if (!l || !l->header)
		return;

	p = l->header;
	p = p->forward[0];

	while (p != NULL)
	{
		q = p->forward[0];
		printf("%d: ", p->nbr);

		for (int j = 0; j < p->nbr; j++)
			printf("%llu ", (unsigned long long)(size_t)p->bkt[j].key);

		printf("\n");
		p = q;
	}

	printf("\n");
}

static int binary_search(const skipbuck *l, const keyval_t n[], const void *key, int imin, int imax)
{
	int imid = 0;

	while (imax >= imin)
	{
		imid = (imax + imin) / 2;

		if (l->compare(n[imid].key, key) == 0)
			return imid;
		else if (l->compare(n[imid].key, key) < 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	return -1;
}

// Modified binary search: return position where it is or ought to be

static int binary_search2(const skipbuck *l, const keyval_t n[], const void *key, int imin, int imax)
{
	int imid = 0;

	while (imax >= imin)
	{
		imid = (imax + imin) / 2;

		if (l->compare(n[imid].key, key) < 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	if (l->compare(n[imid].key, key) < 0)
		imid++;

	return imid;
}

#define frand() ((double)rand() / RAND_MAX)

static int random_level()
{
	const double P = 0.5;
	int lvl = (int)(log(frand())/log(1.-P));
	return lvl < max_level ? lvl : max_level;
}

int sb_set(skipbuck *l, const void *key, const void *value)
{
	slnode *update[max_levels];
	slnode *p, *q;
	slnode stash;
	stash.nbr = 0;
	int k;

	p = l->header;

	for (int k = l->level-1; k >= 0; k--)
	{
		 while ((q = p->forward[k]) && (l->compare(q->bkt[0].key, key) <= 0))
			p = q;

		 update[k] = p;
	}

	if (p != l->header)
	{
		int imid = binary_search2(l, p->bkt, key, 0, p->nbr-1);

		if (p->nbr < SKIPBUCK_KEYS)
		{
			int j;

			//sb_dump(l);
			//printf("SHIFT @ %d\n", imid);

			for (j = p->nbr; j > imid; j--)
				p->bkt[j] = p->bkt[j-1];

			if (l->copykey)
				p->bkt[j].key = l->copykey(key);
			else
				p->bkt[j].key = (void*)key;

			if (l->copyval)
				p->bkt[j].val = l->copyval(value);
			else
				p->bkt[j].val = (void*)value;

			p->nbr++;
			l->count++;
			//sb_dump(l); printf("\n");
			return 1;
		}

		// Don't drop this unless you are 100% sure:

		while ((imid < p->nbr) && (l->compare(p->bkt[imid].key, key) == 0))
			imid++;

		if (imid <= SKIPBUCK_KEYS)
		{
			//sb_dump(l);
			//printf("SPLIT @ %d\n", imid);

			for (int j = imid; j < p->nbr; j++)
				stash.bkt[stash.nbr++] = p->bkt[j];

			p->nbr = imid;
			//sb_dump(l); printf("\n");
		}
	}

	k = random_level();

	if (k >= l->level)
	{
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k+1);

	if (l->copykey)
		q->bkt[0].key = l->copykey(key);
	else
		q->bkt[0].key = (void*)key;

	if (l->copyval)
		q->bkt[0].val = l->copyval(value);
	else
		q->bkt[0].val = (void*)value;

	q->nbr = 1;
	l->count++;

	if (stash.nbr)
	{
		for (int i = 0; i < stash.nbr; i++, q->nbr++)
			q->bkt[q->nbr] = stash.bkt[i];
	}

	for (int i = 0; i < k; i++)
		q->forward[i] = NULL;

	for (; k >= 0; k--)
	{
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	//sb_dump(l); printf("\n");
	return 1;
}

int sb_get(const skipbuck *l, const void *key, const void **value)
{
	int k;
	slnode *p, *q = 0;
	p = l->header;

	for (k = l->level-1; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (l->compare(q->bkt[q->nbr-1].key, key) < 0))
			p = q;
	}

	if (q == NULL)
		return 0;

	int imid = binary_search(l, q->bkt, key, 0, q->nbr-1);
	//printf("GET: %llu @ %d\n", (unsigned long long)key, imid);

	if (imid < 0)
		return 0;

	*value = q->bkt[imid].val;
	return 1;
}

int sb_del(skipbuck *l, const void *key)
{
	int k, m;
	slnode *update[max_levels];
	slnode *p, *q;

	p = l->header;

	for (k = l->level-1; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (l->compare(q->bkt[0].key, key) < 0))
			p = q;

		update[k] = p;
	}

	q = p->forward[0];

	if (q == NULL)
		return 0;

	int imid = binary_search(l, q->bkt, key, 0, q->nbr-1);

	if (imid < 0)
		return 0;

	//printf("DEL: %llu @ %d\n", (unsigned long long)key, imid);
	//sb_dump(l); printf("\n");

	if (l->freekey)
		l->freekey(q->bkt[imid].key);

	if (l->freeval)
		l->freeval(q->bkt[imid].val);

	while (imid < (q->nbr-1))
	{
		q->bkt[imid] = q->bkt[imid+1];
		imid++;
	}

	q->nbr--;
	l->count--;
	//sb_dump(l); printf("\n");

	if (q->nbr)
		return 1;

	//printf("DEL empty\n");

	m = l->level - 1;

	for (k = 0; k <= m; k++)
	{
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

int sb_erase(skipbuck *l, const void *key, const void *value, int (*compare)(const void*,const void*))
{
	if (!compare)
		compare = default_compare;

	int k, m;
	slnode *update[max_levels];
	slnode *p, *q;
	p = l->header;

	for (k = l->level-1; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (l->compare(q->bkt[0].key, key) < 0))
			p = q;

		update[k] = p;
	}

	q = p->forward[0];

	if (q == NULL)
		return 0;

	int imid, done = 0;

	for (imid = 0; imid < q->nbr; imid++)
	{
		if (l->compare(q->bkt[imid].key, key) < 0)
			continue;

		if (!compare(q->bkt[imid].val, value))
		{
			done = 1;
			break;
		}
	}

	if (!done)
		return 0;

	//printf("DEL: %llu @ %d\n", (unsigned long long)key, imid);
	//sb_dump(l); printf("\n");

	if (l->freekey)
		l->freekey(q->bkt[imid].key);

	if (l->freeval)
		l->freeval(q->bkt[imid].val);

	while (imid < (q->nbr-1))
	{
		q->bkt[imid] = q->bkt[imid+1];
		imid++;
	}

	q->nbr--;
	l->count--;
	//sb_dump(l); printf("\n");

	if (q->nbr)
		return 1;

	//printf("DEL empty\n");

	m = l->level - 1;

	for (k = 0; k <= m; k++)
	{
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

int sb_efface(skipbuck *l, const void *value, int (*compare)(const void*,const void*))
{
	if (!compare)
		compare = default_compare;

	int k, m, imid = -1;
	slnode *update[max_levels];
	slnode *p, *q;
	p = l->header;

	for (k = l->level-1; k >= 0; k--)
	{
		int done = 0;

		while ((q = p->forward[k]) != NULL)
		{
			int j;

			for (j = 0; j < q->nbr; j++)
			{
				if (!compare(q->bkt[j].val, value))
					done = 1;
			}

			if (done)
			{
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

	//printf("DEL: %llu @ %d\n", (unsigned long long)key, imid);
	//sb_dump(l); printf("\n");

	if (l->freekey)
		l->freekey(q->bkt[imid].key);

	if (l->freeval)
		l->freeval(q->bkt[imid].val);

	while (imid < (q->nbr-1))
	{
		q->bkt[imid] = q->bkt[imid+1];
		imid++;
	}

	q->nbr--;
	l->count--;
	//sb_dump(l); printf("\n");

	if (q->nbr)
		return 1;

	//printf("DEL empty\n");

	m = l->level - 1;

	for (k = 0; k <= m; k++)
	{
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

void sb_iter(const skipbuck *l, int (*f)(void*,const void*,const void*), void *p1)
{
	slnode *p;
	p = l->header;
	p = p->forward[0];

	while (p != NULL)
	{
		slnode *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++)
		{
			if (!f(p1, p->bkt[j].key, p->bkt[j].val))
				return;
		}

		p = q;
	}
}

void sb_find(const skipbuck *l, const void *key, int (*f)(void*,const void*,const void*), void *p1)
{
	if (!f)
		return;

	slnode *p, *q = 0;

	p = l->header;

	for (int k = l->level-1; k >= 0; k--)
	{
		while ((q = p->forward[k]) && (l->compare(q->bkt[q->nbr-1].key, key) < 0))
			p = q;
	}

	if (q == NULL)
		return;

	int imid = binary_search2(l, q->bkt, key, 0, q->nbr-1);

	if (imid < 0)
		return;

	p = q;

	for (int j = imid; j < p->nbr; j++)
	{
		if (!f(p1, p->bkt[j].key, p->bkt[j].val))
			return;
	}

	while (p != NULL)
	{
		slnode *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++)
		{
			if (!f(p1, p->bkt[j].key, p->bkt[j].val))
				return;
		}

		p = q;
	}
}

