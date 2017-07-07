#ifndef SKIPLIST_H
#define SKIPLIST_H

typedef struct slnode_ slnode;
typedef struct skiplist_ skiplist;

// Define struct so can do static or stack allocation, or malloc
// it to create on the heap. Use sl_init' then to prepare.

struct skiplist_ {
	slnode *header, *iter;
	int (*compare)(const char *, const char *);
	void (*deleter)(void *);
	int level;
	unsigned int seed;
	size_t cnt;
};

// For string keys use &strcmp as the key compare function.
// For integer keys use NULL as the key compare function.
// Otherwise supply your own

// For string keys use &free or NULL as the key deleter function.
// For integer keys use NULL as the key deleter function.
// Otherwise supply your own

extern void sl_init(skiplist *d, int (*compare)(const char *, const char *), void (*deleter)(void *));
extern int sl_set(skiplist *d, const char *key, void *value);
extern int sl_app(skiplist *d, const char *key, void *value);
extern int sl_get(skiplist *d, const char *key, void **value);
extern int sl_del(skiplist *d, const char *key, void **value);
extern int sl_count(skiplist *d);

// There can only be one iteration outstanding.
extern void sl_start(skiplist *d);
extern void sl_find(skiplist *d, const char *key); // >= key
extern const char *sl_next(skiplist *d, void **value);

// These create an iterator on demand
extern slnode *sl_startx(skiplist *d);
extern const char *sl_nextx(slnode **iter, void **value);

extern slnode *sl_findkey(skiplist *d, const char *key);
extern int sl_nextkey(slnode **iter, const char *key, void **value);

// For string values use &free or NULL as the value deleter function.
// For integer values use NULL as the value deleter function.
// Otherwise supply your own

extern void sl_clear(skiplist *d, void (*deleter)(void *));
extern void sl_done(skiplist *d, void (*deleter)(void *));

#endif
