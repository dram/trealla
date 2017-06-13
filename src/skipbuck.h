#ifndef SKIPBUCK_H
#define SKIPBUCK_H

#include <string.h>

typedef struct skipbuck_ skipbuck;
typedef struct sbiter_ sbiter;

extern skipbuck *sb_create(int (*compare)(const void *, const void *), void *(*copykey)(const void *), void (*freekey)(void *));
extern skipbuck *sb_create2(int (*compare)(const void *, const void *), void *(*copykey)(const void *), void (*freekey)(void *),
                            void *(*copyval)(const void *), void (*freeval)(void *));
extern int sb_set(skipbuck *s, const void *key, const void *value);
extern int sb_app(skipbuck *s, const void *key, const void *value);
extern int sb_get(const skipbuck *s, const void *key, const void **value);
extern int sb_del(skipbuck *s, const void *key);
extern int sb_erase(skipbuck *s, const void *key, const void *value, int (*compare)(const void *, const void *));
extern int sb_efface(skipbuck *s, const void *value, int (*compare)(const void *, const void *));
extern void sb_iter(const skipbuck *s, int (*callback)(void *p1, const void *key, const void *value), void *p1);
extern void sb_find(const skipbuck *s, const void *key, int (*callback)(void *p1, const void *key, const void *value),
                    void *p1);
extern sbiter *sb_findkey(const skipbuck *s, const void *key);
extern int sb_nextkey(sbiter *iter, const void *key, void **value);
extern size_t sb_count(const skipbuck *s);
extern void sb_dump(const skipbuck *s);
extern void sb_destroy(skipbuck *s);

// Specialized variant:
//
// sb_int_create - int key, int value
// sb_int_create2 - int key, string value

#define sb_int_create() sb_create(NULL, NULL, NULL)
#define sb_int_create2() sb_create2(NULL, NULL, NULL, (void *(*)(const void *)) & strdup, &free)
#define sb_int_set(s, k, v) sb_set(s, (const void *)(size_t)k, (const void *)(size_t)v)
#define sb_int_app(s, k, v) sb_app(s, (const void *)(size_t)k, (const void *)(size_t)v)
#define sb_int_get(s, k, v) sb_get(s, (const void *)(size_t)k, (const void **)v)
#define sb_int_del(s, k) sb_del(s, (const void *)(size_t)k)
#define sb_int_erase(s, k, v, f) sb_erase(s, (const void *)(size_t)k, (const void *)v, (int (*)(const void *, const void *))f)
#define sb_int_efface(s, v, f) sb_efface(s, (const void *)v, (int (*)(const void *, const void *))f)
#define sb_int_iter(s, f, a) sb_iter(s, (int (*)(void *, const void *, const void *))f, (void *)a)
#define sb_int_find(s, k, f, a) sb_find(s, (const void *)(size_t)k, (int (*)(void *, const void *, const void *))f, (void *)a)

// Specialized variant:
//
// sb_string_create - string key, int value
// sb_string_create2 - string key, string value

#define sb_string_create() sb_create((int (*)(const void *, const void *)) & strcmp, (void *(*)(const void *)) & strdup, &free)
#define sb_string_create2()                                                                                                    \
	sb_create2((int (*)(const void *, const void *)) & strcmp, (void *(*)(const void *)) & strdup, &free,                      \
	           (void *(*)(const void *)) & strdup, &free)
#define sb_string_set(s, k, v) sb_set(s, (const void *)k, (const void *)v)
#define sb_string_app(s, k, v) sb_app(s, (const void *)k, (const void *)v)
#define sb_string_get(s, k, v) sb_get(s, (const void *)k, (const void **)v)
#define sb_string_del(s, k) sb_del(s, (const void *)k)
#define sb_string_erase(s, k, v, f) sb_erase(s, (const void *)k, (const void *)v, (int (*)(const void *, const void *))f)
#define sb_string_efface(s, v, f) sb_efface(s, (const void *)v, (int (*)(const void *, const void *))f)
#define sb_string_iter(s, f, a) sb_iter(s, (int (*)(void *, const void *, const void *))f, (void *)a)
#define sb_string_find(s, k, f, a) sb_find(s, (const void *)k, (int (*)(void *, const void *, const void *))f, (void *)a)

// Specialized variant:
//
// sb_int_uuid_create - int key, uuid value
// sb_int_uuid_create2 - int key, uuid value

#define sb_int_uuid_create() sb_create(NULL, NULL, NULL)
#define sb_int_uuid_create2() sb_create2(NULL, NULL, NULL, (void *(*)(const void *)) & uuid_copy, &free)
#define sb_int_uuid_set(s, k, v) sb_set(s, (const void *)(size_t)k, (const void *)(size_t)v)
#define sb_int_uuid_app(s, k, v) sb_app(s, (const void *)(size_t)k, (const void *)(size_t)v)
#define sb_int_uuid_get(s, k, v) sb_get(s, (const void *)(size_t)k, (const void **)v)
#define sb_int_uuid_del(s, k) sb_del(s, (const void *)(size_t)k)
#define sb_int_uuid_erase(s, k, v)                                                                                             \
	sb_erase(s, (const void *)(size_t)k, (const void *)v, (int (*)(const void *, const void *)) & uuid_compare)
#define sb_int_uuid_efface(s, v) sb_efface(s, (const void *)v, (int (*)(const void *, const void *)) & uuid_compare)
#define sb_int_uuid_iter(s, f, a) sb_iter(s, (int (*)(void *, const void *, const void *))f, (void *)a)
#define sb_int_uuid_find(s, k, f, a)                                                                                           \
	sb_find(s, (const void *)(size_t)k, (int (*)(void *, const void *, const void *))f, (void *)a)

// Specialized variant:
//
// sb_string_uuid_create - string key, uuid value
// sb_string_uuid_create2 - string key, uuid value

#define sb_string_uuid_create()                                                                                                \
	sb_create((int (*)(const void *, const void *)) & strcmp, (void *(*)(const void *)) & strdup, &free)
#define sb_string_uuid_create2()                                                                                               \
	sb_create2((int (*)(const void *, const void *)) & strcmp, (void *(*)(const void *)) & strdup, &free,                      \
	           (void *(*)(const void *)) & uuid_copy, &free)
#define sb_string_uuid_set(s, k, v) sb_set(s, (const void *)k, (const void *)v)
#define sb_string_uuid_app(s, k, v) sb_app(s, (const void *)k, (const void *)v)
#define sb_string_uuid_get(s, k, v) sb_get(s, (const void *)k, (const void **)v)
#define sb_string_uuid_del(s, k) sb_del(s, (const void *)k)
#define sb_string_uuid_erase(s, k, v)                                                                                          \
	sb_erase(s, (const void *)k, (const void *)v, (int (*)(const void *, const void *)) & uuid_compare)
#define sb_string_uuid_efface(s, v) sb_efface(s, (const void *)v, (int (*)(const void *, const void *)) & uuid_compare)
#define sb_string_uuid_iter(s, f, a) sb_iter(s, (int (*)(void *, const void *, const void *))f, (void *)a)
#define sb_string_uuid_find(s, k, f, a) sb_find(s, (const void *)k, (int (*)(void *, const void *, const void *))f, (void *)a)

#define sb_uuid_efface(s, v) sb_efface(s, (const void *)v, (int (*)(const void *, const void *)) & uuid_compare)

#endif
