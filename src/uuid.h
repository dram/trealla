#ifndef UUID_H
#define UUID_H

#include <stdint.h>

typedef struct uuid_ { uint64_t u1, u2; } uuid;

extern char *uuid_to_string(const uuid *u, char *buf);
extern uuid *uuid_from_string(const char*, uuid *u);
extern uint64_t uuid_ts(const uuid *u);
extern void uuid_seed(uint64_t v);		// unique 48-bits eg. MAC-address or rand
extern uuid *uuid_gen(uuid *u);
extern uuid uuid_set(uint64_t, uint64_t);		// used for testing only
extern int uuid_compare(const uuid *v1, const uuid *v2);
extern uuid *uuid_copy(const uuid *v);
extern int uuid_is_zero(const uuid *u);

#endif
