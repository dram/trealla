#ifndef UNCLE_H
#define UNCLE_H

#define UNCLE_MCAST_ADDR_IPV4 239.192.0.66
#define UNCLE_DEFAULT_PORT 6199

typedef struct uncle_ uncle;

#define SCOPE_DEFAULT "DEFAULT"

// Create using an internal handler and thread for manual
// control of resources. See network.h for how to create
// automatically an *uncle* that self-manages resources.

extern uncle *uncle_create(const char *binding, unsigned port, const char *scope, const char *maddr6, const char *maddr4);
extern void uncle_destroy(uncle *u);

extern const char *uncle_get_scope(uncle *u);

// Add, remove ephemeral resources.
// Resources are named and there can be duplicates, but
// the combination of name/addr/port/tcp is unique.

extern int uncle_add(uncle *u, const char *name, const char *addr, unsigned port, int tcp, int ssl, int priority);
extern int uncle_rem(uncle *u, const char *name, const char *addr, int tcp);

// Query for named resource.
// A datagram service will normally be returned before a tcp one.
// Set tcp and/or ssl to >= 0 to refine search.
// Set addr to a buffer to receive the address string.

extern int uncle_query(uncle *u, const char *name, char *addr, unsigned *port, int *tcp, int *ssl, int *priority);

#endif
