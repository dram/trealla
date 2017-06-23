#ifndef NETWORK_H
#define NETWORK_H

#include "thread.h"
#include <time.h>

typedef struct session_ session;
typedef struct handler_ handler;

extern const char *hostname(void);

extern session *session_create(void);
extern session *session_open(const char *host, unsigned port, int tcp, int ssl);

extern int session_on_connect(session *s);
extern int session_on_disconnect(session *s);

extern int session_is_server(session *s);
extern int session_is_client(session *s);
extern int session_is_priority(session *s);
extern int session_is_tls(session *s);
extern int session_is_udp(session *s);
extern int session_is_tcp(session *s);
extern int session_is_ipv4(session *s);
extern int session_is_ipv6(session *s);
extern int session_is_nonblocking(session *s);
extern int session_is_websocket(session *s);

extern int session_getfd(session *s);

// With UDP this will enable multicast sending.
// LOOP = 1 (the default) allow loopback to same host.
// HOPS = 0 same host only, 1 (the default) same sub-net etc.

extern int session_enable_multicast(session *s, int loop, int ttl);
extern int session_enable_broadcast(session *s);
extern int session_enable_tls(session *s, const char *certfile, int level);

extern int session_set_nonblocking(session *s, int mode);
extern int session_set_nodelay(session *s, int mode);
extern int session_set_cork(session *s, int mode);
extern int session_set_quickack(session *s, int mode);
extern int session_set_sndbuffer(session *s, int bufsize);
extern int session_set_rcvbuffer(session *s, int bufsize);
extern int session_set_websocket(session *s);

extern const char *session_get_local_addr(session *s, int /*resolve*/);
extern const char *session_get_remote_addr(session *s, int /*resolve*/);
extern unsigned session_get_local_port(session *s);
extern unsigned session_get_remote_port(session *s);

extern int session_rawwrite(session *s, const void *buf, size_t len);
extern int session_write(session *s, const void *buf, size_t len);
extern int session_writemsg(session *s, const char *buf);

extern int session_bcast(session *s, const void *buf, size_t len);
extern int session_bcastmsg(session *s, const char *buf);

// Read returns >0 on data being read.
// Read returns 0 otherwise (if non-blocking)

extern int session_read(session *s, void *buf, size_t len);

// Reads up to at least 64KB message size.
// Readmsg returns 1 on a complete message being read.
// Readmsg returns 0 otherwise (if non-blocking)
// You MUST dispose of returned buffer!

extern int session_readmsg(session *s, char **dst);

extern time_t session_get_lasttime(session *s);

extern void session_clr_udata_flags(session *s);
extern void session_clr_udata_flag(session *s, unsigned flag);
extern void session_set_udata_flag(session *s, unsigned flag); // flag=0..63
extern unsigned session_get_udata_flag(session *s, unsigned flag);

extern void session_set_udata_int(session *s, int64_t data);
extern int64_t session_get_udata_int(session *s);

extern void session_set_udata_ptr(session *s, void *data);
extern void *session_get_udata_ptr(session *s);

// There is a limit of 1000 stash items per session
// Note: the stash takes copies of key/value strings
extern int session_set_stash(session *s, const char *key, const char *value);
extern const char *session_get_stash(session *s, const char *key);
extern int session_set_stash_int(session *s, const char *key, int64_t value);
extern int64_t session_get_stash_int(session *s, const char *key);
extern int session_set_stash_float(session *s, const char *key, double value);
extern double session_get_stash_float(session *s, const char *key);
// Note: must free returned value after use...
extern const char *session_del_stash(session *s, const char *key);
extern void session_clr_stash(session *s);
extern size_t session_count_stash(session *s);

extern const char *session_get_name(session *s);

#define WS_OP_TEXT 0x1
#define WS_OP_BINARY 0x2
#define WS_OP_CLOSE 0x8
#define WS_OP_PING 0x9
#define WS_OP_PONG 0xA
#define WS_GUID "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

extern unsigned int *get_seed(session *s);
extern int session_ws_parse(session *s, int *fin, unsigned *opcode, char **dst, size_t *dstlen);
extern int ws_msg(session *s, unsigned fin, unsigned opcode, const char *src, size_t srclen);

inline static int session_ws_close(session *s, const char *src, size_t len) { return ws_msg(s, 1, WS_OP_CLOSE, src, len); }

inline static int session_ws_ping(session *s, const char *src, size_t len) { return ws_msg(s, 1, WS_OP_PING, src, len); }

inline static int session_ws_pong(session *s, const char *src, size_t len) { return ws_msg(s, 1, WS_OP_PONG, src, len); }

inline static int session_ws_data(session *s, int fin, int binary, const char *src, size_t len)
{
	return ws_msg(s, fin ? 1 : 0, (binary ? WS_OP_BINARY : WS_OP_TEXT), src, len);
}

extern void session_lock(session *s);   // handler-wide lock will
extern void session_unlock(session *s); // block all other sessions

extern void session_share(session *s);
extern void session_unshare(session *s);

extern int session_close(session *s);

// Handlers use thread-pools to manage sessions asynchronously.
// Such sockets are set non-blocking.
// Note: data available does not mean a complete message is
// available for processing.
// If 'threads' is -1 add your own thread-pool.

extern handler *handler_create(int threads);

extern int handler_set_tls(handler *h, const char *keyfile, const char *certfile);
extern int handler_set_ca(handler *h, const char *cafile, const char *capath);

// Add an application discovery layer.

extern int handler_add_uncle(handler *h, const char *binding, unsigned port, const char *scope);

// Server sessions are created and disposed of automatically by the
// connection handler, accessing the user-supplied callback function.
// Callbacks happen in-line if 'threads' is zero.
// Specify 'tcp' to enable TCP streams or UDP datagrams.
// Use 'ssl' to immediately enable TLS (SSL is not supported).

// If name is not NULL then it is added to the uncle as a named service.
// If port = 0 then one is assigned, can be located with the uncle

extern int handler_add_tpool(handler *h, tpool *tp);
extern int handler_add_multicast(handler *h, int (*f)(session *, void *data), void *data, const char *binding, unsigned port,
                                 const char *maddr6, const char *maddr4, const char *name);
extern int handler_add_server(handler *h, int (*f)(session *, void *data), void *data, const char *binding, unsigned port,
                              int tcp, int ssl, int priority, const char *name);
extern int handler_add_client(handler *h, int (*f)(session *, void *data), void *data, session *s);

// There is where the action occurs. It will not return until
// there are no more sockets to monitor unless 'wait=1'.

extern int handler_wait_indefinitely(handler *h);
extern int handler_wait(handler *h);
extern int handler_shutdown(handler *h);
extern void handler_destroy(handler *h);

#endif
