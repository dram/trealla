// The handler will select the optimal mechanism for the platform:
//
//		BSD				- kqueue
//		Linux 			- epoll
//		Posix/Vista+	- poll
//		Other/XP		- select
//
// Note: server sockets are non-blocking, client sockets blocking.
// Note: there are no plans for IOCPs on Windows.

#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#define close closesocket
#define ioctl ioctlsocket
#define poll WSAPoll
#define msleep Sleep
#define strcasecmp _stricmp
#define SHUT_RD SD_RECEIVE
#define SHUT_WR SD_SEND
#define SHUT_RDWR SD_BOTH
#define MSG_NOSIGNAL 0
#define MSG_MORE MSG_PARTIAL
#ifdef errno
#undef errno
#endif
#define errno WSAGetLastError()
#ifdef EWOULDBLOCK
#undef EWOULDBLOCK
#endif
#define EWOULDBLOCK WSAEWOULDBLOCK
#ifdef EINTR
#undef EINTR
#endif
#define EINTR WSAEINTR
#else
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#define msleep(ms)                                                                                                             \
	{                                                                                                                          \
		struct timespec tv;                                                                                                    \
		tv.tv_sec = (ms) / 1000;                                                                                               \
		tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;                                                                              \
		nanosleep(&tv, &tv);                                                                                                   \
	}
#endif

#ifndef USE_SSL
#define USE_SSL 0
#endif

#if USE_SSL
#include "openssl/crypto.h"
#include "openssl/dh.h"
#include "openssl/err.h"
#include "openssl/pem.h"
#include "openssl/ssl.h"
#include "openssl/x509.h"
#endif

#if !defined(BSD)
#if (defined(__bsdi__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__Darwin__) ||        \
     defined(__DragonFly__))
#define BSD 1
#endif
#endif

#if defined(BSD)
#include <sys/event.h>
#elif defined(__linux__)
#include <sys/epoll.h>
#elif !defined(_WIN32)
#include <poll.h>
#endif

#include "network.h"
#include "skipbuck.h"
#include "skiplist.h"
#include "thread.h"
#include "uncle.h"

#define POLLFD_SIZE 10000
#define MAX_SERVERS 10000
#define MAX_EVENTS 16
#define DEFAULT_NAME "DEFAULT"
#define READ_BUFLEN (1024 * 4)
#define STASH_LIMIT 1000

#ifndef MAX_READMSG_SIZE
#define MAX_READMSG_SIZE (1024 * 1024 * 64)
#endif

#ifndef POLLRDHUP
#define POLLRDHUP 0
#endif

#ifndef TLS_client_method
#define TLS_client_method TLSv1_client_method
#define TLS_method TLSv1_method
#endif

#define DEFAULT_CIPHERS "HIGH:!aNULL" // EECDH+AESGCM:EDH+AESGCM:EECDH+AES256:EDH+AES256

static const int g_debug = 0;
extern volatile int g_abort;

typedef struct {
	int (*f)(session *, void *);
	void *v;
	char *name;
	int fd, pri, is_tcp, is_ssl, ipv4;
	unsigned port;
} server;

struct handler_ {
	skipbuck *fds, *badfds;
	lock *strand;
	tpool *tp;
	uncle *u[MAX_SERVERS];
	server srvs[MAX_SERVERS];
	fd_set rfds, wfds;

#if defined(POLLIN) && !defined(__linux__) && !defined(BSD)
	struct pollfd rpollfds[POLLFD_SIZE];
#endif

#if USE_SSL
	SSL_CTX *ctx;
#endif

	int cnt, hi, fd, uncs, tp_created;
	volatile int halt, use;
};

struct session_ {
	handler *h;
	skiplist *stash;
	slnode *iter;
	const char *name;
	char *remote, *local, *host;
	char srcbuf[READ_BUFLEN];
	const char *src;
	char *dstbuf;
	char *dst;

	union {
		int64_t udata_int;
		void *udata_ptr;
	};

	uint64_t udata_flags;
	unsigned int seed;
	int connected, disconnected, len, busy, handled, srclen, idx;
	int use_cnt, fd, pri, ipv4, hidx, blocked;
	int is_tcp, is_ssl, is_ws, is_nonblocking, is_client;
	time_t lasttime;
	unsigned port;
	int (*f)(session *, void *);

#if USE_SSL
	SSL *ssl;
	SSL_CTX *ctx;
#endif

	void *v;

	union {
		struct sockaddr_in addr4;
		struct sockaddr_in6 addr6;
	};
};

#if USE_SSL
static int g_ssl_init = 0;
#endif

unsigned int *get_seed(session *s) { return &s->seed; }

const char *hostname(void)
{
	static char tmpbuf[256] = {0};

	if (tmpbuf[0])
		return tmpbuf;

	if (gethostname(tmpbuf, sizeof(tmpbuf)) == 0)
		return tmpbuf;
	else
		return "LOCALHOST.LOCALDOMAIN";
}

static int _parse_addr4(const char *host, struct sockaddr_in *addr4, int numeric)
{
	struct addrinfo hints = {0};
	hints.ai_family = AF_INET;
	hints.ai_flags = (numeric ? AI_NUMERICHOST : 0) | (host ? 0 : AI_PASSIVE);

	struct addrinfo *result;
	int status;

	if ((status = getaddrinfo(host, 0, &hints, &result)) != 0)
		return 0;

	const struct addrinfo *AI;
	int i = 0;

	for (AI = result; AI != NULL; AI = AI->ai_next) {
		if (AI->ai_family != AF_INET)
			continue;

		memcpy(addr4, AI->ai_addr, AI->ai_addrlen);
		i++;
	}

	freeaddrinfo(result);
	return i;
}

static int parse_addr4(const char *host, struct sockaddr_in *addr4)
{
	if (_parse_addr4(host, addr4, 0) || _parse_addr4(host, addr4, 1))
		return 1;
	else
		return 0;
}

static int _parse_addr6(const char *host, struct sockaddr_in6 *addr6, int numeric)
{
	struct addrinfo hints = {0};
	hints.ai_family = AF_INET6;
	hints.ai_flags = (numeric ? AI_NUMERICHOST : 0) | (host ? 0 : AI_PASSIVE);

	struct addrinfo *result;
	int status;

	if ((status = getaddrinfo(host, 0, &hints, &result)) != 0)
		return 0;

	const struct addrinfo *AI;
	int i = 0;

	for (AI = result; AI != NULL; AI = AI->ai_next) {
		if (AI->ai_family != AF_INET6)
			continue;

		memcpy(addr6, AI->ai_addr, AI->ai_addrlen);
		i++;
	}

	freeaddrinfo(result);
	return i;
}

static int parse_addr6(const char *host, struct sockaddr_in6 *addr6)
{
	if (_parse_addr6(host, addr6, 0) || _parse_addr6(host, addr6, 1))
		return 1;

	return 0;
}

session *session_create(void)
{
	session *s = (session *)calloc(1, sizeof(session));

	if (g_debug)
		printf("*** session_create %p\n", s);

	s->seed = (unsigned int)(size_t)(s + clock());
	s->disconnected = 1;
	s->fd = -1;
	return s;
}

session *session_open(const char *host, unsigned port, int tcp, int ssl)
{
	if (!host || (port == 0))
		return NULL;

#ifdef _WIN32
	static int cnt = 0;

	if (!cnt++) {
		WORD wVersionRequested = MAKEWORD(2, 2);
		WSADATA wsaData;

		if (WSAStartup(wVersionRequested, &wsaData) != 0) {
			WSACleanup();
			return NULL;
		}
	}
#endif

	int newfd = -1, try_ipv6 = 1, try_ipv4 = 1;
	struct sockaddr_in6 addr6 = {0};
	struct sockaddr_in addr4 = {0};

	while (try_ipv6 || try_ipv4) {
		if (try_ipv6) {
			try_ipv6 = 0;
			addr6.sin6_family = AF_INET6;

			if (parse_addr6(host, &addr6)) {
				addr6.sin6_port = htons(port);
				newfd = socket(AF_INET6, tcp ? SOCK_STREAM : SOCK_DGRAM, 0);

				if (newfd < 0) {
					printf("socket6 failed: %s\n", strerror(errno));
					continue;
				}

#ifdef SO_REUSEADDR
				int flag = 1;
				setsockopt(newfd, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
#endif
#ifdef SO_REUSEPORT
				int flag2 = 1;
				setsockopt(newfd, SOL_SOCKET, SO_REUSEPORT, (char *)&flag2, sizeof(flag2));
#endif

				if (tcp) {
					if (connect(newfd, (struct sockaddr *)&addr6, sizeof(addr6)) != 0) {
						// printf("connect6 failed:
						// %s\n", strerror(errno));
						close(newfd);
						newfd = -1;
						continue;
					}
				}

				break;
			}
		}

		if (try_ipv4) {
			try_ipv4 = 0;
			addr4.sin_family = AF_INET;

			if (parse_addr4(host, &addr4)) {
				addr4.sin_port = htons(port);
				newfd = socket(AF_INET, tcp ? SOCK_STREAM : SOCK_DGRAM, 0);

				if (newfd < 0) {
					printf("socket4 failed: %s\n", strerror(errno));
					continue;
				}

#if SO_REUSEADDR
				int flag = 1;
				setsockopt(newfd, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
#endif
#if SO_REUSEPORT
				int flag2 = 1;
				setsockopt(newfd, SOL_SOCKET, SO_REUSEPORT, (char *)&flag2, sizeof(flag2));
#endif

				if (tcp) {
					if (connect(newfd, (struct sockaddr *)&addr4, sizeof(addr4)) != 0) {
						// printf("connect4 failed:
						// %s\n", strerror(errno));
						close(newfd);
						newfd = -1;
						continue;
					}
				}

				break;
			}
		}
	}

	if (newfd == -1)
		return NULL;

	session *s = session_create();
	s->host = strdup(host);
	s->name = DEFAULT_NAME;
	s->disconnected = 0;
	s->connected = 1;
	s->fd = newfd;
	s->ipv4 = !try_ipv6;
	s->port = port;
	s->is_tcp = tcp;
	s->src = s->srcbuf;
	s->is_client = 1;

	if (s->ipv4)
		s->addr4 = addr4;
	else
		s->addr6 = addr6;

	if (tcp) {
		struct linger linger;
		linger.l_onoff = 0;
		linger.l_linger = 1;
		setsockopt(newfd, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));
		int flag = 1;
		setsockopt(newfd, SOL_SOCKET, SO_KEEPALIVE, (char *)&flag, sizeof(flag));
		setsockopt(newfd, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(flag));

#ifdef TCP_QUICKACK
		setsockopt(newfd, IPPROTO_TCP, TCP_QUICKACK, (char *)&flag, sizeof(flag));
#endif
	}

#if USE_SSL
	if (ssl) {
		if (!session_enable_tls(s, NULL, 0)) {
			session_close(s);
			return NULL;
		}
	}
#endif

	return s;
}

int session_enable_tls(session *s, const char *certfile, int level)
{
	if (s->is_ssl)
		return 0;

#if USE_SSL
	if (!g_ssl_init) {
		g_ssl_init = 1;
		// SSL_load_error_strings();
		SSL_library_init();
	}

	static SSL_CTX *s_ctx = NULL;

	if (s->is_client) {
		if (!s_ctx) {
			s_ctx = SSL_CTX_new(TLS_client_method());

			if (!s_ctx) {
				printf("SSL new client context failed\n");
				ERR_print_errors_fp(stderr);
				return 0;
			}

			SSL_CTX_set_options(s_ctx, SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3);
			SSL_CTX_set_cipher_list(s_ctx, DEFAULT_CIPHERS);

			if (certfile) {
				if (!SSL_CTX_use_certificate_file(s_ctx, (char *)certfile, SSL_FILETYPE_PEM))
					printf("SSL load certificate failed\n");

				// if (!SSL_CTX_set_default_verify_paths(s_ctx))
				//	printf("SSL set_default_verify_paths
				// failed\n");
			}
		}

		s->ctx = s_ctx;
	}

	s->ssl = SSL_new(s->ctx);

	if (!s->ssl) {
		printf("SSL new SSL failed\n");
		ERR_print_errors_fp(stderr);
		return 0;
	}

	SSL_set_ssl_method(s->ssl, !s->is_client ? SSLv23_server_method() : TLS_client_method());
	SSL_set_mode(s->ssl, SSL_MODE_AUTO_RETRY);

	if ((level > 0) && certfile)
		SSL_set_verify(s->ssl, SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT, 0);
	else
		SSL_set_verify(s->ssl, SSL_VERIFY_NONE, 0);

	SSL_set_fd(s->ssl, s->fd);

	if (!s->is_client) {
		if (SSL_accept(s->ssl) == -1) {
			if (g_debug)
				printf("SSL_accept failed\n");

			if (g_debug)
				ERR_print_errors_fp(stderr);

			return 0;
		}
	}
	else {
		SSL_set_tlsext_host_name(s->ssl, s->host);

		if (SSL_connect(s->ssl) == -1) {
			if (g_debug)
				printf("SSL_connect failed\n");

			if (g_debug)
				ERR_print_errors_fp(stderr);

			return 0;
		}
	}

#if 0
	char cipher[256];
	cipher[0] = 0;
	sscanf(SSL_get_cipher(s->ssl), "%255s", cipher);
	cipher[255] = 0;
	//printf("CIPHER: %s\n", cipher);

	X509 *server_cert = SSL_get_peer_certificate(s->ssl);

	if (server_cert) {
		char buf[1024];
		//char *str;
		//buf[0] = 0;
		//str = X509_NAME_oneline(X509_get_subject_name(server_cert), buf, sizeof(buf));
		buf[0] = 0;
		X509_NAME_oneline(X509_get_issuer_name(server_cert), buf, sizeof(buf));
		const char *ptr = strstr(buf, "/CN=");
		char common_name[256];
		common_name[0] = 0;

		if (ptr)
			sscanf(ptr+4, "%255[^/]", common_name);

		common_name[255] = 0;
		//printf("SSL Server Common name = '%s'\n", common_name);
		X509_free(server_cert);
	} else {
		if (level > 0)
			printf("SSL No server certificate\n");
	}
#endif

	SSL_set_read_ahead(s->ssl, 0);
	s->is_ssl = 1;
	return 1;
#else
	return 0;
#endif
}

int session_on_connect(session *s)
{
	if (s->connected) {
		s->connected = 0;
		return 1;
	}

	return 0;
}

int session_getfd(session *s) { return s->fd; }
int session_on_disconnect(session *s) { return s->disconnected; }
int session_is_server(session *s) { return !s->is_client; }
int session_is_websocket(session *s) { return s->is_ws; }
int session_is_nonblocking(session *s) { return s->is_nonblocking; }
int session_is_client(session *s) { return s->is_client; }
int session_is_priority(session *s) { return s->pri; }
int session_is_tls(session *s) { return s->is_ssl; }
int session_is_udp(session *s) { return !s->is_tcp; }
int session_is_tcp(session *s) { return s->is_tcp; }
int session_is_ipv4(session *s) { return s->ipv4; }
int session_is_ipv6(session *s) { return !s->ipv4; }

void session_lock(session *s)
{
	if (s->h)
		lock_lock(s->h->strand);
}
void session_unlock(session *s)
{
	if (s->h)
		lock_unlock(s->h->strand);
}

#if defined(_WIN32) && 0
static const char *inet_ntop(int family, void *address, char *buffer, socklen_t len)
{
	DWORD buflen = 256;

	if (family == AF_INET6) {
		struct sockaddr_in6 sin6 = {0};
		sin6.sin6_family = family;
		sin6.sin6_addr = *((struct in6_addr *)address);

		if (WSAAddressToString((struct sockaddr *)&sin6, sizeof(sin6), NULL, buffer, &buflen) == SOCKET_ERROR)
			strcpy(buffer, "");
	}
	else {
		struct sockaddr_in sin4 = {0};
		sin4.sin_family = family;
		sin4.sin_addr = *((struct in_addr *)address);

		if (WSAAddressToString((struct sockaddr *)&sin4, sizeof(sin4), NULL, buffer, &buflen) == SOCKET_ERROR)
			strcpy(buffer, "");
	}

	return buffer;
}
#endif

time_t session_get_lasttime(session *s) { return s->lasttime; }

void session_set_udata_flag(session *s, unsigned flag) { s->udata_flags |= 1ULL << flag; }
unsigned session_get_udata_flag(session *s, unsigned flag) { return s->udata_flags & (1ULL << flag); }
void session_clr_udata_flag(session *s, unsigned flag) { s->udata_flags &= ~(1ULL << flag); }
void session_clr_udata_flags(session *s) { s->udata_flags = 0; }
void session_set_udata_int(session *s, int64_t data) { s->udata_int = data; }
int64_t session_get_udata_int(session *s) { return s->udata_int; }
void session_set_udata_ptr(session *s, void *data) { s->udata_ptr = data; }
void *session_get_udata_ptr(session *s) { return s->udata_ptr; }
const char *session_get_name(session *s) { return s->name ? s->name : ""; }
int session_set_websocket(session *s) { return s->is_ws = 1; }
size_t session_count_stash(session *s) { return s->stash ? sl_count(s->stash) : 0; }

void session_clr_stash(session *s)
{
	if (s->stash)
		sl_clear(s->stash, &free);
}

const char *session_get_key(session *s, const char *key)
{
	if (!s->stash)
		return NULL;

	if (!s->iter)
		s->iter = sl_findkey(s->stash, key);

	void *tmp_value = NULL;
	sl_nextkey(&s->iter, key, &tmp_value);
	return (char *)tmp_value;
}

const char *session_get_keys(session *s)
{
	if (!s->stash)
		return NULL;

	if (!s->iter)
		s->iter = sl_startx(s->stash);

	return sl_nextx(&s->iter, NULL);
}

int session_app_stash(session *s, const char *key, const char *value)
{
	if (!s->stash) {
		s->stash = (skiplist *)calloc(1, sizeof(skiplist));
		sl_init(s->stash, 1, &strcasecmp, &free);
	}

	if (sl_count(s->stash) >= STASH_LIMIT)
		return 0;

	sl_set(s->stash, strdup(key), strdup(value));
	return 1;
}

int session_set_stash(session *s, const char *key, const char *value)
{
	if (!s->stash) {
		s->stash = (skiplist *)calloc(1, sizeof(skiplist));
		sl_init(s->stash, 1, &strcasecmp, &free);
	}

	if (sl_count(s->stash) > STASH_LIMIT)
		return 0;

	void *tmp_value = NULL;

	if (sl_del(s->stash, key, &tmp_value))
		free(tmp_value);

	sl_set(s->stash, strdup(key), strdup(value));
	return 1;
}

int session_set_stash_int(session *s, const char *key, int64_t n)
{
	char value[40];
	sprintf(value, "%lld", (long long)n);
	return session_set_stash(s, key, value);
}

int session_set_stash_float(session *s, const char *key, double n)
{
	char value[40];
	sprintf(value, "%g", n);
	return session_set_stash(s, key, value);
}

const char *session_del_stash(session *s, const char *key)
{
	if (!s->stash)
		return NULL;

	void *tmp_value = NULL;
	sl_del(s->stash, key, &tmp_value);
	return (char *)tmp_value;
}

const char *session_get_stash(session *s, const char *key)
{
	if (!s->stash)
		return "";

	void *value = (char *)"";
	sl_get(s->stash, key, &value);
	return (char *)value;
}

int64_t session_get_stash_int(session *s, const char *key)
{
	if (!s->stash)
		return 0;

	void *value = (char *)"";
	sl_get(s->stash, key, &value);
	long long v = 0;
	sscanf((char*)value, "%lld", &v);
	return v;
}

double session_get_stash_float(session *s, const char *key)
{
	if (!s->stash)
		return 0;

	void *value = (char *)"";
	sl_get(s->stash, key, &value);
	double v = 0;
	sscanf((char*)value, "%lg", &v);
	return v;
}

int session_enable_broadcast(session *s)
{
	int flag = 1;
	int status = setsockopt(s->fd, SOL_SOCKET, SO_BROADCAST, (char *)&flag, sizeof(flag));

	if (status < 0)
		printf("### Enable broadcast failed, fd: %d, port: %u %s\n", s->fd, s->port, strerror(errno));

	return status >= 0;
}

int session_enable_multicast(session *s, int loop, int ttl)
{
	int status = 0;

	if (!s->ipv4) {
		if (loop)
			status = setsockopt(s->fd, IPPROTO_IPV6, IPV6_MULTICAST_LOOP, (const char *)&loop, sizeof(loop));

		if (ttl)
			status = setsockopt(s->fd, IPPROTO_IPV6, IPV6_MULTICAST_HOPS, (const char *)&ttl, sizeof(ttl));
	}
	else {
		if (loop)
			status = setsockopt(s->fd, IPPROTO_IP, IP_MULTICAST_LOOP, (const char *)&loop, sizeof(loop));

		if (ttl)
			status = setsockopt(s->fd, IPPROTO_IP, IP_MULTICAST_TTL, (const char *)&ttl, sizeof(ttl));
	}

	if (status < 0)
		printf("### Enable multicast failed, fd: %d, port: %u %s\n", s->fd, s->port, strerror(errno));

	return status >= 0;
}

int session_set_sndbuffer(session *s, int n)
{
	int bufsize = n;
	int status = setsockopt(s->fd, SOL_SOCKET, SO_SNDBUF, (const char *)&bufsize, sizeof(bufsize));
	return status == 0;
}

int session_set_rcvbuffer(session *s, int n)
{
	int bufsize = n;
	int status = setsockopt(s->fd, SOL_SOCKET, SO_RCVBUF, (const char *)&bufsize, sizeof(bufsize));
	return status == 0;
}

int session_set_nodelay(session *s, int n)
{
	int flag = n;
	int status = setsockopt(s->fd, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(flag));
	return status == 0;
}

int session_set_quickack(session *s, int n)
{
#if defined(TCP_QUICKACK)
	int flag = n, status = 0;
	status = setsockopt(s->fd, IPPROTO_TCP, TCP_QUICKACK, (char *)&flag, sizeof(flag));
	return status == 0;
#else
	return 0;
#endif
}

int session_set_cork(session *s, int n)
{
	int flag = n, status = 0;
#if defined(TCP_CORK)
	status = setsockopt(s->fd, IPPROTO_TCP, TCP_CORK, (char *)&flag, sizeof(flag));
#elif defined(TCP_NOPUSH)
	status = setsockopt(s->fd, IPPROTO_TCP, TCP_NOPUSH, (char *)&flag, sizeof(flag));
#endif
	return status == 0;
}

int session_set_nonblocking(session *s, int n)
{
	unsigned long flag = n;
	int status = ioctl(s->fd, FIONBIO, &flag);
	s->is_nonblocking = flag;
	return status == 0;
}

const char *session_get_local_addr(session *s, int resolve)
{
	if (s->disconnected)
		return "";

	if (!s->local)
		s->local = (char *)malloc(INET_ADDRSTRLEN + 1);

	if (s->is_tcp && s->ipv4) {
		socklen_t len = sizeof(struct sockaddr_in);

		if (getsockname(s->fd, (struct sockaddr *)&s->addr4, &len) == 0)
			return inet_ntop(AF_INET, &s->addr4.sin_addr, s->local, INET_ADDRSTRLEN);
	}
	else if (s->is_tcp) {
		socklen_t len = sizeof(struct sockaddr_in6);

		if (getsockname(s->fd, (struct sockaddr *)&s->addr6, &len) == 0)
			return inet_ntop(AF_INET6, &s->addr6.sin6_addr, s->local, INET6_ADDRSTRLEN);
	}
	else if (s->ipv4)
		return inet_ntop(AF_INET, &s->addr4.sin_addr, s->local, INET_ADDRSTRLEN);
	else
		return inet_ntop(AF_INET6, &s->addr6.sin6_addr, s->local, INET6_ADDRSTRLEN);

	return "";
}

const char *session_get_remote_addr(session *s, int resolve)
{
	if (s->disconnected)
		return "";

	if (!s->remote)
		s->remote = (char *)malloc(INET_ADDRSTRLEN + 1);

	if (s->is_tcp && s->ipv4) {
		socklen_t len = sizeof(struct sockaddr_in);

		if (getpeername(s->fd, (struct sockaddr *)&s->addr4, &len) == 0)
			return inet_ntop(AF_INET, &s->addr4.sin_addr, s->remote, INET_ADDRSTRLEN);
	}
	else if (s->is_tcp) {
		socklen_t len = sizeof(struct sockaddr_in6);

		if (getpeername(s->fd, (struct sockaddr *)&s->addr6, &len) == 0)
			return inet_ntop(AF_INET6, &s->addr6.sin6_addr, s->remote, INET6_ADDRSTRLEN);
	}
	else if (s->ipv4)
		return inet_ntop(AF_INET, &s->addr4.sin_addr, s->remote, INET_ADDRSTRLEN);
	else
		return inet_ntop(AF_INET6, &s->addr6.sin6_addr, s->remote, INET6_ADDRSTRLEN);

	return "";
}

unsigned session_get_local_port(session *s)
{
	if (s->disconnected)
		return 0;

	if (s->is_tcp && s->ipv4) {
		socklen_t len = sizeof(struct sockaddr_in);

		if (getsockname(s->fd, (struct sockaddr *)&s->addr4, &len) == 0)
			return ntohs(s->addr4.sin_port);
	}
	else if (s->is_tcp) {
		socklen_t len = sizeof(struct sockaddr_in6);

		if (getsockname(s->fd, (struct sockaddr *)&s->addr6, &len) == 0)
			return ntohs(s->addr6.sin6_port);
	}
	else if (s->ipv4)
		return ntohs(s->addr4.sin_port);
	else
		return ntohs(s->addr6.sin6_port);

	return 0;
}

unsigned session_get_remote_port(session *s)
{
	if (s->disconnected)
		return 0;

	if (s->is_tcp && s->ipv4) {
		socklen_t len = sizeof(struct sockaddr_in);

		if (getpeername(s->fd, (struct sockaddr *)&s->addr4, &len) == 0)
			return ntohs(s->addr4.sin_port);
	}
	else if (s->is_tcp) {
		socklen_t len = sizeof(struct sockaddr_in6);

		if (getpeername(s->fd, (struct sockaddr *)&s->addr6, &len) == 0)
			return ntohs(s->addr6.sin6_port);
	}
	else if (s->ipv4)
		return ntohs(s->addr4.sin_port);
	else
		return ntohs(s->addr6.sin6_port);

	return 0;
}

static size_t bufwrite(void *dstbuf, const void *srcbuf, size_t nbytes)
{
	memcpy(dstbuf, srcbuf, nbytes);
	return nbytes;
}

int ws_msg(session *s, unsigned fin, unsigned opcode, const char *src, size_t len)
{
	if (!session_is_websocket(s))
		return -1;

	char *dstbuf = (char *)malloc(len + 64);
	char *dst = dstbuf;
	uint8_t f = 0;

	if (fin)
		f |= 0x1 << 7;

	f |= (unsigned char)opcode & 0xf;
	dst += bufwrite(dst, &f, 1);
	int masked;

	if ((opcode == WS_OP_BINARY) || (opcode == WS_OP_TEXT))
		masked = 1;
	else
		masked = 0;

	f = 0;

	if (masked)
		f |= 0x1 << 7;

	uint8_t tmplen;

	if (len <= 125)
		tmplen = (uint8_t)len;
	else if (len <= 0xffff)
		tmplen = 126;
	else
		tmplen = 127;

	f |= (unsigned char)tmplen & 0x7f;
	dst += bufwrite(dst, &f, 1);

	if (tmplen == 126) {
		uint16_t n = htons(len);
		dst += bufwrite(dst, &n, sizeof(n));
	}
	else {
		if (tmplen == 127) {
			uint32_t n = htonl((uint64_t)len >> 32);
			dst += bufwrite(dst, &n, sizeof(n));
			n = htonl((uint64_t)len & 0xffffffff);
			dst += bufwrite(dst, &n, sizeof(n));
		}
	}

	if (masked) {
		union {
			uint32_t mask;
			char mbytes[4];
		} m;

		m.mask = rand_r(&s->seed);
		dst += bufwrite(dst, &m.mask, sizeof(m.mask));

		for (int i = 0; i < len; i++)
			*dst++ = src[i] ^ m.mbytes[i % 4];
	}
	else if (len > 0)
		dst += bufwrite(dst, src, len);

	if (session_write(s, dstbuf, dst - dstbuf) <= 0) {
		free(dstbuf);
		return 0;
	}

	free(dstbuf);
	return 1;
}

int session_ws_parse(session *s, int *fin, unsigned *opcode, char **dstbuf, size_t *dstlen)
{
	if (!session_is_websocket(s))
		return 0;

	uint8_t f;

	if (!(session_read(s, &f, 1) == 1))
		return 0;

	*fin = (f >> 7) ? 1 : 0;
	*opcode = f & 0xf;

	if (!(session_read(s, &f, 1) == 1))
		return 0;

	const int masked = f >> 7;
	uint64_t len = f & 0x7f;

	if (len == 127) {
		uint32_t n;

		if (!(session_read(s, &n, sizeof(n)) == sizeof(n)))
			return 0;

		len = (uint64_t)ntohl(n) << 32;

		if (!(session_read(s, &n, sizeof(n)) == sizeof(n)))
			return 0;

		len |= ntohl(n);
	}
	else {
		if (len == 126) {
			uint16_t n;

			if (!(session_read(s, &n, sizeof(n)) == sizeof(n)))
				return 0;

			len = ntohs(n);
		}
	}

	union {
		uint32_t mask;
		char mbytes[4];
	} m;

	if (masked) {
		uint32_t n;

		if (!(session_read(s, &n, sizeof(n)) == sizeof(n)))
			return 0;

		m.mask = n;
	}

	if (len) {
		char *bufptr = (char *)malloc(len + 1);

		if (!(session_read(s, bufptr, len) == len)) {
			free(bufptr);
			return 0;
		}

		for (int i = 0; masked && (i < len); i++)
			bufptr[i] ^= m.mbytes[i % 4];

		bufptr[len] = '\0';
		*dstbuf = bufptr;
		*dstlen = len;
	}

	// printf("*** code=%X, len=%d, '%s'\n", *opcode, (int)len, *dstbuf);

	if (*opcode == WS_OP_PING)
		session_ws_pong(s, (len ? *dstbuf : ""), len);

	return 1;
}

int session_rawwrite(session *s, const void *buf, size_t len)
{
	if (s->disconnected)
		return -1;

	if (!len)
		return 0;

	int wlen;

	for (;;) {
#if USE_SSL
		if (s->is_ssl)
			wlen = SSL_write(s->ssl, buf, len);
		else
#endif
		    if (s->is_tcp)
			wlen = send(s->fd, (const char *)buf, len, MSG_NOSIGNAL);
		else if (s->ipv4) {
			socklen_t alen = sizeof(struct sockaddr_in);
			wlen = sendto(s->fd, (const char *)buf, len, MSG_NOSIGNAL, (struct sockaddr *)&s->addr4, alen);
		}
		else {
			socklen_t alen = sizeof(struct sockaddr_in6);
			wlen = sendto(s->fd, (const char *)buf, len, MSG_NOSIGNAL, (struct sockaddr *)&s->addr6, alen);
		}

		if (wlen > 0)
			break;

		if ((errno != EAGAIN) && (errno != EWOULDBLOCK) && (errno != EINTR)) {
			s->disconnected = 1;
			return -1;
		}

		if (errno == EINTR)
			continue;

		s->blocked = 1;
		return 0;
	}

	return wlen;
}

int session_write(session *s, const void *_buf, size_t len)
{
	if (s->disconnected)
		return -1;

	if (!len)
		return 0;

	const char *buf = (const char *)_buf;

	while (len > 0) {
		int wlen = session_rawwrite(s, buf, len);

		if (wlen < 0)
			return 0;

		if (!wlen) {
			s->blocked = 0;

			if (errno != EINTR)
				msleep(1);

			continue;
		}

		buf += wlen;
		len -= wlen;
	}

	return 1;
}

int session_writemsg(session *s, const char *buf) { return session_write(s, buf, strlen(buf)); }

int session_bcast(session *s, const void *buf, size_t len)
{
	if (!len)
		return 0;

	if (s->is_tcp)
		return 0;

	struct sockaddr_in addr4 = {0};
	addr4.sin_family = AF_INET;
	addr4.sin_port = htons(s->port);
	addr4.sin_addr.s_addr = htonl(INADDR_ANY);
	socklen_t alen = sizeof(struct sockaddr_in);
	int wlen = sendto(s->fd, (const char *)buf, len, MSG_NOSIGNAL, (struct sockaddr *)&addr4, alen);
	return wlen > 0;
}

int session_bcastmsg(session *s, const char *buf) { return session_bcast(s, buf, strlen(buf)); }

int session_read(session *s, void *buf, size_t len)
{
	if (s->disconnected)
		return 0;

	int rlen = 0;

	// Read until end of input buffer

	char *dst = (char *)buf;

	while (s->srclen && len) {
		*dst++ = *s->src++;
		s->srclen--;
		len--;
		rlen++;
	}

	if (rlen && !len)
		return rlen;
	else
		buf = dst;

#if USE_SSL
	if (s->is_ssl)
		rlen += SSL_read(s->ssl, (char *)buf, len);
	else
#endif
	    if (s->is_tcp)
		rlen += recv(s->fd, (char *)buf, len, 0);
	else if (s->ipv4) {
		socklen_t tmplen = sizeof(struct sockaddr_in);
		rlen = recvfrom(s->fd, (char *)buf, len, 0, (struct sockaddr *)&s->addr4, &tmplen);
	}
	else {
		socklen_t tmplen = sizeof(struct sockaddr_in6);
		rlen = recvfrom(s->fd, (char *)buf, len, 0, (struct sockaddr *)&s->addr6, &tmplen);
	}

	if ((rlen < 0) && ((errno == EAGAIN) || (errno == EWOULDBLOCK) || (errno == EINTR)))
		return 0;

	if (rlen <= 0) {
		if (s->is_tcp)
			s->disconnected = 1;

		return 0;
	}

	s->lasttime = time(NULL);
	return rlen;
}

int session_readmsg(session *s, char **buf)
{
	if (s->disconnected)
		return 0;

	// Allocate internal destination message buffer
	// if one doesn't already exist...

	if (!s->dstbuf) {
		s->dstbuf = (char *)malloc(s->len = READ_BUFLEN);
		s->dst = s->dstbuf;
	}

	// Read until end of input buffer or newline,
	// whichever comes first...

	while (s->srclen && *s->src && (*s->src != '\n')) {
		*s->dst++ = *s->src++;
		s->srclen--;

		// Allow space for \n\0

		if ((s->dst - s->dstbuf) == (s->len - 2)) {
			int save_len = s->len;
			s->dstbuf = (char *)realloc(s->dstbuf, s->len += READ_BUFLEN);
			s->dst = s->dstbuf + save_len;
		}
	}

	// Newline means we have a complete message...

	if (*s->src == '\n') {
		*s->dst++ = *s->src++;
		s->srclen--;
		*s->dst = '\0';
		*buf = s->dstbuf;
		size_t len = s->dst - s->dstbuf;
		s->dstbuf = NULL;
		return len;
	}

	if ((s->dst - s->dstbuf) >= MAX_READMSG_SIZE) {
		*s->dst = '\0';
		*buf = s->dstbuf;
		size_t len = s->dst - s->dstbuf;
		s->dstbuf = NULL;
		return len;
	}

	// If not then read some more and repeat.

	int rlen = 0;
	s->srcbuf[0] = '\0';
	s->src = s->srcbuf;
	s->srclen = 0;

#if USE_SSL
	if (s->is_ssl) {
		rlen = SSL_read(s->ssl, s->srcbuf, READ_BUFLEN - 1);
	}
	else
#endif
	    if (s->is_tcp) {
		rlen = recv(s->fd, (char *)s->srcbuf, READ_BUFLEN - 1, 0);
	}
	else if (s->ipv4) {
		socklen_t tmplen = sizeof(struct sockaddr_in);
		rlen = recvfrom(s->fd, s->srcbuf, READ_BUFLEN - 1, 0, (struct sockaddr *)&s->addr4, &tmplen);
	}
	else {
		socklen_t tmplen = sizeof(struct sockaddr_in6);
		rlen = recvfrom(s->fd, s->srcbuf, READ_BUFLEN - 1, 0, (struct sockaddr *)&s->addr6, &tmplen);
	}

	if ((rlen < 0) && ((errno == EAGAIN) || (errno == EWOULDBLOCK) || (errno == EINTR)))
		return 0;

	if (rlen <= 0) {
		if (s->is_tcp)
			s->disconnected = 1;

		return 0;
	}

	s->lasttime = time(NULL);
	s->srclen = rlen;
	s->srcbuf[rlen] = '\0';
	return session_readmsg(s, buf);
}

int session_close(session *s)
{
	if (g_debug)
		printf("*** session_close %p fd=%d\n", s, s->fd);

	if (s->fd != -1) {
		s->disconnected = 1;

		if (s->is_client) {
			shutdown(s->fd, SHUT_RDWR);
			close(s->fd);
		}
		else
			shutdown(s->fd, SHUT_WR);
	}

	if (!s->handled)
		session_unshare(s);

	return 1;
}

static void session_free(session *s)
{
	if (g_debug)
		printf("*** session_free %p\n", s);

	if (s->dstbuf)
		free(s->dstbuf);

	if (s->stash) {
		sl_done(s->stash, &free);
		free(s->stash);
	}

	if (s->host)
		free(s->host);

	if (s->local)
		free(s->local);

	if (s->remote)
		free(s->remote);

#if USE_SSL
	if (s->ssl)
		SSL_free(s->ssl);
#endif

	free(s);
}

void session_unshare(session *s)
{
	if (g_debug)
		printf("*** session_unshare %p %d\n", s, (int)s->use_cnt);

	if (--s->use_cnt > 0)
		return;

	session_free(s);
}

void session_share(session *s) { s->use_cnt++; }

static int handler_force_drop(void *_h, int fd, void *_s)
{
	if (g_debug)
		printf("*** force_drop %p\n", _s);

	session_close((session *)_s);
	return 1;
}

static int handler_accept(handler *h, server *srv, session **v)
{
	if (h->halt)
		return -1;

	struct sockaddr_in6 addr6 = {0};
	addr6.sin6_family = AF_UNSPEC;
	socklen_t len = sizeof(addr6);
	int newfd;

	if ((newfd = accept(srv->fd, (struct sockaddr *)&addr6, &len)) < 0) {
		printf("handler_accept: accept6 fd=%d failed: %s\n", srv->fd, strerror(errno));
		return -1;
	}

	struct linger linger;
	linger.l_onoff = 0;
	linger.l_linger = 1;
	setsockopt(newfd, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));
	int flag = 1;
	setsockopt(newfd, SOL_SOCKET, SO_KEEPALIVE, (char *)&flag, sizeof(flag));
	setsockopt(newfd, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(flag));
	session *s = session_create();
	s->name = srv->name;
	session_share(s);
	s->disconnected = 0;
	s->connected = 1;
	s->handled = 1;
	s->h = h;
	s->fd = newfd;
	s->port = srv->port;
	s->pri = srv->pri;
	s->is_tcp = 1;
	s->is_ws = srv->is_tcp == 2;
	s->ipv4 = srv->ipv4;
	s->src = s->srcbuf;
	s->f = srv->f;
	s->v = srv->v;

#if USE_SSL
	s->ctx = h->ctx;
#endif

	*v = s;

#if USE_SSL
	if (srv->is_ssl) {
		if (!session_enable_tls(s, NULL, 0)) {
			close(newfd);
			return -1;
		}
	}
#endif

	unsigned long flag2 = 1;
	ioctl(newfd, FIONBIO, &flag2);
	s->is_nonblocking = flag2;
	sb_int_set(h->fds, newfd, s);
	h->use++;
	return newfd;
}

#if defined(BSD)

static int kqueue_accept(void *data)
{
	if (g_debug)
		printf("*** KQUEUE: accept %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	s->f(s, s->v);
	struct kevent ev = {0};

	if (s->blocked) {
		EV_SET(&ev, s->fd, EVFILT_WRITE, EV_ADD | EV_CLEAR | EV_DISPATCH, 0, 0, s);
		s->blocked = 0;
	}
	else
		EV_SET(&ev, s->fd, EVFILT_READ, EV_ADD | EV_CLEAR | EV_DISPATCH, 0, 0, s);

	kevent(s->h->fd, &ev, 1, NULL, 0, NULL);
	return 1;
}

static int kqueue_close(void *data)
{
	if (g_debug)
		printf("*** KQUEUE: close %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	struct kevent ev = {0};

	if (s->blocked)
		EV_SET(&ev, s->fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
	else
		EV_SET(&ev, s->fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);

	kevent(s->h->fd, &ev, 1, NULL, 0, NULL);
	s->f(s, s->v);
	close(s->fd);
	session_unshare(s);
	return 1;
}

static int kqueue_run(void *data)
{
	if (g_debug)
		printf("*** KQUEUE: run %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	struct kevent ev = {0};

	while (s->f(s, s->v))
		;

	if (!s->disconnected) {
		if (s->blocked) {
			EV_SET(&ev, s->fd, EVFILT_WRITE, EV_ADD | EV_CLEAR | EV_DISPATCH, 0, 0, s);
			s->blocked = 0;
		}
		else
			EV_SET(&ev, s->fd, EVFILT_READ, EV_ADD | EV_CLEAR | EV_DISPATCH, 0, 0, s);

		kevent(s->h->fd, &ev, 1, NULL, 0, NULL);
	}
	else if (s->is_tcp) {
		sb_int_del(s->h->fds, s->fd);
		s->h->use--;
		session_unshare(s);
	}

	if (!s->is_tcp)
		free(s);

	return 1;
}

int handler_wait_kqueue(handler *h, int wait)
{
	if (g_debug)
		printf("*** USING KQUEUE\n");

	struct kevent ev = {0}, events[MAX_EVENTS];

	for (int i = 0; i < h->cnt; i++) {
		server *srv = &h->srvs[i];

		if (!srv->is_tcp)
			EV_SET(&ev, srv->fd, EVFILT_READ, EV_ADD | EV_CLEAR | EV_DISPATCH, 0, 0, (void *)(size_t)i);
		else
			EV_SET(&ev, srv->fd, EVFILT_READ, EV_ADD, 0, 0, (void *)(size_t)i);

		kevent(h->fd, &ev, 1, NULL, 0, NULL);
	}

	while (!g_abort && !h->halt && (h->use || wait)) {
		struct timespec ts = {0, 1000 * 1000 * 10};
		int n = kevent(h->fd, NULL, 0, (struct kevent *)events, MAX_EVENTS, &ts);

		for (int i = 0; i < n; i++) {
			session *s = NULL;

			if ((int)(size_t)events[i].udata < h->cnt) {
				size_t idx = (size_t)events[i].udata;
				server *srv = &h->srvs[idx];

				if (srv->is_tcp) {
					int newfd = handler_accept(h, srv, &s);

					if (newfd == -1)
						continue;

					tpool_schedule(h->tp, &kqueue_accept, s);
					h->use++;
					continue;
				}

				struct kevent ev = {0};
				EV_SET(&ev, s->fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
				kevent(s->h->fd, &ev, 1, NULL, 0, NULL);
				session *s = session_create();
				s->h = h;
				s->fd = srv->fd;
				s->port = srv->port;
				s->ipv4 = srv->ipv4;
				s->src = s->srcbuf;
				s->f = srv->f;
				s->v = srv->v;
				tpool_schedule(h->tp, &kqueue_run, s);
				continue;
			}

			s = (session *)events[i].udata;

			if (events[i].flags & EV_EOF)
				s->disconnected = 1;

			if (s->disconnected) {
				sb_int_del(h->fds, s->fd);
				h->use--;
				tpool_schedule(h->tp, &kqueue_close, s);
			}
			else
				tpool_schedule(h->tp, &kqueue_run, s);
		}
	}

	close(h->fd);
	return 1;
}

#endif

#if defined(__linux__)

static int epoll_accept(void *data)
{
	if (g_debug)
		printf("*** EPOLL: accept %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	s->f(s, s->v);
	struct epoll_event ev = {0};

	if (s->blocked) {
		ev.events = EPOLLOUT | EPOLLRDHUP | EPOLLONESHOT | EPOLLET;
		s->blocked = 0;
	}
	else
		ev.events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT | EPOLLET;

	ev.data.ptr = s;
	epoll_ctl(s->h->fd, EPOLL_CTL_ADD, s->fd, &ev);
	return 1;
}

static int epoll_close(void *data)
{
	if (g_debug)
		printf("*** EPOLL: close %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	struct epoll_event ev = {0};
	epoll_ctl(s->h->fd, EPOLL_CTL_DEL, s->fd, &ev);
	s->f(s, s->v);
	close(s->fd);
	session_unshare(s);
	return 1;
}

static int epoll_run(void *data)
{
	if (g_debug)
		printf("*** EPOLL: run %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	struct epoll_event ev = {0};

	while (s->f(s, s->v))
		;

	if (!s->disconnected) {
		if (s->blocked) {
			ev.events = EPOLLOUT | EPOLLRDHUP | EPOLLONESHOT | EPOLLET;
			s->blocked = 0;
		}
		else
			ev.events = EPOLLIN | EPOLLRDHUP | EPOLLONESHOT | EPOLLET;

		if (!s->is_tcp)
			ev.data.u64 = s->hidx;
		else
			ev.data.ptr = s;

		epoll_ctl(s->h->fd, EPOLL_CTL_MOD, s->fd, &ev);
	}
	else if (s->is_tcp) {
		sb_int_del(s->h->fds, s->fd);
		s->h->use--;
		session_unshare(s);
	}

	if (!s->is_tcp)
		free(s);

	return 1;
}

int handler_wait_epoll(handler *h, int wait)
{
	if (g_debug)
		printf("*** USING EPOLL\n");

	struct epoll_event ev = {0}, events[MAX_EVENTS];

	for (int i = 0; i < h->cnt; i++) {
		server *srv = &h->srvs[i];
		ev.events = EPOLLIN;

		if (!srv->is_tcp)
			ev.events |= EPOLLONESHOT | EPOLLET;

		ev.data.u64 = i;
		epoll_ctl(h->fd, EPOLL_CTL_ADD, srv->fd, &ev);
	}

	while (!g_abort && !h->halt && (h->use || wait)) {
		int n = epoll_wait(h->fd, (struct epoll_event *)events, MAX_EVENTS, 10);

		for (int i = 0; i < n; i++) {
			session *s = NULL;
			int n = (int)events[i].data.u64;

			if ((n >= 0) && (n < h->cnt)) {
				size_t idx = events[i].data.u64;
				server *srv = &h->srvs[idx];

				if (srv->is_tcp) {
					int newfd = handler_accept(h, srv, &s);

					if (newfd == -1)
						continue;

					tpool_schedule(h->tp, &epoll_accept, s);
					h->use++;
					continue;
				}

				session *s = session_create();
				s->h = h;
				s->fd = srv->fd;
				s->port = srv->port;
				s->ipv4 = srv->ipv4;
				s->src = s->srcbuf;
				s->f = srv->f;
				s->v = srv->v;
				s->hidx = idx;
				tpool_schedule(h->tp, &epoll_run, s);
				continue;
			}

			s = (session *)events[i].data.ptr;

			if (events[i].events & EPOLLRDHUP)
				s->disconnected = 1;

			if (s->disconnected) {
				sb_int_del(h->fds, s->fd);
				h->use--;
				tpool_schedule(h->tp, &epoll_close, s);
			}
			else
				tpool_schedule(h->tp, &epoll_run, s);
		}
	}

	close(h->fd);
	return 1;
}

#endif

#if defined(POLLIN) && !defined(__linux__) && !defined(BSD)

static int poll_accept(void *data)
{
	if (g_debug)
		printf("*** POLL: accept %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	s->f(s, s->v);
	s->h->rpollfds[s->idx].fd = s->fd;

	if (s->blocked)
		s->h->rpollfds[s->idx].events = POLLOUT | POLLRDHUP;
	else
		s->h->rpollfds[s->idx].events = POLLIN | POLLRDHUP;

	s->h->rpollfds[s->idx].revents = 0;
	return 1;
}

static int poll_close(void *data)
{
	if (g_debug)
		printf("*** POLL: close %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	s->f(s, s->v);
	close(s->fd);
	session_unshare(s);
	return 1;
}

static int poll_run(void *data)
{
	if (g_debug)
		printf("*** POLL: run %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	s->h->rpollfds[s->idx].fd = s->fd;

	while (s->f(s, s->v))
		;

	if (!s->disconnected) {
		if (s->blocked)
			s->h->rpollfds[s->idx].events = POLLOUT | POLLRDHUP;
		else
			s->h->rpollfds[s->idx].events = POLLIN | POLLRDHUP;

		s->h->rpollfds[s->idx].revents = 0;
	}
	else if (s->is_tcp) {
		sb_int_del(s->h->fds, s->fd);
		s->h->use--;
		session_unshare(s);
	}

	if (!s->is_tcp)
		free(s);

	return 1;
}

int handler_wait_poll(handler *h, int wait)
{
	if (g_debug)
		printf("*** USING POLL\n");

	int i;

	for (i = 0; i < h->cnt; i++) {
		h->rpollfds[i].fd = h->srvs[i].fd;
		h->rpollfds[i].events = POLLIN;
		h->rpollfds[i].revents = 0;
	}

	int cnt = i;

	while (!g_abort && !h->halt && (h->use || wait)) {
		int n = poll(h->rpollfds, cnt, 10);

		for (int i = 0; (i < cnt) && n; i++) {
			session *s = NULL;
			int fd = h->rpollfds[i].fd;

			if (fd == -1)
				continue;

			if ((i < h->cnt) && (h->rpollfds[i].revents & POLLIN)) {
				server *srv = &h->srvs[i];

				if (srv->is_tcp) {
					int newfd = handler_accept(h, srv, &s);

					if (newfd == -1)
						continue;

					s->idx = cnt++;
					tpool_schedule(h->tp, &poll_accept, s);
					h->use++;
					continue;
				}

				session *s = session_create();
				s->h = h;
				s->fd = srv->fd;
				s->port = srv->port;
				s->ipv4 = srv->ipv4;
				s->src = s->srcbuf;
				s->f = srv->f;
				s->v = srv->v;
				s->idx = i;
				h->rpollfds[i].fd = -1;
				tpool_schedule(h->tp, &poll_run, s);
				continue;
			}

			if (!sb_int_get(h->fds, fd, &s))
				continue;

#ifdef _WIN32
			// WSAPoll doesn't support POLLRDHUP...

			if (!s->disconnected) {
				char buf;
				int rlen = recv(s->fd, &buf, 1, MSG_PEEK);

				if (rlen == 0)
					s->disconnected = 1;

				if ((rlen < 0) && (errno != EAGAIN) && (errno != EWOULDBLOCK))
					s->disconnected = 1;
			}
#else
			if (h->rpollfds[i].revents & POLLRDHUP)
				s->disconnected = 1;
#endif

			if (s->disconnected) {
				sb_int_del(h->fds, s->fd);
				h->use--;
				h->rpollfds[i--] = h->rpollfds[--cnt]; // fill the gap
				tpool_schedule(h->tp, &poll_close, s);
			}
			else if (h->rpollfds[i].revents & POLLIN) {
				s->idx = i;
				h->rpollfds[i].fd = -1;
				tpool_schedule(h->tp, &poll_run, s);
			}
		}
	}

	return 1;
}

#endif

static int handler_select_set(void *_h, int fd, void *_s)
{
	if (fd == -1)
		return 1;

	session *s = (session *)_s;
	handler *h = (handler *)_h;

	if (s->blocked)
		FD_SET(fd, &h->wfds);
	else
		FD_SET(fd, &h->rfds);

	if (fd > h->hi)
		h->hi = fd;

	return 1;
}

static int select_accept(void *data)
{
	if (g_debug)
		printf("*** SELECT: accept %d %d\n", ((session *)data)->fd, ((session *)data)->fd);

	session *s = (session *)data;
	s->f(s, s->v);
	s->h->use++;
	return 1;
}

static int select_close(void *data)
{
	if (g_debug)
		printf("*** SELECT: close %d\n", ((session *)data)->fd);

	session *s = (session *)data;
	s->f(s, s->v);
	close(s->fd);
	session_unshare(s);
	return 1;
}

static int select_run(void *data)
{
	if (g_debug)
		printf("*** SELECT: run %d\n", ((session *)data)->fd);

	session *s = (session *)data;

	while (s->f(s, s->v))
		;

	if (!s->disconnected) {
		handler_select_set(s->h, s->fd, s);
		s->h->srvs[s->idx].fd = s->fd;
		s->busy = 0;
	}
	else if (s->is_tcp) {
		sb_int_del(s->h->fds, s->fd);
		s->h->use--;
		session_unshare(s);
	}

	if (!s->is_tcp)
		free(s);

	return 1;
}

static int handler_select_check(void *_h, int fd, void *_s)
{
	handler *h = (handler *)_h;
	session *s = (session *)_s;

	if (s->busy != 0)
		return 1;

	if (!s->disconnected) {
		char buf;
		int rlen = recv(s->fd, &buf, 1, MSG_PEEK);

		if (rlen == 0)
			s->disconnected = 1;

		if ((rlen < 0) && (errno != EAGAIN) && (errno != EWOULDBLOCK))
			s->disconnected = 1;

		if (s->disconnected) {
			sb_int_set(h->badfds, fd, s);
			return 1;
		}
	}

	if (!FD_ISSET(fd, &h->rfds))
		return 1;

	s->busy = 1;
	tpool_schedule(s->h->tp, &select_run, s);
	return 1;
}

static int handler_select_bads(void *_h, int fd, void *_s)
{
	handler *h = (handler *)_h;
	session *s = (session *)_s;
	sb_int_del(h->fds, fd);
	h->use--;
	tpool_schedule(s->h->tp, &select_close, s);
	return 1;
}

int handler_wait_select(handler *h, int wait)
{
	if (g_debug)
		printf("*** USING SELECT\n");

	h->badfds = sb_int_create();

	while (!g_abort && !h->halt && (h->use || wait)) {
		FD_ZERO(&h->rfds);
		FD_ZERO(&h->wfds);
		h->hi = 0;

		for (int i = 0; i < h->cnt; i++)
			handler_select_set(h, h->srvs[i].fd, NULL);

		sb_int_iter(h->fds, &handler_select_set, h);

		// It would be better to wake select with a signal

		struct timeval tv = {0, 1000 * 10};
		int n = select(h->hi + 1, &h->rfds, &h->wfds, 0, &tv);
		int cnt = h->cnt;

		for (int i = 0; (i < h->cnt) && n; i++) {
			server *srv = &h->srvs[i];

			if (!FD_ISSET(srv->fd, &h->rfds))
				continue;

			if (srv->is_tcp) {
				session *s = NULL;
				int newfd = handler_accept(h, srv, &s);

				if (newfd == -1)
					continue;

				s->busy = 1;
				s->idx = cnt++;
				tpool_schedule(h->tp, &select_accept, s);
				continue;
			}

			session *s = session_create();
			s->h = h;
			s->fd = srv->fd;
			s->port = srv->port;
			s->ipv4 = srv->ipv4;
			s->src = s->srcbuf;
			s->f = srv->f;
			s->v = srv->v;
			s->busy = 1;
			s->idx = i;
			h->srvs[i].fd = -1;
			tpool_schedule(h->tp, &select_run, s);
		}

		sb_int_iter(h->fds, &handler_select_check, h);

		if (sb_count(h->badfds) > 0) {
			sb_int_iter(h->badfds, &handler_select_bads, h);
			sb_destroy(h->badfds);
			h->badfds = sb_int_create();
		}
	}

	sb_destroy(h->badfds);
	return 1;
}

// Use the platform-best option...

static int handler_wait2(handler *h, int wait)
{
#if defined(BSD)
	return handler_wait_kqueue(h, wait);
#elif defined(__linux__)
	return handler_wait_epoll(h, wait);
#elif defined(POLLIN)
	return handler_wait_poll(h, wait);
#else
	return handler_wait_select(h, wait);
#endif
}

int handler_wait_indefinitely(handler *h) { return handler_wait2(h, 1); }

int handler_wait(handler *h) { return handler_wait2(h, 0); }

int handler_set_tls(handler *h, const char *keyfile, const char *certfile)
{
#if USE_SSL
	if (!g_ssl_init) {
		g_ssl_init = 1;
		// SSL_load_error_strings();
		SSL_library_init();
	}

	if (h->ctx) // already done
		return 1;

	h->ctx = SSL_CTX_new(TLS_method());

	if (!h->ctx) {
		printf("SSL new server context failed\n");
		ERR_print_errors_fp(stderr);
		return 0;
	}

	SSL_CTX_set_options(h->ctx, SSL_OP_NO_SSLv3 | SSL_OP_NO_SSLv2 | SSL_OP_CIPHER_SERVER_PREFERENCE);
	SSL_CTX_set_cipher_list(h->ctx, DEFAULT_CIPHERS);

	if (keyfile && certfile) {
		if (!SSL_CTX_use_RSAPrivateKey_file(h->ctx, (char *)keyfile, SSL_FILETYPE_PEM))
			printf("SSL load RSA key failed\n");

		if (!SSL_CTX_use_certificate_file(h->ctx, (char *)certfile, SSL_FILETYPE_PEM))
			printf("SSL load certificate failed\n");

		if (!SSL_CTX_load_verify_locations(h->ctx, (char *)certfile, (char *)NULL)) {
			printf("SSL set_load_verify_locations failed\n");

			if (!SSL_CTX_set_default_verify_paths(h->ctx))
				printf("SSL set_default_verify_paths faile\n");
		}
	}

	return 1;
#else
	return 0;
#endif
}

int handler_set_ca(handler *h, const char *cafile, const char *capath)
{
#if USE_SSL

	if (cafile) {
		if (!SSL_CTX_load_verify_locations(h->ctx, (char *)cafile, (char *)capath)) {
			printf("SSL set_load_verify_locations failed\n");

			if (!SSL_CTX_set_default_verify_paths(h->ctx))
				printf("SSL set_default_verify_paths faile\n");
		}
	}

	return 1;
#else
	return 0;
#endif
}

static int join_multicast6(server *srv, const char *addr)
{
	struct sockaddr_in6 addr6 = {0};
	addr6.sin6_family = AF_INET6;
	int status = 0;

	if (parse_addr6(addr, &addr6)) {
		struct ipv6_mreq mreq;
		memcpy(&mreq.ipv6mr_multiaddr, &addr6.sin6_addr, sizeof(struct in6_addr));
		mreq.ipv6mr_interface = 0;
		status = setsockopt(srv->fd, IPPROTO_IPV6, IPV6_JOIN_GROUP, (char *)&mreq, sizeof(mreq));
	}

	return status;
}

static int join_multicast4(server *srv, const char *addr)
{
	struct sockaddr_in addr4 = {0};
	addr4.sin_family = AF_INET;
	int status = 0;

	if (parse_addr4(addr, &addr4)) {
		struct ip_mreq mreq;
		memcpy(&mreq.imr_multiaddr, &addr4.sin_addr, sizeof(struct in_addr));
		mreq.imr_interface.s_addr = htonl(INADDR_ANY);
		status = setsockopt(srv->fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char *)&mreq, sizeof(mreq));
	}

	return status;
}

#if 0
static int leave_multicast6(int fd, const char *addr)
{
	struct sockaddr_in6 addr6 = {0};
	addr6.sin6_family = AF_INET6;
	int status = 0;

	if (parse_addr6(addr, &addr6)) {
		struct ipv6_mreq mreq;
		memcpy(&mreq.ipv6mr_multiaddr, &addr6.sin6_addr, sizeof(struct in6_addr));
		mreq.ipv6mr_interface = 0;
		status = setsockopt(fd, IPPROTO_IPV6, IPV6_LEAVE_GROUP, (char*)&mreq, sizeof(mreq));
	}

	return status;
}

static int leave_multicast4(int fd, const char *addr)
{
	struct sockaddr_in addr4 = {0};
	addr4.sin_family = AF_INET;
	int status = 0;

	if (parse_addr4(addr, &addr4)) {
		struct ip_mreq mreq;
		memcpy(&mreq.imr_multiaddr, &addr4.sin_addr, sizeof(struct in_addr));
		mreq.imr_interface.s_addr = htonl(INADDR_ANY);
		status = setsockopt(fd, IPPROTO_IP, IPV6_DROP_MEMBERSHIP, (char*)&mreq, sizeof(mreq));
	}

	return status;
}
#endif

extern uncle *uncle_create2(handler *h, const char *binding, unsigned port, const char *scope, const char *maddr6,
                            const char *maddr4);

int handler_add_uncle(handler *h, const char *binding, unsigned port, const char *scope)
{
	uncle *u = uncle_create2(h, binding, port, scope, NULL, NULL);

	if (!u)
		return 0;

	h->u[h->uncs++] = u;
	return 1;
}

uncle *handler_get_uncle(handler *h, const char *scope)
{
	for (int i = 0; i < h->uncs; i++) {
		if (!strcmp(uncle_get_scope(h->u[i]), scope))
			return h->u[i];
	}

	return NULL;
}

static int handler_add_server2(handler *h, int (*f)(session *, void *v), void *v, const char *binding, unsigned port, int tcp,
                               int ssl, int pri, const char *maddr6, const char *maddr4, const char *name)
{
	int fd6 = socket(AF_INET6, tcp ? SOCK_STREAM : SOCK_DGRAM, 0);

	if (fd6 != -1) {
#ifdef SO_REUSEADDR
		int flag = 1;
		setsockopt(fd6, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
#endif
#ifdef SO_REUSEPORT
		int flag2 = 1;
		setsockopt(fd6, SOL_SOCKET, SO_REUSEPORT, (char *)&flag2, sizeof(flag2));
#endif
#ifdef IPV6_V6ONLY
		int flag3 = 1;
		setsockopt(fd6, IPPROTO_IPV6, IPV6_V6ONLY, (char *)&flag3, sizeof(flag3));
#endif

		struct sockaddr_in6 addr6 = {0};
		addr6.sin6_family = AF_INET6;
		addr6.sin6_port = htons(port);
		const struct in6_addr my_in6addr_any = IN6ADDR_ANY_INIT;
		addr6.sin6_addr = my_in6addr_any;

		if (bind(fd6, (struct sockaddr *)&addr6, sizeof(addr6)) != 0) {
			printf("handler_add_server: warning bind6 failed port "
			       "%u: %s\n",
			       port, strerror(errno));
			close(fd6);
			fd6 = -1;
			return 0;
		}

		if (!tcp) {
			unsigned long flag2 = 1;
			ioctl(fd6, FIONBIO, &flag2);
		}
		else {
			if (listen(fd6, 128) != 0) {
				printf("handler_add_server: error listen6 "
				       "failed port: %u: %s\n",
				       port, strerror(errno));
				close(fd6);
				return 0;
			}
		}

		if (port == 0) {
			socklen_t len = sizeof(addr6);
			getsockname(fd6, (struct sockaddr *)&addr6, &len);
			port = ntohs(addr6.sin6_port);
		}

		lock_lock(h->strand);
		server *srv = &h->srvs[h->cnt++];
		lock_unlock(h->strand);
		srv->name = strdup(name ? name : DEFAULT_NAME);
		srv->fd = fd6;
		srv->port = port;
		srv->pri = pri;
		srv->is_tcp = tcp;

#if USE_SSL
		srv->is_ssl = ssl && tcp && h->ctx;
#else
		srv->is_ssl = 0;
#endif

		srv->ipv4 = 0;
		srv->f = f;
		srv->v = v;

		if (maddr6)
			join_multicast6(srv, maddr6);
	}

	int fd4 = socket(AF_INET, tcp ? SOCK_STREAM : SOCK_DGRAM, 0);

	if (fd4 != -1) {
#ifdef SO_REUSEADDR
		int flag = 1;
		setsockopt(fd4, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
#endif
#ifdef SO_REUSEPORT
		int flag2 = 1;
		setsockopt(fd4, SOL_SOCKET, SO_REUSEPORT, (char *)&flag2, sizeof(flag2));
#endif

		struct sockaddr_in addr4 = {0};
		addr4.sin_family = AF_INET;
		addr4.sin_port = htons(port);
		addr4.sin_addr.s_addr = htonl(INADDR_ANY);

		if (bind(fd4, (struct sockaddr *)&addr4, sizeof(addr4)) != 0) {
			printf("handler_add_server: warning bind4 failed port "
			       "%u: %s\n",
			       port, strerror(errno));
			close(fd4);
			fd4 = -1;
			return 0;
		}

		if (!tcp) {
			unsigned long flag2 = 1;
			ioctl(fd4, FIONBIO, &flag2);
		}
		else {
			if (listen(fd4, 128) != 0) {
				printf("handler_add_server: error listen4 "
				       "failed port:%u %s\n",
				       port, strerror(errno));
				close(fd4);
				return 0;
			}
		}

		if (port == 0) {
			socklen_t len = sizeof(addr4);
			getsockname(fd4, (struct sockaddr *)&addr4, &len);
			port = ntohs(addr4.sin_port);
		}

		lock_lock(h->strand);
		server *srv = &h->srvs[h->cnt++];
		lock_unlock(h->strand);
		srv->name = strdup(name ? name : DEFAULT_NAME);
		srv->fd = fd4;
		srv->port = port;
		srv->pri = pri;
		srv->is_tcp = tcp;

#if USE_SSL
		srv->is_ssl = ssl && tcp && h->ctx;
#else
		srv->is_ssl = 0;
#endif

		srv->ipv4 = 1;
		srv->f = f;
		srv->v = v;

		if (maddr4)
			join_multicast4(srv, maddr4);
	}

	if (name && name[0] && h->uncs) {
		uncle *u = h->u[h->uncs - 1];
		uncle_add(u, name, hostname(), port, tcp, ssl, pri);
	}

	h->use++;
	return 1;
}

int handler_add_server(handler *h, int (*f)(session *, void *v), void *v, const char *binding, unsigned port, int tcp, int ssl,
                       int pri, const char *name)
{
	return handler_add_server2(h, f, v, binding, port, tcp, ssl, pri, NULL, NULL, name);
}

int handler_add_client(handler *h, int (*f)(session *, void *data), void *data, session *s)
{
	session_share(s);
	s->h = h;
	s->f = f;
	s->v = data;
	s->handled = 1;
	h->use++;
	sb_int_set(h->fds, s->fd, s);
	unsigned long flag2 = 1;
	ioctl(s->fd, FIONBIO, &flag2);
	s->is_nonblocking = flag2;

#if defined(BSD)
	tpool_schedule(h->tp, &kqueue_accept, s);
#elif defined(__linux__)
	tpool_schedule(h->tp, &epoll_accept, s);
#elif defined(POLLIN)
	tpool_schedule(h->tp, &poll_accept, s);
#else
	tpool_schedule(h->tp, &select_accept, s);
#endif

	return 1;
}

int handler_add_tpool(handler *h, tpool *tp)
{
	if (h->tp)
		tpool_destroy(h->tp);

	h->tp = tp;
	h->tp_created = 0;
	return 1;
}

int handler_add_multicast(handler *h, int (*f)(session *, void *v), void *v, const char *binding, unsigned port,
                          const char *addr6, const char *addr4, const char *name)
{
	return handler_add_server2(h, f, v, binding, port, 0, 0, 0, addr6, addr4, name);
}

handler *handler_create(int threads)
{
#ifdef _WIN32
	static int cnt = 0;

	if (!cnt++) {
		WORD wVersionRequested = MAKEWORD(2, 2);
		WSADATA wsaData;

		if (WSAStartup(wVersionRequested, &wsaData) != 0) {
			WSACleanup();
			return NULL;
		}
	}
	else
		cnt = 1;
#endif

	handler *h = (handler *)calloc(1, sizeof(handler));
	h->fds = sb_int_create();

	if (threads >= 0) {
		h->tp = tpool_create(threads);
		h->tp_created = 1;
	}

	h->strand = lock_create();

#if defined(BSD)
	h->fd = kqueue();

	if (h->fd < 0)
		abort();
#elif defined(__linux__)
	h->fd = epoll_create(1);

	if (h->fd < 0)
		abort();
#endif

	return h;
}

int handler_shutdown(handler *h)
{
	h->halt = 1;
	return 1;
}

void handler_destroy(handler *h)
{
	h->halt = 1;
	msleep(10);

	for (int i = 0; i < h->uncs; i++)
		uncle_destroy(h->u[i]);

	for (int i = 0; i < h->cnt; i++)
		free(h->srvs[i].name);

	if (h->tp && h->tp_created)
		tpool_destroy(h->tp);

	lock_destroy(h->strand);

	sb_int_iter(h->fds, &handler_force_drop, h);
	sb_destroy(h->fds);

#if USE_SSL
	if (h->ctx)
		SSL_CTX_free(h->ctx);
#endif

	free(h);
}
