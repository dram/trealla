#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <winsock2.h>
#else
#include <sys/time.h>
#endif

#include "uuid.h"

#if __STDC_VERSION__ >= 201112L
#include <stdatomic.h>
#define atomic _Atomic
#else
#define atomic volatile
#endif

#define MASK_FINAL 0x0000FFFFFFFFFFFF // Final 48 bits

static uint64_t s_seed = 0;

static void atomic_compare_and_zero(uint64_t v1, atomic uint64_t *v2, atomic uint64_t *v)
{
	// FIXME

	if (v1 != *v2) {
		*v2 = v1;
		*v = 0;
	}
}

uuid uuid_set(uint64_t v1, uint64_t v2)
{
	uuid tmp = {0};
	tmp.u1 = v1;
	tmp.u2 = v2;
	return tmp;
}

uint64_t uuid_ts(const uuid *u) { return u->u1; }

char *uuid_to_string(const uuid *u, char *buf)
{
	sprintf(buf, "%016llX:%04llX:%012llX", (unsigned long long)u->u1, (unsigned long long)(u->u2 >> 48),
	        (unsigned long long)(u->u2 & MASK_FINAL));

	return buf;
}

uuid *uuid_from_string(const char *s, uuid *u)
{
	if (!s)
		return u;
	unsigned long long p1 = 0, p2 = 0, p3 = 0;
	sscanf(s, "%llX:%llX:%llX", &p1, &p2, &p3);
	u->u1 = p1;
	u->u2 = p2 << 48;
	u->u2 |= p3 & MASK_FINAL;
	return u;
}

void uuid_seed(uint64_t v) { s_seed = v & MASK_FINAL; }

static uint64_t gettimeofday_usec(void)
#ifdef _WIN32
{
	static const uint64_t epoch = 116444736000000000ULL;
	FILETIME file_time;
	SYSTEMTIME system_time;
	ULARGE_INTEGER u;
	GetSystemTime(&system_time);
	SystemTimeToFileTime(&system_time, &file_time);
	u.LowPart = file_time.dwLowDateTime;
	u.HighPart = file_time.dwHighDateTime;
	return (u.QuadPart - epoch) / 10 + (1000ULL * system_time.wMilliseconds);
}
#else
{
	struct timeval tp;
	gettimeofday(&tp, NULL);
	return ((uint64_t)tp.tv_sec * 1000 * 1000) + tp.tv_usec;
}
#endif

uuid *uuid_gen(uuid *u)
{
	static atomic uint64_t s_last = 0, s_cnt = 0;

	if (!s_seed)
		uuid_seed(time(0));

	uint64_t now = gettimeofday_usec();
	atomic_compare_and_zero(now, &s_last, &s_cnt);
	u->u1 = now;
	u->u2 = s_cnt++;
	u->u2 <<= 48;
	u->u2 |= s_seed;
	return u;
}

uuid *uuid_copy(const uuid *v1)
{
	uuid *v2 = (uuid *)malloc(sizeof(struct uuid_));
	v2->u1 = v1->u1;
	v2->u2 = v1->u2;
	return v2;
}

int uuid_compare(const uuid *v1, const uuid *v2)
{
	if (v1->u1 < v2->u1)
		return -1;

	if (v1->u1 == v2->u1) {
		if (v1->u2 < v2->u2)
			return -1;
		else if (v1->u2 == v2->u2)
			return 0;
	}

	return 1;
}

int uuid_is_zero(const uuid *u) { return !u->u1 && !u->u2; }
