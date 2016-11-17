#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#include <process.h>
#else
#include <unistd.h>
#include <pthread.h>
#endif

#include "thread.h"
#include "list.h"

#ifdef _WIN32
#define msleep Sleep
#else
#define msleep(ms) usleep((ms)*1000)
#endif

extern volatile int g_abort;

typedef struct node_
{
	lnode hdr;
	thread_function *f;
	void *data;
}
 node;

typedef struct thread_
{
	node n;
	void *id;
	tpool *tp;
	volatile int halt;

#ifndef _WIN32
	pthread_cond_t cond;
	pthread_mutex_t mutex;
#endif
}
 thread;

struct lock_
{
#ifdef _WIN32
	CRITICAL_SECTION mutex;
#else
	pthread_mutex_t mutex;
#endif
};

struct tpool_
{
	list idle, queue;
	lock iguard, qguard;
};

static void lock_init(lock *l)
{
#ifdef _WIN32
	InitializeCriticalSection(&l->mutex);
#else
	pthread_mutex_init(&l->mutex, NULL);
#endif
}

static void lock_done(lock *l)
{
#ifndef _WIN32
	pthread_mutex_destroy(&l->mutex);
#endif
}

lock *lock_create()
{
	lock *l = (lock*)calloc(1, sizeof(lock));
	lock_init(l);
	return l;
}

void lock_destroy(lock *l)
{
	lock_done(l);
	free(l);
}

void lock_lock(lock *l)
{
#ifdef _WIN32
	EnterCriticalSection(&l->mutex);
#else
	pthread_mutex_lock(&l->mutex);
#endif
}

void lock_unlock(lock *l)
{
#ifdef _WIN32
	LeaveCriticalSection(&l->mutex);
#else
	pthread_mutex_unlock(&l->mutex);
#endif
}

int thread_run(thread_function *f, void *data)
{
#ifdef _WIN32
	SECURITY_ATTRIBUTES sa = {0};
	sa.nLength = sizeof (sa);
	sa.lpSecurityDescriptor = 0;
	sa.bInheritHandle = 0;
	typedef unsigned(_stdcall *start_routine_t)(void*);
	int id = _beginthreadex(&sa, 0, (start_routine_t)f, (LPVOID)data, 0, NULL);
	return id != 0;
#else
	typedef void *(*start_routine_t)(void*);
	pthread_attr_t sa;
	pthread_attr_init(&sa);
	pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
	pthread_t handle;
	int status = pthread_create(&handle, &sa, (start_routine_t)f, data);
	return status == 0;
#endif
}

static void *start_routine(thread *t)
{
	while (!g_abort && !t->halt)
	{
		if (list_count(&t->tp->queue))
		{
			lock_lock(&t->tp->qguard);
			node *n = (node*)list_pop_front(&t->tp->queue);
			lock_unlock(&t->tp->qguard);
			if (!n) continue;
			n->f((void*)n->data);
			free(n);
			continue;
		}

		lock_lock(&t->tp->iguard);
		list_push_back(&t->tp->idle, &t->n.hdr);
		lock_unlock(&t->tp->iguard);

#ifdef _WIN32
		SuspendThread((HANDLE)t->id);
#else
		pthread_mutex_lock(&t->mutex);
		pthread_cond_wait(&t->cond, &t->mutex);
		pthread_mutex_unlock(&t->mutex);
#endif

		if (!t->halt && t->n.f)
		{
			t->n.f((void*)t->n.data);
			t->n.f = NULL;
		}
	}

	return 0;
}

static thread *thread_create(tpool *tp)
{
	thread *t = (thread*)calloc(1, sizeof(thread));
	t->tp = tp;

#ifdef _WIN32
	SECURITY_ATTRIBUTES sa = {0};
	sa.nLength = sizeof (sa);
	sa.lpSecurityDescriptor = 0;
	sa.bInheritHandle = 0;
	typedef unsigned(_stdcall *start_routine_t)(void*);
	t->id = (void*)_beginthreadex(&sa, 0, (start_routine_t)start_routine, (LPVOID)t, 0, NULL);
#else
	pthread_cond_init(&t->cond, NULL);
	pthread_mutex_init(&t->mutex, NULL);
	typedef void *(*start_routine_t)(void*);
	pthread_attr_t sa;
	pthread_attr_init(&sa);
	pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
	pthread_t handle;
	pthread_create(&handle, &sa, (start_routine_t)start_routine, t);
	t->id = (void*)handle;
#endif

	return t;
}

static void thread_destroy(thread *t)
{
	t->halt = 1;

#ifdef _WIN32
	ResumeThread((HANDLE)t->id);
#else
	pthread_mutex_lock(&t->mutex);
	pthread_cond_signal(&t->cond);
	pthread_mutex_unlock(&t->mutex);
	pthread_cond_destroy(&t->cond);
	pthread_mutex_destroy(&t->mutex);
#endif

	msleep(1);
}

void tpool_queue(tpool *tp, thread_function *f, void *data)
{
	node *n = (node*)malloc(sizeof(node));
	n->f = f;
	n->data = data;
	lock_lock(&tp->qguard);
	list_push_back(&tp->queue, &n->hdr);
	lock_unlock(&tp->qguard);
}

void tpool_start(tpool *tp, thread_function *f, void *data)
{
	lock_lock(&tp->iguard);
	thread *t = (thread*)list_pop_front(&tp->idle);
	lock_unlock(&tp->iguard);

	if (!t)
	{
		tpool_queue(tp, f, data);
		return;
	}

	t->n.f = f;
	t->n.data = data;

#ifdef _WIN32
	ResumeThread((HANDLE)t->id);
#else
	pthread_mutex_lock(&t->mutex);
	pthread_cond_signal(&t->cond);
	pthread_mutex_unlock(&t->mutex);
#endif
}

tpool *tpool_create(int threads)
{
	tpool *tp = (tpool*)calloc(1, sizeof(tpool));
	const int MAX_THREADS = 16;

	if (threads > MAX_THREADS)
		threads = MAX_THREADS;
	else if (threads <= 0)
		threads = 1;

	lock_init(&tp->iguard);
	lock_init(&tp->qguard);

	for (int i = 0; i < threads; i++)
		thread_create(tp);

	while (list_count(&tp->idle) != threads)
		msleep(0);

	return tp;
}

void tpool_destroy(tpool *tp)
{
	lock_lock(&tp->iguard);
	thread *t;

	while ((t = (thread*)list_pop_front(&tp->idle)) != NULL)
	{
		thread_destroy(t);
		free(t);
	}

	lock_unlock(&tp->iguard);
	lock_done(&tp->iguard);

	node *n;
	lock_lock(&tp->qguard);

	while ((n = (node*)list_pop_front(&tp->queue)) != NULL)
		free(n);

	lock_unlock(&tp->qguard);
	lock_done(&tp->qguard);
	free(tp);
}
