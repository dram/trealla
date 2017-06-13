#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#define msleep Sleep
#else
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#define msleep(ms)                                                                                                             \
	{                                                                                                                          \
		struct timespec tv;                                                                                                    \
		tv.tv_sec = (ms) / 1000;                                                                                               \
		tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;                                                                              \
		nanosleep(&tv, &tv);                                                                                                   \
	}
#endif

#include "daemon.h"

volatile int g_stopping = 0;

int daemonize(int argc, char *argv[])
{
	char path[1024];
	path[0] = '\0';
	int watchdog = 0;

	for (int i = 0; i < argc; i++) {
		if (!strcmp(argv[i], "-w") || !strcmp(argv[i], "--watchdog"))
			watchdog = 1;
		else if (!strncmp(argv[i], "--cd=", 5))
			strcpy(path, argv[i] + 5);
	}

#ifdef _WIN32
	char cmd[1024], args[1024 * 8];
	args[0] = 0;
	strcpy(cmd, argv[0]);
	strcat(cmd, ".exe");

	for (int i = 0; i < argc; i++) {
		if (!strcmp(argv[i], "-d") || !strcmp(argv[i], "--daemon"))
			continue;

		if (!args[0])
			strcat(args, " ");

		strcat(args, "\"");
		strcat(args, argv[i]);
		strcat(args, "\"");
	}

	STARTUPINFO startInfo = {0};
	PROCESS_INFORMATION process = {0};

	startInfo.cb = sizeof(startInfo);
	startInfo.dwFlags = STARTF_USESHOWWINDOW;
	startInfo.wShowWindow = SW_HIDE;

	if (!CreateProcessA((LPSTR)cmd,              // application name
	                    (LPSTR)args,             // command line arguments
	                    NULL,                    // process attributes
	                    NULL,                    // thread attributes
	                    FALSE,                   // inherit (file) handles
	                    DETACHED_PROCESS,        // Detach
	                    NULL,                    // environment
	                    (path[0] ? path : NULL), // current directory
	                    &startInfo,              // startup info
	                    &process)                // process information
	    ) {
		printf("Creation of the process failed\n");
		return 1;
	}

	return 0;
#else
	pid_t pid;

	if ((pid = fork()) < 0) // Error
		return -1;
	else if (pid != 0) // Parent
		return 0;

	if (watchdog)
		signal(SIGCHLD, SIG_IGN);

	while (watchdog && !g_stopping) {
		pid_t pid;

		if ((pid = fork()) < 0) // Error
			return -1;
		else if (pid != 0) // Parent
		{
			if (watchdog) {
				int status;
				wait(&status);

				if (g_stopping)
					return 0;

				msleep(1000);
			}
			else
				return 0;
		}
		else // Child
			break;
	}

	if (path[0])
		if (chdir(path) < 0)
			printf("yxtrang can't chdir(%s)\n", path);

	setsid();
	umask(0);
	close(2);
	close(1);
	close(0);
	return 1;
#endif
}
