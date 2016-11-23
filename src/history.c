// This should be fixed to work with ANSI code page on Windows
// TO-DO: CTRL-K delete to end of the line

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#ifdef _WIN32
#define snprintf _snprintf
#include <windows.h>
#else
#include <unistd.h>
#include <termios.h>
#endif

#include "history.h"

typedef struct cmd_ cmd;
struct cmd_ { char *line; cmd *prev, *next; };
static cmd *g_history = NULL;
static FILE *g_histfile = NULL;
static char *g_last = NULL;
static const char **key_words = NULL;

#define bold "\e[1m"
#define normal "\e[0m"
#define blink "\e[5m"
#define negative "\e[7m"
#define italic "\e[3m"
#define red "\e[31m"

#ifdef _WIN32
static int history_getch2(int *alt)
{
	HANDLE hStdin = GetStdHandle(STD_INPUT_HANDLE);
	INPUT_RECORD irInputRecord;
	DWORD dwEventsRead;
	CHAR cChar;
	if (alt) *alt = '\0';

	while (ReadConsoleInputA(hStdin, &irInputRecord, 1, &dwEventsRead))
	{
		if ((irInputRecord.EventType == KEY_EVENT) &&
			(irInputRecord.Event.KeyEvent.wVirtualKeyCode != VK_SHIFT) &&
			(irInputRecord.Event.KeyEvent.wVirtualKeyCode != VK_MENU) &&
			(irInputRecord.Event.KeyEvent.wVirtualKeyCode != VK_CONTROL))
		{
			cChar = irInputRecord.Event.KeyEvent.uChar.AsciiChar;

			if ((cChar == '\0') && alt)
				*alt = irInputRecord.Event.KeyEvent.wVirtualKeyCode;

			ReadConsoleInputA(hStdin, &irInputRecord , 1, &dwEventsRead);
			return cChar;
		}
	}

	return EOF;
}
#else
static int history_getch2(int *alt)
{
	if (alt) *alt = '\0';
	struct termios oldattr, newattr;
	tcgetattr(STDIN_FILENO, &oldattr);
	newattr = oldattr;
	newattr.c_lflag &= ~(ICANON|ECHO);
	tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
	int ch = getchar();
	tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);
	return ch;
}
#endif

int history_getch(void)
{
	return history_getch2(NULL);
}

char *history_readline(const char* prompt)
{
	return history_readline_eol(prompt, '\0');
}

static void output(const char *fmt, const char *prompt, const char *line)
{
	size_t len = (strlen(line)*10)+1024;
	char *dstbuf = (char*)malloc(len);
	const char *src = line;
	char *dst = dstbuf;
	char last_ch = ' ';

	while (*src && len)
	{
		if (key_words && (ispunct(last_ch) || isspace(last_ch)))
		{
#ifndef _WIN32
			for (int i = 0; key_words[i]; i++)
			{
				if (!strncmp(src, key_words[i], strlen(key_words[i])))
				{
					char ch = src[strlen(key_words[i])];

					if (ispunct(ch) || isspace(ch))
					{
						size_t n = snprintf(dst, len, "%s%s%s", italic, key_words[i], normal);
						dst += n;
						len -= n;
						src += strlen(key_words[i]);
						continue;
					}
				}
			}
#endif
		}

		last_ch = *src;
		*dst++ = *src++;
		len--;
	}

	*dst = '\0';
	printf(fmt, prompt, dstbuf);
	free(dstbuf);
}

void history_output(const char *prompt, const char *line)
{
	output("%s%s\n", prompt, line);
}

char *history_readline_eol(const char* prompt, char eol)
{
	static const char *save_prompt;
	static char tmp_prompt[1024];
	const int DEF_BLOCK_SIZE = 1024;
	size_t block_size = DEF_BLOCK_SIZE;
	char *line = (char*)malloc(block_size);
	char *dst = line;
	char escbuf[20], search[1024];
	char *escdst = escbuf;
	int escape = 0, is_insert = 0, reverse_search = 0;
	cmd *last = NULL;
	int alt, tmp;

	printf("%s", prompt);
	fflush(stdout);

	while ((tmp = history_getch2(&alt)) != EOF)
	{
		unsigned char ch = (unsigned char)tmp;

		//printf("%02X (%02X) ", tmp, (char)alt);

#ifdef _WIN32
		if (ch == VK_BACK)
#else
		if ((ch == 0x7f) || (ch == 0x08))
#endif
		{
			if (dst != line)
			{
				dst--;

				if (is_insert)
				{
					char *end = dst;
					char *src = dst+1;

					while (src != (line+strlen(line)))
						*end++ = *src++;

					*end = '\0';
					printf("\b%s ", dst);

					for (int i = 0; i < (strlen(dst)+1); i++)
						putchar('\b');
				}
				else
				{
					*dst = '\0';
					printf("\b \b");
				}

				fflush(stdout);
			}

			continue;
		}

#ifndef _WIN32
		if (ch == '\e')
		{
			escdst = escbuf;
			escape = 1;
		}
#endif

		if (!escape && (ch == '\x0B'))						// CTRL-K (erase to end)
		{
			int len = strlen(dst);

			for (int i = 0; i < len; i++)
				putchar(' ');

			for (int i = 0; i < len; i++)
				putchar('\b');

			fflush(stdout);
			is_insert = 0;
			continue;
		}

		if (!escape && (ch == '\x01'))						// CTRL-A (start of line)
		{
			int len = (int)(dst-line);

			for (int i = 0; i < len; i++)
				putchar('\b');

			fflush(stdout);
			dst = line;
			is_insert = 1;
			continue;
		}

		if (!escape && (ch == '\x05'))						// CTRL-E (end of line)
		{
			int len = (int)strlen(dst);

			for (int i = 0; i < len; i++)
				putchar(*dst++);

			fflush(stdout);
			dst = line+strlen(line);
			is_insert = 0;
			continue;
		}

		if (!escape && (ch == '\x12'))						// CTRL-R
		{
			if (!g_history)
				continue;

			if (!reverse_search)
				save_prompt = prompt;

			prompt = "(reverse-search) \"\" : ";
			strcpy(tmp_prompt, prompt);
			output("\r%s%s", prompt, "");
			reverse_search = 1;
			search[0] = '\0';
			continue;
		}

		if (reverse_search)
		{
#ifdef _WIN32
			if (ch == VK_RETURN)
#else
			if (ch == '\n')
#endif
			{
				prompt = save_prompt;
				reverse_search = 0;
				puts("\r");
				break;
			}

			if (!g_history)
				continue;

			char tmpbuf[2];
			tmpbuf[0] = ch;
			tmpbuf[1] = '\0';
			strcat(search, tmpbuf);
			snprintf(tmp_prompt, sizeof(tmp_prompt), "(reverse-search) \"%s\" : ", search);
			prompt = tmp_prompt;

			if (!last)
				last = g_history;
			else
				last = last->next;

			while (last)
			{
				if (strstr(last->line, search))
					break;

				last = last->next;
			}

			if (!last)
			{
				reverse_search = 0;
				continue;
			}

			free(line);
			line = strdup(last->line);
#ifdef _WIN32
			int curr_len = strlen(line);
			output("\r%s%s", prompt, line);

			for (int i = 0; i < curr_len; i++)
				putchar(' ');

			for (int i = 0; i < curr_len; i++)
				putchar('\b');
#else
			output("\r%s%s\e[K", prompt, line);
#endif
			fflush(stdout);
			dst = line+strlen(line);
			continue;
		}

		if (!escape)
		{
			unsigned char ch2 = ch;
#ifdef _WIN32
			if (ch == VK_RETURN)
				ch2 = '\n';
#endif
			putchar(ch2);
			fflush(stdout);
		}

#ifdef _WIN32
		if ((ch == VK_RETURN) &&
#else
		if ((ch == '\n') &&
#endif
			(!eol || (line[strlen(line)-1] == eol)))
			break;

#ifdef _WIN32
		if (ch == VK_RETURN)
#else
		if (ch == '\n')
#endif
		{
			*dst++ = '\n';
			printf(" |\t");
			fflush(stdout);
			continue;
		}

		if (!escape && is_insert)
		{
			char *end = line+strlen(line);
			char *src = end-1;
			end[1] = '\0';

			while (src >= dst)
				*end-- = *src--;

			printf("%s", dst+1);

			for (int i = 0; i < strlen(dst+1); i++)
				putchar('\b');

			fflush(stdout);
		}

		if (escape)
		{
			if ((escdst-escbuf) < sizeof(escbuf))
			{
				*escdst++ = ch;
				*escdst = '\0';
			}
		}
		else
		{
			*dst++ = ch;

			size_t n = dst - line;

			if (n == block_size)
			{
				line = (char*)realloc(line, block_size+=DEF_BLOCK_SIZE);
				dst = line+n;
			}

			if (!is_insert)
				*dst = '\0';
		}

#ifdef _WIN32
		if (!ch && (alt == VK_DELETE))						// DELETE
		{
			escape = 0;

			if (!is_insert)
				continue;

			if (dst == (line+strlen(line)))
				continue;

			for (int i = 0; i < strlen(dst+1); i++)
				dst[i] = dst[i+1];

			dst[strlen(dst)-1] = '\0';
			printf("%s ", dst);

			for (int i = 0; i < (strlen(dst)+1); i++)
				putchar('\b');

			fflush(stdout);
			continue;
		}
#endif

#ifndef _WIN32
		if (escape && !strcmp(escbuf, "\e[3~"))				// DELETE
		{
			escape = 0;

			if (!is_insert)
				continue;

			if (dst == (line+strlen(line)))
				continue;

			for (int i = 0; i < strlen(dst+1); i++)
				dst[i] = dst[i+1];

			dst[strlen(dst)-1] = '\0';
			printf("%s ", dst);

			for (int i = 0; i < (strlen(dst)+1); i++)
				putchar('\b');

			fflush(stdout);
			continue;
		}
#endif

#ifdef _WIN32
		if (!ch && (alt == VK_HOME))
		{
			escape = 0;
			int len = (int)(dst-line);

			for (int i = 0; i < len; i++)
				putchar('\b');

			fflush(stdout);
			dst = line;
			is_insert = 1;
			continue;
		}
#endif

#ifndef _WIN32
		if (escape && !strcmp(escbuf, "\e[H"))				// HOME
		{
			escape = 0;
			printf("\e[%dD", (int)(dst-line));
			fflush(stdout);
			dst = line;
			is_insert = 1;
			continue;
		}
#endif

#ifdef _WIN32
		if (!ch && (alt == VK_END))							// END
		{
			escape = 0;
			int len = (int)strlen(dst);

			for (int i = 0; i < len; i++)
				putchar(*dst++);

			fflush(stdout);
			dst = line+strlen(line);
			is_insert = 0;
			continue;
		}
#endif

#ifndef _WIN32
		if (escape && !strcmp(escbuf, "\e[F"))				// END
		{
			escape = 0;
			output("\r%s%s\e[K", prompt, line);
			fflush(stdout);
			dst = line+strlen(line);
			is_insert = 0;
			continue;
		}
#endif

#ifdef _WIN32
		if (!ch && (alt == VK_UP))							// UP
		{
			escape = 0;

			if (!g_history)
				continue;

			if (!last)
				last = g_history;
			else if (!last->next)
				continue;
			else
				last = last->next;

			free(line);
			line = strdup(last->line);
			int curr_len = strlen(line);
			output("\r%s%s", prompt, line);

			for (int i = 0; i < curr_len; i++)
				putchar(' ');

			for (int i = 0; i < curr_len; i++)
				putchar('\b');
			fflush(stdout);
			dst = line+strlen(line);
			continue;
		}
#endif

#ifndef _WIN32
		if (escape && !strcmp(escbuf, "\e[A"))				// UP
		{
			escape = 0;

			if (!g_history)
				continue;

			if (!last)
				last = g_history;
			else if (!last->next)
				continue;
			else
				last = last->next;

			free(line);
			line = strdup(last->line);
			output("\r%s%s\e[K", prompt, line);
			fflush(stdout);
			dst = line+strlen(line);
			continue;
		}
#endif

#ifdef _WIN32
		if (!ch && (alt == VK_DOWN))						// DOWN
		{
			escape = 0;

			if (!g_history)
				continue;

			if (!last)
				last = g_history;
			else if (!last->prev)
				continue;
			else
				last = last->prev;

			free(line);
			line = strdup(last->line);
			int curr_len = strlen(line);
			output("\r%s%s", prompt, line);

			for (int i = 0; i < curr_len; i++)
				putchar(' ');

			for (int i = 0; i < curr_len; i++)
				putchar('\b');

			fflush(stdout);
			dst = line+strlen(line);
			continue;
		}
#endif

#ifndef _WIN32
		if (escape && !strcmp(escbuf, "\e[B"))				// DOWN
		{
			escape = 0;

			if (!g_history)
				continue;

			if (!last)
				last = g_history;
			else if (!last->prev)
				continue;
			else
				last = last->prev;

			free(line);
			line = strdup(last->line);
			output("\r%s%s\e[K", prompt, line);
			fflush(stdout);
			dst = line+strlen(line);
			continue;
		}
#endif

#ifdef _WIN32
		if (!ch && (alt == VK_RIGHT))						// RIGHT
		{
			escape = 0;

			if (dst == (line+strlen(line)))
				continue;

			putchar(*dst++);
			fflush(stdout);
			continue;
		}
#endif

#ifndef _WIN32
		if (escape && !strcmp(escbuf, "\e[C"))				// RIGHT
		{
			escape = 0;

			if (dst == (line+strlen(line)))
			{
				is_insert = 0;
				continue;
			}

			dst++;
			printf("%s", escbuf);
			fflush(stdout);
			continue;
		}
#endif

#ifdef _WIN32
		if (!ch && (alt == VK_LEFT))						// LEFT
		{
			escape = 0;

			if (dst == line)
				continue;

			dst--;
			putchar('\b');
			fflush(stdout);
			is_insert = 1;
			continue;
		}
#endif

#ifndef _WIN32
		if (escape && !strcmp(escbuf, "\e[D"))				// LEFT
		{
			escape = 0;

			if (dst == line)
				continue;

			dst--;
			printf("%s", escbuf);
			fflush(stdout);
			is_insert = 1;
			continue;
		}

		if (escape && !strcmp(escbuf, "\e[1;5C"))			// CTRL-RIGHT
		{
			escape = 0;

			if (dst == (line+strlen(line)))
			{
				is_insert = 0;
				continue;
			}

			int cnt = 0;

			while (*dst && !isalnum((*dst)))
			{
				dst++;
				cnt++;
			}

			while (*dst && isalnum((*dst)))
			{
				dst++;
				cnt++;
			}

			if (cnt)
			{
				printf("\e[%dC", cnt);
				fflush(stdout);
			}

			is_insert = 1;
			continue;
		}

		if (escape && !strcmp(escbuf, "\e[1;5D"))			// CTRL-LEFT
		{
			escape = 0;

			if (dst == line)
				continue;

			int cnt = 0;

			while ((dst != line) && !isalnum(*(dst-1)))
			{
				dst--;
				cnt++;
			}

			while ((dst != line) && !isalnum(*dst))
			{
				dst--;
				cnt++;
			}

			while ((dst != line) && isalnum((*(dst-1))))
			{
				dst--;
				cnt++;
			}

			if (cnt)
			{
				printf("\e[%dD", cnt);
				fflush(stdout);
			}

			is_insert = 1;
			continue;
		}
#endif

	}

	if (g_last)
	{
		if (!strcmp(g_last, line))
			return line;
	}

	if (g_histfile)
	{
		fprintf(g_histfile, "%s\n", line);
		fflush(g_histfile);
	}

	cmd *n = (cmd*)malloc(sizeof(cmd));
	n->line = strdup(line);
	n->next = g_history;
	n->prev = NULL;

	if (g_history)
		g_history->prev = n;

	g_history = n;

	if (g_last) free(g_last);
	g_last = strdup(n->line);
	return line;
}

void history_keywords(const char **word_array)
{
	key_words = word_array;
}

void history_load(const char *filename)
{
	g_histfile = fopen(filename, "a+");
	if (!g_histfile) return;
	char line[1024*8];

	while (fgets(line, sizeof(line), g_histfile) > 0)
	{
		line[strlen(line)-1] = '\0';
		cmd *n = (cmd*)malloc(sizeof(cmd));
		n->line = strdup(line);
		n->next = g_history;
		n->prev = NULL;

		if (g_history)
			g_history->prev = n;

		g_history = n;
		if (g_last) free(g_last);
		g_last = strdup(n->line);
	}
}

void history_save(void)
{
	while (g_history)
	{
		cmd *save = g_history;
		g_history = g_history->next;
		free(save);
	}

	if (g_histfile)
		fclose(g_histfile);

	if (g_last) free(g_last);
	g_histfile = NULL;
	g_history = NULL;
	g_last = NULL;
}

