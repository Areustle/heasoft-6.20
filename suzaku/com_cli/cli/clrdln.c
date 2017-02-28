/*
 CLrdln: Interface to GNU readline package
 Author: A.Shirahashi
 Date: 26-Apr-93

 History:
 	29-Sep-1994: READLINE_INCLUDE, fix ^D handling
	19-Feb-2005 Y.ISHISAKI, call free(fn) in clrhis_(), clwhis_()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _OSF_SOURCE
#include <signal.h>
#endif

extern char *readline();
extern void add_history();
extern int read_history();
extern int history_truncate_file();

static char histbuf[32*1024];
static char *histp = histbuf;

void
clsigunsetmask_()
{
#ifdef _OSF_SOURCE	/* for DFARTL360 bug (maybe) */
	sigset_t i_set, o_set;
	i_set = 0;
	sigprocmask(SIG_SETMASK, &i_set, &o_set);
#endif
}

void
clrhis_(file, error, file_len)
  char *file;
  int *error;
  int file_len;
{
	extern void using_history();
	char *fn;

	using_history();

	if ( 0 == file_len ) {
		*error = -1;
		return;
	}

	fn = malloc(file_len+1);
	if ( NULL == fn ) {
		*error = -2;
		return;
	}

	strncpy(fn, file, file_len);
	fn[file_len] = '\0';
	*error = read_history(fn);

	free(fn);
}

void
clwhis_(file, lines, error, file_len)
  char *file;
  int *lines;
  int *error;
  int file_len;
{
	extern int write_history();
	char *fn;

	if ( 0 == file_len ) {
		*error = -1;
		return;
	}

	fn = malloc(file_len+1);
	if ( NULL == fn ) {
		*error = -2;
		return;
	}

	strncpy(fn, file, file_len);
	fn[file_len] = '\0';
	*error = write_history(fn);
	if ( 0 == *error ) {
		*error = history_truncate_file(fn, *lines);
	}

	free(fn);
}

void
clphis_(file, file_len)
  char *file;
  int file_len;
{
	char *p;
	FILE *fp = stdout;

	if ( file_len ) {
		char *fn;
		fn = malloc(file_len+1);
		if ( fn ) {
			strncpy(fn, file, file_len);
			fn[file_len] = '\0';
			fp = fopen(fn, "w");
			if ( NULL == fp ) {
				fp = stdout;
			}
			free(fn);
		}
	}

	for (p = histbuf; p < histp; p += strlen(p) + 1) {
		fputs(p, fp);
		fputc('\n', fp);
	}

	if ( stdout != fp ) {
		fclose(fp);
	}
}

void
clrdln_(pro, buf, len, lpro, lbuf)
  char *pro;
  char *buf;
  int *len;
  int lpro;
  int lbuf;
{
	char *p, *line;
	int i;

	p = (char *)malloc(lpro+1);
	if ( NULL == p ) {
	quit:
		memset(buf,' ',lbuf);
		*len = -1;				/* return negative value on EOF */
		return;
	}
	strncpy(p,pro,lpro);
	*(p+lpro) = '\0';
	line = readline(p);
	free(p);

	if( NULL == line ) {
		goto quit;
	}

	add_history(line);
	*len = strlen(line);
	if ( *len + 1 < sizeof(histbuf) - (histp - histbuf) ) {
		strcpy(histp, line);
		histp += *len + 1;
	} else {
		int offset = (histp - histbuf) / 2;
		offset += strlen(histbuf+offset) + 1;
		for (i = 0; i < sizeof(histbuf); i++) {
			if ( &histbuf[i+offset] < histp ) {
				histbuf[i] = histbuf[i+offset];
			} else {
				histbuf[i] = '\0';
			}
		}
		histp -= offset;
		if ( *len + 1 < sizeof(histbuf) - (histp - histbuf) ) {
			strcpy(histp, line);
			histp += *len + 1;
		}
	}

	for(i = 0; i < lbuf && i < *len; i++) {
		buf[i] = line[i];
	}

	while ( i < lbuf ) {
		buf[i] = ' ';
		i++;
	}

	free(line);
}
