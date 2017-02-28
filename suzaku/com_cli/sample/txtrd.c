/*
   txtrd.c

		repeat CLtxtrd() or CLtitrd() until 'exit' or 'quit' or 'q'.
		good for basic function test of CLI

	2005/02/25 Y.ISHISAKI	version 1.60
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"

static char pname[] = "txtrd";

int
main(int argc, char **argv)
{
	int i, flag;
	char buf[16], line[1024];
	int lun = 6;	/* Fortran stdout */

	flag = argv[argc-1][0];
	if ( 'A' <= flag && flag <= 'Z' ) {
		flag = flag + 'a' - 'A';
	}

	if ( 1 == argc || ('x' != flag && 'i' != flag ) ) {
		fflush(NULL); CLflush(lun); printf("\
usage: %s [x|i]\n\
    Repeat CLtxtrd() or CLtitrd() until 'exit' or 'quit' or 'q'.\n\
    'x' for CLtxtrd(), 'i' for CLtxtrd().\n", pname);
		flag = 'x';
	}

	printf( ('x' == flag) ?  "Using CLtxtrd()\n" : "Using CLtitrd()\n" );
	fflush(NULL); CLflush(lun);

	i = 0;
	for (;;) {
		i++;
		sprintf(buf, "?%d:", i);
		if ( 'x' == flag ) {
			CLtxtrd(buf, line, sizeof(line));
		} else {
			CLtitrd(buf, line, sizeof(line));
		}
		fflush(NULL); CLflush(lun);
		printf("(%d)='%s'\n", i, line);
		if ( 0 == strcmp("exit", line) ||
			 0 == strcmp("quit", line) ||
			 0 == strcmp("q", line) ) {
			break;
		}
	}

	return 0;
}
