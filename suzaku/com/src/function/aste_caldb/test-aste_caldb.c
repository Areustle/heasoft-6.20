/* $Id: test-aste_caldb.c,v 1.6 2007/05/30 11:24:41 ishisaki Exp $

  test-aste_caldb: test program for aste_caldb

	2006-07-02	Y.ISHISAKI	version 1.0
*/

#include <stdio.h>
#include <stdlib.h>
#include "aste_caldb.h"

static char pname[] = "test-aste_caldb";

void
usage(void)
{
	printf("\
usage:\n\
   %s TELESCOP INSTRUME DETNAM FILTER CODENAME EXPR DATE0 TIME0\n",
		pname);
}

int
main(int argc, char **argv)
{
	int i;
	CALDB_INFO caldb;

	if ( argc != 9 ) {
		usage();
		return 1;
	}

	aste_caldb_init(&caldb);

	caldb.telescop = argv[1];
	caldb.instrume = argv[2];
	caldb.detnam   = argv[3];
	caldb.filter   = argv[4];
	caldb.codename = argv[5];
	caldb.expr     = argv[6];
	caldb.date0    = argv[7];
	caldb.time0    = argv[8];

	aste_caldb_get(&caldb);

	printf("\
status=%d, nfound=%d\n", caldb.status, caldb.nfound);
	if ( caldb.nfound ) {
		printf("\
filename='%s'\n", caldb.filename);
		for (i = 0; i < caldb.nfound; i++) {
			printf("\
fname[%d]='%s', extno[%d]=%d\n", i, caldb.fnames[i], i, caldb.extno[i]);
		}
	}

	aste_caldb_free(&caldb);

	return caldb.status;
}
