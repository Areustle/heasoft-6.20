/********************************************************
  anl_flush.c
    flushes output streams from FORTRAN and C routine

    95/04/15 created by Y.Ishisaki & M.Hirayama

    2005/11/29 Y.ISHISAKI	version 1.70
		add anl_flush_() for FORTRAN

    2005/11/29 Y.ISHISAKI	version 1.73
		use CLflush() instead of FLUSH() cfortran macro in anl_flush()

********************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cfortran.h"

void
anl_flush(void)
{
/* flush output from FORTRAN routine */
/*	CLflush(0);*/	/* cause segmentation fault on ULTRIX */
	CLflush(6);

/* flush output from C routine */
	fflush(stdout);
	fflush(stderr);
}

void
anl_flush_(void)
{
	anl_flush();
}
