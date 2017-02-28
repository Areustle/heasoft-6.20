/* $Id: aste_telescop.c,v 1.10 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_telescop.c:	Return TELESCOP keyword for FITS header.

  // Return TELESCOP keyword for FITS header
  char *aste_telescop(void);

  // Return ID number corresponding to the TELESCOP keyword, -1 for unknown
  int aste_telescop_id(char *telescop);

  2004-09-22	Y.ISHISAKI	version 1.27

  2005-06-16	Y.ISHISAKI	version 1.52
		add aste_telescop_id()

  2005-07-10	Y.ISHISAKI	version 1.60
		TELESCOP = 'Astro-E2' -> 'SUZAKU' in aste_telescop()
************************************************************************/

#include <stdio.h>
#include <string.h>
#include <string.h>
#include "atFunctions.h"
#include "aste_coord.h"

char *
aste_telescop(void)
{
	static char *telescop = "SUZAKU";

	return telescop;
}

int
aste_telescop_id(char *telescop)
{
	if ( NULL == telescop ) {
		return -1;
	}

	if ( 0 == strcmp(aste_telescop(), telescop) ||
		 0 == strcmp("Astro-E2", telescop) ||
		 0 == strcmp("ASTRO-E", telescop) ) {
		return ASTE_TELESCOP_ID;
	}

	return -1;
}
