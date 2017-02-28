/* $Id: aste_instrume.c,v 1.7 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_instrume.c:	Return INSTRUME keyword for FITS header.

  // Return INSTRUME keyword for FITS header
  char *aste_instrume(int id);

  // Return ID number corresponding to the INSTRUME keyword, -1 for unknown
  int aste_instrume_id(char *instrume);

  2005-06-16	Y.ISHISAKI	version 1.52
		created from aste_telescop.c

************************************************************************/

#include <stdio.h>
#include <string.h>
#include "atFunctions.h"
#include "aste_coord.h"

static struct {
	int id;
	char *name;
} name_table[] = {
	{ ASTE_XIS0_ID, "XIS0" },
	{ ASTE_XIS1_ID, "XIS1" },
	{ ASTE_XIS2_ID, "XIS2" },
	{ ASTE_XIS3_ID, "XIS3" },
	{ ASTE_XIS0_ID, "XIS-0" },	/* obsolete, but accept this expression */
	{ ASTE_XIS1_ID, "XIS-1" },
	{ ASTE_XIS2_ID, "XIS-2" },
	{ ASTE_XIS3_ID, "XIS-3" },
	{ ASTE_XRS_ID,  "XRS" },
	{ ASTE_HXD_ID,  "HXD" },
	{ ASTE_XIS_ID,  "XIS" },
	{ ASTE_XRT_ID,  "XRT" },
	{ ASTE_XRTI_ID, "XRT-I" },
	{ ASTE_XRT0_ID, "XRT-I0" },
	{ ASTE_XRT1_ID, "XRT-I1" },
	{ ASTE_XRT2_ID, "XRT-I2" },
	{ ASTE_XRT3_ID, "XRT-I3" },
	{ ASTE_XRTS_ID, "XRT-S" },
	{ -1, NULL }
};

char *
aste_instrume(int id)
{
	int i;

	for (i = 0; -1 != name_table[i].id; i++) {
		if ( id == name_table[i].id ) {
			return name_table[i].name;
		}
	}

	return NULL;
}

int
aste_instrume_id(char *instrume)
{
	int i;

	if ( NULL == instrume ) {
		return -1;
	}

	for (i = 0; -1 != name_table[i].id; i++) {
		if ( 0 == strcmp(instrume, name_table[i].name) ) {
			return name_table[i].id;
		}
	}

	return -1;
}
