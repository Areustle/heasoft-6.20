/* $Id: aste_att.c,v 1.14 2008/03/04 17:20:58 ishisaki Exp $ */
/************************************************************************
  aste_att.c:		attitude file utilities for Astro-E2

  ATTFILE* aste_att_init(char *filename);
	open attitude file and return ATTFILE structure.
	Return NULL when open failed.

  void aste_att_close(ATTFILE *ap);
	close attitude file.

  int aste_att_ea(ATTFILE *ap, double astetime, AtEulerAng *ea);
	get Eular angles in given time.
	Return -1 when astetime is out of extrrapolation limits.
	Return CFITSIO error when attitude file read error.

  ATTFILE* aste_att_attfile(void);	[OBSOLETE]
	return currently opened ATTFILE structure.
	Return NULL when not opend.

  int aste_att_euler(double astetime, AtEulerAng *ea);	[OBSOLETE]


  1999-12-20	Y.ISHISAKI	version 1.20

  2006-08-01	Y.ISHISAKI	version 1.80
	add aste_att_ea(), now aste_att_attfile(), aste_att_euler() is obsolete.

************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_att.h"

static ATTFILE *attfp = NULL;

ATTFILE *
aste_att_init(char *filename)
{
	AtRotMat rm;
	ATTFILE *ap;

	ap = openAttFile(filename);
	if ( NULL == ap ) {
		return NULL;
	}

	attfp = ap;
	atQuatToRM(ap->cache_q, rm);
	atRMToEuler(rm, &ap->cache_ea);

	return ap;
}

void
aste_att_close(ATTFILE *ap)
{
	if ( ap == attfp ) {
		attfp = NULL;
	}

	closeAttFile(ap);
}

int
aste_att_ea(ATTFILE *ap, double t, AtEulerAng *ea)
{
	int istat;
	AtQuat q;
	AtRotMat rm;

	if ( t == ap->cache_t ) {
		*ea = ap->cache_ea;
		return 0;
	}

	if ( ! isInExtrapolatedAttFile(ap, t) ) {
		return -1;
	}

	istat = findQuatInAttFile(ap, q, t);
	if ( istat ) {
		return istat;
	}

	atQuatToRM(q, rm);
	atRMToEuler(rm, &ap->cache_ea);

	*ea = ap->cache_ea;

	return 0;
}

/* OBSOETE */
ATTFILE *
aste_att_attfile(void)
{
	return attfp;
}

/* OBSOETE */
int
aste_att_euler(double t, AtEulerAng *ea)
{
	return aste_att_ea(attfp, t, ea);
}
