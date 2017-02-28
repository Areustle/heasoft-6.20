/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/secular2.c,v 3.8 1996/08/13 18:22:44 peachey Exp $   */
/*                   */

/*
 filename:	secular.c
 purpose:	read SIS secular file(s)
 author:	Geoffrey B. Crew
 date:		June 1994
 */
/******************************************************************************
Taskname:    sqaplot

Filename:    secular.c

Description: read SIS secular file(s)

Author/Date: Geoffrey B. Crew, 5/93

Modification History:
    James Peachey, HSTX, 8/96 - changed to use native cfitsio library:
    - Replaced references to C-wrapped fortran fitsio calls (fcXXXX)
      with equivalent native C cfitsio calls (ffXXXX).
    - Added global fitsfile* fc to replace LUN FC in cfitsio calls

Notes:

Functions implemented in this file:

******************************************************************************/

#include "defs.h"
#include "compat.h"

#define RND	((double)(frac = ((frac * mult + shft) % modl))/(double)modl)
#define FC	44

static int	stts, *status = &stts;
/*static int	frac = 0, modl = 6075, mult = 106, shft = 1283;*/
static long	frac = 0L, modl = 6075L, mult = 106L, shft = 1283L;

static double	inv_gain;
static Real	c1[4], c2[4], c3[4], c4[4], c5[4], c6[4];

static void	event_ph2pi(), find_ext(), interpolate();

static fitsfile	*fc = NULL; /* 8/12/96 */

/*
 *  Lookup the echo parameter appropriate to the epoch.
 *  On failure, do nothing and assume sensible defaults are in place.
 */
void
get_echos(efile, epoch, sisn, echop)
	char	*efile;
	double	epoch;
	int	sisn;
	Real	*echop;
{
	static double	time[100];
	static Real	echo[100];
	int		rows, any;

	find_ext(efile, "ECHO_T1", &rows);
	if (*status) { *status = 0; return; }

/*	fcgcvd(FC,1,     1,1,rows,0.0,time,&any,status);
	fcgcve(FC,2+sisn,1,1,rows,0.0,echo,&any,status);
*/
	ffgcvd(fc, 1, 1L, 1L, (long) rows, 0.0, time, &any, status);
	ffgcve(fc, 2+sisn, 1L, 1L , (long) rows, 0.0, echo, &any, status);

	interpolate(epoch, echop, time, echo, 1, rows);

/*	fcclos(FC, status);*/
	ffclos(fc, status);
}

/*
 *  Lookup the PH to PI transformation appropriate to the epoch.
 *  On failure, do nothing and assume sensible defaults are in place.
 */
void
get_ph2pi(pfile, epoch, sisn, xform)
	char	*pfile;
	double	epoch;
	int	sisn;
	void	(**xform)();
{
	static double	time[600];
	static Real	data[600];
/*	static char	com[49];*/
	static char	com[FLEN_COMMENT];
	int		rows, any, c;
	long	dummy; /* 8/12/96 */

	find_ext(pfile, "PH2PI_T1", &rows);
	if (*status) { *status = 0; return; }

/*	fcgkyd(FC, "NOM_GAIN", &inv_gain, com, status);*/
	ffgkyd(fc, "NOM_GAIN", &inv_gain, com, status);
	sqwork.gn = inv_gain / 1000.0;	/* for plot -- keV / PI-ADU */
	inv_gain = 1.0/inv_gain;
/*	fcgkyj(FC, "XFORMTYP", &any,  com, status);
	fcgkyj(FC, "RND_MODL", &modl, com, status);
	fcgkyj(FC, "RND_MULT", &mult, com, status);
	fcgkyj(FC, "RND_SHFT", &shft, com, status);
	fcgkyj(FC, "RND_SEED", &frac, com, status);
*/
	ffgkyj(fc, "XFORMTYP", &dummy,  com, status);/* this keyword ignored */
	ffgkyj(fc, "RND_MODL", &modl, com, status);
	ffgkyj(fc, "RND_MULT", &mult, com, status);
	ffgkyj(fc, "RND_SHFT", &shft, com, status);
	ffgkyj(fc, "RND_SEED", &frac, com, status);

/*	fcgcvd(FC,1,                 1,1,rows,0.0,time,&any,status);*/
	ffgcvd(fc, 1, 1L, 1L, (long) rows, 0.0, time, &any, status);
	for (c = 0; c < 4; c++) {
/*		fcgcve(FC,2+4*sisn+c,1,1,6*rows,0.0,data,&any,status);*/
		ffgcve(fc, 2+4*sisn+c, 1L, 1L, 6*rows, 0.0, data, &any, status);
		interpolate(epoch, c1 + c, time, data + 0, 6, rows);
		interpolate(epoch, c2 + c, time, data + 1, 6, rows);
		interpolate(epoch, c3 + c, time, data + 2, 6, rows);
		interpolate(epoch, c4 + c, time, data + 3, 6, rows);
		interpolate(epoch, c5 + c, time, data + 4, 6, rows);
		interpolate(epoch, c6 + c, time, data + 5, 6, rows);
	}

/*	fcclos(FC, status);*/
	ffclos(fc, status);
	if (*status) return;
	*xform = event_ph2pi;
}

/*
 *  With an event:  do the transformation.
 *  With no event:  sqwork.dopi != 1 do setup
 *		    sqwork.dopi == 1 dump out info
 */
static void
event_ph2pi(ev)
	Event	*ev;
{
	register int    c;

	if (ev) {			/* do the tranform */
		c = (int)ev->c;
		ev->q = ev->p;
		ev->p = (int)( (	  ( (double)ev->p + RND ) *
			( c1[c] + ev->x * c2[c] + ev->y * c3[c] ) +
			( c4[c] + ev->x * c5[c] + ev->y * c6[c] )
		) * inv_gain);
	} else if (sqwork.dopi != 1) {	/* setup */
		sqwork.dopi = 1;
	} else {			/* output verbosity */
		static char	b[MAX_FNAME];

		sqecho("\n** PI transform data follow **\n\n");
		for (c = 0; c < 4; c++) {
			(void)sprintf(b, "\tC1[%d] = %.2lg",
				c, c1[c]);	sqecho(b);
			(void)sprintf(b, "\tC2[%d] = %.2lg",
				c, c2[c]);	sqecho(b);
			(void)sprintf(b, "\tC3[%d] = %.2lg\n",
				c, c3[c]);	sqecho(b);
			(void)sprintf(b, "\tC4[%d] = %.2lg",
				c, c4[c]);	sqecho(b);
			(void)sprintf(b, "\tC5[%d] = %.2lg",
				c, c5[c]);	sqecho(b);
			(void)sprintf(b, "\tC6[%d] = %.2lg\n",
				c, c6[c]);	sqecho(b);
		}
		sqecho("\n** End of PI transform data **\n");
	}
}

/*
 *  Find an extension by name.  (Probably obsolete.)
 *  Result returned by *status;
 */
static void
find_ext(file, name, rows)
	char	*file, *name;
	int	*rows;
{
/*	static char	val[71], com[49];*/
	static char	val[FLEN_VALUE], com[FLEN_COMMENT];
	int		ext, blk, hdu;
	long		longrows; /* 8/12/96 */

/*	fcopen(FC, file, 0, &blk, status);*/
	ffopen(&fc, file, READONLY, status);
	for (ext = 1; ; ext++) {
/*		fcmahd(FC, ext, &hdu, status);*/
		ffmahd(fc, ext, &hdu, status);
		if (*status) break;
/*		fcgkys(FC, "EXTNAME ", val, com, status);*/
		ffgkys(fc, "EXTNAME ", val, com, status);
		if (!*status && !strncmp(name, val, strlen(name))) break;
		else *status = 0;
	}
/*	fcgkyj(FC, "NAXIS2  ", rows, com, status);*/
	ffgkyj(fc, "NAXIS2  ", &longrows, com, status);
	*rows = (int) longrows; /* 8/12/96 */
}

/*
 *  Interpolate between (or extrapolate beyond) rows of the data.
 */
static void
interpolate(epoch, datap, time, data, dimd, nrows)
	double	epoch, *time;
	Real	*datap, *data;
	int	nrows, dimd;
{
	register int	i, j;

	for (i = 1; i < nrows; i++)
		if (epoch < time[i]) break;

	if (i == nrows) i--;	/* extrapolate after time[nrows] */
	j = i - 1;

	*datap = data[j*dimd] + (data[i*dimd] - data[j*dimd]) *
		((epoch - time[j]) / (time[i] - time[j]));
}
