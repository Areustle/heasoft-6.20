/* $Id: attfile.c,v 1.17 2008/03/04 17:20:58 ishisaki Exp $ */
/************************************************************************
  attfile.c

	This file is originally written by Ed Pier (GSFC) and modified by
	Y.ISHISAKI (Tokyo Metro-U) for astetool

  1999-05-17	Ed Pier (original version)

  1999-12-20	Y.ISHISAKI	version 1.20
		almost rewrite for astetool 1.20

  2005-02-23	Y.ISHISAKI	version 1.31
		add dummy initialization of variables in findTimeInAttFile()

  2005-06-26	Y.ISHISAKI	version 1.53
		bug fix in initializing search_q1

  2005-08-23	Y.ISHISAKI	version 1.70
		modified to use atInterpolateQuat() instead of local interpolate_qp()

  2005-10-24	Y.ISHISAKI	version 1.71
		try with EULER when QPARAM column not found

  2006-08-23	Y.ISHISAKI	version 1.82
		modified to use atan2() instead of acos() in local interpolateQuat(),
		which may give slightly different values in different architecture

  2006-11-25	Y.ISHISAKI	version 1.84
		clear cfitsio error for QPARAM, SIGMA, SENSOR columns in openAttFile()

  2008-03-05	Y.ISHISAKI	version 1.86
		call atInterpolateQuat() instead of internal interpolateQuat(),
		which was buggy in iterplating q-parameters of
		[q0 q1 q2 q3] -> [-(q0+a) -(q1+b) -(q2+c) -(q3+d)] (a, b, c, d << 1)

************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "attfile.h"

static char pname[] = "attfile";

/* #define DEBUG */	/* uncomment this line for debugging */

/***********************************************************************
*************************************************************************
* handle FITSIO errors while reading attitude files
* prints error messages if there is an error
************************************************************************/
static int
check_fits_error(int istat, char *doing, char *filename)
{
	if ( istat ) {
		fprintf(stderr, "\
%s: FITSIO error while %s file '%s'", pname, doing, filename);
		fits_report_error(stderr, istat);
	}
	return istat;
}

/**************************************************************************
***************************************************************************
* Read the TIME from a given row of the table
**************************************************************************/
double
readTimeFromAttFile(ATTFILE *attfp, long irow, int *istat)
{
	static double nulval = 0.0;
	double t;
	int t_col;
	fitsfile *fp;
	int anynul = 0;

	if ( NULL == attfp ) {
		*istat = FILE_NOT_OPENED;
		return 0.0;
	}

	if ( ATTFILE_NORMAL_FITS == attfp->type ) {
		fp = attfp->fp;
		t_col = attfp->fileinfo.normal_fits.time_col;
		fits_read_col_dbl(fp, t_col, irow, 1, 1, nulval, &t, &anynul, istat);
	} else if ( ATTFILE_PACKED_FITS == attfp->type ) {
		fp = attfp->fp;
		t_col = attfp->fileinfo.packed_fits.time_col;
		fits_read_col_dbl(fp, t_col, irow, 1, 1, nulval, &t, &anynul, istat);
	} else if ( ATTFILE_ASCII ) {
		if ( 1 <= irow && irow <= attfp->nrows ) {
			t = attfp->fileinfo.ascii_qparam.p[irow-1].t;
		} else {
			*istat = BAD_ROW_NUM;
		}
	} else {
		*istat = FILE_NOT_OPENED;
	}

	if ( *istat ) {
		return 0.0;
	}

	return t;
}

/**************************************************************************
***************************************************************************
* Read the Q parameter from a given row of the table
**************************************************************************/
int
readQuatFromAttFile(ATTFILE *attfp, long irow, double q[4], int *istat)
{
	static double nulval = 0.0;
	int q_col;
	int anynul = 0;

	if ( NULL == attfp ) {
		*istat = FILE_NOT_OPENED;
		return 0.0;
	}

	if ( ATTFILE_NORMAL_FITS == attfp->type ) {
		fitsfile *fp = attfp->fp;
		q_col = attfp->fileinfo.normal_fits.qparam_col;
		fits_read_col_dbl(fp, q_col, irow, 1, 4, nulval, q, &anynul, istat);
	} else if ( ATTFILE_PACKED_FITS == attfp->type ) {
		fitsfile *fp = attfp->fp;
		q_col = attfp->fileinfo.packed_fits.euler_col;
		fits_read_col_dbl(fp, q_col, irow, 1, 3, nulval, q, &anynul, istat);
		if ( 0 == *istat ) {
			AtEulerAng ea;
			AtRotMat rm;
			ea.phi = q[0] * DEG2RAD;
			ea.theta = q[1] * DEG2RAD;
			ea.psi = q[2] * DEG2RAD;
			atEulerToRM(&ea, rm);
			atRMToQuat(rm, q);
		}
	} else if ( ATTFILE_ASCII ) {
		if ( 1 <= irow && irow <= attfp->nrows ) {
			memcpy(q, attfp->fileinfo.ascii_qparam.p[irow-1].q, sizeof(AtQuat));
		} else {
			*istat = BAD_ROW_NUM;
		}
	} else {
		*istat = FILE_NOT_OPENED;
	}

	return *istat;
}

/**************************************************************************
***************************************************************************
* open an ASCII file and create an ATTFILE structure
**************************************************************************/
static ATTFILE *
openAttFileASCII(char *filename)
{
	FILE *fp;
	ATTFILE *attfp;
	int i, n, c;
	char line[256];
	double t, q[4];
	AtEulerAng ea;
	AtRotMat rm;
	int ip, np;
	struct attfile_ascii_data *p;

	fp = fopen(filename, "r");
	if ( NULL == fp ) {
		check_fits_error(FILE_NOT_OPENED, "opening attitude file in", filename);
		return NULL;
	}

	ip = 0;
	np = 10000;
	p = malloc(np * sizeof(*p));
	if ( NULL == p ) {
		fclose(fp);
		fprintf(stderr, "\
%s: malloc() faild in file '%s'", pname, filename);
		return NULL;
	}

	do {
/* read one line */
		i = 0;
		for (;;) {
			c = fgetc(fp);
			if ( EOF == c || '\n' == c ) {
				break;
			}
			if ( i < sizeof(line) - 1 ) {
				line[i++] = (char)c;
			}
		}
		line[i] = '\0';

/* scan one line */
		n = sscanf(line, "%lf%lf%lf%lf%lf", &t, &q[0], &q[1], &q[2], &q[3]);
		if ( 4 == n ) {
			ea.phi = q[0] * DEG2RAD;
			ea.theta = q[1] * DEG2RAD;
			ea.psi = q[2] * DEG2RAD;
			atEulerToRM(&ea, rm);
			atRMToQuat(rm, q);
			n = 5;
		}
		if ( 5 == n ) {
			if ( np <= ip ) {
				np += 10000;
				p = realloc(p, np * sizeof(*p));
				if ( NULL == p ) {
					fclose(fp);
					fprintf(stderr, "\
%s: realloc() faild in file '%s'", pname, filename);
					return NULL;
				}
			}
			p[ip].t = t;
			p[ip].q[0] = q[0];
			p[ip].q[1] = q[1];
			p[ip].q[2] = q[2];
			p[ip].q[3] = q[3];
			ip++;
		} else {
			/* ignore this line */
		}
	} while ( EOF != c );

	fclose(fp);

	np = ip;
	if ( np < 3 ) {
		free(p);
		fprintf(stderr, "\
%s: only %d lines in file '%s'", pname, np, filename);
		return NULL;
	}

	np = ip;
	p = realloc(p, np * sizeof(*p));
	if ( NULL == p ) {
		fprintf(stderr, "\
%s: realloc() faild in file '%s'", pname, filename);
		return NULL;
	}

/* allocate ATTFILE */
	attfp = malloc(sizeof(*attfp) + strlen(filename) + 1);
	if ( NULL == attfp ) {
		fprintf(stderr, "\
%s: malloc() faild in file '%s'", pname, filename);
		return NULL;
	}
	attfp->filename = (void *)&attfp[1];
	strcpy(attfp->filename, filename);
	attfp->telescop = NULL;
	strcpy(attfp->mission, "UNKNOWN");
	attfp->nrows = np;
	attfp->type = ATTFILE_ASCII;
	attfp->fp = NULL;
	attfp->tstart = p[0].t;
	attfp->tstop  = p[np-1].t;
	attfp->duration = attfp->tstop - attfp->tstart;
	resetAttFileExtrapolationLimits(attfp, DEFAULT_ATTFILE_EXTRAPOLATION);
	attfp->search_row = 1;
	attfp->search_t0 = p[0].t;
	attfp->search_t1 = p[1].t;
	memcpy(attfp->search_q0, p[0].q, sizeof(AtQuat));
	memcpy(attfp->search_q1, p[1].q, sizeof(AtQuat));
	attfp->cache_t = attfp->search_t0;
	memcpy(attfp->cache_q, attfp->search_q0, sizeof(AtQuat));
	attfp->fileinfo.ascii_qparam.p = p;

	return attfp;
}

/**************************************************************************
***************************************************************************
* open an actual attitude file and create an ATTFILE structure
**************************************************************************/
ATTFILE *
openAttFile(char *filename)
{
	ATTFILE *attfp;
	ATTFILE attfile;		/* temporary buffer of ATTFILE */
	int istat;
	char comment[FLEN_COMMENT];
	fitsfile *fp;

/* initialize variables */
	istat = 0;
	memset(&attfile, 0, sizeof(attfile));

/* open the attitude file */

	fits_open_file(&attfile.fp, filename, READONLY, &istat);
	if ( istat ) {
		attfp = openAttFileASCII(filename);	/* try ASCII format */
		return attfp;
	}
	fp = attfile.fp;

/* read the mission from the TELESCOP keyword */
	attfile.telescop = attfile.mission;
	fits_read_key_str(fp, "TELESCOP", attfile.mission, comment, &istat);
	if ( istat ) {
		istat = 0;		/* set mission to UNKNOWN by default */
		strncpy(attfile.mission, "UNKNOWN", sizeof(attfile.mission));
		attfile.telescop = NULL;
	}

/* go to ATTITUDE extension */
	attfile.type = ATTFILE_NORMAL_FITS;
	fits_movnam_hdu(fp, BINARY_TBL, "ATTITUDE", 0/*ignore version*/, &istat);
	if ( istat ) {
		istat = 0;
		attfile.type = ATTFILE_PACKED_FITS;
		fits_movnam_hdu(fp, BINARY_TBL, "PACKED ATTITUDE", 0, &istat);
	}
	if ( check_fits_error(istat, "finding ATTITUDE extension in", filename) ) {
		return NULL;
	}

/* read number of rows in table */
	fits_read_key_lng(fp, "NAXIS2", &attfile.nrows, comment, &istat);
	if ( check_fits_error(istat, "reading NAXIS2 from", filename) ) {
		return NULL;
	}

/* read column numbers */
	if ( ATTFILE_NORMAL_FITS == attfile.type ) {
		struct attfile_normal_fits *p = &attfile.fileinfo.normal_fits;
		fits_get_colnum(fp, CASESEN, "TIME", &p->time_col, &istat);
		fits_get_colnum(fp, CASESEN, "QPARAM", &p->qparam_col, &istat);
		if ( istat ) {
			istat = 0;		/* try with EULER */
			fits_get_colnum(fp, CASESEN, "EULER", &p->qparam_col, &istat);
			if ( 0 == istat ) {
				attfile.type = ATTFILE_PACKED_FITS;
				fits_clear_errmsg();
			}
		} else {
			fits_get_colnum(fp, CASESEN, "SIGMA", &p->sigma_col, &istat);
			if ( istat ) {
				p->sigma_col = 0;
				istat = 0;		/* ignore error */
				fits_clear_errmsg();
			}
			fits_get_colnum(fp, CASESEN, "SENSOR", &p->sensor_col, &istat);
			if ( istat ) {
				p->sensor_col = 0;
				istat = 0;		/* ignore error */
				fits_clear_errmsg();
			}
		}
	} else if ( ATTFILE_PACKED_FITS == attfile.type ) {
		struct attfile_packed_fits *p = &attfile.fileinfo.packed_fits;
		fits_get_colnum(fp, CASESEN, "TIME", &p->time_col, &istat);
		fits_get_colnum(fp, CASESEN, "EULER", &p->euler_col, &istat);
	}
	if ( check_fits_error(istat, "locating columns in", filename) ) {
		return NULL;
	}

/* read last and first times and set extrapolation limits */
	attfile.tstop  = readTimeFromAttFile(&attfile, attfile.nrows, &istat);
	if ( check_fits_error(istat, "reading last row in", filename) ) {
		return NULL;
	}
/* this order leave the FITSIO buffers holding the beginning of the file */
	attfile.tstart = readTimeFromAttFile(&attfile, 1, &istat);
	if ( check_fits_error(istat, "reading first row in", filename) ) {
		return NULL;
	}
	attfile.duration = attfile.tstop - attfile.tstart;
	resetAttFileExtrapolationLimits(&attfile, DEFAULT_ATTFILE_EXTRAPOLATION);

/* initialize search results */
	attfile.search_row = 1;
	attfile.search_t0 = readTimeFromAttFile(&attfile, 1, &istat);
	readQuatFromAttFile(&attfile, 1, attfile.search_q0, &istat);
	if ( check_fits_error(istat, "reading first row in", filename) ) {
		return NULL;
	}
	attfile.search_t1 = readTimeFromAttFile(&attfile, 2, &istat);
	readQuatFromAttFile(&attfile, 2, attfile.search_q1, &istat);
	if ( check_fits_error(istat, "reading second row in", filename) ) {
		return NULL;
	}
	attfile.cache_t = attfile.search_t0;
	memcpy(attfile.cache_q, attfile.search_q0, sizeof(AtQuat));

/* allocate ATTFILE */
	attfp = malloc(sizeof(attfile) + strlen(filename) + 1);
	if ( NULL == attfp ) {
		fprintf(stderr, "\
%s: malloc() faild in file '%s'", pname, filename);
		return NULL;
	}
	attfile.filename = (void *)&attfp[1];
	strcpy(attfile.filename, filename);
	*attfp = attfile;

	return attfp;
}

/**************************************************************************
***************************************************************************
* close an attitude file and destroy the ATTFILE structure
**************************************************************************/
void
closeAttFile(ATTFILE *attfp)
{
	int istat = 0;

	if ( NULL == attfp ) {
		return;
	}

	if ( NULL != attfp->fp ) {
		fits_close_file(attfp->fp, &istat);
	}

	if ( ATTFILE_ASCII == attfp->type ) {
		free(attfp->fileinfo.ascii_qparam.p);
	}

	free(attfp);
}

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file
* returns 1 if time is withing the first and last rows
* returns 0 otherwise
**************************************************************************/
int
isInAttFile(ATTFILE *fp, double t)
{
	if ( fp->tstart <= t && t <= fp->tstop ) {
		return 1;
	} else {
		return 0;
	}
}

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file, or its
* reasdonable extrapolation
* returns 1 if time is within extrrapolation limits
* returns 0 otherwise
* See also resetAttFileExtrapolationLimits.
* Note there are no internal checks for whether a time is covered by the
* attfile, these must be supplied by the calling program.
**************************************************************************/
int
isInExtrapolatedAttFile(ATTFILE *fp, double t)
{
	if ( fp->min_extrapolated <= t && t <= fp->max_extrapolated ) {
		return 1;
	} else {
		return 0;
	}
}

/**************************************************************************
***************************************************************************
* Changes the valid extrapolation time limits for the attitude file.
* The default values when the file is opened is
* DEFAULT_ATTFILE_EXTRAPOLATION.
* See also sInExtrapolatedAttFile(ATTFILE* file, double time).
**************************************************************************/
void
resetAttFileExtrapolationLimits(ATTFILE *fp, double margin)
{
	fp->min_extrapolated = fp->tstart - margin;
	fp->max_extrapolated = fp->tstop  + margin;
}

/**************************************************************************
***************************************************************************
* Locates a given time value in the attitude file
* the row will be the last row in the file which has a time value
* less than or equal to the specified time.
* if the time is before the table the first row in the table is returned.
* if the time is at or beyond the end of the table the second to last row
* will be returned.
* In other words, this routine returns the first of the pair of rows
* which should be used to interpolate to the specified time.
**************************************************************************/
long
findTimeInAttFile(ATTFILE *fp, double t)
{
	long i0, i1, irow, irow_limit;
	double t0, t1, current_time, last_time;
	int istat = 0;

/* dummy initialization of variables for gcc -Wall */
	i0 = i1 = 0;
	t0 = t1 = last_time = 0.0;

#ifdef DEBUG
printf("findTimeInAttFile: start\n");
#endif /*DEBUG*/

	if ( NULL == fp ) {
		return -1;
	}

/* are we extrapolating? */
	if ( !isInAttFile(fp, t) || t == fp->tstop ) {

#ifdef DEBUG
		printf("\
extrapolating time=%.14g tstart=%.14g tstop=%.14g\n", t, fp->tstart, fp->tstop);
#endif /*DEBUG*/

/* need to extrapolate */
		if ( t < fp->tstart ) {
			irow = 1;      			/* before beginning of file */
		} else {
			irow = fp->nrows - 1;	/* after end of file */
		}

		if ( fp->search_row != irow ) {
			fp->search_t0 = readTimeFromAttFile(fp, irow, &istat);
			fp->search_t1 = readTimeFromAttFile(fp, irow+1, &istat);
			readQuatFromAttFile(fp, irow, fp->search_q0, &istat);
			readQuatFromAttFile(fp, irow+1, fp->search_q1, &istat);
			if ( istat ) return -1;
			fp->search_row = irow;
		}

#ifdef DEBUG
		printf("\
returning extrapolated\n");
#endif /*DEBUG*/

/* return the answer */
		return fp->search_row;
	}

/* are we still in the same place we found in the last search? */
	if ( fp->search_t0 <= t &&  t < fp->search_t1 ) {
		return fp->search_row;
	}

/************************************************************
* none of the easy answers worked, so we have to go looking
* through the table. The new search location is guaranteed to
* be different from the last one, so we need to mark the
* last found quaternions as unreliable.
************************************************************/

/* determine where we should start an initial linear search */

	if ( t < fp->search_t0 ) {
		irow = 1 + (fp->search_row - 1) *
					(fp->search_t0 - t) / (fp->search_t0 - fp->tstart);
	} else {
		irow = fp->search_row + (fp->nrows - fp->search_row) *
			 		(t - fp->search_t0) / (fp->tstop - fp->search_t0);
	}

/*******************************************************
* make sure we haven't gone beyond the end of the file *
*******************************************************/
	if ( irow < 1 ) {
		irow = 1;
	} else if( fp->nrows <= irow ) {
		irow = fp->nrows - 1;
	}

/* try a linear search for the correct time value */
	current_time = readTimeFromAttFile(fp, irow, &istat);
	if ( istat ) return -1;

	if ( t == current_time ) {
		/* it's right, exactly here */
		fp->search_t0 = current_time;
		fp->search_t1 = readTimeFromAttFile(fp, irow+1, &istat);
		readQuatFromAttFile(fp, irow, fp->search_q0, &istat);
		readQuatFromAttFile(fp, irow+1, fp->search_q1, &istat);
		if ( istat ) return -1;
		fp->search_row = irow;
		return fp->search_row;

	} else if ( current_time < t ) {
		/* search forward */
		irow_limit = irow + ATTFILE_LOCAL_SEARCH_LIMIT;
		if ( fp->nrows <= irow_limit ) {
			irow_limit = fp->nrows - 1;
		}

		while ( current_time <= t && irow < irow_limit ) {
			irow++;
			last_time = current_time;
			current_time = readTimeFromAttFile(fp, irow, &istat);
			if ( istat ) return -1;
		}

		/* check if we found it */
		if ( t < current_time ) {
			/* we passed the point we want */
			fp->search_row = irow - 1;
			fp->search_t0 = last_time;
			fp->search_t1 = current_time;
			readQuatFromAttFile(fp, irow-1, fp->search_q0, &istat);
			readQuatFromAttFile(fp, irow, fp->search_q1, &istat);
			if ( istat ) return -1;
			return fp->search_row;
		}

		/* search between current position and end of table */
		i0 = irow;
		t0 = current_time;
		i1 = fp->nrows;
		t1 = fp->tstop;

	} else if ( t < current_time ) {
		/* search backwards */
		irow_limit = irow - ATTFILE_LOCAL_SEARCH_LIMIT;
		if ( irow_limit < 1 ) {
			irow_limit = 1;
		}

		while ( t < current_time && irow_limit < irow ) {
			irow--;
			last_time = current_time;
			current_time = readTimeFromAttFile(fp, irow, &istat);
			if ( istat ) return -1;
		}

		/* check if we found it */
		if ( current_time <= t ) {
			/* we're at the point we want */
			fp->search_row = irow;
			fp->search_t0 = current_time;
			fp->search_t1 = last_time;
			readQuatFromAttFile(fp, irow, fp->search_q0, &istat);
			readQuatFromAttFile(fp, irow+1, fp->search_q1, &istat);
			if ( istat ) return -1;
			return fp->search_row;
		}

		/* search between current position and beginning of table */
		i0 = 1;
		t0 = fp->tstart;
		i1 = irow;
		t1 = current_time;

	}

/********************************************************************
* now if we get here, it means we haven't found the point we want
* by a linear search so we use bisection to hunt it down.
********************************************************************/

	while ( 1 < i1 - i0 ) {
		irow = (i1 + i0) / 2;
		current_time = readTimeFromAttFile(fp, irow, &istat);
		if ( istat ) return -1;

		if ( current_time == t ) {
			fp->search_row = irow;
			fp->search_t0 = current_time;
			fp->search_t1 = readTimeFromAttFile(fp, irow+1, &istat);
			readQuatFromAttFile(fp, irow, fp->search_q0, &istat);
			readQuatFromAttFile(fp, irow+1, fp->search_q1, &istat);
			if ( istat ) return -1;
			return fp->search_row;
		} else if ( t < current_time ) {
			i1 = irow;
			t1 = current_time;
		} else {
			i0 = irow;
			t0 = current_time;
		}
	}

/* when we get here we have found the point we were looking for by bisection */
	fp->search_row = i0;
	fp->search_t0 = t0;
	fp->search_t1 = t1;
	readQuatFromAttFile(fp, i0, fp->search_q0, &istat);
	readQuatFromAttFile(fp, i0+1, fp->search_q1, &istat);
	if ( istat ) return -1;

	return fp->search_row;
}

/**************************************************************************
***************************************************************************
* determines the quaternion at an arbitrary time
* values are interpolated if necessary and things are optimized
* for the case of repeated calls to this routine with similar time values
**************************************************************************/
int
findQuatInAttFile(ATTFILE *fp, double q[4], double t)
{
	if ( t == fp->cache_t ) {
		memcpy(q, fp->cache_q, sizeof(AtQuat));
		return 0;
	}

	if ( t == fp->search_t0 ) {
		memcpy(q, fp->search_q0, sizeof(AtQuat));
		return 0;
	}

	if ( t == fp->search_t1 ) {
		memcpy(q, fp->search_q1, sizeof(AtQuat));
		return 0;
	}

/* search for the correct place in the table */
	if ( findTimeInAttFile(fp, t) < 0 ) {
		return -1;
	}

	if ( t == fp->search_t0 ) {
		memcpy(q, fp->search_q0, sizeof(AtQuat));
		return 0;
	}

	if ( t == fp->search_t1 ) {
		memcpy(q, fp->search_q1, sizeof(AtQuat));
		return 0;
	}

/* interpolate between t0 and t1 */
	atInterpolateQuat(
		fp->search_t0, fp->search_q0, fp->search_t1, fp->search_q1, t, q);

/* remember last calculated time and value */

	fp->cache_t = t;
	memcpy(fp->cache_q, q, sizeof(AtQuat));

	return 0;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
