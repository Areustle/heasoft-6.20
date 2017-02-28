/*
 * $Source: /headas/headas/attitude/tasks/prefilter/misstime.c,v $
 * $Revision: 1.4 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * Modified from ascalib/src/general/ascatime.c for Swift
 *
 * $Log: misstime.c,v $
 * Revision 1.4  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.3  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.2  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.1  2002/05/14 14:51:57  rwiegand
 * Reworked parameter interface with Ed Pier's suggestions.
 *
 * Revision 1.1  2002/01/28 16:08:22  rwiegand
 * Initial revision
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>

#include "atFunctions.h"
#include "fitsio.h"
#include "report.h"
#include "misstime.h"

#define EPSILON_T	0.00005

typedef struct
{
	AtTime epoch;
	double mjd;
	double step;
} Leap;

static int leapSpace;
static int leapCount;
static Leap *LEAPS;
static int readDataFlag;


/**************************************************************************
 * this variable is used only if the calling function failed to 
 * initialize the leapsec table first using the leapsec.fits file
 * specified in parameter file;
 * If this variable is used, a warning message will be reported
 **************************************************************************/
#define LEAPTABLE "./refdata/leapsec.fits"



/************************************************************************
 * Using leapsec.fits in FTOOLS' refdate area instead of leadsec.dat
 * Leap seconds before mjd0 will be ignored.
 ************************************************************************/
int readLeapTable (const char *path, double missionMJD)
{
	int code = 0;
	fitsfile *fp = 0;
	int i, hdutype, status=0, anynul=0;
	long NAXIS2;
	char comm[73];
#define DEFAULT_BUFFER 64
	double buffer0[DEFAULT_BUFFER];
	double buffer1[DEFAULT_BUFFER];
	double *d0, *d1;
	int firstSignificant;

	if (!path)
		{
			path = LEAPTABLE;
			report_warning("using default leap second table %s\n",
					path);
		}

	if (fits_open_file(&fp, path, READONLY, &status))
		{
			report_error("unable to open leapsec table %s [%d]\n",
					path, status);
			return 1;
		}

	if (fits_movabs_hdu(fp, 2, &hdutype, &status))
		fits_report_error(stderr, status);

	if (fits_read_key_lng(fp, "NAXIS2", &NAXIS2, comm, &status))
		fits_report_error(stderr, status);

	d0 = &buffer0[0];
	d1 = &buffer1[0];
	if (NAXIS2 > DEFAULT_BUFFER)
		{
			d0 = (double*) malloc(NAXIS2 * sizeof(double));
			d1 = (double*) malloc(NAXIS2 * sizeof(double));
		}

	/***************************************************************** 
	 * READ MJD and determine when mjd > lower limit
	 ******************************************************************/
	/* now read mjd time from col 3 in leapsec.fits */
	if (fits_read_col_dbl(fp, 3, 1, 1, NAXIS2, 0.0,
				d0, &anynul, &status))
		fits_report_error(stderr, status);

	/* read column 5/LEAPSECS:	dbl*/
	if (fits_read_col_dbl(fp, 5, 1, 1, NAXIS2, 0.0,
				d1, &anynul, &status))
		fits_report_error(stderr, status);

	/* find rows after mission epoch */
	for (i = 0; i < NAXIS2; ++i)
		if (d0[i] >= missionMJD)
			break;

	firstSignificant = i;

	if (NAXIS2 - firstSignificant > leapCount)
		{
			if (LEAPS)
				free(LEAPS);
			LEAPS = 0;
			leapSpace = 0;
		}

	leapCount = NAXIS2 - firstSignificant;
	if (!LEAPS && leapCount > 0)
		{
			LEAPS = malloc(leapCount * sizeof(Leap));
			leapSpace = leapCount;
		}

	for (i = 0; i < leapCount; ++i)
		{
			Leap *leap = LEAPS + i;
			double mjd = d0[i + firstSignificant];
			leap->mjd = mjd;
			leap->step = d1[i + firstSignificant];
			atMJDate(mjd, &leap->epoch);
		}

	if (fits_close_file(fp, &status))
		{
			report_warning("unable to close %s [%d]\n",
					path, status);
			code = 1;
		}

	readDataFlag = 1;

	if (d0 != &buffer0[0])
		free(d0);
	if (d1 != &buffer1[0])
		free(d1);

	if (!code)
		report_verbose("leapsec FITS data was successfully read\n");

#if 1 /* DEBUG */
	report_debug("leapCount %d [epoch mjd step]\n", leapCount);
	for (i = 0; i < leapCount; ++i)
		{
			const Leap *leap = LEAPS + i;
			const AtTime *t = &leap->epoch;
			report_debug("\t%d/%02d/%02d+%02d:%02d:%02d %f %f %f\n",
					t->yr, t->mo, t->dy, t->hr, t->mn, t->sc, t->ms,
					leap->mjd, leap->step);
		}
#endif

	return code;
}



double leaps_from_to_next(double mjd0_utc, double start, double *mjd_next)
{
	const double SECONDS_PER_DAY = 86400;
	int i;
	double leaps = 0;
	double mjd_start_hat;

	/* mjd_start_hat is not in utc- it does not account for leap seconds */
	mjd_start_hat = mjd0_utc + start / SECONDS_PER_DAY;

	/* by default, avoid checking back often */
	*mjd_next = mjd_start_hat + 1e9;

	for (i = 0; i < leapCount; ++i)
		{
			const Leap *leap = LEAPS + i;

			/* leap_mjd_hat is in the same 'time system' as mjd_start_hat */
			double leap_mjd_hat = leap->mjd + leaps / SECONDS_PER_DAY;

			if (leap_mjd_hat > mjd_start_hat)
				{
					*mjd_next = leap_mjd_hat;
					break;
				}

			if (mjd0_utc < leap_mjd_hat)
				leaps += leap->step;
		}

	return leaps;
}

