char *nccrcsid = "RCS: $Id: nCC.c,v 1.4 2013/06/25 00:43:34 craigm Exp $" ;
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "bary.h"

/*
 * Read NuSTAR clock data from clock file
 *
 * char *filename - name of input clock file
 * int *nrows - upon return contains number of rows in input file
 * double **data - upon return, pointer to data block
 * double **tstart - upon return, pointer to tstart
 * double **tstop - upon return, pointer to tstop
 * double **tz - upon return, pointer to tz
 * double **c0 - upon return, pointer to c0
 * double **c1 - upon return, pointer to c1
 * double **c2 - upon return, pointer to c2
 *
 * RETURNS: CFITSIO status code
 * User is responsible to free(*data); no need to free the other pointers separately.
 */
int nustarReadClockFile(char *filename, 
			long int *nrows,
			double **data, double **tstart, double **tstop,
			double **tz, double **c0, double **c1, double **c2)
			
{
  fitsfile *clockfile = 0;
  int status = 0;
  double nulval = 0;
  int anynul = 0;

  *nrows = 0;
  fits_open_data(&clockfile, filename, READONLY, &status);
  
  if (status) goto SAFE_RETURN;
  fits_get_num_rows(clockfile, nrows, &status);
  if (status) goto SAFE_RETURN;
  if (*nrows <= 0) return BAD_NAXES;

  *data = (double *)malloc(sizeof(double)*(*nrows)*6);
  if (*data == 0) {
    status = MEMORY_ALLOCATION;
    goto SAFE_RETURN;
  }

  *tstart = *data;
  *tstop  = *tstart + *nrows;
  *tz     = *tstop + *nrows;
  *c0     = *tz + *nrows;
  *c1     = *c0 + *nrows;
  *c2     = *c1 + *nrows;

  fits_read_col(clockfile, TDOUBLE, 1, 1, 1, *nrows, &nulval, *tstart, &anynul, &status);
  fits_read_col(clockfile, TDOUBLE, 2, 1, 1, *nrows, &nulval, *tstop, &anynul, &status);
  fits_read_col(clockfile, TDOUBLE, 3, 1, 1, *nrows, &nulval, *tz, &anynul, &status);
  fits_read_col(clockfile, TDOUBLE, 4, 1, 1, *nrows, &nulval, *c0, &anynul, &status);
  fits_read_col(clockfile, TDOUBLE, 5, 1, 1, *nrows, &nulval, *c1, &anynul, &status);
  fits_read_col(clockfile, TDOUBLE, 6, 1, 1, *nrows, &nulval, *c2, &anynul, &status);
      
  /* Make sure we can safely close this file */
 SAFE_RETURN:  
  if (clockfile) {
    int safeerr = 0;
    fits_close_file(clockfile, &safeerr);
  }
  return status;
}

/*
 * nCC - NuSTAR clock correction
 * 
 * INPUTS:
 *   t - mission time in seconds, used as a index
 *   NUSTAR_CLOCK_FILE - environment variable specifying
 *     file path to NuSTAR clock file
 *
 * OUTPUTS:
 *   *tzero - upon return contains mission TIMEZERO value [s]
 *   *tcorr - upon return contains clock offset [us]
 * 
 * RETURNS:
 *   0  = success
 *   -1 = error opening/reading clock correction
 *   -2 = input value of t was out of bounds
 * 
 * When nCC is first called, NUSTAR_CLOCK_FILE is opened and
 * the contents are read once.  If NUSTAR_CLOCK_FILE is not set
 * then the file is ignored and a correction of zero is returned.
 * The input table must have a fixed structure.
 *
 * After the file has been read, a lookup table is used to locate
 * the correction and apply it.
 */

int nCC (double t, double *tZero, double *tcorr, char *clockFile)
{
  static int filestate = 0;
  /* State variable:
      0  = pre-initialization
      -1 = error opening/reading clock file data
      1  = clock data initialized
      2  = no clock file specified 
  */
  static long int nrows = 0;
  static double *data = 0, *tstart = 0, *tstop = 0, 
    *tz = 0, *c0 = 0, *c1 = 0, *c2 = 0;
  int i, ind;
  char *nustarClockFile;
  int prev_ind = -1;

  *tcorr = 0;
  *tZero = 0;

  nustarClockFile = clockFile ? clockFile : getenv("NUSTAR_CLOCK_FILE");
  /* Open FITS clock file upon the first read */
  if (filestate == 0) {
    int status = 0;

    if (nustarClockFile == 0) {
      filestate = 2;
    } else {

      status = nustarReadClockFile(nustarClockFile, &nrows, 
				   &data, &tstart, &tstop, &tz, &c0, &c1, &c2);

      if (status || nrows == 0 || data == 0) {
	filestate = -1;
	fprintf(stderr, "ERROR: could not read clock file '%s' (error code %d)\n",
		nustarClockFile ? nustarClockFile : "(not specified)", status);
	return filestate;
      }
      
      filestate = 1;
    }
  }

  /* If no clock file specified, then just return */
  if (filestate == 2) return 0;

  /* If we already know which row we have been accessing... */

  if (prev_ind >= 0 && t >= tstart[prev_ind] && t < tstop[prev_ind]) {
    ind = prev_ind;
  } else {
    /* Search for bracketing time range */
    ind = prev_ind = -1;
    for (i=0; i<nrows; i++) {
      if (t >= tstart[i] && t < tstop[i]) {
	ind = i; /* Found it! */
	break;
      }
    }
  }

  if (ind >= 0) {
    /* Argument of quadratic correction is time in days from TSTART */
    double t1 = (t - tstart[ind])/86400.0;
    
    /* Apply quadratic correction term */
    *tcorr = c0[ind] + c1[ind]*t1 + c2[ind]*t1*t1;
    *tZero = tz[ind];
    /* printf("   %f %f %f %f %f %f\n", 
       t, t1, c0[ind], c1[ind], c2[ind], *tcorr); */
    
    /* Return immediately because we found the right interval */
    prev_ind = ind;  /* Save this for next time */
    return 0;
  }

  prev_ind = -1; /* Reset state */

  /* No correction found: sad panda :-( */
  /* Warn once about out of bounds */
  {
    static int warned = 0;
    if (! warned ) {
      fprintf(stderr, 
	 "WARNING: some/all of the input data file is outside the clock\n"
	 "  file validity range. Assuming the NuSTAR clock correction is\n"
	 "  zero!\n"
	 "  ===> Absolute timing errors will be ~150 msec\n");
      warned = 1;
    }
  }
  *tZero = 0;
  *tcorr = 0;
  return -2;
}
