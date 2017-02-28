/****************************************************************************
* This program calculates the nominal aspect point for an  attitude file.
****************************************************************************/
#include <math.h>
#include <string.h>

#include "headas.h"
#include "headas_utils.h"
#include "gtis.h"

int writeNewAttitude(fitsfile *oldatt, char *newfilename,
		     GTIS *gti,
		     double ra, double dec, double roll,
		     double qparam[4])
{
  int status = 0;
  int safestatus = 0;
  fitsfile *newptr = 0;
  long int nrows;
  int ncols, colnum;
  char keyname[FLEN_CARD], colname[FLEN_CARD];

  if ((oldatt == 0) || (newfilename == 0) || (newfilename[0] == 0)) {
    return NULL_INPUT_PTR;
  }

  if (strcasecmp(newfilename, "NONE") == 0) {
    return 0;
  }

  /* Copy old attitude file to new one */
  headas_clobberfile(newfilename);
  fits_create_file(&newptr, newfilename, &status);
  if (status) return status;
  fits_copy_file(oldatt, newptr, 1, 1, 1, &status);
  if (status) {
    fits_close_file(newptr, &safestatus);
    return status;
  }

  /* Move to ATTITUDE extension */
  fits_movnam_hdu(newptr,BINARY_TBL,"ATTITUDE",0/* ignore version*/,&status);

  /* Make sure there are two rows */
  fits_get_num_rows(newptr, &nrows, &status);
  fits_get_num_cols(newptr, &ncols, &status);
  fits_delete_rows(newptr, 1, nrows, &status);
  fits_insert_rows(newptr, 0, 2, &status);
  if (status) return status;

  /* Remove extraneous columns */
  for (colnum = 1; ; ) {
    fits_write_errmark();
    fits_make_keyn("TTYPE", colnum, keyname, &status);
    fits_read_key(newptr, TSTRING, keyname, colname, 0, &status);
    fits_clear_errmark();
    if (status) break;

    /* Check for desireable or undesireable column names */
    if ( (strcasecmp(colname, "TIME") == 0) ||
	 (strcasecmp(colname, "QPARAM") == 0) ||
	 (strcasecmp(colname, "POINTING") == 0) ) {
      /* This column is okay, skip it */
      colnum ++;
      /* printf("Found column %s at position %d\n", colname, colnum); */
    } else {
      /* This column is unknown, delete it */
      fits_delete_col(newptr, colnum, &status);
      /* Do *NOT* increment column */
      if (status) break;
      /* printf("Deleting column %s at position %d\n", colname, colnum); */
    }
  }
  status = 0;

  /* Write each column, and repeat the attitude data identically for
     the start and stop times */
  do {

    /* ======== TIME */
    fits_write_errmark();
    fits_get_colnum(newptr, CASEINSEN, "TIME", &colnum, &status);
    fits_clear_errmark();
    if (status == 0) {
      int i;
      double tstart, tstop;

      /* Get start/stop time */
      tstart = gti->start[0]; tstop = gti->stop[0];
      for (i=1; i < (gti->n); i++) {
	if (gti->start[i] < tstart) { tstart = gti->start[i]; }
	if (gti->stop[i]  > tstop)  { tstop  = gti->stop[i];  }
      }

      /* printf("Writing times %f and %f to column %d\n", tstart, tstop, colnum); */
      fits_write_col(newptr, TDOUBLE, colnum, 1, 1, 1, &tstart, &status);
      fits_write_col(newptr, TDOUBLE, colnum, 2, 1, 1, &tstop, &status);
      
      fits_update_key(newptr, TDOUBLE, "TSTART", &tstart, 0, &status);
      fits_update_key(newptr, TDOUBLE, "TSTOP", &tstop, 0, &status);

      if (status) break;
    } 
    status = 0;

    /* ======== POINTING */
    fits_write_errmark();
    fits_get_colnum(newptr, CASEINSEN, "POINTING", &colnum, &status);
    fits_clear_errmark();
    if (status == 0) {
      double pointing[3];
      pointing[0] = ra; pointing[1] = dec; pointing[2] = roll;
      fits_write_col(newptr, TDOUBLE, colnum, 1, 1, 3, pointing, &status);
      fits_write_col(newptr, TDOUBLE, colnum, 2, 1, 3, pointing, &status);
      if (status) break;
    } 
    status = 0;

    /* ======== QPARAM */
    fits_write_errmark();
    fits_get_colnum(newptr, CASEINSEN, "QPARAM", &colnum, &status);
    fits_clear_errmark();
    if (status == 0) {
      fits_write_col(newptr, TDOUBLE, colnum, 1, 1, 4, qparam, &status);
      fits_write_col(newptr, TDOUBLE, colnum, 2, 1, 4, qparam, &status);
      if (status) break;
    } 
    status = 0;

  } while (0);

  /* Return if there was an error */
  fits_close_file(newptr, &safestatus);

  return status;
  
}
