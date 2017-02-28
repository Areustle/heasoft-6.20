#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "fitsio.h"

int sCC (double t, double *tZero, long *tc)
{

  FILE *tdc ;
  fitsfile *ffp;

  double subday, m0, m1, m2, end, t0 ;
  double modt, corr = -999999 , vtime0 = 1419.81266879;
  int error = 0 ;
  char *swcofile, *cofile;
  int ctstart, ctstop, cc0, cc1, cc2;
  long numrows, i;
  double *tstart, *tstop, *c0, *c1, *c2, t1;

  /* SWCOFILE environment variable will be written by swifttime */
  /* But we'll provide the usual fallbacks for standalone use */
  swcofile = (char *) malloc(sizeof(char)*240);
  cofile = (char *) malloc(sizeof(char)*240);
  if (getenv("SWCOFILE")){swcofile=getenv("SWCOFILE");}else{swcofile="swco.dat";}

  /* 04 Dec 2006 - sometimes CALDB-derived filename starts with http: or ftp: */
  if (!strncmp(swcofile,"/",1))
    strcpy(cofile,swcofile);
  else if (!strncmp(swcofile,"http:",5))
    strcpy(cofile,swcofile);
  else if (!strncmp(swcofile,"ftp:",4))
    strcpy(cofile,swcofile);
  else{
    if ( getenv("TIMING_DIR") ) {
      sprintf(cofile,"%s/%s",getenv("TIMING_DIR"),swcofile);
    }else if ( getenv("LHEA_DATA") ) {
      sprintf(cofile,"%s/%s",getenv("LHEA_DATA"),swcofile);
    }else{
      fprintf(stderr, "TIMING_DIR and LHEA_DATA are undefined\n");
      error = 5;
      return error;
    }
  }
   
  fits_open_file(&ffp, cofile, READONLY, &error);
  if (!error){
    fits_movabs_hdu(ffp, 2, NULL, &error);
    if (error){fits_report_error(stderr, error);}
   
    fits_get_num_rows(ffp, &numrows, &error);
    if (error){fits_report_error(stderr, error);}

    tstart = (double *) malloc(sizeof(double)*numrows);
    tstop = (double *) malloc(sizeof(double)*numrows);
    c0 = (double *) malloc(sizeof(double)*numrows);
    c1 = (double *) malloc(sizeof(double)*numrows);
    c2 = (double *) malloc(sizeof(double)*numrows);

    fits_get_colnum(ffp, FALSE, "TSTART", &ctstart, &error);
    if (error){fits_report_error(stderr, error);}
    fits_get_colnum(ffp, FALSE, "TSTOP", &ctstop, &error);
    if (error){fits_report_error(stderr, error);}
    fits_get_colnum(ffp, FALSE, "C0", &cc0, &error);
    if (error){fits_report_error(stderr, error);}
    fits_get_colnum(ffp, FALSE, "C1", &cc1, &error);
    if (error){fits_report_error(stderr, error);}
    fits_get_colnum(ffp, FALSE, "C2", &cc2, &error);
    if (error){fits_report_error(stderr, error);}
 
    fits_read_col_dbl(ffp, ctstart, 1, 1, numrows, 0, tstart, 0, &error);
    if (error){fits_report_error(stderr, error);}
    fits_read_col_dbl(ffp, ctstop, 1, 1, numrows, 0, tstop, 0, &error);
    if (error){fits_report_error(stderr, error);}
    fits_read_col_dbl(ffp, cc0, 1, 1, numrows, 0, c0, 0, &error);
    if (error){fits_report_error(stderr, error);}
    fits_read_col_dbl(ffp, cc1, 1, 1, numrows, 0, c1, 0, &error);
    if (error){fits_report_error(stderr, error);}
    fits_read_col_dbl(ffp, cc2, 1, 1, numrows, 0, c2, 0, &error);
    if (error){fits_report_error(stderr, error);}

    for (i=0;i<numrows;i++){
      if (t >= tstart[i] && t <= tstop[i]){
	t1 = (t-tstart[i])/86400.0;
	corr = c0[i] + c1[i]*t1 + c2[i]*t1*t1;
	break;
      }
    }

    free(tstart);
    free(tstop);
    free(c0);
    free(c1);
    free(c2);
    fits_close_file(ffp, &error);
  }else{ /* wasn't a FITS file, try ascii */
    error = 0;

    if (t/86400.0 >= vtime0) { /* check against first valid time */
 
      if ( !( tdc = fopen (cofile, "r") ) ) {
	fprintf(stderr, "Problem opening %s\n", cofile);
	error = -1 ;
	corr = 0 ;
      }

      while ( !error && ( fscanf (tdc, "%lg %lg %lg %lg", &m0, &m1, &m2, &end) ) ) {
	if ( end < 0.0 ) {
	  if ( m0 < 0.0 ) {
	    error = 2 ;
	    break ;
	  }
	  else {
	    subday = m0 ;
	    modt = t / 86400.0 - subday ;
	    t0 = m1 ;
	    /* must test for times before start of file!
	       05 Dec 2006: this test prevented corrections from
	       being returned near the block transitions.
	       if ( modt < 0.0 ) { break; } */
	    
	  }
	}
	else {
	  if ( modt < end ) {
	    corr = m0 + m1 * modt + m2 * modt * modt ;
	    break ;
	  }
	}
      }
      fclose (tdc);
    }
  }

  if ( error || ( corr == -999999 ) ) {
    fprintf(stderr, "Clock offset could not be determined from %s\n", cofile);
    corr = 0 ;
    error-- ;
  }
  free(cofile);

  *tZero = t0 ;
  *tc = (long) corr ;

  return error ;
}
