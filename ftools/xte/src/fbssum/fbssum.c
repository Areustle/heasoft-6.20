/* RCS: $Id: fbssum.c,v 1.6 2001/10/15 19:27:26 zpan Exp $ */
/*-----------------------------------------------------------------------
 *
 *  fBssum.c
 *
 *  Date: 21 March 1997
 *
 *  Arnold Rots, USRA
 *
 *  fBssum allows summing spectral bins in an fB file.  Output is to stdout.
 * 
 *  07 October 1998: reFTOOLized by M.Tripicco
 *      (w/ "3" appended to fB and Rd_Param for uniqueness)
 *
 *----------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fitsio.h>
#include <longnam.h>

#include <string.h>
#include <errno.h> 
#include "fitsio.h"
#include "xpi.h"
#define ERRMSG 255          /* Must be bigger than FLEN_ERRMSG */

#define NCHAN   4096

char *fB3 = "fBssum $Revision: 1.6 $" ;
void Fbssum()
{
  
  fitsfile *fits_file;
  char line[1024] ;
  char progName[256] ;
  char dataFile[1025] ;
  int chb[NCHAN] ;
  int nchb ;
  int hratio = 0 ;
  int moments = 0 ;
  double pmom[3] = {-1.0, -1.0, 0.25} ;
  double **image ;
  double **errs ;
  double baseline[NCHAN] ;
  char keystrval[80] ;
  char comment[80] ;
  char *rid = comment ;
  int nfiles = 0 ;
  int error = 0 ;
  int htype ;
  long nrows, nch, i, j ;
  long nrows0, nch0, k, l ;
  int n, n1, n2 ;
  double nulv = 0.0 ;
  int any ;
  double *expos ;
  double dnphase0, expos0=0.0 ;
  double *phase ;
  double inspec[NCHAN] ;
  double inerr[NCHAN] ;
  double outspec[NCHAN] ;
  long numbers[NCHAN] ;
  double day, day1 ;
  double div ;
  double peak[NCHAN] ;
  double pk2p[NCHAN] ;
  double mom0[NCHAN] ;
  double mom1[NCHAN] ;
  double mom2[NCHAN] ;
  double mom3[NCHAN] ;
  double x, x0, x1, x2, x3, y ;
  double top, cutoff ;

  char chbstr[256];
  char msg[ERRMSG], msg2[ERRMSG];
  int Rd_Param3(char *, char *, int *, double *);

/*
 *  Decode command line options -----------------------------------------
 */
  chb[0] = 0 ;
  for (i=1; i<NCHAN; i++)
    chb[i] = -1 ;
    
  strcpy(progName, "fbssum");
  if ( Rd_Param3(dataFile, chbstr, &hratio, pmom) )
    exit (1) ;
  
  n=0;
  chb[0]=atoi(chbstr);
  for (i=0; i<strlen(chbstr);i++){
    if ( isspace(chbstr[i]) != 0 ){
      n++;
      chb[n]=atoi(&chbstr[i]);
    }
  }
  
/*
 *  Open input file -----------------------------------------------------
 */
  if ( fits_open_file (&fits_file, dataFile, READONLY, &error) ) {
    fprintf (stderr,"%s: Failed to open input file %s\n",
	     progName, dataFile) ;
    exit (2) ;
  }

/*
 *  Check first extension -----------------------------------------------
 */
  fits_movabs_hdu (fits_file, 2, &htype, &error) ;
  fits_read_key (fits_file, TSTRING, "HDUCLAS5", keystrval, comment, &error) ;
  if ( strcmp (keystrval, "fB") ) {
    fprintf (stderr,"%s: Input file %s is of the wrong type\n",
	     progName, dataFile) ;
    fits_delete_file (fits_file, &error) ;
    exit (3) ;
  }
  fits_read_key_lng (fits_file,        "NAXIS2",    &nrows0, comment, &error) ;
  fits_read_key_dbl (fits_file,      "PHASEDEL",  &dnphase0, comment, &error) ;
  fits_read_key_dbl (fits_file,        "TSTART",       &day, comment, &error) ;
  fits_read_key_dbl (fits_file,         "TSTOP",      &day1, comment, &error) ;
  fits_read_key_lng (fits_file,      "DETCHANS",      &nch0, comment, &error) ;

  day = 0.5 * (day + day1) ;
  nchb = 1 ;
  while ( chb[nchb] >= chb[nchb-1] )
    nchb++ ;
  if ( nchb == 1 )
    chb[1] = nch0 + 1 ;
  else {
    nchb-- ;
    chb[nchb]++ ;
  }

/*
 *  Allocate and initialize memory --------------------------------------
 */
  image = (double **) malloc (nrows0*sizeof(double *)) ;
  *image = (double *) malloc (nchb*nrows0*sizeof(double)) ;
  errs = (double **) malloc (nrows0*sizeof(double *)) ;
  *errs = (double *) malloc (nchb*nrows0*sizeof(double)) ;
  for (i=0; i<nrows0; i++) {
    image[i] = *image + i*nchb ;
    errs[i] = *errs + i*nchb ;
    for (j=0; j<nchb; j++) {
      image[i][j] = 0.0 ;
      errs[i][j] = 0.0 ;
    }
  }
  for (j=0; j<nchb; j++)
    baseline[j] = 0.0 ;

  phase = (double *) malloc (nrows0*sizeof(double)) ;
  expos = (double *) malloc (nrows0*sizeof(double)) ;

  if ( pmom[0] >= 0.0 ) {
    if ( pmom[1] < pmom[0] )
      pmom[1] = 10.0 ;
    moments = 1 ;
  }

/*
 *  Scalar columns ------------------------------------------------------
 */
  fits_read_col (fits_file, TDOUBLE, 2, 1, 1, nrows0, &nulv, phase,
		 &any, &error) ;
  fits_read_col (fits_file, TDOUBLE, 7, 1, 1, nrows0, &nulv, expos,
		 &any, &error) ;

/*
 *  Row loop ------------------------------------------------------------
 */
  for (k=0; k<nrows0; k++) {
    fits_read_col (fits_file, TDOUBLE, 4, k+1, 1, nch0, &nulv, inspec,
		   &any, &error) ;
    fits_read_col (fits_file, TDOUBLE, 5, k+1, 1, nch0, &nulv, inerr,
		   &any, &error) ;
    for (j=0; j<nchb; j++) {
      for (i=chb[j]; i<chb[j+1]; i++) {
	image[k][j] += inspec[i] ;
	errs[k][j] += inerr[i] * inerr[i] ;
      }
      image[k][j] /= expos[k] ;
      errs[k][j] = sqrt (errs[k][j]) / expos[k] ;
    }
  }

/*
 *  Baseline ------------------------------------------------------------
 */
  fits_read_col (fits_file, TDOUBLE, 6, 1, 1, nch0, &nulv,  inspec,
		 &any, &error) ;
  for (j=0; j<nchb; j++) {
    for (i=chb[j]; i<chb[j+1]; i++)
      baseline[j] += inspec[i] ;
    baseline[j] /= expos[0] ;
  }

/*
 *  Do the moments ------------------------------------------------------
 */
/*
 *  Set the range ------------------------------------------
 */
  if ( moments ) {
    while ( pmom[0] > phase[0] )
      pmom[0]-- ;
    while ( pmom[0] < phase[0] )
      pmom[0]++ ;
    while ( pmom[1] > pmom[0] )
      pmom[1]-- ;
    while ( pmom[1] <= pmom[0] )
      pmom[1]++ ;
    n1 = (pmom[0] - phase[0]) / dnphase0 ;
    n2 = (pmom[1] - phase[0]) / dnphase0 ;
    if ( n1 > nrows0 )
      n1 = nrows0 ;
    if ( n2 > nrows0 )
      n2 = nrows0 ;
    n = 1.0 / dnphase0 - 0.5 ;
    if ( ( n2 - n1 ) > n )
      n2 = n1 + n ;
/*
 *  Find the peak ------------------------------------------
 */
    for (j=0; j<nchb; j++) {
      x0 = x1 = x2 = x3 = expos0 = 0.0 ;
      peak[j] = -1.0 ;
      top = -1000.0 ;
      for (i=n1; i<=n2; i++)
	if ( image[i][j] > top ) {
	  top = image[i][j] ;
	  peak[j] = phase[i] ;
	  k = i ;
	}
      cutoff = pmom[2] * top ;
/*
 *  Sum things up ------------------------------------------
 */
      for (i=n1; i<=n2; i++)
	if  ( image[i][j] >= cutoff ) {
	  x = phase[i] ;
	  y = image[i][j] ;
	  x0 += y ;
	  x1 += x * y ;
	  x2 += x * x * y ;
	  x3 += x * x * x * y ;
	  expos0 += expos[i] ;
	}
      if ( peak[j] > 1.0 )
	peak[j]-- ;
/*
 *  Calculate the moments ----------------------------------
 */
      if ( x0 != 0.0 ) {
	x3 = x0*x0*x3 + 2.0*x1*x1*x1 - 3.0*x0*x1*x2 ;
	if (x3 > 0.0)
	  x3 = exp (log (x3) / 3.0) / x0 ;
	else if (x3 < 0.0)
	  x3 = -exp (log (-x3) / 3.0) / x0 ;
	x1 = x1 / x0 ;
	x2 = sqrt (x2/x0 - x1*x1) ;
	if (x1 < 0.0)
	  x1 += 1.0 ;
	else if (x1 >= 1.0)
	  x1 -= 1.0 ;
      }
      mom0[j] = x0 / (n2 - n1 + 1) ;
      mom1[j] = x1 ;
      mom2[j] = x2 ;
      mom3[j] = x3 ;
/*
 *  Calculate the peak -------------------------------------
 */
      x= 2.0 * (2.0 * image[k][j] - image[k-1][j] - image[k+1][j]) ;
      if ( x )
	pk2p[j] = peak[j] + dnphase0 * (image[k+1][j] - image[k-1][j]) / x ;
      else
	pk2p[j] = -2.0 ;
    }
/*
 *  Print moments ------------------------------------------
 */
    c_fcecho(" ");
    sprintf(msg,"# Expo: %9.3f   %9.2f",day,expos0);
    c_fcecho(msg);
    sprintf(msg,"# Mom0: %9.3f  ", day) ;
    for (j=0; j<nchb; j++){
      sprintf (msg2," %9.1f", mom0[j]) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg);
    sprintf (msg,"# Peak: %9.3f  ", day) ;
    for (j=0; j<nchb; j++){
      sprintf (msg2," %9.4f", peak[j]) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg);
    sprintf (msg,"# Pk2p: %9.3f  ", day) ;
    for (j=0; j<nchb; j++){
      sprintf (msg2," %9.4f", pk2p[j]) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg);
    sprintf (msg,"# Mom1: %9.3f  ", day) ;
    for (j=0; j<nchb; j++){
      sprintf (msg2," %9.4f", mom1[j]) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg);
    sprintf (msg,"# Mom2: %9.3f  ", day) ;
    for (j=0; j<nchb; j++){
      sprintf (msg2," %9.4f", mom2[j]) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg);
    sprintf (msg,"# Mom3: %9.3f  ", day) ;
    for (j=0; j<nchb; j++){
      sprintf (msg2," %9.4f", mom3[j]) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg);
  }

/*
 *  Print the baseline and column headings ------------------------------
 */
  sprintf (msg,"# \n# Zero: %9.3f  ", day) ;
  for (j=0; j<nchb; j++){
    sprintf (msg2," %8.1f", baseline[j]) ;
    strcat(msg,msg2);
  }
  c_fcecho(msg);
  c_fcecho("# ");
  c_fcecho("# Columns:\n#  [Phase]  [MJD(TDB)]  [Count rates for spectral ranges:]") ;
  sprintf(msg,"#  ");
  for (j=0; j<nchb; j++){
    sprintf (msg2,"   %d to %d", chb[j], chb[j+1]-1) ;
    strcat(msg,msg2);
  }
  c_fcecho(msg);
  if ( hratio ) {
    c_fcecho("# [Hardness ratios for:]") ;
    sprintf(msg,"#  ");
    for (j=0; j<nchb-1; j++){
      sprintf (msg2,"   (%d to %d) / (%d to %d)", chb[j], chb[j+1]-1, chb[j+1], chb[j+2]-1) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg);
  }
  c_fcecho("# [Exposure time (s)]  [Errors for spectral ranges:]") ;
  sprintf(msg,"#  ");
  for (j=0; j<nchb; j++){
    sprintf (msg2,"   %d to %d", chb[j], chb[j+1]-1) ;
    strcat(msg,msg2);
  }
  c_fcecho(msg);
  c_fcecho("# ") ;

/*
 *  Print the rest out --------------------------------------------------
 */
  for (i=0; i<nrows0; i++) {
    sprintf (msg,"%5.3f %9.3f  ", phase[i], day) ;
    for (j=0; j<nchb; j++){
      sprintf (msg2," %8f", image[i][j]) ;
      strcat(msg,msg2);
    }
    if ( hratio ) {
      for (j=0; j<nchb-1; j++) {
	if ( image[i][j] > 0.0 ) {
	  if ( image[i][j+1] > 0.0 )
	    x = log10 ( image[i][j] / image[i][j+1] ) ;
	    else
	      x = 2.0 ;
	  }
	  else
	    x = -2.0 ;
	  sprintf (msg2," %6g", x) ;
	  strcat(msg,msg2);
	}
      }
    sprintf (msg2,"  %9.2f", expos[i]) ;
    strcat(msg,msg2);
    for (j=0; j<nchb; j++){
      sprintf (msg2," %6f", errs[i][j]) ;
      strcat(msg,msg2);
    }
    c_fcecho(msg) ;
    }

/*
 *  Last fixes and close ------------------------------------------------
 */
  fits_close_file (fits_file, &error) ;
}

int Rd_Param3(char *infile, char *chbstr, 
	     int *hratio, double *pmom)
{
  int BufLen_2 = 255;
  int parstat = 0;
  char text[FLEN_ERRMSG];

  Uclgst("infile", infile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get infile parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgst("chanbins", chbstr, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get chanbins parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsb("hratio",hratio, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get hratio parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  if (*hratio != 0) *hratio=1;

  Uclgsd("startphase", &pmom[0], &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get startphase parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsd("endphase", &pmom[1], &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get endphase parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsd("threshold", &pmom[2], &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get threshold parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  return parstat;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL effbeessum
#endif
#ifdef unix
#define F77CALL effbeessum_
#endif

void F77CALL() 
{ 
  void Fbssum();

  Fbssum(); 
}
