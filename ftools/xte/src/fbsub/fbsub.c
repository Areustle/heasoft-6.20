/* RCS: $Id: fbsub.c,v 1.6 2000/03/30 14:40:25 miket Exp $ */
/*-----------------------------------------------------------------------
 *
 *  fBsub.c
 *
 *  Date: 20 March 1997
 *
 *  Arnold Rots, USRA
 *
 *  fBsub allows subtraction of unpulsed background from a pha (fB) file.
 *  The average spectrum between phasein and phasemax is subtracted from
 *  all spectra in the Data column, and added to the Baseline column (so
 *  that fBsub can be run multiple times).  The errors are not touched.
 *
 *  08 October 1998: reFTOOLized by M.Tripicco
 *    (w/ "4" appended to fB and Rd_Param for uniqueness)
 *
 *  30 March 2000: moved fclose() to avoid closing null file pointer
 *                 (see also fbfsum, fbadd)
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

#define NCHAN   4096

char *fB4 = "fBsub $Revision: 1.6 $" ;
void Fbsub()
{

  fitsfile *fits_file;
  char line[1024] ;
  char progName[256] ;
  char dataFile[1024] ;
  char outFile[1025] ;
  char keystrval[80] ;
  char comment[80] ;
  int error = 0 ;
  int htype ;
  long nrows, nch, i, j, n1, n2, n3, n4 ;
  double nulv = 0.0 ;
  int any ;
  double dnphase, x ;
  double ph1, ph2, ph3, ph4, phase ;
  double base[NCHAN] ;
  double spec[NCHAN] ;
  double nbas[NCHAN] ;
  double oerr[NCHAN] ;
  double nerr[NCHAN] ;

  int clobber = 0;
  int n;
  FILE *fp;
  int Rd_Param4(char *, char *, double *, double *,
		double *, double *, int *) ;

  /* Read Parameter File */

  strcpy(progName, "fbsub");
  if ( Rd_Param4(dataFile, outFile, &ph1, &ph2, 
		 &ph3, &ph4, &clobber) )
    exit (1);

  while ( ph1 < 0.0 )
    ph1++ ;
  while ( ph1 >= 1.0 )
    ph1-- ;
  while ( ph2 > ph1 )
    ph2-- ;
  while ( ph2 <= ph1 )
    ph2++ ;
  if ( ph3 >= 0.0 ) {
    while ( ph3 >= 1.0 )
      ph3-- ;
    while ( ph4 > ph3 )
      ph4-- ;
    while ( ph4 <= ph3 )
      ph4++ ;
  }

/*
 *  Copy the input file -------------------------------------------------
 */
  if ( !strstr (outFile, ".") )
    strcat (outFile, ".pha") ;

  if ( (fp=fopen(outFile,"r")) != NULL) { /* file already exists */
    if (clobber != 1) {                   /* but clobber isn't set... */
      fprintf (stderr,"%s: output file %s already exists\n", progName, outFile);
      exit (1) ;
    }
    n=fclose(fp);
  }

  sprintf (line, "cp %s %s", dataFile, outFile) ;
  if ( system (line) ) {
    fprintf (stderr,"%s: Could not open input file %s or create output file %s\n",
	     progName, dataFile, outFile) ;
    exit (1) ;
  }

/*
 *  Fix up output file --------------------------------------------------
 */
  if ( fits_open_file (&fits_file, outFile, READWRITE, &error) ) {
    fprintf (stderr,"%s: Failed to re-open output file %s\n",
	     progName, outFile) ;
    exit (2) ;
  }
  fits_write_date (fits_file, &error) ;
  fits_write_chksum (fits_file, &error) ;

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
  fits_read_key_lng (fits_file,        "NAXIS2",    &nrows, comment, &error) ;
  fits_read_key_dbl (fits_file,      "PHASEDEL",  &dnphase, comment, &error) ;
  fits_read_key_lng (fits_file,      "DETCHANS",      &nch, comment, &error) ;
  fits_read_col (fits_file, TDOUBLE, 2, 1, 1, 1, &nulv, &phase, &any, &error) ;
  n1 = (ph1 - phase) / dnphase + 1.5 ;
  n2 = (ph2 - phase) / dnphase + 1.5 ;
  if ( ph3 >= 0.0 ) {
    n3 = (ph3 - phase) / dnphase + 1.5 ;
    n4 = (ph4 - phase) / dnphase + 1.5 ;
  }
  else
    n3 = n4 = 0 ;
  if ( n2 > nrows ) {
    fprintf (stderr,"%s: Phase %f not contained in Input file %s\n",
	     progName, ph2, dataFile) ;
    fits_delete_file (fits_file, &error) ;
    exit (4) ;
  }
  if ( n4 > nrows ) {
    fprintf (stderr,"%s: Phase2 %f not contained in Input file %s\n",
	     progName, ph4, dataFile) ;
    fits_delete_file (fits_file, &error) ;
    exit (5) ;
  }

/*
 *  Get baseline correction ---------------------------------------------
 */
  for (i=0; i<nch; i++)
    base[i] = 0.0 ;
  for (j=n1; j<=n2; j++) {
    fits_read_col (fits_file, TDOUBLE, 4, j, 1, nch, &nulv, spec, &any, &error) ;
    for (i=0; i<nch; i++)
      base[i] += spec[i] ;
  }
  x = 1.0 / (n2 - n1 + 1) ;
  if ( n3 ) {
    for (j=n3; j<=n4; j++) {
      fits_read_col (fits_file, TDOUBLE, 4, j, 1, nch, &nulv, spec, &any, &error) ;
      for (i=0; i<nch; i++)
	base[i] += spec[i] ;
    }
    x = 1.0 / (n2 - n1 + n4 - n3 + 2) ;
  }
  for (i=0; i<nch; i++)
    base[i] *= x ;
  
/*
 *  Set new baseline ----------------------------------------------------
 */
  fits_read_col (fits_file, TDOUBLE, 6, 1, 1, nch, &nulv, nbas, &any, &error) ;
  for (i=0; i<nch; i++) {
    nbas[i] += base[i] ;
    nerr[i] = nbas[i] * (1.0 + x) ;
  }

/*
 *  Enter corrections ---------------------------------------------------
 */
  for (j=1; j<=nrows; j++) {
    fits_read_col (fits_file, TDOUBLE, 4, j, 1, nch, &nulv, spec, &any, &error) ;
    for (i=0; i<nch; i++) {
      spec[i] -= base[i] ;
      oerr[i] = sqrt (spec[i] + nerr[i]) ;
    }
    fits_write_col_dbl (fits_file, 4, j, 1, nch, spec, &error) ;
    fits_write_col_dbl (fits_file, 5, j, 1, nch, oerr, &error) ;
    fits_write_col_dbl (fits_file, 6, j, 1, nch, nbas, &error) ;
  }
  
/*
 *  Last fixes and close ------------------------------------------------
 */
  fits_write_date (fits_file, &error) ;
  fits_modify_key_str (fits_file, "CREATOR", fB4, "&", &error) ;
  fits_write_chksum (fits_file, &error) ;

  fits_close_file (fits_file, &error) ;
}

int Rd_Param4(char *infile, char *outfile,
	     double *phasemin, double *phasemax, 
	      double *phasemin2, double *phasemax2, int *clobber)
{
  int BufLen_2 = 255;
  int parstat = 0;
  char text[FLEN_ERRMSG];

  *phasemax = -100.0;
  *phasemin2 = -100.0;
  *phasemax2 = -100.0;

  Uclgst("infile", infile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get infile parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgst("outfile", outfile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get outfile parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsd("phasemin", phasemin, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get phasemin parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsd("phasemax", phasemax, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get phasemax parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsd("phasemin2", phasemin2, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get phasemin2 parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsd("phasemax2", phasemax2, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get phasemax2 parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsb("clobber",clobber, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get clobber parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  if (*clobber != 0) *clobber=1;
  
  return parstat;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL effbeesub
#endif
#ifdef unix
#define F77CALL effbeesub_
#endif

void F77CALL() 
{ 
  void Fbsub();

  Fbsub(); 
}
