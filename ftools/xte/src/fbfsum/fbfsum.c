/* RCS: $Id: fbfsum.c,v 1.5 2000/03/14 20:46:34 miket Exp $ */
/*-----------------------------------------------------------------------
 *
 *  fBfsum.c
 *
 *  Date: 21 March 1997
 *
 *  Arnold Rots, USRA
 *
 *  fBfsum allows summing phase bins in an fB file.  Errors are added in
 *  quadrature.
 *
 *  07 October 1998: reFTOOLized by M.Tripicco
 *     (w/ "2" appended to fB and Rd_Param for uniqueness)
 *
 *  14 March 2000: moved fclose inside brackets so as not to close a null
 *                 file pointer...
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

char *fB2 = "fBfsum $Revision: 1.5 $" ;
void Fbfsum()
{

  fitsfile *fits_infile;
  fitsfile *fits_outfile;
  char line[1024] ;
  char progName[256] ;
  char dataFile[1025] ;
  int phb[3] = {0, 0, 0} ;
  char outFile[1025] ;
  char keystrval[80] ;
  char comment[80] ;
  char *rid = comment ;
  int nfiles = 0 ;
  int error = 0 ;
  int htype ;
  long nrows, nch, i, j ;
  long nrows0, nch0, k, l ;
  double nulv = 0.0 ;
  long lnulv = 0 ;
  int any ;
  double dnphase, expos ;
  double dnphase0, expos0 ;
  double phase ;
  double inspec[NCHAN] ;
  double outspec[NCHAN] ;
  long numbers[NCHAN] ;
  double div ;

  int clobber = 0;
  int n = 0;
  FILE *fp;
  int Rd_Param2(char *, char *, int *, int *);
  
  /* Read Parameter File */

  strcpy(progName, "fbfsum");
  if ( Rd_Param2(dataFile, outFile, &clobber, phb) )
    exit (1) ;

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
  if ( fits_open_file (&fits_outfile, outFile, READWRITE, &error) ) {
    fprintf (stderr,"%s: Failed to re-open output file %s\n",
	     progName, outFile) ;
    exit (2) ;
  }
  fits_write_date (fits_outfile, &error) ;
  fits_write_chksum (fits_outfile, &error) ;

/*
 *  Check first extension -----------------------------------------------
 */
  fits_movabs_hdu (fits_outfile, 2, &htype, &error) ;
  fits_read_key (fits_outfile, TSTRING, "HDUCLAS5", keystrval, comment, &error) ;
  if ( strcmp (keystrval, "fB") ) {
    fprintf (stderr,"%s: Input file %s is of the wrong type\n",
	     progName, dataFile) ;
    fits_delete_file (fits_outfile, &error) ;
    exit (3) ;
  }
  fits_read_key_lng (fits_outfile,        "NAXIS2",    &nrows0, comment, &error) ;
  fits_read_key_dbl (fits_outfile,      "PHASEDEL",  &dnphase0, comment, &error) ;
  fits_read_key_lng (fits_outfile,      "DETCHANS",      &nch0, comment, &error) ;
  fits_delete_rows (fits_outfile, 1, nrows0, &error) ;
  if ( phb[0] <= 0 )
    phb[0] = 5 ;
  if ( ( phb[1] <= 0 ) || ( phb[1] > nrows0 ) )
    phb[1] = 1 ;
  if ( ( phb[2] < phb[1] ) || ( phb[2] > nrows0 ) )
    phb[2] = nrows0 ;
  if ( phb[0] > (phb[2] - phb[1] + 1) )
    phb[0] = phb[2] - phb[1] + 1 ;
  nrows0 = (phb[2] - phb[1] + 1) / phb[0] ;
  dnphase0 *= phb[0] ;
  div = (double) 1.0 / phb[0] ;
  fits_modify_key_lng (fits_outfile,     "NAXIS2",   nrows0,    "&", &error) ;
  fits_modify_key_dbl (fits_outfile,   "PHASEDEL", dnphase0, 3, "&", &error) ;

/*
 *  Open input file -----------------------------------------------------
 */
    if ( fits_open_file (&fits_infile, dataFile, READONLY, &error) ) {
      fprintf (stderr,"%s: Failed to open input file %s\n",
	       progName, dataFile) ;
      exit (2) ;
    }
    fits_movabs_hdu (fits_infile, 2, &htype, &error) ;

/*
 *  Output row loop -----------------------------------------------------
 */
  fits_read_col (fits_infile, TLONG, 3, 1, 1,   nch0, &lnulv, &numbers,
		   &any, &error) ;
  j = phb[1] ;
  for (k=1; k<=nrows0; k++) {

  
/*
 *  Enter new values -----------------------------------------------------
 */
    fits_write_col_lng (fits_outfile, 1, k, 1,    1, &k,      &error) ;
    fits_write_col_lng (fits_outfile, 3, k, 1, nch0, numbers, &error) ;

/*
 *  Phase ------------------------------------------------------
 */
    fits_read_col (fits_infile, TDOUBLE, 2, j, 1,   1, &nulv, &expos0,
		   &any, &error) ;
    for (l=1; l<phb[0]; l++) {
      fits_read_col ( fits_infile, TDOUBLE, 2, j+l, 1,   1, &nulv,  &expos,
		     &any, &error) ;
      expos0 += expos ;
    }
    expos0 *= div ;
    fits_write_col_dbl (fits_outfile, 2, k, 1,   1, &expos0, &error) ;
    sprintf (comment, "Phase %5.3g", expos0) ;
    fits_write_col_str (fits_outfile, 8, k, 1,   1,    &rid, &error) ;
/*
 *  Counts -----------------------------------------------------
 */
    fits_read_col (fits_infile, TDOUBLE, 4, j, 1, nch0, &nulv, outspec,
		   &any, &error) ;
    for (l=1; l<phb[0]; l++) {
      fits_read_col ( fits_infile, TDOUBLE, 4, j+l, 1, nch0, &nulv,  inspec,
		     &any, &error) ;
      for (i=0; i<nch0; i++)
	outspec[i] += inspec[i] ;
    }
    fits_write_col_dbl (fits_outfile, 4, k, 1, nch0, outspec, &error) ;
/*
 *  Errors -----------------------------------------------------
 */
    fits_read_col (fits_infile, TDOUBLE, 5, j, 1, nch0, &nulv, outspec,
		   &any, &error) ;
    for (l=1; l<phb[0]; l++) {
      fits_read_col ( fits_infile, TDOUBLE, 5, j+l, 1, nch0, &nulv,  inspec,
		     &any, &error) ;
      for (i=0; i<nch0; i++)
	outspec[i] = sqrt (outspec[i]*outspec[i] + inspec[i]*inspec[i]) ;
    }
    fits_write_col_dbl (fits_outfile, 5, k, 1, nch0, outspec, &error) ;
/*
 *  Baseline ---------------------------------------------------
 */
    fits_read_col (fits_infile, TDOUBLE, 6, j, 1, nch0, &nulv, outspec,
		   &any, &error) ;
    for (l=1; l<phb[0]; l++) {
      fits_read_col ( fits_infile, TDOUBLE, 6, j+l, 1, nch0, &nulv,  inspec,
		     &any, &error) ;
      for (i=0; i<nch0; i++)
	outspec[i] += inspec[i] ;
    }
    fits_write_col_dbl (fits_outfile, 6, k, 1, nch0, outspec, &error) ;
/*
 *  Exposure ---------------------------------------------------
 */
    fits_read_col (fits_infile, TDOUBLE, 7, j, 1,   1, &nulv, &expos0,
		   &any, &error) ;
    for (l=1; l<phb[0]; l++) {
      fits_read_col ( fits_infile, TDOUBLE, 7, j+l, 1,   1, &nulv,  &expos,
		     &any, &error) ;
      expos0 += expos ;
    }
    fits_write_col_dbl (fits_outfile, 7, k, 1,   1, &expos0, &error) ;

  j += phb[0] ;
  }  
/*
 *  Last fixes and close ------------------------------------------------
 */
  fits_write_date (fits_outfile, &error) ;
  fits_modify_key_str (fits_outfile, "CREATOR", fB2, "&", &error) ;
  fits_write_chksum (fits_outfile, &error) ;

  fits_close_file (fits_outfile, &error) ;
}
  
int Rd_Param2(char *infile, char *outfile, int *clobber, int *phb)
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

  Uclgst("outfile", outfile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get outfile parameter.");
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
  
  Uclgsi("binstep", &phb[0], &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get binstep parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsi("binstart", &phb[1], &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get binstart parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsi("binstop", &phb[2], &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get binstop parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  return parstat;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL effbeefsum
#endif
#ifdef unix
#define F77CALL effbeefsum_
#endif

void F77CALL() 
{ 
  void Fbfsum();

  Fbfsum(); 
}

