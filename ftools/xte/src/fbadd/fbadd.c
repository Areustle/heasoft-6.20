/* RCS: $Id: fbadd.c,v 1.6 2000/03/30 14:40:03 miket Exp $ */
/*-----------------------------------------------------------------------
 *
 *  fBadd.c
 *
 *  Date: 20 March 1997
 *
 *  Arnold Rots, USRA
 *
 *  fBadd allows adding several fB files together.  Errors are added in
 *  quadrature.
 *
 *  07 October 1998: reFTOOLized by M.Tripicco
 *     (fB, Rd_Param have "1" appended for uniqueness again)
 *
 *  30 March 2000: moved fclose() to avoid closing null file pointer
 *                 (see also fbfsum, fbsub)
 *
 *----------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fitsio.h>
#include <longnam.h>
#define NCHAN   4096

#include <string.h>
#include <errno.h> 
#include "fitsio.h"
#include "xpi.h"

char *fB1 = "fBadd $Revision: 1.6 $" ;
void Fbadd()
{

  fitsfile *fits_infile;
  fitsfile *fits_outfile;
  char line[1024] ;
  char progName[256] ;
  char *dataFile[1024] ;
  char outFile[1025] ;
  char keystrval[80] ;
  char comment[80] ;
  int nfiles = 0 ;
  int error = 0 ;
  int htype ;
  long nrows, nch, i, j, n1, n2 ;
  long nrows0, nch0, k ;
  double nulv = 0.0 ;
  int any ;
  double dnphase, expos ;
  double dnphase0, expos0 ;
  double phase ;
  double tstart, tstop, t1, t2 ;
  double inspec[NCHAN] ;
  double outspec[NCHAN] ;

  int clobber = 0;
  int n;
  FILE *fp;
  char dataFparam[256];
  char dFp[256];
  int iseof = 0;
  char tmp[256];
  int Rd_Param1(char *, char *, int *) ;
  
  /* Read Parameter File */

  strcpy(progName, "fbadd");
  if ( Rd_Param1(dataFparam, outFile, &clobber) )
    exit (1) ;

  if ( !strncmp(dataFparam,"@",1) ){
    sscanf(dataFparam,"@%s",dFp);
    if ((fp=fopen(dFp,"r")) == NULL){
      c_fcecho(" ");
      c_fcecho("Could not open datafile list");
      exit(1);
    }
    while(iseof == 0){
      if (fgets(tmp, 255, fp) == NULL) {
	iseof=1;
      }
      if (iseof == 0){
	dataFile[nfiles] = (char*) malloc(1025 * sizeof(char)) ;
	strncpy(dataFile[nfiles], tmp, strlen(tmp)-1);
	*(dataFile[nfiles]+strlen(tmp)-1)='\0';
	nfiles++;
      }
      iseof=feof(fp);
    } 
  } else {
    dataFile[0] = (char*) malloc(1025 * sizeof(char)) ;
    strcpy(dataFile[0], dataFparam);
    nfiles=1;
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

  sprintf (line, "cp %s %s", dataFile[0], outFile) ;
  if ( system (line) ) {
    fprintf (stderr,"%s: Could not open input file %s or create output file %s\n",
	     progName, dataFile[0], outFile) ;
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
	     progName, dataFile[0]) ;
    fits_delete_file (fits_outfile, &error) ;
    exit (3) ;
  }
  fits_read_key_lng (fits_outfile,        "NAXIS2",    &nrows0, comment, &error) ;
  fits_read_key_dbl (fits_outfile,      "PHASEDEL",  &dnphase0, comment, &error) ;
  fits_read_key_dbl (fits_outfile,        "TSTART",    &tstart, comment, &error) ;
  fits_read_key_dbl (fits_outfile,         "TSTOP",     &tstop, comment, &error) ;
  fits_read_key_lng (fits_outfile,      "DETCHANS",      &nch0, comment, &error) ;

/*
 *  Input file loop -----------------------------------------------------
 */
  for (k=1; k<nfiles; k++) {
    if ( fits_open_file (&fits_infile, dataFile[k], READONLY, &error) ) {
      fprintf (stderr,"%s: Failed to open input file %s\n",
	       progName, dataFile[k]) ;
      continue ;
    }

/*
 *  Check first extension -----------------------------------------------
 */
    fits_movabs_hdu (fits_infile, 2, &htype, &error) ;
    fits_read_key (fits_infile, TSTRING, "HDUCLAS5", keystrval, comment, &error) ;
    if ( strcmp (keystrval, "fB") ) {
      fprintf (stderr,"%s: Input file %s is of the wrong type\n",
	       progName, dataFile[k]) ;
      continue ;
    }
    fits_read_key_lng (fits_infile,        "NAXIS2",    &nrows, comment, &error) ;
    fits_read_key_dbl (fits_infile,      "PHASEDEL",  &dnphase, comment, &error) ;
    fits_read_key_dbl (fits_outfile,       "TSTART",       &t1, comment, &error) ;
    fits_read_key_dbl (fits_outfile,        "TSTOP",       &t2, comment, &error) ;
    fits_read_key_lng (fits_infile,      "DETCHANS",      &nch, comment, &error) ;

    if ( ( nrows != nrows0 ) || ( nch != nch0 ) || ( dnphase != dnphase0 ) ) {
      fprintf (stderr,"%s: Input file %s is immensurate with previous file(s)\n",
	       progName, dataFile[k]) ;
      continue ;
    }
    if ( t1 < tstart )
      tstart = t1 ;
    if (t2 > tstop )
      tstop = t2 ;
  
/*
 *  Enter new values -----------------------------------------------------
 */
    for (j=1; j<=nrows; j++) {
/*
 *  Counts -----------------------------------------------------
 */
      fits_read_col ( fits_infile, TDOUBLE, 4, j, 1, nch, &nulv,  inspec,
		     &any, &error) ;
      fits_read_col (fits_outfile, TDOUBLE, 4, j, 1, nch, &nulv, outspec,
		     &any, &error) ;
      for (i=0; i<nch; i++)
	outspec[i] += inspec[i] ;
      fits_write_col_dbl (fits_outfile, 4, j, 1, nch, outspec, &error) ;
/*
 *  Errors -----------------------------------------------------
 */
      fits_read_col ( fits_infile, TDOUBLE, 5, j, 1, nch, &nulv,  inspec,
		     &any, &error) ;
      fits_read_col (fits_outfile, TDOUBLE, 5, j, 1, nch, &nulv, outspec,
		     &any, &error) ;
      for (i=0; i<nch; i++)
	outspec[i] = sqrt (outspec[i]*outspec[i] + inspec[i]*inspec[i]) ;
      fits_write_col_dbl (fits_outfile, 5, j, 1, nch, outspec, &error) ;
/*
 *  Baseline ---------------------------------------------------
 */
      fits_read_col ( fits_infile, TDOUBLE, 6, j, 1, nch, &nulv,  inspec,
		     &any, &error) ;
      fits_read_col (fits_outfile, TDOUBLE, 6, j, 1, nch, &nulv, outspec,
		     &any, &error) ;
      for (i=0; i<nch; i++)
	outspec[i] += inspec[i] ;
      fits_write_col_dbl (fits_outfile, 6, j, 1, nch, outspec, &error) ;
/*
 *  Exposure ---------------------------------------------------
 */
      fits_read_col ( fits_infile, TDOUBLE, 7, j, 1,   1, &nulv,  &expos,
		     &any, &error) ;
      fits_read_col (fits_outfile, TDOUBLE, 7, j, 1,   1, &nulv, &expos0,
		     &any, &error) ;
      expos0 += expos ;
      fits_write_col_dbl (fits_outfile, 7, j, 1,   1, &expos0, &error) ;
    }
    fits_close_file (fits_infile, &error) ;
  }  
/*
 *  Last fixes and close ------------------------------------------------
 */
  fits_write_date (fits_outfile, &error) ;
  fits_modify_key_str (fits_outfile, "CREATOR",    fB1,     "&", &error) ;
  fits_modify_key_dbl (fits_outfile, "TSTART", tstart, 12, "&", &error) ;
  fits_modify_key_dbl (fits_outfile,  "TSTOP",  tstop, 12, "&", &error) ;
  fits_write_chksum (fits_outfile, &error) ;

  fits_close_file (fits_outfile, &error) ;
}

int Rd_Param1(char *infile, char *outfile, int *clobber)
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

  return parstat;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL effbeeadd
#endif
#ifdef unix
#define F77CALL effbeeadd_
#endif

void F77CALL() 
{ 
  void Fbadd();

  Fbadd(); 
}
