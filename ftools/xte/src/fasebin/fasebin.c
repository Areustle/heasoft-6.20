/* RCS: $Id: fasebin.c,v 1.8 2001/10/15 19:26:10 zpan Exp $ */
/*-----------------------------------------------------------------------
 *
 *  faseBin.c
 *
 *  Date: 19 March 1997
 *
 *  Arnold Rots, USRA
 *
 *  faseBin is a program applying barycenter corrections to XTE event
 *  observations, calculating the phase, binning in phase/pha space,
 *  and writing the result as a pha (fB) file.
 *
 *  12 Dec 2000: ftoolized yet again by M.Tripicco (EITI)
 *----------------------------------------------------------------------*/

#include "bary.h"
#define NPHASE  1000
#define NPHASE0 100

#include <stdio.h>
#include <string.h>
#include <errno.h> 
#include "xpi.h"
#include "fitsio.h"
#include "cftools.h"

char fbmsg[ERRMSG];
char *fB = "faseBin $Revision: 1.8 $" ;
void Fasebin()    
{
    
    fitsfile *fits_file;
    char *ttype[8] = {"SPEC_NUM", "PHASE", "CHANNEL", "COUNTS", "STAT_ERR",
		      "BASELINE", "EXPOSURE", "ROWID"} ;
    char *tunit[8] = {"", "", "", "count", "count", "count", "s", ""} ;
    char *tform[8] ;
    char telescope[80] ;
    char instrument[80] ;
    char datareffrm[80] ;
    char plephem[80] ;
    double dir[3] ;
    char progName[16] ;
    char orbitFile[1024] ;
    char *dataFile[1024] ;
    char gtiFile[1024] ;
    char respFile[1024] ;
    char outFile[1024] ;
    int nfiles = 0 ;
    char source[25] ;
    char line[256] ;
    char cpix[4096] ;
    char datobs[80] = "" ;
    int gainapp = 0 ;
    char *subline ;
    double *start ;
    double *stop ;
    double tstart = 1.0e20 ;
    double tstop = 0.0 ;
    double tstt, tstp ;
    double *exposure ;
    double *exposur2 ;
    double totexposure ;
    int nch = 0   ;
    int row = 0 ;
    int debug = 0 ;
    int nTR = 1 ;
    long nrows = 0 ;
    int anynul ;
    double nulval = 0.0 ;
    char filetype[32] ;
    int htype ;
    int accum = 0 ;
    int nevents = 0 ;
    double ra = -100.0 ;
    double dec = -100.0 ;
    double t1, t2 ;
    int binary = 0 ;
    double **image ;
    double *pimage ;
    long **img=0 ;
    int l1only = 1 ;
    int nobcorr = 0 ;
    
    long numbers[256] ;
    char rowid[24] ;
    char *rid = rowid ;
    int channel ;
    int nphase = NPHASE0 ;
    double dnphase ;
    double t ;
    int error = 0 ;
    double tZero = 0.0 ;
    long i, n, j, k ;
    int gotparms = 0 ;
    double radioph, roffset = 0.0 ;
    double f0 ;
    double x ;
    float v ;
    PsrTimPar *inptp=NULL ;
    PsrBinPar *inpbp=NULL ;
    int clobber = 1;
    char dataFparam[256];
    char dFp[256];
    FILE *fp;
    int iseof = 0;
    char tmp[256];
    int Rd_Param(char *, char *, char *,
		 double *, char *, char *, char *, int *, int *, int *, int *, int*);
    
c_ptaskn(fB);
/*
 *  Set defaults --------------------------------------------------------
 strcpy (outFile, "faseBin.pha") ;
 strcpy (respFile, "none") ;
 strcpy (gtiFile, "none") ; */
    strcpy (filetype, "none") ;
    strcpy (cpix, "none") ;
    strcpy (progName, "fasebin") ;
    
/*
 * Read parameter file ------------------------------------------------
 */
    
    if ( Rd_Param(orbitFile, dataFparam, source,
		  &roffset, gtiFile, respFile, outFile,
		  &nphase, &binary, &l1only, &debug, &clobber) )
	exit(1);
    
    if ( ( nphase < 1 ) || ( nphase > NPHASE ) )
	nphase = NPHASE0 ;
    if ( !strstr (outFile, ".") )
	strcat (outFile, ".pha") ;
    if ( debug ){
	sprintf (fbmsg, "%s: %s\n%s: cfitsio version %f",
		 progName, fB, progName, fits_get_version (&v) ) ;
	c_fcerr(fbmsg);
    }
    
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
 *  Read GTI table ------------------------------------------------------
 */
    nrows = 0 ;
    if ( strcmp (gtiFile, "none") ) {
	if ( fits_open_file (&fits_file, gtiFile, READONLY, &error) ) {
	    sprintf(fbmsg, "%s: cannot open file %s", progName, gtiFile) ;
	    c_fcerr(fbmsg);
	    exit (2) ;
	}
	while ( strcmp(filetype, "GTI") && !error ) {
	    fits_movrel_hdu (fits_file, 1, &htype, &error) ;
	    fits_read_key   (fits_file, TSTRING, "HDUCLAS1",  filetype, line, &error) ;
	    if ( debug ){
		sprintf (fbmsg, "%s: GTI file %s HDUCLAS1 %s",
			 progName, gtiFile, filetype) ;
		c_fcerr(fbmsg);
	    }
	}
	fits_read_key   (fits_file, TINT,    "NAXIS2",   &nrows,   line, &error) ;
    }
    if ( nrows && !error ) {
	fits_read_key   (fits_file, TDOUBLE, "TIMEZERO", &tZero,   line, &error) ;
	if ( error ) {
	    tZero = 0.0 ;
	    error = 0 ;
	}
	if ( debug ){
	    sprintf (fbmsg, "%s: GTI file %s nrows %d tZero %f",
		     progName, gtiFile, nrows, tZero) ;
	    c_fcerr(fbmsg);
	}
	start = (double*) malloc (nrows * sizeof(double)) ;
	stop = (double*) malloc (nrows * sizeof(double)) ;
	fits_read_col (fits_file, TDOUBLE, 1, 1, 1, nrows,
		       &nulval, start, &anynul, &error) ;
	fits_read_col (fits_file, TDOUBLE, 2, 1, 1, nrows,
		       &nulval, stop, &anynul, &error) ;
	for (i=0; i<nrows; i++) {
	    start[i] += tZero ;
	    stop[i] += tZero ;
	}
	nTR = nrows ;
    }
    else {
	start = (double*) malloc (sizeof(double)) ;
	stop = (double*) malloc (sizeof(double)) ;
	*start = -1.0e20 ;
	*stop = 1.0e20 ;
	nTR = 1 ;
    }
    if ( strcmp (gtiFile, "none") )
	fits_close_file (fits_file, &error) ;
    error = 0 ;
    if ( debug ){
	sprintf (fbmsg, "%s: finished GTI handling (%s)",
		 progName, gtiFile) ;
	c_fcerr(fbmsg);
    }
    
/*
 *  Open output file ----------------------------------------------------
 */
    if (clobber == 1) error = unlink(outFile);
    error = 0 ;
    if ( fits_create_file (&fits_file, outFile, &error) ) {
	sprintf(fbmsg, "%s: cannot create file %s", progName, outFile) ;
	c_fcerr(fbmsg);
	exit (3) ;
    }
    if ( debug ){
	sprintf (fbmsg, "%s: created output file (%s)",
		 progName, outFile) ;
	c_fcerr(fbmsg);
    }
    n = 0 ;
    fits_create_img (fits_file, 8, 0, &n, &error) ;
    fits_set_hdustruc (fits_file, &error) ;
    fits_write_chksum (fits_file, &error) ;
    
/*
 *  Do it ---------------------------------------------------------------
 */
  for (i=0; i<nfiles; i++) {
    tZero = 0.0 ;
    img = phaseHist (orbitFile, dataFile[i], telescope, instrument,
		     datareffrm, plephem,
		     source, &ra, &dec, roffset, &tZero,
		     binary, inptp, inpbp, l1only, nTR, start, stop,
		     &tstt, &tstp, &exposure, accum, &gainapp, datobs,
		     &nphase, &nch, cpix, &f0, nobcorr, &nevents, debug) ;
    if ( debug ){
	sprintf (fbmsg, "%s: execution %d of phaseHist",
		 progName, i) ;
	c_fcerr(fbmsg);
    }
    accum = 1 ;
    if ( tstart > tstt )
	tstart = tstt ;
    if ( tstop < tstp )
	tstop = tstp ;
  }
  dnphase = (double) 1.0 / nphase ;

/*
 *  Create the bintable -------------------------------------------------
 */
  nrows = 2*nphase+1 ;
  if ( debug ){
      sprintf (fbmsg, "%s: trying to allocate tforms, %d rows", progName, nrows) ;
      c_fcerr(fbmsg);
  }
  *tform = (char *) malloc (160*sizeof(char)) ;
  if ( debug ){
    sprintf (fbmsg, "%s: allocated tforms", progName) ;
    c_fcerr(fbmsg);
  }
  for (i=1; i<8; i++)
      tform[i] = tform[0] + 20*i ;
  if ( debug ){
      sprintf (fbmsg, "%s: start creating TFORMs, %d chns", progName, nch) ;
      c_fcerr(fbmsg);
  }
  strcpy (tform[0], "J") ;
  strcpy (tform[1], "D") ;
  sprintf (tform[2], "%dJ", nch) ;
  sprintf (tform[3], "%dD", nch) ;
  sprintf (tform[4], "%dD", nch) ;
  sprintf (tform[5], "%dD", nch) ;
  strcpy (tform[6], "D") ;
  strcpy (tform[7], "20A") ;
  if ( debug ){
      sprintf (fbmsg, "%s: start creating bintable, %d rows", progName, nrows) ;
      c_fcerr(fbmsg);
  }
  fits_create_tbl (fits_file, BINARY_TBL, nrows, 8,
		   ttype, tform, tunit, "SPECTRUM", &error) ;
  if ( debug ){
      sprintf (fbmsg, "%s: created bintable", progName) ;
      c_fcerr(fbmsg);
  }
  fits_write_comment (fits_file, "===== HDU configuration information", &error) ;
  fits_write_date (fits_file, &error) ;
  strcpy (line, fitsdate()) ;
  fits_update_key (fits_file, TSTRING,     "DATE",       line, "", &error) ;
  fits_write_key (fits_file, TSTRING,  "CREATOR",         fB, "", &error) ;
  fits_write_key (fits_file, TSTRING, "HDUCLASS",     "OGIP", "", &error) ;
  fits_write_key (fits_file, TSTRING, "HDUCLAS1", "SPECTRUM", "", &error) ;
  fits_write_key (fits_file, TSTRING,  "HDUVERS",    "1.1.0", "", &error) ;
  fits_write_key (fits_file, TSTRING, "HDUCLAS2", "ACCEPTED", "", &error) ;
  fits_write_key (fits_file, TSTRING, "HDUCLAS3",  "TYPE:II", "", &error) ;
  fits_write_key (fits_file, TSTRING, "HDUCLAS4",    "COUNT", "", &error) ;
  fits_write_key (fits_file, TSTRING, "HDUCLAS5",       "fB", "", &error) ;
  fits_write_key_longwarn (fits_file, &error) ;
  fits_write_comment (fits_file, "===== Observation - Object, coordinate, time information", &error) ;
  fits_write_key (fits_file, TSTRING,   "OBJECT",     source, "", &error) ;
  fits_write_key (fits_file, TSTRING, "TELESCOP",  telescope, "", &error) ;
  fits_write_key (fits_file, TSTRING, "INSTRUME", instrument, "", &error) ;
  fits_write_key (fits_file, TSTRING,   "FILTER",     "none", "", &error) ;
  fits_write_key_fixdbl (fits_file,     "RA-OBJ",      ra, 9, " \\  Used", &error) ;
  fits_write_key_fixdbl (fits_file,    "DEC-OBJ",     dec, 9, "  | for", &error) ;
  fits_write_key_fixdbl (fits_file,    "EQUINOX",  2000.0, 1, "  | barycentric", &error) ;
  fits_write_key (fits_file, TSTRING, "RADECSYS", datareffrm, " /  corrections", &error) ;
  fits_write_key (fits_file, TSTRING,  "PLEPHEM",    plephem, "Solar system ephem used for barycenter", &error) ;
  fits_write_key (fits_file, TSTRING,  "TIMESYS",      "TDB", " \\ Barycentric", &error) ;
  fits_write_key (fits_file, TSTRING,  "TIMEREF","SOLARSYSTEM", " / corrections applied", &error) ;
  fits_write_key (fits_file, TSTRING, "TIMEUNIT",        "d", "Unit of TSTART/TSTOP is days", &error) ;
  fits_write_key_fixdbl (fits_file,     "MJDREF",     0.0, 1, "Start and stop time are directly in MJD", &error) ;
  fits_write_key_fixdbl (fits_file,     "TSTART", tstart, 12, "Start of data in MJD", &error) ;
  fits_write_key_fixdbl (fits_file,      "TSTOP",  tstop, 12, "End of data in MJD", &error) ;
  fits_write_comment (fits_file, "Modified Julian Day (MJD) = JD - 2400000.5", &error) ;
  fits_write_key (fits_file, TSTRING, "DATE-OBS",     datobs, "", &error) ;
  fits_write_key_fixdbl (fits_file,   "PHASEDEL", dnphase, 5, "Phase bin size", &error) ;
  fits_write_comment (fits_file, "===== Spectral and calibration information", &error) ;
  fits_write_key_lng (fits_file,      "DETCHANS",        nch, "", &error) ;
  fits_write_key_lng (fits_file,        "TLMIN3",          0, "", &error) ;
  fits_write_key_lng (fits_file,        "TLMAX3",      nch-1, "", &error) ;
  fits_write_key_longstr (fits_file,     "CPIX4",       cpix, "", &error) ;
  fits_write_comment (fits_file, "CPIX4 contains the translation of the spectral binning", &error) ;
  fits_write_key (fits_file, TSTRING, "BACKFILE",     "none", "", &error) ;
  fits_write_key (fits_file, TSTRING, "CORRFILE",     "none", "", &error) ;
  fits_write_key (fits_file, TSTRING, "RESPFILE",   respFile, "", &error) ;
  fits_write_key (fits_file, TSTRING, "ANCRFILE",     "none", "", &error) ;
  fits_write_key (fits_file, TSTRING, "CHANTYPE",      "PHA", "", &error) ;
  fits_write_key_log (fits_file,      "POISSERR",          0, "", &error) ;
  fits_write_key_fixdbl (fits_file,   "AREASCAL",     1.0, 1, "", &error) ;
  fits_write_key_fixdbl (fits_file,   "BACKSCAL",     1.0, 1, "", &error) ;
  fits_write_key_fixdbl (fits_file,   "CORRSCAL",     0.0, 1, "", &error) ;
  fits_write_key_log (fits_file,       "GAINAPP",    gainapp, "", &error) ;
  fits_modify_comment (fits_file, "TTYPE1", "Sequence number of spectrum", &error) ;
  fits_modify_comment (fits_file, "TTYPE2", "Phase of spectrum", &error) ;
  fits_modify_comment (fits_file, "TTYPE3", "Channel list of spectrum", &error) ;
  fits_modify_comment (fits_file, "TTYPE4", "Spectrum", &error) ;
  fits_modify_comment (fits_file, "TTYPE5", "Statistical errors of spectral bins", &error) ;
  fits_modify_comment (fits_file, "TTYPE6", "Baseline (background) counts for spectral bins", &error) ;
  fits_modify_comment (fits_file, "TTYPE7", "Total exposure for spectrum", &error) ;
  fits_modify_comment (fits_file, "TTYPE8", "Row label", &error) ;
  if ( debug ){
      sprintf (fbmsg, "%s: completed header", progName) ;
      c_fcerr(fbmsg);
  }

/*
 *  Fill the "data" column ----------------------------------------------
 */
  image = (double **) malloc (nrows * sizeof(double*)) ;
  *image = (double *) malloc (nrows * nch * sizeof(double)) ;
  for (i=1; i<nrows; i++)
      image[i] = *image + i * nch ;
  for (j=0; j<nch; j++) {
      for (i=0; i<nphase; i++)
	  image[i][j] = image[i+nphase][j] = (double) img[j][i] ;
      image[2*nphase][j] = (double) img[j][0] ;
  }
  exposur2 = (double *) malloc (nrows * sizeof(double)) ;
  for (i=0; i<nphase; i++)
      exposur2[i] = exposur2[i+nphase] = exposure[i] ;
  exposur2[2*nphase] = exposur2[0] ;
  if ( debug ){
    sprintf (fbmsg, "%s: filled data", progName) ;
    c_fcerr(fbmsg);
  }

/*
 *  Write to output file ------------------------------------------------
 */
  fits_write_col_dbl (fits_file, 4, 1, 1, nrows*nch,   *image, &error) ;
  if ( debug ){
      sprintf (fbmsg, "%s: wrote counts", progName) ;
      c_fcerr(fbmsg);
  }
  for (i=0, pimage=*image; i<nrows*nch; i++, pimage++)
      *pimage = (*pimage > 0.0 ) ? sqrt(*pimage) : 1.0 ;
  fits_write_col_dbl (fits_file, 5, 1, 1, nrows*nch,   *image, &error) ;
  if ( debug ){
      sprintf (fbmsg, "%s: wrote errors", progName) ;
      c_fcerr(fbmsg);
  }
  for (i=0, pimage=*image; i<nrows*nch; i++, pimage++)
      *pimage = 0.0 ;
  fits_write_col_dbl (fits_file, 6, 1, 1, nrows*nch,   *image, &error) ;
  fits_write_col_dbl (fits_file, 7, 1, 1,     nrows, exposur2, &error) ;
  if ( debug ){
      sprintf (fbmsg, "%s: wrote zero and exp", progName) ;
      c_fcerr(fbmsg);
  }

  for (i=0; i<256; i++)
      numbers[i] = i ;
  for (i=0; i<nrows; i++) {
      x = (double) i*dnphase ;
      sprintf (rowid, "Phase %5.3g", x) ;
      fits_write_col_lng (fits_file, 1, i+1, 1,   1,      &i, &error) ;
      fits_write_col_dbl (fits_file, 2, i+1, 1,   1,      &x, &error) ;
      fits_write_col_lng (fits_file, 3, i+1, 1, nch, numbers, &error) ;
      fits_write_col_str (fits_file, 8, i+1, 1,   1,    &rid, &error) ;
  }
  if ( debug ){
      sprintf (fbmsg, "%s: wrote scalars", progName) ;
      c_fcerr(fbmsg);
  }

/*
 *  Close up and exit ---------------------------------------------------
 */
  fits_write_chksum (fits_file, &error) ;
  fits_close_file (fits_file, &error) ;
  totexposure = 0.0 ;
  for (i=0; i<nphase; i++)
      totexposure += exposure[i] ;
  sprintf (fbmsg,"# Input file(s):") ;
  c_fcecho(fbmsg);
  for (i=0; i<nfiles; i++){
      sprintf (fbmsg,"#   %s", dataFile[i]) ;
      c_fcecho(fbmsg);
  }
  sprintf (fbmsg,"# Accumulated %d events during an exposure time of %f s",
	   nevents, totexposure) ;
  c_fcecho(fbmsg);
  sprintf (fbmsg,"# Total clock correction: %f s", tZero) ;
  c_fcecho(fbmsg);

  free (start) ;
  free (stop) ;
  free (exposur2) ;
  free (*tform) ;
  free (*image) ;
  free (image) ;
  for (i=0; i<nfiles; i++)
      free (dataFile[i]) ;
}

int Rd_Param(char *orbitFile, char *dataFparam, char *source,
	     double *roffset,char *gtiFile, char *respFile, char *outFile,
	     int *nphase, int *binary, int *l1only, int *debug, int *clobber)
{
  int BufLen_2 = 255;
  int parstat = 0;
  char text[FLEN_ERRMSG];
  int propane;

  Uclgsb("debug", debug, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get debug parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  if (*debug != 0) *debug=1;
  
  Uclgsb("binary", binary, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get binary parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  if (*binary != 0) *binary=1;
  
  Uclgsd("roff",roffset, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get roff parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("orbitfile",orbitFile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get orbit file parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("outfile",outFile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get outfile parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("respfile",respFile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get respfile parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("gtifile",gtiFile, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get gtifile parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  BufLen_2 = 24;
  Uclgst("sourcename",source, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get sourcename parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  BufLen_2 = 255;

  Uclgsi("nph",nphase, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get nph parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsb("l1only",l1only, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get l1only parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  if (*l1only != 0) *l1only=1;
  Uclgsb("propane",&propane, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get propane parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  if (propane != 0) *l1only=-1;

  Uclgsb("clobber",clobber, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get clobber parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  if (*clobber != 0) *clobber=1;
  
  Uclgst("datafile",dataFparam, &parstat);
  if(parstat != 0){
    c_fcecho(" ");
    c_fcecho("Could not get datafile parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  return parstat;
}
