
#include <stdlib.h>
#include <stdio.h>

#include "fitsio.h"
#define BITPIX 8
#define MAXARRAY 100
#define MAXSTRING 20
#define CONTXTLEN 120
#ifndef  max
#define max(a, b)       ((a) < (b) ? (b) : (a))
#endif  /* max */

void wftbmd(char *outfil, char *infile, char *modlnm, char *modunt,
	    int nintpm, int naddpm, int qrdshf, int qaddtv, char *addnam[],
	    float *addivl, char *intnam[], float *intivl, int *intntb,
	    long *intmth, int maxtab, float *inttab, int nenerg,
	    float *energy, fitsfile **fptr, int *ierr, int *status,
	    char **contxt) {
  /*
    INTEGER nintpm, naddpm, maxtab, nenerg
    INTEGER ounit, ierr, status
    INTEGER intntb(nintpm), intmth(nintpm)
    REAL addivl(6, naddpm), intivl(6, nintpm)
    REAL inttab(maxtab, nintpm), energy(nenerg)
    CHARACTER*(*) outfil, infile, modlnm, modunt, contxt
    CHARACTER*(*) addnam(naddpm), intnam(nintpm)
    LOGICAL qrdshf, qaddtv
    
c Subroutine to open the FITS file and write out all the header information
c down to the point of actually writing the model spectra. At this point
c returns so that the main program can read and write one model spectrum at
c a time.

c Arguments :
c    outfil      c*(*)  i: FITS filename
c    infile      c*(*)  i: Name of original file
c    modlnm      c*(*)  i: The name of the model
c    modunt      c*(*)  i: The model units
c    nintpm      i      i: The number of interpolation parameters
c    naddpm      i      i: The number of additional parameters
c    qrdshf      l      i: Redshift flag
c    qaddtv      l      i: If true this is an additive table model
c    addnam      c*(*)  i: Names of the additional parameters
c    addivl      r      i: Initial values of the additional parameters
c    intnam      c*(*)  i: Names of the interpolation parameters
c    intivl      r      i: Initial values of the interpolation parameters
c    intntb      i      i: Number of tabulated values for interpolation params.
c    intmth      i      i: Interpolation method for interpolation parameters
c    maxtab      i      i: The size of the first dimension in inttab
c    inttab      r      i: The tabulated parameter values.
c    nenerg      i      i: Number of energies (one more than spectral bins)
c    energy      r      i: Energies
c    fptr (ounit)i      o: The I/O unit in use.  (fits file ptr)
c    ierr        i      o: error
c                          1 = failed to open FITS file
c                          2 = failed to write primary header
c                          3 = failed to write parameter extension
c                          4 = failed to write energy extension
c                          5 = failed to create model spectra extension
c    status      i      o: FITSIO status
c    contxt      c*(*)  o: Error diagnostic string

c  HDUVERS1 1.0.0
*/


  int i, nvals, tfields, nrows;

  /* 100 elements of 20 char strings */
  char *ttype[MAXARRAY], *tunit[MAXARRAY], *tform[MAXARRAY];
  
  *status = 0;
  *ierr = 0;

  for (i=0;i<MAXARRAY;i++) {
    ttype[i] = (char *) malloc(MAXSTRING*sizeof(char));
    tunit[i] = (char *) malloc(MAXSTRING*sizeof(char));
    tform[i] = (char *) malloc(MAXSTRING*sizeof(char));
  }
  *contxt = malloc(CONTXTLEN*sizeof(char));

  /*
    c ------------------------------------------------------------
    c Open the output FITS file.
    c ------------------------------------------------------------
    */
  fits_create_file(fptr, outfil, status);
  if (*status != 0) {
    *ierr = 1;
    strcpy(*contxt,"Failed to open output file");
    return;
  }
  /*
    c ------------------------------------------------------------
    c Write the primary header
    c ------------------------------------------------------------
    */
  fits_write_imghdr(*fptr, BITPIX, 0, 0, status);
  if (*status != 0) {
    strcpy(*contxt,"Failed to define primary header");
    return;
  }

  /*
    c Write out the additional keywords about the creation of the
    c FITS file.
    */
      
  
  fits_write_key(*fptr, TSTRING, "CONTENT", "MODEL",
		 "spectrum file contains time intervals and event", status);
  if (*status != 0) {
    printf("Warning : Failed to write CONTENT keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  fits_write_key(*fptr, TSTRING, "FILENAME", infile,
		 "File that FITS was produced from", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write FILENAME keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  fits_write_key(*fptr, TSTRING, "ORIGIN", "NASA/GSFC",
		 "origin of FITS file", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write ORIGIN keyword\n");
    printf("          FITSIO error %d", *status);
   *status = 0;
  }

  /* c Write the model name and units */

  fits_write_key(*fptr, TSTRING, "MODLNAME", modlnm,"Model name",status);
  if (*status != 0 ) {
    printf("Warning : Failed to write MODLNAME keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  fits_write_key(*fptr, TSTRING, "MODLUNIT", modunt,"Model units",status);
  if (*status != 0 ) {
    printf("Warning : Failed to write MODLUNIT keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  /* c Write the redshift flag */

  fits_write_key(*fptr, TLOGICAL,"REDSHIFT", &qrdshf, 
		 "If true then redshift will be included as a parameter",
		  status);
  if (*status != 0 ) { 
    printf("Warning : Failed to write REDSHIFT keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  /* c Write the additive table model flag */

  fits_write_key(*fptr,TLOGICAL, "ADDMODEL", &qaddtv,
		 "If true then this is an additive table model", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write ADDMODEL keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  /* c Write the OGIP keywords */

  fits_write_key(*fptr, TSTRING, "HDUCLASS", "OGIP",
                 "format conforms to OGIP standard", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLASS keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  fits_write_key(*fptr, TSTRING, "HDUCLAS1", "XSPEC TABLE MODEL",
		 "model spectra for XSPEC", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLAS1 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  fits_write_key(*fptr, TSTRING, "HDUVERS1", "1.0.0",
		 "version of format", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUVERS1 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

  if (*status != 0) { *ierr = 2; return; }

  /*
    c ------------------------------------------------------------------------
    c Write the data extension containing the parameter definitions
    c ------------------------------------------------------------------------
    */
  /* c Create the extension */

  fits_create_hdu(*fptr, status);
  strcpy(*contxt,"Failed to create extension");
  if ( *status != 0 ) { *ierr = 3; return; }

  /* c Set the header keywords for a binary extension */

  nvals = 0;
  for(i=0; i<nintpm; i++) {
    nvals = max(nvals, intntb[i]);
  }

  tfields = 10;
  sprintf(ttype[0],"NAME");
  sprintf(tform[0],"12A");
  sprintf(tunit[0]," ");
  sprintf(ttype[1],"METHOD");
  sprintf(tform[1],"J");
  sprintf(tunit[1]," ");
  sprintf(ttype[2],"INITIAL");
  sprintf(tform[2],"E");
  sprintf(tunit[2]," ");
  sprintf(ttype[3],"DELTA");
  sprintf(tform[3],"E");
  sprintf(tunit[3]," ");
  sprintf(ttype[4],"MINIMUM");
  sprintf(tform[4],"E");
  sprintf(tunit[4]," ");
  sprintf(ttype[5],"BOTTOM");
  sprintf(tform[5],"E");
  sprintf(tunit[5]," ");
  sprintf(ttype[6],"TOP");
  sprintf(tform[6],"E");
  sprintf(tunit[6]," ");
  sprintf(ttype[7],"MAXIMUM");
  sprintf(tform[7],"E");
  sprintf(tunit[7]," ");
  sprintf(ttype[8],"NUMBVALS");
  sprintf(tform[8],"J");
  sprintf(tunit[8]," ");
  sprintf(ttype[9],"VALUE");
  sprintf(tform[9],"%5d%c", nvals, 'E');
  sprintf(tunit[9]," ");
  
  /* c Write the main header keywords. */

  fits_write_btblhdr(*fptr, (nintpm+naddpm), tfields, ttype, tform, tunit,
		     "PARAMETERS", 0, status);
  if ( *status != 0 ) { 
    strcpy(*contxt,"Failed to write main header keywords for extension");
    *ierr = 3; 
    return; 
  }
  	 
  /* c Write the OGIP keywords */

  fits_write_key(*fptr, TSTRING, "HDUCLASS", "OGIP",
		 "format conforms to OGIP standard", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLASS keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  fits_write_key(*fptr, TSTRING, "HDUCLAS1", "XSPEC TABLE MODEL",
		 "model spectra for XSPEC", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLAS1 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  fits_write_key(*fptr, TSTRING, "HDUCLAS2", "PARAMETERS",
		 "extension containing parameter info", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLAS2 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  fits_write_key(*fptr, TSTRING, "HDUVERS1", "1.0.0",
		 "version of format", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUVERS1 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  /* c Write the number of interpolated and additional parameters */
  
  fits_write_key(*fptr,TLONG, "NINTPARM", &nintpm, 
		 "Number of interpolation parameters", status);
  strcpy(*contxt,"Failed to write NINTPARM keyword");
  if ( *status != 0 ) { *ierr = 3; return; }
  
  fits_write_key(*fptr, TLONG, "NADDPARM", &naddpm, 
		 "Number of additional parameters", status);
  strcpy(*contxt,"Failed to write NADDPARM keyword");
  if ( *status != 0 ) { *ierr = 3; return; }
  
  /* c define the binary table */
  
  /* CALL ftbdef(ounit, tfields, tform, 0, (nintpm+naddpm), status); 
     strcpy(*contxt,"Failed to define binary table");
     if ( *status != 0 ) { *ierr = 3; return; }
     */

  /* c loop round the interpolation parameters */

  for (i=0; i<nintpm; i++) {

    /* c write the parameter name */

    fits_write_col(*fptr, TSTRING, 1, i+1, 1, 1, &(intnam[i]), status);
    strcpy(*contxt,"Failed to write NAME column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the parameter method */
    
    fits_write_col(*fptr, TLONG, 2, i+1, 1, 1, &(intmth[i]), status);
    strcpy(*contxt,"Failed to write METHOD column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the initial value */
    
    fits_write_col(*fptr, TFLOAT, 3, i+1, 1, 1, &(intivl[6*i+0]), status);
    strcpy(*contxt,"Failed to write INITIAL column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the delta value */
    
    fits_write_col(*fptr, TFLOAT, 4, i+1, 1, 1, &(intivl[6*i+1]), status);
    strcpy(*contxt,"Failed to write DELTA column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the minimum value */
    
    fits_write_col(*fptr, TFLOAT, 5, i+1, 1, 1, &(intivl[6*i+2]), status);
    strcpy(*contxt,"Failed to write MINIMUM column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the bottom value */
    
    fits_write_col(*fptr, TFLOAT, 6, i+1, 1, 1, &(intivl[6*i+3]), status);
    strcpy(*contxt,"Failed to write BOTTOM column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the top value */
    
    fits_write_col(*fptr, TFLOAT, 7, i+1, 1, 1, &(intivl[6*i+4]), status);
    strcpy(*contxt,"Failed to write TOP column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the maximum value */
    
    fits_write_col(*fptr, TFLOAT, 8, i+1, 1, 1, &(intivl[6*i+5]), status);
    strcpy(*contxt,"Failed to write MAXIMUM column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the number of tabulated parameter values */
    
    fits_write_col(*fptr, TLONG, 9, i+1, 1, 1, &(intntb[i]), status);
    strcpy(*contxt,"Failed to write NUMBVALS column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the tabulated parameter values */
    
    fits_write_col(*fptr, TFLOAT,10, i+1, 1, intntb[i], &(inttab[maxtab*i+0]),
		   status);
    strcpy(*contxt,"Failed to write VALUE column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
  }
  
  /* c loop round the additional parameters */
  
  for(i=0;i<naddpm;i++) {
    
    /* c write the parameter name */
    
    fits_write_col(*fptr, TSTRING, 1, i+1+nintpm, 1, 1,&(addnam[i]),status);
    strcpy(*contxt,"Failed to write NAME column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the initial value */
    
    fits_write_col(*fptr, TFLOAT, 3, i+1+nintpm, 1, 1,&(addivl[6*i+0]),status);
    strcpy(*contxt,"Failed to write INITIAL column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the delta value */
    
    fits_write_col(*fptr, TFLOAT, 4, i+1+nintpm, 1, 1,&(addivl[6*i+1]),status);
    strcpy(*contxt,"Failed to write DELTA column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the minimum value */
    
    fits_write_col(*fptr, TFLOAT, 5, i+1+nintpm, 1, 1,&(addivl[6*i+2]),status);
    strcpy(*contxt,"Failed to write MINIMUM column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the bottom value */
    
    fits_write_col(*fptr, TFLOAT, 6, i+1+nintpm, 1, 1,&(addivl[6*i+3]),status);
    strcpy(*contxt,"Failed to write BOTTOM column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the top value */
    
    fits_write_col(*fptr, TFLOAT, 7, i+1+nintpm, 1, 1,&(addivl[6*i+4]),status);
    strcpy(*contxt,"Failed to write TOP column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
    /* c write the maximum value */
    
    fits_write_col(*fptr, TFLOAT, 8, i+1+nintpm, 1, 1,&(addivl[6*i+5]),status);
    strcpy(*contxt,"Failed to write MAXIMUM column");
    if ( *status != 0 ) { *ierr = 3; return; }
    
  }

  /* Flush this info to the file */
  fits_flush_file(*fptr, status);
  strcpy(*contxt,"Failed to flush datafile");
  if (*status != 0) { *ierr = 3; return; }
  
  /*
    c ---------------------------------------------------------------
    c Write the data extension containing the energies
    c ---------------------------------------------------------------
    */
  
  /* c Create the extension */
  
  fits_create_hdu(*fptr, status);
  strcpy(*contxt,"Failed to create extension");
  if (*status != 0 ) { *ierr = 4; return; }
  
  /* c Set the header keywords for a binary extension */
  
  tfields = 2;
  sprintf(ttype[0],"ENERG_LO");
  sprintf(tform[0],"E");
  sprintf(tunit[0]," ");
  sprintf(ttype[1],"ENERG_HI");
  sprintf(tform[1],"E");
  sprintf(tunit[1]," ");
  
  /* c Write the main header keywords. */

  fits_write_btblhdr(*fptr, nenerg-1, tfields, ttype, tform, tunit,
		     "ENERGIES", 0, status);
  strcpy(*contxt,"Failed to write main header keywords for extension");
  if (*status != 0 ) { *ierr = 4; return; }
  
  fits_write_key(*fptr, TSTRING, "HDUCLAS1", "XSPEC TABLE MODEL",
		 "model spectra for XSPEC", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLAS1 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  fits_write_key(*fptr, TSTRING, "HDUCLAS2", "ENERGIES",
		 "extension containing energy bin info", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLAS2 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  fits_write_key(*fptr, TSTRING, "HDUVERS1", "1.0.0",
		 "version of format", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUVERS1 keyword");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }
  
  /* c define the binary table */
  
  /* 
     CALL ftbdef(ounit, tfields, tform, 0, nenerg, status)
     contxt = "Failed to define binary table"
     IF ( status .NE. 0 ) GOTO 400
     */
  
  /* c write the energies into the table */
  
  fits_write_col(*fptr, TFLOAT, 1, 1, 1, nenerg-1, &(energy[0]), status);
  strcpy(*contxt,"Failed to write ENERG_LO column");
  if (*status != 0 ) { *ierr = 4; return; }
  
  fits_write_col(*fptr, TFLOAT, 2, 1, 1, nenerg-1, &(energy[1]), status);
  strcpy(*contxt,"Failed to write ENERG_HI column");
  if (*status != 0 ) { *ierr = 4; return; }
  
  /*
    c ---------------------------------------------------------------
    c Now write out all the model spectra
    c ---------------------------------------------------------------
    */
  /* c Create the extension */
  
  fits_create_hdu(*fptr, status);
  strcpy(*contxt,"Failed to create extension");
  if (*status != 0 ) { *ierr = 5; return; }
  
  /* c Set the header keywords for a binary extension */
  
  tfields = 2 + naddpm;
  sprintf(ttype[0],"PARAMVAL");
  sprintf(tform[0],"%5d%1s",nintpm, "E");
  sprintf(tunit[0]," ");
  sprintf(ttype[1],"INTPSPEC");
  sprintf(tform[1],"%5d%1s",nenerg-1, "E");
  sprintf(tunit[1],"%s",modunt);
  for (i=0; i<naddpm; i++) {
    sprintf(ttype[i+2],"ADDSP");
    sprintf(ttype[i+2],"     %3d", i+1);
    sprintf(tform[i+2],"%5d%1s", nenerg-1, "E");
    sprintf(tunit[i+2],"%s",modunt);
  }
  
  nrows = 1;
  for(i=0;i<nintpm;i++) {
    nrows = nrows * intntb[i];
  }
  
  /* c Write the main header keywords. */
  
  fits_write_btblhdr(*fptr, nrows, tfields, ttype, tform, tunit,
		     "SPECTRA", 0, status);
  strcpy(*contxt,"Failed to write main header keywords for extension");
  if (*status != 0 ) { *ierr = 5; return; }
  
  fits_write_key(*fptr, TSTRING, "HDUCLAS1", "XSPEC TABLE MODEL",
		 "model spectra for XSPEC", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLAS1 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }      
  
  fits_write_key(*fptr, TSTRING, "HDUCLAS2", "MODEL SPECTRA",
		 "extension containing model spectra", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUCLAS2 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }      
  
  fits_write_key(*fptr, TSTRING, "HDUVERS1", "1.0.0",
		   "version of format", status);
  if (*status != 0 ) {
    printf("Warning : Failed to write HDUVERS1 keyword\n");
    printf("          FITSIO error %d", *status);
    *status = 0;
  }

}

