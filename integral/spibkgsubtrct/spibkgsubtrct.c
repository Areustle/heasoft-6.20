#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/*
#include "cfitsio.h"
#include "cftools.h"
#include "pfile.h"
*/
#include "fitsio.h"
#include "spibkg_common.h"

#include "pil.h"
#include "headas.h"
#include "headas_error.h"

#define TOOLSUB SPIbkgSubtrct
#include "headas_main.c"

#define  Nparams_Lo   11
#define  Nparams_Med  26
#define  Nparams_Hi   51

#define  maxSpecs     16384

/* ------------------------------------------------------------------------------

  main program - calls fitsio routines and XSPEC models spibkgLo, spibkgMed 
                 and spibkgHi

  This program reads the output of the XSPEC (tcl) procedure SavBkgMdl,
  which lists the SPI background model (i.e. spibkg_lo, spibkg_med or
  spibkg_hi) for sequentially for each spectrum analyzed. The relevant
  PHA-II data file (or pertinant subset thereof) is read. The best-fit
  background rates are then computed through a call to the actual XSPEC 
  models, and then subtracted fromthe data (as counts or rates, depending 
  on the input data file. A new PHA-II data file is then output, containing
  the background subtracted data. Note that if some row-selection cut is made 
  in the analysis, e.g. if rows 181-360 are used out of 1-504 total rows, 
  the rows 1-180 and 361-504 will cntain the origginal (i.e. 
  source+background) data, with only 181-360 being background subtracted.
  Thus, if a row-selection cut is made at an earely stage of the analysis
  it should be used throughout.

  inputs:        inpPHAfil          input (FITS) PHA-II spectral data file
                 bkgMdlParams       (ascii) file produced by XSPEC SavBkgMdl 
		                    procedure which contains the sectrum-row 
				    number, plus associated backound model 
				    parameters
   	         bkgRowNum          row number of the "SPI.-RSP.-DBS" table
		                    extension of the PHA-II input file which
				    references the background response 
				    matrices. If this is not specified, the
				    user must manually remove this row from
				    the table.

                 outpPHAfil         PHA-II output file to be used for 
		                    subsequent XSPEC analysis note that that
				    the 
    



           C.R. Shrader  Code 661 NASA/GSFC    07/2004

 ------------------------------------------------------------------------------ */

 int SPIbkgSubtrct(void)
{

/* number of parameters for spibkg_lo, med & hi */
  /*
  const int Nparams_Lo   = 11;   
  const int Nparams_Med  = 21;
  const int Nparams_Hi   = 51;

*/

  int    ii=0, kk=0, jj=0, rowID[maxSpecs], row1=0, rowN=0, Nparams=0;
  int    nrows=0, nch=0, status=0, Ne=0, bkgRowNum=0;
  char   inPHAfil[100], BkgMdlParams[100], outpPHAfil[100], SPIbkgInitFil[100]; 
  float  *specdat=0, *expos=0, *Elo=0, *Ehi=0, *paramArr=0;
  static char tmpstr[1000];

  double *photar=0, *photer=0, *E=0, *params=0;

  FILE   *MdlParams;

  /*    c_ptaskn("SPIbkgSubtrct 1.01");  */

    static char taskname[80] = "SPIbkgSubtrct";
    static char version[8] = "1.01";

    /* Register taskname and version. If not set
       explicitly a default value (= executable name)
       is used. Retrieve these via get_toolname(),
       get_toolversion(), and/or get_toolnamev() */

    set_toolname(taskname);
    set_toolversion(version);


/* read in parameter file data: arf(s), spectral data, spiback data
       weighting factors and ouput file name */

   ii = get_params( inPHAfil, BkgMdlParams, outpPHAfil, SPIbkgInitFil, &bkgRowNum);


    if (ii <0 ) {
      printf("**** error reading parameter file *** \n");
      return(status);
    }

    strcpy (tmpstr,"SPIBKG_INP_DAT");    /* create "keyword = value" string */
    strcat (tmpstr,"=");
    strcat (tmpstr,SPIbkgInitFil);
    putenv(tmpstr);                      /* create the environment variable */


/*  read in information from XSPEC session: model ID (==> num paramaters),
    row number, paramaters for that row. These are in the ascii file, by default
    named SPIbkgMdl.params

*/
    MdlParams = fopen(BkgMdlParams,"r");

    fscanf(MdlParams, "%d \n", &Nparams);      /* number of background model parameters */
    fscanf(MdlParams, "%d \n", &nrows);

/* use these numbers to allocate paramater arrays */

      params    =  malloc(Nparams * sizeof(double));
      paramArr  =  malloc(Nparams * nrows * sizeof(float));
      
      for (ii=0; ii<Nparams; ii++) params[ii]=0.0;


/* for spibkgLo  */

      ii=0;
      if (Nparams == Nparams_Lo) { 
	kk=Nparams-1;

      while ( fscanf(MdlParams, "%d %f %f  %f %f %f %f %f %f %f %f %f \n", 
	&rowID[ii], &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
		    &paramArr[ii*Nparams+kk--] )  != EOF) {

	 ii++;
	 kk=Nparams-1;
      }

	row1=rowID[0]; rowN=rowID[ii-1];
      }


/* for spibkgMed  */

      ii=0;
      if (Nparams == Nparams_Med) { 
	kk=Nparams-1;


      while (  fscanf(MdlParams, "%d %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f \n",  
	&rowID[ii], &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
  		    &paramArr[ii*Nparams+kk--] ) != EOF) {
	  ii++;
	  kk=Nparams-1;
      }

	row1=rowID[0]; 
	rowN=rowID[ii-1];
      }


/* for spibkgHi  */

      ii=0;
      if (Nparams == Nparams_Hi) { 
	kk=Nparams-1;

      while ( fscanf(MdlParams, "%d %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f \n", 
	&rowID[ii], &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
                    &paramArr[ii*Nparams+kk--], 
		    &paramArr[ii*Nparams+kk--] )  != EOF) {
	  ii++;
	  kk=Nparams-1;
      }
	
	row1=rowID[0]; 
	rowN=rowID[ii-1];
      }
      fclose(MdlParams);   


/*  read in the input spectral data file, forming an Nspec X Nchan array, 
    as well as an array of the exposure times, and whether counts or rates
    are used (we must first obtain the spectrum table dimensions, and allocate 
    arrays
*/

      get_spec_info( inPHAfil, &nch, &nrows );    /* get spectral array dimensions */

      specdat  = malloc(nrows * nch * sizeof(float));    /* spectral data array */
      expos    = malloc(nrows * sizeof(float));          /* vector of exposure values */

      Ne       = nch;
      Elo      = malloc(Ne * sizeof(float));  /* vector of energy bin boundaries */
      Ehi      = malloc(Ne * sizeof(float));  /* (just for select range)  */
      E        = malloc((Ne+1) * sizeof(double));     /* (linear) mid-points  */

      photar   = malloc(Ne * sizeof(double)); /* vector of energy bin boundaries */
      photer   = malloc(Ne * sizeof(double)); /* vector of energy bin boundaries */

/* get spectral data, exposure and energy-bin arrays */

      get_spec_dat( inPHAfil, specdat, expos, Elo, Ehi, row1, rowN );

      for (ii=0; ii<Ne; ii++) 
	E[ii] = (double)Elo[ii];             /* populate XSPEC type E-bins arrray */
      E[ii] = (double)Ehi[ii-1];

      free(Elo);
      free(Ehi);

    
/* loop sequentially through selected rows, and compute background model 
   flux at each row. then modify (i.e. background subtract, in count space
   spectral data accordingly. First, check to see which background model,
   as this then sets the number of paramaters per spectrum
*/

      for (ii=row1; ii<= rowN; ii++) {
	for (jj=0; jj<Nparams; jj++) 
	  params[jj] = (double)paramArr[(ii-row1)*Nparams+jj]; /* fill param array for row */

 	kk = rowID[ii-row1];


/* make appropriate calls to Lo, Med and Hi res models */

        switch (Nparams) {

	case Nparams_Lo: 

	spibkgLo( E, Ne, params, kk, photar, photer, 0 );   /* call spibkg_Lo model */
	break;

	case Nparams_Med: 
	spibkgMed( E, Ne, params, kk, photar, photer, 0 );   /* call spibkg_Med model */
	break;

	case Nparams_Hi: 
	spibkgHi( E, Ne, params, kk, photar, photer, 0 );   /* call spibkg_Hi model */
	break;
	}

	for (jj=0; jj<nch; jj++)  
	  specdat[(kk-1)*nch+jj] -= (float)photar[jj]*expos[ii]; /* now background subtract */
      }


    free(paramArr);
    free(E);
    free(photar);
    free(photer);
    
/*  write out the revised PHA-II file into the specified output file 

*/

    wrtPHAoutp( inPHAfil, specdat, nch, outpPHAfil, bkgRowNum );
    
    printf("\n done \n");
    
    return status;

}
/* ------------------------ end of main program ------------------------------*/




/************************************************************************
 *
 *  get input paramaters from standard FTOOLS parameter file
 *
 ************************************************************************/

    int get_params(char *inPHAfil, char *BkgMdlParams, char *outpPHAfil , 
		   char *SPIbkgInitFil, int *bkgRowNum )
{

   char  msg[100];
   int   status=0;                          /* for Uclgst: in/outfile length - 1 */

    status=PILGetString("inPHAfil", inPHAfil);
        if (status != 0) { 
        sprintf( msg, "could not get inpPHAfil parameter, status = %d\n", status);
        return status;
    }
    

    status=PILGetString("BkgMdlParams", BkgMdlParams);
        if (status != 0) { 
        sprintf( msg, "could not get BkgMdlParams parameter, status = %d\n", status);
        return status;
    }
    

    status=PILGetString("outpPHAfil", outpPHAfil);
        if (status != 0) { 
        sprintf( msg, "could not get outpPHAfil parameter, status = %d\n", status);
        return status;
    }

    status=PILGetString("SPIbkgInitFil", SPIbkgInitFil );
        if (status != 0) { 
        sprintf( msg, "could not get SPIbkgInitFil parameter, status = %d\n", status);
        return status;
    }

    status=PILGetInt("bkgSpecNum", bkgRowNum );
        if (status != 0) { 
        sprintf( msg, "could not get bkgSpecNum parameter, status = %d\n", status);
        return status;
    }

 return status; 

}


/*-------------------------------------------------------------------------------
  get_spec_dat      read pha-II data file, and return spectral data as an 
                    nrows X nch array 

    inputs:         spec_fil            character array, name of input pha-II file

    outputs:        specdat             nrows X nch float array containing
                                        spectral data
		    expos               array of exposure times (nrows)
            
 -------------------------------------------------------------------------------*/

 int  get_spec_dat(char *spec_fil, float specdat[], float expos[], float Elo[], 
	     	  float Ehi[], int row1, int rowN)

{
    fitsfile *infptr;            /* pointer to input and output FITS files */
    char extname[30], comm[60];
    int  status, hdutype, nfound, colnum=5, anynulls, nelem, ii, nch, nrows;
    long naxes[2], frow, felem;
    float nullval; 

    char infilename[60];         /* name for existing FITS file   */
    strcpy( infilename, spec_fil);
    
    status = 0;

 /* open the existing FITS files */
    if ( fits_open_file(&infptr,  infilename,  READONLY,  &status))
         printf(" **** FITSio Error ***** %d  \n", status);


 /* read EBOUNDS table to get number of channels */

    /* move to the 3rd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

 /* check to see if this is the correct extension, if not read the next */

    if ( (strncmp(extname,"SPI.-EBDS-OAR",9) != 0) &&
	 (strncmp(extname, "EBOUNDS"     ,6) != 0) )
       if ( fits_movabs_hdu(infptr, 4, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    nch = naxes[1];

/* read the EBOUNDS array values (just for the range of interest */

    colnum= 2;
    frow  = 1;
    felem = 1;
    nelem = nch;
    nullval = -99.;

    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, nelem, 
        &nullval, Elo, &anynulls, &status) )
        printf(" **** FITSio Error ***** %d  \n", status);

    colnum= 3;
    frow  = 1;
    felem = 1;
    nelem = nch;
    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, nelem, 
        &nullval, Ehi, &anynulls, &status) )
        printf(" **** FITSio Error ***** %d  \n", status);

 /* now read SPECTRUM table to get data values (over the full range)  */

 /* move to the 2nd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 2, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

      if ( (strncmp(extname,"SPI.-CSPE-OSR",9) != 0) && 
	   (strncmp(extname,"SPECTRUM",8) != 0) )
     if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    nrows = naxes[1];

/*   specdat = (float *) malloc( nch * nrows * sizeof(float)); */

 /* find which column contains the spectra COUNTS values */
    if ( fits_get_colnum(infptr, CASEINSEN, "counts", &colnum, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the SPECTRUM column values */
    frow  = 1;
    felem = 1;
    nelem = nch * nrows;
    nullval = -99.;
    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, nelem, 
        &nullval, specdat, &anynulls, &status) )
        printf(" **** FITSio Error ***** %d  \n", status);

 /* find which column contains the EXPOSURE values */
    if ( fits_get_colnum(infptr, CASEINSEN, "exposure", &colnum, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the EXPOSURE column values */
    frow  = 1;
    felem = 1;
    nelem = nrows;
    nullval = -99.;
    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, naxes[1], 
        &nullval, expos, &anynulls, &status) )
        printf(" **** FITSio Error ***** %d  \n", status);

    for (ii=0; ii<naxes[1]; ii++) expos[ii]+=epsilon;

    /*    nrows = naxes[1]; */

    return(status);
}



/* ---- wrtPHAoutp: cloned from CFITIO cookbook routines to copy the input PHA file
       to an output file of similatr structure, but with the revised spectral data
       populating the specctrum table                           -------------------- */


int   wrtPHAoutp( char *inpPHAfil, float specdat[], int nch, char *outpPHAfil, int bkgRowNum )
{
    /*******************************88888888*************************************/
    /* copy the 1st, 2nd, 4th & 5th HDUs from the input file to a new FITS file */
    /****************************************************************************/
    fitsfile *infptr;      /* pointer to the FITS file, defined in fitsio.h */
    fitsfile *outfptr;                 /* pointer to the new FITS file      */


    int status, morekeys, hdutype, noutrows, ii, irow, nkeys, nfound,
        keypos, NextExt=1, jj;
    long naxes[2];
    unsigned char *buffer;
    char card[FLEN_CARD], extname[30], comm[30];
    status = 0;

    remove(outpPHAfil);            /* Delete old file if it already exists */

    /* open the existing FITS file */
    if ( fits_open_file(&infptr, inpPHAfil, READONLY, &status) ) 
         printf(" **** FITSio Error ***** %d  \n", status);


    if (fits_create_file(&outfptr, outpPHAfil, &status)) /*create FITS file*/
         printf(" **** FITSio Error ***** %d  \n", status); /* report  errors */


/* --- copy the primary "array" & header from the input-->output file ------*/

    morekeys = 0;     /* don't reserve space for additional keywords */
    if ( fits_copy_hdu(infptr, outfptr, morekeys, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

/* ----- copy the ISDC grouping table extension to the output file ---- */

    /* move to the 2nd HDU in the input file (a binary table in this case) */
    NextExt++;
    if ( fits_movabs_hdu(infptr, NextExt, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);


    if (hdutype != BINARY_TBL)  {
        printf("Error: expected to find a binary table in this HDU\n");
        return(status);
    }


    /* create new extension in the output file */
    if ( fits_create_hdu(outfptr, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);


    /* get number of keywords */
    if ( fits_get_hdrpos(infptr, &nkeys, &keypos, &status) ) 
         printf(" **** FITSio Error ***** %d  \n", status);


    /* copy all the keywords from the input to the output extension */
    for (ii = 1; ii <= nkeys; ii++)  {
        fits_read_record (infptr, ii, card, &status); 
        fits_write_record(outfptr,    card, &status); 
    }

    /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);


    /* allocate buffer large enough for 1 row of the table */
    buffer = (unsigned char *) malloc(naxes[0]);

    /*  Copy row-by-row to the output table */
    for (noutrows = 0, irow = 1; irow <= naxes[1]; irow++)  {
        noutrows++;
        fits_read_tblbytes( infptr, irow,      1, naxes[0], buffer, &status); 
        fits_write_tblbytes(outfptr, noutrows, 1, naxes[0], buffer, &status); 
    } 


/* ---copy the spectrum extension, but with the revised spectral data ----*/

    /* move to the 3rd HDU in the input file (a binary table in this case) */
    NextExt++;
    if ( fits_movabs_hdu(infptr, NextExt, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    if (hdutype != BINARY_TBL)  {
        printf("Error: expected to find a binary table in this HDU\n");
        return(status);
    }


    /* create new extension in the output file */
    if ( fits_create_hdu(outfptr, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    /* get number of keywords */
    if ( fits_get_hdrpos(infptr, &nkeys, &keypos, &status) ) 
         printf(" **** FITSio Error ***** %d  \n", status);


    /* copy all the keywords from the input to the output extension */
    for (ii = 1; ii <= nkeys; ii++)  {
        fits_read_record (infptr, ii, card, &status); 
        fits_write_record(outfptr,    card, &status); 
    }

    /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);


    /* allocate buffer large enough for 1 row of the table */
    buffer = (unsigned char *) malloc(naxes[0]);

    /*  Copy row-by-row to the output table */
    for (noutrows = 0, irow = 1; irow <= naxes[1]; irow++)  {
        noutrows++;
        fits_read_tblbytes( infptr, irow,      1, naxes[0], buffer, &status); 
        fits_write_tblbytes(outfptr, noutrows, 1, naxes[0], buffer, &status); 
    } 

/* ---- now overwrite the spectral data with the background subtracted data---- */

    ii = nch * irow;
    fits_write_col(outfptr, TFLOAT, 5, 1, 1, ii, specdat, &status); 


    /* assume photon statistics are valid, ie STAT_ERR = sqrt(COUNTS) */

    for (ii=0; ii<naxes[1]; ii++)
      for(jj=0; jj<nch; jj++)
	specdat[ii*nch + jj] = sqrt(fabs(specdat[ii*nch + jj]));

    ii = nch * irow;
    fits_write_col(outfptr, TFLOAT, 6, 1, 1, ii, specdat, &status); 


/* restore, at least the non-negative values */
    for (ii=0; ii<naxes[1]; ii++)
      for(jj=0; jj<nch; jj++)
	specdat[ii*nch + jj] = specdat[ii*nch + jj]*specdat[ii*nch + jj];



/* ----- copy the EBOUNDS table extension to the output file (if its there) ---- */

    /* move to the 4th HDU in the input file (a binary table in this case) */

    NextExt++;
    if ( fits_movabs_hdu(infptr, NextExt, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    if (hdutype != BINARY_TBL)  {
        printf("Error: expected to find a binary table in this HDU\n");
        return(status);
    }

   fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);
     

     if (strncmp(extname,"SPI.-EBDS-OAR",12) == 0 ) {
       NextExt++;


    /* create new extension in the output file */
    if ( fits_create_hdu(outfptr, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    /* get number of keywords */
    if ( fits_get_hdrpos(infptr, &nkeys, &keypos, &status) ) 
         printf(" **** FITSio Error ***** %d  \n", status);


    /* copy all the keywords from the input to the output extension */
    for (ii = 1; ii <= nkeys; ii++)  {
        fits_read_record (infptr, ii, card, &status); 
        fits_write_record(outfptr,    card, &status); 
    }

    /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);


    /* allocate buffer large enough for 1 row of the table */
    buffer = (unsigned char *) malloc(naxes[0]);

    /*  Copy row-by-row to the output table */
    for (noutrows = 0, irow = 1; irow <= naxes[1]; irow++)  {
        noutrows++;
        fits_read_tblbytes( infptr, irow,      1, naxes[0], buffer, &status); 
        fits_write_tblbytes(outfptr, noutrows, 1, naxes[0], buffer, &status); 
    } 
  }


/* ----- and next, copy the POINTING table extension to the output file ---- */


    /* move to the next HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, NextExt, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    if (hdutype != BINARY_TBL)  {
        printf("Error: expected to find a binary table in this HDU\n");
        return(status);
    }


    /* create new extension in the output file */
    if ( fits_create_hdu(outfptr, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    /* get number of keywords */
    if ( fits_get_hdrpos(infptr, &nkeys, &keypos, &status) ) 
         printf(" **** FITSio Error ***** %d  \n", status);

    /* copy all the keywords from the input to the output extension */
    for (ii = 1; ii <= nkeys; ii++)  {
        fits_read_record (infptr, ii, card, &status); 
        fits_write_record(outfptr,    card, &status); 
    }

    /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);


    /* allocate buffer large enough for 1 row of the table */
    buffer = (unsigned char *) malloc(naxes[0]);

    /*  Copy row-by-row to the output table */
    for (noutrows = 0, irow = 1; irow <= naxes[1]; irow++)  {
        noutrows++;
        fits_read_tblbytes( infptr, irow,      1, naxes[0], buffer, &status); 
        fits_write_tblbytes(outfptr, noutrows, 1, naxes[0], buffer, &status); 
    } 



  

/* note: stop if there is no POINITNG or EBOUNDS & return */
/* read the extension name from EXTNAME keyword     */
/*
    fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);
    if (strncmp(extname,"SPI.-RSP.-DBS",9) == 0 ) {
       if (fits_close_file(outfptr, &status) ||
         fits_close_file(infptr, &status))     
         printf(" **** FITSio Error ***** %d  \n", status);
	return(status);
    }

*/

/* ------- and finally, the RESP_DB table -----------*/

    /* move to the 6th HDU in the input file (a binary table in this case) */
    NextExt++;

    if ( fits_movabs_hdu(infptr, NextExt, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    if (hdutype != BINARY_TBL)  {
        printf("Error: expected to find a binary table in this HDU\n");
        return(status);
    }


    /* create new extension in the output file */
    if ( fits_create_hdu(outfptr, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    /* get number of keywords */
    if ( fits_get_hdrpos(infptr, &nkeys, &keypos, &status) ) 
         printf(" **** FITSio Error ***** %d  \n", status);


    /* copy all the keywords from the input to the output extension */
    for (ii = 1; ii <= nkeys; ii++)  {
        fits_read_record (infptr, ii, card, &status); 
        fits_write_record(outfptr,    card, &status); 
    }


    /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    if ( bkgRowNum >= 1 ) 
      ii = naxes[1]-1;
      fits_update_key( outfptr, TLONG, "NAXIS2", &ii, 
	"number of rows in table" , &status);

    /* allocate buffer large enough for 1 row of the table */
    buffer = (unsigned char *) malloc(naxes[0]);

    /*  Copy row-by-row to the output table */


    for (noutrows = 0, irow = 1; irow <= naxes[1]; irow++)  {
        noutrows++;
        fits_read_tblbytes( infptr, irow,      1, naxes[0], buffer, &status); 
	if ( noutrows != bkgRowNum ) 
         fits_write_tblbytes(outfptr, noutrows, 1, naxes[0], buffer, &status); 
    } 



/* -----close up, and return -------------------------*/

    if (fits_close_file(outfptr, &status) ||
        fits_close_file(infptr, &status)) /* close files */
         printf(" **** FITSio Error ***** %d  \n", status);

    return(status);
}



/*-------------------------------------------------------------------------------
  get_spec_info     read pha-II data file, and return dimenions of the spectral 
                    array nrows X nch array 

    inputs:         spec_fil            character array, name of input pha-II file

    outputs:        nrows               number of rows in pha-ii SPECTRUM table
                    nch                 number of spectral channels
            
 -------------------------------------------------------------------------------*/

int  get_spec_info(char *spec_fil, int *nch, int *nrows)

{
    fitsfile *infptr;                /* pointer to input and output FITS files */
    char extname[30], comm[60];
    int  status, hdutype, nfound; 
    long naxes[2];

    char infilename[60];         /* name for existing FITS file   */
    strcpy( infilename, spec_fil);
    
    status = 0;

 /* open the existing FITS files */
    if ( fits_open_file(&infptr,  infilename,  READONLY,  &status))
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read EBOUNDS table to get number of channels  */

    /* move to the 3rd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

 /* check to see if this is the correct extension, if not read the next */


    if ( (strncmp(extname,"SPI.-EBDS-OAR",9) != 0) &&
	 (strncmp(extname, "EBOUNDS"     ,6) != 0) )
       if ( fits_movabs_hdu(infptr, 4, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

    *nch = naxes[1];

 /* now read SPECTRUM table to get number of rows   */

 /* move to the 2nd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 2, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

      if ( (strncmp(extname,"SPI.-CSPE-OSR",9) != 0) && 
	   (strncmp(extname,"SPECTRUM", 8)     != 0) )
     if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printf(" **** FITSio Error ***** %d  \n", status);

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printf(" **** FITSio Error ***** %d  \n", status); 

    *nrows = naxes[1];

    return(status);
}
