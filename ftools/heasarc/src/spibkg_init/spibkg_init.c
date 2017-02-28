#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "cfitsio.h"
#include "fitsio.h"
#include "cftools.h"
#include "spibkg.h"
#include "pfile.h"


/*----------------------------------------------------------------------
  main program - calls fitsio routines

  This program prepares a database (FITS) file to be used with the XSPEC
  (ver>=12.0) INTEGRAL SPI background models. Inputs are a type-II PHA data
  file, a component-1 INTERGAL/SPI ARF response matrix file(s), relative 
  weighting factors, an (optional) SPIBACK output file, and an output file
  specification. 

    

	C.R. Shrader    Code 661 NASA/GSFC    06/2003

 ----------------------------------------------------------------------*/


void spibkg_init()
{


    float    *specresp=0,*specdat=0, *bkg=0, *expos=0;

    float    *arf_sum1=0, *arf_sum2=0,*arf_sum3=0,
             *arf_sum4=0, *arf_sort=0;

    float    *spiback_wts=0, *spiback_summed_wts=0, sum=0;
    float    w1, w2, w3, w4, arf_thresh=0, arf_avg=0, xx, yy, 
      wts[4], CtSum[19],  chisq=0.0, chisq_tot[19], chisq_th=0;
    
    int      nrows, nch, ii=0, jj=0, idet=0, ndet=19, nn=0,
             kk=0, ll=0, status=0, numsrc, no_resp_dat=1, iimax=0,
             arf_thresh_idx=0;


    char     inarf1[100], inarf2[100],  inarf3[100], 
             inarf4[100], inpha[100], outfil[100],
             spibackfil[100], w_arf1[1], w_arf2[1],
             w_arf3[1], w_arf4[1], use_spiback[10], Tmp[100]; 

    int      arf_sort_idx[N_max_spec], no_spiback=0, Ndets;

    c_ptaskn("spi_bkg_init 1.01");


/* read in parameter file data: arf(s), spectral data, spiback data
       weighting factors and ouput file name */

   ii = get_bkg_params( inarf1, inarf2, inarf3, inarf4, inpha, 
			use_spiback, spibackfil, outfil, Ndets,
			 &numsrc, wts, &status, Tmp);

   strcpy( inpha, Tmp); /* this is necessary to handle SUN Uclg anomaly */


    if (ii <0 ) {
      printf("**** error reading parameter file *** \n");
      return;
    }

/* normalize, weighting factors, i.e. SUM(wts(i) = 1)        */    

    xx = wts[0] + wts[1] + wts[2] + wts[3];  
    w1 = wts[0]/xx;
    w2 = wts[1]/xx;
    w3 = wts[2]/xx;
    w4 = wts[3]/xx;

    if (strcmp(spibackfil, "NONE") == 0) no_spiback=1;


/* read arf file one time w/o getting response data, to get nch & nrows
       for dynamical array memory allocation */

   get_arf_dat(inarf1, specresp, &nrows, &nch, no_resp_dat);  /* extract arf values */

/* now allocate memory for various arrays  */

   arf_sum1 = malloc(nrows * sizeof(float));          /* row-wise summed arf(s) */
   arf_sum2 = malloc(nrows * sizeof(float));
   arf_sum3 = malloc(nrows * sizeof(float));
   arf_sum4 = malloc(nrows * sizeof(float));

   arf_sort     = malloc(nrows * sizeof(float));      /* rank-ordered summs */

   specresp     = malloc(nrows * nch * sizeof(float));

   specdat  = malloc(nrows * nch * sizeof(float));    /* spectral data array */
   expos    = malloc(nrows * sizeof(float));          /* vector of exposure values */


/* now read in spectral data and get shadowed detector data */

    get_spec_dat( inpha, specdat, expos, &nrows, &nch );


/* this is a really kludgy way to deterimine the number of detectors. it breaks dwn 
   though if (in the unlikely event) N(scws) = 14*ndet, 15*ndet, ..., 19*ndet
   there should be a par file entry for this, but the Uclg routines seem too brain 
   dead to handle it. next build, I'll try switching to PIL, but a new make file
   is needed.
*/

    for (ii=14; ii<=19; ii++) {
      xx = (float)nrows / (float)(ii);
      if ( ceil(xx) == floor(xx) ) ndet=ii;
    }


/* read in the arf(s), combine as appropriate, and find minima (i.e.
   indices corresponding to shadowed detectors                 */

    no_resp_dat=0;
    get_arf_dat(inarf1, specresp, &nrows, &nch, no_resp_dat);  /* extract arf values */
    sum_arfs( nch, nrows, specresp, arf_sum1 );                /* sum rows  */
     for (ii=0; ii<nrows; ii++)
       arf_sum1[ii] *= w1;                                     /* apply weigting factor */

    if (strcmp(inarf2, "NONE") !=0 ) {
     get_arf_dat(inarf2, specresp, &nrows, &nch, no_resp_dat);
     sum_arfs( nch, nrows, specresp, arf_sum2 ); 
     for (ii=0; ii<nrows; ii++)
       arf_sum1[ii] += w2 * arf_sum2[ii];    } 

    if (strcmp(inarf3, "NONE")!=0)  {
     get_arf_dat(inarf3, specresp, &nrows, &nch, no_resp_dat);
     sum_arfs( nch, nrows, specresp, arf_sum3 ); 
     for (ii=0; ii<nrows; ii++)
       arf_sum1[ii] += w3 * arf_sum3[ii];    }

    if (strcmp(inarf4, "NONE")!=0)  {
     get_arf_dat(inarf4, specresp, &nrows, &nch, no_resp_dat);
     sum_arfs( nch, nrows, specresp, arf_sum3 ); 
     for (ii=0; ii<nrows; ii++)
       arf_sum1[ii] += w4 * arf_sum3[ii];    }

    free(specresp);    /* relevant response info now compressed     */
    free(arf_sum2);    /* into single array, so free up some memory */
    free(arf_sum3);


/* rank order summed arfs (lowest first), along with corresponding indicies */


    find_min_idx( arf_sum1, expos, nrows, arf_sort, arf_sort_idx );


  /* get threshold (i.e.maximum net source illumination) arf value from rank 
     sorted arfs */

   xx =  arf_frac_thresh * (float)nrows;
   arf_thresh_idx = (int)xx;
   arf_thresh = arf_sort[arf_thresh_idx];


   free(arf_sort);

/*
 this portion of the code has been revised to work in a more simple way. 
 the detector array is scanned, and the lowest arfIindex (ie most hihghly
 shadowed spectrum for each detector is set as the template background for 
 that detector. thus, the zeroth order model is for detector-to-detector
 background varations, bnut no time variation. The latter can me modeled
 in XSPEC by "untieing the normalization terms
*/

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* allocate background spectral array   */

  bkg     = malloc(nrows * nch * sizeof(float));  
   

   for (idet=0; idet<ndet; idet++) {         /* loop over detectors */
     arf_thresh_idx=nrows;                   /* initialize index and summed cts */
     CtSum[idet]=0;                          /* for each detector */


     for (ii=idet; ii<nrows; ii+=ndet) {     /* loop over spectra   */
                                             /* find min arf index and use */
                                             /* corresponding spectra as template */

      if (arf_sort_idx[ii] < arf_thresh_idx)  {
	for (jj=0; jj<nch; jj++) 
	  bkg[idet*nch+jj] = specdat[ii*nch+jj]/expos[ii];

	arf_thresh_idx = arf_sort_idx[ii];   /* store index at each step */
	kk = ii;                             /* kk'th element is the template spec */
      }
     }


   for (jj=nch/2; jj<nch; jj++)                   /* store summed counts for upper */
     CtSum[idet] += specdat[kk*nch+jj]/expos[kk]; /* channels of idet template */

   }

   for (idet=0; idet<ndet; idet++) {           /* loop over detectors */
     for (ii=idet; ii<nrows; ii+=ndet) {       /* loop over spectra   */
       w1=0;
       for (jj=nch/2; jj<nch; jj++)            /* get total upper-chan cts */
	 w1 += specdat[ii*nch+jj]/expos[ii];

       w1 /= CtSum[idet];
       for (jj=0; jj<nch; jj++)                /* set weighted template */
	  bkg[ii*nch+jj] = w1*bkg[idet*nch+jj]; 
     }
   }


/* %%%%%%%%%%%%%%% */
/* now make a second pass to screen for exceedingly large chisquare values (as
   compared to threshiold value set in the include file). flag these by setting the
   arf_sort_idx to a high value
*/


/*    compute summed chi-square statistic, to screen for bad-bkg templates   */ 

   for (idet=0; idet<ndet; idet++) {         /* loop over detectors */
     chisq_tot[idet]=0;                      /* initialize summed chi-square */

     for (ii=idet; ii<nrows; ii+=ndet) {     /* loop over spectra   */
       chisq=0.;                            /* & accumulte chi-square */
	for (jj=0; jj<nch; jj++) 
	 chisq += pow((bkg[ii*nch+jj]*expos[ii] - specdat[ii*nch+jj]),2) /
	   (specdat[ii*nch+jj]+epsilon);

	chisq_tot[idet] += chisq/(float)nch/(float)(nrows/ndet);
     }
   }

   chisq_th=1.0e6;                           /* get threshhold value from min */
   for (idet=0; idet<ndet; idet++) {         
     if ((chisq_tot[idet] < chisq_th) && (chisq_tot[idet] !=0.))
       chisq_th=chisq_tot[idet];
   }
   chisq_th *= 1.1;

   for (idet=0; idet<ndet; idet++) {         /* loop over detectors */
     arf_thresh_idx=nrows;                   /* initialize index and summed cts */

     if (chisq_tot[idet] > chisq_th ) {

     for (ii=idet; ii<nrows; ii+=ndet) {     /* loop over spectra   */
                                             /* find min arf index and use */
                                             /* corresponding spectra as template */

      if (arf_sort_idx[ii] < arf_thresh_idx)  {
	chisq=0.;
	for (jj=0; jj<nch; jj++) 
	 chisq += pow((bkg[ii*nch+jj]*expos[ii] - specdat[ii*nch+jj]),2) /
	   (specdat[ii*nch+jj]+epsilon);

	chisq /= (float)nch;


/* look for min arf_idx value && high chisquare;i.e. first pass template may be anomalous*/

	kk=-1;
        if (chisq > chisq_th) {
    	 arf_thresh_idx = arf_sort_idx[ii];  /* store index at each step */
 	 kk = ii;                            /* kk'th element is the template spec */


	for (jj=0; jj<nch; jj++) 
	  bkg[idet*nch+jj] = specdat[ii*nch+jj]/expos[ii];
	}
      }
     }

     if (kk > 0) {
       CtSum[idet]=0.;
       for (jj=nch/2; jj<nch; jj++)                   /* store summed counts for upper */
	 CtSum[idet] += specdat[kk*nch+jj]/expos[kk]; /* channels of idet template */
     }
     }
   }

/* & again, reassign weighted values after chi-square screening step */

   for (idet=0; idet<ndet; idet++) {           /* loop over detectors */
     if (chisq_tot[idet] > chisq_th) {
      for (ii=idet; ii<nrows; ii+=ndet) {       /* loop over spectra   */
        w1=0;
        for (jj=nch/2; jj<nch; jj++)            /* get total upper-chan cts */
 	 w1 += specdat[ii*nch+jj]/expos[ii];

        w1 /= CtSum[idet];
        for (jj=0; jj<nch; jj++)                /* set weighted template */
	  bkg[ii*nch+jj] = w1*bkg[idet*nch+jj]; 
      }
     }
   }


/* finally, eliminate extreme cases in terms of chisquare deviation */

   kk=0;
   for (ii=0; ii<nrows; ii++){
       chisq=0.;                            /* & accumulte chi-square */

       for (jj=0; jj<nch; jj++) 
	 chisq += pow((bkg[ii*nch+jj]*expos[ii] - specdat[ii*nch+jj]),2) /
	  (specdat[ii*nch+jj]+epsilon);
       chisq /= (float)nch;

       if (chisq > 1.75*chisq_th) {
       for (jj=0; jj<nch/2; jj++) 
	 bkg[ii*nch+jj] = 0.970*specdat[ii*nch+jj]/expos[ii];
       for (jj=nch/2; jj<nch; jj++) 
	 bkg[ii*nch+jj] = 0.999*specdat[ii*nch+jj]/expos[ii];
       kk++;

       }
   }

   w3 = (float)kk/(float)nrows*100.;



/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


/* extra step to handle cases (early mission data) with zeros */


   for (ii=0; ii<nrows; ii++)           /* 1st remove zeros from bkg template array */
     for (jj=0; jj<nch; jj++) 
       if (bkg[ii*nch+jj] == 0) bkg[ii*nch+jj] = (1.-epsilon)*specdat[ii*nch+jj]/expos[ii];

   for (ii=0; ii<nrows; ii++)          /* reset bkg arr values (i.e. to 0) if spectrum values are zero */

     if (specdat[ii*nch] + specdat[ii*nch+1] + specdat[ii*nch+2] == 0) 
       for(jj=0; jj<nch; jj++) bkg[ii*nch+jj] = specdat[ii*nch+jj]/expos[ii] + epsilon; 


        free(specdat); 


/* apply (optionally) spiback information; at the moment, this is rather unsophisticated
   in principle, this could be handled in XSPEC by the spi model function

*/

      spiback_wts = malloc(nrows * nch * sizeof(float));       /* elem-by-elem spiback weights */
      spiback_summed_wts = malloc(nrows * sizeof(float));      /* energy-summed spiback weights*/

      for (ii=0; ii<nrows*nch; ii++) spiback_wts[ii]=0;        
      for (ii=0; ii<nrows; ii++) spiback_summed_wts[ii]=0;



	if (strcmp(spibackfil, "NONE")!=0 && 
           (strncmp(use_spiback, "Y", 1) == 0 ||  strncmp(use_spiback, "y", 1) == 0) ) {

      get_spiback_dat( spibackfil, spiback_wts, spiback_summed_wts, expos);

/* now form spiback weighted background templates, as average x spiback weight factor */
      
      xx = (float)nrows * arf_frac_thresh;  /* use a subset of the shadowed spectra to */
      iimax = (int)xx;                      /* to form a template, which is then scaled */
                                            /* by spiback statistical weights */

      if (strncmp(use_spiback, "Y", 1) == 0) {
       for (jj=0; jj<nch; jj++) {
 	sum=0;
        kk=0;
	/*
 	for (ii=0; ii<nrows; ii++) 
	  sum += bkg[ii*nch+jj] / (float)nrows;
	*/
	for (ii=0; ii<nrows; ii++) {
	 if (arf_sort_idx[ii] < iimax) {
	   sum += bkg[ii*nch+jj];
	   kk++; }
	  } sum /= (float)(kk);


	for (ii=0; ii<nrows; ii++) 
	  bkg[ii*nch+jj] = sum*spiback_summed_wts[ii];

       }
     }
    }
      
    free(expos);      /* don't need exposure time array any more */



   /* write FITS output    */


    wrt_main_hdr( outfil, inarf1, inarf2, inarf3, inarf4, w_arf1, w_arf2, 
		  w_arf3, w_arf4, inpha, spibackfil );

    wrt_bkg_model( outfil, bkg, spiback_wts, spiback_summed_wts, arf_sort_idx,
		  &nrows, &nch, no_spiback);

    printf("\n done \n");

}
/* ------------------------ end of main program ------------------------------*/




/*------------------------------------------------------------------------------
  this routine reads a component-1 spi-arf file, and returns the values in the
  nch * nrows array specresp
 -------------------------------------------------------------------------------*/

  void  get_arf_dat(char *inarf, float specresp[], int *pNrows, int *pNch,
		    int no_resp_dat) 


    /*********************************************************************/
    /* select rows from an input table and copy them to an output array  */
    /*********************************************************************/
{
    fitsfile *infptr, *outfptr;  /* pointer to input and output FITS files */
    unsigned char *buffer;
    char card[FLEN_CARD];
    int status, hdutype, nkeys, keypos, nfound, colnum, anynulls, ii, jj;
    long naxes[2], frow, felem, noutrows, irow;
    float nullval;

    char infilename[100];         /* name for existing FITS file   */
    /*    ii = strcpy( infilename, inarf);  */
    
    status = 0;

    /* open the existing FITS files */
    if ( fits_open_file(&infptr, inarf,  READONLY,  &status) )
         printerror( status );

 /* read EBOUNDS table to get number of channels */

    /* move to the 3rd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 4, &hdutype, &status) )
         printerror( status );

    if (hdutype != BINARY_TBL)  {
        printf("Error: expected to find a binary table in this HDU\n");
        return;
    }

    /* get number of keywords */
    if ( fits_get_hdrpos(infptr, &nkeys, &keypos, &status) ) 
         printerror( status );

    /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printerror( status );

    *pNch =  naxes[1];   /* number of detector  energy channels */
    ii    =  naxes[1];

/* now read SPECRESP table to get arf values   */

    /* move to the 2nd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printerror( status );

    if (hdutype != BINARY_TBL)  {
        printf("Error: expected to find a binary table in this HDU\n");
        return;
    }

    /* get number of keywords */
    if ( fits_get_hdrpos(infptr, &nkeys, &keypos, &status) ) 
         printerror( status );

    /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printerror( status );

    *pNrows =  naxes[1];   /* number of rows (spectra) in arf SPECRESP table */

    if (no_resp_dat == 1) 
      return;

    /* find which column contains the SPECRESP values */
    if ( fits_get_colnum(infptr, CASEINSEN, "specresp", &colnum, &status) )
         printerror( status );

    /* read the SPECRESP column values */
    frow = 1;
    felem = 1;
    nullval = -99.;

    /* now read in the response data  */

    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, naxes[1]*ii,
        &nullval, specresp, &anynulls, &status) )
        printerror( status );

   return;
}
/*--------------------------------------------------------------------------*/
void readheader ()

    /**********************************************************************/
    /* Print out all the header keywords in all extensions of a FITS file */
    /**********************************************************************/
{
    fitsfile *fptr;       /* pointer to the FITS file, defined in fitsio.h */

    int status, nkeys, keypos, hdutype, ii, jj;
    char filename[]  = "atestfil.fit";     /* name of existing FITS file   */
    char card[FLEN_CARD];   /* standard string lengths defined in fitsioc.h */

    status = 0;

    if ( fits_open_file(&fptr, filename, READONLY, &status) ) 
         printerror( status );

    /* attempt to move to next HDU, until we get an EOF error */
    for (ii = 1; !(fits_movabs_hdu(fptr, ii, &hdutype, &status) ); ii++) 
    {
        /* get no. of keywords */
        if (fits_get_hdrpos(fptr, &nkeys, &keypos, &status) )
            printerror( status );

        printf("Header listing for HDU #%d:\n", ii);
        for (jj = 1; jj <= nkeys; jj++)  {
            if ( fits_read_record(fptr, jj, card, &status) )
                 printerror( status );

            printf("%s\n", card); /* print the keyword card */
        }
        printf("END\n\n");  /* terminate listing with END */
    }

    if (status == END_OF_FILE)   /* status values are defined in fitsioc.h */
        status = 0;              /* got the expected EOF error; reset = 0  */
    else
       printerror( status );     /* got an unexpected error                */

    if ( fits_close_file(fptr, &status) )
         printerror( status );

    return;
}



/*--------------------------------------------------------------------------*/
void printerror( int status)
{
    /*****************************************************/
    /* Print out cfitsio error messages and exit program */
    /*****************************************************/


    if (status)
    {
       fits_report_error(stderr, status); /* print error report */

       exit( status );    /* terminate the program, returning error status */
    }
    return;
}


/*--------------------------------------------------------------------------
   this routine sums the elements of the arf values passed as an nch X nrows 
   array specresp. returns nrows array of summed values
  --------------------------------------------------------------------------*/

void sum_arfs(int nch, int nrows, float specresp[], float arf_sum[] )
{
  int     ii, jj;
  float   tmp;

  for(ii=0; ii<nrows; ii++)
    {
      tmp = 0;
      
      for(jj=0; jj<nch; jj++)
	  tmp += specresp[ii*nch + jj]; 
      arf_sum[ii] = tmp;
    }
  return;
}



/*--------------------------------------------------------------------------
   this routine sorts the elements of summed-arf array values [nrows] array
   and returns arrays of sorted values, and reordered
     indices. for now, just use simple bubble sort algorithm 
  --------------------------------------------------------------------------*/

void  find_min_idx(float arf_sum[], float expos[], int nrows, float arf_sort[], 
		   int arf_sort_idx[] )
{

  int    ii,  jj, itmp;
  int    *idxtmp;
  float  tmp;

  idxtmp = malloc( nrows * sizeof(int));

  for (ii=0; ii<nrows; ii++)
    {
      arf_sort[ii] = arf_sum[ii];
      idxtmp[ii]   = ii;

      if ( expos[ii] <= epsilon ) 
	arf_sort[ii] = 9999.;
    }

   
  for (ii=0; ii<nrows; ii++)
     for (jj=0; jj<nrows-1; jj++)
      {
       if (arf_sort[jj] - arf_sort[jj+1] > 0 )  
	  { 
	    tmp            = arf_sort[jj];
	    arf_sort[jj]   = arf_sort[jj+1];
	    arf_sort[jj+1] = tmp;

	    itmp         = idxtmp[jj];
	    idxtmp[jj]   = idxtmp[jj+1];
	    idxtmp[jj+1] = itmp;
 	  }
       }

/* store first N values in arf_sort_idx, and throguh the rest away*/

  for ( ii=0; ii<nrows; ii++)
     arf_sort_idx[ii] = idxtmp[ii];
  free(idxtmp);

  return; 
}



/*--------------------------------------------------------------------------

bkg_spec_est:   given a set of indices (corresponding to the maximally shadowed 
		source spectra),return the spectral data for those rows



 ---------------------------------------------------------------------------*/

/*
void bkg_spec_est( int bspec_idx, bkg_spec)
{ }
*/

/************************************************************************
 *
 *  get spi_bkg_init paramaters 
 *
 ************************************************************************/

int get_bkg_params(char *inarf1, char *inarf2, char *inarf3, char *inarf4, 
		   char *inpha, char *use_spiback, char *spibackfil, 
		   char *outfil, int Ndets, int *numsrc, float wts[], 
		   int *status, char *Tmp) 

{

  float tmp, xx, yy;  
   char  msg[100];
   int   BufLen_2 = 99, *ii;              /* for Uclgst: in/outfile length - 1 */

    Uclgst("inarf1", inarf1, status);
        if (*status != 0) { 
        sprintf( msg, "could not get inarf1 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
    

    Uclgst("inarf2", inarf2, status);
        if (*status != 0) { 
        sprintf( msg, "could not get inarf2 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
    

    Uclgst("inarf3", inarf3, status);
        if (*status != 0) { 
        sprintf( msg, "could not get inarf3 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }

    Uclgst("inarf4", inarf4, status);
        if (*status != 0) { 
        sprintf( msg, "could not get inarf4 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
    

     Uclgsr("w_arf1", &xx, status);
        if (*status != 0) { 
        sprintf( msg, "could not get w_arf1 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
	}
       wts[0]=xx; 

    Uclgsr("w_arf2", &xx, status);
        if (*status != 0) { 
        sprintf( msg, "could not get w_arf2 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
       wts[1]=xx; 

    Uclgsr("w_arf3", &xx, status);
        if (*status != 0) { 
        sprintf( msg, "could not get w_arf3 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
       wts[2]=xx; 

    Uclgsr("w_arf4", &xx, status);
        if (*status != 0) { 
        sprintf( msg, "could not get w_arf4 parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
      wts[3]=xx;        

    Uclgst("inpha", inpha, status);
        if (*status != 0) { 
        sprintf(msg, "could not get inpha parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }

    strcpy( Tmp, inpha);

    Uclgst("use_spiback", use_spiback, status);
        if (*status != 0) { 
        sprintf(msg, "could not get inpha parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }

	strcpy( spibackfil, "NONE");
	if ( strncmp(use_spiback,"Y",1) == 0   || 
	     strncmp(use_spiback,"y",1) == 0  ) {
     Uclgst("spibackfil", spibackfil, status);
         if (*status != 0) { 
        sprintf(msg, "could not get inpha parameter, status = %d\n", status);
         c_fcerr(msg);
         return(*status);
     }
	}
    

    Uclgst("outfil", outfil, status);
       if (*status != 0) { 
       sprintf(msg, "could not get outfil parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
	} 


       /*
    Uclgsi("ndets", &Ndets, status);

     if (*status != 0) {
       sprintf(msg, "could not get ndets parameter, status = %d\n", *status);
        c_fcerr(msg);
        return(*status); 
	} 

       */

 return(*status); 

}


/*-------------------------------------------------------------------------------
  get_spec_dat      read pha-II data file, and return spectral data as an 
                    nrows X nch array 

    inputs:         spec_fil            character array, name of input pha-II file

    outputs:        specdat             nrows X nch float array containing
                                        spectral data
                    nrows               number of rows in pha-ii SPECTRUM table
                    nch                 number of spectral channels
            
 -------------------------------------------------------------------------------*/

void get_spec_dat(char *spec_fil, float specdat[], float expos[],
		  int *nrows, int *nch)

{
    fitsfile *infptr, *outfptr;  /* pointer to input and output FITS files */
    unsigned char *buffer;
    char card[FLEN_CARD], extname[30], comm[60];
    int status, hdutype, nkeys, keypos, nfound, colnum=5, anynulls, nelem, ii;
    long naxes[2], frow, felem, noutrows, irow;
    float nullval; 

    char infilename[60];         /* name for existing FITS file   */
    strcpy( infilename, spec_fil);
    
    status = 0;

 /* open the existing FITS files */
    if ( fits_open_file(&infptr,  infilename,  READONLY,  &status))
         printerror( status );

 /* read EBOUNDS table to get number of channels */

    /* move to the 3rd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printerror( status );

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

 /* check to see if this is the correct extension, if not read the next */

    if ( (strncmp(extname,"SPI.-OBS.-EBDS",9) != 0) &&
	 (strncmp(extname, "EBOUNDS"      ,6) != 0) )
       if ( fits_movabs_hdu(infptr, 4, &hdutype, &status) )
         printerror( status );

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printerror( status );

    *nch = naxes[1];

 /* now read SPECTRUM table to get data values   */

 /* move to the 2nd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 2, &hdutype, &status) )
         printerror( status );

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

      if ( (strncmp(extname,"SPI.-PHA2-SPE",9) != 0) && 
	   (strncmp(extname,"SPECTRUM",8) != 0) )
     if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printerror( status );

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_keys_lng(infptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printerror( status );

    *nrows = naxes[1];

    /*   specdat = (float *) malloc( *nch * *nrows * sizeof(float)); */

 /* find which column contains the spectra COUNTS values */
    if ( fits_get_colnum(infptr, CASEINSEN, "counts", &colnum, &status) )
         printerror( status );

 /* read the SPECTRUM column values */
    frow = 1;
    felem = 1;
    nelem = *nch * *nrows;
    nullval = -99.;
    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, nelem, 
        &nullval, specdat, &anynulls, &status) )
        printerror( status );

 /* find which column contains the EXPOSURE values */
    if ( fits_get_colnum(infptr, CASEINSEN, "exposure", &colnum, &status) )
         printerror( status );

 /* read the SPECTRUM column values */
    frow = 1;
    felem = 1;
    nelem = *nch * *nrows;
    nullval = -99.;
    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, naxes[1], 
        &nullval, expos, &anynulls, &status) )
        printerror( status );

    for (ii=0; ii<naxes[1]; ii++) expos[ii]+=epsilon;

    /*    *nrows = naxes[1]; */

    return;
}



/*--------------------------------------------------------------------------*/

void wrt_bkg_model(char *outfil, float bkg[], float spiback_wts[], 
		   float spiback_summed_wts[], int arf_sort_idx[],  
		   int *nrows, int *nch, int no_spiback)

    /*******************************************************************/
    /* Create a binary table extension containing 5 columns and N rows */
    /*******************************************************************/
{
    fitsfile *fptr;       /* pointer to the FITS file, defined in fitsio.h */
    int status, hdutype;
    long firstrow, firstelem;

    int ii, kk, tfields   = 5;           /* table will have 5 columns */
    long jj;
    float xx=0.0;

    char filename[100], tmp[7];             /* name for new FITS file */
    char extname[16] = "BKG_TMPLTS";                /* extension name */

    /* define column fields */
    int  specid[N_max_spec];

    /* define the name, datatype, and physical units for the 5 columns */
    char *ttype[] = { "SPECID", "RATE",   "TRACER", "SPIBCK_WTS", "ARF_IDX"};
    char *tunit[] = { "\0",   "cts/sec",  " ",      " ",           " "     };
    char *tform[5];

    ii = *nch;
    sprintf( tmp, "%dE", ii );
    tform[0] = "1I";
    tform[1] = tmp;
    tform[2] = "1E";
    tform[3] = tmp;
    tform[4] = "1I";

    strcpy(filename, outfil);


    for (jj=0; jj<*nrows; jj++) specid[jj]=jj;

    status=0;

    /* open the FITS file containing a primary array and an ASCII table */
    if ( fits_open_file(&fptr, filename, READWRITE, &status) ) 
         printerror( status );

    if ( fits_movabs_hdu(fptr, 1, &hdutype, &status) ) /* move to 2nd HDU */
         printerror( status );

    /* append a new empty binary table onto the FITS file */
    if ( fits_create_tbl( fptr, BINARY_TBL, *nrows, tfields, ttype, tform,
                tunit, extname, &status) )
         printerror( status );

    firstrow  = 1;  /* first row in table to write   */
    firstelem = 1;  /* first element in row  (ignored in ASCII tables) */

    /* write names to the first column (character strings) */
    /* write diameters to the second column (longs) */
    /* write density to the third column (floats) */

    fits_write_col(fptr, TINT, 1, firstrow, firstelem, *nrows, specid,
                   &status);

    ii = *nrows;
    ii *= *nch;

    /* write out background spectral templates */
    fits_write_col(fptr, TFLOAT, 2, firstrow, firstelem, ii, bkg,
                   &status);


      /* write out spiback summed weighting factors  */
      fits_write_col(fptr, TFLOAT, 3, firstrow, firstelem, *nrows, spiback_summed_wts,
      &status);  


      /* write out spiback summed weighting factors  */
      fits_write_col(fptr, TFLOAT, 4, firstrow, firstelem, ii, spiback_wts,
      &status);  


      /* write out spiarf-summed weighting factors  */
      fits_write_col(fptr, TINT, 5, firstrow, firstelem, *nrows, arf_sort_idx,
      &status);  
 
   if ( fits_close_file(fptr, &status) )       /* close the FITS file */
         printerror( status );


    return;
}


/*--------------------------------------------------------------------------*/

void wrt_main_hdr(  char * outfil, char *inarf1,char *inarf2, 
		    char *inarf3, char *inarf4, char *w_arf1, char *w_arf2, 
		    char *w_arf3, char *w_arf4, char *inpha, char* spibackfil )

    /******************************************************/
    /* Create a FITS primary array containing a 2-D image */
    /******************************************************/
{
    fitsfile *fptr;       /* pointer to the FITS file, defined in fitsio.h */
    int status, ii=0, jj=0;
    long  fpixel, nelements;
    unsigned short array[1][1];

    /* initialize FITS image parameters */
    char filename[100],comment[100]; /* name for new FITS file,comment text */
    int bitpix   =  USHORT_IMG; /* 16-bit unsigned short pixel values       */
    long naxis    =   2;        /* 2-dimensional image                      */    
    long naxes[2] = { 1, 1 };   /* image is 1 pixels wide by 1 rows */

    strcpy(filename, outfil);

    remove(filename);               /* Delete old file if it already exists */

    status = 0;         /* initialize status before calling fitsio routines */

    if (fits_create_file(&fptr, filename, &status)) /* create new FITS file */
         printerror( status );           /* call printerror if error occurs */

    /* write the required keywords for the primary array image.     */
    /* Since bitpix = USHORT_IMG, this will cause cfitsio to create */
    /* a FITS image with BITPIX = 16 (signed short integers) with   */
    /* BSCALE = 1.0 and BZERO = 32768.  This is the convention that */
    /* FITS uses to store unsigned integers.  Note that the BSCALE  */
    /* and BZERO keywords will be automatically written by cfitsio  */
    /* in this case.                                                */


    if ( fits_create_img(fptr,  bitpix, ii, naxes, &status) )
         printerror( status );          

    /* initialize the values in the image with a linear ramp function */
    for (jj = 0; jj < naxes[1]; jj++)
    {   for (ii = 0; ii < naxes[0]; ii++)
        {
            array[jj][ii] = ii + jj;
        }
    }

    fpixel = 1;                               /* first pixel to write      */
    nelements = naxes[0] * naxes[1];          /* number of pixels to write */

    /* write the array of unsigned integers to the FITS file */
    /*    if ( fits_write_img(fptr, TUSHORT, fpixel, nelements, array[0], &status) )
        printerror( status );            
    */

    /* write another optional keyword to the header */
    /* Note that the ADDRESS of the value is passed in the routine */

    if ( fits_write_key(fptr, TSTRING, "COMMENT", "Inputs:",
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 1 ARF: %s ", inarf1);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 2 ARF: %s ", inarf2);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 3 ARF: %s ", inarf3);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 4 ARF: %s ", inarf4);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 1 Wt: %s ", w_arf1);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 2 Wt: %s ", w_arf2);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 3 Wt: %s ", w_arf3);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "Source 4 Wt: %s ", w_arf4);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "PHA File: %s ", inpha);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           

    sprintf(comment, "spiback File: %s ", spibackfil);
    if ( fits_write_key(fptr, TSTRING, "COMMENT", comment,
         NULL, &status) )
         printerror( status );           


    if ( fits_close_file(fptr, &status) )                /* close the file */
         printerror( status );           

    return;
}




/*-------------------------------------------------------------------------------
  get_spiback_dat   read spiback output data file, and return scaling factors
                    as an nrows (or nrows X nch) array 

    inputs:         spibackfil          character array, name of input pha-II file

    outputs:        spiback_wts         nrows (X nch) float array containing
                                        weighting factors for background modeling
            
 -------------------------------------------------------------------------------*/

void get_spiback_dat(char *spec_fil, float spiback_wts[], 
		     float spiback_summed_wts[], float expos[] )


{
    fitsfile *infptr, *outfptr;  /* pointer to input and output FITS files */
    unsigned 	char *buffer;
    char 	card[FLEN_CARD], extname[30], comm[60];
    int 	status, hdutype, nrows, nch, nfound, colnum=5, anynulls, nelem;
    int		ii, jj, kk;
    long	naxes[2], frow, felem, noutrows, irow;
    float 	nullval, sum=0;
    char	infilename[80];         /* name for existing FITS file   */

    strcpy( infilename, spec_fil);
    
    status = 0;

 /* open the existing FITS files */
    if ( fits_open_file(&infptr,  infilename,  READONLY,  &status))
         printerror( status );

 /* read SPI.-BMOD-DSP  table to get number of channels */

    /* move to the 2nd HDU in the input file (a binary table in this case) */
    if ( fits_movabs_hdu(infptr, 2, &hdutype, &status) )
         printerror( status );

 /* read the extension name from EXTNAME keyword     */
      fits_read_key_str(infptr, "EXTNAME", extname, comm, &status);

 /* check to see if this is the correct extension, if not read the next */

    if ( (strncmp(extname,"SPI.-BMOD-DSP",10)) != 0) 
       if ( fits_movabs_hdu(infptr, 3, &hdutype, &status) )
         printerror( status );

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_key(infptr, TLONG, "NAXIS2", &nrows, NULL, &status) )
         printerror( status );

 /* read the NAXIS1 and NAXIS2 keyword to get table size */
    if (fits_read_key(infptr, TLONG, "EBIN_NUM", &nch, NULL, &status) )
         printerror( status );

 /* find which column contains the COUNTS values */
    if ( fits_get_colnum(infptr, CASEINSEN, "counts", &colnum, &status) )
         printerror( status );

 /* read the COUNTS column values */
    frow = 1;
    felem = 1;
    nelem =  nrows*nch;
    nullval = -99.;

    if (fits_read_col(infptr, TFLOAT, colnum, frow, felem, nelem, 
        &nullval, spiback_wts, &anynulls, &status) )
        printerror( status );

    
/* form normalized vectors of summed (over energies), and individual 
   (normalized over detector/pointing) wirhting factors
*/

    for (ii=0; ii<nrows; ii++)
 	 spiback_summed_wts[ii] = 0;    

    for (ii=0; ii<nrows; ii++)
	for (jj=0; jj<nch; jj++) 
	  spiback_summed_wts[ii] += spiback_wts[ii*nch + jj]/expos[ii];

    for (ii=0; ii<nrows; ii++)
	sum += spiback_summed_wts[ii]/(float)nrows;

/* form summed (over energies) array of weighting factors   */

    for (ii=0; ii<nrows; ii++)
      spiback_summed_wts[ii] = 1. + (spiback_summed_wts[ii] - sum)/sum;


/* form full array of (relative) weighting factors */
    for (jj=0; jj<nch; jj++) {
	sum = 0;
	for (ii=0; ii<nrows; ii++)
	  sum += spiback_wts[ii*nch+jj]/(float)nrows;
	for (ii=0; ii<nrows; ii++)
	  spiback_wts[ii*nch+jj] =  1. + (spiback_wts[ii*nch+jj] - sum)/sum;
     }
    return;
}
