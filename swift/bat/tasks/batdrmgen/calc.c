/* -------- calc.c - functions to calculate the BAT response-------- */
/* ----------------------------------------------------------------- */
/* 
 * Routines for calculating the BAT  response matrix values
 *
 * internal_escale() - internally derived incident energy scale (dummy)
 * calc_response() - calculate response at each incident energy 
 *
 * A. Parsons / C. Markwardt

----------------------------
revision 1.2  Build 5 version of calc.c - AMP 5/26/03
----------------------------
revision 1.3  Initialize response matrix
----------------------------
revision 1.4  Made the changes required to accommodate the changes in the 
              parms and resp structures in batdrmgen.h AMP 7/8/03
----------------------------
revision 1.6  This version has the user defined energy scale feature working, 
              -AMP 8/11/03
----------------------------
revision 1.7  Cleared up variable name ambiguity between the actual incident 
              energy scale and the Nbins and energy range parameters. Now the 
              resp and parms structures are truly isolated in terms of the 
              kind of information they contain.  Also fixed column names in 
              the output response file so that XSPEC could recognise it 
              -AMP 8/14/03
----------------------------
revision 1.8  Implementing the version of lookup_parms.c that can read in the 
              calibration parameter files.  AMP 8/19/03
----------------------------
revision 1.9  Implementation of CALDB to get the correct files for the cal 
             parameters as well as depth distribution table,  -AMP 8/20/03
----------------------------
revision 1.10 The lookup_param routine  was being called much more often than 
              necessary - Ann moved it out of the nested detector slice loop 
              and placed it just inside the incident energy loop.  -AMP 8/21/03
----------------------------
revision 1.12 Made INDEF escale simply a logarithmic scale 10 - 200 keV, 160 
              bins with adjustments to line up with bin edges at 23.172, 
              26.093,27.471,30.993  - the bin edge values are hard coded 
              inside internal_escale.  Made INDEF the default for escale in 
              the param file and made escale a hidden parameter. --AMP 8/24/03
----------------------------
revision 1.13 Build 6 version of calc.c -AMP 8/24/03
----------------------------
revision 1.14 Must initialise response matrix  to zero before using - Craig M 8/26/03
----------------------------
revision 1.15 Reading in mask weight keywords, corrected K-edges in default 
              energy scale -AMP 11/21/03
----------------------------
revision 1.16 Attempt to put gaussian noise conv. outside loops
----------------------------
revision 1.17 Build 7 version of calc.c - AMP 12/3/03
Abandoned the path that included changing the way the gaussian noise is handled,
This version is closer to ver 1.15
----------------------------
revision 1.18 Moved call to get the parameter lookup file names to outside 
the Ein loop, Added multi-mutau model as an option for calc_response

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_utils.h"
#include "bat_gswdev.h"
#include "batdrmgen.h"


int internal_escale(struct parm_struct *parms, 
		    struct resp_struct *resp)
{
  int i;
  int status=0;
  double ecurr, efact, delta;
  int lo_colnum =0,hi_colnum=0;
/*char escale_fname[PIL_PATH_MAX];*/
  long int nrows = 0;

  fitsfile *escale_fits;

/*strcpy(escale_fname, parms->efile);*/

/*
 * Generate the desired incident photon energy scale
 * Memory is automatically allocated for the bin edges.  The user is
 * responsible for free()ing it when done.
 */

  if (strcasecmp(parms->escale, "FILE") == 0) {

    /* ==== CM added for CALDB incident energy bins ==== */

    if (strcasecmp(parms->efile,"CALDB") == 0) {
      /* Assume the CALDB-related keywords have already been read
         (they have, in respio.c:readphainfo() */
      char *codenam = "SPECRESP MATRIX";   /* Code name of CALDB dataset */
      char *expr = "-";               /* Selection expression */
      int maxret = 1;                 /* Maximum desired results */
      char *pfile = parms->efile;     /* String to receive query result */
      /*char *pfile = escale_fname;*/     /* String to receive query result */
      char online[80], *ponline = online; /* Dummy string to hold online */
      long int extno[1];              /* Dummy array to hold extension */
      int nret = 0, nfound = 0;       /* Number actually found */
      
      bat_caldb_search(&(resp->caldb), codenam, expr, maxret, PIL_PATH_MAX, 
  		       &pfile,extno, &ponline, &nret, &nfound, &status);


      if ((status != 0) || (nret == 0) || (nfound == 0)) {
	/* FALLBACK - check for old EBOUNDS extension */

	codenam = "RESP_ESCALE";   /* Code name of CALDB dataset */
	status = 0;

	/* XXX the "old" caldb file says it's an EBOUNDS extension but
	   the contents do not reflect that.  This is a fall-back
	   position used during the transition to the "new" caldb
	   file. */
	bat_caldb_search(&(resp->caldb), codenam, expr, maxret, PIL_PATH_MAX, 
			 &pfile,extno, &ponline, &nret, &nfound, &status);

	if ((status != 0) || (nret == 0) || (nfound == 0)) {
	  
	  fprintf(stderr, "ERROR: could not locate CALDB energy scale file\n");
	  if (status != 0) return status;
	  return -1;
	}
      }
      
    }

/*  headas_chat(4,"Open the incident photon bin edges fits file (%s)\n",
		escale_fname);*/
    headas_chat(4,"Open the incident photon bin edges fits file (%s)\n",
		parms->efile);

/*  if (fits_open_table(&escale_fits, escale_fname, READONLY, &status)) {*/
    if (fits_open_table(&escale_fits, parms->efile, READONLY, &status)) {
    /*fprintf(stderr,"ERROR: Could not open file '%s'.\n",escale_fname);*/
      fprintf(stderr,"ERROR: Could not open file '%s'.\n",parms->efile);
      return status;
    }
    
    fits_movnam_hdu(escale_fits,BINARY_TBL,"SPECRESP MATRIX",0,&status);
    if (status) {
      status = 0;
      /* XXX the "old" caldb file says it's an EBOUNDS extension but
         the contents do not reflect that.  This is a fall-back
         position used during the transition to the "new" caldb
         file. */
      fits_movnam_hdu(escale_fits,BINARY_TBL,"EBOUNDS",0,&status);
    }
    if (status) {
      fprintf(stderr, "ERROR: could not find the SPECRESP MATRIX extension\n");
      fprintf(stderr, "       for the input photon energy scale\n");
      return status;
    }

    fits_get_colnum(escale_fits, CASEINSEN,"ENERG_LO", &lo_colnum, &status);
    fits_get_colnum(escale_fits, CASEINSEN,"ENERG_HI", &hi_colnum, &status);
    if (status) {
      fprintf(stderr, "ERROR: Could not find the ENERG_LO/HI columns\n");
      return status;
    }

    fits_get_num_rows(escale_fits, &nrows, &status);
    if (status || (nrows == 0)) {
      fprintf(stderr, "ERROR: Could not find the number of rows\n");
      return status;
    }

    resp->nebins = nrows;
    parms->Nphoton_bins = nrows;
       
     /* Allocate memory for incident energy bin edges */

       resp->energ_lo = (double *) malloc(sizeof(double)*resp->nebins);
       if (resp->energ_lo == 0) return MEMORY_ALLOCATION;

       resp->energ_hi = (double *) malloc(sizeof(double)*resp->nebins);
       if (resp->energ_hi == 0) {
	    free(resp->energ_lo);
	    resp->energ_lo = 0;
	    return MEMORY_ALLOCATION;
       }

       headas_chat(4,"Using fits_read_col to read the energy bin edges\n");

       fits_read_col(escale_fits, TDOUBLE, lo_colnum, 1, 1, resp->nebins, 0, 
		resp->energ_lo, 0, &status);
       fits_read_col(escale_fits, TDOUBLE, hi_colnum, 1, 1, resp->nebins, 0, 
		resp->energ_hi, 0, &status);

       fits_close_file(escale_fits,&status);

       if (status) {
	   fits_report_error(stderr,status);
	   exit(status);
       }

       headas_chat(4,"Checking energy bin edges\n");

       for (i=0;i<resp->nebins;i++) {
	 if (resp->energ_lo[i]>resp->energ_hi[i]) {
	   fprintf(stderr,
	       "ERROR: energ_lo[%d]=%f > energ_hi[%d]=%f\n",
	       i,resp->energ_lo[i],i,resp->energ_hi[i]);
	   status=1;
	 }
       }
	   
       for (i=0;i<resp->nebins-1;i++) {
	 if (resp->energ_lo[i+1]!=resp->energ_hi[i]) {
	   fprintf(stderr,
	       "ERROR: energ_hi[%d]=%f != energ_lo[%d]=%f\n",
	       i,resp->energ_hi[i],i+1,resp->energ_lo[i+1]);
	   status=1;
	 }
       }

       if (status) exit(status);

  }

  if (strcasecmp(parms->escale, "LIN") == 0) {
i=0; /* Allocate memory for incident energy bin edges */
       resp->energ_lo = (double *) malloc(sizeof(double)*resp->nebins);
       if (resp->energ_lo == 0) return MEMORY_ALLOCATION;

       resp->energ_hi = (double *) malloc(sizeof(double)*resp->nebins);
       if (resp->energ_hi == 0) {
	     free(resp->energ_lo);
	     resp->energ_lo = 0;
	     return MEMORY_ALLOCATION;
       }

       delta = (parms->Elimit_hi - parms->Elimit_lo)/resp->nebins;
       headas_chat(3,"linear binning\n");
       headas_chat(3,"elimit_lo =%f, elimit_hi = %f, delta =%f\n",
		   parms->Elimit_lo,parms->Elimit_hi,delta);
       headas_chat(5,"i   energ_lo[i]    energ_hi[i]  \n");
	
       ecurr =parms->Elimit_lo;
       for(i=0;i<resp->nebins;i++){
	    resp->energ_lo[i] = ecurr;
	    resp->energ_hi[i] = ecurr + delta;
	    ecurr =resp->energ_hi[i];
	    headas_chat(4,"%d        %f         %f\n", i,
                           resp->energ_lo[i],resp->energ_hi[i] );
       }
  }
       
  if (strcasecmp(parms->escale, "LOG") == 0) {
     
       i=0;     

       /* Allocate memory for incident energy bin edges */
       resp->energ_lo = (double *) malloc(sizeof(double)*resp->nebins);
       if (resp->energ_lo == 0) return MEMORY_ALLOCATION;

       resp->energ_hi = (double *) malloc(sizeof(double)*resp->nebins);
       if (resp->energ_hi == 0) {
	      free(resp->energ_lo);
	      resp->energ_lo = 0;
	      return MEMORY_ALLOCATION;
       }

       ecurr = parms->Elimit_lo;
       efact = pow(parms->Elimit_hi/parms->Elimit_lo,1.0/parms->Nphoton_bins);
	
       headas_chat(3,"Logarithmic binning \n");
       headas_chat(3,"elimit_lo =%f, elimit_hi = %f, efact =%f\n",
                      parms->Elimit_lo,parms->Elimit_hi,efact);
       headas_chat(5,"i   energ_lo[i]    energ_hi[i]  \n");

       for(i=0;i<resp->nebins;i++){
	    resp->energ_lo[i] = ecurr;
	    resp->energ_hi[i] = ecurr*efact;
	    ecurr *= efact;
	    headas_chat(4,"%d        %f         %f\n", i,
			resp->energ_lo[i],resp->energ_hi[i] );
       }
  }

  for(i=0;i<resp->nebins;i++)
    headas_chat(4,"%d   %f  %f\n", i,resp->energ_lo[i],resp->energ_hi[i] );

  headas_chat(5,"Leaving internal_escale\n");
  return 0;
}


int calc_response(struct parm_struct *parms, 
		  struct resp_struct *resp)
{
/* 
 * Make a response matrix for the BAT using the mutau_model generalized 
 * for use with mask-weighted summed spectra over the entire array
 *
 * struct parm_struct *parms - task parameters
 * struct resp_struct *resp - response matrix parameters and data
 *                            all except matrix must be filled in
 *                            matrix must be pre-allocated
 *                            upon return, matrix is filled w/ response values
 *
 * RETURNS: status code, 0=ok, otherwise failure
 *
 */

  struct multi_mt_struct cal_parms;
  float *dist = 0;
  int i, j, status =0;
  int fits_status=0;
  int hdutype;
  int bad_hv;
  char file_type_string[20];
  char comment[100];
  int *detmask=0;         /* Detector quality mask data */
  long int axesd[2];
  int dm_enable[32768];

  fitsfile *depth_fits = 0;

  int n_depths;
  int n_distvectors;

  float ecent;
  float tanx;
  float tany;

  double *rowptr = 0;
  char  calfname[PIL_PATH_MAX]; 
  char  mtval_fname[PIL_PATH_MAX];
  char  depth_fname[PIL_PATH_MAX];
  long extno;

  headas_chat(5,"...Inside calc_response...\n");

  /* ==== CM Cal file and depth file.  If the files are stored in
     CALDB, they query has already taken place in the caller routine,
     so there is nothing for us to do here. ==== */
  strcpy(calfname,parms->calfile);
  strcpy(depth_fname, parms->depthfile);
  strcpy(mtval_fname, parms->mtvalfile);

  tanx =resp->srcpos[0]/resp->srcpos[2];  /* BAT_XOBJ/BAT_ZOBJ */
  tany =resp->srcpos[1]/resp->srcpos[2];  /* BAT_YOBJ/BAT_ZOBJ */

  headas_chat(5,"Source position is %f,%f,%f\n",resp->srcpos[0],
      resp->srcpos[1],resp->srcpos[2]);
  headas_chat(5,"tanx = %f, tany= %f\n",tanx,tany);


    /* mutau parameters*/

    extno = -1;  /* -1 indicates that the extension has already been set */
    if ((status = get_mutau(mtval_fname,extno,&cal_parms))) {
      fprintf(stderr,"ERROR:  get_mutau failed\n");
      return status;
    }

    /* calibration parameters */

    extno = -1;  /* -1 indicates that the extension has already been set */
    if ((status = 
	  get_cal_parms(calfname,parms->use_mean,extno,&cal_parms,resp))){
      fprintf(stderr,"ERROR:  get_cal_parms failed\n");
      return status;
    }

    headas_chat(4,"cal_parms structure:\n");
    headas_chat(4,"     cal_parms.voltage = %f\n",cal_parms.voltage);
    headas_chat(4,"     cal_parms.sigma = %f\n",cal_parms.sigma);
    headas_chat(4,"     cal_parms.gain_coeff = %f\n",cal_parms.gain_coeff);
    headas_chat(4,"     cal_parms.gain_index = %f\n",cal_parms.gain_index);
    headas_chat(4,"     cal_parms.exp_coeff = %f\n",cal_parms.exp_coeff);
    headas_chat(4,"     cal_parms.exp_index = %f\n",cal_parms.exp_index);
    headas_chat(4,"     cal_parms.exp_lambda = %f\n",cal_parms.exp_lambda);
    headas_chat(4,"     cal_parms.norm = %f\n",cal_parms.norm);
    headas_chat(4,"     cal_parms.batfilev = %d\n",cal_parms.batfilev);
    headas_chat(4,"     cal_parms.use_gain_adj = %d\n",cal_parms.use_gain_adj);
    headas_chat(4,"     cal_parms.gain_adj = \n");
    for (i=0;i<5;i++) headas_chat(4,"       %g\n",cal_parms.gain_adj[i]);
    headas_chat(4,"     cal_parms.use_sigma = %d\n",cal_parms.use_sigma);
    headas_chat(4,"     cal_parms.sigma_coeffs = \n");
    for (i=0;i<7;i++) headas_chat(4,"       %g\n",cal_parms.sigma_coeffs[i]);
    headas_chat(4,"     cal_parms.src_density = %f\n",cal_parms.src_density);
    headas_chat(4,"     cal_parms.src_low_coeff = %f\n",
	cal_parms.src_low_coeff);
    headas_chat(4,"     cal_parms.src_low_index = %f\n",
	cal_parms.src_low_index);
    headas_chat(4,"     cal_parms.src_high_coeff = %f\n",cal_parms.
	src_high_coeff);
    headas_chat(4,"     cal_parms.src_high_index = %f\n",cal_parms.
	src_high_index);
    headas_chat(4,"     cal_parms.src_smooth = %f\n",cal_parms.src_smooth);
    headas_chat(4,"     cal_parms.src_thickness = %f\n",
	cal_parms.src_thickness);
    headas_chat(4,"     cal_parms.air_density = %f\n",cal_parms.air_density);
    headas_chat(4,"     cal_parms.air_low_coeff = %f\n",
	cal_parms.air_low_coeff);
    headas_chat(4,"     cal_parms.air_low_index = %f\n",
	cal_parms.air_low_index);
    headas_chat(4,"     cal_parms.air_high_coeff = %f\n",
	cal_parms.air_high_coeff);
    headas_chat(4,"     cal_parms.air_high_index = %f\n",
	cal_parms.air_high_index);
    headas_chat(4,"     cal_parms.air_smooth = %f\n",cal_parms.air_smooth);
    switch (cal_parms.batfilev) {
      case 1:
        headas_chat(4,"     cal_parms.psv = \n");
        for (i=0;i<5;i++) headas_chat(4,"       %f\n",cal_parms.psv[i]);
        break;
      case 2:
        headas_chat(4,"     cal_parms.psv = \n");
        for (i=0;i<14;i++) headas_chat(4,"       %f\n",cal_parms.psv[i]);
        break;
      case 3:
        headas_chat(4,"     cal_parms.psv = \n");
        for (i=0;i<14;i++) headas_chat(4,"       %f\n",cal_parms.psv[i]);
        break;
      case 4:
        headas_chat(4,"     cal_parms.psv = \n");
        for (i=0;i<14;i++) headas_chat(4,"       %f\n",cal_parms.psv[i]);
        break;
      default:
        fprintf(stderr, 
	    "ERROR: batdrmgen does not recognize format version %d\n",
	    cal_parms.batfilev);
        return 1;
        break;
    }
    headas_chat(4,"     cal_parms.pb_density = %f\n",cal_parms.pb_density);
    headas_chat(4,"     cal_parms.pb_low_coeff = %f\n",cal_parms.pb_low_coeff);
    headas_chat(4,"     cal_parms.pb_low_index = %f\n",cal_parms.pb_low_index);
    headas_chat(4,"     cal_parms.pb_high_coeff = %f\n",
	cal_parms.pb_high_coeff);
    headas_chat(4,"     cal_parms.pb_high_index = %f\n",
	cal_parms.pb_high_index);
    headas_chat(4,"     cal_parms.cfunncof = %d\n",
	cal_parms.cfunncof);
    headas_chat(4,"     cal_parms.cfunemin = %g\n",
	cal_parms.cfunemin);
    headas_chat(4,"     cal_parms.cfunemax = %g\n",
	cal_parms.cfunemax);
    headas_chat(4,"     cal_parms.cfunp = \n");
    for (i=0;i<cal_parms.cfunncof;i++)
      headas_chat(4,"       %f\n",cal_parms.cfunp[i]);
 
    /* open depth distribution fits file */

    if (fits_open_file(&depth_fits, depth_fname, READONLY, &fits_status)) {
      fits_report_error(stderr,fits_status);
      exit(fits_status);
     }

     /* check the depth distribution fits file to make sure
      * it is a proper depth distribution fits file */

     if (fits_movabs_hdu(depth_fits, 2, &hdutype, &fits_status)) {
       fits_report_error(stderr,fits_status);
       exit(fits_status);
     }
     if (fits_read_key(depth_fits, TSTRING, "CCNM0001", file_type_string,
           comment, &fits_status)) {
       fprintf(stderr,"ERROR:  searching for CCNM0001 keyword in depthfile\n");
       fprintf(stderr,"        extension 2:\n");
       fits_report_error(stderr,fits_status);
       exit(fits_status);
     }
     if (strcmp(file_type_string,"DEPTH_DIST")) {
       fprintf(stderr,
	   "ERROR:  depthfile extension 2 CCNM0001 keyword != DEPTH_DIST\n");
       return 1;
     }

     /* Read the detector mask */

     if (parms->detmask[0]) {
       headas_chat(4, "  ...Reading Detector Mask file...\n");
       detmask=read_detmask(parms->detmask,axesd,0,&status);
       if (status !=0) {
	 fprintf(stderr,"ERROR: could not read quality mask %s\n",
	     parms->detmask);
	 return status;
       }
     }

     /* Read the housekeeping file */

     status=get_hk(parms->hkfile,parms,&cal_parms,dm_enable,
	 resp->time+resp->timezero);
     if (status) {
       fprintf(stderr,"ERROR: could not get the housekeeping information\n");
       return status;
     }

     /* Check the HV values that came from the housekeeping file and see if 
      * any are outside the range 180 - 220 V.  If one or more is, warn the 
      * user and keep going */

     bad_hv=0;

     for (i=0;i<128;i++) {
       if ((cal_parms.hv[i]<180)||(cal_parms.hv[i]>220)) bad_hv=1;
     }

     if (bad_hv) {
       headas_chat(1,"---\n");
       headas_chat(1,"WARNING: the bias voltage of one or more sandwich\n");
       headas_chat(1,"  is outside the range of 180 to 220 V.\n");
       headas_chat(1,"  The results of batdrmgen may be inaccurate.\n");
     }

     /* Create the array of good detectors per sandwich */

     if (parms->detmask[0]) {
       status=sum_detmask(detmask,cal_parms.san_ngood);
       if (status) {
	 fprintf(stderr,"ERROR: could not sum detector mask\n");
	 return status;
       }
     }
     else {
       /*
	* ----------- COMMENTED OUT ------------
	* --- (detmask not used at present) ----
	*
       headas_chat(3,"---\n");
       headas_chat(3,"WARNING: no detmask specified\n");
       headas_chat(3,"  Assuming all detectors are enabled.\n");

	* -----  END OF COMMENTED SECTION ----
       */
       for (i=0;i<256;i++) cal_parms.san_ngood[i]=128;
       /*
       status=sum_dm_enable(dm_enable,cal_parms.san_ngood);
       if (status) {
	 fprintf(stderr,"ERROR: could not find sum from dm_enable array\n");
	 return status;
       }
       */
     }

  /* loop through the photon energy bins */
  
    for (i=0; i<resp->nebins; i++) { 

      rowptr = resp->matrix + resp->nphabins*i; 
	
      for (j=0; j<(resp->nphabins); j++) rowptr[j] = 0;

      /* Line above initializes this row to zero --DO NOT REMOVE:
       * malloc()'d MEMORY HAS TO BE INITIALIZED TO ZERO */

      ecent = (resp->energ_lo[i] + resp->energ_hi[i])/2.0;
      headas_chat(3, "\nIncident E(%d) = %f\n", i,ecent);

      /* look up the depth distributions */

      status = lookup_params(ecent, tanx, tany,depth_fits,&dist,&n_depths,
          &n_distvectors);

      if (status) {
	if (dist) free(dist);
	fprintf(stderr, "ERROR:  lookup_params failed\n"); 
	return status = -1;
      }
      if (!(n_distvectors == 3))  {
	if (dist) free(dist);
	fprintf(stderr, 
 	    "ERROR:  Number of rows in DEPTH_DIST table is %d, not 3\n",
	    n_distvectors); 
	return status = -1;
      }

      status = multi_mutau_func(resp,&cal_parms,n_distvectors,
	  n_depths,dist,ecent,parms->flight_data,rowptr);   

      if (status) {
	fprintf(stderr, "ERROR:  multi_mutau_func failed\n"); 
	return status;
      }

      /* free memory for dist vector
       * (it is re-allocated inside lookup_params in the next loop) */

      if (dist) free(dist);

    }  /* end of incident energy loop */

    /* apply the Crab correction function */

    if (strcasecmp(parms->fudge,"CRAB")==0) {
      if (cal_parms.cfunncof==0) {
        headas_chat(3,"---\n");
        headas_chat(1, "WARNING: fudge=CRAB, but CFUNNCOF keyword is either 0 or does not exist\n");
        headas_chat(1, "  No correction function will be applied\n");
      }
      status=apply_correction (resp, &cal_parms);
      if (status) {
        fprintf(stderr, "ERROR:  apply_correction failed\n"); 
        return status;
      }
    }

  /* close the depth distribution fits file */

  if (fits_close_file(depth_fits,&fits_status)) {
    fits_report_error(stderr,fits_status);
    exit(fits_status);
  }

headas_chat(5,"...Leaving calc_response...\n");
return status;

}

int get_correction(int n_coeffs, int n_x, float low_x, float high_x,
    float *coeffs, float *x, float *corr)

/* function: get_correction
 *
 * Returns a set of correction values to apply to the response matrix,
 * given a set of chebyshev coefficients that approximate the distributions
 *
 * Based upon "chebev.c" from
 *      "Numerical Recipes in C++: The Art of Scientific
 *      Computing", Second Edition, by
 *      W. Press, S. Teukolsky, W. Vetterling, and B. Flannery
 *      Cambridge University Press, 2002, p. 199
 *
 * The correction values are given by:
 *
 *                 / n_coeffs-1                    \
 *      corr(x) = |     Sum  coeffs[j]*T_j[log(x)]  |
 *                 \    j=0                        /
 *
 *      (Note: this is a slightly different definition than
 *      that used in "chebev.c")
 *
 * This is a slight modification of the "expand_dist" function also used
 * by batdrmgen
 *
 * Inputs:
 *
 *   n_coeffs:    number of chebyshev coefficients
 *   n_x:         number of count bins
 *   low_x:       energy above which the polynomial is evaluated
 *   high_x:      energy above which the polynomial is evaluated
 *   coeffs:      a one-dimensional array of size n_coeffs
 *                  gives the chebyshev coefficients
 *                  the order of the coefficients in the array is from 
 *                  lowest-order to highest order
 *   x:           a one-dimensional array of size n_x
 *                  energies at the center of each count bin
 *                  (in keV)
 *
 * Ouput:
 *   corr:        a one-dimensional array of size n_x
 *                  gives the correction values for response matrix
 */

{
  double d, dd, sv, y, y2;
  int ind, j;
  float *log_x;

  headas_chat(5,"...Inside get_correction...\n");

  /* transform energies to log space, as required for the coefficients 
   * for energies below low_x and above high_x, the polynomial should be
   * evaluated at low_x and high_x */

  log_x = (float *) malloc (n_x*sizeof(float));

  for (ind=0;ind<n_x;ind++) {
    if (x[ind] < low_x) log_x[ind]=log10(low_x);
    else if (x[ind] > high_x) log_x[ind]=log10(high_x);
    else log_x[ind]=log10(x[ind]);
  }

/*printf("log of energies: \n");
  for (ind=0;ind<n_x;ind++)
    printf("  %g\n",log_x[ind]);
*/
  /* compute the correction values */
  
  for (ind=0;ind<n_x;ind++) {
    y2=2.0*(y=(2.0*log_x[ind]-log_x[0]-log_x[n_x-1])/
	(log_x[n_x-1]-log_x[0]));
    d=0.0;
    dd=0.0;
    for (j=n_coeffs-1;j>=1;j--) {
      sv=d;
      d=y2*d-dd+coeffs[j];
      dd=sv;
    }
    corr[ind]=y*d-dd+coeffs[0];
  }
  
headas_chat(5,"---\n");
  headas_chat(5,"correction values: \n");
  for (ind=0;ind<n_x;ind++)
    headas_chat(5,"  %g\n",corr[ind]);

  free (log_x);
  headas_chat(5,"Leaving get_correction\n");
  return 0;
}


int apply_correction (struct resp_struct *resp, 
    struct multi_mt_struct *cal_parms)

/* Applies a correction function to the response matrix which causes the
 * spectrum from the Crab nebula between 14 and 130 keV to give a correct
 * fit to a power law spectrum of index 2.15 and normalization of 10.4 
 * photons/cm^2/s/keV at 1 keV */

{
  float *corr;
  float *emid;
  int status=0;
  int i, j;

  headas_chat(5,"...Inside apply_correction...\n");

  /* find the correction values */

  corr = (float *)malloc(sizeof(float)*resp->nphabins);
  emid = (float *)malloc(sizeof(float)*resp->nphabins);

  for (i=0;i<resp->nphabins;i++)
    emid[i]=(resp->emin[i]+resp->emax[i])/2;
  
  if (cal_parms->cfunncof > 1) {
    status=get_correction(cal_parms->cfunncof, resp->nphabins,
      cal_parms->cfunemin, cal_parms->cfunemax, cal_parms->cfunp, emid, corr);
  }
  else {
    for (i=0;i<resp->nphabins;i++)
      corr[i]=1;
  }

  headas_chat(4,"---\n");
  headas_chat(4,"correction values: \n");
  for (i=0;i<resp->nphabins;i++)
    headas_chat(4,"  %g\n",corr[i]);

  /* apply the correction values */

  for (i=0; i<resp->nebins; i++) { 
    for (j=0; j<(resp->nphabins); j++)
      resp->matrix[i*resp->nphabins+j]*=corr[j];
  }

  free (corr);
  free (emid);

  headas_chat(5,"Leaving apply_correction\n");
  return status;
}
