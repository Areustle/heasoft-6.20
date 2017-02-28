/*  batoccultmap - Tool for correcting images for occultation by the
 *                 earth, moon and/or sun
 *
 *  Originally by Piotr Banat
 *  Modified by C. Markwardt, Jay Cummings
 *
 * $Id: batoccultmap.c,v 1.11 2010/12/16 06:26:45 craigm Exp $
 */

#include <fitsio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "headas_gti.h"

#include "batoccultmap.h"

/* Main driver program */

#define TOOLSUB batoccultmap
static char taskname[] = "batoccultmap";
static char taskver[]  = "4.4";

/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/*

atmosphere (depth) - may be real from 0 to 200 [km]
method - Method to see if the next SAO line is different enough to
         bother to recompute the occultation. May be "position" or "time"
occultation - Output will be either fraction unocculted or 0 for any 
              occultation of pixel. May be "fraction" or "any"
error - If SAO entry has time different by this much, occultation will be
        recomputed for it. May be real from 1 to 60 [sec or arc min]

Defaults are:

atmosphere=160 [km]
method="time"
if method="time" then error=10 [sec]
if method="position" then error=3 [arc min]
occultation="fraction"

*/

int batoccultmap_getpar(struct parm_struct *parms) 
{
  char algstr[PIL_LINESIZE];
  int status = 0;
  int i;
  
  parms->infile[0] = 0; /* Input image*/
  parms->outfile[0] = 0;/* Output image*/
  parms->timesegerr = 10.0; /*error */
  parms->saofile[0] = 0;   /*Input SAO file*/
  parms->occultation[0] = 0;  /* occultation */
  parms->atmdpth = 0.0; /* atmosphere depth*/
  parms->method[0] = 0; /* method */
  parms->gtifile[0] = 0; /* GTI file name */
  parms->multfiles[0] = 0;
  parms->divfiles[0] = 0;
  
  if ((status = PILGetFname("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");
  
  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");
  
  else if ((status = PILGetReal("timesegtol", &parms->timesegerr)))
    fprintf(stderr, "Error reading the 'timesegtol' parameter.\n");
  
  else if ((status = PILGetString("saofile", parms->saofile)))
    fprintf(stderr, "Error reading the 'saofile' parameter.\n");
  
  else if ((status = PILGetString("occultation", parms->occultation)))
    fprintf(stderr, "Error reading the 'occultation' parameter.\n");
  
  else if ((status = PILGetReal("atmdepth", &parms->atmdpth)))
    fprintf(stderr, "Error reading the 'atmdepth' parameter.\n");
  
  else if ((status = PILGetString("method", parms->method)))
    fprintf(stderr, "Error reading the 'method' parameter.\n");
  
  else if ((status = PILGetString("gtifile", parms->gtifile)))
    fprintf(stderr, "Error reading the 'gtifile' parameter.\n");
  
  else if ((status = PILGetReal("rearth", &parms->rearth)))
    fprintf(stderr, "Error reading the 'rearth' parameter.\n");
  
  else if ((status = PILGetReal("rmoon", &parms->rmoon)))
    fprintf(stderr, "Error reading the 'rmoon' parameter.\n");
  
  else if ((status = PILGetReal("rsun", &parms->rsun)))
    fprintf(stderr, "Error reading the 'rsun' parameter.\n");
  
  else if ((status = PILGetString("constraints", parms->constraints)))
    fprintf(stderr, "Error reading the 'constraints' parameter.\n");

  else if ((status = PILGetString("multfiles", parms->multfiles)))
    fprintf(stderr, "Error reading the 'multfiles' parameter.\n");

  else if ((status = PILGetString("divfiles", parms->divfiles)))
    fprintf(stderr, "Error reading the 'divfiles' parameter.\n");

  else if ((status = PILGetString("algorithm", algstr)))
    fprintf(stderr, "Error reading the 'algorithm' parameter.\n");
  
  if (status) return status;
  
  /* Default value of parms */
  if (strcasecmp(parms->method, "none")==0) {
    strcpy(parms->method, "time");
    parms->timesegerr = 10.0;
  } else if ((strcasecmp(parms->method, "time")!=0)&&(strcasecmp(parms->method, "position")!=0)) {
    fprintf(stderr, 
	    "ERROR: method must be be one of 'time' or 'position' \n");
    return -1;
  }
  
  if ( ((parms->timesegerr)< 0.0)||((parms->timesegerr)>60.0) ) {
    fprintf(stderr, 
	    "ERROR: error must be real from 0 to 60 \n");
    return -1;
  }
  
  if ( (strcasecmp(parms->method, "position")==0)&&((parms->timesegerr)< 1.0) ) {
    parms->timesegerr = 3.0;
  }
  
  if ( (strcasecmp(parms->method, "time")==0)&&((parms->timesegerr)< 1.0) ) {
    parms->timesegerr = 10.0;
  }

  for (i=0; parms->occultation[i]; i++) {
    parms->occultation[i] = toupper(parms->occultation[i]);
  }

  if (strcasecmp(parms->occultation, "none")==0) {
    strcpy(parms->occultation, "any");
  } else if ((strcasecmp(parms->occultation, "fraction")!=0)&&(strcasecmp(parms->occultation, "any")!=0)) {
    fprintf(stderr, 
	    "ERROR: occultation must be be one of 'fraction' or 'any' \n");
    return -1;
  }
  
  if ( ((parms->atmdpth)< 0.0)||((parms->atmdpth)>200.0) ) {
    fprintf(stderr, 
	    "ERROR: atmosphere must be real from 0 to 200 \n");
    return -1;
  }
  
  if (strcasecmp(parms->gtifile,"INFILE") == 0) {
    strcpy(parms->gtifile, parms->infile);
  } else if (strcasecmp(parms->gtifile,"NONE") == 0) {
    parms->gtifile[0] = 0;
  }
  
  parms->do_earth = parms->do_moon = parms->do_sun = 0;
  if (strstr(parms->constraints,"earth") || strstr(parms->constraints,"EARTH")) {
    parms->do_earth = 1;
  }
  if (strstr(parms->constraints,"moon") || strstr(parms->constraints,"MOON")) {
    parms->do_moon = 1;
  }
  if (strstr(parms->constraints,"sun") || strstr(parms->constraints,"SUN")) {
    parms->do_sun = 1;
  }
  
  if (strcasecmp(parms->multfiles,"NONE") == 0) {
    parms->multfiles[0] = 0;
  }
  if (strcasecmp(parms->divfiles,"NONE") == 0) {
    parms->divfiles[0] = 0;
  }

  if (strcasecmp(algstr,"IMAGE") == 0) {
    parms->alg = 1;
  } else if (strcasecmp(algstr,"CONTOUR") == 0) {
    parms->alg = 2;
  } else {
    fprintf(stderr, "ERROR: algorithm must be one of IMAGE or CONTOUR\n");
    return -1;
  }
  
  return status;
}

void banner(struct parm_struct *parms)
     
{
  
  headas_chat(2, "***********************************************\n");
  headas_chat(1, "#        %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "     Input Image: %s\n", parms->infile);
  headas_chat(2, "    Output Image: %s\n", parms->outfile);
  headas_chat(2, "       Input SAO: %s\n", parms->saofile);
  if (parms->do_earth) {
    headas_chat(2, "           Earth: YES (rearth = %9.2f km)\n", 
		parms->rearth);
  } else {
    headas_chat(2, "           Earth: NO\n");
  }
  if (parms->do_moon) {
    headas_chat(2, "            Moon: YES (rmoon  = %9.2f arcmin)\n", 
		parms->rmoon);
  } else {
    headas_chat(2, "            Moon: NO\n");
  }
  if (parms->do_sun) {
    headas_chat(2, "             Sun: YES (rsun   = %9.2f arcmin)\n", 
		parms->rsun);
  } else {
    headas_chat(2, "             Sun: NO\n");
  }
  headas_chat(2, "Atmosphere depth: %4.2f [km]\n", parms->atmdpth);
  headas_chat(2, "          Method: %s\n", parms->method);
  if (strcasecmp(parms->method, "position")==0) {
    headas_chat(2, "       Tolerance: %4.2f [arc min]\n", parms->timesegerr);
  } else {
    headas_chat(2, "       Tolerance: %4.2f [sec]\n", parms->timesegerr);
  }
  if (parms->gtifile[0] == 0) {
    headas_chat(2, "             GTI: image TSTART/TSTOP\n");
  } else {
    headas_chat(2, "             GTI: %s\n", parms->gtifile);
  }
  if (parms->multfiles[0]) {
    headas_chat(2, "   Correct files: [MULTIPLY] %s\n", parms->multfiles);
  }
  if (parms->divfiles[0]) {
    headas_chat(2, "   Correct files: [DIVIDE]   %s\n", parms->divfiles);
  }
  headas_chat(2, "     Occultation: %s\n", parms->occultation);
  headas_chat(2, "------------------------------------------\n");
}

void summary(struct parm_struct *parms)
{
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "... DONE \n");
}  



/* ----------------------------------------------------------------- */


int batoccultmap_work(struct parm_struct *parms)
{
  struct image_struct *sky = 0;
  struct image_struct *scratch = 0;
  fitsfile *imgfile = 0, *outfile = 0, *sao = 0, *gtifile = 0;
  int nx, ny;
  int racol = 0, deccol = 0, timecol = 0, altcol = 0;
  int rollcol = 0, ramooncol = 0, decmooncol = 0, rasuncol = 0, decsuncol = 0;
  int raearthcol = 0, decearthcol = 0;
  int status = 0;
  long fpixel[2] = {1, 1};  /* For use with fits_write_pixnull() */
  char creator[FLEN_CARD];
  double earth_radius;
  
  /* Counters */
  long int  ntime = 0, i = 0, j = 0, k = 0, ntim_use = 0, nonulls = 0, nrows = 0;
  long int *twhere = 0;
  long int nrowsg = 0;

  struct img_wcs_struct img_wcs;

  FLOAT nullval = 1e38;
  int anynull = 0, gtirow = 0, ign = 0;
  int *tmp = 0;
  struct gti_struct gti;
  double tstart = 0, tstop = 0;

  double *gs = 0 , *ge = 0, *sin_rapix = 0 , *cos_rapix = 0, *sin_decpix = 0, *cos_decpix = 0, *img = 0;
  double *tmptime = 0, *tmpalt = 0, *tmproll = 0, *tmpra = 0, *tmpdec = 0;
  double *tmpmoonra = 0, *tmpmoondec = 0, *tmpsunra = 0, *tmpsundec = 0, *tmpearthra = 0, *tmpearthdec = 0;
  double *dt = 0, *time = 0, *alt = 0, *roll = 0, *ra = 0, *dec = 0;
  double *moon_ra = 0, *moon_dec = 0, *sun_ra = 0, *sun_dec = 0, *earth_ra = 0, *earth_dec = 0;
  double gstart = 0, gstop = 0;
  
  double error, uerror, dif, atmosphere, expo, cl1, cl2;
  double a1min, a1max, a2min, a2max, a3min, a3max, a, a1, a2, a3, a1sum, a2sum, a3sum, a1qsum, a2qsum, a3qsum, v1, v2, v3;
  int do_earth, do_moon, do_sun;

  char *header = 0;  /* For reading FITS header in one block */
  int nkeys = 0, nreject = 0, nwcs = 0;
  struct wcsprm *wcsdata = 0;

  /* Results for "CONTOUR" method */
  double *bradec = 0, *bijpix = 0;
  int nborderpix = 0;
  int first = 1, last = 0;
  
  banner(parms);
  earth_radius = parms->rearth;
  do_earth = parms->do_earth;
  do_moon = parms->do_moon;
  do_sun = parms->do_sun;
  
  /* ------------------------------- */
  /* Open and read the image */
  fits_open_image(&imgfile, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", parms->infile);
    return status;
  }
  
  /* Read the image */
  img_wcs.image = image_read_i(imgfile, -1, 0,
			       &nullval, &anynull, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not read image data from %s\n", 
	    parms->infile);
    return status;
  }
  
  nx = img_wcs.image->axes[0];
  ny = img_wcs.image->axes[1];
  headas_chat(5,  "       ... image dimensions (%d,%d) ...\n", nx,ny);
  
  /* ------------------------------- */
  /* Read the FITS header and parse the WCS keywords */
  fits_hdr2str(imgfile, 1, 0, 0, &header, &nkeys, &status);
  
  if (status || nkeys == 0) {
    fprintf(stderr, "ERROR: could not read image keywords\n");
    return status;
  }
  
  /* Read all WCS keywords using wcslib routine */
  status = wcspih(header, nkeys, 1, 0, &nreject, &nwcs, &wcsdata);
  if (status) {
    fprintf(stderr, "ERROR: parse of WCS keywords failed\n");
    if (status == 2) status = MEMORY_ALLOCATION;
    if (status == 1) status = NULL_INPUT_PTR;
    return status;
  }
  
  /* ------------------------------- */
  /* Read the good time interval extension */
  if (parms->gtifile[0]) {
    HDgti_init(&gti);
    HDgti_read(parms->gtifile, &gti, 0, 0, 0, 0, &gtifile, &status);
    
    if (status) {
      fprintf(stderr, "ERROR: could not open/read GTI file '%s'\n",
	      parms->gtifile);
      return status;
    }
    
    gs = gti.start;
    ge = gti.stop;
    nrowsg = gti.ngti;
    
  } else {
    /* Case: none -- read TSTART/TSTOP */
    fits_read_key(imgfile, TDOUBLE, "TSTART", &tstart, 0, &status);
    fits_read_key(imgfile, TDOUBLE, "TSTOP", &tstop, 0, &status);
    
    if (status) {
      fprintf(stderr, "ERROR: could not read image TSTART/TSTOP\n");
      return status;
    }
    
    gs = &tstart;
    ge = &tstop;
    nrowsg = 1;
  }
  
  /* ------------------------------- */
  /* Read the SAO (prefilter file) */
  fits_open_data(&sao, parms->saofile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: Unable to open %s for read access\n", 
	    parms->saofile);
    return status;
  }
  fits_get_num_rows(sao, &nrows, &status);
  if (status || nrows == 0) {
    fprintf(stderr, "ERROR: could not read number of rows in SAO file\n");
    if (status == 0) { return NEG_ROWS; }
  }
  
  fits_get_colnum(sao, CASEINSEN, RA_COLNAME,  &racol,  &status);
  fits_get_colnum(sao, CASEINSEN, DEC_COLNAME, &deccol, &status);
  fits_get_colnum(sao, CASEINSEN, TIME_COLNAME,  &timecol,  &status);
  fits_get_colnum(sao, CASEINSEN, ALT_COLNAME, &altcol, &status);
  fits_get_colnum(sao, CASEINSEN, ROLL_COLNAME, &rollcol, &status);
  fits_get_colnum(sao, CASEINSEN, RA_MOON_COLNAME,  &ramooncol,  &status);
  fits_get_colnum(sao, CASEINSEN, DEC_MOON_COLNAME, &decmooncol, &status);
  fits_get_colnum(sao, CASEINSEN, RA_SUN_COLNAME,  &rasuncol,  &status);
  fits_get_colnum(sao, CASEINSEN, DEC_SUN_COLNAME, &decsuncol, &status);
  fits_get_colnum(sao, CASEINSEN, RA_EARTH_COLNAME,  &raearthcol,  &status);
  fits_get_colnum(sao, CASEINSEN, DEC_EARTH_COLNAME, &decearthcol, &status);
  tmptime = (double *) malloc(sizeof(double)*nrows);
  tmpdec = (double *) malloc(sizeof(double)*nrows);
  tmpra = (double *) malloc(sizeof(double)*nrows);
  tmproll = (double *) malloc(sizeof(double)*nrows);
  tmpalt = (double *) malloc(sizeof(double)*nrows);
  tmpsunra = (double *) malloc(sizeof(double)*nrows);
  tmpsundec = (double *) malloc(sizeof(double)*nrows);
  tmpmoonra = (double *) malloc(sizeof(double)*nrows);
  tmpmoondec = (double *) malloc(sizeof(double)*nrows);
  tmpearthra = (double *) malloc(sizeof(double)*nrows);
  tmpearthdec = (double *) malloc(sizeof(double)*nrows);
  fits_read_col(sao, TDOUBLE, timecol, 1, 1, nrows, 0, tmptime, 0, &status);
  fits_read_col(sao, TDOUBLE, racol, 1, 1, nrows, 0, tmpra, 0, &status);
  fits_read_col(sao, TDOUBLE, deccol, 1, 1, nrows, 0, tmpdec, 0, &status);
  fits_read_col(sao, TDOUBLE, altcol, 1, 1, nrows, 0, tmpalt, 0, &status);
  fits_read_col(sao, TDOUBLE, rollcol, 1, 1, nrows, 0, tmproll, 0, &status);
  fits_read_col(sao, TDOUBLE, ramooncol, 1, 1, nrows, 0, tmpmoonra, 0, &status);
  fits_read_col(sao, TDOUBLE, decmooncol, 1, 1, nrows, 0, tmpmoondec, 0, &status);
  fits_read_col(sao, TDOUBLE, rasuncol, 1, 1, nrows, 0, tmpsunra, 0, &status);
  fits_read_col(sao, TDOUBLE, decsuncol, 1, 1, nrows, 0, tmpsundec, 0, &status);
  fits_read_col(sao, TDOUBLE, raearthcol, 1, 1, nrows, 0, tmpearthra, 0, &status);
  fits_read_col(sao, TDOUBLE, decearthcol, 1, 1, nrows, 0, tmpearthdec, 0, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not read data from SAO file\n");
    return status;
  }
  
  /* ------------------------------- */
  /* Initialize the output image */
  sky = image_init(nx, ny, 0);
  sky->nullval = nullval; 
  foreach_pixel(sky, i,{ if (img_wcs.image->data[i] != nullval) {
    sky->data[i] = 1.0; /*Default unocculted */
    nonulls++;} 
  else { sky->data[i] = nullval;}});
  
  
  /* ------------------------------- */
  /* Prepare for time segmentation */
  tmp = (int *) malloc(sizeof(int)*nrows);

  atmosphere =  parms->atmdpth;
  error = parms->timesegerr;
  uerror=error/60.0; /*from arc min to deg*/
  expo = 0;
  for (i=0; i<nrowsg; i++) { expo=expo+(ge[i]-gs[i]);}
  
  headas_chat(2,  "Exposure    : %10.4f [sec]\n", expo);
  if (nrowsg>1) {
    fprintf(stderr, "STDGTI has %ld rows.\n", nrowsg);
  }

  
  /* ------------------------------- */
  /* Sanity check on the earth radius parameter */
  for (i=0; i<nrows; i++) {
    if (earth_radius+atmosphere >= tmpalt[i]) {
      fprintf(stderr, 
	      "ERROR: the orbit radius (%f km) was inside the earth (plus atmosphere) (= %f km)!\n"
	      "       at time %f.\n"
	      "       Please check for the correct earth_radius and orbit file.\n",
	      tmpalt[i], earth_radius+atmosphere, tmptime[i]);
      status = -1;
      return status;
    }
  }

  /* ------------------------------- */
  /* Loop over each good time interval */
  for (gtirow=0; gtirow<nrowsg; gtirow++) {

    if (gtirow == (nrowsg-1)) last = 1;
    
    gstart=gs[gtirow];
    gstop =ge[gtirow];  
    
    headas_chat(2,  "%d STDGTI  start/stop : %10.4f / %10.4f dur : %10.4f [sec]\n",
		gtirow+1, gstart,gstop,gstop-gstart);
    
    /* Select the SAO */
    
    ntime = 0;
    for (i=0; i<nrows; i++) {
      if ( (tmptime[i]>=gstart)&&(tmptime[i]<=gstop) ) {
	/* Case where image spans at least one SAO sample */
	ntime++;
	tmp[i]=1;
      } else if ((i > 0) && 
		 (tmptime[i] >= gstop) && (tmptime[i-1] <= gstart)) {
	/* Case where image is shorter than SAO sampling time */
	ntime++;
	/* Pick nearest sample */
	if (fabs(tmptime[i]-gstop) < fabs(tmptime[i-1]-gstart)) {
	  tmp[i] = 1;
	} else {
	  tmp[i-1] = 1;
	  tmp[i] = 0;
	}
      } else {
	/* No sample yet */
	tmp[i]=0;
      }   
    }
    ign = 0;
    if (ntime == 0) {
      fprintf(stderr, "WARNING: No SAO data found inside STDGTI time window.\n");
      fprintf(stderr, "%d STDGTI row will be ignored and assumed unocculted.\n",gtirow+1);
      ign = 1;
    }
    headas_chat(5,  "Number of time intervals : %ld\n", ntime);
    
    if (ign == 0) {
      /* SAO diagnostic */
      twhere = (long int *) malloc(sizeof(long int)*ntime);
      j = 0;
      for (i=0; i<nrows; i++) {
        if (tmp[i]==1) {
	  twhere[j]=i;
	  j++;
        }
      }
      a1min=181;
      a1max=-181;
      a2min=91;
      a2max=-91;
      a3min=181;
      a3max=-181;
      a1sum = 0;
      a2sum = 0;
      a3sum = 0;
      a1qsum = 0;
      a2qsum = 0;
      a3qsum = 0;
      for (i=0; i<ntime; i++) {
        a=2*fmod(tmpra[twhere[i]],180)-tmpra[twhere[i]];
        a1sum=a1sum+a;
        a1qsum=a1qsum+a*a;
        if (a>a1max) { a1max=a;} 
        if (a<a1min) { a1min=a;}
        a=tmpdec[twhere[i]];
        a2sum=a2sum+a;
        a2qsum=a2qsum+a*a;      
        if (a>a2max) { a2max=a;}
        if (a<a2min) { a2min=a;}
        a=2*fmod(tmproll[twhere[i]],180)-tmproll[twhere[i]];
        a3sum=a3sum+a;
        a3qsum=a3qsum+a*a;
        if (a>a3max) { a3max=a;}
        if (a<a3min) { a3min=a;} 
      }
      a1=a1max-a1min;
      a2=a2max-a2min;
      a3=a3max-a3min;
      v1=(ntime*a1qsum-a1sum*a1sum)/(ntime*ntime-ntime);
      v2=(ntime*a2qsum-a2sum*a2sum)/(ntime*ntime-ntime);
      v3=(ntime*a3qsum-a3sum*a3sum)/(ntime*ntime-ntime);
      headas_chat(2,  "RA       max. diff. : %7.4f [deg] var. : %7.4f [deg]\n", a1, sqrt(v1));
      headas_chat(2,  "DEC      max. diff. : %7.4f [deg] var. : %7.4f [deg]\n", a2, sqrt(v2));
      headas_chat(2,  "WSC roll max. diff. : %7.4f [deg] var. : %7.4f [deg]\n", a3, sqrt(v3));

  
      /* ------------------------------- */
      /* Time segmentation */
      if (ntime>3) {
        i = 0;
        j = 1;  
        if (strcasecmp(parms->method, "position")==0) {
          while (ntime-i-j-1)  {
	    

	    /* This appears to be a weird calculation of just how different 
	     * the attitude/position is from the previous interval */
            dif = 0.1*(fabs(tmpra[twhere[i+j]]-tmpra[twhere[i]])+
		       fabs(tmpdec[twhere[i+j]]-tmpdec[twhere[i]])+
		       fabs(tmproll[twhere[i+j]]-tmproll[twhere[i]])+
		       deg2rad*fabs(asin((earth_radius+atmosphere)/tmpalt[twhere[i+j]])
				    -asin((earth_radius+atmosphere)/tmpalt[twhere[i]]))+
		       fabs(tmpsunra[twhere[i+j]]-tmpsunra[twhere[i]])+  
		       fabs(tmpsundec[twhere[i+j]]-tmpsundec[twhere[i]])+
		       fabs(tmpmoonra[twhere[i+j]]-tmpmoonra[twhere[i]])+  
		       fabs(tmpmoondec[twhere[i+j]]-tmpmoondec[twhere[i]])+   
		       fabs(tmpearthra[twhere[i+j]]-tmpearthra[twhere[i]])+  
		       fabs(tmpearthdec[twhere[i+j]]-tmpearthdec[twhere[i]]));
	    if (dif<uerror)  {
	      /* Only bother to do the calculations once for close attitudes/positions */
	      tmp[twhere[i+j]] = 0;
	      j++;
	    } else {
	      i = i+j;
	      j = 1;
	    }
          }
	  
        } else {
          if (error > 1)  {
            while (ntime-i-j-1)  {
              dif = fabs(tmptime[twhere[i+j]]-tmptime[twhere[i]]);
              if (dif<error)  {
                /* Only bother to do the calculations once for close times */
                tmp[twhere[i+j]] = 0;
                j++;
              } else {
                i = i+j;
                j = 1;
	      }
	    }
	  }
	}
      }
      
      ntim_use = 0;
      for (i=0; i<ntime; i++) {
        if (tmp[twhere[i]]==1) {
	  ntim_use++;
        }
      }
      headas_chat(2,  "Number of selected time intervals : %ld\n", ntim_use);
      
      /* ------------------------------- */
      /* Main Tables and indexes*/
      dt = (double *) malloc(sizeof(double)*ntim_use);
      time = (double *) malloc(sizeof(double)*ntim_use);
      roll = (double *) malloc(sizeof(double)*ntim_use);
      ra = (double *) malloc(sizeof(double)*ntim_use);
      dec = (double *) malloc(sizeof(double)*ntim_use);
      alt = (double *) malloc(sizeof(double)*ntim_use);
      sun_ra = (double *) malloc(sizeof(double)*ntim_use);
      sun_dec = (double *) malloc(sizeof(double)*ntim_use);
      moon_ra = (double *) malloc(sizeof(double)*ntim_use);
      moon_dec = (double *) malloc(sizeof(double)*ntim_use);
      earth_ra = (double *) malloc(sizeof(double)*ntim_use);
      earth_dec = (double *) malloc(sizeof(double)*ntim_use);
      
      /* Pull out the RA/Dec/Roll/etc for the transition points */
      j = 0;
      for (i=0; i<ntime; i++) {
        if (tmp[twhere[i]]==1) {
	  time[j]=tmptime[twhere[i]];
	  alt[j]=tmpalt[twhere[i]];
	  roll[j]=tmproll[twhere[i]];
	  ra[j]=tmpra[twhere[i]];
	  dec[j]=tmpdec[twhere[i]];   
	  sun_ra[j]=tmpsunra[twhere[i]];
	  sun_dec[j]=tmpsundec[twhere[i]];
	  moon_ra[j]=tmpmoonra[twhere[i]];
	  moon_dec[j]=tmpmoondec[twhere[i]];     
	  earth_ra[j]=tmpearthra[twhere[i]];
	  earth_dec[j]=tmpearthdec[twhere[i]];          
	  j++;
        }
      }
      
      /* ------------------------------- */
      /* Durations */
      if (ntim_use==1) {
	dt[0]=1;
      } else {
	dt[0]=(0.5*time[1]+0.5*time[0]-gstart)/expo;
	dt[ntim_use-1]=(gstop-0.5*time[ntim_use-1]-0.5*time[ntim_use-2])/expo;
	if (ntim_use>2) {
	  for (i=1; i<ntim_use-1; i++) {
	    dt[i]=(0.5*time[i+1]-0.5*time[i-1])/expo;
	  }
	}
      }
      
      /* Start the timing clock */
      cl1 = (double) clock()/ CLOCKS_PER_SEC;

      if (parms->alg == 1) {
	/* --------------------------------------------------------- */
	/* BEGIN of algorithm=IMAGE */

	if (first) {

	  sin_rapix = (double *) malloc(sizeof(double)*nonulls);
	  cos_rapix = (double *) malloc(sizeof(double)*nonulls);
	  sin_decpix = (double *) malloc(sizeof(double)*nonulls);
	  cos_decpix = (double *) malloc(sizeof(double)*nonulls);
	  img = (double *) malloc(sizeof(double)*nonulls);
	  
	  /* Default to fully exposed */
	  first = 0;
	  for (i=0; i<nonulls; i++) { img[i] = 1.0; }
	  status = allpixconvert(nonulls, nx, ny, img_wcs.image, nullval, img, wcsdata,
				 sin_rapix, cos_rapix, sin_decpix, cos_decpix);
	  if (status) return status;
	}

	doimagemap(nonulls, ntim_use, img, 
		   sin_rapix, cos_rapix,
		   sin_decpix, cos_decpix, dt, alt,
		   earth_ra, earth_dec, moon_ra, moon_dec,
		   sun_ra, sun_dec,
		   earth_radius, parms->rsun, parms->rmoon,
		   atmosphere, 
		   do_earth, do_moon, do_sun);
	
	/* Copy non-nulls into output */
	k = 0;
	for (j=0; j<ny; j++) {
	  for (i=0; i<nx; i++) {
	    if (img_wcs.image->datap[j][i] != nullval) {
	      sky->datap[j][i] = img[k];
	      k++;
	    } else {
	      sky->datap[j][i] = nullval;
	    }
	  }
	}


	/* End of algorithm=IMAGE */
	
      } else {

	/* --------------------------------------------------------- */
	/* BEGIN of algorithm=CONTOUR */

	/* Pre-compute the image border pixels */
	if (bradec == 0) bordcalc(sky, wcsdata, &nborderpix, &bradec, &bijpix);

	docontourmap(ntim_use, sky,
		     nborderpix, bradec, bijpix,
		     wcsdata,
		     dt, alt,
		     earth_ra, earth_dec, 
		     moon_ra, moon_dec,
		     sun_ra, sun_dec,
		     earth_radius, parms->rsun, parms->rmoon,
		     atmosphere, 
		     do_earth, do_moon, do_sun);

	/* Apply null values */
	foreach_pixel(sky, i, 
          { if (img_wcs.image->data[i] == nullval) sky->data[i] = nullval;});
      }
      
      cl2 = (double) clock()/ CLOCKS_PER_SEC;
      headas_chat(2,  "Elapsed time : %7.2f [sec] (main loop)\n", cl2-cl1);

    }
    free(twhere);
    free(dt);
    free(time);
    free(roll);
    free(ra);
    free(dec);
    free(alt);
    free(sun_ra);
    free(sun_dec);
    free(moon_ra);
    free(moon_dec);
    free(earth_ra);
    free(earth_dec);
  } /* for(gtirow) */
  
    /* ------------------------------- */
    /* End of GTI loop */
  
  if (parms->alg == 1) {
    if (sin_rapix) free(sin_rapix); sin_rapix = 0;
    if (cos_rapix) free(cos_rapix); cos_rapix = 0;
    if (sin_decpix) free(sin_decpix); sin_decpix = 0;
    if (cos_decpix) free(cos_decpix); cos_decpix = 0;
  } else {
    if (bradec) free(bradec); bradec = 0;
    if (bijpix) free(bijpix); bijpix = 0;
  }
    
  /* It's possible for 'sky' to be less than 0 due to round off
     errors.  Be sure to truncate it to zero. */
  foreach_pixel(sky, i, 
      { if (sky->data[i] <= 0) { sky->data[i] = 0; }} );
  
  /* ------------------------------- */
  /* Occultation "any" - use as a mask rather than an exposure map */
  
  if (strcasecmp(parms->occultation, "any")==0) {
    foreach_pixel(sky, i,
      {if ((sky->data[i]< 1)&&(sky->data[i]!=nullval)) sky->data[i] = 0;});
  }
  
  /* ------------------------------- */
  /* Write output image */
  headas_clobberfile(parms->outfile);
  fits_create_file(&outfile, parms->outfile, &status);
  fits_copy_hdu(imgfile, outfile, 30, &status);
  fits_write_pixnull(outfile, TFLTTYPE, fpixel, sky->axes[0]*sky->axes[1], 
		     sky->data, &(sky->nullval), &status);
  
  /* ------------------------------- */
  /* Update major image keywords in the output */
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		  "Program that created this FITS file", &status);
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP",
		  "Conforms to OGIP/GSFC standards", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "IMAGE", 
		  "Contains image data", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS2", "VIGNETTING",
		  "Contains fractional exposure map", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS3", "OCCULTATION",
		  "Contains occultation map", &status);
  fits_update_key(outfile, TSTRING, "IMATYPE", "EXPOSURE",
		  "Contains fractional exposure map", &status);
  /* FITS standard used to forbid EXTNAME in the primary HDU but now
     allows it (version 3.0; released 2008) */
  fits_update_key(outfile, TSTRING, "EXTNAME", "BAT_OCCIMG_1", 
		  "Name of extension", &status);

  fits_set_hdustruc(outfile, &status);

  if (status) {
    fprintf(stderr, "WARNING: could not write %s\n", parms->outfile);
  }     
  
  /* ------------------------------- */
  /* Append history keywords */
  if (status == 0) {
    status = HDpar_stamp(outfile, 0, &status);
    
    if (status != 0) {
      fprintf(stderr, "ERROR: could not write history to %s\n", parms->outfile);
    }
    
  }
  
  /* ------------------------------- */
  /* Copy the GTI */
  if (gtifile) {
    fits_copy_hdu(gtifile, outfile, 0, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not copy STDGTI extension to output\n");
      return status;
    }
  }
  
  /* ------------------------------- */
  /* Close files */
  if (imgfile) fits_close_file(imgfile, &status); 
  imgfile = 0; status = 0;
  if (gtifile) fits_close_file(gtifile, &status);
  gtifile = 0; status = 0;
  if (sao) fits_close_file(sao, &status);
  sao = 0; status = 0;
  
  if (header) free(header);
  header = 0;
  
  if (wcsdata) wcsvfree(&nwcs, &wcsdata);
  
  fits_close_file(outfile, &status);
  outfile = 0;

  /* ------------------------------- */
  /* Apply exposure correction to any images */
  status = imgcorrect(parms, sky, nullval, creator);
  if (status) return status;

  if (sky) image_free(sky);
  sky = 0;
  if (scratch) image_free(scratch);
  scratch = 0;

  summary(parms);
  return 0;
}


int batoccultmap(void)
{
  int status = 0;
  
  struct parm_struct parms;
  
  /* Register taskname and version. */
  
  set_toolname(taskname);
  set_toolversion(taskver);
  
  if ((status = batoccultmap_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }
  
  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];
  
  return batoccultmap_work(&parms);
  
}
