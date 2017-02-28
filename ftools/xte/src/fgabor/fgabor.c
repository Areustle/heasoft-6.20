#include "fitsio.h"
#include "cftools.h"
#include <math.h>
#include <time.h>
#include <string.h>

#define ERRMSG 255          /* Must be bigger than FLEN_ERRMSG */
#define POW2LIM 24

/* 
   20 August 2001 (v1.01)
   Now varying random seed value via system clock
   Tweaked gap-detection algorithm (now based on deltat/2.0 rather than fixed epsilon)
   added hidden parameter: sigma (overrides computed st.dev. characterizing fill/pad values)
   added hidden parameter: normalize (normalize output power array?)

   26 October 2006 (v1.02)
   Cleaned up code to shut up warnings (incl. moving frgrpar() to fgabsubs.c)
   Added WCS keywords to output FITS file
*/
   

void Fgabor()
{
  int fgrpar(char *,float *,float *,int *,float *,int *,char *,char *,char *,float *,float *,float *,int *,int *,int *);
  float *gabor_transpowr(float, float *, int, float *, int, int, int);
  float gasdev(int *);

  static char taskname[20] = "FGABOR_v1.02";
  static char timecol[FLEN_VALUE+1] = "Time";
  static char d = 'd';

  float fmin=0.0, fmax=0.0, window=0.0, maxgap=0.0, winfrac=0.0, sigma=0.0;
  int nfreq=0, time_units=0, clobber=0, quiet=0, normalize=0;

  char infile[FLEN_FILENAME+1], outfile[FLEN_FILENAME+1], extnam[FLEN_VALUE+1], ratecol[FLEN_VALUE+1];
  char *ratenulls0, *ratenulls, *timenulls, msg[ERRMSG], fitsout[FLEN_VALUE+2];
  char *ttype[1], *tform[1];
  char tkwunit[FLEN_VALUE+1], tcolunit[FLEN_VALUE+1], tunitname[FLEN_KEYWORD+1];
  char tmpstr[7], seedstr[5];

  int twopow, tanynul, ranynul, tcolnum, rcolnum, ncols;
  int pstat = 0, status = 0, seed = 0, lastrownull;

  long i, nrows, arraylen, nelements, naxes[2], nconsecnull;
  long rowsum = 0, fpixel = 1, naxis = 2, ninsert = 0, lastinsert = 0, thisgap = 0;

  float *freqs, *rate0, *rate, *gt;
  float deltat, datasum=0.0, datamean, datasd, fstep;
  float datavar=0.0;
  float crpix1=1.0,crpix2=1.0,ffv,fsp;

  double *timevalue, timespan, tstart, tstop;

  time_t tp;
  struct tm *tmp;

  fitsfile *ifp, *fptr;

  c_ptaskn(taskname);

  status=fgrpar(infile,&fmin,&fmax,&nfreq,&window,&time_units,ratecol,extnam,
		outfile,&maxgap,&winfrac,&sigma,&normalize,&quiet,&clobber); 
  if(status !=0){
    c_fcerr(" ");
    c_fcerr("Error reading parameters.");
    c_fcerr("Terminating...");
    exit(1);
  }

  if(!quiet){
    c_fcecho(" ");
    sprintf(msg,"Running %s\n========================",taskname);
    c_fcecho(msg);
  }

  fits_open_file(&ifp, infile, READONLY, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not open input lightcurve file");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_movnam_hdu(ifp, BINARY_TBL, extnam, 0, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    sprintf(msg,"Error moving to %s extension.",extnam);
    c_fcerr(msg);
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_get_num_rows(ifp, &nrows, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error getting number of rows");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_get_colnum(ifp, CASEINSEN, ratecol, &rcolnum, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    sprintf(msg,"Error finding column: %s",ratecol);
    c_fcerr(msg);
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_read_key(ifp, TFLOAT, "TIMEDEL", &deltat, 0, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading TIMEDEL keyword");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_read_key(ifp, TSTRING, "TIMEUNIT", tkwunit, 0, &pstat);
  if(pstat != 0){
    if(!quiet) c_fcecho("No TIMEUNIT keyword found. TIMEDEL units assumed to be seconds");
    strcpy(tkwunit, "NONE");
    pstat = 0;
  }

  fits_get_colnum(ifp, CASEINSEN, timecol, &tcolnum, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding column: Time");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_make_keyn("TUNIT", tcolnum, tunitname, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error constructing TUNITn");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_read_key(ifp, TSTRING, tunitname, tcolunit, 0, &pstat);
  if(pstat != 0){
    if(!quiet){
      sprintf(msg,"No %s keyword found. TIME units assumed to be seconds",tunitname);
      c_fcecho(msg);
    }
    strcpy(tcolunit, "NONE");
    pstat = 0;
  }

  if(!strncasecmp(&d,tkwunit,1)){
    if(!quiet) c_fcecho("Converting TIMEDEL value to seconds from days");
    deltat *= 86400.0; /* convert to seconds */
  }

  timevalue = (double *) malloc(nrows*sizeof(double));
  timenulls = (char *) malloc(nrows*sizeof(char));
  fits_read_colnull(ifp, TDOUBLE, tcolnum, 1L, 1L, nrows, timevalue, timenulls, &tanynul, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading Time column.");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  if(!strncasecmp(&d,tcolunit,1)){
    if(!quiet) c_fcecho("Converting TIME values to seconds from days");
    for(i=0;i<nrows;i++) *(timevalue+i) *= 86400.0;
  }
  tstart = *timevalue;
  tstop = *(timevalue+nrows-1);
  timespan = tstop - tstart;

  rate0 = (float *) malloc(nrows*sizeof(float));
  ratenulls0 = (char *) malloc(nrows*sizeof(char));
  fits_read_colnull(ifp, TFLOAT, rcolnum, 1L, 1L, nrows, rate0, ratenulls0, &ranynul, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    sprintf(msg,"Error reading column: %s",ratecol);
    c_fcerr(msg);
    fits_report_error(stderr, pstat);
    exit(1);
  }

  /*fits_close_file(ifp, &pstat);
    if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error closing input file");
    fits_report_error(stderr, pstat);
    exit(1);
    }*/
    
  if(!quiet){
    c_fcecho(" ");
    sprintf(msg,"Read %ld rows covering %7.2f seconds",nrows, timespan);
    c_fcecho(msg);
  }

  /* determine (and check) window */
  if(time_units){
    if(window < 0.0) window = winfrac * timespan;
    if(window > timespan) {
      sprintf(msg,"Error: window (%f s) larger than data timespan (%f s)",window,timespan);
      c_fcerr(msg);
      exit(1);
    }
    for(i=0;i<POW2LIM;i++) if(pow(2,i) > (timespan+window)/deltat) break;
    if(pow(2,i) < (timespan+window)/deltat){
      sprintf(msg,"Array length (data+window) exceeds limit (2^%2d)",POW2LIM);
      c_fcerr(msg);
      exit(1);
    }
  } else {
    if(window < 0.0) window = winfrac * nrows;
    if(window > nrows) {
      sprintf(msg,"Error: window (%ld rows) larger than dataset (%ld rows)",(long) window,nrows);
      c_fcerr(msg);
      exit(1);
    }
    for(i=0;i<POW2LIM;i++) if(pow(2,i) > (timespan/deltat + window)) break;
    if(pow(2,i) < (timespan/deltat + window)){
      sprintf(msg,"Array length (data+window) exceeds limit (2^%2d)",POW2LIM);
      c_fcerr(msg);
      exit(1);
    }
  }
  twopow = i;
  arraylen = pow(2,twopow);
  /*sprintf(msg,"window is %f, next power of two is %d, arraylen is %ld",window,twopow,arraylen);
    c_fcecho(msg);*/

  /* fill the real rate arrays accounting for missing times */
  rate = (float *) malloc(arraylen*sizeof(float));
  ratenulls = (char *) malloc(arraylen*sizeof(char));
  for(i=0;i<nrows;i++){
    if(i>0 && (*(timevalue+i) == *(timevalue+i-1))) { 
      /* fix identical timestamps (due to extractor bug)!? */
      if(!quiet){
	sprintf(msg,"Repairing consecutive identical time values (%f) at row %ld",*(timevalue+i),i);
	c_fcecho(msg);
      }
      *(timevalue+i) += deltat;
    }
    /* 17Aug2001
       Modified the check for time gaps to use a fraction of deltat instead
       of a fixed epsilon value. Also removed fabs() since gap-detection should
       always have the same sign */
    if(*(timevalue+i) - (*timevalue + deltat*(i+ninsert)) > deltat/2.0){
      /*if(!quiet){
	sprintf(msg,"Filling time gap after row %ld: ",i);
	c_fcecho(msg);
	sprintf(msg,"Got %f s, expected %f s, diff is %f (tolerance is %f)",*(timevalue+i),
	*timevalue+deltat*(i+ninsert),*(timevalue+i)-(*timevalue + deltat*(i+ninsert)),deltat/2.0);
	c_fcecho(msg);
	}*/
      *(rate+i+ninsert) = 0.0;
      *(ratenulls+i+ninsert) = TRUE;
      ninsert++;
      lastinsert = i;
      thisgap++;
      i--;
    } else {
      *(rate+i+ninsert) = *(rate0+i);
      *(ratenulls+i+ninsert) = *(ratenulls0+i);
      if (thisgap){
	if(!quiet){
	  sprintf(msg,"TIMEGAP: Filled %ld missing rows following row %ld",
		  thisgap,lastinsert);
	  c_fcecho(msg);
	}
	thisgap = 0;
	lastinsert = 0;
      }
    }
  }
  nrows += ninsert;

  free(rate0);
  free(ratenulls0);
  free(timevalue);
  free(timenulls);

  if(time_units){
    /*  fmin, fmax were input in Hz           */
    /*  and window in seconds, so             */
    /*  convert freqs to "cycles per dataset" */
    /*  and window to number of rows          */
    if(!quiet){
      c_fcecho("\nPreparing for Gabor Transform using");
      sprintf(msg,"a window of %6.1f sec (%6.1f timesteps)",window,window/deltat);
      c_fcecho(msg);
      sprintf(msg,"for %d frequencies between %7.4g and %7.4g Hz (%6.1f to %6.1f cycles per dataset)",
	      nfreq, fmin, fmax, fmin*timespan, fmax*timespan);
      c_fcecho(msg);
      sprintf(msg,"Estimated memory use: > %ld Mb",arraylen*nfreq*sizeof(float)/(1024*1024));
      c_fcecho(msg);
    }
    fmin = fmin * timespan;
    fmax = fmax * timespan;
    window = window / deltat;
  } else {
    if(!quiet){
      c_fcecho("\nPreparing for Gabor Transform using");
      sprintf(msg,"a window of %6.1f sec (%6.1f timesteps)",window*deltat,window);
      c_fcecho(msg);
      sprintf(msg,"for %d frequencies between %7.4g and %7.4g Hz (%6.1f to %6.1f cycles per dataset)",
	      nfreq, fmin/timespan, fmax/timespan, fmin, fmax);
      c_fcecho(msg);
      sprintf(msg,"Estimated memory use: > %ld Mb",arraylen*nfreq*sizeof(float)/(1024*1024));
      c_fcecho(msg);
    }
  }

  /* statistics: first compute mean... */
  lastrownull=0;
  nconsecnull=1;
  for(i=0;i<nrows;i++){
    if(*(ratenulls+i) != TRUE){
      datasum += *(rate+i);
      rowsum++;
      lastrownull=0;
      nconsecnull=1;
    } else { /* piggybacking on this loop to check gap sizes */
      if(lastrownull){
	nconsecnull++;
	if(nconsecnull > (long) (maxgap*nrows)){
	  sprintf(msg,"Error: maximum number of null rows (%ld) exceeded", (long) (maxgap*nrows));
	  c_fcerr(msg);
	  exit(1);
	}
      }
      lastrownull=1;
    }
  }
  datamean = datasum/rowsum;
  /*sprintf(msg,"mean rate for %ld rows is %f",rowsum,datamean); 
    c_fcecho(msg);*/

  /* ...now variance */
  for(i=0;i<nrows;i++){
    if(*(ratenulls+i) != TRUE){
      *(rate+i) -= datamean; 
      /* subtracting mean at the same time also */
      /* simplifies variance calc below!        */
      datavar += (*(rate+i))*(*(rate+i));
    }
  }
  datavar = datavar/(rowsum-1);
  datasd = sqrt(datavar);

  /* default sigma value of INDEF will return -6.66 */
  if (sigma >= 0.0) datasd=sigma;

  /* fill in null rates and pad end with random (gaussian) noise */
  while (seed == 0){
    /* trying to make sure that seed is a negative (non-zero) integer */
    time(&tp);
    tmp = gmtime(&tp);
    if (!tmp) tmp = localtime(&tp);  /* in case GMT not available */ 
    sprintf(tmpstr,"%ld",(long) tmp->tm_min*tmp->tm_sec*tmp->tm_sec);
    strncpy(seedstr,tmpstr+1,4);
    seed = -1 * atoi(seedstr);
  }
  /*printf("using seed value %d, datasd %f\n",seed,datasd);*/
  for(i=0;i<arraylen;i++){
    if(i < nrows){
      if(*(ratenulls+i) == TRUE) *(rate+i) = datasd * gasdev(&seed);
    }else{
      *(rate+i) = datasd * gasdev(&seed);
    }
  }

  free(ratenulls);

  fmin = fmin * arraylen/nrows; /* scale user-defined freqs to new dataset size */
  fmax = fmax * arraylen/nrows;
  freqs = (float *) malloc(nfreq*sizeof(float));
  fstep = (fmax-fmin)/(nfreq-1);
  for(i=0;i<nfreq;i++) *(freqs+i)=fmin+fstep*i;

  gt = gabor_transpowr(window,freqs,nfreq,rate,arraylen,nrows,normalize);

  /* free(rate); */

  naxes[0] = nrows;
  naxes[1] = nfreq;
  nelements = naxes[0] * naxes[1];
  if (clobber){
    sprintf(fitsout,"!%s",outfile);
  }else{
    sprintf(fitsout,"%s",outfile);
  }
  fits_create_file(&fptr, fitsout, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    sprintf(msg,"Error creating file: %s\n(to overwrite existing file set \"clobber\")",fitsout);
    c_fcerr(msg);
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_create_img(fptr, FLOAT_IMG, naxis, naxes, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error creating image");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_img(fptr, TFLOAT, fpixel, nelements, gt, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error writing to FITS image");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  free(gt);

  for(i=0;i<nfreq;i++) *(freqs+i) = *(freqs+i) * nrows/arraylen; /* restore original freqs */
  fits_write_date(fptr, &pstat);
  
  fits_write_key(fptr,TSTRING,"CTYPE1","Time","",&pstat);
  fits_write_key(fptr,TSTRING,"CUNIT1","s","",&pstat);
  fits_write_key(fptr,TFLOAT,"CRPIX1",&crpix1,"",&pstat);
  fits_write_key(fptr,TDOUBLE,"CRVAL1",&tstart,"First bin time value",&pstat);
  fits_write_key(fptr,TFLOAT,"CDELT1",&deltat,"Time bin width",&pstat);
  fits_write_key(fptr,TSTRING,"CTYPE2","Frequency","",&pstat);
  fits_write_key(fptr,TSTRING,"CUNIT2","Hz","",&pstat);
  fits_write_key(fptr,TFLOAT,"CRPIX2",&crpix2,"",&pstat);
  ffv=*freqs/timespan;
  fits_write_key(fptr,TFLOAT,"CRVAL2",&ffv,"First frequency value",&pstat);
  fsp=(*(freqs+nfreq-1)/timespan - *freqs/timespan)/(nfreq-1);
  fits_write_key(fptr,TFLOAT,"CDELT2",&fsp,"Frequency spacing",&pstat);

  sprintf(msg,"File written by %s",taskname);
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"Minimum Frequency: %7.4g Hz (%6.1f cycles/Tmax)",
	  *freqs/timespan,*freqs);
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"Maximum Frequency: %7.4g Hz (%6.1f cycles/Tmax)",
	  *(freqs+nfreq-1)/timespan,*(freqs+nfreq-1));
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"Number of Frequencies: %d",nfreq);
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"Window size: %6.1f sec (%6.1f timesteps)",window*deltat,window);
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"Data file: %s",infile);
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"Data times: %15.13g to %15.13g s",tstart,tstop);
  fits_write_history(fptr, msg, &pstat);
  if (normalize){
    sprintf(msg,"Output power was normalized");
    fits_write_history(fptr, msg, &pstat);
  } else {
    sprintf(msg,"No normalization of output power");
    fits_write_history(fptr, msg, &pstat);
  }
  sprintf(msg,"Fill/pad values randomly distributed (sigma = %f, ",datasd);
  if (sigma > 0){
    strcat(msg,"supplied)");
  } else {
    strcat(msg,"computed)");
  }
  fits_write_history(fptr, msg, &pstat);

  ttype[0] = (char *) malloc(10*sizeof(char));
  ttype[0] = "Frequency";
  tform[0] = (char *) malloc(2*sizeof(char));
  tform[0] = "E";
  fits_insert_btbl(fptr, nfreq, 1, ttype, tform, 0, 0, 0, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error inserting binary table");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  if(!time_units){
    fits_write_col(fptr, TFLOAT, 1, 1, 1, nfreq, freqs, &pstat);
    fits_write_key(fptr, TSTRING, "TUNIT1", "cycles per dataset", NULL, &pstat);
  }else{
    for(i=0;i<nfreq;i++) *(freqs+i) = *(freqs+i)/timespan;
    fits_write_col(fptr, TFLOAT, 1, 1, 1, nfreq, freqs, &pstat);
    fits_write_key(fptr, TSTRING, "TUNIT1", "Hz", NULL, &pstat);
  }
    
  sprintf(msg,"Extension created by %s",taskname);
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"These are the frequencies covered");
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"by the Gabor Transform, the result");
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"of which is in the previous extension.");
  fits_write_history(fptr, msg, &pstat);

  fits_copy_header(ifp, fptr, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error copying header");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_close_file(ifp, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error closing input file");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  fits_flush_file(fptr, &pstat);
  fits_get_num_cols(fptr, &ncols, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading number of columns");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  for(i=1;i<=ncols;i++){
    if(i != tcolnum && i != rcolnum) {
      fits_delete_col(fptr, i, &pstat);
      if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error deleting column");
	fits_report_error(stderr, pstat);
	exit(1);
      }
      tcolnum--;
      rcolnum--;
      i--;
      ncols--;
    }
  }
  /* after all this carnage let's revise the time/rate column #s */
  fits_get_colnum(fptr, CASEINSEN, timecol, &tcolnum, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding column: Time");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_get_colnum(fptr, CASEINSEN, ratecol, &rcolnum, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    sprintf(msg,"Error finding column: %s",ratecol);
    c_fcerr(msg);
    fits_report_error(stderr, pstat);
    exit(1);
  }

  /* timevalue array was freed earlier and nrows may be different now */
  timevalue = (double *) malloc(nrows*sizeof(double)); 
  for(i=0;i<nrows;i++) *(timevalue+i)=tstart+deltat*i;
    
  fits_write_col(fptr, TDOUBLE, tcolnum, 1, 1, nrows, timevalue, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error writing TIME column in output lc extension");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_col(fptr, TFLOAT, rcolnum, 1, 1, nrows, rate, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error writing RATE column in output lc extension");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  free(timevalue);
  free(rate);

  sprintf(msg,"MODIFIED by %s",taskname);
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"This extension contains a copy of the data");
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"on which the Gabor Transform was performed.");
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"The mean value has been subtracted and any NULL");
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"rates or time gaps have been replaced with");
  fits_write_history(fptr, msg, &pstat);
  sprintf(msg,"Gaussian-distributed random noise.");
  fits_write_history(fptr, msg, &pstat);
  if (sigma >= 0.0){
    sprintf(msg,"(using supplied sigma: %f)",sigma);
    fits_write_history(fptr, msg, &pstat);
  }else{
    sprintf(msg,"(using calculated sigma: %f)",datasd);
    fits_write_history(fptr, msg, &pstat);
  }
    

  fits_close_file(fptr, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error closing output file");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  if(!quiet){
    c_fcecho(" ");
    sprintf(msg,"========================\n%s completed successfully",taskname);
    c_fcecho(msg);
  }

  return;
}
