/*
  FILENAME:    gtisum.c
  author:      M. J. Tripicco
  date:        August 1998

  version: 1.0  27 Aug 1998
  version: 1.1  12 Aug 1999:  fixed case where there is a GTI table with zero rows
  version: 1.2  30 Sep 2008:  revised output formatting for large exponents
  version: 1.3  07 May 2010:  fixed iterator initialization that crashed long gtis
*/

#include <stdio.h>
#include <string.h>
#include "xpi.h"
#include "fitsio.h"

#define ERRMSG 255          /* Must be bigger than FLEN_ERRMSG */

#ifndef TRUE
#define TRUE  1             /* Define a logical TRUE value */
#endif

#ifndef FALSE
#define FALSE 0             /* Define a logical FALSE value */
#endif

typedef struct
{
  double sumtime;
  double timefirst;
  double timelast;
  int numintervals;
} gtinfo;

int sum_gti_ext();
void writeit();
int pargtisum();

int ofused=FALSE, hdunum=0, chatty;
FILE *ofp;
char msg[ERRMSG];

void Gtisum()
{
  char *gtifile, *outfile, *tmpfile;
  int ii, status=0, pstat=0, nhdu=0;
  fitsfile *gtifp;
  static char taskname[20] = "GTISUM_v1.3";

  c_ptaskn(taskname);

  sprintf(msg,"Running %s\n========================",taskname);
  writeit(msg);
  
  gtifile  = (char *) malloc(sizeof(char)*FLEN_FILENAME);
  outfile  = (char *) malloc(sizeof(char)*FLEN_FILENAME);
  tmpfile  = (char *) malloc(sizeof(char)*FLEN_FILENAME);

  status=pargtisum(gtifile,outfile,&chatty); 
  if(status !=0){
    c_fcerr(" ");
    c_fcerr("Fatal error reading parameter file");
    exit(1);
  }
  
  /* uppercase for comparison but keep original filename */
  strcpy(tmpfile,outfile);      
  fits_uppercase(tmpfile);
  if (strcmp(tmpfile,"STDOUT")){
    ofused=TRUE;
    if ((ofp=fopen(outfile,"w"))==NULL){
      c_fcerr(" ");
      c_fcerr("Cannot open output file");
      exit(1);
    }
  }
  free(tmpfile);
  free(outfile);

  fits_open_file(&gtifp, gtifile, READONLY, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Fatal error opening file");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  writeit("\nExt.    HDUCLAS1    Num.Intervals    GTI Start           GTI End       GoodTime (sec)");

  fits_parse_extnum(gtifile, &hdunum, &pstat);
  if (hdunum == -99){     /* no hdu specified: loop over all hdus */
    fits_get_num_hdus(gtifp, &nhdu, &pstat);
    for (ii=1;ii<=nhdu;ii++){
      hdunum=ii;
      fits_movabs_hdu(gtifp, ii, 0L, &pstat);
      sum_gti_ext(gtifp);
    }
  } else {                /* specified hdu: operate on this hdu only */
    sum_gti_ext(gtifp);
  } 
  free(gtifile);

  fits_close_file(gtifp, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    strcpy(msg,"Could not close file ");
    strcat(msg,gtifile);
    c_fcerr(msg);
    fits_report_error(stderr, pstat);
    exit(1);
  }

  if (ofused) fclose(ofp);
}

void writeit(char *text)
{
  if (ofused) {
    fprintf(ofp, "%s\n", text);
  } else {
    c_fcecho(text);
  }
}

int sum_gti_ext(fitsfile *ffp)
{
  char *hduclas1;
  int status=0, colstart=0, colstop=0;
  iteratorCol gtidata[2];
  int my_work_fn();
  gtinfo info_gti_ext;
 
  hduclas1 = (char *) malloc(sizeof(char)*FLEN_VALUE);

  fits_read_keyword(ffp, "HDUCLAS1", hduclas1, NULL, &status);

  if (!strstr(hduclas1,"GTI")){
    sprintf(msg," %-5d%8s",hdunum-1,hduclas1);
    writeit(msg);
    return 0;
  }

  fits_get_colnum(ffp, CASEINSEN, "START", &colstart, &status);
  if (status != 0) {
    writeit("START column not found");
    return 0;
  }

  fits_get_colnum(ffp, CASEINSEN, "STOP", &colstop, &status);
  if (status != 0) {
    writeit("STOP column not found");
    return 0;
  }

  fits_iter_set_by_num(&gtidata[0], ffp, colstart, TDOUBLE, InputCol);
  fits_iter_set_by_num(&gtidata[1], ffp, colstop, TDOUBLE, InputCol);
  info_gti_ext.numintervals=0;

  fits_iterate_data(2, gtidata, 0L, 0L, my_work_fn,(void*) &info_gti_ext, &status);

  if (info_gti_ext.numintervals == 0.0) { /* a GTI extension with zero rows */
    info_gti_ext.timefirst=0.0;
    info_gti_ext.timelast=0.0;
    info_gti_ext.sumtime=0.0;
  }
  sprintf(msg," %-5d%8s       %-8d   %15.15g    %15.15g    %-12.11g",
	  hdunum-1,hduclas1,info_gti_ext.numintervals,info_gti_ext.timefirst,
	  info_gti_ext.timelast,info_gti_ext.sumtime);
  writeit(msg);
  free(hduclas1);
	    
  return status;
}

int my_work_fn(long totaln, long offset, long firstn, long nvalues,
	       int narrays, iteratorCol *data, void *userPointer)
{
  int ii;
  double *start, *stop;
  static gtinfo *current_gti;
  
  start = (double *) fits_iter_get_array(&data[0]);
  stop = (double *) fits_iter_get_array(&data[1]);

  if (firstn == 1){    /* initialize stuff the first time through */
    current_gti = (gtinfo *) userPointer;
    current_gti->sumtime=0.0;
    current_gti->numintervals=0;
    current_gti->timefirst=9e99; /* big enough !?!? */
    current_gti->timelast=-9e99; /* small enough !?!? */
  }

  if (chatty){
    sprintf(msg,"\nDetailed output for extension #%d:",hdunum-1);
    writeit(msg);
  }
  for (ii = 1; ii <= nvalues; ii++){
    current_gti->sumtime += (stop[ii]-start[ii]);
    if ((stop[ii]-start[ii]) > 0) current_gti->numintervals++;
    if (start[ii] < current_gti->timefirst) current_gti->timefirst=start[ii];
    if (stop[ii] > current_gti->timelast) current_gti->timelast=stop[ii];
    if (chatty) {
      sprintf(msg,"row %d, start %lg, stop %lg, time %lg",
	      ii,start[ii],stop[ii],stop[ii]-start[ii]);
      writeit(msg);
    }
  }

  if (chatty)
    writeit("\nExt.    HDUCLAS1    Num.Intervals    GTI Start           GTI End       GoodTime (sec)");

  return 0;
}

int pargtisum(char* filename, char *outfile, int *chatty) 
{
  int parstat=0;
  int BufLen_2 = 160;

  char text[FLEN_STATUS];
    
  Uclgst("gtifile", filename, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get gtifile parameter");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgst("outfile", outfile, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get outfile parameter");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsb("verbose", chatty, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get verbose parameter");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  return parstat;
}

/* This code is needed by IRAF */
  
#ifdef vms
#define F77CALL gtisu
#endif
#ifdef unix
#define F77CALL gtisu_
#endif

void F77CALL() 
{ 
  void Gtisum();
  
  Gtisum(); 
}


  
