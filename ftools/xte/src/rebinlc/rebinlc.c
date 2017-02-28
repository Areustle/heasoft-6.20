/*
  FILENAME:    rebinlc.c
  author:      M. J. Tripicco
  date:        March 1999
*/

#include "fitsio.h"
#include "xpi.h"
#include "math.h"

#define ERRMSG 255          /* Must be bigger than FLEN_ERRMSG */
#define MAXC_FNAME 256      /* Define the size of the arrays for C */
#define MAXCOLS 16

int rbnlcpar();

typedef struct {
  double timedel;
  int multiple;
  int lctype;
  fitsfile *ffptr;
} lcinfo;

void Rebinlc()
{
  char msg[ERRMSG];
  char infile[MAXC_FNAME], outfile[MAXC_FNAME];
  char cols[MAXC_FNAME];
  char hduclas1[FLEN_VALUE], hduclas3[FLEN_VALUE];
  char tmpkwd[FLEN_KEYWORD];
  char tmpval[FLEN_VALUE];
  char datcol[FLEN_KEYWORD], coln[FLEN_KEYWORD];
  char lcdatcolname[MAXCOLS][FLEN_VALUE], lcerrcolname[MAXCOLS][FLEN_VALUE];
  char lcfexpcolname[MAXCOLS][FLEN_VALUE];
  
  int status, cfstat=0, clobber, multiple, ihdu, lctype;
  int lctimecolno=0, i, numcols;
  int lcdatcolno[MAXCOLS], lcerrcolno[MAXCOLS], lcfexpcolno[MAXCOLS];
  int ndatcol, nerrcol, nfexpcol;

  long numrows;
  unsigned long zero=0L;

  double timedel;

  fitsfile *ifp, *ofp;

  iteratorCol lcdata[3*MAXCOLS+1];
  int rbnlc_work_fn();

  lcinfo info;

  /* Define the TASK common block */
  static char taskname[20] = "REBINLC_v1.0";
  c_ptaskn(taskname);
  
  c_fcecho(" ");
  sprintf(msg,"Running %s\n========================\n",taskname);
  c_fcecho(msg);
  
  /* Read the Parameter file */
  status=rbnlcpar(infile,&multiple,outfile,cols,&clobber); 
  if(status !=0){
    c_fcerr("Could not complete rbnlcpar call");
    c_fcerr("aborting");
    exit(1);
  }
  c_fcecho(" ");
  info.multiple = multiple;

  /* Open the input lightcurve file */
  fits_open_file(&ifp, infile, READONLY, &cfstat);
  if(cfstat != 0){
    c_fcerr("Could not open input lightcurve file");
    fits_report_error(stderr, cfstat);
    exit(1);
  }
  
  fits_parse_extnum(infile, &ihdu, &cfstat);
  if(cfstat != 0){
    c_fcerr("Could not parse extension num");
    fits_report_error(stderr, cfstat);
    exit(1);
  }
  /* if user didn't specify, assume first extension */
  if (ihdu == -99) fits_movabs_hdu(ifp, 2, NULL, &cfstat);

  fits_read_key(ifp, TSTRING, "HDUCLAS1", hduclas1, NULL, &cfstat);
  if(cfstat != 0){
    c_fcerr("Error reading HDUCLAS1 keyword");
    fits_report_error(stderr, cfstat);
    exit(1);
  }
  if (strcmp(hduclas1,"LIGHTCURVE") && strcmp(hduclas1,"LIGHT CURVE")) {
    c_fcerr("Not an OGIP-compliant lightcurve...aborting");
    exit(1);
  }
  
  fits_read_key(ifp, TSTRING, "HDUCLAS3", hduclas3, NULL, &cfstat);
  if (cfstat != 0) {
    c_fcerr("Error reading HDUCLAS3 keyword");
    fits_report_error(stderr, cfstat);
    exit(1);
  }  
  if (!strcmp(hduclas3,"RATE")) {
    info.lctype = 1;
    strcpy(datcol,"RATE");
  } else if (!strcmp(hduclas3,"COUNT")) {
    info.lctype = 2;
    strcpy(datcol,"COUNTS");
  } else if (!strcmp(hduclas3,"FLUX")) {
    info.lctype = 3;
    strcpy(datcol,"FLUX"); /* ???? */
  } else {
    sprintf(msg,"HDUCLAS3 (%s) not RATE/COUNT/FLUX...aborting",hduclas3);
    c_fcerr(msg);
    exit(1);
  }
  
  fits_read_key(ifp, TDOUBLE, "TIMEDEL", &timedel, NULL, &cfstat);
  if(cfstat != 0){
    c_fcerr("Error reading TIMEDEL keyword");
    fits_report_error(stderr, cfstat);
    exit(1);
  }
  info.timedel = timedel;

  fits_get_colnum(ifp, CASEINSEN, "Time", &lctimecolno, &cfstat);
  if (cfstat != 0) {
    c_fcerr("Time column not found");
    fits_report_error(stderr, cfstat);
    exit(1);
  }

  /* identify the columns to be rebinned */
  if (!strncmp(cols,"-",1)) {
    fits_get_colname(ifp, CASEINSEN, datcol, lcdatcolname[0], &lcdatcolno[0], &cfstat);
    if (cfstat != 0) {
      sprintf(msg,"Error finding column (%s)...aborting",datcol);
      c_fcerr(msg);
      fits_report_error(stderr, cfstat);
      exit(1);
    }
    ndatcol=1;
    fits_get_colname(ifp, CASEINSEN, "ERROR", lcerrcolname[0], &lcerrcolno[0], &cfstat);
    if (cfstat != 0) {
      lcerrcolno[0] = -1;
      c_fcecho("ERROR column not found...skipping");
      fits_clear_errmsg();
      cfstat = 0;
    }
    fits_get_colname(ifp, CASEINSEN, "FRACEXP", lcfexpcolname[0], &lcfexpcolno[0], &cfstat);
    if (cfstat != 0) {
      lcfexpcolno[0] = -1;
      c_fcecho("FRACEXP column not found...skipping");
      fits_clear_errmsg();
      cfstat = 0;
    }
    for (i=1;i<MAXCOLS;i++) {
      fits_make_keyn(datcol, i+1, coln, &cfstat);
      fits_get_colname(ifp, CASEINSEN, coln, lcdatcolname[i], &lcdatcolno[i], &cfstat);
      if (cfstat != 0) {
	lcdatcolno[i] = -1;
	fits_clear_errmsg();
	cfstat=0;
	break;
      } else ndatcol++;
      fits_make_keyn("ERROR", i+1, coln, &cfstat);
      fits_get_colname(ifp, CASEINSEN, coln, lcerrcolname[i], &lcerrcolno[i], &cfstat);
      if (cfstat != 0) {
	lcerrcolno[i] = -1;
	fits_clear_errmsg();
	cfstat=0;
      }
      fits_make_keyn("FRACEXP", i+1, coln, &cfstat);
      fits_get_colname(ifp, CASEINSEN, coln, lcfexpcolname[i], &lcfexpcolno[i], &cfstat);
      if (cfstat != 0) {
	lcfexpcolno[i] = -1;
	fits_clear_errmsg();
	cfstat=0;
      }
    }
  } else {
    sprintf(msg,"using columns %s",cols);
    c_fcecho(msg);
    c_fcerr("I don't know how to do this yet!");
    exit(1);
  }
  
  /* set up the iterator columns */
  fits_iter_set_by_num(&lcdata[0], ifp, lctimecolno, TDOUBLE, InputCol);
  for (i=0;i<ndatcol;i++){
    fits_iter_set_by_num(&lcdata[3*i+1], ifp, lcdatcolno[i], TDOUBLE, InputCol);
    if (lcerrcolno[i] != -1) 
      fits_iter_set_by_num(&lcdata[3*i+2], ifp, lcerrcolno[i], TDOUBLE, InputCol);
    if (lcfexpcolno[i] != -1) 
      fits_iter_set_by_num(&lcdata[3*i+3], ifp, lcfexpcolno[i], TDOUBLE, InputCol);
  }
  
  /* setup output (rebinned lightcurve) file */
  if (clobber) remove(outfile);
  fits_create_file(&ofp, outfile, &cfstat);
  if (cfstat != 0) {
    sprintf(msg,"Error creating output file %s",outfile);
    c_fcerr(msg);
    fits_report_error(stderr, cfstat);
    exit(1);
  }
  info.ffptr = ofp;
  
  /* create dummy primary array */
  fits_create_img(ofp, 8, 0, 0, &cfstat);
  if (cfstat != 0) {
    c_fcerr("Error creating dummy primary array");
    fits_report_error(stderr, cfstat);
    exit(1);
  }
  
  /* copy lightcurve extension to output file and zero out NAXIS2 */
  fits_copy_header(ifp, ofp, &cfstat);
  if (cfstat != 0) {
    c_fcerr("Error copying HDU");
    fits_report_error(stderr, cfstat);
    exit(1);
  }
  fits_update_key(ofp, TULONG, "NAXIS2", &zero, NULL, &cfstat);
  if (cfstat != 0) {
    c_fcerr("Error updating NAXIS2");
    fits_report_error(stderr, cfstat);
    exit(1);
  }

  /* clear out column-related kwds */
  fits_get_num_cols(ofp, &numcols, &cfstat);
  for (i=1;i<=numcols;i++) {
    fits_delete_col(ofp, 1, &cfstat);
    if (cfstat != 0) {
      c_fcerr("Error deleting column");
      fits_report_error(stderr, cfstat);
      exit(1);
    }
  }

  /* set up new Time column */
  fits_insert_col(ofp, 1, "Time", "D", &cfstat);
  fits_write_key(ofp, TSTRING, "TUNIT1", "s", "physical unit of field", &cfstat);
  if (cfstat != 0) {
    c_fcerr("Error initializing Time column");
    fits_report_error(stderr, cfstat);
    exit(1);
  }

  for (i=0;i<ndatcol;i++){
    fits_insert_col(ofp, 3*i+2, lcdatcolname[i], "E", &cfstat);
    fits_insert_col(ofp, 3*i+3, lcerrcolname[i], "E", &cfstat);
    fits_insert_col(ofp, 3*i+4, lcfexpcolname[i], "E", &cfstat);
    if (!strncmp(lcdatcolname[i],"RATE",4)) {
      fits_make_keyn("TUNIT", 3*i+2, tmpkwd, &cfstat);
      fits_write_key(ofp, TSTRING, tmpkwd, "counts/s", "physical unit of field", &cfstat);
      fits_make_keyn("TUNIT", 3*i+3, tmpkwd, &cfstat);
      fits_write_key(ofp, TSTRING, tmpkwd, "counts/s", "physical unit of field", &cfstat);
    } else {
      fits_make_keyn("TUNIT", 3*i+2, tmpkwd, &cfstat);
      fits_write_key(ofp, TSTRING, tmpkwd, "counts", "physical unit of field", &cfstat);
      fits_make_keyn("TUNIT", 3*i+3, tmpkwd, &cfstat);
      fits_write_key(ofp, TSTRING, tmpkwd, "counts", "physical unit of field", &cfstat);
    }
  }

  fits_iterate_data(3*ndatcol+1, lcdata, 0L, 0L, rbnlc_work_fn, (void *) &info, &status);
  if(status != 0){
    sprintf(msg,"Iterator returned status %d",status);
    c_fcerr(msg);
    /* exit(1); */
  }

  sprintf(msg, "Lightcurve was rebinned using a multiple of %d by %s", info.multiple, taskname);
  fits_write_history(ofp, msg, &cfstat);
  fits_write_date(ofp, &cfstat);
  if(cfstat != 0){
    c_fcerr("Error writing HISTORY/DATE keywords");
    fits_report_error(stderr, cfstat);
    exit(1);
  }

  fits_close_file(ifp, &cfstat);
  if(cfstat != 0){
    c_fcerr("Could not close input lightcurve file");
    fits_report_error(stderr, cfstat);
    exit(1);
  }

  fits_write_chksum(ofp, &cfstat);
  fits_close_file(ofp, &cfstat);
  if(cfstat != 0){
    c_fcerr("Could not close output lightcurve file");
    fits_report_error(stderr, cfstat);
    exit(1);
  }
}

int rbnlc_work_fn(long totaln, long offset, long firstn, long nvalues,
		  int narrays, iteratorCol *data, void *userPointer)
{
  extern double sqrt();
  
  int i, j, binnum, status=0;

  long newbins, activebin;
  static long rowswritten;

  static double newtimedel;
  static double *lctime, *lcdata[MAXCOLS], *lcerr[MAXCOLS], *lcfexp[MAXCOLS];
  double *rbntimes, *rbndata[MAXCOLS], *rbnerr[MAXCOLS], *rbnfexp[MAXCOLS];
  static double carryovertime, carryoverdata[MAXCOLS], carryovererr[MAXCOLS], carryoverfexp[MAXCOLS];

  static lcinfo *info;

  double epsilon=1.0e-16;

  char msg[ERRMSG];
      
  if (firstn == 1){    /* initialize stuff the first time through */
    lctime = (double *) fits_iter_get_array(&data[0]);
    for (i=0;i<(narrays-1)/3;i++) {
      lcdata[i] = (double *) fits_iter_get_array(&data[3*i+1]);
      lcerr[i] = (double *) fits_iter_get_array(&data[3*i+2]);
      lcfexp[i] = (double *) fits_iter_get_array(&data[3*i+3]);
      carryoverdata[i] = 0.0;
      carryovererr[i] = 0.0;
      carryoverfexp[i] = 0.0;
    }
    carryovertime = *(lctime+1);
    info = (lcinfo *) userPointer;
    newtimedel = info->timedel * info->multiple;
    fits_update_key(info->ffptr, TDOUBLE, "TIMEDEL", &newtimedel, NULL, &status);
    rowswritten = 0;
    /*printf("this is a type %d lightcurve\n",info->lctype);
      printf("totaln is %ld\n",totaln);
      printf("offset is %ld\n",offset);
      printf("narrays is %ld\n",narrays);
      printf("new timedel is %f sec\n",newtimedel);*/
  }
  
  /*printf("\n");
  printf("firstn is %ld\n",firstn);
  printf("nvalues is %ld\n",nvalues);
  printf("first lctime is %lf\n",*(lctime+1));
  printf("last lctime is %lf\n",*(lctime+nvalues));
  for (i=0;i<(narrays-1)/3;i++) {
    printf("null lcdata (%d) is %lg\n",i,*(lcdata[i]+0));
    printf("first lcdata (%d) is %lg\n",i,*(lcdata[i]+1));
    printf("last lcdata (%d) is %lg\n",i,*(lcdata[i]+nvalues));
    printf("null lcerr (%d) is %lg\n",i,*(lcerr[i]+0));
    printf("first lcerr (%d) is %lg\n",i,*(lcerr[i]+1));
    printf("last lcerr (%d) is %lg\n",i,*(lcerr[i]+nvalues));
    printf("null lcfexp (%d) is %lg\n",i,*(lcfexp[i]+0));
    printf("first lcfexp (%d) is %lg\n",i,*(lcfexp[i]+1));
    printf("last lcfexp (%d) is %lg\n",i,*(lcfexp[i]+nvalues));
  }
  */

  /* setup output arrays (first bin is "carried over" from last iteration) */
  newbins = (int) ((*(lctime+nvalues) - carryovertime)/newtimedel) + 1;
  rbntimes = (double *) malloc(newbins*sizeof(double));
  *rbntimes = carryovertime;
  for (i=0;i<(narrays-1)/3;i++) {
    rbndata[i] = (double *) malloc(newbins*sizeof(double));
    *rbndata[i] = carryoverdata[i];
    rbnerr[i] = (double *) malloc(newbins*sizeof(double));
    *rbnerr[i] = carryovererr[i];
    rbnfexp[i] = (double *) malloc(newbins*sizeof(double));
    *rbnfexp[i] = carryoverfexp[i];
  }
  for (i=1;i<newbins;i++) {
    *(rbntimes+i) = carryovertime + i * newtimedel;
    for (j=0;j<(narrays-1)/3;j++) {
      *(rbndata[j]+i) = 0.0;
      *(rbnerr[j]+i) = 0.0;
      *(rbnfexp[j]+i) = 0.0;
    }
  }

  /* accumulate output arrays */
  for (i=1;i<=nvalues;i++) {
    binnum = (int) ((*(lctime+i) - carryovertime)/newtimedel);
    /* if (binnum == 0 || binnum == 1 || binnum == newbins-1 || binnum == newbins-2)
       printf("time %f / binnum %d / newtime %f\n",*(lctime+i), binnum, *(rbntimes+binnum));*/
    for (j=0;j<(narrays-1)/3;j++) {
      if (*(lcfexp[j]+i) > 1.0) { /* this shouldn't happen, but it does... */
	sprintf(msg,"FRACEXP for input row %d (%f) greater than 1.0 (%f)",i,*(lctime+i),*(lcfexp[j]+i));
	c_fcecho(msg);
	*(lcfexp[j]+i) = 1.0;
      }
      if (*(lcdata[j]+i) != DOUBLENULLVALUE) { /* if data column is INDEF then skip the row */
	if (info->lctype == 1) { /* RATE lightcurve */
	  *(rbndata[j]+binnum) = *(rbndata[j]+binnum) + *(lcdata[j]+i) * info->timedel * *(lcfexp[j]+i);
	  *(rbnerr[j]+binnum) = *(rbnerr[j]+binnum) + 
	    pow((*(lcerr[j]+i) * info->timedel * *(lcfexp[j]+i)),(double) 2.0);
	  *(rbnfexp[j]+binnum) = *(rbnfexp[j]+binnum) + *(lcfexp[j]+i) * info->timedel;
	} else { /* COUNTS lightcurve */
	  *(rbndata[j]+binnum) = *(rbndata[j]+binnum) + *(lcdata[j]+i);
	  *(rbnerr[j]+binnum) = *(rbnerr[j]+binnum) + pow(*(lcerr[j]+i),(double) 2.0);
	  *(rbnfexp[j]+binnum) = *(rbnfexp[j]+binnum) + *(lcfexp[j]+i);
	}
	/* *(rbnerr[j]+binnum) = *(rbnerr[j]+binnum) + *(lcerr[j]+i) * *(lcerr[j]+i); */
      }
    }
  }

  /* normalize and get rid of empty rows */
  activebin = 0;
  for (i=0;i<newbins-1;i++) { /* skip final newbin to carryover to next iteration (see below) */
    if ((*(rbnfexp[0]+i) - 0.0) > epsilon) { /* not counting on two reals being equal! */
      *(rbntimes+activebin) = *(rbntimes+i);
      for (j=0;j<(narrays-1)/3;j++){
	if (info->lctype == 1) { /* RATE column */
	  *(rbndata[j]+activebin) = *(rbndata[j]+i) / *(rbnfexp[j]+i);
	  *(rbnerr[j]+activebin) = sqrt(*(rbnerr[j]+i)) / *(rbnfexp[j]+i);
	  *(rbnfexp[j]+activebin) = *(rbnfexp[j]+i) / newtimedel;
	} else { /* COUNTS column */
	  *(rbndata[j]+activebin) = *(rbndata[j]+i);
	  *(rbnerr[j]+activebin) = sqrt(*(rbnerr[j]+i));
	  *(rbnfexp[j]+activebin) = *(rbnfexp[j]+i) / info->multiple;
	}
	/* *(rbnerr[j]+activebin) = sqrt(*(rbnerr[j]+i)); */
      }
      activebin++;
    }
  }

  /* save the carryovers... */
  carryovertime = *(rbntimes+newbins-1);
  for (j=0;j<(narrays-1)/3;j++){
    carryoverdata[j] = *(rbndata[j]+newbins-1);
    carryovererr[j] = *(rbnerr[j]+newbins-1);
    carryoverfexp[j] = *(rbnfexp[j]+newbins-1);
  }

  /* ...but on last iteration we want to write that final bin */
  if ((firstn + nvalues - 1) == totaln) { 
    if ((carryoverfexp[0] - 0.0) > epsilon) { /* still not counting on two reals being equal! */
      *(rbntimes+activebin) = carryovertime;
      for (j=0;j<(narrays-1)/3;j++){
	if (info->lctype == 1) { /* RATE column */
	  *(rbndata[j]+activebin) = carryoverdata[j] / carryoverfexp[j];
	  *(rbnerr[j]+activebin) = sqrt(carryovererr[j]) / carryoverfexp[j];
	  *(rbnfexp[j]+activebin) = carryoverfexp[j] / newtimedel;
	} else { /* COUNTS column */
	  *(rbndata[j]+activebin) = carryoverdata[j];
	  *(rbnerr[j]+activebin) = sqrt(carryovererr[j]);
	  *(rbnfexp[j]+activebin) = carryoverfexp[j] / info->multiple;
	}
      }
      activebin++;
    }
  }
  
  newbins = activebin;
  
  /* write new time column */
  fits_write_col(info->ffptr, TDOUBLE, 1, (long) rowswritten+1, 1L, newbins, rbntimes, &status);
  if (status != 0) {
    c_fcerr("Error writing Time column");
    fits_report_error(stderr, status);
    exit(1);
  }

  /* write the new data/error/fracexp columns */
  for (j=0;j<(narrays-1)/3;j++) {
    fits_write_col(info->ffptr, TDOUBLE, 3*j+2, (long) rowswritten+1, 1L, newbins, rbndata[j], &status);
    if (status != 0) {
      c_fcerr("Error writing data column");
      fits_report_error(stderr, status);
      exit(1);
    }
    fits_write_col(info->ffptr, TDOUBLE, 3*j+3, (long) rowswritten+1, 1L, newbins, rbnerr[j], &status);
    if (status != 0) {
      c_fcerr("Error writing error column");
      fits_report_error(stderr, status);
      exit(1);
    }
    fits_write_col(info->ffptr, TDOUBLE, 3*j+4, (long) rowswritten+1, 1L, newbins, rbnfexp[j], &status);
    if (status != 0) {
      c_fcerr("Error writing fracexp column");
      fits_report_error(stderr, status);
      exit(1);
    }
  }
  
  rowswritten = rowswritten + newbins;
  
  /* free up the memory used by the local arrays */
  free(rbntimes);
  for (i=0;i<(narrays-1)/3;i++) {
    free(rbndata[i]);
    free(rbnerr[i]);
    free(rbnfexp[i]);
  }

  return 0;
}

int rbnlcpar(fin,mult,fout,cols,clobber) 
char *fin, *fout, *cols;
int *clobber, *mult;
{
  int BufLen_2 = 255;
  int parstat;
  char text[FLEN_STATUS];

  parstat = *clobber = 0;
  
  Uclgst("infile", fin, &parstat);
  if(parstat != 0){
    c_fcerr("Could not get input filename");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsi("multiple", mult, &parstat);
  if(parstat != 0){
    c_fcerr("Could not get multiple");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgst("outfile", fout, &parstat);
  if(parstat != 0){
    c_fcerr("Could not get output filename");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgst("columns", cols, &parstat);
  if(parstat != 0){
    c_fcerr("Could not get columns parameter");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsb("clobber",clobber, &parstat);
  if(parstat != 0){
    c_fcerr("Could not get clobber.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  return parstat;
}
