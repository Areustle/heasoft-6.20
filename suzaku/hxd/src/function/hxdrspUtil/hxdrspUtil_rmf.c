/*
 * hxdrspUtil_rmf.c
 *      create rmf fits file
 *         version 0.0.1 2003-10-15,  created by Y.Terada
 *         version 0.0.2 2003-10-18, modefied by Y.Terada
 *         version 0.0.3 2003-11-23, modefied by Y.Terada
 *                 for NeXT SGD dummy response
 *         version 0.0.4 2004-08-06, by Y.Terada
 *                 for HXD team, included into astroe_dir (hxd home)
 *         version 0.1.0 2005-04-12, by Y.Terada
 *                  included into hxdrspUtil
 *         version 0.2.3 2013-10-10, by Y.Terada
 *                  warnings
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include <math.h>
#include "fitsio.h"
#include "hxdrspUtil.h"

static char *pname = "hxdrspUtil_rmf";
static char version[]   = "version 0.2.3";

static fitsfile* hxdrspUtil_rmf_fp = NULL;


#define DEBUG 1

/* -----------------------------------------------------------
 *          Initialization
 * -----------------------------------------------------------*/
int 
hxdrspUtil_rmf_init( HxdRmfInfo* com, HxdRmfData *data_rsp, 
		     HxdRmfPIDef *data_ch, 
		     char *telescop, char *instrume, char *detnam,
		     int detchans,  int logscale, 
		     float energy_range_min, float energy_range_max){
  int status = HXDRSPUTIL_OK;
  int i,j,k;

  for (i=0;i<MKDMYRMF_MAX_NAXIS;i++){
    data_rsp->row_num[i]    = 0;
    data_rsp->energy_low[i] = 0;
    data_rsp->energy_high[i] = 0;
    for (j=0;j<MKDMYRMF_MAX_CHANNEL;j++){
      data_rsp->pi[j][i] = 0;
    }
  }
  data_rsp->detchans = detchans;

  for (k=0;k<MKDMYRMF_MAX_CHANNEL;k++){
    data_ch->row_num[k]    = 0;
    data_ch->energy_min[k] = 0;
    data_ch->energy_max[k] = 0;
  }

  data_ch->detchans = detchans;
  data_ch->logscale = logscale;
  data_ch->energy_range_min = energy_range_min;
  data_ch->energy_range_max = energy_range_max;

  sprintf(com->telescop, telescop);
  sprintf(com->instrume, instrume);
  sprintf(com->detnam,   detnam);

  return status;
}

/* -----------------------------------------------------------
 *          Make Table, (PI definition)
 * -----------------------------------------------------------*/
int 
hxdrspUtil_rmf_define_pi  ( HxdRmfPIDef* data_ch){
  int status = HXDRSPUTIL_OK;
  int chan;

  if (data_ch->logscale){ /*** Log Scale ***/
    double log_e_min, log_e_max;
    float step;
    double log_min, log_max;

    log_e_min = log10((double)data_ch->energy_range_min);
    log_e_max = log10((double)data_ch->energy_range_max);
    step = (log_e_max - log_e_min) / data_ch->detchans;
    for (chan = 1; chan <= data_ch->detchans; chan ++){
      data_ch->row_num[chan]    = chan;
      log_min = log_e_min + step*(chan-1);
      log_max = log_e_min + step*chan;
      data_ch->energy_min[chan] = pow(10.0, log_min);
      data_ch->energy_max[chan] = pow(10.0, log_max);
    }
    
  } else {           /*** Lin Scale ***/
    float step;
    step = (data_ch->energy_range_max - data_ch->energy_range_min) 
           / data_ch->detchans;
    for (chan = 1; chan <= data_ch->detchans; chan ++){
      data_ch->row_num[chan]    = chan;
      data_ch->energy_min[chan] = data_ch->energy_range_min + step*(chan-1);
      data_ch->energy_max[chan] = data_ch->energy_range_min + step*chan;
    }
  }

#if DEBUG
  for (chan = 1; chan <= data_ch->detchans; chan ++)
    fprintf(stderr, "PI[%d] %f keV -- %f keV\n", data_ch->row_num[chan], 
            data_ch->energy_min[chan], data_ch->energy_max[chan]);
#endif

  return status;
}


/* -----------------------------------------------------------
 *          Make Table, (Response) I.dummy
 * -----------------------------------------------------------*/
int 
hxdrspUtil_rmf_mkdmy_data ( HxdRmfData*  data_rsp, HxdRmfPIDef *data_ch){
  int status = HXDRSPUTIL_OK;
  int row_id, pi_id;
  int dmpnum = (data_rsp->detchans/20);

  if (DEBUG) fprintf(stderr, "Create DUMMY RSP ");
  data_rsp->irow = data_rsp->detchans;

  for (row_id=1; row_id<=data_rsp->irow; row_id++){
    data_rsp->row_num[row_id]     = row_id;
    data_rsp->energy_low[row_id]  = data_ch->energy_min[row_id];
    data_rsp->energy_high[row_id] = data_ch->energy_max[row_id];

    for(pi_id=1; pi_id<=data_rsp->detchans; pi_id++){
      /*      data_rsp->pi[pi_id][row_id] = 1.0; */
      if(row_id == pi_id)  data_rsp->pi[row_id][pi_id] = 1.0;
    }
    if(dmpnum!=0) if(!(row_id%dmpnum)) fprintf(stderr, ".");
  }

  if (DEBUG) fprintf(stderr, " done.\n");
  return status;
}

/* -----------------------------------------------------------
 *          Make Table, (Response) II. read data
 * -----------------------------------------------------------*/
int 
hxdrspUtil_rmf_read_ascii ( char* asciiname, HxdRmfData* data_rsp){
  int status = HXDRSPUTIL_OK;
  int row_id = 0;
  int pi_id;
  FILE *ascii_fp;
  double e_low, e_high, rsp;

  /*** File Open ***/
  ascii_fp = fopen(asciiname, "r");
  if (ascii_fp == NULL) {
    fprintf(stderr, "file (%s) open error\n", asciiname);
    status = HXDRSPUTIL_NG; 
    return status;
  }

  /*** File Read ***/
  if (DEBUG) fprintf (stderr, "Reading ASCII FILE (%s) ", asciiname);
  while (1) {
    row_id ++; /** row_id starts 1 **/
    fscanf(ascii_fp, "%lf %lf", &e_low, &e_high);
    data_rsp->energy_low[row_id] = e_low;
    data_rsp->energy_high[row_id] = e_high;

    for(pi_id=1; pi_id<=data_rsp->detchans; pi_id++){
      fscanf(ascii_fp, "%lf", &rsp);
      /*      data_rsp->pi[pi_id][row_id] = rsp; */
      data_rsp->pi[row_id][pi_id] = rsp;
    }
    fscanf(ascii_fp, "\n");

    data_rsp->row_num[row_id] = row_id;
    if (feof(ascii_fp)) {
      data_rsp->irow = row_id;
      break;
    }
    if (row_id > MKDMYRMF_MAX_NAXIS){
      fprintf(stderr, "Warning: %s, line is over %d\n", 
              asciiname, MKDMYRMF_MAX_NAXIS);
      break;
    }
    if (DEBUG) if (row_id % 10) fprintf (stderr, ".");
  }
  if (DEBUG) fprintf (stderr, "done\n");

  /*** File Close ***/
  if ( fclose(ascii_fp) ) {
    fprintf(stderr, "file (%s) close error\n", asciiname);
    status = HXDRSPUTIL_NG; 
    return status;
  }

  /*** Normal End ***/
  return status;
}

/* -----------------------------------------------------------
 *          Dump Table, (Response)
 * -----------------------------------------------------------*/
int 
hxdrspUtil_rmf_dump_data  ( HxdRmfData* data_rsp){
  int status = HXDRSPUTIL_OK;
  int row_id, pi_id;

  fprintf (stderr, "ID E_LOW E_HIGH PI[1], PI[2],...PI[%d]\n",
           data_rsp->detchans);

  for (row_id=1; row_id<=data_rsp->irow; row_id++){
    fprintf (stderr, "%d %f %f: ", data_rsp->row_num[row_id],
             data_rsp->energy_low[row_id], data_rsp->energy_high[row_id]);
    for(pi_id=1; pi_id<=data_rsp->detchans; pi_id++){
      fprintf (stderr, "%f ",data_rsp->pi[pi_id][row_id]);
    }
    fprintf (stderr, "\n");
  }
  fprintf (stderr, "Total ROW = %ld\n", data_rsp->irow);


  return status;
}

/* -----------------------------------------------------------
 *          Check Table, (Response)
 * -----------------------------------------------------------*/
int 
hxdrspUtil_rmf_check_data ( HxdRmfData* data_rsp, HxdRmfPIDef *data_ch){
  int status = HXDRSPUTIL_OK;
  int row_id;
  int pi_id;

  if( data_rsp->energy_low[1] > data_ch->energy_range_min ){
    fprintf (stderr, 
             "%s Undefined PI range (RSP E_LOW %3.2f > PI min %3.2f keV)\n", 
             pname, data_rsp->energy_low[1], data_ch->energy_range_min);
    status = HXDRSPUTIL_NG;
  }
  if( data_rsp->energy_high[data_rsp->irow] < data_ch->energy_range_max ){
    fprintf (stderr, 
             "%s Undefined PI range (RSP E_HIGH %3.2f > PI max %3.2f keV)\n",
             pname, data_rsp->energy_high[data_rsp->irow],
             data_ch->energy_range_max);
    status = HXDRSPUTIL_NG;
  }

  for (row_id=1; row_id<=data_rsp->irow; row_id++){
    if (data_rsp->energy_low[row_id] < 0.0){
      fprintf (stderr, "%s line=%d invalid value ENERG_LO (%e)\n", 
               pname, row_id, data_rsp->energy_low[row_id]);
      status = HXDRSPUTIL_NG;
    }
    if (data_rsp->energy_high[row_id] < 0.0){
      fprintf (stderr, "%s line=%d invalid value ENERG_HI (%e)\n", 
               pname, row_id, data_rsp->energy_high[row_id]);
      status = HXDRSPUTIL_NG;
    }
    for(pi_id=1; pi_id<=data_rsp->detchans; pi_id++){
      if (data_rsp->pi[pi_id][row_id] < 0.0){
        fprintf (stderr, "%s line=%d invalid value RSP PI[%d] (%f)\n", 
                 pname, row_id, pi_id, data_rsp->pi[pi_id][row_id]);
        status = HXDRSPUTIL_NG;
      }
    }
    if (data_rsp->energy_low[row_id] > data_rsp->energy_high[row_id]){
      fprintf (stderr, 
               "%s line=%d ENERG_LO is larger than ENERG_HI (%e -> %e)\n", 
               pname, row_id, 
               data_rsp->energy_low[row_id], data_rsp->energy_high[row_id]);
      status = HXDRSPUTIL_NG;
    }
    if (data_rsp->energy_low[row_id] < data_rsp->energy_high[row_id-1]){
      fprintf (stderr, "%s line=%d Energy Range is overlapped (%e -> %e)\n", 
               pname, row_id, 
               data_rsp->energy_low[row_id], data_rsp->energy_high[row_id-1]);
      status = HXDRSPUTIL_NG;
    }
    if (data_rsp->energy_low[row_id-1] > data_rsp->energy_low[row_id]){
      fprintf (stderr, 
               "%s line=%d the order of ENERG_LO is swapped (%e -> %e)\n", 
               pname, row_id, 
               data_rsp->energy_low[row_id], data_rsp->energy_low[row_id+1]);
      status = HXDRSPUTIL_NG;
    }
  }

  /*
  if (status == HXDRSPUTIL_OK) {
    fprintf(stderr, "Input file (%s) check OK\n", pname );
  } else {
    fprintf(stderr, "Please Check the INPUT FILE (%s)\n", pname );
  }
  */

  if (DEBUG) fprintf (stderr, "done.\n");
  return status;
}

/* ===============================================================
 *              Create the RMF Fits file
 * ===============================================================*/
int 
hxdrspUtil_rmf_create_fits( char* rmfname, HxdRmfInfo* com, 
			    HxdRmfData* data_rsp,
			    HxdRmfPIDef *data_ch){
  int status = HXDRSPUTIL_OK;
  int istat = 0;

  static char *ttype1[] = {
    "ENERG_LO", "ENERG_HI", "N_GRP", "F_CHAN", "N_CHAN", "MATRIX"
  };
  static char matrix_form[] = "1024E";
  static char *tform1[] = {
    "1E", "1E", "1I", "1I", "1I", matrix_form
  }; 
  static char *tunit1[] = {
    "keV", "keV", " ", " ", " ", " "
  };
  static char *ttype2[] = {
    "CHANNEL", "E_MIN", "E_MAX"
  };
  static char *tform2[] = {
    "1I", "1E", "1E"
  };
  static char *tunit2[] = {
    " ", "keV", "keV"
  };
  

  static char  telecomment[80] = "mission/satellite name";
  static char  instcomment[80] = "instrument name";
  static char  detcomment[80]  = "detector name";

  /* int bitpix = 8;*/
  /* int naxis = 0; */
  /* long nrow=0;   */
  long naxes[1];
  int ncol;
  char creater[80];
  char origin[80];
  
  if (DEBUG) fprintf (stderr, "Creating RMF Fits (%s), ", rmfname);
 

  /**** create Fits file *****/
  if (DEBUG) fprintf (stderr, "primary, ");
   if( fits_create_file(&hxdrspUtil_rmf_fp, rmfname, &istat) ) {
    fprintf(stderr, "%s:fits_create_file failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  if( fits_write_grphdr(hxdrspUtil_rmf_fp, 1, 8, 0, naxes, 0, 1, 1, &istat) ){
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  sprintf(creater, "%s @", getpwuid(getuid())->pw_gecos);
  gethostname(creater+strlen(creater), sizeof(creater)-strlen(creater));
  sprintf(origin, "%s: %s", pname, version);

  fits_write_key_str(hxdrspUtil_rmf_fp, "CREATOR", creater, 
		     "Creater of this fits", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "ORIGIN", origin, 
		     "Tool name and version", &istat);

  /**** create First Extenstion *****/ 
 if (DEBUG) fprintf (stderr, "1st EXT, ");
  if( fits_create_hdu(hxdrspUtil_rmf_fp, &istat) ){
    fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  sprintf(matrix_form,"%d",data_rsp->detchans);
  strcat(matrix_form,"E");
  *(tform1+5)=matrix_form;
  if (DEBUG) fprintf(stderr, "matrix_form = %s\n",*(tform1+5));

  ncol = sizeof(ttype1)/sizeof(*ttype1);
  if ( fits_write_btblhdr(hxdrspUtil_rmf_fp, data_rsp->irow, ncol, 
                          ttype1, tform1, tunit1, "MATRIX", 0, &istat) ) {
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  
  /*
  if(fits_create_img(hxdrspUtil_rmf_fp, bitpix, naxis, &nrow, &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  ncol = sizeof(ttype1)/sizeof(*ttype1);
  if( fits_create_tbl( hxdrspUtil_rmf_fp, BINARY_TBL, nrow, ncol,
                       ttype1, tform1, tunit1, "MATRIX", &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  */
  
  fits_write_key_str(hxdrspUtil_rmf_fp, "TELESCOP", 
                     com->telescop, telecomment, &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "INSTRUME", 
                     com->instrume, instcomment, &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "DETNAM",
                     com->detnam, detcomment, &istat);
  fits_write_key_lng(hxdrspUtil_rmf_fp, "DETCHANS",  data_rsp->detchans,
                     "total number of detector channels", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "CHANTYPE", "PHA",
                     "Detector Channel Type in use (PHA or PI)", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUCLASS", "OGIP", 
                     "format devised by the OGIP", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUCLAS1", "RESPONSE",
                     "dataset associated with instrument response", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUVERS1", "1.0.0", 
                     "version of HDUCLAS1 format", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUCLAS2", "RSP_MATRIX",
                     "dataset is a Response Matrix", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUVERS2", "1.1.0", 
                     "version of HDUCLAS2 format", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "RMFVERSN", "1992a",
                     "OGIP classification of FITS format", &istat);
  
  /**** create Secound Extenstion *****/
  if (DEBUG) fprintf (stderr, "2nd EXT, ");
  if( fits_create_hdu(hxdrspUtil_rmf_fp, &istat) ){
    fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
  }

  ncol = sizeof(ttype2)/sizeof(*ttype2);
  if ( fits_write_btblhdr(hxdrspUtil_rmf_fp, data_ch->detchans, ncol, 
                          ttype2, tform2, tunit2, "EBOUNDS", 0, &istat) ) {
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  
  /*
  if(fits_create_img(hxdrspUtil_rmf_fp, bitpix, naxis, &nrow, &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  ncol = sizeof(ttype2)/sizeof(*ttype2);
  if( fits_create_tbl( hxdrspUtil_rmf_fp, BINARY_TBL, nrow, ncol,
                       ttype2, tform2, tunit2, "EBOUNDS", &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  */
   
 fits_write_key_str(hxdrspUtil_rmf_fp, "TELESCOP", 
                     com->telescop, telecomment, &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "INSTRUME", 
                     com->instrume, instcomment, &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "DETNAM", com->detnam, 
		     detcomment, &istat);
  fits_write_key_lng(hxdrspUtil_rmf_fp, "DETCHANS",  data_ch->detchans,
                     "total number of detector channels", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "CHANTYPE", "PHA",
                     "Detector Channel Type in use (PHA or PI)", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUCLASS", "OGIP", 
                     "format devised by the OGIP", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUCLAS1", "RESPONSE",
                     "dataset associated with instrument response", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUVERS1", "1.0.0", 
                     "version of HDUCLAS1 format", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUCLAS2", "EBOUNDS",
                     "dataset is a Response Matrix", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "HDUVERS2", "1.1.0", 
                     "version of HDUCLAS2 format", &istat);
  fits_write_key_str(hxdrspUtil_rmf_fp, "RMFVERSN", "1992a",
                     "OGIP classification of FITS format", &istat);
  
  if (DEBUG) fprintf (stderr, "done.\n");
  return status;
}

/* ===============================================================
 *              Write the table into the RMF Fits file
 * ===============================================================*/
int 
hxdrspUtil_rmf_write_fits ( HxdRmfData* data_rsp,  HxdRmfPIDef *data_ch){
  int status = HXDRSPUTIL_OK;
  int hdutype;
  long firstelem = 1;
  long nelements = 1;
  short n_grp  = 1;
  /*  short f_chan = 1; */
  short f_chan = 0;
  int row_id, pi_id;
  int istat = 0;
  int dmpnum1=data_rsp->irow/20;
  int dmpnum2=data_rsp->detchans/20;

  /************ 1st Extenstion *************/
  if (DEBUG) fprintf (stderr, "         1st EXT ");
  if (fits_movabs_hdu(hxdrspUtil_rmf_fp, RMF_FITS_MATRIX_EXTENSION, 
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            pname, RMF_FITS_MATRIX_EXTENSION, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  for (row_id=1; row_id<=data_rsp->irow; row_id++){
    fits_write_col_flt(hxdrspUtil_rmf_fp, 1, row_id, firstelem, nelements, 
                       &data_rsp->energy_low[row_id], &istat);
    fits_write_col_flt(hxdrspUtil_rmf_fp, 2, row_id, firstelem, nelements, 
                       &data_rsp->energy_high[row_id],&istat);
    fits_write_col_sht(hxdrspUtil_rmf_fp, 3, row_id, firstelem, nelements,
                       &n_grp, &istat);
    fits_write_col_sht(hxdrspUtil_rmf_fp, 4, row_id, firstelem, nelements,
                       &f_chan,&istat);
    fits_write_col_int(hxdrspUtil_rmf_fp, 5, row_id, firstelem, nelements,
                       &data_rsp->detchans, &istat);
    /*    fits_write_col_flt(hxdrspUtil_rmf_fp, 6, row_id, firstelem, */
    /*	       data_rsp->detchans, &data_rsp->pi[1][row_id], &istat); */
fits_write_col_flt(hxdrspUtil_rmf_fp, 6, row_id, firstelem, 
    	       data_rsp->detchans, &data_rsp->pi[row_id][1], &istat);
    if(istat){
      fprintf(stderr, "%s: fits_write_col_** failed row=%d (%d)\n", 
              pname, row_id, istat);
      status = HXDRSPUTIL_NG;
      return status;
    }
    if (DEBUG) if(dmpnum1!=0) if(!(row_id%dmpnum1)) fprintf(stderr, ".");
  }
  if (DEBUG) fprintf(stderr, " done\n");

  /************ 2nd Extenstion *************/
  if (DEBUG) fprintf (stderr, "         2nd EXT ");
  if (fits_movabs_hdu(hxdrspUtil_rmf_fp, RMF_FITS_EBOUND_EXTENSION, 
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            pname, RMF_FITS_EBOUND_EXTENSION, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  for(pi_id=1; pi_id<=data_ch->detchans; pi_id++){
    short channel = (short) data_ch->row_num[pi_id];
    fits_write_col_sht(hxdrspUtil_rmf_fp, 1, pi_id, firstelem, nelements,
                       &channel, &istat);
    fits_write_col_flt(hxdrspUtil_rmf_fp, 2, pi_id, firstelem, nelements,
                       &data_ch->energy_min[pi_id], &istat);
    fits_write_col_flt(hxdrspUtil_rmf_fp, 3, pi_id, firstelem, nelements,
                       &data_ch->energy_max[pi_id], &istat);
    /*
    fprintf(stderr, "PI[%d] %f keV -- %f keV\n", channel, 
            data_ch[pi_id].energy_min, data_ch[pi_id].energy_max);
    */
    if(istat){
      fprintf(stderr, "%s: fits_write_col_** failed row=%d (%d)\n", 
              pname, pi_id, istat);
      status = HXDRSPUTIL_NG;
      return status;
    }
    if (DEBUG) if(dmpnum2!=0) if(!(pi_id%dmpnum2)) fprintf(stderr, ".");
  }
  if (DEBUG) fprintf(stderr, " done. ");

  if (DEBUG) fprintf (stderr, "         Write RMF done.\n");

  return status;
}

/* ===============================================================
 *                        Close the RMF File
 * ===============================================================*/
int 
hxdrspUtil_rmf_close_fits ( void ){
  int status = HXDRSPUTIL_OK;
  int istat = 0;
  int hdutype;

  if (fits_movabs_hdu(hxdrspUtil_rmf_fp, RMF_FITS_PRIMARY_HDU, 
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            pname, RMF_FITS_PRIMARY_HDU, istat);
    status = HXDRSPUTIL_NG;
  }
  
  if( fits_write_date(hxdrspUtil_rmf_fp, &istat) ){
    fprintf(stderr, "%s:fits_write_date failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  if (fits_write_chksum(hxdrspUtil_rmf_fp, &istat)) {
    fprintf(stderr, "%s:fits_write_chksum failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
  }
 
  if(fits_close_file(hxdrspUtil_rmf_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  if (DEBUG) fprintf (stderr, "done.\n");

  return status;
}

