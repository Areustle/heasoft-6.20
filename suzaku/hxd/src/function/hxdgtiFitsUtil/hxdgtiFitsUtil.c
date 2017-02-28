/*
 * hxdgtiFitsUtil
 *    version 0.0.x  by M.Sugiho, 1999-09-03
 *    version 0.1.0  by Y.Terada, 2005-06-28
 *    version 2.0.0  by Y.Terada, 2006-09-08
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "anl.h"
#include "atFunctions.h"

#include "hxdgtiFitsUtil.h"

#define pname "hxdgtiFitsUtil"

static char *gti_ttype[GTI_HDU_NFIELDS] = {
  /* gti HDU column name */
  "START", "STOP", 
};

int hxdgtiFits_createGTIxtention( fitsfile *fp, int hdunum, HXD_GTI gti,
				 HXD_STD_KEYS stdkeys, int *status){
  
  int gti_tbltype = BINARY_TBL;
  int gti_tfields = GTI_HDU_NFIELDS;
  char gti_name[] = "GTI";
  
  int i;
  
  char *gti_tform[GTI_HDU_NFIELDS] = {
    /* gti HDU column data size */
    "1D", "1D", 
  };
  char *gti_tunit[GTI_HDU_NFIELDS] = {
    /* gti HDU unit */
    "s", "s"
  };
  
  if (fits_create_tbl(fp, gti_tbltype, gti.row , gti_tfields, gti_ttype,
		      gti_tform, gti_tunit, gti_name, status)) {
    fprintf(stderr, "%s:fits_create_tbl failed (%d)\n", pname, *status);
    return ANL_NG;
  }

  if (hxdFitsHeader_writeHXDStdKeys(fp, stdkeys, 0, status) != ANL_OK) {
    fprintf(stderr,"%s: Error in writeHXDStdKey() (status=%d)\n",pname,
	    *status);
    return ANL_NG;
  }
  
  for (i=0;i<gti.row;i++){
    
    if (fits_write_col_dbl(fp, 1, i+1, 1, 1, &gti.start[i], status)){
      fprintf(stderr, "%s: GTI: fits_write_col_dbl(%d)\n", pname,
	      *status);
      return ANL_NG;
    }
    
    if (fits_write_col_dbl(fp, 2, i+1, 1, 1, &gti.stop[i], status)){
      fprintf(stderr, "%s: GTI: fits_write_col_dbl(%d)\n", pname,
	      *status);
      return ANL_NG;
    }
  }
  
  /* update time-related keywords in #2 HDU */
  if (hxdFitsHeader_updateStdTimeKeys(fp, stdkeys, hdunum, status) != ANL_OK){
    fprintf(stderr,"%s: Error in updateStdTimeKeys() (status=%d)\n",pname,
	    *status);
    return ANL_NG;
  }
  
  /* add addition GTI-related keywords */
  if (fits_modify_key_lng(fp, "NAXIS2", gti.row, "&", status)) {
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }

  if (fits_update_key_lng(fp, "NEVENTS", stdkeys.nevents, "Number of events",
			  status)) {
    fprintf(stderr, "%s: ERZROR in fits_update_key() NEVENTS (%d)\n",
	    pname, *status);
    return ANL_NG;
  }
  
  /* write HDUCLAS1 */
  if (fits_update_key_str(fp, "HDUCLAS1", "GTI",
			  "File contains Good Time Intervals", status)) {
    fprintf(stderr, "%s: fits_update_key_str  HDUCLAS1 failed (%d)\n",
	    pname, *status);
    return ANL_NG;
  }
  if (fits_update_key_str(fp, "HDUCLAS2", "STANDARD",
			 "File contains Good Time Intervals", status)) {
    fprintf(stderr, "%s: fits_write_key_str  HDUCLAS2 failed (%d)\n",
	    pname, *status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "MTYPE1", "TIME", "DM keyword: Data type",
			 status)) {
    fprintf(stderr, "%s: fits_write_key_str MTYPE1  failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "MFORM1", "START,STOP",
			 "DM keyword: names of the start and stop columns",
			 status)) {
    fprintf(stderr, "%s: fits_write_key_str  MFORM1 failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "METYP1", "R","\
DM keyword: data descriptor type: Range, binned data", status)) {
    fprintf(stderr, "%s: fits_write_key_str  METYP1 failed (%d)\n", pname,\
	    *status);
	return ANL_NG;
  }
    
  /* data and checksum */
  if (fits_write_date(fp, status)) {
    fprintf(stderr,"%s: fits_write_date() failed (status=%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  
  if (fits_write_chksum(fp, status)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  
  return ANL_OK;
}


int hxdgtiFits_updateGTI( HXD_GTI *gti, double stop, double start )
{
  
  if (gti->start == NULL) {
    gti->start = malloc(sizeof(double)*2);
    gti->stop = malloc(sizeof(double)*2);
    if (gti->start == NULL || gti->stop == NULL) {
      fprintf(stderr, "%s: ERROR allocating memory for GTI struct\n",pname);
      return ANL_NG;
    }
    gti->row = 1;
  } else {
    gti->start = (double*)realloc(gti->start, (gti->row+1)*sizeof(double));
    gti->stop = (double*)realloc(gti->stop, (gti->row+1)*sizeof(double));
    if (gti->start == NULL || gti->stop == NULL) {
      fprintf(stderr, "%s: ERROR re-allocating memory for GTI struct\n",
	      pname);
      return ANL_NG;
    }
  }
  
  gti->stop[gti->row -1] = stop;
  gti->start[gti->row] = start;
  
  gti->row ++;
  
  /*{
    int i;

    printf("%f %f\n",start, stop);
    
    for(i=1;i<gti->row;i++){
      printf("start: %d %f\n",i,gti->start[i]);
    }
    for(i=0;i<gti->row-1;i++){
      printf("stop:  %d %f\n",i,gti->stop[i]);
    }    
  }*/
  
  return ANL_OK;
  
}

int hxdgtiFits_finalizeGTI ( HXD_GTI *gti, double start, double stop )
{

  int i;
  
  if (gti->start == NULL) {
    gti->start = malloc(sizeof(double));
    gti->stop = malloc(sizeof(double));
    
    gti->start[0] = start;
    gti->stop[0] = stop;
    gti->row = 1;
  } else {
    gti->start[0] = start;
    gti->stop[gti->row -1] = stop;
  }
  
  gti->ontime=0.0;
  
  for( i=0;i<gti->row;i++){
    gti->ontime += gti->stop[i] - gti->start[i];
  }
  
  return ANL_OK;
  
}


/* DEBUG:
 * when an AEpacket comes in, it should have a finite TIME (>exposure time)
 * width;
 * thus, for more accurate solution, this func() should be modified */

int hxdgtiFits_checkGTI (HXD_GTI *gti, double aetime) 
{
  if (gti->start == NULL) {
    gti->start = (double*)malloc(sizeof(double));
    gti->stop = (double*)malloc(sizeof(double));
    if (gti->start == NULL || gti->stop == NULL) {
      fprintf(stderr, "%s: ERROR allocating memory for GTI struct\n",pname);
      return ANL_NG;
    }
    
    gti->start[0]=aetime;  
    gti->stop[0]=aetime;

    gti->ontime=0;
    gti->row=0;

  } else {

    if (gti->stop[gti->row] + GTI_GAP_TOLERANCE < aetime) {
      gti->ontime += (gti->stop[gti->row] - gti->start[gti->row]);
      gti->row++;
      
      gti->start = (double*)realloc(gti->start, (gti->row+1)*sizeof(double));
      gti->stop = (double*)realloc(gti->stop, (gti->row+1)*sizeof(double));
      if (gti->start == NULL || gti->stop == NULL) {
	fprintf(stderr, "%s: ERROR re-allocating memory for GTI struct\n",
		pname);
	return ANL_NG;
      }
      gti->start[gti->row]=aetime;
      gti->stop[gti->row]=aetime;
    } else {
      gti->stop[gti->row]=aetime;
    }
  }
  
  return ANL_OK;
}


int hxdgtiFits_readGTI ( fitsfile *fp, int hdunum, HXD_GTI *gti, int *status )
{
  
  char comment[80];
  int casesen = TRUE;
  int hdutype;
  int gti_colnum[GTI_HDU_NFIELDS];
  
  int anynul;
  long firstelem = 1;
  long nelements = 1;
  double nulval = 0.0;
  
  long nrow;
  
  int i;
  
  fits_movabs_hdu( fp, hdunum, &hdutype, status);
  if ( *status ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, *status);
    return ANL_NG;
  }
  
  fits_read_key_lng( fp, "NAXIS2", &nrow, comment, status );
  
  gti->row = (int) nrow;
  
  if ( *status ) {
    fprintf(stderr, "%s: fits_read_key('NAXIS2') failed (%d)\n",
	    pname, *status);
    return ANL_NG;
  }
  
  for(i=0;i<GTI_HDU_NFIELDS;i++){
    fits_get_colnum(fp, casesen, gti_ttype[i], &gti_colnum[i], status);
    if ( *status ) {
      fprintf(stderr, "%s: fits_get_colnum failed (%d)\n",
	      pname, *status);
      return ANL_NG;
    }
  }
  
  gti->start = malloc(gti->row * sizeof(double));
  gti->stop = malloc(gti->row * sizeof(double));
  
  if (gti->start == NULL || gti->stop == NULL) {
    fprintf(stderr, "%s: ERROR allocating memory for GTI struct\n",pname);
    return ANL_NG;
  }
  
  for(i=0;i<gti->row;i++){
    
    fits_read_col_dbl(fp, gti_colnum[0], i+1, firstelem,nelements, nulval,
		      &gti->start[i], &anynul, status);
    
    if( *status){
      fprintf(stderr, "%s: fits_read_col('START') failed (%d)\n",
	      pname, *status);
      return ANL_NG;
    }
    
    fits_read_col_dbl(fp, gti_colnum[1], i+1, firstelem,nelements, nulval,
		      &gti->stop[i], &anynul, status);
    
    if( *status ){
      fprintf(stderr, "%s: fits_read_col('STOP') failed (%d)\n",
	      pname, *status);
      return ANL_NG;
    }
    
  }
  
  return ANL_OK;

}

/**** add ****/
int hxdgtiFits_UpdateOntime ( HXD_GTI * gti){
  int row;

  gti->ontime = 0;
  for (row = 0; row < gti->row; row++){
    gti->ontime += gti->stop[row] - gti->start[row];
  }

  return ANL_OK;
}
