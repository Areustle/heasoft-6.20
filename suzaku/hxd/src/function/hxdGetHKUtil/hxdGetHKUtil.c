/*
   v0.0.2 test version
   v0.0.3 debug
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"
#include "hxdGetHKUtil.h"

#define FITS_MAX_NUM 10
#define HDU_MAX_NUM 50
#define KEY_MAX_NUM 500
#define KEY_NAME_LENGTH_MAX 128
#define FIRST_ROW -1

static char pname[] = "hxdGetHKUtil";

typedef struct {
    char filename[256];
    fitsfile *fp;
    int nhdu;
} FITS;

typedef struct {
    int ihdu;
    int irow;
    long nrow;
    double *time;
} HDU;

typedef struct {
    int fits;
    int hdu;
    int colnum;
    int typecode;
    long repeat;
} KEY;

struct{
    int nfits;
    FITS FITS[FITS_MAX_NUM];
    HDU HDU[HDU_MAX_NUM];
    KEY KEY[KEY_MAX_NUM];
}com;

static int getHK_val_int( int key, int irow, unsigned int *val ){
    
    int istat = 0;
    
    int anynul;
    int hdutype;
    
    long firstelem = 1;
    long nelements = (long) com.KEY[key].repeat;
    int datatype = com.KEY[key].typecode;

    int i;
    
    fits_movabs_hdu(com.FITS[com.KEY[key].fits].fp,
		    com.HDU[com.KEY[key].hdu].ihdu, &hdutype, &istat );
    if ( istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu('%s') failed (%d)\n",
		pname, com.FITS[com.KEY[key].fits].filename, istat);
	return -1;
    }

    if( datatype == TUINT ){
	unsigned int nulval = 0;
	unsigned int *data;

	data = malloc(sizeof(int)*nelements);
	
	fits_read_col(com.FITS[com.KEY[key].fits].fp, datatype,
		      com.KEY[key].colnum, irow, firstelem, nelements,
		      &nulval, data, &anynul, &istat );
	
	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col('%d') failed (%d)\n",
		    pname, index, istat);
	    return -1;
	}
	
	for(i=0;i<nelements;i++){
	    val[i] = data[i];
	}
    } else if( datatype == TINT ){
	int nulval = 0;
	int *data;
	
	data = malloc(sizeof(int)*nelements);
	
	fits_read_col(com.FITS[com.KEY[key].fits].fp, datatype,
		      com.KEY[key].colnum, irow, firstelem, nelements,
		      &nulval, data, &anynul, &istat );

	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col('%d') failed (%d)\n",
		    pname, index, istat);
	    return -1;
	}

	for(i=0;i<nelements;i++){
	    val[i] = (unsigned int)data[i];
	}
    }else if( datatype == TUSHORT ){
	unsigned short nulval = 0;
	unsigned short *data;
	
	data = malloc(sizeof(short)*nelements);
	
	fits_read_col(com.FITS[com.KEY[key].fits].fp, datatype,
		      com.KEY[key].colnum, irow, firstelem, nelements,
		      &nulval, data, &anynul, &istat );

	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col('%d') failed (%d)\n",
		    pname, index, istat);
	    return -1;
	}
	
	for(i=0;i<nelements;i++){
	    val[i] = (unsigned int)data[i];
	}
    }else if( datatype == TSHORT ){
	short nulval = 0;
	short *data;
	
	data = malloc(sizeof(short)*nelements);
	
	fits_read_col(com.FITS[com.KEY[key].fits].fp, datatype,
		      com.KEY[key].colnum, irow, firstelem, nelements,
		      &nulval, data, &anynul, &istat );

	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col('%d') failed (%d)\n",
		    pname, index, istat);
	    return -1;
	}
	
	for(i=0;i<nelements;i++){
	    val[i] = (unsigned int)data[i];
	}
    }else{
	unsigned char nulval = 0;
	unsigned char *data;

	data = malloc(sizeof(char)*nelements);
	fits_read_col(com.FITS[com.KEY[key].fits].fp, datatype,
		      com.KEY[key].colnum, irow, firstelem, nelements,
		      &nulval, data, &anynul, &istat );
	
	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col('%d') failed (%d)\n",
		    pname, index, istat);
	    return -1;
	}
	
	for(i=0;i<nelements;i++){
	    val[i] = (unsigned int)data[i];
	}
    }	
    
    return 0;
}

int
hxd_getHK_int(double time, int key, unsigned int *val, double *ac_time ){
    
    int irow;
    int nrow;
    int pre_irow;
    
    int time_judge;
    
    nrow = (int) com.HDU[com.KEY[key].hdu].nrow;
    
    if(nrow == 0){
	return HXD_GET_HK_NO_UPDATE;
    }
    
    pre_irow = irow = com.HDU[com.KEY[key].hdu].irow;
    
    if(irow == FIRST_ROW){
	irow = 0;
    }
    
    if( com.HDU[com.KEY[key].hdu].time[nrow-1] <= time ){
	irow = nrow;
    } else if( com.HDU[com.KEY[key].hdu].time[0] > time ){
	irow = 0;
    } else {
	if(irow == 0){
	    irow++;
	}
	if(irow == nrow){
	    irow--;
	}
	while( irow<nrow && irow>0 ){
	    if( (time_judge = com.HDU[com.KEY[key].hdu].time[irow-1] <= time)
	       && com.HDU[com.KEY[key].hdu].time[irow] > time ){
		break;
	    } else if (time_judge){
		irow++;
	    } else {
		irow--;
	    }
	}
    }

    com.HDU[com.KEY[key].hdu].irow = irow;
    
    if(irow == pre_irow){
	return HXD_GET_HK_NO_UPDATE;
    } else {
	if( irow == 0 ){
	    ac_time[1]= com.HDU[com.KEY[key].hdu].time[0];
	    return HXD_GET_HK_NOT_YET_EXIST;
	} else if ( irow == nrow ){
	    if( getHK_val_int(key, irow, val) ){
		return HXD_GET_HK_ERR;
	    }
	    ac_time[0]= com.HDU[com.KEY[key].hdu].time[nrow-1];
	    return HXD_GET_HK_NEXT_NOT_EXIST;
	} else {
	    if( getHK_val_int(key, irow, val) ){
		return HXD_GET_HK_ERR;
	    }
	    ac_time[0]= com.HDU[com.KEY[key].hdu].time[irow-1];
	    ac_time[1]= com.HDU[com.KEY[key].hdu].time[irow];
	    return HXD_GET_HK_OK;
	}
    }
    
    return HXD_GET_HK_ERR;
}


int hxd_getHK_Register(fitsfile *fp, char *key, int *index){

    int istat = 0;

    static key_index = 0;
    
    int fits;
    int ihdu;
    int hdu = 0;
    int row;
    
    int typecode;
    long repeat;
    long width;
    int colnum;

    int hdutype;
    int casesen = TRUE;
    
    for(fits=0;fits<com.nfits;fits++){
	
	for(ihdu=0;ihdu<com.FITS[fits].nhdu;ihdu++){
	    
	    fits_movabs_hdu(com.FITS[fits].fp, com.HDU[hdu].ihdu,
			    &hdutype, &istat );
	    if ( istat ) {
		fprintf(stderr, "%s: fits_movabs_hdu('%s') failed (%d)\n",
			pname, com.FITS[fits].filename, istat);
		return HXD_GET_HK_ERR;
	    }
	    
	    fits_get_colnum(com.FITS[fits].fp, casesen, key, &colnum,
			    &istat);
	    
	    if ( istat == COL_NOT_FOUND ) {
		istat = 0;
		hdu++;
		continue;
	    } else if ( istat ){
		fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
			pname, key, istat);
		return HXD_GET_HK_ERR;
	    }
	    
	    fits_get_coltype(com.FITS[fits].fp, colnum, &typecode,
			     &repeat, &width, &istat);
	    if ( istat ){
		fprintf(stderr, "%s: fits_get_coltype('%s') failed (%d)\n",
			pname, key, istat);
		return HXD_GET_HK_ERR;
	    }
	    	    
	    com.KEY[key_index].fits = fits;
	    com.KEY[key_index].hdu = hdu;
	    com.KEY[key_index].typecode = typecode;
	    com.KEY[key_index].repeat = repeat;
	    com.KEY[key_index].colnum = colnum;
	    
	    *index = key_index;
	    key_index++;	    
	    return HXD_GET_HK_OK;
	    
	}
    }

    return HXD_GET_HK_ERR;
    
}


int hxd_getHK_Init(fitsfile *fp, char *filename){
    
    int istat = 0;
    
    static int fits = 0;
    static int hdu = 0;
    
    int fields = 0;
    int ihdu;
    int irow;
    
    int nhdu;
    long nrow;
    
    int time_colnum;
    long firstelem = 1;
    long nelements = 1;
    
    int hdutype;
    int anynul;
    int casesen = TRUE;
    double nulval = 0.0;
    char comment[81];
    
    strcpy(com.FITS[fits].filename ,filename);
    
    fits_open_file(&fp, filename, READONLY, &istat);
    
    if ( istat ) {
        fprintf(stderr, "%s: fits_open_file('%s') failed (%d)\n",
                pname, filename, istat);
	return HXD_GET_HK_ERR;
    }
    
    com.FITS[fits].fp = fp;
    
    fits_get_num_hdus(fp, &nhdu, &istat);
    if ( istat ) {
        fprintf(stderr, "%s: fits_get_num_hdus('%s') failed (%d)\n",
                pname, filename, istat);
	return HXD_GET_HK_ERR;
    }

    com.FITS[fits].nhdu = nhdu;
    
    for(ihdu=1;ihdu<nhdu;ihdu++){
	
	fits_movabs_hdu(fp, ihdu+1, &hdutype, &istat );
	if ( istat ) {
	    fprintf(stderr, "%s: fits_movabs_hdu('%s') failed (%d)\n",
		    pname, filename, istat);
	    return HXD_GET_HK_ERR;
	}
	
	fits_read_key_lng(fp, "NAXIS2", &nrow, comment, &istat);
	if ( istat ) {
	    fprintf(stderr, "\
%s: fits_read_key_lng NAXIS2 ('%s') failed (%d)\n",
		    pname, filename, istat);
	    return HXD_GET_HK_ERR;
	}
	
	com.HDU[hdu].nrow = nrow;
	
	com.HDU[hdu].time = realloc(com.HDU[hdu].time, sizeof(double)*nrow);
	if(com.HDU[hdu].time == NULL && nrow!=0){
	    fprintf(stderr, "%s realloc failed \n", pname);
	    return HXD_GET_HK_ERR;
	}
	
	fits_get_colnum(fp, casesen, "TIME", &time_colnum, &istat);	
	if ( istat ) {
	    fprintf(stderr, "%s: fits_get_colnum('TIME') failed (%d)\n",
		    pname, istat);
	    return HXD_GET_HK_ERR;
	}
	
	for(irow =0;irow<nrow;irow++){
	    fits_read_col_dbl(fp, time_colnum, irow+1,
			      firstelem, nelements, nulval,
			      &com.HDU[hdu].time[irow], &anynul, &istat);
	    if(istat){
		fprintf(stderr, "%s: fits_read_col_dbl('TIME') failed (%d)\n",
			pname, istat);
		return HXD_GET_HK_ERR;
	    }
        }
	
	com.HDU[hdu].ihdu = ihdu+1;
	com.HDU[hdu].irow = FIRST_ROW;
	hdu++;
	
    }
    
    fits++;
    com.nfits = fits;
    
    return HXD_GET_HK_OK;
    
}
