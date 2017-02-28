#include "gtis.h"

#include <stdio.h>

#include "fitsio.h"
#include "longnam.h"





/******************************************************************************
*
******************************************************************************/
GTIS* allocateGTIS(int n) {

    GTIS* gti;

    if(n<0) {
        fprintf(stderr, "Requested %d GTIs\n", n);
        return NULL;
    }

    gti = (GTIS*)malloc(sizeof(GTIS));

    gti->n = n;
    gti->current=0;

    gti->start = (double*)malloc(sizeof(double)*n);
    gti->stop  = (double*)malloc(sizeof(double)*n);

    if(gti->start == NULL || gti->stop == NULL) {
        fprintf(stderr, "Could not allocate %d GTIs\n", n);
        return NULL;
    }

    return gti;

} /* end of allocateGTIS function */


/******************************************************************************
* read a FITS file[ext]
******************************************************************************/
GTIS* readFITSGTIS(char* filename) {

    GTIS* gti;

    fitsfile* fp;
    int status=0;
    int anynull;

    long n=0;
    int start_col;
    int stop_col;

    int hdu;
    int hdutype;


    /*********************************************************
    * get the number of rows and allocate the GTIS structure *
    *********************************************************/
    fits_open_file(&fp, filename, READONLY, &status);

    /*************************************************
    * if we are in the primary HDU, go to the first
    * extension by default
    *************************************************/
    if( fits_get_hdu_num(fp, &hdu) == 1 ) {
        fits_movabs_hdu(fp, 2, &hdutype, &status);
    }

    /********************************************************
    * get the number of rows and allocate the GTI structure *
    ********************************************************/
    fits_read_key_lng(fp, "NAXIS2", &n, NULL, &status);
    fits_report_error(stderr, status);

    gti = allocateGTIS((int)n);
    if(gti==NULL) return NULL;

    /**********************************
    * read the START and STOP columns *
    **********************************/
    fits_get_colnum(fp, CASEINSEN, "START", &start_col,&status);
    fits_get_colnum(fp, CASEINSEN, "STOP" ,  &stop_col,&status);


    fits_read_col_dbl(fp, start_col, 1l, 1l, n, 0.0, gti->start, 
                      &anynull, &status);

    fits_read_col_dbl(fp,  stop_col, 1l, 1l, n, 0.0, gti->stop, 
                      &anynull, &status);

    /*******************
    * check for errors *
    *******************/
    fits_close_file(fp, &status);
    if(status) {
        fits_report_error(stderr, status);
        return NULL;
    }

    /********************
    * return the result *
    ********************/
    return gti;

} /* end of readFITSGTIS function */



void restrictGTIs (GTIS * gtis, double start, double stop)
{
	int i;
	int o = 0;

	for (i = 0; i < gtis->n; ++i) {
		double t0 = gtis->start[i];
		double t1 = gtis->stop[i];
		if (t0 > stop || t1 < start)
			printf("discarded GTI %d from %f to %f\n", i, t0, t1);
		else {
			const char *verb = (t0 < start || t1 > stop)
					? "truncated" : "kept";
			if (t0 < start)
                t0 = start;
			if (t1 > stop)
                t1 = stop;
			gtis->start[o] = t0;
			gtis->stop[o] = t1;
			printf("%s GTI[%d] %f - %f\n", verb, i, t0, t1);
			++o;
		}
	}
}

