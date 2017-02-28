#include "points.h"

#include <stdlib.h>
#include <string.h>

/****************************************************************************
* create an empty points structure.
****************************************************************************/
POINTS* readPoints(char* filename,   char* extname,
                   char* x0_colname, char* y0_colname,
                   char* x1_colname, char* y1_colname,
                   char* wgt_colname                  ) {

    int  x0_col;
    int  y0_col;
    int  x1_col;
    int  y1_col;
    int wgt_col;

    long long_value;

    POINTS* points;

    int status=0;
    int anynull=0;
    fitsfile* fp;

    /*********************
    * allocate structure *
    *********************/
    points = (POINTS*)malloc(sizeof(POINTS));

    /************
    * open file *
    ************/
    fits_open_file(&fp, filename, READONLY, &status);
    fits_movnam_hdu(fp, BINARY_TBL, extname, 0/*any version*/, &status);

    if(status) {
        fits_report_error(stderr, status);
        return NULL;
    }

    /*****************************
    * read the number of columns *
    *****************************/
    fits_read_key_lng(fp, "NAXIS2", &long_value, NULL, &status);
    if(status) {
        fits_report_error(stderr, status);
        return NULL;
    }

    points->npoints = (int)long_value;
    
    /*******************
    * find the columns *
    *******************/
    fits_get_colnum(fp,  CASEINSEN, x0_colname, &x0_col, &status);
    fits_get_colnum(fp,  CASEINSEN, y0_colname, &y0_col, &status);
    fits_get_colnum(fp,  CASEINSEN, x1_colname, &x1_col, &status);
    fits_get_colnum(fp,  CASEINSEN, y1_colname, &y1_col, &status);
    if(status) {
        fits_report_error(stderr, status);
        return NULL;
    }

    /*********************************************************
    * treat the weight column specially since it is optional *
    *********************************************************/
    if(strncasecmp(wgt_colname, "NONE", FLEN_VALUE)) {

        fits_get_colnum(fp,  CASEINSEN, wgt_colname, &wgt_col, &status);

        /*****************************
        * check if the column exists *
        *****************************/
        if(status==COL_NOT_FOUND) {
            wgt_col=-1;
            status=0;
        }
        
        /*************************
        * check for weird errors *
        *************************/
        if(status) {
            fits_report_error(stderr, status);
            return NULL;
        }

    } else {
        /*******************************************
        * we explicitly specified no weight column *
        *******************************************/
        wgt_col=-1;
    }


    /**********************************
    * allocate storage for the points *
    **********************************/
    points->x0 = (double*)malloc(sizeof(double)*points->npoints);
    points->y0 = (double*)malloc(sizeof(double)*points->npoints);
    points->x1 = (double*)malloc(sizeof(double)*points->npoints);
    points->y1 = (double*)malloc(sizeof(double)*points->npoints);

    if(wgt_col != -1 ) {
        points->wgt = (double*)malloc(sizeof(double)*points->npoints);
    } else {
        points->wgt = NULL;
    }
    
    /*****************************
    * read the columns, first X0 *
    *****************************/
    fits_read_col_dbl(fp, x0_col, 1l, 1l, (long)points->npoints,
                      0.0, points->x0, &anynull, &status);
    if(anynull) {
        fprintf(stderr, "Null values not supported\n");
        return NULL;
    }
    
    /*****
    * Y0 *
    *****/
    fits_read_col_dbl(fp, y0_col, 1l, 1l, (long)points->npoints,
                      0.0, points->y0, &anynull, &status);
    if(anynull) {
        fprintf(stderr, "Null values not supported\n");
        return NULL;
    }
    
    /*****
    * X1 *
    *****/
    fits_read_col_dbl(fp, x1_col, 1l, 1l, (long)points->npoints,
                      0.0, points->x1, &anynull, &status);
    if(anynull) {
        fprintf(stderr, "Null values not supported\n");
        return NULL;
    }

    /*****
    * Y1 *
    *****/
    fits_read_col_dbl(fp, y1_col, 1l, 1l, (long)points->npoints,
                      0.0, points->y1, &anynull, &status);
    if(anynull) {
        fprintf(stderr, "Null values not supported\n");
        return NULL;
    }

    /*********
    * WEIGHT *
    *********/
    if(wgt_col != -1 ) {
        fits_read_col_dbl(fp, wgt_col, 1l, 1l, (long)points->npoints,
                          0.0, points->wgt, &anynull, &status);
        if(anynull) {
            fprintf(stderr, "Null values not supported\n");
            return NULL;
        }
    }
    
    /*******************
    * check for errors *
    *******************/
    if(status) {
        fits_report_error(stderr, status);
        return NULL;
    }

    /**********************
    * clean up and return *
    **********************/
    fits_close_file(fp, &status);
    if(status) {
        fits_report_error(stderr, status);
        return NULL;
    }
    
    return points;

} /* end of readPoints function */
