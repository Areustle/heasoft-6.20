#include "infile.h"

#include <stdlib.h>
#include "param_wrappers.h"


/****************************************************************************
*
****************************************************************************/
INFILE* openInfile(char* name) {

    INFILE* infile;
    int status=0;

    char col_x[FLEN_VALUE];
    char col_y[FLEN_VALUE];
    
    /*************************
    * allocate the structure *
    **************************/
    infile = (INFILE*)malloc(sizeof(INFILE));
    if(infile==NULL) return NULL;

    /****************
    * open the file *
    ****************/
    fits_open_data(&(infile->fp), name, READONLY, &status);
    if(status) {
        fprintf(stderr, "Could not open infile %s\n", name);
        fits_report_error(stderr, status);
        return NULL;
    }

    /*******************
    * find the columns *
    *******************/
    read_string_param("deltaxcol", col_x, FLEN_VALUE);
    read_string_param("deltaycol", col_y, FLEN_VALUE);

    fits_get_colnum(infile->fp, CASEINSEN, col_x, &(infile->col_x), &status);
    fits_get_colnum(infile->fp, CASEINSEN, col_y, &(infile->col_y), &status);
    fits_get_colnum(infile->fp, CASEINSEN, "TIME", &(infile->col_time), &status);

    if(status) {
        fprintf(stderr, "Could not find columns %s, %s, or TIME in %s",
                col_x, col_y, name);
        fits_report_error(stderr, status);
        return NULL;
    }
    
    /***************************************************************
    * determine the number of rows and initialize to the first row *
    ***************************************************************/
    fits_read_key_lng(infile->fp, "NAXIS2", &(infile->nrows), NULL, &status);
    if(status) {
        fprintf(stderr, "Could not determine the number of rows in %s", name);
        fits_report_error(stderr, status);
        return NULL;
    }


    infile->row=1;

    return infile;


} /* end of openInfile function */

/***************************************************************************
*
***************************************************************************/
int readInfileValues(INFILE* infile,
                     double* time, double* detx, double* dety) {
                     
    int anynull=0;
    int status=0;
                     
    /****************
    * check for EOF *
    ****************/
    if(infile->row > infile->nrows ) return 0;

    /******************
    * read the values *
    ******************/
    fits_read_col_dbl(infile->fp, infile->col_time, infile->row,
                      1l, 1l, 0.0, time, &anynull, &status);

    fits_read_col_dbl(infile->fp, infile->col_x, infile->row,
                      1l, 1l, 0.0, detx, &anynull, &status);
                      
    fits_read_col_dbl(infile->fp, infile->col_y, infile->row,
                      1l, 1l, 0.0, dety, &anynull, &status);
                      
    /*******************
    * check for errors *
    *******************/
    if(status) {
        fprintf(stderr, "Error reading row %ld\n", infile->row);
        return 0;
    }
    
    /***************************
    * increment the row number *
    ***************************/
    (infile->row)++;

    return 1;
    
} /* end of readInfileValues function */

/***************************************************************************
*
***************************************************************************/
void closeInfile(INFILE* infile) {
    int status=0;
    
    fits_close_file(infile->fp, &status);
    if(status) fits_report_error(stderr, status);
    

} /* end of closeInfile method */

