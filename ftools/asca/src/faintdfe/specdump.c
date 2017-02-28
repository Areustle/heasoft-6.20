/***************************************************************************
* This file contains functions used to write a spectral dump file.
* Such a file is mostly useful only for debugging
***************************************************************************/
#include <stdlib.h>
#include "specdump.h"

/****************************************************************************
*****************************************************************************
* create a new specdump file
****************************************************************************/
SPECDUMP* openSpecDump(char* filename, int min_pha, int nbins) {

SPECDUMP* specdump;

int status=0;
fitsfile* fp;

int tfields=5;
char* ttype[]={"TIME", "CCD", "NPIXELS", "DFE", "SPECTRUM"};
char* tform[]={"1D"  , "1I" , "1J"     , "1E" , ""};
char* tunit[]={"s"   , ""   , ""       , "ADU", "pixels"};

/************************************************
* don't create a file if the filename is "none" *
************************************************/
if(!strcasecmp(filename,"none") ) {
    return(NULL);
}

/******************
* create the file *
******************/
fits_create_file(&fp, filename, &status);

/*************************
* write the table header *
*************************/
tform[4]=(char*)malloc(sizeof(char)*FLEN_VALUE);
sprintf(tform[4],"%dE",nbins);

fits_create_tbl(fp, BINARY_TBL, 0l, tfields, ttype, tform, tunit, 
                "SPECDUMP", &status);

/******************************
* put in descriptive comments *
******************************/
fits_modify_comment(fp,"TTYPE1","ascatime of DFE entry"       , &status);
fits_modify_comment(fp,"TTYPE2","CCD ID"                      , &status);
fits_modify_comment(fp,"TTYPE3","Number of pixels in spectrum", &status);
fits_modify_comment(fp,"TTYPE4","Dark Frame Error"            , &status);
fits_modify_comment(fp,"TTYPE5","Corner pixel spectrum"       , &status);

/******************
* min PHA keyword *
******************/
fits_write_key_lng(fp, "MIN_PHA", (long)min_pha, 
                   "Minimum PHA value in spectrum",&status);


/*******************
* check for errors *
*******************/
if(status) {
    fprintf(stderr,"Error creating spectrum dump file %s\n",filename);
    fits_report_error(stderr,status);
    return(NULL);
} 



/***********************************************
* create and initialize the specdump structure *
***********************************************/
specdump=(SPECDUMP*)malloc(sizeof(SPECDUMP));
if(!specdump) {
    fprintf(stderr,"Can't allocate memory for SPECDUMP structure\n");
    return NULL;
}

specdump->fp=fp;

specdump->time_col=1;
specdump->ccd_col=2;
specdump->npixels_col=3;
specdump->dfe_col=4;
specdump->spectrum_col=5;

specdump->nbins=nbins;

specdump->nrows=0l;

/****************
* normal return *
****************/
return specdump;

} /* end of open_specdump function */



/************************************************************************
*************************************************************************
* add a single row to the specdump file
************************************************************************/
void addSpecDumpRow(SPECDUMP* specdump, 
               double time, int ccd, int npixels, double dfe, 
               double* spectrum) {

int status=0;

long row;

/******************************************************
* do nothing if the specdump structure is not defined *
******************************************************/
if(specdump==NULL) return;

/*******************************
* increment the number of rows *
*******************************/
row= ++(specdump->nrows);


/****************
* write the row *
****************/
fits_write_col_dbl(specdump->fp, specdump->time_col, row,1l,1l,&time,&status);
fits_write_col_int(specdump->fp, specdump->ccd_col , row,1l,1l,&ccd ,&status);
fits_write_col_int(specdump->fp, specdump->npixels_col,row,1l,1l,&npixels,
                                                                       &status);
fits_write_col_dbl(specdump->fp, specdump->dfe_col , row,1l,1l,&dfe ,&status);

fits_write_col_dbl(specdump->fp, specdump->spectrum_col , row,1l,
                   specdump->nbins, spectrum ,&status);

/*******************
* check for errors *
*******************/
if(status) {
    fprintf(stderr,"Error writing row %d of SPECDUMP file\n",row);
    fits_report_error(stderr,status);
} 


} /* end of addSpecDumpRow function */

/****************************************************************************
*****************************************************************************
* close a specdump file and destroy the corresponding structure
****************************************************************************/
void closeSpecDump(SPECDUMP* specdump) {

int status=0;

/******************************************************
* do nothing if the specdump structure is not defined *
******************************************************/
if(specdump==NULL) return;

/*****************
* close the file *
*****************/
fits_close_file(specdump->fp,&status);
fits_report_error(stderr,status);

/************************
* destroy the structure *
************************/
free(specdump);

}
