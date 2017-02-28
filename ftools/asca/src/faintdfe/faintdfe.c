/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/faintdfe/faintdfe.c,v 1.3 2001/03/16 20:23:38 irby Exp $   */
/*                   */
/* main program for sisftool   by K.Mitsuda */

#include <stdlib.h>
#include "fitsio.h"
#include "longnam.h"

#include "info.h"
#include "function.h"
#include "grade.h"

#include "faintdfe.h"

/****************************************************************************
*****************************************************************************
* calculate the DFE for each chip and write these to the output file 
****************************************************************************/
void write_dfe(INFO* info, int* npixels, double dist[4][DIST_DIMEN], 
               double time ) {

int ccd;
double ccr[CCR_DIMEN];
double zl[4];
static double zl0[]={0.0, 0.0, 0.0, 0.0}; /* DFE in previous bin */



for(ccd=0; ccd<4; ccd++){

    if ( info->ccd_on[ccd] && npixels[ccd] > info->param->mincounts ){
        /******************************************************
        * there are enough counts on this chip to bother with 
        * First normalize the observed distribution, then
        * cross-correlate it with the model function, then
        * set the zero level to the peak of the cross-correlation
        * function. 
        ******************************************************/
        normalize_distribution(dist[ccd],DIST_DIMEN);

        cross_correlate(ccr, info->model[ccd], info->model_dimen,
                             dist[ccd], DIST_DIMEN);

        zl[ccd]  = find_peak(ccr+CCR_PEAK_LO, CCR_PEAK_N)+(double)CCR_PEAK_LO
                   - (double)(MAX_PHA+MAX_MODEL_PHA);

        addSpecDumpRow(info->param->specdump,
                       time,ccd, npixels[ccd], zl[ccd],dist[ccd]);

    } /* end if there are enough counts on current chip */
    else {
        /****************************
        * not enough counts to know *
        ****************************/
        zl[ccd] = zl0[ccd];
    }
} /* end of loop over chips */

/*********************************************
* output the DFE values if they have changed *
*********************************************/
if (zl[0]!=zl0[0] || zl[1]!=zl0[1] || zl[2]!=zl0[2] || zl[3]!=zl0[3]){

    fprintf(info->param->fpout,"%.4lf\t%.2lf\t%.2lf\t%.2lf\t%.2lf\n",
            time, zl[0], zl[1], zl[2], zl[3]);
}

/*********************************************************
* remember the current dfe values for the next iteration *
*********************************************************/
for(ccd=0; ccd<4; ccd++){
    zl0[ccd] = zl[ccd];
}


} /* end of write_dfe function */




/*************************************************************************
**************************************************************************
* iterator work function 
*************************************************************************/
int calculate_dfe(long total_rows, long offset, long first_row, long nrows,    
                  int ncols, iteratorCol *fits_col,  void* void_info ) 
{
INFO* info;
PARAM* param;

double* time;
short int* phadata;
short int* phas;
int* chip;
int row;
int ccd;

int i;

static int npixels[]={0, 0, 0, 0}; /* number of histogrammed pixels per chip */
static double dist1[4][DIST_DIMEN]; /* actual distribution of PHAs            */
                                   /* Note these get initialized in first    */
                                   /* iteration as long as npixels is        */
                                   /* initialized to zero.                   */


static double last_time; 
static double next_time=0.0; /* this forces the dist array to be initialized */

int grade;
int sumph,type,above;
int value;

/**************************************************
* unpack some information from various structures *
**************************************************/
info=(INFO*)void_info;

param=info->param;

time   =(double*    )fits_iter_get_array(fits_col+TIME_COL);
phadata=(short int* )fits_iter_get_array(fits_col+PHAS_COL);
chip   =(int*       )fits_iter_get_array(fits_col+CHIP_COL);



/******************************************
* loop over all rows in the current chunk *
******************************************/
for(row=1;row<=nrows;++row) {

    if(time[row]> next_time ) {
        /******************************************************
        * we have collected all the events in a given bin, so
        * now it's time to calculate the DFE
        ******************************************************/
        write_dfe(info,npixels,dist1,last_time);

        /*********************************************
        * reset the histograms for the next time bin *
        *********************************************/
        for (ccd=0;ccd<4;ccd++){
            if(info->ccd_on[ccd]) {

                for(i=0; i<DIST_DIMEN; ++i) dist1[ccd][i]=0;
                npixels[ccd]=0;
            }
        }

        if(time[row]<=next_time+param->binsec) {
            /*******************
            * consecutive bins *
            *******************/
            last_time = next_time;
        } else {
            /*************************
            * need to skip some bins *
            *************************/
            last_time=time[row];
        }

        next_time = last_time + param->binsec;

       

    } /* end if we are done with a bin */

    /************************
    * determine event grade *
    ************************/
    phas=phadata+((row-1)*9+1);
    grade=normclassify_event(phas,param->split,NULL,NULL);


    /**************************************
    * get the chip number for convenience *
    **************************************/
    ccd=chip[row];
    if(!info->ccd_on[ccd]) {
        fprintf(stdout,"Warning: ignoring event %d on invalid CCD %d\n",
                row+first_row,ccd);
    }

    /**************************************
    * add current event to the histograms *
    **************************************/
    if ( param->use_grade[grade]  && info->ccd_on[ccd] ){
        /***********************************
        * add this event to the histograms *
        ***********************************/
        if ( (value=phas[1])>=-MAX_PHA && value<=MAX_PHA ){
            dist1[ccd][value+MAX_PHA]++;
            npixels[ccd]++;
        }

        if ( (value=phas[3])>=-MAX_PHA && value<=MAX_PHA ){
            dist1[ccd][value+MAX_PHA]++;
            npixels[ccd]++;
        }

        if ( (value=phas[6])>=-MAX_PHA && value<=MAX_PHA ){
            dist1[ccd][value+MAX_PHA]++;
            npixels[ccd]++;
        }

        if ( (value=phas[8])>=-MAX_PHA && value<=MAX_PHA ){
            dist1[ccd][value+MAX_PHA]++;
            npixels[ccd]++;
        }

  } /* end if the grade and chip are valid */


} /* end of loop over rows */

/*************************************************************
* after the last event in the entire table we have to force
* another row to the output DFE table 
*************************************************************/
if(first_row+nrows>=total_rows) {
    write_dfe(info,npixels,dist1,last_time);
}

return(0);

} /* end of calculate_dfe iterator work function */




/***************************************************************************
****************************************************************************
* This is the main function which does all the work
***************************************************************************/
void faintdfe(void)
{

PARAM* param;
INFO* info;

int status=0;
fitsfile* fp;
iteratorCol* fits_col;
int hdu;

int ncols;


/**********************
* read parameter file *
**********************/
param=readParam();



/************
* open file *
************/
fits_open_file(&fp, param->infile, READONLY,&status);
if(status) {
    /*************
    * FITS error *
    *************/
    fprintf(stderr,"Can't open file %s\n",param->infile);
    fits_report_error(stderr,status);
    exit(status);
}

/*************************************************************
* go to default extension if we are in the primary extension *
*************************************************************/
fits_get_hdu_num(fp,&hdu);
if(hdu==1/*primary HDU*/) {
    fits_movnam_hdu(fp,BINARY_TBL,param->extname,0/*any version*/,&status);
    if(status) {
    /*************
    * FITS error *
    *************/
        fprintf(stderr,"Can't move to %s extension in %s\n",
                param->extname, param->infile);
        fits_report_error(stderr,status);
        exit(status);
    }

}



/*******************************************
* create and initialize the info structure *
*******************************************/
info=allocateInfo(param,fp);


/***************************************
* set up FITS columns for the iterator *
***************************************/
ncols=NCOLS;
fits_col=(iteratorCol*)malloc(sizeof(iteratorCol)*ncols);

fits_iter_set_by_name(fits_col+TIME_COL, fp, "TIME" , TDOUBLE, InputCol);
fits_iter_set_by_name(fits_col+PHAS_COL, fp, "PHAS" , TSHORT , InputCol);
fits_iter_set_by_name(fits_col+CHIP_COL, fp, "CCDID", TINT   , InputCol);

/****************************************
* call the iterator to do the real work *
****************************************/
fits_iterate_data(ncols,fits_col,
                  0l/*don't skip any rows*/,
                  0l/*read optimum columns per iteration*/,
                  calculate_dfe,info,&status);


/******************
* do some cleanup *
******************/
fits_close_file(fp,&status);
if(status) {
    fprintf(stderr,"Error reading or closing %s\n",param->infile);
    fits_report_error(stderr,status);
    exit(status);
}

fprintf(param->fpout,"! ZERODEF=%d\n",param->zerodef);
if(param->fpout!=stdout) fclose(param->fpout);


closeSpecDump(param->specdump);

}
