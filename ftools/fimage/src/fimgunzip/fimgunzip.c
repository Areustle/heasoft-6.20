/******************************************************************************
* SELECTOR TASK:
*      fimgunzip
*
* FILE:
*      fimgunzip.c
*
* DESCRIPTION:
*
*      Decompresse a compressed FITS image and write the uncompressed image 
*      to a image HDU. 
*
* Usage:
*     fimgunzip  infile outfile
*
* Note:
*
* AUTHOR:
*      Ning Gan  2/04/2000  
*           Based on the uncompress_fits.c from Bill Pence.
*
* MODIFICATION HISTORY:
*
*     
************************************************************************/
					 

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"
#include "xpi.h"
#include "cftools.h"

void fimgunzip()
{
    char taskname[] = "fimgunzip V1.0.0";
    fitsfile *infptr, *outfptr;  /* pointer to the FITS files */
    char infile[FLEN_FILENAME];     
    char outfile[FLEN_FILENAME];
    int clobber;


    char parname[20];
    int BufLen_2 = FLEN_FILENAME -1;
    char temp[FLEN_FILENAME];
    char mess[255],errmes[255];
    int status = 0; 
    char *pt;
    
    c_ptaskn(taskname);

    /************************************
     *					* 
     *  Obtain the parameters           * 
     *					*          
     ************************************/

    strcpy(parname,"infile");
    Uclgst(parname,infile,&status);
    if(status) {
        strcpy(errmes,"could not get infile parameter");
        c_fcerr(errmes);
        return ;
    }

    strcpy(parname,"outfile");
    Uclgst(parname,temp,&status);
    if(status) {
        strcpy(errmes,"could not get outfile parameter");
        c_fcerr(errmes);
        return ;
    }

    strcpy(parname,"clobber");
    Uclgsb(parname,&clobber,&status);
    if(status) {
        strcpy(errmes,"could not get clobber parameter");
        c_fcerr(errmes);
        return ;
    }
    
    /* set the clobber parameter */
    pt = temp;
    while(*pt == ' ') pt++;
    if(clobber && *pt != '!') {
        strcpy(outfile,"!");
        strcat(outfile,temp);
    } else {
        strcpy(outfile,temp);
    }

    /************************************
     *					* 
     *  Decompresse the image.          * 
     *					*          
     ************************************/

    /*  open input image  */
    if ( fits_open_file(&infptr, infile, READONLY, &status) ) {
         fits_report_error( stderr, status );
         return;
    }

    /* create output file */
    if ( fits_create_file(&outfptr, outfile, &status) ) {
         fits_report_error( stderr, status );
         return;
    }

    /* decompress the image */
    if ( fits_decomp_img(infptr, outfptr, &status) ) { 
         fits_report_error( stderr, status );
         return;
    }

    /* close the files */
    if (fits_close_file(infptr, &status) ) { 
         fits_report_error( stderr,status );
         return;
    }

    if ( fits_close_file(outfptr, &status) ) {
         fits_report_error( stderr,status );
         return;
    }
    return;
}

