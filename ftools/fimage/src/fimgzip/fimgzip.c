/******************************************************************************
* SELECTOR TASK:
*      fimgzip
*
* FILE:
*      fimgzip.c
*
* DESCRIPTION:
*
*      Compresse a FITS image and write the compressed image to a 
*      binary extension.
*
* Usage:
*     fimgzip  infile outfile
*
* Note:
*
* AUTHOR:
*      Ning Gan  2/04/2000  
*           Based on the compress_fits.c from Bill Pence.
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

void fimgzip()
{
    char taskname[] = "fimgzip V1.0.0";
    fitsfile *infptr, *outfptr;  /* pointer to the FITS files */
    char infile[FLEN_FILENAME];     
    char outfile[FLEN_FILENAME];
    char algorithm[FLEN_FILENAME];
    int clobber;

    int  imgtype, compress_type = 0, blocksize, nbits;
    long tilesize[3], origsize, outsize;
    int naxis = 0;
    long naxes[10];

    char parname[20];
    int BufLen_2 = FLEN_FILENAME -1;
    char temp[FLEN_FILENAME];
    int itemp;
    char mess[255],errmes[255];
    int status = 0; 
    int i;
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

    strcpy(parname,"algorithm");
    Uclgst(parname,algorithm,&status);
    if(status) {
        strcpy(errmes,"could not get infile parameter");
        c_fcerr(errmes);
        return ;
    } 

    strcpy(parname,"xtilesize");
    Uclgsi(parname,&itemp,&status);
    if(status && status!=3) {
        strcpy(errmes,"could not get xtilesize parameter");
        c_fcerr(errmes);
        return ;
    } 
    tilesize[0] = itemp;
    if(status) tilesize[0] = 0;
    status = 0;

    strcpy(parname,"ytilesize");
    Uclgsi(parname,&itemp,&status);
    if(status && status!=3) {
        strcpy(errmes,"could not get ytilesize parameter");
        c_fcerr(errmes);
        return ;
    } 
    tilesize[1] = itemp;
    if(status) tilesize[1] = 0;
    status = 0;

    strcpy(parname,"ztilesize");
    Uclgsi(parname,&itemp,&status);
    if(status && status!=3) {
        strcpy(errmes,"could not get ztilesize parameter");
        c_fcerr(errmes);
        return ;
    }
    tilesize[2] = itemp;
    if(status) tilesize[2] = 0;
    status = 0;

    strcpy(parname,"blocksize");
    Uclgsi(parname,&blocksize,&status);
    if(status) {
        strcpy(errmes,"could not get blocksize parameter");
        c_fcerr(errmes);
        return ;
    }

    strcpy(parname,"nbits");
    Uclgsi(parname,&nbits,&status);
    if(status) {
        strcpy(errmes,"could not get nbits parameter");
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
     *  Compresse the image.            * 
     *					*          
     ************************************/

    /* set the algorithm flag */
    switch (algorithm[0]) {
         case 'R':
             compress_type = RICE_1;
             break;
         case 'G':
             compress_type = GZIP_1;
             break;
         case 'P':
             compress_type = PLIO_1;
             break;
         default:
             sprintf(errmes,"unknown compression algorithm: %s", algorithm);
             c_fcerr(errmes);
             return;
     }
     sprintf(mess,"compress infile using %s algorithm.",algorithm);
     c_fcecho(mess);

    /*  open input image  */
    if ( fits_open_file(&infptr, infile, READONLY, &status) ) {
         fits_report_error( stderr, status );
         return;
    }


    /*  get image dimensions and type */
    if (fits_get_img_param(infptr, 10, &imgtype, &naxis, naxes, &status)) {
         fits_report_error( stderr, status );
         return;
    }
    if(naxis == 0) { 
        c_fcerr("Null Image; No compression will be performed");
        return;
    }  
    if(naxis > 3) { 
        c_fcerr("Compression algorithm only supports up to 3-d images.");
        return;
    }  
    origsize = abs(imgtype);
    for ( i = 0; i < naxis; i++) origsize *= naxes[i];
    origsize /= 8;

    if(!tilesize[0]) tilesize[0] = naxes[0];
    if(!tilesize[1]) tilesize[1] = 1;
    if(!tilesize[2]) tilesize[2] = 1;
   
    /* open the output file */
    fits_open_file(&outfptr,outfile, READWRITE, &status);
    if( status == FILE_NOT_OPENED) {
        status = 0; 
        if ( fits_create_file(&outfptr, outfile, &status) ) {
             fits_report_error( stderr, status );
             return;
        }
    } else if (status) {
        fits_report_error( stderr, status );
        return;
    }  else {
        fits_movrel_hdu(outfptr,-1, &i, &status);  
        status = 0;
    }     

    /* compress the image */
    if ( fits_comp_img(infptr, outfptr, compress_type, tilesize,
         blocksize, nbits, &status) ) { 
         fits_report_error( stderr, status );
         return;
    }

    /* calculate the compression ratio */
    ffgkyj(outfptr, "PCOUNT", &outsize, NULL, &status);
    if (outsize) {
       sprintf(mess,"   ratio = %.2f", (double) origsize / outsize);
       c_fcecho(mess);
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


