#include <stdio.h>
#include <string.h>
#include "image.h"
#include "polygon.h"

/*****************************************************************************
******************************************************************************
* alloctaer storage for a new image with given dimensions 
*****************************************************************************/
IMAGE* allocateImage(int dimenx, int dimeny) {

IMAGE* im;
int i;

im=(IMAGE*)malloc(sizeof(IMAGE));

im->dimenx=dimenx;
im->dimeny=dimeny;
im->npixels=dimenx*dimeny;

im->data   =(IMAGE_DATA_TYPE**)malloc(sizeof(IMAGE_DATA_TYPE*)*dimeny     );
im->data[0]=(IMAGE_DATA_TYPE* )malloc(sizeof(IMAGE_DATA_TYPE )*im->npixels);
for(i=1;i<dimeny;++i) {
    im->data[i]=im->data[i-1]+dimenx;
}

return(im);

} /* end of allocateImage function */


/*****************************************************************************
******************************************************************************
* set all pixels in an image to a  given value (typically 0)
*****************************************************************************/
void blankImage(IMAGE* im, int value) {
    
IMAGE_DATA_TYPE* pix;
IMAGE_DATA_TYPE* last_pix;

last_pix=im->data[0]+im->npixels;
for(pix=im->data[0];pix<last_pix;++pix) {
    *pix=value;
}

} /* end of blankImage function */


/*****************************************************************************
******************************************************************************
* increment by 'increment' all the pixels contained within a polygon
* defined by the  corners whose coordinates are given by
* xcorners,ycorners. This routine assumes that any horizontal line
* (y=const) will pass through the sides of this polygon at 
* most twice. The routine also assumes that the coordinates of the
* first corner (x[0],y[0]) are repeated in the last corner (x[n-1],y[n-1]).
* Note "n" gives the number of corners in the polygon and is one less
* than the dimendion of the coordinate arrays.
* the corners must be listed in order either clockwise or counter-clockwise
* around the polygon.
*****************************************************************************/
void incrementConvexPolygonInImage(IMAGE* im, double* x, double* y, int n,
                                   int increment) {

int xlo,xhi; /* real valued bounding box  */
int ylo,yhi; /* around polygon            */

int ilo,ihi; /*actual limits of interior pixels */

int i,j;
int cor;

/****************************************
* determine bounding box around polygon *
****************************************/
xlo=(int)(x[0]+.5);
xhi=(int)(x[0]+.5);
ylo=(int)(y[0]+.5);
yhi=(int)(y[0]+.5);

for(cor=1;cor<n;++cor) {
    i=(int)(x[cor]+.5);
    if(i<xlo) xlo=i;
    if(i>xhi) xhi=i;

    j=(int)(y[cor]+.5);
    if(j<ylo) ylo=j;
    if(j>yhi) yhi=j;
}

/********************************************
* clip the bounding box to within the image *
********************************************/
if(xlo<0) xlo=0;
if(ylo<0) ylo=0;

if(xhi>im->dimenx) xhi=im->dimenx-1;
if(yhi>im->dimeny) yhi=im->dimeny-1;


/*****************************************
* loop over rows within the bounding box *
*****************************************/   
for(j=ylo;j<=yhi;++j) {
    
    /********************************************
    * find the first interior pixel on the left *
    ********************************************/ 
    ilo=xlo;
    while(ilo<=xhi && !isInPolygon((double)ilo,(double)j,x,y,n) ) ++ilo;

    if(ilo<=xhi) {
        /****************************************
        * there are interior pixels in this row 
        * find the boundary on the right
        ****************************************/

        ihi=xhi;
        while(!isInPolygon((double)ihi,(double)j,x,y,n) ) --ihi;

        #ifdef DEBUG
        printf("incrementConvexPolygonInImage: j=%d il0=%d ihi=%d\n",j,ilo,ihi);
        #endif /* DEBUG */

        /*******************************************
        * increment all pixels between ilo and ihi *
        *******************************************/
        for(i=ilo;i<=ihi;++i) {
            im->data[j][i] +=increment;
        }
    } /* end if row has interior pixels */

} /* end of loop over rows of pixels */



} /* end of incrementConvexPolygonInImage function */

/*****************************************************************************
******************************************************************************
* Write image to a new FITS file.
* Also copy keywords from the input event file and copy the entire GTI 
* extension to the image.
*****************************************************************************/
int writeImageToFITSfile(IMAGE* im, char* filename, 
                          fitsfile* eventfp, PARAM* param) {

int status=0;
fitsfile* fp;

long dimen[2];

/*******************************************
* create a new FITS file to hold the image *
*******************************************/
fits_create_file(&fp,filename,&status);
if(status) {
    fprintf(stderr,"Error creating FITS image file %s\n",filename);
    fits_report_error(stderr,status);
    return status;
}

/*************************
* create the primary HDU *
*************************/
dimen[0]=im->dimenx;
dimen[1]=im->dimeny;

fits_create_img(fp,8*sizeof(IMAGE_DATA_TYPE),2,dimen,&status);

/*****************************
* write image scale keywords *
*****************************/
fits_write_keys_dbl(fp,"CRPIX",1/*starting from one*/,2/* two keywords*/,
                    param->crpix,4/*decimals*/,NULL,&status);

fits_write_keys_dbl(fp,"CRVAL",1/*starting from one*/,2/* two keywords*/,
                    param->crval,4/*decimals*/,NULL,&status);

fits_write_keys_dbl(fp,"CDELT",1/*starting from one*/,2/* two keywords*/,
                    param->cdelt,4/*decimals*/,NULL,&status);

/** Added by Y.T. **/

 if(param->isSkyImage) {
fits_write_key_str(fp,"CUNIT1","deg",NULL,&status);
fits_write_key_str(fp,"CUNIT2","deg",NULL,&status);
fits_write_key_str(fp,"CTYPE1","RA---TAN",NULL,&status);
fits_write_key_str(fp,"CTYPE2","DEC--TAN",NULL,&status);
 }

if(status) {
    fprintf(stderr,"Error writing fits keywords to file %s\n",filename);
    fits_report_error(stderr,status);
    return status;
}

/** Added by Y.T.  end **/

/************************************
* copy keywords from the event file *
************************************/
{
long tfields;
char key[FLEN_KEYWORD]; 
char card[FLEN_CARD];
int nkeys;
int row;

/*******************************************************************
* we assume the event file is pointing to the event file extension 
* then we look for the last TFORM keyword in the header
*******************************************************************/
fits_read_key_lng(eventfp,"TFIELDS",&tfields,NULL,&status);

sprintf(key,"TFORM%ld",tfields);
fits_read_card(eventfp,key,card,&status);

/****************************************************************
* There may be a few more keywords after the TFORM keyword, 
* so continue to scan down until we are past those 
****************************************************************/
while(!strncmp(card,"EXTNAME",FLEN_KEYWORD) ||
      !strncmp(card,"TTYPE",5) || !strncmp(card,"TUNIT",5) || 
      !strncmp(card,"TSCAL",5) || !strncmp(card,"TZERO",5) || 
      !strncmp(card,"TNULL",5) || !strncmp(card,"TDISP",5) ||
      !strncmp(card,"TFORM",5)  ) {

    fits_get_hdrpos(eventfp, &nkeys,&row,&status);
    fits_read_record(eventfp,row,card,&status);
}

if(status) {
    fprintf(stderr,"Error in searching for fits keywords from event file=n");
    fits_report_error(stderr,status);
    return status;
}


/*********************************************
* comment about the origin of these keywords *
*********************************************/
fits_write_comment(fp,
                   "The following keywords were copied from the event file",
                   &status);


/**************************************************
* copy the rest of the keywords to the image file *
**************************************************/
while(strncmp(card,"END",FLEN_KEYWORD) ) {

    fits_get_hdrpos(eventfp, &nkeys,&row,&status);
    fits_read_record(eventfp,row,card,&status);
    fits_write_record(fp,card,&status);
}

if(status) {
    fprintf(stderr,"Error copying keywords from event file to image\n");
    fits_report_error(stderr,status);
    return status;
}


} /* end of block of local variables for copying header keywords */


/***********************
* write the image data *
***********************/
fits_write_img(fp,IMAGE_FITS_DATA_TYPE,1l,im->npixels,im->data[0],&status);
if(status) {
    fprintf(stderr,"Error writing image data to %s\n",filename);
    fits_report_error(stderr,status);
    return status;
}



/***************************************************************
* copy the GTI extension from the event file to the image file *
***************************************************************/
{
int event_hdu;
int hdu_type;

fits_get_hdu_num(eventfp,&event_hdu);
fits_movnam_hdu(eventfp,BINARY_TBL,"GTI",0/*any version*/,&status);
if(status) {
    fprintf(stderr,"Error finding GTI extension in event file\n");
    fits_report_error(stderr,status);
    return status;
}

fits_copy_hdu(eventfp,fp,0/*don't reserve room for extra keywords*/,&status);
fits_movabs_hdu(eventfp,event_hdu,&hdu_type,&status);

if(status) {
    fprintf(stderr,"copying keywords from event file to image\n");
    fits_report_error(stderr,status);
    return status;
}

} /* end of block of local variables for copying GTI extension */


/***********************
* close the image file *
***********************/
fits_close_file(fp,&status);

if(status) {
    fprintf(stderr,"Error writing FITS image file %s\n",filename);
    fits_report_error(stderr,status);
    return status;
}

return 0;
} /* end of writeImageToFITSfile function */

