#include "fitsio.h"
#include "longnam.h"
#include "headas.h"

#include "xrsimage.h"
#include "param.h"
#include "headas.h"

/*
int main(int argc, char *argv[]) {
  int status;

  status = headas_init(argc, argv);
  if(0 == status) status = xrsimage();
  if(status) {exit(status);}
  status = headas_close(status);

  return status;
}
*/

/****************************************************************************
* this is the main function for xrsimage, but it doesn't really do much.
* The reason is that xrsimage is really two separate tools in one,
* since detector and sky images are made in very different ways.
* So all this function does is read the main parameters and then
* decide which type of image to create.
****************************************************************************/

int xrsimage (void) {

PARAM* param;

int status=0;
fitsfile* fp;
IMAGE* im;

set_toolversion(VERSION_ID);

/************************
* read basic parameters *
************************/
param=readParam(&status);
 if(status) {
   fprintf(stderr,"xrsimage: Error %d; abort.\n",status);
   return status;
 }

/***************************************************
* open event FITS file and move to event extension *
***************************************************/
fits_open_file(&fp,param->event_file,READONLY,&status);
fits_movnam_hdu(fp,BINARY_TBL,param->event_ext,0/* any version*/,&status);
if(status) {
    fprintf(stderr,"Error while opening event file %s\n",param->event_file);
    fits_report_error(stderr,status);
    return status;
}


/**********************************
* make the appropriate image type *
**********************************/
if(param->isSkyImage) im=xrs_sky_image(param,fp,&status); /* SKY */
else                  im=xrs_det_image(param,fp,&status); /* DET or FOC */

 if(status) {
   fprintf(stderr,"xrsimage: Error %d; abort.\n",status);
   return status;
 }

/******************
* write the image *
******************/
status=writeImageToFITSfile(im,param->out_file,fp,param);
 if(status) {
   fprintf(stderr,"xrsimage: Error %d; abort.\n",status);
   return status;
 }

 return 0;

} /* end of xrsimage function */


