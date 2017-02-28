#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <fitsio.h>
#include "pow.h"
int readimage(char *,void **,float *,float *, long *,double *,double *,
	      double *,double *);
int printerror( int status);

int readpha(ClientData, Tcl_Interp *, int, char **); 



int readpha(ClientData clientData,Tcl_Interp *interp, int argc,
	      char *argv[]) 
{
  char filename[160] = "gis2img.fits";
  char array_name[32];
  FILE *rawout;
  float datamin, datamax;
  void *imagebuff;
  char *i, *j;
  int k;
  float straight_lineX[50],straight_lineY[50];
  int  status, array_type, datasize, height, width;
  int zero = 0, one = 1, fifty = 50;
  double d_zero = 0, d_one = 1;
  long naxes[2];
  double xorigin,xinc,yorigin,yinc;
  int xdisp = 300, ydisp = 300;
  int **databuff; 
  char longStr[1024];

  strcpy(filename,argv[1]);
  
  if((status = readimage(filename,&imagebuff,&datamin,&datamax,naxes, &xorigin,
			 &xinc, &yorigin, &yinc))) {
    printerror ( status);
    return TCL_ERROR;
  } 
  i = strstr(filename, ".");
  if(i == NULL) i = filename + strlen(filename);
  for (j = i; ((memcmp(j,"/",1)) && !(j == filename)) ; j--) {
    /*    printf("%x %x\n",filename,j);*/
  }
  if (!(memcmp(j,"/",1)) ) j++;
  for (k=0;k<32;k++) { array_name[k] = (char) NULL;}
  strncpy(array_name,j,i-j);
  
  array_type = REAL_DATA;
  datasize = pixelSizes[array_type];
  width = (int) naxes[0];
  height = (int) naxes[1];


  sprintf(longStr,PTRFORMAT " %i %i", imagebuff, array_type, width*height);
  Tcl_SetResult(interp, longStr, TCL_OK);
  return TCL_OK;
}


/*--------------------------------------------------------------------------*/
int readimage(char *filename,void **imagebuff,float *datamin,float *datamax,
	      long naxes[2],double *xorigin, double *xinc, double *yorigin,
	      double *yinc)

    /************************************************************************/
    /* Read a FITS image and determine the minimum and maximum pixel values */
    /************************************************************************/
{
    fitsfile *fptr;                             /* pointer to the FITS file */
    int status,  nfound, anynull;
    long  group, fpixel, nbuffer, npixels, ii;
    float *buffptr;
    char comm[80];

    float nullval;

    status = 0;

    Tcl_DumpActiveMemory("TCLDUMP");
    if ( fits_open_file(&fptr, filename, READONLY, &status) )     /* open the image */
         printerror( status );

    /* read the NAXIS1 and NAXIS2 keyword to get image size */
    if ( fits_read_keys_lng(fptr, "NAXIS", 1, 2, naxes, &nfound, &status) )
         printerror( status );

    npixels  = naxes[0] * naxes[1];         /* number of pixels in the image */

    *imagebuff =  ckalloc(sizeof(float)*npixels);
    
    group    = 1;
    fpixel   = 1;
    nullval  = 0;                /* don't check for null values in the image */
    *datamin  = 1.0E30;
    *datamax  = -1.0E30;

    nbuffer = npixels;
    
    if ( fits_read_img(fptr,TFLOAT, fpixel, nbuffer,(void *) &nullval,
		       (void *) *imagebuff, &anynull, &status) ) {
      printerror( status );
      return TCL_ERROR;
    }
    
    buffptr = (float *) *imagebuff;
    for (ii = 0; ii < nbuffer; ii++)  {
      if (*buffptr < *datamin) 
	*datamin = *buffptr;

      if (*buffptr > *datamax) 
	*datamax = *buffptr;
      
      buffptr++;
    }
    fits_read_key_dbl(fptr,"CRVAL1",xorigin,comm,&status);
    fits_read_key_dbl(fptr,"CRVAL2",yorigin,comm,&status);
    fits_read_key_dbl(fptr,"WMREBIN",xinc,comm,&status);
    *yinc = *xinc;
    *xorigin *= *xinc;
    *yorigin *= *yinc;

    printf("\nMin and max image pixels =  %.0f, %.0f\n", *datamin, *datamax);

    if ( ffclos(fptr, &status) )                    /* close the FITS file */
         printerror( status );

    return status;
}





/*--------------------------------------------------------------------------*/
int printerror( int status)
{   char tmp_result[1000] = " ";
    /*****************************************************/
    /* Print out cfitsio error messages and exit program */
    /*****************************************************/

    char status_str[FLEN_STATUS], errmsg[FLEN_ERRMSG];
  
    if (status) {
        Tcl_AppendResult(interp,"\n*** Error occurred during program execution ***\n",(char *) NULL);

    ffgerr(status, status_str);        /* get the error status description */
    sprintf(tmp_result,"\nstatus = %d: %s\n", status, status_str);
    Tcl_AppendResult(interp,tmp_result,(char *) NULL);
    }

    if ( ffgmsg(errmsg) )  /* get first message; null if stack is empty */
    {
         sprintf(tmp_result,"\nError message stack:\n  %s\n", errmsg);
	 Tcl_AppendResult(interp,tmp_result,(char *) NULL);

         while ( ffgmsg(errmsg) )  /* get remaining messages */
	 Tcl_AppendResult(interp,errmsg,"\n",(char *) NULL);
    }

    return TCL_ERROR;       /* terminate the program, returning error status */
}

