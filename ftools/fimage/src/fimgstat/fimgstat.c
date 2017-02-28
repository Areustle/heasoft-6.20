/***********************************************************************
File : fimgstat.c

Description:
       Calculates the sum, mean, rms of the pixels within the values
       defined by threshlo and threshup. Also finds out the minimum
       and maximum pixel values, the numer of pixels included within
       the user defined range, and the location of those minimum and
       maximum pixel values. The output is in the form of STDOUT and
       the parameter file updated with these values.

Author:
       Banashree M Seifert (March25, 1997)V1.0.0

       Banashree M Seifert (July 29, 1997)V1.1.0
          . included callib routine checkinfile.c

      Peter D Wilson (June 26, 1998)V1.2.0
       . Updated for new filename handling procedures
      Peter D Wilson (Apr 13, 1999) V1.3.0
       . Fix rms calculation and allow for 1D images and use doubles
             not floats for calculations
      Ning Gan(November 29, 1999)V1.4.0
       . Updated for new compressed image.


Variables used:
       infile   char   input file name
      outfile   char   output file name. Default is STDOUT
       lopres    int   =0 if lower threshold (thresh_lo) is INDEF
                       =1 if lower threshold (thresh_lo) is defined some value
       uppres    int   =0 if upper threshold (thresh_up) is INDEF
                       =1 if upper threshold (thresh_up) is defined some value
    thresh_lo   real   lower threshold value
    thresh_up   real   upper threshold value
***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "fitsio.h"        /* cfitsio defined constants */
#include "xpi.h"           /* parameter file functions, e.g. Uclgst */
#include "ftoolstruct.h"   /* C-Fortran common blocks */
#include "cftools.h"       /* standard C library constants */
#include "fimgstat.h"      /* task specific definitions */

void fimgstat()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    int lopres=0, uppres=0;
    double thresh_lo=0., thresh_up=0.;
    char subinfo[100];
    int status=0, clobber=0;

    char task[] = "fimgstat V1.5.0";
    c_ptaskn(task);

/************ get parameters from fimgstat.par ************/

    fimgstatgp(infile, outfile, &lopres, &uppres, &thresh_lo, 
               &thresh_up, &clobber, &status);

    if(status) {
      strcpy(subinfo,"..... Error returning from fimgstatgp");
      DispMsg(0,0,subinfo);
      strcpy(subinfo,"****** unsuccessfully exited ******\n");
      DispMsg(0,0,subinfo);
      return;
    }

/************ now do the actual merging of the image files **********/

    fimgstatdo(infile, outfile, &lopres, &uppres, &thresh_lo,
                 &thresh_up, &clobber, &status); 

    if(status) {
      strcpy(subinfo,"..... Error returning from fimgstatdo");
      DispMsg(0,0,subinfo);
      strcpy(subinfo,"****** unsuccessfully exited ******\n");
      DispMsg(0,0,subinfo);
      return;
    }

 
    strcpy(subinfo,"****** successfully exited ******\n");
    DispMsg(0,0,subinfo);
    return;

}

/*******************************************************************
function:
      fimgstatgp

description:
      gets the parameters for the task

author:
      Banashree M Seifert (March, 25, 1997)

modification history:
      Banashree M Seifert (July, 25, 1997)
       . problem running on ALPHAs solved
      Peter D Wilson (June 26, 1998)
       . Updated for new filename handling procedures. Drop calls to
         CheckInFile and CheckFile
      Ning Gan (July 27, 1998)
       .Replaced the cfitsio.h with fitsio.h

usage:
     fimgstatgp(char *infile, char *outfile, int *lopres, int *uppres, 
                double *thresh_lo, double *thresh_up,
                int *clobber, int *status)

*********************************************************************/
void fimgstatgp(char *infile, char *outfile, int *lopres, int *uppres, 
                double *thresh_lo, double *thresh_up,
                int *clobber, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    char threshlo[FLEN_BUFFER], threshup[FLEN_BUFFER];
    char subinfo[100];


    *status = 0;
    Uclgst("infile", infile, status);
    if(infile[0] =='\0')
    { strcpy(subinfo,"..... Error: filename is required \n");
      DispMsg(0,0,subinfo);
      *status = 1;
      return;
    }
    if(*status)
    { strcpy(subinfo,"..... Error reading infile from .par file \n");
      DispMsg(0,0,subinfo);
      return;
    }

    /* PDW 6/26/98: Leave it for FFOPEN to decide
    CheckInFile(infile);
    */

    Uclgst("threshlo", threshlo, status);
    if(*status)
    { strcpy(subinfo,"..... Error reading threshlo from .par file \n");
      DispMsg(0,0,subinfo);
      return;
    }
    if((threshlo[0] == 'I') || (threshlo[0] == 'i'))
    { *lopres = 0; }
    else
    {
      *lopres = 1;
      *thresh_lo=atof(threshlo);
    }

    Uclgst("threshup", threshup, status);
    if(*status)
    { strcpy(subinfo,"..... Error reading threshup from .par file \n");
      DispMsg(0,0,subinfo);
      return;
    }
    if((threshup[0] == 'I') || (threshup[0] == 'i'))
    { *uppres = 0; }
    else
    {
      *thresh_up=atof(threshup);
      *uppres = 1;
    }

    Uclgst("outfile", outfile, status);
    if(*status)
    { strcpy(subinfo,"..... Error reading outfile from .par file \n");
      DispMsg(0,0,subinfo);
      return;
    }

    Uclgsb("clobber", clobber, status);
    if(*status)
    { strcpy(subinfo,"..... Error reading clobber from .par file \n");
      DispMsg(0,0,subinfo);
      return;
    }

    if(outfile[0] == '!') { *clobber = 1; strcpy(outfile,(outfile+1)); }

    /* PDW 6/26/98: Not needed. fopen will delete file automatically
    CheckFile(outfile, 0, *clobber);
    */

    return;
}


/*******************************************************************
function:
      fimgstatdo

description:
      resets the pixels to a constant value 

author:
      Banashree M Seifert (March, 25, 1997)

modification history:

new variables used:
     arrptr    double   pointer for image arrays
     sum       double   sum of all included pixels
     mean      double   mean of all included pixels
     rms       double   rms of all included pixels
     num       int     no. of included pixels
     minvalue  double   minimum value of the included pixels
     maxvalue  double   maximum value of the included pixels
     xmin      int     location of x-pixel at minimum
     ymin      int     location of y-pixel at minimum
     xmax      int     location of x-pixel at maximum
     ymax      int     location of y-pixel at maximum

usage:
    void fimgstatdo(char *infile, char *outfile, int *lopres, int *uppres, 
                     double *thresh_lo, double *thresh_up,
                     int *clobber, int *status); 

*********************************************************************/

void fimgstatdo(char *infile, char *outfile, int *lopres, int *uppres, 
                double *thresh_lo, double *thresh_up,
                 int *clobber, int *status) 
{
    FILE *fp;
    fitsfile *infp1=NULL;

    char  subinfo[100];

    long naxes[4];
    long total;
    int naxis;


    double nulval, sumsq=0.;

    int anynul, i,  num=0, maxpt=0, minpt=0;
    int xmin=0, xmax=0, ymin=0, ymax=0;
    int found;

    double *arrptr;
    double minvalue, maxvalue;
    double sum=0., rms=0., mean=0.;
 
/********** open input file ***************/

    *status = 0;
    ffopen(&infp1, infile, READONLY, status);
    if(*status){ 
      strcpy(subinfo,"..... Error opening input imagefile \n");
      DispMsg(0,0,subinfo); 
      return;
    }

/*************************************************************
Now read the files and do the job
*****************************************************************/

    if(fits_get_img_dim (infp1, &naxis, status)) {
      Printerror(status);
      return;
    }

    if(naxis > 4 || naxis == 0) {
      sprintf(subinfo,
         "NAXIS = %d, fimgstat can be only applied to 1-D or 2-D image array.",
         naxis);
      c_fcerr(subinfo);
      return;
    }

    naxes[0]=0;
    naxes[1]=0;
    naxes[2]=0;
    naxes[3]=0;
    if(fits_get_img_size (infp1, naxis, naxes, status)) {
      Printerror(status);
      return;
    }
 
    if (naxes[2] > 1 )  {
      sprintf(subinfo,
         "NAXIS3 = %ld, fimgstat can be only applied to 1-D or 2-D image array.",
         naxes[2]);
      c_fcerr(subinfo);
      return;
    }

    if (naxes[3] > 1 )  {
      sprintf(subinfo,
         "NAXIS4 = %ld, fimgstat can be only applied to 1-D or 2-D image array.",
         naxes[3]);
      c_fcerr(subinfo);
      return;
    }

/*************************************************************
allocate DMA to the arrays to be read
arrptr = array of the input image
*************************************************************/
    /* Allow for 1-D array */
    if(naxes[1] == 0) {
      total = naxes[0];
    } else {
      total = naxes[0]*naxes[1];
    }
    arrptr = (double*) calloc(total, sizeof(double));
        if(arrptr == NULL)
        { strcpy(subinfo, ".... Error: calloc failed \n");
          DispMsg(0,0,subinfo); 
	  return;
        }
    nulval=DOUBLENULLVALUE;
    anynul=0;
    if(fits_read_img(infp1, TDOUBLE, 1, total,&nulval,arrptr,
       &anynul, status)) {
       Printerror(*status);
       return;
    }

     if((!*lopres) && (!*uppres)) goto NONE; 

     if((*lopres) && (*uppres))  goto BOTH; 

     if((*lopres) && (!*uppres)) goto LOWONLY;

     if((!*lopres) && (*uppres)) goto UPONLY; 
          
NONE:
  minvalue = arrptr[0];
  maxvalue = arrptr[0];
  for(i=0; i<total; i++) {
   if (arrptr[i] == DOUBLENULLVALUE) continue;
   sum += arrptr[i];
   sumsq += arrptr[i]*arrptr[i];
   num++;
   if(arrptr[i] < minvalue) {
     minvalue = arrptr[i]; minpt = i;}
   if(arrptr[i] > maxvalue) {
     maxvalue = arrptr[i]; maxpt = i;}
  }
  goto NEXT;

BOTH:
  maxvalue = *thresh_lo;
  minvalue = *thresh_up;
  for(i=0; i<total; i++){      
    if((arrptr[i] != DOUBLENULLVALUE) &&
       (arrptr[i] >= *thresh_lo) && 
       (arrptr[i] <= *thresh_up)){
     sum += arrptr[i];
     sumsq += arrptr[i]*arrptr[i];
     num++;
     if(arrptr[i] < minvalue) {
       minvalue = arrptr[i]; 
       minpt = i;
     }
     if(arrptr[i] > maxvalue) {
       maxvalue = arrptr[i]; 
       maxpt = i;
     }
    }
  }
  goto NEXT;

LOWONLY:
  maxvalue = *thresh_lo;
  /* Find an initial minvalue in the specified range: */
  minvalue = 0;
  found = 0;
  while (!found) {
    for(i=0; i<total; i++) {      
      if((arrptr[i] != DOUBLENULLVALUE) && (arrptr[i] >= *thresh_lo)){
        minvalue = arrptr[i];
	found = 1;
      }
    }
  }
  for(i=0; i<total; i++){      
    if((arrptr[i] != DOUBLENULLVALUE) &&
       (arrptr[i] >= *thresh_lo)){
      sum += arrptr[i];
      sumsq += arrptr[i]*arrptr[i];
      num++;
      if(arrptr[i] < minvalue){ 
	minvalue = arrptr[i]; 
	minpt = i;
      }
      if(arrptr[i] > maxvalue){ 
	maxvalue = arrptr[i]; 
	maxpt = i;
      }
    }
  }
  goto NEXT;

UPONLY:
  minvalue = *thresh_up;
  /* Find an initial maxvalue in the specified range: */
  maxvalue = 0;
  found = 0;
  while (!found) {
    for(i=0; i<total; i++) {      
      if((arrptr[i] != DOUBLENULLVALUE) && (arrptr[i] <= *thresh_up)){
        maxvalue = arrptr[i];
	found = 1;
      }
    }
  }
  for(i=0; i<total; i++){      
    if((arrptr[i] != DOUBLENULLVALUE) &&
       (arrptr[i] <= *thresh_up)){
      sum += arrptr[i];
      sumsq += arrptr[i]*arrptr[i];
      num++;
      if(arrptr[i] < minvalue){ 
	minvalue = arrptr[i]; 
	minpt = i;
      }
      if(arrptr[i] > maxvalue){
	maxvalue = arrptr[i]; 
	maxpt = i;
      }
    }
  }
  goto NEXT;

NEXT:
/********************************************************************
Calculate mean, rms, (i,j) values for minimum pixel and maximum pixel
for the output
********************************************************************/

    if(num > 0){
      mean = sum/num;
      rms = sqrt( (sumsq - sum*sum/num)/num );
    }
    else {
      mean = rms = 0.; 
      minvalue = maxvalue = 0.; 
    }

    xmin = minpt%naxes[0]+1;
    ymin = minpt/naxes[0]+1;
    xmax = maxpt%naxes[0]+1;
    ymax = maxpt/naxes[0]+1;

/********************************************************************
Write to output file or STDOUT which ever user defines
********************************************************************/

    if(!strcmp(outfile,"STDOUT")){
      printf("\n****** statistics for %s ******\n\n",infile);
      printf("The sum of the selected image                 = %.10g\n",sum);
      printf("The mean of the selected image                = %.10g\n",mean);
      printf("The standard deviation of the selected image  = %.10g\n",rms);
      printf("The number of points used in calculation      = %d\n",num);
      printf("The minimum of selected image                 = %.10g\n", minvalue);
      printf("The maximum of selected image                 = %.10g\n", maxvalue);
      printf("The location of minimum is at pixel number    = (%d,%d)\n", 
	     xmin,ymin);
      printf("The location of maximum is at pixel number    = (%d,%d)\n", 
	     xmax,ymax);
    }
    else{
      if((fp=fopen(outfile,"w"))!=NULL){
	fprintf(fp,"The sum of the selected image                 = %.10g\n",sum);
	fprintf(fp,"The mean of the selected image                = %.10g\n",mean);
	fprintf(fp,"The standard deviation of the selected image  = %.10g\n",rms);
	fprintf(fp,"The number of points used in calculation      = %d\n",num);
	fprintf(fp,"The minimum of selected image                 = %.10g\n", 
		minvalue);
	fprintf(fp,"The maximum of selected image                 = %.10g\n", 
		maxvalue);
	fprintf(fp,"The location of minimum is at pixel number    = (%d,%d)\n",
		xmin,ymin);
	fprintf(fp,"The location of maximum is at pixel number    = (%d,%d)\n",
		xmax,ymax);
	fclose(fp);
      }
      else{
	printf("Error opening output file\n");
	*status=1;
	return;
      }
    }

/********************************************************************
Put the calculated values to the parameter file
********************************************************************/

    *status=0;
    Uclpsd("sum", sum, status);
    Uclpsd("mean", mean, status);
    Uclpsd("rms", rms, status);
    Uclpsi("num", num, status);
    Uclpsd("min", minvalue, status);
    Uclpsd("max", maxvalue, status);
    Uclpsi("xmin", xmin, status);
    Uclpsi("xmax", xmax, status);
    Uclpsi("ymin", ymin, status);
    Uclpsi("ymax", ymax, status);
    if(*status){ 
      strcpy(subinfo,"..... Error putting back values to .par file \n");
      DispMsg(0,0,subinfo);
    }

    
/*********************************************************************
free the pointers and close the input file
*********************************************************************/
    *status=0;
    free(arrptr);
    if(ffclos(infp1, status)) Printerror(*status);
    
    return;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL fimgsta
#endif
#ifdef unix
#define F77CALL fimgsta_
#endif

void F77CALL()
{
 void fimgstat();

 fimgstat();
}
 
