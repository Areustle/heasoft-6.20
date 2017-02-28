/***********************************************************************
File : fimgtrim.c

Description:
       Reset pixels to a constant whose value > upper threshold
                                              < lower threshold 

Author:
       Banashree M Seifert (March25, 1997)V1.0.0

Modification history:
      Peter D Wilson (June 26, 1998)V1.1.0
        . Updated for new file-handling procedures
      Ning Gan (July 28, 1998) V1.2.0
	. Deleted  cfitsio.h.
      toliver (June 4, 1999) V1.3.0
        . Modified to correctly process 3-D (and higher) FITS images

Variables used:
       infile   char   input file name
      outfile   char   output file name
       lopres    int   =0 if lower threshold (thresh_lo) is INDEF
                       =1 if lower threshold (thresh_lo) is defined some value
       uppres    int   =0 if upper threshold (thresh_up) is INDEF
                       =1 if upper threshold (thresh_up) is defined some value
    thresh_lo   real   lower threshold value
    thresh_up   real   upper threshold value
     const_lo   real   value to be set for pixel if that pixel falls below
                       thresh_lo 
     const_up   real   value to be set for pixel if that pixel falls above
                       thresh_up
         type   char   =absolute
                       =percentage (not supported this version) 1.0.0
***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "fitsio.h"        /* cfitsio defined constants */
#include "xpi.h"           /* parameter file functions, e.g. Uclgst */
#include "ftoolstruct.h"   /* C-Fortran common blocks */
#include "cftools.h"       /* standard C library constants */
#include "fimgtrim.h"      /* task specific definitions */

 
void fimgtrim()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    int lopres=0, uppres=0;
    float thresh_lo=0., thresh_up=0., const_lo=0., const_up=0.;
    char type[FLEN_BUFFER];
    char subinfo[100];
    int status=0, clobber=0;

/************ get parameters from fimgtrim.par ************/

    fimgtrimgp(infile, outfile, &lopres, &uppres, &thresh_lo, 
               &thresh_up, &const_lo, &const_up, type, 
               &clobber, &status);
   if(status){
     strcpy(subinfo,"\n... Error returning from fimgtrimgp \n"); 
     DispMsg(0,0,subinfo);
     strcpy(subinfo,"\n****** unsuccessfully exited ******\n");
     DispMsg(0,0,subinfo);
     return;
   }

/************ now do the actual merging of the image files **********/

    fimgtrimdo(infile, outfile, &lopres, &uppres, &thresh_lo,
                 &thresh_up, &const_lo, &const_up, type,
                 &clobber, &status); 
   if(status){
     strcpy(subinfo,"\n... Error returning from fimgtrimdo \n"); 
     DispMsg(0,0,subinfo);
     strcpy(subinfo,"\n****** unsuccessfully exited ******\n");
     DispMsg(0,0,subinfo);
     return;
   }

    strcpy(subinfo,"\n****** successfully exited ******\n");
    DispMsg(0,0,subinfo);

    return;

}

/*******************************************************************
function:
      fimgtrimgp

description:
      gets the parameters for the task

author:
      Banashree M Seifert (March, 25, 1997)

modification history:
      Banashree M Seifert (July, 25, 1997)
        . problem runnig on ALPHAs resolved
      Peter D Wilson (June 26, 1998)
        . Updated for new file-handling procedures

usage:
     fimgtrimgp(char *infile, char *outfile, int *lopres, int *uppres, 
                float *thresh_lo, float *thresh_up, float *const_lo, 
                float *const_up, char *type, int *clobber, int *status)

*********************************************************************/
void fimgtrimgp(char *infile, char *outfile, int *lopres, int *uppres, 
                float *thresh_lo, float *thresh_up, float *const_lo, 
                float *const_up, char *type, int *clobber, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    char threshlo[FLEN_BUFFER], threshup[FLEN_BUFFER];
    char subinfo[100];
    
    *status = 0;
    Uclgst("infile", infile, status);

    if(infile[0] =='\0'){
       *status=1;
       sprintf(subinfo, "\n.... Error: filename is required \n");
       StatusChk(*status, subinfo);
    }

    sprintf(subinfo,  "\n.... Error reading infile from .par file \n");
    StatusChk(*status, subinfo);

    /* PDW 6/26/98: Leave it for FFOPEN to decide
    CheckInFile(infile);
    */

    Uclgst("threshlo", threshlo, status);
    sprintf(subinfo,  "\n.... Error reading thresh_lo from .par file\n");
    StatusChk(*status, subinfo);

    if((threshlo[0] == 'I') || (threshlo[0] == 'i'))
    { *lopres = 0; }
    else
    {
      *lopres = 1;
      *thresh_lo=atof(threshlo);
      Uclgsr("const_lo", const_lo, status);
      sprintf(subinfo,  "\n.... Error reading const_lo from .par file \n");
      StatusChk(*status, subinfo);
    }

    Uclgst("threshup", threshup, status);
    sprintf(subinfo,  "\n.... Error reading thresh_up from .par file\n");
    StatusChk(*status, subinfo);

    if((threshup[0] == 'I') || (threshup[0] == 'i'))
    { *uppres = 0; }
    else{
      *thresh_up=atof(threshup);
      *uppres = 1;
      Uclgsr("const_up", const_up, status);
      sprintf(subinfo,  "\n.... Error reading const_up from .par file\n");
      StatusChk(*status, subinfo);
    }

    if((*lopres) || (*uppres)){
       Uclgst("type", type, status);
       sprintf(subinfo,  "\n.... Error reading TYPE from .par file\n");
       StatusChk(*status, subinfo);

       if(type[0]== 'p'){
          *status = 1;
          strcpy(subinfo,"\n..... Error: TYPE \"p\" is not yet supported\n");
          DispMsg(0,0,subinfo);
          sprintf(subinfo,  "\n...supported TYPE is: absolute(\"a\") \n");
       }
       else if(type[0]!='a'){
          *status = 1;
          strcpy(subinfo,"\n.....Error: unknown TYPE parameter\n");
          DispMsg(0,0,subinfo);
	  sprintf(subinfo,  "\n...supported TYPE is: absolute(\"a\") \n");
       }
       StatusChk(*status, subinfo);
    }

    Uclgsb("clobber", clobber, status);
    sprintf(subinfo,  ".... Error reading clobber from .par file\n");
    StatusChk(*status, subinfo);

    Uclgst("outfile", outfile, status);
    if(outfile[0] == '\0'){
      *status = 1;
      sprintf(subinfo, "\n... Error: output filename is required\n");
      StatusChk(*status, subinfo);
    }

    StatusChk(*status, subinfo);

    /* PDW 6/26/98: Add "bang" to outfile if clobber set...FFINIT understands */
    if( *clobber && outfile[0] != '!') {
       char tmp[FLEN_BUFFER];
       strcpy(tmp,outfile);
       outfile[0]='!';
       strcpy( outfile+1, tmp);
    }

/*-----------------------------------------------------------------
   checking whether outfile exists.  If so, then if clobber=1, then
   delete the pre-existing file     
------------------------------------------------------------------*/
    /* PDW 6/26/98: FFINIT will now delete files starting with a "bang"
    CheckFile(outfile, 0, *clobber);
    */

    return;
}

/*******************************************************************
function:
      fimgtrimdo

description:
      resets the pixels to a constant value 

author:
      Banashree M Seifert (March, 25, 1997)

modification history:
      Banashree M Seifert (July, 25, 1997)
       . for ALPHA compatibility

      Banashree M Seifert (July 29, 1997)
       . incluce callib routine CheckInFile

      toliver (June 4, 1999)
       . modified to correctly process 3-D (and higher) FITS images
usage:
    void fimgtrimdo(char *infile, char *outfile, int *lopres, int *uppres, 
                     float *thresh_lo, float *thresh_up, float *const_lo, 
                     float *const_up, char *type, int *clobber, int *status); 

*********************************************************************/

void fimgtrimdo(char *infile, char *outfile, int *lopres, int *uppres, 
                 float *thresh_lo, float *thresh_up, float *const_lo, 
                 float *const_up, char *type, int *clobber, int *status) 
{
   int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
   fitsfile *infp1=NULL;
   fitsfile *infp2=NULL;
   float nulval;
   int bitpix, naxis, i, anynul;
   long inc[MAXRANGES];
   long fpixels[MAXRANGES], lpixels[MAXRANGES];
   long naxes[MAXRANGES];
   long pcount, gcount, row, loc;
   float *buffer;
   size_t buflen;
 
/********** open input file ***************/

   *status = 0;
   if(ffopen(&infp1, infile, READONLY, status)) Printerror(*status);

/*************************************************************
Now create outfile, read the input files and do the job
*****************************************************************/

   if (ffinit (&infp2, outfile, status)) Printerror(*status);
 
   if (ffcopy (infp1, infp2, 0, status)) Printerror(*status);
 
/*
** Read the image header to get dimensions of image, exit on error.
*/
   if (fits_read_imghdr (infp1, MAXRANGES, NULL, &bitpix, &naxis,
                         naxes, &pcount, &gcount, NULL, status))
   {
      StatusChk (ERROR_STATUS, "...Error: image header could not be read\n");
   }

/*
** Image dimension exception handling, exit for empty image or image
**    with too many dimensions, treat 1-D image specially to work with
**    processing algorithm. 
*/
   if (naxis == 0)
   {
      StatusChk (ERROR_STATUS, "...Error: image header is empty\n");
   }
   else if (naxis == 1)
   {
      naxis = 2;
      naxes[1] = naxes[0];
      naxes[0] = 1;
   }
   else if (naxis > MAXRANGES)
   {
      StatusChk (ERROR_STATUS, "...Error: image header has too many axes\n");
   }
         
/*
** Calculate the size of the buffer to store the image values.  The processing
**    loop will iterate over the size of the last dimension, so calculate the
**    product of the sizes of all preceding dimensions.
*/
   for (buflen = 1, i = 0; i < (naxis - 1); i++)
   {
      buflen *= naxes[i];
   }

/*
** Allocate the buffer, exit on error.
*/
   buffer = (void *)malloc (sizeof (float) * buflen);
   if (buffer == 0)
   {
      StatusChk (ERROR_STATUS,
                 "...Error: couldn't allocate memory for input buffer\n");
   }

/*
** Processing loop initializations.  For all dimensions except the last one,
**    set the first pixel to 1 and the last pixel to the size of the dimension.
*/
   nulval = 0.0;
   loc = 1;
   for ( i = 0; i < naxis; i++ )
   {
      fpixels[i] = 1;
      lpixels[i] = naxes[i];
      inc[i] = 1;
   }

/*
** Processing loop.  Iterate over the size of the last dimension, adjusting
**    the offset in the output file accordingly.
*/
   for (row = 1; row <= naxes[naxis - 1]; row++, loc += buflen)
   {

/*
**    Adjust the first and last pixel values for the last dimension to read in
**       current "row".
*/
      fpixels[naxis - 1] = lpixels[naxis - 1] = row;

/*
**    Read in the image values for the current "row", exit on error.
*/
      if (fits_read_subset_flt (infp1, 1, naxis, naxes, fpixels, lpixels,
                                inc, nulval, buffer, &anynul, status))
      {
         Printerror(*status);
      }

/*
**    Lower threshold processing.
*/
      if (*lopres)
      { 
         for (i = 0; i < buflen; i++)
         {
            if (buffer[i] < *thresh_lo)
            {
               buffer[i] = *const_lo;
            }
         }
      }

/*
**    Upper threshold processing.
*/
      if (*uppres)
      { 
         for (i = 0; i < buflen; i++)
         {
            if (buffer[i] > *thresh_up)
            {
               buffer[i] = *const_up;
            }
         }
      }

/*
**    Write out the processed image values for the current "row", exit on
**       error.
*/
      if (fits_write_img_flt (infp2, 1, loc, buflen, buffer, status))
      {
         Printerror(*status);
      }

   }
 
/*
** Clean up and return.
*/
   free(buffer);
   if(ffclos(infp1, status)) Printerror(*status);
   if(ffclos(infp2, status)) Printerror(*status);

   return;
}

 
/* This code is needed by IRAF */
 
#ifdef vms
#define F77CALL fimgtri
#endif
#ifdef unix
#define F77CALL fimgtri_
#endif
 
void F77CALL()
{
  void fimgtrim();
  fimgtrim();
}
