/***********************************************************************
File : fimhisto.c
 
Description:
 
        calculates a histogram of an image file with user defined range of 
        values to be included for calculation, user defined binsize.  If 
        binsize is set to zero, then the tool internally calculates the size 
        of each bin.

Author:
       Banashree M Seifert (August, 1997) V1.0.0

Modifications:
       Banashree M Seifert (November, 1997) V1.1.0
          . it was giving segmentation fault even though it worked fine.
            now fixed
          . updated  *nulval = -1.e+30 instead of -99.0 to be 100% fool proof
      Peter D Wilson       (June 19   1998) V1.1.1
          . Updated ffopen sequence to support its new capabilities.
          . Added test for an empty image (NAXIS=0)
      Ning Gan       (July 28   1998) V1.1.2
          . Replaced the cfitsio.h with fitsio.h
      Ning Gan       (Dec 20  1999) V1.1.2
          . Updated for reading compressed images. 
   
 
Variables used:
   infile      char   input file name
   outfile     char   output file name
   subinfo     char   messages for display
   clobber     int    whether to overwrite existing file
   status      int    status at any instant
   lopres      int    low x value to be used for the task
                      =0 -- INDEF and task calculates internally
	              =1 -- user input low x value to be used
   uppres      int    upper x value to be used for the task
                        (same as lopres)
   binsize     float  size of bin user wants. If user wants to input
                        no. of bins then he has to input binsize<=0
   nbins       int    if binsize<=0, then it asks for no. of bins
   xmin        float  minimum value for histogram
                      if xmin=INDEF, then minimum value is to be
		      evaluated from the data
   xmax        float  maximum  value for histogram
                      if xmax=INDEF, then maximum value is to be
		      evaluated from the data
***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
 
#include "fitsio.h"        /* cfitsio routines */
#include "fitsio2.h"       /* cfitsio defined constants */
#include "xpi.h"           /* parameter file functions, e.g. Uclgst */
#include "cftools.h"       /* standard C library constants */
#include "fimhisto.h"      /* task specific definitions */
 
#ifndef MAX
#define MAX(A,B) (A>B? A:B)
#endif
#ifndef MIN
#define MIN(A,B) (A<B? A:B)
#endif



void fimhisto()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    char subinfo[C_FCERR_MSG];
    char version[6]="1.1.1";

    int clobber=0, status=0;
    int lopres=0, uppres=0;
    int nbins=0;

    float binsize=0., xmin=0., xmax=0.;

/*----------------------------------------------------------------*/
    c_ptaskn("FIMHISTO"); /* method to store task name for c_fcerr */

    fimhistoGetPar(infile, outfile, version, &lopres, &uppres, &xmin, &xmax, 
		   &binsize, &nbins, &clobber, &status);
    if(status) {
      strcpy(subinfo,"..... Error returning from fimhistoGetPar \n");
      DispMsg(0,0,subinfo);
      goto Exit_error;
    }

    fimhistoDo(infile, outfile, version, lopres, uppres, &xmin, &xmax,
               &binsize, &nbins, clobber, &status);
    if(status) {
      strcpy(subinfo,"..... Error returning from fimhistoDO \n");
      DispMsg(0,0,subinfo);
      goto Exit_error;
    }

    strcpy(subinfo,"****** successfully exited ******");
    DispMsg(0,0,subinfo);
    return;
 
Exit_error:
    strcpy(subinfo,"****** Error exited ******\n");
    DispMsg(0,0,subinfo);
    return;
}
 
/*******************************************************************
function:
      fimhistoGetPar
 
description:
      gets the parameters for the task.
 
author:
      Banashree M Seifert (August, 1997)
 
modification history:
      Peter D Wilson (June 19   1998)
         . Drop calls to CheckFile/CheckInFile... let CFITSIO deal with it
         . Change clobber handling so that it ADDS the '!' to outfile if set
           instead of the reverse

variables:
  (internal variables)
    token   char  pointer is required for the function STRTOK
    range   char  user input which is broken into range2 & range2
    range1  char  =xmin (after converted to float) 
                  =INDEF --> lopres=0
    range2  char  =xaxn (after converted to float)
                  =INDEF --> uppres=0

 
usage:
   void  fimhistoGetPar(char *infile, char *outfile, char *version, 
                        int *lopres, int *uppres,
                        float *xmin, float *xmax, float *binsize, int *nbins, 
                        int *clobber, int *status)
 
*********************************************************************/
void fimhistoGetPar(char *infile, char *outfile, char *version, 
		    int *lopres, int *uppres, 
		    float *xmin, float *xmax, float *binsize, int *nbins,
		    int *clobber, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;      /* required by cfortran.h*/
 
    char subinfo[C_FCERR_MSG];
    char *token;
    char range[FLEN_BUFFER];
    char range1[10],range2[10];

/* --------------------------------------------------------------*/
    *status=0;
    Uclgsb("clobber", clobber, status);
    sprintf(subinfo,"\n....Error reading clobber parameter\n");
    StatusChk(*status,subinfo);
 
    Uclgst("infile", infile, status);
 
    if(infile[0] =='\0'){
      *status=1;
      sprintf(subinfo,"\n....Error: filename is required \n");
      StatusChk(*status,subinfo);
    }
 
    sprintf(subinfo,"\n....Error reading infile parameter\n");
    StatusChk(*status,subinfo);
 
    Uclgst("outfile", outfile, status);
 
    if(outfile[0] == '\0'){
        *status = 1;
        sprintf(subinfo,"\n...Error: output filename is required\n");
        StatusChk(*status,subinfo);
    }
 
    sprintf(subinfo,"\n....Error reading outfile parameter \n");
    StatusChk(*status,subinfo);
 
    if( *clobber && outfile[0] != '!') {
       char tmp[FLEN_BUFFER];
       strcpy(tmp,outfile);
       outfile[0]='!';
       strcpy( outfile+1, tmp);
    }
 
    Uclgst("range", range, status);
    sprintf(subinfo,"....Error reading range parameter\n");
    StatusChk(*status,subinfo);

    /* -------------------------------------------------------------
       parse the parameter range into two parts, 
       range1 == xmin, range2 == xmax
    --------------------------------------------------------------*/

    token = strtok(range,",\t ");
    if(token == NULL){
      *status = 1;
      sprintf(subinfo, "... Error: parsing range parameter");
      StatusChk(*status,subinfo);
    }
    else{
      strcpy(range1,token);
      token = strtok('\0',",\t ");
      if(token == NULL) {
	*status = 1;
	sprintf(subinfo, "... Error: both lower & upper ranges are required");
	StatusChk(*status,subinfo);
      }
      else{
	strcpy(range2,token);
      }
    }
 
    if((range1[0] =='I') || (range1[0] == 'i')) *lopres = 0; 
    else{
      *lopres = 1;
      *xmin = atof(range1);
    }
    if((range2[0] =='I') || (range2[0] == 'i')) *uppres = 0; 
    else{
      *uppres = 1;
      *xmax = atof(range2);
    }

    Uclgsr("binsize", binsize, status);
    sprintf(subinfo,"....Error reading binsize parameter\n");
    StatusChk(*status,subinfo);

    if(*binsize <= 0.){
      Uclgsi("nbins", nbins, status);
      sprintf(subinfo,"....Error reading nbins parameter\n");
      StatusChk(*status,subinfo);
      if(*nbins <= 0){
	sprintf(subinfo,"....Error: No. of bins should be +VE integers\n");
	*status=1;
	StatusChk(*status,subinfo);
      }
    }


    return;
}
  
/*******************************************************************
function:
      fimhistoDo
 
description:
      things needed to do for the tool
 
author:
      Banashree M Seifert (August 1997)
      Banashree M Seifert (Nov    1997)
         . updated  *nulval = -1.e+30 instead of -99.0 to be 100% fool proof
           as suggested by Bill Pence

modification history:
      Peter D Wilson      (June   1998)
         . Updated ffopen sequence to support its new capabilities.
         . Added test for an empty image (NAXIS=0)
 
variable definitions:
   (internal variables)

   filename       char    required for C_FCPARS
   naxes          long    array of NofAxis
   nelem          long    total elements to be read at a time
   felem          long    first element to be read
   TotalElements  long    total no. of elements for image array
   Remain         long    no. of elements left to read
   TotalNoOfBins  long    total no. of bins
   BinNO          long    index of bin no.
   binptr         long    pointer since no. of bins required is unknown
   ExtnNo         int     extension no. returned from C_FCPARS
   anynul         int     true, if null value exists
   tfields        int     no. of fields(columns) to write to output
   hdutyp         int     return value for FFMAHD
   NofAxis        int     value of NAXIS for the image extension
   LastBinSize    float   binsize of last bin. 
                          This is needed when user uses binsize>0 since 
			  last binsize may be different than other bins.
   *imgptr        float   array of image values to read. Allocated memory
                          of BytesAtaTime=100K
   *nulval        float   =-99.0 to make faster
   *minvalptr     float   minimum values for each bin. Allocated memory of 
                          NoOfBins 
usage:
     void fimhistoDo(char *infile, char *outfile, char *version, int lopres, 
                     int uppres, float *xmin, float *xmax, float *binsize, 
		     int *nbins, int clobber, int *status)
*********************************************************************/
void fimhistoDo(char *infile, char *outfile, char *version, int lopres, 
		int uppres, float *xmin, float *xmax, float *binsize, 
		int *nbins, int clobber, int *status)
 {
  int BufLen_2= FLEN_BUFFER -1;     /* required by cfortran.h*/ 
  fitsfile *infp1=NULL;
  fitsfile *infp2=NULL;
 
  char filename[FLEN_BUFFER];
  char subinfo[C_FCERR_MSG];
  char comment[FLEN_COMMENT];
  char keywd[FLEN_KEYWORD];
  char *tform[]={"1E","1J"};
  char *tunit[]={" "," "};
  char *ttype[]={"XMIN", "NHIST"};
  char extname[]= "HISTOGRAM";
  char history[200];

  long naxes[5];
  long nelem, felem=1;
  long TotalElements=1, Remain;
  long TotalNoOfBins=0, BinNO=0;
  long *binptr=NULL;

  int ExtnNo=0;
  int anynul=0;
  int tfields=2, hdutyp=0, NofAxis=0;
  int i;

  float *imgptr=NULL;
  float *nulval=NULL;
  float *minvalptr=NULL;  
  float LastBinSize=0;

/* ------------------------------------------------------------------*/
  *status = 0;
  if(ffopen(&infp1, infile, READONLY, status)) Printerror(*status);
  ffghdn( infp1, &ExtnNo );
  if(ffinit(&infp2, outfile, status)) Printerror(*status);

  /* Copy Primary HDU */
  if(ffmahd(infp1, 1, &hdutyp, status)) Printerror(*status);
  if(ffcopy(infp1, infp2, 3, status)) Printerror(*status);
 
  if(ffmahd(infp1, ExtnNo, &hdutyp, status)) Printerror(*status); 

/*-----------------------------------------------------------------
  look for no. of AXIS and get total elements as one array
------------------------------------------------------------------*/
  if(ffgipr(infp1,5,NULL,&NofAxis, naxes,status)) {
     sprintf(subinfo, ".... Error: Could not read image information.\n");
     StatusChk(*status,subinfo);
     return;
  }
  
  if( NofAxis==0 ) {
     *status = 1;
     sprintf(subinfo, ".... Error: input image is empty.\n");
     StatusChk(*status,subinfo);
  }

  for(i=0; i<NofAxis; i++){
    TotalElements = TotalElements * naxes[i] ;
  }
  
/*-----------------------------------------------------------------
       allocate DMA to the arrays to be read
       imgptr = array of the image
------------------------------------------------------------------*/
  imgptr = (float *)malloc(BytesAtaTime);
  if(imgptr == NULL){
    *status=MEMORY_ALLOCATION;
    sprintf(subinfo, ".... Error: malloc failed for image array\n");
    StatusChk(*status,subinfo);
  }
  
  nelem = BytesAtaTime/sizeof(float);
  nelem = MIN(TotalElements,nelem);
  Remain = TotalElements;
  felem = 1;

/*-----------------------------------------------------------------
  if limits are INDEF, then find out the maximum & minimum values
------------------------------------------------------------------*/
  if(!uppres) *xmax=1.e-30;
  if(!lopres) *xmin=1.e+30;

  if((!lopres) || (!uppres)){
    for(;;){
      for(i=0;i<nelem; i++) imgptr[i] = 0; 
      if(ffgpv(infp1, TFLOAT, felem, nelem, nulval, imgptr,
	       &anynul,status)) Printerror(*status);
      if(!lopres){
	for(i=0; i<nelem; i++) *xmin = MIN(*xmin, imgptr[i]);
      }

      if(!uppres){
	for(i=0; i<nelem; i++) *xmax = MAX(*xmax, imgptr[i]);
      }
      
      if(Remain <= nelem) break;
      Remain = Remain - nelem ;
      nelem = MIN(Remain,nelem);
      felem = felem + nelem ;
    }
  }

/*-----------------------------------------------------------------
  end of finding the maximum & minimum values
  Now, find out the size of each bin and size of last bin
------------------------------------------------------------------*/
  if(*binsize > 0) {
    TotalNoOfBins = (long)((*xmax - *xmin) / *binsize);
    LastBinSize = (*xmax) - ((*xmin) + (TotalNoOfBins*(*binsize)));
  
    if(LastBinSize){
      TotalNoOfBins +=1;
      sprintf(subinfo, "... Warning: Last binsize is %f rather than "\
	      "requested size %f",LastBinSize, *binsize);
      DispMsg(0,0,subinfo);
      sprintf(subinfo, "... Proceeding with this binning .......");
      DispMsg(0,0,subinfo);
    }
  }
  else{
    TotalNoOfBins = (long) *nbins;
    *binsize = (float) ((*xmax - *xmin) / TotalNoOfBins );
  }
  	
/*-----------------------------------------------------------------
  allocate binptr = total no. of bins 
------------------------------------------------------------------*/

  binptr = (long *)malloc(TotalNoOfBins*sizeof(long));
  if(binptr == NULL){
    *status=MEMORY_ALLOCATION;
    sprintf(subinfo, ".... Error: malloc failed for binptr\n");
    StatusChk(*status,subinfo);
  }

  for(i=0; i<TotalNoOfBins; i++) binptr[i] = 0;

  minvalptr = (float *)malloc(TotalNoOfBins*sizeof(float));
  if(minvalptr == NULL){
    *status=MEMORY_ALLOCATION;
    sprintf(subinfo, ".... Error: malloc failed for minvalptr\n");
    StatusChk(*status,subinfo);
  }
 
/*-----------------------------------------------------------------
  determine the minimum value of each bin. 
  The maximum value is < minimum value of the previous bin
------------------------------------------------------------------*/
  minvalptr[0] = *xmin;
  for(i=1; i<TotalNoOfBins; i++) minvalptr[i] = minvalptr[i-1] + (*binsize);

  nelem = BytesAtaTime/sizeof(float);
  nelem = MIN(TotalElements,nelem);
  Remain = TotalElements;
  felem = 1;

  nulval = malloc(sizeof(float));
  *nulval = -1.e+30;

  for(;;){
    for(i=0;i<nelem; i++) imgptr[i] = 0; 

    if(ffgpv(infp1, TFLOAT, felem, nelem, nulval, imgptr,
	     &anynul,status)) Printerror(*status);
    for(i=0; i<nelem; i++) {
      if((imgptr[i] > *xmax) || (imgptr[i] < *xmin)) continue;

      BinNO = (long)((imgptr[i] - *xmin) / *binsize)  ;
      if(BinNO < 0) {
	sprintf(subinfo,"bin no is -ve: %d\n",BinNO);
        *status=1;
	StatusChk(*status,subinfo);
      }
      
      /*-------------------------------------------------------------------
	This IF is added so that to include the last value (xmax)
	-------------------------------------------------------------------*/
      if(BinNO == TotalNoOfBins) BinNO = BinNO - 1;
      
      binptr[BinNO] += 1;
      
    }

    if(Remain <= nelem) break;
    Remain = Remain - nelem ;
    nelem = MIN(Remain,nelem);
    felem = felem + nelem ;
  }

  free(imgptr);
  free(nulval);

  if(ffcrtb(infp2, BINARY_TBL, TotalNoOfBins, tfields, ttype, tform, tunit, 
	    extname, status)) Printerror(*status);

  if(ffhdef(infp2, 20, status)) Printerror(*status);

/*----------------------- add keyword BINSIZE -------------------------*/

  strcpy(comment, "size of each bin");
  if(ffpkye(infp2, "BINSIZE", *binsize, 4, comment, status)) Printerror(*status);

/*----------------------- write data now -------------------------*/
  if(ffpcl(infp2, TFLOAT, 1, 1L, 1L, TotalNoOfBins, minvalptr, status)) 
    Printerror(*status);
  if(ffpcl(infp2, TLONG, 2, 1L, 1L, TotalNoOfBins, binptr, status)) 
    Printerror(*status);

  free(binptr);
  free(minvalptr);

/*----------------------------------------------------------------------
  put some history and/or comments keywords
-----------------------------------------------------------------------*/
  strcpy(subinfo,"FIMHISTO ");
  strcat(subinfo, version);
  if(ffpkys(infp2, "CREATOR", subinfo, " s/w task that wrote this dataset",
            status)) Printerror(*status);
  
  sprintf(history, "--------------------------------------------------------");
  if(ffphis(infp2, history, status)) Printerror(*status);
  
  sprintf(history,"input file \"%s\"",infile);
  if(ffphis(infp2, history, status)) Printerror(*status);
  
  sprintf(history,"minimumvalue for histogram = %8.2f ",*xmin);
  if(ffphis(infp2, history, status)) Printerror(*status);
  
  sprintf(history,"maximumvalue for histogram = %8.2f ",*xmax);
  if(ffphis(infp2, history, status)) Printerror(*status);
  
  sprintf(history,"binsize = %f ", *binsize);
  if(ffphis(infp2, history, status)) Printerror(*status);

  sprintf(history,"total no. of bins = %ld ",TotalNoOfBins);
  if(ffphis(infp2, history, status)) Printerror(*status);
  
  if(LastBinSize){
    sprintf(history, "last binsize = %.2f",LastBinSize);
    if(ffphis(infp2, history, status)) Printerror(*status);
  }

  sprintf(history,"only XMIN values are written in the Table, since");
  if(ffphis(infp2, history, status)) Printerror(*status);
  strcpy(history, "     XMAX (i)       < XMIN (i+1) & ");
  if(ffphis(infp2, history, status)) Printerror(*status);
  sprintf(history,"     XMAX (lastbin) <= %f",*xmax);
  if(ffphis(infp2, history, status)) Printerror(*status);

  sprintf(history, "--------------------------------------------------------");
  if(ffphis(infp2, history, status)) Printerror(*status);
  if(ffpdat(infp2, status)) Printerror(*status);

  if(ffclos(infp1, status)) Printerror(*status);
  if(ffclos(infp2, status)) Printerror(*status);

  return;
}

/* This code is needed by IRAF */
 
#ifdef vms
#define F77CALL fimhisto
#endif
#ifdef unix
#define F77CALL fimhisto_
#endif
 
void F77CALL()
{
 void fimhisto();
 
 fimhisto();
}


