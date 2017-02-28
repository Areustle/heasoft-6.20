/***********************************************************************
File :chimgtyp.c
 
Description:
   changes the datatype of an input image file.
   details logic by Bill Pence

    Case A: If output datatype is float or double (regardless of input type):
            Read the input image into a float or double array, with 
	    fits_read_img, setting any undefined pixels equal to the value 
	    of fnull.  Then write the image out with fits_write_imgnull_flt 
	    (or _dbl) using the value of fnull for the nulval parameter.  
	    If the input image was an integer datatype be sure to delete any 
	    BSCALE, BZERO, and BLANK keywords from the output header before 
	    writing the data so that the values are not scaled again.

    Case B: If the output is an integer type then:
            If the input image is a float/double use the value of Inull for 
	    the output BLANK keyword  then write the values to the integer 
	    output image with fits_write_imgnull_[], using inull as the value 
	    for the nulval parameter.  If the output type is USHORT or ULONG, 
	    you also have to write the BZERO keyword = 32768 or 2147483648, 
	    first.  If there are no undefined pixels in the input image, then 
	    delete the BLANK keyword from the output image.
 
	    If the input integer image is an integer and has a BSCALE or BZERO
	    keyword not equal to 1 and 0, respectively, treat it the same as the
	    double input case, above, except reuse the input BLANK value in the
	    output file, if it is within range, otherwise use the inull parameter
	    file value for the output BLANK keyword.  If there is no input BLANK
	    keyword, then there will be no output BLANK keyword either.
 
	    Otherwise, if input datatype is an integer and there are no BSCALE,
	    BZERO keywords, then simply read the input image into a long integer
	    array using the value of the BLANK keyword, if it exists, for value of
	    any undefined pixels.  If BLANK exists and is within the range of the
	    output datatype, then reuse it, otherwise, use the value of Inull for
	    the output BLANK keyword if it is within range, otherwise quit with an
	    error.  If the BLANK keyword exists, use fits_write_imgnull_[] to write
	    the image, otherwise just use fits_write_img_[].  If the output type is
	    USHORT or ULONG, you also have to write the BZERO keyword = 32768 or
	    2147483648, first.
 

Author:
       Banashree M Seifert (August 1997)V1.0.0
       Banashree M Seifert (Nov    1997)V1.1.0

Modifications:
       Peter D Wilson (June 1998)V1.2.0
             Updated for new file-handling procedures... primarily, remove
             calls to CheckFile, CheckInFile, and CreateNewFits, the later
             copied here as chimgtypNewFile and similarly modified
       Ning Gan  (July 1998) v1.3.0
	    replaced the cfitsio.h with fitsio.h.
       Peter D Wilson (Sept 1999) V1.4.0
            Fix logic in calculating elements left to do 
       Ning Gan (Dec 1999) V1.5.0
            Updated for reading the compressed image. 
       Ziqin Pan (Mar 2005 ) V1.6.0
            Deal the case when input file has Nans

Variables used:
    infile         char   input file name
    outfile        char   output file name
    status         int    status at any instant
    clobber        int    whether to overwrite existing file
    datatype       char   possible datatypes (along with bitpix) are    
                          UBYTE        8
			  USHORT      16
			  SHORT       16
			  ULONG       32
			  LONG        32
			  FLOAT      -32
			  DOUBLE     -64
    Inull          int    default null value (a hidden paramater)=-99
    Fnull          float  default null value (a hidden paramater)=-1e37 
    copyall        int    hidden parameter for,
                          if true,  copy all other extensions
                             false, donot copy any other extensions
***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
 
#include "ftools.h"        /* for fortran interface */
#include "fitsio.h"        /* cfitsio routines */
#include "fitsio2.h"       /* cfitsio defined constants */
#include "xpi.h"           /* parameter file functions, e.g. Uclgst */
#include "ftoolstruct.h"   /* C-Fortran common blocks */
#include "cftools.h"       /* standard C library constants */
#include "chimgtyp.h"      /* task specific definitions */
 
void chimgtyp()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    char subinfo[100];
    char datatype[FLEN_BUFFER];

    int Inull=0, copyall=1, clobber=0, status=0;

    float Fnull=0.;

/*----------------------------------------------------------------*/

    chimgtypGetPar(infile, outfile, datatype, &Fnull, &Inull, &copyall, 
		   &clobber, &status);
    if(status) {
      strcpy(subinfo,"..... Error returning from FIMGMERGEDO \n");
      DispMsg(0,0,subinfo);
      goto Exit_error;
    }

    chimgtypDo(infile, outfile, datatype, &Fnull, &Inull, &copyall, 
	       &clobber, &status);
    if(status) {
      strcpy(subinfo,"..... Error returning from FIMGMERGEDO \n");
      DispMsg(0,0,subinfo);
      goto Exit_error;
    }
    
    strcpy(subinfo,"****** successfully exited ******\n");
    DispMsg(0,0,subinfo);
    return;

Exit_error:
    strcpy(subinfo,"****** Error exited ******\n");
    DispMsg(0,0,subinfo);
    return;
}

/*******************************************************************
function:
      chimgtypGetPar
 
description:
      gets the parameters for the task.
 
author:
      Banashree M Seifert (August, 1997)
 
modification history:
 
usage:
   void  chimgtypGetPar(char *infile, char *outfile, char *datatype, 
                        float *Fnull, int *Inull, int *copyall, 
			int *clobber, int *status)

*********************************************************************/
void chimgtypGetPar(char *infile, char *outfile, char *datatype, 
		    float *Fnull, int *Inull, int *copyall, 
		    int *clobber, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;      /* required by cfortran.h*/
 
    char subinfo[100];
    char datatypeTemplate[][7] = {"UBYTE", "USHORT", "SHORT", "ULONG",
				  "LONG", "FLOAT", "DOUBLE"};

    int casesen=0, match=0, exact=1;
    int i=0;

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

    /*  PDW 6/25/98: Let FFOPEN check for file's existence 
    CheckInFile(infile);
    */
    
    Uclgst("outfile", outfile, status);
 
    if(outfile[0] == '\0'){
        *status = 1;
        sprintf(subinfo,"\n...Error: output filename is required\n");
	StatusChk(*status,subinfo);
    }


    sprintf(subinfo,"\n....Error reading outfile parameter \n");
    StatusChk(*status,subinfo);

    /* PDW 6/25/98: Use the "bang" to flag overwrite */
    if(*clobber && outfile[0] != '!') {
       char tmp[FLEN_BUFFER];
       strcpy(tmp,outfile);
       outfile[0]='!';
       strcpy(outfile+1,tmp);
    }
 
    for(;;){
      Uclgst("datatype", datatype, status);
      sprintf(subinfo,"....Error reading datatype parameter \n");
      StatusChk(*status,subinfo);
      ffupch(datatype);
      
      for(i=0; i<7; i++){
	ffcmps(datatypeTemplate[i],datatype, casesen, &match, &exact);
        if(exact) goto NEXT;
      } 
      
      sprintf(subinfo, " FITS defined data types are: "\
               "UBYTE, USHORT, SHORT, ULONG, LONG, FLOAT, DOUBLE");
      DispMsg(0,0,subinfo);
      sprintf(subinfo,".. Try again!\n");
      DispMsg(0,0,subinfo);
    }

NEXT:

    Uclgsr("Fnull", Fnull, status);
    sprintf(subinfo,"....Error reading Fnull parameter\n");
    StatusChk(*status,subinfo);

    Uclgsi("Inull", Inull, status);
    sprintf(subinfo,"....Error reading Inull parameter\n");
    StatusChk(*status,subinfo);

    Uclgsb("copyall", copyall, status);
    sprintf(subinfo,"\n....Error reading copyall  from parameter file\n");
    StatusChk(*status,subinfo);

    return;
}
 
/*******************************************************************
function:
      chimgtypDo
 
description:
      things needed to do for the tool
 
author:
      Banashree M Seifert (August 1997)
      Banashree M Seifert (Nov    1997)
      . many modifications are made for changing the datatypes for various 
        cases as pointed out by Bill Pence. 
      Peter D Wilson (Sept 1999)
      . Fix logic in calculating elements left to do 
 
variable definitions:

         *Carrptr    unsigned Char  pointer for UBYTE array
	 *USarrptr   unsigned short pointer for USHORT array
         *Sarrptr    short          pointer for SHORT array
	 *ULarrptr   unsigned short pointer for ULONG array
         *Larrptr    long           pointer for LONG array
         *Farrptr    float          pointer for FLOAT array
         *Darrptr    double         pointer for DOUBLE array

         *Cnulval    unsigned char  pointer for BYTE null value
	 *Snulval    short          pointer for SHORT null value
	 *Lnulval    long           pointer for LONG null value
	 *Fnulval    float          pointer for FLOAT null value
	 *Dnulval    double         pointer for DOUBLE null value
      OldDataType    char           input datatype
 datatypeTemplate    char           7 FITS supported dadatypes
                                    UBYTE, USHORT, SHORT, ULONG, LONG,
				    FLOAT & DOUBLE
             card    char           to copy as a whole, i.e, keyword 
	                            with value and comment. This is 
				    used to copy as it is from input 
				    to output
            naxes    long           no. of axes in input data
            nelem    long           no. of elements to read at a time
            felem    long           first elelemt
    TotalElements    long           total no. of elements while as an array
                                    This is the product of 
				    naxes[0]*naxes[1]*.......
           Remain    long           no. of elements remaining to read
           bitpix    int            possible are
                                    UBYTE=8, USHORT=16, SHORT=16
				    ULONG=32, LONG=32, FLOAT=-32
				    DOUBLE=-64
           anynul    int            true if undefined values are present
                                    otherwise false
          NofAxis    int            total no. of axes present in input
	                            dimensions of the input image
            match    int            
	    exact    int
	   nulval    int
	    nkeys    int          
	   keynum    int
	   hdutyp    int
	    blank    int
     BlankPresent    int

usage:
     void  chimgtypDo(char *infile, char *outfile, char *datatype, 
                      int *copyall, int *clobber, int *status)
*********************************************************************/
void chimgtypDo(char *infile, char *outfile, char *datatype, 
		float *Fnull, int *Inull, int *copyall, 
		int *clobber, int *status)
{
  int BufLen_2= FLEN_BUFFER -1;      /* required by cfortran.h*/
  fitsfile *infp1=NULL;
  fitsfile *infp2=NULL;
  
  char subinfo[100];
  char datatypeTemplate[][7] = {"UBYTE", "USHORT", "SHORT", "ULONG",
				"LONG", "FLOAT", "DOUBLE"};
  char history[200], nulHistory[200] ;
  char OldDataType[10];
  char comment[FLEN_COMMENT];
  char card[FLEN_CARD];

  unsigned char  *Carrptr  =NULL, *Cnulval=NULL;
  unsigned short *USarrptr =NULL, *USnulval=NULL;
  short          *Sarrptr  =NULL, *Snulval=NULL;
  unsigned long  *ULarrptr =NULL, *ULnulval=NULL;
  long           *Larrptr  =NULL, *Lnulval=NULL;
  float          *Farrptr  =NULL, *Fnulval=NULL;
  double         *Darrptr  =NULL, *Dnulval=NULL;

  long naxes[5];
  long nelem, felem=1;
  long TotalElements=1, Remain;
  int bitpix=0;

  int anynul=0, NofAxis=0;
  int i=0, casesen=0, match=0, exact=0;
  int nkeys=0, keynum=0, hdutyp=0;
  int blank=0, BlankPresent=0;


/* -----------------------------------------------------------*/
  *status = 0;
  if(ffopen(&infp1, infile, READONLY, status)) Printerror(*status);

  if(ffghps(infp1, &nkeys, &keynum, status)) Printerror(*status);

/*-----------------------------------------------------------------
  look for bitpix, no. of AXIS and get total elements as one array
------------------------------------------------------------------*/

  if(fits_get_img_param(infp1, 5, &bitpix, &NofAxis, naxes,status))
         Printerror(*status);

  for(i=0; i<NofAxis; i++){
      TotalElements = TotalElements * naxes[i] ;
  }

  if(bitpix == 8)        strcpy(OldDataType,"byte");
  else if(bitpix ==  16) strcpy(OldDataType,"short");
  else if(bitpix ==  32) strcpy(OldDataType,"long");
  else if(bitpix == -32) strcpy(OldDataType,"float");
  else if(bitpix == -64) strcpy(OldDataType,"double");

  ffupch(OldDataType);
  ffupch(datatype);

/*-----------------------------------------------------------------
                       look for BLANK keyword
------------------------------------------------------------------*/
  if((bitpix == 8) || (bitpix == 16) || (bitpix == 32)){
    if(ffgky(infp1, TINT, "BLANK", &blank, comment, status)) {
      *status=0; 
      BlankPresent=0;
    }
    else{
      BlankPresent=1;
    }
  } 

/*-----------------------------------------------------------------
        find out which DATATYPE requested for output and accordingly
        determine switch value (in this case it is 'i')

          i      datatype    bitpix
         ---     --------    ------
          0       UBYTE        8
	  1       USHORT      16
	  2       SHORT       16
	  3       ULONG       32
	  4       LONG        32
	  5       FLOAT      -32
	  6       DOUBLE     -64
------------------------------------------------------------------*/
  for(i=0; i<7; i++){
    ffcmps(datatypeTemplate[i], datatype, casesen, &match, &exact);
    if(exact) break;
  } 

  switch(i){
     case 0: 
       Cnulval = malloc(sizeof(unsigned char));
       if(BlankPresent) {
	 if((blank >=0) &&( blank <= UCHAR_MAX)){
          *Cnulval=(unsigned char) blank ;  
	 }
	 else
	 { 
	     if((*Inull >=0) && (*Inull <= UCHAR_MAX)) *Cnulval=(unsigned char)*Inull; 
	     else{
	       sprintf(subinfo,
		       " Nullvalue out of bounds for datatype \"%s\"",datatype);
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,
		       " Please try again with Inull=<nulvalue> on command line");
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,
		       " Range of Nullvalue for %s is 0 - %d",datatype,UCHAR_MAX);
	       DispMsg(0,0,subinfo);
	       *status=1;
	       return;
	     }
	 }
       }
       else 
       {
	 if((*Inull >=0) && (*Inull <= UCHAR_MAX)) *Cnulval=(unsigned char)*Inull; 
	 else
	   *Cnulval= 0;
       }

       sprintf(subinfo, 
       "Any undefined pixel values (if applicable) are set to %u",*Cnulval);
       strcpy(nulHistory, subinfo);

       nelem = BytesAtaTime/sizeof(unsigned char);

       Carrptr = malloc(BytesAtaTime);
       if(Carrptr == NULL){ 
	 *status=1;
	 sprintf(subinfo, ".... Error: malloc for char array failed \n"); 
	 StatusChk(*status,subinfo);
       }

       chimgtypNewFile(&infp2, outfile, 1, BYTE_IMG, NofAxis, naxes, 
		       0L, 1L, 1, status);

       Remain = TotalElements;
       felem = 1;

       if(ffukyj(infp2, "BLANK", *Cnulval, "NULL value", status)) Printerror(*status);
       for(;;){

         nelem = minvalue(Remain,nelem);
	 for(i=0;i<nelem; i++) {Carrptr[i] = 0;}


	 if(ffgpv(infp1, TBYTE, felem, nelem, Cnulval, Carrptr, 
		  &anynul,status)) Printerror(*status);

	 if(ffppn(infp2, TBYTE,felem, nelem, Carrptr, Cnulval, status)) 
	   Printerror(*status); 

         if(Remain <= nelem) break;
         Remain = Remain - nelem ;
         felem = felem + nelem ;
       }

       free(Carrptr);
       free(Cnulval);

       break;	

     case 1: 
       USnulval = malloc(sizeof(unsigned short));
       if(BlankPresent) {
	 if((blank >=0) &&( blank <= USHRT_MAX)){
          *USnulval=(unsigned short)blank ;  
	 }
	 else
	 { 
	     if((*Inull >=0) && (*Inull <= USHRT_MAX)) *USnulval=(unsigned short)*Inull; 
	     else{
	       sprintf(subinfo,
		       " Nullvalue out of bounds for datatype \"%s\"",datatype);
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,
		       " Please try again with Inull=<nulvalue> on command line");
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,		   
		       " Range of Nullvalue for %s is 0 - %hu",datatype,USHRT_MAX);
	       DispMsg(0,0,subinfo);
	       *status=1;
	       return;
	     }
	 }
       }
       else 
       {
	 if((*Inull >=0) && (*Inull <= USHRT_MAX)) *USnulval=(unsigned short)*Inull; 
	 else{
	   sprintf(subinfo,
		   " Nullvalue out of bounds for datatype \"%s\"",datatype);
	   DispMsg(0,0,subinfo);
	   sprintf(subinfo,
		   " Please try again with Inull=<nulvalue> on command line");
	   DispMsg(0,0,subinfo);
	   sprintf(subinfo,		   
		   " Range of Nullvalue for %s is 0 - %hu",datatype,USHRT_MAX);
	   DispMsg(0,0,subinfo);
	   *status=1;
	   return;
	 }
       }

       sprintf(subinfo, 
       "Any undefined pixel values (if applicable) are set to %hu",*USnulval);
       strcpy(nulHistory, subinfo);
/*
       if((!strcmp(OldDataType, "FLOAT")) || (!strcmp(OldDataType, "DOUBLE"))){
	 if(ffdkey(infp2, "BZERO", status)) {*status = 0; ffcmsg(); }
	 if(ffpkyl(infp2, "BZERO", 32768, "zero value", status)) Printerror(*status);
       }
*/

       nelem = BytesAtaTime/sizeof(unsigned short);

       USarrptr = malloc(BytesAtaTime);
       if(USarrptr == NULL){ 
	 *status=1;
	 sprintf(subinfo, ".... Error: malloc for (U)short array failed \n"); 
	 StatusChk(*status,subinfo);
       }

       chimgtypNewFile(&infp2, outfile, 1, USHORT_IMG, NofAxis, naxes, 
		       0L, 1L, 1, status);

       if((!strcmp(OldDataType, "FLOAT")) || (!strcmp(OldDataType, "DOUBLE"))){
	 if(ffdkey(infp2, "BZERO", status)) {*status = 0; ffcmsg(); }
	 if(ffpkyj(infp2, "BZERO", 32768, "zero value", status)) Printerror(*status);
       }
 
       Remain = TotalElements;
       felem = 1;

       if(ffukyj(infp2, "BLANK", *USnulval, "NULL value", status)) Printerror(*status);
       for(;;){

         nelem = minvalue(Remain,nelem);
	 for(i=0;i<nelem; i++) {USarrptr[i] = 0;}

	 if(ffgpv(infp1,TUSHORT, felem, nelem, USnulval, USarrptr, 
		  &anynul,status)) Printerror(*status);

	 if(ffppnui(infp2, 1L, felem, nelem, USarrptr, *USnulval, status))
	   Printerror(*status); 

         if(Remain <= nelem) break;
         Remain = Remain - nelem ;
         felem = felem + nelem ;
       }

       free(USarrptr);
       free(USnulval);

       break;

     case 2: 
       Snulval = malloc(sizeof(short));
       if(BlankPresent) 
       {
	 if((blank >= SHRT_MIN) &&( blank <= SHRT_MAX)){
          *Snulval=(short)blank ;  
	 }
	 else
	 { 
	     if((*Inull >=SHRT_MIN) && (*Inull <= SHRT_MAX)) *Snulval=(short)*Inull; 
	     else{
	       sprintf(subinfo,
		       " Nullvalue out of bounds for datatype \"%s\"",datatype);
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,
		       " Please try again with Inull=<nulvalue> on command line");
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,		   
		" Range of Nullvalue for %s is %hd - %hd",datatype,SHRT_MIN,SHRT_MAX);
 	       DispMsg(0,0,subinfo);
	       *status=1;
	       return;
	     }
	 }
       }
       else 
       {
	 if((*Inull >= SHRT_MIN) && (*Inull <= SHRT_MAX)) *Snulval=(short)*Inull; 
	 else
         {
	   sprintf(subinfo,
		   " Nullvalue out of bounds for datatype \"%s\"",datatype);
	 DispMsg(0,0,subinfo);
	 sprintf(subinfo,
		 " Please try again with Inull=<nulvalue> on command line");
	 DispMsg(0,0,subinfo);
	 sprintf(subinfo,		   
		" Range of Nullvalue for %s is %hd - %hd",datatype,SHRT_MIN,SHRT_MAX);
	 DispMsg(0,0,subinfo);
	 *status=1;
	 return;
	 }
       }	 
       

       sprintf(subinfo, 
       "Any undefined pixel values (if applicable) are set to %hd",*Snulval);
       strcpy(nulHistory, subinfo);

       nelem = BytesAtaTime/sizeof(short);

       Sarrptr = malloc(BytesAtaTime);
       if(Sarrptr == NULL){ 
	 *status=1;
	 sprintf(subinfo, ".... Error: malloc for short array failed \n"); 
	 StatusChk(*status,subinfo);
       }

       chimgtypNewFile(&infp2, outfile, 1, SHORT_IMG, NofAxis, naxes, 
		       0L, 1L, 1, status);

       Remain = TotalElements;
       felem = 1;
       if(ffukyj(infp2, "BLANK", *Snulval, "NULL value", status)) Printerror(*status);

       for(;;){

         nelem = minvalue(Remain,nelem);
	 for(i=0;i<nelem; i++) {Sarrptr[i] = 0;}

	 if(ffgpv(infp1, TSHORT, felem, nelem, Snulval, Sarrptr,
		  &anynul, status))  Printerror(*status);

	 if(ffppni(infp2, 1L, felem, nelem, Sarrptr, *Snulval, status))
	   Printerror(*status); 
	 
         if(Remain <= nelem) break;
         Remain = Remain - nelem ;
         felem = felem + nelem ;
       }     

       free(Sarrptr);
       free(Snulval);

       break;

     case 3: 
       ULnulval = malloc(sizeof(unsigned long));
       if(BlankPresent) {
	 if((blank >= 0) &&( blank <= ULONG_MAX)){
          *ULnulval=(unsigned long)blank ;  
	 }
	 else
	 { 
	     if((*Inull >= 0) && (*Inull <= ULONG_MAX)) *ULnulval=(unsigned long)*Inull; 
	     else{
	       sprintf(subinfo,
		       " Nullvalue out of bounds for datatype \"%s\"",datatype);
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,
		       " Please try again with Inull=<nulvalue> on command line");
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,		   
		 " Range of Nullvalue for %s is 0 - %uld",datatype,ULONG_MAX);
 	       DispMsg(0,0,subinfo);
	       *status=1;
	       return;
	     }
	 }
       }
       else 
       {
	 if((*Inull >= 0) && (*Inull <= ULONG_MAX)) *ULnulval=(unsigned long)*Inull; 
	 else
	 {
	       sprintf(subinfo,
		       " Nullvalue out of bounds for datatype \"%s\"",datatype);
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,
		       " Please try again with Inull=<nulvalue> on command line");
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,		   
		 " Range of Nullvalue for %s is 0 - %uld",datatype,ULONG_MAX);
 	       DispMsg(0,0,subinfo);
	       *status=1;
	       return;
	 }
       }

       sprintf(subinfo, 
       "Any undefined pixel values (if applicable) are set to %ld",*ULnulval);
       strcpy(nulHistory, subinfo);
/*
       if((!strcmp(OldDataType, "FLOAT")) || (!strcmp(OldDataType, "DOUBLE"))){
	 if(ffdkey(infp2, "BZERO", status)) {*status = 0; ffcmsg(); }
	 if(ffpkyj(infp2, "BZERO", 2147483648UL, "zero value", status)) Printerror(*status);
       }
*/

  
       nelem = BytesAtaTime/sizeof(unsigned long);

       ULarrptr = malloc(BytesAtaTime);
       if(ULarrptr == NULL){ 
	 *status=1;
	 sprintf(subinfo, ".... Error: malloc for (U)long array failed \n"); 
	 StatusChk(*status,subinfo);
       }

       chimgtypNewFile(&infp2, outfile, 1, ULONG_IMG, NofAxis, naxes, 
		       0L, 1L, 1, status);
       if((!strcmp(OldDataType, "FLOAT")) || (!strcmp(OldDataType, "DOUBLE"))){
	 if(ffdkey(infp2, "BZERO", status)) {*status = 0; ffcmsg(); }
	 if(ffpkyd(infp2, "BZERO", 2147483648,10, "zero value", status)) Printerror(*status);
       }
 
       Remain = TotalElements;
       felem = 1;
       if(ffukyj(infp2, "BLANK", *ULnulval, "NULL value", status)) Printerror(*status);

       for(;;){

         nelem = minvalue(Remain,nelem);
	 for(i=0;i<nelem; i++) {ULarrptr[i] = 0;}

	 if(ffgpv(infp1, TULONG, felem, nelem, ULnulval, ULarrptr, 
		  &anynul,status)) Printerror(*status);
         if(ffppnuj(infp2, 1L, felem, nelem, ULarrptr, *ULnulval, status))
	   Printerror(*status);

         if(Remain <= nelem) break;
         Remain = Remain - nelem ;
         felem = felem + nelem ;
       }

       free(ULarrptr);
       free(ULnulval);
 
       break;	

     case 4: 
       Lnulval = malloc(sizeof(long));
       if(BlankPresent) {
	 if((blank >= LONG_MIN) &&( blank <= LONG_MAX)){
          *Lnulval=(long)blank ;  
	 }
	 else
	 { 
	     if((*Inull >= LONG_MIN) && (*Inull <= LONG_MAX)) *Lnulval=(long)*Inull; 
	     else{
	       sprintf(subinfo,
		       " Nullvalue out of bounds for datatype \"%s\"",datatype);
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,
		       " Please try again with Inull=<nulvalue> on command line");
	       DispMsg(0,0,subinfo);
	       sprintf(subinfo,		  
	        " Range of Nullvalue for %s is %ld - %ld",datatype,LONG_MIN,LONG_MAX);
 	       DispMsg(0,0,subinfo);
	       *status=1;
	       return;
	     }
	 }
       }
       else 
       {
	 if((*Inull >= LONG_MIN) && (*Inull <= LONG_MAX)) *Lnulval=(long)*Inull; 
	 else
	 {
	   sprintf(subinfo,
		   " Nullvalue out of bounds for datatype \"%s\"",datatype);
	   DispMsg(0,0,subinfo);
	   sprintf(subinfo,
		   " Please try again with Inull=<nulvalue> on command line");
	   DispMsg(0,0,subinfo);
	   sprintf(subinfo,		  
	       " Range of Nullvalue for %s is %ld - %ld",datatype,LONG_MIN,LONG_MAX);
	   DispMsg(0,0,subinfo);
	   *status=1;
	   return;
	 }
       }

       sprintf(subinfo, 
       "Any undefined pixel values (if applicable) are set to %ld",*Lnulval);
       strcpy(nulHistory, subinfo);

       nelem = BytesAtaTime/sizeof(long);

       Larrptr = malloc(BytesAtaTime);
       if(Larrptr == NULL){ 
	 *status=1;
	 sprintf(subinfo, ".... Error: malloc for long array failed \n"); 
	 StatusChk(*status,subinfo);
       }

       chimgtypNewFile(&infp2, outfile, 1, LONG_IMG, NofAxis, naxes, 
		       0L, 1L, 1, status);

       Remain = TotalElements;
       felem = 1;
       if(ffukyj(infp2, "BLANK", *Lnulval, "NULL value", status)) Printerror(*status);

       for(;;){

         nelem = minvalue(Remain,nelem);
	 for(i=0;i<nelem; i++) {Larrptr[i] = 0; }

	 if(ffgpv(infp1, TLONG, felem, nelem, Lnulval, Larrptr, 
		  &anynul, status)) Printerror(*status);

	 if(ffppnj(infp2, 1L, felem, nelem, Larrptr, *Lnulval, status))
	   Printerror(*status); 
	 
         if(Remain <= nelem) break;
         Remain = Remain - nelem ;
         felem = felem + nelem ;
       }

       free(Larrptr);
       free(Lnulval);

       break;

     case 5: 
       Fnulval = malloc(sizeof(float));
       *Fnulval=*Fnull; 

       sprintf(subinfo, 
       "Any undefined pixel values (if applicable) are set to %.2e",*Fnulval);
       strcpy(nulHistory, subinfo);

       nelem = BytesAtaTime/sizeof(float);

       Farrptr = malloc(BytesAtaTime);
       if(Farrptr == NULL){ 
	 *status=1;
	 sprintf(subinfo, ".... Error: malloc for float array failed \n"); 
	 StatusChk(*status,subinfo);
       }

       chimgtypNewFile(&infp2, outfile, 1, FLOAT_IMG, NofAxis, naxes, 
		       0L, 1L, 1, status);

       Remain = TotalElements;
       felem = 1;


       for(;;){

         nelem = minvalue(Remain,nelem);
	 for(i=0;i<nelem; i++) {Farrptr[i] = 0; }

	 if(ffgpv(infp1, TFLOAT, felem, nelem, Fnulval, Farrptr,
		  &anynul,status)) Printerror(*status);

	 if(ffppne(infp2, 1L, felem, nelem, Farrptr, *Fnulval, status))
	   Printerror(*status); 

         if(Remain <= nelem) break;
         Remain = Remain - nelem ;
         felem = felem + nelem ;
       }

       free(Farrptr);
       free(Fnulval);

       break;

     case 6: 
       Dnulval = malloc(sizeof(double));
       *Dnulval=*Fnull; 

       sprintf(subinfo, 
       "Any undefined pixel values (if applicable) are set to %.2e",*Dnulval);
       strcpy(nulHistory, subinfo);

       nelem = BytesAtaTime/sizeof(double);

       Darrptr = malloc(BytesAtaTime);
       if(Darrptr == NULL){ 
	 *status=1;
	 sprintf(subinfo, ".... Error: malloc for double array failed \n"); 
	 StatusChk(*status,subinfo);
       }

       chimgtypNewFile(&infp2, outfile, 1, DOUBLE_IMG, NofAxis, naxes, 
		       0L, 1L, 1, status);

       Remain = TotalElements;
       felem = 1;

       for(;;){

         nelem = minvalue(Remain,nelem);
	 for(i=0;i<nelem; i++) {Darrptr[i] = 0;}

	 if(ffgpv(infp1, TDOUBLE, felem, nelem, Dnulval, Darrptr, 
		  &anynul, status)) Printerror(*status);

	 if(ffppnd(infp2, 1L, felem, nelem, Darrptr, *Dnulval, status))
	   Printerror(*status); 

         if(Remain <= nelem) break;
         Remain = Remain - nelem ;
         felem = felem + nelem ;
       }

       free(Darrptr);
       free(Dnulval);

       break;
  }

  /*-----------------------------------------------------------------
    now write all other keywords e.g., HISTORY, DATE, etc. etc.
    & delete old DATE and CREATOR keywords.
  ------------------------------------------------------------------*/
  for(i=4; i<=nkeys; i++){
    ffgrec(infp1, i, card, status);

    if (strncmp(card,"BLANK   ",8) !=0 ) {  /* don't copy BLANK keyword */
        if (fits_get_keyclass(card) > TYP_CMPRS_KEY)
        {
            /* write the record to the output file */
            ffprec(infp2, card, status); 
        }
    }
  }

  if((!strcmp(datatype, "FLOAT")) || (!strcmp(datatype, "DOUBLE"))){
    if((!strcmp(OldDataType, "SHORT")) || (!strcmp(OldDataType, "LONG"))){
      ffdkey(infp2, "BSCALE", status);
      *status = 0; ffcmsg();
      ffdkey(infp2, "BZERO", status);
      *status = 0; ffcmsg();
      ffdkey(infp2, "BLANK", status);
      *status = 0; ffcmsg();
    }
  }



  if((!strcmp(datatype, "USHORT")) || (!strcmp(datatype, "SHORT")) ||
     (!strcmp(datatype, "ULONG"))  || (!strcmp(datatype, "LONG"))){
    if((!strcmp(OldDataType,"FLOAT")) || (!strcmp(OldDataType,"DOUBLE"))){ 
      if(! anynul) {
	ffdkey(infp2, "BLANK", status);
	*status = 0; ffcmsg();
      }
    }
  }


  if(ffdkey(infp2, "DATE", status))   { *status=0; ffcmsg(); }
  if(ffdkey(infp2, "CREATOR", status)){ *status=0; ffcmsg(); }
  
  if(ffpkys(infp2,"CREATOR","chimgtyp 1.5"," s/w task that wrote this dataset",
	    status)) Printerror(*status);

  if(ffphis(infp2, " ", status)) Printerror(*status);

  sprintf(history, "---------------------------------------------------------");
  if(ffphis(infp2, history, status)) Printerror(*status);
  
  sprintf(history,"input file \"%s\"",infile);
  if(ffphis(infp2, history, status)) Printerror(*status);

  sprintf(history, "Initially input image format was  \"%s\"",OldDataType);
  if(ffphis(infp2, history, status)) Printerror(*status);
  
  sprintf(history, "Requested output image format is  \"%s\"",datatype);
  if(ffphis(infp2, history, status)) Printerror(*status);

  if(ffphis(infp2, nulHistory, status)) Printerror(*status);

  sprintf(history, "---------------------------------------------------------");
  if(ffphis(infp2, history, status)) Printerror(*status);

  if(ffphis(infp2, " ", status)) Printerror(*status);

  if(ffpdat(infp2, status)) Printerror(*status);

/*----------------------------------------------------------------
           now copy all other extensions from input to output
--------------------------------------------------------------------*/

  if(copyall){
    for(;;){
      if(ffmrhd(infp1, 1, &hdutyp, status))
	{*status=0; ffcmsg(); copyall=0; break;}

      if(ffcrhd(infp2, status)) Printerror(*status);
      if(ffcopy(infp1, infp2,0, status)) Printerror(*status);
    }
  }

  if(ffclos(infp1, status)) Printerror(*status); 
  if(ffclos(infp2, status)) Printerror(*status); 

  return;
}

/***********************************************************************
Function:
     chimgtypNewFile

Description:
   
     This routine initialises the output file. 
     Step1: opens and initilises the new file for writing
     Step2: calls FFPHPR to write mandatory keywords for Primary
            calls FFPDAT for date stamp
     Step3: returns to the called function

     NOTE: file is kept opened after this. 

Functions called:

    FITS functions:
      ffinit -- opens and initialises the new FITS file
      ffphpr -- writes the user defined mandatory keywords for Primary Array
      ffpdat -- writes the date stamp
 
    general utility functions:
      Printerror -- prints FITSIO error messages 

Author and modification history:
      Sandhia Bansal & Banashree M Seifert (July, 1997)
      Peter D Wilson (June, 1998) Moved from callib to chimgtyp and modified
                                  to be compatible with new file-handling
                                  procedures
***********************************************************************/
void chimgtypNewFile(fitsfile **infp1, char *filename, int simple,
		     int bitpix, int naxis, long *naxes, long pcount,
		     long gcount, int extend, int *status)
{  
   /* initialise output file */

   *status = 0;
   if( ffinit(infp1, filename, status) ) Printerror(*status);
   
/*-----------------------------------------------------------
           Write Mandatory Primary Array Keywords
-------------------------------------------------------------*/

   if( ffphpr(*infp1, simple, bitpix, naxis, naxes, pcount,
	      gcount, extend, status) ) Printerror(*status);

   if( ffpdat(*infp1, status) ) Printerror(*status);

} 

/* This code is needed by IRAF */
 
#ifdef vms
#define F77CALL chimgtyp
#endif
#ifdef unix
#define F77CALL chimgtyp_
#endif
 
void F77CALL()
{
 void chimgtyp();
 
 chimgtyp();
}
