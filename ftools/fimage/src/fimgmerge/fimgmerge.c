/***********************************************************************
File : fimgmerge.c

Description:
       Merges two or more images together on to a primary image file.

AUTHOR:
       Banashree M Seifert (March19, 1997)

Modifcations:
       Bryan K Irby (June 26, 2014)
             Fix strcpy of overlapping strings which aborts on Macs.
       Bryan K Irby (May 08, 2014)
             Raised arbitrary maximum number of input files to 999.
       Ziqin Pan (Jan 2005) Deal the case when image has null or nans.  
       Bryan K Irby (Aug 23, 2001)
             Fixed bug in fimgmergedo: fits_get_img_size was
             reading the wrong input file for nimg1 & nimg2.
       Peter D Wilson (June 26, 1998)
             Dropped call to CheckInFile... Not compatible with new
             extended syntax filenames
       Ning Gan  (June 1998) replaced the cfitsio.h with fitsio.h.
       Peter D Wilson (Aug 4, 1999)
             bug fix of problem causing only the first xoffs/yoffs
             entry to be used for each image
       Ning Gan  (Dec 1999) Updated for reading the compressed images.

***********************************************************************/
#define MAXFILE 999
#define ABS(A) (A<0? -A:A)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>

#include "fitsio.h"
#include "fitsio2.h"     /* cfitsio defined constants */
#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "ftoolstruct.h" /* C-Fortran common blocks */
#include "cftools.h"     /* standard C library constants */
#include "fimgmerge.h"      /* task specific definitions */

void fimgmerge()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    char imgfiles[MAXFILE][FLEN_BUFFER];
    char subinfo[100];
    int nfiles, i, status=0;
    int xoffs[MAXFILE], yoffs[MAXFILE];
    int clobber;

/************ get parameters from fimgmerge.par ************/

    fimgmergegp(infile, outfile, imgfiles, &nfiles, xoffs, yoffs, &clobber,
                &status);

    if(status){
      strcpy(subinfo,"..... Error returning from FIMGMERGEGP");
      DispMsg(0,0,subinfo);
      goto Exit_error;
    }
    if(nfiles > MAXFILE){
      sprintf(subinfo,"\n Error:");
      DispMsg(0,0,subinfo);
      sprintf(subinfo,  ".... no. of input files = %d",nfiles);
      DispMsg(0,0,subinfo);
      sprintf(subinfo,  ".... Maximum files supported is %d\n",MAXFILE);
      DispMsg(0,0,subinfo);
      goto Exit_error;
    }

    
/************ now do the actual merging of the image files **********/

    fimgmergedo(infile, outfile, imgfiles, &nfiles, xoffs, yoffs, &clobber,
                &status);

    if(status) {
      strcpy(subinfo,"..... Error returning from FIMGMERGEDO \n");
      DispMsg(0,0,subinfo);
      goto Exit_error;
    }
 
    strcpy(subinfo,"\n****** successfully exited ******\n");
    DispMsg(0,0,subinfo);
    return;

Exit_error:
    strcpy(subinfo,"\n****** unsuccessfully exited ******\n");
    DispMsg(0,0,subinfo);
    return;
}

/*******************************************************************
function:
      fimgmergegp

description:
      gets the parameters for the task

author:
      Banashree M Seifert (March, 19, 1997)

modification history:
      Peter D Wilson (June 26, 1998)
            Drop call to CheckInFile

usage:
      fimgmergegp(char *infile, char *outfile, char imgfiles[][FLEN_BUFFER], 
                  int *nfiles, int *xoffs, int *yoffs, int *clobber, 
                  int *status)

*********************************************************************/
void fimgmergegp(char *infile, char *outfile, char imgfiles[][FLEN_BUFFER], 
                 int *nfiles, int *xoffs, int *yoffs, int *clobber, 
                 int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    char list[FLEN_BUFFER];
    char listtmp[FLEN_BUFFER];
    char xoffset[FLEN_BUFFER];
    char yoffset[FLEN_BUFFER];
    char *listptr = list;
    char *token;
    char subinfo[100];
    int i;

    *status = 0;
    Uclgst("infile", infile, status);

    if(infile[0] =='\0'){
       *status=1;
       sprintf(subinfo, "\n.... Error: filename is required \n");
       StatusChk(*status, subinfo);
    }
    sprintf(subinfo,  "\n.... Error reading infile from .par file \n");
    StatusChk(*status, subinfo);
   
    /* PDW 6/26/98: Leave this for FFOPEN to determine
    CheckInFile(infile);
    */

    Uclgst("list", list, status);
    strcpy(subinfo,"..... Error reading list from .par file \n");
    StatusChk(*status, subinfo);


    Uclgst("outfile", outfile, status);
    strcpy(subinfo,"..... Error reading outfile from .par file \n");
    StatusChk(*status, subinfo);

    Uclgsb("clobber", clobber, status);
    strcpy(subinfo,"..... Error reading clobber from .par file \n");
    StatusChk(*status, subinfo);

    if(outfile[0] == '!') *clobber = 1;

/******************************************************
if imgfile has '@' as first character, then
1. read list file to load the names of img files, their x and y offsets
if not,
1. Parse the string and get imgfile names and number of files(fcgcls)
2. Get the x- and y- offsets string(uclgst)
3. Parse the string and get the offsets and number of offsets(fcgcls)
******************************************************/

    if(list[0] == '@'){
      strcpy(listtmp, (list+1));
      strcpy(list, listtmp);
      rdlstfil(list, imgfiles, nfiles, xoffs, yoffs, status);
      strcpy(subinfo,"..... Error returning from RDLSTFIL \n");
      StatusChk(*status, subinfo);
    }

    else { 
      for(i=0;; i++){
	token = strtok(listptr, ",\t ");
	if(token == NULL)
	  break;
	else{
	  if(i > MAXFILE-1){
            sprintf(subinfo,"\n Error:");
            DispMsg(0,0,subinfo);
            sprintf(subinfo,  ".... no. of input files exceeds maximum limit");
            DispMsg(0,0,subinfo);
            sprintf(subinfo,  ".... Maximum files supported is %d\n",MAXFILE);
            DispMsg(0,0,subinfo);
            *status=1;
            return;
	  }
          strcpy(imgfiles[i],token);
          listptr =NULL;
	}
      }

      *nfiles = i;

      Uclgst("xoffset", xoffset, status);
      strcpy(subinfo,"..... Error reading xoffset from .par file \n");
      StatusChk(*status, subinfo);
	
      listptr = xoffset;
      for(i=0; i<MAXFILE; i++){
	token = strtok(listptr, ",\t ");
	if(token == NULL)
	  break;
	else{
	  xoffs[i] = atoi(token);
	  listptr =NULL;
	}
      }

      if(*nfiles != i){
	sprintf(subinfo,"..... Error: \n");
        DispMsg(0,0,subinfo);
	sprintf(subinfo,"..... No. of files = %d and no. of x-offsets = %d\n",
	       *nfiles,i);
        DispMsg(0,0,subinfo);
	return;
      }

      Uclgst("yoffset", yoffset, status);
      sprintf(subinfo,"..... Error reading yoffset from .par file \n");
      StatusChk(*status, subinfo);

      listptr = yoffset;
      for(i=0; i<MAXFILE; i++) {
	token = strtok(listptr, ",\t ");
	if(token == NULL)
	  break;
	else{
	  yoffs[i] = atoi(token);
	  listptr =NULL;
	}
      }

      if(*nfiles != i){
	*status=1;
	sprintf(subinfo,"..... Error: No. of files = %d and no. of y-offsets = %d\n",
		*nfiles,i);        
	StatusChk(*status, subinfo);
      }
    }

    return;
}

/*******************************************************************
function:
      rdlstfil

description:
      reads the file list and extracts info such as:
           filename,xoffs, yoffs and nfiles

author:
      Banashree M Seifert (March, 19, 1997)

modification history:


usage:
      rdlstfil(char *list, char imgfiles[][FLEN_BUFFER], int *nfiles, 
                    int *xoffs,int *yoffs, int *status);

*********************************************************************/
void rdlstfil(char *list, char imgfiles[][FLEN_BUFFER], int *nfiles, 
              int *xoffs, int *yoffs, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    char line[FLEN_BUFFER];
    FILE *fp;
    char *listptr = line;
    char *token;
    int i, j;
    char subinfo[100];
 

    if((fp=fopen(list,"r"))==NULL){
      strcpy(subinfo,"... cannot open file \n");
      DispMsg(0,0,subinfo);
      return;
    }

    for(i=0; ((j = fscanf(fp,"%s",line)) != EOF);i++){
      
      if(i > MAXFILE-1){
	sprintf(subinfo,"\n Error:");
        DispMsg(0,0, subinfo);
	sprintf(subinfo,  ".... no. of input files exceeds maximum limit");
        DispMsg(0,0, subinfo);
	sprintf(subinfo,  ".... Maximum files supported is %d\n",MAXFILE);
        DispMsg(0,0, subinfo);
	*status=1;
	return;
      }

      listptr=line;
      token = strtok(listptr, ",\t ");
      strcpy(imgfiles[i],token);
      listptr =NULL;

      token = strtok(listptr, ",\t ");
      if(token == NULL){
	sprintf(subinfo,"..... Error:X-offset is not supplied for file \" %s \" \n",
	       imgfiles[i] ); 
        DispMsg(0,0, subinfo);
	*status=1;
	return;
      }
      xoffs[i] = atoi(token);
      listptr =NULL;

      token = strtok(listptr, ",\t ");
      if(token == NULL){
	sprintf(subinfo,"..... Error: ");
	DispMsg(0,0,subinfo);
	sprintf(subinfo,"%s\n","..... Y-offset is not supplied for file \"%s\" \n",
		imgfiles[i] ); 
	DispMsg(0,0,subinfo);
	*status=1;
	return;
      }
      yoffs[i] = atoi(token);
      listptr =NULL;
    }
    *nfiles=i;

    return;

}


/*******************************************************************
function:
      fimgmergedo

description:
      merges the image files to a mosaic

author:
      Banashree M Seifert (March, 19, 1997)

modification history:
      Banashree M Seifert (July 28, 1997)
        . bug fixed to run on ALPHAs and the image calculation is
          fixed
      Peter D Wilson (Aug 4, 1999)
        . bug fix of above modification which caused only the first
          xoffs/yoffs entry to be used for each image

usage:
      fimgmergedo(char *infile, char *outfile, char imgfiles[][FLEN_BUFFER], 
                  int *nfiles, int *xoffs, int *yoffs, int *clobber,
                  int *status)

*********************************************************************/
void fimgmergedo(char *infile, char *outfile, char imgfiles[][FLEN_BUFFER], 
                 int *nfiles, int *xoffs, int *yoffs, int *clobber, 
                 int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    fitsfile *infp1=NULL;
    fitsfile *infp2=NULL;
    fitsfile *infp3=NULL;

    char outfiletmp[FLEN_BUFFER];
    char subinfo[100], pathname[100], comment[80];

    int exist=0, same;
    int anynul;
    int i, j, n, xmin, xmax, ymin, ymax, ii, ncount;

    long naxis1, naxis2, nimg1, nimg2;
    long naxes[2];

    double nulval;

    float *arrptr, *imgptr;


/********** open input file ***************/

   *status = 0;
   if(ffopen(&infp1, infile, READONLY, status)) Printerror(*status);

/***************************************************************
if outfile is !infile, then create a temporary file
if outfile exists, then if clobber=true, then delete file and open
if outfile exists, then if clobber=false, then error out
***************************************************************/

   /* PDW 6/26/98: Set clobber if !outfile */
   if(outfile[0] == '!') {
      strcpy(outfiletmp, (outfile+1));
      strcpy(outfile, outfiletmp);
      *clobber = 1;
   }
     
   getcwd(pathname,100);
   strcat(pathname,"/");
   strcat(pathname,outfile);
   exist=access(pathname,F_OK);

   same=strcmp(infile,outfile);
   if((exist == 0) && (same==0)){
     if(*clobber == 1) strcpy(outfile,"cr.tmp");
     else{
       *status=1;
       strcpy(subinfo,".... input & output files are same, but clobber is not set\n");
       StatusChk(*status,subinfo);
     }
   }

   if((exist == 0) && (same != 0)){
     if(*clobber == 1) unlink(outfile); 
     else{
       *status=1;
       strcpy(subinfo,".... file exists, try with !filename\n");
       StatusChk(*status,subinfo);
     }
   }

/*************************************************************
Now read the files and do the job
*****************************************************************/

   if(ffinit(&infp2, outfile, status)) Printerror(*status);
   if(ffcopy(infp1, infp2, 3, status)) Printerror(*status);
   if(ffhdef(infp2, 3, status)) Printerror(*status);
   if(fits_get_img_size(infp1,2,naxes,status)) Printerror(*status);
   naxis1 = naxes[0];
   naxis2 = naxes[1];

/*************************************************************
allocate DMA to the arrays to be read
arrptr = array of the primary image
imgptr = array of the images to be merged
*************************************************************/
   arrptr = malloc(naxis1*naxis2* sizeof(float));
   if(arrptr == NULL){ 
     sprintf(subinfo, ".... Error: malloc failed \n"); 
     DispMsg(0,0,subinfo);
     return;
   }
   imgptr = malloc(10*10*sizeof(float));
   if(imgptr == NULL){ 
     *status=1;
     sprintf(subinfo, ".... Error: malloc failed \n"); 
     StatusChk(*status,subinfo);
   }
   
   nulval=0.; 
   anynul=0;
   if(ffgky(infp1,TDOUBLE,"BLANK",&nulval,NULL,status)) {
      *status =0;
      nulval =-99.;  
   }
   
   if(ffg2de(infp1, 0L, nulval, naxis1, naxis1, naxis2, arrptr, &anynul, 
             status)) Printerror(*status); 

   for(n=0; n<*nfiles; n++, xoffs++, yoffs++){
     if(ffopen(&infp3, imgfiles[n], READONLY, status)) Printerror(*status); 

     if(fits_get_img_size(infp3,2,naxes,status)) Printerror(*status);
     nimg1 = naxes[0];
     nimg2 = naxes[1];
     anynul=0;
/*     nulval=0.; */
     
/****************************************************************
reallocating the image arrays as each image might have different dimensions
****************************************************************/

     imgptr = realloc(imgptr, nimg1*nimg2 * sizeof(float));
     if(imgptr == NULL){ 
       *status = 1;
       sprintf(subinfo, ".... Error: calloc failed \n"); 
       StatusChk(*status, subinfo); 
     }
     if(ffg2de(infp3, 0L, nulval, nimg1, nimg1, nimg2, imgptr, &anynul, status))
       Printerror(*status);
     if(ffclos(infp3, status)) Printerror(*status);

/****************************************************************
After redaing each image file, the arrays are merged with primary image
array and checks are done so that x- and y- direction it doesnot get
outside primary image dimension.  That is, if the merged array gets out
of naxis1 & naxis2 of the primary array, it it just gets cut.  The output
array is what is the dimension of the primary array.
After each image file is done, they are closed and a new one is opened
****************************************************************/

     if((*xoffs) >=0 ) {
       xmin= 0;
       xmax = minvalue((naxis1-*xoffs), nimg1);
     }
     else { 
       xmin = ABS(*xoffs); 
       xmax = minvalue(naxis1, nimg1);
       *xoffs = 0;
     }
     
     if((*yoffs) >=0 ){
       ymin= 0;
       ymax = minvalue((naxis2-*yoffs), nimg2);
     }
     else{ 
       ymin = ABS(*yoffs); 
       ymax = minvalue(naxis2, nimg2);
       *yoffs = 0;
     }     

     ncount=0;
     for(j=ymin; j<ymax; j++, ncount++){   
       ii=0;
       for(i=xmin; i<xmax; i++, ii++) {  
         if ( imgptr[(j*nimg1)+i] != nulval ) {
	 arrptr[(((*yoffs+ncount) * naxis1)+ *xoffs)+ii] += imgptr[(j*nimg1)+i];
         }
       }
     }
   }
   /*** end of do loop for nfiles *****/

/*   if(ffp2de(infp2, 0L, naxis1, naxis1, naxis2, arrptr, status)) Printerror(*status);
*/   
     if(ffppne(infp2,1L,1,naxis1*naxis2,arrptr,nulval,status)) Printerror(*status);
     
/************************************************************************
Close the input primary file, output file and last image file
************************************************************************/
     free(arrptr);
     free(imgptr);

     if(ffclos(infp1, status)) Printerror(*status);
     if(ffclos(infp2, status)) Printerror(*status);

/**********************************************************************
check, if the outfile is same as infile. If so, delete the infile and 
rename the temporary file "cr.tmp" as outfile
***********************************************************************/

     if(same == 0) { 
       unlink(infile); 
       rename(outfile,infile); 
     }

     return; 
}

/* This code is needed by IRAF */
 
#ifdef vms
#define F77CALL fimgmerg
#endif
#ifdef unix
#define F77CALL fimgmerg_
#endif
 
void F77CALL()
{
 void fimgmerge();
 fimgmerge();
}

