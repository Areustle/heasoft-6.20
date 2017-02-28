/*******************************************************************
Description:

  correct RDD of a science fits file
  using zero level fits made with avPixLvl

  Ftoolise the code by T.kotani

Author:
     Banashree Mitra Seifert (April, 1997)

Modifications:
   Banashree M Seifert (Aug, 1997)
      The following changes are made in function "correctrdd_do"
      . reading first the INSTRUME keyword as instrument to determine
        sensor (either 0 or 1)
      . goto primary extension to read S[01]CCDPOW
      . then goto 2nd extension to read others
      . writing the version keyword 'RDDCOR_V' as integer version of 1
   Peter D Wilson (March 30, 1998)... v1.0a
      The following changes are made in function "correctrdd_do"
      . Increased array sizes of rdd_is_corrected and datamode to the
        max length of keyword values... although only 6 or 4 characters
        long, trailing spaces were causing array overflows
      . sscanf for finding instrument number changed... single quotes
        not part of returned string values.
      . Add error checking for bad ccdid in the events list
   Jeff Guerber (Apr 30 1998) v1.0b
      . Bug fix: In FAINT mode, wasn't writing all the PHAS elements
   Peter D Wilson (June 26, 1998) v1.0c
      . Update for new file-handling abilities of CFITSIO
      . Fix problem with clobber parameter... was defined as an integer
   Jeff Guerber (Aug. 31, 1998) v1.0d
      . Check that RDD files are for the right sensor and chip
   Jeff Guerber (1998-10-19)  v1.0e
      . RDDCOR_V was written as logical not integer, changed to ffpkyj
**************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "fitsio2.h"        /* cfitsio defined constants */
#include "xpi.h"           /* parameter file reading functions*/
#include "ftoolstruct.h"   /* C-Fortran common blocks */
#include "cftools.h"       /* standard C library constants */
#include "cfitsio.h"
#include "correctrdd.h"    /* task specific definitions */

/*********************************************************/

void correctrdd()
{
   static char version[] = "Ver 1.0e";
   char infile[FLEN_BUFFER], outfile[FLEN_BUFFER];

   int clobber=0, status=0;

   char subinfo[100];
   float prob[8][9];

/* ------------------- get parameters -----------------------*/

   status = 0;
   correctrdd_par(infile, outfile, &clobber, &status);
   strcpy(subinfo,"..... Error returning from CORRECTRDD_PAR");
   StatusChk(status, subinfo);

/* ------------------- get probability values -----------------------*/

   correctrdd_prob(prob);

/* ------------------- do the RDD correction ----------------*/

   correctrdd_do(infile, outfile, version, prob, &clobber, &status);
   strcpy(subinfo,"..... Error returning from CORRECTRDD_DO");
   StatusChk(status, subinfo);

   strcpy(subinfo,"****** successfully exited ******\n");
   DispMsg(0,0,subinfo);

   return;

}

/*******************************************************************
function:
      correctrdd_par

description:
      gets the parameters for the task

author:
      Banashree M Seifert (April, 1997)

modification history:
      Peter D Wilson (June 26, 1998)
          . Change handling of clobber and outfile
          . Fix problem with clobber parameter... was defined as an integer

usage:
   void correctrdd_par(char *infile, char *outfile, int *clobber int *status);

*********************************************************************/
void correctrdd_par(char *infile, char *outfile, int *clobber, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    char subinfo[100];

    *status = 0;
    Uclgst("infile", infile, status);
    if(infile[0] =='\0') {
       strcpy(subinfo,"..... Error: filename is required\n");
       *status = 1;
       StatusChk(*status, subinfo);
    }
    if(*status) {
       strcpy(subinfo,"..... Error reading infile from .par file \n");
       StatusChk(*status, subinfo);
    }

    Uclgst("outfile", outfile, status);
    if(*status){
       strcpy(subinfo,"..... Error reading outfile from .par file \n");
       StatusChk(*status, subinfo);
    }

    /* PDW 6/26/98: Change definition from integer to boolean */
    Uclgsb("clobber", clobber, status);
    if(*status){
       strcpy(subinfo,"..... Error reading clobber from .par file \n");
       StatusChk(*status, subinfo);
    }

    /* PDW 6/26/98: Add "bang" to outfile if clobber set...FFINIT understands */
    if( *clobber && outfile[0] != '!') {
       char tmp[FLEN_BUFFER];
       strcpy(tmp,outfile);
       outfile[0]='!';
       strcpy( outfile+1, tmp);
    }

    return;
}

/*******************************************************************
function:
      correctrdd_prob

description:
      calculates the probability for i-th pixel to be summed into PHA
      when GRADE=g

author:
      Banashree M Seifert (April 1997)

modification history:


usage:
    void correctrdd_prob(double *prob[][9]);

*********************************************************************/
void correctrdd_prob(float prob[][9])
{
   int g, i;
   for(g=0; g<8; g++)
   {
      prob[g][0]=1.;
      for (i=1; i<9; i++)
          prob[g][i]=0.;
   }

   prob[2][2] = prob[2][7] = .5;
   prob[3][4] = 1.;
   prob[4][5] = 1.;
   prob[5][2] = prob[5][4] = prob[5][5] = prob[5][7] = .25;
   prob[6][1] = prob[6][3] = prob[6][6] = prob[6][8] = .25;
   prob[6][2] = prob[6][4] = prob[6][5] = prob[6][7] = .5;
   prob[7][2] = prob[7][4] = prob[7][5] = prob[7][7] = .5;

   return;
}
/*******************************************************************
function:
      correctrdd_do

description:
      Does the correction to RDD data

author:
      Banashree M Seifert (April, 1997)

modification history:
   Banashree M Seifert (Aug, 1997)
      The following changes are made in function "correctrdd_do"
      . reading first the INSTRUME keyword as instrument to determine
        sensor (either 0 or 1)
      . goto primary extension to read S[01]CCDPOW
      . then goto 2nd extension to read others
      . writing the version keyword 'RDDCOR_V' as integer version of 1
   Peter D Wilson (March 30, 1998)
      . Increased array sizes of rdd_is_corrected and datamode to the
        max length of keyword values... although only 6 or 4 characters
        long, trailing spaces were causing array overflows
      . sscanf for finding instrument number changed... single quotes
        not part of returned string values.
      . Add error checking for bad ccdid in the events list
   Jeff Guerber (Apr. 30, 1998)
      . Bug fix: FAINT mode, write 9*naxis[1], not naxis[1], PHAS elements.
   Peter D Wilson (June 26, 1998)
      . Drop call to CheckFile... let FFINIT delete the file
   Jeff Guerber (Aug. 31, 1998)
      . Check that RDD files are for the right sensor and chip
   Jeff Guerber (1998-10-19)
      . RDDCOR_V was written as logical not long, changed to ffpkyj

usage:
   correctrdd_do(char *infile, char *outfile, char *version, float prob[][9],
                 int *clobber,int *status)

*********************************************************************/
void correctrdd_do(char *infile, char *outfile, char *version,
		   float prob[][9],
                   int *clobber,int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    fitsfile *infp1=NULL;
    fitsfile *infp2=NULL;
    fitsfile *inrdd=NULL;

    char rddfile[4][FLEN_BUFFER];
    char scrdd[11];
    char comment[FLEN_COMMENT], subinfo[200];
    char instrument[FLEN_VALUE], keyword[FLEN_KEYWORD], datamode[FLEN_VALUE];
    char rdd_is_corrected[FLEN_VALUE];

    long *rdd[4];
    long naxis[2];
    long nelem=RDDX*RDDY ;
    long nullval=0;

    int sensor=0;
    int c=0, i=0, exist = 0, pstatus=0, morekeys=0;
    int hdutype=0, anynul=0;
    int rawx_colnum, rawy_colnum, ccdid_colnum, phas_colnum, pha_colnum;
    int grade_colnum, tmppha;
    int *rawxptr, *rawyptr, *ccdptr, *phaptr, *gradptr, *phas;
    int row, mode;
    int rddsensor, rddchip;
    int rawxy2rddxy();

/* -----------------------------------------------------------*/

    /* PDW 6/26/98: No longer needed... FFINIT will delete !filenames
    CheckFile(outfile, 0, *clobber);
    */

/* -------------- open infile for reading -----------------*/

   *status=0;
   if(ffopen(&infp1, infile, READONLY, status)) Printerror(*status);

/* --------------- open outfile for writing ----------------------*/

   if(ffinit(&infp2, outfile, status)) Printerror(*status);

/* ---------------------------------------------------------------------
   copy first extension of input to output
   move to 2nd extension of input (if exists), and
   create 2nd extension of the output
   copy 2nd extension of input to output
-----------------------------------------------------------------------*/

   morekeys = 4;
   if(ffcopy(infp1, infp2, morekeys, status)) Printerror(*status);

   if(ffpkyj(infp2, "RDDCOR_V", 1, "Version of RDDcorrect algorithm used",
	     status)) Printerror(*status);

   if(ffmahd(infp1, 2, &hdutype, status)) Printerror(*status);
   if(ffcrhd(infp2, status)) Printerror(*status);

   if(ffcopy(infp1, infp2, morekeys, status)) Printerror(*status);

/* ----------------- read SENSOR ---------------------------*/

   if(ffgkys(infp1, "INSTRUME", instrument, comment, status))
      Printerror(*status);

   sscanf(instrument, "SIS%d    ", &sensor);

/* ----------- read S[01]CCDPOW from primary extension ---------------*/

   if(ffmahd(infp1, 1, &hdutype, status)) Printerror(*status);
   sprintf(keyword, "S%dCCDPOW", sensor);
   if(ffgkys(infp1, keyword, rdd_is_corrected, comment, status))
      Printerror(*status);

/* ----------------- read DATAMODE -------------------------------*/
   if(ffmahd(infp1, 2, &hdutype, status)) Printerror(*status);
   if(ffgkys(infp1, "DATAMODE", datamode, comment, status))
      Printerror(*status);

/* ----------- Now, read the data from rdd* files -------------*/

   for(c=0; c<4; c++)
   {
      if (rdd_is_corrected[c] == '1')
      {
          sprintf(scrdd, "S%dC%drdd",sensor,c);
          Uclgst(scrdd, rddfile[c], status);
          if(*status){
             sprintf(subinfo,"..... Error reading %s from .par file\n",scrdd);
             StatusChk(*status, subinfo);
          }

          if(ffopen(&inrdd, rddfile[c], READONLY, status)) Printerror(*status);

          if(ffgky(inrdd, TLONG, "NAXIS1", &naxis[0], comment, status))
	     Printerror(*status);

          if(ffgky(inrdd, TLONG, "NAXIS2", &naxis[1], comment, status))
	     Printerror(*status);

          if (ffgky(inrdd, TINT, "SENSOR", &rddsensor, comment, status) != 0)
             Printerror(*status);
          if (rddsensor != sensor) {
             sprintf(subinfo, " Error: %s file %s is for sensor %d\n",
                     scrdd, rddfile[c], rddsensor);
             *status = 1;
             StatusChk(*status, subinfo);
          }

          if (ffgky(inrdd, TINT, "CHIP", &rddchip, comment, status) != 0)
             Printerror(*status);
          if (rddchip != c) {
             sprintf(subinfo, " Error: %s file %s is for chip %d\n",
                     scrdd, rddfile[c], rddchip);
             *status = 1;
             StatusChk(*status, subinfo);
          }

          rdd[c] = calloc(naxis[0]*naxis[1], sizeof(long));
          if(ffgpvj(inrdd, 0L, 1L, nelem, nullval, rdd[c], &anynul, status))
	     Printerror(*status);
          if(anynul != 0){
             sprintf(subinfo," Warning: %d pixels have bad values\n", anynul);
             DispMsg(0,0,subinfo);
          }
          if(ffclos(inrdd, status)) Printerror(*status);
      } else {
          rdd[c]=NULL;
      }
    }

   if(strcmp(datamode, "FAINT") == 0) {
      mode = 0;
      strcpy(datamode, "FNT");
      goto FAINT;
   }
   else if(strcmp(datamode, "BRIGHT") == 0) {
      mode = 1;
      strcpy(datamode, "BRT");
      goto BRIGHT;
   }
   else if(strcmp(datamode, "BRIGHT2") == 0) {
      mode = 2;
      strcpy(datamode, "BRT2");
      goto BRIGHT;
   }
   else {
     sprintf(subinfo, ".... Error: unknown datamode: %s\n", datamode);
     *status=1;
     StatusChk(*status, subinfo);
   }

FAINT:

   nullval = 0;
   if(ffgky(infp1, TLONG, "NAXIS1", &naxis[0], comment, status))
      Printerror(*status);

   if(ffgky(infp1, TLONG, "NAXIS2", &naxis[1], comment, status))
      Printerror(*status);

/*---- get RAWX, RAWY, CCDID, PHAS array from input file --------*/

   if(ffgcno(infp1, 0, "RAWX", &rawx_colnum, status)) Printerror(*status);

   if(ffgcno(infp1, 0, "RAWY", &rawy_colnum, status)) Printerror(*status);

   if(ffgcno(infp1, 0, "CCDID", &ccdid_colnum, status)) Printerror(*status);

   if(ffgcno(infp1, 0, "PHAS", &phas_colnum, status)) Printerror(*status);

   rawxptr = (int*)calloc(naxis[1], sizeof(int));
   rawyptr = (int*)calloc(naxis[1], sizeof(int));
   ccdptr  = (int*)calloc(naxis[1], sizeof(int));
   phaptr  = (int*)calloc(naxis[1]*9, sizeof(int));

   if(ffgcvk(infp1, rawx_colnum, 1L, 1L, naxis[1], nullval, rawxptr, &anynul,
	     status)) Printerror(*status);

   if(ffgcvk(infp1, rawy_colnum, 1L, 1L, naxis[1], nullval, rawyptr, &anynul,
	     status)) Printerror(*status);

   if(ffgcvk(infp1, ccdid_colnum, 1L, 1L, naxis[1], nullval, ccdptr, &anynul,
	     status)) Printerror(*status);

   if(ffgcvk(infp1, phas_colnum, 1L, 1L, 9*naxis[1], nullval, phaptr, &anynul,
	     status)) Printerror(*status);

/* ---------------- subtract RDD ------------------------*/

   for(row = 1; row <= naxis[1]; row++) {
       /*  Make sure pointer has been allocated  */
       if( rdd[ccdptr[row-1]]==NULL ) {
	  sprintf(subinfo,"Bad event list... CCD %d isn't supposed to be on",
		  ccdptr[row-1]);
	  ffpmsg(subinfo);
	  Printerror(1);
       }
       for(i=0; i<9; i++) {
         phaptr[(row-1)*9+i] -=
           rdd[ccdptr[row-1]][rawxy2rddxy(rawxptr[row-1], rawyptr[row-1], i)];
       }
   }

   if(ffpclk(infp2, phas_colnum, 1L,1L, 9*naxis[1], phaptr, status))
      Printerror(*status);

   free(rawxptr);
   free(rawyptr);
   free(phaptr);
   free(ccdptr);
   goto MORE;

BRIGHT:

   nullval = 0;
   if(ffgky(infp1, TLONG, "NAXIS1", &naxis[0], comment, status))
      Printerror(*status);

   if(ffgky(infp1, TLONG, "NAXIS2", &naxis[1], comment, status))
      Printerror(*status);

/*---------------------------------------------------------------------
     get RAWX, RAWY, CCDID, PHAS array from input file
------------------------------------------------------------------*/
   if(ffgcno(infp1, 0, "RAWX", &rawx_colnum, status)) Printerror(*status);

   if(ffgcno(infp1, 0, "RAWY", &rawy_colnum, status)) Printerror(*status);

   if(ffgcno(infp1, 0, "CCDID", &ccdid_colnum, status)) Printerror(*status);

   if(ffgcno(infp1, 0, "PHA", &pha_colnum, status)) Printerror(*status);

   if(ffgcno(infp1, 0, "GRADE", &grade_colnum, status)) Printerror(*status);

   rawxptr = (int*)calloc(naxis[1], sizeof(int));
   rawyptr = (int*)calloc(naxis[1], sizeof(int));
   ccdptr  = (int*)calloc(naxis[1], sizeof(int));
   phaptr  = (int*)calloc(naxis[1], sizeof(int));
   gradptr = (int*)calloc(naxis[1], sizeof(int));

   if(ffgcvk(infp1, rawx_colnum, 1L, 1L, naxis[1], nullval, rawxptr, &anynul,
	     status)) Printerror(*status);

   if(ffgcvk(infp1, rawy_colnum, 1L, 1L, naxis[1], nullval, rawyptr, &anynul,
	     status)) Printerror(*status);

   if(ffgcvk(infp1, ccdid_colnum, 1L, 1L, naxis[1], nullval, ccdptr, &anynul,
	     status)) Printerror(*status);

   if(ffgcvk(infp1, pha_colnum, 1L, 1L, naxis[1], nullval, phaptr, &anynul,
	     status)) Printerror(*status);

   if(ffgcvk(infp1, grade_colnum, 1L, 1L, naxis[1], nullval, gradptr, &anynul,
	     status)) Printerror(*status);

/* ---------------- subtract RDD ----------------------*/

   for (row = 1; row <= naxis[1]; row++) {
      expand(&phaptr[row-1], mode);
      /*  Make sure pointer has been allocated  */
      if( rdd[ccdptr[row-1]]==NULL ) {
	  sprintf(subinfo,"Bad event list... CCD %d isn't supposed to be on",
		  ccdptr[row-1]);
	  ffpmsg(subinfo);
	  Printerror(1);
      }
      for(i=0; i<9; i++) {
        phaptr[row-1] -=
          rdd[ccdptr[row-1]][rawxy2rddxy(rawxptr[row-1], rawyptr[row-1], i)]
                             * prob[gradptr[row-1]][i];
      }
      reduce(&phaptr[row-1], mode);
   }

   if(ffpclk(infp2, pha_colnum, 1L,1L, naxis[1], phaptr, status))
      Printerror(*status);

   free(rawxptr);
   free(rawyptr);
   free(phaptr);
   free(ccdptr);
   free(gradptr);
   goto MORE;

MORE:

/* -----------------------------------------------------------------
       put some history info onto output, copy the 3rd extension
       close input & output files
---------------------------------------------------------------------*/

   sprintf(subinfo, "RDD is corrected with CORRECTRDD: %s",version);
   if(ffphis(infp2, subinfo, status)) Printerror(*status);

   for (c = 0; c< 4; c++) {
     if(rdd_is_corrected[c] == '1') {
       sprintf(subinfo, "rddfile for s%dc%d : %s",sensor, c, rddfile[c]);
       if(ffphis(infp2, subinfo, status)) Printerror(*status);
     }
   }

/* ----------- to copy 3rd extension of input to output ----------
       move to 3rd extension of input file,
       make 3rd extension for output file and
       copy 3rd extension of input to output
---------------------------------------------------------------------*/

   if(ffmahd(infp1, 3, &hdutype, status)) Printerror(*status);
   if(ffcrhd(infp2, status)) Printerror(*status);
   if(ffcopy(infp1, infp2, 0, status)) Printerror(*status);

   if(ffclos(infp1, status)) Printerror(*status);
   if(ffclos(infp2, status)) Printerror(*status);

   return;

EXIT_ERROR:

   *status=0;
   if(ffclos(infp1, status)) Printerror(*status);
   if(ffclos(infp2, status)) Printerror(*status);

   return;
}

/*-------------------------------------------------------------------
          function EXPAND
--------------------------------------------------------------------*/
void expand(int *foo, int mode)
{
  if (mode == 2) return;
  else if(*foo < 1024) return;
  else if(*foo < 1536) *foo = 1024+(*foo-1024)*2;
  else  *foo = 2048+(*foo-1536)*4;

  return;
}

/*-------------------------------------------------------------------
          function REDUCE
--------------------------------------------------------------------*/
void reduce(int *foo, int mode)
{
  if (mode == 2) return;
  else if (*foo < 1024) return;
  else if (*foo < 2048) *foo = 1024+(*foo-1024)/2;
  else *foo = 1536+(*foo-2048)/4;

  return;
}

/*-------------------------------------------------------------------
          function RAWXY2RDDXY
--------------------------------------------------------------------*/
int rawxy2rddxy(int rawx, int rawy, int i)
{
  int rddx, rddy, offsetx=0, offsety=0;

  switch(i)
  {
  case 0:
    offsetx = 0; offsety = 0; break;
  case 1:
    offsetx =-1; offsety =-1; break;
  case 2:
    offsetx = 0; offsety =-1; break;
  case 3:
    offsetx = 1; offsety =-1; break;
  case 4:
    offsetx =-1; offsety = 0; break;
  case 5:
    offsetx = 1; offsety = 0; break;
  case 6:
    offsetx =-1; offsety = 1; break;
  case 7:
    offsetx = 0; offsety = 1; break;
  case 8:
    offsetx = 1; offsety = 1;
  }

  rddx = rawx + offsetx;
  rddy = rawy + offsety;

  return(rddy*RDDX+rddx);
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL correctrd
#endif
#ifdef unix
#define F77CALL correctrd_
#endif

void F77CALL()
{
 void correctrdd();

 correctrdd();
}
