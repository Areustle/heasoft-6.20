/************************************************************************
Ftools task:     chkrmf

DESCRIPTION:
    The  task  performs the necessary checks, e.g, validity of mandatory
    and optional keywords, on an input FITS file (RMF)  to  determine
    whether  it is in the correct format to be acceptable as an input to
    XSPEC (and hence several other  ftools  tasks),  reporting  back  to
    STDOUT  (and/or  an  ASCII  file)  the  results.   To  get report to
    STDOUT, user needs to use chatter >=10 .   This  task  handles  only
    RMF extension.  For RMF data file, it is mandatory that it has both
    response matrix and ebounds extensions.  This task checks for both
    and then go fo checking if both present.  Otherwise (if onle one of 
    them present) it errors out

AUTHOR & MODIFICATION HISTORY:          

        Banashree M Seifert (April 1997) 1.0.0
        Peter D Wilson      (July  1998) 1.0.1
              . Fixed clobber file handling
        Ning Gan            (May   2000) 1.0.2
              . Replaced the calls of buggy ffgkyl with ffgky.
        kaa                 (Nov   2005) 1.0.3
                Fixed infinite loop if can't find one of the extensions

ASSOCIATED ROUTINES:
        chkrmf   : main function
        chkrmfgp: function to read the parameter file
        chkrmfdo: function to do the checking of the keywords

PRIMARY LOCAL VARIABLES:
        fitsfile *infp - input FITS file (RMF)

**********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "fitsio2.h"     /* cfitsio defined constants */
#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "cftools.h"     /* standard C library constants */
#include "chkrmf.h"     /* task specific definitions */


void chkrmf()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    int clobber=0, chatter=0, status=0;
    char subinfo[100];

    c_ptaskn("chkrmf 1.0.3"); /* method to set name of task used by c_fcerr */

/* get parameters from chkrmf.par */
   chkrmfgp(infile, outfile, &chatter, &clobber, &status); 

    if(status)
    {
    strcpy(subinfo,"..... Error returning from CHKRMFGP");
    c_fcecho(subinfo);
       goto Exit_error;
    }

/* do the real checking */
   chkrmfdo(infile, outfile, chatter, clobber, &status);

    if(status)
    {
       strcpy(subinfo,"..... Error returning from CHKRMFDO");
       c_fcecho(subinfo); goto Exit_error;
    }

    strcpy(subinfo,"****** successfully exited ******\n");
    c_fcecho(subinfo);
    return;

Exit_error:
    strcpy(subinfo,"****** unsuccessfully exited ******\n");
    c_fcecho(subinfo);
    return;


}

/*******************************************************************
function:
      chkrmfgp

description:
      gets the parameters for the task

author:
      Banashree M Seifert (April, 1997)

modification history:
      Peter D Wilson (July 1998): Change clobber data type to boolean

usage:
      chkrmfgp(char *infile, char *outfile, int *chatter, int *clobber, 
               int *status)

*********************************************************************/
void chkrmfgp(char *infile, char *outfile, int *chatter, int *clobber, 
              int *status)

{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    char subinfo[100];


    *status = 0;
    Uclgst("infile", infile, status);
    if(infile[0] =='\0')
    { strcpy(subinfo,"..... Error: filename is required\n");
      c_fcecho(subinfo);
      *status = 1;
      return;
    }
    if(*status)
    { strcpy(subinfo,"..... Error reading infile from .par file \n");
      c_fcecho(subinfo);
      return;
    }

    Uclgst("outfile", outfile, status);
    if(*status)
    { strcpy(subinfo,"..... Error reading outfile from .par file \n");
      c_fcecho(subinfo);
      return;
    }

    Uclgsi("chatter", chatter, status);
    if(*status)
    { strcpy(subinfo,"..... Error reading chatter from .par file \n");
      c_fcecho(subinfo);
      return;
    }

    Uclgsb("clobber", clobber, status);
    if(*status)
    { strcpy(subinfo,"..... Error reading clobber from .par file \n");
      c_fcecho(subinfo);
      return;
    }

    if(outfile[0] == '!') { *clobber = 1; strcpy(outfile,(outfile+1)); }

    return;
}
    

/*******************************************************************
function:
      chkrmfdo

description:
      Checks the RMF file for input to XSPEC

author:
      Banashree M Seifert (April, 1997)

modification history:
      Peter D Wilson (July 1998): Add clobber test for output file's existance

usage:
    void chkrmfdo(char *infile, char *outfile, int chatter, int clobber,
                  int *status);

*********************************************************************/

void chkrmfdo(char *infile, char *outfile, int chatter, int clobber,
              int *status)

{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    fitsfile *infp1=NULL;
    char comment[FLEN_BUFFER];
    char subinfo[100], keyinfo1[100], keyinfo2[100], keyinfo3[100];
    char dummyval[20], extname[20];
    FILE *fp;
    int extno=1, ok, nok, ok_op, nok_op;
    int ebounds=0, response=0, hdutype=0, pstatus=0;
    int detchan=0, tlmin4=0, tlmax4=0;
    int extver;

/********** open input file ***************/

    *status = 0;
    ffopen(&infp1, infile, READONLY, status);
    if(*status)
    { strcpy(subinfo,"..... Error opening input RMF file \n");
      c_fcecho(subinfo);
      return;
    }

/********** open output ascii file ***************/
                                              
    if( !clobber ) { /* Make sure file does *not* exist */
       if( (fp=fopen(outfile,"r")) != NULL ) {
          fclose(fp);
          c_fcerr("..... Error: output log file exists, but clobber not set\n");
          *status=1;
          return;
       }
    }

    if( (fp=fopen(outfile,"w")) != NULL )
       fprintf(fp,"Log file containing information on: %s\n",infile);
    else {
       printf("..... Error: unable to open output log file\n");
       *status=1;
       return;
    }
  
/*------------------------------------------------------------------------
 check if both MATRIX & EBOUNDS extensions are presennt. 
 if both,  then check the keywords
 if only one of them, then error out
-------------------------------------------------------------------------*/

    hdutype = BINARY_TBL;
    strcpy(extname, "MATRIX");
    extver = 0;
    response = 1;
    fits_movnam_hdu(infp1, hdutype, extname, extver, status);
    if ( *status == BAD_HDU_NUM ) response = 0;
    *status = 0;

    if ( !response ) 
    {
      strcpy(extname, "SPECRESP MATRIX");
      extver = 0;
      response = 1;
      fits_movnam_hdu(infp1, hdutype, extname, extver, status);
      if ( *status == BAD_HDU_NUM ) response = 0;
      *status = 0;
    }

    strcpy(extname, "EBOUNDS");
    ebounds = 1;
    fits_movnam_hdu(infp1, hdutype, extname, extver, status);
    if ( *status == BAD_HDU_NUM ) ebounds = 0;
    *status = 0;

    if((response) && (ebounds))
     {
      strcpy(subinfo,"Both RESPONSE MATRIX & EBOUNDS found");
      c_fcecho(subinfo); 
      *status=0; 
      goto WORK;
     }
    else if((response) && (!ebounds))
     {
      strcpy(keyinfo1,"Only RESPONSE extension found.");
      strcpy(keyinfo2,"Both MATRIX and EBOUNDS extension required.");
      c_fcecho(keyinfo1); 
      c_fcecho(keyinfo2); 
      *status=1; 
      goto CLOSE;} 
    else if((ebounds) && (!response))
     {
      strcpy(keyinfo1,"Only EBOUNDS extension found.");
      strcpy(keyinfo2,"Both MATRIX and EBOUNDS extension required.");
      c_fcecho(keyinfo1); 
      c_fcecho(keyinfo2); 
      *status=1; 
      goto CLOSE;
     }
    else
     {
      strcpy(keyinfo1,"Neither MATRIX or EBOUNDS extensions found.");
      strcpy(keyinfo2,"Both MATRIX and EBOUNDS extension required.");
      c_fcecho(keyinfo1); 
      c_fcecho(keyinfo2); 
      *status=1; 
      goto CLOSE;
     }

/*-----------------------------------------------------------------
 move to required extension.  First it will move to the first extension
 and then next and so on 
------------------------------------------------------------------*/
WORK:
    extno = 1;

NEXT_EXTENSION:

    extno++; 
    ffmahd(infp1, extno, &hdutype, status);
    if(*status == 107) {*status=0; goto CLOSE;}
    
    ok = 0; nok = 0; ok_op = 0; nok_op = 0; 
  
/*----------------------------------------------------------------
                        EXTNAME
-----------------------------------------------------------------*/

strcpy(keyinfo1,"EXTNAME keyword is the name of extension for RMF file\n");
strcpy(keyinfo2,"EXTNAME keyword need to be MATRIX or SPECRESP MATRIX for\
                   RMF files\n\n");
    *status=0;
    ffgkys(infp1, "EXTNAME", dummyval, comment, status);
    if(*status)
    {
     nok++ ;
     strcpy(subinfo,"..... EXTNAME keyword is not present\n");
     c_fcecho(subinfo); 
     goto CLOSE ;
    }
    else
     ok++ ;

if((strcmp(dummyval,"MATRIX"))==0 || (strcmp(dummyval,"SPECRESP MATRIX"))==0) 
{ 
  response = 1; 
  ebounds = 0;
} 
else if(strcmp(dummyval,"EBOUNDS") == 0) 
{ 
  ebounds = 1; 
  response = 0;
}

/* if neither MATRIX or EBOUNDS extension then go onto the next one */

if ( (!response) && (!ebounds) ) goto NEXT_EXTENSION;

/*---------------------------------------------------------------
 write some heading for output file and check for mandatory keywords
-----------------------------------------------------------------*/

fprintf(fp, "\nExtension = %s\n\n",dummyval);
if(chatter >= 10) printf("Extension = %s\n\n",dummyval); 

strcpy(subinfo,"               *** Mandatory keywords ***\n");
fprintf(fp,"%s\n",subinfo);
if(chatter >= 10) printf("%s\n",subinfo); 

/*--------------------------------------------------------------
                         TELESCOP                                      
---------------------------------------------------------------*/
strcpy(keyinfo1,"TELESCOP keyword is the mission/satellite name\n\n");

   *status = 0;
    ffgkys(infp1, "TELESCOP", dummyval, comment, status);
    if(*status)
    {
     nok++ ;
     strcpy(subinfo,"..... TELESCOP keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     if(chatter >= 10) {c_fcecho(subinfo); c_fcecho(keyinfo1);} 
    }
    else 
     ok++;  
       
/*--------------------------------------------------------------
                         INSTRUME                                      
---------------------------------------------------------------*/
strcpy(keyinfo1,"INSTRUME keyword is the instrument/detector name\n\n");

   *status = 0;
    ffgkys(infp1, "INSTRUME", dummyval, comment, status);
    if(*status)
    {
     nok++ ;
     strcpy(subinfo,"..... INSTRUME keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     if(chatter >= 10){ c_fcecho(subinfo); c_fcecho(keyinfo1);}
    }
    else
     ok++;

/*--------------------------------------------------------------
                         FILTER                                        
---------------------------------------------------------------*/
strcpy(keyinfo1,"FILTER keyword is the instrument filter in use (if any)\n\n");

   *status = 0;
    ffgkys(infp1, "FILTER", dummyval, comment, status);
    if(*status)
    {
     nok++ ;
     strcpy(subinfo,"..... FILTER keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     if(chatter >= 10){ c_fcecho(subinfo); c_fcecho(keyinfo1);}
    }
    else
     ok++;
       
/*--------------------------------------------------------------
                         RMFVERSN                                      
---------------------------------------------------------------*/
strcpy(keyinfo1,"RMFVERSN keyword is the OGIP version no. of the FITS format in use\n\n");

   *status = 0;
    ffgkys(infp1, "RMFVERSN", dummyval, comment, status);
    if(*status)
    {
     nok++ ;
     strcpy(subinfo,"..... RMFVERSN keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     if(chatter >= 10){ c_fcecho(subinfo); c_fcecho(keyinfo1);}
    }
    else
     ok++;

/*--------------------------------------------------------------
                         DETCHANS                                      
---------------------------------------------------------------*/
strcpy(keyinfo1,"DETCHANS keyword indicates the total no. of detector\n"); 
strcpy(keyinfo2,"channels available\n\n");

   *status = 0;
    ffgky(infp1, TINT, "DETCHANS", &detchan, comment, status);
    if(*status)
    {
     nok++ ;
     strcpy(subinfo,"..... DETCHANS keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     fprintf(fp,keyinfo2);
     if(chatter >= 10){c_fcecho(subinfo);c_fcecho(keyinfo1);c_fcecho(keyinfo2);}
    }
    else
     ok++;

if(response == 1)
{
/*-------------------------------------------------------------------
 check for TLMIN4 and TLMAX4 if it tallies with DETCHAN
---------------------------------------------------------------------*/
strcpy(keyinfo1,"TLMIN4 keyword indicates the lower channel no. used\n");

   *status = 0;
    ffgky(infp1, TINT, "TLMIN4", &tlmin4, comment, status);
    if(*status)
    {
     nok++ ;
     strcpy(subinfo,"\n..... TLMIN4 keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     if(chatter >= 10){c_fcecho(subinfo);c_fcecho(keyinfo1);}
    }
    else
     ok++;

strcpy(keyinfo1,"TLMAX4 keyword indicates the upper channel no. used\n");

   *status = 0;
    ffgky(infp1, TINT, "TLMAX4", &tlmax4, comment, status);
    if(*status)
    {
     strcpy(subinfo,"\n..... TLMAX4 keyword is not present (not mandatory)");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     if(chatter >= 10){c_fcecho(subinfo);c_fcecho(keyinfo1);}
    }
    else
    {
       if(detchan == tlmax4 - tlmin4 +1)
       {
          strcpy(subinfo,"\n..... DETCHANS and TLMIN4 & TLMAX4 matches\n");
          fprintf(fp,"%s\n",subinfo);
          if(chatter >= 10){c_fcecho(subinfo);}
       }
       else
       { 
          strcpy(subinfo,"ERROR: DETCHANS and TLMIN4 & TLMAX4 NOT matching\n");
          fprintf(fp,"%s\n",subinfo);
          if(chatter >= 10){c_fcecho(subinfo);}
       } 
    }
 
}
/*---------------------------------------------------------------
 check if all mandatory keywords are ok or see how many are
 ok and not ok
----------------------------------------------------------------*/

sprintf(keyinfo1,"\nMandatory keyword checked are:\n");
if(response == 1) sprintf(keyinfo2,"EXTNAME, TELESCOP, INSTRUME, FILTER, RMFVERSN, DETCHANS & TLMIN4\n\n"); 
if(ebounds ==1) sprintf(keyinfo2,"EXTNAME, TELESCOP, INSTRUME, FILTER, RMFVERSN, DETCHANS\n\n");

fprintf(fp,keyinfo1);
fprintf(fp,keyinfo2);
c_fcecho(keyinfo1);
c_fcecho(keyinfo2);

  if(nok == 0)
  {
    fprintf(fp,"All mandatory keywords are ok\n");
    if(chatter >=10)printf("All mandatory keywords are ok\n");
  }
  else
  {
    fprintf(fp,"No of mandatory keywords present = %d\n",ok);
    fprintf(fp,"No of mandatory keywords missing = %d\n",nok);  
    if(chatter >=10)
    {    
       printf("No of mandatory keywords present = %d\n",ok);
       printf("No of mandatory keywords missing = %d\n",nok);
    }
  }

/*-------------------------------------------------------------
                      OPTIONAL KEYWORDS
 Optional keywords are suggested, as they supply further detailed
 informations regarding data sets
--------------------------------------------------------------*/

strcpy(subinfo,"\n               *** Optional keywords ***\n");
fprintf(fp,subinfo);
if(chatter >= 10){c_fcecho(subinfo); }

strcpy(subinfo,"------------------------------------------------------\n");
strcpy(keyinfo1,"Optional keywords are suggested, as they supply further\n");
strcpy(keyinfo2,"detailed informations regarding data sets\n");      
strcpy(keyinfo3,"------------------------------------------------------\n\n\n");

fprintf(fp,subinfo);
fprintf(fp,keyinfo1);
fprintf(fp,keyinfo2);
fprintf(fp,keyinfo3);
    
/*--------------------------------------------------------------
                         PHAFILE                                      
---------------------------------------------------------------*/
strcpy(keyinfo1,"PHAFILE keyword indicates the name of the PHA/PI file\n");
strcpy(keyinfo2,"for which this matrix has been customised\n\n");

   *status = 0;
   ffgkys(infp1, "PHAFILE", dummyval, comment, status);
    if(*status)
    {
     nok_op++ ;
     strcpy(subinfo,"..... PHAFILE keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     fprintf(fp,keyinfo2);
     if(chatter >=10){c_fcecho(subinfo);c_fcecho(keyinfo1);c_fcecho(keyinfo2);}
    }
    else
     ok_op++;
 
/*--------------------------------------------------------------
                         HDUCLASS                                     
---------------------------------------------------------------*/
strcpy(keyinfo1,"HDUCLASS keyword should contain the string OGIP to indicate\n");
strcpy(keyinfo2,"that this is an OGIP style file\n\n");
   *status = 0;
    ffgkys(infp1, "HDUCLASS", dummyval, comment, status);
    if(*status)
    {
     nok_op++ ;
     strcpy(subinfo,"..... HDUCLASS keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     fprintf(fp,keyinfo2);
     if(chatter >=10){c_fcecho(subinfo);c_fcecho(keyinfo1);c_fcecho(keyinfo2);}
    }
    else
     ok_op++;

/*--------------------------------------------------------------
                         HDUCLAS1                                     
---------------------------------------------------------------*/
strcpy(keyinfo1,"HDUCLAS1 = RESPONSE indicating extension contains data\n");
strcpy(keyinfo2,"relating to the response of the instrument\n\n");

   *status = 0;
    ffgkys(infp1, "HDUCLAS1", dummyval, comment, status);
    if(*status)
    {
     nok_op++ ;
     strcpy(subinfo,"..... HDUCLAS1 keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     fprintf(fp,keyinfo2);
     if(chatter >=10){c_fcecho(subinfo);c_fcecho(keyinfo1);c_fcecho(keyinfo2);}
    }
    else
     ok_op++;

    if((strncmp(dummyval,"RESPONSE",8)) != 0 ) {
        sprintf(keyinfo1,"HDUCLAS1 keyword '%s' is non-OGIP standard.",dummyval);
        strcpy(keyinfo2,"HDUCLAS1 keyword must be 'RESPONSE'.\n");
        c_fcecho(keyinfo1);
        c_fcecho(keyinfo2);
        *status=1;
        goto CLOSE;
    }

/*--------------------------------------------------------------
                         HDUCLAS2                                     
---------------------------------------------------------------*/
strcpy(keyinfo1,"HDUCLAS2 = RSP_MATRIX indicating the type of data stored\n\n");
strcpy(keyinfo2,"HDUCLAS2 = EBOUNDS indicating the type of data stored\n\n");

   *status = 0;
    ffgkys(infp1, "HDUCLAS2", dummyval, comment, status);
    if(*status)
    { 
     nok_op++ ;
     strcpy(subinfo,"..... HDUCLAS2 keyword is not present");
     fprintf(fp,"%s\n",subinfo);
       if(response)
       { fprintf(fp,keyinfo1); 
         if(chatter >= 10){c_fcecho(subinfo);c_fcecho(keyinfo1); }
       }
       if(ebounds)
       { fprintf(fp,keyinfo2); 
         if(chatter >= 10){c_fcecho(subinfo);c_fcecho(keyinfo2); } 
       }
    }
    else
     ok_op++;

    if((strncmp(dummyval,"RSP_MATRIX",10)) != 0 && (strncmp(dummyval,"EBOUNDS",7)) != 0 ) {
        sprintf(keyinfo1,"HDUCLAS2 keyword '%s' is non-OGIP standard.",dummyval);
        strcpy(keyinfo2,"HDUCLAS2 keyword should be either 'RSP_MATRIX' or 'EBOUNDS'.\n");
        c_fcecho(keyinfo1);
        c_fcecho(keyinfo2);
        *status=1;
        goto CLOSE;
    }
  
/*--------------------------------------------------------------
                         HDUCLAS3                                     
---------------------------------------------------------------*/
strcpy(keyinfo1,"HDUCLAS3 gives further details of the stored matrix\n");
strcpy(keyinfo2,"Allowed values are: REDLIST, DETECTOR, FULL\n\n");

   *status = 0;
    ffgkys(infp1, "HDUCLAS3", dummyval, comment, status);
    if(*status)
    {
     nok_op++ ;
     strcpy(subinfo,"..... HDUCLAS3 keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     fprintf(fp,keyinfo2);
     if(chatter >=10){c_fcecho(subinfo);c_fcecho(keyinfo1);c_fcecho(keyinfo2);}
    }
    else
     ok_op++;

/*--------------------------------------------------------------
                         HDUVERS1                                     
---------------------------------------------------------------*/
strcpy(keyinfo1,"HDUVERS1 indicates the version of the HDUCLAS1 format in use\n");
strcpy(keyinfo2,"HDUVERS1 = 1.0.0/1.1.0\n\n");

    *status = 0;
    ffgkys(infp1, "HDUVERS1", dummyval, comment, status);
    if(*status)
    {
     nok_op++ ;
     strcpy(subinfo,"..... HDUVERS1 keyword is not present");
     fprintf(fp,"%s\n",subinfo);
     fprintf(fp,keyinfo1);
     fprintf(fp,keyinfo2);
     if(chatter >=10){c_fcecho(subinfo);c_fcecho(keyinfo1);c_fcecho(keyinfo2);}
     *status = 0;
    }
    else
     ok_op++;
   
/*---------------------------------------------------------------
 check if all optional keywords are ok or see how many are
 ok and not ok
----------------------------------------------------------------*/
sprintf(keyinfo1,"Optional keyword checked are:\n");
sprintf(keyinfo2,"PHAFILE, HDUCLASS, HDUCLAS1, HDUCLAS2, HDUCLAS3 & HDUVERS1\n\n");
fprintf(fp,keyinfo1);
fprintf(fp,keyinfo2);
c_fcecho(keyinfo1);
c_fcecho(keyinfo2);

  if(nok_op == 0)
  {
    fprintf(fp,"All optional keywords are ok\n");
    if(chatter >=10)printf("All optional keywords are ok\n");
  }
  else
  {
    fprintf(fp,"No of optional keywords present = %d\n",ok_op);
    fprintf(fp,"No of optional keywords missing = %d\n",nok_op);  
    if(chatter >=10)
    {
      printf("No of optional keywords present = %d\n",ok_op);
      printf("No of optional keywords missing = %d\n",nok_op);
    }
  }

goto NEXT_EXTENSION;


CLOSE:

    fclose(fp);
    pstatus=0;
    ffclos(infp1, &pstatus);
 
    return;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL chkrm
#endif
#ifdef unix
#define F77CALL chkrm_
#endif

void F77CALL()
{
 void chkrmf();

 chkrmf();
}

