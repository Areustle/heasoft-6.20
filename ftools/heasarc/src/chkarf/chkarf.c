/************************************************************************
Ftools task:     chkarf

DESCRIPTION:
    The  task  performs the necessary checks, e.g, validity of mandatory
    and optional keywords, on an input FITS file (ARF)  to  determine
    whether  it is in the correct format to be acceptable as an input to
    XSPEC (and hence several other  ftools  tasks),  reporting  back  to
    STDOUT  (and/or  an  ASCII  file)  the  results.   To  get report to
    STDOUT, user needs to use chatter >=10 .   This  task  handles  only
    ARF extension.  

AUTHOR & MODIFICATION HISTORY:          

        Banashree M Seifert (April 1997) 1.0.0
        Peter D Wilson      (July  1998) 1.0.1
              . Fix clobber handling

ASSOCIATED ROUTINES:
        chkarf  : main function
        chkarfgp: function to read the parameter file
        chkarfdo: function to do the checking of the keywords

PRIMARY LOCAL VARIABLES:
        fitsfile *infp - input FITS file (ARF)

**********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "fitsio2.h"     /* cfitsio defined constants */
#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "cftools.h"     /* standard C library constants */
#include "chkarf.h"     /* task specific definitions */


void chkarf()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    int clobber=0, chatter=0, status=0;
    char subinfo[100];

    c_ptaskn("chkarf"); /* method to set name of task used by c_fcerr */

/* get parameters from chkarf.par */
   chkarfgp(infile, outfile, &chatter, &clobber, &status); 

    if(status)
    {
    strcpy(subinfo,"..... Error returning from CHKARFGP");
    c_fcecho(subinfo);
       goto Exit_error;
    }

/* do the real checking */
    chkarfdo(infile, outfile, chatter, clobber, &status);

    if(status)
    {
       strcpy(subinfo,"..... Error returning from CHKARFDO");
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
      chkarfgp

description:
      gets the parameters for the task

author:
      Banashree M Seifert (April, 1997)

modification history:
      Peter D Wilson (July 1998): Change clobber data type to boolean

usage:
      chkarfgp(char *infile, char *outfile, int *chatter, int *clobber, 
               int *status)

*********************************************************************/
void chkarfgp(char *infile, char *outfile, int *chatter, int *clobber, 
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
      chkarfdo

description:
      Checks the ARF file for input to XSPEC

author:
      Banashree M Seifert (April, 1997)

modification history:
      Peter D Wilson (July 1998): Added clobber test for output log file's
                                  existance.

usage:
    void chkarfdo(char *infile, char *outfile, int chatter, int clobber,
                  int *status);

*********************************************************************/

void chkarfdo(char *infile, char *outfile, int chatter, int clobber,
              int *status)

{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    fitsfile *infp1=NULL;
    char comment[FLEN_BUFFER];
    char subinfo[100], keyinfo1[100], keyinfo2[100], keyinfo3[100];
    char dummyval[20];
    char mandatory[8][10] = {"EXTNAME", "TELESCOP", "INSTRUME", "FILTER",
			     "HDUCLASS", "HDUCLAS1", "HDUCLAS2", "HDUVERS"};
    char optional[1][10] = {"PHAFILE"};
    char obsolete[3][10] = {"ARFVERSN", "HDUVERS1", "HDUVERS2"};
    int Nmandatory=8, Noptional=1, Nobsolete=3;
    FILE *fp;
    int extno=1, ok=0, nok=0, ok_op=0, nok_op=0, ok_obs=0, nok_obs=0;
    int hdutype=0, pstatus=0, i;

/********** open input file ***************/

    *status = 0;
    ffopen(&infp1, infile, READONLY, status);
    if(*status)
    { strcpy(subinfo,"..... Error opening input ARF file \n");
      c_fcecho(subinfo);
      return;
    }

/*--------------------------------------------------------------
               open output ascii file and heading  
---------------------------------------------------------------*/
                                              
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
       c_fcerr("..... Error: unable to open output log file\n");
       *status=1;
       return;
    }
  
    strcpy(subinfo,"               *** Mandatory keywords ***\n");
    fprintf(fp,"%s\n",subinfo);
    if(chatter >= 10) printf("%s\n",subinfo); 

/*-----------------------------------------------------------------
  move to the desired extension
------------------------------------------------------------------*/
    extno++; 
    ffmahd(infp1, extno, &hdutype, status);
    if(*status)
    {
     strcpy(subinfo,"Error moving to response extension\n"); 
     c_fcecho(subinfo);
     goto CLOSE;
    }

    printf("\n");

/*----------------------------------------------------------------
               Loop round checking mandatory keywords
-----------------------------------------------------------------*/

    for ( i=0; i<Nmandatory; i++) {
      *status=0;
      ffgkys(infp1, mandatory[i], dummyval, comment, status);
      if(*status) {
	nok++ ;
	fprintf(fp, "..... %s keyword is not present\n", mandatory[i]);
	if(chatter >= 5) {
	  printf("..... %s keyword is not present\n", mandatory[i]);
	  c_fcecho(keyinfo1);
	  c_fcecho(keyinfo2);
	}
      }
      else {
	ok++;  
      }
    }

/*---------------------------------------------------------------
 check if all mandatory keywords are ok or see how many are
 ok and not ok
----------------------------------------------------------------*/

sprintf(keyinfo1,"Mandatory keywords checked are:\n");
sprintf(keyinfo2,"%s, %s, %s, %s, %s, %s, %s, %s\n\n", mandatory[0], mandatory[1], 
	mandatory[2], mandatory[3], mandatory[4], mandatory[5], mandatory[6], 
	mandatory[7]);
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
    fprintf(fp,"See http://heasarc.gsfc.nasa.gov/docs/heasarc/caldb/docs/memos/cal_gen_92_002/cal_gen_92_002.html#tth_sEc4.1.1\n");
    if(chatter >=5)
    {    
       printf("No of mandatory keywords present = %d\n",ok);
       printf("No of mandatory keywords missing = %d\n",nok);
       printf("See http://heasarc.gsfc.nasa.gov/docs/heasarc/caldb/docs/memos/cal_gen_92_002/cal_gen_92_002.html#tth_sEc4.1.1\n\n");
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


/*----------------------------------------------------------------
               Loop round checking optional keywords
-----------------------------------------------------------------*/

    for ( i=0; i<Noptional; i++) {
      *status=0;
      ffgkys(infp1, optional[i], dummyval, comment, status);
      if(*status) {
	nok_op++ ;
	fprintf(fp, "..... %s keyword is not present\n", optional[i]);
	if(chatter >= 10) {
	  printf("..... %s keyword is not present\n", optional[i]);
	  c_fcecho(keyinfo1);
	  c_fcecho(keyinfo2);
	  goto CLOSE ;
	}
      }
      else {
	ok_op++;  
      }
    }

/*---------------------------------------------------------------
 check if all optional keywords are ok or see how many are
 ok and not ok
----------------------------------------------------------------*/
sprintf(keyinfo1,"Optional keyword checked are:\n");
sprintf(keyinfo2,"%s\n\n", optional[0]);
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


/*-------------------------------------------------------------
                      OBSOLETE KEYWORDS
 Obsolete keywords may be required by older software
--------------------------------------------------------------*/

strcpy(subinfo,"\n               *** Obsolete keywords ***\n");
fprintf(fp,subinfo);
if(chatter >= 10){c_fcecho(subinfo); }

strcpy(subinfo,"------------------------------------------------------\n");
strcpy(keyinfo1,"Obsolete keywords may be required by older software\n");
strcpy(keyinfo2,"------------------------------------------------------\n\n\n");

fprintf(fp,subinfo);
fprintf(fp,keyinfo1);
fprintf(fp,keyinfo2);


/*----------------------------------------------------------------
               Loop round checking obsolete keywords
-----------------------------------------------------------------*/

    for ( i=0; i<Nobsolete; i++) {
      *status=0;
      ffgkys(infp1, obsolete[i], dummyval, comment, status);
      if(*status) {
	nok_obs++ ;
	fprintf(fp, "..... %s keyword is not present\n", obsolete[i]);
	if(chatter >= 10) {
	  printf("..... %s keyword is not present\n", obsolete[i]);
	  c_fcecho(keyinfo1);
	  c_fcecho(keyinfo2);
	  goto CLOSE ;
	}
      }
      else {
	ok_obs++;  
      }
    }

/*---------------------------------------------------------------
 check if all obsolete keywords are ok or see how many are
 ok and not ok
----------------------------------------------------------------*/
sprintf(keyinfo1,"Obsolete keyword checked are:\n");
sprintf(keyinfo2,"%s %s %s\n\n", obsolete[0], obsolete[1], obsolete[2]);
fprintf(fp,keyinfo1);
fprintf(fp,keyinfo2);
c_fcecho(keyinfo1);
c_fcecho(keyinfo2);

  if(nok_obs == 0)
  {
    fprintf(fp,"All obsolete keywords are ok\n");
    if(chatter >=10)printf("All obsolete keywords are ok\n");
  }
  else
  {
    fprintf(fp,"No of obsolete keywords present = %d\n",ok_obs);
    fprintf(fp,"No of obsolete keywords missing = %d\n",nok_obs);  
    if(chatter >=10)
    {
      printf("No of obsolete keywords present = %d\n",ok_obs);
      printf("No of obsolete keywords missing = %d\n",nok_obs);
    }
  }

  *status=0;

CLOSE:

    fclose(fp);
    pstatus=0;
    ffclos(infp1, &pstatus);
 
    return;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL chkar 
#endif
#ifdef unix
#define F77CALL  chkar_
#endif

void F77CALL()
{
 void chkarf();

 chkarf();
}

