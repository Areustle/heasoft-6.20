/*
  FILENAME:    vle2fits.c
  purpose:     Translate Vle Mode data 1, & 2 to SE FITS file
               or to file with ASCII columns describing the bits.
  author:      Ed Morgan
  date:        July 1995
  
status:      


   July:   Began work on code with much of the specifics based upon
           the prior work performed on TRANS2FITS. This code will read
	   Good Xenon Mode 1 and 2 data and create a SE fits file
	   out of the input data. This SE fits file can be processed 
	   with SEEXTRCT, or SESELECT.PL with SEBITMASK. See these 
	   FTOOLS for for more information. 

  17Nov97: MJT changes re: FITSIO v5.04 (FCPDEF,FCBDEF calls deleted) 
           ==> version 4.0b

  Apr07:   Ed Morgan changed xenon2fits.c to vle2fits.c
           ==> version 1.0
  */

#include <stdio.h>
#include <string.h>
#include <errno.h> 
#include "cfortran.h"
#include "pctype.h"
#include "cfitsio.h"
#include "ftools.h"
#include "ftoolstruct.h"
#include "xpi.h"
#include "xte.h" 
 
#define FITS_BLOCKSIZE 2880 /* Blocksize of FITS file */

#define KEYWORD_SIZE 9      /* Allocate space for all keyword holders */
#define KEYCOMMENT 73       /* Allocate space for all keyword comments */
#define HEADERCARD 81       /* Allocate space for all header cards */
#define KEYWORD_VAL 71      /* Allocate space for all keyword values */
#define ERRMSG 31           /* Allocate space for all error messages */
#define MXELEM 20           /* Allocate for number of columns for files */
#define FitsStrBufLen 73    /* Allocate string buffer length */

#define MAXC_FNAME 256      /* Define the size of the arrays for C */
/*#define BufLen_2   255      /* Define FORTRAN length character string */

#ifndef TRUE
#define TRUE  1             /* Define a logical TRUE value */
#endif

#ifndef FALSE
#define FALSE 0             /* Define a logical FALSE value */
#endif

#define DEBUGIT FALSE         /* Set Debug flag for easier debugging */
/* The following are variables defined in CFITSIO.H:
   They are repeated here for convenience.

#define FITS_CLEN_COMMENT   73
#define FITS_FLEN_COMMENT   72
#define FITS_CLEN_KEYVAL    71
#define FITS_FLEN_KEYVAL    70
#define FITS_CLEN_CARD      81
#define FITS_FLEN_CARD      80
#define FITS_CLEN_KEYNAME    9
#define FITS_FLEN_KEYNAME    8
#define FITS_CLEN_HDEFKWDS  25
#define FITS_FLEN_HDEFKWDS  24
#define FITS_CLEN_ERRMSG    31
#define FITS_FLEN_ERRMSG    30
*/

/* The following are used as sample parameters when writing
   keyword values to the FITS file 
   
   The following are FITSIO variables the ones terminatine in "x"
   will create standard XTE_SE fits files and the others will create
   a standard ASCII dump of the data in the Vle Mode data
   files.*/

static char ttypex[MXELEM][FITS_CLEN_HDEFKWDS] = {
  "TIME", "Event"
  };
 
static char tformx[MXELEM][FITS_CLEN_HDEFKWDS] = {
    "D", "16X"
  };

static char tunitx[MXELEM][FITS_CLEN_HDEFKWDS] = {
  "s"," "
   };

static char ttype[MXELEM][FITS_CLEN_HDEFKWDS] = {
  "TIME", "PCUID", "Alpha", "Veto", "Propane", "XL1", "XR1", "XL2", "XR2", "XL3", "XR3"
  };
 
static char tform[MXELEM][FITS_CLEN_HDEFKWDS] = {
    "D", "I", "I", "I", "I", "I", "I", "I", "I", "I"
  };

static char tunit[MXELEM][FITS_CLEN_HDEFKWDS] =  {
  "s", " "," "," "," "," "," "," "," "," "
  };


static char extnam[20] = "XTE_SE";

extern void XTE_Fcecho();
extern int PCU_ID();
extern int Check_Time();
extern int Reorder_Events();
extern int Read_Line();
extern void Add_Event_VLE();

void Check_and_Write_Info_Vle();
void Write_Events_to_Fits_Vle();
int Read_Param_Vle();
void Add_Event_A_Vle();
void Write_Event_Vle();

/* Subroutine VLE2FITS starts */
void Vle2fits()
  {
  char infile1[MAXC_FNAME], infile2[MAXC_FNAME],
    outfile[MAXC_FNAME], wrt_parm[10], tmp[MAXC_FNAME],
    file1[MAXC_FNAME], file2[MAXC_FNAME];
  char *cmptest1, *cmptest2;

  char ttype1[MXELEM][FITS_CLEN_HDEFKWDS],tform1[MXELEM][FITS_CLEN_HDEFKWDS],
    tunit1[MXELEM][FITS_CLEN_HDEFKWDS], extnam1[FITS_CLEN_COMMENT];
  char ttype2[MXELEM][FITS_CLEN_HDEFKWDS],tform2[MXELEM][FITS_CLEN_HDEFKWDS],
    tunit2[MXELEM][FITS_CLEN_HDEFKWDS], extnam2[FITS_CLEN_COMMENT];

  char vle1[FitsStrBufLen+1], vle2[FitsStrBufLen+1], 
    comment1[FITS_CLEN_COMMENT],
    ctemp[2], comment2[FITS_CLEN_COMMENT];
  char errtxt[FITS_CLEN_ERRMSG];

  int Nrows[2], Nfield[2], Rows[2];
  int status, olun, simple=1, bitpix=8, naxis=0, naxes[99], pcount=0, 
    gcount=1, extend=1, frow, felem, nelem, nrows, tfield, vardat, 
    iflag[19], jflag[19], colnum, dattyp, rcount, width;
  
  float equinox, value[19], day[30];

  
  int ex1, ex2, ilun1, ilun2, i, ilunt1, ilunt2,
    ext1, ext2, block, zero, one, two, xtend, fd[2], rowval[2],
    test, pstat, nfield, nfieldx;
  double time1, time2;

  unsigned int *events[2];
  int numberEvents[2];
  double rowTimes[2], timesave=0.0;
  unsigned int lostEventCounts[2];
  unsigned char triggerFlags[2];
  unsigned char spillageFlags[2];
  int iii,ii,ilen,ierror,runit=1, BufLen_2=255,val_len=21;
  int irow=0, terminate=0;
  char cval[21], update[80];
  
/* Define the common block TASK to be vle2fits */
  C2FCBSTR("vle2fits",TASK.taskname,0);  

/* Initial all variables that will be receiving information */

  status=ierror=0;
  ex1=ex2=ilun1=ilun2=olun=zero=pstat=xtend=dattyp=rcount=width=0;
  one=1;
  two=2;
  nfieldx=2, nfield=10;
  nrows=1;
  Rows[0]=Rows[1]=0;
  ctemp[0]='1';
  ctemp[1]='2';

  XTE_Fcecho(" ");
  XTE_Fcecho("Running VLE2FITS version 1.0");
  XTE_Fcecho("=============================================");

/* Read the Parameter file to get the input and output filenames. */

  status=Read_Param_Vle(infile1,infile2,outfile,wrt_parm); 
  if(status !=0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not complete Read_Param_Vle call");
    XTE_Fcecho("Terminating run");
    if(status != 5)Fcerrm(status);
    exit(1);
  }
  
/* Now that we have read the files from the parameter file, we need to 
   parse that filename and remove the extension that was input with the 
   filename. If the extension is set to a negative value, i.e., no  
   extension is found, then the default is set to the first extension+1. */

/* Notice that I have used PCINT in Fcpars */
  Fcpars(infile1, file1, &ex1, &pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not parse first input file");
    Fcerrm(pstat);
  }
  if(ex1 <= 0)ex1 = 1;

  Fcpars(infile2, file2, &ex2, &pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not parse second input file");
    Fcerrm(pstat);
  }
  if(ex2 <= 0)ex2 = 1;

  XTE_Fcecho("Reading in Good Vle data files for 1, & 2");
  XTE_Fcecho(file1);
  XTE_Fcecho(file2);
  XTE_Fcecho(" ");
  if(wrt_parm[0] == 'X')XTE_Fcecho("Creating bit-stream output of form nX");
  if(wrt_parm[0] == 'A')XTE_Fcecho("Creating ASCII translation output file.");

  XTE_Fcecho(" ");
  XTE_Fcecho("Creating output file:");
  XTE_Fcecho(outfile);
  XTE_Fcecho(" ");

/* Open assign logical unit numbers to the above files */
  FCGIOU(&ilun1,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get logical unit number for file1");
    Fcerrm(pstat);
  }

  FCGIOU(&ilun2,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get logical unit number for file2");
    Fcerrm(pstat);
  }

  FCGIOU(&olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get logical unit number for outfile");
    Fcerrm(pstat);
  }

/* Open the files to be input. */

  FCOPEN(ilun1,file1,zero,&block,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not open input file1");
    Fcerrm(pstat);
    exit(1);
  }

  FCOPEN(ilun2,file2,zero,&block,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not open input file2");
    Fcerrm(pstat);
    exit(1);
  }

/* Create an output file with the appropriate output file name. Search for
   clobber parameter in the par file and if set to yes, overwrite any
   existing file with the same name. */

  Ffinit(olun,outfile,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not create output file");
    Fcerrm(pstat);
    exit(1);
  }

  simple=1, bitpix=8,naxis=0,pcount=0,gcount=1,extend=1;
  naxes[0]=1, naxes[1]=100;

/* Initialize space for the file
  FCPDEF(olun,bitpix,naxis,naxes,pcount,gcount,&pstat); 
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could initialize output file space");
    Fcerrm(pstat);
    exit(1);
  }
17Nov97 -- MJT
This is no longer necessary and breaks w/FITSIO v5.04 */

  FCPHPR(olun,simple,bitpix,naxis,naxes,pcount,gcount,extend,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not write out basic keywords");
    Fcerrm(pstat);
    exit(1);
  }

/* Move to the appropriate extension for each of the two
   input files. This should be the first. */

  FCMAHD(ilun1,ex1+1,&xtend,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not move to proper extension in file1");
    Fcerrm(pstat);
    exit(1);
  }

  FCMAHD(ilun2,ex2+1,&xtend,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not move to proper extension in file2");
    Fcerrm(pstat);
    exit(1);
  }

/* Lets do a check to make sure that we are dealing with files of the 
   proper type */

  FCGKYS(ilun1,"DATAMODE",vle1,comment1,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATAMODE information for file 1");
    Fcerrm(pstat);
    exit(1);
  }
  FCGKYS(ilun2,"DATAMODE",vle2,comment2,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATAMODE information for file 2");
    Fcerrm(pstat);
    exit(1);
  }
  
  ilunt1=ilun1, ext1=ex1;
  ilunt2=ilun2, ext2=ex2;

  ilen=strlen(vle1);
  for(ii=0; ii < ilen; ii++) {
    if(vle1[ii] == '_')ii=ilen+1;

    if(vle1[ii] == '2'){
     ilunt2=ilun1;
     ext2=ex1;
    }
  }

  ilen=strlen(vle2);
  for(ii=0; ii < ilen; ii++) {
    if(vle2[ii] == '_')ii=ilen+1;

    if(vle2[ii] == '1'){
     ilunt1=ilun2;
     ext1=ex2;
    }
  }

  ilun1=ilunt1;
  ilun2=ilunt2;

/* Set up integer pointer to the correct IO unit number. */
  fd[0]=ilun1;
  fd[1]=ilun2;

/* We may have had to rearrange the files and the extension numbers are
   different we must move to the correct extension again. */
  if(ex1 != ext1 || ex2 != ext2) {
    ex1=ext1;
    ex2=ext2;
    
/* Move to the appropriate extension for each of the two
   input files. This should be the first. */
    
    FCMAHD(ilun1,ex1+1,&xtend,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not move to proper extension in file1");
      Fcerrm(pstat);
      exit(1);
    }

    FCMAHD(ilun2,ex2+1,&xtend,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not move to proper extension in file2");
      Fcerrm(pstat);
      exit(1);
    }
  }

/* Get the information that describes each file */
  
  FCGHBN(ilun1,MXELEM,&Nrows[0],&Nfield[0],ttype1,tform1,tunit1,extnam1,&pcount,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get information file format for file 1");
    Fcerrm(pstat);
    exit(1);
  }
  
  FCGHBN(ilun2,MXELEM,&Nrows[1],&Nfield[1],ttype2,tform2,tunit2,extnam2,&pcount,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get information file format for file 2");
    Fcerrm(pstat);
    exit(1);
  }

/* Perform checks on files to make sure that all of the columns have 
   the same names and all the units and data types are the same for all 
   input files. */

  for(ii=0; ii < Nfield[0]; ii++){

    cmptest1 = &ttype1[ii][0];
    cmptest2 = &ttype2[ii][0];
    if(strcmp(cmptest1,cmptest2)){
      XTE_Fcecho("TTYPEs do not match for File1 and File2");
      XTE_Fcecho("Check files to be sure they are correct.");
      exit(1);
    }

    cmptest1 = &tform1[ii][0];
    cmptest2 = &tform2[ii][0];
    if(strcmp(cmptest1,cmptest2)){
      XTE_Fcecho("TFORMs do not match for File1 and File2");
      XTE_Fcecho("Check files to be sure they are correct.");
      exit(1);
    }

    cmptest1 = &tunit1[ii][0];
    cmptest2 = &tunit2[ii][0];
    if(strcmp(cmptest1,cmptest2)){
      XTE_Fcecho("TUNITs do not match for File1 and File2");
      XTE_Fcecho("Check files to be sure they are correct.");
      exit(1);
    }
  }

/* Since both files MUST have exactly the same FORMAT, we can simply
   extract the information from the first file that was input and apply
   that to all files that are being processed. */

  FCBNFM(tform1[Nfield[0]-1],&dattyp,&rcount,&width,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATTYP information from file 1");
    Fcerrm(pstat);
    exit(1);
  }
  if(dattyp != 21){
    XTE_Fcecho(" ");
    XTE_Fcecho("DATTYP information is not in integer (I) format, aborting");
    exit(1);
  }
/*  if(rcount != 256){
    XTE_Fcecho(" ");
    XTE_Fcecho("Number of elements to be processed in each row != 256 as necessary.");
    XTE_Fcecho("Aborting.");
    exit(1);
  }
 */ 
    
/* Okay we are through opening the files at this point there are a
   lot of things that still have to be added before we are ready to 
   generate REAL FITS files, but at this point we can come up with
   a very crude output file to check that the algorithm created by
   Ed is working correctly. */

/* Update the CHKSUM keywords in the header just created */
  FCPCKS(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not create/update CHKSUM keyword.");
    XTE_Fcecho("Continuing with creation of file.");
    pstat = 0;
  }  

/* Create another data header */
  FCCRHD(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not create second header for output file");
    Fcerrm(pstat);
    exit(1);
  }  

  naxis=1;

/* Initialize space for the file */
  if(wrt_parm[0] == 'X'){
/*
    FCBDEF(olun,nfieldx,tformx,zero,nrows,&pstat); 
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not reserve space for output file binary extension");
      Fcerrm(pstat);
      exit(1);
    }    
17Nov97 -- MJT
This is no longer necessary and breaks w/FITSIO v5.04 */

/* Write out information that describes what the columns are */
    FCPHBN(olun,nrows,nfieldx,ttypex,tformx,tunitx,extnam,zero,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write information for output file");
      Fcerrm(pstat);
      exit(1);
    }    
  }

/* Initialize space for the file */
  if(wrt_parm[0] == 'A'){
/*
    FCBDEF(olun,nfield,tform,zero,nrows,&pstat); 
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not reserve space for output file binary extension");
      Fcerrm(pstat);
      exit(1);
    }    
17Nov97 -- MJT
This is no longer necessary and breaks w/FITSIO v5.04 */

/* Write out information that describes what the columns are */
    FCPHBN(olun,nrows,nfield,ttype,tform,tunit,extnam,zero,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write information for output file");
      Fcerrm(pstat);
      exit(1);
    }    
  }


/* Now that we actually have all of the absolutely necessary information 
   we can check for and insert additional information for ease of 
    processing. */
  
  Check_and_Write_Info_Vle(ilun1, ilun2, olun, wrt_parm);

/* Now we KNOW that formats for each of the input files and that never 
   changes so there is no need to try to extract that information from 
   the FITS files. So we will have that information hardwired into the 
   code. This may cause problems at a later point, but for now we will 
   operate within these parameters and stick to the code supplied by 
   MIT as closely as possible. */

  for (ii =0; ii < 2; ii++){
    events[ii] = (unsigned int *) malloc(16384 * sizeof(unsigned int));
    if(!(events[ii])){
      XTE_Fcecho("Out of Memory. Cannot continue! Aborting");
      exit(1);
    }
  } 

  XTE_Fcecho("Initializing values from input files ...");

  for(ii =0; ii < 2; ii++){
    if(*(Rows+ii) < Nrows[ii] - 1 ){
      numberEvents[ii] = Read_Line(fd[ii], Rows +ii, rowTimes +ii, lostEventCounts +ii, triggerFlags + ii, spillageFlags + ii, events[ii], rcount, Nfield[ii], &pstat);
      if(DEBUGIT == TRUE){
	(void) printf("Nrows is %i,%i\n",Nrows[ii]-1,*(Rows+ii));
	(void) printf("Rowtimes %f\n",*(rowTimes+ii));
	(void) printf("Number of Events is %i\n",numberEvents[ii]);
	(void) printf("lostEventCounts %u\n",*(lostEventCounts+ii));
	(void) printf("spillageFlags %i\n",*(spillageFlags+ii));
	(void) printf("TriggerFlags %i\n",*(triggerFlags+ii));
	for(i=(numberEvents[ii]-10);i < numberEvents[ii];i++) {
	  (void) printf("Event values is %i,%hx\n",i,*(events[ii]+i));
	  (void) printf("Event values is %i,%i\n",i,*(events[ii]+i));
	}
      }
    }
  }

  for(ii=0;ii < 2;ii++)Rows[ii]=0;

  XTE_Fcecho(" ");
  XTE_Fcecho("Initialization complete. Begin processing data: ");

  while (runit){
    if(irow != 0) {
      strcpy(update, "Wrote data to output file, up to row number: ");
      ilen=strlen(update);
      sprintf(cval,"%i",irow);
      val_len=strlen(cval);
      for (ii = 0; ii < val_len; ii++){
	update[ilen]=cval[ii];
	ilen++;
      }
      update[ilen]='\0';
      XTE_Fcecho(update);
    }
    XTE_Fcecho(" ");

    for (ii =0; ii < 2; ii++){
      if((*(Rows+ii) < Nrows[ii] - 1) && !pstat){
	numberEvents[ii] = Read_Line(fd[ii], Rows +ii, rowTimes + ii, lostEventCounts +ii, triggerFlags + ii, spillageFlags + ii, events[ii], rcount, Nfield[ii], &pstat);

	if(DEBUGIT == TRUE){
	  (void) printf("Nrows is %i,%i\n",Nrows[ii]-1,*(Rows+ii));
	  (void) printf("Rowtimes %f\n",*(rowTimes+ii));
	  (void) printf("Number of Events is %i\n",numberEvents[ii]);
	  (void) printf("lostEventCounts %u\n",*(lostEventCounts+ii));
	  (void) printf("spillageFlags %i\n",*(spillageFlags+ii));
	  (void) printf("TriggerFlags %i\n",*(triggerFlags+ii));
	  for(i=(numberEvents[ii]-10);i < numberEvents[ii];i++) {
	    (void) printf("Event values is %i,%hx\n",i,*(events[ii]+i));
	    (void) printf("Event values is %i,%i\n",i,*(events[ii]+i));
	  }
	}
      }
      else {
	runit=0;
	pstat=1;
      }
    }

    /*  Keep reading rows until we find 2 with the same timestamp. */

    while (( (rowTimes[1] != rowTimes[0])) && runit){
      for (ii =1; ii < 2; ii++){
	if(*(Rows+ii) < Nrows[ii]){
	  while ((rowTimes[ii] < rowTimes[0]) && !pstat){
	    Rows[ii]++;
	    if(*(Rows+ii) >= Nrows[ii] -1 ){
	      runit = 0;
	      pstat = 1;
	      break;
	    }
	    numberEvents[ii] = Read_Line(fd[ii], Rows +ii, rowTimes + ii, lostEventCounts +ii, triggerFlags + ii, spillageFlags + ii, events[ii], rcount, Nfield[ii], &pstat);
	    if(DEBUGIT == TRUE){
	      (void) printf("Nrows is %i,%i\n",Nrows[ii]-1,*(Rows+ii));
	      (void) printf("Rowtimes %f\n",*(rowTimes+ii));
	      (void) printf("Number of Events is %i\n",numberEvents[ii]);
	      (void) printf("lostEventCounts %u\n",*(lostEventCounts+ii));
	      (void) printf("spillageFlags %i\n",*(spillageFlags+ii));
	      (void) printf("TriggerFlags %i\n",*(triggerFlags+ii));
	      for(i=(numberEvents[ii]-10);i < numberEvents[ii];i++) {
		(void) printf("Event values is %i,%hx\n",i,*(events[ii]+i));
		(void) printf("Event values is %i,%i\n",i,*(events[ii]+i));
	      }
	    }
	  }

	  while (rowTimes[ii] > rowTimes[0] && !pstat){
	    Rows[0]++;
	    if(*(Rows) >= Nrows[0] - 1 ){
	      runit = 0;
	      pstat = 1;
	      break;
	    }
	    numberEvents[0] = Read_Line(fd[0], Rows, rowTimes, lostEventCounts , triggerFlags, spillageFlags, events[0], rcount, Nfield[0], &pstat);
	    if(DEBUGIT == TRUE){
	      (void) printf("Nrows is %i,%i\n",Nrows[ii]-1,*(Rows));
	      (void) printf("Rowtimes %f\n",*(rowTimes));
	      (void) printf("Number of Events is %i\n",numberEvents[ii]);
	      (void) printf("lostEventCounts %u\n",*(lostEventCounts));
	      (void) printf("spillageFlags %i\n",*(spillageFlags));
	      (void) printf("TriggerFlags %i\n",*(triggerFlags));
	      for(i=(numberEvents[ii]-10);i < numberEvents[ii];i++){
		(void) printf("Event values is %i,%hx\n",i,*(events[ii]+i));
		(void) printf("Event values is %i,%i\n",i,*(events[ii]+i));
	      }
	    }
	  }
	  if(pstat != 0)runit=0;
	}
	else runit=0;
      }
    }

    if(runit)
      Write_Events_to_Fits_Vle(olun, &irow, &timesave, rowTimes[0], events, numberEvents, &runit, wrt_parm); 

/* Since we have processed one complete ROW let's increment the row
   number being processed in both files and continue... */
    for (ii =0; ii < 2; ii++)Rows[ii]++;
  }

/* Now that we have finished processing and writing the files, we have
   to go back and update the initial header information such that it 
   contains the correct number of rows. */

  pstat=0;

/*
  FCMKYD(olun,"TSTOP",timesave,11,"Last element in the TIME column",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not update TSTOP information.");
    Fcerrm(pstat);
  }
  */

  FCMKYJ(olun,"NAXIS2",irow," Number of rows ",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not update total number of rows output in output file");
    Fcerrm(pstat);
    exit(1);
  }  

  FCRDEF(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not update structure of output in output file");
    Fcerrm(pstat);
    exit(1);
  }  

  for (ii =0; ii < 2; ii++)  
      (void) free(events[ii]);

/* Update the CHKSUM keywords in this data extension */
  FCPCKS(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not create/update CHKSUM keyword.");
    XTE_Fcecho("Continuing to close output file.");
    pstat = 0;
  }  

/* Move to the next extension in the first input file - this should be the
   First GTI extension. We are going to copy it into the created file. */
  FCMRHD(ilun1,one,&xtend,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not move to first GTI extension in first input file.");
    XTE_Fcecho("Continuing to close output file.");
    pstat = 0;
    terminate = 1;
  } else {
    FCCRHD(olun,&pstat);
    if(pstat == 0){
      FCCOPY(ilun1,olun,zero,&pstat);
      if(pstat == 0){
        FCPCKS(olun,&pstat);
	if(pstat != 0){
	  XTE_Fcecho("Could not update/create CHKSUM keyword.");
	  XTE_Fcecho("Continuing to close output file.");
	  pstat = 0;
	  terminate = 1;
	}
      } else {
	XTE_Fcecho("Could not COPY first extension from file1 to output file.");
	XTE_Fcecho("Continuing to close output file.");
	pstat = 0;
	terminate = 1;
      }
    } else {
      XTE_Fcecho("Could not create first GTI extension in output file.");
      XTE_Fcecho("Continuing to close output file.");
      pstat = 0;
      terminate = 1;
    }

/* If nothing went wrong in the above, let's try it again to copy 
   the next GTI that "may" be in the first input file.*/
    if ( terminate == 0 ) {

/* Move to the next extension in the first input file - this should be the
   second GTI extension. We are going to copy it into the created file. */
      FCMRHD(ilun1,one,&xtend,&pstat);
      if(pstat != 0){
	XTE_Fcecho(" ");
	XTE_Fcecho("No secondary GTI extension in first input file.");
	XTE_Fcecho("No secondary GTI will be present in the output file.");
	XTE_Fcecho("Continuing to close output file.");
	pstat = 0;
      } else {
	FCCRHD(olun,&pstat);
	if(pstat == 0){
	  FCCOPY(ilun1,olun,zero,&pstat);
	  if(pstat == 0){
	    FCPCKS(olun,&pstat);
	    if(pstat != 0){
	      XTE_Fcecho("Could not update/create CHKSUM keyword.");
	      XTE_Fcecho("Continuing to close output file.");
	      pstat = 0;
	    }
	  } else {
	    XTE_Fcecho("Could not COPY second extension from file1 to output file.");
	    XTE_Fcecho("Continuing to close output file.");
	    pstat = 0;
	  }
	} else {
	  XTE_Fcecho("Could not create second GTI extension in output file.");
	  XTE_Fcecho("Continuing to close output file.");
	  pstat = 0;
	}
      }
    }
  }  
  
/* Close up all of the input files that have been read and our output file*/
  FCCLOS(ilun1,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not close input file1");
    Fcerrm(pstat);
    exit(1);
  }

  FCCLOS(ilun2,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not close input file2");
    Fcerrm(pstat);
    exit(1);
  }

/* Update the CHKSUM keywords in this data extension */
  FCPCKS(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not create/update CHKSUM keyword.");
    XTE_Fcecho("Continuing to close output file.");
    pstat = 0;
  }  

  FCCLOS(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not close output file");
    Fcerrm(pstat);
    exit(1);
  }

/* Free assigned logical unit numbers*/
  FCFIOU(ilun1,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not free logical unit number for file1");
    Fcerrm(pstat);
  }

  FCFIOU(ilun2,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not free logical unit number for file2");
    Fcerrm(pstat);
  }

  FCFIOU(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not free logical unit number for output file in X format");
    Fcerrm(pstat);
  }

  FCFIOU(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not free logical unit number for ASCII output file");
    Fcerrm(pstat);
  }

  XTE_Fcecho("Finished creating SE file from Good Vle Mode data.");

  }

int Read_Param_Vle(rfname1,rfname2,ofname,wrt_parm) 
     char *rfname1, *rfname2, *ofname, *wrt_parm; 
{
  int BufLen_2 = 255;
  int MaxElem_2 = 2;
  int parstat, zero, ival2, cols=0,log=0,ilen,i,ihold;
  char text[FITS_CLEN_ERRMSG];
  char names[2][256];
  parstat = 0;
  
  Uclgst("vle1", rfname1, &parstat);
  if(parstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get FIRST input file name.");
    XTE_Fcecho("Check input file name!");
    FCGERR(parstat, text);
    exit(1);
  }

  ilen=strlen(rfname1);
  ihold=0;
  for(i=0; i < ilen; i++)if(rfname1[i] == '@')ihold=i;

  if(rfname1[ihold] == '@'){
    Fcgcls(rfname1,names,&cols,&log);
    strcpy(rfname1,&names[0][0]);
    strcpy(rfname2,&names[1][0]);
  }
  else{
    Uclgst("vle2",rfname2, &parstat);
    if(parstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get SECOND input file name.");
      XTE_Fcecho("Check input file name!");
      FCGERR(parstat, text);
      exit(1);
    }
  }

  Uclgst("outfile",ofname, &parstat);
  if(parstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get output file name.");
    XTE_Fcecho("Check output file name!");
    FCGERR(parstat, text);
    exit(1);
    }

/* Reset BufLen_2 to 9 since we have allocated 10 spaces to receive
   the string. The acceptable values are X, ASCII, BOTH */
  BufLen_2=9;
  Uclgst("wrtparm",wrt_parm, &parstat);
  wrt_parm[0]=toupper(wrt_parm[0]);

  if(parstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not wrtparm parameter.");
    XTE_Fcecho("Check wrtparm - should be either X or ASCII");
    FCGERR(parstat, text);
    exit(1);
    }

  if(wrt_parm[0] != 'X' && wrt_parm[0] != 'A'){
    XTE_Fcecho("Assign either X or ASCII to wrtparm");
    XTE_Fcecho("Cannot continue, Wrtparm contains: ");
    XTE_Fcecho(wrt_parm);
    parstat=5;
  }
  
  printf("Finished reading in parameters\n");
  return parstat;
  }
  

/* ====================================================================== */


/*******************************************************
  This routine adds a row (event) to a FITS file.
  ******************************************************/
void Add_Event_A_Vle(ounit, irow, time, alpha, veto, propane, l1, r1, l2, r2, l3, r3, pcu)
     double time; 
     int alpha, veto, propane, l1, r1, l2, r2, l3, r3;
     int ounit;
     int irow; 
     int pcu;
{

  int stat=0,staterr=0;
  int felem=1, nelem=1, col=0, fifteen=15;

  int lalpha[1], lveto[1], ll1[1], lr1[1], ll2[1], lr2[1], ll3[1], lr3[1];
  int lpcu[1], lpropane[1];

  lalpha[0]=  alpha;
  lveto[0]= veto;
  lpropane[0]= propane;
  ll1[0]= l1; 
  lr1[0]= r1; 
  ll2[0]= l2;  
  lr2[0]= r2;  
  ll3[0]= l3;  
  lr3[0]= r3; 
  lpcu[0]= pcu;

/* Since we don't know what the TSTART is until this point, we have to 
   update the information that we wrote into that KEYWORD. */

/*
  if(irow == 1)FCMKYD(ounit,"TSTART",time,fifteen,"First element in the TIME column",&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not update TSTART information.");
    Fcerrm(stat);
  } 
  */

  col++;
  FCPCLD(ounit,col,irow,felem,nelem,&time,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put TIME information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }
  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,lpcu,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put PCUID information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }

  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,lalpha,&stat);    
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put ALPHA information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }


  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,lveto,&stat);    
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put VETO information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }

  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,lpropane,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put PROPANE information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }

  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,ll1,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put L1 information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }
  col++;

  FCPCLJ(ounit,col,irow,felem,nelem,lr1,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put R1 information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }
  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,ll2,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put L2 information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }
  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,lr2,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put R2 information into outputfile");
    printf("Error was found in col %i, irow %i, lr2 %i\n",col,irow,lr2);
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }
  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,ll3,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put L3 information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }
  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,lr3,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put R3 information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }
}

/********************************************************************

********************************************************************/
void Write_Event_Vle(ounit, irow, timesave, rowTime, micro, t1, t2, wrt_parm)
     double rowTime, *timesave; 
     int ounit; 
     int irow;
     unsigned int micro; 
     unsigned short t1, t2;
     char *wrt_parm;
{
  double tt;
  int l1=0,l2=0,l3=0,r1=0,r2=0,r3=0, pcu=0, veto=0, alpha=0;
  int propane=0;
  int i,value[16];

  for(i=0; i < 16 ; i++){
    value[i]=0;
  }



/* Here we are calculating the amount of time that is to be added to the
   timestamp that is in the file. This is calculated from two parts. The 
   first is extracted from the Vle mode 1 data (t1) and the last
   is gotten from the Vle mode 2 data file (t2). This is added to 
   tt and than that is added to rowTime to calculate the total time. */
  micro += 0x0800 * (t1 & 0x03);
  micro += (t2 & 0x07ff);
  tt = (double) micro/1048576.; 
  tt += rowTime;
  *timesave=tt;

/* Due to the way that FITSIO works it is VERY HARD to pass it a bit stream
   and to have that bit stream properly inserted. It is therefore easier to 
   create an integer array composed of 0's and 1's and to pass that for each
   bit that is to be inserted into the output file. Thus in order to output
   a 16X column of data we have to construct a variable[16] array and to 
   insert that via fcpcli so that it thinks it is outputting an array of 
   integers. Also since I constructed the integer array with value[0] 
   containing the least significant bit going up to 16 which is the reverse
   of how FITSIO interprets this data stream, I will reverse it
   in Int2log*

   So the array that I am creating goes like this  with each bit defined as follows

    15    14 13 12   11   10    9   8   7   6    5 4    3      2      1  0
    ---  ----------  ---  ---  --- --- --- ---  -----  ---   -----  -------
     |        |       |    |    |   |   |   |     |     |      |       |
   M[1]{1}   PCU     R3   L3   R2  L2  R1  L1   Vx{2} Alpha Propane  Zero{2}

 */

  if ((t1 & 0x0040) == 0x0040){
    value[6] = 1;              /* L1 */
    l1 = 1;
  }
  if ((t1 & 0x0080) == 0x0080){
    value[7] = 1;              /* R1 */
    r1 = 1;
  }
  if ((t1 & 0x0100) == 0x0100){
    value[8] = 1;             /* L2 */
    l2 = 1;
  }
  if ((t1 & 0x0200) == 0x0200){
    value[9] = 1;             /* R2 */
    r2 = 1;
  }
  if ((t1 & 0x0400) == 0x0400){
    value[10] = 1;             /* L3 */
    l3 = 1;
  }
  if ((t1 & 0x0800) == 0x0800){
    value[11] = 1;             /* R3 */
    r3 = 1;
  }
  if ((t1 & 0x0004) == 0x0004){
    value[2] = 1;              /* VPR - Propane */
    propane = 1;
  }
  if ((t1 & 0x0008) == 0x0008){
    value[3] = 1;              /* Alpha - cal */
    alpha = 1;
  }
  
  veto= (t1 & 0x0030) >> 4;
  value[5] = (veto & 2 ) >> 1;
  value[4] = veto & 1;

  pcu = (t1 & 0x7000) >> 12;              /* PCUID this is a 3 bit string */
  value[12] = (pcu & 0x1);
  value[13] = (pcu & 0x2) >> 1;
  value[14] = (pcu & 0x4) >> 2;
  value[15] = 1 ; /* Set the M[1]{1} value to agree with other DDES's */

/* If we want separate ASCII columns we use Add_Event_A_Vle, if we want a 
   bit-stream column titled Event then we use Add_Event_X. */

  if(wrt_parm[0] == 'X')Add_Event_VLE(ounit, irow, tt, value);
  if(wrt_parm[0] == 'A')Add_Event_A_Vle(ounit, irow, tt, alpha, veto, propane, l1, r1, l2, r2, l3, r3, pcu);

}

/******************************************************************
  This routine take the 3 event list arrays, does some sanity checks, 
  and finds and writes out all the real events.  
  *****************************************************************/
void Write_Events_to_Fits_Vle(ounit, irow, timesave, partTime, eventLists, numberEvents, runit, wrt_parm)
     double partTime, *timesave; 
     unsigned int **eventLists; 
     int *numberEvents, ounit, *runit;
     int *irow;
     char *wrt_parm;
{
  int ii;
  int i,j, ifirst=6, isecond=2 ;
  unsigned int mtic=0;
  unsigned short delta=0;
  int id, wrong_order=0;
  int irowtemp;
  unsigned int start;

  /* First lets do some sanity checking on the event lists */
  for (ii =0; ii < 2 && *runit != 0 ; ii++)
    if (numberEvents[ii] > 16384 ){
      fprintf(stdout, "Too many events in partition at time %lf.\n", partTime);
      printf("Number of events is %i\n",numberEvents[ii]);
      
      *runit = 0;
    }

  for (ii =0; ii < 2 && *runit != 0 ; ii++)
    if (numberEvents[ii] < 16383 )
      if (eventLists[ii][numberEvents[ii] - 1] != 0x7f7f){
	/*	fprintf(stdout,"NumberEvents is %i %i\n",ii,numberEvents[ii]);
	fprintf(stdout,"Value is %x\n",eventLists[ii][numberEvents[ii] - 1]);*/
	fprintf(stdout, "Last event not End-of-Data in partition at time %lf.\n", partTime);
	*runit = 0;
      }

  for (ii =0; ii < 2 && *runit != 0 ; ii++)
    if ((eventLists[ii][0] != 0x7ff8) && (eventLists[ii][0] != 0x7ffc)){
      fprintf(stdout,"First event not interval mark in partition at time %lf.\n", partTime);
      *runit = 0;
    }

  for (ii =1; ii < 2 && *runit != 0 ; ii++)
    if (numberEvents[ii] != numberEvents[0]){
      fprintf(stdout, "Warning, partitions have different lengths %d %d\n", numberEvents[0], numberEvents[1]);
      /*      if(numberEvents[0] > numberEvents[ii])numberEvents[0]=numberEvents[ii]; */
    }

  for (i=1; i< numberEvents[0] - 1; i++){
    wrong_order = 0;
    id = PCU_ID(eventLists[0][i]);
    if (PCU_ID(eventLists[1][i]) != id)
	wrong_order=1;
    if (wrong_order == 1)
      if (Reorder_Events(eventLists, i, ifirst, isecond))
	wrong_order = 0;
    if (wrong_order == 0){

      if (eventLists[0][i] >= 0x8000){
	 irowtemp= *irow;
	 irowtemp++;
	 *irow=irowtemp;
	Write_Event_Vle(ounit, irowtemp, timesave, partTime + delta, mtic,eventLists[0][i],eventLists[1][i], wrt_parm);

      }
      else{ 
	mtic += 8192;
	if (mtic >= 0x100000){
	  mtic -= 0x100000;
	  delta++;
	}  
      }
    }

    else{
      fprintf(stdout, "Unable to switch events: \n");
      fprintf(stdout, "First Event: %x %x\n", eventLists[0][i],eventLists[1][i]);
      fprintf(stdout, "Second Event: %x %x\n", eventLists[0][i + 1],eventLists[1][i + 1]);
      fprintf(stdout, "Third Event: %x %x\n", eventLists[0][i + 2],eventLists[1][i + 2]);
      fprintf(stdout, "Fourth Event: %x %x\n", eventLists[0][i + 3],eventLists[1][i + 3]);
      fprintf(stdout, "Fifth Event: %x %x\n", eventLists[0][i + 4],eventLists[1][i + 4]);
      /*
      fprintf(stdout, "Check these files! \n");
      fprintf(stdout,"These files may have been constructed improperly! \n");
      *runit = 0; */
    }
  }
}

void Check_and_Write_Info_Vle(ilun1, ilun2, olun, wrt_parm)
     int ilun1,ilun2, olun;
     char *wrt_parm;
{
  int il[2],i,itemp,pstat=0;
  double dtemp=0.0;
  float rtemp=0.0;
  char ckeyval[FITS_CLEN_KEYVAL],comm[FITS_CLEN_COMMENT];
  char ckeyval2[FITS_CLEN_KEYVAL];
  char s1[200],s2[100],chartemp;
  int fifteen=15;
  char vle1[FitsStrBufLen+1],datamode[FitsStrBufLen+1],test;
  int ilen,ii,ihold;
  
  il[0]=ilun1, il[1]=ilun2;

  FCPKYS(olun,"ORIGIN","XTE-GOF","Created from Good Vle 1,& 2 PCA data.",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set ORIGIN keyword in output file");
    Fcerrm(pstat);
  }

  if(wrt_parm[0] == 'X'){
    FCPKYS(olun,"CREATOR","VLE2FITSX","Created with supplied C code wrapped in FTOOLS format",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not set CREATOR keyword in output file");
      Fcerrm(pstat);
    }
  }

  if(wrt_parm[0] == 'A'){
    FCPKYS(olun,"CREATOR","VLE2FITS","Created with supplied C code wrapped in FTOOLS format",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not set CREATOR keyword in output file");
      Fcerrm(pstat);
    }
  }

  FCPKYS(olun,"HDUCLASS","OGIP"," ",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set HDUCLASS keyword in output file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"HDUCLAS1","EVENT"," ",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set HDUCLAS1 keyword in output file");
    Fcerrm(pstat);
  }
  
  FCPKYS(olun,"HDUCLAS2","ALL"," ",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set HDUCLAS2 keyword in output file");
    Fcerrm(pstat);
  }

  FCPDAT(olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set DATE keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"DATAMODE",vle1,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATAMODE information for file 1");
    XTE_Fcecho("Setting DATAMODE to read GoodVle");
    strcpy(vle1,"GoodVleN_UNKNOWN");
    pstat=0;
  }

  /*  XTE_Fcecho(&vle1[0]);
  ilen=strlen(vle1);
  ihold==0;
  strcpy(datamode,"");
  */
  /* printf("Ilen is %i\n",ilen); */

  /*
  for(ii=0; ii < ilen; ii++) {
    if(vle1[ii] == '_'){
      vle1[ii-1]='\0';
      ihold=ii;
      strcat(datamode,vle1);
      ii=ilen-1;
    }
  }
  */

  /*
  XTE_Fcecho("Out of first loop, and into second"); 
  printf("The value of Ihold is %i and ilen is %i\n",ihold,ilen);
  */

  /*  for(ii=ilen-1; ii >= ihold; ii--){ */
    /*    XTE_Fcecho("In second loop"); 
    XTE_Fcecho(&vle1[ii]); */
  /* if(vle1[ii] == '_' || ii == ihold){ */
      /*      XTE_Fcecho("We got a match");
      XTE_Fcecho(datamode);       */
      /* strcat(datamode,&vle1[ii]); */
      /*      XTE_Fcecho(datamode); */
  /*      ii = ihold-1;
    }
  }
  */

  ilen=strlen(vle1);
  ihold==0;
  strcpy(datamode,"");

  for(ii=0; ii < ilen; ii++) {
    if(vle1[ii] == '_'){
      test=vle1[ii-1];
      if(isdigit(vle1[ii-1]) != 0) {
	strcpy(datamode,"GoodVle");
	/* vle1[ii-1]='\0';
	strcat(datamode,vle1); */
      }
      ihold=ii;
      ii=ilen-1;
    }
  }

  /*  printf("Ihold is %i\n",ihold);
  XTE_Fcecho("Out of first loop"); 
  XTE_Fcecho(datamode);
  */

  for(ii=ilen-1; ii >= ihold; ii--){
    /*    XTE_Fcecho("In second loop"); 
    XTE_Fcecho(&vle1[ii]); */
    if(vle1[ii] == '_' || ii == ihold){
      /*      XTE_Fcecho("We got a match");
      XTE_Fcecho(datamode);       */
      strcat(datamode,&vle1[ii]);
      /*      XTE_Fcecho(datamode); */
      ii = ihold-1;
    }
  }


  /*  XTE_Fcecho(datamode); */


  /*  XTE_Fcecho("Out of second loop"); */

  XTE_Fcecho("Setting DATAMODE keyword to:");
  XTE_Fcecho(datamode); 
  XTE_Fcecho(" ");

  FCPKYS(olun,"DATAMODE",datamode,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set DATAMODE keyword in output file");
    XTE_Fcecho("Continuing without DATAMODE value");
    pstat=0;
  }

  FCGKYS(il[0],"TIMESYS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get keyword from input file");
    Fcerrm(pstat);
    exit(1);
  }

  /* We have to add a check here. To see if the input files
     have the same time-system. */
  FCGKYS(il[1],"TIMESYS",ckeyval2,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get keyword from second file");
    Fcerrm(pstat);
    exit(1);
  }

  /* Check to see the time systems match */
  if(strcmp(ckeyval,ckeyval2) != 0 ) {
    XTE_Fcecho(" ");
    XTE_Fcecho("Time systems are NOT the same! Aborting...");
    exit(1);
  }

  if(strcmp(ckeyval,"TDB") == 0) {
    XTE_Fcecho(" ");
    XTE_Fcecho("TIMESYS is TDB, meaning the source files have");
    XTE_Fcecho("been barycenter corrected. Since this tool adds");
    XTE_Fcecho("a discrete amount of time to the original time-stamp,");
    XTE_Fcecho("there will be an inherent error introduced.");
    XTE_Fcecho(" ");
    XTE_Fcecho("The time added is accurate ONLY for original TIMESYS=TT");
    XTE_Fcecho("files.");
    XTE_Fcecho(" ");
    XTE_Fcecho("To avoid this use the original TIMESYS=TT Good Vle");
    XTE_Fcecho("data files. You have been warned!");
  }

  if(strcmp(ckeyval,"TCB") == 0) {
    XTE_Fcecho(" ");
    XTE_Fcecho("TIMESYS is TCB, meaning the source files have");
    XTE_Fcecho("been barycenter corrected. Since this tool adds");
    XTE_Fcecho("a discrete amount of time to the original time-stamp,");
    XTE_Fcecho("there will be an inherent error introduced.");
    XTE_Fcecho(" ");
    XTE_Fcecho("The time added is accurate ONLY for original TIMESYS=TT");
    XTE_Fcecho("files.");
    XTE_Fcecho(" ");
    XTE_Fcecho("To avoid this use the original TIMESYS=TT Good Vle");
    XTE_Fcecho("data files. You have been warned!");
  }
  
  if((strcmp(ckeyval,"TDB") != 0) && (strcmp(ckeyval,"TDB") != 0) &&
     (strcmp(ckeyval,"TT") != 0)) {
    XTE_Fcecho(" ");
    XTE_Fcecho("TIMESYS is neither TT, TDB, nor TCB.");
    XTE_Fcecho("Attempting to process this file - the output may");
    XTE_Fcecho("or may not be valid.");
  }

  FCPKYS(olun,"TIMESYS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIMESYS keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TIMEUNIT",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIMEUNIT keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TIMEUNIT",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIMEUNIT keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TIMEREF",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIMEREF keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TIMEREF",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIMEREF keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TASSIGN",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TASSIGN keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TASSIGN",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TASSIGN keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TIMVERSN",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIMVERSN keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TIMVERSN",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIMVERSN keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"DATE-OBS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATE-OBS keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"DATE-OBS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set DATE-OBS keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TIME-OBS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIME-OBS keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TIME-OBS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIME-OBS keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"DATE-END",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATE-END keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"DATE-END",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set DATE-END keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TIME-END",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIME-END keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TIME-END",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIME-END keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"OBSERVER",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get OBSERVER keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"OBSERVER",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set OBSERVER keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"INSTRUME",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get INSTRUME keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"INSTRUME",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set INSTRUME keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TELESCOP",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TELESCOP keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TELESCOP",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TELESCOP keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"OBS_MODE",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get OBS_MODE keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"OBS_MODE",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set OBS_MODE keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"TDDES",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TDDES keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TDDES",ckeyval,"Root Data descriptor",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TDDES keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYD(il[0],"RA_PNT",&dtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get RA_PNT keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYD(olun,"RA_PNT",dtemp,9,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set RA_PNT keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYD(il[0],"DEC_PNT",&dtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DEC_PNT keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYD(olun,"DEC_PNT",dtemp,9,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set DEC_PNT keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYE(il[0],"EQUINOX",&rtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get EQUINOX keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYF(olun,"EQUINOX",rtemp,2,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set EQUINOX keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"RADECSYS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get RADECSYS keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"RADECSYS",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set RADECSYS keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYJ(il[0],"DELTAT",&itemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DELTAT keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYJ(olun,"DELTAT",itemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set DELTAT keyword in output file");
    Fcerrm(pstat);
  }
  
  FCGKYJ(il[0],"TIMEDEL",&itemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIMEDEL keyword from input file");
    Fcerrm(pstat);
  }
  
  FCPKYD(olun,"TIMEDEL",9.5367431640625e-7,13,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIMEDEL keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYD(il[0],"TIERRELA",&dtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIERRELA keyword from input file");
    XTE_Fcecho("Setting to 0.0d0");
    pstat=0;
    dtemp=0.0;
  }

  FCPKYD(olun,"TIERRELA",dtemp,14,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIERRELA keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYD(il[0],"TIERABSO",&dtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIERABSO keyword from input file");
    XTE_Fcecho("Setting to 0.0d0");
    pstat=0;
    dtemp=0.0;
  }

  FCPKYD(olun,"TIERABSO",dtemp,14,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIERABSO keyword in output file");
    Fcerrm(pstat);
  }

/* Due to the question of precision, MJDREF may have to be broken up into
   an integer part and a fractional part. So we first test to see if 
   MJDREF is present. If it isn't then we search for MJDREFI and MJDREFF
   and print that information out if found. */

  FCGKYD(il[0],"MJDREF",&dtemp,comm,&pstat);
  if(pstat != 0){
    pstat=0;
    FCGKYJ(il[0],"MJDREFI",&itemp,comm,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not find MJDREF or MJDREFI. ");
      Fcerrm(pstat);
      exit(1);
    }  
    FCPKYJ(olun,"MJDREFI",itemp,comm,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not set MJDREFI keyword in output file");
      Fcerrm(pstat);
    }

    FCGKYD(il[0],"MJDREFF",&dtemp,comm,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get MJDREFF keyword from input file");
      Fcerrm(pstat);
    }

    FCPKYD(olun,"MJDREFF",dtemp,fifteen,comm,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not set MJDREFF keyword in output file");
      Fcerrm(pstat);
    }
    pstat=0;
  }

  else FCPKYD(olun,"MJDREF",dtemp,fifteen,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set MJDREF keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYD(il[0],"TIMEZERO",&dtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TIMEZERO keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYD(olun,"TIMEZERO",dtemp,11,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TIMEZERO keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYD(il[0],"TSTART",&dtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TSTART keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYD(olun,"TSTART",dtemp,11,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TSTART keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYD(il[0],"TSTOP",&dtemp,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get TSTOP keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYD(olun,"TSTOP",dtemp,11,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TSTOP keyword in output file");
    Fcerrm(pstat);
  }

  FCGKYS(il[0],"OBJECT",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get OBJECT keyword from input file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"OBJECT",ckeyval,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set OBJECT keyword in output file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TPACK2","8,3","Data is packed",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TPACK2 keyword in output file");
    Fcerrm(pstat);
  }

  FCPKYS(olun,"TDISP2","Z8.2","Display format",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TDISP keyword in output file");
    Fcerrm(pstat);
  }


  FCPKYS(olun,"TDDES2","D[0~4] & E[(512~1023)^(1536~2047)] & C[0~255]"," ",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set TDDES2 keyword in output file");
    Fcerrm(pstat);
  }

  if(wrt_parm[0] == 'X'){
    FCPLSW(olun,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not include LONGSTRN warning");
      Fcerrm(pstat);
    }


    {
      int icom;
      char *comments[] = 
	{
	  " ",
	  "E[VPR,CAL,VXH,VXL] are bit flags for various PCU anodes.",
          "   They are either TRUE or FALSE.",
	  "   VPR = Propane; CAL = calibration event;",
          "   VXL/H = veto layer pulse height low/high bit.",
	  " ",
	  "E[0:63]{6} stands for E[X3R,X3L,X2R,X2L,X1R,X1L] layers.",
	  "   Thus the bit pattern is: 5 , 4 , 3 , 2 , 1 , 0 ",
	  "   so the bit value is:    32 ,16 , 8 , 4 , 2 , 1 ",
	  "   Thus X3R=32, X3L=16, X2R=8, X2L=4, X1R=2, X1L=1",
	  "   If only X2R is desired, E[0:63] == 8.",
	  "   If X1R and X3R is desired, E[0:63] == 2 & E[0:63] == 32",
	  "   If all layers are desired, do not filter on this parameter.",
	  " ",
	  0
	};
      for (icom = 0; comments[icom]; icom++) {
	FCPCOM(olun,comments[icom],&pstat);
	if(pstat != 0){
	  XTE_Fcecho(" ");
	  XTE_Fcecho("Could not write output comment card:");
	  XTE_Fcecho(comments[icom]);
	}
      }
    }


    /* Special TEVTB2 for GoodVLE mode */
    strcpy(s1,
	   "(M[1]{1},D[0:4]{3},E[0:63]{6},E[VXH]{1},E[VXL]{1},"
	   "E[CAL]{1},E[VPR]{1},S[Zero]{2})");
    FCPKLS(olun,"TEVTB2",s1,"Event Table descriptor",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not set TEVTB2 keyword in output file");
      Fcerrm(pstat);
    }
  }
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL vle2fit
#endif
#ifdef unix
#define F77CALL vle2fit_
#endif

void F77CALL() 
{ 
 void Vle2fits();

 Vle2fits(); 
} 
