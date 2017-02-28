/*
  FILENAME:    trans2fits.c
  purpose:     Translate Transparent Mode data 1, 2, & 3 to SE FITS file
               or to file with ASCII columns describing the bits.
  author:      Brian K. Elza
  date:        March 1995
  
status:      
  October:  Initial test and work writing wrappers.

  December: Wrappers abandoned for use with MACROS using new cfortran.h
            file.

  January:  Completed first iteration of code but not set up for usage with
            IRAF.

  February: Code modified to work with IRAF and incorporates all modifications
            to header files. Code modified to write out bitstream of data
	    in a nX format. Data descriptor written so that code can work
	    with SEEXTRCT in HEXTE mode. 

  March:    Modified to accept a file which contains a list of files rather
            than requiring a separate file on each input line. Also modified
	    to sort the files into transparent mode 1, 2, and 3 so that they
	    can be processed properly. All help files and support files were 
	    completed. Also modified to write out the bitmask into a specific
	    file for input into FSELECT and allow the output to be either
	    of the same form of science event (SE) FITS files and the other
	    supported output file will have a separate column for each of the
	    flags and will show the actual value of that flag.

	    Modified to run under IRAF (the added code is at the bottom).
	    And converted all Fcecho calls to XTEfcecho due to OSF memory 
	    allocation problem and the way that cfortran.h and the C 
	    preprocessor functions. Since Fcecho was used so many times
	    it was causing the OSF (ALPHA) compiler to die and the code was
	    slowing down substantially.

  17Nov97: MJT changes re: FITSIO v5.04 (FCPDEF,FCBDEF calls deleted)
           ==> version 4.0a

  15Jul98: MJT fixed typo in Check_and_Write_Info (v4.1)
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
   a standard ASCII dump of the data in the Transparent Mode data
   files.*/

static char ttypex[MXELEM][FITS_CLEN_HDEFKWDS] = {
  "TIME", "Event"
  };
 
static char tformx[MXELEM][FITS_CLEN_HDEFKWDS] = {
    "D", "24X"
  };

static char tunitx[MXELEM][FITS_CLEN_HDEFKWDS] = {
  "s"," "
   };

static char ttype[MXELEM][FITS_CLEN_HDEFKWDS] = {
  "TIME", "PCUID", "PulseHeight", "XL1", "XR1", "XL2", "XR2", "XL3", "XR3",
  "Calibration", "Propane", "Veto", "VLE"
  };
 
static char tform[MXELEM][FITS_CLEN_HDEFKWDS] = {
    "D", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I"
};

static char tunit[MXELEM][FITS_CLEN_HDEFKWDS] = {
  "s"," ","Channel"," "," "," "," "," "," "," "," "," "," "};

static char extnam[20] = "XTE_SE";

extern void XTE_Fcecho();
extern int PCU_ID();
extern int Check_Time();
extern int Reorder_Events();
extern int Read_Line();
extern void Add_Event_X();

void Check_and_Write_Info();
void Write_Events_to_Fits();
int Read_Param();
void Add_Event_A();
void Write_Event();

/* Subroutine TRANS2FITS starts */
void Trans2fits()
  {
  char infile1[MAXC_FNAME], infile2[MAXC_FNAME], infile3[MAXC_FNAME],
    outfile[MAXC_FNAME], wrt_parm[10], tmp[MAXC_FNAME],
    file1[MAXC_FNAME], file2[MAXC_FNAME], file3[MAXC_FNAME];
  char *cmptest1, *cmptest2;

  char ttype1[MXELEM][FITS_CLEN_HDEFKWDS],tform1[MXELEM][FITS_CLEN_HDEFKWDS],
    tunit1[MXELEM][FITS_CLEN_HDEFKWDS], extnam1[FITS_CLEN_COMMENT];
  char ttype2[MXELEM][FITS_CLEN_HDEFKWDS],tform2[MXELEM][FITS_CLEN_HDEFKWDS],
    tunit2[MXELEM][FITS_CLEN_HDEFKWDS], extnam2[FITS_CLEN_COMMENT];
  char ttype3[MXELEM][FITS_CLEN_HDEFKWDS],tform3[MXELEM][FITS_CLEN_HDEFKWDS],
    tunit3[MXELEM][FITS_CLEN_HDEFKWDS], extnam3[FITS_CLEN_COMMENT];

  char trans1[FitsStrBufLen+1], trans2[FitsStrBufLen+1], 
    trans3[FitsStrBufLen+1], comment1[FITS_CLEN_COMMENT],
    ctemp[3], comment2[FITS_CLEN_COMMENT], comment3[FITS_CLEN_COMMENT];
  char errtxt[FITS_CLEN_ERRMSG];

  int Nrows[3], Nfield[3], Rows[3];
  int status, olun, simple=1, bitpix=8, naxis=0, naxes[99], pcount=0, 
    gcount=1, extend=1, frow, felem, nelem, nrows, tfield, vardat, 
    iflag[19], jflag[19], colnum, dattyp, rcount, width;
  
  float equinox, value[19], day[30];

  
  int ex1, ex2, ex3, ilun1, ilun2, ilun3, i, ilunt1, ilunt2, ilunt3,
    ext1, ext2, ext3, block, zero, one, two, xtend, fd[3], rowval[3],
    test, pstat, nfield, nfieldx;
  double time1, time2, time3;

  unsigned int *events[3];
  int numberEvents[3];
  double rowTimes[3], timesave=0.0;
  unsigned int lostEventCounts[3];
  unsigned char triggerFlags[3];
  unsigned char spillageFlags[3];
  int iii,ii,ilen,ierror,runit=1, BufLen_2=255,val_len=21;
  int irow=0;
  char cval[21], update[80];

/* Initial all variables that will be receiving information */

  status=ierror=0;
  ex1=ex2=ex3=ilun1=ilun2=ilun3=olun=zero=pstat=xtend=dattyp=rcount=width=0;
  one=1;
  two=2;
  nfieldx=2, nfield=13;
  nrows=1;
  Rows[0]=Rows[1]=Rows[2]=0;
  ctemp[0]='1';
  ctemp[1]='2';
  ctemp[2]='3';

  XTE_Fcecho(" ");
  XTE_Fcecho("Running TRANS2FITS version 4.1");
  XTE_Fcecho("=============================================");

/* Read the Parameter file to get the input and output filenames. */

  status=Read_Param(infile1,infile2,infile3,outfile,wrt_parm); 
  if(status !=0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not complete Read_Param call");
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

  Fcpars(infile3, file3, &ex3, &pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not parse third input file");
    Fcerrm(pstat);
  }
  if(ex3 != 0)ex3 = 1;

  XTE_Fcecho("Reading in Transparent mode files for 1, 2, & 3");
  XTE_Fcecho(file1);
  XTE_Fcecho(file2);
  XTE_Fcecho(file3);
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

  FCGIOU(&ilun3,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get logical unit number for file3");
    Fcerrm(pstat);
  }

  FCGIOU(&olun,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get logical unit number for X outfile");
    Fcerrm(pstat);
  }

/*  (void) printf("LUNS are %i,%i,%i,%i\n",ilun1, ilun2, ilun3, olun); */
  
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

  FCOPEN(ilun3,file3,zero,&block,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not open input file3");
    Fcerrm(pstat);
    exit(1);
  }

/* Create an output file with the appropriate output file name. */
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

  
/* Move to the appropriate extension for each of the three
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

  FCMAHD(ilun3,ex3+1,&xtend,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not move to proper extension in file3");
    Fcerrm(pstat);
    exit(1);
  }

/* Lets do a check to make sure that we are dealing with files of the 
   proper type */

  FCGKYS(ilun1,"DATAMODE",trans1,comment1,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATAMODE information for file 1");
    Fcerrm(pstat);
    exit(1);
  }
  FCGKYS(ilun2,"DATAMODE",trans2,comment2,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATAMODE information for file 2");
    Fcerrm(pstat);
    exit(1);
  }
  FCGKYS(ilun3,"DATAMODE",trans3,comment3,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATAMODE information for file 3");
    Fcerrm(pstat);
    exit(1);
  }
  
  ilunt1=ilun1, ext1=ex1;
  ilunt2=ilun2, ext2=ex2;
  ilunt3=ilun3, ext3=ex3;

  ilen=strlen(trans1);
  for(ii=0; ii < ilen; ii++) {
    if(trans1[ii] == '_')ii=ilen+1;    

    if(trans1[ii] == '2'){
     ilunt2=ilun1;
     ext2=ex1;
    }
    if(trans1[ii] == '3'){
     ilunt3=ilun1;
     ext3=ex1;
    }
  }

  ilen=strlen(trans2);
  for(ii=0; ii < ilen; ii++) {
    if(trans2[ii] == '_')ii=ilen+1;

    if(trans2[ii] == '1'){
     ilunt1=ilun2;
     ext1=ex2;
    }
    if(trans2[ii] == '3'){
     ilunt3=ilun2;
     ext3=ex2;
    }
  }

  ilen=strlen(trans3);
  for(ii=0; ii < ilen; ii++) {
    if(trans3[ii] == '_')ii=ilen+1;

    if(trans3[ii] == '1'){
     ilunt1=ilun3;
     ext1=ex3;
    }
    if(trans3[ii] == '2'){
     ilunt2=ilun3;
     ext2=ex3;
    }
  }

  ilun1=ilunt1;
  ilun2=ilunt2;
  ilun3=ilunt3;

/* Set up integer pointer to correct IO unit. */
  fd[0]=ilun1;
  fd[1]=ilun2;
  fd[2]=ilun3;

/* We have had to rearrange the files and the extension numbers are
   different we must move to the correct extension again. */
  if(ex1 != ext1 || ex2 != ext2 || ex2 != ext3) {
    ex1=ext1;
    ex2=ext2;
    ex3=ext3;
    
/* Move to the appropriate extension for each of the three
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

    FCMAHD(ilun3,ex3+1,&xtend,&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not move to proper extension in file3");
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
  
  FCGHBN(ilun3,MXELEM,&Nrows[2],&Nfield[2],ttype3,tform3,tunit3,extnam3,&pcount,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get information file format for file 3");
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

    cmptest2 = &ttype3[ii][0];
    if(strcmp(cmptest1,cmptest2)){
      XTE_Fcecho("TTYPEs do not match for File1 and File3");
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

    cmptest2 = &tform3[ii][0];
    if(strcmp(cmptest1,cmptest2)){
      XTE_Fcecho("TFORMs do not match for File1 and File3");
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

    cmptest2 = &tunit3[ii][0];
    if(strcmp(cmptest1,cmptest2)){
      XTE_Fcecho("TUNITs do not match for File1 and File3");
      XTE_Fcecho("Check files to be sure they are correct.");
      exit(1);
    }
  }

/* Since all three files MUST have exactly the same FORMAT, we can simply
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
/*  if(rcount != 16383){
    XTE_Fcecho(" ");
    XTE_Fcecho("Number of elements to be processed != 16383 as necessary.");
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
/*  FCBDEF(olun,nfieldx,tformx,zero,nrows,&pstat); 
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
/*  FCBDEF(olun,nfield,tform,zero,nrows,&pstat); 
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

  Check_and_Write_Info(ilun1, ilun2, ilun3, olun, wrt_parm);

/* Now we KNOW that formats for each of the input files and that never 
   changes so there is no need to try to extract that information from 
   the FITS files. So we will have that information hardwired into the 
   code. This may cause problems at a later point, but for now we will 
   operate within these parameters and stick to the code supplied by 
   MIT as closely as possible. */

  for (ii =0; ii < 3; ii++){
    events[ii] = (unsigned int *) malloc(16384 * sizeof(unsigned int));
    if(!(events[ii])){
      XTE_Fcecho("Out of Memory. Cannot continue! Aborting");
      exit(1);
    }
  } 
	
  XTE_Fcecho("Initializing values from input files ...");

  for(ii =0; ii < 3; ii++){
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

  for(ii=0;ii < 3;ii++)Rows[ii]=0;

  XTE_Fcecho(" ");
  XTE_Fcecho("Initialization complete. Begin processing data:");

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

    for (ii =0; ii < 3; ii++){
      if((*(Rows+ii) < Nrows[ii] -1) && !pstat){
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

    /*  Keep reading rows until we find 3 with the same timestamp. */

    while (( (rowTimes[1] != rowTimes[0]) || (rowTimes[2] != rowTimes[0])) && runit){

      for (ii =1; ii < 3; ii++){
	
	if(*(Rows+ii) < Nrows[ii] - 1){
	  
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
	      (void) printf("Nrows is %i,%i\n",Nrows[ii]-1,*(Rows+ii));
	      (void) printf("Rowtimes %f\n",*(rowTimes+ii));
	      (void) printf("Number of Events is %i\n",numberEvents[ii]);
	      (void) printf("lostEventCounts %u\n",*(lostEventCounts+ii));
	      (void) printf("spillageFlags %i\n",*(spillageFlags+ii));
	      (void) printf("TriggerFlags %i\n",*(triggerFlags+ii));
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
      Write_Events_to_Fits(olun, &irow, &timesave, rowTimes[0], events, numberEvents, &runit, wrt_parm); 

/* Since we have processed one complete ROW let's increment the row
   number being processed in both files and continue... */
    for (ii =0; ii < 3; ii++)Rows[ii]++;
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

  for (ii =0; ii < 3; ii++)  
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
   GTI extension. We are going to copy it into the created file. */
  FCMRHD(ilun1,one,&xtend,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not move to GTI extension in first input file.");
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
	XTE_Fcecho("Could not COPY extension from file1 to output file.");
	XTE_Fcecho("Continuing to close output file.");
	pstat = 0;
      }
    } else {
      XTE_Fcecho("Could not create GTI extension in output file.");
      XTE_Fcecho("Continuing to close output file.");
      pstat = 0;
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

  FCCLOS(ilun3,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not close input file3");
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

  FCFIOU(ilun3,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not free logical unit number for file3");
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

  XTE_Fcecho("Finished creating SE file Transparent Mode data.");

  }

int Read_Param(rfname1,rfname2,rfname3,ofname,wrt_parm) 
     char *rfname1, *rfname2, *rfname3, *ofname, *wrt_parm; 
{
  int BufLen_2 = 255;
  int MaxElem_2 = 3;
  int parstat, zero, ival2, cols=0,log=0,ilen,i,ihold;
  char text[FITS_CLEN_ERRMSG];
  char names[3][256];
  parstat = 0;
  
  Uclgst("trans1", rfname1, &parstat);
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
    strcpy(rfname3,&names[2][0]);
  }
  else{
    Uclgst("trans2",rfname2, &parstat);
    if(parstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get SECOND input file name.");
      XTE_Fcecho("Check input file name!");
      FCGERR(parstat, text);
      exit(1);
    }

    Uclgst("trans3",rfname3, &parstat);
    if(parstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get THIRD input file name.");
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
  
  return parstat;
  }
  
/* ====================================================================== */

/*******************************************************
  This routine adds a row (event) to a FITS file.
  ******************************************************/
void Add_Event_A(ounit, irow, time, pulseHeight, l1, r1, l2, r2, l3, r3, propane, veto, calibration, vle, pcu)
     double time; 
     int pulseHeight, l1, r1, l2, r2, l3, r3;
     int ounit;
     int irow; 
     int propane, veto, calibration, vle, pcu;
{

  int stat=0,staterr=0;
  int felem=1, nelem=1, col=0, fifteen=15;

  int lpulseHeight[1], ll1[1], lr1[1], ll2[1], lr2[1], ll3[1], lr3[1];
  int lpropane[1], lveto[1], lcalibration[1], lvle[1], lpcu[1];

  lpulseHeight[0]=  pulseHeight;
  ll1[0]= l1; 
  lr1[0]= r1; 
  ll2[0]= l2;  
  lr2[0]= r2;  
  ll3[0]= l3;  
  lr3[0]= r3; 
  lpropane[0]= propane;
  lveto[0]= veto;
  lcalibration[0]= calibration;
  lvle[0]= vle;
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
  FCPCLJ(ounit,col,irow,felem,nelem,lpulseHeight,&stat);    
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put PULSEHEIGHT information into outputfile");
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
  col++;
  FCPCLJ(ounit,col,irow,felem,nelem,lcalibration,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put CALIBRATION information into outputfile");
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
  FCPCLJ(ounit,col,irow,felem,nelem,lvle,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put VLE information into outputfile");
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
void Write_Event(ounit, irow, timesave, rowTime, micro, t1, t2, t3, wrt_parm)
     double rowTime, *timesave; 
     int ounit; 
     int irow;
     unsigned long micro; 
     unsigned short t1, t2,t3;
     char *wrt_parm;
{
  double tt;
  int l1,l2,l3,r1,r2,r3,vle, propane,veto, calibration, pcu, pulseheight;
  int i,value[24];

  for(i=0; i < 24 ; i++){
    value[i]=0;
  }

  if ((t2 & 0x03) != ((t3 &0xc00) >> 10)){
    if ((t3 & 0x3ff) == 0x3ff)
      t3 = ((t3 & 0xf000) | (((t3 & 0xfff) + 1) & 0xfff));
    else if ((t3 & 0x3ff) == 0x000)
      t3 = ((t3 & 0xf000) | (((t3 & 0xfff) - 1) & 0xfff));
    else
      fprintf(stdout,"time mismatch between t3 and t2 %x %x, at time %ld, %ld\n",
	      t2, t3, rowTime, micro);
  }

/* Here we are calculating the amount of time that is to be added to the
   timestamp that is in the file. This is calculated from two parts. The 
   first is extracted from the transparent mode 2 data (t2) and the last
   is gotten from the transparent mode 3 data file (t3). This is added to 
   tt and than that is added to rowTime to calculate the total time. */
  micro += 1024 * (t2 & 0x1c);
  micro += (t3 & 0x0fff);
  tt = (double) micro/1048576.;
  tt += rowTime;
  *timesave=tt;

/* Due to the way that FITSIO works it is VERY HARD to pass it a bit stream
   and to have that bit stream properly inserted. It is therefore easier to 
   create an integer array composed of 0's and 1's and to pass that for each
   bit that is to be inserted into the output file. Thus in order to output
   a 24X column of data we have to construct a variable[24] array and to 
   insert that via fcpcli so that it thinks it is outputting an array of 
   integers. Also since I constructed the integer array with value[0] 
   containing the least significant bit going up to 24 which is the reverse
   of how FITSIO interprets this data stream, I will reverse it
   in Int2log*

   So the array that I am creating goes like this
    23-16            15-8         7-0   with each bit defined as follows

   23       22      21       20       19    18     17         16     
 M[1]{1} [Zero]{1}   -------------------    |      |           |     
    /       /                 |             |      |           |      
    1       0       P  C  U  I  D {3}   E[CAL]   E[VLE]    Propane{1} 
                        D[0:4]{3}     E[CAL]{1} E[VLE]{1}   E[VPR]{1}    


    15       14         13       12       11       10       9        8     
      ---------           ---------------------------------------------      
          |                                 |                            
        Veto{2}      E[X3R   X3L      X2R      X2L      X1R     X1L] {6} 
 E[VXH]{1} E[VXL]{1}                    E[0:63]{6}

    7        6        5        4        3        2        1        0
   (128)    (64)     (32)     (16)     (8)      (4)      (2)      (1)
    ----------------------------------------------------------------
                                   |
                        Pulse Height Channel {8}

 */

  pulseheight = (t1 & 0x0ff);	/* Pulse Height Channel*/
  value[0] = (pulseheight & 0x1); /* 1 bit */
  value[1] = (pulseheight & 0x2) >> 1; /* 2 bit */
  value[2] = (pulseheight & 0x4) >> 2; /* 4 bit */
  value[3] = (pulseheight & 0x8) >> 3; /* 8 bit */
  value[4] = (pulseheight & 0x10) >> 4; /* 16 bit */
  value[5] = (pulseheight & 0x20) >> 5; /* 32 bit */
  value[6] = (pulseheight & 0x40) >> 6; /* 64 bit */
  value[7] = (pulseheight & 0x80) >> 7; /* 128 bit */

  vle = (t2 & 0x20) >> 5;	/* VLE */
  value[17] = vle;

  l1 = (t2 & 0x40) >> 6;	/* L1 */
  value[8] = l1;

  r1 = (t2 & 0x80) >> 7;	/* R1 */
  value[9] = r1;

  l2 =  (t2 & 0x100) >> 8;	/* L2 */
  value[10] = l2;

  r2 =  (t2 & 0x200) >> 9;	/* R2 */
  value[11] = r2;

  l3 =  (t2 & 0x400) >> 10;	/* L3 */
  value[12] = l3;

  r3 = (t2 & 0x800) >> 11;	/* R3 */
  value[13] = r3;

  calibration =  (t1 & 0x800) >> 11; /* Calibration Bit */
  value[18] = calibration;

  veto = (t1 & 0x300) >> 8;	/* Vx is a 2 bit string VxH and VXL*/
  value[14] = (veto & 0x1);  /* Set the 14th bit to Vxl */
  value[15] = (veto & 0x2) > 1; /* Set the 15th bit to Vxh */

  propane =  (t1 & 0x400) >> 10; /* Propane */
  value[16] = propane;

  pcu = PCU_ID(t1);              /* PCUID this is a 3 bit string */
  value[19] = (pcu & 0x1);
  value[20] = (pcu & 0x2) >> 1;
  value[21] = (pcu & 0x4) >> 2;
  value[23] = 1; /* Set the M[1]{1} value to agree with other DDES's */

/* If we want separate ASCII columns we use Add_Event_A, if we want a 
   bit-stream column titled Event then we use Add_Event_X. */

  if(wrt_parm[0] == 'X')Add_Event_X(ounit, irow, tt, value);
  if(wrt_parm[0] == 'A')Add_Event_A(ounit, irow, tt, pulseheight, l1, r1, l2, r2, l3, r3, propane, veto, calibration, vle, pcu);

}

/******************************************************************
  This routine take the 3 event list arrays, does some sanity checks, 
  and finds and writes out all the real events.  
  *****************************************************************/
void Write_Events_to_Fits(ounit, irow, timesave, partTime, eventLists, numberEvents, runit, wrt_parm)
     double partTime, *timesave; 
     unsigned int **eventLists; 
     int *numberEvents, ounit, *runit;
     int *irow;
     char *wrt_parm;
{
  int ii;
  int i,j, ifirst = 6, isecond = 3;
  unsigned long mtic=0;
  unsigned short delta=0;
  int id, wrong_order=0;
  int irowtemp;
  unsigned long start;

  /* First lets do some sanity checking on the event lists */
  for (ii =0; ii < 3 && *runit != 0 ; ii++)
    if (numberEvents[ii] > 16384 ){
      fprintf(stdout, "Too many events in partition at time %lf.\n", partTime);
      printf("Number of events is %i\n",numberEvents[ii]);
      
      *runit = 0;
    }

  for (ii =0; ii < 3 && *runit != 0 ; ii++)
    if (numberEvents[ii] < 16383 )
      if (eventLists[ii][numberEvents[ii] - 1] != 0x7f7f){
	/*	fprintf(stdout,"NumberEvents is %i %i\n",ii,numberEvents[ii]);
	fprintf(stdout,"Value is %x\n",eventLists[ii][numberEvents[ii] - 1]); 
	fprintf(stdout,"Value is %x\n",eventLists[ii][numberEvents[ii] - 2]); */
	fprintf(stdout, "Last event not End-of-Data in partition at time %lf.\n", partTime);
	*runit = 0;
      }

  for (ii =0; ii < 3 && *runit != 0 ; ii++)
    if ((eventLists[ii][0] != 0x7ff8) && (eventLists[ii][0] != 0x7ffc)){
      fprintf(stdout,"First event not interval mark in partition at time %lf.\n", partTime);
      *runit = 0;
    }

  for (ii =1; ii < 3 && *runit != 0 ; ii++)
    if (numberEvents[ii] != numberEvents[0]){
      fprintf(stdout, "Warning, partitions have different lengths %d %d %d\n", numberEvents[0], numberEvents[1], numberEvents[2]);
      /*      if(numberEvents[0] > numberEvents[ii])numberEvents[0]=numberEvents[ii]; */
    }

  for (i=1; i< numberEvents[0] - 1; i++){
    wrong_order = 0;
    id = PCU_ID(eventLists[0][i]);
    for (j=1; j < 3; j++)
      if (PCU_ID(eventLists[j][i]) != id)
	wrong_order=1;
    if (wrong_order == 1)
      if (Reorder_Events(eventLists, i, ifirst, isecond))
	wrong_order = 0;
    if (wrong_order == 0){

      if (eventLists[0][i] >= 0x8000){
	 irowtemp= *irow;
	 irowtemp++;
	 *irow=irowtemp;
	Write_Event(ounit, irowtemp, timesave, partTime + delta, mtic,eventLists[0][i],eventLists[1][i],eventLists[2][i], wrt_parm);

      }
	    
      else{ 
	mtic += 32768;
	if (mtic >= 0x100000){
	  mtic -= 0x100000;
	  delta++;
	}  
      }
    }

    else{
      fprintf(stdout, "Unable to switch events: \n");
      fprintf(stdout, "First Event: %x %x %x\n", eventLists[0][i],eventLists[1][i],eventLists[2][i]);
      fprintf(stdout, "Second Event: %x %x %x\n", eventLists[0][i + 1],eventLists[1][i + 1],eventLists[2][i + 1]);
      fprintf(stdout, "Third Event: %x %x %x\n", eventLists[0][i + 2],eventLists[1][i + 2],eventLists[2][i + 2]);
      fprintf(stdout, "Fourth Event: %x %x %x\n", eventLists[0][i + 3],eventLists[1][i + 3],eventLists[2][i + 3]);
      fprintf(stdout, "Fifth Event: %x %x %x\n", eventLists[0][i + 4],eventLists[1][i + 4],eventLists[2][i + 4]);
      /*
      fprintf(stdout, "Check these files! \n");
      fprintf(stdout,"These files may have been constructed improperly! \n");
      *runit = 0; */
    }
  }
}

void Check_and_Write_Info(ilun1, ilun2, ilun3, olun, wrt_parm)
     int ilun1,ilun2,ilun3, olun;
     char *wrt_parm;
{
  int il[3],i,itemp,pstat=0;
  double dtemp=0.0;
  float rtemp=0.0;
  char ckeyval[FITS_CLEN_KEYVAL],comm[FITS_CLEN_COMMENT];
  char ckeyval2[FITS_CLEN_KEYVAL],ckeyval3[FITS_CLEN_KEYVAL];
  char s1[200],s2[100];
  int fifteen=15;
  char trans1[FitsStrBufLen+1],datamode[FitsStrBufLen+1];
  int ilen,ii,ihold=0;

  /* il[0]=ilun1, il[1]=ilun2, il[3]=ilun3; 
  15Jul98 (MJT) How could this have ever worked ????? */
  il[0]=ilun1, il[1]=ilun2, il[2]=ilun3;

  FCPKYS(olun,"ORIGIN","XTE-GOF","Created from Transparent Mode 1, 2, and 3 PCA data.",&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not set ORIGIN keyword in output file");
    Fcerrm(pstat);
  }

  if(wrt_parm[0] == 'X'){
    FCPKYS(olun,"CREATOR","TRANS2FITSX","Created with supplied C code wrapped in FTOOLS format",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not set CREATOR keyword in output file");
      Fcerrm(pstat);
    }
  }

  if(wrt_parm[0] == 'A'){
    FCPKYS(olun,"CREATOR","TRANS2FITS","Created with supplied C code wrapped in FTOOLS format",&pstat);
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

  FCGKYS(il[0],"DATAMODE",trans1,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get DATAMODE information for file 1");
    XTE_Fcecho("Setting DATAMODE to read Transparent");
    strcpy(trans1,"TransparentN_UNKNOWN");
    pstat=0;
  }

  ilen=strlen(trans1);
  ihold==0;
  strcpy(datamode,"");

  for(ii=0; ii < ilen; ii++) {
    if(trans1[ii] == '_'){
      trans1[ii-1]='\0';
      ihold=ii;
      strcat(datamode,trans1);
      /*      strcat(datamode,&trans1[ii]); */
      ii=ilen-1;
    }
  }

  for(ii=ilen-1; ii >= ihold; ii--){
    if(trans1[ii] == '_' || ii == ihold){
      strcat(datamode,&trans1[ii]);
      ii = ihold-1;
    }
  }

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

  FCGKYS(il[1],"TIMESYS",ckeyval2,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get keyword from input file");
    Fcerrm(pstat);
    exit(1);
  }

  FCGKYS(il[2],"TIMESYS",ckeyval3,comm,&pstat);
  if(pstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get keyword from input file");
    Fcerrm(pstat);
    exit(1);
  }

  /* Check to see the time systems match */
  if(strcmp(ckeyval,ckeyval2) != 0 ) {
    XTE_Fcecho(" ");
    XTE_Fcecho("Time systems are NOT the same! Aborting...");
    exit(1);
  }

  if(strcmp(ckeyval,ckeyval3) != 0 ) {
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
    XTE_Fcecho("To avoid this use the original TIMESYS=TT Good Xenon");
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
    XTE_Fcecho("To avoid this use the original TIMESYS=TT Good Xenon");
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


  FCPKYS(olun,"TDDES2","D[0~4] & E[0~2047] & C[0~255]"," ",&pstat);
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

    FCPCOM(olun," ",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write blank comment line");
    }

    FCPCOM(olun,"E[0:63]{6} stands for E[X3R,X3L,X2R,X2L,X1R,X1L] layers.",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write comment to output file");
    }

    FCPCOM(olun,"Thus the bit pattern is: 5 , 4 , 3 , 2 , 1 , 0 ",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write comment to output file");
    }

    FCPCOM(olun,"So the bit value is:    32 ,16 , 8 , 4 , 2 , 1 ",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write comment to output file");
    }

    FCPCOM(olun,"Thus X3R=32, X3L=16, X2R=8, X2L=4, X1R=2, X1L=1",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write comment to output file");
    }

    FCPCOM(olun,"If only X2R is desired, E[0:63] == 8.",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write comment to output file");
    }

    FCPCOM(olun,"If X1R and X3R is desired, E[0:63] == 2 & E[0:63] == 32",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write comment to output file");
    }

    FCPCOM(olun,"If all layers are desired, do not filter on this parameter.",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write comment to output file");
    }

    FCPCOM(olun," ",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write blank comment line");
    }

    FCPCOM(olun,"IF filtering on E[VLE] is desired, E[VLE] == 0 or 1, etc.",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write blank comment line");
    }

    FCPCOM(olun," ",&pstat);
    if(pstat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not write blank comment line");
    }

    strcpy(s1,"(M[1]{1},S[Zero]{1},D[0:4]{3},E[CAL]{1},E[VLE]{1},E[VPR]{1},");
    strcpy(s2,"E[VXH]{1},E[VXL]{1},E[0:63]{6},C[0:255]{8})");
    strcat(s1,s2);
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
#define F77CALL trans2fit
#endif
#ifdef unix
#define F77CALL trans2fit_
#endif

void F77CALL() 
{ 
 void Trans2fits();

 Trans2fits(); 
} 
