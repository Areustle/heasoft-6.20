/*
  FILENAME:    fcollect.c
  author:      M. J. Tripicco
  date:        August 1995

  version: 5.10   2014-06-09  Fix strcpy() bug with overlapping memory
                              (C. Markwardt)

  version: 5.9    29 Jul 2008 Fix string overflow bug in apiddb.name, triggered
                              by the possibility of long breakfile filenames.

  version: 5.8    21 Apr 2008 Added the "breakfile" parameter, which is treated
                              as a "global" APID.  breakfile=CALDB is also
                              allowed, via the HDgtcalf() interface.  The
                              Makefile was modified to link appropriate 
                              libraries. (C. Markwardt)

  version: 5.7    26 May 2006 Removed diagnostic prints; cleaned up warnings
                              due to implicit declarations, etc.

  version: 5.6    26 Jul 2005 Added reset of status for Y2K fix in v5.5

  version: 5.5    27 Aug 2001 Changes submitted by C.Markwardt to avoid opening
                              (and uncompressing) each data file twice. Also
                              MJT added conversion of old-style DATE keywords.

  version: 5.4    21 Jun 1999 Added check in Fetch_AppId to keep it from trying
                              to open files not actually in the requested AppId

  version: 5.3    11 Mar 1999 TDISP keyword is now suppressed when TFORM has been
                              changed from "I" to "E" (since a mismatch occurs)
                             
  version: 5.2    12 Feb 1999 Now fdpath can include a named FMI file for greater
                              flexibility (eg, standard products processing).
			      NB: named FMI file *must* begin with "FMI"
  
  version: 5.1    14 Oct 1998 Writes TIMEPIXR = 0 into primary/bintable headers

  version: 5.0    06 Aug 1998 NATIVE CFITSIO! Only logic change was that ffgcvs 
                              (when called directly) returns " " for empty rows
                              so tests for strlen == 0 are strlen > 1 now.

  version: 4.3    08 Jul 1998 TSTART/TSTOP now match first/last time values!
  
  version: 4.2.1  08 May 1998 Missing data files handled more gracefully.
 
  version: 4.2    07 May 1998 Now cycles over multiple data files.

                              More 3-digit column numbers bugs fixed.

  version: 4.1    29 Apr 1998 Allows collection of *single-valued* columns
                              from pca EA*Data files. 

			      Added check to ignore vector columns.

                              Tweaked sscanf recipes for cases when
                              col is >= 100 (ie TTYPE99 = 'xyz' vs. 
                              TTYPE100= 'xyz').

			      Missing subsystem index files (FI*) now 
                              warned & ignored.

  version: 4.01   10 Jul 1997 changed Rd_Param() -> readpar() for dispatch lib

  version: 4.0    11 Jun 1997 finally freeing memory (duh). Version #
                              synchronized for FTOOLS v4.0 release.

  version: 3.8    13 Mar 1997 cmdhvXE now forced to be a discrete quantity;
                              discret() called instead of lintrp2()

  version: 3.7    05 Feb 1997 Improved error-handling for exisiting
                              output file (and cleared FITSIO error
                              stack following FCGCNN calls)

  version: 3.6    04 Nov 1996 ACSEARTHLON now placed on monotonically 
                              increasing scale prior to interpolation
                              and then rescaled modulo 360

  version: 3.5    23 Oct 1996 include stdprod subsys again and reverse
                              sense of for-loop filling AppId struct array

  version: 3.4    16 Oct 1996 fixed bug for *Hk* columns in FIHX files
                              with zero rows (treated as empty column)

  version: 3.3    02 Oct 1996 added TIMEDEL header keyword to primary and 
                              bintable extension

 (version: 3.2.1  18 Oct 1996 bug fix from 3.4 replicated here)
                              (for release w/FTOOLSv3.6.0)

  version: 3.2    12 Sep 1996 bug fixes: initializing comm, newaname dims
                              (rel w/v3.6.0)

  version: 3.1    27 Aug 1996 Changing any I-type columns to E and dropping
                              scaling (which fmerge can't handle). Also
                              made minor style/wording modifications.

  version: 3.0    30 May 1996 Now handling TZERO/TSCAL columns correctly
                              Also completely revamped code style (if/else)

  version: 2.9    22 May 1996 Added a new error condition for datafiles
                              listed in ssidx file but not present
  
  version: 2.8    05 Apr 1996 All timesystem keywords now appear both in
                              primary array header and bintable header
  
  version: 2.7    04 Apr 1996 Moved all timesystem keywords to bintable
  
  version: 2.6    03 Apr 1996 Can now handle a third column in the AppId
                              list which will be used as the column name
                              in the resulting filter file.
  
  version: 2.5    01 Apr 1996 TSTART,TSTOP (=startMET, stopMET) moved
                              to bintable extension header.
  
  version: 2.4    07 Feb 1996 TSTART,TSTOP (=startMET, stopMET) added
                              to primary header.
  
  version: 2.3    06 Feb 1996 Added new hidden parameter "maxgap" which
                              is the maximum time (in seconds) across
                              which the routine should interpolate.
  
  version: 2.2    01 Feb 1996 Added new hidden parameter "fname" which
                              is *written* by fcollect for later use by
                              the xtefilt Perl script.
  
                              Also changed default for outfile parameter
                              from XDF to XFD.
  
  version: 2.1    26 Jan 1996 Write time system info keywords copied 
                              from FMI into output file primary header.
  
  version: 2.0    24 Jan 1996 Replaced lintrp by lintrp2 (called once
                              per appid instead of nstep times),
                              resulting in a dramatic speedup.
  
                              Also slightly modified screen output format 
                              for AppId Data.
  
  version: 1.5.1  22 Dec 1995 Added LISTAPPIDS compile option; cleaned up
  
  version: 1.5    18 Dec 1995 Fixed problem in lintrp where the time to
                              be interpolated equals point number 1.
                              Also changed handling of cases where a
                              column name can't be found in a correct
                              AppId number.
                              THIS VERSION WORKS ON ALL UNIX PLATFORMS
  
  version: 1.4.8  15 Dec 1995 IRIX stuff: stupidly closing ascii file as 
                              a FITS file (w/FCCLOS) causes problem with 
                              the *next* call to FCCLOS.

                              Also increased ERRMSG from 31 --> 81 since
                              sprintf was overwriting adjacent memory!!!
  
  version: 1.4.5  13 Dec 1995 HP-UX stuff: initialize datapath string,
                              switch close & free ops in Fetch_AppId
                              close/free index file for no data case
  
  version: 1.4     8 Dec 1995 Prevented bailout for empty *Hk* AppIds
                              Changed lots of printf to sprintf/XTE_Fcecho
                              Added RA/DEC_OBJ,EQUINOX to primary array
                              NULLVAL changed to -999999999.
  
  version: 1.3     1 Dec 1995 catches *Hk* appids for PCA/HEXTE subsys
  
  version: 1.2    27 Nov 1995 fixed handling of INDEF for B,I,J-types
                              (also deals with TZEROn keywords properly)
  
  version: 1.1.5  14 Nov 1995 better errmsg when AppId column not found
                              stopped searching stdprod area for AppIds
  
  version: 1.1     7 Nov 1995 improved handling of empty/undefined AppIds.
                              added "outfile" parameter -- default "XDF".
  
  version: 1.0     6 Nov 1995 original as placed in XTEGOF FTOOLS area
*/

#include <stdio.h>
#include <string.h>
#include <errno.h> 
#include <ctype.h> 
#include "xpi.h"
#include "fitsio.h"
#include "cftools.h"
#include "headas.h"
#include "hdcal.h"

#define BREAKDOWN_APPID (295)
#define NULLVAL -999999999.
#define NULLB        255
#define NULLI      32767
#define NULLJ 2147483647

#define MXELEM 1            /* Allocate for number of columns for files */

#define MAXC_FNAME 1024     /* Define the size of the arrays for C */
#define ERRMSG (MAXC_FNAME+81) /* Must be able to hold the largest file name */

#define MAX_APPIDS 2000     /* Define the maximum number of AppIDs */

#ifndef TRUE
#define TRUE  1             /* Define a logical TRUE value */
#endif

#ifndef FALSE
#define FALSE 0             /* Define a logical FALSE value */
#endif

#ifndef DEBUGIT
#define DEBUGIT FALSE         /* Set Debug flag(s) for easier debugging */
#endif

#ifndef DEBUGIT1
#define DEBUGIT1 FALSE
#endif

#ifndef LISTAPPIDS
#define LISTAPPIDS FALSE     /* For ease of printing out a list of AppIds */
#endif

#define FITS_CLEN_HDEFKWDS  25 /* used to be defined in cfitsio.h... */

struct fdsubsys
{
  char *name;
  char *path;
  char *fmicnam;
  int fmicnum;
};

struct appidinfo
{
  int used;
  char name[MAXC_FNAME];
  struct fdsubsys *subsys;
  int colnum;
};

struct appdata
{
  double *timedat;
  double *appdat;
  int nrdat;
  char *hdkw;
  int numhdkw;
  int flag;
  double maxgap;
};  

int readpar(char*, char*, char*, char*, double*, double*, int*, char*);
void lintrp2(double *, double *, int, double *, double *, int, double);
void discret(double *, double *, int, double *, double *, int, double);
int Fetch_AppId(int, char*, int, char*, char*, struct appdata *);

void Fcollect()
{
  char fdpath[MAXC_FNAME], appidfile[MAXC_FNAME], obsid[MAXC_FNAME];
  char fmifile[MAXC_FNAME], obsidpath[MAXC_FNAME], outfl[MAXC_FNAME];
  char tmp[MAXC_FNAME],outfile[MAXC_FNAME];
  char breakfile[MAXC_FNAME];

  char *obs[MXELEM], *ttype[MXELEM], *tform[MXELEM], *tunit[MXELEM];
  char *startdate[MXELEM], *stopdate[MXELEM];
  char *starttime[MXELEM], *stoptime[MXELEM];
  char *user[MXELEM], *source[MXELEM], *dumstr;
  char *ttype1[MXELEM], *tform1[MXELEM], *tunit1[MXELEM];

  char mjdrefi[FLEN_CARD],mjdreff[FLEN_CARD];
  char timesys[FLEN_CARD],timezero[FLEN_CARD];
  char timeunit[FLEN_CARD],mjdcomm[FLEN_CARD];
  char racrd[FLEN_CARD], deccrd[FLEN_CARD], eqxcrd[FLEN_CARD];

  char extnam[FLEN_COMMENT], anam[FITS_CLEN_HDEFKWDS];
  char ttypex[FITS_CLEN_HDEFKWDS],tformx[FITS_CLEN_HDEFKWDS];
  char apline[81],newanam[FITS_CLEN_HDEFKWDS];
  char msg[ERRMSG], kwnam[FLEN_VALUE], ainum[4];
  char dumkw[FLEN_KEYWORD], dumcom[FLEN_COMMENT];
  char dumval[FLEN_VALUE];
  char date_obs[FLEN_VALUE], time_obs[FLEN_VALUE];
  char date_end[FLEN_VALUE], time_end[FLEN_VALUE];

  long nrows, pcount=0, dumrows, *naxes, gcount, nstep;

  int nfield, appidnum, clobber, anum, n;
  int status, ex1, xtend, pstat, dumstat;
  int i, j, anyf;
  int simple=1, bitpix=8, naxis=0; 
  int extend=1, startmetcol, stopmetcol;
  int startdatecol, stopdatecol, starttimecol, stoptimecol;
  int usercol, sourcecol;
  int stopmet, startmet, iseof=0, colno=1, validap=0;
  int obsidcol, obsrow=0, kwnum;
  int dumintval=0, currentkw=0, longrp;
  int pntinfo=0, usenewanam=0, tformwasi=0;
  int yy, mm, dd;
  
  FILE *fp;

  fitsfile *ifp1, *ofp, *ssfp;
  
  double *time, deltat, *tmpcol, appid_intrp, maxgap, dumfltval=0.0;
  
  static struct fdsubsys subsys[16];
  struct fdsubsys *subsys_global;
  static struct appidinfo appiddb[MAX_APPIDS];
  struct appdata ad;

  static char extnam1[20] = "XTE_MKF";
  static char taskname[20] = "FCOLLECT_v5.10";
  
  /* Define the TASK common block */
  c_ptaskn(taskname);
  
  /* Initialize all variables that will be receiving information */
  ex1=pstat=deltat=maxgap=anyf=dumstat=0;
  
  for (i=0;i<MAX_APPIDS;i++) {
    appiddb[i].used = 0;
  }
  
  for (i=0;i<=sizeof(subsys)/sizeof(subsys[0]);i++) {
    subsys[i].path = (char *) malloc(sizeof(char)*FLEN_FILENAME);
  }

  subsys[0].name="pca";
  subsys[0].fmicnam="PCA_Index_File";
  subsys[1].name="hexte";
  subsys[1].fmicnam="HEXTE_Index_File";
  subsys[2].name="eds";
  subsys[2].fmicnam="EDS_Index_File";
  subsys[3].name="acs";
  subsys[3].fmicnam="ACS_Index_File";
  subsys[4].name="ace";
  subsys[4].fmicnam="ACE_Index_File";
  subsys[5].name="fds";
  subsys[5].fmicnam="FDS_Index_File";
  subsys[6].name="gsace";
  subsys[6].fmicnam="GSACE_Index_File";
  subsys[7].name="ipsdu";
  subsys[7].fmicnam="IPSDU_Index_File";
  subsys[8].name="spsdu";
  subsys[8].fmicnam="SPSDU_Index_File";
  subsys[9].name="pse";
  subsys[9].fmicnam="PSE_Index_File";
  subsys[10].name="ifog";
  subsys[10].fmicnam="IFOG_Index_File";
  subsys[11].name="orbit";
  subsys[11].fmicnam="Orbit_Index_File";
  subsys[12].name="clock";
  subsys[12].fmicnam="Clock_Index_File";
  subsys[13].name="cal";
  subsys[13].fmicnam="CAL_Index_File";
  subsys[14].name="stdprod";
  subsys[14].fmicnam="Std_Prod_Index_File";
  subsys[15].name="global";
  subsys[15].fmicnam="NONE";
  subsys_global = &subsys[15];

  
  c_fcecho(" ");
  sprintf(msg,"Running %s\n========================",taskname);
  c_fcecho(msg);
  
  /* Read the Parameter file to get the input and output filenames. */
  
  status=readpar(obsid,fdpath,appidfile,outfl,&deltat,&maxgap,&clobber,breakfile); 
  if(status !=0){
    c_fcerr(" ");
    c_fcerr("Could not complete readpar call");
    c_fcerr("Terminating run");
    exit(1);
  }
  
  /* Parse the FMI file name */
  
  if (strstr(fdpath,"FMI")) {
    strcpy(fmifile,fdpath);
    for (i=strlen(fdpath)-1;i>=0;i--){
      if (fdpath[i] == '/'){
	fdpath[i+1]='\0';
	i=0;
      }
    }
  } else {
    if (fdpath[strlen(fdpath)-1] != '/') {
      strcat(fdpath,"/");
    }
    strcpy(fmifile,fdpath);
    strcat(fmifile,"FMI");
  }
  /*printf("fmifile is %s, fdpath is %s\n",fmifile,fdpath);*/
  
  /* Open the files to be input. */
  
  fits_open_file(&ifp1, fmifile, READONLY, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not open FMI file");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  if ((fp=fopen(appidfile,"r")) == NULL){
    c_fcerr(" ");
    c_fcerr("Could not open AppId file");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Get time system information keywords from FMI primary header */
  
  fits_read_card(ifp1, "MJDREFI", mjdrefi, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading MJDREFI keyword from FMI");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_card(ifp1, "MJDREFF", mjdreff, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading MJDREFF keyword from FMI");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_get_hdrpos(ifp1, &dumintval, &currentkw, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading number of keywords in FMI primary");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_record(ifp1, currentkw, mjdcomm, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading MJD COMMENT keyword from FMI");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_card(ifp1, "TIMESYS", timesys, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading TIMESYS keyword from FMI");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_card(ifp1, "TIMEZERO",timezero,&pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading TIMEZERO keyword from FMI");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_card(ifp1, "TIMEUNIT",timeunit,&pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading TIMEUNIT keyword from FMI");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Move to the appropriate extension for the FMI file */
  
  fits_movabs_hdu(ifp1, 2, &xtend, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not move to proper extension in FMI file");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Get the information that describes the FMI file */
  
  ttype[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
  tform[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
  tunit[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
  fits_read_btblhdr(ifp1, MXELEM, &nrows, &nfield, ttype, tform, tunit, extnam, &pcount, &pstat); 
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get information file format for FMI file");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Locate the ObsID, StartMET & StopMET columns in the FMI */
  
  fits_get_colnum(ifp1, FALSE, "ObsID", &obsidcol, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding ObsID column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_get_colnum(ifp1, FALSE, "StartMET", &startmetcol, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding StartMET column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_get_colnum(ifp1, FALSE, "StopMET", &stopmetcol, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding StopMET column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Locate the Start/Stop Date/Time columns in the FMI */
  
  fits_get_colnum(ifp1, FALSE, "StartDate", &startdatecol, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding StartDate column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_get_colnum(ifp1, FALSE, "StartTime", &starttimecol, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding StartTime column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_get_colnum(ifp1, FALSE, "StopDate", &stopdatecol, &pstat); 
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding StopDate column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_get_colnum(ifp1, FALSE, "StopTime", &stoptimecol, &pstat); 
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding StopTime column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Locate the User & Source columns in the FMI */
  
  fits_get_colnum(ifp1, FALSE, "User", &usercol, &pstat); 
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding User column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_get_colnum(ifp1, FALSE, "Source", &sourcecol, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error finding Source column number");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Locate the 15 Index_File columns in the FMI */
  
  for (i=0;i<15;i++){
    fits_get_colnum(ifp1, FALSE, subsys[i].fmicnam, &subsys[i].fmicnum, &pstat);
    if(pstat != 0){
      strcpy(msg,"Error finding column number for ");
      strcat(msg,subsys[i].fmicnam);
      c_fcerr(" ");
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
  }
  
  /* Match the tabulated ObsIDs against the obsid parameter */
  
  obs[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
  for (i=1;i<=nrows;i++){
    fits_read_col_str(ifp1, obsidcol, i, 1, 1, "undef", obs, &anyf, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Error reading ObsID column");
      fits_report_error(stderr, pstat);
      exit(1);
    }
    if (strcmp(obs[0],obsid) == 0) {
      obsrow=i;
      break;
    }
  }
  if (obsrow == 0){
    c_fcerr(" ");
    c_fcerr("Could not locate requested ObsID");
    exit(1);
  }
  
  /* Get start and stop MET */
  
  fits_read_col_int(ifp1, startmetcol, obsrow, 1, 1, NULLJ, &startmet, &anyf, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading StartMET column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_col_int(ifp1, stopmetcol, obsrow, 1, 1, NULLJ, &stopmet, &anyf, &pstat); 
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading StopMET column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  if (DEBUGIT == TRUE){
    printf("StartMET: %d (%7x), StopMET: %d (%7x)\n",startmet,startmet,stopmet,stopmet);
  }
  
  /* Get Start/Stop Date/Time, User, Source values */

  startdate[0] = (char *) malloc(sizeof(char)*FLEN_VALUE);
  stopdate[0] = (char *) malloc(sizeof(char)*FLEN_VALUE);
  starttime[0] = (char *) malloc(sizeof(char)*FLEN_VALUE);
  stoptime[0] = (char *) malloc(sizeof(char)*FLEN_VALUE);
  user[0] = (char *) malloc(sizeof(char)*FLEN_VALUE);
  source[0] = (char *) malloc(sizeof(char)*FLEN_VALUE);

  fits_read_col_str(ifp1, startdatecol, obsrow, 1, 1, "undef", startdate, &anyf, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading StartDate column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_col_str(ifp1, stopdatecol, obsrow, 1, 1, "undef", stopdate, &anyf, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading StopDate column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_col_str(ifp1, starttimecol, obsrow, 1, 1, "undef" ,starttime, &anyf, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading StartTime column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_col_str(ifp1, stoptimecol, obsrow, 1, 1, "undef", stoptime, &anyf, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading StopTime column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_col_str(ifp1, usercol, obsrow, 1, 1, "undef", user, &anyf, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading User column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_read_col_str(ifp1, sourcecol, obsrow, 1, 1, "undef", source, &anyf, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading Source column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  sprintf(msg,"\nStartDate: %s, StopDate: %s",startdate[0],stopdate[0]);
  c_fcecho(msg);
  sprintf(msg,"StartTime: %s, StopTime: %s",starttime[0],stoptime[0]);
  c_fcecho(msg);
  sprintf(msg,"User: %s, Source: %s",user[0],source[0]);
  c_fcecho(msg);
  
  /* Construct paths to the subsystem Index Tables */
  
  for (i=0;i<15;i++){
    fits_read_col_str(ifp1, subsys[i].fmicnum, obsrow, 1, 1, "undef",
		      &subsys[i].path, &anyf, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      strcpy(msg,"Error reading Index File column for ");
      strcat(msg,subsys[i].fmicnam);
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
    strcpy(tmp,fdpath);
    strcat(tmp,subsys[i].path);
    strcpy(subsys[i].path,tmp); 
  }
  
  if (DEBUGIT == TRUE){
    for (i=0;i<15;i++){
      printf("subsys[%d].name = %s\n",i,subsys[i].name);
      printf("subsys[%d].path = %s\n",i,subsys[i].path);
      printf("subsys[%d].fmicnam = %s\n",i,subsys[i].fmicnam);
      printf("subsys[%d].fmicnum = %d\n",i,subsys[i].fmicnum);
    }
  }
  
  /* Construct path to ObsID directory */
  
  for (i=strlen(tmp)-1;i>=0;i--){
    if (tmp[i] == '/'){
      tmp[i+1]='\0';
      strcpy(obsidpath,tmp);
      i=0;
    }
  }
  
  /* Fill AppIDs structure array */
  
  /* 
     14Nov95: Skipping i=14 (stdprod) area below since stuff
     labelled AppId_NN lives in there and would override
     the earlier stuff 
     27Nov95: also skipping i=13 (cal) area since it's currently
     linked to /socops, which I can't see!!!
     (there should never be AppIds in there anyway)
     23Oct96: now we're using stdprod again since in real-time data
     we need the stuff in there. The trick is to reverse the sense
     of the loop so that if stuff *does* exist in original subsys
     directories it'll override the stdprod entry!
     */
  
  dumstr = (char *) malloc(sizeof(char)*FLEN_CARD);
  /*  for (i=0;i<13;i++){ */
  for (i=14;i>=0;i--){
    if (i == 13) continue;
    fits_open_file(&ssfp, subsys[i].path, READONLY, &pstat);
    if(pstat != 0){
      strcpy(msg,"Could not open ");
      strcat(msg,subsys[i].fmicnam);
      /*
	fits_report_error(stderr, pstat);
	exit(1);
      */
      strcat(msg," ...skipping");
      c_fcecho(msg);
      pstat=0;
      continue;
    }
    if (DEBUGIT){
      printf("AppID struct array loop: opened file %s\n",subsys[i].path);
    }
    if (pntinfo != 1){
      fits_read_card(ssfp, "RA_OBJ", racrd, &pstat);
      fits_read_card(ssfp, "DEC_OBJ", deccrd, &pstat);
      fits_read_card(ssfp, "EQUINOX", eqxcrd, &pstat);
      if(pstat == 0){
	pntinfo=1;
      }
      pstat=0;
    }
    fits_movabs_hdu(ssfp, 2, &xtend, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Could not move to BINTABLE extension in subsys file");
      fits_report_error(stderr, pstat);
      exit(1);
    }
    fits_read_key_lng(ssfp, "NAXIS2", &dumrows, dumcom, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      strcpy(msg,"Could not get number of rows in subsys file ");
      strcat(msg,subsys[i].path);
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
    /* First test is for any column named AppId* in the index file 
       Then, for PCA (i=0) and HEXTE (i=1) only, check for *Hk* as well
       Finally, for PCA only, check for EA*Data
       FITSIO status flags returned for FTGCNN:
               0 - unique match found
             237 - multiple matches found (recall w/this status to find next)
             219 - no matches found/left */
    while (pstat == 0 || pstat == 237){
      fits_get_colname(ssfp, FALSE, "AppId*", kwnam, &kwnum, &pstat);
      switch (pstat){
      case 0:
      case 237:
	j=1;
	while (j <= dumrows) {
	  fits_read_col_str(ssfp, kwnum, (long) j, 1, 1, "undef", 
			    &dumstr, &anyf, &dumstat);
	  if(dumstat != 0){
	    c_fcerr(" ");
	    strcpy(msg,"Error reading column in file ");
	    strcat(msg,subsys[i].path);
	    c_fcerr(msg);
	    fits_report_error(stderr, dumstat);
	    exit(1);
	  }
	  /* if (strlen(dumstr) != 0) { */
	  if (strlen(dumstr) > 1) { /* fits_read_col_str returns " " on empty entry! */
	    j=dumrows;
	  }
	  j++;
	}
	/* if ((strlen(dumstr) == 0) || (dumrows == 0)){ */
	if ((strlen(dumstr) <= 1) || (dumrows == 0)) { /* fits_read_col_str returns " " on empty entry! */
	  if (DEBUGIT){
	    c_fcecho(" ");
	    strcpy(msg,"Empty column found for AppId: ");
	    strcat(msg,kwnam);
	    strcat(msg," ...skipping");
	    c_fcecho(msg);
	  }
	  break;
	} 
	for (j=0;j<3;j++) {
	  ainum[j]=kwnam[j+6];
	}
	ainum[3]='\0';
	appidnum=atoi(ainum);
	appiddb[appidnum].used = 1;
	strcpy(appiddb[appidnum].name,kwnam);
	appiddb[appidnum].subsys = &subsys[i];
	appiddb[appidnum].colnum = kwnum;
        if (DEBUGIT){
          printf("AppId* pstat 237\n");
          printf("hit on appidnum %d kwnam %s subsys %s\n",appidnum,kwnam,subsys[i].name); 
        }
	break;
      case 219:
	pstat = -1;
	break;
      default:
	c_fcecho("ffgcnn didn't return 0, 219 or 237");
	fits_report_error(stderr, pstat);
	break;
      }
    }

    pstat=0;
    if (i == 0 || i == 1){ /* PCA or HEXTE subsystem */
      /* Not only do we have to check for *Hk* columns but we have to read
         the table entries for those columns to figure out the AppId number! */
      while (pstat == 0 || pstat == 237){
	fits_get_colname(ssfp, TRUE, "*Hk*", kwnam, &kwnum, &pstat);
        switch (pstat){
	  case 0:
	  case 237:
	    j=1;
	    while (j <= dumrows) {
	      fits_read_col_str(ssfp, kwnum, (long) j, 1, 1, "undef",
				&dumstr, &anyf, &dumstat);
	      if(dumstat != 0){
		c_fcerr(" ");
		strcpy(msg,"Error reading column in file ");
		strcat(msg,subsys[i].path);
		c_fcerr(msg);
		fits_report_error(stderr, dumstat);
		exit(1);
	      }
	      /* if (strlen(dumstr) != 0) { */
	      if (strlen(dumstr) > 1) { /* fits_read_col_str returns " " on empty entry! */
		j=dumrows;
	      }
	      j++;
	    }
	    /* if ((strlen(dumstr) == 0) || (dumrows == 0)){ */
	    if ((strlen(dumstr) <= 1) || (dumrows == 0)){ /* fits_read_col_str returns " " on empty entry! */
	      if (DEBUGIT){
		c_fcecho(" ");
		strcpy(msg,"Empty column found for AppId: ");
		strcat(msg,kwnam);
		strcat(msg," ...skipping");
		c_fcecho(msg);
	      }
	      break;
	    } 
            if (DEBUGIT){
              printf("*Hk* pstat 237 read column value %s for kwnam %s\n",dumstr,kwnam);
            }
	    /* parse filename */
	    n=sscanf(dumstr,"%*[^/] /FH%[^_]",tmp);
	    if (n > 0) {
	      /* hextoi() !! */
	      appidnum=strtol(tmp, (char**)NULL, 16);
	    } else {
	      c_fcerr(" ");
	      strcpy(msg,"Error parsing AppId #");
	      c_fcerr(msg);
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	    appiddb[appidnum].used = 1;
	    strcpy(appiddb[appidnum].name,kwnam);
	    appiddb[appidnum].subsys = &subsys[i];
	    appiddb[appidnum].colnum = kwnum;
            if (DEBUGIT){
              printf("*Hk* pstat 237\n");
              printf("hit on appidnum %d kwnam %s subsys %s\n",appidnum,kwnam,subsys[i].name); 
            }
	    break;
	  case 219:
	    pstat = -1;
	    break;
	  default:
	    c_fcerr("ffgcnn didn't return 0, 219 or 237");
	    fits_report_error(stderr, pstat);
	    break;
        } /* end switch */
      } /* end while */
    } /* end if (PCA/HEXTE - *Hk*) */

    pstat=0;
    if (i == 0){ /* PCA subsystem only */
      /*We now have to check for EA*Data columns and read
         the table entries for those columns to figure out the AppId number! */
      while (pstat == 0 || pstat == 237){
	fits_get_colname(ssfp, TRUE, "EA*Data", kwnam, &kwnum, &pstat);
        switch (pstat){
	  case 0:
	  case 237:
	    j=1;
	    while (j <= dumrows) {
	      fits_read_col_str(ssfp, kwnum, (long) j, 1, 1, "undef",
				&dumstr, &anyf, &dumstat);
	      if(dumstat != 0){
		c_fcerr(" ");
		strcpy(msg,"Error reading column in file ");
		strcat(msg,subsys[i].path);
		c_fcerr(msg);
		fits_report_error(stderr, dumstat);
		exit(1);
	      }
	      /* if (strlen(dumstr) != 0) { */
	      if (strlen(dumstr) > 1) {/* fits_read_col_str returns " " on empty entry! */
		j=dumrows;
	      }
	      j++;
	    }
	    /* if ((strlen(dumstr) == 0) || (dumrows == 0)){ */
	    if ((strlen(dumstr) <= 1) || (dumrows == 0)){/* fits_read_col_str returns " " on empty entry! */
	      if (DEBUGIT){
		c_fcecho(" ");
		strcpy(msg,"Empty column found for AppId: ");
		strcat(msg,kwnam);
		strcat(msg," ...skipping");
		c_fcecho(msg);
	      }
	      break;
	    } 
            if (DEBUGIT){
              printf("*Hk* pstat 237 read column value %s for kwnam %s\n",dumstr,kwnam);
            }
	    /* parse filename */
	    n=sscanf(dumstr,"%*[^/] /FS%[^_]",tmp);
	    if (n > 0) {
	      /* hextoi() !! */
	      appidnum=strtol(tmp, (char**)NULL, 16);
	    } else {
	      c_fcerr(" ");
	      strcpy(msg,"Error parsing AppId #");
	      c_fcerr(msg);
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	    appiddb[appidnum].used = 1;
	    strcpy(appiddb[appidnum].name,kwnam);
	    appiddb[appidnum].subsys = &subsys[i];
	    appiddb[appidnum].colnum = kwnum;
            if (DEBUGIT){
              printf("EA*Data pstat 237\n");
              printf("hit on appidnum %d kwnam %s subsys %s\n",appidnum,kwnam,subsys[i].name); 
            }
	    break;
	  case 219:
	    pstat = -1;
	    break;
	  default:
	    c_fcerr("ffgcnn didn't return 0, 219 or 237");
	    fits_report_error(stderr, pstat);
	    break;
        } /* end switch */
      } /* end while */
    } /* end if (PCA - EA*Data) */

    pstat=0;
    fits_clear_errmsg(); /* clear FITSIO error stack of FCGCNN-related messages */

    fits_close_file(ssfp, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      strcpy(msg,"Could not close file ");
      strcat(msg,subsys[i].path);
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
  } /* end for (subsytems) */

  
  if (DEBUGIT == TRUE || LISTAPPIDS == TRUE){
    for(i=0;i<MAX_APPIDS;i++){
      if (appiddb[i].used != 0){
        printf("AppID %d name: %s subsys: %s column #: %d\n", i,
	       appiddb[i].name, appiddb[i].subsys->name, appiddb[i].colnum);
      }
    }
  }
  if (LISTAPPIDS == TRUE) {
    exit(0);
  }
  
  /* Create an output file with the appropriate output file name.
     If the clobber parameter is set, then use !filename convention.
     
     07Nov: using new parameter..if it's XFD then do as before, otherwise
     take the parameter to be the full pathname to the output file */
  
  if (strcmp("XFD",outfl) == 0){
    sprintf(outfl,"FP_%07x-%07x",startmet,stopmet);
  }
  if (clobber){
    sprintf(outfile,"!%s",outfl);
  } else {
    strcpy(outfile,outfl);
  }
  fits_create_file(&ofp, outfile, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    strcpy(msg,"ERROR: Could not create output file: ");
    strcat(msg,outfile);
    c_fcerr(msg);
    c_fcerr("Check write permission, and/or remove");
    c_fcerr("any existing file with the same name");
    c_fcerr("or enable the clobber parameter.");
    c_fcerr(" ");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  Uclpst("fname", outfl, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write filename into par file");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  simple=1, bitpix=8, naxis=0, pcount=0, gcount=1, extend=1;
  naxes = (long *) malloc(sizeof(long));
  *naxes = 1;
    
  /* Create observation time axis */
  
  nstep = ((stopmet-startmet) / deltat) + 1;
  time = (double *) malloc(nstep * sizeof(double));
  tmpcol = (double *) malloc(nstep * sizeof(double));
  for (i=0;i<nstep;i++) {
    *(time+i) = startmet + i * deltat;
  }
  if (DEBUGIT1 == TRUE) {
    for (i=0;i<nstep;i++) {
      printf("i: %d, *(time+i): %f\n",i,*(time+i));
    }
  }
  sprintf(msg,"\ntime element 1: %f, %ld: %f",*time,nstep,*(time+nstep-1));
  c_fcecho(msg);
  
  /* Initialize the FITS file */
  
  fits_write_grphdr(ofp, simple, bitpix, naxis, naxes, pcount, gcount, extend, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not setup the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Write informational keywords into primary header */
  
  strcpy(dumkw,"OBSERVER");
  strcpy(dumcom,"Observation info block--------");
  strcpy(dumval,user[0]);
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write OBSERVER into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  strcpy(dumkw,"OBJECT");
  strcpy(dumcom," ");
  strcpy(dumval,source[0]);
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write OBJECT into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  strcpy(dumkw,"OBS_ID");
  strcpy(dumcom,"<proposal>-<target>-<viewing>-<seq no><type>");
  strcpy(dumval,obs[0]);
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write OBS_ID into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  strcpy(dumkw,"DATE-OBS");
  strcpy(dumcom,"Start date for data");
  fits_str2date(startdate[0], &yy, &mm, &dd, &pstat);
  /*printf("str2date returned yy=%d mm=%d dd=%d pstat=%d\n",yy,mm,dd,pstat);*/
  if (yy < 1950) {
    yy += 100; /* FMI date columns still use old-style format */
    pstat = 0; /* 26July2005 - must reset from str2date */
  }
  /*printf("now yy=%d mm=%d dd=%d\n",yy,mm,dd);*/
  fits_time2str(yy, mm, dd, 0, 0, 0.0, -1, startdate[0], &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error converting start date read from FMI column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  strcpy(dumval,startdate[0]);
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write DATE-OBS into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  /* Save for later CALDB query */
  strcpy(date_obs, dumval);
  
  strcpy(dumkw,"TIME-OBS");
  strcpy(dumcom,"Start time for data");
  strcpy(dumval,starttime[0]);
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIME-OBS into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  /* Save for later CALDB query */
  strcpy(time_obs, dumval);
  
  strcpy(dumkw,"DATE-END");
  strcpy(dumcom,"End date for data");
  fits_str2date(stopdate[0], &yy, &mm, &dd, &pstat);
  if (yy < 1950) {
    yy += 100; /* FMI date columns still use old-style format */
    pstat = 0; /* 26July2005 - must reset from str2date */
  }
  fits_time2str(yy, mm, dd, 0, 0, 0.0, -1, stopdate[0], &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error converting stop date read from FMI column");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  strcpy(dumval,stopdate[0]);
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write DATE-END into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  /* Save for later CALDB query */
  strcpy(date_end, dumval);
  
  strcpy(dumkw,"TIME-END");
  strcpy(dumcom,"End time for data");
  strcpy(dumval,stoptime[0]);
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIME-END into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  /* Save for later CALDB query */
  strcpy(time_end, dumval);
  
  strcpy(dumkw,"TSTART");
  strcpy(dumcom,"As in the \"Time\" column");
  /* FCPKYD(olun,dumkw,(double) startmet,15,dumcom,&pstat); */
  fits_write_key_dbl(ofp, dumkw, *time, 15, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TSTART into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  strcpy(dumkw,"TSTOP");
  strcpy(dumcom,"As in the \"Time\" column");
  /* FCPKYD(olun,dumkw,(double) stopmet,15,dumcom,&pstat); */
  fits_write_key_dbl(ofp, dumkw, *(time+nstep-1), 15, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TSTOP into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_write_record(ofp, mjdrefi, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write MJDREF into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, mjdreff, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write MJDREFF into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, mjdcomm, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write MJDCOMMENT into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, timesys, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMESYS into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, timezero, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEZERO into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, timeunit, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEUNIT into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_key_dbl(ofp, "TIMEDEL", deltat, 15, "Filter file timestep", &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEDEL into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_key_lng(ofp, "TIMEPIXR", 0, "Time stamps refer to the start of each pixel", &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEPIXR into the primary array header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  

  /* I'm assuming that if we got one, we got all three */ 
  if (pntinfo == 1){
    fits_write_record(ofp, racrd, &pstat);
    fits_write_record(ofp, deccrd, &pstat);
    fits_write_record(ofp, eqxcrd, &pstat);
    pstat=0;
  }
  
  /* Create a new FITS header for binary table */
  fits_create_hdu(ofp, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not create new HDU");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Write the keywords for the binary table (only 1 column {Time} for now) */
  ttype1[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
  ttype1[0] = "Time";
  tform1[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
  tform1[0] = "D";
  tunit1[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
  tunit1[0] = "s";
  fits_write_btblhdr(ofp, nstep, 1, ttype1, tform1, tunit1, extnam1, 0, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not place binary table header keywords into CHU");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  strcpy(dumkw,"TSTART");
  strcpy(dumcom,"As in the \"Time\" column");
  /* FCPKYD(olun,dumkw,(double) startmet,15,dumcom,&pstat); */
  fits_write_key_dbl(ofp, dumkw, *time, 15, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TSTART into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  strcpy(dumkw,"TSTOP");
  strcpy(dumcom,"As in the \"Time\" column");
  /* FCPKYD(olun,dumkw,(double) stopmet,15,dumcom,&pstat); */
  fits_write_key_dbl(ofp, dumkw, *(time+nstep-1), 15, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TSTOP into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, mjdrefi, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write MJDREFI into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, mjdreff, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write MJDREFF into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, mjdcomm, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write MJDCOMMENT into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, timesys, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMESYS into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, timezero, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEZERO into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_record(ofp, timeunit, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEUNIT into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_key_dbl(ofp, "TIMEDEL", deltat, 15, "Filter file timestep", &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEDEL into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  fits_write_key_lng(ofp, "TIMEPIXR", 0, "Time stamps refer to the start of each pixel", &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TIMEPIXR into the bintable header");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Add TDDES1 keyword and modify comment in TTYPE1 */
  
  strcpy(dumkw,"TDDES1");
  strcpy(dumval,"TIME");
  strcpy(dumcom," ");
  fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not write TDDES1 keyword");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  strcpy(dumkw,"TTYPE1");
  strcpy(dumstr,"Packet data time stamp (sec since 1 Jan 1994)");
  fits_modify_comment(ofp, dumkw, dumstr, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error modifying TTYPE1 comment");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Insert the time column into the CDU */
  
  fits_write_col_dbl(ofp, 1, 1, 1, nstep, time, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not put binary table elements into CHU");
    fits_report_error(stderr, pstat);
    exit(1);
  }

  /* Check for PCA breakdown file */
  if (breakfile[0]) {
    if (strcasecmp(breakfile,"CALDB") == 0) {
      char *telescop = "XTE", *instrume = "PCA", *detnam = "-", *filter = "-";
      char *expr = "-";
      char *codenam = "PCA_BREAKDOWN_HIST";
      char *pfile[1];
      char online[80], *ponline[1];
      char filenam[MAXC_FNAME];
      long int extno[1];
      int maxret = 1;
      int nret = 0, nfound = 0;
      int status = 0;

      pfile[0] = filenam;
      ponline[0] = online;

      HDgtcalf(telescop, instrume, detnam, filter, 
	       codenam, date_obs, time_obs, date_end, time_end, expr,
	       maxret, MAXC_FNAME, 
	       pfile, extno, ponline, &nret, &nfound, &status);
      
      if ((status != 0) || (nret == 0) || (nfound == 0)) {
	c_fcerr(" ");
	c_fcerr("ERROR: could not find PCA_BREAKDOWN_HIST in CALDB");
	sprintf(msg, "   status=%d nret=%d nfound=%d", status, nret, nfound);
	c_fcerr(msg);
	fits_report_error(stderr, status);
	exit(1);
      }

      /* NOTE: tried to append extension to filename as filename[%ld],
	 but this resulted in failure during the ffopen stage.  Since
	 this file should always have the data in the first extension,
	 and since fcollect hard-codes for HDU1 anyway, I discard the
	 extension. */
      sprintf(breakfile, "%s", filenam);
    }

    appiddb[BREAKDOWN_APPID].used = 1;
    strcpy(appiddb[BREAKDOWN_APPID].name, breakfile);
    appiddb[BREAKDOWN_APPID].subsys = subsys_global;
    appiddb[BREAKDOWN_APPID].colnum = -1;
    
  }
    


  
  /* Now do the nitty-gritty */
  
  while(iseof == 0){
    validap=0;
    usenewanam=0;
    tformwasi=0;
    strcpy(anam,"/0");
    strcpy(newanam,"/0");
    if (fgets(apline, 81, fp) == NULL) {
      iseof=1;
    }
    if (iseof == 0){
      if (sscanf(apline,"%d %s %s",&anum,anam,newanam) == 3){
	validap=1;
	usenewanam=1;
      } else {
	if (sscanf(apline,"%d %s",&anum,anam) == 2){
	  validap=1;
	} else {
	  sprintf(msg,"\nINVALID FORMAT IN APPID LIST: %s ... skipping",apline);
	  c_fcecho(msg);
	}
      }
    }
    iseof=feof(fp);
    if (iseof == 0 && validap == 1){
      colno++;
      ad.flag=0;
      if (usenewanam == 1) {
	sprintf(msg,"\nread from AppId list: %d %s %s",anum,anam,newanam);
      } else {
	sprintf(msg,"\nread from AppId list: %d %s",anum,anam);
      }
      c_fcecho(msg);
           
      if (anum < 0 || anum >= MAX_APPIDS || appiddb[anum].used == 0) { 
	c_fcecho("Invalid AppId -- no data or badly formed number/name");
	colno--;
	continue;
      } 
      
      if (appiddb[anum].subsys == subsys_global) {
	/* This is a "global" APID (i.e. a breakdown file) */
	pstat=Fetch_AppId(anum, appiddb[anum].name, appiddb[anum].colnum,
			  anam, obsidpath, &ad);
      } else {
	/* This is a "real" file from the observation indices */
	pstat=Fetch_AppId(anum, appiddb[anum].subsys->path, appiddb[anum].colnum,
			  anam, obsidpath, &ad);
      }

      if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error in Fetch_AppId");
	exit(1);
      }
      if (ad.maxgap <= 0) ad.maxgap = maxgap;

      if (ad.flag == 0){
	sprintf(msg,"Packet Data Time Stamp 1: %f, %d: %f",
		*ad.timedat, ad.nrdat, *(ad.timedat+ad.nrdat-1));
	c_fcecho(msg);
	sprintf(msg,"AppId Data 1: %f, %d: %f",
		*ad.appdat, ad.nrdat, *(ad.appdat+ad.nrdat-1));
	c_fcecho(msg);
	
	/** Need to put Longitude on a monotonically increasing scale
            before interpolating and then revert to 0<=lon<=360 after **/
	if (!strcmp(anam,"ACSEARTHLON")){
	  longrp=0;
	  for (i=1;i<ad.nrdat;i++){
	    if (*(ad.appdat+i) - (*(ad.appdat+i-1)-longrp*360.0) < 0) longrp++;
	    *(ad.appdat+i) = *(ad.appdat+i) + longrp*360.0;
	  }
	}

	/**   Perform the interpolation of appid data onto time axis... **/
	
	if (!strcmp(anam,"cmdhvXE")){ 
	  /* cmdhvXE is a discrete quantity; don't interpolate */
	  discret(ad.timedat,ad.appdat,ad.nrdat,time,tmpcol,nstep,ad.maxgap);
	}else{
	  lintrp2(ad.timedat,ad.appdat,ad.nrdat,time,tmpcol,nstep,ad.maxgap);
	}
        free(ad.timedat);
        free(ad.appdat);

	/** Put Longitude back to normal scale **/
	if (!strcmp(anam,"ACSEARTHLON")){
	  for (i=0;i<nstep;i++){
	    if (*(tmpcol+i) != NULLVAL){
	      dumintval = (int) *(tmpcol+i)/360.0;
	      *(tmpcol+i) = *(tmpcol+i) - dumintval*360;
	    }
	  }
	}
	
	/**   Write the new column into the output FITS file **/
	
	/* Parse header cards for ttype, tform strings  */
	/* (seems to be easier than using FCPSVC/FCDTYP */
	
	if (usenewanam == 1) {
	  strcpy(ttypex, newanam);
	} else {
	  /* 29Apr98: falls down when colnum >= 100!)
	    sscanf(ad.hdkw,"%*s %*s \'%[^']",ttypex); */
	  sscanf(ad.hdkw,"%*[^'] \'%[^']",ttypex);
	}

        /* 30Apr98: same bug, different time (sigh) 
	sscanf((ad.hdkw+(ad.numhdkw-1)*FITS_CLEN_CARD),"%*s %*s \'%s[^']",tformx); */
	sscanf((ad.hdkw+(ad.numhdkw-1)*FLEN_CARD),"%*[^'] \'%[^']",tformx);
	/* Note that now '1D' columns are allowed; remove the "1". */
	if (tformx[0] == '1') {
	  /* Stupid strcpy can't handle overlappings strings */
	  /* Tripped up on 32-bit heasoft 6.15.1 Mac OS */
	  char tformtemp[FITS_CLEN_HDEFKWDS] = "";
	  strcpy(tformtemp,tformx+1);
	  strcpy(tformx,tformtemp);
	}

	/* Changing TFORM="I" to "E" since fmerge often fails with TZERO/TSCAL data */
	if (tformx[0] == 'I' ) {
	  /* XXX Should this include tformx[0] == 'J' ?? */
	  /* XXX However, if we do, then it will cause hurtful
	     problems when trying to merge old filter files that were
	     'J' with new files that are transformed to 'E' 
	        C. Markwardt 26 Apr 2008 */
	  strcpy(tformx,"E");
	  tformwasi=1;
	}
	
	/* Reserve space and write required keywords */
	
	fits_insert_col(ofp, colno, ttypex, tformx, &pstat);
	if(pstat != 0){
	  c_fcerr(" ");
	  c_fcerr("Could not initialize new column");
	  fits_report_error(stderr, pstat);
	  exit(1);
	}
	
	/* Edit the ttype keyword to put the original comment back in  */
	/* (we're assuming that a comment will never contain a double-quote) */
	
	n=sscanf(ad.hdkw,"%*[^/] %*[/] %[^\"]",dumstr); 
	fits_make_keyn("TTYPE", colno, dumkw, &pstat);
	if(pstat != 0){
	  c_fcerr(" ");
	  c_fcerr("Could not reform TTYPE keyword");
	  fits_report_error(stderr, pstat);
	  exit(1);
	}
	if (n > 0) {
	  fits_modify_comment(ofp, dumkw, dumstr, &pstat);
	  if(pstat != 0){
	    c_fcerr(" ");
	    c_fcerr("Error modifying TTYPE comment");
	    fits_report_error(stderr, pstat);
	    exit(1);
	  }
	}
		
	/* Write remaining keywords {skipping first (TTYPE) and last (TFORM)} */
	for (i=2;i<=ad.numhdkw-1;i++) {
	  strcpy(tmp,(ad.hdkw+(i-1)*FLEN_CARD));
	  /* n=sscanf(tmp,"%*s %[=] %*s",dumstr); */
	  n=sscanf(tmp,"%*[^=] %[=] %*s",dumstr);
	  if (n > 0) {         /* found an equal-sign -> reform KW */
	    /* sscanf(tmp,"%[^0-9] %*[=]",dumstr); */
	    sscanf(tmp,"%[^0-9]",dumstr);
	    fits_make_keyn(dumstr, colno, dumkw, &pstat);
	    if(pstat != 0){
	      c_fcerr(" "); 
	      c_fcerr("Could not reform keyword");
	      c_fcerr(dumstr);
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	    /* n=sscanf(tmp,"%*s %*s \'%[^']",dumval); */
	    n=sscanf(tmp,"%*[^=] %*[=] \'%[^']",dumval);
	    if (n == 0){   /* no single-quote; parse value numerically, eg, TZEROn */
	      /* n=sscanf(tmp,"%*s %*s %s",dumval); */
	      n=sscanf(tmp,"%*[^=] %*[=] %s",dumval);
	      dumfltval=atof(dumval);
	      n=sscanf(tmp,"%*[^/] %*[/] %[^\"]",dumstr); 
	      if (n > 0) {
		strcpy(dumcom,dumstr);
	      } else {
		strcpy(dumcom," "); 
	      }
	      /* don't write TZEROn or TSCALn keywords if "I" has been changed to "E" */
	      if (!(tformwasi && (!strncmp(dumkw,"TZERO",5) || 
				  !strncmp(dumkw,"TSCAL",5)))) { 
		fits_write_key_dbl(ofp, dumkw, dumfltval, 15, dumcom, &pstat);
		if(pstat != 0){
		  c_fcerr(" ");
		  c_fcerr("Could not write double-precision keyword");
		  fits_report_error(stderr, pstat);
		  exit(1);
		}
	      }
	    } else { /* single-quote found -> parse value as string */
	      n=sscanf(tmp,"%*[^/] %*[/] %[^\"]",dumstr); 
	      if (n > 0) {
		strcpy(dumcom,dumstr);
	      } else {
		strcpy(dumcom," "); 
	      }
	      /* 11Mar99 - suppress TDISP kwd when TFORM "I" has been changed to "E" */
	      if (!(tformwasi && !strncmp(dumkw,"TDISP",5))) {
		fits_write_key_str(ofp, dumkw, dumval, dumcom, &pstat);
		if(pstat != 0){
		  c_fcerr(" ");
		  c_fcerr("Could not write string-valued keyword");
		  fits_report_error(stderr, pstat);
		  exit(1);
		}
	      }
	    }
	  } else {  /* no equal-sign found; lob in whole 80-char "card", eg, COMMENT */
	    fits_write_record(ofp, (ad.hdkw+(i-1)*FLEN_CARD), &pstat);
	    if(pstat != 0){
	      c_fcerr(" ");
	      c_fcerr("Could not write 80-char record");
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	  } /* end else */
	} /* end for (i=2;i<=ad.numhdkw-1;i++) */
        free(ad.hdkw);
      } /* end if (ad.flag == 0) */

      /* Write INDEF column (valid AppId w/o data) */
      if (ad.flag == 1){
	if (usenewanam == 1) {
	  strcpy(ttypex,newanam);
	} else {
	  strcpy(ttypex,anam);
	}
	strcpy(tformx,"E");
	fits_insert_col(ofp, colno, ttypex, tformx, &pstat);
	if(pstat != 0){
	  c_fcerr(" ");
	  c_fcerr("Could not initialize dummy column");
	  fits_report_error(stderr, pstat);
	  exit(1);
	}
	fits_write_comment(ofp, "Preceeding column INDEF - no AppId data found",&pstat);
	if(pstat != 0){
	  c_fcerr(" ");
	  c_fcerr("Could not write comment for dummy column");
	  fits_report_error(stderr, pstat);
	  exit(1);
	}
	for (i=0;i<nstep;i++){
	  appid_intrp=NULLVAL;
	  *(tmpcol+i) = appid_intrp;
	}
      } /* end if (ad.flag == 1) */
      
      /*  Insert the column data  */
      
      if (ad.flag == 0 || ad.flag == 1){
	if (strncmp(tformx,"B",1) == 0 || 
            strncmp(tformx,"I",1) == 0 ||
            strncmp(tformx,"J",1) == 0){
	  fits_make_keyn("TNULL", colno, dumkw, &pstat);
	  if(pstat != 0){
	    c_fcerr(" ");
	    c_fcerr("Could not reform TNULL keyword");
	    fits_report_error(stderr, pstat);
	    exit(1);
	  }
	  if (strncmp(tformx,"B",1) == 0){
	    fits_set_btblnull(ofp, colno, NULLB, &pstat);
	    if(pstat != 0){
	      c_fcerr(" ");
	      c_fcerr("Error setting value for undefined integer entries");
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	    strcpy(dumcom,"NULL value for B-type columns");
	    fits_write_key_lng(ofp, dumkw, NULLB, dumcom, &pstat);
	    if(pstat != 0){
	      c_fcerr(" ");
	      c_fcerr("Could not write TNULL keyword");
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	  }
	  if (strncmp(tformx,"I",1) == 0){
	    fits_set_btblnull(ofp, colno, NULLI, &pstat);
	    if(pstat != 0){
	      c_fcerr(" ");
	      c_fcerr("Error setting value for undefined integer entries");
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	    strcpy(dumcom,"NULL value for I-type columns");
	    fits_write_key_lng(ofp, dumkw, NULLI, dumcom, &pstat);
	    if(pstat != 0){
	      c_fcerr(" ");
	      c_fcerr("Could not write TNULL keyword");
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	  }
	  if (strncmp(tformx,"J",1) == 0){
	    fits_set_btblnull(ofp, colno, NULLJ, &pstat);
	    if(pstat != 0){
	      c_fcerr(" ");
	      c_fcerr("Error setting value for undefined integer entries");
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	    strcpy(dumcom,"NULL value for J-type columns");
	    fits_write_key_lng(ofp, dumkw, NULLJ, dumcom, &pstat);
	    if(pstat != 0){
	      c_fcerr(" ");
	      c_fcerr("Could not write TNULL keyword");
	      fits_report_error(stderr, pstat);
	      exit(1);
	    }
	  }
	} /* end if (B,I or J) */
	fits_set_hdustruc(ofp, &pstat);
	if(pstat != 0){
	  c_fcerr(" ");
	  c_fcerr("Error redefining CHU");
	  fits_report_error(stderr, pstat);
	  exit(1);
	}
	fits_write_colnull_dbl(ofp, colno, 1, 1, nstep, tmpcol, NULLVAL, &pstat);
	if(pstat != 0){
	  int safe_status = 0;  /* XXX: craig debug */
	  fits_close_file(ofp, &safe_status);
	  c_fcerr(" ");
	  c_fcerr("Error inserting new column data");
	  fits_report_error(stderr, pstat);
	  exit(1);
	}
      } else { /* ad.flag not 0 or 1, don't write anything */
	colno--;
      }
    } /* end if (iseof == 0 && validap == 1) */
  } /* end while (iseof == 0) */

  free(time);
  free(tmpcol);
  
  /* Finally, write some additional useful header keywords */
  
  strcpy(dumstr,"File created using ");
  strcat(dumstr,taskname);
  fits_write_history(ofp, dumstr, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error writing HISTORY keyword");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_write_date(ofp, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Error writing DATE keyword");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  /* Close all of the files */
  
  fits_close_file(ifp1, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not close input FMI file");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  n=fclose(fp);
  if(n != 0){
    c_fcerr(" ");
    c_fcerr("Could not close input AppID list");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  fits_close_file(ofp, &pstat);
  if(pstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not close output FITS file");
    fits_report_error(stderr, pstat);
    exit(1);
  }
  
  c_fcecho("=====================");
  c_fcecho("FCOLLECT finished");
}

int readpar(obsid,fdpath,appidfile,outfile,deltat,maxgap,clobber,breakfile) 
char *obsid, *fdpath, *appidfile, *outfile;
double *deltat, *maxgap;
char *breakfile;
int *clobber;
{
  int BufLen_2 = 255;
  int parstat;
  char text[FLEN_STATUS];
  parstat = *deltat = *maxgap = *clobber = 0;
  
  Uclgst("obsid", obsid, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get Observation Code.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("fdpath", fdpath, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get FD pathname.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("appidfile",appidfile, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get ASCII file name.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("outfile",outfile, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get output file name.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgsd("deltat",deltat, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get DeltaT.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgsd("maxgap",maxgap, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get maxgap.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgsb("clobber",clobber, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get clobber.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgst("breakfile",breakfile, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Could not get breakfile.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  /* Handle case of no input */
  if (strcmp(breakfile,"NONE") == 0) { 
    breakfile[0] = 0;
  }

  return parstat;
}

int Fetch_AppId(anum, ssidxpath,colnum,appidnam,datdirpath,adstruc)
char *ssidxpath, *datdirpath, *appidnam;
int colnum, anum; /* anum now being passed for checking purposes (see below) */
struct appdata *adstruc;
{
  char *hack_fnam();

  int pstat, block,anyf, xtend, i, acol;
  int numkw, currentkw, kwnum1=0, kwnum2=0;
  int j, numdatfils, opened_ok, nrdat, rowsread;
  int appidnum, n; /* appidnum is computed from the filename and compared to anum */

  long nrowidx, dumrows;
  
  char msg[ERRMSG], datfile[MAXC_FNAME]; 
  char comm[FITS_CLEN_HDEFKWDS];
  char tmpstr[FLEN_CARD], dumstr[FLEN_CARD];
  char typstr[FLEN_KEYWORD], formstr[FLEN_KEYWORD];
  char filelist[20][MAXC_FNAME];
  char dumcom[FLEN_COMMENT];
  char *cards, *datapath;
  char tmp[3];

  fitsfile *idxfp, *datfp;
  
  pstat=block=nrowidx=anyf=acol=0;
  numkw=currentkw=opened_ok=0;

  adstruc->nrdat=0;
  adstruc->maxgap=0;

  /* Gotta initialize this for HP-UX, IRIX */
  datapath = (char *) malloc(sizeof(char)*MAXC_FNAME);
  strcpy(datapath,"\0");
  strcpy(comm,"\0");

  if (colnum >= 0) {
    /* ====================== "Real" file (not a global file) */
    fits_open_file(&idxfp, ssidxpath, READONLY, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      strcpy(msg,"Could not open file ");
      strcat(msg,ssidxpath);
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
    if (DEBUGIT){
      printf("opened file %s\n",ssidxpath);
    }
    
    fits_movabs_hdu(idxfp, 2, &xtend, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Could not move to proper extension in ssidx file");
      fits_report_error(stderr, pstat);
      exit(1);
    }
    
    fits_read_key_lng(idxfp, "NAXIS2", &nrowidx, comm, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      strcpy(msg,"Could not get number of rows in file ");
      strcat(msg,ssidxpath);
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
    
    /* Read down the subdivided observation period and record 
       the first encountered path to the data */
    /* 21Jun99: this is getting ugly. I've added a check to 
       ensure that the files found in the specified column
       really belong to the appid in question. This would
       never come up in normal housekeeping files but since
       the science data has EA?Data for column names there
       can be files from multiple appids in the same column.
       If one hasn't copied all of the files (ie, Std2 only)
       then the lack of a listed file causes an error */
    i=1;
    j=0; /* counter for number of unique data files */
    n=0;
    while (i <= nrowidx) {
      fits_read_col_str(idxfp, colnum, (long) i, 1, 1, "undef", &datapath, &anyf, &pstat);
      if(pstat != 0){
	c_fcerr(" ");
	strcpy(msg,"Error reading column in ");
	strcat(msg,ssidxpath);
	c_fcerr(msg);
	fits_report_error(stderr, pstat);
	exit(1);
      }
      /* found an entry */
      /* if (strlen(datapath) != 0) { */
      if (strlen(datapath) > 1) { /* new CFITSIO behavior */
	/* parse filename */
	appidnum=0;
	n=sscanf(datapath,"%*[^/] /FH%[^_]",tmp);
	if (n == 0) n=sscanf(datapath,"%*[^/] /FS%[^_]",tmp);
	if (n > 0) appidnum=strtol(tmp, (char**)NULL, 16); /* hextoi again! */
	if ((j == 0) && (anum == appidnum)) {
	  strcpy(filelist[0],datapath);
	  j++;
	} else {
	  /* is it the same as the last one? */
	  if ((anum == appidnum) && (strcmp(filelist[j-1],datapath))) {
	    strcpy(filelist[j],datapath);
	    j++;
	  }
	}
      }
      i++;
    }
    numdatfils=j;
    /* printf("Found %d unique datafiles in %s\n",numdatfils,ssidxpath);
       for (i=0; i<numdatfils; i++) {
       printf("%s\n",filelist[i]);
       }
    */
    
    /* Close idx file */
    fits_close_file(idxfp, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      strcpy(msg,"Could not close file ");
      strcat(msg,ssidxpath);
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
    
  } else {  /* colnum == -1 */
    /* ====================== Global file */
    /* The "path" is the actual name of the file */
    strcpy(filelist[0], ssidxpath);
    numdatfils = 1;
  }
  
  /* Column exists but is empty */
  if (numdatfils == 0){
    c_fcecho("No data indexed for this AppID -- column will be INDEF");
    /* flag = 1 for this condition */
    adstruc->flag=1;
    return pstat;
  }

  rowsread=0;
  adstruc->timedat = (double *) 0;  /* Zero the array pointers at start */
  adstruc->appdat = (double *) 0;

  for (j=0;j<numdatfils;j++) {
    strcpy(datapath,filelist[j]);
    datfile[0] = 0;
    if (colnum >= 0) strcpy(datfile,datdirpath);
    strcat(datfile,datapath);
  
  /* Open the current data file */
    fits_open_file(&datfp, datfile, READONLY, &pstat);
    if(pstat != 0){
      /* Datafile didn't exist, despite indexfile */
      strcpy(msg,"Could not open ");
      strcat(msg,datfile);
      c_fcecho(msg);
      c_fcecho("though it was listed in the index file");
      fits_report_error(stderr, pstat);
      pstat=0;
      /* set flag to 1 and return only if zero data files were opened */
      if (j==numdatfils-1 && !opened_ok) {
	adstruc->flag=1;
	c_fcecho("No data found for this AppId - entire column will be INDEF");
	return pstat;
      } else {
	continue; /* go on to the next file */
      }
    } else {
      opened_ok++;
    }
  
    fits_movabs_hdu(datfp, 2, &xtend, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Error moving to extension No. 1 in data file");
      fits_report_error(stderr, pstat);
      exit(1);
    }
  
    /* Number of keywords in the CHU? Where am I? */
    fits_get_hdrpos(datfp, &numkw, &currentkw, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Error reading header position");
      fits_report_error(stderr, pstat);
      exit(1);
    }
    cards = (char *) malloc(numkw * sizeof(char) * FLEN_CARD);
    for (i=1;i<=numkw;i++) {
      fits_read_record(datfp, i, (cards+(i-1)*FLEN_CARD), &pstat);
      if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading header record");
	fits_report_error(stderr, pstat);
	exit(1);
      }
    }
  
    /* How many rows of appid data? */
    i=1;
    while (i<=numkw) {
      if (strncmp("NAXIS2",(cards+(i-1)*FLEN_CARD),6) == 0 ){
	strncpy(tmpstr,(cards+(i-1)*FLEN_CARD),FLEN_CARD);
	/* sscanf(tmpstr,"NAXIS2 = %d / Number of rows",&(adstruc->nrdat)); */
	sscanf(tmpstr,"NAXIS2 = %d / Number of rows",&nrdat);
	i=numkw+1;
      }
      i++;
    }
    adstruc->nrdat=adstruc->nrdat+nrdat;
    
    /* Which column is the AppId data? */
    fits_get_colnum(datfp, FALSE, appidnam, &acol, &pstat);
    if(pstat != 0){
      strcpy(msg,"No column named ");
      strcat(msg,appidnam);
      strcat(msg," in ");
      strcat(msg,hack_fnam(datfile));
      strcat(msg," ...skipping");
      c_fcecho(msg);
      /* flag = 2 for this condition */
      adstruc->flag=2;
      pstat=0;
      fits_close_file(datfp, &pstat);
      if(pstat != 0){
	c_fcerr(" ");
	strcpy(msg,"Could not close file ");
	strcat(msg,datfile);
	c_fcerr(msg);
	fits_report_error(stderr, pstat);
	exit(1);
      }
      return pstat;
    }

    strcpy(msg,"Found ");
    strcat(msg,appidnam);
    strcat(msg," in ");
    strcat(msg,hack_fnam(datfile));
    c_fcecho(msg);
  
  /* Find the Header Keywords for the AppID data column */
    sprintf(typstr,"TTYPE%d",acol);
    sprintf(formstr,"TFORM%d",acol);
    i=1;
    while (i<=numkw) {
      if (strncmp(typstr,(cards+(i-1)*FLEN_CARD),strlen(typstr)) == 0 ) {
	kwnum1=i-1;
      }
      if (strncmp(formstr,(cards+(i-1)*FLEN_CARD),strlen(formstr)) == 0 ){
	int nfound, nvec; /* CM debug */
	/* 29Apr98: skip vector columns */
	strncpy(dumstr,strchr((cards+(i-1)*FLEN_CARD),'\'')+1,1);
	nfound = sscanf(dumstr, "%d", &nvec);
	if (nfound == 1 && nvec != 1) {
	  strcpy(msg,"Sorry, but this is a vector column...skipping");
	  c_fcecho(msg);
	  /* flag = 2 for this condition too (don't write anything in o/p file) */
	  adstruc->flag=2;
	  pstat=0;
	  fits_close_file(datfp, &pstat);
	  if(pstat != 0){
	    c_fcerr(" ");
	    strcpy(msg,"Could not close file ");
	    strcat(msg,datfile);
	    c_fcerr(msg);
	    fits_report_error(stderr, pstat);
	    exit(1);
	  }
	  return pstat;
	} /* end test for vector cols */
	kwnum2=i-1;
	i=numkw+1;
      }
      i++;
    }
    /* Continuing to assume that TFORM is always last and TTYPE is first. */
    /* Seems to be true for *single valued* columns but NOT for vectors! */
    adstruc->numhdkw = kwnum2 - kwnum1 + 1;
  
  /* Allocate memory for the header keywords */
    adstruc->hdkw=(char *)malloc(adstruc->numhdkw*sizeof(char)*FLEN_CARD);
  
    /* Load up the header keyword array in the appid structure */
    for (i=0;i<adstruc->numhdkw;i++) {
      strncpy((adstruc->hdkw+i*FLEN_CARD),(cards+(i+kwnum1)*FLEN_CARD), FLEN_CARD);
    }
    free(cards);

    /* Allocate memory for the packet time and appid data columns */
    if (adstruc->timedat == 0) {
	adstruc->timedat = (double *) malloc(adstruc->nrdat * sizeof(double));
	adstruc->appdat = (double *) malloc(adstruc->nrdat * sizeof(double));
    } else {
	/* If memory was already allocated then attach to that */
	adstruc->timedat = (double *) realloc(adstruc->timedat,
					      adstruc->nrdat * sizeof(double));
	adstruc->appdat = (double *) realloc(adstruc->appdat, 
					     adstruc->nrdat * sizeof(double));
    }

    /* Try to read the MAX_GAP keyword, but don't report any errors
       since there should be a sensible default, at the global level. CM 2008-04-26 */
    if (adstruc->maxgap == 0) {
      int xstatus = 0;
      fits_write_errmark();
      fits_read_key(datfp, TDOUBLE, "MAX_GAP", &adstruc->maxgap, 0, &xstatus);
      fits_clear_errmark();
      if (xstatus == 0 && adstruc->maxgap != 0) {
	sprintf(msg,"Explicit MAX_GAP: %f", adstruc->maxgap);
	c_fcecho(msg);
      }
    }

    fits_read_key_lng(datfp, "NAXIS2", &dumrows, dumcom, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Error reading NAXIS2 keyword");
      fits_report_error(stderr, pstat);
      exit(1);
    }

    /* Get packet time stamp data (ASSUMED ALWAYS COLUMN #1) */
    fits_read_col_dbl(datfp, 1, 1, 1, dumrows, NULLVAL, (adstruc->timedat)+rowsread, &anyf, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Error reading Packet Time Stamp column");
      fits_report_error(stderr, pstat);
      exit(1);
    }
    
    /* Get AppID data */
    fits_read_col_dbl(datfp, acol, 1, 1, dumrows, NULLVAL, (adstruc->appdat)+rowsread, &anyf, &pstat); 
    if(pstat != 0){
      c_fcerr(" ");
      c_fcerr("Error reading AppID data column");
      fits_report_error(stderr, pstat);
      exit(1);
    }

    rowsread=rowsread+dumrows;

    fits_close_file(datfp, &pstat);
    if(pstat != 0){
      c_fcerr(" ");
      strcpy(msg,"Error closing file ");
      strcat(msg,datfile);
      c_fcerr(msg);
      fits_report_error(stderr, pstat);
      exit(1);
    }
  } /* end second loop over datafiles */
  return pstat;
}

void lintrp(x,y,num,x2,y2)
double *x, x2;
double *y, *y2;
int num;
{
  double slope;
  int pt1, pt2, i=0;
  
  while(x2 > *(x+i)) i++;
  /* in case x2=x */
  if (i == 0) {
    i=1;
  }
  pt1=i-1;
  pt2=i;
  if (DEBUGIT1 == TRUE) {
    printf("interpolating x2=%f between %f and %f\n",x2,*(x+pt1),*(x+pt2));
  }
  slope= (*(y+pt2) - *(y+pt1)) / (*(x+pt2) - *(x+pt1));
  *y2 = (x2 - *(x+pt1)) * slope + *(y+pt1);
  
}

void lintrp2(x,y,num,x2,y2,num2,gapsz)
double *x, *x2, gapsz;
double *y, *y2;
int num, num2;
{
  double slope;
  int pt1, pt2, i=0, j;
  
  for (j=0;j<num2;j++){
    if ( (*(x2+j) >= *x) && (*(x2+j) <= *(x+num-1)) ){
      while(*(x2+j) > *(x+i)) {
	i++;
      }
      /* in case *x2=*x */
      if (i == 0) {
	i=1;
      }
      pt1=i-1;
      pt2=i;
      if (DEBUGIT1 == TRUE) {
        printf("lintrp2: x2=%f between %f and %f\n",*(x2+j),*(x+pt1),*(x+pt2));
      }
      slope= (*(y+pt2) - *(y+pt1)) / (*(x+pt2) - *(x+pt1));
      *(y2+j) = (*(x2+j) - *(x+pt1)) * slope + *(y+pt1);
      /* Don't interpolate across gaps bigger than gapsz */
      if ( (*(x+pt2) - *(x+pt1)) > gapsz ) {
	*(y2+j)=NULLVAL;
      } 
    }else {
      *(y2+j) = NULLVAL;
    }
    if (DEBUGIT1 == TRUE) {
      printf("lintrp2: y2=%f\n",*(y2+j));
    }
  }
}

char *hack_fnam(fnam)
char fnam[];
/* returns the last two portions of a
   unix pathname: /v/w/x/y/z -> y/z */
{
  static char fnam_hacked[MAXC_FNAME]="";
  int i, numslash=0;

  for (i=strlen(fnam)-1;i>=0;i--){
    if (fnam[i] == '/') numslash++;
    if (numslash == 2) break;
  }
  strcpy(fnam_hacked,&fnam[i+1]);
  
  return(fnam_hacked);
}

void discret(x,y,num,x2,y2,num2,gapsz)
double *x, *x2, gapsz;
double *y, *y2;
int num, num2;
{
  int pt1, pt2, i=0, j;

  for (j=0;j<num2;j++){
    if ( (*(x2+j) >= *x) && (*(x2+j) <= *(x+num-1)) ){
      while(*(x2+j) > *(x+i)) {
	i++;
      }
      /* in case *x2=*x */
      if (i == 0) {
	i=1;
      }
      pt1=i-1;
      pt2=i;
      if (DEBUGIT1 == TRUE) {
        printf("discret: x2=%f between %f and %f\n",*(x2+j),*(x+pt1),*(x+pt2));
      }
      *(y2+j) = *(y+pt1);
      /* Don't work across gaps bigger than gapsz */
      if ( (*(x+pt2) - *(x+pt1)) > gapsz ) {
	*(y2+j)=NULLVAL;
      } 
    }else { /* x2 is out of range of x */
      *(y2+j) = NULLVAL;
    }
    if (DEBUGIT1 == TRUE) {
      printf("discret: y2=%f\n",*(y2+j));
    }
  }
}

/* This code is needed by IRAF */
  
#ifdef vms
#define F77CALL fcollec
#endif
#ifdef unix
#define F77CALL fcollec_
#endif

void F77CALL() 
{ 
  void Fcollect();
  
  Fcollect(); 
}
    
    


