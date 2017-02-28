/*
 * hxtdead - Input from events list FITS file; computes dead time corrections
 *           for each detector and applies the average to the EXPOSURE keyword
 *           in the .pha file.  Also writes 4 new keywords to .pha or .lc file,
 *           one per detector.
 *           Error checking - Both files must open correctly, necessary keywords
 *           must exist, deadtime correction has not yet been applied to the .pha
 *           or .lc file, detector ID data exists in the bytes of the event list
 *           data.  This file is directly derived from the hxtdeadpha and hxtdeadlc
 *           code as of 05/16/97.
 *
 * hxtdead <uld, xuld, apPos and apMod values file> <events_list_FITS_file> <.pha_file | .lc_file>
 *
 * Modified:
 */

static char rcsid[] = "$Id: hxtdead.c,v 1.6 2001/11/07 16:34:08 miket Exp $";
/*
 * $Log: hxtdead.c,v $
 * Revision 1.6  2001/11/07 16:34:08  miket
 * shutting up compiler warnings about possible uninitialized variables
 *
 * Revision 1.5  2001/10/15 19:28:35  zpan
 *
 * fix uninitialized variable's warning
 *
 * Revision 1.4  1999/08/17 15:05:02  miket
 * bugs fixed in v2.0.0 from T.Gasaway (UCSD)
 *
 * Revision 1.9  1998/07/09 15:09:12  tgasaway
 * Make changes to the generic error message to allow identification
 * of actual line number where error occurred.  Add many other changes
 * since the last of these local checkins mostly fixes to accommodate
 * extending the ticksOfDeadTimePerIDF[][] array for data out of range.
 *
 * Revision 1.8  1998/02/26 21:30:31  tgasaway
 * Revised to look at .pha files that use more than one SpecDeti
 * value to produce single COUNT or RATE (HDUCLAS3) files.  Moved
 * code around to allow all averages to use only those detectors
 * that appear in the .pha/.lc file, i.e., All detectors for the
 * ROWID1 value TimSerCnt and any and all of the SpecDeti that
 * appear in ROWID1-ROWID4.  As of this check-in, this code has
 * not been tested.
 *
 * Revision 1.7  1998/01/20 19:28:44  tgasaway
 * Add multiple detector case.  Change all exit(1) to exit(-1)
 * and change all returns after errors to exit(-1), all for use
 * in scripting.  Clean a few small bugs found by Duane Gruber.
 *
 * Revision 1.6  1997/11/26 18:20:53  tgasaway
 * Check it in with corrections through 11/20/97.  This is
 * everything except adding DEADC column to the archive/lc/
 * counts cases.
 *
 * Revision 1.5  1997/10/14 17:56:28  tgasaway
 * Check-in to save intermediate changes as of Oct 14, 1997.
 * After this big changes are coming.
 *
 * Revision 1.4  1997/09/03 20:50:40  tgasaway
 * Fix a bug in using CALDB; Move the '{' from after the for loop to before
 * the for loop to make the "if" statement work properly.  P.R. Blanco did
 * this fix which is applied here.  Add dump of file name if chatter level
 * is sufficient.  Do this whether the file name was used as input or if it
 * was obtained using CALDB as the input name.  Add/subtract offsets of 0.001
 * to the variables tstopOfPhaLcFile and tstartOfPhaLcFile to account for
 * roundoff errors in reading/constructing the times used for time range
 * checking.
 *
 * Revision 1.3  1997/07/01  15:13:08  tgasaway
 * Add CALDB interface via FORTRAN call using gtcalf_interface.f.
 * Not yet fully tested since the CALDB doesn't have the HEXTE
 * cal files in it yet.
 *
 * Revision 1.2  1997/06/26  22:25:27  tgasaway
 * Adds use of hxtdead_pwa.fits and hxtdead_pwb.fits files to
 * get Alpha and Gamma coefficients.  Does not yet use CALDB.
 *
 * Revision 1.1  1997/05/19  15:31:35  tgasaway
 * Initial revision
 *
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h> 
#include <math.h> 
#include <stdio.h>
#include <string.h>
#include "cfortran.h"
#include "pctype.h"
#include "cfitsio.h"
#include "ftools.h"
#include "ftoolstruct.h"
#include "xpi.h"
#include "xte.h"

#include "fitsio.h"  /* Every program that uses the CFITSIO interface must */
/* include the fitsio.h header file.  This file       */
/* contains the prototypes for all the routines and   */
/* defines the error status values and other symbolic */
/* constants used in the interface.                   */

#define CHATTER(level)   if ( chatter >= (level) ) Fcecho( errorMsg );

double xuldArray[8192][4];
double uldArray[8192][4];
double armArray[8192][4];
double trigArray[8192][4];
double hwVetoArray[8192][4];
double goodEventsArray[8192][4];
double xuld[4];
double uld[4];
double arm[4];
double trig[4];
double hwVeto[4];
double goodEvents[4];

unsigned char apPos[8192];
unsigned char apModPeriod[8192];

double nonBlankTime[5] = { 16.0, 12.0, 14.0, 16.0, 16.0 };  /* Currently works only for 0, 1 and 2 (stare, 16S and 32S) */

#define Sixteen ((double)16.0)

#define goodEventsCorrectionFactor  -3.75e-6   /* -3.75e-7s/good event */
#define extraArmsCorrectionFactor   -4.00e-6   /* 16 * -2.5e-7s */
#define extraVetosCorrectionFactor  -4.80e-5   /* 16 * -3.0e-6s */

/* The following arrays are [detector] indexed. */
float Alpha[4];
float Gamma[4];
double AlphaCorr[4];
double GammaCorr[4];

double ticksOfDeadTimePerIDF[8192][4];  /* Indexed by IDF and detector number. */
double avgTicksOfDeadTimePerIDF[4];     /* Indexed by detector number. */
double goodTimeStart[8192];
double goodTimeStop[8192];
int detectorHasLivetimeCounts[4];
int detRequested[4] = { FALSE, FALSE, FALSE, FALSE };
double goneBadTime[2][4] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 68729232.0, 0.0 };
char validDetectorString[8] = { 'P', 'W', '\000', '\000', '\000', '\000', '\000', '\000' };
char detectorsInLivetimeString[9] = { '\000', ' ', '\000', ' ', '\000', ' ', '\000', ' ', '\000' };

double averageDeadTimePerIDF[8192];

double pStart[8192];
double pStop[8192];

double dtfract[1];
double dtfractPerDetector[4];

/* Define the uld/xuld error table entries. */
typedef struct sErrorEntry {
  short int start;
  short int stop;
  unsigned char type;     /* 0 == uld, 1 == xuld, 2 == arm, 3 == trigger, 4 == H/W Veto */
  unsigned char detector; /* 0-3 == detectors 1-4 */
} sErrorEntry;

/* Define the array of error table entries. */
static sErrorEntry errorsList[8192];
short int errorsListIndex = -1;
short int engineeringErrorsFound = 0;

char dataName[][16] = { "Uld ", "Xuld", "Arm", "Trigger", "H/W Veto" };

char hduclasNames[][16] = { "COUNT", "MEAN", "RATE", "TYPE:II" };
enum { COUNT, MEAN, RATE, TYPEII };

char deadcNameString[8]  = { "DEADC\0\0" };
char timeNameString[8]   = { "TIME\0\0\0" };
char rateNameString[8]   = { "RATE\0\0\0" };
char countsNameString[8] = { "COUNTS\0" };
char errorNameString[8]  = { "ERROR\0\0" };
char rowidNameString[8]  = { "ROWID\0\0"};

void printerror( int status, int lineNumber );

/* Add things to make this a "real" ftool. */
#define FitsStrBufLen 73    /* Allocate string buffer length */
#define MAXC_FNAME 256      /* Define the size of the arrays for C */

#define EventsListData  0
#define ArchiveData     1
#define BinnedData      2

#define phaFileType  0
#define lcFileType   1

typedef struct commonType {
  char passedFileName[128];
} commonType;

extern commonType pfile_;

/* Prototype for the FORTRAN routine we're calling. */
void gtcaface_( long int *chatter, long int *detnum, long int *status );

void hxtdead()
{
  char infile2[MAXC_FNAME], infile3[MAXC_FNAME],
    infile4[MAXC_FNAME], calFileName[MAXC_FNAME],
    detList[MAXC_FNAME];

  int parstat=0, BufLen_2=255;
  char text[FITS_CLEN_ERRMSG], errorMsg[512];

  fitsfile *eventarcptr;   /* pointer to the events list FITS file   */
  fitsfile *phalcptr;      /* pointer to the .pha FITS file          */
  fitsfile *engvalptr;     /* pointer to the uld/xuld data file      */
  fitsfile *calptr;        /* pointer to the duration cal data file  */
  int hdutype, i, idfDiff, j, k, l, status;
  int calnaxis2, detId, detIndex, eLByteCnt, naxis2, tailDatamode, detchans;
  char eLByte[7];
  char keyval[FLEN_VALUE];       /* string lengths defined in fitsioc.h */
  char keycomment[FLEN_COMMENT]; /* string lengths defined in fitsioc.h */
  int anynul, errorFound;
  int tstartIOfPhaLcFile, tstopIOfPhaLcFile;
  double Time[1], timezero, timedel, offsetToTime, tstartOfPhaLcFile, tstopOfPhaLcFile;
  double tstartOfEngFile, tstopOfEngFile;
  double tstartOfEvtArcFile, tstopOfEvtArcFile;
  long int tstartOfEvtArcFileIDF, tstopOfEvtArcFileIDF;
  double Rate[1];
  double Error[1];
  long int LiveTime[1], oldIDF=0L, newIDF, begIDF, endIDF, recCnt=1, chanCnt, realStartingRecord, realStoppingRecord;
  long int realStartingRecordIDF=0L, realStoppingRecordIDF=0L;
  long int index0IDFValue=0L, currentIDF=0L, lastValidIDFValue, maxIDFsSpanned, numberOfValidTlmData;
  double newEventListTime, oldArchiveDataTime, oldEventListTime,
index0TimeValue=0L, lastValidTimeValue;
  unsigned char ClusterPos[1];
  unsigned char oldClusterPos[1];
  unsigned char EventList[7];
  double detIDFCorrFact[4], detIDFCorrFactSave[4], LTAverage, deltaValue;
  short int ctUldD[4], ctXuldD[4], ctArmD[4], ctTrigD[4], ctHwVetoD[4];
  short int DurXuldD[4];
  unsigned char apPosition[1];
  unsigned char apPeriod[1];
  long int GoodCnt[1];
  float exposure, oldexposure=-3.14159;
  double dt[4], delta[4], f, det0LTCorrection;
  int ClusterIndex, DurationIndex;
  int colNumSTART, colNumSTOP, detectorIsToBeCounted;
  int colNumTime, colNumLiveTime, colNumClstrPosition, colNumEvent;
  int colNumGoodCnt, colNumLiveTimeDet0, colNumLiveTimeDet1, colNumLiveTimeDet2, colNumLiveTimeDet3;
  int colNumposApMod, colNumDwellPdApMod;
  int colNumctUldD0, colNumctXuldD0, colNumctUldD1, colNumctXuldD1;
  int colNumctUldD2, colNumctXuldD2, colNumctUldD3, colNumctXuldD3;
  int colNumctArmD0, colNumctArmD1, colNumctArmD2, colNumctArmD3;
  int colNumctTrigD0, colNumctTrigD1, colNumctTrigD2, colNumctTrigD3;
  int colNumctHwVetoD0, colNumctHwVetoD1, colNumctHwVetoD2, colNumctHwVetoD3;
  int colNumERROR, colNumRATE, colNumTIME, colNumSTAT_ERR;
  int colTSTART, colTSTOP, colNumROWID;
  int colpw0co0, colpw1co0, colpw2co0, colpw3co0;
  int colpw0co1, colpw1co1, colpw2co1, colpw3co1;
  double Start[1];
  double Stop[1];
  double startTimeOfGoodTimeInterval, stopTimeOfGoodTimeInterval, multFactor, multFactorPerDetector[4];
  double lt, ltPerDetector[4], ovl, p0, sumovl, sumlt, sumovlPerDetector[4], sumltPerDetector[4];
  double numberOfValidDetectors=0L, numberOfValidDetectorsInPhaLcFile=0.0, realNumberOfValidDetectors;
  char *pGLeftBracket, *pColon, *p1, *p2, *p3;
  int chatter, goodSEXFFRevision = 50100, goodSAXFFRevision = 50301, XFFRevision, yet_checked = 0;
  int typeOfDeadtimeData, typeOfPhaLcFile;
  int usePreviousLivetimes=0, hxtbackWasUsed;
  double XA_i, XV_i;
  double startTimeOfBin, stopTimeOfBin;
  char *detName, *telescop, *instrume, *pcalFilNam;
  char hduclas3[FLEN_VALUE];
  char hduclas4[FLEN_VALUE];
  char rowid1[FLEN_VALUE];
  char rowidn[FLEN_VALUE];
  char rowid2[FLEN_VALUE];  /* Use this just for the ROWID2 keyword info. */
  char *rowid[1];
  int hduclas3IntIndex=-1, hduclas4IntIndex=-1;
  int deadTimesUpdated=FALSE, furtherProcessingOk = TRUE, detectorNumber;
  double sum;
  int deadcNumber;
  int deadcColValue;
  int tfields, nfield, numberOfSets;
  char detectorIsValid[4] = { FALSE, FALSE, FALSE, FALSE };
  int shift;

  /* Do some initializations too tough to do elsewhere. */
  (void)strcpy( hduclas3, "" );
  (void)strcpy( hduclas4, "" );

  rowid[0] = (char *)malloc(FLEN_VALUE);

  Fcecho(" ");
  Fcecho("Running HXTDEAD version 2.0.0");
  Fcecho("=============================================");
  Fcecho(" ");

  Uclgst("calvalf", calFileName, &parstat);
  if (parstat != 0) {
    Fcecho(" ");
    Fcecho("Could not get Calibration file name.");
    FCGERR(parstat, text);
    exit(-1);
  }

  Uclgst("engvalf", infile2, &parstat);
  if (parstat != 0) {
    Fcecho(" ");
    Fcecho("Could not get Engineering  values input file name.");
    FCGERR(parstat, text);
    exit(-1);
  }

  Uclgst("eventarcf", infile3, &parstat);
  if (parstat != 0) {
    Fcecho(" ");
    Fcecho("Could not get 'events list or archive data' input file name.");
    FCGERR(parstat, text);
    exit(-1);
  }

  Uclgst("phalcf", infile4, &parstat);
  if (parstat != 0) {
    Fcecho(" ");
    Fcecho("Could not get '.pha/.lc file' name.");
    FCGERR(parstat, text);
    exit(-1);
  }

  Uclgsi("chatter", &chatter, &parstat);
  if (parstat != 0) {
    Fcecho(" ");
    Fcecho("Could not get chatter parameter from .par file.");
    parstat = 0;
    Fcecho(" ");
    Fcecho("Setting CHATTER = 20");
    chatter = 20;
  }

  Uclgst("detectors", detList, &parstat);
  if (parstat != 0) {
    Fcecho(" ");
    Fcecho("Could not get list of detectors to use.");
    FCGERR(parstat, text);
    exit(-1);
  }

  /* Parse the detector list.  Punt if badly formed. */
  if (strlen(detList) > 4 || strlen(detList) < 1) {  /* First check the string length. */
    sprintf( errorMsg, "Error: Unable to parse Detector string input parameter value.");
    Fcecho( errorMsg );
    sprintf( errorMsg, "       It appears to be of illegal length equal to %d", strlen(detList) );
    Fcecho( errorMsg );
    sprintf( errorMsg, "       Parameter string = %s", detList);
    Fcecho( errorMsg );
    exit(-1);
  }

  /* The string is a good length, try to parse it into "-" or [0-3]{1,4}.  */
  /* Note that if there is a duplicate of a requested detector, a warning  */
  /* message will be produced but it will not kill the program.            */
  if ( strcmp(detList, "-") == 0 ) {
    /* We will use the defaults. */
    for (i=0; i<4; i++) {
      detRequested[i] = TRUE;
    }
  }
  else {
    /* Parse each character of the detlist string. */
    for (i=0; i<strlen(detList); i++) {
      detectorNumber = detList[i] - 0x30;
      if (detectorNumber > 3 || detectorNumber < 0) {
	sprintf( errorMsg, "Error: Detector out of range.  Detector character = %c in detector string %s",
		 detList[i], detList );
	Fcecho( errorMsg );
	exit(-1);
      }
      else {
	if ( detRequested[detectorNumber] == TRUE ) {
	  sprintf( errorMsg, "Warning: Duplicate detector requested.  Detector character = %c in detector string %s",
		 detList[i], detList );
	  Fcecho( errorMsg );
	}
	else {
	  detRequested[detectorNumber] = TRUE;
	}
      }
    }
  }

  for (i=0; i<4; i++) {
    detIDFCorrFact[i] = 0.0;
    detIDFCorrFactSave[i] = 0.0;
    DurXuldD[i] = 0;
  }

  for (i=0; i<8192; i++) {
    averageDeadTimePerIDF[i] = 0.0;
    apModPeriod[i] = (unsigned char) 255;
    for (j=0; j<4; j++) {
      ticksOfDeadTimePerIDF[i][j] = 0.0;
      xuldArray[i][j] = -1.0;
      uldArray[i][j] = -1.0;
      armArray[i][j] = -1.0;
      trigArray[i][j] = -1.0;
      hwVetoArray[i][j] = -1.0;
      goodEventsArray[i][j] = 0.0;
    }
  }

  status = 0;         /* initialize status before calling fitsio routines */

  /* We need a cluster index and the only reliable way to get it is by  */
  /* using the TDDES keyword in the events list file. (cf. Duane Gruber */
  /* and Phil Blanco)  We parse the keyword string for G[n] where n is  */
  /* 0 or 1 and that will be used to select the cluster.                */
  /* Open input events FITS file. */
  if (fits_open_file(&eventarcptr, infile3, READONLY, &status))
    printerror( status, __LINE__ );

  /* Attempt to move to HDU 2 in the events file. */
  if (fits_movabs_hdu(eventarcptr, 2, &hdutype, &status))
    printerror( status, __LINE__ );

  /* Get TDDES keyword as a string, we will parse it momentarily. */
  if ( fits_read_keyword(eventarcptr, "TDDES", keyval, keycomment, &status) )
    printerror( status, __LINE__ );

  if ( (pGLeftBracket = strstr( keyval, "G[" )) != NULL ) {
    ClusterIndex = ((int)(*(pGLeftBracket+2))) - 0x30;
  }
  else {
    sprintf( errorMsg, "TDDES = %s / %s", keyval, keycomment);
    Fcecho( errorMsg );
    sprintf( errorMsg, "Error: Unable to parse TDDES keyword value to determine cluster.");
    Fcecho( errorMsg );
    exit(-1);
  }

  /* Get CREATOR keyword as a string, we will parse it momentarily. */
  /* Parsing this keyword gives us the XFF Revision number to be    */
  /* used to check that we have enough info to do the correction.   */
  if ( fits_read_keyword(eventarcptr, "CREATOR", keyval, keycomment, &status) )
    printerror( status, __LINE__ );
  if ( (pColon = strstr( keyval, ":" )) != NULL ) {
    pColon++;
    p1 = strtok( pColon, "." );
    p2 = strtok( NULL, "." );
    p3 = strtok( NULL, "." );
    XFFRevision = ((int)atol( p1 ) * 10000 + (int)atol( p2 ) * 100 + (int)atol( p3 ));
  }
  else {
    sprintf( errorMsg, "CREATOR = %s / %s", keyval, keycomment);
    Fcecho( errorMsg );
    sprintf( errorMsg, "Error: Unable to parse CREATOR keyword value to determine XFF Revision.");
    Fcecho( errorMsg );
    exit(-1);
  }

  /* open uld/xuld values FITS file */
  if (fits_open_file(&engvalptr, infile2, READONLY, &status))
    printerror( status, __LINE__ );

  /* attempt to move to HDU 2 in the uld/xuld values FITS file */
  if (fits_movabs_hdu(engvalptr, 2, &hdutype, &status))
    printerror( status, __LINE__ );

  /* Save the TSTART and TSTOP values from the engineering file. */
  /* TSTART is equal to the 1st row of eng. data time stamp. */
  if ( fits_read_key(engvalptr, TDOUBLE, "TSTART", (void *)&tstartOfEngFile, keycomment, &status) )
    printerror( status, __LINE__ );
  /* TSTOP is equal to the last row of eng. data time stamp + 16 seconds!!! */
  if ( fits_read_key(engvalptr, TDOUBLE, "TSTOP", (void *)&tstopOfEngFile, keycomment, &status) )
    printerror( status, __LINE__ );

  /* Save the TSTART and TSTOP values from the events-list/archive file. */
  if ( fits_read_key(eventarcptr, TDOUBLE, "TSTART", (void *)&tstartOfEvtArcFile, keycomment, &status) )
    printerror( status, __LINE__ );
  if ( fits_read_key(eventarcptr, TDOUBLE, "TSTOP", (void *)&tstopOfEvtArcFile, keycomment, &status) )
    printerror( status, __LINE__ );

  /* Get NAXIS2 for number of sets of values in the engineering file. */
  if ( fits_read_key(engvalptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
    printerror( status, __LINE__ );

  sprintf( errorMsg, "uld/xuld values NAXIS2 = %d / %s", naxis2, keycomment);
  CHATTER(10);

  /* Get column numbers for Time, Moulator Position, Modulator Dwell Time and the */
  /*  ULD/XULD Duration values.                                                   */
  if ( fits_get_colnum(engvalptr, FALSE,         "Time",         &colNumTime, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "posApMod",     &colNumposApMod, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE, "DwellPdApMod", &colNumDwellPdApMod, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctUldD0",      &colNumctUldD0, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctXuldD0",     &colNumctXuldD0, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctArmD0",      &colNumctArmD0, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctTrigD0",     &colNumctTrigD0, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,   "ctHwVetoD0",   &colNumctHwVetoD0, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctUldD1",      &colNumctUldD1, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctXuldD1",     &colNumctXuldD1, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctArmD1",      &colNumctArmD1, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctTrigD1",     &colNumctTrigD1, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,   "ctHwVetoD1",   &colNumctHwVetoD1, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctUldD2",      &colNumctUldD2, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctXuldD2",     &colNumctXuldD2, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctArmD2",      &colNumctArmD2, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctTrigD2",     &colNumctTrigD2, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,   "ctHwVetoD2",   &colNumctHwVetoD2, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctUldD3",      &colNumctUldD3, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctXuldD3",     &colNumctXuldD3, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,      "ctArmD3",      &colNumctArmD3, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,     "ctTrigD3",     &colNumctTrigD3, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(engvalptr, FALSE,   "ctHwVetoD3",   &colNumctHwVetoD3, &status) )
    printerror( status, __LINE__ );

  /* Ceck the beginning and ending times and use them to determine what duration value */
  /* to use when selecting the Alpha and Gamma parameters.                             */
  fits_read_col_dbl(engvalptr,           colNumTime,       1, 1L, 1L,
		    (double)0,              Time, &anynul, &status); /* read Time of first record */
  begIDF = ((long int)Time[0]) / 16;
  startTimeOfGoodTimeInterval = Time[0];  /* Temporarily use this variable. */
  fits_read_col_dbl(engvalptr,           colNumTime,  naxis2, 1L, 1L,
		    (double)0,              Time, &anynul, &status); /* read Time of last record */
  endIDF = ((long int)Time[0]) / 16;
  stopTimeOfGoodTimeInterval = Time[0];  /* Temporarily use this variable. */

  sprintf( errorMsg, "Engineering data:  begIDF = %ld  endIDF = %ld", begIDF, endIDF );
  CHATTER(15);

  if ( strcasecmp ( calFileName, "CALDB" ) == 0 ) {
    for (i=0; i<128; i++) pfile_.passedFileName[i] = 0;  /* Zero out the file name. */

    gtcaface_( (long int *)&chatter, (long int *)&ClusterIndex, (long int *)&status );

    if ( status == 0 ) {
      pcalFilNam = pfile_.passedFileName;
      i = 0;
      while ( pfile_.passedFileName[i] == ' ' ) {
	i++;
	pcalFilNam++;
      }
      for ( j=i; j<128; j++ ) {
	if ( pfile_.passedFileName[j] == ' ' ) {
	  pfile_.passedFileName[j] = 0;
	}
      }
      (void)strcpy( calFileName, pcalFilNam );
    }
    else {
      sprintf( errorMsg, "Error: hxtdead: Unable to obtain calibration file using CALDB.  Program terminating...");
      Fcecho( errorMsg );
      exit(-1);
    }	  
  }

  sprintf( errorMsg, "Dead-time coefficients file is %s.\n", calFileName );
  CHATTER(10);

  if (fits_open_file(&calptr, calFileName, READONLY, &status))
    printerror( status, __LINE__ );

  /* Attempt to move to HDU 2 in the xuld duration calibration FITS file */
  if (fits_movabs_hdu(calptr, 2, &hdutype, &status))
    printerror( status, __LINE__ );

  /* Get the NAXIS2 for number of sets of values */
  if ( fits_read_key(calptr, TINT, "NAXIS2", (void *)&calnaxis2, keycomment, &status) )
    printerror( status, __LINE__ );

  sprintf( errorMsg, "xuld duration calibration NAXIS2 = %d / %s", calnaxis2, keycomment);
  CHATTER(10);

  /* Get column numbers for the pwa0co0 - pwa3co1 columns. */
  if ( fits_get_colnum(calptr, FALSE,  "TSTART",  &colTSTART, &status) )
    printerror( status, __LINE__ );
  if ( fits_get_colnum(calptr, FALSE,   "TSTOP",   &colTSTOP, &status) )
    printerror( status, __LINE__ );
  if ( ClusterIndex == 0 ) {  /* Do Cluster A column names. */
    if ( fits_get_colnum(calptr, FALSE, "pwa0co0", &colpw0co0, &status) )  /* Alpha Det 0 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwa1co0", &colpw1co0, &status) )  /* Alpha Det 1 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwa2co0", &colpw2co0, &status) )  /* Alpha Det 2 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwa3co0", &colpw3co0, &status) )  /* Alpha Det 3 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwa0co1", &colpw0co1, &status) )  /* Gamma Det 0 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwa1co1", &colpw1co1, &status) )  /* Gamma Det 1 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwa2co1", &colpw2co1, &status) )  /* Gamma Det 2 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwa3co1", &colpw3co1, &status) )  /* Gamma Det 3 */
      printerror( status, __LINE__ );
  }
  else {  /* Do Cluster B column names. */
    if ( fits_get_colnum(calptr, FALSE, "pwb0co0", &colpw0co0, &status) )  /* Alpha Det 0 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwb1co0", &colpw1co0, &status) )  /* Alpha Det 1 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwb2co0", &colpw2co0, &status) )  /* Alpha Det 2 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwb3co0", &colpw3co0, &status) )  /* Alpha Det 3 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwb0co1", &colpw0co1, &status) )  /* Gamma Det 0 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwb1co1", &colpw1co1, &status) )  /* Gamma Det 1 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwb2co1", &colpw2co1, &status) )  /* Gamma Det 2 */
      printerror( status, __LINE__ );
    if ( fits_get_colnum(calptr, FALSE, "pwb3co1", &colpw3co1, &status) )  /* Gamma Det 3 */
      printerror( status, __LINE__ );
  }

  for (recCnt=1; recCnt<=calnaxis2; recCnt++) {
    fits_read_col_dbl(calptr, colTSTART, recCnt, 1L, 1L,
		      (double)0,  Start, &anynul, &status); /* read Start Time */
    fits_read_col_dbl(calptr,  colTSTOP, recCnt, 1L, 1L,
		      (double)0,   Stop, &anynul, &status); /* read  Stop Time */
    if ( (Start[0] <= startTimeOfGoodTimeInterval) && (stopTimeOfGoodTimeInterval <= Stop[0]) ) {
      fits_read_col_flt(calptr, colpw0co0, recCnt, 1L, 1L,
			(float)0, &Alpha[0], &anynul, &status); /* pwX0co0 */
      fits_read_col_flt(calptr, colpw1co0, recCnt, 1L, 1L,
			(float)0, &Alpha[1], &anynul, &status); /* pwX1co0 */
      fits_read_col_flt(calptr, colpw2co0, recCnt, 1L, 1L,
			(float)0, &Alpha[2], &anynul, &status); /* pwX2co0 */
      fits_read_col_flt(calptr, colpw3co0, recCnt, 1L, 1L,
			(float)0, &Alpha[3], &anynul, &status); /* pwX3co0 */
      fits_read_col_flt(calptr, colpw0co1, recCnt, 1L, 1L,
			(float)0, &Gamma[0], &anynul, &status); /* pwX0co1 */
      fits_read_col_flt(calptr, colpw1co1, recCnt, 1L, 1L,
			(float)0, &Gamma[1], &anynul, &status); /* pwX1co1 */
      fits_read_col_flt(calptr, colpw2co1, recCnt, 1L, 1L,
			(float)0, &Gamma[2], &anynul, &status); /* pwX2co1 */
      fits_read_col_flt(calptr, colpw3co1, recCnt, 1L, 1L,
			(float)0, &Gamma[3], &anynul, &status); /* pwX3co1 */
      for (i=0; i<4; i++) {
	sprintf( errorMsg, "Alpha[%d] = %f   Gamma[%d] = %f", i, Alpha[i], i, Gamma[i] );
	CHATTER(20);
      }
    }
  }

  if ( fits_close_file(calptr, &status) )
    printerror( status, __LINE__ );

  for (i=0; i<4; i++) {
    AlphaCorr[i] = Sixteen * (double)Alpha[i];
    GammaCorr[i] = Sixteen * (double)Gamma[i];
  }

  for (recCnt=1; recCnt<=naxis2; recCnt++) {
    fits_read_col_dbl(engvalptr,           colNumTime, recCnt, 1L, 1L,
		      (double)0,              Time, &anynul, &status); /* read Time */
    fits_read_col_byt(engvalptr,       colNumposApMod, recCnt, 1L, 1L,
		      (unsigned char)0, apPosition, &anynul, &status); /* read aperture position */
    fits_read_col_byt(engvalptr,   colNumDwellPdApMod, recCnt, 1L, 1L,
		      (unsigned char)0,   apPeriod, &anynul, &status); /* read aperture period */
    fits_read_col_sht(engvalptr,        colNumctUldD0, recCnt, 1L, 1L,
		      (short int)0,     &ctUldD[0], &anynul, &status); /* read uld Det 0 */
    fits_read_col_sht(engvalptr,       colNumctXuldD0, recCnt, 1L, 1L,
		      (short int)0,    &ctXuldD[0], &anynul, &status); /* read xuld Det 0 */
    fits_read_col_sht(engvalptr,        colNumctArmD0, recCnt, 1L, 1L,
		      (short int)0,     &ctArmD[0], &anynul, &status); /* read Arm Cnts/Sec 0 */
    fits_read_col_sht(engvalptr,       colNumctTrigD0, recCnt, 1L, 1L,
		      (short int)0,    &ctTrigD[0], &anynul, &status); /* read Trigger Cnts/Sec 0 */
    fits_read_col_sht(engvalptr,     colNumctHwVetoD0, recCnt, 1L, 1L,
		      (short int)0,  &ctHwVetoD[0], &anynul, &status); /* read H/W Veto Cnts/Sec 0 */
    fits_read_col_sht(engvalptr,        colNumctUldD1, recCnt, 1L, 1L,
		      (short int)0,     &ctUldD[1], &anynul, &status); /* read uld Det 1 */
    fits_read_col_sht(engvalptr,       colNumctXuldD1, recCnt, 1L, 1L,
		      (short int)0,    &ctXuldD[1], &anynul, &status); /* read xuld Det 1 */
    fits_read_col_sht(engvalptr,        colNumctArmD1, recCnt, 1L, 1L,
		      (short int)0,     &ctArmD[1], &anynul, &status); /* read Arm Cnts/Sec 1 */
    fits_read_col_sht(engvalptr,       colNumctTrigD1, recCnt, 1L, 1L,
		      (short int)0,    &ctTrigD[1], &anynul, &status); /* read Trigger Cnts/Sec 1 */
    fits_read_col_sht(engvalptr,     colNumctHwVetoD1, recCnt, 1L, 1L,
		      (short int)0,  &ctHwVetoD[1], &anynul, &status); /* read H/W Veto Cnts/Sec 1 */
    fits_read_col_sht(engvalptr,        colNumctUldD2, recCnt, 1L, 1L,
		      (short int)0,     &ctUldD[2], &anynul, &status); /* read uld Det 2 */
    fits_read_col_sht(engvalptr,       colNumctXuldD2, recCnt, 1L, 1L,
		      (short int)0,    &ctXuldD[2], &anynul, &status); /* read xuld Det 2 */
    fits_read_col_sht(engvalptr,        colNumctArmD2, recCnt, 1L, 1L,
		      (short int)0,     &ctArmD[2], &anynul, &status); /* read Arm Cnts/Sec 2 */
    fits_read_col_sht(engvalptr,       colNumctTrigD2, recCnt, 1L, 1L,
		      (short int)0,    &ctTrigD[2], &anynul, &status); /* read Trigger Cnts/Sec 2 */
    fits_read_col_sht(engvalptr,     colNumctHwVetoD2, recCnt, 1L, 1L,
		      (short int)0,  &ctHwVetoD[2], &anynul, &status); /* read H/W Veto Cnts/Sec 2 */
    fits_read_col_sht(engvalptr,        colNumctUldD3, recCnt, 1L, 1L,
		      (short int)0,     &ctUldD[3], &anynul, &status); /* read uld Det 3 */
    fits_read_col_sht(engvalptr,       colNumctXuldD3, recCnt, 1L, 1L,
		      (short int)0,    &ctXuldD[3], &anynul, &status); /* read xuld Det 3 */
    fits_read_col_sht(engvalptr,        colNumctArmD3, recCnt, 1L, 1L,
		      (short int)0,     &ctArmD[3], &anynul, &status); /* read Arm Cnts/Sec 3 */
    fits_read_col_sht(engvalptr,       colNumctTrigD3, recCnt, 1L, 1L,
		      (short int)0,    &ctTrigD[3], &anynul, &status); /* read Trigger Cnts/Sec 3 */
    fits_read_col_sht(engvalptr,     colNumctHwVetoD3, recCnt, 1L, 1L,
		      (short int)0,  &ctHwVetoD[3], &anynul, &status); /* read H/W Veto Cnts/Sec 3 */

    if ( recCnt == 1 ) {
      index0IDFValue = ((long int)Time[0]) / 16;  /* This is the same as begIDF. */
      index0TimeValue = Time[0];
      currentIDF = index0IDFValue;
      for (i=0; i<4; i++) {
	uldArray[0][i] = (double)ctUldD[i];
	xuldArray[0][i] = (double)ctXuldD[i];
	armArray[0][i] = (double)ctArmD[i];
	trigArray[0][i] = (double)ctTrigD[i];
	hwVetoArray[0][i] = (double)ctHwVetoD[i];
      }
      apPos[0] = apPosition[0];
      apModPeriod[0] = apPeriod[0];
    }
    else {
      currentIDF = ((long int)Time[0]) / 16;
      for (i=0; i<4; i++) {
	uldArray[currentIDF-index0IDFValue][i] = (double)ctUldD[i];
	xuldArray[currentIDF-index0IDFValue][i] = (double)ctXuldD[i];
	armArray[currentIDF-index0IDFValue][i] = (double)ctArmD[i];
	trigArray[currentIDF-index0IDFValue][i] = (double)ctTrigD[i];
	hwVetoArray[currentIDF-index0IDFValue][i] = (double)ctHwVetoD[i];
      }
      apPos[currentIDF-index0IDFValue] = apPosition[0];
      apModPeriod[currentIDF-index0IDFValue] = apPeriod[0];
    }
  }
  lastValidIDFValue = currentIDF;
  lastValidTimeValue = Time[0];
  numberOfValidTlmData = lastValidIDFValue - index0IDFValue + 1;

  sprintf( errorMsg, "\nDone collecting uld/xuld, aperture position and aperture period data from engineering file.");
  CHATTER(10);

  sprintf( errorMsg, "\nlastValidIDFValue = %d;  lastValidTimeValue = %lf;  numberOfValidTlmData = %d", lastValidIDFValue, lastValidTimeValue, numberOfValidTlmData );
  CHATTER(20);

  for (i=0; i<numberOfValidTlmData; i++) {
    sprintf( errorMsg, "apPos[%d] = %d", i, apPos[i] );
    CHATTER(20);
  }

  errorFound = 0;
  for (i=0; i<numberOfValidTlmData; i++) {
    if ( apModPeriod[i] == (unsigned char)255 ) {
      sprintf( errorMsg, "apModPeriod undfined for index i = %d", i );
      CHATTER(2);
      errorFound = 1;
    }
    for (j=0; j<4; j++) {
      if ( uldArray[i][j] == -1.0 || xuldArray[i][j] == -1.0 ) {
	sprintf( errorMsg, "uld or xuld undfined for indices i = %d j = %d", i, j);
	CHATTER(2);
	errorFound = 1;
      }
    }
  }
  sprintf( errorMsg, "\nDone checking uld/xuld data\n");
  CHATTER(10);

  if ( errorFound ) {
    /* Oh well, we have to intialize the error list now. */
    for (i=0; i<8192; i++) {
      errorsList[i].type = 5; /* since 0 - 4 are the only valid entries */
    }
    engineeringErrorsFound = 1;  /* Flag this for printing at end of program. */
    sprintf( errorMsg, "Warning: Missing uld or xuld value(s); interpolation proceding...\n");
    CHATTER(2);
    /* Re-search the uld and xuld arrays and for -1.0 values; interpolate to get a value. */
    for (i=0; i<numberOfValidTlmData; i++) {  /* for each IDF */
      if ( apModPeriod[i] == (unsigned char) 255 ) apModPeriod[i] = apModPeriod[i-1];
      for (j=0; j<4; j++) {  /* for each detector */
	if ( uldArray[i][j] == -1.0 ) {
	  /* Get a new error list entry to play with. */
	  errorsListIndex++;
	  errorsList[errorsListIndex].start = i;
	  errorsList[errorsListIndex].type = 0;
	  errorsList[errorsListIndex].detector = (unsigned char)j;
	  /* Correct the bad ULD value. */
	  k = i;
	  while ( uldArray[k][j] == -1.0 ) k++;
	  errorsList[errorsListIndex].stop = k-1;
	  deltaValue = (uldArray[k][j] - uldArray[i-1][j]) / ((double)(k-i+1));
	  for (l=i; l<k; l++) {
	    uldArray[l][j] = uldArray[i-1][j] + (((double)(l-i+1)) * deltaValue);
	    /* uldArray[i][j] = uldArray[i-1][j] + deltaValue; */
	  }
	}
	if ( xuldArray[i][j] == -1.0 ) {
	  /* Get a new error list entry to play with. */
	  errorsListIndex++;
	  errorsList[errorsListIndex].start = i;
	  errorsList[errorsListIndex].type = 1;
	  errorsList[errorsListIndex].detector = (unsigned char)j;
	  /* Correct the bad XULD value. */
	  k = i;
	  while ( xuldArray[k][j] == -1.0 ) k++;
	  errorsList[errorsListIndex].stop = k-1;
	  deltaValue = (xuldArray[k][j] - xuldArray[i-1][j]) / ((double)(k-i+1));
	  for (l=i; l<k; l++) {
	    xuldArray[l][j] = xuldArray[i-1][j] + (((double)(l-i+1)) * deltaValue);
	    /* xuldArray[i][j] = xuldArray[i-1][j] + deltaValue; */
	  }
	}
      }
    }
  }

  /* open output .pha/.lc FITS file */
  if (fits_open_file(&phalcptr, infile4, READWRITE, &status))
    printerror( status, __LINE__ );

  /* attempt to move to HDU 2 in the .pha/.lc file */
  if (fits_movabs_hdu(phalcptr, 2, &hdutype, &status))
    printerror( status, __LINE__ );

  /* try to get the DEADAPP keyword from the .pha/.lc file   */
  /* if it exists and is T, then somebody already applied DT */
  /*correction to the file                                   */
  fits_read_key(phalcptr, TSTRING, "DEADAPP", (void *)keyval, keycomment, &status);
  if ( status ) {
    /* It doesn't exist or failed some other way, continue since DEADAPP == T */
    /* is the only real failure indicator.                                    */
    status = 0;  /* reset status since we _expected_ it to fail */
    fits_clear_errmsg();  /* clear the message stack to this point */
  }
  else {
    if ( strchr(keyval, (int)'T') != NULL ) {
      sprintf( errorMsg, "DEADAPP keyword exists; DEADAPP = %s / %s\nError: Deadtime correction already applied to file.", keyval, keycomment );
      Fcecho( errorMsg );
      exit(-1);
    }
  }

  /* Use the.pha/.lc file HDUCLAS1 value to determine if the */
  /* input is a .pha file or a .lc file.                     */
  if ( fits_read_key(phalcptr, TSTRING, "HDUCLAS1", (void *)keyval, keycomment, &status) )
    printerror( status, __LINE__ );

  if ( strcmp( keyval, "LIGHTCURVE" ) == 0 ) {
    typeOfPhaLcFile = lcFileType;
  }
  else {
    if ( strcmp( keyval, "SPECTRUM" ) == 0 ) {
      typeOfPhaLcFile = phaFileType;
    }
    else {
      sprintf( errorMsg, "Error: Unknown .pha/.lc file type, check last input file.  Now exiting...");
      Fcecho( errorMsg );
      exit(-1);
    }
  }

  /* Get TIMEZERO correction used for .pha/.lc file, it's in the the events list/archive data file. */
  if ( fits_read_key(eventarcptr, TDOUBLE, "TIMEZERO", (void *)&timezero, keycomment, &status) )
    printerror( status, __LINE__ );

  sprintf( errorMsg, "TIMEZERO = %lf / %s", timezero, keycomment);
  CHATTER(10);

  /* Get extension name keyword from the events list/archive data file. */
  if ( fits_read_key(eventarcptr, TSTRING, "EXTNAME", (void *)keyval, keycomment, &status) )
    printerror( status, __LINE__ );

  sprintf( errorMsg, "events list/archive data file EXTNAME = %s / %s", keyval, keycomment);
  CHATTER(10);

  /* Use the events list/archive data file EXTNAME value to determine if input */
  /* is events list-SE, binned-SA or Archive-SA data.                          */
  if ( !strcmp( keyval, "XTE_SE" ) ) {
    typeOfDeadtimeData = EventsListData;
    /* Fix bug found by Dr. M. J. Tripicco 08/28/97 - Uninitialized automatic variable. */
    offsetToTime = 0.0;  /* Assume this constant is zero. */
  }
  else {
    /* We have XTE_SA type data, either Archive or Binned Science data. */
    /* Get data mode keyword. */
    if ( fits_read_key(eventarcptr, TSTRING, "DATAMODE", (void *)keyval, keycomment, &status) )
      printerror( status, __LINE__ );
    /* Check the keyval for an "Archive" value of DATAMODE.  This is a special */
    /* type of Binned data and needs a correction to the Time value before the */
    /* IDF is calculated.                                                      */
    offsetToTime = 0.0;  /* Assume this constant is zero. */
    if ( !strcmp( keyval, "Archive" ) ) {  /* keyval is the value of DATAMODE at this point. */
      offsetToTime = -Sixteen;  /* This constant is -16.0 for Archive data. */
      typeOfDeadtimeData = ArchiveData;  /* Set type of data to Archive for the switch statement. */
    }
    else {
      typeOfDeadtimeData = BinnedData;  /* Set type of data to Binned for the switch statement. */
    }

    /* For now (3/21/97) Binned data is Archive data, later we will check the DATAMODE keyword. */
    typeOfDeadtimeData = ArchiveData;
  }

  /* Now that we know from whence the science data is drived, i.e., event or archive data, we */
  /* can do a basic check to see if we have overlap in the science and engineering data.      */

  /* First correct for 16 second offset in archive data. */
  tstartOfEvtArcFile += offsetToTime;
  tstopOfEvtArcFile  += offsetToTime;

  sprintf( errorMsg, "tstartOfEngFile = %lf tstartOfEvtArcFile = %lf\ntstopOfEngFile  = %lf tstopOfEvtArcFile  = %lf\n", tstartOfEngFile, tstartOfEvtArcFile, tstopOfEngFile, tstopOfEvtArcFile );
  CHATTER(20);

  /* Make sure that there is at least minimal overlap of the engineering and event-list/archive files. */
  if ( !((tstartOfEvtArcFile < tstopOfEngFile) || (tstopOfEvtArcFile > tstartOfEngFile)) ) {
    sprintf( errorMsg, "Error: No minimum overlap between Engineering file and Event-List/Archive File.\n\n       Check that you have used the correct files.");
    Fcecho( errorMsg );
    exit(-1);
  }

  /* Set some IDF values for use when "shifting" the ticksOfDeadTimePerIDF[][] array.  */
  /* These times have already been corrected for the arhive data offset of 16 seconds. */
  tstartOfEvtArcFileIDF = ((long)tstartOfEvtArcFile)/16;
  tstopOfEvtArcFileIDF  = ((long)tstopOfEvtArcFile)/16;

  sprintf( errorMsg, "\ntstartOfEvtArcFileIDF = %d  tstopOfEvtArcFileIDF = %d\n", tstartOfEvtArcFileIDF, tstopOfEvtArcFileIDF );
  CHATTER(20);

  /* Get the TSTART and TSTOP values from the .pha/.lc file. */
  switch ( typeOfPhaLcFile ) {
  case phaFileType:
    if ( fits_read_key(phalcptr, TDOUBLE, "TSTART", (void *)&tstartOfPhaLcFile, keycomment, &status) )
      printerror( status, __LINE__ );
    tstartOfPhaLcFile += offsetToTime;
    tstartOfPhaLcFile -= timezero;
    tstartOfPhaLcFile += (double)0.001;   /* Added by P. R. Blanco per D. Gruber */
    if ( fits_read_key(phalcptr, TDOUBLE, "TSTOP", (void *)&tstopOfPhaLcFile, keycomment, &status) )
      printerror( status, __LINE__ );
    tstopOfPhaLcFile += offsetToTime;
    tstopOfPhaLcFile -= timezero;
    tstopOfPhaLcFile -= (double)0.001;
    break;
  case lcFileType:
    /* Get the integer part of TSTART. */
    if ( fits_read_key(phalcptr, TINT, "TSTARTI", (void *)&tstartIOfPhaLcFile, keycomment, &status) )
      printerror( status, __LINE__ );
    /* Get the floating part of TSTART. */
    if ( fits_read_key(phalcptr, TDOUBLE, "TSTARTF", (void *)&tstartOfPhaLcFile, keycomment, &status) )
      printerror( status, __LINE__ );
    tstartOfPhaLcFile = ((double)tstartIOfPhaLcFile) + tstartOfPhaLcFile;
    tstartOfPhaLcFile += offsetToTime;
    tstartOfPhaLcFile -= timezero;
    tstartOfPhaLcFile += (double)0.001;   /* Added by P. R. Blanco per D. Gruber */
    /* Get the integer part of TSTOP. */
    if ( fits_read_key(phalcptr, TINT, "TSTOPI", (void *)&tstopIOfPhaLcFile, keycomment, &status) )
      printerror( status, __LINE__ );
    /* Get the floating part of TSTOP. */
    if ( fits_read_key(phalcptr, TDOUBLE, "TSTOPF", (void *)&tstopOfPhaLcFile, keycomment, &status) )
      printerror( status, __LINE__ );
    tstopOfPhaLcFile = ((double)tstopIOfPhaLcFile) + tstopOfPhaLcFile;
    tstopOfPhaLcFile += offsetToTime;
    tstopOfPhaLcFile -= timezero;
    tstopOfPhaLcFile -= (double)0.001;
    break;
  }

  sprintf( errorMsg, "tstartOfPhaLcFile = %lf  tstopOfPhaLcFile = %lf\n", tstartOfPhaLcFile, tstopOfPhaLcFile );
  CHATTER(20);

  /* Check to see if we have requested any detectors for which the data is known ot be bad. */
  for (i=0;i<4;i++) {  /* For each detector */
    if ( goneBadTime[ClusterIndex][i] != 0.0 && detRequested[i] && tstopOfPhaLcFile > goneBadTime[ClusterIndex][i] ) {
      sprintf( errorMsg, "\nWarning: Cluster %c detector #%1d has suspect spectral information for this", 0x41+ClusterIndex, i );
      Fcecho( errorMsg );
      sprintf( errorMsg, "observation because TSTOP exceeds this detector's 'Gone Bad' time.  The observer's" );
      Fcecho( errorMsg );
      sprintf( errorMsg, "handbook has more specific information on the detectors and their characteristics." );
      Fcecho( errorMsg );
      sprintf( errorMsg, "You may want to omit this detector from the live-time calculation by using the" );
      Fcecho( errorMsg );
      sprintf( errorMsg, "detectors=n..nnn parameter on the command line.\n" );
      Fcecho( errorMsg );
    }
  }

  /* Compare them to the TSTART and TSTOP of the engineering file.  It's */
  /* an error if the engineering data does not span the .pha/.lc data.   */
  if ( (tstartOfPhaLcFile < tstartOfEngFile) || (tstopOfEngFile <= tstopOfPhaLcFile) ) {
    sprintf( errorMsg, "\nWarning!  Your .pha or .lc file covers time interval\n\n%15.5f MET to %15.5f MET\n\nbut your houskeeping file, %s, only covers\n\n%15.5f MET to %15.5f MET\n\nCorrection proceeding but will use average deadtimes at end(s) of file.\n\n",
	     tstartOfPhaLcFile, tstopOfPhaLcFile, infile2, tstartOfEngFile, tstopOfEngFile );
    Fcecho( errorMsg );
  }

  /* Get NAXIS2 for number of events/IDF records in the data input file. */
  if ( fits_read_key(eventarcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
    printerror( status, __LINE__ );

  sprintf( errorMsg, "Deadtime input file NAXIS2 = %d / %s", naxis2, keycomment);
  CHATTER(10);

  /* Get data mode keyword. */
  if ( fits_read_key(eventarcptr, TSTRING, "DATAMODE", (void *)keyval, keycomment, &status) )
    printerror( status, __LINE__ );

  sprintf( errorMsg, "DATAMODE = %s / %s", keyval, keycomment);
  CHATTER(10);

  switch ( typeOfDeadtimeData ) {
  case EventsListData:
    /* ####  Beginning of deadtime collection for events list data. #### */

    /* Get column numbers for Time, LiveTime, ClstrPosition and Event keywords */
    if ( fits_get_colnum(eventarcptr, FALSE,          "Time",          &colNumTime, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE,      "LiveTime",      &colNumLiveTime, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE, "ClstrPosition", &colNumClstrPosition, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE,         "Event",         &colNumEvent, &status) )
      printerror( status, __LINE__ );

    /* parse the keyval for DATAMODE to get the bytes in the event list data. */
    tailDatamode = (int)strtol(&keyval[(int)strlen(keyval)-2], (char **)NULL, 16);

    sprintf( errorMsg, "tailDatamode = %02x", tailDatamode);
    CHATTER(10);

    eLByteCnt = 0;
    for (i=0; i<7; i++) {
      eLByte[i] = 0;
      if ( (tailDatamode << i) & 0x40 ) {
	eLByte[i] = 1;
	eLByteCnt++;
      }
    }

    /* This is actually a case that we may be able to handle at some point in the future. */
    /* The problem is that we can go through the data for an IDF and determine the four   */
    /* different deadtimes but the question is then "Which deadtime belongs to which of   */
    /* the detectors?"  For now we will dump an error message and punt.                   */
    if ( !eLByte[3] ) {  /* If detector info does not exist */
      sprintf( errorMsg, "Error: No detector info in Event List Data.  tailDatamode = %02x", tailDatamode );
      Fcecho( errorMsg );
      sprintf( errorMsg, "We are unable to correct this data at this time.  Program terminating..." );
      Fcecho( errorMsg );
      exit(-1);
    }

    detIndex = 0;
    for (i=0; i<3; i++) {
      if ( eLByte[i] )
	detIndex++;
    }

    for (i=0; i<7; i++) {
      sprintf( errorMsg, " %02x", eLByte[i]);
      CHATTER(10);
    }
    sprintf( errorMsg, "\nCount = %d  detIndex = %d", eLByteCnt, detIndex);
    CHATTER(10);

    fits_read_col_dbl(eventarcptr, colNumTime, 1L, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time of first event in list */
    oldIDF = ((long)(Time[0] + offsetToTime))/16;  /* set initial oldIDF so we collect data for first time */
    oldEventListTime = Time[0] + offsetToTime;

    /* Set a (possibly) new start for the data so that we only look at science data */
    /* that lies within the bounds of the engineering data.                         */
    recCnt=2;  /* Start at the second record since we have already read the first one. */
    while ( oldEventListTime < tstartOfEngFile ) {
      /* Read Time of next event in list. */
      if ( fits_read_col_dbl(eventarcptr, colNumTime, recCnt, 1L, 1L,
			     (double)0,       Time, &anynul, &status) ) {
	sprintf( errorMsg, "Error: While loop will be infinite at or near line #%d", ((__LINE__)-4) );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Unable to move oldEventListTime past tstartOfEngFile." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       There is no overlap between science data and engineering data." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Program exiting..." );
	Fcecho( errorMsg );
	exit(-1);
      }
      oldIDF = ((long)(Time[0] + offsetToTime))/16;  /* set initial oldIDF so we collect data for first time */
      oldEventListTime = Time[0] + offsetToTime;
      recCnt++;
    }
    realStartingRecord = recCnt - 1;
    sprintf( errorMsg, "realStartingRecord = %d\n", realStartingRecord );
    CHATTER(20);

    /* Set a (possibly) new stop for the data so that we only look at science data */
    /* that lies within the bounds of the engineering data.                        */
    fits_read_col_dbl(eventarcptr, colNumTime, naxis2, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time of last event in list */
    recCnt=naxis2 - 1;  /* Start at the penultimate record and work backwards. */
    while ( tstopOfEngFile <= (Time[0] + offsetToTime)) {
      /* Read Time of previous event in list */
      if ( fits_read_col_dbl(eventarcptr, colNumTime, recCnt, 1L, 1L,
			     (double)0,       Time, &anynul, &status) ) {
	sprintf( errorMsg, "Error: While loop will be infinite at or near line #%d", ((__LINE__)-4) );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Unable to move (Time[0] + offsetToTime to less than tstopOfEngFile." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       There is no overlap between science data and engineering data." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Program exiting..." );
	Fcecho( errorMsg );
	exit(-1);
      }
      recCnt--;
    }
    realStoppingRecord = recCnt + 1;
    sprintf( errorMsg, "realStoppingRecord = %d\n", realStoppingRecord );
    CHATTER(20);

    /* Read the Time values for the realStartingRecord and realStoppingRecord. */
    /* Compute the IDF values for those times and you then have the range of   */
    /* values in ticksOfDeadTimePerIDF[][0-3] which can be used to compute the */
    /* averages needed to extend the array.                                    */
    fits_read_col_dbl(eventarcptr,          colNumTime, realStartingRecord, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time for realStartingRecord */
    realStartingRecordIDF = ((long)(Time[0] + offsetToTime))/16;  /* Convert to IDF. */
    fits_read_col_dbl(eventarcptr,          colNumTime, realStoppingRecord, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time for realStoppingRecord */
    realStoppingRecordIDF = ((long)(Time[0] + offsetToTime))/16;  /* Convert to IDF. */


    /* Read the "first" cluster position so we have a basis for getting */
    /* the cluster pos. from the science data.                          */
    fits_read_col_byt(eventarcptr, colNumClstrPosition, realStartingRecord, 1L, 1L,
		      (unsigned char)0, oldClusterPos, &anynul, &status); /* read ClstrPosition */

    for (recCnt=realStartingRecord; recCnt<=realStoppingRecord; recCnt++) {
      fits_read_col_dbl(eventarcptr,          colNumTime, recCnt, 1L, 1L,
			(double)0,       Time, &anynul, &status); /* read Time */
      fits_read_col_lng(eventarcptr,      colNumLiveTime, recCnt, 1L, 1L,
			0L,   LiveTime, &anynul, &status); /* read LiveTime */
      fits_read_col_byt(eventarcptr, colNumClstrPosition, recCnt, 1L, 1L,
			(unsigned char)0, ClusterPos, &anynul, &status); /* read ClstrPosition */
      fits_read_col_byt(eventarcptr,         colNumEvent, recCnt, 1L, (long)eLByteCnt,
			(unsigned char)0,  EventList, &anynul, &status); /* read Event bytes */

      detId = (EventList[detIndex] >> 0x03) & 0x03;  /* get detector ID */

      newIDF = ((long)(Time[0] + offsetToTime))/16;  /* check for IDF change */
      newEventListTime = Time[0] + offsetToTime;

      if ( newIDF != oldIDF ) {
	/* We have rolled into a new IDF, i.e., the IDF for which we have been */
	/* getting the dead times is complete.  We now have to get the uld and */
	/* xuld data for the oldIDF just completed.                            */

	/* Check the first time through to see if we really have enough info */
	/* to continue.  The events list data must have been built with a    */
	/* version of the software that is new enough, or the events list    */
	/* data must include all detectors if it includes detector 0.        */
	if ( !yet_checked ) {
	  if ( XFFRevision < goodSEXFFRevision ) {  /* If a flawed version of XFF was used... */
	    if ( detIDFCorrFact[0] > 0.0 ) {  /* and detector 0 was included... */
	      /* Check to see that all of the other detectors were included too. */
	      if ( (detIDFCorrFact[1] == 0.0) || (detIDFCorrFact[2] == 0.0) || (detIDFCorrFact[3] == 0.0) ) {
		sprintf( errorMsg, "Error: Insufficient data available." );
		Fcecho( errorMsg );
		sprintf( errorMsg, "       XFF Revision # < 5.1.0 and detector 0 data present" );
		Fcecho( errorMsg );
		sprintf( errorMsg, "       without all other detectors data also available." );
		Fcecho( errorMsg );
		sprintf( errorMsg, "       Now exiting, data cannot be processed." );
		Fcecho( errorMsg );
		exit(-1);  /* Go away now, no more processing. */
	      }
	    }
	  }
	  yet_checked = 1;  /* Now we've checked it. */
	}
	/* Check to see if we have the necessary info for this IDF */
	if ( (oldIDF < index0IDFValue) || (lastValidIDFValue < oldIDF) ) {
	  sprintf( errorMsg, "Error: MET #%lf in event data is out of bounds;\nindex0TimeValue = %lf ; lastValidTimeValue = %lf ;\nprogram terminating...", oldEventListTime, index0TimeValue, lastValidTimeValue);
	  Fcecho( errorMsg );
	  exit(-1);
	}

	/* Get the uld and xuld values for the current (i.e., old) IDF */
	/* and compute the delta[i] values                             */
	for (i=0; i<4; i++) {
	  uld[i] = uldArray[oldIDF-index0IDFValue][i];
	  xuld[i] = xuldArray[oldIDF-index0IDFValue][i];
	  arm[i] = armArray[oldIDF-index0IDFValue][i];
	  trig[i] = trigArray[oldIDF-index0IDFValue][i];
	  hwVeto[i] = hwVetoArray[oldIDF-index0IDFValue][i];
	  goodEvents[i] = goodEventsArray[oldIDF-index0IDFValue][i];
	  XA_i = arm[i] - trig[i];
	  if ( XA_i < 0.0 ) XA_i = 0.0;  /* It can't be < 0 in real life. */
	  XV_i = hwVeto[i] - XA_i - xuld[i];
	  if ( XV_i < 0.0 ) XV_i = 0.0;  /* It can't be < 0 in real life. */
	  delta[i] = (AlphaCorr[i] * xuld[i]) + (GammaCorr[i] * uld[i]) +
	    (extraArmsCorrectionFactor * XA_i) + (extraVetosCorrectionFactor * XV_i);
	}

	/* fix LT for detector 0: See if det0 is sum of all detectors (old style) */
	det0LTCorrection = detIDFCorrFact[0] - detIDFCorrFact[1] - detIDFCorrFact[2] - detIDFCorrFact[3];
	if ( det0LTCorrection >= 0.0 )
	  detIDFCorrFact[0] = det0LTCorrection;

	sprintf( errorMsg, "Pre-uld/xuld  %lf %lf %lf %lf", detIDFCorrFact[0], detIDFCorrFact[1], detIDFCorrFact[2], detIDFCorrFact[3]);
	CHATTER(15);

	/* Convert the ClusterPos value into an index (as though it was read from an Engineering Data file). */

	/* Determine if this is an on-source or off-source IDF and if it */
	/* is an off-source IDF, then determine the value of f           */
	if ( ((unsigned int) oldClusterPos[0]) & 9 ) {
	  f = 1.0;
	}
	else {
	  f = nonBlankTime[apModPeriod[oldIDF-index0IDFValue]] / Sixteen;
	}

	/* Now adjust each deadtime for the uld/xuld contribution. */
	for (i=0; i<4; i++) {
	  if ( detIDFCorrFact[i] != 0.0 ) {
	    dt[i] = detIDFCorrFact[i] / ((double)524288.0);  /* Get deadtime in secs/IDF */
	    dt[i] = dt[i] + (goodEventsCorrectionFactor * goodEvents[i]) + (f * delta[i]);
	    detIDFCorrFact[i] = dt[i] * ((double)524288.0);  /* Reset deadtime as clk ticks/IDF */
	  }
	}

	sprintf( errorMsg, "pos, f, delta, P, I, 0x%2x %lf %lf %d %d", oldClusterPos[0], f, delta[0], apModPeriod[oldIDF-index0IDFValue], oldIDF);
	CHATTER(17);
	sprintf( errorMsg, "Post-uld/xuld %lf %lf %lf %lf", detIDFCorrFact[0], detIDFCorrFact[1], detIDFCorrFact[2], detIDFCorrFact[3]);
	CHATTER(15);

	/* For each detector, save the livetime counts for the current IDF number. */
	for (i=0; i<4; i++) {
	  ticksOfDeadTimePerIDF[oldIDF-index0IDFValue][i] = detIDFCorrFact[i];
	}

	/* Save 'new' values for use later. */
	oldIDF = newIDF;
	oldEventListTime = newEventListTime;
	oldClusterPos[0] = ClusterPos[0];
      }

      if ( LiveTime[0] != -1 ) {
	detIDFCorrFact[detId] = (double)LiveTime[0];
	goodEventsArray[oldIDF-index0IDFValue][detId] += 1.0;
      }
    }

    /* when we are done reading records, add in the last of the info obtained */
    /* Note: it hasn't been added yet since there has been no IDF rollover    */

    if ( (oldIDF < index0IDFValue) || (lastValidIDFValue < oldIDF) ) {
      sprintf( errorMsg, "Error: IDF #%d in event data is out of bounds; index0IDFValue = %d ; lastValidIDFValue = %d ; program terminating...\n", oldIDF, index0IDFValue, lastValidIDFValue);
      Fcecho( errorMsg );
      exit(-1);
    }

    /* Get the uld and xuld values for the current (i.e., old) IDF */
    /* and compute the delta[i] values                             */
    for (i=0; i<4; i++) {
      uld[i] = uldArray[oldIDF-index0IDFValue][i];
      xuld[i] = xuldArray[oldIDF-index0IDFValue][i];
      arm[i] = armArray[oldIDF-index0IDFValue][i];
      trig[i] = trigArray[oldIDF-index0IDFValue][i];
      hwVeto[i] = hwVetoArray[oldIDF-index0IDFValue][i];
      goodEvents[i] = goodEventsArray[oldIDF-index0IDFValue][i];
      XA_i = arm[i] - trig[i];
      if ( XA_i < 0.0 ) XA_i = 0.0;  /* It can't be < 0 in real life. */
      XV_i = hwVeto[i] - XA_i - xuld[i];
      if ( XV_i < 0.0 ) XV_i = 0.0;  /* It can't be < 0 in real life. */
      delta[i] = (AlphaCorr[i] * xuld[i]) + (GammaCorr[i] * uld[i]) +
	(extraArmsCorrectionFactor * XA_i) + (extraVetosCorrectionFactor * XV_i);
    }

    /* fix LT for detector 0: See if det0 is sum of all detectors (old style) */
    det0LTCorrection = detIDFCorrFact[0] - detIDFCorrFact[1] - detIDFCorrFact[2] - detIDFCorrFact[3];
    if ( det0LTCorrection >= 0.0 )
      detIDFCorrFact[0] = det0LTCorrection;

    sprintf( errorMsg, "Pre-uld/xuld  %lf %lf %lf %lf", detIDFCorrFact[0], detIDFCorrFact[1], detIDFCorrFact[2], detIDFCorrFact[3]);
    CHATTER(15);

    /* Convert the ClusterPos value into an index (as though it was read from an Engineering Data file). */

    /* Determine if this is an on-source or off-source IDF and if it */
    /* is an off-source IDF, then determine the value of f           */
    if ( ((unsigned int) oldClusterPos[0]) & 9 ) {
      f = 1.0;
    }
    else {
      f = nonBlankTime[apModPeriod[oldIDF-index0IDFValue]] / Sixteen;
    }

    /* Now adjust each deadtime for the uld/xuld contribution. */
    for (i=0; i<4; i++) {
      if ( detIDFCorrFact[i] != 0.0 ) {
	dt[i] = detIDFCorrFact[i] / ((double)524288.0);  /* Get deadtime in secs/IDF */
	dt[i] = dt[i] + (goodEventsCorrectionFactor * goodEvents[i]) + (f * delta[i]);
	detIDFCorrFact[i] = dt[i] * ((double)524288.0);  /* Reset deadtime as clk ticks/IDF */
      }
    }

    sprintf( errorMsg, "Post-uld/xuld %lf %lf %lf %lf", detIDFCorrFact[0], detIDFCorrFact[1], detIDFCorrFact[2], detIDFCorrFact[3]);
    CHATTER(15);

    /* For each detector, save the livetime counts for the current IDF number. */
    for (i=0; i<4; i++) {
      ticksOfDeadTimePerIDF[oldIDF-index0IDFValue][i] = detIDFCorrFact[i];
    }

    /* ####  End of deadtime collection for events list data. #### */
    break;

  case ArchiveData:
    /* ####       Beginning of deadtime collection for archive data.      #### */
    /* Try to get the BACKAPP keyword from the livetime file     */
    /* if it exists, then somebody used hxtback to get this data */
    hxtbackWasUsed = 0;  /* Assume false for now. */
    if ( !fits_read_key(eventarcptr, TSTRING, "BACKAPP", (void *)keyval, keycomment, &status) ) {
      if ( strcmp( keyval, "T") == 0 ) {
	hxtbackWasUsed = 1;  /* It was True. */
      }
    }
    status = 0;  /* Reset status whether it failed or not. */
    fits_clear_errmsg();  /* clear the message stack to this point */

    /* Get column numbers for Time, ClstrPosition, LiveTimeDet0, LiveTimeDet1, */
    /* LiveTimeDet2 and , LiveTimeDet3 keywords                                */
    if ( fits_get_colnum(eventarcptr, FALSE,          "Time",          &colNumTime, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE,       "GoodCnt",       &colNumGoodCnt, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE,  "LiveTimeDet0",  &colNumLiveTimeDet0, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE,  "LiveTimeDet1",  &colNumLiveTimeDet1, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE,  "LiveTimeDet2",  &colNumLiveTimeDet2, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE,  "LiveTimeDet3",  &colNumLiveTimeDet3, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(eventarcptr, FALSE, "ClstrPosition", &colNumClstrPosition, &status) )
      printerror( status, __LINE__ );

    /* Set a (possibly) new start for the data so that we only look at science data */
    /* that lies within the bounds of the engineering data.                         */
    fits_read_col_dbl(eventarcptr, colNumTime, 1L, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time of first data in list */
    recCnt=2;  /* Start at the second record since we have already read the first one. */
    while ( (Time[0] + offsetToTime) < tstartOfEngFile ) {
      /* Read Time of next event in list */
      if ( fits_read_col_dbl(eventarcptr, colNumTime, recCnt, 1L, 1L,
			     (double)0,       Time, &anynul, &status) ) {
	sprintf( errorMsg, "Error: While loop will be infinite at or near line #%d", ((__LINE__)-4) );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Unable to move (Time[0] + offsetToTime) past tstartOfEngFile." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       There is no overlap between science data and engineering data." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Program exiting..." );
	Fcecho( errorMsg );
	exit(-1);
      }
      recCnt++;
    }
    realStartingRecord = recCnt - 1;
    sprintf( errorMsg, "realStartingRecord = %d\n", realStartingRecord );
    CHATTER(20);

    /* Set a (possibly) new stop for the data so that we only look at science data */
    /* that lies within the bounds of the engineering data.                        */
    fits_read_col_dbl(eventarcptr, colNumTime, naxis2, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time of last event in list */
    recCnt=naxis2 - 1;  /* Start at the penultimate record and work backwards. */
    while ( tstopOfEngFile <= (Time[0] + offsetToTime)) {
      /* Read Time of previous event in list */
      if ( fits_read_col_dbl(eventarcptr, colNumTime, recCnt, 1L, 1L,
			     (double)0,       Time, &anynul, &status) ) {
	sprintf( errorMsg, "Error: While loop will be infinite at or near line #%d", ((__LINE__)-4) );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Unable to move (Time[0] + offsetToTime to less than tstopOfEngFile." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       There is no overlap between science data and engineering data." );
	Fcecho( errorMsg );
	sprintf( errorMsg, "       Program exiting..." );
	Fcecho( errorMsg );
	exit(-1);
      }
      recCnt--;
    }
    realStoppingRecord = recCnt + 1;
    sprintf( errorMsg, "realStoppingRecord = %d\n", realStoppingRecord );
    CHATTER(20);

    /* Read the Time values for the realStartingRecord and realStoppingRecord. */
    /* Compute the IDF values for those times and you then have the range of   */
    /* values in ticksOfDeadTimePerIDF[][0-3] which can be used to compute the */
    /* averages needed to extend the array.                                    */
    fits_read_col_dbl(eventarcptr,          colNumTime, realStartingRecord, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time for realStartingRecord */
    realStartingRecordIDF = ((long)(Time[0] + offsetToTime))/16;  /* Convert to IDF. */
    fits_read_col_dbl(eventarcptr,          colNumTime, realStoppingRecord, 1L, 1L,
		      (double)0,       Time, &anynul, &status); /* read Time for realStoppingRecord */
    realStoppingRecordIDF = ((long)(Time[0] + offsetToTime))/16;  /* Convert to IDF. */

    sprintf( errorMsg, "realStartingRecordIDF = %d   realStoppingRecordIDF = %d\n", realStartingRecordIDF, realStoppingRecordIDF );
    CHATTER(20);

    for (recCnt=realStartingRecord; recCnt<=realStoppingRecord; recCnt++) {
      fits_read_col_dbl(eventarcptr,          colNumTime, recCnt, 1L, 1L,
			(double)0,              Time, &anynul, &status); /* read Time */
      fits_read_col_lng(eventarcptr,       colNumGoodCnt, recCnt, 1L, 1L,
			0L,                  GoodCnt, &anynul, &status); /* read GoodCnt */
      fits_read_col_lng(eventarcptr,  colNumLiveTimeDet0, recCnt, 1L, 1L,
			0L,                 LiveTime, &anynul, &status); /* read LiveTimeDet0 */
      detIDFCorrFact[0] = (double)LiveTime[0];
      fits_read_col_lng(eventarcptr,  colNumLiveTimeDet1, recCnt, 1L, 1L,
			0L,                 LiveTime, &anynul, &status); /* read LiveTimeDet1 */
      detIDFCorrFact[1] = (double)LiveTime[0];
      fits_read_col_lng(eventarcptr,  colNumLiveTimeDet2, recCnt, 1L, 1L,
			0L,                 LiveTime, &anynul, &status); /* read LiveTimeDet2 */
      detIDFCorrFact[2] = (double)LiveTime[0];
      fits_read_col_lng(eventarcptr,  colNumLiveTimeDet3, recCnt, 1L, 1L,
			0L,                 LiveTime, &anynul, &status); /* read LiveTimeDet3 */
      detIDFCorrFact[3] = (double)LiveTime[0];
      fits_read_col_byt(eventarcptr, colNumClstrPosition, recCnt, 1L, 1L,
			(unsigned char)0, ClusterPos, &anynul, &status); /* read ClstrPosition */

      oldIDF = ((long)(Time[0] + offsetToTime))/16;  /* Set new IDF value (but use oldIDF as the variable) */
      oldArchiveDataTime = Time[0] + offsetToTime;

      sprintf( errorMsg, "%lf   oldIDF = %d    oldIDF-index0IDFValue = %d", oldArchiveDataTime, oldIDF, oldIDF-index0IDFValue );
      CHATTER(17);

      /* Check the first time through for any corrections that need to be  */
      /* made for data produced by flawed version(s) of the XFF program.   */
      if ( !yet_checked ) {
	usePreviousLivetimes = 0;  /* Assume good data. */
	if ( XFFRevision < goodSAXFFRevision ) {  /* If a flawed version of XFF was used... */
	  sprintf( errorMsg, "Error: Analysing SA type data." );
	  Fcecho( errorMsg );
	  sprintf( errorMsg, "       XFF Revision # < 5.3.1" );
	  Fcecho( errorMsg );
	  sprintf( errorMsg, "       Continuing with analysis until error processing can be established." );
	  Fcecho( errorMsg );
	  usePreviousLivetimes = 1;  /* If older data, livetimes were shifted backwards in time. */
	  if ( hxtbackWasUsed ) {
	    sprintf( errorMsg, "Error: BACKAPP keyword is True; Deadtime correction will not be correct.  Exiting...");
	    Fcecho( errorMsg );
	    exit(-1);
	  }
	}
	yet_checked = 1;  /* Now we've checked it. */
      }

      /* Check to see if we have the necessary info for this IDF */
      if ( (oldIDF < index0IDFValue) || (lastValidIDFValue < oldIDF) ) {
	sprintf( errorMsg, "\nTime Error: MET #%lf in Archive data is out of bounds of available ULD/XULD data;\nindex0TimeValue = %lf ; lastValidTimeValue = %lf\n", oldArchiveDataTime, index0TimeValue, lastValidTimeValue);
	CHATTER(15);
	/* For each detector, save the livetime counts for possible use in the next IDF. */
	for (i=0; i<4; i++)
	  {
	    detIDFCorrFactSave[i] = detIDFCorrFact[i];
	  }
      }
      else {
	/* Get the uld and xuld values for the current (i.e., old) IDF */
	/* and compute the delta[i] values                             */
	for (i=0; i<4; i++) {
	  uld[i] = uldArray[oldIDF-index0IDFValue][i];
	  xuld[i] = xuldArray[oldIDF-index0IDFValue][i];
	  arm[i] = armArray[oldIDF-index0IDFValue][i];
	  trig[i] = trigArray[oldIDF-index0IDFValue][i];
	  hwVeto[i] = hwVetoArray[oldIDF-index0IDFValue][i];
	  goodEvents[i] = ((double)GoodCnt[0]) / 4.0;
	  XA_i = arm[i] - trig[i];
	  if ( XA_i < 0.0 ) XA_i = 0.0;  /* It can't be < 0 in real life. */
	  XV_i = hwVeto[i] - XA_i - xuld[i];
	  if ( XV_i < 0.0 ) XV_i = 0.0;  /* It can't be < 0 in real life. */
	  delta[i] = (AlphaCorr[i] * xuld[i]) + (GammaCorr[i] * uld[i]) +
	    (extraArmsCorrectionFactor * XA_i) + (extraVetosCorrectionFactor * XV_i);
	}

	/* fix LT for detector 0: See if det0 is sum of all detectors (old style) */
	det0LTCorrection = detIDFCorrFact[0] - detIDFCorrFact[1] - detIDFCorrFact[2] - detIDFCorrFact[3];
	if ( det0LTCorrection >= 0.0 ) {
	  detIDFCorrFact[0] = det0LTCorrection;
	}

	if ( usePreviousLivetimes ) {  /* Use the array detIDFCorrFactSave for the livetime data. */
	  sprintf( errorMsg, "Save Pre-uld/xuld  %lf %lf %lf %lf", detIDFCorrFactSave[0], detIDFCorrFactSave[1], detIDFCorrFactSave[2], detIDFCorrFactSave[3]);
	  CHATTER(15);
	}
	else {
	  sprintf( errorMsg, "Std. Pre-uld/xuld  %lf %lf %lf %lf", detIDFCorrFact[0], detIDFCorrFact[1], detIDFCorrFact[2], detIDFCorrFact[3]);
	  CHATTER(15);
	}

	/* Convert the ClusterPos value into an index (as though it was read from an Engineering Data file). */
	/* Determine if this is an on-source or off-source IDF and if it */
	/* is an off-source IDF, then determine the value of f           */
	sprintf( errorMsg, "ClusterPos[0] = %d", ClusterPos[0] );
	CHATTER(15);
	if ( (ClusterPos[0] == 0) || (ClusterPos[0] == 3) ) {
	  f = 1.0;
	}
	else {
	  f = nonBlankTime[apModPeriod[oldIDF-index0IDFValue]] / Sixteen;
	}

	if ( usePreviousLivetimes ) {  /* Use the array detIDFCorrFactSave for the livetime data. */
	  /* Now adjust each deadtime for the uld/xuld contribution. */
	  for (i=0; i<4; i++) {
	    if ( detIDFCorrFactSave[i] != 0.0 ) {
	      dt[i] = detIDFCorrFactSave[i] / ((double)524288.0);  /* Get deadtime in secs/IDF */
	      dt[i] = dt[i] + (goodEventsCorrectionFactor * goodEvents[i]) + (f * delta[i]);
	      detIDFCorrFactSave[i] = dt[i] * ((double)524288.0);  /* Reset deadtime as clk ticks/IDF */
	    }
	  }

	  sprintf( errorMsg, "pos, f, delta, P, I, 0x%2x %lf %lf %d %d", ClusterPos[0], f, delta[0], apModPeriod[oldIDF-index0IDFValue], oldIDF);
	  CHATTER(17);

	  sprintf( errorMsg, "Save Post-uld/xuld %lf %lf %lf %lf\n", detIDFCorrFactSave[0], detIDFCorrFactSave[1], detIDFCorrFactSave[2], detIDFCorrFactSave[3]);
	  CHATTER(15);

	  /* For each detector, save the livetime counts for the current IDF number. */
	  for (i=0; i<4; i++) {
	    if ( (oldIDF-index0IDFValue) >= 0 ) {
	      ticksOfDeadTimePerIDF[oldIDF-index0IDFValue][i] = detIDFCorrFactSave[i];
	    }
	    /* This is the save for XFF Revision # < 5.3.1 so we can use these livetimes */
	    /* on the next iteration.                                                    */
	    detIDFCorrFactSave[i] = detIDFCorrFact[i];
	  }
	}
	else {  /* Use the array detIDFCorrFact for the livetime data. */
	  /* Now adjust each deadtime for the uld/xuld contribution. */
	  for (i=0; i<4; i++) {
	    if ( detIDFCorrFact[i] != 0.0 ) {
	      dt[i] = detIDFCorrFact[i] / ((double)524288.0);  /* Get deadtime in secs/IDF */
	      dt[i] = dt[i] + (goodEventsCorrectionFactor * goodEvents[i]) + (f * delta[i]);
	      detIDFCorrFact[i] = dt[i] * ((double)524288.0);  /* Reset deadtime as clk ticks/IDF */
	    }
	  }

	  sprintf( errorMsg, "pos, f, delta, P, I, 0x%2x %lf %lf %d %d", ClusterPos[0], f, delta[0], apModPeriod[oldIDF-index0IDFValue], oldIDF);
	  CHATTER(17);
	  sprintf( errorMsg, "Std. Post-uld/xuld %lf %lf %lf %lf\n", detIDFCorrFact[0], detIDFCorrFact[1], detIDFCorrFact[2], detIDFCorrFact[3]);
	  CHATTER(15);

	  /* For each detector, save the livetime counts for the current IDF number. */
	  for (i=0; i<4; i++) {
	    if ( (oldIDF-index0IDFValue) >= 0 ) {
	      ticksOfDeadTimePerIDF[oldIDF-index0IDFValue][i] = detIDFCorrFact[i];
	    }
	  }
	}
      }
    }

    /* ####  End of deadtime collection for archive data. #### */
    break;
  case BinnedData:
    /* ####       Beginning of deadtime collection for Binned Science data.      #### */
    sprintf( errorMsg, "Binned Science Data _NOT_ currently supported." );
    Fcecho( errorMsg );
    exit(-1);  /* Go away now, no more processing. */
    /* ####  End of deadtime collection for Binned Science data. #### */
    break;
  }

  for (i=0; i<=oldIDF-index0IDFValue; i++) {
    for (j=0; j<4; j++) {
      sprintf( errorMsg, "ticksOfDeadTimePerIDF[%d][%d] = %lf ", i, j, ticksOfDeadTimePerIDF[i][j] );
      CHATTER(10);
    }
  }
  sprintf( errorMsg, "\n oldIDF-index0IDFValue = %d", oldIDF-index0IDFValue );
  CHATTER(10);

  /* For the ticksOfDeadTimePerIDF[][] array, compute the average and use it to   */
  /* extend the array in both directions, i.e., from 0 to (realStartingRecordIDF  */
  /* - index0IDFValue) and from (realStoppingRecordIDF-index0IDFValue+1) to 8192. */
  /* First compute the averages. */
  for (j=0; j<4; j++) {
    avgTicksOfDeadTimePerIDF[j] = 0.0;
  }
  for (i=realStartingRecordIDF-index0IDFValue; i<=realStoppingRecordIDF-index0IDFValue; i++) {
    for (j=0; j<4; j++) {
      avgTicksOfDeadTimePerIDF[j] += ticksOfDeadTimePerIDF[i][j];
    }
  }
  for (j=0; j<4; j++) {
    avgTicksOfDeadTimePerIDF[j] /= (double)(realStoppingRecordIDF-realStartingRecordIDF+1);
  }
  /* Now replicate the average in the unfilled portions of ticksOfDeadTimePerIDF[][]. */
  for (i=0; i<realStartingRecordIDF-index0IDFValue; i++) {  /* Beginning. */
    for (j=0; j<4; j++) {
      ticksOfDeadTimePerIDF[i][j] = avgTicksOfDeadTimePerIDF[j];
    }
  }
  for (i=realStoppingRecordIDF-index0IDFValue+1; i<8192; i++) {  /* End */
    for (j=0; j<4; j++) {
      ticksOfDeadTimePerIDF[i][j] = avgTicksOfDeadTimePerIDF[j];
    }
  }

  /* Now, if necessary, "shift" the data in the ticksOfDeadTimePerIDF[][] array to accomodate */
  /* cases where the evt/arc data (i.e., the time of the pha/lc file) starts before the eng   */
  /* data file.  Adjust the index0IDFValue so that any further references will work.          */
  if ( tstartOfEvtArcFileIDF < index0IDFValue ) {
    shift = index0IDFValue - tstartOfEvtArcFileIDF;
    /* Shift the data towards the end of the array.  Go backwards to handle the overlap. */
    for (i=realStoppingRecordIDF-index0IDFValue; i>=0; i--) {
      for (j=0; j<4; j++) {
	ticksOfDeadTimePerIDF[i+shift][j] = ticksOfDeadTimePerIDF[i][j];
      }
    }
    /* Now refill the beginning of the array with average values. */
    for (i=0; i<shift; i++) {
      for (j=0; j<4; j++) {
	ticksOfDeadTimePerIDF[i][j] = avgTicksOfDeadTimePerIDF[j];
      }
    }
    /* Adjust the index0 value so subsequent calculations will work. */
    index0IDFValue -= shift;
  }
  sprintf( errorMsg, "New index0IDFValue = %d", index0IDFValue );
  CHATTER(20);

  /* For each detector, check that the livetime counts are non-zero somewhere */
  /* and if so, then inc the number of detectors to be used in averaging the  */
  /* livetimes.                                                               */
  numberOfValidDetectors = 0.0;
  for (i=0; i<4; i++) {
    detectorHasLivetimeCounts[i] = FALSE;
    detectorIsToBeCounted = FALSE;
    for (j=0; j<(oldIDF-index0IDFValue+1); j++) {
      if ( ticksOfDeadTimePerIDF[j][i] > 0.0 ) {
	detectorIsToBeCounted = TRUE;
      }
    }
    if ( detRequested[i] ) {  /* If the detector was requested... */
      if ( detectorIsToBeCounted ) {  /* If the detector has lt counts... */
	numberOfValidDetectors += 1.0;
	detectorHasLivetimeCounts[i] = TRUE;
      }
      else {  /* The requested detector has no livetime counts so print a warning. */
	sprintf( errorMsg, "Warning: Detector %1d was requested but it has no livetime counts.  This\n", i );
	Fcecho( errorMsg );
	sprintf( errorMsg, "         detector will not average into the livetime average computation.\n" );
	Fcecho( errorMsg );
      }
    }
  }

  sprintf( errorMsg, "numberOfValidDetectors = %d", (int)numberOfValidDetectors );
  CHATTER(10);

  /* Now switch on the type of file that is to be modified. */
  switch ( typeOfPhaLcFile ) {
  case phaFileType:
    /* Get the HDUCLAS3 keyword from the .pha file. */
    fits_read_key(phalcptr, TSTRING, "HDUCLAS3", (void *)hduclas3, keycomment, &status);
    if ( status ) {
      /* If it doesn't exist or failed some other way, we cannot continue. */
      sprintf( errorMsg, "HDUCLAS3 keyword does not exist!  No further processing possible." );
      Fcecho( errorMsg );
      furtherProcessingOk = FALSE;
    }

    if ( strcmp(hduclas3, "TYPE:II") == 0 ) {  /* We have a multiple pha case here (from archive data). */
      /* Get the HDUCLAS4 keyword from the .pha file. */
      fits_read_key(phalcptr, TSTRING, "HDUCLAS4", (void *)hduclas4, keycomment, &status);
      if ( status ) {
	/* If it doesn't exist or failed some other way, we cannot continue. */
	sprintf( errorMsg, "For HDUCLAS3 == TYPE:II, the HDUCLAS4 keyword read failed!  No further processing possible." );
	Fcecho( errorMsg );
	furtherProcessingOk = FALSE;
      }
    }

    if ( strcmp(hduclas3, "TYPE:II") != 0 ) {  /* HDUCLAS3 == TYPE:II means the ROWID info is in a data column     */
                                               /* not in a set of keywords, so do this check only if _NOT_ TYPE:II */
      /* Check the .pha file for ROWIDn, n=1,4 and set the validity array. */
      fits_read_key(phalcptr, TSTRING, "ROWID1", (void *)rowid1, keycomment, &status);
      if ( status ) {
	/* If it doesn't exist or failed some other way, we cannot continue. */
	sprintf( errorMsg, "For HDUCLAS3 != TYPE:II, the ROWID1 keyword read failed!  No further processing possible." );
	Fcecho( errorMsg );
	furtherProcessingOk = FALSE;
	break;
      }
      if ( (strcmp(rowid1, "Event") != 0) && (strcmp(rowid1, "TimSerCnt") != 0) ) {  /* First one is _NOT_ Event/TimSerCnt. */
	numberOfValidDetectorsInPhaLcFile = 0.0;
	/* For each of the possible detectors to be detected :) */
	for (nfield=1; nfield<=4; nfield++) {
	  /* Create the ROWIDn string. */
	  rowidNameString[5] = (char)(0x30 + nfield);
	  /* Get the ROWIDn keyword from the .pha file putting it in the rowidn variable. */
	  fits_read_key(phalcptr, TSTRING, rowidNameString, (void *)rowidn, keycomment, &status);
	  if ( status ) {
	    /* If it doesn't exist then we are done looking for detectors. */
	    status = 0;  /* reset status since we _expected_ it to fail */
	    fits_clear_errmsg();  /* clear the message stack to this point */
	    nfield = 5;  /* This kicks us out of the local for{} statement. */
	  }
	  else {
	    numberOfValidDetectorsInPhaLcFile += 1.0;
	    /* Extract the actual detector number from the keyword value. */
	    detectorNumber = rowidn[7] - 0x30;
	    /* Mark the actual detector as a contributor. */
	    detectorIsValid[detectorNumber] = TRUE;
	  }
	}
      }
      else {
	/* For TimSerCnt all detectors were used. */
	numberOfValidDetectorsInPhaLcFile = 4.0;
	for (nfield=0; nfield<4; nfield++) {
	  detectorIsValid[nfield] = TRUE;
	}
      }
      /* Report which detectors will actually be used in the livetime computation. */
      /* This means the detector must have been requested, it must have livetime   */
      /* counts and it must be in the pha/lc file. Due to the livetime check, the  */
      /* variable detectorHasLivetimeCounts[i] can only be true if detRequested[i] */
      /* is true.                                                                  */
      validDetectorString[2] = (char)(ClusterIndex + 0x41);  /* validDetectorString[] is now "PWA" or "PWB". */
      j = 3;  /* Start appending here. */
      k = 0;
      for (i=0;i<4;i++){
	if ( detectorHasLivetimeCounts[i] && detectorIsValid[i] ) {
	  validDetectorString[j++] = (char)(i + 0x30);
	  detectorsInLivetimeString[k] = (char)(i + 0x30);
	  k += 2;
	}
      }
      sprintf( errorMsg, "Detectors to be used in livetime calculation: %s", detectorsInLivetimeString );
      Fcecho( errorMsg );
      if (fits_write_history(phalcptr, errorMsg, &status))
	printerror( status, __LINE__ );
      if ( strcmp( &validDetectorString[3], "0123" ) == 0 ) {
	validDetectorString[3] = '\0';  /* End the string after PWA or PWB. */
      }
      if ( fits_update_key_str(phalcptr, "DETNAM", validDetectorString, "Detectors used in livetime calculation", &status) )
	printerror( status, __LINE__ );
    }

    /* Find the last HDU in the .pha file.  The assumption is that     */
    /* this is the correct GTI info for the file resides.  To do this  */
    /* attempt to move beyond the last GTI extension.  When failure is */
    /* detected at HDU i, then the correct HDU is i-1.                 */
    for (i=3; i<100; i++ ) {
      fits_movabs_hdu(phalcptr, i, &hdutype, &status);
      if ( status ) {
	/* It doesn't exist or failed some other way, continue since */
	/* this is what we wanted.                                   */
	status = 0;  /* reset status since we _expected_ it to fail */
	fits_clear_errmsg();  /* clear the message stack to this point */
	break;
      }
    }
    i--;  /* This one was bad, so the last one *was* the last good one. */


    /* Attempt to move to the last HDU in the .pha/.lc file.  This is   */
    /* where we think we will find the goodtime extensions to the file. */
    /* These start and stop times will be used to get the actual live-  */
    /* times which will be averaged and used to adjust the EXPOSURE     */
    /* keyword.                                                         */
    if (fits_movabs_hdu(phalcptr, i, &hdutype, &status))
      printerror( status, __LINE__ );

    /* Get column numbers for START and STOP keywords */
    if ( fits_get_colnum(phalcptr, FALSE, "START", &colNumSTART, &status) )
      printerror( status, __LINE__ );
    if ( fits_get_colnum(phalcptr, FALSE, "STOP", &colNumSTOP, &status) )
      printerror( status, __LINE__ );

    /* Now get the number of time pairs (NAXIS2) to be used. */
    if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
      printerror( status, __LINE__ );

    sprintf( errorMsg, "good time pairs NAXIS2 = %d / %s", naxis2, keycomment);
    CHATTER(10);

    for (recCnt=1; recCnt<=naxis2; recCnt++) {
      fits_read_col_dbl(phalcptr, colNumSTART,  recCnt, 1L, 1L,
			(double)0,       Start, &anynul, &status);  /* read START */
      fits_read_col_dbl(phalcptr, colNumSTOP, recCnt, 1L, 1L,
			(double)0,        Stop, &anynul, &status);  /* read STOP  */
      goodTimeStart[recCnt] = Start[0] + offsetToTime - timezero;
      goodTimeStop[recCnt]  = Stop[0] + offsetToTime - timezero;
    }
    sprintf( errorMsg, "Done collecting good time start/stop pairs");
    CHATTER(10);

    /* Now, using the good time start/stop pairs, compute the average livetime for */
    /* the data in the .pha file.                                                  */

    sumovl = 0.0;
    sumlt = 0.0;
    for (j=0; j<4; j++) {
      sumovlPerDetector[j] = 0.0;
      sumltPerDetector[j] = 0.0;
    }

    realNumberOfValidDetectors = (numberOfValidDetectorsInPhaLcFile <= numberOfValidDetectors) ?
      numberOfValidDetectorsInPhaLcFile : numberOfValidDetectors;

    for (recCnt=1; recCnt<=naxis2; recCnt++) {
      startTimeOfGoodTimeInterval = goodTimeStart[recCnt];
      stopTimeOfGoodTimeInterval  = goodTimeStop[recCnt];

      /* Get start time of IDF containing starting bin time */
      p0 = ((double)(((long)startTimeOfGoodTimeInterval)/16)) * Sixteen;

      maxIDFsSpanned = (int)(ceil((stopTimeOfGoodTimeInterval - startTimeOfGoodTimeInterval)/((double)16.0)) + (double)1.0);

      /* Initialize some presumptive segments. */
      for (i=0; i<maxIDFsSpanned; i++) {
	pStart[i] = p0 + (Sixteen * ((double)i));
	pStop[i]  = pStart[i] + Sixteen;
      }
      pStart[0] = startTimeOfGoodTimeInterval;

      /* Now adjust them based on what we know about the bin. */
      for (i=0; i<maxIDFsSpanned; i++) {
	if ( pStop[i] >= stopTimeOfGoodTimeInterval ) pStop[i] = stopTimeOfGoodTimeInterval;
	if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
      }

      for (i=0; i<maxIDFsSpanned; i++) {
	ovl = pStop[i] - pStart[i];

	/* Compute average lt for the number of valid detectors used in the .pha file. */
	lt = 0.0;
	for (j=0; j<4; j++) {
	  /* Save the individual detector data. */
	  ltPerDetector[j] = ticksOfDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue][j];
	  if ( detectorIsValid[j] && detectorHasLivetimeCounts[j] ) {
	    /* Adds zero if detector is not present in the data. */
	    /* We only add in a detector used in the .pha file and which has been requested. */
	    lt += ltPerDetector[j];
	  }
	}
	lt = lt / realNumberOfValidDetectors;

	if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
	  sumovl += ovl;
	  sumlt += ovl * lt;
	}
	for (j=0; j<4; j++) {  /* Do the individual detectors. */
	  if ( ltPerDetector[j] > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
	    sumovlPerDetector[j] += ovl;
	    sumltPerDetector[j] += ovl * ltPerDetector[j];
	  }
	}
      }

      sprintf( errorMsg, "GTI Start = %lf  GTI Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfGoodTimeInterval, stopTimeOfGoodTimeInterval, maxIDFsSpanned, sumlt, sumovl );
      CHATTER(15);
    }

    /* We do not want to divide by zero, so check it out here. */
    if ( sumovl > 0.0 ) {
      /* Compute the deadtime fraction.  This is an average percentage of time the */
      /* detectors are dead during the time in the good time intervals.            */
      dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);  /* This is the average of the sum */
    }
    else {
      if ( sumlt != 0.0 ) {
	sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
	CHATTER(2);
      }
      else {
	/* sumlt == 0.0 AND sumovl == 0.0 */
	dtfract[0] = 0.0;
	sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
	CHATTER(10);
      }
    }

    sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
    CHATTER(10);

    /* The multiplication factor is (1.0 - dtfract[0]), i.e., LIVE time fraction. */
    multFactor = 1.0 - dtfract[0];

    sprintf( errorMsg, "multFactor = %lf", multFactor);
    CHATTER(10);

    /* Compute the per detector deadtime fractions and multiplier factors. */
    for (j=0; j<4; j++) {  /* Do the average for each detector. */
      if ( sumovlPerDetector[j] > 0.0 ) {
	/* Compute the deadtime fraction.  This is an average percentage of time */
	/* the detector is dead during the time in the good time intervals.      */
	dtfractPerDetector[j] = (sumltPerDetector[j] / sumovlPerDetector[j]) / ((double)8388608.0);
      }
      else {
	if ( sumltPerDetector[j] != 0.0 ) {
	  sprintf( errorMsg, "Suspicious computation: sumovlPerDetector[%d] == 0.0, sumltPerDetector[%d] != 0.0 in trying to compute dtfractPerDetector[%d]", j, j, j );
	  CHATTER(2);
	}
	else {
	  /* sumltPerDetector[j] == 0.0 AND sumovlPerDetector[j] == 0.0 */
	  dtfractPerDetector[j] = 0.0;
	  sprintf( errorMsg, "sumltPerDetector[%d] == 0.0 AND sumovlPerDetector[%d] == 0.0 in trying to compute dtfractPerDetector[%d].  dtfractPerDetector[%d] set to 0.0", j, j, j, j );
	  CHATTER(10);
	}
      }

      sprintf( errorMsg, "dtfractPerDetector[%d] = (sumltPerDetector[%d] / sumovlPerDetector[%d]) / ((double)8388608.0) = %lf", j, j, j, dtfractPerDetector[j] );
      CHATTER(10);

      /* The multiplication factor is (1.0 - deadtimefraction), i.e., LIVE time fraction. */
      multFactorPerDetector[j] = 1.0 - dtfractPerDetector[j];

      sprintf( errorMsg, "multFactorPerDetector[%d] = %lf", j, multFactorPerDetector[j]);
      CHATTER(10);
    }

    /* Reset to look at the correct HDU for the data, presumed always to be 2. */
    if (fits_movabs_hdu(phalcptr, 2, &hdutype, &status))
      printerror( status, __LINE__ );

    /* Get the current exposure since we will correct it for all .pha files. */
    fits_read_key(phalcptr, TFLOAT, "EXPOSURE", (void *)&exposure, keycomment, &status);

    sprintf( errorMsg, "Current  EXPOSURE = %f", exposure);
    CHATTER(5);

    oldexposure = exposure;

    if ( furtherProcessingOk ) {
      /* Create switch indices for the HDUCLAS3 and HDUCLAS4 keywords. */
      for (i=0; i<4; i++) {
	if ( strcmp(hduclas3, hduclasNames[i]) == 0 ) {
	  hduclas3IntIndex = i;
	}
	if ( strcmp(hduclas4, hduclasNames[i]) == 0 ) {
	  hduclas4IntIndex = i;
	}
      }
      /* Now we have to check and see just what kind of .pha file we have, i.e., was  */
      /* it derived from an events list file or an archive file; was it a COUNT, MEAN */
      /* or RATE; how many detectors and which ones were used.                        */
      switch ( typeOfDeadtimeData ) {  /* First, switch on event list or archive data. */
      case EventsListData:
	switch ( hduclas3IntIndex ) {  /* Switch on HDUCLAS3 keyword value. */
	case COUNT:
	case MEAN:
	  /* Adjust the exposure time by the livetime correction for all detectors used in the .pha file. */
	  exposure = (float)((double)exposure * multFactor);

	  sprintf( errorMsg, "Adjusted EXPOSURE = %f", exposure);
	  CHATTER(5);

	  /* Rewrite the exposure keyword in the .pha file HDU 2 header area */
	  if ( fits_update_key_fixflt(phalcptr, "EXPOSURE", exposure, 3, keycomment, &status) )
	    printerror( status, __LINE__ );
	  deadTimesUpdated = TRUE;
	  break;
	case RATE:
	  /* Adjust the exposure time by the livetime correction for all detectors used in the .pha file. */
	  exposure = (float)((double)exposure * multFactor);

	  sprintf( errorMsg, "Adjusted EXPOSURE = %f", exposure);
	  CHATTER(5);

	  /* Rewrite the exposure keyword in the .pha file HDU 2 header area */
	  if ( fits_update_key_fixflt(phalcptr, "EXPOSURE", exposure, 3, keycomment, &status) )
	    printerror( status, __LINE__ );

	  /* Adjust the columns by the multFactor as we do with a .lc file. This means */
	  /* changing the multFactor in place.                                         */
	  multFactor = 1.0 / multFactor;  /* For rates use mf = 1/(1-dtfract) */

	  /* Get the NAXIS2 value for the number of channels. */
	  if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	    printerror( status, __LINE__ );

	  sprintf( errorMsg, "event list data derived .pha file with HDUCLAS3 == RATE, NAXIS2 = %d / %s", naxis2, keycomment);
	  CHATTER(10);

	  /* Get column number for RATE and STAT_ERR keywords */
	  if ( fits_get_colnum(phalcptr, FALSE,     "RATE",     &colNumRATE, &status) )
	    printerror( status, __LINE__ );
	  if ( fits_get_colnum(phalcptr, FALSE, "STAT_ERR", &colNumSTAT_ERR, &status) )
	    printerror( status, __LINE__ );

	  for (recCnt=1; recCnt<=naxis2; recCnt++) {
	    fits_read_col_dbl(phalcptr,     colNumRATE,  recCnt, 1L, 1L,
			      (double)0,       Rate, &anynul, &status);  /* read RATE  */
	    fits_read_col_dbl(phalcptr, colNumSTAT_ERR, recCnt, 1L, 1L,
			      (double)0,      Error, &anynul, &status);  /* read STAT_ERR */

	    /* sprintf( errorMsg, " Current  RATE = %lf   Current ERROR = %lf", Rate[0], Error[0] );
	    CHATTER(5); */

	    /* Alter the RATE and STAT_ERR column values. */
	    Rate[0] = Rate[0] * multFactor;
	    Error[0] = Error[0] * multFactor;

	    /* sprintf( errorMsg, "Adjusted  RATE = %lf  Adjusted ERROR = %lf", Rate[0], Error[0] );
	    CHATTER(5); */

	    /* Write and re-write column values. */
	    fits_write_col_dbl(phalcptr,     colNumRATE, recCnt, 1L, 1L,    Rate, &status); /* write  RATE */
	    fits_write_col_dbl(phalcptr, colNumSTAT_ERR, recCnt, 1L, 1L,   Error, &status); /* write  STAT_ERR */
	  }
	  deadTimesUpdated = TRUE;
	  break;
	case TYPEII:
	  sprintf( errorMsg, "HDUCLAS3 == TYPE:II is illegal keyword value for event list derived pha file." );
	  Fcecho( errorMsg );
	  break;
	default:
	  Fcecho(" ");
	  sprintf( errorMsg, "Default case not valid.  Line %d", __LINE__ );
	  Fcecho( errorMsg );
	  exit(-1);
	}
	break;

      case ArchiveData:
	switch ( hduclas3IntIndex ) {  /* Switch on HDUCLAS3 keyword value. */
	case COUNT:
	case MEAN:
	  /* Adjust the exposure time by the livetime correction for all detectors used in the .pha file. */
	  exposure = (float)((double)exposure * multFactor);

	  sprintf( errorMsg, "Adjusted EXPOSURE = %f", exposure);
	  CHATTER(5);

	  /* Rewrite the exposure keyword in the .pha file HDU 2 header area */
	  if ( fits_update_key_fixflt(phalcptr, "EXPOSURE", exposure, 3, keycomment, &status) )
	    printerror( status, __LINE__ );
	  deadTimesUpdated = TRUE;
	  break;
	case RATE:
	  /* Adjust the exposure time by the livetime correction for all detectors used in the .pha file. */
	  exposure = (float)((double)exposure * multFactor);

	  sprintf( errorMsg, "Adjusted EXPOSURE = %f", exposure);
	  CHATTER(5);

	  /* Rewrite the exposure keyword in the .pha file HDU 2 header area */
	  if ( fits_update_key_fixflt(phalcptr, "EXPOSURE", exposure, 3, keycomment, &status) )
	    printerror( status, __LINE__ );

	  /* Adjust the columns by the multFactor as we do with a .lc file. This means */
	  /* changing the multFactor in place.                                         */
	  multFactor = 1.0 / multFactor;  /* For rates use mf = 1/(1-dtfract) */

	  /* Get the NAXIS2 value for the number of channels. */
	  if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	    printerror( status, __LINE__ );

	  sprintf( errorMsg, "archive data derived .pha file with HDUCLAS3 == RATE, NAXIS2 = %d / %s", naxis2, keycomment);
	  CHATTER(10);

	  /* Get column number for RATE and STAT_ERR keywords */
	  if ( fits_get_colnum(phalcptr, FALSE,     "RATE",  &colNumRATE, &status) )
	    printerror( status, __LINE__ );
	  if ( fits_get_colnum(phalcptr, FALSE, "STAT_ERR", &colNumSTAT_ERR, &status) )
	    printerror( status, __LINE__ );

	  for (recCnt=1; recCnt<=naxis2; recCnt++) {
	    fits_read_col_dbl(phalcptr,     colNumRATE, recCnt, 1L, 1L,
			      (double)0,       Rate, &anynul, &status);  /* read RATE  */
	    fits_read_col_dbl(phalcptr, colNumSTAT_ERR, recCnt, 1L, 1L,
			      (double)0,      Error, &anynul, &status);  /* read STAT_ERR */

	    /* sprintf( errorMsg, " Current  RATE = %lf   Current ERROR = %lf", Rate[0], Error[0] );
	    CHATTER(5); */

	    /* Alter the RATE and STAT_ERR column values. */
	    Rate[0] = Rate[0] * multFactor;
	    Error[0] = Error[0] * multFactor;

	    /* sprintf( errorMsg, "Adjusted  RATE = %lf  Adjusted ERROR = %lf", Rate[0], Error[0] );
	    CHATTER(5); */

	    /* Write and re-write column values. */
	    fits_write_col_dbl(phalcptr,     colNumRATE, recCnt, 1L, 1L,    Rate, &status); /* write  RATE */
	    fits_write_col_dbl(phalcptr, colNumSTAT_ERR, recCnt, 1L, 1L,   Error, &status); /* write  STAT_ERR */
	  }
	  deadTimesUpdated = TRUE;
	  break;
	case TYPEII:
	  switch ( hduclas4IntIndex ) {  /* HDUCLAS3 is TYPE:II so switch on HDUCLAS4 keyword value. */
	  case COUNT:
	  case MEAN:
	    /* Get column number for ROWID keyword */
	    if ( fits_get_colnum(phalcptr, FALSE, "ROWID", &colNumROWID, &status) )
	      printerror( status, __LINE__ );

	    /* Get the NAXIS2 value for the number of channels. */
	    if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	      printerror( status, __LINE__ );

	    /* Add up the multFactorPerDetector[detectorNumber] values for */
	    /* all utilized detectors.                                     */
	    sum = 0.0;
	    for (recCnt=1; recCnt<=naxis2; recCnt++) {
	      /* Get the ROWID value from the .pha file. */
	      fits_read_col_str(phalcptr, colNumROWID,  recCnt, 1L, 1L,
				"", rowid, &anynul, &status);  /* Read ROWID */
	      detectorNumber = rowid[0][7] - 0x30;
	      sum += multFactorPerDetector[detectorNumber];
	    }
	    exposure = (float) ((double)exposure * (sum / ((double)naxis2)));

	    sprintf( errorMsg, "Adjusted EXPOSURE = %f", exposure);
	    CHATTER(5);

	    /* Rewrite the exposure keyword in the .pha file HDU 2 header area */
	    if ( fits_update_key_fixflt(phalcptr, "EXPOSURE", exposure, 3, keycomment, &status) )
	      printerror( status, __LINE__ );
	    deadTimesUpdated = TRUE;
	    break;
	  case RATE:
	    /* Get column number for ROWID keyword */
	    if ( fits_get_colnum(phalcptr, FALSE, "ROWID", &colNumROWID, &status) )
	      printerror( status, __LINE__ );
	    /* Get the NAXIS2 value for the number of detectors used. */
	    if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	      printerror( status, __LINE__ );
	    /* Get the DETCHANS value for the number of channels. This */
	    /* gives channels per Row == Detector to adjust.           */
	    if ( fits_read_key(phalcptr, TINT, "DETCHANS", (void *)&detchans, keycomment, &status) )
	      printerror( status, __LINE__ );
	    /* Get column number for RATE and STAT_ERR keywords */
	    if ( fits_get_colnum(phalcptr, FALSE,     "RATE",  &colNumRATE, &status) )
	      printerror( status, __LINE__ );
	    if ( fits_get_colnum(phalcptr, FALSE, "STAT_ERR", &colNumSTAT_ERR, &status) )
	      printerror( status, __LINE__ );

	    /* Adjust the EXPOSURE keyword first. */
	    sum = 0.0;
	    for (recCnt=1; recCnt<=naxis2; recCnt++) {
	      /* Get the ROWID value from the .pha file. */
	      fits_read_col_str(phalcptr, colNumROWID,  recCnt, 1L, 1L,
				"",  rowid, &anynul, &status);  /* Read ROWID */
	      detectorNumber = rowid[0][7] - 0x30;
	      sum += multFactorPerDetector[detectorNumber];  /* Add to sum for adjusting exposure keyword. */
	    }
	    exposure = (float) ((double)exposure * (sum / ((double)naxis2)));

	    sprintf( errorMsg, "Adjusted EXPOSURE = %f", exposure);
	    CHATTER(5);

	    /* Rewrite the exposure keyword in the .pha file HDU 2 header area */
	    if ( fits_update_key_fixflt(phalcptr, "EXPOSURE", exposure, 3, keycomment, &status) )
	      printerror( status, __LINE__ );

	    /* If we got this far, then adjust the values for each detector. */
	    for (recCnt=1; recCnt<=naxis2; recCnt++) {
	      /* Get the ROWID value from the .pha file. */
	      fits_read_col_str(phalcptr, colNumROWID,  recCnt, 1L, 1L,
				"",  rowid, &anynul, &status);  /* Read ROWID */
	      detectorNumber = rowid[0][7] - 0x30;
	      for (chanCnt=1; chanCnt<=detchans; chanCnt++) {
		fits_read_col_dbl(phalcptr,     colNumRATE, recCnt, chanCnt, 1L,
				  (double)0,       Rate, &anynul, &status);  /* read RATE  */
		fits_read_col_dbl(phalcptr, colNumSTAT_ERR, recCnt, chanCnt, 1L,
				  (double)0,      Error, &anynul, &status);  /* read STAT_ERR */

		/* sprintf( errorMsg, " Current  RATE = %lf   Current ERROR = %lf", Rate[0], Error[0] );
		CHATTER(5); */

		/* Alter the RATE and STAT_ERR column values. */
		Rate[0]  =  Rate[0] * (1.0 / multFactorPerDetector[detectorNumber]);
		Error[0] = Error[0] * (1.0 / multFactorPerDetector[detectorNumber]);

		/* sprintf( errorMsg, "Adjusted  RATE = %lf  Adjusted ERROR = %lf", Rate[0], Error[0] );
		CHATTER(5); */

		/* Write and re-write column values. */
		fits_write_col_dbl(phalcptr,     colNumRATE, recCnt, chanCnt, 1L,  Rate, &status); /* write  RATE */
		fits_write_col_dbl(phalcptr, colNumSTAT_ERR, recCnt, chanCnt, 1L, Error, &status); /* write  STAT_ERR */
	      }
	    }
	    deadTimesUpdated = TRUE;
	    break;
	  default:
	    Fcecho(" ");
	    sprintf( errorMsg, "Default case not valid.  Line %d", __LINE__ );
	    Fcecho( errorMsg );
	    exit(-1);
	  }
	  break;
	default:
	  Fcecho(" ");
	  sprintf( errorMsg, "Default case not valid.  Line %d", __LINE__ );
	  Fcecho( errorMsg );
	  exit(-1);
	}
	break;
      default:
	Fcecho(" ");
	sprintf( errorMsg, "Default case not valid.  Line %d", __LINE__ );
	Fcecho( errorMsg );
	exit(-1);
      }
    }
    break;

  case lcFileType:
    /* Get the HDUCLAS3 keyword from the .lc file. */
    fits_read_key(phalcptr, TSTRING, "HDUCLAS3", (void *)hduclas3, keycomment, &status);
    if ( status ) {
      /* If it doesn't exist or failed some other way, we cannot continue. */
      sprintf( errorMsg, "HDUCLAS3 keyword does not exist!  No further processing possible." );
      Fcecho( errorMsg );
      furtherProcessingOk = FALSE;
    }

    /* Check the .pha file for ROWIDn, n=1,4 and set the validity array. */
    fits_read_key(phalcptr, TSTRING, "ROWID1", (void *)rowid1, keycomment, &status);
    if ( status ) {
      /* If it doesn't exist or failed some other way, we cannot continue. */
      sprintf( errorMsg, "For case lcFileType, the ROWID1 keyword read failed!  No further processing possible." );
      Fcecho( errorMsg );
      furtherProcessingOk = FALSE;
      break;
    }
    if ( (strcmp(rowid1, "Event") != 0) && (strcmp(rowid1, "TimSerCnt") != 0) ) {  /* First one is _NOT_ Event/TimSerCnt. */
      numberOfValidDetectorsInPhaLcFile = 0.0;
      /* For each of the possible detectors to be detected :) */
      for (nfield=1; nfield<=4; nfield++) {
	/* Create the ROWIDn string. */
	rowidNameString[5] = (char)(0x30 + nfield);
	/* Get the ROWIDn keyword from the .pha file putting it in the rowidn variable. */
	fits_read_key(phalcptr, TSTRING, rowidNameString, (void *)rowidn, keycomment, &status);
	if ( status ) {
	  /* If it doesn't exist then we are done looking for detectors. */
	  status = 0;  /* reset status since we _expected_ it to fail */
	  fits_clear_errmsg();  /* clear the message stack to this point */
	  nfield = 5;  /* This kicks us out of the local for{} statement. */
	}
	else {
	  numberOfValidDetectorsInPhaLcFile += 1.0;
	  /* Extract the actual detector number from the keyword value. */
	  detectorNumber = rowidn[7] - 0x30;
	  /* Mark the actual detector as a contributor. */
	  detectorIsValid[detectorNumber] = TRUE;
	}
      }
    }
    else {
      /* For TimSerCnt all detectors were used. */
      numberOfValidDetectorsInPhaLcFile = 4.0;
      for (nfield=0; nfield<4; nfield++) {
	detectorIsValid[nfield] = TRUE;
      }
    }
    /* Report which detectors will actually be used in the livetime computation. */
    /* This means the detector must have been requested, it must have livetime   */
    /* counts and it must be in the pha/lc file. Due to the livetime check, the  */
    /* variable detectorHasLivetimeCounts[i] can only be true if detRequested[i] */
    /* is true.                                                                  */
    validDetectorString[2] = (char)(ClusterIndex + 0x41);  /* validDetectorString[] is now "PWA" or "PWB". */
    j = 3;  /* Start appending here. */
    k = 0;
    for (i=0;i<4;i++){
      if ( detectorHasLivetimeCounts[i] && detectorIsValid[i] ) {
	validDetectorString[j++] = (char)(i + 0x30);
	detectorsInLivetimeString[k] = (char)(i + 0x30);
	k += 2;
      }
    }
    sprintf( errorMsg, "Detectors to be used in livetime calculation: %s", detectorsInLivetimeString );
    Fcecho( errorMsg );
    if (fits_write_history(phalcptr, errorMsg, &status))
      printerror( status, __LINE__ );
    if ( strcmp( &validDetectorString[3], "0123" ) == 0 ) {
      validDetectorString[3] = '\0';  /* End the string after PWA or PWB. */
    }
    if ( fits_update_key_str(phalcptr, "DETNAM", validDetectorString, "Detectors used in livetime calculation", &status) )
      printerror( status, __LINE__ );

    /* Compute the average deadtime values. We need this for Event and TimSerCnt files. */
    realNumberOfValidDetectors = (numberOfValidDetectorsInPhaLcFile <= numberOfValidDetectors) ?
      numberOfValidDetectorsInPhaLcFile : numberOfValidDetectors;
    for (j=0; j<8192; j++) {  /* For all IDF's */
      LTAverage = (double)0.0;
      for (i=0; i<4; i++) {  /* Sum over the detectors. */
	if ( detectorIsValid[i] && detectorHasLivetimeCounts[i] ) {
	  /* We only add in a detector used in the .lc file and which has been requested. */
	  LTAverage += ticksOfDeadTimePerIDF[j][i];
	}
      }
      averageDeadTimePerIDF[j] = LTAverage / realNumberOfValidDetectors;
      sprintf( errorMsg, "averageDeadTimePerIDF[%4d] = %lf", j, averageDeadTimePerIDF[j] );
      CHATTER(15);
    }

    /* Get time delta for use with an .lc file */
    if ( fits_read_key(phalcptr, TDOUBLE, "TIMEDEL", (void *)&timedel, keycomment, &status) )
      printerror( status, __LINE__ );
    sprintf( errorMsg, "TIMEDEL = %lf / %s", timedel, keycomment );
    CHATTER(10);

    maxIDFsSpanned = (int)(ceil(timedel/Sixteen) + 1.0);
    sprintf( errorMsg, "maxIDFsSpanned = %d", maxIDFsSpanned );
    CHATTER(10);

    if ( furtherProcessingOk ) {
      /* Create switch index for the HDUCLAS3 keyword. */
      for (i=0; i<4; i++) {
	if ( strcmp(hduclas3, hduclasNames[i]) == 0 ) {
	  hduclas3IntIndex = i;
	}
      }
      /* Now we have to check and see just what kind of .lc file we have, i.e., was   */
      /* it derived from an events list file or an archive file; was it a COUNT, MEAN */
      /* or RATE; how many detectors and which ones were used.                        */
      switch ( typeOfDeadtimeData ) {  /* First, switch on event list or archive data.  */
      case EventsListData:
	switch ( hduclas3IntIndex ) {  /* Switch on HDUCLAS3 keyword value. */
	case COUNT:
	  deadcNumber = 1;
	  deadcColValue = (deadcNumber * 4) + 1;
	  deadcNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	  /* Now create a new column in the light curve file called DEADC. */
	  if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
	    printerror( status, __LINE__ );

	  /* Get NAXIS2 for number of time bins */
	  if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	    printerror( status, __LINE__ );

	  sprintf( errorMsg, ".lc file, events list data, case COUNT: NAXIS2 = %d / %s", naxis2, keycomment);
	  CHATTER(10);

	  /* Build the TIME name string. */
	  timeNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	  /* Get column number for TIME keyword. */
	  if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
	    printerror( status, __LINE__ );

	  for (recCnt=1; recCnt<=naxis2; recCnt++) {
	    fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
			      (double)0,       Time, &anynul, &status);  /* read TIME  */

	    startTimeOfBin = Time[0] + offsetToTime - timezero;
	    stopTimeOfBin  = startTimeOfBin + timedel;

	    p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

	    /* Initialize some presumptive segments. */
	    for (i=0; i<maxIDFsSpanned; i++) {
	      pStart[i] = p0 + (Sixteen * ((double)i));
	      pStop[i]  = pStart[i] + Sixteen;
	    }
	    pStart[0] = startTimeOfBin;

	    /* Now adjust them based on what we know about the bin. */
	    for (i=0; i<maxIDFsSpanned; i++) {
	      if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
	      if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
	    }

	    /* Sum the livetime weighted by the time; Sum the time. */
	    sumovl = 0.0;
	    sumlt = 0.0;
	    for (i=0; i<maxIDFsSpanned; i++) {
	      ovl = pStop[i] - pStart[i];
	      lt = averageDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue];
	      sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
	      CHATTER(15);
	      if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		sumovl += ovl;
		sumlt += ovl * lt;
	      }
	    }

	    sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
	    CHATTER(15);

	    /* We do not want to divide by zero, so check it out here. */
	    if ( sumovl > 0.0 ) {
	      /* Compute the deadtime fraction.  This is an average percentage of time the */
	      /* detectors are dead during the time in the good time intervals.            */
	      dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
	    }
	    else {
	      if ( sumlt != 0.0 ) {
		sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		CHATTER(2);
	      }
	      else {
		/* sumlt == 0.0 AND sumovl == 0.0 */
		dtfract[0] = 0.0;
		sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		CHATTER(10);
	      }
	    }

	    sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
	    CHATTER(10);

	    /* Write new column value. */
	    fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* write  DEADC */
	  }
	  deadTimesUpdated = TRUE;
	  break;
	case MEAN:
	  Fcecho(" ");
	  sprintf( errorMsg, "Function not yet implemented.  Line %d", __LINE__ );
	  Fcecho( errorMsg );
	  exit(-1);
	  break;
	case RATE:
	  deadcNumber = 1;
	  deadcColValue = (deadcNumber * 4) + 1;
	  deadcNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	  /* Now create a new column in the light curve file called DEADC. */
	  if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
	    printerror( status, __LINE__ );

	  /* Get NAXIS2 for number of time bins */
	  if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	    printerror( status, __LINE__ );

	  sprintf( errorMsg, ".lc file, events list data, case RATE: NAXIS2 = %d / %s", naxis2, keycomment);
	  CHATTER(10);

	  /* Build the TIME, RATE and ERROR name strings. */
	  timeNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';
	  rateNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';
	  errorNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	  /* get column number for TIME, RATE and ERROR keywords */
	  if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
	    printerror( status, __LINE__ );

	  if ( fits_get_colnum(phalcptr, FALSE,  rateNameString,  &colNumRATE, &status) )
	    printerror( status, __LINE__ );

	  if ( fits_get_colnum(phalcptr, FALSE, errorNameString, &colNumERROR, &status) )
	    printerror( status, __LINE__ );

	  for (recCnt=1; recCnt<=naxis2; recCnt++) {
	    fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
			      (double)0,       Time, &anynul, &status);  /* read TIME  */
	    fits_read_col_dbl(phalcptr,  colNumRATE,  recCnt, 1L, 1L,
			      (double)0,       Rate, &anynul, &status);  /* read RATE  */
	    fits_read_col_dbl(phalcptr, colNumERROR, recCnt, 1L, 1L,
			      (double)0,      Error, &anynul, &status);  /* read ERROR */

	    startTimeOfBin = Time[0] + offsetToTime - timezero;
	    stopTimeOfBin  = startTimeOfBin + timedel;

	    p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

	    /* Initialize some presumptive segments. */
	    for (i=0; i<maxIDFsSpanned; i++) {
	      pStart[i] = p0 + (Sixteen * ((double)i));
	      pStop[i]  = pStart[i] + Sixteen;
	    }
	    pStart[0] = startTimeOfBin;

	    /* Now adjust them based on what we know about the bin. */
	    for (i=0; i<maxIDFsSpanned; i++) {
	      if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
	      if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
	    }

	    /* Sum the livetime weighted by the time; Sum the time. */
	    sumovl = 0.0;
	    sumlt = 0.0;
	    for (i=0; i<maxIDFsSpanned; i++) {
	      ovl = pStop[i] - pStart[i];
	      lt = averageDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue];
	      sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
	      CHATTER(15);
	      if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		sumovl += ovl;
		sumlt += ovl * lt;
	      }
	    }

	    sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
	    CHATTER(15);

	    /* We do not want to divide by zero, so check it out here. */
	    if ( sumovl > 0.0 ) {
	      /* Compute the deadtime fraction.  This is an average percentage of time the */
	      /* detectors are dead during the time in the good time intervals.            */
	      dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
	    }
	    else {
	      if ( sumlt != 0.0 ) {
		sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		CHATTER(2);
	      }
	      else {
		/* sumlt == 0.0 AND sumovl == 0.0 */
		dtfract[0] = 0.0;
		sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		CHATTER(10);
	      }
	    }

	    sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
	    CHATTER(10);

	    /* The multiplication factor is 1/(1.-DEADC). per Duane Gruber */
	    multFactor = 1.0 / (1.0 - dtfract[0]);

	    sprintf( errorMsg, "multFactor = %lf", multFactor);
	    CHATTER(10);

	    sprintf( errorMsg, " Current  RATE = %lf   Current ERROR = %lf", Rate[0], Error[0] );
	    CHATTER(5);

	    /* Alter the Rate and Error column values. */
	    Rate[0] = Rate[0] * multFactor;
	    Error[0] = Error[0] * multFactor;

	    sprintf( errorMsg, "Adjusted  RATE = %lf  Adjusted ERROR = %lf", Rate[0], Error[0] );
	    CHATTER(5);

	    /* Write and re-write column values. */
	    fits_write_col_dbl(phalcptr,    colNumRATE, recCnt, 1L, 1L,    Rate, &status); /* write  RATE */
	    fits_write_col_dbl(phalcptr,   colNumERROR, recCnt, 1L, 1L,   Error, &status); /* write  ERROR */
	    fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* write  DEADC */
	  }
	  deadTimesUpdated = TRUE;
	  break;
	default:
	  Fcecho(" ");
	  sprintf( errorMsg, "Default case not valid.  Line %d", __LINE__ );
	  Fcecho( errorMsg );
	  exit(-1);
	}
	break;

      case ArchiveData:
	switch ( hduclas3IntIndex ) {  /* Switch on HDUCLAS3 keyword value. */
	case COUNT:
	  /* Check the ROWID1 keyword value and determine if it is from 1 detector or the */
	  /* sum of the detectors.                                                        */
	  if ( strcmp(rowid1, "TimSerCnt") == 0 ) {
	    deadcNumber = 1;
	    deadcColValue = (deadcNumber * 4) + 1;
	    deadcNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	    /* Now create a new column in the light curve file called DEADC or DEADCn */
	    if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
	      printerror( status, __LINE__ );

	    /* Get NAXIS2 for number of light curve bins */
	    if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	      printerror( status, __LINE__ );

	    sprintf( errorMsg, "light curve NAXIS2 = %d / %s", naxis2, keycomment);
	    CHATTER(10);

	    /* Build the TIME name string. */
	    timeNameString[4]   = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	    /* Get column number for the TIME keyword. */
	    if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
	      printerror( status, __LINE__ );

	    for (recCnt=1; recCnt<=naxis2; recCnt++) {
	      fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
				(double)0,       Time, &anynul, &status);  /* read TIME  */

	      startTimeOfBin = Time[0] + offsetToTime - timezero;
	      stopTimeOfBin  = startTimeOfBin + timedel;

	      p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

	      /* Initialize some presumptive segments. */
	      for (i=0; i<maxIDFsSpanned; i++) {
		pStart[i] = p0 + (Sixteen * ((double)i));
		pStop[i]  = pStart[i] + Sixteen;
	      }
	      pStart[0] = startTimeOfBin;

	      /* Now adjust them based on what we know about the bin. */
	      for (i=0; i<maxIDFsSpanned; i++) {
		if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
		if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
	      }

	      /* Sum the livetime weighted by the time; Sum the time. */
	      sumovl = 0.0;
	      sumlt = 0.0;
	      for (i=0; i<maxIDFsSpanned; i++) {
		ovl = pStop[i] - pStart[i];
		lt = averageDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue];
		sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
		CHATTER(15);
		if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		  sumovl += ovl;
		  sumlt += ovl * lt;
		}
	      }

	      sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
	      CHATTER(15);

	      /* We do not want to divide by zero, so check it out here. */
	      if ( sumovl > 0.0 ) {
		/* Compute the deadtime fraction.  This is an average percentage of time the */
		/* detectors are dead during the time in the good time intervals.            */
		dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
	      }
	      else {
		if ( sumlt != 0.0 ) {
		  sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		  CHATTER(2);
		}
		else {
		  /* sumlt == 0.0 AND sumovl == 0.0 */
		  dtfract[0] = 0.0;
		  sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		  CHATTER(10);
		}
	      }

	      sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
	      CHATTER(10);

	      /* Write new column value. */
	      fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* write  DEADC */
	    }
	    deadTimesUpdated = TRUE;
	  }
	  else /* We have one or more ROWIDn equal to SpecDeti. */ {
	    /* Now we can have two cases here.  The first is if the TFIELDS keyword   */
	    /* is greater than 4, i.e., there is a ROWIDn for each "set" of data      */
	    /* corresponding to a detector.  The second case is if there exists n > 1 */
	    /* ROWIDn entries but TFIELDS == 4, i.e., detectors were averaged.        */

	    /* Get the TFIELDS keyword from the .lc file. */
	    fits_read_key(phalcptr, TINT, "TFIELDS", (void *)&tfields, keycomment, &status);
	    if ( status ) {
	      /* If it doesn't exist or failed some other way, we cannot continue. */
	      sprintf( errorMsg, "For HDUCLAS3 == COUNT, the TFIELDS keyword read failed!  No further processing possible." );
	      Fcecho( errorMsg );
	      furtherProcessingOk = FALSE;
	      break;
	    }

	    numberOfSets = (tfields-1)/3;

	    if ( numberOfSets < (int)numberOfValidDetectorsInPhaLcFile ) {
	      /* Then we have averaging case.  Assume that numberOfSets == 1. */
	      deadcColValue = 5;
	      deadcNameString[5] = '\0';  /* Force to have no number "extension". */

	      /* Now create a new column in the light curve file called DEADC */
	      if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
		printerror( status, __LINE__ );

	      /* Get NAXIS2 for number of light curve bins */
	      if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
		printerror( status, __LINE__ );

	      sprintf( errorMsg, "light curve NAXIS2 = %d / %s", naxis2, keycomment);
	      CHATTER(10);

	      /* Build the TIME name string. */
	      timeNameString[4]  = '\0';  /* Force to have no number "extension". */

	      /* get column number for the TIME keyword. */
	      if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
		printerror( status, __LINE__ );

	      for (recCnt=1; recCnt<=naxis2; recCnt++) {
		fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
				  (double)0,       Time, &anynul, &status);  /* read TIME  */

		startTimeOfBin = Time[0] + offsetToTime - timezero;
		stopTimeOfBin  = startTimeOfBin + timedel;

		p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

		/* Initialize some presumptive segments. */
		for (i=0; i<maxIDFsSpanned; i++) {
		  pStart[i] = p0 + (Sixteen * ((double)i));
		  pStop[i]  = pStart[i] + Sixteen;
		}
		pStart[0] = startTimeOfBin;

		/* Now adjust them based on what we know about the bin. */
		for (i=0; i<maxIDFsSpanned; i++) {
		  if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
		  if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
		}

		/* Sum the average livetime weighted by the time; Sum the time. */
		sumovl = 0.0;
		sumlt = 0.0;
		for (i=0; i<maxIDFsSpanned; i++) {
		  ovl = pStop[i] - pStart[i];
		  /* Get the sum of livetimes for the this interval, we'll divide it out later. */
		  lt = 0.0;
		  for ( j=0; j<4; j++ ) {
		    if ( detectorIsValid[j] && detectorHasLivetimeCounts[j] ) {
		      lt += ticksOfDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue][j];
		    }
		  }
		  /* lt /= numberOfValidDetectors; */
		  sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
		  CHATTER(15);
		  if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		    sumovl += ovl;
		    sumlt += ovl * lt;
		  }
		}
		/* Divide out by the number of detectors that contributed to sumlt. */
		sumlt /= realNumberOfValidDetectors;

		sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
		CHATTER(15);

		/* We do not want to divide by zero, so check it out here. */
		if ( sumovl > 0.0 ) {
				/* Compute the deadtime fraction.  This is an average percentage of time the */
				/* detectors are dead during the time in the good time intervals.            */
		  dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
		}
		else {
		  if ( sumlt != 0.0 ) {
		    sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		    CHATTER(2);
		  }
		  else {
				/* sumlt == 0.0 AND sumovl == 0.0 */
		    dtfract[0] = 0.0;
		    sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		    CHATTER(10);
		  }
		}

		sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
		CHATTER(10);

		/* Write column value. */
		fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* write  DEADC */
	      }
	      deadTimesUpdated = TRUE;
	    }
	    else /* TFIELDS matches the number of sets of data. */ {
	      /* Get NAXIS2 for number of light curve bins */
	      if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
		printerror( status, __LINE__ );

	      sprintf( errorMsg, "light curve NAXIS2 = %d / %s", naxis2, keycomment);
	      CHATTER(10);

	      /* For each of the detectors detected :) */
	      for (nfield=1; nfield<=numberOfSets; nfield++) {
		/* Create the ROWIDn string. */
		rowidNameString[5] = (char)(0x30 + nfield);
		/* Get the ROWIDn keyword from the .pha file putting it in the rowid1 variable. */
		fits_read_key(phalcptr, TSTRING, rowidNameString, (void *)rowid1, keycomment, &status);
		detectorNumber = rowid1[7] - 0x30;

		deadcNumber = nfield;
		deadcColValue = (deadcNumber * 4) + 1;
		deadcNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

		/* Now create a new column in the light curve file called DEADC or DEADCn */
		if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
		  printerror( status, __LINE__ );

		/* Build the TIME name string. */
		timeNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

		/* get column number for TIME keyword. */
		if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
		  printerror( status, __LINE__ );

		for (recCnt=1; recCnt<=naxis2; recCnt++) {
		  fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
				    (double)0,       Time, &anynul, &status);  /* read TIME  */

		  startTimeOfBin = Time[0] + offsetToTime - timezero;
		  stopTimeOfBin  = startTimeOfBin + timedel;

		  p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

		  /* Initialize some presumptive segments. */
		  for (i=0; i<maxIDFsSpanned; i++) {
		    pStart[i] = p0 + (Sixteen * ((double)i));
		    pStop[i]  = pStart[i] + Sixteen;
		  }
		  pStart[0] = startTimeOfBin;

		  /* Now adjust them based on what we know about the bin. */
		  for (i=0; i<maxIDFsSpanned; i++) {
		    if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
		    if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
		  }

		  /* Sum the livetime weighted by the time; Sum the time. */
		  sumovl = 0.0;
		  sumlt = 0.0;
		  for (i=0; i<maxIDFsSpanned; i++) {
		    ovl = pStop[i] - pStart[i];
		    lt = ticksOfDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue][detectorNumber];
		    sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
		    CHATTER(15);
		    if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		      sumovl += ovl;
		      sumlt += ovl * lt;
		    }
		  }

		  sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
		  CHATTER(15);

		  /* We do not want to divide by zero, so check it out here. */
		  if ( sumovl > 0.0 ) {
				/* Compute the deadtime fraction.  This is an average percentage of time the */
				/* detectors are dead during the time in the good time intervals.            */
		    dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
		  }
		  else {
		    if ( sumlt != 0.0 ) {
		      sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		      CHATTER(2);
		    }
		    else {
				/* sumlt == 0.0 AND sumovl == 0.0 */
		      dtfract[0] = 0.0;
		      sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		      CHATTER(10);
		    }
		  }

		  sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
		  CHATTER(10);

		  /* Write new column value. */
		  fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* Write  DEADC or DEADCn. */
		}
	      }
	      deadTimesUpdated = TRUE;
	    }
	  }
	  break;
	case MEAN:
	  Fcecho(" ");
	  sprintf( errorMsg, "Function not yet implemented.  Line %d", __LINE__ );
	  Fcecho( errorMsg );
	  exit(-1);
	  break;
	case RATE:
	  /* Check the ROWID1 keyword value and determine if it is from 1 detector or the */
	  /* sum of the detectors.                                                        */
	  if ( strcmp(rowid1, "TimSerCnt") == 0 ) {
	    deadcNumber = 1;
	    deadcColValue = (deadcNumber * 4) + 1;
	    deadcNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	    /* Now create a new column in the light curve file called DEADC or DEADCn */
	    if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
	      printerror( status, __LINE__ );

	    /* Get NAXIS2 for number of light curve bins */
	    if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
	      printerror( status, __LINE__ );

	    sprintf( errorMsg, "light curve NAXIS2 = %d / %s", naxis2, keycomment);
	    CHATTER(10);

	    /* Build the TIME, RATE and ERROR name strings. */
	    timeNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';
	    rateNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';
	    errorNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

	    /* get column number for TIME, RATE and ERROR keywords */
	    if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
	      printerror( status, __LINE__ );

	    if ( fits_get_colnum(phalcptr, FALSE,  rateNameString,  &colNumRATE, &status) )
	      printerror( status, __LINE__ );

	    if ( fits_get_colnum(phalcptr, FALSE, errorNameString, &colNumERROR, &status) )
	      printerror( status, __LINE__ );

	    for (recCnt=1; recCnt<=naxis2; recCnt++) {
	      fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
				(double)0,       Time, &anynul, &status);  /* read TIME  */
	      fits_read_col_dbl(phalcptr,  colNumRATE,  recCnt, 1L, 1L,
				(double)0,       Rate, &anynul, &status);  /* read RATE  */
	      fits_read_col_dbl(phalcptr, colNumERROR, recCnt, 1L, 1L,
				(double)0,      Error, &anynul, &status);  /* read ERROR */

	      startTimeOfBin = Time[0] + offsetToTime - timezero;
	      stopTimeOfBin  = startTimeOfBin + timedel;

	      p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

	      /* Initialize some presumptive segments. */
	      for (i=0; i<maxIDFsSpanned; i++) {
		pStart[i] = p0 + (Sixteen * ((double)i));
		pStop[i]  = pStart[i] + Sixteen;
	      }
	      pStart[0] = startTimeOfBin;

	      /* Now adjust them based on what we know about the bin. */
	      for (i=0; i<maxIDFsSpanned; i++) {
		if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
		if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
	      }

	      /* Sum the livetime weighted by the time; Sum the time. */
	      sumovl = 0.0;
	      sumlt = 0.0;
	      for (i=0; i<maxIDFsSpanned; i++) {
		ovl = pStop[i] - pStart[i];
		lt = averageDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue];
		sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
		CHATTER(15);
		if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		  sumovl += ovl;
		  sumlt += ovl * lt;
		}
	      }

	      sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
	      CHATTER(15);

	      /* We do not want to divide by zero, so check it out here. */
	      if ( sumovl > 0.0 ) {
				/* Compute the deadtime fraction.  This is an average percentage of time the */
				/* detectors are dead during the time in the good time intervals.            */
		dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
	      }
	      else {
		if ( sumlt != 0.0 ) {
		  sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		  CHATTER(2);
		}
		else {
				/* sumlt == 0.0 AND sumovl == 0.0 */
		  dtfract[0] = 0.0;
		  sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		  CHATTER(10);
		}
	      }

	      sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
	      CHATTER(10);

	      /* The multiplication factor is 1/(1.-DEADC). per Duane Gruber */
	      multFactor = 1.0 / (1.0 - dtfract[0]);

	      sprintf( errorMsg, "multFactor = %lf", multFactor);
	      CHATTER(10);

	      sprintf( errorMsg, " Current  RATE = %lf   Current ERROR = %lf", Rate[0], Error[0] );
	      CHATTER(5);

	      /* Alter the Rate and Error column values. */
	      Rate[0] = Rate[0] * multFactor;
	      Error[0] = Error[0] * multFactor;

	      sprintf( errorMsg, "Adjusted  RATE = %lf  Adjusted ERROR = %lf", Rate[0], Error[0] );
	      CHATTER(5);

	      /* Write and re-write column values. */
	      fits_write_col_dbl(phalcptr,    colNumRATE, recCnt, 1L, 1L,    Rate, &status); /* write  RATE */
	      fits_write_col_dbl(phalcptr,   colNumERROR, recCnt, 1L, 1L,   Error, &status); /* write  ERROR */
	      fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* write  DEADC */
	    }
	    deadTimesUpdated = TRUE;
	  }
	  else /* We have one or more ROWIDn equal to SpecDeti. */ {
	    /* Now we can have two cases here.  The first is if the TFIELDS keyword   */
	    /* is greater than 4, i.e., there is a ROWIDn for each "set" of data      */
	    /* corresponding to a detector.  The second case is if there exists n > 1 */
	    /* ROWIDn entries but TFIELDS == 4, i.e., detectors were averaged.        */

	    /* Get the TFIELDS keyword from the .lc file. */
	    fits_read_key(phalcptr, TINT, "TFIELDS", (void *)&tfields, keycomment, &status);
	    if ( status ) {
	      /* If it doesn't exist or failed some other way, we cannot continue. */
	      sprintf( errorMsg, "For HDUCLAS3 == RATE, the TFIELDS keyword read failed!  No further processing possible." );
	      Fcecho( errorMsg );
	      furtherProcessingOk = FALSE;
	      break;
	    }

	    numberOfSets = (tfields-1)/3;

	    if ( numberOfSets < (int)numberOfValidDetectorsInPhaLcFile ) {
	      /* Then we have averaging case.  Assume that numberOfSets == 1. */
	      deadcColValue = 5;
	      deadcNameString[5] = '\0';  /* Force to have no number "extension". */

	      /* Now create a new column in the light curve file called DEADC */
	      if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
		printerror( status, __LINE__ );

	      /* Get NAXIS2 for number of light curve bins */
	      if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
		printerror( status, __LINE__ );

	      sprintf( errorMsg, "light curve NAXIS2 = %d / %s", naxis2, keycomment);
	      CHATTER(10);

	      /* Build the TIME, RATE and ERROR name strings. */
	      timeNameString[4]  = '\0';  /* Force to have no number "extension". */
	      rateNameString[4]  = '\0';  /* Force to have no number "extension". */
	      errorNameString[5] = '\0';  /* Force to have no number "extension". */

	      /* get column number for TIME, RATE and ERROR keywords */
	      if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
		printerror( status, __LINE__ );

	      if ( fits_get_colnum(phalcptr, FALSE,  rateNameString,  &colNumRATE, &status) )
		printerror( status, __LINE__ );

	      if ( fits_get_colnum(phalcptr, FALSE, errorNameString, &colNumERROR, &status) )
		printerror( status, __LINE__ );

	      for (recCnt=1; recCnt<=naxis2; recCnt++) {
		fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
				  (double)0,       Time, &anynul, &status);  /* read TIME  */
		fits_read_col_dbl(phalcptr,  colNumRATE,  recCnt, 1L, 1L,
				  (double)0,       Rate, &anynul, &status);  /* read RATE  */
		fits_read_col_dbl(phalcptr, colNumERROR, recCnt, 1L, 1L,
				  (double)0,      Error, &anynul, &status);  /* read ERROR */

		startTimeOfBin = Time[0] + offsetToTime - timezero;
		stopTimeOfBin  = startTimeOfBin + timedel;

		p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

		/* Initialize some presumptive segments. */
		for (i=0; i<maxIDFsSpanned; i++) {
		  pStart[i] = p0 + (Sixteen * ((double)i));
		  pStop[i]  = pStart[i] + Sixteen;
		}
		pStart[0] = startTimeOfBin;

		/* Now adjust them based on what we know about the bin. */
		for (i=0; i<maxIDFsSpanned; i++) {
		  if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
		  if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
		}

		/* Sum the average livetime weighted by the time; Sum the time. */
		sumovl = 0.0;
		sumlt = 0.0;
		for (i=0; i<maxIDFsSpanned; i++) {
		  ovl = pStop[i] - pStart[i];
		  /* Get the sum of livetimes for the this interval, we'll divide it out later. */
		  lt = 0.0;
		  for ( j=0; j<4; j++ ) {
		    if ( detectorIsValid[j] && detectorHasLivetimeCounts[j] ) {
		      lt += ticksOfDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue][j];
		    }
		  }
		  /* lt /= numberOfValidDetectors; */
		  sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
		  CHATTER(15);
		  if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		    sumovl += ovl;
		    sumlt += ovl * lt;
		  }
		}
		/* Divide out by the number of detectors that contributed to sumlt. */
		sumlt /= realNumberOfValidDetectors;

		sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
		CHATTER(15);

		/* We do not want to divide by zero, so check it out here. */
		if ( sumovl > 0.0 ) {
				/* Compute the deadtime fraction.  This is an average percentage of time the */
				/* detectors are dead during the time in the good time intervals.            */
		  dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
		}
		else {
		  if ( sumlt != 0.0 ) {
		    sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		    CHATTER(2);
		  }
		  else {
				/* sumlt == 0.0 AND sumovl == 0.0 */
		    dtfract[0] = 0.0;
		    sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		    CHATTER(10);
		  }
		}

		sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
		CHATTER(10);

		/* The multiplication factor is 1/(1.-DEADC). per Duane Gruber */
		multFactor = 1.0 / (1.0 - dtfract[0]);

		sprintf( errorMsg, "multFactor = %lf", multFactor);
		CHATTER(10);

		sprintf( errorMsg, " Current  RATE = %lf   Current ERROR = %lf", Rate[0], Error[0] );
		CHATTER(5);

		/* Alter the Rate and Error column values. */
		Rate[0] = Rate[0] * multFactor;
		Error[0] = Error[0] * multFactor;

		sprintf( errorMsg, "Adjusted  RATE = %lf  Adjusted ERROR = %lf", Rate[0], Error[0] );
		CHATTER(5);

		/* Write and re-write column values. */
		fits_write_col_dbl(phalcptr,    colNumRATE, recCnt, 1L, 1L,    Rate, &status); /* write  RATE */
		fits_write_col_dbl(phalcptr,   colNumERROR, recCnt, 1L, 1L,   Error, &status); /* write  ERROR */
		fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* write  DEADC */
	      }
	      deadTimesUpdated = TRUE;
	    }
	    else /* TFIELDS matches the number of sets of data. */ {
	      /* Get NAXIS2 for number of light curve bins */
	      if ( fits_read_key(phalcptr, TINT, "NAXIS2", (void *)&naxis2, keycomment, &status) )
		printerror( status, __LINE__ );

	      sprintf( errorMsg, "light curve NAXIS2 = %d / %s", naxis2, keycomment);
	      CHATTER(10);

	      /* For each of the detectors detected :) */
	      for (nfield=1; nfield<=numberOfSets; nfield++) {
		/* Create the ROWIDn string. */
		rowidNameString[5] = (char)(0x30 + nfield);
		/* Get the ROWIDn keyword from the .lc file putting it in the rowid1 variable. */
		fits_read_key(phalcptr, TSTRING, rowidNameString, (void *)rowid1, keycomment, &status);
		detectorNumber = rowid1[7] - 0x30;

		deadcNumber = nfield;
		deadcColValue = (deadcNumber * 4) + 1;
		deadcNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

		/* Now create a new column in the light curve file called DEADC or DEADCn */
		if ( fits_insert_col( phalcptr, deadcColValue, deadcNameString, "1D", &status ) )
		  printerror( status, __LINE__ );

		/* Build the TIME, RATE and ERROR name strings. */
		timeNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';
		rateNameString[4]  = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';
		errorNameString[5] = (deadcNumber > 1) ? (0x30 + deadcNumber) : '\0';

		/* get column number for TIME, RATE and ERROR keywords */
		if ( fits_get_colnum(phalcptr, FALSE,  timeNameString,  &colNumTIME, &status) )
		  printerror( status, __LINE__ );

		if ( fits_get_colnum(phalcptr, FALSE,  rateNameString,  &colNumRATE, &status) )
		  printerror( status, __LINE__ );

		if ( fits_get_colnum(phalcptr, FALSE, errorNameString, &colNumERROR, &status) )
		  printerror( status, __LINE__ );

		for (recCnt=1; recCnt<=naxis2; recCnt++) {
		  fits_read_col_dbl(phalcptr,  colNumTIME,  recCnt, 1L, 1L,
				    (double)0,       Time, &anynul, &status);  /* read TIME  */
		  fits_read_col_dbl(phalcptr,  colNumRATE,  recCnt, 1L, 1L,
				    (double)0,       Rate, &anynul, &status);  /* read RATE  */
		  fits_read_col_dbl(phalcptr, colNumERROR, recCnt, 1L, 1L,
				    (double)0,      Error, &anynul, &status);  /* read ERROR */

		  startTimeOfBin = Time[0] + offsetToTime - timezero;
		  stopTimeOfBin  = startTimeOfBin + timedel;

		  p0 = ((double)(((long)startTimeOfBin)/16)) * Sixteen;  /* start time of IDF containing starting bin time */

		  /* Initialize some presumptive segments. */
		  for (i=0; i<maxIDFsSpanned; i++) {
		    pStart[i] = p0 + (Sixteen * ((double)i));
		    pStop[i]  = pStart[i] + Sixteen;
		  }
		  pStart[0] = startTimeOfBin;

		  /* Now adjust them based on what we know about the bin. */
		  for (i=0; i<maxIDFsSpanned; i++) {
		    if ( pStop[i] >= stopTimeOfBin ) pStop[i] = stopTimeOfBin;
		    if ( pStop[i] <= pStart[i] ) pStart[i] = pStop[i];
		  }

		  /* Sum the livetime weighted by the time; Sum the time. */
		  sumovl = 0.0;
		  sumlt = 0.0;
		  for (i=0; i<maxIDFsSpanned; i++) {
		    ovl = pStop[i] - pStart[i];
		    lt = ticksOfDeadTimePerIDF[(((long)pStart[i])/16) - index0IDFValue][detectorNumber];
		    sprintf( errorMsg, "pStart[%d] = %lf  pStop[%d] = %lf  lt = %lf", i, pStart[i], i, pStop[i], lt );
		    CHATTER(15);
		    if ( lt > 0.0 ) {  /* Add in only those pieces that have "real" livtime data. */
		      sumovl += ovl;
		      sumlt += ovl * lt;
		    }
		  }

		  sprintf( errorMsg, "Bin Start = %lf  Bin Stop = %lf  Max IDF's Spanned = %d  sumlt = %lf  sumovl = %lf", startTimeOfBin, stopTimeOfBin, maxIDFsSpanned, sumlt, sumovl );
		  CHATTER(15);

		  /* We do not want to divide by zero, so check it out here. */
		  if ( sumovl > 0.0 ) {
		    /* Compute the deadtime fraction.  This is an average percentage of time the */
		    /* detectors are dead during the time in the good time intervals.            */
		    dtfract[0] = (sumlt / sumovl) / ((double)8388608.0);
		  }
		  else {
		    if ( sumlt != 0.0 ) {
		      sprintf( errorMsg, "Suspicious computation: sumovl == 0.0, sumlt != 0.0 in trying to compute dtfract[0]" );
		      CHATTER(2);
		    }
		    else {
		      /* sumlt == 0.0 AND sumovl == 0.0 */
		      dtfract[0] = 0.0;
		      sprintf( errorMsg, "sumlt == 0.0 AND sumovl == 0.0 in trying to compute dtfract[0].  dtfract[0] set to 0.0" );
		      CHATTER(10);
		    }
		  }

		  sprintf( errorMsg, "dtfract[0] = (sumlt / sumovl) / ((double)8388608.0) = %lf", dtfract[0] );
		  CHATTER(10);

		  /* The multiplication factor is 1/(1.-DEADC). per Duane Gruber */
		  multFactor = 1.0 / (1.0 - dtfract[0]);

		  sprintf( errorMsg, "multFactor = %lf", multFactor);
		  CHATTER(10);

		  sprintf( errorMsg, " Current  RATE = %lf   Current ERROR = %lf", Rate[0], Error[0] );
		  CHATTER(5);

		  /* Alter the Rate and Error column values. */
		  Rate[0] = Rate[0] * multFactor;
		  Error[0] = Error[0] * multFactor;

		  sprintf( errorMsg, "Adjusted  RATE = %lf  Adjusted ERROR = %lf", Rate[0], Error[0] );
		  CHATTER(5);

		  /* Write and re-write column values. */
		  fits_write_col_dbl(phalcptr,    colNumRATE, recCnt, 1L, 1L,    Rate, &status); /* write  RATE */
		  fits_write_col_dbl(phalcptr,   colNumERROR, recCnt, 1L, 1L,   Error, &status); /* write  ERROR */
		  fits_write_col_dbl(phalcptr, deadcColValue, recCnt, 1L, 1L, dtfract, &status); /* write  DEADC */
		}
	      }
	      deadTimesUpdated = TRUE;
	    }
	  }
	  break;
	default:
	  Fcecho(" ");
	  sprintf( errorMsg, "Default case not valid.  Line %d", __LINE__ );
	  Fcecho( errorMsg );
	  exit(-1);
	}
	break;
      default:
	Fcecho(" ");
	sprintf( errorMsg, "Default case not valid.  Line %d", __LINE__ );
	Fcecho( errorMsg );
	exit(-1);
      }
    }
    break;
  }

  /* If we successfully recomputed the deadtimes, mark the file as fixed */
  /* and update/add the OLDEXP (original exposure) keyword.              */
  if ( deadTimesUpdated ) {
    /* Write (or update) the old exposure value. */
    if ( oldexposure != -3.14159 ) {
      if ( fits_update_key_fixflt(phalcptr, "OLDEXP", oldexposure, 3, "Original EXPOSURE value", &status) )
	printerror( status, __LINE__ );
    }
    /* Update (append) the DEADAPP keyword indicating that livetime correction */
    /* has been applied to the .pha/.lc file.  Will create keyword if it does  */
    /* not yet exist.                                                          */
    if ( fits_update_key_log(phalcptr, "DEADAPP", 1, "DeadTime Correction Factor Applied", &status) )
      printerror( status, __LINE__ );
  }

  /* Close the FITS files */
  if ( fits_close_file(eventarcptr, &status) )
    printerror( status, __LINE__ );

  if ( fits_close_file(phalcptr, &status) )
    printerror( status, __LINE__ );

  if ( fits_close_file(engvalptr, &status) )
    printerror( status, __LINE__ );

  /* Print out final error summary if any interpolation of engineering values was performed. */
  if ( engineeringErrorsFound ) {
    i = 0;
    while ( errorsList[i].type != 5 ) {  /* since 0 - 4 are the only valid entries */
      idfDiff = errorsList[i].stop - errorsList[i].start + 1;
      sprintf( errorMsg, "Detector %d: %s data interpolated for %3d IDFs: %d - %d", errorsList[i].detector, dataName[errorsList[i].type], idfDiff, errorsList[i].start+index0IDFValue, errorsList[i].stop+index0IDFValue );
      CHATTER(2);
      i++;
    }
  }

  return;
}
/*--------------------------------------------------------------------------*/
void printerror( int status, int lineNumber )
{
  /*****************************************************/
  /* Print out cfitsio error messages and exit program */
  /*****************************************************/

  char status_str[FLEN_STATUS], errmsg[FLEN_ERRMSG], errorMsg[256];

  if (status) {
    sprintf( errorMsg, "\n*** Error occurred during program execution at line %d ***", lineNumber );
    Fcecho( errorMsg );
  }

  fits_get_errstatus(status, status_str);   /* get the error description */
  sprintf( errorMsg, "\nstatus = %d: %s", status, status_str);
  Fcecho( errorMsg );

    /* get first message; null if stack is empty */
  if ( fits_read_errmsg(errmsg) )  {
    sprintf( errorMsg, "\nError message stack:");
    Fcecho( errorMsg );
    sprintf( errorMsg, " %s", errmsg);
    Fcecho( errorMsg );

    while ( fits_read_errmsg(errmsg) ) {  /* get remaining messages */
      sprintf( errorMsg, " %s", errmsg);
      Fcecho( errorMsg );
    }
  }

  exit( status );       /* terminate the program, returning error status */
}
