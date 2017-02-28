/*----------------------------------------------------------------------
 *
 *  recofmi.c
 *
 *  Date:       3 June 1996
 *  Programmer: Arnold Rots, USRA
 *  FTOOLized by: M. Tripicco (HSTX) 19Nov96
 *
 *  07Apr00: fixed day boundary glitch (modifies obsid level FMI and up)
 *
 *  Description:
 *    Reconstruct an FMI from subdirectories' FMIs which may be nested
 *    one level or two levels deep (determined by level parameter).
 *
 *  Parameters:
 *    dirpath   - Directory path where the FMI file to be fixed can be found.
 *    level     - Nesting level for sub-FMIs; may be 1 or 2; default: 1.
 *    delete    - Delete all rows in the FMI before reconstructing it; default: no
 *    overwrite - Allow existing rows in FMI to be overwritten; default: no
 *                (only new rows may be added.)
 *    check     - Check FMI consistency; default: no.
 *    debug     - Run in debug mode (chatty); default: no.
 *
 ----------------------------------------------------------------------*/

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>

#include "fitsio.h"
#include "cfortran.h"
#include "pctype.h"
#include "ftools.h"
#include "ftoolstruct.h"
#include "xpi.h"
#include "xte.h"

void Recofmi ()
{

  char FMIDir[256] ;
  char FMIName[256] ;
  char inputDir1[256] ;
  char inputDir2[256] ;
  char inputDir[256] ;
  char inputFile[256] ;
  char colstring1[64] ;
  char colstring2[64] ;
  DIR *dirp1 ;
  struct dirent *direntp1 ;
  DIR *dirp2 ;
  struct dirent *direntp2 ;

  char tbd[12] ;
  int status=0 ;
  int instatus=0 ;
  fitsfile *miunit ;
  fitsfile *inunit ;
  char comment[80] ;
  long i ;
  char li ;
  int blk ;
  double d ;
  char obsId[16] ;
  char clock1[9], clock2[9], date1[9], date2[9];
  char olddate2[9];
  char **outObsId=0 ;
  long *start ;
  long *stop ;
  long instart ;
  long instop ;
  char line[256] ;
  char string[256] ;
  char cols[55] ;
  char *c ;
  char *t1, *t2, *d1, *d2;
  int any ;
  char dumstr[9];
  int day, mon, yr;

  long nrows=0 ;
  long nobs=0 ;
  long inrows=0 ;
  long irow ;
  long rowNum ;
  long rowlen ;
  long inrowlen ;
  unsigned char *therow ;
  int naxes=0 ;
  int delete = 0 ;
  int overwrite = 0 ;
  int debug=0 ;
  int level=1 ;
  int check=0 ;
  int wenable=1 ;

  int errstat=0 ;
  int BufLen_2 = 255;
  char text[31];
  char msg[256];

  int truncatefits (fitsfile**, char*) ;

  strcpy (comment, "                    ") ;

/*
 *   --------------------------
 * - Get command line arguments -
 *   --------------------------
 */

  Uclgst("dirpath",FMIDir,&errstat);
  if(errstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get Directory Path parameter");
    ffgerr(errstat, text);
    exit(1);
  }
  Uclgsi("level",&level,&errstat);
  if(errstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get Nesting Level paramter");
    ffgerr(errstat, text);
    exit(1);
  }
  Uclgsb("delete",&delete,&errstat);
  if(errstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get Delete parameter");
    ffgerr(errstat, text);
    exit(1);
  }
  Uclgsb("overwrite",&overwrite,&errstat);
  if(errstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get Overwrite parameter");
    ffgerr(errstat, text);
    exit(1);
  }
  Uclgsb("check",&check,&errstat);
  if(errstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get Check parameter");
    ffgerr(errstat, text);
    exit(1);
  }
  Uclgsb("debug",&debug,&errstat);
  if(errstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get Debug parameter");
    ffgerr(errstat, text);
    exit(1);
  }
  
  if ( !FMIDir[0] || ( level < 1 ) || ( level > 2 ) || ( delete && check ) ) {
    XTE_Fcecho("recoFMI - Invalid Usage:\n   level may be 1 or 2;\n   delete and check are mutually exclusive") ;
    exit (1) ;
  } 

/*
 *   ------------
 * - Open the FMI -
 *   ------------
 */
  if ( check )
    wenable = 0 ;
  sprintf (FMIName, "%s/FMI", FMIDir) ;

  ffopen (&miunit, FMIName, wenable, &status) ;
  if (status) {
    sprintf (msg, "recoFMI - Could not open file %s", FMIName) ;
    XTE_Fcecho(msg);
    ffgerr(status, text);
    exit (1) ;
  }
  if ( debug ){
    sprintf (msg, "recoFMI - Opened Master FMI %s", FMIName) ;
    XTE_Fcecho(msg);
  }

  ffmrhd (miunit, 1, &blk, &status) ;
  if (status) {
    sprintf (msg, "recoFMI - Could not move to next HDU in %s", FMIName) ;
    XTE_Fcecho(msg);
    ffgerr(status, text);
    exit (1) ;
  }

  ffgkyj (miunit, "NAXIS1", &rowlen, comment, &status) ;
  if (status) {
    sprintf (msg, "recoFMI - Could not get NAXIS1 in %s", FMIName) ;
    XTE_Fcecho(msg);
    ffgerr(status, text);
    exit (1) ;
  }

  ffgkyj (miunit, "NAXIS2", &nrows, comment, &status) ;
  if (status) {
    sprintf (msg, "recoFMI - Could not get NAXIS2 in %s", FMIName) ;
    XTE_Fcecho(msg);
    ffgerr(status, text);
    exit (1) ;
  }
  if ( debug ){
    sprintf (msg, "recoFMI - Starting out with %d rows", nrows) ;
    XTE_Fcecho(msg);
  }

  therow = (unsigned char *) malloc (rowlen * sizeof(unsigned char)) ;
  if ( debug ){
    sprintf (msg, "recoFMI - Allocated %d bytes for the row buffer", rowlen) ;
    XTE_Fcecho(msg);
  }

/*
 *   -------------------------------------
 * - If delete flag is set, first empty it -
 *   -------------------------------------
 */
  if ( delete && nrows ) {
    ffdrow (miunit, 1, nrows, &status) ;
    if (status) {
      sprintf (msg, "recoFMI - Could not delete rows in file %s", FMIName) ;
      XTE_Fcecho(msg);
      ffgerr(status, text);
      exit (1) ;
    }
    nrows = 0 ;
    if ( truncatefits (&miunit, FMIName) ) {
      sprintf (msg, "recoFMI - Could not truncate file %s", FMIName) ;
      XTE_Fcecho(msg);
      exit (1) ;
    }
  }

/*
 *   -------------------------------------------
 * - Now loop over all the files in the directory -
 *   -------------------------------------------
 */

  if ( ( dirp1 = opendir (FMIDir) ) == NULL )
    return ;
  if ( FMIDir[strlen(FMIDir)-1] != '/' )
    strcat (FMIDir, "/") ;
  if ( !strcmp (FMIDir, "./") )
    *FMIDir = 0 ;

/*  Top level loop  */
  while ( 1 ) {
    *inputDir1 = 0 ;
/*  for two levels  */
    if ( level == 2 ) {
      if ( ( direntp1 = readdir (dirp1) ) == NULL )
	break ;
      else {
	if ( *(direntp1->d_name) == '.' )
	  continue ;
	strcpy (inputDir1, direntp1->d_name) ;
	if ( inputDir1[strlen(inputDir1)-1] != '/' )
	  strcat (inputDir1, "/") ;
      }
    }

/*  or for one - nothing needs to be done, the default's fine  */
    if ( !inputDir1 )
      strcpy (inputDir1, "./") ;

/*  Second level directory  */
    if ( *FMIDir ) {
      strcpy (inputDir, FMIDir) ;
      strcat (inputDir, inputDir1) ;
    }
    else if ( *inputDir )
      strcpy (inputDir, inputDir1) ;
    else
      strcpy (inputDir, "./") ;
    if ( ( dirp2 = opendir (inputDir) ) == NULL )
      continue ;
/*  Bottom level loop  */
    while ( ( direntp2 = readdir (dirp2) ) != NULL ) {
      strcpy (inputDir2, direntp2->d_name) ;

      if ( *inputDir2 == '.' )
	continue ;

      if ( debug ){
	sprintf (msg, "recoFMI - Trying directory %s", inputDir2) ;
	XTE_Fcecho(msg);
      }

      if ( inputDir2[strlen(inputDir2)-1] != '/' )
	strcat (inputDir2, "/") ;

      if ( *inputDir1 && ( *inputDir1 != '.' ) ) {
	strcpy (inputDir, inputDir1) ;
	strcat (inputDir, inputDir2) ;
      }
      else
	strcpy (inputDir, inputDir2) ;

      strcpy (inputFile, FMIDir) ;
      strcat (inputFile, inputDir) ;
      strcat (inputFile, "FMI") ;
      if ( debug ){
	sprintf (msg, "recoFMI - Try to open %s", inputFile) ;
	XTE_Fcecho(msg);
      }

      instatus = 0 ;

/*
 *   -----------------------------------------------
 * - See whether it is a directory containing an FMI -
 *   -----------------------------------------------
 */
      if ( !ffopen (&inunit, inputFile, READWRITE, &instatus) ) {
	if ( debug ){
	  sprintf (msg, "recoFMI - Appears to have opened %s, status %d",
		   inputFile, instatus) ;
	  XTE_Fcecho(msg);
	}
	ffmrhd (inunit, 1, &blk, &instatus) ;
	if (instatus) {
	  sprintf (msg, "recoFMI - Could not move to next HDU in %s",
		   inputFile) ;
	  XTE_Fcecho(msg);
	  ffclos (inunit, &instatus) ;
	  continue ;
	}
	ffgkyj (inunit, "NAXIS1", &inrowlen, comment, &instatus) ;
	if (instatus) {
	  sprintf (msg, "recoFMI - Could not get NAXIS1 in %s",
		   inputFile) ;
	  XTE_Fcecho(msg);
	  ffclos (inunit, &instatus) ;
	  continue ;
	}
	if ( inrowlen != rowlen ) {
	  sprintf (msg, "recoFMI - Row length in file %s is %d; should be %d",
		   inputFile, inrowlen, rowlen) ;
	  XTE_Fcecho(msg);
	  ffclos (inunit, &instatus) ;
	  continue ;
	}
	ffgkyj (inunit, "NAXIS2", &inrows, comment, &instatus) ;
	if (instatus) {
	  sprintf (msg, "recoFMI - Could not get NAXIS2 in %s",
		   inputFile) ;
	  XTE_Fcecho(msg);
	  ffclos (inunit, &instatus) ;
	  continue ;
	}
	if ( debug ){
	  sprintf (msg, "recoFMI - %s has %d rows", inputFile, inrows) ;
	  XTE_Fcecho(msg);
	}

/*
 *   ----------------------
 * - Loop over all its rows -
 *   ----------------------
 */
	for (irow=1; irow<=inrows; irow++) {

	  c = obsId ;
	  ffgcvs (inunit, 1, irow, 1, 1, "", &c, &any, &instatus) ;
	  ffgcvj (inunit, 2, irow, 1, 1, 0, &instart, &any, &instatus) ;
	  ffgcvj (inunit, 3, irow, 1, 1, 0, &instop, &any, &instatus) ;
	  if ( !(*obsId) || ( *obsId == ' ' ) ) {
	    sprintf (msg, "recoFMI - Skip row %d of %s: null ObsId",
		     irow, inputFile) ;
	    XTE_Fcecho(msg);
	    continue ;
	  }
	  if ( debug ){
	    sprintf (msg, "recoFMI - Row %d: %s", irow, obsId) ;
	    XTE_Fcecho(msg);
	  }
	  /* check and fix day boundary glitch (07Apr00:MJT) */
	  t1 = clock1;
	  t2 = clock2;
	  d1 = date1;
	  d2 = date2;
	  ffgcvs (inunit, 4, irow, 1, 1, "", &d1, &any, &instatus) ;
	  ffgcvs (inunit, 5, irow, 1, 1, "", &t1, &any, &instatus) ;
	  ffgcvs (inunit, 6, irow, 1, 1, "", &d2, &any, &instatus) ;
	  ffgcvs (inunit, 7, irow, 1, 1, "", &t2, &any, &instatus) ;
	  if ((strncmp(clock2,clock1,2) < 0) && !(strncmp(date2,date1,2))){
	      strcpy(olddate2,date2);
	      strncpy(dumstr,date2,2);
	      day = atoi(dumstr);
	      strncpy(dumstr,&date2[3],2);
	      dumstr[2] = '\0';
	      mon = atoi(dumstr);
	      strncpy(dumstr,&date2[6],2);
	      dumstr[2] = '\0';
	      yr = atoi(dumstr);
	      if (day >= 28) {
		  switch (mon) {
		  case 2:
		      /* simple leapyear test is OK here *
		       * (since only 2000 is relevant)   */
		      if (day == 28 && ((yr % 4) == 0)) day++;
		      else {
			  day = 1;
			  mon++;
		      }
		      break;
		  case 12:
		      if (day == 31) {
			  day = 1;
			  mon = 1;
			  if (yr == 99) yr = 0;
			  else yr++;
		      } else day++;
		      break;
		  case 9: case 4: case 6: case 11:
		      if (day == 30) {
			  day = 1;
			  mon++;
		      } else day++;
		      break;
		  default:
		      if (day == 31) {
			  day = 1;
			  mon++;
		      } else day++;
		      break;
		  }
	      } else {
		  day++;
	      }
	      sprintf(dumstr, "%02d", day);
	      date2[0] = dumstr[0];
	      date2[1] = dumstr[1];
	      sprintf(dumstr, "%02d", mon);
	      date2[3] = dumstr[0];
	      date2[4] = dumstr[1];
	      sprintf(dumstr, "%02d", yr);
	      date2[6] = dumstr[0];
	      date2[7] = dumstr[1];
	      ffpcls(inunit, 6, irow, 1, 1, &d2, &instatus);
	      ffpcks(inunit, &instatus); /* update checksum/datasum */
	      if (instatus) {
		sprintf (msg, "recoFMI - Error inserting fixed date (%s) into %s",
			 date2, obsId) ;
		XTE_Fcecho(msg);
		ffgerr(instatus, text);
		exit (1) ;
	      }
	      if (debug) {
		  XTE_Fcecho("recoFMI - Fixed day boundary glitch");
		  sprintf(msg, "recoFMI - Old StopDate: %s, New StopDate: %s", olddate2, date2);
		  XTE_Fcecho(msg);
	      }
	  }
	  

/*
 *   --------------------------------------
 * - If there is nothing in the output FMI: -
 *   --------------------------------------
 */
	  if ( delete || !nrows ) {
	    if ( debug ){
	      sprintf (msg, "recoFMI - Start with empty FMI") ;
	      XTE_Fcecho(msg);
	    }
	    if ( ! nrows ) {
	      ffirow (miunit, 0, 1, &status) ;
	      if (status) {
		sprintf (msg, "recoFMI - Could not insert row 1 in file %s (status %d)",
			 FMIName, status) ;
		XTE_Fcecho(msg);
		ffgerr(status, text);
		exit (1) ;
	      }
	    }
	    rowNum = 1 ;
	    nrows = 1 ;
	    delete = 0 ;
	  }

/*
 *   -------------------------
 * - First check on the ObsIds -
 *   -------------------------
 */
	  else {
	    if ( nobs != nrows ) {
	      if ( nobs ) {
		for (i=0; i<nobs ; i++)
		  free (outObsId[i]) ;
		free (outObsId) ;
	      }
	      nobs = nrows ;
	      outObsId = (char **) malloc (nrows * sizeof (char*)) ;
	      for (i=0; i<nrows ; i++)
		outObsId[i] = (char *) malloc (20 * sizeof (char)) ;
	      ffgcvs (miunit, 1, 1, 1, nrows, "", outObsId, &any, &status) ;
	      if (status) {
		sprintf (msg, "recoFMI - Could not read ObsIds in file %s",
			 FMIName) ;
		XTE_Fcecho(msg);
		ffgerr(status, text);
		exit (1) ;
	      }
	    }
	    rowNum = 0 ;
	    for (i=0; i<nrows; i++)
	      if ( !strcmp(outObsId[i], obsId) ) {
		rowNum = i + 1 ;
		if ( debug ){
		  sprintf (msg, "recoFMI - Match for %s on row %d",obsId, rowNum) ;
		  XTE_Fcecho(msg);
		}
		break ;
	      }
/*
 *   ------------------------------------------------------------------
 * - If there is a match and you're not allowed to overwrite, that's it -
 *   ------------------------------------------------------------------
 */
	    if ( rowNum ) {
	      if ( check ) {
		strcpy (outObsId[rowNum-1], "X") ;
		continue ;
	      }
	      else if ( !overwrite ) {
		sprintf (msg, "recoFMI - Row %d in file %s has same ObsId as row %d (%s)",
			 irow, inputFile, rowNum, obsId) ;
		XTE_Fcecho(msg);
		continue ;
	      }
	      else{
		sprintf (msg, "recoFMI - Overwriting row %d by row %d in file %s (ObsId %s)",
			 rowNum, irow, inputFile, obsId) ;
		XTE_Fcecho(msg);
	      }
	    }

/*
 *   ---------------------------------
 * - The tedious checking of the times -
 *   ---------------------------------
 */
	    else {
	      if ( check ) {
		sprintf (msg, "recoFMI - No match for ObsId %s from %s in %s",
			 obsId, inputFile, FMIName) ;
		XTE_Fcecho(msg);
		continue ;
	      }
	      if ( debug ){
		sprintf (msg, "recoFMI - No match for ObsId %s", obsId) ;
		XTE_Fcecho(msg);
	      }
	      start = (long *) malloc (nrows * sizeof(long)) ;
	      stop = (long *) malloc (nrows * sizeof(long)) ;
	      ffgcvj (miunit, 2, 1, 1, nrows, 0, start, &any, &status) ;
	      ffgcvj (miunit, 3, 1, 1, nrows, 0, stop, &any, &status) ;
	      if (status) {
		sprintf (msg, "recoFMI - Could not read times in file %s",
			 FMIName) ;
		XTE_Fcecho(msg);
		ffgerr(status, text);
		exit (1) ;
	      }
	      rowNum = 0 ;
	      while ( rowNum < nrows ) {
		if ( instop > start[rowNum] )
		  rowNum++ ;
		else
		  break ;
	      }
	      if ( rowNum && ( instart < stop[rowNum-1] ) ) {
		sprintf (msg, "recoFMI - Row %d in file %s overlaps with row %d",
			 irow, inputFile, rowNum) ;
		XTE_Fcecho(msg);
		free (start) ;
		free (stop) ;
		continue ;
	      }
	      free (start) ;
	      free (stop) ;

/*
 *   -------------------
 * - Create a new rowNum -
 *   -------------------
 */
	      ffirow (miunit, rowNum, 1, &status) ;
	      rowNum++ ;
	      nrows++ ;
	      if (status) {
		sprintf (msg, "recoFMI - Could not insert row %d in file %s",
			 rowNum, FMIName) ;
		XTE_Fcecho(msg);
		ffgerr(status, text);
		exit (1) ;
	      }
	    }
	  }

/*
 *   ----------------------
 * - Copy the row to rowNum -
 * - Beware: change paths   -
 *   ----------------------
 */

	  ffgtbb (inunit, irow, 1, rowlen, therow, &instatus) ;
	  if ( instatus ) {
	    sprintf (msg, "recoFMI - Failed to read row %d from file %s\n            status: %d",
		     irow, inputFile, instatus) ;
	    XTE_Fcecho(msg);
	    ffclos (inunit, &instatus) ;
	    break ;
	  }
	  ffptbb (miunit, rowNum, 1, rowlen, therow, &status) ;
	  if ( status ) {
	    sprintf (msg, "recoFMI - Failed to insert row %d in file %s\n            status: %d",
		     rowNum, FMIName, status) ;
	    XTE_Fcecho(msg);
	    ffclos (inunit, &instatus) ;
	    ffclos (miunit, &status) ;
	    exit (1) ;
	  }
	  for (i=14; i<=28; i++) {
	    c = colstring1 ;
	    ffgcvs (miunit, i, rowNum, 1, 1, "", &c, &any, &status) ;
	    strcpy (colstring2, inputDir) ;
	    strcat (colstring2, colstring1) ;
	    c = colstring2 ;
	    ffpcls (miunit, i, rowNum, 1, 1, &c, &status) ;
	  }

	}
	ffclos (inunit, &instatus) ;
      }
    }
    if ( level == 2 )
      (void) closedir (dirp2) ;
    else
      break ;
  }

/*
 *   ---------------
 * - Close it all up -
 *   ---------------
 */
  if ( check ){
    for (i=0; i<nobs; i++){
      if ( strcmp(outObsId[i], "X") ){
	sprintf (msg, "recoFMI - %s row %d: No match for ObsId %s",
		 FMIName, i+1, outObsId[i]) ;
	XTE_Fcecho(msg);
      }
    }
  }

  if ( nobs ) {
    for (i=0; i<nobs ; i++)
      free (outObsId[i]) ;
    free (outObsId) ;
  }

  (void) closedir (dirp1) ;

  if ( wenable ) {
    ffpdat (miunit, &status) ;
    ffpcks (miunit, &status) ;
  }

  if ( status ){
    sprintf (msg, "recoFMI - Terminate with status %d", status) ;
    XTE_Fcecho(msg);
  }
  ffclos (miunit, &status) ;

  free (therow) ;

  /* exit (0) ; */
  return;
}

int truncatefits (fitsfile **miunit, char *FMIName)
{
/*----------------------------------------------------------------------
 *
 *  Do the file truncation that cfitsio won't do.
 *
 ----------------------------------------------------------------------*/

  int status ;
  int blk ;
  char buf[2880] ;
  int end = 0 ;
  char *string ;
  int i, j ;
  FILE *FFin ;
  FILE *FFout ;
  char FMIDir[256], pdqfile[256], msg[256];

  strncpy(FMIDir, FMIName, strlen(FMIName)-4);
/* It helps to null-terminate... */
  FMIDir[strlen(FMIName)-4]='\000';
  sprintf(pdqfile, "%s/PDQBach", FMIDir);
  
/*  Close the FITS file and rename it  */
  ffclos (*miunit, &status) ;
  rename (FMIName, pdqfile) ;
/*  Open the file again, as well as the new file  */
  FFin = fopen (pdqfile, "r") ;
  FFout = fopen (FMIName, "w") ;

/*  Copy the headers  */
  while ( end < 2 ) {
    fread (buf, 1, 2880, FFin) ;
    string = buf ;
    for (i=0; i<36; i++,string+=80)
      if ( !strncmp (string, "END     ", 8) ) {
	end++ ;
	break ;
      }
    fwrite (buf, 1, 2880, FFout) ;
  }

/*  Close the files  */
  fclose (FFin) ;
  status = fclose (FFout) ;
/*  Delete the old file and open the new one as FITS file  */
  if ( !status )
    remove (pdqfile) ;
  sprintf (msg, "Now reopen %s", FMIName) ;
  XTE_Fcecho(msg);
  ffopen (miunit, FMIName, 1, &status) ;
  ffmrhd (*miunit, 1, &blk, &status) ;
  return status ;
}

/* This code is needed by IRAF */
  
#ifdef vms
#define F77CALL rfmi
#endif
#ifdef unix
#define F77CALL rfmi_
#endif
 
void F77CALL() 
{ 
  void Recofmi();
  
  Recofmi(); 
}
