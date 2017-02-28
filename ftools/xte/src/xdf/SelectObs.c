/* $Id: SelectObs.c,v 1.5 2003/08/27 15:39:55 irby Exp $ */



#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <memory.h>
#include <stdarg.h>
#include <tcl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "xdf.h"
#define FitsStrBufLen 79

int SelectObs (ClientData clientData, Tcl_Interp *interp,
	       int argc, char *argv[])
/*---------------------------------------------------------------
 *
 *  SelectObs searches the XFDB Master Index for observations
 *  that match a set of source names and/or a set of time range
 *  constraints.  It expects the argv strings to contain:
 *    0                  "SelectObs"
 *    1                  Master Index path
 *    2                  source name list
 *    3                  time range list
 *  The result is a list of strings, each containing:
 *    ObsId  Source StartDate StartTime Stopdate StopTime
 *
 *---------------------------------------------------------------
 */
{
  int nSrc ;
  char ** snames ;
  int freesn ;
  int nTR ;
  char ** tranges ;
  int freetr ;
  double *TRstart ;
  double *TRstop ;
  int i ;
  int error ;
  char ** obsId ;
  char ** obsRange ;
  char ** source ;
  double * obsStart ;
  double * obsStop ;
  int nObs ;
  int nSelect ;
  char line[256] ;
  char *dummy ;

/*    ----------
 *  - Get inputs -
 *    ----------
 */
  if ( argc < 4 )
    {
      interp->result = "Not enough arguments for SelectObs" ;
      return TCL_ERROR ;
    }

  error = Tcl_SplitList (interp, argv[2], &nSrc, &snames) ;
  if ( error != TCL_OK )
    return error ;

  error = Tcl_SplitList (interp, argv[3], &nTR, &tranges) ;
  if ( error != TCL_OK )
    return error ;

  freesn = 1 ;
  if ( !nSrc )
    {
      freesn = 0 ;
      nSrc = 1 ;
      *snames = "*" ;
    }


/*    -------------------
 *  - Convert time ranges -
 *    -------------------
 */
  if ( nTR )
    {
      freetr = 1 ;
      TRstart = (double *) Tcl_Alloc (nTR * sizeof(double)) ;
      TRstop = (double *) Tcl_Alloc (nTR * sizeof(double)) ;
      for (i=0;i<nTR;i++)
	{
	  TRstart[i] = date2sec (tranges[i], 0) ;
	  if ( ( dummy = strstr(tranges[i], "TO") ) == NULL )
	    if ( ( dummy = strstr(tranges[i], "to") ) == NULL )
	      dummy = strstr(tranges[i], "To") ;
	  TRstop[i] = date2sec (dummy+3, 0) ;
	  if ( ( TRstart[i] < -90.0 ) || ( TRstop[i] < TRstart[i] ) )
	    {
	      interp->result = "Illegal time range for SelectObs" ;
	      Tcl_Free (TRstart) ;
	      Tcl_Free (TRstop) ;
	      return TCL_ERROR ;
	    }
	}
    }
  else
    {
      freetr = 0 ;
      nTR = 1 ;
      TRstart = (double *) Tcl_Alloc (sizeof(double)) ;
      TRstop = (double *) Tcl_Alloc (sizeof(double)) ;
      *TRstart = 0.0 ;
      *TRstop = 1.0e9 ;
    }

/*    --------------------
 *  - Get the Master Index -
 *    --------------------
 */
  nObs = getObsIds (interp, argv[1], &obsId, &source, &obsStart, &obsStop, 
		    &obsRange) ;

/*    --------------------
 *  - Select the happy few -
 *    --------------------
 */
  Tcl_ResetResult(interp);
  nSelect = 0 ;
  for (i=0;i<nObs;i++)
    if ( matchObs (source[i], obsStart[i], obsStop[i], nSrc, snames,
		   nTR, TRstart, TRstop) )
      {
	sprintf (line, "%-15s  %-16s  %s", obsId[i], source[i], obsRange[i]) ;
	Tcl_AppendElement (interp, line) ;
	nSelect++ ;
      }
  if ( !nSelect ) {
    interp->result = "No observations selected" ;
    return TCL_ERROR;
  }

/*    ----------------------
 *  - Free memory and return -
 *    ----------------------
 */
  Tcl_Free (TRstart) ;
  Tcl_Free (TRstop) ;

  Tcl_Free (obsId[0]) ;
  Tcl_Free (source[0]) ;
  Tcl_Free (obsRange[0]) ;


  Tcl_Free (obsId) ;
  Tcl_Free (source) ;
  Tcl_Free (obsStart) ;
  Tcl_Free (obsStop) ;
  Tcl_Free (obsRange) ;
  if ( freesn )
    Tcl_Free ((char * ) snames) ;
  if ( freetr )
    Tcl_Free ((char * ) tranges) ;

  return TCL_OK ;
}

int getObsIds (Tcl_Interp *interp, char *path, char *** obsId, char *** source,
	       double ** obsStart, double ** obsStop, char *** obsRange)
/*---------------------------------------------------------------
 *
 *  getObsIds pulls the ObsIds, source names, start and stop times,
 *  and a time range string from the Master Index in path.
 *
 *---------------------------------------------------------------
 */
{
  long nObs ;
  fitsfile *unit;
  int blk ;
  int status ;
  int any ;
  int i ;
  char file[200] ;
  char comment[73] ;
  char ** datobs ;
  char ** timobs ;
  char ** datend ;
  char ** timend ;
  double *tzero ;
  double *p1 ;
  double *p2 ;
  char *tclvalue;

  /* stuff for stat */
  struct stat fmifile;

/*    ----------------------------
 *  - Initialize and open the file -
 *    ----------------------------
 */
  status = 0 ;
  memset (comment, ' ', 73) ;
  comment[72] = 0 ;
  if ( i=strlen(path) )
    {
      strcpy (file, path) ;
      if ( file[i-1] == '/' )
	file[i-1] = 0 ;
    }
  else
    strcpy (file, ".") ;
  strcat (file, "/FMI") ;

  /* is the file there? */

  if (stat(file,&fmifile)) 
    {
      tclvalue = Tcl_GetVar(interp,"ftpoption",TCL_GLOBAL_ONLY);
      if (!strcmp("FTP Any Files",tclvalue)) {
	status = Tcl_VarEval(interp,"ftp_get_file legacy.gsfc.nasa.gov /xte/data/archive/FMI ",file,(char *) NULL);
      }
    }
  status = 0;
  ffopen (&unit, file, 0, &status) ;
  if ( status ) {
    return -1 ;
  }
  
/*    --------------------------
 *  - Get the header information -
 *    --------------------------
 */
  ffmrhd (unit, 1, &blk, &status) ;
  ffgkyj (unit, "NAXIS2", &nObs, comment, &status) ;

/*    ---------------
 *  - Allocate memory -
 *    ---------------
 */
  *obsStart = (double *) Tcl_Alloc (nObs * sizeof(double)) ;
  *obsStop = (double *) Tcl_Alloc (nObs * sizeof(double)) ;
  tzero = (double *) Tcl_Alloc (nObs * sizeof(double)) ;
  *obsId = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  (*obsId)[0] = (char *) Tcl_Alloc (80 * nObs * sizeof(char)) ;
  *source = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  (*source)[0] = (char *) Tcl_Alloc (80 * nObs * sizeof(char)) ;
  *obsRange = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  (*obsRange)[0] = (char *) Tcl_Alloc (80 * nObs * sizeof(char)) ;
  datobs = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  datobs[0] = (char *) Tcl_Alloc (80 * nObs * sizeof(char)) ;
  timobs = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  timobs[0] = (char *) Tcl_Alloc (80 * nObs * sizeof(char)) ;
  datend = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  datend[0] = (char *) Tcl_Alloc (80 * nObs * sizeof(char)) ;
  timend = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  timend[0] = (char *) Tcl_Alloc (80 * nObs * sizeof(char)) ;

  for (i=1;i<nObs;i++)
    {
      (*obsId)[i] = (*obsId)[0] + i * 80;
      (*source)[i] = (*source)[0] + i * 80;
      (*obsRange)[i] = (*obsRange)[0] + i * 80;
      datobs[i] = datobs[0] + i * 80;
      timobs[i] = timobs[0] + i * 80;
      datend[i] = datend[0] + i * 80;
      timend[i] = timend[0] + i * 80;
    }
/*
  for (i=0;i<nObs;i++)
    {
      (*obsId)[i] = (char *) Tcl_Alloc (80 * sizeof(char)) ;
      (*source)[i] = (char *) Tcl_Alloc (80 * sizeof(char)) ;
      (*obsRange)[i] = (char *) Tcl_Alloc (80 * sizeof(char)) ;
      datobs[i] = (char *) Tcl_Alloc (80 * sizeof(char)) ;
      timobs[i] = (char *) Tcl_Alloc (80 * sizeof(char)) ;
      datend[i] = (char *) Tcl_Alloc (80 * sizeof(char)) ;
      timend[i] = (char *) Tcl_Alloc (80 * sizeof(char)) ;
    }
*/

/*    --------------------------------
 *  - Get the goods and close the file -
 *    --------------------------------
 */
  ffgcvs (unit, 1, 1, 1, nObs, "", &(*obsId)[0], &any, &status) ;
  ffgcvd (unit, 2, 1, 1, nObs, 0.0, *obsStart, &any, &status) ;
  ffgcvd (unit, 3, 1, 1, nObs, 0.0, *obsStop, &any, &status) ;
  ffgcvs (unit, 4, 1, 1, nObs, "", &datobs[0], &any, &status) ;
  ffgcvs (unit, 5, 1, 1, nObs, "", &timobs[0], &any, &status) ;
  ffgcvs (unit, 6, 1, 1, nObs, "", &datend[0], &any, &status) ;
  ffgcvs (unit, 7, 1, 1, nObs, "", &timend[0], &any, &status) ;
  ffgcvd (unit, 8, 1, 1, nObs, 0.0, tzero, &any, &status) ;
  ffgcvs (unit, 11, 1, 1, nObs, "", &(*source)[0], &any, &status) ;

  ffclos (unit, &status) ;

/*    --------------------------------------------
 *  - Construct the time ranges, correct the times -
 *    --------------------------------------------
 */
  for (i=0;i<nObs;i++)
    sprintf ((*obsRange)[i],"%s %s to %s %s",
	     datobs[i], timobs[i], datend[i], timend[i]) ;
  for (i=0,p1=*obsStart,p2=tzero;i<nObs;i++)
    *(p1++) += *(p2++) ;
  for (i=0,p1=*obsStop,p2=tzero;i<nObs;i++)
    *(p1++) += *(p2++) ;

/*    ------------
 *  - Free scratch -
 *    ------------
 */

  Tcl_Free (datobs[0]) ;
  Tcl_Free (timobs[0]) ;
  Tcl_Free (datend[0]) ;
  Tcl_Free (timend[0]) ;

  Tcl_Free (datobs) ;
  Tcl_Free (timobs) ;
  Tcl_Free (datend) ;
  Tcl_Free (timend) ;
  Tcl_Free (tzero) ;

/*    ------
 *  - Return -
 *    ------
 */
  return nObs ;
}

int matchObs (char *source, double obsStart, double obsStop,
	      int nSrc, char ** snames,
	      int nTR, double *TRstart, double *TRstop)
/*---------------------------------------------------------------
 *
 *  matchObs matches an observation of source <source>, running
 *  from <obsStart> to <obsStop>, against a list of <nSrc> source
 *  names, <snames>, and a list of <nTR> time ranges defined by
 *  <TRstart> and <TRstop>.  Elements within the same list are OR-ed,
 *  the lists themselves are AND-ed with each other.
 *
 *---------------------------------------------------------------
 */
{
  int i ;
  int select ;

/*    -----------------
 *  - Match time ranges -
 *    -----------------
 */
  select = 0 ;
  for (i=0;i<nTR;i++)
    if ( ( obsStart < *TRstop ) && ( obsStop > *TRstart ) )
      {
	select++ ;
	break ;
      }

/*    ---------------------------------
 *  - If successful, match source names -
 *    ---------------------------------
 */
  if ( select )
    for (i=0;i<nSrc;i++)
      if ( select = Tcl_StringMatch (source, snames[i]) )
	break ;

/*    ------
 *  - Return -
 *    ------
 */
  return select ;
}

#if defined(__cplusplus)
double date2sec (char *datetime, int timesys=0)
#else
double date2sec (char *datetime, int timesys)
#endif
/*---------------------------------------------------------------
 *
 *  date2sec converts a date-time string of the form:
 *      <yyyyMMMdd> at <hh:mm:ss.s>
 *  or:
 *      <yyMMMdd> at <hh:mm:ss.s>
 *  to spacecraft clock seconds plus clock correction.
 *  MMM is not case-sensitive.
 *  timesys indicates the time system of the datetime string:
 *    timesys = 0 : TT - subtract 60.184
 *              1 : Raw spacecraft clock - no change
 *              2 : TAI - subtract 28
 *              3 : UTC - add leap seconds
 *  (Spacecraft clock time plus clock correction plus 60.184s
 *  is TT.)
 *
 *---------------------------------------------------------------
 */
{
  const char * const months[]={"JAN","FEB","MAR","APR","MAY","JUN",
			       "JUL","AUG","SEP","OCT","NOV","DEC"} ;
  const int dayspm[]={0,31,59,90,120,151,181,212,243,273,304,334} ;
  int year ;
  int month ;
  int day ;
  int hour ;
  int minute ;
  double sec ;
  double clocksec ;
  int i ;
  char smonth[4] ;



/*      ----------------
 *    - Parse the string -
 *      ----------------
 */
  if ( sscanf (datetime, "%d%c%c%c%d at %d:%d:%lg", &year, smonth, smonth+1,
	       smonth+2, &day, &hour, &minute, &sec) < 8 )
    {
      fprintf (stderr, "===> date2sec could not parse string: %s\n", datetime) ;
      return -1.0 ;
    }


/*      --------------------------
 *    - Month string to upper case -
 *      --------------------------
 */
  for (i=0;i<3;i++)
    if ( smonth[i] > 96 )
      smonth[i] -= 32 ;
  smonth[3] = 0 ;


/*      ------------------
 *    - Get the year right -
 *      ------------------
 */
  if ( year < 90 )
    year += 6 ;
  else if ( year < 100 )
    year -= 94 ;
  else
    year -= 1994 ;
  if ( ( year < 0 ) || ( year > 20 ) )
    {
      fprintf (stderr, "===> date2sec found invalid year: %s\n", datetime) ;
      return -1.0 ;
    }


/*      -------------
 *    - Get the month -
 *      -------------
 */
  month = -1 ;
  for (i=0;i<12;i++)
    if ( !strcmp (smonth, months[i]) )
      {
	month = i ;
	break ;
      }
  if ( month < 0 )
    {
      fprintf (stderr, "===> date2sec found invalid month: %s\n", datetime) ;
      return -1.0 ;
    }


/*      ------------------------
 *    - Check and count the days -
 *      ------------------------
 */
  day-- ;                               /* make it zero-relative */
  if ( ( day < 0 ) || ( day > 30 ) )
    {
      fprintf (stderr, "===> date2sec found invalid day: %s\n", datetime) ;
      return -1.0 ;
    }
  day += dayspm[month] ;
  if ( !( (year+2)%4 ) && ( month > 1 ) )
    day++ ;                             /* past Feb 29 in a leap year */
  day += (int) (year+1) / 4 ;           /* one day for each leap year passed */
  day += 365 * year ;


/*      ------------------------
 *    - Check and count the time -
 *      ------------------------
 */
  if ( ( hour < 0 ) || ( hour > 23 ) || ( minute < 0 ) || ( minute > 59 ) ||
       ( sec < 0.0 ) || ( sec > 61.0 ) )
    {
      fprintf (stderr, "===> date2sec found invalid time: %s\n", datetime) ;
      return -1.0 ;
    }
  sec += (double) minute * 60.0 + hour * 3600.0 + day * 86400.0 ;


/*      --------------------------
 *    - The different time systems -
 *      --------------------------
 */
  switch ( timesys )
    {
    case 0:                /* TT  */
      sec -= 60.184 ;
      break ;
    case 1:                /* MET */
      break ;
    case 2:                /* TAI */
      sec -= 28.0 ;
    case 3:                /* UTC */
      if ( day > 181 )             /* leap second June 30, 1994 */
	sec++ ;
      if ( year > 1 )              /* expected leap second December 31, 1995 */
	sec++ ;
      break ;
    }

/*      ------
 *    - Return -
 */
  return sec ;
}

/*    TEST MAIN
 *int main (int argc, char *argv[])
 *{
 *
 *  char line[100] ;
 *  int mode ;
 *  double sec ;
 *
 *  printf ("Type time system mode (0-3):") ;
 *  gets (line) ;
 *  sscanf (line, "%d", &mode) ;
 *  printf ("Type date-time string; terminate with <CR>:\n") ;
 *
 *  while ( strlen ( gets (line) ) )
 *    {
 *      sec = date2sec (line, mode) ;
 *      printf ("SCCS: %f\n", sec) ;
 *    }
 *  exit (0) ;
 *}
 */
