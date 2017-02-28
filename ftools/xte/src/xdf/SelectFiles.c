/* $Id: SelectFiles.c,v 1.5 2003/08/27 15:39:55 irby Exp $ */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <memory.h>
#include <stdarg.h>
#include <tcl.h>

#include "xdf.h"

int SelectFiles (ClientData clientData, Tcl_Interp *interp,
	       int argc, char *argv[])
/*---------------------------------------------------------------
 *
 *  SelectFiles searches the subsystem indices pointed to by the
 *  XFDB Master Index, given a set of ObsIds, subsystems,
 *  AppId/Configuration combinations, and time ranges, returning
 *  the names of the data files satisfying those selection criteria.
 *  It expects the argv strings to contain:
 *    0                  "SelectFiles"
 *    1                  Master Index path
 *    2                  ObsId list - each element:
 *                         <ObsId> <optional stuff>
 *    3                  Subsystem list - each element:
 *                         <subsys number (1:15)> <subsys name>
 *    4                  AppId/Config list - each element:
 *                         <AppId> <optional ConfigName>
 *    5                  Time Range list - each element:
 *                         yyyymmmdd at hh:mm:ss.s to yyyymmmdd at hh:mm:ss.s
 *  The result is a list of strings, each containing a FITS data
 *  table file name with complete path.
 *
 *---------------------------------------------------------------
 */
{
  int nObs ;
  char ** obsIdList ;
  int nSubs ;
  char ** subsys ;
  int iSub[15] ;
  int i, j, k, n ;
  int error ;
  char ** obsId ;
  double obsStart ;
  double obsStop ;
  char obsRange[40] ;
  double obsClkCor ;
  int obsClkApp ;
  char obsUser[33] ;
  char obsSource[17] ;
  double obsRA ;
  double obsDec ;
  char * obsSubFile[15] ;
  char obsSubFile2[15][200] ;
  int nSelect ;
  int nSel[15] ;
  char ** dataFile[15] ;
  int nTR ;
  char ** tranges ;
  double *TRstart ;
  double *TRstop ;
  int nAC ;
  char ** appconfs ;
  char * dummy ;

  char origpath[1000];


  for (i=0;i<15;i++)
    obsSubFile[i] = obsSubFile2[i] ;
/*    ----------
 *  - Get inputs -
 *    ----------
 */
  if ( argc < 6 ) {
      interp->result = "Not enough arguments for SelectFiles" ;
      return TCL_ERROR ;
  }

  error = Tcl_SplitList (interp, argv[2], &nObs, &obsIdList) ;
  if ( error != TCL_OK )
    return error ;

  error = Tcl_SplitList (interp, argv[3], &nSubs, &subsys) ;
  if ( error != TCL_OK )
    return error ;

  error = Tcl_SplitList (interp, argv[4], &nAC, &appconfs) ;
  if ( error != TCL_OK )
    return error ;

  error = Tcl_SplitList (interp, argv[5], &nTR, &tranges) ;
  if ( error != TCL_OK )
    return error ;


/*    -----------------------------------
 *  - Extract ObsIds, identify subsystems -
 *    -----------------------------------
 */
  if ( nObs ) {
    obsId = (char ** ) Tcl_Alloc (nObs * sizeof(char *)) ;
    for (i=0;i<nObs;i++) {
      obsId[i] = (char * ) Tcl_Alloc (16 * sizeof(char)) ;
      sscanf (obsIdList[i], "%s", obsId[i]) ;
    }
    Tcl_Free ((char * ) obsIdList) ;
  }
  else {
    Tcl_Free ((char * ) tranges) ;
    Tcl_Free ((char * ) subsys) ;
    Tcl_Free ((char * ) appconfs) ;
    interp->result = "No observations selected for SelectFiles" ;
    return TCL_ERROR ;
  }


  if ( nSubs ) {
    for (j=0;j<15;j++)
      iSub[j] = nSel[j] = 0 ;
    for (i=0;i<nSubs;i++) {
      sscanf (subsys[i], "%d", &j) ;
      if ( ( j > 0 ) && ( j < 16 ) )
	iSub[j-1] = 1 ;
    }
    Tcl_Free ((char * ) subsys) ;
  }
  else
    for (j=0;j<15;j++) {
      iSub[j] = 1 ;
      nSel[j] = 0 ;
    }

/*    -------------------
 *  - Convert time ranges -
 *    -------------------
 */
  if ( nTR ) {
    TRstart = (double *) Tcl_Alloc (nTR * sizeof(double)) ;
    TRstop = (double *) Tcl_Alloc (nTR * sizeof(double)) ;
    for (i=0;i<nTR;i++)	{
      TRstart[i] = date2sec (tranges[i], 0) ;
      if ( ( dummy = strstr(tranges[i], "TO") ) == NULL )
      if ( ( dummy = strstr(tranges[i], "to") ) == NULL )
      dummy = strstr(tranges[i], "To") ;
      TRstop[i] = date2sec (dummy+3, 0) ;
      if ( ( TRstart[i] < -90.0 ) || ( TRstop[i] < TRstart[i] ) ) {
        interp->result = "Illegal time range for SelectFiles" ;
	Tcl_Free ((char * ) tranges) ;
	Tcl_Free (TRstart) ;
	Tcl_Free (TRstop) ;
	return TCL_ERROR ;
      }
    }
    Tcl_Free ((char * ) tranges) ;
  }
  else {
    nTR = 1 ;
    TRstart = (double *) Tcl_Alloc (sizeof(double)) ;
    TRstop = (double *) Tcl_Alloc (sizeof(double)) ;
    *TRstart = 0.0 ;
    *TRstop = 1.0e9 ;
  }

/*    --------------------
 *  - Select the happy few -
 *    --------------------
 */
  strcpy(origpath,argv[1]);
  nSelect = 0 ;
  for (i=0;i<nObs;i++) {
    if ( k = getObs (interp, origpath, obsId[i], &obsStart, &obsStop, obsRange,
                     &obsClkCor, &obsClkApp, obsUser, obsSource,
		     &obsRA, &obsDec, obsSubFile) ) {
      if ( !obsClkApp )
	obsClkCor = 0.0 ;
	for (j=0;j<15;j++)
	  if ( iSub[j] )
	    nSelect += getFiles (nSel+j, dataFile+j, origpath, obsSubFile[j], 
				 obsClkCor, nTR, TRstart, TRstop, nAC, 
				 appconfs) ;
    }
    else
      fprintf (stderr, "Observation %s not available\n", obsId[i]) ;
    Tcl_Free (obsId[i]) ;
  }
  Tcl_Free (obsId) ;

/*    --------------
 *  - Write them out -
 *    --------------
 */
  if ( nSelect ) {
    n = strlen (origpath) ;               /* Strip off FMI path */
    if ( origpath[n-1] != '/' ) n++ ;
    for (j=0;j<15;j++)
      if ( iSub[j] && nSel[j] ) {
        for (i=0;i<nSel[j];i++) {
          /* Tcl_AppendElement (interp, dataFile[j][i]+n) ; */
          Tcl_AppendElement (interp, dataFile[j][i]) ;
	  Tcl_Free (dataFile[j][i]) ;
	}
        Tcl_Free (dataFile[j]) ;
      }
  }
  else
    interp->result = "No data files selected" ;

/*    ----------------------
 *  - Tcl_Free memory and return -
 *    ----------------------
 */
  Tcl_Free ((char *) appconfs) ;
  Tcl_Free (TRstart) ;
  Tcl_Free (TRstop) ;

  return TCL_OK ;
}

int getFiles (int * nSel, char *** dataFile, char *origpath, char * obsSubFile,
	      double tZero, int nTR, double * TRstart, double * TRstop,
	      int nAC, char ** appconfs)
/*---------------------------------------------------------------
 *
 *  getFiles searches the subsystem index in <obsSubFile> and
 *  extracts the data table file names that satisfy the selection
 *  criteria set by <nTR> time ranges defined by <TRstart> and
 *  <TRstop>, and the <nAC> AppId/Config combinations in <appconfs>.
 *  These are then provided with a complete path and added to the
 *  list <dataFile> unless they already exist in that list.
 *  *nSel is the number of entries in the list.
 *  The number of new entries is returned.
 *
 *---------------------------------------------------------------
 */
{
  int i, j, k ;
  int col ;
  int select ;
  fitsfile *unit ;
  int blk ;
  int status ;
  long nRows ;
  long nCols ;
  int any ;
  char comment[73] ;
  char colName[69] ;
  char tType[10] ;
  char newItem[200] ;
  char path[1000] ;
  char ** colData ;
  char ** modeName ;
  int modeCol ;
  int addSel ;
  double *start ;
  double *stop ;
  char newpath[1000];

/*    ----------------------------
 *  - Initialize and open the file -
 *    ----------------------------
 */
  strcpy (path, origpath);
  strcat (path,"/");
  strcat (path, obsSubFile) ;
  strcpy (newpath,path);
  if (strrchr(path,(int)'/')) {
    /* remove the last bit of the path */
    *( ( strrchr (path, (int) '/') ) ) = 0 ;
  } else {
    /* there is no path effectivly */
    *(path) = 0;
  }
  modeCol = 0 ;
  addSel = 0 ;
  status = 0 ;
  memset (comment, ' ', 73) ;
  comment[72] = 0 ;

  ffopen (&unit, newpath, 0, &status) ;
  if ( status ) {
    printf  ("failed to open file %s with status %d\n",newpath,status);
    return 0;
  }

/*    --------------------------
 *  - Get the header information -
 *    --------------------------
 */
  ffmrhd (unit, 1, &blk, &status) ;
  ffgkyj (unit, "NAXIS2", &nRows, comment, &status) ;
  if ( !nRows || status ) {              /* empty table */
    ffclos (unit, &status) ;
    return 0 ;
  }
  ffgkyj (unit, "TFIELDS", &nCols, comment, &status) ;

/*    --------------------
 *  - Allocate data memory -
 *    --------------------
 */
#define FitsStrBufLen 59

  start = (double *) Tcl_Alloc (nRows * sizeof(double)) ;
  stop = (double *) Tcl_Alloc (nRows * sizeof(double)) ;
  colData = (char **) Tcl_Alloc (nRows * sizeof(char *)) ;
  colData[0] = (char *) Tcl_Alloc (60 * nRows * sizeof(char)) ;
  modeName = (char **) Tcl_Alloc (nRows * sizeof(char *)) ;
  modeName[0] = (char *) Tcl_Alloc (60 * nRows * sizeof(char)) ;
  for (i=0;i<nRows;i++) {
    colData[i] = colData[0] + i * 60;
    modeName[i] = modeName[0] + i * 60;
  }

/*
  colData = (char **) Tcl_Alloc (nRows * sizeof(char *)) ;
  modeName = (char **) Tcl_Alloc (nRows * sizeof(char *)) ;
  for (i=0;i<nRows;i++) {
    colData[i] = (char *) Tcl_Alloc (60 * sizeof(char)) ;
    modeName[i] = (char *) Tcl_Alloc (60 * sizeof(char)) ;
  }
*/

/*    --------------------------------
 *  - Get the row start and stop times -
 *    --------------------------------
 */

  ffgcvd (unit, 2, 1, 1, nRows, 0.0, start, &any, &status) ;
  ffgcvd (unit, 3, 1, 1, nRows, 0.0, stop, &any, &status) ;

/*    ---------------------
 *  - Loop over the columns -
 *    ---------------------
 */
  for (col=4;col<=nCols;col++) {
    sprintf (tType, "TTYPE%d", col) ;
    ffgkys (unit, tType, colName, comment, &status) ;

/*        ----------------
 *      - Non-data columns -
 *        ----------------
 */
    if ( strstr (colName, "Mode") ) {
      if ( strstr (colName, "ModeNm") )
	modeCol = col ;
    }

/*        ------------
 *      - Data columns -
 *        ------------
 */
    else {

/*            --------------------------------
 *          - Only if the AppId is in the list -
 *            --------------------------------
 */
      j = 0 ;
      for (i=0;i<nAC;i++)
	if ( strstr(appconfs[i], colName) )
	  j++ ;
      if ( j ) {

/*            ------------------
 *          - Get the file names -
 *            ------------------
 */
	ffgcvs (unit, col, 1, 1, nRows, " ", colData, &any, &status) ; 

/*                ----------------------------
 *              - Columns with a configuration -
 *                ----------------------------
 */
	if ( modeCol )
	  ffgcvs (unit, modeCol, 1, 1, nRows, " ", modeName, &any,
                  &status) ;

/*            ------------------
 *          - Loop over the rows -
 *            ------------------
 */
	for (i=0;i<nRows;i++) {

/*                ----------------
 *              - If nothing, skip -
 *                ----------------
 */
          if ( !strlen(colData[i]) || ( *colData[i] == ' ' ) )
            continue ;

/*                --------------------
 *              - With a configuration -
 *                --------------------
 */
	  if ( modeCol ) {
            sprintf (newItem, "%-10s  %s", colName, modeName[i]) ;
	    j = 0 ;

/*                    -----------------
 *                  - If no match, skip -
 *                    -----------------
 */
	    for (k=0;k<nAC;k++)
	      if ( !strcmp (newItem, appconfs[k]) )
		j++ ;
	      if ( !j )
		continue ;
	  }

/*                -----------------------
 *              - Check and add data file -
 *                -----------------------
 */
	  if ( !strncmp (colData[i], "FPclock", 7) )
	    sprintf (newItem, "%s/clock/%s", path, colData[i]) ;
	  else if ( !strncmp (colData[i], "FPorbit", 7) )
	    sprintf (newItem, "%s/orbit/%s", path, colData[i]) ;
	  else
	    sprintf (newItem, "%s/%s", path, colData[i]) ;
	  if ( k = checkTime (newItem, tZero, nTR, TRstart, TRstop) )
	    {

	      /* printf ("datafile %s\n",newItem); */
	      k = addItem (dataFile, *nSel, newItem) ;
	    }
	  *nSel += k ;
	  addSel += k ;

/*            ---------------
 *          - End of row loop -
 *            ---------------
 */
	}

/*    -------------------------------------
 *  - Finish up column / End of column loop -
 *    -------------------------------------
 */
      }
      modeCol = 0 ;
    }
  }

/*    ----------------
 *  - Free data memory -
 *    ----------------
 */
  ffclos (unit, &status) ;
/*
  for (i=0;i<nRows;i++) {
    Tcl_Free (colData[i]) ;
    Tcl_Free (modeName[i]) ;
  }
*/
  Tcl_Free (colData[0]);
  Tcl_Free (colData) ;
  Tcl_Free (modeName[0]);
  Tcl_Free (modeName) ;
  Tcl_Free (start) ;
  Tcl_Free (stop) ;

/*    ------
 *  - Return -
 *    ------
 */
  return addSel ;
}

int checkTime (char *fileName, double tZero,
	       int nTR, double * TRstart, double * TRstop)
/*---------------------------------------------------------------
 *
 *  checkTime looks to see whether the file <fileName> is
 *  wholly or partially contained in one or more of the <nTR>
 *  <TRstart>-to-<TRstop> time ranges.
 *  It returns the number of time ranges covered by the file.
 *
 *---------------------------------------------------------------
 */
{
  char * sName ;
  unsigned long start ;
  unsigned long stop ;
  double tstart ;
  double tstop ;
  int i ;
  int k ;

/*    ----------------
 *  - Decode file name -
 *    ----------------
 */
  if ( ( sName = strrchr (fileName, (int) '/') ) == NULL )
    sName = fileName - 1 ;
  sName += 6 ;
  if ( sscanf (sName, "%lx-%lx", &start, &stop) != 2 )
    return 1 ;
  tstart = start + tZero ;
  tstop = stop + tZero ;

/*    ---------------
 *  - Check on ranges -
 *    ---------------
 */
  k = 0 ;
  for (i=0;i<nTR;i++)
    if ( ( tstart < TRstop[i]) && ( tstop > TRstart[i] ) )
      k++ ;

/*    ------
 *  - Return -
 *    ------
 */
  return k ;
}
