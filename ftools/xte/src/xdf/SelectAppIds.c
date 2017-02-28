/* $Id: SelectAppIds.c,v 1.6 2003/08/27 15:39:55 irby Exp $ */

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

int SelectAppIds (ClientData clientData, Tcl_Interp *interp,
	       int argc, char *argv[])
/*---------------------------------------------------------------
 *
 *  SelectAppIds searches the subsystem indices pointed to by the
 *  XFDB Master Index, given a set of ObsIds and subsystems, for
 *  available AppIds and Configurations.
 *  It expects the argv strings to contain:
 *    0                  "SelectAppIds"
 *    1                  Master Index path
 *    2                  ObsId list - each element:
 *                         <ObsId> <optional stuff>
 *    3                  Subsystem list - each element:
 *                         <subsys number (1:15)> <subsys name>
 *  The result is a list of strings, each containing:
 *    AppId  ConfigurationName  SubsystemIndexFileName
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
  char ** subAppId[15] ;
  char path[1000];


  for (i=0;i<15;i++)
    obsSubFile[i] = obsSubFile2[i] ;
/*    ----------
 *  - Get inputs -
 *    ----------
 */
  if ( argc < 4 ) {
    interp->result = "Not enough arguments for SelectAppIds" ;
    return TCL_ERROR ;
  }

  error = Tcl_SplitList (interp, argv[2], &nObs, &obsIdList) ;
  if ( error != TCL_OK )
    return error ;

  if (nObs == 0) {
    interp->result = "No Observations Selected";
    return TCL_ERROR;
  }

  error = Tcl_SplitList (interp, argv[3], &nSubs, &subsys) ;
  if ( error != TCL_OK )
    return error ;

/*    -----------------------------------
 *  - Extract ObsIds, identify subsystems -
 *    -----------------------------------
 */
#define FitsStrBufLen 15
  obsId = (char ** ) Tcl_Alloc (nObs * sizeof(char *)) ;
  for (i=0;i<nObs;i++) {
    obsId[i] = (char * ) Tcl_Alloc (16 * sizeof(char)) ;
    sscanf (obsIdList[i], "%s", obsId[i]) ;
  }
  Tcl_Free ((char * ) obsIdList) ;

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

/*    --------------------
 *  - Select the happy few -
 *    --------------------
 */
  strcpy(path,argv[1]);
  nSelect = 0 ;
  for (i=0;i<nObs;i++) {
    if ( k = getObs (interp, path, obsId[i], &obsStart, &obsStop, obsRange,
                     &obsClkCor, &obsClkApp, obsUser, obsSource,
		     &obsRA, &obsDec, obsSubFile) ) {
      for (j=0;j<15;j++)
	if ( iSub[j] )
	  nSelect += getAppIds (interp, path, nSel+j, subAppId+j, 
				obsSubFile[j],obsId[i]) ;
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
  Tcl_ResetResult(interp);
  if ( nSelect ) {
    for (j=0;j<15;j++)
      if ( iSub[j] && nSel[j] ) {
	for (i=0;i<nSel[j];i++) {
	  Tcl_AppendElement (interp, subAppId[j][i]) ;
	  Tcl_Free (subAppId[j][i]) ;
        }
	Tcl_Free (subAppId[j]) ;
      }
  }
  else {
    interp->result = "No AppIds selected" ;
    return TCL_ERROR;
  }

/*    ----------------------
 *  - Free memory and return -
 *    ----------------------
 */

  return TCL_OK ;
}

int getObs (Tcl_Interp *interp, char * path, char * theObsId, 
	    double * obsStart, double * obsStop, char * obsRange, 
	    double * obsClkCor, int * obsClkApp,
	    char * obsUser, char * obsSource, double * obsRA, double * obsDec,
	    char ** obsSubFile)
/*---------------------------------------------------------------
 *
 *  getObs gets all items for a particular ObsId from its row in
 *  the Master Index in <path> and returns the row number.
 *
 *---------------------------------------------------------------
 */
{
  long nObs ;
  fitsfile *unit ;
  int blk ;
  int status ;
  int any ;
  char ** obsIds ;
  int i, k ;
  char * cc ;
  char c2[60] ;
  char file[200] ;
  char comment[73] ;
  char datobs[10] ;
  char timobs[10] ;
  char datend[10] ;
  char timend[10] ;
  char *tmp;
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
  if ( i=strlen(path) ) {
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
  if ( status )
    return -1 ;

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
  obsIds = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;
  obsIds[0] = (char *) Tcl_Alloc (16 * nObs * sizeof(char)) ;

  for (i=0;i<nObs;i++)
    obsIds[i] = obsIds[0] + i * 16;

/*
  obsIds = (char **) Tcl_Alloc (nObs * sizeof(char*)) ;

  for (i=0;i<nObs;i++)
    obsIds[i] = (char *) Tcl_Alloc (16 * sizeof(char)) ;
*/

/*    ------------
 *  - Identify row -
 *    ------------
 */
  ffgcvs (unit, 1, 1, 1, nObs, "", &obsIds[0], &any, &status) ;
  k = 0 ;
  for (i=0;i<nObs;i++)
    if ( !strcmp(obsIds[i], theObsId) ) {
      k = i + 1 ;
      break ;
    }
  if ( !k )
    return 0 ;

/*    --------------------------------
 *  - Get the goods and close the file -
 *    --------------------------------
 */
  ffgcvd (unit, 2, k, 1, 1, 0.0, obsStart, &any, &status) ;
  ffgcvd (unit, 3, k, 1, 1, 0.0, obsStop, &any, &status) ;
  cc = datobs ;
  tmp = datobs;
  ffgcvs (unit, 4, k, 1, 1, "", &tmp, &any, &status) ;
  cc = timobs ;
  tmp = timobs;
  ffgcvs (unit, 5, k, 1, 1, "", &tmp, &any, &status) ;
  cc = datend ;
  tmp = datend;
  ffgcvs (unit, 6, k, 1, 1, "", &tmp, &any, &status) ;
  cc = timend ;
  tmp = timend;
  ffgcvs (unit, 7, k, 1, 1, "", &tmp, &any, &status) ;
  ffgcvd (unit, 8, k, 1, 1, 0.0, obsClkCor, &any, &status) ;
  ffgcl (unit, 9, k, 1, 1, (char *) &obsClkApp, &status) ;
  cc = obsUser ;
  ffgcvs (unit, 10, k, 1, 1, "", &obsUser, &any, &status) ;
  cc = obsSource ;
  ffgcvs (unit, 11, k, 1, 1, "", &obsSource, &any, &status) ;
  ffgcvd (unit, 12, k, 1, 1, 0.0, obsRA, &any, &status) ;
  ffgcvd (unit, 13, k, 1, 1, 0.0, obsDec, &any, &status) ;
#define FitsStrBufLen 59
  for (i=0;i<15;i++) {
    cc = c2 ;
    tmp = c2;
    ffgcvs (unit, 14+i, k, 1, 1, "", &tmp ,&any, &status) ;
    /* sprintf (obsSubFile[i], "%s/%s", path, c2) ; */
    sprintf (obsSubFile[i], "%s", c2) ;
  }

  ffclos (unit, &status) ;

/*    -------------------------------------------
 *  - Construct the time range, correct the times -
 *    -------------------------------------------
 */
  sprintf (obsRange,"%s %s to %s %s",
	   datobs, timobs, datend, timend) ;
  *obsStart += *obsClkCor ;
  *obsStop += *obsClkCor ;

/*    ------------
 *  - Free scratch -
 *    ------------
 */
  
  /* for (i=0;i<nObs;i++) */
  Tcl_Free (obsIds[0]) ;

  Tcl_Free (obsIds) ;

/*    ------
 *  - Return -
 *    ------
 */
  return k ;
}

int getAppIds (Tcl_Interp *interp, char * path, int * nSel, char *** subAppId,
	       char * obsSubFile, char * theObsId)
/*---------------------------------------------------------------
 *
 *  getAppIds searches the subsystem index in <obsSubFile> and
 *  extracts the AppIds and ConfigNames that are actually present.
 *  These are then added to the list <subAppId> unless they already
 *  exist in that list.  *nSel is the number of entries in the
 *  list.  The number of new entries is returned.
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
  char newItem[100] ;
  char ** colData ;
  int modeCol ;
  int addSel ;
  char basefilename[200];
  char newfilename[1000];

  char *tclvalue;
  struct stat fmifile;

/*    ----------------------------
 *  - Initialize and open the file -
 *    ----------------------------
 */
  modeCol = 0 ;
  addSel = 0 ;
  status = 0 ;
  memset (comment, ' ', 73) ;
  comment[72] = 0 ;
  strcpy(basefilename,obsSubFile);
  sprintf (newfilename, "%s/%s",path,obsSubFile);

  /* is the file there? */

  if (stat(newfilename,&fmifile)) 
    {
      tclvalue = Tcl_GetVar(interp,"ftpoption",TCL_GLOBAL_ONLY);
      if (!strcmp("FTP Any Files",tclvalue)) {
	if (*basefilename == 'A') {
	  /* full path */
	  status = Tcl_VarEval(interp,"ftp_get_file legacy.gsfc.nasa.gov /xte/data/archive/",
			       basefilename," ",
			       newfilename,(char *) NULL);
	} else if (*basefilename == 'F') {
	  /* no path, need to add in the obsid path */
	  status = Tcl_VarEval(interp,"ftp_get_file legacy.gsfc.nasa.gov /xte/data/archive/[makefullftppath ",
			       theObsId,"/",basefilename,"] ",
			       newfilename,(char *) NULL);
	} else {
	  status = Tcl_VarEval(interp,"ftp_get_file legacy.gsfc.nasa.gov /xte/data/archive/[makefullftppath ",
			       basefilename,"] ",
			       newfilename,(char *) NULL);
	}
      }
    }
  status = 0;

  ffopen (&unit, newfilename, 0, &status) ;
  if ( status )
    return 0 ;

/*    --------------------------
 *  - Get the header information -
 *    --------------------------
 */
  nRows = 0;
  nCols = 0;
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
  colData = (char **) Tcl_Alloc (nRows * sizeof(char *)) ;
  colData[0] = (char *) Tcl_Alloc (60 * nRows * sizeof(char)) ;
  for (i=0;i<nRows;i++) {
    colData[i] = colData[0] + i * 60;
  }

/*
  colData = (char **) Tcl_Alloc (nRows * sizeof(char *)) ;
  for (i=0;i<nRows;i++)
    colData[i] = (char *) Tcl_Alloc (60 * sizeof(char)) ;
*/

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
      ffgcvs (unit, col, 1, 1, nRows, " ", colData, &any, &status) ;
      j = 0 ;
      for (i=0;i<nRows;i++)
	if ( strlen(colData[i]) && ( *colData[i] != ' ' ) )
	  j++ ;
      if ( j ) {

/*            ----------------------------
 *          - Columns with a configuration -
 *            ----------------------------
 */
      if ( modeCol ) {
        ffgcvs (unit, modeCol, 1, 1, nRows, " ", colData, &any,
                &status) ;
        for (i=0;i<nRows;i++)
          if ( strlen(colData[i]) && ( *colData[i] != ' ' ) ) {
            sprintf (newItem, "%-10s  %s", colName, colData[i]) ;
	    k = addItem (subAppId, *nSel, newItem) ;
            *nSel += k ;
            addSel += k ;
          }
      }

/*            -------------------------------
 *          - Columns without a configuration -
 *            -------------------------------
 */
      else {
        k = addItem (subAppId, *nSel, colName) ;
	*nSel += k ;
        addSel += k ;
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
  for (i=0;i<nRows;i++)
    Tcl_Free (colData[i]) ;
*/
  Tcl_Free (colData[0]);
  Tcl_Free (colData) ;

/*    ------
 *  - Return -
 *    ------
 */
  return addSel ;
}

int addItem (char *** subAppId, int nItems, char * newItem)
/*---------------------------------------------------------------
 *
 *  addItem looks to see whether newItem is already contained
 *  in subAppId; if not, it is added.
 *  addItem also takes care of allocating memory for subAppId.
 *
 *---------------------------------------------------------------
 */
{
  char ** sArray ;
  int i ;

/*    --------------------
 *  - Check existing items -
 *    --------------------
 */
  for (i=0;i<nItems;i++)
    if ( !strcmp (newItem, (*subAppId)[i]) )
      return 0 ;

/*    -----------------------------
 *  - Additional pointer allocation -
 *    -----------------------------
 */
  if ( nItems%50 == 0 ) {
    sArray = Tcl_Alloc ((nItems+50) * sizeof(char *)) ;
    for (i=0;i<nItems;i++)
      sArray[i] = (*subAppId)[i] ;
    if (nItems)
      Tcl_Free (*subAppId) ;
    *subAppId = sArray ;
  }

/*    ------------------------------
 *  - String allocation and add item -
 *    ------------------------------
 */
  (*subAppId)[nItems] = Tcl_Alloc (100 * sizeof(char)) ;
  strcpy ((*subAppId)[nItems], newItem) ;

/*    ------
 *  - Return -
 *    ------
 */
  return 1 ;
}
