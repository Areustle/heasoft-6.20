#include <stdio.h>
#include <string.h>
#include "fitsio.h"
#include "headas.h"
#include "hdcal.h"
#include "bat_gswdev.h"

/* 
 * CALDB routines - another version
 *
 * batkw_to_caldb_parms() - parse CALDB-applicable keyword from FITS file
 * bat_caldb_search() - search CALDB for requested dataset
 *
 * Example calling:
 * 
 * Step 1. Read relevant keywords from input file
 *
 * fits_open_file(&infile, infilename, READONLY, &status);
 * if (status) { ... error handling ... };
 * batkw_to_caldb_parms(infile, &caldb, 1, &status);
 * if (status) { ... error handling ... };
 * 
 * Step 2. Query CALDB with desired code name
 *
 *    if (strcasecmp(infilename, "CALDB") == 0) {
 *       char *codenam = "CODED_MASK";   // Code name of CALDB dataset
 *       char *expr = "-";               // Selection expression 
 *       int maxret = 1;                 // Maximum desired results
 *       char *pfile = infilename;       // String to receive query result
 *       char online[80], *ponline = online; // Dummy string to hold online
 *       long int extno[1];              // Dummy array to hold extension
 *       int nret = 0, nfound = 0;       // Number actually found
 * 
 *       bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
 * 		       &pfile, extno, &ponline, &nret, &nfound, &status);
 *       if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
 *     }	       
 * 
 */

/* 
 * batkw_to_caldb_parms - parse CALDB-applicable keyword from FITS file
 *
 * This routine reads keywords from an input file (usually a data
 * file) which are relevant to CALDB.  Namely, instrument and detector
 * values, as well as date/time information.  The data is returned in
 * the caldb struct.
 * 
 * If the values are unreadable from the input, then the defaults
 * remain in place.
 *
 * If the 'init' flag is set, then the routine will initialize the
 * structure to SWIFT/BAT for the instrument and "now" for the time.
 *
 * fitsfile *fitsptr - pointer to FITS file, already open for reading
 * struct caldbparms_struct *caldb - pointer to structure for storing
 *                    CALDB parameters.  User must provide the storage.
 * int init - initialize structure to SWIFT/BAT and "now"? 1=yes 0=no.
 * int *status - CFITSIO status code
 *
 * RETURNS: CFITSIO status code, or -1 if no data sets are found
 */

int batkw_to_caldb_parms(fitsfile *fitsptr, struct caldbparms_struct *caldb,
			 int init, int *systatus)
{
  int status = 0;
  char dateobs[80];
  int year, month, day, hour, minute;
  double second;
  char str[FLEN_CARD];

  if (systatus == 0) return NULL_INPUT_PTR;
  if (*systatus) return *systatus;
  if (caldb == 0) return (*systatus = NULL_INPUT_PTR);

  /* Initialize the structure if requested */
  if (init == 1) {
    strcpy(caldb->telescop, "SWIFT");
    strcpy(caldb->instrume, "BAT");
    strcpy(caldb->detnam, "-");
    strcpy(caldb->filter, "-");
    strcpy(caldb->strtdate, "now");
    strcpy(caldb->strttime, "now");
    strcpy(caldb->stopdate, "now");
    strcpy(caldb->stoptime, "now");
  }

  /* No file to read, so return silently with default values intact */
  if (fitsptr == 0) return *systatus;

  /* Mark this position so we can cancel any errors on the stack */
  fits_write_errmark();

  /* Read the instrument information */
  fits_read_key(fitsptr, TSTRING, "TELESCOP", str, 0, &status);
  if ((status == 0) && (strlen(str)>0)) strcpy(caldb->telescop, str);
  status = 0;

  fits_read_key(fitsptr, TSTRING, "INSTRUME", str, 0, &status);
  if ((status == 0) && (strlen(str)>0)) strcpy(caldb->instrume, str);
  status = 0;

  fits_read_key(fitsptr, TSTRING, "DETNAM", str, 0, &status);
  if ((status == 0) && (strlen(str)>0)) strcpy(caldb->detnam, str);
  status = 0;

  fits_read_key(fitsptr, TSTRING, "FILTER", str, 0, &status);
  if ((status == 0) && (strlen(str)>0)) strcpy(caldb->filter, str);
  status = 0;

  /* Read the time information, parse into CALDB format.  Note, this
     is a little convoluted, so that we can get the date and time
     separately.  It is assumed that DATE-OBS has *both* date and time
     stored within it. */
  fits_read_key(fitsptr, TSTRING, "DATE-OBS", dateobs, 0, &status);
  if (status == KEY_NO_EXIST) {
    fprintf(stderr, 
	    "================================================================\n"
	    "WARNING: the DATE-OBS keyword does not exist.  Assuming that the\n"
	    "         observation occurred today!\n");
  }
  if ((status == 0) && (strncmp(dateobs, "2001-01-01", 10) == 0)) {
    fprintf(stderr, 
	    "================================================================\n"
	    "ERROR: DATE-OBS is '2001-01-01' which indicates a data processing\n"
	    "       or telemetry conversion error.  Please check that DATE-OBS\n"
	    "       contains the correct observation date, or CALDB access will\n"
	    "       fail.\n");
    *systatus = KEY_OUT_BOUNDS;
  }
  if (status == 0) {
    fits_str2time(dateobs, &year, &month, &day, 
		  &hour, &minute, &second, &status);
    if (status == 0) {
      fits_time2str(year, month, day, 0, 0, 0, -1, caldb->strtdate, &status);
      fits_time2str(0, 0, 0, hour, minute, second, 1, caldb->strttime, &status);
    }
  }
  status = 0;

  /* Same for stop time, taken from DATE-END keyword */
  fits_read_key(fitsptr, TSTRING, "DATE-END", dateobs, 0, &status);
  if (status == 0) {
    fits_str2time(dateobs, &year, &month, &day, 
		  &hour, &minute, &second, &status);
    if (status == 0) {
      fits_time2str(year, month, day, 0, 0, 0, -1, caldb->stopdate, &status);
      fits_time2str(0, 0, 0, hour, minute, second, 1, caldb->stoptime, &status);
    }
  }
  status = 0;

  /* Cancel any errors that may have developed in this routine */
  fits_clear_errmark();

  return *systatus;
}

/* 
 * bat_caldb_search() - search CALDB for requested dataset
 *
 * This is a thin wrapper around HEADAS HDgtcalf(), with some
 * additional debugging information and some error checking/reporting.
 * The user is responsible for filling the caldb structure, and
 * passing the appropriate pointers as well.
 *
 * Many of the parameters follow the HDgtcalf() parameters exactly;
 * see the documentation for HDgtcalf() for more information.
 *
 * struct caldbparms_struct *caldb - pointer to filled CALDB structure.
 * char *codenam - code name of dataset requested, i.e. "EBOUNDS"
 * char *expr - selection expression, or "-"
 * int maxret - maximum number of files requested
 * int filenamesize - maximum filename path length requested
 * char **filenam - equivalent to char *filenam[maxret]; upon return,
 *                  these strings are filled with the requested data
 *                  set file names.
 * long *extno - pointer to storage for extension numbers
 * char **online - same format as filenam, but indicates if dataset is
 *                 "ONLINE" or not.
 * int *nret - number of filenames returned
 * int *nfound - number of file names found (could be more than *nret)
 * int *status - CFITSIO status code
 *
 * RETURNS: CFITSIO status code
 */
int bat_caldb_search(struct caldbparms_struct *caldb, char *codenam, 
		     char *expr, int maxret, int filenamesize, char **filenam,
		     long *extno, char **online, 
		     int *nret, int *nfound, int *status)
{
  int i;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;
  if (caldb == 0) return (*status = NULL_INPUT_PTR);

  /* Debugging */
  headas_chat(5, "...CALDB:telscop=%s instrume=%s detnam=%s filter=%s\n"
	      "    codenam=%s, strtdate=%s, strttime=%s, stopdate=%s, stoptime=%s\n"
	      "    expr=%s...\n",
	      caldb->telescop, caldb->instrume, 
	      caldb->detnam, caldb->filter, codenam, 
	      caldb->strtdate, caldb->strttime, caldb->stopdate, caldb->stoptime,
	      expr);

  /* Actual call */
  HDgtcalf(caldb->telescop, caldb->instrume, caldb->detnam, caldb->filter, 
	   codenam, caldb->strtdate, caldb->strttime, 
	   caldb->stopdate, caldb->stoptime, expr,
	   maxret, filenamesize, filenam, extno, online, nret, nfound, status);

  /* Error message */
  if ((*status != 0) || (*nret == 0) || (*nfound == 0)) {
    /* fprintf(stderr, "ERROR: could not find CALDB %s file\n", codenam); */
    if (*status) return *status;
    return (*status = -1);
  }

  /* Debugging: print results */
  headas_chat(5, "   (nfound=%d nret=%d)\n", *nfound, *nret);
  for (i=0; i<(*nret); i++) {
    headas_chat(5, "   (%d.  %s[%d])\n", i+1, filenam[i], extno[i]);
  }

  return *status;
}

