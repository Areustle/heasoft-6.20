#ifndef ATTFILE_INCLUDED

#include "fitsio.h"
#include "longnam.h"

#include "quat.h"

/********************************************************************
* maximum time in seconds beyond the end of the attfile which can
* be safely extrapolated.
********************************************************************/
#define DEFAULT_ATTFILE_EXTRAPOLATION 32.

#define UNINITIALIZED_ATTFILE_ROW -1
#define ATTFILE_LOCAL_SEARCH_LIMIT 100
#define DIST_FOR_ATTFILE_RESET 1000


#define ATTFILE_CONSTANT  0
#define ATTFILE_LINEAR    1
#define ATTFILE_LINEAR_NEARBY    2


/*************************************************************************
* attitude file structure
*************************************************************************/
typedef struct {

char* name;   /* attitude file name */

fitsfile *fp; /* FITSIO file structure */

long nrows; /* NAXIS2 keyword value */
char mission[FLEN_VALUE]; /* value of the TELESCOP keyword */

int time_col;    /* column numbers  */
int qparam_col;  /* for the various */
int sigma_col;   /* columns in the  */
int sensor_col;  /* attitude file   */

double tstart; /* time of first attitude record */
double tstop;  /* time of last  attitude record */
double duration; /* = tstop-tstart */

double min_extrapolated; /* earliest valid extrapolated time */
double max_extrapolated; /*   latest valid extrapolated time */

long search_row; /* the most recent search result row */
double search_time0; /* the value of TIME at search_row   */
double search_time1; /* the value of TIME at search_row+1 */

QUAT* search_q0;
QUAT* search_q1;
QUAT* search_deltaq;
QUAT* hatq; /* just a dummy */
int search_qs_reliable;

int interpolation; /* by default, interpolation is linear */
int extrapolation;  /* by default, extrapolation is constant */
double limit_nearby;

} ATTFILE;



/***********************************************************************
*************************************************************************
* handle FITSIO errors while reading attitude files
* prints error messages and exits if there is an error
************************************************************************/
void checkAttFileFITSerrors(int status, char* doing, ATTFILE* file);


/**************************************************************************
***************************************************************************
* create an attfile structure and open an actual attitude file
**************************************************************************/
ATTFILE* openAttFile(char* filename );

/**************************************************************************
***************************************************************************
* close an attitude file and destroy the ATTFILE structure
**************************************************************************/
void closeAttFile(ATTFILE* file);

/**************************************************************************
***************************************************************************
* Read a generic quantity from the attitude file.
* This routine makes the FITSIO read call, checks for errors and 
* resets the search flags if needed.
* This routine should not be used by the general user. Instead, the
* column-specific functions
* readQuatFromAttFile
* readTimeFromAttFile, etc should be used.
**************************************************************************/
void readThingFromAttFile(ATTFILE* file, void* data, void* nullvalue,
                          int datatype,
                          long row, int col, long nelements);

/**************************************************************************
***************************************************************************
* Read the Q parameter from a given row of the table
**************************************************************************/
void readQuatFromAttFile(ATTFILE* file, QUAT* q,long row);

/**************************************************************************
***************************************************************************
* Read the TIME from a given row of the table
**************************************************************************/
double readTimeFromAttFile(ATTFILE* file,long row);

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file
* returns 1 if time is withing the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInAttFile(ATTFILE* file, double time);

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file, or its 
* reasonable extrapolation
* returns 1 if time is within the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInExtrapolatedAttFile(ATTFILE* file, double time);

/**************************************************************************
***************************************************************************
* Changes the valid extrapolation time limits for the attitude file.
* The default values when the file is opened is 
* DEFAULT_ATTFILE_EXTRAPOLATION.
* See also isInExtrapolatedAttFile(ATTFILE* file, double time).
**************************************************************************/
void resetAttFileExtrapolationLimits(ATTFILE* file, double margin );

/**************************************************************************
***************************************************************************
* Locates a given time value in the attitude file
* the row will be the last row in the file which has a time value 
* less than or equal to the specified time. 
* if the time is before the table the first row in the table is returned.
* if the time is at or beyond the end of the table the second to last row
* will be returned.
* In other words, this routine returns the first of the pair of rows 
* which should be used to interpolate to the specified time.
**************************************************************************/
long findTimeInAttFile(ATTFILE* file, double time);

/**************************************************************************
***************************************************************************
* determines the quaternion at an arbitrary time
* values are interpolated if necessary and things are optimized
* for the case of repeated calls to this routine with similar time values
* Returns true if the request meets search constraints.
**************************************************************************/
int findQuatInAttFile(ATTFILE* file, QUAT* q, double time);

/**************************************************************************
***************************************************************************
* Sets search constraint so success requires attitude record within <limit>
* of requested time (or clears the constraint if <limit> < 0).
**************************************************************************/
void requireTimeNearAttFileRecord (ATTFILE * file, double limit);

/**************************************************************************
***************************************************************************
* Enable or disable interpolation of the surrounding pair of quaternions.
* By default, quaternions for times within the 'extrapolated' file are
* linearly interpolated and those for times outside are constant.
**************************************************************************/
void setAttFileInterpolation (ATTFILE * file, int mode);
void setAttFileExtrapolation (ATTFILE * file, int mode);

/**************************************************************************
***************************************************************************
* Move the current search row ahead by one row in the attitude file.
* Note that a search (findTimeInAttFile or findQuatInAttFile) must
* be done first to initialize the current search row.
* This routine will not advance beyound the end of the file, but will
* leave the search row unchanged if asked to do so.
**************************************************************************/
void incrementAttFileSearchRow(ATTFILE* file);

/* Print the contents of an ATTFILE structure to a stream. */
void printAttFile(ATTFILE* attfile, FILE* stream);

#define ATTFILE_INCLUDED
#endif /* ATTFILE_INCLUDED */

