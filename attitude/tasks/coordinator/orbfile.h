#ifndef ORBFILE_INCLUDED
#define ORBFILE_INCLUDED

#include "fitsio.h"


/********************************************************************
* maximum time in seconds beyond the end of the attfile which can
* be safely extrapolated.
********************************************************************/
#define DEFAULT_ORBFILE_EXTRAPOLATION 32.

#define UNINITIALIZED_ORBFILE_ROW -1
#define ORBFILE_LOCAL_SEARCH_LIMIT 100
#define DIST_FOR_ORBFILE_RESET 1000


typedef struct
{
	double pos[3];
	double vel[3];

} PVrecord;


/*************************************************************************
* orbit file structure
*************************************************************************/
typedef struct {

char *name;   /* file name */

fitsfile *fptr; /* FITSIO file structure */

long nrows; /* NAXIS2 keyword value */
char mission[FLEN_VALUE]; /* value of the TELESCOP keyword */

int time_col;    /* column numbers  */
int pos_col;  /* for the various */
int vel_col;   /* columns in the  */

double tstart; /* time of first orbit record */
double tstop;  /* time of last  orbit record */
double duration; /* = tstop-tstart */

double min_extrapolated; /* earliest valid extrapolated time */
double max_extrapolated; /*   latest valid extrapolated time */

long search_row; /* the most recent search result row */
double search_time0; /* the value of TIME at search_row   */
double search_time1; /* the value of TIME at search_row+1 */

PVrecord search_pv0;
PVrecord search_pv1;
int search_pvs_reliable;

int search_nearby;
double limit_nearby;

} ORBFILE;



/***********************************************************************
*************************************************************************
* handle FITSIO errors while reading orbit files
* prints error messages and exits if there is an error
************************************************************************/
void checkOrbitFileFITSerrors(int status, const char* doing, ORBFILE* file);


/**************************************************************************
***************************************************************************
* create an attfile structure and open an actual orbit file
**************************************************************************/
ORBFILE* openOrbitFile(const char* filename );

/**************************************************************************
***************************************************************************
* close an orbit file and destroy the ORBFILE structure
**************************************************************************/
void closeOrbitFile(ORBFILE* file);

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an orbit file
* returns 1 if time is withing the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInOrbitFile(ORBFILE* file, double time);

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an orbit file, or its 
* reasonable extrapolation
* returns 1 if time is within the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInExtrapolatedOrbitFile(ORBFILE* file, double time);

/**************************************************************************
***************************************************************************
* Changes the valid extrapolation time limits for the orbit file.
* The default values when the file is opened is 
* DEFAULT_ORBFILE_EXTRAPOLATION.
* See also isInExtrapolatedOrbitFile(ORBFILE* file, double time).
**************************************************************************/
void resetOrbitFileExtrapolationLimits(ORBFILE* file, double margin);

/**************************************************************************
***************************************************************************
* determines the orbital state at an arbitrary time
* values are interpolated if necessary and things are optimized
* for the case of repeated calls to this routine with similar time values
* Returns true if the request meets search constraints.
**************************************************************************/
int findRecordInOrbitFile(ORBFILE* file, PVrecord  * pv, double time);


#endif /* ORBFILE_INCLUDED */
