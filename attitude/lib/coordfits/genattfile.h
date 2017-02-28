#ifdef __cplusplus
extern "C" {
#endif

#ifndef GENATTFILE_INCLUDED
#define GENATTFILE_INCLUDED

/* This General Attitude File library expands the existing
 * quaternion-based Attitude File library that is part of the
 * attitude-coordfits library.

 * The quaternion-based library assumes that quaternions will be read
 * from FITS attitude files from a specific extension and column and
 * in a quaternion format.  The software using the library can then
 * acquire attitude quaternions for its use.

 * This General Attitude File library can also read attitude in ZYZ Euler
 * angle trio form and convert the trio to quaternion form to give to the
 * calling software.  Additionally, the attitude extension and column
 * names can be fed to the library routines by the calling software
 * for more flexibility.  Read formats besides quaternion and ZYZ
 * Euler angle trio can be added fairly easily if a future need should
 * arise.
 */

#include "attfile.h"
#include "quat.h"
#include "euler.h"

#include "longnam.h"
#include "fitsio.h"

/* List the valid attitude formats and their corresponding number of
   elements and names. */

#define N_ATTITUDE_FORMATS 2
typedef enum {AF_QUAT, AF_EULER} ATTFORMAT;

/* Define a general attitude file structure that adds to the 
 * ATTFILE structure by including storage for Euler angles, 
 * attitude format, attitude extension name, and attitude column
 * name and number */

typedef struct 
{
  ATTFILE* af; /* Structure compatible with attfile functions */

  int att_format; /* Format AF_QUAT or AF_EULER */
  char* att_format_name; /* Attitude format name */
  int att_n_elements; /* Number of elements for attitude format. */
  char* att_ext_name; /* Attitude extension name. */
  char* att_col_name; /* Attitude column name. */
  int att_col_number; /* Column number for Euler angles in attitude file */
  double interp_margin; /* Difference in allowed attitude and event time within attitude file time range. */
  int use_interp_margin; /* 1 = Use the time margin for interpolation.
			  * 0 = Don't.  Either way, use the time margin for extrapolation. */
  
  double v3[3];  /* Array for reading attitude in 3-element format 
		    (e.g., Euler angles) */
  EULER* e; /* Euler angle trio for reading attitude. */

} GENATTFILE;


/* Create a GENATTFILE structure and open the attitude file. */
GENATTFILE* openGenAttFile /* Returns a new GENATTFILE struct */
(
 char* filename, /* Attitude filename */
 char* att_ext_name, /* Attitude extension name */
 char* att_col_name,  /* Attitude column name */
 int att_format, /* Attitude format */
 int interpolation_method, /* Interpolation method */
 double extrap_margin, /* Difference in allowed attitude and event time outside the attitude file time range. */
 double interp_margin, /* Difference in allowed attitude and event time within attitude file time range. */
 int use_interp_margin /* Use time margin for interpolation? */
);

/* Determine the attitude quaternion at a given time.  If the attitude
   file gives the attitude in a different format, the attitude is
   converted to a quaternion.  Values are interpolated if necessary,
   and the process is optimized for the case of repeated calls to this
   routine with similar time values. */
int findQuatInGenAttFile /* Returns 1 if attitude is found at the time,
			  * 0 if not. */
(
 GENATTFILE* file, /* Attitude file structure */
 QUAT* q, /* Returns the attitude in quaternion form at the time */
 double time /* Time at which attitude is desired */
 );

/* Read the attitude from the attitude file and return it in quaternion form. */
void readQuatFromGenAttFile
(
 GENATTFILE* file, /* Attitude file structure */
 QUAT* q, /* Returns quaternion in the row */
 long row /* Row at which attitude is desired */
 );

/* Close the attitude file and free memory. */
void closeGenAttFile
(
 GENATTFILE* file /* Attitude file structure to close/destroy */
 );

/* Retrieve the number of elements for an attitude format. */
int getAttFormatNElements /* Returns the number of elements */
(
 int att_format /* Attitude format */
 );

/* Retrieve the name of an attitude format. */
void getAttFormatName
(
 int att_format, /* Attitude format number */
 char* att_format_name /* Returns attitude format name */
 );

/* Handle FITSIO errors while reading attitude file.  Display error
   messages but do not terminate execution. */
void checkGenAttFileFITSErrors
(
 int status, /* CFITSIO status */
 char* doing, /* present participle string of last action */
 GENATTFILE* file /* Attitude file structure */
 );

/* Print a GENATTFILE structure to a stream. */
void printGenAttFile
(
 GENATTFILE* genattfile, /* Pointer to GENATTFILE structure */
 FILE* stream /* Output stream */
 );

#endif /* GENATTFILE_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif

/* ====================================================================== */

/* Revision log
   $Log: genattfile.h,v $
   Revision 1.8  2014/07/14 19:54:17  rshill
   Added cvs log

*/
