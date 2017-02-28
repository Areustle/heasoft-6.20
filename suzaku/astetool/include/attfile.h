/* $Id: attfile.h,v 1.12 2007/05/07 16:45:31 ishisaki Exp $ */
/************************************************************************

************************************************************************/
#ifndef _ATTFILE_H_
#define _ATTFILE_H_

/********************************************************************
  maximum time in seconds beyond the end of the attfile which can
  be safely extrapolated.
********************************************************************/
#define DEFAULT_ATTFILE_EXTRAPOLATION	32.0

#define UNINITIALIZED_ATTFILE_ROW	-1
#define ATTFILE_LOCAL_SEARCH_LIMIT	100
#define DIST_FOR_ATTFILE_RESET		1000

typedef enum attfile_type {
	ATTFILE_NORMAL_FITS,
	ATTFILE_PACKED_FITS,
	ATTFILE_ASCII
} ATTFILE_TYPE;

/*************************************************************************
  attitude file structure
*************************************************************************/
typedef struct attfile {
	char *telescop;			/* pointer to mission[] */
	char mission[64];		/* value of the TELESCOP keyword */
	char *filename;			/* attitude file name */
	long nrows;				/* NAXIS2 keyword value */
	ATTFILE_TYPE type;		/* kind of file type */
	fitsfile *fp;			/* CFITSIO file structure, or NULL if type=ASCII */

	double tstart;			/* time of first attitude record */
	double tstop;			/* time of last  attitude record */
	double duration;		/* = (tstop - tstart) */

	double min_extrapolated;	/* earliest valid extrapolated time */
	double max_extrapolated;	/*   latest valid extrapolated time */

	long search_row;		/* the most recent search result row */
	double search_t0;		/* the value of TIME at search_row   */
	double search_t1;		/* the value of TIME at search_row+1 */
	double search_q0[4];	/* the value of qparam at search_row   */
	double search_q1[4];	/* the value of qparam at search_row+1 */
	double cache_t;			/* last calculated time */
	double cache_q[4];		/* last calculated q-parameters */
	AtEulerAng cache_ea;	/* cache buffer used in aste_att_ea() */

	union attfile_info {
		struct attfile_normal_fits {
			int time_col;		/* column number of TIME  */
			int qparam_col;		/* column number of QPARAM */
			int sigma_col;		/* column number of SIGMA  */
			int sensor_col;		/* column number of SENSOR */
		} normal_fits;
		struct attfile_packed_fits {
			int time_col;		/* column number of TIME  */
			int euler_col;		/* column number of EULER */
		} packed_fits;
		struct attfile_ascii {
			struct attfile_ascii_data {	/* read all data in previous */
				double t;
				double q[4];
			} *p;
		} ascii_qparam;
	} fileinfo;

} ATTFILE;

#ifdef __cplusplus
extern "C"
{
#endif

/**************************************************************************
***************************************************************************
* create an attfile structure and open an actual attitude file
**************************************************************************/
ATTFILE *openAttFile(char *filename);

/**************************************************************************
***************************************************************************
* close an attitude file and destroy the ATTFILE structure
**************************************************************************/
void closeAttFile(ATTFILE *fp);

/**************************************************************************
***************************************************************************
* Read the Q parameter from a given row of the table
**************************************************************************/
int readQuatFromAttFile(ATTFILE *fp, long irow, double q[4], int *istat);

/**************************************************************************
***************************************************************************
* Read the TIME from a given row of the table
**************************************************************************/
double readTimeFromAttFile(ATTFILE *fp, long irow, int *istat);

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file
* returns 1 if time is withing the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInAttFile(ATTFILE *fp, double given_time);

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file, or its
* reasonable extrapolation
* returns 1 if time is withing the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInExtrapolatedAttFile(ATTFILE *fp, double given_time);

/**************************************************************************
***************************************************************************
* Changes the valid extrapolation time limits for the attitude file.
* The default values when the file is opened is
* DEFAULT_ATTFILE_EXTRAPOLATION.
* See also sInExtrapolatedAttFile(ATTFILE* file, double time).
**************************************************************************/
void resetAttFileExtrapolationLimits(ATTFILE *fp, double margin);

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
long findTimeInAttFile(ATTFILE *fp, double given_time);

/**************************************************************************
***************************************************************************
* determines the quaternion at an arbitrary time
* values are interpolated if necessary and things are optimized
* for the case of repeated calls to this routine with similar time values
**************************************************************************/
int findQuatInAttFile(ATTFILE *fp, double q[4], double given_time);

#ifdef __cplusplus
}
#endif

#endif /* _ATTFILE_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
