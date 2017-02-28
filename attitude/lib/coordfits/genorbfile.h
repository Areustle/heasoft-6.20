#ifdef __cplusplus
extern "C" {
#endif

#ifndef GENORBFILE_INCLUDED
#define GENORBFILE_INCLUDED

#include "fitsio.h"

/* Define these constants for searching for orbit times and setting the velocity units to the speed of light. */
#define UNINITIALIZED_ORBFILE_ROW -1
#define ORBFILE_LOCAL_SEARCH_LIMIT 100
#define ORBFILE_TIME_EPSILON 0.001
/* #define LIGHT_SPEED_KM_PER_S 299792.458 */

/* List the valid orbit formats. */
#define NUM_ORB_FORMATS 9
#define MAX_NUM_POS_COLS 3
#define MAX_NUM_VEL_COLS 3
#define MAX_NUM_KEP_COLS 6

typedef enum {
  ORBU_LEN_UNKNOWN,     /* Units are unknown */
  ORBU_LEN_M,           /* Units are m and m/s */
  ORBU_LEN_KM           /* Units are km and km/s */
} ORBLENUNIT;

typedef enum {
  ORBU_ANG_UNKNOWN,     /* Units are unknown */
  ORBU_ANG_DEG,         /* Units are degrees */
  ORBU_ANG_RAD          /* Units are radians */
} ORBANGUNIT;

typedef enum {
  ORBF_UNKNOWN,     /* Unknown orbit format */
  ORBF_COMPONENTS,  /* Cartesian components, one in each of 6 columns, position in km, velocity in km/s */
  ORBF_VECTOR,      /* Cartesian components, in  2 vector columns, position in km, velocity in km/s */
  ORBF_COMPONENTS_POS,  /* Cartesian components, one in each of 3 columns, position in km */
  ORBF_VECTOR_POS,      /* Cartesian components, all in 1 vector column, position in km */
  ORBF_COMPONENTS_VEL,  /* Cartesian components, one in each of 3 columns, velocity in km/s */
  ORBF_VECTOR_VEL,      /* Cartesian components, all in 1 vector columns, velocity in km/s */
  ORBF_KEPLERIAN,    /* Keplerian elements */
  ORBF_KEPLERIAN_NODUP /* Keplerian, duplicate time not permitted */
} ORBFORMAT;

typedef enum {
  ORBI_UNKNOWN,   /* Unknown interpolation method */
  ORBI_WEIGHTED,  /* Linear interpolation with respect to time */
  ORBI_TAYLOR,    /* Taylor expansion from acceleration and velocity */
  ORBI_NEAREST    /* Use previous time point - no interpolation */
} ORBINTERP;

/* Define a struct to store their corresponding numbers of elements
   and names. Used by getOrbFormatInfo(). */
typedef struct {
  int num_pos_cols;
  int num_pos_elements;
  int num_vel_cols;
  int num_vel_elements;
  int num_kep_cols;
  int num_kep_elements;
} ORB_FORMAT_INFO;
  
/* Define an orbital velocity structure. */
typedef struct {
  double v[3]; /* 3-vector of x, y, z velocity components */
} ORBVELOCITY;
  
/* Define an orbital position structure. */
typedef struct {
  double p[3]; /* 3-vector of x, y, z position components */
} ORBPOSITION;
  
/* Define an orbital elements structure. */
typedef struct {
  double a, e, i, an, ap, ma;             /* Keplerian orbital elements */
} ORBKEPLERIAN;

/* Define a struct to store orbit file information and results of
 * latest time/velocity look-up. */
typedef struct {
  fitsfile* fp;           /* FITS file pointer */
  char* filename;     /* Orbit file name */
  char* ext_name;     /* Orbit extension name */
  char* telescope;    /* Value of TELESCOP keyword */
    
  char* time_col_name; /* Time column name */
  int time_col_num;  /* Time column number */

  ORBFORMAT orb_format_num;  /* Orbit format number */
  ORBLENUNIT orb_len_unit_num;      /* Enum for km or m */
  ORBANGUNIT orb_ang_unit_num;      /* Enum for km or m */
  ORBINTERP interp_method_num;   /* Interpolation method to be used */

  char** vel_col_names; /* Array of velocity column names */
  int* vel_col_nums;   /* Array of velocity column numbers */
  int* num_vel_col_elements; /* Array of numbers of velocity column elements */
  int num_vel_cols;     /* Number of velocity columns */

  char** pos_col_names; /* Array of position column names */
  int* pos_col_nums;   /* Array of position column numbers */
  int* num_pos_col_elements; /* Array of numbers of position column elements */
  int num_pos_cols;     /* Number of position columns */

  char** kep_col_names; /* Array of Keplerian element column names */
  int* kep_col_nums;   /* Array of position column numbers */
  int* num_kep_col_elements; /* Array of numbers of position column elements */
  int num_kep_cols;     /* Number of position columns */

  double tstart; /* Time of first row */
  double tstop; /* Time of last row */
  double duration; /* tstop - tstart */

  double min_extrapolated_time; /* Earliest valid extrapolated time */
  double max_extrapolated_time; /* Latest valid extrapolated time */

  long num_rows; /* Number of rows in orbit extension. */
  long search_row; /* Most recent search result row */
  double search_timem1;  /* Time value at row (search_row - 1) */
  double search_time0; /* Time value at row search_row */
  double search_time1; /* Time value at row (search_row + 1) */
  double search_timep1;  
  ORBVELOCITY* search_velm1; /* Orbital velocity at row (search_row - 1) */
  ORBVELOCITY* search_velm2;
  ORBVELOCITY* search_vel0; /* Orbital velocity at row search_row */
  ORBVELOCITY* search_vel1; /* Orbital velocity at row (search_row + 1) */
  ORBPOSITION* search_pos0; /* Orbital position at row search_row */
  ORBPOSITION* search_pos1; /* Orbital position at row (search_row + 1) */
  ORBPOSITION* search_posm1; 
  ORBKEPLERIAN* search_kep0; /* Keplerian elements at row search_row */
  ORBKEPLERIAN* search_kep1; /* Keplerian elements at row (search_row + 1) */
  int last_search_is_reliable; /* Track if last search_* variables are still reliable */
  double mjdref; /* MJDREF from orbit file header */
} GENORBFILE;

/* ================================================================== */

/* Create a GENORBFILE structure and open the orbit file. */
GENORBFILE* openGenOrbFile( /* Returns a pointer to a new GENORBFILE
			     * struct. The pointer is NULL if the file
			     * cannot be opened. */
  char* filename, /* Orbit filename */
  char* ext_name, /* Extension containing orbital velocity */
  char** orb_col_names, /* Array of velocity column names. */
  ORBFORMAT orb_format, /* Enumerated velocity format. */
  ORBINTERP interp_method /* Enumerated interpolation method. */
  );

/* Close an orbit file and free a GENORBFILE structure. */
void closeGenOrbFile(GENORBFILE* file);

/* Print the contents of a GENORBFILE structure to a stream. */
void printGenOrbFile(GENORBFILE* file, FILE* stream);

/* Handle FITSIO errors while reading the orbit file.  Display error
   messages but do not terminate execution. */
void checkGenOrbFileFITSErrors
(
 int status, /* CFITSIO status */
 char* doing, /* present participle string of last action */
 GENORBFILE* file /* Orbit file structure */
);

/* Set flag in the file structure for the distance unit, m or km */
void setGenOrbFileUnits
(
  GENORBFILE* file,     /* Pointer to file structure */
  char* tunit_pos[3],   /* TUNIT strings for position columns */
  char* tunit_vel[3],   /* TUNIT strings for velocity columns */
  char* tunit_kep[6]    /* TUNIT strings for Keplerian  columns */
);

/* Find out whether the orbit file contains position */
int checkGenOrbFileHasPosition(GENORBFILE* file);

/* Find out whether the orbit file contains velocity */
int checkGenOrbFileHasVelocity(GENORBFILE* file);

/* Read the time from a certain row of the orbit extension. */
double readTimeFromGenOrbFile(GENORBFILE* file, long row);

/* Locate the consecutive rows of the orbit table that bracket the input time. */
long findTimeInGenOrbFile(GENORBFILE* file, double time);

/* Determine if the time is within the orbit file time range (not
 * extended by the time margin). Returns 1 (yes) or 0 (no). */
int isTimeWithinGenOrbFile(GENORBFILE* file, double time);

/* Set the extrapolated time limits of the orbit table. */
void setGenOrbFileExtrapolationLimits(GENORBFILE* file, double margin);

/* Return 1 if time is within the extrapolated time range of the orbit file. Return 0 if not. */
int isInExtrapolatedGenOrbFile(GENORBFILE* file, double time);

/* Read the orbital position in units of km from the
 * orbit table row. Returns the CFITSIO status. */
int readOrbPositionFromGenOrbFile(GENORBFILE* file, ORBPOSITION* orbpos, long row);

/* Find the interpolated orbital position at a certain time from an
 * orbit file. The position is in units of km. */
int findOrbPositionInGenOrbFile(GENORBFILE* file, ORBPOSITION* orbpos, double time);

/* Find the interpolated orbital position at a certain time from an
 * orbit file, with the magnitude and unit vector returned separately.
 * The position is in units of km. */
int findSplitOrbPositionInGenOrbFile(GENORBFILE* file, double* p_mag, double* p_unit, double time);

/* Read the orbital velocity in units of km/s from the
 * orbit table row. Returns the CFITSIO status. */
int readOrbVelocityFromGenOrbFile(GENORBFILE* file, ORBVELOCITY* orbvel, long row);

/* Find the interpolated orbital velocity at a certain time from an
 * orbit file. The velocity is in units of km/s. */
int findOrbVelocityInGenOrbFile(GENORBFILE* file, ORBVELOCITY* orbvel, double time);

/* Find the interpolated orbital velocity at a certain time from an
 * orbit file, with the magnitude and unit vector returned separately.
 * The velocity is in units of km/s. */
int findSplitOrbVelocityInGenOrbFile(GENORBFILE* file, double* v_mag, double* v_unit, double time);

/* Read the Keplerian orbital elements from the
 * orbit table row. Returns the CFITSIO status. */
int readOrbKeplerianFromGenOrbFile(GENORBFILE* file, ORBKEPLERIAN* orbkep, long row);

/* Compute position and velocity from the Keplerian elements. */
void computePosVelFromKeplerian(ORBPOSITION* pos, ORBVELOCITY* vel, ORBKEPLERIAN* kep);

/* Find the interpolated Keplerian orbital elements at a certain time from an
 * orbit file. */
int findOrbKeplerianInGenOrbFile(GENORBFILE* file, ORBKEPLERIAN* orbkep, double time);

/* Linearly interpolate (component-wise) between the start and stop
 * positions at a time fraction tfrac of the time between the start
 * and stop positions. */
void interpolateOrbPosition(ORBPOSITION* pos_out, ORBPOSITION* pos_start, 
			    ORBPOSITION* pos_stop, double tfrac);

/* Use the Taylor series expansion to interpolate the orbital position
 * (component-wise) about the middle point of three points in time.  The output
 * point is at time dt from the start time, and dtacc is the difference between
 * start and stop times.  The position at the middle time is used, together
 * with the velocities at all three times. */
void interpolateOrbPositionTaylor(ORBPOSITION* pos_out,
  ORBPOSITION* pos_middle, 
  ORBVELOCITY* vel_start, ORBVELOCITY* vel_middle, ORBVELOCITY* vel_stop,
  double dt, double dtacc);

/* Linearly interpolate (component-wise) between the start and stop
 * velocities at a time fraction tfrac of the time between the start
 * and stop velocities. */
void interpolateOrbVelocity(ORBVELOCITY* vel_out, ORBVELOCITY* vel_start, 
			    ORBVELOCITY* vel_stop, double tfrac);

/* Linearly interpolate between the start and stop
 * Keplerian elements  at a time fraction tfrac of the time between the start
 * and stop times. */
void interpolateOrbKeplerian(ORBKEPLERIAN* kep_out, ORBKEPLERIAN* kep_start, 
			    ORBKEPLERIAN* kep_stop, double tfrac);

/* Linearly interpolate between two angles in degrees. */
double interpolateAngle(double tfrac, double ang_start, double ang_stop);

/* Allocate an orbital position vector. */
ORBPOSITION* allocateOrbPosition();

/* Allocate an orbital velocity vector. */
ORBVELOCITY* allocateOrbVelocity();

/* Allocate an orbital elements vector. */
ORBKEPLERIAN* allocateOrbKeplerian();

/* Free an orbital position vector. */
void destroyOrbPosition(ORBPOSITION* orbpos);

/* Free an orbital velocity vector. */
void destroyOrbVelocity(ORBVELOCITY* orbvel);

/* Free an orbital elements vector. */
void destroyOrbKeplerian(ORBKEPLERIAN* orbkep);

/* Set the orbital position from its components. */
void setOrbPosition(ORBPOSITION* orbpos, double px, double py, double pz);

/* Set the orbital velocity from its components. */
void setOrbVelocity(ORBVELOCITY* orbvel, double vx, double vy, double vz);

/* Set the Keplerian orbital elements structure from its components. */
void setOrbKeplerian(ORBKEPLERIAN* orbkep, double kep_a, double kep_e, double kep_i,
  double kep_an, double kep_ap, double kep_ma);

/* Set the orbital position to zero. */
void setOrbPositionToZero(ORBPOSITION* orbpos);

/* Set the orbital velocity to zero. */
void setOrbVelocityToZero(ORBVELOCITY* orbvel);

/* Set the Keplerian orbital elements to zero. */
void setOrbKeplerianToZero(ORBKEPLERIAN* orbkep);

/* Get the orbital velocity magnitude. */
double getOrbVelocityMagnitude(ORBVELOCITY* orbvel);

/* Get the orbital velocity unit vector as a 3-element array. Returns
 * 1 if magnitude of orbvel is 0, otherwise returns 1. */
int getOrbVelocityUnitVector(ORBVELOCITY* orbvel, double* unit_vector);

/* Return 1 if all components of orbvel are 0, otherwise return 0. */
/* int isOrbVelocityNull(ORBVELOCITY* orbvel);  */

/* Print the orbital position to a stream. */
void printOrbPosition(ORBPOSITION* orbpos, FILE* stream);

/* Print the orbital velocity to a stream. */
void printOrbVelocity(ORBVELOCITY* orbvel, FILE* stream);

/* Print the Keplerian orbital elements to a stream. */
void printOrbKeplerian(ORBKEPLERIAN* orbkep, FILE* stream);

/* Return an array of pointers to structures containing the numbers of
 * columns and elements within a column for each orbit format. Use
 * the ORBFORMAT values as array indices, for example,
 * orb_format_info[ORB_VECTOR]->num_elements. */
ORB_FORMAT_INFO** getOrbFormatInfo();


#endif /* GENORBFILE_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif
 
/* ====================================================================== */

/* Revision log
   $Log: genorbfile.h,v $
   Revision 1.7  2015/11/17 20:15:47  driethmi
   Number of orbit formats is now 9, instead of 8, since we've added the
   ORBF_KEPLERIAN_NODUP type.

   Revision 1.6  2015/11/13 18:59:29  driethmi
   If the orbit format is ORBF_KEPLERIAN_NODUP, then we're in a mode that forbids
   duplicate rows in the orbit file.  In this case, automatically set all weights
   to 1.0, and don't bother looking for the weights column.

   Revision 1.5  2015/10/01 19:11:00  driethmi
   Implemented genorbfile changes as prescribed by HAK in the coordevt TRF,
   15-09-23.

   Revision 1.4  2015/06/19 23:38:26  rshill
   Added Taylor interpolation method; fixed some inconsistencies in
   variable names and also between findOrbPositionInGenOrbFile and findOrbVelocityInGenOrbFile.

   Revision 1.3  2015/04/03 22:55:36  rshill
   Automatic processing of orbital units: m vs. km, rad vs. deg.

   Revision 1.2  2015/03/18 23:04:42  rshill
   Support orbital position and Keplerian elements.

   Revision 1.1  2014/07/14 19:56:05  rshill
   General orbit file handling

*/
