/** \file coordevtlib.h
    \brief Data structure and function declarations for coordevt
    \author Timothy Reichard
    \date $Date: 2016/01/13 18:23:34 $ 

    The following declarations and functions are used to create
    a structure to hold all the run-time task information needed by
    the CFITSIO iterator framework, which is used for processing the
    event file rows and calculating the coordinates. 
*/

#ifndef TOOL_COORDEVT_COORDEVTLIB_H
#define TOOL_COORDEVT_COORDEVTLIB_H

#include "coordfits2.h"

#define new variable_new
#include "headas_gti.h"
#undef new

#include "param_wrappers.h"
#include "caldbquery.h"

#include "ahlog/cahlog.h"
#include "headas_rand.h"

#include "headas.h"
#include "fitsio.h"
#include "longnam.h"

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>

/** \addtogroup tool_coordevt
    @{
*/

/* ====================================================================== */

/** Maximum length of a coordinate system name. */
#define COORDSYSNAME_LENGTH 11

/** The part of an event file where a MULTISEG property can be read from. */
typedef enum {
  e_PL_UNKNOWN,  /**< Unknown location */   
  e_PL_COLUMN,   /**< Event table column value */ 
  e_PL_KEYWORD   /**< Event table header keyword */ 
} PropertyLocationEnum;

#ifdef __cplusplus
extern "C" {
#endif

/* Command-line parameter structure. */
typedef struct {
  
  char* in_file; /**< Input filename */
  char temp_file[FLEN_FILENAME]; /**< Temporary output filename */
  char* out_file_as_input; /**< Output filename exactly as specified by user */
  char out_file[FLEN_FILENAME]; /**< Output filename */
  int out_file_bang; /**< out_file_as_input begins with '!' (1) or not (0) */
  int are_files_same; /**< infile and outfile are (1)/are not (0) the same file */
  char teldef_file[FLEN_FILENAME]; /**< TelDef filename */
  char* att_file; /**< Attitude filename */
  char* datt_files; /**< String list of delta attitude filenames */
  int identity_att; /**< Flag for attfile=IDENTITY */
  int identity_datt; /**< Flag for dattfile=IDENTITY */
  char* orb_file; /**< Orbit filename */
  char* startsysname; /**< Name of first originating coord. sys. */
  char* stopsysname; /**< Name of final destination coord. sys. */
  int startsys; /**< Number of first originating coord. sys. */
  int stopsys; /**< Number of final destination coord. sys. */
  int lowsys; /**< Lower of startsys and stopsys */
  int highsys; /**< Higher of startsys and stopsys */

  int do_annual_aberration; /**< 1 = Do annual aberration correction. 0 = Don't */
  int invert_annual_aberration; /**< 0 = Do annual aberration correction normally, 1 = Do inverse correction */
  int do_orbital_aberration; /**< 1 = Do orbital aberration correction. 0 = Don't */
  int invert_orbital_aberration; /**< 0 = Do orbial aberration correction normally, 1 = Do inverse correction */
  int follow_sun; /**< 1 = Recalculate Earth velocity for each event. 0 = Don't */
  int use_att; /**< Attitude files are (1)/are not (0) used. */
  int use_sky_att; /**< A sky attitude file is (1)/is not (0) used. */
  int use_orb; /**< Orbit files is (1)/is not (0) used. */
  int att_annaber; /**< ABERRAT keyword from attitude file. */
  int att_orbaber; /**< ORBIABER keyword from attitude file. */
  int att_followsun; /**< FOLLOWSUN keyword from attitude file.  Also, if ABERRAT=T, then this = 1. */
  int att_invaberr; /**< INVABERR keyword from attitude file. */
  int att_invoaber; /**< INVOABER keyword from attitude file. */
  int really_do_annaber; /**< Do aberration correction, based on do_annual_aberration and att_annaber */
  int really_inv_annaber; /**< Do inverse correction, based on invert_annual_aberration and att_invaberr */
  int really_do_orbaber; /**< Do orbital correction, based on do_orbital_aberration and att_orbaber */
  int really_inv_orbaber; /**< Do inverse correction, based on invert_orbital_aberration and att_invoaber */
  
  char* randomize; /**< Value of randomize command-line parameter */
  int dorandomization; /**< 1 = Do randomization. 0 = Don't */
  int seed; /**< Random number generator seed */
  char randsysname[COORDSYSNAME_LENGTH]; /**< Name of the coord. sys. to apply randomization to */
  int randsys; /**< Number of the coord. sys. to apply randomization to */
  char randscalesysname[COORDSYSNAME_LENGTH]; /**< Name of the coord. sys. that sets the scale of randomization */
  int randscalesys; /**< Number of the coord. sys. that sets the scale of randomization */

  int att_interpolation; /**< Number of sky attitude time interpolation method */
  int datt_interpolation; /**< Number of delta attitude time interpolation method */

  char* event_extension; /**< Name of events extension in event file */
  char* time_col_name; /**< Name of time column in event file */
  char* att_extension; /**< Name of attitude extension in attitude file */
  char* orb_extension; /**< Name of orbit extension in orbit file */
  char* att_col_name; /**< Name of attitude column in attitude file */
  char* orb_col_name; /**< String of orbital velocity column names in orbit file */
  char* orb_format_string; /**< Format of orbital velocity in orbit file: COMPONENTS or VECTOR. */
  char** vel_col_name; /**< Array of orbital velocity column names in orbit file */
  int num_vel_cols; /**< Number of orbital velocity column names in orbit file */
  char* att_format_string; /**< Name of attitude format (e.g,. QUAT) */
  ATTFORMAT att_format; /**< Number of attitude format */
  ORBFORMAT vel_format; /**< Number of orbit format */

  char** seg_col_name; /**< Names of segment columns: seg_col_name[sys] */
 
  int includeintcol; /**< 1 = Use rounded coordinates in event file. 0 = Don't */
  int includefloatcol; /**< 1 = Use unrounded coordinates in event file. 0 = Don't */
  int includefloatskycol; /**< 1 = Use unrounded SKY coordinates in event file. 0 = Don't */
  int startwithfloat; /**< 1 = Use unrounded coordinates in startsys system. 0 = Use rounded coordinates */
  char* floatcolsuffix; /**< Suffix of unrounded coordinate column names */

  int blank_col; /**< 1 = Fill in null values for coordinate columns not calculated. 0 = Don't change the values */
 
  LONGLONG b_null_value; /**< Null value for rounded coordinate values, TBYTE */
  LONGLONG i_null_value; /**< Null value for rounded coordinate values, TSHORT */
  LONGLONG j_null_value; /**< Null value for rounded coordinate values, TLONG */
  LONGLONG k_null_value; /**< Null value for rounded coordinate values, TLONGLONG */
  LONGLONG sb_null_value; /**< Null value for rounded coordinate values, TSBYTE */
  LONGLONG ui_null_value; /**< Null value for rounded coordinate values, TUSHORT */
  LONGLONG uj_null_value; /**< Null value for rounded coordinate values, TULONG */

  double ra; /**< Right ascension of nominal pointing */
  double dec; /**< Declination of nominal pointing */
  double roll; /**< Counterclockwise angle between celestial north axis and SKY +Y axis, nominally 0 */
  int ra_from_event_file; /**< T = Read RA from RA_NOM or RA_PNT event header keyword.
			    * F = Use command-line parameter value. */
  int dec_from_event_file; /**< T = Read dec from DEC_NOM or DEC_PNT event header keyword.
			     * F = Use command-line parameter value. */

  double att_time_margin; /**< Margin (seconds) for extrapolating time in sky attitude file */
  double datt_time_margin; /**< Margin (seconds) for extrapolating time in delta attitude file */
  int use_att_time_margin; /**< T = Use att_time_margin for restricting interpolation time range in sky attitude file.
			     * F = Always interpolate */
  int use_datt_time_margin; /**< T = Use datt_time_margin for restricting interpolation time range in delta attitude file.
			      * F = Always interpolate */
  int chatter; /**< Chatter level */
  int clobber; /**< Overwrite existing file? */
  int debug; /**< 1 = Show extra debugging output. 0 = Don't */

  int write_history; /**< 1 = Write command-line parameters in output event header. 0 = Don't */
  long buffer; /**< -1 = automatic. 0 = 1 line. >=1 = that many lines */

  int tempfile_copied; /**< Writing to output is (1)/is not (0) complete. */
  int tempfile_written; /**< Writing to output is (1)/is not (0) complete. */

} PARAM;


/** \brief Run-time information needed by the updateCoordinates function */
typedef struct {
  
  /* Param, TelDef, and Attitude structures and associated parameters */

  PARAM* param; /**< Parameters structure */
  TELDEF2* teldef; /**< TelDef structure */
  GENATTFILE** att; /**< Array of attitude file structures */
  GENORBFILE* orb; /**< Orbit file structure */
  char** datt_file_list; /**< List of delta attitude filenames */
  char** att_filenames; /**< List of all attitude filenames */
  int n_datt_files; /**< Number of delta attitude files */
        /* +++ RSH 2015-08-17 IDENTITY */
  int* att_identity_flags;  /**< Flag whether identity quat is to be used in a given transformation */
  QUAT* q; /**< Current pointing quaternion (used in iterator framework for SKYATT transformations) */
  int conv_to_higher; /**< Coordinate conversion direction */
  char conv_arrow[3]; /**< Arrow string showing coordinate conversion direction: "->" or "<-" */

  /* Event file properties. */

  char mission[FLEN_VALUE]; /**< Event file TELESCOP value */
  char instrument[FLEN_VALUE]; /**< Event file INSTRUME value */
  long buffer_rows; /**< Number of rows in read/write file buffer */
  long total_rows; /**< Number of rows in event table */
  
  /* Aberration information */

  double v_earth_orbit;       /**< Magnitude of Earth's velocity */
  double vhat_earth_orbit[3]; /**< Direction vector of Earth's velocity (normalized to unity)  */
  double v_sat_orbit;         /**< Magnitude of satellite's orbital velocity */
  double vhat_sat_orbit[3];   /**< Direction vector of satellite's orbital velocity (normalized to unity)  */
  double v_sat_total;         /**< Magnitude of satellite's total velocity */
  double vhat_sat_total[3];   /**< Direction vector of satellite's total velocity (normalized to unity)  */
  double mjdref;              /**< Value of MJDREF keyword in event file */
  int is_orbvel_valid;        /**< 1 if orbital velocity valid, 0 if not */
  
  /* CFITSIO iterator column info. */
  
  iteratorCol* time_col; /**< CFITSIO iterator column for time */
  iteratorCol** seg_cols;  /**< CFITSIO iterator column array for RAWTODET segment properties: seg_cols[sys] */
  iteratorCol*** prop_cols;  /**< CFITSIO iterator column array for MULTISEG properties: prop_cols[sys][prop] */
  iteratorCol** intx_cols;  /**< CFITSIO iterator column array for rounded x coordinates: intx_cols[sys] */
  iteratorCol** inty_cols;  /**< CFITSIO iterator column array for rounded y coordinates: inty_cols[sys] */
  iteratorCol** floatx_cols;  /**< CFITSIO iterator column array for unrounded x coordinates: floatx_cols[sys] */
  iteratorCol** floaty_cols;  /**< CFITSIO iterator column array for unrounded y coordinates: floaty_cols[sys] */
  
  /* Time, coordinate, and segment property value arrays used in the CFITSIO iterator */
  
  double* time; /**< CFITSIO iterator data array for time: time[row] */

  /* LONGLONG is a macro defined in fitsio.h */
 
  LONGLONG** intx;  /**< CFITSIO iterator data array for rounded x coordinates: intx[sys][row] */
  LONGLONG** inty;  /**< CFITSIO iterator data array for rounded y coordinates: inty[sys][row] */
  double** floatx; /**< CFITSIO iterator data array for unrounded x coordinates: floatx[sys][row] */
  double** floaty; /**< CFITSIO iterator data array for unrounded y coordinates: floaty[sys][row] */
  int** segs; /**< CFITSIO iterator data array for RAWTODET segment numbers: segs[sys][row] */
  int*** props; /**< CFITSIO iterator data array for MULTISEG properties: props[sys][prop][row] */
  int** rowprops;  /**< MULTISEG properties for one event row: rowprops[sys][prop] */

  LONGLONG* nullx; /**< List of integer null values for each x coordinate: nullx[sys] */
  LONGLONG* nully; /**< List of integer null values for each y coordinate: nully[sys] */
  LONGLONG** nullcol; /**< Array of pointers to stored null values: int* nullcol[col] */
  
  /* MULTISEG property locations and values if read from keywords. */
  
  PropertyLocationEnum** prop_locations; /**< Array of MULTISEG property locations: prop_locations[sys][prop] */
  long** prop_key_values; /**< Array of MULTISEG keyword property values: prop_key_values[sys][prop] */
  
  long** window_offsets_x;   /**< Array of MULTISEG windowing x-offsets: window_offset_x[sys][propvalue] */
  long** window_offsets_y;   /**< Array of MULTISEG windowing y-offsets: window_offset_y[sys][propvalue] */

  double random_px_scale; /**< Max amount by which to randomize the intrapixel location*/
  double prev_percent_done; /**< Progress made processing event file lines */

  /* Event counters */

  long n_events_processed; /**< Number of processed events */
  long n_events_succeeded; /**< Number of events processed successfully */
  long n_events_failed; /**< Number of events processed with problems */
  long n_events_null_time; /**< Number of events with NULL time */
  long* n_events_no_attitude; /**< Number of events without attitude: n_events_no_attitude[sys] */
  long n_events_no_orbit;    /**< Number of events without orbit: n_events_no_orbit */
  long* n_events_no_multiseg; /**< Number of events without successful MULTISEG property table look-ups: n_events_no_multiseg[sys] */
  long* n_events_no_rawtodet; /**< Number of events with problems in each RAWTODET transformations: n_events_no_rawtodet[sys] */
  long n_events_out_of_range; /**< Number of events with out-of-range coordinates */
  long* first_event_with_attitude; /**< Number of first event with valid attitude for each SKYATT transformation: first_event_with_attitude[sys] */
  long* last_event_with_attitude; /**< Number of last event with valid attitude for each SKYATT transformation: last_event_with_attitude[sys] */
  long first_event_with_orbit; /**< Number of first event with valid orbit for each SKYATT transformation: first_event_with_orbit[sys] */
  long last_event_with_orbit; /**< Number of last event with valid orbit for each SKYATT transformation: last_event_with_orbit[sys] */
  double* event_time_at_last_quat; /**< Last time a quat was read from an attitude file for each SKYATT system. */
  double event_time_at_last_orbit; /**< Last time a velocity was read from an orbit file for each SKYATT system. */

  /* Event counters related to aberration calculation */

  long n_events_no_att_or_orb_update; /**< Numbers of events not needing an attitude or orbit update. */
  long n_events_aber_ann_and_orb_success; /**< Number of events needing both annual and orbital aberration where the calculation succeeded */
  long n_events_aber_ann_and_orb_failure; /**< Number of events needing both annual and orbital aberration where the calculation failed */
  long n_events_aber_ann_only_success; /**< Number of events needing annual aberration only where the calculation succeeded */
  long n_events_aber_orb_only_success; /**< Number of events needing orbital aberration only where the calculation succeeded */
  long n_events_aber_orb_only_failure; /**< Number of events needing orbital aberration only where the calculation failed */
  
} INFO;

#ifdef __cplusplus
}  /* end extern "C" */
#endif

/* ====================================================================== */

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Initialize the random number generator with an input seed;
 *    if the seed is zero, a send is derived from the date and time.
 *  \param[in] seedIn Input seed or zero
 */
void initializeRandom(const unsigned long int seedIn);

/** \brief Free the storage associated with a random number generator.
 */
void destroyRandom(void);

/** \brief Draw a random number from a uniform distribution on the 
 *   interval [0,1).
 */
double getRandomDouble(void);

/** \brief Draw a random number from a uniform distribution on the
 *    interval [-scale,+scale).
 */
double getRandomShift(const double scale);

/** \brief allocate and initialize a param structure.
 *  \return pointer to new param structure.
 */
PARAM* createParam(void);

/** \brief Free the memory of a PARAM structure.
    \param[in] param Pointer to PARAM structure.
*/
void destroyParam(PARAM* param);

/** \brief Set param->teldef_file to the actual filename.
 *  \info[in] info INFO structure.
 *  \return non-zero for failure.
 */
int resolveTeldefFilename(INFO* info);

/** \brief Set up the info structure to hand to the iterator.
 *  \return Pointer to new INFO structure
 *  \param[in] param Parameter structure
 *  \param[out] fp Event file pointer
 */
INFO* createInfo(PARAM* param, fitsfile* fp);

/** \brief Set up CFITSIO iterator column structures, one for each
 *    column to be read or written
 *  \return Pointer to array of iterator columns
 *  \param[in] info INFO structure
 *  \param[in,out] fp Event file pointer
 *  \param[out] n_cols Number of Iterator column structures
 */
iteratorCol* setIteratorColumns(INFO* info, fitsfile* fp, int* n_cols);

/** \brief Free all the memory associated with an INFO structure.
 *    Note this even destroys the associated PARAM structure even
 *    though that structure was created separately.
 *  \param[in] info Pointer to INFO structure to destroy
 */
void destroyInfo(INFO* info);

#ifdef __cplusplus
}  /* end extern "C" */
#endif

/** \brief Initialize attitude files for reading.
 *  \param[in,out] info Pointer to the INFO structure
 */
int initAttFiles(INFO* info);

/** \brief Initialize orbit file for reading.
 *  \param[in,out] info Pointer to the INFO structure
 */
int initOrbFile(INFO* info);

/** \brief Decide what needs to be updated in the event header and update it. 
 *  \param[in,out] fp Event file pointer
 *  \param[in] info Pointer to INFO structure
 *  \param[in] fits_col Array of iterator column structures
 *  \param[in] taskname Name and version of this task
 */
int updateKeywords(fitsfile* fp, INFO* info, iteratorCol* fits_col, const char * taskname);

/** \brief Check that the needed coordinate columns exist in the event
    extension of the event file.  If any output columns are missing, add
    them.
 *  \param[in,out] fp Event file pointer 
 *  \param[in] fits_col Array of CFITSIO Iterator column structures 
 *  \param[in] n_cols Number of CFITSIO Iterator column structures 
 *  \param[in] nullcol Array of pointers nullcol[col] pointing to stored null values 
 *  \param[in] param Pointer to parameter structure 
 */
int checkCoordinateColumns(fitsfile* fp, iteratorCol* fits_col, 
			    const int n_cols, LONGLONG** nullcol, PARAM* param);

/** \brief convertToHigherCoordinates is the CFITSIO Iterator work function.  The
    iterator framework reads a block of rows from the event table of
    the event file.  updateCoordinates performs the coordinate
    conversions and fills the data arrays for the iterator to return to
    the event table.  
 *  \param[in] total_rows Number of rows in event file 
 *  \param[in] offset Number of initial rows to skip 
 *  \param[in] first_row Number of first row to work on for this function call. 
 *  \param[in] n_rows Number of rows to work on for this function call. 
 *  \param[in] ncols Number of iterator columns 
 *  \param[in] fits_col CFITSIO Iterator column structure array 
 *  \param[in,out] void_info Structure of runtime info 
 */
int convertToHigherCoordinates(long total_rows, long offset, long first_row, long n_rows, 
                               int ncols, iteratorCol* fits_col, void* void_info);

int convertToLowerCoordinates
(long total_rows, /**< [in] Number of rows in event file */
 long offset,     /**< [in] Number of initial rows to skip */
 long first_row,  /**< [in] Number of first row to work on 
                     for this function call. */
 long n_rows,     /**< [in] Number of rows to work on for this 
                      function call. */
 int ncols,       /**< [in] Number of iterator columns */
 iteratorCol* fits_col, /**< [in] CFITSIO Iterator column structure array */
 void* void_info  /**< [in,out] Structure of additional runtime info */
  );

/* } */ /* end extern */

/** \brief Return a uniform random number between +-0.5*scale.
    \returns random number */
double getRandomShift
(const double scale /**< scale of random number range */
 );

/** \brief Look at an attitude file and determine the destination coordinate 
 *    system in the transformation the attitude should be used for. 
 *  \param[in] filename Attitude filename 
 *  \param[in] att_extension Attitude extension name 
 *  \param[out] orig_sys_name Originating coordinate system name 
 *  \param[in] teldef Pointer to TelDef structure 
 */
int getDestSysFromAttFile(char* filename, char* att_extension, char* orig_sys_name, TELDEF2* teldef);

/** \brief Negate the components of a 3-vector.
 *  \param[in,out] v Vector (3-element array) to negate 
 */
void negate3Vector(double v[3]);

/** \brief Display event counters.
 *  \param[in] info Pointer to INFO structure containing the event counters
 */
void displayEventCounters(INFO* info);

/** \brief Read windowing offset keywords. 
 *  \param[in,out] fp Event file pointer 
 *  \param[in,out] info INFO structure pointer 
 *  \param[in] sys Originating coordinate system number 
 *  \param[in] dim 'X' or 'Y' dimension 
 */
int readWindowingOffsetKeywords(fitsfile* fp, INFO* info, 
				 const int sys, char dim);

/** \brief Write the aspecting keywords if SKY coordinates were calculated. 
 *  \param[in,out] fp Event file pointer 
 *  \param[in] param Pointer to parameter structure 
 *  \param[in] decimals Number of decimals places for floating-point keyword values 
 */
int writeAspectingKeywords(fitsfile* fp, PARAM* param, const int decimals);

/** \brief Write WCS and other keywords for each coordinate column.
 *  \param[in,out] fp Event file pointer 
 *  \param[in] coord Pointer to coordinate system structure 
 *  \param[in] colx Iterator column pointer for x coordinate 
 *  \param[in] coly Iterator column pointer for y coordinate 
 *  \param[in] crvalx x coord. of reference pixel 
 *  \param[in] crvaly y coord. of reference pixel 
 *  \param[in] crroll Roll of coordinate system (sky) or 0 (other coord. sys.) 
 *  \param[in] deg_per_sky_unit Conversion factor from sky units to deg. 
 *  \param[in] is_sky The coordinates are (1)/are not (0) sky coordinates 
 *  \param[in] decimals Number of decimal places for floating-point keyword values 
 */
int updateCoordKeywords
(
 fitsfile* fp, COORDDEF* coord, iteratorCol* colx, iteratorCol* coly, 
 const double crvalx, const double crvaly, const double crroll,  
 const double deg_per_sky_unit, const int is_sky, const int decimals  
 );

/** \brief Update the event header history with what coordevt has completed. 
 *  \param[in,out] fp Event file pointer 
 *  \param[in] info INFO structure pointer 
 *  \param[in] taskname Name and version of task 
 */
int addHistoryComments(fitsfile* fp, INFO* info, const char * taskname);

/** @} */

#endif /* TOOL_COORDEVT_COORDEVTLIB_H */


/* ====================================================================== */

/* Revision Log
   $Log: coordevtlib.h,v $
   Revision 1.10  2016/01/13 18:23:34  klrutkow
   added variables to PARAM for attitude file keywords for aberration correction

   Revision 1.9  2015/11/04 15:44:09  driethmi
   Reverted to revision 1.7; tstart is not needed as a structure parameter.

   Revision 1.7  2015/08/19 01:11:55  rshill
   Added IDENTITY spec for attitude files.  Reduced precision of number in messages.

   Revision 1.6  2015/07/16 19:53:34  rshill
   Update event counters for aberration processing.

   Revision 1.5  2015/07/16 15:40:46  rshill
   Changed PARAM struct to support ! clobber; pass INFO to resolveTeldefFilename.

   Revision 1.4  2015/06/25 23:00:31  rshill
   Cosmetic change to resolveTeldefFilename.

   Revision 1.3  2015/06/25 22:34:19  rshill
   Corrected C string handling bugs.

   Revision 1.2  2015/06/25 00:24:50  rshill
   Resolve TelDef filename with explicit call to HDgtcalf.

   Revision 1.1  2015/05/14 22:25:48  rshill
   Converted language to plain C.

*/

/* Old revision Log from C++ version:

   Revision 1.11  2015/03/18 23:12:20  rshill
   Support Keplerian elements via upgraded genorbfile.

   Revision 1.10  2015/01/12 23:47:23  rshill
   Parameters standardized.  Most important changes are new values
   YES, NO, INVERT for orbaber and annaber.

   Revision 1.9  2014/10/02 21:43:22  rshill
   Enabled the following: (1) multiple default null values by specific integer type;
   (2) randomization in FOC->SKY.

   Revision 1.8  2014/08/05 00:13:25  rshill
   Aberration routines separated into library.

   Revision 1.7  2014/07/09 22:01:20  rshill
   Added orbital aberration per trf_coordevt_14-05-28.
   Still need to break out find_aberration_correction as a library routine.
   (Currently in coordevtlib.)

   Revision 1.6  2014/04/30 01:18:54  treichar
   Fixed transformation direction arrow in output messages to point in the correct direction when inverse coordinate transformations are performed, e.g., RAW<-ACT.  Simplified the % progress indicators.  Fixed the input coordinate columns and list of output coordinate columns written to the event file header as HISTORY lines.

   Revision 1.5  2014/03/06 20:02:38  treichar
   Fix bug where new attitude is not read after the event time changed since the last attitude was read due to intervening event with NULL coords.

   Revision 1.4  2014/02/25 19:06:39  treichar
   Added capability to convert higher-level coordinates to lower-level ones, though with several parameters functioning properly for only one of its settings for this
   direction of coordinate conversion. Advanced features like windowing modes and nonlinear distortion corrections do not function in this conversion direction.

   Revision 1.3  2014/01/24 20:58:55  treichar
   Changed random number generator functions from those in the attitude-random library to the ahgen library

   Revision 1.2  2014/01/23 21:25:23  treichar
   Finished code reorganization

   Revision 1.1  2014/01/17 19:14:58  treichar
   Reorganized code from earthvel.{h,cxx}, info.{h,cxx} keywords.{h,cxx} and param.{h,cxx} into coordevtlib.{h,cxx}.

*/
