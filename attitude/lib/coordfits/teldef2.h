#ifdef __cplusplus
extern "C" {
#endif

#ifndef TELDEF2_INCLUDED

#include "fitsio.h"
#include "coord.h"
#include "align.h"
#include "tr_basic.h"
#include "tr_skyatt.h"
#include "tr_multiseg.h"
#include "tr_rawtodet.h"
#include "tr_telescope.h"
#include "teldef.h"

/* Define a negative value for the sky coordinate system index when
 * the TelDef file lacks a sky coordinate system. */

#define NO_SKY_SYS -9999

/* Enumerated coordinate transformation type */
typedef enum {
  e_TT_UNKNOWN,   /* Unknown type */
  e_TT_BASIC,     /* BASIC type */
  e_TT_RAWTODET,  /* RAWTODET type (Linear coefficients or pixel map) */
  e_TT_MULTISEG,  /* MULTISEG type */
  e_TT_SKYATT     /* SKYATT type */
} TransformationTypeEnum;


/*****************************************************************************
* The TELDEF2 (Telescope Definition 2) structure defines the coordinate
* systems used for a given instrument. 
*
* The coordinates are generalized instead of being fixed in position and
* raw/detector/sky classes as they were in the original TELDEF structure.
*
* This new teldef library supports v0.2 of the Teldef File Format
* Specification and is backwards-compatible with v0.1 TelDef files.
*
*****************************************************************************/
typedef struct {

  /* Input file, mission and instrument names. */

  char filename[FLEN_FILENAME];  /* TelDef file name */
  char mission[FLEN_VALUE];      /* Mission name */
  char instrument[FLEN_VALUE];   /* Instrument abbreviation */
  double td_version;             /* Version of Teldef file spec. */

  /* Coordinate systems */

  long n_coordsys;   /* Number of coordinate systems */

  char** coordsysnames; /* Array of 3-letter names of coordinate systems 
			 * coordsysnames[sys] */

  COORDDEF*** coordsys;  /* Array of coordinate system structures
			  * coordsys[sys][seg] */

  int* n_segments;   /* Number of segments in each coordinate system level
			n_segments[sys] */

  int* min_segment;  /* Minumum segment number in each coord. sys.
		      * min_segments[sys] */
  
  char** trtypenames;    /* Transformation type names, such as
			  * MULTISEG or RAWTODET for each system 
			  * trtypenames[sys] */

  TransformationTypeEnum* trtypes; /* Enumerated transformation type 
				   * trtypes[sys] */

  long sky_sys;  /* Coordinate system index for the sky coordinate system. */

  /* Optical axis coordinates */

  char* optcoord; 
  double optaxisx;
  double optaxisy;

  /* Parameters for randomization of intrapixel event locations. */

  int randsys;   /* System number of lower-level system where 
			       randomization is applied */
  char randsysname[FLEN_VALUE];   /* System name of lower-level system where
				    randomization is applied */
  int randscalesys;  /* Number of system whose pixel size is used
		     * to set the amount of randomization. */
  char randscalesysname[FLEN_VALUE];  /* Name of system whose pixel size is used 
					to set the amount of randomization */
  double* px_scale_factor; /* Ratio of the pixel scale in one coord. sys. to
			      the pixel scale of the bottom-level sys. */

  /* Arrays of structures for transformation parameters. *
   * TR_BASIC is used for all transformations.   *
   * Only one of the other structures is used for each 
   * coordinate system level. */

  TR_BASIC**    basicparam;  /* Structure holding basic transformation
			      * parameters, such as flip, offset, and
			      * rotation */
 
  TR_RAWTODET** rawtodetparam;  /* Structure holding RAWTODET
				   transformation parameters */
  TR_MULTISEG** multisegparam;  /* Structure holding MULTISEG
				   transformation parameters (linear
				   coefficients for each set of
				   segment properties) */
  TR_SKYATT**   skyattparam;    /* Structure holding SKYATT
				   parameters, such as the alignment
				   matrix and focal length. */
  TR_TELESCOPE* telescopeparam; /* Structure holding parameters
				   for conversions involving the
				   TELPOL and TELXY systems. */
		    
} TELDEF2;


/******************************************************
 * Functions to create, destroy, or output a TELDEF2 structure.
 ******************************************************/

/* Create a new TELDEF2 structure and read its contents from a file. */
int readTelDef2 /* Returns CFITSIO status */
(
 const char* filename, /* TelDef filename */
 TELDEF2** p_teldef   /* Pointer to TelDef2 structure pointer */
);


/* Print a whole TELDEF2 structure to a stream. */

void printTelDef2
 (
  TELDEF2* teldef, /* TelDef structure */
  FILE* stream /* stream */
  );


/* Free all memory associated with a TELDEF2 structure. */
void destroyTelDef2
(
 TELDEF2* teldef /* TELDEF2 structure to destroy */
 );


/*********************************************************************
 * Functions to load transformation parameters in a TELDEF2 structure.
 *********************************************************************/

/* Return the enumerated transformation type given the name of the type. */

TransformationTypeEnum getTransformationTypeNumberFromName
(char* name /* Name of transformation type */
);

/* Read the transformation types into the teldef structure from the
   TRTYPE keywords in the header of the TelDef file. */

int readTransformationTypesFromTelDef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TELDEF2* teldef /* TelDef structure */
 );

/* Read the transformation parameters of all the transformations from
   the TelDef file and store them in the teldef structure. */

int setTransformationParametersFromTelDef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TELDEF2* teldef /* teldef structure */
 );

/* Determine the number of segments in each coordinate system and
   store them in teldef->n_segments. */

int setSegmentCountInTelDef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TELDEF2* teldef /* teldef structure */
 );

/* Read the coordinate system properties from various TelDef keywords
   and store them in the coord structure. */

int setCoordinatesFromKeywordsInTeldef2 /* Returns CFITSIO status */
(
 TELDEF2* teldef, /* teldef structure */ 
 COORDDEF* coord, /* coord structure */
 fitsfile* fp,  /* TelDef file pointer */
 long sys, /* system number of lower-level system */
 int is_lowest_seg /* 1 if coord represents the lowest-indexed segment for system sys; 0 otherwise */
 );

/* Read randomization parameters from TelDef keywords. */

int readRandomizationFromTeldef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TELDEF2* teldef /* teldef structure */
);

/* Retrieve the coordinate system number from the name. */

int getCoordSystemNumberFromName /* Returns coordinate system number */
(
 TELDEF2* teldef,  /* TelDef structure */
 const char* name /* Coordinate system number */
 );
  
/****************************************************************************
 * Functions for calculating the 2D transformations for the coord. systems.
 ****************************************************************************/

/* Set the orientation of the sky coordinate tangent plane.
 * This is used for to-sky SKYATT conversions.
 * R.A. and Dec. are the "nominal pointing" in decimal degrees.
 * When roll = 0, the tangent plane will have zhat pointing toward (ra,dec).
 * xhat will point along -RA and yhat will point along +Dec. */
void setSkyCoordCenterInTelDef2
(
 TELDEF2* teldef, /* TelDef structure */
 double ra, /* right ascension of nominal pointing */
 double dec, /* Declination of nominal pointing */
 double roll /* Roll angle from positive SKY Y axis to positive declination (North) 
	      *  normally set to 0 */
 );

/* Call the various routines for setting the various types of 2D
   transformations for all the coordinate systems and segments. */

void setXform2dFromTransformationParameters
(
 TELDEF2* teldef, /* teldef structure */
 COORDDEF* origcoord, /* originating coordinate system structure */
 COORDDEF* destcoord, /* destination coordinate system structure */
 int sys, /* system number of originating coordinate system */
 int seg  /* segment number */
 );

/* Set the basic transformation parameters. 
 * Return the 2D transformation for the segment */

XFORM2D* getXform2dFromTrBasicParameters /* Returns 2D transformation */
(
 TELDEF2* teldef, /* teldef structure */
 COORDDEF* origcoord, /* originating coordinate system structure */
 COORDDEF* destcoord, /* destination coordinate system structure */ 
 TR_BASIC* basicparam, /* basic transformation structure */
 int sys               /* system number of originating corodinate system */
 );

/* Set the parameters for a RAWTODET linear coefficients transformation. 
 * Return the 2D transformation for the segment */

XFORM2D* getXform2dFromTrRawtodetLinearCoeffParameters /* Returns 2D transformation */
(
 TELDEF2* teldef, /* teldef structure */
 XFORM2D* int2dettrans,  /* 2D transformation structure for the 
			    internal-to-upper coordinate system */
 TR_RAWTODET* rawtodetparam, /* Structure holding transformation parameters */
 int sys, /* system number of originating coordinate structure */
 int seg /* segment number of originating coordinate system */
 );

/* Set the parameters for a RAWTODET pixel corner map transformation.
 * Return the 2D transformation for the segment */

XFORM2D* getXform2dFromTrRawtodetCornerMapParameters /* Returns 2D transformation */
(
 TELDEF2* teldef, /* teldef structure */
 XFORM2D* int2hightrans, /* 2D transformation structure for the 
			    internal-to-upper coordinate system */
 COORDDEF* origcoord, /* originating coordinate system structure */
 TR_RAWTODET* rawtodetparam, /* structure holding transformation parameters */
 int sys, /* system number of originating coordinate structure */
 int seg  /* segment number of originating coordinate system */
);

/* Set the parameters for a MULTISEG linear coefficients transformation.
 * Return the 2D transformation for the segment. */
				  
XFORM2D* getXform2dFromTrMultisegLinearCoeffParameters /* Returns 2D transformation */
(
 TELDEF2* teldef, /* teldef structure */
 XFORM2D* int2hightrans, /* 2D transformation structure for the 
			 internal-to-upper coordinate system */
 TR_MULTISEG* multisegparam, /* structure holding transformation parameters */
 int sys, /* system number of originating coordinate structure */
 int seg  /* segment number of originating coordinate system */
 );


/****************************************************************************
 * Functions for applying the transformations to coordinate values
 ****************************************************************************/

/* Convert coordinates from lower- to higher-level coord. system using the RAWTODET transformation. */

int convertToHigherCoordRawtodet /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* teldef structure */
 double lowx, /* x coordinate of lower-level system */
 double lowy, /* y coordinate of lower-level system */
 double* highx, /* x coordinate of higher-level system */
 double* highy, /* y coordinate of higher-level system */
 int lowsys, /* number of lower-level system */
 int lowseg  /* segment number of lower-level system */
);

/* Convert coordinates from higher- to lower-level coord. system using the RAWTODET transformation. */

int convertToLowerCoordRawtodet /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* teldef structure */
 double* lowx, /* x coordinate of lower-level system */
 double* lowy, /* y coordinate of lower-level system */
 double highx, /* x coordinate of higher-level system */
 double highy, /* y coordinate of higher-level system */
 int lowsys, /* number of lower-level system */
 int lowseg  /* segment number of lower-level system */
);

/* Convert coordinates from lower- to higher-level coord. system using the BASIC transformation. */

int convertToHigherCoordBasic /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* teldef structure */
 double lowx, /* x coordinate of lower-level system */
 double lowy, /* y coordinate of lower-level system */
 double* highx, /* x coordinate of higher-level system */
 double* highy, /* y coordinate of higher-level system */
 int lowsys /* number of lower-level system */
);

/* Convert coordinates from higher- to lower-level coord. system using the BASIC transformation. */

int convertToLowerCoordBasic /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* teldef structure */
 double* lowx, /* x coordinate of lower-level system */
 double* lowy, /* y coordinate of lower-level system */
 double highx, /* x coordinate of higher-level system */
 double highy, /* y coordinate of higher-level system */
 int lowsys /* number of lower-level system */
);

/* Convert coordinates from lower- to higher-level coord. system using the MULTISEG transformation. */

int convertToHigherCoordMultiseg /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* teldef structure */
 double lowx, /* x coordinate of lower-level system */
 double lowy, /* y coordinate of lower-level system */
 double* highx, /* x coordinate of higher-level system */
 double* highy, /* y coordinate of higher-level system */
 int lowsys, /* number of lower-level system */
 int* lowprops,  /* segment properties of lower-level system */
 long window_offset_x, /* x offset for windowing */
 long window_offset_y  /* y offset for windowing */
 );

/* Convert coordinates from higher- to lower-level coord. system using the MULTISEG transformation. */

int convertToLowerCoordMultiseg /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* teldef structure */
 double* lowx, /* x coordinate of lower-level system */
 double* lowy, /* y coordinate of lower-level system */
 double highx, /* x coordinate of higher-level system */
 double highy, /* y coordinate of higher-level system */
 int lowsys, /* number of lower-level system */
 int* lowprops,  /* segment properties of lower-level system */
 long window_offset_x, /* x offset for windowing */
 long window_offset_y  /* y offset for windowing */
 );

/* Convert coordinates from lower- to higher-level coord. system using
   the SKYATT transformation. This function will calculate the
   transformation for a new attitude quaternion and earth velocity. */

int convertToHigherCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* TelDef structure */
 double lowx, /* x coordinate of lower-level system */
 double lowy, /* y coordinate of lower-level system */
 double* highx, /* x coordinate of higher-level system */
 double* highy, /* y coordinate of higher-level system */
 int lowsys, /* number of lower-level system */
 QUAT* q,    /* attitude quaternion  */
 double v,   /* earth speed */
 double vhat[3] /* earth velocity vector */
 );

/* Convert coordinates from higher- to lower-level coord. system using
   the SKYATT transformation. This function will calculate the
   transformation for a new attitude quaternion and earth velocity. */

int convertToLowerCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* TelDef structure */
 double* lowx, /* x coordinate of lower-level system */
 double* lowy, /* y coordinate of lower-level system */
 double highx, /* x coordinate of higher-level system */
 double highy, /* y coordinate of higher-level system */
 int lowsys, /* number of lower-level system */
 QUAT* q,    /* attitude quaternion  */
 double v,   /* earth speed */
 double vhat[3] /* earth velocity vector */
 );

/* Convert coordinates from lower- to higher-level coord. system using
   the SKYATT transformation. This function will use the previously
   calculated transformation, which is a time-saver when the attitude
   quaternion and earth velocity have not changed. */

int repeatConvertToHigherCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* TelDef structure */
 double lowx, /* x coordinate of lower-level system */
 double lowy, /* y coordinate of lower-level system */
 double* highx, /* x coordinate of higher-level system */
 double* highy, /* y coordinate of higher-level system */
 int lowsys /* number of lower-level system */
 );

/* Convert coordinates from higher- to lower-level coord. system using
   the SKYATT transformation. This function will use the previously
   calculated transformation, which is a time-saver when the attitude
   quaternion and earth velocity have not changed. */

int repeatConvertToLowerCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
 TELDEF2* teldef, /* TelDef structure */
 double* lowx, /* x coordinate of lower-level system */
 double* lowy, /* y coordinate of lower-level system */
 double highx, /* x coordinate of higher-level system */
 double highy, /* y coordinate of higher-level system */
 int lowsys /* number of lower-level system */
 );

/* Determine the TelDef Format Specification version of a TelDef file. */

double getTelDefFormatVersion /* Returns TelDef Format Spec version */
 (
  char* filename /* Teldef filename */
  ); 

/****************************************************************************
 * Functions for applying nonlinear corrections in a RAWTODET transformation
 ****************************************************************************/

/* Determine if nonlinear corrections are needed, and if they are, read them
 * from the TelDef file. */
int readNonLinearCorrectionsInTeldef2
(
 fitsfile* fp, /* TelDef file pointer */
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
 COORDDEF* origcoord, /* COORDDEF structure for lowest-numbered segment of originating coordinate system */
 char* filename /* TelDef filename */
);

/* Read nonlinear correction image map from TelDef file. */
int readImageMapXformFromTeldef2
(
 fitsfile* fp, /* Pointer to TelDef file */
 TR_RAWTODET* rawtodetparam, /* Pointer to TR_RAWTODET structure */
 COORDDEF* origcoord, /* COORDDEF structure for lowest-numbered segment of originating coordinate system */
 char* filename, /* TelDef filename */
 int inv_map /* Flag for inverse transformation */
 );

/* Determine if the nonlinear corrections are expressed in the originatingr or destination coordinates of
 * the transformation.  If they are expressed in orig. coordinates, the corrections need to
 * be transformed to the dest. coordinates, since the corrections are applied after the linear
 * part of the RAWTODET transformation. */
int isDistortionInOrigCoords
(
 fitsfile* fp, /* Pointer to TelDef file */
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
 char* filename /* TelDef filename */
 );



/************************************************************************
* Functions to handle FITSIO errors while reading TelDef files.
************************************************************************/

/* Handle FITSIO errors while reading Teldef Files. Prints error
   messages but don't terminate execution if there is an error. */
void checkFITSerrors
(
 int status, /* CFITSIO status */
 char* doing, /* FITS action that was just performed, given as a present participle */
 char* filename /* TelDef filename */
);



#define TELDEF2_INCLUDED
#endif  /* TELDEF2_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif
