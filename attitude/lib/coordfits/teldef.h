#ifndef TELDEF_INCLUDED

#define TELDEF_UPDATED_OCT_2003

#include "fitsio.h"
#include "coord.h"
#include "align.h"
/*****************************************************************************
******************************************************************************
* A TELDEF structure defines the coordinate systems used for a given 
* instrument. We make the following assumptions:
*
* - a single coordinate system can be adequately defined by a COORDDEF
*   structure (see coorddef.h).
*
* - There is a "raw" coordinate system which represents values
*   basicly taken from the telemetry. We assume that detector is composed of an
*   arbitrary number of segments arranged arbitrarily in the focal plane.
*   Each segment gets its own COORDDEF structure. We also make allowances
*   for non-linearity between the raw coordinates and the "real" coordinates
*   in the focal plane.
*
* - There can be any number of "detector" coordinates. We assume that each
*   type of detector coordinate differs from the others by a linear 
*   transformation. 
*
* - there is a SKY coordinate system representing positions in e.g. RA and
*   Dec. coordinates.
*
*****************************************************************************/
typedef struct {

/*******************************************
* input file, mission and instrument names *
*******************************************/
char* filename;
char mission[FLEN_VALUE];
char instrument[FLEN_VALUE];

/******************
* raw coordinates *
******************/
int nsegments;
int min_segment;

COORDDEF **raw;
double det0x_min; /* bounding box for entire */ 
double det0x_max; /* raw coordinate system   */
double det0y_min; /* in first level detector */
double det0y_max; /* coordinates             */

int raw_corrections_needed; /* do we need to make non-linear corrections? */
MAPXFORM* raw_map;

char* seg_col_name;

/***********************
* detector coordinates *
***********************/
int n_det_levels; /* number of types of detector coordinates */
COORDDEF **det;

/******************
* sky coordinates *
******************/
int sky_from; /* Detector coordinate level to be converted to sky coordinates */
COORDDEF* sky;


ALIGN* alignment;

double focal_length;
double sky_pix_per_radian; /* plate scale used to det -> sky conversions */

/*************************************************************************
* the following items are not read from the teldef file, but
* can be set "manually" after the teldef file has been read.
* q0 is the quaternion describing the nominal pointing - i.e. it gives the
* the orientation of the sky coordinate plane with respect to the celestial
* fcoordinate axies.
* rot0 is a 3x3 rotation matrix equivalent to q0. This is needed to
* do the aberration calculation efficiently.
* delta_q, xrt, and det2sky are temporary storage for 
* convertDetectorToSkyUsingTeldef
* they record the last rotation between q0 and the current pointing,
* xrt pointing and detector to sky transform calculated.
**************************************************************************/
QUAT* q0; 
ROTMATRIX* rot0;

QUAT* delta_q;
QUAT*    xrt;
XFORM2D* det2sky;


} TELDEF;

/****************************************************************************
*****************************************************************************
* create a new TELDEF structure and read its contents from a file.
****************************************************************************/
TELDEF* readTelDef(char* filename);

/****************************************************************************
* returns the number of segments - i.e. the number of different raw coordinates.
* Note that this is not the same as the nsegments field if min_segment > 0
*****************************************************************************/
int getTelDefSegmentCount(TELDEF* teldef);

/***********************************************************************
*************************************************************************
* set nsegments and allocate the array of raw COORDDEF structures
* Assumes the raw COORDDEFs have not been previously allocated
************************************************************************/
void setNsegmentsInTelDef(TELDEF* teldef, int min_segment, int nsegments);


/***********************************************************************
*************************************************************************
* set n_det_levels and allocate the array of det COORDDEF structures
* Assumes the det COORDDEFs have not been previously allocated.
* Note n_det_levels is the number of types of detector coordinates
(e.g. 2 for DET + FOC coordinates).
************************************************************************/
void setDetLevelsInTelDef(TELDEF* teldef, int n_det_levels);


/***********************************************************************
*************************************************************************
* after the raw COORDDEF structures in a TELDEF structure have been
* set, you should call this routine to calculate the bouding box
* of the raw coordinate system in bottom level detector coordinates 
************************************************************************/
void calculateBoundingBoxInTeldef(TELDEF* teldef);

/********************************************************************
*********************************************************************
* This function 
* tries to read a double valued keyword with a given prefix and
* returns a default value if the keyword is not found.
********************************************************************/
double readTelDefKey(TELDEF* teldef, fitsfile* fp,
                     char* key, char* key_end, char* suffix, 
                     double def);

/*****************************************************************************
* This function reads a linear transformation from one coordinate system
* to another as defined by a set of keywords as follows:
*
* xxx_XOFF - x offset between centers default 0.
* xxx_YOFF - y offset between centers default 0.
* xxxXFLIP - x inversion (-1 or 1)    default 1.
* xxxYFLIP - y inversion (-1 or 1)    default -1. for RAW, otherwise 1. 
* xxx_SCAL - x and y scaling          default 1.
* xxx_ROTD - rotation in degrees      default 0.
*
* Where "xxx" is the keyword name of the destination coordinate system.
*
* These define a offset between the centers of the two coordinate systems,
* a scaling and possible inversion applied such that the center of the
* destination coordinate system remains fixed, and a rotation about the center
* of the destination coordinate system counterclockwise in degrees.
* 
* If any of these keywords is missing the default is no transformation
* except in the case of YFLIP. Here a transformation from raw 
* coordinates to detector coordinates flips the y coordiinates by default,
* but a transformation from detector to detector coordinates
* does not flip by default.
*******************************************************************************/
void setXform2dFromTelDefKeywords(TELDEF* teldef, XFORM2D* trans, fitsfile* fp, 
                                  COORDDEF* orig, COORDDEF* dest );
				  

/****************************************************************************
*****************************************************************************
* Read the address space limits, pixel scale and column names
* for a given set of coordinates. The "name" element of the
* given COORDDEF structure must be set before calling this function
****************************************************************************/
void setCoordinatesFromKeywordsInTeldef(TELDEF* teldef, COORDDEF* coord,
                                        fitsfile* fp );
					
/***********************************************************************
*************************************************************************
* set the raw COORDDEFs assuming that each segment contains only a single 
* pixel given a table of pixel corner values. This is how XRS teldef files
* work. The fits file pointer must point to the extension containing 
* a table with one row per pixel. There must be at least three columns,
* one giving the segment (pixel) number (counting from zero).
* and two giving the x and y corner positions as four element vectors.
* Note teldef->det[0]->scale_x and teldef->det[0]->scale_y must be defined
* before calling this function.
* Note also that this routine may change the current HDU of the fitsfile
*
* returns 1 if the raw coordinates were set by this method and 
* returns 0 if there is no PIXEL_MAP extension
************************************************************************/
int setSinglePixelRawCoordsInTelDef(TELDEF* teldef, fitsfile* fp);

/****************************************************************************
*****************************************************************************
* read the raw coordinate information from "COE_[X/Y]n_[A/B/C]" keywords in the 
* primary header. This is the way things are done for the ASCA SIS and GIS.
* Note that this method is limited to 10 segments at most, otherwise the
* "COE" keywords would be too long. An alternative format could
* lift this restriction if needed, but large numbers of COE keywords
* can be cumbersome in a file, so perhaps it is best to define a new
* table based format for such cases.
* 
* This format works in the following way:
* a set of COE_[X/Y]n_[A/B/C] keywords define the transformation from
* raw coordinates (for segment "n") to an intermediate coordinate system.
* Then the keywords
* DET_ROTD
* RAWFLIPX
* RAWFLIPY
* define the translation from the internal cooridnate system to the bottom
* level detector coordinates.
* Note that these three keywords are optional with defaults
* DET_ROTD=0.
* RAWFLIPX=1.
* RAWFLIPY=-1.
* Note also that the intermediate coordinates will not be identical to the
* DET coordinate if all the defaults are used. This requires RAWFLIPY=1.
****************************************************************************/
int setRawFromCoefInTelDef(TELDEF* teldef, fitsfile* fp);

/****************************************************************************
*****************************************************************************
* This is a variant of setRawFromCoefInTelDef which handles some
* unique quirks in ASCA GIS teldef files.
****************************************************************************/
int setRawForASCAGISTelDef(TELDEF* teldef, fitsfile* fp);

/***************************************************************************
****************************************************************************
* read the non-linear corrections required for the raw to bottom level
* detector coordinates transformation if any are required.
***************************************************************************/
void readNonLinearCorrectionsInTeldef(TELDEF* teldef, fitsfile* fp);

/************************************************************************
*************************************************************************
* read the standard detector coordinate keywords into a TELDEF structure
*
************************************************************************/
void readTeldefDetectorCoordinates(TELDEF* teldef, fitsfile* fp);

/***********************************************************************
*************************************************************************
* set the alignment matrix which gives the transformation between
* spacecraft coordinates and bottom level detector coordinates
* The matrix is given as a rotation matrix encoded as a set of
* ALGNMnm keywords. This is stored in the TELDEF structure as
* a quaternion. We also calculate the inverse of this quaternion.
************************************************************************/
void readTelDefAlignment(TELDEF* teldef, fitsfile* fp);

/***********************************************************************
*************************************************************************
* read Sky coordinate information from a teldef file
************************************************************************/
void readTelDefSkyCoordinates(TELDEF* teldef, fitsfile* fp);

/************************************************************************
*************************************************************************
* handle FITSIO errors while reading teldef files
* prints error messages and exits if there is an error
************************************************************************/
void checkTelDefFITSerrors(int status, char* doing, TELDEF* teldef);

/*****************************************************************************
******************************************************************************
* free all memory associated with a TELDEF structure
*****************************************************************************/
void destroyTelDef(TELDEF* teldef);


/************************************************************************
*************************************************************************
* set the sky tagnent plane in the TELDEF  structure.
* This is used for detector to SKY conversions
* R.A. and Dec. are the "nominal pointing" in decimal degrees.
* the tangent plane will have zhat pointing toward (ra,dec)
* xhat will point along -RA and yhat will point along +Dec.
************************************************************************/
void setSkyCoordCenterInTeldef(TELDEF* teldef, double ra, double dec );


/****************************************************************************
******************************************************************************
* This function converts raw coordinates to bottom level detector coordinates
* Note that it takes three arguments for the segment, rawx, and rawy, all
* integers. 
* dx, and dy give the location of the event with respect to the pixel
* center. typically these are random numbers between -.5 and +.5 
* (see applyXform2dToDiscreteCoords).
* If you want to transform real valued raw coordinates, 
* use convertContRawToDetectorUsingTeldef
*****************************************************************************/
void convertRawToDetectorUsingTeldef(TELDEF* teldef, double* detx, double* dety,
                                     int seg, int rawx, int rawy, 
                                     double dx, double dy);

/****************************************************************************
******************************************************************************
* This function converts raw coordinates to bottom level detector coordinates
* Note that rawx and rawy are given as doubles. 
* See also convertRawToDetectorUsingTeldef
******************************************************************************/
void convertContRawToDetectorUsingTeldef(TELDEF* teldef, 
                                         double* detx, double* dety,
                                         int seg, double rawx, double rawy);

/******************************************************************************
******************************************************************************
* this function converts bottom level detector coordinates to raw coordinates.
* the function returns the number of the segment containing the coordinates.
* If the point does not lie on any segment the function will return -1,
* and the values of rawx and rawy will be undefined.
* If the point is on more than one overlapping segment, the results are 
* undefined.
******************************************************************************/
int convertDetectorToRawUsingTelDef(TELDEF* teldef, double* rawx, double* rawy,
                                    double detx, double dety);

/****************************************************************************
*****************************************************************************
* Convert one level of detector coordinates to the next.
* Note this function assumes the input coordinates are continuous.
* if you want to transform from discrete coordinates (adding a random
* location within the pixel) you will have to do this "by hand" using
* applyXform2dToDiscreteCoords. 
* Perhaps this is not the most efficient implimentation, particularly if
* you are transforming a large number of coordinates over more
* than two levels of coordinates.
****************************************************************************/
void convertDetectorToDetectorUsingTelDef(TELDEF* teldef, int from, int to,
                                          double* x1, double* y1,
                                          double x0, double y0);


/****************************************************************************
*****************************************************************************
* Convert bottom level detector coordinates to sky coordinates.
* q is the current spacecraft pointing
* v and vhat are the scalar and vector components of the earth's velocity
*            these two quantities are used for abberation correction.
*            aberation will not be applied if v=0.;
* Note this routine is not efficient if you have to convert many different
* coordinates which have the same pointing.
* Note also that you must first set the sky tangent plane using
* setSkyTangetPlaneInTeldef before the first call to this routine.
*****************************************************************************/
void convertDetectorToSkyUsingTeldef(TELDEF* teldef, 
                                     double* skyx, double* skyy,
                                     double  detx, double  dety, 
                                     QUAT* q, 
                                     double v, double vhat[3]);

/*****************************************************************************
******************************************************************************
* do a bottom level detector to sky coordinate conversion using the same
* pointing and aberration as in the most recent call to 
* convertDetectorToSkyUsingTeldef. This is useful if you are converting
* the coordinate for a number of events which all happened at the same time
*****************************************************************************/
void repeatDetectorToSkyTeldefConversion(TELDEF* teldef, 
                                         double* skyx, double* skyy,
                                         double  detx, double  dety );

/*****************************************************************************
******************************************************************************
* find the detector coordinate level corresponding to a given 
* 3 character coordinate type name
* returns -1 if there is no matching detector coordinate
*****************************************************************************/
int getTelDefDetLevelFromName(TELDEF* teldef, char* name);


/***************************************************************************
****************************************************************************
* read the non-linear corrections required for the raw to bottom level
* detector coordinates transformation if any are required.
***************************************************************************/
void readDetToMapPixelScaling(double* scale, double* origin, int axis,
                              fitsfile* fp);


#define TELDEF_INCLUDED
#endif  /* TELDEF_INCLUDED */
