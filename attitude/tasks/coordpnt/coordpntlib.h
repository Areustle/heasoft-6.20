/** \file coordpntlib.h
    \brief Data structure and function declarations for coordpnt
    \author Robert Hill
    \date $Date: 2016/03/12 19:21:35 $
*/

/** \addtogroup tool_coordevt
    @{
*/

#ifndef TOOL_COORDPNT_COORDPNTLIB_H
#define TOOL_COORDPNT_COORDPNTLIB_H

#include "teldef2.h"
#include "coordfits2.h"
#include "region.h"
#include "fitsio.h"

/* ====================================================================== */

/* Maximum number of user parameters in par file */
#define MAX_PARAM 30

/* Maximum number of properties to be parsed from CSV */
#define MAX_PROPS 100  

/* Maximum number of rows in MULTISEG coeffs table */
#define MAX_ROWS 100  

/* Maximum number of allowed region shapes */
#define MAX_SHAPES 15

/* Maximum number of allowed region transformations */
#define MAX_TRANS 11

/* Maximum number of pixels in a detector with enumerated pixels */
#define MAX_PIXELS 500

/* Maximum number of pixel sets allowed in a pixel list */
#define MAX_PIXEL_SETS 100

/* Length of character string denoting a coordinate system */
#define COORDSYSNAME_LENGTH 10

/* ========================================================== */

/* Move routines to library:  */

/* 

fillWCSstructureFromParams (existing)
convertCoordRADEC (existing)
convertCoordPolar (new)

*/

/*
HAK 150430 – Begin new functions to be included in coordpnt.cxx
The new local subroutines are:
convertCoordPolar
convertCoordCartesian
convertRegionLengthTelescope
convertRegionAngleTelescope
*/

/* ========================================================== */


typedef struct {

  /* Parameter values */
  char* input;
  char* outfile;
  char* telescop;
  char* instrume;
  double ra;
  double dec;
  double roll;
  double ranom;
  double decnom;
  double rollnom;
  char* teldeffile;
  char* startsysinput;
  char* stopsysinput;
  char* multisegpar;
  int startsys_is_radec;
  int startsys_is_telpol;
  int startsys_is_telxy;
  int stopsys_is_radec;
  int stopsys_is_telpol;
  int stopsys_is_telxy;
  long rawtodetseg;
  char* pixeltest;
  long winoffsetx;
  long winoffsety;
  double outx;
  double outy;
  char clobber;

  /* Flags for undefined numerical values */
  int ra_undef;
  int dec_undef;
  int roll_undef;
  int ranom_undef;
  int decnom_undef;
  int rollnom_undef;
  int outx_undef;
  int outy_undef;

  /* Derived values */
  char startsysname[FLEN_VALUE];
  char stopsysname[FLEN_VALUE];
  char in_file[FLEN_FILENAME];
  int startsysnum;
  int stopsysnum;
  int lowsysnum;
  int highsysnum;
  long in_pixel;
  double in_x;
  double in_y;
  int input_is_reg_file;
  int input_is_pixel;
  int input_is_point;
} Param;

/* Structure type for lists of numbered pixels within regions */
typedef struct {
  long pixel_nums[MAX_PIXELS];  /* MAX_PIXELS defined at head of this file */
  long n_pixels;
} PixelSet;

typedef struct {
  PixelSet* pixel_sets[MAX_PIXEL_SETS];  /* MAX_PIXEL_SETS defined at head of this file */
  long n_sets;
} PixelList;

/* Symbols for the transformations of region components */
typedef enum {
  e_nullTrans,
  e_pointTrans, 
  e_lengthTrans,
  e_angleTrans,
  e_numberTrans
} TransEnum;

/** Create and initialize Param structure 
 *  \return pointer to new param structure
 */
Param* createParam();

/** Free Param structure 
 *  \param[in] param pointer to param structure
 */
void destroyParam (Param* param);

/** \brief Get the actual TelDef filename based on TELESCOP and INSTRUME
 *  \param[out] actual_filename Filename of TelDef file
 *  \param[in] telescop Value of TELESCOP parameter
 *  \param[in] instrume Value of INSTRUME parameter
 */
int resolveTeldefFilename(char* actual_filename, char* telescop, char* instrume);

/** \brief Copy information into a WCSdata structure.
 *  \param[in] ra Right Ascension
 *  \param[in] dec Declination
 *  \param[in] roll Roll angle
 *  \param[in] coorddef Structure defining a coordinate system
 *  \param[in] skyatt Structure defining a SKYATT transformation
 */
WCSdata* fillWCSstructureFromParams (double ra, double dec, double roll, 
  COORDDEF* coorddef, TR_SKYATT* skyatt);

/** \brief Convert SKY coords to or from equatorial coords.
 *  \param[in] conv_to_higher Flag for conversion direction
 *  \param[inout] lowx X in SKY system
 *  \param[inout] lowy Y in SKy system
 *  \param[inout] highx Right Ascension
 *  \param[inout] highy Declination
 *  \param[in] wcs Structure with WCS description
 */
int convertCoordRADEC (int conv_to_higher, double* lowx, double* lowy, 
  double* highx, double* highy, WCSdata* wcs);

/** \brief Convert coordinates from the ordinary sequence to telescope polar 
  * \param[in] conv_to_higher_tel Flag for conversion direction
  * \param[in] sys Number of the origin system
  * \param[inout] lowx First coordinate in lower system
  * \param[inout] lowy Second coordinate in lower system
  * \param[inout] highx First coordinate in higher system
  * \param[inout] highy Second coordinate in higher system
  * \param[inout] teldef Information from TelDef file
  */
int convertCoordPolar(
  int conv_to_higher_tel,
  double* lowx, double* lowy, double* highx, double* highy, TELDEF2* teldef);

/** \brief Convert coordinates from the ordinary sequence to telescope Cartesian
  * \param[in] conv_to_higher_tel Flag for conversion direction
  * \param[in] sys Number of the origin system
  * \param[inout] lowx First coordinate in lower system
  * \param[inout] lowy Second coordinate in lower system
  * \param[inout] highx First coordinate in higher system
  * \param[inout] highy Second coordinate in higher system
  * \param[inout] teldef Information from TelDef file
  */
int convertCoordCartesian(
  int conv_to_higher_tel, int sys, double* lowx, double* lowy,
  double* highx, double* highy, TELDEF2* teldef);

/** \brief Convert coordinates from RAW system to pixel number
 *  param[in] teldef TelDef structure
 *  param[in] lowx X coordinate of lower-level system 
 *  param[in] lowy Y coordinate of lower-level system 
 *  param[in] highx X coordinate of higher-level system 
 *  param[in] highy Y coordinate of higher-level system 
 *  param[in] lowsys Number of lower-level system 
 */
int convertToLowerCoordRawToPixel (TELDEF2* teldef, double* lowx, double* lowy, 
  double highx, double highy, int lowsys);

/** \brief Convert a length parameter for a region
 *  param[in] conv_to_higher Flag for conversion direction
 *  param[in] teldef TelDef structure
 *  param[in] length_in input length
 *  param[out] length_out output length
 *  param[in] sys number of lower coordinate system
 */
int convertRegionLength (
  int conv_to_higher,
  TELDEF2* teldef,
  double length_in,
  double* length_out,
  int sys
);

/** \brief Convert a length parameter for a region
 *  param[in] conv_to_higher Flag for conversion direction
 *  param[in] length_in input length
 *  param[out] length_out output length
 *  param[in] sys number of lower coordinate system
 */
int convertRegionLengthRADec (
  int conv_to_higher,
  double length_in,
  double* length_out,
  WCSdata* wcs
);

/** \brief Convert a length parameter to telescope coordinates
  * \param[in] conv_to_higher_tel Flag for conversion direction
  * \param[in] teldef Information from TelDef file
  * \param[in] length_in Input length
  * \param[out] length_out Output length
  */
int convertRegionLengthTelescope (
  int conv_to_higher_tel,
  TELDEF2* teldef, 
  double length_in, 
  double* length_out 
);

/** \brief Convert an angle parameter for a region
 *  param[in] conv_to_higher Flag for conversion direction
 *  param[in] teldef TelDef structure
 *  param[in] in_angle input angle
 *  param[out] out_angle output angle
 *  param[in] sys number of lower coordinate system
 */
int convertRegionAngle (
  int conv_to_higher,
  TELDEF2* teldef,
  double in_angle,
  double* out_angle,
  int sys
);

/** \brief Convert an angle parameter for a region
 *    using a multiseg transformation
 *  param[in] conv_to_higher Flag for conversion direction
 *  param[in] teldef TelDef structure
 *  param[in] in_angle input angle
 *  param[out] out_angle output angle
 *  param[in] sys number of lower coordinate system
 *  param[in] seg segment number in lower coordinate system
 */
int convertRegionAngleMultiseg (
  int conv_to_higher,
  TELDEF2* teldef,
  double in_angle,
  double* out_angle,
  int sys,
  long seg
);

/** \brief Convert an angle parameter for a region
 *    using a SKYATT transformation
 *  param[in] conv_to_higher Flag for conversion direction
 *  param[in] teldef TelDef structure
 *  param[in] in_angle input angle
 *  param[out] out_angle output angle
 *  param[in] sys number of lower coordinate system
 *  param[in] quat attitude quaternion
 */
int convertRegionAngleSkyAtt (
  int conv_to_higher,
  TELDEF2* teldef,
  double in_angle,
  double* out_angle,
  int sys,
  QUAT* q
);

/** \brief Convert an angle parameter for a region
 *    from SKY to RA and Dec
 *  param[in] conv_to_higher Flag for conversion direction
 *  param[in] in_angle input angle
 *  param[out] out_angle output angle
 *  param[in] sys number of lower coordinate system
 *  param[in] wcs structure with WCS information
 */
int convertRegionAngleRADec (
  int conv_to_higher,
  double in_angle,
  double* out_angle,
  int sys,
  WCSdata* wcs
);

/** \brief Convert an angle parameter for a region
 *    according to relative rotation
 *  param[in] conv_to_higher_tel Flag for conversion direction
 *  param[in] teldef Information from TelDef file
 *  param[in] in_angle input angle
 *  param[in] sys number of origin coordinate system
 *  param[out] out_angle output angle
 */
int convertRegionAngleTelescope (
  /* Convert an angle parameter for a region based on the relative
   * rotation between the two coordinate systems. */
  int conv_to_higher_tel,
  TELDEF2* teldef,
  double in_angle,
  int sys,
  double* out_angle
);

/** \brief For an instrument with numbered pixels, find
 *    those within a region
 *  param[in] region structure with description of the region
 *  param[in] sys number of the RAW coordinate system
 *  param[in] teldef TelDef structure
 *  param[in] pixeltest method to test if a pixel is in the region
 *  param[out] pixel_set list of pixel numbers
 */
void findPixelsWithinRegion (
  SAORegion* region,
  int sys,
  TELDEF2* teldef,
  char* pixeltest,
  PixelSet** pixel_set
);

/** \brief Clone a region struction to a new region structure
 *  param[in] SAORegion_src input region structure
 *  param[out] SAORegion_dest output region structure
 */
void cloneSAORegion (
  SAORegion* SAORegion_src,
  SAORegion** SAORegion_dest
);

/** \brief Write a region description to an output file
 *  param[in] trans_type_table table of region types
 *  param[in] SAORegion_output region to write out
 *  param[in] stopsys_is_radec flag for whether output coord sys is RA, Dec
 *  param[in] pixel_list list of pixels for instruments with enumerated pixels
 *  param[in] outfile output file name
 *  param[in] clobber value of clobber parameter
 *  \return 0 for success, 1 for failure
 */
int writeSAORegion (
  TransEnum** trans_type_table,
  SAORegion* SAORegion_output,
  int stopsys_is_radec,
  PixelList* pixel_list,
  char* outfile,
  int clobber
);

/** \brief Read an ASCII region file 
 *  param[in] filename input file
 *  param[in] wcs WCS information structure
 *  param[out] Rgn structure holding region description
 *  param[ou] status code for success (0) or failure (!=0)
 */
int coordpnt_read_ascii_region( 
  const char *filename,
  WCSdata    *wcs,
  SAORegion  **Rgn,
  int        *status );

/** \brief Allocate a PixelSet structure
 *  \return new structure
 */
PixelSet* createPixelSet(void);

/** \brief Append a pixel number to a PixelSet
 *  \param[in] pixel_set structure to which number is to be appended
 *  \param[in] pixnum number to append
 *  \return 0 for success, 1 for failure
 */
int appendPixelToPixelSet(PixelSet* pixel_set, long pixnum);

/** \brief Append the pixel numbers in one PixelSet to another PixelSet
 *  \param[in] pixel_set_det structure to which numbers are to be appended
 *  \param[in] pixel_set_src structure supplying the number
 *  \return 0 for success, 1 for failure
 */
int appendPixelSetToPixelSet(PixelSet* pixel_set_dest, PixelSet* pixel_set_src);

/** \brief qsort-callable auxiliary function
 *  \param[in] px1 a pixel number
 *  \param[in] px2 a pixel number
 *  \return -1 for px1<px2, 0 for px1==px2, 1 for px1>px2
 */
int cfPixel(const void* px1, const void* px2);

/** \brief Sort and remove duplicates from PixelSet
 *  \param[in] pixel_set structure with number to be sorted
 */
void sortUniquePixelSet(PixelSet* pixel_set);

/** \brief Deallocate a PixelSet
  * \param[in] pixel_set structure to be deallocated
  */
void destroyPixelSet(PixelSet* pixel_set);

/** \brief Allocate a PixelList structure
 *  \return new structure
 */
PixelList* createPixelList(void);

/** \brief Deallocate a PixelList structure
 *  \param[in] pixel_list structure to be deallocated
 */
void destroyPixelList(PixelList* pixel_list);

/** \brief Append a PixelSet to a PixelList
 *  \param[in] pixel_list structure to which pixel_set is to be appended
 *  \param[in] pixel_set structure to append
 *  \return 0 for success, 1 for failure
 */
int appendPixelSetToPixelList(PixelList* pixel_list, PixelSet* pixel_set);

/** \brief Sort and remove duplicates from pixel sets within pixel list
 *  \param[in] pixel_list structure with pixel sets to be sorted
 */
void sortUniquePixelSetsInList(PixelList* pixel_list);

/** @} */

#endif /* TOOL_COORDPNT_COORDPNTLIB_H  */

/* ====================================================================== */

/* Revision Log
   $Log: coordpntlib.h,v $
   Revision 1.6  2016/03/12 19:21:35  rshill
   For output pixel lists, merge all regions.

   Revision 1.5  2016/01/13 03:55:41  klrutkow
   added new param pixeltest to findPixelsWithinRegion and Param struct

   Revision 1.4  2015/06/26 17:58:29  rshill
   Implemented resolution of TelDef filename via CALDB.

   Revision 1.3  2015/06/15 21:22:34  rshill
   Debugged the simple cases of TELXY or TELPOL to and from OPTCOORD.

   Revision 1.2  2015/06/10 00:10:28  rshill
   Added telescope coordinate TELPOL and TELXY.

   Revision 1.1  2015/05/22 19:58:36  rshill
   Converted to straight C.  Split into main and lib files.  Moved from astroh/gen/tasks.


*/
