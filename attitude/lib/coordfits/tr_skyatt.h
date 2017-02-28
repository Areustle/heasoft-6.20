#ifdef __cplusplus
extern "C" {
#endif

#ifndef TR_SKYATT_INCLUDED

#include "fitsio.h"
#include "align.h"

/****************************************************************************
* The TR_SKYATT structure holds the parameters needed for a transformation
* that includes an attitude adjustment, such as converting to sky
* coordinates.
* The transformation parameters in common with the TR_BASIC
* transformation are held in the TR_BASIC structure and are not repeated
* here.
*****************************************************************************/
typedef struct {

  /* Coordinate system identification */

  char lowcoordsysname[FLEN_VALUE]; /* Name of originating coord. sys. */
  char highcoordsysname[FLEN_VALUE]; /* Name of destination coord. sys. */
  int low_sys; /* Number of orig. coord. sys. */
  int high_sys; /* Number of dest. coord. sys. */

  ALIGN* alignment; /* Stores the alignment matrix */
  
  double focal_length; /* Focal length of telescope */
  double sky_pix_per_radian; /* Plate scale used in to-SKY conversions */
  double deg_per_sky_unit; /* Conversion factor from sky units to degrees */
  
  /* The following items are not read from the TelDef file, but
   * can be set "manually" after the TelDef file has been read.
   * delta_q, xrt, and det2sky are temporary storage for 
   * convertDetectorToSkyUsingTeldef
   * they record the last rotation between q0 and the current pointing,
   * xrt pointing and detector to sky transform calculated. */

  QUAT* q0; /* Quaternion of nominal pointing, i.e., the orientation
	     * of the sky coordinate plane with respect to the celestial
	     * coordinate axes. */
  ROTMATRIX* rot0; /* Rotation matrix equivalent to nominal pointing q0 */
  
  /* Temporary storage for the to-SKY transformation, recording the 
   * current pointing, xrt pointing, and 2D transformation */

  QUAT* delta_q; /* Current pointing */
  QUAT* xrt; /* Current pointing corrected with alignment matrix. */
  XFORM2D* det2sky; /* Transformation to dest. coord. sys. */
  XFORM2D* sky2det; /* Transformation to orig. coord. sys. */
		    
} TR_SKYATT;


/* Create a new TR_SKYATT structure and read its contents from a TelDef file. */
int readTrSkyAtt /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_SKYATT** p_skyattparam, /* Pointer to TR_SKYATT structure pointer */
 char* lowcoordsysname, /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys,  /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
);

/* Read TelDef alignment matrix into an ALIGN structure */
int readTelDef2Alignment /* Return CFITSIO structure */
(
 fitsfile* fp, /* TelDef file pointer */
 ALIGN** p_align, /* Pointer to ALIGN structure pointer */
 char* lowcoordsysname, /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
);

/* Print a TR_SKYATT structure to a stream */
void printTrSkyAtt
(
 TR_SKYATT* skyattparam, /* TR_SKYATT structure to print */
 FILE* stream /* Destination stream */
 );

/* Destroy a TR_SKYATT structure. */
void destroyTrSkyAtt
(
 TR_SKYATT* skyattparam /* TR_SKYATT structure to destroy */
 );
 
#define TR_SKYATT_INCLUDED
#endif  /* TR_SKYATT_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif
