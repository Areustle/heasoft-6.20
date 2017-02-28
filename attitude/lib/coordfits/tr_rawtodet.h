#ifdef __cplusplus
extern "C" {
#endif

#ifndef TR_RAWTODET_INCLUDED

#include "fitsio.h"

/* Define maximum number of linear coefficient segments */
#define MAX_LINEAR_COEFF_SEGS 10

/* Raw method types - which Raw-To-Det transformation is used. */
enum RM_TYPE {RM_UNKNOWN, RM_CORNER_LIST, RM_LINEAR_COEFF, RM_ASCA_GIS};

/****************************************************************************
* The TR_RAWTODET structure holds the parameters needed for a variety
* of transformations from a multi-segment coordinate system to a
* single-segment system. The transformation parameters in common with
* the TR_BASIC transformation are held in the TR_BASIC structure and
* are not repeated here.
*****************************************************************************/
typedef struct {

  /* Coordinate system identification */

  char lowcoordsysname[FLEN_VALUE]; /* Name of originating coord. sys. */
  char highcoordsysname[FLEN_VALUE]; /* Name of destination coord. sys. */
  int low_sys; /* Number of orig. coord. sys. */
  int high_sys; /* Number of dest. coord. sys. */

  /* Internal coordinate system center */

  double int_cen_x; /* x coordinate */
  double int_cen_y; /* y coordinate */
  int int_cen_found; /* Has the center been found? */

  /* Transformation method */

  enum RM_TYPE rawmethod;
  
  /* Segment info */

  int n_segs; /* Number of segments in the orig. coord. sys. */
  int min_seg; /* Number of first segment */
  char segcolname[FLEN_VALUE]; /* Event file column name for the segment number */

  /* Pixel corner map columns */

  int* corner_seg; /* Segment number */
  double** corner_x; /* x coordinates of segment corners: corner_x[seg][corner] */
  double** corner_y; /* y coordinates of segment corners: corner_y[seg][corner] */

  /* Linear Coefficients: maximum of 10 segments 0-9. */

  double coeff_x_a[MAX_LINEAR_COEFF_SEGS];
  double coeff_x_b[MAX_LINEAR_COEFF_SEGS];
  double coeff_x_c[MAX_LINEAR_COEFF_SEGS];
  double coeff_y_a[MAX_LINEAR_COEFF_SEGS];
  double coeff_y_b[MAX_LINEAR_COEFF_SEGS];
  double coeff_y_c[MAX_LINEAR_COEFF_SEGS];

  /* Nonlinear corrections */

  int use_nonlinear_corr;
  int distortion_in_orig_coord;
  MAPXFORM* corr_map;
  MAPXFORM* inv_corr_map;

} TR_RAWTODET;


/* Create a new TR_RAWTODET structure and read its contents from a TelDef file. */
int readTrRawtodet /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_RAWTODET** p_rawtodetparam, /* Pointer to TR_RAWTODET structure pointer */
 char* lowcoordsysname,  /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys, /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
 );

/* Try to read pixel corner map and update p_rawmethod if successful. 
 * A failure may mean that this is the wrong type of RAWTODET 
 * transformation. */
int readPixelCornerMapInTelDef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* Teldef file pointer */
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
 char* filename, /* TelDef filename */
 enum RM_TYPE* p_rawmethod /* Returns type of RAWTODET transformation */
 );

/* Try to read linear coefficients of segments and update p_rawmethod 
 * if successful.  A failure may mean that this is the wrong type of 
 * RAWTODET transformation. */
int readLinearCoeffInTelDef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
 char* filename, /* TelDef filename */
 enum RM_TYPE* p_rawmethod /* Returns type of RAWTODET transformation */
);

/* Print a TR_RAWTODET structure to a stream */
void printTrRawtodet
(
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure to print */
 FILE* stream /* destination stream */
 );

/* Destroy a TR_RAWTODET structure. */
void destroyTrRawtodet
(
 TR_RAWTODET* rawtodetparam /* TR_RAWTODET structure to destroy */
 );
 
#define TR_RAWTODET_INCLUDED
#endif  /* TR_RAWTODET_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif
