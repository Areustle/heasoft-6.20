#ifdef __cplusplus
extern "C" {
#endif

#ifndef TR_MULTISEG_INCLUDED

#include "fitsio.h"

/* Number of MULTISEG coefficients used in the transformation */
#define N_MULTISEG_COEFF 10 

/****************************************************************************
* The TR_MULTISEG structure holds the parameters needed for a transformation
* between multi-segment coordinate systems using coefficients for the
* transformation.  The transformation parameters in common with the TR_BASIC
* transformation are held in the TR_BASIC structure and are not repeated
* here.
*****************************************************************************/
typedef struct {

  /* Coord. sys. identification */

  char lowcoordsysname[FLEN_VALUE]; /* Name of originating coord. sys. */
  char highcoordsysname[FLEN_VALUE]; /* Name of destination coord. sys. */
  int low_sys; /* Number of orig. coord. sys. */
  int high_sys; /* Number of dest. coord. sys. */

  /* column arrays for the MULTISEGn_COEFF table */

  long n_properties;     /* Stores the number of property columns 
			   from the NPROP keyword */
  char** propertynames; /* Stores values of PROPp keyword names, which give
			   the corresponding event file column names propertynames[prop] */
  long n_rows;          /* Stores the number of table rows */
  long n_cols;          /* Stores the number of table columns */
  long** properties;    /* Stores property columns properties[prop][row] */
  long* min_properties; /* Stores minimum values of property columns: min_properties[prop] */
  long* max_properties; /* Stores minimum values of property columns: max_properties[prop] */

  double* coeff_x_a;    /* Stores column COEFF_X_A */
  double* coeff_x_b;    /* Stores column COEFF_X_B */
  double* coeff_x_c;    /* Stores column COEFF_X_C */
  double* coeff_x_d;    /* Stores column COEFF_X_D */
  double* coeff_x_e;    /* Stores column COEFF_X_E */
  double* coeff_y_a;    /* Stores column COEFF_Y_A */
  double* coeff_y_b;    /* Stores column COEFF_Y_B */
  double* coeff_y_c;    /* Stores column COEFF_Y_C */
  double* coeff_y_d;    /* Stores column COEFF_Y_D */
  double* coeff_y_e;    /* Stores column COEFF_Y_E */
  double* coeffarrays[N_MULTISEG_COEFF]; /* Stores pointers for the same coeff arrays. */

  char winoffx_name[FLEN_VALUE];   /* Name of windowing offset keyword in x direction */
  char winoffy_name[FLEN_VALUE];   /* Name of windowing offset keyword in y direction */
  char winpropx_name[FLEN_VALUE];   /* Name of windowing property keyword in x direction */
  char winpropy_name[FLEN_VALUE];   /* Name of windowing property keyword in y direction */
  int winpropx_num;   /* Property number of windowing property keyword in x direction */
  int winpropy_num;   /* Property number of windowing property keyword in y direction */
  int use_multiple_winoffx;  /* 0 to use a single windowing x-offset event file keyword; 1 to use multiple ones */
  int use_multiple_winoffy;  /* 0 to use a single windowing y-offset event file keyword; 1 to use multiple ones */
  long min_winpropx; /* Minimum value of winpropx property column */
  long max_winpropx; /* Maximum value of winpropx property column */
  long min_winpropy; /* Minimum value of winpropy property column */
  long max_winpropy; /* Maximum value of winpropy property column */
		    
} TR_MULTISEG;



/* Create a new TR_MULTISEG structure and read its contents from a TelDef file. */
int readTrMultiseg /* Returns CFITSIO status */
(
 fitsfile* fp, /* Teldef file pointer */
 TR_MULTISEG** p_multisegparam, /* Pointer to TR_MULTISEG structure pointer */
 char* lowcoordsysname, /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys,  /* Number of orig. coord. sys. */
 char* filename /* Name of TelDef file */
 );

/* Print a TR_MULTISEG structure to a stream. */
void printTrMultiseg
(
 TR_MULTISEG* multisegparam, /* TR_MULTISEG structure to print */
 FILE* stream /* destination stream */
 );


/* Destroy a TR_MULTISEG structure. */
void destroyTrMultiseg
(
 TR_MULTISEG* multisegparam /* TR_MULTISEG structure to destroy. */
 );
 
#define TR_MULTISEG_INCLUDED
#endif  /* TR_MULTISEG_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif
