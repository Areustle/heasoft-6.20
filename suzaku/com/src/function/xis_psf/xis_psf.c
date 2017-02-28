#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "xis_psf.h"

static char pname[] = "xis_psf-1.6";

#define EPSILON  0.00001      /* "Zero":  For use as a 'safe comparison' between floats */

typedef struct {

  double *psf;
  int data_type;
  /* As per the note above: This is (currently) redundant. */
  /* The psf data type is double. So the value for this    */
  /* parameter will always be / had better be TDOUBLE.     */
  /* In future, this may change, so we'll leave it here.   */

  long naxes[2];  /* The psf dimensions */

  int cent_x, cent_y;
  /* The pixel values (as doubles) of  */
  /* the center of the psf. These will */
  /* be stored in the CRPIX keywords.  */

  size_t nbytes;

  int hdu_num;

  /* This is for book-keeping / error checking purposes */
  char *psf_file;

} _Psf;

typedef _Psf *Psf;

/** Here we defined some requirements/assumptions for the PSF extension **/
#define psf_extension_name       "PSF"

#define OFFAXIS_COL_NAME         "OFFAXIS"
#define AZIMUTH_COL_NAME         "AZIMUTH"

static long _NAXIS1 = FASTARF_PSF_NAXIS1; /* These give the x & y dimensions */
static long _NAXIS2 = FASTARF_PSF_NAXIS2; /* of the weighted_psf.            */

#define N_WEIGHTS     4     /** Each weighting/interpolation will involve no more than four functions/points **/

static double weights[N_WEIGHTS];
static Psf psfs[N_WEIGHTS];
static int fits_inds[N_WEIGHTS];

static char *caldb_file = NULL;

static Psf weighted_psf = NULL;

static const double normalization_radius = 6.0;  /** mm **/
static const double pixel_size = 0.024;          /** mm **/

/***  Global variables begin here ***/

static fitsfile *fptr=NULL;
static char *fits_caldb_file=NULL;

static const int psf_bintable_hdu=2;

static double *offaxis =NULL;
static double *azimuths=NULL;

static int n_off_az=0;

typedef struct {
  int N_UNIQUE;
  double *values;
} GridValues;

static GridValues *offaxisGrid=NULL;
static GridValues *azimuthGrid=NULL;

/**********************************************************************/

static int _Fits_closefile ( fitsfile *fp );
static fitsfile *_Fits_openfile ( const char *filename );

/***  Global variables end here ***/

/**********************************************************************/
/*****  Generic routines: These may be used for any fitsfile **********/
/**********************************************************************/

static int
_Fits_closefile ( fitsfile *fp )
{
  fitsfile *_fp;
  int status=0;

  _fp=fp;

  if ( _fp==NULL )
    return 0;

  fits_close_file ( _fp, &status );

  if ( status ) {
    fprintf(stderr, "\n\
%s: Error closing fits file.\n\n", pname);
    return -1;
  }

  return 0;
}

static fitsfile *
_Fits_openfile ( const char *filename )
{
  fitsfile *_fptr=NULL;
  int status=0;
  const char *_filename=filename;

#define _READ_ONLY_  0

  if ( _filename == NULL ) {
    fprintf(stderr, "\n\
%s: Empty/null filename passed to _Fits_openfile().\n\n", pname);
    return NULL;
  }

  fits_open_file (&_fptr,  _filename, _READ_ONLY_, &status );

  if ( status ) {
    _fptr=NULL;
  }

  return _fptr;
}

static void *
_allocate ( void *ptr, size_t bytes )
{
  void *_ptr=ptr;

  if (bytes <= 0)
    return NULL;

  if ( _ptr != NULL )
    return realloc ( _ptr, bytes );

  /** The remainder of this function is implicitly an 'else' clause to the 'if ( _ptr != NULL)' block **/
  _ptr= malloc ( bytes );

  if ( _ptr != NULL )
    memset ( _ptr, 0, bytes );

  return _ptr;
}

/**********************************************************************/
/*****  End, generic routines  ****************************************/
/**********************************************************************/

static int
_init_mem_new_psf (Psf psf)
{
  size_t bytes_per_pix, bytes;

  if ( psf==NULL ) {
    fprintf(stderr, "\n\
%s: Null psf while trying to initialize psf.\n\n", pname);
    return -1;
  }

  if ( (psf->naxes[0] <= 0)  ||  (psf->naxes[1] <= 0) ) {
    fprintf(stderr, "\n\
%s: Non-sensical array size value(s) while initializing psf. (naxes[0]=%ld, naxes[1]=%ld.\n\n", pname,
	     psf->naxes[0], psf->naxes[1]);

    return -1;
  }

  bytes_per_pix=0;

  /***************************************************************/
  /*** As of now (October, 2009), the psf's are stored as fits ***/
  /*** images with BITPIX=-64 -- double precision. If this     ***/
  /*** changes in the future, the code here must me modified!  ***/
  /***************************************************************/
  bytes_per_pix= sizeof ( double );

  bytes= psf->naxes[0] * psf->naxes[1] * bytes_per_pix;

  if (bytes==0) {
    fprintf(stderr, "\n\
%s: Error trying to initialize space for psf.\n\n", pname);
    return-1;
  }

  psf->psf= _allocate ( NULL, bytes );

  if (psf->psf == NULL) {
    fprintf(stderr, "\n\
%s: Error initializing psf datatype: Malloc error.\n\n", pname);
    return -1;
  }
  psf->nbytes= bytes;

  return 0;
}

static int
_move_to_image_hdu ( int hdu_num )
{
  int status=0;
  int hdu_type=0;

  if (fptr == NULL ) {
    fprintf(stderr, "\n\
%s: Empty/null fitsfile pointer while in _move_to_image_hdu()\n\n", pname);
    return -1;
  }

  if (hdu_num < 1) {
    fprintf(stderr, "\n\
%s: Non-sensical hdu number while trying to move to request hdu. (hdunum=%d)\n\n", pname, hdu_num);
    return -1;
  }

  fits_movabs_hdu (fptr, hdu_num, &hdu_type, &status);
  if (status) {
    fprintf(stderr, "\n\
%s: Error moving to header #%d\n\n", pname, hdu_num);
    return -1;
  }
  if (hdu_type != IMAGE_HDU) {
    fprintf(stderr, "\n\
%s: HDU #%d does not contain an image.\n\n", pname, hdu_num);
    return -1;
  }

  return 0;
}

/*************************************************************/
/** This routine assumes that the fitsfile has already been **/
/** positioned to the desired header data unit (hdu).       **/
/*************************************************************/
static int
_initialize_psf_struct ( int hdu_num, Psf psf )
{
  Psf _psf=psf;

  int naxis, bitpix;
  int status=0;

  double cval_x_temp;
  double cval_y_temp;

  /*************************************************/
  /** These first three checks represent cases    **/
  /** which should never happen. In other words,  **/
  /** they represent programming errors, not data **/
  /** errors or user errors.                      **/
  /*************************************************/
  if (fptr == NULL) {
    fprintf(stderr, "\n\
%s: Unopened fits file in _initialize_psf_struct().\n\n", pname);
    return -1;
  }
  if (psf == NULL) {
    fprintf(stderr, "\n\
%s: Null psf pointer passed in _initialize_psf_struct().\n\n", pname);
    return -1;
  }
  if (fits_caldb_file == NULL) {
    fprintf(stderr, "\n\
%s: Null fits_file pointer passed to _initialize_psf_struct().\n\n", pname);
    return -1;
  }

  /********************************************/

  if ( hdu_num < 0 ) {
    fprintf(stderr, "\n\
%s: Non-sensical header data unit number while trying to initialized psf structure. (hdunum=%d)\n\n", pname, hdu_num);
    return -1;
  }

  _psf->psf_file= _allocate (NULL, strlen (fits_caldb_file) + 1);

  if (_psf->psf_file == NULL)  {
    fprintf(stderr, "\n\
%s: Memory allocation error while initialzing psf structure.\n\n", pname);
    return -1;
  }
  strcpy (_psf->psf_file, fits_caldb_file);
  _psf->hdu_num=hdu_num;

  /** Now read the header for the given hdu. Again: it is assumed **/
  /** that the fitsfile *fp is already positioned in this hdu.    **/
  fits_read_imghdr (fptr, 2, NULL, &bitpix, &naxis, _psf->naxes, NULL, NULL, NULL, &status);
  if (status) {
    fprintf(stderr, "\n\
%s: fitsio error while attempting to read image header for hdu# %d\n\n",
	    pname, hdu_num);
    return -1;
  }
  _psf->data_type=TDOUBLE;

  fits_read_key_dbl (fptr, "CRPIX1", &cval_x_temp, NULL, &status);
  fits_read_key_dbl (fptr, "CRPIX2", &cval_y_temp, NULL, &status);

  _psf->cent_x= (int) ( cval_x_temp ) - 1; /** CRPIXn gives the center of the image in fractional pixels.   **/
  _psf->cent_y= (int) ( cval_y_temp ) - 1; /** Also, the pixel coordinates start from 1, our arrays from 0. **/

  if (status) {
    fprintf(stderr, "\n\
%s: Error reading CRPIX1/CRPIX2 keywords: Psf centroid cannot be found!\n\n",
	    pname);
    return -1;
  }

  if (-1 == _init_mem_new_psf (_psf))
    return -1;

  return 0;
}

static int
_element_exists  ( double val, double *array, int n_els )
{
  int k;

  for ( k=0; k < n_els; k++ ) {
    if ( val == array [k] )
      return 1;
  }
  return 0;
}

static int
_fill_grid_values ( void )
{
  int N, k;
  int maxvals=16;

  offaxisGrid= _allocate ( NULL, sizeof (GridValues ) );
  azimuthGrid= _allocate ( NULL, sizeof (GridValues ) );

  if (offaxisGrid == NULL  ||  azimuthGrid == NULL ) {
    fprintf(stderr, "\n\
%s: Malloc error while trying to initialize grid axes.\n\n", pname);
    return -1;
  }

  offaxisGrid->values= _allocate ( NULL, maxvals * sizeof ( double ) );
  azimuthGrid->values= _allocate ( NULL, maxvals * sizeof ( double ) );

  if ( offaxisGrid->values == NULL  || azimuthGrid->values == NULL) {
    fprintf(stderr, "\n\
%s: Malloc error while trying to initialize grid axes.\n\n", pname);
    return -1;
  }

  for (N=0, k=0; k < n_off_az; k++) {
    if ( _element_exists ( offaxis[k], offaxisGrid->values, N ) )
      continue;

    if ( N == maxvals ) {
      maxvals *= 2;
      offaxisGrid->values= _allocate ( offaxisGrid->values, maxvals * sizeof ( double ) );
      if (offaxisGrid->values == NULL) {
	fprintf(stderr, "\n\
%s: Malloc error while trying to initialize grid axes.\n\n", pname);
	return -1;
      }
    }
    offaxisGrid->values[N++]=offaxis[k];
  } /** End, for (k=0...) block **/
  offaxisGrid->N_UNIQUE=N;

  maxvals=16;
  for (N=0, k=0; k < n_off_az; k++) {
    if ( _element_exists ( azimuths[k], azimuthGrid->values, N ) )
      continue;

    if ( N == maxvals ) {
      maxvals *= 2;
      azimuthGrid->values= _allocate ( azimuthGrid->values, maxvals * sizeof ( double ) );
      if (azimuthGrid->values == NULL) {
	fprintf(stderr, "\n\
%s: Malloc error while trying to initialize grid axes.\n\n", pname);
	return -1;
      }
    } /** End, for (k=0...) block **/
    azimuthGrid->values[N++]=azimuths[k];
  }
  azimuthGrid->N_UNIQUE=N;

  return 0;
}

static int
_read_psf_bintable_extension ( void )
{
  int status=0;
  int hdu_type=0;

  int k;
  int off_col, az_col;

  long nrows=0;

  fits_movabs_hdu (fptr, psf_bintable_hdu, &hdu_type, &status);

  if (status) {
    fprintf(stderr, "\n\
%s: Error moving to header #%d\n\n", pname, psf_bintable_hdu);
    return -1;
  }
  if (hdu_type != BINARY_TBL ) {
    fprintf(stderr, "\n\
%s: Error: HDU #%d does is not a binary table.\n\n", pname, psf_bintable_hdu);
    return -1;
  }

  fits_get_num_rows ( fptr, &nrows, &status );

  if ( status ) {
    fprintf(stderr, "\n\
%s: Error reading the number of rows in the PSF extension.\n\n", pname);
    return -1;
  }

  offaxis  = _allocate ( NULL, nrows * sizeof ( double ) );
  azimuths = _allocate ( NULL, nrows * sizeof ( double ) );

  if ( offaxis == NULL  ||  azimuths == NULL ) {
    fprintf(stderr, "\n\
%s: Malloc/memory error while initializing offsets/azimuths.\n\n", pname);
    return -1;
  }

  fits_get_colnum ( fptr, CASEINSEN, OFFAXIS_COL_NAME, &off_col, &status);
  fits_get_colnum ( fptr, CASEINSEN, AZIMUTH_COL_NAME, &az_col, &status);

  for (k=0; k < nrows; k++) {
    fits_read_col_dbl(fptr, off_col,k+1,1,1, 0, &(offaxis[k]), NULL, &status);
    fits_read_col_dbl(fptr, az_col, k+1,1,1, 0, &(azimuths[k]),NULL, &status);
  }

  n_off_az=nrows;

  /******************************************************************/
  /** Fill in the GridValues for the offaxis & azimuth: These give **/
  /** the list of values along the offaxis/azimuth axes.           **/
  /******************************************************************/
  if (-1 == _fill_grid_values ()) {
    fprintf(stderr, "\n\
%s: Error: Failed to set up offset/azimuth axes.\n\n", pname);
    return -1;
  }

  return 0;
}

static int
_search_interp_low ( double val, GridValues *grid )
{
  int k;
  double *_array;

  if ( grid == NULL )
    return -1;

  k=grid->N_UNIQUE-1;

  _array=grid->values;

  if (_array == NULL )
    return -1;

  while ( val < _array[k]) k--;

  return k;
}

static void
normalize_offset_azimuth(double *offset, double *azimuth)
{
  if ( *offset < 0.0 ) {
    *offset = - (*offset);
    *azimuth += 180.0;
  }

  while ( *azimuth < 0.0 ) {
    *azimuth += 360.0;
  }

  while ( 360.0 <= *azimuth ) {
    *azimuth -= 360.0;
  }
}

static int
set_weights(double offset, double azimuth, int *fits_ind, double *weights)
{
  double off_hi, off_low, delta_offset, u;
  double az_hi, az_low, delta_azimuth, t;

  /*********************************************/
  /** Care must be taken here:  The fits_ind  **/
  /** array defines the order of the curves   **/
  /** as follows:                             **/
  /**   (offset low, azimuth low)             **/
  /**   (offset low, azimuth hi )             **/
  /**   (offset hi,  azimuth low)             **/
  /**   (offset hi,  azimuth hi)              **/
  /**                                         **/
  /** When calculating the delta's below,     **/
  /** we must be sure to use the appropriate  **/
  /** indices!!                               **/
  /*********************************************/

  normalize_offset_azimuth(&offset, &azimuth);

  off_low= offaxis [fits_ind [0] - 1];
  if ( -1 != fits_ind[2] ) {
    off_hi = offaxis [fits_ind [2]-1];
  } else if ( offset == off_low ) {
    off_hi = off_low;
  } else {
    fprintf(stderr, "\
%s: ERROR: offset > offset_max = %.3f\n", pname, off_low);
    return -1;
  }
  delta_offset = off_hi - off_low;

  if ( 1 == fits_ind[0] ) {	/* (0, 0) */
    if ( -1 != fits_ind[2] && -1 != fits_ind[3] ) {
      az_low = azimuths [fits_ind [2] - 1];
      az_hi = azimuths [fits_ind [3] - 1 ];
    } else {
      az_low = az_hi = 0.0;	/** Dummy value: We only have one curve to interpolate with **/
    }
  } else {
    if ( -1 != fits_ind[0] && -1 != fits_ind[1] ) {
      az_low = azimuths [fits_ind [0] - 1];
      az_hi = azimuths [fits_ind [1] - 1];
    } else {
      az_low = az_hi = 0.0;	/** Dummy value: We only have one curve to interpolate with **/
    }
  }
  delta_azimuth = az_hi - az_low;
  while ( delta_azimuth < 0 ) {
    delta_azimuth += 360.0;
  }

  /** u & t are calculated as is outline in "Numerical Recipes", 3rd Edition, p132-133. **/
  if ( delta_offset < EPSILON ) {
    u = 0.0;
  } else {
    u = ( offset - off_low ) / delta_offset;
  }
  if ( delta_azimuth < EPSILON ) {
    t = 0.0;
  } else {
    t= ( azimuth - az_low ) / delta_azimuth;
  }

  printf("\
offset=%.1lf, azimuth=%.1lf\n\
off_low=%.3lf, off_hi=%.3lf, delta_offset=%.3f, u=%.6lf\n\
az_low=%.1lf, az_hi=%.1lf, delta_azimuth=%.1f, t=%.6lf\n",
	 offset, azimuth,
	 off_low, off_hi, delta_offset, u,
	 az_low, az_hi, delta_azimuth, t);

  if ( u > 1.0 || t > 1.0 ) {
    fprintf(stderr, "\n\
%s: Numerical error while determining weights: Weight > 1.0 found! u=%.1f, t=%.1f \n\n", pname, u, t);
    return -1;
  }

  weights[0] = (1.0 - t) * (1.0 - u);
  weights[1] = t * (1.0 - u);
  weights[2] = ( 1.0 - t ) * u;
  weights[3] = t * u;

  if ( -1 == fits_ind[0] ) {
    return -1;
  }

  if ( -1 == fits_ind[1] ) {
    weights[0] += weights[1];
    weights[1] = 0.0;
  }

  if ( -1 == fits_ind[3] ) {
    weights[2] += weights[3];
    weights[3] = 0.0;
  }

  if ( -1 == fits_ind[2] ) {
    weights[0] += weights[2];
    weights[2] = 0.0;
  }

  return 0;
}


/**************************************************************************
  The services offered by this library are contained in the functions
  below here
**************************************************************************/

static int
init_psf_fits ( char *fits_file )
{
  if ( fits_file == NULL ) {
    fprintf(stderr, "\n\
%s: ERROR: Empty/null fits file while trying to initialized psf fits library.\n\n", pname);
    return -1;
  }
  if ( fptr != NULL ) {
    fprintf(stderr, "\n\
%s: ERROR: Attempting to re-initialize psf fits library.\n\n", pname);
    return -1;
  }

/**************************************************************************
  It is assumed that, if fptr==NULL, the library is uninitialized. We don't
  particularly care about the the status of caldb_file. We can write over it.
**************************************************************************/
  if ( fits_caldb_file != NULL ) free ( (void *) fits_caldb_file );

  fits_caldb_file= malloc ( strlen (fits_file) + 1);
  if ( fits_caldb_file == NULL ) {
    fprintf(stderr, "\n\
%s: Malloc error while trying to initialized psf fits library.\n\n", pname);
    return -1;
  }

  strcpy (fits_caldb_file, fits_file);

  fptr= _Fits_openfile ( (const char *) fits_caldb_file );

  if ( fptr == NULL ) {
    fprintf(stderr, "\n\
%s: Error opening fits file (file='%s')\n\n", pname, fits_caldb_file );
    return -1;
  }

  /** Read the AZIMUTH & OFFSET columns in the bintable extension (named "PSF") **/
  if (-1 == _read_psf_bintable_extension () )
    return -1;

  return 0;
}

static void
psf_fits_cleanup ( void )
{
  if ( fits_caldb_file != NULL )
    free ( (void *) fits_caldb_file );

  if ( fptr != NULL )
    _Fits_closefile ( fptr );

  fptr=NULL;
  fits_caldb_file=NULL;

  free ( (void *) offaxis );
  free ( (void *) azimuths );

  free ( (void *) offaxisGrid->values );
  free ( (void *) offaxisGrid );

  free ( (void *) azimuthGrid->values );
  free ( (void *) azimuthGrid );

  return;
}

static void
psf_cleanup ( Psf _psf )
{
  if ( _psf == NULL )
    return;

  free ( (void *) _psf->psf_file);
  free ( (void *) _psf->psf);

  return;
}

static Psf
read_fits_hdu_psf ( int hdu_num )
{
  Psf _psf;

  int status=0;
  long nelements;

  _psf= _allocate (NULL, sizeof (_Psf));

  if (_psf == NULL ) {
    fprintf(stderr, "\n\
%s: Failed to properly allocate memory for Psf type.\n\n", pname);
    return NULL;
  }

  if (fptr==NULL) {
    fprintf(stderr, "\n\
%s: Error: psf fits library uninitialized in read_fits_hdu_psf()\n\n", pname);
    return NULL;
  }

  if (hdu_num < 0) {
    fprintf(stderr, "\n\
%s: Non-sensical hdu number while trying to read psf. (hdu #=%d)\n\n",
	    pname, hdu_num);
    return NULL;
  }

  if (-1 == _move_to_image_hdu ( hdu_num )) {
    fprintf(stderr, "\n\
%s: Error moving to hdu number %d\n\n", pname, hdu_num);
    return NULL;
  }
  if (-1 ==  _initialize_psf_struct ( hdu_num, _psf ))
    return NULL;

  /*** The data structure has now been setup (space allocated, etc.).
       Read the image ***/
  nelements= _psf->naxes[0] * _psf->naxes[1];

  fits_read_img (fptr, _psf->data_type, 1, nelements, NULL, _psf->psf, NULL, &status);

  if (status) {
    fprintf(stderr, "\n\
%s: Failed to read image in extension %d.\n\n", pname, hdu_num);
    return NULL;
  }

  return _psf;
}

static int
_determine_fits_indices(double offset, double azimuth, int fits_inds[])
{
  int N_AZ, N_OF;
  int off_index, az_index;

  int row1, row2;
  int row3, row4;

  normalize_offset_azimuth(&offset, &azimuth);

  row1 = row2 = row3 = row4 = 999999;	/** Initialize to a dummy value **/

  /** Initialize the values in the return array **/
  fits_inds[0] = fits_inds[1] = fits_inds[2] = fits_inds[3] = -1;

  N_OF = offaxisGrid->N_UNIQUE;
  N_AZ = azimuthGrid->N_UNIQUE;

  /** First special case: If the offset is 0.0, no interpolation  **/
  /** is needed. Return the index to the first row in the ae file **/
  if ( offset < EPSILON ) {
    fits_inds[0]= 1;
    return 0;
  }

  off_index=  _search_interp_low ( offset,  offaxisGrid );
  az_index =  _search_interp_low ( azimuth, azimuthGrid );

  if ( -1 == off_index || -1 == az_index ) {
    return -1;
  }

  if ( 0 == off_index ) {		/* (0, 0) */
    row1 =  1;
    row2 = -1;
  } else {
    row1 = N_AZ * ( off_index - 1 ) + az_index + 2;
    if ( az_index == (N_AZ-1) ) { /* consider cyclic condition of azimuth */
      row2 = row1 - (N_AZ-1);
    } else {
      row2 = row1 + 1;
    }
  }

  off_index++;
  if ( off_index < N_OF ) {
    row3 = N_AZ * ( off_index - 1 ) + az_index + 2;
    if ( az_index == (N_AZ-1) ) { /* consider cyclic condition of azimuth */
      row4 = row3 - (N_AZ-1);
    } else {
      row4 = row3 + 1;
    }
  } else {
    row3 = row4 = -1;
  }

  fits_inds[0] = row1;
  fits_inds[1] = row2;
  fits_inds[2] = row3;
  fits_inds[3] = row4;

  return 0;
}

static int
dump_psf_to_file ( Psf psf, char *outfile )
{

  fitsfile *fp_out;

  int naxis=2;
  int bitpix=DOUBLE_IMG;

  long naxes[2], nels;

  double zero=0.0;
  double en=4.51;
  double crpix_x, crpix_y;
  double cdelt=.024;

  int status=0;

  if ( psf==NULL ) {
    fprintf(stderr, "\n\
%s: Null/empty psf passed to fits dump routine.\n\n", pname);
    return -1;
  }

  naxes[0]= psf->naxes[0];
  naxes[1]= psf->naxes[1];

  fits_create_file (&fp_out, outfile, &status);

  if (status) {
    fprintf(stderr, "\n\
%s: Failed to create fits file.\n\n", pname);
    return 0;
  }
  fits_create_img (fp_out, bitpix, naxis, naxes, &status);
  if (status) {
    fprintf(stderr, "\n\
%s: Failed to create image.\n\n", pname);
    return 0;
  }
  nels= naxes[0] * naxes[1];

  crpix_x= psf->cent_x;
  crpix_y= psf->cent_y;

  fits_write_key (fp_out, TDOUBLE, "OFFAXIS", &zero, NULL, &status);
  fits_write_key (fp_out, TDOUBLE, "AZIMUTH", &zero, NULL, &status);
  fits_write_key (fp_out, TDOUBLE, "ENERGY",  &en, NULL, &status);
  fits_write_key (fp_out, TSTRING, "CTYPE1",  "LINEAR  ", NULL, &status);
  fits_write_key (fp_out, TDOUBLE, "CRPIX1",  &crpix_x, NULL, &status);
  fits_write_key (fp_out, TDOUBLE, "CRVAL1",  &zero, NULL, &status);
  fits_write_key (fp_out, TDOUBLE, "CDELT1",  &cdelt, NULL, &status);
  fits_write_key (fp_out, TSTRING, "CUNIT1",  "mm      ", NULL, &status);
  fits_write_key (fp_out, TSTRING, "CTYPE2",  "LINEAR  ", NULL, &status);
  fits_write_key (fp_out, TDOUBLE, "CRPIX2",  &crpix_y, NULL, &status);
  fits_write_key (fp_out, TDOUBLE, "CRVAL2",  &zero, NULL, &status);
  fits_write_key (fp_out, TSTRING, "CUNIT2",  "mm      ", NULL, &status);

  fits_write_img (fp_out, TDOUBLE, 1, nels, psf->psf, &status);
  if (status) {
    fprintf(stderr, "\n\
%s: Failed to write image.\n\n", pname);
    return 0;
  }
  fits_close_file (fp_out, &status);

  if (status)
    fprintf(stderr, "\n\
%s: Error closing the file.\n\n", pname);

  return 0;
}

static int
_return_off_az ( int hdu, double *off, double *az )
{
  *off= offaxis [hdu-3];
  *az = azimuths  [hdu-3];

  return 0;
}

/****************************  Begin, generic psf routines  *****************************/

/*************************************************************************/
/**    The psf's are two-dimensional, but are stored in one-dimensional **/
/** arrays. The pixel_number routine is used to determine the array     **/
/** index ("pixel number") into this one-dimensional array, given an x  **/
/** and a y pixel pair of pixel values. It must be noted here that that **/
/** cfitsio stores (2-d) image arrays such that counting is done along  **/
/** the y axis first, and then the x.                                   **/
/*************************************************************************/

static long
_pixel_number ( Psf psf, int pixel_x, int pixel_y, int *status )
{

  *status=0;

  if ( psf == NULL ) {
    fprintf(stderr, "\n\
%s: Empty/null psf passed to pixel_number()\n\n", pname);
    *status=-1;
    return -1;
  }

  if (pixel_x < 0 || pixel_y < 0) {
    fprintf(stderr, "\n\
%s: Non-sensical pixel value(s) passed to pixel_number(): pixel_x=%d, pixel_y=%d\n\n",
	    pname, pixel_x, pixel_y);
    *status=-1;
    return -1;
  }

  /** Attempting to access beyond the bounds of the image is **/
  /** considered to be an error.                             **/
  if ( pixel_x >= psf->naxes[0] || pixel_y >= psf->naxes[1] )
    return -1;

  /** See the comment in fastarf_psf.h: Fits images  **/
  /** are store in [y][x] format, not [x][y] format. **/
  return  ( psf->naxes[0] * pixel_y + pixel_x );
}

static double
_get_psf_counts ( Psf psf, int pixel_x, int pixel_y, int *status )
{

  long pixel;
  long npixels_psf;

  *status=0;

  if ( psf == NULL ) {
    fprintf(stderr, "\n\
%s: Empty/null psf passed to psf_counts()\n\n", pname);
    *status=-1;
    return -1;
  }

  /** Do not treat attempts to read beyond the array size as an error. Just return 0. **/
  if (pixel_x < 0 || pixel_y < 0)
    return 0;

  if ( pixel_x >= psf->naxes[0]  ||  pixel_y >= psf->naxes[1] )
    return 0;

  pixel= _pixel_number ( psf, pixel_x, pixel_y, status );

  if ( *status == -1 )
    return -1;

  /** Calculate the total number of array elements in the psf array **/
  npixels_psf= ( psf->nbytes / sizeof (psf->psf[0]) );

  if ( pixel >= npixels_psf  || pixel < 0 )
    return 0;

  return psf->psf[ pixel ];

}

static int
_set_psf_counts ( Psf psf, int pixel_x, int pixel_y, double value )
{
  long pixel;
  int status=0;

  if ( psf == NULL ||  psf->psf == NULL ) {
    fprintf(stderr, "\n\
%s: ERROR: Null/empty psf passed to set_psf_counts()\n\n", pname);
    return -1;
  }


  /** Attempts to access elements beyond the x/y axes are ignored **/
  if ( pixel_x < 0  ||  pixel_y < 0 )
    return 0;

  if ( pixel_x >= psf->naxes[0]  ||  pixel_y >= psf->naxes[1] )
    return 0;

  pixel= _pixel_number ( psf, pixel_x, pixel_y, &status );
  if ( status == -1 )
    return 0;

  psf->psf[pixel]=value;

  return 0;
}

static double
_normalize_psf ( Psf psf_to_normalize, int *status )
{
  Psf _psf= psf_to_normalize;

  double counts=0.0;

  long x, y;
  long j, k;

  const long npixels_norm= (long) ( normalization_radius / pixel_size );
  const long r_squared= (long) ( ( normalization_radius / pixel_size ) * ( normalization_radius / pixel_size ) );

  if ( _psf == NULL ) {
    fprintf(stderr, "\n\
%s: ERROR: Empty/null psf passed to psf normalization routine.\n\n", pname);
    *status=-1;
    return -1.0;
  }

  *status=0;

  for (x=_psf->cent_x, j=0; j<npixels_norm; x++, j++) {
    for (y=_psf->cent_y, k=0; k<npixels_norm; y++, k++) {

      long x_conjugate= x - j - j;
      long y_conjugate= y - k - k;

      if ( (j*j + k*k) > r_squared)
	continue;

      counts += _get_psf_counts ( _psf, x, y, status );

      if ( *status == -1 )
	return -1;

      if ( j == 0 && k == 0 )
	continue;

      if ( j == 0 ) {
	counts += _get_psf_counts ( _psf, x, y_conjugate, status );

	if ( *status == -1 )
	  return -1;

	continue;
      }

      if ( k == 0 ) {
	counts += _get_psf_counts ( _psf, x_conjugate, y, status );

	if ( *status == -1 )
	  return -1;

	continue;
      }

      /** The remainder of this loop is an implied "...else ( j != 0 && k != 0 )" ***/
      counts += _get_psf_counts ( _psf, x, y_conjugate, status );
      counts += _get_psf_counts ( _psf, x_conjugate, y, status );
      counts += _get_psf_counts ( _psf, x_conjugate, y_conjugate, status );


      if ( *status == -1 )
	return -1;

    }
  }

  return counts;
}

static int
_add_to_pixel ( Psf psf_to_copy, long x, long y, long x_copy, long y_copy, double weight)
{
  double to_add;
  int status=0;

  to_add = weight * _get_psf_counts ( psf_to_copy, x_copy, y_copy, &status);

  to_add  += _get_psf_counts ( weighted_psf, x, y, &status );

  if ( status == -1 )
    return -1;

  if (-1 == _set_psf_counts ( weighted_psf, x, y, to_add ))
    return -1;

  /** So as to avoid double counting, set the caldb_psf[pixel] to 0 **/
  _set_psf_counts ( psf_to_copy, x_copy, y_copy, 0 );

  return 0;

#undef EPSILON
}

/** weighted_psf is the psf to which this routine will add the caldb_psf, weighted by weight **/
static int
_add_psf ( Psf caldb_psf, double weight )
{
#define EPSILON    0.00001

  long j, k;
  long x, y;

  double psf_norm_counts=0.0;

  int weighted_x_cent, weighted_y_cent;
  int caldb_x_cent, caldb_y_cent;

  int status=0;

  if ( weight < EPSILON )
    return 0;

  if ( weighted_psf == NULL   ||   caldb_psf == NULL ) {
    fprintf(stderr, "\n\
%s: ERROR: Empty/uninitialized psf passed to _add_psf() routine.\n\n", pname);
    return -1;
  }
  if ( weighted_psf->psf == NULL ||  caldb_psf->psf == NULL ) {
    fprintf(stderr, "\n\
%s: ERROR: Empty/uninitialized psf passed to _add_psf() routine.\n\n", pname);
    return -1;
  }

  weighted_x_cent= weighted_psf->cent_x;
  weighted_y_cent= weighted_psf->cent_y;

  caldb_x_cent= caldb_psf->cent_x;
  caldb_y_cent= caldb_psf->cent_y;

  psf_norm_counts = _normalize_psf ( caldb_psf, &status );
  if ( status == -1 )
    return -1;

  if ( psf_norm_counts < EPSILON ) {
    fprintf(stderr, "\n\
%s: Found normalization=0.0 while trying to merge psfs!\n\n", pname);
    return -1;
  }

  weight = weight / psf_norm_counts;

  for (x=weighted_x_cent, j=0; j<caldb_psf->naxes[0] / 2; x++, j++) {
    for (y=weighted_y_cent, k=0; k<caldb_psf->naxes[1] / 2; y++, k++) {

      long x_copy=caldb_x_cent + j;
      long y_copy=caldb_y_cent + k;

      long x_conjugate, x_copy_conjugate;
      long y_conjugate, y_copy_conjugate;

      /**** Define the conjugate coordinates ****/
      x_conjugate= weighted_x_cent - j;
      y_conjugate= weighted_y_cent - k;

      x_copy_conjugate= caldb_x_cent - j;
      y_copy_conjugate= caldb_y_cent - k;

      /**** **** **** **** **** **** **** ** ****/

      /** First point: (x, y)  **/

      if ( -1 == _add_to_pixel ( caldb_psf, x, y, x_copy, y_copy, weight))
	return -1;

      /** Second point: (-x, y)  **/
      if ( -1 == _add_to_pixel ( caldb_psf, x_conjugate, y, x_copy_conjugate, y_copy, weight))
	return -1;

      /** Third point: (-x, -y)  **/

      if ( -1 == _add_to_pixel ( caldb_psf, x_conjugate, y_conjugate, x_copy_conjugate, y_copy_conjugate, weight))
	return -1;

      /** Fourth point: (x, -y)  **/

      if ( -1 == _add_to_pixel ( caldb_psf, x, y_conjugate, x_copy, y_copy_conjugate, weight))
	return -1;

      /**** **** **** **** **** **** **** ** ****/

    } /** End, for (y=weighted_y_cent, k=0...) block **/
  } /** End, for (x=weighted_x_cent, j=0...) block **/

  return 0;

#undef EPSILON
}

/**********************************************************************/
/*****  End, generic psf routines  ************************************/
/**********************************************************************/

/**************************************************************************
  The services offered by this library are contained in the functions
  below here
**************************************************************************/

void
xis_psf_cleanup ( void )
{
  int k;

  psf_fits_cleanup ( );

  for (k=0; k<N_WEIGHTS; k++) {
    if ( psfs[k] ) {
      psf_cleanup ( psfs[k] );
    }
    psfs[k] = NULL;
    weights[k] = 0.0;
  }

  free(caldb_file);
  caldb_file = NULL;

  psf_cleanup(weighted_psf);
  free (weighted_psf);

  return;
}

int
xis_psf_init ( char *psf_file )
{
  int k;
  size_t nbytes;

  if ( psf_file == NULL ) {
    fprintf(stderr, "\n\
%s: Error initializing psf library: Empty/null fits filename.\n\n", pname);
    return -1;
  }
  if ( caldb_file != NULL ) {
    fprintf(stderr, "\n\
%s: WARNING: caldb file has apparently already been set.\n\
   Will reset to '%s'\n\n", pname, psf_file);
    xis_psf_cleanup ();
  }

  for (k=0; k< N_WEIGHTS; k++) {
    if ( psfs[k] != NULL ) {
      psf_cleanup ( psfs[k] );
    }
    psfs[k] = NULL;
    weights[k] = 0.0;
  }
  caldb_file= malloc(strlen(psf_file) + 1);
  if ( caldb_file == NULL ) {
    fprintf(stderr, "\n\
%s: Malloc error while initializing psf library.\n\n", pname);
    return -1;
  }
  strcpy (caldb_file, psf_file );

  /** Initialize the psf fits library **/
  if ( -1 == init_psf_fits(caldb_file) ) {
    fprintf(stderr, "\n\
%s: Failed to initialize psf fits library.\n\n", pname);
    return -1;
  }

  /** Now initialize the weighted_psf global variable **/
  weighted_psf = malloc ( sizeof ( _Psf ) );
  if ( weighted_psf == NULL ) {
    fprintf(stderr, "\n\
%s: Malloc error while initializing psf library!\n\n", pname);
    return -1;
  }

  weighted_psf->naxes[0]=_NAXIS1;  weighted_psf->naxes[1]=_NAXIS2;

  nbytes= _NAXIS1 * _NAXIS2 * sizeof ( double );
  weighted_psf->psf= malloc ( nbytes );
  if  ( weighted_psf->psf == NULL ) {
    fprintf(stderr, "\n\
%s: Malloc error while initializing psf library!\n\n", pname);
    return -1;
  }
  memset (weighted_psf->psf, 0, nbytes);

  weighted_psf->nbytes= _NAXIS1 * _NAXIS2 * sizeof ( double );
  weighted_psf->hdu_num=-1;

  weighted_psf->psf_file=NULL;

  weighted_psf->cent_x = weighted_psf->cent_y = -1;

  return 0;
}

int
xis_psf( double offset, double azimuth, double **_psf )
{
  int k;
  size_t nbytes;
  double off, az;

  normalize_offset_azimuth(&offset, &azimuth);

  for (k=0; k<N_WEIGHTS; k++) {
    psfs[k] = NULL;
    weights[k] = 0.0;
  }

  nbytes= _NAXIS1 * _NAXIS2 * sizeof ( double );
  memset (weighted_psf->psf, 0, nbytes);

  if (-1 == _determine_fits_indices( offset, azimuth, fits_inds ) ) {
    fprintf(stderr, "\n\
%s: Failed to determine appropriate fits hdus.\n\n", pname);
    return -1;
  }
  if (-1 == set_weights ( offset, azimuth, fits_inds, weights ) ) {
    fprintf(stderr, "\n\
%s: Failed to determine appropriate weights.\n\n", pname);
    return -1;
  }

  weighted_psf->cent_x= ( _NAXIS1 - 1 ) / 2;
  weighted_psf->cent_y= ( _NAXIS2 - 1 ) / 2;

  for (k=0; k<N_WEIGHTS; k++) {
    if (fits_inds[k] == -1) {
      psfs[k]=NULL;
      continue;
    }
    psfs[k]= read_fits_hdu_psf ( fits_inds[k] + 2 );

    if ( -1 == _add_psf ( psfs[k], weights[k] ) ) {
      fprintf(stderr, "\n\
%s: ERROR: Failed to add psf to merged psf.\n\n", pname);
      return -1;
    }
    psf_cleanup ( psfs[k] );
    free(psfs[k]);
    psfs[k] = NULL;

    /** Scaffolding / testing purposes only **/
    _return_off_az (fits_inds[k]+2, &off, &az);
    printf("\
Ext %2d: ( %5.2f , %5.1f ) weight = %f\n",
	    fits_inds[k]+1, off, az,  weights[k]);
  }

  if ( _psf != NULL ) {
    *_psf= weighted_psf->psf;
  }

  return 0;
}

int
xis_psf_write_fits (double offset, double azimuth, char *outputFile )
{
  if ( outputFile == NULL ) {
    fprintf(stderr, "\n\
%s: Empty filename passed to xis_psf_write_fits()\n\n", pname);
    return -1;
  }

  if (-1 == xis_psf ( offset, azimuth, NULL )) {
    fprintf(stderr, "\n\
%s: xrt_xrtpsf() failed.\n\n", pname);
    return -1;
  }

  printf("\n\
Dumping psf to file '%s'...\n\n", outputFile );

  return dump_psf_to_file ( weighted_psf, outputFile );
}

/* for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
