#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "fitsio.h"
#include "xis_effarea.h"

static char pname[] = "xis_eff-1.6";

#define EPSILON  0.00001      /* "Zero":  For use as a 'safe comparison' between floats */

typedef struct {
  double **energy_grids;
  double **ea_curves;

  int n_elements_per_row;   /* This applies to energy_grids & ea_curves only */

  double *offaxis;
  double *azimuth;

  int n_rows;

  char *effAreaFile;

} _EffectiveArea_Fits;

typedef _EffectiveArea_Fits *EffectiveArea_Fits;

/******** Constants defined here *******/

#define ENERGY_COL_NAME           "ENERGY"
#define EA_CURVE_COL_NAME         "EFFAREA"
#define OFFSET_COL_NAME           "OFFAXIS"
#define AZIMUTH_COL_NAME          "AZIMUTH"
#define ea_extension_name         "EFFAREA"

/*** Library globals begin here ***/

static EffectiveArea_Fits caldb_ea;
static int ea_library_initialized = 0;
static char *ea_caldb_file = NULL;

/*** End, library globals ***/

static void *
_allocate ( void *ptr, size_t n_bytes )
{
  void *_p;

  if ( ptr != NULL )
    _p= realloc ( ptr, n_bytes );
  else
    _p= malloc ( n_bytes );

  /** If ptr != NULL, we have reallocated memory. **/
  /** Do not "wipe out" this previous memory!     **/

  if ( _p && ptr==NULL )
    memset (_p, 0, n_bytes);

  if ( _p == NULL )
    fprintf(stderr, "\n\
%s: Memory allocation failure!\n\n", pname);

  return _p;
}

static int
_copy_string ( char **dest_string,  char *src_string )
{
  char *_dest;

  if ( *dest_string != NULL )
    return 0;

  if ( src_string == NULL )
    return 0;

  _dest= *dest_string;

  _dest= _allocate ( NULL, strlen ( src_string ) + 1);

  if ( _dest == NULL )
    return -1;

  strcpy ( _dest, src_string );

  return 0;
}

static void
cleanup_EffectiveArea_Fits ( EffectiveArea_Fits ef )
{

  if ( ! ef )
    return;

  if (ef->effAreaFile)
    free ( (void *) ef->effAreaFile);

  if (ef->offaxis)
    free ( (void *) ef->offaxis);

  if (ef->azimuth)
    free ( (void *) ef->azimuth);

  if ( ef->energy_grids && ef->ea_curves ) {

    if ( ef->n_elements_per_row > 1 )
      {
	int k;
	for (k=0; k<ef->n_rows; k++) {
	  free ( (void *) ef->energy_grids[k] );
	  free ( (void *) ef->ea_curves[k] );
	}
      }
    free ( (void *) ef->energy_grids );
    free ( (void *) ef->ea_curves );
  }

  return;
}

/**********************************************************************/
static int
_make_room ( EffectiveArea_Fits eff, long nrows )
{

  EffectiveArea_Fits _e=eff;

  if ( ! _e )
    return 0;

  _e->energy_grids= _allocate ( NULL, nrows * sizeof ( double * ) );
  _e->ea_curves   = _allocate ( NULL, nrows * sizeof ( double * ) );
  _e->offaxis     = _allocate ( NULL, nrows * sizeof ( double ) );
  _e->azimuth     = _allocate ( NULL, nrows * sizeof ( double ) );

  if (_e->energy_grids == NULL  ||  _e->ea_curves == NULL  || _e->offaxis == NULL  || _e->azimuth == NULL)
    return -1;

  return 0;
}

static EffectiveArea_Fits
read_fits_ea_file ( const char *eaFile )
{

  EffectiveArea_Fits _eff;
  fitsfile *_fp=NULL;

  long nrows, nels_per_row;

  int k, status=0;
  int nrows_int;

  int ene_col, ea_col;
  int off_col, azi_col;

  if ( eaFile == NULL ) {
    fprintf(stderr, "\n\
%s: Empty/null effective area file name while trying to read e.a. file\n\n", pname);
    return NULL;
  }
  _eff= _allocate ( NULL,  sizeof ( _EffectiveArea_Fits ) );

  if ( _eff == NULL ) {
    fprintf(stderr, "\n\
%s: Aborting read_fits_ea_file()\n\n", pname);
    return NULL;
  }

  if ( -1 == _copy_string ( &(_eff->effAreaFile), (char *) eaFile ) ) {
    cleanup_EffectiveArea_Fits ( _eff );
    return NULL;
  }

#define _READ_ONLY_  0

  /** Now read the fits file **/
  fits_open_file (&_fp, (char *) eaFile, _READ_ONLY_, &status );

  if ( status ) {
    fprintf(stderr, "\n\
%s: Error opening fits file '%s' for reading.\n\n", pname, eaFile);
    cleanup_EffectiveArea_Fits ( _eff );
    return NULL;
  }

  fits_movnam_hdu ( _fp, BINARY_TBL, (char *) ea_extension_name, 0, &status );

  if ( status ) {
    fprintf(stderr, "\n\
%s: Error moving to extension named '%s' in the effective area file '%s'\n\n",
	     pname, ea_extension_name, eaFile);
    cleanup_EffectiveArea_Fits ( _eff );
    return NULL;
  }

  /** CASEINSEN is a #define in fitsio.h: The extension name is case-insensitive **/
  fits_get_colnum ( _fp, CASEINSEN, ENERGY_COL_NAME, &ene_col, &status);
  fits_get_colnum ( _fp, CASEINSEN, EA_CURVE_COL_NAME, &ea_col, &status);
  fits_get_colnum ( _fp, CASEINSEN, OFFSET_COL_NAME, &off_col, &status);
  fits_get_colnum ( _fp, CASEINSEN, AZIMUTH_COL_NAME, &azi_col, &status);

  if ( status ) {
    fprintf(stderr, "\n\
%s: Error accessing column(s) in ea file '%s'\n\n", pname, eaFile);
    cleanup_EffectiveArea_Fits ( _eff );
    return NULL;
  }

  /***************************************************************************/
  /* Determine nrows & nels_per_row:  We are assuming here that nrows is the */
  /* same for all columns! We are also assuming that n_els_per_row is the    */
  /* same for both the energy grids and ea curves rows (they had better be!  */
  /***************************************************************************/
  fits_read_tdim ( _fp, ene_col, 2, &nrows_int, &nels_per_row, &status);

  if ( status ) {
    fprintf(stderr, "\n\
%s: Failed to determine the number of rows or the elements-per-row.\n\n", pname);
    cleanup_EffectiveArea_Fits ( _eff );
    return NULL;
  }

  fits_get_num_rows ( _fp, &nrows, &status );

  if (-1 == _make_room ( _eff, nrows ) ) {
    cleanup_EffectiveArea_Fits ( _eff );
    return NULL;
  }
  _eff->n_rows=nrows;
  _eff->n_elements_per_row=nels_per_row;

  for (k=0;  k<nrows;  k++ ) {
    _eff->energy_grids [k]= _allocate ( NULL, nels_per_row * sizeof ( double ) );
    _eff->ea_curves [k]= _allocate ( NULL, nels_per_row * sizeof ( double ) );

    if (_eff->energy_grids [k] == NULL || _eff->ea_curves [k] == NULL ) {
      fprintf(stderr, "\n\
%s: Error allocating space for ea/energy grids.\n\n", pname);
      cleanup_EffectiveArea_Fits ( _eff );
      return NULL;
    }
    fits_read_col_dbl ( _fp, off_col,  (long) k+1, 1, 1, 0, &(_eff->offaxis[k]), NULL, &status );
    fits_read_col_dbl ( _fp, azi_col,  (long) k+1, 1, 1, 0, &(_eff->azimuth[k]), NULL, &status );
    fits_read_col_dbl ( _fp, ene_col,  (long) k+1, 1, nels_per_row, 0, _eff->energy_grids [k], NULL, &status );
    fits_read_col_dbl ( _fp, ea_col, (long) k+1, 1, nels_per_row, 0, _eff->ea_curves [k], NULL, &status );

    if ( status ) {
      fprintf(stderr, "\n\
%s: Error reading table data for row %d.\n\n", pname, k+1);
      cleanup_EffectiveArea_Fits ( _eff );
      return NULL;
    }
  }

  fits_close_file ( _fp, &status );

  return _eff;
}

/**************************************************************/
/**     This structure is used to store the list of _unique_ **/
/** offset & azimuth values stored in the caldb file. This   **/
/**                                                          **/
/**     The routines immediately following this definition   **/
/** are used to fill two data structures of this type, one   **/
/** structure for the offset values and another structure    **/
/** for the azimuth values.                                  **/
/**************************************************************/
typedef struct {
  int N_UNIQUE;
  double *values;
} GridValues;

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
_element_exists( double val, double *array, int n_els )
{
  int k;

  for ( k=0; k < n_els; k++ ) {
    if ( val == array [k] )
      return 1;
  }
  return 0;

}

static GridValues *
_grid_from_full_list( double *fullList, long nels )
{

  GridValues *_gridValues;
  int N, k;
  int maxvals=16;

  double *_list= fullList;

  if ( !_list ) {
    fprintf(stderr, "\n\
%s: NULL array passed to _grid_from_full_list().\n\n", pname);
    return NULL;
  }
  _gridValues= malloc ( sizeof ( GridValues ) );
  if ( ! _gridValues ) {
    /** malloc error **/
    return NULL;
  }

  _gridValues->values= malloc ( maxvals * sizeof ( double ) );
  if ( ! _gridValues->values ) {
    /** malloc error **/
    return NULL;
  }
  memset ( _gridValues->values, 0, maxvals * sizeof ( double ) );

  for (N=0, k=0; k < nels; k++) {
    if ( _element_exists ( _list[k], _gridValues->values, N ))
      continue;

    if ( N == maxvals ) {
      maxvals *= 2;
      _gridValues->values= realloc ( _gridValues->values, maxvals * sizeof ( double ) );
      if ( _gridValues->values == NULL ) {
				/** malloc error **/
	return NULL;
      }
    } /* End, if ( N == maxvals ) block */
    _gridValues->values [N++]= _list[k];

  }

  _gridValues->N_UNIQUE=N;

  return _gridValues;
}

static int
search_interp_low ( double value, GridValues *_gv )
{
  int k;
  double *_array;

  if ( ! _gv ) {
    /** null GridValues error **/
    return -1;
  }
  _array= _gv->values;

  if ( ! _array ) {
    /** null array error **/
    return -1;
  }

  k=_gv->N_UNIQUE-1;

  while (value < _array [k]) k--;

  return k;
}

/**************************************************************************
  Determine which rows in the caldb file are required in the interpolation
  and weighting

  The routine returns an integer array of four values giving the row numbers
  in the fits bintary table to be used in the interpolation/weighting.

  The ordering of the curves is as follows:
	fits_cols[0]: (offset low, azimuth low)
	fits_cols[1]: (offset low, azimuth hi )
	fits_cols[2]: (offset hi,  azimuth low)
	fits_cols[3]: (offset hi,  azimuth hi)
**************************************************************************/

static int
_determine_fits_rows(double offset, double azimuth, int fits_rows[])
{
  GridValues *_caldbAzimuths;
  GridValues *_caldbOffsets;

  int N_AZ, N_OF;
  int off_index, az_index;

  int row1, row2;
  int row3, row4;

  normalize_offset_azimuth(&offset, &azimuth);

  _caldbAzimuths= _grid_from_full_list ( caldb_ea->azimuth, caldb_ea->n_rows );
  _caldbOffsets = _grid_from_full_list ( caldb_ea->offaxis, caldb_ea->n_rows );

  row1=row2=row3=row4=999999;	/** Initialize to a dummy value **/

  /** Initialize the values in the return array **/
  fits_rows[0] = fits_rows[1] = fits_rows[2] = fits_rows[3] = -1;

  N_AZ = _caldbAzimuths->N_UNIQUE;
  N_OF = _caldbOffsets ->N_UNIQUE;

  /** First special case: If the offset is 0.0, no interpolation  **/
  /** is needed. Return the index to the first row in the ae file **/
  if ( offset < EPSILON ) {
    fits_rows[0]= 0;
    return 0;
  }

  off_index=  search_interp_low ( offset, _caldbOffsets );
  az_index =  search_interp_low ( azimuth, _caldbAzimuths );

  if ( -1 == off_index || -1 == az_index ) {
    return -1;
  }

  if ( 0 == off_index ) {		/* (0, 0) */
    row1 =  1;
    row2 = -1;
  } else {
    row1 = N_AZ * ( off_index - 1 ) + az_index + 2;
    if ( az_index == (N_AZ-1) ) {
      row2 = row1 - (N_AZ-1);
    } else {
      row2 = row1 + 1;
    }
  }

  off_index++;
  if ( off_index < N_OF ) {
    row3 = N_AZ * ( off_index - 1 ) + az_index + 2;
    if ( az_index == (N_AZ-1) ) {
      row4 = row3 - (N_AZ-1);
    } else {
      row4 = row3 + 1;
    }
  } else {
    row3 = row4 = -1;
  }

  /** Because rows in the fits file begin at 1 **/
  /** whereas c arrays are indexed from 0, we  **/
  /** need to correct the row values. We must  **/
  /** be careful, though, to make no such      **/
  /** corrections for any rows with -1: These  **/
  /** indicate that a curve is unneeded in the **/
  /** interpolation.                           **/

  fits_rows[0] = (row1 == -1) ? -1 : row1 - 1;
  fits_rows[1] = (row2 == -1) ? -1 : row2 - 1;
  fits_rows[2] = (row3 == -1) ? -1 : row3 - 1;
  fits_rows[3] = (row4 == -1) ? -1 : row4 - 1;

  return 0;
}

/****************************************************************************/
static void
cleanup(void)
{
  cleanup_EffectiveArea_Fits ( caldb_ea );
  /*
    cleanup_Psf_Fits ( caldb_psf );
  */

  if ( ea_caldb_file )
    free ( (void *) ea_caldb_file );

  /**
     if ( psf_caldb_file )
     free ( (void *) psf_caldb_file );
  **/

}

/******************* Begin, effective area code *******************/

static int
_copy_ea_caldb_file_name ( char *caldbFile )
{
  ea_caldb_file= malloc ( strlen ( caldbFile ) + 1 );
  if (ea_caldb_file == NULL ) {
    return -1;
  }
  strcpy ( ea_caldb_file, caldbFile );
  return 0;
}

/** Determine the weights to apply to the four (or less) curves: **/

static int
_weights(double offset, double azimuth, int *fits_rows, double *weights)
{
  double off_hi, off_low, delta_offset, u;
  double az_hi, az_low, delta_azimuth, t;

  /*********************************************/
  /** Care must be taken here:  The fits_rows **/
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

  off_low= caldb_ea->offaxis [fits_rows [0]];
  if ( -1 != fits_rows[2] ) {
    off_hi = caldb_ea->offaxis [fits_rows [2]];
  } else if ( offset == off_low ) {
    off_hi = off_low;
  } else {
    fprintf(stderr, "\
%s: ERROR: offset > offset_max = %.3f\n", pname, off_low);
    return -1;
  }
  delta_offset = off_hi - off_low;

  if ( 0 == fits_rows[0] ) {	/* (0, 0) */
    if ( -1 != fits_rows[2] && -1 != fits_rows[3] ) {
      az_low = caldb_ea->azimuth [fits_rows [2]];
      az_hi = caldb_ea->azimuth [fits_rows [3]];
    } else {
      az_low = az_hi = 0.0;		/** Dummy value: We only have one curve to interpolate with **/
    }
  } else {
    if ( -1 != fits_rows[0] && -1 != fits_rows[1] ) {
      az_low = caldb_ea->azimuth [fits_rows [0]];
      az_hi = caldb_ea->azimuth [fits_rows [1]];
    } else {
      az_low = az_hi = 0.0;		/** Dummy value: We only have one curve to interpolate with **/
    }
  }
  delta_azimuth = az_hi - az_low;
  while ( delta_azimuth < 0 ) {
    delta_azimuth += 360.0;
  }

  /** This is a hack that needs to be fixed: The fix must come in **/
  /** the section where the GridValues/fits rows  are determined  **/
  /*	if ( az_low < EPSILON  && az_hi > 314 ) {
	az_hi = 360.0;
	az_low = 315.0;
	}*/

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
%s: Numerical error while determining weights: Weight > 1.0 found!\n\n", pname);
    return -1;
  }

  weights[0] = (1.0 - t) * (1.0 - u);
  weights[1] = t * (1.0 - u);
  weights[2] = ( 1.0 - t ) * u;
  weights[3] = t * u;

  if ( -1 == fits_rows[0] ) {
    return -1;
  }

  if ( -1 == fits_rows[1] ) {
    weights[0] += weights[1];
    weights[1] = 0.0;
  }

  if ( -1 == fits_rows[3] ) {
    weights[2] += weights[3];
    weights[3] = 0.0;
  }

  if ( -1 == fits_rows[2] ) {
    weights[0] += weights[2];
    weights[2] = 0.0;
  }

  return 0;
}

static int
_calculate_weighted_ea(int *fits_rows, double *weight, double **weighted_ene, double **weighted_ea)
{

  int k;
  double *ene_grid;
  double *ea0, *ea1, *ea2, *ea3;

  if ( weighted_ene == NULL || weighted_ea == NULL ) {
    fprintf(stderr, "\n\
%s: Null vectors passed to _calculate_weighted_ea()\n\n", pname);
    return -1;
  }

  *weighted_ene= malloc ( caldb_ea->n_elements_per_row * sizeof ( double ) );
  *weighted_ea = malloc ( caldb_ea->n_elements_per_row * sizeof ( double ) );

  if ( *weighted_ene == NULL  ||  *weighted_ea == NULL ) {
    fprintf(stderr, "\n\
%s: Malloc error while preparing energy & effective area curves.\n\n", pname);
    return -1;
  }

  if ( fits_rows[0] != -1 ) {
    ene_grid= caldb_ea->energy_grids[0];
  } else if ( fits_rows[1] != -1 ) {
    ene_grid= caldb_ea->energy_grids[1];
  } else if ( fits_rows[2] != -1 ) {
    ene_grid= caldb_ea->energy_grids[2];
  } else if ( fits_rows[3] != -1 ) {
    ene_grid= caldb_ea->energy_grids[3];
  } else {
    fprintf(stderr, "\n\
%s: No energy grid found in caldb file!\n\n", pname);
    return -1;
  }

  /* #Bug found by CB
     ea0= fits_rows[0] == -1 ? NULL : caldb_ea->ea_curves[0];
     ea1= fits_rows[1] == -1 ? NULL : caldb_ea->ea_curves[1];
     ea2= fits_rows[2] == -1 ? NULL : caldb_ea->ea_curves[2];
     ea3= fits_rows[3] == -1 ? NULL : caldb_ea->ea_curves[3];
  */

  ea0= fits_rows[0] == -1 ? NULL : caldb_ea->ea_curves[fits_rows[0]];
  ea1= fits_rows[1] == -1 ? NULL : caldb_ea->ea_curves[fits_rows[1]];
  ea2= fits_rows[2] == -1 ? NULL : caldb_ea->ea_curves[fits_rows[2]];
  ea3= fits_rows[3] == -1 ? NULL : caldb_ea->ea_curves[fits_rows[3]];

  /********************************************/
  /** We are making 2 big assumptions here:  **/
  /** (1) The energy grids                   **/
  /** (2) The grids have the same length     **/
  /********************************************/
  for (k=0; k<caldb_ea->n_elements_per_row; k++) {
    double a0, a1, a2, a3;

    a0 = (NULL != ea0) ? ea0 [k] : 0.0;
    a1 = (NULL != ea1) ? ea1 [k] : 0.0;
    a2 = (NULL != ea2) ? ea2 [k] : 0.0;
    a3 = (NULL != ea3) ? ea3 [k] : 0.0;

    (*weighted_ene)[k]= ene_grid [k];
    (*weighted_ea)[k] = weight [0] * a0
      + weight [1] * a1
      + weight [2] * a2
      + weight [3] * a3;
  }

  return 0;
}

int
xis_effarea_init (char *eaFile)
{
  ea_library_initialized = 0;
  caldb_ea=NULL;
  ea_caldb_file=NULL;

  if ( eaFile == NULL ) {
    fprintf(stderr, "\n\
%s: NULL caldb file passed to initialization routine.\n\n", pname);
    return -1;
  }

  caldb_ea= read_fits_ea_file ( (const char *) eaFile );
  if ( NULL == caldb_ea ) {
    return -1;
  }

  ea_library_initialized=1;

  if (-1 == _copy_ea_caldb_file_name ( eaFile )) {
    cleanup ( );
    return -1;
  }

  return 0;
}

int
xis_effarea( double offaxis, double azimuth, double **ene, double **ea)
{
  int fits_rows[4];
  double weights[4];
  int k;

  if ( offaxis < 0.0 ) {
    fprintf(stderr, "\n\
%s: Offaxis value < 0.0 not allowed! (offaxis=%f)\n\n", pname, offaxis);
    cleanup ();
    return -1;
  }

  if ( ! ea_library_initialized ) {
    fprintf(stderr, "\n\
%s: The xis_xtrea library has not been initialized/no caldb file has been given!\n\n", pname);
    return -1;
  }

  /** Here we are calculating the row numbers in the fits table **/
  /** that contain the curves that will be needed.              **/

  if ( -1 == _determine_fits_rows( offaxis, azimuth, fits_rows ) ) {
    return -1;
  }

  if ( -1 == _weights( offaxis, azimuth, fits_rows, weights ) ) {
    return -1;
  }

  for (k=0; k<4; k++) {
    printf("\
Row %2d: ( %5.2f , %5.1f ) weight = %f\n",
	   (fits_rows[k] < 0) ? -1 : fits_rows[k]+1,
	   caldb_ea->offaxis[ fits_rows[k] ],
	   caldb_ea->azimuth[ fits_rows[k] ],
	   weights[k]);
  }

  if ( -1 == _calculate_weighted_ea ( fits_rows, weights, ene, ea ) ) {
    return -1;
  }

  return caldb_ea->n_elements_per_row;
}

/* for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
