#include <math.h>
#include <string.h>
#include "fitsio.h"
#include "longnam.h"
#include "teldef2.h"

/* Create a new TR_MULTISEG structure and read its contents from a TelDef file. */
int readTrMultiseg /* Returns CFITSIO status */
(
 fitsfile* fp, /* Teldef file pointer */
 TR_MULTISEG** p_multisegparam, /* Pointer to TR_MULTISEG structure pointer */
 char* lowcoordsysname, /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys,  /* Number of orig. coord. sys. */
 char* filename /* Name of TelDef file */
 )
{
  TR_MULTISEG* multisegparam = NULL;
  char keyname[FLEN_KEYWORD];
  char tempstring[1000];
  int status = 0;
  int hdutype = ANY_HDU;
  char extname[20];
  int col = 0;
  long row = 0;
  char colname[FLEN_VALUE] = "";
  int colnum = 0;
  int anynul = 0;
  int is_propnum_found = 0;
  char* coeffcolnames[N_MULTISEG_COEFF] = {"COEFF_X_A", "COEFF_X_B", 
					   "COEFF_X_C", "COEFF_X_D", "COEFF_X_E",
					   "COEFF_Y_A", "COEFF_Y_B", 
					   "COEFF_Y_C", "COEFF_Y_D", "COEFF_Y_E"};

  /* Allocate and initialize the structure. */

  *p_multisegparam = (TR_MULTISEG*) malloc(sizeof(TR_MULTISEG));
  multisegparam = *p_multisegparam;

  strcpy(multisegparam->lowcoordsysname, lowcoordsysname);
  strcpy(multisegparam->highcoordsysname, highcoordsysname);
  multisegparam->low_sys = low_sys;
  multisegparam->high_sys = low_sys + 1;

  multisegparam->n_properties = 0;
  multisegparam->propertynames = NULL;
  multisegparam->n_rows = 0;
  multisegparam->n_cols = 0;
  multisegparam->properties = NULL;
  multisegparam->min_properties = NULL;
  multisegparam->max_properties = NULL;
  multisegparam->coeff_x_a = NULL;
  multisegparam->coeff_x_b = NULL;
  multisegparam->coeff_x_c = NULL;
  multisegparam->coeff_x_d = NULL;
  multisegparam->coeff_x_e = NULL;
  multisegparam->coeff_y_a = NULL;
  multisegparam->coeff_y_b = NULL;
  multisegparam->coeff_y_c = NULL;
  multisegparam->coeff_y_d = NULL;
  multisegparam->coeff_y_e = NULL;
  strcpy(multisegparam->winoffx_name, "NONE");
  strcpy(multisegparam->winoffy_name, "NONE");
  strcpy(multisegparam->winpropx_name, "NONE");
  strcpy(multisegparam->winpropy_name, "NONE");
  multisegparam->winpropx_num = -1;
  multisegparam->winpropy_num = -1;
  multisegparam->use_multiple_winoffx = 0;
  multisegparam->use_multiple_winoffy = 0;
  multisegparam->min_winpropx = 0;
  multisegparam->max_winpropx = 0;
  multisegparam->min_winpropy = 0;
  multisegparam->max_winpropy = 0;

  for(col = 0; col < N_MULTISEG_COEFF; col++)
    {
      multisegparam->coeffarrays[col] = NULL;
    }

  /* Move to MULTISEGn_COEFF extension. */

  sprintf(extname, "MULTISEG%d_COEFF", low_sys);
  fits_movnam_hdu(fp, hdutype, extname, 0, &status);
  sprintf(tempstring, "moving to %s extension of", extname);
  checkFITSerrors(status, tempstring, filename); 
  if(status)
    return status;


 /* Read the number of segment properties from the NPROP keyword. */

  fits_read_key_lng(fp, "NPROP", &multisegparam->n_properties, NULL, &status);
  checkFITSerrors(status, "reading NPROP from", filename);
  if(status)
    return status;

  /* Allocate propertynames array. These names are assumed to 
   * match the event file column names. */

  multisegparam->propertynames = malloc(multisegparam->n_properties * sizeof(char*));

  /* Read the property names. */

  for(col = 0; col < multisegparam->n_properties; col++)
    {
      multisegparam->propertynames[col] = malloc(FLEN_KEYWORD * sizeof(char));

      sprintf(keyname, "PROP%d", col);
      fits_read_key_str(fp, keyname, tempstring, NULL, &status);
      strcpy(multisegparam->propertynames[col], tempstring);
      sprintf(tempstring, "reading %s from", keyname);
      checkFITSerrors(status, tempstring, filename);
      if(status)
	return status;
    }


  /* Read the number of table rows from the NAXIS2 keyword. */

  fits_read_key_lng(fp, "NAXIS2", &multisegparam->n_rows, NULL, &status);
  checkFITSerrors(status, "reading NAXIS2 from", filename);
  if(status)
    return status;

  /* Read the number of table columns from the TFIELDS keyword. */

  fits_read_key_lng(fp, "TFIELDS", &multisegparam->n_cols, NULL, &status);
  checkFITSerrors(status, "reading TFIELDS from", filename);
  if(status)
    return status;

  /* Check if there are enough columns in the table. */

  if(multisegparam->n_properties + N_MULTISEG_COEFF > multisegparam->n_cols)
    {
      fprintf(stderr, "Teldef error: Mismatch in the number of columns of extension %s of teldef file %s.\nNPROP = %ld and TFIELDS = %ld, but TFIELDS must be at least NPROP + %d.\n", extname, filename, multisegparam->n_properties, multisegparam->n_cols, N_MULTISEG_COEFF);
      return 1;
    }
  
  /* Allocate properties and coeff arrays to store the MULTISEGn_COEFF table. */
  
  multisegparam->properties = (long**) malloc(multisegparam->n_properties * sizeof(long*));
  multisegparam->min_properties = (long*) malloc(multisegparam->n_properties * sizeof(long));
  multisegparam->max_properties = (long*) malloc(multisegparam->n_properties * sizeof(long));
  multisegparam->coeff_x_a = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_x_b = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_x_c = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_x_d = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_x_e = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_y_a = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_y_b = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_y_c = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_y_d = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeff_y_e = (double*) malloc(multisegparam->n_rows * sizeof(double));
  multisegparam->coeffarrays[0] = multisegparam->coeff_x_a;
  multisegparam->coeffarrays[1] = multisegparam->coeff_x_b;
  multisegparam->coeffarrays[2] = multisegparam->coeff_x_c;
  multisegparam->coeffarrays[3] = multisegparam->coeff_x_d;
  multisegparam->coeffarrays[4] = multisegparam->coeff_x_e;
  multisegparam->coeffarrays[5] = multisegparam->coeff_y_a;
  multisegparam->coeffarrays[6] = multisegparam->coeff_y_b;
  multisegparam->coeffarrays[7] = multisegparam->coeff_y_c;
  multisegparam->coeffarrays[8] = multisegparam->coeff_y_d;
  multisegparam->coeffarrays[9] = multisegparam->coeff_y_e;

  for(col = 0; col < multisegparam->n_properties; col++)
    {
      multisegparam->properties[col] = malloc(multisegparam->n_rows * sizeof(long));
      multisegparam->min_properties[col] = LONG_MAX;
      multisegparam->max_properties[col] = LONG_MIN;
    }


  /* Read PROPERTYn columns of MULTISEGn_COEFF table and calculate their min and max values. */

  for(col = 0; col < multisegparam->n_properties; col++)
    {
      sprintf(colname, "PROPERTY%d", col);
      fits_get_colnum(fp, CASEINSEN, colname, &colnum, &status);
      sprintf(tempstring, "finding column %s in %s extension of", colname, extname);
      checkFITSerrors(status, tempstring, filename);
      if(status)
	return status;
      
      fits_read_col_lng(fp, colnum, 1, 1, multisegparam->n_rows, 
			-999, multisegparam->properties[col], &anynul, &status);
      sprintf(tempstring, "reading column %s in %s extension of", colname, extname);
      checkFITSerrors(status, tempstring, filename);
      if(status)
	return status;

      for(row = 0; row < multisegparam->n_rows; row++)
	{
	  if(multisegparam->properties[col][row] < multisegparam->min_properties[col])
	    multisegparam->min_properties[col] = multisegparam->properties[col][row];

	  if(multisegparam->properties[col][row] > multisegparam->max_properties[col])
	    multisegparam->max_properties[col] = multisegparam->properties[col][row];
	}
    }


  /* Read COEFF_[XY]_[A-E] columns of MULTISEGn_COEFF table. */

  for(col = 0; col < N_MULTISEG_COEFF; col++)
    {
      strcpy(colname, coeffcolnames[col]);
      fits_get_colnum(fp, CASEINSEN, colname, &colnum, &status);
      sprintf(tempstring, "finding column %s in %s extension of", colname, extname);
      checkFITSerrors(status, tempstring, filename);
      if(status)
	return status;
      
      fits_read_col_dbl(fp, colnum, 1, 1, multisegparam->n_rows, 
			-999, multisegparam->coeffarrays[col], &anynul, &status);
      sprintf(tempstring, "reading column %s in %s extension of", colname, extname);
      checkFITSerrors(status, tempstring, filename);
      if(status)
	return status;
    }

  /* Require the moduli (COEFF_[XY]_D) to be positive to avoid strange negative modulus operations
   * and division by 0 exceptions. */

  for(row = 0; row < multisegparam->n_rows; row++)
    {
      if(multisegparam->coeff_x_d[row] < 1)
	{
	  fprintf(stderr, "In TelDef file %s, extension %s: Column COEFF_X_D, Row %ld value = %f must be positive to function as a modulus.\n", filename, extname, row + 1, multisegparam->coeff_x_d[row]);
	  return 1;
	}

      if(multisegparam->coeff_y_d[row] < 1)
	{
	  fprintf(stderr, "In TelDef file %s, extension %s: Column COEFF_Y_D, Row %ld value = %f must be positive to function as a modulus.\n", filename, extname, row + 1, multisegparam->coeff_y_d[row]);
	  return 1;
	}
    }

  /* Read the windowing offset keyword names, which are the values of the 
   * WINOFFX and WINOFFY TelDef keywords.*/

  fits_read_key_str(fp, "WINOFFX", multisegparam->winoffx_name, NULL, &status);

  if(status == KEY_NO_EXIST)
    {
      strcpy(multisegparam->winoffx_name, "NONE");
      strcpy(multisegparam->winpropx_name, "NONE");
      status = 0;
#ifdef DEBUG
      fprintf(stdout, "Teldef extension %s keyword %s not found. Disabling the windowing X offset.\n", extname, multisegparam->winoffx_name);
#endif
    }
  else if(status)
    return status;
  else
    {
      sprintf(tempstring, "reading keyword WINOFFX in %s extension of", extname);
      checkFITSerrors(status, tempstring, filename);
    }

  fits_read_key_str(fp, "WINOFFY", multisegparam->winoffy_name, NULL, &status);

  if(status == KEY_NO_EXIST)
    {
      strcpy(multisegparam->winoffy_name, "NONE");
      strcpy(multisegparam->winpropy_name, "NONE");
      status = 0;
#ifdef DEBUG
      fprintf(stdout, "Teldef extension %s keyword %s not found. Disabling the windowing Y offset.\n", extname, multisegparam->winoffy_name);
#endif
    }
  else if(status)
    return status;
  else
    {
      sprintf(tempstring, "reading keyword WINOFFY in %s extension of", extname);
      checkFITSerrors(status, tempstring, filename);
    }

  /* Determine whether the WINPROPX keyword is needed and if there will be one or multiple windowing x-offset keywords. */

  if(!strcasecmp(multisegparam->winoffx_name, "NONE"))
    {
      /* No windowing x-offset is used. */

      strcpy(multisegparam->winpropx_name, "NONE");
      multisegparam->use_multiple_winoffx = 0;
    }
  else
    {
      /* A windowing x-offset is used.  See if multiple offsets are expected. */

      fits_read_key_str(fp, "WINPROPX", multisegparam->winpropx_name, NULL, &status);
      
      if(status == KEY_NO_EXIST)
	{
	  strcpy(multisegparam->winpropx_name, "NONE");
	  multisegparam->use_multiple_winoffx = 0;
	  status = 0;
#ifdef DEBUG
	  fprintf(stdout, "Teldef extension %s keyword %s not found. Assuming a single windowing X offset.\n", extname, multisegparam->winpropx_name);
#endif
	}
      else if(status)
	return status;
      else
	{
	  sprintf(tempstring, "reading keyword WINPROPX in %s extension of", extname);
	  checkFITSerrors(status, tempstring, filename);

	  if(!strcasecmp(multisegparam->winpropx_name, "NONE"))
	    multisegparam->use_multiple_winoffx = 0;
	  else
	    multisegparam->use_multiple_winoffx = 1;
	}

      
      if(multisegparam->use_multiple_winoffx)
	{
	  /* Determine the property number of the winprop property. */
	  
	  is_propnum_found = 0;
	  
	  for(col = 0; col < multisegparam->n_properties; col++)
	    {
	      if(!strcasecmp(multisegparam->winpropx_name, multisegparam->propertynames[col]))
		{
		  multisegparam->winpropx_num = col;
		  is_propnum_found = 1;
		  break;
		}
	    }
	  
	  if(!is_propnum_found)
	    {
	      fprintf(stderr, "Cannot match the value '%s' of keyword WINPROPX with a property name given by the PROPn keywords in extension %s of TelDef file %s.\n", multisegparam->winpropx_name, extname, filename);
	      return 1;
	    }

	  /* Find the range of winprop property values. */
	  
	  multisegparam->min_winpropx = multisegparam->min_properties[multisegparam->winpropx_num];
	  multisegparam->max_winpropx = multisegparam->max_properties[multisegparam->winpropx_num];
	  
	  /* Check that all values of the winprop property are nonnegative. */
	  
	  if(multisegparam->min_winpropx < 0)
	    {
	      fprintf(stderr, "Some values of column %s have a negative value in extension %s of TelDef file %s, but values of this column must be nonnegative because this column specifies the windowing x-offset event header keyword names.", multisegparam->winpropx_name, extname, filename);
	      return 1;
	    }
	}
    }
  
  /* Determine whether the WINPROPY keyword is needed and if there will be one or multiple windowing y-offset keywords. */
      
  if(!strcasecmp(multisegparam->winoffy_name, "NONE"))
    {
      /* No windowing y-offset is used. */

      strcpy(multisegparam->winpropy_name, "NONE");
      multisegparam->use_multiple_winoffy = 0;
    }
  else
    {
      /* A windowing y-offset is used.  See if multiple offsets are expected. */

      fits_read_key_str(fp, "WINPROPY", multisegparam->winpropy_name, NULL, &status);
      
      if(status == KEY_NO_EXIST)
	{
	  strcpy(multisegparam->winpropy_name, "NONE");
	  multisegparam->use_multiple_winoffy = 0;
	  status = 0;
#ifdef DEBUG
	  fprintf(stdout, "Teldef extension %s keyword %s not found. Assuming a single windowing X offset.\n", extname, multisegparam->winpropy_name);
#endif
	}
      else if(status)
	return status;
      else
	{
	  sprintf(tempstring, "reading keyword WINPROPY in %s extension of", extname);
	  checkFITSerrors(status, tempstring, filename);

	  if(!strcasecmp(multisegparam->winpropy_name, "NONE"))
	    multisegparam->use_multiple_winoffy = 0;
	  else
	    multisegparam->use_multiple_winoffy = 1;
	}

      if(multisegparam->use_multiple_winoffy)
	{
	  /* Determine the property number of the winprop property. */
	  
	  is_propnum_found = 0;
	  
	  for(col = 0; col < multisegparam->n_properties; col++)
	    {
	      if(!strcasecmp(multisegparam->winpropy_name, multisegparam->propertynames[col]))
		{
		  multisegparam->winpropy_num = col;
		  is_propnum_found = 1;
		  break;
		}
	    }
	  
	  if(!is_propnum_found)
	    {
	      fprintf(stderr, "Cannot match the value '%s' of keyword WINPROPY with a property name given by the PROPn keywords in extension %s of TelDef file %s.\n", multisegparam->winpropy_name, extname, filename);
	      return 1;
	    }
	  
	  /* Find the range of winprop properties. */
	  
	  multisegparam->min_winpropy = multisegparam->min_properties[multisegparam->winpropy_num];
	  multisegparam->max_winpropy = multisegparam->max_properties[multisegparam->winpropy_num];
	  
	  /* Check that all values of the winprop property are nonnegative. */
	  
	  if(multisegparam->min_winpropy < 0)
	    {
	      fprintf(stderr, "Some values of column %s have a negative value in extension %s of TelDef file %s, but values of this column must be nonnegative because this column specifies the windowing y-offset event header keyword names.", multisegparam->winpropy_name, extname, filename);
	      return 1;
	    }
	}
    }
  
  return 0;
}


/* Print a TR_MULTISEG structure to a stream. */
void printTrMultiseg
(
 TR_MULTISEG* multisegparam, /* TR_MULTISEG structure to print */
 FILE* stream /* destination stream */
 )
{
  int col;
  int row;
  
  /* Print the structure contents element by element. */

  fprintf(stream, "    Tr_Multiseg structure contents:\n");
  fprintf(stream, "      lowcoordsysname: %s\n", multisegparam->lowcoordsysname);
  fprintf(stream, "      highcoordsysname: %s\n", multisegparam->highcoordsysname);
  fprintf(stream, "      low_sys: %d\n", multisegparam->low_sys);
  fprintf(stream, "      high_sys: %d\n", multisegparam->high_sys);
  fprintf(stream, "      winoffx_name: %s\n", multisegparam->winoffx_name);
  fprintf(stream, "      winoffy_name: %s\n", multisegparam->winoffy_name);
  fprintf(stream, "      winpropx_name: %s\n", multisegparam->winpropx_name);
  fprintf(stream, "      winpropy_name: %s\n", multisegparam->winpropy_name);
  fprintf(stream, "      winpropx_num: %d\n", multisegparam->winpropx_num);
  fprintf(stream, "      winpropy_num: %d\n", multisegparam->winpropy_num);
  fprintf(stream, "      use_multiple_winoffx: %d\n", multisegparam->use_multiple_winoffx);
  fprintf(stream, "      use_multiple_winoffy: %d\n", multisegparam->use_multiple_winoffy);
  fprintf(stream, "      min_propx: %ld\n", multisegparam->min_winpropx);
  fprintf(stream, "      max_propx: %ld\n", multisegparam->max_winpropx);
  fprintf(stream, "      min_propy: %ld\n", multisegparam->min_winpropy);
  fprintf(stream, "      max_propy: %ld\n", multisegparam->max_winpropy);
  fprintf(stream, "      n_properties: %ld\n", multisegparam->n_properties);

  for(col = 0; col < multisegparam->n_properties; col++)
    {
      fprintf(stream, "      propertynames[%d]: %s\n", col, multisegparam->propertynames[col]);

    }

  fprintf(stream, "      n_rows: %ld\n", multisegparam->n_rows);
  fprintf(stream, "      n_cols: %ld\n", multisegparam->n_cols);

  /* Print the MULTISEGn_COEFF table. */

  fprintf(stream, "      MULTISEG%d_COEFF table:\n", multisegparam->low_sys);
  fprintf(stream, "           ");
  
  for(col = 0; col < multisegparam->n_properties; col++)
    {
      fprintf(stream, "  P%d " , col);
    }
  fprintf(stream, "     X_A      X_B      X_C      X_D      X_E      Y_A      Y_B      Y_C      Y_D      Y_E\n");


  for(row = 0; row < multisegparam->n_rows; row++)
    {
      fprintf(stream, "           ");
      for(col = 0; col < multisegparam->n_cols; col++)
	{
	  if(col < multisegparam->n_properties)
	    {
	      fprintf(stream, "%4ld ", multisegparam->properties[col][row]);
	    }
	  else
	    {
	      fprintf(stream, "%8g ", multisegparam->coeffarrays[col - multisegparam->n_properties][row]);
	    }
	}
      fprintf(stream, "\n");
    }

      fprintf(stream, "   min:    ");
      for(col = 0; col < multisegparam->n_properties; col++)
	{
	  fprintf(stream, "%4ld ", multisegparam->min_properties[col]);
	}
      fprintf(stream, "\n");


      fprintf(stream, "   max:    ");
      for(col = 0; col < multisegparam->n_properties; col++)
	{
	  fprintf(stream, "%4ld ", multisegparam->max_properties[col]);
	}
      fprintf(stream, "\n");
  
}


/* Destroy a TR_MULTISEG structure. */
void destroyTrMultiseg
(
 TR_MULTISEG* multisegparam /* TR_MULTISEG structure to destroy. */
 )
{
  int col = 0;

#ifdef DEBUG
  int low_sys = -1;
#endif

  /* If multisegparam is a null pointer, no memory can be freed, so return. */

  if(multisegparam == NULL)
    return;

#ifdef DEBUG
  low_sys = multisegparam->low_sys;
  printf("Freeing multisegparam[%d]\n", low_sys);
#endif
  
  /* Free the coefficient arrays. */

  free(multisegparam->min_properties);
  free(multisegparam->max_properties);
  free(multisegparam->coeff_x_a);
  free(multisegparam->coeff_x_b);
  free(multisegparam->coeff_x_c);
  free(multisegparam->coeff_x_d);
  free(multisegparam->coeff_x_e);
  free(multisegparam->coeff_y_a);
  free(multisegparam->coeff_y_b);
  free(multisegparam->coeff_y_c);
  free(multisegparam->coeff_y_d);
  free(multisegparam->coeff_y_e);

  /* Free the property arrays. */

  for(col = 0; col < multisegparam->n_properties; col++)
    {
      free(multisegparam->propertynames[col]);
      free(multisegparam->properties[col]);
    }

  free(multisegparam->properties);
  free(multisegparam->propertynames);

  /* Free the whole structure. */

  free(multisegparam);

#ifdef DEBUG
  printf("Freed multisegparam[%d]\n", low_sys);
#endif
}

