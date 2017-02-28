#include <math.h>
#include <string.h>
#include "fitsio.h"
#include "longnam.h"
#include "teldef2.h"

/*#define DEBUG 1*/

/* Create a new TR_RAWTODET structure and read its contents from a TelDef file. */
int readTrRawtodet /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_RAWTODET** p_rawtodetparam, /* Pointer to TR_RAWTODET structure pointer */
 char* lowcoordsysname,  /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys, /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
 )
{
  TR_RAWTODET* rawtodetparam = NULL;
  int status = 0;
  int hdutype = ANY_HDU;
  char keyname[FLEN_KEYWORD];
  char tempstring[1000];
  int row = 0;
  
  /* Allocate and initialize the structure. */

  *p_rawtodetparam = (TR_RAWTODET*) malloc(sizeof(TR_RAWTODET));
  rawtodetparam = *p_rawtodetparam;

  strcpy(rawtodetparam->lowcoordsysname, lowcoordsysname);
  strcpy(rawtodetparam->highcoordsysname, highcoordsysname);
  rawtodetparam->low_sys = low_sys;
  rawtodetparam->high_sys = low_sys + 1;
  rawtodetparam->min_seg = 0; /* Default value */
  rawtodetparam->n_segs = 1; /* Default value */

  rawtodetparam->int_cen_x = 0.;
  rawtodetparam->int_cen_y = 0.;
  rawtodetparam->int_cen_found = 0;
  rawtodetparam->rawmethod = RM_UNKNOWN;
  strcpy(rawtodetparam->segcolname, "");
  rawtodetparam->corner_seg = NULL;
  rawtodetparam->corner_x = NULL;
  rawtodetparam->corner_y = NULL;

  for(row = 0; row < MAX_LINEAR_COEFF_SEGS; row++)
    {
      rawtodetparam->coeff_x_a[row] = 0;
      rawtodetparam->coeff_x_b[row] = 0;
      rawtodetparam->coeff_x_c[row] = 0;
      rawtodetparam->coeff_y_a[row] = 0;
      rawtodetparam->coeff_y_b[row] = 0;
      rawtodetparam->coeff_y_c[row] = 0;
    }

  rawtodetparam->use_nonlinear_corr = 0;
  rawtodetparam->distortion_in_orig_coord = 0;
  rawtodetparam->corr_map = NULL;
  rawtodetparam->inv_corr_map = NULL;

  /* Move to primary extension. */

  fits_movabs_hdu(fp, 1, &hdutype, &status);
  checkFITSerrors(status, "moving to primary extension of", filename); 
  if(status)
    return status;
  
  /* Need to add ASCA routine here if ASCA will be supported. */

  /* Read INn_[XY]CEN keywords for center of internal coordinate system. */
  
  sprintf(keyname, "IN%d_XCEN", low_sys);
  fits_read_key_dbl(fp, keyname, &rawtodetparam->int_cen_x, NULL, &status);
  if(status == KEY_NO_EXIST)
    {
      status = 0;

      /* Since INn_[XY]CEN wasn't found, try INT_XCEN. */

      sprintf(keyname, "INT_XCEN");
      fits_read_key_dbl(fp, keyname, &rawtodetparam->int_cen_x, NULL, &status);
      if(status == KEY_NO_EXIST)
	{
	  /* Default values are 0. for when the keywords aren't found.*/

	  rawtodetparam->int_cen_x = 0.;
	  rawtodetparam->int_cen_y = 0.;
	  rawtodetparam->int_cen_found = 0;
	}
      else
	{
	  /* Read the INT_[XY]CEN keywords. */
	  
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;

	  sprintf(keyname, "INT_YCEN");
	  fits_read_key_dbl(fp, keyname, &rawtodetparam->int_cen_y, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;

	  rawtodetparam->int_cen_found = 1;
	}
    }
  else
    {
      /* Read the INn-[XY]CEN keywords. */
      
      sprintf(tempstring, "reading %s from", keyname);
      checkFITSerrors(status, tempstring, filename);
      if(status)
	return status;

      sprintf(keyname, "IN%d_YCEN", low_sys);
      fits_read_key_dbl(fp, keyname, &rawtodetparam->int_cen_y, NULL, &status);
      sprintf(tempstring, "reading %s from", keyname);
      checkFITSerrors(status, tempstring, filename);
      if(status)
	return status;

      rawtodetparam->int_cen_found = 1;
    }


  /* Read the segment column name from xxx_SCOL keyword. */

  sprintf(keyname, "%s_SCOL", rawtodetparam->lowcoordsysname);
  fits_read_key_str(fp, keyname, rawtodetparam->segcolname, NULL, &status);
  
  if(status == KEY_NO_EXIST)
    {
      /* If the xxx_SCOL keyword isn't found, try SEG_COL. */
      
      status = 0;
      strcpy(keyname, "SEG_COL");
      fits_read_key_str(fp, keyname, rawtodetparam->segcolname, NULL, &status);
      
      if(status == KEY_NO_EXIST)
	{
	  status = 0;
	  strcpy(rawtodetparam->segcolname, "NONE");
	}
      else 
	{  
	  sprintf(tempstring, "reading %s from primary extension of", keyname);
	  checkFITSerrors(status, tempstring, filename);
	}

    }
  else
    {
      sprintf(tempstring, "reading %s from primary extension of", keyname);
      checkFITSerrors(status, tempstring, filename);
    }
  
  if(status)
    return status;
  



  /* Determine the transformation method by trying to read the TelDef file
   * in a handful of ways. */

  rawtodetparam->rawmethod = RM_UNKNOWN;

  if(rawtodetparam->rawmethod == RM_UNKNOWN)
    {
      /* Try reading a pixel corner map. */

      status = readPixelCornerMapInTelDef2(fp, rawtodetparam, filename, &(rawtodetparam->rawmethod));
      if(status)
	return status;
    }
  if(rawtodetparam->rawmethod == RM_UNKNOWN)
    {
      /* Try reading a table of linear coefficients. */

      status = readLinearCoeffInTelDef2(fp, rawtodetparam, filename, &(rawtodetparam->rawmethod));
      if(status)
	return status;
    }

  return 0;
}

/* Try to read pixel corner map and update p_rawmethod if successful. 
 * A failure may mean that this is the wrong type of RAWTODET 
 * transformation. */
int readPixelCornerMapInTelDef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* Teldef file pointer */
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
 char* filename, /* TelDef filename */
 enum RM_TYPE* p_rawmethod /* Returns type of RAWTODET transformation */
 )
{
  int orig_hdu;
  int hdutype;
  int anynul;
  int status = 0;

  long row;
  long n_rows;
  char tempstring[1000];

  char xcolname[] = "PIXELX";
  char ycolname[] = "PIXELY";
  char extname[] = "PIXEL_MAP";
  int segcolnumber, xcolnumber, ycolnumber;

  

  fits_get_hdu_num(fp, &orig_hdu); /* This might not be needed. */
  
  /* Try to find the PIXEL_MAP extension. */

  fits_movnam_hdu(fp, ANY_HDU, extname, 0/* any version*/, &status);

  if(status==BAD_HDU_NUM) 
    return (RM_UNKNOWN);

  sprintf(tempstring, "finding %s extension in", extname);
  checkFITSerrors(status, tempstring, filename);

  /* Since PIXEL_MAP extension is found, the rawmethod is RM_CORNER_LIST. */

  *p_rawmethod = RM_CORNER_LIST;

  /* Read the number of PIXEL_MAP table rows.  This value is the
   * number of segments. */

  fits_read_key_lng(fp,"NAXIS2", &n_rows, NULL, &status);
  sprintf(tempstring, "reading NAXIS2 from %s extension in", extname);
  checkFITSerrors(status, tempstring, filename);

  rawtodetparam->n_segs = n_rows;

  /* Allocate the arrays for storing the table.*/

  rawtodetparam->corner_seg = calloc(n_rows, sizeof(double));
  rawtodetparam->corner_x = calloc(n_rows, sizeof(double*));
  rawtodetparam->corner_y = calloc(n_rows, sizeof(double*));

  for(row = 0; row < n_rows; row++)
    {
      rawtodetparam->corner_x[row] = calloc(4, sizeof(double));
      rawtodetparam->corner_y[row] = calloc(4, sizeof(double));
    }


  /* Return to primary extension. */
  
  fits_movabs_hdu(fp,1,&hdutype,&status);
  checkFITSerrors(status,"moving to primary extension in", filename);
  if(status)
    return status;

  /* Return to PIXEL_MAP extension. */

  fits_movnam_hdu(fp, ANY_HDU, extname, 0/* any version*/, &status);

  if(status==BAD_HDU_NUM) 
    return (RM_UNKNOWN);
  sprintf(tempstring, "finding %s extension in", extname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Find column names in PIXEL_MAP table. */

  fits_get_colnum(fp, CASESEN, rawtodetparam->segcolname, &segcolnumber, &status);
  sprintf(tempstring, "finding %s column in %s extension", rawtodetparam->segcolname, extname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  fits_get_colnum(fp, CASESEN, xcolname, &xcolnumber, &status);
  sprintf(tempstring, "finding %s column in %s extension", xcolname, extname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  fits_get_colnum(fp, CASESEN, ycolname, &ycolnumber, &status);
  sprintf(tempstring, "finding %s column in %s extension", ycolname, extname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Read the PIXEL_MAP table data. */

  for(row = 0; row < n_rows; row++)
    {
      fits_read_col_int(fp, segcolnumber, row + 1, 1l, 1l, 0  , &rawtodetparam->corner_seg[row],    &anynul, &status);
      fits_read_col_dbl(fp,   xcolnumber, row + 1, 1l, 4l, 0.0, rawtodetparam->corner_x[row], &anynul, &status);
      fits_read_col_dbl(fp,   ycolnumber, row + 1, 1l, 4l, 0.0, rawtodetparam->corner_y[row], &anynul, &status);

    }

  sprintf(tempstring, "reading %s, %s, %s columns in %s extension", rawtodetparam->segcolname, xcolname, ycolname, extname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  return status;
}


/* Try to read linear coefficients of segments and update p_rawmethod 
 * if successful.  A failure may mean that this is the wrong type of 
 * RAWTODET transformation. */
int readLinearCoeffInTelDef2 /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
 char* filename, /* TelDef filename */
 enum RM_TYPE* p_rawmethod /* Returns type of RAWTODET transformation */

)
{
  char keyname[FLEN_KEYWORD];
  char tempstring[1000];
  double keyvalue = 0.0;
  int status = 0;
  int coeff_found = 0;
  int seg = 0;

  /* Look first for COE_[XY]_[ABC] keywords.  If found, there is only
     one segment. */

  fits_read_key_dbl(fp, "COE_X_A", &keyvalue, NULL, &status);
  if(!status)
    {
      /* Since COE_X_A was found, there is only one segment. */

      rawtodetparam->n_segs = 1;
      rawtodetparam->min_seg = 0;
      
      coeff_found = 1;
      checkFITSerrors(status,"reading COE_X_A from", filename);
      if(status)
	return status;
      rawtodetparam->coeff_x_a[0] = keyvalue;

      /* Read the remaining COE_[XY]_[ABC] keywords. */

      fits_read_key_dbl(fp, "COE_X_B", &keyvalue, NULL, &status);
      checkFITSerrors(status,"reading COE_X_B from", filename);
      if(status)
	return status;
      rawtodetparam->coeff_x_b[0] = keyvalue;

      fits_read_key_dbl(fp, "COE_X_C", &keyvalue, NULL, &status);
      checkFITSerrors(status,"reading COE_X_C from", filename);
      if(status)
	return status;
      rawtodetparam->coeff_x_c[0] = keyvalue;

      fits_read_key_dbl(fp, "COE_Y_A", &keyvalue, NULL, &status);
      checkFITSerrors(status,"reading COE_Y_A from", filename);
      if(status)
	return status;
      rawtodetparam->coeff_y_a[0] = keyvalue;

      fits_read_key_dbl(fp, "COE_Y_B", &keyvalue, NULL, &status);
      checkFITSerrors(status,"reading COE_Y_B from", filename);
      if(status)
	return status;
      rawtodetparam->coeff_y_b[0] = keyvalue;

      fits_read_key_dbl(fp, "COE_Y_C", &keyvalue, NULL, &status);
      checkFITSerrors(status,"reading COE_Y_C from", filename);
      if(status)
	return status;
      rawtodetparam->coeff_y_c[0] = keyvalue;

      *p_rawmethod = RM_LINEAR_COEFF;

      return 0;
    }
  else if(status != KEY_NO_EXIST)
    {
      checkFITSerrors(status,"reading COE_X_A from", filename);
      if(status)
	return status;
    }

  /* The COE_X_A keyword was not found.  Look next for
   * COc_[XY]n_[ABC] keywords.  Here c = coord sys number of
   * lower-level system, and n = segment number. Search
   * only single-digit n values. */
  
  status = 0;
  
  for(seg = 0; seg < MAX_LINEAR_COEFF_SEGS; seg++)
    {
      sprintf(keyname, "CO%d_X%d_A", rawtodetparam->low_sys, seg);
      fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
      if(status == KEY_NO_EXIST)
	{
	  /* If the keyword for this segment is not found, but
	     previous keywords were found, then all of the keywords
	     have already been read, so break. Otherwise continue to
	     the next segment number in hopes it will be the first
	     segment with keywords to read. */
	  
	  status = 0;

	  if(coeff_found == 1)
	    {
	      rawtodetparam->n_segs = seg - rawtodetparam->min_seg;
	      break;
	    }
	  continue;
	}
      else
	{
	  /* The keyword was found, so read all of the keywords for this segment. */

	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;

	  rawtodetparam->coeff_x_a[seg] = keyvalue;
	  if(!coeff_found)
	    {
	      /* This is the first segment number that is used. */

	      rawtodetparam->min_seg = seg;
	    }
	  coeff_found = 1;
	  
	  
	  sprintf(keyname, "CO%d_X%d_B", rawtodetparam->low_sys, seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_x_b[seg] = keyvalue;

	  sprintf(keyname, "CO%d_X%d_C", rawtodetparam->low_sys, seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_x_c[seg] = keyvalue;


	  sprintf(keyname, "CO%d_Y%d_A", rawtodetparam->low_sys, seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_y_a[seg] = keyvalue;

	  sprintf(keyname, "CO%d_Y%d_B", rawtodetparam->low_sys, seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_y_b[seg] = keyvalue;

	  sprintf(keyname, "CO%d_Y%d_C", rawtodetparam->low_sys, seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_y_c[seg] = keyvalue;

	}
    }

  /* Set the transformation method if any coeff. keywords were found. */

  if(coeff_found)
    {
      *p_rawmethod = RM_LINEAR_COEFF;
      return 0;
    }

  /* The COc_Xn_A keywords were not found either.  Look next for
   * COE_[XY]n_[ABC] keywords.  
   * Here n = segment number. Search only single-digit n values. */
  
  status = 0;
  
  for(seg = 0; seg < MAX_LINEAR_COEFF_SEGS; seg++)
    {
      sprintf(keyname, "COE_X%d_A", seg);
      fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
      if(status == KEY_NO_EXIST)
	{
	  status = 0;

	  /* If the keyword for this segment is not found, but
	     previous keywords were found, then all of the keywords
	     have already been read, so break. Otherwise continue to
	     the next segment number in hopes it will be the first
	     segment with keywords to read. */
	  
	  if(coeff_found == 1)
	    {
	      /* This is the first segment number that is used. */

	      rawtodetparam->n_segs = seg - rawtodetparam->min_seg;
	      break;
	    }
	  continue;
	}
      else
	{
	  /* The keyword was found, so read all of the keywords for
	     this segment. */

	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_x_a[seg] = keyvalue;
	  if(!coeff_found)
	    {
	      rawtodetparam->min_seg = seg;
	    }
	  coeff_found = 1;
	  
	  
	  sprintf(keyname, "COE_X%d_B", seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_x_b[seg] = keyvalue;

	  sprintf(keyname, "COE_X%d_C", seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_x_c[seg] = keyvalue;


	  sprintf(keyname, "COE_Y%d_A", seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_y_a[seg] = keyvalue;

	  sprintf(keyname, "COE_Y%d_B", seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_y_b[seg] = keyvalue;

	  sprintf(keyname, "COE_Y%d_C", seg);
	  fits_read_key_dbl(fp, keyname, &keyvalue, NULL, &status);
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, filename);
	  if(status)
	    return status;
	  rawtodetparam->coeff_y_c[seg] = keyvalue;

	}
    }

  /* Set the transformation method if any coeff. keywords were found. */

  if(coeff_found)
    {
      *p_rawmethod = RM_LINEAR_COEFF;
      return 0;
    }

  /* If no keywords were found, then the method remains unknown. */

  *p_rawmethod = RM_UNKNOWN;  
  return 0;
}



/* Print a TR_RAWTODET structure to a stream */
void printTrRawtodet
(
 TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure to print */
 FILE* stream /* destination stream */
 )
{
  int row;

  /* Print the structure element by element. */

  fprintf(stream, "    Tr_Rawtodet structure contents:\n");
  
  fprintf(stream, "      lowcoordsysname: %s\n", rawtodetparam->lowcoordsysname);
  fprintf(stream, "      highcoordsysname: %s\n", rawtodetparam->highcoordsysname);
  fprintf(stream, "      low_sys: %d\n", rawtodetparam->low_sys);
  fprintf(stream, "      high_sys: %d\n", rawtodetparam->high_sys);
  fprintf(stream, "      n_segs: %d\n", rawtodetparam->n_segs);
  fprintf(stream, "      min_seg: %d\n", rawtodetparam->min_seg);
  fprintf(stream, "      segcolname: %s\n", rawtodetparam->segcolname);
  fprintf(stream, "      int_cen_x: %g\n", rawtodetparam->int_cen_x);
  fprintf(stream, "      int_cen_y: %g\n", rawtodetparam->int_cen_y);
  fprintf(stream, "      int_cen_found: %d\n", rawtodetparam->int_cen_found);
  fprintf(stream, "      rawmethod: %d\n", rawtodetparam->rawmethod);
  fprintf(stream, "      use_nonlinear_corr: %d\n", rawtodetparam->use_nonlinear_corr);
  fprintf(stream, "      distortion_in_orig_coord: %d\n", rawtodetparam->distortion_in_orig_coord);
  
  /* Print the pixel corner map if it was read. */

  if(rawtodetparam->rawmethod == RM_CORNER_LIST)
    {
      fprintf(stream, "      pixel map:\n");
      fprintf(stream, "         %4s  %7s %7s %7s %7s   %7s %7s %7s %7s\n", "SEG", "X0", "X1", "X2", "X3", "Y0", "Y1", "Y2", "Y3");

      for(row = 0; row < rawtodetparam->n_segs; row++)
	{
	  fprintf(stream, "         %4d  %7g %7g %7g %7g   %7g %7g %7g %7g\n", rawtodetparam->corner_seg[row], rawtodetparam->corner_x[row][0], rawtodetparam->corner_x[row][1],  rawtodetparam->corner_x[row][2],  rawtodetparam->corner_x[row][3],  rawtodetparam->corner_y[row][0],rawtodetparam->corner_y[row][1],  rawtodetparam->corner_y[row][2],  rawtodetparam->corner_y[row][3]);
	}

    }

  /* Print the linear coefficients table if it was read. */

  if(rawtodetparam->rawmethod == RM_LINEAR_COEFF)
    {
      fprintf(stream, "      linear coefficients:\n");
      fprintf(stream, "         %4s  %7s %7s %7s   %7s %7s %7s\n", "SEG", "X_A", "X_B", "X_C", "Y_A", "Y_B", "Y_C");

      for(row = rawtodetparam->min_seg; row < rawtodetparam->min_seg + rawtodetparam->n_segs; row++)
	{
	  fprintf(stream, "         %4d  %7g %7g %7g   %7g %7g %7g\n", row, rawtodetparam->coeff_x_a[row], rawtodetparam->coeff_x_b[row], rawtodetparam->coeff_x_c[row], rawtodetparam->coeff_y_a[row], rawtodetparam->coeff_y_b[row], rawtodetparam->coeff_y_c[row]);
	}
    }

  /* Print nonlinear correction map if it is used. */

  if(rawtodetparam->use_nonlinear_corr)
    {
      fprintf(stream, "      corr_map:\n");
      printMapXform(rawtodetparam->corr_map, stream, 8, 8);
      fprintf(stream, "      inv_corr_map:\n");
      printMapXform(rawtodetparam->inv_corr_map, stream, 8, 8);
    }

}


/* Destroy a TR_RAWTODET structure. */
void destroyTrRawtodet
(
 TR_RAWTODET* rawtodetparam /* TR_RAWTODET structure to destroy */
 )
{
  int row;

#ifdef DEBUG
  int low_sys = -1;
#endif

  /* If rawtodetparam is a null pointer, no memory can be freed, so return. */

  if(rawtodetparam == NULL)
    return;

#ifdef DEBUG
  low_sys =  rawtodetparam->low_sys;
  printf("Freeing rawtodetparam[%d]\n", low_sys);
#endif
  
  /* Free the arrays. */

  if(rawtodetparam->rawmethod == RM_CORNER_LIST)
    {
      for(row = 0; row < rawtodetparam->n_segs; row++)
	{
	  free(rawtodetparam->corner_x[row]);
	  free(rawtodetparam->corner_y[row]);
	}

      free(rawtodetparam->corner_seg);
      free(rawtodetparam->corner_x);
      free(rawtodetparam->corner_y);
    }

  /* Free the nonlinear corrections map and transformation.*/

  if(rawtodetparam->corr_map != NULL)
    destroyMapXform(rawtodetparam->corr_map);
  if(rawtodetparam->inv_corr_map != NULL)
    destroyMapXform(rawtodetparam->inv_corr_map);

  /* Free the whole structure. */

  free(rawtodetparam);

#ifdef DEBUG
  printf("Freed rawtodetparam[%d]\n", low_sys);
#endif
}

