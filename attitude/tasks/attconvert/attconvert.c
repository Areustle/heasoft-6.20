/** \file attconvert.c
 *  \brief This source code file contains the main() function for
 *  attconvert and the standard functions called by main(). 
 *  \author Timothy Reichard
 *  \date $Date: 2016/03/28 18:45:15 $
 */

/**
\defgroup tool_attconvert Convert attitude between representations
@ingroup mod_gen_tasks


This task converts between quaternion, Euler angle, and
R.A./Dec./Roll representations of attitude in an attitude FITS file
or in a single attitude point.  
 

Source files:

  attconvert.c
  attconvertlib.c
  attconvertlib.h

Library depenencies:

  heacore/headas
  heacore/ape
  heacore/heaapp
  attitude/lib/coordfits

Modification history:

  Ver   Date        Author  Description
  1.0   2016-03-28  RSH     initial version after adding prologue
*/

#include "attconvertlib.h"

#include "att_fatal.h"
#include "headas_main.c"

/* ======================================================================*/
/* Function declarations */

/* Allocate Params structure, read command-line parameters,
 * and set some other useful variables. */
int getPars
(
 Params** p_param /* (output) Pointer to parameter structure */
 );

/* In file mode, create and open the output file. Check the format of attitude column. */
int initialize
(
 Params* param, /* (input/output) Pointer to parameters structure */
 fitsfile** p_fp /* (output) Pointer to output file pointer */
 );

/* In file mode, read the file and convert the attidue in each row. In point mode, 
 * convert the point. */
int doWork
(
 Params* param, /* (input/output) Pointer to parameters structure */
 fitsfile* fp /* (output) file pointer */
 );

/* Clean up. */
void finalize
(
 Params* param, /* Pointer to parameter structure */
 fitsfile* fp /* Output file pointer */
 );

/* Create and open the output file. Check format of attitude column. */
int initializeForFile
(
 Params* param, /* Pointer to parameters structure */
 fitsfile** p_fp /* Pointer to output file pointer */
 );

/* Convert the attitude point to all formats. */
int doWorkForPoint
(
 Params* param /* Pointer to parameters structure */
 );

/* Read the output file rows and convert the attitude in each row. */
int doWorkForFile
(
 Params* param, /* Pointer to parameters structure */
 fitsfile* fp /* Output file pointer */
 );

/* ====================================================================== */
/* Function definitions */

/* Driver function: main() */
void attconvert_aux(void) 
{
  /* Declare the file pointer, parameters structure, and runtime status. */

  fitsfile* fp = NULL;
  Params* param = NULL;
  int status = 0;

  /* Execute the major parts of the task. */

  status = getPars(&param);
  if(status)
    {
      headas_chat(0, "%s error: Quitting due to problems reading or using parameters.\n", TOOLNAME);
      finalize(param, fp);
      return;
    }

  status = initialize(param, &fp);
  if(status)
    {
      headas_chat(0, "%s error: Quitting due to problems with initialization.\n", TOOLNAME);
      finalize(param, fp);
      return;
    }

  status = doWork(param, fp);
  if(status)
    {
      headas_chat(0, "%s error: Quitting due to problems processing the attitude data.\n", TOOLNAME);
      finalize(param, fp);
      return;
    }

  finalize(param, fp);

  return;
} 

/* ---------------------------------------------------------------------- */

/* Allocate the Params structure, read command-line parameters,
 * and parse the input parameter lists. */
int getPars
(
 Params** p_param /* Pointer to parameter structure */
 )
{
  int status = eOK; /* APE status */
  Params* param = NULL; /* Parameters structure */
  double input_object[MAX_COMPONENTS]; /* Array of rotation object components */
  const double rad_per_deg = M_PI/180.; /* radians per degree */
  int att_format = e_AF_UNKNOWN; /* enumerated attitude format */
  int are_lists_valid = 1; /* flag for if format and column lists are valid */
  char temp_suffix[] = ".tmp"; /* Temporary file extension */

  headas_chat(1, "%s info: Started.\n", TOOLNAME);

  /* Initialize/allocate members. */

  *p_param = (Params*) malloc(sizeof(Params));
  param = *p_param;
  param->m_input = NULL;
  param->m_in_file = NULL;
  param->m_out_file = NULL;
  param->m_temp_file = NULL;
  param->m_att_ext = NULL;
  param->m_in_att_col = NULL;
  param->m_out_att_col = NULL;
  param->m_in_att_format = NULL;
  param->m_out_att_format = NULL;
  param->m_temp_file_copied = 0;
  param->m_temp_file_written = 0;
  param->m_are_files_same = 0;

  param->m_att_io = (AttInputOutput*) malloc(N_ATT_FORMATS*sizeof(AttInputOutput));
  param->m_n_elements = (int*) malloc(N_ATT_FORMATS*sizeof(int));
  param->m_col_names = (char**) malloc(N_ATT_FORMATS*sizeof(char*));
  param->m_col_numbers = (int*) malloc(N_ATT_FORMATS*sizeof(int));
  
  for(att_format = 0; att_format < N_ATT_FORMATS; att_format++)
    {
      param->m_att_io[att_format] = e_IO_IGNORE;
      param->m_n_elements[att_format] = 0;
      param->m_col_names[att_format] = (char*) malloc(FLEN_VALUE*sizeof(char));
      strcpy(param->m_col_names[att_format], "");
    }
  
  param->m_quat = allocateQuat();
  param->m_euler = allocateEuler();
  param->m_pointing = allocatePointing();
  param->m_align = NULL;

  param->m_n_fits_cols = 0;
  param->m_fits_cols = NULL;
  param->m_fits_quat = NULL;
  param->m_fits_euler = NULL;
  param->m_fits_pointing = NULL;
  
  /* Read input file/point and input format.  Parse the input to figure out
   * which input type (file or point) will be used in conversions. */

  status = ape_trad_query_string("input", &(param->m_input));
  status = ape_trad_query_string("inform", &(param->m_in_att_format));
  ape_trad_query_string("alignfile", &(param->m_align_file));
  
  /* Validate the input format. */

  param->m_in_format = getAttFormatNumber(param->m_in_att_format);
  if(param->m_in_format == e_AF_UNKNOWN)
    {
      headas_chat(0, "%s error: Input attitude format '%s' is not valid.\n", TOOLNAME, param->m_in_att_format);
      return 1;
    }

  /* Set the input format. */

  param->m_in_n_elements = getAttFormatNumberOfElements(param->m_in_format);
  param->m_n_elements[param->m_in_format] = param->m_in_n_elements;
  param->m_att_io[param->m_in_format] = e_IO_INPUT;

  /* Parse the input parameter string and determine whether to convert a FITS file column
     or a single point. */

  param->m_in_type = parseInput(param->m_input, param->m_in_n_elements, input_object);
  
  if(param->m_in_type == e_IN_UNKNOWN)
    {
      /* The input string couldn't be parsed as a filename or as the multi-component 
       * numerical object specified by in_format, so give up. */
      
      headas_chat(0, "%s error: Cannot understand input value \"%s\". Quitting.\n", TOOLNAME, param->m_input);
    }
  else if(param->m_in_type == e_IN_FILE)
    {
      /* The input string wasn't a multi-component numerical object, so assume
       * it's a filename. Read the other parameters associated with file mode. */
      
      param->m_in_file = param->m_input;

      status = ape_trad_query_string("outfile", &(param->m_out_file));
      param->m_temp_file = (char*) malloc((strlen(param->m_out_file) + strlen(temp_suffix) + 1)
					  *sizeof(char));
      sprintf(param->m_temp_file, "%s%s", param->m_out_file, temp_suffix);

      status = ape_trad_query_string("attext", &(param->m_att_ext));
      status = ape_trad_query_string("incol", &(param->m_in_att_col));
      strcpy(param->m_col_names[param->m_in_format], param->m_in_att_col);
      status = ape_trad_query_string("outcol", &(param->m_out_att_col));
      status = ape_trad_query_string("outform", &(param->m_out_att_format));

      /* Parse and validate the output formats and columns. */

      are_lists_valid = parseOutputAttFormats(param->m_out_att_format, param->m_out_att_col,
					      param->m_att_io, param->m_n_elements,
					      param->m_col_names);

      for(att_format = 0; att_format < N_ATT_FORMATS; att_format++)
	{
	  if(param->m_att_io[att_format] == e_IO_OUTPUT && 
	     !strcasecmp(param->m_col_names[att_format], param->m_in_att_col)
	     )
	    {
	      headas_chat(0, "%s error: Output column name '%s' cannot be the same as the input column name.\n", TOOLNAME, param->m_col_names[att_format]);
	      are_lists_valid = 0;
	    }
	}

      if(!are_lists_valid)
	{
	  headas_chat(0, "%s error: Output formats and column names could not be validated. Quitting.\n", TOOLNAME);
	  return 1;
	}
    }
  
  /* Read parameters needed for all input modes. */

  status = ape_trad_query_bool("clobber", &(param->m_clobber));
  status = ape_trad_query_bool("debug", &(param->m_debug));
  status = ape_trad_query_bool("history", &(param->m_history));
  if(status != eOK)
    return status;

  /* Put the input into the appropriate math object, for point mode. */

  if(param->m_in_type == e_IN_POINT)
    {
      if(param->m_in_format == e_AF_QUAT)
	{
	  if(fabs(input_object[0]) < ZEROQTOL &&
	     fabs(input_object[1]) < ZEROQTOL &&
	     fabs(input_object[2]) < ZEROQTOL &&
	     fabs(input_object[3]) < ZEROQTOL
	     )
	    {
	      /* If the input quaternion has a magnitude of 0, it cannot be normalized or converted. */

	      headas_chat(0, "%s error: The input quaternion is the zero quaternion, which cannot be converted.\n", TOOLNAME);
	      return 1;
	    }

	  setQuat(param->m_quat, input_object[0], input_object[1],
		  input_object[2], input_object[3]);
	  forceRenormalizeQuat(param->m_quat);
	}
      else if(param->m_in_format == e_AF_EULER)
	{
	  /* Input Euler angles are in degrees, but the EULER struct
	   * uses radians, so convert each angle. Then convert the
	   * Euler angles to a quaternion and back to Euler angles to
	   * put them in the reproducible output representation (e.g.,
	   * input Euler (270, 270, 270) --> quat --> output Euler
	   * (90, 90, 90)).*/
	  
	  setEulerAngles(param->m_euler, input_object[0]*rad_per_deg, 
			 input_object[1]*rad_per_deg, input_object[2]*rad_per_deg);
	  convertEulerToQuat(param->m_quat, param->m_euler);
	  convertQuatToEuler(param->m_euler, param->m_quat);
	  fixEulerAngles(param->m_euler);
	}
      else if(param->m_in_format == e_AF_POINTING)
	{
	  /* Input Pointing angles are in degrees, and the POINTING struct uses 
	   * degrees, so no conversion is necessary. */

	  setPointing(param->m_pointing, input_object[0], 
		      input_object[1], input_object[2]);
	}
    }

  /* Display debugging output */

  if(param->m_debug && param->m_in_type == e_IN_FILE)
    {
      for(att_format = 0; att_format < N_ATT_FORMATS; att_format++)
	{
	  headas_chat(0,"%s debug: att_format=%d att_io=%d n_elements=%d col_names=%s\n", TOOLNAME, att_format, param->m_att_io[att_format], param->m_n_elements[att_format], param->m_col_names[att_format]);
	}
    }

  return status;
}

/* ---------------------------------------------------------------------- */

/* Read the alignment matrix and roll definition if needed, create the
   output file if needed, and set up the CFITSIO iterator
   framework. */
int initialize
(
 Params* param, /* Pointer to parameters structure */
 fitsfile** p_fp /* Pointer to output file pointer */
 )
{
  /* Declare runtime status */

  int status = 0;
  
  /* Read the alignment info. This is needed for conversions with ra, dec,
   * and roll. The case when it is not needed is for file mode 
   * conversions between QUAT and EULER.  All other file mode
   * conversions and all point mode conversions need it. */

  if(param->m_in_type == e_IN_POINT || 
     (param->m_in_type == e_IN_FILE && param->m_att_io[e_AF_POINTING] != e_IO_IGNORE)
     )
    param->m_align = readAlignmentInfo(param->m_align_file, param->m_debug);
  else
    param->m_align = allocateDefaultAlign();

  if(param->m_align == NULL)
    {
      headas_chat(0, "%s error: Cannot read alignment matrix and roll definition from %s. Quitting.\n", TOOLNAME, param->m_align_file);
      return 1;
    }

  if(param->m_debug)
    printAlign(param->m_align, stdout);

  /* Prepare the output file if a FITS file is to be used. */

  if(param->m_in_type == e_IN_FILE)
    {
      status = initializeForFile(param, p_fp);
    }
  else if(param->m_in_type == e_IN_POINT)
    {
      /* In point mode, there is nothing more to initialize. */

      status = 0;
    }

  return status;
}

/* ---------------------------------------------------------------------- */

/* Create and open the output file. Check the formats of attitude
   columns. Add any missing output columns. */
int initializeForFile
(
 Params* param, /* Pointer to parameters structure */
 fitsfile** p_fp /* Pointer to output file pointer */
 )
{
  char tempstring[2000] = "";
  
  /* Declare input and output column types and number of elements. */

  int incoltype = 0;
  int outcoltype = 0;
  long in_n_elements = 0;
  long out_n_elements = 0;

  /* Declare file pointers for input and temporary output files and a
   * flag for if a file exists. */

  fitsfile* fp_in = NULL;
  fitsfile* fp = NULL;
  int file_exists = 0;

  /* Declare a long value, status, and column number for CFITSIO operations. */

  long longvalue = 0;
  int status = 0;
  int col = -1;

  /* Declare an enumerated attitude format. */

  AttFormat att_format = e_AF_UNKNOWN;
  
  /* Check for the existence of the input file. */

  fits_file_exists(param->m_in_file, &file_exists, &status);
  if(status)
    {
      reportFITSError(status, "checking existence of", param->m_in_file, param->m_debug);
      return 1;
    }

  if(file_exists != 1)
    {
      headas_chat(0, "%s error: input file %s does not exist.\n", TOOLNAME, param->m_in_file);
      return 1;
    }

  /* Check if input and output files are the same file. */
  
  param->m_are_files_same = (areFilenamesSameFile(param->m_in_file, param->m_out_file) == 1);

  /* Quit if the alignfile is the same as either of the input or output files. */

  if(areFilenamesSameFile(param->m_in_file, param->m_align_file) == 1)
    {
      headas_chat(0, "%s error: alignfile %s cannot be the same file as the input file.\n", TOOLNAME, param->m_align_file, param->m_in_file);
      return 1;
    }

  if(areFilenamesSameFile(param->m_out_file, param->m_align_file) == 1)
    {
      headas_chat(0, "%s error: alignfile %s cannot be the same file as the output file.\n", TOOLNAME, param->m_align_file, param->m_out_file);
      return 1;
    }

  /* Remove the temp file if the file exists. */

  fits_file_exists(param->m_temp_file, &file_exists, &status);
  if(status)
    {
      reportFITSError(status, "searching for", param->m_temp_file, param->m_debug);
      return 1;
    }
  if(file_exists == 1)
    remove(param->m_temp_file);

  
  /* Remove the output file if it already exists, clobber is enabled, and it is not also the input file. */

  if(!param->m_are_files_same)
    {
      fits_file_exists(param->m_out_file, &file_exists, &status);
      if(status)
	{
	  reportFITSError(status, "searching for", param->m_temp_file, param->m_debug);
	  return 1;
	}
      if(file_exists == 1)
	{
	  if(param->m_clobber)
	    remove(param->m_out_file);
	  else
	    {
	      headas_chat(0, "%s error: Output file %s already exists, and clobber is disabled.  Will not overwrite that file.\n", TOOLNAME, param->m_out_file);
	      return 1;
	    }
	}
    }
  else if(param->m_are_files_same && !param->m_clobber)
    {
      headas_chat(0, "%s error: Will not overwrite infile with outfile because they refer to the same file, and clobber is disabled.\n", TOOLNAME);
      return 1;

    }

  /* Create the output file. */

  fits_open_file(&fp_in, param->m_in_file, READONLY, &status);
  reportFITSError(status, "opening", param->m_in_file, param->m_debug);
  if(status)
    return status;

  fits_create_file(p_fp, param->m_temp_file, &status);
  reportFITSError(status, "creating", param->m_temp_file, param->m_debug);
  if(status)
    return status;

  fp = *p_fp;
  
  fits_copy_file(fp_in, fp, 1, 1, 1, &status);
  sprintf(tempstring, "copying %s to", param->m_in_file);
  reportFITSError(status, tempstring, param->m_temp_file, param->m_debug);
  if(status)
    return status;
  
  /* Close input file. */
  
  fits_close_file(fp_in, &status);
  reportFITSError(status, "closing", param->m_in_file, param->m_debug);
  if(status)
    return status;
  
  /* Note that the file copying succeeded. */

  param->m_temp_file_copied = 1;
  headas_chat(3,"%s info: Input file %s copied as temporary output file %s.\n", TOOLNAME, param->m_in_file, param->m_temp_file);

  /* Move to attitude extension in temporary output file. */
  
  fits_movnam_hdu(fp, ANY_HDU, param->m_att_ext, 0, &status);
  sprintf(tempstring, "finding %s extension of", param->m_att_ext);
  reportFITSError(status, tempstring, param->m_temp_file, param->m_debug);
  if(status)
    return status;
  
  /* Find input attitude column. */
  
  fits_get_colnum(fp, CASEINSEN, param->m_in_att_col, &param->m_in_col_number, &status);
  sprintf(tempstring, "finding input column %s of", param->m_in_att_col);
  reportFITSError(status, tempstring, param->m_temp_file, param->m_debug);
  if(status)
   return status;
  
  /* Count the input column. */

  param->m_n_fits_cols = 1;  
  
  /* Get the input attitude column info. */
  
  fits_get_coltype(fp, param->m_in_col_number, &incoltype, &in_n_elements, &longvalue, &status);
  sprintf(tempstring, "obtaining info about column %s of", param->m_in_att_col);
  reportFITSError(status, tempstring, param->m_temp_file, param->m_debug);
  if(status)
    return status;
  
  /* Quit if input attitude format is not what is needed. */
  
  if(in_n_elements != param->m_in_n_elements)
    {
      headas_chat(0, "%s error: Input attitude column %s has vectors of %ld elements, but the attitude format %s requires vectors with %d elements.\n", TOOLNAME, param->m_in_att_col, in_n_elements, param->m_in_att_format, param->m_in_n_elements);
      return 1;
    }
  
  if(incoltype != TFLOAT && incoltype != TDOUBLE)
    {
      headas_chat(0, "%s error: Input attitude column %s is not of a floating-point type.\n", TOOLNAME, param->m_in_att_col);
      return 1;
    }
  
  /* Find output attitude columns and check their format. Add any missing columns. */
  
  for(att_format = 0; att_format < N_ATT_FORMATS; att_format++)
    {
      if(param->m_att_io[att_format] != e_IO_OUTPUT)
	continue;

      /* Find the output column. */

      fits_get_colnum(fp, CASEINSEN, param->m_col_names[att_format], &(param->m_col_numbers[att_format]), &status);
      if(status == COL_NOT_FOUND)
	{
	  status = 0;
	  param->m_col_numbers[att_format] = -1;
	}
      sprintf(tempstring, "finding output column %s of", param->m_col_names[att_format]);
      reportFITSError(status, tempstring, param->m_temp_file, param->m_debug);
      if(status)
	return status;
      
      /* Count the output column. */
      
      param->m_n_fits_cols++;

      /* Check existing column for format/type or make the column. */
      
      if(param->m_col_numbers[att_format] >= 0)
	{
	  /* Get output attitude column info. */
	  
	  fits_get_coltype(fp, param->m_col_numbers[att_format], &outcoltype, &out_n_elements, &longvalue, &status);
	  sprintf(tempstring, "obtaining info about column %s of", param->m_col_names[att_format]);
	  reportFITSError(status, tempstring, param->m_temp_file, param->m_debug);
	  if(status)
	    return status;
	  
	  /* Check if output attitude format is floating-point and
	     with the correct number of components. */
	  
	  if(out_n_elements != param->m_n_elements[att_format]
	     || (outcoltype != TFLOAT && outcoltype != TDOUBLE) 
	     )
	    {
	      /* Quit if output attitude format is not what is needed. */
	      
	      headas_chat(0, "%s error: Output attitude column '%s' exists but is of the wrong vector type or has the wrong number of elements for attitude format '%s'. Delete the column from the input file, or choose a new outcol name.\n", TOOLNAME, param->m_col_names[att_format], "***fill me in***");
	      return 1;
	    }
	}
      else
	{
	  /* Add the missing output column. */

	  param->m_col_numbers[att_format] = param->m_in_col_number + 1;
	  sprintf(tempstring, "%dD", param->m_n_elements[att_format]);
	  fits_insert_col(fp, param->m_col_numbers[att_format], param->m_col_names[att_format], 
			  tempstring, &status);
	  sprintf(tempstring, "adding column %s to", param->m_col_names[att_format]);
	  reportFITSError(status, tempstring, param->m_temp_file, param->m_debug);
	  if(status)
	    return status;
	}
    }

  /* Set up the CFITSIO iterator columns. */

  param->m_fits_cols = (iteratorCol*) malloc(param->m_n_fits_cols*sizeof(iteratorCol));

  col = 0;
  for(att_format = 0; att_format < N_ATT_FORMATS; att_format++)
    {
      if(param->m_att_io[att_format] == e_IO_IGNORE)
	{
	  continue;
	}
      else if(param->m_att_io[att_format] == e_IO_INPUT)
	{
	  fits_iter_set_by_name(&(param->m_fits_cols[col]), fp,
				param->m_col_names[att_format], TDOUBLE, InputCol);
	  col++;
	}
      else if(param->m_att_io[att_format] == e_IO_OUTPUT)
	{
	  fits_iter_set_by_name(&(param->m_fits_cols[col]), fp,
				param->m_col_names[att_format], TDOUBLE, OutputCol);
	  col++;
	}
    }

  /* The output FITS file is ready for reading from the input column 
     and writing to the output column. */
  
  return 0;
}

/* ---------------------------------------------------------------------- */

int doWork
(
 Params* param, /* Pointer to parameters structure */
 fitsfile* fp /* Output file pointer */
 )
{
  /* Declare the runtime status. */
  
  int status = 0;

  /* Branch to run either in file mode or single point mode. */

  if(param->m_in_type == e_IN_FILE)
    {
      status = doWorkForFile(param, fp);
    }
  else if(param->m_in_type == e_IN_POINT)
    {
      status = doWorkForPoint(param);
    }

  return status;
} 

/* ---------------------------------------------------------------------- */

int doWorkForPoint
(
 Params* param
 )
{
  /* Convert the input object to the output objects. */

  if(param->m_in_format == e_AF_QUAT)
    {
      convertQuatToEuler(param->m_euler, param->m_quat);
      fixEulerAngles(param->m_euler);

      convertQuatToPointing(param->m_pointing, param->m_quat, param->m_align);
    }
  else if(param->m_in_format == e_AF_EULER)
    {
      /* Convert only to pointing angles here because the conversion
	 to a quat was done when initially reading the Euler
	 angles. */

      convertEulerToPointing(param->m_pointing, param->m_euler, param->m_align);
    }
  else if(param->m_in_format == e_AF_POINTING)
    {
      convertPointingToQuat(param->m_quat, param->m_pointing, param->m_align);

      convertPointingToEuler(param->m_euler, param->m_pointing, param->m_align);
      fixEulerAngles(param->m_euler);
    }

  /* Send the output attitude to the screen and to the output parameters. */

  printAttObjects(stdout, param->m_quat, param->m_euler, param->m_pointing);
  saveAttObjects(param->m_quat, param->m_euler, param->m_pointing);

  return 0;
}

/* ---------------------------------------------------------------------- */

/* Read the output file rows and convert the attitude in each row. */
int doWorkForFile
(
 Params* param, /* Pointer to parameters structure */
 fitsfile* fp /* Output file pointer */
 )
{
  /* Declare the runtime status and an error message string. */

  int status = 0;
  char errmsg[FLEN_VALUE] = "";

  /* Use the CFITSIO iterator to process the attitude file. */

  fits_iterate_data(param->m_n_fits_cols, param->m_fits_cols, 0L, 0L, convertAttitude, param, &status);

  if(status)
    {
      /* Display CFITSIO errors from the iterator if there are any. */

      fits_get_errstatus(status, errmsg);
      headas_chat(0, "%s error: %s\nCannot continue processing due to error", TOOLNAME, errmsg);
    }

  /* Indicate that the file processing succeeded. */
  param->m_temp_file_written = 1;

  return status;
}


/* ---------------------------------------------------------------------- */

/* Clean up. */
void finalize
(
 Params* param, /* Pointer to parameter structure */
 fitsfile* fp /* Output file pointer */
 )
{
  /* Declare a runtime status and enumerated attitude format. */

  int status = 0;
  AttFormat att_format = 0;

  /* Handle the temporary output file if it was made. */

  if(param->m_in_type == e_IN_FILE)
    {
      /* Write command-line parameters to output file if requested, and
       * close the file. */    
      
      if(fp != NULL)
	{
	  if(param->m_history)
	    {
	      HDpar_stamp(fp, 0, &status);
	      if(status)
		{
		  headas_chat(0, "%s error: Could not write task parameters to attitude header of %s.\n", TOOLNAME, param->m_out_file);
		  status = 0;
		}
	    }
	  
	  fits_write_chksum(fp, &status);
	  fits_close_file(fp, &status);
	}

      if(param->m_temp_file_written)
	{
	  /* The temporary output file was written normally, so rename the file as the output file. */
	  
	  rename(param->m_temp_file, param->m_out_file);
	  headas_chat(3,"%s info: Renamed the temporary output file %s as the output filename %s.\n", TOOLNAME, param->m_temp_file, param->m_out_file);
	  headas_chat(1,"%s info: Output file %s is complete.\n", TOOLNAME, param->m_out_file);
	}
      else if(param->m_temp_file_copied && !param->m_debug)
	{
	  /* There was a problem during the file processing, so the
	     output file is not in the expected post-processing state.
	     Delete the file so that it is not confused by the user as
	     a good output file. */

	  remove(param->m_temp_file);
	  headas_chat(1,"%s info: Output file %s was not created.\n", TOOLNAME, param->m_out_file);
	}
      else if(param->m_temp_file_copied && param->m_debug)
	{
	  /* There was a problem during the processing, so the output file
	   * is not in the expected post-processing state.  Leave the file
	   * if debug mode is enabled as that may be helpful for
	   * investigating the problem. */

	  headas_chat(1,"%s info: Incomplete temporary output file %s was written.\n", 
		  TOOLNAME, param->m_temp_file);
	}
    }

  /* Free memory. */

  freeString(param->m_in_file);
  freeString(param->m_out_file);
  freeString(param->m_temp_file);
  freeString(param->m_in_att_format);
  freeString(param->m_out_att_format);
  freeString(param->m_att_ext);
  freeString(param->m_in_att_col);
  freeString(param->m_out_att_col);
  freeString(param->m_align_file);

  if(param->m_col_names != NULL)
    {
      for(att_format = 0; att_format < N_ATT_FORMATS; att_format++)
	{
	  if(param->m_col_names[att_format] != NULL)
	    free(param->m_col_names[att_format]);
	}
      free(param->m_col_names);
    }

  if(param->m_fits_cols != NULL)
    free(param->m_fits_cols);
  if(param->m_col_numbers != NULL)
    free(param->m_col_numbers);
  if(param->m_att_io != NULL)
    free(param->m_att_io);
  if(param->m_n_elements != NULL)
    free(param->m_n_elements);
  if(param->m_quat != NULL)
    destroyQuat(param->m_quat);
  if(param->m_euler != NULL)
    destroyEuler(param->m_euler);
  if(param->m_pointing != NULL)
    destroyPointing(param->m_pointing);
  if(param->m_align != NULL)
    destroyAlign(param->m_align);
  
  if(param != NULL)
    free(param);

  headas_chat(1, "%s info: Finished.\n", TOOLNAME);
}

/* ======================================================================*/

/* Revision log:
 * $Log: attconvert.c,v $
 * Revision 1.18  2016/03/28 18:45:15  rshill
 * Separated test of output code from fits_file_exists from
 * the test of cfitsio status. (A message was getting lost.)
 *
 * Revision 1.17  2015/10/20 20:47:46  rshill
 * Corrected bug in checking for output file same as input file.
 *
 * Revision 1.16  2015/01/07 23:46:29  rshill
 * Updated for big parameter edit.
 *
 * Revision 1.15  2014/01/29 22:10:13  treichar
 * Eliminated a minor memory leak
 *
 * Revision 1.14  2014/01/27 18:49:44  treichar
 * Added more comments
 *
 * Revision 1.13  2014/01/14 18:25:54  treichar
 * Handles zero quaternions in an input attitude file.
 *
 * Revision 1.12  2014/01/14 16:02:32  treichar
 * Now displays an error message if a zero quaternion is the input.
 *
 * Revision 1.11  2014/01/13 22:34:26  treichar
 * Added the Euler-->Euler conversion in Single Point Mode so that the output Euler angles match what conversions from quaternion or pointing angles to Euler angles would give instead of matching what the user inputs
 *
 * Revision 1.10  2013/12/18 19:49:27  treichar
 * Reorganized the source code.
 *
 * Revision 1.9  2013/12/18 16:35:39  treichar
 * Changed default chatter value to 1.
 *
 * Revision 1.8  2013/12/18 16:32:18  treichar
 * Implemented the chatter parameter.  Allows the output file to be the same as the input file.
 *
 * Revision 1.7  2013/12/10 00:05:39  treichar
 * Code clean-up. Changed input parameter string delimiter from space to comma for consistency with other list-oriented parameters.
 *
 * Revision 1.6  2013/12/05 19:53:16  treichar
 * Added capability to write multiple output attitude columns instead of only one.  Implemented buffered writing.
 *
 * Revision 1.5  2013/11/25 22:16:34  treichar
 * Suppressed most error messages from reading the alignfile as the wrong type (alignment or TelDef).
 *
 * Revision 1.4  2013/11/21 20:22:23  treichar
 * Added the capability to convert to/from pointing angles (ra, dec, roll) in addition to the quaternion and Euler angle formats.  Added the
 * capability to convert a single attitude point in addition to processing an attitude file.
 *
 * Revision 1.3  2013/07/22 19:28:16  treichar
 * Put functions in standard order and added more comments.
 *
 * Revision 1.2  2013/07/19 15:33:25  treichar
 * Added null pointer check when freeing strings.
 *
 * Revision 1.1  2013/03/14 19:57:10  treichar
 * Added new attconvert task, which converts attitude from one format to another from an attitude FITS file.
 *
 * Revision 1.1  2013/03/14 19:45:30  treichar
 * New attconvert task converts attitude between quaternion and Euler angle formats in an attitude FITS file by reading the one format and writing the
 * other as a new column.
 *
 */
