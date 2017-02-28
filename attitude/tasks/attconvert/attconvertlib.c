/* Auxiliary functions for the attconvert task. 
   $Date: 2015/08/04 18:19:37 $
*/

#include "attconvertlib.h"

int convertAttitude
(
 long total_rows, 
 long offset, 
 long first_row, 
 long n_rows, 
 int n_cols,
 iteratorCol* fits_cols,
 void* void_param
 )
{
  /* Recast the parameters structure and define a row and a column number. */

  Params* param = (Params*) void_param;
  long row = 0;
  int col = 0;

  /* Declare a angle units conversion factor */

  const double rad_per_deg = M_PI/180.;

  /* Allocate the rotation objects. */

  QUAT* att_q = allocateQuat();
  EULER* att_e = allocateEuler();
  POINTING* att_p = allocatePointing();

  /* Initialize the FITS arrays and indices for each rotation object. */

  double* fits_q = param->m_fits_quat;
  double* fits_e = param->m_fits_euler;
  double* fits_p = param->m_fits_pointing;
  long index_q = 0;
  long index_e = 0;
  long index_p = 0;

  /* Declare the input and output formats. */

  AttFormat in_format = e_AF_UNKNOWN;
  AttFormat out_format = e_AF_UNKNOWN;

  /* Connect the iterator columns to the data arrays. */

  col = 0;
  for(in_format = 0; in_format < N_ATT_FORMATS; in_format++)
    {
      if(param->m_att_io[in_format] == e_IO_IGNORE)
	continue;
      else if(param->m_att_io[in_format] == e_IO_INPUT || param->m_att_io[in_format] == e_IO_OUTPUT)
	{
	  if(in_format == e_AF_QUAT)
	    {
	      fits_q = (double*) fits_iter_get_array(&(param->m_fits_cols[col]));
	      fits_q[0] = DOUBLENULLVALUE;
	    }
	  else if(in_format == e_AF_EULER)
	    {
	      fits_e = (double*) fits_iter_get_array(&(param->m_fits_cols[col]));
	      fits_e[0] = DOUBLENULLVALUE;
	    }
	  else if(in_format == e_AF_POINTING)
	    {
	      fits_p = (double*) fits_iter_get_array(&(param->m_fits_cols[col]));
	      fits_p[0] = DOUBLENULLVALUE;
	    }
	  col++;
	}
    }
  
  /* Loop over the attitude extension's table rows and convert the attitude. */

  headas_chat(3, "info: Processing %d rows in cfitsio iterator starting with row %d\n", n_rows, first_row);

  for(row = 1; row <= n_rows; row++)
    {
      /* Set the 1D array indices of the iterator data arrays to the
	 first component of each attitude format. 

	 The CFITSIO iterator framework flattens what could have been
	 a 2D array like
	 hypothetical_fits_q[row=1..n_rows][component=0..n_components-1]
	 into a 1D array like
	 fits_q[index=0..1+n_rows*n_components-1]. For example, the
	 indices of the fits_q array for quats are:

	 meaning   :  indices of fits_q[index]
	 ================================================================
	 null value:  0
	 row 1     :  1,2,3
	 row 2     :  4,5,6
	 ...
	 row r     :  1 + (r - 1)*4 + 0, 1 + (r - 1)*4 + 1, 1 + (r - 1)*4 + 2
	 ...
      */
      
      index_q = 1 + (row - 1) * param->m_n_elements[e_AF_QUAT];
      index_e = 1 + (row - 1) * param->m_n_elements[e_AF_EULER];
      index_p = 1 + (row - 1) * param->m_n_elements[e_AF_POINTING];

      in_format = param->m_in_format;

      /* Put the input attitude into the attitude objects. */
      
      if(in_format == e_AF_QUAT)
	{
	  if(fabs(fits_q[index_q]) < ZEROQTOL &&
	     fabs(fits_q[index_q + 1]) < ZEROQTOL &&
	     fabs(fits_q[index_q + 2]) < ZEROQTOL &&
	     fabs(fits_q[index_q + 3]) < ZEROQTOL	     )
	    {
	      /* Set the quaternion components to zero. */
	      
	      setNullQuat(att_q);
	    }
	  else
	    {
	      /* Load the quaternion struct with the components. Force a
		 renormalization to ensure conversions are done with a
		 unit quaternion. */
	      
	      setQuat(att_q, fits_q[index_q], fits_q[index_q + 1], 
		      fits_q[index_q + 2], fits_q[index_q + 3]);
	      forceRenormalizeQuat(att_q);
	    }
	}
      else if(in_format == e_AF_EULER)
	{
	  /* Convert from degrees in attitude file to radians in EULER structure. */
	  
	  setEulerAngles(att_e, fits_e[index_e]*rad_per_deg, 
			 fits_e[index_e + 1]*rad_per_deg, fits_e[index_e + 2]*rad_per_deg);
	}
      else if(in_format == e_AF_POINTING)
	{
	  /* No deg<->radians conversion needed for the pointing angles. */

	  setPointing(att_p, fits_p[index_p], fits_p[index_p + 1], fits_p[index_p + 2]);
	}
      
      /* Loop over the attitude formats again and work only on the output formats. */
      
      for(out_format = 0; out_format < N_ATT_FORMATS; out_format++)
	{
	  if(param->m_att_io[out_format] != e_IO_OUTPUT)
	    continue;
	  
	  /* Do the attitude conversions. */
	  
	  if(in_format == e_AF_QUAT)
	    {
	      if(out_format == e_AF_EULER)
		{
		  if(isQuatNull(att_q))
		    {
		      /* Convert a null quat to null Euler angles. */

		      att_e->phi = DOUBLENULLVALUE;
		      att_e->theta = DOUBLENULLVALUE;
		      att_e->psi = DOUBLENULLVALUE;
		    }
		  else
		    {
		      /* Convert a unit quat to Euler angles. */

		      convertQuatToEuler(att_e, att_q);
		      fixEulerAngles(att_e);
		    }
		}
	      else if(out_format == e_AF_POINTING)
		{
		  if(isQuatNull(att_q))
		    {
		      /* Convert the zero quat to null pointing angles. */
		      
		      att_p->ra = DOUBLENULLVALUE;
		      att_p->dec = DOUBLENULLVALUE;
		      att_p->roll = DOUBLENULLVALUE;
		    }
		  else
		    {
		      /* Convert a unit quat to pointing angles. */

		      convertQuatToPointing(att_p, att_q, param->m_align);
		    }
		}
	    }
	  else if(in_format == e_AF_EULER)
	    {
	      if(out_format == e_AF_QUAT)
		{
		  convertEulerToQuat(att_q, att_e);
		}
	      else if(out_format == e_AF_POINTING)
		{
		  convertEulerToPointing(att_p, att_e, param->m_align);
		}
	    }
	  else if(in_format == e_AF_POINTING)
	    {
	      if(out_format == e_AF_QUAT)
		{
		  convertPointingToQuat(att_q, att_p, param->m_align);
		}
	      else if(out_format == e_AF_EULER)
		{
		  convertPointingToEuler(att_e, att_p, param->m_align);
		  fixEulerAngles(att_e);
		}
	    }
	  
	  /* Copy the converted attitude to the iterator data arrays to be written
	     to the attitude file. */
	  
	  if(out_format == e_AF_QUAT)
	    {
	      fits_q[index_q] = att_q->p[0];
	      fits_q[index_q + 1] = att_q->p[1];
	      fits_q[index_q + 2] = att_q->p[2];
	      fits_q[index_q + 3] = att_q->p[3];
	    }
	  else if(out_format == e_AF_EULER)
	    {
	      fits_e[index_e] = (att_e->phi == DOUBLENULLVALUE ? DOUBLENULLVALUE : att_e->phi/rad_per_deg);
	      fits_e[index_e + 1] = (att_e->theta == DOUBLENULLVALUE ? DOUBLENULLVALUE : att_e->theta/rad_per_deg);
	      fits_e[index_e + 2] = (att_e->psi == DOUBLENULLVALUE ? DOUBLENULLVALUE : att_e->psi/rad_per_deg);
	    }
	  else if(out_format == e_AF_POINTING)
	    {
	      fits_p[index_p] = (att_p->ra == DOUBLENULLVALUE ? DOUBLENULLVALUE : att_p->ra);
	      fits_p[index_p + 1] = (att_p->dec == DOUBLENULLVALUE ? DOUBLENULLVALUE : att_p->dec);
	      fits_p[index_p + 2] = (att_p->roll == DOUBLENULLVALUE ? DOUBLENULLVALUE : att_p->roll);
	    }
	} /* end loop over output formats */
    } /* end loop over rows */

  /* Deallocate the attitude objects. */

  destroyQuat(att_q);
  destroyEuler(att_e);
  destroyPointing(att_p);

  return 0;
}
		    
/* ---------------------------------------------------------------------- */

/* Put the Euler angles into their valid ranges.
 * E.g., change phi from -1.5pi to +0.5pi. */
void fixEulerAngles
(
 EULER* e /* Pointer to Euler angle trio */
 )
{
  while(e->phi < 0.)
    e->phi += 2.0*M_PI;
  while(e->phi >= 2*M_PI)
    e->phi -= 2.0*M_PI;

  while(e->psi < 0.)
    e->psi += 2.0*M_PI;
  while(e->psi >= 2*M_PI)
    e->psi -= 2.0*M_PI;
}

/* ---------------------------------------------------------------------- */

void printAttObjects
(
 FILE* stream, 
 QUAT* q, 
 EULER* e,
 POINTING* p
 )
{
  /* Print the output attitude to the stream. */

  fprintf(stream, "Quaternion     : ");
  printQuat(q, stream);
  fprintf(stream, "\n");

  fprintf(stream, "Euler angles   : ");
  printEulerDeg(e, stream);
  fprintf(stream, "\n");

  fprintf(stream, "Pointing angles: ");
  printPointing(p, stream, 1);
  fprintf(stream, "\n");
}

/* ---------------------------------------------------------------------- */

void saveAttObjects(QUAT* q, EULER* e, POINTING* p)
{
  /* Declare an APE status and the angle units conversion factor. */

  int status = eOK;
  double rad2deg = 180./M_PI;
  
  /* Write the attitude components to the output parameters. */

  status = ape_trad_set_double("quatx", q->p[0]);
  status = ape_trad_set_double("quaty", q->p[1]);
  status = ape_trad_set_double("quatz", q->p[2]);
  status = ape_trad_set_double("quatr", q->p[3]);

  status = ape_trad_set_double("eulerphi", e->phi*rad2deg);
  status = ape_trad_set_double("eulertheta", e->theta*rad2deg);
  status = ape_trad_set_double("eulerpsi", e->psi*rad2deg);

  status = ape_trad_set_double("outra", p->ra);
  status = ape_trad_set_double("outdec", p->dec);
  status = ape_trad_set_double("outroll", p->roll);

  if(status != eOK)
    headas_chat(0, "%s error: Could not save output quaternion, Euler angles, and pointing to output parameters.\n", TOOLNAME);
}

/* ---------------------------------------------------------------------- */

/* Return the enum AttFormat value for a attitude format name, or return
 * e_AF_UNKNOWN if the name is invalid. */
AttFormat getAttFormatNumber
(
 char* att_format_name /* Attitude format string */
 )
{
  if(!strcasecmp(att_format_name, "QUAT"))
    return e_AF_QUAT;
  else if(!strcasecmp(att_format_name, "EULER"))
    return e_AF_EULER;
  else if(!strcasecmp(att_format_name, "POINTING"))
    return e_AF_POINTING;
  else
    return e_AF_UNKNOWN;
}

/* ---------------------------------------------------------------------- */

/* Return the number of elements in the desired attitude format. */
int getAttFormatNumberOfElements /* Returns number of elements */
(
 AttFormat att_format /* Enumerated attitude format */
 )
{
  if(att_format < 0 || att_format >= N_ATT_FORMATS)
    return 0;

  if(att_format == e_AF_QUAT)
    return 4;
  else if(att_format == e_AF_EULER)
    return 3;
  else if(att_format == e_AF_POINTING)
    return 3;
  else
    return 0;
}

/* ---------------------------------------------------------------------- */

ALIGN* readAlignmentInfo(char* file, int debug)
{
  /* Declare the runtime status and initialize pointers to the TelDef
   * and Align structures. Initialize the alignment file type. */

  int status = 0;
  TELDEF2* teldef = NULL;
  ALIGN* align = NULL;
  AlignFileType aligntype = e_FT_UNKNOWN;

  /* If the filename is NONE or blank, then use the identity alignment matrix
   * and default roll definition. */

  if(!strcasecmp(file, "NONE") || file[0] == '\0')
    {
      align = allocateDefaultAlign();

      return align;
    }

  /* If the filename is STANDARD, then use the identity alignment
     matrix with roll offset = 0 and the roll sign flipped from the
     default to -1. */

  if(!strcasecmp(file, "STANDARD"))
    {
      align = allocateDefaultAlign();
      align->roll_sign = -1;

      return align;
    }

  /* Determine if alignfile is an alignment file or TelDef file. */
  
  aligntype = getAlignFileType(file, debug);

  /* Give up if the file could not be found. */

  if(aligntype == e_FT_NOT_FOUND)
    return NULL;

  /* Try to read the file as a TelDef (v0.1-0.2 specification) file
   * with either ALIGN_Mij (v0.1) or ccc_Mij (v0.2) keywords, where
   * ccc is the penultimate coordinate system name. */
  
  if(aligntype == e_FT_TELDEF || aligntype == e_FT_UNKNOWN)
    {
      status = readTelDef2(file, &teldef);
      
      /* A zero CFITSIO status means the TelDef file was successfully
	 read. A nonzero status means it could not be read as a TelDef
	 File.  In the latter case, read it as an alignment file. */
      
      if(status == 0)
	{
	  /* Check if the TelDef file has a to-sky alignment matrix, or more
	   * technically, if the skyattparam structure is allocated for the
	   * final coordinate transformation. If that structure is allocated,
	   * the align structure is in it. */
	  
	  if(teldef == NULL || teldef->skyattparam == NULL ||
	     teldef->skyattparam[teldef->n_coordsys - 2] == NULL || 
	     teldef->skyattparam[teldef->n_coordsys - 2]->alignment == NULL)
	    {
	      headas_chat(0, "%s error: Alignment matrix or roll definition not found in TelDef file '%s'\n", TOOLNAME, file);
	      destroyTelDef2(teldef);
	      return NULL;
	    }
	  
	  /* Copy the ALIGN structure from the TELDEF2 structure so that the ALIGN
	   * struct can be returned and the TELDEF2 struct can be deleted. */
	  
	  align = allocateDefaultAlign();
	  copyAlign(align, teldef->skyattparam[teldef->n_coordsys - 2]->alignment);
	  destroyTelDef2(teldef);
	  
	  return align;
	}
    }
  
  if(aligntype == e_FT_ALIGN || aligntype == e_FT_UNKNOWN)
    {
      /* Try to read the file as an alignment file with ALIGNMij keywords.
       * If this succeeds, use the returned alignment structure. If it fails,
       * a NULL pointer will be returned. */
      
      align = readAlign(file);

      return align;
    }

  return align;
}

/* ---------------------------------------------------------------------- */

/* Determine the type of file giving the alignment and roll definitions. */
AlignFileType getAlignFileType
(
 char* filename, /* Name of file */
 int debug /* debug mode */
 )
{
  /* Declare the CFITSIO status, file pointer, and alignment file type. */

  int status = 0;
  fitsfile* fp = NULL;
  AlignFileType aligntype = e_FT_NOT_FOUND;

  /* Declare strings for the filetype, keyword name, and action. */

  char filetype[FLEN_VALUE] = "";
  char keyname[FLEN_VALUE] = "CCNM0001";
  char action[FLEN_VALUE] = "";

  /* Open the alignment file and automatically move to primary header. */

  fits_open_file(&fp, filename, READONLY, &status);
  reportFITSError(status, "opening", filename, debug);
  if(status)
    return e_FT_NOT_FOUND;

  /* Read the CCNM0001 keyword value to determine the file type. */
  
  fits_read_key_str(fp, keyname, filetype, NULL, &status);
  sprintf(action, "reading keyword %s", keyname);
  reportFITSError(status, action, filename, debug);
  if(!status && !strcasecmp(filetype, "ALIGNMENT"))
    {
      aligntype = e_FT_ALIGN;
    }
  else if(!status && !strcasecmp(filetype, "TELDEF"))
    {
      aligntype = e_FT_TELDEF;
    }
  else
    {
      headas_chat(1, "%s warning: Cannot determine if alignfile '%s' is an alignment file or TelDef file, so a guess will be made.  If the guess is wrong but the file can be successfully read, some harmless error messages may appear anyway.\n", TOOLNAME, filename);
      aligntype = e_FT_UNKNOWN;
      status = 0;
    }

  fits_close_file(fp, &status);

  return aligntype;
}

/* ---------------------------------------------------------------------- */

/* Parse the input parameter and determine its type (filename or
 * rotation/pointing object). If it's a rotation/pointing object,
 * determine its components. */
InputType parseInput
(
 char* input, 
 int n_components, 
 double* object
 )
{
  /* Declare a loop counter and input type. */

  long i = 0;
  InputType type = e_IN_UNKNOWN;
  
  /* Declare the arguments for the list parsing function expand_item_list(). */

  char delimiter = ',';
  char** pieces = NULL;
  int n_items = 0;
  int trim = 1;
  int skip_empty = 1;
  int guard_paren = 0;
  int status = 0;

  /* Check for positive string length. */

  if(strlen(input) == 0)
    return e_IN_UNKNOWN;

  /* Try to split the string on the delimiter. */

  pieces = expand_item_list(input, &n_items, delimiter, 
			    trim, skip_empty, guard_paren, &status);

  /* Validate the list. */

  if(status != 0)
    {
      /* pieces wasn't allocated or something else went wrong. */

      type = e_IN_UNKNOWN;
    }
  else if(n_items == 1)
    {
      /* No components were delimited, so treat the string as a filename. */

      type = e_IN_FILE;
    }
  else if(n_items > 1 && n_items != n_components)
    {
      /* Wrong number of components for the prescribed object. */
      
      type = e_IN_UNKNOWN;
    }
  else 
    {
      /* The number of components in the string matches the expected number,
       * so convert the components to floating-point numbers. */
      
      type = e_IN_POINT;

      for(i = 0; i < n_items; i++)
	{
	  object[i] = atof(pieces[i]);
	}
    }

  /* Free the array of components if it was allocated. */

  if(pieces != NULL)
    {
      free(pieces);
    }

  return type;
}

/* ---------------------------------------------------------------------- */

int parseOutputAttFormats(char* format_string, char* column_string, AttInputOutput* io, int* n_elements, char** col_names)
{
  
  /* Declare variables for list validity, attitude format, and maximum
   * number of list items. */
  
  int are_lists_valid = 1;
  AttFormat format = e_AF_UNKNOWN;
  int max_items = N_ATT_FORMATS - 2;
  
  /* Declare arrays and numbers of elements for the list parsing. */

  int n_formats = 0;
  int n_columns = 0;
  char** format_pieces = NULL;
  char** column_pieces = NULL;

  /* Declare the arguments to the expand_item_list function(). */

  char delimiter = ',';
  int trim = 1;
  int skip_empty = 1;
  int guard_paren = 0;
  int format_status = 0;
  int column_status = 0;

  /* Declare loop counters. */

  int item = 0;
  int item2 = 0;

  /* Parse the lists of output attitude formats and column names. */

  format_pieces = expand_item_list(format_string, &n_formats, delimiter, 
				   trim, skip_empty, guard_paren, &format_status);
  
  column_pieces = expand_item_list(column_string, &n_columns, delimiter, 
				   trim, skip_empty, guard_paren, &column_status);
  

  /* Verify the parsing, list lengths, and validity of the formats. */

  if(format_status != 0)
    {
      headas_chat(0, "%s error: Could not parse the outformat list '%s'.\n", TOOLNAME, format_string);
      are_lists_valid = 0;
    }

  if(column_status != 0)
    {
      headas_chat(0, "%s error: Could not parse the outcol list '%s'.\n", TOOLNAME, column_string);
      are_lists_valid = 0;
    }

  if(n_formats > max_items)
    {
      headas_chat(0, "%s error: Too many items in outformat list '%s'. The maximum number of items is %d.\n", TOOLNAME, format_string, max_items);
      are_lists_valid = 0;
    }
  
  if(n_formats != n_columns)
    {
      headas_chat(0, "%s error: The number of column names in outcol = '%s' does not match the number of formats in outformat = '%s'.\n", TOOLNAME, column_string, format_string);
      are_lists_valid = 0;
    }

  if(!are_lists_valid)
    return are_lists_valid;

  for(item = 0; item < n_formats; item++)
    {
      for(item2 = item + 1; item2 < n_formats; item2++)
	{
	  if(!strcasecmp(format_pieces[item], format_pieces[item2]))
	    {
	      headas_chat(0, "%s error: The case-insensitive output format '%s' is duplicated in the outformat parameter.\n", TOOLNAME, format_pieces[item]);
	      are_lists_valid = 0;
	    }

	  if(!strcasecmp(column_pieces[item], column_pieces[item2]))
	    {
	      headas_chat(0, "%s error: The case-insensitive output column name '%s' is duplicated in the outcol parameter.\n", TOOLNAME, column_pieces[item]);
	      are_lists_valid = 0;
	    }
	}
    }

  if(!are_lists_valid)
    return are_lists_valid;
  

  /* Set the input/output/ignore types of each of the possible
     attitude formats, and link the column name with each. */

  for(item = 0; item < n_formats; item++)
    {
      format = getAttFormatNumber(format_pieces[item]);

      if(format == e_AF_UNKNOWN)
	{
	  headas_chat(0, "%s error: Cannot understand output format '%s' in outformat parameter.\n", TOOLNAME, format_pieces[item]);
	  return 0;
	}
      else if(io[format] == e_IO_INPUT)
	{
	  headas_chat(0, "%s error: Output format '%s' cannot be the same as the input format.\n", TOOLNAME, format_pieces[item]);
	  return 0;
	}
      else
	{
	  io[format] = e_IO_OUTPUT;
	  n_elements[format] = getAttFormatNumberOfElements(format);
	  strcpy(col_names[format], column_pieces[item]);
	}
    }
  
  /* Free the pieces arrays. */

  if(format_pieces != NULL)
    free(format_pieces);
  if(column_pieces != NULL)
    free(column_pieces);

  return are_lists_valid;
}

/* ---------------------------------------------------------------------- */

/* Determine if the filenames refer to the same file.  Returns -1 is
   either file does not exist, 0 if both files exist separately, and 1
   if the filenames refer to the same, existing file. */
int areFilenamesSameFile
(
 char* filename1, /* One filename */
 char* filename2  /* Another filename */
 ) 
{
    struct stat stat1, stat2;
    if(stat(filename1, &stat1) < 0) return -1;
    if(stat(filename2, &stat2) < 0) return -1;
    return (stat1.st_dev == stat2.st_dev) && (stat1.st_ino == stat2.st_ino);
}

/* ---------------------------------------------------------------------- */

/* Free string if it was used. */
void freeString
(
 char* string /* String to be freed. */
 )
{
  if(string != NULL)
    free(string);
}

/* ---------------------------------------------------------------------- */
/* Report FITS-related errors. */
void reportFITSError
(
 int status, /* CFITSIO status */
 char* doing, /* Present participle description of action just performed */
 char* filename, /* Output file name */
 int debug /* Debug mode on (1) or off (0) */
 ) 
{
  /* Display successes only in debug mode. */

  if(!status) 
    {
      if(debug)
	headas_chat(0, "%s debug: FITSIO success while %s file %s.\n", TOOLNAME, doing, filename);

      return;
    }
  
  /* Always display failures. */

  headas_chat(0, "%s error: FITSIO error while %s file %s:\n", TOOLNAME, doing, filename);
  fits_report_error(stdout,status);
}

/* ---------------------------------------------------------------------- */

/* Report error reading parameters from par file. */
void reportParError
(
 char* parname, /* Name of parameter */
 int status /* APE status */
 )
{
  if(status == eOK)
    return;
  
  headas_chat(0, "%s error: Problem reading parameter %s. (APE status: %d)\n", TOOLNAME, parname, status);
  return;
}

/* ======================================================================== */

/* Revision log:
 * $Log: attconvertlib.c,v $
 * Revision 1.6  2015/08/04 18:19:37  rshill
 * Added a message to iterator work function.
 *
 * Revision 1.5  2015/01/07 23:46:29  rshill
 * Updated for big parameter edit.
 *
 * Revision 1.4  2014/01/27 18:49:44  treichar
 * Added more comments
 *
 * Revision 1.3  2014/01/14 18:25:54  treichar
 * Handles zero quaternions in an input attitude file.
 *
 * Revision 1.2  2014/01/13 22:34:26  treichar
 * Added the Euler-->Euler conversion in Single Point Mode so that the output Euler angles match what conversions from quaternion or pointing angles to Euler angles would give instead of matching what the user inputs
 *
 * Revision 1.1  2013/12/18 19:49:27  treichar
 * Reorganized the source code.
 *
 */

