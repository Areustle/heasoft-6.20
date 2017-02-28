/* Params structure for holding command-line parameters and other
 * useful quantities for the attconvert task. 
 * $Date: 2014/01/27 18:49:44 $
 */

#ifndef TOOL_ATTCONVERT_ATTCONVERTLIB_H
#define TOOL_ATTCONVERT_ATTCONVERTLIB_H

#include "headas.h"
#define TOOLSUB attconvert
#include "coordfits2.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "pil.h"
#include "sys/stat.h"
#include "float.h"
#include "math.h"
#include "string.h"
#include <stdio.h>

#define TOOLNAME "attconvert"

/* ======================================================================== */
/* Data structure definitions */

/* Define a tolerance for zero quaternion components. */
#define ZEROQTOL FLT_MIN

/* Define the input (IN) type and execution mode */
typedef enum 
{
  e_IN_UNKNOWN, /* Unknown input type */
  e_IN_FILE, /* Input is a filename. (FITS file mode) */
  e_IN_POINT /* Input is a list of components of an attitude object. (Single point mode) */
} InputType;

/* Define the supported attitude formats (AF). */
#define N_ATT_FORMATS 4
typedef enum 
  {
    e_AF_UNKNOWN, /* Unknown format */
    e_AF_QUAT,    /* Quaternion */
    e_AF_EULER,   /* Euler angles */
    e_AF_POINTING /* Pointing angles */
  } AttFormat;
#define MAX_COMPONENTS 4  /* The largest number of components of the attitude formats
			     (4 for quaternions). */

/* Define the alignment file types (FT). */
typedef enum 
  {
    e_FT_NOT_FOUND, /* Cannot find the alignment file. */
    e_FT_UNKNOWN,   /* Cannot determine the alignment file type. */
    e_FT_ALIGN,     /* alignfile refers to an alignment file. */
    e_FT_TELDEF     /* alignfile refers to a TelDef file. */
  } AlignFileType;

/* Define the read/write options for an attitude format. */
typedef enum 
  {
    e_IO_IGNORE, /* Ignore this format. */
    e_IO_INPUT,  /* Use this format as input. */
    e_IO_OUTPUT  /* Use this format as output. */
  } AttInputOutput;

/* Define a structure holding command-line parameters and associated useful info. */
typedef struct {

  /* Input and file parameters. */

  char* m_input; /* Input parameter */
  InputType m_in_type; /* Input type (FITS file or single point) */
  char* m_in_file; /* Input filename */
  char* m_out_file; /* Output filename */
  char* m_temp_file; /* Temporary output filename */
  char* m_align_file; /* Alignment filename */
  int m_are_files_same; /* Flag if two filenames are the same file */
  int m_temp_file_copied; /* Flag if temp file was copied. */
  int m_temp_file_written; /* Flag if temp file was written. */
  char** m_col_names; /* m_col_names[att_format] 1D string array */
  int* m_col_numbers; /* m_col_numbers[att_format] 1D array */
  char* m_att_ext; /* Attitude extension name */
  char* m_in_att_col; /* Input attitude column name */
  char* m_out_att_col; /* Output attitude column names string */
  int m_in_col_number; /* Input attitude column number */

  /* Attitude format info. */
  
  int m_in_format; /* Input attitude format */
  char* m_in_att_format; /* Input attitude format name */
  char* m_out_att_format; /* Output attitude format names */
  int m_in_n_elements; /* Input attitude number of elements */
  AttInputOutput* m_att_io; /* m_att_io[att_format] array of I/O types */
  int* m_n_elements; /* m_n_elements[att_format] array of number of elements */

  /* Attitude objects and alignment structure */

  QUAT* m_quat; /* Quaternion */
  EULER* m_euler; /* Euler angle trio (radians) */
  POINTING* m_pointing; /* Pointing angle trio (degress) */
  ALIGN* m_align; /* Alignment structure incl. roll definition */

  /* Generic tool parameters */

  char m_clobber; /* Clobber parameter */
  char m_debug; /* Debug parameter */
  char m_history; /* History parameter */

  /* CFITSIO iterator arrays. */

  int m_n_fits_cols; /* Number of m_fits_col elements */
  iteratorCol* m_fits_cols; /* m_fits_col[att_format] 1D array of CFITSIO iterator column structures */
  double* m_fits_quat;     /* m_fits_quat[row] FITS quaternion data */
  double* m_fits_euler;    /* m_fits_euler[row] FITS Euler data */
  double* m_fits_pointing; /* m_fits_pointing[row] FITS pointing data */

} Params;

/* ======================================================================== */

/* CFITSIO iterator driver function. Converts attitude from the input
   column into the output columns' formats. */
int convertAttitude
(
 long total_rows, 
 long offset, 
 long first_row, 
 long n_rows, 
 int n_cols,
 iteratorCol *fits_cols,
 void* void_param
 );

/* Put the Euler angles into their valid ranges.
 * E.g., change phi from -1.5pi to +0.5pi. */
void fixEulerAngles
(
 EULER* e /* Pointer to Euler angle trio */
 );

/* Print attitude objects to a stream. */
void printAttObjects
(
 FILE* stream,  /* Output stream */
 QUAT* q,       /* Attitude quaternion */
 EULER* e,      /* Attitude Euler angles */
 POINTING* p    /* Attitude pointing angles */
 );

/* Save the attitude objects to output parameters. */
void saveAttObjects
(
 QUAT* q,       /* Attitude quaternion */
 EULER* e,      /* Attitude Euler angles */
 POINTING* p    /* Attitude pointing angles */
 );

/* Return the enum AttFormat value for a attitude format name. */
AttFormat getAttFormatNumber
(
 char* att_format_name /* Attitude format string */
 );

/* Return the number of elements in the desired attitude format. */
int getAttFormatNumberOfElements /* Returns number of elements */
(
 AttFormat att_format /* Enumerated attitude format */
 );

/* Read the alignment matrix and roll definition from an alignment or
   TelDef file. */
ALIGN* readAlignmentInfo
(
 char* file, /* Alignment or TelDef filename */
 int debug   /* Debug parameter (suppress output if set to 0) */
 );

/* Determine the type of file giving the alignment and roll definitions. */
AlignFileType getAlignFileType
(
 char* filename, /* alignfile name */
 int debug       /* debug mode */
 );

/* Parse the input parameter and return the input type (file or
   attitude components).  Return the attitude components and number of
   components if the input parameter contains them. */
InputType parseInput
(
 char* input, /* input parameter string */
 int n_components, /* Number of components of input attitude */
 double* object /* Attitude components */
 );

int parseOutputAttFormats(char* format_string, char* column_string, AttInputOutput* io, int* n_elements, char** col_names);

/* Determine if the filenames refer to the same file.  Returns -1 is
   either file does not exist, 0 if both files exist separately, and 1
   if the filenames refer to the same, existing file. */
int areFilenamesSameFile
(
 char* filename1, /* One filename */
 char* filename2  /* Another filename */
 );

/* Free a string if it is non-NULL. */
void freeString
(
 char* string /* String to be freed. */
 );

/* Report FITS-related errors. */
void reportFITSError
(
 int status, /* CFITSIO status */
 char* doing, /* Present participle description of action just performed */
 char* filename, /* Output file name */
 int debug /* Debug mode on (1) or off (0) */
 );

/* Report error reading parameters from par file. */
void reportParError
(
 char* parname, /* Name of parameter */
 int status /* APE status */
 );

#endif /* TOOL_ATTCONVERT_ATTCONVERTLIB_H */

/* ======================================================================== */

/* Revision log:
 * $Log: attconvertlib.h,v $
 * Revision 1.4  2014/01/27 18:49:44  treichar
 * Added more comments
 *
 * Revision 1.3  2014/01/14 18:25:54  treichar
 * Handles zero quaternions in an input attitude file.
 *
 * Revision 1.2  2014/01/14 16:02:32  treichar
 * Now displays an error message if a zero quaternion is the input.
 *
 * Revision 1.1  2013/12/18 19:49:27  treichar
 * Reorganized the source code.
 *
 */


