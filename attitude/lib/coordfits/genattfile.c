#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "genattfile.h"
#include "attfile.h"
#include "attconvert.h"

/* #define DEBUG */

/* Create a GENATTFILE structure and open the attitude file. */
GENATTFILE* openGenAttFile /* Returns a new GENATTFILE struct */
(
 char* filename, /* Attitude filename */
 char* att_ext_name, /* Attitude extension name */
 char* att_col_name,  /* Attitude column name */
 int att_format, /* Attitude format */
 int interpolation_method, /* Interpolation method */
 double extrap_margin, /* Difference in allowed attitude and event time outside the attitude file time range. */
 double interp_margin, /* Difference in allowed attitude and event time within the attitude file time range. */
 int use_interp_margin /* Use time margin for interpolation? */
)
{
  GENATTFILE* file = NULL;
  int name_length = 0;
  int status = 0;
  long n_elements = 0;
  long col_bytes = 0;
  int type_code = 0;
  char comment[FLEN_COMMENT] = "";
  char tempstring[2000] = "";

  /* Allocate memory for the structure. */

  file = (GENATTFILE*) calloc(1, sizeof(GENATTFILE));
  file->af = (ATTFILE*) calloc(1, sizeof(ATTFILE));

  /* Allocate memory for file name, extension name, column name and
   * attitude format name and copy them into the structure. */

  name_length = strlen(filename) + 1;
  file->af->name = (char*) malloc(name_length * sizeof(char));
  strncpy(file->af->name, filename, name_length);

  name_length = strlen(att_ext_name) + 1;
  file->att_ext_name = (char*) malloc(name_length * sizeof(char));
  strncpy(file->att_ext_name, att_ext_name, name_length);
  
  name_length = strlen(att_col_name) + 1;
  file->att_col_name = (char*) malloc(name_length * sizeof(char));
  strncpy(file->att_col_name, att_col_name, name_length);

  file->att_format = att_format;

  name_length = FLEN_VALUE + 1;
  file->att_format_name = (char*) malloc(name_length * sizeof(char));
  getAttFormatName(att_format, file->att_format_name);

  file->att_n_elements = getAttFormatNElements(att_format);
  file->att_col_number = -1;
  
  file->e = allocateEuler();
  setEulerAngles(file->e, 0., 0., 0.);

  /* Open the attitude file. */

  file->af->fp = open_any_attfile(filename);

  /* Go to attitude extension. */

  fits_movnam_hdu(file->af->fp, BINARY_TBL, att_ext_name, 0, &status);
  sprintf(tempstring, "finding attitude extension %s in", att_ext_name);
  checkGenAttFileFITSErrors(status, tempstring, file);
  if(status)
    return NULL;

  /* Read the mission from the TELESCOP keyword. */
  
  fits_read_key_str(file->af->fp,"TELESCOP",file->af->mission,comment,&status);
  if(status==KEY_NO_EXIST) 
    {
      /* Set mission to UNKNOWN by default. */

      status = 0;
      strncpy(file->af->mission, "UNKNOWN", FLEN_VALUE);
    }
  checkGenAttFileFITSErrors(status,"reading TELESCOP from",file);
  if(status)
    return NULL;

  /* Read number of rows in the attitude table. */

  fits_read_key_lng(file->af->fp, "NAXIS2", &(file->af->nrows), comment, &status);
  checkGenAttFileFITSErrors(status,"reading NAXIS2 from",file);
  if(status)
    return NULL;

  if(0 >= file->af->nrows)
    {
      fprintf(stderr, "Attitude file %s has no data\n", file->af->name);
      return NULL;
    }

  /* Read column numbers. */

  fits_get_colnum(file->af->fp, CASESEN, "TIME", &(file->af->time_col), &status);
  checkGenAttFileFITSErrors(status,"locating TIME column in",file);
  if(status)
    return NULL;

  fits_get_colnum(file->af->fp, CASESEN, att_col_name, &(file->af->qparam_col), &status);
  sprintf(tempstring, "locating attitude column %s in", att_col_name);
  checkGenAttFileFITSErrors(status, tempstring, file);
  if(status)
    return NULL;

  file->att_col_number = file->af->qparam_col;

  /* Check the vector length of the attitude column. */

  fits_get_coltype(file->af->fp, file->att_col_number, &type_code, 
		   &n_elements, &col_bytes, &status);
  checkGenAttFileFITSErrors(status,"determining number of vector elements of attitude column in",file);
  if(status)
    return NULL;
  
  if(n_elements != file->att_n_elements)
    {
      fprintf(stderr, "Column %s of attitude file %s\nhas %ld elements, but %d elements are expected for attitude format %s.\n", att_col_name, file->af->name, n_elements, file->att_n_elements, file->att_format_name);
      return NULL;
    }
    

  /* Mark the last search results as uninitialized since no searches
     have been done. */

  file->af->search_row = UNINITIALIZED_ATTFILE_ROW;
  file->af->search_qs_reliable = 0;

  /* Allocate memory for the resident quaternion structures, which are
   * used to make repeated interpolations more efficient. Initialize the quaternions.
   */

  file->af->search_q0 = allocateQuat();
  setQuatToIdentity(file->af->search_q0);
  file->af->search_q1 = allocateQuat();
  setQuatToIdentity(file->af->search_q1);
  file->af->search_deltaq = allocateQuat();
  setQuatToIdentity(file->af->search_deltaq);
  file->af->hatq = allocateQuat();
  setQuatToIdentity(file->af->hatq);

  /* Read the last and first times from the attitude table. Read the
   * first time last to leave the FITSIO buffers referring to the
   * beginning of the file.
   */

  file->af->tstop = readTimeFromAttFile(file->af, file->af->nrows);
  file->af->tstart = readTimeFromAttFile(file->af, 1L);
  file->af->duration = file->af->tstop - file->af->tstart;
  
  /* Check that the attitude file has a positive duration. */

  if(file->af->duration <= 0)
    {
      printf("warning: attitude file %s does not have positive duration [%f]\n", 
	     file->af->name, file->af->duration);
      file->af->duration = 1;
    }
  
  /* Set extrapolation limits and method. */
  
  resetAttFileExtrapolationLimits(file->af, extrap_margin);
  
  file->af->interpolation = interpolation_method;
  file->af->extrapolation = ATTFILE_CONSTANT;

  /* Set interpolation parameters. */

  file->interp_margin = interp_margin;
  file->use_interp_margin = use_interp_margin;
  
  return file;
}

/* Determine the attitude quaternion at a given time.  If the attitude
   file gives the attitude in a different format, the attitude is
   converted to a quaternion.  Values are interpolated if necessary,
   and the process is optimized for the case of repeated calls to this
   routine with similar time values. */
int findQuatInGenAttFile /* Returns 1 if attitude is found at the time,
			  * 0 if not. */
(
 GENATTFILE* file, /* Attitude file structure */
 QUAT* q, /* Returns the attitude in quaternion form at the time */
 double time /* Time at which attitude is desired */
 )
{
  int found = 1;
  int doLinear = 0;
  double hat = 0.;

  /* Search for the correct place in the table. */

  findTimeInAttFile(file->af, time);

  /* Check if we need to read a new pair of attitude quaternions. */

  if(!file->af->search_qs_reliable)
    {
      /* Read two values and calculate the quaternion of change between them. */
      readQuatFromGenAttFile(file, file->af->search_q0, file->af->search_row);

      if(file->af->nrows > 1)
	{
	  readQuatFromGenAttFile(file, file->af->search_q1, file->af->search_row + 1L);
	  getQuatOfChange(file->af->search_deltaq, file->af->search_q0, file->af->search_q1);
	}

      file->af->search_qs_reliable = 1;
    }

  /* If there is only one row, we don't need to interpolate. */
  
  if(file->af->nrows == 1)
    {
      *q = *(file->af->search_q0);
      
      if(file->af->limit_nearby >= 0)
	found = fabs(file->af->search_time0 - time) <= file->af->limit_nearby;
      
      return found;
    }

  doLinear = 0;
  
  /* Do the interpolation as needed. */
  
  if(time <= file->af->search_time0 || time >= file->af->search_time1)
    {
      /* Case: Time is outside normal bounds. */
      
      if(file->af->extrapolation == ATTFILE_CONSTANT)
	{
	  if(time <= file->af->search_time0)
	    *q = *(file->af->search_q0);
	  else
	    *q = *(file->af->search_q1);
	}
      else
	doLinear = 1;
    }
  else if(file->use_interp_margin && fabs(file->af->search_time0 - time) > file->interp_margin
	  && fabs(file->af->search_time1 - time) > file->interp_margin)
    {
      /* Case: Time is inside bounds, but the interpolation margin is enabled and the event time is farther away
       *       from either bounding time than the time margin, so return a null quat. */

      setNullQuat(q);
    }
  else if(file->use_interp_margin && fabs(file->af->search_time0 - time) <= file->interp_margin 
	  && fabs(file->af->search_time1 - time) > file->interp_margin)
    {
      /* Case: Time is inside bounds and the interpolation margin is enabled.  The event time is within
       *       the interpolation margin of the lower bound but not within that of the upper bound.
       *       so use the quat of the lower bound. */

      *q = *(file->af->search_q0);
    }
  else if(file->use_interp_margin && fabs(file->af->search_time1 - time) <= file->interp_margin 
	  && fabs(file->af->search_time0 - time) > file->interp_margin)
    {
      /* Case: Time is inside bounds and the interpolation margin is enabled.  The event time is within
       *       the interpolation margin of the upper bound but not within that of the lower bound.
       *       so use the quat of the upper bound. */

      *q = *(file->af->search_q1);
    }
  else if(file->af->interpolation == ATTFILE_CONSTANT)
    {
      /* Case: Time is inside bounds. Interpolation method is CONSTANT. Select the quat of the closer time. */
      
      if(fabs(file->af->search_time1 - time) < fabs(file->af->search_time0 - time))
	*q = *(file->af->search_q1);
      else
	*q = *(file->af->search_q0);
    }
  else if(file->af->interpolation == ATTFILE_LINEAR_NEARBY)
    {
      /* Case: Time is inside bounds. Interpolation method is LINEAR_NEARBY. */
      
      if(fabs(file->af->search_time1 - time) <= file->af->limit_nearby
	 && fabs(file->af->search_time0 - time) <= file->af->limit_nearby
	 )
	doLinear = 1;
      else if(fabs(file->af->search_time1 - time) < fabs(file->af->search_time0 - time))
	*q = *(file->af->search_q1);
      else
	*q = *(file->af->search_q0);
    }
  else
    {
      /* Case: Time is inside bounds. Interpolate linearly. */
      
      doLinear = 1;
    }

  /* Linearly interpolate quaternions if this method has been selected. */

  if(doLinear)
    {
      double dt = file->af->search_time1 - file->af->search_time0;
      
      if(dt > 0)
	{
	  double tfrac = (time - file->af->search_time0)/dt;
	  interpolateQuat(q, file->af->search_q0, file->af->search_q1, 
			  file->af->search_deltaq, file->af->hatq, tfrac);

#ifdef DEBUG
	  {
	    double qfrac0 = (file->af->search_q0->p[0] == file->af->search_q1->p[0] ? 0.5 : (q->p[0] - file->af->search_q0->p[0])/(file->af->search_q1->p[0] - file->af->search_q0->p[0]));
	    double qfrac1 = (file->af->search_q0->p[1] == file->af->search_q1->p[1] ? 0.5 : (q->p[1] - file->af->search_q0->p[1])/(file->af->search_q1->p[1] - file->af->search_q0->p[1]));
	    double qfrac2 = (file->af->search_q0->p[2] == file->af->search_q1->p[2] ? 0.5 : (q->p[2] - file->af->search_q0->p[2])/(file->af->search_q1->p[2] - file->af->search_q0->p[2]));
	    double qfrac3 = (file->af->search_q0->p[3] == file->af->search_q1->p[3] ? 0.5 : (q->p[3] - file->af->search_q0->p[3])/(file->af->search_q1->p[3] - file->af->search_q0->p[3]));
	    
	    printf("q0="); printQuat(file->af->search_q0, stdout); printf("\n");
	    printf("q ="); printQuat(q, stdout); printf("\n");
	    printf("q1="); printQuat(file->af->search_q1, stdout); printf("\n");
	    printf("dq="); printQuat(file->af->search_deltaq, stdout); printf("\n"); 
	    
	    printf("tfrac= %.6g qfrac=[%.6g %.6g %.6g %.6g]\n", tfrac,
		 qfrac0, qfrac1, qfrac2, qfrac3);
	  }
#endif

	}
      else /* dt <= 0 */
	{
	  *q = *(file->af->search_q0);
	}

#ifdef DEBUG
      printf("findQuatInGenAttFile: evt_t=%g att_t=(%g, %g) att_q=(", time, file->af->search_time0, file->af->search_time1);
      printQuat(file->af->search_q0, stdout);
      printf(", ");
      printQuat(file->af->search_q1, stdout);
      printf(") ==> evt_q=");
      printQuat(q, stdout);
      printf("\n");
#endif

    }

  if(file->af->limit_nearby >= 0)
    {
      found = fabs(file->af->search_time1 - time) <= file->af->limit_nearby 
	      || fabs(file->af->search_time0 - time) <= file->af->limit_nearby;
    }

  return found;
}

/* Read the attitude from the attitude file and return it in quaternion form. */
void readQuatFromGenAttFile
(
 GENATTFILE* file, /* Attitude file structure */
 QUAT* q, /* Returns quaternion in the row */
 long row /* Row at which attitude is desired */
 )
{
  double nullvalue = 0.;

  /* Branch by attitude format and calculate attitude quaternion. */

  if(file->att_format == AF_QUAT)
    {
      /* Read attitude as a 4-element quaternion table cell. */

      readThingFromAttFile(file->af, q->p, &nullvalue, TDOUBLE, row, 
			   file->att_col_number, 4L);
    }
  else if(file->att_format == AF_EULER)
    {
      /* Read attitude as a 3-element Euler angle trio table cell and
       * convert to quaternion. */

      readThingFromAttFile(file->af, file->v3, &nullvalue, TDOUBLE, 
			   row, file->att_col_number, 3L);
      setEulerAngles(file->e, file->v3[0]*M_PI/180., file->v3[1]*M_PI/180., file->v3[2]*M_PI/180.);
#ifdef DEBUG
      printf("e from att file: ");
      printEulerDeg(file->e, stdout);
      printf(" deg.\n");
#endif
      convertEulerToQuat(q, file->e);
    }

#ifdef DEBUG
  printf("readQuatFromGenAttFile: row=%ld, q from att file: ", row);
  printQuat(q, stdout);
  printf("\n");
#endif

  /* Fix numerical precision errors in the norm of the quat. */

  renormalizeQuat(q);

}

/* Close the attitude file and free memory. */
void closeGenAttFile
(
 GENATTFILE* file /* Attitude file structure to close/destroy */
 )
{
  /* If file is a null pointer, no memory can be freed, so return. */

  if(file == NULL)
    return;

  closeAttFile(file->af);

  free(file->att_ext_name);
  free(file->att_col_name);
  free(file->att_format_name);
  destroyEuler(file->e);
  free(file);
}


/* Retrieve the number of elements for an attitude format. */
int getAttFormatNElements /* Returns the number of elements */
(
 int att_format /* Attitude format */
 )
{
  int n_elements[N_ATTITUDE_FORMATS];

  /* Return a zero element count if att_format is invalid. */

  if(att_format < 0 || att_format >= N_ATTITUDE_FORMATS)
    return 0;

  /* Define numbers of elements. */

  n_elements[AF_QUAT] = 4;
  n_elements[AF_EULER] = 3;

  /* Return the appropriate number of elements. */

  return n_elements[att_format];
}

/* Retrieve the name of an attitude format. */
void getAttFormatName
(
 int att_format, /* Attitude format number */
 char* att_format_name /* Returns attitude format name */
 )
{
  char names[N_ATTITUDE_FORMATS][FLEN_VALUE];

  /* Return INVALID if att_format is invalid .*/

  if(att_format < 0 || att_format >= N_ATTITUDE_FORMATS)
    {
      strcpy(att_format_name, "INVALID");
      return;
    }

  /* Define attitude format names. */

  strcpy(names[AF_QUAT ], "QUAT");
  strcpy(names[AF_EULER], "EULER");
  
  /* Return the appropriate attitude format name. */

  strcpy(att_format_name, names[att_format]);

  return;
}

/* Handle FITSIO errors while reading attitude file.  Display error
   messages but do not terminate execution. */
void checkGenAttFileFITSErrors
(
 int status, /* CFITSIO status */
 char* doing, /* present participle string of last action */
 GENATTFILE* file /* Attitude file structure */
 )
{
  /* Nothing to do if status == 0. */

   if(!status) return;
 
   /* Print error messages. */

   fprintf(stderr,"FITSIO error while %s attitude file %s:",doing,file->af->name);
   fits_report_error(stderr,status);
}


/* Print a GENATTFILE structure to a stream. */
void printGenAttFile
(
 GENATTFILE* genattfile, /* Pointer to GENATTFILE structure */
 FILE* stream /* Output stream */
 )
{
  fprintf(stream, "  att_format: %d\n", genattfile->att_format);
  fprintf(stream, "  att_format_name: %s\n", genattfile->att_format_name);
  fprintf(stream, "  att_n_elements: %d\n", genattfile->att_n_elements);
  fprintf(stream, "  att_ext_name: %s\n", genattfile->att_ext_name);
  fprintf(stream, "  att_col_name: %s\n", genattfile->att_col_name);
  fprintf(stream, "  att_col_number: %d\n", genattfile->att_col_number);
  fprintf(stream, "  interp_margin: %g\n", genattfile->interp_margin);
  fprintf(stream, "  use_interp_margin: %d\n", genattfile->use_interp_margin);
  fprintf(stream, "  v3: (%g %g %g)\n", genattfile->v3[0], genattfile->v3[1], genattfile->v3[2]);
  fprintf(stream, "  e: "); printEulerDeg(genattfile->e, stream); fprintf(stream, "\n");
  fprintf(stream, "  af ATTFILE structure:\n");
  printAttFile(genattfile->af, stream);
}

/* ====================================================================== */
/* Revision log
   $Log: genattfile.c,v $
   Revision 1.10  2015/12/29 17:32:21  rshill
   Added check for empty input file.

   Revision 1.9  2014/07/14 19:54:17  rshill
   Added cvs log

*/
