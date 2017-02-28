/** \file coordevtlib.cxx
 *  \brief Data structure and function definitions for coordevt
 *  \author Timothy Reichard
 *  \date $Date: 2016/08/09 20:48:13 $
 */

#define AHLABEL TASKS_COORDEVT_COORDEVTLIB
#define AHCVSID "$Id: coordevtlib.c,v 1.22 2016/08/09 20:48:13 rshill Exp $"

#include "aber.h"
#include "coordevtlib.h"
#include "coordfits2.h"
#include "hdcal.h"
#include "ape/ape_trad.h"
#include "hd_math_compat.h"
#include "headas_rand.h"
#include <ctype.h>
#include <sys/time.h>

/* Length for temporary string buffers */
#define STRBUFFLEN 4000 

/* ====================================================================== */

/* Random number state and functions. */

static HDmt_state* ran_state = 0;

void initializeRandom(const unsigned long int seedIn)
{
  struct timeval tt;
  unsigned long int seed = 0;
  
  if (0 == seedIn)
  {
    gettimeofday(&tt, NULL);
    seed = tt.tv_sec + tt.tv_usec;
  }
  else
  {
    seed = seedIn;
  }
  ran_state = HDmt_srand(seed);
}

/* ---------------------------------------------------------------------------- */

double getRandomDouble(void)
{
  return HDmt_drand(ran_state);
}

/* ---------------------------------------------------------------------------- */

void destroyRandom(void)
{
  HDmt_destroy_state(ran_state);
}

/* ---------------------------------------------------------------------------- */

/* Return a random number between +-0.5*scale. */
double getRandomShift(const double scale) 
{
  /* return scale * (ahgen::getRandom() - 0.5); */
  return scale * (getRandomDouble() - 0.5); 
}

/* ---------------------------------------------------------------------------- */

/* Make a param structure. */
PARAM* createParam(void)
{
  PARAM* param = (PARAM*) calloc(1, sizeof(PARAM));
  param->in_file = NULL;
  strcpy(param->temp_file, "");
  param->out_file_as_input = NULL;
  strcpy(param->out_file, "");
  param->out_file_bang = 0;
  param->are_files_same = 0;
  strcpy(param->teldef_file, "");
  param->att_file = NULL;
  param->datt_files = NULL;
  param->orb_file = NULL;
  param->startsysname = NULL;
  param->stopsysname = NULL;
  param->startsys = 0;
  param->stopsys = 0;
  param->lowsys = 0;
  param->highsys = 0;
  param->do_annual_aberration = 0;
  param->invert_annual_aberration = 0;
  param->do_orbital_aberration = 0;
  param->invert_orbital_aberration = 0;
  param->follow_sun = 0;
  param->use_att = 0;
  param->use_sky_att = 0;
  param->use_orb = 0;
  param->att_annaber = 0;
  param->att_orbaber = 0;
  param->att_followsun = 0;
  param->att_invaberr = 0;
  param->att_invoaber = 0;
  param->really_do_annaber = 0;
  param->really_inv_annaber = 0;
  param->really_do_orbaber = 0;
  param->really_inv_orbaber = 0;
  param->randomize = NULL;
  param->dorandomization = 0;
  param->seed = 0;
  strcpy(param->randsysname, "");
  param->randsys = 0;
  strcpy(param->randscalesysname, "");
  param->randscalesys = 0;
  param->att_interpolation = 0;
  param->datt_interpolation = 0;
  param->event_extension = NULL;
  param->time_col_name = NULL;
  param->att_extension = NULL;
  param->orb_extension = NULL;
  param->att_col_name = NULL;
  param->orb_col_name = NULL;
  param->orb_format_string = NULL;
  param->vel_col_name = NULL;
  param->num_vel_cols = 0;
  param->att_format_string = NULL;
  param->att_format = 0;
  param->vel_format = 0;
  param->seg_col_name = NULL;
  param->includeintcol = 0;
  param->includefloatcol = 0;
  param->includefloatskycol = 0;
  param->startwithfloat = 0;
  param->floatcolsuffix = NULL;
  param->blank_col = 0;
  param->b_null_value = 0;
  param->i_null_value = 0;
  param->j_null_value = 0;
  param->k_null_value = 0;
  param->sb_null_value = 0;
  param->ui_null_value = 0;
  param->uj_null_value = 0;
  param->ra = 0.0;
  param->dec = 0.0;
  param->roll = 0.0;
  param->ra_from_event_file = 0;
  param->dec_from_event_file = 0;
  param->att_time_margin = 0.0;
  param->datt_time_margin = 0.0;
  param->use_att_time_margin = 0;
  param->use_datt_time_margin = 0;
  param->chatter = 0;
  param->clobber = 0;
  param->debug = 0;
  param->write_history = 0;
  param->buffer = 0;
  param->tempfile_copied = 0;
  param->tempfile_written = 0;
  return param;
}

/* ---------------------------------------------------------------------------- */

/* Free the memory associated with a param structure. */
void destroyParam(PARAM* param) {

  int icol=0; /* Velocity column counter. */

  if (NULL == param)
    return;

  free(param->in_file);
  free(param->out_file_as_input);
  free(param->att_file);
  free(param->datt_files);
  free(param->orb_file);
  free(param->startsysname);
  free(param->stopsysname);
  free(param->randomize);
  free(param->event_extension);
  free(param->time_col_name);
  free(param->att_extension);
  free(param->orb_extension);
  free(param->att_col_name);
  free(param->orb_col_name);
  free(param->orb_format_string);
  free(param->att_format_string);


  if (0 != param->really_do_orbaber)
  {
    for (icol=0; icol<param->num_vel_cols; icol++) 
    {
      free(param->vel_col_name[icol]);
    }
    free(param->vel_col_name);
  }

  free(param);

} /* end of destroyParam function */

/* ---------------------------------------------------------------------------- */

/* Get the actual filename for the TelDef file. */
int resolveTeldefFilename(INFO* info) {

  int status = 0;  /* HDgtcalf status */

  /* Query parameters that never change for this query. */
  const char* codenam="TELDEF";
  const char* detnam="-";
  const char* filt="-";
  const char* strtdate="-";
  const char* strttime="-";
  const char* stpdate="-";
  const char* stptime="-";
  const char* expr="-";

  /* Variable query parameters. */
  char tele[FLEN_VALUE];   /* TELESCOP (mission) */
  char instr[FLEN_VALUE];  /* INSTRUME */

  int maxret = 1;  /* Maximum number of returned values */
  char teldef_filename[FLEN_FILENAME];  /* Buffer for found filename */
  int fnamesize = FLEN_FILENAME;  /* Length of filename buffer */
  char* fnptr = teldef_filename;  /* Alias for filename buffer */
  long extno = 0;  /* Extension number */
  char online[10];  /* Online indicator */
  char* onptr = online;  /* Alias for online indicator */
  int nret = 0;   /* Number of entries returned */
  int nfound = 0;   /* Total number of datasets matching selection criteria */

  strcpy(tele, "");
  strcpy(instr, "");
  strcpy(teldef_filename, "");

  if (0 == strcasecmp(info->param->teldef_file, "CALDB")) {
  
    ahlog_info(HIGH, __func__, "Querying CALDB for TelDef file\n");

    strcpy(tele, info->mission);
    strcpy(instr, info->instrument);

    status = 0;
    HDgtcalf(tele,instr,detnam,filt,codenam,strtdate,strttime,
             stpdate,stptime,expr,maxret,fnamesize,&fnptr,&extno,
             &onptr,&nret,&nfound,&status);

    if (0 != status) {
      ahlog_err(__func__, "Could not get TelDef file from CALDB; HDgtcalf status = %d\n", 
        status);
      return 1;
    }

    if (0 == nfound) {
      ahlog_err(__func__, "No TelDef file found.\n");
      return 1;
    }

    strcpy(info->param->teldef_file, "");
    strncat(info->param->teldef_file, teldef_filename, FLEN_FILENAME-1);
    ahlog_info(HIGH, __func__, "TelDef file = %s\n", info->param->teldef_file);

  }

  return 0;
}


/* ---------------------------------------------------------------------- */

/* Set up a misc. info structure to hand to the cfitsio iterator. */
INFO* createInfo /* Returns the INFO structure */
(
  PARAM* param, /* Parameter structure */
  fitsfile* fp /* Event file pointer */
  )
{
  INFO* info = NULL; /* Structure holding runtime info for the CFITSIO iterator */
  int status = 0; /* CFITSIO status */
  int teldef_status = 0;  /* Status for resolving TelDef filename. */
  int ape_status_set = 0;  /* Status for setting an APE parameter. */
  char comment[FLEN_COMMENT]; /* Comment string */
  int sys = 0; /* coordinate system index */

  strcpy(comment, "");

  /* Allocate memory for the info structure and initialize. */

  info = (INFO*) calloc(1, sizeof(INFO));

  info->att = NULL;
  info->orb = NULL;
  info->datt_file_list = NULL;
  info->att_filenames = NULL;
  info->n_datt_files = 0;
  info->att_identity_flags = NULL;
  info->q = NULL;
  info->buffer_rows = (param->buffer < 0 ? 0 : 
                       (param->buffer == 0 ? 1 : param->buffer));
  info->total_rows = 0;
  info->v_earth_orbit = 0.;
  info->vhat_earth_orbit[0] = 0.;
  info->vhat_earth_orbit[1] = 0.;
  info->vhat_earth_orbit[2] = 0.;
  info->v_sat_orbit = 0.;
  info->vhat_sat_orbit[0] = 0.;
  info->vhat_sat_orbit[1] = 0.;
  info->vhat_sat_orbit[2] = 0.;
  info->v_sat_total = 0.;
  info->vhat_sat_total[0] = 0.;
  info->vhat_sat_total[1] = 0.;
  info->vhat_sat_total[2] = 0.;
  info->is_orbvel_valid = 0;
  info->mjdref = 0.;
  info->conv_to_higher = 1;
  strcpy(info->conv_arrow, "->");

  info->time_col = NULL;
  info->seg_cols = NULL;
  info->prop_cols = NULL;
  info->intx_cols = NULL;
  info->inty_cols = NULL;
  info->floatx_cols = NULL;
  info->floaty_cols = NULL;

  info->time = NULL;
  info->intx = NULL;
  info->inty = NULL;
  info->floatx = NULL;
  info->floaty = NULL;
  info->segs = NULL;
  info->props = NULL;
  info->rowprops = NULL;
  info->nullx = NULL;
  info->nully = NULL;
  info->nullcol = NULL;

  info->prop_locations = NULL;
  info->prop_key_values = NULL;

  info->window_offsets_x = NULL;
  info->window_offsets_y = NULL;

  info->random_px_scale = 0.;
  info->prev_percent_done = 0.;

  info->n_events_processed = 0;
  info->n_events_succeeded = 0;
  info->n_events_failed = 0;
  info->n_events_null_time = 0;
  info->n_events_no_attitude = NULL;
  info->n_events_no_orbit = 0;
  info->n_events_no_multiseg = NULL;
  info->n_events_no_rawtodet = NULL;
  info->n_events_out_of_range = 0;
  info->first_event_with_attitude = NULL;
  info->last_event_with_attitude = NULL;
  info->first_event_with_orbit = 0;
  info->last_event_with_orbit = 0;
  info->event_time_at_last_quat = NULL;

  /* Remember the parameter structure. */

  info->param=param;
  
  /* Read the mission and instrument from the event file. */

  fits_read_key_str(fp,"TELESCOP",info->mission   ,comment,&status);
  fits_read_key_str(fp,"INSTRUME",info->instrument,comment,&status);
  if (0 != status) 
  {
    ahlog_info(HIGH, __func__, "Cannot read TELESCOP and INSTRUME keywords from %s.\n", param->in_file);
    status=0;
  }

  /* Look up the CALDB TelDef file. */

  teldef_status = resolveTeldefFilename(info);
  if (0 != teldef_status) {
    ahlog_err(__func__, "Unable to resolve TelDef filename.\n");
    return NULL;
  }
  ape_status_set = ape_trad_set_string("teldeffile", param->teldef_file);
  if (0 != ape_status_set) {
    ahlog_err(__func__, "Unable to set teldeffile parameter to actual filename.\n");
    return NULL;
  }

  /* Open the TelDef file and make some consistency checks. */
 
  status = readTelDef2(param->teldef_file, &(info->teldef));
  if (0 != status)
  {
    ahlog_err(__func__, "An error was encountered while reading the TelDef file %s.\n", param->teldef_file);
    return NULL;
  }
  
  /* Check that TELESCOP and INSTRUME keywords match between event and TelDef files. */

  if (0 != strncasecmp(info->mission, info->teldef->mission, FLEN_VALUE)) {
    ahlog_info(HIGH, __func__, "Keyword TELESCOP=%s in %s but TELESCOP=%s in %s.\n", info->mission, param->in_file, 
      info->teldef->mission, info->teldef->filename);
  }

  if (0 != strncasecmp(info->instrument, info->teldef->instrument, FLEN_VALUE)) {
    ahlog_info(HIGH, __func__, "Keyword INSTRUME=%s in %s but TELESCOP=%s in %s.\n", info->instrument, param->in_file, 
      info->teldef->instrument, info->teldef->filename);
  }

  /* Set dorandomization correctly if param->randomize ==
     TELDEF. dorandomization is already set to 1 or 0 if randomize is
     set to YES or NO. */

  if (0 == strcasecmp(param->randomize, "TELDEF")) {
    ahlog_info(HIGH, __func__, "Randomization specified as TELDEF.\n");
    if (0 > info->teldef->randsys) {
      param->dorandomization = 0;
      ahlog_info(HIGH, __func__, "Not randomizing.\n");
    } else {
      param->dorandomization = 1;
      ahlog_info(HIGH, __func__, "Randomizing.\n");
    }
  }

  /* Allocate storage for the current pointing quaternion and the event times when the quats are read.  */

  info->q=allocateQuat();
  info->event_time_at_last_quat = (double*) calloc(info->teldef->n_coordsys - 1, sizeof(double));
  for(sys = 0; sys < info->teldef->n_coordsys - 1; sys++)
    info->event_time_at_last_quat[sys] = DOUBLENULLVALUE;

  /* Initialize the processing progress. */
  
  info->prev_percent_done = 0.;

  /* Allocate storage for one row of MULTISEG properties. */

  info->rowprops = (int**) calloc(info->teldef->n_coordsys - 1, sizeof(int*));
  for(sys = 0; sys < info->teldef->n_coordsys - 1; sys++)
  {
    info->rowprops[sys] = NULL;
    if (e_TT_MULTISEG == info->teldef->trtypes[sys])
    {
      info->rowprops[sys] = (int*) calloc(info->teldef->multisegparam[sys]->n_properties, sizeof(int));
    }
  }


  return info;

} /* end of createInfo function */

/* ---------------------------------------------------------------------- */

/* Set up CFITSIO iterator column structures, one for each column to
   be read or written */
iteratorCol* setIteratorColumns /* Returns array of iterator columns */
(
  INFO* info, /* INFO structure */
  fitsfile* fp,  /* Event file pointer */
  int* n_cols /* Number of columns */
  )
{
  iteratorCol* fits_col = NULL; /* Array of CFITSIO iterator column structs */
  iteratorCol* col = NULL; /* Useful iterator column pointer */
  int fits_col_number = 0; /* Iterator column number */
  PARAM* param = info->param; /* Parameters structure */
  TELDEF2* teldef = info->teldef; /* Standard TelDef structure */
  char tempstring[STRBUFFLEN]; /* Reused string */
  int iotype_int = InputOutputCol; /* I/O type for an integer column */
  int iotype_float = InputOutputCol;  /* I/O type for a decimal column */

  int includeintcol = info->param->includeintcol; /* Synonym */
  int includefloatcol = info->param->includefloatcol; /* Synonym */
  int includefloatskycol = info->param->includefloatskycol; /* Synonym */
  int includethisintcol = 0; /* Flag if this integer column is included */
  int includethisfloatcol = 0; /* Flag if this decimal column is included */
  
  /* Declare array indices for coordinate system and MULTISEG property. */
  
  int sys = 0;
  int prop = 0;
  
  /* Set initial and final system numbers based on startsys, stopsys, and blankcol parameter values. */

  int min_sys = -1;
  int max_sys = -1;

  /* Initialize string. */

  strcpy(tempstring, "");
  
  if (0 != info->conv_to_higher)
  {
    min_sys = info->param->startsys; 
    max_sys = (info->param->blank_col ? teldef->n_coordsys - 1 : info->param->stopsys); 
  }
  else
  {
    min_sys = (info->param->blank_col ? 0 : info->param->stopsys);
    max_sys = info->param->startsys;
  }
  ahlog_info(HIGH, __func__, "Minimum system number = %d; maximum system number = %d.\n", min_sys, max_sys);
  
  *n_cols = 0;

  if (0 == includeintcol && 0 == includefloatcol)
  {
    ahlog_err(__func__, "Both integer and floating-point coordinate columns are set to be suppressed. Quitting.\n");
    fits_col = NULL;
    return NULL;
  }
  
  /* Get count of columns. Begin by adding a time column if
     necessary. */
  
  if (0 != param->use_att) 
    (*n_cols)++; /* time column */
  
  /* Add columns for each system based on the transformation type and
     its details. */

  for(sys = min_sys; sys <= max_sys; sys++)
  {
    /* Determine if the rounded and unrounded columns are needed for this system. */

    if (0 != info->conv_to_higher)
    {
      includethisintcol = (includeintcol && ((sys == min_sys && 0 == param->startwithfloat) || sys > min_sys) ? 1 : 0);
      includethisfloatcol = ((includefloatcol && ((sys == min_sys && param->startwithfloat) || sys > min_sys))
                             || (includefloatskycol && sys == teldef->sky_sys)
                             ? 1 : 0);
    }
    else
    {
      includethisintcol = (includeintcol && ((sys == max_sys && 0 == param->startwithfloat) || sys < max_sys) ? 1 : 0);
      includethisfloatcol = ((includefloatcol && ((sys == max_sys && param->startwithfloat) || sys < max_sys))
                              || (includefloatskycol && sys == teldef->sky_sys)
                              ? 1 : 0);
    }

    /* Continue counting columns -- coordinate system-specific columns. */

    if (sys < teldef->n_coordsys - 1)
    {
      if (e_TT_RAWTODET == teldef->trtypes[sys])
      {
        if (RM_CORNER_LIST != teldef->rawtodetparam[sys]->rawmethod)
        {
          /* RAWTODET methods besides the pixel corner map
             have two coordinate columns. */
                  
          *n_cols += 2 * (includethisintcol + includethisfloatcol);  
        }
              
        if (1 < teldef->n_segments[sys])
        {
          /* If there are multiple segments, then there is a
             segment column in addition to the
             coordinate columns. */
                  
          (*n_cols)++;
        }
      }
      else if (e_TT_MULTISEG == teldef->trtypes[sys])
      {
        /* The MULTISEG transformation needs two coordinate
           columns and a number of property columns (like
           segment, window mode, etc).  Since some of the
           properties can appear as keywords instead of
           columns, not all properties require a column in the count. */
              
        *n_cols += 2 * (includethisintcol + includethisfloatcol);

        for(prop = 0; prop < teldef->multisegparam[sys]->n_properties; prop++)
        {
          if (e_PL_COLUMN == info->prop_locations[sys][prop])
            (*n_cols)++;
        }

      }
      else /* Other transformations */
      {
        /* Other transformation types have only two coordinate
           columns without any other columns like one for
           segments. */
              
        *n_cols += 2 * (includethisintcol + includethisfloatcol);
      }

    }
    else /* highest coordinate system */
    {
      /* The last (SKY) coordinate system always results in two
         coordinate columns without any extra columns. */

      *n_cols += 2 * (includethisintcol + includethisfloatcol); 
    }
      
  } /* end for(sys) */

  ahlog_debug(__func__, __FILE__, __LINE__, "%d columns of the event table will be accessed for reading or writing.\n", *n_cols);

  /* Allocate the nullcol pointer array. */

  info->nullcol = (LONGLONG**) calloc(*n_cols, sizeof(LONGLONG*));

  /* Allocate space for the iterator columns and set col to first column. */
  
  fits_col = (iteratorCol*) calloc(*n_cols, sizeof(iteratorCol));
  col = fits_col;

  info->seg_cols = (iteratorCol**) calloc(teldef->n_coordsys, sizeof(iteratorCol*));
  info->intx_cols = (iteratorCol**) calloc(teldef->n_coordsys, sizeof(iteratorCol*));
  info->inty_cols = (iteratorCol**) calloc(teldef->n_coordsys, sizeof(iteratorCol*));
  info->floatx_cols = (iteratorCol**) calloc(teldef->n_coordsys, sizeof(iteratorCol*));
  info->floaty_cols = (iteratorCol**) calloc(teldef->n_coordsys, sizeof(iteratorCol*));
  info->prop_cols = (iteratorCol***) calloc(teldef->n_coordsys, sizeof(iteratorCol**));

  /* Allocate space for the coordinate, segment, property, and integer null values. */

  info->intx = (LONGLONG**) calloc(teldef->n_coordsys, sizeof(LONGLONG*));
  info->inty = (LONGLONG**) calloc(teldef->n_coordsys, sizeof(LONGLONG*));
  info->floatx = (double**) calloc(teldef->n_coordsys, sizeof(double*));
  info->floaty = (double**) calloc(teldef->n_coordsys, sizeof(double*));
  info->segs = (int**) calloc(teldef->n_coordsys, sizeof(int*));
  info->props = (int***) calloc(teldef->n_coordsys, sizeof(int**));
  info->nullx = (LONGLONG*) calloc(teldef->n_coordsys, sizeof(LONGLONG));
  info->nully = (LONGLONG*) calloc(teldef->n_coordsys, sizeof(LONGLONG));
  
  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    /* Set iterator column pointers to null initially. */

    info->intx_cols[sys] = NULL;
    info->inty_cols[sys] = NULL;
    info->floatx_cols[sys] = NULL;
    info->floaty_cols[sys] = NULL;
    info->seg_cols[sys] = NULL;
    info->prop_cols[sys] = NULL;
    info->intx[sys] = NULL;
    info->inty[sys] = NULL;
    info->floatx[sys] = NULL;
    info->floaty[sys] = NULL;
    info->segs[sys] = NULL;
    info->props[sys] = NULL;

    /* Initialize integer null values to parameter j_nullvalue. */

    info->nullx[sys] = param->j_null_value;
    info->nully[sys] = param->j_null_value;
  }

  /* Set time column if needed. */

  if (0 != param->use_att)
  {
    ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d.\n", fits_col_number, param->time_col_name, InputCol);
    fits_iter_set_by_name(col, fp, param->time_col_name, TDOUBLE, InputCol);
    info->time_col = col;
    col++;
    fits_col_number++;
  }
  else
    info->time_col = NULL;
  
  /* Set segment, property, and coordinate columns where needed. */

  for(sys = min_sys; sys <= max_sys; sys++)
  {
    /* Determine if the rounded and unrounded columns are needed for this system. */

    if (0 != info->conv_to_higher)
    {
      includethisintcol = (includeintcol && ((sys == min_sys && 0 == param->startwithfloat) || sys > min_sys) ? 1 : 0);
      includethisfloatcol = ((includefloatcol && ((sys == min_sys && param->startwithfloat) || sys > min_sys))
                             || (includefloatskycol && sys == teldef->sky_sys)
                             ? 1 : 0);
    }
    else
    {
      includethisintcol = (includeintcol && ((sys == max_sys && 0 == param->startwithfloat) || sys < max_sys) ? 1 : 0);
      includethisfloatcol = ((includefloatcol && ((sys == max_sys && param->startwithfloat) || sys < max_sys))
                              || (includefloatskycol && sys == teldef->sky_sys)
                             ? 1 : 0);
    }
    /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ sys=%d includethisintcol=%d includethisfloatcol=%d\n", 
      sys, includethisintcol, includethisfloatcol); */

    /* Set input/output column types. */
      
    if ((0 != info->conv_to_higher && sys == min_sys) || (0 == info->conv_to_higher && sys == max_sys))
    {
      if (0 != param->startwithfloat)
      {
        iotype_int = InputOutputCol; /* Moot because column should be ignored. */
        iotype_float = InputCol;
      }
      else
      {
        iotype_int = InputCol;
        iotype_float = InputOutputCol; /* Moot because column should be ignored. */
      }
    }
    else if ((0 != info->conv_to_higher && sys > min_sys) || (0 == info->conv_to_higher && sys < max_sys))
    {
      iotype_int = OutputCol;
      iotype_float = OutputCol;
    }
                             
    /* Set the iterator column structures. */

    if (sys < teldef->n_coordsys - 1) 
    {
      if (e_TT_RAWTODET == teldef->trtypes[sys])
      {
        if (RM_CORNER_LIST != teldef->rawtodetparam[sys]->rawmethod)
        {
          /* Set the first coordinate column. */
                  
          if (0 != includethisintcol)
          {
            fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, TLONGLONG, iotype_int);
            ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", 
              fits_col_number, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, iotype_int);
            info->intx_cols[sys] = col;
            info->nullcol[fits_col_number] = &(info->nullx[sys]);
            col++;
            fits_col_number++;
          }
                  
          if (0 != includethisfloatcol)
          {
	    sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, param->floatcolsuffix);
	    fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
            ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s%s iotype %d\n", 
              fits_col_number, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, param->floatcolsuffix, iotype_float);
            info->floatx_cols[sys] = col;
            col++;
            fits_col_number++;
          }
                  
                  
          /* Set the second coordinate column. */
          if (0 != includethisintcol)
          {
            fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, TLONGLONG, iotype_int);
            ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n",  
              fits_col_number, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, iotype_int);
            info->inty_cols[sys] = col;
            info->nullcol[fits_col_number] = &(info->nully[sys]);
            col++;
            fits_col_number++;
          }
                  
          if (0 != includethisfloatcol)
          {
	    sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, param->floatcolsuffix);
	    fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
            ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s%s iotype %d\n", 
              fits_col_number, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, param->floatcolsuffix, iotype_float);
            info->floaty_cols[sys] = col;
            col++;      
            fits_col_number++;
          }
        }
              
        if (1 < teldef->n_segments[sys])
        {
          /* Set the segment column, which is needed when there are more than one segment. */
              
          fits_iter_set_by_name(col, fp, teldef->rawtodetparam[sys]->segcolname, TINT, InputCol);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n",  
            fits_col_number, teldef->rawtodetparam[sys]->segcolname, InputCol);
          info->seg_cols[sys] = col;
          col++;     
          fits_col_number++;
        }
      } /* end if RAWTODET */
      else if (e_TT_MULTISEG == teldef->trtypes[sys])
      {
        /* Allocate property arrays. */

        info->prop_cols[sys] = (iteratorCol**) calloc(teldef->multisegparam[sys]->n_properties, sizeof(iteratorCol*));
        info->props[sys] = (int**) calloc(teldef->multisegparam[sys]->n_properties, sizeof(int*));
        for (prop=0; prop<teldef->multisegparam[sys]->n_properties; prop++)
        {
          info->prop_cols[sys][prop] = NULL;
          info->props[sys][prop] = NULL;
        }

        /* Set the coordinate columns. */

        if (0 != includethisintcol)
        {
          fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, TLONGLONG, iotype_int);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, 
            teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, iotype_int);
          info->intx_cols[sys] = col;
          info->nullcol[fits_col_number] = &(info->nullx[sys]);
          col++;
          fits_col_number++;

          fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, TLONGLONG, iotype_int);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, 
            teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, iotype_int);
          info->inty_cols[sys] = col;
          info->nullcol[fits_col_number] = &(info->nully[sys]);
          col++;
          fits_col_number++;
        }
              
        if (0 != includethisfloatcol)
        {
          sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, param->floatcolsuffix);
          fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, tempstring, iotype_float);
          info->floatx_cols[sys] = col;
          col++;
          fits_col_number++;
      

          sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, param->floatcolsuffix);
          fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, tempstring, iotype_float);
          info->floaty_cols[sys] = col;
          col++;
          fits_col_number++;

        }

        /* Set property columns. */

        for(prop = 0; prop < teldef->multisegparam[sys]->n_properties; prop++)
        {
          /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ Setting property columns: sys=%d prop=%d location=%d\n", 
            sys, prop, info->prop_locations[sys][prop]); */
          if (e_PL_COLUMN == info->prop_locations[sys][prop])
          {
            fits_iter_set_by_name(col, fp, teldef->multisegparam[sys]->propertynames[prop], TINT, InputCol);
            ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, 
              teldef->multisegparam[sys]->propertynames[prop], InputCol);
            info->prop_cols[sys][prop] = col;
            col++;
            fits_col_number++;
          }
        }
              
      } /* end if MULTISEG */
      else /* other transformations */
      {
        /* Set the coordinate columns. */

        if (0 != includethisintcol)
        {
          fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, TLONGLONG, iotype_int);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, 
            teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, iotype_int);
          info->intx_cols[sys] = col;
          info->nullcol[fits_col_number] = &(info->nullx[sys]);
          col++;
          fits_col_number++;

          fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, TLONGLONG, iotype_int);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, 
            teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, iotype_int);
          info->inty_cols[sys] = col;
          info->nullcol[fits_col_number] = &(info->nully[sys]);
          col++;
          fits_col_number++;
        }
              
        if (0 != includethisfloatcol)
        {
          sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, param->floatcolsuffix);
          fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, tempstring, iotype_float);
          info->floatx_cols[sys] = col;
          col++;      
          fits_col_number++;
                  
          sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, param->floatcolsuffix);
          fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
          ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, tempstring, iotype_float);
          info->floaty_cols[sys] = col;
          col++;
          fits_col_number++;
        }
      } /* end else (other transformations) */
    } /* end if not last system */
    else /* last system */
    {
      /* Set x and y coordinate columns. */
          
      if (0 != includethisintcol)
      {
        fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, TLONGLONG, iotype_int);
        ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n",  
          fits_col_number, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, iotype_int);
        info->intx_cols[sys] = col;
        info->nullcol[fits_col_number] = &(info->nullx[sys]);

        col++;
        fits_col_number++;
              
        fits_iter_set_by_name(col, fp, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, TLONGLONG, iotype_int);
        ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n",  
          fits_col_number, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, iotype_int);
        info->inty_cols[sys] = col;
        info->nullcol[fits_col_number] = &(info->nully[sys]);
        col++;
        fits_col_number++;
      }
          
      if (0 != includethisfloatcol)
      {
        sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, param->floatcolsuffix);
        fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
        ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, tempstring, iotype_float);
        info->floatx_cols[sys] = col;
        col++;
        fits_col_number++;
      
        sprintf(tempstring, "%s%s", teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, param->floatcolsuffix);
        fits_iter_set_by_name(col, fp, tempstring, TDOUBLE, iotype_float);
        ahlog_debug(__func__, __FILE__, __LINE__, "Set iterator column #%d %s iotype %d\n", fits_col_number, tempstring, iotype_float);
        info->floaty_cols[sys] = col;
        col++;
        fits_col_number++;
      }
    } /* end else last system */
  } /* end for sys loop */

  return fits_col;
}

/* ---------------------------------------------------------------------- */

/* Free all the memory associated with an INFO structure.  Note that
 * this even destroys the associated PARAM structure even though that
 * structure was created separately. */
void destroyInfo
(
  INFO* info /* INFO structure to destroy */
  )
{
  int sys = 0; /* Coordinate system index */
  int min_sys = 0; /* Lowest coordinate system index */
  int max_sys = 0; /* Highest coordinate system index */
  
  /* Don't free a null pointer. */
  
  if (NULL == info)
    return;
  
  /* Free the coordinate and segment property arrays that were allocated. */
  
  free(info->nullx);
  free(info->nully);
  free(info->intx);
  free(info->inty);
  free(info->floatx);
  free(info->floaty);
  free(info->segs);
  
  if (info->param->startsys < info->param->stopsys)
  {
    min_sys = info->param->startsys;
    max_sys = info->param->stopsys;
  }
  else
  {
    min_sys = info->param->stopsys;
    max_sys = info->param->startsys;
  }

  for(sys = min_sys; sys <= max_sys && sys < info->teldef->n_coordsys - 1; sys++)
  {
    if (e_TT_MULTISEG == info->teldef->trtypes[sys])
    {
      if (NULL != info->prop_cols && NULL != info->prop_cols[sys])
        free(info->prop_cols[sys]);
      
      if (NULL != info->props && NULL != info->props[sys])
        free(info->props[sys]);
      
      if (NULL != info->rowprops && NULL != info->rowprops[sys])
        free(info->rowprops[sys]);
      
      if (NULL != info->prop_locations && NULL != info->prop_locations[sys])
        free(info->prop_locations[sys]);
      
      if (NULL != info->prop_key_values && NULL != info->prop_key_values[sys])
        free(info->prop_key_values[sys]);
      
      if (NULL != info->window_offsets_x  && NULL != info->window_offsets_x[sys])
        free(info->window_offsets_x[sys]);

      if (NULL != info->window_offsets_y && NULL != info->window_offsets_y[sys])
        free(info->window_offsets_y[sys]);
    }
    
    if (info->param->use_att && sys <= max_sys - 1 
       && NULL != info->att && NULL != info->att[sys])
    {
      closeGenAttFile(info->att[sys]);
    }
    
    if (info->param->use_att && sys <= max_sys - 1 
       && NULL != info->att_filenames && NULL != info->att_filenames[sys])
      free(info->att_filenames[sys]);
  }
  
  /* Free the attitude file structures. */
  
  if (NULL != info->att)
    free(info->att);
  if (NULL != info->att_filenames)
    free(info->att_filenames);
  if (NULL != info->att_identity_flags)
    free(info->att_identity_flags);
  if (NULL != info->datt_file_list)
    free(info->datt_file_list);


  /* Free the orbit file structure. */

  closeGenOrbFile(info->orb);
  
  /* Free the other array elements. */
  
  free(info->props);
  free(info->rowprops);
  free(info->prop_locations);
  free(info->prop_key_values);
  free(info->seg_cols);
  free(info->intx_cols);
  free(info->inty_cols);
  free(info->floatx_cols);
  free(info->floaty_cols);
  free(info->prop_cols);
  free(info->nullcol);
  free(info->n_events_no_attitude);
  free(info->n_events_no_multiseg);
  free(info->n_events_no_rawtodet);
  free(info->first_event_with_attitude);
  free(info->last_event_with_attitude);
  free(info->event_time_at_last_quat);
  free(info->window_offsets_x);
  free(info->window_offsets_y);

  /* Free the structures. */

  destroyTelDef2(info->teldef);
  destroyQuat(info->q);
  destroyParam(info->param); 

  free(info); 
}

/* -------------------------------------------------------------------- */

/* Initialize attitude files for reading. */
int initAttFiles
(
  INFO* info /* The INFO structure */
  )
{
  int sys = -1; /* Coordinate system index */
  int status = 0; /* CFITSIO status */

  /* Set up attitude files if needed. */

  /* Allocate GENATTFILE structures. */

  int n_att_files = 0; /* Number of attitude files */
  int a = 0; /* Attitude file index */
  char att_sys_name[FLEN_VALUE] = ""; /* Attitude coord. system name */
  int att_sys_number = -1; /* Attitude coord. system number */
  char att_filename[FLEN_FILENAME] = ""; /* Attitude filename */

  PARAM* param = info->param; /* Shortcut */

  /* Count the attitude files that are needed. */

  for(sys = 0; sys < info->teldef->n_coordsys - 1; sys++)
  {
    if ((info->conv_to_higher && sys >= param->startsys && sys <= param->stopsys - 1 
       && e_TT_SKYATT == info->teldef->trtypes[sys])
       || (0 == info->conv_to_higher && sys <= param->startsys -1 && sys >= param->stopsys 
        && e_TT_SKYATT == info->teldef->trtypes[sys])
      )
    {
      n_att_files++;
    }
  }

  if (0 < n_att_files)
    param->use_att = 1;
  else
    param->use_att = 0;

  /* If no attitude files are needed, don't use a SKY attitude file even if provided in the command-line parameters. */
  
  if (0 == param->use_att)
    param->use_sky_att = 0;

  /* Check attitude files for which SKYATT transformation to apply them to. */

  if (0 != param->use_att)
  {
    
    /* Allocate and initialize the GENATTFILE structures and filename strings. */

    info->att = (GENATTFILE**) calloc(info->teldef->n_coordsys - 1, sizeof(GENATTFILE*));
    info->att_filenames = (char**) calloc(info->teldef->n_coordsys - 1, sizeof(char*));
    info->att_identity_flags = (int*) calloc(info->teldef->n_coordsys-1, sizeof(int));
    info->n_events_no_attitude = (long*) calloc(info->teldef->n_coordsys - 1, sizeof(long));
    info->first_event_with_attitude = (long*) calloc(info->teldef->n_coordsys - 1, sizeof(long));
    info->last_event_with_attitude = (long*) calloc(info->teldef->n_coordsys - 1, sizeof(long));

    for(sys = 0; sys < info->teldef->n_coordsys - 1; sys++)
    {
      info->att[sys] = NULL;
      info->n_events_no_attitude[sys] = 0;
      info->first_event_with_attitude[sys] = -1;
      info->last_event_with_attitude[sys] = -1;
    }

    /* Parse list of delta attitude files if the user supplied any filenames. */

    if (0 == strcasecmp(param->datt_files, "NONE"))
      info->n_datt_files = 0;
    else
      info->datt_file_list = expand_item_list(param->datt_files, &(info->n_datt_files), ',',
                                              1, 1, 0, &status);

    for(a = 0; a < info->n_datt_files; a++)
    {
      ahlog_debug(__func__, __FILE__, __LINE__, "datt %d of %d: %s\n",  a + 1, info->n_datt_files, info->datt_file_list[a]);
    }

    /* Match the attitude files to coordinate transformations. */

    for(sys = 0; sys < info->teldef->n_coordsys - 1; sys++)
    {
      if ( (0 != info->conv_to_higher && (sys < param->startsys || sys > param->stopsys - 1))
          || (0 == info->conv_to_higher && (sys < param->stopsys || sys > param->startsys - 1))
      )
        continue;

      ahlog_debug(__func__, __FILE__, __LINE__, "sys=%d info->teldef->trtypes[sys]=%d\n",
        sys, info->teldef->trtypes[sys]);
          
      if (e_TT_SKYATT == info->teldef->trtypes[sys])
      {
        /* Search for the correct attitude file, which might be the one intended for the -->SKY 
         * coordinate conversion (a = -1) or one of the delta attitude files for other 
         * conversions (a >= 0).
         */
              
        info->att[sys] = NULL;
        info->att_filenames[sys] = NULL;

        /* Process special IDENTITY filename, which says to always use the identity
         * quaternion for the attitude.  */

        /* This array says whether the current transformation (subscripted with sys) 
         * needs an identity quaternion. */

        info->att_identity_flags[sys] = 0;

        if (sys == info->teldef->n_coordsys - 2)
        {
          /* This is the real transformation to sky, starting from the
           * next-to-highest coordinate system. */
          info->att_identity_flags[sys] = param->identity_att;
        } 
        else
        {
          /* This is a transformation that uses a delta attitude file. */
          info->att_identity_flags[sys] = param->identity_datt;
        }

        if (0 != info->att_identity_flags[sys])
        {
          ahlog_info(HIGH, __func__, "IDENTITY quaternion will be used for transformation %s%s%s.\n", 
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1]);
        }

        if (0 == info->att_identity_flags[sys])
        {
          
          /* The identity quaternion is not used for attitude, so an
           * attitude file is needed for each SKYATT transformation. */
              
          /* Search through all attitude files if the current transformation is
           * SKYATT type and requires an attitude file.  a = -1 corresponds to
           * a sky attitude file, while a >= 0 corresponds to other ("delta")
           * attitude files. */

          for(a = -1; a < info->n_datt_files; a++)
          {

            /* Skip attitude file if IDENTITY transform specified. */

            if (-1 == a && 0 != param->identity_att) continue;
            if (-1 != a && 0 != param->identity_datt) continue;

            /* There is no sky attitude file to look for if stopsys is
             * set to a system below the last if converting to
             * higher coordinates, or if startsys is set to a system
             * below the last if converting to lower coordinates. */
                    
            if (-1 == a  && 
               ((info->conv_to_higher && param->stopsys == info->teldef->n_coordsys - 2)
                || (0 == info->conv_to_higher && param->startsys == info->teldef->n_coordsys - 2))
              )
              continue;

            if (-1 == a ) {
              strcpy(att_filename, param->att_file);
            }
            else
            {
              strcpy(att_filename, info->datt_file_list[a]);
            }
                    
            /* Determine the name and number of the destination
             * coordinate system that the attitude file is
             * intended for. */

            if (0 != getDestSysFromAttFile(att_filename, param->att_extension, att_sys_name, info->teldef))
            {
              ahlog_err(__func__, "Could not get destination system %s from attitude file %s.\n", 
                att_sys_name, att_filename);
              return 1;
            }
            att_sys_number = getCoordSystemNumberFromName(info->teldef, att_sys_name) - 1;
                    
            /* If the loop counter (sys) matches the attitude file's system number,
             * a match is found. In that case, open and initialize the attitude file. */

            if (att_sys_number == sys)
            {
              /* Display that a match has been found. */
                        
              ahlog_info(2, __func__, "Using attitude file %s for the transformation %s%s%s.\n", 
                att_filename, info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1]);
                        
              /* Open a GENATTFILE structure for this attitude file and system. 
               * Use sky attitude options for a == -1 and delta attitude options for a >= 0. */

              info->att[sys] = openGenAttFile(att_filename, param->att_extension,
                                              param->att_col_name, param->att_format,
                                              (a == -1 ? param->att_interpolation : param->datt_interpolation),
                                              (a == -1 ? param->att_time_margin : param->datt_time_margin),
                                              (a == -1 ? param->att_time_margin : param->datt_time_margin),
                                              (a == -1 ? param->use_att_time_margin : param->use_datt_time_margin)
                                             );
              if (NULL == info->att[sys])
              {
                ahlog_err(__func__, "Cannot initialize the attitude file %s .\n", att_filename);
                return 1;
              }

              if (0 != param->debug)
              {
                printf("GENATTFILE structure for transformation %s->%s:\n", 
                       info->teldef->coordsysnames[sys], info->teldef->coordsysnames[sys + 1]);
                printGenAttFile(info->att[sys], stdout);
              }

              /* Record the attitude filename for this system. */
                        
              info->att_filenames[sys] = (char*) calloc(FLEN_FILENAME, sizeof(char));
              strcpy(info->att_filenames[sys], att_filename);
                        
              /* Check that the attitude file's mission matches the event file mission. */
                        
              if (0 != strncasecmp(info->mission,info->att[sys]->af->mission, FLEN_VALUE))
              {
                ahlog_info(HIGH, __func__, "Keyword TELESCOP = %s in event file %s\n",
                  info->mission, param->in_file);
                ahlog_info(HIGH, __func__, "but TELESCOP = %s in attitude file %s\n",
                  info->att[sys]->af->mission, info->att[sys]->af->name);
              }
              
              /* Read extra keywords from SKY attitude file 
               * Note: the coord system before SKY is: 
               *            info->teldef->n_coordsys - 2 */
              
              /* +++ 20160111 KLR If time ever permits, these keywords should 
               *                  be added to the ATTFILE struct. */
              
              if (sys == info->teldef->n_coordsys - 2) {
                fits_read_key_log(info->att[sys]->af->fp, "ABERRAT", &(param->att_annaber), NULL, &status);
                if (KEY_NO_EXIST == status) {
                  ahlog_info(LOW, __func__, "ABERRAT is not present in attitude file %s\n", att_filename);
                }
                status = 0;
                fits_read_key_log(info->att[sys]->af->fp, "ABERORB", &(param->att_orbaber), NULL, &status);
                if (KEY_NO_EXIST == status) {
                  ahlog_info(LOW, __func__, "ABERORB is not present in attitude file %s\n", att_filename);
                }
                status = 0;
                
                if (param->att_annaber || param->att_orbaber) {
                  ahlog_info(HIGH, __func__, "The aberration correction has already been done on the attitude and will not be done again in coordevt\n");
                }
                
                fits_read_key_log(info->att[sys]->af->fp, "FOLOWSUN", &(param->att_followsun), NULL, &status);
                /* If this keyword is missing from the attitude file (or 
                 * present with value F, which shouldn’t happen), but 
                 * ABERRAT=T, then param->att_followsun = 1.  This is because 
                 * the only aberration correction done in aberattitude is with 
                 * the equivalent of followsun=yes. */
                if ( ((KEY_NO_EXIST == status) || (0 == param->att_followsun)) && 
                     (0 != param->att_annaber) ) {
                  param->att_followsun = 1;
                }
                
                fits_read_key_log(info->att[sys]->af->fp, "INVABERR", &(param->att_invaberr), NULL, &status);
                fits_read_key_log(info->att[sys]->af->fp, "INVOABER", &(param->att_invoaber), NULL, &status);
                
              }
              
              break;
            } /* end if sys matches */
          } /* end for a */
        } /* end if not IDENTITY */

        /* Quit if no matching attitude file was found. */

        if (0 == info->att_identity_flags[sys] && NULL == info->att[sys])
        {
          ahlog_err(__func__, "No compatible attitude file found for transformation %s%s%s.\n",
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1]);
          return 1;
        }
              
      }
      else /* Transformation type is not of SKYATT type. */
      {
        /* Display message and initialize to null the attitude arrays if no attitude file is needed for this system. */

        ahlog_info(3, __func__, "No attitude file needed for transformation %s%s%s.\n", 
          info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1]);
              
        info->att[sys] = NULL;
        info->att_filenames[sys] = NULL;
      }
    } /* end for sys */
  } /* end if use_att */

  return 0;
}


/* -------------------------------------------------------------------- */

/* Initialize orbit file for reading. */
int initOrbFile
(
  INFO* info /* The INFO structure */
  )
{
  const ORBINTERP orb_interp = ORBI_WEIGHTED;

  PARAM* param = info->param; /* Shortcut */

  /* Do nothing if the orbital aberration correction is not needed. */
  if (0 == param->really_do_orbaber) return 0;

  /* Initialize the event counters associated with orbit. */
  info->n_events_no_orbit = 0;
  info->first_event_with_orbit = -1;
  info->last_event_with_orbit = -1;

  /* Open the orbit file and initialize the orb structure. */
  info->orb = openGenOrbFile(param->orb_file, param->orb_extension, param->vel_col_name,
    param->vel_format, orb_interp);

  /* Complain and quit if the orbit file couldn't be opened and the orb structure initialized. */
  if (NULL == info->orb)
  {
    ahlog_err(__func__, "Orbit file could not be opened.\n");
    return 1;
  }
  return 0;
}

/* -------------------------------------------------------------------- */

/* Check that the needed coordinate columns exist in the event
   extension of the event file.  If any output columns are missing, add
   them. */
int checkCoordinateColumns
(
  fitsfile* fp, /* Event file pointer */
  iteratorCol* fits_col, /* Iterator column structures */
  int n_cols, /* Number of iterator columns */
  LONGLONG** nullcol, /* Array of pointers nullcol[col] pointing to stored null values */
  PARAM* param /* Parameter structure */
  )
{
  int iotype = InputOutputCol; /* I/O type for a column */
  char colname[FLEN_VALUE]; /* Column name */
  int col = 0; /* Column index */
  int colnumber = 0; /* Column number */
  int n_eventcol = 0; /* Number of columns in event file */
  int status = 0; /* CFITSIO statu s*/
  int errorcount = 0; /* Count of FITS errors */
  char tempstring[STRBUFFLEN]; /* Reusable string */
  char tformat[FLEN_VALUE]; /* TFORM value */
  char tdisp[FLEN_VALUE]; /* TDISP value */
  int datatype = TLONGLONG; /* Expected column data type */
  int typecode = 0; /* Existing column data type */
  long repeat = 0; /* Number of elements in column */
  long width = 0; /* Column width */
  LONGLONG nullvalue; /* Column null value; LONGLONG is in fitsio.h */
  long lng_nullvalue; /* Column null value */

  /* Initialize strings */
  strcpy(colname, "");
  strcpy(tempstring, "");
  strcpy(tformat, "");
  strcpy(tdisp, "");

  /* Read the number of columns in the event extension. */

  fits_get_num_cols(fp, &n_eventcol, &status);

  if (0 != status)
  {
    fits_report_error(stderr, status);
    ahlog_err(__func__, "Could not read number of columns in event extension of event file.");
    return 1;
  }

  /* Loop through the expected columns, check for existence and
     format, add missing output columns, ensure any integer null
     values exist. */

  for(col = 0; col < n_cols; col++)
  {
    strcpy(colname, fits_iter_get_colname(&fits_col[col]));
    iotype = fits_iter_get_iotype(&fits_col[col]);
    datatype = fits_iter_get_datatype(&fits_col[col]);
    ahlog_debug(__func__, __FILE__, __LINE__, "Searching for column %s #%d iotype %d.\n",  colname, col, iotype);

    fits_get_colnum(fp, CASEINSEN, colname, &colnumber, &status);
      
    if (COL_NOT_FOUND == status && InputCol == iotype)
    {
      /* Complain if an input column is not found. */

      ahlog_err(__func__, "Input column %s not found in the event extension.\n", colname);
      errorcount++;
      status = 0;
    }
    else if (COL_NOT_FOUND == status && OutputCol == iotype)
    {
      /* Add missing output column. */

      ahlog_info(3, __func__, "Output column %s not found in the event extension. Adding that column.\n", colname);

      status = 0;

      if (TLONGLONG == datatype)
      {
        strcpy(tformat, "1J");
        strcpy(tdisp, "I5");
      }
      else
      {
        strcpy(tformat, "1D");
        strcpy(tdisp, "F14.8");
      }

      fits_get_num_cols(fp, &n_eventcol, &status);
  
      fits_insert_col(fp, n_eventcol+1, colname, tformat, &status);
      ahlog_debug(__func__, __FILE__, __LINE__, "status after adding column %s = %d\n",  colname, status);

      sprintf(tempstring, "TDISP%d", n_eventcol+1);
      fits_update_key_str(fp, tempstring, tdisp, "", &status);


      sprintf(tempstring, "TNULL%d", n_eventcol+1);
      if (TLONGLONG == datatype)
        fits_update_key_lng(fp, tempstring, param->j_null_value, "", &status);
          
      if (0 != status)
      {
        fits_report_error(stderr, status);
        ahlog_err(__func__, "Could not create output column %s in the event extension.\n", colname);
        errorcount++;
      }
      else
      {
        ahlog_info(3, __func__, "Created output column %s in the event extension.\n", colname);
      }
      status = 0;
    }
    else if (0 != status)
    {
      /* Complain if the column search failed. */

      ahlog_err(__func__, "Could not search for column %s in event extension.\n", colname);
      fits_report_error(stderr, status);
      errorcount++;
    }
    else 
    {
      /* The column was found.  Now check if the column is of the right format. */

      fits_get_coltype(fp, colnumber, &typecode, &repeat, &width, &status);

      if (1 != repeat)
      {
        ahlog_err(__func__, "The format of column %s of the input event extension has a repeat count of %d instead of the required count of 1.\n",
          colname, repeat);
        errorcount++;
      }

      if (TLONGLONG == datatype && TBYTE != typecode && TSHORT != typecode && TLONG != typecode && TLONGLONG != typecode)
      {
        ahlog_err(__func__, "Column %s of the input event extension must have an integer format (1B, 1I, 1J, or 1K), but its format is of a different type.\n", colname);
        errorcount++;
      }
      else if (TDOUBLE == datatype && TFLOAT != typecode && TDOUBLE != typecode)
      {
        ahlog_err(__func__, "Column %s of the input event extension must have a floating-point format (1E or 1D), but its format is of a different type.\n", colname);
        errorcount++;
      }

    }

  }

  /* Give up if we've had to complain. */

  if (0 < errorcount)
  {
    ahlog_err(__func__, "There were problems finding, creating, or using the necessary columns in the event extension.\n");
    return 1;
  }


  /* Retrieve the null value for output integer coordinate columns. If the TNULL keyword is missing
   * from the event file, add it. */
  
  for(col = 0; col < n_cols; col++)
  {
    iotype = fits_iter_get_iotype(&fits_col[col]);
    datatype = fits_iter_get_datatype(&fits_col[col]);
      
    /* Only deal with integer output coordinate columns. 
     * CFITSIO has a predefined floating-point null value that does not require mention 
     * in a keyword, so floating-point columns should be skipped here. */

    /* if (TLONGLONG == datatype && OutputCol == iotype) */
    if (TLONGLONG == datatype)
    {
      /* Retrieve the column number and then look for the corresponding TNULL keyword. */
          
      strcpy(colname, fits_iter_get_colname(&fits_col[col]));
      status = 0;
      fits_get_colnum(fp, CASEINSEN, colname, &colnumber, &status);
      
      /* Make sure the equivalent type is valid.  In particular, TZEROn and TSCALn, if
       * present, must be used to implement unsigned types and not physical units.
       * cfitsio will confirm valid usage by returning an unsigned integer type as
       * the typecode here. */

      status = 0;
      fits_get_eqcoltype(fp, colnumber, &typecode, &repeat, &width, &status);
      if (TBYTE != typecode && TSBYTE != typecode  && TSHORT != typecode
        && TUSHORT != typecode && TLONG != typecode && TULONG != typecode
        && TLONGLONG != typecode) {
        ahlog_err(__func__, "Column %s of the input event extension must have an integer equivalent type, but does not.\n", colname);
        return 1;
      }
      sprintf(tempstring, "TNULL%d", colnumber);
      status = 0;
      fits_read_key_lnglng(fp, tempstring, &nullvalue, NULL, &status);

      if (0 == status)
      {
        /* If the TNULL keyword is found, store the value in the appropriate
         * nullx[sys] or nully[sys] element using the nullcol[col] pointer. */

        *(nullcol[col]) = nullvalue;
      }
      else if (KEY_NO_EXIST == status)
      {
        /* If the TNULL keyword is not found, insert the keyword into the header
         * with the user-given null value, and store that value. */

        status = 0;
        switch (typecode) {
          case TLONG: nullvalue = param->j_null_value; break;
          case TBYTE: nullvalue = param->b_null_value; break;
          case TSHORT: nullvalue = param->i_null_value; break;
          case TUSHORT: nullvalue = param->ui_null_value; break;
          case TULONG: nullvalue = param->uj_null_value; break;
          case TSBYTE: nullvalue = param->sb_null_value; break;
          case TLONGLONG: nullvalue = param->k_null_value; break;
          default:
            ahlog_err(__func__, "Invalid typecode for integer coordinate column found in processing null values.");
            return 1;
        }

        /* Range check */
        if (
          ((TSBYTE == typecode || TBYTE == typecode) &&
            (0 > nullvalue || 255 < nullvalue)) ||
          ((TSHORT == typecode || TUSHORT == typecode) &&
            (-32768 > nullvalue || 32767 < nullvalue)) ||
          ((TLONG == typecode || TULONG == typecode) &&
            (-2147483648 > nullvalue || 2147483647< nullvalue))) {
              ahlog_err(__func__, "Attempt to set null value %d for typecode %d.\n", *(nullcol[col]), typecode);
              return 1;
        }

        /* The conversion routines set null values directly in the data arrays
         * rather than through the cfitsio interface; therefore, the values need
         * to be in the data range rather than in the FITS binary column domain. */

        switch (typecode) {
          case TLONG: *(nullcol[col]) = nullvalue; break;
          case TBYTE: *(nullcol[col]) = nullvalue; break;
          case TSHORT: *(nullcol[col]) = nullvalue; break;
          case TUSHORT: *(nullcol[col]) = nullvalue + 32768; break;
          case TULONG: *(nullcol[col]) = nullvalue + 2147483648; break;
          case TSBYTE: *(nullcol[col]) = nullvalue - 128; break;
          case TLONGLONG: *(nullcol[col]) = nullvalue; break;
          default:
            ahlog_err(__func__, "Invalid typecode for integer coordinate column found in processing null values.\n");
            return 1;
        }
        lng_nullvalue = (long) nullvalue;
        fits_update_key_lng(fp, tempstring, lng_nullvalue, "Null value for this column", &status);
        if (0 != status)
        {
          ahlog_err(__func__, "Could not insert the %s keyword into the events extension of event file %s.\n",
            tempstring, param->in_file);
          return 1;
        }
              
        ahlog_info(3, __func__, "Set null value keyword %s = %ld in the event extension.\n",
          tempstring, lng_nullvalue);
      }
      else if (0 != status)
      {
        /* Complain and quit if the TNULL check failed for any other unlikely reason. */

        ahlog_err(__func__, "Could not check for the %s keyword in events extension of event file %s.\n",
          tempstring, param->in_file);
        return 1;
      }
    } /* end if TLONGLONG and OutputCol */
  } /* end for col */

  return 0;

}

/* -------------------------------------------------------------------- */

/* convertToHigherCoordinates is a CFITSIO Iterator work function.  The
   iterator framework reads a block of rows from the event table of
   the event file.  updateCoordinates performs the coordinate
   conversions and fills the data arrays for the iterator to return to
   the event table.  */

int convertToHigherCoordinates
(
  long total_rows, /* Number of rows in event file */
  long offset, /* Number of initial rows to skip */
  long first_row, /* Number of first row to work on for this function call. */
  long n_rows, /* Number of rows to work on for this function call. */
  int ncols, /* Number of iterator columns */
  iteratorCol *fits_col, /* iterator column structure array */
  void* void_info /* structure of additional info (INFO structure */
  ) 
{
  /* Declare pointer shortcuts and recast the info structure */

  INFO* info = (INFO*) void_info;
  PARAM* param = info->param;
  TELDEF2* teldef = info->teldef;

  /* Declare coord. sys., MULTISEG property, and table row indices. */

  int sys = 0;
  int prop = 0;
  int row = 0;

  double percent_done = 0.; /* Progress indicator */

/* Declare shortcuts for FITS column arrays. */  

  double* time = info->time;       /* time[row] */
  int** segs = info->segs;         /* segs[sys][row] */
  LONGLONG** intx = info->intx;         /* intx[sys][row] */
  LONGLONG** inty = info->inty;         /* inty[sys][row] */
  double** floatx = info->floatx;  /* floatx[sys][row] */
  double** floaty = info->floaty;  /* floaty[sys][row] */
  int*** props = info->props;      /* props[sys][prop][row] */

  /* Declare coordinate variables. */

  double x = 0.; /* The x coordinate being calculated. */
  double y = 0.; /* The y coordinate being calculated. */
  double prev_x = 0.; /* The x coordinate from the previous system. */
  double prev_y = 0.; /* The y coordinate from the previous system. */
  int px_x = 0; /* The rounded version of x. */
  int px_y = 0; /* The rounded version of y. */

  /* Declare random coordinate offsets. */

  double dx = 0.; 
  double dy = 0.;

  int convertstatus = 0; /* 0 means the conversion succeeded, 1 means it failed. */
  int status = 0; /* 0 means the aberration lookup succeeded, 1 means it failed. */
  double mjd = 0.; /* Modified Julian date */
  int is_sky_sys = 0; /* Flag if a system is the sky system */
  int did_event_fail = 0; /* Flag if an event's computations failed */
  int attitude_needs_updated = 1; /* Flag if the attitude quat needs to be updated (1=yes, 0=no). */
  int orbvelocity_needs_updated = 1; /* Flag if the attitude quat needs to be updated (1=yes, 0=no). */

  /* Declare windowing offsets values and property numbers. */

  long window_offset_x = 0;
  long window_offset_y = 0;
  long winpropx = 0;
  long winpropy = 0;

  /* Declare strings for message output. */

  char x1dbg[FLEN_VALUE];
  char y1dbg[FLEN_VALUE];
  char x2dbg[FLEN_VALUE];
  char y2dbg[FLEN_VALUE];

  strcpy(x1dbg, "");
  strcpy(y1dbg, "");
  strcpy(x2dbg, "");
  strcpy(y2dbg, "");

  info->total_rows = total_rows;

  /* Get the allocated column arrays from the column structures. */

  /* Get the time column if needed (when attitude files are used). */

  if (NULL != info->time_col)
    time = (double*) fits_iter_get_array(info->time_col);

  for(sys = param->startsys; sys < teldef->n_coordsys; sys++)
  {
    /* Get segment columns for RAWTODET transformations. */

    if (sys < teldef->n_coordsys - 1 && info->seg_cols[sys] != NULL)
    {
      segs[sys] = (int*) fits_iter_get_array(info->seg_cols[sys]);
    }
    
    /* Get integer x,y coordinate columns. */

    if (0 != param->includeintcol && info->intx_cols[sys] != NULL)
    {
      intx[sys] = (LONGLONG*) fits_iter_get_array(info->intx_cols[sys]);
    }

    if (0 != param->includeintcol && info->inty_cols[sys] != NULL)
    {
      inty[sys] = (LONGLONG*) fits_iter_get_array(info->inty_cols[sys]);
    }

    /* Get floating-point x,y coordinate columns. */
    
    if (NULL != info->floatx_cols[sys])
    {
      floatx[sys] = (double*) fits_iter_get_array(info->floatx_cols[sys]);
      floatx[sys][0] = DOUBLENULLVALUE;
    }

    if (NULL != info->floaty_cols[sys])
    {
      floaty[sys] = (double*) fits_iter_get_array(info->floaty_cols[sys]);
      floaty[sys][0] = DOUBLENULLVALUE;
    }

    /* Get MULTISEG property columns. */

    if (sys <= param->stopsys && sys < teldef->n_coordsys - 1 && e_TT_MULTISEG == teldef->trtypes[sys])
    {
      for(prop = 0; prop < teldef->multisegparam[sys]->n_properties; prop++)
      {
        if (e_PL_COLUMN == info->prop_locations[sys][prop])
        {
          props[sys][prop] = (int*) fits_iter_get_array(info->prop_cols[sys][prop]);
          /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ props[sys=%d][prop=%d]=%d\n",  sys, prop, props[sys][prop]); */
        }
      }
    }
  }

  /* Work on each row and do the coordinate conversions. */

  for(row = 1; row <= n_rows; row++)
  {
    /* Initialize the event failure status to OK before any transformations are attempted. */

    did_event_fail = 0;

    /* Check for null event time if attitude files are used. */
    
    if (0 != param->use_att && NULL != time && DOUBLENULLVALUE == time[row])
    {
      info->n_events_null_time++;
    }

    /* Get initial coordinates prev_x and prev_y for the startsys
       system. For each of the x and y coordinates, look for a
       floating point value or integer value based on the value of
       startwithfloat.  If neither exists, then this must be a pixel
       map RAW system, where each segment has only one pixel, and
       that value is given by first_pixel_x or _y. */

    if (0 != param->startwithfloat && NULL != info->floatx_cols[param->startsys])
    {
      prev_x = floatx[param->startsys][row];
    }
    else if (NULL != info->intx_cols[param->startsys])
    {
      prev_x = (double) intx[param->startsys][row];

      if (prev_x == info->nullx[param->startsys])
        prev_x = DOUBLENULLVALUE;
    }
    else
    {
      prev_x = teldef->coordsys[param->startsys][teldef->min_segment[param->startsys]]->first_pixel_x;
    }

    if (0 != param->startwithfloat && info->floaty_cols[param->startsys] != NULL)
    {
      prev_y = floaty[param->startsys][row];
    }
    else if (NULL != info->inty_cols[param->startsys])
    {
      prev_y = (double) inty[param->startsys][row];

      if (prev_y == info->nully[param->startsys])
        prev_y = DOUBLENULLVALUE;
    }
    else
    {
      prev_y = teldef->coordsys[param->startsys][teldef->min_segment[param->startsys]]->first_pixel_y;
    }
    
    /* Loop through the coordinate transformations for this row. */

    for(sys = param->startsys; sys < teldef->n_coordsys - 1; sys++)
    {
      /* Don't change any coordinate column values in the destination
         system if the destination system is outside the range of
         conversions and blank_col is false. */
        
      if (0 == param->blank_col && sys >= param->stopsys)
        continue;

      /* Determine if the destination system is a sky coordinate system. */
        
      is_sky_sys = (sys + 1 == teldef->sky_sys);

      /* If the destination system is outside the range of conversions and
         blank_col is true, then fill in those coordinate columns
         with null values. */

      if (0 != param->blank_col && sys >= param->stopsys)
      {
        if (0 != param->includeintcol)
        {
          intx[sys + 1][row] = info->nullx[sys + 1];
          inty[sys + 1][row] = info->nully[sys + 1];
        }
        if (0 != param->includefloatcol || (0 != is_sky_sys && 0 != param->includefloatskycol))
        {
          floatx[sys + 1][row] = DOUBLENULLVALUE;
          floaty[sys + 1][row] = DOUBLENULLVALUE;
        }
        continue;
      }
        
      /* If transformation sys->(sys+1) is within the range of conversions, then
         do the conversions. */

      convertstatus = 0; /* 0 means status is OK. */

      if (DOUBLENULLVALUE == prev_x || DOUBLENULLVALUE == prev_y
         || (param->blank_col && sys >= param->stopsys) )
      {
        /* If the previous coordinates were null, then the next
         * coordinates cannot be calculated, so set them to null. 
         * Also set coordinates to null if this system is after the 
         * last (stopsys) system and blank_col is true.
         */

        x = DOUBLENULLVALUE;
        y = DOUBLENULLVALUE;
        convertstatus = 1;
        did_event_fail = 1;
      }
        

      /* Add a random amount between +-0.5 of random_px_scale to each 
       * coordinate if randomization is enabled for this system. */

      if (0 != param->dorandomization && param->randsys == sys)
      {
        dx = getRandomShift(info->random_px_scale);
        dy = getRandomShift(info->random_px_scale);
      }
      else
      {
        dx = 0.;
        dy = 0.;
      }

      /* Perform the correct transformation based on the transformation type. */

      if (0 == convertstatus && e_TT_RAWTODET == teldef->trtypes[sys])
      {
        convertstatus = convertToHigherCoordRawtodet(teldef, prev_x + dx, prev_y + dy, &x, &y, sys, 
                                                     teldef->rawtodetparam[sys]->n_segs == 1 ? teldef->min_segment[sys]
                                                     : segs[sys][row]);

        if (0 != convertstatus)
          info->n_events_no_rawtodet[sys]++;
      }
      else if (0 == convertstatus && e_TT_BASIC == teldef->trtypes[sys])
      {
        convertstatus = convertToHigherCoordBasic(teldef, prev_x + dx, prev_y + dy, &x, &y, sys);
      }
      else if (0 == convertstatus && e_TT_MULTISEG == teldef->trtypes[sys])
      {
        for(prop = 0; prop < teldef->multisegparam[sys]->n_properties; prop++)
        {
          /* The detector properties must be retrieved from
           * the right place: either a column of the event
           * table or the event header keyword. */

          if (e_PL_COLUMN == info->prop_locations[sys][prop])
            info->rowprops[sys][prop] = props[sys][prop][row];
          else if (e_PL_KEYWORD == info->prop_locations[sys][prop])
            info->rowprops[sys][prop] = info->prop_key_values[sys][prop];
          else
          {
            ahlog_err(__func__, "A MULTISEG property was not determined to be either a column or keyword in the event file.\n");
            return 1;
          }
        }

        /* Determine the windowing x-offset for this event. */

        if (0 != teldef->multisegparam[sys]->use_multiple_winoffx)
        {
          /* There are multiple windowing offset keywords, so find the value of the associated property from the event file
           * to determine which offset to use. Check that the property value is in the TelDef range of the property. */
                
          winpropx = info->rowprops[sys][teldef->multisegparam[sys]->winpropx_num];
          if (winpropx < teldef->multisegparam[sys]->min_winpropx || winpropx > teldef->multisegparam[sys]->max_winpropx)
          {
            /* The value of the property is out-of-range, so flag this to avoid bothering with the coordinate converstion. */
                    
            window_offset_x = 0;
            convertstatus = 1;
          }
          else
          {
            /* The value of the property is in-range, so find the associated windowing offset value. */

            window_offset_x = info->window_offsets_x[ sys ][ info->rowprops[sys][teldef->multisegparam[sys]->winpropx_num] ];
          }
        }
        else
        {
          /* A single offset value is used because windowing is disabled (offset = 0) or only one offset keyword has been read. */

          window_offset_x = info->window_offsets_x[sys][teldef->multisegparam[sys]->min_winpropx];
        }


        /* Determine the windowing y-offset for this event. */

        if (0 != teldef->multisegparam[sys]->use_multiple_winoffy)
        {
          /* There are multiple windowing offset keywords, so find the value of the associated property from the event file
           * to determine which offset to use. Check that the property value is in the TelDef range of the property. */
                
          winpropy = info->rowprops[sys][teldef->multisegparam[sys]->winpropy_num];
          if (winpropy < teldef->multisegparam[sys]->min_winpropy || winpropy > teldef->multisegparam[sys]->max_winpropy)
          {
            /* The value of the property is out-of-range, so flag this to skip the coordinate converstion. */
                    
            window_offset_y = 0;
            convertstatus = 1;
          }
          else
          {
            /* The value of the property is in-range, so find the associate windowing offset value. */

            window_offset_y = info->window_offsets_y[ sys ][ info->rowprops[sys][teldef->multisegparam[sys]->winpropy_num] ];
          }
        }
        else
        {
          /* A single offset value is used because windowing is disabled (offset = 0) or only one offset keyword has been read. */

          window_offset_y = info->window_offsets_y[sys][teldef->multisegparam[sys]->min_winpropy];
        }
            
        if (0 == convertstatus)
          convertstatus = convertToHigherCoordMultiseg(teldef, prev_x + dx, prev_y + dy, &x, &y, 
                                                       sys, info->rowprops[sys], 
                                                       window_offset_x, window_offset_y);

        if (0 != convertstatus)
          info->n_events_no_multiseg[sys]++;

      }
      else if (0 == convertstatus && e_TT_SKYATT == teldef->trtypes[sys])
      {

        /* Check if event time is NULL.  */

        if (DOUBLENULLVALUE == time[row])
        {
          info->n_events_no_attitude[sys]++;
          if (0 != param->really_do_orbaber) info->n_events_no_orbit++;
          convertstatus = 1;
        }
        else if (0 == info->att_identity_flags[sys] 
          && 0 == isInExtrapolatedAttFile(info->att[sys]->af, time[row]))
        {
          /* Event time is not within the attitude file time range. */

          info->n_events_no_attitude[sys]++;
          convertstatus = 1;
        }
        else if (0 != param->really_do_orbaber && 0 == isTimeWithinGenOrbFile(info->orb, time[row]))
        {
          /* Orbital aberration correction is enabled, but the 
           * event time is not within the orbit file time range. */

          info->n_events_no_orbit++;
          convertstatus = 1;
        }
        else 
        {

          /* Time is within attitude and orbit file time ranges.  In other
           * words, event time is within the attitude file time range, and if
           * the orbit file is needed, the event time is also within that
           * time range. Determine if the attitude or the orbital velocity
           * needs to be updated.
           *
           * If attitude is identity, then time is counted as being 
           * always in range. */

          if (0 == info->att_identity_flags[sys]) 
          {
            if (1 < row && time[row] == info->event_time_at_last_quat[sys] && 0 == isQuatNull(info->q))
              attitude_needs_updated = 0;
            else
              attitude_needs_updated = 1;
          }
          else 
          {
            /* Setting the quaternion to identity counts as an update. */
            attitude_needs_updated = 1;
          }

          if ((0 != param->really_do_orbaber && 1 < row
              && time[row]==info->event_time_at_last_orbit
              && 0 != info->is_orbvel_valid) ||
             (0 == param->really_do_orbaber))
            orbvelocity_needs_updated = 0;
          else
            orbvelocity_needs_updated = 1;

          if (0 == attitude_needs_updated && 0 == orbvelocity_needs_updated) 
          {
            /* This event occurred at the same time as the previous event,
             * the previous quat is valid, and the orbital velocity (if used)
             * is still valid, so there is no need to re-read the attitude
             * file or orbit file, nor to re-calculate the aberration.
             */
            info->last_event_with_attitude[sys] = row + first_row - 1;
            if (0 != param->really_do_orbaber) info->last_event_with_orbit = row + first_row - 1;
            convertstatus = repeatConvertToHigherCoordSkyAtt( teldef, prev_x+dx, prev_y+dy, &x, &y, sys);
            info->n_events_no_att_or_orb_update++;
          }
          else
          {
            /* This is a new time or the previous quat or orbital velocity if
             * used was null, so we have to read the attitude file and, if
             * the transformation is to sky coordinates and aberration
             * correction is enabled, calculate the aberration.
             */
            if (0 != attitude_needs_updated)
            {
              if (0 == info->att_identity_flags[sys]) 
              {
                /* We are not using the identity quat:  get quaternion from attitude file. */
                findQuatInGenAttFile(info->att[sys], info->q, time[row]);
                ahlog_debug(__func__, __FILE__, __LINE__, "from att file:  sys=%s t=%f info->q=[ %f %f %f %f ]\n",  
                  info->teldef->coordsysnames[sys], time[row], info->q->p[0], info->q->p[1],
                  info->q->p[2], info->q->p[3]);
              }
              else
              {
                /* Identity quaternion selected by user parameter. */
                setQuatToIdentity(info->q);
                ahlog_debug(__func__, __FILE__, __LINE__, "identity quat:  sys=%s t=%f info->q=[ %f %f %f %f ]\n",  
                  info->teldef->coordsysnames[sys], time[row], info->q->p[0], info->q->p[1],
                  info->q->p[2], info->q->p[3]);
              }

              if (0 != isQuatNull(info->q))
              {
                /* If info->q is returned as a null quat to flag a problem,
                 * then the conversion cannot be done, and the next
                 * coordinates should be set to null. This case occurs when
                 * the time margin is used for interpolation and a large gap
                 * in attitude file times surrounds the event time. */
                info->n_events_no_attitude[sys]++;
                convertstatus = 1;
                ahlog_info(LOW, __func__, "Null quaternion returned for row %d, time=%.15g\n", row, time[row]);
              }
              else
              {
                /* info->q is a valid quat, so remember the associated
                 * event time and continue with the coordinate
                 * conversion. */
                info->event_time_at_last_quat[sys] = time[row];

                if (0 != param->follow_sun && sys == teldef->n_coordsys - 2)
                {
                  /* Recalculate the Earth's velocity vector for this 
                   * event if follow_sun is enabled. Invert the velocity
                   * to invert the aberration correction if invert_aberration is enabled. */
                              
                  mjd = info->mjdref + time[row]/86400.;

                  /* The logic here is as follows:
                   * If the attitude update is not required, then there is no need for
                   *   aberration correction.
                   * Otherwise:
                   * A. If follow_sun is enabled and orbital velocity update is required,
                   *    then get total correction.
                   * B. If follow_sun is enabled and orbital velocity update is not required,
                   *    then get annual only.
                   * C. If follow_sun is disabled and orbital velocity update is required,
                   *    then get orbital only.
                   * D. If follow_sun is disabled and orbital velocity update is not required,
                   *    then no correction. */

                  if (0 != orbvelocity_needs_updated) 
                  {

                    /* A. Both annual & orbital */
                    info->is_orbvel_valid = findAberrationCorrection(
                      mjd, info->mjdref, info->orb, param->really_do_annaber,
                      param->really_do_orbaber, param->really_inv_annaber,
                      param->really_inv_orbaber, &(info->v_sat_total),
                      info->vhat_sat_total);
                    
                    if (0 == info->is_orbvel_valid) 
                      {
                        /* Orbital velocity lookup failed, so that the conversion
                         * cannot be done, and the next coordinates should be
                         * set to null. */
                        info->n_events_aber_ann_and_orb_failure++;
                        info->n_events_no_orbit++;
                        convertstatus = 1;
                      }
                    else
                      {
                        /* Orbital velocity lookup succeeded, so the associated
                         * event time is saved and coordinate converssion
                         * proceeds. */
                        info->n_events_aber_ann_and_orb_success++;
                        info->event_time_at_last_orbit = time[row];
                      }
                  }
                  else
                  {
                      /* B. Not doing the orbital aberration here. Annual only. */
                      status = findAberrationCorrection(
                        mjd, info->mjdref, info->orb, param->really_do_annaber,
                        0, param->really_inv_annaber,
                        0, &(info->v_sat_total),
                        info->vhat_sat_total);

                      if (0 == status)
                      {
                        info->n_events_aber_ann_only_success++;
                      }
                      else
                      {
                        ahlog_err(__func__, "Annual aberration calculation failed.\n");
                        return 1;
                      }

                  }
                }  /* End if follow_sun enabled */
                else
                {  /* Begin follow_sun disabled */
                  if (0 != orbvelocity_needs_updated)
                  {
                    /* C. Not doing the annual aberration here. Orbital only. */
                    mjd = info->mjdref + time[row]/86400.;
                    info->is_orbvel_valid = findAberrationCorrection(
                      mjd, info->mjdref, info->orb, 0,
                      param->really_do_orbaber, 0,
                      param->really_inv_orbaber, &(info->v_sat_total),
                      info->vhat_sat_total);

                    if (0 == info->is_orbvel_valid)
                    {
                      /* Orbital velocity lookup failed, so that the conversion
                       * cannot be done, and the next coordinates should be
                       * set to null. */
                      info->n_events_aber_orb_only_failure++;
                      info->n_events_no_orbit++;
                      convertstatus = 1;
                    }
                    else
                    {
                      /* Orbital velocity lookup succeeded, so the associated
                       * event time is saved and coordinate converssion
                       * proceeds. */
                      info->n_events_aber_orb_only_success++;
                      info->event_time_at_last_orbit = time[row];
                    }
                  } /* End of orbital used, and annual const or not used. */
                  if (0 == convertstatus)
                  {
                    calcTotalSatVelocity(
                      &(info->v_sat_total), info->vhat_sat_total,
                      info->v_earth_orbit, info->vhat_earth_orbit,
                      info->v_sat_orbit, info->vhat_sat_orbit);
                  }
                } /* End of follow_sun disabled */
              } /* End of quat not null */
            } /* End of attitude needs updating */

            if (0 == convertstatus)
            {
                          
              /* Do the SKYATT transformation. Disable the aberration
               * correction if the destination coordinate system is not sky
               * coordinates by passing 0 instead of info->v_sat_total. If
               * the dest. coord. sys. is sky coordinates but the aberration 
               * correction is disabled, then info->v_sat_total = 0 already. */

              convertstatus = convertToHigherCoordSkyAtt(
                teldef, prev_x+dx, prev_y+dy, &x, &y,
                sys, info->q, 
                (sys == teldef->n_coordsys - 2 ? info->v_sat_total : 0), 
                info->vhat_sat_total);

              if (sys == teldef->n_coordsys-2) {
                ahlog_info(LOW, __func__, "Satellite speed = %.7g; velocity unit vector = (%.7g, %.7g, %.7g)\n",
                  info->v_sat_total, 
                  info->vhat_sat_total[0], info->vhat_sat_total[1], info->vhat_sat_total[2]);
              }

              /* Track the first and last events with an attitude or orbit
               * file lookup. */

              if (-1 == info->first_event_with_attitude[sys])
                info->first_event_with_attitude[sys] = row + first_row - 1;
              info->last_event_with_attitude[sys] = row + first_row - 1;

              if (0 != param->really_do_orbaber) 
              {
                if (-1 == info->first_event_with_orbit)
                  info->first_event_with_orbit = row + first_row - 1;
                info->last_event_with_orbit = row + first_row - 1;
              }
            }
          }
              
        } /* end: if time is in attitude file time range */
      } /* end: which transformation type */
       
        /* Check that the newly calculated coordinates are within
         * the coordinate ranges specified by the TelDef file. Also
         * check that newly calculated coordinates are finite (not
         * inf nor nan) due to 2D transformation arithmetic.  */

      if (0 != convertstatus ||
         0 == isfinite(x) || 0 == isfinite(y) ||
         x < teldef->coordsys[sys+1][teldef->min_segment[sys+1]]->min_x ||
         x > teldef->coordsys[sys+1][teldef->min_segment[sys+1]]->max_x ||
         y < teldef->coordsys[sys+1][teldef->min_segment[sys+1]]->min_y ||
         y > teldef->coordsys[sys+1][teldef->min_segment[sys+1]]->max_y 
        )
      {
        /* Set out-of-range and uncalculated coordinates to null. */

        x = DOUBLENULLVALUE;
        y = DOUBLENULLVALUE;
        px_x = info->nullx[sys + 1];
        px_y = info->nully[sys + 1];
        did_event_fail = 1;

        if (0 == convertstatus)
          info->n_events_out_of_range++;
      }
      else
      {
        /* Round the in-range coordinates. The tiny correction
         * allows correct rounding (up) of values that should be
         * exactly 0.5 above an integer but may be rounded down
         * due to numerical imprecision.  The case of a discrete
         * coordinate being exactly 0.5 above an integer is far
         * more common than the case of a continuous coordinate
         * that happens to land between 0.49999999 and 0.50000000
         * above an integer, so this small fudge provides much
         * more help than harm. */

        px_x = (int) floor(x + 0.5 + 1.e-8);
        px_y = (int) floor(y + 0.5 + 1.e-8);
      }

      /* Copy the new coordinates to the column arrays so that the CFITSIO 
       * iterator can write them to the event file. */

      if (0 != param->includeintcol)
      {
        intx[sys + 1][row] = px_x;
        inty[sys + 1][row] = px_y;
      }
      if (0 != param->includefloatcol || (0 != is_sky_sys && 0 != param->includefloatskycol))
      {
        floatx[sys + 1][row] = x;
        floaty[sys + 1][row] = y;
      }
        
      /* Display the coordinate conversion in debug mode. */

      if (0 != param->debug)
      {
        if (DOUBLENULLVALUE == prev_x)
          strcpy(x1dbg, "NULL");
        else
          sprintf(x1dbg, "%28.20e", prev_x);

        if (DOUBLENULLVALUE == prev_y)
          strcpy(y1dbg, "NULL");
        else
          sprintf(y1dbg, "%28.20e", prev_y);

        if (DOUBLENULLVALUE == x)
          strcpy(x2dbg, "NULL");
        else
          sprintf(x2dbg, "%28.20e", x);

        if (DOUBLENULLVALUE == y)
          strcpy(y2dbg, "NULL");
        else
          sprintf(y2dbg, "%28.20e", y);

        ahlog_debug(__func__, __FILE__, __LINE__, 
          "row=%d %s (%s, %s) --> %s (%s, %s) --round-> (%d %d)\n",
          row + first_row - 1, 
          teldef->coordsysnames[sys], x1dbg, y1dbg,
          teldef->coordsysnames[sys + 1], x2dbg, y2dbg, px_x, px_y);
      }
        
      /* The new coordinates must become the previous coordinates for the next 
       * transformation. */

      prev_x = x;
      prev_y = y;

    } /* End loop over systems */

    /* Update event counters */

    info->n_events_processed++;
    if (0 != did_event_fail)
      info->n_events_failed++;
    else
      info->n_events_succeeded++;

  } /* End loop over rows */


  /* Display a progress indicator (% done). */

  percent_done= 100* ((double)(first_row-offset+n_rows-1)/ 
                      (double)(total_rows-offset)      );

  if (floor(percent_done/10.) > floor(info->prev_percent_done/10.) || 100.0 == percent_done)
  {
    ahlog_info(2, __func__, "%g%% done.\n", percent_done);
  }
  info->prev_percent_done = percent_done;

  return 0;
}

/* -------------------------------------------------------------------- */

/* convertToLowerCoordinates is a CFITSIO Iterator work function.  The
   iterator framework reads a block of rows from the event table of
   the event file.  updateCoordinates performs the coordinate
   conversions and fills the data arrays for the iterator to return to
   the event table.  */

int convertToLowerCoordinates
(long total_rows, long offset, long first_row, long n_rows, 
 int ncols, iteratorCol* fits_col, void* void_info)
{
  /* Declare pointer shortcuts and recast the info structure */
  
  INFO* info = (INFO*) void_info;
  PARAM* param = info->param;
  TELDEF2* teldef = info->teldef;
  
  /* Declare coord. sys., MULTISEG property, and table row indices. */
  
  int sys = 0;
  int prop = 0;
  int row = 0;
  
  double percent_done = 0.; /* Progress indicator */
  
  /* Declare shortcuts for FITS column arrays. */  
  
  double* time = info->time;       /* time[row] */
  int** segs = info->segs;         /* segs[sys][row] */
  LONGLONG** intx = info->intx;         /* intx[sys][row] */
  LONGLONG** inty = info->inty;         /* inty[sys][row] */
  double** floatx = info->floatx;  /* floatx[sys][row] */
  double** floaty = info->floaty;  /* floaty[sys][row] */
  int*** props = info->props;      /* props[sys][prop][row] */

  /* Declare coordinate variables. */

  double x = 0.; /* The x coordinate being calculated. */
  double y = 0.; /* The y coordinate being calculated. */
  double prev_x = 0.; /* The x coordinate from the previous system. */
  double prev_y = 0.; /* The y coordinate from the previous system. */
  int px_x = 0; /* The rounded version of x. */
  int px_y = 0; /* The rounded version of y. */
 
  int convertstatus = 0; /* 0 means the conversion succeeded, 1 means it failed. */
  int status = 0; /* 0 means the aberration lookup succeeded, 1 means it failed. */
  double mjd = 0.; /* Modified Julian date */
  int is_sky_sys = 0; /* Flag if a system is the sky system */
  int attitude_needs_updated = 1; /* Flag if the attitude quat needs to be updated (1=yes, 0=no). */
  int orbvelocity_needs_updated = 1; /* Flag if the attitude quat needs to be updated (1=yes, 0=no). */
  int did_event_fail = 0; /* Flag if an event's computations failed */

  /* Declare windowing offsets values and property numbers */

  long window_offset_x = 0;
  long window_offset_y = 0;
  long winpropx = 0;
  long winpropy = 0;

  /* Declare strings for message output. */

  char x1dbg[FLEN_VALUE];
  char y1dbg[FLEN_VALUE];
  char x2dbg[FLEN_VALUE];
  char y2dbg[FLEN_VALUE];

  strcpy(x1dbg, "");
  strcpy(y1dbg, "");
  strcpy(x2dbg, "");
  strcpy(y2dbg, "");

  info->total_rows = total_rows;

  /* Get the allocated column arrays from the column structures. */

  /* Get the time column if needed (when attitude files are used). */

  if (NULL != info->time_col)
    time = (double*) fits_iter_get_array(info->time_col);

  for(sys = param->startsys; sys >= 0; sys--)
  {
    /* Get segment columns for RAWTODET transformations. */

    if (sys < teldef->n_coordsys - 1 && NULL != info->seg_cols[sys])
    {
      segs[sys] = (int*) fits_iter_get_array(info->seg_cols[sys]);
      /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ segs[sys=%d]=%d\n",  sys, (long) segs[sys]); */
    }
    
    /* Get integer x,y coordinate columns. */

    if (0 != param->includeintcol && NULL != info->intx_cols[sys])
    {
      intx[sys] = (LONGLONG*) fits_iter_get_array(info->intx_cols[sys]);
      /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ intx[sys=%d]=%d\n",  sys, (long) intx[sys]); */
    }

    if (0 != param->includeintcol && NULL != info->inty_cols[sys])
    {
      inty[sys] = (LONGLONG*) fits_iter_get_array(info->inty_cols[sys]);
      /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ inty[sys=%d]=%d\n",  sys, (long) inty[sys]); */
    }

    /* Get floating-point x,y coordinate columns. */
    
    if (NULL != info->floatx_cols[sys])
    {
      floatx[sys] = (double*) fits_iter_get_array(info->floatx_cols[sys]);
      /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ floatx[sys=%d]=%f\n",  sys, (long) floatx[sys]); */
      floatx[sys][0] = DOUBLENULLVALUE;
    }

    if (NULL != info->floaty_cols[sys])
    {
      floaty[sys] = (double*) fits_iter_get_array(info->floaty_cols[sys]);
      /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ floaty[sys=%d]=%f\n",  sys, (long) floaty[sys]); */
      floaty[sys][0] = DOUBLENULLVALUE;
    }

    /* Get MULTISEG property columns. */

    if (sys >= param->stopsys && sys < teldef->n_coordsys - 1 && e_TT_MULTISEG == teldef->trtypes[sys])
    {
      for(prop = 0; prop < teldef->multisegparam[sys]->n_properties; prop++)
      {
        if (e_PL_COLUMN == info->prop_locations[sys][prop])
        {
          props[sys][prop] = (int*) fits_iter_get_array(info->prop_cols[sys][prop]);
          /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ props[sys=%d][prop=%d]=%d\n",  sys, prop, props[sys][prop]); */
        }
      }
    }
  }

  /* Work on each row and do the coordinate conversions. */
  for(row = 1; row <= n_rows; row++)
  {
    /* Initialize the event failure status to OK before any transformations are attempted. */

    did_event_fail = 0;

    /* Check for null event time if attitude files are used. */
    
    if (0 != param->use_att && NULL != time && DOUBLENULLVALUE == time[row])
    {
      info->n_events_null_time++;
    }

    /* Get initial coordinates prev_x and prev_y for the startsys
       system. For each of the x and y coordinates, look for a
       floating point value or integer value based on the value of
       startwithfloat. One of these (int or float value) should be
       found for each coordinate, but as a precaution, set the
       coordinate to null if neither is found.*/

    if (0 != param->startwithfloat && NULL != info->floatx_cols[param->startsys])
      prev_x = floatx[param->startsys][row];
    else if (NULL != info->intx_cols[param->startsys])
    {
      prev_x = (double) intx[param->startsys][row];

      if (prev_x == info->nullx[param->startsys])
        prev_x = DOUBLENULLVALUE;
    }
    else
      prev_x = DOUBLENULLVALUE;

    if (0 != param->startwithfloat && NULL != info->floaty_cols[param->startsys])
      prev_y = floaty[param->startsys][row];
    else if (NULL != info->inty_cols[param->startsys])
    {
      prev_y = (double) inty[param->startsys][row];

      if (prev_y == info->nully[param->startsys])
        prev_y = DOUBLENULLVALUE;
    }
    else
      prev_y = DOUBLENULLVALUE;

    /* Loop through the coordinate transformations for this row. */

    for(sys = param->startsys - 1; sys >= 0; sys--)
    {
      /* ahlog_debug(__func__, __FILE__, __LINE__, "KBQ first_row=%d row=%d sys=%d\n",  first_row, row, sys); */
      /* Don't change any coordinate column values in the destination
         system if the destination system is outside the range of
         conversions and blank_col is false. */
      
      if (0 == param->blank_col && sys < param->stopsys)
        continue;

      /* Determine if the originating system is a sky coordinate system. */
        
      is_sky_sys = (sys + 1 == teldef->sky_sys);

      /* If the destination system is outside the range of conversions and
         blank_col is true, then fill in those coordinate columns
         with null values. */

      if (0 != param->blank_col && sys < param->stopsys)
      {
        if (0 != param->includeintcol)
        {
          intx[sys][row] = info->nullx[sys + 1];
          inty[sys][row] = info->nully[sys + 1];
        }
        if (0 != param->includefloatcol || (0 != is_sky_sys && 0 != param->includefloatskycol))
        {
          floatx[sys][row] = DOUBLENULLVALUE;
          floaty[sys][row] = DOUBLENULLVALUE;
        }
        continue;
      }

      /* If transformation sys->(sys+1) is within the range of conversions, then
         do the conversions. */

      convertstatus = 0; /* 0 means status is OK. */

      if (DOUBLENULLVALUE == prev_x || DOUBLENULLVALUE == prev_y
         || (param->blank_col && sys < param->stopsys) )
      {
        /* If the previous coordinates were null, then the next
         * coordinates cannot be calculated, so set them to null. 
         * Also set coordinates to null if this system is after the 
         * last (stopsys) system and blank_col is true.
         */

        x = DOUBLENULLVALUE;
        y = DOUBLENULLVALUE;
        convertstatus = 1;
        did_event_fail = 1;
      }

      /* Randomizing the coordinates would be done at this point,
       * but no randomization is needed for converting from higher-
       * to lower-level coordinates. */


      /* Perform the correct transformation based on the transformation type. */
      
      if (0 == convertstatus && e_TT_RAWTODET == teldef->trtypes[sys] && RM_CORNER_LIST == teldef->rawtodetparam[sys]->rawmethod)
      {
        /* For the pixel map transformation (x,y) --> pixel , no
         * transformation is needed because there are no x and y
         * coordinates in the lower coordinate system. Put some dummy values here. */
        
        x = teldef->coordsys[sys][teldef->min_segment[sys]]->first_pixel_x;
        y = teldef->coordsys[sys][teldef->min_segment[sys]]->first_pixel_y;
      }
      else if (0 == convertstatus && e_TT_RAWTODET == teldef->trtypes[sys] && RM_LINEAR_COEFF == teldef->rawtodetparam[sys]->rawmethod)
      {
        convertstatus = convertToLowerCoordRawtodet(teldef, &x, &y, prev_x, prev_y, sys, 
                                                    teldef->rawtodetparam[sys]->n_segs == 1 ? teldef->min_segment[sys]
                                                    : segs[sys][row]);

        if (0 != convertstatus)
          info->n_events_no_rawtodet[sys]++;
      }
      else if (0 == convertstatus && e_TT_BASIC == teldef->trtypes[sys])
      {
        convertstatus = convertToLowerCoordBasic(teldef, &x, &y, prev_x, prev_y, sys);
      }
      else if (0 == convertstatus && e_TT_MULTISEG == teldef->trtypes[sys])
      {
        for(prop = 0; prop < teldef->multisegparam[sys]->n_properties; prop++)
        {
          /* The detector properties must be retrieved from
           * the right place: either a column of the event
           * table or the event header keyword. */

          if (e_PL_COLUMN == info->prop_locations[sys][prop])
            info->rowprops[sys][prop] = props[sys][prop][row];
          else if (e_PL_KEYWORD == info->prop_locations[sys][prop])
            info->rowprops[sys][prop] = info->prop_key_values[sys][prop];
          else
          {
            ahlog_err(__func__, "A MULTISEG property was not determined to be either a column or keyword in the event file.\n");
            return 1;
          }
        }

        /* Determine the windowing x-offset for this event. */

        if (0 != teldef->multisegparam[sys]->use_multiple_winoffx)
        {
          /* There are multiple windowing offset keywords, so find the value of the associated property from the event file
           * to determine which offset to use. Check that the property value is in the TelDef range of the property. */
                
          winpropx = info->rowprops[sys][teldef->multisegparam[sys]->winpropx_num];
          if (winpropx < teldef->multisegparam[sys]->min_winpropx || winpropx > teldef->multisegparam[sys]->max_winpropx)
          {
            /* The value of the property is out-of-range, so flag this to avoid bothering with the coordinate converstion. */
                    
            window_offset_x = 0;
            convertstatus = 1;
          }
          else
          {
            /* The value of the property is in-range, so find the associated windowing offset value. */

            window_offset_x = info->window_offsets_x[ sys ][ info->rowprops[sys][teldef->multisegparam[sys]->winpropx_num] ];
          }
        }
        else
        {
          /* A single offset value is used because windowing is disabled (offset = 0) or only one offset keyword has been read. */

          window_offset_x = info->window_offsets_x[sys][teldef->multisegparam[sys]->min_winpropx];
        }


        /* Determine the windowing y-offset for this event. */

        if (0 != teldef->multisegparam[sys]->use_multiple_winoffy)
        {
          /* There are multiple windowing offset keywords, so find the value of the associated property from the event file
           * to determine which offset to use. Check that the property value is in the TelDef range of the property. */
                
          winpropy = info->rowprops[sys][teldef->multisegparam[sys]->winpropy_num];
          if (winpropy < teldef->multisegparam[sys]->min_winpropy || winpropy > teldef->multisegparam[sys]->max_winpropy)
          {
            /* The value of the property is out-of-range, so flag this to skip the coordinate converstion. */
                    
            window_offset_y = 0;
            convertstatus = 1;
          }
          else
          {
            /* The value of the property is in-range, so find the associate windowing offset value. */

            window_offset_y = info->window_offsets_y[ sys ][ info->rowprops[sys][teldef->multisegparam[sys]->winpropy_num] ];
          }
        }
        else
        {
          /* A single offset value is used because windowing is disabled (offset = 0) or only one offset keyword has been read. */

          window_offset_y = info->window_offsets_y[sys][teldef->multisegparam[sys]->min_winpropy];
        }
            
        if (0 == convertstatus)
          convertstatus = convertToLowerCoordMultiseg(teldef, &x, &y, prev_x, prev_y, 
                                                       sys, info->rowprops[sys], 
                                                       window_offset_x, window_offset_y);

        if (0 != convertstatus)
          info->n_events_no_multiseg[sys]++;

      }
      else if (0 == convertstatus && e_TT_SKYATT == teldef->trtypes[sys])
      {
        /* Check if event time is NULL.  */

        if (DOUBLENULLVALUE == time[row])
        {
          info->n_events_no_attitude[sys]++;
          if (0 != param->really_do_orbaber) info->n_events_no_orbit++;
          convertstatus = 1;
        }
        else if (0 == info->att_identity_flags[sys] 
          && 0 == isInExtrapolatedAttFile(info->att[sys]->af, time[row]))
        {
          /* Event time is not covered by attitude file time range. */

          info->n_events_no_attitude[sys]++;
          convertstatus = 1;
        } 
        else if (0 != param->really_do_orbaber && 0 == isTimeWithinGenOrbFile(info->orb, time[row])) 
        {
          /* Orbital aberration correction is enabled, but the event time is
           * not within the orbit file time range. */

          info->n_events_no_orbit++;
          convertstatus = 1;
        }
        else
        {

          /* Time is within attitude and orbit file time ranges.  In other
           * words, event time is within the attitude file time range, and if
           * the orbit file is needed, the event time is also within that
           * time range. Determine if the attitude or the orbital velocity
           * needs to be updated. 
           *
           * Essentially, if an identity quaternion is required, then
           * the time is considered always within range. */

          if (0 == info->att_identity_flags[sys]) 
          {
            if (1 < row && time[row] == info->event_time_at_last_quat[sys] && 0 == isQuatNull(info->q))
              attitude_needs_updated = 0;
            else
              attitude_needs_updated = 1;
          }
          else 
          {
            /* Setting the quaternion to identity counts as an update. */
            attitude_needs_updated = 1;
          }

          if ((0 != param->really_do_orbaber && 1 < row
              && time[row] == info->event_time_at_last_orbit && 0 != info->is_orbvel_valid)
              || 0 == param->really_do_orbaber)
            orbvelocity_needs_updated = 0;
          else
            orbvelocity_needs_updated = 1;

          if (0 == attitude_needs_updated && 0 == orbvelocity_needs_updated)
          {
            /* This event occurred at the same time as the previous event,
             * the previous quat is valid, and the previous orbital velocity
             * (if used) is still valid, so there is no need to re-read the
             * attitude file or orbit file, nor to re-calculate the
             * aberration.  */

            info->last_event_with_attitude[sys] = row + first_row - 1;
            if (0 != param->really_do_orbaber) info->last_event_with_orbit = row + first_row - 1;
            convertstatus = repeatConvertToLowerCoordSkyAtt(teldef, &x, &y, prev_x, prev_y, sys);
            info->n_events_no_att_or_orb_update++;
          }
          else
          {
            /* This is a new time or the previous quat or orbital velocity
             * was null, so we have to read the attitude file and, if the
             * transformation is to sky coordinates and aberration correcting
             * is enabled, calculate the aberration.  */

            if (0 != attitude_needs_updated)
            {
              
              if (0 == info->att_identity_flags[sys])
              {
                /* We are not using the identity quat:  get quaternion from attitude file. */
                findQuatInGenAttFile(info->att[sys], info->q, time[row]);
                ahlog_debug(__func__, __FILE__, __LINE__, "from att file:  sys=%s t=%f info->q=[ %f %f %f %f ]\n",  
                  info->teldef->coordsysnames[sys], time[row], info->q->p[0], info->q->p[1],
                  info->q->p[2], info->q->p[3]);
              }
              else
              {
                /* Identity quaternion selected by user parameter. */
                setQuatToIdentity(info->q);
                ahlog_debug(__func__, __FILE__, __LINE__, "identity quat:  sys=%s t=%f info->q=[ %f %f %f %f ]\n",  
                  info->teldef->coordsysnames[sys], time[row], info->q->p[0], info->q->p[1],
                  info->q->p[2], info->q->p[3]);
              }

              if (0 != isQuatNull(info->q))
              {
                /* If info->q is returned as a null quat to flag a problem, then the conversion cannot be done, 
                 * and the next coordinates should be set to null. This case occurs when the time margin
                 * is used for interpolation and a large gap in attitude file times surrounds the event time. */

                info->n_events_no_attitude[sys]++;
                convertstatus = 1;
              }
              else
              {
                /* info->q is a valid quat, so remember the associated
                 * event time and continue with the coordinate
                 * conversion. */

                info->event_time_at_last_quat[sys] = time[row];

                if (0 != param->follow_sun && sys == teldef->n_coordsys - 2)
                {
                  /* Recalculate the Earth's velocity vector for this 
                   * event if follow_sun is enabled. Invert the velocity
                   * to invert the aberration correction if invert_aberration is enabled. */
                              
                  mjd = info->mjdref + time[row]/86400.;

                  /* The logic here is as follows:
                   * If the attitude update is not required, then there is no need for
                   *   aberration correction.
                   * Otherwise:
                   * A. If follow_sun is enabled and orbital velocity update is required,
                   *    then get total correction.
                   * B. If follow_sun is enabled and orbital velocity update is not required,
                   *    then get annual only.
                   * C. If follow_sun is disabled and orbital velocity update is required,
                   *    then get orbital only.
                   * D. If follow_sun is disables and orbital velocity update is not required,
                   *    then no correction. */

                  if (0 != orbvelocity_needs_updated) 
                  {

                    /* A. Both annual & orbital */
                    info->is_orbvel_valid = findAberrationCorrection(
                      mjd, info->mjdref, info->orb, param->really_do_annaber,
                      param->really_do_orbaber, param->really_inv_annaber,
                      param->really_inv_orbaber, &(info->v_sat_total),
                      info->vhat_sat_total);
                    
                    if (0 == info->is_orbvel_valid) 
                    {
                      /* Orbital velocity lookup failed, so that the conversion
                       * cannot be done, and the next coordinates should be
                       * set to null. */
                      info->n_events_aber_ann_and_orb_failure++;
                      info->n_events_no_orbit++;
                      convertstatus = 1;
                    }
                    else
                    {
                      /* Orbital velocity lookup succeeded, so the associated
                       * event time is saved and coordinate converssion
                       * proceeds. */
                      info->n_events_aber_ann_and_orb_success++;
                      info->event_time_at_last_orbit = time[row];
                    }
                  }  /* End orbvelocity_needs_updated == true */
                  else
                  {  /* Begin orbvelocity_needs_updated == false */
                      /* B. Not doing the orbital aberration here. Annual only. */
                      status = findAberrationCorrection(
                        mjd, info->mjdref, info->orb, param->really_do_annaber,
                        0, param->really_inv_annaber, 0,
                        &(info->v_sat_total),
                        info->vhat_sat_total);

                      if (0 == status)
                      {
                        info->n_events_aber_ann_only_success++;
                      }
                      else
                      {
                        ahlog_err(__func__, "Annual aberration calculation failed.\n");
                        return 1;
                      }

                  }
                }  /* End follow_sun enabled */
                else
                {  /* Begin follow_sun disabled */
                  if (0 != orbvelocity_needs_updated)
                  {
                    /* C. Not doing the annual aberration here. Orbital only. */
                    info->is_orbvel_valid = findAberrationCorrection(
                      mjd, info->mjdref, info->orb, 0,
                      param->really_do_orbaber, 0,
                      param->really_inv_orbaber, &(info->v_sat_orbit),
                      info->vhat_sat_orbit);

                    if (0 == info->is_orbvel_valid)
                    {
                      /* Orbital velocity lookup failed, so that the conversion
                       * cannot be done, and the next coordinates should be
                       * set to null. */
                      info->n_events_aber_orb_only_failure++;
                      info->n_events_no_orbit++;
                      convertstatus = 1;
                    }
                    else
                    {
                      /* Orbital velocity lookup succeeded, so the associated
                       * event time is saved and coordinate converssion
                       * proceeds. */
                      info->n_events_aber_orb_only_success++;
                      info->event_time_at_last_orbit = time[row];
                    }
                  } /* End of orbital used, and annual const or not used. */
                  if (0 == convertstatus)
                  {
                    calcTotalSatVelocity(
                      &(info->v_sat_total), info->vhat_sat_total,
                      info->v_earth_orbit, info->vhat_earth_orbit,
                      info->v_sat_orbit, info->vhat_sat_orbit);
                  }
                } /* End of follow_sun disabled */
              } /* End of quat not null */
            } /* End of attitude needs updating */

            if (0 == convertstatus) 
            {

              /* Do the SKYATT transformation. Disable the aberration
               * correction if the destination coordinate system is not sky
               * coordinates by passing 0 instead of info->v_sat_total. If
               * the dest. coord. sys. is sky coordinates but the aberration
               * correction is disabled, then info->v_sat_total = 0 already. */

              convertstatus = 
                convertToLowerCoordSkyAtt(teldef, &x, &y, 
                  prev_x, prev_y, sys, info->q, 
                  (sys == teldef->n_coordsys - 2 ? info->v_sat_total : 0), 
                  info->vhat_sat_total);

              if (sys == teldef->n_coordsys-2) {
                ahlog_info(LOW, __func__, "Satellite speed = %.7g; velocity unit vector = (%.7g, %.7g, %.7g)\n",
                  info->v_sat_total,
                  info->vhat_sat_total[0], info->vhat_sat_total[1], info->vhat_sat_total[2]);
              }

              /* Track the first and last events with a valid attitude or
               * orbit file lookup. */

              if (-1 == info->first_event_with_attitude[sys])
                info->first_event_with_attitude[sys] = row + first_row - 1;
              info->last_event_with_attitude[sys] = row + first_row - 1;

              if (0 != param->really_do_orbaber) 
              {
                if (-1 == info->first_event_with_orbit)
                  info->first_event_with_orbit = row + first_row - 1;
                info->last_event_with_orbit = row + first_row - 1;
              }
            }
          } /* end: attitude or orbital velocity needs updating */
        } /* end: if time is in attitude and orbit file time range */
      } /* end: SKYATT transformation type */

      /* Check that the newly calculated coordinates are within
       * the coordinate ranges specified by the TelDef file. Also
       * check that newly calculated coordinates are finite (not
       * inf nor nan) due to 2D transformation arithmetic.  */

      if (0 != convertstatus ||
         0 == isfinite(x) || 0 == isfinite(y) ||
         x < teldef->coordsys[sys][teldef->min_segment[sys]]->min_x ||
         x > teldef->coordsys[sys][teldef->min_segment[sys]]->max_x ||
         y < teldef->coordsys[sys][teldef->min_segment[sys]]->min_y ||
         y > teldef->coordsys[sys][teldef->min_segment[sys]]->max_y 
        )
      {
        /* Set out-of-range and uncalculated coordinates to null. */

        x = DOUBLENULLVALUE;
        y = DOUBLENULLVALUE;
        px_x = info->nullx[sys];
        px_y = info->nully[sys];
        did_event_fail = 1;

        if (0 == convertstatus)
          info->n_events_out_of_range++;
      }
      else
      {
        /* Round the in-range coordinates. The tiny correction
         * allows correct rounding (up) of values that should be
         * exactly 0.5 above an integer but may be rounded down
         * due to numerical imprecision.  The case of a discrete
         * coordinate being exactly 0.5 above an integer is far
         * more common than the case of a continuous coordinate
         * that happens to land between 0.49999999 and 0.50000000
         * above an integer, so this small fudge provides much
         * more help than harm. */

        px_x = (int) floor(x + 0.5 + 1.e-8);
        px_y = (int) floor(y + 0.5 + 1.e-8);
      }

      /* Copy the new coordinates to the column arrays so that the
       * CFITSIO iterator can write them to the event file. Skip
       * this for RAWTODET pixel map transformations because there
       * are no x and y coordinate columns to write to. */

      if (0 == (e_TT_RAWTODET == teldef->trtypes[sys] && RM_CORNER_LIST == teldef->rawtodetparam[sys]->rawmethod))
      {
        if (0 != param->includeintcol)
        {
          intx[sys][row] = px_x;
          inty[sys][row] = px_y;
        }
        if (0 != param->includefloatcol || (0 != is_sky_sys && 0 != param->includefloatskycol))
        {
          floatx[sys][row] = x;
          floaty[sys][row] = y;
        }
      }

      /* Display the coordinate conversion in debug mode. */

      if (0 != param->debug)
      {
        if (DOUBLENULLVALUE == prev_x)
          strcpy(x1dbg, "NULL");
        else
          sprintf(x1dbg, "%28.20e", prev_x);

        if (DOUBLENULLVALUE == prev_y)
          strcpy(y1dbg, "NULL");
        else
          sprintf(y1dbg, "%28.20e", prev_y);

        if (DOUBLENULLVALUE == x)
          strcpy(x2dbg, "NULL");
        else
          sprintf(x2dbg, "%28.20e", x);

        if (DOUBLENULLVALUE == y)
          strcpy(y2dbg, "NULL");
        else
          sprintf(y2dbg, "%28.20e", y);

        ahlog_debug(__func__, __FILE__, __LINE__, 
          "row=%d %s (%s, %s) --> %s (%s, %s) --round-> (%d %d)\n",
          row + first_row - 1, 
          teldef->coordsysnames[sys + 1], x1dbg, y1dbg,
          teldef->coordsysnames[sys], x2dbg, y2dbg, px_x, px_y);
      }

      /* The new coordinates must become the previous coordinates for the next 
       * transformation. */

      prev_x = x;
      prev_y = y;

    } /* end loop over systems */
    
    /* Update event counters */

    info->n_events_processed++;
    if (0 != did_event_fail)
      info->n_events_failed++;
    else
      info->n_events_succeeded++;

  } /* end loop over rows */

  /* Display a progress indicator (% done). */

  percent_done= 100* ((double)(first_row-offset+n_rows-1)/ 
                      (double)(total_rows-offset)      );

  if (floor(percent_done/10.) > floor(info->prev_percent_done/10.) || 100.0 == percent_done)
  {
    ahlog_info(2, __func__, "%g%% done.\n", percent_done);
  }
  info->prev_percent_done = percent_done;

  return 0;
}

/* -------------------------------------------------------------------- */

/* Look at an attitude file and determine the destination coordinate 
 * system in the transformation the attitude should be used for. */
int getDestSysFromAttFile
(
  char* filename, /* Attitude filename */
  char* att_extension, /* Attitude extension */
  char* orig_sys_name, /* Originiating coordinate system name */
  TELDEF2* teldef /* TelDef structure */
  )
{
  /* Declare the CFITSIO status, file pointer, and extension type. */

  int status = 0;
  fitsfile* fp = NULL;
  int hdutype = ANY_HDU;

  /* Open the attitude file. */

  fits_open_file(&fp, filename, READONLY, &status);
  if (0 != status)
  {
    ahlog_err(__func__, "Could not open attitude file %s.\n", filename);
    return 1;
  }

  /* Move to the ATTITUDE header. */

  fits_movnam_hdu(fp, hdutype, att_extension, 0, &status);
  if (0 != status)
  {
    status = 0;
    fits_close_file(fp, &status);
    strcpy(orig_sys_name, "NONE");
    return 0;
  }

  /* Read the DESTSYS keyword. */

  fits_read_key_str(fp, "DESTSYS", orig_sys_name, NULL, &status);
  if (KEY_NO_EXIST == status)
  {
    /* If the DESTSYS keyword is not found in ATTITUDE extension header, then assume that
     * this attitude file is intended for the coordinate transformation that ends at SKY.
     */

    status = 0;
    fits_close_file(fp, &status);
    strcpy(orig_sys_name, teldef->coordsysnames[teldef->n_coordsys-1]);
    return 0;
  }
  else if (0 != status)
  {
    status = 0;
    fits_close_file(fp, &status);
    strcpy(orig_sys_name, "NONE");
    return 0 ;
  }
  else
  {
    /* If the keyword was found, then use the value. Nothing more to do. */

    fits_close_file(fp, &status);
    return 0;
  }
  
  return 0;
}

/* -------------------------------------------------------------------- */

/* Negate the components of a 3-vector. */
void negate3Vector
(
  double v[3] /* Vector to negate */
  )
{
  v[0] = -v[0];
  v[1] = -v[1];
  v[2] = -v[2];
}

/* -------------------------------------------------------------------- */

/* Display event counters. */
void displayEventCounters
(
  INFO* info /* INFO structure (pointer) containing the event counters */
  )
{
  int sys = 0; /* coord. sys. index */
  int min_sys = 0;
  int max_sys = 0;

  if (info->param->startsys < info->param->stopsys)
  {
    min_sys = info->param->startsys;
    max_sys = info->param->stopsys;
  }
  else
  {
    min_sys = info->param->stopsys;
    max_sys = info->param->startsys;
  }

  /* Display counts for all events. */

  ahlog_info(HIGH, __func__, "Total number of events processed: %d\n", info->n_events_processed);
  ahlog_info(HIGH, __func__, "Number of events processed successfully: %d\n", info->n_events_succeeded);
  ahlog_info(HIGH, __func__, "Number of events processed with problems: %d\n", info->n_events_failed);

  /* Display counts involving specific problems if there were any. */
  
  if (0 < info->n_events_failed)
  {
    /* Display count of out-of-range coordinates. */

    if (0 < info->n_events_out_of_range)
    ahlog_info(HIGH, __func__, "   Number of events with out-of-range coordinates: %d\n", info->n_events_out_of_range);

    /* Display count of problems in MULTISEG property look-up table and RAWTODET segment numbers. */

    for(sys = min_sys; sys < max_sys && sys < info->teldef->n_coordsys - 1; sys++)
    {
      if (e_TT_MULTISEG == info->teldef->trtypes[sys])
      {
        if (0 < info->n_events_no_multiseg[sys])
          ahlog_info(HIGH, __func__, 
            "   Number of events without matching %s%s%s MULTISEG properties in TelDef table: %d\n",
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1],
            info->n_events_no_multiseg[sys]);
      }
      else if (e_TT_RAWTODET == info->teldef->trtypes[sys])
      {
        if (0 < info->n_events_no_rawtodet[sys])
          ahlog_info(HIGH, __func__, 
            "   Number of events without valid %s%s%s RAWTODET segments: %d\n",
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1],
            info->n_events_no_rawtodet[sys]);
      }

    }
      
    /* Display counts of problems involving attitude files. */

    if (0 != info->param->use_att)
    {
      if (0 < info->n_events_null_time)
        ahlog_info(HIGH, __func__, "   Number of events with NULL time: %d\n", info->n_events_null_time);

      for(sys = min_sys; sys < max_sys && sys < info->teldef->n_coordsys - 1; sys++)
      {
        if (e_TT_SKYATT != info->teldef->trtypes[sys])
          continue;
              
        if (0 < info->n_events_no_attitude[sys])
          ahlog_info(HIGH, __func__, 
            "   Number of events without attitude from the %s%s%s attitude file %s: %d\n",
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1],
            info->att_filenames[sys], info->n_events_no_attitude[sys]);
      }
          
      for(sys = info->param->startsys; sys < info->param->stopsys && sys < info->teldef->n_coordsys - 1; sys++)
      {
        if (e_TT_SKYATT != info->teldef->trtypes[sys])
          continue;
              
        if (-1 == info->first_event_with_attitude[sys] || -1 == info->last_event_with_attitude[sys])
        {
          /* Explain that no events were matched to an attitude time. */
                  
          ahlog_info(HIGH, __func__, 
            "No event times were within the extrapolated time range of the %s%s%s attitude file %s.\n",
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1],
            info->att_filenames[sys]);
        }
        else if (1 == info->first_event_with_attitude[sys]
                && info->last_event_with_attitude[sys] == info->total_rows)
        {
          /* Explain that all events were matched to an attitude time. */
                  
          ahlog_info(HIGH, __func__, 
            "All event times were within the extrapolated time range of the %s%s%s attitude file %s.\n",
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1],
            info->att_filenames[sys]);
        }
        else
        {
          /* Explain which events were within the extrapolation range of the attitude file. */
                  
          ahlog_info(HIGH, __func__, 
            "Events %d-%d were within the extrapolated time range of %s%s%s attitude file %s.\n",
            info->first_event_with_attitude[sys], info->last_event_with_attitude[sys], 
            info->teldef->coordsysnames[sys], info->conv_arrow, info->teldef->coordsysnames[sys + 1],
            info->att_filenames[sys]);
        }

        /* Give similar counts for use of the orbit file. */
        ahlog_info(HIGH, __func__, 
          "Number of events with successful annual-only aberration calculation:      %ld\n",
          info->n_events_aber_ann_only_success);

        if (0 != info->param->really_do_orbaber) 
        {

          ahlog_info(HIGH, __func__, 
            "Number of events with successful annual + orbital aberration calculation: %ld\n",
            info->n_events_aber_ann_and_orb_success);
          ahlog_info(HIGH, __func__, 
            "Number of events with failed annual + orbital aberration calculation:     %ld\n",
            info->n_events_aber_ann_and_orb_failure);
          ahlog_info(HIGH, __func__, 
            "Number of events with successful orbital-only aberration calculation:     %ld\n",
            info->n_events_aber_orb_only_success);
          ahlog_info(HIGH, __func__, 
            "Number of events with failed orbital-only aberration calculation:         %ld\n",
            info->n_events_aber_orb_only_failure);

          if (0 < info->n_events_no_orbit)
            ahlog_info(HIGH, __func__, 
              "Number of events without orbital velocity from orbit file %s: %d\n",
              info->param->orb_file, info->n_events_no_orbit);

          if (-1 == info->first_event_with_orbit || -1 == info->last_event_with_orbit)
          {

            /* Explain that no events were matched to an orbit time. */

            ahlog_info(HIGH, __func__, 
              "No event times were within the extrapolated time range of the orbit file %s.\n",
              info->param->orb_file);
          } 
          else if (1 == info->first_event_with_orbit && info->last_event_with_orbit == info->total_rows) 
          {
            /* Explain that all events were matched to an orbit time. */

            ahlog_info(HIGH, __func__, 
              "All event times were within the extrapolated time range of the orbit file %s.\n",
              info->param->orb_file);
          } 
          else 
          {
            /* Explain which events were within the extrapolation range of the orbit file. */

          ahlog_info(HIGH, __func__, 
            "Events %d - %d were within the extrapolated time range of orbit file %s.\n",
            info->first_event_with_orbit, info->last_event_with_orbit, 
            info->param->orb_file);
          }
        }
      }
    }
  } /* end if n_events_failed > 0 */
}

/* -------------------------------------------------------------------- */

int readWindowingOffsetKeywords
(
  fitsfile* fp, /* Event file pointer */
  INFO* info,   /* INFO structure pointer */
  const int sys,      /* originating coordinate system number */
  char dim      /* 'X' or 'Y' dimension */
  )
{
  /* Make TelDef shortcuts. */

  TELDEF2* teldef = info->teldef;
  TR_MULTISEG* multisegparam = teldef->multisegparam[sys];

  /* Declare shortcuts for one dimension's windowing property info. */

  char* winoff_name = NULL;
  char* winprop_name = NULL;
  int winprop_num = -1;
  int use_multiple_winoff = 0;
  long* window_offsets = NULL;
  long min_winprop = 0;
  long max_winprop = 0;
  long n_winprop = 0;
  long winprop = 0;

  /* Declare a keyword name string and CFITSIO status. */

  char keyname[FLEN_VALUE];
  int status = 0;

  strcpy(keyname, "");

  /* Check if dim is either 'X' or 'Y'. */

  if ('X' != toupper(dim) && 'Y' != toupper(dim))
    return 0;
  else
    dim = toupper(dim);
  
  /* Set local windowing offset variables to values of either the info or teldef x or y variables. */
  
  if ('X' == dim)
  {
    winoff_name = multisegparam->winoffx_name;
    winprop_name = multisegparam->winpropx_name;
    winprop_num = multisegparam->winpropx_num;
    use_multiple_winoff = multisegparam->use_multiple_winoffx;
    window_offsets = info->window_offsets_x[sys];
    ahlog_info(HIGH, __func__, "Windowing in X.\n");
  }
  else /* if ('Y' == dim) */
  {
    winoff_name = multisegparam->winoffy_name;
    winprop_name = multisegparam->winpropy_name;
    winprop_num = multisegparam->winpropy_num;
    use_multiple_winoff = multisegparam->use_multiple_winoffy;
    window_offsets = info->window_offsets_y[sys];
    ahlog_info(HIGH, __func__, "Windowing in Y.\n");
  }
  ahlog_info(HIGH, __func__, "Window offset name = %s\n", winoff_name);
  ahlog_info(HIGH, __func__, "Window property name = %s; window property number=d\n", winprop_name, winprop_num);
  ahlog_info(HIGH, __func__, "Use multiple window offsets: %s\n", (0 == use_multiple_winoff ? "NO" : "YES"));
  ahlog_info(HIGH, __func__, "Coordinate system number = %d\n", sys);
  
  /* Run through the cases of no windowing, windowing from one keyword, windowing from multiple keywords. */
  
  if (0 == strcasecmp(winoff_name, "NONE"))
  {
    /* Windowing is disabled.  Use a single windowing offset of 0. */
      
    min_winprop = 0;
    max_winprop = 0;
    n_winprop = 1;
    window_offsets = (long*) calloc(1, sizeof(long));
    window_offsets[min_winprop] = 0;
    ahlog_info(HIGH, __func__, "Windowing disabled; using a single windowing offset of zero.\n");
  }
  else if (0 != strcasecmp(winoff_name, "NONE") && 0 == strcasecmp(winprop_name, "NONE"))
  {
    /* Windowing is enabled and uses a single windowing offset. */
      
    min_winprop = 0;
    max_winprop = 0;
    n_winprop = 1;
    window_offsets = (long*) calloc(1, sizeof(long));
      
    /* Read the offset value from the single expected keyword, or use 0 if it is not found. */
      
    fits_read_key_lng(fp, winoff_name, &(window_offsets[min_winprop]), NULL, &status);
    if (KEY_NO_EXIST == status)
    {
      /* Set the offset to 0 if the keyword is not found. */
          
      window_offsets[min_winprop] = 0;
      ahlog_info(HIGH, __func__, "Windowing %c-offset keyword %s is missing from the event file.\n",
        tolower(dim), winoff_name);
      status = 0;
    }
    else if (0 < status)
    {
      ahlog_err(__func__, "Cannot read keyword %s from event file.\n", winoff_name);
      return 1;
    }
      
    ahlog_info(HIGH, __func__,
      "Using a uniform windowing %c-offset = %d in transformation %s%s%s.\n",
      tolower(dim), window_offsets[min_winprop],
      multisegparam->lowcoordsysname, info->conv_arrow, multisegparam->highcoordsysname);

  }
  else
  {
    /* Windowing is enabled and uses multiple windowing offsets.  
     * Deduce the range of values of the winprop property from the MULTISEG[sys]_COEFF table. */
      
    min_winprop = multisegparam->min_properties[winprop_num];
    max_winprop = multisegparam->max_properties[winprop_num];
    n_winprop = max_winprop - min_winprop + 1;
      
    /* Allocate memory for the offset values. */
      
    window_offsets = (long*) calloc(max_winprop + 1, sizeof(long));
      
    /* Read the windowing offset keywords from the event header. 
     * Set values of missing keywords to 0. */
      
    for(winprop = 0; winprop <= max_winprop; winprop++)
    {
      if (winprop < min_winprop)
      {
        /* Initialize unneeded offsets to 0. */
              
        window_offsets[winprop] = 0;
      }
      else
      {
        /* Read offset keyword values. */
              
        sprintf(keyname, "%s%ld", winoff_name, winprop);
        fits_read_key_lng(fp, keyname, &(window_offsets[winprop]), NULL, &status);
        if (KEY_NO_EXIST == status)
        {
          /* Set the offset to 0 if the keyword is not found. */
                  
          window_offsets[winprop] = 0;
          ahlog_info(HIGH, __func__, "Windowing %c-offset keyword %s is missing from the event file.\n", tolower(dim), keyname);
          ahlog_info(HIGH, __func__, "Setting the offset to 0 for events with %s = %ld in transformation %s%s%s.\n",
            winprop_name, winprop, multisegparam->lowcoordsysname, info->conv_arrow, multisegparam->highcoordsysname);
          status = 0;
        }
        else if (0 < status)
        {
          ahlog_err(__func__, "Cannot read keyword %s from event file.\n", keyname);
          return 1;
        }
        ahlog_info(HIGH, __func__, "Windowing property number = %d; window offset for that property = %d\n", winprop, window_offsets[winprop]);
              
      }
    } /* end for winprop */
      
    ahlog_info(HIGH, __func__,
      "Using multiple windowing %c-offsets from the %s# keywords depending on the %s values in transformation %s%s%s.\n",
      tolower(dim), winoff_name, winprop_name,
      multisegparam->lowcoordsysname, info->conv_arrow, multisegparam->highcoordsysname);

  } /* end multiple windowing offsets case */

  /* Tell the info structure where to find the newly allocated windowing offsets array. */

  if ('X' == dim)
  {
    info->window_offsets_x[sys] = window_offsets;
  }
  else /* if ('Y' == dim) */
  {
    info->window_offsets_y[sys] = window_offsets;
  }

  return 0;

}

/* ---------------------------------------------------------------------------- */

/* Decide what needs to be updated in the event header and update it. */
int updateKeywords
(
  fitsfile* fp, /* Event file pointer */
  INFO* info, /* INFO structure */
  iteratorCol* fits_col, /* Array of iterator column structures */
  const char* taskname /* Name and version of this task */
  )
{
  /* Declare shortcuts for the TelDef and parameters structures. */

  TELDEF2* teldef = info->teldef;
  PARAM* param = info->param;
  
  int is_sky = 0; /* Flag if a system is the sky system */
  int decimals = 6; /* Number of decimals in output keyword value */
  int status = 0; /* CFITSIO status */
  double crroll = 0.; /* Roll angle */
  double deg_per_sky_unit = 1.; /* Sky unit conversion factor */
  int dsys = (info->conv_to_higher ? 1 : -1); /* Increment of sys index */
  int sys = 0; /* Loop index for coordinate systems */

  ahlog_info(2, __func__, "Updating aspecting and coordinate keywords and history in header.\n");

  /* Write RA_NOM, DEC_NOM, ABERRAT, and FOLOWSUN keywords if SKY coordinates
   * were calculated. */

  if (0 != param->use_sky_att && param->highsys == info->teldef->n_coordsys - 1)
  {
    if (0 != writeAspectingKeywords(fp, param, decimals))
    {
      ahlog_err(__func__, "Could not write the aspecting keywords.\n");
      return 1;
    }
  }

  /* Loop through the updated coordinate columns and add coordinate keywords. 
   * SKY coordinates are a special case. */

  for(sys = param->startsys + dsys; dsys*sys <= dsys*param->stopsys; sys += dsys)
  {
    /* There are no x or y columns for the inverse RAWTODET pixel map
     * transformations, so there are no keywords to update in this
     * case. */

    if (0 == info->conv_to_higher && e_TT_RAWTODET == teldef->trtypes[sys] && RM_CORNER_LIST == teldef->rawtodetparam[sys]->rawmethod)
      continue;

    /* Determine if this system is SKY coordinates. */

    is_sky = (sys == teldef->sky_sys);

    /* Update keywords for rounded coordinate columns. Roll and conversion factor to degrees are
     * needed only for sky coordinates. */

    if (0 != is_sky && 0 < sys)
    {
      crroll = teldef->skyattparam[sys - 1]->alignment->roll_sign 
        * (param->roll + teldef->skyattparam[sys - 1]->alignment->roll_offset);
      deg_per_sky_unit = teldef->skyattparam[sys - 1]->deg_per_sky_unit;
    }
    else
    {
      crroll = 0.;
      deg_per_sky_unit = 1.;
    }

    if (0 != param->includeintcol)
      if (0 != updateCoordKeywords(fp, teldef->coordsys[sys][teldef->min_segment[sys]], 
                          info->intx_cols[sys], info->inty_cols[sys], 
                          (is_sky ? param->ra : 0.), (is_sky ? param->dec : 0.), crroll,
                          deg_per_sky_unit, is_sky, decimals))
      {
        ahlog_err(__func__, "Could not update coordinate keywords for integer coordinate columns.\n");
        return 1;
      }

    /* Update keywords for unrounded coordinate columns. */

    if (0 != param->includefloatcol || (0 != param->includefloatskycol && 0 != is_sky))
      if (0 != updateCoordKeywords(fp, teldef->coordsys[sys][teldef->min_segment[sys]], 
                          info->floatx_cols[sys], info->floaty_cols[sys], 
                          (is_sky ? param->ra : 0.), (is_sky ? param->dec : 0.), crroll,
                          deg_per_sky_unit, is_sky, decimals))
      {
        ahlog_err(__func__, "Could not update coordinate keywords for unrounded coordinate columns.\n");
        return 1;
      }
  }
  
  /* Add the history comments. */

  if (0 != addHistoryComments(fp, info, taskname))
  {
    ahlog_err(__func__, "Could not add history or comment cards to event file header.\n");
    return 1;
  }

  /* Put command-line parameters in event header if the user requested them. */

  if (0 != param->write_history)
    HDpar_stamp(fp, 0, &status);

  return 0;
}

/* ---------------------------------------------------------------------------- */

/* Write the aspecting keywords. Call this only if SKY coordinates were calculated. */
int writeAspectingKeywords
(
  fitsfile* fp, /* Event file pointer */
  PARAM* param, /* Parameter structure */
  const int decimals /* Number of decimals places for floating-point keyword values */
  )
{
  int status = 0; /* CFITSIO status */

  /* Update the values of RA_NOM and DEC_NOM with the value
   * set at the center of the SKY coordinates plane. */

  fits_update_key_dbl(fp, "RA_NOM", param->ra, decimals, 
                      "R. A. of nominal aspect point", &status);

  fits_update_key_dbl(fp, "DEC_NOM", param->dec, decimals, 
                      "Dec. of nominal aspect point", &status);

  /* Write aspecting options as keywords. */
  
  int key_val = 0;  /* default value of aspecting keyword */
  
  if (param->do_annual_aberration || param->att_annaber) {
    key_val = 1; 
  } else {
    key_val = 0;  
  }
  fits_update_key_log(fp, "ABERRAT", key_val,
                      "Was an aberration correction applied to sky coords.?", 
                      &status);
  
  if (param->do_orbital_aberration || param->att_orbaber) {
    key_val = 1; 
  } else {
    key_val = 0;  
  }
  if (0 != key_val) {
    fits_update_key_log(fp, "ABERORB", key_val, 
                        "Was an orbital aberration correction applied to sky coords.?", 
                        &status);
  }

  if (param->follow_sun || param->att_followsun) {
    key_val = 1; 
  } else {
    key_val = 0;  
  }
  fits_update_key_log(fp, "FOLOWSUN", key_val,
                      "Was the Sun position recalculated for each event?", 
                      &status);

  if ( ( (0 != param->do_annual_aberration) || (0 != param->att_annaber) ) && 
       ( (0 != param->invert_annual_aberration) || (0 != param->att_invaberr) ) ) {
    key_val = 1; 
  } else {
    key_val = 0;  
  }
  if (0 != key_val) {
    fits_update_key_log(fp, "INVABERR", key_val,
                        "Was the aberration correction inverted?", 
                        &status);
  }

  if ( ( (0 != param->do_orbital_aberration) || (0 != param->att_orbaber) ) && 
       ( (0 != param->invert_orbital_aberration) || (0 != param->att_invoaber) ) ) {
    key_val = 1; 
  } else {
    key_val = 0;  
  }
  if (0 != key_val) {
    fits_update_key_log(fp, "INVOABER", key_val, 
                        "Was the orbital aberration correction inverted?", &status);
  }

  if (0 != status)
  {
    ahlog_err(__func__, "Could not update aspecting keywords in the event extension header.\n");
    return 1;
  }

  return 0;

}

/* ---------------------------------------------------------------------------- */

/* Write WCS and other keywords for each updated coordinate column. */
int updateCoordKeywords
(
  fitsfile* fp, /* Event file pointer */
  COORDDEF* coord, /* Coordinate system structure */
  iteratorCol* colx, /* Iterator column for x coordinate */
  iteratorCol* coly, /* Iterator column for y coordinate */
  const double crvalx, /* x coord. of reference pixel */
  const double crvaly, /* y coord. of reference pixel */
  const double crroll, /* roll of coordinate system (sky) or 0 (other coord. sys.) */
  const double deg_per_sky_unit, /* conversion factor from sky units to deg. */
  const int is_sky, /* These are (1)/are not (0) sky coordinates */
  const int decimals /* Number of decimal places for floating-point keyword values */
  )
{
  int status = 0; /* CFITSIO status */
  int colnum = -1; /* FITS column number */
  char* colname = NULL; /* Column name */
  char key[FLEN_KEYWORD]; /* Keyword name */
  char comment[FLEN_COMMENT]; /* Keyword comment */
  char value[FLEN_VALUE]; /* Keyword value */
  char deg_units[FLEN_VALUE]; /* Degrees unit */

  /* Initialize strings. */
  strcpy(key, "");
  strcpy(comment, "");
  strcpy(value, "");
  strcpy(deg_units, "deg");

  /* Work on the X coordinate keywords. */

  colnum = fits_iter_get_colnum(colx);
  colname = fits_iter_get_colname(colx);

  ahlog_debug(__func__, __FILE__, __LINE__, "Updating coordinate keywords for column %s.\n", colname);

  /* Add X TLMIN keyword. */

  sprintf(key, "TLMIN%d", colnum);
  sprintf(comment, "Minimum value for %s column", colname);
  fits_update_key_lng(fp, key, (long) coord->first_pixel_x, comment, &status);

  /* Add X TLMAX keyword. */

  sprintf(key, "TLMAX%d", colnum);
  sprintf(comment, "Maximum value for %s column", colname);
  fits_update_key_lng(fp, key, (long) coord->last_pixel_x, comment, &status);

  /* Add X TCRPX keyword. */

  sprintf(key, "TCRPX%d", colnum);
  sprintf(comment, "%s image ref. pixel", colname);
  fits_update_key_dbl(fp, key, coord->center_x, decimals, comment, &status);

  /* Add X TCRVL keyword. */

  sprintf(key, "TCRVL%d", colnum);
  sprintf(comment, "%s image ref. pixel coord. (%s)", colname, (is_sky ? deg_units : coord->scale_unit));
  fits_update_key_dbl(fp, key, crvalx, decimals, comment, &status);

  /* Add X TCDLT keyword. 
   * Flip the X axes for sky coordinates to follow the convention 
   * that RA increases to the left. 
   */

  sprintf(key, "TCDLT%d", colnum);
  sprintf(comment, "%s image scale (%s/pixel)", colname, (is_sky ? deg_units : coord->scale_unit));
  fits_update_key_dbl(fp, key, 
                      (is_sky ? -deg_per_sky_unit : 1) * coord->scale_x, 
                      decimals, comment, &status);


  /* Add X TCTYP keyword. SKY coordinates are special. */

  sprintf(key, "TCTYP%d", colnum);
  sprintf(comment, "%s coordinate type", colname);
  if (0 != is_sky)
    sprintf(value, "RA---TAN");
  else
    sprintf(value, "%sX", coord->name);
  ahlog_debug(__func__, __FILE__, __LINE__, "Setting keyword %s = %s.\n", key, value);
  fits_update_key_str(fp, key, value, comment, &status);

  /* Add X TCUNI keyword. */

  sprintf(key, "TCUNI%d", colnum);
  sprintf(comment, "%s units", colname);
  fits_update_key_str(fp, key, (is_sky ? deg_units : coord->scale_unit), comment, &status);

  /* Add X TCROT keyword if needed (this coordinate system is SKY). */
  /* The TCROT keyword is never needed for X (RA), so this part is commented out. */

  /*
  if (0 != is_sky)
  {
    sprintf(comment, "Sky coordinate roll (deg)");
    sprintf(key, "TCROT%d", colnum);
    fits_update_key_dbl(fp, key, crroll, decimals, comment, &status);
  }
  */
  
  /* Acknowledge any CFITSIO errors. */

  if (0 != status)
  {
    ahlog_err(__func__, "Cannot update coordinate keywords for %s column\n", colname);
    return 1;
  }

  /* Work on the Y coordinate keywords. */

  colnum = fits_iter_get_colnum(coly);
  colname = fits_iter_get_colname(coly);

  ahlog_debug(__func__, __FILE__, __LINE__, "Updating coordinate keywords for column %s.\n",  colname);

  /* Add Y TLMIN keyword. */

  sprintf(key, "TLMIN%d", colnum);
  sprintf(comment, "Minimum value for %s column", colname);
  fits_update_key_lng(fp, key, (long) coord->first_pixel_y, comment, &status);

  /* Add Y TLMAX keyword. */

  sprintf(key, "TLMAX%d", colnum);
  sprintf(comment, "Maximum value for %s column", colname);
  fits_update_key_lng(fp, key, (long) coord->last_pixel_y, comment, &status);

  /* Add Y TCRPX keyword. */

  sprintf(key, "TCRPX%d", colnum);
  sprintf(comment, "%s image ref. pixel", colname);
  fits_update_key_dbl(fp, key, coord->center_y, decimals, comment, &status);

  /* Add Y TCRVL keyword. */

  sprintf(key, "TCRVL%d", colnum);
  sprintf(comment, "%s image ref. pixel coord. (%s)", colname, (is_sky ? deg_units : coord->scale_unit));
  fits_update_key_dbl(fp, key, crvaly, decimals, comment, &status);

  /* Add Y TCDLT keyword. */

  sprintf(key, "TCDLT%d", colnum);
  sprintf(comment, "%s image scale (%s/pixel)", colname, (is_sky ? deg_units : coord->scale_unit));
  fits_update_key_dbl(fp, key, (is_sky ? deg_per_sky_unit : 1) * coord->scale_y, decimals, comment, &status);


  /* Add Y TCTYP keyword. SKY coordinates are special. */

  sprintf(key, "TCTYP%d", colnum);
  sprintf(comment, "%s coordinate type", colname);
  if (0 != is_sky)
    sprintf(value, "DEC--TAN");
  else
    sprintf(value, "%sY", coord->name);
  ahlog_debug(__func__, __FILE__, __LINE__, "Setting keyword %s = %s.\n", key, value);
  fits_update_key_str(fp, key, value, comment, &status);

  /* Add Y TCUNI keyword. */

  sprintf(key, "TCUNI%d", colnum);
  sprintf(comment, "%s units", colname);
  fits_update_key_str(fp, key, (is_sky ? deg_units : coord->scale_unit), comment, &status);

  /* Add Y TCROT keyword if needed (this coordinate system is SKY). */
  
  if (0 != is_sky)
  {
    sprintf(comment, "Sky coordinate roll (deg)");
    sprintf(key, "TCROT%d", colnum);
    fits_update_key_dbl(fp, key, crroll, decimals, comment, &status);
  }

  /* Acknowledge any CFITSIO errors. */

  if (0 != status)
  {
    ahlog_err(__func__, "Cannot update coordinate keywords for %s column.\n", colname);
    return 1;
  }

  return 0;
}

/* ---------------------------------------------------------------------------- */

/* Update the event header history with what coordevt has completed. */
int addHistoryComments
(
  fitsfile* fp, /* Event file pointer */
  INFO* info, /* INFO structure */
  const char* taskname /* Task name and version */
  )
{
  PARAM* param = info->param; /* Shortcut */
  TELDEF2* teldef = info->teldef; /* Shortcut */
  int status = 0; /* CFITSIO status */
  char date[FLEN_VALUE]; /* Date */
  int local_time = 0; /* Local time */
  char history[STRBUFFLEN]; /* Header history string */
  int is_sky_sys = 0; /* Flag if system is sky system */
  int dsys = (info->conv_to_higher ? 1 : -1); /* Counter increment */
  int sys = 0; /* Loop index for coordinate systems */
  int low_sys = 0, high_sys = 0; /* Temporaries to hold coord sys numbers */

  /* Initialize strings. */
  strcpy(date, "");
  strcpy(history, "");


  /* Build up a long history string, which will be wrapped around in
   * multiple HISTORY card images by cfitsio. */
  strcpy(history, "The ");

  /* sys runs from startsys + 1 to stopsys by 1 if converting to higher coords., and
     sys runs from startsys - 1 to stopsys by -1 if converting to lower coords. */
  for(sys = param->startsys + dsys; dsys*sys <= dsys*param->stopsys; sys += dsys)
  {
    /* Determine if the rounded and unrounded columns are needed for
     * this system.  List those columns that have been
     * updated. The inverse RAWTODET pixel map transformation have no
     * destination coordinate columns to update. */

    if (0 == info->conv_to_higher && e_TT_RAWTODET == teldef->trtypes[sys] && RM_CORNER_LIST == teldef->rawtodetparam[sys]->rawmethod)
      continue;
    
    is_sky_sys = (sys == teldef->sky_sys);
    
    if (0 != param->includeintcol)
    {
      strncat(history, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, COORDSYSNAME_LENGTH-1);
      strcat(history, " ");
      strncat(history, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, COORDSYSNAME_LENGTH-1);
      strcat(history, " ");
    }
    
    if (0 != param->includefloatcol || (0 != param->includefloatskycol && 0 != is_sky_sys))
    {
      strncat(history, teldef->coordsys[sys][teldef->min_segment[sys]]->name_x, COORDSYSNAME_LENGTH-1);
      strcat(history, param->floatcolsuffix);
      strcat(history, " ");
      strncat(history, teldef->coordsys[sys][teldef->min_segment[sys]]->name_y, COORDSYSNAME_LENGTH-1);
      strcat(history, param->floatcolsuffix);
      strcat(history, " ");
    }
  }
  strcat(history, "columns");
  fits_write_history(fp, history, &status);
  
  fits_get_system_time(date, &local_time, &status);
  sprintf(history, "were updated by %s on %s", taskname, date);
  if (0 == local_time)
    strcat(history, " UTC");

  fits_write_history(fp, history, &status);

  /* Display the input coordinate columns. If the first transformation
   * was a pixel map RAWTODET transformation (and not an inverse one),
   * then there aren't any input coordinate columns, so mention the
   * input pixel column name instead. */
  

  if (0 != info->conv_to_higher && param->startsys < teldef->n_coordsys - 1 && e_TT_RAWTODET == teldef->trtypes[param->startsys] && RM_CORNER_LIST == teldef->rawtodetparam[param->startsys]->rawmethod)
  {
    sprintf(history, "using input pixel column %s", teldef->rawtodetparam[param->startsys]->segcolname);
    fits_write_history(fp, history, &status);
  }
  else
  {    
    strcpy(history, "using input coordinate columns ");
    
    if (0 != param->startwithfloat && (0 != param->includefloatcol || (0 != param->includefloatskycol && param->startsys == teldef->sky_sys)))
    {
      strncat(history, teldef->coordsys[param->startsys][teldef->min_segment[param->startsys]]->name_x, COORDSYSNAME_LENGTH-1);
      strcat(history, param->floatcolsuffix);
      strcat(history, " ");
      strncat(history, teldef->coordsys[param->startsys][teldef->min_segment[param->startsys]]->name_y, COORDSYSNAME_LENGTH-1);
      strcat(history, param->floatcolsuffix);
    }
    else
    {
      strncat(history, teldef->coordsys[param->startsys][teldef->min_segment[param->startsys]]->name_x, COORDSYSNAME_LENGTH-1);
      strcat(history, " ");
      strncat(history, teldef->coordsys[param->startsys][teldef->min_segment[param->startsys]]->name_y, COORDSYSNAME_LENGTH-1);
    }
    fits_write_history(fp, history, &status);
  }
  
  /* List the TelDef file. */

  strcpy(history, "and using TelDef file");
  fits_write_history(fp, history, &status);

  strcpy(history, " ");
  strncat(history, teldef->filename, FLEN_FILENAME-1);
  fits_write_history(fp, history, &status);

  /* List the attitude files if any were used. */

  if (0 != param->use_att)
  {
    low_sys = (info->conv_to_higher ? param->startsys : param->stopsys);
    high_sys = (info->conv_to_higher ? param->stopsys - 1 : param->startsys - 1);

    strcpy(history, "and attitude file(s)");
    fits_write_history(fp, history, &status);
          
    for(sys = low_sys; sys <= high_sys; sys++)
    {
      if (NULL == info->att_filenames[sys])
        continue;
          
      sprintf(history, " %s (%s%s%s)", info->att_filenames[sys], 
        teldef->coordsysnames[sys], info->conv_arrow, teldef->coordsysnames[sys + 1]);
      fits_write_history(fp, history, &status);
    }
  }
  else
  {
    strcpy(history, "and no attitude files.");
    fits_write_history(fp, history, &status);
  }

  /* List the orbit file if it was used. */

  if (0 != param->really_do_orbaber)
  {
    strcpy(history, "and orbit file");
    fits_write_history(fp, history, &status);
    sprintf(history, " %s", param->orb_file);
    fits_write_history(fp, history, &status);
  }
  else
  {
    strcpy(history, "and no orbit file.");
    fits_write_history(fp, history, &status);
  }

  if (0 != status)
  {
    ahlog_err(__func__, "Error writing HISTORY keywords to event header.\n");
    return 1;
  }

  return 0;

}

/* ====================================================================== */

/* Revision log
   $Log: coordevtlib.c,v $
   Revision 1.22  2016/08/09 20:48:13  rshill
   Only update aspecting keywords if the transformation involves SKY coordinates.

   Revision 1.21  2016/04/01 20:10:25  rshill
   Commented out setting TCROTn keyword for X (RA).

   Revision 1.20  2016/03/22 18:29:46  rshill
   Changed three string comparisons to be case-insensitive (issue #610).

   Revision 1.19  2016/02/19 19:33:40  rshill
   Correct bug in MJD processing for followsun=no.

   Revision 1.18  2016/02/18 23:55:09  rshill
   Changed keyword ORBIABER to ABERORB (keyword ABERRAT left as-is).

   Revision 1.17  2016/01/13 18:24:42  klrutkow
   read extra attitude file keywords to decide on aberration correction

   Revision 1.16  2015/12/31 00:23:31  rshill
   Process TNULLn the same for both input and output integer columns.

   Revision 1.15  2015/08/19 20:07:49  rshill
   Add both X and Y TCROT keywords if coord sys is SKY, regardless of roll value.

   Revision 1.14  2015/08/19 01:11:55  rshill
   Added IDENTITY spec for attitude files.  Reduced precision of number in messages.

   Revision 1.13  2015/08/05 22:15:19  rshill
   Minor corrections to log messages.

   Revision 1.12  2015/08/03 19:41:49  rshill
   Cleaned up messages.  Limited the setting of INVABER, ORBIABER, INVOABER keywords to true cases.

   Revision 1.11  2015/07/29 22:12:18  rshill
   Corrected bug in resolving teldeffile=CALDB.

   Revision 1.10  2015/07/16 19:53:34  rshill
   Update event counters for aberration processing.

   Revision 1.9  2015/07/16 18:09:09  rshill
   Fixes to resolveTeldefFilename.

   Revision 1.8  2015/07/16 15:42:39  rshill
   Changed PARAM struct to support ! clobber; pass INFO to resolveTeldefFilename; set TCROT always for SKY.

   Revision 1.7  2015/06/25 23:00:31  rshill
   Cosmetic change to resolveTeldefFilename.

   Revision 1.6  2015/06/25 22:34:19  rshill
   Corrected C string handling bugs.

   Revision 1.5  2015/06/25 00:24:50  rshill
   Resolve TelDef filename with explicit call to HDgtcalf.

   Revision 1.4  2015/06/23 15:24:43  rshill
   Updated openGenOrbFile call to include interpolation method.

   Revision 1.3  2015/06/08 20:32:41  rshill
   Alternative math lib header to pick up isfinite macro for older gcc versions.

   Revision 1.2  2015/05/28 21:23:08  rshill
   Fixed string buffer overrun in addHistoryComments.  Changed malloc to calloc.

   Revision 1.1  2015/05/14 22:25:48  rshill
   Converted language to plain C.

*/

/* Old revision log from C++ version:

   Revision 1.29  2015/04/17 22:30:40  rshill
   Tweaked multiseg property parsing to handle two corner cases
   where uninitialized property attributes were accessed.

   Revision 1.28  2015/03/18 23:12:20  rshill
   Support Keplerian elements via upgraded genorbfile.

   Revision 1.27  2015/01/13 18:57:27  rshill
   Use value TELDEF instead of CALDB for randomization parameters.

   Revision 1.26  2015/01/12 23:47:23  rshill
   Parameters standardized.  Most important changes are new values
   YES, NO, INVERT for orbaber and annaber.

   Revision 1.25  2014/10/31 21:45:22  rshill
   Corrected null setting for sbnullvalue, ujnullvalue, and uinullvalue.

   Revision 1.24  2014/10/02 21:43:22  rshill
   Enabled the following: (1) multiple default null values by specific integer type;
   (2) randomization in FOC->SKY.

   Revision 1.23  2014/08/05 18:55:13  rshill
   Moved aber lib.

   Revision 1.22  2014/08/05 00:13:25  rshill
   Aberration routines separated into library.

   Revision 1.21  2014/07/21 17:19:30  rshill
   Removed some AH_DEBUG statements and +++ comments.

   Revision 1.20  2014/07/18 21:20:07  rshill
   Substituted isTimeWithinGenOrbFile for isInExtrapolatedGenOrbFile.

   Revision 1.19  2014/07/18 20:16:37  rshill
   Fixes motivated by unit tests.

   Revision 1.18  2014/07/17 16:24:05  rshill
   Returning to two-argument readTelDef2.

   Revision 1.17  2014/07/16 23:02:59  rshill
   Substituted isTimeWithinGenOrbFile for isInExtrapolatedGenOrbFile.

   Revision 1.16  2014/07/16 21:39:25  rshill
   Fixed a difference from TRF.

   Revision 1.15  2014/07/15 23:39:11  rshill
   Mods from H. Krimm for inverse non-linear table.

   Revision 1.14  2014/07/09 22:01:20  rshill
   Added orbital aberration per trf_coordevt_14-05-28.
   Still need to break out find_aberration_correction as a library routine.
   (Currently in coordevtlib.)

   Revision 1.13  2014/04/30 01:18:54  treichar
   Fixed transformation direction arrow in output messages to point in the correct direction when inverse coordinate transformations are performed, e.g., RAW<-ACT.  Simplified the % progress indicators.  Fixed the input coordinate columns and list of output coordinate columns written to the event file header as HISTORY lines.

   Revision 1.12  2014/03/11 19:52:09  treichar
   Corrected type of dsys variable.

   Revision 1.11  2014/03/07 20:19:22  treichar
   Changed floating point column format from single- to double-precision for output columns added by coordevt.  Fixed an issue where an array index was past the array end.

   Revision 1.10  2014/03/06 20:02:38  treichar
   Fix bug where new attitude is not read after the event time changed since the last attitude was read due to intervening event with NULL coords.

   Revision 1.9  2014/02/28 22:23:33  treichar
   Implemented the inverse RAWTODET pixel map transformation.

   Revision 1.8  2014/02/25 19:06:39  treichar
   Added capability to convert higher-level coordinates to lower-level ones, though with several parameters functioning properly for only one of its settings for this
   direction of coordinate conversion. Advanced features like windowing modes and nonlinear distortion corrections do not function in this conversion direction.

   Revision 1.7  2014/02/20 21:46:14  treichar
   Now uses the sky_sys index in the TELDEF2 structure to determine the sky coordinate system index instead of recalculating it in a few different places
   in coordevt.

   Revision 1.6  2014/02/20 19:21:55  treichar
   Types of transformations now use the enumerated types instead of the type names.

   Revision 1.5  2014/01/30 18:45:27  treichar
   Eliminated a minor memory leak

   Revision 1.4  2014/01/27 19:40:57  treichar
   Added more comments

   Revision 1.3  2014/01/24 20:58:55  treichar
   Changed random number generator functions from those in the attitude-random library to the ahgen library

   Revision 1.2  2014/01/23 21:25:23  treichar
   Finished code reorganization

   Revision 1.1  2014/01/17 19:14:58  treichar
   Reorganized code from earthvel.{h,cxx}, info.{h,cxx} keywords.{h,cxx} and param.{h,cxx} into coordevtlib.{h,cxx}.

   Revision 1.22  2013/10/11 23:21:29  treichar
   Implemented the history and buffer parameters.  Fixed const char* --> char* casts to suppress
   g++ warnings.  Updated doxygen markup.

   Revision 1.21  2013/07/18 18:30:56  treichar
   Cosmetic changes: added doxygen markup, improved function descriptions, etc.

*/

