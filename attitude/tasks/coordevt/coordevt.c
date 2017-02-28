/** \file coordevt.c
 *  \brief This source code file contains the main() function for
 *  coordevt and the standard functions called by main(). 
 *  \author Timothy Reichard
 *  \date $Date: 2016/04/07 20:50:47 $
 */

/**
\defgroup tool_coordevt Calculate coordinates in an event file (coordevt)
@ingroup mod_gen_tasks


This task transforms between coordinate systems for each event in an event file.
The coordinate systems are those defined in a teldef file, e.g., RAW, ACT, DET,
FOC, and SKY.  The transformations can be either time-independent (based on geometry 
only) or time-dependent (based on attitude).

Source files:

  coordevt.c
  coordevtlib.c
  coordevtlib.h

Library depenencies:

  heacore/ahlog
  heacore/ape
  heacore/heaapp
  attitude/lib/aber
  attitude/lib/coordfits
  attitude/lib/atFunctions

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-19  RSH     initial version, after cleaning code

*/

#define TOOLTAG "$Name: heasoft6_20_20170113 $"
#define TOOLSUB "coordevt"

#include "aber.h"
#include "coordevtlib.h"

/* Logging/messaging. */
#include "ahlog/cahlog.h"

/* Parameter file access. */
#include "ape/ape_error.h"
#include "ape/ape_trad.h"

/* heautils utilities. */
#include "headas_utils.h"

/* Heaapp access for start-up and shut-down. */
#include "heaapp/heaapp.h"
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"

#include "fitsio.h"   /* fitsio functions */
#include <string.h>
#include <sys/stat.h>


/** \addtogroup tool_coordevt 
    @{
*/

#define MAX_PARAM 100   /* Maximum number of parameters in intermediate arrays. */

/* Function declarations */

int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata);
void writeParametersToLog();
int shutDown(HeaAppData * appdata);

/** \brief Allocate a PARAM structure and read the command-line arguments
    into the structure.  Checksfor errors in the input parameters and
    throw an exception if needed. 
    \param[out] param Pointer to PARAM structure to create and fill
    \return Execution status, 0=OK, 1=no good
*/
int getPar(PARAM** param);

/** \brief Initialize the event file pointer, param structure, info
    structure, and CFITSIO iterator column structures.
    \param[out] fp Event file pointer
    \param[in,out] param Parameter structure pointer
    \param[out] info INFO structure pointer
    \return Execution status, 0=OK, 1=no good
*/
int initialize(fitsfile** fp, PARAM* param, INFO** info);

/** \brief Set up the CFITSIO iterator column structures. Call the iterator to
    use the work function to convert coordinates. Update output event header.
    \param[in,out] fp Event file pointer
    \param[in,out] info INFO structure
    \param[out] n_cols Number of iterator columns
    \param[out] fits_col Array of iterator column structures
    \return Execution status, 0=OK, 1=no good
*/
int doWork(fitsfile* fp, INFO* info);

/** \brief Clean up allocated memory.  Close the event file. 
    \param[in] info Pointer to INFO structure
    \param[in] param Pointer to parameter structure
    \param[in] fp Event file pointer
    \return Execution status, 0=OK, 1=no good
*/
int finalize(INFO* info, PARAM* param, fitsfile* fp);

/* ==================================================================== */
/* Function definitions */

/* \brief Execute the coordevt tool. */
int main (
  int argc, /* Standard number of input arguments */
  char** argv /* Standard array of input arguments */
) {
  /* Declare the file, param struct, and info struct pointers and the
     CFITSIO and runtime statuses. */

  fitsfile* fp = NULL;
  PARAM* param = NULL;
  INFO* info = NULL;
  int status = 0;         /* Initialization & execution status (0: normal) */
  int finalStatus = 0;    /* Status after shutdown (0: normal) */

  HeaAppData appdata = { 0 };

  status = startUp(argc, argv, TOOLTAG, &appdata);

  if (0 == status)
  {
    status = getPar(&param);

    if (0 != status) 
    {
      writeParametersToLog();
      ahlog_err(__func__, "getPar returned status %d, not 0 as expected.\n", status);
      finalStatus = 0;
    }

    if (0 == status) 
    {
      status = initialize(&fp, param, &info);
      if (0 != status) {
        ahlog_err(__func__, "initialize returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    }
    
    if (0 == status) 
    {
      status = doWork(fp, info);
      if (0 != status) 
      {
        ahlog_err(__func__, "doWork returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    } 

    status = finalize(info, param, fp);
    if (0 != status) 
    {
      ahlog_err(__func__, "finalize returned status %d, not 0 as expected.\n", status);
      finalStatus = 1;
    }
      
  } 
  else 
  {
    ahlog_err(__func__, "Unable to start up tool.\n");
    finalStatus = 1;
  }

  status = shutDown(&appdata);
  if (0 != status) {
    appdata.printerr(__PRETTY_FUNCTION__, "shutDown returned status %d, not 0 as expected.\n", status);
    finalStatus = 1;
  }

  return finalStatus;

}

/* -------------------------------------------------------------------- */

int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata) {

  int status = 0;

  /* Check arguments. Use stdio functions for reporting problems at this stage, since no other message streams are set up yet. */
  if (0 >= argc) { fprintf(stderr, "startUp: logic (programming) error; argc == %d; must be positive.\n", argc); status = 1; }
  if (0 == argv) { fprintf(stderr, "startUp: logic (programming) error; argv is null.\n"); status = 1; }
  else if (0 == *argv) { fprintf(stderr, "startUp: logic (programming) error; *argv is null.\n"); status = 1; }
  /* if (0 == tooltag) no problem; tooltag is optional. */
  if (0 == appdata) { fprintf(stderr, "startUp: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  if (0 == status) {
    /** Initialize the application data structure. */
    *appdata = heaapp_construct_appdata(argc, argv); /* TODO: add tooltag when heaapp has it. */
  }

  /** From here on, use I/O functions in appdata to report errors. These will function correctly even if
      all else fails below. */

  if (0 == status) {
    /** Connect ape. Note this does not actually initialize ape, but it connects code that will initialize ape.
        The ape initialization code will read standard parameters, including chatter, logfile, history and
        clobber and store them in the application data structure. */
    status = heaapp_ape_connect(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to connect ape.\n");
  }

  if (0 == status) {
    /** Connect ahlog. Note this does not actually initialize ahlog, but it connects code that will initialize ahlog.
        The ahlog initialization code will pull the name of the tool, chatter and logfile parameters, etc., from the
        application data structure when it *does* run. */
    status = heaapp_ahlog_connect(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to connect ahlog.\n");
  }

  if (0 == status) {
    /** Connect heautils. Note this does not actually initialize heautils, but it connects code that will initialize heautils.
        The heautils initialization code will pull history, clobber, etc., from the
        application data structure when it *does* run. */
    status = heaapp_heautils_connect(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to connect heautils.\n");
  }

  if (0 == status) {
    /** Finally, run all the initialization codes that were connected above. */
    status = heaapp_initialize(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to initialize application.\n");
  }

  return status;

}

void writeParametersToLog() {
  char** par_names = 0;
  char* value = 0;
  char* out = 0;
  char* new_out = 0;
  int ii = 0;

  /* start output line with name of tool */
  char toolname[128];          /* 128 is the size used in headas_toolname.c */
  get_toolname(toolname);
  ape_util_copy_string(toolname, &out);

  /* write parameters to output line */
  ape_trad_get_par_names(&par_names);
  while(par_names[ii] != NULL){
    ape_trad_get_string(par_names[ii],&value);
    ape_util_cat_string(out, " '", &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, par_names[ii], &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, "=", &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, value, &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, "'", &new_out);
    free(out); out = new_out; new_out = 0;
    free(value);
    ii++;
  }

  /* Write parameter list to log file */
  ahlog_info(HIGH, __func__, "START PARAMETER LIST:\n");
  ahlog_info(HIGH, __func__, "%s\n", out);
  ahlog_info(HIGH, __func__, "END PARAMETER LIST\n");

  /* Clean up output string */
  free(out);

  /* Clean up parameter list array */
  ape_util_free_string_array(par_names);

} /* end of writeParametersToLog */


/* Allocates a PARAM structure and reads the command-line arguments
 * into the structure.  Checks for errors in the input parameters and
 * throws an exception if needed. */
int getPar
(
  PARAM** param /* Pointer to PARAM structure to fill. */
)
{

  const char sep[] = ",";  /* Separator for velocity column names. */
  char velcolbuffer[FLEN_VALUE]; /* Utility buffer for tokenizing column names. */
  char* token = NULL;   /* Utility pointer for tokenizing column names. */
  long tlong = 0;       /* Temporary for long integers. */
  int npar = 0;         /* Total number of parameters found. */
  char tbool = 0;       /* Temporary for booleans. */
  char* tchar = 0;      /* Temporary for strings. */

  /* Declare interpolation method strings. */
  
  char* att_interpolation;
  char* datt_interpolation;

  /* Declare aberration strings. */
  
  char* annaber;
  char* orbaber;

  /* Variables for tracking parameters using APE calls. */
  /* MAX_PARAM is a constant at the top of this file. */
  int ape_status[MAX_PARAM];  /* Status of each APE call to retrieve a parameter. */
  int ape_status_all = 0;  /* Overall status of parameter retrieval. */
  const char* pname[MAX_PARAM];  /* Name of each parameter retrieved. */

  /* Utility and temporary variables. */
  int ipar = 0; /* Counter for parameters. */
  int icol = 0; /* Counter for velocity columns. */
  int i = 0; /* Loop counter. */
  
  /* Allocate the PARAM structure. */
  
  *param = createParam();
  
  /* Read command-line parameters and initialize associated quantities. */

  (*param)->tempfile_copied = 0;
  (*param)->tempfile_written = 0;
  
  for (ipar=0; ipar<MAX_PARAM; ipar++) ape_status[ipar] = eOK;
  
  ipar = 0;

  pname[ipar] = "infile";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->in_file)); ipar++;

  pname[ipar] = "outfile";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->out_file_as_input)); ipar++;

  if ('!' == (*param)->out_file_as_input[0]) {
    strcpy((*param)->out_file, &((*param)->out_file_as_input[1]));
    (*param)->out_file_bang = 1;
  } else {
    strcpy((*param)->out_file, (*param)->out_file_as_input);
    (*param)->out_file_bang = 0;
  }
  
  sprintf((*param)->temp_file, "%s.tmp", (*param)->out_file);

  pname[ipar] = "teldeffile";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &tchar); ipar++;
  strcpy((*param)->teldef_file, "");
  strncat((*param)->teldef_file, tchar, FLEN_FILENAME-1);
  free(tchar);
  
  pname[ipar] = "attfile";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->att_file)); ipar++;

  pname[ipar] = "dattfile";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->datt_files)); ipar++;

  pname[ipar] = "orbfile";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->orb_file)); ipar++;
  
  pname[ipar] = "startsys";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->startsysname)); ipar++;

  pname[ipar] = "stopsys";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->stopsysname)); ipar++;
  
  pname[ipar] = "infileext";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->event_extension)); ipar++;
  
  pname[ipar] = "timecol";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->time_col_name)); ipar++;
  
  pname[ipar] = "attext";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->att_extension)); ipar++;

  pname[ipar] = "attcol";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->att_col_name)); ipar++;

  pname[ipar] = "attform";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->att_format_string)); ipar++;

  pname[ipar] = "orbext";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->orb_extension)); ipar++;

  pname[ipar] = "orbcol";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->orb_col_name)); ipar++;

  pname[ipar] = "orbform";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->orb_format_string)); ipar++;
  
  if (0 == strcasecmp((*param)->att_format_string, "EULER"))
    (*param)->att_format = AF_EULER;
  else
    (*param)->att_format = AF_QUAT;
  
  pname[ipar] = "btnull";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
  (*param)->b_null_value = tlong;

  pname[ipar] = "itnull";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
  (*param)->i_null_value = tlong;

  pname[ipar] = "jtnull";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
  (*param)->j_null_value = tlong;

  pname[ipar] = "ktnull";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
  (*param)->k_null_value = tlong;

  pname[ipar] = "sbtnull";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
  (*param)->sb_null_value = tlong;

  pname[ipar] = "uitnull";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
  (*param)->ui_null_value = tlong;

  pname[ipar] = "ujtnull";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
  (*param)->uj_null_value = tlong;

  pname[ipar] = "blankcol";
  ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
  (*param)->blank_col = tbool;

  (*param)->includeintcol = 1; /* Rounded coordinate columns are always included. */

  pname[ipar] = "inclfloatcol";
  ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
  (*param)->includefloatcol = tbool;

  pname[ipar] = "inclfloatskycol";
  ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
  (*param)->includefloatskycol = tbool;

  /* The startwithfloat parameter is needed only when inclfloatcol is enabled. */

  pname[ipar] = "startwithfloat";
  if (0 != (*param)->includefloatcol) 
  {
    ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
    (*param)->startwithfloat = tbool;
  }
  else
  {
    (*param)->startwithfloat = 0;
    ape_status[ipar++] = eOK;
  }
  
  /* The floatcolsuffix parameter is needed when either the includefloatcol or the 
   * includefloatskycol parameter is enabled. */
  
  pname[ipar] = "floatcolsuffix";
  if (0 != (*param)->includefloatcol || 0 != (*param)->includefloatskycol)
  {
    ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->floatcolsuffix)); ipar++;
    if (0 == strcmp((*param)->floatcolsuffix, ""))
    {
      ahlog_err(__func__, "Parameter floatcolsuffix cannot be blank.\n");
      return 1;
    }
  }
  else
  {
    (*param)->floatcolsuffix = (char*)calloc(6, sizeof(char));
    strcpy((*param)->floatcolsuffix, "_NONE");
    ape_status[ipar++] = eOK;
  }
  
  /* Guess whether attitude files and particularly the sky attitude
   * files will be needed based on the attfile and dattfile parameter
   * values.  This cannot be determined with full certainty until the
   * TelDef file is read later. */
  
  if (0 == strcasecmp((*param)->att_file, "NONE") || (*param)->att_file[0] == '\0') 
  {
    (*param)->use_sky_att = 0;
  }
  else
  {
    (*param)->use_sky_att = 1;
  }
  
  if ((*param)->use_sky_att == 1)
    (*param)->use_att = 1;
  else
  {
    if (0 == strcasecmp((*param)->datt_files, "NONE") || (*param)->datt_files[0] == '\0')
      (*param)->use_att = 0;
    else
      (*param)->use_att = 1;
  }

  /* Look for special value IDENTITY for attitude files,
   * which causes attitude requests to be intercepted and the identity
   * quaternion supplied instead.  These flags affect the function
   * initAttFiles. */

  (*param)->identity_att = 0;
  if (0 == strcasecmp((*param)->att_file, "IDENTITY")) {
    (*param)->identity_att = 1;
    ahlog_info(HIGH, __func__, "Identity quaternion used for final transformation to SKY, if any.\n");
  }

  (*param)->identity_datt = 0;
  if (0 == strcasecmp((*param)->datt_files, "IDENTITY")) {
    (*param)->identity_datt = 1;
    ahlog_info(HIGH, __func__, "Identity quaternion used for any transformations requiring delta attitude.\n");
  }

  /* At this point,
   * (*param)->use_att means that either an attitude or a delta-attitude file
   *   name was specified (or both);
   * (*param)->use_sky_att means that an attitude file was specified.
   */

  /* Read aspecting parameters if needed. */
  
  pname[ipar] = "annaber";
  pname[ipar+1] = "followsun";
  pname[ipar+2] = "orbaber";
  pname[ipar+3] = "ra";
  pname[ipar+4] = "dec";
  pname[ipar+5] = "roll";
  pname[ipar+6] = "attinterp";
  pname[ipar+7] = "attdt";
  pname[ipar+8] = "chkattgap";
  if ((*param)->use_sky_att) 
  {
    /* It is safest to read in all of these parameters so that they will 
     * be available for assignment to FITS keywords. */

    /* Aberration parameters */

    ape_status[ipar] = ape_trad_query_string(pname[ipar], &annaber); ipar++;
    ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
    (*param)->follow_sun = tbool;
    ape_status[ipar] = ape_trad_query_string(pname[ipar], &orbaber); ipar++;

    /* Nominal pointing  parameters */
    
    ape_status[ipar] = ape_trad_query_double(pname[ipar], &((*param)->ra)); ipar++;    /* deg */
    ape_status[ipar] = ape_trad_query_double(pname[ipar], &((*param)->dec)); ipar++;   /* deg */
    ape_status[ipar] = ape_trad_query_double(pname[ipar], &((*param)->roll)); ipar++;  /* deg */

    /* Interpolation/extrapolation parameters */

    ape_status[ipar] = ape_trad_query_string(pname[ipar], &att_interpolation); ipar++;
    ape_status[ipar] = ape_trad_query_double(pname[ipar], &((*param)->att_time_margin)); ipar++;
    ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
    (*param)->use_att_time_margin = tbool;

    /* Set up annual aberration. */
    
    if (0 == strcasecmp(annaber, "YES")) 
    {
      (*param)->do_annual_aberration = 1;
      (*param)->invert_annual_aberration = 0;
    } 
    else if (0 == strcasecmp(annaber, "INVERT")) 
    {
      (*param)->do_annual_aberration = 1;
      (*param)->invert_annual_aberration = 1;
    } 
    else if (0 == strcasecmp(annaber, "NO")) 
    {
      (*param)->do_annual_aberration = 0;
      (*param)->invert_annual_aberration = 0;
    } 
    else 
    {
      ahlog_err(__func__, "Invalid input value of annaber:  %s\n", annaber);
      return 1;
    }

    /* Set up orbital aberration. */
    
    if (0 == strcasecmp((*param)->orb_file, "NONE") || (*param)->orb_file[0] == '\0')
      (*param)->use_orb = 0;
    else
      (*param)->use_orb = 1;
    
    if (0 == strcasecmp(orbaber, "YES")) 
    {
      (*param)->do_orbital_aberration = 1;
      (*param)->invert_orbital_aberration = 0;
    } 
    else if (0 == strcasecmp(orbaber, "INVERT")) 
    {
      (*param)->do_orbital_aberration = 1;
      (*param)->invert_orbital_aberration = 1;
    } 
    else if (0 == strcasecmp(orbaber, "NO")) 
    {
      (*param)->do_orbital_aberration = 0;
      (*param)->invert_orbital_aberration = 0;
    } 
    else 
    {
      ahlog_err(__func__, "Invalid input value of orbaber:  %s\n", orbaber);
      return 1;
    }

    if ((*param)->do_orbital_aberration && (!(*param)->use_orb))
    {
      ahlog_err(__func__, "Orbital aberration correction requested but no orbit file specified.\n");
      return 1;
    }

    if ((*param)->do_orbital_aberration) 
    {
      if (0 == strcasecmp((*param)->orb_format_string, "COMPONENTS")) 
      {
        (*param)->vel_format = ORBF_COMPONENTS_VEL;
        (*param)->num_vel_cols = 3;
      }
      else if (0 == strcasecmp((*param)->orb_format_string, "VECTOR"))
      {
        (*param)->vel_format = ORBF_VECTOR_VEL;
        (*param)->num_vel_cols = 1;
      }
      else if (0 == strcasecmp((*param)->orb_format_string, "KEPLERIAN"))
      {
        (*param)->vel_format = ORBF_KEPLERIAN;
        (*param)->num_vel_cols = 6;
      } else {
        ahlog_err(__func__, "Invalid value of orbform: %s\n", (*param)->orb_format_string);
        return 1;
      }

      (*param)->vel_col_name = (char**) calloc((*param)->num_vel_cols, sizeof(char*));
      if ((*param)->vel_format != ORBF_VECTOR_VEL)
      {
        strcpy(velcolbuffer, (*param)->orb_col_name);
        for (icol=0; icol < (*param)->num_vel_cols; icol++) 
        {
          (*param)->vel_col_name[icol] = (char *) calloc(FLEN_VALUE, sizeof(char));
          if (0 == icol)
            token = strtok(velcolbuffer, sep);
          else
            token = strtok(NULL, sep);

          if (0 == token) {
            ahlog_err(__func__, "Column name number %d cannot be found\n", icol+1);
            return 1;
          }

          strcpy((*param)->vel_col_name[icol], token);
          ahlog_info(LOW, __func__, "Orbit column name %d parsed: %s\n", icol+1, (*param)->vel_col_name[icol]);
        }
      }
      else
      {
        (*param)->vel_col_name[0] = (char *) calloc(FLEN_VALUE, sizeof(char));
        strcpy((*param)->vel_col_name[0], (*param)->orb_col_name);
      }
      if (0 == strcasecmp(orbaber, "INVERT"))
        (*param)->invert_orbital_aberration = 1;
      else
        (*param)->invert_orbital_aberration = 0;
    }
    else
    {
      (*param)->invert_orbital_aberration = 0;
    }
    
    /* Set up nominal pointing. */

    /* If either of right ascension or declination is out-of-range, then  
     * try to read the value from the event file later. */
    
    if ((*param)->ra < 0. || (*param)->ra > 360.)
      (*param)->ra_from_event_file = 1;
    else
      (*param)->ra_from_event_file = 0;
    
    if ((*param)->dec < -90. || (*param)->dec > 90.)
      (*param)->dec_from_event_file = 1;
    else
      (*param)->dec_from_event_file = 0;
    
    /* Set up interpolation/extrapolation */
    
    if (0 == strcasecmp(att_interpolation, "CONSTANT")) 
      (*param)->att_interpolation = ATTFILE_CONSTANT;
    else 
      (*param)->att_interpolation = ATTFILE_LINEAR;
  }
  else 
  {
    /* The fact that no attitude file is specified means that
     * no transformation to sky coordinates will be done. */

    for (i=0; i<9; i++) ape_status[ipar++] = eOK;  
    
    (*param)->do_annual_aberration = 0;
    (*param)->follow_sun = 0;
    (*param)->invert_annual_aberration = 0;
    (*param)->do_orbital_aberration = 0;
    (*param)->invert_orbital_aberration = 0;
    (*param)->att_interpolation = ATTFILE_LINEAR;
    (*param)->att_time_margin = 0.;
    (*param)->use_att_time_margin = 0;
    (*param)->datt_interpolation = ATTFILE_LINEAR;
    (*param)->datt_time_margin = 0.;
    (*param)->use_datt_time_margin = 0;
  }
  
  pname[ipar] = "dattinterp";
  if ((*param)->use_att)
  {

    /* This point is reached if either an attitude or a delta-attitude
     * file is specified (not necessarily both - so this point can be reached
     * without a delta-attitude file). */

    /* Read extra-/interpolation parameters. */
    
    ape_status[ipar] = ape_trad_query_string(pname[ipar], &datt_interpolation); ipar++;
    if (0 == strcasecmp(datt_interpolation, "CONSTANT")) 
      (*param)->datt_interpolation = ATTFILE_CONSTANT;
    else 
      (*param)->datt_interpolation = ATTFILE_LINEAR;
    (*param)->datt_time_margin=read_double_param("dattdt");
    (*param)->use_datt_time_margin = read_boolean_param("chkdattgap");
  }
  else
  {
    /* Delta attitude files are not needed. Initialize the variables even if the values will not be used. */
    
    (*param)->datt_interpolation = ATTFILE_LINEAR;
    (*param)->datt_time_margin = 0.;
    (*param)->use_datt_time_margin = 0;
    ipar++;
  }
  
  /* Read randomization parameters. */
  
  pname[ipar] = "randomize";
  ape_status[ipar] = ape_trad_query_string(pname[ipar], &((*param)->randomize)); ipar++;
  
  /* For now, ignore the case where randomize=CALDB.  This will be
     taken care of when building the INFO structure after the TelDef
     file has been read. */
  
  if (0 == strcasecmp((*param)->randomize, "NO"))
    (*param)->dorandomization = 0;
  else
    (*param)->dorandomization = 1;
  
  pname[ipar] = "seed";
  pname[ipar+1] = "randsys";
  pname[ipar+2] = "randscalesys";
  if ((*param)->dorandomization) 
  {
    ape_status[ipar] = ape_trad_query_long(pname[ipar], &tlong); ipar++;
    (*param)->seed = tlong;
    ape_status[ipar+1] = ape_trad_query_string(pname[ipar], &tchar); ipar++;
    strcpy((*param)->randsysname, "");
    strncat((*param)->randsysname, tchar, COORDSYSNAME_LENGTH-1);
    free(tchar);
    ape_status[ipar+2] = ape_trad_query_string(pname[ipar], &tchar); ipar++;
    strcpy((*param)->randscalesysname, "");
    strncat((*param)->randscalesysname, tchar, COORDSYSNAME_LENGTH-1);
    free(tchar);
  }
  else
  {
    for (i=0; i<3; i++) ape_status[ipar++] = eOK;  
    (*param)->seed = 0;
    strcpy((*param)->randsysname, "NONE");
    strcpy((*param)->randscalesysname, "NONE");
  }
  
  /* Read standard tool parameters. */
  
  pname[ipar] = "chatter";
  ape_status[ipar] = ape_trad_query_int(pname[ipar], &((*param)->chatter)); ipar++;
  pname[ipar] = "clobber";
  ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
  (*param)->clobber = tbool;
  pname[ipar] = "debug";
  ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
  (*param)->debug = tbool;
  pname[ipar] = "history";
  ape_status[ipar] = ape_trad_query_bool(pname[ipar], &tbool); ipar++;
  (*param)->write_history = tbool;
  pname[ipar] = "buffer";
  ape_status[ipar] = ape_trad_query_long(pname[ipar], &((*param)->buffer)); ipar++;

  npar = ipar;

  /* Sweep up any APE errors. */

  ape_status_all = 0;
  for (ipar=0; ipar<npar; ipar++)
  {
    if (eOK != ape_status[ipar])
    {
      ape_status_all = 1;
      ahlog_err(__func__, "Bad parameter retrieval status for parameter %s.\n", pname[ipar]);
    }
  }
  if (0 != ape_status_all) 
  {
    for (ipar=0; ipar<npar; ipar++)
    {
      ahlog_err(__func__, "  Parameter %s: status = %d\n", pname[ipar], ape_status[ipar]);
    }
    return 1;
  }
  
  return 0;
}

/* -------------------------------------------------------------------- */

/* Initialize the event file structure, param structure, info
   structure, and CFITSIO iterator column structures. */
int initialize
(
  fitsfile** fp, /* Event file pointer */
  PARAM* param, /* Parameter structure */
  INFO** info   /* INFO structure */
)
{
  int sys = 0; /* Coordinate system number */
  int unrecognized_system = 0; /* Flag for unrecognized coordinate system name */
  double mjd = 0.; /* Modified Julian Date */
  double tstart = 0.0;  /* Start time */
  char comment[FLEN_COMMENT]=""; /* Header comment */
  char validsysstring[1000]=""; /* List of valid coordinate systems for error messages */
  int colnum = -1; /* Column number */
  int file_exists = 0; /* 1 = exists, 0 = doesn't exist */
  fitsfile* fp_in = NULL; /* Input file pointer */
  int lowest_sys = -1; /* Index of lowest coordinate system */
  int highest_sys = -1; /* Index of highest coordinate system */
  int min_sys = -1;  /* Index of lowest system where multiseg info is searched for */
  int max_sys = -1;  /* Index of highest system where multiseg info is searched for */
  struct stat fileStat1, fileStat2;  /* For testing file equivalence */
  int same_file = 0; /* Result of testing file equivalence */
  int status = 0; /* CFITSIO status */
  int prop = 0; /* Loop index for properties */

  ahlog_out(__FUNCTION__, "Started.\n");

  /* Check existence of input file. */

  fits_file_exists(param->in_file, &file_exists, &status);
  if (0 != status)
  {
    ahlog_err(__func__, "Could not determine if %s exists.\n", param->in_file);
    return 1;
  }
  if (0 == file_exists || 2 == file_exists)
  {
    ahlog_err(__func__, "Infile %s cannot be found.\n", param->in_file);
    return 1;
  }
  
  /* Check if input and output files are the same file. */
  /* Use stat() to get unique file node. */

  same_file = 1;
  if (0 != stat(param->in_file, &fileStat1)) same_file = 0;
  if (0 != stat(param->out_file, &fileStat2)) same_file = 0;
  if (0 != same_file) 
  {
    if (fileStat1.st_ino != fileStat2.st_ino) same_file = 0;
  }
  
  param->are_files_same = same_file;
  
  /* Remove the temp file if the file exists. */

  fits_file_exists(param->temp_file, &file_exists, &status);
  if (0 != status)
  {
    ahlog_err(__func__, "Could not determine if %s exists.\n", param->temp_file);
    return 1;
  }
  if (1 == file_exists)
    remove(param->temp_file);
  
  /* Remove the output file if it exists, clobber is enabled, and it
     is not also the input file. */
  
  if (0 == param->are_files_same)
  {
    fits_file_exists(param->out_file, &file_exists, &status);
    if (0 != status)
    {
      ahlog_err(__func__, "Could not determine if %s exists.\n", param->out_file);
      return 1;
    }
    if (1 == file_exists)
    {
      if (0 != param->clobber || 0 != param->out_file_bang)
      {
        remove(param->out_file);
      }
      else
      {
        ahlog_err(__func__, "Output file %s already exists, and clobber is disabled.\n", param->out_file);
        return 1;
      }
    }
  }
  else if (0 != param->are_files_same && 0 == param->clobber)
  {
    /* The input and output files are the same but clobber is disabled, so complain and quit. */
    
    ahlog_err(__func__, "Will not overwrite infile with outfile because they refer to the same file and clobber is disabled.\n");
    return 1;
  }
  
  /* Copy the input FITS file to make the temporary output file. */
  
  fits_open_file(&fp_in, param->in_file, READONLY, &status);
  if (0 != status)
  {
    ahlog_err(__func__, "Could not open input file %s.\n", param->in_file);
    return 1;
  }
  
  fits_create_file(fp, param->temp_file, &status);
  if (0 != status)
  {
    ahlog_err(__func__, "Could not create temporary output file %s.\n", param->temp_file);
    return 1;
  }
  
  fits_copy_file(fp_in, *fp, 1, 1, 1, &status);
  if (0 != status)
  {
    ahlog_err(__func__, "Could not copy input file %s to temporary output file %s.\n", param->in_file, param->temp_file);
    return 1;
  }
  
  fits_close_file(fp_in, &status);
  if (0 != status)
  {
    ahlog_err(__func__, "Could not close input file %s.\n", param->in_file);
    return 1;
  }
  
  /* Report copy success. */
  
  param->tempfile_copied = 1;
  ahlog_info(LOW, __func__, "Copied input file %s to %s to be edited as the output file.\n", param->in_file, param->temp_file);
  
  /* Move to the event extension of the temporary output file. */
  
  fits_movnam_hdu(*fp,BINARY_TBL,param->event_extension,0/* any version*/,&status);
  if (0 != status) 
  {
    fits_report_error(stderr,status);
    ahlog_err(__func__, "Error while opening event file %s and event extension %s.\n", param->temp_file, param->event_extension);
    return 1;
  }
  
  /* Initialize the info structure, which will hold everything needed for the
   * CFITSIO iterator. */
  
  *info = createInfo(param, *fp);
  if ((0 == (*info)->param->identity_att || 0 == (*info)->param->identity_datt)
    && (0 != (*info)->param->use_att)) {
    ahlog_info(HIGH, __func__, "Using attitude file.\n");
  } else {
    ahlog_info(HIGH, __func__, "Not using attitude file.\n");
  }
  
  /* Check startsys and stopsys parameters and get system numbers
     matching the system names. */
  
  /* Generate a formatting string of the valid system names for error messages. */

  sprintf(validsysstring, "Valid coordinate system names for %s %s: LOWEST HIGHEST", 
    (*info)->teldef->mission, (*info)->teldef->instrument);

  for(sys = 0; sys < (*info)->teldef->n_coordsys; sys++)
  {
    strcat(validsysstring, " ");
    strncat(validsysstring, (*info)->teldef->coordsysnames[sys], COORDSYSNAME_LENGTH-1);
  }
  strcat(validsysstring, "\n");
  
  /* Translate LOWEST and HIGHEST to the first and last coordinate system names
   * if the user used these values. Otherwise, verify that the startsys and stopsys
   * values are real coordinate system names from the TelDef file. */
  
  lowest_sys = 0;
  highest_sys = (*info)->teldef->n_coordsys - 1;

  if (0 == strcasecmp(param->startsysname, "LOWEST"))
    param->startsys = lowest_sys;
  else if (0 == strcasecmp(param->startsysname, "HIGHEST"))
    param->startsys = highest_sys;
  else
    param->startsys = getCoordSystemNumberFromName((*info)->teldef, param->startsysname);
  
  if (0 == strcasecmp(param->stopsysname, "LOWEST"))
    param->stopsys = lowest_sys;
  else if (0 == strcasecmp(param->stopsysname, "HIGHEST"))
    param->stopsys = (*info)->teldef->n_coordsys - 1;
  else
    param->stopsys = getCoordSystemNumberFromName((*info)->teldef, param->stopsysname);
  
  if (param->startsys < param->stopsys)
  {
    param->lowsys = param->startsys;
    param->highsys = param->stopsys;
  }
  else
  {
    param->lowsys = param->stopsys;
    param->highsys = param->startsys;
  }

  /* If either of startsys or stopsys isn't recognized, explain and quit. */
  
  unrecognized_system = 0;
  if (0 > param->startsys)
  {
    ahlog_err(__func__, "startsys '%s' not recognized.\n", param->startsysname);
    unrecognized_system = 1;
  }

  if (0 > param->stopsys)
  {
    ahlog_err(__func__, "stopsys '%s' not recognized.\n", param->stopsysname);
    unrecognized_system = 1;
  }

  if (0 != unrecognized_system)
  {
    ahlog_err(__func__, "Valid coordinate system names for %s %s: LOWEST HIGHEST %s\n", 
      (*info)->teldef->mission, (*info)->teldef->instrument, validsysstring);
    return 1;
  }
  
  if (param->startsys < param->stopsys)
  {
    (*info)->conv_to_higher = 1;
    strcpy((*info)->conv_arrow, "->");
  }
  else if (param->startsys > param->stopsys)
  {
    (*info)->conv_to_higher = 0;
    strcpy((*info)->conv_arrow, "<-");
  }
  else
  {
    ahlog_err(__func__, "Will not convert from %s to %s coordinate system.\n", param->startsysname, param->stopsysname);
    return 1;
  }
  
  
  /* Determine randomization strategy if randomization is
   * enabled. The strategy requires randsysname as the name of the
   * origin system whose coordinates will be randomized and
   * randscalesysname as the name of the system setting the amount of
   * randomization (+- 0.5 px in that system). */
  
  if (0 != param->dorandomization)
  {

    initializeRandom(param->seed);
    
    if (0 == strcasecmp(param->randsysname, "TELDEF"))
    {
      /* Use system given in TelDef file. */
      
      param->randsys = (*info)->teldef->randsys;
      strcpy(param->randsysname,  (*info)->teldef->randsysname);
    }
    else if (0 == strcasecmp(param->randsysname, "LOWEST"))
    {
      /* Use first coordinate system. */
      
      param->randsys = lowest_sys;
    }
    else if (0 == strcasecmp(param->randsysname, "HIGHEST"))
    {
      /* Cannot use last coordinate system. */
      
      ahlog_err(__func__, "randsys = HIGHEST is invalid. Randomization must be applied before the final coordinate system.\n");
      return 1;
    }
    else
    {
      param->randsys = getCoordSystemNumberFromName((*info)->teldef, param->randsysname);
      if (0 > param->randsys)
      {
        ahlog_err(__func__, "randsys '%s' not recognized.\n", param->randsysname);
        ahlog_err(__func__, "Valid coordinate system names for %s %s: LOWEST HIGHEST %s\n", 
          (*info)->teldef->mission, (*info)->teldef->instrument, validsysstring);
        return 1;
      }
      else if (param->randsys == (*info)->teldef->n_coordsys - 1)
      {
        ahlog_err(__func__, "randsys = %s is invalid. Randomization must be applied before the final coordinate system.\n", param->randsysname);
        return 1;
      }
    }
        
    if (0 == strcasecmp(param->randscalesysname, "TELDEF"))
    {
      /* Use system given in TelDef file. */

      if (-1 == (*info)->teldef->randscalesys)
      {
        /* If the TelDef file didn't specify a system, use the same system
         * as randsys. */

        param->randscalesys = param->randsys;
        strcpy(param->randscalesysname, param->randsysname);
      }
      else
      {
        /* Otherwise use the TelDef file's value for randscalesys. */

        param->randscalesys = (*info)->teldef->randscalesys;
        strcpy(param->randscalesysname,  (*info)->teldef->randscalesysname);
      }
    }
    else if (0 == strcasecmp(param->randscalesysname, "LOWEST"))
    {
      /* Use the first coordinate system. */

      param->randscalesys = lowest_sys;
    }
    else if (0 == strcasecmp(param->randscalesysname, "HIGHEST"))
    {
      /* Use the last coordinate system. */

      param->randscalesys = highest_sys;
    }
    else
    {
      /* Use the system specified by the user if it exists. */

      param->randscalesys = getCoordSystemNumberFromName((*info)->teldef, param->randscalesysname);

      /* Complain and quit if the system is invalid. */

      if (0 > param->randscalesys)
      {
        ahlog_err(__func__, "randscalesys '%s' not recognized.\n", param->randscalesysname);
        ahlog_err(__func__, "Valid coordinate system names for %s %s: LOWEST HIGHEST %s\n", 
          (*info)->teldef->mission, (*info)->teldef->instrument, validsysstring);
        return 1;
      }
    }
        
    /* Set the scale of the randscalesys pixel in units of the randsys pixel. */
        
    (*info)->random_px_scale = (*info)->teldef->px_scale_factor[param->randscalesys]/(*info)->teldef->px_scale_factor[param->randsys];
  }
  else /* Don't do randomization. */
  {
    param->randsys = -1;
    param->randscalesys = -1;
    strcpy(param->randsysname, "NONE");
    strcpy(param->randscalesysname, "NONE");
  }
  
  /* If attitude files are needed, open them and match them to the correct
   * SKYATT transformation:  read the attitude extension's DESTSYS keyword and
   * match it to the transformation with the destination coordinate system of
   * the same name. If the DESTSYS keyword is missing, associate the attitude
   * file with the transformation to SKY coordinates. If any SKYATT
   * transformation has no matching attitude file, then display an error
   * message and quit. If the TELESCOP keywords in the event and attitude files
   * do not match, display a warning and continue. */
    
  if (0 != initAttFiles(*info))
  {
    ahlog_err(__func__, "Could not initialize attitude files.\n");
    return 1;
  }
  
  /* Set variables that decide if aberration corrections should be done,
   * based on a combination of input parameters and keywords from the
   * attitude file */

  if ( (param->do_annual_aberration == 1) && (param->att_annaber == 0) )
    param->really_do_annaber = 1;
  if ( (param->do_orbital_aberration == 1) && (param->att_orbaber == 0) )
    param->really_do_orbaber = 1;
  if ( (param->invert_annual_aberration == 1) && (param->att_invaberr == 0) ) 
    param->really_inv_annaber = 1;
  if ( (param->invert_orbital_aberration == 1) && (param->att_invoaber == 0) ) 
    param->really_inv_orbaber = 1; 

  /* If the orbit file is needed, open it. */
  if (0 != initOrbFile(*info))
  {
    ahlog_err(__func__, "Could not initialize orbit file.\n");
    return 1;
  }

  /* If a transformation to SKY coordinates is requested and the
     input right ascension is outside the range 0 <= ra <= 360, try
     to read the right ascension from the RA_NOM or RA_PNT event
     header keywords. */
    
  if (0 != param->use_sky_att && param->highsys == (*info)->teldef->n_coordsys - 1 
       && 0 != param->ra_from_event_file)
  {
    double ra = 0.;
        
    ahlog_info(HIGH, __func__, "Right ascension from input parameter ra is outside allowed range 0 to 360 degrees.\n");
    ahlog_info(HIGH, __func__, "Looking in event file event extension header for a suitable value.\n");

    /* Try RA_NOM first. */
        
    fits_read_key_dbl(*fp, "RA_NOM", &ra, NULL, &status);
    if (KEY_NO_EXIST == status)
    {
      /* RA_NOM wasn't found, so try RA_PNT. */
            
      status = 0;
      fits_read_key_dbl(*fp, "RA_PNT", &ra, NULL, &status);

      if (KEY_NO_EXIST == status)
      {
        /* RA_PNT also wasn't found. Complain and quit. */
                
        ahlog_err(__func__, "Cannot find RA_NOM or RA_PNT keywords in event file %s extension header.\n", param->event_extension);
        return 1;
      }
      else if (0 != status)
      {
        /* Unexpected CFITSIO error. Complain and quit. */

        ahlog_err(__func__, "Cannot search for RA_NOM or RA_PNT keywords in event file %s extension header.\n", param->event_extension);
        return 1;
      }
    }
    else if (0 != status)
    {
      /* Unexpected CFITSIO error. Complain and quit. */
            
      ahlog_err(__func__, "Cannot search for RA_NOM or RA_PNT keywords in event file %s extension header.\n", param->event_extension);
      return 1;
    }
        
    param->ra = ra;
  }

  /* If a transformation to SKY coordinates is requested and the
     input declination was outside the range -90 <= dec <= 90, try
     to read the right declination from the DEC_NOM or DEC_PNT event
     header keywords. */
    
  if (0 != param->use_sky_att && param->highsys == (*info)->teldef->n_coordsys - 1 
     && 0 != param->dec_from_event_file)
  {
    double dec = 0.;
        
    ahlog_info(HIGH, __func__, "Declination from input parameter dec is outside allowed range -90 to +90 degrees.\n");
    ahlog_info(HIGH, __func__, "Looking in event file event extension header for a suitable value.\n");

    /* Try DEC_NOM first. */
        
    fits_read_key_dbl(*fp, "DEC_NOM", &dec, NULL, &status);
    if (KEY_NO_EXIST == status)
    {
      /* DEC_NOM wasn't found, so try DEC_PNT. */
            
      status = 0;
      fits_read_key_dbl(*fp, "DEC_PNT", &dec, NULL, &status);
      if (KEY_NO_EXIST == status)
      {
        /* DEC_PNT also wasn't found. Complain and quit. */
                
        ahlog_err(__func__, "Cannot find DEC_NOM or DEC_PNT keywords in event file %s extension header.\n", param->event_extension);
        return 1;
      }
      else if (0 != status)
      {
        /* Unexpected CFITSIO error. Complain and quit. */

        ahlog_err(__func__, "Cannot search for DEC_NOM or DEC_PNT keywords in event file %s extension header.\n", param->event_extension);
        return 1;
      }
    }
    else if (0 != status)
    {
      /* Unexpected CFITSIO error. Complain and quit. */
            
      ahlog_err(__func__, "Cannot search for DEC_NOM or DEC_PNT keywords in event file %s extension header.\n", param->event_extension);
      return 1;
    }
        
    param->dec = dec;
  }

  /* Display pointing ra and dec. */

  if (0 != param->use_sky_att && param->highsys == (*info)->teldef->n_coordsys - 1)
  {
    ahlog_info(HIGH, __func__, "Using nominal pointing (ra, dec) = (%.8g, %.8g).\n", param->ra, param->dec);
  }
    
  /* Set sky coordinates plane and aberration correction options if SKY 
   * coordinates will be calculated. */

  if (0 != param->use_sky_att && param->highsys == (*info)->teldef->n_coordsys - 1)
  {
    /* Set the tangent plane coordinate system corresponding to the nominal pointing. */

    setSkyCoordCenterInTelDef2((*info)->teldef, param->ra, param->dec, param->roll);

    if (0 != param->really_do_annaber) 
    {
      /* Prepare for applying aberration correction. */

      if (0 != param->follow_sun) 
      {
        /* We will be recalculating the earth's velocity for
         * each event. All we need to do here is read the
         * MJD reference time used to calculate MJD from mission time. */
                
        (*info)->mjdref = HDget_frac_time(*fp, "MJDREF", 0, 0, &status);
                
        if (0 != status) 
        {
          ahlog_err(__func__, "Cannot read MJDREF keyword(s) from %s, but the keyword(s) are needed for options aberration=yes followsun=yes.\n",
            param->in_file);
          return 1;
        }
      } 
      else 
      {
        /* We won't be recalculating the earth's velocity for
         * each event, so get the earth's velocity at MJD-OBS
         * and use that throughout the observation */

	/* If we can find MJD-OBS, then the next "if" block will be unnecessary */
	  fits_read_key_dbl(*fp,"MJD-OBS",&mjd,comment,&status);

	  if (0 != status){ /* If we couldn't find MJD-OBS, then we'll need MJDREF{F,I} and TSTART */
	    status = 0;

	    /* Try to get the MJDREF{F,I} using the HDget function - pieces together fractional parts of MJD */
	    (*info)->mjdref = HDget_frac_time(*fp, "MJDREF", 0, 0, &status);
	    if (0 != status){
	      ahlog_err(__func__, "Cannot read necessary MJDREF keyword(s) from %s. (aberration=yes followsun=no MJD-OBS=missing)\n",
		param->in_file);
	      return 1;
	    }
	      
	    /* Try to get the TSTART keyword */
	    fits_read_key_dbl(*fp,"TSTART",&tstart,comment,&status);
	    if (0 != status){
	      ahlog_err(__func__, "Cannot read necessary TSTART keyword from %s.  (aberration=yes followsun=no MJD-OBS=missing)\n",
		param->in_file);
	      return 1;
	    } else {
	      /* If we reach this block, we have everything necessary to compute mjd. */
	      mjd = (*info)->mjdref + tstart/86400;
	    } 
	  }
	  
	  /* (*info)->mjdref = 0.0; */ /* already have valid mjd, can reset this to 0. */

	  status = findAberrationCorrection(mjd, (*info)->mjdref, (*info)->orb,
	    param->really_do_annaber, 0, 
	    param->really_inv_annaber, 0,
	    &((*info)->v_earth_orbit), (*info)->vhat_earth_orbit);
                
      } /* end if we will not be recalculating sun position for each event */
    } 
    else 
    {
      /* The aberration correction is disabled. Indicate this by
       * setting v=0.  Note that in this case follow_sun will have
       * been set to "NO". */
            
      (*info)->v_earth_orbit=0.;
    }

    if (param->really_do_orbaber) {
      /* Try to get the MJDREF{F,I} using the HDget function - pieces together fractional parts of MJD */
      (*info)->mjdref = HDget_frac_time(*fp, "MJDREF", 0, 0, &status);
      if (0 != status){
        ahlog_err(__func__, "Cannot read necessary MJDREF keyword(s) from %s. (for orbital aberration)\n",
          param->in_file);
        return 1;
      }
    }
  } /* end if param->use_sky_att */

    /* Initialize counts of problematic events. */

  (*info)->n_events_no_multiseg = (long*) calloc((*info)->teldef->n_coordsys - 1, sizeof(long));
  (*info)->n_events_no_rawtodet = (long*) calloc((*info)->teldef->n_coordsys - 1, sizeof(long));

  for(sys = 0; sys < (*info)->teldef->n_coordsys - 1; sys++)
  {
    (*info)->n_events_no_multiseg[sys] = 0;
    (*info)->n_events_no_rawtodet[sys] = 0;
  }

  /* Check for the existence of any needed property columns in the event
   * file for any MULTISEG transformations. Any that are missing can
   * be a keyword instead.  Allocate arrays to keep track of which properties
   * come from keywords or columns and what the keyword values are.
   *
   * Also read the windowing offset keyword values in the event file.  The names of
   * these keywords are already read from the TelDef file.
   */
    
  (*info)->prop_locations = (PropertyLocationEnum**) calloc((*info)->teldef->n_coordsys - 1, sizeof(PropertyLocationEnum*));
  (*info)->prop_key_values = (long**) calloc((*info)->teldef->n_coordsys - 1, sizeof(long*));
  (*info)->window_offsets_x = (long**) calloc((*info)->teldef->n_coordsys - 1, sizeof(long*));
  (*info)->window_offsets_y = (long**) calloc((*info)->teldef->n_coordsys - 1, sizeof(long*));

  /* Loop through the systems, looking for any MULTISEG transformations. */
  /* The loop limits need to be adjusted to tally with those in setIteratorColumns. */
  
  if((*info)->conv_to_higher)
  {
    min_sys = (*info)->param->startsys; 
    max_sys = (*info)->teldef->n_coordsys - 1;
  }
  else
  {
    min_sys = 0;
    max_sys = (*info)->param->startsys;
  }
  
  for(sys = min_sys; sys <= max_sys && sys < (*info)->teldef->n_coordsys - 1; sys++)
  {
    (*info)->prop_locations[sys] = NULL;
    (*info)->window_offsets_x[sys] = NULL;
    (*info)->window_offsets_y[sys] = NULL;
        
    if (e_TT_MULTISEG == (*info)->teldef->trtypes[sys])
    {
      /* Read windowing offset keywords. */

      if (0 != readWindowingOffsetKeywords(*fp, *info, sys, 'X'))
      {
        ahlog_err(__func__, "Could not read windowing offset keywords for X.\n");
        return 1;
      }

      if (0 != readWindowingOffsetKeywords(*fp, *info, sys, 'Y'))
      {
        ahlog_err(__func__, "Could not read windowing offset keywords for Y.\n");
        return 1;
      }
            
      /* Read MULTISEG properties. */
            
      (*info)->prop_locations[sys] = (PropertyLocationEnum*) calloc((*info)->teldef->multisegparam[sys]->n_properties, sizeof(PropertyLocationEnum));
      (*info)->prop_key_values[sys] = (long*) calloc((*info)->teldef->multisegparam[sys]->n_properties, sizeof(long));
            
      for(prop = 0; prop < (*info)->teldef->multisegparam[sys]->n_properties; prop++)
      {
        /* Initialize the property locations and keyword values. */

        (*info)->prop_locations[sys][prop] = e_PL_UNKNOWN;
        (*info)->prop_key_values[sys][prop] = 0;
                
        /* First check if there is an event file column for this property. */
                
        fits_get_colnum(*fp, CASEINSEN, (*info)->teldef->multisegparam[sys]->propertynames[prop],
                        &colnum, &status);
        if (COL_NOT_FOUND == status)
        {
          /* The column does not exist, so look for a keyword for this property. */
                    
          status = 0;
          fits_read_key_lng(*fp, (*info)->teldef->multisegparam[sys]->propertynames[prop],
                            &(*info)->prop_key_values[sys][prop], NULL, &status);
                    
          if (KEY_NO_EXIST == status)
          {
            /* The keyword also does not exist, so complain and quit. */
                        
            ahlog_err(__func__, "Cannot find column or keyword called %s in event extension of %s.\n", 
              (*info)->teldef->multisegparam[sys]->propertynames[prop], (*info)->param->out_file);
            ahlog_err(__func__, "This column/keyword is prescribed in the TelDef file %s.\n",
              (*info)->param->teldef_file);
            return 1;
          }
          else if (0 < status)
          {
            /* Handle error looking for the keyword. */

            ahlog_err(__func__, "Error looking for column %s in event file %s.\n", 
              (*info)->teldef->multisegparam[sys]->propertynames[prop], (*info)->param->out_file);
            return 1;
          }
          else
          {
            /* The keyword was found. */
                        
            (*info)->prop_locations[sys][prop] = e_PL_KEYWORD;
            ahlog_info(HIGH, __func__, "%s%s%s MULTISEG property %s will be read from a keyword in the event file.\n",
              (*info)->teldef->coordsysnames[sys], (*info)->conv_arrow, (*info)->teldef->coordsysnames[sys + 1],
              (*info)->teldef->multisegparam[sys]->propertynames[prop]);
              
          }
                    
                    
                    
        }
        else if (0 < status)
        {
          /* Handle error looking for the column. */

          ahlog_err(__func__, "Error looking for column %s in event file %s.\n",
            (*info)->teldef->multisegparam[sys]->propertynames[prop], (*info)->param->out_file);
          return 1;
        }
        else
        {
          /* The column was found. */
                    
          (*info)->prop_locations[sys][prop] = e_PL_COLUMN;
          ahlog_info(HIGH, __func__, "%s%s%s MULTISEG property %s will be read from a column in the event file.\n",
            (*info)->teldef->coordsysnames[sys], (*info)->conv_arrow, (*info)->teldef->coordsysnames[sys + 1],
            (*info)->teldef->multisegparam[sys]->propertynames[prop]);
        }
                
      }  /* end for prop */
    } /* end if a MULTISEG transformation */
  } /* end for sys */
    
  /* Display the TelDef structure if debug mode is enabled. */
    
  if (0 != param->debug)
    printTelDef2((*info)->teldef, stdout);

  writeParametersToLog();
    
  ahlog_debug(__func__, __FILE__, __LINE__, "end   initialize use_att= %d\n", (*info)->param->use_att); 

  return 0;
}

/* -------------------------------------------------------------------- */

/*  Set up the CFITSIO iterator column structures. Call the iterator
    to use the work function to convert all coordinates. */ 
int doWork
(
  fitsfile* fp, /* Event file pointer */
  INFO* info  /* INFO structure */
)
{
  char errmsg[FLEN_VALUE] = ""; /* Error message string */
  char taskname[FLEN_VALUE]= ""; /* Task name */
  iteratorCol* fits_col = NULL;  /* Array pointer for iterator column structs */
  int n_cols = 0; /* Number of columns */
  int status = 0; /* CFITSIO status */

  strcpy(taskname, TOOLSUB); /* Task name */

  /* Set up iterator column structures. */

  fits_col = setIteratorColumns(info, fp, &n_cols);
  if (NULL == fits_col) {
    ahlog_err(__func__, "Iterator column setup failed.\n");
    return 1;
  }
  
  /* Check that needed columns exist.  Create any output columns that
   * do not yet exist. */

  if (0 != checkCoordinateColumns(fp, fits_col, n_cols, info->nullcol, info->param))
  {
    ahlog_err(__func__, "Coordinate column check failed.\n");
    return 1;
  }

  /* Process each row of the event table, calculating coordinates and writing them
   * to the event file. */

  ahlog_debug(__func__, __FILE__, __LINE__, "About to iterate.\n");
  ahlog_debug(__func__, __FILE__, __LINE__, "info->conv_to_higher=%d\n", info->conv_to_higher);

  if (0 != info->conv_to_higher)
    fits_iterate_data(n_cols, fits_col, 0L, info->buffer_rows, 
                      convertToHigherCoordinates, info, &status);
  else
    fits_iterate_data(n_cols, fits_col, 0L, info->buffer_rows, 
                      convertToLowerCoordinates, info, &status);
    
  ahlog_debug(__func__, __FILE__, __LINE__, "Finished iterating. CFITSIO status=%s.\n", status);

  if (0 != status)
  {
    /* Display CFITSIO errors from iterator if there are any. */

    fits_get_errstatus(status, errmsg);
    ahlog_err(__func__, "CFITSIO error during event table processing: %s.\n");
    ahlog_err(__func__, "Cannot continue processing due to error.");
    return 1;
  }

  /* Display event counters. */

  displayEventCounters(info);

  /* Update the header keywords of the event file. */

  if (0 != updateKeywords(fp, info, fits_col, taskname))
  {
    ahlog_err(__func__, "Could not update keywords in the event file.\n");
    return 1;
  }

  /* Indicate that the file processing succeeded. */

  info->param->tempfile_written = 1;

  /* Free memory. */

  if (0 != fits_col)
    free(fits_col);

  return 0;
}

/* -------------------------------------------------------------------- */

/* Clean up allocated memory.  Close the event file. */
int finalize
(
  INFO* info, /* INFO structure */
  PARAM* param, /* Parameters structure */
  fitsfile* fp /* Event file pointer */
)
{
  int status = 0;  /* CFITSIO status */

  /* Close the temporary output file. */
  
  if (0 != fp)
    fits_close_file(fp, &status);

  /* Handle the temporary output file. */

  if (0 != param->tempfile_written)
  {
    /* Writing to the temporary output file completed normally, so rename the file
     * as the output file. */

    rename(param->temp_file, param->out_file);
    ahlog_info(HIGH, __func__, "Output file %s is complete.\n", param->out_file);
  }
  else if (0 != param->tempfile_copied && 0 == param->debug)
  {
    /* There was a problem during the processing, so the output file
     * is not in the expected post-processing state.  Delete the
     * file so that it is not confused by the user as a good output
     * file. */

    remove(param->temp_file);
    ahlog_info(HIGH, __func__, "Output file %s was not created.\n", param->out_file);
  }
  else if (0 != param->tempfile_copied && 0 != param->debug)
  {
    /* There was a problem during the processing, so the output file
     * is not in the expected post-processing state.  Leave the file
     * if debug mode is enabled as that may be helpful for
     * investigating the problem. */

    ahlog_out(__func__, "Incomplete temporary output file %s was written.\n", param->temp_file);
  }

  /* Free the random number state. */
  if (param->dorandomization)
    destroyRandom();

  /* Free the info structure. */

  if (0 != info)
    destroyInfo(info);

  ahlog_out(__FUNCTION__, "Finished.\n");

  return 0;

}

/* -------------------------------------------------------------------- */

int shutDown(HeaAppData * appdata) {

  int status = 0;
  /* Check arguments. Use stdio functions (only) for reporting null appdata, since no other option. */
  if (0 == appdata) { fprintf(stderr, "shutDown: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  /* Do finalize operations. */
  if (0 == status) {
    /** This will shut down the libraries in reverse order that were started up in the startUp function. */
    status = heaapp_finalize(appdata);
    /* Report error using appdata IO, which is valid even if there was an error. */
    if (0 != status) { appdata->printerr("shutDown", "heaapp_finalize returned an error.\n"); }
  }
  return status;

}

/** @} */      

/* ==================================================================== */

/* Revision log
   $Log: coordevt.c,v $
   Revision 1.17  2016/04/07 20:50:47  rshill
   Parameter dump to log changed to command line format.

   Revision 1.16  2016/03/22 18:31:05  rshill
   Added parameter logging (issue #610).

   Revision 1.15  2016/02/19 19:33:20  rshill
   Correct bug in MJD processing for followsun=no;
   correct bug in column name parsing and provide better error message.

   Revision 1.14  2016/01/13 18:23:22  klrutkow
   using new keywords read from the attitude file to decide on aberration correction

   Revision 1.13  2015/11/04 15:42:19  driethmi
   Reverted to version 1.10, and re-inserted MJD-OBS and TSTART changes in a
   much cleaner way.

   Revision 1.10  2015/08/19 21:38:48  rshill
   Comment prologue cleanup.

   Revision 1.9  2015/08/19 01:11:55  rshill
   Added IDENTITY spec for attitude files.  Reduced precision of number in messages.

   Revision 1.8  2015/08/05 22:15:19  rshill
   Minor corrections to log messages.

   Revision 1.7  2015/08/03 19:41:49  rshill
   Cleaned up messages.  Limited the setting of INVABER, ORBIABER, INVOABER keywords to true cases.

   Revision 1.6  2015/07/29 22:12:18  rshill
   Corrected bug in resolving teldeffile=CALDB.

   Revision 1.5  2015/07/16 15:39:01  rshill
   Clobber output file with name preceded by exclamation point.

   Revision 1.4  2015/06/25 23:00:31  rshill
   Cosmetic change to resolveTeldefFilename.

   Revision 1.3  2015/06/25 22:34:19  rshill
   Corrected C string handling bugs.

   Revision 1.2  2015/06/25 00:24:50  rshill
   Resolve TelDef filename with explicit call to HDgtcalf.

   Revision 1.1  2015/05/14 22:25:14  rshill
   Converted languaged to plain C; cleaned up getpar by using APE directly.

*/
   

/* Old revision log from C++ version of code, coordevt.cxx:
 
   Revision 1.62  2015/04/17 22:30:39  rshill
   Tweaked multiseg property parsing to handle two corner cases
   where uninitialized property attributes were accessed.

   Revision 1.61  2015/03/18 23:12:20  rshill
   Support Keplerian elements via upgraded genorbfile.

   Revision 1.60  2015/01/23 22:08:11  rshill
   Fixed uninitialized storage bug by reading all aspecting
   parameters if any of them is needed.

   Revision 1.59  2015/01/13 18:57:27  rshill
   Use value TELDEF instead of CALDB for randomization parameters.

   Revision 1.58  2015/01/12 23:47:23  rshill
   Parameters standardized.  Most important changes are new values
   YES, NO, INVERT for orbaber and annaber.

   Revision 1.57  2014/12/16 18:22:21  rshill
   Updated filePathsEquivalent call.

   Revision 1.56  2014/10/02 21:43:22  rshill
   Enabled the following: (1) multiple default null values by specific integer type;
   (2) randomization in FOC->SKY.

   Revision 1.55  2014/08/05 18:55:13  rshill
   Moved aber lib.

   Revision 1.54  2014/08/05 00:13:25  rshill
   Aberration routines separated into library.

   Revision 1.53  2014/07/18 21:20:26  rshill
   Fixed velocity column name parsing in COMPONENTS case.

   Revision 1.52  2014/07/18 20:16:37  rshill
   Fixes motivated by unit tests.

   Revision 1.51  2014/07/17 16:24:05  rshill
   Returning to two-argument readTelDef2.

   Revision 1.50  2014/07/16 21:39:40  rshill
   Added a missing malloc.

   Revision 1.49  2014/07/09 22:01:20  rshill
   Added orbital aberration per trf_coordevt_14-05-28.
   Still need to break out find_aberration_correction as a library routine.
   (Currently in coordevtlib.)

   Revision 1.48  2014/04/30 01:18:54  treichar
   Fixed transformation direction arrow in output messages to point in the correct direction when inverse coordinate transformations are performed, e.g., RAW<-ACT.  Simplified the % progress indicators.  Fixed the input coordinate columns and list of output coordinate columns written to the event file header as HISTORY lines.

   Revision 1.47  2014/02/25 19:06:39  treichar
   Added capability to convert higher-level coordinates to lower-level ones, though with several parameters functioning properly for only one of its settings for this
   direction of coordinate conversion. Advanced features like windowing modes and nonlinear distortion corrections do not function in this conversion direction.

   Revision 1.46  2014/02/20 19:21:55  treichar
   Types of transformations now use the enumerated types instead of the type names.

   Revision 1.45  2014/01/28 19:02:23  mwitthoe
   coordevt: move a couple Doxygen comments so that description appears in documentation

   Revision 1.44  2014/01/27 19:40:57  treichar
   Added more comments

   Revision 1.43  2014/01/24 20:58:55  treichar
   Changed random number generator functions from those in the attitude-random library to the ahgen library

   Revision 1.42  2014/01/23 21:25:23  treichar
   Finished code reorganization

   Revision 1.41  2014/01/17 19:14:58  treichar
   Reorganized code from earthvel.{h,cxx}, info.{h,cxx} keywords.{h,cxx} and param.{h,cxx} into coordevtlib.{h,cxx}.

   Revision 1.40  2014/01/03 20:35:07  treichar
   Updated main function.

   Revision 1.39  2013/12/02 22:41:57  asargent
   Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

   Revision 1.38  2013/10/11 23:21:29  treichar
   Implemented the history and buffer parameters.  Fixed const char* --> char* casts to suppress
   g++ warnings.  Updated doxygen markup.

   Revision 1.37  2013/07/19 21:20:42  treichar
   Added missing header include and deleted another header include that is no longer needed.

   Revision 1.36  2013/07/18 19:41:49  treichar
   Fixed bug in previous update involving passing of taskname.

   Revision 1.35  2013/07/18 18:30:55  treichar
   Cosmetic changes: added doxygen markup, improved function descriptions, etc.

   Revision 1.34  2013/07/10 15:09:43  treichar
   Moved the rowprops array allocation/deallocation out of a loop.

   Revision 1.33  2013/07/09 21:14:59  treichar
   Fixed bug affecting only special cases of attitude and nominal pointing leading to inf/nan sky coordinates.

   Revision 1.32  2013/06/25 19:31:07  treichar
   Fix to previous fix.

   Revision 1.31  2013/06/25 19:17:37  treichar
   Fixed bug where the aberration correction could be applied to non-sky SKYATT transformtions.

   Revision 1.30  2013/06/20 17:38:13  treichar
   Added info/warning messages about which windowing offset values are used in MULTISEG transformations.

   Revision 1.29  2013/06/10 15:12:01  treichar
   Added check that existing output columns in input events extension are of the right format (holds 1 value, not 0 or 2+, and is integer or floating-point as needed).

   Revision 1.28  2013/06/06 20:32:55  treichar
   Handles the option of multiple windowing offset keywords in the event file.

   Revision 1.27  2013/05/30 20:43:25  treichar
   Changed file copy method from using ftcopy to using CFITSIO functions.

   Revision 1.26  2013/05/29 20:08:18  treichar
   Now displays counts of events processed successfully or with various problems.  Now handles events with NULL times by setting destination coordinates to null for transformations
   involving an attitude file.

   Revision 1.25  2013/05/24 19:25:53  treichar
   Changed to display nominal pointing regardless of its source (command-line parameters or event file keywords).  Added invaberration parameter for allowing the option of inverting the
   aberration correction.

   Revision 1.24  2013/05/16 19:54:55  treichar
   Added TCROT (WCS) keywords to output header SKY coordinate columns to account for SKY roll.  Suzaku XIS PPU coordinates are no longer ignored.  Rounding of floating-point coordinates is
   tweaked to handle correct rounding (up) of coordinates that are supposed to be exactly 0.5 above an integer but fall ever so slightly below 0.5 due to numerical imprecision.

   Revision 1.23  2013/05/06 19:05:23  treichar
   Updated TNULL keyword usage so that TNULL values in the input file are preserved and the parameter nullvalue is used for integer coordinate columns without a pre-existing TNULL value.

   Revision 1.22  2013/04/19 21:10:48  treichar
   Fixed bug where infile is clobbered when infile==outfile and clobber is disabled.

   Revision 1.21  2013/04/17 19:07:34  treichar
   Fixed the bug in RAWTODET transformation for a single-segment detector when passing the segment number into convertToHigherCoordRawtodet to transform the coordinates.

   Revision 1.20  2013/04/11 20:27:22  treichar
   Fixed bad line break in TELESCOP keyword error message.

   Revision 1.19  2013/04/11 17:51:57  treichar
   Implemented the attitude interpolation/extrapolation parameters attinterpolation, atttimemargin, and useatttimemargin, and the analogous delta attitude parameters prefixed with d-.  Moved the getPar function from param.cxx
   to coordevt.cxx.


*/
