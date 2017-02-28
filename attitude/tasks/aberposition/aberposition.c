/** \file aberposition.c
 *  \brief Convert coordinates of point or region from one system to another
 *  \author Robert S. Hill
 *  \date $Date: 2016/04/14 19:18:25 $
 */

/**

  \defgroup tool_aberposition Computed aberrated coordinates of point (aberposition)
  \ingroup mod_gen_tasks

Given as input a sky position (RA, Dec) and time, this program calculates
the aberration of the position due to the finite speed of light.  Earth
motion and/or satellite orbital motion may be taken into account.

Source files:

  aberposition.c

Library depenencies:

  heacore/ahlog
  heacore/ape
  heacore/heaapp
  heacore/heautils
  attitude/lib/aber
  attitude/lib/coordfits
  attitude/lib/atFunctions

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-19  RSH     initial version, after cleaning code

*/

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "aber.h"
#include "atFunctions.h"
#include "coordfits2.h"

#include "ahlog/cahlog.h"

#include "hdcal.h"
#include "headas_utils.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "heaapp/heaapp.h"
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"

#include "fitsio.h"
#include <math.h>
#include <string.h>

#define MAX_PARAM 20  

typedef struct {
  /* Input parameters */
  double ra; /* Input Right Ascension */
  double dec; /* Input Declination */
  char* time; /* Time for which these coordinates are valid */
  char* orbfile; /* Orbit file */
  char* annaber; /* Flag to do annual aberration */
  char* orbaber; /* Flag to do orbital aberration */
  char* leapsecfile; /* Leap second filename as input */
  char* orbext; /* Orbit extension name */
  char* orbcol; /* Orbit column name */
  char* orbform; /* Orbit column format */
  double outra; /* Output Right Ascension */
  double outdec; /* Output Declination */
  int chatter;  /* Standard parameter: chatter level */
  char clobber;  /* Standard parameter: output file overwrite flag */
  char debug;  /* Standard parameter: debug mode */
  char write_history;  /* Standard parameter: FITS file history flag  */
  /* Derived parameters */
  char do_annaber; /* Flag to compute annual aberration */
  char do_orbaber; /* Flag to compute orbital aberration */
  char inv_annaber; /* Flag to invert annual aberration */
  char inv_orbaber; /* Flag to invert orbital aberration */
} Param;

/* -------------------------------------------------------------------------- */
/* Top-level functions in standard main */

/** \brief Get parameter values
 *  \param[out] param parameter structure
 */ 
int getPar(Param** param);

/** \brief Set up tool operation
 *  \param[in] param parameter structure
 *  \param[out] orb structure with orbit file information
 */
int initialize(Param* param, GENORBFILE** orb);

/** \brief Do the actual work of the tool.
 *  \param[in] param parameter structure
 *  \param[in] orb structure with orbit file information
 */
int doWork(Param* param, GENORBFILE* orb);

/** \brief Set outra and outdec as "learned" parameters
 *  \param[in] param parameter structure
 */
int finalize(Param* param, GENORBFILE* orb);

/* -------------------------------------------------------------------------- */

/* Lower-level functions. */

/** \brief Lower-level start-up for task in C language
 *  \param[in] argc Command-line argument count
 *  \param[in] argv Command-line argument strings
 *  \param[in] tooltag String labelling the tool
 *  \param[out] appdata Structure with low-level initialization information
 */
int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata);

/** \brief Write user parameters to the tool log file
 */
void writeParametersToLog();

/** \brief Lower-level shutdown for task in C language
 *  \param[in] appdata Structure with low-level initialization information
 */
int shutDown(HeaAppData* appdata);

/** \brief Allocate and initialize param structure
 * \return pointer to param structure
 */
Param* createParam ();

/** \brief Deallocate param structure
 * \param[in] param parameter structure
 */
void destroyParam (Param* param);

/** \brief Get the actual filename for the leap second file.
 * \param[out] actual_filename Name for leap second file; may be opened and read
 * \param[in] specified_filename CALDB, REFDATA, or actual filename
 */
int resolve_leapsec_filename(char* actual_filename, char* specified_filename);


/******************************************************************************/

/* \brief Execute the aberposition tool. */
int main (
  int argc, /* Standard number of input arguments */
  char** argv /* Standard array of input arguments */
) {

  /* Declare the file, param struct, and info struct pointers and the
     CFITSIO and runtime statuses. */


  int status = 0;         /* Initialization & execution status (0: normal) */
  int finalStatus = 0;    /* Status after shutdown (0: normal) */

  Param* param=0;
  GENORBFILE* orb=0;

  HeaAppData appdata = { 0 };

  status = startUp(argc, argv, TOOLTAG, &appdata);

  if (0 == status) {
    status = getPar(&param);

    if (0 != status) {
      writeParametersToLog();
      ahlog_err(__func__, "getPar returned status %d, not 0 as expected.\n", status);
      finalStatus = 0;
    }

    if (0 == status) {
      status = initialize(param, &orb);
      if (0 != status) {
        ahlog_err(__func__, "initialize returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    }
    
    if (0 == status) {
      status = doWork(param, orb);
      if (0 != status) {
        ahlog_err(__func__, "doWork returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    } 

    status = finalize(param, orb);
    if (0 != status) {
      ahlog_err(__func__, "finalize returned status %d, not 0 as expected.\n", status);
      finalStatus = 1;
    }
      
  } 
  else {
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

/* -------------------------------------------------------------------------- */

int getPar(Param** param) {

  /* MAX_PARAM is a constant macro at the top of this file. */
  int ape_status[MAX_PARAM];  /* Status of each APE query */
  const char* par_names[MAX_PARAM];  /* Names of parameters queried, in order */
  int ipar = 0;  /* Parameter index */
  int n_param = 0;  /* Actual number of parameters */
  int ape_error_occurred = 0; /* Flag for one or more parameter errors */

  Param* parptr = createParam();  /* Create Param structure */
  *param = parptr;  /* Copy to output variable */
  
  /* Initialize APE parameter retrieval status for every parameter. */
  for (ipar=0; ipar<MAX_PARAM; ipar++) ape_status[ipar] = 0;

  /* Fetch parameter values. */

  ipar = 0;

  par_names[ipar] = "ra";
  ape_status[ipar] = ape_trad_query_double(par_names[ipar], &(parptr->ra)); ipar++;
  par_names[ipar] = "dec";
  ape_status[ipar] = ape_trad_query_double(par_names[ipar], &(parptr->dec)); ipar++;
  par_names[ipar] = "time";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->time)); ipar++;
  par_names[ipar] = "orbfile";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbfile)); ipar++;
  par_names[ipar] = "annaber";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->annaber)); ipar++;
  par_names[ipar] = "orbaber";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbaber)); ipar++;
  par_names[ipar] = "leapsecfile";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->leapsecfile)); ipar++;
  par_names[ipar] = "orbext";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbext)); ipar++;
  par_names[ipar] = "orbcol";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbcol)); ipar++;
  par_names[ipar] = "orbform";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbform)); ipar++;
  par_names[ipar] = "outra";
  ape_status[ipar] = ape_trad_query_double(par_names[ipar], &(parptr->outra)); ipar++;
  par_names[ipar] = "outdec";
  ape_status[ipar] = ape_trad_query_double(par_names[ipar], &(parptr->outdec)); ipar++;
  par_names[ipar] = "chatter";
  ape_status[ipar] = ape_trad_query_int(par_names[ipar], &(parptr->chatter)); ipar++;
  par_names[ipar] = "clobber";
  ape_status[ipar] = ape_trad_query_bool(par_names[ipar], &(parptr->clobber)); ipar++;
  par_names[ipar] = "debug";
  ape_status[ipar] = ape_trad_query_bool(par_names[ipar], &(parptr->debug)); ipar++;
  par_names[ipar] = "history";
  ape_status[ipar] = ape_trad_query_bool(par_names[ipar], &(parptr->write_history)); ipar++;

  n_param = ipar;

  /* Check for APE errors. */
  ape_error_occurred = 0;
  for (ipar=0; ipar<n_param; ipar++) {
    if (eOK != ape_status[ipar]) {
      ahlog_err(__func__, "Error in parameter %s\n", par_names[ipar]);
      ape_error_occurred = 1;
    }
  }
  if (0 != ape_error_occurred) {
    ahlog_err(__func__, "One or more parameter errors encountered\n");
    return 1;
  }

  /* Resolve aberration flags. */
  if (0 == strcasecmp(parptr->annaber, "YES") || 0 == strcasecmp(parptr->annaber, "INVERT")) {
    parptr->do_annaber = 1;
    if (0 == strcasecmp(parptr->annaber, "INVERT")) {
      parptr->inv_annaber = 1;
    } else {
      parptr->inv_annaber = 0;
    }
  } else {
    parptr->do_annaber = 0;
    parptr->inv_annaber = 0;
  }

  if (0 == strcasecmp(parptr->orbaber, "YES") || 0 == strcasecmp(parptr->orbaber, "INVERT")) {
    parptr->do_orbaber = 1;
    if (0 == strcasecmp(parptr->orbaber, "INVERT")) {
      parptr->inv_orbaber = 1;
    } else {
      parptr->inv_orbaber = 0;
    }
  } else {
    parptr->do_orbaber = 0;
    parptr->inv_orbaber = 0;
  }

  return 0;

}

/* -------------------------------------------------------------------------- */

int initialize(Param* param, GENORBFILE** orb) {

  const ORBINTERP orb_interp = ORBI_WEIGHTED;  /* Orbit file interpolation method number */
  char velcolbuffer[FLEN_VALUE];  /* Buffer for manipulating column names */ 
                                  /* FLEN_VALUE is a constant in fitsio.h */
  char* token = 0;  /* Utility pointer for tokenizing column names */
  int apes = 0;  /* APE status */
  int icol = 0;  /* Loop index for column names */
  const char sep[] = ",";  /* Separator for parsing velocity column names */

  int leapsec_status = 0;  /* Status of leap second filename resolution */
  char leapsec_file[FLEN_FILENAME];  /* For passing to atMissionTime */
  char orbfile[FLEN_FILENAME];  /* For passing to openGenOrbFile */
  char orbext[FLEN_VALUE];  /* For passing to openGenOrbFile */
                            /* FLEN_ constants are in fitsion.h */

  /* Values derived from parameters. */

  ORBFORMAT vel_format=ORBF_UNKNOWN;  /* Enum type in genorbfile.h */
  int num_vel_cols = 0;
  char** vel_col_name = 0; 

  /* Initialize strings. */
  strcpy(leapsec_file, "");
  strcpy(orbfile, "");
  strcpy(orbext, "");

  /* Resolve the leap second file in CALDB or REFDATA. */

  leapsec_status = resolve_leapsec_filename(leapsec_file, param->leapsecfile);
  if (0 != leapsec_status) {
    ahlog_err(__func__, "Unable to resolve leap second filename.\n");
    return 1;
  }
  apes = ape_trad_set_string("LEAPSECFILE", leapsec_file);
  if (0 != apes) {
    ahlog_err(__func__, "Unable to set LEAPSECFILE parameter.\n");
    return 1;
  }

  /* Initialize the atFunctions time conversion. */

  atMissionTimeInit(leapsec_file, 0);

  /* Open the orbit file and initialize the orbit structure if needed. */

  if (0 != strcasecmp(param->orbfile, "NONE") && 0 != param->do_orbaber) {
    if (0 == strcasecmp(param->orbform, "COMPONENTS")) {
      vel_format = ORBF_COMPONENTS_VEL;  /* Constant in genorbfile.h */
      num_vel_cols = 3;
    } else if (0 == strcasecmp(param->orbform, "VECTOR")) {
      vel_format = ORBF_VECTOR_VEL;      /* Constant in genorbfile.h */
      num_vel_cols = 1;
    } else if (0 == strcasecmp(param->orbform, "KEPLERIAN")) {
      vel_format = ORBF_KEPLERIAN;       /* Constant in genorbfile.h */
      num_vel_cols = 6;
    } else {
      ahlog_err(__func__, "Invalid value of orbform: %s\n", param->orbform);
      return 1;
    }

    vel_col_name = (char**) calloc(num_vel_cols, sizeof(char*));
    if (ORBF_VECTOR_VEL != vel_format) {
      strcpy(velcolbuffer, param->orbcol);
      for (icol=0; icol < num_vel_cols; icol++) {
        vel_col_name[icol] = (char *) calloc(FLEN_VALUE, sizeof(char));
        if (0 == icol) {
          token = strtok(velcolbuffer, sep);
        } else {
          token = strtok(NULL, sep);
        }
        strcpy(vel_col_name[icol], token);
        ahlog_info(LOW, __func__, "Velocity column number %d: %s\n", icol, vel_col_name[icol]);
      }
    } else {
      vel_col_name[0] = (char *) calloc(FLEN_VALUE, sizeof(char));
      strcpy(vel_col_name[0], param->orbcol);
      ahlog_info(LOW, __func__, "Velocity column: %s\n", vel_col_name[0]);
    }

    strcpy(orbfile, param->orbfile);
    strcpy(orbext, param->orbext);
    *orb = openGenOrbFile(
      orbfile,
      orbext,
      vel_col_name,
      vel_format,
      orb_interp);
      if (0 == *orb) {
        ahlog_err(__func__, "Orbit file not opened\n");
        return 1;
      }
  } else {
    *orb = 0;
  }

  /* Free temporary storage. */

  for (icol=0; icol < num_vel_cols; icol++) {
    free(vel_col_name[icol]);
  }
  free(vel_col_name);

  writeParametersToLog();

  return 0;

} /* end of initialize function */

/* -------------------------------------------------------------------------- */

int doWork(Param* param, GENORBFILE* orb) {

  double unit[3];  /* Unit vector */
  double corr_unit[3];  /* Corrected unit vector */
  double corr_ra = 0.0;  /* Corrected Right Ascension */
  double corr_dec = 0.0;  /* Corrected Declination */
  int i = 0;  /* Loop index */
  double v_sat_total = 0.0;  /* Satellite speed */
  double vhat_sat_total[3];  /* Unit vector in direction of satellite velocity */
  double mjdref = 0.0;  /* Zero-point MJD for mission time scale */
  double mjdrefi = 0.0;  /* Integer part of zero-point MJD for mission time scale */
  double mission_time = 0.0;  /* Elapsed time in s since MJDREF */
  double mjd_tt = 0.0;  /* MJD in TT system corresponding to input UTC */

  const double S_PER_DAY = 86400.0; /* Number of SI seconds in a TAI or TT day */
  
  /* Default MJDREF(UTC) = 41317 = 1972-01-01T00:00:00.
   * Convert to TT by adding 
   *   10 seconds initial difference between TAI and UTC in 1972 (see Note)
   *   32.184 seconds for the TT - TAI offset, giving  
   *     (10 + 32.184)/86400 ~ 0.0004882407407407 .
   * The resulting MJDREF is used if there is no orbit file.
   *
   * Note:  The 10 second initial difference between TAI and UTC is verifiable
   * by doing the following subtraction as of any given date (here, 2015-07-01):
   *   TAI - UTC difference                              = 36 s
   *   number of leap seconds announced since 1972-01-01 = 26 s
   *   initial TAI - UTC difference                      = 10 s.
   */
  const double DEFAULT_MJDREFI=41317.0;
  const double UTCtoTAI1972=10.0;  /* s */
  const double TAItoTT=32.184;     /* s */

  AtTimeD t_utc = {0, 0, 0, 0, 0, 0, 0.0};  /* Structure to hold UTC input time for atFunctions */

  for (i=0; i<3; i++) vhat_sat_total[i] = 0.0;

  /* If there is to be an aberration correction, then find the time for which the
   * correction is to be  applied.  If there is to be an orbital aberration 
   * correction, then read the orbit velocity file and find the time closest to the
   * time of the correction. 
   */

  if (0 != param->do_annaber || 0 != param->do_orbaber) {

    if (0 != strcmp(param->time, "9999-99-99T99:99:99")) {

      /* Convert input time to MJD(TT).  This is a multi-step process. */

      /* Store input time in structure. */

      sscanf(param->time, "%4d-%2d-%2dT%2d:%2d:%2d", 
        &t_utc.yr, &t_utc.mo, &t_utc.dy, &t_utc.hr, &t_utc.mn, &t_utc.sc);

      t_utc.ss = 0.0;

      /* In the absence of an orbit file, the mission time starts at an
       * arbitrary point and is not associated with any particular mission. */

      if (0 != orb) {
        mjdref = orb->mjdref;
      } else {
        mjdref = DEFAULT_MJDREFI + (UTCtoTAI1972 + TAItoTT)/S_PER_DAY;
      }

      /* Convert input time to mission time, taking leap seconds into account.
       * Mission time is measured in seconds following a given zero point. */

      /* The assumption made here is that MJDREF is given in the TT time system,
       * but has been chosen deliberately to coincide with midnight in the UTC time system.
       * This assumption has two consequences:
       *   floor(MJDREF) == MJDREF converted to UTC;
       *   fractional part of MJDREF == offset TT - UTC (at the time specified by MJDREF).
       */

      mjdrefi = floor(mjdref);   /* Obtain reference MJD in UTC system. */

      /* Obtain mission time in s, taking into account leap seconds that have occurred
       * between mjdrefi and t_utc. */

      ahlog_info(LOW, __func__, 
        "t_utc = yr:%d mon:%d day:%d h:%d m:%d s:%d ss:%12.4e\n", 
        t_utc.yr, t_utc.mo, t_utc.dy, t_utc.hr, t_utc.mn, t_utc.sc, t_utc.ss);
      
      atAtTimeDToMission(mjdrefi, &t_utc, &mission_time);  

      ahlog_info(LOW, __func__, "mission_time = %20.6f\n", mission_time);

      /* Convert mission time to MJD(TT), which is used in ephemeris computations. */

      mjd_tt = mission_time/S_PER_DAY /* Elapsed time since referencd MJD */
               + mjdref;              /* Reference MJD in TT system. */

      ahlog_info(LOW, __func__,
        "MJD(TT) = %20.10f MJDREF(TT) = %20.10f\n", mjd_tt, mjdref);

      /* Compute the aberration correction as a unit vector times v/c. */

      findAberrationCorrection(mjd_tt, mjdref, orb, param->do_annaber,
        param->do_orbaber, param->inv_annaber, param->inv_orbaber,
        &v_sat_total, vhat_sat_total);

      ahlog_info(LOW, __func__,
        "v_sat_total = %.8g  vhat_sat_total = %.8g %.8g %.8g\n", 
        v_sat_total, vhat_sat_total[0], vhat_sat_total[1], vhat_sat_total[2]);

      /* Convert pointing to unit 3D vector. */

      convertLongLatDegToUnitv(param->ra, param->dec, unit);

      /* Apply the aberration correction. */

      for (i=0; i<3; i++) {
        corr_unit[i] = unit[i] + v_sat_total*vhat_sat_total[i];
      }

      /* Convert the unit vector vector back to RA, Dec. */

      convertUnitvToLongLatDeg(corr_unit, &corr_ra, &corr_dec);

      param->outra = corr_ra;
      param->outdec = corr_dec;

      ahlog_info(HIGH, __func__, "Input:   RA   = %11.6f   Dec %11.6f\n", param->ra, param->dec);
      ahlog_info(HIGH, __func__, "Output:  RA   = %11.6f   Dec %11.6f\n", param->outra, param->outdec);

    } else {  

      /* No time is given.  Print information and output the input (ra,dec) */

      ahlog_info(HIGH, __func__, "No input time specified. Returning input RA and Dec.\n");
      param->outra = param->ra;
      param->outdec = param->dec;
    }
  } else {

    /* No aberration correction is specified.  Print information and output the input (ra,dec) */

    ahlog_info(HIGH, __func__, "No aberration correction specified. Returning input RA and Dec.\n");
    param->outra = param->ra;
    param->outdec = param->dec;
  }

  return 0;

} /* End of do_work */

/* -------------------------------------------------------------------------- */

int finalize(Param* param, GENORBFILE* orb) {

  /* Write to the parameter file.  */

  int status = 0;  /* Status of this routine */
  int ape_status = 0;  /* Status of parameter setting */

  ape_status = ape_trad_set_double("outra", param->outra);
  if (eOK != ape_status) {
    ahlog_err(__func__, "Could not set outx parameter\n");
    status = 1;
  }

  ape_status = ape_trad_set_double("outdec", param->outdec);
  if (eOK != ape_status) {
    ahlog_err(__func__, "Could not set outy parameter\n");
    status = 1;
  }
  /* Close the orbit file and free the orbit structure. */

  if (0 != orb) {
    closeGenOrbFile(orb);
  }

  destroyParam(param);

  return status;
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

/* -------------------------------------------------------------------- */

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

/* -------------------------------------------------------------------- */

/* Allocate and initialize param structure */
Param* createParam () {
  Param* param = (Param*)calloc(1, sizeof(Param));
  param->ra = 0.0;
  param->dec = 0.0;
  param->time = 0;
  param->orbfile = 0;
  param->annaber = 0;
  param->orbaber = 0;
  param->leapsecfile = 0;
  param->orbext = 0;
  param->orbcol = 0;
  param->orbform = 0;
  param->outra = 0.0;
  param->outdec = 0.0;
  param->chatter = 0;
  param->clobber = 0;
  param->debug = 0;
  param->write_history = 0;
  param->do_annaber = 0;
  param->do_orbaber = 0;
  param->inv_annaber = 0;
  param->inv_orbaber = 0;
  return param;
}

/* -------------------------------------------------------------------- */

/* Deallocate param structure */
void destroyParam (Param* param) {
  free(param->time);
  free(param->annaber);
  free(param->orbaber);
  free(param->leapsecfile);
  free(param->orbext);
  free(param->orbcol);
  free(param->orbform);
  free(param);
}

/* -------------------------------------------------------------------- */

/* Get the actual filename for the leap second file. */
int resolve_leapsec_filename(char* actual_filename, char* specified_filename) {

  const char* tele="gen";
  const char* instr="ins";
  const char* detnam="-";
  const char* filt="-";
  const char* codenam="leapsecs";
  const char* strtdate="-";
  const char* strttime="-";
  const char* stpdate="-";
  const char* stptime="-";
  const char* expr="-";

  int maxret=1;
  char leapsec_filename[FLEN_FILENAME];
  int fnamesize=FLEN_FILENAME;
  char* fnptr=leapsec_filename;
  long extno=0;
  char online[10];
  char* onptr=online;
  int nret=0;
  int nfound=0;
  int status=0;

  char* tmp_env=0;

  if (0 == strcasecmp(specified_filename, "CALDB")) {
  
    ahlog_info(HIGH, __func__, "Querying CALDB for leap second file\n");

    HDgtcalf(tele,instr,detnam,filt,codenam,strtdate,strttime,
             stpdate,stptime,expr,maxret,fnamesize,&fnptr,&extno,
             &onptr,&nret,&nfound,&status);

    if (0 != status) {
      ahlog_err(__func__, "Could not get leap second file from CALDB; HDgtcalf status = %d\n", 
        status);
      return 1;
    }

    if (0 == nfound) {
      ahlog_err(__func__, "No leap second file found.\n");
      return 1;
    }

    strcpy(actual_filename, leapsec_filename);
    ahlog_info(HIGH, __func__, "Leap second file = %s\n", actual_filename);

  } else if (0 == strcasecmp(specified_filename, "REFDATA")) {

    ahlog_info(HIGH, __func__, "Using leap second file from REFDATA area\n");

    tmp_env=getenv("LHEA_DATA");
    if (0 == tmp_env) {
      ahlog_err(__func__, "Use of REFDATA requires LHEA_DATA environment variable\n");
      return 1;
    }

    strcpy(actual_filename, tmp_env);
    strcat(actual_filename, "/leapsec.fits");
    ahlog_info(HIGH, __func__, "Leap second file = %s\n", actual_filename);

  } else {

    /* No special value; must be the actual filename. */

    strcpy(actual_filename, specified_filename);

  }


  return 0;
}


/* Revision Log
 $Log: aberposition.c,v $
 Revision 1.9  2016/04/14 19:18:25  rshill
 Added writeParametersToLog().

 Revision 1.8  2015/10/07 19:49:21  rshill
 Corrected test for whether orbfile opened.

 Revision 1.7  2015/08/21 18:20:33  rshill
 Reduced precision of log outputs to reasonable levels.

 Revision 1.6  2015/08/19 21:31:27  rshill
 Comment prologue cleanup.

 Revision 1.5  2015/08/04 20:42:51  rshill
 Added log output.

 Revision 1.4  2015/06/24 22:22:51  rshill
 Refined the CALDB and REFDATA resolution.

 Revision 1.3  2015/06/23 15:22:40  rshill
 Updated openGenOrbFile call to include interpolation method.

 Revision 1.2  2015/05/18 23:15:00  rshill
 Added a missing newline to a message.

 Revision 1.1  2015/05/14 22:31:55  rshill
 Converted language to plain C.  Refactored for adherence to standard flow of control.

 Revision 1.11  2015/03/18 23:15:41  rshill
 Support Keplerian elements via upgraded genorbfile.

 Revision 1.10  2015/01/07 19:30:00  rshill
 Update parameters as part of parameter update for all tasks.

 Revision 1.9  2014/10/02 18:10:25  rshill
 Eliminated false precision from numerical output.

 Revision 1.8  2014/09/10 03:59:07  mwitthoe
 aberposition: switch over to new leap second library

 Revision 1.7  2014/09/03 19:54:56  rshill
 Corrected bug in parsing names of orbital velocity columns
 for ORBFORMAT=COMPONENTS.

 Revision 1.6  2014/08/11 18:50:34  rshill
 Check orbitfile=NONE.

 Revision 1.5  2014/08/11 18:19:00  rshill
 Added documentation.

 Revision 1.4  2014/08/05 18:51:27  rshill
 Moved aber lib.

 Revision 1.3  2014/08/05 00:34:53  rshill
 Factored out support routines into a library.  Fixed bugs.

 Revision 1.2  2014/08/01 22:52:34  rshill
 Removed some debugging statements.

 Revision 1.1  2014/08/01 22:49:58  rshill
 Initial check-in.


*/
