/** \file coordpnt.c
 *  \brief Convert coordinates of point or region from one system to another
 *  \author Robert S. Hill
 *  \date $Date: 2016/04/07 20:57:13 $
 */

/**

  \defgroup tool_coordpnt Convert coordinates of point or region (coordpnt)
  \ingroup mod_gen_tasks

This task transforms a given point or region from one coordinate system to another.
The choice of coordinate systems includes the standard event file systems RAW,
ACT, DET, FOC, and SKY, as well as RADEC for world coordinates.  Also, the task
supports telescope coordinates TELPOL and TELXY, which are required for
raytracing, and it supports SXS-style pixel numbers in lieu of floating-point
RAW coordinates if appropriate.

Source files:

  coordpnt.c
  coordpntlib.c
  coordpntlib.h

Library depenencies:

  heacore/ahlog
  heacore/ape
  heacore/heaapp
  attitude/lib/coordfits

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-19  RSH     initial version, after cleaning code

*/

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

/* in attitude/lib/coordfits
   provides:
    * getCoordSystemNumberFromName()
    * enum for transformation types, as commnted below where they are used.
 */

#include "coordpntlib.h"
#include "teldef2.h"

#include "ahlog/cahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "headas_utils.h"

#include "heaapp/heaapp.h"
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
/*
  int startsys;  Number of first originating coord. sys.
  int stopsys;  Number of final destination coord. sys.
  int lowsys;  Lower of startsys and stopsys
  int highsys;  Higher of startsys and stopsys
*/

/* -------------------------------------------------------------------------- */
/* Top-level functions in standard main */

/** \brief Get parameter values
 *  \param[out] param parameter structure
 */ 
int getPar(Param **param);

/** \brief Set up tool operation
 *  \param[in] param parameter structure
 *  \param[out] trans_type_table table of region and transformation types
 *  \param[out] teldef TelDef structure
 *  \param[out] wcs structure with World Coordinate System information
 *  \param[out] SAORegion_input structure with input region information
 *  \param[out] q nominal pointing quaternion 
 *  \param[out] conv_to_higher flag for direction of conversion for ordinary coords
 *  \param[out] conv_to_higher_tel flag for direction of conversion for telescope coords
 *  \param[out] props array of possible MULTISEG properties
 *  \param[out] msegs array of MULTISEG segment numbers
 *  \param[out] segs array of RAWTODET segment numbers
 */
int initialize(Param* param, TransEnum*** trans_type_table, TELDEF2** teldef,
  long*** props, long** msegs, long** segs, 
  int* conv_to_higher, int* conv_to_higher_tel,
  QUAT** q, WCSdata** wcs, SAORegion** SAORegion_input); 

/** \brief Compute the output coordinates or region description
 *  \param[in] param parameter structure
 *  \param[in] trans_type_table table of region and transformation types
 *  \param[in] teldef TelDef structure
 *  \param[in] wcs structure with World Coordinate System information
 *  \param[in] SAORegion_input structure with input region information
 *  \param[in] q nominal pointing quaternion 
 *  \param[in] conv_to_higher flag for direction of conversion for ordinary coords
 *  \param[in] conv_to_higher_tel flag for direction of conversion for telescope coords
 *  \param[in] props array of possible MULTISEG properties
 *  \param[in] msegs array of MULTISEG segment numbers
 *  \param[in] segs array of RAWTODET segment numbers
 *  \param[out] SAORegion_output structure with output region information
 *  \param[out] pixel_list list of labeled pixels in region
 */
int doWork(Param* param, TransEnum** trans_type_table, TELDEF2* teldef,
  long** props, long* msegs, long* segs, 
  int conv_to_higher, int conv_to_higher_tel,
  QUAT* q, WCSdata* wcs, SAORegion* SAORegion_input, 
  SAORegion** SAORegion_output, PixelList** pixel_list);

/** \brief Write out the output coordinates (or region) and deallocate
 *   structures
 *  \param[in] param parameter structure
 *  \param[in] trans_type_table table of region and transformation types
 *  \param[in] teldef TelDef structure
 *  \param[in] wcs structure with World Coordinate System information
 *  \param[in] SAORegion_input structure with input region information
 *  \param[in] q nominal pointing quaternion 
 *  \param[in] props array of possible MULTISEG properties
 *  \param[in] msegs array of MULTISEG segment numbers
 *  \param[in] segs array of RAWTODET segment numbers
 *  \param[in] SAORegion_output structure with output region information
 *  \param[in] pixel_list list of labeled pixels in region
 */
int finalize(Param* param, TransEnum** trans_type_table, TELDEF2* teldef,
  long** props, long* msegs, long* segs,
  QUAT* q, WCSdata* wcs, SAORegion* SAORegion_input, 
  SAORegion* SAORegion_output, PixelList* pixel_list);

/* -------------------------------------------------------------------------- */
/* Lower-level functions  */

/** \brief Lower-level start-up for task in C language
 *  \param[in] argc Command-line argument count
 *  \param[in] argv Command-line argument strings
 *  \param[in] tooltag String labelling the tool
 *  \param[out] appdata Structure with low-level initialization information
 */
int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata);

/** \brief Lower-level routine in C language to dump parameters
 */
void writeParametersToLog();

/** \brief Lower-level shutdown for task in C language
 *  \param[in] appdata Structure with low-level initialization information
 */
int shutDown(HeaAppData* appdata);

/******************************************************************************/

/* \brief Execute the coordpnt tool. */
int main (
  int argc, /* Number of input arguments */
  char** argv /* Array of input arguments */
) {

  /* Declare the file, param struct, and info struct pointers and the
     CFITSIO and runtime statuses. */

  Param* param=0;
  TELDEF2* teldef = 0;
  TransEnum** trans_type_table = 0;
  QUAT* q = 0;
  SAORegion* SAORegion_input = 0;
  SAORegion* SAORegion_output = 0;
  PixelList* pixel_list = 0;
  WCSdata* wcs = 0;
  int conv_to_higher = 0;
  int conv_to_higher_tel = 0;
  long** props = 0;
  long* msegs = 0;
  long* segs = 0;

  int status = 0;         /* Initialization & execution status (0: normal) */
  int finalStatus = 0;    /* Status after shutdown (0: normal) */

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
      status = initialize(param, &trans_type_table, &teldef, &props, &msegs, &segs, 
        &conv_to_higher, &conv_to_higher_tel, &q, &wcs, &SAORegion_input);
      if (0 != status) {
        ahlog_err(__func__, "initialize returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    }
    
    if (0 == status) {
      status = doWork(param, trans_type_table, teldef, props, msegs, segs, 
        conv_to_higher, conv_to_higher_tel,
        q, wcs, SAORegion_input, &SAORegion_output, &pixel_list);
      if (0 != status) {
        ahlog_err(__func__, "doWork returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    } 

    status = finalize(param, trans_type_table, teldef, props, msegs, segs, q, wcs, 
      SAORegion_input, SAORegion_output, pixel_list);
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

/* -------------------------------------------------------------------------- */

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

/* -------------------------------------------------------------------------- */

int getPar(Param** param) {

  /* MAX_PARAM defined at the head of this file. */
  int modified_ape_status[MAX_PARAM];  /* Status of each APE query */
  int apes = 0;   /* One status */
  const char* par_names[MAX_PARAM];  /* Names of parameters queried, in order */
  int ipar = 0;  /* Parameter index */
  int n_param = 0;  /* Actual number of parameters */
  int ape_error_occurred = 0; /* Flag for one or more parameter errors */
  char t_par[FLEN_FILENAME];  /* Buffer for string manipulation */
  
  /* Local variables for parsing the input parameter. */

  char delimiter = ',';  /* INPUT may have comma-delimited values */
  char **pieces = NULL;  /* Pointers to comma-delimited parts of INPUT */
  int n_pieces = 0;  /* Number of comma-delimited parts of INPUT */
  int trim_blanks = 1;  /* Flag to trim blanks preceding and following comma-delimited strings */
  int skip_empty = 1;  /* Flag to skip empty slots between commas */
  int guard_paren = 0;  /* Flag to allow quoting by parens or quote marks */
  int parse_status = 0;  /* Flag for valid outcome of breaking up INPUT at commas */
  char *ptr = NULL;  /* Used by strtod and strtol */
  long t_in_pixel = 0;  /* Local storage for input pixel number */
  double t_in_x = 0.0;  /* Local storage for input X, RA, etc. */
  double t_in_y = 0.0;  /* Local storage for input Y, Dec, etc. */

  Param* parptr = createParam();  /* Create and initialize Param structure */
  *param = parptr;  /* Copy to output variable */

  /* Initialize APE parameter retrieval status for every parameter. */

  for (ipar=0; ipar<MAX_PARAM; ipar++) modified_ape_status[ipar] = 0;

  strcpy(t_par, "");

  ipar = 0;
  par_names[ipar] = "input";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->input)); ipar++;

  par_names[ipar] = "outfile";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->outfile)); ipar++;

  par_names[ipar] = "telescop";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->telescop)); ipar++;

  par_names[ipar] = "instrume";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->instrume)); ipar++;

  par_names[ipar] = "ra";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->ra));
  if (eUndefinedValue == apes) {
    parptr->ra_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->ra_undef = 0;
    modified_ape_status[ipar] = apes;
  } 
  ipar++;

  par_names[ipar] = "dec";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->dec));
  if (eUndefinedValue == apes) {
    parptr->dec_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->dec_undef = 0;
    modified_ape_status[ipar] = apes;
  }
  ipar++;
 
  par_names[ipar] = "roll";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->roll));
  if (eUndefinedValue == apes) {
    parptr->roll_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->roll_undef = 0;
    modified_ape_status[ipar] = apes;
  }
  ipar++;

  par_names[ipar] = "ranom";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->ranom));
  if (eUndefinedValue == apes) {
    parptr->ranom_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->ranom_undef = 0;
    modified_ape_status[ipar] = apes;
  }
  ipar++;

  par_names[ipar] = "decnom";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->decnom));
  if (eUndefinedValue == apes) {
    parptr->decnom_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->decnom_undef = 0;
    modified_ape_status[ipar] = apes;
  }
  ipar++;

  par_names[ipar] = "rollnom";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->rollnom));
  if (eUndefinedValue == apes) {
    parptr->rollnom_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->rollnom_undef = 0;
    modified_ape_status[ipar] = apes;
  }
  ipar++;

  par_names[ipar] = "teldeffile";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->teldeffile)); ipar++;

  par_names[ipar] = "startsys";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->startsysinput)); ipar++;

  par_names[ipar] = "stopsys";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->stopsysinput)); ipar++;

  par_names[ipar] = "multisegpar";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->multisegpar)); ipar++;
  
  par_names[ipar] = "pixeltest";
  modified_ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->pixeltest)); ipar++;

  par_names[ipar] = "rawtodetseg";
  modified_ape_status[ipar] = ape_trad_query_long(par_names[ipar], &(parptr->rawtodetseg)); ipar++;

  par_names[ipar] = "winoffsetx";
  modified_ape_status[ipar] = ape_trad_query_long(par_names[ipar], &(parptr->winoffsetx)); ipar++;

  par_names[ipar] = "winoffsety";
  modified_ape_status[ipar] = ape_trad_query_long(par_names[ipar], &(parptr->winoffsety)); ipar++;

  par_names[ipar] = "outx";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->outx));
  if (eUndefinedValue == apes) {
    parptr->outx_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->outx_undef = 1;  /* Initialize to undefined even if valid value given */
    modified_ape_status[ipar] = apes;
  }
  ipar++;

  par_names[ipar] = "outy";
  apes = ape_trad_query_double(par_names[ipar], &(parptr->outy));
  if (eUndefinedValue == apes) {
    parptr->outy_undef = 1;
    modified_ape_status[ipar] = eOK;
  } else {
    parptr->outy_undef = 1;  /* Initialize to undefined even if valid value given */
    modified_ape_status[ipar] = apes;
  }
  ipar++;

  par_names[ipar] = "clobber";
  modified_ape_status[ipar] = ape_trad_query_bool(par_names[ipar], &(parptr->clobber)); ipar++;

  n_param = ipar;

  /* Check for APE errors. */
  ape_error_occurred = 0;
  for (ipar=0; ipar<n_param; ipar++) {
    if (eOK != modified_ape_status[ipar]) {
      ahlog_err(__func__, "Error in parameter %s\n", par_names[ipar]);
      ape_error_occurred = 1;
    }
  }
  if (0 != ape_error_occurred) {
    ahlog_err(__func__, "One or more parameter errors encountered\n");
    return 1;
  }

  if (0 == strcasecmp(parptr->stopsysinput, parptr->startsysinput)) {
    ahlog_err(__func__, "Output coordinate system %s is the same as input coordinate system %s.\n",
      parptr->stopsysname, parptr->startsysname);
    return 1;
  }

  /* Need special processing for RADEC coordinates, since these are not used
   * in the teldef file. */
  parptr->startsys_is_radec = 0;
  parptr->startsys_is_telpol = 0;
  parptr->startsys_is_telxy = 0;
  parptr->stopsys_is_radec = 0;
  parptr->stopsys_is_telpol = 0;
  parptr->stopsys_is_telxy = 0;

  if (0 == strcasecmp(parptr->startsysinput, "RADEC")) {
    strcpy(parptr->startsysname, "SKY");
    parptr->startsys_is_radec = 1;
  } else if (0 == strcasecmp(parptr->startsysinput, "TELPOL")) {
    strcpy(parptr->startsysname, "OPTCOORD");
    parptr->startsys_is_telpol = 1;
  } else if (0 == strcasecmp(parptr->startsysinput, "TELXY")) {
    strcpy(parptr->startsysname, "OPTCOORD");
    parptr->startsys_is_telxy = 1;
  } else {
    strcpy(parptr->startsysname, parptr->startsysinput);
  }

  if (0 == strcasecmp(parptr->stopsysinput, "RADEC")) {
    strcpy(parptr->stopsysname, "SKY");
    parptr->stopsys_is_radec = 1;
  } else if (0 == strcasecmp(parptr->stopsysinput, "TELPOL")) {
    strcpy(parptr->stopsysname, "OPTCOORD");
    parptr->stopsys_is_telpol = 1;
  } else if (0 == strcasecmp(parptr->stopsysinput, "TELXY")) {
    strcpy(parptr->stopsysname, "OPTCOORD");
    parptr->stopsys_is_telxy = 1;
  } else {
    strcpy(parptr->stopsysname, parptr->stopsysinput);
  }

  ahlog_info(LOW, __func__, "startsysname=%s; stopsysname=%s\n", parptr->startsysname, parptr->stopsysname);
  ahlog_info(LOW, __func__, "startsys_is_radec=%d; stopsys_is_radec=%d\n", parptr->startsys_is_radec, parptr->stopsys_is_radec);
  ahlog_info(LOW, __func__, "startsys_is_telpol=%d; stopsys_is_telpol=%d\n", parptr->startsys_is_telpol, parptr->stopsys_is_telpol);
  ahlog_info(LOW, __func__, "startsys_is_telxy=%d; stopsys_is_telxy=%d\n", parptr->startsys_is_telxy, parptr->stopsys_is_telxy);

  if ((parptr->startsys_is_telpol && parptr->stopsys_is_telxy) ||
      (parptr->startsys_is_telxy  && parptr->stopsys_is_telpol)) {
    ahlog_err(__func__, "Cannot transform directly from one telescope system to another;\n");
    ahlog_err(__func__, "however, this can be done by using coordpnt twice, to transform\n");
    ahlog_err(__func__, "into the OPTCOORD system and then out of the OPTCOORD system.\n");
    return 1;
  }

  if (parptr->input_is_reg_file && (parptr->startsys_is_telpol || parptr->stopsys_is_telpol)) {
    ahlog_err(__func__, "For region files, telescope coordinates must be Cartesian (polar not permitted).\n");
    return 1;
  }

  /* Parse the input parameter to determine whether it is a point, the name of a
   * region file, or a pixel. */

  strcpy(t_par, parptr->input);
  pieces = expand_item_list(t_par, &n_pieces, delimiter, trim_blanks,
    skip_empty, guard_paren, &parse_status);

  if (0 != parse_status) {
    ahlog_err(__func__, "Parameter INPUT is malformed.\n");
    return 1;
  }

  /* There may be
   * (1) a filename (one string),
   * (2) a pixel (one number),
   * (3) a point (two numbers)
   */

  if (1 != n_pieces && 2 != n_pieces) {
    ahlog_err(__func__, "Parameter INPUT must have at most 2 comma-separated parts.\n");
    return 1;
  }

  if (1 == n_pieces) {  /* Filename or pixel */
    ptr = pieces[0];
    t_in_pixel = strtol(pieces[0], &ptr, 0);
    if (ptr == pieces[0] || ptr != pieces[0] + strlen(pieces[0])) { /* Not a number, so assume filename */
      strcpy(parptr->in_file, parptr->input);
      parptr->in_pixel = 0;
      parptr->in_x = 0.0;
      parptr->in_y = 0.0;
      parptr->input_is_reg_file = 1;
      parptr->input_is_pixel = 0;
      parptr->input_is_point = 0;
    } else { /* A number, so assume pixel */
      strcpy(parptr->in_file, "");
      parptr->in_pixel = t_in_pixel;
      parptr->in_x = 1.0;
      parptr->in_y = 1.0;
      parptr->input_is_reg_file = 0;
      parptr->input_is_pixel = 1;
      parptr->input_is_point = 0;
    }
  } else { /* n_pieces == 2, so assume point */
    ptr = pieces[0];
    t_in_x = strtod(pieces[0], &ptr);
    if (ptr == pieces[0] || ptr != pieces[0] + strlen(pieces[0])) {
      ahlog_err(__func__, "First coordinate of input point is not a well-formatted number\n");
      return 1;
    }
    ptr = pieces[1];
    t_in_y = strtod(pieces[1], &ptr);
    if (ptr == pieces[1] || ptr != pieces[1] + strlen(pieces[1])) {
      ahlog_err(__func__, "Second coordinate of input point is not a well-formatted number\n");
      return 1;
    }
    parptr->in_pixel = 0.0;
    parptr->in_x = t_in_x;
    parptr->in_y = t_in_y;
    strcpy(parptr->in_file, "");
    parptr->input_is_reg_file = 0;
    parptr->input_is_pixel = 0;
    parptr->input_is_point = 1;
  }

  /* Check ranges of angular quantities */

  if (0 == parptr->ra_undef) {
    if (0.0 > parptr->ra || 360.0 <= parptr->ra) {
      ahlog_err(__func__, "Actual pointing RA must be in range 0 <= RA < 360\n");
      return 1;
    }
  }
  if (0 == parptr->dec_undef) {
    if (-90.0 > parptr->dec || 90.0 < parptr->dec) {
      ahlog_err(__func__, "Actual pointing Dec must be in range -90 <= Dec <= 90\n");
      return 1;
    }
  }

  /* Roll angle check is deprecated
  if (0 == parptr->roll_undef) {
    if (-180.0 > parptr->roll || 180.0 < parptr->roll) {
      ahlog_err(__func__, "Actual pointing roll must be in range -180 <= roll <= 180\n");
      return 1;
    }
  }
  */
  if (0 == parptr->ranom_undef) {
    if (-999.0 != parptr->ranom) {
      if (0.0 > parptr->ranom || 360.0 <= parptr->ranom) {
        ahlog_err(__func__, "Nominal pointing RA_NOM must be -999 or in range 0 <= RA_NOM < 360\n");
        return 1;
      }
    }
  }
  if (0 == parptr->decnom_undef) {
    if (-999.0 != parptr->decnom) {
      if (-90.0 > parptr->decnom || 90.0 <= parptr->decnom) {
        ahlog_err(__func__, "Nominal pointing DEC_NOM must be -999 or in range -90 <= DEC_NOM < 90\n");
        return 1;
      }
    }
  }
  /* Roll angle check is deprecated
  if (0 == parptr->rollnom_undef) {
    if (-999.0 != parptr->rollnom) {
      if (-180.0 > parptr->rollnom || 180.0 <= parptr->rollnom) {
        ahlog_err(__func__, "Nominal pointing ROLL_NOM must be -999 or in range -180 <= ROLL_NOM < 180\n");
        return 1;
      }
    }
  }
  */

  return 0;

}

/* -------------------------------------------------------------------------- */

int initialize(Param* param, TransEnum*** trans_type_table, TELDEF2** teldef,
  long*** props, long** msegs, long** segs, 
  int* conv_to_higher, int* conv_to_higher_tel,
  QUAT** q, WCSdata** wcs, SAORegion** SAORegion_input) {

  /* Internal variables */

  int cf_status = 0;  /* CFITSIO status */
  int teldef_status = 0;  /* Status of reading teldef file */
  TELDEF2* pteldef = 0;  /* Local pointer to teldef structure */
  int ape_status_set = 0;  /* Status from ape_trad_set_string */
  int sys = 0;  /* Coordinate system index */
  int prop = 0;  /* Multiseg property index */
  int pixel_type_not_valid = 0;  /* Flag for results of checking PIXEL input spec */
  int lowest_sys = 0;  /* Minimum coord system index */
  int highest_sys = 0;  /* Minimum coord system index */
  int optcoord_sysnum = 0;  /* Telescope coordinate system number */
  int n_pieces = 0;  /* Number of comma-separated pieces in string */
  const char delimiter = ',';  /* Delimiter for string parsing */
  const int skip_empty = 1;  /* For string parsing, skip empty substrings */
  const int trim_blanks = 1;  /* For string parsing, trim blanks from values */
  const int guard_paren = 0;  /* For string parsing, honor parens and quotes */
  int parse_status = 0;  /* Status of strin parsing */
  int n_prop = 0;  /* Number of MULTISEG properties */
  int props_found = 0;  /* Flag used in MULTISEG property search */
  int counter = 0;  /* Counter used in MULTISEG property search */
  int telcount = 0;  /* Count the number of times TEL* systems are specified (must be 1) */
  char* ptr = 0;  /* Pointer used to parse numbers with strtod */
  char** pieces = 0;  /* Output of expand_list_items */
  /* MAX_PROPS defined at the head of this file */
  long props_list[MAX_PROPS];  /* Values of MULTISEG properties */
  int row = 0;  /* Loop index for rows of a MULTISEG table */
  int use_default_row = 0;  /* Flag to use default MULTISEG row */
  double ranom_eff=0.0;  /* Effective values of the nominal pointing parameters */
  double decnom_eff=0.0;
  double rollnom_eff=0.0;

  /* FLEN_FILENAME defined in fitsio.h */
  char* t_par = (char *)calloc(FLEN_FILENAME, 1);  /* Temporary for C-style string */

  int i=0, j=0, k=0;  /* Loop indexes */

  char t_teldeffile[FLEN_FILENAME];  /* Temporary for TelDef filename */
  char t_coordsysname[COORDSYSNAME_LENGTH];  /* Temporary for coordinate system name */
  char t_trtypename[FLEN_VALUE];  /* Temporary for tranformation type name */
  char t_segcolname[FLEN_VALUE];  /* Temporary for segment number column name */
  char t_instrume[FLEN_VALUE];  /* Temporary for instrume in teldef file */

  strcpy(t_teldeffile, "");
  strcpy(t_coordsysname, "");
  strcpy(t_trtypename, "");
  strcpy(t_segcolname, "");
  strcpy(t_instrume, "");

  /* MULTISEG arrays are subscripted (for example) props[sys][prop]. */

  /* Structures defined in heacore/cfitsio/region.h:
   * WCSdata, SAORegion */

  /* Current attitude quaternion */

  *q = allocateQuat();
  setQuatToIdentity(*q);

  /* === Initialization 1 of 6:  Region Transformation Table === */
  /* MAX_SHAPES and MAX_TRANS defined at the head of this file. */
  (*trans_type_table) = (TransEnum**)calloc(MAX_SHAPES, sizeof(TransEnum*));
  for (i=0; i<MAX_SHAPES; i++) {
    (*trans_type_table)[i] = (TransEnum*)calloc(MAX_TRANS, sizeof(TransEnum));
  }

  for (i=0; i<MAX_SHAPES; i++) {
    for (j=0; j<MAX_TRANS; j++) {
      (*trans_type_table)[i][j] = e_nullTrans;
    }
  }

  /* Fill in the transformation types, with brute force. */
  /* The enum for region shapes, *_rgn, is in region.h. */
  /* The enum for the transformation types, TransEnum, is defined 
   * in the top part of this file. */

  (*trans_type_table)[point_rgn][0] = e_pointTrans;
  (*trans_type_table)[point_rgn][1] = e_pointTrans;

  (*trans_type_table)[line_rgn][0] = e_pointTrans;
  (*trans_type_table)[line_rgn][1] = e_pointTrans;
  (*trans_type_table)[line_rgn][2] = e_pointTrans;
  (*trans_type_table)[line_rgn][3] = e_pointTrans;

  (*trans_type_table)[circle_rgn][0] = e_pointTrans;
  (*trans_type_table)[circle_rgn][1] = e_pointTrans;
  (*trans_type_table)[circle_rgn][2] = e_lengthTrans;

  (*trans_type_table)[annulus_rgn][0] = e_pointTrans;
  (*trans_type_table)[annulus_rgn][1] = e_pointTrans;
  (*trans_type_table)[annulus_rgn][2] = e_lengthTrans;
  (*trans_type_table)[annulus_rgn][3] = e_lengthTrans;

  (*trans_type_table)[ellipse_rgn][0] = e_pointTrans;
  (*trans_type_table)[ellipse_rgn][1] = e_pointTrans;
  (*trans_type_table)[ellipse_rgn][2] = e_lengthTrans;
  (*trans_type_table)[ellipse_rgn][3] = e_lengthTrans;
  (*trans_type_table)[ellipse_rgn][4] = e_angleTrans;

  (*trans_type_table)[elliptannulus_rgn][0] = e_pointTrans;
  (*trans_type_table)[elliptannulus_rgn][1] = e_pointTrans;
  (*trans_type_table)[elliptannulus_rgn][2] = e_lengthTrans;
  (*trans_type_table)[elliptannulus_rgn][3] = e_lengthTrans;
  (*trans_type_table)[elliptannulus_rgn][4] = e_lengthTrans;
  (*trans_type_table)[elliptannulus_rgn][5] = e_lengthTrans;
  (*trans_type_table)[elliptannulus_rgn][6] = e_angleTrans;

  (*trans_type_table)[box_rgn][0] = e_pointTrans;
  (*trans_type_table)[box_rgn][1] = e_pointTrans;
  (*trans_type_table)[box_rgn][2] = e_lengthTrans;
  (*trans_type_table)[box_rgn][3] = e_lengthTrans;
  (*trans_type_table)[box_rgn][4] = e_angleTrans;

  (*trans_type_table)[boxannulus_rgn][0] = e_pointTrans;
  (*trans_type_table)[boxannulus_rgn][1] = e_pointTrans;
  (*trans_type_table)[boxannulus_rgn][2] = e_lengthTrans;
  (*trans_type_table)[boxannulus_rgn][3] = e_lengthTrans;
  (*trans_type_table)[boxannulus_rgn][4] = e_lengthTrans;
  (*trans_type_table)[boxannulus_rgn][5] = e_lengthTrans;
  (*trans_type_table)[boxannulus_rgn][6] = e_angleTrans;

  (*trans_type_table)[rectangle_rgn][0] = e_pointTrans;
  (*trans_type_table)[rectangle_rgn][1] = e_pointTrans;
  (*trans_type_table)[rectangle_rgn][2] = e_pointTrans;
  (*trans_type_table)[rectangle_rgn][3] = e_pointTrans;
  (*trans_type_table)[rectangle_rgn][4] = e_angleTrans;
  (*trans_type_table)[rectangle_rgn][5] = e_lengthTrans;
  (*trans_type_table)[rectangle_rgn][6] = e_lengthTrans;

  (*trans_type_table)[diamond_rgn][0] = e_pointTrans;
  (*trans_type_table)[diamond_rgn][1] = e_pointTrans;
  (*trans_type_table)[diamond_rgn][2] = e_lengthTrans;
  (*trans_type_table)[diamond_rgn][3] = e_lengthTrans;
  (*trans_type_table)[diamond_rgn][4] = e_angleTrans;

  (*trans_type_table)[sector_rgn][0] = e_pointTrans;
  (*trans_type_table)[sector_rgn][1] = e_pointTrans;
  (*trans_type_table)[sector_rgn][2] = e_numberTrans;
  (*trans_type_table)[sector_rgn][3] = e_numberTrans;

  (*trans_type_table)[poly_rgn][0] = e_pointTrans;
  (*trans_type_table)[poly_rgn][1] = e_pointTrans;
  (*trans_type_table)[poly_rgn][2] = e_pointTrans;
  (*trans_type_table)[poly_rgn][3] = e_pointTrans;
  (*trans_type_table)[poly_rgn][4] = e_pointTrans;
  (*trans_type_table)[poly_rgn][5] = e_pointTrans;
  (*trans_type_table)[poly_rgn][6] = e_pointTrans;
  (*trans_type_table)[poly_rgn][7] = e_pointTrans;
  (*trans_type_table)[poly_rgn][8] = e_pointTrans;
  (*trans_type_table)[poly_rgn][9] = e_pointTrans;

  (*trans_type_table)[panda_rgn][0] = e_pointTrans;
  (*trans_type_table)[panda_rgn][1] = e_pointTrans;
  (*trans_type_table)[panda_rgn][2] = e_numberTrans;
  (*trans_type_table)[panda_rgn][3] = e_numberTrans;
  (*trans_type_table)[panda_rgn][4] = e_numberTrans;
  (*trans_type_table)[panda_rgn][5] = e_lengthTrans;
  (*trans_type_table)[panda_rgn][6] = e_lengthTrans;
  (*trans_type_table)[panda_rgn][7] = e_numberTrans;

  (*trans_type_table)[epanda_rgn][0] = e_pointTrans;
  (*trans_type_table)[epanda_rgn][1] = e_pointTrans;
  (*trans_type_table)[epanda_rgn][2] = e_numberTrans;
  (*trans_type_table)[epanda_rgn][3] = e_numberTrans;
  (*trans_type_table)[epanda_rgn][4] = e_numberTrans;
  (*trans_type_table)[epanda_rgn][5] = e_lengthTrans;
  (*trans_type_table)[epanda_rgn][6] = e_lengthTrans;
  (*trans_type_table)[epanda_rgn][7] = e_lengthTrans;
  (*trans_type_table)[epanda_rgn][8] = e_lengthTrans;
  (*trans_type_table)[epanda_rgn][9] = e_numberTrans;
  (*trans_type_table)[epanda_rgn][10] = e_angleTrans;

  (*trans_type_table)[bpanda_rgn][0] = e_pointTrans;
  (*trans_type_table)[bpanda_rgn][1] = e_pointTrans;
  (*trans_type_table)[bpanda_rgn][2] = e_numberTrans;
  (*trans_type_table)[bpanda_rgn][3] = e_numberTrans;
  (*trans_type_table)[bpanda_rgn][4] = e_numberTrans;
  (*trans_type_table)[bpanda_rgn][5] = e_lengthTrans;
  (*trans_type_table)[bpanda_rgn][6] = e_lengthTrans;
  (*trans_type_table)[bpanda_rgn][7] = e_lengthTrans;
  (*trans_type_table)[bpanda_rgn][8] = e_lengthTrans;
  (*trans_type_table)[bpanda_rgn][9] = e_numberTrans;
  (*trans_type_table)[bpanda_rgn][10] = e_angleTrans;

  /* === Initialization 2 of 6:  teldef Structure === */

  /* Find the teldef file */

  if (0 == strcasecmp(param->teldeffile, "CALDB")) {
    if (0 != strcasecmp(param->telescop, "NULL") && 0 != strcasecmp(param->instrume, "NULL")) {
      /* Query CALDB for teldef based on telescop and instrume */
      teldef_status = resolveTeldefFilename(t_teldeffile, param->telescop, param->instrume);
      if (0 != teldef_status) {
        ahlog_err(__func__, "Unable to resolve TelDef filename.\n");
        return 1;
      }
      ape_status_set = ape_trad_set_string("teldeffile", t_teldeffile);
      if (0 != ape_status_set) {
        ahlog_err(__func__, "Unable to set teldeffile parameter to actual filename.\n");
        return 1;
      }
      /*  ahlog_err (__func__, "CALDB query for teldef file not yet implemented\n");
      return 1; */
    } else {
      ahlog_err (__func__, "Teldef specified as CALDB, but TELESCOPE and INSTRUMENT not found\n");
      return 1;
    }
  } else {
    strcpy(t_teldeffile, param->teldeffile);
  }

  /* Read the teldef file */

  teldef_status = readTelDef2(t_teldeffile, teldef);
  if (0 != teldef_status) {
    fits_report_error(stderr, teldef_status);
    ahlog_err(__func__, "Could not read TELDEF file\n");
    return 1;
  }
  pteldef = *teldef;  /* Set a local pointer for simplicity in syntax */

  /* Make sure teldef matches instrume */
  strcpy(t_instrume, pteldef->instrument);
  if (0 != strcasecmp(t_instrume, param->instrume)) {
    ahlog_err(__func__, "Instrument in TELDEF file is %s\n", pteldef->instrument);
    ahlog_err(__func__, "Instrument parameter is %s\n", param->instrume);
    ahlog_err(__func__, "Instrument specified does not match TELDEF file.\n");
    return 1;
  }

  if (0 != ahlog_get_debug()) {
    printTelDef2(pteldef, stderr);
  } 
  
  /* === Initialization 3 of 6:  Attributes of coordinate systems === */

  /* Get optical coordinate system attributes from teldef structure. */

  optcoord_sysnum = pteldef->telescopeparam->low_sys;

  ahlog_info(LOW, __func__,"optcoord_sysnum: %d\n",
    optcoord_sysnum);

  /* Determine start and stop systems */

  /* (1) Check startsys and stopsys parameters and get coord system numbers
   * corresponding to the system names.  
   * (2) Translate LOWEST and HIGHEST to the first and list coordinate system
   * names if the user gave these values. Otherwise, verify that the startsys
   * and stopsys values are real coordinate system names from the teldef file. 
   */

  lowest_sys = 0;
  highest_sys = pteldef->n_coordsys - 1;
  ahlog_info(HIGH, __func__, "lowest_sys, highest_sys: %d, %d\n", lowest_sys, highest_sys);
  ahlog_info(HIGH, __func__, "startsysname: %s\n", param->startsysname);
  if (0 == strcasecmp(param->startsysname, "LOWEST")) {
    param->startsysnum = lowest_sys;
  } else if (0 == strcasecmp(param->startsysname, "HIGHEST")) {
    param->startsysnum = highest_sys;
  } else if (0 == strcasecmp(param->startsysname, "OPTCOORD")) {
    param->startsysnum = optcoord_sysnum;
  } else {
    param->startsysnum = getCoordSystemNumberFromName(pteldef, param->startsysname);
  }
  ahlog_info(HIGH, __func__, "startsysnum: %d\n", param->startsysnum);
  ahlog_info(HIGH, __func__, "stopsysname: %s\n", param->stopsysname);
  if (0 == strcasecmp(param->stopsysname, "LOWEST")) {
    param->stopsysnum = lowest_sys;
  } else if (0 == strcasecmp(param->stopsysname, "HIGHEST")) {
    param->stopsysnum = highest_sys;
  } else if (0 == strcasecmp(param->stopsysname, "OPTCOORD")) {
    param->stopsysnum = optcoord_sysnum;
  } else {
    param->stopsysnum = getCoordSystemNumberFromName(pteldef, param->stopsysname);
  }
  ahlog_info(HIGH, __func__, "stopsysnum: %d\n", param->stopsysnum);

  if (0 > param->startsysnum) {
    ahlog_err(__func__, "Starting coordinate system name not recognized\n");
    return 1;
  }

  if (0 > param->stopsysnum) {
    ahlog_err(__func__, "Ending coordinate system name not recognized\n");
    return 1;
  }

  /* Set all of the flags describing conversion directions to "higher" or "lower"
   * systems.  This is not always simple because there are conversions that are
   * outside the main chain RAW<->ACT<->DET<->SKY.  Here the program sorts out
   * two of these offshoot cases:  (1) conversion between SKY coordinates, 
   * which are in a Cartesian projection such as tangent plane, and equatorial 
   * coordinates RA and Dec; (2) conversion between some instrument-based
   * coordinates (e.g., DET) and telescope coordinates used in optical calculation.
   */

  /* Check if telescope coordinates (TELXY or TELPOL) are part of the
   * calculation. */

  if (0 != param->startsys_is_telpol || 
      0 != param->startsys_is_telxy  || 
      0 != param->stopsys_is_telpol  || 
      0 != param->stopsys_is_telxy) {

    /* Error check: make sure telescope coord sys is used exactly once. */
    telcount = 0;
    if (0 != param->startsys_is_telpol) telcount++;
    if (0 != param->stopsys_is_telpol) telcount++;
    if (0 != param->startsys_is_telxy) telcount++;
    if (0 != param->stopsys_is_telxy) telcount++;
    if (1 != telcount) {
      ahlog_err(__func__, "Telescope coordinate system specified %d times; should be exactly once.\n", telcount);
      return 1;
    }

    if (-999 == optcoord_sysnum) {
      ahlog_err(__func__, "Cannot transform to telescope coordinates because the optical axis is undefined.\n");
      return 1;
    } else {
      if (param->startsysnum >= optcoord_sysnum || param->stopsysnum < optcoord_sysnum) *conv_to_higher = 0;
      if (param->startsysnum <= optcoord_sysnum || param->stopsysnum > optcoord_sysnum) *conv_to_higher = 1;
      if (0 != param->startsys_is_telpol || 0 != param->startsys_is_telxy) *conv_to_higher_tel = 1;
      if (0 != param->stopsys_is_telpol || 0 != param->stopsys_is_telxy) *conv_to_higher_tel = 0;
    }
  } else {   

    /* Telescope coordinates are not involved, so examine main coordinate chain + (RA, Dec). */

    if (param->startsysnum < param->stopsysnum) {
      *conv_to_higher = 1;
    } else if (param->startsysnum > param->stopsysnum) {
      *conv_to_higher = 0;
    } else {
      if (param->stopsysnum == highest_sys && 0 != param->stopsys_is_radec) {
        *conv_to_higher = 1;
      } else if (param->startsysnum == highest_sys && 0 != param->startsys_is_radec) {
        *conv_to_higher = 0;
      } else {
        ahlog_err(__func__, "Starting and ending coordinate systems are the same\n");
        return 1;
      }
    }
  }

  param->lowsysnum = param->startsysnum;
  if (param->stopsysnum < param->lowsysnum) param->lowsysnum = param->stopsysnum;
  param->highsysnum = param->stopsysnum;
  if (param->startsysnum > param->highsysnum) param->highsysnum = param->startsysnum;

  /* Another offshoot coordinate system is pixel numbers, which may be associated
   * with the RAW level of coordinates for some instruments. */

  if (0 != param->input_is_pixel) {
    pixel_type_not_valid = 0;
    sys = param->startsysnum;
    strcpy(t_coordsysname, pteldef->coordsysnames[sys]);
    strcpy(t_trtypename, pteldef->trtypenames[sys]);
    if (0 == strcasecmp(t_coordsysname, "RAW") && 0 == strcasecmp(t_trtypename, "RAWTODET")) {
      strcpy(t_segcolname, pteldef->rawtodetparam[sys]->segcolname);
      if (0 != strcasecmp(t_segcolname, "PIXEL")) {
        pixel_type_not_valid = 1;
      }
    } else {
      pixel_type_not_valid = 1;
    }
    if (0 != pixel_type_not_valid) {
      ahlog_err(__func__, "When PIXEL input specified, the starting coord sys must be RAW with segment column PIXEL\n");  
      return 1;
    }
  }

  /* === Initialization 4 of 6:  WCSdata === */

  /* Check range of inputs, if RA and Dec specified */

  /* Set the tangent plane coordinate system corresponding to the nominal pointing.
   * If there is a SKYATT transformation that leads to SKY coordinates, set the
   * sky coordinates tangent plane rotation matrix corresponding to the nominal
   * pointing (ra, dec, roll) and the teldef roll keywords ROLLSIGN and ROLLOFF.
   * Begin by converting (ra, dec, roll) to the Euler angle triplet (ra, 90-dec,
   * ROLLSIGN*rol + 90 - ROLLOFF)*pi/180, and then convert the triplet to
   * a unit quaternion using the attitude-coord library function convertEulertoQuat().
   */

  if (param->startsysnum == highest_sys || param->stopsysnum == highest_sys) {

    if (0 != param->ra_undef || 0 != param->dec_undef || 0 != param->roll_undef) {
      ahlog_err(__func__, "Actual pointing (attitude) missing or incomplete (RA, DEC, ROLL\n");
      return 1;
    }

    ahlog_info(LOW, __func__, "Values of actual pointing parameters:\n");
    ahlog_info(LOW, __func__, "ra: %.8g\n", param->ra);
    ahlog_info(LOW, __func__, "dec: %.8g\n", param->dec);
    ahlog_info(LOW, __func__, "roll: %.8g\n", param->roll);

    /* Set the attitude quaternion. */

    if (0 != pteldef->skyattparam[pteldef->n_coordsys-2]) {
      convertRADecRollToQuat(pteldef->skyattparam[pteldef->n_coordsys-2]->alignment,
        *q, param->ra, param->dec, param->roll);
      ahlog_info(LOW, __func__,"Actual pointing quaternion (attitude):  %.8g %.8g %.8g %.8g\n",
        (*q)->p[0], (*q)->p[1] ,(*q)->p[2], (*q)->p[3]);
    }

    if (0 != param->ranom_undef || 0 != param->decnom_undef || 0 != param->rollnom_undef) {
      ahlog_err(__func__, "Nominal pointing missing or incomplete (RA_NOM, DEC_NOM, ROLL_NOM\n");
      return 1;
    }

    if (-999.0 == param->ranom) {
      ranom_eff = param->ra;
    } else {
      ranom_eff = param->ranom;
    }
    if (-999.0 == param->decnom) {
      decnom_eff = param->dec;
    } else {
      decnom_eff = param->decnom;
    }
    if (-999.0 == param->rollnom) {
      rollnom_eff = param->roll;
    } else {
      rollnom_eff = param->rollnom;
    }
    ahlog_info(LOW, __func__, "Effective values of nominal pointing parameters:\n");
    ahlog_info(LOW, __func__, "ra: %.8g\n", ranom_eff);
    ahlog_info(LOW, __func__, "dec: %.8g\n", decnom_eff);
    ahlog_info(LOW, __func__, "roll: %.8g\n", rollnom_eff);

    *wcs = fillWCSstructureFromParams(ranom_eff, decnom_eff, rollnom_eff, 
      pteldef->coordsys[pteldef->n_coordsys-2][0], pteldef->skyattparam[pteldef->n_coordsys-2]);

    ahlog_info(LOW, __func__, "WCSdata structure:\n");
    ahlog_info(LOW, __func__, "exists:  %d\n", (*wcs)->exists);
    ahlog_info(LOW, __func__, "xrefval: %.8g\n", (*wcs)->xrefval);
    ahlog_info(LOW, __func__, "yrefval: %.8g\n", (*wcs)->yrefval);
    ahlog_info(LOW, __func__, "xrefpix: %.8g\n", (*wcs)->xrefpix);
    ahlog_info(LOW, __func__, "yrefpix: %.8g\n", (*wcs)->yrefpix);
    ahlog_info(LOW, __func__, "xinc:    %.8g\n", (*wcs)->xinc);
    ahlog_info(LOW, __func__, "yinc:    %.8g\n", (*wcs)->yinc);
    ahlog_info(LOW, __func__, "rot:     %.8g\n", (*wcs)->rot);
    ahlog_info(LOW, __func__, "type:    %s\n", (*wcs)->type);

    /* Set the nominal pointing quaternion. */

    /* From Astro-H Coordinates document:  The third [quaternion]
     * is a static (for a given observation) pointing quaternion Q_B derived
     * from the nominal pointing RA (alpha) and Declination (delta) with roll 
     * set to zero. */
 
    setSkyCoordCenterInTelDef2(pteldef, ranom_eff, decnom_eff, rollnom_eff);
    ahlog_info(LOW, __func__, "Nominal quaternion (attitude):  %.8g %.8g %.8g %.8g\n",
      pteldef->skyattparam[pteldef->n_coordsys-2]->q0->p[0], 
      pteldef->skyattparam[pteldef->n_coordsys-2]->q0->p[1], 
      pteldef->skyattparam[pteldef->n_coordsys-2]->q0->p[2], 
      pteldef->skyattparam[pteldef->n_coordsys-2]->q0->p[3]);
  }

  /* === Initialization 5 of 6:  SAORegion_input (region) structure === */

  /* Read region file if applicable */

  if (0 != param->input_is_reg_file) {
    
    /* Call to modified version of function 
       in heacore/cfistio/region.c to fill the region structures */
    cf_status = coordpnt_read_ascii_region(param->in_file, *wcs, SAORegion_input, &cf_status);

    if (0 != cf_status) {
      ahlog_err(__func__, "Unable to read region file\n");
      return 1;
    }
  }

  /* === Initialization 6 of 6:  MULTISEG Attributes === */

  /* Allocate space for the segments and properties. */

  /* props[sys] is the vector of multiseg properties that applies to system number sys.
   * msegs[sys] is the selected multiseg table row that applies to system number sys.
   * segs[sys] is the rawtodetseg that applies to system number sys. */

  *props = (long **)calloc(pteldef->n_coordsys, sizeof(long *));
  *msegs = (long *)calloc(pteldef->n_coordsys, sizeof(long *));
  *segs = (long *)calloc(pteldef->n_coordsys, sizeof(long *));

  for (sys=0; sys<pteldef->n_coordsys; sys++) {
    (*props)[sys] = 0;
    (*msegs)[sys] = 0;
    (*segs)[sys] = 0;
  }

  for (sys=0; sys<pteldef->n_coordsys; sys++) {
    (*props)[sys] = (long*)calloc(MAX_PROPS, sizeof(long));
    for (k=0; k < MAX_PROPS; k++) (*props)[sys][k]=0;
  }

  /* Fill the required parameters for the MULTISEG transformations from the
   * command line parameter. */

  /* Loop through the systems, looking for any MULTISEG transformations 
   * or RAWTODET transformations that need a segment number.  */

  ahlog_info(LOW, __func__, "Looking for MULTISEG transformation:\n");
  ahlog_info(LOW, __func__, "lowsysnum, highsysnum: %d, %d\n", param->lowsysnum, param->highsysnum);
  ahlog_info(LOW, __func__, "startsysnum, stopsysnum: %d, %d\n", param->startsysnum, param->stopsysnum);
  for (sys=param->lowsysnum; sys < param->highsysnum; sys++) {

    /* enum of transformation types is in coordfits2.h. */
    if (e_TT_MULTISEG == pteldef->trtypes[sys]) {

      /* Parse the MULTISEG properties from the command-line parameter
       * param->multisegpar.  Have to assume that the multiseg properties
       * are in the right order. */

      n_prop = pteldef->multisegparam[sys]->n_properties;

      strcpy(t_par, param->multisegpar);

      pieces = expand_item_list(t_par, &n_pieces, delimiter, trim_blanks,
        skip_empty, guard_paren, &parse_status);

      if (MAX_PROPS < n_pieces || MAX_PROPS < n_prop) {
        ahlog_err(__func__, "Array props_list is too short\n");
        return 1;
      }

      use_default_row = 0;

      if (n_pieces == n_prop) {

        /* Convert CSV to long */

        for (prop=0; prop<n_prop; prop++) {
          ptr = pieces[prop];
          props_list[prop] = strtol(pieces[prop], &ptr, 0);
          if (ptr == pieces[prop] || ptr != pieces[prop] + strlen(pieces[prop])) {
            ahlog_err(__func__, "Could not convert CSV list item to property value\n");
            return 1;
          }
        }
          
        /* Loop through rows in MULTISEG table. */

        for (row=0; row<pteldef->multisegparam[sys]->n_rows; row++) {
          props_found = 0;
          counter = 0;

          for (prop=0; prop<n_prop; prop++) {

            /* Check whether the provided property matches what is in the current row in
             * the teldef MULTISEGn_COEFF table.  All must match! */

            if (props_list[prop] == pteldef->multisegparam[sys]->properties[prop][row]) counter++;
          }

          if (counter == n_prop) {
            props_found = 1;
            (*msegs)[sys] = row;
            break;
          }
        }

        /* If found in the table, use the value, otherwise default to lowest value 
         * in the table for each property. */
        if (0 != props_found) {
          for (prop=0; prop<n_prop; prop++) {
            ahlog_info(HIGH, __func__, "using MULTISEG properties: sys=%d, prop=%d, value=%d\n", 
              sys, prop, props_list[prop]);
            (*props)[sys][prop] = props_list[prop];
          }
        } else {
          use_default_row = 1;  /* Use default row because no matching row found */
        }
      } else {
        use_default_row = 1;  /* Use default row because multisegpar unusable */
      }

      /* If the user did not supply proper multiseg parameters, then use the
       * row having the minimum values for all of those parameters. */

      if (0 != use_default_row) {

        /* Find the row with all the minumum property values; */
        for (prop=0; prop<n_prop; prop++) {
          (*props)[sys][prop] = pteldef->multisegparam[sys]->min_properties[prop];
            ahlog_info(HIGH, __func__, "using MULTISEG properties: sys=%d, prop=%d, value=%d\n", 
             sys, prop, pteldef->multisegparam[sys]->min_properties[prop]);
        }
        for (row=0; row<pteldef->multisegparam[sys]->n_rows; row++) {
          props_found = 0;
          counter = 0;
          for (prop=0; prop<n_prop; prop++) {
            if ((*props)[sys][prop] == pteldef->multisegparam[sys]->properties[prop][row]) counter++;
          }
          if (counter == n_prop) {
            props_found = 1;
            (*msegs)[sys] = row;
            break;
          }
        }
      }
      ahlog_info(HIGH, __func__, "Selected row in MULTISEG param table: %d\n", (*msegs)[sys]);
      if (0 == props_found) {
        ahlog_err(__func__, "Neither the user-specified nor default MULTISEG coeffs were found\n");
        return 1;
      }
    } /* end if e_TT_MULTISEG */
    /* enum of transformation types is in coordfits2.h. */
    if (e_TT_RAWTODET == pteldef->trtypes[sys] && RM_LINEAR_COEFF == pteldef->rawtodetparam[sys]->rawmethod) {
      (*segs)[sys] = param->rawtodetseg;
    }
  } /* end of loop through systems searching for e_TT_MULTISEG */

  /* Dump listing of parameters to log output. */
  writeParametersToLog();

  return 0;

} /* end of initialize function */

/* -------------------------------------------------------------------------- */
int doWork(Param* param, TransEnum** trans_type_table, TELDEF2* teldef,
  long** props, long* msegs, long* segs, 
  int conv_to_higher, int conv_to_higher_tel,
  QUAT* q, WCSdata* wcs, SAORegion* SAORegion_input, 
  SAORegion** SAORegion_output, PixelList** pixel_list) {

  int sys = 0;  /* Index of coordinate system */
  int prop = 0;  /* Index of MULTISEG properties */
  int bad_region = 0;  /* Flag for bad SAOregion */
  double x = 0.0;  /* X coordinate being calculated */
  double y = 0.0;  /* Y coordinate being calculated */
  double prev_x = 0.0;  /* X coordinate from previous system */
  double prev_y = 0.0;  /* Y coordinate from previous system */
  int convertstatus = 0;  /* 0 (false) = success in conversion, 1 (true) = failure */
  int stepstatus = 0;  /* 0 (false) = success in one conversion step, 1 (true) = failure */
  int* rowprops; /* Array of MULTISEG properties for a given system */
  int i = 0, j = 0, k = 0, isys = 0;  /* Loop indexes */
  int n_iter = 0;  /* Loop limit for going through coord systems */
  int number_of_shapes = 0;  /* Number of shapes in input region file */
  int optcoord_sysnum = 0;  /* System number of coords in which optical axis defined */

  int num_trans_params = 0;  /* Number of transformation parameters for given shape */

  shapeType shape;  /* Shape enum from region.h */
  TransEnum trans_type;  /* Transformation type */
  double v_sat_total = 0.0;  /* Satellite speed (always 0.0, aberration not implemented) */
  double vhat_sat_total[3];  /* Satellite velocity unit vector */
  int t_seg = 0;  /* Temporary for segment count or number */
  double length_start = 0.0; /* Initial length for a length transform */
  double length_finish = 0.0; /* Final length for a length transform */
  double angle_start = 0.0; /* Initial angle for an angle transform */
  double angle_finish = 0.0; /* Final angle for an angle transform */
  int make_pixel_list = 0; /* Flag for whether pixel_list will be filled */
  PixelSet* pixel_set = 0; /* Pixels included within one region */

  /* Working copies of SAORegions */
  SAORegion* SAORegion_initial = 0;
  SAORegion* SAORegion_intermediate = 0;

  /* Working attitude quaternion */
  QUAT q1;
  setQuatToIdentity(&q1);

  /* Get system number for conversion to/from raytracing coordinates */
  optcoord_sysnum = teldef->telescopeparam->low_sys;

  /* Initialize velocity to zero */
  for (i=0; i<3; i++) {
    vhat_sat_total[i] = 0.0;
  }

  /* Get initial coordinates prev_x and prev_y for the startsys system */
  if (0 != param->input_is_reg_file) {
    /* Find number of regions in the input SAORegion structure */
    number_of_shapes = SAORegion_input->nShapes;
  } else {
    number_of_shapes = 1;
  }

  bad_region = 0;  /* Default even for an input point */
  if (0 != param->input_is_reg_file) {
    if (0 != param->stopsys_is_radec || 0 != param->startsys_is_radec) {
      if (0 == SAORegion_input->wcs.exists) bad_region=1;  /* Require WCS data */
    }
    /* Make working copies of the initial region structure. */
    cloneSAORegion(SAORegion_input, &SAORegion_initial);
    cloneSAORegion(SAORegion_input, &SAORegion_intermediate);
  }
          
  /* Compute the number of conversion iterations within the
   * teldef file (excluding add-ons such as RADEC). */

  if (0 != conv_to_higher) {   /* RSH 2014-10-23 */
    n_iter = param->stopsysnum - param->startsysnum;
  } else {
    n_iter = param->startsysnum - param->stopsysnum;
  }
 
  /* Figure out if we need to convert a region to a pixel list
   * at the end. */

  if (0 == conv_to_higher && 0 != param->input_is_reg_file
    && e_TT_RAWTODET == teldef->trtypes[param->stopsysnum]
    && RM_CORNER_LIST == teldef->rawtodetparam[param->stopsysnum]->rawmethod) {
    make_pixel_list = 1;
  } else {
    make_pixel_list = 0;
  }

  if (0 != make_pixel_list) {
    
    *pixel_list = createPixelList();  /* Allocate empty pixel list */

    /* If a pixel list is needed, then the last conversion is meaningless
     * and needs to be skipped. */

    n_iter--;
  }

  /* Loop over the shapes; a single input point is treated as
   * a special case of a shape. */

  for (i=0; i<number_of_shapes; i++) {

    /* Examine the region shape type to determine what transformations are needed. */

    if (0 != param->input_is_reg_file) {
      shape = SAORegion_initial->Shapes[i].shape;
      num_trans_params = 0;
      for (j=0; j<MAX_TRANS; j++) {
        if (e_nullTrans != trans_type_table[shape][j]) {
          num_trans_params++;
        }
      }
      if (poly_rgn == shape) {
        if (SAORegion_initial->Shapes[i].param.poly.nPts > num_trans_params) {
          ahlog_err(__func__, "Too many points in polygon\n");
          return 1;
        }
        num_trans_params = SAORegion_initial->Shapes[i].param.poly.nPts;
      }
    } else {
      num_trans_params = 2;  /* Default = one point, two coords */
    }

    /* Loop over parameters for each region.  Ten is the maximum number of parameters,
     * but typically, many of these will be null.  If this is not a region, then
     * only one iteration of the loop. */

    for (j=0; j<num_trans_params; j++) {
      
      if (0 != param->input_is_reg_file) {
        shape = SAORegion_initial->Shapes[i].shape;
        trans_type = trans_type_table[shape][j];
      } else {
        shape = point_rgn;
        trans_type = e_pointTrans;  /* Default */
      }
      switch (trans_type) {
        case e_pointTrans:
          if (0 != param->input_is_reg_file) {
            if (poly_rgn == shape) {
              prev_x = SAORegion_initial->Shapes[i].param.poly.Pts[j];
              prev_y = SAORegion_initial->Shapes[i].param.poly.Pts[j+1];
            } else {
              prev_x = SAORegion_initial->Shapes[i].param.gen.p[j];
              prev_y = SAORegion_initial->Shapes[i].param.gen.p[j+1];
            }
          } else {
            prev_x = param->in_x;
            prev_y = param->in_y;
          }
          ahlog_info(LOW, __func__, "POINT:  Starting X,Y: %.8g, %.8g\n", prev_x, prev_y);
          x = prev_x;  /* In case n_iter == 0 */
          y = prev_y;

          /* Loop through coordinate transformations. */

          ahlog_info(LOW, __func__, "startsysnum, stopsysnum: %d, %d\n", param->startsysnum, param->stopsysnum);

          /* If the starting system is TELPOL or TELXY, a preliminary transformation
           * from TEL to OPTCOORD is required. */

          if (0 != conv_to_higher_tel) {
            if (param->startsys_is_telpol) {
              stepstatus = convertCoordPolar(conv_to_higher_tel,
                &prev_x, &prev_y, &x, &y, teldef);
              ahlog_info(HIGH, __func__, "Starting system is TELPOL.\n");
            }
            if (param->startsys_is_telxy) {
              stepstatus = convertCoordCartesian(conv_to_higher_tel, optcoord_sysnum, 
                &prev_x, &prev_y, &x, &y, teldef);
              ahlog_info(HIGH, __func__, "Starting system is TELXY.\n");
            }
            prev_x = x;
            prev_y = y;
            ahlog_info(LOW, __func__, "Previous x=%.8g, y=%.8g\n");
          }

          /* printTelDef2(teldef, stdout); */
          for (isys=0; isys<n_iter; isys++) {

            if (0 != conv_to_higher) {
              /* isys == 0 --->        sys == param->startsysnum
               * isys == n_iter-1 ---> sys == teldef->n_coordsys - 2 
               * sys is the source system */
              sys = isys + param->startsysnum;
            } else {
              /* isys == 0        ---> sys == param->startsysnum-1
               * isys == n_iter-1 ---> sys == 0
               * sys is the destination system */
              sys = param->startsysnum - 1 - isys;
            }

            /* If transformation sys->(sys+1) is in the range of
             * converstions, then do the conversion. */

            convertstatus = 0;  /* Track conversion status for one transformation; 0 means OK */

            ahlog_info(LOW, __func__, "conv_to_higher=%d, isys=%d\n", 
              conv_to_higher, isys);
            ahlog_info(LOW, __func__, "sys: %d  seg: %d\n", sys, teldef->min_segment[sys]);

            /* Check for validity of prev_x, prev_y: that they are within
             * the coordinate ranges specified by the teldef file.  If the previous
             * coordinates were null, then the next coordinates cannot be
             * calculated, so set them to null.  Also, set coordinates to null
             * if this sytem is after the last (stopsys) system. */

            if (
              DOUBLENULLVALUE == prev_x || DOUBLENULLVALUE == prev_y ||
              1 == bad_region) {
                ahlog_info(LOW, __func__, "conversion failure\n");
                x = DOUBLENULLVALUE;
                y = DOUBLENULLVALUE;
                convertstatus = 1;
            }

            /* Check to see if destination system is RADEC (WCS) and/or SKY. */

            if (0 != param->input_is_pixel) {
              t_seg = param->in_pixel;
            } else {
              t_seg = segs[sys];
            }
            
            if (0 == convertstatus) {

              stepstatus = 0;   /* Track status inside this if-block; 0 = OK */

              /* enum of transformation types is in coordfits2.h. */

              if (
                e_TT_RAWTODET == teldef->trtypes[sys] &&
                RM_CORNER_LIST == teldef->rawtodetparam[sys]->rawmethod) {

                if (0 != conv_to_higher) {
                  stepstatus = convertToHigherCoordRawtodet(teldef, prev_x, prev_y, &x, &y, sys, t_seg);
                  ahlog_info(LOW, __func__, "convert to higher (RAWTODET using corner list):\n"); 
                  ahlog_info(LOW, __func__, "prev_x=%.8g, prev_y=%.8g, x=%.8g, y=%.8g, sys=%d, t_seg=%d\n",
                    prev_x, prev_y, x, y, sys, t_seg);
                } else {
                  stepstatus = convertToLowerCoordRawToPixel(  /* Not in coordevt */
                    teldef, &x, &y, prev_x, prev_y, sys);
                  ahlog_info(LOW, __func__, "convert to lower (RAWTODET using corner list):\n"); 
                  ahlog_info(LOW, __func__, "prev_x=%.8g, prev_y=%.8g, x=%.8g, y=%.8g, sys=%d\n",
                    prev_x, prev_y, x, y, sys);
                }

              } else if (
                e_TT_RAWTODET == teldef->trtypes[sys] &&
                RM_LINEAR_COEFF == teldef->rawtodetparam[sys]->rawmethod) {

                if (0 != conv_to_higher) {
                  stepstatus = convertToHigherCoordRawtodet(
                    teldef, prev_x, prev_y, &x, &y, sys, t_seg);
                  ahlog_info(LOW, __func__, "convert to higher (RAWTODET using linear coeff):\n");
                } else {
                  stepstatus = convertToLowerCoordRawtodet(
                    teldef, &x, &y, prev_x, prev_y, sys, t_seg);
                  ahlog_info(LOW, __func__, "convert to lower (RAWTODET using linear coeff):\n");
                }
                ahlog_info(LOW, __func__, "prev_x=%.8g, prev_y=%.8g, x=%.8g, y=%.8g, sys=%d, t_seg=%d\n",
                  prev_x, prev_y, x, y, sys, t_seg);

              } else if (
                e_TT_BASIC == teldef->trtypes[sys]) {

                if (0 !=conv_to_higher) {
                  stepstatus = convertToHigherCoordBasic(
                    teldef, prev_x, prev_y, &x, &y, sys);
                  ahlog_info(LOW, __func__, "convert to higher (BASIC):\n");
                } else {
                  stepstatus = convertToLowerCoordBasic(
                    teldef, &x, &y, prev_x, prev_y, sys);
                  ahlog_info(LOW, __func__, "convert to lower (BASIC):\n");
                }
                ahlog_info(LOW, __func__, "prev_x=%.8g, prev_y=%.8g, x=%.8g, y=%.8g, sys=%d\n",
                  prev_x, prev_y, x, y, sys);

              } else if (
                e_TT_MULTISEG == teldef->trtypes[sys]) {

                rowprops = (int *)calloc(
                  teldef->multisegparam[sys]->n_properties, sizeof(int));

                ahlog_info(LOW, __func__, "multiseg properties for selected row:\n");
                for (prop=0; prop<teldef->multisegparam[sys]->n_properties; prop++) {
                  rowprops[prop] = props[sys][prop];
                  ahlog_info(LOW, __func__, "property[%d]=%d\n",prop, rowprops[prop]);
                }

                if (0 !=conv_to_higher) {
                  stepstatus = convertToHigherCoordMultiseg(
                    teldef, prev_x, prev_y, &x, &y, sys, rowprops,
                    param->winoffsetx, param->winoffsety);
                  ahlog_info(LOW, __func__, "convert to higher (MULTISEG):\n");
                } else {
                  stepstatus = convertToLowerCoordMultiseg(
                    teldef, &x, &y, prev_x, prev_y, sys, rowprops,
                    param->winoffsetx, param->winoffsety);
                  ahlog_info(LOW, __func__, "convert to lower (MULTISEG):\n");
                }

                ahlog_info(LOW, __func__, "prev_x=%.8g, prev_y=%.8g, x=%.8g, y=%.8g, sys=%d\n",
                  prev_x, prev_y, x, y, sys);

                free(rowprops);

              } else if (
                e_TT_SKYATT == teldef->trtypes[sys]) {

                /* For the SKYATT transformation, we use the unity quaternion
                 * with zero velocity (no attitude or aberration).
                 * The SKYATT transformation is a two-step transformation if
                 * the user-specified type is RADEC. */

                if (0 !=conv_to_higher) {
                  /* SKYATT transformations can occur before the final step (it just means
                   * there is a matrix involved).  If this transformation is one of those,
                   * ignore the attitude. */
                  if (sys == teldef->n_coordsys-2) {
                    copyQuat(&q1, q);
                  } else {
                    setQuatToIdentity(&q1);
                  }
                  stepstatus = convertToHigherCoordSkyAtt(
                    teldef, prev_x, prev_y, &x, &y, sys,
                    &q1, v_sat_total, vhat_sat_total);
                  ahlog_info(LOW, __func__, "convert to higher (SKYATT):\n");
                  ahlog_info(LOW, __func__, "q1->p[0]=%.8g\n", q1.p[0]);
                  ahlog_info(LOW, __func__, "q1->p[1]=%.8g\n", q1.p[1]);
                  ahlog_info(LOW, __func__, "q1->p[2]=%.8g\n", q1.p[2]);
                  ahlog_info(LOW, __func__, "q1->p[3]=%.8g\n", q1.p[3]);
                  ahlog_info(LOW, __func__, "prev_x=%.8g, prev_y=%.8g, x=%.8g, y=%.8g, sys=%d\n", 
                    prev_x, prev_y, x, y, sys);
                  if (sys == teldef->n_coordsys-2) {
                    if (0 == stepstatus && 0 != param->stopsys_is_radec) {
                      prev_x = x;
                      prev_y = y;
                      stepstatus = convertCoordRADEC(  /* Not in coordevt */
                        conv_to_higher, &prev_x, &prev_y, &x, &y, wcs);
                      ahlog_info(LOW, __func__, "convert to higher (RADEC):\n");
                      ahlog_info(LOW, __func__, "prev_x=%.8g, prev_y=%.8g, x=%.8g, y=%.8g\n", 
                        prev_x, prev_y, x, y);
                    }
                  }
                } else {
                  if (sys == teldef->n_coordsys-2) {
                    copyQuat(&q1, q);
                    if (0 != param->startsys_is_radec) {
                      /* A technicality of the underlying cfitsio library:  
                       * if the region file is fk5 type, then
                       * fits_read_ascii_region has already converted point coordinates
                       * to SKY. */
                      if (0 != param->input_is_reg_file) {
                        x = prev_x;
                        y = prev_y;
                        stepstatus = 0;
                        ahlog_info(LOW, __func__, "Automatic RADEC->SKY conversion.\n");
                      } else {
                        stepstatus = convertCoordRADEC(  /* Not in coordevt */
                          conv_to_higher, &x, &y, &prev_x, &prev_y, wcs);
                        ahlog_info(LOW, __func__, "convert to higher (RADEC):  sys=%d\n", sys);
                        ahlog_info(LOW, __func__, "prev_x=%.8g; prev_y=%.8g, x=%.8g, y=%.8g\n",
                          prev_x, prev_y, x, y);
                        prev_x = x;
                        prev_y = y;
                      }
                    }
                    if (0 == stepstatus) {
                      stepstatus = convertToLowerCoordSkyAtt(
                        teldef, &x, &y, prev_x, prev_y, sys,
                        &q1, v_sat_total, vhat_sat_total);
                      ahlog_info(LOW, __func__, "convert to lower (SKYATT):  sys=%d\n", sys);
                      ahlog_info(LOW, __func__, "q1->p[0]=%.8g\n", q1.p[0]);
                      ahlog_info(LOW, __func__, "q1->p[1]=%.8g\n", q1.p[1]);
                      ahlog_info(LOW, __func__, "q1->p[2]=%.8g\n", q1.p[2]);
                      ahlog_info(LOW, __func__, "q1->p[3]=%.8g\n", q1.p[3]);
                      ahlog_info(LOW, __func__, "prev_x=%.8g; prev_y=%.8g, x=%.8g, y=%.8g\n\n",
                        prev_x, prev_y, x, y);
                    }
                  } else {
                    setQuatToIdentity(&q1);
                    stepstatus = convertToLowerCoordSkyAtt(
                      teldef, &x, &y, prev_x, prev_y, sys,
                      &q1, v_sat_total, vhat_sat_total);
                    ahlog_info(LOW, __func__, "convert to lower (SKYATT):  sys=%d\n", sys);
                    ahlog_info(LOW, __func__, "q1->p[0]=%.8g\n", q1.p[0]);
                    ahlog_info(LOW, __func__, "q1->p[1]=%.8g\n", q1.p[1]);
                    ahlog_info(LOW, __func__, "q1->p[2]=%.8g\n", q1.p[2]);
                    ahlog_info(LOW, __func__, "q1->p[3]=%.8g\n", q1.p[3]);
                    ahlog_info(LOW, __func__, "prev_x=%.8g; prev_y=%.8g, x=%.8g, y=%.8g\n",
                      prev_x, prev_y, x, y);
                  }
                }
              } else {
                ahlog_err(__func__, "Invalid conversion type for point\n");
                return 1;

              }
              convertstatus = stepstatus;
            }
            ahlog_info(LOW, __func__, "Finished with current conversion: x=%.8g, y=%.8g\n", x, y);

            if (0 == convertstatus) {  /* Previous output is next input */
              prev_x = x;
              prev_y = y;
            }
          } /* End loop over systems */

          /* Special case of transformation SKY <--> RADEC. */

          if (param->startsysnum == teldef->n_coordsys-1 && param->stopsysnum == teldef->n_coordsys-1) {
            if (0 !=conv_to_higher) {
              stepstatus = convertCoordRADEC(  /* Not in coordevt */
                conv_to_higher, &prev_x, &prev_y, &x, &y, wcs);
              ahlog_info(LOW, __func__, "Convert to higher (RADEC); only one conversion:%d\n");
              ahlog_info(LOW, __func__, "prev_x=%.8g; prev_y=%.8g, x=%.8g, y=%.8g\n",
                prev_x, prev_y, x, y);
            } else {
              if (0 != param->input_is_reg_file) {
                /* Another occurrence of this technicality.  For an fk5 type input region
                 * file, fits_read_ascii_region has already converted the point. */
                x = prev_x;
                y = prev_y;
                stepstatus = 0;
                ahlog_info(LOW, __func__, "Automatic RADEC->SKY conversion; only one conversion.\n");
              } else {
                stepstatus = convertCoordRADEC(  /* Not in coordevt */
                  conv_to_higher, &x, &y, &prev_x, &prev_y, wcs);
                ahlog_info(LOW, __func__, "Convert to lower (RADEC); only one conversion:%d\n");
                ahlog_info(LOW, __func__, "prev_x=%.8g; prev_y=%.8g, x=%.8g, y=%.8g\n",
                  prev_x, prev_y, x, y);
              }
            }
          }

          /* Special case of transformation to/from telescope coordinates. */

          ahlog_info(LOW, __func__, "stopsys_is_telpol=%d\n", param->stopsys_is_telpol);
          ahlog_info(LOW, __func__, "stopsys_is_telxy =%d\n", param->stopsys_is_telxy );
          ahlog_info(LOW, __func__, "conv_to_higher_tel=%d\n", conv_to_higher_tel );

          if ( (param->stopsys_is_telpol || param->stopsys_is_telxy) && 0 == conv_to_higher_tel) {
            if (param->stopsys_is_telpol) {
              stepstatus = convertCoordPolar(conv_to_higher_tel,
                &x, &y, &prev_x, &prev_y, teldef);
              ahlog_info(LOW, __func__, "convert to TELPOL:\n");
              ahlog_info(LOW, __func__, "prev_x=%.8g; prev_y=%.8g, x=%.8g, y=%.8g\n",
                prev_x, prev_y, x, y);
            }
            if (param->stopsys_is_telxy) {
              stepstatus = convertCoordCartesian(conv_to_higher_tel, optcoord_sysnum, 
                &x, &y, &prev_x, &prev_y, teldef);
              ahlog_info(LOW, __func__, "convert to TELXY:\n");
              ahlog_info(LOW, __func__, "prev_x=%.8g; prev_y=%.8g, x=%.8g, y=%.8g\n",
                prev_x, prev_y, x, y);
            }
          }

          if (0 != param->input_is_reg_file) {
            if (poly_rgn == shape) {
              SAORegion_intermediate->Shapes[i].param.poly.Pts[j] = x;
              SAORegion_intermediate->Shapes[i].param.poly.Pts[j+1] = y;
            } else {
              SAORegion_intermediate->Shapes[i].param.gen.p[j] = x;
              SAORegion_intermediate->Shapes[i].param.gen.p[j+1] = y;
            }
            SAORegion_intermediate->wcs.xrefval = x;
            SAORegion_intermediate->wcs.yrefval = y;
          } else {
            param->outx = x;
            param->outy = y;
            param->outx_undef = 0;
            param->outy_undef = 0;
          }
          /* Increment the counter to skip the next parameter,
           * since it was part of a point pair. */

          j++;

          break;
        
        case e_lengthTrans:
          /* Length parameters are transformed by the ratio of the scales of the
           * lower and upper systems.  This scale is in px_scale_factor in TELDEF2
           * and is in units of the scale of the bottom-level coordinate system. */

          length_start = SAORegion_initial->Shapes[i].param.gen.p[j];
          length_finish = length_start;  /* In case n_iter == 0 */

          ahlog_info(LOW, __func__, "Length transformation:  length_start=%.8g   n_iter=%d\n",
            length_start, n_iter);

          /* Preliminary conversion if starting system is TELXY (TELPOL not supported for length). */

          if (0 != conv_to_higher_tel && 0 != param->startsys_is_telxy) {
            stepstatus = convertRegionLengthTelescope (conv_to_higher_tel, teldef,
              length_start, &length_finish);
            length_start = length_finish;
          }

          for (isys=0; isys<n_iter; isys++) {  /* RSH 2014-10-23 */


            if (0 !=conv_to_higher) {
              /* isys == 0 --->        sys == param->startsysnum
               * isys == n_iter-1 ---> sys == teldef->n_coordsys - 2 
               * sys is the source system */
              sys = isys + param->startsysnum;
            } else {
              /* isys == 0        ---> sys == param->startsysnum-1
               * isys == n_iter-1 ---> sys == 0
               * sys is the destination system */
              sys = param->startsysnum - 1 - isys;
            }

            convertstatus = 0;  /* 0 means OK */

            ahlog_info(LOW, __func__, "Length transformation:  conv_to_higher=%d, isys=%d, sys=%d\n",
              conv_to_higher, isys, sys);

            if (DOUBLENULLVALUE == length_start) {
              length_finish = DOUBLENULLVALUE;
              convertstatus = 1;
              ahlog_info(LOW, __func__, "Starting length is null.\n");
            }

            if (0 == convertstatus) {

              stepstatus = 0;

              if (0 == isys && 0 != param->startsys_is_radec) {
                stepstatus = convertRegionLengthRADec(conv_to_higher,
                  length_start, &length_finish, wcs);
                ahlog_info(LOW, __func__, "Convert length to lower (RADEC): \n");
                ahlog_info(LOW, __func__, "conv_to_higher=%d, length_start=%.8g, length_finish=%.8g\n", conv_to_higher, length_start, length_finish);
                if (0 == stepstatus) {
                  length_start = length_finish;  /* Output becomes next input */
                }
              }

              if (0 == stepstatus) {
                stepstatus = convertRegionLength(conv_to_higher,
                  teldef, length_start, &length_finish, sys);
                ahlog_info(LOW, __func__, "Convert length: \n");
                ahlog_info(LOW, __func__, "conv_to_higher=%d, length_start=%.8g, length_finish=%.8g, sys=%d\n", conv_to_higher, length_start, length_finish, sys);
              }

              if (0 == stepstatus) {
                if (isys == n_iter-1 && 0 != param->stopsys_is_radec) {
                  length_start = length_finish;
                  stepstatus = convertRegionLengthRADec(conv_to_higher,
                    length_start, &length_finish, wcs);
                ahlog_info(LOW, __func__, "Convert length to higher (RADEC): \n");
                ahlog_info(LOW, __func__, "conv_to_higher=%d, length_start=%.8g, length_finish=%.8g\n", conv_to_higher, length_start, length_finish);
                }
              }

              if (0 == stepstatus) {
                length_start = length_finish;  /* Output becomes next input */
              } else {
                length_start = DOUBLENULLVALUE;
              }

            }

          } /* End loop over systems */

          if (param->startsysnum == teldef->n_coordsys-1 && param->stopsysnum == teldef->n_coordsys-1) {
            stepstatus = convertRegionLengthRADec(conv_to_higher,
              length_start, &length_finish, wcs);
            ahlog_info(LOW, __func__, "Convert length to higher (RADEC), only conversion: \n");
            ahlog_info(LOW, __func__, "conv_to_higher=%d, length_start=%.8g, length_finish=%.8g\n", conv_to_higher, length_start, length_finish);
          }

          /* Additional conversion for stopsys == TELXY (TELPOL not supported for length). */

          if (0 != param->stopsys_is_telxy && 0 == conv_to_higher_tel){
            stepstatus = convertRegionLengthTelescope(conv_to_higher_tel, teldef, length_start, &length_finish);
            ahlog_info(LOW, __func__, "Convert length to TELXY: \n");
            ahlog_info(LOW, __func__, "length_start=%.8g, length_finish=%.8g\n", length_start, length_finish);
          }

          SAORegion_intermediate->Shapes[i].param.gen.p[j] = length_finish;
          break;

        case e_angleTrans:

          /* Angle parameters are transformed by the rotation angle between the
           * lower and upper systems, which is stored as rot in the XFORM2D structure. */

          angle_start = SAORegion_initial->Shapes[i].param.gen.p[j];
          angle_finish = angle_start;  /* In case n_iter == 0 */

          /* Preliminary conversion for case where startsys == TELXY (TELPOL not supported for angle. */

          if (0 != conv_to_higher_tel && 0 != param->startsys_is_telxy) {
            stepstatus = convertRegionAngleTelescope (conv_to_higher_tel, teldef, angle_start, sys, &angle_finish);
            ahlog_info(LOW, __func__, "Convert angle from TELXY:\n");
            ahlog_info(LOW, __func__, "angle_start=%.8g, angle_finish=%.8g, sys=%d\n", angle_start, angle_finish, sys);
            angle_start = angle_finish;
          }

          for (isys=0; isys<n_iter; isys++) {  /* RSH 2014-10-23 */

            if (0 !=conv_to_higher) {
              /* isys == 0 --->        sys == param->startsysnum
               * isys == n_iter-1 ---> sys == teldef->n_coordsys - 2 
               * sys is the source system */
              sys = isys + param->startsysnum;
            } else {
              /* isys == 0        ---> sys == param->startsysnum-1
               * isys == n_iter-1 ---> sys == 0
               * sys is the destination system */
              sys = param->startsysnum - 1 - isys;
            }

            ahlog_info(LOW, __func__, "Converting angle: conv_to_higher=%d, isys=%d, sys=%d\n", conv_to_higher, isys, sys);

            convertstatus = 0;  /* 0 means OK */

            if (DOUBLENULLVALUE == angle_start) {
              angle_finish = DOUBLENULLVALUE;
              convertstatus = 1;
            }

            if (0 == convertstatus) {

              stepstatus = 0;

              if (0 == isys && 0 != param->startsys_is_radec) {
                stepstatus = convertRegionAngleRADec(conv_to_higher,
                  angle_start, &angle_finish, sys, wcs);
                ahlog_info(LOW, __func__, "Convert angle to lower (RADEC):\n");
                ahlog_info(LOW, __func__, "conv_to_higher=%d, angle_start=%.8g, angle_finish=%.8g, sys=%d\n",  
                  conv_to_higher, angle_start, angle_finish, sys);
                if (0 == stepstatus) {
                  angle_start = angle_finish;
                }
              }

              if (0 == stepstatus) {
                /* enum of transformation types is in coordfits2.h. */
                if (e_TT_MULTISEG == teldef->trtypes[sys]) {
                  stepstatus = convertRegionAngleMultiseg(
                    conv_to_higher, teldef, angle_start, &angle_finish, sys, msegs[sys]);
                  ahlog_info(LOW, __func__, "Convert angle (MULTISEG):\n");
                  ahlog_info(LOW, __func__, "conv_to_higher=%d, angle_start=%.8g, angle_finish=%.8g, sys=%d\n",  
                    conv_to_higher, angle_start, angle_finish, sys);
                } else if (e_TT_RAWTODET == teldef->trtypes[sys]) {
                    if (segs[sys] >= 0) {
                      stepstatus = convertRegionAngleMultiseg(
                        conv_to_higher, teldef, angle_start, &angle_finish, sys, segs[sys]);
                    } else {
                      stepstatus = convertRegionAngle(
                        conv_to_higher, teldef, angle_start, &angle_finish, sys);
                    }
                } else if (e_TT_SKYATT == teldef->trtypes[sys]) {
                  /* SKYATT transformations can occur before the final step (it just means
                   * there is a matrix involved).  If this transformation is one of those,
                   * ignore the attitude. */
                  if (sys == teldef->n_coordsys-2) {
                    copyQuat(&q1, q);
                  } else {
                    setQuatToIdentity(&q1);
                  }
                  stepstatus = convertRegionAngleSkyAtt(
                    conv_to_higher, teldef, angle_start, &angle_finish, sys, &q1);
                    ahlog_info(LOW, __func__, "Converting angle (SKYATT):  conv_to_higher=%d, sys=%d\n", 
                      conv_to_higher, sys);
                    ahlog_info(LOW, __func__, "q1->p[0]=%.8g\n", q1.p[0]);
                    ahlog_info(LOW, __func__, "q1->p[1]=%.8g\n", q1.p[1]);
                    ahlog_info(LOW, __func__, "q1->p[2]=%.8g\n", q1.p[2]);
                    ahlog_info(LOW, __func__, "q1->p[3]=%.8g\n", q1.p[3]);
                    ahlog_info(LOW, __func__, "angle_start=%.8g, angle_finish=%.8g\n",
                      angle_start, angle_finish);
                } else {
                  stepstatus = convertRegionAngle(
                  conv_to_higher, teldef, angle_start, &angle_finish, sys);
                  ahlog_info(LOW, __func__, "Converting angle:  conv_to_higher=%d, sys=%d\n", 
                    conv_to_higher, sys);
                  ahlog_info(LOW, __func__, "angle_start=%.8g, angle_finish=%.8g\n",
                    angle_start, angle_finish);
                }
              }

              if (0 == stepstatus) {
                if (isys == n_iter-1 && 0 != param->stopsys_is_radec) {
                  angle_start = angle_finish;
                  stepstatus = convertRegionAngleRADec(conv_to_higher,
                    angle_start, &angle_finish, sys, wcs);
                  ahlog_info(LOW, __func__, "Converting angle to higher (RADEC):\n");
                  ahlog_info(LOW, __func__, "angle_start=%.8g, angle_finish=%.8g, sys=%d\n",
                    angle_start, angle_finish, sys);
                }
              }

              if (0 == stepstatus) {
                angle_start = angle_finish;  /* Output becomes next input */
              } else {
                angle_start = DOUBLENULLVALUE;
              }
            }
          } /* End loop over systems */

          if (param->startsysnum == teldef->n_coordsys-1 && param->stopsysnum == teldef->n_coordsys-1) {
            stepstatus = convertRegionAngleRADec(conv_to_higher,
              angle_start, &angle_finish, sys, wcs);
            ahlog_info(LOW, __func__, "Converting angle to higher (RADEC) only conversion:\n");
            ahlog_info(LOW, __func__, "angle_start=%.8g, angle_finish=%.8g, sys=%d\n",
              angle_start, angle_finish, sys);
          }

          /* Additional conversion for case where stopsys == TELXY (TELPOL not supported for angle. */

          if (0 != param->stopsys_is_telxy && 0 == conv_to_higher_tel) {
            stepstatus = convertRegionAngleTelescope(conv_to_higher_tel, teldef, angle_start, sys, &angle_finish);
            ahlog_info(LOW, __func__, "Converting angle to TELXY:\n");
            ahlog_info(LOW, __func__, "angle_start=%.8g, angle_finish=%.8g, sys=%d\n",
              angle_start, angle_finish, sys);
          }

          SAORegion_intermediate->Shapes[i].param.gen.p[j] = angle_finish;
          break;

        case e_numberTrans:

          /* This one is easy since a number doesn't need to be transformed. */

          SAORegion_intermediate->Shapes[i].param.gen.p[j]
            = SAORegion_initial->Shapes[i].param.gen.p[j];

          ahlog_info(LOW, __func__, "Transforming number (stays the same):  %.8g\n",
            SAORegion_intermediate->Shapes[i].param.gen.p[j]);
            
          break;

        default:

          ahlog_err(__func__, "Invalid transformation type for region parameter\n");
          return 1;

      } /* End switch for transformation types */

    } /* End loop over parameters */

    /* cfitsio needs derived parameters for each shape in order to filter
     * individual points; the following call sets those parameters in the region
     * structure. */

    if (0 != param->input_is_reg_file) {
      fits_setup_shape(&SAORegion_intermediate->Shapes[i]);
    }

  } /* End loop over shapes  */

  if (0 != param->input_is_reg_file) {

    /* Process pixel lists. */
    if (0 != make_pixel_list) {
      ahlog_info(LOW, __func__, "Finding pixels in a region:  param->stopsysnum=%d\n", param->stopsysnum);
      findPixelsWithinRegion(SAORegion_intermediate, param->stopsysnum, teldef, param->pixeltest, &pixel_set);
      appendPixelSetToPixelList(*pixel_list, pixel_set);
      ahlog_info(LOW, __func__, "Generated pixel list for region:\n");
      for (k=0; k<pixel_set->n_pixels; k++) {
        ahlog_info(LOW, __func__, "list member %d=%d\n", k, pixel_set->pixel_nums[k]);
      }
    }

    /* Copy the working region structure into the output region structure. */
    cloneSAORegion(SAORegion_intermediate, SAORegion_output);
    /* Free working copies. */
    if (0 != SAORegion_initial) fits_free_region(SAORegion_initial);
    if (0 != SAORegion_intermediate) fits_free_region(SAORegion_intermediate);
  }

  return 0;

} /* End of do_work */

/* -------------------------------------------------------------------------- */

int finalize(Param* param, TransEnum** trans_type_table, TELDEF2* teldef,
  long** props, long* msegs, long* segs,
  QUAT* q, WCSdata* wcs, SAORegion* SAORegion_input, 
  SAORegion* SAORegion_output, PixelList* pixel_list) {

  int i=0;  /* Loop index */
  int ape_status_x = 0;   /* Status of parameter setting  */
  int ape_status_y = 0;   /* Status of parameter setting  */

  if (0 != param->input_is_reg_file) {
    if (0 != SAORegion_output) {
      if(0 != writeSAORegion(trans_type_table, SAORegion_output, param->stopsys_is_radec, pixel_list,
        param->outfile, param->clobber)) {
        ahlog_err(__func__, "Call to writeSAORegion failed\n");
        return 1;
      }
    }
    if (0 != SAORegion_input) fits_free_region(SAORegion_input);
    if (0 != SAORegion_output) fits_free_region(SAORegion_output);
  } else {
    if (0 != param->outx_undef) {
      ape_status_x = ape_trad_set_string("outx", "INDEF");
    } else {
      ape_status_x = ape_trad_set_double("outx", param->outx);
    }
    if (eOK != ape_status_x) {
      ahlog_err(__func__, "Could not set outx parameter\n");
    }
    if (0 != param->outy_undef) {
      ape_status_y = ape_trad_set_string("outy", "INDEF");
    } else {
      ape_status_y = ape_trad_set_double("outy", param->outy);
    }
    if (eOK != ape_status_y) {
      ahlog_err(__func__, "Could not set outy parameter\n");
    }
    if (0 == param->outx_undef && 0 == param->outy_undef) {
      ahlog_out(__func__, "OUTX OUTY=%14.8f %14.8f\n", param->outx, param->outy);
    }
  }

  if (0 != trans_type_table) {
    for (i=0; i<MAX_SHAPES; i++) {
      if (0 != trans_type_table) free(trans_type_table[i]);
    }
    free(trans_type_table);
  }

  if (0 != props) {
    for (i=0; i<teldef->n_coordsys; i++) {
      free(props[i]);
    }
    free(props);
  }

  if (0 != msegs) {
    free(msegs);
  }

  if (0 != segs) {
    free(segs);
  }

  if (0 != pixel_list) {
    destroyPixelList(pixel_list);
  }

  destroyTelDef2(teldef);
  if (0 != wcs) free(wcs);
  destroyQuat(q);
  destroyParam(param);

  return 0;
}

/* -------------------------------------------------------------------------- */

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
/* -------------------------------------------------------------------------- */

/* Revision Log
 $Log: coordpnt.c,v $
 Revision 1.22  2016/04/07 20:57:13  rshill
 Parameter dump to log changed to command line format.

 Revision 1.21  2016/03/22 19:59:16  rshill
 Added writeParametersToLog.

 Revision 1.20  2016/03/21 20:59:31  rshill
 Deleted a stray inserted character.

 Revision 1.19  2016/03/21 20:58:25  rshill
 Fixed flow of control for pixel lists.  Output pixel list
 is made using entire region file, not one shape at a time (which was incorrectly implemented
 in any case).

 Revision 1.17  2016/01/13 03:55:04  klrutkow
 added new param pixeltest to findPixelsWithinRegion

 Revision 1.16  2015/12/21 22:46:43  rshill
 Deleted roll angle range check.

 Revision 1.15  2015/10/23 00:37:17  rshill
 Correct bug whereby multiseg transform for angle in
 segmented RAWTODET was being ignored.

 Revision 1.14  2015/08/21 17:11:16  rshill
 Removed an unneede log output call.

 Revision 1.13  2015/08/21 15:15:05  rshill
 Changed the message output numerical precision to 8 digits from 15; repaired mismatches between data items and formats in I/O statements.

 Revision 1.12  2015/08/19 21:46:04  rshill
 Comment prologue cleanup.

 Revision 1.11  2015/08/04 03:27:38  rshill
 Corrected typos and trivial errors.

 Revision 1.10  2015/08/04 03:22:04  rshill
 Finished coordpnt cleanup.

 Revision 1.9  2015/08/03 23:39:16  rshill
 Partway through code cleanup; more log statements remain to be added.

 Revision 1.8  2015/07/16 15:52:08  rshill
 Fixed some debug output; deleted redundant checks of stopsys_is_telxy.

 Revision 1.7  2015/07/08 17:01:10  rshill
 Tweaked a comment.

 Revision 1.6  2015/06/26 17:58:29  rshill
 Implemented resolution of TelDef filename via CALDB.

 Revision 1.5  2015/06/15 21:22:34  rshill
 Debugged the simple cases of TELXY or TELPOL to and from OPTCOORD.

 Revision 1.4  2015/06/11 22:42:38  rshill
 Corrected output same as input system test.

 Revision 1.3  2015/06/11 22:19:06  rshill
 Fixed mistake in validating TELXY coordinate name;
 fixed mistake in determining whether input and output systems the same.

 Revision 1.2  2015/06/10 00:10:28  rshill
 Added telescope coordinate TELPOL and TELXY.

 Revision 1.1  2015/05/22 19:58:36  rshill
 Converted to straight C.  Split into main and lib files.  Moved from astroh/gen/tasks.


 Previous log messages for C++ version:

 Revision 1.27  2015/01/16 22:06:41  rshill
 Fixed bugs in treatment of pixel list regions (not
 standard).

 Revision 1.26  2015/01/07 02:56:37  mwitthoe
 coordpnt: fix type in parameter description; remove non-standard quote characters in par file

 Revision 1.25  2015/01/06 13:55:13  mdutka
 Updated parameter list see Issue #472

 Revision 1.24  2014/12/10 01:28:16  rshill
 Corrected three string in region file output.  Corrected one loop limit.

 Revision 1.23  2014/12/05 02:23:39  rshill
 Fixed bugs in HAK bug list.  Mostly related
 to improper reinitialization of variables between conversion steps.
 Fixed sense of angle for forward vs. backward rotations.
 Corrected clobber processing.  Corrected format of pure numbers
 in region specification (should be int).

 Revision 1.22  2014/12/03 00:05:02  rshill
 Corrected bug in handling of fk5 (RADEC) input region files.
 Attempted correction of sign in tranforming angles.
 Corrected bug in clobber implementation.

 Revision 1.21  2014/11/21 18:47:16  rshill
 Commented out printTeldef2.

 Revision 1.20  2014/11/21 02:20:43  rshill
 Forced integer output of pure scalars in region files;
 implemented pixel lists as region file output.

 Revision 1.19  2014/11/20 01:38:55  rshill
 Added clobber handling for region files;
 put additional notations into region files (fk5, physical, short comments)

 Revision 1.18  2014/11/13 16:09:50  rshill
 Fixed several bugs related to angle transformations,
 as well as to RADEC coordinates.

 Revision 1.17  2014/11/07 22:21:29  rshill
 Fixed bugs in pixel processing, in scaling, and in handling default pointing.

 Revision 1.16  2014/10/30 23:28:12  rshill
 Region files working in rudimentary way (include region spec only).
 RA_NOM, DEC_NOM, and ROLL_NOM parameters added.

 Revision 1.15  2014/10/29 01:29:12  rshill
 Fixed loop limits in CopySAORegion.

 Revision 1.14  2014/10/29 00:45:09  rshill
 Got RADEC working on input and output; RADEC may be lowercase on command line.

 Revision 1.13  2014/10/24 16:44:30  rshill
 Reconciled with TRF.

 Revision 1.12  2014/10/24 01:21:05  rshill
 Partial bug fixes.

 Revision 1.11  2014/10/23 17:23:48  rshill
 Attempted fixes to problems reported by H. Krimm.

 Revision 1.10  2014/08/11 18:28:00  rshill
 Many bugfixes and cleanups: see +++ comments.  Two different
 kinds of segment (RAWTODET and MULTISEG).

 Revision 1.9  2014/08/05 19:48:40  mwitthoe
 coordpnt: partially debugged; passed from MCW to RSH

 Revision 1.8  2014/07/30 19:15:25  rshill
 Added storing output params outx and outy, and writing
 output region file.

 Revision 1.7  2014/07/30 16:29:39  rshill
 Fixed linkage problems by adding extern "C".

 Revision 1.6  2014/07/30 14:29:00  rshill
 Argument lists rationalized; handling of region structure revised.

 Revision 1.5  2014/07/25 02:10:13  rshill
 Have begun trying to compile and check,
 but does not compile yet.

 Revision 1.4  2014/07/24 23:52:20  rshill
 Code all entered from TRF, but still in a rough state.

 Revision 1.3  2014/07/24 01:38:39  rshill
 Another check-in for backup.

 Revision 1.2  2014/07/23 23:15:52  rshill
 Another check-in of incomplete code for backup.

 Revision 1.1  2014/07/23 00:54:30  rshill
 Initial checkin for backup, code very incomplete.

*/
