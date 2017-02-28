/** \file aberattitude.c
 *  \brief Correct attitude file for aberration of light
 *  \author Robert S. Hill
 *  \date $Date: 2016/06/13 15:37:46 $
 */

/**

  \defgroup tool_aberattitude Correct attitude file for aberration of light (aberattitude)
  \ingroup mod_gen_tasks

This task applies an aberration correction to an attitude file.  The input attitudes
describe pointing toward a target position that is shifted due to the finite speed
of light ("aberration of starlight").  The output attitudes describe pointing toward
the catalog position of the target in the standard astrometric reference frame.

Source files:

  aberattitude.c

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

#include "aber.h"
#include "coordfits2.h"

#include "ahlog/cahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "headas_utils.h"

#include "heaapp/heaapp.h"
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"

#include "fitsio.h"
#include <math.h>
#include <string.h>
#include <sys/stat.h>

#define MAX_PARAM 30  

/* Type of attitude column in the attitude file. */

typedef enum {
  e_attUnknown,    /* Unknown */
  e_attQuat,       /* Quaternion */
  e_attEuler,      /* Euler angles */
  e_attPointing    /* RA, Dec, roll */
} AttColType;

typedef struct {
  
  /* User parameters, filled in getPar(). */

  char* infile;  /* Input attitude file */
  char  outfile[FLEN_FILENAME];  /* Output attitude file */
  char* orbfile;  /* Input orbit file */
  char* alignfile;  /* Input alignment file */
  char* annaber;  /* Flag to do annual aberration */
  char* orbaber;  /* Flag to do orbital aberration */
  char* attext;  /* Attitude extension name */
  char* attcol;  /* Attitude column name */
  char* attform;  /* Attitude column format */
  char* orbext;  /* Orbit extension name */
  char* orbcol;  /* Orbit column name */
  char* orbform;  /* Orbit column format */
  int chatter;  /* Standard parameter: chatter level */
  char clobber;  /* Standard parameter: output file overwrite flag */
  char debug;  /* Standard parameter: debug mode */
  char write_history;  /* Standard parameter: FITS file history flag  */

  /* For clobber processing. */
  char* outfile_as_input;
  int outfile_bang;

  /* Derived values, filled in initialize(). */

  AttColType attcoltype;  /* enum defined above: attitude column format */
  int attcolnum;  /* Attitude column number */
  int timecolnum;  /* TIME column number */
  int do_annaber;  /* Flag to compute annual aberration */
  int do_orbaber;  /* Flag to compute orbital abrration */
  int inv_annaber;  /* Flag to invert annual aberration */
  int inv_orbaber;  /* Flag to invert orbital abrration */

} Param;

/* Structure to hold information related to coordevt-like
 * file processing. */

typedef struct {
  fitsfile* fp;  /* Output file pointer */
  char* tempfile;  /* Output file temporary name */
  int tempfile_copied;  /* Success indicator for cloning input file */
  int tempfile_written;  /* Success indicator for writing data to output file */
} Tempfile;

/* -------------------------------------------------------------------------- */
/* Top-level functions in standard main */

/* Function declarations */

/** \brief Get parameter values
 *  \param[out] param parameter structure
 */ 
int getPar(Param** param);

/** \brief Set up tool operation
 *  \param[in] param parameter structure
 *  \param[out] tf temporary output file descriptor
 *  \param[out] orb orbit file descriptor
 *  \param[out] align instrument alignment
 *  \param[out] mjdref reference time (MJD) from attitude file
 *  \param[out] nrow_gti number of good time intervals (GTI)
 *  \param[out] tstart starting times of GTI
 *  \param[out] tstop stopping times of GTI
 */
int initialize(Param* param, Tempfile** tf, GENORBFILE** orb,
  ALIGN** align, double* mjdref);

/** \brief Do the actual work of the tool
 *  \param[in] param parameter structure
 *  \param[inout] tf temporary output file descriptor
 *  \param[in] orb orbit file descriptor
 *  \param[in] mjdref reference time (MJD) from attitude file
 *  \param[in] align instrument alignment
 *  \param[in] nrow_gti number of good time intervals (GTI)
 *  \param[in] tstart starting times of GTI
 *  \param[in] tstop stopping times of GTI
 */
int doWork(Param* param, Tempfile* tf, GENORBFILE* orb, 
  ALIGN* align, double mjdref);

/** \brief Close files and deallocate structures and arrays
 *  \param[in] param parameter structure
 *  \param[in] tf temporary output file descriptor
 *  \param[in] orb orbit file descriptor
 *  \param[in] align instrument alignment
 *  \param[in] tstart starting times of GTI
 *  \param[in] tstop stopping times of GTI
 */
int finalize(Param* param, Tempfile* tf, GENORBFILE* orb, 
  ALIGN* align);

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

/** \brief Test whether two filenames refer to the same file
 *  \param[in] filename1 a filename
 *  \param[in] filename2 another filename
 */
int areFilenamesSameFile (const char* filename1, const char* filename2);

/** \brief Allocate and initialize param structure
 * \return pointer to param structure
 */
Param* createParam ();

/** \brief Deallocate param structure
 * \param[in] param parameter structure
 */
void destroyParam (Param* param);

/** \brief Allocate and initialize structure to manage temporary output file
 * \return pointer to tempfile structure
 */
Tempfile* createTempfile ();

/** \brief Deallocate structure to manage temporary output file
 * \param[in] tf tempfile structure
 */
void destroyTempfile (Tempfile* tf);

/******************************************************************************/

/* \brief Execute the aberattitude tool. */
int main (
  int argc, /* Standard number of input arguments */
  char** argv /* Standard array of input arguments */
) {

  /* Declare the file, param struct, and info struct pointers and the
     CFITSIO and runtime statuses. */

  GENORBFILE* orb=0;
  ALIGN* align=0;
  double mjdref=0.0;

  int status = 0;         /* Initialization & execution status (0: normal) */
  int finalStatus = 0;    /* Status after shutdown (0: normal) */

  Param* param=0;
  Tempfile* tf=0;

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
      status = initialize(param, &tf, &orb, &align, &mjdref);
      if (0 != status) {
        ahlog_err(__func__, "initialize returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    }
    
    if (0 == status) {
      status = doWork(param, tf, orb, align, mjdref);
      if (0 != status) {
        ahlog_err(__func__, "doWork returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    } 

    status = finalize(param, tf, orb, align);
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

int getPar(Param** param) {

  /* MAX_PARAM is a global constant at the top of this file. */
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

  par_names[ipar] = "infile";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->infile)); ipar++;
  par_names[ipar] = "outfile";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->outfile_as_input)); ipar++;
  par_names[ipar] = "orbfile";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbfile)); ipar++;
  par_names[ipar] = "alignfile";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->alignfile)); ipar++;
  par_names[ipar] = "annaber";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->annaber)); ipar++;
  par_names[ipar] = "orbaber";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbaber)); ipar++;
  par_names[ipar] = "attext";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->attext)); ipar++;
  par_names[ipar] = "attcol";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->attcol)); ipar++;
  par_names[ipar] = "attform";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->attform)); ipar++;
  par_names[ipar] = "orbext";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbext)); ipar++;
  par_names[ipar] = "orbcol";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbcol)); ipar++;
  par_names[ipar] = "orbform";
  ape_status[ipar] = ape_trad_query_string(par_names[ipar], &(parptr->orbform)); ipar++;
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
    if (ape_status[ipar] != eOK) {
      ahlog_err(__func__, "Error in parameter %s.\n", par_names[ipar]);
      ape_error_occurred = 1;
    }
  }
  if (ape_error_occurred) {
    ahlog_err(__func__, "One or more parameter errors encountered.\n");
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

  /* Handle clobbering with exclamation mark. */

  if ('!' == parptr->outfile_as_input[0]) {
    strcpy(parptr->outfile, &(parptr->outfile_as_input[1]));
    parptr->outfile_bang = 1;
  } else {
    strcpy(parptr->outfile, parptr->outfile_as_input);
    parptr->outfile_bang = 0;
  }
  

  return 0;

}

/* -------------------------------------------------------------------------- */

int initialize(Param* param, Tempfile** tf, GENORBFILE** orb,
  ALIGN** align, double* mjdref) {

  const ORBINTERP orb_interp = ORBI_WEIGHTED;  /* Orbital interpolation method number */
  long mjdrefi=0.0;  /* Reference MJD for mission, integer part */
  double mjdreff=0.0;  /* Reference MJD for mission, fractional part */
  long orb_mjdrefi=0.0;  /* Reference MJD, integer part, from orbit file */
  double orb_mjdreff=0.0;  /* Reference MJD, fractional part, from orbit file */

  int file_exists=0;  /* Arg for fits_file_exists */
  int cf_status=0;  /* CFITSIO status */
  int are_files_same=0;  /* Flag for infile and outfile specified as same file */

  fitsfile* fp_in=0;  /* Input attitude file */
  fitsfile* fp_orbit=0;  /* Input orbit file */

  char key_comment[FLEN_COMMENT];  /* Buffer for FITS keyword comments */
                            /* FLEN_ constants are in fitsio.h */

  int att_typecode=0;  /* Attitude column CFITSIO typecode */
  long att_repeat=0;  /* Attitude column format repeat factor */
  long att_width=0;  /* Attitude column format width */
  char att_ttype_old[FLEN_VALUE];  /* Attitude column name with _OLD appended */
  char formatstring[FLEN_VALUE];  /* TFORM of attitude column */

  /* Variables for orbit file management */

  char* token = 0;  /* Utility pointer for tokenizing column names */
  int icol = 0;  /* Loop index for column names */
  const char sep[] = ",";  /* Separator for parsing velocity column names */

  char buffer1[FLEN_VALUE];  /* Buffer for converting strings */
  char buffer2[FLEN_VALUE];  /* Buffer for converting strings */
                                  /* FLEN_VALUE is a constant in fitsio.h */
  char orbfile[FLEN_FILENAME];  /* For passing to openGenOrbFile */
  char orbext[FLEN_VALUE];  /* For passing to openGenOrbFile */
                            /* FLEN_ constants are in fitsion.h */

  ORBFORMAT vel_format = ORBF_UNKNOWN;  /* Enum type in genorbfile.h */
  int num_vel_cols = 0;
  char** vel_col_name = 0; 

  /* Pointer aliases to file structures that are allocated here and
   * passed up as arguments. */
  Tempfile* tfptr = 0;
  GENORBFILE* orbptr = 0;
  ALIGN* alignptr = 0;

  /* Initialize Tempfile struct. */
  tfptr = createTempfile();
  *tf = tfptr;

  /* Check existence of input file. */

  fits_file_exists(param->infile, &file_exists, &cf_status);
  if (0 != cf_status) {
    ahlog_err(__func__, "Could not determine if %s exists.\n", param->infile);
    return 1;
  }
  if (0 == file_exists || 2 == file_exists) {
    ahlog_err(__func__, "Infile %s cannot be found.\n", param->infile);
    return 1;
  }
  
  /* Check if input and output files are the same file. */
  
  if (0 == strcmp(param->infile, param->outfile)) { /* strings are the same */
    if (1 == areFilenamesSameFile(param->infile, param->outfile)) {  
      /* devices and inodes are the same */
      are_files_same = 1;
    } else {
      are_files_same = 0;
    }
  } else {
    are_files_same = 0;
  }

  /* Generate a temporary file name. */

  strncpy(tfptr->tempfile, param->outfile, FLEN_FILENAME);
  strcat(tfptr->tempfile, ".tmp");
  
  /* Remove the temp file if the file exists. */

  fits_file_exists(tfptr->tempfile, &file_exists, &cf_status);
  if (0 != cf_status) {
    ahlog_err(__func__, "Could not determine if %s exists.\n", tfptr->tempfile);
    return 1;
  }
  if (0 != file_exists) {
    remove(tfptr->tempfile);
  }
  
  /* Remove the output file if it exists, clobber is enabled, and it
     is not also the input file. */
  
  if (0 == are_files_same) {  /* infile and outfile are different files */
    fits_file_exists(param->outfile, &file_exists, &cf_status);
    if (0 != cf_status) {
      ahlog_err(__func__, "Could not determine if %s exists.\n", param->outfile);
      return 1;
    }
    if (0 != file_exists) {  /* outfile exists */
      if (0 != param->clobber || 0 != param->outfile_bang) {
        remove(param->outfile);
      } else {
        ahlog_err(__func__, "Output file %s already exists, and clobber is disabled.\n", param->outfile);
        return 1;
      }
    }
  } else if (0 != are_files_same && 0 == param->clobber) {
    /* The input and output files are the same but clobber is disabled, so complain and quit. */
    ahlog_err(__func__, "Will not overwrite infile with outfile because they refer to the same file and clobber is disabled.\n");
    return 1;
  }
  
  /* Copy the input FITS file to make the temporary output file. */
  
  fits_open_file(&fp_in, param->infile, READONLY, &cf_status);
  if (0 != cf_status) {
    ahlog_err(__func__, "Could not open input file %s.\n", param->infile);
    return 1;
  }
  
  fits_create_file(&(tfptr->fp), tfptr->tempfile, &cf_status);
  if (0 != cf_status) {
    ahlog_err(__func__, "Could not create temporary output file %s.\n", tfptr->tempfile);
    return 1;
  }
  
  fits_copy_file(fp_in, tfptr->fp, 1, 1, 1, &cf_status);
  if (0 != cf_status) {
    ahlog_err(__func__, "Could not copy input file %s to temporary output file %s.\n", param->infile, tfptr->tempfile);
    return 1;
  }
  
  fits_close_file(fp_in, &cf_status);
  if (0 != cf_status) {
    ahlog_err(__func__, "Could not close input file %s.\n", param->infile);
    return 1;
  }
  
  /* Report copy success. */
  
  tfptr->tempfile_copied = 1;
  ahlog_info(LOW, __func__, "Copied input file %s to %s to be edited as the output file.\n",
    param->infile, tfptr->tempfile);
  
  /* Move to the attitude extension of the temporary output file. */
  
  strcpy(buffer1, param->attext);
  fits_movnam_hdu(tfptr->fp,BINARY_TBL,buffer1,0/* any version*/,&cf_status);
  if (0 != cf_status) {
    fits_report_error(stderr,cf_status);
    ahlog_err(__func__, "Error while opening event file %s and attitude extension %s.\n",
      tfptr->tempfile, param->attext);
      return 1;
  }

  ahlog_info(HIGH, __func__, "Input attitude file:      %s\n", param->infile);
  ahlog_info(HIGH, __func__, "Input attitude extension: %s\n", param->attext);
  ahlog_info(HIGH, __func__, "Input attitude format:    %s\n", param->attform);

  /* Read keywords for reference MJD. */

  fits_read_key_lng(tfptr->fp, "MJDREFI", &mjdrefi, key_comment, &cf_status);
  fits_read_key_dbl(tfptr->fp, "MJDREFF", &mjdreff, key_comment, &cf_status);
  if  (0 != cf_status) {
    fits_report_error(stderr, cf_status);
    ahlog_err(__func__, "Error reading MJDREFI or MJDREFF keyword from FITS header.\n");
    return 1;
  }
  *mjdref = mjdrefi + mjdreff;

  /* Look for the time column. */

  fits_get_colnum(tfptr->fp, CASEINSEN, "TIME", &(param->timecolnum), &cf_status);
  if (0 != cf_status) {
    fits_report_error(stderr, cf_status);
    ahlog_err(__func__, "Could not find TIME column in attitude file.\n");
    return 1;
  }

  /* Look for the attitude column. */

  strcpy(buffer1, param->attcol);
  fits_get_colnum(tfptr->fp, CASEINSEN, buffer1, &(param->attcolnum), &cf_status);
  if (0 != cf_status) {
    fits_report_error(stderr, cf_status);
    ahlog_err(__func__, "Could not find attitude column name %s.\n", param->attcol);
    return 1;
  }

  /* Get the attributes of the attitude column. */

  fits_get_coltype(tfptr->fp, param->attcolnum, &att_typecode, &att_repeat, &att_width, &cf_status);
  if (0 != cf_status) {
    fits_report_error(stderr, cf_status);
    ahlog_err(__func__, "Could not get attributes of attitude column.\n");
    return 1;
  }

  /* Test the attributes of the attitude column. */

  if (0 == strcasecmp(param->attform, "QUAT")) {
    if (TDOUBLE != att_typecode || 4 != att_repeat) {
      ahlog_err(__func__, "Format of attitude quaternion column is not 4D.\n");
      return 1;
    }
    param->attcoltype = e_attQuat;
    strcpy(formatstring, "4D");
  } else if (0 == strcasecmp(param->attform, "EULER")) {
    if (TDOUBLE != att_typecode || 3 != att_repeat) {
      ahlog_err(__func__, "Format of attitude Euler angle column is not 3D.\n");
      return 1;
    }
    param->attcoltype = e_attEuler;
    strcpy(formatstring, "3D");
  } else if (0 == strcasecmp(param->attform, "POINTING")) {
    if (TDOUBLE != att_typecode || 3 != att_repeat) {
      ahlog_err(__func__, "Format of attitude pointing column is not 3D.\n");
      return 1;
    }
    param->attcoltype = e_attPointing;
    strcpy(formatstring, "3D");
  } else {
    ahlog_err(__func__, "Attitude format must be QUAT, EULER, or POINTING.\n");
    return 1;
  }

  /* Copy the attitude column into the <attcol>_OLD column. */

  strcpy(att_ttype_old, param->attcol);
  strcat(att_ttype_old, "_OLD");
  strcpy(buffer1, att_ttype_old);
  strcpy(buffer2, formatstring);
  fits_insert_col(tfptr->fp, param->attcolnum+1, buffer1, buffer2, &cf_status);
  if (0 != cf_status) {
    fits_report_error(stderr, cf_status);
    ahlog_err(__func__, "Could not add old attitude column %s.\n", param->attcol);
    return 1;
  }

  fits_copy_col(tfptr->fp, tfptr->fp, param->attcolnum, param->attcolnum+1, 0, &cf_status);
  if (0 != cf_status) {
    fits_report_error(stderr, cf_status);
    ahlog_err(__func__, "Could not copy attitude column.\n");
    return 1;
  }

  /* Copy the parameters into history. */
  if (0 != param->write_history) {
    HDpar_stamp(tfptr->fp, 0, &cf_status);
    if (0 != cf_status) {
      fits_report_error(stderr, cf_status);
      ahlog_err(__func__, "Could not write parameters to header history.\n");
      return 1;
    }
  }

  /* Orbit file initialization. */

  if (0 != strcasecmp(param->orbfile, "NONE") && 0 != param->do_orbaber) {

    /* Open temporarily to check. */
  
    fits_open_file(&fp_orbit, param->orbfile, READONLY, &cf_status);
    if (0 != cf_status) {
      ahlog_err(__func__, "Could not open orbit file %s.\n", param->orbfile);
      return 1;
    }
  
    strcpy(buffer1, param->orbext);
    fits_movnam_hdu(fp_orbit,BINARY_TBL,buffer1,0/* any version*/,&cf_status);
    if (0 != cf_status) {
      fits_report_error(stderr,cf_status);
      ahlog_err(__func__, "Error while opening orbit file extension %s.\n", param->orbext);
      return 1;
    }

    /* Check keywords for reference MJD. */

    fits_read_key_lng(fp_orbit, "MJDREFI", &orb_mjdrefi, key_comment, &cf_status);
    fits_read_key_dbl(fp_orbit, "MJDREFF", &orb_mjdreff, key_comment, &cf_status);
    if  (0 != cf_status) {
      fits_report_error(stderr, cf_status);
      ahlog_err(__func__, "Error reading MJDREFI or MJDREFF keyword from orbit file FITS header.\n");
      return 1;
    }

    if (orb_mjdrefi != mjdrefi || orb_mjdreff != mjdreff) {
      ahlog_err(__func__, "MJDREFI or MJDREFF not equal in attitude and orbit files.\n");
      return 1;
    }

    fits_close_file(fp_orbit, &cf_status);
    if  (0 != cf_status) {
      fits_report_error(stderr, cf_status);
      ahlog_err(__func__, "Error closing orbit file after checking MJDREFI and MJDREFF.\n");
      return 1;
    }

    /* Reopen the orbit file as a genorbfile. */

    if (param->do_orbaber) {
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
        ahlog_err(__func__, "Invalid value of orbform:  %s.\n", param->orbform);
        return 1;
      }

      vel_col_name = (char**) calloc(num_vel_cols, sizeof(char*));
      if (vel_format != ORBF_VECTOR_VEL) {
        strcpy(buffer1, param->orbcol);
        for (icol=0; icol < num_vel_cols; icol++) {
          vel_col_name[icol] = (char *) calloc(FLEN_VALUE, sizeof(char));
          if (icol == 0) {
            token = strtok(buffer1, sep);
          } else {
            token = strtok(0, sep);
          }
          strcpy(vel_col_name[icol], token);
          ahlog_debug(__func__, __FILE__, __LINE__, "Velocity column name %d parsed:  %s.\n", 
            icol, vel_col_name[icol]);
        }
      } else {
        vel_col_name[0] = (char *) calloc(FLEN_VALUE, sizeof(char));
        strcpy(vel_col_name[0], param->orbcol);
      }

      strcpy(orbfile, param->orbfile);
      strcpy(orbext, param->orbext);
      orbptr = openGenOrbFile(
        orbfile,
        orbext,
        vel_col_name,
        vel_format,
        orb_interp);
        if (0 == orb) {
          ahlog_err(__func__, "Orbit file not opened.  Exiting.\n");
          return 1;
        }
    } else {
      orbptr = 0;
    }
  }
  *orb = orbptr;  /* Copy pointer to calling program. */

  /* Get alignment file contents. */

  /* Returns default alignment if filename is "NONE". */
  strcpy(buffer1, param->alignfile);  
  alignptr = readAlign(buffer1);
  *align = alignptr;  /* Copy pointer to calling program. */

  writeParametersToLog();

  return 0;

} /* end of initialize function */

/* -------------------------------------------------------------------------- */

int doWork(Param* param, Tempfile* tf, GENORBFILE* orb, 
  ALIGN* align, double mjdref) {

  /* Variables used in reading attitude file */

  long nrow_att=0;  /* Number of rows */
  long irow=0;  /* Row index */
  int cf_status=0;  /* CFITSIO status */
  double nulval=0.0;  /* Dummy null value for binary table columns */
  int anynul=0;  /* Null value flag for binary table columns */
  double att_buffer[4];  /* Buffer for attitude vector in a single row */
  QUAT* q; /* Quaternion struct for coordfits library */
  EULER* e; /* Euler angle struct for coordfits library */
  POINTING* p;  /* Pointing struct for coordfits library */
  QUAT* corr_q; /* Quaternion struct for coordfits library */
  EULER* corr_e; /* Euler angle struct for coordfits library */
  POINTING* corr_p;  /* Pointing struct for coordfits library */

  /* Variables used in aberration computation */

  double unitv[3];  /* Cartesion unit vector of pointing */
  double corr_unitv[3];  /* Cartesion unit vector of aberrated pointing */
  double corr_ra=0.0;  /* Aberration-corrected RA */
  double corr_dec=0.0;  /* Aberration-corrected Dec */
  double vhat_sat_total[3];  /* Resultant velocity unit vector of satellite */
  double v_sat_total;  /* Resultant velocity magnitude of satellite */
  double mjd=0.0;  /* Current Modified Julian Date */

  /* Used in debug computation of total and earth velocity */
  const double c_km_per_s = 299792.458;  /* Speed of light */
  double vel_e_c[3];  /* Velocity of earth in units of c */
  double vel_e_km_per_s[3];  /* Velocity of earth in units of km/s */
  double vel_e_mag_km_per_s = 0.0;  /* Speed of earth in units of km/s */
  double vel_tot_km_per_s[3];  /* Total velocity of satellite in units of km/s */
  double vel_tot_mag_km_per_s = 0.0;  /* Total speed of satellite in units of km/s */
  double eradvel = 0.0;  /* Component of earch velocity in direction of pointing */

  double time=0.0;  /* Current attitude time */

  int i=0;  /* Loop index */

  for (i=0; i<3; i++) {
    unitv[i] = 0.0;
    corr_unitv[i] = 0.0;
    vhat_sat_total[i] = 0.0;
    vel_e_c[i] = 0.0;
    vel_e_km_per_s[i] = 0.0;
    vel_tot_km_per_s[i] = 0.0;
  }
  for (i=0; i<4; i++) att_buffer[i] = 0.0;

  /* Loop through rows of the attitude file. */

  fits_get_num_rows(tf->fp, &nrow_att, &cf_status);
  if (0 != cf_status) {
    fits_report_error(stderr, cf_status);
    ahlog_err(__func__, "Error getting number of rows in attitude file.\n");
    return 1;
  }

  /* These are small, so allocate all of them. */

  q = allocateQuat();
  e = allocateEuler();
  p = allocatePointing();
  corr_p = allocatePointing();
  corr_e = allocateEuler();
  corr_q = allocateQuat();

  ahlog_debug(__func__, __FILE__, __LINE__, "%s%s\n",
     "        TIME           VTOT       VTOTX      VTOTY      VTOTZ     ",
     " VEARTH     VEARTHX    VEARTHY    VEARTHZ    ERADVEL");

  for (irow=1; irow<=nrow_att; irow++) {

    /* Get time. */

    fits_read_col_dbl(tf->fp, param->timecolnum, irow, 1, 1, nulval, &time, &anynul, &cf_status);

    /* Convert attitude to pointing struct. */

    switch (param->attcoltype) {
      case e_attQuat:
        fits_read_col_dbl(tf->fp, param->attcolnum, 
          irow, 1, 4, nulval, att_buffer, &anynul, &cf_status);
        setQuat(q, att_buffer[0], att_buffer[1], att_buffer[2], att_buffer[3]);
        convertQuatToPointing(p,q,align);
        break;
      case e_attEuler:
        fits_read_col_dbl(tf->fp, param->attcolnum, 
          irow, 1, 3, nulval, att_buffer, &anynul, &cf_status);
        setEulerAngles(e, att_buffer[0]*M_PI/180.0, att_buffer[1]*M_PI/180.0, att_buffer[2]*M_PI/180.0);
        convertEulerToPointing(p,e,align);
        break;
      case e_attPointing:
        fits_read_col_dbl(tf->fp, param->attcolnum, 
          irow, 1, 3, nulval, att_buffer, &anynul, &cf_status);
        setPointing(p, att_buffer[0], att_buffer[1], att_buffer[2]);
        break;
      default:
        ahlog_err(__func__, "Bad AttColType enum.\n");
        return 1;
    }

    convertLongLatDegToUnitv(p->ra, p->dec, unitv);

    /* Compute the aberration correction as a unit vector times v/c. */
    mjd = mjdref + time/86400.0;
    findAberrationCorrection(mjd, mjdref, orb, param->do_annaber,
      param->do_orbaber, param->inv_annaber, param->inv_orbaber,
      &v_sat_total, vhat_sat_total);

    /* Debug mode:  print total and earth velocity. */
    if (0 != ahlog_get_debug()) {
      
      /* Earth velocity */
      earthVelcAtMJD(mjd, vel_e_c);
      vel_e_mag_km_per_s = 0.0;
      for (i=0; i<3; i++) {
        vel_e_km_per_s[i] = vel_e_c[i]*c_km_per_s;
        vel_e_mag_km_per_s += vel_e_km_per_s[i]*vel_e_km_per_s[i];
      }
      vel_e_mag_km_per_s = sqrt(vel_e_mag_km_per_s);

      /* Total velocity */
      vel_tot_mag_km_per_s = v_sat_total*c_km_per_s;
      for (i=0; i<3; i++) {
        vel_tot_km_per_s[i] = vhat_sat_total[i]*vel_tot_mag_km_per_s;
      }

      /* Earth velocity component in direction of pointing */
      eradvel = 0.0;
      for (i=0; i<3; i++) {
        eradvel += vel_e_km_per_s[i]*unitv[i];
      }

      ahlog_debug(__func__, __FILE__, __LINE__, 
         " %17.6f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f\n",
         time, vel_tot_mag_km_per_s, vel_tot_km_per_s[0], vel_tot_km_per_s[1], vel_tot_km_per_s[2],
         vel_e_mag_km_per_s, vel_e_km_per_s[0], vel_e_km_per_s[1], vel_e_km_per_s[2], eradvel);
    }

    /* Add in the aberration correction */
    for (i=0; i<3; i++) {
      corr_unitv[i] = unitv[i] + v_sat_total*vhat_sat_total[i];
    }

    /* Convert the vector back to RA, Dec */
    convertUnitvToLongLatDeg(corr_unitv, &corr_ra, &corr_dec);
    setPointing(corr_p, corr_ra, corr_dec, p->roll);

    /* Convert attitude from pointing struct to correct format and store */

    switch (param->attcoltype) {
      case e_attQuat:
        convertPointingToQuat(corr_q, corr_p, align);
        for (i=0; i<4; i++) att_buffer[i] = corr_q->p[i];
        fits_write_col_dbl(tf->fp, param->attcolnum, 
          irow, 1, 4, att_buffer, &cf_status);
        break;
      case e_attEuler:
        convertPointingToEuler(corr_e,corr_p,align);
        att_buffer[0] = corr_e->phi*180.0/M_PI;
        att_buffer[1] = corr_e->theta*180.0/M_PI;
        att_buffer[2] = corr_e->psi*180.0/M_PI;
        fits_write_col_dbl(tf->fp, param->attcolnum, 
          irow, 1, 3, att_buffer, &cf_status);
        break;
      case e_attPointing:
        att_buffer[0] = corr_p->ra;
        att_buffer[1] = corr_p->dec;
        att_buffer[2] = corr_p->roll;
        fits_write_col_dbl(tf->fp, param->attcolnum, 
          irow, 1, 3, att_buffer, &cf_status);
        break;
      default:
        ahlog_err(__func__, "Bad AttColType enum.\n");
        return 1;
    }
  }

  fits_update_key_log(tf->fp, "ABERRAT", param->do_annaber,
                      "Was aberration correction applied to sky coords", 
                      &cf_status);

  fits_update_key_log(tf->fp, "ABERORB", param->do_orbaber, 
                      "Was orbital aberr corr applied to sky coords?", 
                      &cf_status);

  fits_update_key_log(tf->fp, "FOLOWSUN", param->do_annaber,
                      "Was the Sun position recalculated for each evt?", 
                      &cf_status);

  if (0 != param->do_annaber && 0 != param->inv_annaber) {
    fits_update_key_log(tf->fp, "INVABERR", param->inv_annaber,
                        "Was the aberration correction inverted?", 
                        &cf_status);
  }

  if (0 != param->do_orbaber && 0 != param->inv_orbaber) {
    fits_update_key_log(tf->fp, "INVOABER", param->inv_orbaber, 
                        "Was the orbital aberration correction inverted?", 
                         &cf_status);
  }
  
  if (0 != cf_status) {
    fits_report_error(stderr,cf_status);
    ahlog_err(__func__, "Error writing RA_NOM and DEC_NOM or Aberration keywords.\n");
    return 1;
  }

  tf->tempfile_written = 1;   /* Success */

  ahlog_info(LOW, __func__, "%d attitude file rows edited.\n", nrow_att);

  return 0;

} /* End of do_work */

/* -------------------------------------------------------------------------- */

int finalize(Param* param, Tempfile* tf, GENORBFILE* orb, 
  ALIGN* align) {

  int cf_status=0;

  /* Close output attitude file. */
  if (0 != tf->fp) {
    fits_close_file(tf->fp, &cf_status);
    if (0 != cf_status) {
      ahlog_err(__func__, "Error closing temporary output file.\n");
      return 1;
    }
  }

  /* Handle the temporary output file. */

  if(tf->tempfile_written)
  {
    /* Writing to the temporary output file completed normally, so rename the file
     * as the output file. */
    rename(tf->tempfile, param->outfile);
    ahlog_info(HIGH, __func__, "Output file %s has been written.\n", param->outfile);
  }
  else if(tf->tempfile_copied && !param->debug)
  {
    /* There was a problem during the processing, so the output file
     * is not in the expected post-processing state.  Delete the
     * file so that it is not confused by the user as a good output
     * file. */
    remove(tf->tempfile);
    ahlog_info(HIGH, __func__, "Output file %s was not created.\n", tf->tempfile);
  }
  else if(tf->tempfile_copied && param->debug)
  {
    /* There was a problem during the processing, so the output file
     * is not in the expected post-processing state.  Leave the file
     * if debug mode is enabled as that may be helpful for
     * investigating the problem. */
    ahlog_info(HIGH, __func__, "Incomplete temporary output file %s was written.\n", tf->tempfile);
  }

  if (0 != param) destroyParam(param);

  if (0 != tf) {
    destroyTempfile(tf);
  }

  if (0 != orb) closeGenOrbFile(orb);

  if (0 != align) destroyAlign(align);

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

/* Determine if the filenames refer to the same file.  Returns -1 is
   either file does not exist, 0 if both files exist separately, and 1
   if the filenames refer to the same, existing file. */
int areFilenamesSameFile
(
 const char* filename1, /* One filename */
 const char* filename2  /* Another filename */
 ) 
{
    struct stat stat1, stat2;
    if(stat(filename1, &stat1) < 0) return -1;
    if(stat(filename2, &stat2) < 0) return -1;
    return (stat1.st_dev == stat2.st_dev) && (stat1.st_ino == stat2.st_ino);
}

/* -------------------------------------------------------------------------- */

/* Allocate and initialize param structure */
Param* createParam () {
  Param* param = (Param*)calloc(1, sizeof(Param));
  param->infile = 0;
  strcpy(param->outfile, "");
  param->orbfile = 0;
  param->alignfile = 0;
  param->annaber = 0;
  param->orbaber = 0;
  param->attext = 0;
  param->attcol = 0;
  param->attform = 0;
  param->orbext = 0;
  param->orbcol = 0;
  param->orbform = 0;
  param->outfile_as_input = 0;
  param->outfile_bang = 0;
  param->chatter = 0;
  param->clobber = 0;
  param->debug = 0;
  param->write_history = 0;
  param->attcoltype = e_attUnknown;
  param->attcolnum = 0;
  param->timecolnum = 0;
  param->do_annaber = 0;
  param->do_orbaber = 0;
  param->inv_annaber = 0;
  param->inv_orbaber = 0;
  return param;
}

/** \brief Deallocate param structure
 * \param[in] param parameter structure
 */
void destroyParam (Param* param) {
  free(param->infile);
  free(param->orbfile);
  free(param->alignfile);
  free(param->annaber);
  free(param->orbaber);
  free(param->attext);
  free(param->attcol);
  free(param->attform);
  free(param->orbext);
  free(param->orbcol);
  free(param->orbform);
  free(param->outfile_as_input);
  free(param);
}

/** \brief Allocate and initialize structure to manage temporary output file
 * \return pointer to tempfile structure
 */
Tempfile* createTempfile () {
  Tempfile* tf = (Tempfile*)calloc(1, sizeof(Tempfile));
  tf->fp = 0;
  tf->tempfile = (char*)calloc(FLEN_FILENAME, sizeof(char));
  tf->tempfile_copied = 0;
  tf->tempfile_written = 0;
  return tf;
}

/** \brief Deallocate structure to manage temporary output file
 * \param[in] tf tempfile structure
 */
void destroyTempfile (Tempfile* tf) {
  free(tf->tempfile);
  free(tf);
}

/* ========================================================================== */

/* Revision Log
 $Log: aberattitude.c,v $
 Revision 1.16  2016/06/13 15:37:46  rshill
 Added projection of earth velocity onto source direction vector
 to the debug output.

 Revision 1.15  2016/05/31 19:44:01  rshill
 Added total velocity and total and earth speed to velocity printout.

 Revision 1.14  2016/05/31 19:01:23  rshill
 Added DEBUG mode calculation and printing of earth velocity.

 Revision 1.13  2016/04/14 20:12:49  rshill
 Added writeParametersToLog().

 Revision 1.12  2016/03/24 23:50:09  rshill
 Removed aspect calculation from the tool.

 Revision 1.11  2016/02/18 23:50:55  rshill
 Changed keyword ORBIABER to ABERORB (keyword ABERRAT left as-is).

 Revision 1.10  2016/01/13 14:18:49  klrutkow
 added new aberration keywords to output file

 Revision 1.9  2015/12/31 00:58:11  rshill
 Added parameter stamping to header history.

 Revision 1.8  2015/08/19 21:32:02  rshill
 Corrected prologue line.

 Revision 1.7  2015/08/19 21:26:51  rshill
 Comment prologue cleanup.

 Revision 1.6  2015/08/05 18:21:57  rshill
 Improved some messages.  Corrected free() error in finalize.

 Revision 1.5  2015/08/04 20:40:56  rshill
 Added exclamation point to clobber handling.

 Revision 1.4  2015/06/23 15:23:25  rshill
 Updated openGenOrbFile call to include interpolation method.

 Revision 1.3  2015/05/19 15:20:06  rshill
 Corrected the testing for infile and outfile being the same file.

 Revision 1.2  2015/05/18 23:12:36  rshill
 Added some missing newlines to messages.

 Revision 1.1  2015/05/14 22:29:24  rshill
 Converted language to plain C.

 Revision 1.9  2015/03/18 23:14:07  rshill
 Support Keplerian elements via upgraded genorbfile.

 Revision 1.8  2015/01/06 01:27:31  rshill
 Updated in the general parameter update for all tools.

 Revision 1.7  2014/09/05 23:29:12  rshill
 Corrected writing of DEC_NOM keyword.

 Revision 1.6  2014/09/05 00:54:08  rshill
 Fixed bug in ORBCOL parsing.  Convert Euler angles between
 degrees and radians (EULER struct in radians).  Change sign in applying aberration correction
 to attitude.

 Revision 1.5  2014/08/14 18:18:04  mwitthoe
 aberattitude: put name of tool in Doxygen group description to match convention; my editor automatically replaced a couple tabs with spaces

 Revision 1.4  2014/08/11 20:16:01  rshill
 Fix ahtime reference.

 Revision 1.3  2014/08/11 18:20:48  rshill
 Fixed parameter retrieval; added documentation.

 Revision 1.2  2014/08/08 03:46:47  rshill
 Many changes.  Compiles.  Not tested.

 Revision 1.1  2014/08/05 23:08:31  rshill
 Initial checkin for backup.  Incomplete.


*/
