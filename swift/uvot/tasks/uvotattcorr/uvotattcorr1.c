/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotattcorr/uvotattcorr1.c,v $
 * $Revision: 1.1 $
 * $Date: 2006/07/27 14:54:23 $
 *
 *
 * $Log: uvotattcorr1.c,v $
 * Revision 1.1  2006/07/27 14:54:23  rwiegand
 * Tool for applying UVOT aspect corrections to attitude file.
 *
 */


#include <stdio.h>              /* pil.h uses but does not include */
#include <string.h>

#include "headas.h"
#include "pil.h"
#include "report.h"
#include "keyutil.h"
#include "attfile.h"
#include "attout.h"
#include "align.h"

#include "uvottool.h"
#include "caldbquery.h"


#define TOOLSUB uvotattcorr
#include "headas_main.c"


#define VERSION 1.0
#define CREATOR _STRINGIFY(TOOLSUB) " " _STRINGIFY(VERSION)
#define ORIGIN "GSFC"

#define PAR_INFILE    "infile"
#define PAR_DELTAFILE "deltafile"
#define PAR_OUTFILE   "outfile"
#define PAR_ALIGNFILE "alignfile"



typedef struct
{
  int chatter;
  int clobber;
  int history;

  char infile[PIL_LINESIZE];
  char deltafile[PIL_LINESIZE];
  char outfile[PIL_LINESIZE];
  char alignfile[PIL_LINESIZE];

} Parameters;



static int
get_parameters (Parameters * p)
{
  int code = 0;

  if (!code)
    code = PILGetFname(PAR_INFILE, p->infile);

  if (!code)
    code = PILGetFname(PAR_DELTAFILE, p->deltafile);

  if (!code)
    code = PILGetFname(PAR_OUTFILE, p->outfile);

  if (!code)
    code = PILGetFname(PAR_ALIGNFILE, p->alignfile);

  p->clobber = headas_clobpar;
  p->chatter = headas_chatpar;
  get_history(&p->history);

  if (code)
    {
      code = TASK_SETUP_ERROR;
      report_error("unable to load parameters\n");
    }
  else
    report_verbose("parameters loaded\n");

  return code;
}


static int
initialize_output_pointing (ATTFILE * attin, ATTOUT * attout, int * colnum)
{
    int status = 0;
    int create = TRUE;
    int outcols;
    int in_pointing;
    int out_pointing;
    char key[FLEN_KEYWORD];

    if (fits_get_colnum(attin->fp, CASEINSEN, "POINTING",
			&in_pointing, &status))
        headas_chat(0, "unable to locate POINTING [%d]\n", status);

    else if (fits_get_num_cols(attout->fp, &outcols, &status))
        headas_chat(0, "unable to get number of columns [%d]\n", status);

    else if (!(out_pointing = outcols + 1)) {
        headas_chat(0, "non-positive column number\n");
        return 1;
    }

    else if (fits_insert_rows(attout->fp, 0, attin->nrows, &status))
        headas_chat(0, "unable to insert output rows [%d]\n", status);

    else if (fits_copy_col(attin->fp, attout->fp, in_pointing,
                  out_pointing, create, &status))
        headas_chat(0, "unable to create POINTING [%d]\n", status);

    else if (!sprintf(key, "TTYPE%d", out_pointing))
        headas_chat(0, "unable to format TTYPEn\n");

    else if (fits_copy_col(attin->fp, attout->fp, in_pointing,
                  out_pointing + 1, create, &status))
        headas_chat(0, "unable to create POINTING_ORIG [%d]\n", status);

    else if (!sprintf(key, "TTYPE%d", out_pointing + 1))
        headas_chat(0, "unable to format TTYPEn\n");

    else if (fits_update_key_str(attout->fp, key, "POINTING_ORIG", 0, &status))
        headas_chat(0, "unable to name POINTING_ORIG [%d]\n", status);

    else
        headas_chat(1, "initialized POINTING and POINTING_ORIG columns\n");


    *colnum = out_pointing;

    return status;
}


static int
correct_attitude (/* const */ Parameters * par)
{
  int code = 0;

  ATTFILE * attin = 0;
  ATTFILE * attdelta = 0;
  ATTOUT * attout = 0;
  ALIGN * align = 0;
  fitsfile * fpout = 0;

  QUAT * q1 = 0;
  QUAT * q2 = 0;
  QUAT * q = 0;

  long row;
  double time;
  int out_pointing = 0;


  attin = openAttFile(par->infile);
  if (attin == NULL) {
    code = TASK_INPUT_ERROR;
    report_error("could not open %s\n", par->infile);
  }

  attdelta = openAttFile(par->deltafile);
  if (attdelta == NULL) {
    code = TASK_INPUT_ERROR;
    report_error("could not open %s\n", par->deltafile);
  }

  attout = createAttOut(par->outfile);
  if (attout == NULL) {
    code = TASK_OUTPUT_ERROR;
    report_error("could not create %s\n", par->outfile);
  }


  /* initialize bonus output columns if alignment provided */
  if (strcasecmp(par->alignfile, "NONE")) {
    
    if (!strncasecmp(par->alignfile, "CALDB", 5)) {
      CALDBQuery query = { 0 };
      strcpy(query.codename, "ALIGNMENT");
      strcpy(query.instrument, "SC");
      query.infile = par->infile;
      set_caldb_query_qualifiers(&query, par->alignfile);
      if (simple_caldb_query(&query, 0, par->alignfile)) {
        code = TASK_SETUP_ERROR;
        report_error("unable to resolve alignfile=%s\n", par->alignfile);
        goto cleanup;
      }
      HDpar_note(PAR_ALIGNFILE, par->alignfile);
    }

    align = readAlign(par->alignfile);
    if (!align) {
      code = TASK_INPUT_ERROR;
      report_error("unable to load %s=%s\n", PAR_ALIGNFILE, par->alignfile);
    }
      
    else if (initialize_output_pointing(attin, attout, &out_pointing)) {
      code = TASK_OUTPUT_ERROR;
      report_error("unable to initialize output POINTING\n");
    }
  }

  if (code)
    goto cleanup;


  /***********************************************
  * loop over the rows in the attin attitude file
  * applying rotations from the attdelta
  ***********************************************/
  q1 = allocateQuat();
  q2 = allocateQuat();
  q  = allocateQuat();

  for (row = 1; row <= attin->nrows; ++row) {

    time = readTimeFromAttFile(attin, row);
    readQuatFromAttFile(attin, q1, row);

    findQuatInAttFile(attdelta, q2, time);

    /************************************************
    * combine the quaternions and write the results *
    ************************************************/
    invertQuatInPlace(q1);
    productOfQuats(q, q1, q2);
    invertQuatInPlace(q);
    addAttOutRow(attout, time, q);

    if (out_pointing) {
      int status = 0;
      double ra, dec, roll, pointing[3];
      convertQuatToRADecRoll(align, q, &ra, &dec, &roll);
      pointing[0] = ra;
      pointing[1] = dec;
      pointing[2] = roll;
      fits_write_col_dbl(attout->fp, out_pointing, row, 1, 3,
                 pointing, &status);
      if (status)
        report_warning("unable to write updated row %ld pointing [%d]\n",
                       row, status);
    }
  }

  /* transfer HDU keywords and ACS_DATA HDU */
  {
    int hdu0;
    int status = 0;
    fpout = finishAttOut(attout);
    for (hdu0 = 0; !code && hdu0 < 2; ++hdu0)
      {
        FITSHeader header = { 0 };
        int types[] = { TYP_COMM_KEY, TYP_CONT_KEY,
                          TYP_USER_KEY, TYP_REFSYS_KEY, 0 };
        header.user = types;
        header.accept = accept_keycard_types;

        if (fits_movabs_hdu(attin->fp, hdu0+1, NULL, &status))
          report_error("unable to move to input HDU %d [%d]\n", hdu0, status);

        else if (fits_movabs_hdu(fpout, hdu0+1, NULL, &status))
          report_error("unable to move to output HDU %d [%d]\n", hdu0, status);

        else if (fetch_header_records_fits(&header, attin->fp)) {
          code = TASK_INPUT_ERROR;
          report_error("unable to retrieve input keywords\n");
        }

		else if ((header.accept = 0))
          report_error("impossible: header.accept=0 is true!\n");

        else if (update_header_records_fits(&header, fpout)) {
          code = TASK_INPUT_ERROR;
          report_error("unable to write output keywords\n");
        }

        if (status)
          code = TASK_FITS_ERROR;
      }

    if (!code)
      {
        if (fits_movabs_hdu(attin->fp, 3, NULL, &status))
          report_error("unable to move to input ACS_DATA HDU 3 [%d]\n", status);

        else if (fits_copy_hdu(attin->fp, fpout, 0, &status))
          report_error("unable to copy input ACS_DATA HDU [%d]\n", status);

        if (status)
          code = TASK_FITS_ERROR;
      }
  }

  /*************************
  * HEADAS parameter stamp *
  *************************/
  {
    int status = 0;
    HDpar_stamp(fpout, 1, &status);
    if (status)
      report_warning("unable to save parameter history [%d]\n", status);
#if 0
    if (fits_write_chksum(task.ofptr, &status))
      {
        code = TASK_OUTPUT_ERROR;
        report_error("unable to write checksums\n");
      }
#endif
  }


cleanup:
  /**************
  * close files *
  **************/
  if (attin)
    closeAttFile(attin);
  if (attdelta)
    closeAttFile(attdelta);

#if 1
  if (fpout) {
    int status = 0;
    fits_close_file(fpout, &status);
  }
#else
  did finishAttOut above
  if (attout)
    closeAttOut(attout);
#endif

  if (align)
    destroyAlign(align);

  if (q) {
    destroyQuat(q1);
    destroyQuat(q2);
    destroyQuat(q);
  }

  return code;
}




int
TOOLSUB ()
{
  int code = 0;
  Parameters p = { 0 };

  set_toolname(_STRINGIFY(TOOLSUB));
  set_toolversion(_STRINGIFY(VERSION));

  add_report_function(&report_headas);

  if (!code)
    code = get_parameters(&p);

  if (!code)
    headas_clobberfile(p.outfile);

  if (!code)
    code = correct_attitude(&p);

  remove_report_function(&report_headas);

  return code;
}

