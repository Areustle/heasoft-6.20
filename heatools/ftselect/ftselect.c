#include <stdio.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
#define MAXMSG 256

/*
HISTORY
-------
  Version 1.0 written by William Pence, NASA/GSFC, March 2002
*/

#define TOOLSUB ftselect
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftselect (void);
int ftselect_getpar (char *infile, char *outfile, char *expression,
              int *copyall);
int ftselect_work   (char *infile, char *outfile, char *expression,
              int copyall);

/*---------------------------------------------------------------------------*/
int ftselect (void)
{
/* copy selected rows from a table */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE], expression[PIL_LINESIZE];
    int status, copyall;
    static char taskname[80] = "ftselect";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftselect_getpar(infile, outfile, expression, &copyall);

    /* call work function to select rows */
    if (!status)
        status = ftselect_work(infile, outfile, expression, copyall);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftselect_getpar(
    char *infile,     /* O - Input file name     */
    char *outfile,    /* O - Output file name    */
    char *expression, /* O - Boolean expression  */
    int *copyall)     /* O - copy all HDUs?      */
{
/*  read input parameters for the ftselect task from the .par file */

    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
      sprintf(msg, "Error reading the 'infile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("expression", expression))) {
      sprintf(msg, "Error reading the 'expression' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("copyall", copyall))) {
      sprintf(msg, "Error reading the 'copyall' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftselect_work(
    char *infile,     /* I - Input file name     */
    char *outfile,    /* I - Output file name    */
    char *expression, /* I - Boolean expression  */
    int copyall)      /* I - copy all HDUs?      */
{
/* Copy selected rows from a table */
/* Evaluate the expression on each row of the table; copy the row from */
/* the input to the output table if it evaluates to TRUE */

    fitsfile *infptr = 0, *outfptr = 0;
    int status = 0, hdunum;
    long inrows, outrows, zero = 0;

    /* Open the input file, and move to first 'interesting' table */
    if (fits_open_table(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input table:\n %s\n",infile);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    if (fits_create_file(&outfptr, outfile, &status)) goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfile);

    fits_get_hdu_num(infptr, &hdunum);  /* save current position */

    /* copy preceding HDUs to the output file */
    if (copyall) {
      if (fits_copy_file(infptr, outfptr, 1, 0, 0, &status)) goto cleanup;
      headas_chat(5,"Copied all preceding HDUs from input to output file.\n");
    } else {
      /* create null primary array */
      if (fits_create_img(outfptr, BYTE_IMG, 0, NULL, &status)) goto cleanup;
      headas_chat(5,"Created null primary array in the output file.\n");
    }
  
    /* Copy header keywords in CHDU and reset NAXIS2 and PCOUNT = 0 */
    if (fits_copy_header(infptr, outfptr, &status)) goto cleanup;
    fits_update_key(outfptr, TLONG, "NAXIS2", &zero, NULL, &status);
    fits_update_key(outfptr, TLONG, "PCOUNT", &zero, NULL, &status);

    /* copy the rows that evaluate to TRUE */
    if (fits_select_rows(infptr, outfptr, expression, &status)) goto cleanup;

    headas_chat(5, "Used the following selection expression:\n %s\n",
                expression);

    fits_get_num_rows(infptr, &inrows, &status);
    fits_get_num_rows(outfptr, &outrows, &status);

    headas_chat(3, " Copied %ld out of %ld rows to the output table.\n",
           outrows, inrows);

    /* copy any following HDUs to the output file */
    if (copyall) {
      if (fits_copy_file(infptr, outfptr, 0, 0, 1, &status)) goto cleanup;
      headas_chat(5,"Copied any following HDUs from input to output file.\n");
    }

    HDpar_stamp(outfptr, 0, &status); /* write optional history keywords */

cleanup:

    if (outfptr) fits_close_file(outfptr, &status);
    if (infptr)  fits_close_file(infptr,  &status);

    return(status);
}
