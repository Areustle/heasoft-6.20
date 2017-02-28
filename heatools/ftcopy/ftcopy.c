#include <stdio.h>
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

#define TOOLSUB ftcopy
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftcopy (void);
int ftcopy_getpar (char *infile, char *outfile, int *copyall);
int ftcopy_work   (char *infile, char *outfile, int  copyall);

/*---------------------------------------------------------------------------*/
int ftcopy (void)
{
/*  copy the input HDU or file to the output file */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    int copyall, status;
    static char taskname[80] = "ftcopy";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftcopy_getpar(infile, outfile, &copyall);

    /* call work function to copy the input file to the output file */
    if (!status)
        status = ftcopy_work(infile, outfile, copyall);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftcopy_getpar(
    char *infile,   /* O - Input file name */
    char *outfile,  /* O - Output file name */
    int  *copyall)  /* O - Copy all HDUs? */

/*  read input parameters for the ftcopy task from the .par file */
{
    int status;
    char  msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
      sprintf(msg, "Error reading the 'infile' parameter");
      HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetFname("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("copyall", copyall))) {
      sprintf(msg, "Error reading the 'copyall' parameter.\n");
      HD_ERROR_THROW(msg,status);
    }


    return(status);
}
/*---------------------------------------------------------------------------*/
int ftcopy_work(
    char *infile,   /* I - Input file name */
    char *outfile,  /* I - Output file name */
    int  copyall)   /* I - Copy all HDUs? */

/*  copy the input HDU or file to the output file */
{
    fitsfile *infptr = 0, *outfptr = 0;
    int status = 0;

    /* Open the input file, and move to first 'interesting' HDU */
    if (fits_open_data(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input file:\n %s\n",infile);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    if (fits_create_file(&outfptr, outfile, &status)) goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfile);

    if (copyall) {   /* copying all HDUs? */

      if (fits_copy_file(infptr, outfptr, 1, 1, 1, &status)) goto cleanup;
      headas_chat(5, "Copied all HDUs to the output file.\n");

      /* move back to 1st HDU, before writing HISTORY keywords */
      if (fits_movabs_hdu(outfptr, 1, NULL, &status)) goto cleanup;

    } else {   /* just copy the current HDU */

      if (fits_copy_hdu(infptr, outfptr, 0, &status)) goto cleanup;
      headas_chat(5, "Copied the current HDU to the output file.\n");
    }

    /* write optional history keywords */
    status = HDpar_stamp(outfptr, 0, &status);

cleanup:

    if (outfptr) fits_close_file(outfptr, &status);
    if (infptr)  fits_close_file(infptr,  &status);

    return(status);
}
