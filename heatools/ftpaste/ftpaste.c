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

#define TOOLSUB ftpaste
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftpaste (void);
int ftpaste_getpar(char *infile, char *pastefile, char *outfile, int *copyall);
int ftpaste_work  (char *infile, char *pastefile, char *outfile, int  copyall);

/*---------------------------------------------------------------------------*/
int ftpaste (void)
{
/*  copy the input HDU or file to the output file and then   */
/*  add the columns in the pastefile HDU to the output file */

    char infile[PIL_LINESIZE], pastefile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    int copyall, status;
    static char taskname[80] = "ftpaste";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftpaste_getpar(infile, pastefile, outfile, &copyall);

    /* call work function to copy the table columns to the output file */
    if (!status)
        status = ftpaste_work(infile, pastefile, outfile, copyall);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftpaste_getpar(
    char *infile,     /* O - Input file name */
    char *pastefile,  /* O - Output file name */
    char *outfile,    /* O - Output file name */
    int  *copyall)    /* O - Copy all HDUs? */

/*  read input parameters for the ftpaste task from the .par file */
{
    int status = 0;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
        sprintf(msg, "Error reading the 'infile' parameter.");
        HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetFname("pastefile", pastefile))) {
        sprintf(msg, "Error reading the 'pastefile' parameter.");
        HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetFname("outfile", outfile))) {
        sprintf(msg, "Error reading the 'outfile' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("copyall", copyall))) {
        sprintf(msg, "Error reading the 'copyall' parameter.");
        HD_ERROR_THROW(msg,status);
    }


    return(status);
}
/*---------------------------------------------------------------------------*/
int ftpaste_work(
    char *infile,      /* I - Input file name */
    char *pastefile,   /* I - Input file name */
    char *outfile,     /* I - Output file name */
    int  copyall)      /* I - Copy all HDUs? */

/*  copy the input HDU or file to the output file and then   */
/*  add the columns in the pastefile HDU to the output file */
{
    fitsfile *infptr = 0, *pastefptr = 0, *outfptr = 0;
    int status = 0, hdunum, ncols, ii;

    /* Move to first 'interesting' HDU if none was specified. */
    if (fits_open_table(&infptr, infile, READONLY, &status) )goto cleanup;
    headas_chat(5,"Opened the input table:\n %s\n",infile);

    /* Open the file containing the colummns to be pasted.    */
    /* Move to first 'interesting' HDU if none was specified. */
    if (fits_open_table(&pastefptr, pastefile, READONLY, &status))goto cleanup;
    headas_chat(5,"Opened the paste table:\n %s\n", pastefile);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    /* Create the output file */
    if (fits_create_file(&outfptr, outfile, &status) )goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfile);

    if (copyall) {      /* copying all HDUs? */

        fits_get_hdu_num(infptr, &hdunum);  /* save current position */

        if (fits_copy_file(infptr, outfptr, 1, 1, 1, &status) )goto cleanup;
        headas_chat(5,"Copied all HDUs from input file to output file.\n");

        /* reset position to the initial HDU  */
        if (fits_movabs_hdu(outfptr, hdunum, NULL, &status)) goto cleanup;

    } else {      /* just copy the current HDU */

        if (fits_copy_hdu(infptr, outfptr, 0, &status)) goto cleanup;
        headas_chat(5, "Copied the input HDU to the output file.\n");
    }

    fits_get_num_cols(pastefptr, &ncols, &status);
   
    /* copy all the columns from the paste file to the output file */
    for (ii = 0; ii < ncols; ii++) {
        if (fits_copy_col(pastefptr, outfptr, ii+1, 1000, 1, &status))
            goto cleanup;
        headas_chat(5, 
            "Copied column %d from paste table to output table.\n", ii+1);
    }

    HDpar_stamp(outfptr, 0, &status); /* write history keywords */

cleanup:

    if (outfptr)   fits_close_file(outfptr,   &status);
    if (pastefptr) fits_close_file(pastefptr, &status);
    if ( infptr)   fits_close_file(infptr,    &status);

    return(status);
}
