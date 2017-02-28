#include <stdio.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"

#define MAXMSG 256

/*
HISTORY
-------
  Version 1.0 written by William Pence, NASA/GSFC, May 2004
*/

#define TOOLSUB ftinsert
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftinsert (void);
int ftinsert_getpar (char *inhdu, char *infile, char *outfile, int *after);
int ftinsert_work   (char *inhdu, char *infile, char *outfile, int after);

/*---------------------------------------------------------------------------*/
int ftinsert (void)
{
/*  Make a copy of the input file (2nd parameter) while also inserting a  */
/*  copy of the input HDU (first parameter) so that it immediately follows */
/*  the CHDU of the 2nd file.   */

    char inhdu[PIL_LINESIZE], infile[PIL_LINESIZE], outfile[PIL_LINESIZE];

    int status, after;
    static char taskname[80] = "ftinsert";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftinsert_getpar(inhdu, infile, outfile, &after);

    /* call work function to insert the input HDU to the output file */
    if (!status)
        status = ftinsert_work(inhdu, infile, outfile, after);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftinsert_getpar(
    char *inhdu,     /* O - Input HDU to be inserted */
    char *infile,    /* O - Input file to be copied */
    char *outfile,   /* O - Output file name */
    int  *after)     /* O - Insert AFTER (or BEFORE) exising HDU */

/*  read input parameters for the ftinsert task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("inhdu", inhdu))) {
        sprintf(msg, "Error reading 'inhdu' parameter in .par file");
         HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("infile", infile) )) {
        sprintf(msg, "Error reading 'infile' parameter in .par file");
         HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("outfile", outfile) )) {
        sprintf(msg, "Error reading 'outfile' parameter in .par file");
         HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("after", after))) {
        sprintf(msg, "Error reading the 'after' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftinsert_work(
    char *inhdu,     /* I - Input HDU to be inserted  */
    char *infile,    /* I - Input file to be copied  */
    char *outfile,   /* I - Output file name  */
    int   after)     /* I - Insert AFTER (or BEFORE) exising HDU */

/* insert the extension pointed to by inhdu into a copy of infile */
{
    fitsfile *infptr1 = 0, *infptr2 = 0, *outfptr = 0;
    int status = 0;

    /* Open both input files and move to the specified extensions */
    if (fits_open_file(&infptr1, inhdu, READONLY, &status))
        goto cleanup;
    headas_chat(5,"Opened the inhdu file:\n %s\n",inhdu);

    if (fits_open_file(&infptr2, infile, READONLY, &status))
        goto cleanup;
    headas_chat(5,"Opened the infile:\n %s\n",infile);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    /* Create the new output file */
    if (fits_create_file(&outfptr, outfile, &status))
        goto cleanup;
    headas_chat(5,"Opened the output file:\n %s\n", outfile);

    /* copy the previous HDUs (and current HDU if 'after' is TRUE) from infile */
    if (fits_copy_file(infptr2, outfptr, 1, after, 0, &status))
        goto cleanup;
    headas_chat(5, "Copied first HDUs from infile to the output file.\n");

    /* copy the current HDU from input2 to output file */
    if (fits_copy_file(infptr1, outfptr, 0, 1, 0, &status))
        goto cleanup;
    headas_chat(5, "Copied CHDU from inhdu to the output file.\n");

    /* write history keywords, depending on HISTORY parameter */
    HDpar_stamp(outfptr, 0, &status); /* write to current HDU */ 

    /* copy following HDUs (and current HDU if 'after' is FALSE) from infile */
    if (fits_copy_file(infptr2, outfptr, 0, !after, 1, &status))
        goto cleanup;
    headas_chat(5, "Copied any following HDUs from infile to the output file.\n");

cleanup:

    if (outfptr) fits_close_file(outfptr,  &status);
    if (infptr2)  fits_close_file(infptr2, &status);
    if (infptr1)  fits_close_file(infptr1, &status);

    return(status);
}



