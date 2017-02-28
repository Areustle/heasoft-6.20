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

#define TOOLSUB ftappend
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftappend (void);
int ftappend_getpar (char *infile, char *outfile);
int ftappend_work   (char *infile, char *outfile);

/*---------------------------------------------------------------------------*/
int ftappend (void)
{
/*  append the input HDU to the output file */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    int status;
    static char taskname[80] = "ftappend";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftappend_getpar(infile, outfile);

    /* call work function to append the input HDU to the output file */
    if (!status)
        status = ftappend_work(infile, outfile);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftappend_getpar(
    char *infile,   /* O - Input file name */
    char *outfile)  /* O - Output file name */

/*  read input parameters for the ftappend task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
        sprintf(msg, "Error reading 'infile' parameter in .par file");
         HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetFname("outfile", outfile) )) {
        sprintf(msg, "Error reading 'outfile' parameter in .par file");
         HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftappend_work(
    char *infile,   /* I - Input file name */
    char *outfile)  /* I - Output file name */

/*  append the input HDU to the output file */
{
    fitsfile *infptr = 0, *outfptr = 0;
    int status = 0;

    /* Open input file and move to 1st interesting HDU, if none specified */
    if (fits_open_data(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input file:\n %s\n",infile);

    if (fits_open_file(&outfptr, outfile, READWRITE, &status)) goto cleanup;
    headas_chat(5,"Opened the output file:\n %s\n", outfile);

    if (fits_copy_hdu(infptr, outfptr, 0, &status)) goto cleanup;
    headas_chat(5, "Appended input HDU to the output file.\n");

    /* write history keywords, depending on HISTORY parameter */
    HDpar_stamp(outfptr, 0, &status); /* write to current HDU */ 

cleanup:

    if (outfptr) fits_close_file(outfptr, &status);
    if (infptr)  fits_close_file(infptr,  &status);

    return(status);
}
