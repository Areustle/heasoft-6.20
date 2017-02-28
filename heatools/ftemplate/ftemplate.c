#include <string.h>
#include <stdlib.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"

#define MAXMSG 256

/*
HISTORY
-------
 *  
 *  26 Sep 2000  ftools version by William Pence and James Peachey
 *  04 Feb 2003  Converted to run in the HEADAS environment by W Pence 
*/

#define TOOLSUB ftemplate
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Prototypes */
int ftemplate(void);
int ftemplateGetPar(char*, char*);

#define BUFSIZE 1025

int ftemplate()
{

/*  Creates new FITS file from a user-supplied template file.
    Get names of (input) template file and (output) FITS file using
    ftemplateGetPar, then call CFITSIO routines to create the FITS file.
 */

    int status = 0;
    fitsfile *fptr = NULL; 
    char template[BUFSIZE] = "", filename[BUFSIZE] = "";
    char msg[MAXMSG];

    static char taskname[80] = "ftemplate";
    static char version[8] = "1.00";

    /* Register taskname and version. */
    set_toolname(taskname);
    set_toolversion(version);

    /* get file name and template name parameters */
    status = ftemplateGetPar(template, filename);

    if (strlen(filename) + strlen(template) + 3 > BUFSIZE) {
      sprintf(msg, "File name + template name is too long.\n");
      status =1;
      HD_ERROR_THROW(msg,status);
    }

    /*  append template to filename. Format = "filename(template)". */
    strcat(filename, "(" );
    strcat(filename, template);
    strcat(filename, ")" );

    fits_create_file(&fptr, filename, &status); /* create new FITS file */
    fits_close_file(fptr, &status);    /* close the file */

    return(status);
}
/*----------------------------------------------------------------*/
int ftemplateGetPar(char *template, char *outfile)
{
    /* get input parameters for ftemplate */

    int status;
    char msg[MAXMSG];

    if ((status = PILGetString("template", template))) {
      sprintf(msg, "Error reading the 'template' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

  return(status);
}

