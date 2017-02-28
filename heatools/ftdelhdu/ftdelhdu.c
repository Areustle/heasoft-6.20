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

#define TOOLSUB ftdelhdu
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftdelhdu (void);
int ftdelhdu_getpar (char *infile, char *outfile);
int ftdelhdu_outfile(char *infile, char *outfile);
int ftdelhdu_inplace(char *infile);

/*---------------------------------------------------------------------------*/
int ftdelhdu (void)
{
/*  Delete the current HDU in place or in a copy of the input file */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    int status;
    static char taskname[80] = "ftdelhdu";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    if (!(status = ftdelhdu_getpar(infile, outfile))) {

      if (*outfile) {

        /* Call work function to copy all but the current HDU */
        /* from the input file to the output file. */
        status = ftdelhdu_outfile(infile, outfile);

      } else {

        /* Delete the current HDU in place in the input file */
        status = ftdelhdu_inplace(infile);
      }
    }
    return(status);
}
/*---------------------------------------------------------------------------*/
int ftdelhdu_getpar(
    char *infile,   /* O - Input file name */
    char *outfile)  /* O - Output file name */

/*  read input parameters for the ftdelhdu task from the .par file */
{
    int status, len;
    char msg[MAXMSG];
    char *cptr;

    if ((status = PILGetFname("infile", infile))) {
        sprintf(msg, "Error reading the 'infile' parameter.");
        HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetString("outfile", outfile))) {
        sprintf(msg, "Error reading the 'outfile' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    else  {

        /* remove leading blanks in string */
        cptr = outfile;
        while (*cptr == ' ') cptr++;  
        if (cptr != outfile) {
            len = strlen(cptr);
            memmove(outfile, cptr, len + 1);
        }

        /* test for special strings */
        if (!strcmp(outfile, "none") || !strcmp(outfile, "NONE") )
        *outfile = '\0';
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftdelhdu_outfile(
    char *infile,   /* I - Input file name */
    char *outfile)  /* I - Output file name */
{
/*  Copy all but the current HDU from the input to the output file  */

    fitsfile *infptr = 0, *outfptr = 0;
    int status = 0, hdunum;
    long dummy[1];
    
    if (fits_open_file(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input file:\n %s\n",infile);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */
        
    if (fits_create_file(&outfptr, outfile, &status)) goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfile);

    if (fits_get_hdu_num(infptr, &hdunum) == 1) {

       /* If deleting primary array, replace it with a null primary array */
       fits_create_img(outfptr, 16, 0, dummy, &status);

       /* now copy any following HDUs */
       if (fits_copy_file(infptr, outfptr, 0, 0, 1, &status)) goto cleanup;
       headas_chat(5, "Substituted a null array in the output file.\n");

    } else {   /* copy preceding and following HDUs, but not current HDU */

       if (fits_copy_file(infptr, outfptr, 1, 0, 1, &status)) goto cleanup;
    }
     
    headas_chat(5, 
           "Copied all other HDUs to the output file except the input HDU.\n");

cleanup:

    if (outfptr) fits_close_file(outfptr, &status);
    if ( infptr) fits_close_file(infptr,  &status);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftdelhdu_inplace(
    char *infile)   /* I - Input file name */
{
/*  delete the current HDU in the input file, in place */

    fitsfile *infptr = 0;
    int status = 0, tstatus = 0, hdunum, hdutype, bitpix, naxis, ncols;
    int confirm, ii;
    long naxes[9], nrows;
    char extname[FLEN_VALUE];

    if (fits_open_file(&infptr, infile, READWRITE, &status) ) goto cleanup;
    headas_chat(5,"Opened the input file:\n %s\n",infile);

    fits_get_hdu_num(infptr, &hdunum);
    fits_get_hdu_type(infptr, &hdutype, &status);
    *extname = '\0';
    fits_read_key(infptr, TSTRING, "EXTNAME", extname, NULL, &tstatus);  
    headas_chat(1, "\nWarning: this ");

    if (hdunum == 1)
        headas_chat(1, "Primary Array ");
    else if (hdutype == IMAGE_HDU)
        headas_chat(1, "IMAGE extension ");
    else if (hdutype == ASCII_TBL)
        headas_chat(1, "ASCII table ");
    else
        headas_chat(1, "Binary table ");

    headas_chat(1, "will be deleted IN PLACE:\n");
    headas_chat(1, "  HDU number %d,  EXTNAME = %s\n", hdunum, extname);

    if (hdutype == IMAGE_HDU) {

      fits_get_img_param(infptr, 9, &bitpix, &naxis, naxes, &status);
      headas_chat(1, "  BITPIX = %d, NAXIS = %d\n", bitpix, naxis);
      for (ii = 0; ii < naxis; ii++)
        headas_chat(1, "    NAXIS%d = %ld\n", ii+1, naxes[ii]);
      headas_chat(1, "\n");

    } else {

      fits_get_num_cols(infptr, &ncols, &status);
      fits_get_num_rows(infptr, &nrows, &status);
      headas_chat(1, "  No. of columns = %d, no. of rows = %ld\n\n",
                  ncols, nrows);
    }

    if (!status)
       status = PILGetBool("confirm", &confirm);

    if (!status && confirm) {
       fits_delete_hdu(infptr, NULL, &status);
       headas_chat(5,"Deleted the HDU in place.\n");
    } else 
        headas_chat(5,"Did NOT delete the HDU.\n");

cleanup:

    if (infptr) fits_close_file(infptr, &status);

    return(status);
}
