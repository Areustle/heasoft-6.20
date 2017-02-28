#include <stdio.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
#define MAXRANGES 500
#define MAXMSG 256

/*
HISTORY
-------
  Version 1.0 written by William Pence, NASA/GSFC, March 2002
*/

#define TOOLSUB ftdelrow
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftdelrow (void);
int ftdelrow_getpar (char *infile, char *outfile, char *rows);
int ftdelrow_outfile(char *infile, char *outfile, char *rows);
int ftdelrow_inplace(char *infile, char *rows);
int fits_parse_ranges(char *rowlist, long maxrows, long maxranges,
    long *numranges, long *minrow, long *maxrow, int *status);
/*---------------------------------------------------------------------------*/
int ftdelrow (void)
{
    /* ftdelrow: delete rows in the input file, either in place or in a copy */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE], rows[PIL_LINESIZE];
    int status;
    static char taskname[80] = "ftdelrow";
    static char version[8] = "1.10";

    /* Register taskname and version. */
    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftdelrow_getpar(infile, outfile, rows);

    if (!status) {

      if (*outfile) {

        /* Call work function to delete rows in a copy of the input file. */
        status = ftdelrow_outfile(infile, outfile, rows);

      } else {

        /* Delete rows in place in the input file */
        status = ftdelrow_inplace(infile, rows);
      }
    }
    return(status);
}
/*---------------------------------------------------------------------------*/
int ftdelrow_getpar(
    char *infile,   /* O - Input file name */
    char *outfile,  /* O - Output file name */
    char *rows)     /* O - list of row ranges */

/*  read input parameters for the ftdelrow task from the .par file */
{
    int status, len;
    char *cptr;
    char msg[MAXMSG];
    char *p = 0;
    FILE *rowfile = 0;
    char rowlist[PIL_LINESIZE], line[PIL_LINESIZE];

    if ((status = PILGetFname("infile", infile))) {
        sprintf(msg, "Error reading the 'infile' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("outfile", outfile))) {
        sprintf(msg, "Error reading the 'outfile' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("rows", rows))) {
        sprintf(msg, "Error reading the 'rows' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    else  {

        /* remove leading blanks in output file string */
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

    if (*rows == '@') { /* open text file containing list of rows? */
       
        if( (rowfile = fopen(rows+1, "r" ))==NULL ) {

            sprintf(msg,"Could not open ASCII file %s.",rows+1);
            status = FILE_NOT_OPENED;
            HD_ERROR_THROW(msg,status);
            return(status);

        }

        if ( !fgets(line, PIL_LINESIZE-1, rowfile)) {

            sprintf(msg,"File list is empty: %s.",rows+1);
            status = EOF;
            HD_ERROR_THROW(msg,status);
            goto cleanup; /* EOF */

        } else {

            headas_chat(5,"Opened list of rows:\n %s\n",rows+1);

            /* Remove newline: */
            if ( (p = strchr(line, '\n')) ) *p = 0;

            /* Copy first line to rowlist: */
            strcpy (rowlist,line);

            /* Copy remaining lines (separated by commas) to rowlist: */
            while ( fgets(line, PIL_LINESIZE-1, rowfile) != NULL ) {
                    if ( (p = strchr(line, '\n')) ) *p = 0;
                    strcat (rowlist,",");
                    strcat (rowlist,line);
            }
            strcpy(rows, rowlist);
	}
    }

cleanup:

    if (rowfile) fclose(rowfile);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftdelrow_outfile(
    char *infile,   /* I - Input file name */
    char *outfile,  /* I - Output file name */
    char *rows)     /* I - rows to delete */
{
/*  Delete rows in a copy of the input file.  */

    fitsfile *infptr = 0, *outfptr = 0;
    int status = 0, hdunum;

    /* Open first interesting HDU, if HDU not specified */
    if (fits_open_table(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input table:\n %s\n",infile);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */
    
    /* Create the output file */
    if (fits_create_file(&outfptr, outfile, &status)) goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfile);
     
    fits_get_hdu_num(infptr, &hdunum); /* save current position */

    /* copy all HDUs */
    /* Note: we copy the whole HDU (including the heap) then delete */
    /* the bad rows, instead of just copying the good rows, because */
    /* this insures that any heap data (in variable length columns) */
    /* gets properly copied to the output file.                     */

    if (fits_copy_file(infptr, outfptr, 1, 1, 1, &status) ) goto cleanup;
    headas_chat(5,"Copied input file to output file.\n");

    /* move back to the original table */
    if (fits_movabs_hdu(outfptr, hdunum, NULL, &status)) goto cleanup;

    /* delete the rows */
    if (fits_delete_rowrange(outfptr, rows, &status)) goto cleanup;
    headas_chat(5,"Deleted these rows in the output file:\n %s\n",rows);

    HDpar_stamp(outfptr, 0, &status); /* write optional history keywords */

cleanup:

    if (outfptr) fits_close_file(outfptr, &status);
    if ( infptr) fits_close_file(infptr,  &status);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftdelrow_inplace(
    char *infile,   /* I - Input file name */
    char *rows)     /* I - rows to delete */
{
/*  delete rows in place in the input file  */

    fitsfile *infptr = 0;
    int status = 0, tstatus = 0, hdutype, hdunum, ncols, confirm;
    long nrows;
    char extname[FLEN_VALUE];

    /* Open first interesting table, if HDU not specified */
    if (fits_open_table(&infptr, infile, READWRITE, &status)) goto cleanup;
    headas_chat(5,"Opened the input table:\n %s\n",infile);

    fits_get_hdu_num(infptr, &hdunum);
    *extname = '\0';
    fits_read_key(infptr, TSTRING, "EXTNAME", extname, NULL, &tstatus);  
    fits_get_num_cols(infptr, &ncols, &status);
    fits_get_num_rows(infptr, &nrows, &status);

    headas_chat(1, "\nWarning: will delete rows in the following ");

    fits_get_hdu_type(infptr, &hdutype, &status);
    if (hdutype == ASCII_TBL)
        headas_chat(1, "ASCII table:\n");
    else
        headas_chat(1, "Binary table:\n");

    headas_chat(1, "  HDU number %d,  EXTNAME = %s\n", hdunum, extname);
    headas_chat(1, "  %d columns X %ld rows\n", ncols, nrows);
    headas_chat(1, "  Rows to be deleted = %s\n", rows);

    if (!status)
       status = PILGetBool("confirm", &confirm);

    if (!status && confirm) {

      /* delete the rows */
      if (fits_delete_rowrange(infptr, rows, &status) ) goto cleanup;
      headas_chat(5,"Deleted these rows in the output file:\n %s\n", rows);

      /* write history keywords, depending on HISTORY parameter */
      HDpar_stamp(infptr, 0, &status); /* write to current HDU */ 
    }

cleanup:

    if (infptr) fits_close_file(infptr, &status);

    return(status);
}
