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
  Version 1.0 written by William Pence, NASA/GSFC, April 2002
*/

#define TOOLSUB ftchecksum
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftchecksum (void);
int ftchecksum_getpar (char *infile, int *update, int *datasum);
int ftchecksum_work   (char *infile, int  update, int  datasum);

/*---------------------------------------------------------------------------*/
int ftchecksum (void)
{
/*  check or update the CHECKSUM keywords in the input file */

    char infile[PIL_LINESIZE];
    int update, datasum, status;
    static char taskname[80] = "ftchecksum";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftchecksum_getpar(infile, &update, &datasum);

    /* call work function to compute checksums */
    if (!status)
        status = ftchecksum_work(infile, update, datasum);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftchecksum_getpar(
    char *infile,   /* O - Input file name */
    int *update,    /* O - Update checksum keywords if out of date? */
    int *datasum)   /* O - Recompute DATASUM keyword value?  */

/*  read input parameters for the ftchecksum task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
      sprintf(msg, "Error reading the 'infile' parameter");
      HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetBool("update", update))) {
      sprintf(msg, "Error reading the 'update' parameter");
      HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetBool("datasum", datasum))) {
      sprintf(msg, "Error reading the 'datasum' parameter.\n");
      HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftchecksum_work(
    char *infile,   /* I - Input file name */
    int  update,    /* I - Update checksum keywords if out of date? */
    int  datasum)   /* I - Recompute DATASUM keyword value?  */

/*  check or update the CHECKSUM keywords in the input file */
{
    FILE *textfile = 0;
    fitsfile *fptr = 0;
    int status = 0, tstatus, mode, hdunum, data_ok, hdu_ok;
    int global_ok = 1, all_ok;
    char oldcheckval[40], olddataval[40], newcheckval[40], newdataval[40];
    char *p;
    char msg[MAXMSG];

    status = PILPutBool("allok", 0);  /* initialize output parameter */

    if (update)
        mode = READWRITE;
    else
        mode = READONLY;

    if (*infile == '@') {  /* open text file containing list of files? */
       
        if( (textfile = fopen(infile+1, "r" ))==NULL ) {
            sprintf(msg,"Could not open ASCII file %s.",infile+1);
            status = FILE_NOT_OPENED;
            HD_ERROR_THROW(msg,status);
            return(status);
        }
    }

    while(1) {  /* loop over every file in the list */

      if (textfile) {  /* get next file name from text file */

         if (fptr) {
            fits_close_file(fptr,  &status);
            fptr = 0;
         }

         if ( fgets(infile,256,textfile) == NULL ) {
            break;  /* end of file */
         }
         if (NULL != (p = strchr(infile, '\n'))) *p = 0; /* remove EOL chars */
         if (NULL != (p = strchr(infile, '\r'))) *p = 0; /* remove EOL chars */ 
      }

      /* Open the input file  */
      if (fits_open_file(&fptr, infile, mode, &status)) goto cleanup;

      headas_chat(1,"File: %s\n",infile);

      all_ok = 1;

      if (update) { 

         /* Open and close every HDU before calculating the checksum because */
         /* CFITSIO might modify certain keywords like TFORM='PE' -> 'PE(n)' */
         /* Move to the end of file by moving to a very large extension no.  */

         if (fits_movabs_hdu(fptr, 999, NULL, &status) == END_OF_FILE)
            status = 0;
      }

      headas_chat(2,"  HDU CHECKSUM  DATASUM\n");
      hdunum = 1;

      /* loop over every HDU in the file */
      while (!(fits_movabs_hdu(fptr, hdunum, NULL, &status) )) {

        if (update) { /* update CHECKSUM and DATASUM keywords if out of date */

            /* save current values of the keywords */
            *oldcheckval = *olddataval = '\0';
            tstatus   = 0;
            fits_read_key(fptr, TSTRING, "CHECKSUM", oldcheckval,
                           NULL, &tstatus);
            tstatus   = 0;
            fits_read_key(fptr, TSTRING, "DATASUM", olddataval,
                           NULL, &tstatus);

            /* compute or update the checksums */
            if (datasum) {
                if (fits_write_chksum(fptr, &status)) {
                    all_ok = 0;  /* error computing checksum */
                    goto cleanup;
                }
            } else {
                if (fits_update_chksum(fptr, &status)) {
                    all_ok = 0;  /* error computing checksum */
                    goto cleanup;
                }
            }

            *newcheckval = *newdataval  = '\0';
            tstatus   = 0;
            fits_read_key(fptr, TSTRING, "CHECKSUM", newcheckval,
                           NULL, &tstatus);
            tstatus   = 0;
            fits_read_key(fptr, TSTRING, "DATASUM", newdataval,
                           NULL, &tstatus);

            /* compare new values with old, to see if they were modified */
            if (strcmp(oldcheckval, newcheckval))
                headas_chat(2,"%4d:  updated ", hdunum);
            else
                headas_chat(2,"%4d:  correct ", hdunum);
      
            if (strcmp(olddataval, newdataval))
                headas_chat(2," updated\n");
            else
                headas_chat(2," correct\n");

        } else  { /* simply check if the CHECKSUM is correct */

            if (fits_verify_chksum(fptr, &data_ok, &hdu_ok, &status))
                goto cleanup;

            if (hdu_ok == 1) {
               headas_chat(2,"%4d:  correct ", hdunum);
            } else if (hdu_ok == 0) {
               headas_chat(2,"%4d:  missing ", hdunum);
               if (all_ok == 1)
                   all_ok = 0;
            } else if (hdu_ok == -1) {
               all_ok = -1;
               headas_chat(2,"%4d:  invalid ", hdunum);
            }

            if (data_ok == 1) {
               headas_chat(2," correct\n");
            } else if (data_ok == 0) {
               headas_chat(2," missing\n");  /* DATASUM not required */
            } else if (data_ok == -1) {
               headas_chat(2," invalid\n");
               all_ok = 0;
            }

        }

        hdunum++;
      } /* end of loop over every HDU in the file */

      if (status == END_OF_FILE)
          status = 0;

      if (all_ok == 1) {
          headas_chat(1,"   OK, all checksums are valid.\n");
      } else if (all_ok == 0) {
          headas_chat(1,"   Some or all HDUs are not checksummed (no CHECKSUM keyword)\n");
          global_ok = 0;
      } else if (all_ok == -1) {
          headas_chat(1,"   !! Some or all HDUs have invalid checksums !!\n");
          global_ok = 0;
      }


      headas_chat(2,"\n");  /* add a blank line for clarity */

      if (!textfile)
          break;      /* only doing a single file */

    } /* end of while loop over file names in a text file */

cleanup:

    if (textfile) fclose(textfile);

    if (fptr)  fits_close_file(fptr,  &status);

    if (!status)
        PILPutBool("allok", global_ok);  /* write output parameter */

    return(status);
}
