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
  Version 1.00 written by William Pence, NASA/GSFC, March 2002
  Version 1.01 updated to not return error if keyword not found (WDP), October 2010  (WDP)
*/

#define TOOLSUB ftkeypar
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftkeypar (void);
int ftkeypar_getpar (char *infile, char *keyword);
int ftkeypar_work   (char *infile, char *keyword);

/*---------------------------------------------------------------------------*/
int ftkeypar (void)
{
/*  read the specified keyword and write the value to the .par file */

    char infile[PIL_LINESIZE], keyword[80];
    int status;
    static char taskname[80] = "ftkeypar";
    static char version[8] = "1.01";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftkeypar_getpar(infile, keyword);

    /* call work function to copy the keyword value to the .par file */
    if (!status)
        status = ftkeypar_work(infile, keyword);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftkeypar_getpar(
    char *infile,   /* O - Input file name */
    char *keyword)  /* O - Keyword name */

/*  read input parameters for the ftkeypar task from the .par file */
{
    int status = 0;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
        sprintf(msg, "Error reading the 'infile' parameter.");
        HD_ERROR_THROW(msg,status);
    }



    else if ((status = PILGetString("keyword", keyword))) {
        sprintf(msg, "Error reading the 'keyword' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftkeypar_work(
    char *infile,   /* I - Input file name */
    char *keyword)  /* I - Keyword name */

/*  read the specified keyword and write the value to the .par file */
{
    fitsfile *infptr = 0;
    int status, ivalue, bvalue, hdunum = 0;
    char value[FLEN_VALUE], comment[FLEN_COMMENT], dtype[2], *longstr;
    double rvalue;
    char msg[MAXMSG];

    /* initialize output parameters to default values */
    status = PILPutBool("exist", 0);
    if (!status) status = PILPutString("value", ""); 
    if (!status) status = PILPutString("datatype", "");
    if (!status) status = PILPutReal("rvalue", 0.0);
    if (!status) status = PILPutInt("ivalue", 0);
    if (!status) status = PILPutString("svalue", "");
    if (!status) status = PILPutBool("bvalue", 0);
    if (!status) status = PILPutString("comment", "");

    if (status)  {
        sprintf(msg, "Error initializing parameters in .par file");
        HD_ERROR_THROW(msg,status);
        return(status);
    }

    if (!strcmp("COMMENT", keyword) || !strcmp("comment", keyword) ) {
        sprintf(msg, "ERROR: COMMENT keyword not allowed.");
        status = 1;
        HD_ERROR_THROW(msg,status);
        return(status);
    }
    else if (!strcmp("HISTORY", keyword) || !strcmp("history", keyword) ) {
        sprintf(msg, "ERROR: HISTORY keyword not allowed.");
        status = 1;
        HD_ERROR_THROW(msg,status);
        return(status);
    }
    else if (!strcmp("CONTINUE", keyword) || !strcmp("continue", keyword) ) {
        sprintf(msg, "ERROR: CONTINUE keyword not allowed.");
        status = 1;
        HD_ERROR_THROW(msg,status);
        return(status);
    }

    /* Open the input file, and move to first 'interesting' HDU */
    if (fits_open_data(&infptr, infile, READONLY, &status) ) goto cleanup;
    headas_chat(5,"Opened the input file:\n %s\n",infile);

    fits_get_hdu_num(infptr, &hdunum); /* get the current HDU number */

    if (hdunum > 1) headas_chat(5,"Moved to HDU number: %d\n",hdunum);

    /* try to read the keyword, returning a literal string */
    fits_read_keyword(infptr, keyword, value, comment, &status);

    /* ignore keyword value undefined error */
    if (status == VALUE_UNDEFINED) status = 0;
    
    if (status) {
        headas_chat(3, "Keyword: %s does not exist in this header.\n", keyword);

        if (status == KEY_NO_EXIST) status = 0;

        goto cleanup;
    }

    headas_chat(5,"Read the keyword:\n");
    headas_chat(3, "keyword: %s\n", keyword);
    headas_chat(3, "value:   %s\n", value);
    headas_chat(3, "comment: %s\n", comment);

    status = PILPutString("value", value);  /* write the literal value */

    fits_get_keytype(value, dtype, &status);
    /* ignore keyword value undefined error */
    if (status == VALUE_UNDEFINED) status = 0;
    
    /* In case there are multiple keywords with the same name, move back */
    /* to top of the header to guarantee that we reread the same keyword. */
    if (fits_read_record(infptr, 0, value, &status)) goto cleanup;

    switch (*dtype) {
      case 'C':       /* character string datatype? */
        /* support possible long string values */
        if (!fits_read_key_longstr(infptr, keyword, &longstr, comment, &status)) {
          status = PILPutString("svalue", longstr); /* write the string value */
          if (!status) status = PILPutString("datatype","string");
          free(longstr);
        }
        break;
      case 'F':       /* real floating point datatype ? */
        if (!fits_read_key(infptr, TDOUBLE, keyword, &rvalue, comment, &status)) {
          status = PILPutReal("rvalue", rvalue); /* write the double value */
          if (!status) status = PILPutString("datatype","real");
        }
        break;
      case 'I':       /* integer datatype ? */

        if (!fits_read_key(infptr, TINT, keyword, &ivalue, comment, &status)) {
          status = PILPutInt("ivalue", ivalue); /* write the integer value */
          if (!status) status = PILPutString("datatype","integer");
        }
        break;
      case 'L':       /* boolean datatype ? */
        if (!fits_read_key(infptr, TINT, keyword, &bvalue, comment, &status)) {
          status = PILPutBool("bvalue", bvalue); /* write the boolean value */
          if (!status) status = PILPutString("datatype","boolean");
        }
    }

    if (status == 0) {
        status = PILPutString("comment", comment);  /* write the comment */
        if (!status) status = PILPutBool("exist", 1);
    }

cleanup:

    if (infptr) fits_close_file(infptr,  &status);

    return(status);
}
