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

#define TOOLSUB ftcalc
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftcalc (void);
int ftcalc_getpar (char *infile, char *outfile, char *column, char *expression, 
                   char *rows, int *element, char *tform);
int ftcalc_work   (char *infile, char *outfile, char *column, char *expression, 
                   char *rows, int element, char *tform);

/*---------------------------------------------------------------------------*/
int ftcalc (void)
{
/* calculate values for a table column based on input  expression */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE], expression[PIL_LINESIZE];
    char column[80], rows[PIL_LINESIZE], tform[80];
    int status;
    int element;
    static char taskname[80] = "ftcalc";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftcalc_getpar(infile, outfile, column, expression, rows, &element, tform);

    /* call work function to calculate table column values */
    if (!status)
        status = ftcalc_work(infile, outfile, column, expression, rows, element, tform);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftcalc_getpar(
    char *infile,     /* O - Input file name  */
    char *outfile,    /* O - Output file name */
    char *column,     /* O - Column name */
    char *expression, /* O - Arithmetic expression      */
    char *rows,       /* O - List of rows to operate on */
    int  *element,    /* O - Optional vector element number for result */
    char *tform)      /* O - Optional TFORM value for new column */
{
/*  read input parameters for the ftcalc task from the .par file */

    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
      sprintf(msg, "Error reading the 'infile' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("column", column))) {
      sprintf(msg, "Error reading the 'column' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("expression", expression))) {
     sprintf(msg, "Error reading the 'expression' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("rows", rows))) {
      sprintf(msg, "Error reading the 'rows' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetInt("element", element))) {
      sprintf(msg, "Error reading the 'element' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("tform", tform))) {
      sprintf(msg, "Error reading the 'tform' parameter");
      HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftcalc_work(
    char *infile,   /* I - Input file name */
    char *outfile,  /* I - Output file name */
    char *column,   /* I - Column name */
    char *expression, /* I - Arithmetic expression */
    char *rows,     /* I - list of rows to operate on */
    int  element,  /* I - optional vector element number for result */
    char *tform)    /* I - optional TFORM value for new column */
{
/*  calculate values for a table column */

    fitsfile *infptr = 0, *outfptr = 0;
    char *cptr;
    int status = 0, hdunum, nranges, colnum, typecode, valuesize, anynul;
    long nrows, *minrow = 0, *maxrow = 0, repeat, ii;
    char msg[MAXMSG];
    void *array = 0, *dataptr;

    /* Open the input file, and move to first 'interesting' table */
    if (fits_open_table(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input table:\n %s\n",infile);

    /* find how many ranges were specified ( = no. of commas in string + 1) */
    for (cptr = rows, nranges = 1; (cptr = strchr(cptr, ',')); nranges++)
       cptr++;
 
    /* allocate arrays for min and max values for each range */
    minrow = calloc(nranges, sizeof(long));
    maxrow = calloc(nranges, sizeof(long));

    if (!minrow || !maxrow) {
        status = MEMORY_ALLOCATION;
        sprintf(msg, "ERROR: failed to allocate memory for row ranges.");
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }

    fits_get_num_rows(infptr, &nrows, &status);  /* No. of table rows */

    /* parse range list into array of range min and max values */
    if (ffrwrg(rows, nrows, nranges, &nranges, minrow, maxrow, &status))
        goto cleanup;

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    if (fits_create_file(&outfptr, outfile, &status)) goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfile);

    fits_get_hdu_num(infptr, &hdunum);  /* save current position */

    if (fits_copy_file(infptr, outfptr, 1, 1, 1, &status)) goto cleanup;
    headas_chat(5,"Copied all HDUs from input file to output file.\n");

    fits_movabs_hdu(outfptr, hdunum, NULL, &status); /* reset HDU position */

    /* finally, calculate the column values */

    if (element == 0) {
        /* this is the normal case: the result is written directly to the column */

        if (fits_calculator_rng(infptr, expression, outfptr, column, tform,
            nranges, minrow, maxrow, &status)) goto cleanup;

        headas_chat(5, "Calculated the '%s' column using this expression:\n %s\n",
                column, expression);

    } else {
       /* this is a special case: write the result to a vector element */

       if (fits_get_colnum(outfptr, CASEINSEN, column, &colnum, &status)) goto cleanup;
       
       if (fits_get_eqcoltype(outfptr, colnum, &typecode, &repeat, 0, &status)) goto cleanup;
       
       if (typecode < 0) {
        status = BAD_TFORM;
        sprintf(msg, "ERROR: variable length array columns are not supported.");
        HD_ERROR_THROW(msg,status);
        goto cleanup;          
       }

       if (element > repeat) {
        status = BAD_ELEM_NUM;
        sprintf(msg, "ERROR: element number is greater than vector column length");
        HD_ERROR_THROW(msg,status);
        goto cleanup;          
       }
       
       if (typecode == TSHORT) {
         array = calloc(nrows, sizeof(short));
	 valuesize = sizeof(short);
       } else if (typecode == TBYTE) {
         array = calloc(nrows, sizeof(char));
	 valuesize = sizeof(char);
       } else if (typecode == TINT32BIT || typecode == TLONG) {
         array = calloc(nrows, sizeof(int));
	 valuesize = sizeof(int);
       } else if (typecode == TFLOAT) {
         array = calloc(nrows, sizeof(float));
	 valuesize = sizeof(float);
       } else if (typecode == TDOUBLE) {
         array = calloc(nrows, sizeof(double));
	 valuesize = sizeof(double);
       } else {
         status = BAD_TFORM;
         sprintf(msg, "ERROR: unsupported vector column data type.");
         HD_ERROR_THROW(msg,status);
         goto cleanup;          
       }

       if (!array) {
        status = MEMORY_ALLOCATION;
        sprintf(msg, "ERROR: failed to allocate memory for result.");
        HD_ERROR_THROW(msg,status);
        goto cleanup;
       }

       if (fits_calc_rows(infptr, typecode, expression, 1, nrows, 0, array, 
            &anynul, &status)) goto cleanup;

       headas_chat(5, "Calculated the '%s' column using this expression:\n %s\n",
                column, expression);

       /* copy result to vector element in each row */
       for (ii = 0; ii < nrows; ii++) {
          dataptr = (char *) array + ii * valuesize;
          fits_write_col(outfptr, typecode, colnum, ii + 1, element, 1, 
	      dataptr, &status);
       }

       headas_chat(5, "Wrote the result to element '%d' \n", element);
    }

    /* write optional history keywords */
    status = HDpar_stamp(outfptr, 0, &status); 

cleanup:

    if (maxrow) free(maxrow);
    if (minrow) free(minrow);
    if (array) free(array);
    if (outfptr) fits_close_file(outfptr, &status);
    if (infptr)  fits_close_file(infptr,  &status);

    return(status);
}
