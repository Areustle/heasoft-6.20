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
  Version 1.0 written by William Pence, NASA/GSFC, September 2002
*/

/* headas_main() requires that TOOLSUB be defined first */
#define TOOLSUB ftedit
#include "headas_main.c"

/* Function Prototypes */
int ftedit (void);
int ftedit_getpar(char *infile, char *column, int *row, char *value,
    int *element);
int ftedit_image_file (fitsfile *infptr, char *editfile);
int ftedit_table_file (fitsfile *infptr, char *editfile);
int ftedit_image_par(fitsfile *infptr, char *column, int row,
    char *value, int element);
int ftedit_table_par(fitsfile *infptr, char *column, int row,
    char *value, int element);

/*---------------------------------------------------------------------------*/
int ftedit (void)
{
/*
    Edit individual element values in the input table.
*/
    fitsfile *infptr = 0;
    char infile[PIL_LINESIZE], column[PIL_LINESIZE], value[PIL_LINESIZE];
    int row, element, hdutype, status, tstatus = 0;
    static char taskname[80] = "ftedit";
    static char version[8] = "1.00";

    /* Register taskname and version. */
    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftedit_getpar(infile, column, &row, value, &element);
    if (status) return(status);

    /* Open the input file. */
    /* Move to first 'interesting' HDU if none was specified. */
    if (fits_open_data(&infptr, infile, READWRITE, &status) ) goto cleanup;
    headas_chat(5,"Opened the following input file:\n %s\n",infile);

    fits_get_hdu_type(infptr, &hdutype, &status);
    if (hdutype == IMAGE_HDU)
        headas_chat(5," This is an IMAGE HDU.\n");
    else
        headas_chat(5," This is a TABLE HDU.\n");

    /* call work function to edit the HDU */

    if (column[0] == '@') {

        /* Case 1: edit commands are given in a text file */
        /* skip the leading '@' character in the file name */
        if (hdutype == IMAGE_HDU)
           status = ftedit_image_file(infptr, column + 1);
        else
           status = ftedit_table_file(infptr, column + 1);

    } else {

        /* Case 2: single edit command given via parameters */
        if (hdutype == IMAGE_HDU)
          status = ftedit_image_par(infptr, column, row, value, element);
        else
          status = ftedit_table_par(infptr, column, row, value, element);
    }

cleanup:

    
    HDpar_stamp(infptr, 0, &status); /* write history keywords */

    /* use tstatus = 0 to properly close the file, even if errors occurred */
    if (infptr) fits_close_file(infptr, &tstatus);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftedit_getpar(
    char *infile,     /* O  Input file name */
    char *column,     /* O  column name, or @filename */
    int *row,         /* O  row number to edit */
    char *value,      /* O  value for element  */
    int *element)     /* O  element number */

/*  read input parameters for the ftedit task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
        sprintf(msg, "Error reading the 'infile' parameter.");
      HD_ERROR_THROW(msg,status);
    }



    else if ((status = PILGetString("column", column))) {
        sprintf(msg, "Error reading the 'column' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if (column[0] != '@') {

    /* 'column' is not the name of a text file, so read remaining parameters */
        if ((status = PILGetInt("row", row))) {
            sprintf(msg, "Error reading the 'row' parameter.");
            HD_ERROR_THROW(msg,status);
        }

        else if ((status = PILGetString("value", value))) {
            sprintf(msg, "Error reading the 'value' parameter.");
            HD_ERROR_THROW(msg,status);
        }

        else if ((status = PILGetInt("element", element))) {
            sprintf(msg, "Error reading the 'element' parameter.");
            HD_ERROR_THROW(msg,status);
        }
    }
    return(status);
}
/*---------------------------------------------------------------------------*/
int ftedit_table_file(
    fitsfile *infptr,  /* I - input file pointer */
    char *editfile)    /* I - edit file name */

/*
    Edit table elements, using text file that lists the column name,
    row number, optional element number, and value for cells
    to be modified.
*/
{
    FILE *template = 0;
    int status = 0, colnum, typecode, ii, jj;
    long repeat, rownum, elemnum = 0, nrows;
    char line[1000], linecopy[1000], *column, *row;
    char *cvalue[1], bvalue[1], *loc, *element, *value, *complx;
    double dvalue[2];
    char msg[MAXMSG];

    /* Open the text file containing the edit instructions */
    if (!(template = fopen(editfile, "r"))) {
        sprintf(msg, "Error: could not open edit template file:\n %s",
                editfile);
        status = FILE_NOT_OPENED;
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }
    headas_chat(5,"\nOpened the following edit template file:\n %s\n",editfile);

    /* get number of rows in table */
    fits_get_num_rows(infptr, &nrows, &status);

    /* process template lines, one by one */
    headas_chat(5,"\nReading template lines:\n");

    for (ii = 1; fgets(line, 1000, template); ii++) {

        /* replace CR and newline chars at end of line with nulls */
        jj = strlen(line) - 1;

        if ( ( jj >= 0) && (line[jj] == '\n' || line[jj] == '\r'))
        {
            line[jj] = '\0';
            jj--;
            if ( ( jj >= 0) && (line[jj] == '\n' || line[jj] == '\r'))
                 line[jj] = '\0';
        }

        headas_chat(5,"%3d:  %s\n",ii,line);

        /* skip comment lines in the template file */
        if (*line == '#') continue;

        /* make a copy, because the 'line' string will */
        /* be chopped up by the strtok function */
        strcpy(linecopy, line);

        /* parse the template string to get column name and row number */
        column  = strtok(line, " ");
        row     = strtok(NULL, " ");

        /* skip blank lines in the template file */
        if (!column) continue;

        /* get column number, datatype, and vector length */
        fits_get_colnum(infptr, CASEINSEN, column, &colnum, &status);
        if (fits_get_coltype(infptr, colnum, &typecode, &repeat, NULL, &status))
            goto cleanup;

        if (typecode < 0) {
            sprintf(msg, 
              "Variable length array columns are not supported: %s",column);
            status = BAD_DATATYPE;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
        }

        /* if repeat > 1 and it is not a string column then this is a vector */
        if ( (abs(typecode) != TSTRING) && (repeat > 1 || typecode < 0) ) {

            /* this is a vector column, so 3rd token is element, 4th is value */
            element = strtok(NULL, " ");
            value = strtok(NULL, " ,");

        } else {

            /* not a vector column; 3rd token is the value */
            value = strtok(NULL, " ,");
            element = 0;
            elemnum = 1;  /* only one element in the row */
        }

        if (!column || !row || !value) { /* must be at least 3 tokens */
            sprintf(msg, 
            "Error parsing 'column', 'row#' and 'value' in line %d of template file:\n%s", ii,linecopy);
            status = PARSE_SYNTAX_ERR;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
        }

        rownum = strtol(row, &loc, 10);  /* read row string as an integer */

        /* check for read error, or junk following the integer */
        if (*loc != '\0' && *loc != ' ' ) {
          sprintf(msg, 
            "Error reading integer row number in line %d:\n%s",
            ii, linecopy);
            status = BAD_C2I;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
        }

        if (rownum > nrows) {
           sprintf(msg, 
            "Row number in line %d is greater than number of rows in the table (%ld):\n%s\n",
            ii, nrows, row);
           status = BAD_ROW_NUM;
           HD_ERROR_THROW(msg,status);
           goto cleanup;
        }

        if (element) {
            elemnum = strtol(element, &loc, 10);  /* read the string as int */
 
           /* check for read error, or junk following the integer */
            if (*loc != '\0' && *loc != ' ' ) {
               sprintf(msg, 
                 "Error reading integer element number in line %d:\n%s\n",
                 ii, element);
               status = BAD_C2I;
               HD_ERROR_THROW(msg,status);
               goto cleanup;
            }
        }

        /* write to the FITS table with the appropriate data type */

        if (tolower(*value) == 'i' || tolower(*value) == 'n') {
          if (!strcmp(value, "INDEF") || !strcmp(value, "indef") ||
            !strcmp(value, "NULL")  || !strcmp(value, "null")  ||
            !strcmp(value, "NAN")   || !strcmp(value, "nan")   ||
            !strcmp(value, "NaN")) {

            /* write a null value to the column */
            fits_write_col_null(infptr, colnum, rownum, elemnum, 1, 
                &status);
         } 
       } else if (abs(typecode) == TLOGICAL) {
            
            *value = tolower(*value);
            if (*value == 'y' || *value == 't' || *value == '1' ) { 
                bvalue[0] = 1;
            } else if (*value == 'n' || *value == 'f' || *value == '0') {
                bvalue[0] = 0;
            } else {
                sprintf(msg,
                  "Error reading value as a logical in line %d: %s\n",
                  ii, value);
                status = PARSE_BAD_TYPE;
                HD_ERROR_THROW(msg,status);
                goto cleanup;
            }

            /* write Boolean value to a logical column */
            fits_write_col(infptr, TLOGICAL, colnum, rownum, elemnum, 1, 
                bvalue, &status);

        } else if (abs(typecode) == TSTRING) {

            /* string values could have embedded spaces */
            /* and strtok would only find the first part, */
            /* so point to the value token in the copy of the line */

            cvalue[0] = linecopy + (value - line);

            /* write string value */
            fits_write_col(infptr, TSTRING, colnum, rownum, elemnum, 1, 
                cvalue, &status);

        } else {

            /* read all numeric values as a double */
            dvalue[0] = strtod(value, &loc);  /* read the string as an double */

            /* check for read error, or junk following the value */
            if (*loc != '\0' && *loc != ' ' && *loc != ',')
            {
               sprintf(msg,
                 "Error reading value as double in line %d: %s\n",
                 ii, value);
               status = BAD_C2D;
               HD_ERROR_THROW(msg,status);
               goto cleanup;
            }

            if (abs(typecode) < TCOMPLEX) {

               fits_write_col(infptr, TDOUBLE, colnum, rownum, elemnum, 1, 
                 dvalue, &status);

            } else {

               /* read the imaginary part of the complex value */
               complx = strtok(NULL, " "); 
               if (!complx) {
                  sprintf(msg, 
                 "Error parsing complex value in line %d of template file:\n%s", 
                  ii, linecopy);
                  status = PARSE_SYNTAX_ERR;
                  HD_ERROR_THROW(msg,status);
                  goto cleanup;
               }
 
               dvalue[1] = strtod(complx, &loc);  /* read the string as an double */

               /* check for read error, or junk following the value */
               if (*loc != '\0' && *loc != ' ')
               {
                 sprintf(msg,
                 "Error reading complex value as double in line %d: %s",
                 ii, complx);
                 status = BAD_C2D;
                 HD_ERROR_THROW(msg,status);
                 goto cleanup;
               }

               fits_write_col(infptr, TDBLCOMPLEX, colnum, rownum, elemnum, 1, 
                  dvalue, &status);

            }
        }

        if (status) goto cleanup;
    }

cleanup:

    /*  close files and go home */
    if (template) fclose(template);
  
    return(status);
}
/*---------------------------------------------------------------------------*/
int ftedit_table_par(
    fitsfile *infptr, /* O - Input file name */
    char *column,     /* name of column to edit */
    int  rownum,      /* row number */
    char *value,      /* value string */
    int  elemnum)     /* element number (for vector columns */
{
/*
    Edit a single table element, as specified by the task parameters.
*/
    int status = 0, colnum, typecode;
    char bvalue[1], *cvalue[1], *loc;
    long repeat, width, nrows;
    double dvalue[2];
    char msg[MAXMSG];

    /* get column number and datatype */
    fits_get_colnum(infptr, CASEINSEN, column, &colnum, &status);
    if (fits_get_coltype(infptr, colnum, &typecode, &repeat, &width, &status) )
            goto cleanup;

    if (typecode < 0) {
            sprintf(msg, 
                "Variable length array columns are not supported.\n");
            status = BAD_DATATYPE;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
    }

    /* get number of rows in table */
    fits_get_num_rows(infptr, &nrows, &status);

    if (rownum > nrows) {
        sprintf(msg, 
          "Row number %d is greater than number of rows in table (%ld).\n",
           rownum, nrows);
           status = BAD_ROW_NUM;
           HD_ERROR_THROW(msg,status);
           goto cleanup;
    }

    /* ignore input elemnum value if this is not a vector column */
    if (repeat == 1 || (typecode == TSTRING && repeat == width) )
       elemnum = 1;

    /* write the column with the appropriate data type */

    if (!strcmp(value, "INDEF") || !strcmp(value, "indef") ||
        !strcmp(value, "NULL")  || !strcmp(value, "null")  ||
        !strcmp(value, "NAN")   || !strcmp(value, "nan")   ||
        !strcmp(value, "NaN")) {

        /* write a null value to the column */
        fits_write_col_null(infptr, colnum, rownum, elemnum, 1, 
                &status);

    } else if (typecode == TLOGICAL) {
            
        *value = tolower(*value);
        if (*value == 'y' || *value == 't' || *value == '1' ) { 
                bvalue[0] = 1;
        } else if (*value == 'n' || *value == 'f' || *value == '0') {
                bvalue[0] = 0;
        } else {
                sprintf(msg,
                  "Error reading value as a logical: %s\n",
                   value);
                status = PARSE_BAD_TYPE;
                HD_ERROR_THROW(msg,status);
                goto cleanup;
        }

        /* write Boolean value to a logical column */
        fits_write_col(infptr, TLOGICAL, colnum, rownum, elemnum, 1, 
                bvalue, &status);

    } else if (typecode == TSTRING) {

        cvalue[0] = value;
        fits_write_col(infptr, TSTRING, colnum, rownum, elemnum, 1, 
                cvalue, &status);

    } else {

        /* read all numeric values as a double */
        dvalue[0] = strtod(value, &loc);  /* read the string as an double */

        /* check for read error, or junk following the value */
        if (*loc != '\0' && *loc != ' ' && *loc != ',')
        {
               sprintf(msg,
                 "Error reading value as double: %s\n",value);
               status = BAD_C2D;
               HD_ERROR_THROW(msg,status);
               goto cleanup;
        }

        if (typecode < TCOMPLEX) {

            fits_write_col(infptr, TDOUBLE, colnum, rownum, elemnum, 1, 
                dvalue, &status);

        } else {

          /*  read the imaginary part of the complex value */
          dvalue[1] = strtod(loc + 1, &loc);

          /* check for read error, or junk following the value */
          if (*loc != '\0' && *loc != ' ' && *loc != ',')
          {
               sprintf(msg,
                 "Error reading value as double: %s\n",value);
               status = BAD_C2D;
               HD_ERROR_THROW(msg,status);

               goto cleanup;
          }

          fits_write_col(infptr, TDBLCOMPLEX, colnum, rownum, elemnum, 1, 
                dvalue, &status);
        }
    }

    if (status) goto cleanup;

    if (repeat == 1 || (typecode == TSTRING && repeat == width) ) {
      headas_chat(5,"Set column '%s', row %d = %s:\n", column, rownum, value);
    }  else  {
      headas_chat(5,"Set column '%s', row %d, vector element %d = %s:\n",
        column, rownum, elemnum, value);
    }

cleanup:

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftedit_image_file(
    fitsfile *infptr,  /* I - input file pointer */
    char *editfile)    /* I - edit file name */
{
/*
    Edit image pixels, using text file that lists the column number,
    row number, and value for pixel to be modified.
*/
    FILE *template = 0;
    int status = 0, naxis, ii, jj;
    long naxes[9], colnum, rownum, fpixel[2];
    char line[1000], linecopy[1000], *column, *row;
    char  *loc, *value;
    double dvalue[1], dnulval = -1.;
    char msg[MAXMSG];

    /* Open the text file containing the edit instructions */
    if (!(template = fopen(editfile, "r"))) {
        sprintf(msg, "Error: could not open edit template file:\n %s\n",
                editfile);
        status = FILE_NOT_OPENED;
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }
    headas_chat(5,"\nOpened the following edit template file:\n %s\n",editfile);

    /* get number of columns and rows in the image */
    fits_get_img_dim(infptr, &naxis, &status);
    fits_get_img_size(infptr, 9, naxes, &status);
    if (naxis == 1) naxes[1] = 1;  /* 1D array has 1 row */

    if (naxis == 0 || naxis > 2) {
        sprintf(msg, 
    "Error: image has %d dimensions; only 1-D or 2-D images are supported.\n",
                naxis);
        status = BAD_NAXIS;
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }

    /* process template lines, one by one */
    headas_chat(5,"\nReading template lines:\n");

    for (ii = 1; fgets(line, 1000, template); ii++) {

        /* replace CR and newline chars at end of line with nulls */
        jj = strlen(line) - 1;

        if ( ( jj >= 0) && (line[jj] == '\n' || line[jj] == '\r'))
        {
            line[jj] = '\0';
            jj--;
            if ( ( jj >= 0) && (line[jj] == '\n' || line[jj] == '\r'))
                 line[jj] = '\0';
        }

        headas_chat(5,"%3d:  %s\n",ii,line);

        /* skip comment lines in the template file */
        if (*line == '#') continue;

        /* make a copy, because the 'line' string will */
        /* be chopped up by the strtok function */
        strcpy(linecopy, line);

        /* parse the template string to get column and row number and value */
        column  = strtok(line, " ");
        row     = strtok(NULL, " ");
        value   = strtok(NULL, " ");

        /* skip blank lines in the template file */
        if (!column) continue;

        if (!column || !row || !value) { /* must be at least 3 tokens */
            sprintf(msg, 
            "Error parsing 'column#', 'row#' and 'value' in line %d of template file:\n%s", 
            ii,linecopy);
            status = PARSE_SYNTAX_ERR;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
        }

        colnum = strtol(column, &loc, 10);  /* read column as an integer */
        /* check for read error, or junk following the integer */
        if (*loc != '\0' && *loc != ' ' ) {
          sprintf(msg, 
            "Error reading integer column number in line %d:\n%s\n",
            ii, linecopy);
            status = BAD_C2I;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
        }

        rownum = strtol(row, &loc, 10);  /* read row string as an integer */
        /* check for read error, or junk following the integer */
        if (*loc != '\0' && *loc != ' ' ) {
          sprintf(msg, 
            "Error reading integer row number in line %d:\n%s\n",
            ii, linecopy);
            status = BAD_C2I;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
        }

        if (colnum < 1 || colnum > naxes[0]) {
           sprintf(msg, 
            "Column number in line %d is out of range: %ld.  Image has %ld columns.\n",
            ii, colnum, naxes[0]);
           status = BAD_COL_NUM;
           HD_ERROR_THROW(msg,status);
           goto cleanup;
        }

        if (rownum < 1 || rownum > naxes[1]) {
           sprintf(msg, 
            "Row number in line %d is out of range: %ld.  Image has %ld rows.\n",
            ii, rownum, naxes[1]);
           status = BAD_ROW_NUM;
           HD_ERROR_THROW(msg,status);
           goto cleanup;
        }

        fpixel[0] = colnum;
        fpixel[1] = rownum;

        /* write to the FITS image */

        if (tolower(*value) == 'i' || tolower(*value) == 'n') {
          if (!strcmp(value, "INDEF") || !strcmp(value, "indef") ||
              !strcmp(value, "NULL")  || !strcmp(value, "null")  ||
              !strcmp(value, "NAN")   || !strcmp(value, "nan")   ||
              !strcmp(value, "NaN")) {

            /* write a null value to the image pixel */
            dvalue[0] = dnulval;
            fits_write_pixnull(infptr, TDOUBLE, fpixel, 1, dvalue,
            &dnulval, &status);
          } 
        } else {     /* read all numeric values as a double */

            dvalue[0] = strtod(value, &loc);  /* read the string as an double */

            /* check for read error, or junk following the value */
            if (*loc != '\0' && *loc != ' ' && *loc != ',')
            {
               sprintf(msg,
                 "Error reading value as double in line %d: %s\n",
                 ii, value);
               status = BAD_C2D;
               HD_ERROR_THROW(msg,status);
               goto cleanup;
            }

            fits_write_pix(infptr, TDOUBLE, fpixel, 1, dvalue, &status);
        }

        if (status) goto cleanup;
    }

cleanup:

    /*  close files and go home */
    if (template) fclose(template);
  
    return(status);
}
/*---------------------------------------------------------------------------*/
int ftedit_image_par(
    fitsfile *infptr, /* O - Input file name */
    char *column,     /* name of column to edit */
    int  rownum,      /* row number */
    char *value,      /* value string */
    int  elemnum)     /* element number (for vector columns */
{
/*
    Edit a single image pixel as specified by the task parameters.
*/
    int status = 0, naxis;
    char *loc;
    long fpixel[2], naxes[9], colnum;
    double dvalue[1], dnulval = -1;
    char msg[MAXMSG];

    /* get number of columns and rows in the image */
    fits_get_img_dim(infptr, &naxis, &status);
    fits_get_img_size(infptr, 9, naxes, &status);
    if (naxis == 1) naxes[1] = 1;  /* 1D array has 1 row */

    if (naxis == 0 || naxis > 2) {
        sprintf(msg, 
    "Error: image has %d dimensions; only 1-D or 2-D images are supported.\n",
                naxis);
        status = BAD_NAXIS;
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }

    colnum = strtol(column, &loc, 10);  /* read column as an integer */
    /* check for read error, or junk following the integer */
    if (*loc != '\0' && *loc != ' ' ) {
          sprintf(msg, 
            "Error reading integer column number: %s\n", column);
            status = BAD_C2I;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
    }

    if (colnum < 1 || colnum > naxes[0]) {
       sprintf(msg, 
       "Column number is out of range: %ld.  Image has %ld columns.\n",
           colnum, naxes[0]);
           status = BAD_COL_NUM;
           HD_ERROR_THROW(msg,status);
           goto cleanup;
    }

    if (rownum < 1 || rownum > naxes[1]) {
           sprintf(msg, 
       "Row number is out of range: %d.  Image has %ld rows.\n",
            rownum, naxes[1]);
           status = BAD_ROW_NUM;
           HD_ERROR_THROW(msg,status);
           goto cleanup;
    }

    fpixel[0] = colnum;
    fpixel[1] = rownum;

    /* write the column with the appropriate data type */

    if (!strcmp(value, "INDEF") || !strcmp(value, "indef") ||
        !strcmp(value, "NULL")  || !strcmp(value, "null")  ||
        !strcmp(value, "NAN")   || !strcmp(value, "nan")   ||
        !strcmp(value, "NaN")) {

        /* write a null value to the image pixel */
        dvalue[0] = dnulval;
        fits_write_pixnull(infptr, TDOUBLE, fpixel, 1, dvalue,
            &dnulval, &status);

    } else {     /* read all numeric values as a double */

        dvalue[0] = strtod(value, &loc);  /* read the string as an double */

        /* check for read error, or junk following the value */
        if (*loc != '\0' && *loc != ' ' && *loc != ',')
        {
            sprintf(msg,"Error reading value as double: %s\n", value);
            status = BAD_C2D;
            HD_ERROR_THROW(msg,status);
            goto cleanup;
        }

        fits_write_pix(infptr, TDOUBLE, fpixel, 1, dvalue, &status);
    }     

    if (status) goto cleanup;

    headas_chat(5,"Set pixel (%ld,%ld) = %s\n",fpixel[0], fpixel[1], value);

cleanup:

    return(status);
}
