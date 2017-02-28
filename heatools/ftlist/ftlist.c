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

#define TOOLSUB ftlist
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftlist (void);
int ftlist_getpar (char *infile, char *printopt, char *outfile, char *include,
		   char *exclude, char *section, char *columns, char
		   *rows, char *vector, char *separ, int *rownum, int
		   *colheader);
int ftlist_work   (char *infile, char *printopt, char *outfile, char *include,
		   char *exclude, char *section, char *columns, char
		   *rows, char *vector, char *separ, int rownum, int
		   colheader);
int ftlist_prkeys  (fitsfile *infptr, FILE *stream, char *incl, char *excl,
                    int *status);
int ftlist_prcols  (fitsfile *infptr, FILE *stream, int ncols, int *status);
int ftlist_prtab  (fitsfile *infptr, FILE *stream, int hdutype, int ncols,
                   LONGLONG nrows, char *columns, char *rows, char *separ,
                   int rownum, char *vector, int colheader, int *status);
int ftlist_primg  (fitsfile *infptr, FILE *stream, int bitpix, int naxis,
                   LONGLONG *naxes, char *separ, int  rownum, char *section,
                   int colheader, int *status);

/*---------------------------------------------------------------------------*/
int ftlist (void)
{
/*  Print out information about the input file and return */
/*  output parameters that describe the HDU structure */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE],printopt[80];
    char rows[PIL_LINESIZE];
    char include[PIL_LINESIZE], exclude[PIL_LINESIZE], section[80];
    char vector[80], separ[40], columns[PIL_LINESIZE];
    int status, rownum, colheader;
    static char taskname[80] = "ftlist";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftlist_getpar(infile, printopt, outfile, include, exclude,
               section, columns, rows, vector, separ, &rownum, &colheader);

    /* call work function to output structural parameters */
    if (!status)
        status = ftlist_work(infile, printopt, outfile, include, exclude,
          section, columns, rows, vector, separ, rownum, colheader);
    return(status);
}
/*---------------------------------------------------------------------------*/
int ftlist_getpar(
    char *infile,    /* O - Input file name */
    char *option,    /* O - output print options */
    char *outfile,   /* O - Output file name */
    char *include,   /* O - included keywords */
    char *exclude,   /* O - excluded keywords */
    char *section,   /* O - image section to print */
    char *columns,   /* O - table columns to print */
    char *rows,      /* O - table rows to print */
    char *vector,    /* O - vector range to print */
    char *separ,     /* O - row separator string */
    int  *rownum,    /* O - print row number? */
    int  *colheader) /* O - print column header? */

/*  read input parameters for the ftlist task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
      sprintf(msg, "Error reading the 'infile' parameter.");
      HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetString("option", option))) {
      sprintf(msg, "Error reading the 'option' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("include", include))) {
      sprintf(msg, "Error reading the 'include' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("exclude", exclude))) {
      sprintf(msg, "Error reading the 'exclude' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("section", section))) {
      sprintf(msg, "Error reading the 'section' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("columns", columns))) {
      sprintf(msg, "Error reading the 'columns' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("rows", rows))) {
      sprintf(msg, "Error reading the 'rows' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("vector", vector))) {
      sprintf(msg, "Error reading the 'vector' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("separator", separ))) {
      sprintf(msg, "Error reading the 'separator' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("rownum", rownum))) {
      sprintf(msg, "Error reading the 'rownum' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("colheader", colheader))) {
      sprintf(msg, "Error reading the 'colheader' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    if (*separ == '\0')
         strcpy(separ, " ");  /* use single space by default */

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftlist_work(
    char *infile,    /* I - Input file name */
    char *printopt,  /* I - output print options */
    char *outfile,   /* I - Output file name */
    char *include,   /* I - included keywords */
    char *exclude,   /* I - excluded keywords */
    char *section,   /* I - image section to print */
    char *columns,   /* I - table columns to print */
    char *rows,      /* I - table rows to print */
    char *vector,    /* I - vector range to print */
    char *separ,     /* I - row separator string */
    int  rownum,     /* I - print row number? */
    int  colheader)  /* I - print column header? */

/*  Print out information about the input file and return */
/*  output parameters that describe the HDU structure */
{
    FILE *stream;
    fitsfile *infptr = 0;
    int status = 0, tstatus, len;
    int hdunum, hdutype, doall = 1, bitpix, naxis, ii, ncols;
    int  prhdu = 0, prcols = 0, prkeys = 0, primage = 0, prtable = 0;
    LONGLONG naxes[20], nrows;
    char name[FLEN_VALUE], type[20], dimens[50], buffer[50], *cptr;
    char msg[MAXMSG];

    /* strip leading and trailing spaces off of 'outfile' */
    cptr = outfile;
    while (*cptr == ' ')cptr++;
    len = strlen(cptr);
    while (len > 0 && *(cptr+len-1) == ' ') {
        len--;
        *(cptr+len) = '\0';
    }

    /* determine output stream (stdout or to a file) */
    if (*cptr == '\0' || !strcmp(cptr, "-") || 
        !strcmp(cptr, "stdout") || !strcmp(cptr, "STDOUT") ) {
        stream = stdout;
    } else {
        if (*cptr == '!') {  /* clobber the output file */
            cptr++;
            remove(cptr);
        } else {
            headas_clobberfile(cptr);  /* delete existing file if clobber=YES */
        }

        stream = fopen(cptr, "r"); /* does file already exist? */

        if (stream)
        {
            fclose(stream);         /* close file and exit with error */
            sprintf(msg,"Error: Output file already exists:\n %s",cptr);
            status = FILE_NOT_CREATED;
            HD_ERROR_THROW(msg,status);
            return(status); 
        }

        if (!(stream = fopen(cptr, "w"))) {
            sprintf(msg,"Error creating output file:\n %s",cptr);
            status = FILE_NOT_CREATED;
            HD_ERROR_THROW(msg,status);
            return(status); 
        }
    }

    /* determine printopt output options */
    if (strchr(printopt, 'h') || strchr(printopt, 'H')) prhdu = 1;
    if (strchr(printopt, 'c') || strchr(printopt, 'C')) prcols = 1;
    if (strchr(printopt, 'k') || strchr(printopt, 'K')) prkeys = 1;
    if (strchr(printopt, 'i') || strchr(printopt, 'I')) primage = 1;
    if (strchr(printopt, 't') || strchr(printopt, 'T')) prtable = 1;

    if (fits_open_file(&infptr, infile, READONLY, &status)) goto cleanup;

    /* determine if a specific extension was entered, or just the filename */
    fits_get_hdu_num(infptr, &hdunum);
    if (hdunum > 1) {
        doall = 0;   /* a specific extension was given */
    } else {
        if (strstr(infile, "+0") || strstr(infile, "[0]") )
           doall = 0;  /* the primary array was specifically given */
    }

    if (prhdu) {
       fprintf(stream, "\n        Name               Type       Dimensions\n");
       fprintf(stream,   "        ----               ----       ----------\n");
    }

     /* main loop over each HDU */

     while (!fits_movabs_hdu(infptr, hdunum, &hdutype, &status) ) {

        naxes[0] = naxes[1] = naxes[2] = naxes[3] = 1;
        nrows = 0;
        ncols = 0;

        fits_get_hdu_type(infptr, &hdutype, &status);  /* Get the HDU type */

        /* try reading the name of the HDU in EXTNAME or HDUNAME keywords */
        name[0] = '\0';
        tstatus = 0;
        if (fits_read_key(infptr, TSTRING, "EXTNAME", name, NULL, &tstatus) ) {
            tstatus = 0;
            fits_read_key(infptr, TSTRING, "HDUNAME", name, NULL, &tstatus);
        }
        if (*name == '\0' && hdunum == 1)
            strcpy(name,"Primary Array");

        if (hdutype == IMAGE_HDU)   /* primary array or image HDU */
        {
            fits_get_img_paramll(infptr, 10, &bitpix, &naxis, naxes, &status);
            if (naxis == 0) {
                strcpy(type, "Null Array");
                dimens[0] = '\0';
                naxes[0] = naxes[1] = naxes[2] = naxes[3] = 0;
            } else {
                strcpy(type, "Image");

                if (bitpix == 8) strcpy(dimens, "Int1(");
                else if (bitpix == 16) strcpy(dimens, "Int2(");
                else if (bitpix == 32) strcpy(dimens, "Int4(");
                else if (bitpix == 64) strcpy(dimens, "Int8(");
                else if (bitpix == -32) strcpy(dimens, "Real4(");
                else if (bitpix == -64) strcpy(dimens, "Real8(");

                /* construct image dimensions string */
                for (ii = 0; ii < naxis; ii++) {
#if (USE_LL_SUFFIX == 1)
                  sprintf(buffer, "%lld",naxes[ii]);
#else
                  sprintf(buffer, "%ld",naxes[ii]);
#endif
                  strcat(dimens, buffer);
                  if (ii < naxis - 1)
                    strcat(dimens, "x");
                }
                strcat(dimens, ")");
            }
        }
        else  /* a table HDU */
        {

            bitpix = 8;
            naxis = 2;
            if (hdutype == ASCII_TBL)
              strcpy(type, "Table");
            else
              strcpy(type, "BinTable");

            fits_get_num_rowsll(infptr, &nrows, &status);
            fits_get_num_cols(infptr, &ncols, &status);
#if (USE_LL_SUFFIX == 1)
            sprintf(dimens,"%3d cols x %lld rows", ncols, nrows);
#else
            sprintf(dimens,"%3d cols x %ld rows", ncols, nrows);
#endif

            fits_read_key(infptr, TLONGLONG, "NAXIS1", naxes, NULL, &status);
            naxes[1] = nrows;
        }

        /* *************** */
        /*  Print HDU info */
        /* *************** */
        if (prhdu)
           fprintf(stream, "HDU %-3d %-18s %-10s %-30s\n",
                  hdunum, name, type, dimens);

        /* ****************** */
        /*  Print Column info */
        /* ****************** */
        if (prcols && hdutype != IMAGE_HDU ) {
            if (!prhdu)
                fprintf(stream, "HDU %-3d\n",hdunum); /* print brief header */
            if (ftlist_prcols(infptr, stream, ncols, &status)) goto cleanup;
        }

        /* **************** */
        /*  Print Keywords  */
        /* **************** */
        if (prkeys) {

          if (!prhdu && !prcols)
	    /* just print the raw keywords in this case, without */
	    /* any header or separator text */
	    tstatus=0;
            /*   fprintf(stream, "HDU %-3d\n\n",hdunum); */
          else
              fprintf(stream, "\n");

          if (ftlist_prkeys(infptr, stream, include, exclude, &status))
              goto cleanup;
        }

        /* ************* */
        /*  Print Image  */
        /* ************* */
        if (primage && hdutype == IMAGE_HDU && naxis <= 2) {

              if (ftlist_primg(infptr, stream, bitpix, naxis, naxes, separ, 
                  rownum, section, colheader, &status))
                  goto cleanup;
        }

        /* ************* */
        /*  Print Table  */
        /* ************* */
        if (prtable && hdutype != IMAGE_HDU) {

              if (ftlist_prtab(infptr, stream, hdutype, ncols, nrows, columns,
                  rows, separ, rownum, vector, colheader, &status))
                  goto cleanup;
        }
        
        hdunum++;

        if (!doall) goto cleanup;  /* exit if only doing one HDU? */

        if (prkeys)  /* print HDU separator line only if keywords were listed */
          fprintf(stream, 
   "-----------------------------------------------------------------------\n");

        if (hdutype == IMAGE_HDU && prkeys)
          fprintf(stream, "\n");  /* add a blank line for visual clarity */
        else if (hdutype != IMAGE_HDU && (prkeys || prcols))
          fprintf(stream, "\n");  /* add a blank line for visual clarity */
    }

cleanup:

    if (status == END_OF_FILE) status = 0; /* reset normal error */

    if (infptr)  fits_close_file(infptr,  &status);

    return(status);  
}
/*---------------------------------------------------------------------------*/
int ftlist_prkeys(
    fitsfile *fptr,   /* I - Input file ptr */
    FILE *stream,     /* I - Output file stream */
    char *include,    /* I - included keywords */
    char *exclude,    /* I - excluded keywords */
    int *status)      /* O - status */
{
/*  print header keywords HDU */
    int len, nincl = 0, nexcl = 0, printend = 1;
    char *inclist[100], kincl[100][20], kexcl[100][20], *exclist[100], *cptr;
    char card[FLEN_CARD];


    /* parse the include keyword list */
    cptr = include;
    while (*cptr == ' ')cptr++;  /* skip leading spaces */

    while ((len = strcspn(cptr, ","))) {

       if (len < 20 && nincl < 100) {
           inclist[nincl] =  kincl[nincl];

           strncpy(inclist[nincl], cptr, len);
           inclist[nincl][len] = '\0'; 
           nincl++;
           if (len > 1 || *cptr != '*')
               printend = 0;
       }

       cptr += len;
       if (*cptr == ',')cptr++;
       while (*cptr == ' ')cptr++;  /* skip leading spaces */
    }

    if (nincl == 0) {
        inclist[0l] = kincl[0];
        strcpy(inclist[0], "*");   /* list all keywords by default */
        nincl = 1;
    } 

    /* parse the exclude keyword list */
    cptr = exclude;
    while (*cptr == ' ')cptr++;  /* skip leading spaces */

    while ((len = strcspn(cptr, ","))) {
       if (len < 20 && nexcl < 100) {
           exclist[nexcl] =  kexcl[nexcl];

           strncpy(exclist[nexcl], cptr, len);
           exclist[nexcl][len] = '\0';
           nexcl++;
           printend = 0;
       }

       cptr += len;
       if (*cptr == ',')cptr++;
       while (*cptr == ' ')cptr++;  /* skip leading spaces */
    }

    
    fits_read_record(fptr, 0, card, status); /* move to top of header */
    while (!fits_find_nextkey(fptr, inclist, nincl, exclist, nexcl, 
                              card, status)) {

            fprintf(stream, "%s\n", card);
    }

    if (printend)                   /* print END card if include  */
        fprintf(stream, "END\n");   /* and exclude are null */

    if (*status == KEY_NO_EXIST) {
        *status = 0;        /* reset normal error status */
    }

    return(*status);
}
/*---------------------------------------------------------------------------*/
int ftlist_prcols(
    fitsfile *fptr,   /* I - Input file ptr */
    FILE *stream,     /* I - Output file stream */
    int ncols,        /* I - number of columns */
    int *status)      /* O - status */
{
/*  print column names, formats, units, and ranges in the input table HDU */

    int ii, tstatus;
    char keyname[20], colname[FLEN_VALUE], comment[FLEN_COMMENT];
    char coltype[FLEN_VALUE], colunit[FLEN_VALUE], colinfo[200];
    char tlmin[FLEN_VALUE], tlmax[FLEN_VALUE];

    fprintf(stream, 
         "\n  Col  Name             Format[Units](Range)      Comment\n");

    for (ii = 1; ii <= ncols; ii++)
    {
        fits_make_keyn("TTYPE", ii, keyname, status); /* make keyword */
        colname[0] = '\0';
        tstatus = 0;
        fits_read_key(fptr, TSTRING, keyname, colname, comment, &tstatus);

        fits_make_keyn("TFORM", ii, keyname, status); /* make keyword */
        fits_read_key(fptr, TSTRING, keyname, coltype, NULL, status);

        fits_make_keyn("TUNIT", ii, keyname, status); /* make keyword */
        colunit[0] = '\0';
        tstatus = 0;
        fits_read_key(fptr, TSTRING, keyname, colunit, NULL, &tstatus);

        fits_make_keyn("TLMIN", ii, keyname, status); /* make keyword */
        tlmin[0] = '\0';
        tstatus = 0;
        fits_read_key(fptr, TSTRING, keyname, tlmin, NULL, &tstatus);

        fits_make_keyn("TLMAX", ii, keyname, status); /* make keyword */
        tlmax[0] = '\0';
        tstatus = 0;
        fits_read_key(fptr, TSTRING, keyname, tlmax, NULL, &tstatus);

        strcpy(colinfo, coltype);

        if (*colunit) {
            strcat(colinfo, " [");
            strcat(colinfo, colunit);
            strcat(colinfo, "]");
        }

        if (*tlmin || * tlmax) {
            strcat(colinfo, " (");
            strcat(colinfo, tlmin);
            strcat(colinfo, ":");
            strcat(colinfo, tlmax);
            strcat(colinfo, ")");
        }

        fprintf(stream, "%5d %-18s %-20s %s\n", ii, colname, colinfo, comment);
    }
    return(*status);
}
/*---------------------------------------------------------------------------*/
int ftlist_prtab(
    fitsfile *fptr,   /* I - Input file ptr */
    FILE *stream,     /* I - Output file stream */
    int   hdutype,    /* I - type of table, ASCII or Binary */
    int   ncols,      /* I - number of columns in table */
    LONGLONG nrows,   /* I - number of rows to print */
    char *columns,    /* I - list of columns to print */
    char *rows,       /* I - list of rows or row ranges to print */
    char *separ,      /* I - column separator string */
    int  rownum,      /* I - print row number? */
    char *vector,     /* I - range of vector elements to print */
    int  colheader,   /* I - print column header (name and units) ? */
    int *status)      /* O - status */
{
/*  print specfied rows and columns in the input table HDU */

    int MAXROWS = 1000, varicol = 0, tstatus, pcols, colnum[999], len;
    int  rowwidth, *dispwidth, ii, kk, anynul, nranges, *coltype;
    long jj, ll, lenstr;
    LONGLONG firstrow, firstelem, remain, todo, maxr;
    LONGLONG *minrow = 0, *maxrow = 0;
    long *veclen = 0, maxvec = 1, savmaxvec = 1;
    long minelem, maxelem;
    char keyword[20], colname[FLEN_VALUE], nullstr[8]="NULL";
    char ***cval = 0, *cptr; 
    char msg[MAXMSG];

    if (ncols == 0 || nrows == 0)return(*status);

    /* get the column numbers to be printed */
    cptr = columns;
    while (*cptr == ' ')cptr++;  /* skip leading spaces */

    if (*cptr == '\0' || *cptr == '-') {

        /* print every column by default */
        pcols = ncols;
        for (ii = 0; ii < pcols; ii++) {
            colnum[ii] = ii + 1;
        }

    } else {

        /* parse the column name list */
        pcols = 0;
        while ((len = strcspn(cptr, ","))) {
          if (len < FLEN_VALUE) {
            *colname='\0';
            strncat(colname, cptr, len);

            while (fits_get_colnum(fptr, CASEINSEN, colname, &colnum[pcols],
                        status) == COL_NOT_UNIQUE) {
                pcols++;
            }

            if (*status <= 0) {
                pcols++;
            } else if (*status == COL_NOT_FOUND) {
                *status = 0; /* ignore non-existent column names */
            }

            cptr += len;
            if (*cptr == ',')cptr++;
            while (*cptr == ' ')cptr++;  /* skip leading spaces */
          }
        }
    }

    if (pcols == 0) return(*status);  /* no columns to print */

    /* attempt to parse the 'vector' range:  format = "12-50" */
    tstatus = 0;
    cptr = strchr(vector, ':');
    if (cptr)
        *cptr = '-';  /* convert 1:5 to 1-5 */

    if (ffrwrg(vector, 1000000000, 1, &nranges, &minelem, &maxelem, &tstatus)) {
         /* failed to interpret the range, so print entire range */
         minelem = 1;
         maxelem = 1000000000;
    }

    /* allocate arrays for datatype and vector length of each column */
    veclen = calloc(pcols, sizeof(long)); 
    coltype = calloc(pcols,sizeof(int));
    dispwidth = calloc(pcols,sizeof(int));

    if (!veclen || !coltype || !dispwidth) {
        sprintf(msg, "ERROR: failed to allocate memory for column info.\n");
        *status = MEMORY_ALLOCATION;
        HD_ERROR_THROW(msg,*status);
        goto cleanup;
    }

    /* get vector length of each column in binary tables */
    if (hdutype == BINARY_TBL) {
        for (ii = 0; ii < pcols; ii++) {

            if (fits_get_coltype(fptr, colnum[ii], &coltype[ii], &veclen[ii], 
                NULL, status))
                goto cleanup;
        
            if (coltype[ii] < 0) {     /* variable length column? */
                varicol = 1;
                veclen[ii] = 1;  /* determine actual length of each row later */
            }
        
            if (coltype[ii] == TSTRING)
                veclen[ii] = 1;  /* a string is considered a single element */
            else if (coltype[ii] == TBIT)
                veclen[ii] = (veclen[ii] - 1)/8 + 1; /* Treat '8X' like '1B' */

            savmaxvec = veclen[ii] > savmaxvec ? veclen[ii] : savmaxvec;
        }

        if (savmaxvec > 1 || varicol)  
            MAXROWS = 1;  /* must process file one row at a time */
    }
    else {
        for (ii = 0; ii < pcols; ii++)
            veclen[ii] = 1;  /* ASCII tables have vector length = 1 */
    }
 

    /* allocate a top-level pointer for each column */
    cval = calloc(pcols, sizeof(char*)); 
    if (!(cval)) {
        sprintf(msg, "ERROR: failed to allocate column pointers.\n");
        *status = MEMORY_ALLOCATION;
        HD_ERROR_THROW(msg,*status);
        goto cleanup;
    }

    /* find how many ranges were specified ( = no. of commas in string + 1) */
    for (cptr = rows, nranges = 1; (cptr = strchr(cptr, ',')); nranges++)
        cptr++;
 
    /* allocate arrays for min and max values for each range */
    minrow = calloc(nranges, sizeof(LONGLONG));
    maxrow = calloc(nranges, sizeof(LONGLONG));

    if (!minrow || !maxrow) {
        *status = MEMORY_ALLOCATION;
        sprintf(msg, "ERROR: failed to allocate memory for row ranges.\n");
        HD_ERROR_THROW(msg,*status);
        goto cleanup;
    }

    /* parse range list into array of range min and max values */
    if (ffrwrgll(rows, nrows, nranges, &nranges, minrow, maxrow, status)) {
        goto cleanup;
    }

    for (ii = 0; ii < pcols; ii++) {

        if (coltype[ii] == -TSTRING) { /* variable length ASCII column? */

            /* have to scan descriptors to get maximum string length */
            dispwidth[ii] = 1;
            for (kk = 0; kk < nranges; kk++) {
                for (jj = minrow[kk]; jj <= maxrow[kk]; jj++) {


                    if (fits_read_descript(fptr, colnum[ii], jj,
                      &lenstr, NULL, status)) goto cleanup;

                     dispwidth[ii] = lenstr > dispwidth[ii] ? 
                                     lenstr : dispwidth[ii];
                }
            }
        } else
           fits_get_col_display_width(fptr, colnum[ii], &dispwidth[ii], status);

        /* allocate a pointer to each of the MAXROWS rows */
        cval[ii] = calloc(MAXROWS, sizeof(char*)); /* char* for each row */
        if (!(cval[ii])) {
            sprintf(msg, "ERROR: failed to allocate row pointers.\n");
            *status = MEMORY_ALLOCATION;
            HD_ERROR_THROW(msg,*status);
            goto cleanup;
        }

        /* allocate big block for the actual element value strings */
        cval[ii][0] = calloc(dispwidth[ii] + 1, MAXROWS);
        if (!(cval[ii][0])) {
            sprintf(msg, "ERROR: failed to allocate row memory.\n");
            *status = MEMORY_ALLOCATION;
            HD_ERROR_THROW(msg,*status);
            goto cleanup;
        }

        /* initialize the other pointer addresses into the big memory block */
        for (jj = 1; jj < MAXROWS; jj++) {
            cval[ii][jj] = cval[ii][jj-1] + dispwidth[ii] + 1;
        }
    }

    /* width of the row number field */
    if (rownum) {
      maxr = maxrow[nranges - 1];
      if (maxr < 1000)
        rowwidth = 3;
      else if (maxr < 10000)
        rowwidth = 4;
      else if (maxr < 100000)
        rowwidth = 5;
      else if (maxr < 1000000)
        rowwidth = 6;
      else
        rowwidth = 7;
    } else
      rowwidth = 0;

    /* print column names as column headers */

    if (colheader) {  /* print column header (name and units ) */
        fprintf(stream, "\n");
        if (rowwidth) {
            fprintf(stream, "%*s",rowwidth," ");
            fprintf(stream, "%s",separ);
        }

        for (ii = 0; ii < pcols; ii++) {
            fits_make_keyn("TTYPE", colnum[ii], keyword, status);
            if (fits_read_key(fptr, TSTRING, keyword, colname, NULL, status))
                goto cleanup;

            /* truncate long names to the width of the data field */
            colname[dispwidth[ii] >= FLEN_VALUE?FLEN_VALUE-1:dispwidth[ii]]
                 = '\0';
            fprintf(stream, "%*s",dispwidth[ii], colname);
            if (ii != pcols -1)
                fprintf(stream, "%s",separ);
        }
        fprintf(stream, "\n");  /* terminate header line */

        /* print column units as column headers */
        if (rowwidth) {
            fprintf(stream, "%*s",rowwidth," ");
            fprintf(stream, "%s",separ);
        }

        for (ii = 0; ii < pcols; ii++) {
            fits_make_keyn("TUNIT", colnum[ii], keyword, status);
            tstatus = 0;
            colname[0] = '\0';
            fits_read_key(fptr, TSTRING, keyword, colname, NULL, &tstatus);

            /* truncate long units to the width of the data field */
            colname[dispwidth[ii] >= FLEN_VALUE?FLEN_VALUE-1:dispwidth[ii]]
                 = '\0';
            fprintf(stream, "%*s",dispwidth[ii], colname);
            if (ii != pcols -1)
                fprintf(stream, "%s",separ);
        }
        fprintf(stream, "\n");  /* terminate header line */
    }

    /* loop over each row range */
    for (kk = 0; kk < nranges; kk++) {
      remain = maxrow[kk] - minrow[kk] + 1;

      todo = remain > MAXROWS ? MAXROWS : remain;
      firstrow = minrow[kk];
      firstelem = 1;

      /* main loop; read the the next 'todo' rows at one time */
      while(remain) {
        maxvec = savmaxvec;
        firstelem = 1;

        if (varicol) { /* get length of variable length arrays in this row */
           for (ii = 0; ii < pcols; ii++) {
              if (coltype[ii] < 0) {
                 if (fits_read_descript(fptr, colnum[ii], firstrow,
                      &veclen[ii], NULL, status)) goto cleanup;

                if (coltype[ii] == -TSTRING)
                  veclen[ii] = 1;  /* a string is considered a single element */
                else if (coltype[ii] == -TBIT)
                  veclen[ii]= (veclen[ii] - 1)/8 + 1; /* Treat '8X' like '1B' */

                maxvec = veclen[ii] > maxvec ? veclen[ii]:maxvec;
              }
           }
        }

        /* loop over each element of tables with vector columns */
        for (ll = 0; ll < maxvec; ll++, firstelem++) { 

          if (ll < minelem - 1 || ll >= maxelem) continue;

          /* loop over columns in the row */
          for (ii = 0; ii < pcols; ii++) {

            **cval[ii] = '\0';
            if (veclen[ii] >= firstelem) {
              /* read values as a string, regardless of intrinsic datatype */

              if (fits_read_col_str(fptr, colnum[ii], firstrow, firstelem, todo,
                nullstr, cval[ii], &anynul, status)) goto cleanup;
            }
          }
                  
          for (jj = 0; jj < todo; jj++) {

            /* print row number only for first element of vector */
            if (rowwidth) {

                if (firstelem == minelem)
#if (USE_LL_SUFFIX == 1)
                    fprintf(stream, "%*lld", rowwidth,jj + firstrow);
#else
                    fprintf(stream, "%*ld", rowwidth,jj + firstrow);
#endif
                else
                    fprintf(stream, "%*s", rowwidth,"");

                fprintf(stream, "%s",separ);
            }

            for (ii = 0; ii < pcols; ii++) {
                fprintf(stream, "%-*s",dispwidth[ii], cval[ii][jj]);
                if (ii != pcols - 1)
                    fprintf(stream, "%s",separ);
            }
            fprintf(stream, "\n");  /* terminate row */
          }
        }  /* end element loop */
        
        remain -= todo;
        firstrow += todo;
        todo = remain > MAXROWS ? MAXROWS:remain;
      }
    }  /* end loop ever ranges */

cleanup:

    /* free memory, in reverse order */
    if (maxrow) free(maxrow);
    if (minrow) free(minrow);

    if (cval) {
        for (ii = pcols - 1; ii >= 0; ii--) {
            if (cval[ii]) {
                if (cval[ii][0]) free(cval[ii][0]);

                free(cval[ii]);
            }
        }
        free(cval);
    }
    if (dispwidth) free(dispwidth);
    if (coltype) free(coltype);
    if (veclen) free(veclen);

    return(*status);
}
/*---------------------------------------------------------------------------*/
int ftlist_primg(
    fitsfile *fptr,   /* I - Input file ptr */
    FILE *stream,     /* I - Output file stream */
    int bitpix,       /* I - image datatype */
    int naxis,        /* I - no. of dimensions */
    LONGLONG *naxes,  /* I - size of dimensions */
    char *separ,      /* I - column separator string */
    int  rownum,      /* I - print row number? */
    char *section,    /* I - column separator string */
    int  colheader,   /* I - print column number? */
    int *status)      /* O - status */
{
/*  print pixel values in an image HDU */

    int ii, tstatus=0, scaled=0, incr, anynull;
    long fpixel[9] = {1,1,1,1,1,1,1,1,1};
    double *pixels = 0, bscale;
    char *nullarray = 0;
    char format[20], hdformat[20], nullformat[20], *cptr;
    long xmin, xmax, ymin, ymax, dummy, nread, rstop;
    LONGLONG *llpixels = 0;

    if (naxis == 0) return(*status);
    if (naxis == 1) naxes[1] = 1;

    /* get memory for 1 row */
    nullarray = (char *)   malloc(naxes[0]);

    if (bitpix == 64) {
      llpixels = (LONGLONG *) malloc(naxes[0] * sizeof(LONGLONG));

      if (llpixels == NULL || nullarray == NULL) {
         *status = MEMORY_ALLOCATION;
         goto cleanup;
      }
    } else {
      pixels    = (double *) malloc(naxes[0] * sizeof(double));
      if (pixels == NULL || nullarray == NULL) {
         *status = MEMORY_ALLOCATION;
         goto cleanup;
      }
    }

    /* get image section to print */
    cptr = section;
    tstatus = 0;
    if (fits_get_section_range(&cptr, &xmin, &xmax, &dummy, &tstatus) ) {
        /* failed to parse the x range */
        xmin = 1;
        xmax = naxes[0];
    }
    if (xmin == 1 && xmax == 0) {
        xmin = 1;
        xmax = naxes[0];
    }

    tstatus = 0;
    if (fits_get_section_range(&cptr, &ymin, &ymax, &dummy, &tstatus) ) {
        /* failed to parse the y range */
        ymin = 1;
        ymax = naxes[1];
    }
    if (ymin == 1 && ymax == 0) {
        ymin = 1;
        ymax = naxes[1];
    }

    if (xmax > naxes[0]) xmax = naxes[0];
    if (xmin > xmax) xmin = xmax;
    if (ymax > naxes[1]) ymax = naxes[1];
    if (ymin > naxes[1]) ymin = naxes[1]; 
    nread = xmax - xmin +1;
    fpixel[0] = xmin;

    tstatus = 0;
    if (!ffgkyd(fptr, "BSCALE", &bscale, NULL, &tstatus))
    {
        if (bscale != 1.0)
            scaled = 1;    /* yes, this is a scaled integer image */
    }

    if (scaled || bitpix < 0)  {
        strcpy(hdformat,   "%14d");
        strcpy(format,     "%#14.6G");
        strcpy(nullformat, "%14s");
   } else if (bitpix == 8) {
        strcpy(hdformat,   "%4d");
        strcpy(format,     "%4.0f");
        strcpy(nullformat, "%4s");
   } else if (bitpix == 16) {
        strcpy(hdformat,   "%6d");
        strcpy(format,     "%6.0f");
        strcpy(nullformat, "%6s");
   } else if (bitpix == 64) {
        strcpy(hdformat,   "%20d");

#if defined(_MSC_VER)
    /* Microsoft Visual C++ 6.0 uses '%I64d' syntax  for 8-byte integers */
        strcpy(format,     "%20I64d");
#elif (USE_LL_SUFFIX == 1)
        strcpy(format,     "%20lld");
#else
        strcpy(format,     "%20ld");
#endif

        strcpy(nullformat, "%20s");
   } else {
        strcpy(hdformat,   "%11d");
        strcpy(format,     "%11.0f");
        strcpy(nullformat, "%11s");
    }


    if (colheader) {   /* print column header */
        fprintf(stream, "\n");
        if (rownum) {
            fprintf(stream, "     ");
            fprintf(stream, "%s",separ);
        }
   
        for (ii = xmin; ii <= xmax; ii++) {
            fprintf(stream, hdformat, ii);
            if (ii != xmax)
                fprintf(stream, "%s",separ);
        }

        fprintf(stream, "\n");                /* terminate header line */
    }

    if (ymax >= ymin) {
        incr = -1;
        rstop = ymin - 1;
    } else {
        incr = 1;
        rstop = ymin + 1;
    }

    /* loop over  the rows in the image, top to bottom */
    for (fpixel[1] = ymax; fpixel[1] != rstop; fpixel[1] += incr)
    {

        if (bitpix == 64) {
          if (fits_read_pixnull(fptr, TLONGLONG, fpixel, nread,
            llpixels, nullarray, &anynull, status) )  /* read row of pixels */
                  goto cleanup;  /* jump out of loop on error */
        } else {
          if (fits_read_pixnull(fptr, TDOUBLE, fpixel, nread,
            pixels, nullarray, &anynull, status) )  /* read row of pixels */
                  goto cleanup;  /* jump out of loop on error */
        }

        if (rownum) {
            fprintf(stream, " %4ld",fpixel[1]);         /* print row number */
            fprintf(stream, "%s",separ);
        }

        for (ii = 0; ii <= (xmax - xmin); ii++) {
            if (nullarray[ii]) {
                fprintf(stream, nullformat, "Null");   /* print Null  */
            } else {
                if (bitpix == 64) {
                  fprintf(stream, format, llpixels[ii]);   /* print each value  */
                } else {
                  fprintf(stream, format, pixels[ii]);   /* print each value  */
                }
            }
 
            if (ii != (xmax - xmin))
                fprintf(stream, "%s",separ);
            
        }

        fprintf(stream, "\n");                      /* terminate line */
    }

cleanup:

    if (pixels) free(pixels);
    if (llpixels) free(llpixels);
    if (nullarray) free(nullarray);
    return(*status);
}
