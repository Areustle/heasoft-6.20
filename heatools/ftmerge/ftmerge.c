#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"

#define MAXKEYS 256
#define MAXMSG 256

/*
HISTORY
-------
  Version 1.0 written by William Pence, NASA/GSFC, May 2002
  Version 2.0 Bryan Irby, NASA/GSFC, Feb 2006
              Added 'lastkey' parameter and modified to allow '@' syntax
              for a columns file.  New function ftmerge_get_list (adapted
	      from ftdiff's get_keylist) is used to parse both the columns
	      and lastkey parameters.
  Version 2.1 Bryan Irby, NASA/GSFC, Mar 2006
              Added 'copyall' parameter.
  Version 2.2 Bryan Irby, NASA/GSFC, Dec 2009
              Added 'insertrow' parameter.
*/

#define TOOLSUB ftmerge
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftmerge (void);
int ftmerge_getpar (char *infile, char *outfile, char *columns, long *insertrow, char *lastkey, int *copyall, int *skipbadfiles);
int ftmerge_work   (char *infile, char *outfile, char *columns, long insertrow, char *lastkey, int copyall, int skipbadfiles);
void ftmerge_get_list (char *parameter_string, char **ptoken_list, int ptoken_len, int *nptokens);

/*---------------------------------------------------------------------------*/
int ftmerge (void)
{
/*  merge all the rows in the input tables into a single output table */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    char columns[PIL_LINESIZE], lastkey[PIL_LINESIZE];
    int copyall, status, skipbadfiles;
    long insertrow;
    static char taskname[80] = "ftmerge";
    static char version[8] = "2.20";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftmerge_getpar(infile, outfile, columns, &insertrow, lastkey,
		    &copyall, &skipbadfiles);

    /* call work function to merge the input HDU to the output file */
    if (!status)
        status = ftmerge_work(infile, outfile, columns, insertrow, lastkey,
			copyall, skipbadfiles);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftmerge_getpar(
    char *infile,   /* O - Input file name */
    char *outfile,  /* O - Output file name */
    char *columns,  /* O - Columns to be merged */
    long *insertrow,/* O - Row number in first input table after which
		           remaining input tables will be inserted */
    char *lastkey,  /* O - Keywords to be copied from last input file */
    int  *copyall,  /* O - copy all HDUs from input file? */
    int  *skipbadfiles)  /* O - skip any files that cannot be opened? */

/*  read input parameters for the ftmerge task from the .par file */
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

    else if ((status = PILGetString("columns", columns) )) {
        sprintf(msg, "Error reading 'columns' parameter in .par file");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetLong("insertrow", insertrow) )) {
        sprintf(msg, "Error reading 'insertrow' parameter in .par file");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("lastkey", lastkey) )) {
        sprintf(msg, "Error reading 'lastkey' parameter in .par file");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("copyall", copyall))) {
        sprintf(msg, "Error reading 'copyall' parameter in .par file");
        HD_ERROR_THROW(msg,status);
    }
    
    else if ((status = PILGetBool("skipbadfiles", skipbadfiles))) {
      sprintf(msg, "Error reading the 'skipbadfiles' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftmerge_work(
    char *infile,   /* I - Input file name */
    char *outfile,  /* I - Output file name */
    char *columns,  /* I - Columns to be merged */
    long insertrow, /* I - Row number in first input table after which
		           remaining input tables will be inserted */
    char *lastkey,  /* I - Keywords to be copied from last input file */
    int  copyall,   /* I - copy all HDUs from input file? */
    int  skipbadfiles)  /* I - skip any files that cannot be opened? */

/*  merge all the rows in the input tables to a single output table */
{
    FILE *textfile = 0;
    FILE *columnsfile = 0;
    fitsfile *firstfptr = 0, *lastfptr = 0, *infptr = 0, *outfptr = 0;
    int status = 0, icol, outcols, incols, intype, outtype;
    int buffsize = 100000;
    long outlen, inlen, inrep, outrep, ntodo, inrow, outrow, maxrows, nrows;
    long remaining;
    unsigned char *bptr = 0;
    char *p = 0, *q = 0;
    char filename[PIL_LINESIZE], unfiltered_filename[PIL_LINESIZE];
    char msg[MAXMSG];

    char lastkey_card[MAXKEYS];
    char *lastkey_list[MAXKEYS];
    char *columns_list[MAXKEYS];
    int nlastkeys = 0, ncolumns = 0;
    int i;
    long numrows, nextrow;

    numrows = 0;
    nextrow = 0;

    if (*infile == '@') { /* open text file containing list of files? */
       
        if( (textfile = fopen(infile+1, "r" ))==NULL ) {
            sprintf(msg,"Could not open ASCII file %s.",infile+1);
            status = FILE_NOT_OPENED;
            HD_ERROR_THROW(msg,status);
            return(status);
        }
        headas_chat(5,"Opened list of input files:\n  %s\n", infile+1);

        if ( !fgets(filename, PIL_LINESIZE-1, textfile)) {
            sprintf(msg,"File list is empty: %s.",infile+1);
            status = EOF;
            HD_ERROR_THROW(msg,status);
            goto cleanup; /* EOF */
        }
        if ((p = strchr(filename, '\n'))) *p = 0; /* remove EOL  */
        if ((p = strchr(filename, '\r'))) *p = 0; 

    } else {
        p = fits_split_names(infile);
        if (!p) {
           sprintf(msg, "Must have at least 1 input file to merge.");
           status = EOF; 
           HD_ERROR_THROW(msg,status);
           goto cleanup;
        }
        strcpy(filename, p);
    }    

    /* get list of columns */
    for (i = 0; i < MAXKEYS; i++)
	columns_list[i] = (char *)calloc(FLEN_VALUE+1,sizeof(char));
    ncolumns = 0;  
    ftmerge_get_list(columns, columns_list, FLEN_VALUE, &ncolumns);

    /* add column filter to base filename */
    if ( *columns != '\0' && *columns != '*' &&
       ((*columns != '-') || (columns[1] != '\0')) ) {
       strcat(filename, "[col ");
       for (i = 0; i<ncolumns; i++) {
           strcat(filename, columns_list[i]);
           if (i != ncolumns-1) {
              strcat(filename, ";");
	   }
       }
       strcat(filename, "]");
    } else {
       *columns = '\0';
    }

    if (fits_open_table(&firstfptr, filename, READONLY, &status)) {
        sprintf(msg,"Unable to open first FITS table: %s.",filename);
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }
    headas_chat(5,"Opened 1st input table:\n  %s\n", filename);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    if (fits_create_file(&outfptr, outfile, &status)) {
        sprintf(msg,"Unable to create output file: %s.",outfile);
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }
    headas_chat(5,"Created the output file:\n  %s\n", outfile);

    if (copyall) { /* copying all HDUs? */

       if (fits_copy_file(firstfptr, outfptr, 1, 1, 0, &status)) goto cleanup;
       headas_chat(5, "Copied preceding HDUs to the output file.\n");

    } else { /* just copy the current HDU */

       if (fits_copy_hdu(firstfptr, outfptr, 0, &status)) goto cleanup;
       headas_chat(5, "Copied the current HDU to the output file.\n");
    }

    /* get parameters about the output table */
    fits_get_num_cols(outfptr, &outcols, &status);
    fits_get_num_rows(outfptr, &outrow, &status);

    if (insertrow) {
       headas_chat(5, "  Copied %ld rows from 1st input table to the output table.\n", insertrow);
       numrows = outrow;
       outrow = insertrow + 1;
       nextrow = outrow; /* Where to pick up in 1st input file afterwards */
    } else {
       headas_chat(5, "  Copied %ld rows from 1st input table to the output table.\n", outrow);
       outrow = outrow + 1;
    }

    if (fits_read_key(outfptr, TLONG, "NAXIS1", &outlen, NULL, &status))
         goto cleanup;

    /* allocate buffer to hold rows of the table */
    maxrows = buffsize / outlen;
    if (maxrows == 0) maxrows = 1;
    buffsize = maxrows * outlen;

    if (!(bptr = (unsigned char *) malloc(buffsize)) ) {
        status = MEMORY_ALLOCATION;
        goto cleanup;
    }
    
    while(1) {  /* loop over all additional input files in the list */

       if (textfile) {  /* get next file name from text file */
          if ( !fgets(filename, PIL_LINESIZE-1, textfile)) break;
          if ((p = strchr(filename, '\n'))) *p = 0; /* remove EOL  */
          if ((p = strchr(filename, '\r'))) *p = 0; 

       } else { /* get next name from comma or space-separated list */
          q = fits_split_names(0); /* store pointer to next name, if any */
          if (!q)break;
          strcpy(filename, q);
       }

       strcpy(unfiltered_filename,filename);
       if (*columns) {  /* apply column filtering, if required */
          strcat(filename, "[col ");
          for (i = 0; i<ncolumns; i++) {
              strcat(filename, columns_list[i]);
              if (i != ncolumns-1) {
                 strcat(filename, ";");
	      }
          }
          strcat(filename, "]");
       }

       if (fits_open_table(&infptr, filename, READONLY, &status)) {
       
          if (skipbadfiles) {
            headas_chat(1," \n");
            headas_chat(1,"WARNING: Unable to open input file %s\n", filename);
            headas_chat(1,"WARNING:   Will ignore it, and continue with next file...\n");
            status = 0;
	    continue;
          } else {
            sprintf(msg,"Unable to open input FITS table: %s.",filename);
            HD_ERROR_THROW(msg,status);
            goto cleanup;
	  }
       } else {
          headas_chat(5,"Opened next input table:\n  %s\n", filename);
       }

       /* verify that input and output table have same structure */
       fits_get_num_cols(infptr, &incols, &status);
       if (fits_read_key(infptr, TLONG, "NAXIS1", &inlen, NULL, &status))
          goto cleanup;

       if (incols != outcols || inlen != outlen) {
           sprintf(msg, 
         "Error: This table does not have same number or type of columns as 1st table:\n%s",filename);
           status=1;
           HD_ERROR_THROW(msg,status);
           goto cleanup;
       }

       /* check that the corresponding columns have the same datatypes */
       for (icol = 1; icol <= incols; icol++) {
            fits_get_coltype(infptr,  icol, &intype,  &inrep,  NULL, &status);
            if (fits_get_coltype(outfptr, icol, &outtype, &outrep, NULL, &status))
               goto cleanup;

            if (intype != outtype || inrep != outrep) {
                sprintf(msg, 
                   "Column %d is not the same as in previous tables.\n %s", icol,filename);
                status=1;
                HD_ERROR_THROW(msg,status);
                goto cleanup;
            }
            if (intype < 0) {
                sprintf(msg, 
           "Variable length arrays (in column %d) are not supported\n %s", icol,filename);
                status=1;
                HD_ERROR_THROW(msg,status);
                goto cleanup;
            }
       }

       /* OK, now copy the rows */
       if (fits_get_num_rows(infptr, &ntodo, &status)) goto cleanup;

       inrow = 1;

       while(ntodo) {
          if (ntodo > maxrows)
              nrows = maxrows;
          else
              nrows = ntodo;

          buffsize = nrows * inlen;

          /* read row from input and write it to the output table */
          fits_read_tblbytes( infptr,  inrow, 1, buffsize, bptr, &status);
          if (fits_write_tblbytes(outfptr, outrow, 1, buffsize, bptr, &status))
                goto cleanup;

          ntodo  -= nrows;
          inrow  += nrows;
          outrow += nrows;
       }
       headas_chat(5, "  Copied %ld rows to the output table.\n", inrow-1);

       fits_close_file(infptr,  &status);
       infptr = 0;
    } /* end of main file 'while' loop */

    /* Copy any remaining rows from first input table if insertrow was used */
    if (insertrow && insertrow < numrows) {

       ntodo = numrows - nextrow + 1;
       remaining = ntodo;

       while(ntodo) {
          if (ntodo > maxrows)
              nrows = maxrows;
          else
              nrows = ntodo;

          buffsize = nrows * inlen;

          fits_read_tblbytes(firstfptr, nextrow, 1, buffsize, bptr, &status);
          if (fits_write_tblbytes(outfptr, outrow, 1, buffsize, bptr, &status))
             goto cleanup;

          ntodo  -= nrows;
          nextrow  += nrows;
          outrow += nrows;
       }
       headas_chat(5, "Copied remaining %ld rows from 1st input table to the output table.\n", remaining);
    }

    for (i = 0; i < MAXKEYS; i++) free(columns_list[i]); 

    /* copy any requested keywords from the last input file */
    for (i = 0; i < MAXKEYS; i++)
	lastkey_list[i] = (char *)calloc(FLEN_KEYWORD+1,sizeof(char));
    nlastkeys = 0;  
    ftmerge_get_list(lastkey, lastkey_list, FLEN_KEYWORD, &nlastkeys);

    if (*lastkey) {
       if (fits_open_file(&lastfptr, unfiltered_filename, READONLY, &status))
	       goto cleanup;
       for (i = 0; i<nlastkeys; i++)
       {
	   if (fits_test_keyword(lastkey_list[i], &status)) goto cleanup;
	   if (fits_read_card(lastfptr, lastkey_list[i], lastkey_card, &status))
		goto cleanup;
	   if (fits_modify_card(outfptr, lastkey_list[i], lastkey_card, &status))
		goto cleanup;
       }
       headas_chat(5, "Copied %ld keyword(s) from last file to the output file.\n", nlastkeys);
       for (i = 0; i < MAXKEYS; i++) free(lastkey_list[i]); 
    }

    /* write history keywords, depending on HISTORY parameter */
    HDpar_stamp(outfptr, 0, &status); /* write to current HDU */ 

    if (copyall) { /* copying all HDUs? */

       if (fits_copy_file(firstfptr, outfptr, 0, 0, 1, &status)) goto cleanup;
       headas_chat(5,
           "Copied any remaining HDUs from 1st file to the output file.\n");

    }

    headas_chat(5, "\nCopied a total of %ld rows to the output table.\n",
         outrow-1);

cleanup:

    if (infptr)  fits_close_file(infptr,  &status);
    if (bptr) free(bptr);
    if (outfptr) fits_close_file(outfptr, &status);
    if (firstfptr) fits_close_file(firstfptr, &status);
    if (lastfptr) fits_close_file(lastfptr, &status);
    if (textfile)  fclose(textfile);
    if (columnsfile)  fclose(columnsfile);

    return(status);
}

/******************************************************************************
* Function
*      ftmerge_get_list
*
*
* DESCRIPTION:
*      Adapted from function get_keylist in ftdiff:
*      Parse an input parameter string (in comma-separated list or '@' file
*      syntax.
*
*******************************************************************************/
void ftmerge_get_list( char *parameter_string,	/* parameter string */
		 char **ptoken_list,		/* parameter list */
		 int  ptoken_len,		/* length of individual items */
		 int  *nptokens			/* number of items in list */
		 )
{
    FILE *list;					/* ASCII list file pointer */
    int i;
    char *p;
    char temp[80];
    char comm[MAXMSG];

    *nptokens = 0;

    p = parameter_string; 
    while(*p == ' ') p++; 
    if(*p == '\0') return ;
    i = *nptokens;
    if(*p != '@') { 
	if(strchr(p,',') == NULL) { 
	    while(isspace((int) *p)) p++; 
	    strncpy(ptoken_list[i],p,ptoken_len);
	    ptoken_list[i][ptoken_len] = '\0';
	    p = ptoken_list[i];
	    while(*p && !isspace((int) *p) ) p++; 
	    *p = '\0';
	    i++;
        } else {
	    p = strtok(parameter_string,",");
	    while(p) { 
	        while(isspace((int) *p)) p++; 
	        strncpy(ptoken_list[i],p,ptoken_len);
	        ptoken_list[i][ptoken_len] = '\0';
	        p = ptoken_list[i];
	        while(*p && !isspace((int)*p) ) p++; 
	        *p = '\0';
		i++;
		p = strtok(NULL,","); 
		if(i >= MAXKEYS) break;
            }
        } 
    } else { 
	p++; 
        if((list = fopen(p,"r")) == NULL) { 
            sprintf(comm,"Cannot find the list file %s. Ignored. \n", p);
        } else { 
	    while(fgets(temp,80,list)) {  
		p = temp;
	        while(isspace((int) *p)) p++; 
		strncpy(ptoken_list[i],p,ptoken_len);
	        ptoken_list[i][ptoken_len] = '\0';
	        p = ptoken_list[i];
	        while( *p && !isspace((int) *p) ) p++; 
	        *p = '\0';
		i++;
		if(i >= MAXKEYS) break;
            }
        }
	fclose(list);
    }
    *nptokens = i;

    /* Make all the keywords uppercase */ 
    for (i = 0; i < *nptokens; i++) { 
	p = ptoken_list[i]; 
	while(*p != '\0') { 
	    if( *p >= 'a' && *p <= 'z') *p -= 32;
	    p++;
        }
    }
    return; 
}
