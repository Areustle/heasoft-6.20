#include <stdio.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "ftsort.h"
#include "headas_error.h"

#define MAXMSG 256

/*
HISTORY
-------
  Version 1.0 written by Ziqin Pan, NASA/GSFC, MAY 2002
*/

#define TOOLSUB ftsort
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftsort(void);
int ftsort_getpar(char *infile, char *outfile, int* memory, 
      char *method, char *column, int *unique, int *copyall);
int ftsort_work(char *infile, char *outfile, int memory, 
      char *method, char *column, int unique, int copyall); 
int gcls(  char* columns,char** colist,int* ascend);
int cmpl ( char ** list1, int * nlist1, char ** list2, 
      int * nlist2);
int read_table( fitsfile *infptr,int nrows,int  ncols, 
      long* repeat,int *dtype,int* colnum, int** tbl_index, 
      column **data, int *status) ;
void do_memsort(int start, int end, int* tbl_index, column * col, int* ascend,
      int ncols, char* method, int unique);
void write_table(fitsfile * infptr, fitsfile *outfptr,
      int nrows,int ldflag,
      int *tbl_index, int *status);
void free_mem(int ncols, column* data);
void do_filesort(int start, int end, fitsfile* outfptr,
      int* ascend, long* repeat, int* dtype, int* colnum,int ncols,
      char* method,int unique,int *status);


int ftsort (void)
{
/* sorts rows for a FITS file based on input column names */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    char columns[80], method[PIL_LINESIZE];
    int unique,memory,copyall;
    int status;
    static char taskname[80] = "ftsort";
    static char version[8] = "1.01";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
        status = ftsort_getpar(infile, outfile, &memory, method,columns, &unique, &copyall);

    /* call work function to sort table */
    if (!status)
        status = ftsort_work(infile, outfile, memory, method, columns, unique,copyall);

    return(status);
}

/*---------------------------------------------------------------------------*/
int ftsort_getpar(
    char *infile,     /* O - Input file name  */
    char *outfile,    /* O - Output file name */
    int *memory,      /* O - Memory sort */
    char *method,     /* O - Sorting method to use */
    char *columns,    /* O - Column name */
    int *unique,      /* O - Whether to purge rows with identical keys*/
    int *copyall)     /* O - Copy all HDUs to the output file */
{
/*  read input parameters for the ftsort task from the .par file */

    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
      sprintf(msg, "Error reading the 'infile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("columns", columns))) {
      sprintf(msg, "Error reading the 'columns' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("method", method))) {
      sprintf(msg, "Error reading the 'method' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("memory", memory))) {
      sprintf(msg, "Error reading the 'memory' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("unique", unique))) {
      sprintf(msg, "Error reading the 'unique' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("copyall", copyall))) {
      sprintf(msg, "Error reading the 'unique' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    return(status);
}

/*---------------------------------------------------------------------------*/
int ftsort_work(
    char *infile,   /* I - Input file name */
    char *outfile,  /* I - Output file name */
    int memory,     /* I - Use memory sort */
    char *method,   /* I - Method used to sorting */
    char *columns,  /* I - Column names */
    int unique,     /* I - Whether to purge rows with identical keys*/
    int copyall)    /* I - Copy all HDUs to the output file */
{
    fitsfile *infptr = 0, *outfptr = 0;
    int status, hdutype, hdunum,tfields=0;
    char **ttype = 0, **tform = 0;
    long rowlen, nrows, pcount,wid;

    int ncols, colnum[80], dtype[80], ascend[80];
    char* colname[80];
    long repeat[80];
   
    int * tbl_index;
    column* data;

    long i,j;

    /* Initialization of variables */
    status =0;

    /* Open the input file, and move to first 'interesting' table */
    if (fits_open_table(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input table:\n %s\n",infile);

    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    if (fits_create_file(&outfptr, outfile, &status)) goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfile);

    fits_get_hdu_num(infptr, &hdunum);  /* save current position */

    if (copyall ) {

        if (fits_copy_file(infptr, outfptr, 1, 1, 1, &status)) goto cleanup;
        headas_chat(5,"Copied all HDUs from input file to output file.\n");

    } else {  /* just copy the current HDU */

        if (fits_copy_hdu(infptr, outfptr, 0, &status)) goto cleanup;
        headas_chat(5,"Copied the current table to output file.\n");
    }

    /* reset infile HDU position */
    if (fits_movabs_hdu(infptr, hdunum, &hdutype, &status)) goto cleanup;

    /* reset outfile HDU position */
    if(copyall) {

        if (fits_movabs_hdu(outfptr, hdunum, NULL, &status)) goto cleanup;

    } else {

        if (fits_movabs_hdu(outfptr, 2, NULL, &status)) goto cleanup;
    }

    /* Get fits table column information */
    if (fits_get_num_cols(infptr,&tfields,&status)) goto cleanup;

    ttype =(char **) malloc(tfields*sizeof(char*));
    tform =(char **) malloc(tfields*sizeof(char*));

    for (i = 0; i < tfields; i++)
    {
        ttype[i] = (char *) malloc(70);
        tform[i] = (char *) malloc(20);
    }

    if ( hdutype == ASCII_TBL )
    {
        if(fits_read_atblhdr (infptr, tfields, &rowlen, &nrows, &tfields, ttype,
            NULL, tform, NULL, NULL, &status)) goto cleanup;
    }
    else  if( hdutype == BINARY_TBL)
    {
        if(fits_read_btblhdr (infptr, tfields, &nrows, &tfields, ttype,
            tform, NULL, NULL, &pcount, &status)) goto cleanup;
    }

    /* Get the names of the columns to be sorted */
    ncols = gcls ( columns, colname, ascend );


    /* Verify the column names */ 
    if( cmpl(colname,&ncols,ttype,&tfields) ) 
       goto cleanup;

    /* Get column number, datatype, repeat and width of columns */
    for (i=0; i<ncols; i++)
    {
        fits_get_colnum(infptr,0,colname[i],colnum+i,&status);

        j=colnum[i]-1;

        if ( hdutype == ASCII_TBL )
             fits_ascii_tform(tform[j],&dtype[i], &repeat[i], NULL, &status);

        else if ( hdutype == BINARY_TBL )
             fits_binary_tform(tform[j], &dtype[i], &repeat[i],&wid, &status);
    }


    /* Read the table to memory, if fail set memory false */
    if(memory) 
    {
       headas_chat(5,"Reading input table into memory.\n");

       if( read_table(infptr,nrows,ncols,repeat,dtype,
          colnum, &tbl_index, &data, &status) != 0)
       {
          memory =0;
          headas_chat(5,"Failed to read table into memory.\n");
          headas_chat(5,"Will sort table on disk, instead.\n");
       }
    }

    
    if(memory)
    {
       headas_chat(5,"Sorting table in memory...\n");
       do_memsort(0, nrows,tbl_index,data,ascend,ncols,method,unique);

       free_mem(ncols,data);    
       free(data);

       write_table(infptr,outfptr,nrows,1,tbl_index,&status);

       free(tbl_index);
    }
    else
    {
       headas_chat(5,"Sorting table on disk...\n");
       do_filesort(1,nrows+1,outfptr,ascend,repeat,dtype,colnum,ncols,method,unique,&status);
    }


    headas_chat(5, "Sorted '%s' file using those columns:\n",
                infile);
    for (i=0; i<ncols; i++)
    {
        headas_chat(5, "  %s\n", colname[i]);
    }

    HDpar_stamp(outfptr, 0, &status); /* write optional history keywords */

cleanup:

    if (outfptr) fits_close_file(outfptr, &status);
    if (infptr)  fits_close_file(infptr,  &status);

    for (i=0; i<tfields; i++) 
    {
       if(tform[i]) free(tform[i]);
       if(ttype[i]) free(ttype[i]);
    }
    if(tform) free(tform);
    if(ttype) free(ttype);

    return(status);
}

