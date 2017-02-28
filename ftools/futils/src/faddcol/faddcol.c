/***********************************************************************
FTOOLS task:	faddcol
File:	faddcol.c
Description:	Copy a column or columns from one FITS extension to another. 

Author:	Zhiyu Guo, written based on the FORTRAN version by William Pence.
Modified: Ning Gan, 10/5/1998, write the error message to the stderr
                    instead.
          P. Wilson 12/21/1998, Insert columns at end of file input file
                                instead of at location in input file
                                corresponding to the end of the column file
                                (use tfields_in instead of tfields_col)
          P. Wilson 01/27/1999, Don't search filenames for + or [ characters
                                to learn whether an extension was specified.
                                Instead, get current HDU number.
       M. Tripicco 14 Oct 1999, colfile should be opened READONLY to allow piping
       M. Tripicco 27 Mar 2000, Changed strings which hold column names
                                from 10 to FITS_CLEN_STRVAL (69).

           B. Irby 05 Mar 2001, Added code to correctly exclude a column
				(ie. -COL_NAME as input colname).
				Added code to perform 'delkey=yes' operations.
				Increased size of 'comment' buffer to
				hold longer filenames, etc.

Primary Local Variables:
	infile	- name of fits file to be modified
	colfile	- name of fits file containing new columns
	colname	- input column name string

Associated subroutines:
	gaddcol	- function to read parameter file
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h> 
#include "cfortran.h"
#include "fitsio.h"     /* cfitsio defined constants */
#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "ftools.h"     /* standard C library constants */
#include "ftoolstruct.h"
#include "cfitsio.h"

#define BufLen_2 254
#define MaxElem_2 999

int gaddcol(char *, char *, char *, int *, int *, int *, int *);  

void faddcol()
{
	fitsfile *inptr, *colptr;
	int status=0;
	int hdutype,anynul,i,j,k,ncol,colnum,negflag,pstatus,nfound,col;
	double nulval,array[139];
	long firstrow,firstelem,nelem;
	char *infile, *colfile, *colname,tmpname[10]; 
	char colist[MaxElem_2][BufLen_2+1], comment[256];
	int delkey,dohist,casesen,tfield_col,tfield_in,nrows_col,nrows_in;
	int found;
	char **namelist_col;

	firstrow=1,firstelem=1,nelem=1;

/* get input information */
	infile=(char*) malloc(255*sizeof(char));
	colfile=(char*) malloc(255*sizeof(char));
	colname=(char*) malloc(255*sizeof(char));

	gaddcol(infile,colfile,colname,&delkey,&dohist,&casesen,&pstatus);

/* open the file to be updated */
	ffopen(&inptr,infile,READWRITE,&status);

        if(status != 0){
		fits_report_error(stderr,status);
		exit(1);
        }

/* check to see if we are at the primary array.
   If yes, move to the first one. */

	fits_get_hdu_num( inptr, &k );
	if(k == 1){
		ffmahd(inptr,2,&hdutype,&status);
                if(status != 0){
		         fits_report_error(stderr,status);
			 exit(1);
                }
	}

/* open the file that contains columns to be added to infile */
	ffopen(&colptr,colfile,READONLY,&status);

        if(status != 0){
		fits_report_error(stderr,status);
		exit(1);
        }


/* check to see if we are at the primary array.
   If yes, move to the first one. */

	fits_get_hdu_num( colptr, &k );
	if(k == 1){
                ffmahd(colptr,2,&hdutype,&status);
	        if(status != 0){
		         fits_report_error(stderr,status);
			 exit(1);
        	}
        }

/* get NAXIS2 keywords from both infile and colfile */

        fits_read_key(colptr,TINT,"NAXIS2",&nrows_col,comment,&status);
        if(status != 0){
		fits_report_error(stderr,status);
		exit(1);
        }

        fits_read_key(inptr,TINT,"NAXIS2",&nrows_in,comment,&status);
        if(status != 0){
		fits_report_error(stderr,status);
		exit(1);
        }

/* check if number of rows match */

	if(nrows_col != nrows_in){ 
	c_fcerr("Number of rows in colfile doesn't match rows in input file.");
		exit(1);
	}

/* get TFIELDS keywords */

	fits_read_key(colptr,TINT,"TFIELDS",&tfield_col,comment,&status);
        if(status != 0){
		fits_report_error(stderr,status);
		exit(1);
        }

        fits_read_key(inptr,TINT,"TFIELDS",&tfield_in,comment,&status);
        if(status != 0){
		fits_report_error(stderr,status);
		exit(1);
        }

/* assign memory to namelist_col */

	namelist_col=(char **) calloc(tfield_col,sizeof(char*));
	for(i=0;i<tfield_col;i++){
		namelist_col[i]=(char*) calloc(FITS_CLEN_STRVAL,sizeof(char));
	}

/* get all column names from colfile */

	fits_read_keys_str(colptr,"TTYPE",1,tfield_col,namelist_col,
		&nfound,&status);
	if(status != 0){
		fits_report_error(stderr,status);
		exit(1);
	}
	
/* check input column name string and copy the corresponding columns */

	/* find the corresponding column numbers from input column names */
        Fcgcls(colname,colist,&ncol,&negflag);

	if((strcmp(colname,"-") == 0) || (strcmp(colname,"\0") == 0) ||
		negflag) { 

        /* copy all columns, unless exclusions were named or they
	   already exist */

	   for(i=0;i<tfield_col;i++) {
		found=0;
		if(negflag) {
	   		for(j=0;j<ncol;j++) {
			if(strcmp(colist[j],namelist_col[i]) == 0) found=1;
			}
		}
		if (found) continue; else {
		fits_get_colname(inptr,casesen,namelist_col[i],
				tmpname,&col,&status);
		if(status == 0) {
		   sprintf(comment,"FADDCOL: column %s already exists.",
 				namelist_col[i]);
		   c_fcerr(comment);
		   exit(1);
		} else {
		   status=0;
		   ffcpcl(colptr,inptr,i+1,tfield_in+i+1,TRUE,&status);
		   if(status != 0){
				fits_report_error(stderr,status);
				exit(1);
		   }
		   if(dohist != 0) { /* add history */
			strcpy(comment,"Column ");	
			strcat(comment,namelist_col[i]);
			strcat(comment," is copied from file ");
			strcat(comment,colfile);
			fits_write_history(inptr,comment,&status);
			if(status != 0){
				fits_report_error(stderr,status);
				exit(1);
                        }
		   }
		   if(delkey != 0) { /* delete matching keyword record */
			fits_delete_key(inptr,tmpname,&status);
			if(status != 0){
				fits_report_error(stderr,status);
				exit(1);
                        }
		   }
		}
		}
	   }
	} else {

	/* only copy the named column(s) */

	for(i=0;i<ncol;i++){
		fits_get_colname(colptr,casesen,colist[i],colname,&colnum,
				&status);

		fits_get_colname(inptr,casesen,colname,tmpname,&col,&status);
		if(status == 0){
			sprintf(comment, "FADDCOL: column %s already exists.\n",
				colname);
			c_fcerr(comment);
			exit(1);
		} else {
			status=0;
			ffcpcl(colptr,inptr,colnum,tfield_in+i+1,TRUE,&status);
			if(status != 0){
				fits_report_error(stderr,status);
				exit(1);
			}
                	if(dohist != 0) { /* add history */
                        	strcpy(comment,"Column ");      
                        	strcat(comment,colname);
                        	strcat(comment," is copied from file ");
                        	strcat(comment,colfile);
                        	fits_write_history(inptr,comment,&status);
                        	if(status != 0){
					fits_report_error(stderr,status);
					exit(1);
                        	}
                	}
			if(delkey != 0) { /* delete matching keyword */
				fits_delete_key(inptr,colname,&status);
				if(status != 0){
					fits_report_error(stderr,status);
					exit(1);
                        	}
			}
		}
	}
	}

	ffclos(inptr,&status);
	if(status != 0){
	        fits_report_error(stderr,status);
        }

	ffclos(colptr,&status);
        if(status != 0){
	        fits_report_error(stderr,status);
        }
free(infile);
free(colfile);
free(colname);
}   /* the end of faddcol */

#ifdef vms
#define F77CALL faddcol
#endif
#ifdef unix
#define F77CALL faddcol_
#endif
 
void F77CALL()
{
        void faddcol();
 
        faddcol();
}

int gaddcol(char *infile,char *colfile,char *colname,int *delkey,int *dohist,
	int *casesen, int *pstatus)
{
	char context[80];

	*pstatus=0;

/* get the name of the fits file to be updated */
	Uclgst("infile",infile,pstatus);
	if(*pstatus != 0){
		c_fcerr("Can't get the name of the file to be updated!");
		exit(1);
	}

/* get the name of the fits file containing the column information */
	Uclgst("colfile",colfile,pstatus);
        if(*pstatus != 0){
                c_fcerr("Can't get the name of the file containing column information!");
                exit(1);
        }

/* get the string for a list of column names */
        Uclgst("colname",colname,pstatus);
        if(*pstatus != 0){
                c_fcerr("Can't get the name of the column(s)!");
                exit(1);
        }

/* get flag indicating whether to delete keyword with the same name */
        Uclgsb("delkey",delkey,pstatus);
        if(*pstatus != 0){
                c_fcerr("Can't get DELKEY parameter!");
                exit(1);
        }

/* get flag indicating whether to write HISTORY keywords */
        Uclgsb("history",dohist,pstatus);
        if(*pstatus != 0){
                c_fcerr("Can't get HISTORY parameter!");
                exit(1);
        }

/* get flag indicating whether input column names are case sensitive */
        Uclgsb("casesen",casesen,pstatus);
        if(*pstatus != 0){
                c_fcerr("Can't get CASESEN parameter!");
                exit(1);
        }
	return *pstatus;
}
