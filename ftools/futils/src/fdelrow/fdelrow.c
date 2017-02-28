/******************************************************************************

FTOOLS TASK:  	 
			fdelrow

FILE:
	fdelrow.c

DESCRIPTION:
	Deletes specified ROWS from a FITS Table.

AUTHOR:		
	Srilal Weera (Mar 97)

MODIFICATION HISTORY:
        17Jun98: MJT fixed silent failure on default (or error) in
                 extension number.
        04Aug98: Ning Gan 
		 Added check for absence of the extension number. 
		 print out the cfitsio error message.
        12Dec 99 Ning Gan (v1.0.3) 
                 If the EXTNAME is missing, just go ahead.



NOTES:

ASSOCIATED ROUTINES:
	Fdelrow: "main" function 
  	gdelrow: function to read the parameter file

PRIMARY LOCAL VARIABLES:
        fitsfile *infp - fitsfile with ROWS to delete
   	extnum - HDU of the ROWS to be deleted
	firstrow - start row to be deleted
	nrows  -  number of rows to be deleted
	libcftools.a:
    	c_fcpars - utility to parse file names
   	c_fcerr - write to STDERR
   	c_fcecho - write to STDOUT
	libcfitsio.a 
	ffXXXX - cfitsio library
******************************************************************************/


#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "fitsio.h"     /* cfitsio defined constants */
#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "cftools.h"     /* standard C library constants */
#include "fdelrow.h"     /* task specific definitions */

#define MAXHDU 100000

void Fdelrow()
{
	char task[] = "fdelrow v1.0.3";
	char infile[FLEN_BUFFER] = "";
	char temp[FLEN_BUFFER] = "";
	char comment[FLEN_ERRMSG]   = "";
	char context[FLEN_ERRMSG]   = "";
	char keyword[FLEN_KEYWORD] = "";

	int  inopen = FALSE;
	int  extnum = 0;
	int  status = 0;
	int  hdutype = 0;
	int  confirm = 0;
	int  proceed = 0;

	int i_firstrow = 0;
	int i_nrows = 0;
	long firstrow = 0;
	long nrows = 0;

	fitsfile *infp = NULL;

	c_ptaskn(task); /* name of task used by c_fcerr */
	/* get parameters from fdelrow.par */
	gdelrow(infile, &i_firstrow, &i_nrows, &confirm, &status);
	if(status) return;

	/* parse the filename  */
	c_fcpars(infile, temp, &extnum, &status);
	strcpy(infile, temp);
	if(status)
	{
		strcpy(context, "Error occurred while parsing filename");
		c_fcerr(context);
		goto Exit_error;
	}
        if (extnum == -99 )  {
           strcpy(context, "The extension name or number is not given");
          strcat(context, " in the file: ");
           strcat(context, infile);
           c_fcerr(context);
           goto Exit_error;
        }


  
	if((extnum < 1) || (extnum > MAXHDU))
	{
		sprintf(context, "Illegal extension (%d) given for file %s",
		    extnum, infile);
		c_fcerr(context);
		goto Exit_error;
	}


	/* open infile, move to the HDU containing the ROWS  to be deleted */
	if(ffopen(&infp, infile, READWRITE, &status)){
		ffrprt(stderr,status);
                sprintf(context, "Error open file %s",infile);
                goto Exit_error;
	}
	else inopen = TRUE;

	if(ffmahd(infp, extnum + 1, &hdutype, &status))
	{
		ffrprt(stderr,status);
		sprintf(context, "Cannot find extension %d in file %s",
		    extnum, infile);
		goto Exit_error;
	}


	/* if CONFIRM is set to YES, print information about the extension
	containing the rows to be deleted. 
	If CONFIRM is set to NO and PROCEED is set to YES, just delete the 
        rows without any warnings. (suitable for a script) */


	if(confirm) {

		/* Read and print XTENSION keyword */
		ffgkey(infp, "XTENSION", temp, comment, &status);
		if(status)
		{
		        ffrprt(stderr,status);
			sprintf(context, "Error reading XTENSION keyword");
			goto Exit_error;
		}
		sprintf(context,"\n EXTENSION: %s",temp);
		c_fcecho(context);


		/* Read and print EXTNAME keyword */
		ffgkey(infp, "EXTNAME", temp, comment, &status);
                if(status == KEY_NO_EXIST || status == VALUE_UNDEFINED) {
                    status = 0;
                    strcpy(temp,"");
                }
                if(status )
		{
		        ffrprt(stderr,status);
			sprintf(context, "Error reading EXTNAME keyword");
			goto Exit_error;
		}
		sprintf(context,"\n EXTNAME: %s",temp);
		c_fcecho(context);
		c_fcecho(" ");

	}

/* Read the PROCEED parameter from the parameter file.  
(The UCLGSB call is placed here instead in the gdelrow routine, since the
extension data has to be printed if CONFIRM is set to YES) */

		Uclgsb("proceed", &proceed, &status);
		if(status)
		{
			sprintf(context, "Parameter 'proceed' not found in .par file");
			goto Exit_error;
		}

		/* If the user wish to proceed, delete the specified rows in the
		CHDU of the infile */
		firstrow = (long) i_firstrow;
		nrows = (long) i_nrows;
		if(proceed) ffdrow(infp, firstrow, nrows, &status);
		if(status)
		{
		        ffrprt(stderr,status);
			sprintf(context, "Error deleting the specified rows");
			goto Exit_error;
		}


Exit_error:
	if(status)
	{
		c_fcerr(context);
	}

	/* close the modified infile */
	status = 0;
	if(inopen) ffclos(infp, &status);
	if(status)
	{
		ffrprt(stderr,status);
		sprintf(context, "Error closing the input FITS file");
		c_fcerr(context);
	}

	return;
}


/******************************************************************************
Function: gdelrow

Notes:

Primary Local Variables:
    char *infile - name and extension of the source file
    int *status _ program status

Functions called:
library functions:
    libhost.a:
        Uclgst - get a string from .par file
        Uclgsi - get an integer value from .par file
        Uclgsb - get a boolean (char) value from .par file

    libcftools.a
        c_fcerr - write to STDERR
******************************************************************************/

int gdelrow(char *infile, int *i_firstrow, int *i_nrows, int *confirm, int *status)
{
	char temp[FLEN_BUFFER] = "";
	char context[FLEN_ERRMSG]   = "";
	char msg[FLEN_ERRMSG] = "";
	int BufLen_2 =  FLEN_BUFFER - 1;/* required by cfortran.h */




	if(*status) return *status;
	Uclgst("infile", infile, status);
	if(*status)
	{
		sprintf(context, "Parameter 'infile' not found in .par file");
		goto Exit_error;
	}

	Uclgsi("firstrow", i_firstrow, status);
	if(*status)
	{
		sprintf(context, "Parameter 'firstrow' not found in .par file");
		goto Exit_error;
	}


	Uclgsi("nrows", i_nrows, status);
	if(*status)
	{
		sprintf(context, "Parameter 'nrows' not found in .par file");
		goto Exit_error;
	}

	Uclgsb("confirm", confirm, status);
	if(*status)
	{
		sprintf(context, "Parameter 'confirm' not found in .par file");
		goto Exit_error;
	}

	/* If CONFIRM is set to YES, then print information about the extension
	to be deleted and query (in the main routine). */

	return *status; /* don't execute Exit_error unless there is an error */

Exit_error:
	c_fcerr(context);

	return *status;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL fdelrow
#endif
#ifdef unix
#define F77CALL fdelrow_ 
#endif

void F77CALL()
{
	void Fdelrow();

	Fdelrow();
}

/*
$Log: fdelrow.c,v $
Revision 1.8  2002/12/23 17:13:10  irby
Define MAXHDU (=100000) since it was taken out of fitsio.h in cfitsio v2.430.

Revision 1.7  1999/12/14 03:13:05  ngan
Skipped the case if the EXTNAME is undefined.

Revision 1.6  1999/12/14 02:50:16  ngan
If EXTNAME doesn't exist, go ahead to perform the task.

 * Revision 1.5  1998/08/04  21:06:41  ganning
 *  Added the check for the presence of extension number.  Added the
 *  feature to print out the cfitsio error message.
 *
Revision 1.4  1998/06/17 20:13:55  miket
now calls c_fcerr on extnum errors instead of silently doing nothing!

Revision 1.3  1997/10/07 15:14:57  peachey
Changed initialization of taskname to make consistent with libcftools changes

Revision 1.2  1997/10/03 21:37:43  peachey
Updated to make consistent with changes to libcftools.a: TASK Fortran common
block replaced by a global C string variable.

*/
