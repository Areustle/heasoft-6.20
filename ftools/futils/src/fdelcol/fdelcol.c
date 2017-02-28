/******************************************************************************

FTOOLS TASK:  	 
			fdelcol

FILE:
	fdelcol.c

DESCRIPTION:
	Deletes a specified COLUMN from a FITS Table.

AUTHOR:		
	Srilal Weera (Mar 97)

MODIFICATION HISTORY: see log at EOF
	Srilal Weera (Mar 97)  v1.0.0 Original 

        Revision 1.2  1997/10/03 21:16:46  peachey
        Updated to make consistent with changes to libcftools.a: 
	TASK Fortran common block replaced by a global C string variable.

        Revision 1.3  1997/10/07 15:15:03  peachey
        Changed initialization of taskname to make consistent with 
	libcftools changes

	Ning Gan (Aug 98)  v1.0.3 Added check for the existence of the 
				extension number/name. Minor changes
				for error message.
	
  	Ning Gan (Dec 99)  v1.0.4 If the EXTNAME is missing, just go
                                  ahead. 

NOTES:

ASSOCIATED ROUTINES:
	Fdelcol: "main" function 
  	gdelcol: function to read the parameter file

PRIMARY LOCAL VARIABLES:
        fitsfile *infp - fitsfile with COLUMN to delete
   	extnum - HDU of the COLUMN to be deleted
	colname - column name to be deleted
	colnum  - column number to be deleted
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
#include "fdelcol.h"     /* task specific definitions */

#define MAXHDU 100000

void Fdelcol()
{
	char task[] = "fdelcol v1.0.4";
	char infile[FLEN_BUFFER] = "";
	char temp[FLEN_BUFFER] = "";
	char colname[FLEN_BUFFER] = "";
	char comment[FLEN_ERRMSG]   = "";
	char context[FLEN_ERRMSG]   = "";
	char keyword[FLEN_KEYWORD] = "";

	int  extnum = 0;
	int  colnum = 0;
	int  status = 0;
	int  hdutype = 0;
	int  inopen = FALSE;
	int  confirm = 0;
	int  proceed = 0;


	fitsfile *infp = NULL;

	c_ptaskn(task); /* name of task used by c_fcerr */

	/* get parameters from fdelcol.par */
	gdelcol(infile, colname, &confirm, &status);
	if(status) 
	{
	strcpy(context, "Error occurred while reading par file");
	goto Exit_error;
	}

	/* parse the filename  */
	c_fcpars(infile, temp, &extnum, &status);
	strcpy(infile, temp);
	if(status)
	{
		strcpy(context, "Error occurred while parsing filename");
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


	/* open infile, move to the HDU containing the COLUMN  to be deleted */
	if(ffopen(&infp, infile, READWRITE, &status)){
		sprintf(context, "Error open file %s",infile);
		goto Exit_error;
	}
	else inopen = TRUE;

	if(ffmahd(infp, extnum + 1, &hdutype, &status))
	{
		sprintf(context, "Cannot find extension %d in file %s",
		    extnum, infile);
		goto Exit_error;
	}

	if(ffgcno(infp, FALSE, colname, &colnum, &status))
	{
		sprintf(context, "Cannot find column name %s in file %s",
		    colname, infile);
		goto Exit_error;
	}


	/* if CONFIRM is set to YES, print information about the extension
	containing the column to be deleted. 
	If CONFIRM is set to NO and PROCEED is set to YES, just delete the 
        column without any warnings. (suitable for a script) */


	if(confirm) {

		/* Read and print XTENSION keyword */
		ffgkey(infp, "XTENSION", temp, comment, &status);
		if(status)
		{
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
			sprintf(context, "Error reading EXTNAME keyword");
			goto Exit_error;
		}
		sprintf(context,"\n EXTNAME: %s",temp);
		c_fcecho(context);

		/* Read and print column name to be deleted */

		sprintf(context,"\n COLUMN NAME= %s",colname);
		c_fcecho(context);
		c_fcecho(" ");

	}

/* Read the PROCEED parameter from the parameter file.  
(The UCLGSB call is placed here instead in the gdelcol routine, since the
extension data has to be printed first if CONFIRM is set to YES) */
		Uclgsb("proceed", &proceed, &status);
		if(status)
		{
			sprintf(context, "Parameter 'proceed' not found in .par file");
			goto Exit_error;
		}

		/* If the user wishes to proceed, delete the column in the
		CHDU of the infile */
		if(proceed) ffdcol(infp, colnum, &status);
		if(status)
		{
			sprintf(context, "Error deleting the specified column");
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
		sprintf(context, "Error closing the input FITS file");
		c_fcerr(context);
	}

	return;
}


/******************************************************************************
Function: gdelcol

Notes:

Primary Local Variables:
    char *infile - name and extension of the source file
    char *colname - name of the column to be deleted
    int *confirm _ print information before delete
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

int gdelcol(char *infile, char *colname, int *confirm, int *status)
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

	Uclgst("colname", colname, status);
	if(*status)
	{
		sprintf(context, "Parameter 'colname' not found in .par file");
		goto Exit_error;
	}

	Uclgsb("confirm", confirm, status);
	if(*status)
	{
		sprintf(context, "Parameter 'confirm' not found in .par file");
		goto Exit_error;
	}


Exit_error:
	if(*status)
	{c_fcerr(context);}

	return *status;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL fdelcol
#endif
#ifdef unix
#define F77CALL fdelcol_ 
#endif

void F77CALL()
{
	void Fdelcol();

	Fdelcol();
}
/*
$Log: fdelcol.c,v $
Revision 1.10  2002/12/23 17:13:02  irby
Define MAXHDU (=100000) since it was taken out of fitsio.h in cfitsio v2.430.

Revision 1.9  1999/12/14 03:14:06  ngan
Continue if the EXTNAME keyword is undefined.

Revision 1.8  1999/12/14 02:48:19  ngan
Bug fixes.

Revision 1.7  1999/12/14 02:40:32  ngan
If the EXTNAME is missing, go ahead.

 * Revision 1.6  1998/08/04  20:03:31  ganning
 * Minor changes in error message.
 *
Revision 1.5  1998/08/04 19:49:40  ganning
version number error

Revision 1.4  1998/08/04 19:48:32  ganning
Added check for the existence of extension number. Minor changes for
other error message.

Revision 1.3  1997/10/07 15:15:03  peachey
Changed initialization of taskname to make consistent with libcftools changes

Revision 1.2  1997/10/03 21:16:46  peachey
Updated to make consistent with changes to libcftools.a: TASK Fortran common
block replaced by a global C string variable.

*/
