/******************************************************************************

FTOOLS TASK:  	 
			fdelhdu

FILE:
	fdelhdu.c

DESCRIPTION:
	Deletes a specified HDU from a FITS file.

AUTHOR:		
	Srilal Weera (Feb 97)

MODIFICATION HISTORY:
	Srilal Weera (Feb 97):  V1.0.0
          Original

	Ning Gan (Aug 98): V1.0.1
          Added check for the existence of the extension number/name. 
          Minor changes for error message.

	Ning Gan (Aug 98): V1.0.2
	  print  out the cfitsio error message.

	Ning Gan (Oct 98): V1.0.4
	  Accept the HDU without the EXTNAME keyword or values. 

        Peter Wilson (Jan 99): V1.0.5
          Remove 'proceed' test from 'confirm' block so that one cand
          delete without having to see keyword values.
NOTES:

ASSOCIATED ROUTINES:
	Fdelhdu: "main" function 
  	gdelhdu: function to read the parameter file

PRIMARY LOCAL VARIABLES:
        fitsfile *infp - fitsfile with HDU to delete
   	extnum - HDU to be deleted
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
#include "fdelhdu.h"     /* task specific definitions */

#define MAXHDU 100000

void Fdelhdu()
{
	char task[] = "fdelhdu v1.0.5";
	char infile[FLEN_BUFFER] = "";
	char temp[FLEN_BUFFER] = "";
	char comment[FLEN_ERRMSG]   = "";
	char context[FLEN_ERRMSG]   = "";
	char keyword[FLEN_KEYWORD] = "";
	int  extnum = 0;
	int  status = 0;
	int  hdutype = 0;
	int  inopen = FALSE;
	int  confirm = 0;
	int  proceed = 0;
	int nextkey = 0;
	int ii = 0;

	long longnaxis, axislen;

	fitsfile *infp = NULL;

	c_ptaskn(task); /* name of task used by c_fcerr */

	/* get parameters from fdelhdu.par */
	gdelhdu(infile, &confirm, &status);
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


	/* open infile, move to the HDU needs to be deleted */
	if(ffopen(&infp, infile, READWRITE, &status)){
	       ffrprt(stderr, status); 
               sprintf(context, "Error open file %s",infile);
	       goto Exit_error;
	}
	else inopen = TRUE;

	if(ffmahd(infp, extnum + 1, &hdutype, &status))
	{
	        ffrprt(stderr, status); 
		sprintf(context, "Cannot find extension %d in file %s",
		    extnum, infile);
		goto Exit_error;
	}


	/* if CONFIRM is set to YES, print information about the extension
	 to be deleted. 
	If CONFIRM is set to NO and PROCEED is set to YES, just delete the 
        extension without any warnings. (suitable for a script) */

	if(confirm) {

		/* Read and print XTENSION keyword */
		ffgkey(infp, "XTENSION", temp, comment, &status);
		if(status)
		{
	                ffrprt(stderr, status); 
			sprintf(context, "Error reading XTENSION keyword");
			goto Exit_error;
		}
		sprintf(context,"\n EXTENSION: %s",temp);
		c_fcecho(context);


		/* Read and print EXTNAME keyword */
		ffgkey(infp, "EXTNAME", temp, comment, &status);
		if(status == VALUE_UNDEFINED || status == KEY_NO_EXIST) 
		     status = 0;
		if(status)
		{
	                ffrprt(stderr, status); 
			sprintf(context, "Error reading EXTNAME keyword");
			goto Exit_error;
		}
		sprintf(context,"\n EXTNAME: %s",temp);
		c_fcecho(context);

		/* Read and print NAXIS keyword */

		ffgtkn(infp, 3, "NAXIS",  &longnaxis, &status);
		if(status)
		{
	                ffrprt(stderr, status); 
			sprintf(context, "Error reading NAXIS keyword");
			goto Exit_error;
		}
		sprintf(context,"\n NAXIS= %d",longnaxis);
		c_fcecho(context);


		/* Read and print NAXES[] values  */

		for (ii=0, nextkey=4; ii < longnaxis; ii++, nextkey++
		    )
		{
			ffkeyn("NAXIS", ii+1, keyword, &status);
			ffgtkn(infp, 4+ii, keyword, &axislen, &status);
			sprintf(context,"\n NAXES[%d]= %d", ii+1,
			    axislen);
			c_fcecho(context);

		}
		c_fcecho(" ");

	}

	/* Read the 'PROCEED' parameter from the parameter file 
	(The UCLGSB call is placed here instead in the gdelhdu routine,
	since the extension data has to be printed first if CONFIRM is set
	to YES) */

	Uclgsb("proceed", &proceed, &status);
	if(status)
	{
		sprintf(context, "Parameter 'proceed' not found in .par file");
		goto Exit_error;
	}
	
	/* If the user wishes to proceed, delete the CHDU in the infile */
	if(proceed) ffdhdu(infp, &hdutype, &status);
	if(status)
	{
	        ffrprt(stderr, status); 
		sprintf(context, "Error deleting the specified extension");
		goto Exit_error;
	}



Exit_error:
	if(status)
	{
		c_fcerr(context);
	}

	/* close the modified infile */
	status=0;
	if(inopen) ffclos(infp, &status);
	if(status)
	{
	        ffrprt(stderr, status); 
		sprintf(context, "Error closing the input FITS file");
		c_fcerr(context);
	}

	return;
}


/******************************************************************************
Function: gdelhdu

Notes:

Primary Local Variables:
    char *infile - name and extension of the source file
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

int gdelhdu(char *infile, int *confirm, int *status)
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
#define F77CALL fdelhdu
#endif
#ifdef unix
#define F77CALL fdelhdu_ 
#endif

void F77CALL()
{
	void Fdelhdu();

	Fdelhdu();
}
