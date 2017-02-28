/******************************************************************************

File name: fexpoadapt.c

Task name: fexpoadapt

Description:	Template for creating a C ftool. Demonstrates 
		parameter file handling, and proper output to 
		stdout and stderr.

Author/Date: FTOOL version by Koji Mukai August 2003
	     	Fitting Ed Pier's ASCA processing tool into
	        "cdummyftool" template by James Peachey.

/*
Usage:	fexpoadapt image expmap cdummyftool dummy (host)

Arguments: none

Functions implemented in this file:
	int ReadParameters(char* dummy); /* reads 'dummy' from .par file
	void DoWork(char* dummy, int status); /* writes 'dummy' to screen
	void fexpoadapt(); /* This is the top level function. 

Library routines called:
	Uclgst(char* param name, char* param var, int* status);
	c_fcerr(char* error_message);
	c_fcecho(char* user_message);
	c_ptaskn(char* taskname);
******************************************************************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>

#include <fitsio.h>	/* Required for FLEN_FILENAME definition.  */
#include <xpi.h>	/* Required for parameter file operations. */
#include <cftools.h>	/* Required to use c_fcerr, c_fcecho, etc. */

#include "image.h"

int ReadVsmoothParams(char *image_file, char *expo_file, char *smoothed_file,
                      float *min_counts, float *min_expo)
{
	int status = OK;
	char context[FLEN_FILENAME];

	int BufLen_2 = FLEN_FILENAME - 1; /* Required for C calls to Uclxxx. */

/* new code */
/* Augment/replace the following, as needed, with other calls to 
parameter utilities, following this format. */

	Uclgst("image", image_file, &status); /* Gets a string param. */
	if(status != OK)
	{
		strcpy(context,"Parameter 'image' not found in .par file.");
		c_fcerr(context);
	}
	Uclgst("expomap", expo_file, &status); /* Gets a string param. */
	if(status != OK)
	{
		strcpy(context,"Parameter 'expomap' not found in .par file.");
		c_fcerr(context);
	}
	Uclgst("smooth", smoothed_file, &status); /* Gets a string param. */
	if(status != OK)
	{
		strcpy(context,"Parameter 'smooh' not found in .par file.");
		c_fcerr(context);
	}
	Uclgsr("mincounts", min_counts, &status); /* Gets a string param. */
	if(status != OK)
	{
		strcpy(context,"Parameter 'mincounts' not found in .par file.");
		c_fcerr(context);
	}
	Uclgsr("minexpo", min_expo, &status); /* Gets a string param. */
	if(status != OK)
	{
		strcpy(context,"Parameter 'minexpo' not found in .par file.");
		c_fcerr(context);
	}
	return status;
}

void DoVsmooth(char *image_file, char *expo_file, char *smoothed_file,
                      float *min_counts, float *min_expo, int *status)
{
	/*********************************************************************
	* This is a boxcar smoothing routine with a variable size box. The box
	* has the minimum size which contains n (such as 50) counts
	* minexpo is the optional lower-limit exposure value below which
	* the output image value is zero.
	**********************************************************************/
	IMAGE* im;
	IMAGE* expo;
	IMAGE* smoothed;

	int dimenx, dimeny;

	char context[FLEN_FILENAME];

	/**************
	* read images *
	**************/
	im = readImage(image_file, *status);
	if(*status != OK)
	{
		strcpy(context, "Error reading input image file.");
		c_fcerr(context);
		return;
	}
	expo = readImage(expo_file, *status);
	if(*status != OK)
	{
		strcpy(context,"Error reading exposure map.");
		c_fcerr(context);
		return;
	}

	/*********
	* smooth *
	*********/
	smoothed = adaptivelySmoothImageWithExposure(im, expo,
                            *min_counts, *min_expo, *status );
	if(*status != OK)
	{
		strcpy(context,"Error smoothing image.");
		c_fcerr(context);
		return;
	}


	/***************************
	* write the smoothed image *
	***************************/
	*status = writeImage(smoothed, smoothed_file);
	if(*status != OK)
	{
		strcpy(context,"Error writing smoothed image.");
		c_fcerr(context);
	}

	return;
}

void fexpoadapt()
{
	char image_file[FLEN_FILENAME];
	char expo_file[FLEN_FILENAME];
	char smoothed_file[FLEN_FILENAME];

	float min_counts;
	float min_expo;
	int status;

	char context[FLEN_FILENAME];

/* The following call must be used to set the taskname, for use by
   c_fcerr and other routines in the ftools libraries */
	c_ptaskn("fexpoadapt"); /* used by c_fcerr */

/* User may treat this function like main() for this ftool. */

	status = ReadVsmoothParams( image_file, expo_file, smoothed_file,
                                    &min_counts, &min_expo);
	if(status != OK)
	{
		strcpy(context,"Error reading parameter file.");
		c_fcerr(context);
		return;
	}

        DoVsmooth(image_file, expo_file, smoothed_file,
                      &min_counts, &min_expo, &status);

	return;
}

/* The following code is needed to allow IRAF to call cdummyftool.
This extra subroutine layer is needed because of differences in
linkers between vms and unix. */
#ifdef vms
#define F77CALL cdumftool
#endif
#ifdef unix
#define F77CALL cdumftool_
#endif

void F77CALL()
{
	void fexpoadapt();
	fexpoadapt();
}
