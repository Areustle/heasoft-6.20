/******************************************************************************

File name: c_fcpars.c

Function name: c_fcpars

Description: Supports CFITSIO extended syntax. This is meant to be used
             only by legacy Fortran FTOOLS (via a wrapper). New FTOOLS
             written in C should never need to call this, since parsing
             can now be done (if needed) by fits_parse_extnum / ffextn.
             Note that this now returns same filename as input so any
             tools using the returned filename for anything other than
             input to ffopen should beware...

Author/Date: M. Tripicco, 28May1998; original by J. Peachey, 4/96

Modification History: see log at EOF

Usage:	[status = ] c_fcpars(fullname, name, ext, status);

Arguments:	char fullname[]	-- input, e.g.: file.fits[SPECTRUM][CHANNEL .gt. 99]
		char name[]	-- output, identical to input!
		int* ext	-- output, *extension = 1
		int* status	-- input/output

Other functions called:
	c_fcerr(char* error_message);
    int ffextn(char *url, int *extension_num, int *status)


******************************************************************************/

#include <stdio.h>
#include <string.h>
#include "cftools.h"
#include "fitsio.h"
#include "cfortran.h"

/*****************************************************************************/

int c_fcpars(char *fullname, char *name, int *ext, int *status)
{
    char context[C_FCPARS_MSG];
    fitsfile *fptr;
    int hdunum;

    if(!status) return NOT_OK;
    else if(*status) return *status;
    else if(!fullname || !ext || !name)
    {
        *status = NOT_OK;
        strncpy(context, "Function c_fcpars called with null pointer.",
            C_FCPARS_MSG - 1);
        c_fcerr(context);
    }

    /* I/O filenames are identical */
    strcpy(name, fullname);

    ffextn(fullname, &hdunum, status); /* hdunum = -99 for non-specified ext */

    if (*status)
    {
        strcpy(context,"could not parse the input filename: (c_fcpars)");
        strcat(context,fullname);
	c_fcerr(context);
    }

    /* CFITSIO: primary == 1 vs. FTOOLS: primary == 0 */
    if (hdunum != -99) {
      *ext = hdunum - 1;
    } else {
      *ext = hdunum;
    }

    return *status;
}

FCALLSCSUB4(c_fcpars,FCPARS,fcpars,STRING,PSTRING,PINT,PINT)

/*
$Log: c_fcpars.c,v $
Revision 1.6  1998/06/17 15:55:18  miket
ffextn call changed to match cfitsio change (added status argument)

 * Revision 1.5  1998/06/02  14:50:38  miket
 * wrapper now lives inside c_fcpars.c
 *
Revision 1.4  1998/06/01 21:10:33  miket
gutted to now use ffextn -- this routine is now called by a wrapper for fcpars()

 * Revision 1.3  1997/10/03  18:18:13  peachey
 * Added cvs $Log macro at the end of the file, to document future changes
 * automatically.
 *
*/

