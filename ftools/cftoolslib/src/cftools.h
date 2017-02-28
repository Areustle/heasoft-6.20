/******************************************************************************

File name: cftools.h

Description: Header file containing definitions used by routines calling
             functions in the cftools library, as well as by the library
             functions themselves.

Author/Date: James Peachey, HEASARC/GSFC/NASA, Hughes STX, October, 1997

Modification History: see log at EOF

Notes:

Usage: #include "cftools.h"

******************************************************************************/
#ifndef _CFTOOLS_H
#define _CFTOOLS_H
#include "fitsio.h"

#define NOEXTENSION	-99
#define ALLEXTENSIONS	-1
#define OK		0
#define NOT_OK		1

#define C_FCECHO_MSG	1024
#define C_FCERR_MSG	1024
#define C_FCPARS_MSG	1024
#define C_TASKNAME	41

/*****************************************************************************/

void	c_fcecho(char *errmsg);
void	c_fcerr(char *errmsg);
int	c_fcpars(char *fullname, char *name, int *ext, int *status);
char*	c_gtaskn(char *name);
void	c_ptaskn(char *name);
char*	c_trimstr(char *buf, int len);
int     c_timestamp(fitsfile *fptr);

/******************************************************************************
$Log: cftools.h,v $
Revision 1.14  2007/05/21 20:46:07  irby
Increase C_FCECHO_MSG, C_FCERR_MSG, C_FCPARS_MSG.  In e.g. fdiff, long
filenames are being truncated in the filename echo.

Revision 1.13  1998/07/27 19:00:13  peachey
Remove prototypes for routines which are moving elsewhere; conditional include

Revision 1.12  1998/07/27 17:22:34  ganning
Add the prototype of c_timestamp.

Revision 1.11  1998/06/26 17:54:57  guerber
Added prototype for new gttime function.

Revision 1.10  1998/06/10 18:24:13  ganning
Added "include fitsio.h" statement and the prototypes of c_copyhead,
c_copyheadn, fits_comment.

Revision 1.9  1997/10/23 20:38:30  peachey
Changed structure of included files to use a separate header file,
cftools_internal.h, which is internal to libcftools

Revision 1.8  1997/10/07 15:22:32  peachey
Use new methods c_gtask and c_ptask, rather than global variables, to get/put
taskname, for c_fcerr and fcerr

Revision 1.7  1997/10/03 20:38:16  peachey
Replaced TASK Fortran common block defined by ftoolstruct.h with a simple
C global string variable.

Revision 1.6  1997/10/03 18:33:29  peachey
Removed #include directives, to avoid unnecessary clutter in programs using
this header file. In particular, ftools.h is no longer needed at all, since
fcerr and fcecho are no longer called by this library. Added cvs $Log macro
at the end of the file, to document future changes automatically.

******************************************************************************/
#endif
