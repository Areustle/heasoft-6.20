/******************************************************************************

File name: c_fcerr.c

Function name: c_fcerr

Description: C version of fcerr(). Write a string to stderr in IRAF-safe way

Author/Date: James Peachey, Hughes STX 6/96

Modification History: see log at EOF

Notes:	For c_fcerr to print properly the name of the task along with the
	error message, the statement:

	c_ptaskn("name of task");

	should be placed in the top-level c source file, where "name of task"
	would be the actual task name, e.g. "fdump".

Usage:
	#include "cftools.h"
	c_fcerr(errmsg);

Arguments:	char errmsg[]	-- error message

Main local variables: 
		int   BufLen_1             -- required for C calls to fortran
		char  context[C_FCERR_MSG] -- error msg, truncated if needed

******************************************************************************/

#include <string.h>
#include "cftools_internal.h"
#include "cftools.h"

#define SAFELEN(context) BufLen_1 - strlen(context)

/*****************************************************************************/

void c_fcerr(char *errmsg)
{
    int  BufLen_1 = C_FCERR_MSG - 1;
    char context[C_FCERR_MSG];

    if(!errmsg) return;
    c_gtaskn(context);
    c_trimstr(context, BufLen_1);
    strncat(context, " : ", SAFELEN(context));
    strncat(context, errmsg, SAFELEN(context));
    context[BufLen_1] = 0;
    Fxwrite(context, 6);
    return;
}

/******************************************************************************
$Log: c_fcerr.c,v $
Revision 1.7  1998/01/15 21:10:36  peachey
Updated comments

Revision 1.6  1997/10/23 20:38:26  peachey
Changed structure of included files to use a separate header file,
cftools_internal.h, which is internal to libcftools

Revision 1.5  1997/10/07 15:22:31  peachey
Use new methods c_gtask and c_ptask, rather than global variables, to get/put
taskname, for c_fcerr and fcerr

Revision 1.4  1997/10/03 20:38:16  peachey
Replaced TASK Fortran common block defined by ftoolstruct.h with a simple
C global string variable.

Revision 1.3  1997/10/03 18:29:21  peachey
Changed to call fxwrite directly, instead of calling fcerr. This required
addition of TASK common block macro, from ftoolstruct.h, and new utility,
c_trimstr. Added cvs $Log macro at the end of the file, to document future
changes automatically.

******************************************************************************/
