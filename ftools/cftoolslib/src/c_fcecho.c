/******************************************************************************

File name: c_fcecho.c

Function name: c_fcecho

Description: C version of fcecho(). Puts a string to stdout in IRAF-safe way

Author/Date: James Peachey, Hughes STX 6/96

Modification History: see log at EOF

Notes:	

Usage:	c_fcecho(msg);

Arguments:	char msg[]	-- message

Main local variables: 
		int   BufLen_1             -- required for C calls to fortran
		char  context[C_FCECHO_MSG] -- truncated error msg

******************************************************************************/

#include <string.h>
#include "cftools_internal.h"
#include "cftools.h"

/*****************************************************************************/

void c_fcecho(char *msg)
{
    int  BufLen_1 = C_FCECHO_MSG - 1;
    char context[C_FCECHO_MSG];

    if(!msg) return;
    strncpy(context, msg, BufLen_1);
    context[BufLen_1] = 0;
    Fxwrite(context, 5);
    return;
}

/******************************************************************************
$Log: c_fcecho.c,v $
Revision 1.4  1997/10/23 20:38:25  peachey
Changed structure of included files to use a separate header file,
cftools_internal.h, which is internal to libcftools

Revision 1.3  1997/10/07 15:22:31  peachey
Use new methods c_gtask and c_ptask, rather than global variables, to get/put
taskname, for c_fcerr and fcerr

Revision 1.2  1997/10/03 18:26:58  peachey
Changed to call fxwrite directly instead of calling fcecho. Added cvs $Log
macro at the end of the file, to document future changes.

******************************************************************************/
