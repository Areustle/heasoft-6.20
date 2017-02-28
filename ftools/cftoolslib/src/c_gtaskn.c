/******************************************************************************

File name: c_gtaskn.c

Function name: c_gtaskn

Description: C version of gtaskn(). Method to get name of current task.

Author/Date: James Peachey, Hughes STX 6/96

Modification History: see log at EOF

Notes:	

Usage:
	#include "cftools.h"
	c_gtaskn(taskname);

Arguments:	char *name	-- buffer in which task name will
				   be stored. Calling routine must allocate
				   this space for at least C_TASKNAME
				   characters (see cftools.h)

Main local variables: 
		int   BufLen_1             -- required for C calls to fortran

******************************************************************************/

#include <string.h>
#include "cftools_internal.h"
#include "cftools.h"

#define Gtaskn(name) CCALLSFSUB1(GTASKN, gtaskn, PSTRING, name)

/*****************************************************************************/

char *c_gtaskn(char *taskname)
{
    int  BufLen_1 = C_TASKNAME - 1;
    char buf[C_TASKNAME];

    if(!taskname) return 1;
    Gtaskn(buf);
    strncpy(taskname, buf, C_TASKNAME);

    return taskname;
}

/******************************************************************************
$Log: c_gtaskn.c,v $
Revision 1.3  2013/08/28 15:19:27  irby
Non-void functions should return a value.  These were flagged as
compile errors under Mac OSX 10.9 (Mavericks).

Revision 1.2  1997/10/23 20:38:27  peachey
Changed structure of included files to use a separate header file,
cftools_internal.h, which is internal to libcftools

Revision 1.1  1997/10/07 15:20:17  peachey
New methods added to put/get the taskname used by c_fcerr/fcerr

******************************************************************************/
