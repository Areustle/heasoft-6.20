/******************************************************************************

File name: c_ptaskn.c

Function name: c_ptaskn

Description: C version of ptaskn(). Method to get name of current task.

Author/Date: James Peachey, HEASARC/GSFC/NASA, Hughes STX, October, 1997

Modification History: see log at EOF

Notes:	

Usage:
	#include "cftools.h"
	c_ptaskn(taskname);

Arguments:	char *name	-- buffer containing new task name. Only
				   C_TASKNAME characters (see cftools.h)
				   will be used. The rest will be truncated

Main local variables: 
		int   BufLen_1             -- required for C calls to fortran

******************************************************************************/

#include "cftools_internal.h"
#include "cftools.h"

#define Ptaskn(name) CCALLSFSUB1(PTASKN, ptaskn, STRING, name)

/*****************************************************************************/

void c_ptaskn(char *name)
{
    int  BufLen_1 = C_TASKNAME - 1;

    if(!name) return;
    BufLen_1 = strlen(name) < BufLen_1 ? strlen(name) : BufLen_1;
    Ptaskn(name);
    return;
}

/******************************************************************************
$Log: c_ptaskn.c,v $
Revision 1.3  1998/01/15 21:14:52  peachey
Pass minimum length of task name to Fortran ptaskn

Revision 1.2  1997/10/23 20:38:29  peachey
Changed structure of included files to use a separate header file,
cftools_internal.h, which is internal to libcftools

Revision 1.1  1997/10/07 15:20:17  peachey
New methods added to put/get the taskname used by c_fcerr/fcerr

******************************************************************************/
