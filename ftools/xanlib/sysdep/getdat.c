/******************************************************************************

File name: getdat.c

Function name: getdat

Description: Return date in form dd-mmm-yyyy, using ANSI C time library
             calls. Replaces Fortran subroutine GETDAT, from files sys.*

Author/Date: James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 17 July, 1998

Modification History: see log at EOF

Notes:

Usage:	getdat(char* date); (C)
        call getdat(character*(*) date) (Fortran)
******************************************************************************/
#include <time.h>
#include "cfortran.h"

#define DATE_SIZE 12

void getdat(char* datestring)
{
    time_t now_time_t;
    struct tm* now_tm;

    now_time_t = time(NULL);
    now_tm = localtime(&now_time_t);
    strftime(datestring, DATE_SIZE, "%d-%b-%Y", now_tm);

    return;
}
FCALLSCSUB1(getdat, GETDAT, getdat, PSTRING)

/******************************************************************************
$Log: getdat.c,v $
Revision 3.1  1998/07/21 20:46:38  peachey
Y2K compliance changes, associated with the replacement of the
system-dependent subroutine GETDAT with a system-independent C
function (getdat.c) which uses only ANSI C date functions, and
returns a date in the format dd-mmm-yyyy.

*******************************************************************************/
