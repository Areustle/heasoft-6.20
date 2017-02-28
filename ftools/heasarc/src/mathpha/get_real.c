/****************************************************************************

  get_real:
    Use strtod to attempt to convert a string to a real value.
    If successful, return the value in realval. Otherwise, set
    isitreal to .false. (0)

    MODIFICATION HISTORY:

    James Peachey, HEASARC/GSFC/NASA, (1.0.0:1999 June 09), original

    James Peachey, HEASARC/GSFC/NASA, (1.0.1:1999 June 30),
        . Check whether string starts with + or - or . or 0-9
          before trying internal read.

    James Peachey, HEASARC/GSFC/NASA, (1.1.0:2000 Apr 11),
        . Use formatted internal read for finding the real value.
          Do not tolerate any characters after the real value.

    Bryan Irby, HEASARC/GSFC/NASA, (2.0.0 : 2003 Sep 30),
        . Split get_real subroutine out of mathpha.f and reworked in C
          instead of Fortran to get around f90 problem with the internal read.
 
    Passed Parameters
      chatter         i  : Chattiness flag
      string          i  : String to be converted
      realval         o  : Output real variable -- only assigned if
                           conversion is successful
      isitreal        o  : Status of conversion (.false. [0] if string
                           could not be converted to real)
 
****************************************************************************/

#define BUFSIZE 81

#include <string.h>
#include <float.h>
#include "cfortran.h"

void get_real(int chatter, char *string, float *realval, int *isitreal);

void get_real(int chatter, char *string, float *realval, int *isitreal)
{
    double rtmp;
    char *leftovers;
    char message[BUFSIZE], *subname, *version;

    subname="get_real";
    version="2.0.0";

    if ( chatter >= 10 ) {
        sprintf(message," ...... using %s Ver %s",subname,version);
        c_fcecho(message);
    }

    *isitreal = 0;

    rtmp = strtod(string, &leftovers);

    if ( strcmp(leftovers,"") == 0 && rtmp < FLT_MAX ) {
	*realval = rtmp;
	*isitreal = 1;
    }
}

FCALLSCSUB4(get_real,GET_REAL,get_real,INT,STRING,PFLOAT,PLOGICAL)
