/******************************************************************************

File name: c_trimstr.c

Function name: c_trimstr

Description: Strip trailing blank characters (space, tab, etc.) off a string

Author/Date: James Peachey, Hughes STX, 4 October, 1997 

Modification History: see log at EOF

Notes:	

Usage:	c_trimstr(string, length);

Arguments:	char string[]	-- string to be trimmed
		int length	-- length of this string

******************************************************************************/

#include <ctype.h>
#include <string.h>

char *c_trimstr(char *buf, int length)
{
    if(length < 1) return buf;
    while(--length) {
        if(!buf[length] || isspace(buf[length])) buf[length] = 0;
        else return buf;
    }
}

/*
$Log: c_trimstr.c,v $
Revision 1.2  2013/08/28 15:19:27  irby
Non-void functions should return a value.  These were flagged as
compile errors under Mac OSX 10.9 (Mavericks).

Revision 1.1  1997/10/03 18:34:02  peachey
New utility to remove blank characters from end of a string

*/
