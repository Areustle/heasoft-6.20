/******************************************************************************

Filename:
     fdelete.h

Description:
      Header file containing common definitions and function prototypes used
      in the functions comprising the tool fdelete.

Author/Date: toliver / May, 1999

Modification History:

$Log: fdelete.h,v $
Revision 1.1  1999/06/01 18:05:27  toliver
initial revision


Notes:	

Usage:
     #include "fdelete.h"

******************************************************************************/

#include <cftools.h>
#include <ftools.h>
#include <fitsio.h>

#define BUFSIZE 128
#define CBUFSIZE BUFSIZE+1

/*
** Function prototypes.
*/

int fdelete (void);

int fdelete_get_params (char *,
                         int *);
