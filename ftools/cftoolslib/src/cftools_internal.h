/******************************************************************************

File name: cftools_internal.h

Description: Header file containing declarations needed for use
             within the cftools library.

Author/Date: James Peachey, HEASARC/GSFC/NASA, Hughes STX, October, 1997

Usage: DO NOT USE THIS FILE!

Modification History: see log at EOF

Notes: This file is not intended for use outside of the cftools library.
       When writing code which calls the functions in libcftools, only
       the statement:

       #include "cftools.h"

       is needed.

******************************************************************************/

#include "cfortran.h"

#define Fxwrite(context, action) \
        CCALLSFSUB2(FXWRITE, fxwrite, STRING, INT, context, action)

/******************************************************************************
$Log: cftools_internal.h,v $
Revision 1.3  1998/06/02 14:49:15  miket
reversed last change...

Revision 1.2  1998/06/01 21:09:28  miket
added wrapper to translate calls to fcpars() into c_fcpars()

Revision 1.1  1997/10/23 20:38:30  peachey
Changed structure of included files to use a separate header file,
cftools_internal.h, which is internal to libcftools


******************************************************************************/
