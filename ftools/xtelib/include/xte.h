/* Let's check to see if "xtelib.h" has already been included, if it hasn't
   been then let's define all of the C MACRO calls and define XPI so that
   if this routine is called a second time it will not be processed. */

#ifndef XTE_LOADED
#define XTE_LOADED

/* Here we check to see if cfortran.h and pctype.h have been included 
   as these are needed to define the variables the definitions of
   INT, PCINT, STRING, PZTRING, etc... If they haven't already been
   included in your code then we will include them at this time. */

/* cfortran.h acts as a transparent interface between C and FORTRAN 
   subroutines. This HEADER file defines several macros that will be 
   translated into the proper FORTRAN calls. This file is designed to 
   work with pctype.h as well as cfortran.h so that we check to see if 
   these header files as defined and if they aren't then we define them. 
   */

#include "cfortran.h"

/* pctype.h defines a new CFORTRAN type that allows the C-User to pass a 
 * pointer to an integer, float, or double directly to the FORTRAN routine, 
 * so that you can say
 *
 * FCGERR(...,&status)
 *
 * which is the correct C-usage...
 * It also introduces the PZTRING type, a fixed length string type.
 * It also corrects some of the PZTRING definitions so that a dynamically
 * constructed array can be passed.
 */

#include "pctype.h"

/******************* int2forlog ********************/

/* This routine will read in an array of integers containing 
   either 0 or 1 and and will output a FALSE (0) for each 0 found and 
   a TRUE (1) value for each 1 found. This routine is mostly for use
   in C routines that need to output an array of logicals.*/

#define Int2log(ounit,colnum,frow,nelem,valarray,status) \
    CCALLSFSUB6(INT2LOG,int2log,INT,INT,INT,INT,INTV,PCINT,ounit,colnum,frow,nelem,valarray,status)

#endif
