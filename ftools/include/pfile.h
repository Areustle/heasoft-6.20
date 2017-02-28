#ifndef PFILE_LOADED
#define PFILE_LOADED

#include "cfortran.h"

#include "pctype.h"

/******************** Uclgsb  ********************/
/* Since there are no LOGICAL declarations in C you will have to use
   an int declaration and pass that pointer */
#define Uclgsb(parname,buffer,status) \
  CCALLSFSUB3(UCLGSB,uclgsb,STRING,PCLOGICAL,PCINT,parname,buffer,status)

/******************** Uclgsd ********************/

#define Uclgsd(parname,buffer,status) \
  CCALLSFSUB3(UCLGSD,uclgsd,STRING,PCDOUBLE,PCINT,parname,buffer,status)

/********************  uclgsi ********************/

#define Uclgsi(parname,buffer,status) \
  CCALLSFSUB3(UCLGSI,uclgsi,STRING,PCINT,PCINT,parname,buffer,status)

/******************** Uclgsl ********************/

#define Uclgsl(parname,buffer,status) \
  CCALLSFSUB3(UCLGSL,uclgsl,STRING,PCDOUBLE,PCINT,parname,buffer,status)

/******************** Uclgsr ********************/

#define Uclgsr(parname,buffer,status) \
  CCALLSFSUB3(UCLGSR,uclgsr,STRING,PCFLOAT,PCINT,parname,buffer,status)

/******************** Uclgss ********************/

#define Uclgss(parname,buffer,status) \
  CCALLSFSUB3(UCLGSS,uclgss,STRING,PCINT,PCINT,parname,buffer,status)

/******************** Uclgst ********************/

/* These definitions are used in routines that return strings when
   the original string is empty. Remember that your C character array 
   that is to receive this string MUST have 1 more element than the FORTRAN
   to allow for the null character!  

   You must define BufLen_2 to be the length of your C character array 
   minus one. A convenient length is 255 for BufLen_ and 256 for the 
   declared length of your C character array.

   You MUST have a char variable F_file defined in your C code to receive 
   the FITS file name defined to be char F_file[BufLen_2 + 1].

   See above for more information*/

#define uclgst_ELEMLEN_2 ZTRINGV_NUM(BufLen_2)
#define Uclgst(P,B,S) \
  CCALLSFSUB3(UCLGST,uclgst,STRING,PZTRING,PCINT,P,B,S)

/******************** Uclpsb ********************/
/* Since there are no LOGICAL declarations in C you will have to use
   an int declaration and pass that value */

#define Uclpsb(parname,buffer,status) \
  CCALLSFSUB3(UCLPSB,uclpsb,STRING,LOGICAL,PCINT,parname,buffer,status)

/******************** Uclpsd ********************/

#define Uclpsd(parname,buffer,status) \
  CCALLSFSUB3(UCLPSD,uclpsd,STRING,DOUBLE,PCINT,parname,buffer,status)

/******************** Uclpsi ********************/

#define Uclpsi(parname,buffer,status) \
  CCALLSFSUB3(UCLPSI,uclpsi,STRING,INT,PCINT,parname,buffer,status)

/******************** Uclpsl ********************/

#define Uclpsl(parname,buffer,status) \
  CCALLSFSUB3(UCLPSL,uclpsl,STRING,LONG,PCINT,parname,buffer,status)

/******************** Uclpsr ********************/

#define Uclpsr(parname,buffer,status) \
  CCALLSFSUB3(UCLPSR,uclpsr,STRING,FLOAT,PCINT,parname,buffer,status)

/******************** Uclpss ********************/

#define Uclpss(parname,buffer,status) \
  CCALLSFSUB3(UCLPSS,uclpss,STRING,INT,PCINT,parname,buffer,status)

/******************** Uclpst ********************/

#define Uclpst(parname,buffer,status) \
  CCALLSFSUB3(UCLPST,uclpst,STRING,STRING,PCINT,parname,buffer,status)

#endif
