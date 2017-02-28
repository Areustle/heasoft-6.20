/*
 * $Id: xpi.h,v 3.7 2005/07/14 18:52:11 irby Exp $
 * $Log: xpi.h,v $
 * Revision 3.7  2005/07/14 18:52:11  irby
 * Try again to fix cvs Id & Log keyword comments.
 *
 * Revision 3.6  2005/07/14 17:39:28  irby
 * Clean up comments within comments in lines 3-5.
 *
 * Revision 3.5.1.1  1996/04/16 01:39:33  dunfee
 * Start of pristine ftools CVS...
 *
 * Revision 1.2  1995/04/14  15:20:58  oneel
 * BEO First version
 *
 * Revision 1.1  1995/04/14  15:20:19  oneel
 * Initial revision
 */

/* Let's check to see if "xpi.h" has already been included, if it hasn't
   been then let's define all of the C MACRO calls and define XPI so that
   if this routine is called a second time it will not be processed.  */

#ifndef XPI_LOADED
#define XPI_LOADED

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

#include "pfile.h"

/*
 * These are the #defines for the FIXED sized strings and are defined
 in pctype.h. We repeat them here (commented out) simply for ease of 
 understanding the following.

 #define FITS_CLEN_COMMENT   73
 #define FITS_FLEN_COMMENT   72
 #define FITS_CLEN_KEYVAL    71
 #define FITS_FLEN_KEYVAL    70
 #define FITS_CLEN_CARD      81
 #define FITS_FLEN_CARD      80
 #define FITS_CLEN_KEYNAME    9
 #define FITS_FLEN_KEYNAME    8
 #define FITS_CLEN_HDEFKWDS  25
 #define FITS_FLEN_HDEFKWDS  24
 #define FITS_CLEN_ERRMSG    31
 #define FITS_FLEN_ERRMSG    30
 */

/***********************************************************************/

/*

   Whenever PZTRING is used a BufLen_N variable (where N is the number 
   of the argument in the call) MUST be specified. Since FORTRAN passes a 
   length of the character array with the subroutine function and C doesn't
   we must pass this information manually. Also since C adds a "null" (/0)
   at the end of a character array the C character array MUST be set to be
   ONE (1) more than the value sent to the FORTRAN subroutine. Thus if 
   you declare the "int BufLen_1=255" you will have to also declare the
   C character array as "char Carray[BufLen_1+1]" in order for the PZTRING
   declaration to function properly. So for instance say we are going to call
   Uclgst in our C code if you look at the macro definition you see that the 
   PZTRING definition affects the 2nd element in the call 
   (STRING,PZTRING,PCINT) and these are for variables (P, B, S) in the 
   calling sequence. So Let us say that we are going to read from the PAR 
   file the character string that goes with Filename. And our calling 
   sequence looks like:

   BufLen_2=255;
   Uclgst("Filename",Input,&status);

   Then this will requite that the char array "Input" be declared as
   char Input[256]; or BufLen_2+1 
   Also note that "status" is a normal int declaration
   int status=0;

   Note that if you call Uclgst again or any other routine that uses 
   BufLen_2 than it will still have the value 255 within that same function. 
   If you KNOW that all of your character arrays will be a fixed length it
   is recommended that you declare these "int" values as global or use
   #define BufLen_N XXX 
   and define all character arrays that will receive input to be
   char Input_array[BufLen_N+1] */

/* This is the Beginning of the CFORTRAN macros that define the 
 * XPI calls...
 */

/******************** Tbldstandiraf ********************/

#define Tbldstandiraf(file) \
  CCALLSFSUB1(TBLDSTANDIRAF,tbldstandiraf,STRING,file)

/******************** Gtbufsetcmdline ********************/

#define Gtbufsetcmdline(cmdline) \
  CCALLSFSUB1(GTBUFSETCMDLINE,gtbufsetcmdline,STRING,cmdline)

/******************** Gtcom2 *******************/

#define gtcom2_ELEMLEN_1 ZTRINGV_NUM(BufLen_1)
#define gtcom2_ELEMLEN_4 ZTRINGV_NUM(BufLen_4)
#define Gtcom2(string,parse,prompt,command,comnum,disk,dir,name,version,idone)\
 CCALLSFSUB10(GTCOM2,gtcom2,PZTRING,PCINT,STRING,PZTRING,PCINT,STRING,STRING,STRING,STRING,PCINT,string,parse,prompt,command,comnum,disk,dir,name,version,idone)

/******************** Gtcom2_nolog *************/
#define Gtcom2_nolog() CCALLSFSUB0(GTCOM2_NOLOG,gtcom2_nolog)

/******************** Uclgno *******************/
#define Uclgno(ncmds) CCALLSFSUB1(UCLGNO,uclgno,PCINT,ncmds)

/******************** Uclgot *******************/
#define Uclgot(parname,status)\
 CCALLSFSUB2(UCLGOT,uclgot,STRING,PCINT,parname,status)

/******************** Xpisavepar ***************/
#define Xpisavepar(status) CCALLSFSUB1(XPISAVEPAR,xpisavepar,PCINT,status)

#endif
