/* Let's check to see if "ftools.h" has already been included, if it hasn't
   been then let's define all of the C MACRO calls and define XPI so that
   if this routine is called a second time it will not be processed. */

#ifndef FTOOLS_LOADED
#define FTOOLS_LOADED

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
   Fcpars in our C code if you look at the macro definition you see that the 
   PZTRING definition affects the 2nd element in the call 
   (STRING,PZTRING,PCINT,PCINT) and these are for variables 
   (infile,filename,extnumb,status) in the calling sequence. So Let us 
   say that we are going to parce and input file to figure out what the
   filename is and the extension number the comes with it.

   BufLen_2=255;
   Fcpars(Infile,Outfile,&extnumb,&status);

   Then this will require that the char array "Outfile" be declared as
   char Outfile[256]; or char Outfile[BufLen_2+1];

   Also note that "status" and "extnumb" are normal int declarations
   int extnumb, status=0;

   Note that if you call Fcpars again or any other routine that uses 
   BufLen_2 than it will still have the value 255 within that same function. 
   If you KNOW that all of your character arrays will be a fixed length it
   is recommended that you declare these "int" values as global or use
   #define BufLen_N XXX 
   and define all character arrays that will receive input to be
   char [BufLen_N+1] */

/*
   When using Fcgcls you will notice that 2 int variables MUST be set. 
   The first is of the form MaxElem_N where N is the argument number 
   (2 for Fcgcls) and the second necessary int variable needed is BufLen_2.
   You MUST declare these before the call and they must agree with the
   declaration of the char variable to receive the information. Since within
   the FORTRAN subroutine "colist" is an array of character*(*) it is
   necessary that we specify EXACTLY how long these strings are, BufLen_2,
   AND how many elements there are, MaxElem_2, withing the array. 
   Therefore for this subroutine call we would have something like:

   int numcols,negflag;
   int BufLen_2=255, MaxElem_2=999;
   char colist[MaxElem_2][BufLen_2+1];
   char columns[XXXXX];

   Fcgcls(columns,colist,&numcols,&negflag);
   
   Of course we could reset BufLen_2 and MaxElem_2 for subsequent calls 
   as long as they agree with the declaration for the SECOND argument in the
   call.

   */

/******************* Fcpars ********************/

/* These definitions are used in routines that return strings when
   the original string is empty. Remember that your C 
   character array to receive this string MUST have 1 more element! */

/* You MUST have a variable F_file defined in your C code to receive 
   the FITS file name defined to be char F_file[BufLen_2 +1].

   See above for more information.
*/

#define fcpars_ELEMLEN_2 ZTRINGV_NUM(BufLen_2)
#define Fcpars(infile,filename,extnumb,status) \
  CCALLSFSUB4(FCPARS,fcpars,STRING,PZTRING,PCINT,PCINT,infile,filename,extnumb,status)

/******************* Fcecho ********************/

#define Fcecho(comment) \
  CCALLSFSUB1(FCECHO,fcecho,STRING,comment)

/******************* Fcerr ********************/
/* Fcerr and Fcerrm require that ftoolstruct.h be included 
   to define the structure TASK and you must have the line 
   in your code:
 
   C2FCBSTR("string_for_taskname",TASK.taskname,0);
 
   where "string_for_taskname" is the name of your task.

   See ftoolstruct.h for more information 

   */
#define Fcerr(comment) \
  CCALLSFSUB1(FCERR,fcerr,STRING,comment)

/******************* Fcerrm ********************/
/* The COMMON block TASK must be defined for this to work - see above */

#define Fcerrm(status) \
  CCALLSFSUB1(FCERRM,fcerrm,INT,status)

/******************* Ffinit ********************/

#define Ffinit(ounit,filename,status) \
  CCALLSFSUB3(FFINIT,ffinit,INT,STRING,PCINT,ounit,filename,status)

/******************* Faopen ********************/

#define Faopen(ounit,filename,iomode,recl, status) \
  CCALLSFSUB5(FAOPEN,faopen,INT,STRING,INT,INT, PCINT,ounit,\
filename,iomode,recl,status)

/******************* Fitsdstk ********************/

#define Fitsdstk() \
    CCALLSFSUB0(FITSDSTK,fitsdstk)

/******************* Fccmpl ********************/
/* Since there are no LOGICAL declarations in C you will have to use
   an int declaration in your code.*/

#define Fccmpl(nolist1,nolist2,list1,list2,negflg,subset) \
    CCALLSFSUB6(FCCMPL,fccmpl,INT,INT,STRINGV,STRINGV,LOGICAL,PCLOGICAL,\
nolist1,nolist2,list1,list2,negflg,subset)

/******************* Fcgcls ********************/

/* This subroutine has 999 character strings HARDWIRED into the code.
   To use this routine you MUST allocate a character array "VAL" with:
   char VAL[MaxElem_2][BufLen_2+1];

   You MUST set these definitions within your code but
   be sure that you are doing things properly as unexpected 
   errors can result. 
   MaxElem_2 must be Less than or equal to 999*/

/* Since there are no LOGICAL declarations in C you will have to use
   an int declaration in your code.*/

#define fcgcls_ELEMS_2 ZTRINGV_NUM(MaxElem_2)
#define fcgcls_ELEMLEN_2 ZTRINGV_NUM(BufLen_2)

#define Fcgcls(columns,colist,numcols,negflag) \
    CCALLSFSUB4(FCGCLS,fcgcls,STRING,PZTRINGV,PCINT,PCLOGICAL,\
columns,colist,numcols,negflag)

/******************* Fcgrgs ********************/

/* rowrange1 and rowrange2 has a maximum array size of 15 */

#define Fcgrgs(rows,nrows,numranges,rowrange1,rowrange2)\
 CCALLSFSUB5(FCGRGS,fcgrgs,STRING,INT,PCINT,INTV,INTV,rows,nrows,numranges,rowrange1,rowrange2)

/******************* Cbdcr **********************/
#define Cbdcr(cbdstr,op,value,result,status)\
 CCALLSFSUB5(CBDCR,cbdcr,STRING,STRING,FLOAT,PCLOGICAL,PCINT,cbdstr,op,value,result,status)
 
/******************* Cbdcs *********************/
#define Cbdcs(cbdstr,value,result,status)\
 CCALLSFSUB4(CBDCS,cbdcs,STRING,STRING,PCLOGICAL,PCINT,cbdstr,value,result,status)
 
/******************* Ccnfg ********************/
#define Ccnfg(unit) CCALLSFSUB1(CCNFG,ccnfg,INT,unit)
 
/******************* Cgetlun *****************/
#define Cgetlun(unit) CCALLSFSUB1(CGETLUN,cgetlun,PCINT,unit)
 
/******************* Cpthnm ******************/

#define cpthnm_ELEMLEN_3  ZTRINGV_NUM(BufLen_3)

#define Cpthnm(disk,dir,file,status)\
 CCALLSFSUB4(CPTHNM,cpthnm,STRING,STRING,PZTRING,PCINT,disk,dir,file,status)
 
/******************* Ctrlog ******************/
#define ctrlog_ELEMLEN_3 ZTRINGV_NUM(BufLen_3)
#define Ctrlog(cbuf,lbuf,cret,lret)\
 CCALLSFSUB4(CTRLOG,ctrlog,STRING,INT,PZTRING,PCINT,cbuf,lbuf,cret,lret)

/******************* Dt2mjd ******************/
#define Dt2mjd(date,quiet,mjd,status)\
 CCALLSFSUB4(DT2MJD,dt2mjd,STRING,LOGICAL,PCDOUBLE,PCINT,date,quiet,mjd,status)

/******************* Ocnfg *******************/
#define ocnfg_ELEMLEN_3 ZTRINGV_NUM(BufLen_3)
#define Ocnfg(unit,quiet,config,status)\
 CCALLSFSUB4(OCNFG,ocnfg,INT,LOGICAL,PZTRING,PCINT,unit,quiet,config,status)

/******************* Pgfout ******************/
/* In order for this call to work the structure PGFCOUNT must
   be defined. You can do this by simply including ftoolstruct.h
   in your main routine. 
   */

#define Pgfout(ounit, ifout, context, status)\
 CCALLSFSUB4(PGFOUT,pgfout,INT,LOGICAL,STRING,PCINT,ounit,ifout,context,status)

/******************* Rcnfgl ******************/
#define rcnfgl_ELEMLEN_4 ZTRINGV_NUM(BufLen_4)
#define rcnfgl_ELEMLEN_5 ZTRINGV_NUM(BufLen_5)
#define rcnfgl_ELEMLEN_6 ZTRINGV_NUM(BufLen_6)
#define rcnfgl_ELEMLEN_7 ZTRINGV_NUM(BufLen_7)
#define rcnfgl_ELEMLEN_8 ZTRINGV_NUM(BufLen_8)
#define rcnfgl_ELEMLEN_9 ZTRINGV_NUM(BufLen_9)
#define rcnfgl_ELEMLEN_10 ZTRINGV_NUM(BufLen_10)
#define Rcnfgl(unit,quiet,lnum,misval,insval,cifdev,cifdir,cif,insdev,insdir,status)\
 CCALLSFSUB11(RCNFGL,rcnfgl,INT,LOGICAL,PCINT,PZTRING,PZTRING,PZTRING,PZTRING,PZTRING,PZTRING,PZTRING,PCINT,unit,quiet,lnum,misval,insval,cifdev,cifdir,cif,insdev,insdir,status)

/******************* Rdcnfg *****************/
#define rdcnfg_ELEMLEN_4 ZTRINGV_NUM(BufLen_4)
#define rdcnfg_ELEMLEN_5 ZTRINGV_NUM(BufLen_5)
#define Rdcnfg(mission,instr,quiet,cif,instdir,status)\
 CCALLSFSUB6(RDCNFG,rdcnfg,STRING,STRING,LOGICAL,PZTRING,PZTRING,PCINT,mission,instr,quiet,cif,instdir,status)

/******************* Tim2df *****************/
#define Tim2df(time,quiet,dayfrac,status)\
 CCALLSFSUB4(TIM2DF,tim2df,STRING,LOGICAL,PCDOUBLE,PCINT,time,quiet,dayfrac,status)

#endif
