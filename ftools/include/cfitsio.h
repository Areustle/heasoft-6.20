#define CFITSIO_VERSION_STRING  "1.30"

/* CFITSIO.H
 *
 * Version  Date       Author      Reason
 * ---------------------------------------------------------------------------
 *   1.0   11/28/94    J. Ingham   Original.
 *   1.1   12/28/94    W. Pence    Added definitions for new FITSIO subroutines
 *                                 and set default length of string keywords=68
 *   1.2   01/04/95    W. Pence    defined bytes as strings and added FTx3Dx 
 *   1.21  01/27/95    W. Pence    added 3 routines: FCIREC, FCDREC, FCGHPS 
 *   1.22  01/30/95    W. Pence    fixed numerous typos in the code.
 *   1.23  03/17/95    W. Pence    read or write bytes now use BYTEV not PZTRING
 *   1.24  04/13/95    W. Pence    FTGCVS and FTGCFS changed back to STRING
 *   1.25  04/14/95    W. Pence    added FTICOL and FTDCOL
 *   1.26  04/14/95    W. Pence    added FTDHDU
 *   1.27  04/28/95    W. Pence    added FTUCRD
 *   1.28  05/17/95    W. Pence    added FTGHAD, FTIIMG, FTITAB, FTIBIN, 
 *                                       FTUCKS, FTESUM, FTDSUM
 *   1.29  09/08/95    W. Pence    fixed the FCGHPS macro (had wrong arguments)
 *   1.30  09/25/95    W. Pence    fixed datatypes for SHORT & LOGICAL arguments
 * ----------------------------------------------------------------------------
 * 
 * This is the header file that defines the interface between C and the
 * FORTRAN fitsio routines.
 *
 * It includes the file pctype.h, which adds some types to CFORTRAN,
 * and overrides a few of the definitions.  This way, I can avoid actually
 * changing the cfortran.h file...
 *
 * Because of the requirements of CFORTRAN, the following rules must
 * be followed:
 *
 *  For SINGLE STRINGS:
 *  ------------------------------------------------------------
 *  If the string is used to pass a value into the routine, which will not 
 *  be altered, then no particular care need be taken.  Just make sure the
 *  string is a proper null-terminated string.
 *
 *  If the string will be altered:
 *
 *  If the argument is the i th argument to the function FUN, use
 * 
 *            char string[DIM+1];
 *
 *  where DIM is the number that appears in the FUN_ELEMLEN_i macro
 *  for that function.  If the string's maximum length is not known from 
 *  FITS considerations, then the ELEMLEN macro will contain an int 
 *  variable name which must hold the string length.  In CFITSIO, this will
 *  always be 'FitsStrBufLen'.
 *
 *     
 *  VECTORS of STRINGS:
 *  ------------------------------------------------------------
 *
 *  Restrictions on string types:
 *  ----------------------------
 *
 *     Fortran requires a CONTIGUOUS block of memory for a string array.
 * 
 *     This leaves you with two choices:
 *
 *     1) A vector defined as:
 *
 *
 *     char strings[DIM1][DIM2];
 *
 *     In CFORTRAN this is the only string Vector that 
 *     can  be passed to FORTRAN.
 *
 *     2) Dynamically dimensioned arrays...
 *
 *     pctypes.h contains a few definitions that allow you to use memory 
 *     allocated with the trick:
 *
 *	blockPtr = ( char **) malloc ( nelements * sizeof( char *));
 *	blockPtr[0] = (char *) malloc ( stringLength * 
 *					nelements * sizeof(char) );
 *	for( i = 1; i < nelements; i++ ) 
 *	    blockPtr[i] = blockPtr[0] + i * stringLength;
 *
 *      If you do this, then when you call the cfortran macro, 
 *      you have to call it as:
 *      
 *      FCGCVS(...,blockPtr[0],...);
 *
 *      NOT AS
 *
 *      FCGCVS(...,blockPtr,...);
 *
 *      The mnemonic for this is that you have to pass FORTRAN the pointer
 *      that ACTUALLY points to the block of memory, FORTRAN cannot follow
 *      the pointers.
 *
 *  Number of Elements
 *  ------------------
 *
 *     In all cases in CFITSIO, the maximum number of elements that will be
 *     used is an argument to the function.  CFITSIO passes this information
 *     to CFORTRAN...  Your arrays can have more elements, but NEVER less. 
 *
 *  String Lengths
 *  --------------
 *
 *     There are two kinds of vector arrays used in CFITSIO, known maximum
 *     length, and variable length string arrays.
 *
 *     a) KNOWN length:
 *        In some cases the maximium length of the string is known in advance.
 *        Here are the common examples ( remember we must add one to the actual
 *        length for the concluding null):
 *
 *         keyword namnes         - keyname[9] 
 *         keyword values         - keyval[69]  (max length of a quoted string)
 *         string keyword values  - strval[69]  (see note below)
 *         keyword comments       - comment[73]
 *         header cards           - card[81]
 *         tform, ttype, tunit, tdisp - value[25]  (by convention, these 
 *                                                 keyword values should be no
 *                                                 longer than 24 characters)
 *         FITSIO error messages  - errmsg[31]
 *         ASCII encoded checksum - checksum[17]
 *         coordinate axis type   - ctype[5]
 *
 *     There are #defines for all of these as well...
 *
 *     [Note: The FCGKYS and FCGKNS routines support a convention in which long
 *            string keyword values may be continued over multiple keywords.  
 *            The FITS_FLEN_STRVAL variable defines the maximum length of the
 *            returned string (it will be truncated to this length if it is 
 *            longer).  By default this value is set to 68;  application
 *            programs must redefine this variable if they wish to read
 *            longer string values.]
 *
 *     These routines, e.g. FCGHTB, are called as follows:
 *
 *     int iunit,tfields,varidat,status;
 *     char ttype[MXELEM][25],tform[MXELEM][25],tunit[MXELEM][25],extname[69];
 *
 *     FCGHBN(iunit,MXELEM,nrows,&tfields,ttype,tform,tunit,
 *                                  extname,&varidat,&status);
 * 
 *     b) Variable length arrays:
 *        In this case, the string length will be set by the variable
 *        FitsStrBufLen.  The array must be made of strings dimensioned 
 *        to FitsStrBufLen+1.  Of course, if you dynamically allocate the 
 *        array as shown above, this is merely a matter of getting the
 *        auxillary pointers to point to the correct places.            
 *
 *  Caveats:
 *   1) If you have the DECFortran compiler on an ULTRIX machine, you have
 *      to pass -DDECFortran on the compile line to get logicals to pass
 *      correctly.
 *   2) Due to a macro conflict between CFORTRAN and some header files
 *      gcc does not currently work under OSF.     
 */

#ifndef _CFITSIO
#define _CFITSIO

/*
 * These are the #defines for the FIXED sized strings
 */

#define FITS_CLEN_KEYNAME    9
#define FITS_FLEN_KEYNAME    8
#define FITS_CLEN_KEYVAL    69
#define FITS_FLEN_KEYVAL    68
#define FITS_CLEN_STRVAL    69
#define FITS_FLEN_STRVAL    68
#define FITS_CLEN_COMMENT   73
#define FITS_FLEN_COMMENT   72
#define FITS_CLEN_CARD      81
#define FITS_FLEN_CARD      80
#define FITS_CLEN_HDEFKWDS  25
#define FITS_FLEN_HDEFKWDS  24
#define FITS_CLEN_ERRMSG    31
#define FITS_FLEN_ERRMSG    30
#define FITS_CLEN_CHECKSUM  17
#define FITS_FLEN_CHECKSUM  16
#define FITS_CLEN_CTYPE      5
#define FITS_FLEN_CTYPE      4
#define FITS_CLEN_1CHAR      2
#define FITS_FLEN_1CHAR      1
/*
  Copied from the Tcl Header file - Thanks to J. Oosterhout
*/

#undef _ANSI_ARGS_
#undef CONST
#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)       x
#   define CONST const
#   ifdef __cplusplus
#       define VARARGS (first) (first, ...)
#   else
#       define VARARGS(first) ()
#   endif
#else
#   define _ANSI_ARGS_(x)       ()
#   define CONST
#endif

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

/*
 End copy
*/

/*
 * STRING ARRAY DIMENSION MACROS FOR TEMPORARY STORAGE FOR (P)ZTRINGS 
 */

#define FITS_CHUNKSIZE  100 /* Generic # of rows to grab at a chunk */

/* FUNCTION PROTOTYPES */

EXTERN void fdgcvs _ANSI_ARGS_((int unit,int colnum,int frow,int felem,
				int nelem,char *nullval,char ***values,
				int *anyf,int *status));

#define UNSIGNED_BYTE /* This makes byte types unsigned char */

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

/* This is the Beginning of the CFORTRAN macros that define the 
 * CFITSIO calls...
 */

/******************** fcadef********************/

#define FCADEF(unit,rowlen,tfields,tbcol,tform,nrows,status) \
  CCALLSFSUB7(FTADEF,ftadef,INT,INT,INT,INTV,STRINGV,INT,PCINT,\
	      unit,rowlen,tfields,tbcol,tform,nrows,status)

/******************** fcarch********************/

#define FCARCH(mtype) \
  CCALLSFSUB1(FTARCH,ftarch,PCINT,\
	      mtype)

/******************** fcbdef********************/

#define FCBDEF(unit,tfields,tform,varidat,nrows,status) \
  CCALLSFSUB6(FTBDEF,ftbdef,INT,INT,STRINGV,INT,INT,PCINT,\
	      unit,tfields,tform,varidat,nrows,status)

/******************** fcbnfm********************/

#define FCBNFM(tform,dattyp,repeat,width,status) \
  CCALLSFSUB5(FTBNFM,ftbnfm,STRING,PCINT,PCINT,PCINT,PCINT,\
	      tform,dattyp,repeat,width,status)

/******************** fcclos********************/

#define FCCLOS(unit,status) \
  CCALLSFSUB2(FTCLOS,ftclos,INT,PCINT,\
	      unit,status)

/******************** fccmsg********************/

#define FCCMSG CCALLSFSUB0(FTCMSG,ftcmsg)

/******************** fccopy********************/

#define FCCOPY(unit,ounit,morekeys,status) \
  CCALLSFSUB4(FTCOPY,ftcopy,INT,INT,INT,PCINT,\
	      unit,ounit,morekeys,status)

/******************** fccpdt********************/

#define FCCPDT(unit,ounit,status) \
  CCALLSFSUB3(FTCPDT,ftcpdt,INT,INT,PCINT,\
	      unit,ounit,status)

/******************** fccrhd********************/

#define FCCRHD(unit,status) \
  CCALLSFSUB2(FTCRHD,ftcrhd,INT,PCINT,\
	      unit,status)

/******************** fcdhdu********************/

#define FCDHDU(unit,hdutype,status) \
  CCALLSFSUB3(FTDHDU,ftdhdu,INT,PCINT,PCINT,\
	      unit,hdutype,status)

/******************** fcddef********************/

#define FCDDEF(unit,bytlen,status) \
  CCALLSFSUB3(FTDDEF,ftddef,INT,INT,PCINT,\
	      unit,bytlen,status)

/******************** fcdelt********************/

#define FCDELT(unit,status) \
  CCALLSFSUB2(FTDELT,ftdelt,INT,PCINT,\
	      unit,status)

/******************** fcdrec********************/

#define FCDREC(unit,keyno,status) \
  CCALLSFSUB3(FTDREC,ftdrec,INT,INT,PCINT,\
	      unit,keyno,status)

/******************** fcdkey********************/

#define FCDKEY(unit,keyword,status) \
  CCALLSFSUB3(FTDKEY,ftdkey,INT,STRING,PCINT,\
	      unit,keyword,status)

/******************** fcdtyp********************/

#define ftdtyp_ELEMLEN_2 ZTRINGV_NUM(FITS_FLEN_1CHAR)
#define FCDTYP(value,dtype,status) \
  CCALLSFSUB3(FTDTYP,ftdtyp,STRING,PZTRING,PCINT,\
	      value,dtype,status)

/******************** fcflus********************/

#define FCFLUS(unit,status) \
  CCALLSFSUB2(FTFLUS,ftflus,INT,PCINT,\
	      unit,status)

/******************** fcghad********************/

#define FCGHAD(iunit,curadd,nxtadd) \
  CCALLSFSUB3(FTGHAD,ftghad,INT,PCINT,PCINT,\
	      iunit,curadd,nxtadd)

/******************** fcfiou********************/

#define FCFIOU(unit,status) \
  CCALLSFSUB2(FTFIOU,ftfiou,INT,PCINT,\
	      unit,status)

/******************** fcg2db********************/

#define ftg2db_ELEMLEN_7 ( ZTRINGV_ARGS(4) ) * ( ZTRINGV_ARGS(6) )
#define FCG2DB(unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)\
 CCALLSFSUB9(FTG2DB,ftg2db,INT,INT,BYTE,INT,INT,INT,BYTEV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)

/******************** fcg2dd********************/

#define FCG2DD(unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)\
  CCALLSFSUB9(FTG2DD,ftg2dd,INT,INT,DOUBLE,INT,INT,INT,DOUBLEV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)

/******************** fcg2de********************/

#define FCG2DE(unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)\
  CCALLSFSUB9(FTG2DE,ftg2de,INT,INT,FLOAT,INT,INT,INT,FLOATV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)

/******************** fcg2di********************/

#define FCG2DI(unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)\
  CCALLSFSUB9(FTG2DI,ftg2di,INT,INT,INT,INT,INT,INT,SHORTV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)

/******************** fcg2dj********************/

#define FCG2DJ(unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)\
  CCALLSFSUB9(FTG2DJ,ftg2dj,INT,INT,INT,INT,INT,INT,INTV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,naxis1,naxis2,image,anyf,status)

/******************** fcg3db********************/

#define ftg3db_ELEMLEN_9 ( ZTRINGV_ARGS(4) ) * ( ZTRINGV_ARGS(5) ) * ( ZTRINGV_ARGS(8) )
#define FCG3DB(unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)\
 CCALLSFSUB11(FTG3DB,ftg3db,INT,INT,BYTE,INT,INT,INT,INT,INT,BYTEV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)

/******************** fcg3dd********************/

#define FCG3DD(unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)\
  CCALLSFSUB11(FTG3DD,ftg3dd,INT,INT,DOUBLE,INT,INT,INT,INT,INT,DOUBLEV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)

/******************** fcg3de********************/

#define FCG3DE(unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)\
  CCALLSFSUB11(FTG3DE,ftg3de,INT,INT,FLOAT,INT,INT,INT,INT,INT,FLOATV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)

/******************** fcg3di********************/

#define FCG3DI(unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)\
  CCALLSFSUB11(FTG3DI,ftg3di,INT,INT,INT,INT,INT,INT,INT,INT,SHORTV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)

/******************** fcg3dj********************/

#define FCG3DJ(unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)\
  CCALLSFSUB11(FTG3DJ,ftg3dj,INT,INT,INT,INT,INT,INT,INT,INT,INTV,PCLOGICAL,PCINT,\
	      unit,group,nullval,dim1,dim2,naxis1,naxis2,naxis3,cube,anyf,status)

/******************** fcgabc********************/

#define FCGABC(tfields,tform,space,rowlen,tbcol,status) \
  CCALLSFSUB6(FTGABC,ftgabc,INT,STRINGV,INT,INTV,INTV,PCINT,\
	      tfields,tform,space,rowlen,tbcol,status)

/******************** fcgacl********************/

#define ftgacl_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftgacl_ELEMLEN_5 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftgacl_ELEMLEN_6 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftgacl_ELEMLEN_9 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftgacl_ELEMLEN_10 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define FCGACL(unit,colnum,ttype,tbcol,tunit,tform,tscal,tzero,snull,tdisp,status)\
  CCALLSFSUB11(FTGACL,ftgacl,INT,INT,PZTRING,PCINT,PZTRING,PZTRING,PCDOUBLE,PCDOUBLE,PZTRING,PZTRING,PCINT,\
	      unit,colnum,ttype,tbcol,tunit,tform,tscal,tzero,snull,tdisp,status)

/******************** fcgbcl********************/


#define ftgbcl_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftgbcl_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftgbcl_ELEMLEN_5 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftgbcl_ELEMLEN_10 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define FCGBCL(unit,colnum,ttype,tunit,datatype,repeat,tscal,tzero,tnull,tdisp,status)\
  CCALLSFSUB11(FTGBCL,ftgbcl,INT,INT,PZTRING,PZTRING,PZTRING,PCINT,PCDOUBLE,PCDOUBLE,PCINT,PZTRING,PCINT,\
	      unit,colnum,ttype,tunit,datatype,repeat,tscal,tzero,tnull,tdisp,status)

/******************** fcgcfb********************/

#define ftgcfb_ELEMLEN_6 ZTRINGV_ARGS(5)
#define FCGCFB(unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB9(FTGCFB,ftgcfb,INT,INT,INT,INT,INT,BYTEV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)

/******************** fcgcfd********************/

#define FCGCFD(unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB9(FTGCFD,ftgcfd,INT,INT,INT,INT,INT,DOUBLEV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)

/******************** fcgcfe********************/

#define FCGCFE(unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB9(FTGCFE,ftgcfe,INT,INT,INT,INT,INT,FLOATV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)

/******************** fcgcfi********************/

#define FCGCFI(unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB9(FTGCFI,ftgcfi,INT,INT,INT,INT,INT,SHORTV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)

/******************** fcgcfj********************/

#define FCGCFJ(unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB9(FTGCFJ,ftgcfj,INT,INT,INT,INT,INT,INTV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)

/******************** fcgcfl********************/

#define FCGCFL(unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB9(FTGCFL,ftgcfl,INT,INT,INT,INT,INT,LOGICALV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)

/******************** fcgcfs********************/

#define ftgcfs_ELEMS_6 ZTRINGV_ARGS(5)
#define ftgcfs_ELEMLEN_6 ZTRINGV_NUM(FitsStrBufLen)
#define FCGCFS(unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB9(FTGCFS,ftgcfs,INT,INT,INT,INT,INT,PZTRINGV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,values,flagvals,anyf,status)

/******************** fcgcl********************/

#define FCGCL(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTGCL,ftgcl,INT,INT,INT,INT,INT,LOGICALV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcgcno********************/

#define FCGCNO(unit,exact,colname,colnum,status) \
  CCALLSFSUB5(FTGCNO,ftgcno,INT,LOGICAL,STRING,PCINT,PCINT,\
	      unit,exact,colname,colnum,status)

/******************** fcgcrd********************/

#define ftgcrd_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_CARD)
#define FCGCRD(unit,keyword,card,status) \
  CCALLSFSUB4(FTGCRD,ftgcrd,INT,STRING,PZTRING,PCINT,\
	      unit,keyword,card,status)

/******************** fcgcvb********************/

#define ftgcvb_ELEMLEN_7 ZTRINGV_ARGS(5)
#define FCGCVB(unit,colnum,frow,felem,nelements,nullval,values,anyf,status)\
  CCALLSFSUB9(FTGCVB,ftgcvb,INT,INT,INT,INT,INT,BYTE,BYTEV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,nullval,values,anyf,status)

/******************** fcgcvd********************/

#define FCGCVD(unit,colnum,frow,felem,nelements,nullval,values,anyf,status)\
  CCALLSFSUB9(FTGCVD,ftgcvd,INT,INT,INT,INT,INT,DOUBLE,DOUBLEV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,nullval,values,anyf,status)

/******************** fcgcve********************/

#define FCGCVE(unit,colnum,frow,felem,nelements,nullval,values,anyf,status)\
  CCALLSFSUB9(FTGCVE,ftgcve,INT,INT,INT,INT,INT,FLOAT,FLOATV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,nullval,values,anyf,status)

/******************** fcgcvi********************/

#define FCGCVI(unit,colnum,frow,felem,nelements,nullval,values,anyf,status)\
  CCALLSFSUB9(FTGCVI,ftgcvi,INT,INT,INT,INT,INT,SHORT,SHORTV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,nullval,values,anyf,status)

/******************** fcgcvj********************/

#define FCGCVJ(unit,colnum,frow,felem,nelements,nullval,values,anyf,status)\
  CCALLSFSUB9(FTGCVJ,ftgcvj,INT,INT,INT,INT,INT,INT,INTV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,nullval,values,anyf,status)

/******************** fcgcvs********************/

#define ftgcvs_ELEMS_7   ZTRINGV_ARGS(5)
#define ftgcvs_ELEMLEN_7 ZTRINGV_NUM(FitsStrBufLen)
#define FCGCVS(unit,colnum,frow,felem,nelements,nullval,values,anyf,status)\
  CCALLSFSUB9(FTGCVS,ftgcvs,INT,INT,INT,INT,INT,STRING,PZTRINGV,PCLOGICAL,PCINT,\
	      unit,colnum,frow,felem,nelements,nullval,values,anyf,status)

/******************** fcgcx********************/

#define FCGCX(unit,colnum,frow,fbit,nbit,lray,status)\
  CCALLSFSUB7(FTGCX,ftgcx,INT,INT,INT,INT,INT,LOGICALV,PCINT,\
	      unit,colnum,frow,fbit,nbit,lray,status)

/******************** fcgcxd********************/

#define FCGCXD(unit,colnum,frow,nrow,fbit,nbit,array,status)\
  CCALLSFSUB8(FTGCXD,ftgcxd,INT,INT,INT,INT,INT,INT,DOUBLEV,PCINT,\
	      unit,colnum,frow,nrow,fbit,nbit,array,status)

/******************** fcgcxi********************/

#define FCGCXI(unit,colnum,frow,nrow,fbit,nbit,array,status)\
  CCALLSFSUB8(FTGCXI,ftgcxi,INT,INT,INT,INT,INT,INT,SHORTV,PCINT,\
	      unit,colnum,frow,nrow,fbit,nbit,array,status)

/******************** fcgcxj********************/

#define FCGCXJ(unit,colnum,frow,nrow,fbit,nbit,array,status)\
  CCALLSFSUB8(FTGCXJ,ftgcxj,INT,INT,INT,INT,INT,INT,INTV,PCINT,\
	      unit,colnum,frow,nrow,fbit,nbit,array,status)

/******************** fcgdes********************/

#define FCGDES(unit,colnum,rownum,nelements,offset,status)\
  CCALLSFSUB6(FTGDES,ftgdes,INT,INT,INT,PCINT,PCINT,PCINT,\
	      unit,colnum,rownum,nelements,offset,status)

/******************** fcgerr********************/

#define ftgerr_ELEMLEN_2 ZTRINGV_NUM(FITS_FLEN_ERRMSG)
#define FCGERR(status,errtext) CCALLSFSUB2(FTGERR,ftgerr,INT,PZTRING,\
	      status,errtext)

/******************** fcggpb********************/

#define ftggpb_ELEMLEN_5 ZTRINGV_ARGS(4)
#define FCGGPB(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTGGPB,ftggpb,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcggpd********************/

#define FCGGPD(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTGGPD,ftggpd,INT,INT,INT,INT,DOUBLEV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcggpe********************/

#define FCGGPE(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTGGPE,ftggpe,INT,INT,INT,INT,FLOATV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcggpi********************/

#define FCGGPI(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTGGPI,ftggpi,INT,INT,INT,INT,SHORTV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcggpj********************/

#define FCGGPJ(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTGGPJ,ftggpj,INT,INT,INT,INT,INTV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcghbn********************/

#define ftghbn_ELEMS_5 ZTRINGV_ARGS(2)
#define ftghbn_ELEMLEN_5 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftghbn_ELEMS_6 ZTRINGV_ARGS(2)
#define ftghbn_ELEMLEN_6 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftghbn_ELEMS_7 ZTRINGV_ARGS(2)
#define ftghbn_ELEMLEN_7 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftghbn_ELEMLEN_8 ZTRINGV_NUM(FITS_FLEN_KEYVAL)
#define FCGHBN(unit,maxdim,nrows,tfields,ttype,tform,tunit,extname,varidat,status) CCALLSFSUB10(FTGHBN,ftghbn,INT,INT,PCINT,PCINT,PZTRINGV,PZTRINGV,PZTRINGV,PZTRING,PCINT,PCINT,unit,maxdim,nrows,tfields,ttype,tform,tunit,extname,varidat,status)

/******************** fcghdn********************/

#define FCGHDN(unit,nhdu) \
  CCALLSFSUB2(FTGHDN,ftghdn,INT,PCINT,\
	      unit,nhdu)

/******************** fcghpr********************/

#define FCGHPR(unit,maxdim,simple,bitpix,naxis,naxes,pcount,gcount,extend,status)\
  CCALLSFSUB10(FTGHPR,ftghpr,INT,INT,PCLOGICAL,PCINT,PCINT,INTV,PCINT,PCINT,PCLOGICAL,PCINT,\
	      unit,maxdim,simple,bitpix,naxis,naxes,pcount,gcount,extend,status)

/******************** fcghsp********************/

#define FCGHSP(unit,keysexist,keysadd,status) \
  CCALLSFSUB4(FTGHSP,ftghsp,INT,PCINT,PCINT,PCINT,\
	      unit,keysexist,keysadd,status)

/******************** fcghps********************/

#define FCGHPS(unit,keysexist,keyno,status) \
  CCALLSFSUB4(FTGHPS,ftghps,INT,PCINT,PCINT,PCINT,\
	      unit,keysexist,keyno,status)

/******************** fcghtb********************/


#define ftghtb_ELEMS_6 ZTRINGV_ARGS(2)
#define ftghtb_ELEMLEN_6 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftghtb_ELEMS_8 ZTRINGV_ARGS(2)
#define ftghtb_ELEMLEN_8 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftghtb_ELEMS_9 ZTRINGV_ARGS(2)
#define ftghtb_ELEMLEN_9 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS)
#define ftghtb_ELEMLEN_10 ZTRINGV_NUM(FITS_FLEN_KEYVAL)
#define FCGHTB(unit,maxdim,rowlen,nrows,tfields,ttype,tbcol,tform,tunit,extname,status) \
      CCALLSFSUB11(FTGHTB,ftghtb,INT,INT,PCINT,PCINT,PCINT,PZTRINGV,INTV,\
         PZTRINGV,PZTRINGV,PZTRING,PCINT,\
      unit,maxdim,rowlen,nrows,tfields,ttype,tbcol,tform,tunit,extname,status)

/******************** fcgiou********************/

#define FCGIOU(unit,status) \
  CCALLSFSUB2(FTGIOU,ftgiou,PCINT,PCINT,\
	      unit,status)

/******************** fcgkey********************/

#define ftgkey_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_KEYVAL)
#define ftgkey_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKEY(unit,keyword,value,comment,status) \
  CCALLSFSUB5(FTGKEY,ftgkey,INT,STRING,PZTRING,PZTRING,PCINT,\
	      unit,keyword,value,comment,status)

/******************** fcgknd********************/

#define FCGKND(unit,keyroot,startno,max_keys,keyvals,nfound,status) \
  CCALLSFSUB7(FTGKND,ftgknd,INT,STRING,INT,INT,DOUBLEV,PCINT,PCINT,\
	      unit,keyroot,startno,max_keys,keyvals,nfound,status)

/******************** fcgkne********************/

#define FCGKNE(unit,keyroot,startno,max_keys,keyvals,nfound,status) \
  CCALLSFSUB7(FTGKNE,ftgkne,INT,STRING,INT,INT,FLOATV,PCINT,PCINT,\
	      unit,keyroot,startno,max_keys,keyvals,nfound,status)

/******************** fcgknj********************/

#define FCGKNJ(unit,keyroot,startno,max_keys,keyvals,nfound,status) \
  CCALLSFSUB7(FTGKNJ,ftgknj,INT,STRING,INT,INT,INTV,PCINT,PCINT,\
	      unit,keyroot,startno,max_keys,keyvals,nfound,status)

/******************** fcgknl********************/

#define FCGKNL(unit,keyroot,startno,max_keys,keyvals,nfound,status) \
  CCALLSFSUB7(FTGKNL,ftgknl,INT,STRING,INT,INT,LOGICALV,PCINT,PCINT,\
	      unit,keyroot,startno,max_keys,keyvals,nfound,status)

/******************** fcgkns********************/

#define ftgkns_ELEMS_5 ZTRINGV_ARGS(4)
#define ftgkns_ELEMLEN_5 ZTRINGV_NUM(FITS_FLEN_STRVAL)
#define FCGKNS(unit,keyroot,startno,max_keys,keyvals,nfound,status)\
  CCALLSFSUB7(FTGKNS,ftgkns,INT,STRING,INT,INT,PZTRINGV,PCINT,PCINT,\
	      unit,keyroot,startno,max_keys,keyvals,nfound,status)

/******************** fcgkyd********************/

#define ftgkyd_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKYD(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTGKYD,ftgkyd,INT,STRING,PCDOUBLE,PZTRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcgkye********************/

#define ftgkye_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKYE(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTGKYE,ftgkye,INT,STRING,PCFLOAT,PZTRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcgkyj********************/

#define ftgkyj_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKYJ(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTGKYJ,ftgkyj,INT,STRING,PCINT,PZTRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcgkyl********************/

#define ftgkyl_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKYL(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTGKYL,ftgkyl,INT,STRING,PCINT,PZTRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcgkyn********************/

#define ftgkyn_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_KEYNAME)
#define ftgkyn_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_KEYVAL)
#define ftgkyn_ELEMLEN_5 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKYN(unit,key_no,keyword,value,comment,status) \
  CCALLSFSUB6(FTGKYN,ftgkyn,INT,INT,PZTRING,PZTRING,PZTRING,PCINT,\
	      unit,key_no,keyword,value,comment,status)

/******************** fcgkys********************/

#define ftgkys_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_STRVAL)
#define ftgkys_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKYS(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTGKYS,ftgkys,INT,STRING,PZTRING,PZTRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcgkyt********************/

#define ftgkyt_ELEMLEN_5 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCGKYT(unit,keyword,intval,dblval,comment,status) \
  CCALLSFSUB6(FTGKYT,ftgkyt,INT,STRING,PCINT,PCDOUBLE,PZTRING,PCINT,\
	      unit,keyword,intval,dblval,comment,status)

/******************** fcgmsg********************/

#define ftgmsg_ELEMLEN_1 ZTRINGV_NUM(FITS_FLEN_CARD)
#define FCGMSG(errmsg) CCALLSFSUB1(FTGMSG,ftgmsg,PZTRING,errmsg)

/******************** fcgpfb********************/

#define ftgpfb_ELEMLEN_5 ZTRINGV_ARGS(4)
#define FCGPFB(unit,group,fpixel,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB8(FTGPFB,ftgpfb,INT,INT,INT,INT,BYTEV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,values,flagvals,anyf,status)

/******************** fcgpfd********************/

#define FCGPFD(unit,group,fpixel,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB8(FTGPFD,ftgpfd,INT,INT,INT,INT,DOUBLEV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,values,flagvals,anyf,status)

/******************** fcgpfe********************/

#define FCGPFE(unit,group,fpixel,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB8(FTGPFE,ftgpfe,INT,INT,INT,INT,FLOATV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,values,flagvals,anyf,status)

/******************** fcgpfi********************/

#define FCGPFI(unit,group,fpixel,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB8(FTGPFI,ftgpfi,INT,INT,INT,INT,SHORTV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,values,flagvals,anyf,status)

/******************** fcgpfj********************/

#define FCGPFJ(unit,group,fpixel,nelements,values,flagvals,anyf,status)\
  CCALLSFSUB8(FTGPFJ,ftgpfj,INT,INT,INT,INT,INTV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,values,flagvals,anyf,status)

/******************** fcgpvb********************/

#define ftgpvb_ELEMLEN_6 ZTRINGV_ARGS(4)
#define FCGPVB(unit,group,fpixel,nelements,nullval,values,anyf,status)\
  CCALLSFSUB8(FTGPVB,ftgpvb,INT,INT,INT,INT,BYTE,BYTEV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,nullval,values,anyf,status)

/******************** fcgpvd********************/

#define FCGPVD(unit,group,fpixel,nelements,nullval,values,anyf,status)\
  CCALLSFSUB8(FTGPVD,ftgpvd,INT,INT,INT,INT,DOUBLE,DOUBLEV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,nullval,values,anyf,status)

/******************** fcgpve********************/

#define FCGPVE(unit,group,fpixel,nelements,nullval,values,anyf,status)\
  CCALLSFSUB8(FTGPVE,ftgpve,INT,INT,INT,INT,FLOAT,FLOATV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,nullval,values,anyf,status)

/******************** fcgpvi********************/

#define FCGPVI(unit,group,fpixel,nelements,nullval,values,anyf,status)\
  CCALLSFSUB8(FTGPVI,ftgpvi,INT,INT,INT,INT,SHORT,SHORTV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,nullval,values,anyf,status)

/******************** fcgpvj********************/

#define FCGPVJ(unit,group,fpixel,nelements,nullval,values,anyf,status)\
  CCALLSFSUB8(FTGPVJ,ftgpvj,INT,INT,INT,INT,INT,INTV,PCLOGICAL,PCINT,\
	      unit,group,fpixel,nelements,nullval,values,anyf,status)

/******************** fcgrec********************/

#define ftgrec_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_CARD)
#define FCGREC(unit,key_no,card,status) \
  CCALLSFSUB4(FTGREC,ftgrec,INT,INT,PZTRING,PCINT,\
	      unit,key_no,card,status)

/******************** fcgsdt********************/

#define FCGSDT(dd,mm,yy,status) \
  CCALLSFSUB4(FTGSDT,ftgsdt,PCINT,PCINT,PCINT,PCINT,\
	      dd,mm,yy,status)

/******************** fcgsfb********************/

#define FCGSFB(unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)\
  CCALLSFSUB11(FTGSFB,ftgsfb,INT,INT,INT,INTV,INTV,INTV,INTV,BYTEV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)

/******************** fcgsfd********************/

#define FCGSFD(unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)\
  CCALLSFSUB11(FTGSFD,ftgsfd,INT,INT,INT,INTV,INTV,INTV,INTV,DOUBLEV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)

/******************** fcgsfe********************/

#define FCGSFE(unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)\
  CCALLSFSUB11(FTGSFE,ftgsfe,INT,INT,INT,INTV,INTV,INTV,INTV,FLOATV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)

/******************** fcgsfi********************/

#define FCGSFI(unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)\
 CCALLSFSUB11(FTGSFI,ftgsfi,INT,INT,INT,INTV,INTV,INTV,INTV,SHORTV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)

/******************** fcgsfj********************/

#define FCGSFJ(unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)\
 CCALLSFSUB11(FTGSFJ,ftgsfj,INT,INT,INT,INTV,INTV,INTV,INTV,INTV,LOGICALV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,array,flagvals,anyf,status)

/******************** fcgsvb********************/

#define FCGSVB(unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)\
  CCALLSFSUB11(FTGSVB,ftgsvb,INT,INT,INT,INTV,INTV,INTV,INTV,BYTE,BYTEV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)

/******************** fcgsvd********************/

#define FCGSVD(unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)\
  CCALLSFSUB11(FTGSVD,ftgsvd,INT,INT,INT,INTV,INTV,INTV,INTV,DOUBLE,DOUBLEV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)

/******************** fcgsve********************/

#define FCGSVE(unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)\
  CCALLSFSUB11(FTGSVE,ftgsve,INT,INT,INT,INTV,INTV,INTV,INTV,FLOAT,FLOATV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)

/******************** fcgsvi********************/

#define FCGSVI(unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)\
 CCALLSFSUB11(FTGSVI,ftgsvi,INT,INT,INT,INTV,INTV,INTV,INTV,SHORT,SHORTV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)

/******************** fcgsvj********************/

#define FCGSVJ(unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)\
 CCALLSFSUB11(FTGSVJ,ftgsvj,INT,INT,INT,INTV,INTV,INTV,INTV,INT,INTV,PCLOGICAL,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,incs,nullval,array,anyf,status)

/******************** fcgtbb********************/

#define FCGTBB(unit,frow,startbyte,nbytes,array,status)\
  CCALLSFSUB6(FTGTBB,ftgtbb,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,frow,startbyte,nbytes,array,status)

/******************** fcgtbs********************/

#define ftgtbs_ELEMLEN_5 ZTRINGV_ARGS(4)
#define FCGTBS(unit,frow,startchar,nchars,string,status)\
  CCALLSFSUB6(FTGTBS,ftgtbs,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,frow,startchar,nchars,string,status)

/******************** fcgtdm********************/

#define FCGTDM(unit,colnum,maxdim,naxis,naxes,status)\
  CCALLSFSUB6(FTGTDM,ftgtdm,INT,INT,INT,PCINT,INTV,PCINT,\
	      unit,colnum,maxdim,naxis,naxes,status)

/******************** fcgthd********************/

#define ftgthd_ELEMLEN_2 ZTRINGV_NUM(FITS_FLEN_CARD)
#define FCGTHD(template,card,hdtype,status) \
  CCALLSFSUB4(FTGTHD,ftgthd,STRING,PZTRING,PCINT,PCINT,\
	      template,card,hdtype,status)


/******************** fchdef********************/

#define FCHDEF(unit,morekeys,status) \
  CCALLSFSUB3(FTHDEF,fthdef,INT,INT,PCINT,\
	      unit,morekeys,status)

/******************** fcikyd********************/

#define FCIKYD(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTIKYD,ftikyd,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcikye********************/

#define FCIKYE(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTIKYE,ftikye,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcikyf********************/

#define FCIKYF(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTIKYF,ftikyf,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcikyg********************/

#define FCIKYG(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTIKYG,ftikyg,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcikyj********************/

#define FCIKYJ(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTIKYJ,ftikyj,INT,STRING,INT,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcikyl********************/

#define FCIKYL(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTIKYL,ftikyl,INT,STRING,LOGICAL,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcikys********************/

#define FCIKYS(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTIKYS,ftikys,INT,STRING,STRING,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcirec********************/

#define FCIREC(unit,keyno,card,status) \
  CCALLSFSUB4(FTIREC,ftirec,INT,INT,STRING,PCINT,\
	      unit,keyno,card,status)

/******************** fcinit********************/

#define FCINIT(unit,filename,blocksize,status) \
  CCALLSFSUB4(FTINIT,ftinit,INT,STRING,INT,PCINT,\
	      unit,filename,blocksize,status)

/******************** fcirow********************/

#define FCIROW(unit,frow,nrows,status) \
  CCALLSFSUB4(FTIROW,ftirow,INT,INT,INT,PCINT,\
	      unit,frow,nrows,status)

/******************** fcdrow********************/

#define FCDROW(unit,frow,nrows,status) \
  CCALLSFSUB4(FTDROW,ftdrow,INT,INT,INT,PCINT,\
	      unit,frow,nrows,status)

/******************** fcicol********************/

#define FCICOL(unit,colnum,ttype,tform,status) \
  CCALLSFSUB5(FTICOL,fticol,INT,INT,STRING,STRING,PCINT,\
	      unit,colnum,ttype,tform,status)

/******************** fcdcol********************/

#define FCDCOL(unit,colnum,status) \
  CCALLSFSUB3(FTDCOL,ftdcol,INT,INT,PCINT,\
	      unit,colnum,status)

/******************** fckeyn********************/

#define ftkeyn_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_KEYNAME)
#define FCKEYN(keyroot,seq_no,keyword,status) \
  CCALLSFSUB4(FTKEYN,ftkeyn,STRING,INT,PZTRING,PCINT,\
	      keyroot,seq_no,keyword,status)

/******************** fcmahd********************/

#define FCMAHD(unit,nhdu,hdutype,status) \
  CCALLSFSUB4(FTMAHD,ftmahd,INT,INT,PCINT,PCINT,\
	      unit,nhdu,hdutype,status)

/******************** fcmcom********************/

#define FCMCOM(unit,keyword,comment,status) \
  CCALLSFSUB4(FTMCOM,ftmcom,INT,STRING,STRING,PCINT,\
	      unit,keyword,comment,status)

/******************** fcmcrd********************/

#define FCMCRD(unit,keyword,card,status) \
  CCALLSFSUB4(FTMCRD,ftmcrd,INT,STRING,STRING,PCINT,\
	      unit,keyword,card,status)

/******************** fcmkyd********************/

#define FCMKYD(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTMKYD,ftmkyd,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcmkye********************/

#define FCMKYE(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTMKYE,ftmkye,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcmkyf********************/

#define FCMKYF(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTMKYF,ftmkyf,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcmkyg********************/

#define FCMKYG(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTMKYG,ftmkyg,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcmkyj********************/

#define FCMKYJ(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTMKYJ,ftmkyj,INT,STRING,INT,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcmkyl********************/

#define FCMKYL(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTMKYL,ftmkyl,INT,STRING,LOGICAL,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcmkys********************/

#define FCMKYS(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTMKYS,ftmkys,INT,STRING,STRING,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcmnam********************/

#define FCMNAM(unit,keyword,newkey,status) \
  CCALLSFSUB4(FTMNAM,ftmnam,INT,STRING,STRING,PCINT,\
	      unit,keyword,newkey,status)

/******************** fcmrec********************/

#define FCMREC(unit,key_no,card,status) \
  CCALLSFSUB4(FTMREC,ftmrec,INT,INT,STRING,PCINT,\
	      unit,key_no,card,status)

/******************** fcmrhd********************/

#define FCMRHD(unit,nmove,hdutype,status) \
  CCALLSFSUB4(FTMRHD,ftmrhd,INT,INT,PCINT,PCINT,\
	      unit,nmove,hdutype,status)

/******************** fcnkey********************/

#define ftnkey_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_KEYNAME)
#define FCNKEY(seq_no,keyroot,keyword,status) \
  CCALLSFSUB4(FTNKEY,ftnkey,INT,STRING,PZTRING,PCINT,\
	     seq_no,keyroot,keyword,status)

/******************** fcopen********************/

#define FCOPEN(unit,filename,rwmode,blocksize,status) \
  CCALLSFSUB5(FTOPEN,ftopen,INT,STRING,INT,PCINT,PCINT,\
	      unit,filename,rwmode,blocksize,status)

/******************** fcp2db********************/

#define ftp2db_ELEMLEN_6 ( ZTRINGV_ARGS(3) ) * ( ZTRINGV_ARGS(5) )
#define FCP2DB(unit,group,dim1,naxis1,naxis2,image,status)\
  CCALLSFSUB7(FTP2DB,ftp2db,INT,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,group,dim1,naxis1,naxis2,image,status)

/******************** fcp2dd********************/

#define FCP2DD(unit,group,dim1,naxis1,naxis2,image,status)\
  CCALLSFSUB7(FTP2DD,ftp2dd,INT,INT,INT,INT,INT,DOUBLEV,PCINT,\
	      unit,group,dim1,naxis1,naxis2,image,status)

/******************** fcp2de********************/

#define FCP2DE(unit,group,dim1,naxis1,naxis2,image,status)\
  CCALLSFSUB7(FTP2DE,ftp2de,INT,INT,INT,INT,INT,FLOATV,PCINT,\
	      unit,group,dim1,naxis1,naxis2,image,status)

/******************** fcp2di********************/

#define FCP2DI(unit,group,dim1,naxis1,naxis2,image,status)\
  CCALLSFSUB7(FTP2DI,ftp2di,INT,INT,INT,INT,INT,SHORTV,PCINT,\
	      unit,group,dim1,naxis1,naxis2,image,status)

/******************** fcp2dj********************/

#define FCP2DJ(unit,group,dim1,naxis1,naxis2,image,status)\
  CCALLSFSUB7(FTP2DJ,ftp2dj,INT,INT,INT,INT,INT,INTV,PCINT,\
	      unit,group,dim1,naxis1,naxis2,image,status)

/******************** fcp3db********************/

#define ftp3db_ELEMLEN_8 ( ZTRINGV_ARGS(3) ) * ( ZTRINGV_ARGS(4) ) * ( ZTRINGV_ARGS(7) )
#define FCP3DB(unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)\
 CCALLSFSUB9(FTP3DB,ftp3db,INT,INT,INT,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)

/******************** fcp3dd********************/

#define FCP3DD(unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)\
  CCALLSFSUB9(FTP3DD,ftp3dd,INT,INT,INT,INT,INT,INT,INT,DOUBLEV,PCINT,\
	      unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)

/******************** fcp3de********************/

#define FCP3DE(unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)\
  CCALLSFSUB9(FTP3DE,ftp3de,INT,INT,INT,INT,INT,INT,INT,FLOATV,PCINT,\
	      unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)

/******************** fcp3di********************/

#define FCP3DI(unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)\
  CCALLSFSUB9(FTP3DI,ftp3di,INT,INT,INT,INT,INT,INT,INT,SHORTV,PCINT,\
	      unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)

/******************** fcp3dj********************/

#define FCP3DJ(unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)\
  CCALLSFSUB9(FTP3DJ,ftp3dj,INT,INT,INT,INT,INT,INT,INT,INTV,PCINT,\
	      unit,group,dim1,dim2,naxis1,naxis2,naxis3,cube,status)

/******************** fcpclb********************/

#define ftpclb_ELEMLEN_6 ZTRINGV_ARGS(5)
#define FCPCLB(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTPCLB,ftpclb,INT,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcpcld********************/

#define FCPCLD(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTPCLD,ftpcld,INT,INT,INT,INT,INT,DOUBLEV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcpcle********************/

#define FCPCLE(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTPCLE,ftpcle,INT,INT,INT,INT,INT,FLOATV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcpcli********************/

#define FCPCLI(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTPCLI,ftpcli,INT,INT,INT,INT,INT,SHORTV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcpclj********************/

#define FCPCLJ(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTPCLJ,ftpclj,INT,INT,INT,INT,INT,INTV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcpcll********************/

#define FCPCLL(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTPCLL,ftpcll,INT,INT,INT,INT,INT,LOGICALV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcpcls********************/

#define ftpcls_ELEMS_6 ZTRINGV_ARGS(5)
#define ftpcls_ELEMLEN_6 ZTRINGV_NUM(FitsStrBufLen)
#define FCPCLS(unit,colnum,frow,felem,nelements,values,status)\
  CCALLSFSUB7(FTPCLS,ftpcls,INT,INT,INT,INT,INT,ZTRINGV,PCINT,\
	      unit,colnum,frow,felem,nelements,values,status)

/******************** fcpclu********************/

#define FCPCLU(unit,colnum,frow,felem,nelements,status)\
  CCALLSFSUB6(FTPCLU,ftpclu,INT,INT,INT,INT,INT,PCINT,\
	      unit,colnum,frow,felem,nelements,status)

/******************** fcpclx********************/

#define FCPCLX(unit,colnum,frow,fbit,nbit,lray,status)\
  CCALLSFSUB7(FTPCLX,ftpclx,INT,INT,INT,INT,INT,LOGICALV,PCINT,\
	      unit,colnum,frow,fbit,nbit,lray,status)

/******************** fcpcnb********************/

#define ftpcnb_ELEMLEN_6 ZTRINGV_ARGS(5)
#define FCPCNB(unit,colnum,frow,felem,nelements,values,nulval,status)\
  CCALLSFSUB8(FTPCNB,ftpcnb,INT,INT,INT,INT,INT,BYTEV,BYTE,PCINT,\
	      unit,colnum,frow,felem,nelements,values,nulval,status)

/******************** fcpcnd********************/

#define FCPCND(unit,colnum,frow,felem,nelements,values,nulval,status)\
  CCALLSFSUB8(FTPCND,ftpcnd,INT,INT,INT,INT,INT,DOUBLEV,DOUBLE,PCINT,\
	      unit,colnum,frow,felem,nelements,values,nulval,status)

/******************** fcpcne********************/

#define FCPCNE(unit,colnum,frow,felem,nelements,values,nulval,status)\
  CCALLSFSUB8(FTPCNE,ftpcne,INT,INT,INT,INT,INT,FLOATV,FLOAT,PCINT,\
	      unit,colnum,frow,felem,nelements,values,nulval,status)

/******************** fcpcni********************/

#define FCPCNI(unit,colnum,frow,felem,nelements,values,nulval,status)\
  CCALLSFSUB8(FTPCNI,ftpcni,INT,INT,INT,INT,INT,SHORTV,SHORT,PCINT,\
	      unit,colnum,frow,felem,nelements,values,nulval,status)

/******************** fcpcnj********************/

#define FCPCNJ(unit,colnum,frow,felem,nelements,values,nulval,status)\
  CCALLSFSUB8(FTPCNJ,ftpcnj,INT,INT,INT,INT,INT,INTV,INT,PCINT,\
	      unit,colnum,frow,felem,nelements,values,nulval,status)

/******************** fcpcom********************/

#define FCPCOM(unit,comment,status) \
  CCALLSFSUB3(FTPCOM,ftpcom,INT,STRING,PCINT,\
	      unit,comment,status)

/******************** fcpdat********************/

#define FCPDAT(unit,status) \
  CCALLSFSUB2(FTPDAT,ftpdat,INT,PCINT,\
	      unit,status)

/******************** fcpdef********************/

#define FCPDEF(unit,bitpix,naxis,naxes,pcount,gcount,status) \
  CCALLSFSUB7(FTPDEF,ftpdef,INT,INT,INT,INTV,INT,INT,PCINT,\
	      unit,bitpix,naxis,naxes,pcount,gcount,status)

/******************** fciimg********************/

#define FCIIMG(unit,bitpix,naxis,naxes,status) \
  CCALLSFSUB5(FTIIMG,ftiimg,INT,INT,INT,INTV,PCINT,\
	      unit,bitpix,naxis,naxes,status)

/******************** fcpdes********************/

#define FCPDES(unit,colnum,rownum,nelements,offset,status)\
  CCALLSFSUB6(FTPDES,ftpdes,INT,INT,INT,INT,INT,PCINT,\
	      unit,colnum,rownum,nelements,offset,status)

/******************** fcpgpb********************/

#define ftpgpb_ELEMLEN_5 ZTRINGV_ARGS(4)
#define FCPGPB(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTPGPB,ftpgpb,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcpgpd********************/

#define FCPGPD(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTPGPD,ftpgpd,INT,INT,INT,INT,DOUBLEV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcpgpe********************/

#define FCPGPE(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTPGPE,ftpgpe,INT,INT,INT,INT,FLOATV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcpgpi********************/

#define FCPGPI(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTPGPI,ftpgpi,INT,INT,INT,INT,SHORTV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcpgpj********************/

#define FCPGPJ(unit,group,fparm,nparm,values,status)\
  CCALLSFSUB6(FTPGPJ,ftpgpj,INT,INT,INT,INT,INTV,PCINT,\
	      unit,group,fparm,nparm,values,status)

/******************** fcphbn********************/

#define FCPHBN(unit,nrows,tfields,ttype,tform,tunit,extname,varidat,status)\
CCALLSFSUB9(FTPHBN,ftphbn,INT,INT,INT,STRINGV,STRINGV,STRINGV,STRING,INT,PCINT,\
     unit,nrows,tfields,ttype,tform,tunit,extname,varidat,status)

/******************** fcibin********************/

#define FCIBIN(unit,nrows,tfields,ttype,tform,tunit,extname,varidat,status)\
CCALLSFSUB9(FTIBIN,ftibin,INT,INT,INT,STRINGV,STRINGV,STRINGV,STRING,INT,PCINT,\
     unit,nrows,tfields,ttype,tform,tunit,extname,varidat,status)

/******************** fcphis********************/

#define FCPHIS(unit,history,status) \
  CCALLSFSUB3(FTPHIS,ftphis,INT,STRING,PCINT,\
	      unit,history,status)

/******************** fcphpr********************/

#define FCPHPR(unit,simple,bitpix,naxis,naxes,pcount,gcount,extend,status)\
  CCALLSFSUB9(FTPHPR,ftphpr,INT,LOGICAL,INT,INT,INTV,INT,INT,LOGICAL,PCINT,\
	      unit,simple,bitpix,naxis,naxes,pcount,gcount,extend,status)

/******************** fcphtb********************/

#define FCPHTB(unit,rowlen,nrows,tfields,ttype,tbcol,tform,tunit,extname,status)\
 CCALLSFSUB10(FTPHTB,ftphtb,INT,INT,INT,INT,STRINGV,INTV,STRINGV,STRINGV,STRING,PCINT,\
         unit,rowlen,nrows,tfields,ttype,tbcol,tform,tunit,extname,status)

/******************** fcitab********************/

#define FCITAB(unit,rowlen,nrows,tfields,ttype,tbcol,tform,tunit,extname,status)\
 CCALLSFSUB10(FTITAB,ftitab,INT,INT,INT,INT,STRINGV,INTV,STRINGV,STRINGV,STRING,PCINT,\
         unit,rowlen,nrows,tfields,ttype,tbcol,tform,tunit,extname,status)

/******************** fcpkls********************/

#define FCPKLS(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTPKLS,ftpkls,INT,STRING,STRING,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcpknd********************/

#define ftpknd_ELEMS_7 ZTRINGV_ARGS(4)
#define ftpknd_ELEMLEN_7 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPKND(unit,keyroot,startno,no_keys,keyvals,decimals,comments,status) \
  CCALLSFSUB8(FTPKND,ftpknd,INT,STRING,INT,INT,DOUBLEV,INT,ZTRINGV,PCINT,\
	      unit,keyroot,startno,no_keys,keyvals,decimals,comments,status)

/******************** fcpkne********************/

#define ftpkne_ELEMS_7 ZTRINGV_ARGS(4)
#define ftpkne_ELEMLEN_7 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPKNE(unit,keyroot,startno,no_keys,keyvals,decimals,comments,status) \
  CCALLSFSUB8(FTPKNE,ftpkne,INT,STRING,INT,INT,FLOATV,INT,ZTRINGV,PCINT,\
	      unit,keyroot,startno,no_keys,keyvals,decimals,comments,status)

/******************** fcpknf********************/

#define ftpknf_ELEMS_7 ZTRINGV_ARGS(4)
#define ftpknf_ELEMLEN_7 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPKNF(unit,keyroot,startno,no_keys,keyvals,decimals,comments,status) \
  CCALLSFSUB8(FTPKNF,ftpknf,INT,STRING,INT,INT,FLOATV,INT,ZTRINGV,PCINT,\
	      unit,keyroot,startno,no_keys,keyvals,decimals,comments,status)

/******************** fcpkng********************/

#define ftpkng_ELEMS_7 ZTRINGV_ARGS(4)
#define ftpkng_ELEMLEN_7 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPKNG(unit,keyroot,startno,no_keys,keyvals,decimals,comments,status) \
  CCALLSFSUB8(FTPKNG,ftpkng,INT,STRING,INT,INT,DOUBLEV,INT,ZTRINGV,PCINT,\
	      unit,keyroot,startno,no_keys,keyvals,decimals,comments,status)

/******************** fcpknj********************/

#define ftpknj_ELEMS_6 ZTRINGV_ARGS(4)
#define ftpknj_ELEMLEN_6 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPKNJ(unit,keyroot,startno,no_keys,keyvals,comments,status) \
  CCALLSFSUB7(FTPKNJ,ftpknj,INT,STRING,INT,INT,INTV,ZTRINGV,PCINT,\
	      unit,keyroot,startno,no_keys,keyvals,comments,status)

/******************** fcpknl********************/

#define ftpknl_ELEMS_6 ZTRINGV_ARGS(4)
#define ftpknl_ELEMLEN_6 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPKNL(unit,keyroot,startno,no_keys,keyvals,comments,status) \
  CCALLSFSUB7(FTPKNL,ftpknl,INT,STRING,INT,INT,LOGICALV,ZTRINGV,PCINT,\
	      unit,keyroot,startno,no_keys,keyvals,comments,status)

/******************** fcpkns********************/

#define ftpkns_ELEMS_5   ZTRINGV_ARGS(4)
#define ftpkns_ELEMLEN_5 ZTRINGV_NUM(FITS_FLEN_KEYVAL)
#define ftpkns_ELEMS_6   ZTRINGV_ARGS(4)
#define ftpkns_ELEMLEN_6 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPKNS(unit,keyroot,startno,no_keys,keyvals,comments,status) \
  CCALLSFSUB7(FTPKNS,ftpkns,INT,STRING,INT,INT,ZTRINGV,ZTRINGV,PCINT,\
	      unit,keyroot,startno,no_keys,keyvals,comments,status)

/******************** fcpkyd********************/

#define FCPKYD(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTPKYD,ftpkyd,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcpkye********************/

#define FCPKYE(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTPKYE,ftpkye,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcpkyf********************/

#define FCPKYF(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTPKYF,ftpkyf,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcpkyg********************/

#define FCPKYG(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTPKYG,ftpkyg,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcpkyj********************/

#define FCPKYJ(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTPKYJ,ftpkyj,INT,STRING,INT,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcpkyl********************/

#define FCPKYL(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTPKYL,ftpkyl,INT,STRING,LOGICAL,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcpkys********************/

#define FCPKYS(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTPKYS,ftpkys,INT,STRING,STRING,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcpkyt********************/

#define FCPKYT(unit,keyword,intkey,dblkey,comment,status) \
  CCALLSFSUB6(FTPKYT,ftpkyt,INT,STRING,INT,DOUBLE,STRING,PCINT,\
	      unit,keyword,intkey,dblkey,comment,status)

/******************** fcplsw********************/

#define FCPLSW(unit,status) \
  CCALLSFSUB2(FTPLSW,ftplsw,INT,PCINT,\
	      unit,status)

/******************** fcpmsg********************/

#define FCPMSG(errmsg) CCALLSFSUB1(FTPMSG,ftpmsg,STRING,errmsg)

/******************** fcpnul********************/

#define FCPNUL(unit,blank,status) \
  CCALLSFSUB3(FTPNUL,ftpnul,INT,INT,PCINT,\
	      unit,blank,status)

/******************** fcppnb********************/

#define ftppnb_ELEMS_5 ZTRINGV_ARGS(4)
#define ftppnb_ELEMLEN_5 ZTRINGV_NUM(1) 
#define FCPPNB(unit,group,fpixel,nelements,values,nulval,status)\
  CCALLSFSUB7(FTPPNB,ftppnb,INT,INT,INT,INT,BYTEV,BYTE,PCINT,\
	      unit,group,fpixel,nelements,values,nulval,status)

/******************** fcppnd********************/

#define FCPPND(unit,group,fpixel,nelements,values,nulval,status)\
  CCALLSFSUB7(FTPPND,ftppnd,INT,INT,INT,INT,DOUBLEV,DOUBLE,PCINT,\
	      unit,group,fpixel,nelements,values,nulval,status)

/******************** fcppne********************/

#define FCPPNE(unit,group,fpixel,nelements,values,nulval,status)\
  CCALLSFSUB7(FTPPNE,ftppne,INT,INT,INT,INT,FLOATV,FLOAT,PCINT,\
	      unit,group,fpixel,nelements,values,nulval,status)

/******************** fcppni********************/

#define FCPPNI(unit,group,fpixel,nelements,values,nulval,status)\
  CCALLSFSUB7(FTPPNI,ftppni,INT,INT,INT,INT,SHORTV,SHORT,PCINT,\
	      unit,group,fpixel,nelements,values,nulval,status)

/******************** fcppnj********************/

#define FCPPNJ(unit,group,fpixel,nelements,values,nulval,status)\
  CCALLSFSUB7(FTPPNJ,ftppnj,INT,INT,INT,INT,INTV,INT,PCINT,\
	      unit,group,fpixel,nelements,values,nulval,status)

/******************** fcpprb********************/

#define ftpprb_ELEMLEN_5 ZTRINGV_ARGS(4)
#define FCPPRB(unit,group,fpixel,nelements,values,status)\
  CCALLSFSUB6(FTPPRB,ftpprb,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,group,fpixel,nelements,values,status)

/******************** fcpprd********************/

#define FCPPRD(unit,group,fpixel,nelements,values,status)\
  CCALLSFSUB6(FTPPRD,ftpprd,INT,INT,INT,INT,DOUBLEV,PCINT,\
	      unit,group,fpixel,nelements,values,status)

/******************** fcppre********************/

#define FCPPRE(unit,group,fpixel,nelements,values,status)\
  CCALLSFSUB6(FTPPRE,ftppre,INT,INT,INT,INT,FLOATV,PCINT,\
	      unit,group,fpixel,nelements,values,status)

/******************** fcppri********************/

#define FCPPRI(unit,group,fpixel,nelements,values,status)\
  CCALLSFSUB6(FTPPRI,ftppri,INT,INT,INT,INT,SHORTV,PCINT,\
	      unit,group,fpixel,nelements,values,status)

/******************** fcpprj********************/

#define FCPPRJ(unit,group,fpixel,nelements,values,status)\
  CCALLSFSUB6(FTPPRJ,ftpprj,INT,INT,INT,INT,INTV,PCINT,\
	      unit,group,fpixel,nelements,values,status)

/******************** fcppru********************/

#define FCPPRU(unit,group,fpixel,nelements,status) \
  CCALLSFSUB5(FTPPRU,ftppru,INT,INT,INT,INT,PCINT,\
	      unit,group,fpixel,nelements,status)

/******************** fcprec********************/

#define FCPREC(unit,card,status) \
  CCALLSFSUB3(FTPREC,ftprec,INT,STRING,PCINT,\
	      unit,card,status)

/******************** fcpscl********************/

#define FCPSCL(unit,bscale,bzero,status) \
  CCALLSFSUB4(FTPSCL,ftpscl,INT,DOUBLE,DOUBLE,PCINT,\
	      unit,bscale,bzero,status)

/******************** fcpssb********************/

#define FCPSSB(unit,group,naxis,naxes,fpixels,lpixels,array,status)\
  CCALLSFSUB8(FTPSSB,ftpssb,INT,INT,INT,INTV,INTV,INTV,BYTEV,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,array,status)

/******************** fcpssd********************/

#define FCPSSD(unit,group,naxis,naxes,fpixels,lpixels,array,status)\
  CCALLSFSUB8(FTPSSD,ftpssd,INT,INT,INT,INTV,INTV,INTV,DOUBLEV,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,array,status)

/******************** fcpsse********************/

#define FCPSSE(unit,group,naxis,naxes,fpixels,lpixels,array,status)\
  CCALLSFSUB8(FTPSSE,ftpsse,INT,INT,INT,INTV,INTV,INTV,FLOATV,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,array,status)

/******************** fcpssi********************/

#define FCPSSI(unit,group,naxis,naxes,fpixels,lpixels,array,status)\
  CCALLSFSUB8(FTPSSI,ftpssi,INT,INT,INT,INTV,INTV,INTV,SHORTV,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,array,status)

/******************** fcpssj********************/

#define FCPSSJ(unit,group,naxis,naxes,fpixels,lpixels,array,status)\
  CCALLSFSUB8(FTPSSJ,ftpssj,INT,INT,INT,INTV,INTV,INTV,INTV,PCINT,\
	      unit,group,naxis,naxes,fpixels,lpixels,array,status)

/******************** fcpsvc********************/

#define ftpsvc_ELEMLEN_2 ZTRINGV_NUM(FITS_FLEN_KEYVAL)
#define ftpsvc_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define FCPSVC(card,value,comment,status) \
  CCALLSFSUB4(FTPSVC,ftpsvc,STRING,PZTRING,PZTRING,PCINT,\
	      card,value,comment,status)

/******************** fcptbb********************/

#define FCPTBB(unit,frow,startbyte,nbytes,array,status)\
  CCALLSFSUB6(FTPTBB,ftptbb,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,frow,startbyte,nbytes,array,status)

/******************** fcptbs********************/

#define FCPTBS(unit,frow,startchar,nchars,string,status)\
  CCALLSFSUB6(FTPTBS,ftptbs,INT,INT,INT,INT,BYTEV,PCINT,\
	      unit,frow,startchar,nchars,string,status)

/******************** fcptdm********************/

#define FCPTDM(unit,colnum,naxis,naxes,status)\
  CCALLSFSUB5(FTPTDM,ftptdm,INT,INT,INT,INTV,PCINT,\
	      unit,colnum,naxis,naxes,status)

/******************** fcpthp********************/

#define FCPTHP(unit,theap,status) \
  CCALLSFSUB3(FTPTHP,ftpthp,INT,INT,PCINT,\
	      unit,theap,status)

/******************** fcrdef********************/

#define FCRDEF(unit,status) \
  CCALLSFSUB2(FTRDEF,ftrdef,INT,PCINT,\
	      unit,status)

/******************** fcsnul********************/

#define FCSNUL(unit,colnum,snull,status) \
  CCALLSFSUB4(FTSNUL,ftsnul,INT,INT,STRING,PCINT,\
	      unit,colnum,snull,status)

/******************** fctkey********************/

#define FCTKEY(keyword,status) \
  CCALLSFSUB2(FTTKEY,fttkey,STRING,PCINT,\
	      keyword,status)

/******************** fctnul********************/

#define FCTNUL(unit,colnum,tnull,status) \
  CCALLSFSUB4(FTTNUL,fttnul,INT,INT,INT,PCINT,\
	      unit,colnum,tnull,status)

/******************** fctscl********************/

#define FCTSCL(unit,colnum,tscale,tzero,status) \
  CCALLSFSUB5(FTTSCL,fttscl,INT,INT,DOUBLE,DOUBLE,PCINT,\
	      unit,colnum,tscale,tzero,status)

/******************** fcucrd********************/

#define FCUCRD(unit,keyword,card,status) \
  CCALLSFSUB4(FTUCRD,ftucrd,INT,STRING,STRING,PCINT,\
	      unit,keyword,card,status)

/******************** fcukyd********************/

#define FCUKYD(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTUKYD,ftukyd,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcukye********************/

#define FCUKYE(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTUKYE,ftukye,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcukyf********************/

#define FCUKYF(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTUKYF,ftukyf,INT,STRING,FLOAT,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcukyg********************/

#define FCUKYG(unit,keyword,keyval,decimals,comment,status) \
  CCALLSFSUB6(FTUKYG,ftukyg,INT,STRING,DOUBLE,INT,STRING,PCINT,\
	      unit,keyword,keyval,decimals,comment,status)

/******************** fcukyj********************/

#define FCUKYJ(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTUKYJ,ftukyj,INT,STRING,INT,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcukyl********************/

#define FCUKYL(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTUKYL,ftukyl,INT,STRING,LOGICAL,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcukys********************/

#define FCUKYS(unit,keyword,keyval,comment,status) \
  CCALLSFSUB5(FTUKYS,ftukys,INT,STRING,STRING,STRING,PCINT,\
	      unit,keyword,keyval,comment,status)

/******************** fcupch********************/

#define FCUPCH(strng) CCALLSFSUB1(FTUPCH,ftupch,PSTRING,strng)

/******************** fcvers********************/

#define FCVERS(version) CCALLSFSUB1(FTVERS,ftvers,PCFLOAT,version)

/******************** fcgcnn********************/

#define ftgcnn_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_HDEFKWDS) 
#define FCGCNN(unit,exact,colname,realname,colnum,status) \
  CCALLSFSUB6(FTGCNN,ftgcnn,INT,LOGICAL,STRING,PZTRING,PCINT,PCINT,\
	      unit,exact,colname,realname,colnum,status)

/******************** fccmps********************/

#define FCCMPS(template,strng,casesn,match,exact) \
  CCALLSFSUB5(FTCMPS,ftcmps,STRING,STRING,LOGICAL,PCLOGICAL,PCLOGICAL,\
	      template,strng,casesn,match,exact)

/******************** fcpcks********************/

#define FCPCKS(unit,status) \
  CCALLSFSUB2(FTPCKS,ftpcks,INT,PCINT,\
	      unit,status)

/******************** fcucks********************/

#define FCUCKS(unit,status) \
  CCALLSFSUB2(FTUCKS,ftucks,INT,PCINT,\
	      unit,status)

/******************** fcvcks********************/

#define FCVCKS(unit,dataok,hduok,status) \
  CCALLSFSUB4(FTVCKS,ftvcks,INT,PCINT,PCINT,PCINT,\
	      unit,dataok,hduok,status)

/******************** fcgcks********************/

#define FCGCKS(unit,datasum,hdusum,status) \
  CCALLSFSUB4(FTGCKS,ftgcks,INT,PCDOUBLE,PCDOUBLE,PCINT,\
	      unit,datasum,hdusum,status)

/******************** fcesum********************/

#define ftesum_ELEMLEN_3 ZTRINGV_NUM(FITS_FLEN_CHECKSUM)
#define FCESUM(sum,comp,chksum) \
  CCALLSFSUB3(FTESUM,ftesum,DOUBLE,LOGICAL,PZTRING,\
	      sum,comp,chksum)

/******************** fcdsum********************/

#define FCDSUM(chksum,comp,sum) \
  CCALLSFSUB3(FTDSUM,ftdsum,STRING,LOGICAL,PCDOUBLE,\
	      chksum,comp,sum)

/******************** fcgics********************/

#define ftgics_ELEMLEN_9 ZTRINGV_NUM(FITS_FLEN_CTYPE)
#define FCGICS(unit,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,status) \
  CCALLSFSUB10(FTGICS,ftgics,INT,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PZTRING,PCINT,\
	      unit,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,status)

/******************** fcgtcs********************/

#define ftgtcs_ELEMLEN_B ZTRINGV_NUM(FITS_FLEN_CTYPE)
#define FCGTCS(unit,xcol,ycol,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,status) \
  CCALLSFSUB12(FTGTCS,ftgtcs,INT,INT,INT,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PCDOUBLE,PZTRING,PCINT,\
	      unit,xcol,ycol,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,status)

/******************** fcwldp********************/

#define FCWLDP(xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,xpos,ypos,status) \
 CCALLSFSUB13(FTWLDP,ftwldp,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,STRING,PCDOUBLE,PCDOUBLE,PCINT,\
	      xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,xpos,ypos,status)

/******************** fcxypx********************/

#define FCXYPX(xpos,ypos,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,xpix,ypix,status) \
  CCALLSFSUB13(FTXYPX,ftxypx,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,STRING,PCDOUBLE,PCDOUBLE,PCINT,\
	      xpos,ypos,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,xpix,ypix,status)

/******************** fcgtcl********************/

#define FCGTCL(unit,colnum,code,repeat,width,status) \
  CCALLSFSUB6(FTGTCL,ftgtcl,INT,INT,PCINT,PCINT,PCINT,PCINT,\
	      unit,colnum,code,repeat,width,status)

/******************** fcasfm********************/

#define FCASFM(tform,code,width,decims,status) \
  CCALLSFSUB5(FTASFM,ftasfm,STRING,PCINT,PCINT,PCINT,PCINT,\
	      tform,code,width,decims,status)

#endif /* _CFITSIO */
