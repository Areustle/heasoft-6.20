
/*
 filename:	compat.h
 purpose:	correct for standard unix/vms anomalies
 author:	Geoffrey B. Crew
 date:		July 1993
 updated:	February 1996
                Jeff Guerber, Dec 1997: Added Gtcalf cfortran macro.
 */

/*
 *  Include the world here to support fortran links.
 */
#include "cfitsio.h"
#include "pfile.h"

#define BufLen_2 ((unsigned)(MAX_FNAME-1))	/* Cf. Uclgst macro */

#ifndef FALSE
# define	FALSE	0			/* for FITSIO logic */
#endif /* FALSE */
#ifndef TRUE
# define	TRUE	1			/* for FITSIO logic */
#endif /* TRUE */

/*
 *  CF uses memset() with a sizeof when it can,
 *  so all other lengths must also be made unsigned.
 */
#ifdef lint
# undef FITS_CLEN_KEYNAME
# undef FITS_FLEN_KEYNAME
# undef FITS_CLEN_KEYVAL
# undef FITS_FLEN_KEYVAL
# undef FITS_CLEN_COMMENT
# undef FITS_FLEN_COMMENT
# undef FITS_CLEN_CARD
# undef FITS_FLEN_CARD
# undef FITS_CLEN_ERRMSG
# undef FITS_FLEN_ERRMSG
# undef FITS_CLEN_CHECKSUM
# undef FITS_FLEN_CHECKSUM
# define FITS_CLEN_KEYNAME    (unsigned)9
# define FITS_FLEN_KEYNAME    (unsigned)8
# define FITS_CLEN_KEYVAL    (unsigned)69
# define FITS_FLEN_KEYVAL    (unsigned)68
# define FITS_CLEN_COMMENT   (unsigned)73
# define FITS_FLEN_COMMENT   (unsigned)72
# define FITS_CLEN_CARD      (unsigned)81
# define FITS_FLEN_CARD      (unsigned)80
# define FITS_CLEN_ERRMSG    (unsigned)31
# define FITS_FLEN_ERRMSG    (unsigned)30
# define FITS_CLEN_CHECKSUM  (unsigned)17
# define FITS_FLEN_CHECKSUM  (unsigned)16
#endif /* lint */

/*
 *  Possibly not correct on all architectures?
 */
#ifdef RETURNFLOAT
# undef RETURNFLOAT
#endif /* RETURNFLOAT */
#ifdef ASSIGNFLOAT
# undef ASSIGNFLOAT
#endif /* ASSIGNFLOAT */
#ifdef NeXT
# define _POSIX_SOURCE
# include <math.h>
# include<ansi/m68k/math.h>
#else
# include <math.h>
#endif /* NeXT */
#ifdef vms
/* compensate for holes in vms <math.h> */
# define M_2_SQRTPI	1.12837916709551257390
# define M_SQRT2	1.41421356237309504880
# define M_PI		3.14159265358979323846
extern double	strtod();
#endif /* vms */

/* get correct memset. jdd */
#include <string.h>
#ifdef sun
#include <memory.h>
#endif


/* extern char *memset(); */

/*
 *  Cf. getpha.f
 */
#define getpha_ELEMLEN_4 ZTRINGV_NUM(FITS_FLEN_COMMENT)
#define get_pha(file, ints, doubs, mesg, status) \
	CCALLSFSUB5(GETPHA,getpha,STRING,PCINT,PCDOUBLE,PZTRING,PCINT,\
				  file,  ints, doubs,   mesg,   status)

#define DEL_FIL(file, status) \
	CCALLSFSUB2(DELFIL,delfil,STRING,PCINT,\
				  file,  status)
#define FC_ECHO(message) \
	CCALLSFSUB1(FCECHO,fcecho,STRING,\
				  message)

/* Note: filenam, extno, and online should really be *arrrays*, but this */
/* #define currently treats them as scalers (ie, maxret=1)               */
/* (hmmm, maybe maxret could go in the gtcalf_ELEM_... macros as the 2nd dim)*/

#define gtcalf_ELEMLEN_13 ZTRINGV_NUM(MAX_FNAME*2-1)  /* len(filenam)-1 */
#define gtcalf_ELEMLEN_15 ZTRINGV_NUM(20)  /* len(online)-1 */

#define Gtcalf(chatter,tele,instr,detnam,filt,codenam,strtdate,strtime,\
	       stpdate,stptime,expr,maxret,filenam,extno,online,nret,nfound,\
	       status)\
     CCALLSFSUB18(GTCALF,gtcalf,INT,PSTRING,PSTRING,PSTRING,PSTRING,\
		  PSTRING,STRING,STRING,STRING,STRING,PSTRING,INT,\
		  PZTRING,PCINT,PZTRING,PCINT,PCINT,PCINT,\
		  chatter,tele,instr,detnam,filt,codenam,strtdate,strtime,\
		  stpdate,stptime,expr,maxret,filenam,extno,online,nret,\
		  nfound,status)


#ifdef lint
# define sprintf	bsd_sprintf
# define memset		cf_memset
#endif
#ifdef SISDEBUG
#	define ERRCHK(C,M,V,A)	do if(C){		\
			(void)fprintf(stderr,(M),(V));	\
			A;				\
		} while(0)
#else
#	define ERRCHK(C,M,V,A)	do if(C){		\
			extern void	errchk();	\
			static char	ms[160];	\
			(void)sprintf(ms,(M),(V));	\
			errchk(ms);			\
			A;				\
		} while(0)
#endif /* SISDEBUG */

/*
 *  Sixletter names beginning with sis for all external symbols
 *  to make it likely that this stuff will link together in IRAF.
 */
#ifndef SISDEBUG
#  define sisrmg_main	CFC_(SISRMG,sisrmg)	/* Cf sisrmg.c	*/
#  define string_fix	CFC_(SISFXS,sisfxs)	/* Cf sisrmg.c  */
#  define string_cat	CFC_(SISFCT,sisfct)	/* Cf sisrmg.c  */
#  define rmg_works	CFC_(SISWRK,siswrk)	/* everywhere.	*/
#  define get_point	CFC_(SISPTS,sispts)	/* Cf points.c	*/
#  define get_brnch	CFC_(SISBCH,sisbch)	/* Cf points.c	*/
#  define get_ecd	CFC_(SISECD,sisecd)	/* Cf points.c	*/
#  define rmf_array	CFC_(SISRMF,sisrmf)	/* Cf rmloop.c	*/
#  define rmf_ebnds	CFC_(SISEBX,sisebx)	/* Cf rmebxt.c	*/
#  define cal_stuff	CFC_(SISCAL,siscal)	/* Cf caltls.c	*/
#  define get_ph2pi	CFC_(SISP2P,sisp2p)	/* Cf secular.c	*/
#  define get_echos	CFC_(SISECH,sisech)	/* Cf secular.c	*/
#  define get_rddis	CFC_(SISRDD,sisrdd)	/* Cf secular.c	*/
#  define get_nucti	CFC_(SISNUC,sisnuc)	/* Cf secular.c	*/
#endif /* SISDEBUG */

/*
 *  End of compat.h
 */
