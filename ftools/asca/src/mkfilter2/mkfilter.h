#include "cfortran.h"
#include "pctype.h"

/* C calls FORTRAN macros... */

#define FTRDEF(A1,A2) CCALLSFSUB2(FTRDEF,ftrdef,INT,PINT,A1,A2)

#define mkf2ReadInfFil(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14) \
 CCALLSFSUB14(mkf2ReadInfFil,mkf2readinffil,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,FLOATV,PDOUBLE,PSTRING,PLOGICAL,PDOUBLE,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14)

#define FTGKYJ(A1,A2,A3,A4,A5) CCALLSFSUB5(FTGKYJ,ftgkyj,INT,STRING,PINT,PSTRING,PINT,A1,A2,A3,A4,A5)

#define FTGKYD(A1,A2,A3,A4,A5) CCALLSFSUB5(FTGKYD,ftgkyd,INT,STRING,PDOUBLE,PSTRING,PINT,A1,A2,A3,A4,A5)

#define FTGKYS(A1,A2,A3,A4,A5) CCALLSFSUB5(FTGKYS,ftgkys,INT,STRING,PSTRING,PSTRING,PINT,A1,A2,A3,A4,A5)

#define FCECHO(A1) CCALLSFSUB1(FCECHO,fcecho,PSTRING,A1)

#define FTOPEN(A1,A2,A3,A4,A5) CCALLSFSUB5(FTOPEN,ftopen,INT,PSTRING,INT,PINT,PINT,A1,A2,A3,A4,A5)

#define FTMRHD(A1,A2,A3,A4) CCALLSFSUB4(FTMRHD,ftmrhd,INT,INT,PINT,PINT,A1,A2,A3,A4)

#define FTMAHD(A1,A2,A3,A4) CCALLSFSUB4(FTMAHD,ftmahd,INT,INT,PINT,PINT,A1,A2,A3,A4)

#define FTGCNO(A1,A2,A3,A4,A5) CCALLSFSUB5(FTGCNO,ftgcno,INT,INT,STRING,PINT,PINT,A1,A2,A3,A4,A5)

#define FTGCFD(A1,A2,A3,A4,A5,A6,A7,A8,A9) CCALLSFSUB9(FTGCFD,ftgcfd,INT,INT,INT,INT,INT,DOUBLEV,INTV,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

#define FTGCFS(A1,A2,A3,A4,A5,A6,A7,A8,A9) CCALLSFSUB9(FTGCFS,ftgcfs,INT,INT,INT,INT,INT,PSTRING,INTV,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

#define FTGCFJ(A1,A2,A3,A4,A5,A6,A7,A8,A9) CCALLSFSUB9(FTGCFJ,ftgcfj,INT,INT,INT,INT,INT,INTV,INTV,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

#define FTCLOS(A1,A2) CCALLSFSUB2(FTCLOS,ftclos,INT,PINT,A1,A2)

#define FFINIT(A1,A2,A3) CCALLSFSUB3(FFINIT,ffinit,INT,PSTRING,PINT,A1,A2,A3)

#define FTINIT(A1,A2,A3,A4) CCALLSFSUB4(FTINIT,ftinit,INT,PSTRING,INT,PINT,A1,A2,A3,A4)

#define FTPHPR(A1,A2,A3,A4,A5,A6,A7,A8,A9)\
 CCALLSFSUB9(FTPHPR,ftphpr,INT,LONG,INT,INT,INT,INT,INT,LONG,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

#define FTPKYS(A1,A2,A3,A4,A5) CCALLSFSUB5(FTPKYS,ftpkys,INT,STRING,STRING,STRING,PINT,A1,A2,A3,A4,A5)

#define FTPKYF(A1,A2,A3,A4,A5,A6) CCALLSFSUB6(FTPKYF,ftpkyf,INT,STRING,FLOAT,INT,STRING,PINT,A1,A2,A3,A4,A5,A6)

#define FTPKYD(A1,A2,A3,A4,A5,A6) CCALLSFSUB6(FTPKYD,ftpkyd,INT,STRING,DOUBLE,INT,STRING,PINT,A1,A2,A3,A4,A5,A6)

#define FTPDAT(A1,A2) CCALLSFSUB2(FTPDAT,ftpdat,INT,PINT,A1,A2)

#define FTPDEF(A1,A2,A3,A4,A5,A6,A7) CCALLSFSUB7(FTPDEF,ftpdef,INT,INT,INT,INT,INT,INT,PINT,A1,A2,A3,A4,A5,A6,A7)

#define FTCRHD(A1,A2) CCALLSFSUB2(FTCRHD,ftcrhd,INT,PINT,A1,A2)

#define mkf2Ftphbn(A1,A2) CCALLSFSUB2(mkf2Ftphbn,mkf2ftphbn,INT,PINT,A1,A2)

#define FTMKYJ(A1,A2,A3,A4,A5) CCALLSFSUB5(FTMKYJ,ftmkyj,INT,STRING,INT,STRING,PINT,A1,A2,A3,A4,A5)

#define FTMKYD(A1,A2,A3,A4,A5,A6) CCALLSFSUB6(FTMKYD,ftmkyd,INT,STRING,DOUBLE,INT,STRING,PINT,A1,A2,A3,A4,A5,A6)

#define FTMKYS(A1,A2,A3,A4,A5) CCALLSFSUB5(FTMKYS,ftmkys,INT,STRING,STRING,STRING,PINT,A1,A2,A3,A4,A5)

#define FTPCLS(A1,A2,A3,A4,A5,A6,A7)\
 CCALLSFSUB7(FTPCLS,ftpcls,INT,INT,INT,INT,INT,PSTRING,PINT,A1,A2,A3,A4,A5,A6,A7)

#define FTPCLD(A1,A2,A3,A4,A5,A6,A7)\
 CCALLSFSUB7(FTPCLD,ftpcld,INT,INT,INT,INT,INT,DOUBLE,PINT,A1,A2,A3,A4,A5,A6,A7)

#define FTPCLE(A1,A2,A3,A4,A5,A6,A7)\
 CCALLSFSUB7(FTPCLE,ftpcle,INT,INT,INT,INT,INT,FLOAT,PINT,A1,A2,A3,A4,A5,A6,A7)

#define FTPCLB(A1,A2,A3,A4,A5,A6,A7)\
 CCALLSFSUB7(FTPCLB,ftpclb,INT,INT,INT,INT,INT,BYTE,PINT,A1,A2,A3,A4,A5,A6,A7)

#define FTPCLJ(A1,A2,A3,A4,A5,A6,A7)\
 CCALLSFSUB7(FTPCLJ,ftpclj,INT,INT,INT,INT,INT,INT,PINT,A1,A2,A3,A4,A5,A6,A7)

#define FTGCVJ(A1,A2,A3,A4,A5,A6,A7,A8,A9)\
 CCALLSFSUB9(FTGCVJ,ftgcvj,INT,INT,INT,INT,INT,INT,INTV,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

#define FTGCVD(A1,A2,A3,A4,A5,A6,A7,A8,A9)\
 CCALLSFSUB9(FTGCVD,ftgcvd,INT,INT,INT,INT,INT,DOUBLE,DOUBLEV,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

#define NUMCAL2(A1,A2,A3,A4) \
 CCALLSFSUB4(NUMCAL2,numcal2,DOUBLEV,DOUBLEV,DOUBLE,PDOUBLE,A1,A2,A3,A4)

#define GETLUN(A1) CCALLSFSUB1(GETLUN,getlun,PINT,A1)

#define FRELUN(A1) CCALLSFSUB1(FRELUN,frelun,INT,A1)


#define GTICLEAN(gtistart, gtistop, ngti, mxgti, astart, astop, anum, status)\
     CCALLSFSUB8( GTICLEAN, gticlean, \
		  DOUBLEV, DOUBLEV, PCINT, INT, DOUBLEV, DOUBLEV, INT, PCINT, \
		  gtistart, gtistop, ngti, mxgti, astart, astop, anum, status)

PROTOCCALLSFFUN6(DOUBLE, GTIEXP, gtiexp, \
		 DOUBLE, DOUBLE, DOUBLEV, DOUBLEV, INT, PCINT)
#define GTIEXP( t1, t2, gtistart, gtistop, ngti, status ) \
     CCALLSFFUN6( GTIEXP, gtiexp, \
		  DOUBLE, DOUBLE, DOUBLEV, DOUBLEV, INT, PCINT, \
		  t1, t2, gtistart, gtistop, ngti, status )