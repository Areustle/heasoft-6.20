#ifndef NULL_H
#define NULL_H

/*
 *  Routines that handle null values in C (by way of fortran)
 *
 *  Usage:
 *
 *  INTEGER
 
   int ivar;
   ivar = INULL();    # Sets ivar to special integer defined as null

   if ( isinull(ivar) ) {   # Checks if ivar is null
      do something
   }
 *
 *  REAL
 *
    float rvar;

    rvar = RNULL();   # Sets rvar to special real value defined as null
 
    if ( ISRNULL(rvar) ) {   # Checks if rvar is null
       do something
    }
 *
 *  DOUBLE
 *
    double dvar;

    dvar = DNULL();   # Sets dvar to special double value defined as null
 
    if ( ISDNULL(dvar) ) {   # Checks if dvar is null
       do something
    }
 */

PROTOCCALLSFFUN0(INT,INULL,inull)
#define INULL() CCALLSFFUN0(INULL, inull)

PROTOCCALLSFFUN1(INT,ISINULL,isinull,INT)
#define ISINULL(I) CCALLSFFUN1(ISINULL, isinull, INT, I)

PROTOCCALLSFFUN0(FLOAT,RNULL,rnull)
#define RNULL() CCALLSFFUN0(RNULL, rnull)

PROTOCCALLSFFUN1(INT,ISRNULL,isrnull,FLOAT)
#define ISRNULL(R) CCALLSFFUN1(ISRNULL, isrnull, FLOAT, R)

PROTOCCALLSFFUN0(DOUBLE,DNULL,dnull)
#define DNULL() CCALLSFFUN0(DNULL, dnull)

PROTOCCALLSFFUN1(INT,ISDNULL,isdnull,DOUBLE)
#define ISDNULL(D) CCALLSFFUN1(ISDNULL, isdnull, DOUBLE, D)

#endif
