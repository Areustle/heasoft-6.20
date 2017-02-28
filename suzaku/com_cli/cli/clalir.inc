C
C File: clalir.inc
C Description: COMMON definitions for CLI aliases
C
C History:
C     24-Aug-1995, created
C     22-Feb-2005, rename MAC -> ALI
C     25-Feb-2005 Y.ISHISAKI, define ALIKSIZ, ALIBSIZ
C
C const
      Integer  MAXALI, ALIKSIZ, ALIBSIZ
      Parameter( MAXALI = 1024 )
      Parameter( ALIKSIZ = 16 )
      Parameter( ALIBSIZ = 80 )
C common
      Common / CLALI1 / NALI, LKALI(MAXALI), LMALI(MAXALI)
      Common / CLALI2 / ALIKEY(MAXALI), ALIBUF(MAXALI)
      Integer   *  4  NALI, LKALI, LMALI
      Character * (ALIKSIZ)  ALIKEY
      Character * (ALIBSIZ)  ALIBUF
C
C     for Emacs
C     ;;; Local Variables: ***
C     ;;; mode:Fortran ***
C     ;;; End: ***
