C
C File: clmacr.inc
C Description: COMMON definitions for CLI macro
C
C History:
C     04-Dec-1996 Y.ISHISAKI, created
C     25-Feb-2005 Y.ISHISAKI, change size 255 -> 256, define MACKSIZ, MACBSIZ
C
C const
      Integer  MAXMAC, MACKSIZ, MACBSIZ
      Parameter( MAXMAC = 1024 )
      Parameter( MACKSIZ = 64 )
      Parameter( MACBSIZ = 256 )
C common
      Common / CLMAC1 / NMAC,LKMAC(MAXMAC),LMMAC(MAXMAC)
      Common / CLMAC2 / MACKEY(MAXMAC),MACBUF(MAXMAC)
      Integer   *  4  NMAC, LKMAC, LMMAC
      Character * (MACKSIZ)  MACKEY
      Character * (MACBSIZ)  MACBUF
C
C     for Emacs
C     ;;; Local Variables: ***
C     ;;; mode:Fortran ***
C     ;;; End: ***
