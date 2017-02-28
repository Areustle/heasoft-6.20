C
C File: clhcmd.inc
C Description: COMMON definitions for HISTORY facility
C
C History:
C     21-Feb-2005 Y.ISHISAKI, extracted from clhcmd.f, increase HISBUF 80->256
C     02-Mar-2005 Y.ISHISAKI, add LHIS(0:MAXHIS-1)
C
C CONST
       Integer MAXHIS
       Parameter( MAXHIS = 20 )
C
C Common
       Common / CLHIS1 / HISBUF(0:MAXHIS-1)
       Common / CLHIS2 / LHIS(0:MAXHIS-1), IHEAD, ITAIL, NENT
C
       Character * (LINSIZ)  HISBUF
       Integer * 4  LHIS, IHEAD, ITAIL, NENT
C
C     for Emacs
C     ;;; Local Variables: ***
C     ;;; mode:Fortran ***
C     ;;; End: ***
