CH    SUBROUTINE XDIRCK            FROM PDP 11/70 RD1:[300,147]XDIRCK.FTN;1
CH                                 MICROVAX 3/26/88
CH
CCCC  XDIRCK.FTN
C Jesse Allen  23 Jan 1998  Implicit typing removed

      SUBROUTINE XDIRCK(ZRA,ZDEC,YRA,YDEC,SOR,ECP,QDIR,QBAD,HPT)

      implicit none

      logical qdir, qbad

      integer*2 hpoint, hpt

      real yra, ydec, zra, zdec, ecp(3), sor(3), y(3), z(3)
      real cossy, cosez


      QDIR=.TRUE.
      QBAD=.FALSE.
      HPOINT=IAND(INT(HPT),1)
      IF(HPOINT.EQ.1)GO TO 10
      CALL CVXYZ(ZRA,ZDEC,Z)
      CALL DOT(ECP,Z,COSEZ)
      IF(ABS(COSEZ).LT..09)GO TO 10
      QDIR=.FALSE.
      QBAD=.TRUE.
      RETURN
   10 CONTINUE
      CALL CVXYZ(YRA,YDEC,Y)
      CALL DOT(SOR,Y,COSSY)
      IF(COSSY.LT..935)QDIR=.FALSE.

      RETURN

      END
