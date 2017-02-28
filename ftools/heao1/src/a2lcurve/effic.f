CH    SUBROUTINE EFFIC          FROM PDP 11/70 RD1:[300,147]HEFFJK.FTN;1
CH                              TO MICROVAX 3/1/88
CCCC  HEFFJK.FTN FOR COMPUTING HEAOA2 EFFIC FOR HRATES
CCCC  VALUES OF AREAS AND OFFSETS AS OF 4/5/79
CCCC  J SWANK+DELOFF ADDED 9/13/79AND OFFSET MEDX 16/16/79
CCCC  CHANGED TO USE MY BEST OFFSETS(M,H3) AND KIMS(H1,H2)
CCCC  AS OF 9/29/79 + COR TO HD1(Z)
C     J ALLEN 28 Oct 1997  Corrected MED total areas
C             12 Dec 1997  Remove DELOFF (Was set to zero anyway)
C             23 Jan 1998  Implicit typing removed
C             26 Jan 1998  Returned collimator correction instead of 
C                          total and effective cross sections
C             16 Feb 1998  Reordered arrays to handle MED, HED-1,2,3 order
C  L. Breedon 19 Oct 1998  Handle just 1 detector (rather than up to 4)
C  L. Breedon 26 Oct 1998  Eliminate common blocks
C  L. Breedon 26 Oct 1998  Parse ANGLIM parameter (instead of via common)

      SUBROUTINE EFFIC(YRA,YDEC,ZRA,ZDEC,SRA,SDEC,HDET,HD,JFD,
     &                   ANGLIM,QOK,QANG,eff)

      implicit none



      logical qok(4), qang

      integer jfd
c      real scanf

      character(40) taskname
      common /TASK/ taskname

C Local variables

      logical qleft, qright

      integer*2 hdet, hd(4), hhh, hsign, hm

      integer jd, js, iside

      real eff, effx, effz, vignet, dtr,anglim(2)
      real sra, sdec, yra, ydec, zra, zdec, sx, sy, sz
      real x(3), y(3), z(3), s(3), v(3), vp(3), vp1(3)
      real offset(3,4), detang(4,4)
      real scndec, scnoff, a, ang, angx, detx, detz

      

C      DATA OFFSET/.0575,.0275,.0467,.0919,.0275,6.0753,0.00
C     *,.1008,6.009,.0100,.0114,.0600,.1350,.0183,-.0125,
C     *.075,.0044,.0325/
      DATA OFFSET/ .1350, .0183, -.0125, 0.00, .1008, 6.009,
     +             .0100, .0114,  .0600, .075, .0044, .0325 /
C      DATA DETANG/1.55,2.95,2.8,2.55,4.15,2.8,2.7,2.75,2.85,
C     *2.74,5.78,2.74,2.91,2.84,5.92,2.84,2.94,2.87,1.4,2.93,
C     *2.91,2.79,1.47,2.79/
      DATA DETANG/ 2.94, 2.87,  1.4, 2.93, 2.85, 2.74, 5.78, 2.74,
     +             2.91, 2.84, 5.92, 2.84, 2.91, 2.79, 1.47, 2.79 /
      DATA DTR/.0174532925D0/

C     THIS SUBROUTINE WILL CACULATE THE EFFECTIVE AREAS OF
C     A SOURCE IN HEAO DETECTORS FIELD OF VIEW...
C     MAXIMUM EFFICIENCY(ON AXIS) IS 1.
C      S. PRAVDO 10/5/77

C     CHANGE COORDINATES TO CARTESIAN

      CALL CVXYZ(YRA,YDEC,Y)
      CALL CVXYZ(ZRA,ZDEC,Z)
      CALL CVXYZ(SRA,SDEC,S)
      CALL CROSS(Y,Z,X)
      CALL SCANAN(YRA,YDEC,ZRA,ZDEC,SCNDEC,SCNOFF)
c      IF(JFD.EQ.1)SCANF=SCNDEC/DTR

C     FIND OFFSET ANGLE COS IN X,Y,Z DIRECTIONS

      CALL DOT(S,X,V(1))
      CALL DOT(S,Y,V(2))
      CALL DOT(S,Z,V(3))

C     ROTATE ANGLE COS BECAUSE OF DETECTOR Z OFFFSETS

         jd=1
         eff = 0.0
         IF(HDET.EQ.0) GO TO 100
         QLEFT=.FALSE.
         QRIGHT=.FALSE.
         IF(.NOT.QOK(hdet)) GO TO 100
         IF (HDET .EQ. 2) then
            A = SCNOFF
         else 
            A = SCNDEC
         endif
         CALL SCNCHK(jd,A,ANGLIM,QANG)
         ANG=OFFSET(3,HDET)*DTR
         CALL ROTAT(V,ANG,VP1)
         ANGX=OFFSET(1,HDET)*DTR
         CALL ROTATX(VP1,ANGX,VP)
         SX=ABS(VP(1))
         SY=VP(2)
         SZ=ABS(VP(3))

C     CALCULATE EFFICIENCY

         DO 101 JS=1,4
            HHH=ABS(HD(JS))
            IF(HHH.EQ.0) GO TO 101
            HSIGN=1
            IF(HHH.NE.HD(JS)) HSIGN=-1
            HM=MOD(INT(HHH),2)
            IF(HM.EQ.0.AND.QRIGHT) GO TO 101
            IF(HM.EQ.1.AND.QLEFT)  GO TO 101
            IF(HM.EQ.0) QRIGHT=.TRUE.
            IF(HM.EQ.1) QLEFT=.TRUE.
            ISIDE=1
            IF(HM.EQ.0) ISIDE=2
            DETZ=DETANG(2*ISIDE,HDET)*DTR
            DETX=DETANG(2*ISIDE-1,HDET)*DTR
            EFFZ=1.-SZ/SY/TAN(DETZ)
            EFFX=1.-SX/SY/TAN(DETX)
            vignet = SY*EFFX*EFFZ
            IF(EFFX.LT.0..OR.EFFX.GT.1..OR.EFFZ.LT.0..OR.EFFZ.GT.1.)
     *        vignet = 0.0
            eff = eff + vignet
 101     CONTINUE
 100  CONTINUE
      RETURN
      END
