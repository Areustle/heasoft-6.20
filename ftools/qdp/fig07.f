      PROGRAM FIG07
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   MXI, MXJ
      PARAMETER (MXI=30,MXJ=30)
      REAL      FNGAUS
C
      INTEGER   IERY(MXJ)
      INTEGER   I, J, K
      REAL      F(MXI,MXJ)
      REAL      pval1(5), pval2(5), pval3(5)
      REAL      xmid, xt, ymid, yt
C put 3 "stars" in image.
      DATA  pval1/ 8.7, 2.6, 10.2, 2.8, 100./
      DATA  pval2/20.7, 2.6, 24.2, 2.8,  70./
      DATA  pval3/24.7, 2.6, 20.2, 2.8,  60./
C
   11 FORMAT(A)
C
C Compute a suitable function.
C
      XMID=MXI/2.
      YMID=MXJ/2.
      DO I=1,MXI
         DO J=1,MXJ
            xt = i
            yt = j
            F(I,J) = FNGAUS(xt, yt, pval1) + FNGAUS(xt, yt, pval2) +
     &               FNGAUS(xt, yt, pval3)
         END DO
      END DO
      DO j=1,MXJ
         iery(j) = 0
      END DO
C---
      OPEN(UNIT=2,FILE='fig07.qdp',STATUS='NEW')
      WRITE(2,11) 'COL OFF 1..999'
      WRITE(2,11) 'ADJ ON'
      WRITE(2,11) 'GAP 0.0 Errors'
      WRITE(2,11) 'LAB T Three gaussian "stars"'
      WRITE(2,11) 'LAB X RA'
      WRITE(2,11) 'LAB Y Dec'
      WRITE(2,11) 'CON LEV 5 10 20 40 80 LS 4 4 4 4 4'
      WRITE(2,11) 'IMA MIN 0 MAX 100 CCT -3 CB'
      WRITE(2,11) '!'
      DO I=1,MXI
         CALL WRQDAT(2, 3, F(I,1), iery, MXI, MXJ)
      END DO
      CLOSE(UNIT=2)
      END
C*********
      REAL FUNCTION FNGAUS(Xt, Yt, Pval)
      REAL      Xt, Yt, Pval(5)
C---
C Pval(1)  I    X coordinate of center
C Pval(2)  I    X width
C Pval(3)  I    Y coordinate of center
C Pval(4)  I    Y width
C Pval(5)  I    peak value
C---
      REAL      ex, xsig, ysig
C---
      xsig = (xt-PVAL(1))/PVAL(2)
      ysig = (yt-PVAL(3))/PVAL(4)
      IF ( ABS(xsig).LT.12.0 .AND. ABS(ysig).LT.12.0 ) THEN
         ex = EXP(-(xsig*xsig+ysig*ysig)/2.)
      ELSE
         ex = 0.0
      END IF
      FNGAUS = PVAL(5)*ex
      RETURN
      END
