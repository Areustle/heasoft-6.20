      PROGRAM FIG06
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   MXI, MXJ
      PARAMETER (MXI=30,MXJ=30)
C
      INTEGER   IERY(MXJ)
      INTEGER   I, J, K
      REAL      F(MXI,MXJ)
      REAL      XT, YT
C
   11 FORMAT(A)
C
C Compute a suitable function.
C
      DO I=1,MXI
         DO J=1,MXJ
            XT=I
            YT=J
            F(I,J) = COS(0.6*SQRT(2.*XT)-0.4*YT/3.)*COS(0.4*XT/3)+
     1                     (XT-YT)/FLOAT(MXI)
         END DO
      END DO
      DO j=1,MXJ
         iery(j) = 0
      END DO
C---
      OPEN(UNIT=2,FILE='fig06.qdp',STATUS='NEW')
      WRITE(2,11) 'COL OFF 1..999'
      WRITE(2,11) 'CON LEV -1 -.5 0 .5 1'
      WRITE(2,11) 'CON COL 2 8 3 4 6 LS 4 4 1 1 1   LW 1 2 3 4 5'
      WRITE(2,11) 'R   1 30 1 30'
      WRITE(2,11) '!'
      DO I=1,MXI
         CALL WRQDAT(2, 3, F(I,1), iery, MXI, MXJ)
      END DO
      CLOSE(UNIT=2)
      END
