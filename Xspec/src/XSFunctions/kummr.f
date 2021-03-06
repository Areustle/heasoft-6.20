      SUBROUTINE KUMMR(A, B, X, TEMP, KUM)
C     *********************************
C     BERECHNUNG DER KUMMER'SCHEN FUNKTIONEN, DIE FUER DAS
C     UNTER 7 DEFINIERTE COMPTONISIERUNGSSPEKTRUM BENOETIGT WERDEN
cc      IMPLICIT DOUBLEPRECISION(A-H, O-Z)
cc      IMPLICIT INTEGER*4(I-N)
      DOUBLE PRECISION X, Y, TEMP, SUM, A, B, SUMOLD, DABS
      INTEGER K
      DOUBLE PRECISION KUM, M

      Y = X/TEMP
      M = 1.0D0
      SUM = 1.0D0
      DO K = 1, 999
         M = M*(A+K-1.D0)*Y/((B+K-1.D0)*K)
         SUMOLD = SUM
         SUM = SUM + M
         KUM = SUM
         IF (DABS(SUM-SUMOLD).LT.0.00000001D0) THEN
            GOTO 20
         ENDIF
      ENDDO
C     WRITE(6,'(1X,"
c ! > 1000 CALC. OF KUMMERFKT. NECESSARY !! ")')
 20   CONTINUE
      END
