      SUBROUTINE WRTIMA(Lun, Ig, Itfun, Icbar, Zscale,
     &      Cctnam, Ctmp)
      INTEGER   Lun, Ig, Itfun, Icbar
      REAL      Zscale(2)
      CHARACTER Cctnam*(*), Ctmp*(*)
C---
C Write out lines that describe the current contour plot.
C---
C Lun     I
C Ig      I    Image number
C Itfun   I
C Icbar   I
C Zscale  I
C Cctnam  I
C Ctmp    S    Scratch area
C---
C 2010-May-28 - [AFT]
C---
      INTEGER   LENACT
C
      INTEGER   ltmp
C---
   11 FORMAT(A)
C---
      WRITE(Ctmp,121) Ig,Zscale(1),Zscale(2)
  121 FORMAT('IMAG',I4,' MIN',1PG11.4,' MAX',G11.4)
      ltmp = 37
C
      IF ( itfun.LT.0 ) THEN
         Ctmp(ltmp+1:)='HIST '
         ltmp=ltmp+5
      ELSE IF ( itfun.EQ.1 ) THEN
         Ctmp(ltmp+1:)='LOG  '
         ltmp=ltmp+5
      ELSE IF ( itfun.EQ.2 ) THEN
         Ctmp(ltmp+1:)='SQRT '
         ltmp=ltmp+5
      END IF
C
      IF ( icbar.GT.0 ) THEN
         Ctmp(ltmp+1:)='CBar ON '
         ltmp=ltmp+8
      END IF
C---
      IF(Lun.EQ.0) THEN
         WRITE(*,11) Ctmp(:ltmp-1)
      ELSE
         WRITE(Lun,11) Ctmp(:ltmp-1)
      END IF
C
      WRITE(Ctmp,151) Ig,cctnam(:LENACT(cctnam))
  151 FORMAT('IMAG',I4,' CCT ',A)
      ltmp=LENACT(Ctmp)
      IF(Lun.EQ.0) THEN
         WRITE(*,11) Ctmp(:ltmp)
      ELSE
         WRITE(Lun,11) Ctmp(:ltmp)
      END IF
      RETURN
      END
