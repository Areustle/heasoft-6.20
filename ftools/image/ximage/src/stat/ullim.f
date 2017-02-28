      SUBROUTINE ULLIM(Cntsrc,Cntbg,Conflev,Res,Status)
      implicit none
c
c   'sosta method' upper limit calculation
c
c  I  Cntsrc  (r) Raw counts
c  I  Cntbg   (r) Background level
c  I  Conflev (r) Confidence level (e.g. 0.9987 when sigma=3)
c  O  Res     (r) Result
c  O  Status  (i) Error flag (0=OK)
c
      INCLUDE '../include/io.inc'

      real*4 Cntsrc, Cntbg, Conflev, Res
      integer*4 Status
c
c  Local variables
c
      REAL*4 POISS , ame , alim , cl1, ok
      REAL*4 xmax , result
      REAL*4 arycon(3)
      INTEGER*4 mmme , imsol
      EXTERNAL FLIM
c
      cl1 = 1.0 - Conflev
      if ( cl1.eq.0. ) then
         call xwrite(' Confidence level exceeds precision of ullim', 10)
         status = -1
         return
      endif
      alim = (9.+2.*Cntsrc+3.*SQRT(9.+4.*Cntsrc)-2.*Cntbg)/2.
      xmax = 200. - Cntbg
      IF ( xmax.LE.0. ) THEN
         Res = alim
         RETURN
      ENDIF
      arycon(1) = Cntsrc
      arycon(2) = Cntbg
      arycon(3) = Conflev
      CALL ZFUN(FLIM,0.0,xmax,1.0,1.E-5,result,imsol,arycon)
      Res = result
      IF ( imsol.EQ.0 ) THEN
         ame = 200.
         mmme = Cntsrc
         ok = 1. - POISS(mmme,ame,1) + POISS(mmme,ame,2)
         IF ( ok.LT.cl1 .OR. alim.LE.0. ) THEN
            CALL XWRITE('** This source looks more like',10)
            CALL XWRITE(' a hole in the background',10)
	    ENDIF
      ENDIF
      IF ( imsol.GT.1 ) THEN
         WRITE (ZWRite,99001) imsol
         CALL XWRITE(ZWRite,10)
      ENDIF
      IF ( result.GT.0.0 ) THEN
         RETURN
      ELSE
         Res = alim
      ENDIF
      RETURN
99001 FORMAT (' **Warning more than one solution found**',I3)
      END
