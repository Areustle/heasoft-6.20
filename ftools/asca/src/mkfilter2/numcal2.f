C     fortran subroutine for "sisAdSet"  coded    by T.Dotani
C                           1993.12.20   modified by C.Otani (numcal2)
C
      SUBROUTINE numcal2( sat, sis, erad, dang)
      DOUBLE PRECISION sat(3), sis(3), erad, dang
      DOUBLE PRECISION xguess, x, bound, step, fact
      DATA xguess/0.0/, x/0.0/, bound/0.0/, step/0.0/
      DOUBLE PRECISION f,  pie
      DATA  pie/3.1415926535897932385/
      DOUBLE PRECISION satw(3), sisw(3), eradw
      EXTERNAL f
      COMMON /fcom/satw,sisw,eradw

      DO 10 I=1,3
         satw(I) = sat(I)
         sisw(I) = sis(I)
 10   CONTINUE
      eradw = erad
C                                     Check input arguments
C      IF( ABS(sat(2)).GE.1.0E-6) THEN
C         WRITE(*,*) 'SAT IS NOT ON THE XZ-PLANE; call dotani'
C      ENDIF
C                    Calculate boundary
C      IF( ABS(sat(1)).LT.erad) THEN
C         WRITE(*,*) 'SAT(1) IS LESS THAN ERAD; call dotani'
C         WRITE(*,*) 'SAT(1)=',sat(1),' ERAD=',erad
C         RETURN
C      ENDIF
      bound = ACOS(erad/sat(1))
C      IF ( bound.LT.0 )   bound = -bound
      xguess = 0.0
      step = bound/100.0
      CALL DUVMIF2( f, xguess, step, bound, 1.0D-4, 100, x)
C                                             limit x within the bound
C      IF( x-xguess.GT.bound) THEN
C         x = xguess + bound
C      ELSE IF( x-xguess.LT. -bound) THEN
C         x = xguess - bound
C      ENDIF
C                                              estimate ang. distance
      fact = SQRT( sis(1)**2 + sis(2)**2 + sis(3)**2 )
      dang = ACOS(-f(x)/fact)
      RETURN
      END

C     << f >>
C     this function returns the inner product of sis and the direction
C     from the satellite to the boundary of the day earth, but the signe
C     is changed.
C     x : angle which define a point on Earth, (R*cos(x), R*sin(x), 0)
C
      DOUBLE PRECISION FUNCTION f(x)
      DOUBLE PRECISION sat(3), sis(3), erad, x
      DOUBLE PRECISION nvect(3), fact
      COMMON /fcom/sat,sis,erad
      nvect(1) = erad*COS(x) - sat(1)
      nvect(2) = erad*SIN(x)
      nvect(3) = -sat(3)
      fact = SQRT(nvect(1)**2 + nvect(2)**2 + nvect(3)**2)
      DO 10 I=1,3
         nvect(I) = nvect(I)/fact
 10   CONTINUE
      f = -(sis(1)*nvect(1) + sis(2)*nvect(2) + sis(3)*nvect(3))
      RETURN
      END
