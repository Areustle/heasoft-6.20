      SUBROUTINE gridmp(step_alpha, step_delta, status)
      IMPLICIT NONE
c
c  Calculate grid lines
c
c  I  step_alpha  (d)  RA/longitude step (degrees)
c  I  step_delta  (d)  Dec/latitude step (degrees)
c  O  status      (i)  Error flag (0=OK)
c
      real*8 step_alpha, step_delta
      integer status

      INCLUDE '../include/pi.inc'
      INCLUDE 'projec.inc'
c
c  Local variables
c
      REAL*8 precision
      PARAMETER (precision=1.0D-10)

      REAL*8 aradstep, dradstep, ra, dec

* local ---------------------------------------------------------------
      character(80) chain
c     CHARACTER c(24)*20

      REAL chsize
      INTEGER i, nstep, imax, imin
*
      chsize = 0.6
c
c  Translate from degrees to radians
c
      aradstep = step_alpha*pi/180.D0
      dradstep = step_delta*pi/180.D0
*
      IF ( ramin.LT.0.D0 ) THEN
         imin = -int(-ramin/aradstep) - 1
      ELSE
         imin = int(ramin/aradstep)
      END IF
      IF ( ramax.LT.0.D0 ) THEN
         imax = -int(-ramax/aradstep)
      ELSE
         imax = int(ramax/aradstep) + 1
      END IF

      DO i = imin, imax
         ra = dfloat(i)*aradstep
         IF ( ra.GT.ramin-precision .AND. ra.LT.ramax+precision ) THEN
            ra = max(ramin,min(ra,ramax))
c            write(*,*)' RA=',ra,ramin,ramax
            IF ( abs(ra-ramax).LT.precision ) THEN
               IF ( ramax-ramin.EQ.2.D0*pi ) THEN
                  IF ( p_type.EQ.p_gnomonic .OR. p_type.EQ.p_ortho )
     &                  GO TO 100
               END IF
            END IF
            CALL meridien(ra,status)
            IF ( status.ne.0 ) THEN
               IF ( i_system.EQ.2 ) THEN
                  WRITE (chain,99001) 'RA', ra*180.D0/pi
               ELSE IF ( i_system.EQ.1 ) THEN
                  WRITE (chain,99001) 'absolute X', ra*180.D0/pi
               ELSE
                  WRITE (chain,99001) 'Lii', ra*180.D0/pi
               END IF
               CALL xwrite(chain, 10)
               status = 0
            END IF
         END IF
      END DO
*
 100  CONTINUE
      nstep = int(180.D0/step_delta)
      nstep = nstep/2
      DO i = -nstep, nstep
         dec = dfloat(i)*dradstep
         IF ( dec.GT.decmin-precision .AND. dec.LT.decmax+precision )
     &         THEN
            dec = max(decmin,min(dec,decmax))
            CALL parallele(dec,status)
            IF ( status.ne.0 ) THEN
               IF ( i_system.EQ.1 ) THEN
                  WRITE (chain,99001) 'absolute Y', dec*180.D0/pi
               ELSE IF ( i_system.EQ.2 ) THEN
                  WRITE (chain,99001) 'Dec', dec*180.D0/pi
               ELSE
                  WRITE (chain,99001) 'Bii', dec*180.D0/pi
               END IF
               CALL xwrite(chain, 10)
               status = 0
            END IF
         END IF
c         ENDIF
      END DO
      RETURN
99001 FORMAT ('W-GRID,  Spline failed at ',A,1X,F12.6)
      END
