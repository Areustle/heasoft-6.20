      SUBROUTINE parallele(dec,status)
      IMPLICIT NONE
C----------------------------------------------------------------------
C map	internal routine
C	draw a parallel
C arguments :
C	dec	r*8	declination			input
C	status	i       Error flag (0=OK)               input
C----------------------------------------------------------------------
      REAL*8 dec
      INTEGER status

      INCLUDE 'gridlab.inc'
      INCLUDE '../include/pi.inc'
      INCLUDE 'greg.inc'
      INCLUDE 'projec.inc'
c
c  Local variables
c
      INTEGER nstep
      PARAMETER (nstep=21)
      REAL*8 precision
      PARAMETER (precision=1.0D-10)
      REAL*8 x(nstep), y(nstep), z, d(nstep), a(nstep), lever_x, h, 
     &       sp, cp
      INTEGER i
      character(20) variable

      status = 0
c
c  gridlab settings (works in degrees)
c
      axisnum = 2
      labval = dec*180.d0/PI
*
      IF ( p_type+1.EQ.1 ) THEN
*
* unprojected data
         CALL relocate(gux1,dec)
         CALL draw(gux2,dec)
         RETURN
      ELSE IF ( p_type+1.EQ.2 ) THEN
*
* gnomonic
         variable = 'POLYGONAL_LENGTH'
         GO TO 100
      ELSE IF ( p_type+1.EQ.3 ) THEN
*
* orthographic
         variable = 'NUMBERING'
         GO TO 100
      ELSE IF ( p_type+1.EQ.4 ) THEN
      ELSE IF ( p_type+1.EQ.5 ) THEN
*
* stereographic
         IF ( dec.EQ.pi*0.5D0 ) THEN
            sp = sin(pangle)*npole
            cp = cos(pangle)*npole
            CALL relocate(sp,cp)
            CALL draw(sp,cp)
         ELSE IF ( dec.EQ.-pi*0.5D0 ) THEN
            sp = sin(pangle)*spole
            cp = cos(pangle)*spole
            CALL relocate(sp,cp)
            CALL draw(sp,cp)
         ELSE
            variable = 'POLYGONAL_LENGTH'
            IF ( abs(dec+d0).LE.precision ) THEN
               h = min(max((a0-ramin),(ramax-a0)),pi-0.01D0)
               variable = 'NUMBERING'
            ELSE
               h = min(max((a0-ramin),(ramax-a0)),pi)
            END IF
            IF ( h.NE.pi ) THEN
               DO i = 1, nstep
                  a(i) = a0 + (2*i-nstep-1)*h/(nstep-1)
                  d(i) = dec
               END DO
               CALL abs_to_rel(a,d,x,y,nstep)
               CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',variable,
     &                     .FALSE.,status)
            ELSE
               DO i = 1, nstep - 1
                  a(i) = a0 + (2*i-nstep-1)*h/(nstep-1)
                  d(i) = dec
               END DO
               CALL abs_to_rel(a,d,x,y,nstep-1)
               x(nstep) = x(1)
               y(nstep) = y(1)
               CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',
     &                     'POLYGONAL_LENGTH',.TRUE.,status)
            END IF
         END IF
         RETURN
      ELSE IF ( p_type+1.EQ.6 ) THEN
      ELSE IF ( p_type+1.EQ.7 ) THEN
*
         variable = 'NUMBERING'
         h = min(max((a0-ramin),(ramax-a0)),pi)
         DO i = 1, nstep
            a(i) = a0 + (2*i-nstep-1)*h/(nstep-1)
            d(i) = dec
         END DO
         CALL abs_to_rel(a,d,x,y,nstep)
         CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',variable,.FALSE.,
     &               status)
         RETURN
      ELSE IF ( p_type+1.EQ.8 ) THEN
*
         CALL abs_to_rel(a0,dec,x,y,1)
         h = pi*cos(dec)
         CALL relocate(x(1)-h,y(1))
         CALL draw(x(1)+h,y(1))
         RETURN
      ELSE
         RETURN
      END IF
*
* azimuthal and lambert
      IF ( dec.EQ.pi*0.5D0 ) THEN
         sp = sin(pangle)*npole
         cp = cos(pangle)*npole
         CALL relocate(sp,cp)
         CALL draw(sp,cp)
      ELSE IF ( dec.EQ.-pi*0.5D0 ) THEN
         sp = sin(pangle)*spole
         cp = cos(pangle)*spole
         CALL relocate(sp,cp)
         CALL draw(sp,cp)
      ELSE
         variable = 'POLYGONAL_LENGTH'
         IF ( abs(dec+d0).LE.precision ) THEN
            h = min(max((a0-ramin),(ramax-a0)),pi-1.D1*precision)
         ELSE
            h = min(max((a0-ramin),(ramax-a0)),pi)
         END IF
         IF ( h.NE.pi ) THEN
            DO i = 1, nstep
               a(i) = a0 + (2*i-nstep-1)*h/(nstep-1)
               d(i) = dec
            END DO
            CALL abs_to_rel(a,d,x,y,nstep)
            CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',variable,
     &                  .FALSE.,status)
         ELSE
            DO i = 1, nstep - 1
               a(i) = a0 + (2*i-nstep-1)*h/(nstep-1)
               d(i) = dec
            END DO
            CALL abs_to_rel(a,d,x,y,nstep-1)
            x(nstep) = x(1)
            y(nstep) = y(1)
            CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',
     &                  'POLYGONAL_LENGTH',.TRUE.,status)
         END IF
      END IF
      RETURN
*
* determine up time
 100  CONTINUE
      IF ( abs(dec).GT.0.5*pi ) RETURN
      IF ( 0.5*pi-abs(dec).LT.precision ) THEN
         h = lever_x(dec,d0)
         IF ( h.LT.0.D0 ) RETURN
         CALL abs_to_rel(a0,dec,x,y,1)
         CALL relocate(x(1),y(1))
         CALL draw(x(1),y(1))
         RETURN
      END IF
      h = lever_x(dec,d0)
c         write(*,*) 'h dec d0',dec,d0,h
      IF ( h.LT.0.D0 ) THEN
         RETURN
      ELSE IF ( h.LT.precision ) THEN
c         write(*,*) 'h.LT.precision '
         CALL abs_to_rel(a0,dec,x,y,1)
         CALL relocate(x(1),y(1))
         CALL draw(x(1),y(1))
      ELSE IF ( h.GT.pi+precision .AND. ipole.NE.0 ) THEN
c         write(*,*)'h.GT.pi+precision'
         DO i = 1, nstep - 1
            a(i) = a0 + (2*i-nstep-1)*pi/(nstep-1)
            d(i) = dec
         END DO
         CALL abs_to_rel(a,d,x,y,nstep-1)
c         write(*,*)'  3: ',x(1),y(1)
         x(nstep) = x(1)
         y(nstep) = y(1)
         CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',variable,.TRUE.,
     &               status)
      ELSE IF ( p_type.EQ.p_gnomonic ) THEN
c         write(*,*)'p-type'
         h = min(max((a0-ramin),(ramax-a0)),0.8D0*h)
         DO i = 1, nstep
            a(i) = a0 + (2*i-nstep-1)*h/(nstep-1)
            d(i) = dec
         END DO
c         write(*,*)'p_type.eq.p_gnomonic'
         CALL abs_to_rel(a,d,x,y,nstep)
c         type *,'  4: ',sngl(d(1)),sngl(x(1)),sngl(y(1)),nstep
         CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',variable,.FALSE.,
     &               status)
      ELSE
         h = min(max((a0-ramin),(ramax-a0)),h)
         DO i = 1, nstep
            a(i) = a0 + (2*i-nstep-1)*h/(nstep-1)
            d(i) = dec
         END DO
         CALL abs_to_rel(a,d,x,y,nstep)
         CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',variable,.FALSE.,
     &               status)
      END IF
      RETURN
      END
