      SUBROUTINE meridien(ra,status)
      IMPLICIT NONE
c
c  Plot meridian at a given ra
c
c   I  ra       (d)  Right ascension of meridian
c   O  status   (i)  Error flag (0=OK)
c
      real*8 ra
      integer status

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
      PARAMETER (precision=1.0D-7)
      REAL*8 x(nstep), y(nstep), z, d(nstep), a(nstep), delta1, y1, 
     &       y2, slope, ypole, sp, tp, cp, xpole, corr
      INTEGER i

      status = 0
      corr = 0.00001 
c
c  gridlab settings (works in degrees)
c
      axisnum = 1
      labval = ra*180.d0/PI
*
      IF ( p_type+1.EQ.1 ) THEN
* no projection. straight line from (ra,decmin) to (ra,decmax)
         CALL relocate(ra,guy1)
         CALL draw(ra,guy2)
         RETURN
      ELSE IF ( p_type+1.EQ.2 ) THEN
*
* gnomonic : straight line from (ra,0) to pole
         ypole = min(spole,npole)
c      type '(1x,a,f7.2)','pangle =',sngl(pangle*180.0/pi)
         IF ( pi*0.5D0-precision .LE. 
     &                    max(abs(decmin),abs(decmax))) THEN
c            CALL abs_to_rel(ra,1.D0,x,y,1)
c             type *,'  1: ',ra,x(1),y(1)
c            slope = (y(1)-ypole)/x(1)
c            y1 = (gux1-x(1))*slope + y(1)
c            y2 = gux2*slope + ypole
c            type *,sngl(gux1),sngl(y1),sngl(gux2),sngl(y2)
            if(d0.lt.0)then
             CALL abs_to_rel(ra,-pi*0.5D0,xpole,ypole,1)
             CALL abs_to_rel(ra,decmax,x,y,1)
            else
             CALL abs_to_rel(ra,pi*0.5D0,xpole,ypole,1)
             CALL abs_to_rel(ra,decmin,x,y,1)
            endif
            CALL relocate(xpole,ypole)
            CALL draw(x(1),y(1))
         ELSE IF ( abs(pangle).GT.precision ) THEN
c            type *,' 2: ',pangle, ra,decmin,decmax
            CALL abs_to_rel(ra,pi*0.25D0,x,y,1)
            CALL abs_to_rel(ra,pi*0.45D0,x(2),y(2),1)
            IF ( x(2).NE.x(1) ) THEN
               slope = (y(2)-y(1))/(x(2)-x(1))
               y1 = (gux1-x(1))*slope + y(1)
               y2 = (gux2-x(1))*slope + y(1)
               CALL relocate(gux1,y1)
               CALL draw(gux2,y2)
            ELSE
               CALL relocate(x(1),guy1)
               CALL draw(x(1),guy2)
            END IF
         ELSE IF ( abs(ra-a0).LT.precision ) THEN
c            type *,' 3: ',ra,a0
            CALL relocate(0.D0,guy1)
            CALL draw(0.D0,guy2)
c         ELSE IF ( pi*0.5D0-abs(d0).GT.precision ) THEN
         ELSE IF ( abs(d0).GT.precision ) THEN
            CALL abs_to_rel(ra,0.0D0,x,y,1)
c            type *,'  4: ',y(1),xpole,ypole,x(1)
c
c            write(*,*)'gux1=',gux1,' gux2=',gux2
c            write(*,*)
c     &           'ra=',ra,' x(1)=',x(1),' y(1)=',y(1),' ypole=',ypole
            if ( x(1).eq.0.d0 ) then
c              Set to "infinite" slope
               if ( y(1)-ypole .lt. 0 ) then
                  slope=-1.d10
               else
                  slope=1.d10
               endif
            else
               slope = (y(1)-ypole)/x(1)
            endif
            y1 = (gux1-x(1))*slope + y(1)
            y2 = gux2*slope + ypole
            CALL relocate(gux1,y1)
            CALL draw(gux2,y2)
         ELSE
            CALL abs_to_rel(ra,0.D0,x,y,1)
c            type *,'  5: ',ra,x(1),y(1),guy1,guy2
            CALL relocate(x(1),guy1)
            CALL draw(x(1),guy2)
         END IF
         RETURN
       ELSE IF ( p_type+1.EQ.3 ) THEN
*
* orthographic : in general these are conics
         IF ( abs(ra-a0).LE.precision ) THEN
            sp = sin(pangle)
            cp = cos(pangle)
            IF ( d0.GE.0.D0 ) THEN
               CALL relocate(-sp,-cp)
               CALL draw(npole*sp,npole*cp)
            ELSE
               CALL relocate(-sp,-cp)
               CALL draw(spole*sp,spole*cp)
            END IF
         ELSE IF ( abs(abs(ra-a0)-pi).LE.precision ) THEN
            sp = sin(pangle)
            cp = cos(pangle)
            IF ( d0.GE.0.D0 ) THEN
               CALL relocate(sp,cp)
               CALL draw(npole*sp,npole*cp)
            ELSE
               CALL relocate(sp,cp)
               CALL draw(spole*sp,spole*cp)
            END IF
         ELSE
*
* else go from delta1 to pi/2 or from pi/2 to -delta1, depending
* on whether the north or south poles are visible.
*
            IF ( d0.GE.0.D0 ) THEN
               delta1 = atan(cos(ra-a0)*tan(d0-pi*0.5D0))
               DO i = 1, nstep
                  d(i) = delta1 + (pi*0.5-delta1)/(nstep-1)*(i-1)
                  a(i) = ra
               END DO
               CALL abs_to_rel(a,d,x,y,nstep)
               CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',
     &                     'NUMBERING',.FALSE.,status)
            ELSE
               delta1 = atan(cos(ra-a0)*tan(pi*0.5D0+d0))
               DO i = 1, nstep
                  d(i) = delta1 - (pi*0.5+delta1)/(nstep-1)*(i-1)
                  a(i) = ra
               END DO
               CALL abs_to_rel(a,d,x,y,nstep)
               CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',
     &                     'NUMBERING',.FALSE.,status)
            END IF
            RETURN
         END IF
         RETURN
      ELSE IF ( p_type+1.EQ.4 ) THEN
*
* azimuthal (schmidt)
         IF ( abs(ra-a0).LE.precision ) THEN
            sp = pi*sin(pangle)
            cp = pi*cos(pangle)
            CALL relocate(-sp,-cp)
            CALL draw(sp,cp)
         ELSE
            DO i = 1, nstep
               d(i) = (2*i-nstep-1)*(pi*0.5D0-precision)/(nstep-1)
               a(i) = ra
            END DO
            CALL abs_to_rel(a,d,x,y,nstep)
            CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',
     &                  'POLYGONAL_LENGTH',.FALSE.,status)
         END IF
         RETURN
      ELSE IF ( p_type+1.EQ.5 ) THEN
*
* stereo
         IF ( abs(ra-a0).LE.precision ) THEN
            tp = tan(pangle)
            CALL relocate(guy1*tp,guy1)
            CALL draw(guy2*tp,guy2)
         ELSE IF ( pi-abs(ra-a0).LE.precision ) THEN
         ELSE IF ( abs(0.5*pi-abs(d0)).GE.precision ) THEN
            DO i = 1, nstep
               d(i) = (2*i-nstep-1)*(pi*0.5D0-precision)/(nstep-1)
               a(i) = ra
            END DO
            CALL abs_to_rel(a,d,x,y,nstep)
            CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE','NUMBERING',
     &                  .FALSE.,status)
         ELSE
            tp = 2*max(abs(gux1),abs(gux2),abs(guy1),abs(guy2))
            cp = tp*cos(pangle+ra-a0)
            sp = tp*sin(pangle+ra-a0)
            CALL relocate(0.0D0,0.0D0)
            CALL draw(sp,cp)
         END IF
         RETURN
      ELSE IF ( p_type+1.EQ.6 ) THEN
*
* lambert
         IF ( abs(ra-a0).LE.precision ) THEN
            sp = 2.0*sin(pangle)
            cp = 2.0*cos(pangle)
            CALL relocate(-sp,-cp)
            CALL draw(sp,cp)
         ELSE
            DO i = 1, nstep
               d(i) = (2*i-nstep-1)*(pi*0.5D0-precision)/(nstep-1)
               a(i) = ra
            END DO
            CALL abs_to_rel(a,d,x,y,nstep)
            CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE',
     &                  'POLYGONAL_LENGTH',.FALSE.,status)
         END IF
         RETURN
      ELSE IF ( p_type+1.EQ.7 ) THEN
      ELSE IF ( p_type+1.NE.8 ) THEN
         RETURN
      END IF
*
* aitoff equal area : all sky is visible
* stupid radio projection : in principle also...
      IF ( abs(ra-a0).LT.precision ) THEN
         sp = sin(pangle)
         cp = cos(pangle)
         CALL relocate(spole*sp,spole*cp)
         CALL draw(npole*sp,npole*cp)
      ELSE
         DO i = 1, nstep
            d(i) = (pi*0.5D0-precision)/(nstep-1)*(2*i-nstep-1)
            a(i) = ra
         END DO
         CALL abs_to_rel(a,d,x,y,nstep)
         CALL plcurv(nstep,x,y,z,accurd,'CUBIC_SPLINE','NUMBERING',
     &               .FALSE.,status)
      END IF
      RETURN
      END
