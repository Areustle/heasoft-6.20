      subroutine galac(step_alpha, step_delta, status)
      implicit none
c
c  Calculate a lii/bii grid onto a ra-dec plot or vice-versa
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
      REAL*8 angal, xpole, ypole, gxpole, gypole, dpole
      REAL*8 a1, d1, raold, decold, angold, lever_x
      INTEGER typold
      LOGICAL north_up

c  Initialize to avoid warning
      north_up = .FALSE.
c  --
      status = 0
*
* check if i know what to do
      IF ( i_system.EQ.1 ) THEN
         CALL xwrite('E-GRID,  System is UNKNOWN', 10)
         status = -1
         RETURN
      END IF
*
* check if i know how to do it
      IF ( p_type.EQ.p_aitoff .OR. p_type.EQ.p_radio .OR. 
     &     p_type.EQ.p_lambert ) THEN
         CALL xwrite('W-GRID,  Galactic-Equatorial not '//
     &               'supported in this projection', 10)
         status = -1
         RETURN
      END IF
 
 
* change the projection center to appropriate one in alternate system
      IF ( i_system.EQ.2 ) THEN

* galactic coordinates of projection center
         CALL equ_gal(a0,d0,a1,d1,1)

* find which pole is up
         IF ( p_type.EQ.p_gnomonic .OR. p_type.EQ.p_ortho ) THEN
            north_up = lever_x(pi*0.5D0,d1).GT.0.D0
         ELSE
            north_up = .TRUE.
         END IF

* equatorial coordinates of galactic pole
         IF ( north_up ) THEN
            CALL gal_equ(0.D0,pi*0.5D0,gxpole,gypole,1)
         ELSE
            CALL gal_equ(0.D0,-pi*0.5D0,gxpole,gypole,1)
         END IF

      ELSE IF ( i_system.EQ.3 ) THEN

* equatorial coordinates of projection center
         CALL gal_equ(a0,d0,a1,d1,1)

* find which pole is up
         IF ( p_type.EQ.p_gnomonic .OR. p_type.EQ.p_ortho ) THEN
            north_up = lever_x(pi*0.5D0,d1).GT.0.D0
         ELSE
            north_up = .TRUE.
         END IF

* galactic coordinates of pole
         IF ( north_up ) THEN
            CALL equ_gal(0.D0,pi*0.5D0,gxpole,gypole,1)
         ELSE
            CALL equ_gal(0.D0,-pi*0.5D0,gxpole,gypole,1)
         END IF

      END IF
 
* relative coordinates of alternate pole
      CALL abs_to_rel(gxpole,gypole,xpole,ypole,1)
      dpole = sqrt(xpole**2+ypole**2)
      IF ( dpole.GE.precision ) THEN
         angal = atan2(xpole,ypole)
         IF ( .NOT.north_up ) angal = pi + angal
      ELSE
         angal = 0.D0
      END IF
*
      raold = a0
      decold = d0
      angold = pangle
      typold = p_type
      i_system = 5 - i_system      
      CALL setpro(a1,d1,angal,p_type)
      CALL gridmp(step_alpha, step_delta, status)
      i_system = 5 - i_system
      CALL setpro(raold,decold,angold,typold)
*
      END
