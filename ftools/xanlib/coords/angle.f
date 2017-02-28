**==ANGLE.spg  processed by SPAG 3.09I  at 18:30 on  6 Dec 1991
      SUBROUTINE ANGLE(Ra,Dec,Cenra,Cendec,Radius,Theta,First,Found)
c
cc  returns the angle theta between two positions on the sky
cc          and found is true if the angle is less than radius
c   uses direction cosines
*   nick 3.03.90
c
c	ra	trial ra
c	dec	trial dec
c	cenra	input ra
c	cendec	input dec
c	radius	cone radius
c	theta	angular separation
c	first	true if its the first call of angle
c	found	true if theta <radius
c
c	all angles are r*8
c
c Modified by James Peachey, HEASARC/GSFC/NASA, 22 May, 2000
c   Replaced call to dacosd with dacos, plus explicit conversion
c   to degrees after performing the trig. This is because dacosd is
c   not standard f77, and does not exist on all platforms.
c
 
* import export
      REAL*8 Ra , Dec , Cenra , Cendec , Radius , Theta
      LOGICAL*4 First , Found
* local
      REAL*8 cen_dircos(3) , dircos(3)
      REAL*8 Deg_per_rad
c Value for pi is from CRC Standard Mathematical Tables, 26th Ed., p. 5
c Retained 17 decimals for double precision
      parameter(Deg_per_rad = 180.0D0 / 3.14159 26535 89793 2 D0)
c
c calculate the things that need be done only once
c
      IF ( First ) THEN
         CALL DIR_COS(Cenra,Cendec,cen_dircos)
         First = .FALSE.
      ENDIF
c
c get theta using direction cosines
c
      CALL DIR_COS(Ra,Dec,dircos)
      Theta = dircos(1)*cen_dircos(1) + dircos(2)*cen_dircos(2)
     &        + dircos(3)*cen_dircos(3)
c
      IF ( Theta.GE.1.0D00 ) THEN
         Theta = 0.0
      ELSE
         Theta = DACOS(Theta) * Deg_per_rad
      ENDIF
c
      IF ( Theta.LE.Radius ) THEN
         Found = .TRUE.
      ELSE
         Found = .FALSE.
      ENDIF
      RETURN
      END
