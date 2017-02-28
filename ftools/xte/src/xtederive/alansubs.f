	Subroutine XYZ_RADEC(X,Y,Z,RA_deg,DEC_deg)
c--------------------------------------------------------------------
c
c Converts between Cartesian and Celestial coordinates.
C
C	Input: X,Y,Z are Cartesian components in a right-angled 
c	system, see P. 2-11 of the "Mathematical Background
c	for Flight Dynamics Support of the BBXRT mission"
c
c	Output: RA, Dec in degrees. 
c				Alan Smale, May 1990
c
c--------------------------------------------------------------------

	Implicit none

	Real*8 X,Y,Z,RA,DEC,RA_deg,DEC_deg,XP,YP,ZP,Pi,Twopi

	Data Pi/3.1415926535D0/
	Twopi=2.0D0*Pi

c IF X,Y,Z ARE NOT A UNIT VECTOR, MAKE THEM ONE. nP BECOME THE UNIT
c VECTOR, WITH X,Y,Z REMAINING UNCHANGED.

	call UNITIZE(X,Y,Z,XP,YP,ZP)

c DERIVE RA AND DEC IN RADIANS.

	if(dabs(XP) .lt. 1.0D-20)XP=1.0D-20
	if(dabs(YP) .lt. 1.0D-20)then
	   RA = 0.D0
	else
	   RA = datan(YP/XP)
	endif
	DEC = dasin(ZP)

	if(RA .lt. 0.0D0)RA = RA + Twopi

c TURN INTO DEGREES

	RA_deg  = RA * 5.729577951D+01
	Dec_deg = Dec * 5.729577951D+01

c GET THE QUADRANT RIGHT

	if( X.lt.0.0D0)then
		if( Y .lt.0.0D0)then
			RA_deg = RA_deg + 180.0D0
		else
			RA_deg = RA_deg - 180.0D0
		endif
	endif
c END
	Return
	End

	subroutine angsep(RA1_deg,Dec1_deg,RA2_deg,Dec2_deg,Angle)
c--------------------------------------------------------------------------
c
c Finds the great-circle angular separation between two celestial positions
c
C	Input: (RA, DEC)_1 and (RA, DEC)_2 in degrees (DOUBLE PREC)
c	Output: Angle in degrees (DOUBLE PREC)
c
c				Alan Smale, May 1990
c
c--------------------------------------------------------------------------

	implicit none

	Real*8 RA1_deg, Dec1_deg, RA2_deg, Dec2_deg
	Real*8 RA1_rad, Dec1_rad, RA2_rad, Dec2_rad
	Real*8 Angle, Angle_rad
	Real*8 A,B,C
	Real*8 Pi, Twopi, Halfpi, dtr

	Data Pi/3.1415926535D0/

	Twopi = Pi * 2.0D+00
	Halfpi = Pi/2.0D+00
	dtr = 180.D+00/Pi

c TURN THE INPUT DEGREES INTO RADIANS.

	RA1_rad  = RA1_deg/dtr
	RA2_rad  = RA2_deg/dtr
	Dec1_rad = Dec1_deg/dtr
	Dec2_rad = Dec2_deg/dtr

c DO THE CALCULATION

	A = Halfpi - Dec1_rad
	B = Halfpi - Dec2_rad
	C = RA1_rad - RA2_rad

	Angle_rad = dcos(A)*dcos(B) + dsin(A)*dsin(B)*dcos(C)
	Angle_rad = dacos( Angle_rad)

c TURN BACK INTO DEGREES

	Angle = Angle_rad*dtr

	return
	end


        Subroutine UNITIZE(X,Y,Z,XP,YP,ZP)
 
        Implicit none
 
        Real*8 X,Y,Z,XP,YP,ZP,T
 
        T = DSqrt( X*X + Y*Y + Z*Z )
 
        XP = X/T
        YP = Y/T
        ZP = Z/T
 
        Return
        End
 

