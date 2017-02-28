*+ONAX_DECORR
	subroutine onax_decorr(pha,pi,dx,dy,dx_inter,dy_inter,
     &                 errflg,chatter)
        IMPLICIT NONE
c $Id: onax_decorr.f,v 3.7 2013/05/21 19:08:39 irby Exp $
c --- DESCRIPTION -----------------------------------------------
c This routine undos the field distortion applied to US Rev 0
c data.
c --- AUTHORS/MODIFICATION HISTORY ------------------------------
c
c Dr Jane Turner (August 1995) 1.0.0; Original program
c Rehana Yusaf (Sept 1995) 1.0.1;     Changed it to a subroutine.
c                                     The calculations are now done
c                                     row by row, therefore arrays
c                                     are no longer used.
c                                     Minor cosmetic changes.
c Rehana Yusaf  (Oct 18 1995) 1.0.2;  update Diff to DIFF
c Joe Ftools    (July 1 1996) 1.0.3;  cosd->cos, sind->sin
      character(5) version
      parameter (version='1.0.3')
c ---------------------------------------------------------------
*-
c VARIABLES 
c
        real DELTX,DELTY, XOPT
	real DEG, DR, YOPT,  DR_inter, DPHI
	real DELTX_inter, DELTY_inter
	real DIFF1,DIFF2
	real  dx_inter, dy_inter
        integer errflg,chatter,dx,dy,pi,pha
        character(80) desc
	XOPT=4119.0
	YOPT=3929.0

c       USER INFO

        IF (chatter.GE.40) THEN
          desc = ' ... using onax_decorr Ver '//version
          call fcecho(desc)
        ENDIF

C	RADIANS TO DEGREES

	DEG=57.29578

C	Undo field distortion
C	calculate offsets from optical axis

	DELTX=dx-XOPT
	DELTY=dy-YOPT

c	This is an approximation as the following should be done on the inter 
c	position really

	IF (DELTX.EQ.0 .and. DELTY.EQ.0) then
          dx_inter=dx
          dy_inter=dy
	ELSE

C	compute OFF AXIS  angle

	  DR=SQRT(DELTX*DELTX+DELTY*DELTY)

c	field angle

	  DPHI=ATAN2(DELTY,DELTX)
c	  DPHI=ATAN2(DELTY,DELTX)*DEG
C	DEG IS CONVERSION SCALAR BETWEEN radians and degrees...perhaps

	  DIFF1=4.1305E-02*DR**0.63
	  DIFF2=4.1305E-2*(DR+DIFF1)**0.63
	  DR_inter=DR+DIFF2
          DELTX_inter=DR_inter*COS(DPHI)
          DELTY_inter=DR_inter*SIN(DPHI)
c         DELTX_inter=DR_inter*COSD(DPHI)
c         DELTY_inter=DR_inter*SIND(DPHI)

C	coordinates are now decorrected 

          dx_inter=DELTX_inter+XOPT
	  dy_inter=DELTY_inter+YOPT
        ENDIF
        return
	end
