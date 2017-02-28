C EARTHP
C $Id: earthp.f,v 1.2 2013/05/21 19:08:24 irby Exp $
C         *************************************************************
C         EARTHP subroutine calculates the earth position as a function
C         of time for intervals less than one orbit.  It essentially
C         interpolates between a starting and ending location assuming
C         that the earth moves uniformly along a great circle.
C
C         This routine is used in the EGRET Exposure map generation
C         process to report the earth position in the calculation of the
C         earth shadow effects.
C
C         Written:  5/29/91.  D.L.Bertsch.
C
C         Calling Sequence:
C         -----------------
C
C            CALL EARTHP( NSTEP, NTOTL, COORDS, EPOS, QNEW, SLAT, CLAT,
C                         RLON )
C
C                VARIABLE     I/O    DESCRIPTION
C                --------     ---    -----------------
C                 NSTEP   I*2  I     Step number.  Range 1 to NTOTL
C                 NTOTL   I*2  I     Total number of steps
C                 COORDS  C*4  I     Coordinate system ('GALA' or
C                                    'CELA')
C                 EPOS(4) R*4  I     Earth starting and ending positions
C                                    in radians.
C                                      (1)  start Right Ascension
C                                      (2)  start Declination
C                                      (3)  end Right Ascension
C                                      (4)  end Declination
C                 QNEW    L*4  I     When TRUE, process new earth
C                                    starting and ending coordinates.
C                 SLAT    R*4  O     Sine of the latitude (or decl) at
C                                    NSTEP
C                 CLAT    R*4  O     Cosine of the latitude (or decl)
C                                    at NSTEP
C                 RLON    R*4  O     Earth longitude (or RA) in radians
C                                    at NSTEP.
C
C         Called by:  SHADOW
C         Calls:      CELGAL  to convert from celestial to galactic
C                             coordinates, if desired.
C	Modifications:
CH5     2.00    E.S.Panduranga 08/29/91
CH5                     Moved code from IBM to SUN.
CH5                     Stripped off line numbers and trailing blanks.
CH5			Declared all variables not declared on the IBM.
CH5
CH5 $Log: earthp.f,v $
CH5 Revision 1.2  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.1  2002/04/16 20:24:02  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
C        **************************************************************

      subroutine earthp( nstep, ntotl, coords, epos, qnew, slat, clat,
     &     rlon )


Cesp  ! declarting varaibles undeclared on IBM !
      integer	nstep, ntotl, irtn
      real	slat, clat, rlon, delra, sds, cds, sde, cde, gamma
      real	cgam, sbeta, beta, cbeta, delgam, czeta, szeta
      real epos(4)
      character(4) coords
      logical qnew

      character(80)	id

      save

      common	/id/id
      id = '$Id: earthp.f,v 1.2 2013/05/21 19:08:24 irby Exp $'

C     Initialization

      if (qnew) then
         qnew = .false.

         if (coords.eq.'GALA') then
            call celgal( 'CG',epos(1),epos(2), epos(1),epos(2), irtn)
            call celgal( 'CG',epos(3),epos(4), epos(3),epos(4), irtn)
         endif

         delra = epos(3) - epos(1)

         sds = sin( epos(2) )
         cds = cos( epos(2) )
         sde = sin( epos(4) )
         cde = cos( epos(4) )

         cgam = sds*sde + cds*cde*cos(delra)
         gamma = acos( cgam )
         delgam = gamma/ntotl

         czeta = ( sde-sds*cgam )/( cds*sin(gamma) )
         szeta =  cde*sin(delra)/sin(gamma)
      endif

C     End of initialization.
      beta = (nstep-0.5)*delgam
      sbeta = sin(beta)
      cbeta = cos(beta)
      slat = sds*cbeta + cds*sbeta*czeta
      clat = sqrt( 1.0-slat*slat )
      
      rlon = epos(1) + atan2( sbeta*szeta*cds, cbeta-sds*slat )

      return
      end
