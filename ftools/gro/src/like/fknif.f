C  SUBROUTINE FKNIF(e1,e2,factor,funcert)
C
c $Id: fknif.f,v 1.2 2013/05/21 19:08:25 irby Exp $
c
c========================================================
c
C  Calculated "Kniffen" factors.
C  Empirical correction factors for low-energy bands.
C  CVM worked out a simple functional form for the factors.
C  PLN worked out a similar form for the uncertainties that must 
C   be added to the statistical errors.
C
C Inputs:
C   e1, e2:  Lower and upper bounds of energy band.
C
C Outputs:
C   factor:  Correction factor (.ge. 1)
C   funcert: Uncertainty in factor
C
C  PLN, Stanford, December 1994
C
C================================================================
C
C $Log: fknif.f,v $
C Revision 1.2  2013/05/21 19:08:25  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 1.1  2002/04/16 20:27:30  irby
C New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.21  1996/07/31  19:01:37  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.20  1996/07/31  16:49:43  jae
c Subroutine Module for like V5.00
c
C
C

      subroutine fknif(e1,e2,factor,funcert)

      implicit none	
      real e1,e2,factor,funcert
      real emid
      character(80) id

      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      common /id/id

      id = '$Id: fknif.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='FKNIF'

      if(jae_fknif)write(*,'("In routine ",a)') LOC
c
c
      emid = .5*(e1+e2)
      if (emid.lt.100.) then
         factor = 9.97*exp(-0.0327*emid)
         if (factor.lt.1.) factor = 1.
         funcert = 3.41*exp(-0.0174*emid) - 1.
         if (funcert.lt.0.) funcert = 0.
      else
	 factor = 1.
	 funcert = 0.
      endif

      return
      end
