*  FKNIF
*
*  Calculated "Kniffen" factors.
*  Empirical correction factors for low-energy bands.
*  CVM worked out a simple functional form for the factors.
*  PLN worked out a similar form for the uncertainties that must 
*   be added to the statistical errors.
*
* Inputs:
*   e1, e2:  Lower and upper bounds of energy band.
*
* Outputs:
*   factor:  Correction factor (.ge. 1)
*   funcert: Uncertainty in factor
*
*  PLN, Stanford, December 1994
*
*  @(#) fknif.f 1.4@(#)

      subroutine fknif(e1,e2,factor,funcert)
      implicit none	
      real e1,e2,factor,funcert
      real emid

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
