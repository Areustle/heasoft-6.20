*  FCLASSB
*
*  Empirical correction factors to make up for an old error in the
*  Class B energy algorithm.  Rather than rebuilding the data base,
*  just fudge the effective area for analysis that uses Class A+B+C
*  events.  Class A is not affected.
*
* Inputs:
*   e1, e2:  Lower and upper bounds of energy band.
*
* Outputs:
*   factor:  Correction factor (.ge. 1)
*
*  PLN, Stanford, February 1997
*
*  @(#) fclassb.f 1.2@(#)

      subroutine fclassb(e1,e2,factor)
      implicit none	
      real e1,e2,factor
      real x

      x = alog10(sqrt(e1*e2))
      factor = .93 + .04*x - .33 * exp(-((x-3.4)/.27)**2)

      return
      end
