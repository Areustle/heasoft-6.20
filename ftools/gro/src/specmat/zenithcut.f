********************************************************************
* Energy-dependent zenith angle cutoff function.
* Based on "4 sigma" model agreed in EGRET team meeting
* of 25 June 1992
* Programmed by PLN, Jan 1993

*  @(#) zenithcut.f 1.2@(#)

      real function zenithcut(energy,zenoff,zenmax)
      real energy,zenoff,zenmax

      zenithcut = 110. - zenoff*sigma(energy)
      if (zenithcut.gt.zenmax) zenithcut = zenmax
      return
      end
