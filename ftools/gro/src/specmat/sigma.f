******************************************************************
* Approximate width (in degrees) of the point spread function as a 
* function of photon energy.  Used in calculating acceptance cones and
* zenith angle cuts.  This functional form is due to John Mattox.
* Coded by P. Nolan, 1993.

*  @(#) sigma.f 1.2@(#)

      real function sigma(E)
      real E

      sigma = 5.85 * (E/100.) **(-0.534)
      return
      end
