
      FUNCTION sisfdg ( lower, hiher)

      DOUBLE PRECISION sisfdg, lower, hiher

c  Function to calculate the SIS fudge factor on the efficiency
c  Arguments :
c     lower     r      i: lower energy of range
c     hiher     r      i: upper energy of range
c     sisfdg    r      r: fudge factor

      DOUBLE PRECISION CRBIND, E0, SIGMA, EW
      PARAMETER (CRBIND=-2.09, E0=2.2, SIGMA=0.1, EW=0.038)
      DOUBLE PRECISION SQR2PI
      PARAMETER (SQR2PI=2.50662827463100050241d0)

      DOUBLE PRECISION norm, x, energy

      LOGICAL first

      SAVE first, norm

      DATA first /.true./

      IF ( first ) THEN
         first = .false.
         norm = EW * (E0**CRBIND) / SQR2PI / SIGMA
      ENDIF

      energy = (lower + hiher)/2
      x = ( energy - E0 ) / SIGMA
      sisfdg = 1 + norm * EXP(-x*x/2) / (energy**CRBIND)

      RETURN
      END
