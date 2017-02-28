      SUBROUTINE cumga(zz,q)
C      based on erfc which was provided by bill press 18 sept 1980
C      computes the cumulative probability distribution from zz to infinity
C      basic erfc claimed to have relative error less than 1.2e-7 for all zz>0.
c
      real*4 zz, q, two, srtwo, z, t, erfc
c
      two = 2.
      srtwo = 1.41421356
      z = zz/srtwo
      IF ( zz.LT.0. ) z = -z
      t = 1./(1.+0.5*z)
      erfc = t*exp(-z*z-1.26551223+
     &       t*(1.00002368+t*(0.37409196+t*(0.09678418+
     &       t*(-0.18628806+t*(0.27886807+
     &       t*(-1.13520398+t*(1.48851587+t*(-0.82215223+t*0.17087277)))
     &       ))))))
      q = erfc/two
      IF ( zz.LT.0. ) q = 1. - q
      RETURN
      END
