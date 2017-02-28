 
      FUNCTION lum(flux,z,alpha,q0,h0)
c      returns luminosity given flux, redshift and (energy) spectral
c      index (a power law spectrum is assumed)
c      luminosity is in units of 1.e40
      REAL*4 luminosity, norm_constant, lum, q0, h0
      real*4 z, alpha, dl, fourpi, flux
      REAL*8 dell, alph, zz, k_correction, e1, e2
      DATA fourpi, norm_constant, e1, e2/12.5664, 9.4864E8, .3, 3.5/
      zz = z
      alph = alpha
      dl = dell(zz,q0,h0)
      CALL kcorr(zz,1,alph,1.,e1,e2,1.0,k_correction)
      luminosity = norm_constant*fourpi*flux*dl*dl*k_correction
      lum = luminosity
      RETURN
      END
