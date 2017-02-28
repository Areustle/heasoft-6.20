      FUNCTION HRIPSF(Eps,R)
C
C HRI PSF approximation for off-axis data
C December 2nd 1993 version
C Jane Turner from larrys model (Nov '93) tested on 6 cal
C datasets, using some of JT/IMG code from September 1993 version.
C tests show model is sometimes a poor fit to radial
C profile data, partly due to
C variations in aspect quality. NOT to be used for image
C deconvolution. Can be used to do crude extent tests, and
C determine extraction cell sizes.
C
C     EPS is off-axis anlgle from optical axis, in arcminutes
C     R is radial distance from centroid  in arcseconds
C     S1,S2 and S3 are in arcseconds, A1,A2,A3 are empirically determined
C     constants.
 
      IMPLICIT NONE
      REAL a1 , a2 , a3 , exp2 , exp3 , s1 , s2 , s3 , arg1 , arg2
      REAL HRIPSF , exp1 , Eps , pi , renorm
      REAL psf1 , psf2 , psf3 , R
      pi = 3.1415927
 
 
C      INTEGER*4      j
C
C Calc predicted profile as function of R where R is radial dist in arcsec
C Constants A1,A2,A3,S1,S2,S3 from HRi report then tweaked to fit off-axis
C data.
c JT
      a1 = 0.9638
      a2 = 0.1798
      a3 = 0.00090
      s1 = 2.1858
      s3 = 31.69
      s2 = 3.3 + 0.019*(Eps) - (0.016*(Eps**2)) + (0.0044*(Eps**3))
      arg1 = (R/s1)**2
      exp1 = EXP(-0.5*arg1)
      arg2 = (R/s2)**2
      exp2 = EXP(-0.5*arg2)
      exp3 = EXP(-R/(s3))
C Keep the number of photons under mirror Gaussian constant
      psf1 = (a1)*exp1
      psf2 = (a2)*exp2
      psf3 = a3*exp3
C Renormalise to 1
c IMG SEpt 1993
      renorm = 2*pi*(a1*(s1)**2+a2*(s2)**2+a3*(s3)**2)
      HRIPSF = (psf1+psf2+psf3)/renorm
      RETURN
      END
