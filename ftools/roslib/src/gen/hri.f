
*+HRI
	function hri(eps,R)

C ____________________________________________________________________________
C
C --- DESCRIPTION ---
C 
C HRI PSF approximation for off-axis data.
C The PSF is normalised to 1 count under the curve. 
C 
C --- AUTHORS/MODIFICATION HISTORY ---
C
C Larry David (Nov 1993)  1.0.0; Original model
C 
C Jane Turner (Dec 2 1993)1.0.1; Tested on 6 cal datasets. The tests show that
C                                model is sometimes a poor fit to radial
C                                profile data, partly due to variations in
C                                aspect quality.
C                                NOTE : It is advised NOT to use this for model
C                                deconvolution. Can be used to do crude extent
C                                tests, and determine extraction cell sizes.
C
      IMPLICIT NONE  
      character(5) version
      parameter (version='1.0.1')
C
C --- VARIABLES ---
C 
      REAL A1, A2, A3, EXP2,EXP3, S1,S2,S3, arg1,arg2
      REAL PSF, EXP1, EPS, PI, renorm
      real psf1, psf2, psf3, hri,R
      PI = 3.1415927      
C
C --- VARIABLE DIRECTORY ---
C
C EPS       real : off-axis angle from optical axis, in arcminutes
C R         real : radial distance from centroid  in arcseconds
C S1,S2,S3  real : in arcseconds
C A1,A2,A3  real : empirically determined constants.
C
*-
C __________________________________________________________________________

C
C Calc predicted profile as function of R where R is radial dist in arcsec
C Constants A1,A2,A3,S1,S2,S3 from HRi report then tweaked to fit off-axis 
C data by JT
C 
	A1=0.9638
	a2=0.1798
	a3=0.00090
	S1=2.1858
	S3=31.69
		s2=3.3+0.019*(EPS) - (0.016*(EPS**2)) + (0.0044*(EPS**3))
		  arg1=(R/S1)**2
		  EXP1=exp(-0.5*arg1)
		  arg2=(R/S2)**2
		  EXP2=exp(-0.5*arg2)
		  EXP3=exp(-R/(S3))
C
C Keep the number of photons under mirror Gaussian constant
C
		PSF1=(A1)*EXP1
		PSF2=(A2)*EXP2                                
		PSF3=A3*EXP3
C
C Renormalise to 1 
C IMG SEPT 1993
C
	renorm = 2*pi*(A1*(S1)**2 + A2*(S2)**2 + A3*(s3)**2)   
		PSF=(PSF1+PSF2+PSF3)/renorm
                hri = psf
      return
      END     

c ---------------------------------------------------------------------------
c     END OF HRI FUNCTION
c ---------------------------------------------------------------------------

