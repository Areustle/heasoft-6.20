C..
      SUBROUTINE c6mekl(ear,ne,param,ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(10), photar(ne), photer(ne)

c
c XSPEC model subroutine to calculate Differential Emission
c Measure of the form:
c   Q(T) = Norm*(w(T)), where w(T) = Sum over 6 orders of Chebyshev
c                                        polynomials with 6 coeffs. 
c See, for example, Lemen et al. ApJ 341, 474 (1989), 
c The DEM is not constrained to be positive here, as the exponential 
c of the Chebyshev Polynomial is not being used as in Lemen et al. 
c Parameters:
c    param(1) = coeff a1 of Chebyshev polynomial order 1 
c    param(2) = coeff a2 of Chebyshev polynomial order 2 
c    param(3) = coeff a3 of Chebyshev polynomial order 3 
c    param(4) = coeff a4 of Chebyshev polynomial order 4 
c    param(5) = coeff a5 of Chebyshev polynomial order 5 
c    param(6) = coeff a6 of Chebyshev polynomial order 6 
c    param(7) = nH (cm^-3)  Fixed at 1 for most applications
c    param(8) = metallicity used in the Mewe-Kaastra plasma model
c    param(9) = redshift used in the Mewe-Kaastra plasma model
c    param(10) = switch(0=calculate MEKAL model, 1=interpolate MEKAL model)
c
c K. P. Singh    March 23, 1995 
c                June 1, 1995 Changed for XSPEC 8.70
c                Jan 13, 1996 CP6 defined properly within the limits
c                             of orthogonality and Changed for XSPEC 9.0
c Disclaimer: Any resemblance to a real program is purely
c             coincidental
c
c
c Declare variables:
c

      REAL pparam(23)
 
      INTEGER i

      DO i = 1, 7
         pparam(i) = param(i)
      ENDDO
      pparam(8) = 1.
      DO i = 9, 21
         pparam(i) = param(8)
      ENDDO
      pparam(22) = param(9)
      pparam(23) = param(10)

      CALL c6vmekl(ear, ne, pparam, ifl, photar, photer)

      return
      end 

