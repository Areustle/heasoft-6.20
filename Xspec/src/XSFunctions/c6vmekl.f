
      SUBROUTINE c6vmekl(ear,ne,param,ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne),param(*),photar(ne), photer(ne)

c
c XSPEC model subroutine to calculate Differential Emission
c Measure of the form:
c   Q(T) = Norm*(w(T)), where w(T) = Sum over 6 orders of Chebyshev
c                                        polynomials with 6 coeffs. 
c
C CAUTION : The DEM here is not constrained to be positive throught,
C as is the case in ``cp6vmkl'' where the exponential of the sum of the 
C polynomial is taken and which follows Lemen et al. ApJ 341, 474 (1989), 
c 
c Parameters:
c    param(1) = coeff a1 of Chebyshev polynomial order 1 
c    param(2) = coeff a2 of Chebyshev polynomial order 2 
c    param(3) = coeff a3 of Chebyshev polynomial order 3 
c    param(4) = coeff a4 of Chebyshev polynomial order 4 
c    param(5) = coeff a5 of Chebyshev polynomial order 5 
c    param(6) = coeff a6 of Chebyshev polynomial order 6 
c    param(7) = nH (cm^-3)  Fixed at 1 for most applications
c    param(8) = He abundance
c    param(9) = C   "
c    param(10) = N   "
c    param(11) = O   "
c    param(12) = Ne  "
c    param(13) = Na  "
c    param(14)= Mg  "
c    param(15)= Al  "
c    param(16)= Si  "
c    param(17)= S   "
c    param(18)= Ar  "
c    param(19)= Ca  "
c    param(20)= Fe  "
c    param(21)= Ni  "
c    param(22) = redshift used in the Mewe-Kaastra plasma model
c    param(23) = switch(0=calculate MEKAL model, 1=interpolate MEKAL model)
c
c K. P. Singh    September 15, 1995 
c Disclaimer: Any resemblance to a real program is purely
c             coincidental
c
c
c Declare variables:

      INTEGER NTEMP
      PARAMETER(NTEMP=25)

      REAL tarr(NTEMP), dem(NTEMP)
      REAL a1,a2,a3,a4,a5,a6,logtemp,k
      REAL X,P1, P2, P3, P4, P5, P6, Q, temp

      INTEGER switch, i, status

c Initialize values:

      k=8.6171e-8
      a1=param(1)
      a2=param(2)
      a3=param(3)
      a4=param(4)
      a5=param(5)
      a6=param(6)

c Integrate contributions in form:
c    f = (sum over j) Q * Fj * logdeltT
c where
c   Q= (sum over k) ak(Pk)
c where Pk is Chebyshev polynomial of order k,
c and Fj is F(Tj) from meka for diff. energies or wavelengths
c
c it is important to do sum over j in uniform Log(T) steps.
c
c Note: Due to the stupidity of FORTRAN and problems with variable
c array declarations I am shuffling the definitions of Fj and photar
c temporarily for ease.
c
c Note: Doing the stepping from logT=5.5 to 8.0 in 0.1 steps.
c
c       tarr is in units of keV
c       temp is the temperature scaled with a constant=1E-6
c

      DO i = 1, NTEMP

         logtemp = 5.4 + 0.1*i
         tarr(i) = (10**logtemp) * k

         temp  = (1E-6)*(10**logtemp)
         X = ((logtemp - 5.5)*0.8 - 1.0)
         P1 = X
         P2 = 2*X**2 - 1
         P3 = 2*X*P2 - P1
         P4 = 2*X*P3 - P2
         P5 = 2*X*P4 - P3
         P6 = 2*X*P5 - P4
         Q  = a1*P1 +a2*P2 +a3*P3 +a4*P4 +a5*P5 +a6*P6

         dem(i) = Q * temp * 0.1

      ENDDO

      switch = NINT(param(23))

      CALL sumdem(2, switch, ear, ne, param(8), param(7), param(22),
     &            NTEMP, tarr, dem, ifl, .FALSE., 0., photar, photer, 
     &            status)

      RETURN
      END 

