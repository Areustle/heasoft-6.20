*****************************************************************************
C     VIGN_AWAKI.F
C
C     ERIC GOTTHELF, GSFC, FEB 6 1993
C
C     SUBROUTINE TO RETURN THE MULTIPLICATIVE CORRECTION TO THE ON-AXIS
C     XRT EFFECTIVE AREA FOR A GIVEN ENERGY (KEV), PHI (ARC MINUTES),
C     AND THETA (RADIANS).
C
C     THE AWAKI FORMULA (1992), DERIVE FROM RAY-TRACING RESULTS
C     AND CALIBRATION DATA, IS USED.
C
*****************************************************************************
 
      REAL FUNCTION VIGN_AWAKI(Energy,Theta,Phi,Vign_coeff)
 
 
C     ARGUMENTS:
 
      REAL Energy , Theta , Phi , Vign_coeff(*)
 
C     LOCAL VARIABLES:
 
      INTEGER i
      REAL*8 a_coeff(4) , c_coeff(5)
      REAL*8 a , b , c , d , e , c1
      REAL*8 theta2 , theta_corr , azim_corr
 
C     START:
 
C     COPY INPUT COEFF TO WORKING VARIABLES:
 
      DO 100 i = 1 , 4
         a_coeff(i) = Vign_coeff(i)
         c_coeff(i) = Vign_coeff(i+5)
 100  CONTINUE
 
      c_coeff(5) = Vign_coeff(10)
 
      b = Vign_coeff(5)
      d = Vign_coeff(11)
 
C     INITIALIZE VARIABLES:
 
      e = 1.0
      a = a_coeff(1)
 
      theta2 = Theta*Theta
 
C     COMPUTE VIGNETING FUNCTION COEFFICENTS:
 
      DO 200 i = 2 , 4
         e = e*Energy
         a = a + e*a_coeff(i)
 200  CONTINUE
 
      c1 = c_coeff(1) + c_coeff(2)*Energy + c_coeff(3)*Energy*Energy + 
     &     c_coeff(4)*Theta*(1.0-EXP(-c_coeff(5)*Energy))
 
      c = theta2/c1
 
C     COMPUTE THETA CORRECTION:
 
      theta_corr = (1.0+a*theta2)**(-b)
 
C     COMPUTE AZIMUTHAL CORRECTION (CONVERT PHI TO DEGREES):
C I am not sure if is right......so i comment it out
C      phi = phi * 180.0 / 3.1415
 
      azim_corr = 1.0 - c*((1.0-ABS(COS(2.0*Phi))**d))
C     RETURN VIGNETTING CORRECTION:
 
      VIGN_AWAKI = REAL(theta_corr*azim_corr)
 
      RETURN
 
      END
