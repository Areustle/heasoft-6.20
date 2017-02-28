      SUBROUTINE ASCA_VIGN(Off_axis,Phi,Energy,Vcor,Ierr)
      IMPLICIT NONE
c
c  I  off-axis in armin
c  I  phi
c  I  energy in keV (constant value)
c  O  vcor vignetting correction value
c  O  ierr error
 
c     On-axis effective area with off-axis coeff (Awaki, 1992)
      INTEGER*4 Ierr
      REAL*4 vign_coeff(11)
      REAL*4 VIGN_AWAKI
      REAL*4 Vcor , Energy , Off_axis , Phi
c      DATA   vign_coeff / 0.013075, 0.00082468, -0.00015377,
c     &             3.9686E-5, 0.6, 2555.0, -405.0, 16.81, 46.4,
c     &             0.28, 1.4/
c
c  updated as per awaki email 6/22/93 by Nick
 
      DATA vign_coeff/0.010413091 , 0.00040268 , -0.000141254 , 
     &     4.09908E-5 , 0.743 , 2555.0 , -405.0 , 16.81 , 46.4 , 0.28 , 
     &     1.4/
C          include awakicoeff.inc
c
      Vcor = VIGN_AWAKI(Energy,Off_axis,Phi,vign_coeff)
 
      RETURN
      END
