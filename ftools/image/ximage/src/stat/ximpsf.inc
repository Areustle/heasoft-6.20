C
C Include file to define psf arrays
C
      INTEGER*4 MAXPSF
      PARAMETER (MAXPSF=5000)

C     Fractional EEF as a function of radius (detector pixels)
      REAL*4 FRAC(0:MAXPSF)
      
C     Weighted Fractional EEF as a function of radius (detector pixels)
      REAL*4 FRACW(0:MAXPSF)

C     Max expo contained w/in a given radius (detector pixels)
      REAL*8 EXPOMAXS(0:MAXPSF)
      
      COMMON /PSFCOM/FRAC
      COMMON /PSFCOM/FRACW
      COMMON /PSFCOM/EXPOMAXS
