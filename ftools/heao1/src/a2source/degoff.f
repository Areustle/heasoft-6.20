**==degoff.spg  processed by SPAG 4.50J  at 11:08 on 28 Oct 1998
 
C THIS ROUTINE COMPUTES SCAN ANGLE AND DEG. OFF SCAN LINE
      SUBROUTINE DEGOFF(Sunlon,Eclon,Eclat,Sa,Dgoff)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL*8 alf , Dgoff , Eclat , Eclon , phi , phis , rad , Sa , 
     &       srccos , srcsin , suncos , Sunlon , sunsin , sx , sy , sz , 
     &       theta , xtmp
C*** End of declarations inserted by SPAG
C  INPUTS ARE ECL.LONG. OF SUN, ECLON & LAT OF SOURCE
C  OUPUTS ARE SCAN ANGLE AND DEG. OFF SCAN LINE
C  UNITS ARE DEGREES
      DATA rad/57.29578D0/
      theta = (90.-Eclat)/rad
      phi = Eclon/rad
      phis = Sunlon/rad
      sunsin = DSIN(phis)
      suncos = DCOS(phis)
      srcsin = DSIN(theta)
      srccos = DCOS(theta)
      sz = srccos
      sx = srcsin*DCOS(phi)
      sy = srcsin*DSIN(phi)
C  COMPUTE DEG OFF
      alf = sx*suncos + sy*sunsin
      Dgoff = 90. - DACOS(alf)*rad
C   COMPUTE SA
      xtmp = -sunsin*sx + suncos*sy
      Sa = DATAN2(srccos,xtmp)*rad
      IF ( Sa.LT.0. ) Sa = Sa + 360.
 
      RETURN
 
      END
