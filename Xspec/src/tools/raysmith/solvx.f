**==solvx.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION SOLVX(F,E,H)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ax0 , ax1 , ax2 , DENz , DYNam , E , F , H , PB , plink , 
     &     PREss , qx , rx , SOLVX , thetax , x
      INTEGER k
C*** End of declarations inserted by SPAG
      COMMON /SLV   / DENz , PREss , PB , DYNam

c suppress compiler warning
      x = F

C     CALCULATE COMPRESSION
      ax2 = 2.*DENz*(H-E)/PB
      ax1 = -5.*PREss/PB
      ax0 = 4.*DYNam/PB
      qx = ax2*ax2/9. - ax1/3.
      rx = (3.*ax0-ax1*ax2)/6. + (ax2**3)/27.
      plink = rx/qx**1.5
      IF ( plink.LE.0.997 ) THEN
         thetax = ACOS(plink)/3.
         SOLVX = SQRT(qx)*(COS(thetax)+1.732*SIN(thetax)) - ax2/3.
         RETURN
      ENDIF
      x = (-ax1+SQRT(ax1*ax1-4.*ax2*ax0))/(2.*ax2)
      DO 100 k = 1 , 4
         x = (-ax1+SQRT(ax1*ax1-4.*ax2*(ax0+x**3)))/(2.*ax2)
 100  CONTINUE
      SOLVX = x
      RETURN
      END
 
