**==fbg.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION FBG(U,Gam)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , a1 , born , FBG , g1 , g2 , Gam , gam1 , gam2 , gam3 , 
     &     p , power , U , u1 , u2
      INTEGER i , m , m1 , n
C*** End of declarations inserted by SPAG
c  39th values of A1, A2, and A3 revised to get rid of 5% bump
c  in Kellog et al fit to Karzas and Latter at gamma2 = 0.1,
c  u = .3-1. : uses numbers from Larry Molnar, Jan 1988
c  and corrections from Jack Hughes
c
      DOUBLE PRECISION t , ai , ak , u4
      DIMENSION a(6,7,3) , a1(126) , gam2(6) , gam3(6)
      EQUIVALENCE (a,a1)
      DATA gam2/.7783 , 1.2217 , 2.6234 , 4.3766 , 20. , 70./
      DATA gam3/1. , 1.7783 , 3. , 5.6234 , 10. , 30./
      DATA (a1(i),i=1,42)/1.001 , 1.004 , 1.017 , 1.036 , 1.056 , 
     &      1.121 , 1.001 , 1.005 , 1.017 , 1.046 , 1.073 , 1.115 , 
     &      .9991 , 1.005 , 1.030 , 1.055 , 1.102 , 1.176 , .9970 , 
     &      1.005 , 1.035 , 1.069 , 1.134 , 1.186 , .9962 , 1.004 , 
     &      1.042 , 1.100 , 1.193 , 1.306 , .9874 , .9962 , 1.047 , 
     &      1.156 , 1.327 , 1.485 , .9681 , .9755 , 1.02009 , 1.208 , 
     &      1.525 , 1.965/
      DATA (a1(i),i=43,84)/.30290 , .16160 , .04757 , .01300 , .00490 , 
     &      -.00320 , .49050 , .21550 , .08357 , .02041 , .00739 , 
     &      .00029 , .65400 , .28330 , .08057 , .03257 , .00759 , 
     &      -.00151 , 1.0290 , .39100 , .12660 , .05149 , .01274 , 
     &      .00324 , .95690 , .48910 , .17640 , .05914 , .01407 , 
     &      -.00024 , 1.2360 , .75790 , .32600 , .10770 , .02800 , 
     &      .00548 , 1.3270 , 1.0170 , 0.60166 , .20500 , .06050 , 
     &      .00187/
      DATA (a1(i),i=85,126)/ - 1.3230 , -.25400 , -.01571 , -.001000 , 
     &      -.000184 , .00008 , -4.7620 , -.33860 , -.03571 , -.001786 , 
     &      -.000300 , .00001 , -6.3490 , -.42060 , -.02571 , -.003429 , 
     &      -.000234 , .00005 , -13.231 , -.59000 , -.04571 , -.005714 , 
     &      -.000445 , -.00004 , -7.6720 , -.68520 , -.06430 , 
     &      -.005857 , -.000420 , .00004 , -7.1430 , -.99470 , -.12000 , 
     &      -.010070 , -.000851 , -.00004 , -3.1750 , -1.1160 , 
     &      -.22695 , -.018210 , -.001729 , .00023/

      m=0
      n=0
 
      gam1 = Gam*1000.
      IF ( gam1.GT.100. ) THEN
         power = -.134/(Gam**.2097)
         FBG = 1.5*(3.*U)**power
         RETURN
      ELSE
         u2 = U**2
C
C*****COMPUTE BORN APPROXIMATION GAUNT FACTOR
C
         u1 = U/2.
         t = u1/3.75
         u4 = u1/2.
         IF ( u1.GT.2. ) THEN
C
            ak = 1.2533141 - .07832358/u4 + .02189568/u4**2 - 
     &           .01062446/u4**3 + .00587872/u4**4 - .00251540/u4**5 + 
     &           .00053208/u4**6
            ak = ak/(EXP(u1)*SQRT(u1))
         ELSE
            ai = 1.0 + 3.5156229*t**2 + 3.0899424*t**4 + 
     &           1.2067492*t**6 + 0.2659732*t**8 + 0.0360768*t**10 + 
     &           0.0045813*t**12
            ak = -1.*LOG(u4)*ai - .57721566 + .42278420*u4**2 + 
     &           .23069758*u4**4 + .0348859*u4**6 + .00262698*u4**8 + 
     &           .00010750*u4**10 + .0000074*u4**12
         ENDIF
         born = SNGL(0.5513d0*EXP(u1)*ak)
C
C*****COMPUTE POLYMONIAL FACTOR TO MULTIPLY BORN APPROXIMATION
C
         IF ( gam1.GE.1. ) THEN
            IF ( U.GE..003 ) THEN
               IF ( U.LE..03 ) n = 1
               IF ( (U.LE..3) .AND. (U.GT..03) ) n = 2
               IF ( (U.LE.1.) .AND. (U.GT..3) ) n = 3
               IF ( (U.LE.5.) .AND. (U.GT.1.) ) n = 4
               IF ( (U.LE.15.) .AND. (U.GT.5.) ) n = 5
               IF ( U.GT.15. ) n = 6
               IF ( gam1.LE.1.7783 ) m = 1
               IF ( (gam1.LE.3.) .AND. (gam1.GT.1.7783) ) m = 2
               IF ( (gam1.LE.5.6234) .AND. (gam1.GT.3.) ) m = 3
               IF ( (gam1.LE.10.) .AND. (gam1.GT.5.6234) ) m = 4
               IF ( (gam1.LE.30.) .AND. (gam1.GT.10.) ) m = 5
               IF ( (gam1.LE.100.) .AND. (gam1.GT.30.) ) m = 6
               m1 = m + 1
               g1 = (a(n,m,1)+a(n,m,2)*U+a(n,m,3)*u2)*born
               g2 = (a(n,m1,1)+a(n,m1,2)*U+a(n,m1,3)*u2)*born
               p = (gam1-gam3(m))/gam2(m)
               FBG = (1.0-p)*g1 + p*g2
               RETURN
            ENDIF
         ENDIF
      ENDIF
      FBG = born
      RETURN
      END
 
