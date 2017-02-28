      SUBROUTINE kolm1(nintao,psum1,psume,j1,chisq,prob)
      real*4 psum1(*), psume(*)
C   find the maximum difference of the integral probability
C   distributions of sample from expected theoretical distribution
C   and calculates the corresponding
C   chi suared according to the kolmogorov-smirnov test
C    psum1 = integral distribution to test
C    psume = expected integral distribution
      real*4 difmax, chisq, prob, pdiff, probks, aj1, aa
      integer*4 n, nintao, j1
      difmax = 0.
      DO n = 1, nintao
        pdiff = abs(psum1(n)-psume(n))
        IF ( pdiff.GE.difmax ) difmax = pdiff
      END DO
C  compute chisquare
      chisq = 4.*(difmax)**2*j1
      aj1 = j1
      aa = sqrt(aj1)*difmax
      prob = probks(aa)
      RETURN
      END
