C
      SUBROUTINE kolmo(nintao,psum1,psum2,j1,j2,chisq,prob)
      real*4 psum1(*), psum2(*)
C   find the maximum difference of the integral probability
C   distributions of the two samples and calculates the corresponding
C   chi suared according to the kolmogorov-smirnov test
c
      real*4 difmax, chisq, prob, pdiff, probks, aa, ab
      integer*4 n, nintao, j1, j2
      difmax = 0.
      DO n = 1, nintao
        pdiff = abs(psum1(n)-psum2(n))
        IF ( pdiff.GE.difmax ) difmax = pdiff
      END DO
C  compute chisquare
      aa = float(j1)*float(j2)/(float(j1)+float(j2))
      chisq = 4.*aa*difmax**2.
      ab = sqrt(aa)*difmax
      prob = probks(ab)
      WRITE (*,*) ' max dist fotto1 fotto2 ', difmax, j1, j2
      RETURN
      END
