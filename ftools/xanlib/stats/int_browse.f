 
      SUBROUTINE integral_browse(gamma,final)
      EXTERNAL funzione
      real*4 gamma, final
      integer*4 n, nmax
      REAL*8 accur, a, b, funzione, aint, r, gamma1
      COMMON /funzio/ gamma1
      DATA a, b, nmax/0., 45., 300/
      accur = 1.D-4
      gamma1 = gamma
      CALL simp1(funzione,a,b,aint,r,n,accur,nmax)
      final = (aint*2.*3.1416/3600.-.0032)**(1./gamma1)
      RETURN
      END
