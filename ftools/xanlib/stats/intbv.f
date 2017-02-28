      SUBROUTINE intbv(class,ecor)
      integer*4 iclass
      real*4 class, ecor
      REAL*4 cor(70)
      DATA cor/4*0., -0.32, -0.32, -0.32, -0.31, -0.31, -0.30, -0.26,
     &     -0.24, -0.2, -0.18, -0.16, -0.14, -0.12, -0.09, -0.06, 0.,
     &     0.03, 0.06, 0.09, 0.12, 0.15, 0.18, 0.20, 0., 0., 0.33, 0.35,
     &     0.38, 0.42, 0.44, 0.45, 0.47, 0.5, 0.53, 0., 0.6, 0.62, 0.64,
     &     0.65, 0.66, 0.68, 0.69, 0.70, 0.72, 0., 0.81, 0.88, 0.92,
     &     0.98, 1.05, 1.15, 1.22, 1.30, 0., 0., 1.41, 1.48, 1.52, 1.55,
     &     1.56, 1.61, 1.7, 1.8, 2., 0., 0./
      IF ( class.LT.2100. .AND. class.GE.2900. )then
        write(*,*) ' parameter outside allowed range in sub intbv'
        return
      endif
      iclass = (class-2000.)/10.
      ecor = cor(iclass-10)
      RETURN
      END
