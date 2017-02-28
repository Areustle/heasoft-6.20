      SUBROUTINE ft_gcf(gammcf,a,x,gln,status)
      INTEGER ITMAX,status
      REAL a,gammcf,gln,x,EPS,FPMIN
      PARAMETER (ITMAX=100,EPS=3.e-7,FPMIN=1.e-30)
CU    USES ft_gammln
      INTEGER i
      REAL an,b,c,d,del,h,ft_gammln
      gln=ft_gammln(a)
      b=x+1.-a
      c=1./FPMIN
      d=1./b
      h=d
      do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.
        d=an*d+b
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
      status = 1003
C      call fcecho('a too large, ITMAX too small in ft_gcf')
1     gammcf=exp(-x+a*log(x)-gln)*h
      return
      END
