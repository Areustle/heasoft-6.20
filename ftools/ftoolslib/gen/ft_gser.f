      SUBROUTINE ft_gser(gamser,a,x,gln,status)
      INTEGER ITMAX,status
      REAL a,gamser,gln,x,EPS
      PARAMETER (ITMAX=100,EPS=3.e-7)
CU    USES ft_gammln
      INTEGER n
      REAL ap,del,sum,ft_gammln
      gln=ft_gammln(a)
      if(x.le.0.)then
        if(x.lt.0.) then
          status = 1001
          call fcecho('x < 0 in ft_gser')
        endif
        gamser=0.
        return
      endif
      ap=a
      sum=1./a
      del=sum
      do 11 n=1,ITMAX
        ap=ap+1.
        del=del*x/ap
        sum=sum+del
        if(abs(del).lt.abs(sum)*EPS)goto 1
11    continue
      status = 1002
C      call fcecho('a too large, ITMAX too small in ft_gser')
1     gamser=sum*exp(-x+a*log(x)-gln)
      return
      END
