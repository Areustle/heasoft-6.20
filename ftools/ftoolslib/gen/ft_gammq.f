      FUNCTION ft_gammq(a,x,status)
      REAL a,ft_gammq,x
      INTEGER status
CU    USES ft_gcf,ft_gser
      REAL gammcf,gamser,gln
      status = 0
      if(x.lt.0..or.a.le.0.) then
        status = 1000
        call fcecho('bad arguments in ft_gammq')
      endif
      if(x.lt.a+1.)then
        call ft_gser(gamser,a,x,gln,status)
        ft_gammq=1.-gamser
      else
        call ft_gcf(gammcf,a,x,gln,status)
        ft_gammq=gammcf
      endif
      return
      END
