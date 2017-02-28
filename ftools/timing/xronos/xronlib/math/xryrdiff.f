c
      SUBROUTINE xryrdiff(iyra,iyro,idays)
c
c  8/9/89 ls  convert difference of years in days 
c
c  I  iyra=start year 
c  I  iyro=stop year
c  O  idays=no of days between iyra and iyro 
c
      INTEGER iyra, iyro, idays
c
c  Yrs stores the offsets (in days) for the begin. of each year between 1968 
c  and 2007 with respct to 1980 
c
c
      INTEGER yrs(40)
c                          1968+4n    1969+4n    1970+4n   1971+4n 
      DATA    yrs    /     -4383,     -4017,     -3652,     -3287,
     &                       -2922,     -2556,     -2191,     -1826,
     &                       -1461,     -1095,      -730,      -365, 
     &                           0,       366,       731,      1096, 
     &                        1461,      1827,      2192,      2557,
     &                        2922,      3288,      3653,      4018,
     &                        4383,      4749,      5114,      5479,
     &                        5844,      6210,      6575,      6940,
     &                        7305,      7670,      8035,      8400,
     &                        8765,      9131,      9496,      9861/
     
c
c
c
        IF (iyro.lt.1968.or.iyra.lt.1968.or.
     &      iyro.gt.2007.or.iyra.gt.2007)return
        idays=yrs(iyro-1968+1)-yrs(iyra-1968+1)
        RETURN
      END
c
c
c
