*- xmdyd - (month,day-of-month) to (year,day-of-year) 
      subroutine xmdyd(month,dom,year,doy)
* Author :
*  Andy Pollock
* History :
*  6 October 1992 : original
* Import :
      integer*4 month
      integer*4 dom
      integer*4 year
* Export :
      integer*4 doy
* Local variables :
      logical*4 leap_year
* Local data :
      integer*2 m(12)
      data m/1,32,60,91,121,152,182,213,244,274,305,335/
*-
      doy=m(month)+dom-1
      leap_year=((year/4)*4.eq.year)
      if(leap_year.and.(doy.ge.m(3)))doy=doy+1

      return

      end
