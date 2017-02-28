*- xydmd - (year,day-of-year) to (month,day-of-month)
      subroutine xydmd(year,doy,month,dom)
* Author :
*  Andy Pollock
* History :
*  3 October 1991 : original
* Import :
      integer*4 year
      integer*4 doy
      integer*4 month
      integer*4 dom
* Local variables :
      logical*4 leap_year
c      character(20) s
c      integer*4 status
* External reference :
      integer*4 loc0i
* Local data :
      integer*4 m(12)
      data m/1,32,60,91,121,152,182,213,244,274,305,335/
*-
      leap_year=((year/4)*4.eq.year)
      if(leap_year.and.(doy.ge.m(3)))then
         if(doy.eq.m(3))then
            month=2
            dom=29
         else
            month=loc0i(doy-1,12,m)
            if((month.lt.12).and.(doy-1.eq.m(month+1)))month=month+1
            dom=doy-m(month)
         endif
      else
         month=loc0i(doy,12,m)
         if((month.lt.12).and.(doy.eq.m(month+1)))month=month+1
         dom=doy-m(month)+1
      endif

      return

      end
