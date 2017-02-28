*- xr8.for - get r*8 from string
      real*8 function xr8(s,status)
      include 'status.codes'
* Import :
      character*(*) s
* Status :
      integer*4 status
* External reference :
      logical*4 isdouble
      logical*4 isfloat
      logical*4 isint
*-
      if(status.ne.ok__)return
      if(isdouble(s).or.isfloat(s).or.isint(s))then
         read(s,*,iostat=status)xr8
      else
         status=error__
      endif
      return
      end
