*- xr4.for - get r*4 from string
      real*4 function xr4(s,status)
      include 'status.codes'
* Import :
      character*(*) s
* Status :
      integer*4 status
* External reference :
      logical*4 isfloat
      logical*4 isint
*-
      if(status.ne.ok__)return
      if(isfloat(s).or.isint(s))then
         read(s,*,iostat=status)xr4
      else
         status=error__
      endif
      return
      end
