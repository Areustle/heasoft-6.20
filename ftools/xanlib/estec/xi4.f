*- xi4.for - geti*4 from string
      integer*4 function xi4(s,status)
      include 'status.codes'
* Import :
      character*(*) s
* Status :
      integer*4 status
* External reference :
      logical*4 isint
*-
      if(status.ne.ok__)return
      if(isint(s))then
         read(s,*,iostat=status)xi4
      else
         status=error__
      endif
      return
      end
