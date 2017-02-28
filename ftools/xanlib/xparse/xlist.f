*- xlist.for - deliver successive arguments from list
      character*(*) function xlist(list,parse,length,status)
* Description :
*  The PARSE argument is XPARSE's normal PARSE that shows the current 
*  position. PARSE=0 on entry signals that the first argument from the
*  list is  required. Otherwise it should be left alone.
* Author :
*  Andy Pollock
* History :
*  12 February 1993 : original

* Import :
      character*(*) list
* Import parse=0/export :
      integer*4 parse
* Export :
      integer*4 length
* Status :
      integer*4 status
* Local variables :
      integer*4 jp1,jp2,delimiter
      logical*4 empty
*-
      call xgtarg(list,parse,jp1,jp2,empty,status,delimiter)
      if((status.eq.0).and..not.empty)then
         xlist=list(jp1:jp2)
         length=jp2-jp1+1
      else
         xlist=' '
         length=0
      endif

      return

      end
