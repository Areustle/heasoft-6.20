*- xra.for - extract ra in degrees from string
      real*8 function xra(string,status)
* Author :
*  Andy Pollock (HEAGIP::POLLOCK)
*  Computer & Scientific Co. Ltd., 34 Westwood Road, Sheffield S11 7EY, England.
* History :
*  10 October 1991 : original
*  26 May 1993 : more rigour using xlist & xr8

      include 'status.codes'

* Import :
      character*(*) string
* Status :
      integer status
* Local variables :
      real*8 hour,minute,second
      character(10) term
      integer*4 lt
      integer parse
* External reference :
      character(10) xlist
      real*8 xr8
*-
      if(status.ne.ok__) then
        xra = 0
        return
      endif

      parse=0
      term=xlist(string,parse,lt,status)
      if(status.eq.ok__)then
         hour=xr8(term,status)
         if(status.eq.ok__)then
            term=xlist(string,parse,lt,status)
            if(status.eq.ok__)then
               minute=xr8(term,status)
               if(status.eq.ok__)then
                  term=xlist(string,parse,lt,status)
                  if(status.eq.ok__)then
                     second=xr8(term,status)
                  else
                     status=ok__
                     second=0d0
                  endif
               endif
            else
               status=ok__
               minute=0d0
               second=0d0
            endif
         endif
      endif
        
      if(status.eq.ok__)then
         xra=(hour+minute/6d1+second/36d2)*15d0
      else
         xra=0d0
      endif

      return

      end
