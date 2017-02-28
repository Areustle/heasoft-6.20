*- xdec - extract declination in degrees from string
      real*8 function xdec(string,status)
* Author :
*  Andy Pollock (HEAGIP::POLLOCK)
*  Computer & Scientific Co. Ltd., 34 Westwood Road, Sheffield S11 7EY, England.
* History :
*  10 October 1991 : original
*  21 January 1992 : deal with space between sign and degrees
*  26 May 1993 : more rigour using south, xlist & xr8

      include 'status.codes'

* Import :
      character*(*) string
* Status :
      integer status
* Local variables :
      character(80) s
      real*8 degree,arcmin,arcsec
      logical*4 south
      character(10) term
      integer*4 lt
      integer*4 parse
      integer*4 i
* External references :
      character(10) xlist
      real*8 xr8
*-
      if(status.ne.ok__) then
        xdec = 0
        return
      endif

      s=string
      i=index(s,'-')
      south=(i.gt.0)
      if(south)then
         do while(i.gt.0)
            s(i:i)=' '
            i=index(s,'-')
         end do
      endif

      parse=0
      term=xlist(s,parse,lt,status)
      if(status.eq.ok__)then
         degree=xr8(term,status)
         if(status.eq.ok__)then
            term=xlist(s,parse,lt,status)
            if(status.eq.ok__)then
               arcmin=xr8(term,status)
               if(status.eq.ok__)then
                  term=xlist(s,parse,lt,status)
                  if(status.eq.ok__)then
                     arcsec=xr8(term,status)
                  else
                     status=ok__
                     arcsec=0d0
                  endif
               endif
            else
               status=ok__
               arcmin=0d0
               arcsec=0d0
            endif
         endif
      endif

      if(status.eq.ok__)then
         xdec=degree+arcmin/6d1+arcsec/36d2
         if(south)xdec=-xdec
      else
         xdec=0d0
      endif

      return

      end
