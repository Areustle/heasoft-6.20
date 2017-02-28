      subroutine prback(mapid, status)
      implicit none
c
c  Print calculated background and box size used
c
c  I  mapid   (s)  Map id string
c  O  status  (i)  Error flag(0=OK)
c
      CHARACTER*(*) Mapid
      INTEGER*4 Status

      INCLUDE 'backgd.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      REAL*4 imgpix, detpix, arcmin, arcminsec, detpixsec
      LOGICAL ISRNULL
c
      Status = 0

      if ( IBBac.gt.0 ) then
         WRITE (ZWRite,99002) IBBac
         CALL XWRITE(ZWRite,10)
      endif
c
      call backvals(mapid, imgpix, detpix, arcmin, detpixsec, arcminsec, 
     &              status)
      WRITE (ZWRite,99003) detpix
      CALL XWRITE(ZWRite,10)
      WRITE (ZWRite,99004) imgpix
      CALL XWRITE(ZWRite,10)
      if ( .not.ISRNULL(arcminsec) ) then
         WRITE (ZWRite,99005) arcminsec
         CALL XWRITE(ZWRite,10)
         WRITE (ZWRite,99006) detpixsec
         CALL XWRITE(ZWRite,10)
      else if ( .not.ISRNULL(arcmin) ) then
         WRITE (ZWRite,99007) arcmin
         CALL XWRITE(ZWRite,10)
      endif
c
      RETURN
99002 FORMAT (' Background box size =',i4)
99003 FORMAT (' Background =',1pe10.4,0p,' cts/original-pixel')
99004 FORMAT ('            =',1pe10.4,0p,' cts/image-pixel')
99005 FORMAT ('            =',1pe10.4,0p,' cts/sqarcmin/s')
99006 FORMAT ('            =',1pe10.4,0p,' cts/original-pixel/s')
99007 FORMAT ('            =',1pe10.4,0p,' cts/sqarcmin')
      END
