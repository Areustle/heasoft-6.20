      subroutine get_refram (Mapid,szx, szy,zmx,zmy, xcen,ycen,
     &                       status)
      implicit none
c
c Read internal header and convert ra dec roll north pixsize to degrees
c
c  I  MapID   (c)  Internal XIMAGE mapid 
c  O  szx     (i)  array size x dimention 
c  O  szy     (i)  array size in x dimention
c  O  zmx     (d)  zoom in x direction
c  O  zmy     (d)  zoom in y direction
c  O  xcen    (d)  x image center in ximage frame coordinates
c  O  ycen    (d)  y image center in ximage frame coordinates 
c  O  Status  (i)  Error flag (0=OK)
c
      include '../include/maxvals.inc'

      CHARACTER*(*) Mapid
      CHARACTER*(MAX_IDSTR) wcsid
      INTEGER*4 szx, szy, status
      REAL*8 zmx, zmy, xcen, ycen
c
c  Local variables
c
      INTEGER*4 tmpszx, tmpszy
      REAL*8 ximg, yimg, xiref, yiref, xref, yref, tmpzmx, tmpzmy
      LOGICAL isloaded
c
      if ( .not. isloaded(Mapid) ) then
         call XWRITE (' Image not loaded', 10)
         status=1 
         RETURN
      endif

      call gheadi(Mapid, 'SZX', tmpszx, 0, status)
      call gheadi(Mapid, 'SZY', tmpszy, 0, status)
      call gheadd(Mapid, 'ZMX', tmpzmx, 0, status)
      call gheadd(Mapid, 'ZMY', tmpzmy, 0, status)
      ximg = dble(tmpszx)/2.0 + 0.5
      yimg = dble(tmpszy)/2.0 + 0.5
      call gheads(Mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
c        No wcsid, calculating center from header
         call gheadd(Mapid, 'CRPIX1', xiref, 0, status)
         call gheadd(Mapid, 'CRPIX2', yiref, 0, status)
         call gheadd(Mapid, 'DRPIX1', xref, 0, status)
         call gheadd(Mapid, 'DRPIX2', yref, 0, status)
         xcen = (ximg - xiref)*tmpzmx + xref
         ycen = (yimg - yiref)*tmpzmy + yref
      else
         call wcsimgpix(wcsid, ximg, yimg, xcen, ycen, 1, status)
      endif
      szx = tmpszx
      szy = tmpszy
      zmx = tmpzmx
      zmy = tmpzmy

      RETURN
      END
