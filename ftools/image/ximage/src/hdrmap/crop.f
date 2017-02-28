      subroutine crop(Cmdid,Mapid,Status)
      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Crop and existing map with new center and/or size.
c   Pixel size is always preserved.  Only crops on whole pixels.
c  
c  I  cmdid        (i) Command id
c  I  mapid        (s) Current map id
c  O  status       (i) Error flag (0=OK)
c
      integer Cmdid, Status
      character*(*) Mapid
c
c  Local variables
      integer i, argc
      character(1) maptype
      character(40) rastr, decstr
      integer equinox, ixmin, ixmax, iymin, iymax
      real*8 xsky, ysky, xpix, ypix, pnts(4)
      real*8 xmin, xmax, ymin, ymax, ximg, yimg
      integer size, ineqx
      integer inszx, inszy, outszx, outszy, totsz
      integer inexszx, inexszy
      integer inptr, outptr, inexptr, outexptr
      character*(MAX_IDSTR) inmapid, outmapid, inwcsid, cpwcsid
      character*(MAX_IDSTR) inexpid, outexpid, exwcsid, cpexwcsid
      real*4 newmin, newmax
      integer maxparm, frstat
      parameter(maxparm = 4)
      real*4 val, dar1(3), dar2(3)
      real*8 cenpix(2), newzm(2), dd

      real*8 dnull
      real*4 rnull
      logical isloaded, iseqmapid, iscpmapid, ineqout, isrnull, isdnull
      logical isdisplay
      
      outmapid = ' '
      inmapid = Mapid
      rastr = ' '
      decstr = ' '
      xpix = dnull()
      ypix = dnull()
      xsky = dnull()
      ysky = dnull()
      outszx = -1
      outszy = -1
      size = -1
      ineqx = -10

      Status = 0

      call numcarg(cmdid, argc, status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'OUTMAP',outmapid,Status)
      CALL GPARS(Cmdid,'INMAP',inmapid,Status)
      CALL GPARD(Cmdid,'XPIX',xpix,Status)
      CALL GPARD(Cmdid,'YPIX',ypix,Status)
      CALL GPARS(Cmdid,'RA',rastr,Status)
      CALL GPARS(Cmdid,'DEC',decstr,Status)
      CALL GPARD(Cmdid,'XSKY',xsky,Status)
      CALL GPARD(Cmdid,'YSKY',ysky,Status)
      CALL GPARI(Cmdid,'EQUINOX',ineqx,Status)
      CALL GPARI(Cmdid,'SZX',outszx,Status)
      CALL GPARI(Cmdid,'SZY',outszy,Status)
      CALL GPARI(Cmdid,'SIZE',size,Status)
      if ( Status.ne.0 ) return

      if ( size.gt.0 ) then
         outszx = size
         outszy = size
      endif

c  Get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)

      if ( ineqx.eq.-10 ) ineqx = Equinox
      if ( rastr.ne.' ' .and. decstr.ne.' ' ) then
         call cnv_radec(rastr, decstr, xsky, ysky, dar1, dar2, 1, 0,
     &                  status)
         if ( status.ne.0 ) then
            call xwrite(' Bad ra/dec format', 5)
            return
         endif
      endif

      if ( .not.isloaded(inmapid) ) then
         call XWRITE(' Image for inmap not loaded', 10)
         Status = -1
         return
      endif
      call gheads(inmapid, 'WCSID', inwcsid, 0, status)
      if ( inwcsid.eq.' ' ) then
         call XWRITE(' No WCSID for inmap', 10)
         Status = -1
         return
      endif
      if ( outmapid.eq.' ' ) then
         outmapid = inmapid
         call XWRITE(' Overwriting input map', 10)
      endif
c
c  Determine center and size
c
      if (.not.isdnull(xsky) .and. .not.isdnull(ysky) ) then
         call wcsimgsky(inwcsid, ximg, yimg, xsky, ysky,
     &                  ineqx, 0, status)
         call wcsimgpix(inwcsid, ximg, yimg, xpix, ypix, 1,
     &                  status)
      else if ( isdnull(xpix) .or. isdnull(ypix) ) then
         if ( .not.isdisplay() ) then
            call xwrite(' WARNING: No display', 10)
            status = -1
         else if ( .not.iscpmapid('DIS',inmapid) ) then
            call xwrite(' Display map is not input map', 10)
            status = -1
         endif
         if ( status.ne.0 ) then
            call xwrite(' Cropped area undefined', 10)
            return
         endif

         call xwrite(' Select rectangular area to crop', 10)
         call tclresld('select box', pnts, i, 4, status)
         if ( status.ne.0 ) return
         xpix = (pnts(3) + pnts(1))/2.0
         ypix = (pnts(4) + pnts(2))/2.0
         call wcsimgpix(inwcsid, xmin, ymin, pnts(1), pnts(2), 0,
     &                  status)
         call wcsimgpix(inwcsid, xmax, ymax, pnts(3), pnts(4), 0,
     &                  status)
         outszx = nint(abs(xmax - xmin))
         outszy = nint(abs(ymax - ymin))
      endif

      if ( isdnull(xpix) .or. isdnull(ypix) ) then
         call XWRITE(' Center of cropped area undefined', 10)
         status = -1
         return
      endif
      if ( outszx.le.0 .or. outszy.le.0 ) then
         call XWRITE(' Size of cropped area undefined', 10)
         status = -1
         return
      endif
c
c  At this point center is distilled down to x/ypix 
c    and size is outszx/y. Now force center to be on image pixel 
c    boundary and force even image size
c
      call wcsimgpix(inwcsid, ximg, yimg, xpix, ypix, 0, status)
      ximg = int(ximg) + 0.5
      yimg = int(yimg) + 0.5
      call wcsimgpix(inwcsid, ximg, yimg, xpix, ypix, 1, status)
      outszx = MAX(2,int(outszx/2)*2)
      outszy = MAX(2,int(outszy/2)*2)
      ixmin = int(ximg - outszx/2.0 + 0.5)
      ixmax = int(ximg + outszx/2.0 - 0.5)
      iymin = int(yimg - outszy/2.0 + 0.5)
      iymax = int(yimg + outszy/2.0 - 0.5)

      inptr = -1
      outptr = -1
      inexptr = -1
      outexptr = -1
c
c  Get input map and setup memory for output map
c
      call gheadi(inmapid, 'MAPPTR', inptr, 0, status)
      call gheads(inmapid, 'MAPTYPE', maptype, 0, status)
      call gheadi(inmapid, 'SZX', inszx, 0, status)
      call gheadi(inmapid, 'SZY', inszy, 0, status)
      call expiremap(inmapid, status)
      call gheads(inmapid, 'EXMAPID', inexpid, 0, status)
      if ( inexpid.ne.' ' ) then
         call gheadi(inexpid, 'MAPPTR', inexptr, 0, status)
         call gheadi(inexpid, 'SZX', inexszx, 0, status)
         call gheadi(inexpid, 'SZY', inexszy, 0, status)
         call expiremap(inexpid, status)
         call gheads(inmapid, 'WCSID', exwcsid, 0, status)
         if ( inexszx.ne.inszx .or. inexszy.ne. inszy ) then
            call xwrite(' Input map size != exposure map size', 5)
            call xwrite(' Exposure map will be ignored', 5)
            inexptr = -1
            inexpid = ' '
         endif
      endif
c
c  Modify copy of input wcs
c
      call copywcs(inwcsid, MAX_IDSTR, cpwcsid, status)
      cenpix(1) = ximg
      cenpix(2) = yimg
      newzm(1) = 1.
      newzm(2) = 1.
      call wcslintrf(cpwcsid, cenpix, outszx, outszy, newzm, status)
c
c  Copy header and allocate output map
c  (save mapptr from freeing if in/out are same map)
c
      ineqout = .false.
      if ( iseqmapid(inmapid, outmapid) ) then
         ineqout = .true.
      else
         call mapfree(outmapid, status)
         call cphead(inmapid, outmapid)
      endif
      call gheadi(outmapid, 'MAPPTR', -1, 1, status)
      call gheads(outmapid, 'WCSID', ' ', 1, status)
      call gheads(outmapid, 'EXMAPID', ' ', 1, status)
      call gheads(outmapid, 'MAPCOPY', ' ', 1, status)
      call mapalloc(outszx, outszy, outmapid, outptr, status)
      if ( status.ne.0 ) then
         call xwrite(' Failed to allocate memory for crop', 10)
         return
      endif
c
c  Copy subsection of existing map into new output map
c    Negative values for bounds are permitted.  Those out-of-bounds
c    pixels will be set to NULL
c
      call cprrmap(memr(inptr), inszx, inszy, ixmin, ixmax, iymin,
     &             iymax, memr(outptr), outszx, outszy, 1, outszx, 1,
     &             outszy, status)
      if ( status.ne.0 ) then
         call mapfree(outmapid, frstat)
         goto 500
      endif
c
c  Update coordinate info
c
      call wcstohdr(cpwcsid, outmapid, status)
      call gheads(outmapid, 'WCSID', cpwcsid, 1, status)
c
c  Remap exposure map if it exists
c
      if ( inexpid.ne.' ' ) then
         call getexmap(outmapid, outexpid, status)
         call cphead(inexpid, outexpid)
         call copywcs(cpwcsid, MAX_IDSTR, cpexwcsid, status)
         call gheadi(outexpid, 'MAPPTR', -1, 1, status)
         call gheads(outexpid, 'MAPTYPE', 'R', 1, status)
         call gheads(outexpid, 'WCSID', ' ', 1, status)
         call gheads(outexpid, 'MAPCOPY', ' ', 1, status)
         call gheads(outmapid, 'EXMAPID', outexpid, 1, status)
         call mapalloc(outszx, outszy, outexpid, outexptr, status)

         call cprrmap(memr(inexptr), inszx, inszy, ixmin, ixmax, iymin,
     &                iymax, memr(outexptr), outszx, outszy, 1, outszx,
     &                1, outszy, status)
         if ( status.ne.0 ) then
            call mapfree(outexpid, frstat)
            goto 500
         endif
c
c  Update coordinate info
c
         call wcstohdr(cpexwcsid, outexpid, status)
         call gheads(outexpid, 'WCSID', cpexwcsid, 1, status)

      endif
c
c  Update min/max
c
      totsz = outszx*outszy
      newmin = rnull()
      newmax = rnull()
      do i = 1, totsz
         val = memr(outptr+i-1)
         if ( .not.isrnull(val) ) then
            if ( isrnull(newmin) .or. val.lt.newmin ) newmin = val
            if ( isrnull(newmax) .or. val.gt.newmax ) newmax = val
         endif
      enddo
      dd = newmin
      call gheadd(mapid, 'DATAMIN', dd, 1, status)
      dd = newmax
      call gheadd(mapid, 'DATAMAX', dd, 1, status)
c
c  Free input image if in=out
c
  500 continue
      if ( ineqout ) then
         call ralloc(0, inszx, inszy, inptr, status)
         if ( inexpid.ne.' ' ) then
            call ralloc(0, inszx, inszy, inexptr, status)
         endif
      endif
   
      return
      end
