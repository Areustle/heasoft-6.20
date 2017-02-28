      subroutine remap(Cmdid,Mapid,Status)
      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Remap an existing map into another grid, rotated, resized,
c   recentered, etc. from the original.
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
      character(40) rotstr, methstr, rastr, decstr
      real*8 xsky, ysky, rebin
      integer size, equinox, ineqx
      integer inszx, inszy, outszx, outszy, coorszx, coorszy, totsz
      integer inexszx, inexszy
      integer inptr, outptr, inexptr, outexptr
      character*(MAX_IDSTR) inmapid, outmapid, coorid, inwcsid, cpwcsid
      character*(MAX_IDSTR) inexpid, outexpid, exwcsid, cpexwcsid
      real*4 newmin, newmax
      integer maxparm, numparm, frstat, REMAPMODE
      parameter(maxparm = 4)
      real*4 val, dar1(3), dar2(3)
      real*8 params(4), cenpix(2), newzm(2), outrot, dd
      character(100) ds
      logical isremap

      real*8 dnull
      real*4 rnull
      logical isloaded, iseqmapid, ineqout, isrnull, isdnull
      
      methstr = 'CONSERVE'
      outmapid = ' '
      inmapid = Mapid
      coorid = ' '
      rotstr = ' '
      rastr = ' '
      decstr = ' '
      xsky = dnull()
      ysky = dnull()
      rebin = 0.d0
      outszx = -1
      outszy = -1
      size = -1
      ineqx = -10
      do i = 1, maxparm
         params(i) = 0.d0
      enddo
c
c  Check remap_method/remap_params for default interpolation method
c
      call tclress("if [info exists remap_method] {set remap_method}", 
     &              methstr, 40, status)
      call tclresld(
     &      "if [info exists remap_params] {listclean $remap_params}", 
     &              params, numparm, maxparm, status)
c
c  Use most rigorous (slowest) algorithm as default
c
      if ( methstr.eq.' ' ) methstr = 'CONSERVE'

      Status = 0

      call numcarg(cmdid, argc, status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'METHOD',methstr,Status)
      CALL GPARLD(Cmdid,'PARAMS',params,numparm,maxparm,Status)
      CALL GPARS(Cmdid,'OUTMAP',outmapid,Status)
      CALL GPARS(Cmdid,'INMAP',inmapid,Status)
      CALL GPARS(Cmdid,'COORID',coorid,Status)
      CALL GPARS(Cmdid,'ROTANGLE',rotstr,Status)
      CALL GPARS(Cmdid,'RA',rastr,Status)
      CALL GPARS(Cmdid,'DEC',decstr,Status)
      CALL GPARD(Cmdid,'XSKY',xsky,Status)
      CALL GPARD(Cmdid,'YSKY',ysky,Status)
      CALL GPARI(Cmdid,'EQUINOX',ineqx,Status)
      CALL GPARI(Cmdid,'SZX',outszx,Status)
      CALL GPARI(Cmdid,'SZY',outszy,Status)
      CALL GPARI(Cmdid,'SIZE',size,Status)
      CALL GPARD(Cmdid,'REBIN',rebin,Status)
      if ( Status.ne.0 ) return

      call upc(methstr)

      if ( methstr.ne.' ' ) then
         if ( REMAPMODE(methstr, numparm, params) .eq. 0 ) then
            if ( methstr.ne.'?' ) status = -1
            return
         endif
      endif

      if ( size.gt.0 ) then
         outszx = size
         outszy = size
      endif

c Get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)

      if ( ineqx.eq.-10 ) ineqx = equinox
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
      if ( outmapid.eq.' ' ) then
         outmapid = inmapid
         call XWRITE(' Overwriting input map', 10)
      endif

      if ( coorid.ne.' ' ) then
         if ( iseqmapid(inmapid, coorid) ) then
            call XWRITE(
     &       ' Input map is already aligned to those coordinates', 10)
            Status = -1
            return
         endif
      endif

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
      call gheads(inmapid, 'WCSID', inwcsid, 0, status)
      call gheads(inmapid, 'EXMAPID', inexpid, 0, status)
      if ( inexpid.ne.' ' ) then
         call gheadi(inexpid, 'MAPPTR', inexptr, 0, status)
         call gheadi(inexpid, 'SZX', inexszx, 0, status)
         call gheadi(inexpid, 'SZY', inexszy, 0, status)
         call expiremap(inexpid, status)
         call gheads(inexpid, 'WCSID', exwcsid, 0, status)
         if ( inexszx.ne.inszx .or. inexszy.ne. inszy ) then
            call xwrite(' Input map size != exposure map size', 5)
            call xwrite(' Exposure map will be ignored', 5)
            inexptr = -1
            inexpid = ' '
         endif
      endif
c
c  Setup output coordinates
c
      coorszx = 0
      coorszy = 0

      if ( coorid.eq.' ' ) then
         isremap = .FALSE.
c
c  If no coorid is given modify copy of input wcs
c
         call wcsfrmcopy(inwcsid, 'GRID', 'OGRID', status)
         call copywcs(inwcsid, MAX_IDSTR, cpwcsid, status)

         if (.not.isdnull(xsky) .and. .not.isdnull(ysky) ) then
            call wcsimgsky(cpwcsid, cenpix(1), cenpix(2), xsky, ysky,
     &                     ineqx, 0, status)
            newzm(1) = 1.
            newzm(2) = 1.
            call wcslintrf(cpwcsid, cenpix, inszx, inszy, newzm, status)
            isremap = .TRUE.
         endif

         if ( rotstr.ne.' ' ) then
            ds = ' '
            call cnv_radec(ds, rotstr, dd, outrot, dar1, dar2, 1,
     &                     0, status)
            cenpix(1) = dble(inszx)/2. + 0.5
            cenpix(2) = dble(inszy)/2. + 0.5
            call wcsrotate(cpwcsid, cenpix, outrot, status)
            isremap = .TRUE.
         endif

         if ( outszx.gt.0 .and. outszy.gt.0 ) then
            cenpix(1) = dble(inszx)/2. + 0.5
            cenpix(2) = dble(inszy)/2. + 0.5
            if ( rebin.le.0.d0 ) rebin = 1.d0
            newzm(1) = rebin
            newzm(2) = rebin
            call wcslintrf(cpwcsid, cenpix, outszx, outszy, newzm, 
     &                     status)
            isremap = .TRUE.
         else if ( rebin.gt.0.d0 ) then
            cenpix(1) = dble(inszx)/2. + 0.5
            cenpix(2) = dble(inszy)/2. + 0.5
            newzm(1) = rebin
            newzm(2) = rebin
            outszx = NINT(dble(inszx)/rebin)
            outszy = NINT(dble(inszy)/rebin)
            call wcslintrf(cpwcsid, cenpix, outszx, outszy, newzm, 
     &                     status)
            isremap = .TRUE.
         endif
          
         if ( .not.isremap ) then
            call XWRITE(' No coordinates given for remap', 10)
            status = -1
            return
         endif

      else
         call upc(coorid)
c
c  If mapid is given as coorid, assume want to match its grid 
c
         if ( coorid(1:3).ne.'WCS' ) then
            call gheadi(coorid, 'SZX', coorszx, 0, status)
            call gheadi(coorid, 'SZY', coorszy, 0, status)
            if ( outszx.le.0 .or. outszy.le.0 ) then
               outszx = coorszx
               outszy = coorszy
            endif
         endif
         call copywcs(coorid, MAX_IDSTR, cpwcsid, status)
         if ( status.ne.0 ) then
            call xwrite(' Failed to copy wcsid for remapped image', 10)
            return
         endif
      endif
c
c  If no size specified, default to input map size
c
      if ( outszx.le.0 .or. outszy.le.0 ) then
         outszx = inszx
         outszy = inszy
      endif
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
         call xwrite(' Failed to allocate memory for remap', 10)
         return
      endif
c
c  Need to communicate to remap when integer map is being
c  run with the CONSERVE interpolation method (using params(1)...)
c
      if ( methstr.eq.'CONSERVE' .and. maptype.eq.'I' ) then
         numparm = 1
         params(1) = 1
      endif
c
c  Remap image to new coordinates
c
      call remapprep(methstr, numparm, params, inwcsid, memr(inptr),
     &               inszx, inszy, cpwcsid, coorszx, coorszy, 
     &               memr(outptr), outszx, outszy, status)
      if ( status.ne.0 ) then
         call xwrite(' Remap failure', 10)
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
         call wcsfrmcopy(exwcsid, 'GRID', 'OGRID', status)
         call copywcs(cpwcsid, MAX_IDSTR, cpexwcsid, status)
         call gheadi(outexpid, 'MAPPTR', -1, 1, status)
         call gheads(outexpid, 'MAPTYPE', 'R', 1, status)
         call gheads(outexpid, 'WCSID', ' ', 1, status)
         call gheads(outexpid, 'MAPCOPY', ' ', 1, status)
         call gheads(outmapid, 'EXMAPID', outexpid, 1, status)
         call mapalloc(outszx, outszy, outexpid, outexptr, status)
c
c  If CONSERVE method must clobber earlier setting to account for
c    exposure maps always being real
c
         if ( methstr.eq.'CONSERVE' ) params(1) = 0.

         call remapprep(methstr, numparm, params, exwcsid,
     &                  memr(inexptr), inszx, inszy, cpexwcsid, coorszx,
     &                  coorszy, memr(outexptr), outszx, outszy, status)
         if ( status.ne.0 ) then
            call xwrite(' Exposure map remap failure', 10)
            call mapfree(outexpid, frstat)
            goto 500
         endif
c
c  Update coordinate info
c
         call wcstohdr(cpexwcsid, outexpid, status)
         call gheads(outexpid, 'WCSID', cpexwcsid, 1, status)
c
c  Cleanup OGRID frame
c
         call wcsfrmdel(exwcsid, 'OGRID', status)
         call wcsfrmdel(cpexwcsid, 'OGRID', status)
         status = 0

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
      call gheadd(outmapid, 'DATAMIN', dd, 1, status)
      dd = newmax
      call gheadd(outmapid, 'DATAMAX', dd, 1, status)
c
c  Cleanup OGRID frame (if it exists)
c
      call wcsfrmdel(inwcsid, 'OGRID', status)
      call wcsfrmdel(cpwcsid, 'OGRID', status)
      status = 0
c
c  Free input image if in=out
c
  500 continue
      if ( ineqout ) then
         call ralloc(0, inszx, inszy, inptr, frstat)
         call wcsdecref(inwcsid)
         if ( inexpid.ne.' ' ) then
            call ralloc(0, inszx, inszy, inexptr, frstat)
            call wcsdecref(exwcsid)
         endif
      endif
   
      return
      end
