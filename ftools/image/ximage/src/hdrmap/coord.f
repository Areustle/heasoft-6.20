      SUBROUTINE coord(Cmdid, Mapid, Status)
      implicit none
c
c  Return values for specified coordinate in current map
c
c I  cmdid   (i)  Command id
c I  map     (r)  Image map
c I  szx/y   (i)  Size of map
c I  mapid   (s)  Map id string
c O  status  (i)  Error flag (0=OK)
c
      INTEGER*4 Cmdid, Status
      CHARACTER*(*) Mapid

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/dynmem.inc'
c
c  Local variables
c
      integer*4 inequinox, equinox, mapptr, szx, szy, argc, i, j, di
      real*4 rxpix, rypix, dumra(3), dumdec(3), dr
      real*8 xpix, ypix, ximg, yimg, xsky, ysky
      real*8 ra, dec, lii, bii, dequinox
      integer PIXCNV, RACNV, IMGCNV, cnvmode
      parameter (PIXCNV = 1, RACNV = 2, IMGCNV = 3)
c
      character*(MAX_IDSTR) coorid, wcsid
      character(1) maptype
      integer lenact
      logical cursor, isdisplay, isloaded, ismouse, iscpmapid, isrnull

      character(40) xmstr, ymstr, xpstr, ypstr, rastr, decstr
      character(40) system, proj, unit, xlab, ylab, xsfmt, ysfmt
      real*8 epoch, DNULL
      logical readonly, global, ISDNULL

      coorid = ' '
      wcsid = ' '
      cursor = .FALSE.
      xpstr = ' '
      ypstr = ' '
      xmstr = ' '
      ymstr = ' '
      rastr = ' '
      decstr = ' '
      xsky = DNULL()
      ysky = DNULL()
      inequinox = 0

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'MAPID',coorid,Status)
      CALL GPARS(Cmdid,'WCSID',wcsid,Status)
      CALL GPARL(Cmdid,'CURSOR',cursor,Status)
      CALL GPARS(Cmdid,'XPIX',xpstr,Status)
      CALL GPARS(Cmdid,'YPIX',ypstr,Status)
      CALL GPARS(Cmdid,'XIMG',xmstr,Status)
      CALL GPARS(Cmdid,'YIMG',ymstr,Status)
      CALL GPARD(Cmdid,'XSKY',xsky,Status)
      CALL GPARD(Cmdid,'YSKY',ysky,Status)
      CALL GPARS(Cmdid,'RA',rastr,Status)
      CALL GPARS(Cmdid,'DEC',decstr,Status)
      CALL GPARI(Cmdid,'EQUINOX',inequinox,Status)
      if ( status.ne.0 ) return
c
      if ( wcsid.eq.' ' .and. coorid.eq.' ' ) coorid = Mapid
      if ( .not. isloaded(coorid) ) coorid = ' '
      if ( wcsid.eq.' ' .and. coorid.eq.' ' ) then
         call XWRITE (' Image not loaded', 10)
         status = -1
         RETURN
      endif

      if ( cursor ) then
         if ( .not.isdisplay() ) then
            CALL XWRITE(' Please display an image first',10)
            status = -1
         elseif ( .not.ismouse() ) then
            CALL XWRITE(' CURSOR unavailable on this device',10)
            status = -1
         elseif ( .not.iscpmapid('DIS',Mapid) ) then
            write(ZWRite,'(3a)') ' Map: ',Mapid(:LENACT(Mapid)),
     &                           ' is not display map'
            call xwrite(ZWRite, 10)
            status = -1
         endif
         if ( status.ne.0 ) return
      endif
c
c  Retrieve coordinate info of map
c  If mapid,wcsid both defined prefer mapid unless not loaded
c
      
      if ( coorid.ne.' ' ) then
         call gheads(coorid, 'WCSID', wcsid, 0, status)
         call gheads(coorid, 'MAPTYPE', maptype, 0, status)
         call gheadi(coorid, 'MAPPTR', mapptr, 0, status)
         call gheadi(coorid, 'SZX', szx, 0, status)
         call gheadi(coorid, 'SZY', szy, 0, status)
      else
         szx = 0
         szy = 0
      endif
      if ( status.ne.0 ) return

      call wcsfrminfo(wcsid, system, proj, xlab, ylab, unit, epoch)
c
c  Get numeric values for entered coordinates
c
      cnvmode = 0
      if ( cursor ) then

         cnvmode = PIXCNV
         call XWRITE(' Select pixel location', 10)
         call inxypix(cursor,rxpix,rypix)
         xpix = rxpix
         ypix = rypix

      elseif ( xpstr.ne.' ' .and. ypstr.ne.' ' ) then

         cnvmode = PIXCNV
         call strnum(xpstr, 4, xpix, status)
         call strnum(ypstr, 4, ypix, status)

      elseif ( xmstr.ne.' ' .and. ymstr.ne.' ' ) then

         cnvmode = IMGCNV
         call strnum(xmstr, 4, ximg, status)
         call strnum(ymstr, 4, yimg, status)

      elseif  ( rastr.ne.' ' .and. decstr.ne.' ' ) then

         cnvmode = RACNV
         call cnv_radec(rastr, decstr, xsky, ysky, dumra,
     &                  dumdec, 1, 0, status)
         if ( status.ne.0 ) return

      elseif ( .not.ISDNULL(xsky) .and. .not.ISDNULL(ysky) ) then
         cnvmode = RACNV
      else

         call xwrite(' No coordinates entered', 10)
         status = -1
         return

      endif

c  Get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)
      if ( inequinox.eq.0 ) inequinox = equinox
c
c  Perform conversion
c
      if ( cnvmode.eq.PIXCNV ) then

         call wcsimgpix(wcsid,ximg,yimg,xpix,ypix,0,status)
         call wcsimgsky(wcsid,ximg,yimg,xsky,ysky,equinox,1,status)

      elseif ( cnvmode.eq.IMGCNV ) then

         call wcsimgpix(wcsid,ximg,yimg,xpix,ypix,1,status)
         call wcsimgsky(wcsid,ximg,yimg,xsky,ysky,equinox,1,status)

      elseif ( cnvmode.eq.RACNV ) then

         call wcsimgsky(wcsid,ximg,yimg,xsky,ysky,inequinox,0,status)
         call wcsimgpix(wcsid,ximg,yimg,xpix,ypix,1,status)
         if ( inequinox.ne.equinox ) then
            call wcsimgsky(wcsid,ximg,yimg,xsky,ysky,equinox,1,status)
         endif

      else

         call xwrite(' Bad coord conversion mode', 10)
         status = -1
         return

      endif
      call wcsformat(wcsid, xsky, ysky, xsfmt, ysfmt)
c
c  Find galactic coordinates
c
      dequinox = equinox
      call wcsskysky(system, epoch, xsky, ysky, 'GALACTIC', 2000.d0,
     &               lii, bii, status)

      if ( xlab.eq.'RA' ) then
         ra = xsky
         dec = ysky
      else
         call wcsskysky(system, epoch, xsky, ysky, 'EQUATORIAL', 
     &                  dequinox, ra, dec, status)
      endif
c
c  Export values to tcl
c
      readonly = .FALSE.
      global = .FALSE.
      call tclvard('coord(ra)', ra, readonly, global, status)
      call tclvard('coord(dec)', dec, readonly, global, status)
      call tclvars('coord(system)', system, readonly, global, status)
      call tclvars('coord(unit)', unit, readonly, global, status)
      call tclvard('coord(xsky)', xsky, readonly, global, status)
      call tclvard('coord(ysky)', ysky, readonly, global, status)
      call tclvars('coord(xsfmt)', xsfmt, readonly, global, status)
      call tclvars('coord(ysfmt)', ysfmt, readonly, global, status)
      call tclvari('coord(equinox)', equinox, readonly, global, status)
      call tclvard('coord(lii)', lii, readonly, global, status)
      call tclvard('coord(bii)', bii, readonly, global, status)
      call tclvard('coord(xpix)', xpix, readonly, global, status)
      call tclvard('coord(ypix)', ypix, readonly, global, status)
      call tclvard('coord(ximg)', ximg, readonly, global, status)
      call tclvard('coord(yimg)', yimg, readonly, global, status)
      i = NINT(ximg)
      j = NINT(yimg)
      if ( i.ge.1 .and. i.le.Szx .and. j.ge.1 .and. j.le.Szy ) then
         call mapvalget(memr(mapptr), szx, szy, i, j, dr)
         if ( isrnull(dr) ) then
            call tclvars('coord(value)', 'NULL', readonly, global, 
     &                   status)
         elseif ( maptype.eq.'I' ) then
            di = dr
            call tclvari('coord(value)', di, readonly, global, status)
         else
            call tclvarr('coord(value)', dr, readonly, global, 
     &                   status)
         endif
      else
         call tclvars('coord(value)', ' ', readonly, global, status)
      endif

      status = 0
      RETURN
      END
