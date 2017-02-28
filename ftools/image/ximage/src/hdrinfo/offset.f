      SUBROUTINE offset(Cmdid, Status)
      implicit none
c
c  Calculate the angular offset for two positions
c
c O  status  i  error return
c
      INTEGER*4 Cmdid, Status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/pi.inc'
c
c  Local variables
c
      CHARACTER*(MAX_IDSTR) mapid, name, wcsid
      LOGICAL cursor, plot, isdisplay, isloaded, iscpmapid

      INTEGER argc, method, equinox
      INTEGER*4 symbol, color, lwidth
      REAL*4 rx, ry
      REAL*8 x1, y1, x2, y2, ximg, yimg
      REAL*8 ra1, dec1, ra2, dec2
      REAL*4 dumra(3), dumdec(3)
      character(20) x1buf, y1buf, x2buf, y2buf
      character(20) ra1buf, dec1buf, ra2buf, dec2buf
      INTEGER deglen, arclen, lenact
      character(30) degstr, arcstr
      character(40) system, proj, xlab, ylab, unit
      character(6) unit2
      REAL*4 csize
      REAL*8 epoch, dist
      LOGICAL first, found, readonly, global, ismap, isastbad

      method = 0
      cursor = .FALSE.
      x1buf = ' '
      y1buf = ' '
      x2buf = ' '
      y2buf = ' '
      ra1buf = ' '
      dec1buf = ' '
      ra2buf = ' '
      dec2buf = ' '
      plot = .FALSE.
      symbol = 8
      color = 16
      call get_color(color)
      csize = -1.0
      lwidth = 0

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'CURSOR',cursor,Status)
      CALL GPARS(Cmdid,'X1PIX',x1buf,Status)
      CALL GPARS(Cmdid,'Y1PIX',y1buf,Status)
      CALL GPARS(Cmdid,'X2PIX',x2buf,Status)
      CALL GPARS(Cmdid,'Y2PIX',y2buf,Status)
      CALL GPARS(Cmdid,'RA1',ra1buf,Status)
      CALL GPARS(Cmdid,'DEC1',dec1buf,Status)
      CALL GPARS(Cmdid,'RA2',ra2buf,Status)
      CALL GPARS(Cmdid,'DEC2',dec2buf,Status)
      CALL GPARL(Cmdid,'PLOT',plot,Status)
      CALL GPARI(Cmdid,'SYMBOL',symbol,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      if ( status.ne.0 ) return
c
      mapid='CUR'
      IF ( cursor ) THEN
         IF ( .NOT.isdisplay() ) THEN
            CALL XWRITE(' Please display an image first',10)
            status=1
            RETURN
         ENDIF
         mapid='DIS'
      ENDIF
      ismap = isloaded(mapid)
      if ( ismap ) then
         call mapname(mapid, name)
         write(ZWRite,'(2a)') ' Conversion based on ', 
     &                            name(:lenact(name))
         call xwrite(ZWRite, 10)
      endif

      if ( plot ) then
         if ( .not.ismap ) then
            call XWRITE(' No map: Cannot plot points', 10)
            plot = .FALSE.
         elseif ( .not.isdisplay() ) then
            call XWRITE(' No display: Cannot plot points', 10)
            plot = .FALSE.
         elseif ( .not.iscpmapid('DIS',mapid) ) then
            call XWRITE(
     &        ' Current map is not display map: Cannot plot points', 10)
            plot = .FALSE.
         endif
      endif

      if ( x1buf.ne.' ' .and. y1buf.ne.' ' .and. 
     &     x2buf.ne.' ' .and. y2buf.ne.' ' ) then
         method = 1
      elseif ( ra1buf.ne.' ' .and. dec1buf.ne.' ' .and.
     &         ra2buf.ne.' ' .and. dec2buf.ne.' ' ) then
         method = 2
      elseif ( cursor ) then
         method = 1
      else
         call xwrite(' Two positions not specified', 10)
         status = -1
         return
      endif

      if ( .not.ismap .and. method.ne.2 ) then
         call xwrite(' Only RA/Dec inputs usable without map', 10)
         status = -1
         return
      endif
c
      readonly = .FALSE.
      global = .FALSE.
      call tclunset('offset', global, Status)

      wcsid = ' '
      if ( ismap ) then
         CALL gheads(mapid, 'WCSID', wcsid, 0, status)
      endif

c get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)
c
c return x#,y# either from cursor or as input parameters,
c in unit of image frame (es. if image is 256X256, x1,y1
c run from 1 to 256)
c
      if ( method.eq.1 ) then
         if ( cursor ) then
            call XWRITE(' Enter pixel location', 10)
            call inxypix(cursor,rx,ry)
            x1 = rx
            y1 = ry
            call XWRITE(' Enter another pixel location', 10)
            call inxypix(cursor,rx,ry)
            x2 = rx
            y2 = ry
         else
            call strnum(x1buf, 4, x1, status)
            call strnum(y1buf, 4, y1, status)
            call strnum(x2buf, 4, x2, status)
            call strnum(y2buf, 4, y2, status)
         endif

         call wcsimgpix(wcsid, ximg, yimg, x1, y1, 0, status)
         write (ZWRite,*) ' x1(img) = ', ximg, ' y1(img) = ', yimg
         call XWRITE(ZWRite, 15)

         call wcsimgsky(wcsid, ximg, yimg, ra1, dec1, equinox, 1,
     &                  status)

         call wcsimgpix(wcsid, ximg, yimg, x2, y2, 0, status)
         write (ZWRite,*) ' x2(img) = ', ximg, ' y2(img) = ', yimg
         call XWRITE(ZWRite, 15)

         call wcsimgsky(wcsid, ximg, yimg, ra2, dec2, equinox, 1,
     &                  status)

      elseif ( method.eq.2 ) then

         call cnv_radec(ra1buf, dec1buf, ra1, dec1, dumra,
     &                  dumdec, 1, 0, status)
         call cnv_radec(ra2buf, dec2buf, ra2, dec2, dumra,
     &                  dumdec, 1, 0, status)

         if ( ismap ) then
            call wcsimgsky(wcsid, ximg, yimg, ra1, dec1, equinox, 0,
     &                     status)
            write (ZWRite,*) ' x1(img) = ', ximg, ' y1(img) = ', yimg
            call XWRITE(ZWRite, 15)
            call wcsimgpix(wcsid, ximg, yimg, x1, y1, 1, status)

            call wcsimgsky(wcsid, ximg, yimg, ra2, dec2, equinox, 0,
     &                     status)
            write (ZWRite,*) ' x2(img) = ', ximg, ' y2(img) = ', yimg
            call XWRITE(ZWRite, 15)
            call wcsimgpix(wcsid, ximg, yimg, x2, y2, 1, status)
        endif

      endif

      if ( plot ) then
         call PGSAVE
         call sym_pgstate(color, csize, lwidth)
         rx = x1
         ry = y1
         call PGPT(1,rx,ry,symbol)
         rx = x2
         ry = y2
         call PGPT(1,rx,ry,symbol)
         call PGUNSA
      endif
c
c writing on the screen
      CALL XWRITE(' ',10)
      if ( .not.ismap ) mapid = ' '
      call write_radec(mapid,ra1,dec1,equinox)
      call xwrite(' ', 10)
      call write_radec(mapid,ra2,dec2,equinox)

      first = .TRUE.
      if ( wcsid.eq.' ' ) then
         call xwarn(' Using direct angle calculation, not '//
     &              'shortest path', 10)
         call angle(ra1,dec1,ra2,dec2,1.d0,dist,first,found)
         unit = 'deg'
      else
         call wcsdist(wcsid, 'CUR', ra1, dec1, ra2, dec2, dist,
     &                status)
         call wcsfrminfo(wcsid, system, proj, xlab, ylab, unit, epoch)
      endif

      status = 0
      call tclvars('offset(unit)',unit,readonly,global,status)
      call tclvard('offset(value)',dist,readonly,global,status)
      if ( unit.eq.'deg' ) then
         call tclvard('offset(deg)',dist,readonly,global,status)
      endif
  
      call xwrite(' ', 10)
      call xdstr(dist, 14, degstr, deglen)
      if ( isastbad(dist) ) then
         ZWRite = '    Offset  : UNDEF'
         dist = 0.
      else if ( unit.eq.'deg' ) then
         if ( dist.gt.1. ) then
            write (ZWRite,'(3a)') '    Offset  : ', degstr(:deglen), 
     &                                            ' degrees'
         else
            if ( dist*60. .gt. 1.0 ) then
               call xdstr(dist*60.d0, -1, arcstr, arclen)
               unit2 = 'arcmin'
            else
               call xdstr(dist*3600.d0, -1, arcstr, arclen)
               unit2 = 'arcsec'
            endif
            write (ZWRite,'(2a,1x,4a)') '    Offset  : ', 
     &              arcstr(:arclen), unit2, '  (', degstr(:deglen), 
     &              ' degrees)'
         endif
      else
         write (ZWRite,'(2a,1x,a)') '    Offset  : ', degstr(:deglen), 
     &                                            unit(:LENACT(unit))
      endif

      call xwrite(ZWRite, 10)
      call xwrite(' ', 10)

      RETURN
      END
