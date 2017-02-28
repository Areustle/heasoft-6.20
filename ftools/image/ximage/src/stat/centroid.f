      SUBROUTINE CENTROID(Cmdid, Map, Szx, Szy, Mapid, Status)
      IMPLICIT NONE
c
c  Calculate centroid
c
c  I  cmdid   (i)  Command id
c  I  map     (r)  Image map
c  I  szx/y   (i)  Size of map
c  I  mapid   (s)  Map id string
c  O  status  (i)  Error flag (0=OK)
c
      integer*4 Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/dynmem.inc'
c
c  Local variables
c
      CHARACTER*(MAX_IDSTR) wcsid
      REAL*4 bxcen , bycen, boxrad, boxwid
      REAL*4 dbxcen, dbycen, dumbox, xold, yold, xdif, ydif
      REAL*4 xpix, ypix, xnew, ynew, xout, yout
      REAL*8 ra, dec, xcen, ycen, ximg, yimg, zmx, zmy, dd
      INTEGER*4 argc, equinox, idelim, iflag, n, nret, di
      real*8 pixsize(2)
      integer*4 bufsz, ibox, tchat, lchat, xlen, ylen
      character(40) descr, xcstr, ycstr, pixradstr, radstr, xstr, ystr
      integer*4 p_bxbuf, p_dpsw, p_dpsdd, p_dpsder, frstat

      LOGICAL cursor, hist, deriv, good, readonly, global
      INTEGER symbol, boxcolor, color, lwidth, csize, lenact
      LOGICAL isdisplay, ismouse, isloaded, iscpmapid
      character(10) font
      character(1) maptype

      DATA dbxcen /0./, dbycen /0./, dumbox /0./
      SAVE dbxcen, dbycen, dumbox
c defaults and inits.
      xcstr = ' '
      ycstr = ' '
      radstr = ' '
      pixradstr = ' '
      cursor = .FALSE.
      hist = .FALSE.
      deriv = .FALSE.
      good = .TRUE.
      color = 16
      call get_color(color)
      boxcolor = 3
      symbol = 8
      lwidth = 2
      csize = -1.0
      font = ' '
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'XPIX',xcstr,Status)
      CALL GPARS(Cmdid,'YPIX',ycstr,Status)
      CALL GPARS(Cmdid,'BOXRADIUS',radstr,Status)
      CALL GPARS(Cmdid,'PIXBOXRAD',pixradstr,Status)
      CALL GPARL(Cmdid,'CURSOR',cursor,Status)
      CALL GPARL(Cmdid,'HIST',hist,Status)
      CALL GPARL(Cmdid,'DERIV',deriv,Status)
      CALL GPARI(Cmdid,'SYMBOL',symbol,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      if ( status.ne.0 ) return

      write(ZWRite,'(2a)') ' Using ', Mapid(:lenact(Mapid))
      call xwrite(ZWRite, 10)

      if ( .not.isloaded(Mapid) ) then
         call XWRITE(' Image not loaded', 10)
         Status = -1
         return
      endif
      call gheads(Mapid, 'MAPTYPE', maptype, 0, status)

      call xgtcht(tchat, lchat)
 
      IF ( cursor ) THEN
         IF ( .not. isdisplay() ) THEN
            CALL XWARN(
     &         'No image displayed, CURSOR qualifier disallowed',5)
            RETURN
         ELSE IF ( .not. ismouse() ) THEN
            CALL XWARN('Device does not allow cursor input',10)
            RETURN
         ELSE IF ( .not.iscpmapid('DIS',Mapid) ) THEN
            call xwrite(' Current map is not display map', 10)
            call maprcmd(1)
            RETURN
         ENDIF
      ENDIF
c
c Assign values from Header
c
      wcsid = ' '
      call gheads(Mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
         call xwrite(' No wcsid defined', 10)
         status = -1
         return
      endif

c Get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)

      call gheadd(Mapid, 'CDELT1', pixsize(1), 0, status)
      call gheadd(Mapid, 'CDELT2', pixsize(2), 0, status)
      call get_refram(Mapid,di,di,zmx,zmy,xcen,ycen,status)

      if ( xcstr.ne.' ' .and. ycstr.ne.' ' ) then
         call strnum(xcstr,4,dd,status)
         dbxcen = dd
         call strnum(ycstr,4,dd,status)
         dbycen = dd
      else
         call XWRITE (' ',5)
         if ( cursor ) then
            call XWRITE (' Select center of box',5)
         else
            call XWRITE (' Enter box center',5)
         endif
         call INXYPIX (cursor, dbxcen, dbycen)
      endif

      call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,dbxcen,dbycen,
     &               bxcen,bycen,2)
         
      if ( pixradstr.ne.' ' ) then
         call strnum(pixradstr,4,dd,status)
         boxrad = dd
      elseif ( radstr.ne.' ' ) then
         call strnum(radstr,4,dd,status)
         dumbox = dd
         boxrad = dumbox/(abs(pixsize(1))*60.)
      elseif (cursor) then
         call XWRITE (' ',5)
         call XWRITE (' Select edge of box',5)
         call INXYPIX (cursor, xpix, ypix)
         boxrad = MAX(abs(dbxcen-xpix), abs(dbycen-ypix))/zmx
      else
         idelim = 0
         iflag = 0
         descr = ' Enter box radius (arcmin)'
         n = 1
         call XPXGTR (ZSTring, ZPArse, n, descr, n, 
     &                dumbox, nret, iflag, idelim)
         boxrad = dumbox/(abs(pixsize(1))*60.)
      endif
c
c  Force box center to be between pixels, and box radius to be integer
c   number of image pixels
c
      bxcen = float(int(bxcen)) + 0.5
      bycen = float(int(bycen)) + 0.5
      ibox = MAX(2,nint(boxrad)*2)
      boxrad = float(ibox)/2.0
      dumbox = boxrad*(abs(pixsize(1))*60.)

      call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xpix,ypix,
     &               bxcen,bycen,1)
      write(ZWRite,*) ' Box center (pix,img)', xpix, ypix, bxcen, bycen
      call XWRITE(ZWRite, 15)
      write(ZWRite,*) ' Box radius (pix,img,arcmin)', 
     &                 boxrad*zmx,boxrad, dumbox
      call XWRITE(ZWRite, 15)
c
c  Check box bounds
c
      if ( int(bxcen + boxrad - 0.5).gt.Szx .or. 
     &     int(bycen + boxrad - 0.5).gt.Szy .or.
     &     int(bxcen - boxrad + 0.5).lt.1 .or.
     &     int(bycen - boxrad + 0.5).lt.1 ) then
         call xwrite(' Selected box is out of image bounds', 5)
         status = -1
         goto 500
      endif
c
c  Plot start box
c
      if ( tchat.ge.15 .and. 
     &     iscpmapid('DIS',Mapid) .and. isdisplay() ) then
         boxwid = boxrad*zmx*2.0
         call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xpix,ypix,
     &                  bxcen,bycen,1)
         call jrnbox(xpix,ypix,boxwid,boxwid,0.,color,lwidth,-1)
      endif
c
c  Center box on "max" pixel, determined by histogram, rebinning, or
c   pixel with maximum value.
c
      if ( hist ) then
         call histomax (Map,Szx,Szy,bxcen,bycen,boxrad,xout,yout)
         write(ZWRite,*) ' Histogram max:',xout, yout
         call XWRITE(ZWRite,15)
      else
         call ralloc(1, ibox, ibox, p_bxbuf, status)
         if ( status.ne.0 ) goto 500
         call rebinmax (Map,Szx,Szy,memr(p_bxbuf),ibox,bxcen,bycen,
     &                  xout,yout, status)
         call ralloc(0, ibox, ibox, p_bxbuf, frstat)
         if ( status.ne.0 ) goto 500
         write(ZWRite,*) ' Rebin max:',xout, yout
         call XWRITE(ZWRite,15)
      endif
c
c  Check new box bounds
c
      if ( int(xout + boxrad - 0.5).gt.Szx .or. 
     &     int(yout + boxrad - 0.5).gt.Szy .or.
     &     int(xout - boxrad + 0.5).lt.1 .or.
     &     int(yout - boxrad + 0.5).lt.1 ) then
         call xwrite(
     &    ' Ignoring max selection as it pushes box out of bounds', 15)
         xout = bxcen
         yout = bycen
      else
c
c  Plot box with max at center
c
         if ( tchat.ge.20 .and. 
     &        iscpmapid('DIS',Mapid) .and. isdisplay() ) then
            call xwrite(' Dotted line box is centered on max', 20)
            boxwid = boxrad*zmx*2.0
            call jrnbox(xout,yout,boxwid,boxwid,0.,color,lwidth,4)
         endif

      endif
c
c  Do calculation using new center
c
      if ( deriv ) then
         call xwrite(' Using derivative of partial sums method', 15)
         bxcen = xout
         bycen = yout
         bufsz = INT(MAX(boxrad*2., 4.))
         call ralloc(1, bufsz, 1, p_dpsw, status)
         if ( status.ne.0 ) goto 500
         call ralloc(1, bufsz, 1, p_dpsdd, status)
         if ( status.ne.0 ) goto 500
         call ralloc(1, bufsz, 1, p_dpsder, status)
         if ( status.ne.0 ) goto 500
         call dps_cntrd (Map,Szx,Szy,memr(p_dpsw),memr(p_dpsdd),
     &                   memr(p_dpsder),bufsz,bxcen,bycen,boxrad,
     &                   xout,yout,status)
         call ralloc(0, bufsz, 1, p_dpsw, frstat)
         call ralloc(0, bufsz, 1, p_dpsdd, frstat)
         call ralloc(0, bufsz, 1, p_dpsder, frstat)
         if ( status.ne.0 ) goto 500
      else
         call xwrite(' Using barycenter method', 15)
c
c       Barycenter routine uses integer at edge convention
c
         if ( maptype.ne.'I' ) then
            call xwarn('Non-integer map, rounding in barycenter',5)
         endif
         if ( tchat.ge.15 ) then
            xold = xout
            yold = yout
c
c  Note: barycenter is originally for detect.  The coordinate
c  convention it uses assumes the left edge of the pixel is 
c  its integer value.  i.e. |_|_|_|
c                           1 2 3 
c
c  Centroid wants the center of the pixel to be the integer value
c                           |_|_|_|
c                            1 2 3
c  There is a 0.5 adjustment to translate between the conventions
c
            xold = xold + 0.5
            yold = yold + 0.5
            call barycenter(Map,Szx,Szy,xold,yold,ibox,good)
            if ( .not.good ) goto 400
            xold = xold - 0.5
            yold = yold - 0.5
            call offbary(Map,Szx,Szy,xold,yold,ibox,xdif,ydif,
     &                   Status)
            write(ZWRite,*) ' Old barycenter ', xold, yold
            call xwrite(ZWRite, 15)
            write(ZWRite,*) '   Offset       ', xdif, ydif
            call xwrite(ZWRite, 15)
         endif
  400    continue
         call trybary(Map,Szx,Szy,xout,yout,ibox,status)
         if ( status.ne.0 ) then
            call xwrite(' Barycenter calculation failed', 10)
            goto 500
         endif
         write(ZWRite,*) ' New barycenter ', xout, yout
         call xwrite(ZWRite, 15)
         call offbary(Map,Szx,Szy,xout,yout,ibox,xdif,ydif,
     &                Status)
         write(ZWRite,*) '   Offset       ', xdif, ydif
         call xwrite(ZWRite, 15)
      endif

      call XWRITE (' ',5)
      call XWRITE (' Calculated centroid:',5)
      
      ximg = xout
      yimg = yout
      call wcsimgsky(wcsid, ximg, yimg, ra, dec, equinox, 1, status)

      call write_radec (mapid, ra, dec, equinox)

      call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xnew,ynew,
     &               xout,yout,1)

      dd = xnew
      call xdstr(dd,-1,xstr,xlen)
      dd = ynew
      call xdstr(dd,-1,ystr,ylen)
      write(ZWRite,'(4x,2a,1x,a)') 'X/Ypix = ',xstr(:xlen),ystr(:ylen)
      call XWRITE(ZWRite,10)
      dd = xout
      call xdstr(dd,-1,xstr,xlen)
      dd = yout
      call xdstr(dd,-1,ystr,ylen)
      write(ZWRite,'(4x,2a,1x,a)') 'X/Yimg = ',xstr(:xlen),ystr(:ylen)
      call XWRITE(ZWRite,15)
      call XWRITE(' ',10)
      if ( iscpmapid('DIS',Mapid) .and. isdisplay() ) then
         call jrnlab(xnew,ynew,' ',' ',-1,-1.0,-1,symbol,color,
     &               csize,lwidth,font,0.0)
         if ( tchat.ge.20 ) then
            boxwid = boxrad*zmx*2.0
            call jrnbox(xnew,ynew,boxwid,boxwid,0.,boxcolor,lwidth,-1)
         endif
      endif
c
c  Export values to tcl
c
      readonly = .FALSE.
      global = .FALSE.
      call tclvard('centroid(ra)', ra, readonly, global, status)
      call tclvard('centroid(dec)', dec, readonly, global, status)
      call tclvari('centroid(equinox)', equinox, readonly, global, 
     &             status)
      call tclvarr('centroid(xpix)', xnew, readonly, global, status)
      call tclvarr('centroid(ypix)', ynew, readonly, global, status)
      call tclvarr('centroid(ximg)', xout, readonly, global, status)
      call tclvarr('centroid(yimg)', yout, readonly, global, status)

      status = 0
  500 continue

      RETURN
      END
