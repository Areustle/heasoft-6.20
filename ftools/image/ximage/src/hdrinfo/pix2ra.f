      SUBROUTINE pix2ra(Cmdid, Status)
      implicit none
c
c return ra and dec for a given xpi and ypix
c
c I  cmdid   (i)  Command id
c O  status  (i)  Error flag (0=OK)
c
      INTEGER*4 Cmdid, Status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 argc, equinox
      REAL*4 xpix, ypix, xpos , ypos
      REAL*8 alpout , decout
      real*8 dblx, dbly
c
      CHARACTER*(MAX_IDSTR) mapid, name, wcsid
      LOGICAL cursor, isdisplay, isloaded, iscpmapid

      INTEGER*4 symbol, color, lwidth, lenact
      character(20) xbuf, ybuf
      REAL*4 csize
      REAL*8 dd

      DATA xpix/0./, ypix/0./
      SAVE xpix, ypix

      cursor = .FALSE.
      xbuf = ' '
      ybuf = ' '
      symbol = 8
      color = 16
      call get_color(color)
      csize = -1.0
      lwidth = 0

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'CURSOR',cursor,Status)
      CALL GPARS(Cmdid,'XPIX',xbuf,Status)
      CALL GPARS(Cmdid,'YPIX',ybuf,Status)
      CALL GPARI(Cmdid,'SYMBOL',symbol,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      if ( status.ne.0 ) return
c
      IF ( cursor ) THEN
         IF ( .NOT.isdisplay() ) THEN
            CALL XWRITE(' Please display an image first',10)
            status=1
            RETURN
         ENDIF
         mapid='DIS'
      ELSE
         mapid='CUR'
      ENDIF
      call mapname(mapid, name)
      write(ZWRite,'(2a)') ' Conversion based on ', name(:lenact(name))
      call xwrite(ZWRite, 10)

      if ( .not. isloaded(mapid) ) then
         call XWRITE (' Image not loaded', 10)
         status=1
         RETURN
      endif
c
      CALL gheads(mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
          call xwrite(' No wcsid available for conversion', 5)
          status = -1
          return
      endif
c
c return xpos and ypos either from cursor or as input parameters,
c in unit of image frame (es. if image is 256X256, xpos and ypos
c run from 1 to 256)
c
      if ( xbuf.ne.' ' .and. ybuf.ne.' ' ) then
         call strnum(xbuf, 4, dd, status)
         xpix = dd
         call strnum(ybuf, 4, dd, status)
         ypix = dd
      else
         call XWRITE(' Enter pixel location', 10)
         call inxypix(cursor,xpix,ypix)
      endif

      if ( isdisplay() ) then
         if ( iscpmapid('DIS',mapid) ) then
            call jrnlab(xpix,ypix,' ',' ',-1,-1.0,-1,symbol,color,
     &                  csize,lwidth,' ',0.0)
         else
            call xwrite(' Point not plotted: '//
     &                   'Current map is not display map', 10)
         endif
      endif

      call imgpix(mapid,xpix,ypix,xpos,ypos,2,status)
      write (ZWRite,*) ' x(img) = ', xpos, ' y(img) = ', ypos
      call XWRITE(ZWRite, 15)
      dblx = xpos
      dbly = ypos

c get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)

      call wcsimgsky(wcsid, dblx, dbly, alpout, decout, equinox, 1,
     &               status)
c
c writing on the screen
      call xwrite(' ',10)
      call write_radec(mapid,alpout,decout,equinox)
      call xwrite(' ',10)

      status = 0
      RETURN
      END
