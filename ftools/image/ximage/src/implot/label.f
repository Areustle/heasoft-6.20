      SUBROUTINE LABEL(Cmdid, Status)
      implicit none
c
c  Write labels on the image (Using cursor, viewport coords, or 
c   detector coords)
c
c  I  cmdid    (i)  Command id
c  O  status   (i)  Error flag (0=OK)
c
      integer Cmdid, Status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      integer idelim, iflag, n, nret

      character(128) text
      REAL*4 xpix , ypix , vpx, vpy, csize, symcsize, spaces
      INTEGER*4 argc, i
      INTEGER*4 symbol , color , lwidth, symcolor, symlwidth
      REAL*4 rrd(2)
      character(80) descr(2)

      character(40) rastr, decstr, xpstr, ypstr
      character*(MAX_IDSTR) mapid, wcsid
      real*4 ra3(3), dec3(3)
      real*8 ra, dec, dd
      real*8 ximg, yimg, dx, dy
      integer equinox, di, xplen, yplen

      character(10) just, font
      REAL*4 angle, rjust
      INTEGER njust
      PARAMETER (njust=3)
      character(6) justopts(njust)
      REAL*4 justvals(njust)
      LOGICAL cursor, clip, plot, isdisplay
      real*4 xmin, xmax, ymin, ymax
c
      DATA justopts /'LEFT', 'CENTER', 'RIGHT'/
      DATA justvals /0.0, 0.5, 1.0/
c
      cursor = .FALSE.
      text = ' '
c     color = 10
      color = 16
      call get_color(color)
      csize = 0.8
      lwidth = 1
      symbol = -999
      symcolor = -1
      symcsize = -1.0
      symlwidth = 0
      font = ' '
      angle = 0.
      just = 'LEFT'
      clip = .FALSE.

      mapid = 'DIS'
c
      xpstr = ' '
      ypstr = ' '
      vpx = -1.0
      vpy = -1.0
      rastr = ' '
      decstr = ' '
c
c set qualifier
c symbol symbol position
c color  color tobe used
c size_of_symbol
c width_of_symbol
c
      status = 0

      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'TEXT',text,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      CALL GPARI(Cmdid,'SYMBOL',symbol,Status)
      CALL GPARI(Cmdid,'SYMCOLOR',symcolor,Status)
      CALL GPARR(Cmdid,'SYMCSIZE',symcsize,Status)
      CALL GPARI(Cmdid,'SYMLWIDTH',symlwidth,Status)
      CALL GPARS(Cmdid,'XPIX',xpstr,Status)
      CALL GPARS(Cmdid,'YPIX',ypstr,Status)
      CALL GPARL(Cmdid,'CURSOR',cursor,Status)
      CALL GPARR(Cmdid,'ANGLE',angle,Status)
      CALL GPARS(Cmdid,'JUST',just,Status)
      CALL GPARR(Cmdid,'VX',vpx,Status)
      CALL GPARR(Cmdid,'VY',vpy,Status)
      CALL GPARS(Cmdid,'RA',rastr,Status)
      CALL GPARS(Cmdid,'DEC',decstr,Status)
      CALL GPARL(Cmdid,'CLIP',clip,Status)
      if ( status.ne.0 ) return

      if ( .not.isdisplay() ) then
         call XWARN ('No display',5)
         status = -1
         return
      endif
      if ( text.eq.' ' .and. symbol.eq.-999 ) then
         call xwrite(' No text or symbol to plot', 10)
         status = -1
         return
      endif
c
c Check qualifiers
c
c  Justification
c
      call matchopts (just, justopts, njust, i, status)
      if ( status.eq.0 ) then
         just = justopts(i)(1:1)
         rjust = justvals(i)
      else
         call XWRITE (' Invalid justification: Defaulting to LEFT...'
     &                , 10)
         just = 'L'
         rjust = 0.0
         status = 0
      endif
c
c  Text state
c
      call PGSAVE
      call text_pgstate(color, csize, lwidth, font)
c
c Get Ximage equinox
c
      call tclresi("set default(equinox)", equinox, status)
c
c Get label coordinates
c
      if ( cursor ) then

         call XWRITE (' Click desired label location', 10)
         call inxypix(.TRUE., xpix, ypix)
         dd = xpix
         call xdstr(dd, -1, xpstr, xplen)
         dd = ypix
         call xdstr(dd, -1, ypstr, yplen)
c
c  Modify parameters for journalling
c
         call sparl(Cmdid,'CURSOR',.FALSE.,status)
         call spars(Cmdid,'XPIX',xpstr,status)
         call spars(Cmdid,'YPIX',ypstr,status)

      else if ( vpx.ge.0.0 .and. vpy.ge.0.0 ) then

         call ndc2wor (vpx, vpy, xpix, ypix)
         clip = .FALSE.

      else if ( rastr.ne.' ' .and. decstr.ne.' ' ) then

         call cnv_radec(rastr, decstr, ra, dec, ra3, dec3, 1, 0, status)
         wcsid = ' '
         call gheads(mapid, 'WCSID', wcsid, 0, status)
         if ( wcsid.eq.' ' ) then
            call xaerror(' No wcsid to convert coordinates', 5)
            status = -1
            return
         endif
         call wcsimgsky(wcsid, ximg, yimg, ra, dec, equinox, 0,
     &                  status)
         call wcsimgpix(wcsid, ximg, yimg, dx, dy, 1, status)
         xpix = dx
         ypix = dy

      else if ( xpstr.ne.' ' .and. ypstr.ne.' ' ) then

         call strnum(xpstr,4,dd,status)
         xpix = dd
         call strnum(ypstr,4,dd,status)
         ypix = dd

      else
c
c prompt
c
         rrd(1) = 0
         rrd(2) = 0
         idelim = 0
         iflag = 0
         descr(1) = ' X Pixel coordinate  '
         descr(2) = ' Y Pixel coordinate '
         n = 2
c
         CALL XPXGTR(ZSTring,ZPArse,n,descr,n,rrd,nret,iflag,idelim)

         xpix = rrd(1)
         ypix = rrd(2)
         dd = xpix
         call xdstr(dd, -1, xpstr, di)
         call spars(Cmdid,'XPIX',xpstr,status)
         dd = ypix
         call xdstr(dd, -1, ypstr, di)
         call spars(Cmdid,'YPIX',ypstr,status)

      endif
c
c  See if label is outside window
c
      if ( clip ) then
         call pgqwin(xmin, xmax, ymin, ymax)
         if ( xpix.ge.xmin .and. xpix.le.xmax .and.
     &        ypix.ge.ymin .and. ypix.le.ymax ) then
            plot = .TRUE.
         else
            plot = .FALSE.
         endif
      else
         plot = .TRUE.
      endif
    
c  plot label
c
      if ( plot ) then
         if (symbol.ne.-999) then
            call sym_pgstate(symcolor, symcsize, symlwidth)
            call pgsclp(0)
            call PGPT(1,xpix,ypix,symbol)
            call pgsclp(1)
            call text_pgstate(color, csize, lwidth, font)
            spaces = 0.5
            if ( csize.gt.0. ) then
               call LBLPT(xpix, ypix, symcsize, spaces, angle, 
     &                    just, text)
            endif
         elseif ( csize.gt.0 ) then
            CALL PGPTXT(xpix,ypix,angle,rjust,text)
         endif
      endif

      call PGUNSA
c
c  Journal command
c
      call jrncmd(Cmdid, status)
      
      status = 0
      RETURN
      END
