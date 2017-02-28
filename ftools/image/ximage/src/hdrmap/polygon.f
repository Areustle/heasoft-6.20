      SUBROUTINE POLYGON(Cmdid, Map, Szx, Szy, Mapid, Status)
      IMPLICIT NONE
c
c  POLYGON command probes map values
c
c  I  Cmdid     (i)  Command id
c  I  Map       (r)  Image map
c  I  Szx/y     (i)  Size of map
c  I  Mapid     (s)  Map id string
c  O  Status    (i)  Error flag (0 = OK)
c
      INTEGER*4 Cmdid, Szx, Szy, Status
      REAL*4 Map(Szx,Szy)
      CHARACTER*(*) Mapid

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER MAX_PTS, num_pts
      PARAMETER(MAX_PTS=50)
      REAL*4 x(MAX_PTS), y(MAX_PTS)

      INTEGER*4 equinox, ierr, i, argc, di, LENACT
      REAL*8 xsky, ysky, ximg, yimg, dx, dy, epoch, dd
      character*(MAX_IDSTR) wcsid
      character(40) system, proj, xlab, ylab, unit, strra, strdec
      real*4 dr3(3)
      logical isloaded, isdisplay, ismouse, isrnull
      character(1) maptype
      character(20) valstr
      integer vallen

      integer j, k

      LOGICAL fill , line, label
      INTEGER*4 symbol, color, lwidth, lstyle, boxsize
      REAL*4 csize
      character(10) font

      num_pts = 0
      symbol = 2
      boxsize = -1
      fill = .FALSE.
      line = .FALSE.
      label = .FALSE.
      color = 16
      call get_color(color)
      csize = 1.0
      lwidth = 1
      font = ' '
      lstyle = 1
      
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'FILL',Fill,Status)
      CALL GPARL(Cmdid,'LINE',Line,Status)
      CALL GPARL(Cmdid,'LABEL',Label,Status)
      CALL GPARI(Cmdid,'SYMBOL',Symbol,Status)
      CALL GPARI(Cmdid,'BOXSIZE',boxsize,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      CALL GPARI(Cmdid,'LSTYLE',lstyle,Status)
      if ( status.ne.0 ) return

      if ( .not.isloaded(Mapid) ) then
         call XWARN (' Image not loaded', 10)
         status = -1
         return
      endif

      if ( .not.isdisplay() ) then
         call XWARN (' No display.', 10)
         status = -1
         return
      endif
 
      if ( .not.ismouse() ) then
         call XWARN (' Requires an interactive display.', 10)
         status = -1
         return
      endif
      call gheads(Mapid, 'MAPTYPE', maptype, 0, status)
      wcsid = ' '
      call gheads(Mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
         call xwrite(' WCSID not found', 5)
         status = -1
      endif
      if ( status.ne.0 ) return

      call PGSAVE

      call xwrite(' Left button selects point, middle erases last, and'
     &            //' right ends selection', 10)

      if ( label ) then
         call text_pgstate(color, csize, lwidth, font)
         call POLYLBL(Map, Szx, Szy, Mapid, maptype, boxsize, MAX_PTS,
     &                num_pts, x, y, status)
      elseif ( boxsize.ge.0 ) then
         call sym_pgstate(color, csize, lwidth)
         call POLYBOX(Map, Szx, Szy, Mapid, maptype, boxsize, MAX_PTS,
     &                num_pts, x, y, symbol, status)
      elseif ( line ) then
         call line_pgstate(color, lwidth, lstyle)
         call PGLCUR (MAX_PTS, num_pts, x, y)
      else
         call sym_pgstate(color, csize, lwidth)
         call PGOLIN (MAX_PTS, num_pts, x, y, symbol)
      endif
      if ( status.ne.0 ) goto 200
      if ( num_pts.LT.1 ) goto 200
      if ( fill ) call PGPOLY(num_pts, x, y)
c
c  Header
c
      call wcsfrminfo(wcsid, system, proj, xlab, ylab, unit, epoch)
      call tclresi("set default(equinox)", equinox, status)

      CALL XWRITE(' ',10)
      if ( xlab.eq.'RA' ) then
         WRITE (ZWRite,99001) equinox, equinox
      else
         WRITE (ZWRite,99002) xlab(1:12), ylab(1:12)
      endif
      CALL XWRITE(ZWRite,10)
c
      DO 100 i = 1 , num_pts

         dx = x(i)
         dy = y(i)
         call wcsimgpix(wcsid, ximg, yimg, dx, dy, 0, status)
         call wcsimgsky(wcsid, ximg, yimg, xsky, ysky, equinox, 1, 
     &                  status)

         if ( xlab.eq.'RA' ) then
            call cnv_radec(strra,strdec,xsky,ysky,dr3,dr3,2,2,ierr)
         else
            call wcsformat(system, xsky, ysky, strra, strdec)
         endif

         j = NINT(ximg)
         k = NINT(yimg)
         if ( j.ge.1 .and. j.le.Szx .and. k.ge.1 .and. k.le.Szy ) then
            if ( isrnull(Map(j,k)) ) then
               valstr = 'NULL'
               vallen = 4
            elseif ( maptype.eq.'I' ) then
               di = NINT(Map(j,k))
               call xistr(di, valstr, vallen)
            else
               dd = Map(j,k)
               call xdstr(dd, 8, valstr, vallen)
            endif
         else
            valstr = 'OUTSIDE MAP'
            vallen = LENACT(valstr)
         endif
         WRITE (ZWRite,'(1x,2(1x,f7.1),2(1x,i5),3x,a,2x,a,3x,a)')
     &         x(i) , y(i) , j, k, strra(1:12), strdec(1:12), 
     &         valstr(1:vallen)
         CALL XWRITE(ZWRite,10)

 100  CONTINUE

      CALL XWRITE(' ',10)

      status = 0
 200  call PGUNSA

      RETURN
99001 FORMAT ('    xpix    ypix   ximg  yimg     ra (',i4,
     &       ')    dec (',i4,')   pixel value')
99002 FORMAT ('    xpix    ypix   ximg  yimg   ',a,1x,a,3x,
     &       'pixel value')
      END
