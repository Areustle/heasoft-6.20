      SUBROUTINE PCONTOUR(Cmdid,Map,Szx,Szy,Mapid,
     &                    Uptitle,Lotitle,Vpconfig,
     &                    Maxvps,Vpconnum,Vpnum,Vpset,Vpframe,
     &                    Numload,Ldlevs,Status) 
      IMPLICIT NONE
c
c  Pcontour command (Part of former CONTOUR command)
c
c  I  Cmdid           (i)  Command id
c  I  Map             (i)  Image map
c  I  Szx, Szy        (i)  Size of current maps
c  I  Mapid           (i)  Current map id
c  I  Uptitle         (c)  Text placed immediately above Lotitle
c  I  Lotitle         (c)  Text placed immediately above viewport
c  I  Vpconfig        (r)  Viewport configuration
c  I  Maxvps          (i)  Dimension of vpconfig array
c  I  Vpconnum        (i)  Number of viewports in configuration
c  I  Vpnum           (i)  Viewport number in configuration
c  I  Vpset           (r)  Viewport specification
c  I  Vpframe         (l)  Whether to plot frame with vp configuration
c  I  Numload         (i)  Number of loaded levels
c  I  Ldlevs          (r)  Loaded levels
c  O  Status          (i)  Error flag (0=OK)
c
      INTEGER*4 Cmdid, Szx, Szy, Status
      INTEGER*4 Maxvps, Vpconnum, Vpnum, Numload
      REAL*4 Map(Szx,Szy)
      REAL*4 Vpconfig(Maxvps,4), Vpset(4), Ldlevs(*)
      LOGICAL Vpframe
      CHARACTER*(*) Mapid, Uptitle, Lotitle

      include '../include/io.inc'
      include '../include/maxvals.inc'

      EXTERNAL CONXBUF
c
c  Local variables
      INTEGER*4 argc, i
      REAL*4 vport(4), csize, r_black, r_white
      REAL*4 xmin_frame , xmax_frame , ymin_frame , ymax_frame
      character(10) font
      character(16) xlabel, ylabel
      character(80) uptmp, lotmp
      CHARACTER*(MAX_IDSTR) fromwcs, towcs
      LOGICAL noframe , nobox, nolabel, overlay, refresh
      INTEGER color, lwidth, lstyle, firstcont
      LOGICAL isdisplay, isloaded, notitle, keepvp

      real*8 dd
      character(100) cmd, ds
      character(10) vpstr
      integer curvp, slen
      integer stxmin, stxmax, stymin, stymax
      integer ixmin, ixmax, iymin, iymax
      integer nintrf
      real*4 trf(6), intrf(6), dumtrf(6), tmplevs(MAX_NUMLEVS)
      real*4 svx1, svx2, svy1, svy2

      csize = -1.0
      font = ' '
      noframe = .FALSE.
      nobox = .FALSE.
      nolabel = .FALSE.
      overlay = .FALSE.
      refresh = .FALSE.
      color = -1
      lwidth = -1
      lstyle = -1
      firstcont = 1
      stxmin = 1
      stxmax = Szx
      stymin = 1
      stymax = Szy
      nintrf = 0
      fromwcs = ' '
      towcs = ' '

      Status = 0
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      CALL GPARL(Cmdid,'NOFRAME',noframe,Status)
      CALL GPARL(Cmdid,'NOBOX',nobox,Status)
      CALL GPARL(Cmdid,'NOLABEL',nolabel,Status)
      CALL GPARL(Cmdid,'OVERLAY',overlay,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARI(Cmdid,'LSTYLE',lstyle,Status)
      CALL GPARI(Cmdid,'FIRST_CONTOUR_DRAWN',firstcont,Status)
      CALL GPARI(Cmdid,'XMIN',stxmin,Status)
      CALL GPARI(Cmdid,'XMAX',stxmax,Status)
      CALL GPARI(Cmdid,'YMIN',stymin,Status)
      CALL GPARI(Cmdid,'YMAX',stymax,Status)
      CALL GPARL(Cmdid,'REFRESH',refresh,Status)
      CALL GPARLR(Cmdid,'TRF',intrf,nintrf,6,Status)
      CALL GPARS(Cmdid,'FROMWCS',fromwcs,Status)
      CALL GPARS(Cmdid,'TOWCS',towcs,Status)
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      if ( Status.ne.0 ) return
c
c choose what to plot...
c
      if ( .not.isloaded(mapid) ) then
         call XWRITE(' No map to display', 10)
         status = -1 
         return
      endif
      CALL XWRITE(' Plotting contours ',10)

      keepvp = .false.
      notitle = .false.
      uptmp = Uptitle
      lotmp = Lotitle
c
c  Require LEVELS command
c 
      if ( Numload.eq.0 ) then
         call XWRITE (' No loaded levels. Use the LEVELS command',10)
         call XWRITE (' to calculate levels before plotting. ',10)
         return
      endif
c
      IF ( nobox ) noframe = .TRUE.
      if ( firstcont.lt.1 .or. firstcont.gt.Numload ) then
         call xwrite(' FIRST_CONTOUR_DRAWN out of range', 10)
         status = -1
         return
      endif
c
c If automatic viewport setting is in use, auto-overlay
c
      if ( Vpnum.gt.0 .and. .not.overlay ) then

         curvp = Vpnum
c
c  Copy viewport from configuration
c
            if ( isdisplay() .and. Vpnum.gt.1 ) overlay = .TRUE.
            Vpset(1) = Vpconfig(Vpnum,1)
            Vpset(2) = Vpconfig(Vpnum,2)
            Vpset(3) = Vpconfig(Vpnum,3)
            Vpset(4) = Vpconfig(Vpnum,4)
            write(ZWRite,'(a,i2,a)') ' Viewport ',Vpnum,
     &                               ' from configuration: '
            call XWRITE(ZWRite, 20)
c
c  Advance to next viewport in configuration
c
            Vpnum = Vpnum + 1
            if ( Vpnum.gt.Vpconnum ) Vpnum = 1

c
c  Blank frame and title if .not.Vpframe
c
         if ( .not.Vpframe ) then
            noframe = .TRUE.
            notitle = .TRUE.
            if ( Uptitle(80:80).eq.'*' .or. Lotitle(80:80).eq.'*' ) then
               call XWRITE(' TITLE command has no effect when using', 
     &                     10)
               call XWRITE('   a viewport configuration. Use VPLABEL.', 
     &                     10)
            endif
         endif
      elseif ( overlay ) then
c
c If overlay, send flag value (-1) to pgtk::initstate, so
c   it knows to use current viewport
c
         curvp = -1
      else
         curvp = 1
      endif

      if ( Vpset(1).lt.0.0 ) then
         call set_vpvals(vport,0)
      else
         vport(1) = Vpset(1)
         vport(2) = Vpset(2)
         vport(3) = Vpset(3)
         vport(4) = Vpset(4)
      endif

      write(ZWRite,*) '   vp = ',vport(1),vport(2),vport(3),vport(4)
      call XWRITE(ZWRite, 20)

      IF ( .NOT.isdisplay() .AND. overlay ) THEN
         CALL XERROR('No display up. Overlay not possible',5)
         RETURN
      ENDIF
c
c  Determine effective ixmin/max and iymin/max from entered
c  stxmin/max and stymin/max accounting for image edges
c
      ixmin = MAX(1,stxmin)
      ixmax = MIN(Szx,stxmax)
      iymin = MAX(1,stymin)
      iymax = MIN(Szy,stymax)
      if ( .not. (ixmin.le.ixmax .and. iymin.le.iymax) ) then
         call xerror('XMIN > XMAX or YMIN > YMAX', 5)
         status = -1
         return
      endif
c
c  Set displayed map and initialize state
c
      call setdismap(mapid, status)
      if ( .not.refresh ) then
         if ( .not.overlay ) call tclrun('pgtk::newpage', status)
         call xistr(curvp, vpstr, slen)
         write(cmd,'(2a)') 'pgtk::initstate pcontour ', vpstr(1:slen)
         call tclrun(cmd, status)
         call jrncmd(Cmdid, status)
      endif
c
c  Use transformation matrix if user-entered
c
      if ( nintrf.eq.6 ) then
         notitle = .true.
         noframe = .true.
         do i = 1, 6
            trf(i) = intrf(i)
         enddo
         if ( refresh ) keepvp = .true.
      elseif ( nintrf.ne.0 ) then
         call xwrite(' Ignoring TRF, must contain 6 values...', 10)
         nintrf = 0
      elseif ( fromwcs.ne.' ' ) then
         notitle = .true.
         noframe = .true.
         if ( refresh ) keepvp = .true.
      elseif ( .not.refresh ) then
c
c  Check coordinate-dependence to see if a transformation is necessary
c 
         call xistr(curvp, vpstr, slen)
         write(cmd,'(2a)') 'pgtk::coordvp ', vpstr(1:slen)
         call tclress(cmd, ds, 100, status)
         if ( ds.ne.' ' ) then
            call gheads(mapid, 'WCSID', fromwcs, 0, status)
            if ( status.ne.0 ) then
               call xwrite(" Failed to get wcsid for contour map", 5)
               return
            endif
            towcs = ds
            notitle = .true.
            noframe = .true.
            keepvp = .true.
         endif
      endif
c
c  Determine display properties
c
      CALL PLOT_START(mapid,ixmin,ixmax,iymin,iymax,Uptitle,Lotitle,
     &                xlabel,ylabel,r_black,r_white,xmin_frame,
     &                xmax_frame,ymin_frame,ymax_frame,status)
      IF ( status.NE.0 ) goto 500
c
c  Open device or advance page
c
      call pgqwin(svx1, svx2, svy1, svy2)
      call pg_start(ixmin,ixmax,iymin,iymax,dumtrf,overlay,
     &              keepvp,vport,refresh,Status)
      if ( Status.ne.0 ) return
      if ( nintrf.eq.0 ) then
         do i = 1, 6
            trf(i) = dumtrf(i)
         enddo
      endif
c
c If overlaying, use highest color of color table, otherwise,
c use foreground color
c
      if ( color.lt.0 ) then
         if ( overlay ) then
            color = 16
            call get_color(color)
         else
            color = 1
         endif
      endif

      if ( isdisplay() ) then
         call pgsave
         call line_pgstate(color, lwidth, lstyle)

         write(ZWRite,'(a,i4)') ' Number of contours:    ' , Numload
         call xwrite(ZWRite, 10)
         write(ZWRite,'(a,i4)') ' First plotted contour: ' , firstcont
         call xwrite(ZWRite, 10)
         do i = firstcont, Numload
            dd = Ldlevs(i)
            call xdstr(dd, -1, ds, slen)
            write(ZWRite,'(a,i4,2a)') ' Contour ', i, ' : ', ds(:slen)
            call xwrite(ZWRite, 10)
            tmplevs(i-firstcont+1) = Ldlevs(i)
         enddo
         i = Numload-firstcont+1
         if ( keepvp ) call pgswin(svx1, svx2, svy1, svy2)
         if ( fromwcs.ne.' ' ) then
            call wcsconxbeg(fromwcs, towcs, status)
            if ( status.eq.0 ) then
               call pgconx(Map,Szx,Szy,ixmin,ixmax,iymin,iymax,
     &                     tmplevs,i,CONXBUF)
               call wcsconxend(status)
            else
               status = 0
               call xwrite(' Plotting simple overlay...', 10)
               call pgcons(Map,Szx,Szy,ixmin,ixmax,iymin,iymax,
     &                     tmplevs,i,trf)
            endif
         else
            call pgcons(Map,Szx,Szy,ixmin,ixmax,iymin,iymax,
     &                  tmplevs,i,trf)
         endif
         call pgunsa
      endif
c
c  Set default size and font attributes
c
      IF ( csize.lt.0.0 ) THEN
         IF ( vport(2)-vport(1).lt.0.5 .or. 
     &        vport(4)-vport(3).lt.0.5 ) then
            csize = 0.8
         ELSE
            csize = 1.0
         ENDIF
      ENDIF
      color = -1
      lwidth = -1
      call text_pgstate(color,csize,lwidth,font)
c
c  Display frame
c
      if ( notitle ) then
         Uptitle = ' '
         Lotitle = ' '
      endif
      if ( nolabel ) then
         xlabel = ' '
         ylabel = ' '
      endif
      CALL PLOT_FRAME(Uptitle,Lotitle,xlabel,ylabel,noframe,nobox,
     &                xmin_frame,xmax_frame,ymin_frame,ymax_frame)
      if ( keepvp ) call pgswin(svx1, svx2, svy1, svy2)

      if ( refresh ) then
         call pgebuf
      else 
         call tclrun('pgtk::updtwin', status)
      endif
      call tclrun('pgtk::flushxtk', status)
      call tclrun('update idletasks', status)

  500 CONTINUE
c
c  Restore titles if they've been blanked
c
      if ( notitle ) then
         Uptitle = uptmp
         Lotitle = lotmp
      endif

      RETURN
      END
