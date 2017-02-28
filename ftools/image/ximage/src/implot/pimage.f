      SUBROUTINE PIMAGE(Cmdid,Map,Szx,Szy,Mapid,
     &                  Uptitle,Lotitle,Vpconfig,
     &                  Maxvps,Vpconnum,Vpnum,Vpset,Vpframe,
     &                  Numload,Ldlevs,Status) 
      IMPLICIT NONE
c
c  Plot command (Part of former DISPLAY command)
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
c
c  Local variables
      INTEGER*4 argc, numlevs, color, lwidth
      REAL*4 vport(4), csize, r_black, r_white
      REAL*4 xmin_frame , xmax_frame , ymin_frame , ymax_frame
      character(10) font
      character(16) xlabel, ylabel
      character(80) uptmp, lotmp
      LOGICAL noframe , nobox, nolabel, overlay, refresh
      LOGICAL isdisplay, isloaded, keepvp, notitle, truecol
      LOGICAL readonly, global

      character(100) cmd
      character(10) vpstr
      integer curvp, slen
      integer stxmin, stxmax, stymin, stymax
      integer ixmin, ixmax, iymin, iymax
      real*4 trf(6)

      csize = -1.0
      font = ' '
      notitle = .FALSE.
      noframe = .FALSE.
      nobox = .FALSE.
      nolabel = .FALSE.
      overlay = .FALSE.
      refresh = .FALSE.
      stxmin = 1
      stxmax = Szx
      stymin = 1
      stymax = Szy
c
      Status = 0
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      CALL GPARL(Cmdid,'NOFRAME',noframe,Status)
      CALL GPARL(Cmdid,'NOBOX',nobox,Status)
      CALL GPARL(Cmdid,'NOLABEL',nolabel,Status)
      CALL GPARL(Cmdid,'OVERLAY',overlay,Status)
      CALL GPARI(Cmdid,'XMIN',stxmin,Status)
      CALL GPARI(Cmdid,'XMAX',stxmax,Status)
      CALL GPARI(Cmdid,'YMIN',stymin,Status)
      CALL GPARI(Cmdid,'YMAX',stymax,Status)
      CALL GPARL(Cmdid,'REFRESH',refresh,Status)
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
      CALL XWRITE(' Plotting image ',10)
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
      uptmp = Uptitle
      lotmp = Lotitle
c
c If automatic viewport setting is in use, overlay with no frame
c
      if ( Vpnum.gt.0 ) then
         curvp = Vpnum
         if ( isdisplay() .and. Vpnum.gt.1 ) overlay = .TRUE.
c
c  Copy viewport from configuration
c
         Vpset(1) = Vpconfig(Vpnum,1)
         Vpset(2) = Vpconfig(Vpnum,2)
         Vpset(3) = Vpconfig(Vpnum,3)
         Vpset(4) = Vpconfig(Vpnum,4)
         write(ZWRite,'(a,i2,a)') ' Viewport ',Vpnum,
     &                            ' from configuration: '
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
c  Determine display properties
c
      CALL PLOT_START(mapid,ixmin,ixmax,iymin,iymax,Uptitle,Lotitle,
     &                xlabel,ylabel,r_black,r_white,xmin_frame,
     &                xmax_frame,ymin_frame,ymax_frame,status)
      IF ( status.NE.0 ) goto 500
c
c  Display image
c
      keepvp = .FALSE.
      CALL PG_START(ixmin,ixmax,iymin,iymax,trf,overlay,
     &              keepvp,vport,refresh,Status)
      if ( Status.ne.0 ) return

      readonly = .FALSE.
      global = .TRUE.
      numlevs = Numload
      call tclvari('default(numlevs)', numlevs, readonly, global,
     &             status)
      call pg_setcir(numlevs)
c
      if ( numlevs.gt.1 ) then
c
c  Set levels
c
         call pgsetlevs(numlevs, Ldlevs)
c
c  Set image transfer function to use levels
c
         call XPGSITF(10)

         CALL refresh_coltab
c
c Fix to keep color table up to date on truecolor device
c
         if ( .not.refresh .and. .not.overlay ) then
            truecol = .FALSE.
            call tclresl('pgtk::istruecol', truecol, status)
            if ( truecol ) call pgpage
         endif

         CALL XPGIMAG(Map,Szx,Szy,ixmin,ixmax,iymin,iymax,r_black,
     &                  r_white,trf)
      ELSE
         CALL PGGRAY(Map,Szx,Szy,ixmin,ixmax,iymin,iymax,r_black,
     &                  r_white,trf)
      ENDIF
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
c
c  Set displayed map and initialize state if /xtk
c
      call setdismap(mapid, status)
      if ( refresh ) then
         call pgebuf
      else
         if ( .not.overlay ) call tclrun('pgtk::newpage', status)
         call xistr(curvp, vpstr, slen)
         write(cmd,'(2a)') 'pgtk::initstate pimage ', vpstr(1:slen)
         call tclrun(cmd, status)
         call tclrun('pgtk::updtwin', status)
         call jrncmd(Cmdid, status)
      endif

  500 CONTINUE
c
c  Restore titles if they've been blanked for automatic viewport
c
      if ( notitle ) then
         Uptitle = uptmp
         Lotitle = lotmp
      endif

      RETURN
      END
