      subroutine surface(Cmdid, Map, Szx, Szy, Mapid, Vpconfig,
     &                   Maxvps, Vpconnum, Vpnum, Vpset, Status)
      implicit none
c
c  Draw a 3-D surface where height corresponds to pixel values
c
c   I  Cmdid           (i)  Command id
c   I  Map             (r)  Map
c   I  Szx/y           (i)  Map size
c   I  Mapid           (s)  Map id string
c   I  Vpconfig        (r)  Viewport configuration
c   I  Maxvps          (i)  Dimension of vpconfig array
c   I  Vpconnum        (i)  Number of viewports in configuration
c   I  Vpnum           (i)  Viewport number in configuration
c   I  Vpset           (r)  Viewport specification
c   O  Status          (i)  Error flag (0=OK)
c
      integer Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid
      integer*4 Maxvps, Vpconnum, Vpnum
      real*4 Vpconfig(Maxvps,4), Vpset(4)

      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      integer argc, i, cutmode
      logical isloaded, isdisplay
      real*4 vport(4), trf(6)
      real*4 datamin, datamax, range, xmin, xmax, ymin, ymax
      real*4 x1, x2, y1, y2, v1, v2
      real*4 maxoff
      character(2) opt
      real*8 xcen, ycen, zmx, zmy
      integer p_xbin, p_ywork, p_cross

      logical overlay, log, avgcut, neacut, noframe, refresh
      real permax
      integer slantpix

      integer numcross
      real bias

      integer color, lwidth
      real csize
      character(10) font

      character(100) cmd
      character(10) vpstr, slstr
      integer curvp, vlen, slen
      logical keepvp
      integer stxmin, stxmax, stymin, stymax
      integer ixmin, ixmax, iymin, iymax, xwid, ywid
      real dr
      real*8 slantfact

      overlay = .false.
      permax = 25.
      slantpix = -999
      log = .FALSE.
      numcross = -1
      avgcut = .FALSE.
      neacut = .FALSE.
      noframe = .FALSE.
      color = 1
      lwidth = -1
      csize = -1.0
      font = ' '
      refresh = .FALSE.
      stxmin = 1
      stxmax = Szx
      stymin = 1
      stymax = Szy

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'OVERLAY',overlay,status)
      CALL GPARR(Cmdid,'PERCENTMAX',permax,status)
      CALL GPARI(Cmdid,'SLANTPIX',slantpix,status)
      CALL GPARL(Cmdid,'LOG',log,status)
      CALL GPARI(Cmdid,'NUMCROSS',numcross,status)
      CALL GPARL(Cmdid,'AVGCUT',avgcut,status)
      CALL GPARL(Cmdid,'NONEMPTYAVGCUT',neacut,status)
      CALL GPARL(Cmdid,'NOFRAME',noframe,status)
      CALL GPARI(Cmdid,'COLOR',color,status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,status)
      CALL GPARR(Cmdid,'CSIZE',csize,status)
      CALL GPARS(Cmdid,'FONT',font,status)
      CALL GPARI(Cmdid,'XMIN',stxmin,Status)
      CALL GPARI(Cmdid,'XMAX',stxmax,Status)
      CALL GPARI(Cmdid,'YMIN',stymin,Status)
      CALL GPARI(Cmdid,'YMAX',stymax,Status)
      CALL GPARL(Cmdid,'REFRESH',refresh,status)
      if ( status.ne.0 ) return


      if ( .not.isloaded(mapid) ) then
         call XWRITE(' No map to plot', 10)
         Status = -1
         return
      endif

      IF ( .NOT.isdisplay() .AND. overlay ) THEN
         CALL XERROR('No display up. Overlay not possible',5)
         Status = -1
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
         call xerror(
     &      'Conditions not satisfied: 1 <= XMIN < XMAX <= Szx', 5)
         call xerror(
     &      '                          1 <= YMIN < YMAX <= Szy', 5)
         status = -1
         return
      endif

      xwid = ixmax - ixmin + 1
      ywid = iymax - iymin + 1
      if ( slantpix.eq.-999 ) slantpix = MAX(1,int(ywid/150.))
      if ( abs(slantpix) .gt. Szx ) then
         call xerror('ERROR: Slant cannot exceed size of image in x', 5)
         status = -1
         return
      endif
c     if ( numcross.lt.0 ) numcross = MIN(int(ywid/5.),100)
      if ( numcross.lt.0 ) numcross = MIN(int(ywid/2.),100)

      if ( numcross.gt.ywid ) then
         call xwrite(' NUMCROSS cannot exceed size of image in y', 10)
         return
      endif

      if ( avgcut.and.neacut ) then
         call xwrite(' AVGCUT and NONEMPTYAVGCUT are incompatible', 10)
         return
      endif

      if ( avgcut ) then
         cutmode = 1
      elseif ( neacut ) then
         cutmode = 2
      else
         cutmode = 0
      endif

      write(ZWRite, '(a,f5.1,a)') ' Max peak takes up ', permax,
     &                            '% of viewport'
      call xwrite(ZWRite, 15)
      write(ZWRite, *) ' Number of cross-sections: ', numcross
      call xwrite(ZWRite, 15)
      write(ZWRite, '(a,i3,a)') ' Cross-sections plotted with ',
     &                          slantpix, ' pixel slant'
      call xwrite(ZWRite, 15)
      if ( avgcut ) then
         call xwrite(' Averaging nearby cross-sections...', 10)
      elseif ( neacut ) then
         call xwrite(' Averaging nearby cross-sections, ignoring '//
     &               'non-empty pixels...', 10)
         call xwrite(' Standard cut, ignoring nearby cross-sections...', 
     &               10)
      endif
c
c  Find viewport bounds
c
      vport(1) = Vpset(1)
      vport(2) = Vpset(2)
      vport(3) = Vpset(3)
      vport(4) = Vpset(4)
      if ( vport(1).lt.0. ) call set_vpvals(vport,0)
c
c If automatic viewport setting is in use, overlay with no frame
c
      if ( Vpnum.gt.0 ) then
         curvp = Vpnum
         if ( isdisplay() .and. Vpnum.gt.1 ) overlay = .TRUE.
c
c  Copy viewport from configuration
c
         vport(1) = Vpconfig(Vpnum,1)
         vport(2) = Vpconfig(Vpnum,2)
         vport(3) = Vpconfig(Vpnum,3)
         vport(4) = Vpconfig(Vpnum,4)
         write(ZWRite,'(a,i2,a)') ' Viewport ',Vpnum,
     &                            ' from configuration: '
         call XWRITE(ZWRite, 20)
c
c  Save viewport in Vpset
c
            Vpset(1) = vport(1)
            Vpset(2) = vport(2)
            Vpset(3) = vport(3)
            Vpset(4) = vport(4)
c
c  Advance to next viewport in configuration
c
         Vpnum = Vpnum + 1
         if ( Vpnum.gt.Vpconnum ) Vpnum = 1
      else
         curvp = 1
      endif

      write(ZWRite,*) '   vp = ',vport(1),vport(2),vport(3),vport(4)
      call XWRITE(ZWRite, 20)
c
c  Save current window coordinates
c
      keepvp = .FALSE.
      call pg_start(ixmin, ixmax, iymin, iymax, trf, overlay, 
     &              keepvp, vport, refresh, status)
      if ( status.ne.0 ) goto 500 
c
c  Copy cross-sections into buffer
c
      call ralloc(1, xwid, numcross, p_cross, status)
      if ( status.ne.0 ) goto 500

      call crosssect(cutmode,log,Map,Szx,Szy,ixmin,ixmax,iymin,iymax,
     &               memr(p_cross),xwid,numcross,datamin, datamax)
      write(ZWRite,*) ' Minimum : ', datamin
      call rmvxbk(ZWRite(2:))
      call xwrite(ZWRite, 15)
      write(ZWRite,*) ' Maximum peak: ', datamax
      call rmvxbk(ZWRite(2:))
      call xwrite(ZWRite, 10)
c
c  Allocate workspace
c
      call ralloc(1, xwid, 1, p_xbin, status)
      if ( status.ne.0 ) goto 500
      call ralloc(1, xwid, 1, p_ywork, status)
      if ( status.ne.0 ) goto 500

      maxoff = (numcross*slantpix)/2
      do i = 1, xwid
         memr(p_xbin+i-1) = float(i)-maxoff
      enddo

      bias = (datamax-datamin)/(float(numcross)*(permax/100.))
c
c  Set y-axis to depend on data range
c
      call pgqwin(xmin, xmax, ymin, ymax)

      range = (datamax-datamin)/(permax/100.)
c     call pgswin(xmin, xmax, datamin, datamin+range)
      call pgswin(0.5, float(xwid)+0.5, datamin, datamin+range)
c
c  Draw surface without clipping
c
      call pgsclp(0)
      call pgsave
      call line_pgstate(color, lwidth, -1)

      call pghi2d(memr(p_cross),xwid,numcross,1,xwid,1,numcross,
     &            memr(p_xbin),slantpix,bias,.true.,memr(p_ywork))
      call pgunsa
      if ( .not.noframe ) then
c
c  Plot axes
         call pgsave
         call text_pgstate(-1, csize, -1, font)
c
c  Height (Pixel value)
c
         if ( log ) then
            opt = 'LN'
         else
            opt = 'N'
         endif
         if ( slantpix.lt.0 ) then
c           x1 = xmax - maxoff
            x1 = float(xwid) + 0.5 - maxoff
            call pgaxis(opt,x1,datamin,x1,datamax,datamin,datamax,
     &                  0.,0,0.5,0.,0.5,0.7,180.)
         else
c           x1 = xmin - maxoff
            x1 = 0.5 - maxoff
            call pgaxis(opt,x1,datamax,x1,datamin,datamax,datamin,
     &                  0.,0,0.5,0.,0.5,0.7,180.)
         endif
c
c  Image x axis
c
         call gheadd (mapid, 'ZMX', zmx, 0, status)
         call gheadd (mapid, 'ZMY', zmy, 0, status)
         call gheadd (mapid, 'DRPIX1', xcen, 0, status)
         call gheadd (mapid, 'DRPIX2', ycen, 0, status)
         status = 0

c        x1 = xmin - maxoff
c        x2 = xmax - maxoff
         x1 = 0.5 - maxoff
         x2 = float(xwid) + 0.5 - maxoff
         call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,v1,dr,xmin,ymin,1)
         call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,v2,dr,xmax,ymax,1)
         call pgaxis('N',x1,datamin,x2,datamin,v1,v2,
     &               0.,0,0.,0.5,0.5,0.7,0.)
c
c  Image y axis
c
         if ( slantpix.lt.0 ) then
c           x1 = xmin + maxoff
c           x2 = xmin - maxoff
            x1 = 0.5 + maxoff
            x2 = 0.5 - maxoff
            y1 = datamin+range
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,dr,v1,xmin,ymin,1)
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,dr,v2,xmax,ymax,1)
            call pgaxis('N',x1,y1,x2,datamin,v1,v2,
     &                  0.,0,0.,0.5,0.5,0.7,0.)
         else
c           x1 = xmax - maxoff
c           x2 = xmax + maxoff
            x1 = float(xwid) + 0.5 - maxoff
            x2 = float(xwid) + 0.5 + maxoff
            y2 = datamin+range
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,dr,v1,xmin,ymin,1)
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,dr,v2,xmax,ymax,1)
            call pgaxis('N',x1,datamin,x2,y2,v1,v2,
     &                  0.,0,0.,0.5,0.5,0.7,0.)
         endif
         call pgunsa
      endif
      call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,x1,y1,xmin,ymin,1)
      call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,x2,y2,xmax,ymax,1)
      call pgswin(x1,x2,y1,y2)
c
c  Turn clipping back on
c
      call pgsclp(1)

 500  continue
      call ralloc(0, Szx, numcross, p_cross, status)
      call ralloc(0, Szx, 1, p_xbin, status)
      call ralloc(0, Szx, 1, p_ywork, status)
c
c  Set displayed map and initialize state if /xtk
c
      call setdismap(mapid, status)
      if ( refresh ) then
         call pgebuf
      else
         if ( .not.overlay ) call tclrun('pgtk::newpage', status)
         call xistr(curvp, vpstr, vlen)
         write(cmd,'(2a)') 'pgtk::initstate surface ', vpstr(1:vlen)
         call tclrun(cmd, status)
         call tclrun('pgtk::updtwin', status)
         call jrncmd(Cmdid, status)
c
c  slantfact may need updating on refresh too, but how do you
c  get that info into the right state?
c
         slantfact = float(slantpix)*float(numcross)/float(ywid)
         call xdstr(slantfact, -1, slstr, slen)
         write(cmd,'(4a)') 'pgtk::slantset ', slstr(1:slen)
         call tclrun(cmd, status)
      endif

      return
      end
