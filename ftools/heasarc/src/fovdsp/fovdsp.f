c     fovdsp -- display ISDC reference catalog and INTEGRAL FOV.
C                display a FITS image, and also can superpose several 
C                images with different pixel sizes.
C
C     Originally written by Ken Ebisawa (ebisawa@obs.unige.ch) 
C     in Fortran77, and converted to ISDC style Fortran 90 by
C     Bruce O'Neel (bruce.oneel@obs.unige.ch)
c
c     v1.0   2002-10-04  The first released version 
c     v1.1   2002-10-04  Fixed a bug that segmentation fault
C                        error happens when images have NaN entries
C                        (this is the case for jemx).
c     v1.2   2002-10-18  Display both fully-coded and zero-response FOV.
c     v1.2.1 2002-10-29  makeisdc1.in fixed. No change in the source code.
c     v1.3   2002-12-06  Satellite attitudes may be directory specified
C                        from parameters (not through the attitude file).
C                        Satellite attitude RA_SCX, DEC_SCX, RA_SCZ, DEC_SCZ
C                        are changed from REAL to DOUBLE PRECISION.
C     v1.4   2003-01-24  A new label above the top label may be
C                        specified from the parameter. This can be useful,
C                        e.g., to tell the ScW ID when spawned from "iqla". 
C     v1.5   2003-03-24  swg_idx_ibis.fits can be used instead of an
C                        attitude file.
C     v1.6   2003-04-09  Fonsize is variable throug the parameter "fontsize".
C     v1.7   2003-08-18  X and Z-axes need not to be orthogonal to draw the
C                        FOV.  Only X-axis RA and DEC can be specified to
C                        draw FOV.
C     v1.8   2004-04-10  When FOV crosses l=180 (e.g. Crab observation), 
C                        lines do not go over the sky crossing l=0. 
C
C     v1.9   2004-08-20  ftools version. Renamed from fovdisp to fovdsp to
C                        avoid confusion.  Use fitsio instead of DAL,
C                        change f90 statemsnts to f77.
C     v2.0   2004-08-20  Incorporate Swift FOV. INTEGRAL and Swift is
C                        distingushed by a new parameter "mission.
C     v2.1   2004-09-03  If Swift is specified, Swift defined Euler angles are
C                        read from the parameter file and used to calculate
C                        the pointing direction and roll
C     v2.2   2004-10-04  When started, INTEGRAL FOV to be displayed can be spefied with
C                        the whichinst parameter.
C     v2.3   2004-10-14  Explanation of key strokes are displayed initially only once. 
C
C     v2.4   2005-11-14  * "Segmentation error" occur was fixed when input catalog is not found.
C                        * Suzaku FOV (HXD-GSO, HXD-PIN and XIS) may be drawn.
C                        * "ipixeldist" is no longer used.
C                        *  The that 't' does not work when sources are not plotted was fixed.
C     v2.5   2005-11-15  * Suzaku HXD nominal position (DETX,DETY)=(-3.5arcmin,0.0) is marked
C                          for the Suzaku FOV.
C                        * TGT_RA, TGT_DEC and TGT_NAM parameters are added for target RA, DEC
C                        * and target name

 
      subroutine fovdsp
      implicit none
      integer maxattentry
      parameter (maxattentry=9999)
      character(8)  pointing_id(maxattentry)
      double precision  ra_scx(maxattentry), dec_scx(maxattentry),
     $     ra_scz(maxattentry), dec_scz(maxattentry)
      character attfile*999, catfile*999, mission*10
      integer  npointing, status, i, mode
      double precision l, b, ra, dec, x, y
      logical  hardcopyistaken

      integer maxsources
      parameter (maxsources=2000)
      character(20) source_name(maxsources)
      real source_ra(maxsources), source_dec(maxsources)
      real isgr_flux1(maxsources), isgr_flux2(maxsources),
     $     jemx_flux1(maxsources), jemx_flux2(maxsources)
      integer nsource, length

      common/catalog/source_name, source_ra, source_dec, nsource,
     $     isgr_flux1, isgr_flux2, jemx_flux1, jemx_flux2
      real  crab_flux
      real size
      integer cindex, coordinates, plotsource, plotfov, plotname
      character(10) device, cursor,cursor_in,olddevice
      character toplabel*1024,overtoplabel*1024
      real ViewPortcenterX, ViewPortcenterY,
     $     newViewPortcenterX, newViewPortcenterY,
     $     ViewPortXwidth, ViewPortYwidth,cursorX, cursorY
      double precision tmpX,tmpY,tmpra,tmpdec,tmp1,tmp2,
     $     tmp3,tmp4
      double precision VpCentRA, VPcentDEC,VpCentL, VPcentB
      real xmin, xmax, ymin, ymax
      character text*30
      double precision euler(3),dummyeuler(3)
      double precision cursorRA, cursorDEC, cursorL, cursorB
      real tanwidth
      integer tangential 
      logical plot_source_name
      integer icilo, icihi,transfer
      logical dispimage
      real scale
      integer whichinst, outimagesize
      character(999) imagefilelist
      logical showgrids
      logical pixeldist
c      integer ipixeldist
      character(255)      procName
      character(5)        version
      character string*1200
      integer rilStat
      logical attinput
      double precision VPwidth
      double precision fontsize
      double precision roll

      integer warning_0, showhelp
      
      character(40) taskname
      common /task/ taskname

      double precision tgt_ra, tgt_dec
      character(20) tgt_name

C     Data:
      data status/0/, mode/0/, showhelp/0/
      data hardcopyistaken/.false./
      data procName/'fovdsp'/
      data version/'2.5'/
C     Crab flux is based on jemx soft (149), hard (46), 
C     ibis soft (181), hard (81) sum (counts/s) in version 5 catalog.
      data crab_flux/457/

      taskname = 'fovdsp  ver 2.5 (2005-11-15)'

      call fcecho(taskname)
      call  uclgst('mission', mission, status)
      call  Uclgst('toplabel', overtoplabel,status)
C     Target position and name to display
      call Uclgsd('TGT_RA',  tgt_ra,status)
      call Uclgsd('TGT_DEC', tgt_dec,status)
      call uclgst('TGT_NAME',tgt_name, status)

      if(index(mission,'Suzaku').gt.0) then 
         attinput = .false.
C     Note, Suzaku EA is defined as Z-Y-Z, while INTEGRAL EA
C     is defined as X-Z-X.
C     Namely, Suzaku-Z axis (pointing direction) is INTEGRAL-X axis, and
C     Suzaku-XY plane (detector plane) is INTEGRAL-YZ plane
         call Uclgsd('Euler1', euler(1),status)
         call Uclgsd('Euler2', euler(2),status)
         call Uclgsd('Euler3', euler(3), status)
      else if(index(mission,'Swift').gt.0) then 
         attinput = .false.
         call Uclgsd('RA_SCX', RA_SCX(1),status)
         call Uclgsd('DEC_SCX',DEC_SCX(1),status)
         call Uclgsd('Roll', roll, status)
      else if(index(mission,'INTEGRAL').gt.0) then
         call Uclgsi('whichinst',whichinst,status)
         call  Uclgsb('attinput', attinput,status)
         if(attinput) then
            call  Uclgst('attfile',attfile,status)
         else
            call Uclgsd('RA_SCX', RA_SCX(1),status)
            call Uclgsd('DEC_SCX',DEC_SCX(1),status)
            call Uclgsd('RA_SCZ', RA_SCZ(1),status)
            call Uclgsd('DEC_SCZ',DEC_SCZ(1),status)
         endif
      else
         write(string,'(a,a,a)') 'Warning: mission ', mission,
     $        'not defined.'
         call fcecho(string)
         call fcecho(
     $     'Specify Suzaku, INTEGRAL or Swift to draw instrument FOV.'
     $        )
         stop
      endif
      call uclgst('catalog', catfile, status)
      call uclgsi('tangential',tangential,status)
      ViewPortYwidth=180.0
      if(tangential.eq.1) then
         call Uclgsd('VPcentRA', VPCentRA,status)
         call Uclgsd('VPcentDEC',VPCentDEC,status)
         call Uclgsd('VPwidth',VPwidth,status)
         ViewPortYwidth=real(VPwidth)
      endif
      call Uclgsi('coordinates',coordinates,status)
      call Uclgsi('plotsource',plotsource,status)
      call Uclgsi('plotname',plotname,status)
      call Uclgsi('plotfov',plotfov,status)
      call Uclgst('imagelist',imagefilelist,status)
      call uclgsb('pixeldist',pixeldist,status)
      call Uclgsd('fontsize',fontsize,status)
c      pixeldist = .false.
c      if (ipixeldist .ne. 0) pixeldist = .true.
      call Uclgst('gdevice',device,status)

      call readfits(catfile, status)
      if(status.ne.0) then
         write(string,'(a,a)') 'error in reading the catalog file: ',
     $        catfile
         call fcecho(string)
c     without catalog file, still we may display instrument
c     fov and images.
         status =0
      endif

      if(attinput) then
         call read_att_file(attfile,pointing_id,ra_scx, dec_scx,
     $        ra_scz,dec_scz,npointing,status)
         if(status.eq.0) then
         write(toplabel,'(a,a)')
     $        'attfile=',attfile
         else
            write(string,'(a,a,a)')
     $           'Attitude file: ', attfile, ' not found.'
            call FCECHO (string)
            return
c            write(toplabel,'(a)') ' '
c            status = 0
         endif
      else
         npointing=1
         pointing_id(1)=' '
         if(index(mission,'Suzaku').gt.0) then 
            write(toplabel,'(a,f7.3,a,f7.3,a,f7.3,a)')
     $           'Euler Angles=(',euler(1),',',euler(2),
     $           ',',euler(3),')' 
         elseif(index(mission,'Swift').gt.0) then 
            write(toplabel,'(a,f6.2,a,f6.2,a,f6.2,a)')
     $           'X-axis=(',ra_scx(1),',',dec_scx(1),
     $           '), roll =(',roll,')' 
         else if(index(mission,'INTEGRAL').gt.0) then
            call scxz2euler(ra_scx(1),dec_scx(1),
     $           ra_scz(1),dec_scz(1),pointing_id(1),
     $           euler,status)
            write(toplabel,'(a,f6.2,a,f6.2,a,f6.2,a,f6.2,a)')
     $           'X-axis=(',ra_scx(1),',',dec_scx(1),
     $           '), Z-axis=(',ra_scz(1),',',dec_scz(1),')' 
         endif
      endif

      if(status.ne.0) then
         write(string,'(a,a)') 'error in reading the attitiude file: ',
     $        attfile
         call fcecho(string)
c     without attitude file, just display catalog.
          status=0
      endif

      ViewPortCenterX=0.0
      ViewPortCenterY=0.0
      ViewPortXwidth=360.0
      tmpra  = 0.0
      tmpdec = 0.0
      transfer =0
      scale = 1.0
      dispimage=.false.
c     Since version 2.2, whichinst is read from the parameter
c      whichinst = 0
      outimagesize=256
      showgrids=.true.

 100  continue
      call pgbegin(0,device,1,1)

      call pgpage
      call pgiden

      call pgvstd
      xmin =  ViewPortCenterX+ViewPortXwidth/2.0
      xmax =  ViewPortCenterX-ViewPortXwidth/2.0
      ymin =  ViewPortCenterY-ViewPortYwidth/2.0
      ymax =  ViewPortCenterY+ViewPortYwidth/2.0
      if(tangential.ne.1) then
         call pgswin(xmin,xmax,ymin,ymax)
      else
         if((ymax-ymin).ge.180.) then
            tanwidth = 89.
         else
            tanwidth=(ymax-ymin)/2.0
         endif
         call pgwnad(-tanwidth,tanwidth,-tanwidth,tanwidth)
      endif
      call pgbox(' ',0.0,0,' ',0.0,0)

      if(coordinates.eq.1) then
         call pglabel('Galactic longitude', 'Galactic latitude ',
     $        toplabel)
         if(tangential.ne.1) then
            call aitoffinverse(dble(ViewportcenterX),
     $           dble(viewPortCenterY), VPcentL,VPcentB)
         endif
         call lb2radec(VPcentL,VPcentB,VPcentRA,VPcentDEC)
      else
         call pglabel('Right Ascension (J2000)', 'Declination (J2000)',
     $        toplabel)
         if(tangential.ne.1) then
         call aitoffinverse(dble(ViewportcenterX),dble(viewPortCenterY),
     $        VPcentRA,VPcentDEC)
         endif
         call radec2lb(VPcentRA,VPcentDEC,VPcentL,VPcentB)
      endif

C     write the over-the-top-label
      call PGMTXT('T', 3.2, 0.5, 0.5, overtoplabel)
      call PGMTXT('T', 0.5, 0.05, 0.5, 'Mission: '//mission)
      call PGMTXT('B', 1.2, 0.05, 0.5, 'Viewport center')
      if(VPcentL.lt.0.0) VPcentL=VPcentL+360.0
      if(VPcentRA.lt.0.0) VPcentRA=VPcentRA+360.0

      write(text,'(a,f6.2,a,f6.2,a)')
     $     '(l,b)=(',VPcentL,',',VPcentB,')'
      call  PGMTXT ('B', 3.2, 0.05, 0.5, text)
      write(text,'(5a,f6.2,a,f6.2,a)')
     $     '(',char(92),'ga,',char(92),'gd)=(',
     &     VPcentRA,',',VPCentDEC,')'
      call  PGMTXT ('B', 2.2, 0.05, 0.5, text)

      call pgsch(1.0)

      if(dispimage.and.tangential.eq.1) then
         if(coordinates.eq.0) then
            dummyeuler(1)=VPcentRA
            dummyeuler(2)=90.0-VPcentDEC
            dummyeuler(3)=90.0
         else
            dummyeuler(1)=VPcentL
            dummyeuler(2)=90.0-VPcentB
            dummyeuler(3)=90.0
         endif
         call
     $display_image(imagefilelist,
     $ViewPortYwidth,coordinates, dummyeuler,transfer,scale,
     $        outimagesize,pixeldist)
      endif

c     note indexes 0 to 15 are predefined in pgplot
C     We define gradation of colors between Red (1,0,0) to Blue (1,0,0)
C     in the RGB system to tell the hardness of the sources.
      do i =0,31
         call pgscr(16+i,1.0-1./31.*i,0.0,1./31.*i)
      end do

c     Always plot the main target
      call pgslw(3)
      if(coordinates.eq.1) then
         call radec2lb(tgt_ra,tgt_dec,l,b)
         if(tangential.ne.1) then
            call aitoff(l,b,x,y)
         else
            dummyeuler(1)=VPcentL
            dummyeuler(2)=90.0-VPcentB
            dummyeuler(3)=90.0
            call radec2detYZ(l,b,dummyeuler,x,y)
         endif
      else
         if(tangential.ne.1) then
            call aitoff(tgt_ra,tgt_dec,x,y)
         else
            dummyeuler(1)=VPcentRA
            dummyeuler(2)=90.0-VPcentDEC
            dummyeuler(3)=90.0
            call radec2detYZ(tgt_ra,tgt_dec,
     $           dummyeuler,x,y)
         endif
      endif
      call pgsch(1.5)
      call pgsci(1)
      call pgpoint(1,real(x),real(y),17)
      call PGPTXT(real(x), real(y), 0.0, 0.5, tgt_name)
      call pgslw(1)


C     Plot catalog sources
c      if(plotsource.eq.0) then
         call pgscir(16,47)
         call PGSITF(2)
         call pgqcir(icilo,icihi)
         if(plotsource.eq.0)
     $        call PGWEDG('rI', 0., 3.0, -1.0, 1.0, 'hardness')
         do i = 1, nsource
            if(coordinates.eq.1) then
               call radec2lb(dble(source_ra(i)),dble(source_dec(i)),l,b)
               if(tangential.ne.1) then
                  call aitoff(l,b,x,y)
               else
                  dummyeuler(1)=VPcentL
                  dummyeuler(2)=90.0-VPcentB
                  dummyeuler(3)=90.0
                  call radec2detYZ(l,b,dummyeuler,x,y)
               endif
            else
               if(tangential.ne.1) then
                  call aitoff(dble(source_ra(i)),
     $                 dble(source_dec(i)),x,y)
               else
                  dummyeuler(1)=VPcentRA
                  dummyeuler(2)=90.0-VPcentDEC
                  dummyeuler(3)=90.0
                  call radec2detYZ(dble(source_ra(i)),
     $                 dble(source_dec(i)),dummyeuler,x,y)
               endif
            endif
            call symbol_size(crab_flux,
     $           isgr_flux1(i)+isgr_flux2(i)+
     $           jemx_flux1(i)+jemx_flux2(i), size)
            call pgsch(size*2.0)
            call get_color(jemx_flux1(i),
     $           isgr_flux1(i)+isgr_flux2(i), cindex)
            call pgsci(cindex)
            if(plotsource.eq.0) call pgpoint(1,real(x),real(y),17)
            call pgsch(size*REAL(fontsize))
            plot_source_name=.false.
            if(tangential.ne.1) then
               if(x.ge.xmax.and.x.le.xmin.
     $              and.y.ge.ymin.and.y.le.ymax)
     $              plot_source_name=.true.
            else
               if(x.ge.-tanwidth.and.x.le.tanwidth.
     $              and.y.ge.-tanwidth.and.y.le.tanwidth)
     $              plot_source_name=.true.
            endif
         if(plotname.eq.0.and.plot_source_name.and.plotsource.eq.0) then
               call PGPTXT(real(x), real(y), 0.0, 0.5,
     $           source_name(i))

            endif
         end do
c      endif
      call pgsch(1.0)

      if(showgrids) then
         if(tangential.ne.1) then
            call drawgrid(xmin,xmax,ymin,ymax)
         else
            call draw_coordinate_grids(dummyeuler,-tanwidth,
     $           tanwidth,-tanwidth,tanwidth)
         endif
      endif

      if(plotfov.eq.0) then
         do i = 1, npointing
            if(index(mission,'Swift').gt.0) then 
               call Swift2Euler(ra_scx(i),dec_scx(i),roll, euler)
            else if(index(mission,'INTEGRAL').gt.0) then
               call scxz2euler(ra_scx(i),dec_scx(i),
     $              ra_scz(i),dec_scz(i),pointing_id(i),
     $              euler,status)
            endif
C     In case of Suzaku, Euler(1),Euler(2),Euler(3) are directly input.
            call draw_inst_fov(mission,euler, coordinates,
     $           pointing_id(i),tangential,dummyeuler,xmin,xmax,ymin,
     $           ymax,tanwidth,whichinst)
         end do
      endif

      call PGQINF ('CURSOR', cursor, length)
      if(cursor.eq.'YES') then
         if(showhelp.eq.0) then
         cursor_in=' '
         write(*,*) '##### Put the cursor on the screen         #####'
         write(*,*)
     $ 'Click     --> shift the viewport center onto the cursor'//
     $        ' position'
         write(*,*)
     $        'Type i    --> zoom in with the same viewport center'
         write(*,*)
     $        'Type o    --> zoom out with the same viewport center'
         write(*,*) 'Type I    --> zoom in with new viewport center'//
     $        ' at cursor position'
         write(*,*) 'Type O    --> zoom out with new viewport center'//
     $        ' at cursor position'
         write(*,*) ' '
         write(*,*)
     $   'Type g    --> Galactic coordinates (hold the grids)'
         write(*,*)
     $'Type e    --> Equatorial (J2000) coordinates (hold the grids)'
         write(*,*)
     $   'Type G    --> Galactic coordinates (hold the sky)'
         write(*,*)
     $'Type E    --> Equatorial (J2000) coordinates (hold the sky)'
         write(*,*)
     $        'Type t    --> tangential projection'
         write(*,*) ' '
         write(*,*)
     $   'Type s    --> toggle display sources or not'
         write(*,*)
     $   'Type n    --> toggle show source names or not'
         write(*,*)
     $   'Type c    --> toggle show coordinate grids or not'
         write(*,*) ' '
         write(*,*)
     $   'Type f    --> toggle display FOV or not'
         write(*,*)
     $'Type 1    --> toggle which inst FOV to display'
         write(*,*) ' '
         write(*,*)
     $   'Type d    --> toggle display image (when tangential'
     $        //' projection) or not'
         write(*,*)
     $'Type +    --> increase the image lower-threshold'//
     $        ' (too see only bright feature)'
         write(*,*)
     $'Type -    --> decrease the image upper-threshold'//
     $        ' (too see dim feature)'
         write(*,*)
     $'Type b/B  --> increase (b) or decrease (B) image binsize '//
     $        'between 64 and 1024'
         write(*,*)
     $'Type r    --> switch image transfer function'//
     $      ' (linear->log->square-root)'
         write(*,*) ' '

         write(*,*)
     $'Type p    --> read the cursor position (J2000 and Galactic'//
     $        ' coordinates)'
         write(*,*)
     $'Type h    --> hardcopy (pgplot.ps is made)'
         write(*,*) 'Type q    --> quit'
         showhelp = showhelp + 1
         endif
c     Control the cursor/keyboard inputs
         call pgcurs(cursorX, cursorY, cursor_in)
         if(cursor_in.eq.'q') then
            continue
         else
            if(cursor_in.eq.'i') then
               ViewPortXwidth= ViewPortXwidth/2.0
               ViewPortYwidth= ViewPortYwidth/2.0
            else if(cursor_in.eq.'o') then
               ViewPortXwidth=ViewPortXwidth*2.0
               ViewPortYwidth=ViewPortYwidth*2.0
               if(ViewPortXwidth.gt.360.0.and.
     $            ViewPortYwidth.gt.180.0) then
                  ViewPortXwidth=360.0
                  ViewPortYwidth=180.0
                  ViewPortCenterX=0.0
                  ViewPortCenterY=0.0
               endif
            else if(cursor_in.eq.'g') then
               tangential = 0
               coordinates=1
            else if(cursor_in.eq.'e') then
               tangential = 0
               coordinates=0
            else if(cursor_in.eq.'c') then
               showgrids=.not.showgrids
            else if(cursor_in.eq.'s') then
               if(plotsource.eq.0) then
                  plotsource=1
               else
                  plotsource=0
               endif
            else if(cursor_in.eq.'f') then
               if(plotfov.eq.0) then
                  plotfov=1
               else
                  plotfov=0
               endif
            else if(cursor_in.eq.'n') then
               if(plotname.eq.0) then
                  plotname=1
               else
                  plotname=0
               endif
            else if(cursor_in.eq.'1') then
               whichinst=mod(whichinst+1,16)
            else if(cursor_in.eq.'b') then
               if(outimagesize.eq.1024) outimagesize=32
               outimagesize=outimagesize*2
            else if(cursor_in.eq.'B') then
               if(outimagesize.eq.64) outimagesize=2048
               outimagesize=outimagesize/2
            else
               if(tangential.ne.1) then
                  call aitoffinverse(dble(cursorX),dble(cursorY),
     $              tmpra,tmpdec)
               else
                  if(coordinates.eq.1) then
                     dummyeuler(1)=VPcentL
                     dummyeuler(2)=90.0-VPcentB
                     dummyeuler(3)=90.0
                  else
                     dummyeuler(1)=VPcentRA
                     dummyeuler(2)=90.0-VPcentDEC
                     dummyeuler(3)=90.0
                  endif
                  call detYZ2radec(dble(cursorX),dble(cursorY),
     $                 dummyeuler,tmpra,tmpdec)
               endif
               if(tmpra.lt.0.0) tmpra=tmpra+360.0
               if(cursor_in.eq.'E') then
                  tangential = 0
                  if(coordinates.eq.1) then
                     coordinates=0
                     call lb2radec(tmpra,tmpdec,VpCentRA,VpCentDEC)
                     call aitoff(VpCentRA,VpCentDEC,tmp1,tmp2)
                     ViewPortCenterX=real(tmp1)
                     ViewPortCenterY=real(tmp2)
                  endif
               elseif(cursor_in.eq.'G') then
                  tangential = 0
                  if(coordinates.eq.0) then
                     coordinates=1
                     call radec2lb(tmpra,tmpdec,VpcentL,VpcentB)
                     call aitoff(VpcentL,VpcentB,tmp1,tmp2)
                     ViewPortCenterX=real(tmp1)
                     ViewPortCenterY=real(tmp2)
                  endif
               elseif(cursor_in.eq.'p') then
                  if(coordinates.eq.0) then
                     cursorRA =tmpRA
                     cursorDEC=tmpDEC
                     call radec2lb(cursorRA,cursorDEC,cursorL,cursorB)
                  else
                     cursorL =tmpRA
                     cursorB =tmpDEC
                     call lb2radec(cursorL,cursorB,cursorRA,cursorDEC)
                  endif
                  write(*,*)
                  write(*,*) '### Cursor Position ###'
                  write(*,'(a,f7.3,a,f7.3,a)')
     $                 '(ra,dec) = (', cursorRA,',', cursorDEC,')' 
                  write(*,'(a,f7.3,a,f7.3,a)')
     $                 '(l, b)   = (', cursorL,',', cursorB,')' 
                  write(*,*)
               elseif(cursor_in.eq.'t') then
                  tangential = 1
               elseif(cursor_in.eq.'r') then
                  transfer=mod(transfer+1,3)
               elseif(cursor_in.eq.'-') then
                  scale = scale*1.5
               elseif(cursor_in.eq.'+') then
                  scale = scale/1.5
               elseif(cursor_in.eq.'h') then
                  olddevice=device
                  device='/cps'
                  hardcopyistaken=.true.
               elseif(cursor_in.eq.'d') then
                  if(dispimage) then
                     dispimage=.false.
                     write(*,*) 'Not display image'
                  else
                     dispimage=.true.
                     write(*,*) 'Display image'
                  endif
               else
                  if(tangential.ne.1)  then
                     ViewPortcenterX =  cursorX
                     ViewPortcenterY =  cursorY
                  else
                     ViewPortCenterX=0.0
                     ViewPortCenterY=0.0
                     if(coordinates.eq.0) then
                        VPcentRA=tmpRA
                        VPcentDEC=tmpDEC
                        dummyeuler(1)=VPcentRA
                        dummyeuler(2)=90.0-VPcentDEC
                        dummyeuler(3)=90.0
                     else
                        VPcentL=tmpRA
                        VPcentB=tmpDEC
                        dummyeuler(1)=VPcentL
                        dummyeuler(2)=90.0-VPcentB
                        dummyeuler(3)=90.0
                     endif
                  endif
                  if(cursor_in.eq.'I') then
                     ViewPortXwidth= ViewPortXwidth/2.0
                     ViewPortYwidth= ViewPortYwidth/2.0
                  elseif(cursor_in.eq.'O') then
                     ViewPortXwidth=min(360.0,ViewPortXwidth*2.0)
                     ViewPortYwidth=min(180.0,ViewPortYwidth*2.0)
                  endif
               endif
            endif
            call pgeras
            go to 100
         endif
      endif
      if(hardcopyistaken) then
         hardcopyistaken=.false.
         device=olddevice
         go to 100
      endif
      call pgend
 999  continue
      
      end

      subroutine read_att_file(attfile,pointing_id,ra_scx,dec_scx,
     $     ra_scz,dec_scz,npointings,status)

      implicit none
      character attfile*(*)
      character(8) pointing_id(*)
      double precision ra_scx(*), dec_scx(*), ra_scz(*), dec_scz(*)
      integer npointings, status, blocksize, unit, hdutype
      character comment*80
      logical anyf
      integer object1, type, rilstat, colnum

      call ftgiou(object1,status)
      call FTTOPN(object1, attfile, 0, status)
      if(status.ne.0) then
         call fcecho('Error reading the attitude or index file.')
         return
      endif
C     Note, if the input is ScWIndex file, status=-104, but this is
C     acceptable, and keep going.
      call FTGNRW(object1, npointings, status)
      call FTGCNO(object1, .false.,'pointing_id', colnum,status)
      call FTGCVS(object1,colnum,1,1,npointings,' ',
     $     pointing_id,anyf,status)

      if(status.ne.0) then
C     Note, if the input is ScWIndex file, 'pointing_id' does not exist,
C     but ScWID is available
         status=0
         call fcecho('SWID is read from ScW IDX file')
         call FTGCNO(object1, .false.,'SWID', colnum,status)
         call FTGCVS(object1,colnum,1,1,npointings,' ',
     $        pointing_id,anyf,status)
      endif
      call FTGCNO(object1, .false.,'RA_SCX', colnum,status)
      call ftgcvd(object1, colnum, 1, 1, npointings, 0.0D0, ra_scx(1),
     $     anyf, status)
      call FTGCNO(object1, .false.,'DEC_SCX', colnum,status)
      call ftgcvd(object1, colnum, 1, 1, npointings, 0.0D0, dec_scx(1),
     $     anyf, status)
      call FTGCNO(object1, .false.,'RA_SCZ', colnum,status)
      call ftgcvd(object1, colnum, 1, 1, npointings, 0.0D0, ra_scz(1),
     $     anyf, status)
      call FTGCNO(object1, .false.,'DEC_SCZ', colnum,status)
      call ftgcvd(object1, colnum, 1, 1, npointings, 0.0D0, dec_scz(1),
     $     anyf, status)
      end

      subroutine rot_x(in,ang,out)
      implicit none
      double precision in(3),ang,out(3)
      double precision mat(3,3)
      double precision deg2rad
      data deg2rad/1.745329252d-2/
      mat(1,1)= 1.0
      mat(1,2)= 0.0
      mat(1,3)= 0.0
      mat(2,1)= 0.0
      mat(2,2)= cos(ang*deg2rad)
      mat(2,3)= sin(ang*deg2rad)
      mat(3,1)= 0.0
      mat(3,2)= -sin(ang*deg2rad)
      mat(3,3)= cos(ang*deg2rad)
      call calmat(in,mat,out)
      end

      subroutine rot_y(in,ang,out)
      implicit none
      double precision in(3),ang,out(3)
      double precision mat(3,3)
      double precision  deg2rad
      data deg2rad/1.745329252d-2/
      mat(1,1)= cos(ang*deg2rad)
      mat(1,2)= 0.0
      mat(1,3)= -sin(ang*deg2rad)
      mat(2,1)= 0.0
      mat(2,2)= 1.0
      mat(2,3)= 0.0
      mat(3,1)= sin(ang*deg2rad)
      mat(3,2)= 0.0
      mat(3,3)= cos(ang*deg2rad)
      call calmat(in,mat,out)
      end

      subroutine rot_z(in,ang,out)
      implicit none
      double precision in(3),ang,out(3)
      double precision mat(3,3)
      double precision  deg2rad
      data deg2rad/1.745329252d-2/
      mat(1,1)= cos(ang*deg2rad)
      mat(1,2)= sin(ang*deg2rad)
      mat(1,3)= 0.0
      mat(2,1)= -sin(ang*deg2rad)
      mat(2,2)= cos(ang*deg2rad)
      mat(2,3)= 0.0
      mat(3,1)= 0.0
      mat(3,2)= 0.0
      mat(3,3)= 1.0
      call calmat(in,mat,out)
      end

      subroutine calmat(in,mat,out)
      implicit none
      double precision in(3),out(3),mat(3,3)
      integer i, j
      do i=1,3
         out(i)=0.0
         do j=1,3
            out(i)=out(i)+mat(i,j)*in(j)
         end do
      end do
      end

      subroutine radec2point(ra,dec,point)
      implicit none
      double precision  ra, dec, point(3), deg2rad
      data deg2rad/1.745329252d-2/
      point(1)= sin(dec*deg2rad)
      point(2)= cos(dec*deg2rad)*cos(ra*deg2rad)
      point(3)= cos(dec*deg2rad)*sin(ra*deg2rad)
      end

      subroutine point2radec(point,ra,dec)
      implicit none
      double precision  ra, dec, point(3), zero, deg2rad
      data zero/1e-3/
      data deg2rad/1.745329252d-2/
      if(abs(point(2)).le.ZERO.and.abs(point(3)).le.ZERO) then
         ra=0.0
         dec=90.0
      else
         dec=atan(point(1)/sqrt(point(2)**2+point(3)**2))/deg2rad
         if(point(2).eq.0.0) then
            if(point(3).gt.0) then
               ra =90.0
            else
               ra =270.0
            endif
         else if(point(2).gt.0.0) then
            ra =atan(point(3)/point(2))/deg2rad
         else
            ra =atan(point(3)/point(2))/deg2rad+180.0
         endif
      endif
      if(ra.lt.0.0) then
         ra=ra+360.0
      endif
      end

      subroutine eqvect2galvect(eqvect,galvect)
      implicit none
C     eqvect and galvect are pointing vector in the Equatorial
C     and Galactic coordinates rspectively
      double precision eqvect(3), galvect(3)
      double precision tmp1(3), tmp2(3)
      double precision alpha, delta, theta
C     J2000 Galactic center alpha and delta
      alpha = 266.404996D0
      delta =  -28.936172D0
C     theta is the rotation angle around the axis to the Galactic center
      theta =  58.598666D0
      call rot_x(eqvect,alpha,tmp1)
      call rot_z(tmp1,-delta,tmp2)
      call rot_y(tmp2,theta,galvect)
      end      

      subroutine galvect2eqvect(galvect,eqvect)
      implicit none
C     eqvect and galvect are pointing vector in the Equatorial
C     and Galactic coordinates rspectively
      double precision eqvect(3), galvect(3)
      double precision tmp1(3), tmp2(3)
      double precision alpha, delta, theta
C     J2000  Galactic center alpha and delta
      alpha = 266.404996D0
      delta =  -28.936172D0
C     theta is the rotation angle around the axis to the Galactic center
      theta =  58.598666D0
      call rot_y(galvect,-theta,tmp1)
      call rot_z(tmp1, delta,tmp2)
      call rot_x(tmp2,-alpha,eqvect)
      end

      subroutine radec2lb(ra,dec,l,b)
      implicit none
      double precision ra, dec, l, b
      double precision radecpoint(3),lbpoint(3)
      call radec2point(ra,dec,radecpoint)
      call eqvect2galvect(radecpoint,lbpoint)
      call point2radec(lbpoint,l,b)
      end

      subroutine lb2radec(l,b,ra,dec)
      implicit none
      double precision ra, dec, l, b
      double precision radecpoint(3),lbpoint(3)
      call radec2point(l,b,lbpoint)
      call galvect2eqvect(lbpoint,radecpoint)
      call point2radec(radecpoint,ra,dec)
      end

      subroutine aitoff(phi,theta,x,y)
      implicit none
      double precision theta, phi, x,y
      double precision alpha
      double precision tmpphi
      double precision deg2rad
      data deg2rad/1.745329252d-2/
      tmpphi = phi
      if(tmpphi .gt. 180.) tmpphi = tmpphi - 360.0D0
      alpha =
     $  sqrt(2.0D0/(1.0D0+cos(theta*deg2rad)*cos(tmpphi*deg2rad/2.0D0)))
      alpha = 180.0D0/3.1415D0*alpha
      x  = 2.0D0*alpha*cos(theta*deg2rad)*sin(tmpphi/2.0D0*deg2rad)
      y  = alpha*sin(theta*deg2rad)
      end

      subroutine drawgrid(xmin,xmax,ymin,ymax)
      implicit none
      double precision theta,phi,x,y, tmpphi
      integer i, j
      character number*8
      real xmin, xmax, ymin, ymax
      logical moved
      double precision decmin,decmax,ramin,ramax
      double precision tmpx,tmpy,tmpra,tmpdec,xwidth
      integer numDecgrid,numRAgrid
c     note xmin, xmax, ymin, ymax are given in the
c     aitoff-projected coordinates.
      xwidth=xmin-xmax
      if(xwidth.ge.180.0) then
         ramin=180.0
         ramax=-180.0
         decmin=-90.0
         decmax=90.0
      else
         ramin=-999.
         ramax=999.
         decmin=999.
         decmax=-999.
         do i =1, 3
            tmpx = xmax+(xmin-xmax)/2.0*(i-1)
            do j = 1, 3
               tmpy = ymin+(ymax-ymin)/2.0*(j-1)
               call aitoffinverse(tmpx,tmpy,tmpra,tmpdec)
               if(tmpra.gt.-900.and.tmpdec.gt.-900) then
                  ramin=max(tmpra,ramin)
                  ramax=min(tmpra,ramax)
                  decmin=min(tmpdec,decmin)
                  decmax=max(tmpdec,decmax)
               endif
               if(xwidth.ge.0.1) then
                  if(ramin.ge.120.0) ramin=180.
                  if(ramax.le.-120.0) ramax=-180.
               endif
               if(xwidth.ge.1.) then
                  if(decmin.le.-85.0) decmin=-90.0
                  if(decmax.ge.85.0) decmax=90.0
               endif
            end do
         end do
      endif

      call pgsci(1)
      call pgsls(4)
      numDecgrid=12
      numRAgrid=24
      do i = -numDecgrid/2,numDecgrid/2,1
c     light yellow color
         call pgscr(4,0.75,0.75,0.0)
         call pgsci(4)
         theta = (decmax+decmin)/2.0+(decmax-decmin)/numDecgrid*i
         moved=.false.
         do j=1,100
            phi =ramin+(ramax-ramin)/99.0*(j-1)
            call aitoff(phi,theta,x,y)
            if(.not.moved.and.real(x).ge.xmax.and.real(x).le.xmin.and.
     $         real(y).ge.ymin.and.real(y).le.ymax) then
               call pgmove(REAL(x),REAL(y))
               moved=.true.
            endif
            if(moved) then
               call pgdraw(REAL(x),REAL(y))
            endif
            if(j.eq.50) then
               if(xmin-xmax.lt.0.2) then
                  write(number,'(f7.3)') theta
               elseif(xmin-xmax.lt.2.0) then
                  write(number,'(f6.2)') theta
               elseif(xmin-xmax.lt.20.0) then
                  write(number,'(f5.1)') theta
               else
                  write(number,'(f4.0)') theta
               endif
               call pgptext(REAL(x),REAL(y),0.0,0.5,number)
               call pgmove(REAL(x),REAL(y))
            endif
         end do
      end do
      do j=-numRAgrid/2,numRAgrid/2
c     light green color
         call pgscr(2,0.5,1.0,0.5)
         call pgsci(2)
         phi = (ramax+ramin)/2.0+(ramax-ramin)/numRAgrid*j
         moved=.false.
         do i = 1, 100
            theta = decmin+(decmax-decmin)/99.0*(i-1)
            call aitoff(phi,theta,x,y)
            if(.not.moved.and.real(x).ge.xmax.and.real(x).le.xmin.and.
     $           real(y).ge.ymin.and.real(y).le.ymax) then
               call pgmove(REAL(x),REAL(y))
               moved=.true.
            endif
            if(moved) then
               call pgdraw(REAL(x),REAL(y))
            endif
            if(phi.lt.0.0)  then
               tmpphi=phi+360.0
            else
               tmpphi=phi
            endif
            if(i.eq.51.and.mod(j,2).eq.0) then
               if(ymax-ymin.lt.0.1) then
                  if(mod(j,6).eq.0) then
                     write(number,'(f8.3)') tmpphi
                     call pgptext(REAL(x),REAL(y),0.0,0.7,number)
                  endif
               elseif(ymax-ymin.lt.1.0) then
                  if(mod(j,4).eq.0) then
                     write(number,'(f7.2)') tmpphi
                     call pgptext(REAL(x),REAL(y),0.0,0.7,number)
                  endif
               elseif(ymax-ymin.lt.10.0) then
                  write(number,'(f6.1)') tmpphi
                  call pgptext(REAL(x),REAL(y),0.0,0.7,number)
               else
                  write(number,'(f5.0)') tmpphi
                  call pgptext(REAL(x),REAL(y),0.0,0.7,number)
               endif
               call pgmove(REAL(x),REAL(y))
            endif
         end do
      end do
      call pgsls(1)
      call pgsci(1)
      end

      subroutine readfits(catalog,status)

      implicit none
      character*(*) catalog
      integer status, unit
      character comment*80

      integer maxsources
      parameter (maxsources=2000)
      character(20) source_name(maxsources)
      real source_ra(maxsources), source_dec(maxsources)
      real isgr_flux1(maxsources), isgr_flux2(maxsources),
     $     jemx_flux1(maxsources), jemx_flux2(maxsources)
      integer nsource
      common/catalog/source_name, source_ra, source_dec, nsource,
     $     isgr_flux1, isgr_flux2, jemx_flux1, jemx_flux2

      logical anyf
      integer i, blocksize, hdutype
      integer object1, type, colnum

      call ftgiou(object1,status)
      call FTTOPN(object1, catalog, 0, status)
      if(status.ne.0) return
      call FTGNRW(object1, nsource, status)

      call FTGCNO(object1, .false.,'name', colnum,status)

      call FTGCVS(object1,colnum,1,1,nsource,' ',
     $     source_name,anyf,status)


      call FTGCNO(object1, .false.,'ra_obj', colnum,status)
      call FTGCVe(object1,colnum,1,1,nsource,0.0D0,
     $     source_ra(1),anyf,status)

      call FTGCNO(object1, .false.,'dec_obj', colnum,status)
      call FTGCVe(object1,colnum,1,1,nsource,0.0D0,
     $     source_dec(1),anyf,status)

      call FTGCNO(object1, .false.,'isgr_flux_1', colnum,status)
      call FTGCVe(object1,colnum,1,1,nsource,0.0D0,
     $     isgr_flux1(1),anyf,status)

      call FTGCNO(object1, .false.,'jemx_flux_2', colnum,status)
      call FTGCVe(object1,colnum,1,1,nsource,0.0D0,
     $     jemx_flux2(1),anyf,status)

      call FTGCNO(object1, .false.,'jemx_flux_1', colnum,status)
      call FTGCVe(object1,colnum,1,1,nsource,0.0D0,
     $     jemx_flux1(1),anyf,status)

      call FTGCNO(object1, .false.,'jemx_flux_2', colnum,status)
      call FTGCVe(object1,colnum,1,1,nsource,0.0D0,
     $     jemx_flux2(1),anyf,status)

      call ftclos(object1,status)

      end

      subroutine get_color(softflux, hardflux, cindex)
      implicit none
      real softflux, hardflux
      integer cindex
      real hardness
      if(softflux+hardflux.le.0.0) then
         cindex=16
      else
         hardness = (hardflux-softflux)/(hardflux+softflux)
         if(hardness.ge.1.0) then
            cindex=47
         else
            cindex=32+hardness/0.0625
         endif
      endif
      end

      subroutine symbol_size(crab_flux, source_flux,size)
      implicit none
      real crab_flux, source_flux
      real size

      size = 0.4*log10(min(10.0,max(source_flux/crab_flux,0.001)))+2.0
      end

      subroutine aitoffinverse(x, y, phi,theta)
      implicit none
      double precision theta, phi, x,y
      double precision tmpX, tmpY, tmp
      double precision  Z, PI, deg2rad
      data PI/3.1415926535/
      data deg2rad/1.745329252d-2/
C     note, this routine is not perfect.
c     for example, this give (152.96994,0) for (x,y)=(180.0,0).
            
      tmp = (PI/180.0*x/4.0)**2+(PI/180.0*y/2.0)**2
      if(tmp.gt.1.0) then
         phi = -999.
         theta=-999.
         return
      endif
      Z=sqrt(1.0-tmp)
      tmpX=2.0*Z**2-1
      tmpY=PI/180.0*Z/2.0*x
      phi = 2.0*asin(tmpY/sqrt(tmpX**2+tmpY**2))/deg2rad
      theta = asin(PI/180.0*y*Z)/deg2rad
      end

      subroutine scxz2euler(ra_scx, dec_scx, ra_scz, dec_scz,
     $     pointing_id,ea, status)
C     This subroutine calculates the X-Z-X Euler angle, starting
C     the X-axis pointing North, Y-axis pointing Equinox, with
C     the normal direction of the rotation (seeing from the
C     Origin, always clockwize). 
C     In the case of INTEGRAL, X-direction (pointing angle) and
C     Z-direction are specified in the attitude file, then the
C     euler angles thus defined are returned.
      
      implicit none
      double precision ra_scx, dec_scx, ra_scz, dec_scz, ea(3)
      integer status
      character pointing_id*(*)
      double precision scx_point(3), scz_point(3)
      double precision tmpZ(3), pointX(3),pointZ(3), op(3),ip,iptmp
      integer rilStat
      character string*128
      double precision tmp1(3)

      integer warning_0

      double precision  zero, deg2rad
      data zero/1e-3/
      data deg2rad/1.745329252d-2/

      call radec2point(ra_scx, dec_scx, scx_point)
      call radec2point(ra_scz, dec_scz, scz_point)
      ip =scx_point(1)*scz_point(1)+scx_point(2)*scz_point(2)
     $   +scx_point(3)*scz_point(3)
      if(abs(ip).gt.0.01) then
         if(abs(ip).lt.0.05) then
            call FCECHO ('X- and Z-axes are not perfectly orthogonal.')
            write(string,'(a,a,a,f8.3,a)') 'Pointing ',pointing_id,
     $           ': angle between X- and Z-axes is',
     $           acos(ip)/deg2rad, ' deg.'
            call FCECHO (string)
            call FCECHO ('Still this FOV will be drawn.')
         else
            call FCECHO ('X- and Z-axes are not orthogonal.')
            write(string,'(a,a,a,f8.3,a)') 'Pointing ',pointing_id,
     $           ': angle between X- and Z-axes is',
     $           acos(ip)/deg2rad, ' deg.'
            call FCECHO (string)
            write(string,'(a)') 'Z-axis is calculated to point North. '
            call FCECHO (string)
            status = 0
C        Get the Z-axis to point North, when X-axis is given
            tmpZ(1)=0.0
            tmpZ(2)=0.0
            tmpZ(3)=1.0
            call rot_x(tmpZ, -90.0D0, tmp1)
            call rot_z(tmp1, -(90.0D0-dec_scx), tmpZ)
            call rot_x(tmpZ, -ra_scx, scz_point)
            call point2radec(scz_point, ra_scz, dec_scz)
         endif
      endif
      ea(1)=ra_scx
      ea(2)=90.0-dec_scx
      call radec2point(ra_scx, dec_scx, pointX)
      tmpZ(1)= 0.0
      tmpZ(2)=-sin(ra_scx*deg2rad)
      tmpZ(3)= cos(ra_scx*deg2rad)
      call radec2point(ra_scz, dec_scz, pointZ)
      call InnerProduct(tmpZ,pointZ,ip)
      call OuterProduct(tmpZ,pointZ,op)
      call InnerProduct(op, pointX,iptmp)
      if(iptmp.ge.0.0) then
         ea(3) = acos(ip)/deg2rad
      else
         ea(3) = 360.0-acos(ip)/deg2rad
      endif
      end

      subroutine InnerProduct(a,b,ip)
      implicit none
      double precision a(3), b(3), ip 
      ip = a(1)*b(1)+a(2)*b(2)+a(3)*b(3)
      end     

      subroutine OuterProduct(a,b,op)
      implicit none
      double precision a(3), b(3),op(3)
      op(1) = a(2)*b(3)-a(3)*b(2)
      op(2) = a(3)*b(1)-a(1)*b(3)
      op(3) = a(1)*b(2)-a(2)*b(1)
      end


      subroutine detYZ2radec(detY,detZ,euler,ra,dec)
      implicit none

      double precision ra, dec, euler(3), detY, detZ
      double precision  sky(3),deg2rad,
     $     sat(3),tmp1(3),tmp2(3),ray(3),ray_alpha,ray_delta,tmp3(3)
      integer i, coordinates
      data deg2rad/1.745329252d-2/

C     Flip the Z-axis (look up --> look down).
      detZ=-detZ

      if(detY.eq.0.0) then
         if(detZ.gt.0) then
            ray_alpha = 90.0D0
         else
            ray_alpha = -90.0D0
         endif
      elseif(detY.gt.0.0) then
         ray_alpha = atan(detZ/detY)/deg2rad
      else
         ray_alpha = atan(detZ/detY)/deg2rad+180.0D0

      endif

c     TAN projection. 2002-10-01
      if(detY.eq.0.and.detZ.eq.0) then
         ray_delta = -90.0D0
      else
         ray_delta = -atan(1.0/(sqrt(detY**2+detZ**2)*deg2rad))/deg2rad
      endif

      call radec2point(ray_alpha,ray_delta,ray)


C     ray is the direction vector of the ray *from* the point source,
C     and the sat is the direction vector of the source in the
C     satellite coordinate
      sat(1)=-ray(1)
      sat(2)=-ray(2)
      sat(3)=-ray(3)
C     calculate the pointing vector in the celestial coordinates
      call rot_x(sat, -euler(3),tmp1)
      call rot_z(tmp1,-euler(2),tmp2)
      call rot_x(tmp2,-euler(1),sky)
      call point2radec(sky,ra,dec)
      end 

      subroutine draw_inst_fov(mission,euler,coordinates,pointing_id,
     $     tangential,dummyeuler,xmin,xmax,ymin,ymax,tanwidth,whichinst)
      implicit none
      character mission*(*)
      real xmin,xmax,ymin,ymax,tanwidth
      character pointing_id*(*)
      double precision euler(3)
      integer coordinates,tangential,whichinst
      real  theta,jemx_radius, spi_side
      double precision ra,dec,detY,detZ,l,b, x, y,FOVcenterX,FOVcenterY,
     $     FOVcenterRA,FOVcenterDEC,dummyeuler(3)
      integer  i, j
      real ibis_x(5), ibis_y(5)
      real spi_x(7), spi_y(7)
      logical plot_fov
      integer bit1, bit2, bit3, bit4
      character string*(99)
      double precision BAT_5percent(3,15),BAT_50percent(3,19),
     $     BAT_95percent(3,11)
      double precision sky(3), tmp0(3),tmp1(3), tmp2(3)
      real suzaku_x(5),suzaku_y(5)
      double precision  oldx, drawline
      data drawline/40.0/

      call pgsci(1)
      call pgslw(2)

      if(index(mission,'Suzaku').gt.0) then 
C     Suzaku XIS, PIN and GSO FOV are drawn
C     Note, Suzaku uses Z-Y-Z euler angle, and INTEGRAL uses X-Z-X euler angle
C     (which fovdsp is based on).  Therefore, Suzaku XY detector plane is
C     YZ plain in fovdsp.

C     For three instruments of Suzaku
         do j = 1, 4
            if(j.eq.1) then
C     XIS FOV is 18 arcmin x 18 arcmin 
               suzaku_x(1)=-0.150
               suzaku_y(1)=-0.150
               suzaku_x(2)= 0.150
               suzaku_y(2)=-0.150
               suzaku_x(3)= 0.150
               suzaku_y(3)= 0.150
               suzaku_x(4)=-0.150
               suzaku_y(4)= 0.150
               suzaku_x(5)=suzaku_x(1)
               suzaku_y(5)=suzaku_y(1)
            else if (j.eq.2) then
C     PIN FOV is 0.56 deg x 0.56 deg (FWHM)
               suzaku_x(1)=-0.560
               suzaku_y(1)=-0.560
               suzaku_x(2)= 0.560
               suzaku_y(2)=-0.560
               suzaku_x(3)= 0.560
               suzaku_y(3)= 0.560
               suzaku_x(4)=-0.560
               suzaku_y(4)= 0.560
               suzaku_x(5)=suzaku_x(1)
               suzaku_y(5)=suzaku_y(1)
            else if (j.eq.3) then
C     GSO FOV is 4.6 deg x 4.6 deg
               suzaku_x(1)=-4.60
               suzaku_y(1)=-4.60
               suzaku_x(2)= 4.60
               suzaku_y(2)=-4.60
               suzaku_x(3)= 4.60
               suzaku_y(3)= 4.60
               suzaku_x(4)=-4.60
               suzaku_y(4)= 4.60
               suzaku_x(5)=suzaku_x(1)
               suzaku_y(5)=suzaku_y(1)
            else if (j.eq.4) then
C     HXD nominal position at (DETX,DETY)=(-3.5 arcmin, 0.0)
C     We draw a 1arcmin x 1 arcmin square.
               suzaku_x(1)=-8.3333e-3-5.833333e-2
               suzaku_y(1)=-8.3333e-3
               suzaku_x(2)= 8.3333e-3-5.833333e-2
               suzaku_y(2)=-8.3333e-3
               suzaku_x(3)= 8.3333e-3-5.833333e-2
               suzaku_y(3)= 8.3333e-3
               suzaku_x(4)=-8.3333e-3-5.833333e-2
               suzaku_y(4)= 8.3333e-3
               suzaku_x(5)=suzaku_x(1)
               suzaku_y(5)=suzaku_y(1)
            endif
            do i = 1, 5
               call pgsls(1)
               call detYZ2radec(dble(suzaku_x(i)),
     $              dble(suzaku_y(i)),euler,ra,dec)
               if(coordinates.eq.1) then
                  call radec2lb(ra,dec,l,b)
                  if(tangential.ne.1) then
                     call aitoff(l,b,x,y)
                  else
                     call radec2detYZ(l,b,dummyeuler,x,y)            
                  endif
               else
                  if(tangential.ne.1) then
                     call aitoff(ra,dec,x,y)
                  else
                     call radec2detYZ(ra,dec,dummyeuler,x,y)            
                  endif
               endif
               if(i.eq.1) then
                  call pgmove(real(x),real(y))
               else
                  if(abs(oldx-x).lt.drawline) then
                     call pgdraw(real(x),real(y))
                  else
                     call pgmove(real(x),real(y))
                  endif
               endif
               oldx=x
            end do
         end do

      else if(index(mission,'Swift').gt.0) then 
C     Get Swift BAT FOV on the detector 
         call  BAT_FOV(BAT_5percent, BAT_50percent, BAT_95percent)

C     Made 95 % contour map
         do i=1,11
            call rot_x(BAT_95percent(1,i), -euler(3),tmp1)
            call rot_z(tmp1,-euler(2),tmp2)
            call rot_x(tmp2,-euler(1),sky)
            call point2radec(sky,ra,dec)
            if(coordinates.eq.1) then
               call radec2lb(ra,dec,l,b)
               if(tangential.ne.1) then
                  call aitoff(l,b,x,y)
               else
                  call radec2detYZ(l,b,dummyeuler,x,y)            
               endif
            else
               if(tangential.ne.1) then
                  call aitoff(ra,dec,x,y)
               else
                  call radec2detYZ(ra,dec,dummyeuler,x,y)            
               endif
            endif
            if(i.eq.1) then
C     To check the roll definition. This point (i.eq.1) is one the X-Z plane,
C     slightely inclined from the X-axis to the Z diretion.
C     If roll=0,  this point should be on the North of the pointing direction.
C     If roll=90, this point should be on the Eastof the pointing direction.
c              call pgpt(1,real(x),real(y),10)
               call pgmove(real(x),real(y))
            else
               if(abs(oldx-x).lt.drawline.or.tangential.eq.1)
     $              then
                  call pgsls(4)
                  call pgdraw(real(x),real(y))
               else
                  call pgmove(real(x),real(y))
               endif
            endif
            oldx=x
         end do

C     Made 50 % contour map
         do i=1,19
            call rot_x(BAT_50percent(1,i), -euler(3),tmp1)
            call rot_z(tmp1,-euler(2),tmp2)
            call rot_x(tmp2,-euler(1),sky)
            call point2radec(sky,ra,dec)
            if(coordinates.eq.1) then
               call radec2lb(ra,dec,l,b)
               if(tangential.ne.1) then
                  call aitoff(l,b,x,y)
               else
                  call radec2detYZ(l,b,dummyeuler,x,y)            
               endif
            else
               if(tangential.ne.1) then
                  call aitoff(ra,dec,x,y)
               else
                  call radec2detYZ(ra,dec,dummyeuler,x,y)            
               endif
            endif
            if(i.eq.1) then
               call pgmove(real(x),real(y))
            else
               if(abs(oldx-x).lt.drawline*1.5.or.tangential.eq.1)
     $              then
                  call pgsls(2)
                  call pgdraw(real(x),real(y))
               else
                  call pgmove(real(x),real(y))
               endif
            endif
            oldx=x
         end do

C     Made 5 % contour map
         do i=1,15
            call rot_x(BAT_5percent(1,i), -euler(3),tmp1)
            call rot_z(tmp1,-euler(2),tmp2)
            call rot_x(tmp2,-euler(1),sky)
            call point2radec(sky,ra,dec)
            if(coordinates.eq.1) then
               call radec2lb(ra,dec,l,b)
               if(tangential.ne.1) then
                  call aitoff(l,b,x,y)
               else
                  call radec2detYZ(l,b,dummyeuler,x,y)            
               endif
            else
               if(tangential.ne.1) then
                  call aitoff(ra,dec,x,y)
               else
                  call radec2detYZ(ra,dec,dummyeuler,x,y)            
               endif
            endif
            if(i.eq.1) then
               call pgmove(real(x),real(y))
            else
               if(abs(oldx-x).lt.drawline*2.0.or.tangential.eq.1)
     $              then
                  call pgsls(1)
                  call pgdraw(real(x),real(y))
               else
                  call pgmove(real(x),real(y))
               endif
            endif
            oldx=x
         end do


      else if(index(mission,'INTEGRAL').gt.0) then
C     INTEGRAL FOV is drawn

c     whichinst is from 0 to 15
         bit1=whichinst/8
         bit2 =(whichinst - bit1*8)/4
         bit3 =(whichinst-bit1*8-bit2*4)/2
         bit4 =(whichinst-bit1*8-bit2*4-bit3*2)
c     bit1,2,3,4 contols if each item is displayed or not
         
         call pgsls(1)
         detY=0.0
         detZ=0.0
         call detYZ2radec(detY,detZ,euler,FOVcenterRA,FOVcenterDEC)
         if(coordinates.eq.1) then
            call radec2lb(FOVcenterRA,FOVcenterDEC,l,b)
            if(tangential.ne.1) then
               call aitoff(l,b,x,y)
            else
               call radec2detYZ(l,b,dummyeuler,x,y)            
            endif
         else
            if(tangential.ne.1) then
               call aitoff(FOVcenterRA,FOVcenterDEC,x,y)
            else
               call radec2detYZ(FOVcenterRA,FOVcenterDEC,
     $              dummyeuler,x,y)            
            endif
         endif
         plot_fov=.false.
         if(tangential.ne.1) then
            if(x.ge.xmax.and.x.le.xmin.
     $           and.y.ge.ymin.and.y.le.ymax)
     $           plot_fov=.true.
         else
            if(x.ge.-tanwidth.and.x.le.tanwidth.
     $           and.y.ge.-tanwidth.and.y.le.tanwidth)
     $           plot_fov=.true.
         endif
         
         if(plot_fov) then 
            if(bit4.eq.0) then
c     show the pointing ID
               call pgptxt(real(x),real(y), 0.0,0.5,pointing_id)
            endif
            call pgsls(1)
            
            if(bit3.eq.0) then
c     jem-x
C     Fully coded FOV
               call pgsls(1)
               theta = 0.0
               jemx_radius = 2.4
               do i = 0, 100
                  theta = (6.283185e-2)*i
                  call detYZ2radec(dble(jemx_radius*cos(theta)),
     $                 dble(jemx_radius*sin(theta)), euler,ra,dec)
                  if(coordinates.eq.1) then
                     call radec2lb(ra,dec,l,b)
                     if(tangential.ne.1) then
                        call aitoff(l,b,x,y)
                     else
                        call radec2detYZ(l,b,dummyeuler,x,y)            
                     endif
                  else
                     if(tangential.ne.1) then
                        call aitoff(ra,dec,x,y)
                     else
                        call radec2detYZ(ra,dec,dummyeuler,x,y)            
                     endif
                  endif
                  if(i.eq.0) then
                     call pgmove(real(x),real(y))
                  else
                     if(abs(oldx-x).lt.drawline) then
                        call pgdraw(real(x),real(y))
                     else
                        call pgmove(real(x),real(y))
                     endif
                  endif
                  oldx=x
               end do
C     zero response FOV
               call pgsls(4)
               theta = 0.0
               jemx_radius = 6.6
               do i = 0, 100
                  theta = (6.283185e-2)*i
                  call detYZ2radec(dble(jemx_radius*cos(theta)),
     $                 dble(jemx_radius*sin(theta)), euler,ra,dec)
                  if(coordinates.eq.1) then
                     call radec2lb(ra,dec,l,b)
                     if(tangential.ne.1) then
                        call aitoff(l,b,x,y)
                     else
                        call radec2detYZ(l,b,dummyeuler,x,y)            
                     endif
                  else
                     if(tangential.ne.1) then
                        call aitoff(ra,dec,x,y)
                     else
                        call radec2detYZ(ra,dec,dummyeuler,x,y)            
                     endif
                  endif
                  if(i.eq.0) then
                     call pgmove(real(x),real(y))
                  else
                     if(abs(oldx-x).lt.drawline) then
                        call pgdraw(real(x),real(y))
                     else
                        call pgmove(real(x),real(y))
                     endif
                  endif
                  oldx=x
               end do
            endif
            call pgsls(1)
            
            if(bit2.eq.0) then
c     ibis fully coded
               ibis_x(1)=-4.5
               ibis_x(2)= 4.5
               ibis_x(3)= 4.5
               ibis_x(4)=-4.5
               ibis_x(5)=-4.5
               ibis_y(1)=-4.5
               ibis_y(2)=-4.5
               ibis_y(3)= 4.5
               ibis_y(4)= 4.5
               ibis_y(5)=-4.5
               do i = 1, 5
                  call detYZ2radec(dble(ibis_x(i)),
     $                 dble(ibis_y(i)),euler,ra,dec)
                  if(coordinates.eq.1) then
                     call radec2lb(ra,dec,l,b)
                     if(tangential.ne.1) then
                        call aitoff(l,b,x,y)
                     else
                        call radec2detYZ(l,b,dummyeuler,x,y)            
                     endif
                  else
                     if(tangential.ne.1) then
                        call aitoff(ra,dec,x,y)
                     else
                        call radec2detYZ(ra,dec,dummyeuler,x,y)            
                     endif
                  endif
                  if(i.eq.1) then
                     call pgmove(real(x),real(y))
                  else
                     if(i.eq.2) call pgsls(2)
                     if(abs(oldx-x).lt.drawline) then
                        call pgdraw(real(x),real(y))
                     else
                        call pgmove(real(x),real(y))
                     endif
                     call pgsls(1)
                  endif
                  oldx=x
               end do
c     ibis zero response
               ibis_x(1)=-15.3
               ibis_x(2)= 15.3
               ibis_x(3)= 15.3
               ibis_x(4)=-15.3
               ibis_x(5)=-15.3
               ibis_y(1)=-15.3
               ibis_y(2)=-15.3
               ibis_y(3)= 15.3
               ibis_y(4)= 15.3
               ibis_y(5)=-15.3
               do i = 1, 5
                  call detYZ2radec(dble(ibis_x(i)),
     $                 dble(ibis_y(i)),euler,ra,dec)
                  if(coordinates.eq.1) then
                     call radec2lb(ra,dec,l,b)
                     if(tangential.ne.1) then
                        call aitoff(l,b,x,y)
                     else
                        call radec2detYZ(l,b,dummyeuler,x,y)            
                     endif
                  else
                     if(tangential.ne.1) then
                        call aitoff(ra,dec,x,y)
                     else
                        call radec2detYZ(ra,dec,dummyeuler,x,y)            
                     endif
                  endif
                  if(i.eq.1) then
                     call pgmove(real(x),real(y))
                  else
                     if(i.eq.2) call pgsls(2)
                     if(abs(oldx-x).lt.drawline) then
                        call pgdraw(real(x),real(y))
                     else
                        call pgmove(real(x),real(y))
                     endif
                     call pgsls(4)
                  endif
                  oldx=x
               end do
            endif
            call pgsls(1)
            
            if(bit1.eq.0) then
c     spi fully coded
               spi_side=8.0
               do i = 1, 7
                  theta = 1.0472*(i-1)
                  spi_x(i)=spi_side*cos(theta)
                  spi_y(i)=spi_side*sin(theta)
                  call detYZ2radec(dble(spi_x(i)),
     $                 dble(spi_y(i)),euler,ra,dec)
                  if(coordinates.eq.1) then
                     call radec2lb(ra,dec,l,b)
                     if(tangential.ne.1) then
                        call aitoff(l,b,x,y)
                     else
                        call radec2detYZ(l,b,dummyeuler,x,y)            
                     endif
                  else
                     if(tangential.ne.1) then
                        call aitoff(ra,dec,x,y)
                     else
                        call radec2detYZ(ra,dec,dummyeuler,x,y)            
                     endif
                  endif
                  if(i.eq.1) then
                     call pgmove(real(x),real(y))
                  else
                     if(i.eq.6) then
                        call pgsls(2)
                     else
                        call pgsls(1)
                     endif
                     if(abs(oldx-x).lt.drawline) then
                        call pgdraw(real(x),real(y))
                     else
                        call pgmove(real(x),real(y))
                     endif
                  endif
                  oldx=x
               end do
c     spi zero response
               spi_side=17.5
               do i = 1, 7
                  theta = 1.0472*(i-1)
                  spi_x(i)=spi_side*cos(theta)
                  spi_y(i)=spi_side*sin(theta)
                  call detYZ2radec(dble(spi_x(i)),
     $                 dble(spi_y(i)),euler,ra,dec)
                  if(coordinates.eq.1) then
                     call radec2lb(ra,dec,l,b)
                     if(tangential.ne.1) then
                        call aitoff(l,b,x,y)
                     else
                        call radec2detYZ(l,b,dummyeuler,x,y)            
                     endif
                  else
                     if(tangential.ne.1) then
                        call aitoff(ra,dec,x,y)
                     else
                        call radec2detYZ(ra,dec,dummyeuler,x,y)            
                     endif
                  endif
                  if(i.eq.1) then
                     call pgmove(real(x),real(y))
                  else
                     if(i.eq.6) then
                        call pgsls(2)
                     else
                        call pgsls(4)
                     endif
                     if(abs(oldx-x).lt.drawline) then
                        call pgdraw(real(x),real(y))
                     else
                        call pgmove(real(x),real(y))
                     endif
                  endif
                  oldx=x
               end do
               call pgsls(1)
            endif
         endif
         call pgslw(1)
      else
C     Mission is not INTEGRAL nor Swift
         write(string,'(a,a,a)') 'Warning: mission ', mission,
     $        'not defined.'
         call fcecho(string)
         call fcecho(
     $'Specify either INTEGRAL or Swift to draw instrument FOV.'
     $        )
      endif
         
      end

      subroutine radec2detYZ(ra,dec,euler,detY,detZ)
C     convert (RA,DEC) to (detY, detZ), where detY and detZ
C     are looking-up coordinates on the focal plane.
C     detY direction agrees with  satY, while detZ = -satZ.
      implicit none
      double precision ra, dec, euler(3), detY, detZ
      integer i

      double precision  sky(3),deg2rad,
     $     sat(3),tmp1(3),tmp2(3),ray(3),ray_alpha,ray_delta, tmp3(3)
      data deg2rad/1.745329252d-2/

      detY = -999.0
      detZ = -999.0

C     calculate the pointing vector of the source in the celestial coordinates 
      call radec2point(ra,dec,sky)
         
C     calculate the pointing vector in the satellite coordinates
      call rot_x(sky, euler(1),tmp1)
      call rot_z(tmp1,euler(2),tmp2)
      call rot_x(tmp2,euler(3),sat)

C     Now, sat is the pointing vector of the source in the
C     satellite coorinates.  
      if(sat(1).lt.0) then
c     source is from below the satellite
         go to 999
      endif
         
C     ray is the direction vector of the ray *from* the point source
C     (we invert the vector)
      ray(1)=-sat(1)
      ray(2)=-sat(2)
      ray(3)=-sat(3)

      ray_delta = atan(ray(1)/sqrt(ray(2)**2+ray(3)**2))

      if(ray(2).gt.0.0) then
         ray_alpha = atan(ray(3)/ray(2))
      else
         ray_alpha = atan(ray(3)/ray(2))+180.0*deg2rad
      endif

c      This is not tangential projection.
c      detY=cos(ray_delta)*cos(ray_alpha)/deg2rad
c      detZ=cos(ray_delta)*sin(ray_alpha)/deg2rad

c      TAN projection.  2002-10-01
      detY=-cos(ray_alpha)/deg2rad/tan(ray_delta)
      detZ=-sin(ray_alpha)/deg2rad/tan(ray_delta)

C     Flip the Z-axis (look down --> look up).
C     Note that the look-down image on the detector is a mirror image
C     of the sky, thus this flip is required.
      detZ=-detZ

 999  continue
      end

      subroutine draw_coordinate_grids(euler,xmin,xmax,ymin,ymax)
      implicit none
      double precision euler(3)      
      real xmin, xmax, ymin, ymax

      integer ngrid
      parameter (ngrid=61)

      double precision ra, dec

      integer j
      double precision ra_border(9),dec_border(9)
      double precision minra,maxra,maxdec,mindec 

      call get_ra_dec_border(euler, ra_border,dec_border,
     $     xmin,xmax,ymin,ymax)

c     determine the range of DEC and R.A. for the grids
      call getminmaxradec(ra_border,dec_border,minra,maxra,
     $     mindec,maxdec)
      do j=1,9
         dec = mindec+(maxdec-mindec)/8*(j-1)
C     draw the const. declination line
         call draw_grid('dec',euler,dec,ngrid,minra,maxra,
     $        xmin,xmax,ymin,ymax)
      end do

      do j=1,9
         ra  = minra +(maxra-minra)/8*(j-1)
C     draw the const. right ascension line
         if(.not.(maxra.eq.360.and.minra.eq.0.0.and.ra.eq.maxra))
     $        call draw_grid('ra',euler, ra,ngrid,
     $        mindec,maxdec,xmin,xmax,ymin,ymax)
      end do
      end

      subroutine get_ra_dec_border(euler, 
     $     ra_border,dec_border,xmin, xmax, ymin, ymax)
      implicit none
      double precision euler(3),ra_border(9),dec_border(9)
      real xmin, xmax, ymin, ymax

      double precision dety, detz
      integer i,j

      do i=1,3
         dety=DBLE(xmin+(xmax-xmin)/2.0*(i-1))
         do j=1, 3
            detz=DBLE(ymin+(ymax-ymin)/2.0*(j-1))
            call detYZ2radec(dety,detz,euler,
     $           ra_border(3*(i-1)+j), dec_border(3*(i-1)+j))
         end do
      end do
      end 

      subroutine getminmaxradec(ra_border,dec_border,minra,maxra,
     $     mindec,maxdec)
      implicit none
      double precision ra_border(9),dec_border(9),minra,maxra,
     $     mindec,maxdec
      
      double precision sorted(9),difference(9),tmp,maxdiff
      integer i,j,k
      k=0
      do i=1, 9
         if(ra_border(i).ge.0.0.and.ra_border(i).le.360.0) then
            k = k + 1
            sorted(k)=ra_border(i)
         endif
      end do

      do i=1,k
         do j=i+1,k
            if(sorted(i).gt.sorted(j)) then
               tmp = sorted(j)
               sorted(j)=sorted(i)
               sorted(i)=tmp
            endif
         end do
      end do
      do i=1,k-1
         difference(i)=sorted(i+1)-sorted(i)
      end do
      difference(k) = sorted(1)+360.0-sorted(k)

      maxdiff=difference(1)
      do i=2,k
         maxdiff = max(maxdiff,difference(i))
      end do

      do i=1, k
         if(maxdiff.eq.difference(i)) then
            if(i.ne.k) then
               minra = sorted(i+1)
               maxra = sorted(i)
            else
               minra = sorted(1)
               maxra = sorted(k)
            endif
         endif
      end do
      
      if(minra.gt.maxra) minra = minra - 360.0

      maxdec=-999.
      mindec= 999.
      do i = 1,9
         if(dec_border(i).ge.-90.0.and.
     $      dec_border(i).le. 90.0) then
            maxdec=max(maxdec,dec_border(i))
            mindec=min(mindec,dec_border(i))
         endif
      end do
      if(maxdiff.lt.180.0) then
c     North pole or south pole is in the figure
         if(mindec.gt.0.0) maxdec=90.0
         if(maxdec.lt.0.0) mindec=-90.0
         minra = 0.0
         maxra = 360.0
      endif
      end

      subroutine  draw_grid(ra_or_dec,euler,
     $     const_ang,ngrid,start,end, xmin, xmax, ymin, ymax)
      implicit none
      character*(*) ra_or_dec
      double precision start, end, const_ang,euler(3),focal_length
      integer ngrid,i
      real xmin, xmax, ymin, ymax

      double precision ra, dec,detY,detZ
      real gridX(61),gridY(61)
      character text*20
      real angle
      logical pgmoved
      integer igrid
      
      call pgsci(1)
      call pgsls(4)

      if(index(ra_or_dec,'ra').gt.0) then
c     light green color
         call pgscr(2,0.5,1.0,0.5)
         call pgsci(2)
         ra = const_ang
            if(const_ang.lt.0.0) then
               write(text,'(f7.3)') const_ang+360.            
            else
               write(text,'(f7.3)') const_ang            
            endif
      else
         dec = const_ang
c     light yellow color
         call pgscr(4,0.75,0.75,0.0)
         call pgsci(4)
         write(text,'(f7.3)') const_ang      
      endif
      do i = 1, ngrid
         if(index(ra_or_dec,'ra').gt.0) then
            dec = start+(end-start)/(ngrid-1)*i
         else
            ra = start+(end-start)/(ngrid-1)*i
         endif
         call radec2detYZ(ra,dec,euler,detY,detZ)
         gridX(i)=detY
         gridY(i)=detZ
      end do

      pgmoved=.false.
      do i = 1, ngrid
         if(gridX(i).ge.xmin.and.gridX(i).le.xmax.and.
     $      gridY(i).ge.ymin.and.gridY(i).le.ymax) then
            if(.not.pgmoved) then
               call pgmove(gridX(i), gridY(i))
               pgmoved=.true.
            else
               call pgdraw(gridX(i), gridY(i))
            endif
         else
            pgmoved=.false.
         endif
      end do

      igrid = -999
      if(gridx(ngrid/2).gt.xmin.and.    
     $     gridx(ngrid/2).lt.xmax.and.
     $     gridy(ngrid/2).gt.ymin.and.
     $     gridy(ngrid/2).lt.ymax) then
         igrid=ngrid/2
      else if(gridx(1).gt.xmin.and.    
     $     gridx(1).lt.xmax.and.
     $     gridy(1).gt.ymin.and.
     $     gridy(1).lt.ymax) then
         igrid=1
      else if(gridx(ngrid).gt.xmin.and.    
     $     gridx(ngrid).lt.xmax.and.
     $     gridy(ngrid).gt.ymin.and.
     $     gridy(ngrid).lt.ymax) then
         igrid=ngrid
      else if(gridx(ngrid/4).gt.xmin.and.    
     $     gridx(ngrid/4).lt.xmax.and.
     $     gridy(ngrid/4).gt.ymin.and.
     $     gridy(ngrid/4).lt.ymax) then
         igrid=ngrid/4
      else if(gridx(ngrid*3/4).gt.xmin.and.    
     $     gridx(ngrid*3/4).lt.xmax.and.
     $     gridy(ngrid*3/4).gt.ymin.and.
     $     gridy(ngrid*3/4).lt.ymax) then
         igrid=ngrid*3/4
      endif
      if(igrid.ge.1.and.igrid.le.ngrid) then
         call get_angle(gridx(igrid),
     $        gridy(igrid),
     $        gridx(igrid+1),
     $        gridy(igrid+1), angle)
C     write the values of the RA and DEC (do not write at the poles)
         if(dec.ne.90.0.and.dec.ne.-90.0)
     $        call pgptxt(gridx(igrid),
     $        gridy(igrid),angle,0.0,text)
      endif
      call pgsci(1)      
      end

      subroutine get_angle(x1,y1,x2,y2,angle)
      implicit none
      real x1,y1,x2,y2,angle
      if(x1 .eq. x2) then 
         angle = 90.0
      else
         angle = atan((y2-y1)/(x2-x1))*57.29
      endif
      end

      subroutine display_image(imagelist,
     $     imwidth,coordinates, euler,transfer,scale,outimagesize,
     $     pixeldist)

      implicit none
      character imagelist*(*)
      integer bin, coordinates,transfer
C     coordinates=1 means galactic, 0 is equatorial
      double precision euler(3)
      real imwidth
      integer outimagesize
      real outimage(outimagesize,outimagesize)
      integer inputimagemax
      parameter (inputimagemax=1536)
      real  inputimage(inputimagemax,inputimagemax)
      real  tmpimage(inputimagemax*inputimagemax)
      integer inputimagesizeX, inputimagesizeY
      real  tr(6)
      integer  i, j, k, icilo, icihi,unit,status, blocksize,
     $     hdutype
      logical anyf
      real maxvalue,minvalue, maxdisplay, mindisplay,scale
c      double precision xrval, yrval, xrpix, yrpix, xinc,yinc,
c     $     rot
      double precision xrval, yrval, xrpix, yrpix,
     $     xinc,yinc, rot
      character coordtype*100
      double precision xpix,ypix,xpos,ypos
      integer  seed, m, n
      double precision x, y
      double precision oldeuler(3)
      real oldimwidth
      integer oldcoordinates
      double precision l, b
      integer tmp, ii
      character(999) inputimagefile(999)
      integer ninputimage
      real virtualphoton
      integer oldoutimagesize
      real savedimage(1024,1024)
      logical newimage
      character comment*80, extname*8
      real rand
      logical pixeldist
      integer object1, type, numvalues
      integer numaxes
      integer axes(10), startvalues(10)
      integer ll, mm
      integer rilStat
      character string*1200

      
      integer warning_0, pcount,gcount,naxis, bitpix

      logical simple, extend

      data status/0/

      save savedimage, oldeuler, oldimwidth, oldcoordinates,
     $     oldoutimagesize,maxvalue,minvalue

      seed = 0

      if(euler(1).ne.oldeuler(1).or.euler(2).ne.oldeuler(2).or.
     $     euler(3).ne.oldeuler(3).or.imwidth.ne.oldimwidth.or.
     $     coordinates.ne.oldcoordinates.or.
     $     outimagesize.ne.oldoutimagesize) then
         newimage=.true.
c     New image to display is created.
         do i =1, outimagesize
            do j=1,outimagesize
               outimage(i,j)=0.0
            end do
         end do
         maxvalue=-999.
         minvalue=999.
         
C     imagelist is FITS file?
         call ftgiou(object1,status)         
         call FTiOPN(object1,imagelist,0,status)
         if(status.eq.0) then
            ninputimage=1
            inputimagefile(1)=imagelist
            call ftclos(object1,status)
            write(string,'(a)') 'FITS image is read'
            call FCECHO (string)
            write(string,'(a,a)') 'Imagefile: ', inputimagefile(1)
            call FCECHO (string)
         else
            write(string,'(a)') 'image file ascii list is read'
            call FCECHO (string)
            status=0
            i = 0
            open(20,file=imagelist,status='old',err=50)
 1          continue
            i = i +1
            read(20,'(a)',end=20) inputimagefile(i)
            write(string,'(a,i3,a,a)')
     $           'Imagefile',i,': ',inputimagefile(i)
            call FCECHO (string)
            go to 1
 20         continue
            close(20)
            ninputimage = i-1
         endif

         do ii = 1, ninputimage
            hdutype=-99
            call ftiopn(object1,inputimagefile(ii),0,status)

            call FTGHPR(object1,2,simple,bitpix,naxis,axes,pcount,
     $           gcount,extend,               status)
             inputimagesizeX = axes(1)
             inputimagesizeY = axes(2)
             
             if(status.ne.0) then
                write(string,'(a,a)') 'image extension not found in ',
     $               inputimagefile(ii)
                call FCECHO (string)
                inputimagesizeX=-999
                inputimagesizeY=-999
                minvalue=0.0
                maxvalue=0.0
                status=0
             else

                startvalues(1) = 1
                startvalues(2) = 1

                call FTG2DE(object1, 0, 0.0,inputimagemax,
     $               axes(1),axes(2),inputimage, anyf, status)
                
                call FTGICS(object1, xrval,yrval,xrpix,yrpix,xinc,
     $               yinc,rot,coordtype,status)
             endif
             call ftclos(object1,status)
             do i = 1, inputimagesizeX
                do j = 1, inputimagesizeY
C     The following line is a trick to avoid the NaN values
                   if(inputimage(i,j).ne.inputimage(i,j))
     $                  inputimage(i,j)=0.0
                   maxvalue=max(maxvalue,inputimage(i,j))
                   minvalue=min(minvalue,inputimage(i,j))
                   if(inputimage(i,j).ne.0.0) then
                      if(abs(xinc).ge.0.5*imwidth/outimagesize.and.
     $                     abs(yinc).ge.0.5*imwidth/outimagesize.and.
     $                     pixeldist) then
C     Redisribute the pixel values when new pixel size is comparable to 
C     the original pixel size.
                         if(inputimage(i,j).gt.1.0) then
                            virtualphoton=inputimage(i,j)/
     $                           int(inputimage(i,j))
                            tmp = inputimage(i,j)
                         else
                            virtualphoton=inputimage(i,j)
                            tmp = 1
                         endif
                      else
c     Otherwise, no pixel redistribution
                         virtualphoton=inputimage(i,j)
                         tmp = 1
                      endif
                      do k=1,tmp
                         xpix=dble(i)+rand(seed)-0.50D0
                         ypix=dble(j)+rand(seed)-0.50D0
                         call FTWLDP(xpix,ypix,xrval,yrval,xrpix,yrpix,
     $                        xinc,yinc,rot,coordtype, xpos,ypos,status)
                         if(coordinates.eq.1) then
                            call radec2lb(xpos,ypos,l,b)         
                            call radec2detYZ(l,b,euler,x,y)         
                         else
                            call radec2detYZ(xpos,ypos,euler,x,y)
                         endif
                         if(x.ge.-imwidth/2.0.and.x.lt.imwidth/2.0.and.
     $                      y.ge.-imwidth/2.0.and.y.lt.imwidth/2.0) then
                          m = outimagesize/2+1+x/(imwidth/outimagesize)
                          n = outimagesize/2+1+y/(imwidth/outimagesize)
                          outimage(m,n)=outimage(m,n)+virtualphoton
                         endif
                      end do
                   endif
                end do
             end do
          end do
       else
          newimage=.false.
          do i =1,outimagesize
             do j=1,outimagesize
                outimage(i,j)=savedimage(i,j)
             end do
          end do
       endif
       
C     The center of the pixel (i,j) to given with 
C     X = tr(1)+tr(2)*i
C     Y = tr(4)+tr(6)*j
      tr(1)=-imwidth/2.0-imwidth/outimagesize/2.0
      tr(2)=imwidth/outimagesize
      tr(3)=0.0
      tr(4)=-imwidth/2.0-imwidth/outimagesize/2.0
      tr(5)=0.0
      tr(6)=imwidth/outimagesize

      call pgsitf(transfer)
      if(transfer.eq.0) then 
         write(*,*) 'Linear Image scale'
      elseif(transfer.eq.1) then
         write(*,*) 'Logarithmic Image scale'
      elseif(transfer.eq.2) then
         write(*,*) 'Square-root Image scale'
      endif

      maxdisplay = min(maxvalue,minvalue+(maxvalue-minvalue)/scale)
      mindisplay = max(minvalue,maxvalue-(maxvalue-minvalue)*scale)

      call pggray(outimage,outimagesize,outimagesize,1,outimagesize,
     $     1,outimagesize,maxdisplay,
     $     mindisplay,tr)
      oldeuler(1)=euler(1)
      oldeuler(2)=euler(2)
      oldeuler(3)=euler(3)
      oldimwidth =imwidth
      oldcoordinates=coordinates
      oldoutimagesize=outimagesize
      if(newimage) then
         do i = 1, outimagesize
            do j = 1, outimagesize
               savedimage(i,j)=outimage(i,j)
            end do
         end do
      endif

      return
 50   continue
      call FCECHO ('image file cannot be opened.')
      end

      subroutine BAT_FOV(BAT_5percent, BAT_50percent, BAT_95percent)
      implicit none
      double precision BAT_5percent(3,15),BAT_50percent(3,19),
     $     BAT_95percent(3,11)

      integer i,j

C     Definition of BAT FOV (contour of transmission)
C     From an e-mail from Craig Markwardt <craigm@milkyway.gsfc.nasa.gov>
C     on Fri, 20 Aug 2004 15:20:04 -0400.

c     BAT 5% partial coding contour
      double precision  tmp5(45)

c     BAT 50% partial coding contour
      double precision  tmp50(57)

c     BAT 95% partial coding contour
      double precision  tmp95(33)

      data tmp5/
     $              0.74329416,-8.2522249D-18,0.66896472,
     $              0.59845916, 0.59845916   ,0.53262864,
     $              0.50517294, 0.77291458   ,0.38393143,
     $              0.49758635, 0.83096918   ,0.24879317,
     $              0.51374006, 0.85794588   ,-3.8214523D-17,
     $              0.55814558, 0.78140380   , -0.27907279,
     $              0.64623814, 0.54284002   , -0.53637764,
     $              0.74329416,-1.5679223D-16, -0.66896472,
     $              0.64623814,-0.54284002   , -0.53637764,
     $              0.55814558, -0.78140380  , -0.27907279,
     $              0.51374006, -0.85794588  , 1.5228773e-16,
     $              0.49758635, -0.83096918  ,    0.24879317,
     $              0.50517294, -0.77291458  ,    0.38393143,
     $              0.59845916, -0.59845916  ,    0.53262864,
     $              0.74329416,-8.2522249D-18,    0.66896472/

      data tmp50/
     $                0.86126927,  -3.9204242D-17,      0.50814884,
     $                0.76178455,      0.48754210,      0.42659935,
     $                0.69214512,      0.65753786,      0.29762241,
     $                0.65198137,      0.74325875,      0.14995572,
     $                0.68223236,      0.72998866,    -0.040933940,
     $                0.75962756,      0.60010579,     -0.25067711,
     $                0.75962756,      0.60010579,     -0.25067711,
     $                0.84730552,      0.29655693,     -0.44059885,
     $                0.85935900,      0.21483975,     -0.46405388,
     $                0.87621591,  -1.5078324D-16,     -0.48191876,
     $                0.85935900,     -0.21483975,     -0.46405388,
     $                0.84730552,     -0.29655693,     -0.44059885,
     $                0.75962756,     -0.60010579,     -0.25067711,
     $                0.75962756,     -0.60010579,     -0.25067711,
     $                0.68223236,     -0.72998866,    -0.040933940,
     $                0.65198137,     -0.74325875,      0.14995572,
     $                0.69214512,     -0.65753786,      0.29762241,
     $                0.76178455,     -0.48754210,      0.42659935,
     $                0.86126927,  -3.9204242D-17,      0.50814884/

      data tmp95/
     $               0.97014250,  -8.0780591D-17,      0.24253563,
     $               0.87783491,      0.43013911,      0.21068037,
     $               0.85473208,      0.49574459,      0.15385178,
     $               0.90369235,      0.42473540,    -0.054221540,
     $               0.95936779,      0.23024827,     -0.16309253,
     $               0.97664447,  -1.3228377D-16,     -0.21486178,
     $               0.95936779,     -0.23024827,     -0.16309253,
     $               0.90369235,     -0.42473540,    -0.054221540,
     $               0.85473208,     -0.49574459,      0.15385178,
     $               0.87783491,     -0.43013911,      0.21068037,
     $               0.97014250,  -8.0780591D-17,      0.24253563/

      do i = 1,15
         do j = 1, 3
            BAT_5percent(j,i) = tmp5(3*(i-1)+j)
         end do
      end do

      do i = 1,19
         do j = 1, 3
            BAT_50percent(j,i) = tmp50(3*(i-1)+j)
         end do
      end do

      do i = 1,11
         do j = 1, 3
            BAT_95percent(j,i) = tmp95(3*(i-1)+j)
         end do
      end do
      end

      subroutine Swift2Euler(ra,dec,roll,ea)
      implicit none
      double precision ra, dec, roll, ea(3)      

C     This subruotine converts the Swift definition of the Euler
C     angles to our definition.

C     In the case of Swift, the Euler angle is defined as follows:
C     (E-mail from Craig Markwardt <craigm@milkyway.gsfc.nasa.gov>,
C     dated on  20 Aug 2004):
C     > Roll is defined by the following Euler angle sequence: Start with the
C     > Spacecraft +X axis pointed towards the J2000 vernal Equinox (RA=0, dec=0)
C     > and the +Z axis pointing towards the North pole, then yawing
C     > counterclockwise (as seen from a point on the +Z axis) by RA, then pitching
C     > North (clockwise as seen from a point on the +Y axis) by DEC, then rolling
C     > by ROLL clockwise as seen from a point on the +X axis.
C     Namely, the first two Swift euler angles give the pointing direction
C     (X-axis), and the third euler angle is the roll, whose difinition
C     is opposte to ours.

C     Our definition is X-Z-X rotation, starting with the X-axis pointing 
C     North, Y-axis pointing Equinox, with the normal direction of the 
C     rotation (seeing from the Origin, always clockwize). 

      ea(1)=ra
      ea(2)=90.0-dec
      ea(3)=90.0-roll
      end

