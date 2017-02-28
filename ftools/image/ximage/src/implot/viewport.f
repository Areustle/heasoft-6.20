      SUBROUTINE VIEWPORT(Cmdid,Maxvps,Vpset,Vpfile,Vpnum,
     &                    Vpconfig,Vpconnum,Vpframe,Status)
      IMPLICIT NONE
c
c  Setup viewport. There are a sets of viewport preset. These are identified 
c  by the following qualifiers : TOP', 'BOTTOM', 'CENTER', 'LEFT', 'RIGHT'
c  It is possible to sets a user defined viewport using the qualifiers
c  'V1' , 'V2', 'V3', 'V4'. 
c  To have multiple user defined viewports uses an ascii file as input
c  containig as many lines as many viewports are required. Each lne 
c  contains 4 numbers ranging between 0-1 with the following meaning 
c  minx, maxx, miny, maxy. The qualifier NUMBER addresses  viewport number  
c  in the multiple configuration
c 
c  I  cmdid    (i) Command id
c  I  maxvps   (i) Maximum number of viewports
c I/O vpset    (r) Viewport specification
c I/O vpfile   (c) Viewport file 
c I/O vpnum    (i) Current viewport number
c  O  vpconfig (r) Viewport configuration array
c  O  vpconnum (i) Number of viewports in configuration
c  O  vpframe  (l) Whether to plot frame with vp configuration
c  I  status   (i) Error flag (0=OK)
c
      INTEGER Cmdid, Status
      INTEGER Maxvps, Vpnum, Vpconnum
      REAL*4  Vpset(4), Vpconfig(Maxvps,4)
      CHARACTER*(*) Vpfile
      LOGICAL Vpframe
c
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/sitedef.inc'
c
c Local variables
      INTEGER i, n, argc, number, nrows
      REAL v1, v2, v3, v4
      REAL x1, x2, y1, y2, vptmp(4), pnts(4)
      INTEGER ix, iy, iframe, curvpnum
      LOGICAL reset, left, right, top, bottom, center, devsz, switch
      LOGICAL isdisplay, ismouse, cursor, info
      CHARACTER*(MAX_FILELEN) filename
      REAL*4 wx1, wx2, wy1, wy2, dr
      REAL*4 vx1, vx2, vy1, vy2
      character(100) cmd
      logical readonly, global
c
c initialize variables
      filename = ' '
      reset = .FALSE.
      left = .FALSE.
      right = .FALSE.
      top = .FALSE.
      bottom = .FALSE.
      center = .FALSE.
      devsz = .FALSE.
      info = .FALSE.
      cursor = .FALSE.
      switch = .FALSE.
      number = 0
      v1 = Vpset(1)
      v2 = Vpset(2)
      v3 = Vpset(3)
      v4 = Vpset(4)

      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARR(cmdid,'V1',v1,status)
      CALL GPARR(cmdid,'V2',v2,status)
      CALL GPARR(cmdid,'V3',v3,status)
      CALL GPARR(cmdid,'V4',v4,status)
      CALL GPARL(cmdid,'RESET',reset,status)
      CALL GPARL(cmdid,'LEFT',left,status)
      CALL GPARL(cmdid,'RIGHT',right,status)
      CALL GPARL(cmdid,'TOP',top,status)
      CALL GPARL(cmdid,'BOTTOM',bottom,status)
      CALL GPARL(cmdid,'CENTER',center,status)
      CALL GPARL(cmdid,'DEVSZ',devsz,status)
      CALL GPARL(cmdid,'INFO',info,status)
      CALL GPARS(cmdid,'FILE',filename,status)
      CALL GPARI(cmdid,'NUMBER',number,status)
      CALL GPARL(cmdid,'CURSOR',cursor,status)
      CALL GPARL(cmdid,'SWITCH',switch,status)
      if ( status.ne.0 ) return

      call qustrip(filename)

      if ( switch ) then
         if ( number.le.0 ) then
            call xwrite(' No viewport number given to switch to', 10)
            return
         endif
         write(cmd,'(a,1x,i3)') 'pgtk::fswitchvp', number
         call tclrun(cmd, status)
         if ( status.ne.0 ) return
         goto 500
      endif

      if ( cursor ) then
         if ( .not.isdisplay() ) then
            call xwrite(' No image displayed', 10)
            return
         endif
         if ( .not.ismouse() ) then
            call xwrite(' Non-interactive device', 10)
            return
         endif
         call xwrite(' Select viewport box', 10)
         call tclreslr('select box', pnts, n, 4, status)
         if ( status.ne.0 ) return
         x1 = pnts(1)
         y1 = pnts(2)
         x2 = pnts(3)
         y2 = pnts(4)
         call pgqwin(wx1,wx2,wy1,wy2)
         call pgqvp(0,vx1,vx2,vy1,vy2)
         v1 = (x1-wx1)*(vx2-vx1)/(wx2-wx1)+vx1
         v2 = (x2-wx1)*(vx2-vx1)/(wx2-wx1)+vx1
         v3 = (y1-wy1)*(vy2-vy1)/(wy2-wy1)+vy1
         v4 = (y2-wy1)*(vy2-vy1)/(wy2-wy1)+vy1
         if ( v1.gt.v2 ) then
            dr = v1
            v1 = v2
            v2 = dr
         endif
         if ( v3.gt.v4 ) then
            dr = v3
            v3 = v4
            v4 = dr
         endif
         write(ZWRite,'(a,4(1x,f6.4))') ' Viewport: ', v1, v2, v3, v4
         call xwrite(ZWRite, 10)
      endif
c
      if ( info ) then
         call tclrun("pgtk::infovp", status)
         goto 500
      endif

      if ( devsz ) then
         if ( .not.isdisplay() ) then
            call xwrite(' No image displayed', 10)
            return
         endif
         call PGQVSZ(3, x1, x2, y1, y2)
         ix = x2-x1
         iy = y2-y1
         write(ZWRite, *) ' Device dimensions (device pixels): ', 
     &                      ix, ' x ', iy
         call rmvxbk(ZWRite(2:))
         call xwrite(ZWRite, 5)
         call pgqvp(0,v1,v2,v3,v4)
         write(ZWRite,'(a,4(1x,f6.4))') ' Viewport: ', v1, v2, v3, v4
         call xwrite(ZWRite, 10)
         goto 500
      endif

      if ( reset ) then
         call set_vpvals(Vpset,-1)
         Vpfile = ' '
         Vpnum = 0
      else if ( left ) then
         call set_vpvals(Vpset,1)
         Vpfile = ' '
         Vpnum = 0
      else if ( right ) then
         call set_vpvals(Vpset,2)
         Vpfile = ' '
         Vpnum = 0
      else if ( top ) then
         call set_vpvals(Vpset,3)
         Vpfile = ' '
         Vpnum = 0
      else if ( bottom ) then
         call set_vpvals(Vpset,4)
         Vpfile = ' '
         Vpnum = 0
      else if ( center ) then
         call set_vpvals(Vpset,0)
         Vpfile = ' '
         Vpnum = 0
      else if ( filename.eq.' ' .and. number.ne.0 ) then
         if ( Vpfile.ne.' ' ) then
            if ( number.le.Vpconnum ) then
               Vpnum = number
            elseif ( Vpconnum.eq.0 ) then
               call XWRITE(' NUMBER ignored, no set configuration', 10)
            else
               call XWRITE(
     &              ' NUMBER ignored, exceeds number of viewports', 10)
            endif
         else
            if ( number.gt.1 ) then
               call xwrite(' No configuration, NUMBER ignored', 10)
            endif
c           Ignore NUMBER <= 0
         endif
      else if ( filename.ne.' ' ) then
         Vpconnum = 0
         call lkupfile(filename, 'vpc', 'viewport configuration',
     &                 status)
         if ( status.eq.0 ) then
            call txrdikey(filename, 'frame', 0, iframe, status)
            if ( status.eq.0 .and. iframe.eq.1 ) then
               Vpframe = .TRUE.
            else
               status = 0
               Vpframe = .FALSE.
            endif
            do i = 1, 4
               call txrdcol(filename, i, Maxvps, Vpconfig(1,i), nrows,
     &                     status)
            enddo
            if ( status.ne.0 ) then
               call xwrite (' Failed to read viewport configuration',10)
               return
            endif
            Vpconnum = nrows
         endif
         status = 0

         if ( Vpconnum.gt.0 ) then
            Vpfile = filename
            Vpnum = 1
         endif
         if ( number.gt.0 .and. number.le.Vpconnum ) Vpnum = number

      else if ( Vpset(1).ne.v1 .or. Vpset(2).ne.v2 .or. 
     &          Vpset(3).ne.v3 .or. Vpset(4).ne.v4 ) then
         if ( v1.lt.0.0 .or. v1.gt.1.0 .or. 
     &        v2.lt.0.0 .or. v2.gt.1.0 .or.
     &        v3.lt.0.0 .or. v3.gt.1.0 .or. 
     &        v4.lt.0.0 .or. v4.gt.1.0 ) then
            call XWRITE(' Viewport values must be between 0.0 and 1.0',
     &                  10)
         else
            Vpset(1) = v1
            Vpset(2) = v2
            Vpset(3) = v3
            Vpset(4) = v4
            Vpfile = ' '
            Vpnum = 0
         endif
      endif
c
c  Set viewport variable in Tcl
c
  500 continue
      readonly = .TRUE.
      global = .FALSE.
      if ( Vpfile.ne.' ' ) then
         call tclvars('viewport(file)',Vpfile,readonly,global,status)
         call tclvari('viewport(next)',Vpnum,readonly,global,status)
         curvpnum = 0
         call tclresi('pgtk::curvp', curvpnum, status)
         status = 0
         call tclvari('viewport(cur)',curvpnum,readonly,global,
     &                status)
         if ( curvpnum.gt.0 ) then
            vptmp(1) = Vpconfig(curvpnum,1)
            vptmp(2) = Vpconfig(curvpnum,2)
            vptmp(3) = Vpconfig(curvpnum,3)
            vptmp(4) = Vpconfig(curvpnum,4)
            call tclvarlr('viewport(list)',vptmp,4,readonly,global,
     &                    status)
         endif
         call tclvari('viewport(max)',Vpconnum,readonly,global,
     &                status)
      else
         call tclvars('viewport(file)',' ',readonly,global,status)
         call tclvari('viewport(next)',1,readonly,global,status)
         call tclvari('viewport(cur)',1,readonly,global,status)
         call tclvari('viewport(max)',1,readonly,global,status)
         call tclvarlr('viewport(list)',Vpset,4,readonly,global,status)
      endif
 
      status = 0
      RETURN

      END
