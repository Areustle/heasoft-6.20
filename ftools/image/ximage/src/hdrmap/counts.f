      subroutine counts(Cmdid, Map, Szx, Szy, Mapid, Status)
      implicit none
c
c  Sum counts in an area specified by a region file, cursor or 
c    x and y range
c
c  I  Cmdid     (i)  Command id
c  I  Map       (r)  Image map
c  I  Szx/y     (i)  Size of map
c  I  Mapid     (s)  Map id string
c  O  Status    (i)  Error flag (0 = OK)
c
      integer*4 Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      INTEGER n, argc, sigdig, slen, clen, frstat
      character(1) maptype
      character(20) sumcntstr, areaimgstr, areadetstr, avgimgstr,
     &             avgdetstr, unit

      LOGICAL isloaded, isdisplay, ismouse, iscpmapid

      REAL*4 xbmin , xbmax , ybmin , ybmax, pnts(4)
      REAL*4 sumcnt, area, rsum, rarea

      LOGICAL cursor, noplot, plot

      character(20) xmnstr, xmxstr, ymnstr, ymxstr
      character*(MAX_FILELEN) regionfile, regionsave
      character*(MAX_IDSTR) wcsid
      logical xyspec
      integer color, lwidth, lstyle, excolor, exlwidth, exlstyle

      integer*4 di
      real*8 zmx, zmy, xcen, ycen, dd
      real*8 xbcen, ybcen, xbwid, ybwid

      integer numreg, ireg, lenact
      logical readonly, global
c
c defaults and inits.
c
      cursor = .FALSE.
      noplot = .FALSE.
      xmnstr = ' '
      ymnstr = ' '
      xmxstr = ' '
      ymxstr = ' '
      regionfile = ' '
      regionsave = ' '
c
c  Include = green, exclude = red
c
      color = 3
      lwidth = -1
      lstyle = -1
      excolor = 2
      exlwidth = -1
      exlstyle = -1
c
c  Retrieve argument as region file
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'XMIN',xmnstr,Status)
      CALL GPARS(Cmdid,'XMAX',xmxstr,Status)
      CALL GPARS(Cmdid,'YMIN',ymnstr,Status)
      CALL GPARS(Cmdid,'YMAX',ymxstr,Status)
      CALL GPARS(Cmdid,'REGIONFILE',regionfile,Status)
      CALL GPARL(Cmdid,'NOPLOT',noplot,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARI(Cmdid,'LSTYLE',lstyle,Status)
      if ( status.ne.0 ) return

      write(ZWRite,'(2a)') ' Using ', Mapid(:lenact(Mapid))
      call xwrite(ZWRite, 10)

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 10)
         Status = -1
         return
      endif

      call qustrip(regionfile)
c
c  Check qualifiers
c
      plot = .not.noplot .and. isdisplay() .and. iscpmapid('DIS',Mapid)

      call get_refram(mapid,di,di,zmx,zmy,xcen,ycen,status)

      xyspec = .FALSE.
      if ( xmnstr.ne.' ' .or. xmxstr.ne.' ' .or.
     &     ymnstr.ne.' ' .or. ymxstr.ne.' ' ) then
         xyspec = .TRUE.
         if ( xmnstr.ne.' ' .and. xmxstr.ne.' ' .and.
     &        ymnstr.ne.' ' .and. ymxstr.ne.' ' ) then
            call strnum(xmnstr, 4, dd, status)
            xbmin = dd
            call strnum(xmxstr, 4, dd, status)
            xbmax = dd
            call strnum(ymnstr, 4, dd, status)
            ybmin = dd
            call strnum(ymxstr, 4, dd, status)
            ybmax = dd
         else
            call xwrite(' X/YMIN, X/YMAX must all be specified', 10)
            status = -1
            return
         endif
      ENDIF
 
      IF ( .not.xyspec .and. regionfile.eq.' ' ) THEN
         if ( ismouse() ) then
            cursor = .TRUE.
            if ( .not.iscpmapid('DIS',Mapid) ) then
               call xwrite(' Current map is not display map', 10)
               call maprcmd(1)
               status = -1
               return
            endif
         else
            call XWRITE(' Interactive device not in use for cursor', 10)
            call XWRITE(' No area specified for summation ', 10)
            Status = 1
            return
         endif
      ENDIF
c
c  Select box with cursor to count
c
      IF ( cursor ) THEN
         call xwrite(' Select rectangular area for summation', 10)
         call tclreslr('select box', pnts, n, 4, status)
         if ( status.ne.0 ) return
         xbmin = pnts(1)
         ybmin = pnts(2)
         xbmax = pnts(3)
         ybmax = pnts(4)
      ENDIF
c
c  Count region
c
      if ( regionfile.ne.' ' ) then
c
c    Set WCS data
c
         call gheads(mapid, 'WCSID', wcsid, 0, status)
         call setregwcs(wcsid, status)
c
c    Read in regions
c
         call xinitreg(Regionfile, numreg, status)
         if ( status.ne.0 ) then
            Regionfile = ' '
            Status = -1
            return
         endif
         if ( numreg.gt.1 ) then
            call xwrite(' All regions in region list will be summed',
     &                  10)
         endif

      else
c
c  Generate internal box region
c
         xbwid = ABS(xbmax - xbmin)
         ybwid = ABS(ybmax - ybmin)
         xbcen = (xbmin + xbmax)/2.d0
         ybcen = (ybmin + ybmax)/2.d0
         call xboxreg(0, xbcen, ybcen, xbwid, ybwid, 0.d0, numreg, 
     &                status)

      endif

      sumcnt = 0.
      area = 0.
      do ireg = 1, numreg
c
c    Plot region
c
         if ( plot ) then
            call plotreg(ireg, color, lwidth, lstyle, excolor, exlwidth,
     &                   exlstyle, status)
            if ( status.ne.0 ) goto 500
         endif
c
c    Count region
c
         call cntreg(ireg, Map, Szx, Szy, zmx, zmy, xcen, ycen,
     &               rsum, rarea, status)
         if ( status.ne.0 ) goto 500
         sumcnt = sumcnt + rsum
         area = area + rarea

      enddo

  500 continue
      call xfreereg(frstat)
      if ( status.ne.0 ) return
c
c  Write results to screen
c
      readonly = .FALSE.
      global = .FALSE.
      maptype = ' '
      sigdig = -1
      call tclrun('catch {unset counts}', status)
      call gheads(mapid, 'MAPTYPE', maptype, 0, status)
      call gheads(mapid, 'BUNIT', unit, 0, status)
      call tclvars('counts(unit)', unit, readonly, global, status)
      if ( maptype.eq.'I' ) then
         call xistr(NINT(sumcnt),sumcntstr,slen)
         call tclvari('counts(total)', NINT(sumcnt), readonly, global, 
     &                status)
      else
         dd = sumcnt
         call xdstr(dd,sigdig,sumcntstr,slen)
         call tclvard('counts(total)', dd, readonly, global, status)
      endif
      dd = area
      call xdstr(dd,sigdig,areaimgstr,slen)
      call tclvard('counts(areaimg)', dd, readonly, global, status)
      dd = area*(zmx*zmy)
      call xdstr(dd,sigdig,areadetstr,slen)
      call tclvard('counts(areadet)', dd, readonly, global, status)
      if ( area.gt.0.d0 ) then
         dd = sumcnt/area
         call xdstr(dd,sigdig,avgimgstr,slen)
         call tclvard('counts(avgimg)', dd, readonly, global, status)
      else
         avgimgstr = '---'
      endif
      if ( area.gt.0. .and. zmx.ne.0.d0 .and. zmy.ne.0.d0 ) then
         dd = sumcnt/(area*(zmx*zmy))
         call xdstr(dd,sigdig,avgdetstr,slen)
         call tclvard('counts(avgdet)', dd, readonly, global, status)
      else
         avgdetstr = '---'
      endif

      clen = 14
      call xwrite(' ', 10)
      call xwrite('Total           Area           Area           '//
     &            'Average           Average', 10)
      slen = LENACT(unit)
      if ( slen.gt.clen-1 ) then
         unit(clen:clen) = ')'
      else
         unit(slen+1:) = ')'
      endif
      write(ZWRite,'(2a,1x,a)') ' (', unit(1:clen), '(img-pix)      '//
     &                 '(det-pix)      (per img-pix)     (per det-pix)'
      call xwrite(ZWRite, 10)
      write(ZWRite,'(2a,1x,a,1x,a,1x,a,4x,a)') '# ', sumcntstr(:clen), 
     &             areaimgstr(:clen), areadetstr(:clen), 
     &             avgimgstr(:clen), avgdetstr(:clen)
      call xwrite(ZWRite, 10)
      call xwrite(' ', 10)
      status = 0

      RETURN
      END
