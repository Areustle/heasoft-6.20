      subroutine uplimit(Cmdid, Map, Szx, Szy, Mapid, Status)
      implicit none
c
c  Calculate three sigma upper limit
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
      include 'backgd.inc'
c
c  Local variables
c
      INTEGER n, argc, sigdig, slen, clen, frstat, lenact
      character(1) maptype
      character(20) cntstr, areastr, ulratestr, cntratestr
      character(20) conflevstr, sigstr, bgcntstr, ulstr

      LOGICAL isloaded, isdisplay, ismouse, iscpmapid, ISRNULL

      REAL*4 ERF, BAYLIKE, BAYPRI
      REAL*4 counts, background, sigma, conflev, bgimg, pnts(4), RNULL
      REAL*4 xbmin , xbmax , ybmin , ybmax
      REAL*4 sumcnt, area, rsum, rarea, cntsrc, cntbg, ulres
      INTEGER ibbace

      LOGICAL cursor, noplot, plot, optimize

      character(20) xmnstr, xmxstr, ymnstr, ymxstr, expostr
      character*(MAX_FILELEN) regionfile, regionsave
      character*(MAX_IDSTR) wcsid
      logical xyspec
      integer color, lwidth, lstyle, excolor, exlwidth, exlstyle

      integer*4 di
      real*8 zmx, zmy, xcen, ycen, dd
      real*8 xbcen, ybcen, xbwid, ybwid

      real*8 exposure

      integer numreg, ireg
      logical readonly, global
c
c defaults and inits.
c
      counts = RNULL()
      background = RNULL()
      sigma = RNULL()

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

      CALL GPARR(Cmdid,'COUNTS',counts,Status)
      CALL GPARR(Cmdid,'BACKGROUND_LEVEL',background,Status)
      CALL GPARR(Cmdid,'SIGMA',sigma,Status)
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
c
c  Loaded image is unnecessary if counts are entered directly
c
      zmx = 0.
      zmy = 0.
      if ( ISRNULL(counts) ) then

         write(ZWRite,'(2a)') ' Using ', Mapid(:lenact(Mapid))
         call xwrite(ZWRite, 10)

         if ( .not.isloaded(mapid) ) then
            call XWRITE(' Image not loaded', 10)
            Status = -1
            return
         endif
         call get_refram(mapid,di,di,zmx,zmy,xcen,ycen,status)

      endif

      call qustrip(regionfile)
c
c  Check qualifiers
c
      plot = .not.noplot .and. isdisplay() .and. iscpmapid('DIS',Mapid)

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
 
      IF ( ISRNULL(counts) .and. 
     &       .not.xyspec .and. regionfile.eq.' ' ) THEN
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
c  Calculate confidence level
c
      if ( ISRNULL(sigma) ) then
         sigma = 3.
         conflev = 0.99865
      else 
         if ( sigma.le.0. ) then
            call XWARN(' Sigma less than zero', 10)
            Status = 1
            return
         endif
         if ( sigma.gt.5. ) then
            call XWARN(' Sigma greater than 5', 10)
            Status = 1
            return
         endif
         conflev = 0.5*(1.0 + ERF(sigma/sqrt(2.0)))
      endif
      dd = conflev
      call xdstr(dd,-1,conflevstr,slen)
      call xwrite(('Confidence level: '//conflevstr(:slen)), 15)
      call xistr(nint(sigma),sigstr,slen)
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

      sumcnt = 0.
      area = 0.
c
c  Counts entered directly
c
      if ( .not. ISRNULL(counts) ) then

         sumcnt = counts
         if ( ISRNULL(background) ) then
            call xwarn(
     &       ' When COUNTS is specified BACKGROUND_LEVEL is required',
     &        10)
            status = -1
            return
         endif
         area = RNULL()
         numreg = 0
c
c  Count region
c
      elseif ( regionfile.ne.' ' ) then
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
c
c  Set background
c
      ibbace = 0
      if ( .not.ISRNULL(counts) ) then
         bgimg = background
      elseif ( .not.ISRNULL(background) ) then
         bgimg = background*(zmx*zmy)
         BNEw = bgimg
         call prback(mapid, status)
         NBOxes = 0
      elseif ( NBOxes.gt.0 ) then
         call xwrite(' Using existing background calculation', 10)
         call prback(mapid, status)
         bgimg = BNEw
      else
         call getbgmap(Mapid, BGMap, BGSz, status)
         if ( Status.ne.0 ) return
         optimize = .FALSE.
         SIGmult = DEFsigm
         BARylim = DEFbarl
         BXHprob = DEFhprob
         call do_back(Map, Szx, Szy, Mapid, optimize, ibbace, status)
         if ( status.eq.0 ) then
            bgimg = BNEw
         else
            call xwrite(' Background calc failed, using 0.', 10)
            bgimg = 0.
         endif
         NBOxes = 0
      endif

      sigdig = -1
      call gheadd(mapid, 'EXPOSURE', exposure, 0, status)
      call xdstr(exposure,sigdig,expostr,slen)
      write(ZWRite,'(2a)') ' Exposure: ', expostr(1:slen)
      call xwrite(ZWRite, 10)

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
      frstat = 0
      if ( numreg.gt.0 ) call xfreereg(frstat)
      if ( status.ne.0 ) return
c
c  Perform upper limit calculation
c
      cntsrc = sumcnt
      if ( ISRNULL(area) ) then
         cntbg = bgimg
      else
         cntbg = bgimg*area
      endif
c
c  Write results to screen
c
      readonly = .FALSE.
      global = .FALSE.
      maptype = ' '
      call tclrun('catch {unset uplimit}', status)
      call gheads(mapid, 'MAPTYPE', maptype, 0, status)
      dd = sumcnt
      call xdstr(dd,sigdig,cntstr,slen)
      call tclvard('uplimit(rawcnt)', dd, readonly, global, status)

      call tclvarr('uplimit(areaimg)', area, readonly, global, status)
      if ( ISRNULL(area) ) then
         dd = area
         areastr = '---'
      else
         dd = area*(zmx*zmy)
         call xdstr(dd,sigdig,areastr,slen)
      endif
      call tclvard('uplimit(areadet)', dd, readonly, global, status)
      if ( exposure.gt.0.d0 ) then
         dd = sumcnt/exposure
         call xdstr(dd,sigdig,cntratestr,slen)
         call tclvard('uplimit(rawrate)', dd, readonly, global, status)
      else
         cntratestr = '---'
      endif
      dd = cntbg
      call xdstr(dd,sigdig,bgcntstr,slen)
      call tclvard('uplimit(bgcnt)', dd, readonly, global, status)

      clen = 14
      call xwrite(' ', 10)
      call xwrite(' Raw            Raw Rate       Background    '//
     &            ' Area', 10)
      call xwrite(' Counts         (cnts/sec)     Counts        '//
     &            ' (det-pix)', 10)
      write(ZWRite,'(2a,1x,a,1x,a,1x,a)') '# ', cntstr(:clen), 
     &             cntratestr(:clen), bgcntstr(:clen), areastr(:clen) 
      call xwrite(ZWRite, 10)
c
c  Classical (sosta) method
c
      call xwrite(' ', 10)
      call xwrite(' Classical Method:', 10)
      call xwrite(' Upper Limit    Rate', 10)
      write(ZWRite,'(1x,3a,5x,a)') '(Sigma=', sigstr(1:LENACT(sigstr)), 
     &                             ')','(cnts/sec)'
      call xwrite(ZWRite, 10)

      if ( cntsrc.lt.cntbg ) then
         call xwrite(' Method not applicable for given inputs', 10)
         ulres = RNULL()
      else
         call ullim(cntsrc, cntbg, conflev, ulres, status)
         if ( status.ne.0 ) then
            ulres = RNULL()
         endif
      endif

      dd = ulres
      if ( ISRNULL(ulres) ) then
         ulstr = '---'
      else
         call xdstr(dd,sigdig,ulstr,slen)
      endif
      call tclvard('uplimit(value)', dd, readonly, global, status)

      if ( exposure.gt.0.d0 ) then
         if ( ISRNULL(ulres) ) then
            dd = ulres
            ulratestr = '---'
         else
            dd = ulres/exposure
            call xdstr(dd,sigdig,ulratestr,slen)
         endif
      else
         dd = RNULL()
         ulratestr = '---'
      endif
      call tclvard('uplimit(ulrate)', dd, readonly, global, status)

      write(ZWRite,'(2a,1x,a)') '# ', ulstr(:clen), ulratestr(:clen)
      call xwrite(ZWRite, 10)
      call xwrite(' ', 10)
c
c  Bayesian method (hidden)
c
      call xwrite(' Bayesian Method: Physc Rev D 54 (1996) 166:',15)
      call xwrite(' Upper Limit    Rate', 15)
      write(ZWRite,'(1x,3a,6x,a)') '(Sigma=', sigstr(1:LENACT(sigstr)), 
     &                             ')','(cnts/sec)'
      call xwrite(ZWRite, 15)
      dd = BAYLIKE(1, cntsrc, cntbg, conflev, status)
      call xdstr(dd,sigdig,ulstr,slen)
      call tclvard('uplimit(ullike)', dd, readonly, global, status)
      if ( exposure.gt.0. ) then
         dd = dd/exposure
         call xdstr(dd,sigdig,ulratestr,slen)
      else
         ulratestr = '---'
      endif
      call tclvard('uplimit(ullikerate)', dd, readonly, global, status)

      write(ZWRite,'(2a,1x,a)') '# ', ulstr(:clen), ulratestr(:clen)
      call xwrite(ZWRite, 15)
      call xwrite(' ', 15)
c
c  Method using prior probability function
c
      call xwrite(' Bayesian method with prior probability '//
     &              'function from Apj 374:344-355 1991:', 10)
      call xwrite(' Upper Limit    Rate', 10)
      write(ZWRite,'(1x,3a,6x,a)') '(Sigma=', sigstr(1:LENACT(sigstr)),
     &                             ')','(cnts/sec)'
      dd = BAYPRI(cntsrc, cntbg, conflev, status)
      call xdstr(dd,sigdig,ulstr,slen)
      call tclvard('uplimit(ulpri)', dd, readonly, global, status)
      if ( exposure.gt.0. ) then
         dd = dd/exposure
         call xdstr(dd,sigdig,ulratestr,slen)
      else
         ulratestr = '---'
      endif
      call tclvard('uplimit(ulprirate)', dd, readonly, global, status)

      call xwrite(ZWRite, 10)
      write(ZWRite,'(2a,1x,a)') '# ', ulstr(:clen), ulratestr(:clen)
      call xwrite(ZWRite, 10)
      call xwrite(' ', 10)

      status = 0

      RETURN
      END
