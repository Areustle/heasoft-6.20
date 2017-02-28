      SUBROUTINE iminfo(cmdid, status)
      implicit none
c
c Write information on the image display
c
c  I  cmdid    (i)  Command id
c  I  mapid    (s)  Map id string
c  O  status   (i)  Error flag (0=OK)
c
      integer cmdid, status

      INCLUDE '../include/startup.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/pi.inc'
c
c input variables 
c
      INTEGER color, lwidth
      REAL*4 xpix, ypix, csize
      character(10) font
      LOGICAL short, cursor, textonly, scale, compass, xset, yset
      character*(MAX_IDSTR) mapid
c
c  Local variables
c
      INTEGER i, n, argc, lenact, iexp, di, idx, slen, sigdig
      REAL*4 xcmp(3), ycmp(3), xst, yst, x(2), y(2), pnt(2), dr
      REAL*4 xxe, yye, xxn, yyn, rho, rhone
      REAL*4 xlef, xrit, ytop, ybot, pixd, xch, ych
      REAL*4 xmin, xmax, ymin, ymax, lnhgt
      REAL*4 xhalf, yhalf, scwid, schgt, schalf, prval
      REAL*4 sgn1, sgn2, widscale, rnull
      REAL*4 xinf, yinf, xscl, yscl
      character(10) cexp
      character(20) prtxt
      character(40) system, proj, xlab, ylab, unit
      character(80) string, in(5), ds
      character(80) telescop, instrume, detnam
      REAL*8 crot, rcrot, ddelt1, ddelt2, pixscl, dd
      LOGICAL infotext, isdisplay, isloaded, isrnull, isdnull, default
c
c initialize variable
      string = ' '
      default = .true.
      infotext = .true.
      textonly = .false.
      short = .false.
      scale = .false.
      compass = .false.
      cursor = .false.
      color = 16
      call get_color(color)
      csize = 1.0
      lwidth = 1
      font = ' '
      do i = 1, 5
        in(i) = ' '
      enddo
c
c Use boundaries to calculate safe flag value
c
      call PGQWIN(xmin,xmax,ymin,ymax)
      widscale = -1.
      xpix = rnull()
      ypix = rnull()
      xset = .FALSE.
      yset = .FALSE.
c
      status = 0
      CALL GPARI(cmdid,'COLOR',color,Status)
      CALL GPARR(cmdid,'CSIZE',csize,Status)
      CALL GPARI(cmdid,'LWIDTH',lwidth,Status)
      CALL GPARS(cmdid,'FONT',font,Status)
      CALL GPARR(cmdid,'XPIX',xpix,status)
      CALL GPARR(cmdid,'YPIX',ypix,status)
      CALL GPARL(cmdid,'CURSOR',cursor,status)
      CALL GPARL(cmdid,'SHORT',short,status)
      CALL GPARL(cmdid,'TEXTONLY',textonly,status)
      CALL GPARL(cmdid,'COMPASS',compass,status)
      CALL GPARL(cmdid,'SCALE',scale,status)
      CALL GPARR(cmdid,'WIDSCALE',widscale,status)
      if ( status.ne.0 ) return
c
c  Retrieve argument as label string
c
      call numcarg(cmdid,argc,status)
      if ( argc.eq.1 ) then
         call nextcarg(cmdid,string, 80, status)
         argc = argc - 1
      endif
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      if ( status.ne.0 ) return
c
c check if display up and image loaded 
c
      IF ( .not.isdisplay() ) then
         call XWARN(' No display',5)
         status=1
         RETURN
      ENDIF

      mapid = 'DIS'
      if ( .not. isloaded(mapid) ) then
         call XWRITE (' Image not loaded', 10)
         status=1
         RETURN
      endif
c
c start here check if x and y location are specified and 
c if test for the other logical to get the final info plot
      if ( .not.isrnull(xpix) ) xset = .TRUE.
      if ( .not.isrnull(ypix) ) yset = .TRUE.
      if ( short .or. scale .or. compass ) then
         infotext = .false.
         default = .false.
      endif
      if ( short .or. infotext ) then
         if ( textonly ) then
            default = .false.
         else
            scale = .true.
            compass = .true.
         endif
      endif
c
c Setup PGPLOT parameters
c Base spacing on character height
      call PGSAVE
      call text_pgstate(color, csize, lwidth, font)
      call PGQCS(4,xch,ych)
      lnhgt = ych*1.25
      if ( .not.xset ) xpix = xmin + ych
      if ( .not.yset ) ypix = ymax - ych/2. - lnhgt
      IF( infotext ) THEN
         if ( cursor ) then
            call xwrite(' Select a location for info to be printed:',10)
            call tclreslr('select', pnt, n, 2, status)
            if ( status.ne.0 ) return
            xpix = pnt(1)
            ypix = pnt(2)
            xinf = xpix
            yinf = ypix
         endif
c
c write input text
         IF ( string.NE.' ' ) THEN
            CALL PGTEXT(xpix,ypix,string(:LENACT(string)))
            RETURN
         ENDIF
c
c Get header information to print 
c get info from the header
         call gheads(mapid, 'TELESCOP', telescop, 0, status)
         call gheads(mapid, 'INSTRUME', instrume, 0, status)
         call gheads(mapid, 'DETNAM', detnam, 0, status)
         idx = 1
         if ( telescop.ne.' ' ) then
            in(idx) = telescop(1:LENACT(telescop))//' Observatory '
            idx = idx + 1
         endif
         if ( instrume.ne.' ' ) then
            if ( detnam.eq.' ' ) then
               in(idx) = instrume(1:LENACT(instrume))//' Detector '
            else
               in(idx) = instrume(1:LENACT(instrume))//' '//
     &                   detnam(1:LENACT(detnam))//' Detector '
            endif
            idx = idx + 1
         endif
c
c Field name
         call gheads(mapid, 'OBJECT', ds, 0, status)
         if ( ds.ne.' ' ) then
            in(idx) = 'Field name : '//ds(:LENACT(ds))
            idx = idx + 1
         endif
c
c Exposure time
         call gheadd(mapid, 'EXPOSURE', dd, 0, status)
         iexp=dd
         if ( iexp.gt.0 ) then
            call xistr(iexp,cexp,di)
            in(idx) = 'Exposure time : '//cexp(:di)//' sec'
            idx = idx + 1
         endif
c
c Filter information get what is in the header
         call gheads(mapid, 'FILTER', ds, 0, status)
         di=lenact(ds)
         if ( di.gt.0 ) then
            in(idx)= 'Filter : '//ds(:di)
         endif
c
c Print information
         DO i=0,4 
           ds=in(i+1)
           dr = ypix - real(i)*lnhgt
           CALL PGTEXT(xpix,dr,ds(1:LENACT(ds)))
         ENDDO
      ENDIF
c
c get image 
      call gheadd(mapid, 'DDELT1', ddelt1, 0, status)
      call gheadd(mapid, 'DDELT2', ddelt2, 0, status)
      call gheadd(mapid, 'CROTA2', crot, 0, status)
      if ( isdnull(crot) ) crot = 0.
c  Rotation convention - counter-clockwise
      rcrot = -crot/(180.d0/PI)
      pixscl = abs(ddelt1)
      if ( ddelt1.gt.0 ) then
         sgn1 = 1.0
      else
         sgn1 = -1.0
      endif
      if ( ddelt2.gt.0 ) then
         sgn2 = 1.0
      else
         sgn2 = -1.0
      endif
c
c Center for compass/scale
c Locations relative to compass corner
      rho = ych*2.
      rhone = rho + 0.75*ych
      xxn = -sgn1*rhone*DSIN(rcrot)
      yyn = -sgn1*rhone*DCOS(rcrot)
      xxe = -sgn2*rhone*DCOS(rcrot)
      yye =  sgn2*rhone*DSIN(rcrot)
c
c Make relative to compass center
      xlef = MIN(0.,xxn,xxe)
      xrit = MAX(0.,xxn,xxe)
      ybot = MIN(0.,yyn,yye)
      ytop = MAX(0.,yyn,yye)
      xhalf = (xrit - xlef)/2.
      yhalf = (ytop - ybot)/2.
      if ( .not.xset .or. infotext ) xpix = xmax - ych*3.5 - xhalf
      if ( .not.yset .or. infotext ) ypix = ypix - yhalf
      if ( (scale.or.compass) .and. cursor ) then
         call xwrite(' Select a location for compass/scale',10)
         call tclreslr('select', pnt, n, 2, status)
         if ( status.ne.0 ) return
         xpix = pnt(1)
         ypix = pnt(2)
      endif
      xscl = xpix
      yscl = ypix
c
c Do Compass
      if ( compass ) then
         xst = xpix - xhalf - xlef
         yst = ypix - yhalf - ybot
         xxn = xxn + xst
         xxe = xxe + xst
         yyn = yyn + yst
         yye = yye + yst
         xcmp(1) = -sgn1*rho*DSIN(rcrot) + xst
         ycmp(1) = -sgn1*rho*DCOS(rcrot) + yst
         xcmp(2) = xst
         ycmp(2) = yst
         xcmp(3) = -sgn2*rho*DCOS(rcrot) + xst
         ycmp(3) =  sgn2*rho*DSIN(rcrot) + yst
         call pgsclp(0)
         CALL PGLINE(3,xcmp,ycmp)
         call pgsclp(1)
         call pgptxt(xxn,yyn-ych/4.,0.0,0.5,'N')
         call pgptxt(xxe,yye-ych/4.,0.0,0.5,'E')
c
c  Base location of scale on compass
         yscl = ypix - yhalf - ych*1.5
      endif
c
c Do  Scale
      if ( scale ) then
         scwid = ych*2.
         schgt = ych/4.
c
c  Determine size of scale (user-entered or automatic)
c
         call wcsfrminfo(mapid, system, proj, xlab, ylab, unit, dd)
         pixd = scwid*pixscl
         prval = pixd

         if ( widscale.gt.0 ) then

            prval = widscale
c
c         Assume input in arcmin for sky coords
c
            if ( unit.eq.'deg' ) prval = widscale/60.

         elseif ( unit.eq.'deg' ) then

            if ( pixd.ge.1 ) then
               prval = nint(pixd*2)/2.
            else
               if ( pixd*60.0 .ge. 1 ) then
                  prval = float(nint(pixd*60.))/60.0
               else
                  prval = float(nint(pixd*3600.))/3600.0
                  if ( prval.lt.1 ) prval = pixd
               endif
            endif

         endif
c
c  Determine best unit to display as
c
         scwid = prval/pixscl
         if ( unit.eq.'deg' ) then
            if ( prval.lt.1. ) then
               prval = prval*60.0
               if ( prval .ge. 1.0 ) then
                  unit = 'arcmin'
               else
                  prval = prval*60.
                  unit = 'arcsec'
               endif
            endif
         endif
         
         if ( prval.gt.0 .and. prval.lt.1 ) then
            sigdig = 0
            dd = prval
            do while ( nint(dd).lt.1 ) 
               sigdig = sigdig + 1
               dd = dd*10.0
            enddo
            dd = dble(nint(dd))*0.1**sigdig
            call xdstr(dd, sigdig+1, ds, slen)
            scwid = dd/pixscl
            write (prtxt,'(a,1x,a)') ds(1:slen), unit(:lenact(unit))
         elseif ( int(prval).eq.nint(prval) ) then
            di = int(prval)
            call xistr(di,ds,slen)
            write (prtxt,'(a,1x,a)') ds(1:slen), unit(:lenact(unit))
         else
            write (prtxt,'(f4.1,1x,a)') prval, unit(:lenact(unit))
         endif
         schalf = (0.55*ych*(lenact(prtxt) + 2) + scwid)/2.
         CALL PGTEXT(xscl-schalf,yscl-ych/4.,prtxt(1:LENACT(prtxt)))
         write (ds,'(a,f8.3)') 'scale width: ',scwid
         call XWRITE(ds, 25)
         x(1) = xscl + schalf
         y(1) = yscl
         x(2) = xscl + schalf - scwid
         y(2) = yscl
         call pgsclp(0)
         CALL PGLINE(2,x,y)
         x(1) = xscl + schalf
         y(1) = yscl - schgt
         x(2) = xscl + schalf
         y(2) = yscl + schgt
         CALL PGLINE(2,x,y)
         x(1) = xscl + schalf - scwid
         x(2) = xscl + schalf - scwid
         CALL PGLINE(2,x,y)
         call pgsclp(1)
      endif
      call PGUNSA
c
c  Journal command 
c
      if ( cursor ) then
         call sparl(Cmdid,'CURSOR',.FALSE.,status)
         if ( default ) then
            call sparl(Cmdid,'TEXTONLY',.TRUE.,status)
            call sparr(Cmdid,'XPIX',xinf,status)
            call sparr(Cmdid,'YPIX',yinf,status)
            call jrncmd(Cmdid, status)
            call sparl(Cmdid,'TEXTONLY',.FALSE.,status)
            call sparl(Cmdid,'SHORT',.TRUE.,status)
         endif
         if ( textonly ) then
            call sparr(Cmdid,'XPIX',xinf,status)
            call sparr(Cmdid,'YPIX',yinf,status)
         else
            call sparr(Cmdid,'XPIX',xpix,status)
            call sparr(Cmdid,'YPIX',ypix,status)
         endif
      endif
      call jrncmd(Cmdid, status)
      status = 0

      RETURN
      END
