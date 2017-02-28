      SUBROUTINE REMOVE_SOURCES(Cmdid, Map, Szx, Szy, Mapid, Status)
      IMPLICIT NONE
c
c Remove part of image and substitute with background value
c
c  I  cmdid  (i)  Command id
c  I  map    (r)  Image map
c  I  szx/y  (i)  Size of map
c  I  mapid  (s)  Map id string
c  O  status (i)  Error flag (0=OK)
c
      integer*4 Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
      include 'backgd.inc'
      include 'detect.inc'
c
c  Local variables
c
      INTEGER*4 n, argc, date(3), itime(3), seed, modulus
      INTEGER*4 nsources
      real*4 constant, rnull, pnts(4)
      REAL*4 xpos_min , xpos_max , ypos_min , ypos_max
      REAL*4 xbmin , xbmax , ybmin , ybmax
      REAL*4 x, y, xwid, ywid
      character(8) systime
      LOGICAL cursor , optimize , remove
      LOGICAL isloaded, isdisplay, isrnull
      INTEGER i , j , jj , ii , ibbace
      INTEGER ixmin, ixmax, iymin, iymax
      INTEGER xpos_temp , ypos_temp , IPOSS
      logical isinreg
      integer numreg, ireg
      real*4 ri, rj, rx, ry, rmin, rmax
      real*8 dx, dy
      character(20) constr, xmnstr, xmxstr, ymnstr, ymxstr
      character*(MAX_FILELEN) regionfile, regionsave
      character*(MAX_IDSTR) wcsid
      real*4 xoff, yoff
      logical xyspec, good, there
      integer LENACT

      logical detect
      integer*4 di
      real*8 xcen, ycen, zmx, zmy, dd
      real*4 ximg, yimg, bgimg
      character(100) ds
      character(80) mapcodes

c defaults and inits.
c
      cursor = .FALSE.
      DRAw_boxes = .FALSE.
      detect = .FALSE.
      nsources = 1
      xmnstr = ' '
      ymnstr = ' '
      xmxstr = ' '
      ymxstr = ' '
      regionfile = ' '
      regionsave = ' '
      constr = ' '

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'XMIN',xmnstr,Status)
      CALL GPARS(Cmdid,'XMAX',xmxstr,Status)
      CALL GPARS(Cmdid,'YMIN',ymnstr,Status)
      CALL GPARS(Cmdid,'YMAX',ymxstr,Status)
      CALL GPARI(Cmdid,'NSOURCES',nsources,Status)
      CALL GPARL(Cmdid,'CURSOR',cursor,Status)
      CALL GPARL(Cmdid,'DETECT_SOURCES',detect,Status)
      CALL GPARS(Cmdid,'CONSTANT',constr,Status)
      CALL GPARS(Cmdid,'REGIONFILE',regionfile,Status)
      if ( status.ne.0 ) return

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 10)
         Status = -1
         return
      endif
c
c  Check qualifiers
c
      constant = 0.
      call upc(constr)
      if ( constr.eq.'NULL' ) then
         constant = rnull()
      elseif ( constr.ne.' ' ) then
         call strnum(constr, 4, dd, status)
         constant = dd
      endif

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
            call xwrite(' X/Y_MIN, X/Y_MAX must all be specified', 10)
            status = -1
            return
         endif
      ENDIF
 
      IF ( cursor .AND. .NOT.isdisplay() ) THEN
         CALL XWRITE(' CURSOR requires displayed image', 10)
         Status = -1
         RETURN
      ELSEIF ( .NOT.cursor .AND. .NOT.detect .AND. .not.xyspec
     &         .and. regionfile.eq.' ' ) THEN
         CALL XWRITE(' Use CURSOR or other qualifier to define sources',
     &               10)
         Status = 1
         RETURN
      ENDIF
 
      IF ( detect ) THEN
         IF ( NUMdet.LE.0 ) THEN
            CALL XWRITE(' Error: no sources found by DETECT',10)
            RETURN
         ENDIF
         nsources = NUMdet
      ENDIF
c
c  calculate seed and make sure it is a large, odd integer
c
      CALL GETTIM(systime)
      READ (systime(1:2),'(i2)') itime(1)
      READ (systime(4:5),'(i2)') itime(2)
      READ (systime(7:8),'(i2)') itime(3)
      call XIDATE(date)
      seed = (date(1)*date(2)*date(3)
     &       +(itime(1)**2+itime(2)**2+itime(3)**2))*1234
      IF ( seed.LT.1000000 ) seed = seed + 1000000
      modulus = MOD(seed,2)
      IF ( modulus.EQ.0 ) seed = ABS(seed) - 1
c
c calculate background value and substitute in the area removed
c
c
      IF ( constr.eq.' ' ) THEN

         if ( NBOxes.gt.0 ) then
            call xwrite(' Using existing background calculation', 10)
            call prback(mapid, status)
            bgimg = BNEw
         else
            call getbgmap(Mapid, BGMap, BGSz, status)
            if ( Status.ne.0 ) return

            ibbace = 0
            optimize = .FALSE.
            SIGmult = DEFsigm
            BARylim = DEFbarl
            BXHprob = DEFhprob
            CALL DO_BACK(Map, Szx, Szy, Mapid, optimize, ibbace, Status)
            IF ( Status.NE.0 ) RETURN
            bgimg = BNEw
            NBOxes = 0
         endif

      ENDIF
c
c  Remove region
c
      if ( regionfile.ne.' ' ) then
c
c  Look for region file
c
         call qustrip(regionfile)
         inquire(FILE=regionfile, EXIST=there)
         if ( .not.there ) then
            call XTEND(regionfile,'reg')
            inquire(FILE=regionfile, EXIST=there)
         endif
         if ( .not.there ) then
            write(ZWRite, *) ' Region file not found: ',
     &                       regionfile(:LENACT(regionfile))
            call xwrite(ZWRite, 10)
            status = -1
            return
         endif
c
c    Plot region
c
         if ( isdisplay() ) then
            call ezregplot(mapid, regionfile, status)
         else
c
c    Set WCS data (Note: ezregplot sets wcs in display case)
c
            call gheads(mapid, 'WCSID', wcsid, 0, status)
            call setregwcs(wcsid, status)
         endif
c
c    Read in regions
c
         call xinitreg(regionfile, numreg, status)
         if ( status.ne.0 ) then
            call XWRITE (' Failed to read region', 10)
            Status = -1
            return
         endif

         xoff = xcen
         yoff = ycen
c
c  Set pixels that fall in region
c
         Status = 0
         do i = 1, Szx
            do j = 1, Szy

               ri = i
               rj = j
               call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,rx,ry,
     &                        ri,rj,1)
               dx = rx
               dy = ry
               good = .FALSE.
               do ireg = 1, numreg
                  good = good.or.isinreg(ireg,dx,dy)
               enddo
               if ( good ) then
                  if ( constr.ne.' ' ) then
                     Map(i,j) = constant
                  else
                     Map(i,j) = IPOSS(bgimg,seed)
                  endif
               endif
            enddo
         enddo

         call xfreereg(status)
         if ( status.ne.0 ) then
            call xwrite(' Failed to free regions', 10)
            status = 0
         endif

         goto 500

      endif
c
c loop around
c
      jj = 0
      DO 200 ii = 1 , nsources
         remove = .FALSE.
         IF ( detect ) THEN
            IF ( HOT(ii).EQ.1 ) THEN
               jj = jj + 1
               call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,DTSox(ii),
     &                        DTSoy(ii),ximg,yimg,2)
               WRITE (ZWRite,99001) jj , DTSox(ii) , DTSoy(ii)
               CALL XWRITE(ZWRite,10)
               xpos_min = ximg - BXHa(ii)/zmx
               ypos_min = yimg - BXHa(ii)/zmy
               xpos_max = ximg + BXHa(ii)/zmx
               ypos_max = yimg + BXHa(ii)/zmy
               call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xbmin,
     &                        ybmin,xpos_min,ypos_min,1)
               call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xbmax,
     &                        ybmax,xpos_max,ypos_max,1)
               remove = .TRUE.
            ENDIF
         ELSEIF ( cursor ) THEN
            call xwrite(' Select rectangular area for removal', 10)
            call tclreslr('select box', pnts, n, 4, status)
            if ( status.ne.0 ) return
            xbmin = pnts(1)
            ybmin = pnts(2)
            xbmax = pnts(3)
            ybmax = pnts(4)
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xbmin,ybmin,
     &                     xpos_min,ypos_min,2)
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xbmax,ybmax,
     &                     xpos_max,ypos_max,2)
            remove = .TRUE.
         ELSE
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xbmin,ybmin,
     &                     xpos_min,ypos_min,2)
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xbmax,ybmax,
     &                     xpos_max,ypos_max,2)
            remove = .TRUE.
         ENDIF
c
c draw box to be removed, if it hasnt already been detected
c
         IF ( remove ) THEN

            if ( isdisplay() ) then
               x = (xbmax + xbmin)/2.
               y = (ybmax + ybmin)/2.
               xwid = abs(xbmax - xbmin)
               ywid = abs(ybmax - ybmin)
               call jrnbox(x,y,xwid,ywid,0.,-1,-1,-1)
            endif
 
            IF ( xpos_min.GT.xpos_max ) THEN
               xpos_temp = xpos_min
               xpos_min = xpos_max
               xpos_max = xpos_temp
            ENDIF
            IF ( ypos_min.GT.ypos_max ) THEN
               ypos_temp = ypos_min
               ypos_min = ypos_max
               ypos_max = ypos_temp
            ENDIF
 
            ixmin = MAX(1,NINT(xpos_min))
            ixmax = MIN(Szx,NINT(xpos_max))
            iymin = MAX(1,NINT(ypos_min))
            iymax = MIN(Szy,NINT(ypos_max))
            DO 80 i = ixmin , ixmax
               DO 60 j = iymin , iymax
                  IF ( constr.ne.' ' ) THEN
                     Map(i,j) = constant
                  ELSE
                     Map(i,j) = IPOSS(bgimg,seed)
                  ENDIF
 60            CONTINUE
 80         CONTINUE
         ENDIF
 200  CONTINUE
 500  CONTINUE
c
c reset max and min
c
      rmin = rnull()
      rmax = rnull()
      DO i = 1 , Szx
         DO j = 1 , Szy
            if ( .not.isrnull(Map(i,j)) ) then
               if ( isrnull(rmin) .or. MAP(i,j).lt.rmin ) then
                  rmin = MAP(i,j)
               endif
               if ( isrnull(rmax) .or. MAP(i,j).gt.rmax ) then
                  rmax = MAP(i,j)
               endif
            endif
         ENDDO
      ENDDO
      dd = rmin
      call gheadd(mapid, 'DATAMIN', dd, 1, status)
      dd = rmax
      call gheadd(mapid, 'DATAMAX', dd, 1, status)
c
c  Append operation to MAPCODES
c
      call gheads(mapid, 'MAPCODES', mapcodes, 0, status)
      write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'Rm'
      call gheads(mapid, 'MAPCODES', ds, 1, status)
      call expiremap(mapid, status)

      RETURN
99001 FORMAT (' Removing source ',i4,' at ',2F10.2)
      END
