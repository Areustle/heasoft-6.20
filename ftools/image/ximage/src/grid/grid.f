      SUBROUTINE grid(cmdid, status)
      IMPLICIT NONE
c
c Plot grid on the image
c
c I cmdid    i  Command id
c O status   i  Error flag (0=OK)
c
      INTEGER*4 cmdid, status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/pi.inc'

      INCLUDE 'gridlab.inc'
      INCLUDE 'greg.inc'
      INCLUDE 'projec.inc'
c
c  Local variables
c
c step_alpha  step in ra for the grid 
c step_delta  step in dec for the grid
c galactic  draw galactic coordinates
c ticks    .true. draw only tickmarks 
c          .false. draw complete grid (the default)
c
c input parameters/variables

      INTEGER*4 argc, color, lwidth, lstyle
      character(40) stepstr
      REAL*8 step_alpha , step_delta
      LOGICAL galactic
c
      CHARACTER*(MAX_IDSTR) mapid
      LOGICAL isdisplay,isloaded 

      REAL*8 fovx, fovy, bii, lii, dimgequ
      REAL*8 crval(2), drpix(2), ddelt(2), crota2
      character(80) type
      REAL*8 xcen, ycen, zmx, zmy
      INTEGER*4  equinox, equinox1, szx, szy, imgequ
      real*4 xpix, ypix, ximg, yimg, xref, yref, dr1, dr2
      real*8 pixsize(2), rnorth, ra, dec, raout, decout, dd
c
c Initialize variables 
      color = -1
      lwidth = 1
      lstyle = 1
      galactic = .FALSE.
      ticks= .FALSE.
      step_alpha = -1.0
      step_delta = -1.0
c
c  Retrieve arguments as distance between lines
c
      status = 0
      call numcarg(cmdid,argc,status)
      call nextcarg(cmdid,stepstr, 40, status)
      if ( status.eq.0 ) then
         argc = argc - 1
         call strnum(stepstr,8,step_alpha,status)
      endif
      call nextcarg(cmdid,stepstr, 40, status)
      if ( status.eq.0 ) then
         argc = argc - 1
         call strnum(stepstr,8,step_delta,status)
      endif
      status = 0
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'GALACTIC',galactic,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARI(Cmdid,'LSTYLE',lstyle,Status)
      CALL GPARL(Cmdid,'TICKS_ONLY',ticks,Status)
      if ( status.ne.0 ) return
c
c if image loaded and it is display
c
      if (.not.isdisplay() ) then
         call XWARN('Image is not displayed', 5)
         status=1 
         return
      endif

      mapid='DIS'
      if (.not.isloaded(mapid) ) then
         call XWARN('Image is not loaded', 5)
         status=1 
         return
      endif
c
c  If color not set by user, plot red for galactic and last color
c  otherwise
c
      if ( color.eq.-1 ) then
c        if ( galactic ) then
c           color = 2
c        else
            color = 16
            call get_color(color)
c        endif
      endif

      call PGSAVE
      call line_pgstate(color,lwidth,lstyle)
c
      call ghdwcs(mapid, crval, drpix, ddelt, crota2, imgequ,
     &            type, status)
c     Duplicate requests -- for now (need crota2 and rnorth)
      call get_skyhea(mapid,ra,dec,rnorth,dd,pixsize,
     &                imgequ,type,status)

      call get_refram (mapid, szx, szy, zmx, zmy, xcen, ycen,
     &                 status)
      status = 0
c
c  Ask window for actual (detector) coordinates of visible area
c
      call PGQWIN(gx1,gx2,gy1,gy2)
c
c  New crval is center of visible area
c
      xpix = (gx2+gx1)/2.
      ypix = (gy2+gy1)/2.
      call calimgpix(szx,szy,zmx,zmy,xcen,ycen,xpix,ypix,
     &               ximg,yimg,2)
      xref = ximg - (szx/2. + 0.5)
      yref = yimg - (szy/2. + 0.5)
      dr1 = 0.
      dr2 = 0.
      call ximprec(ra, dec, imgequ, 1950)
      call XYTRD (xref,yref,pixsize,raout,decout,ra,dec,
     &            rnorth,dr1,dr2)
      call ximprec(raout, decout, 1950, imgequ)
      crval(1) = raout
      crval(2) = decout
c
c  Field of View
c
      fovx = dabs(ddelt(1))*(gx2-gx1)
      fovy = dabs(ddelt(2))*(gy2-gy1)
c     fovx = dabs(ddelt(1))*dble(szx*zmx)
c     fovy = dabs(ddelt(2))*dble(szy*zmy)
c
c if steps not assigned calculate the defaults
      IF ( step_alpha.LT.0.0 ) THEN
          IF ( galactic ) THEN  
             dimgequ=imgequ
c!
c!  Need conversion to galactic for all projections
c!
             CALL CTG(crval(1),crval(2),'FK4',dimgequ,lii,bii)
             IF (DABS(bii).LT.(90.0-fovy/2.0)) THEN
                fovx = fovx/DCOS(DABS(bii+fovy/2.0)/(180.D0/PI))
                step_alpha = fovx/4.0
             ELSE
                fovx = 360.0
                step_alpha = fovx/8.0
             ENDIF
          ELSE
             IF (DABS(crval(2)).LT.(90.0-fovy/2.0)) THEN
                 fovx = fovx/DCOS(DABS(crval(2)+fovy/2.0)/(180.D0/PI))
                 step_alpha = fovx/4.0
             ELSE
                 fovx = 360.0
                 step_alpha = fovx/8.0
             ENDIF
          ENDIF 
          step_delta = fovy/4.0
      ELSE
         if ( step_delta.lt.0 ) step_delta = step_alpha
      ENDIF
      write (ZWRite, *) ' alpha step: ', step_alpha
      call XWRITE(ZWRite, 15)
      write (ZWRite, *) ' delta step: ', step_delta
      call XWRITE(ZWRite, 15)

      if ( step_alpha.le.0. .or. step_delta.le.0. ) then
          call XWRITE(' Invalid grid step', 10)
          status = -1
          goto 500
      endif

c get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)
c
c precess coordinates at current ximage equinox 
c input coordinates ra and dec are in degrees
c
      IF (galactic) THEN
         CALL XWRITE(' Galactic grid', 10)
         equinox1=1950
      ELSE
         equinox1=equinox
      ENDIF
      call ximprec(crval(1),crval(2),imgequ,equinox1)
c
c sets grids variables: i_system (coord system set to equatorial) 
C and p_type (type of projection set to gnomonic)
c
      i_system = 2
      p_type = 1
c      
c draw a box
      if ( galactic ) ticks = .FALSE.
      CALL gridrnd (step_delta, 1)
      CALL gridrnd (step_alpha, 2)
      call xwrite(' After adjusting to "nice" number', 15)
      write (ZWRite, *) ' alpha step: ', step_alpha
      call XWRITE(ZWRite, 15)
      write (ZWRite, *) ' delta step: ', step_delta
      call XWRITE(ZWRite, 15)

      accurd = 0.01

      gux1 = pi/180.D0*(-ddelt(1)*(gx2-gx1)/2.D0)
c     gux1 = pi/180.D0*(-ddelt(1)*dble(szx*zmx)/2.D0)
      gux2 = -gux1
      guy1 = pi/180.D0*(-ddelt(2)*(gy2-gy1)/2.D0)
c     guy1 = pi/180.D0*(-ddelt(2)*dble(szy*zmy)/2.D0)
      guy2 = -guy1

      gux = (gx2-gx1)/(gux2-gux1)
      guy = (gy2-gy1)/(guy2-guy1)

      pangle = pi/180.D0*crota2
 
      do while ( pangle.GE.2.D0*pi .or. pangle.LT.0.D0 )
         IF ( pangle.GT.0.D0)THEN
            pangle = pangle - 2.D0*pi
         ELSE
            pangle = pangle + 2.D0*pi
         ENDIF        
      enddo

      a0 = pi/180.D0*crval(1)
      d0 = pi/180.D0*crval(2)
      decmin = pi/180.D0*(crval(2)-((gy2-gy1)*ddelt(2)/2.D0))
      decmax = pi/180.D0*(crval(2)+((gy2-gy1)*ddelt(2)/2.D0))
      ramin = pi/180.D0*(crval(1)-((gx2-gx1)/2.D0*ddelt(1)))
      ramax = pi/180.D0*(crval(1)+((gx2-gx1)/2.D0*ddelt(1)))
c     decmin = pi/180.D0*(crval(2)-(dble(szy*zmy)*ddelt(2)/2.D0))
c     decmax = pi/180.D0*(crval(2)+(dble(szy*zmy)*ddelt(2)/2.D0))
c     ramin = pi/180.D0*(crval(1)-(dble(szx*zmx)/2.D0*ddelt(1)))
c     ramax = pi/180.D0*(crval(1)+(dble(szx*zmx)/2.D0*ddelt(1)))
c
      CALL setpro(a0,d0,pangle,p_type)
      
      call pgbbuf
      IF ( .NOT.galactic ) THEN
         coorsys = 'CEL'
         CALL gridmp(step_alpha, step_delta, status)
      ELSE
         coorsys = 'GAL'
         CALL galac(step_alpha, step_delta, status)
      END IF
      call pgebuf
c
c  Cover up gridline bits that overlay frame and restore PGPLOT
c  attributes
c
  500 continue
      CALL PGUNSA
      call pgbox('BC',0.0,0,'BC',0.0,0)

      IF ( status.ne.0 ) THEN
         CALL XWRITE(' Error in GRID command',10)
      ENDIF
      call jrncmd(cmdid,status)

      RETURN
      END
 
