      SUBROUTINE FINDING_CHART(Cmdid, Status)
      implicit none
c
c  Reads RA DEC from cursor then gets Finding Chart from 
c  the ST service via emulated WWW query
c
c  I  cmdid     (i)  Command id
c  O  status    (i)  Error flag (0 = OK)
c
      INTEGER*4 Cmdid, Status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 LENACT  
      character(10) mapid, name
      INTEGER*4  equinox, szx, szy
      REAL*4 xpix, ypix
      REAL*8 ximg, yimg, dx, dy
      REAL*8 xsky, ysky, xout, yout, epoch, ineqx, outeqx
c
      INTEGER*4 argc 
      character*(MAX_FILELEN) cmd
      character*(MAX_IDSTR) wcsid
      character(20) system, xlab, ylab
      character(255) string
      integer*4 ierr
      REAL*4 field_width
      LOGICAL cursor
      LOGICAL isdisplay, isloaded
      DATA xpix/0./, ypix/0./
      save xpix, ypix
c
      cursor = .FALSE.
      field_width = 2.0
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL gparl(Cmdid,'CURSOR',cursor,Status)
      CALL gparr(Cmdid,'FIELD_WIDTH',field_width,status)
      if ( status.ne.0 ) return

      IF ( cursor ) THEN
         IF ( .NOT.isdisplay() ) THEN
            CALL XWRITE(' Please display an image first',10)
            status=1
            RETURN
         ENDIF
         mapid='DIS'
      ELSE
         mapid='CUR'
      ENDIF
      call mapname(mapid, name)
      write(ZWRite,'(2a)') ' Conversion based on ', name(:lenact(name))
      call xwrite(ZWRite, 10)
      if ( .not.isloaded(mapid) ) then
         CALL XWRITE('Image not loaded',10)
         Status = -1
         RETURN
      endif
c
c get ra dec center image north and pixsize

      CALL gheadi(mapid, 'SZX',szx,0, status)
      CALL gheadi(mapid, 'SZY',szy,0, status)
      wcsid = ' '
      CALL gheads(mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
         CALL XWRITE(' WCSID not found',10)
         Status = -1
         RETURN
      endif

c get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)
c
c calculate from x/ypix the ra and dec
c
      call XWRITE(' Enter location for query', 10)
      call inxypix(cursor, xpix, ypix)
      dx = xpix
      dy = ypix
      call wcsimgpix(wcsid, ximg, yimg, dx, dy, 0, status)
      call wcsimgsky(wcsid, ximg, yimg, xsky, ysky, equinox, 1, status)
      
c writing on the screen
      CALL XWRITE(' ',10)
      call write_radec(mapid, xsky, ysky, equinox)
 
      ineqx = equinox
      outeqx = 1950
      call wcsskyinfo(wcsid, system, xlab, ylab, epoch)
      call wcsskysky(system, ineqx, xsky, ysky, 'FK4', outeqx,
     &               xout, yout, status)
      if ( status.ne.0 ) then
         call xwrite(' Failed to convert coordinates to RA/Dec system', 
     &               10)
         return
      endif

      cmd = 'st.pl'
      write(string,
     & '(1x,f10.4,1x,f10.4,1x,''B1950 f '',f5.1,'' 3'')')
     &   xout, yout, field_width
      string = cmd(:lenact(cmd))//string(:lenact(string))//
     &                            ' > st.fits'
      call xwrite (' Writing FITS image to st.fits', 10)
      call xwrite (string, 15)
      call spawn(string,lenact(string),ierr)
      status = 0

      RETURN
      END
