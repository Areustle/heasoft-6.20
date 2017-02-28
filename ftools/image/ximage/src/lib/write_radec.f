      SUBROUTINE write_radec(Mapid, Ra, Dec, Equinox)
      implicit none
c
c  writes input ra/dec (degrees) in hr min sec, and deg min sec
c  and tranform and write the galactic coordintes 
c  I  Mapid    s Mapid (if blank, assume RA/Dec system)
c  I  Ra       d Rt. Asc. in degrees
c  I  Dec      d Declination in degrees
c  I  Equinox  i equinox of coordinates
c
      CHARACTER*(*) Mapid
      INTEGER*4 Equinox
      REAL*8 Ra , Dec

      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
c
c  Local variables
c
      CHARACTER*(MAX_IDSTR) wcsid
      INTEGER status, lenra, lendec, clen, LENACT
      REAL*4 dr3(3)
      REAL*8 lii, bii, epoch
      character(40) strra, strdec, system, proj, xlab, ylab, unit
      LOGICAL hideeqx

      clen = 15

      wcsid = ' '
      if ( Mapid.ne.' ' ) call gheads(Mapid, 'WCSID', wcsid, 0, status)
c      
c        Use wcsid to determine system of input coordinates
c        but overwrite with input equinox
c      
      if ( wcsid.eq.' ' ) then
         system = 'EQUATORIAL'
         xlab = 'RA'
         ylab = 'Dec'
      else
         call wcsfrminfo(wcsid, system, proj, xlab, ylab, unit, epoch)
      endif
      epoch = Equinox

      hideeqx = .TRUE.
      if ( xlab.eq.'RA' ) then
         call cnv_radec(strra,strdec,Ra,Dec,dr3,dr3,2,4,status)
         hideeqx = .FALSE.
      else
         call wcsformat(system, Ra, Dec, strra, strdec)
      endif

c writing on the screen
      if ( strra.eq.' ' ) strra = 'UNDEF'
      if ( strdec.eq.' ' ) strdec = 'UNDEF'
      if ( hideeqx ) then
         WRITE (zwrite,94000) xlab(:LENACT(xlab)), ylab(:LENACT(ylab)),
     &                        strra(:clen), strdec(:clen)
      else
         WRITE (zwrite,94001) xlab(:LENACT(xlab)), ylab(:LENACT(ylab)),
     &                        Equinox, strra(:clen), strdec(:clen)
      endif
      if ( xlab.eq.'RA' ) then
         CALL XWRITE(zwrite,10)
      endif

      if ( strra.ne.'UNDEF' .and. strdec.ne.'UNDEF' ) then
         call xdstr(Ra, 12, strra, lenra)
         call xdstr(Dec, 12, strdec, lendec)
         if ( hideeqx ) then
            WRITE (zwrite,94000) xlab(:LENACT(xlab)), 
     &                  ylab(:LENACT(ylab)), strra(:clen), strdec(:clen)
         else
            WRITE (zwrite,94001) xlab(:LENACT(xlab)), 
     &                  ylab(:LENACT(ylab)), Equinox, strra(:clen), 
     &                  strdec(:clen)
         endif
         CALL XWRITE(zwrite,10)

         call wcsskysky(system, epoch, Ra, Dec, 'GALACTIC', 2000.d0,
     &                  lii, bii, status)

         if ( status.eq.0 ) then
            CALL XWRITE(' ',10)
            WRITE (zwrite,94002) lii , bii
            CALL XWRITE(zwrite,10)
         endif
         status = 0
      endif

      RETURN
94000 FORMAT (4X,a,'/',a,' = ',a,1x,a)
94001 FORMAT (4X,a,'/',a,' (',i4,') = ',a,1x,a)
94002 FORMAT (4X,'LII/BII = ',f6.2,1x,f6.2)
      END

