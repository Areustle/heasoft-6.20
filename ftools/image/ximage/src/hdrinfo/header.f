      SUBROUTINE header(Cmdid, Status)
      
      IMPLICIT NONE
c
c show header of current image or current exposure map
c
c  I  Cmdid     (i) Command id
c  O  Status    (i) Error flag (0=OK)
c
      INTEGER*4 Cmdid, Status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      CHARACTER*(MAX_IDSTR) mapid

      integer argc
      INTEGER*4 lenact, equinox, imgequ, szx, szy, di1, di2    
      REAL*4 dr1, dr3(3)
      REAL*8 zmx, zmy
      REAL*8 crot, rnorth, roll, pixsize(2), xcen, ycen, dd1
      REAL*8 ximg, yimg, xsky, ysky, epoch
      character(80) ds1, ds2
      REAL*4 size1, size2, sq_arcmin, sq_deg
      INTEGER isqmin, isqdeg
      character(24) csqmin , csqdeg
      CHARACTER*(MAX_IDSTR) wcsid
      character(40) strra, strdec, system, proj, xlab, ylab, unit
      LOGICAL isloaded, isdnull

      mapid = 'CUR'

      status = 0

      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'MAPID',mapid,status)
      if ( status.ne.0 ) return

      if ( mapid.eq.'?' ) then
         call lsmapid
         return
      endif
c
      if ( .not. isloaded(mapid) ) then
         call XWRITE (' Image not loaded', 10)
         status=1 
         RETURN
      endif

c get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)
c
c telescope instrument object, date
      call gheads(mapid, 'TELESCOP', ds1, 0, status)
      call gheads(mapid, 'INSTRUME', ds2, 0, status)
      CALL UPC(ds1)
      CALL UPC(ds2)
      WRITE (ZWRite,99001) ds1(1:LENACT(ds1)) , 
     &                     ds2(1:LENACT(ds2))
      CALL XWRITE(ZWRite,10)
c
      call gheads(mapid, 'FILTER', ds1, 0, status)
      IF ( ds1.ne.' ' ) then
          WRITE (ZWRite,'(''   Filter        : '',a)') ds1
          CALL XWRITE(ZWRite,10)
      ENDIF
      call gheads(mapid, 'OBJECT', ds1, 0, status)
      WRITE (ZWRite,99002) ds1
      CALL XWRITE(ZWRite,10)
c
      call gheads(mapid, 'DATE-OBS', ds1, 0, status)
      di1 = INDEX(ds1, 'T')
      ds1(di1:di1) = ' '
      WRITE (ZWRite,99003) ds1
      CALL XWRITE(ZWRite,10)
c
      call gheads(mapid, 'DATE-END', ds1, 0, status)
      di1 = INDEX(ds1, 'T')
      ds1(di1:di1) = ' '
      WRITE (ZWRite,99004) ds1
      CALL XWRITE(ZWRite,10)
c
      call gheadd(mapid, 'EXPOSURE', dd1, 0, status)
      call xdstr(dd1, -1, ds1, di1)
      WRITE (ZWRite,99005) ds1(:di1)
      CALL XWRITE(ZWRite,10)

      call get_refram(mapid,szx, szy,zmx,zmy, xcen, ycen, status)

c center
      call xdstr(xcen, -1, ds1, di1)
      call xdstr(ycen, -1, ds2, di2)
      WRITE (ZWRite,99006) ds1(:di1), ds2(:di2)
      CALL XWRITE(ZWRite,10)

      ximg = float(szx)/2.0 + 0.5
      yimg = float(szy)/2.0 + 0.5
      call gheads(mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
         system = 'EQUATORIAL'
         xlab = 'RA'
         ylab = 'Dec'
         epoch = 2000.
         call gheadd(Mapid, 'CRVAL1', xsky, 0, status)
         call gheadd(Mapid, 'CRVAL2', ysky, 0, status)
         call wcsprec(epoch, xsky, ysky, equinox, xsky, ysky, status)
      else
         call wcsfrminfo(wcsid, system, proj, xlab, ylab, unit, epoch)
         call wcsimgsky(wcsid, ximg, yimg, xsky, ysky, equinox, 1,
     &                  status)
      endif

      if ( xlab.eq.'RA' ) then
         call cnv_radec(strra,strdec,xsky,ysky,dr3,dr3,2,2,status)
         WRITE (ZWRite,99007) equinox, strra
         CALL XWRITE(ZWRite,10)
         WRITE (ZWRite,99008) equinox, strdec
         CALL XWRITE(ZWRite,10)
      else
         call wcsformat(system, xsky, ysky, strra, strdec)
         WRITE (ZWRite,'(3x,a,1x,a)') 'Coord. System :', system
         CALL XWRITE(ZWRite,10)
         WRITE (ZWRite,'(3x,2a,1x,a)') xlab(1:14), ':', strra
         CALL XWRITE(ZWRite,10)
         WRITE (ZWRite,'(3x,2a,1x,a)') ylab(1:14), ':', strdec
         CALL XWRITE(ZWRite,10)
         WRITE (ZWRite,'(3x,a,1x,a)') 'Unit          :', unit
         CALL XWRITE(ZWRite,10)
      endif
c 
c write ra dec roll north
      if ( xlab.eq.'RA' ) then
         call gheadd(mapid, 'CROTA2', crot, 0, status)
         if ( .not.isdnull(crot) ) then
            roll = crot - 90.
            CALL CHDEC(roll,di1,di2,dr1,1)
            WRITE (ZWRite,99009) di1, di2, dr1
            CALL XWRITE(ZWRite,10)

            call gheadd(mapid, 'XIMNORTH', rnorth, 0, status)
            CALL CHDEC(rnorth,di1,di2,dr1,1)
            WRITE (ZWRite,99010) di1, di2, dr1
            CALL XWRITE(ZWRite,10)
         endif
      endif
c
      call gheadd(mapid, 'CDELT1', pixsize(1), 0, status)
      call gheadd(mapid, 'CDELT2', pixsize(2), 0, status)
      if ( xlab.eq.'RA' ) then
         pixsize(1) = pixsize(1) * 3600.
         pixsize(2) = pixsize(2) * 3600.
         ds2 = 'arcsecs'
      else
         ds2 = ' '
      endif
      if ( pixsize(1).ne.0.d0 ) then
         call xdstr(pixsize(1), -1, ds1, di1)
         WRITE (ZWRite,99014) ds1(:di1), ds2
         CALL XWRITE(ZWRite,10)
      endif
      if ( pixsize(2).ne.0.d0 ) then
         call xdstr(pixsize(2), -1, ds1, di1)
         WRITE (ZWRite,99015) ds1(:di1), ds2
         CALL XWRITE(ZWRite,10)
      endif
c size y
      call xistr(szx,ds1,di1)
      WRITE (ZWRite,99011) ds1(:di1)
      CALL XWRITE(ZWRite,10)
c size x
      call xistr(szy,ds2,di2)
      WRITE (ZWRite,99012) ds2(:di2)
      CALL XWRITE(ZWRite,10)
c size (sky units)
      if ( xlab.eq.'RA' ) then
c        pixsize in arcsec... need arcmin for size1/2
         size1 = abs(pixsize(1)/60.)*FLOAT(szx)
         size2 = abs(pixsize(2)/60.)*FLOAT(szy)
         sq_arcmin = size1*size2
         sq_deg = (size1*size2/3600.)
         dd1 = size1
         call xdstr(dd1,6,ds1,di1)
         dd1 = size2
         call xdstr(dd1,6,ds2,di2)
         dd1 = sq_arcmin
         call xdstr(dd1,7,csqmin,isqmin)
         dd1 = sq_deg
         call xdstr(dd1,7,csqdeg,isqdeg)
         write(ZWRite,99204) ds1(:di1), ds2(:di2),
     &                       csqmin(:isqmin),
     &                       csqdeg(:isqdeg)
         CALL XWRITE(ZWRite,10)
      endif
c zoom
      call xdstr(zmx,5,ds1,di1)
      call xdstr(zmy,5,ds2,di2)
      if ( ds1.eq.ds2 ) then
         WRITE (ZWRite,99013) ds1(1:di1)
      else
         WRITE (ZWRite,99500) ds1(1:di1), ds2(1:di2)
      endif
      CALL XWRITE(ZWRite,10)
c
c write min and max 
c and image equinox
      call gheadd(mapid, 'DATAMIN', dd1, 0, status)
      call xdstr(dd1, -1, ds1, di1)
      WRITE (ZWRite,99016) ds1(:di1)
      CALL XWRITE(ZWRite,10)
c
      call gheadd(mapid, 'DATAMAX', dd1, 0, status)
      call xdstr(dd1, -1, ds1, di1)
      WRITE (ZWRite,99017) ds1(:di1)
      CALL XWRITE(ZWRite,10)
c
      imgequ = int(epoch)
      if ( imgequ.gt.0 ) then
         WRITE (ZWRite,99020) imgequ
         CALL XWRITE(ZWRite,10)
      endif
      status = 0

      RETURN
99001 FORMAT (1x,'  Instrument    : ',a,1x,a)
99002 FORMAT (1x,'  Field Title   : ',a)
99003 FORMAT (1x,'  Start Time    : ',a)
99004 FORMAT (1x,'  End Time      : ',a)
99005 FORMAT (1x,'  Exposure Time : ',a,'  seconds')
99006 FORMAT (1x,'  Image Center  : ',a,1x,a)
99007 FORMAT (1x,'  Ra  (',i4,')    :  ', a)
99008 FORMAT (1x,'  Dec (',i4,')    : ', a)
99009 FORMAT (1x,'  Roll Angle    :',i5,'  d  ',i3,'  m  ',f6.2,'  s  ')
99010 FORMAT (1x,'  North Angle   :',i5,'  d  ',i3,'  m  ',f6.2,'  s  ')
99011 FORMAT (1x,'  Size (x)      : ',a,' pixels')
99012 FORMAT (1x,'  Size (y)      : ',a,' pixels')
c99201 FORMAT (' ',f8.1,' x ',f8.1)
c99202 FORMAT (' ',f12.2)
c99203 FORMAT (' ',f12.4)
99204 FORMAT (1x,'  Image FOV     : ',a,' x ',a,' arcmin (',a,
     &                              ' sq min, ',a,' sq deg)')
99013 FORMAT (1x,'  Rebin Factor  : ', a)
99500 FORMAT (1x,'  Rebin Factor  : ', a,' x ',a)
99014 FORMAT (1x,'  Pixsize (x)   : ',a,1x,a)
99015 FORMAT (1x,'  Pixsize (y)   : ',a,1x,a)
99016 FORMAT (1x,'  Image Min     : ',a)
99017 FORMAT (1x,'  Image Max     : ',a)
99020 FORMAT (1x,'  Image Equinox : ',i5)
99070 FORMAT (1x,'  Ref coord (x) : ', a)
99080 FORMAT (1x,'  Ref coord (y) : ', a)
c 99003 FORMAT (1x,'  Start Time  : ',i4,'/',i3,2x,i2,':',i2,':',i2)
c 99004 FORMAT (1x,'  End Time    : ',i4,'/',i3,2x,i2,':',i2,':',i2)
      END
