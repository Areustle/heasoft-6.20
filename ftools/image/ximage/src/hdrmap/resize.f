      SUBROUTINE RESIZE(Cmdid,Mapid,Status)
      IMPLICIT NONE
c
c  Resize images, i.e. change image pixel size
c
c  I  Cmdid   (i)  Command id
c  I  Mapid   (s)  Map id string
c  O  Status  (i)  Error return
c
      INTEGER*4 Cmdid, Status
      CHARACTER*(*) Mapid

      INCLUDE '../include/dynmem.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 nch , LENACT , ier
      character(30) instring

      INTEGER*4 argc, i
      LOGICAL isloaded
      REAL*4 pixsiz, newpix, pixratio
      REAL*8 pixsize(2), crpix(2), datamin, datamax
      character(80) ctype
      INTEGER*4 p_work, p_row, newszx, newszy
      INTEGER*4 mapptr, szx, szy
      character(1) maptype
      character(100) ds
      character(80) mapcodes
c
      newpix = -99.
c
      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARR(Cmdid,'PIXEL_SIZE',newpix,Status)
      if ( status.ne.0 ) return

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 5)
         Status = -1
         return
      endif
c
c  Get map details
c
      call gheadi(mapid,'MAPPTR',mapptr,0,status)
      call gheadi(mapid,'SZX',szx,0,status)
      call gheadi(mapid,'SZY',szy,0,status)
      call gheads(mapid,'MAPTYPE',maptype,0,status)
c
      IF ( newpix.LT.0. ) THEN
         CALL XCREAD(' Pixel size : ',instring,ier)
         IF ( ier.EQ.0 ) THEN
            nch = LENACT(instring)
            IF ( nch.NE.0 ) READ (instring(1:nch),*) newpix
         ENDIF
      ENDIF
      IF ( newpix.LE.0 ) THEN
         CALL XWRITE(' Error: pixel size not given or < 0.',10)
         Status = 1.
         RETURN
      ENDIF
c
c Original pixel size from header
      call gheadd(mapid, 'CDELT1', pixsize(1), 0, status)
      call gheadd(mapid, 'CDELT2', pixsize(2), 0, status)
      call gheads(mapid, 'CTYPE1', ctype, 0, status)
      i = LENACT(ctype)
      if ( ctype(i-2:i).eq.'TAN' ) then
         call XWRITE(' Assuming entered pixel in arcsecs', 10)
         pixsiz = abs(pixsize(1)*3600.)
      else
         call XWRITE(' Assuming entered pixel in units of CDELT', 10)
         pixsiz = abs(pixsize(1))
      endif
      IF ( pixsiz.EQ.0. ) pixsiz = 1.
      write(ZWRite,*) ' Current size: ', pixsiz
      call XWRITE(ZWRite,20)
      IF ( newpix.LT.pixsiz .OR. newpix.GT.2*pixsiz )
     &     THEN
         WRITE (ZWRite,
     &'('' Error : pixel size must be between '',                       
     &    f5.2,'' and '',f5.2)') pixsiz , 2.*pixsiz
         CALL XWRITE(ZWRite,10)
         Status = 1
         RETURN
      ENDIF
c
      pixratio = newpix/pixsiz
c
      call ralloc(1, Szx, Szy, p_work, status)
      if ( status.ne.0 ) return
      call ralloc(1, Szx, 1, p_row, status)
      if ( status.ne.0 ) return

      call resizework(pixratio,memr(mapptr),memr(p_work),memr(p_row),
     &                Szx, Szy, maptype, datamin, datamax)

      call ralloc(0, Szx, 1, p_row, status)
      call pmapfree(mapptr, status)

      newszx = MAX(2,int(float(Szx)/pixratio/2.)*2)
      newszy = MAX(2,int(float(Szy)/pixratio/2.)*2)

      call gheadi(mapid,'MAPPTR',-1,1,status)
      call mapalloc(newszx, newszy, mapid, mapptr, status)
      if ( status.ne.0 ) then
         call ralloc(0, Szx, Szy, p_work, status)
         return
      endif

      call cprrmap(memr(p_work),Szx,Szy,1,newszx,1,newszy,
     &             memr(mapptr),newszx,newszy,1,newszx,1,newszy,status)
      call ralloc(0, Szx, Szy, p_work, status)

      pixsize(1) = pixsize(1)*pixratio
      pixsize(2) = pixsize(2)*pixratio
      crpix(1) = float(Szx/2) + 0.5
      crpix(2) = float(Szy/2) + 0.5

      call gheadd(mapid, 'CRPIX1', crpix(1), 1, status)
      call gheadd(mapid, 'CRPIX2', crpix(2), 1, status)
      call gheadd(mapid, 'CDELT1', pixsize(1), 1, status)
      call gheadd(mapid, 'CDELT2', pixsize(2), 1, status)

      call gheadd(mapid, 'DATAMIN', datamin, 1, status)
      call gheadd(mapid, 'DATAMAX', datamax, 1, status)
c
c  Append operation to MAPCODES
c
      call gheads(mapid, 'MAPCODES', mapcodes, 0, status)
      write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'Rz'
      call gheads(mapid, 'MAPCODES', ds, 1, status)
      call expiremap(mapid, status)

      RETURN
      END
