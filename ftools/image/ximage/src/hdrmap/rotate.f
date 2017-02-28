      subroutine rotate(Cmdid, Map, Szx, Szy, Mapid, Status)
      implicit none
c
c  Rotates image
c
c  adapted to run from ximage by p.g. march 1990
c  adapted to dynamic memory and new header by m.j. feb 1999
c
c  I  Cmdid   (i)  Command id
c  I  Map     (r)  Image map
c  I  Szx/y   (i)  Size of image map
c  I  Mapid   (s)  Map id string
c  O  Status  (i)  Error flag (0 = OK)
c
      integer*4 Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      character(20) string_angle
      character(1) maptype
      integer*4 argc, p_work, p_row, lenact
      logical isloaded, isdnull
      character(100) ds
      character(80) mapcodes
      REAL*8 outrol, roll1, datamin, datamax, dd
      REAL*4 dar1(3), dar2(3)
c
      string_angle = ' '
c
      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      CALL GPARS(Cmdid,'ROLL_ANGLE',string_angle,Status)
      if ( status.ne.0 ) return

      IF ( .not.isloaded(mapid) ) THEN
         call XWRITE(' Image not loaded', 5)
         Status = -1
         return
      ENDIF
c
c  Get map details
c
      call gheads(mapid,'MAPTYPE',maptype,0,status)
c
      IF ( string_angle.NE.' ' ) THEN
         ds = ' '
         call cnv_radec(ds, string_angle, dd, outrol, dar1, dar2, 1, 
     &                  0, status)
      ELSE
         outrol = -90.
         CALL XWRITE(' Rotating image to align with north',10)
         WRITE (ZWRite,*) ' Default roll angle = ', outrol
         CALL XWRITE(ZWRite,15)
      ENDIF
c
c  Get image information
c
      call gheadd(mapid, 'CROTA2', roll1, 0, status)
      if ( isdnull(roll1) ) then
         call xwrite(' Image has no CROTA2, use remap/rotangle', 10)
         status = -1
         return
      endif
      roll1 = roll1 - 90.

      write(ZWRite,*) ' Image roll: ',roll1, ' New roll: ', outrol
      call XWRITE(ZWRite,20)
c
c  updating image header
c  default rotation is with roll -90 and north -270
c
c     call gheadd(mapid,'XIMROLL',outrol,1,status)
      dd = outrol + 90.
      call gheadd(mapid,'CROTA2',dd,1,status)
      dd = outrol - 180.
      call gheadd(mapid,'XIMNORTH',dd,1,status)

c
c  Allocate temporary buffers
c
      call ralloc(1, szx, szy, p_work, status)
      if ( status.ne.0 ) return
      call ralloc(1, szx, 1, p_row, status)
      if ( status.ne.0 ) return

      call rotatework(roll1, outrol, Map, memr(p_work), memr(p_row), 
     &                Szx, Szy, maptype, datamin, datamax)
     
      call ralloc(0, szx, 1, p_row, status)
      call ralloc(0, szx, szy, p_work, status)

      call gheadd(mapid, 'DATAMIN', datamin, 1, status)
      call gheadd(mapid, 'DATAMAX', datamax, 1, status)
c
c  Append operation to MAPCODES
c
      call gheads(mapid, 'MAPCODES', mapcodes, 0, status)
      write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'Ro'
      call gheads(mapid, 'MAPCODES', ds, 1, status)
      call expiremap(mapid, status)

      return
      end
