      SUBROUTINE RESCALE(Cmdid, Map, Szx, Szy, Mapid, Status)
      IMPLICIT NONE
c
c  Rescale image
c
c multiplication and addition (division and subtraction are done
c inserting number less then 1 and negative number respectivaly)
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
      include '../include/dynmem.inc'
      include '../include/io.inc'
c
c  Local variables
c
      LOGICAL mult , add, div
 
      INTEGER*4 argc, column, p_vec, vecsz, lenact
      REAL*4 factor, to_expo, min_frac
      CHARACTER*(MAX_FILELEN) filevec
      LOGICAL x_dir, y_dir, isloaded
      REAL*8 exposure, dd
      character(40) factstr
      character(1) facttype
      LOGICAL ISINT
      character(100) ds
      character(80) mapcodes
c
c defaults and inits.
c
      mult = .TRUE.
      div = .FALSE.
      add = .FALSE.
      factstr = ' '
      to_expo= 0.
      x_dir = .FALSE.
      y_dir = .FALSE.
      min_frac=-1.e30
      filevec = ' '
      column = 0
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'MULTIPLY',mult,Status)
      CALL GPARL(Cmdid,'ADD',add,Status)
      CALL GPARL(Cmdid,'DIVIDE',div,Status)
      CALL GPARS(Cmdid,'SCALING_FACTOR',factstr,Status)
      CALL GPARL(Cmdid,'X_DIRECTION',x_dir,Status)
      CALL GPARL(Cmdid,'Y_DIRECTION',y_dir,Status)
      CALL GPARR(Cmdid,'TO_EXPOSURE',to_expo,status)
      CALL GPARR(Cmdid,'MIN_FRACTION',min_frac,status)
      CALL GPARS(Cmdid,'FILE_VECTOR',filevec,status)
      CALL GPARI(Cmdid,'COLUMN',column,status)
      if ( status.ne.0 ) return

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 5)
         status = -1
         return
      endif

      IF ( add .OR. div ) mult = .FALSE.

      facttype = 'I'

      if ( to_expo.gt.0. ) then
         exposure = 0.d0
         call gheadd(mapid, 'EXPOSURE', exposure, 0, status)
         if ( status.ne.0 .or. exposure.le.0.d0 ) then
            call xwrite(' No valid exposure time, aborting rescale', 10)
            return
         endif
         factor = to_expo/exposure
         facttype = 'R'
         write(ZWRite,*) ' Scaling factor: ', factor
         call xwrite(ZWRite, 10)
      else
         if ( factstr.eq.' ' .and. filevec.eq.' ' ) then
            CALL XCREAD(' Enter Scaling Factor',factstr,status)
            IF ( status.NE.0 .OR. factstr.EQ.' ' ) THEN
               CALL XWRITE(' Scaling factor not entered',10)
               status = -1
               RETURN
            ENDIF
         endif
         if ( factstr.ne.' ' ) then
            if ( .not.ISINT(factstr) ) facttype = 'R'
            call strnum(factstr, 4, dd, status)
            if ( status.ne.0 ) return
            factor = dd
         elseif ( filevec.ne.' ' ) then
            factor = 1.0
         endif
      ENDIF
      if ( filevec.eq.' ' .and. div ) then
         if ( factor.eq.0 ) then
            call xwrite(' Cannot divide by zero', 10)
            status = -1
            return
         endif
         factor = 1./factor
         mult = .TRUE.
      endif

      if ( filevec.ne.' ' ) then
         call qustrip(filevec)
         if ( column.eq.0 ) then
            call XWRITE(' Column number not given, setting to 1...', 10)
            column = 1
         endif
         if ( .not.x_dir .and. .not.y_dir ) then
            call XWRITE(' Please select a direction with X_DIR, Y_DIR',
     &                  10)
            Status = -1
            return
         endif
         if ( add ) then
            call xwrite(' Adding not available for vector file', 10)
            Status = -1
            return
         endif
      endif

      vecsz = MAX(Szx,Szy)
      call ralloc(1, vecsz, 1, p_vec, status)
      if ( status.ne.0 ) return
      
      call rescalework(Map, Szx, Szy, Mapid, memr(p_vec), vecsz, factor,
     &                 mult, filevec, column, x_dir, y_dir, 
     &                 min_frac,status)

      call ralloc(0, vecsz, 1, p_vec, status)
c
c  If Map is int, Dividing by anything, multiplying by real,
c    or adding real makes it a real map
c
      if ( div .or. facttype.eq.'R' ) then
         call gheads(mapid, 'MAPTYPE', 'R', 1, status)
      endif
c
c  Append operation to MAPCODES
c
      call gheads(mapid, 'MAPCODES', mapcodes, 0, status)
      write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'Rs'
      call gheads(mapid, 'MAPCODES', ds, 1, status)
      call expiremap(mapid, status)

      RETURN
      END
