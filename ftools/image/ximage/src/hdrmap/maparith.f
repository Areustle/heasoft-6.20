      subroutine maparith(arg1, arg2, outid, hdrid, crotstr, rastr,
     &                    decstr, radec_equinox, szx, szy, mode, status)
      implicit none
c
c  Perform arithmetic operations on image maps
c  (Algorithm assumes equal pixel sizes)
c
c  I  arg1          (s)  First argument, map or constant
c  I  arg2          (s)  Second argument, map or constant
c  I  outid         (s)  Id of output map 
c  I  hdrid         (s)  Id of header to use for output
c  I  crotstr       (s)  User input rotation
c  I  rastr         (s)  User input RA of center
c  I  decstr        (s)  User input Dec of center
c  I  radec_equinox (i)  Equinox of RA/Dec
c  I  szx           (i)  User input size in x of output map
c  I  szy           (i)  User input size in y of output map
c  I  mode          (i)  1=add, 2=sub, 3=mult, 4=div, 5=int, 6=float
c  O  status        (i)  Error flag (0=OK)
c
      character*(*) arg1, arg2, outid, crotstr, rastr, decstr
      integer radec_equinox, szx, szy, mode, status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      INTEGER*4 szx1, szy1, szx2, szy2
      INTEGER*4 ptr1, ptr2, optr, exptr1, exptr2, oexptr
      INTEGER*4 tmpptr1, tmpptr2, tmpexptr1, tmpexptr2
      character(1) type1, type2, otype
      LOGICAL isloaded, iseqmapid, isint, isfloat
      REAL*4 mapmin, mapmax, exmapmin, exmapmax, exdum
      REAL*8 datamin, datamax, dd
      REAL*8 expotot, expo1, expo2, dtc1, dtc2
      REAL*4 const1, const2
      LOGICAL ismap1, ismap2
      INTEGER*4 szxo, szyo, LENACT
      CHARACTER*(MAX_IDSTR) hdrid, outhdr, exmap1, exmap2, exoutid
      integer*4 exszx, exszy, di
      real*8 ra1, dec1, ra2, dec2, rao, deco, zmx, zmy
      real*8 rnorth, roll, crot1, crot2, croto, cdelt(2), drpix(2)
      integer*4 equimg1, equimg2, equimgo
      real*4 dr3(3)
      character(100) ds
      character(80) mapcodes
      real*8 DNULL

      real*8 ANGTOLER
      parameter (ANGTOLER=1.e-30)
c
c  Determine type of args and whether they are maps or constants
c  type1, type2  (I=integer R=real)
c
      Status = 0

      ismap1 = .FALSE.
      if ( mode.eq.5 .or. mode.eq.6 ) then
c        INT() or FLOAT() -> No arg1, so no type1
         type1 = ' '
         const1 = 0
      elseif ( isint(arg1) ) then
         type1 = 'I'
         call strnum(arg1, -4, dd, status)
         const1 = dd
      elseif ( isfloat(arg1) ) then
         type1 = 'R'
         call strnum(arg1, 4, dd, status)
         const1 = dd
      elseif ( isloaded(arg1) ) then
         call gheads(arg1, 'MAPTYPE', type1, 0, status)
         call upc(type1)
         call gheadi(arg1, 'MAPPTR', ptr1, 0, status)
         ismap1 = .TRUE.
      else
         write(ZWRite,'(2a)') ' Map not loaded: ', 
     &                        arg1(:LENACT(arg1))
         call xwrite(ZWRite, 10)
         Status = -1
         return
      endif

      ismap2 = .FALSE.
      type2 = ' '
      if ( isint(arg2) ) then
         type2 = 'I'
         call strnum(arg2, -4, dd, status)
         const2 = dd
      elseif ( isfloat(arg2) ) then
         type2 = 'R'
         call strnum(arg2, 4, dd, status)
         const2 = dd
      elseif ( isloaded(arg2) ) then
         call gheads(arg2, 'MAPTYPE', type2, 0, status)
         call upc(type2)
         call gheadi(arg2, 'MAPPTR', ptr2, 0, status)
         ismap2 = .TRUE.
      else
         write(ZWRite,'(2a)') ' Map not loaded: ', 
     &                        arg2(:LENACT(arg2))
         call xwrite(ZWRite, 10)
         Status = -1
         return
      endif

      if ( .not. ( ismap1 .or. ismap2 ) ) then
         call xwrite(' Must use at least one map in operation', 10)
         status = -1
         return
      endif
c
c  Based on user-input coordinates and header contents,
c  determine final coordinates of output map
c
c  In order of descending precedence, set output coordinates
c  (i.e. higher precedence occurs later, overriding the 
c        value previously set)
c
c  Order of precedence:
c   1. User-entered coords 
c   2. First map coords  
c   3. Second map coords
c
      szxo = -1
      szyo = -1
      if ( ismap2 ) then
c
c read the reference center image ra and dec and the rotation
c and calculate the degree for map 2.
c
         call get_skyhea(arg2,ra2,dec2,rnorth,roll,cdelt,equimg2,ds,
     &                   status)
         call get_refram(arg2,szx2,szy2,zmx,zmy,drpix(1),drpix(2),
     &                   status)
         crot1 = roll + 90.
         croto = crot1
         rao = ra2
         deco = dec2
         equimgo = equimg2
         szxo = szx2
         szyo = szy2
      endif

      if ( ismap1 ) then
c
c read the reference center image ra and dec and the rotation
c and calculate the degree for map 1.
c
         call get_skyhea(arg1,ra1,dec1,rnorth,roll,cdelt,equimg1,ds,
     &                   status)
         call get_refram(arg1,szx1,szy1,zmx,zmy,drpix(1),drpix(2),
     &                   status)
         crot2 = roll + 90.
         croto = crot2
         rao = ra1
         deco = dec1
         equimgo = equimg1
         szxo = MAX(szx1,szxo)
         szyo = MAX(szy1,szyo)
      endif
c
c  Parse user-entered coordinates
c
c   Inherit coordinates of hdrid
c
      if ( hdrid.ne.' ' ) then
         call get_skyhea(hdrid,rao,deco,rnorth,roll,cdelt,equimgo,ds,
     &                   status)
         call get_refram(hdrid,di,di,zmx,zmy,drpix(1),drpix(2),
     &                   status)
         croto = roll + 90.
      endif
c
      if ( crotstr.ne.' ' ) then
         ds = ' '
         call cnv_radec(ds, crotstr, dd, croto, dr3, dr3, 1, 0, 
     &                  status)
         if ( status.ne.0 ) then
            call xwrite(' Failed to parse rotation angle', 10)
            return
         endif
      endif
c
      if ( rastr.ne.' ' .and. decstr.ne.' ' ) then
         call cnv_radec(rastr, decstr, rao, deco, dr3, dr3, 1, 0, 
     &                  status)
         if ( status.ne.0 ) then
            call xwrite(' Failed to parse RA/Dec', 10)
            return
         endif
         equimgo = radec_equinox
      endif
      if ( szx.gt.0 ) szxo = szx
      if ( szy.gt.0 ) szyo = szy

      call ximprec(ra1,dec1,equimg1,equimgo)
      call ximprec(ra2,dec2,equimg2,equimgo)
c
c  Retrieve exposure/deadtime information
c
      exmap1 = ' '
      exmap2 = ' '
      exptr1 = -1
      exptr2 = -1
      expo1 = 0.
      expo2 = 0.
      dtc1 = 1.
      dtc2 = 1.
      if ( ismap1 ) then
         call gheadd(arg1, 'EXPOSURE', expo1, 0, status)
         call gheadd(arg1, 'DTIME', dtc1, 0, status)
         call gheads(arg1, 'EXMAPID', exmap1, 0, status)
         if ( exmap1.ne.' ' ) then
            call gheadi(exmap1, 'MAPPTR', exptr1, 0, status)
            call gheadi(exmap1, 'SZX', exszx, 0, status)
            call gheadi(exmap1, 'SZY', exszy, 0, status)
            if ( exszx.ne.szx1 .or. exszy.ne.szy1 ) then
               call xwrite(
     &           ' First map different size from exposure map', 5)
               call xwrite(' Ignoring exposure map...', 5)
               exptr1 = -1
               exmap1 = ' '
            endif
         endif
      endif
      if ( ismap2 ) then
         call gheadd(arg2, 'EXPOSURE', expo2, 0, status)
         call gheadd(arg2, 'DTIME', dtc2, 0, status)
         call gheads(arg2, 'EXMAPID', exmap2, 0, status)
         if ( exmap2.ne.' ' ) then
            call gheadi(exmap2, 'MAPPTR', exptr2, 0, status)
            call gheadi(exmap2, 'SZX', exszx, 0, status)
            call gheadi(exmap2, 'SZY', exszy, 0, status)
            if ( exszx.ne.szx2 .or. exszy.ne.szy2 ) then
               call xwrite(
     &           ' Second map different size from exposure map', 5)
               call xwrite(' Ignoring exposure map...', 5)
               exptr2 = -1
               exmap2 = ' '
            endif
         endif
      endif
      if ( exmap1.ne.' ' .and. exmap2.ne.' ' ) then
         if ( mode.ne.1 ) then
            call xwrite(
     &         ' Operation not addition, exposure maps ignored', 15)
            exmap1 = ' '
            exmap2 = ' '
            exptr1 = -1
            exptr2 = -1
         endif
      elseif ( exmap1.ne.' ' .or. exmap2.ne.' ' ) then
         call xwrite(' Only one image has associated exposure map', 15)
         call xwrite(' Ignoring exposure map in operation...', 15)
         exmap1 = ' '
         exmap2 = ' '
         exptr1 = -1
         exptr2 = -1
      endif
c
c  Initialize temporary transformation map pointers
c
      tmpptr1 = -1
      tmpptr2 = -1
      tmpexptr1 = -1
      tmpexptr2 = -1
c
c  Allocate temporary space and transform images
c
      if ( ismap1 ) then
         if ( abs(rao-ra1).le.ANGTOLER .and. abs(deco-dec1).le.ANGTOLER
     &        .and. abs(croto-crot1).le.ANGTOLER .and. szxo.eq.szx1
     &        .and. szyo.eq.szy1 ) then
            write(ZWRite,'(3a)') ' No need to reseed ',
     &                           arg1(:LENACT(arg1)), ' image'
            CALL XWRITE(ZWRite,15)
         else
            call ralloc(1, szxo, szyo, tmpptr1, status)
            if ( status.ne.0 ) then
               call xwrite(' Failed to allocate temporary map', 10)
               return
            endif
            write(ZWRite,'(3a)') ' Processing ', arg1(:LENACT(arg1)), 
     &                           ' image'
            CALL XWRITE(ZWRite,10)
            if ( exmap1.ne.' ' ) then
               call ralloc(1, szxo, szyo, tmpexptr1, status)
               if ( status.ne.0 ) then
                  call xwrite(' Failed to allocate temporary exmap', 10)
                  call ralloc(0, szxo, szyo, tmpptr1, status)
                  return
               endif
               write(ZWRite,'(3a)') ' Processing ', arg1(:LENACT(arg1)), 
     &                              ' exposure image'
               CALL XWRITE(ZWRite,10)
               call maptrfm(ra1,dec1,crot1,rao,deco,croto,cdelt,
     &                      memr(ptr1),szx1,szy1,memr(exptr1),szx1,szy1,
     &                      type1,memr(tmpptr1),szxo,szyo,
     &                      memr(tmpexptr1),szxo,szyo)
            else
               exdum = 0.
               call maptrfm(ra1,dec1,crot1,rao,deco,croto,cdelt,
     &                      memr(ptr1),szx1,szy1,exdum,1,1,type1,
     &                      memr(tmpptr1),szxo,szyo,exdum,1,1)
            endif
         endif
      endif

      if ( ismap2 ) then
         if ( abs(rao-ra2).le.ANGTOLER .and. abs(deco-dec2).le.ANGTOLER
     &        .and. abs(croto-crot2).le.ANGTOLER .and. szxo.eq.szx2
     &        .and. szyo.eq.szy2 ) then
            write(ZWRite,'(3a)') ' No need to reseed ',
     &                           arg2(:LENACT(arg2)), ' image'
            CALL XWRITE(ZWRite,15)
         else
c
c           Transform map
c
            call ralloc(1, szxo, szyo, tmpptr2, status)
            if ( status.ne.0 ) then
               call xwrite(' Failed to allocate temporary map', 10)
               return
            endif
            write(ZWRite,'(3a)') ' Processing ', arg2(:LENACT(arg2)), 
     &                           ' image'
            CALL XWRITE(ZWRite,10)
            if ( exmap2.ne.' ' ) then
               call ralloc(1, szxo, szyo, tmpexptr2, status)
               if ( status.ne.0 ) then
                  call xwrite(' Failed to allocate temporary exmap', 10)
                  call ralloc(0, szxo, szyo, tmpptr2, status)
                  return
               endif
               write(ZWRite,'(3a)') ' Processing ', arg2(:LENACT(arg2)), 
     &                              ' exposure image'
               CALL XWRITE(ZWRite,10)
               call maptrfm(ra2,dec2,crot2,rao,deco,croto,cdelt,
     &                      memr(ptr2),szx2,szy2,memr(exptr2),szx2,szy2,
     &                      type2,memr(tmpptr2),szxo,szyo,
     &                      memr(tmpexptr2),szxo,szyo)
            else
               exdum = 0.
               call maptrfm(ra2,dec2,crot2,rao,deco,croto,cdelt,
     &                      memr(ptr2),szx2,szy2,exdum,1,1,type2,
     &                      memr(tmpptr2),szxo,szyo,exdum,1,1)
            endif
         endif
      endif
c
c  Determine which header the output map is to inherit
c
      outhdr = hdrid
      if ( ismap1 ) then
         outhdr = arg1
      elseif ( ismap2 ) then
         outhdr = arg2
      endif
c
c  Pointer management
c
      if ( ismap1 .and. iseqmapid(arg1,outid) ) then
c
c        Overwriting first map
c
         if ( tmpptr1.ne.-1 ) then
c           Free ptr1, as it will no longer be needed
            call ralloc(0, szx1, szy1, ptr1, status)
         else
c           Set as tmpptr1 so ptr1 will be freed later
            tmpptr1 = ptr1
         endif
         if ( exmap1.ne.' ' ) then
            if ( tmpexptr1.ne.-1 ) then
c              Free exptr1, as it will no longer be needed
               call ralloc(0, szx1, szy1, exptr1, status)
            else
c              Set as tmpexptr1 so exptr1 will be freed later
               tmpexptr1 = exptr1
            endif
         endif
      elseif ( ismap2 .and. iseqmapid(arg2,outid) ) then
c
c        Overwriting second map
c
         if ( tmpptr2.ne.-1 ) then
c           Free ptr2, as it will no longer be needed
            call ralloc(0, szx2, szy2, ptr2, status)
         else
c           Set as tmpptr2 so ptr2 will be freed later
            tmpptr2 = ptr2
         endif
         if ( exmap2.ne.' ' ) then
            if ( tmpexptr2.ne.-1 ) then
c              Free exptr2, as it will no longer be needed
               call ralloc(0, szx2, szy2, exptr2, status)
            else
c              Set as tmpexptr2 so exptr2 will be freed later
               tmpexptr2 = exptr2
            endif
         endif
      else
c
c        Writing into new map, not associated with operation
c        Free what is there currently
c
         call mapfree(outid, status)

      endif
c
c  Allocate necessary space for output map
c
      optr = -1
      oexptr = -1
      call cphead(outhdr, outid)
      call gheadi(outid, 'MAPPTR', -1, 1, status)
      call gheads(outid, 'WCSID', ' ', 1, status)
      call mapalloc(szxo, szyo, outid, optr, status)
      if ( exmap1.ne.' ' .and. exmap2.ne.' ' ) then
         call getexmap(outid, exoutid, status)
         call gheadi(exoutid, 'MAPPTR', -1, 1, status)
         call gheads(exoutid, 'WCSID', ' ', 1, status)
         call mapalloc(szxo, szyo, exoutid, oexptr, status)
      else
         exoutid = ' '
      endif
      if ( status.ne.0 ) return
c
c  If temporary, transformed images have been allocated,
c  use them instead of originals
c
      if ( tmpptr1.ne.-1 ) ptr1 = tmpptr1
      if ( tmpptr2.ne.-1 ) ptr2 = tmpptr2
      if ( tmpexptr1.ne.-1 ) exptr1 = tmpexptr1
      if ( tmpexptr2.ne.-1 ) exptr2 = tmpexptr2

      if ( ismap1 .and. ismap2 ) then
         call marithwork(memr(ptr1), szxo, szyo, memr(ptr2), szxo,
     &                   szyo, memr(optr), szxo, szyo, mode, mapmin, 
     &                   mapmax, status)
         if ( exoutid.ne.' ' ) then
            call marithwork(memr(exptr1), szxo, szyo, memr(exptr2),
     &                      szxo, szyo, memr(oexptr), szxo, szyo, mode,
     &                      exmapmin, exmapmax, status)
         endif
      elseif ( ismap1 ) then
         call marithwork(memr(ptr1), szxo, szyo, const2, 1, 1, 
     &                   memr(optr), szxo, szyo, mode, mapmin, mapmax,
     &                   status)
      else
         call marithwork(const1, 1, 1, memr(ptr2), szxo, szyo, 
     &                   memr(optr), szxo, szyo, mode, mapmin, mapmax,
     &                   status)
      endif
      call ralloc(0, szx1, szy1, tmpptr1, status)
      call ralloc(0, szx2, szy2, tmpptr2, status)
      call ralloc(0, szx1, szy1, tmpexptr1, status)
      call ralloc(0, szx2, szy2, tmpexptr2, status)
c
c  Set output type 
c
      call upc(type1)
      call upc(type2)
      if ( mode.eq.5 ) then
c        mode = 5 -> int() function : always integer type
         otype = 'I'
      elseif ( mode.eq.6 ) then
c        mode = 6 -> float() function : always real type
         otype = 'R'
      elseif ( type1.eq.'I' .and. type2.eq.'I' .and. mode.ne.4 ) then
c        mode = 4 -> division : always real type
         otype = 'I'
      else
         otype = 'R'
      endif
c
c  Update output map header
c
      call gheads(outid, 'MAPTYPE', otype, 1, status)
      datamin = mapmin
      datamax = mapmax
      call gheadd(outid, 'DATAMIN', datamin, 1, status)
      call gheadd(outid, 'DATAMAX', datamax, 1, status)
      call gheadd(outid, 'CROTA2', croto, 1, status)
      call gheadd(outid, 'CRVAL1', rao, 1, status)
      call gheadd(outid, 'CRVAL2', deco, 1, status)
      if ( equimgo.eq.0 ) then
         dd = DNULL()
      else
         dd = equimgo
      endif
      call gheadd(outid, 'EQUINOX', dd, 1, status)
c
c   mode = 1 -> add : update EXPOSURE, DTIME
c
      if ( mode.eq.1 ) then
         expotot = expo1+expo2
         call gheadd(outid, 'EXPOSURE', expotot, 1, status)
         if ( expotot.eq.0.d0 ) then
            dd = 1.0
         else 
            dd = (expo1*dtc1 + expo2*dtc2)/expotot
         endif
         call gheadd(outid, 'DTIME', dd, 1, status)
      endif
      call gheadd(outid, 'XIMNORTH', croto-270., 1, status)
      call gheads(outid, 'EXMAPID', exoutid, 1, status)
c
c  Update exposure map header
c
      if ( exoutid.ne.' ' ) then
         call cphead(outid, exoutid)
         call gheadi(exoutid, 'MAPPTR', oexptr, 1, status)
         call gheads(exoutid, 'MAPTYPE', 'R', 1, status)
         call gheads(exoutid, 'EXMAPID', ' ', 1, status)
         datamin = exmapmin
         datamax = exmapmax
         call gheadd(exoutid, 'DATAMIN', datamin, 1, status)
         call gheadd(exoutid, 'DATAMAX', datamax, 1, status)
      endif
c
c  Append operation to MAPCODES
c
      call gheads(outid, 'MAPCODES', mapcodes, 0, status)
      if ( mode.eq.1 ) then
         write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), '+'
      elseif ( mode.eq.2 ) then
         write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), '-'
      elseif ( mode.eq.3 ) then
         write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), '*'
      elseif ( mode.eq.4 ) then
         write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), '/'
      elseif ( mode.eq.5 ) then
         write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'I'
      elseif ( mode.eq.6 ) then
         write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'Ft'
      else
         return
      endif
      call gheads(outid, 'MAPCODES', ds, 1, status)
      call expiremap(outid, status)

      return
      end
