      subroutine moper(Cmdid,Mapid,Status)
      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
      include '../include/dynmem.inc'
c
c  Perform simple operations on image maps
c   Maps must be identical size
c  (Used by Tcl command marith)
c  
c  I  cmdid        (i) Command id
c  I  mapid        (s) Current map id
c  O  status       (i) Error flag (0=OK)
c
      integer Cmdid, Status
      character*(*) Mapid
c
c  Local variables
      integer argc, mode, reqargc, echoval
      real*4 rmode
      character(40) arg1, arg2

      character*(MAX_IDSTR) outid, hdrid, outhdr, wcsid, cpwcsid
      logical addop, subop, multop, divop, intop, floatop
      character(40) procname, procchk

      INTEGER*4 ptr1, ptr2, optr, oexptr, tmpptr, exptr1, exptr2
      character(1) type1, type2, otype
      LOGICAL isloaded, iseqmapid, isint, isfloat
      REAL*4 mapmin, mapmax, exmapmin, exmapmax
      REAL*8 datamin, datamax, expotot, expo1, expo2, dtc1, dtc2, dd
      REAL*4 const1, const2
      LOGICAL ismap1, ismap2, overwr
      INTEGER*4 szx1, szy1, szx2, szy2, szxo, szyo, exszx, exszy, LENACT
      INTEGER*4 tmpszx, tmpszy
      CHARACTER*(MAX_IDSTR) exmap1, exmap2, exoutid
      character(100) ds
      character(80) mapcodes

      outid = Mapid
      hdrid = ' '
      addop = .FALSE.
      subop = .FALSE.
      multop = .FALSE.
      divop = .FALSE.
      intop = .FALSE.
      floatop = .FALSE.
      procname = ' '

      Status = 0
      CALL GPARS(Cmdid,'MAPOUT',outid,Status)
      CALL GPARS(Cmdid,'HDRID',hdrid,Status)
      CALL GPARL(Cmdid,'ADD',addop,Status)
      CALL GPARL(Cmdid,'SUB',subop,Status)
      CALL GPARL(Cmdid,'MULT',multop,Status)
      CALL GPARL(Cmdid,'DIV',divop,Status)
      CALL GPARL(Cmdid,'INT',intop,Status)
      CALL GPARL(Cmdid,'FLOAT',floatop,Status)
      CALL GPARS(Cmdid,'PROCNAME',procname,Status)
      if ( Status.ne.0 ) return

      call numcarg(cmdid,argc,status)
      if ( argc.eq.0 ) then
         call xwrite(" No arguments specified for operation", 10)
         status = -1
         return
      endif
c
c  Retrieve arguments for operation
c
      arg1 = ' '
      call nextcarg(cmdid, arg1, 40, status)
      arg2 = ' '
      call nextcarg(cmdid, arg2, 40, status)
      status = 0
c
c  Determine operation
c
      rmode = 0.
      if ( addop ) rmode = rmode + 10**1
      if ( subop ) rmode = rmode + 10**2
      if ( multop ) rmode = rmode + 10**3
      if ( divop ) rmode = rmode + 10**4
      if ( intop ) rmode = rmode + 10**5
      if ( floatop ) rmode = rmode + 10**6
      if ( procname.ne.' ' ) rmode = rmode + 10**7
      if ( rmode.eq.0. ) then
         call xwrite(' No map operation specified', 5)
         status = -1
         return
      endif
      mode = nint(log10(rmode))
      if ( int(rmode) - 10**mode .gt. 0 ) then
         call xwrite(' Only one map operation permitted at a time', 5)
         status = -1
         return
      endif
c
c  Determine required number of arguments
c
      if ( mode.ge.1 .and. mode.le.4 ) then
         reqargc = 2
      elseif ( mode.le.6 ) then
         reqargc = 1
      else
c
c  Turn off script echo 
c
         echoval = 1
         call tclresi("set xm_echo_script", echoval, status) 
         call tclrun("set xm_echo_script 0", status) 
         write(ds, '(2a)') "info procs ", procname
         call tclress(ds, procchk, 40, status)
         if ( procchk.eq.' ' ) then
            write(ds, '(2a)') "Nonexistent procname: ", procname
            call xwrite(ds, 5)
            status = -1
            return
         endif
         write(ds, '(3a)') "llength [info args ", procname, "]"
         call tclresi(ds, reqargc, status)
         if ( reqargc.lt.1 .or. reqargc.gt.2 ) then
            write(ds, '(2a)') 
     &         " Invalid procname (must take 1 or 2 args): ", procname
            call xwrite(ds, 5)
            status = -1
            return
         endif
         if ( reqargc.eq.2 ) mode = 8
      endif
c
c  Keep track of exposure and deadtime when adding maps
c
      exmap1 = ' '
      exmap2 = ' '
      exptr1 = -1
      exptr2 = -1
      expo1 = 0.d0
      expo2 = 0.d0
      dtc1 = 1.d0
      dtc2 = 1.d0
c
c  Determine type of args and whether they are maps or constants
c  type1, type2  (I=integer R=real)
c
      Status = 0

      ismap1 = .FALSE.
      if ( reqargc.eq.1 ) then
         type1 = ' '
         const1 = 0
         arg2 = arg1
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
         call gheadi(arg1, 'SZX', szx1, 0, status)
         call gheadi(arg1, 'SZY', szy1, 0, status)
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
         szxo = szx1
         szyo = szy1
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
         call gheadi(arg2, 'SZX', szx2, 0, status)
         call gheadi(arg2, 'SZY', szy2, 0, status)
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
         szxo = szx2
         szyo = szy2
         ismap2 = .TRUE.
      else
         write(ZWRite,'(2a)') ' Map not loaded: ', 
     &                        arg2(:LENACT(arg2))
         call xwrite(ZWRite, 10)
         Status = -1
         return
      endif
c
c  Sanity checks
c
      if ( .not. ( ismap1 .or. ismap2 ) ) then
         call xwrite(' Must use at least one map in operation', 5)
         status = -1
         return
      endif

      if ( ismap1 .and. ismap2 ) then
         if ( szx1.ne.szx2 .or. szy1.ne.szy2 ) then
            call xwrite(' Map operation must be on equal sized maps', 5)
            status = -1
            return
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
c  Temporary map pointer holds map to cleanup
c
      tmpptr = -1
      overwr = .FALSE.
      if ( ismap1 .and. iseqmapid(arg1,outid) ) then
         tmpptr = ptr1
         tmpszx = szx1
         tmpszy = szy1
         overwr = .TRUE.
      elseif ( ismap2 .and. iseqmapid(arg2,outid) ) then
         tmpptr = ptr2
         tmpszx = szx2
         tmpszy = szy2
         overwr = .TRUE.
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
      call gheads(outid, 'MAPCOPY', ' ', 1, status)
      call gheads(outid, 'WCSID', wcsid, 0, status)
      call gheads(outid, 'WCSID', ' ', 1, status)
      call mapalloc(szxo, szyo, outid, optr, status)
      if ( status.ne.0 ) return
      call copywcs(wcsid, MAX_IDSTR, cpwcsid, status)
      if ( overwr ) call wcsdecref(wcsid)
      call gheads(outid, 'WCSID', cpwcsid, 1, status)

      exoutid = ' '
      if ( exmap1.ne.' ' .and. exmap2.ne.' ' ) then
         call getexmap(outid, exoutid, status)
         call cphead(outid, exoutid)
         call gheads(exoutid, 'EXMAPID', ' ', 1, status)
         call gheadi(exoutid, 'EXPMAP', 1, 1, status)
         call gheadi(exoutid, 'MAPPTR', -1, 1, status)
         call gheads(exoutid, 'MAPTYPE', 'R', 1, status)
         call gheads(exoutid, 'WCSID', wcsid, 0, status)
         call gheads(exoutid, 'WCSID', ' ', 1, status)
         call mapalloc(szxo, szyo, exoutid, oexptr, status)
         if ( status.ne.0 ) return
         call copywcs(wcsid, MAX_IDSTR, cpwcsid, status)
         if ( overwr ) call wcsdecref(wcsid)
         call gheads(exoutid, 'WCSID', cpwcsid, 1, status)
      endif

      if ( status.ne.0 ) return

      if ( ismap1 .and. ismap2 ) then
         call mopwork(memr(ptr1), szxo, szyo, memr(ptr2), szxo,
     &                szyo, memr(optr), szxo, szyo, mode, procname,
     &                mapmin, mapmax, status)
         if ( exoutid.ne.' ' ) then
            call mopwork(memr(exptr1), szxo, szyo, memr(exptr2), szxo,
     &                   szyo, memr(oexptr), szxo, szyo, mode, procname,
     &                   exmapmin, exmapmax, status)
         endif
      elseif ( ismap1 ) then
         call mopwork(memr(ptr1), szxo, szyo, const2, 1, 1, 
     &                memr(optr), szxo, szyo, mode, procname, mapmin,
     &                mapmax, status)
      else
         call mopwork(const1, 1, 1, memr(ptr2), szxo, szyo, 
     &                memr(optr), szxo, szyo, mode, procname, mapmin,
     &                mapmax, status)
      endif
      call ralloc(0, tmpszx, tmpszy, tmpptr, status)
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
c        mode = 7/8 -> proc : always real type
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
c
c  Exposure and dead time correction updated on addition
c
      if ( mode.eq.1 ) then
         expotot = expo1 + expo2
         call gheadd(outid, 'EXPOSURE', expotot, 1, status)
         if ( expotot.eq.0.d0 ) then
            dd = 1.0
         else
            dd = (expo1*dtc1 + expo2*dtc2)/expotot
         endif
         call gheadd(outid, 'DTIME', dd, 1, status)
      endif
c
c  Update exposure map keys
c
      call gheads(outid, 'EXMAPID', exoutid, 1, status)
      if ( exoutid.ne.' ' ) then
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
      elseif ( mode.eq.7 .or. mode.eq.8 ) then
         write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'Pr'
         call tclvari("xm_echo_script", echoval, .FALSE., .TRUE.,
     &                status)
      else
         return
      endif
      call gheads(outid, 'MAPCODES', ds, 1, status)
      call expiremap(outid, status)

      return
      end
