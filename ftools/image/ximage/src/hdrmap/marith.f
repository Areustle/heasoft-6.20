      subroutine marith(Cmdid,Mapid,Status)
      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
c
c  Perform arithmetic operations on image maps
c  
c  I  cmdid        (i) Command id
c  I  mapid        (s) Current map id
c  O  status       (i) Error flag (0=OK)
c
      integer Cmdid, Status
      character*(*) Mapid
c
c  Local variables
      integer argc, mode
      character(40) arg1, arg2, crotstr, rastr, decstr
      integer szx, szy, size, equinox, radec_equinox

      character*(MAX_IDSTR) outid, hdrid
      integer MAX_EXLEN
      parameter (MAX_EXLEN=100)
      character*(MAX_EXLEN) expr
      
      expr = ' '
      outid = Mapid
      hdrid = ' '
      crotstr = ' '
      rastr = ' '
      decstr = ' '
      szx = -1
      szy = -1
      size = -1
      radec_equinox = -10

      Status = 0
      CALL GPARS(Cmdid,'MAPOUT',outid,Status)
      CALL GPARS(Cmdid,'HDRID',hdrid,Status)
      CALL GPARS(Cmdid,'EXPR',expr,Status)
      CALL GPARS(Cmdid,'CROTA',crotstr,Status)
      CALL GPARS(Cmdid,'RA',rastr,Status)
      CALL GPARS(Cmdid,'DEC',decstr,Status)
      CALL GPARI(Cmdid,'EQUINOX',radec_equinox,Status)
      CALL GPARI(Cmdid,'SZX',szx,Status)
      CALL GPARI(Cmdid,'SZY',szy,Status)
      CALL GPARI(Cmdid,'SIZE',size,Status)
      if ( Status.ne.0 ) return

      if ( size.gt.0 ) then
         szx = size
         szy = size
      endif

c Get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)

      if ( radec_equinox.eq.-10 ) radec_equinox = equinox
c
c If expr not specified in parameter, assume arguments as expression
c
      if ( expr.eq.' ' ) then
         call numcarg(cmdid,argc,status)
         if ( argc.eq.0 ) then
            call xwrite(" No expression specified", 10)
            status = -1
            return
         endif
c
c  Retrieve arguments as expression
c
         call catcarg(cmdid,expr, MAX_EXLEN, Status)
      endif

      call mapexpr(expr, arg1, arg2, mode, Status)
      if ( status.ne.0 ) return
      call xwrite(expr, 15)
      call maparith(arg1, arg2, outid, hdrid, crotstr, rastr, decstr,
     &              radec_equinox, szx, szy, mode, status)

      return
      end
