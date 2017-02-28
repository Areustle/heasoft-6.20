      subroutine chatter(cmdid,status)
      implicit none
c
c  Set chatter level
c
c  I  cmdid   I  Command id number
c  O  status  I  Error flag (0=OK)
c
      integer cmdid, status

      include '../include/io.inc'
c
c  Local variables
c
      integer argc, oterm, olog, tchat, lchat
      character(20) termstr, logstr
      real*8 dd
      logical readonly, global

      termstr = ' '
      logstr = ' '
      status = 0

      call xgtcht(oterm, olog)
      call numcarg(cmdid,argc,status)
      if ( argc.eq.1 ) then
          call nextcarg(cmdid,termstr, 20, status)
      elseif ( argc.eq.2 ) then
          call nextcarg(cmdid,termstr, 20, status)
          call nextcarg(cmdid,logstr, 20, status)
      else
         call xwrite(" Usage: chatter <terminal level> <log level>", 5)
         status = -1
      endif
      if ( status.ne.0 ) return

      tchat = -1
      lchat = -1
      if ( termstr.eq.'?' ) then
         write(ZWRite,'(a,i4)') ' Terminal chat level: ', oterm
         call xwrite(ZWRite, 10)
      else
         call strnum(termstr, -4, dd, status)
         if ( status.ne.0 ) return
         tchat = int(dd)
      endif
      if ( logstr.eq.'?' ) then
         write(ZWRite,'(a,i4)') ' Log file chat level: ', olog
         call xwrite(ZWRite, 10)
      elseif ( logstr.ne.' ' ) then
         call strnum(logstr, -4, dd, status)
         if ( status.ne.0 ) return
         lchat = int(dd)
      endif

      call xchaty(tchat, lchat)
c
c  Assign tchat and lchat variables with current levels
c
      call xgtcht(tchat, lchat)
      readonly = .TRUE.
      global = .TRUE.
      call tclvari('tchat', tchat, readonly, global, status)
      call tclvari('lchat', lchat, readonly, global, status)

      return
      end
