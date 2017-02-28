      SUBROUTINE HELP(Cmdid,Version,Status)
      IMPLICIT NONE
c
c interfaces help commands with the xanadu help facility
c
c  I  cmdid    (i) Command id
c  I  version  (s) XIMAGE version identifier
c  O  status   (i) Error flag (0=OK)
c
      character(12) Version
      INTEGER*4 Cmdid, Status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/sitedef.inc'
c
c  Local variables
c 
      INTEGER*4 argc, LENACT
      CHARACTER*(MAX_FILELEN) help_file
      character(80) xcommand, tmpstr, tmpdir

      tmpstr = ' '
      xcommand = ' '
c
c Retrieve argument as command name
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.eq.1 ) then
c        Maximum length is 71 to account for 'commands '
         call nextcarg(cmdid,tmpstr, 71, status)
         if ( status.ne.0 ) return
         write(xcommand,'(2a)') 'commands ', tmpstr(:LENACT(tmpstr))
      endif
 
      help_file = 'ximage.hlp'
      tmpdir = ' '
      call PTEND(LHEAHELP,tmpdir,help_file)
      call xwrite(' Reading help file:', 15)
      call xwrite(help_file, 15)
c
c call the routine which read the help file
c
      CALL F_IHF(help_file,xcommand)
 
      RETURN
      END
