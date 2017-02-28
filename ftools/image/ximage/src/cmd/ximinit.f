      subroutine ximinit(standalone)
      IMPLICIT NONE
c
c  I  standalone  (l)  Whether started alone or with script

      logical standalone
c
c       Image accumulation and analysis environment
c
      include '../include/maxvals.inc'
      include '../include/ximage.inc'
      include '../include/colordef.inc'
      include '../include/io.inc'
      include '../include/sitedef.inc'
      include '../include/dynmem.inc'

c  XIMAGE startup

c  status = status flag

      integer*4 status
      integer tchat, lchat
      logical readonly, global
      real*4 RNULL

      readonly = .TRUE.
      global = .TRUE.
c
c  Initializations
c
      imgcnt = 0

      call txinit(status) 
      call initmaps 
      call inithdrs 
      call initmdb
      call initstat
      call initwcs
c
c    Viewports
c
      call set_vpvals(vpset,-1)
      vpnum = 0
      vpconnum = 0
      vpfile = ' '
      vpframe = .FALSE.

      numload = 0
      uptitle = ' '
      lotitle = ' '
      filename = ' '
c
c  Read in startup info
c
      call xim_startup(standalone, prname, version, status)
      if ( status.ne.0 ) then
         call XWRITE(' Error in the Startup ',5)
         call leave
      endif
c
c  Get defaults
c
      call getdefault(status)
      if ( status.ne.0 ) then
         call XWRITE(' Error getting default settings', 5)
      endif
c
c  Set tchat and lchat variables in Tcl to current values
c
      call xgtcht(tchat, lchat)
      call tclvari('tchat',tchat,readonly,global,status)
      call tclvari('lchat',lchat,readonly,global,status)
c
c  Set up null variable for use in Tcl
c
      call tclvarr('NULL',RNULL(),readonly,global,status)

      return
      end
