      subroutine closepg()
      implicit none
c
c  Close PGPLOT device if one is currently open, and
c  update XIMAGE state
c
      include '../include/colordef.inc'
c
c  Local variables
c
      logical isdisplay, istk
      integer status, iot
      character(8) opntype

      if ( isdisplay() ) then
         if ( istk() ) then
            call pgqinf('TYPE', opntype, iot)
            if ( opntype(1:iot).eq.'XTK' ) then
               call tclrun('pgtk::close', status)
            else
               call pgclos
            endif
         else
            call pgclos
         endif
         call read_coltab(CTAB_CURRENT,' ')
         call setdismap(' ', status)
      endif

      status = 0
      return
      end
