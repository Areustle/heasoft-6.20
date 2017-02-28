      subroutine set_defcols(status)
      implicit none
c
c  Overwrite PGPLOT standard colors with special defaults
c
c  O  status  (i)  Error flag (0=OK)
c
      integer status

      include '../include/io.inc'
      include '../include/colordef.inc'
c
c  Local variables
c
      integer i

      status = 0

      do i = 0, 15
         if ( pgrvals(i+1).ge.0. ) then
            write(ZWRite,*) ' Color index overwritten: ', i+1
            call xwrite(ZWRite, 15)
            call pgscr(i, PGRvals(i+1), PGGvals(i+1), PGBvals(i+1))
         endif
      enddo

      return
      end
