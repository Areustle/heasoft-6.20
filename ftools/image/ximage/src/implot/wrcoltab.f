      subroutine wrcoltab(Filename, Status)
      implicit none
c
c  Write out current color table into file
c
c I/O filename   (s)  Color table file
c  O  status     (i)  Error flag (0 = OK)
c
      character*(*) Filename
      integer Status

      include 'colortab.inc'
      include '../include/dynmem.inc'
      include '../include/io.inc'
c
c  Local variables
c
      logical isdisplay 
      integer i, minci, maxci, ncols, LENACT
      integer p_rvals, p_gvals, p_bvals, frstat

      if ( .not.isdisplay() ) then
         call xwrite(' No current display', 10)
         call xwrite(' Cannot write color table file', 10)
         Status = -1
         return
      endif
c
c  Inquire current color state
c
      call pgqcir(minci, maxci)
      ncols = maxci - minci + 1
      call ralloc(1, ncols, 1, p_rvals, status)
      call ralloc(1, ncols, 1, p_gvals, status)
      call ralloc(1, ncols, 1, p_bvals, status)

      do i = 1, ncols
         call pgqcr(i+minci-1, memr(p_rvals+i-1), memr(p_gvals+i-1),
     &                         memr(p_bvals+i-1))
      enddo
c
c  Write color table
c
      call xtend(Filename, 'tab')
      call txinit(Status)
      write (ZWRite, *) '! Original color table: ',
     &                  CURcoltab(:LENACT(CURcoltab))
      call txwrcom(Filename, ZWRite, status)
      write (ZWRite, *) '! Contrast setting    : ', CURcontra 
      call txwrcom(Filename, ZWRite, status)
      write (ZWRite, *) '! Brightness setting  : ', CURbright 
      call txwrcom(Filename, ZWRite, status)
      call txwrcol(Filename, memr(p_rvals), ncols, status)
      call txwrcol(Filename, memr(p_gvals), ncols, status)
      call txwrcol(Filename, memr(p_bvals), ncols, status)
      write(ZWRite, *) ' Writing color table: ',
     &                   Filename(:LENACT(Filename))
      call xwrite(ZWRite, 10)
      call txwrfile(Filename, status)

      call ralloc(0, ncols, 1, p_rvals, frstat)
      call ralloc(0, ncols, 1, p_gvals, frstat)
      call ralloc(0, ncols, 1, p_bvals, frstat)

      return
      end
