      subroutine txrdicol(filename, icol, maxrow, outary, 
     &                  numrow, status)
      implicit none
c
c  Gets icol-th column from text i/o buffer and saves column values in
c    integer array
c
c  I  filename  (s)  Text file location
c  I  icol      (i)  Column number
c  I  maxrow    (i)  Maximum number of rows
c  O  outary    (i)  Output array
c  O  numrow    (i)  Number of rows in output array
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer maxrow
      integer icol, outary(maxrow)
      integer numrow, status

      include '../include/dynmem.inc'
c
c  Local variables
c
      integer i, p_rbuf, frestat

      if ( status.ne.0 ) return

      call ralloc(1, maxrow, 1, p_rbuf, status)
      if ( status.ne.0 ) then
         call XWRITE(' txrdicol: Failed to allocate temp buffer', 10)
         return
      endif

      call txrdcol(filename, icol, maxrow, memr(p_rbuf), numrow, status)

      if ( status.eq.0 ) then
         do i = 1, numrow
            outary(i) = int(memr(p_rbuf+i-1))
         enddo
      endif

      call ralloc(0, maxrow, 1, p_rbuf, frestat)

      return
      end
