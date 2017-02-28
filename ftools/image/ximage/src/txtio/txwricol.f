      subroutine txwricol(filename, inary, numrow, status)
      implicit none
c
c    Appends an integer column to text i/o buffer 
c
c  I  filename  (s)  Text file location
c  I  inary     (i)  Input array of column values
c  I  numrow    (i)  Number of column values
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer*4 inary(*)
      integer numrow, status

      include 'txtio.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      integer i, p_rbuf, frestat

      if ( status.ne.0 ) return

      call ralloc(1, numrow, 1, p_rbuf, status)
      if ( status.ne.0 ) then
         call XWRITE(' txwricol: Failed to allocate temp buffer', 10)
         return
      endif

      do i = 1, numrow
         memr(p_rbuf+i-1) = float(inary(i))
      enddo

      call txwrcol(filename, memr(p_rbuf), numrow, status)

      call ralloc(0, numrow, 1, p_rbuf, frestat)

      return
      end
