      subroutine txwrcol(filename, inary, numrow, status)
      implicit none
c
c    Appends a column to text i/o buffer 
c
c  I  filename  (s)  Text file location
c  I  inary     (r)  Input array of column values
c  I  numrow    (i)  Number of column values
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      real*4 inary(*)
      integer numrow, status

      include 'txtio.inc'
      include '../include/io.inc'
c
c  Local variables
c
      integer i, j

      if ( status.ne.0 ) return

      if ( TXTqfile.ne.filename ) then
         call txinit(status)
         TXTqfile = filename
      elseif ( TXTqmode.eq.0 ) then
c
c  rdqdp sets wrap to maximum number of rows depending on number of
c  columns.  When appending columns to read in data, it is necessary
c  to move adjust the data such that the wrap value is the number
c  of rows
c
        if ( NQRows.lt.NQPts ) then
           call XWRITE(' txwrcol: Not enough space to append column',
     &                  10)
           status = -1
           return
        endif
        do i = 2, NQCol
           do j = 1, NQPts
              QBUF(j+NQPts*(i-1)) = QBUf(j+NQRows*(i-1))
           enddo
        enddo
        NQRows = NQpts
        TXTqmode = 1
         
      endif

      if ( NQPts.eq.0 ) then
         NQPts = numrow
         NQRows = numrow
      endif

      if ( numrow.ne.NQPts ) then
         call XWRITE(' txwrcol: Column lengths differ', 10)
         status = -1
         return
      endif

      NQCol = NQCol + 1
      QERr(NQCol) = 0

      do i = 1, numrow
         QBUf(i+NQRows*(NQCol-1)) = inary(i)
      enddo

      TXTqmode = 1

      return
      end
