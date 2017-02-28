      subroutine txrdcol(filename, icol, maxrow, outary, 
     &                 numrow, status)
      implicit none
c
c  Gets icol-th column from text i/o buffer and saves column values in
c    real array
c
c  I  filename  (s)  Text file location
c  I  icol      (i)  Column number
c  I  maxrow    (i)  Maximum number of rows
c  O  outary    (r)  Output array
c  O  numrow    (i)  Number of rows in output array
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer maxrow
      integer icol
      real*4 outary(maxrow)
      integer numrow, status

      include 'txtio.inc'
      include '../include/io.inc'
c
c  Local variables
c
      integer i

      if ( status.ne.0 ) return

      call txrdfile(filename, status)
      if ( status.ne.0 ) return
  
      if ( icol.lt.1 .or. icol.gt.NQCol ) then
         write(ZWRite,*) ' txrdcol: Column ', icol, ' does not exist'
         call RMVXBK(ZWRite(2:))
         call XWRITE(ZWRite, 10)
         status = -1
         return
      endif

      if ( maxrow.lt.NQPts ) then
         call XWRITE(' txrdcol: Column size exceeded', 10)
         status = -1
         return
      endif

      numrow = NQPts
      do i = 1, numrow
         outary(i) = QBUf(i+NQRows*(icol-1))
      enddo

      return
      end
