      subroutine txrdval(filename, icol, irow, val, status)
      implicit none
c
c  Gets real value from icol-th column, irow-th row from text i/o 
c  buffer and copies value into val
c
c  I  filename  (s)  Text file location
c  I  icol      (i)  Column number
c  I  irow      (i)  Row number
c  O  val       (r)  Real value
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer icol, irow
      real*4 val
      integer status

      include 'txtio.inc'
      include '../include/io.inc'

      if ( status.ne.0 ) return

      call txrdfile(filename, status)
      if ( status.ne.0 ) return
  
      if ( icol.lt.1 .or. icol.gt.NQCol ) then
         write(ZWRite,*) ' txrdval: Column ', icol, ' does not exist'
         call RMVXBK(ZWRite(2:))
         call XWRITE(ZWRite, 10)
         status = -1
         return
      endif

      if ( irow.lt.1 .or. irow.gt.NQPts ) then
         write(ZWRite,*) ' txrdval: Row ', irow, ' does not exist'
         call RMVXBK(ZWRite(2:))
         call XWRITE(ZWRite, 10)
         status = -1
         return
      endif

      val = QBUf(irow+NQRows*(icol-1))

      return
      end
