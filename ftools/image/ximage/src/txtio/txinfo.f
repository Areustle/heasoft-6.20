      subroutine txinfo(filename, ncols, nrows, status)
      implicit none
c
c  Returns info on internal text i/o buffer
c
c  I  filename  (s)  Text file location
c  O  ncols     (i)  Number of columns
c  O  nrows     (i)  Number of rows
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer ncols, nrows, status

      include 'txtio.inc'
c
c  Local variables
c
      if ( status.ne.0 ) return

      call txrdfile(filename, status)
      if ( status.ne.0 ) return

      ncols = nqcol
      nrows = nqpts

      if ( txtqfile.eq.' ' ) status = -1
      if ( nrows.eq.-1 ) status = -2
      
      return
      end
