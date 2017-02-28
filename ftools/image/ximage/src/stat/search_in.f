      subroutine search_in(infile, status)
      implicit none
c
c  Read source searching results from file
c
c  I  infile   (s)  input file
c  O  status   (i)  error flag
c
      integer status
      character*(*) infile

      include '../include/io.inc'
      include 'detect.inc'
c
c  Local variables
c
      integer i, LENACT
      logical there

      inquire(file=infile,exist=there)
      if ( .not.there ) then
         call xtend(infile, 'sr')
      endif

      write(ZWRite,*) ' Reading source search file: ',
     &                infile(:LENACT(infile))
      call xwrite(ZWRite, 10)

      call txinit(status)
      call txrdfile(infile, status)
      call txrdcol(infile, 2, MAXdet, DTSox, NUMdet, status)
      call txrdcol(infile, 3, MAXdet, DTSoy, NUMdet, status)
      call txrdcol(infile, 4, MAXdet, PSFco, NUMdet, status)
      call txrdcol(infile, 5, MAXdet, VCO, NUMdet, status)
      call txrdcol(infile, 6, MAXdet, BASo, NUMdet, status)
      call txrdcol(infile, 7, MAXdet, SINt, NUMdet, status)
      call txrdcol(infile, 8, MAXdet, ERR, NUMdet, status)
      call txrdcol(infile, 9, MAXdet, PROb, NUMdet, status)
      call txrdcol(infile, 10, MAXdet, BXHa, NUMdet, status)
      call txrdicol(infile, 11, MAXdet, GOOdflag, NUMdet, status)
      do i = 1, NUMdet
         HOT(i) = 1
      enddo

      if ( status.ne.0 ) then
         call XWRITE(' Failed to read source search file', 10)
         NUMdet = 0
      endif

      return
      end
