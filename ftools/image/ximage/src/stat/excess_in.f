      subroutine excess_in(infile, status)
      implicit none
c
c  Reads excess finding results from file into excess.inc
c
c  I  infile   (s)  output file
c  O  status    (i)  error flag
c
      integer status
      character*(*) infile

      include '../include/io.inc'
      include 'excess.inc'
c
c  Local variables
c
      integer LENACT
      logical there

      inquire(file=infile,exist=there)
      if ( .not.there ) then
         call xtend(infile, 'xs')
      endif

      write(ZWRite,*) ' Reading excess file: ',
     &                infile(:LENACT(infile))
      call xwrite(ZWRite, 10)

      call txinit(status)
      call txrdfile(infile, status)
      call txrdcol(infile, 2, MAXsou, ASOux, NUMexs, status)
      call txrdcol(infile, 3, MAXsou, ASOuy, NUMexs, status)
      call txrdcol(infile, 6, MAXsou, BXN, NUMexs, status)
      call txrdicol(infile, 8, MAXsou, INTm, NUMexs, status)

      if ( status.ne.0 ) then
         call XWRITE(' Failed to read excess file', 10)
         NUMexs = 0
      endif

      return
      end
