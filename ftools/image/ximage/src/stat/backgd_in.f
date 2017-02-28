      subroutine backgd_in(infile, status)
      implicit none
c
c  Read file into backgd include
c
c  I  infile   (i)  Input file location
c  O  status   (i)  Error flag (0 = OK)
c
      character*(*) infile
      integer status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include 'backgd.inc'
c
c  Local variables
c
      integer LENACT
      logical there

      inquire(file=infile,exist=there)
      if ( .not.there ) then
         call xtend(infile, 'bg')
      endif

      write(ZWRite, *) ' Reading background file: ', 
     &                   infile(:LENACT(infile))
      call XWRITE(ZWRite, 10)
      call txinit(status)
      call txrdfile(infile, status)
      call txrdikey(infile, 'NBX', 0, NBX, status)
      call txrdikey(infile, 'NBY', 0, NBY, status)
      call txrdikey(infile, 'IBBAC', 0, IBBac, status)
      call txrdkey(infile, 'BACK', 0, BACk, status)
      call txrdkey(infile, 'BNEW', 0, BNEw, status)
      call txrdicol(infile, 2, MAXbox, BB_flags, NBOxes, status)
      call txrdcol(infile, 3, MAXbox, BB, NBOxes, status)
      call txrdcol(infile, 4, MAXbox, BB_sig, NBOxes, status)
      call txrdcol(infile, 5, MAXbox, WW, NBOxes, status)
      call txrdicol(infile, 6, MAXbox, BB_npix, NBOxes, status)

      if ( status.ne.0 ) then
         call XWRITE(' Failed to read background file', 10)
         NBOxes = 0
      endif

      RETURN
      END
