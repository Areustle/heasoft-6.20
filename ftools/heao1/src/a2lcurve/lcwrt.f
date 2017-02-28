      subroutine writefitslc(fitsunit, row, daynum, msec, source, error, 
     +           eff, status)

      implicit none

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables

      integer fitsunit, row, daynum, msec,status

      real source, error, eff

      double precision obstime

      obstime =  (daynum*8.64D4) + (msec/1.0D3)
      call ftpcld(fitsunit, 1, row, 1, 1, obstime, status)
      call ftpcle(fitsunit, 2, row, 1, 1, source, status)
      call ftpcle(fitsunit, 3, row, 1, 1, error, status)
      call ftpcle(fitsunit, 4, row, 1, 1, eff, status)

      return

      end

