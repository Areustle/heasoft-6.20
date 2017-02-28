
C******************************************************************************
C FTOOLS TASK:
C      int2mjd
C
C FILE:
C      int2mjd.f
C
C DESCRIPTION:
C      Convert integer times to MJD
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       November, 1993
C
C MODIFICATION HISTORY:
C       3/30/94 EAG - Was adding one too many days since months start at day 1
C        9/2/97 James Peachey, HEASARC/GSFC/NASA, Hughes STX - bug fix:
C               code was calculating absyear to convert from 2 digit to
C               4 digit year, but was then not actually using this variable
C               to compute the mjd. Also changed to conform to fits
C               standard in this respect: years in format XX *should* always
C               be interpreted as 19XX.
C       1998-07-03 Jeff Guerber, RSTX/GSFC - use fcislpyr()
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      fcislpyr - is arg a leap year
C
C******************************************************************************
      subroutine int2mjd (year, month, day, hour, minute, second,
     &     mjd, status)

      integer status
      integer day, month, year, hour, minute
      double precision second, mjd

      integer absyear, i, refyear
      double precision absdays, mjdref
      logical fcislpyr

      parameter (refyear = 1970)
      parameter (mjdref = 40587)

      integer daysinmonth(12)
      data daysinmonth /31,28,31,30,31,30,31,31,30,31,30,31/

C check that year has the appropriate 4 digits
      if (year .lt. 100) then
         absyear = year + 1900
      else
         absyear = year
      endif

C  convert day, month, year to days from refyear
      absdays = (absyear - refyear) * 365
      do 10 i = refyear, absyear-1
         if ( fcislpyr(i) ) absdays = absdays + 1
 10   continue

      do 20 i = 1, month - 1
         absdays = absdays + daysinmonth(i)
 20   continue
      if ((month .gt. 2) .and. fcislpyr(absyear) )
     &     absdays = absdays + 1

      absdays = absdays + day

C and add in the fractional day
      absdays = absdays +
     &          (hour + (minute + second/60.0d0)/60.0d0)/24.0d0

C and the mjd is just that many more days ....
C but mjdref is day 1 of the month, not day 0
      mjd = mjdref + absdays - 1

 999  return
      end
