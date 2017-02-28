
C*****************************************************************************
C FUNCTION:
C      fcislpyr
C
C DESCRIPTION:
C      Is the given year a leap year in the Gregorian calendar?
C
C AUTHOR/DATE:
C      Jeff Guerber, Raytheon STX, 1998-07-03
C
C MODIFICATION HISTORY:
C
C NOTES:
C       A simple "mod(year,4)", often used, is not correct.  Century years
C       are leap years only if divisible by 400 (2000 is; 1900, 2100 are not).
C
C USAGE:
C
C ARGUMENTS:
C      year
C
C*****************************************************************************

      logical function fcislpyr( year )

      implicit none
      integer  year

      fcislpyr = ( (mod(year,4) .eq. 0) .and. (mod(year,100) .ne. 0) )
     &                .or. (mod(year,400) .eq. 0)

      return
      end
