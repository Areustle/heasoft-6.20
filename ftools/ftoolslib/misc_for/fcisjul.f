
C*****************************************************************************
C FUNCTION:
C      fcisjul
C
C DESCRIPTION:
C      Does the argument look like a JD (or MJD), or a calendar date?
C      JD if it has neither '/' nor '-'.
C
C AUTHOR
C      Jeff Guerber, Raytheon STX, 1998-06-27
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C ARGUMENTS:
C      date - a date string
C
C*****************************************************************************

      logical function fcisjul( date )

      implicit none
      character*(*)  date

      fcisjul = (index(date,'/') .eq. 0) .and. (index(date,'-') .eq. 0)

      return
      end
