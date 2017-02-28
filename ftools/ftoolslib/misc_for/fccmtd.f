
C******************************************************************************
C FUNCTION:
C      fccmtd
C
C DESCRIPTION:
C      Converts a month to the number of days in the year up to that month
C
C AUTHOR:
C      Janice Tarrant  3/13/92
C
C MODIFICATION HISTORY:
C      Jeff Guerber, 1998-06-27: Fixed leap years, converted to table lookup.
C
C NOTES:
C
C USAGE:
C      x = fccmtd(year,month)
C
C ARGUMENTS:
C      year  - year
C      month - month
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      integer function fccmtd(year,month)
      integer year, month

      integer reg(12)
      data reg /  0,  31,  59,  90, 120, 151,
     &          181, 212, 243, 273, 304, 334/
      integer leap(12)
      data leap/  0,  31,  60,  91, 121, 152,
     &          182, 213, 244, 274, 305, 335/

      if ( ( (mod(year,4).eq.0) .and. (mod(year,100).ne.0) )
     &    .or. (mod(year,400).eq.0) ) then
          fccmtd = leap(month)
      else
          fccmtd = reg(month)
      endif

      return
      end
