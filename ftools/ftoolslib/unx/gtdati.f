*+GTDATI
        subroutine gtdati(day, month, year)
        implicit none
        integer day, month, year

C-----------------------------------------------------------------------
C Description: Gets the current system date and time and returns the
C              day of the month, month number and year as integers to
C              day, month, and year respectively.
C
C Arguments:   day    (r): the day of the month (1-31)
C              month  (r): the month number (1-12)
C              year   (r): the year (1993-2092)
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar (1993 June 9) Original Version
C              Joe Ftools (1996 July 2) Naughty idate call replaced
c           Mike Tripicco (1996 Aug 29) Cleaning up Joe's mess again
c                                       by initializing variables
C
C-----------------------------------------------------------------------
*-Version 2.0

        integer status

        day=0
        month=0
        year=0
        status=0

        call ftgsdt(day,month,year,status)

        return
        end
