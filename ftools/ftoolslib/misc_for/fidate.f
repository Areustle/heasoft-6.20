
C******************************************************************************
C SUBROUTINE:
C      fidate
C
C DESCRIPTION:
C      Parse a date string of the form dd/mm/yy or yyyy-mm-dd and return
C      as integers
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       26 August, 1992
C
C MODIFICATION HISTORY:
C       EAG  1/22/93 - Added BN to format statment for VAX
C       EAG  3/30/94 - Fixed bug if second / is omitted
C       Jeff Guerber 1998-06-27 - Replaced parsing code with call to new
C           cfitsio routine fts2dt, which also does new-format dates and
C           gives 4-digit years.
C       Jeff Guerber 1998-07-03 - use fcislpyr()
C
C NOTES:
C       DEPRECATED: USE CFITSIO FTS2DT OR FTS2TM IN PREFERENCE TO THIS!!!
C       Now returns 4-digit years.
C
C USAGE:
C      call fidate (date,day,month,year,status)
C
C ARGUMENTS:
C    input:
C       date - string containing date of the form dd/mm/yy
C    output:
C       day   - integer day of month
C       month - integer month of year
C       year  - 4 digit integer year
C       status   - returned error status (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      function fcislpyr - is argument a leap year
C
C******************************************************************************
      subroutine fidate (date, day, month, year, status)

      character*(*) date
      integer day, month, year, status

      character(80) context
      logical  fcislpyr

      call fts2dt( date, year, month, day, status )

      if (status .ne. 0) then
          write (context, *) ' fidate: fts2dt status=', status
          call fcerr(context)
          call ftgerr(status, context)
          call fcerr(context)
          goto 999
      endif

C now check the values to make sure they make sense (redundant with fts2dt)

      if ((month .le. 0) .or. (month .gt. 12)) then
         status = 14
         write (context, 1101) month
 1101    format (' month not reasonable - ',i6)
         call fcerr (context)
         goto 999
      endif

C check each month for reasonable day number
      if (day .gt. 0) then
         if (((month .eq. 1) .or. (month .eq. 3) .or. (month .eq. 5)
     &        .or. (month .eq. 7) .or. (month .eq. 8)
     &        .or. (month .eq. 10) .or. (month .eq. 12))
     &        .and. (day .gt. 31)) then
            status = 15
            write (context, 1102) day, month
 1102       format(' day number ',i6,' is not reasonable for month ',i6)
            call fcerr (context)
            goto 999
         else if (((month.eq.4).or.(month.eq.6).or.(month .eq. 9)
     &           .or.(month.eq.11)).and.(day.gt.30)) then
            status = 15
            write (context, 1102) day, month
            call fcerr (context)
            goto 999
C February
         else if ((month .eq. 2) .and. (day .gt. 28)) then
C check for leap year
            if ( (fcislpyr(year) .or. (year .eq. 0)) .and.
     &           (day .gt. 29)) then
               status = 15
               write (context, 1103) day, year
 1103          format (' February ',i6,' is not reasonable for the year'
     &              ,i6)
               call fcerr (context)
               goto 999
            endif
         endif
      else
         status = 16
         context = ' Day of month is less than 0'
         call fcerr (context)
         goto 999
      endif

 999  return
      end
