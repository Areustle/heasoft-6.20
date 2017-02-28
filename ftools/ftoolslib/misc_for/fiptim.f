
C******************************************************************************
C SUBROUTINE:
C      fiptim
C
C DESCRIPTION:
C      Parse a time string of the form hh:mm:ss.ssss and return hour and
C       minutes as integer, and seconds as real
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       August, 1992
C
C MODIFICATION HISTORY:
C       EAG  1/22/93 - Added BN to format statement for VAX
C       EAG  2/22/93 - Fixed bug with integer seconds
C       EAG 2/25/94 2.8a - Again fixed bug with integer seconds
C       Jeff Guerber, 1998-06-23 - 2.8b.  Second can be 60 (UT leap seconds)
C       Jeff Guerber, 1999-03-26 - 2.8c.  Bug fixes: Check loc2 against loc1
C             not 0; return a status on read error.
C
C NOTES:
C      Cfitsio routine fts2tm/ffs2tm/fits_str2time can do the same thing
C      (except that fiptim allows 1-digit components).
C
C USAGE:
C      call fiptim(time, hour, minute, second, status)
C
C ARGUMENTS:
C    input:
C       time - string containing time of the form hh:mm:ss.ssss
C    output:
C       hour   - integer hour
C       minute - integer minute
C       second - real second
C       status   - returned error status (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************
      subroutine fiptim(time, hour, minute, second, status)

      implicit none
      character*(*) time
      integer hour, minute, status
      real second

      character(80) context
      character(4) version
      integer loc1, loc2, fcstln

      version = '2.8c'

C find the first : in the time string
      loc1 = index(time, ':')

C blank string
      if (loc1 .le. 0) then
         status = 10
         context = 'FIPTIM' // version // ' blank time string'
         call fcerr (context)
         goto 999
      endif

C nothing before 1st :
 150  if (loc1 .eq. 1) then
         status = 11
         context = 'FIPTIM' // version // ' no hour in string'
         call fcerr (context)
         goto 999
      endif

C if only 1 digit before :
      if (loc1 .eq. 2) then
         read (time, 1000, err=998) hour
 1000    format (BN,i1)
      else

C otherwise 2 digits
         read (time(loc1-2:loc1-1),1001,err=998) hour
 1001    format (BN,i2)
      endif

C find the second : in the time
      loc2 = index(time(loc1+1:), ':')
      loc2 = loc2 + loc1

C blank string
      if (loc2 .le. loc1) then
         status = 12
         context = 'FIPTIM' // version // ' no second :'
         call fcerr (context)
         goto 999
      endif

 250  read (time(loc1+1:loc2-1),1001, err=998) minute

C and finally read the seconds
C temporary string storage needed to add a . if necessary
      context = time(loc2+1:)
      if (index(context,'.') .le. 0)
     &     context(fcstln(context)+1:fcstln(context)+1) = '.'
      read (context, 1002, err=998) second
 1002 format(F10.6)

C now check the values to make sure they make sense
      if ((hour .lt. 0) .or. (hour .gt. 23)) then
         status = 13
         write (context, 1100) version, hour
 1100    format ('FIPTIM', A4,
     &        ' Value of hour not reasonable - ',i6)
         call fcerr (context)
         goto 999
      endif

      if ((minute .lt. 0) .or. (minute .ge. 60)) then
         status = 14
         write (context, 1101) version, minute
 1101    format ('FIPTIM', A4,
     &        ' Value of minute not reasonable - ',i6)
         call fcerr (context)
         goto 999
      endif

      if ((second .lt. 0.) .or. (second .ge. 61.)) then
         status = 15
         write (context, 1102) version, second
 1102    format ('FIPTIM', A4,
     &        ' Value of second not reasonable - ',f10.4)
         call fcerr (context)
         goto 999
      endif

      return
 998  context = 'FIPTIM' // version // ' error reading value: ' // time
      call fcerr (context)
      status = 16
 999  return
      end
