
C****************************************************************************
C subroutine:
C      fgetdval
C
C FILE:
C      fgetdval.f
C
C DESCRIPTION:
C       This routine reads one row of the specified column and returns
C       a single value based on the mode.
C
C   Currently supported modes:
C       sum entire array
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       December 12, 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C CALLING SEQUENCE:
C       call fgetdval (unit, colno, row, mode, value, count, status)
C
C ARGUMENTS:
C    unit   - integer -  input - the unit number to use
C    colno  - integer -  input - the column number to use
C    repeat - integer -  input - total number of elements in the column
C    row    - integer -  input - the row to use
C    mode   - integer -  input - what to do
C    value  - double  - output - the returned output value
C    count  - integer - output - the number of elements used to calculate value
C    status - integer - output - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C*****************************************************************************

      subroutine fgetdval (unit, colno, repeat, row, mode, value,
     &     count, status)

      integer unit, colno, row, mode, count, status
      double precision value

      integer maxdim, maxsize
      parameter (maxdim = 10)
      parameter (maxsize = 360)

      integer i, repeat, remain, nelem
      logical anyf, flagvals(maxsize)
      double precision dvalues(maxsize)

C  Since only one mode is currently supported, don't check.  That way
C  future needs can determine the values for mode

C get all the elements in the current row
      remain = repeat
      value = 0.D0
      count = 0

 50   if (remain .gt. maxsize) then
         nelem = maxsize
      else
         nelem = remain
      endif

      call ftgcfd (unit, colno, row, 1, nelem, dvalues, flagvals, anyf,
     &     status)

      do 100 i = 1, nelem
         if (.not. flagvals(i)) then
            value = value + dvalues(i)
            count = count + 1
         endif
 100  continue

      remain = remain - nelem
      if (remain .gt. 0) goto 50

      return
      end
