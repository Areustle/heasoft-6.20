
C******************************************************************************
C SUBROUTINE:
C      fccmpr
C
C DESCRIPTION:
C      Compares the rows list with a list of allowable characters.
C
C AUTHOR/DATE:
C      Janice Tarrant  1/17/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call fccmpr(rows,goodrows)
C
C ARGUMENTS:
C      rows     - list of rows to translate
C      goodrows - flag indicates allowable row list
C
C PRIMARY LOCAL VARIABLES:
C      goodchars - string of allowable row characters
C      rowlen    - length of rows list
C
C CALLED ROUTINES:
C      function fcstln - returns length of character string (integer)
C
C******************************************************************************
      subroutine fccmpr(rows,goodrows)
      character*(*) rows
      logical goodrows

      character(12) goodchars
      integer i, j, rowlen, fcstln

C check for a starting row of 0 which is not allowed
      if (rows(1:1) .eq. '0') then
         goodrows = .false.
         goto 30
      endif

C set the allowable characters
      goodchars = '0123456789,-'

C get the rows list length
      rowlen = fcstln(rows)

C determine whether each character in rows list is allowable
      do 10 i = 1, rowlen
         goodrows = .false.
         do 20 j = 1, 12
            if (rows(i:i) .eq. goodchars(j:j))  goodrows = .true.
 20      continue
         if (.not. goodrows)  goto 30
 10   continue
      goto 40

C  send error message
 30   call fcerr('row specifier is not allowed')

 40   continue
      return
      end
