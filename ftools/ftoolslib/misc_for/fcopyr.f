
C******************************************************************************
C SUBROUTINE:
C      fcopyr
C
C DESCRIPTION:
C      copys a row from one fits file to another
C
C AUTHOR/DATE:
C      J. Kent Blackburn 9/3/92 - as fswapr
C
C MODIFICATION HISTORY:
C       10/15/92 - modified to copy from one file to another (EAG)
C
C NOTES:
C
C
C USAGE:
C      call fcopyr (iunit ,row1, ounit, row2,width,ftstat)
C
C ARGUMENTS:
C      iunit   - input file unit number
C      row1    - input row number
C      ounit   - output file unit number
C      row2    - output row number
C      width   - width of row in bytes
C      ftstat  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      repeat  - repeat factor for column element, only 1 supported
C      fchar   - first char of row to move
C      buffer  - buffer for transferring bytes in row
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine fcopyr (iunit,row1, ounit, row2,width,ftstat)

      integer iunit, row1, ounit, row2,width,ftstat
      integer buffer1(512),buffer2(512)
      integer mxbyte,fchar,remain,nchar
      parameter ( mxbyte = 2048 )

C   don't overflow buffer
      buffer1(1) = 32
      buffer2(1) = 32
      fchar = 1
      remain = width
 10   continue
      if (remain .gt. 0) then
         if (remain .gt. mxbyte) then
            nchar = mxbyte
         else
            nchar = remain
         endif
      endif

C   read in the row from the fits file in blocks
      call ftgtbb(iunit,row1,fchar,nchar,buffer1,ftstat)

C   write out the row to the fits
      call ftptbb(ounit,row2,fchar,nchar,buffer1,ftstat)

      remain = remain - nchar
      fchar = fchar + nchar
      if (remain .gt. 0) goto 10

      return
      end
