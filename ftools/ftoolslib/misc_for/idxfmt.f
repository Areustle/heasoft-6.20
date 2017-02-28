

C******************************************************************************
C subroutine:
C      idxfmt
C
C FILE:
C      idxfmt.f
C
C DESCRIPTION:
C       This routine formats a vector of array indices into a string
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       February 15, 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C CALLING SEQUENCE:
C       call idxfmt (ndims, elements, string, status)
C
C ARGUMENTS:
C       ndims    - number of dimensions
C       elements - vector containing the current element numbers
C       string   - output formatted string
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************

      subroutine idxfmt (ndims, elements, string, status)

      integer ndims, elements(ndims), status
      character*(*) string

      integer i, position

      string = '['
      position = 2
      do 100 i = 1, ndims
         write (string(position:), 1001) elements(i)
 1001    format (I10,';')
         position = position + 11
 100  continue
      string(position-1:) = ']'
      call frmblk (string)

      return
      end
