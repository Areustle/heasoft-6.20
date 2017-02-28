

C*****************************************************************************
C subroutine:
C      rmsubset
C
C FILE:
C      rmsubset.f
C
C DESCRIPTION:
C       This routine removes the subset specification [] from the
C       end of a column name and returns it in another string
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       February 15, 1994
C
C MODIFICATION HISTORY:
C      12/9/94 EAG Allow for enclosing ()
C
C NOTES:
C
C CALLING SEQUENCE:
C      call rmsubset (ncols, colist, subset, status)
C
C ARGUMENTS:
C       ncol   - number of columns to operate on
C       colist - array of column names
C       subset - returned subset specifications
C       status - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine rmsubset (ncols, colist, subset, status)

      integer ncols, status
      character*(*) colist(ncols), subset(ncols)

      integer i, position

C initialize subset to blank
      do 100 i = 1, ncols
         subset(i) = ' '
         position = max(index(colist(i),'['),index(colist(i),'('))
         if (position .gt. 0) then
            subset(i) = colist(i)(position:)
            colist(i)(position:) = ' '
         endif
 100  continue

 999  return
      end
