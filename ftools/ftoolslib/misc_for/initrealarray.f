
C******************************************************************************
C SUBROUTINE:
C     initrealarray, initdoublearray, initintarray
C
C DESCRIPTION:
C      Initialize a real, integer or double array to 0.
C      Required for dynamic memory
C
C AUTHOR:
C      Dr. Emily A. Greene
C      NASA/GSFC / Hughes STX
C      January, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call initrealarray (memr(array_ptr), nelem)
C      call initdoublearray (memd(array_ptr), nelem)
C      call initintarray (memi(array_ptr), nelem)
C
C ARGUMENTS:
C       array - array to be initialized to 0
C       nelem - number of elements in array
C
C PRIMARY LOCAL VARIABLES:
C
C******************************************************************************
      subroutine initrealarray(array, nelem)
      integer nelem, i
      real array(nelem)

      do 10 i = 1, nelem
         array(i) = 0.
 10   continue
      return
      end
