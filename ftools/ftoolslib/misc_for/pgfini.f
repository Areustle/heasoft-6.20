

C******************************************************************************
C SUBPROGRAM:
C       pgfini
C
C DESCRIPTION:
C       Initializes the count variable of the PGFCOUNT common block
C
C AUTHOR/DATE:
C       Ron Zellar March 10, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C
C USAGE:
C      This program unit is executed automatically at run time and therefore
C      cannot be called like a subroutine.
C
C PRIMARY LOCAL VARIABLES:
C
C******************************************************************************
      block data pgfini

      integer count
      common /pgfcount/ count

C     Initialize pgfcount common block variable 'count' to be 0
      data count /0/

      end
