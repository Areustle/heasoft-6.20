
C******************************************************************************
C SUBROUTINE:
C      fcasfm
C
C DESCRIPTION:
C      determine the data type for ascii table formats,similar to ftbnfm
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C
C USAGE:
C      call fcasfm(form,dattyp,status)
C
C ARGUMENTS:
C     form - ascii format type
C     dattyp - data type
C     status - error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine fcasfm(form,dattyp,status)

      character*(*) form
      integer     dattyp,status

      status = 0
      if ( form(1:1) .eq. 'I' ) then
         dattyp = 41
      else if ( form(1:1) .eq. 'A' ) then
         dattyp = 16
      else if ( form(1:1) .eq. 'F' ) then
         dattyp = 42
      else if ( form(1:1) .eq. 'E' ) then
         dattyp = 42
      else if ( form(1:1) .eq. 'D' ) then
         dattyp = 82
      else
         status = 500
      endif

      return
      end
