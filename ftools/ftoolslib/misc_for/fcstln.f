


C******************************************************************************
C FUNCTION:
C      fcstln
C
C DESCRIPTION:
C      Finds length of character string throwing out end spaces
C
C AUTHOR/DATE:
C      Janice Tarrant  12/12/91
C
C MODIFICATION HISTORY:
C      Ning Gan 04/30/00: Added the check to see whether the char is \0.
C
C NOTES:
C
C USAGE:
C      x = fcstln(string)
C
C ARGUMENTS:
C      string - text string
C
C PRIMARY LOCAL VARIABLES:
C      length - length of text string, including end spaces
C
C CALLED ROUTINES:
C
C******************************************************************************
      integer function fcstln(string)

      character*(*) string
      integer  length, i

      length = len(string)
      do 10 i = length,1,-1
         if ( string(i:i) .ne. ' ' .and. ichar(string(i:i)).ne.0) 
     *     goto 20
 10   continue
 20   fcstln = i
      return
      end
