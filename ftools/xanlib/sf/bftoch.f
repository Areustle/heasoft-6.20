      subroutine BFTOCH (buffer, lenb, string)
      character(1) buffer(*)
      integer   lenb
      character*(*) string
C---
C SF subroutine to convert a byte buffer array to a character
C string
C---
C buffer  I    Input byte array
C lenb    I    Length of array to be transfered
C string    O  Character string destination
C---
C 1985-Mar-08 - rashafer
C---
      integer*4 lenc, len, i
C---
      lenc=len(string)
      do i=1,min(lenc,lenb)
         string(i:i)=buffer(i)
      end do
      if(lenc.gt.lenb) then
         string(lenc:)=' '
      end if
      return
      end
