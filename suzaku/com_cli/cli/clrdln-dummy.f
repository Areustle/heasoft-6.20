C
C File: clrdln-dummy.f
C Desription: Dummy Version CLrdln
C Author: A.Shirahashi
C Date: 27-Apr-1993
C
C History:
C     29-Sep-1994, PROMPT -> CLprom
C     19-Feb-2005 Y.ISHISAKI,
C         add dummy entries of CLsigunsetmask, CLrhis, CLwhis, CLphis
C
      Subroutine CLrdln( pro,buf,length )
      Implicit None
C output
      Character * (*)  pro, buf
      Integer  length
C function
      Integer  Lenrd
C begin
      Call CLprom( pro )
C
      Read( 5,'(A)',End=800 ) buf
      length = Lenrd(buf)
C
      Return
C
 800  Continue
      length = -1
      Return
C
      End
C
      Subroutine CLsigunsetmask
      Implicit None
C begin
      Return
      End
C
      Subroutine CLrhis( file, error )
      Implicit None
C input
      Character * (*)  file
C output
      Integer  error
C begin
      error = 0
C
      Return
      End
C
      Subroutine CLwhis( file, lines, error )
      Implicit None
C input
      Character * (*)  file
      Integer  lines
C output
      Integer  error
C begin
      error = 0
C
      Return
      End
C
      Subroutine CLphis( file )
      Implicit None
C input
      Character * (*)  file
C begin
      Return
      End
