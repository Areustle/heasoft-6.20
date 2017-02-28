**==CRTFMT.spg  processed by SPAG 3.09I  at 09:41 on 20 Aug 1992
      SUBROUTINE CRTFMT(Inint,Outchars,Case)
 
c  this routine determines the variable part of a variable
c    format statement and creates a string to be used in the
c    format specification
 
c    inint:  the variable part of the format specifier
c    outchars:  the format specification string returned to calling pgm
c    case:  what kind of format specification is needed
 
 
      INTEGER*4 Inint
      character(20) Outchars
      INTEGER*4 Case
      INTEGER*4 newint
 
      character(1) cvals(10)
      DATA cvals/'0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , 
     &     '9'/
c
c  This case is (in)
c
      IF ( Case.EQ.1 ) THEN
         Outchars(1:2) = '(i'
         Outchars(3:3) = cvals(Inint+1)
         Outchars(4:4) = ')'
      ENDIF
c
c This case is (in.n)
c
      IF ( Case.EQ.2 ) THEN
         Outchars(1:2) = '(I'
         Outchars(3:3) = cvals(Inint+1)
         Outchars(4:4) = '.'
         Outchars(5:5) = cvals(Inint+1)
         Outchars(6:6) = ')'
      ENDIF
c
c This case is (nx,in,1x,a)
c
      IF ( Case.EQ.3 ) THEN
         newint = 6 - Inint
         Outchars(1:1) = '('
         Outchars(2:2) = cvals(newint+1)
         Outchars(3:5) = 'x,i'
         Outchars(6:6) = cvals(Inint+1)
         Outchars(7:12) = ',1x,a)'
      ENDIF
c
c This case is (na1)
c
      IF ( Case.EQ.4 ) THEN
         Outchars(1:1) = '('
         Outchars(2:2) = cvals(Inint+1)
         Outchars(3:5) = 'a1)'
      ENDIF
c
c This case is (n(1x,f7.3))
c
      IF ( Case.EQ.5 ) THEN
         Outchars(1:2) = '('//cvals(Inint+1)
         Outchars(3:12) = '(1x,f7.3))'
      ENDIF
 
      IF ( Case.LT.1 .OR. Case.GT.5 ) THEN
         WRITE (*,*) ' crtfmt cannot find requested format specifier'
      ENDIF
 
      RETURN
      END
