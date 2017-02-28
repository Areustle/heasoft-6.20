**==umsput.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UMSPUT(Text,Dest,Priority,Status)

C
C EAG 11/28/94 changed to be called by fxwrite
C
 
      CHARACTER*(*) Text
      INTEGER Dest , Priority , Status
      integer lstr, lenact

c suppressing a compiler warning
      lstr = Priority

c chattiness dealt with in xwrite 
c      CALL XCHATY(10,10)

      lstr = max(1, lenact(Text))
 
      IF ( Dest.EQ.2 ) THEN
         call cstderrwrt(Text)
      ELSE
         call ttwrt (Text, lstr)
      ENDIF
c
      Status = 0
      RETURN
      END
 
