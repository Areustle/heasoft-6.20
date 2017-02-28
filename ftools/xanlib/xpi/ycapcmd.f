**==ycapcmd.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Captures the command
*
      SUBROUTINE YCAPCMD(Command)
*
*
      IMPLICIT NONE
 
      INCLUDE 'yaccfor.inc'
 
*      BYTE Command(MAXLINESIZE)
      CHARACTER*(*) Command
 
      INTEGER i
 
      DO 100 i = 1 , LEN(Command)
*         IF ( Command(i).EQ.0 ) GOTO 200
         IF ( ICHAR(Command(i:i)).GE.32 .AND. ICHAR(Command(i:i))
     &        .LE.127 ) THEN
            SCOm(i:i) = Command(i:i)
            COMval = .TRUE.
         ENDIF
 100  CONTINUE
 
 
      RETURN
 
      END
