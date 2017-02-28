**==tbgtkw.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* This gets the values out of the keyword table
*  Name, and command are character*(*)
* and are returned to you, context is the context variable.
* Set it to 0 to start and it is returned to you as -1 when you have
* gotten them all.
*
*
      SUBROUTINE TBGTKW(Context,Name,Command)
*
      IMPLICIT NONE
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 Context
      CHARACTER*(*) Name , Command
 
      IF ( Context.LE.0 ) Context = 1
      IF ( Context.GT.TBLwcnt ) THEN
         Context = -1
         RETURN
      ENDIF
      Name = TBLwname(Context)
      Command = TBLwcomm(Context)
      Context = Context + 1
C      IF ( Context.GT.TBLccnt ) Context = -1
      RETURN
      END
