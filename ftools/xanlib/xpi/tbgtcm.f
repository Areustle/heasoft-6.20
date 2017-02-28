**==tbgtcm.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* This gets the values out of the command table
*  Name, descrip, access, and wintype are character*(*) and are
* returned to you, context is the context variable.  Set it to 0 to start and it
* is returned to you as -1 when you have gotten them all.
*
*
      SUBROUTINE TBGTCM(Context,Name,Descrip,Access,Wintype)
*
      IMPLICIT NONE
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 Context
      CHARACTER*(*) Name , Descrip , Access , Wintype
 
      IF ( Context.LE.0 ) Context = 1
      IF ( Context.GT.TBLccnt ) THEN
         Context = -1
         RETURN
      ENDIF
      Name = TBLcname(Context)
      Descrip = TBLcdesc(Context)
      Access = TBLcacce(Context)
      Wintype = TBLcwtyp(Context)
      Context = Context + 1
      RETURN
      END
