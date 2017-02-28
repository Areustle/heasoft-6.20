**==tbgtpr.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* This gets the values out of the parameter table
*  Name, descrip, type, minp, maxp, default, and update are character*(*)
* and are returned to you, context is the context variable.
* Set it to 0 to start and it is returned to you as -1 when you have
* gotten them all.
*
*
      SUBROUTINE TBGTPR(Context,Name,Descrip,Type,Minp,Maxp,Default,
     &                  Update)
*
      IMPLICIT NONE
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 Context
      CHARACTER*(*) Name , Descrip , Type , Minp , Maxp , Default , 
     &              Update
 
      IF ( Context.LE.0 ) Context = 1
      IF ( Context.GT.TBLpcnt ) THEN
         Context = -1
         RETURN
      ENDIF
      Name = TBLpname(Context)
      Descrip = TBLpdesc(Context)
      Type = TBLptype(Context)
      Minp = TBLpminp(Context)
      Maxp = TBLpmaxp(Context)
      Default = TBLpdefl(Context)
      Update = TBLpupd(Context)
      Context = Context + 1
C      IF ( Context.GT.TBLccnt ) Context = -1
      RETURN
      END
