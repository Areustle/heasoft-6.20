**==tbgtky.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* This gets the values out of the key table
*  Name, param, position, and cluster are character*(*)
* and are returned to you, context is the context variable.
* Set it to 0 to start and it is returned to you as -1 when you have
* gotten them all.
*
*
      SUBROUTINE TBGTKY(Context,Name,Param,Position,Cluster,Exclusive)
*
      IMPLICIT NONE
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 Context
      CHARACTER*(*) Name , Param , Cluster
      INTEGER*4 Position , Exclusive
 
      IF ( Context.LE.0 ) Context = 1
      IF ( Context.GT.TBLkcnt ) THEN
         Context = -1
         RETURN
      ENDIF
      Name = TBLkname(Context)
      Param = TBLkparm(Context)
      Position = TBLkposo(Context)
      Cluster = TBLkclus(Context)
      Context = Context + 1
C     IF ( Context.GT.TBLccnt ) Context = -1
      RETURN
      END
