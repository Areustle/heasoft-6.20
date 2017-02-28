**==ycmprg.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* this compress ranges from multiple parameters down to one parameter
* it does all of the parameter from start to npars
 
      SUBROUTINE YCMPRG(Start)
 
 
      INCLUDE 'yaccfor.inc'
 
 
 
      INTEGER Start
 
      INTEGER i
      INTEGER LENACT
 
 
      DO 100 i = Start + 1 , NPArs
 
 
         IF ( SVAl(Start)(LENACT(SVAl(Start)):LENACT(SVAl(Start)))
     &        .EQ.',' ) THEN
            SVAl(Start) = SVAl(Start)(1:LENACT(SVAl(Start)))//SVAl(i)
         ELSE
            SVAl(Start) = SVAl(Start)(1:LENACT(SVAl(Start)))
     &                    //','//SVAl(i)
         ENDIF
 100  CONTINUE
 
      SVAl(Start) = SVAl(Start)(1:LENACT(SVAl(Start)))//','
      NPArs = NPArs - (NPArs-Start)
      RETURN
 
 
      END
