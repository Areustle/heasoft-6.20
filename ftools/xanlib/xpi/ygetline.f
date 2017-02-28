**==ygetline.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Returns the whole line
*
      SUBROUTINE YGETLINE(Tline)
 
      IMPLICIT NONE
 
      INCLUDE 'yaccfor.inc'
 
      CHARACTER*(*) Tline
 
 
*      Sline = Scom
*
*      tmp = Scom
*      CALL UPC(tmp)
*
*      IF ( tmp.EQ.'CPD' ) THEN
*         Tline = Scom(1:LENACT(Scom)) // ' /' // Spars(1)
*         RETURN
*      ENDIF
*
*      IF ( Npars.GT.0 ) THEN
*         DO 50 i = 1 , Npars
*            IF ( Spars(i).NE.' ' ) THEN
*               Sline = Sline(1:LENACT(Sline)) // '/' // Spars(i)
*            ENDIF
*            IF ( Sval(i).NE.' ' ) THEN
*               IF ( Spars(i).NE.' ' ) THEN
*                  Sline = Sline(1:LENACT(Sline)) // '=' // Sval(i)
*               ELSE
*                  Sline = Sline(1:LENACT(Sline)+1) // Sval(i)
*               ENDIF
*            ENDIF
* 50      CONTINUE
*      ENDIF
 
 
      Tline = SLIne
 
      RETURN
      END
