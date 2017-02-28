**==gtbufc.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
* fortran <=> c interface between gtbuf and lex/yacc for gtcom
*
      FUNCTION GTBUFC(Line,Nbytes)
      IMPLICIT NONE
 
      INTEGER*4 GTBUFC
 
*      BYTE Line(1000)
      character(1000) Line
 
 
      character(1000) inline
 
      INCLUDE 'yaccfor.inc'
      INCLUDE 'tbl.inc'
 
      INTEGER*4 ier
      INTEGER*4 lbuf
      INTEGER*4 Nbytes
      INTEGER*4 LENACT
      INTEGER i
 
      LOGICAL*4 first
 
      SAVE first
 
      DATA first/.TRUE./
 
 
      IF ( first .AND. TBLstandalone ) THEN
         first = .FALSE.
         i = LENACT(TBLstandalonecmd)
         inline = TBLstandalonecmd(1:i)//CHAR(10)
         lbuf = i + 1
         Line = inline
         GTBUFC = lbuf
         RETURN
      ENDIF
      lbuf = 0
      IF ( first ) THEN
         first = .FALSE.
         CALL RDFORN(inline,lbuf)
      ENDIF
      IF ( lbuf.EQ.0 ) THEN
 
         CALL GTBUF(GTPrompt,ier)
 
         IF ( ier.NE.0 ) THEN
            GTBUFC = 4
            Line = 'EOF'//CHAR(10)
            RETURN
         ENDIF
 
         CALL GTREST(inline,lbuf)
      ENDIF
 
      inline = inline(1:LENACT(inline))//CHAR(10)
      lbuf = lbuf + 1
*      DO 100 ier = 1 , Nbytes
*         Line(ier) = 0
* 100  CONTINUE
 
*      DO 200 ier = 1 , MIN(lbuf,Nbytes)
*         Line(ier) = ICHAR(inline(ier:ier))
* 200  CONTINUE
      Line = inline
      GTBUFC = lbuf
      RETURN
      END
