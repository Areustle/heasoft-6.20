**==xnxtrd.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Xparse replacement using yacc/lex
*
 
      SUBROUTINE XNXTRD1(Prom,Tstring,Zparse,Ierr,*)
 
      IMPLICIT NONE
*
      INTEGER YYPARSEF
 
 
      INCLUDE 'tbl.inc'
      INCLUDE 'yaccfor.inc'
 
      CHARACTER*(*) Prom , Tstring
      INTEGER*4 Ierr
      INTEGER*4 Zparse
      INTEGER ltmp
      character(100) ctmp
      INTEGER*4 LENACT
      LOGICAL*4 found
      INTEGER*4 i , j
      character(80) str1 , str2
 
      Ierr = 0
      Zparse = 0
      Tstring = ' '
      DO WHILE ( Tstring.EQ.' ' )
 
         Tstring = ' '
 
C         CALL PROMPT(Prom,LENACT(Prom))
         GTPrompt = Prom
 
         CALL YINIT
         IF ( (YYPARSEF().EQ.0) ) THEN
            CALL YPRINT('xnxtrd 1')
         ELSE
            Ierr = 1
            RETURN 1
         ENDIF
         found = .FALSE.
         DO 50 i = 1 , NPArs
            IF ( SPArs(i).EQ.'?' ) THEN
               found = .TRUE.
               str1 = SCOm
               DO 10 j = 1 , TBLkcnt
                  str2 = TBLkname(j)
                  CALL UPC(str2)
                  IF ( str1.EQ.str2 )
     &                 CALL XWRITE(' '//TBLkparm(j)(1:LENACT(TBLkparm(j)
     &                 )),5)
 10            CONTINUE
            ENDIF
 50      CONTINUE
         IF ( .NOT.found ) CALL YGETLINE(Tstring)
      ENDDO
 
      ltmp = 0
      ctmp = Prom(1:LEN(Prom))//' '//Tstring(1:LENACT(Tstring))
C      CALL LOGGER(5,rbuf,0,ctmp,ltmp)
 
*      tblgood = YSTCMD()
 
      RETURN
      END
**==xpiparstr.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
 
*
* xpi parse a string
*
 
      SUBROUTINE XPIPARSTR(Tstring)
 
      IMPLICIT NONE
*
      INTEGER XPIPARSESTR
 
 
 
      CHARACTER*(*) Tstring
 
      CALL YINIT
      IF ( (XPIPARSESTR(Tstring).EQ.0) ) CALL YPRINT ('xpiparstr 1')
 
      CALL YSTCMD()
      CALL YPRINT('xpiparstr 2')
 
      RETURN
      END
 
