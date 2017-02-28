C   15/04/87 704151956  MEMBER NAME  CRANGE   (FORT)     M  FORTRAN
C+
C File : CRANGE
C Description : Change Range
C
C Author : Ryosuke Itoh, Univ. of Tokyo
C Date : 15 - APR - 1987
C
C History:
C     17-Jul-1998 Y.Ishisaki, COMKEY(PROMPT,RESP) -> COMKEY(PROMPT,RESP,RESP)
C-
C
       SUBROUTINE CRANGE ( QUEST,NVAL,NAMES,HELP,VARTYP,ARRAY )
C
C ARGUMNETS
C
       INTEGER   *  4  NVAL
       CHARACTER * (*) NAMES(NVAL), HELP(NVAL), QUEST
       CHARACTER * (*) VARTYP
       INTEGER   *  4  ARRAY(2,NVAL)
C
C COMMON
C
       include 'comode.inc' ! 't#tp.com.fort'
C
C VARIABLES
C
       CHARACTER * 16   RNAMES ( MAXENT )
       INTEGER*4        ISTAT, I, J
       INTEGER*4        IFLQ, IFCR
       CHARACTER*64     RESP
       LOGICAL*1        MODIF
       INTEGER * 4      IBUF ( 2, MAXENT )
       CHARACTER * 20   TEXT
       INTEGER * 4      LTEXT
       CHARACTER * 5    RUNFRM
C
C MAIN
C
       IF (ICOMER.LT.0) GOTO LABEL
       IF (LUNCOM.EQ.0) CALL INICOM('BUG')
       IF (VARTYP .NE. 'I' ) THEN
         WRITE (LUNCOM,104) VARTYP
 104     FORMAT (' COM: ILLEGAL VARIABLE TYPE:',A1,' IN CRANGE')
         RETURN
       END IF
C
C..... SET THE INITIAL VALUES
C
 3     RESP = ' '
       MODIF = .FALSE.
       DO 10 I = 1, NVAL
         RNAMES(I) = NAMES(I)
         DO 20 J = 1, 2
           IBUF (J,I) = ARRAY ( J, I )
20       CONTINUE
10     CONTINUE
       RNAMES(NVAL+1)='FROM'
       RNAMES(NVAL+2)='TO'
C
C..... TYPE THE OPTIONS
C
 1     CONTINUE
C       IF ( .NOT. BATCH ) CALL CM$CLR
       WRITE (LUNCOM,110) COMPRM,QUEST
 110   FORMAT (' '/' ',A,':  ',A,/' ')
       IF (RESP.EQ.'?') THEN
         CALL COMHLP(NVAL,NAMES,HELP)
       END IF
       IF (VARTYP.EQ.'I') THEN
         DO 30 J = 1, NVAL
           WRITE (LUNCOM,*) NAMES(J),' ',
     &     IBUF(1,J), ' ', IBUF(2,J)
30       CONTINUE
       ENDIF
C
C..... GET A KEYWORD
C
       CALL LINRD(1,1)
 2     CALL FLARD(IFLQ,IFCR)
       IF (IFLQ.NE.0) WRITE(LUNCOM,222)
222    FORMAT (' '/' Syntax -> KEY FROM XXXX TO XXXX' )
       IF (.NOT.MODIF) THEN
         IF (IFLQ.NE.0) WRITE (LUNCOM,122) COMPRM
 122     FORMAT (' '/' ',A,': Press <ENTER> to Accept')
 80      ASSIGN 80 TO LABEL
         CALL COMKEY('?'//COMPRM//'> ',RESP,RESP)
         IF (ICOMER.NE.1) RETURN
         IF (RESP.EQ.' ' .OR. RESP.EQ.'OK' .OR. RESP.EQ.'ok') THEN
           CALL LINRD(1,1)
           RETURN
         END IF
       ELSE
         IF (IFLQ.NE.0) WRITE (LUNCOM,123) COMPRM
 123     FORMAT (' '/' ',A,': Type OK to Accpet')
 81      ASSIGN 81 TO LABEL
         CALL COMKEY('?'//COMPRM//'> ',RESP,RESP)
         IF (ICOMER.GT.1) GOTO 3
         IF (ICOMER.NE.1) RETURN
         IF (RESP.EQ.'OK' .OR. RESP.EQ.'ok') THEN
           DO 601 I = 1, NVAL
             DO 602 J = 1, 2
               ARRAY ( J, I ) = IBUF ( J, I )
602          CONTINUE
601        CONTINUE
           CALL LINRD(1,1)
           RETURN
         END IF
       END IF
       IF (RESP.EQ.' ' .OR. RESP.EQ.'?') GOTO 1
       CALL FLGRD(0,0)
       NLEN = LEN(NAMES(1))
       CALL COMTCH(RNAMES,NVAL+2,RESP(1:NLEN),J)
       IF (J.LE.0) THEN
         CALL LINRD(1,1)
         IF (ICOMER.EQ.3)  RETURN
         GOTO 2
       ELSE IF ( J .LE. NVAL ) THEN
         IDENT = J
       END IF
C
C..... GET THE VALUE
C
       ISTAT = 0
       IF ( J .EQ. NVAL+1 ) THEN
         IF ( IDENT .EQ. 0 ) GOTO 99
         CALL GETTXT ( '?I*4', TEXT )
         IF ( TEXT .EQ. ' ' ) THEN
           ISTAT = 2
         ELSE
           LTEXT = LKBRD ( TEXT, 0 )
           WRITE ( RUNFRM, '(A,I2,A)' ) '(I',LTEXT,')'
           LTEXT = LKBRD ( RUNFRM, 0 )
           READ ( TEXT, RUNFRM(1:LTEXT), ERR=99 ) IBUF(1,IDENT)
         END IF
       ELSE IF ( J .EQ. NVAL+2 ) THEN
         IF ( IDENT .EQ. 0 ) GOTO 99
         CALL GETTXT ( '?I*4', TEXT )
         IF ( TEXT .EQ. ' ' ) THEN
           ISTAT = 2
         ELSE
           LTEXT = LKBRD ( TEXT, 0 )
           WRITE ( RUNFRM, '(A,I2,A)' ) '(I',LTEXT,')'
           LTEXT = LKBRD ( RUNFRM, 0 )
           READ ( TEXT, RUNFRM(1:LTEXT), ERR=99 ) IBUF(2,IDENT)
         END IF
       END IF
       IF (ISTAT.NE.0) THEN
         CALL LINRD(1,1)
         WRITE (LUNCOM,*) 'COM: ERROR DECODING ',RESP(1:NLEN)
         GOTO 2
       END IF
       MODIF = .TRUE.
       GOTO 2
C
99     CONTINUE
       CALL LINRD(1,1)
       WRITE (LUNCOM,*) 'COM: ERROR DECODING ',RESP(1:NLEN)
       GOTO 2
       END
