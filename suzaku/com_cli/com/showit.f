C   11/11/86 611290756  MEMBER NAME  SHOWIT   (FORT)     M  FORTRAN
C
C History:
C     17-Jul-1998 Y.Ishisaki, COMKEY(PROMPT,RESP) -> COMKEY(PROMPT,RESP,RESP)
C
C+++++ THIS IS SHOWIT.FOR
C
       SUBROUTINE SHOWIT(QUEST,MVAL,NAMES,HELP)
c      include 'comode.inc'
       include 'comode.inc'
       CHARACTER*(*) NAMES(*),HELP(*),QUEST
       CHARACTER*50 RESP
C
C..... SHOW A TABLE OF OPTIONS
C
       NVAL = ABS(MVAL)
       IF (ICOMER.LT.0) GOTO LABEL
       IF (LUNCOM.EQ.0) CALL INICOM('BUG')
 3     RESP = ' '
       NLEN = LEN(NAMES(1))
 1     CONTINUE
C      IF ( .NOT. BATCH ) CALL CM$CLR
       WRITE (LUNCOM,100) COMPRM,QUEST
 100   FORMAT (' '/' ',A,':  ',A,/' ')
       IF (RESP.EQ.'?') THEN
         CALL COMHLP(NVAL,NAMES,HELP)
       END IF
       IF (RESP.NE.'?' .OR. HELP(1)(1:1).EQ.'$') THEN
         INC = MAX(1,MIN(9,72/(NLEN+2)))
         WRITE (LUNCOM,105) (NAMES(K),K=1,NVAL)
 105     FORMAT (4(1X,A,1X))
         IF (MVAL.LT.0) RETURN
       END IF
       CALL LINRD(1,1)
 2     WRITE (LUNCOM,102) COMPRM
 102   FORMAT (' '/' ',A,': Press ENTER to CONTINUE')
 80    ASSIGN 80 TO LABEL
       CALL COMKEY('?'//COMPRM//'> ',RESP,RESP)
       IF (ICOMER.NE.1) RETURN
       IF (RESP.EQ.' ' .OR. RESP.EQ.'OK') RETURN
       IF (RESP.EQ.'?') GOTO 1
       GOTO 2
       END
