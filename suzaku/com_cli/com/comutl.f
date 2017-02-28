C   11/11/86 708161309  MEMBER NAME  COMUTL   (FORT)     M  FORTRAN
C
C History:
C     17-Jul-1998 Y.ISHISAKI, add ANS0 arg (do not upcase) for COMKEY
C     22-Jul-2005 Y.ISHISAKI, rmove using PRCMOD() in INICOM, always BATCH=F
C     09-Mar-2005 Y.ISHISAKI, use CLexit() instead of Exit() in COMTCH()
C
C+++++ THIS IS COMUTL.FOR
C
       Subroutine COMKEY(QUEST,ANS0,ANSWER)
C
       Implicit None
C
       Include 'comode.inc'
C
       CHARACTER*(*) QUEST,ANS0,ANSWER
       INTEGER*4 LABEL
C
C..... GET A KEYWORD
C
       IF (ICOMER.LT.0) GOTO LABEL
80     ASSIGN 80 TO LABEL
       CALL GETTXT(QUEST,ANS0)
       IF (ICOMER.NE.1) RETURN
       ANSWER = ANS0
       CALL CLstrupc( LEN(ANSWER),ANSWER )
       ICOMER = 1
       IF (ANSWER.EQ.'HELP') ANSWER = '?'
       END
C
C
       Subroutine GETTXT(QUEST,ANSWER)
C
       Implicit None
C
       CHARACTER*(*) QUEST, ANSWER
C
       Include 'comode.inc'
C
C..... ASK A QUESTION
C
       IF (.NOT.RLTIME) ICOMER = 1
       CALL TXTRD(QUEST,ANSWER)
       END
C
C
       Subroutine COMTCH(NAMES,NVAL,RESP,ITEM)
C
       Implicit None
C
       INTEGER*4 NVAL,ITEM,IV
       CHARACTER*(*) NAMES(NVAL),RESP
       include 'comode.inc'
C
C..... LOOK IF KEYWORD IS IN THE LIST
C
       CALL AMBRD(NAMES,NVAL,RESP,ITEM)
       IF (ITEM.LE.0 .AND. RESP(1:1).EQ.'#') THEN
         CALL CLatoi(RESP(2:),IV)
         IF (IV.GT.0 .AND. IV.LE.NVAL) ITEM = IV
       END IF
       IF (ITEM.EQ.0) WRITE (LUNCOM,*) 'COM: No Such Answer: ', RESP
       IF (ITEM.LT.0) WRITE (LUNCOM,*) 'COM: Ambiguous Answer: ', RESP
       IF (ITEM.LE.0 .AND. BATCH)  THEN
         PRINT *, '%COM-F, COM fatal error. Execution terminated.'
         CALL CLEXIT(1)
       END IF
       END
C
C
       Subroutine INICOM ( prompt )
C
       Implicit None
C
       Entry Init_Com_Talk( prompt )
C common
       Include 'comode.inc'
C input
       Character*(*) PROMPT
C function
       External  COM_sigsub
C begin
C ... define prompt
       COMPRM = PROMPT
C ... define logical unit
       LUNCOM = 6
       BATCH = .FALSE.
C ... clear CLI buffer
       CALL LINRD(1,1)
C      ... Establish Ctrl-C handler
cccc       Call CLsigF(2,COM_sigsub)
       Return
       End
C
C
       Subroutine COM_sigsub( signum )
C
       Implicit None
C input
       Integer  signum
C common
       Include 'comode.inc'
C begin
       Icomer = 2
       Return
       End
C
