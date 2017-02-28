C Program used to convert the PLT.HLP file into a PLT.TEX file.
C Not everything is converted, so user must make a final pass
C by hand.
C---
      INTEGER   LENACT
C
      CHARACTER CBUF*132, CSAV*132
      CHARACTER CTMP*132
      CHARACTER CFILE*64, CTOPIC*64, CSUB*64
      INTEGER   IHEAD, IER, IFIRST, IVERB
      INTEGER   LBUF, LFILE, LSAV, LSUB, LTOPIC
C---
   11 FORMAT(A)
C---
  100 CALL GTBUF('Input file:', IER)
      IF(IER.LT.0) GOTO 900
      CALL GTCHAR(CFILE, LFILE)
      CALL XTEND(CFILE, 'HLP')
      CALL OPENWR(1,CFILE,'OLD',' ',' ',0,1,IER)
      IF(IER.NE.0) GOTO 100
      CALL XTEND(CFILE, '.TEX')
      CALL OPENWR(2,CFILE,'NEW',' ','L',0,0,IER)
C---
      IHEAD=0
      IVERB=0
      IFIRST=0
  200 CONTINUE
      READ(1,11,ERR=900,END=900) CBUF
      LBUF=LENACT(CBUF)
C
      IF(CBUF(1:2).EQ.'1 ') THEN
C Top level topic starts the chapter.
         IHEAD=0
         CALL ENDVER(IVERB)
         CALL FIXER(CBUF, LBUF, CTMP)
         WRITE(2,11) '\chapter{'//CBUF(3:LBUF)//' Command Summary}'
      ELSE IF(CBUF(2:6).EQ.' exam') THEN
C An example subtopic, go to verbatim mode, this is because PLT examples
C are mostly verbatim examples.
         IHEAD=0
         WRITE(2,*)
         WRITE(2,11) '\medskip\noindent'
         IF(INDEX(CBUF(1:LBUF),'s').NE.0) THEN
            WRITE(2,11) '{\em Examples:}'
         ELSE
            WRITE(2,11) '{\em Example:}'
         END IF
         WRITE(2,11) '\begin{verbatim}'
         IVERB=1
      ELSE IF(CBUF(1:2).EQ.'2 ') THEN
C New topic, starts a new section
         IHEAD=0
         CTOPIC=CBUF(3:LBUF)
         LTOPIC=LBUF-2
         CALL ENDVER(IVERB)
         WRITE(2,*)
         CALL FIXER(CBUF, LBUF, CTMP)
         WRITE(2,11) '\section*{'//CBUF(3:LBUF)//'}'
         IFIRST=1
      ELSE IF(CBUF(1:2).EQ.'3 ') THEN
C New sub-topic
         IHEAD=0
         CSUB=CBUF(3:LBUF)
         LSUB=LBUF-2
         CALL ENDVER(IVERB)
         WRITE(2,11) '\medskip\begin{verbatim}'
         WRITE(2,11) CTOPIC(:LTOPIC)//' '//CBUF(3:LBUF)
         WRITE(2,11) '\end{verbatim}'
      ELSE IF(CBUF(1:2).EQ.'4 ') THEN
C New section
         IHEAD=0
         CALL ENDVER(IVERB)
         WRITE(2,11) '\medskip\begin{verbatim}'
         WRITE(2,11) CTOPIC(:LTOPIC)//' '//CSUB(:LSUB)//
     :         ' '//CBUF(3:LBUF)
         WRITE(2,11) '\end{verbatim}'
      ELSE IF(IHEAD.NE.0) THEN
         IF(CBUF(1:3).EQ.'   ') THEN
C Must be a option of the current command
            IF(IFIRST.EQ.0) WRITE(2,11) '\medskip'
            WRITE(2,11) '\begin{verbatim}'
            IVERB=1
            WRITE(2,11) CSAV(:LSAV)
            CALL ENDVER(IVERB)
         ELSE
C Potential heading just happened to be the topic name embedded in the
C text.
            CALL FIXER(CSAV, LSAV, CTMP)
            WRITE(2,11) CSAV(:LSAV)
         END IF
         CALL FIXER(CBUF, LBUF, CTMP)
         WRITE(2,11) CBUF(:LBUF)
         IHEAD=0
         IFIRST=0
      ELSE
         IF(CBUF(:LTOPIC).EQ.CTOPIC(:LTOPIC)) THEN
C Potential new heading.  Most PLT commands are followed by a single
C line showing the syntax of the command.  A syntax line begins with the
C topic name and is followed by a blank line (not 100 percent but good
C enough).  Therefore set flag, to examine the next line.
            CSAV=CBUF(:LBUF)
            LSAV=LBUF
            IHEAD=1
         ELSE
C Straight copy of the text, converting to LaTeX syntax where possible.
            IF(IVERB.EQ.0) CALL FIXER(CBUF, LBUF, CTMP)
            WRITE(2,11) CBUF(:LBUF)
         END IF
      END IF
      GOTO 200
C---
900   CONTINUE
      CALL ENDVER(IVERB)
      CLOSE(UNIT=2)
      END
C*********
      SUBROUTINE ENDVER(IVERB)
      INTEGER   IVERB
C---
   11 FORMAT(A)
C---
      IF(IVERB.NE.0) THEN
         WRITE(2,11) '\end{verbatim}'
         IVERB=0
      END IF
      RETURN
      END
C*********
      SUBROUTINE FIXER(CBUF, LBUF, CTMP)
      CHARACTER CBUF*(*), CTMP*(*)
      INTEGER   LBUF
C---
C Searches for common strings in the .HLP file and replaces them with
C things that work in LaTeX.  
C----
      INTEGER   MXCH
      PARAMETER (MXCH=13)
      INTEGER   LENACT
C
      CHARACTER COLD(MXCH)*6, CNEW(MXCH)*12, CREP*12
      INTEGER   LOLD(MXCH)
      INTEGER   IQUOTE, IP, J, LO, LREP, LTMP
      DATA LOLD/  1,    1,    1,    1,      5,           5,
     :               4,     2,     2,     3,     1,    1,     2/
      DATA COLD/  '#', '_',  '^',  '$', 'Delta',     'chi^2',
     :          'i.e.',  'x-',  'y-',  'x,y',  '<',  '>',   '>='/
      DATA CNEW/ '\verb@#@', '\_', '\^', '\$', '$\Delta$', '$\chi^2$',
     :    '{\it i.e.}','$x$-','$y$-','$x,y$','$<$','$>$','$\geq$'/
C---
      LTMP=0
      IQUOTE=0
      IP=0
  110 CONTINUE
         IP=IP+1
         CREP=CBUF(IP:IP)
         LREP=1
         IF(CBUF(IP:IP).EQ.'"') THEN
            IF(IQUOTE.EQ.0) THEN
               IQUOTE=1
               CREP='{\tt '
               LREP=5
            ELSE
               IQUOTE=0
               CREP='}'
               LREP=1
            END IF
         ELSE
            IF(IQUOTE.EQ.0) THEN
C Don't alter text inside a \verb@...@ string
               DO 160 J=1,MXCH
                  LO=LOLD(J)
                  IF(CBUF(IP:IP+LO-1).EQ.COLD(J)(1:LO)) THEN
                     CREP=CNEW(J)
                     LREP=LENACT(CREP)
                     IP=IP+LO-1
                  END IF
  160          CONTINUE
            ELSE
C Change ' ' to '~' inside a \verb@...@ string
               IF(CBUF(IP:IP).EQ.' ') THEN
                  CREP='~'
                  LREP=1
               ELSE IF(CBUF(IP:IP).EQ.'#') THEN
                  CREP='\#'
                  LREP=2
               ELSE IF(CBUF(IP:IP).EQ.'$') THEN
                  CREP='\$'
                  LREP=2
               END IF
            END IF
         END IF
         CTMP(LTMP+1:LTMP+LREP)=CREP(:LREP)
         LTMP=LTMP+LREP
      IF(IP.LT.LBUF) GOTO 110
      CBUF=CTMP
      LBUF=LTMP
      RETURN
      END
