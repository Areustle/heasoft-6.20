      PROGRAM XHELP1
C---
C Program to invoke the XHELP facility subroutine GTXHLP
C Usage:
C XHELP [file] [topic]
C---
C 1995-Jan-5  - use $XANBIN,LIB directory for help files.
C 1990-Nov-13 - Restore ability to use non XANADU:[DOC] directories [AFT]
C 28-Jul-88 - [AFT]
C  5-Jul-85 - rashafer
C---
      INTEGER NFILES
      PARAMETER (NFILES=5)
      INTEGER   I, IER, LFILE, LTOPIC
      CHARACTER CTOPIC*80
      CHARACTER cfile*80, CTMP*80
      CHARACTER cshort(NFILES)*9
      CHARACTER CDIR(NFILES)*32
      CHARACTER CDISK*7
      LOGICAL QEXIST
      DATA cshort/ 'xspec', 'tutorial', 'xspecmod', 'plt', 'cod'/
      DATA CDIR  / 'doc',   'doc',  'doc','plot/qdp','plot/qdp'/
      DATA CDISK/'$XANADU'/
C---
      cfile = ' '
      CTOPIC = ' '
 100  CONTINUE
      CALL GTBUF(' ', IER)
      IF (IER.LT.0) GOTO 900
      CALL GTCHAR(cfile, LFILE)
      CALL GTREST(CTOPIC, LTOPIC)
C---
C If user enters 3 or more characters, try it as a file name.
      IF (LFILE.GE.3) THEN
C- Look in current directory first
         CTMP = cfile
         CALL XTEND(cfile, 'dhf')
         INQUIRE (FILE=cfile, EXIST=QEXIST)
         IF (QEXIST) GOTO 200
C---
C- Now look in the XANADU documentation directory
         cfile = CTMP
         CALL PTEND(CDISK, CDIR(1), cfile)
         CALL XTEND(cfile, 'dhf')
         INQUIRE (FILE=cfile, EXIST=QEXIST)
         IF (QEXIST) GOTO 200
C---
C- Finally consider user has type part of the short file name
         cfile = CTMP
         CALL LOCASE(cfile)
         DO I = 1, NFILES
            IF (cfile(:LFILE).EQ.cshort(I)(:LFILE)) THEN
               cfile=cshort(I)
               CALL PTEND(CDISK, CDIR(I), cfile)
               GOTO 200
            ENDIF
         END DO
      ENDIF
C---
C Finally search for a sub-topic of "XANADU,DOC,XHELP"
      cfile(LFILE+2:) = CTOPIC
      CTOPIC = cfile
      cfile  = 'xhelp'
      CALL PTEND(CDISK, CDIR(1), cfile)
C---
 200  CONTINUE
      CALL GTXHLP(1, cfile, CTOPIC)
      CLOSE (1)
C---
 900  CONTINUE
      CALL EDICOM('OFF',3)
      END
