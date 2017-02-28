C--- System dependent routines (MS-DOS version)
C--- Contains entry points for:
C CONC    (SUB)  Converts filename to system preferred case
C DIRPOS  (SUB)  Return the number of characters in directory spec
C FRELUN  (SUB)  Free up a logical unit number
C GETLUN  (SUB)  Get a free logical unit number
C LOCASE  (SUB)  Convert to lower case
C OPENWR  (SUB)  Open wrapup
C PLTTER         Toggle graphics/alpha mode on terminal
C PROMPT  (SUB)  Prompt
C PTEND   (SUB)  Add prefix to file names
C RDFORN  (SUB)  Read a foreign command, i.e., the command line
C SPAWN   (SUB)  Spawn to the operating system
C TRLOG   (SUB)  Translate logical name
C UPC     (SUB)  Convert to upper case
C*********
      SUBROUTINE CONC(CBUF)
      CHARACTER CBUF*(*)
C---
C Convert case of CBUF to default case of system.
C---
C CBUF    I/O  The file name to be converted.
C---
      CALL UPC(CBUF)
      RETURN
      END
C*********
      SUBROUTINE DIRPOS(CFILE, ISTA, IEND)
      CHARACTER CFILE*(*)
      INTEGER   ISTA, IEND
C---
C CFILE is a full filespec containing the disk and directory names.
C Upon return CFILE(ISTA:IEND) contains only the directory spec.
C---
C CFILE   I    The full file name
C ISTA      O  First valid character in directory spec, can be zero
C IEND      O  Last valid character in directory spec, can be zero
C---
C 1989-Jul-08 - [AFT]
C---
      INTEGER   LENACT
      INTEGER   I, LFILE
C---
      LFILE=LENACT(CFILE)
      IEND=0
      DO 190 I=1,LFILE
         IF(CFILE(I:I).EQ.',') IEND=I
         IF(CFILE(I:I).EQ.'\') IEND=I
  190 CONTINUE
      ISTA=MIN(1,IEND)
      RETURN
      END
C*********
      SUBROUTINE LOCASE(CBUF)
      CHARACTER CBUF*(*)
C---
C Converts CBUF to lower case.
C---
C CBUF    I/O  The character array to convert to lower case
C---
      INTEGER   I, ITMP
C---
      DO 120 I=1,LEN(CBUF)
         ITMP=ICHAR(CBUF(I:I))
         IF(ITMP.GT.64 .AND. ITMP.LT.91) CBUF(I:I)=CHAR(ITMP+32)
  120 CONTINUE
      RETURN
      END
C*********
      SUBROUTINE OPENWR(LUN,CFILE,CSTAT,CACC,CAR,LRECL,IREAD,IER)
      CHARACTER CFILE*(*), CSTAT*(*), CACC*(*), CAR*(*)
      INTEGER   LUN, LRECL, IREAD, IER
C---
C Wrapup routine for the Fortran OPEN statement.  This version will
C force the filename to be case insensitive, UNLESS the the first
C character of CFILE is a backslash.  If the first character is a
C backslash, then it should not be considered part of the filename
C on any system.
C   CFILE can be of the form 'disk,dir/sub,file' in which case, OPENWR
C will create 3 strings and call PTEND to construct the (system-
C dependent) file name.  This provides a system independent way
C to specify file names.
C---
C LUN     I    The logical unit number to be opened
C CFILE   I/O  The name of the file
C CSTAT   I    ='OLD', 'NEW' or 'UNKNOWN'
C CACC    I    ='D' for direct access, 'U' for unformatted, 'E' for append
C CAR     I    Carriage control, 'L' for 'LIST' or 'F'
C LRECL   I    Record length (required for direct access files)
C IREAD   I    <>0 for READONLY, SHARED
C IER       O  <>0 if file not opened
C---
C 1989-Aug-10 - Do not call CONC if file name starts with backslash [AFT]
C 1989-Jul-07 - Allow file names of form 'disk,dir/sub,file' [AFT]
C 1989-Feb-13 - Latest mod [AFT]
C---
      INTEGER   LENACT
C
      CHARACTER CTMP*255
      CHARACTER CDISK*255, CDIR*255
      CHARACTER CDUM
      INTEGER   IFILE, IOS, IS, KP, LFILE, LTMP
C---
   11 FORMAT(A)
C---
      LFILE=LENACT(CFILE)
C---
C Special treatment for terminal device.
C---
      IF(LFILE.EQ.0) THEN
C Needs additional check to prevent trashing a terminal
         IF(CAR(1:1).EQ.'L'.OR.CAR(1:1).EQ.'l') THEN
            OPEN(UNIT=LUN, FILE='CON', STATUS='OLD',
     :        IOSTAT=IOS)
            GOTO 140
         END IF
      END IF
C---
C Sort out file type.
C  0 = sequential access, unformatted         " " only
C  1 = direct access => unformatted           "D" + anything
C  2 = sequential access, unformatted         "U" only
C  3 = sequential access, formatted, append   "E" only
C  4 = sequential access, unformatted, append "UE" only
C
      CTMP=CACC
      CALL UPC(CTMP)
      IF(INDEX(CTMP,'D').NE.0) THEN
         IFILE=1
      ELSE IF(INDEX(CTMP,'U').NE.0 .AND. INDEX(CTMP,'E').NE.0) THEN
         IFILE=4
      ELSE IF(INDEX(CTMP,'U').NE.0) THEN
         IFILE=2
      ELSE IF(INDEX(CTMP,'E').NE.0) THEN
         IFILE=3
      ELSE
         IFILE=0
      END IF
C---
C Check for a "+" as the first character of the first name in which case
C force an append
      IF(CFILE(1:1) .EQ. '+') THEN
         IF(IFILE.EQ.0) IFILE=3
         IF(IFILE.EQ.2) IFILE=4
         CFILE=CFILE(2:)
         LFILE=LFILE-1
      END IF
C
      KP=0
      CALL ALF(CFILE, LFILE, KP, CTMP, LTMP)
      IS=1
      IF(LTMP.LT.LFILE) THEN
C Discovered a 'system independent' file specification.  Decode.
         CDISK=CTMP
         CALL ALF(CFILE, LFILE, KP, CTMP, LTMP)
         CDIR=CTMP
         CALL ALF(CFILE, LFILE, KP, CTMP, LTMP)
         CFILE=CTMP
         CALL PTEND(CDISK, CDIR, CFILE)
         LFILE=LENACT(CFILE)
      END IF
      CALL CONC(CFILE)
C---
  130 CONTINUE
      IF(IREAD.EQ.0) THEN
C- Read/Write
         IF(IFILE.EQ.1) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       ACCESS='DIRECT', RECL=LRECL, IOSTAT=IOS)
         ELSE IF(IFILE.EQ.2) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       FORM='UNFORMATTED',IOSTAT=IOS)
         ELSE IF(IFILE.EQ.3) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       IOSTAT=IOS)
         ELSE IF(IFILE.EQ.4) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       FORM='UNFORMATTED',IOSTAT=IOS)
         ELSE
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       IOSTAT=IOS)
         END IF
      ELSE
C- READONLY
         IF(IFILE.EQ.1) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       ACCESS='DIRECT', RECL=LRECL, MODE='READ', IOSTAT=IOS)
         ELSE IF(IFILE.EQ.2) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       FORM='UNFORMATTED', MODE='READ', IOSTAT=IOS)
         ELSE IF(IFILE.EQ.3) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       MODE='READ', IOSTAT=IOS)
         ELSE IF(IFILE.EQ.4) THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       FORM='UNFORMATTED', MODE='READ', IOSTAT=IOS)
         ELSE
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS=CSTAT,
     :       MODE='READ', IOSTAT=IOS)
         END IF
      END IF
C---
  140 IF(IOS.EQ.0) THEN
         IER=0
      ELSE
         IF(IOS.EQ.6415) THEN
            CTMP=CFILE(IS:LFILE)//' exists, reuse it?'
            CALL PROMPT(CTMP,0)
            READ(*,11,ERR=900,END=900) CDUM
            IF(CDUM.EQ.'Y' .OR. CDUM.EQ.'y') THEN
               OPEN(UNIT=LUN, FILE=CFILE(:LFILE), STATUS='UNKNOWN')
               CLOSE(UNIT=LUN, STATUS='DELETE')
               GOTO 130
            END IF
         END IF
      END IF
  900 CONTINUE
      IER=IOS
      RETURN
      END
C*********
      SUBROUTINE PLTTER(CTYPE)
      CHARACTER CTYPE*(*)
C---
C CTYPE='A' switches terminal into alpha mode.
C CTYPE='G' switches terminal into graphics mode.
C NOTE: Caps.
C---
      RETURN
      END
C*********
      SUBROUTINE PROMPT(CBUF, LBUF)
      CHARACTER CBUF*(*)
      INTEGER   LBUF
C---
C Write CBUF to the terminal leaving the cursor at the end of CBUF
C when finished.
C---
C CBUF    I    The prompt string
C LBUF    I    The number of valid characters in CBUF (can be zero)
C---
      INTEGER   LENACT
      INTEGER   LB
C---
      LB= LBUF
      IF(LB.EQ.0) LB= LENACT(CBUF)
      IF(LB.GT.0) THEN
         WRITE(*,111) CBUF(:LB)
  111    FORMAT(1X, A, ' ', $)
      END IF
      RETURN
      END
C*********
      SUBROUTINE PTEND(CDISK, CDIR, CFILE)
      CHARACTER CDISK*(*), CDIR*(*), CFILE*(*)
C---
C Prefix exTENsion.  Add 'prefix' to file name.
C---
C CDISK     I    The disk name
C CDIR      I    The directory name
C CFILE     I/O  The file name
C---
C  2-Aug-1988 - [AFT]
C---
      INTEGER   LENACT
      INTEGER   I, LDISK, LDIR, LFILE, LOUT
C---
      LDISK=LENACT(CDISK)+2
      LDIR =LENACT(CDIR)+1
      LFILE=LENACT(CFILE)
      LOUT=LDISK+LDIR+LFILE
      IF(LOUT.GT.LEN(CFILE)) THEN
         WRITE(*,*) 'PTEND error, not enough room to create filename.'
         RETURN
      END IF
C---
C- Move characters in CFILE
      DO 170 I=0,LFILE-1
         CFILE(LOUT-I:LOUT-I)=CFILE(LFILE-I:LFILE-I)
  170 CONTINUE
C---
C- Finish the job
      CFILE(1:LDISK)='\'//CDISK(:LDISK-2)//'\'
C---
C- Copy directory name changing '/' to '\'.
      DO 190 I=1,LDIR-1
         IF(CDIR(I:I).NE.'/') THEN
            CFILE(LDISK+I:LDISK+I)=CDIR(I:I)
         ELSE
            CFILE(LDISK+I:LDISK+I)='\'
         END IF
  190 CONTINUE
      CFILE(LDISK+LDIR:LDISK+LDIR)='\'
      RETURN
      END
C*********
      SUBROUTINE RDFORN(CBUF, LBUF)
      CHARACTER CBUF*(*)
      INTEGER   LBUF
C---
C Reads the entire command line.
C---
C CBUF      O  Data from the command line
C LBUF      O  The number of valid characters in CBUF (can be zero)
C---
      INTEGER   NARGS
C
      INTEGER   NARG, I, LENACT
      INTEGER*2 I2STAT
C---
      NARG= NARGS()
      CBUF=' '
      LBUF= 0
      IF(NARG.GT.0) THEN
         DO 190 I= 1, NARG
            CALL GETARG(I, CBUF(LBUF+1:), I2STAT)
C- Add a space between arguments
            LBUF= LENACT(CBUF)+1
  190    CONTINUE
C- Remove trailing space
         LBUF=LBUF-1
      END IF
      END

C*********
      SUBROUTINE SPAWN(CBUF, LBUF, IER)
	USE DFPORT
      CHARACTER CBUF*(*)
      INTEGER   LBUF
C---
C Spawn to operating system.  If LBUF=0 then this routine should
C spawn a shell and leave the user in the shell until the user logs
C out or exits the shell.  If LBUF<>0 then the system should only
C execute that one command and immediately return to the calling
C routine.
C---
C CBUF    I    The system command to execute
C LBUF    I    The number of valid characters in CBUF (can be zero)
C IER       O  =0 spawn was successful, <>0 otherwise
C---
C
      INTEGER   IER
C---
      IF(LBUF.GT.0) THEN
         LBUF=LBUF+1
         CBUF(LBUF:LBUF)=CHAR(0)
         IER=SYSTEM(CBUF(:LBUF))
      ELSE
         IER=SYSTEM('\COMMAND'//CHAR(0))
      END IF
      IF(IER.EQ.-1) WRITE(*,*) 'SPAWN--Error of some sort.'
      RETURN
      END
C*********

      SUBROUTINE TRLOG(CBUF, LBUF, CRET, LRET)
	USE DFPORT
	CHARACTER CBUF*(*), CRET*(*)
      INTEGER   LBUF, LRET

C---
C Translate the logical name CBUF(:LBUF) to return CRET(:LRET)
C---
C CBUF    I    The string to translate
C LBUF    I    The number of valid characters in CBUF (can be zero)
C CRET      O  The translated string
C LRET      O  The number of valid characters in CRET (can be zero)
C---
      CHARACTER DUM*64, CTMP*64
C
      INTEGER   I, LTMP
C---
      LTMP=LBUF+1
      CBUF(LTMP:LTMP)=CHAR(0)
      CALL GETENV(CBUF(:LTMP),CTMP)
      CRET=' '
      LRET=0
C---
C MS-Fortran Kludge, if the environment variable is undefined, then
C GETENV points to NULL (memory location zero).  I see no easy way to
C detect this condition in Fortran, therefore, I compare with an
C environment variable that noone would ever define and hence should
C always point at NULL.
	CALL GETENV('#$%^'//CHAR(0),DUM)
      IF(DUM.EQ.CTMP) THEN
         RETURN
      END IF
      DO 130 I=1,LEN(CTMP)
         IF(CTMP(I:I).EQ.CHAR(0)) GOTO 140
         LRET=LRET+1
         CRET(LRET:LRET)=CTMP(I:I)
  130 CONTINUE
  140 CONTINUE
      RETURN
      END
C*********
      SUBROUTINE UPC(CSTR)
      CHARACTER CSTR*(*)
C---
C Converts character array in CSTR to upper case.
C---
C CSTR    I/O  The string to convert to upper case
C---
      INTEGER   I, ICSTR
C---
      DO 120 I= 1, LEN(CSTR)
         ICSTR= ICHAR(CSTR(I:I))
         IF(ICSTR.GT.96 .AND. ICSTR.LT.123) CSTR(I:I)= CHAR(ICSTR-32)
  120 CONTINUE
      RETURN
      END

	SUBROUTINE GETTIM(timestr)
	USE DFPORT
	character(8) TIMESTR
	CALL TIME(TIMESTR)
	RETURN
	END

	SUBROUTINE GETDAT(datestr)
	USE DFPORT
	character(11) datestr
	CHARACTER DSTRING*24

	CALL FDATE(DSTRING)

	datestr(1:7) = DSTRING(10:11)//'-'//DSTRING(5:7)//'-'
	datestr(8:) =	DSTRING(21:24)
	RETURN 
	END