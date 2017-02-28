c
c
c ---------------------------------------------
      subroutine XSL_OPEN(LUN,CFILE,CSTAT,CACC,CAR,LRECL,IREAD,IER)
c ---------------------------------------------
      CHARACTER CFILE*(*), CSTAT*(*), CACC*(*), CAR*(*)
      INTEGER   LUN, LRECL, IREAD, IER
C---
C Wrapup routine for the Fortran OPEN statement.  This is taken from 
C the xanlib routine, but case conversion has been removed, and it
C no longer returns the full path.
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
      INTEGER   LENACT
C
      CHARACTER(255) CTMP
      CHARACTER(90) CDISK, CDIR
      CHARACTER CDUM
      INTEGER  IFILE, IOS, IS, KP, LFILE, LTMP
C---
   11 FORMAT(A)
C---
      IS = 1
      IOS = 0
      IFILE = 0
C---
      LFILE=LENACT(CFILE)
C---
C Special treatment for terminal device.
C---
      IF(LFILE.EQ.0) THEN
C Needs additional check to prevent trashing a terminal
         IF(CAR(1:1).EQ.'L'.OR.CAR(1:1).EQ.'l') THEN
            OPEN(UNIT=LUN, FILE='/dev/tty', STATUS='OLD',
     :        IOSTAT=IOS)
            GOTO 140
         END IF
      END IF
C---
C Sort out file type.
C  0 = sequential access, formatted         " " only
C  1 = direct access => unformatted           "D" + anything
C  2 = sequential access, unformatted         "U" only
C  3 = sequential access, formatted, append   "E" only
C  4 = sequential access, unformatted, append "UE" only
C  5 = direct access, formatted               "DF' only
C
C NB I don't know how to do 3 and 4 under Ultrix so they
c    default to 5 and 2 respectively.

      CTMP=CACC
      CALL UPC(CTMP)
      IF(INDEX(CTMP,'D').NE.0 .AND. INDEX(CTMP,'F').EQ.0) THEN
         IFILE=1
      ELSE IF(INDEX(CTMP,'D').NE.0 .AND. INDEX(CTMP,'F').NE.0) THEN
         IFILE=5
      ELSE IF(INDEX(CTMP,'U').NE.0 .AND. INDEX(CTMP,'E').NE.0) THEN
         IFILE=4
      ELSE IF(INDEX(CTMP,'U').NE.0) THEN
         IFILE=2
      ELSE IF(INDEX(CTMP,'E').NE.0) THEN
         IFILE=3
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
      IF(LTMP.LT.LFILE) THEN
C Discovered a 'system independent' file specification.  Decode.
         CDISK=CTMP(:MIN(LENACT(CTMP),LEN(CDISK)))
         CALL ALF(CFILE, LFILE, KP, CTMP, LTMP)
         CDIR=CTMP(:MIN(LENACT(CTMP),LEN(CDIR)))
         CALL ALF(CFILE, LFILE, KP, CTMP, LTMP)
         CFILE=CTMP
         CALL PTEND(CDISK, CDIR, CFILE)
         LFILE=LENACT(CFILE)
      END IF
C---
  130 CONTINUE

C Explicitly supply the status, this is necessary for OSF/1 Fortran.

      CTMP = CSTAT
      CALL UPC(CTMP)
      IF(IFILE.EQ.1) THEN
         IF (CTMP(1:3) .EQ. 'OLD') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='old',
     :       ACCESS='DIRECT', RECL=LRECL/4,
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:3) .EQ. 'NEW') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='new',
     :       ACCESS='DIRECT', RECL=LRECL/4,
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:7) .EQ. 'UNKNOWN') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='unknown',
     :       ACCESS='DIRECT', RECL=LRECL/4,
     :       IOSTAT=IOS)
         ENDIF
      ELSE IF(IFILE.EQ.2) THEN
         IF (CTMP(1:3) .EQ. 'OLD') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='old',
     :       FORM='UNFORMATTED',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:3) .EQ. 'NEW') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='new',
     :       FORM='UNFORMATTED',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:7) .EQ. 'UNKNOWN') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='unknown',
     :       FORM='UNFORMATTED',
     :       IOSTAT=IOS)
         ENDIF
      ELSE IF(IFILE.EQ.3) THEN
         IF (CTMP(1:3) .EQ. 'OLD') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='old',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:3) .EQ. 'NEW') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='new',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:7) .EQ. 'UNKNOWN') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='unknown',
     :       IOSTAT=IOS)
         ENDIF
      ELSE IF(IFILE.EQ.4) THEN
         IF (CTMP(1:3) .EQ. 'OLD') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='old',
     :       FORM='UNFORMATTED',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:3) .EQ. 'NEW') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='new',
     :       FORM='UNFORMATTED',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:7) .EQ. 'UNKNOWN') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='unknown',
     :       FORM='UNFORMATTED',
     :       IOSTAT=IOS)
         ENDIF
      ELSE IF(IFILE.EQ.5) THEN
         IF (CTMP(1:3) .EQ. 'OLD') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='old',
     :       ACCESS='DIRECT', FORM='FORMATTED', RECL=LRECL,
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:3) .EQ. 'NEW') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='new',
     :       ACCESS='DIRECT', FORM='FORMATTED', RECL=LRECL,
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:7) .EQ. 'UNKNOWN') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='unknown',
     :       ACCESS='DIRECT', FORM='FORMATTED', RECL=LRECL,
     :       IOSTAT=IOS)
         ENDIF
      ELSE
         IF (CTMP(1:3) .EQ. 'OLD') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='old',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:3) .EQ. 'NEW') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='new',
     :       IOSTAT=IOS)
         ELSEIF (CTMP(1:7) .EQ. 'UNKNOWN') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='unknown',
     :       IOSTAT=IOS)
         ENDIF
      END IF
C


 140  IF(IOS.EQ.10) THEN
C Special treatment under UNIX, to allow user to overwrite an
C existing file

         CALL EDICOM('SAV', 3)
         ctmp = CFILE(IS:LFILE)//' exists, overwrite it?'
         CALL PROMPT(ctmp,0)
         READ(*,11,ERR=900,END=900) CDUM
         CALL EDICOM('RES', 3)
         IF(CDUM.EQ.'Y' .OR. CDUM.EQ.'y') THEN
            OPEN(UNIT=LUN, FILE=CFILE(IS:LFILE), STATUS='UNKNOWN')
            CLOSE(UNIT=LUN, STATUS='DELETE')
            GOTO 130
         END IF
      END IF
 900  CONTINUE
      IER=IOS
c      IF(IER.ne.0) THEN
c         write(*,'(a,a)') ' Failed to open ', 
c     &                       CFILE(:LENACT(CFILE))
c      write(*,'(a,i3)') ' iostat = ', ier
c      ENDIF
      RETURN
      END

c
c
c
c ---------------------------------------------
      subroutine XSL_WRTCF(ilun,command,iflag)
c ---------------------------------------------
c Writes a command to the command file
c     Jim Ingham, April 1993
      implicit none

      character*(*) command
      character(4096) str1,errout
      integer ilun,iflag,len1,len2,i
      character(255) cmdfil, lstfil,errfil,wrkdir
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir
      integer LENACT

c get rid of leading blanks.
      call RMVLBK(command)

c length = 0 means don't add the error stuff.  Necessary because the
c error stuff messes up PGPLOT on UNIX systems.

      IF(iflag.eq.0) THEN
         write(ilun,'(a)') command
         return
      ENDIF

c Build up the errorfile, in/tmp
c This is B. Oneel's suggestion, so that we can catch disk full errors.
      errout = '/tmp/$$'//errfil(:LENACT(errfil))
c Now append the redirection strings

      len1=LENACT(command)

c Get the command name
      do i=1,len1
         if(command(i:i).eq.' ') then
            str1 =  'CMDNAME='//command(1:i-1)
            goto 100
         endif
      enddo
 100  write(ilun,53) str1(1:i+7)

      len2 = LENACT(errout)
      command = command(1:len1) //' 2> '//errout(1:len2)
      len1 = LENACT(command)

      write(ilun,53)command(1:len1)

c Now write in the logic that checks the errout, and exits with a
c non-zero error status if the errout has anything in it.
c This is written for the Bourne shell!!
      

c Try checking $? as well...
      str1 = 'err_return=$?'
      write(ilun,53) str1(:LENACT(str1))

      str1 = 'err_file=`cat '//errout(1:len2)//
     &       '| grep -v ld.so | wc -l `'
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)

      str1 = 'if [ $err_file -ne 0 -o $err_return -ne 0 ] '
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)

      str1 = '       then'
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)

      str1 = '           cat '//errout(1:len2)
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)

c      str1 = '           echo ''Failure for '' $CMDNAME'
c      len1 = LENACT(str1)
c      write(ilun,53) str1(1:len1)

      str1 = '           rm -f '//errout(:len2)
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)


      str1 = '           exit 1'
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)

      str1 = 'fi'
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)
      
      str1 = 'rm -f '//errout(:len2)
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)
      
 53   format(a)


      return
      end

