      SUBROUTINE SETLOG(instrg, lenn, default_file, seshead)
      CHARACTER instrg*(*)
      INTEGER lenn
      CHARACTER*(*) default_file
      CHARACTER*(*) seshead
C---
C subroutine to open a log file.  if the file name is none
C then the file is closed.  also sets the logging of command file info.
C---
C instrg    i    parse string
C lenn      i/o  parse position string
C---
C 7 aug 1984 - rashafer
C---
c generalised NEW Oct 90
c
c     default_file   c*(*)    i: default log file
c     seshead        c*(*)    i: session header
C---
      LOGICAL*4 qpart, xqmtch
      character(255) filename
      INTEGER*4 ios
      INTEGER*4 lenact
      INTEGER*4 nret, iflag, inqunit, ierr, lseshd
      LOGICAL*4 opnlog
      LOGICAL*4 exist
      INTEGER*4 logunit
C
      character(35) descr(2)
      INTEGER*4 chatvl(2)
      DATA descr/'chattyness level to log commands', 
     &     'increment for indirect command files'/
      DATA opnlog/.FALSE./
      DATA logunit/0/

c MJT 6Aug96 initializing ierr
      ierr=0

      filename=default_file 
      CALL xgtstr(instrg,lenn,1,
     &            'log file name (or ''none'' to disable)',1,filename,
     &            nret,iflag,-1)
      IF ( nret.LE.0 ) THEN
C       ** if the file is already open, or if an eof occured during the
C       ** handling of the '?', return, otherwise use the current filename
        IF ( (opnlog) .OR. (iflag.LT.0) ) RETURN
      END IF
      IF ( .NOT.(xqmtch('none',filename,qpart)) ) THEN
        CALL xtend(filename,'log')
        IF ( opnlog ) THEN
          INQUIRE (FILE=filename,EXIST=exist,NUMBER=inqunit,IOSTAT=ios)
C         ** already connected to the unit, so keep it as is
          IF ( exist .AND. (inqunit.EQ.logunit) ) GO TO 100
        ELSE
          opnlog = .TRUE.
        END IF
        lseshd = lenact(seshead)
        CALL xopnlg(filename,.FALSE.,seshead(:lseshd),.FALSE.,logunit,
     &              ierr)
        IF ( ierr.NE.0 ) THEN
          write(*,*) ' Unable to open the log file `'//filename
     &             (:lenact(filename))//''''
          opnlog = .FALSE.
        END IF
      ELSE IF ( opnlog ) THEN
        CALL xclslg('keep')
        logunit = 0
        opnlog = .FALSE.
      END IF
C     ** come from when the file was already connected
 100  CONTINUE
      CALL xgtint(instrg,lenn,2,descr,2,chatvl,nret,iflag,-1)
C     call xcmdlg(chatvl(1),chatvl(2))
      RETURN
      END
