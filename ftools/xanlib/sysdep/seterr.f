      SUBROUTINE SETERR(instrg, parse, default_file, seshead)
      CHARACTER instrg*(*)
      INTEGER parse
      CHARACTER*(*) default_file
      CHARACTER*(*) seshead
C---
C subroutine to open an error log file.  if the file name is none
C then the file is closed.  also sets the logging of command file info.
C---
C 21 Nov 1991 - Andy Pollock's adaptation of SETLOG
C---
C---
C instrg    i    parse string
C parse     i/o  parse position string
c default_file   c*(*)    i: default log file
c seshead        c*(*)    i: session header
C---
      LOGICAL*4 qpart, xqmtch
      character(64) filename
      INTEGER*4 ios
      INTEGER*4 lenact
      INTEGER*4 nret, iflag, inqunit, ierr, lseshd
      LOGICAL*4 opnerr
      LOGICAL*4 exist
      INTEGER*4 errunit
C
      character(35) descr(2)
      INTEGER*4 chatvl(2)
      DATA descr/'chattyness level to log commands', 
     &     'increment for indirect command files'/
      DATA opnerr/.FALSE./
      DATA errunit/0/

      filename=default_file 
      CALL xgtstr(instrg,parse,1,
     &            'error log file name (or ''none'' to disable)',
     &            1,filename,
     &            nret,iflag,-1)
      IF ( nret.LE.0 ) THEN
C       ** if the file is already open, or if an eof occured during the
C       ** handling of the '?', return, otherwise use the current filename
        IF ( (opnerr) .OR. (iflag.LT.0) ) RETURN
      END IF
      IF ( .NOT.(xqmtch('none',filename,qpart)) ) THEN
        CALL xtend(filename,'err')
        IF ( opnerr ) THEN
          INQUIRE (FILE=filename,EXIST=exist,NUMBER=inqunit,IOSTAT=ios)
C         ** already connected to the unit, so keep it as is
          IF ( exist .AND. (inqunit.EQ.errunit) ) GO TO 100
        ELSE
          opnerr = .TRUE.
        END IF
        lseshd = lenact(seshead)
        CALL xopner(filename,.FALSE.,seshead(:lseshd),.FALSE.,errunit,
     &              ierr)
        IF ( ierr.NE.0 ) THEN
          write(*,*) ' Unable to open the error log file `'//filename
     &             (:lenact(filename))//''''
          opnerr = .FALSE.
        END IF
      ELSE IF ( opnerr ) THEN
        CALL xclser('keep')
        errunit = 0
        opnerr = .FALSE.
      END IF
C     ** come from when the file was already connected
 100  CONTINUE
      CALL xgtint(instrg,parse,2,descr,2,chatvl,nret,iflag,-1)
      RETURN
      END
