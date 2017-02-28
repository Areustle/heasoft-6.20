      SUBROUTINE SETSCR(instrg, lenn, default_file, default_suffix,
     &                  seshead)
      CHARACTER instrg*(*)
      INTEGER lenn
      CHARACTER*(*) default_file
      CHARACTER*(*) default_suffix
      CHARACTER*(*) seshead
C---
C subroutine to open a script file.  if the file name is none
C then the file is closed.
C---
C instrg         i    parse string
C lenn           i/o  parse position string
C default_file   i    default filename for script
C default_suffix i    default suffix for filename
C seshead        i    session identifier
C---
C 25 jan 1991 - kaa
C---

      LOGICAL*4 qpart, xqmtch
      character(255) filename
      INTEGER*4 ios
      INTEGER*4 lenact
      INTEGER*4 nret, iflag, inqunit, ierr, lseshd
      LOGICAL*4 opnscr
      LOGICAL*4 exist
      INTEGER*4 scrunit

      DATA opnscr/.FALSE./
      DATA scrunit/0/

      filename=default_file 
      CALL xgtstr(instrg,lenn,1,
     &            'script file name (or ''none'' to disable)',1,
     &            filename,nret,iflag,-1)

C If no input and if the file is already open, or if an eof occured
C during the handling of the '?', return.

      IF ( nret.LE.0 ) THEN
        IF ( (opnscr) .OR. (iflag.LT.0) ) RETURN
      END IF

C If filename is not "none" then check whether it is already in use.
C If so, return, otherwise start the new script file.

      IF ( .NOT.(xqmtch('none',filename,qpart)) ) THEN

        CALL xtend(filename,default_suffix)
        IF ( opnscr ) THEN
          INQUIRE (FILE=filename,EXIST=exist,NUMBER=inqunit,IOSTAT=ios)
C         ** already connected to the unit, so keep it as is
          IF ( exist .AND. (inqunit.EQ.scrunit) ) RETURN
        ELSE
          opnscr = .TRUE.
        END IF

        lseshd = lenact(seshead)
        CALL xopnsc(filename,.FALSE.,seshead(:lseshd),.FALSE.,scrunit,
     &              ierr)
        IF ( ierr.NE.0 ) THEN
          write(*,*) ' Unable to open the script file `'//filename
     &             (:lenact(filename))//''''
          opnscr = .FALSE.
        END IF

C If filename is "none" and a file is open then close it.

      ELSE IF ( opnscr ) THEN

        CALL xclslg('keep')
        scrunit = 0
        opnscr = .FALSE.

      END IF

      RETURN
      END
