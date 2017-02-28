**==setscr1.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE SETSCR1(Instrg,Lenn,Default_file,Default_suffix,
     &                   Seshead)
      CHARACTER Instrg*(*)
      INTEGER Lenn
      CHARACTER*(*) Default_file
      CHARACTER*(*) Default_suffix
      CHARACTER*(*) Seshead
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
 
      LOGICAL*4 qpart , XQMTCH
      character(255) filename
      INTEGER*4 ios
      INTEGER*4 LENACT
      INTEGER*4 nret , iflag , inqunit , ierr , lseshd
      LOGICAL*4 opnscr
      LOGICAL*4 exist
      INTEGER*4 scrunit
 
      DATA opnscr/.FALSE./
      DATA scrunit/0/
 
      filename = Default_file
      CALL UCLGST('xpifname',filename,ios)
      nret = 1
C      CALL xgtstr(instrg,lenn,1,
C     &            'script file name (or ''none'' to disable)',1,
C     &            filename,nret,iflag,-1)
 
C If no input and if the file is already open, or if an eof occured
C during the handling of the '?', return.
 
      IF ( nret.LE.0 ) THEN
         IF ( (opnscr) .OR. (iflag.LT.0) ) RETURN
      ENDIF
 
C If filename is not "none" then check whether it is already in use.
C If so, return, otherwise start the new script file.
 
      IF ( .NOT.(XQMTCH('none',filename,qpart)) ) THEN
 
         CALL XTEND(filename,Default_suffix)
         IF ( opnscr ) THEN
            INQUIRE (FILE=filename,EXIST=exist,NUMBER=inqunit,
     &               IOSTAT=ios)
C         ** already connected to the unit, so keep it as is
            IF ( exist .AND. (inqunit.EQ.scrunit) ) RETURN
         ELSE
            opnscr = .TRUE.
         ENDIF
 
         lseshd = LENACT(Seshead)
         CALL XOPNSC(filename,.FALSE.,Seshead(:lseshd),.FALSE.,scrunit,
     &               ierr)
         IF ( ierr.NE.0 ) THEN
            WRITE (*,*) ' Unable to open the script file `'//
     &                  filename(:LENACT(filename))//''''
            opnscr = .FALSE.
         ENDIF
 
C If filename is "none" and a file is open then close it.
 
      ELSEIF ( opnscr ) THEN
 
         CALL XCLSSC('keep')
         scrunit = 0
         opnscr = .FALSE.
 
      ENDIF
 
      RETURN
      END
