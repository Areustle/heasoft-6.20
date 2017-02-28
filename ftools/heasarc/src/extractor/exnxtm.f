
      FUNCTION exnxtm (qreset, status)

      IMPLICIT NONE

      INTEGER status
      LOGICAL qreset
      CHARACTER*(*) exnxtm


c Returns the next time window filename. If qreset is true then returns the
c first. If there are no more files then return status=-1.

c Arguments :
c      qreset     l       i: If true then start at the beginning
c      status     i       r: 0==OK, -1==off end of list
c      exnxtm     c       r: Filename

      INCLUDE 'extractor.inc'

      INTEGER ilun

      LOGICAL qfirst, qopen

      CHARACTER(255) tmpstr

      SAVE qfirst, qopen, ilun

      INTEGER lenact
      EXTERNAL lenact

      DATA qfirst, qopen /.TRUE., .FALSE./

      status = 0
      IF ( qreset ) qfirst = .TRUE.

      IF ( timefile .EQ. ' ' ) THEN
         status = -1
         RETURN
      ENDIF

c If we have multiple time window files

      IF ( timefile(1:1) .EQ. '@' ) THEN

c If reset then close the file if it is open

         IF ( qreset .AND. qopen ) THEN
            CLOSE(ilun)
            CALL frelun(ilun)
            qopen = .FALSE.
         ENDIF

c If the file is not open then open it

         IF ( .NOT.qopen ) THEN

            CALL getlun(ilun)
            tmpstr = timefile(2:)
            CALL openwr(ilun, tmpstr, 'old', ' ', ' ', 0, 1, status)
            IF ( status .NE. 0 ) THEN
               tmpstr = 'Cannot open '//timefile(2:lenact(timefile))
               CALL fcerr(tmpstr)
               CALL fcerrm(status)
               RETURN
            ENDIF
            qopen = .TRUE.

         ENDIF

c Read the next filename

         READ (ilun, '(a)', iostat=status) exnxtm
         IF ( status .NE. 0 ) THEN
            CLOSE(ilun)
            CALL frelun(ilun)
            qopen = .FALSE.
         ENDIF

c If there was only one event file

      ELSE

         IF ( qfirst ) THEN
            exnxtm = timefile
         ELSE
            status = -1
         ENDIF

      ENDIF

      qfirst = .FALSE.

      RETURN      

      END










