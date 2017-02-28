      FUNCTION exevfl(nevfl, qfilt, status)

      IMPLICIT NONE

      INTEGER nevfl, status
      CHARACTER*(*) exevfl
      LOGICAL qfilt

c Returns the name of the nevfl'th event file. If this file doesn't exist
c then return status = -1.

c Arguments :
c      nevfl      i       i: Number of event file to return
c      qfilt      l       i: If true then include any region filter string
c      status     i       r: 0==OK, !0==error
c      exevfl     c       r: Filename

      INCLUDE 'extractor.inc'

      INTEGER ilun, i

      CHARACTER(255) tmpstr

      INTEGER lenact
      EXTERNAL lenact

      status = 0

c If we have multiple input event files

      IF ( infile(1:1) .EQ. '@' ) THEN

         CALL getlun(ilun)
         tmpstr = infile(2:)
         CALL openwr(ilun, tmpstr, 'old', ' ', ' ', 0, 1, status)
         IF ( status .NE. 0 ) THEN
            tmpstr = 'Cannot open '//infile(2:lenact(infile))
            CALL fcerr(tmpstr)
            CALL fcerrm(status)
            RETURN
         ENDIF

c Loop round the file to the line we need to read

         DO i = 1, nevfl
            READ (ilun, '(a)', iostat=status) tmpstr
         ENDDO
         exevfl = tmpstr

c Close the file again

         CLOSE(ilun)
         CALL frelun(ilun)

c If there was only one event file

      ELSE

         IF ( nevfl .EQ. 1 ) THEN
            exevfl = infile
         ELSE
            status = -1
         ENDIF

      ENDIF

c Use cfitsio region filtering

      IF ( regionfile .NE. ' ' .AND. qfilt ) THEN
         i = LENACT(exevfl) + 1
         exevfl(i:) = '[regfilter("'//regionfile(:LENACT(regionfile))//
     &     '",'//Xcolf(:lenact(Xcolf))//','//Ycolf(:lenact(Ycolf))//')]'
      ENDIF

      RETURN
      END

c ***********************************************************************

      FUNCTION exnevf()

      IMPLICIT NONE

      INTEGER exnevf

c Returns the number of event files or -1 on error (which shouldn't occur).

c Arguments :
c      exnevf     i       r: The number of files or -1 on error

      INCLUDE 'extractor.inc'

      INTEGER ilun, status

      CHARACTER(255) tmpstr

      INTEGER lenact
      EXTERNAL lenact

      status = 0

c If we have multiple input event files

      IF ( infile(1:1) .EQ. '@' ) THEN

         CALL getlun(ilun)
         tmpstr = infile(2:)
         CALL openwr(ilun, tmpstr, 'old', ' ', ' ', 0, 1, status)
         IF ( status .NE. 0 ) THEN
            tmpstr = 'Cannot open '//infile(2:lenact(infile))
            CALL fcerr(tmpstr)
            CALL fcerrm(status)
            exnevf = -1
            RETURN
         ENDIF

c Loop round the file till we fall off the end counting the successful
c reads

         exnevf = 0
         DO WHILE ( status .EQ. 0 )
            READ (ilun, '(a)', iostat=status) tmpstr
            exnevf = exnevf + 1
         ENDDO
         exnevf = exnevf - 1

c Close the file again

         CLOSE(ilun)
         CALL frelun(ilun)

c If there was only one event file

      ELSE

         exnevf = 1

      ENDIF

      RETURN
      END

      







