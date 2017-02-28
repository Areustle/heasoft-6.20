
      FUNCTION tmftyp(filenm, status)

      IMPLICIT NONE

      CHARACTER*(*) filenm
      INTEGER tmftyp, status

c Function to find out what sort of a time selection file filenm is.

c Argument :
c      filenm       c       i: input filename
c      tmftyp       i       r: 1 = FITS, 2 = Xronos, 3 = simple ascii
c      status       i       r: 0==OK

      INTEGER ilen, lun, blocksize

      CHARACTER(255) inline
      CHARACTER(80) contxt

      INTEGER lenact
      EXTERNAL lenact

      tmftyp =0

c First try it as a GTI file (this may have [extension#] on the end of 
c the string).

      ilen = lenact(filenm)
      IF ( filenm(ilen:ilen).EQ.']' ) THEN
         DO WHILE ( filenm(ilen:ilen) .NE. '[' .AND. ilen .GT. 1 )
            ilen = ilen - 1
         ENDDO
         ilen = ilen - 1
      ENDIF

c Check whether we can open this as a FITS file. If so, close the
c file and set tmftyp=1

      CALL getlun(lun)
      CALL ftopen(lun, filenm(:ilen), 0, blocksize, status)

      IF ( status .EQ. 0 ) THEN

         tmftyp = 1
         CALL ftclos(lun, status)
         CALL frelun(lun)

      ELSE

         status = 0
         CALL ftclos(lun, status)
         CALL frelun(lun)
         status = 0
         CALL ftcmsg()

c It was not a FITS file so open as an ASCII file and read the first
c line to find out whether it is a xronos file. If we can't open the
c file then go onto the next file

         CALL getlun(lun)
         CALL openwr(lun, filenm, 'old', ' ', ' ', 0, 1, status)
         contxt = 'Unable to open '//
     &            filenm(:MIN(lenact(filenm),len(contxt)-15))
         IF ( status .NE. 0 ) GOTO 999

c Read the first line

         READ(lun, '(a)', iostat=status) inline
         contxt = 'Failed to read first line of '//
     &            filenm(:MIN(lenact(filenm),len(contxt)-29))
         IF ( status .NE. 0 ) GOTO 999

c If the first line contains the string Xronos then it is a xronos window
c file (type 2) otherwise it is a simple ascii file (type 3).

         IF ( index(inline, 'Xronos') .NE. 0 ) THEN
            tmftyp = 2
         ELSE
            tmftyp = 3
         ENDIF

         CLOSE(lun)
         CALL frelun(lun)

      ENDIF

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcecho(contxt)
      ENDIF

      END
