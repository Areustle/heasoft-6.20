
      SUBROUTINE evprse(Status)

      IMPLICIT NONE

      INTEGER Status

c Subroutine to parse the event filename and look for any arguments
c specifying limits on event attributes. These are of form KEYWORD=#:#
c and are separated by commas or spaces. If the first two have no 
c explicit keyword then they are assumed to be X and Y. If any of the
c numerical arguments are replaced by '*' then that is returned as the
c special value of -99999. 

c Argument :
c     Status       i          r: 0=OK

c This routine sets the key and keyval arrays in keys.inc.

      INCLUDE 'expar.inc'
      INCLUDE 'keys.inc'
      INCLUDE 'extractor.inc'

      INTEGER ipos, ipos2, eqpos, colpos, iarg
      INTEGER i, fillen

      CHARACTER(72) contxt
      CHARACTER(255) argstr

      LOGICAL qdone, qkeyread

      INTEGER lenact
      EXTERNAL lenact

      Status = 0
      argstr = ' '
      qkeyread = .FALSE.
      nkeys = 0

c Check whether any arguments are included

      fillen = lenact(infile)
      IF ( infile(fillen:fillen) .NE. ']' ) RETURN

      ipos = fillen - 1
      DO WHILE ( ipos .GT. 1 .AND. infile(ipos:ipos) .NE. '[' )
         ipos = ipos - 1
      ENDDO

      IF ( infile(ipos:ipos) .NE. '[' ) THEN
         contxt = '  Event file name has mismatched square brackets'
         Status = 1
         GOTO 999
      ENDIF

c Set the string with the arguments and reset the event filename to
c remove the arguments

      argstr = infile(ipos+1:lenact(infile)-1)
      DO i = ipos, fillen
         infile(i:i) = ' '
      ENDDO

c Replace any commas in argstr with blanks

      DO ipos = 1, lenact(argstr)
         IF ( argstr(ipos:ipos) .EQ. ',' ) argstr(ipos:ipos) = ' '
      ENDDO

c If there are no arguments then return

      IF (lenact(argstr) .EQ. 0) RETURN

c Loop round arguments

      qdone = .FALSE.
      iarg = 0
      ipos = 1
      colpos = index(argstr,':')

c If there is no ':' then the filter specifications are not correct so ignore them
c but warn the user

      IF ( colpos .EQ. 0 ) THEN
         WRITE(contxt, '(a,a)') '  Invalid filter specification : ', 
     &                          argstr(:lenact(argstr))
         Status = 3
         GOTO 999
      ENDIF

      DO WHILE ( .NOT.qdone )

         DO WHILE ( argstr(ipos:ipos) .EQ. ' ' .AND. 
     &              ipos .LT. len(argstr) )
            ipos = ipos + 1
         ENDDO

         iarg = iarg + 1
         eqpos = index(argstr(ipos:),'=')
         IF ( eqpos .NE. 0 ) eqpos = eqpos + ipos - 1

c If the next '=' is later than the next ':' or there is no other '='
c then no keyword is given. If a keyword has already been read then
c assume this is a second range for this keyword. If a keyword has not
c already been read and this is the first or second argument then assume
c the keyword is X or Y.

         IF ( eqpos .EQ. 0 .OR. eqpos .GT. colpos ) THEN

            IF ( qkeyread ) THEN

               key(iarg) = key(iarg-1)

            ELSE

               IF ( iarg .EQ. 1 ) THEN
                  key(iarg) = 'X'
               ELSEIF ( iarg .EQ. 2 ) THEN
                  key(iarg) = 'Y'
               ELSE
                  contxt = 
     &              '  Keyword missing in event filename argument'
                  Status = 2
                  GOTO 999
               ENDIF

            ENDIF

c Otherwise read the keyword

         ELSE

            key(iarg) = argstr(ipos:eqpos-1)
            CALL upc(key(iarg))
            ipos = eqpos + 1

            DO WHILE ( argstr(ipos:ipos) .EQ. ' ' .AND. 
     &                 ipos .LT. len(argstr) )
               ipos = ipos + 1
            ENDDO

            qkeyread = .TRUE.

         ENDIF

c Now read the two arguments. If the argument is '*' then the keyval is
c set to -99999.d0

         ipos2 = colpos - 1
         DO WHILE ( argstr(ipos2:ipos2) .EQ. ' ' .AND. 
     &              ipos2 .GT. ipos )
            ipos2 = ipos2 - 1
         ENDDO

         IF ( argstr(ipos:ipos) .EQ. '*' ) THEN
            keyval(1,iarg) = -99999.d0
         ELSE
            READ(argstr(ipos:ipos2),*,iostat=Status) keyval(1,iarg)
            WRITE(contxt,'(a,i2,a,a)') 
     &        '  Failed to read first value for argument ', iarg, ' : ',
     &        argstr(ipos:ipos2)
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

         ipos = colpos+1
         DO WHILE ( argstr(ipos:ipos) .EQ. ' ' .AND. 
     &              ipos .LT. len(argstr) )
            ipos = ipos + 1
         ENDDO

         ipos2 = index(argstr(ipos+1:),' ')
         IF ( ipos2 .EQ. 0 ) THEN
            ipos2 = lenact(argstr)
         ELSE
            ipos2 = ipos2 + ipos - 1
         ENDIF

         IF ( argstr(ipos:ipos) .EQ. '*' ) THEN
            keyval(2,iarg) = -99999
         ELSE
            READ(argstr(ipos:ipos2),*,iostat=Status) keyval(2,iarg)
            WRITE(contxt,'(a,i2,a,a)') 
     &        'Failed to read second value for argument ', iarg, ' : ',
     &        argstr(ipos:ipos2)
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

c Look for the next argument. Note that if there are no more colons but there is still
c text then there is an error in the filter specification

         colpos = index(argstr(ipos2+1:),':')
         IF ( colpos .EQ. 0 ) THEN
            qdone = .TRUE.
            IF (lenact(argstr(ipos2+1:)) .GT. 0) THEN
               WRITE(contxt, '(a,a)') 
     &                '  Invalid filter specification : ', 
     &                argstr(ipos2+1:lenact(argstr))
               Status = 3
               GOTO 999
            ENDIF
         ELSE
            colpos = colpos + ipos2
            ipos = ipos2 + 1
         ENDIF

      ENDDO

      nkeys = iarg

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcecho(contxt)
         WRITE(contxt,'(a,i3)') '  Status = ', Status
         CALL fcecho(contxt)
      ENDIF

      RETURN
      END
