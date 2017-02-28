
      SUBROUTINE rmvdky(lun, status)

      IMPLICIT NONE

      INTEGER lun, status

c Subroutine to remove duplicate keywords from the header of a FITS
c extension

      INTEGER MAXKEYS
      PARAMETER (MAXKEYS=5000)

      INTEGER i, j, nkeys, nmore
      INTEGER length
      INTEGER icur, irec

      REAL fcur, frec

      character(80) record(MAXKEYS,2)
      character(80) current, curname, curvalue, comment
      character(255) contxt
      character rectyp, curtyp

      LOGICAL match

      INTEGER lenact
      EXTERNAL lenact

c Get the number of keywords

      CALL ftghsp(lun, nkeys, nmore, status) 
      contxt = 'Failed to get number of keywords'
      IF ( status .NE. 0 ) GOTO 999

      nkeys = MIN(nkeys, MAXKEYS)

c Loop over the keywords

      call ftgrec(lun, 1, current, status)
      WRITE(contxt, '(a,i5)') 'Failed to get keyword # 1'
      IF ( status .NE. 0 ) GOTO 999
      call ftgknm(current, curname, length, status)
      call ftpsvc(current, curvalue, comment, status)
      WRITE(contxt, '(a,i5)') 'Failed to parse keyword # 1'
      IF ( status .NE. 0 ) GOTO 999
      record(1,1) = curname
      record(1,2) = curvalue

      i = 2
      DO WHILE ( i .GT. 0 .AND. i .LE. nkeys .AND. nkeys .GT. 0 )

c Get the next record

         call ftgrec(lun, i, current, status)
         WRITE(contxt, '(a,i5)') 'Failed to get keyword # ', i
         IF ( status .NE. 0 ) GOTO 999
         call ftgknm(current, curname, length, status)
         call ftpsvc(current, curvalue, comment, status)
         WRITE(contxt, '(a,i5)') 'Failed to parse keyword # ', i
         IF ( status .NE. 0 ) GOTO 999

c Strip any trailing blanks from the value

         curvalue = curvalue(:lenact(curvalue))

c Compare it with previous records

         match = .FALSE.
         j = 1
         DO WHILE ( .NOT.match .AND. j .LE. i-1 )
            IF ( curname .EQ. record(j,1) ) THEN

c Found a keyword name match. Now check for a value match. If the type is string
c then do a string match otherwise convert and check for equality in order to
c cover the same number being encoded in different ways.

               IF ( lenact(curvalue) .GT. 0 ) THEN
                  call ftdtyp(curvalue, curtyp, status)
                  WRITE(contxt, '(a)') 'Failed to get datatype for '//
     &                 curvalue(:lenact(curvalue))
                  IF ( status .NE. 0 ) GOTO 999
               ELSE
                  curtyp = 'C'
               ENDIF
               IF ( lenact(record(j,2)) .GT. 0 ) THEN
                  call ftdtyp(record(j,2), rectyp, status)
                  WRITE(contxt, '(a)') 'Failed to get datatype for '//
     &                 record(j,2)(:lenact(record(j,2)))
                  IF ( status .NE. 0 ) GOTO 999
               ELSE
                  curtyp = 'C'
               ENDIF

               IF ( curtyp .NE. rectyp .OR. curtyp .EQ. 'C' .OR. 
     &              curtyp .EQ. 'L' .OR. curtyp .EQ. 'X' ) THEN
                  IF ( curvalue .EQ. record(j,2) ) match = .TRUE.
               ELSE IF ( curtyp .EQ. 'I' ) THEN
                  READ(curvalue,*) icur
                  READ(record(j,2),*) irec
                  IF ( icur .EQ. irec ) match = .TRUE.
               ELSE IF ( curtyp .EQ. 'F' ) THEN
                  READ(curvalue,*) fcur
                  READ(record(j,2),*) frec
                  IF ( fcur .EQ. frec ) match = .TRUE.
               ENDIF

               IF ( .NOT.match ) THEN
                  IF ( curname .NE. 'COMMENT' .AND.
     &                 curname .NE. 'HISTORY' ) THEN
                     contxt = 'Keyword '//curname(:lenact(curname))//
     &                    ' has two values: '//
     &                    record(j,2)(:lenact(record(j,2)))//' and '//
     &                    curvalue(:lenact(curvalue))
                     call xwrite(contxt, 5)
                  ENDIF
                  j = j + 1
               ENDIF
            ELSE
               j = j + 1
            ENDIF
         ENDDO

c If a match was found then delete the current record otherwise store and move on

         IF ( match ) THEN
            CALL ftdrec(lun, i, status)
            contxt = 'Failed to delete keyword '//current(1:8)
            IF ( status .NE. 0 ) GOTO 999
            nkeys = nkeys - 1
         ELSE
            record(i,1) = curname
            record(i,2) = curvalue
            i = i + 1
         ENDIF

      ENDDO

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF

      RETURN
      END
