
      SUBROUTINE w_dskeys(Lun, Status)

      IMPLICIT NONE

      INTEGER Lun, Status

c Routine to write out the data subspace keywords

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'subkeys.inc'

      INTEGER i, j

      CHARACTER(80) keynm, strval
      CHARACTER(255) contxt

      LOGICAL qfound

      INTEGER lenact
      EXTERNAL lenact

c Update the DS keyword specifying the grades in use

      IF ( needgrade ) CALL update_grade_ds()

c Loop round the DSTYP keywords held in memory

      DO i = 1, ndstyp

         IF ( dstyp(i) .EQ. ' ' ) GOTO 100

c Write the DSTYP, DSUNI, and DSFORM

         keynm = 'DSTYP'
         CALL DSSCAT(keynm, 1, i)
         CALL FTUKYS(Lun, keynm, dstyp(i),
     &               'Data subspace descriptor: name',Status)
         contxt = 'W_DSKEYS: Failed to update '//keynm(:lenact(keynm))
         IF ( Status .NE. 0 ) GOTO 999

         IF ( dsuni(i) .NE. ' ' ) THEN
            keynm = 'DSUNI'
            CALL DSSCAT(keynm, 1, i)
            CALL FTUKYS(Lun, keynm, dsuni(i),
     &                  'Data subspace descriptor: units',Status)
            contxt = 'W_DSKEYS: Failed to update '
     &                 //keynm(:lenact(keynm))
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

         IF ( dsform(i) .NE. ' ' ) THEN
            keynm = 'DSFORM'
            CALL DSSCAT(keynm, 1, i)
            CALL FTUKYS(Lun, keynm, dsform(i),
     &                  'Data subspace descriptor: datatype',Status)
            contxt = 'W_DSKEYS: Failed to update '
     &                 //keynm(:lenact(keynm))
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

c Write the DSVAL and DSREF for this DSTYP. Also delete any keywords in
c the file that are no longer required.

         DO j = 1, idsval(i)

            IF ( dsval(j,i) .NE. ' ' ) THEN
               keynm='DSVAL'
               CALL DSSCAT(keynm, j, i)
               CALL FTUKYS(Lun, keynm, dsval(j,i),
     &                     'Data subspace descriptor: value',Status)
               contxt = 'W_DSKEYS: Failed to update '
     &                    //keynm(:lenact(keynm))
               IF ( Status .NE. 0 ) GOTO 999
            ENDIF

         ENDDO

         j = idsval(i) + 1
         DO WHILE ( Status .EQ. 0 )
            keynm = 'DSVAL'
            CALL DSSCAT(keynm, j, i)
            CALL FTDKEY(Lun, keynm, Status)
            j = j + 1
         ENDDO
         Status = 0

         DO j = 1, idsref(i)

            IF ( dsref(j,i) .NE. ' ' ) THEN
               keynm='DSREF'
               CALL DSSCAT(keynm, j, i)
               CALL FTUKYS(Lun, keynm, dsref(j,i),
     &                     'Data subspace descriptor: reference',Status)
               contxt = 'W_DSKEYS: Failed to update '
     &                    //keynm(:lenact(keynm))
               IF ( Status .NE. 0 ) GOTO 999
            ENDIF

         ENDDO

         j = idsref(i) + 1
         DO WHILE ( Status .EQ. 0 )
            keynm = 'DSREF'
            CALL DSSCAT(keynm, j, i)
            CALL FTDKEY(Lun, keynm, Status)
            j = j + 1
         ENDDO
         Status = 0

 100     CONTINUE

      ENDDO

c If there is a region file defined then write the data subspace descriptor
c unless an appropriate DSVAL already exists.

      qfound = .FALSE.
      strval = 'POS('//xcolf(:lenact(xcolf))//','//
     &         ycolf(:lenact(ycolf))//')'
      DO i = 1, ndstyp
         IF ( dstyp(i)(:lenact(strval)) .EQ. 
     &        strval(:lenact(strval)) ) qfound = .TRUE.
      ENDDO 

      IF ( regionfile .NE. ' ' .AND. .NOT.qfound ) THEN
      
         keynm='DSTYP' 
         CALL DSSCAT(keynm,1,ndstyp+1)
         CALL FTUKYS(Lun,keynm,strval,
     &               'Data subspace descriptor: name',Status)
         contxt = 'W_DSKEYS: Failed to update '//keynm(:lenact(keynm))
         IF ( Status.NE.0 ) GOTO 999

c Construct the hduname by looking for other instances of REG001 and 
c incrementing by one

         j = 0
         DO i = 1, ndstyp
            IF ( dsref(1,i)(1:7) .EQ. ':REG001' ) j = j + 1
         ENDDO
         strval = ':REG001'
         IF ( j .GE. 9 ) THEN
            WRITE(strval(8:9), '(i2)') (j+1)
         ELSEIF ( j .LT. 9 ) THEN
            WRITE(strval(8:9), '(a1,i1)') '0', (j+1)
         ENDIF

         regextname = strval(2:9)

         keynm='DSREF' 
         CALL DSSCAT(keynm,1,ndstyp+1)
         CALL FTUKYS(Lun,keynm,strval,
     &               'Data subspace descriptor: reference',Status)
         contxt = 'W_DSKEYS: Failed to update '//keynm(:lenact(keynm))
         IF ( Status.NE.0 ) GOTO 999
         keynm='DSVAL' 
         CALL DSSCAT(keynm,1,ndstyp+1)
         CALL FTUKYS(Lun,keynm,'TABLE',
     &               'Data subspace descriptor: value',Status)
         contxt = 'W_DSKEYS: Failed to update '//keynm(:lenact(keynm))
         IF ( Status.NE.0 ) GOTO 999
      ENDIF

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END
