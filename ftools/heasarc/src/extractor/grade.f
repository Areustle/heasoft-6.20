c Set the grade array from the grade filter input by the user

      SUBROUTINE SET_GARRAY(Gstring, Gbound)

      IMPLICIT NONE

      CHARACTER*(*) Gstring
      INTEGER Gbound(2)

c Arguments
c     Gstring      c      i: Grade filter string
c     Gbound       i      i: Bounds on grade values

      INCLUDE 'grade.inc'

      INTEGER i, istart, iend, status, grade, grade2, ipt

      CHARACTER(128) cstr
      CHARACTER(256) contxt

      LOGICAL qdone

      INTEGER lenact
      EXTERNAL lenact

c Set the grade min and max values

      gmin = Gbound(1)
      gmax = Gbound(2)

c If no TLMIN/MAX keywords were read then assume gmin=0 and gmax=GMAXSIZE-1.

      IF ( gmin .EQ. -999 .AND. gmax .EQ. -999 ) THEN
         gmin = 0
         gmax = GMAXSIZE-1
      ENDIF

c Check that grade min and max values are in allowed range

      IF ( gmin .LT. 0 .OR. gmax .GT. GMAXSIZE-1 ) THEN
         contxt = 'Error in SET_GARRAY: lowest grade too low '//
     &            'or highest grade too high'
         GOTO 999
      ENDIF

c Set the elements to false

      DO i = gmin, gmax
         garray(i) = .FALSE.
      ENDDO

c Convert any ranges delimited by ":" to ranges delimited by "-"

      DO i = 1, lenact(gstring)
         IF ( gstring(i:i) .EQ. ':' ) gstring(i:i) = '-'
      ENDDO

c Convert any ranges separated by "," into ranges separated by " "

      DO i = 1, lenact(gstring)
         IF ( gstring(i:i) .EQ. ',' ) gstring(i:i) = ' '
      ENDDO

c Loop round specifications in gstring setting the garray

      istart = 1
      qdone = .FALSE.
      DO WHILE ( .NOT.qdone )

c find the current specification (up to the next space or the end of
c the string).

         iend = index(gstring(istart:),' ')
         IF ( iend .EQ. 0 ) THEN
            iend = lenact(gstring)
            qdone = .TRUE.
         ELSE
            iend = iend + istart - 2
         ENDIF

c Parse the current specification setting the garray as appropriate

         cstr = gstring(istart:iend)

         IF ( index(cstr,'-') .NE. 0 ) THEN

c read the beginning and end of a range

            ipt = index(cstr,'-')
            READ(cstr(1:ipt-1), *, iostat=status) grade
            contxt = 'Failed to read first integer from range '//cstr
            IF ( status .NE. 0 ) GOTO 999
            READ(cstr(ipt+1:), *, iostat=status) grade2
            contxt = 'Failed to read second integer from range '//cstr
            IF ( status .NE. 0 ) GOTO 999

            grade = MAX(gmin, grade)
            grade2 = MIN(gmax, grade2)

            DO i = grade, grade2
               garray(i) = .TRUE.
            ENDDO

         ELSEIF ( index(cstr,'>') .NE. 0 ) THEN

c all grades above some value are valid

            ipt = index(cstr,'>')
            READ(cstr(ipt+1:), *, iostat=status) grade
            contxt = 'Failed to read integer from '//cstr(ipt+1:)
            IF ( status .NE. 0 ) GOTO 999

            DO i = grade+1, gmax
               garray(i) = .TRUE.
            ENDDO

         ELSEIF ( index(cstr,'<') .NE. 0 ) THEN

c all grades below some value are valid

            ipt = index(cstr,'<')
            READ(cstr(ipt+1:), *, iostat=status) grade
            contxt = 'Failed to read integer from '//cstr(ipt+1:)
            IF ( status .NE. 0 ) GOTO 999

            DO i = gmin, grade-1
               garray(i) = .TRUE.
            ENDDO

         ELSE

c should be a single number

            READ(cstr, *, iostat=status) grade
            contxt = 'Failed to read integer from '//cstr
            IF ( status .NE. 0 ) GOTO 999

            IF ( grade .GE. gmin .AND. grade .LE. gmax ) THEN
               garray(grade) = .TRUE.
            ENDIF

         ENDIF

c Move onto the next specification

         istart = iend + 2
         IF ( istart .GT. lenact(gstring) ) qdone = .TRUE.

      ENDDO

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         WRITE(contxt,'(a,i5)') 'SET_GARRAY : Status = ', status
         CALL fcerr(contxt)
      ENDIF

      RETURN
      END

c ******************************************************************
      LOGICAL FUNCTION IS_GOOD_GRADE(Grade)

      IMPLICIT NONE

      INTEGER Grade

c Returns True if Grade is in the grade filter specified by the user
c Arguments :
c     Grade         i         i: Grade value
c     Is_good_grade l         r: True if valid grade

      INCLUDE 'grade.inc'

      IF ( grade .LT. gmin .OR. grade .GT. gmax ) THEN
         is_good_grade = .FALSE.
      ELSE
         is_good_grade = garray(grade)
      ENDIF

      END

c ******************************************************************
      SUBROUTINE UPDATE_GRADE_DS()

      IMPLICIT NONE

c Routine to update the datasubspace string for the grade filtering
c Assumes that the DS string is in the form #:#,#:#,#:#,...

      INCLUDE 'extractor.inc'
      INCLUDE 'expar.inc'
      INCLUDE 'subkeys.inc'
      INCLUDE 'grade.inc'

      INTEGER istart, iend, ipt, i, igcol, j
      INTEGER grade1, grade2
      INTEGER opt, status

      CHARACTER(256) dstring, outstr, contxt
      CHARACTER(128) cstr

      LOGICAL qdone, qon

      INTEGER lenact
      EXTERNAL lenact

c Find the DSTYP corresponding to gcol

      igcol = 0
      qdone = .FALSE.
      DO i = 1, ndstyp
         IF ( dstyp(i)(:lenact(gcol)) .EQ. gcol(:lenact(gcol)) ) THEN
            igcol = i
            qdone = .TRUE.
         ENDIF
      ENDDO

c If there is no DSTYP keyword then create one along with the relevant
c #DSVAL keywords

      IF ( .NOT.qdone ) THEN

         ndstyp = ndstyp + 1
         igcol = ndstyp
         dstyp(igcol) = gcol(:lenact(gcol))
         dsform(igcol) = ' '
         dsuni(igcol) = ' '
         idsref(igcol) = 0

c Set the number of dsvals to the maximum of those in other DSS keywords

         idsval(igcol) = 1
         DO j = 1, ndstyp-1
            idsval(igcol) = MAX(idsval(igcol), idsval(j))
         ENDDO

c Set the DSVAL string to be gmin to gmax

         DO j = 1, idsval(igcol)
            WRITE(outstr,'(i3,a,i3)') gmin, ':', gmax
            dsval(j,igcol) = ' '
            opt = 1
            DO i = 1, lenact(outstr)
               IF ( outstr(i:i) .NE. ' ' ) THEN
                  dsval(j,igcol)(opt:opt) = outstr(i:i)
                  opt = opt + 1
               ENDIF
            ENDDO
         ENDDO
            
      ENDIF


c Loop round DSVALs

      DO j = 1, idsval(igcol)

         dstring = dsval(j,igcol)

c Initialize output string

         outstr = ' '
         opt = 1

c Loop round input string

         istart = 1
         qdone = .FALSE.
         DO WHILE ( .NOT.qdone )

c find the current specification (up to the next comma or the end of
c the string).

            iend = index(dstring(istart:),',')
            IF ( iend .EQ. 0 ) THEN
               iend = lenact(dstring)
               qdone = .TRUE.
            ELSE
               iend = iend + istart - 2
            ENDIF

c find position of : and read numbers on each side

            cstr = dstring(istart:iend)
            ipt = index(cstr,':')
            READ(cstr(:ipt-1),*,iostat=status) grade1
            contxt = 'Failed to read integer from '//cstr(:ipt-1)
            IF ( status .NE. 0 ) GOTO 999
            READ(cstr(ipt+1:),*,iostat=status) grade2
            contxt = 'Failed to read integer from '//cstr(ipt+1:)
            IF ( status .NE. 0 ) GOTO 999

c construct output string from those grades in grade1-grade2 that are
c selected in the garray

            qon = .FALSE.
            DO i = grade1, grade2
               IF ( garray(i) ) THEN
                  IF ( .NOT.qon ) THEN
                     WRITE(outstr(opt:opt+3),'(i3,a)') i, ':'
                     opt = opt + 4
                     qon = .TRUE.
                  ENDIF
               ELSE
                  IF ( qon ) THEN
                     WRITE(outstr(opt:opt+3),'(i3,a)') i-1, ','
                     opt = opt + 4
                     qon = .FALSE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( qon ) THEN
               WRITE(outstr(opt:opt+3),'(i3,a)') grade2, ','
               opt = opt + 4
            ENDIF

c Go onto next specification from the input string

            istart = iend + 2

         ENDDO

         IF ( outstr(lenact(outstr):lenact(outstr)) .EQ. ',' ) THEN
            outstr(lenact(outstr):lenact(outstr)) = ' '
         ENDIF

c Copy to the DSVAL string squeezing out any spaces

         dsval(j,igcol) = ' '
         opt = 1
         DO i = 1, lenact(outstr)
            IF ( outstr(i:i) .NE. ' ' ) THEN
              dsval(j,igcol)(opt:opt) = outstr(i:i)
               opt = opt + 1
            ENDIF
         ENDDO

c End loop over DSVALs

      ENDDO

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         WRITE(contxt,'(a,i5)') 'UPDATE_GRADE_DS : Status = ', status
         CALL fcerr(contxt)
      ENDIF

      RETURN
      END

