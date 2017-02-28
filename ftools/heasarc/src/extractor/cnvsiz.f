
      SUBROUTINE cnvsiz(cpos, imod2, points, ierr)

      DOUBLE PRECISION points
      CHARACTER*(*) cpos
      INTEGER imod2, ierr

c Converts a dimension into pixels assuming it is given in arcsec.
c Arguments :
c      cpos          c         i: String array containing dimension
c      imod2         i         i: 1==X-axis, 0==Y-axis
c      points        d         r: Dimension
c      ierr          i         r: Status 0==OK

      INCLUDE 'extractor.inc'

      DOUBLE PRECISION dval

      ierr = 0

c Convert from arcsec into degrees to match fcrdelt

      READ(cpos, *, IOSTAT=ierr) dval
      IF ( ierr .NE. 0 ) RETURN

      dval = dval / 3600.d0

c Convert to physical pixels

      IF ( imod2 .EQ. 1 ) THEN
         points = dval / ABS(fcrdelt(1))
      ELSE
         points = dval / ABS(fcrdelt(2))
      ENDIF

      RETURN
      END
