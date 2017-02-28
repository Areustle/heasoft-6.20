      SUBROUTINE CCALDJ (IY, IM, ID, DJM, J)
*+
*     - - - - - -
*      C A L D J
*     - - - - - -
*
*  Gregorian Calendar to Modified Julian Date
*
*  (Includes century default feature:  use sla_CLDJ for years
*   before 100AD.)
*
*  Given:
*     IY,IM,ID     int    year, month, day in Gregorian calendar
*
*  Returned:
*     DJM          dp     modified Julian Date (JD-2400000.5) for 0 hrs
*     J            int    status:
*                           0 = OK
*                           1 = bad year   (MJD not computed)
*                           2 = bad month  (MJD not computed)
*                           3 = bad day    (MJD computed)
*
*  Acceptable years are 00-99, interpreted as 1900-1999,
*                       100 upwards, interpreted literally.
*
*  Called:  CCLDJ
*
*  P.T.Wallace   Starlink   November 1985
*  Modified by Ron Zellar for use in the OGIP CALDB
*  Modified by Jeff Guerber, RSTX/NASA-GSFC, 1998-06-23.  Default century
*      always 1900, as in the new FITS standard.  Formerly, 00-49 were 2000.
*-

      IMPLICIT NONE

      INTEGER IY,IM,ID
      DOUBLE PRECISION DJM
      INTEGER J

      INTEGER NY

*  Default century if appropriate
      IF (IY .GE. 0 .AND. IY .LE. 99) THEN
         NY = IY + 1900
      ELSE
         NY = IY
      END IF

*  Modified Julian Date
      CALL CCLDJ(NY,IM,ID,DJM,J)

      END
