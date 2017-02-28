      SUBROUTINE GETDAYNUM(Month,Day,Year,Daynumber)
c
c Subroutine to convert from month (number), day, and year to sequential
c day number.  Extra days for leap years are included.
c
      INTEGER*4 imod1 , imod2 , imod3
      INTEGER*4 Month , Day , Year , Daynumber , leap
c
      imod1 = MOD(Year,4)
      imod2 = MOD(Year,400)
      imod3 = MOD(Year,100)
c
c If a year is divisible by 4, then it is a leap year unless it is also a
c CENTURY year that is NOT divisible by 400.
c
      IF ( imod1.NE.0 ) THEN
         leap = 0
      ELSEIF ( imod3.NE.0 ) THEN
         leap = 1
      ELSEIF ( (imod3.EQ.0) .AND. (imod2.EQ.0) ) THEN
         leap = 1
      ELSE
         leap = 0
      ENDIF
c
      IF ( Month.EQ.1 ) THEN
         Daynumber = Day
      ELSEIF ( Month.EQ.2 ) THEN
         Daynumber = 31 + Day
      ELSEIF ( Month.EQ.3 ) THEN
         Daynumber = 59 + Day + leap
      ELSEIF ( Month.EQ.4 ) THEN
         Daynumber = 90 + Day + leap
      ELSEIF ( Month.EQ.5 ) THEN
         Daynumber = 120 + Day + leap
      ELSEIF ( Month.EQ.6 ) THEN
         Daynumber = 151 + Day + leap
      ELSEIF ( Month.EQ.7 ) THEN
         Daynumber = 181 + Day + leap
      ELSEIF ( Month.EQ.8 ) THEN
         Daynumber = 212 + Day + leap
      ELSEIF ( Month.EQ.9 ) THEN
         Daynumber = 243 + Day + leap
      ELSEIF ( Month.EQ.10 ) THEN
         Daynumber = 273 + Day + leap
      ELSEIF ( Month.EQ.11 ) THEN
         Daynumber = 304 + Day + leap
      ELSEIF ( Month.EQ.12 ) THEN
         Daynumber = 334 + Day + leap
      ENDIF
      RETURN
      END
