c**********************************************************************
c     The purpose of this function is to take as input the day, month,
c and year as integers and to return the integer value
c for the Modified Julian Day.
c A convenient check is to remember that Noon of May 23, 1968 is the
c Julian Day 2440000 so if we convert to the Modified Julian Day
c beginning at Midnight of May 23, 1968 is day 39998. 
c**********************************************************************      

      integer function mjdconvert(iday,imonth,iyear)
      integer juliandy,iday,imonth,iyear,igregorian,
C    &     jday,jmonth,jyear,mjdconvert    **VAX complained here**
     &     jday,jmonth,jyear

      parameter (igregorian=15+31*(10+12*1582))

      jyear=iyear

      if (jyear.eq.0) call fcecho('There is no year ZERO!')
      if (jyear.lt.0) jyear=jyear+1

      if (imonth.gt.2) then
        jmonth=imonth+1
      else
        jyear=jyear-1
        jmonth=imonth+13
      endif

      juliandy=int(365.25*float(jyear))+
     &   int(30.6001*float(jmonth))
     &   +iday+1720995

c     If the combination of day, month, and year is greater than
c the date at which we switched from the Julian calendar to the Gregorian
c calendar we have to deal with the more complicated way of dealing with
c leap years and the like that is done under the Gregorian calendar.
c This switch occurred on Oct 15, 1582 which is how "igregorian" was
c calculated. 
      if (iday+31*(imonth+12*iyear).ge.igregorian) then
        jday=int(0.01*jyear)
        juliandy=juliandy+2-jday+int(0.25*float(jday))
      endif

c      While the MJD is defined as JD-2400000.5 we have to account for the
c      fact that the day we are presently in is not completely over so we
c      have to subtract another 1 day and since a JD starts at Noon we have
c      to account for that additional half day so we are left with
c      Juliandy-1.5-2400000.5 = juliandy - 2400002
      
      mjdconvert=juliandy-2400002

      return
      end

