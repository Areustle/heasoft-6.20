 
      subroutine timofday(day,accur,time)
 
c Convert day number fraction to character hh:mm:ss.sss representation.

c  I  day   (d) = Input day number
c  I  accur (i) = 0 for milliseconds
c               = nonzero for microseconds
c  O  time  (c) = Output time string

c Author:  Eric Lufkin, HEASARC/GSFC, September 1993
c Revised:                            November 1993, for accuracy switch.

      character(15) time
      integer accur, iday,ihr,imin,isec,imms
      double precision day, hr, min, sec

      iday = int (      dabs( day))
       hr  =     (day - dble(iday)) * 24.d0
      ihr  = int (             hr )
       min =     ( hr - dble( ihr)) * 60.d0
      imin = int (            min )
       sec =     (min - dble(imin)) * 60.d0
      isec = int (            sec )
      if(accur.eq.0) then
         imms =nint((sec - dble(isec)) *  1.d3)
         if(imms.eq.1000) then
            isec = isec + 1
            imms = 0
         endif
         write(time,100) ihr,imin,isec,imms
      else
         imms =nint((sec - dble(isec)) *  1.d6)
         if(imms.eq.1000000) then
            isec = isec + 1
            imms = 0
         endif
         write(time,101) ihr,imin,isec,imms
      endif

100   format(i2.2,':',i2.2,':',i2.2,'.',i3.3,3x)
101   format(i2.2,':',i2.2,':',i2.2,'.',i6.6)

      return
      end

