      SUBROUTINE gridrnd(xdeg,mode)
      IMPLICIT NONE
c
c  Returns the nearest "nice" angle
c
c  I/O  xdeg   (d)  Angle (degrees)
c   I   mode   (i)  1=degrees, 2=hours
c
      real*8 xdeg
      integer mode
c
c  Local variables
c
      REAL*8 am, x

      if ( mode.eq.1 ) then
         x = xdeg
      elseif ( mode.eq.2 ) then
         x = xdeg/15.d0
      else
         call xwarn(' Invalid mode for GRIDRND', 5)
         return
      endif

      am = 0.016666666666666666666666667d0

      if (x.ge.50*am) then    ! degrees
        if (x.lt.1.5) then
          x = 1.0
	elseif (x.ge.1.5.and.x.lt.2.25) then
          x = 2.0
       	elseif (x.ge.2.25.and.x.lt.3.25) then
          x = 2.5
	elseif (x.ge.3.25.and.x.lt.4.5) then
          x = 4.0
	elseif (x.ge.4.5.and.x.lt.6.25) then
          x = 5.0
	elseif (x.ge.6.25.and.x.lt.8.75) then
          x = 7.5
        else
	  x = 10.0
	endif
      else                          ! arcmin
        if (x.ge.35*am) then
          x = 40*am
        elseif (x.ge.25*am.and.x.lt.35*am) then
          x = 30*am
        elseif (x.ge.17.5*am.and.x.lt.25*am) then
          x = 20*am
        elseif (x.ge.12.5*am.and.x.lt.17.5*am) then
          x = 15*am
        elseif (x.ge.8.75*am.and.x.lt.12.5*am) then
          x = 10*am
        elseif (x.ge.6.75*am.and.x.lt.8.75*am) then
          x = 7.5*am
        elseif (x.ge.5.5*am.and.x.lt.6.75*am) then
          x = 6*am
        elseif (x.ge.4.5*am.and.x.lt.5.5*am) then
          x = 5*am
        elseif (x.ge.3.5*am.and.x.lt.4.5*am) then
          x = 4*am
        elseif (x.ge.2.5*am.and.x.lt.3.5*am) then
          x = 3*am
        elseif (x.ge.1.5*am.and.x.lt.2.5*am) then
          x = 2*am
        elseif (x.ge.35*am*am) then
          x = 1*am
        elseif (x.ge.25*am*am) then
          x = 30*am*am
        elseif (x.ge.17.5*am*am) then
          x = 20*am*am
        elseif (x.ge.12.5*am*am) then
          x = 15*am*am
        elseif (x.ge.8.75*am*am) then
          x = 10*am*am
        elseif (x.ge.2.5*am*am) then
          x = 5*am*am
        elseif (x.ge.1.5*am*am) then
          x = 2*am*am
        else
          x = 1*am*am
        endif
          
      endif

      if ( mode.eq.1 ) then
         xdeg = x
      elseif ( mode.eq.2 ) then
         xdeg = x*15.d0
      endif

      RETURN
      END 
