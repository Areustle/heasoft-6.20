*- c11_shf - convert calendar char*11 representation to shf key
      integer function c11_shf(time,status)
* History :
*  3 October 1988 : original
*  17 Jun  1998 : modified to read the four digit year(from shf_c11).
*                 Ning Gan
* Author :
*  Andy Pollock (EXOSAT::ANDY)
      implicit none
      include 'status.codes'
* Import :
      character(11) time
* Status :
      integer status
* Local variables :
      character(23) t
      integer day,hour,min
* External reference :
      integer xshf
*-
      if(status.ne.ok__) THEN
	c11_shf = 0
	return
      ENDIF
      t='01-JAN-'//time(1:4)//' 00:00:00'
      read(time(5:7),'(i3)',iostat=status)day
      read(time(8:9),'(i2)',iostat=status)hour
      read(time(10:11),'(i2)',iostat=status)min
      c11_shf=xshf(t,status)+(day-1)*86400+hour*3600+min*60
      end
