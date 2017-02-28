*- shf_c11 - convert shf key to calendar char*11 representation
      character(11) function shf_c11(shf,status)
* History :
*  3 October 1988 : original
*  6 March 1990 : Nick modified to make it do to 1999
*  17 July 1990 : modified to do anything
*  17 Jun  1998 : modified to write the four digit year(from shf_c9) 
* Author :
*  Andy Pollock (EXOSAT::ANDY)
      implicit none
      include 'status.codes'
* Import :
      integer shf
* Status :
      integer status
* Local variables :
      character(23) t
      integer shf0,shf1,dt,year,day,hour,min
      character(11) string
* External reference :
      integer xshf
*-
      if(status.ne.ok__)return

      t='01-JAN-xxxx 00:00:00'
      year=1983
      write(t(8:11),'(i4)')year
      shf0=xshf(t,status)
      if(shf.ge.shf0)then
         shf1=shf0
         do while(shf.ge.shf1)
            shf0=shf1
            year=year+1
            write(t(8:11),'(i4)')year
            shf1=xshf(t,status)
         end do
         year=year-1
      else
         do while(shf.lt.shf0)
            year=year-1
            write(t(8:11),'(i4)')year
            shf0=xshf(t,status)
         end do
      endif
      write(string(1:4),'(i4)')year
      dt=shf-shf0
      day=dt/86400
      dt=dt-86400*day
      day=day+1
      write(string(5:7),'(i3.3)')day
      hour=dt/3600
      dt=dt-3600*hour
      write(string(8:9),'(i2.2)')hour
      min=dt/60
      write(string(10:11),'(i2.2)')min

      shf_c11=string

      end
