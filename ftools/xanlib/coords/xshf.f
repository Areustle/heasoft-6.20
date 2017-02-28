*- xshf.sun - shf key for SUN character time
      integer*4 function xshf(ct,status)
* History :
*  5 October 1992 : SUN original
* Author :
*  Andy Pollock (EXOSAT::ANDY)

      include 'status.codes'

* Import :
*  ct - character time representation
      character*(*) ct
* Status :
      integer status

* Local constants :
*  shf0 - shf key at the origin of SUN system time at 00:00:00 GMT 1 Jan 1970
      integer*4 shf0
      parameter (shf0=-315532800)
*  np - no of past events
      integer np
      parameter (np=10)

* Local variables :
      integer*4 t
      character(24) s
      integer*4 ls
      character(24) datum
      integer*4 ld
      real*4 x
      integer*4 it(9)
      integer*4 i, today

* System reference :
      integer*4 time
* External reference :
      logical*4 isydoy
      logical*4 isctime
      logical*4 isxdattim
      integer*4 lenact

* Local data :
      character(80) event(np)
      data event/'NOW',
     &           'TODAY',
     &           'YESTERDAY',
     &           'LAST_WEEK',
     &           'LAST_MONTH',
     &           'LAST_YEAR',
     &           '_DAYS_AGO',
     &           '_WEEKS_AGO',
     &           '_MONTHS_AGO',
     &           '_YEARS_AGO'/
*-
      if(status.ne.ok__) then
          xshf = 0
          return
      endif

      s=ct

      if(isydoy(s))then
         call timsk(s,xshf)
         return
      else if(isxdattim(s))then
         call timxk(s,xshf)
         return
      endif

      ls=lenact(s)
      call upc(s)
      i=index(s,'_')
      if(i.ge.1)then
         datum=s(1:i-1)
         if(datum.ne.'LAST')then
            read(datum,*)x
            s=s(i:ls)
         endif
      endif
      call gmatch(event,np,s,i,status)
      if(status.eq.ok__)then
         t=time()
         call ltime(t,it)
         today=(3600*it(3)+60*it(2)+it(1))
      else if(isctime(s))then
         xshf = 0
         return
      endif

      if(s.ne.'NOW')t=t-today

      if(s.eq.'YESTERDAY')then
         t=t-86400
      else if(s.eq.'LAST_WEEK')then
         t=t-(7+it(7))*86400
      else if(s.eq.'LAST_MONTH')then
         t=t-(31+it(4))*86400
      else if(s.eq.'LAST_YEAR')then
         t=t-(366+it(8))*86400
      else if(s.eq.'_DAYS_AGO')then
         t=t-nint((x*real(86400)))
      else if(s.eq.'_WEEKS_AGO')then
         t=t-nint((x*real(7*86400)))
      else if(s.eq.'_MONTHS_AGO')then
         t=t-nint((x*real(31*86400)))
      else if(s.eq.'_YEARS_AGO')then
         t=t-nint((x*real(366*86400)))
      endif

      xshf=shf0+t

      return
      
      end
