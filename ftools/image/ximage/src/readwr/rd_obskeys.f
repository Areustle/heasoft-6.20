      subroutine rd_obskeys (Lun, Begobs, Endobs, Status)
      
      implicit none
c
c  Given a unit number for an open FITS file, reads and returns 
c  keyword information on observation date and time in integer arrays
c
c  I  Lun    (i)  Logical unit of open FITS file
c  O  Begobs (c)  Date/time for observartion begin
c  O  Endobs (c)  Date/time for observation end
c  O  Status (i)  Error flag  (0=OK)
c
c  Begobs, Endobs format => yyyy-mm-ddThh:mm:ss.s
c
      integer*4 Lun, Status
      character*(*) Begobs, Endobs 
c      
c Local variables
c
      integer*4 year, daynum, mon, day, hr, min
      real*8 sec, dd
      character(80) comment
      character(80) datestr,timestr
      integer*4 di
c
c get observation start
c
      Status = 0
      Begobs = ' '
      Endobs = ' '
      datestr = ' '
      timestr = ' '
      
      status = 0
      call ftgrec(Lun,0,comment,status)
      call ftgkys(Lun,'DATE?OBS',datestr,comment,status)
      if ( status.eq.0 ) then
         call XWRITE(' Using DATE-OBS keyword', 20)
         call fts2tm(datestr,year,mon,day,hr,min,sec,status)
         call ftgrec(Lun,0,comment,status)
         if ( status.eq.0 .and. hr.eq.0 .and. min.eq.0 .and.
     &        sec.eq.0 ) then
            call ftgkys(Lun,'TIME?OBS',timestr,comment,status)
            if ( status.eq.0 ) then
               call XWRITE(' Using TIME-OBS keyword', 20)
               call fts2tm(timestr,di,di,di,hr,min,sec,status)
            endif
         endif
      else
         status = 0
         call ftgkys(Lun,'OBS_STAR',datestr,comment,status)
         if ( status.eq.0 ) then
            call XWRITE(' Using OBS_STAR keyword', 20)
c           ** Format  yy/ddd hh:mm:ss.s
            di = INDEX(datestr,'/')
            if ( di.eq.0 ) status = -1
            if ( status.eq.0 ) then
               call strnum(datestr(:di-1), -4, dd, status)
               year = int(dd)
               if ( year.lt.1000 ) year = year + 1900
               datestr(:di) = ' '
               call rmvlbk(datestr)
               di = INDEX(datestr,' ')
               if ( di.eq.0 ) status = -1
            endif
            if ( status.eq.0 ) then
               call strnum(datestr(:di-1), -4, dd, status)
               daynum = int(dd)
               call xydmd(year,daynum,mon,day)
               datestr(:di) = ' '
               call rmvlbk(datestr)
            endif
            call fts2tm(datestr,di,di,di,hr,min,sec,status)
         endif
      endif
c
c   Write begin date string
c
      if ( status.eq.0 ) then
         call fttm2s(year,mon,day,hr,min,sec,2,Begobs,status)
      endif
      
c
c get observation end
c
      datestr = ' '
      timestr = ' '
      
      status = 0
      call ftgrec(Lun,0,comment,status)
      call ftgkys(Lun,'DATE?END',datestr,comment,status)
      if ( status.eq.0 ) then
         call XWRITE(' Using DATE-END keyword', 20)
         call fts2tm(datestr,year,mon,day,hr,min,sec,status)
         call ftgrec(Lun,0,comment,status)
         if ( status.eq.0 .and. hr.eq.0 .and. min.eq.0 .and.
     &        sec.eq.0 ) then
            call ftgkys(Lun,'TIME?END',timestr,comment,status)
            if ( status.eq.0 ) then
               call XWRITE(' Using TIME-END keyword', 20)
               call fts2tm(timestr,di,di,di,hr,min,sec,status)
            endif
         endif
      else
         status = 0
         call ftgkys(Lun,'OBS_END',datestr,comment,status)
         if ( status.eq.0 ) then
            call XWRITE(' Using OBS_END keyword', 20)
c           ** Format  yy/ddd hh:mm:ss.s
            di = INDEX(datestr,'/')
            if ( di.eq.0 ) status = -1
            if ( status.eq.0 ) then
               call strnum(datestr(:di-1), -4, dd, status)
               year = int(dd)
               if ( year.lt.1000 ) year = year + 1900
               datestr(:di) = ' '
               call rmvlbk(datestr)
               di = INDEX(datestr,' ')
               if ( di.eq.0 ) status = -1
            endif
            if ( status.eq.0 ) then
               call strnum(datestr(:di-1), -4, dd, status)
               daynum = int(dd)
               call xydmd(year,daynum,mon,day)
               datestr(:di) = ' '
               call rmvlbk(datestr)
            endif
            call fts2tm(datestr,di,di,di,hr,min,sec,status)
         endif
      endif
c
c   Write begin date string
c
      if ( status.eq.0 ) then
         call fttm2s(year,mon,day,hr,min,sec,2,Endobs,status)
      endif

      return
      end
