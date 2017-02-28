       subroutine daytofile(startday, stopday, startfile, stopfile,
     &     dayfile)

       integer startday, stopday, startfile, stopfile
       integer firstday(1009), lastday(1009)

       character(80) message

       character(12) dummy
       character*(*) dayfile
       integer j,unitnum,ierr,status


       prevfday = 0
       prevlday = 0
       startfile = 0
       stopfile = 0

       call getlun(unitnum)
       call openwr(unitnum,dayfile,'OLD',' ',
     &         ' ',0,1, status)
       read (unitnum,*, iostat=ierr) dummy
       read (unitnum,*, iostat=ierr) (firstday(j),j=1,1009)
       read (unitnum,*, iostat=ierr) dummy
       read (unitnum,*, iostat=ierr) (lastday(j),j=1,1009)

       if (startday .le. 224) startfile = 1
       if (stopday .ge. 739) stopfile = 1009 
       do 100 i = 1, 1009
          day = firstday(i)
          if ((day .eq. startday) .and. (prevfday .lt. startday)) then
             startfile = i
             if (i .gt. 1) then
                if (lastday(i - 1) .eq. startday) startfile = i - 1
             endif
          endif
          prevfday = day
          day = lastday(i)
          if ((prevlday .eq. stopday) .and. (day .gt. stopday))
     +       stopfile = i
          prevlday = day
 100   end do
       if (startfile .eq. 0) then
      write(message,'(''No matching start file number for day '', i3)')
     &         startday
          call xaerror(message, 10)
          startfile = 1
       endif
       if (stopfile .eq. 0) then
        write(message,'(''No matching stop file number for day '', i3)')
     &         stopday
          call xaerror(message, 10)
          stopfile = 1009
       endif

       return

       end
