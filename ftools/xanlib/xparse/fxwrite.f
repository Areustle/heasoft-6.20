      subroutine fxwrite (Text, Destination)
c
c Text - string - the text string to be written
c Destination - integer - the destination for the string
c                         3 possible destinations:
c                             1 = STDOUT
c                             2 = STDERR
c                             4 = logfile
c
c                         1 = STDOUT 
c                         2 = STDERR
c                         4 = logfile
c                         5 = STDOUT and logfile
c                         6 = STDERR and logfile
c        fails silently for unknown destination request
c

      character*(*) Text
      integer Destination

      integer umsdest, priority, status, lstr
      integer lenact
      real rbuf
      integer nbuf
      integer gstat

      status = 0
      priority = 0

      if (Destination .eq. 1 ) then
         umsdest = 1
      else if (Destination .eq. 2) then
         umsdest = 2
      else if (Destination .eq. 4) then
         umsdest = 0
      else if (Destination .eq. 5) then
         umsdest = 1
      else if (Destination .eq. 6) then
         umsdest = 2
      else
c unknown destination request, fail silently
         return
      endif

c write to screen appropriately
      if (umsdest .gt. 0) call umsput(Text,umsdest,priority,status)

c log if requested
      lstr = max(1, lenact(Text))
      if (Destination .ge. 4) call logger (5, rbuf, nbuf, Text, lstr)

c check to see whether the global status is 0. If it is not,
c leave it alone. Otherwise, assign it to 1.     
      if(Destination.eq.6.or.Destination.eq.2) then
          call getheastatus(gstat)
          if(gstat.eq.0) then 
             gstat = 1
             call setheastatus(gstat) 
          endif
      endif

      return
      end



