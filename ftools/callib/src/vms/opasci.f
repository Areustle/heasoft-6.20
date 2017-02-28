*+OPASCI
c    -------------------------------------------------
      subroutine opasci (unit, filename, iomode, recl, 
     &                   killit,chatter,status)
c    -------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------------
c      Open an ascii file with system dependent options on vms platforms
c
c --- VARIABLES -------------------------------------------------------------
c
      IMPLICIT NONE
      integer unit, iomode, recl, status,chatter
      character*(*) filename
      logical killit
c
c --- ARGUMENTS ---
c      unit     - input  - integer - unit number to open
c      filename - input  - string  - name of file to open ! to overwrite
c      iomode   - input  - integer - I/O mode to open file
c                                    1 - readonly
c                                    2 - new file (! overwrites if exists)
c                                    3 - read/write
c
c                                    4 - append
c      recl     - input  - integer - maximum record length <0 system default
c                                    (133 for VMS)
c      status   - output - integer - status of operation
c      killit   - input  - boolean - if true then clobber
c      chatter  - input  - integer - chattines flag
c
c --- MODIFICATION HISTORY ---
c
c Rehana Yusaf (1.0.0:Sept 13 1994), Emily Greene's FAOPEN used as basis
c     
      character(5) version
      parameter (version='1.0.0')
*-
c -----------------------------------------------------------------------
c
c Internals

      character(70) subinfo
      character(30) errstr

      if (status .ne. 0) return

      errstr = ' ERROR : OPASCI Ver '//version
      if (chatter.GE.15) then
         subinfo = ' ... using OPASCI Ver '//version
         call fcecho(subinfo)
      endif 

      if (filename(1:1) .eq. '!') then
         filename = filename(2:)
         killit = .true.
      endif

      if ((iomode .ne. 1) .and. (killit))
     &     call clobber (filename, status)
      if (status .ne. 0) then
         subinfo = errstr//' clobbering !'
         call fcecho(subinfo)
         goto 999
      endif

C check for recl<=0 and don't specify if so
      if (recl .le. 0) then

         if (iomode .eq. 1) then
            open (unit, file=filename, status='old', iostat=status,
     &           readonly, carriagecontrol = 'list')
         else if (iomode .eq. 2) then
            open (unit, file=filename, status='new', iostat=status,
     &           carriagecontrol = 'list')
         else if (iomode .eq. 3) then
            open (unit, file=filename, status='unknown', iostat=status,
     &           carriagecontrol='list')
         else if (iomode .eq. 4) then
            open (unit, file=filename, status='unknown',
     &           access='append', iostat=status, carriagecontrol='list')
         endif
      else

         if (iomode .eq. 1) then
            open (unit, file=filename, status='old', iostat=status,
     &           readonly, carriagecontrol='list', recl=recl)
         else if (iomode .eq. 2) then
            open (unit, file=filename, status='new', iostat=status,
     &           carriagecontrol = 'list', recl=recl)
         else if (iomode .eq. 3) then
            open (unit, file=filename, status='unknown', iostat=status,
     &           carriagecontrol='list', recl=recl)
         else if (iomode .eq. 4) then
            open (unit, file=filename, status='unknown',
     &           access='append',  iostat=status,
     &           carriagecontrol='list', recl=recl)
         endif

      endif

 999  return
      end


