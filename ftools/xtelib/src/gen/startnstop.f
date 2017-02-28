C ********************************************************************
C SUBROUTINE:
C     startnstop
C
C DESCRIPTION:
C     obtain the average value of the quaternions from the XTEFILT file
C     between the input start and stop times
C
C AUTHOR:
C     James Lochner 5/95
C
C MODIFICATION HISTORY:
C
C NOTES:
C 
C USEAGE:
C     call startnstop(iunit, filnam, tstart, tstop, mjdref, ierr)
C
C ARGUMENTS:
C     iunit     - unit for input file
C     filnam    - name of input file
C     tstart    - TSTART time
C     tstop     - TSTOP time
C     mjdref    - mjd reference value for start and stop times
C     ierr      - error flag
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED SUBROUTINES:
C
C ******************************************************************

      SUBROUTINE startnstop(iunit, filnam, tstart, tstop, mjdref, ierr)

      character*(*) filnam
      integer ierr, iunit

      character(80) comment, errstr, message
      character(8) tunit
      integer tstarti, tstopi, mjdrefi
      integer ftstatus
      double precision tstart, tstartf, tstop, tstopf, mjdreff, mjdref


       ftstatus=0

C      get MJDREF value (if it doesn't exist simply keep it at 0.0)
c      Read the integer and fractional pieces of MJDREF.
c      If they do not exist (ftstatus .eq. 202), then look for the
c      MJDREF keyword.
       mjdref = 0.0
       call ftgkyj(iunit,'MJDREFI',mjdrefi,comment,ftstatus)
       if (ftstatus .eq. 202) then
          ftstatus = 0
          call ftgkyd(iunit,'MJDREF',mjdref,comment,ftstatus)
          if (ftstatus .ne. 0 .and. ftstatus .ne. 202) then
             errstr = 'error getting MJDREF value'
             call fcerr(errstr)
             goto 999
          endif
       else
          if (ftstatus .ne. 0) then
             errstr = 'error getting MJDREFI value'
             call fcerr(errstr)
             goto 999
          endif
          call ftgkyd(iunit,'MJDREFF',mjdreff,comment,ftstatus)
          if (ftstatus .ne. 0) then
             errstr = 'error getting MJDREFF value'
             call fcerr(errstr)
             goto 999
          endif
          mjdref = mjdrefi + mjdreff
       endif

C     read the start and stop times in the input file
c      Read the integer and fractional pieces of TSTART
c      If they do not exist (ftstatus .eq. 202), then look for the
c      TSTART keyword.
         call ftgkyj(iunit,'TSTARTI',tstarti,comment,ftstatus)
         if (ftstatus .eq. 202) then
            ftstatus = 0
            call ftgkyd(iunit,'TSTART',tstart,comment,ftstatus)
            if (ftstatus .ne. 0) then
               errstr = 'error getting TSTART value'
               call fcerr(errstr)
               goto 999
            endif
         else
            if (ftstatus .ne. 0) then
               errstr = 'error getting TSTARTI value'
               call fcerr(errstr)
               goto 999
            endif
            call ftgkyd(iunit,'TSTARTF',tstartf,comment,ftstatus)
            if (ftstatus .ne. 0) then
               errstr = 'error getting TSTARTF value'
               call fcerr(errstr)
               goto 999
            endif
            tstart = tstarti + tstartf
         endif

C get TIMEUNIT value (units for TSTART and TSTOP)
       call ftgkys(iunit,'TIMEUNIT',tunit,comment,ftstatus)
       if (ftstatus .ne. 0) then
          errstr = 'error getting TIMEUNIT value'
          call fcerr(errstr)
          goto 999
       endif

c      Read the integer and fractional pieces of TSTOP
c      If they do not exist (ftstatus .eq. 202), then look for the
c      TSTART keyword.
         call ftgkyj(iunit,'TSTOPI',tstopi,comment,ftstatus)
         if (ftstatus .eq. 202) then
            ftstatus = 0
            call ftgkyd(iunit,'TSTOP',tstop,comment,ftstatus)
            if (ftstatus .ne. 0) then
               errstr = 'error getting TSTOP value'
               call fcerr(errstr)
               goto 999
            endif
         else
            if (ftstatus .ne. 0) then
               errstr = 'error getting TSTOPI value'
               call fcerr(errstr)
               goto 999
            endif
            call ftgkyd(iunit,'TSTOPF',tstopf,comment,ftstatus)
            if (ftstatus .ne. 0) then
               errstr = 'error getting TSTOPF value'
               call fcerr(errstr)
               goto 999
            endif
            tstop = tstopi + tstopf
         endif


C
999     if (ftstatus .ne. 0) call fcerrm(ftstatus)
        if (ierr .ne. 0) call fcecho(message)
        
        return
        end
