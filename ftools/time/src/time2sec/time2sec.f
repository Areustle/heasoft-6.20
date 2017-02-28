C******************************************************************************
C FTOOLS TASK:
C      time2sec
C
C FILE:
C      time2sec.f
C
C DESCRIPTION:
C      Convert absolute time to time offset in IRAF parameter file
C
C AUTHOR:
C      Emily A. Greene
C	Hughes STX
C	August, 1992
C
C MODIFICATION HISTORY:
C	11/2/93 EAG modified to include leapseconds
C	1/25/94 EAG 2.7a more explanatory output
C       3/16/94 EAG/RZ 2.8a leapfile through CALDB routines
C                           leapfile not hidden, change order of parameters
C       4/1/94  EAG 2.8b bug fixes, actually in misc.for
C       4/22/94 EAG 2.8c more bug fixes, actually in misc.for
C       1998-07-01 Jeff Guerber 2.8d.  Fixed format for new dates; removed
C           unused daysinmonth array.
C       2005-12-27 Bryan Irby 2.8e Fixed memory corruption in call to fgtcal
C       2006-01-04 Bryan Irby 2.9 Replaced call to fgtcal (routine removed
C                                 from ftoolslib) with its functionality.
C
C NOTES:
C      Dates may now be yyyy-mm-dd.  2-digit years assumed 19yy.
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine gtimes - gets parameters from environment
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine time2c
      double precision offset
      character(70) datezero, timezero, date, time
      character(160) leapfile
      integer status

      integer day, month, year, hour, minute
      integer aday, amonth, ayear, ahour, aminute
      double precision second, asecond, refsecs, abssecs

      integer  refyear
      parameter (refyear = 1993)
      real realsec

      character(120) context
      character(40) taskname
      common /task/ taskname

      taskname = 'time2sec2.9'
      status=0

C       get all the input parameters
      call gtimes(offset, datezero, timezero, date, time, leapfile,
     &     status)
      if (status .gt. 0) then
         context = ' error getting parameters'
         call fcerr(context)
         go to 999
      endif

C convert datezero to integer day, month, year
      call fidate (datezero, day, month, year, status)
      if (status .gt. 0) then
         context = ' error converting datezero to integers'
         call fcerr (context)
         goto 999
      endif

C convert timezero to integer hour, minute, real seconds
      call fiptim (timezero, hour, minute, realsec, status)
      second = realsec
      if (status .gt. 0) then
         context = ' error converting timezero string to values'
         call fcerr (context)
         goto 999
      endif

C convert zero time to seconds
      call int2sec (year, month, day, hour, minute, second, refyear,
     &     leapfile, refsecs, status)

C convert date to integer day, month, year
      call fidate (date, aday, amonth, ayear, status)
      if (status .gt. 0) then
         context = ' error converting date to integers'
         call fcerr (context)
         goto 999
      endif

C convert time to integer hour, minute, real seconds
      call fiptim (time, ahour, aminute, realsec, status)
      asecond = realsec
      if (status .gt. 0) then
         context = ' error converting time string to values'
         call fcerr (context)
         goto 999
      endif

C convert time to seconds
      call int2sec (ayear, amonth, aday, ahour, aminute, asecond,
     &     refyear, leapfile, abssecs, status)

C calculate the seconds between the two times
      offset = abssecs - refsecs

C write to the parameter file
      call uclpsd ('offset', offset, status)
      if (status .ne. 0) call fcerrm(status)
      write (context, 1000) datezero, timezero
 1000 format (' offset in seconds (including leapseconds) from ',
     &     a10,3x,a13,'is')
      call fcecho (context)
      write (context, 1001) offset
 1001 format (1pe23.15)
      call fcecho (context)

 999  return
      end

C******************************************************************************
C SUBROUTINE:
C      gtimes
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Emily A. Greene
C	Hughes STX
C	31 August, 1992
C
C MODIFICATION HISTORY:
C       2005-12-27 Bryan Irby Fixed memory corruption in call to fgtcal
C       2006-01-04 Bryan Irby Replaced call to fgtcal (routine removed
C                             from ftoolslib) with its functionality.
C
C NOTES:
C      gtimes uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gtimes(offset,datezero,timezero,date,time,leapfile,status)
C
C ARGUMENTS:
C    output:
C	offset   - offset time in seconds
C	datezero - start time of offset
C	timezero - start time of offset
C	date     - absolute date
C	time     - absolute time
C	status   - returned error status (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsr - get real parameter
C      subroutine fgfcal - gets the system dependent path to the calibration
C                          file in the refdata directory
C      subroutine gtcalf - gets the system dependent path to the calibration
C                          file in the Caldb
C
C******************************************************************************
      subroutine gtimes (offset, datezero, timezero, date, time,
     &     leapfile, status)

      double precision offset
      character*(*) datezero, timezero, date, time, leapfile
      integer status

      character(80) context
      character(12) calfile
      character(3) telescope, instrument
      character(8) codename

      character(16) detnam, filt, expr, online
      integer inlen, fcstln, extno, chatter, maxret, nfound, nret

C  get the date
      call uclgst('date', date, status)
      if (status .ne. 0) then
         context = 'could not get date parameter'
         call fcerr(context)
         goto 999
      endif

C  get the time
      call uclgst('time', time, status)
      if (status .ne. 0) then
         context = 'could not get time parameter'
         call fcerr(context)
         goto 999
      endif

C get the location of the leapsecond fits file
      call uclgst ('leapfile', leapfile, status)
      if (status .ne. 0) then
         context = ' could not get leapfile parameter'
         call fcerr (context)
         goto 999
      endif

C  get the offset date
      call uclgst('datezero', datezero, status)
      if (status .ne. 0) then
         context = 'could not get datezero parameter'
         call fcerr(context)
         goto 999
      endif

C  get the offset time
      call uclgst('timezero', timezero, status)
      if (status .ne. 0) then
         context = 'could not get timezero parameter'
         call fcerr(context)
         goto 999
      endif

C tranlate any special file name values into their full path
      calfile='leapsec.fits'
      telescope='GEN'
      instrument='INS'
      codename='LEAPSECS'

      inlen = fcstln(leapfile)
      if (leapfile(:inlen) .eq. 'FTOOLS') then
         call fgfcal(leapfile,calfile,status)
         extno = -99
      else if (leapfile(:inlen) .eq. 'CALDB') then
         detnam='-'
         filt='-'
         expr='-'
         online=''
         chatter=0
         maxret=1
         nfound=0
         nret=0
         call gtcalf(chatter,telescope,instrument,detnam,filt,codename,
     &               date,time,datezero,timezero,expr,maxret,leapfile,
     &               extno,online,nret,nfound,status)
      endif
      if (status .ne. 0) then
         context = ' could not translate leapfile parameter'
         call fcerr(context)
         goto 999
      endif

999   continue
      return
      end

C******************************************************************************
