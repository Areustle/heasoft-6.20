C******************************************************************************
C FTOOLS TASK:
C      sec2time
C
C FILE:
C      sec2time.f
C
C DESCRIPTION:
C      Convert time offset to absolute time in IRAF parameter file
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       August, 1992
C
C MODIFICATION HISTORY:
C       11/2/93 EAG add leapseconds, extensively changed to use time conversion
C                   subroutines
C       1/25/94 EAG 2.7a more expanatory output
C       3/16/94 EAG 2.8a searching for leapsec file with caldb routine
C                        change order of parameters, leapfile not hidden
C       4/1/94  EAG 2.8b various bug fixes, actually in misc.for
C       4/22/94 EAG 2.8c more problems with leapsecs, actually in misc.for
C       1998-07-01 Jeff Guerber 2.8d  Fixed formats to handle new fits dates
C       2005-12-27 Bryan Irby 2.8e Fixed memory corruption in call to fgtcal
C       2006-01-04 Bryan Irby 2.9 Replaced call to fgtcal (routine removed
C                                 from ftoolslib) with its functionality.
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine gsec2t - gets parameters from environment
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine sec2te

      double precision offset, refsecs, abssecs, mjd, second
      character(70) datezero, timezero, date, time
      character(160) leapfile
      integer status
      integer day, month, year, hour, minute, refyear
      real realsec

      parameter (refyear= 1993)

      character(120) context
      character(40) taskname
      common /task/ taskname

      taskname = 'sec2time2.9'
      status=0

C       get all the input parameters
      call gsec2t(offset, datezero, timezero, date, time, leapfile,
     &     status)
      if (status .gt. 0) then
         context = ' error getting parameters'
         call fcerr(context)
         go to 999
      endif

C check for offset less than 100 years
      if (abs(offset) .gt. 3.1608e9) then
         context = ' offset greater than 100 years'
         call fcerr (context)
         goto 999
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

C convert to seconds from reference time
      call int2sec (year, month, day, hour, minute, second, refyear,
     &     leapfile, refsecs, status)

C add (or subtract) the offset to the zero date
      abssecs = refsecs + offset

C convert from seconds to integer format
      call sec2int (abssecs, refyear, leapfile, year, month, day,
     &     hour, minute, second, status)

C convert from integer to string format
      call jnt2str (year, month, day, hour, minute, second, date,
     &     time, status)

C convert from integer to mjd format
      call int2mjd (year, month, day, hour, minute, second, mjd,
     &     status)

C write to the parameter file
      call uclpst ('date', date, status)
      call uclpst ('time', time, status)
      call uclpsd ('mjd', mjd, status)
      if (status .ne. 0) call fcerrm(status)
      write (context, 2999) offset, datezero, timezero
 2999 format(' Offset of',1pg23.15, ' from ', a10,3x,a14)
      call fcecho(context)
      write (context, 3000) date, time
 3000 format(' is date and time (including leapseconds) = ',a10,3x,a14)
      call fcecho (context)
      write (context, 3001) mjd
 3001 format (' Modified Julian Day    = ',f23.15)
      call fcecho (context)

 999  return
      end

C******************************************************************************
C SUBROUTINE:
C      gsec2t
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       26 August, 1992
C
C MODIFICATION HISTORY:
C       2005-12-27 Bryan Irby Fixed memory corruption in call to fgtcal
C       2006-01-04 Bryan Irby Replaced call to fgtcal (routine removed
C                             from ftoolslib) with its functionality.
C
C NOTES:
C      gsec2t uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gsec2t(offset,datezero,timezero,date,time,leapfile,status)
C
C ARGUMENTS:
C    output:
C       offset   - offset time in seconds
C       datezero - start time of offset
C       timezero - start time of offset
C       date     - absolute date
C       time     - absolute time
C       leapfile - location of the leapsecond fits file
C       status   - returned error status (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsr - get real parameter
C      subroutine fgfcal - gets the system dependent path to the calibration
C                          file in the refdata directory
C      subroutine gtcalf - gets the system dependent path to the calibration
C                          file in the Caldb
C
C******************************************************************************
      subroutine gsec2t (offset, datezero, timezero, date, time,
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

C  get the offset value
      call uclgsd ('offset', offset, status)
      if (status .ne. 0) then
         context = 'could not get offset parameter'
         call fcerr(context)
         goto 999
      endif

C get the location of the leapsecond file
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
      end

C******************************************************************************
