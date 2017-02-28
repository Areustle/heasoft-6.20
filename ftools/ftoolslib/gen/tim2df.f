*+TIM2DF
      subroutine tim2df(time,quiet,dayfrac,status)

      implicit none
      character*(*) time
      logical       quiet
      double precision dayfrac
      integer       status
C-----------------------------------------------------------------------
C Description: parses time string and calculates fraction of day from
C              00:00:00.  If the time string is 'now' or 'NOW' then the
C              current system time is used to determine the day fraction
C
C Arguments:   time    (i) : time string in hh:mm:ss.s format
C              quiet   (i) : toggles error messages on/off (but not fiptim's)
C              dayfrac (o) : the fraction of day elapsed since 0 hrs
C              status  (o) : the success status of this routine
C
C Origin:      Written for the Calibration Database
C
C Note:        This should work for full new-style date strings
C              (yyyy-mm-ddThh:mm:ss) since fiptim works off the positions
C              of the colons.  However, maybe it really ought to call
C              fts2tm instead.  (JRG 1999-03-29)
C
C Authors/Modification History:
C Ron Zellar Nov 17, 1994 -- Orignal code removed from gtcal subroutine
C                            for use by brcif task
C Jeff Guerber Mar 29, 1999 -- Capitalize the time string so eg 'Now' works
C                            too.  Untabified.
C-----------------------------------------------------------------------
*-
      character(80)  contxt, captime
      integer       errstat, timelen
      integer       ihr, imin, iss, fcstln
      real          ss

      character(20) subname

      subname = 'tim2df'

      status = 0
      errstat = 0

C     If time parameter is 'NOW' or 'now', use system time
      timelen = fcstln(time)
      captime = time(:timelen)
      call ftupch(captime)
      if (captime .eq. 'NOW') then
          call gttime(ihr, imin, iss)
          ss = real(iss)
      else
C         Parse time into hour min and secs
          call fiptim(time,ihr,imin,ss,errstat)
          if (errstat .ne. 0) then
              if (.not. quiet) then
                  contxt=subname(:fcstln(subname))//' : '//
     &                'Unable to parse time argument '//time(:timelen)
                  call fcerr(contxt)
              endif
              status = 70
              return
          endif
      endif

C     Convert ihr, imin, ss to fraction of day
      dayfrac = (dble(ihr)*3600.+dble(imin)*60.+dble(ss))/(86400.)

      return
      end
