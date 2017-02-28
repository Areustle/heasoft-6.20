*+dt2mjd
      subroutine dt2mjd(indate,quiet,mjd,status)

      implicit none
      character*(*) indate
      logical       quiet
      double precision mjd
      integer       status

C-----------------------------------------------------------------------
C Description: parses date string and calculates a modified julian day.
C              If the date string is 'now' or 'NOW' then the current
C              system date is used to determine the MJD.
C
C Arguments:   indate (i) : date string in dd/mm/yy or yyyy-mm-dd format
C              quiet  (i) : toggles error messages on/off
C              mjd    (r) : the modified julian day
C              status (r) : the success status of this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar   (1.0.0:94 Nov 17) Orignal code removed from gtcal subroutine
C                                 for use by brcif task
c  Ian M George (1.1.0:96 Feb 05) Additional error checking, plus added
c                                 CALDB-standard messsage writers
c                                 Also fixed indate - was falling over w/ 'now'
c  Jeff Guerber (1.2.0: 1998 Jun 5)  Y2K: Call new cfitsio routines that parse
c       both dd/mm/yy and yyyy-mm-dd dates, and for system date.  Year now
c       guaranteed correct, so call ccldj directly instead of via ccaldj.
c       ftupch(date) allows eg. 'Now' also.
c
      character(5) version
      parameter (version='1.2.0')
*-
c Internals
      character(6) subname
      parameter (subname = 'dt2mjd')
      character(30)  date
      character(80)  contxt
      integer       errstat,datelen
      integer       id, im, iy, fcstln

c Initialize
      status = 0
      errstat = 0
      date = indate

C If date parameter is 'NOW' or 'now', use system date
      call ftupch( date )
      datelen = fcstln(date)
      if ( date(:datelen) .eq. 'NOW') then
          call ftgsdt( id, im, iy, errstat )
          if (errstat .ne. 0) then
              contxt = 'Unable to get system date'
              if(.not.quiet) call wterrm(subname,version, contxt)
              status = 50
              goto 999
          endif

      else

C Parse date into day month and year
          call fts2dt( date, iy, im, id, errstat )
          if (errstat .ne. 0) then
              contxt = 'Unable to parse date argument: '//
     &            date(:fcstln(date))
              if(.not.quiet) call wterrm(subname,version, contxt)
              status = 50
              goto 999
          endif
      endif

C Convert id, im, iy to modified julian date.  New fitsio date functions
C guarantee 4-digit years, so call ccldj instead of ccaldj.

      call ccldj(iy,im,id,mjd,errstat)
      if (errstat .ne. 0) then
          if(.not.quiet) call wterrm(subname,version,
     &        'CCLDJ unable to calculate a MJD')
          status = 60
          goto 999
      endif


  999 if(status.ne.0) then
          call wterrm(subname,version,'Aborting')
      endif

      return
      end
