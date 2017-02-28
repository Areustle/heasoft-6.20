
C******************************************************************************
C SUBROUTINE:
C      fctofs
C
C DESCRIPTION:
C      Given two dates and times (usually the starts of observation and GTIs),
C      find the smallest non-common part; return that date/time and the
C      offsets in seconds to each input date/time.  If the dates are Julian
C      day numbers, return the minimum of the two and the seconds to the other.
C
C AUTHOR/DATE:
C      Janice Tarrant  3/12/92
C
C MODIFICATION HISTORY:
C       EAG  1/22/93 - Added BN to format statement for VAX
C       EAG  2/19/93 - Allow for double precision
C       EAG  2/22/93 - Allow for non-integer seconds
C       EAG  5/12/93 - Rewrite to allow for Julian day specification
C       EAG  2/24/95 - Fix internal read problem in VMS
C       Jeff Guerber, 1998-06-27 - Return non-JD date in yyyy-mm-dd format
C           (but read either).  Call new cfitsio routines to parse date, time.
C           Fixed leapyears.  Clarified the description (I hope).
C       Jeff Guerber, 1998-07-03 - Replaced leap-year calc w/ new fcislpyr()
C
C NOTES:
C      Dates can be either JD/MJD, or in the calendar formats "yyyy-mm-dd"
C      or "dd/mm/yy" (latter assumed 1900).  For calendar formats,
C      "yyyy-mm-dd" is returned.  Times must be in format "hh:mm:ss.dddd"
C      Time is ignored for JD date.
C
C USAGE:
C      call fctofs(odate,otime,gdate,gtime,ndate,ntime,ooffs,goffs,
C                  julian, status)
C
C ARGUMENTS:
C      odate - observation start date
C      otime - observation start time
C      gdate - GTI observation start date
C      gtime - GTI observation start time
C      ndate - new observation start date
C      ntime - new observation start time
C      ooffs - observation time offset
C      goffs - GTI time offset
C       julian - whether the date is julian or dd/mm/yy
C       status - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      oday    - observation day
C      omonth  - observation month
C      oyear   - observation year
C      ohour   - observation hour
C      omin    - observation minute
C      osec    - observation second
C      gday    - GTI day
C      gmonth  - GTI month
C      gyear   - GTI year
C      ghour   - GTI hour
C      gmin    - GTI minute
C      gsec    - GTI second
C      odayspm - observation days per month
C      gdayspm - GTI days per month
C      dayspy  - days per year
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine fctofs(odate,otime,gdate,gtime,ndate,ntime,
     &     ooffs,goffs, julian, status)

      implicit none
      character*(*) odate, otime, gdate, gtime, ndate, ntime
      double precision ooffs, goffs
      logical julian
      integer status

      integer fccmtd, fcstln
      integer i, oday, omonth, oyear, ohour, omin, gday, gmonth,
     &     gyear, ghour, gmin, odayspm, gdayspm, dayspy, dummy
      double precision osec, gsec
      double precision ovalue, gvalue
      character(80) context
      logical fcislpyr

C if the date is julian, see if there is any offset
      if (julian) then
         read (odate(1:fcstln(odate)), '(E23.0)', err=999) ovalue
         read (gdate(1:fcstln(gdate)), '(E23.0)', err=999) gvalue

C pick the older (smaller) value and use that as the reference
         if (ovalue .lt. gvalue) then
            ooffs = 0.D0
            goffs = (gvalue - ovalue) * 86400.D0
            ndate = odate
         else
            ooffs = (ovalue - gvalue) * 86400.D0
            goffs = 0.D0
            ndate = gdate
         endif
      else
C  get the day, month and year
         call fts2dt(odate, oyear, omonth, oday, status)
         call fts2dt(gdate, gyear, gmonth, gday, status)

C  get the hours, minutes and seconds
         call fts2tm(otime, dummy, dummy, dummy, ohour, omin, osec,
     &       status)
         call fts2tm(gtime, dummy, dummy, dummy, ghour, gmin, gsec,
     &       status)
         if (status .ne. 0) then
            context = ' Problem parsing date or time keywords'
            call fcerr (context)
            goto 999
         endif

C  compare the dates and times
C  if not the same, get the offset in seconds from the new time offset
         if (oyear .eq. gyear) then
            if (omonth .eq. gmonth) then
               if (oday .eq. gday) then

C                 Same date: write in new format
                  write (ndate, 100) oyear, omonth, oday
  100             format(i4.4,'-',i2.2,'-',i2.2)

                  if (ohour . eq. ghour) then
                     if (omin .eq. gmin) then
                        if (osec .eq. gsec) then
                           ntime = otime
                           ooffs = 0.D0
                           goffs = 0.D0
                        else

C                          Same hour and minute:
                           ntime(1:6) = otime(1:6)
                           ntime(7:8) = '00'
                           ooffs = osec
                           goffs = gsec
                        endif
                     else

C                       Same hour:
                        ntime(1:3) = otime(1:3)
                        ntime(4:8) = '00:00'
                        ooffs = omin * 60. + osec
                        goffs = gmin * 60. + gsec
                     endif
                  else

C                    Same date only:
                     ntime = '00:00:00'
                     ooffs = ohour * 3600 + omin * 60 + osec
                     goffs = ghour * 3600 + gmin * 60 + gsec
                  endif
               else

C                  Same year and month: day 1
                   write (ndate, 100) oyear, omonth, 1
                   ntime = '00:00:00'
                   ooffs = (oday - 1) * 86400 + ohour * 3600 +
     &                 omin * 60 + osec
                   goffs = (gday - 1) * 86400 + ghour * 3600 +
     &                 gmin * 60 + gsec
               endif
            else

C               Same year:  Jan 1
                write (ndate, 100) oyear, 1, 1
                ntime = '00:00:00'
                odayspm = fccmtd(oyear,omonth)
                gdayspm = fccmtd(gyear,gmonth)
                ooffs = (odayspm + oday - 1) * 86400 + ohour * 3600  +
     &              omin * 60 + osec
                goffs = (gdayspm + gday - 1) * 86400 + ghour * 3600 +
     &              gmin * 60 + gsec
            endif
         else

C           Different years: oyear Jan 1
            write (ndate, 100) oyear, 1, 1
            ntime = '00:00:00'
            odayspm = fccmtd(oyear,omonth)
            gdayspm = fccmtd(gyear,gmonth)
            ooffs = (odayspm + oday - 1) * 86400 + ohour * 3600  +
     &           omin * 60 + osec

C           Calculate days (maybe neg) from start of oyear to start of gyear
            dayspy = 0
            if (oyear .lt. gyear) then
               do 10 i = oyear, gyear-1
                  if ( fcislpyr(i) ) then
                     dayspy = dayspy + 366
                  else
                     dayspy = dayspy + 365
                  endif
 10            continue
            else
               do 20 i = gyear, oyear-1
                  if ( fcislpyr(i) ) then
                     dayspy = dayspy - 366
                  else
                     dayspy = dayspy - 365
                  endif
 20            continue
            endif

            goffs = (dayspy + gdayspm + gday - 1) * 86400 +
     &           ghour * 3600 + gmin * 60 + gsec
         endif
      endif

 999  return
      end
