C*****************************************************************************
C SELECTOR TASK:
C   fvelmap
C
C FILE:
C   fvelmap.f
C
C DESCRIPTION:
C   Constructs a pseudo-map of Vela 5B boxes (2 deg x 2 deg squares) within
C a specified galactic region.  The resulting map is used as input to
C the task FVELGALLC which will use this pseudo-map to generate light
C curves for multiple sources in the maps.
C
C AUTHOR/DATE:
C   pre-FTOOLS  DRD
C   FTOOLS      Jesse Allen
C
C MODIFICATION HISTORY:
C   Version 0.9  27 Feb 1995  Beta test version
C           1.0  31 Aug 1995  First release version.  Passes corners to
C                              FVELGALLC via FITS keywords
C           1.1   6 Sep 1995  Changed longitude to Vela logic (> 360 okay)
C           1.2  31 Oct 1995  Removed address array
C           1.21 18 Dec 1995  Added minimum array size for dynamic memory
C                              to avoid a udmget bug
C           1.22 30 Jan 1996  Changes in binning to prevent last row in
C                              map to be all NULLs
C           1.3  15 Mar 1996  Prevents selection of the no background option
C                             Also limits data to galactic plane (-48 < l < 48)
C           1.3b 13 Aug 1998  Jeff Guerber - Y2K: use 4-digit dates internally;
C                  julian() supports them now. Let user enter 2digits & correct
C
C NOTES:
C     To maintain compatibility with old non-ftools software, Vela ftools
C will continue to write DATE-OBS and DATE-END in the old date format.  Since
C these dates cannot exceed 1979, there is no chance of an overflow.
C
C MAKE:
C   HOST: make fvelmap
C   IRAF:
C
C USAGE:
C   HOST: fvelmap
C   IRAF:
C
C ARGUMENTS:
C   none
C
C PRIMARY LOCAL VARIABLES:
C   COMMON BLOCKS:
C      sourcename          A short user-provided name for the source
C      begintime, endtime  Modified julian date boundaries for data inclusion
C      long_cnr1, lat_cnr1 Galactic coordinates (lII, bII) of one corner of
C                            the region in which to make the map
C      long_cnr2, lat_cnr2 Opposite corner of the region to be mapped
C      minflux, maxflux, maxerr
C                          Limits for flux and variance
C      backopt             Background subtraction option
C      stimbin             The size of the time bins (seconds)
C      spincheck, pointcheck
C                          Logical flags for pointing and spin errors
C      weight              Logical flag for using weighted bins
C      iwp, jwp            Number of boxes in longitude, latitude
C      address             3-D array for storing the longitude, latitude, and
C                            box numbers for each i,j map element
C   LOCAL:
C      beginday, beginyr,
C      endday, endyr       Variables to store the start and stop time for maps
C      julian              Temporary variable for converting above to MJD
C
C CALLED ROUTINES:
C      getmappar           Gets the user selected parameters
C      makemap             Make the pseudo maps
C      julian              Function to convert between Universal Time and
C                           Julian dates
C
C*****************************************************************************

      subroutine fvelmp

      implicit none

C Common block declarations

      common /SOURCE/ begintime, endtime, long_cnr1, lat_cnr1,
     +       long_cnr2, lat_cnr2, minflux, maxflux, maxerr,
     +       stimbin, backopt, iwp, jwp, spincheck, pointcheck,
     +       weight

      logical spincheck, pointcheck, weight
      integer backopt, iwp, jwp
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real minflux, maxflux, maxerr, stimbin
      double precision begintime, endtime

      common /TASK/ taskname
      character(40) taskname

C Local variables

      integer beginyr, beginday, endyr, endday, status

      double precision julian

      character(80) message

C Begin program

      taskname = 'FVELMAP v1.3b'
      call ftcmsg

C Get parameters from the par file

      call getmappar(beginyr, beginday, endyr, endday, status)
      if (status .ne. 0) goto 999

      write(message,'('' Box region starts at '',F6.2,'', '',F6.2)')
     +     long_cnr1, lat_cnr1
      call fcecho(message)
      write(message,'('' Box region ends at '',F6.2,'', '',F6.2)')
     +     long_cnr2, lat_cnr2
      call fcecho(message)
      if (backopt .EQ. 1) then
         message = ' Constant background selected.'
      else if (backopt .EQ. 2) then
         message = ' Sine background selected.'
      else
         message = ' Illegal background option selected.'
         call fcerr(message)
         goto 999
      endif
      call fcecho(message)
      if (spincheck) then
         message =
     +     ' Rejecting data which is flagged for unstable spin period'
      else
         message = ' Ignoring spin period stability flags'
      endif
      call fcecho(message)
      if (pointcheck) then
         message =
     +     ' Rejecting data which is flagged for pointing error(s)'
      else
         message = ' Ignoring pointing error flags'
      endif
      call fcecho(message)
      if (weight) then
          message = ' Binning weighted by background rate '
      else
          message = ' Straight binning selected'
      endif
      call fcecho(message)

      write(message,'('' Maximum count: '', F8.2,
     +     '' Maximum variance: '', F8.2)') maxflux, maxerr
      call fcecho(message)
      write(message,'('' Minimum count: '', F8.2)') minflux
      call fcecho(message)

      write(message,'('' Reading data starting day '', i3,
     +     '' of 19'', i2.2, '' and ending day '', i3, '' of 19'',
     +     i2.2)') beginday, beginyr, endday, endyr
      call fcecho(message)

      write(message,'('' Summing data into '',F9.2,'' second bins'')')
     +     stimbin
      call fcecho(message)

C Convert user provided starting and ending times into modified julian
C  dates

      begintime = julian(beginyr, beginday, 0.0) - 2400000.5D0
      endtime = julian(endyr, endday, 86399.99) - 2400000.5D0

C Make the light curves for each box in the specified area

      call makemap(status)

 999  return

      end

C----------------------------------------------------------------------------
C Calls parameters for FVELMAP task from the parameter file
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0   26 Jan 1995  FVELMAP routine
C        1.1    6 Sep 1995  Changed longitude to Vela logic (> 360 okay)

      subroutine getmappar(beginyr, beginday, endyr, endday, irafsts)

      implicit none

C Common block declarations

      common /SOURCE/ begintime, endtime, long_cnr1, lat_cnr1,
     +       long_cnr2, lat_cnr2, minflux, maxflux, maxerr,
     +       stimbin, backopt, iwp, jwp, spincheck, pointcheck,
     +       weight

      logical spincheck, pointcheck, weight
      integer backopt, iwp, jwp
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real minflux, maxflux, maxerr, stimbin
      double precision begintime, endtime

C Local variable declarations

      integer beginyr, beginday, endyr, endday
      integer irafsts

      character(80) message

C Initialize error checking flag

      irafsts = 0

C Get the galactic longitude and latitude of two opposite corners of the
C region in which to make a map.  Constrain the longitude to lie within
C 0.0 - 360.0 degrees (e.g. set a longitude of  -179.0 deg to 181.0 deg).
C Reject illegal latitude values.

      call uclgsr('long_cnr1', long_cnr1, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get LONG_CNR1 parameter'
         call fcerr(message)
         go to 999
      endif

 100  call uclgsr('lat_cnr1', lat_cnr1, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get LAT_CNR1 parameter'
         call fcerr(message)
         go to 999
      else if ((lat_cnr1 .lt. -48.0) .or. (lat_cnr1 .gt. 48.0)) then
         message =
     +' Maps restricted to the galactic plane ( -48 <= lat <= 48 )'
         call fcecho(message)
         message = ' Please enter a valid latitude'
         call fcecho(message)
         go to 100
      endif

      call uclgsr('long_cnr2', long_cnr2, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get LONG_CNR2 parameter'
         call fcerr(message)
         go to 999
      endif

 200  call uclgsr('lat_cnr2', lat_cnr2, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get LAT_CNR2 parameter'
         call fcerr(message)
         go to 999
      else if ((lat_cnr2 .lt. -48.0) .or. (lat_cnr2 .gt. 48.0)) then
         message =
     +' Maps restricted to the galactic plane ( -48 <= lat <= 48 )'
         call fcecho(message)
         message = ' Please enter a valid latitude'
         call fcecho(message)
         go to 200
      endif

C Get the processing method options (background type to use, maximum flux and
C error values to include)

 300  call uclgsi('backopt', backopt, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get BACKOPT parameter'
         call fcerr(message)
         go to 999
      else if ((backopt .ne. 1) .and. (backopt .ne. 2)) then
         message = ' Illegal value!  Background options are 1 or 2'
          call fcecho(message)
          message = ' Please enter a valid background option now.'
          call fcecho(message)
          go to 300
      endif

      call uclgsr('minflux', minflux, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get MINFLUX parameter'
         call fcerr(message)
         go to 999
      endif

      call uclgsr('maxflux', maxflux, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get MAXFLUX parameter'
         call fcerr(message)
         go to 999
      endif

      call uclgsr('maxerr', maxerr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get MAXERR parameter'
         call fcerr(message)
         go to 999
      endif

       call uclgsb('spincheck', spincheck, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get SPINCHECK parameter'
          call fcerr(message)
          go to 999
       endif

       call uclgsb('pointcheck', pointcheck, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get POINTCHECK parameter'
          call fcerr(message)
          go to 999
       endif

C Get the beginning and end times for the light curve creation.  If the
C user enters a 2-digit year (e.g. 78 instead of 1978), accept the entry and
C correct it.  Accept ending year entries up to 1980 (e.g. although
C 1980 day 1 is after the end of the mission, accept this as an input),
C but do not permit a start year after the end of the mission.

 400  call uclgsi('beginyr', beginyr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get BEGINYR parameter'
         call fcerr(message)
         go to 999
      endif
      if (beginyr .le. 99) beginyr = beginyr + 1900
      if ((beginyr .lt. 1969) .or. (beginyr .gt. 1979)) then
         message = ' Illegal value!  Mission spanned from 1969 to 1979'
         call fcecho(message)
         message = ' Please enter a valid starting year (1969 - 1979)'
         call fcecho(message)
         go to 400
      endif

 500  call uclgsi('beginday', beginday, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get BEGINDAY parameter'
         call fcerr(message)
         go to 999
      endif
      if ((beginday .lt. 1) .or. (beginday .gt. 366)) then
         message = ' Illegal value!  1 <= day number <= 366 '
         call fcecho(message)
         message = ' Please enter a valid day of the year '
         call fcecho(message)
         go to 500
      endif

 600  call uclgsi('endyr', endyr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get ENDYR parameter'
         call fcerr(message)
         go to 999
      endif
      if (endyr .le. 99) endyr = endyr + 1900
      if ((endyr .lt. 1969) .or. (endyr .gt. 1980)) then
         message = ' Illegal value!  Mission spanned from 1969 to 1979'
         call fcecho(message)
         message = ' Please enter a valid ending year (1969 - 1980)'
         call fcecho(message)
         go to 600
      endif

 700  call uclgsi('endday', endday, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get ENDDAY parameter'
         call fcerr(message)
         go to 999
      endif
      if ((endday .lt. 1) .or. (endday .gt. 366)) then
         message = ' Illegal value!  1 <= day number <= 366 '
         call fcecho(message)
         message = ' Please enter a valid day of the year '
         call fcecho(message)
         go to 700
      endif

C Get the data binning options.

      call uclgsr('timbin', stimbin, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get TIMBIN parameter'
         call fcerr(message)
         go to 999
      endif

      call uclgsb('weight', weight, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get WEIGHT parameter'
         call fcerr(message)
         go to 999
      endif

 999  return

      end


C----------------------------------------------------------------------------
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  12 Jan 1995
C        2.0  26 Jan 1995  Modified for FVELMAP
C        2.1  31 Oct 1995  Removed address array

      subroutine makemap(status)

      implicit none

C Common block declarations

      common /SOURCE/ begintime, endtime, long_cnr1, lat_cnr1,
     +       long_cnr2, lat_cnr2, minflux, maxflux, maxerr,
     +       stimbin, backopt, iwp, jwp, spincheck, pointcheck,
     +       weight

      logical spincheck, pointcheck, weight
      integer backopt, iwp, jwp
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real minflux, maxflux, maxerr, stimbin
      double precision begintime, endtime

C Local variables

      integer status
      integer numofboxes, boxlist(400)

      real long_cntr(400), lat_cntr(400)

      double precision firsttime

      call findmaps(long_cnr1, lat_cnr1, long_cnr2, lat_cnr2,
     +     numofboxes, boxlist, long_cntr, lat_cntr, iwp, jwp)
      call binmaps(boxlist, long_cntr(1), lat_cntr(1), numofboxes,
     +     firsttime, status)

 999  return

      end
