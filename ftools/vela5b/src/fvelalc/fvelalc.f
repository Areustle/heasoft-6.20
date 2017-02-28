C*****************************************************************************
C SELECTOR TASK:
C   fvelalc
C
C FILE:
C   fvelalc.f
C
C DESCRIPTION:
C   Constructs a light curve from the Vela 5B data.  The light curve
C is stored as FITS file with a binary table extension.  The FITS headers,
C keywords, and general format are compliant with the standard of the
C Office of Guest Investigator Programs (OGIP) at the HEASARC/GSFC/NASA.
C
C AUTHOR/DATE:
C   pre-FTOOLS  Jim Lochner and Laura Whitlock
C   FTOOLS      Jesse Allen
C
C MODIFICATION HISTORY:
C      May 1993 - Date of SVELA program from which FTOOL is based
C   18 Nov 1994 - Initial conversion to FTOOLS, uses FITS raw data files
C   12 Jan 1995 - Development FTOOL converted to use dynamic memory arrays
C    5 Jul 1995 - Channel 0 error in data reading repaired
C   15 Aug 1995 - Eliminated the (obsolete) trend binning option
C   18 Dec 1995 - Force a minimum value of 50 for IMAX to prevent errors
C                  with dynamic memory allocation with udmget
C    5 Deb 1996 - Modified reading in raw data boxes to reject data flagged
C                  as bad on the NOS side
C   13 Aug 1998 Jeff Guerber - Y2K: Use 4-digit years internally (but still
C                  let user enter 2), julian supports them now.
C
C NOTES:
C     Maximum array size is set by a user-selectable parameter IMAX.  The
C default setting is 500,000 which requires 10 Mb for the storage of 5
C one dimensional double precision arrays.
C
C     To maintain compatibility with old non-ftools software, Vela ftools
C will continue to write DATE-OBS and DATE-END in the old date format.  Since
C these dates cannot exceed 1979, there is no chance of an overflow.
C
C MAKE:
C   HOST: make fvelalc
C   IRAF:
C
C USAGE:
C   HOST: fvelalc
C   IRAF:
C
C ARGUMENTS:
C   none
C
C PRIMARY LOCAL VARIABLES:
C   COMMON BLOCKS:
C      begintime, endtime  Modified julian date boundaries for data inclusion
C      long_src, lat_src   Galactic coordinates (lII, bII) of the source
C      searchrad           Radius around source position to search for data
C      maxflux, maxerr     Upper bound limits for flux and variance
C      backopt, collim     Options for background and collimator corrections
C      binopt, weight      Options for binning data
C
C    LOCAL:
C      numofboxes          The number of box files with relevant data
C      boxlist             A list of the relevant box numbers
C      rtimbin             Size of the bins into which the data will be
C                           summed (seconds)
C
C CALLED ROUTINES:
C      gsource             Gets source parameters
C      createlc            Finds, reads, and writes the data into a light curve
C
C*****************************************************************************

      subroutine fvelac

      implicit none

C Common block declarations

       common /SOURCE/ begintime, endtime, long_src, lat_src,
     +        searchrad, minflux, maxflux, maxerr, stimbin,
     +        backopt, binopt, collim, spincheck, pointcheck, weight
       common /NAME/ sourcename

       logical collim, spincheck, pointcheck, weight
       integer backopt, binopt
       real long_src, lat_src, searchrad, minflux, maxflux, maxerr
       real stimbin
       double precision begintime, endtime
       character(16) sourcename

       common /TASK/ taskname

       character(40) taskname

C Local variables

       integer beginyr, beginday, endyr, endday, imax, status

       double precision julian

       character(80) message


C Begin program

      taskname = 'FVELALC v1.3a'
      call ftcmsg

C Get parameters from the par file

       call gsource(beginyr, beginday, endyr, endday, imax, status)
       if (status .ne. 0) goto 999

C Echo parameters to the user's screen

       message = ' Selected source is named ' // sourcename
       call fcecho(message)
       write(message,'('' Source has galactic coordinates '',
     +   F6.2, '', '', F6.2)') long_src, lat_src
       call fcecho(message)
       write(message,'('' Search area around the source (deg): '',
     +   F5.2)') searchrad
       call fcecho(message)
       if (collim) then
          message =
     +      ' Collimator response function corrections will be applied.'
       else
          message = ' No collimator response corrections applied.'
       endif
       call fcecho(message)
       if (backopt .EQ. 0) then
          message = ' No background subtraction (raw counts).'
       else if (backopt .EQ. 1) then
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
     +      ' Rejecting data which is flagged for unstable spin period'
       else
          message = ' Ignoring spin period stability flags'
       endif
       call fcecho(message)
       if (pointcheck) then
          message =
     +      ' Rejecting data which is flagged for pointing error(s)'
       else
          message = ' Ignoring pointing error flags'
       endif
       call fcecho(message)

       write(message,'('' Maximum count: '', F8.2,
     +      '' Maximum variance: '', F8.2)') maxflux, maxerr
       call fcecho(message)
       write(message,'('' Minimum count: '', F8.2)') minflux
       call fcecho(message)

       write(message,'('' Reading data starting day '', i3,
     +      '' of '', i4.4, '' and ending day '', i3, '' of '',
     +      i4.4)') beginday, beginyr, endday, endyr
       call fcecho(message)

       if (binopt .eq. 0) then
          message = ' No binning of the data (1 s time resolution) '
          call fcecho(message)
       else
          if (weight) then
             message = ' Binning will be weighted by the background '
             call fcecho(message)
          else
             message = ' Using straight weighted binning '
             call fcecho(message)
          endif
          if (binopt .eq. 1) then
             message = ' 10 day binning of the data selected '
             call fcecho(message)
          else if (binopt .eq. 2) then
             message = ' Natural 56 hour data binning selected '
             call fcecho(message)
          else if (binopt .eq. 3) then
             write(message,'('' Summing data into '', F9.2,
     +            '' second bins'')') stimbin
             call fcecho(message)
          else
             message = ' Illegal binning option specified, aborting.'
             call fcerr(message)
             go to 999
          endif
       endif

C Convert user provided starting and ending times into modified julian
C  dates

       begintime = julian(beginyr, beginday, 0.0) - 2400000.5D0
       endtime = julian(endyr, endday, 86399.0) - 2400000.5D0
       call createlc(imax, status)

 999   return

       end

C----------------------------------------------------------------------------
C Calls parameters for FVELALC task from the parameter file
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0   18 Nov 1994  First draft
C        1.1    5 Jan 1995  Added data binning features and error checking
C        1.2   12 Jan 1995  Pick up maximum size for dynamic memory use IMAX

       subroutine gsource(beginyr, beginday, endyr, endday, imax,
     +            irafsts)

       implicit none

C Common block declarations

       common /SOURCE/ begintime, endtime, long_src, lat_src,
     +        searchrad, minflux, maxflux, maxerr, stimbin,
     +        backopt, binopt, collim, spincheck, pointcheck, weight
       common /NAME/ sourcename

       logical collim, spincheck, pointcheck, weight
       integer backopt, binopt
       real long_src, lat_src, searchrad, minflux, maxflux, maxerr
       real stimbin
       double precision begintime, endtime
       character(16) sourcename

C Local variable declarations

       integer beginyr, beginday, endyr, endday
       integer imax, irafsts

       character(80) message

C Initialize error checking flag

       irafsts = 0

       call uclgsi('imax', imax, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get IMAX parameter'
          call fcerr(message)
          go to 999
       endif

C Get the name of the source for which the light curve is being generated

       call uclgst('sourcename', sourcename, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get SOURCENAME parameter'
          call fcerr(message)
          go to 999
       endif

C Get the galactic longitude and latitude of the source.  Constrain the
C longitude to lie within 0.0 - 360.0 degrees (e.g. set a longitude of
C -179.0 deg to 181.0 deg).  Reject illegal latitude values.

       call uclgsr('long_src', long_src, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get LONG_SRC parameter'
          call fcerr(message)
          go to 999
       endif
       if ((long_src .gt. 360.0) .or. (long_src .lt. 0.0))
     +    long_src = MOD(long_src, 360.0)

 100   call uclgsr('lat_src', lat_src, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get LAT_SRC parameter'
          call fcerr(message)
          go to 999
       endif
       if ((lat_src .lt. -90.0) .or. (lat_src .gt. 90.0)) then
          message = ' Illegal value! -90.0 <= lat <= 90.0 '
          call fcecho(message)
          message = ' Please enter a valid latitude now.'
          call fcecho(message)
          go to 100
       endif

C Get the radius around the source from which to extract the data

       call uclgsr('searchrad', searchrad, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get SEARCHRAD parameter'
          call fcerr(message)
          go to 999
       endif

C Get the processing method options (background type to use, collimator
C  reposnse, maximum flux and error values to include)

       call uclgsb('collim', collim, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get COLLIM parameter'
          call fcerr(message)
          go to 999
       endif

 200   call uclgsi('backopt', backopt, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get BACKOPT parameter'
          call fcerr(message)
          go to 999
       endif
       if ((backopt .ne. 0) .and. (backopt .ne. 1) .and.
     +     (backopt .ne. 2)) then
           message = ' Illegal value!  Available options are 0, 1, & 2'
          call fcecho(message)
          message = ' Please enter a valid background option now.'
          call fcecho(message)
          go to 200
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

C Get the beginning and end times for the light curve creation.  If the
C user enters the last 2 digits (e.g. 78 instead of 1978), accept the entry
C and correct it.  Accept ending year entries up to 1980 (e.g. although
C 1980 day 1 is after the end of the mission, accept this as an input),
C but do not permit a start year after the end of the mission.

 300   call uclgsi('beginyr', beginyr, irafsts)
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
          go to 300
       endif

 400   call uclgsi('beginday', beginday, irafsts)
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
          go to 400
       endif

 500   call uclgsi('endyr', endyr, irafsts)
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
          go to 500
       endif


 600   call uclgsi('endday', endday, irafsts)
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
          go to 600
       endif

 700   call uclgsi('binopt', binopt, irafsts)
       if (irafsts .ne. 0) then
          message = 'Unable to get BINOPT parameter'
          call fcerr(message)
          go to 999
       endif
       if ((binopt .ne. 0) .and. (binopt .ne. 1) .and.
     +     (binopt .ne. 2) .and. (binopt .ne. 3)) then
          write(message,'('' Illegal value!  Available options '',
     +         ''are 0, 1, 2, & 3'')')
          call fcecho(message)
          message = ' Please enter a valid binning option '
          call fcecho(message)
          go to 700
        endif


       if (binopt .ne. 0) then
          call uclgsb('weight', weight, irafsts)
          if (irafsts .ne. 0) then
             message = 'Unable to get WEIGHT parameter'
             call fcerr(message)
             go to 999
          endif
       endif

       if (binopt .eq. 3) then
          call uclgsr('timbin', stimbin, irafsts)
          if (irafsts .ne. 0) then
             message = 'Unable to get TIMBIN parameter'
             call fcerr(message)
             go to 999
          endif
       endif

 999   continue

       return

       end


C----------------------------------------------------------------------------
C This subroutine calls the routines to find the boxes which contain the
C data of interest to the user, reads in all the data into the data arrays,
C performs any corrections to the data requested, bins the data, and writes
C a OGIP compliant FITS light curve file
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  12 Jan 1994
C        1.1  18 Dec 1994  Force minimum size for dynamic memory allocation
C                           (Satan's number...)

       subroutine createlc(imax, status)

       implicit none

C Common block declarations

       common /SOURCE/ begintime, endtime, long_src, lat_src,
     +        searchrad, minflux, maxflux, maxerr, stimbin,
     +        backopt, binopt, collim, spincheck, pointcheck, weight
       common /NAME/ sourcename

       logical collim, spincheck, pointcheck, weight
       integer backopt, binopt
       real long_src, lat_src, searchrad, minflux, maxflux, maxerr
       real stimbin
       double precision begintime, endtime
       character(16) sourcename

C Declarations for the use of dynamic memory
C  the following MEM common block definition is in the system iraf77.inc file
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C note:
C       data_type       value
C       logical         1
C       integer*2       3
C       Integer         4
C       Long Integer    5
C       Real            6
C       Double          7
C       Complex         8

C Local variable declarations

       integer imax, status
       integer boxlist(144), numofboxes, nx
       character(16) object
       character(20) fitsname
       character(80) message

C Pointers for the location of the arrays

       integer pttime, ptcnt, ptbkg, ptlong, ptlat

C Allocate the dynamic memory for the five data arrays
C Set IMAX to be at least 50 to avoid umdget problems

       if (imax .lt. 50) imax = 50

       pttime = 0
       ptcnt = 0
       ptbkg = 0
       ptlong = 0
       ptlat = 0

       call udmget(imax, 7, pttime, status)
       if (status .ne. 0) then
          message = ' Error allocating dynamic memory for time array'
          call fcerr(message)
          go to 999
       endif

       call udmget(imax, 6, ptcnt, status)
       if (status .ne. 0) then
          message = ' Error allocating dynamic memory for counts array'
          call fcerr(message)
          go to 999
       endif

       call udmget(imax, 6, ptbkg, status)
       if (status .ne. 0) then
          write(message,'('' Error allocating dynamic memory '',
     +         ''for background array'')')
          call fcerr(message)
          go to 999
       endif

       call udmget(imax, 6, ptlong, status)
       if (status .ne. 0) then
          write(message,'('' Error allocating dynamic memory '',
     +         ''for longitude array'')')
          call fcerr(message)
          go to 999
       endif

       call udmget(imax, 6, ptlat, status)
       if (status .ne. 0) then
          write(message,'('' Error allocating dynamic memory '',
     +         ''for latitude array'')')
          call fcerr(message)
          go to 999
       endif

C Find the number/name of the raw data files (boxes) inside the search radius
C around the source and list them in the array boxlist.

       call finddata(long_src, lat_src, searchrad, numofboxes, boxlist)

C Read the data from the FITS files into arrays, sort it, and perform
C any corrections necessary, including barycenteric corrections due to
C motion with respect to the center of mass of the Solar System.

       message = ' Reading in data from channel 1'
       call fcecho(message)
       fitsname = 'channel1.lc'
       object = sourcename
       call rdvbx(boxlist, numofboxes, 1, MEMD(pttime), MEMR(ptcnt),
     +      MEMR(ptbkg), MEMR(ptlong), MEMR(ptlat), imax, nx, status)
       if (status .ne. 0) then
          message = ' Unable to read channel 1 data, exiting '
          call fcerr(message)
          go to 999
       endif
       call wrvlc(fitsname, 20, object, long_src, lat_src, 1,
     +      stimbin, backopt, binopt, collim, .TRUE., weight,
     +      MEMD(pttime), MEMR(ptcnt), MEMR(ptbkg), nx, status)
       if (status .ne. 0) then
          message = ' Unable to write channel 1 data, exiting '
          call fcerr(message)
          go to 999
       endif

       message = ' Reading in data from channel 2'
       call fcecho(message)
       fitsname = 'channel2.lc'
       call rdvbx(boxlist, numofboxes, 2, MEMD(pttime), MEMR(ptcnt),
     +      MEMR(ptbkg), MEMR(ptlong), MEMR(ptlat), imax, nx, status)
       if (status .ne. 0) then
          message = ' Unable to read channel 2 data, exiting '
          call fcerr(message)
          go to 999
       endif
       call wrvlc(fitsname, 20, object, long_src, lat_src, 2,
     +      stimbin, backopt, binopt, collim, .TRUE., weight,
     +      MEMD(pttime), MEMR(ptcnt), MEMR(ptbkg), nx, status)
       if (status .ne. 0) then
          message = ' Unable to write channel 2 data, exiting '
          call fcerr(message)
          go to 999
       endif

C Free the dynamic memory

 999  call udmfre(pttime, 7 ,status)
      call udmfre(ptcnt, 6, status)
      call udmfre(ptbkg, 6, status)
      call udmfre(ptlong, 6, status)
      call udmfre(ptlat, 6, status)

      return

      end
