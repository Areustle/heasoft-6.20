C*****************************************************************************
C TASK NAME: bodgetvp
C
C FILE NAME: bodgetvp.f
C
C DESCRIPTION: FTOOL for obtaining CGRO viewing period information from BATSE
C     occultation data FITS file, which usually covers many viewing periods.
C
C AUTHOR/DATE: Peter J.T. Leonard, HSTX, 08/97
C
C NOTES:
C
C USAGE:
C     Host: bodgetvp
C     IRAF: bodgetvp
C
C ROUTINES IMPLEMENTED IN THIS FILE:
C     subroutine bodgep    - top level subroutine, called by IRAF or
C                            host C wrapper in hbodgetvp.c
C     subroutine read_ssdt - reads start and stop dates and times from
C                            header of BATSE occultation data FITS file
C     subroutine comp_vps  - compares start and stop Truncated Julian Dates
C                            of BATSE occultation data with CGRO viewing
C                            period information, and sends overlap to screen
C     subroutine read_vps  - reads CGRO viewing period information FITS file
C
C     subroutine getdate   - read in date from the input FITS file, and 
C                            converts it to integers of day, month, and year
C             
C MODIFICATION HISTORY:
C     James Peachey, HEASARC/GSFC/NASA, Hughes STX, 2 September, 1997
C         Porting into Ftools. Removed local copy of int2mjd, so that
C         "standard" version in libftools.a, mutatis mutandis, will be called
C         instead.
C     Ning Gan, HEASARC/GSFC/NASA, Hughes STX, 1 JULY, 1998
C         Updating for the new format of DATE keyword. 
C         Using fts2dt and fts2tm to parse the keyword.
C     Ning Gan, HEASARC/GSFC/NASA, Hughes STX, 12 October, 1998
C         Changing the secb and sece in bodgetvp and read_ssdt 
C         from real*4 to double precision. 
C     Chunhui Pan, HEASARC/GSFC/NASA, June 19, 2002
c          use subroutine getdate instead of fts2dt, use vp_list.fits
c          instead of vplist.fits
C*****************************************************************************

C*****************************************************************************
C SUBROUTINE: bodgep
C
C DESCRIPTION: FTOOL for obtaining CGRO viewing period information from BATSE
C     occultation data FITS file, which usually covers many viewing periods.
C
C AUTHOR/DATE: Peter J.T. Leonard, HSTX, 08/97
C
C NOTES:
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C     bodfil - name of BATSE occultation data FITS file
C     vplist - name of CGRO viewing period information FITS file
C
C CALLED ROUTINES:
C     subroutine read_ssdt - reads start and stop dates and times from
C                            header of BATSE occultation data FITS file
C     subroutine int2mjd   - converts integer date and time to Modified
C                            Julian Date
C     subroutine comp_vps  - compares start and stop Truncated Julian Dates
C                            of BATSE occultation data with CGRO viewing
C                            period information and sends overlap to screen
C
C MODIFICATION HISTORY:
C*****************************************************************************
 
      subroutine bodgep ()

      implicit none

      integer*4 status
      integer*4 dayb, monb, yearb, daye, mone, yeare
      integer*4 hourb, minb, houre, mine
 
      double precision secb, sece
      real*8 secbd, mjdb, seced, mjde, jdb, jde, tjdb, tjde
 
      character(64) bodfil, vplist

C Setup common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common /task/taskname
      taskname = 'bodgetvp'
 
C Get name of BATSE occultation data FITS file.
      status = 0
      call uclgst ('bodfil', bodfil, status)
 
      if (status.ne.0) then
         msg =
     &       'Problem getting name of BATSE occultation data FITS file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      end if
 
C Get name of CGRO viewing period information FITS file.
      status = 0
      call uclgst ('vplist', vplist, status)
 
      if (status.ne.0) then
         write (msg,10)
10       format ('Problem getting name of CGRO viewing ',
     &           'period information FITS file!')
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      end if

C Read start and stop dates and times from header of BATSE occultation data
C FITS file.
      call read_ssdt (bodfil, dayb, monb, yearb, daye, mone, yeare,
     &                hourb, minb, secb, houre, mine, sece, status)
 
      if (status.ne.0) then
         msg = 'Problem reading in start and stop dates and times!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      end if
      
C Convert start and stop dates and times to Modified Julian Dates.
      secbd = secb
      call int2mjd (yearb, monb, dayb, hourb, minb, secbd, mjdb, status)
      seced = sece
      call int2mjd (yeare, mone, daye, houre, mine, seced, mjde, status)

      if (status.ne.0) then
         msg =
     &    'Problem converting dates and times to Modified Julian Dates!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      end if
 
C Convert Modified Julian Dates to Julian Dates.
      jdb = 2400000.5d0 + mjdb
      jde = 2400000.5d0 + mjde
 
C Convert Julian Dates to Truncated Julian Dates.
      tjdb = jdb - 2440000.0d0
      tjde = jde - 2440000.0d0
 
C Send start and stop Truncated Julian Dates of BATSE occultation data to screen.
      write (msg,20) tjdb,tjde
20    format ('BATSE occultation data spans TJD ',f8.2,' to ',f8.2)
      call fcerr (msg)
 
C Find relevant CGRO viewing periods.
      call comp_vps (vplist, tjdb, tjde, status)
 
      if (status.ne.0) then
         msg =
     &      'Problem reading CGRO viewing period information FITS file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      end if

      call exit (0)
      end
 
C*****************************************************************************
C SUBROUTINE: read_ssdt
C
C DESCRIPTION: Reads start and stop dates and times from header
C              of BATSE occultation data FITS file.
C
C AUTHOR/DATE: Peter J.T. Leonard, HSTX, 08/97
C
C NOTES:
C
C ARGUMENTS:
c     filnam - name of BATSE occultation data FITS file
C     dayb   - begin day
C     monb   - begin month
C     yearb  - begin year
C     daye   - end day
C     mone   - end month
C     yeare  - end year
C     hourb  - begin hour
C     minb   - begin minute
C     secb   - begin second
C     houre  - end hour
C     mine   - end minute
C     sece   - end second
C     status - error status
C
C PRIMARY LOCAL VARIABLES:
C     dateb - start date string
C     datee - stop date string
C     timeb - start time string
C     timee - stop time string
C
C CALLED ROUTINES:
C
C MODIFICATION HISTORY:
C******************************************************************************

      subroutine read_ssdt (filnam, dayb, monb, yearb, daye, mone,
     &              yeare, hourb, minb, secb, houre, mine, sece, status)

      implicit none

      integer*4 unit, dummy, status
      integer*4 dayb, monb, yearb, daye, mone, yeare
      integer*4 hourb, minb, houre, mine
 
      double precision secb, sece

      character*(*) filnam
      character(68) dateb, datee
      character(68) timeb, timee
      character(30) comment
      character(130) filename
 
C Find unused logical unit number.
      call ftgiou (unit, status)
 
C Open BATSE occultation data FITS file.
      filename = filnam
      dummy = 0
      call ftopen (unit, filename, 0, dummy, status)
          
C Read start and stop date and time from FITS header.
      call ftgkys (unit, 'DATE-BEG', dateb, comment, status)
      call ftgkys (unit, 'DATE-END', datee, comment, status)
      call ftgkys (unit, 'TIME-BEG', timeb, comment, status)
      call ftgkys (unit, 'TIME-END', timee, comment, status)
 
C Close BATSE occultation data FITS file, and free logical unit.
      call ftclos (unit, status)
      call ftfiou (unit, status)
 
C Parse dates and times.
c       call  fts2dt(dateb,yearb,monb,dayb,status)
      call  getdate(dateb,yearb,monb,dayb)
      call  fts2tm(timeb,dummy,dummy,dummy, hourb,minb,secb ,status)
c      call  fts2dt(datee,yeare,mone,daye,status)
      call  getdate(datee,yeare,mone,daye)
      call  fts2tm(timee,dummy,dummy,dummy, houre,mine,sece ,status)

      return
      end
 
C*****************************************************************************
C SUBROUTINE: comp_vps
C
C DESCRIPTION: Compares start and stop Truncated Julian Dates of BATSE
C     occultation data with CGRO viewing period information, and sends
C     overlap to screen.
C
C AUTHOR/DATE: Peter J.T. Leonard, HSTX, 08/97
C
C NOTES:
C
C ARGUMENTS:
c     filnam   - name of CGRO viewing period information FITS file
C     tjdb     - start TJD of BATSE occultation data
C     tjde     - stop TJD of BATSE occultation data
C     status   - error status
C
C PRIMARY LOCAL VARIABLES:
C     nvp      - number of CGRO viewing periods
C     nfc      - first CGRO observing cycle
C     nlc      - final CGRO observing cycle
C     vp(i)    - CGRO viewing period number
C     start(i) - TJD of beginning of CGRO viewing period
C     stop(i)  - TJD of end of CGRO viewing period
C
C CALLED ROUTINES:
C     read_vps - reads CGRO viewing period information
C
C MODIFICATION HISTORY:
C*****************************************************************************
 
      subroutine comp_vps (filnam, tjdb, tjde, status)
 
      implicit none
 
      integer*4  vp(1000), nvp, nfc, nlc, status, i
 
      real*8  start(1000), stop(1000), tjdb, tjde
 
      character*(*) filnam

C Setup common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common /task/taskname
      taskname = 'bodgetvp'
 
C Read CGRO viewing period information.
      call read_vps (filnam, nvp, nfc, nlc, vp, start, stop, status)
 
C Note if BATSE occultation data starts before CGRO viewing period information.
c      if (start(1).gt.tjdb) then
c         write (msg,10) nfc
c10       format ('BATSE occultation data starts before CGRO Cycle',i2)
c         call fcerr (msg)
c      end if
 
C Loop through CGRO viewing period information.
      write (msg,20)
20    format ('BATSE occultation data includes the ',
     &        'following CGRO viewing periods:')
      call fcerr (msg)
      do i = 1, nvp
 
C Note partial CGRO viewing period at start of BATSE occultation data.
         if ((start(i).lt.tjdb).and.(stop(i).gt.tjdb)) then
            write (msg,30) vp(i), start(i), stop(i)
30          format ('CGRO viewing period ',i4,
     &              ' (TJD ',f8.2,' to ',f8.2,')')
            call fcerr (msg)
         end if
 
C Note complete CGRO viewing periods within BATSE occultation data.
         if ((start(i).gt.tjdb).and.(stop(i).lt.tjde)) then
            write (msg,30) vp(i), start(i), stop(i)
            call fcerr (msg)
         end if
 
C Note partial CGRO viewing period at end of BATSE occultation data.
         if ((start(i).lt.tjde).and.(stop(i).gt.tjde)) then
            write (msg,30) vp(i), start(i), stop(i)
            call fcerr (msg)
         end if
 
      end do
 
C Note if BATSE occultation data ends after CGRO viewing period information.
      if (stop(nvp).lt.tjde) then
         write (*,*)'BATSE occultation data is beyond CGRO vp!'
         call fcerr (msg)
      end if

      return
      end
 
C*****************************************************************************
C SUBROUTINE: read_vps
C
C DESCRIPTION: Reads CGRO viewing period information FITS file.
C
C AUTHOR/DATE: Peter J.T. Leonard, HSTX, 08/97
C
C NOTES: CGRO viewing period information FITS files is called VPLIST.FITS,
C     and lives in /refdata. This information is courtesy of Tom Bridgman,
C     and is based on OSSE observing records.
C
C ARGUMENTS:
c     filnam   - name of CGRO viewing period information FITS file
C     nvp      - number of CGRO viewing periods
C     nfc      - first CGRO observing cycle
C     nlc      - final CGRO observing cycle
C     vp(i)    - CGRO viewing period number
C     start(i) - TJD of beginning of CGRO viewing period
C     stop(i)  - TJD of end of CGRO viewing period
C     status   - error status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C MODIFICATION HISTORY:
C*****************************************************************************
 
      subroutine read_vps (filnam, nvp, nfc, nlc,
     &                     vp, start, stop, status)
 
      implicit none
 
      integer*4 unit, dummy, status, hdutype
      integer*4 vp(1000), nvp, nfc, nlc
      integer*4 felem, nelems
 
      real*8 nulld
      real*8  start(1000), stop(1000)
  
      character*(*) filnam
      character(130) filename
 
      logical anynull
 
C Find unused logical unit number.
      call ftgiou (unit, status)
 
C Open CGRO viewing period information FITS file.
      filename = filnam
      dummy = 0
      call ftopen (unit, filename, 0, dummy, status)
          
C Read total number of CGRO viewing periods, first CGRO observing cycle,
C and final CGRO observing cycle from FITS table header.
      call ftmrhd (unit,1,hdutype, status)
      write(*,*) 'type',hdutype
c      call ftgkyj (unit, 'NUMBVPS', nvp, comment, status)
c      call ftgkyj (unit, 'FIRSTCY', nfc, comment, status)
c      call ftgkyj (unit, 'FINALCY', nlc, comment, status)
 
C Read CGRO viewing period information from FITS table one row at a time.
      felem = 1
      nelems = 1
      nulld = 0.0d0
      nvp = 354
      call ftgcvj(unit,1,1,1,nvp,nulld,vp,anynull,status)
      call ftgcvd(unit,4,1,1,nvp,nulld,start,anynull,status)
      call ftgcvd(unit,7,1,1,nvp,nulld,stop,anynull,status)
     
C Close CGRO viewing period information FITS file, and free logical unit.
      call ftclos (unit, status)
      call ftfiou (unit, status)

      return
      end
