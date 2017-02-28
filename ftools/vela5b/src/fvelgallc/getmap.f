C This subroutine opens a map data file and reads it into memory, reporting
C the options used to create it to the screen.
C
C Author: Jesse Allen
C History:
C   Version 0.9   1 Mar 1995
C           1.0  15 Aug 1995  Passes on information to HISTORY records
C           1.1  12 Sep 1995  Allows up to 20 sources instead of 16
C           1.2  31 Oct 1995  Eliminated unnecessary address array
C           1.21 11 Dec 1995  Changed common to remove begin and end times
C           2.0  18 Dec 1995  umdget bug requires a minimum size for all
C                              dynamically allocated arrays...

      subroutine getmap(channel, status)

      implicit none

C Common block declarations

      common /SOURCE/ long_src, lat_src, longcntr, latcntr, longmin, 
     +  longmax, latmin, latmax, longavg, latavg, counts, backgnd, 
     +  frate, numpnts, nsrc, imax, jmax
      common /NAME/ sourcename

      integer nsrc, imax, jmax, numpnts(20,20)
      real long_src(20), lat_src(20)
      real longcntr(20,20), latcntr(20,20)
      real counts(20,20), backgnd(20,20)
      real longmin, longmax, latmin, latmax, longavg, latavg, frate

      character(16) sourcename(20)

C Declarations for the use of dynamic memory
C  the following MEM common block definition is in the system iraf77.inc file

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

C Local variables

      logical pointcheck, spincheck, weight

      integer channel, naxis, naxes(2), status
      integer mapunit, rwmode, blocksize, nhdu, hdutype, group
      integer numofrows, i, j, product, pttime, backopt, numelems
      integer ptast, ptaerrst, ptcsqst, ptndofst
      integer nbox, nerr

      real erate, minflux, maxflux, maxerr, stimbin
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2, lngbx1, latbx1

      character(16) telescop, creator
      character(20) mapfile
      character(70) comment
      character(80) message

C Open the map data file generated by FVELMAP

      mapunit = 1
      rwmode = 0
      group = 0
      write(mapfile,'(''mapdata_ch'', I1, ''.bin'')') channel
      call ftopen(mapunit, mapfile, rwmode, blocksize, status)
      if (status .ne. 0) then
         write(message, '('' Error opening '', A20)') mapfile
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message, '('' FITSIO status = '', i3)') status
         call ftclos(mapunit, status)
         call fcerr(message)
         go to 999
      endif

C Check that the data file is a Vela 5B binned map data file from
C Version 1.2 (gamma) of FVELMAP

      call ftgkys(mapunit, 'TELESCOP', telescop, comment, status)
      call ftgkys(mapunit, 'CREATOR', creator, comment, status)
      if (telescop .ne. 'Vela 5B ') then
         write(message,'('' Expected TELESCOP = Vela 5B, found '',
     +     a16)') telescop
         call fcecho(message)
         call ftclos(mapunit, status)
         if (status .eq. 0) status = -100
         go to 999
      else if (creator .ne. 'FVELMAP v1.3a') then
         write(message,'('' Expected CREATOR = FVELMAP v1.3a, '', 
     +     ''found '',a16)') creator
         call fcecho(message)
      endif

C Move to the binary table extension to get the data and its processing 
C characteristics

      nhdu = 2
      call ftmahd(mapunit, nhdu, hdutype, status)
      if (hdutype .ne. 2) then
         message = ' File does not have a binary table'
         call fcecho(message)
         call ftclos(mapunit, status)
         if (status .eq. 0) status = -100
         go to 999
      endif
      call ftgkyj(mapunit, 'NAXIS2', numofrows, comment, status)
      call ftgkye(mapunit, 'LNG_CNR1', long_cnr1, comment, status)
      call ftgkye(mapunit, 'LAT_CNR1', lat_cnr1, comment, status)
      call ftgkye(mapunit, 'LNG_CNR2', long_cnr2, comment, status)
      call ftgkye(mapunit, 'LAT_CNR2', lat_cnr2, comment, status)
      call ftgkye(mapunit, 'LNGBOX1', lngbx1, comment, status)
      call ftgkye(mapunit, 'LATBOX1', latbx1, comment, status)
      call ftgkyj(mapunit, 'NUM_LONG', imax, comment, status)
      call ftgkyj(mapunit, 'NUM_LAT', jmax, comment, status)
      call ftgkyj(mapunit, 'BACKOPT', backopt, comment, status)
      call ftgkye(mapunit, 'MINFLUX', minflux, comment, status)
      call ftgkye(mapunit, 'MAXFLUX', maxflux, comment, status)
      call ftgkye(mapunit, 'MAXVAR', maxerr, comment, status)
      call ftgkyl(mapunit, 'REJECTSP', spincheck, comment, status)
      call ftgkyl(mapunit, 'REJECTPT', pointcheck, comment, status)
      call ftgkyl(mapunit, 'WEIGHTED', weight, comment, status)
      call ftgkye(mapunit, 'BINSIZE', stimbin, comment, status)
      call ftgtdm(mapunit, 2, 400, naxis, naxes, status) 
      if (status .ne. 0) then
         message = ' Error retrieving parameters used in FVELMAP'
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message,'('' FITSIO status = '', I3)') status
         call ftclos(mapunit, status)
         call fcerr(message)
         go to 999
      endif
      if (naxis .ne. 2) then
         message = ' BINTABLE array has been damaged, exiting '
         call fcerr(message)
         go to 999
      endif
      if ((naxes(1) .ne. imax) .or. (naxes(2) .ne. jmax)) then
         message = ' TDIM dimensions do not match keywords, exiting '
         call fcerr(message)
         go to 999
      endif

      longmin = 1.0E10
      latmin = 1.0E10
      longmax = -1.0E10
      latmax = -1.0E10
      longavg = 0.0
      latavg = 0.0
      do 200 j = 1, jmax
         do 100 i = 1, imax
            longcntr(i,j) = lngbx1 - (2.0 * (i - 1))
            latcntr(i,j) =  latbx1 + (2.0 * (j - 1))
            if (longcntr(i,j) .lt. longmin) longmin = longcntr(i,j)
            if (longcntr(i,j) .gt. longmax) longmax = longcntr(i,j)
            if (latcntr(i,j) .lt. latmin) latmin = latcntr(i,j)
            if (latcntr(i,j) .gt. latmax) latmax = latcntr(i,j)
            longavg = longavg + longcntr(i,j)
            latavg = latavg + latcntr(i,j)
            if (longcntr(i,j) .lt. 0.0) 
     +         longcntr(i,j) = longcntr(i,j) + 360.0
 100     continue
 200  continue
      longavg = longavg / (imax * jmax)
      if (longavg .lt. 0.0) longavg = longavg + 360.0
      if (longavg .ge. 360.0) longavg = longavg - 360.0
      latavg = latavg / (imax * jmax)

      message = '----------------------------------------------------'
      call fcecho(message)
      write(message,'('' Settings used to create '', A20)') mapfile
      call fcecho(message)
      if (backopt .EQ. 0) then 
         message = ' Data has no background subtracted'
      else if (backopt .EQ. 1) then
         message = ' Data has a constant background subtracted'
      else if (backopt .EQ. 2) then
         message = 
     +     ' Data has the sinusoidial modeled background subtracted'
      else
         message = ' Unrecognized background option used '
      endif
      call fcecho(message)
      if (spincheck) then
         message = 
     +     ' Data excludes points flagged for unstable spin period'
      else
         message = 
     +     ' Data includes points flagged for unstable spin period'
      endif
      call fcecho(message)
      if (pointcheck) then
         message = 
     +     ' Data excludes points flagged for pointing errors'
      else
         message = 
     +     ' Data includes points flagged for pointing errors'
      endif
      call fcecho(message)

      write(message,'('' Maximum count: '', F8.2, 
     +     '' Maximum variance: '', F8.2)') maxflux, maxerr
      call fcecho(message)
      write(message,'('' Minimum count: '', F8.2)') minflux
      call fcecho(message)

      write(message,'('' Data summed into '',F9.2,'' second bins'')')
     +     stimbin
      call fcecho(message)
      if (weight) then
         message = ' Binned counting rate weighted by the background'
      else
         message = ' Straight binning used '
      endif
      call fcecho(message)
      message = '----------------------------------------------------'
      call fcecho(message)

C Assign dynamic memory for the data arrays, using 50 elements as a 
C minumum size to get around umdget limitations (Satan's number...)

      if (numofrows .lt. 50) then
         numelems = 50
      else
         numelems = numofrows
      endif

      pttime = 0
      ptast = 0
      ptaerrst = 0
      ptcsqst = 0
      ptndofst = 0

      call udmget(numelems, 7, pttime, status)
      if (status .ne. 0) then
         message = ' Error allocating dynamic memory for time array'
         call fcerr(message)
         go to 999
      endif

      product = numelems * (nsrc+3)
      call udmget(product, 6, ptast, status)
      if (status .ne. 0) then
         message = ' Error allocating dynamic memory for AST array'
         call fcerr(message)
         go to 999
      endif

      call udmget(product, 6, ptaerrst, status)
      if (status .ne. 0) then
         message = ' Error allocating dynamic memory for AERRST array'
         call fcerr(message)
         go to 999
      endif

      call udmget(numelems, 6, ptcsqst, status)
      if (status .ne. 0) then
         message = 
     +      ' Error allocating dynamic memory for chi squared array'
         call fcerr(message)
         go to 999
      endif

      call udmget(numelems, 4, ptndofst, status)
      if (status .ne. 0) then
         message = 
     + ' Error allocating dynamic memory for degrees of freedom array'
         call fcerr(message)
         go to 999
      endif

      do 300 i = 1, numofrows
         call rdmap(mapunit, i, MEMD(pttime), numofrows, status)
         call chkmap(erate, nbox, nerr)
         if (erate .le. frate) 
     +      call fitsrc(i, MEMD(pttime), MEMR(ptast), MEMR(ptaerrst), 
     +           MEMR(ptcsqst), MEMI(ptndofst), numofrows)
 300  continue

      call ftclos(mapunit, status)
      if (status .ne. 0) then
         message = ' Error closing the map data file '
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message,'('' FITSIO status = '', I3)') status
         call fcerr(message)
         go to 999
      endif

      call allout(channel, MEMD(pttime), MEMR(ptast), MEMR(ptaerrst), 
     +     MEMR(ptcsqst), MEMI(ptndofst), numofrows, minflux, maxflux,
     +     maxerr, spincheck, pointcheck, weight, backopt, stimbin,
     +     long_cnr1, lat_cnr1, long_cnr2, lat_cnr2)

      call udmfre(pttime, 7, status)
      call udmfre(ptast, 6, status)
      call udmfre(ptaerrst, 6, status)
      call udmfre(ptcsqst, 6, status)
      call udmfre(ptndofst, 4, status)

      if (status .ne. 0) then
         message = ' Error freeing dynamically allocated memory '
         call fcecho(message)
      endif

 999  return

      end

C----------------------------------------------------------------------------
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.9   1 Mar 1995
C          1.0  18 Dec 1995  Added common

      subroutine rdmap(mapunit, row, mjdtime, numofrows, status)

      implicit none

C Common block declarations

      common /SOURCE/ long_src, lat_src, longcntr, latcntr, longmin, 
     +  longmax, latmin, latmax, longavg, latavg, counts, backgnd, 
     +  frate, numpnts, nsrc, imax, jmax
      common /NAME/ sourcename

      integer nsrc, imax, jmax, numpnts(20,20)
      real long_src(20), lat_src(20)
      real longcntr(20,20), latcntr(20,20)
      real counts(20,20), backgnd(20,20)
      real longmin, longmax, latmin, latmax, longavg, latavg, frate

      character(16) sourcename(20)

C Local variables

      logical anyf

      integer mapunit, row, product, numofrows, status
      integer itemp(400), i, j, index

      real rtemp1(400), rtemp2(400)

      double precision mjdtime(numofrows), time


      product = imax * jmax
      call ftgcvd(mapunit, 1, row, 1, 1, 0.0D0, time, anyf, status)
      call ftgcve(mapunit, 2, row, 1, product, -100.0, rtemp1, anyf, 
     +     status)
      call ftgcve(mapunit, 3, row, 1, product, -100.0, rtemp2, anyf, 
     +     status)
      call ftgcvj(mapunit, 4, row, 1, product, 0, itemp, anyf, status)
      mjdtime(row) = time
      do 200 j = 1, jmax
         do 100 i = 1, imax
            index = i + ((j - 1) * imax)
            counts(i,j) = rtemp1(index)
            backgnd(i,j) = rtemp2(index)
            numpnts(i,j) = itemp(index)
 100     continue
 200  continue

      return

      end


C----------------------------------------------------------------------
C Checks the map data. 
C erate = the number of boxes with no data / total number of boxes
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0               Priedhorsky's version
C          0.9   3 Mar 1995  FVELGALLC FTOOL first draft version
C          1.1  12 Sep 1995  Allows 20 sources

      subroutine chkmap(erate, nbox, nerr)

      implicit none

C Common block declarations

      common /SOURCE/ long_src, lat_src, longcntr, latcntr, longmin, 
     +  longmax, latmin, latmax, longavg, latavg, counts, backgnd, 
     +  frate, numpnts, nsrc, imax, jmax
      common /NAME/ sourcename

      integer nsrc, imax, jmax, numpnts(20,20)
      real long_src(20), lat_src(20)
      real longcntr(20,20), latcntr(20,20)
      real counts(20,20), backgnd(20,20)
      real longmin, longmax, latmin, latmax, longavg, latavg, frate

      character(16) sourcename(20)

C Local variables

      integer i, j, nbox, nerr

      real erate

      character(80) message


      nbox = imax * jmax
      nerr = 0
      do 200 j = 1, jmax
         do 100 i = 1, imax
            if (counts(i,j) .eq. -100.0) nerr = nerr + 1
 100     continue
 200  continue
      if (nbox .le. 0) then
         message = ' No boxes selected, aborting'
         call fcerr(message)
         erate = 0.0
      else 
         erate = REAL(nerr) / REAL(nbox)
      endif

      return

      end
