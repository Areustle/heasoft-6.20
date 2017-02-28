C*****************************************************************************
C SELECTOR TASK:
C      velamap
C
C FILE:
C      velamap.f
C
C DESCRIPTION:
C      Given two opposite corners of a box on the galactic plane, VELAMAP 
C      will compute which data files (box numbers) will be required to 
C      extract the source's light curve using the FVELMAP FTOOL
C
C AUTHOR/DATE:
C    27 Jan 1995   Jesse Allen  Hughes STX, NASA/GSFC/HEASARC
C
C MODIFICATION HISTORY:
C    Version 0.9  27 Jan 1995  Beta test version
C            1.0  15 Aug 1995  First release version.  Sorting error corrected 
C            1.1   6 Sep 1995  Changed longitude to Vela logic (> 360 okay)
C            1.2  15 Mar 1996  Sets map limits at +/- 48 galactic latitude
C
C NOTES:
C
C MAKE:
C      HOST: make velamap
C      IRAF:
C
C USAGE:
C      HOST: velamap
C      IRAF:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED ROUTINES:
C      gboxmap
C      findmaps
C
C*****************************************************************************

      subroutine velamp

      common /TASK/ taskname
      
      integer status, i, numofboxes, boxlist(400)
      integer iwp, jwp

      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real long_cntr(400), lat_cntr(400)
      real address(20,20,3)

      character(40) taskname
      character(80) message


      taskname = 'VELAMAP v1.2a'

      call ftcmsg

C Get parameters from the par file

       call gmappar(long_cnr1, lat_cnr1, long_cnr2, lat_cnr2, status)
       if (status .ne. 0) goto 999

C Get the box numbers

      call findmaps(long_cnr1, lat_cnr1, long_cnr2, lat_cnr2, 
     +     numofboxes, boxlist, address, long_cntr, lat_cntr, 
     +     iwp, jwp)
  
      write(message,'('' Number of data files required: '',i6)') 
     +     numofboxes
      call fcecho(message)
      do 100 i = 1, numofboxes
         write(message,'('' File '', i3, '' is named b'', i5.5,
     +         ''.raw'')') i, boxlist(i)
         call fcecho(message)
 100  continue
     
 999  return

      end


C------------------------------------------------------------------------
C Calls parameters for VELABOX task from the parameter file

      subroutine gmappar(long_cnr1, lat_cnr1, long_cnr2, lat_cnr2,
     +           irafsts)

       integer irafsts

       real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2

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

 999  return

      end




