C*****************************************************************************
C SELECTOR TASK:
C      velabox
C
C FILE:
C      velabox.f
C
C DESCRIPTION:
C      Given a source location and a search radius, VELABOX will compute
C      which data files (box numbers) will be required to extract the 
C      source's light curve using the FVELALC FTOOL
C
C AUTHOR/DATE:
C    16 Dec 1994   Jesse Allen  Hughes STX, NASA/GSFC/HEASARC
C
C MODIFICATION HISTORY:
C    18 Nov 1994   Creation of the box calculation code for FVELALC FTOOL
C    13 Aug 1995   Corrected error in sorting routine
C
C NOTES:
C
C MAKE:
C      HOST: make velabox
C      IRAF:
C
C USAGE:
C      HOST: velabox
C      IRAF:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      numofboxes          The number of box files with relevant data
C      boxlist             A list of the relevant box numbers
C
C
C CALLED ROUTINES:
C      gboxpar
C      around   Part of the "finddata" routines from FVELALC
C      sortbox  Part of the "sort" routines from FVELALC
C
C*****************************************************************************

      subroutine velabx

      common /TASK/ taskname
      
      integer status, i, numofboxes, boxlist(144)

      real long_src, lat_src, searchrad

      character(40) taskname
      character(80) message


      taskname = 'VELABOX v1.0a'

      call ftcmsg

C Get parameters from the par file

       call gboxpar(long_src, lat_src, searchrad, status)
       if (status .ne. 0) goto 999

C Get all the boxes inside the search radius around the source
C and list them in the array boxlist.

      call finddata(long_src, lat_src, searchrad, numofboxes, boxlist)
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

       subroutine gboxpar(long_src, lat_src, searchrad, irafsts)

       integer irafsts

       real long_src, lat_src, searchrad

       character(80) message


C Initialize error checking flag

       irafsts = 0

C Get the galactic longitude and latitude of the source

       call uclgsr('long_src', long_src, irafsts)
       if (irafsts .ne. 0) then
          message = ' Unable to get LONG_SRC parameter'
          call fcerr(message)
          go to 999
       endif
       if ((long_src .gt. 360.0) .or. (long_src .lt. 0.0))
     +    long_src = MOD(long_src,360.0)

       call uclgsr('lat_src', lat_src, irafsts)
       if (irafsts .ne. 0) then
          message = ' Unable to get LAT_SRC parameter'
          call fcerr(message)
          go to 999
       endif
       if ((lat_src .lt. -90.0) .or. (lat_src .gt. 90.0)) then
          message = ' Illegal latitude value, -90 <= lat <= 90 '
          call fcerr(message)
          irafsts = -100
          go to 999
       endif

C Get the radius around the source from which to extract the data

       call uclgsr('searchrad', searchrad, irafsts)
       if (irafsts .ne. 0) then
          message = ' Unable to get SEARCHRAD parameter'
          call fcerr(message)
          go to 999
       endif

 999   continue

       return

       end




