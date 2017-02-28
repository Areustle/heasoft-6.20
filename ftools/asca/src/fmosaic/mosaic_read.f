      subroutine mosaic_read(status)

      implicit none     
      include 'common_par.inc'

      character(200) context
      integer errstat,status
*------------------------------------------------------------------------------
* Description: Gets the parameters from the fmosaic.par file.
*
* Arguments:
*              keywd (c) : keyword for output file names
*               seed (i) : random seed for image generation 
*           sizeunit (c) : img size units (degr|arcmin|pix|max)
* imgsizex, imgsizey (r) : img size x & y
*              cells (r) : img.cell size (arcsec) 
*           ra0,dec0 (r) : img. center RA,DEC (if method=manual)
*            enrgcut (c) : Do an energy cut (y|n)?
*              el eu (r) : lower and upper bound in energy [keV]
*    bckdone,bckmeth (c) : bckgrd subtraction done or not/ user-caldb files.
*           emapdone (c) : used generated exposure maps? 
*             evfile (c) : events file (with list of evtfiles to be processed).
*            status  (r) : The success status for this routine
*                            0 = OK
*                            else = NOT OK
*
* Authors/Modification History:
*              Ilana Harrus  Sep 15, 1999 - Based on Churazov & Gilfanov 
*                                           routine and on example from kaa's
*                                           program.
*
*  Set status = OK

      errstat = 0
      status = 0

*    Get parameters from the fmosaic.par file. 
* 

      call uclgst('keywd', keywd, errstat)
      context = 'Problem with key word for output files'
      if (errstat.ne.0) goto 999
c      print*,'keyword for output files:', keywd

c---- random seed
      call uclgsi('seed',seed, errstat)
      context = 'Problem with random seed'
      if (errstat.ne.0) goto 999

c---- wcs parameters of the summed image
c.... image size
      call uclgst('sizeunit',sizeunit, errstat)
      context = 'Problem with unit in size of the image'
      if (errstat.ne.0) goto 999

      call uclgsr('imgsizex',imgsizex, errstat)
      context = 'Problem with size of the image-X'
      if (errstat.ne.0) goto 999

      call uclgsr('imgsizey',imgsizey, errstat)
      context = 'Problem with size of the image-Y'
      if (errstat.ne.0) goto 999
c      print*,' Image of', imgsizex,'by', imgsizey,sizeunit 

c.... cell size
      call uclgsr('cells',cells, errstat)
      context = 'Problem with size of the cell'
      if (errstat.ne.0) goto 999

c....image definition
      call uclgsr('ra0',ra0, errstat)
      context = 'Problem with RA of the center of image'
      if (errstat.ne.0) goto 999

      call uclgsr('dec0',dec0, errstat)
      context = 'Problem with Dec of the center of image'
      if (errstat.ne.0) goto 999
c      print*,' Image centered on: ',ra0,dec0

      call uclgst('enrgcut', enrgcut, errstat)
      context = 'Problem with key word for energy cut'
      if (errstat.ne.0) goto 999
      call LOCASE( enrgcut )

      if(index(enrgcut,'y').ne.0) then
c----  energy range for image
       call uclgsr('el',el, errstat)
       context = 'Problem with lower energy bound'
       if (errstat.ne.0) goto 999
      
       call uclgsr('eu',eu, errstat)
       context = 'Problem with higher energy bound'
       if (errstat.ne.0) goto 999
      endif
 
c---- Background subtraction
      call uclgst('bckdone',bckdone, errstat)
      context = 'Problem with keyword about bckground substraction'
      if (errstat.ne.0) goto 999
       call LOCASE( bckdone )
       if(index(bckdone,'y').ne.0) then
        call uclgst('bckmeth',bckmeth, errstat)
        context = 
     >  'Problem with keyword about bckground substraction method'
        if (errstat.ne.0) goto 999
        call LOCASE( bckmeth )
        if( index( bckmeth, 'auto' ) .ne. 0 ) then
          bckmeth = 'caldb'
        end if
       endif 

c----Exposure maps to be included?
c                  (Exp maps are computed in stand-alone ftools)
      call uclgst('emapdone',emapdone, errstat)
      context = 'Problem with keyword about exposure maps'
      if (errstat.ne.0) goto 999
      call LOCASE( emapdone )
      
c---- event files directory
      call uclgst('evfile',evfile, errstat)
      context = 'Problem with file containing list of *.evt'
      if (errstat.ne.0) goto 999
c      print*,' ',evfile,'contains all the events files used for the mosaic' 

      RETURN

999   CONTINUE
      CALL fcerr(context)
      status = errstat
      RETURN

      END










