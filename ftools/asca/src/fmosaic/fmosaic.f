C*****************************************************************************
C TASK NAME: fmosaic
C
C FILE NAME: fmosaic.f
C
C DESCRIPTION: Make mosaic image of several observations. 
C              
C AUTHOR/DATE: Ilana Harrus (Sept 1999) 
c Modified:  Tue Feb  29 15:43:15 EST 2000 (Ilana Harrus) 
c            Used FITSIO to get conversion between X,Y and RA/Dec
c            June 23, 2005 (Ilana Harrus)
C            Complete rewrite of the read_emap.f routine (version 2.7)
c            September 9, 2005 (Ilana Harrus) 
c            Change Integer to Integer*4 for 
c            pha/pi/rawx/rawy/detx/dety/rti/rise_time/spread/grade/ccid (change required by Bryan)
c
C
C ROUTINES IMPLEMENTED IN THIS FILE:
C     subroutine  mosaic_read : read the parameter file.
C     subroutine  make_image  : add all the files together
C     subroutine  make_back   : create the background files. 
C     subroutine  read_emap   : mosaic all the exposure files. 
CC
C*****************************************************************************
       subroutine fmosaic

C     First include files.

       implicit none
c---- Events
       include 'common_evts.inc'
c---- Image
       include 'common_img.inc'
c---- Parameters from the parameter file
       include 'common_par.inc'
c----Final images
       include 'common_final.inc'

c.... input event files
      integer nevfm
      parameter(nevfm=50)
      integer nevf,nevts,nout,ln
      character(200) tmp_str
      character(200) evfname(nevfm,4)
      character(200) evfnami,rmfnami,mkfnami,bcknami,fn,fnbck
     &,fnemap,emapnami

c---- functions
      integer len_trim
c---- misc
      integer status,fiostat
      integer unit,i,kk, j, j1
 
      character(200) contxt 
      character(40) taskname
      common /task/taskname
      taskname = 'fmosaic v2.7.1'
      status = 0

      call fcecho('*******FMOSAIC vers 2.7.1  September, 2005.********')

c Get the parameters from "fmosaic.par" file using mosaic_read.

      call mosaic_read(status)
      
      contxt = 'Error in getting parameters'
      if (status.ne.0) goto 999

      unit =1
      rmfnami='dummy'
      bcknami='dummy'
      mkfnami='dummy'
      emapnami='dummy'
c      Configuration depending on what's asked.
      ln=1
      if(index(enrgcut,'n').ne.0) then 
       if(index(bckdone,'n').ne.0) then
        if(index(emapdone,'y').ne.0) ln=2
       else
        ln=2
        if(index(emapdone,'y').ne.0) ln=3          
       endif
      else
       ln=2       
       if(index(bckdone,'n').ne.0) then
        if(index(emapdone,'y').ne.0) ln=3
       else
        ln=3
        if(index(emapdone,'y').ne.0) ln=4          
       endif
      endif


      open(unit,file=evfile) 
      do i=1,nevfm
c         read(unit,*,end=100) (evfname(i,kk), kk=1,ln)
         read( unit, '(a)', end=100 ) tmp_str
         j = 1
         do while( j .le. 200 .and. tmp_str( j: j ) .le. ' ' )
           j = j + 1
         end do
         do kk = 1, ln
           if( j .gt. 200 ) then
             contxt = 'Error: Expected file names not found '
     &                          // '(input option/event list mismatch?)'
             goto 999
           end if
           j1 = j
           do while( j .le. 200 .and. tmp_str( j: j ) .gt. ' ' )
             j = j + 1
           end do
           evfname( i, kk ) = tmp_str( j1: j - 1 )
           do while( j .le. 200 .and. tmp_str( j: j ) .le. ' ' )
             j = j + 1
           end do
         end do
         if( j .ne. 201 ) then
           contxt = 'Error: List contains more names than expected '
     &                          // '(input option/event list mismatch?)'
           goto 999
         end if
         evfnami=evfname(i,1)
c
         print*,'Processing ',evfnami(:len_trim(evfnami))
      end do 
      contxt = 'Error:Exceeding limit for number of input files'
      goto 999
 100  continue 
      nevf=i-1
      print*,' total of:',nevf,' file(s)'

      unit=10
C====  Compute the total number of events to be processed 
      call def_nevents(unit,evfname,nevf,nevtott,status)
      print*,'Total number of events to be read',nevtott

      contxt = 'Error in getting number of events to be processed'
      if (status.ne.0) goto 999

c----- Define image using the input parameters.
       call def_image_wcs

c      We are looping over all the files to be read and filling the resulting 
C      image array. 
c                                          Ilana Harrus (September 1999)
      nevts=0
      nout=0
      do 998 i=1,nevf
         evfnami=evfname(i,1)
         if(ln.eq.2) then
          if(index(enrgcut,'n').ne.0) then 
           if(index(bckdone,'y').ne.0) then 
            if(index(bckmeth,'caldb').ne.0) mkfnami=evfname(i,2)
            if(index(bckmeth,'user').ne.0) bcknami=evfname(i,2)
           else
            emapnami=evfname(i,2)
           endif
          else
            rmfnami=evfname(i,2)
          endif
         elseif(ln.eq.3) then
          if(index(enrgcut,'n').ne.0) then 
            if(index(bckmeth,'caldb').ne.0) mkfnami=evfname(i,2)
            if(index(bckmeth,'user').ne.0) bcknami=evfname(i,2)
            emapnami=evfname(i,3)
          else              
            rmfnami=evfname(i,2)
           if(index(bckdone,'y').ne.0) then 
            if(index(bckmeth,'caldb').ne.0) mkfnami=evfname(i,3)
            if(index(bckmeth,'user').ne.0) bcknami=evfname(i,3)
           else
            emapnami=evfname(i,3)
           endif
          endif
         elseif(ln.eq.4) then
            rmfnami=evfname(i,2)
            if(index(bckmeth,'caldb').ne.0) mkfnami=evfname(i,3)
            if(index(bckmeth,'user').ne.0) bcknami=evfname(i,3)
            emapnami=evfname(i,4)           
         endif

         call ini_events(unit,evfnami,rmfnami,mkfnami,nout,status)
        
         contxt = 'Problem with the initialization program'
        if (status.ne.0) goto 998
c==== open image and do things accordingly
c       Just image
         if(index(enrgcut,'y').ne.0) then 
           call make_image(el,eu,nout)
           if(index(bckdone,'y').ne.0) call make_bkg(el,eu,bcknami,nout)
         else
c          Negative arguments for e1 and e2 means NO CUT in energy. 
           call make_image(-1.,-1.,nout)
           if(index(bckdone,'y').ne.0)
     &                               call make_bkg(-1.,-1.,bcknami,nout)
         endif

         if(index(emapdone,'y').ne.0) call read_emap(emapnami,nout)

 998  continue

cc==== write results to fits files

c  Write image FITS files

      if(index(enrgcut,'y').ne.0) then

       write(fn,123) keywd(:len_trim(keywd)),nint(el*10.),nint(eu*10.)
 123  format(a,'.',i3.3,'_',i3.3,'_image.fits')
 
       if(index(bckdone,'y').ne.0) write(fnbck,234) 
     %    keywd(:len_trim(keywd)),nint(el*10.),nint(eu*10.)
234   format(a,'.',i3.3,'_',i3.3,'_back.fits')

        if(index(emapdone,'y').ne.0) write(fnemap,345)
     %     keywd(:len_trim(keywd)),nint(el*10.),nint(eu*10.)
345   format(a,'.',i3.3,'_',i3.3,'_emap.fits')

       endif

      if(index(enrgcut,'y').eq.0) then

       write(fn,12) keywd(:len_trim(keywd))
 12   format(a,'_image.fits')
 
       if(index(bckdone,'y').ne.0) 
     & write(fnbck,23) keywd(:len_trim(keywd))
23    format(a,'_back.fits')

        if(index(emapdone,'y').ne.0)
     & write(fnemap,45) keywd(:len_trim(keywd))
45    format(a,'_emap.fits')

       endif

C----  Write images and background files.  
       fiostat = 0
       call image2fits(10,fn,fiostat)
       if(index(bckdone,'y').ne.0) call image2fits(10,fnbck,fiostat)
       if(index(emapdone,'y').ne.0) call image2fits(10,fnemap,fiostat)
       return

 999  continue
      call fcerr(contxt)
c      call fcerrm(contxt)
c      call fcerrm(status)
      return

      end

c ************************************************************************
