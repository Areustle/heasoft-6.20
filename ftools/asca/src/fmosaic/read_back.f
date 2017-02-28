      subroutine read_back(bcknami,nout)
c--------------------------------------------------------------         
c     read the user given background maps and add them up. 
c     Created: Tue Mar 14 15:01:48 EST 2000  (Ilana Harrus)
c     The background maps are supposed to have NO pointings informations
c      in their headers (like a blank sky map) so the pixels are treated  
c     as "detectors" coordinates.
c-------------------------------------------------------------- 
      implicit none

      include 'common_evts.inc'
      include 'common_img.inc'
      include 'common_final.inc'
      include 'common_par.inc'
      include 'common_defpixel.inc'
      include 'pi.inc'


      character*(*) bcknami
c---- emap events
      real bcktemp(nixm,niym)
      integer iimg,jimg,i,j,k,nbev1,nbev2,naxis
      integer naxes(3)
      integer maxdim,bitpix
      integer nout,nout1,nout2,nin
      real*8 bexp
      real*8 ximg1,yimg1,ra,dec,ximg,yimg,xrot,xx0,xy0
c---- misc 
      integer scalex,scaley
c---- fitsio
      integer unit,status,block,hdutype
      character comment*80,errtext*30
      logical anyf
c---- functions
      logical valid_det_pixel
      integer len_trim
      integer*4 seed1      

c      real ran1
      real ft_ran2
            
      nout1=0
      nout2=0
      nin=0
      nout=0
      status = 0

c==== ini rand
      seed1=img_seed

c==== Initialize also the temp array. 
       do j=1,nixm
        do k=1,niym
         bcktemp(j,k)=0.
        enddo
       enddo
c== Start
        print*,'Read background map:',bcknami(:len_trim(bcknami))
        unit=10
        call ftopen(unit,bcknami,0,block,status)
        call ftgkyj(unit,'NAXIS1',nbev1,comment,status)
        call ftgkyj(unit,'NAXIS2',nbev2,comment,status)
        call ftg2de(unit,0,0,nixm,nbev1,nbev2,bcktemp,anyf,status)
c==== This map should have the same binning than the event file associated
c==== We stop if it's not the case.
        scalex=int(nbev1/xbins)
        scaley=int(nbev2/ybins)
        if(scalex.ne.1) then 
         print*,'Error: Factor', scalex,' between background map'//
     > ' and evt file image size (X) '
         stop
        endif
        if(scaley.ne.1) then
         print*,'Error: Factor', scaley,' between background map'//
     > ' and evt file image size (Y)'
         stop
        endif
        call ftclos(unit,status)
        call ftgerr(status,errtext)
        print*,'    END reading this file'
        print*,'    FITSIO context: ',errtext 
        if(status.ne.0) then
         print*,'>> ERROR: read_bck: error reading background file.'
         stop
        endif


c===== Fill the real array with the number read in bcktemp:
c------ look if this is a valid pixel. 
           do 110 i=1,nbev1
            do 120 j=1,nbev2
            if(.not.valid_det_pixel(i,j,instrume,ccdpow)) then
             nout1=nout1+1
             goto 120
            endif
c............. define photon position on the summed image
            if(img_seed.eq.0) then
              ximg1=dble(i)
              yimg1=dble(j)
            else
              ximg1=dble(i)-0.5+ft_ran2(seed1)
              yimg1=dble(j)-0.5+ft_ran2(seed1)
            endif
c  This will treat the (x,y) pixel as image coordinate. 
c   Because there is no header to transform this, we treat the pixels 
c    as detectors coordinates... 
c         call ftwldp(ximg1,yimg1,sky_ra0,sky_dec0,sky_x0,sky_y0,
c     &   sky_dx,sky_dy,0.,'-TAN',ra,dec,status)
c
            xx0=det_x0+detoff_x
            xy0=det_y0+detoff_y
            xrot=pa_pnt
         call ftwldp(ximg1,yimg1,sky_ra0,sky_dec0,xx0,xy0,
     &     sky_dx,sky_dy,xrot,'-TAN',ra,dec,status)
            xrot= img_rot
         call ftxypx(ra,dec,img_ra0,img_dec0,img_x0,img_y0,
     &   img_dx,img_dy,xrot,'-TAN',ximg,yimg,status)
            iimg=nint(ximg)
            jimg=nint(yimg)
c............. put into image
           if(iimg.lt.1.or.iimg.gt.nix.or.jimg.lt.1.or.jimg.gt.niy)
     >  then
              nout2=nout2+1
              goto 120
            endif
             bkg(iimg,jimg)=bkg(iimg,jimg)+bcktemp(i,j)*exp_gti
            nin=nin+1
 120        continue
 110      continue 
         print*,' Test on the exp_gti',exp_gti  
         print*,' Test on the exp_igti',exp_igti  
         print*,'Total entries in the image  :',nin
         nout=nout1+nout2  
         print*,'Total out                   :',nout
         print*,'This includes: ',nout1, ' cut for calibration sources'
         print*,'and ', nout2, ' outside the defined image.'
      return
      end




