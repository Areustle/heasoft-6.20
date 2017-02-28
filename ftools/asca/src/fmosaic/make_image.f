      subroutine make_image(e1,e2,ntot)
c--------------------------------------------------------------         
c     put photons from event list to image
c        - in the specified energy range
c        - using already filled wcs records for summed image
c
c     Created: Tue May 17 00:34:10 EDT 1994
c
c   July 1995 Change: call valid_det_pixel instead of good_det_pixel to
c    account on SIS CCD clock mode
c
c-------------------------------------------------------------- 
      implicit none
      include 'common_evts.inc'
      include 'common_img.inc'
      include 'common_final.inc'
      include 'pi.inc'

      real e1,e2

      integer i,ntot,status
      integer nin1,nout1,nout2,nout3,nout4,nout
      integer iimg,jimg,idet,jdet
      real*8 xsky,ysky,ximg,yimg,ra,dec,xrot
      integer*4 seed1      

      logical valid_det_pixel
c      real ran1
      real ft_ran2


      print 10
 10   format(/'open and fill image')

      status=0


c==== ini rand
      seed1=img_seed

c==== add
       nout=0
       nout1=0
       nout2=0
       nout3=0
       nout4=0
       nin1=0
         do 200 i=1,ntot
c........ check energy of the photon # i
c.......... e1<0 and e2<0 means that there are no cut on energy made. 
          if(e1.gt.0.and.e2.gt.0.) then
           if(ene1(i).lt.e1.or.ene2(i).gt.e2) then
             nout1=nout1+1
             goto 200
           endif
          endif
c....... good/bad pixel on detector
          idet=int(detx(i))
          jdet=int(dety(i))
          if(.not.valid_det_pixel(idet,jdet,instrume,ccdpow)) then
            nout2=nout2+1
            goto 200
          endif
c....... define photon position on the summed image
          if(img_seed.eq.0) then
            xsky=dble(skyx(i))
            ysky=dble(skyy(i))
          else
            xsky=dble(skyx(i))-0.5+ft_ran2(seed1)
            ysky=dble(skyy(i))-0.5+ft_ran2(seed1)
          endif
c Corner pixel is a problem with the random generation 
c  (taken out)   Ilana Harrus. 
          if(xsky.lt.2.or.ysky.lt.2) then
           nout3=nout3+1
           goto 200
          endif
         call ftwldp(xsky,ysky,sky_ra0,sky_dec0,sky_x0,sky_y0,
     &   sky_dx,sky_dy,0.0D00,'-TAN',ra,dec,status)
         xrot= img_rot
         call ftxypx(ra,dec,img_ra0,img_dec0,img_x0,img_y0,
     &   img_dx,img_dy,xrot,'-TAN',ximg,yimg,status)
          iimg=nint(ximg)
          jimg=nint(yimg)
c.......... check it is in the image
          if(iimg.lt.1.or.iimg.gt.nix.or.jimg.lt.1.or.jimg.gt.niy) 
     >     then
           nout4=nout4+1
           goto 200
          endif
c.......... Put in the image
           nin1=nin1+1
          image(iimg,jimg)=image(iimg,jimg)+1.
c.......... photon # i finished
 200     continue
         nout=nout1+nout2+nout3+nout4
         print*,'Total summed in the image      :',nin1
         print*,'Total rejected (detail below)  :',nout
         print*,' Cut in energy                 :',nout1
         print*,' Cut for calibration sources   :',nout2
         print*,' Geom cut (corner pixel)       :',nout3
         print*,' Cut (outside defined image)   :',nout4

c==== GTIs for the summed image - to be made later  <<<<<
c==== total exposure time of the image
       exp_igti=exp_igti+exp_gti

       print*,'    expos.:',exp_igti,' sec'
 
      return
      end


