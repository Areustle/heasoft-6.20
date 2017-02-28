      subroutine def_image_wcs
c--------------------------------------------------------------         
c     define wcs parameters of the summed image
c     defined in the job file
c
c     Created: Tue May 17 00:10:14 EDT 1994
c-------------------------------------------------------------- 
      implicit none
      include 'common_evts.inc'
      include 'common_img.inc'
      include 'common_par.inc'
      include 'common_final.inc'

      integer index,i,j

      print 10
 10   format(/'Define parameters of the summed image')


c---- cell size
         print*,' Cell size '
         img_dx=-cells/3600.
         img_dy= cells/3600.
      print*,'    deltx=',img_dx*3600.,' arcsec'
      print*,'    delty=',img_dy*3600.,' arcsec'

c---- image size
      print*,' Image size units: ',sizeunit
      nix=nixm
      niy=niym
      if(index(sizeunit,'degr').ne.0) then 
         nix=int(imgsizex*3600./cells)
         niy=int(imgsizey*3600./cells)
      endif
      if(index(sizeunit,'arcmin').ne.0) then 
         nix=int(imgsizex*60./cells)
         niy=int(imgsizey*60./cells)
      endif
      if(index(sizeunit,'pix').ne.0) then 
         nix=int(imgsizex)
         niy=int(imgsizey)
      endif
      print*,' Image size:',imgsizex,' by',
     >    imgsizex,sizeunit
      print*,'           ',nix,' by',niy,' pixels'
      print*,' Max.image size: ',nixm,' by',niym,' pixels'

      if(nix.gt.nixm.or.niy.gt.niym) then
       print*,' ERROR: def_image_wcs: requested image size exceeds ',
     >       'maximum (',nixm,niym,')'
         stop
      endif
c  initialize the image,background and exposure map files: 
        do i=1,nix
         do j=1,niy
          image(i,j)=0.
          bkg(i,j)=0.
          emap(i,j)=0.
         enddo
        enddo

c---- ra&dec of the image center
         print*,' RA & Dec of the image center '
         img_ra0=ra0
         img_dec0=dec0

      print*,'    R.A.=',img_ra0
      print*,'    DEC.=',img_dec0

c---- roll_angle
      img_rot=0.0D00
c      print*,' Roll angle:', img_rot,' degr.'

c---- center of the image
c     the center of the image is position, corresponding to reference ra,dec
c     it is choosen so, that i-th pixel runs from i-0.5 thru i+0.5
c     therefore pixel position is i=nint(x), where x is continuous coord.value
c     It's the same style as for ASCA pixels
       img_x0= float(nix)/2.+0.5
       img_y0= float(niy)/2.+0.5
      print*,' Image center:', img_x0, img_y0,' pix.'

c---- seed
       img_seed=-iabs(seed)
      if( img_seed.eq.0) then
        print*,' Random seed =', img_seed
        print*,' ==> Do not randomize.'
      else
        print*,' Random seed =', img_seed
      end if

      return
      end




