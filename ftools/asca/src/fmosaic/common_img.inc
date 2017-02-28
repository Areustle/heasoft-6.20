C
C       common_img.inc : common block for image.
C
C       Ilana Harrus 1999
C
      integer nigtim
      parameter(nigtim=1000)

      integer img_seed
      integer nix,niy      
      real*8 img_dx,img_dy
      real*8 img_ra0,img_dec0,img_x0,img_y0,img_rot
      real*8 igti_start,igti_stop,exp_igti

      common/image1/img_seed, nix,niy
      common/image/img_dx,img_dy,img_ra0
     & ,img_dec0,img_x0,img_y0,img_rot,igti_start(nigtim)
     & ,igti_stop(nigtim),exp_igti
