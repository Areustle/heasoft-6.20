      subroutine imgwcs2fits(unit,fn,status)
c--------------------------------------------------------------         
c     Write WCS information (stored in common_img.inc) to fits file
c     should be called after ftphpr (see image2fits for example)
c
c     Created: Tue May 24 14:01:53 EDT 1994
c     Modified: Mon Sep 27 15:29:21 EDT 1999 (Ilana Harrus)
c-------------------------------------------------------------- 
      implicit none
      include 'common_img.inc'
      include 'common_par.inc'

      integer unit,status
      character(30) comment
      character*(*) fn



      comment='Value of X center pixel'
      call ftpkyd(unit,'CRPIX1  ',dble(img_x0),10,comment,status)
      comment='Value of Y center pixel'
      call ftpkyd(unit,'CRPIX2  ',dble(img_y0),10,comment,status)
      comment='RA of image center'
      call ftpkyd(unit,'CRVAL1  ',dble(img_ra0),10,comment,status)
      comment='Dec of image center'
      call ftpkyd(unit,'CRVAL2  ',dble(img_dec0),10,comment,status)
      comment='X pixel size in arcmin'
      call ftpkyd(unit,'CDELT1  ',dble(img_dx),10,comment,status)
      comment='Y pixel size in arcmin'
      call ftpkyd(unit,'CDELT2  ',dble(img_dy),10,comment,status)
      comment = ' '
      call ftpkys(unit,'CTYPE1  ','RA---TAN',comment,status)
      call ftpkys(unit,'CTYPE2  ','DEC--TAN',comment,status)
      call ftpkyd(unit,'CROTA2  ',dble(img_rot),10,comment,status)
      call ftpkys(unit,'RADECSYS','FK5',comment,status)
      call ftpkyd(unit,'EQUINOX ',2000.d0,10,comment,status)
      call ftpkyd(unit,'EXPOSURE ',dble(exp_igti),10,comment,status)

      if(index(fn,'image').ne.0) then
      comment='Total number of events'
      call ftpkyj(unit,'NEVTS',nevtott,10,comment,status)
      endif        

      return
      end
