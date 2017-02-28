c==============================================================         
c     Library of subroutines for exposure map calculation
c
c     Created: Mon May 16 17:04:47 EDT 1994
c==============================================================         
      logical function valid_det_pixel(i,j,instrument,ccdpow)
c--------------------------------------------------------------         
c     checks if pixel (i,j) should be used for analysis
c
c     instrument - as read from FITS header and stored 
c                  in zkeywds common block 
c                  (allowed values are 'SIS0', 'SIS1', 'GIS2', 'GIS3')
c     i,j - linearized position on detector (DETX, DETY)
c           for GIS: 1-256
c           for SIS: 1-1280
c
c     Created: Mon May 16 13:58:21 EDT 1994
c     Modifed: Thu Sep 23 18:35:08 EDT 1999
c       to allow for different pixel sizes -  (Ilana Harrus)
c        and for REV2 criteria of selection
c
c-------------------------------------------------------------- 
      implicit none

      include 'bad_places.inc'
c   in bad_places.inc the bad pixels are given in the default unit.
c       coded on 1280 for SIS and 256 for GIS 
      include 'common_defpixel.inc'

      integer i,j,ok_x,ok_y,ok_xy
      real facx,facy
      character*(*) instrument
      logical ccdpow(4)
      real sis0_bord,sis1_bord,gis2_bord,gis3_bord
      real x,y,d2
      real r1,r2

C Instead of tranforming the reference pixels to the one to check, 
C   we do the inverse. (just one tranformation)... 
C   The factor depends on the instrument. 
C        (set to 0.0 initially to remove compiler warnings - KM, 2001 December)
       facx=0.0
       facy=0.0
       if(index(instrument,'SIS').ne.0) then 
            facx=float(1280/xbins)
            facy=float(1280/ybins)
       elseif(index(instrument,'GIS').ne.0) then 
            facx=float(256/xbins)
            facy=float(256/ybins)
       endif
       valid_det_pixel=.false.
       x=float(i)*facx
       y=float(j)*facy

c      print*,' i,j,x,y:',i,j,x,y
c==== SIS0
      sis0_bord=0.
      if(index(instrument,'SIS0').ne.0) then
c... Check boundaries
         ok_x=1
         ok_y=1
         if(abs(x-sis0_centr_x).lt.(sis0_gap_x/2+sis0_bord))
     >       ok_x=0
         if(abs(x-sis0_centr_x).gt.(sis0_gap_x/2+sis0_chip_x-sis0_bord))
     >       ok_x=0
         if(abs(y-sis0_centr_y).lt.(sis0_gap_y/2+sis0_bord))
     >       ok_y=0
         if(abs(y-sis0_centr_y).gt.(sis0_gap_y/2+sis0_chip_y-sis0_bord))
     >       ok_y=0
         ok_xy=ok_x*ok_y
         valid_det_pixel=.false.
         if(ok_xy.eq.1) valid_det_pixel=.true.
c... Check if this chip is in use
         if(valid_det_pixel) then
           if(x.gt.sis0_centr_x.and.y.gt.sis0_centr_y
     >  .and. .not.ccdpow(4)) valid_det_pixel=.false. 
           if(x.gt.sis0_centr_x .and. y.lt.sis0_centr_y
     >  .and. .not.ccdpow(3)) valid_det_pixel=.false. 
           if(x.lt.sis0_centr_x .and. y.gt.sis0_centr_y 
     >  .and. .not.ccdpow(1)) valid_det_pixel=.false. 
           if(x.lt.sis0_centr_x .and. y.lt.sis0_centr_y 
     >  .and. .not.ccdpow(2)) valid_det_pixel=.false. 
         end if
      endif
c==== SIS1
      sis1_bord=0
      if(index(instrument,'SIS1').ne.0) then
c... Check boundaries
         ok_x=1
         ok_y=1
         if(abs(x-sis1_centr_x).lt.(sis1_gap_x/2+sis1_bord))
     >       ok_x=0
         if(abs(x-sis1_centr_x).gt.
     > (sis1_gap_x/2+sis1_chip_x-sis1_bord))  ok_x=0
         if(abs(y-sis1_centr_y).lt.(sis1_gap_y/2+sis1_bord))
     >       ok_y=0
         if(abs(y-sis1_centr_y).gt.
     > (sis1_gap_y/2+sis1_chip_y-sis1_bord))  ok_y=0
         ok_xy=ok_x*ok_y
         valid_det_pixel=.false.
         if(ok_xy.eq.1) valid_det_pixel=.true.
c... Check if this chip is in use
         if(valid_det_pixel) then
           if(x.gt.sis1_centr_x. and. y.gt.sis1_centr_y. 
     > and. .not.ccdpow(2))  valid_det_pixel=.false. 
           if(x.gt.sis1_centr_x. and. y.lt.sis1_centr_y. 
     > and. .not.ccdpow(1))  valid_det_pixel=.false. 
           if(x.lt.sis1_centr_x. and. y.gt.sis1_centr_y. 
     > and. .not.ccdpow(3))  valid_det_pixel=.false. 
           if(x.lt.sis1_centr_x. and. y.lt.sis1_centr_y. 
     > and. .not.ccdpow(4))  valid_det_pixel=.false. 
         end if
      endif
c==== GIS2
      gis2_bord=0
      if(index(instrument,'GIS2').ne.0) then
         valid_det_pixel=.true.
          r1=((x-gis2_cal_x)*cos(angle_g2)+(y-gis2_cal_y)*sin(angle_g2))
          r2=((y-gis2_cal_y)*cos(angle_g2)-(x-gis2_cal_x)*sin(angle_g2))
          d2=(r1/gis2_cal_r1)**2+(r2/gis2_cal_r2)**2
          if(d2.lt.1.) valid_det_pixel=.false.
          d2=(x-gis2_br_x)**2+(y-gis2_br_y)**2
          if(d2.gt.(gis2_br_r-gis2_bord)**2) valid_det_pixel=.false.
       endif
c==== GIS3
      gis3_bord=0
      if(index(instrument,'GIS3').ne.0) then
         valid_det_pixel=.true.
          r1=((x-gis3_cal_x)*cos(angle_g3)+(y-gis3_cal_y)*sin(angle_g3))
          r2=((y-gis3_cal_y)*cos(angle_g3)-(x-gis3_cal_x)*sin(angle_g3))
          d2=(r1/gis3_cal_r1)**2+(r2/gis3_cal_r2)**2
          if(d2.lt.1.) valid_det_pixel=.false.
          d2=(x-gis3_br_x)**2+(y-gis3_br_y)**2
          if(d2.gt.(gis3_br_r-gis3_bord)**2) valid_det_pixel=.false.
      endif

      return
      end























