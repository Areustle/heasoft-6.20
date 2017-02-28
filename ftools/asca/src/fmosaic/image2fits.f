      subroutine image2fits(unit,filename,status)
c--------------------------------------------------------------         
c     Write to FITS file:
c        - image array [counts]
c        - WCS information
c
c     Created: Tue May 17 15:48:24 EDT 1994
c     Modified: Mon Sep 27 10:18:43 EDT 1999  (Ilana Harrus)
c-------------------------------------------------------------- 
      implicit none
      include 'common_par.inc'
      include 'common_img.inc'
      include 'common_final.inc'

      integer unit
      character*(*) filename

      integer len_trim
      integer status,bitpix,naxis,pcount,gcount
      integer naxes(2)
      character(30) errtext

      if(index(filename,'image').ne.0) then
      print 10,filename(:len_trim(filename))
 10   format(/'Write image to FITS file ',a)
      elseif(index(filename,'back').ne.0) then
       print 11,filename(:len_trim(filename))
 11   format(/'Write background to FITS file ',a)
      elseif(index(filename,'emap').ne.0) then
       print 12,filename(:len_trim(filename))
 12    format(/'Write exposure map to FITS file ',a)
       endif
c==== open FITS file
      call ftinit(unit,filename,0,status)
      
      naxis=2
      pcount=0
      gcount=1
      naxes(1)=nix
      naxes(2)=niy

c==== primary image
      bitpix=-32
      call ftphpr(unit,.true.,bitpix,naxis,naxes,pcount,gcount,.true.,
     > status)
      call imgwcs2fits(unit,filename,status)

      call ftpdef(unit,bitpix,naxis,naxes,pcount,gcount,status)
       if(index(filename,'image').ne.0) then
         call ftp2de(unit,0,nixm,naxes(1),naxes(2),image,status)
       elseif(index(filename,'back').ne.0) then
         call ftp2de(unit,0,nixm,naxes(1),naxes(2),bkg,status)
       elseif(index(filename,'emap').ne.0) then
         call ftp2de(unit,0,nixm,naxes(1),naxes(2),emap,status)
       endif

c==== close FITS file
      call ftclos(unit,status)

c==== final status
      call ftgerr(status,errtext)

      if(index(filename,'image').ne.0) then
       print*,' END writing image file'
      elseif(index(filename,'back').ne.0) then
       print*,' END writing background file'
      elseif(index(filename,'emap').ne.0) then
       print*,' END writing exposure file'
      endif
      print*,' FITSIO context: ',errtext 

      return
      end 

