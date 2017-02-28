      subroutine read_emap(emapnami,nout)
c--------------------------------------------------------------         
c     read the exposure map computed with stand-alone ftools packages. 
c     Created: Thu Oct 14 18:51:51 EDT 1999 (Ilana Harrus)
c     Modified: Tue Mar  7 16:53:47 EST 2000 (Ilana Harrus)
c             Change: Now go from x/y to ra/dec using FITSIO routines.
C    Rewritten: Thu Jun 23 11:08:01 EDT 2005 (Ilana Harrus) 
c             Complete rewrite of the subroutine -- we split the pixels 
c             and keep track of the number of times they're split. 
c             There is no randomization done for the exposure map -- 
c             Randomization is only done for events files 
c             (data and background)
c-------------------------------------------------------------- 
      implicit none

      include 'common_evts.inc'
      include 'common_img.inc'
      include 'common_final.inc'
      include 'common_par.inc'
      include 'common_defpixel.inc'
      include 'pi.inc'


      character*(*) emapnami
c---- emap events
      real emaptemp(nixm,niym), emaptemp2(nixm,niym)
      integer ncount(nixm,niym)
      integer iimg,jimg,i,j,k,nbev1,nbev2
      integer nout
       integer iimg1,iimg2,jimg1,jimg2

      real*8 bexp
      real*8 ximg1,yimg1,ra,dec,ximg,yimg,xrot

      real*8 ximgc1,ximgc2,yimgc1,yimgc2

      real*8 dx_exp,dy_exp,x0_exp,y0_exp,ra_exp,dec_exp
c---- fitsio
      integer unit,status,block
      character comment*80,errtext*30
      logical anyf
c---- functions
       integer len_trim
            
      nout=0
      status=0

c==== Initialize also the temp array. 
       do j=1,nixm
        do k=1,niym
         emaptemp(j,k)=0.
         emaptemp2(j,k)=0.
          ncount(j,k)=0
        enddo
       enddo
c== Start
        print*,'Read exposure file:',emapnami(:len_trim(emapnami))
        unit=10
        call ftopen(unit,emapnami,0,block,status)
        call ftgkyd(unit,'EXPOSURE',bexp,comment,status)
        print*,'    exposure:',bexp
        call ftgkyj(unit,'NAXIS1',nbev1,comment,status)
        call ftgkyj(unit,'NAXIS2',nbev2,comment,status)
        call ftgkyd(unit,'CDELT1',dx_exp,comment,status)
        call ftgkyd(unit,'CDELT2',dy_exp,comment,status)
        call ftgkyd(unit,'CRPIX1',x0_exp,comment,status)
        call ftgkyd(unit,'CRPIX2',y0_exp,comment,status)
        call ftgkyd(unit,'CRVAL1',ra_exp,comment,status)
        call ftgkyd(unit,'CRVAL2',dec_exp,comment,status)
        call ftgkyd(unit,'ROLLANG',xrot,comment, status)
        call ftg2de(unit,0,0,nixm,nbev1,nbev2,emaptemp,anyf,status)
c==== This map should have the same binning than the event file associated
       if(nbev1.ne.xbins) 
     >  print*,'Warning: Exposure map and event file do not have'// 
     > ' same image size (X)'
       if(nbev2.ne.ybins) 
     >  print*,'Warning: Exposure map and event file do not have'// 
     > ' same image size (X)'
        if(nbev1.eq.0) then
         print*,'Error: Exposure map NAXIS1 is zero'
        stop
        endif
        if(nbev2.eq.0) then
         print*,'Error: Exposure map NAXIS2 is zero'
        stop
        endif
c======= We only accept exposure map with pixels SMALLER than the ones used for
c=======   the mosaic map. 
       if(abs(dx_exp).gt.abs(img_dx)) then  
       print*,'Exposure map should have smaller pixel size'//
     > ' than mosaic images (X). Stop here'
        stop
       endif
c
       if(abs(dy_exp).gt.abs(img_dy)) then
       print*,'Exposure map should have smaller pixel size'//
     > ' than mosaic images (Y). Stop here'
        stop
       endif

        call ftclos(unit,status)
        call ftgerr(status,errtext)
        print*,'    END reading this file'
        print*,'    FITSIO context: ',errtext 
        if(status.ne.0) then
         print*,'>> ERROR: read_emap: error reading exposure file.'
         stop
        endif

c===== Fill the real array with the number read in emaptemp:
           do 110 i=1,nbev1
            do 120 j=1,nbev2
              ximg1=dble(i)
              yimg1=dble(j)

c  get the RA/dec from the image coordinate then put into new image.
c Almost identical as the case for the image -- except that for the 
c exposure map we can "split" a pixel according to where it comes on the 
c final map -- 
c
         xrot=0.  
         call ftwldp(ximg1,yimg1,ra_exp,dec_exp,x0_exp,y0_exp,
     &   dx_exp,dy_exp,xrot,'-TAN',ra,dec,status)


c   Now compute the associated ximg/yimg for the center and the corners of the 
c    pixel

C        Center of the pixel
         xrot=0.
         call ftxypx(ra,dec,img_ra0,img_dec0,img_x0,img_y0,
     &   img_dx,img_dy,xrot,'-TAN',ximg,yimg,status)
         
c         Four corners (this part does not exist for the image mosaic)
          ximgc1= ximg -0.5*abs(dx_exp/img_dx)
          ximgc2= ximg +0.5*abs(dx_exp/img_dx)
          yimgc1= yimg -0.5*abs(dy_exp/img_dy)
          yimgc2= yimg +0.5*abs(dy_exp/img_dy)

          iimg= nint(ximg)
          jimg= nint(yimg)
          iimg1=nint(ximgc1) 
          iimg2=nint(ximgc2)
          jimg1=nint(yimgc1)
          jimg2=nint(yimgc2)
c      This should not happen -- all should be within one pixel -- 
         if(abs(iimg1-iimg).ge.2.or.abs(iimg2-iimg).ge.2.
     & or.abs(jimg1-jimg).ge.2.or.abs(jimg2-jimg).ge.2.)  
     & print*,'Something is wrong in the pixel redistribution', 
     & iimg,iimg1,iimg2,jimg,jimg1,jimg2

C First check that the results are in the image: 
        if(iimg.lt.1.or.iimg.gt.nix.or.jimg.lt.1.or.jimg.gt.niy)
     >  then
           nout=nout+1
           goto 120
        endif
        if(iimg1.lt.1.or.iimg1.gt.nix.or.jimg1.lt.1.or.jimg1.gt.niy)
     >  then
           nout=nout+1
           goto 120
         endif
        if(iimg2.lt.1.or.iimg2.gt.nix.or.jimg2.lt.1.or.jimg2.gt.niy)
     >  then
          nout=nout+1
          goto 120
        endif

C Then spread the exposure map in proportion of where it is... 

c      Emap pixel right inside the image pixel -- no problem.  
        if(iimg1.eq.iimg.and.jimg1.eq.jimg
     & 	.and.iimg2.eq.iimg.and.jimg2.eq.jimg) then
          emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
          ncount(iimg,jimg)=ncount(iimg,jimg)+1
        endif
c        
	if(iimg1.lt.iimg.and.iimg2.eq.iimg) then 
         if(jimg1.lt.jimg) then
           emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
           ncount(iimg,jimg)=ncount(iimg,jimg)+1
           emaptemp2(iimg,jimg1)=emaptemp2(iimg,jimg1)+emaptemp(i,j)
           ncount(iimg,jimg1)=ncount(iimg,jimg1)+1
           emaptemp2(iimg1,jimg1)=emaptemp2(iimg1,jimg1)+emaptemp(i,j)
           ncount(iimg1,jimg1)=ncount(iimg1,jimg1)+1
           emaptemp2(iimg1,jimg)=emaptemp2(iimg1,jimg)+emaptemp(i,j)
           ncount(iimg1,jimg)=ncount(iimg1,jimg)+1
           if(jimg2.ne.jimg) print*,'impossible2',
     & 	  jimg1,jimg,jimg2,iimg1,iimg2,iimg
	 elseif(jimg1.eq.jimg) then           
           if(jimg2.gt.jimg) then
            emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
            ncount(iimg,jimg)=ncount(iimg,jimg)+1
            emaptemp2(iimg,jimg2)=emaptemp2(iimg,jimg2)+emaptemp(i,j)
            ncount(iimg,jimg2)=ncount(iimg,jimg2)+1
            emaptemp2(iimg1,jimg)=emaptemp2(iimg1,jimg)+emaptemp(i,j)
            ncount(iimg1,jimg)=ncount(iimg1,jimg)+1  
            emaptemp2(iimg1,jimg2)=emaptemp2(iimg1,jimg2)+emaptemp(i,j)
            ncount(iimg1,jimg2)=ncount(iimg1,jimg2)+1
           elseif(jimg2.eq.jimg) then
            emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
            ncount(iimg,jimg)=ncount(iimg,jimg)+1
            emaptemp2(iimg1,jimg)=emaptemp2(iimg1,jimg)+emaptemp(i,j)
            ncount(iimg1,jimg)=ncount(iimg1,jimg)+1
	   endif  
	 endif 
	endif  
c
	if(iimg1.eq.iimg.and.iimg2.eq.iimg) then 
         if(jimg1.lt.jimg) then
           emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
           ncount(iimg,jimg)=ncount(iimg,jimg)+1
           emaptemp2(iimg,jimg1)=emaptemp2(iimg,jimg1)+emaptemp(i,j)
          ncount(iimg,jimg1)=ncount(iimg,jimg1)+1
          if(jimg2.ne.jimg) print*,'impossible2a',
     & 	  jimg1,jimg,jimg2,iimg1,iimg2,iimg
	 elseif(jimg1.eq.jimg) then           
           if(jimg2.gt.jimg) then
             emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
              ncount(iimg,jimg)=ncount(iimg,jimg)+1
             emaptemp2(iimg,jimg2)=emaptemp2(iimg,jimg2)+emaptemp(i,j)
              ncount(iimg,jimg2)=ncount(iimg,jimg2)+1
	   endif  
	 endif 
	endif  
c
c        
	if(iimg1.eq.iimg.and.iimg2.gt.iimg) then 
         if(jimg1.lt.jimg) then
           emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
           ncount(iimg,jimg)=ncount(iimg,jimg)+1
           emaptemp2(iimg,jimg1)=emaptemp2(iimg,jimg1)+emaptemp(i,j)
           ncount(iimg,jimg1)=ncount(iimg,jimg1)+1
           emaptemp2(iimg2,jimg1)=emaptemp2(iimg2,jimg1)+emaptemp(i,j)
           ncount(iimg2,jimg1)=ncount(iimg2,jimg1)+1
           emaptemp2(iimg2,jimg)=emaptemp2(iimg2,jimg)+emaptemp(i,j)
          ncount(iimg2,jimg)=ncount(iimg2,jimg)+1
 	 elseif(jimg1.eq.jimg) then           
           if(jimg2.gt.jimg) then
             emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
             ncount(iimg,jimg)=ncount(iimg,jimg)+1
             emaptemp2(iimg,jimg2)=emaptemp2(iimg,jimg2)+emaptemp(i,j)
             ncount(iimg,jimg2)=ncount(iimg,jimg2)+1
             emaptemp2(iimg2,jimg)=emaptemp2(iimg2,jimg)+emaptemp(i,j)
             ncount(iimg2,jimg)=ncount(iimg2,jimg)+1  
            emaptemp2(iimg2,jimg2)=emaptemp2(iimg2,jimg2)+emaptemp(i,j)
             ncount(iimg2,jimg2)=ncount(iimg2,jimg2)+1
           elseif(jimg2.eq.jimg) then
             emaptemp2(iimg,jimg)=emaptemp2(iimg,jimg)+emaptemp(i,j)
          ncount(iimg,jimg)=ncount(iimg,jimg)+1
             emaptemp2(iimg2,jimg)=emaptemp2(iimg2,jimg)+emaptemp(i,j)
             ncount(iimg2,jimg)=ncount(iimg2,jimg)+1
	   endif  
	 endif 
	endif  
c
 120        continue
 110      continue 

        do 130 i=1,nixm
         do 140 j=1,niym
         if(ncount(i,j).ne.0) emaptemp2(i,j)=emaptemp2(i,j)/ncount(i,j)
         emap(i,j)=emap(i,j)+emaptemp2(i,j)
 140     continue
 130    continue

          print*,'Outside defined image       :',nout
      return
      end




