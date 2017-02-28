      subroutine make_bkg(e1,e2,bcknami,nout)
c--------------------------------------------------------------         
c     prepare backround map
c        - in the specified energy range
c          If the argument e1,e2 are negative-- NO cut is made in energy
c        - using rigidity distribution for each event list
c
c     Created: Wed May 18 13:40:48 EDT 1994
c     Last modified: Tue May 22 15:53:02 EDT 2001: add case for e1/e2<0 
c                     NO cut in energy made-- (Ilana Harrus)
c
c   July 1995 Change: call valid_det_pixel instead of good_det_pixel to
c    account on SIS CCD clock mode
c   Modified: Wed Sep 29 15:14:53 EDT 1999   (Ilana Harrus)
c    (create the background in a parallele procedure than the one used 
c       the for image)
c
c
c-------------------------------------------------------------- 
      implicit none

      include 'common_evts.inc'
      include 'common_img.inc'
      include 'common_final.inc'
      include 'common_channels.inc'
      include 'common_par.inc'
      include 'common_defpixel.inc'
      include 'common_keywords.inc'

      include 'pi.inc'


c---- input/output params
      real e1,e2
      character*(*) bcknami
c---- bkg events
      character bkgname*200
      integer nbevm
      parameter(nbevm=200000)
      integer*2 bdetx(nbevm),bdety(nbevm),bpi(nbevm),bccdid(nbevm)
      integer nbev
      real*8 bexp
c---- misc
      integer iimg,jimg,idet,jdet
      integer lloop,linit
      integer ih,i,nout,ch,nout1,nout2,nout3,nin1
      real rig,weight,be1,be2
      real*8 xdet,ydet,ra,dec,ximg,yimg,xrot,xx0,xy0
c---- misc 
      integer pidetbck1,pidetbck2,pidetbck,xdetbck,ydetbck
      integer scalep,scalex,scaley
c---- fitsio
      integer unit,status,block,hdutype,ic
      character comment*80,errtext*30
      logical anyf
c---- functions
      logical valid_det_pixel,inite,initx,inity
      integer len_trim
      integer*4 seed1
c      real ran1
      real ft_ran2
      
     
      print 10
 10   format(/'Calculate background map')
      
      nout1=0
      nout2=0
      nout3=0
      nin1=0
      nout=0
      inite=.true.
      initx=.true.
      inity=.true.

c==== ini rand
      seed1=img_seed

c
      lloop=1
      linit=1
      if(index(bckmeth,'caldb').ne.0) lloop=nhrig
       do 110 ih=1,lloop
        if(index(bckmeth,'caldb').ne.0) then 
          if(hrig(ih).eq.0) goto 110
c---------- define name of the corresponding bkg file
          rig=(ih-0.5)*hrigstp
          call get_bcg_fn(instr_id,rig,bkgname)
          print*,' rig.bin #',ih,' <rig>=',rig,' weight=',
     >        hrig(ih)
      elseif(index(bckmeth,'user').ne.0) then
c       read the maps and exit (we fill the final image in read_back.f). 
           bkgname=bcknami
           call read_back(bcknami,nout)
           goto 300
      endif
        print*,'    bkg fn:',bkgname(:len_trim(bkgname))
c---------- read from bgk file
        unit=10
        status = 0
c------- Initialize all the variables to be read: 
          bexp=0.
          nbev=0
          do linit=1,nbevm
            bdetx(linit)=0
            bdety(linit)=0
            bpi(linit)=0
            bccdid(linit)=0
          enddo
          pidetbck1=0
          pidetbck2=0
          pidetbck=1
          xdetbck=1
          ydetbck=1

c--- Start reading
          call ftopen(unit,bkgname,0,block,status)
        call ftgkyd(unit,'EXPOSURE',bexp,comment,status)
        print*,'    exposure:',bexp
c---- Move to second header
        call ftmahd(unit,2,hdutype,status)
        call ftgkyj(unit,'NAXIS2',nbev,comment,status)
        print*,'    Total number of events in this bkg list:',nbev
        if(nbev.gt.nbevm) then
          print*,' >>> ERROR: make_bkg: bkg event list is too large.'
          stop
        endif
        call ftgcno(unit,.false.,'DETX',ic,status)
        call ftgcvi(unit,ic,1,1,nbev,0,bdetx,anyf,status)
        call ftgcno(unit,.false.,'DETY',ic,status)
        call ftgcvi(unit,ic,1,1,nbev,0,bdety,anyf,status)
        call ftgcno(unit,.false.,'PI',ic,status)
        call ftgcvi(unit,ic,1,1,nbev,0,bpi,anyf,status)
        if(index(instrume,'SIS').ne.0.and.
     & index(telescop,'ASCA').ne.0) then
          call ftgcno(unit,.false.,'CCDID',ic,status)
          call ftgcvi(unit,ic,1,1,nbev,0,bccdid,anyf,status)
        endif
c====== look at the number of channels for coding DETX
c=== Get the keywords to decode this from read_events...

c=== Look at the keywords for pha and DETX,DETY (defined in 
c=== read_events.f (so the background files should have the 
c=== same header organization than the events files...) 

         call ftgkyj(unit,TLMAXPHA,pidetbck1,comment,status)
         call ftgkyj(unit,TLMINPHA,pidetbck2,comment,status)
           pidetbck=abs(pidetbck1)+abs(pidetbck2)+1
c
         call ftgkyj(unit,TLMAXDX,xdetbck,comment,status)
         call ftgkyj(unit,TLMAXDY,ydetbck,comment,status)

C========First rebin for SIS and 2047 mode. 
        if(index(instrume,'SIS').ne.0.and.
     & index(telescop,'ASCA').ne.0.and.pidetbck.eq.2048) then
         do i=1,nbev
          if(bpi(i).le.1023) bpi(i)=int(bpi(i)/8)
          if(bpi(i).ge.1024.and.bpi(i).le.1535) 
     >                       bpi(i)=int((bpi(i)-1024)/4+128)
          if(bpi(i).ge.1536) bpi(i)=int((bpi(i)-1536)/2+256)
         end do 
         pidetbck=512
        endif
C=======Compare with phabins saved from events file.
        scalep=int(pidetbck/phabins)
        scalex=int(xdetbck/xbins)
        scaley=int(ydetbck/ybins)
        if(scalep.ne.1.and.inite.eqv..true.) then 
         print*,'WARNING: Factor', scalep,' between background'//
     > ' and evt file energy bins'
         print*,' PI file will be rebined to', phabins,' bins'
         inite=.false.
        endif
        if(scalex.ne.1.and.initx.eqv..true.) then 
         print*,'WARNING: Factor', scalex,' between background'//
     > ' and evt file image size (X)'
         print*,' X-coordinate will be rebined to', xbins,' bins'
         initx=.false.
        endif
        if(scaley.ne.1.and.inity.eqv..true.) then 
         print*,'WARNING: Factor', scaley,' between background'//
     > 'and evt file image size (Y)'
         print*,' Y-coordinate will be rebined to', ybins,' bins'
         inity=.false.
        endif
        call ftclos(unit,status)
        call ftgerr(status,errtext)
        print*,'    END reading this file'
        print*,'    FITSIO context: ',errtext 
        if(status.ne.0) then
         print*,'>> ERROR: make_bkg: error reading background file.'
         stop
        endif

c---------- put events from this bkg file to bkg map
        weight=exp_gti/bexp
        if(index(bckmeth,'caldb').ne.0) weight=hrig(ih)*weight
        print*,'    final weight of one bkg event:',weight

c    Built background image like evt image
        do 120 i=1,nbev
c------------- bkg photon # i
c............. check energy of the photon # i
c.  Rebin the energy on the number of chanels 
C the channel is modified here because the equivalence energy/pi is done 
c  with the rmf associated with the event file
            ch=int(bpi(i)/scalep)
            be1=ch2e1(ch,instr_id)
            be2=ch2e2(ch,instr_id)
c------  IF e1 and e2 are negative NO CUT made in energy. (IMH - May 2001) 
            if(e1.ge.0.and.e2.ge.0.) then
             if(be1.lt.e1.or.be2.gt.e2) then
              nout1=nout1+1
              goto 120
             endif
            endif
c..... modify the x and y coordinates to create same size background image
c............. good/bad pixel on detector (or account on SIS clock mode)
C=== and make sure that we are reading the same CCD as for the events file 
C  for SIS-- (the background events are in 4 CCD mode) 
            idet=int(bdetx(i)/scalex)
            jdet=int(bdety(i)/scaley)
            if(.not.valid_det_pixel(idet,jdet,instrume,ccdpow)) then
            nout2=nout2+1
            goto 120
            endif
c............. define bkg photon position on the summed image
c........... (looking "down" on the detector.. mirror the ydet coordinates..)
            if(img_seed.eq.0) then
              xdet=dble(idet)
              ydet=dble(jdet)
            else
              xdet=dble(idet)-0.5+ft_ran2(seed1)
              ydet=dble(jdet)-0.5+ft_ran2(seed1)
            endif
            xx0=det_x0+detoff_x
            xy0=det_y0+detoff_y
            xrot=pa_pnt
         call ftwldp(xdet,ydet,sky_ra0,sky_dec0,xx0,xy0,
     &     sky_dx,sky_dy,xrot,'-TAN',ra,dec,status)
             xrot= img_rot
         call ftxypx(ra,dec,img_ra0,img_dec0,img_x0,img_y0,
     &   img_dx,img_dy,xrot,'-TAN',ximg,yimg,status)
            iimg=nint(ximg)
            jimg=nint(yimg)
c............. put into image
           if(iimg.lt.1.or.iimg.gt.nix.or.jimg.lt.1.or.jimg.gt.niy)
     >  then
              nout3=nout3+1
              goto 120
            endif
            bkg(iimg,jimg)=bkg(iimg,jimg)+weight
            nin1=nin1+1
c------------- photon # i finished
 120      continue 
 110    continue 
         print*,'    summed:',nin1
         print*,'    out (cut in energy)  :',nout1
         print*,'    out (cut for calibration sources)  :',nout2
         print*,'    out (outside defined image)  :',nout3
         nout=nout1+nout2+nout3     
         print*,' Total  out:',nout
 300     continue
      return
      end



