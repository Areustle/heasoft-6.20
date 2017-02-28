      subroutine read_events(iunit,evf_name,status,nevtot)

c--------------------------------------------------------------         
c     Read one events file and make some basic calculations
c        iunit - i/o unit #
c        evf_name - name of events file
c        status - FITSIO status + it will be set locally if smth is wrong:
c
c     The following calculations are made:
c     1. instr_id is defined for each ev.file (0,1,2,3)
c     2. zexp_gti - exposure for event list (from GTIs)
c     3. zpi column for SISs - pha from event file, rebinned into
c        512 RMF channels
c        note, that for GISs zpi is pulse invariant as read from events file
c
c     Created: Mon May 16 01:01:30 EDT 1994
c
c  A change is made to introduce CCDPOW parameter, needed for 2-ccd clock mode.
c..............................................................
c
c     Revised: Fri Dec 16 20:04:40 MSK 1994
c     For SIS we are using now PI column instead of PHA
c     Revised: Tue Sep 21 10:45:22 EDT 1999 (Ilana Harrus)
c     Look at extra keywords to see how much contraction is done in Pha/pi and 
C     detx/dety 
c     also supress call to read all the time variables: never used in 
C     Image generation.
c-------------------------------------------------------------- 
c     Revised: Tue Nov 16 11:32:02 EST 1999 (Ilana Harrus)
c              Include mofifications to get the program to recognize
c              what are the keywords associated to X,Y,DETX,DETY,... 
c              This is a step toward a generalization for others missions.
c
c..............................................................
c     Changed: Tue Feb 29 10:28:34 EST 2000: included in the Linux version. 
c
c-------------------------------------------------------------- 
      implicit none
      include 'common_evts.inc'
      include 'common_defpixel.inc'
      include 'common_keywords.inc'


c---- input params
      character*(*) evf_name
      integer iunit,status,nevtot
c---- for fitsio lib
      integer ngtic
      integer rwmode,block,hdutype
      character errtext*30, comment*80
      logical exact,anyf
      integer*2 inull

c----- misc
      integer ic
      integer index,i
      integer len_trim
      real crate
      character str_ccd*8
      integer*2 int_ccd(4)


c---- temporary arrays
      integer*4 ti2(nevtotfm)
c      real*8 tr8(nevtotfm)
      real*8 t2ng(ngtim)


      print 10,evf_name
 10   format(/40('-')/' Read data from file:',a)

c==== open fits file
       rwmode=0
       call ftopen(iunit, evf_name, rwmode, block, status)

c==== get some keyword values from PRIMARY header
       call ftmahd(iunit,1,hdutype,status)
c---- telescope 
       call ftgkys(iunit,'TELESCOP',telescop,comment,status)
      print*,' telescop: ',telescop
c---- instrument
       call ftgkys(iunit,'INSTRUME',instrume,comment,status)
      print*,' instrument: ',instrume
c.... define instr_id  - we will need it in spectral subroutines
       if(index(instrume,'SIS0').ne.0.and.
     & index(telescop,'ASCA').ne.0 ) then
        instr_id=0
c.. SIS0 clock mode
        call ftgkys(iunit,'S0CCDPOW',str_ccd,comment,status)
        read(str_ccd,"(4i1)") (int_ccd(i),i=1,4)
      print*,(int_ccd(i),i=1,4),
     & ' Which S0 CCDs are in use(0123): 0=OFF 1=ON'
        do i=1,4
          ccdpow(i)=.true.
          if(int_ccd(i).eq.0) ccdpow(i)=.false.
        enddo
       end if
        
       if(index(instrume,'SIS1').ne.0.and.
     & index(telescop,'ASCA').ne.0) then
        instr_id=1
c.. SIS1 clock mode
        call ftgkys(iunit,'S1CCDPOW',str_ccd,comment,status)
        read(str_ccd,"(4i1)") (int_ccd(i),i=1,4)
        print*,(int_ccd(i),i=1,4),
     &' Which S1 CCDs are in use(0123): 0=OFF 1=ON'
        do i=1,4
          ccdpow(i)=.true.
          if(int_ccd(i).eq.0) ccdpow(i)=.false.
        enddo
      end if

      if(index(instrume,'GIS2').ne.0.and.
     & index(telescop,'ASCA').ne.0) instr_id=2
      if(index(instrume,'GIS3').ne.0.and.
     & index(telescop,'ASCA').ne.0) instr_id=3

c==== move to EVENTS extension
      call ftmahd(iunit,2,hdutype,status)

c==== read some keyword values from EVENTS extension
c==== number of events in list
      nevtot=0
      call ftgkyj(iunit,'NAXIS2',nevtot,comment,status)
      print*,' Total number of events in this list:',nevtot
      if(nevtot.gt.nevtotfm) then
      print*,' Number of events in file is larger that maximum allowed'
      stop
      endif

c---- misc
      call ftgkys(iunit,'DATE-OBS',date_obs,comment,status)
      call ftgkys(iunit,'DATE-END',date_end,comment,status)

      if(index(telescop,'ASCA').ne.0) then 
       call ftgkys(iunit,'TIME-OBS',time_obs,comment,status)
       call ftgkys(iunit,'TIME-END',time_end,comment,status)
       call ftgkyd(iunit,'MJD-OBS',mjd_obs,comment,status)
       print*,' Observation start: ',date_obs,time_obs
       print*,' Observation end  : ',date_end,time_end
      endif

       print*,' MJD obs.start    : ',mjd_obs
c---- 
c---- WCS information
      call ftgkyd(iunit,'EQUINOX',equinox,comment,status)
      call ftgkys(iunit,'RADECSYS',radecsys,comment,status)
      if(index(telescop,'ASCA').ne.0) then           
       call ftgkyd(iunit,'RA_PNT',ra_pnt,comment,status)
       call ftgkyd(iunit,'DEC_PNT',dec_pnt,comment,status)
       call ftgkyd(iunit,'PA_PNT',pa_pnt,comment,status)
      endif

C  Now we try to generalize the approach: determine automatically which 
C    fields should be read according to the instrument value.

C   First look up how many fields in each row. 
        call ftgkyj(iunit,'TFIELDS', tfield,comment,status)

       do i=1,tfield
         namekw='TTYPE'
       if(i.lt.10) write(namekey,123) namekw(:len_trim(namekw)),i
       if(i.ge.10.and.i.lt.100)  
     + write(namekey,124) namekw(:len_trim(namekw)),i
       call ftgcrd(iunit,namekey,valuekey,status)

c------ Find where is 'X'
      if(index(valuekey,'x').ne.0.and.(index(valuekey,'detx').eq.0
     + .and.index(valuekey,'rawx').eq.0)) iskyx=i
      if(index(valuekey,'X').ne.0.and.(index(valuekey,'DETX').eq.0
     +  .and.index(valuekey,'RAWX').eq.0)) iskyx=i

c------ Assume that 'Y' is just right after 
c         (because TTYPE contains an "Y")
        iskyy= iskyx+1

c------ Find where is 'PHA'
      if((index(valuekey,'pha').ne.0.or.index(valuekey,'PHA').ne.0)
     + .and.index(valuekey,'phas').eq.0.and.index(valuekey,'PHANEW')
     +  .eq.0) ipha=i

c------ Find where are 'DETX' and 'DETY'
      if(index(valuekey,'detx').ne.0.or.index(valuekey,'DETX').ne.0) 
     + idetx=i
      if(index(valuekey,'dety').ne.0.or.index(valuekey,'DETY').ne.0)
     + idety=i


      end do
c  Define all the relevant keyword to be looked at: 
       print*,'SKYX,SKYY,DETX,DETY,PHA are coded at keywords numbers',
     +   iskyx,iskyy,idetx,idety,ipha

c-------- Then look at all the relevant keywords:
c------- Reference pixel.
   
         namekw='TCRPX'

       if(iskyx.lt.10) 
     + write(TCRPXX,123) namekw(:len_trim(namekw)),iskyx
       if(iskyx.ge.10.and.iskyx.lt.100)  
     + write(TCRPXX,124) namekw(:len_trim(namekw)),iskyx

       if(iskyy.lt.10) 
     + write(TCRPXY,123) namekw(:len_trim(namekw)),iskyy
       if(iskyy.ge.10.and.iskyy.lt.100)  
     + write(TCRPXY,124) namekw(:len_trim(namekw)),iskyy

       if(idetx.lt.10) 
     + write(TCRPXDX,123) namekw(:len_trim(namekw)),idetx
       if(idetx.ge.10.and.idetx.lt.100)  
     + write(TCRPXDX,124) namekw(:len_trim(namekw)),idetx

       if(idety.lt.10) 
     + write(TCRPXDY,123) namekw(:len_trim(namekw)),idety
       if(idety.ge.10.and.idety.lt.100)  
     + write(TCRPXDY,124) namekw(:len_trim(namekw)),idety

c------- Pixel value
         namekw='TCRVL'

       if(iskyx.lt.10) 
     + write(TCRVLX,123) namekw(:len_trim(namekw)),iskyx
       if(iskyx.ge.10.and.iskyx.lt.100)  
     + write(TCRVLX,124) namekw(:len_trim(namekw)),iskyx

       if(iskyy.lt.10) 
     + write(TCRVLY,123) namekw(:len_trim(namekw)),iskyy
       if(iskyy.ge.10.and.iskyy.lt.100)  
     + write(TCRVLY,124) namekw(:len_trim(namekw)),iskyy

c--------increment
         namekw='TCDLT'

       if(iskyx.lt.10) 
     + write(TCDLTX,123) namekw(:len_trim(namekw)),iskyx
       if(iskyx.ge.10.and.iskyx.lt.100)  
     + write(TCDLTX,124) namekw(:len_trim(namekw)),iskyx

       if(iskyy.lt.10) 
     + write(TCDLTY,123) namekw(:len_trim(namekw)),iskyy
       if(iskyy.ge.10.and.iskyy.lt.100)  
     + write(TCDLTY,124) namekw(:len_trim(namekw)),iskyy

c----------type
         namekw='TCTYP'

       if(iskyx.lt.10) 
     + write(TCTYPX,123) namekw(:len_trim(namekw)),iskyx
       if(iskyx.ge.10.and.iskyx.lt.100)  
     + write(TCTYPX,124) namekw(:len_trim(namekw)),iskyx

       if(iskyy.lt.10) 
     + write(TCTYPY,123) namekw(:len_trim(namekw)),iskyy
       if(iskyy.ge.10.and.iskyy.lt.100)  
     + write(TCTYPY,124) namekw(:len_trim(namekw)),iskyy

c-------- Optic axis info - Does not exist for Chandra... 
         namekw='OPTIC'

       if(idetx.lt.10) 
     + write(OPTICDX,123) namekw(:len_trim(namekw)),idetx
       if(idetx.ge.10.and.idetx.lt.100)  
     + write(OPTICDX,124) namekw(:len_trim(namekw)),idetx

       if(idety.lt.10) 
     + write(OPTICDY,123) namekw(:len_trim(namekw)),idety
       if(idety.ge.10.and.idety.lt.100)  
     + write(OPTICDY,124) namekw(:len_trim(namekw)),idety

C.....  Get the extra keyword about the data coding.
c
         namekw='TLMAX'

       if(ipha.lt.10) 
     + write(TLMAXPHA,123) namekw(:len_trim(namekw)),ipha
       if(ipha.ge.10.and.ipha.lt.100)  
     + write(TLMAXPHA,124) namekw(:len_trim(namekw)),ipha

       if(idetx.lt.10) 
     + write(TLMAXDX,123) namekw(:len_trim(namekw)),idetx
       if(idetx.ge.10.and.idetx.lt.100)  
     + write(TLMAXDX,124) namekw(:len_trim(namekw)),idetx

       if(idety.lt.10) 
     + write(TLMAXDY,123) namekw(:len_trim(namekw)),idety
       if(idety.ge.10.and.idety.lt.100)  
     + write(TLMAXDY,124) namekw(:len_trim(namekw)),idety

         namekw='TLMIN'

       if(ipha.lt.10) 
     + write(TLMINPHA,123) namekw(:len_trim(namekw)),ipha
       if(ipha.ge.10.and.ipha.lt.100)  
     + write(TLMINPHA,124) namekw(:len_trim(namekw)),ipha

 123  format(a,i1)
 124  format(a,i2)

         call ftgkyd(iunit,TCRPXX, sky_x0,comment,status)
         call ftgkyd(iunit,TCRPXY, sky_y0,comment,status)
         call ftgkyd(iunit,TCRVLX, sky_ra0,comment,status)
         call ftgkyd(iunit,TCRVLY, sky_dec0,comment,status)
         call ftgkyd(iunit,TCDLTX, sky_dx,comment,status)
         call ftgkyd(iunit,TCDLTY, sky_dy,comment,status)
         call ftgkys(iunit,TCTYPX, skytyp_ra,comment,status)
         call ftgkys(iunit,TCTYPY, skytyp_dec,comment,status)
        print *,TCRPXX,TCRPXY,TCRVLX,TCRVLY,TCDLTX,TCDLTY,
     +  TCTYPX,TCTYPY
        print *,sky_x0,sky_y0,sky_ra0,sky_dec0,sky_dx,sky_dy,
     +  skytyp_ra,skytyp_dec,status
c-------------
        if(index(telescop,'ASCA').ne.0) then
         call ftgkyd(iunit,TCRPXDX, det_x0,comment,status)
         call ftgkyd(iunit,TCRPXDY, det_y0,comment,status)
         call ftgkyd(iunit,OPTICDX, detopt_x,comment,status)
         call ftgkyd(iunit,OPTICDY, detopt_y,comment,status)
       endif


C.....  Get the extra keyword about the energy and detx/dety 
c                                          (Ilana Harrus -Sept 1999)

         call ftgkyj(iunit,TLMAXPHA,phabins1,comment,status)
         call ftgkyj(iunit,TLMINPHA,phabins2,comment,status)
         phabins=abs(phabins1)+abs(phabins2)+1

         call ftgkyj(iunit,TLMAXDX,xbins,comment,status)
         call ftgkyj(iunit,TLMAXDY,ybins,comment,status)
 

         print*,
     & ' The energy data are coded using', phabins ,' number of bins.'

         print*,
     & ' The image is', xbins ,' by',ybins


      print*,' WCS info:'
      print*,'     EQUINOX: ', equinox
      print*,'     RADECSYS: ', radecsys
      print*,'     <RA>  :', ra_pnt
      print*,'     <DEC> :', dec_pnt
      print*,'     <ROLL>:', pa_pnt

      print*,"STATUS:",status

c==== read events
      exact=.false.
      inull=0

      call ftgcno(iunit,exact,'X',ic,status)
      if(index(telescop,'ASCA').ne.0) then
      call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
      do i=1,nevtot
         skyx(i)=float(ti2(i))
      end do 
      endif
c-----
      call ftgcno(iunit,exact,'Y',ic,status)
      if(index(telescop,'ASCA').ne.0) then
      call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
      do i=1,nevtot
         skyy(i)=float(ti2(i))
      end do 
      endif
c-----
      call ftgcno(iunit,exact,'PHA',ic,status)
      call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
      do i=1,nevtot
         pha(i)=ti2(i)
      end do 
      call ftgcno(iunit,exact,'PI',ic,status)
      call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
      do i=1,nevtot
         pi(i)=ti2(i)
      end do 
      if(index(telescop,'ASCA').ne.0) then
      call ftgcno(iunit,exact,'RAWX',ic,status)
      call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
      do i=1,nevtot
         rawx(i)=ti2(i)
      end do 
      call ftgcno(iunit,exact,'RAWY',ic,status)
      call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
      do i=1,nevtot
         rawy(i)=ti2(i)
      end do 
      endif
c----------
      call ftgcno(iunit,exact,'DETX',ic,status)
      if(index(telescop,'ASCA').ne.0) then
       call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
       do i=1,nevtot
          detx(i)=float(ti2(i))
       end do 
      endif
c---
      call ftgcno(iunit,exact,'DETY',ic,status)
      if(index(telescop,'ASCA').ne.0) then
       call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
       do i=1,nevtot
          dety(i)=float(ti2(i))
       end do 
      endif
c
c---- GIS specific
      if(index(instrume,'GIS').ne.0.and.
     & index(telescop,'ASCA').ne.0) then
         call ftgcno(iunit,exact,'RTI',ic,status)
         call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
         do i=1,nevtot
            rti(i)=ti2(i)
         end do 
         call ftgcno(iunit,exact,'RISE_TIME',ic,status)
         call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
         do i=1,nevtot
            rise_time(i)=ti2(i)
         end do 
         call ftgcno(iunit,exact,'SPREAD',ic,status)
         call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
         do i=1,nevtot
            spread(i)=ti2(i)
         end do 
c....... set (SIS) specific to ZERO
         do i=1,nevtot
            grade(i)=0
            ccdid(i)=0
         end do 
      endif

c---- SIS specific
      if(index(instrume,'SIS').ne.0.and.
     & index(telescop,'ASCA').ne.0) then
         call ftgcno(iunit,exact,'GRADE',ic,status)
         call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
         do i=1,nevtot
            grade(i)=ti2(i)
         end do 
         call ftgcno(iunit,exact,'CCDID',ic,status)
         call ftgcvi(iunit,ic,1,1,nevtot,inull,ti2,anyf,status)
         do i=1,nevtot
            ccdid(i)=ti2(i)
         end do 
c....... set (GIS) specific to ZERO
         do i=1,nevtot
            rti(i)=0
            rise_time(i)=0
            spread(i)=0
         end do 

      endif

c==== rebinn PHA into 512 channels (according to RMF chann.) - SIS ONLY.
c==== and only in the case that it was coded on 2047 channels.
      if(index(instrume,'SIS').ne.0.and.phabins.eq.2048) then
         do i=1,nevtot
c----------- we are using now PI column from fits file instead of PHA
          if(pi(i).le.1023) pi(i)=pi(i)/8
          if(pi(i).ge.1024.and.pi(i).le.1535) pi(i)=(pi(i)-1024)/4+128
          if(pi(i).ge.1536) pi(i)=(pi(i)-1536)/2+256
         end do 
         phabins = 512
      endif

c==== move to STDGTI extension
      call ftmahd(iunit,3,hdutype,status)

c==== read STDGTI
      call ftgkyj(iunit,'NAXIS2',ngti,comment,status)
      print*,' Total number of GTIs in this list:',ngti
      if(ngti.gt.ngtim) then
         status=1003
         print*,' >>> ERROR: read_events: ngti > ngtim. Stopping...'
         stop
      endif
      ngtic=ngti

      call ftgcno(iunit,exact,'START',ic,status)
      call ftgcvd(iunit,ic,1,1,ngti,inull,t2ng,anyf,status)
      do i=1,ngti
         start(i)=t2ng(i)
      end do 
      call ftgcno(iunit,exact,'STOP',ic,status)
      call ftgcvd(iunit,ic,1,1,ngti,inull,t2ng,anyf,status)
      do i=1,ngti
         stop(i)=t2ng(i)
      end do 

c---- calculate total expos.time from GTIs
      exp_gti=0.d0
      do i=1,ngti
         exp_gti=exp_gti+stop(i)-start(i)
      end do 
      print*,' Exposure time from GTIs:',exp_gti,' sec'
      crate=float(nevtot)/exp_gti
      print*,' Average count rate:',crate,' counts/sec'

c==== close file
      call ftclos(iunit,status)

c==== final status
      call ftgerr(status,errtext)
      print*,' END reading this file'
      print*,' FITSIO context: ',errtext 


      return
      end


