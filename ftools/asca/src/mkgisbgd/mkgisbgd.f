      subroutine mkgisbgd
      implicit none

C     A program to calculate GIS blank sky
C     spectrum and image from point-source-excluded
C     blank sky event files by calculating the exposure time 
C     taking account of the source-excluding effect.

C     September 24, 1999,Ken Ebisawa (ebisawa@olegacy.gsfc.nasa.gov)

c     v1.1 Fixed a bug on SUN and Solaris,   October 20, 1999
c     changed ftgcvi --> ftgcvj
C     Ken Ebisawa

c     v1.2 Reads PI_LOW and PI_HIGH values for the PI
C     lower and upper boundaries (Ken Ebisawa, 1999-12-17)

c     v1.3 Not terminate when floating exeption error occurs,
C     when dividing the image with zero-exposure pixels.
C     (Ken Ebisawa, 2000-1-6)

C     v1.4 Work with 128 channel event files (those taken
C     during the on-board GIS3 problem and fixed with the
C     gis3bitfix.e script).  Ken Ebisawa 2000-3-7
C     v1.5 Initialized the status variable in the beginning
C     of the program. Ken Ebisawa 2001-10-18

C     v1.6 Supports HDUVERS=2 WMAPs.  kaa 1/11/02

C     v1.7 Primary image was not correct when input file has 64x64 channel.
C     I am glad to Thomas Pannuti <tpannuti@ipac.caltech.edu> who pointed
C     out the problem.
C     Ken Ebisawa 07/07/2004 (Tanabata version)


      character(999)  filelist,specfile,gtifile,outfile,contxt,
     $     blankfile(999),string
      common/infilenames/filelist,specfile,gtifile

      logical fileopen

      integer status,inunit,inspecunit, nblankfile,blocksize,
     $     rwmode,gtiunit, pi_low, pi_high
      data status/0/
      integer selected_pixels(256,256), hdutype,detchans,wmapsize
      character extname*8, comment*80
      real backscal
      logical clobber_outfile,exists

C     Error report
      character(40) taskname
      common /task/ taskname

      taskname = 'mkgisbgd ver 1.7 (2004-07-07)'

      call fcecho(taskname)

C     Get the parameters from the parameter file
C     Get the blank sky file list
      call uclgst('filelist',filelist,status)
      if(status.ne.0) then
         contxt = 'Could not get the input file list'
         call fcerr(contxt)
         go to 999
      endif
      fileopen=.false.
      contxt = 'Input file list open error'
      call ftgiou(inunit,status)
      open(UNIT=INUNIT,file=filelist,status='old',err=9)
      fileopen=.true.
      contxt = 'Input event file list exists'
      call fcecho(contxt)
      nblankfile = 0
 1    continue
      read(INUNIT,'(a)',end=10) string
      if(lge(string,'!')) then
         nblankfile = nblankfile+1
         blankfile(nblankfile)=string
      endif
      go to 1
 10   close(INUNIT)

 9    continue
      if(.NOT.fileopen) then
      call fcerr(contxt)
         go to 999
      endif

C     Get the spectral file
C     Read the input spectral file
C     What we need is only the WMAP and the BACKSCAL keyword value
      call uclgst('specfile',specfile,status)
      if(status.ne.0) then
         contxt = 'Could not get the spectarl file name'
         call fcerr(contxt)
         go to 999
      endif
      rwmode=0
      blocksize=1
      call ftgiou(inspecunit,status)
      call ftopen(inspecunit,specfile,rwmode,blocksize,status)
      if(status.ne.0) then
         contxt = 'Could not open the input spectral file'
         call fcerr(contxt)
         go to 999
      endif
C     read the WMAP of the spectral file
      call read_wmap(inspecunit, selected_pixels, wmapsize, status)
      if(status.ne.0) then
         contxt = 'Error in reading the WMAP of the input spectral file'
         call fcerr(contxt)
         go to 999
      endif
 11   continue
      call ftmrhd(inspecunit,1,hdutype,status)
      if (status.ne.0) go to 999
      call ftgkys(inspecunit,'EXTNAME',extname,comment,status)
      if (status.ne.0) go to 999
      if(extname.eq.'SPECTRUM') then 
C     obtain BACKSCAL value from the spectral extention of the file
         call ftgkye(inspecunit,'BACKSCAL',backscal,comment,status)
         if (status.ne.0) go to 999
C     obtain the number of spectral channels
         call ftgkyj(inspecunit,'DETCHANS',detchans,comment,status)
         if (status.ne.0) go to 999
         if(detchans.ne.1024.and.detchans.ne.256.
     $        and.detchans.ne.128) then
            contxt = 'detchans is not either 1024, 256 or 128.'
            call fcerr(contxt)
            go to 999
         endif
      else
         go to 11
      end if
C     close the spectral file
      call ftclos(inspecunit,status)
      if (status.ne.0) go to 999
      

C     Get the optional GTI file
      call uclgst('gtifile', gtifile,status)
      if(status.ne.0) then
         contxt = 'Could not get the GTI file'
         call fcerr(contxt)
         go to 999
      endif
      if(gtifile.ne.'none'.and.gtifile.ne.'NONE') then
         call ftgiou(gtiunit,status)
         call open_gti(gtiunit,gtifile,status)
      endif

C     Get the output file name
      call uclgst('output',  outfile,status)
      if(status.ne.0) then
         contxt = 'Could not get the output file name'
         call fcerr(contxt)
         go to 999
      endif
      call uclgsb('clobber', clobber_outfile, status)
      IF(clobber_outfile) THEN
         inquire(file=outfile,exist=exists)
         if(exists) then
            contxt =
     $  'Output file exists and will be overwritten since clobber=yes'
            call fcecho(contxt)
         endif
C     overwrite the output file
         CALL CLOBBER(OUTFILE,STATUS)
      endif

C     Get the optional low and high PI limits (new feature added on 1999-12-17)
      call uclgsi('PI_LOW',  pi_low, status)
      if(status.ne.0) then
         contxt = 'Could not get the PI_LOW value'
         call fcerr(contxt)
         go to 999
      endif

      call uclgsi('PI_HIGH', pi_high, status)
      if(status.ne.0) then
         contxt = 'Could not get the PI_HIGH value'
         call fcerr(contxt)
         go to 999
      endif

C     Read the event files, correct exposure and calculate correct 
C     normalization (to be written in the output BACKSCAL).
C     Output the background file, which includes the blank sky
C     image and exposure map, as well as the background
C     spectrum
      call calc_bgd(blankfile,nblankfile,selected_pixels,wmapsize,
     $     backscal, detchans, outfile, pi_low, pi_high,
     $     status)

 999  continue

      if(status.ne.0) call fcerrm(status)

      end

      subroutine read_wmap(unit, selected_pixels, wmapsize, status)
      implicit none
      integer unit, selected_pixels(256,256), wmapsize, status
C     Read GIS WMAP or DET XY image, and output 256x256 Wmap or
C     image.  Outer parts of the WMAP (which is usually smaller
C     than 256x256) should be filled with -1.
C     
C     It is assumed that the fits file is opened and
C     the current header unit (CHU) is pointing the correct
C     extention.

      integer xsize, ysize, i, j,wmap(256,256)
      integer crpix1,crpix2,crval1,crval2,ihdvrs
      character extname*8, comment*80, contxt*80, hduvers*10
      logical anyf
      integer rawxbins, rawybins, wmrebin
      

      do i = 1, 256
         do j = 1, 256
            wmap(i,j) = 0
            selected_pixels(i,j)=-1
         end do
      end do

      CALL ftgkys(unit,'HDUCLAS2',extname,comment,status)
      IF ( status .NE. 0 ) then
         contxt='HDUCLAS2 is not found'
         GOTO 99
      endif

      if(index(extname,'WMAP').gt.0) then
C     This is a WMAP
         CALL ftgkys(unit,'HDUVERS',hduvers,comment,status)
         IF ( status .NE. 0 ) THEN
            status = 0
            CALL ftgkys(unit,'HDUVERS1',hduvers,comment,status)
         ENDIF
         IF ( status .NE. 0 ) THEN
            ihdvrs = 0
            status = 0
         ELSE
            READ(hduvers(1:1),'(i1)') ihdvrs
         ENDIF
         CALL ftgkyj(unit,'RAWXBINS',rawxbins,comment,status)
         if(status.ne.0) then
            contxt='Failed to get RAWXBINS'
            go to 99
         endif
         CALL ftgkyj(unit,'RAWYBINS',rawybins,comment,status)
         if(status.ne.0) then
            contxt='Failed to get RAWYBINS'
            go to 99
         endif
         IF ( ihdvrs .LT. 2 ) THEN
            CALL ftgkyj(unit,'WMREBIN',wmrebin,comment,status)
            contxt='Failed to get WMREBIN'
         ELSE
            CALL ftgkyj(unit,'CDELT1P',wmrebin,comment,status)
            contxt='Failed to get CDELT1P'
         ENDIF
         if(status.ne.0) go to 99
         if(wmrebin.ne.1) then
            write(contxt,'(a20,i2)') 'WMAP is binned by ',wmrebin
            call fcecho(contxt)
            call fcecho(
     $      'This will be all right, but why did you do that??')
            call fcecho(
     &      '(A bin factor of 1 is the default and recommended.)')
         endif
         if((rawxbins.ne.256.or.rawybins.ne.256).and.
     $        (rawxbins.ne.64.or.rawybins.ne.64)) then
            contxt='The image binsize is not 256 or 64'
            go to 99
         endif

         wmapsize = rawxbins/wmrebin

         CALL ftgkyj(unit,'NAXIS1',xsize,comment,status)
         if(status.ne.0) then
            contxt='NAXIS1 is not found'
            go to 99
         endif
         CALL ftgkyj(unit,'NAXIS2',ysize,comment,status)
         if(status.ne.0) then
            contxt='NAXIS2 is not found'
            go to 99
         endif
         IF ( ihdvrs .LT. 2 ) THEN
            CALL ftgkyj(unit,'CRPIX1',crpix1,comment,status)
            if(status.ne.0) then
               contxt='CRPIX1 is not found'
               go to 99
            endif
            CALL ftgkyj(unit,'CRPIX2',crpix2,comment,status)
            if(status.ne.0) then
               contxt='CRPIX2 is not found'
               go to 99
            endif
            CALL ftgkyj(unit,'CRVAL1',crval1,comment,status)
            if(status.ne.0) then
               contxt='CRVAL1 is not found'
               go to 99
            endif
            CALL ftgkyj(unit,'CRVAL2',crval2,comment,status)
            if(status.ne.0) then
               contxt='CRVAL2 is not found'
               go to 99
            endif
         ELSE
            CALL ftgkyj(unit,'CRPIX1P',crpix1,comment,status)
            if(status.ne.0) then
               contxt='CRPIX1P is not found'
               go to 99
            endif
            CALL ftgkyj(unit,'CRPIX2P',crpix2,comment,status)
            if(status.ne.0) then
               contxt='CRPIX2P is not found'
               go to 99
            endif
            CALL ftgkyj(unit,'CRVAL1P',crval1,comment,status)
            if(status.ne.0) then
               contxt='CRVAL1P is not found'
               go to 99
            endif
            crval1 = (crval1 + (wmrebin-1)/2.)/wmrebin
            CALL ftgkyj(unit,'CRVAL2P',crval2,comment,status)
            if(status.ne.0) then
               contxt='CRVAL2P is not found'
               go to 99
            endif
            crval2 = (crval2 + (wmrebin-1)/2.)/wmrebin
         ENDIF

         call ftg2dj(unit,0,0,256,xsize,ysize,wmap,anyf,status)
         if(status.ne.0) then
            contxt='Error in reading the WMAP values'
            go to 99
         endif
         do i = 1, xsize
            do j = 1, ysize
               selected_pixels(i-crpix1+crval1,j-crpix2+crval2)
     $              = wmap(i,j)
            end do
         end do
      end if

C     Normal End
      return

C     End with an error
 99   continue
      CALL fcerr(contxt)
      CALL fcerrm(status)
      end

      subroutine calc_bgd(blankfile,nblankfile,selected_pixels,
     $     wmapsize, backscal, detchans, outfile,
     $     pi_low, pi_high, status)
      implicit none

      integer wmapsize,selected_pixels(256,256),status,
     $     nblankfile,detchans, pi_low, pi_high

      character*(*) blankfile(nblankfile), outfile
      real backscal
      real indef
      data indef/-999./

      integer blocksize, inunit, outunit, i, j, k, l,
     $     pispec(0:1023),rwmode,hdutype,nrow_gti,n_new_gti,
     $     nevents,pispec256(0:255)
      
      real exposure_map(256,256),mask_image(256,256),exposure,
     $     corrected_exposure, exposure_corrected_image(256,256),
     $     binned_exposure_map(64,64)
C     the following is added in version 1.7
      real binned_exposure_corrected_image(64,64)
      integer det_image(256,256),binned_det_image(64,64)
      logical anyf, select_this_event, extend
      character comment*80, instrume*8,contxt*255
      double precision gti_start(999),gti_end(999)
      double precision new_gti_start(999),new_gti_end(999),
     $     event_time
      integer pha, pi, detx, dety, totcts, naxes(2),total_pixels,
     $     n_good_events, n_selected_events
      character(8) ttype(10),tform(10),tunit(10)

      character(40) taskname
      common /task/ taskname

      character(999)  filelist,specfile,gtifile
      common/infilenames/filelist,specfile,gtifile

c     string length
      integer fcstln,slength
      external fcstln

C     number of selected pixels given in the WMAP
C     note that wmap may be rebinned or may be 64x64ch
      total_pixels = 0
         do i = 1, wmapsize
            do j = 1, wmapsize
               if(selected_pixels(i,j).gt.-1) then
                  total_pixels = total_pixels + 1
               end if
            end do
         end do

      call ftgiou(inunit, status)
      call ftgiou(outunit,status)

C     open the output file
      call ftinit(outunit,outfile,blocksize,status)
      if (status.ne.0) go to 9999
C     initialize the exposure map and the spectral file arrays.
      do i = 1, 256
         do j = 1, 256
            exposure_map(i,j) = 0.0
         end do
      end do
      do i = 0, 1023
         pispec(i)=0
      end do

C     open the source-removed input event file one by one
      do k= 1, nblankfile
         call ftopen(inunit,blankfile(k),rwmode,blocksize,status)
         if(status.ne.0) go to 9999
         slength=fcstln(blankfile(k))
         call fcecho(' ')
         contxt='Blank sky file: '//blankfile(k)(:slength)
         call fcecho(contxt)

C     Read the primary image of the event file (mask file)
         call ftmahd(inunit,1,hdutype,status)
         call ftg2de(inunit,0,0.0,256,256,256,mask_image,anyf,status)

C     Now, mask_image(256,256) has mask values. Pixels with 
C     0 are not used, pixels with 1 are used.

C     Read the GTI extention
         call ftmahd(inunit,3,hdutype,status)
         call ftgkyj(inunit,'NAXIS2',nrow_gti,comment,status)
         do i = 1, nrow_gti
            call ftgcvd(inunit,1,i,1,1,0.0d0,gti_start(i),anyf,status)
            call ftgcvd(inunit,2,i,1,1,0.0d0,gti_end(i),  anyf,status)
         end do

C     Calculate overlapping GTI of the input GTI and
C     the GTI in the event file
         exposure = 0.0
         do i = 1, nrow_gti
            exposure = exposure +
     $           (REAL(gti_end(i))-REAL(gti_start(i)))
         end do
         write(contxt,'(a36,1pe10.4)')
     $        'Original exposure in the file: ', exposure
         call fcecho(contxt)

         if(gtifile.eq.'none'.or.gtifile.eq.'NONE') then
            n_new_gti=nrow_gti
            do i = 1, n_new_gti
               new_gti_start(i)=gti_start(i)
               new_gti_end(i)=gti_end(i)
            end do
         else
            call add_gti(nrow_gti,gti_start,gti_end,n_new_gti,
     $           new_gti_start,new_gti_end)
         endif

C     total exposure time for this event file
         exposure = 0.0
         do i = 1, n_new_gti
            exposure = exposure +
     $           (REAL(new_gti_end(i))-REAL(new_gti_start(i)))
         end do
         write(contxt,'(a36,1pe10.4)')
     $        'Total exposure after GTI selection: ', exposure
         call fcecho(contxt)

C     accumulate the exposure time, calculate the exposure map in sec/pixel
         do i = 1, 256
            do j = 1, 256
               if(selected_pixels((i-1)/(256/wmapsize)+1,
     $                            (j-1)/(256/wmapsize)+1).gt.-1) then
                  exposure_map(i,j) = exposure_map(i,j)+
     $                 exposure*mask_image(i,j)
               end if
            end do
         end do

C     Read the EVENTS extention and accumulate spectrum
         call ftmahd(inunit,2,hdutype,status)
         call ftgkys(inunit,'INSTRUME',instrume,comment,status)
         call ftgkyj(inunit,'NAXIS2',nevents,comment,status)

         write(contxt,'(a41,i10)')
     $        'Number of events in the file: ', nevents
         call fcecho(contxt)

         n_good_events = 0
         n_selected_events = 0
         do i = 1, nevents
            call ftgcvd(inunit,1,i,1,1,0.0D0,EVENT_TIME,
     $           anyf,status)
            call ftgcvj(inunit,4,i,1,1,0,PHA,anyf,status)
            call ftgcvj(inunit,5,i,1,1,0,PI,anyf,status)
            call ftgcvj(inunit,10,i,1,1,0,DETX,anyf,status)
            call ftgcvj(inunit,11,i,1,1,0,DETY,anyf,status)
C     check if the event is in the GTI
            select_this_event=.false.
            do l = 1, n_new_gti
               if(EVENT_time.ge.new_gti_start(l).and.
     $              EVENT_time.le.new_gti_end(l)) then
                  select_this_event=.true.
                  n_good_events = n_good_events+1
               endif
            end do
C     check if the event is in the selected DETXY region
            if(selected_pixels((DETX-1)/(256/wmapsize)+1,
     $                         (DETY-1)/(256/wmapsize)+1).lt.0) then
               select_this_event=.false.
            endif

C     check if the event is in the selected PI range
            if(PI.lt.PI_LOW.or.PI.gt.PI_HIGH) then
               select_this_event=.false.
            endif

C     take this event
            if(select_this_event) then
               pispec(PI) = pispec(PI) + 1
               det_image(DETX,DETY)=det_image(DETX,DETY)+1
               totcts = totcts + 1
               n_selected_events=n_selected_events+1
            end if
         end do
         write(contxt,'(a41,i10)')
     $        'Number of events after GTI selection: ', n_good_events
         call fcecho(contxt)
         write(contxt,'(a41,i10)')
     $        'Number of events selected           : ',
     $        n_selected_events
         call  fcecho(contxt)
C     close the input file
         call ftclos(inunit,status)
      end do

C     Write the exposure corrected DETECTOR image to the primary
      naxes(1)=wmapsize
      naxes(2)=wmapsize
      do i = 1, 256
         do j = 1, 256
            exposure_corrected_image(i,j)=0.0
            if(i.le.64.and.j.le.64) then
               binned_exposure_corrected_image(i,j)=0.0
            endif
         end do
      end do

c     if wmapsize is smaller than 256, rebin the
C     detimage and average the exposure map

      if(wmapsize.eq.256) then
         do i = 1, 256
            do j = 1, 256
               exposure_corrected_image(i,j)=0.0
               if(selected_pixels(i,j).gt.-1) then
                  if(exposure_map(i,j).eq.0) then
                     write(contxt,'(a16,i3,a1,i3,a17)')
     $                ' Warning:pixel (',i,',',j,') is not exposed.'
                     call fcecho(contxt)
                     write(contxt,'(a47)')
     $                ' Indefinite value NaN is written for the image.'
                     call fcecho(contxt)
                     exposure_corrected_image(i,j)=indef
                  else
                     exposure_corrected_image(i,j)=REAL(det_image(i,j))
     $                    /exposure_map(i,j)
                  endif
               end if
            end do
         end do
      else
         do i = 1,wmapsize
            do j = 1,wmapsize
               binned_exposure_corrected_image(i,j)=0.0
               binned_det_image(i,j) = 0
               binned_exposure_map(i,j)=0.0
               do k=1,256/wmapsize
                  do l=1,256/wmapsize
                     binned_det_image(i,j) =
     $                    binned_det_image(i,j)+
     $                    det_image((i-1)*(256/wmapsize)+k,
     $                    (j-1)*(256/wmapsize)+l)
                     binned_exposure_map(i,j) =
     $                    binned_exposure_map(i,j)+
     $                    exposure_map((i-1)*(256/wmapsize)+k,
     $                    (j-1)*(256/wmapsize)+l)/(256/wmapsize)**2
                  end do
               end do
               if(selected_pixels(i,j).gt.-1) then
                  if(binned_exposure_map(i,j).eq.0) then
                     write(contxt,'(a16,i3,a1,i3,a17)')
     $        ' Warning:pixel (',i,',',j,') is not exposed.'
                     call fcecho(contxt)
                     write(contxt,'(a47)')
     $        ' Indefinite value NaN is written for the image.'
                     call fcecho(contxt)
                     binned_exposure_corrected_image(i,j)=indef
                  else
                     binned_exposure_corrected_image(i,j)=
     $                    REAL(binned_det_image(i,j))
     $                    /binned_exposure_map(i,j)
                  endif
               end if
            end do
         end do
      endif

      call ftphpr(outunit,.true.,-32,2,naxes,0,1,extend,status)
      call ftpdef(outunit,-32,2,naxes,0,1,status)

      comment='Creator of the file'
      call ftpkys(outunit,'CREATOR',taskname,comment,status)

      comment = 'PI lower bourndary'
      call ftpkyj(outunit,'PI_LOW',pi_low,comment,status)

      comment = 'PI upper bourndary'
      call ftpkyj(outunit,'PI_HIGH',pi_high,comment,status)

      comment ='input blank sky event file list'
      call ftpkys(outunit,'INEVLIST',filelist,comment,status)

      comment ='input spectral file'
      call ftpkys(outunit,'INSPEC',specfile,comment,status)

      comment ='input GTI file'
      call ftpkys(outunit,'INGTI',gtifile,comment,status)

      call ftpcom(outunit,
     $'This is an exposure corrected detector image.', status)
      call ftpcom(outunit,'Unit is counts/pixel/sec.', status)

      call ftpdat(outunit,status)

C     ftp2de had been used until v1.2
c      call ftp2de(outunit,0,256,wmapsize,wmapsize,
c     $     exposure_corrected_image,status)

c     Instead ftppne is used since v1.3, since it
c     has a capability to put the NaN for null values
      if(wmapsize.eq.256) then
         call ftppne(outunit,0,1,wmapsize*wmapsize,
     $        exposure_corrected_image,indef,status)
      else
         call ftppne(outunit,0,1,wmapsize*wmapsize,
     $        binned_exposure_corrected_image,indef,status)
      endif

C     Write the SPECTRUM extension of the output file
      corrected_exposure = 0.0
      do i = 1, 256
         do j = 1, 256
            corrected_exposure=corrected_exposure+exposure_map(i,j)
         end do
      end do

      corrected_exposure=corrected_exposure/
     $     (TOTAL_PIXELS*(256/wmapsize)**2)

      call ftcrhd(outunit,status)
      ttype(1)='CHANNEL'
      tform(1) = 'I'
      ttype(2)='COUNTS'
      tform(2) = 'J'
      tunit(1) = ' '
      tunit(2) = 'count'
      call ftphbn(outunit,detchans,2,ttype,tform,tunit,'SPECTRUM',
     $     0,status)
      comment='exposure time corrected for the source masks'
      call ftpkye(outunit, 'EXPOSURE',corrected_exposure,8,
     $     comment,status)

      call fcecho(' ')
      write(contxt,'(a38,1pe10.4)')
     $     'Total exposure after mask correction: ',
     $     corrected_exposure
      call fcecho(contxt)

      write(contxt,'(a38,i10)')
     $     'Total number of events: ', totcts
      call fcecho(contxt)

c     add necessary spectral keywords
      call add_spec_keywords(outunit,backscal,instrume,detchans,
     $     pi_low,pi_high, status)

C     total counts 
      comment='Total counts in the spectrum'
      call ftpkyj(outunit,'TOTCTS',totcts,comment,status)

C     write the spectral data
      call ftbdef(outunit,2,tform,0,detchans,status)
      if(detchans.eq.128.or.detchans.eq.256) then
         do i = 0, detchans-1
            pispec256(i) = 0
            do j = 0,(1024/detchans)-1
               pispec256(i)=pispec256(i)+pispec((1024/detchans)*i+j)
            end do
         end do
         call ftpclj(outunit,2,1,1,detchans,pispec256,status)
      else
         call ftpclj(outunit,2,1,1,detchans,pispec,status)
      endif
      do i = 1, detchans
         call ftpclj(outunit,1,i,1,1,i-1,status)
      end do

C     Write the DETECTOR image to the image extention
      call ftcrhd(outunit,status)
      call ftphpr(outunit,.true.,32,2,naxes,0,1,.true.,status)
      call ftpcom(outunit,' ',status)
      call ftpcom(outunit,
     $     'Detector image without exposure correction.',status)
      call ftpcom(outunit,'Unit is counts/pixel.',status)
      call ftpdef(outunit,32,2,naxes,0,1,status)
      if(wmapsize.eq.256) then
         call ftp2dj(outunit,0,256,256,256,det_image,status)
      else
         call ftp2dj(outunit,0,64,wmapsize,wmapsize,
     $        binned_det_image,status)
      endif
      
C     Write the exposure map to the image extention
      call ftcrhd(outunit,status)
      call ftphpr(outunit,.true.,-32,2,naxes,0,1,.true.,status)
      call ftpcom(outunit,' ',status)
      call ftpcom(outunit,
     $     'Detector exposure map. Unit is sec/pixel.',status)
      call ftpdef(outunit,-32,2,naxes,0,1,status)
      if(wmapsize.eq.256) then
         call ftp2de(outunit,0,256,256,256,exposure_map,status)
      else
         call ftp2de(outunit,0,64,wmapsize,wmapsize,
     $        binned_exposure_map,status)
      endif

C     close the output file
      call ftclos(outunit,status)

9999  continue
      if(status.ne.0) call fcerrm(status)
      end

      subroutine add_gti(n_in_gti, in_gti_start, in_gti_end,
     $     n_out_gti, out_gti_start, out_gti_end)
      implicit none

C     Take 'AND' of the input GTI and the GTI 
C     specified by the common block /gti_from_users/, and ouput
C     the overlapping GTI.

C     number of input and output GTI intervals
      integer n_in_gti, n_out_gti

C     Input GTI intervals and output GTI intervals 
      double precision in_gti_start(n_in_gti), in_gti_end(n_in_gti),
     $     out_gti_start(*), out_gti_end(*)
      integer i,j,k,l

      common/gti_from_users/gti_start, gti_end
      double precision gti_start(9999), gti_end(9999)

      common/number_of_gti_from_users/n_gti
      integer n_gti

      k = 0
      l = 0
      do i = 1, n_in_gti
         do j = 1, n_gti
            if ( in_gti_start(i).ge.gti_start(j).and.
     $           in_gti_start(i).lt.gti_end(j)) then
               k = k + 1
               out_gti_start(k) = in_gti_start(i)
            end if
            if ( in_gti_end(i)  .gt.gti_start(j).and.
     $           in_gti_end(i)  .le.gti_end(j)) then
               l = l + 1
               out_gti_end(l) = in_gti_end(i)
            endif
         end do
      end do

      do i = 1, n_gti
         do j = 1, n_in_gti
            if ( gti_start(i).gt.in_gti_start(j).and.
     $           gti_start(i).lt.in_gti_end(j)) then
               k = k + 1
               out_gti_start(k) = gti_start(i)         
            endif
            if ( gti_end(i)  .gt.in_gti_start(j).and.
     $           gti_end(i)  .lt.in_gti_end(j)) then
               l = l + 1
               out_gti_end(l) = gti_end(i) 
            endif
         end do
      end do

      n_out_gti = k

      call gti_sort(n_out_gti,out_gti_start)
      call gti_sort(n_out_gti,out_gti_end)
      end

      subroutine gti_sort(n,gti)
      implicit none

      integer n, j
      double precision gti(n)

      double precision buffer
      integer i

      do j = 1, n-1
         do i = j+1, n
            if(gti(i).lt.gti(j)) then
               buffer=gti(j)            
               gti(j) = gti(i)
               gti(i) = buffer
            endif
         end do
      end do
      end

      subroutine add_spec_keywords(unit,backscal,instrume,
     $     detchans,pi_low,pi_high,status)
      implicit none

      integer unit,status,detchans,pi_low,pi_high
      real backscal
      character comment*80, instrume*8

      character(40) taskname
      common /task/ taskname

      character(999)  filelist,specfile,gtifile
      common/infilenames/filelist,specfile,gtifile

      comment='type of channel PHA/PI'
      call ftpkys(unit,'CHANTYPE','PI',comment,status)

      comment='Telescope (mission) name'
      call ftpkys(unit,'TELESCOP','ASCA',comment,status)
      comment='Instrument name'
      call ftpkys(unit,'INSTRUME',instrume,comment,status)
      comment='Filter name'
      call ftpkys(unit,'FILTER','NONE',comment,status)

      comment='point source removed multiple blank sky data'
      call ftpkys(unit,'OBJECT','blank sky',comment,status)

      comment='Creator of the file'
      call ftpkys(unit,'CREATOR',taskname,comment,status)

      comment='Area scaling factor'
      call ftpkye(unit,'AREASCAL',1.0,8,comment,status)

      comment='Correlation scale factor'
      call ftpkye(unit,'CORRSCAL',1.0,8,comment,status)
      comment='BGD scaling factor from the input spec file'
      call ftpkye(unit,'BACKSCAL',backscal,8,comment,status)

      comment='associated background filename'
      call ftpkys(unit,'BACKFILE','none',comment,status)

      comment='associated correction filename'
      call ftpkys(unit,'CORRFILE','none',comment,status)

      comment='associated redistrib matrix filename'
      call ftpkys(unit,'RESPFILE','none',comment,status)

      comment='associated ancillary response filename'
      call ftpkys(unit,'ANCRFILE','none',comment,status)

      comment='no statistical error specified'
      call ftpkyj(unit,'STAT_ERR',0,comment,status)

      comment='no systematic error specified'
      call ftpkyj(unit,'SYS_ERR',0,comment,status)

      comment='no data quality information specified'
      call ftpkyj(unit,'QUALITY',0,comment,status)

      comment='Total No. of Detector Channels available'
      call ftpkyj(unit,'DETCHANS',detchans,comment,status)

      comment='no grouping of the data has been defined'
      call ftpkyj(unit,'GROUPING',0,comment,status)

      comment='Poissonian errors to be assumed'
      call ftpkyl(unit,'POISSERR',.true.,comment,status)

      comment='format conforms to OGIP standard'
      call ftpkys(unit,'HDUCLASS','OGIP',comment,status)

      comment='PHA dataset (OGIP memo OGIP-92-007)'
      call ftpkys(unit,'HDUCLAS1','SPECTRUM',comment,status)

      comment='Version of format (OGIP memo OGIP-92-007a)'
      call ftpkys(unit,'HDUVERS','1.1.0',comment,status)

      comment='Version of format (OGIP memo OGIP-92-007a)'
      call ftpkys(unit,'HDUVERS1','1.1.0',comment,status)

      comment='Background spectral file'
      call ftpkys(unit,'HDUCLAS2','BGD',comment,status)

      comment='PHA data stored as Counts (not count/s)'
      call ftpkys(unit,'HDUCLAS3','COUNT',comment,status)

      comment = 'PI lower bourndary'
      call ftpkyj(unit,'PI_LOW',pi_low,comment,status)

      comment = 'PI upper bourndary'
      call ftpkyj(unit,'PI_HIGH',pi_high,comment,status)

      comment ='input blank sky event file list'
      call ftpkys(unit,'INEVLIST',filelist,comment,status)
      comment ='input spectral file'
      call ftpkys(unit,'INSPEC',specfile,comment,status)
      comment ='input GTI file'
      call ftpkys(unit,'INGTI',gtifile,comment,status)
      call ftpdat(unit,status)

      end


      subroutine  open_gti(unit, gtifile,status)
      implicit none
      integer unit, status
      character*(*) gtifile

      common/gti_from_users/gti_start, gti_end
      double precision gti_start(9999), gti_end(9999)

      common/number_of_gti_from_users/n_gti
      integer n_gti

      integer blocksize, hdutype,i
      character comment*80, extname*8
      logical anyf

c     string length
      integer fcstln,slength
      external fcstln

      call fcecho('GTI file used')
      slength=fcstln(gtifile)
      call fcecho(gtifile(:slength))

      call ftopen(unit,gtifile,0,blocksize,status)
      if(status.ne.0) go to 999

 5    continue
      call ftmrhd(unit,1,hdutype,status)
      call ftgkys(unit,'EXTNAME',extname,comment,status)
      if (status.ne.0) go to 999
      if(index(extname,'GTI').gt.0) then 
         call ftgkyj(unit,'NAXIS2',n_gti,comment,status)
         do i = 1, n_gti
            call ftgcvd(unit,1,i,1,1,0.0d0,gti_start(i),anyf,status)
            call ftgcvd(unit,2,i,1,1,0.0d0,gti_end(i),  anyf,status)
         end do
      else
         go to 5
      end if
      call ftclos(unit,status)

      return

 999  continue
      call fcerr('error in open_gti')
      return
      end

