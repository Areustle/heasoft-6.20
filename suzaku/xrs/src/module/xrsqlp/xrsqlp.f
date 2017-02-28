C     xrsqlp
c     version 2.0 by Ken Ebisawa (ebisawa@subaru.gsfc.nasa.gov)
C     2004-05-14.
C     Originally written for ASTRO-E1 xrssim output file
C     and real XRS event files.  Almost completely rewritten for
C     ASTRO-E2.
c
c     version 2.1 by Ken Ebisawa (ebisawa@subaru.gsfc.nasa.gov)
C     2004-05-15 Check presence of the teldef file to avoid
C     segmentation fault error.
c
c     version 2.2
C     2004-05-21 Sky binning algorithm is modified so that it
C     does not break when X or Y=1.
C     Does not write wrong coordinate label nor draw sky grids on the sky image
c     when North or South pole is included (pgtbox failes).
c
c     version 2.3
C     2004-05-28
c     Remove an additional commna in call pgtext(xpos,ypos,'Mid-res events:')
C     Ingore Pixel 3 completely
c
c     version 2.4
C     2004-07-01
c     Diplay High+Mid-prinmry in the third figure (it used to be
C     (High+Mid-primary+Mid-secondary).
C     Change display event types in the fourth figure
c     version 2.3 and before   ---->      version 2.4
c     High                                High
C     Mid                                 Mid-primary
C     Low                                 Mid-secondary
C     Mid-secondary                       Low-primary
C     Low_secoondary                      Low-secondary
C
c     version 2.5
C     2005-02-17
C     When a real XRS event file, not a simulated file, is specified,
C     contour map is not drawn.
C
c     version 2.6
C     2005-02-19
C     A new parameter "outfits" is introuduce to control if
C     FITS image is output (outfits=.true.) or not (outfits=.false.).
C     A bug in fillXRSimage was fixed so that only sky pixels
C     covered by the XRS pixel is filled.

c     version 2.7
C     2005-03-09
C     Modification for HEADAS style.

c     version 2.8
C     2005-05-21
C     A better algorithm to parse the Euler angle from
C     HISTORY keyword of the realistic event file.
C     In ftgcvd, nullval was defined as integer, but should
C     have been double.  Introduced dblenullval.  (Then unaligned
C     error on Alpha/OSF disappeared.)

c     version 2.9, Y.ISHISAKI
C     2005-07-06
C     read MEAN_EA1/2/3 for the Euler angles

c     version 3.0, K.Ebisawa
C     2005-07-18
C     Core dump bug on Linux ppc fixed, at the line,
C     'Pixel_center_focx(i)=0.0', which should have been
C     'Pixel_center_focy(i)=0.0'
C     Version is explicitly dispalyed when started.

c     version 3.1, Y.ISHISAKI
C     2006-06-07
C     check whether SmoothWidth is odd, in xrsqlp()
C     use 'smoothbin = (SmoothWidth-1)/2' in smooth(), plot_contour() for g95

      integer function xrsqlp()
      implicit none

      real Pixel_corner_focx(4,0:31), Pixel_corner_focy(4,0:31)
      real Pixel_center_focx(0:31), Pixel_center_focy(0:31)
      real Pixel_corner_rel_focx(4,0:31), Pixel_corner_rel_focy(4,0:31)

      real Pixel_corner_skyx(4), Pixel_corner_skyy(4)
      integer status, unit, blocksize,hdutype,i
      character*80 comment
      logical anyf
      real BGred, BGgreen, BGblue, red, green, blue
      integer xmin, ymin, xmax, ymax

      double precision xrval, yrval, xrpix, yrpix, xinc, yinc, rot,
     $     ra_max, dec_max, ra_min, dec_min
      character coordtype*8
      integer skyx_col, skyy_col, roll_col, pixel_col,
     $        detx_col, dety_col, flag_lowres_col, flag_midres_col,
     $     flag_secondary_col
      integer naxis2,j
      integer nullval, PIXEL
      double precision dblenullval
      integer flag_lowres, flag_midres, flag_secondary
      double precision X, Y, ROLL
      double precision ROLL_rad, PI
      real skyimage(1536,1536), skymin, skymax
      real XRSimageFITS(1536,1536)
      real  PIXEL_VALUES(0:31), PIXEL_HM_VALUES(0:31),
     $     PIXEL_HM_pri_VALUES(0:31),
     $     PIXEL_L_VALUES(0:31), PIXEL_H_VALUES(0:31),
     $     PIXEL_M_VALUES(0:31), PIXEL_low_Secondary(0:31),
     $     PIXEL_mid_Secondary(0:31),
     $     PIXEL_M_Primary(0:31), PIXEL_L_Primary(0:31)
      integer ncont
      real C(16), TR(6)
      real XRSimageMin, XRSimageMax
      integer ImageBinning, binnedX, binnedY
      character*256 text
      real sky_pixel_scale
      integer SmoothWidth
      character*1024 infile, outputFITS
C     DETXY and FOCXY pixelSize = 0.02274mm,
C     which is the same as XIS pixel size
C     At folcal plane 1mm=0.763943727 arcmin
C     Hence, 1 pixel = 0.01737208 arcmin
c      real pixel2mm/0.02274/
      real pixel2arcmin
      double precision Euler(3)
      real detimage(256, 256)
      integer DETX, DETY
      integer nevents, nlowevents
      real xpos, ypos
      integer low_secondarysum, mid_secondarysum, pixelsum,midsum,
     $     lowsum, highsum
      real pixel_x(0:31),pixel_y(0:31)
      character gdevice*8
      logical page
      real tmp1, tmp2,minvalue, maxvalue
      logical logscale, clobber_file, exists,outfits
      integer naxes(2)
      character teldef_file*256, creator*80
      logical simfile, coordinate_grids
      character*80  errm, history
      integer PILGETSTRING, PILGETFNAME, PILGETINT
      integer PILGETBOOL
      character version*5

      data status/0/
      data nullval/999/
      data dblenullval/999.0/
      data PI/3.141592654D0/
      data XRSimageMin/999./, XRSimageMax/-999./
      data pixel2arcmin/0.01737208/

      xrsqlp = 0

      call hdnameset('xrsqlp')
      version = '3.0'
      call hdverset(version)

      write(text,'(a,a)') 'xrsqlp version ',version
      call hdecho(text)

CCC   Parameters setting
      status = PILGETFNAME('infile',infile)
      if (status .ne. 0) call geterror('Error in getting infile',xrsqlp)
      status = PILGETSTRING('gdevice',gdevice)
      if (status .ne. 0)
     $     call geterror('Error in getting gdevice',xrsqlp)
      status = PILGETBOOL('page',page)
      if (status .ne. 0)
     $     call geterror('Error in getting page',xrsqlp)
      status = PILGETINT('ImageBinning',ImageBinning)
      if (status .ne. 0)
     $     call geterror('Error in getting ImageBinning',xrsqlp)
      status = PILGETINT('SmoothWidth',SmoothWidth)
      if (status .ne. 0)
     $     call geterror('Error in Getting SmoothWidth',xrsqlp)
      if ( SmoothWidth.le.0 .or. 0.eq.Mod(SmoothWidth,2) ) then
         status = -1
         call geterror('Error in SmoothWidth, must be odd number',
     $        xrsqlp)
      endif
      status = PILGETBOOL('logscale',logscale)
      if (status .ne. 0)
     $     call geterror('Error in getting logscale',xrsqlp)
      status = PILGETINT('ncont',ncont)
      if (status .ne. 0)
     $     call geterror('Error in getting ncont',xrsqlp)
      status = PILGETSTRING('teldef_file',teldef_file)
      if (status .ne. 0)
     $     call geterror('Error in getting teldef_file',xrsqlp)
      status = PILGETBOOL('outfits',outfits)
      if (status .ne. 0)
     $     call geterror('Error in getting outfits',xrsqlp)

      if (status.ne.0) go to 999

      if(outfits) then
         status = PILGETFNAME('outputFITS',outputFITS)
         status = PILGETBOOL('clobber',clobber_file)
         inquire(file=outputFITS,exist=exists)
         if(exists) then
            if(clobber_file) then
               text =
     $ 'Output file exists and will be overwritten since clobber=yes'
               call hdecho(text)
               if (outputFITS(1:1) .ne. '!') then
                  outputFITS='!'//outputFITS
               endif
            else
               text =
     $ 'Output file exists and will NOT be overwritten since clobber=no'
               call hderr(text)
               go to 999
            endif
         endif
      endif

CCC   End of setting parameters

C     Readin the teldef file, get the XRS pixel corner positions
C     in the FOC coordinates
      call read_teldef(teldef_file,Pixel_corner_focx, Pixel_corner_focy,
     $     status)
      if(status.ne.0) go to 999

      do i = 0, 31
         Pixel_center_focx(i)=0.0
c The following line was a bug up to version 2.9
c     Pixel_center_focx(i)=0.0
         Pixel_center_focy(i)=0.0
         do j = 1, 4
            Pixel_center_focx(i)=Pixel_center_focx(i)+
     $           pixel_corner_focx(j,i)
            Pixel_center_focy(i)=Pixel_center_focy(i)+
     $           pixel_corner_focy(j,i)
         end do
         Pixel_center_focx(i)=Pixel_center_focx(i)/4.0
         Pixel_center_focy(i)=Pixel_center_focy(i)/4.0
         do j = 1, 4
            pixel_corner_rel_focx(j,i)=pixel_corner_focx(j,i)
     $           -Pixel_center_focx(i)
            pixel_corner_rel_focy(j,i)=pixel_corner_focy(j,i)
     $           -Pixel_center_focy(i)
         end do
      end do

C     Open the event file
      call ftgiou(unit,status)

      call ftopen(unit,infile,0,blocksize,status)
      if(status.ne.0) then
         call hdecho('Error to open the input event file')
         go to 999
      endif

      call ftmahd(unit,2,hdutype,status)

C     Read the CREATOR, and judge if this file is made with xrssim
C     or, real XRS event file.
      call ftgkys(unit,'CREATOR',creator, comment,status)
      write(text,'(a,a)') 'Creator = ', creator
      call hdecho(text)
      if(index(creator,'xrssim').gt.0) then
         write(text,'(a)') 'This is a simulatied event file.'
         call hdecho(text)
         simfile = .true.
      else
         write(text,'(a)') 'This is a real XRS event file.'
         call hdecho(text)
         simfile = .false.
      endif

C     Get necessary column numbers
      call ftgcno(unit, .false., 'X',skyx_col,status)
      call ftgcno(unit, .false., 'Y',skyy_col,status)
      call ftgcno(unit, .false., 'DETX',detx_col,status)
      call ftgcno(unit, .false., 'DETY',dety_col,status)
      call ftgcno(unit, .false., 'ROLL',roll_col,status)
      call ftgcno(unit, .false., 'PIXEL',pixel_col,status)
      call ftgcno(unit, .false., 'FLAG_LOWRES',flag_lowres_col,status)
      call ftgcno(unit, .false., 'FLAG_MIDRES',flag_midres_col,status)
      call ftgcno(unit, .false.,'FLAG_SECONDARY',
     $     flag_secondary_col,status)

C     Get WCS keywords from the event file header
      call FTGTCS(unit,skyx_col,skyy_col,
     $     xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coordtype,status)

C     Start the PGPLOT, determine the number of vieports
C     in a single page, and the first viewport size
      if(page) then
         call pgbegin(0,gdevice,2,2)
      else
         call pgbegin(0,gdevice,1,1)
      endif

      call pgpage
      call pgvport(0.1,0.9,0.15,0.88)

C     Sky pixel range (after the binning) to be plotted
C     This corresponds to the central ~4.5 min x ~4.5 min
C     region covering the entire XRS .
      xmin = int(128.0/ImageBinning*5.0+1.0)
      xmax = int(128.0/ImageBinning*7.0)
      ymin=xmin
      ymax=xmax
      call pgwnad(REAL(xmin)-0.5,REAL(xmax)-0.5,
     $     REAL(ymin)+0.5,REAL(ymax)+0.5)

C     write labels
      call pgmtxt('t',4.,0.5,0.5,infile)
      call pgmtxt('b',2.2,0.5,0.5,'Sky Coordinate X (pixel)')
      call pgmtxt('l',2.5,0.5,0.5,'Sky Coordinate Y (pixel)')

C     write labels
      call pgsch(0.7)
      call pgmtxt('bv', 6.4,1.02,0.0,'Sky pixel size:')
      sky_pixel_scale = pixel2arcmin*ImageBinning
      write(text,'(f6.4,a)') sky_pixel_scale, ' arcmin'
      call pgmtxt('bv', 7.6,1.02,0.0,text)
      call pgsch(1.0)

C     OK, let's leave pgplot for now, and start reading
C     the event file.

C     Get the number of events from the event file header
      call ftgkyj(unit,'NAXIS2',naxis2,comment,status)

C     Get Euler angles
      if(simfile) then
C     In the case of simfile. The third euler angle is
C     90.0-roll.  Roll is taken from the first event.
         call ftgkyd(unit,'TCRVL7',  euler(1),comment,status)
         call ftgkyd(unit,'TCRVL8',  euler(2),comment,status)
         euler(2) = 90.0-euler(2)
         call ftgcvd(unit,9,1,1,1,dblenullval,euler(3),anyf,
     $        status)
         euler(3) = 90 - euler(3)
      else

         status = 0
         call ftgkyd(unit,'MEAN_EA1', euler(1), comment, status)
         call ftgkyd(unit,'MEAN_EA2', euler(2), comment, status)
         call ftgkyd(unit,'MEAN_EA3', euler(3), comment, status)
         if ( 0.eq.status ) then
            call hdecho('FITS keywords for mean euler angles found.')
            write(text,'(a,f9.3,a,f9.3,a,f9.3,a)')
     $           '(',euler(1),',',euler(2),',',euler(3),')'
            call hdecho(text)
            coordinate_grids=.true.
            goto 99
         endif

C     In the case of real event file.
C     Try to find the mean Euler angle from the History records
C     expected like following:
C     HISTORY XRSpixelToXY version 1.4
C     HISTORY   teldef='xrs_teldef_2003-02-15a.fits'
C     HISTORY   attitude='ae20041216_0033_0424.att'
C     HISTORY     (mean)=(351.0707, 30.9212,-141.9003)
C     HISTORY   skyref=(351.0707, 59.0788, 0.0)

         status = 0
         do while (status.eq.0)
            call FTGCRD(unit,'HISTORY*', history, status)
            if(index(history,'mean').gt.0) then
               call geteuler(history,euler)
               call hdecho('Mean euler angles found.')
               write(text,'(a,f9.3,a,f9.3,a,f9.3,a)')
     $              '(',euler(1),',',euler(2),',',euler(3),')'
               call hdecho(text)
               coordinate_grids=.true.
               go to 99
            end if
         end do
         call hdecho('Mean euler angles not found.')
         call hdecho('Coordinage grids are not written for DET images.')
         coordinate_grids=.false.
         status = 0
      endif
 99   continue

C     Initialize array
      do i =1, 1536
         do j=1,1536
            skyimage(i,j)=0.0
            XRSimageFITS(i,j)=0.0
            if(i.le.256.and.j.le.256) then
               detimage(i,j)=0.0
            endif
         end do
         if(i.le.33) then
            PIXEL_VALUES(i-1)=0.0
            PIXEL_HM_VALUES(i-1)=0.0
            PIXEL_H_VALUES(i-1)=0.0
            PIXEL_M_VALUES(i-1)=0.0
            PIXEL_L_VALUES(i-1)=0.0
            PIXEL_Low_secondary(i-1)=0.0
            PIXEL_Mid_secondary(i-1)=0.0
         endif
      end do

C     Read event file
      skymin=1e10
      skymax=-1e10
      nevents = 0
      nlowevents = 0
      do i =1, naxis2
         call ftgcvd(unit,skyx_col, i,1,1,dblenullval,X,anyf, status)
         call ftgcvd(unit,skyy_col, i,1,1,dblenullval,Y,anyf, status)
         if(X.ge.1.and.X.le.1536.and.y.ge.1.and.Y.le.1536) then
C     This is not correct!
c     binnedX=INT(X/ImageBinning+0.5)
c     binnedY=INT(Y/ImageBinning+0.5)
C     Modified on 2004-05-21
            binnedX=INT((X-0.5)/ImageBinning+1.0)
            binnedY=INT((Y-0.5)/ImageBinning+1.0)
            skyimage(binnedX, binnedY)=skyimage(binnedX, binnedY)+1.0
            skymin=min(skymin,skyimage(binnedX,binnedY))
            skymax=max(skymax,skyimage(binnedX,binnedY))
            call ftgcvj(unit,detx_col, i,1,1,nullval,DETX,anyf, status)
            call ftgcvj(unit,dety_col, i,1,1,nullval,DETY,anyf, status)
            if(DETX.ge.1.and.DETX.le.256.and.DETY.ge.1.and.DETY.le.256)
     $           then
               DETIMAGE(DETX, DETY)=DETIMAGE(DETX, DETY)+1.0
            endif

            call ftgcvd(unit,roll_col,i,1,1,
     $           dblenullval,ROLL,anyf,status)
            call ftgcvj(unit,pixel_col,i,1,1,nullval,PIXEL,anyf, status)
            call ftgcvj(unit,flag_lowres_col,i,1,1,nullval,FLAG_LOWRES,
     $           anyf, status)
            call ftgcvj(unit,flag_midres_col,i,1,1,nullval,FLAG_MIDRES,
     $           anyf, status)
            call ftgcvj(unit,flag_secondary_col,i,1,1,nullval,
     $           FLAG_SECONDARY, anyf, status)

            ROLL_rad = ROLL/180.0D0*PI
            if(PIXEL.ge.0.and.PIXEL.le.31) then
               PIXEL_VALUES(PIXEL)= PIXEL_VALUES(PIXEL)+1.0
               if(flag_lowres.ne.0) then
                  PIXEL_L_VALUES(PIXEL)= PIXEL_L_VALUES(PIXEL)+1.0
                  if(flag_secondary.ne.0) then
                     pixel_low_secondary(pixel)=
     $                    pixel_low_secondary(pixel)+1
                  endif
               endif
               if(flag_midres.ne.0) then
                  PIXEL_M_VALUES(PIXEL)= PIXEL_M_VALUES(PIXEL)+1.0
                  if(flag_secondary.ne.0) then
                     pixel_mid_secondary(pixel)=
     $                    pixel_mid_secondary(pixel)+1
                  endif
               endif

C     For each event, we need to determine the corner positions of this
C     pixel in the sky coordinates.  For the simulated file,
C     FOC to SKY is just rotation on the image plane
               do j = 1, 4
                  if(simfile) then
                     Pixel_corner_skyx(j) = (cos(ROLL_rad)*
     $                    Pixel_corner_focx(j, pixel)
     $                    -sin(ROLL_rad)*Pixel_corner_focy(j, pixel))
     $                    /ImageBinning+(1.0D0+1536/ImageBinning)/2.0D0
                     Pixel_corner_skyy(j) = (sin(ROLL_rad)*
     $                    Pixel_corner_focx(j, pixel)
     $                    +cos(ROLL_rad)*Pixel_corner_focy(j, pixel))
     $                    /ImageBinning+(1.0D0+1536/ImageBinning)/2.0D0
                  else
                     Pixel_corner_skyx(j)=(X-768.5+cos(roll_rad)*
     $                    Pixel_corner_rel_focx(j,pixel)
     $                    -sin(roll_rad)*Pixel_corner_rel_focy(j,pixel))
     $                    /ImageBinning+(1.0D0+1536/ImageBinning)/2.0D0
                     Pixel_corner_skyy(j)=(Y-768.5+sin(roll_rad)*
     $                    Pixel_corner_rel_focx(j,pixel)
     $                    +cos(roll_rad)*Pixel_corner_rel_focy(j,pixel))
     $                    /ImageBinning+(1.0D0+1536/ImageBinning)/2.0D0
                  endif
               enddo

               if(PIXEL.ne.3.and.PIXEL.ne.2) then
                  call fillXRSimage(ImageBinning, XRSimageFITS,
     $                Pixel_corner_skyx, Pixel_corner_skyy, XRSimageMin,
     $                 XRSimageMax)
                  nevents = nevents + 1
                  if(flag_lowres.ne.0) then
                     nlowevents = nlowevents + 1
                  endif
               endif

C     End of if(PIXEL.ge.0.and.PIXEL.le.31)
            endif
C     End of if(X.ge.1.and.X.le.1536.and.y.ge.1.and.Y.le.1536)
         endif

         if(mod(i,10000).eq.1) then
            write(text,'(a,i7)') 'Number of events read : ', i
            call hdecho(text)
         endif

      end do
c     close the input event file
      call ftclos(unit,status)

      if(outfits) then
C     Output the XRS image FITS file
         call ftgiou(unit,status)
         blocksize=1
         call ftinit(unit,outputFITS,blocksize,status)
         naxes(1)=1536/ImageBinning
         naxes(2)=1536/ImageBinning
C     write the required header keywords
         call ftphpr(unit,.true.,-32,2,naxes,0,1,.false.,status)
         call FTP2DE(unit,0,1536,naxes(1),naxes(2),XRSimageFITS, status)

C     Write necessary WCS keywords and other keywords.
         call ftpkys(unit, 'CREATOR', 'xrsqlp','Creator of the file',
     $        status)
         call ftpkys(unit, 'INPUT', infile,'xrsqlp input file', status)
         call ftpkyj(unit,'BINNING',ImageBinning,
     $        'Sky image binning factor',status)
         call ftpkys(unit,'CTYPE1', 'RA---TAN', 'Projection method',
     $        status)
         call ftpkys(unit,'CTYPE2', 'DEC--TAN', 'Projection method',
     $        status)
         call ftpkyd(unit,'CRPIX1',(1.0D0+1536.0D0/ImageBinning)/2.0D0,
     $        9, 'X center Pixel', status)
         call ftpkyd(unit,'CRPIX2',(1.0D0+1536.0D0/ImageBinning)/2.0D0,
     $        9, 'Y center Pixel', status)
         call ftpkyd(unit,'CRVAL1', xrval, 9, 'Center Right Ascension',
     $        status)
         call ftpkyd(unit,'CRVAL2', yrval, 9, 'Center Declination',
     $        status)
         call ftpkyd(unit,'CDELT1', xinc*ImageBinning, 9,
     $        'X increment (deg/pixel)', status)
         call ftpkyd(unit,'CDELT2', yinc*ImageBinning, 9,
     $        'Y increment (deg/pixel)', status)
         call ftpkys(unit,'CUNIT1', 'deg', 'X in degree', status)
         call ftpkys(unit,'CUNIT2', 'deg', 'Y in degree', status)
         call FTPDAT(unit,status)
C     OK, we have closed the output image file.
         call ftclos(unit,status)
      endif

C     Let's get back to the plot.

C     Write the number of events etc at the lower-left corner
c     of the diagram
      call write_nevents(nevents, ImageBinning, Smoothwidth)

C     Draw XRS pixel image

C     color index zero is the background color
      call pgqcr(0,BGred,BGgreen,BGblue)
      do i = 0,31
         if(bgred.eq.0) then
            red   =  1./31.*i
            green =  i*(31-i)/15./16.0
            blue  =  0.
         else
            red   =   1.0-i*(31.-i)/15./16.0
            green =   (31-i)/31.
            blue  =   1.
         endif
         call pgscr(16+i,red,green,blue)
      end do
      call pgscir(16,47)

C     set image transfer function:0=liner,1=log,2=square
      call pgsitf(0)

      TR(1)= 0.0
      TR(2)= 1.0
      TR(3)= 0.0
      TR(4)= 0.0
      TR(5)= 0.0
      TR(6)= 1.0

      call pgimag(XRSimageFITS,
     $        1536,1536,xmin,xmax,ymin,ymax,XRSimageMin,XRSimageMax,TR)

C     Draw the wedge
      call pgwedg('bi',2.9,3.,XRSimageMin,XRSimageMax,
     $     'counts/sky-pixel')

C     Smooth the sky image
      call smooth(skyimage,1536,ImageBinning,SmoothWidth,skymin,skymax)

      if(simfile) then
C     Draw contour of the input sky image
C     draw the contour in grey
         call pgscr(48,0.5,0.5,0.5)
         call pgsci(48)
         do i = 1, ncont
            if(logscale) then
               tmp1 = log10(skymin)
               tmp2 = log10(skymax)
               c(i) = 10**(tmp1+(tmp2-tmp1)/(ncont-1)*(i-1))
            else
               c(i) = skymin+(skymax-skymin)/(ncont-1)*(i-1)
            endif
         end do
         call pgcont(skyimage,1536,1536,xmin,xmax,ymin,ymax,C,-ncont,TR)
         call pgsci(1)

C     Plot the contour level
         call pgsch(0.6)
         do i=1, ncont
            write(text,'(f10.2)') c(i)
            call pgmtxt('Rv',4.5,1.0+0.05*(i-ncont),0.0,text)
         end do
         write(text,'(a)') 'Contour levels'
         call pgmtxt('Rv',1.2,1.1,0.0,text)
         write(text,'(a,f6.4,a)') 'counts/(',sky_pixel_scale,
     $        'arcmin)\\u2\\d'
         call pgmtxt('Rv',1., 1.05,0.0,text)
         call pgsch(1.0)
      endif

C     Drwa box in pixel scale in bottom and left
      call pgbox('bnst',0.0,0,'bnst',0.0,0)

C     Draw the box in RA and DEC units
C     When unbinned, the lower-left and upper-right corner
C     of the viewport corresonds to (X,Y)=(640.5,640.5) and
C     (896.5, 896.5), respectively.
      call FTWLDP(640.5D0,640.5D0,xrval,yrval,
     $        xrpix,yrpix,xinc,yinc,rot, coordtype, ra_max,dec_min,
     $        status)
      call FTWLDP(896.5D0,896.5D0,xrval,yrval,
     $        xrpix,yrpix,xinc,yinc,rot,
     $        coordtype, ra_min,dec_max,status)
C     Now the coordinate is in RA and DEC
      if(abs(dec_min-dec_max).lt.0.001) then
         call hdecho('### Warning ###')
         call hdecho
     $  ('It seems like the sky image includes North or South pole.')
         call hdecho('Coordinate grids are not drawn.')
         call pgbox('cmst',0.0,0,'cmst',0.0,0)
         call pgmtxt('t',2.5,0.5,0.5,'Sky Coordinate X (pixel)')
         call pgmtxt('r',3.0,0.5,0.5,'Sky Coordinate Y (pixel)')
      else
         call pgswin(REAL(ra_max)*240.,REAL(ra_min)*240.,
     $        REAL(dec_min)*3600.,REAL(dec_max)*3600.)
         call pgmtxt('t',2.5,0.5,0.5,'Right Ascension (J2000)')
         call pgmtxt('r',3.0,0.5,0.5,'Declination (J2000)')
C     RA and DEC in top an right
         call pgtbox('czhmst',0.0,0,'czdmst',0.0,0)

C     draw RA and DEC grids with dotted line
         call pgsls(4)
         call pgtbox('zg',0.0,0,'zg',0.0,0)
         call pgsls(1)
      endif


C     OK, now we are going to make the second and third
C     diagrams for detector image
C     First, smooth the detector image
      call smooth(detimage,256, 1,
     $     SmoothWidth*ImageBinning,minvalue,maxvalue)

C     DET image with Total Events (second plot)
      call PlotDetImage(Pixel_Values,Pixel_corner_focx,
     $     Pixel_corner_focy, Euler, detimage,Smoothwidth*ImageBinning,
     $     ncont,minvalue,maxvalue,logscale,simfile, coordinate_grids)
      call pgmtxt('t',3.0,0.5,0.5,'All events (High+Mid+Low)')
      call write_nevents(nevents,1,smoothwidth*imagebinning)

C     DET image only with High and Mid events (third plot)
      pixelsum = 0
      highsum = 0
      midsum = 0
      lowsum = 0
      low_secondarysum = 0
      mid_secondarysum = 0
      do i=0,31
         if(i.ne.2.and.i.ne.3) then
            PIXEL_hm_Values(i) = Pixel_Values(i)-
     $           Pixel_L_values(i)
            PIXEL_h_Values(i)  = Pixel_hm_Values(i)-Pixel_m_values(i)
            pixel_hm_pri_values(i) =
     $           PIXEL_hm_Values(i)-pixel_mid_secondary(i)
            pixelsum = pixelsum + pixel_values(i)
            highsum = highsum + pixel_h_values(i)
            midsum = midsum + pixel_m_values(i)
            lowsum = lowsum + pixel_l_values(i)
            mid_secondarysum = mid_secondarysum + pixel_mid_secondary(i)
            low_secondarysum = low_secondarysum + pixel_low_secondary(i)
         endif
      end do
      call PlotDetImage(Pixel_HM_pri_Values,Pixel_corner_focx,
     $     Pixel_corner_focy, Euler, detimage,Smoothwidth*ImageBinning,
     $     ncont,minvalue, maxvalue,logscale,simfile,coordinate_grids)
      call pgmtxt('t',3.0,0.5,0.5,'Only High+Mid-primary events')
      call write_nevents(nevents-nlowevents-mid_secondarysum,
     $     1,smoothwidth*imagebinning)

C     Pixel summary (fourth plot)
      CALL PGPAGE
      CALL PGVport(0.1,0.89,0.05,0.92)
      CALL PGSWIN(0.0,6.0,0.0,6.0)
C     Draw pixel boundaries
      call pixel_boundaries
      call pixel2position(pixel_x,pixel_y)

      do i = 0, 31
         Pixel_M_primary(i)= Pixel_M_values(i)-Pixel_mid_secondary(i)
         Pixel_L_primary(i)= Pixel_L_values(i)-Pixel_low_secondary(i)
         if(i.ne.2.and.i.ne.3) then
            write(text,'(a,i2)') 'Pixel ',i
            call pgsci(2)
            call pgsch(0.8)
            call pgtext(pixel_x(i)+0.02,pixel_y(i)+0.02,text)
            call pgsci(4)
            call pgtext(pixel_x(i)+0.02,pixel_y(i)+0.88,'Total:')
            write(text,'(i8)') int(Pixel_values(i))
            call pgtext(pixel_x(i)+0.26,pixel_y(i)+0.88,text)
            call pgsch(0.6)
            call pgsci(1)
            call pgtext(pixel_x(i)+0.02,pixel_y(i)+0.75,'H-res:')
            write(text,'(i7,a,f5.1,a)')  int(Pixel_H_values(i)),
     $           ' (',100.*(PIXEL_H_VALUES(i))/Pixel_values(i),'%)'
            call pgtext(pixel_x(i)+0.26,pixel_y(i)+0.75,text)

            call pgtext(pixel_x(i)+0.02,pixel_y(i)+0.63,'M-pri:')
            write(text,'(i7,a,f5.1,a)') int(Pixel_M_primary(i)),
     $           ' (',100.*(PIXEL_M_primary(i))/Pixel_values(i),'%)'
            call pgtext(pixel_x(i)+0.26,pixel_y(i)+0.63,text)

            call pgtext(pixel_x(i)+0.02,pixel_y(i)+0.45,
     $           'M-sec:')
            write(text,'(i7,a,f5.1,a)') int(Pixel_mid_secondary(i)),
     $           ' (',100.*(Pixel_mid_secondary(i))/Pixel_values(i),'%)'
            call pgtext(pixel_x(i)+0.26,pixel_y(i)+0.45,text)

            call pgtext(pixel_x(i)+0.02,pixel_y(i)+0.27,'L-pri:')
            write(text,'(i7,a,f5.1,a)') int(Pixel_L_primary(i)),
     $           ' (',100.*(PIXEL_L_primary(i))/Pixel_values(i),'%)'
            call pgtext(pixel_x(i)+0.26,pixel_y(i)+0.27,text)

            call pgtext(pixel_x(i)+0.02,pixel_y(i)+0.17,
     $           'L-sec:')
            write(text,'(i7,a,f5.1,a)') int(Pixel_low_secondary(i)),
     $           ' (',100.*(Pixel_low_secondary(i))/Pixel_values(i),'%)'
            call pgtext(pixel_x(i)+0.26,pixel_y(i)+0.17,text)

         endif
      end do
      xpos=4.2
      ypos=4.8
      call pgtext(xpos,ypos,'Total events:')
      write(text,'(i9)') pixelsum
      call pgtext(xpos+0.6,ypos,text)

      ypos=ypos-0.15
      call pgtext(xpos,ypos,'High-res events:')
      write(text,'(i9,a,f5.1,a)')
     $     highsum,' (',100.*highsum/pixelsum,'%)'
      call pgtext(xpos+0.6,ypos,text)

      ypos=ypos-0.1
      call pgtext(xpos,ypos,'Mid-primary:')
      write(text,'(i9,a,f5.1,a)')
     $     midsum-mid_secondarysum,
     $     ' (',100.*(midsum-mid_secondarysum)/pixelsum,'%)'
      call pgtext(xpos+0.6,ypos,text)

      ypos=ypos-0.15
      call pgtext(xpos,ypos,'Mid-secondary: ')
      write(text,'(i9,a,f5.1,a)')
     $     mid_secondarysum,' (',
     $     100.*mid_secondarysum/pixelsum,'%)'
      call pgtext(xpos+0.6,ypos,text)

      ypos=ypos-0.15
      call pgtext(xpos,ypos,'Low-primary:')
      write(text,'(i9,a,f5.1,a)')lowsum-low_secondarysum,
     $     ' (',100.*(lowsum-low_secondarysum)/pixelsum,'%)'
      call pgtext(xpos+0.6,ypos,text)

      ypos=ypos-0.1
      call pgtext(xpos,ypos,'Low-secondary:')
      write(text,'(i9,a,f5.1,a)')
     $     low_secondarysum,' (',
     $     100.*low_secondarysum/pixelsum,
     $     '%)'
      call pgtext(xpos+0.6,ypos,text)

      call pgsch(1.0)

C     End pgplot
      call pgend

C     End program
      write(text,'(a,i7)') 'xrsqlt plot finishing status: ', status
      call hdecho(text)
 999  continue
      if(status.ne.0) then
        call ftgerr(status, errm)
        call hderr(errm)
      endif
      end

      subroutine read_teldef(teldef,Pixel_corner_focx,Pixel_corner_focy,
     $     status)
      implicit none
      character teldef*(*)
      real Pixel_corner_focx(4,0:31), Pixel_corner_focy(4,0:31)
      real PixelX(4,0:31), PixelY(4,0:31)
      integer unit, status, blocksize
      real DET_XSCL, DET_YSCL, INT_XCEN, INT_YCEN, DET_XOFF, DET_YOFF
      integer DETXFLIP, DETYFLIP, hdutype, i, Pixel,j
      character*80 comment
      logical anyf

C     Open the fits file
      call ftgiou(unit,status)
      call ftopen(unit,teldef,0, blocksize,status)

C     Get necessary keywords from primary
      call ftgkye(unit,'DET_XSCL', DET_XSCL, comment,status)
      call ftgkye(unit,'DET_YSCL', DET_YSCL, comment,status)
      call ftgkye(unit,'INT_XCEN', INT_XCEN, comment,status)
      call ftgkye(unit,'INT_YCEN', INT_YCEN, comment,status)
      call ftgkye(unit,'DET_XOFF', DET_XOFF, comment,status)
      call ftgkye(unit,'DET_YOFF', DET_YOFF, comment,status)

      call ftgkyj(unit,'DETXFLIP', DETXFLIP, comment,status)
      call ftgkyj(unit,'DETYFLIP', DETYFLIP, comment,status)

C     Get Pixel corner positions from 1-st extension
      call ftmahd(unit,2,hdutype,status)
      do i = 1, 32
         call ftgcvj(unit,1,i,1,1,0,Pixel,anyf, status)
         call ftgcve(unit,2,i,1,4,0,PixelX(1,Pixel),anyf, status)
         call ftgcve(unit,3,i,1,4,0,PixelY(1,Pixel),anyf, status)
      end do

      do i = 0, 31
         do j = 1, 4
C     Pixel_corner_focxy are given in pixel (center of FOC image is 0,0)
            Pixel_corner_focx(j, i) =
     $           DETXFLIP*(PixelX(j,i)-INT_XCEN-DET_XOFF)/DET_XSCL
            Pixel_corner_focy(j, i) =
     $           DETYFLIP*(PixelY(j,i)-INT_YCEN-DET_YOFF)/DET_YSCL
         end do
      end do
      call ftclos(unit,status)
      end

      subroutine fillXRSimage(ImageBinning, XRSimage,Pixel_corner_skyx,
     $     Pixel_corner_skyy, XRSimageMin, XRSimageMax)
      implicit none
      integer ImageBinning
      real XRSimage(1536,1536)
      real Pixel_corner_skyx(4), Pixel_corner_skyy(4)
      integer i,j,k
      integer minx,miny,maxx,maxy
      real outer_product,judge,vector(2),vector1(2),vector2(2)
      real SkyPixelsPerXRSPixel
      real XRSimageMin, XRSimageMax

C     XRS pixel size is 0.624 mm x 0.624 mm
C     Sky pixel size (without binning) is 0.02274 mm x 0.02274 mm
C     Hence, number of sky pixels per XRS pixel is the following:
      SkyPixelsPerXRSPixel = 752.988

C     Number of sky pixels in one XRS pixel after the
C     sky image binning is the following.
      SkyPixelsPerXRSPixel = SkyPixelsPerXRSPixel
     $     /ImageBinning/ImageBinning


      minx=9999
      miny=9999
      maxx=-9999
      maxy=-9999
      do i=1, 4
         minx=min(minx,INT(Pixel_corner_skyx(i)))
         maxx=max(maxx,INT(Pixel_corner_skyx(i)))
         miny=min(miny,INT(Pixel_corner_skyy(i)))
         maxy=max(maxy,INT(Pixel_corner_skyy(i)))
      end do

      do i=minx,maxx
         do j = miny,maxy
            vector(1)=real(i)
            vector(2)=real(j)
C     Check if vector is inside the quadrilateral
C     determined by the four corners
            do k=1,4
               vector1(1)=Pixel_Corner_skyx(k)
               vector1(2)=Pixel_Corner_skyy(k)
               vector2(1)=Pixel_Corner_skyx(mod(k,4)+1)
               vector2(2)=Pixel_Corner_skyy(mod(k,4)+1)
C     Well, this is the criterion. Think carefully!
               judge= outer_product(vector1,vector)
     $               +outer_product(vector,vector2)
     $               -outer_product(vector1,vector2)

               if(judge.lt.0.0) go to 100
            end do
C     Fill all the sky pixels included in the XRS pixel
            XRSimage(i,j)=XRSimage(i,j)+1./SkyPixelsPerXRSPixel
            XRSimageMin=min(XRSimageMin,XRSimage(i,j))
            XRSimageMax=max(XRSimageMax,XRSimage(i,j))

 100        continue
         end do
      end do

      end

      real function outer_product(vector1,vector2)
      implicit none
      real vector1(2),vector2(2)
      outer_product=
     $     vector1(1)*vector2(2)-vector1(2)*vector2(1)
      end

      subroutine smooth(image, ArraySize, ImageBinning, SmoothWidth,
     $     minvalue,maxvalue)
      implicit none
      integer imagebinning, SmoothWidth, ArraySize
      real image(ArraySize,ArraySize),minvalue,maxvalue

      real tmpimage(1536,1536), average
      integer i, j, k, l, smoothbin

      minvalue=1e10
      maxvalue=-1e10
      smoothbin = (SmoothWidth - 1) / 2

      do i =1, ArraySize/ImageBinning
         do j=1, ArraySize/ImageBinning
            if(i-smoothbin.lt.1 .or. j-smoothbin.lt.1 .or.
     $         i+smoothbin.gt.ArraySize/ImageBinning .or.
     $         j+smoothbin.gt.ArraySize/ImageBinning ) then
c     outer most parts of the image are not smoothed.
               tmpimage(i,j)=image(i,j)
            else
               average = 0.0
               do l = i-smoothbin, i+smoothbin
                  do k = j-smoothbin, j+smoothbin
                     average = average + image(l,k)
                  end do
               end do
               average = average/SmoothWidth/SmoothWidth
               tmpimage(i,j) = average
            endif
         end do
      end do

      do i=1, ArraySize/ImageBinning
         do j=1, ArraySize/ImageBinning
            image(i,j) = tmpimage(i,j)
            if(image(i,j).gt.0.0) then
               minvalue=min(minvalue,image(i,j))
               maxvalue=max(maxvalue,image(i,j))
            endif
         end do
      end do

      end

      subroutine draw_coordinate_grids(euler,xmin,xmax,ymin,ymax)
      implicit none
      double precision Euler(3)
      real xmin, xmax, ymin, ymax

      integer ngrid
      parameter (ngrid=61)

      double precision ra, dec

      integer j
      double precision focal_length
      double precision ra_border(9),dec_border(9)
      double precision minra,maxra,maxdec,mindec
      data focal_length/4500./

      call get_ra_dec_border(euler,focal_length,ra_border,dec_border,
     $     xmin,xmax,ymin,ymax)

c     determine the range of DEC and R.A. for the grids
      call getminmaxradec(ra_border,dec_border,minra,maxra,
     $     mindec,maxdec)

      do j=1,10
         dec = mindec+(maxdec-mindec)/9*(j-1)
C     draw the const. declination line
         call draw_grid('dec',euler,focal_length,dec,ngrid,minra,maxra,
     $        xmin,xmax,ymin,ymax)
      end do

      do j=1,9
         ra  = minra +(maxra-minra)/8*(j-1)
C     draw the const. right ascension line
         if(.not.(maxra.eq.360.and.minra.eq.0.0.and.ra.eq.maxra))
     $        call draw_grid('ra',euler,focal_length,ra,ngrid,
     $        mindec,maxdec,xmin,xmax,ymin,ymax)
      end do
      end

      subroutine get_ra_dec_border(euler, focal_length,
     $     ra_border,dec_border,xmin, xmax, ymin, ymax)
      implicit none
      double precision focal_length,euler(3),ra_border(9),dec_border(9)

      real xmin, xmax, ymin, ymax

      double precision detx, dety
      integer i,j
      do i=1,3
         detx=DBLE(xmin+(xmax-xmin)/2.0*(i-1))
         do j=1, 3
            dety=DBLE(ymin+(ymax-ymin)/2.0*(j-1))
            call detXY2radec(detx,dety,euler,focal_length,
     $           ra_border(3*(i-1)+j), dec_border(3*(i-1)+j))
         end do
      end do
      end

      subroutine getminmaxradec(ra_border,dec_border,minra,maxra,
     $     mindec,maxdec)
      implicit none
      double precision ra_border(9),dec_border(9),minra,maxra,
     $     mindec,maxdec

      double precision sorted(9),difference(9),tmp,maxdiff
      integer i,j

      do i=1, 9
         sorted(i)=ra_border(i)
      end do

      do i=1,9
         do j=i+1,9
            if(sorted(i).gt.sorted(j)) then
               tmp = sorted(j)
               sorted(j)=sorted(i)
               sorted(i)=tmp
            endif
         end do
      end do

      do i=1,8
         difference(i)=sorted(i+1)-sorted(i)
      end do
      difference(9) = sorted(1)+360.0-sorted(9)

      maxdiff=difference(1)
      do i=2,9
         maxdiff = max(maxdiff,difference(i))
      end do

      do i=1, 9
         if(maxdiff.eq.difference(i)) then
            if(i.ne.9) then
               minra = sorted(i+1)
               maxra = sorted(i)
            else
               minra = sorted(1)
               maxra = sorted(9)
            endif
         endif
      end do


      if(minra.gt.maxra) minra = minra - 360.0

      maxdec=-999.
      mindec= 999.
      do i = 1,9
         maxdec=max(maxdec,dec_border(i))
         mindec=min(mindec,dec_border(i))
      end do
      if(maxdiff.lt.180.0) then
c     North pole or south pole is in the figure
         if(mindec.gt.0.0) maxdec=90.0
         if(maxdec.lt.0.0) mindec=-90.0
         minra = 0.0
         maxra = 360.0
      endif

      end

      subroutine  draw_grid(ra_or_dec,euler,focal_length,
     $     const_ang,ngrid,start,end, xmin, xmax, ymin, ymax)
      implicit none
      character*(*) ra_or_dec
      double precision start, end, const_ang,euler(3),focal_length
      integer ngrid,i
      real xmin, xmax, ymin, ymax

      double precision ra, dec, detX, detY
      real gridX(61), gridY(61)
      character*256 text
      real angle

      if(index(ra_or_dec,'ra').gt.0) then
         ra = const_ang
c     light red for RA grids
         call pgscr(2,1.,0.5,0.5)
         call pgsci(2)
         if(const_ang.lt.0.0) then
            write(text,'(a,f6.2)') 'RA=',const_ang+360.
         else
            write(text,'(a,f6.2)') 'RA=',const_ang
         endif
      else
         dec = const_ang
c     light blue for DEC grids
         call pgscr(4,0.5,0.5,1.)
         call pgsci(4)
         write(text,'(a,f7.3)') 'DEC=',const_ang
      endif
      do i = 1, ngrid
         if(index(ra_or_dec,'ra').gt.0) then
            dec = start+(end-start)/(ngrid-1)*i
         else
            ra = start+(end-start)/(ngrid-1)*i
         endif
         call radec2detXY(ra,dec,euler,focal_length,detX,detY)
         gridX(i)=detX
         gridY(i)=detY
      end do

      call pgline(ngrid, gridX, gridY)

      if(gridx(ngrid/2).gt.xmin.and.gridx(ngrid/2).lt.xmax.and.
     $   gridy(ngrid/2).gt.ymin.and.gridy(ngrid/2).lt.ymax) then
         call get_angle(gridx(ngrid/2),gridy(ngrid/2),
     $        gridx(ngrid/2+1),gridy(ngrid/2+1), angle)
C     write the values of the RA and DEC (do not write at the poles)
         if(dec.ne.90.0.and.dec.ne.-90.0) then
            call pgptxt(gridx(ngrid/2),gridy(ngrid/2),angle,0.0,text)
         endif
      endif

      call pgsci(1)

      end

      subroutine detXY2radec(detX,detY,euler,focal_length,ra,dec)
      implicit none
      double precision ra, dec, euler(3), focal_length, detX, detY

      double precision sky(3),deg2rad,
     $     sat(3),tmp1(3),tmp2(3),ray(3),ray_alpha,ray_delta
      data deg2rad/1.745329252d-2/

C     Flip the Y-axis (look up --> look down).
      detY=-detY

      if(detX.eq.0.0) then
         if(dety.gt.0) then
            ray_alpha = 90.0*deg2rad
         else
            ray_alpha = -90.0*deg2rad
         endif
      elseif(detX.gt.0.0) then
         ray_alpha = atan(detY/detX)
      else
         ray_alpha = atan(detY/detX)+180.0*deg2rad
      endif

      ray_delta = -acos(sqrt(detX**2+detY**2)/focal_length)

      ray(1)= cos(ray_delta)*cos(ray_alpha)
      ray(2)= cos(ray_delta)*sin(ray_alpha)
      ray(3)= sin(ray_delta)

C     ray is the direction vector of the ray *from* the point source,
C     and the sat is the direction vector of the source in the
C     satellite coordinate
      sat(1)=-ray(1)
      sat(2)=-ray(2)
      sat(3)=-ray(3)

C     calculate the pointing vector in the celestial coordinates
      call rot_z(sat, -euler(3),tmp1)
      call rot_y(tmp1,-euler(2),tmp2)
      call rot_z(tmp2,-euler(1),sky)

C     sky is the pointing vector of the source in the celestial coordinates
      dec=atan(sky(3)/sqrt(sky(1)**2+sky(2)**2))/deg2rad

      if(sky(1).gt.0.0) then
         ra =atan(sky(2)/sky(1))/deg2rad
      else
         ra =atan(sky(2)/sky(1))/deg2rad+180.0
      endif
      if(ra.lt.0.0) then
         ra=ra+360.0
      endif

      end

      subroutine radec2detXY(ra,dec,euler,focal_length,detX,detY)
      implicit none
      double precision ra, dec, euler(3), focal_length, detX, detY

      double precision sky(3),deg2rad,
     $     sat(3),tmp1(3),tmp2(3),ray(3),ray_alpha,ray_delta
      data deg2rad/1.745329252d-2/

C     calculate the pointing vector of the source in the celestial coordinates
      sky(1)= cos(dec*deg2rad)*cos(ra*deg2rad)
      sky(2)= cos(dec*deg2rad)*sin(ra*deg2rad)
      sky(3)= sin(dec*deg2rad)

C     calculate the pointing vector in the satellite coordinates
      call rot_z(sky, euler(1),tmp1)
      call rot_y(tmp1,euler(2),tmp2)
      call rot_z(tmp2,euler(3),sat)

C     Now, sat is the pointing vector of the source in the
C     satellite coorinates.
      if(sat(3).lt.0) then
         call hderr('X-rays from behind the detector??')
         call hderr('Cannot determine the detXY position')
         go to 999
      endif

C     ray is the direction vector of the ray *from* the point source
C     (we invert the vector)
      ray(1)=-sat(1)
      ray(2)=-sat(2)
      ray(3)=-sat(3)

      ray_delta = atan(ray(3)/sqrt(ray(1)**2+ray(2)**2))
      if(ray(1).gt.0.0) then
         ray_alpha = atan(ray(2)/ray(1))
      else
         ray_alpha = atan(ray(2)/ray(1))+180.*deg2rad
      endif

      detX=focal_length*cos(ray_delta)*cos(ray_alpha)
      detY=focal_length*cos(ray_delta)*sin(ray_alpha)

C     Flip the Y-axis (look down --> look up).
C     Note that the look-down image on the detector is a mirror image
C     of the sky.
      detY=-detY

 999  continue
      end

      subroutine rot_z(in,ang,out)
      implicit none
      double precision in(3),ang,out(3)
      double precision deg2rad
      double precision mat(3,3)
      data deg2rad/1.745329252d-2/
      mat(1,1)= cos(ang*deg2rad)
      mat(1,2)= sin(ang*deg2rad)
      mat(1,3)= 0.0
      mat(2,1)= -sin(ang*deg2rad)
      mat(2,2)= cos(ang*deg2rad)
      mat(2,3)= 0.0
      mat(3,1)= 0.0
      mat(3,2)= 0.0
      mat(3,3)= 1.0
      call calmat(in,mat,out)
      end

      subroutine rot_y(in,ang,out)
      implicit none
      double precision in(3),ang,out(3)
      double precision deg2rad
      double precision mat(3,3)
      data deg2rad/1.745329252d-2/
      mat(1,1)= cos(ang*deg2rad)
      mat(1,2)= 0.0
      mat(1,3)= -sin(ang*deg2rad)
      mat(2,1)= 0.0
      mat(2,2)= 1.0
      mat(2,3)= 0.0
      mat(3,1)= sin(ang*deg2rad)
      mat(3,2)= 0.0
      mat(3,3)= cos(ang*deg2rad)
      call calmat(in,mat,out)
      end

      subroutine calmat(in,mat,out)
      implicit none
      double precision in(3),out(3),mat(3,3)
      integer i, j
      do i=1,3
         out(i)=0.0
         do j=1,3
            out(i)=out(i)+mat(i,j)*in(j)
         end do
      end do
      end

      subroutine get_angle(x1,y1,x2,y2,angle)
      implicit none
      real x1,y1,x2,y2,angle
      if(x1 .eq. x2) then
         angle = 90.0
      else
         angle = atan((y2-y1)/(x2-x1))*57.29
      endif
      end

      subroutine plot_contour(imagesize,image,center,pixelsize,
     $     xmin,ymin,xmax,ymax,SmoothWidth,logscale,nc,minvalue,
     $     maxvalue)
      implicit none
      integer imagesize, SmoothWidth, nc
      real image(imagesize,imagesize)
      real center,pixelsize
      real xmin,ymin,xmax,ymax
      real maxvalue,minvalue

      integer i, smoothbin
      character*80 text
      real tr(6), c(16)
      real tmp1,tmp2
      logical logscale

      if(logscale) then
         do i=1, nc
            tmp1 = log10(minvalue)
            tmp2 = log10(maxvalue)
            c(i) = 10**(tmp1+(tmp2-tmp1)/(nc-1)*(i-1))
         end do
      else
         do i=1,nc
            c(i) = minvalue+(maxvalue-minvalue)/(nc-1)*(i-1)
         end do
      endif
      call pgsch(0.6)
      do i=1, nc
         write(text,'(f10.2)') c(i)
         call pgmtxt('Rv',4.5,1.0+0.05*(i-nc),0.0,text)
      end do
      write(text,'(a)') 'Contour levels'
      call pgmtxt('Rv',1.2,1.1,0.0,text)
      write(text,'(a,f6.4,a)') 'counts/(',pixelsize,'arcmin)\\u2\\d'
      call pgmtxt('Rv',1., 1.05,0.0,text)
      call pgsch(1.0)

      tr(1)=-center*pixelsize
      tr(2)=pixelsize
      tr(3)=0.
      tr(4)=-center*pixelsize
      tr(5)=0.
      tr(6)=pixelsize
C     Define color grey
      call pgscr(48,0.5,0.5,0.5)
      call pgsci(48)
      smoothbin = (SmoothWidth - 1) / 2
      call pgcont(image, 256, 256,
     $     2+smoothbin, 255-smoothbin, 2+smoothbin, 255-smoothbin,
     $     c, nc, tr)
      call pgsci(1)
      end

      subroutine fill_XRS_pixel(value,Pixel_corner_X,Pixel_corner_Y)
      implicit none
      real value(0:31)
      real pixel_corner_x(4,0:31),pixel_corner_y(4,0:31)

      real xcorners(4),ycorners(4)
      real red, green,blue
      integer i,j
      real pixelmax,pixelmin
      integer order(0:29),inverseorder(0:29)
      integer cindex_used
      real pixel2arcmin

      real tmpvalue(0:29)

      data cindex_used/32/
      data pixel2arcmin/0.01737208/

C     Pixel 2 is not used, and Pixel 3 is dead.
      do i=0,29
         if(i.le.1) then
            tmpvalue(i) = value(i)
         else
            tmpvalue(i) = value(i+2)
         endif
      end do

      pixelmax=-999.
      pixelmin=1e10
      do i=0,29
         pixelmax=max(pixelmax,tmpvalue(i))
         pixelmin=min(pixelmin,tmpvalue(i))
      end do

C     Check the order of the counts/pixel
C     'order(i)' gives the brightness order of the i-th pixel (in 0-31),
C     brightest (0) to the dimmest (31)
C     'inverseorder(i)' gives the pixel_id (in 0-31),
C     of the pixels from the brightest (i=0) to the dimmest (i=31)
      call sort_pixel(tmpvalue,order,inverseorder)

C     determine the color scale
      do i=0,29
         red=(tmpvalue(inverseorder(i))-pixelmin)/(pixelmax-pixelmin)
         green=1.-red
         blue=1.-(red-green)**2
         call pgscr(cindex_used+30-i,red,green,blue)
      end do
      do i=0,29
         do j = 1, 4
            if(i.le.1) then
               xcorners(j)=Pixel_corner_x(j,i)*pixel2arcmin
               ycorners(j)=Pixel_corner_y(j,i)*pixel2arcmin
            else
               xcorners(j)=Pixel_corner_x(j,i+2)*pixel2arcmin
               ycorners(j)=Pixel_corner_y(j,i+2)*pixel2arcmin
            endif
         end do
         call pgsci(cindex_used+30-order(i))
         call pgsfs(1)
         call pgpoly(4,xcorners,ycorners)
      end do
      call pgsci(1)

C     draw the wedge
      call pgscir(cindex_used+1,cindex_used+30)
C     set image transfer function:0=liner,1=log,2=square
      call pgsitf(2)
      call pgwedg('bi',2.75,3.,pixelmin,pixelmax,'counts/XRS pixel')

      end

      subroutine sort_pixel(value,order,inverseorder)
      implicit none
      real value(0:29)
      integer order(0:29),inverseorder(0:29)
      integer i,j,k
      real biggest,tmp
      real newarray(0:29)
      logical j_appeared

      do i=0,29
         newarray(i)=value(i)
      end do
      do i=0,28
         tmp = newarray(i)
         biggest=newarray(i)
         k = i
         do j=i+1,29
            if(newarray(j).gt.biggest) then
               biggest=newarray(j)
               k = j
            end if
         end do
         inverseorder(i)=k
         newarray(i)=biggest
         newarray(k)=tmp
      end do

C     determine the order and inverseorder
C     order(i) gives the brightness order of the i-th pixel
C     inverseorder(i) gives the pixel ID of the i-th brightest pixel
      do i=0,29
         do j=0,29
            if(newarray(i).eq.value(j)) then
c     check if j already appeared in inverseorder
               j_appeared=.false.
               do k=0,i-1,1
                  if(j.eq.inverseorder(k)) then
                     j_appeared=.true.
                  endif
               end do
               if(.not.j_appeared) then
                  inverseorder(i)=j
               endif
            endif
         end do
      end do
      do i=0,29
         do j=0,29
            if(newarray(j).eq.value(i)) then
c     check if j already appeared in inverseorder
               j_appeared=.false.
               do k=0,i-1,1
                  if(j.eq.order(k)) then
                     j_appeared=.true.
                  endif
               end do
               if(.not.j_appeared) then
                  order(i)=j
               endif
            endif
         end do
      end do
      end

      subroutine PlotDetImage(Pixel_Values,Pixel_corner_focx,
     $     Pixel_corner_focy, Euler, detimage, margin,nc,minvalue,
     $     maxvalue,logscale,simfile,coordinate_grids)
      implicit none
      real  PIXEL_VALUES(0:31)
      real Pixel_corner_focx(4,0:31), Pixel_corner_focy(4,0:31)
      double precision Euler(3)
      real detimage(256, 256)
      real detxmin,detxmax,detymin,detymax
      real mm2arcmin
      real pixel2arcmin
      integer i, k, margin
      character*256 text
      integer nc
      real minvalue, maxvalue, xp, yp
      logical logscale,simfile, coordinate_grids
      data mm2arcmin/0.763943727/
      data pixel2arcmin/0.01737208/

C     Detector image in arcmin scale
      detxmin=-128.0*pixel2arcmin
      detxmax= 128.0*pixel2arcmin
      detymin=detxmin
      detymax=detxmax
      call pgpage
      call pgvport(0.1,0.9,0.15,0.88)
      call pgwnad(detxmin,detxmax,detymin,detymax)

c     fill the pixels
      call fill_XRS_pixel(pixel_values,
     $     Pixel_corner_focx,Pixel_corner_focy)

C     draw the coordinate grids
C     xmin, xmax, ymin, ymax should be given in mm
      if(coordinate_grids) then
         call draw_coordinate_grids(euler,detxmin,detxmax,detymin,
     $        detymax)
      endif

      if(simfile) then
         call plot_contour(256, detimage,
     $     128.5, pixel2arcmin,
     $     detxmin,detymin,detxmax,detymax,margin,
     $     logscale, nc,minvalue,maxvalue)
C     Draw the pixel map. Note, we are already in the DETXY
C     coordinates, where center is (0,0) and unit is arcmin.
      endif
      call pgsch(0.7)
      do i = 0, 31
         if(i.ne.2.and.i.ne.3) then
            do k = 1, 4
               if(k.eq.1) then
                  call pgmove(Pixel_corner_focx(k,i)*pixel2arcmin,
     $                 Pixel_corner_focy(k,i)*pixel2arcmin)
               else
                  call pgdraw(Pixel_corner_focx(k,i)*pixel2arcmin,
     $                 Pixel_corner_focy(k,i)*pixel2arcmin)
                  if(k.eq.4) then
                     call pgdraw(Pixel_corner_focx(1,i)*pixel2arcmin,
     $                    Pixel_corner_focy(1,i)*pixel2arcmin)
                  end if
               endif
            end do
            write(text,'(a,i2)') 'Pixel:',i
            call pgtext(Pixel_corner_focx(4,i)*pixel2arcmin+0.02,
     $           Pixel_corner_focy(4,i)*pixel2arcmin+0.02,text)
            if(i.ne.3) then
               write(text,'(i8)') INT(Pixel_values(i))
               xp = Pixel_corner_focx(4,i)*pixel2arcmin-0.02
               yp = Pixel_corner_focy(4,i)*pixel2arcmin+0.2
               call pgtext(xp, yp, text)
            endif
         endif
      end do
      call pgsch(1.0)

C     draw the box and label (in arcmin)
      call pgbox('bnst',0.0,0,'bnst',0.0,0)
      call pgsls(4)
      call pgbox('ag',0.0,0,'ag',0.0,0)
      call pgsls(1)
      call pglabel(' ','DETY [arcmin]', ' ')
      call pgmtxt('B',2.2,0.5,0.5,'DETX [arcmin]')
C     draw the box and label (in mm)
      call pgwnad(detxmin/mm2arcmin,detxmax/mm2arcmin,
     $            detymin/mm2arcmin,detymax/mm2arcmin)
      call pgbox('cmst',0.0,0,'cmst',0.0,0)
      call pgmtxt('R',2.2,0.5,0.5,'DETY [mm]')
      call pgmtxt('T',1.8,0.5,0.5,'DETX [mm]')
      end

      subroutine write_nevents(nevents,bin,smooth)
      implicit none
      integer nevents, bin, smooth
      character*80 text
      write(text,'(a,i7)') 'Number of events:', nevents
      call pgsch(0.7)
      call pgmtxt('bv',2.8,-0.3,0.0,text)
      write(text,'(a,i1,a,i2)')
     $     'Binning: ',bin, '   Smooth: ', smooth
      call pgmtxt('bv',4.0,-0.3,0.0,text)
      call pgsch(1.0)
      end

      subroutine pixel_boundaries
      implicit none
      call pgmove(0.0,1.0)
      call pgdraw(0.0,5.0)
      call pgdraw(1.0,5.0)
      call pgdraw(1.0,6.0)
      call pgdraw(5.0,6.0)
      call pgdraw(5.0,5.0)
      call pgdraw(4.0,5.0)
      call pgdraw(4.0,4.0)
      call pgdraw(6.0,4.0)
      call pgdraw(6.0,1.0)
      call pgdraw(5.0,1.0)
      call pgdraw(5.0,0.0)
      call pgdraw(1.0,0.0)
      call pgdraw(1.0,1.0)
      call pgmove(0.0,1.0)
      call pgdraw(5.0,1.0)
      call pgmove(0.0,2.0)
      call pgdraw(6.0,2.0)
      call pgmove(0.0,2.0)
      call pgdraw(6.0,2.0)
      call pgmove(0.0,3.0)
      call pgdraw(6.0,3.0)
      call pgmove(0.0,4.0)
      call pgdraw(6.0,4.0)
      call pgmove(1.0,1.0)
      call pgdraw(1.0,5.0)
      call pgmove(2.0,0.0)
      call pgdraw(2.0,6.0)
      call pgmove(3.0,0.0)
      call pgdraw(3.0,6.0)
      call pgmove(1.0,5.0)
      call pgdraw(4.0,5.0)
      call pgmove(4.0,0.0)
      call pgdraw(4.0,6.0)
      call pgmove(5.0,1.0)
      call pgdraw(5.0,4.0)
      end

      subroutine pixel2position(pixel_x,pixel_y)
      implicit none
      real pixel_x(0:31),pixel_y(0:31)
      pixel_x(20)=1.0
      pixel_y(20)=0.0
      pixel_x(22)=2.0
      pixel_y(22)=0.0
      pixel_x(31)=3.0
      pixel_y(31)=0.0
      pixel_x(29)=4.0
      pixel_y(29)=0.0

      pixel_x(18)=0.0
      pixel_y(18)=1.0
      pixel_x(19)=1.0
      pixel_y(19)=1.0
      pixel_x(21)=2.0
      pixel_y(21)=1.0
      pixel_x(30)=3.0
      pixel_y(30)=1.0
      pixel_x(28)=4.0
      pixel_y(28)=1.0
      pixel_x(27)=5.0
      pixel_y(27)=1.0

      pixel_x(16)=0.0
      pixel_y(16)=2.0
      pixel_x(17)=1.0
      pixel_y(17)=2.0
      pixel_x(23)=2.0
      pixel_y(23)=2.0
      pixel_x(24)=3.0
      pixel_y(24)=2.0
      pixel_x(26)=4.0
      pixel_y(26)=2.0
      pixel_x(25)=5.0
      pixel_y(25)=2.0

      pixel_x(9)=0.0
      pixel_y(9)=3.0
      pixel_x(10)=1.0
      pixel_y(10)=3.0
      pixel_x(8)=2.0
      pixel_y(8)=3.0
      pixel_x(7)=3.0
      pixel_y(7)=3.0
      pixel_x(1)=4.0
      pixel_y(1)=3.0
      pixel_x(0)=5.0
      pixel_y(0)=3.0

      pixel_x(11)=0.0
      pixel_y(11)=4.0
      pixel_x(12)=1.0
      pixel_y(12)=4.0
      pixel_x(14)=2.0
      pixel_y(14)=4.0
      pixel_x(5) =3.0
      pixel_y(5) =4.0

      pixel_x(13) =1.0
      pixel_y(13) =5.0
      pixel_x(15) =2.0
      pixel_y(15) =5.0
      pixel_x(6)  =3.0
      pixel_y(6)  =5.0
      pixel_x(4)  =4.0
      pixel_y(4)  =5.0
      end

      subroutine geterror(message,xrsqlp)
      character*(*) message
      integer xrsqlp
      call hderr(message)
      xrsqlp=1
      return
      end

      subroutine geteuler(history,euler)
C     Get mean euler angle from the HISTORY keyword
C     written by xrscoord.  Using HISTORY keyword is not
C     desirable, and in future this should be replaced to
C     read more properly-defined mean euler angle keywords.

      implicit none
      character*(*) history
      double precision euler(3)
      integer pos0,pos1,pos2,pos3,pos4

      pos0=index(history,'=')
      pos1=pos0+index(history(pos0:len(history)),'(')-1
      pos2=pos1+index(history(pos1:len(history)),',')-1
      pos3=pos2+index(history(pos2+1:len(history)),',')
      pos4=pos3+index(history(pos3:len(history)),')')-1

      read(history(pos1+1:pos2-1),*) euler(1)
      read(history(pos2+1:pos3-1),*) euler(2)
      read(history(pos3+1:pos4-1),*) euler(3)

      end
