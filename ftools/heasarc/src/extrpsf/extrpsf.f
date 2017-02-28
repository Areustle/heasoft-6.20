C $Id: extrpsf.f,v 3.21 2013/07/27 19:45:02 irby Exp $ 
*+EXTRPSF 

      subroutine extrpf
c ------------------- description ----------------------------
c This subroutine reads observed event file as input and extracts 
c the data to an output observed RPSF and model RPSF extensions
c in FITS format. The user specify the position at which the RPSF is
c required, outer radius, and the number of bins.  The resultant RPSF
c dataset is then converted to standard units of counts/arcmin^2

c ------------------ routines called ---------------------------------
c xrpsf_gp        : gets parameters
c read_region     : reads region file
c region_boundary : converts shape parameter to shape
c open_file       : opens input event file
c cels2xy         : converts celestial coordinates to detector coord
c calc_rad_bin    : calculates the radial bins in arcmin
c xrpsf_rdat      : read event file one row at a time
c xrpsf_calc      : finds out if the read event is inside the defined region
c calc_areawgt_all: calculates areawgt factor for entire region
c calc_areawgt_rad: calculates areawgt factor for given radius 
c xrpsf_conv      : makes the conversion to counts/arcmin^2
c xrpsf_wt        : writes the converted data in FITS format
c
c ------------------ declare variables ------------------------------+

      implicit none
      character(180) infil,outfil,regfil
      character(100) subinfo
      character(10) telescope,instrument 
      character(7) error
      character(1) area_wgt_option
      integer maxbins,maxrad,maxtheta,maxen 
      parameter (maxbins=1000,maxrad=1000)
      real pi,x_in,y_in,celsx,celsy,equinox
      real bck,bckgrnd,bckgrnd_percent,bck_area
      real pix_size,pix_size_min
      real sumrcts,sumtcts
      real rad_length,bin_length,rad_length_sq
      real  bck_rad, bck_lo,bck_hi,bck_lo_sq,bck_hi_sq
      real total_area,back_area_percent
      real sumevt(maxbins)
      real bck_pixel, bck_per_pixel
      integer p_rad_lo,p_rad_hi,p_rad_hi_sq,p_rad_lo_sq
      integer p_cts,p_cts_err,p_area,p_area_wgt

      integer nbins,x,y,i,j,frow
      integer nevt,xcol,ycol,picol,chanmin,chanmax
      integer accept(maxbins), reject(maxbins),npi(maxbins)
      integer chatter, errflg, status, ineed,iget,tot_accept
      real wgt_rad
      logical celestial,image,killit,bkgrnd_pres,properr,areawgt
      logical region_pres
c ---------------- variables for region file ---------------------------
      integer maxpoints
      integer iunitr
      parameter (maxpoints=1000)
      character(200) comm_line(10)
      character(10) shape(10)
      character(1) sign(10)
      real points(maxpoints,maxpoints)
      real x_center(10),y_center(10),xmin(10),xmax(10),ymin(10),ymax(10)
      real radiusmin(10),radiusmax(10),radius(10),twice_a(10)
      real x1(10),y1(10),x2(10),y2(10)
      integer iunit,line_no,shape_no
      integer npoints(maxpoints),char_no(10)
      logical minus
      integer minus_bck,minus_cts(maxbins)
c -----------------------------------------------------------------------
c      real rad_lo(maxbins), rad_hi(maxbins), rad_hi_sq(maxbins)
c      real rad_lo_sq(maxbins),area(maxbins,maxtheta,maxen)
c      real cts(maxbins,maxtheta,maxen)
c      real cts_err(maxbins,maxtheta,maxen)
c      real area_wgt(maxbins,maxtheta,maxen)

c ------------------ variable definitions ---------------------------
c
c infil      char input input event filename
c outfil     char input output event filename
c regfil     char       region file name
c x_in       real input the central coordinate of the event to be extracted
c x_out      real       total outer radius of the event to be extracted 
c rad_length int  input the extent of the radius 
c                       i.e., rad_length = xout - x_in
c nbins      int  input input user input no. of bins wants
c bin_length int        length of each bins
c rad_lo     real       inner radius of annulus
c rad_hi     real       outer radius of annulus
c rad_bin_sq real       sq. of the radial distance of each bin from the
c                       event center
c bck_rad    int  input minimum radial distance from the centroid of event
c                       where background to be calculated upto rad_length
c bck_lo     int        inner boundary for background calculation
c bck_hi     int        outer boundary for background calculation
c celestial  logical    true if event center is supplied as RA and DEC
c image      logical    true if event center is supplied as coordinates
c bkgrnd_pres logical   true if background calculation is required
c properr    logical    true if propagation error is required(hidden parameter)
c areawgt    logical    true if area weighting is to be done
c region_pres logical   true if region file is present
c bck_pixel  real       total pixels in background region
c                       (=total bckarea/area of each pixel) 
c
c --------------- authors/modification -------------------------------
c
c Banashree Mitra Seifert (Jan. 24, 1996) 1.0.0:
c
c Banashree Mitra Seifert (Oct. 22, 1996) 1.1.0:
c        . background per pixel is corrected
c          was wrong on V1.0.0
c
c Srilal Weera (01/11/97) 1.2.0:
c	 For HRI data the PHA column is now read instead of the PI column,
c        (which contains only zeros). 
c
c Banashree M Seifert (May15, 1997) 1.3.0:
c       . Default error calculation set to POISS-1 
c
c Banashree M Seifert (Sept. 24, 1996)1.4.0
c     . fixes are in routine CELS2XY
c         error while writing TCDELT(ycol) fixed
c         some error while calling wtinfo fixed
c Ning Gan (Feb. 5, 1997) 1.5.0
c       Increase the maxbins, maxrad, and maxpoints from 100 to 1000.
c       Add the limitation of the nbins in the parfile. (1-1000) 
c Peter D Wilson (June 23, 1998) 1.5.1
c       Updated for new extended file syntax (drop INQUIRE test on infil)
c --------------------------------------------------------------------
      character(5) version
      parameter (version = '1.5.1')
      character(10) taskname
      character(6) subname

c ------------- dynamic memory allocated array -------------------------

c      real rad_lo(maxbins), rad_hi(maxbins), rad_hi_sq(maxbins)
c      real rad_lo_sq(maxbins),area(maxbins,maxtheta,maxen)
c      real cts(maxbins,maxtheta,maxen)
c      real cts_err(maxbins,maxtheta,maxen)
c      real area_wgt(maxbins,maxtheta,maxen)

c --- DYNAMIC MEMORY ALLOCATION ---
c  the following MEM common block definition is in the system iraf77.inc
c  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ----------------------------------------------------------------------+

      subname  = 'extrpsf'
      taskname = 'extrpsf'
*-
      pi = 4.*atan(1.0)

c --------------------- initialisations --------------------------------+
      celestial = .false.
      image = .false.
      x_in = 0.
      y_in = 0.
      celsx = 0.
      celsy = 0.

c ----------------- get parameters -------------------------------------+
c GET PARAMETERS

      call xrpsf_gp (infil,outfil,x_in,y_in,celsx,celsy,equinox,
     >               chanmin,chanmax,rad_length,bkgrnd_pres,bck_rad,
     >               nbins,celestial,image,error,properr,areawgt,
     >               area_wgt_option,wgt_rad,region_pres,regfil,
     >               errflg,chatter,killit)

      if (errflg .ne. 0) then
          subinfo='returning from xrpsf_gp'
          call wterrm(subname,version,subinfo)
          goto 100
      endif
      call wtbegm(taskname,version,chatter)

c ------------------------ open region file ------------------------------ 
c OPEN REGION FILE

      if(region_pres) then
         status = 0
         call ftgiou(iunitr,status)
         call faopen (iunitr, regfil, 1, -1, status)
         if (status .ne. 0) then
             subinfo='opening region file'
             call wterrm(subname,version,subinfo)
             errflg=1
             goto 100
         endif

c ------------------- read region file -----------------------------------
c READ REGION FILE

      call read_region_psf(iunitr,char_no,maxpoints,line_no,
     >                       comm_line,shape_no,shape,npoints,
     >                       points,sign,chatter,errflg)

      if (errflg .ne. 0) then
          subinfo='returning from read_region_psf'
          call wterrm(subname,version,subinfo)
          errflg=1
          goto 100
      endif
c ------------------- calc. region boundary -----------------------------
c CALCULATE REGION BOUNDARIES

      do i=1,shape_no
         x_center(i)=0.
         y_center(i)=0.
         xmin(i)=0.
         xmax(i)=0.
         ymin(i)=0.
         ymax(i)=0.
         radiusmin(i)=0.
         radiusmax(i)=0.
         radius(i)=0.
         x1(i)=0.
         y1(i)=0.
         x2(i)=0.
         y2(i)=0.
         twice_a(i)=0.
      enddo

      call region_boundary(char_no,maxpoints,
     >                       shape_no,shape,npoints,points,sign,
     >                       x_center,y_center,xmin,xmax,ymin,ymax,
     >                       radiusmin,radiusmax,radius,x1,x2,y1,y2,
     >                       twice_a,chatter,errflg)

      if (errflg .ne. 0) then
          subinfo='returning from region_boundary'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

      endif

c ------------------- open data file ----------------------------------+
c OPEN INPUT EVENT FILE

      call open_file(infil,iunit,nevt,pix_size,telescope,instrument,
     >                xcol,ycol,picol,chatter,errflg)
      
      if (errflg .ne. 0) then
          subinfo='returning from xrpsf_open'
          call wterrm(subname,version,subinfo)
          goto 100
      endif
      
      
c ----------------- if celestial coordinate ---------------------------
c              convert it to detector coordinate
c              celsx ---> detx, celsy ---> dety
c ---------------------------------------------------------------------
c CONVERT CELESTIAL TO IMAGE COORD

      errflg=0
      if (celestial) then
          subinfo = 'input is in CELESTIAL coordinate'
          call wtinfo(chatter,10,2,subinfo)

          call cels2xy(infil,celsx,celsy,xcol,ycol,x_in,y_in,
     >                 chatter,errflg)  

          if (errflg .ne. 0) then
              subinfo = 'returning from cels2xy'
              call wtferr(subname,version,errflg,subinfo)
              goto 100
          endif

      elseif (image) then
          subinfo = 'input is in IMAGE coordinate'
          call wtinfo(chatter,10,2,subinfo)
 
      else
          subinfo = 'coordinate input should be defined'
          call wtinfo(chatter,1,2,subinfo)
          errflg = 1
          goto 100     
 
      endif
 
c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------
c GET DMA

      p_rad_lo = 0
      p_rad_hi = 0
      p_rad_hi_sq = 0
      p_rad_lo_sq = 0
      p_area = 0 
      p_area_wgt = 0
      p_cts = 0 
      p_cts_err = 0

      maxtheta=1
      maxen=1

      iget=0
      status = 0
      call udmget(nbins, 6, p_rad_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*4
 
      status = 0
      call udmget(nbins, 6, p_rad_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*4

      status = 0
      call udmget(nbins, 6, p_rad_hi_sq, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*4

      status = 0
      call udmget(nbins, 6, p_rad_lo_sq, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*4

      status = 0
      call udmget(nbins*maxtheta*maxen, 6, p_area, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*maxtheta*maxen*4

      status = 0
      call udmget(nbins*maxtheta*maxen, 6, p_area_wgt, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*maxtheta*maxen*4

      status = 0
      call udmget(nbins*maxtheta*maxen, 6, p_cts, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*maxtheta*maxen*4

      status = 0
      call udmget(nbins*maxtheta*maxen, 6, p_cts_err, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nbins*maxtheta*maxen*4

 50   ineed = 4*nbins*4 + 4*nbins*maxtheta*maxen*4

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
           call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory I get   =',iget
           call wtinfo(chatter,10,1,subinfo)

      if (status .ne. 0) then
          errflg = -1
          subinfo='failed to allocate dynamic memory '
          call wtinfo(chatter,1,1,subinfo)
          goto 100
      endif
  
c -------------------- background calculation ------------------------+
c          find out the annulus for background calculation
c --------------------------------------------------------------------+
c BACK GROUND CALCULATION

      if(bkgrnd_pres) then 
         if (bck_rad .gt. rad_length) then
             subinfo ='FOR BACKGROUND CALCULATION, Inner radius > '
     >              //'Outer radius of the event'
             call wterrm (subname,version,subinfo)
             errflg = 1
             goto 100
         endif

         if (bck_rad .eq. rad_length) then
             subinfo= 'FOR BACKGROUND CALCULATION, Inner radius = '
     >             //'                 Outer radius of the event'
             call wtwarm (subname,version,chatter,0,subinfo)
             subinfo= 'Background is set to 0'
             call wtinfo(chatter,0,1,subinfo)
         else 
         
             bckgrnd_percent = (rad_length - bck_rad)*100./rad_length
             write(subinfo,'(a,f5.1,a)')'BACKGROUND RADIUS IS',
     >                   bckgrnd_percent, '% OF THE TOTAL RADIUS'
             call wtinfo(chatter,10,1,subinfo)
         endif
      else
         subinfo='Background is not calculated'
         call wtinfo(chatter,10,1,subinfo)
      endif

c ----------- calculate the pixelsize in arcmin -----------------------

      pix_size_min = pix_size*60

c -------------------- radial bin calculation ------------------------+
c BIN CALCULATIONS

      bck_lo = bck_rad
      bck_hi = rad_length 
      bck_lo_sq = bck_lo * bck_lo 
      bck_hi_sq = bck_hi * bck_hi 
      bck_area  = pi*(bck_hi_sq - bck_lo_sq)
      total_area = pi*rad_length*rad_length
      back_area_percent = bck_area*100./total_area
      bck_pixel= bck_area/(pix_size_min*pix_size_min)

      bin_length = rad_length/nbins
      rad_length_sq = rad_length * rad_length 

      write(subinfo,'(a,f6.2,a)') 'Length of each bin for RPSF is',
     >                         bin_length,' arc_min'
      call wtinfo(chatter,10,1,subinfo)
      if(bkgrnd_pres) then
         subinfo='FOR BACKGROUND CALCULATION:'
         call wtinfo(chatter,10,0,subinfo)
         write(subinfo,'(a,f6.2,a)') 'Minimum radius is ',
     >                     bck_lo,' arc_min'
         call wtinfo(chatter,10,1,subinfo)
         write(subinfo,'(a,f6.2,a)') 'Maximum radius is ',
     >                 bck_hi,' arc_min'
         call wtinfo(chatter,10,1,subinfo)
         write(subinfo,'(a,f6.2,a)') 'Background area is ',
     >      back_area_percent, '% of total area'
         call wtinfo(chatter,10,1,subinfo)
      endif

c ------------------- calculation of radial bins ----------------------
c CALCULATE RADIAL BINS IN arcmin

      call calc_rad_bin(MEMR(p_rad_hi),MEMR(p_rad_lo),bin_length,nbins,
     >                  MEMR(p_rad_hi_sq),MEMR(p_rad_lo_sq),
     >                  MEMR(p_area),errflg,chatter)

c ---------------------- initialisation -------------------------------

      i=1
      do while (i .le. nbins)
         accept(i)=0
         reject(i)=0
         sumevt(i)=0.
         i=i+1
      enddo

c ------------------- read data row by row ----------------------------+
      
      subinfo = '!! PLEASE WAIT, GOING THROUGH EACH EVENT ONE BY ONE !!'
      call wtinfo(chatter,1,0,subinfo)

      frow = 0
      bckgrnd = 0.0
      tot_accept = 0

      minus_bck=0
      do j=1,nbins
         minus_cts(j) = 0
      enddo

      i=1

      do while (i .le. nevt)
         frow=frow+1

         call xrpsf_rdat(infil,iunit,xcol,ycol,picol,x,y,INT(pi),
     >                   frow,chatter,errflg)

                 if (errflg .ne. 0) then
                     subinfo='error returning from xrpsf_rdat'
                     call wterrm(subname,version,subinfo)
                     goto 100
                 endif
  
c ----------- initialising background and no. of counts --------------+
                 bck = 0
                 j=1
                 do while (j .le. nbins)
                    npi(j) = 0
                    j=j+1
                 enddo
c --------------------------------------------------------------------+

         call xrpsf_calc(x_in,y_in,x,y,INT(pi),rad_length_sq,
     >                   MEMR(p_rad_hi_sq),nbins,chanmin,chanmax,
     >                   bck_lo_sq,bck_hi_sq,pix_size_min,npi,
     >                   bck,region_pres,
     >                   shape_no,shape,x_center,y_center,
     >                   xmin,xmax,ymin,ymax,radiusmin,
     >                   radiusmax,radius,x1,x2,y1,y2,twice_a,minus,
     >                   minus_cts,chatter,errflg)

                 if (errflg .ne. 0) then
                     subinfo='error returning from xrpsf_calc'
                     call wterrm(subname,version,subinfo)
                     goto 100
                 endif

c ----------------- calculation ---------------------------------------+
c minus =.true. if the pixel is not inside region to be subtracted
c so initialise it with .false.
c DO REGION CALCULATION


                 if(bkgrnd_pres) then
                    if (bck_rad .eq. rad_length) then
                        bckgrnd = 0
                    else
                        if(minus) then
                           minus_bck=minus_bck+bck
                        else
                           bckgrnd = bckgrnd + bck
                        endif
                    endif
                 else
                    bckgrnd = 0
                 endif

                 do j = 1,nbins
                      if (npi(j) .ne. 0) then
                          accept(j) = accept(j) + 1
                          tot_accept = tot_accept + 1
                      else
                          reject(j) = reject(j) + 1
                      endif
                 enddo

            
      call xclock(i,nevt,10)

      i=i+1
      enddo

c ------------------- closing input file -----------------------------+
c CLOSING EVENT FILE
      status = 0
      call ftclos(iunit,status)
      subinfo= 'closing input event file'
      call wtferr(subname,version,status,subinfo)

      call cfrelun(iunit)
c --------------------------------------------------------------------

      write(subinfo,'(a,i8,a,i8)') 'Out of',nevt,
     >       ' events, total accepted events =', tot_accept
      call wtinfo(chatter,15,0,subinfo)
      write(subinfo,'(a,f10.2)') 'and total of background counts is ',
     >                         bckgrnd
      call wtinfo(chatter,15,0,subinfo)

c ------------------- calculate area_wgt factor -------------------
c CALCULATING AREA_WGT

      if(areawgt) then
         subinfo='calculating area_weighting factor'
         call wtinfo(chatter,0,0,subinfo)

         if(area_wgt_option .eq. '1') then
            subinfo='Area weighting to be done throughout the'
     >        //    ' region'
            call wtinfo(chatter,10,0,subinfo)
         
            call calc_areawgt_all(x_in,y_in,rad_length,
     >                            MEMR(p_rad_hi_sq),nbins,
     >                            pix_size_min,MEMR(p_area),areawgt,
     >                            MEMR(p_area_wgt),chatter,errflg)
           
            if (errflg .ne. 0) then
                subinfo='returning from calc_areawgt_all'
                call wterrm(subname,version,subinfo)
                goto 100
            endif 
         
         elseif(area_wgt_option .eq. '2') then
            call calc_areawgt_rad(x_in,y_in,rad_length,MEMR(p_rad_hi),
     >                            MEMR(p_rad_hi_sq),nbins,pix_size_min,
     >                            MEMR(p_area),areawgt,wgt_rad,
     >                            MEMR(p_area_wgt),chatter,errflg)
         
            if (errflg .ne. 0) then
                subinfo='returning from calc_areawgt_5'
                call wterrm(subname,version,subinfo)
                goto 100
            endif
         endif
      else
         subinfo='No area weighting is done'
         call wtinfo(chatter,10,0,subinfo)
         call calc_areawgt_no(nbins,MEMR(p_area_wgt),chatter,errflg)
      endif

  
c ------------------- calculate the counts -------------------------
c CONVERT THE COUNTS TO COUNTS/AREA

      if (bck_pixel .ne. 0.0) then
          bck_per_pixel=bckgrnd/bck_pixel
      else 
          bck_per_pixel=0.0
      endif


      call xrpsf_conv(MEMR(p_rad_lo),MEMR(p_rad_hi),sumevt,
     >                MEMR(p_area),bckgrnd,bck_area,accept,reject,
     >                minus_cts,minus_bck,
     >                nbins,pix_size_min,error,MEMR(p_cts),
     >                MEMR(p_cts_err),MEMR(p_area_wgt),sumrcts,
     >                sumtcts,chatter,errflg)

      if (errflg .ne. 0) then
          subinfo='error returning from xrpsf_conv'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c ---------------------- write to output file -------------------------+
c WRITE RESULTS

      call xrpsf_wt(outfil,infil,version,nbins,MEMR(p_rad_lo),
     >              MEMR(p_rad_hi),MEMR(p_cts),MEMR(p_cts_err),
     >              MEMR(p_area_wgt),pix_size,nevt,maxrad,maxtheta,
     >              telescope,instrument,bckgrnd,chanmin,chanmax,
     >              bck_per_pixel,
     >              sumrcts, sumtcts,errflg,chatter, killit)

      if (errflg .ne. 0) then
          subinfo='error returning from xrpsf_wt'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_rad_lo, 6, status)
      status = 0
      call udmfre(p_rad_hi, 6, status)
      status = 0
      call udmfre(p_rad_hi_sq, 6, status)
      status = 0
      call udmfre(p_rad_lo_sq, 6, status)
      status = 0
      call udmfre(p_area, 6, status)
      status = 0
      call udmfre(p_area_wgt, 6, status)
      status = 0
      call udmfre(p_cts, 6, status)
      status = 0
      call udmfre(p_cts_err, 6, status)

      if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wterrm(taskname,version,subinfo)
      endif

 100  call wtendm(taskname,version,errflg,chatter)
      end
c ----------------------------------------------------------------------
c            **** END OF RPSF2EEF ****
c ----------------------------------------------------------------------

*+XRPSF_GP

      subroutine xrpsf_gp (infil,outfil,x_in,y_in,celsx,celsy,equinox,
     >               chanmin,chanmax,rad_length,bkgrnd_pres,bck_rad,
     >               nbins,celestial,image,error,properr,areawgt,
     >               area_wgt_option,wgt_rad,region_pres,regfil,
     >               errflg,chatter,killit)

c --------------- description of xrpsf-gp --------------------------------
c
c This subroutine reads the user defined parameters as well as chatterflag.
c
c --------------- variables --------------------------------------------

      implicit none
      character*(*) infil,outfil,regfil
      character(7) error
      character*(*) area_wgt_option
      integer chatter,errflg
      real x_in,y_in,celsx,celsy,equinox,rad_length,bck_rad
      integer nbins,chanmin,chanmax
      real wgt_rad
      logical celestial,image,killit,bkgrnd_pres,properr,areawgt
      logical region_pres

c --------------- variable directory -----------------------------------
c
c infil    char : (input) EVENT file (user defined)
c outfil   char : (input) Output (PSF FITS FILE) filename
c chatter  int  : (input) chattiness flag (quiet<5, warning>5, verbose>20)
c errflg   int  : error flag (no error=0), returned to main
c errstr   char : errorstring for this routine
c
c --- called routines --------------------------------------------------


c --------------- authors/modification ---------------------------------
c
c Banashree Mitra Seifert (Jan, 1995)
c
c Banashree M Seifert (May, 1997) v1.1.0:
c       . default error calculation set to POISS-1
c Peter D Wilson (June 23, 1998) v1.1.1:
c       . Dropped INQUIRE test on infil... fails with new extended filenames
c     
c-----------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.1.1')
      character(9) subname
*-
c --------------- internal declarations --------------------------------
      character(100) info,fileinfo
      character(180) filename,ill_file(3)
      character(120) subinfo
      character(20) coordsys
      integer  status, extnum, n_ill
      logical ext, valfil

c ----------------------------------------------------------------------
*-
      subname ='xrpsf_gp'
      subinfo  = 'using '//subname//'Ver '//version
      call wtinfo(chatter,10,1,subinfo)

c ----------------- get input file parameters --------------------------

      n_ill = 0
      status = 0
      call uclgst('evtfil', infil, status)
      if (status .ne. 0) then
          subinfo = 'getting input file parameters !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(infil)
      if (infil(1:2) .eq. '  ') then
          subinfo = 'input file has to be entered, or none !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/23/98: Leave this for FTOPEN to determin
C      call fcpars(infil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
C          subinfo= 'input file does not exist !'
C          call wterrm(subname,version,subinfo)
C          fileinfo = 'filename : '//filename
C          call wterrm(subname,version,fileinfo)
C          errflg = 1
C          return
C      endif

      n_ill = n_ill + 1
C PDW 6/23/98: Call ftrtnm to strip off extension number
C      ill_file(n_ill) = filename
      call ftrtnm(infil, ill_file(n_ill), status)

c ---------------- outfile name ----------------------------------------

      call uclgst('rpsfil', outfil, status)
      if (status .ne. 0) then
          subinfo = 'getting output file parameter'
          call wterrm(subname,version,fileinfo)
      endif
      call crmvlbk(outfil)

c ---- terminate with an error mesg if user does not give outfil -------

      if (outfil(1:2)  .eq.  '  ') then
          subinfo = 'output filename must be entered !'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

c ------------------ check validity of outfil --------------------------

      valfil = .true.
      call ck_file(outfil,ill_file,n_ill,valfil,killit,chatter)
      if (.not. valfil) then
          subinfo = 'output file is not valid !'
          call wterrm(subname,version,fileinfo)
          errflg = 1
          return
      endif

c ----------------- coordinate system ------------------------------

      info = 'For coordinate system to be entered by user at the'
     >       //' prompt,'
      call wtinfo(chatter,0,0,info)
      info = 'if X and Y then it is IMAGE coord. system'
      call wtinfo(chatter,0,1,info)
      info = 'if  RA and DEC. then it is the CELESTIAl coord. system' 
      call wtinfo(chatter,0,1,info)
      info = 'RA and DEC are to be entered in degrees'
      call wtinfo(chatter,0,1,info)

      status = 0
      call uclgst('coordsys',coordsys,status)
      if (status .ne. 0) then
          subinfo ='getting coordinate system name for centroid'
          call wterrm(subname, version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(coordsys)
      call ftupch (coordsys)

c ----------------- coordinate system ------------------------------
c If the coordinate system is IMAGE then X and Y positions are given.
c But if the coordinate system is CELESTIAL, then right ascenssion (celx)
c and declination (celsy) are given.
c ----------------- celestial coord  system ------------------------------
      if (coordsys .eq. 'CELESTIAL') then
          celestial = .true.
          call uclgsr('x_in', celsx,status)
          if (status .ne. 0) then
              subinfo ='getting coord. system for celsx'
              call wterrm(subname, version,subinfo)
              errflg=1
              return
          endif
 
          call uclgsr('y_in', celsy,status)
          if (status .ne. 0) then
              subinfo ='getting coord. system for celsy'
              call wterrm(subname, version,subinfo)
              errflg=1
              return
          endif
          x_in = 0.
          y_in = 0.

c ----------------- iamge coord system ------------------------------
      elseif (coordsys .eq. 'IMAGE') then
         image = .true.
         call uclgsr('x_in',x_in,status)
          if (status .ne. 0) then
              subinfo ='getting coord. system for x'
              call wterrm(subname, version,subinfo)
              errflg=1
              return
          endif

          call uclgsr('y_in',y_in,status)
          if (status .ne. 0) then
              subinfo ='getting coord. system for y'
              call wterrm(subname, version,subinfo)
              errflg=1
              return
          endif
          celsx = 0.
          celsy = 0.  
      else

          subinfo='coordinate system should be entered'
          call wterrm(subname, version,subinfo)
          errflg=1
          return
      endif

c ----------------- chanmin ------------------------------
      status = 0
      call uclgsi('chanmin',chanmin,status)
      if (status .ne. 0) then
          subinfo ='getting chanmin value'
          call wterrm(subname, version,subinfo)
      endif

c ----------------- chanmax ------------------------------
      status = 0
      call uclgsi('chanmax',chanmax,status)
      if (status .ne. 0) then
          subinfo ='getting chanmax value'
          call wterrm(subname, version,subinfo)
      endif

c -------------- check if chanmax < chanmin ------------------

      if(chanmax .lt. chanmin) then
         subinfo='Maximum PI channel <  minimum PI channel'
         call wterrm(subname,version,subinfo)
         subinfo = 'It should be > minimum PI channel'
         call wtinfo(chatter,0,3,subinfo)
         errflg=1
         return
      endif

c ----------------- rad_hi ------------------------------
      status = 0
      call uclgsr('rad_length', rad_length,status)
      if (status .ne. 0) then
          subinfo ='getting radial length value'
          call wterrm(subname, version,subinfo)
          errflg=1
          return
      endif
c ----------------- number of bins ------------------------------
      status = 0
      call uclgsi('nbins', nbins,status)
      if (status .ne. 0) then
          subinfo ='getting number of bins(nbins) value'
          call wterrm(subname, version,subinfo)
          errflg=1
          return
      endif

c --------- background calculation is required or not ----------

      status = 0
      call uclgsb('bkgrnd_pres', bkgrnd_pres,status)
      if (status .ne. 0) then
          subinfo ='background calc. wanted or not'
          call wterrm(subname, version,subinfo)
          errflg=1
          return
      endif

c ----------------- minimum background radius -------------------
      if(bkgrnd_pres) then
         status = 0
         call uclgsr('bck_rad', bck_rad,status)
         if (status .ne. 0) then
             subinfo ='getting radial distance for backgrnd calc.'
             call wterrm(subname, version,subinfo)
             errflg=1
             return
         endif
      endif

c ----------------- type of error -------------------------
      status = 0
      call uclgst('error', error,status)
      if (status .ne. 0) then
          subinfo ='getting type of error calculation'
          call wterrm(subname, version,subinfo)
          errflg=1
          return
      endif
   
      call crmvlbk(error)
      call ftupch (error)

      if(error(1:2) .eq. '  ') then
         error='POISS-1'
      endif
  
c     if error is neither of Gauss, Poiss-1,2,3, then set it to Poiss1

      if ((error(1:5) .ne. 'GAUSS') .or. (error(1:7) .ne. 'POISS-1') 
     >     .or. (error(1:7) .ne. 'POISS-2') .or. 
     >     (error(1:7) .ne. 'POISS-3')) then
          
           error='POISS-1'
           subinfo='By default, error is set to POISS-1'
           call wtinfo(chatter, 0, 2,subinfo)
      endif

      
c ----------------- read in properr -----------------------

      status = 0
      call uclgsb('properr', properr, status)
      if (status .ne. 0) then
          subinfo = 'getting error propagation parameter'
          call wterrm(subname,version,fileinfo)
          properr=.true.
      endif

c ----------------- read in area_wgt -----------------------

      status = 0
      call uclgsb('areawgt', areawgt,status)
      if (status .ne. 0) then
          subinfo ='area wgt required or not'
          call wterrm(subname, version,subinfo)
          errflg=1
          return
      endif

c ----------------- read type of area_wgt --------------------
      if (areawgt) then
          subinfo='Two options are available for area weighting'
          call wtinfo(chatter,0,0,subinfo)
          subinfo='area weighting for the ENTIRE REGION(OPTION=1)' 
          call wtinfo(chatter,0,0,subinfo)
          subinfo= 'area weighting for  PART OF IT as fast method'
     >         // '(OPTION=2)'
          call wtinfo(chatter,0,0,subinfo)
          call uclgst('area_wgt_option',area_wgt_option,status)
          if (status .ne. 0) then
              subinfo = 'getting area_wgt option parameter'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
          endif

          if(area_wgt_option .ne. '1') then
             if(area_wgt_option .ne. '2') then
                subinfo='area_wgt_option no. should be entered'
                call wterrm(subname,version,subinfo)              
                errflg=1
                return
             endif
          endif  

c ----------------- read distance for area_wgt --------------

          if (area_wgt_option .eq. '2') then
              call uclgsr('wgt_rad',wgt_rad,status)
              if (status .ne. 0) then
                  subinfo = 'getting area wgt radius parameter'
                  call wterrm(subname,version,fileinfo)
                  errflg = 1
                  return
              endif
          endif         
      endif

c -------------- find out if region file is wanted or not -------------

      status = 0
      call uclgsb('region_pres', region_pres,status)
      if (status .ne. 0) then
          subinfo ='region file present or not'
          call wterrm(subname, version,subinfo)
          errflg=1
          return
      endif

c -------------------- get the region file --------------------
 
      if(region_pres) then

         n_ill = 0
         status = 0
         call uclgst('regfil', regfil, status)
         if (status .ne. 0) then
             subinfo = 'getting region file parameter !'
             call wterrm(subname,version,subinfo)
             errflg = 1
             return
         endif

         call crmvlbk(regfil)
         if (regfil(1:2) .eq. '  ') then
             subinfo='you replied to have region file'
             call wterrm(subname,version,subinfo)
             subinfo='so region filename has to be entered!'
             call wtinfo(chatter,0,3,subinfo)
             errflg = 1
             return
         endif

         call fcpars(regfil, filename, extnum, status)
         call crmvlbk(filename)
         n_ill = n_ill + 1
         ill_file(n_ill) = filename
         ext = .true.
         INQUIRE(FILE=filename, EXIST=ext)
         if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
             subinfo= 'region file does not exist !'
             call wterrm(subname,version,subinfo)
             fileinfo = 'filename : '//filename
             call wterrm(subname,version,fileinfo)
             errflg = 1
             return
         endif

      endif

c ----------------- read in clobber -----------------------

      status = 0
      call uclgsb('clobber', killit, status)
      if (status .ne. 0) then
          subinfo = 'getting clobber parameter'
          call wterrm(subname,version,fileinfo)
          killit =.false.
          status = 0
      endif

c ----------------- chatter parameter ------------------------------

      status = 0
      call uclgsi('chatter',chatter,status)
      if (status .ne. 0) then
          subinfo = 'getting chatter parameter'
          call wterrm(subname,version,fileinfo)
          chatter = 9
          status = 0
      endif

      errflg = 0
      return
      end
c ----------------------------------------------------------------------
c                       END OF EEF_GP
c ----------------------------------------------------------------------

*+XRPSF_OPEN

      subroutine open_file(infil,iunit,nevt,pix_size,telescope,
     >                      instrument,xcol,ycol,picol,
     >                      chatter,errflg)


c ------------------ description ---------------------------------------
c
c This subroutine open the data input file for the XRPSF task. It reads
c the column numbers for X,Y and PI column numbers as xcol,ycol,picol 
c from EVTRAT FITS file 
c
c ------------------- variables ----------------------------------------
 
      implicit none
      character*(*) infil
      integer xcol,ycol,picol
      integer nevt,chatter,errflg

c -------------------- variable directory ------------------------------

c --------------------------- called routines ---------------------------
c mvext  : moves to desired extension
C---------------- authors/modification ---------------------------------
c
c Banashree Mitra Seifert (Jan. 24, 1996)
c
c-----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.0')
      character(11) subname
*-
c ------------------- internal variables -------------------------------
      integer maxevt
      integer iunit,nsearch, status,next(50),ninstr
      character*(*) telescope,instrument
      character(100) subinfo,comm
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(9,50)
      integer colnum,maxcol,ncols
      integer phacol,timecol
      integer snum
      parameter (maxcol=10)
      character(8) ttype(maxcol)
      character(16) tform(maxcol)
      character(6) pixcol
      real pix_size
      logical foundcol


c ---------------------------------------------------------------------

       subname ='xrpsf_open'
       subinfo = 'using '//subname//'Ver '//version
       call wtinfo(chatter,10,1,subinfo)

       maxevt = 300000
c ----------- opening evt file -----------------------------------------

       status = 0
       extname='EVENTS'
       ninstr = 2
       instr(1) = 'EVENTS'
       instr(2) = 'ACCEPTED'
       nsearch = 50

       call mvext (0, infil, iunit, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, status, chatter)
       if (status .ne. 0) then
           subinfo='opening the input file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit,status) 
           subinfo = 'closing input EVT file'
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       else
           subinfo='moved to EVENTS extension'
           call wtinfo(chatter,10,1,subinfo)
       endif

c ----------- read the EVENT file ----------------------------------

c read naxis2 (no. of events)

       status = 0
       call ftgkyj(iunit,'NAXIS2',nevt,comm,status)
       subinfo = 'reading NAXIS2'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif

       write(subinfo,'(a,i10)')'number of records found = ',nevt
       call wtinfo(chatter,10,1,subinfo)

c read TELESCOPE keyword

       status = 0
       call ftgkys(iunit,'TELESCOP',telescope,comm,status)
       subinfo = 'reading telescope'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif

c read INSTRUMENT keyword

       status = 0
       call ftgkys(iunit,'INSTRUME',instrument,comm,status)
       subinfo = 'reading instrument'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif

c ----  check to find x and y column ----------------------------

c read ttype (columns)

       status = 0
       call ftgkns(iunit,'TTYPE',snum,maxcol,ttype,ncols,status)
       subinfo = 'reading column names'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 5
           return
       endif
 

c read tform (format of columns)

       call ftgkns(iunit,'TFORM',snum,maxcol,tform,ncols,status)
       subinfo = 'reading column format'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 5
           return
       endif

c -------------------------------------------------------------
c locate the column no. of the X,Y,PHA,PI,TIME,DX,Dy column no.
c for X the column=xcol, for Y =ycol, for pha = phacol, 
c for pi=picol, for time=timecol, for DX=dxcol, for DY=dycol
c ------------------------------------------------------------
   
      foundcol=.true.
      status = 0 
      call ftgcno(iunit,.false.,'X',colnum,status)
      if (status .ne. 0) then
          foundcol=.false.
      endif
      if (.not. foundcol) then
          subinfo='X column not present in EVENT extension'
          call wterrm(subname,version,subinfo)
          errflg = 2
          return
      endif
      xcol = colnum 

      call ftgcno(iunit,.false.,'Y',colnum,status)
      if (status .ne. 0) then
          foundcol=.false.
      endif
      if (.not. foundcol) then
          subinfo='Y column not present in EVENT extension'
          call wterrm(subname,version,subinfo)
          errflg = 2
          return
      endif
      ycol = colnum

c read pixelsize keyword (TCDLT1) 

      write(pixcol,'(a,i1)')'TCDLT',xcol 
       status = 0
       call ftgkye(iunit,pixcol,pix_size,comm,status)
       write(subinfo,'(a,a)')'reading pixel size ',pixcol 
       call wtfwrn(subname,version,chatter,9,status,subinfo)
       if (status .ne. 0) then
          subinfo='looking for CDELT1'
          call wtinfo(chatter,9,2,subinfo)
          status=0
          write(pixcol,'(a,i1)')'CDELT',xcol 
          call ftgkye(iunit,pixcol,pix_size,comm,status)
          if (status .eq. 0) then
              subinfo='got CDELT1 keyword'
              call wtinfo(chatter,9,2,subinfo)
          endif
          write(subinfo,'(a,a)')'reading pixel size ',pixcol 
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
       endif

c ---- if pix_size is -ve then make it positive ---------------------

       if (pix_size .lt. 0.) then
           pix_size = -pix_size
       endif
c -------------------------------------------------------------------

c read PI channel column no

      call ftgcno(iunit,.false.,'PI',colnum,status)
      if (status .ne. 0) then
          foundcol=.false.
      endif
      if (.not. foundcol) then
          subinfo='PI column not present in EVENT extension'
          call wterrm(subname,version,subinfo)
          errflg = 2
          return
      endif
      picol = colnum

c read PHA channel column no

      call ftgcno(iunit,.false.,'PHA',colnum,status)
      if (status .ne. 0) then
          foundcol=.false.
      endif
      if (.not. foundcol) then
          subinfo='PHA column not present in EVENT extension'
          call wterrm(subname,version,subinfo)
          errflg = 2
          return
      endif
      phacol = colnum

C	NOTE: For HRI data the PI column contains only zeros. Therefore the 
C	      following fix is done. (Srilal 01/11/97)
C
      if (instrument .eq. 'HRI') then
      picol = phacol
      endif


c read TIME channel column no

      call ftgcno(iunit,.false.,'TIME',colnum,status)
      if (status .ne. 0) then
          foundcol=.false.
      endif
      if (.not. foundcol) then
          subinfo='TIME column not present in EVENT extension'
          call wterrm(subname,version,subinfo)
          errflg = 2
          return
      endif
      timecol = colnum

c      call ftgcno(iunit,.false.,'DETX',colnum,status)
c      if (status .ne. 0) then
c          foundcol=.false.
c      endif
c      if (.not. foundcol) then
c          subinfo='DETX column not present in EVENT extension'
c          call wterrm(subname,version,subinfo)
c          subinfo='Looking for DX column'
c          call wterrm(subname,version,subinfo)
cc
c          status=0
c          call ftgcno(iunit,.false.,'DX',colnum,status)
c          if (status .ne. 0) then
c              foundcol=.false.
c          endif
c          if (.not. foundcol) then
c              subinfo='DX column not present in EVENT extension'
c              call wterrm(subname,version,subinfo)
c              errflg = 2
c              return
c          endif
c      endif
c      dxcol = colnum
c
c      call ftgcno(iunit,.false.,'DETY',colnum,status)
c      if (status .ne. 0) then
c          foundcol=.false.
c      endif
c      if (.not. foundcol) then
c          subinfo='DETY column not present in EVENT extension'
c          call wterrm(subname,version,subinfo)
c          subinfo='Looking for DX column'
c          call wterrm(subname,version,subinfo)
c
c          status=0
c          call ftgcno(iunit,.false.,'DY',colnum,status)
c          if (status .ne. 0) then
c              foundcol=.false.
c          endif
c          if (.not. foundcol) then
c              subinfo='DY column not present in EVENT extension'
c              call wterrm(subname,version,subinfo)
c              errflg = 2
c              return
c          endif
c      endif
c      dycol = colnum

      errflg = 0
      return
      end
c ------------------------------------------------------------------
c                   end of opening input file
c ------------------------------------------------------------------

*+XRPSF_RDAT

      subroutine xrpsf_rdat(infil,iunit,xcol,ycol,picol,x,y,pi,frow,
     >                      chatter,errflg)

c ------------------ description ---------------------------------------
c
c This subroutine reads the data inputs for the XRPSF task. It reads
c the EVTRAT FITS file for data row by row 
c
c ------------------- variables ----------------------------------------
 
      implicit none
      character*(*) infil
      integer xcol,ycol,picol,x,y,pi
      integer iunit,frow,chatter,errflg

c -------------------- variable directory ------------------------------


c --------------------------- called routines ---------------------------

C---------------- authors/modification ---------------------------------
c
c Banashree Mitra Seifert (Jan. 24, 1996)
c
c-----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.0')
      character(11) subname
*-
c ------------------- internal variables -------------------------------
      character(50) subinfo
      integer felem,status,nullval
      logical anyflg

c ---------------------------------------------------------------------

       subname ='xrpsf_rdat'

c reading X,Y,PI columns

ccc       frow =1
       felem =1
       nullval =0
       status=0
       call ftgcvj(iunit,xcol,frow,felem,1,nullval,x,anyflg,status)
       write(subinfo,'(a,i12)')'reading x in row = ',frow
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 3
           return
       endif

       call ftgcvj(iunit,ycol,frow,felem,1,nullval,y,anyflg,status)
       write(subinfo,'(a,i12)')'reading y in row = ',frow
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 3
           return
       endif
         
       call ftgcvj(iunit,picol,frow,felem,1,nullval,pi,anyflg,status)
       write(subinfo,'(a,i12)')'reading pi in row = ',frow
       if (status .ne. 0) then
           call wtferr(subname,version,status,subinfo)
           errflg = 3
           return
       endif

       errflg = 0
       return
       end
c --------------------------------------------------------------------
c                  end of xrpsf_rdat
c --------------------------------------------------------------------
 
*+XRPSF_CALC
 
      subroutine xrpsf_calc(x_in,y_in,x,y,pi,rad_length_sq,
     >                      rad_hi_sq,nbins,chanmin,chanmax,
     >                      bck_lo_sq,bck_hi_sq,pix_size_min,npi,
     >                      bck,region_pres,
     >                      shape_no,shape,x_center,y_center,
     >                      xmin,xmax,ymin,ymax,radiusmin,
     >                      radiusmax,radius,x1,x2,y1,y2,twice_a,minus,
     >                      minus_cts,chatter,errflg)

c --------------- description of xrpsf_calc -----------------------------
c
c This subroutine calculates whether the x and y coordinate read from
c the input file is inside the radius described by the user.
c If inside the area, then count it as 1 in the particular bin.
c
c --------------- variables --------------------------------------------

       integer x,y,pi
       real x_in,y_in
       real rad_hi_sq(*),pix_size_min,rad_length_sq
       real bck,bck_lo_sq,bck_hi_sq
       integer nbins, npi(*),chanmin,chanmax
       integer chatter,errflg
       character*(*) shape(*)
       integer shape_no,minus_cts(*)
       real x_center(*),y_center(*),xmin(*),xmax(*),ymin(*),ymax(*)
       real radiusmin(*),radiusmax(*),radius(*)
       real twice_a(*),x1(*),y1(*),x2(*),y2(*)
       logical region_pres,minus

c -------------------- variable directory -----------------------------
c
c rad_length  int  input  radial length from centroid to outer boundary
c nbins       int  input  number of radial bins
c bin_length  int         radial length of each bin
c rad_lo      real        inner radius of each bin
c rad_hi     real        outer radius of each bin
c
c --------------------- called routines --------------------------------
c do_region   : gets the region parameters for region calculations
c --------------------- internal variables -----------------------------
      real xsq,ysq,rsq

c --------------- authors/modification ---------------------------------
c
c Banashree Mitra Seifert (Jan, 1995)
c
c-----------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.0.0')
      character(11) subname
*-

      subname = 'xrpsf_calc'
c      subinfo = 'using '//subname//'Ver '//version
c      call wtinfo(chatter,10,1,subinfo)
c ---------------------------------------------------------------------+
      xsq = (x-x_in)*(x-x_in)*(pix_size_min)*(pix_size_min) 
      ysq = (y-y_in)*(y-y_in)*(pix_size_min)*(pix_size_min)
      rsq = xsq + ysq
c ---------------------------------------------------------------------+
      i=1
      do while(i .le. nbins)
         npi(i) = 0
         i=i+1
      enddo
 
      errflg=0
 
      if (rsq .le. rad_length_sq) then
          if (rsq .gt. bck_lo_sq ) then
              if((pi .le. chanmax) .and. (pi .ge. chanmin)) then
                  bck = 1
              endif
          endif
          do i=1,nbins
             if (rsq .le. rad_hi_sq(i)) then
                 if((pi .le. chanmax) .and. (pi .ge. chanmin)) then
                     npi(i) = 1
                     if(region_pres) then
                        call do_region(x,y,shape_no,shape,x_center,
     >                          y_center,pix_size_min,xmin,xmax,
     >                          ymin,ymax,radiusmin,radiusmax,radius,
     >                          x1,x2,y1,y2,twice_a,minus,
     >                          chatter,errflg)

                        if(minus) then
                           npi(i)=0
                           minus_cts(i) = minus_cts(i) + 1
                        endif
                     endif
                     return
                 endif
             endif
          enddo

      endif

      errflg = 0
      return
      end

c --------------------------------------------------------------------- 
c                 end of xrpsf_calc
c ---------------------------------------------------------------------

*+XRPSF_CONV

      subroutine xrpsf_conv(rad_lo,rad_hi,sumevt,area,bckgrnd,bck_area,
     >                      accept,reject,minus_cts,minus_bck,
     >                      nbins,pix_size_min,
     >                      error,cts,cts_err,area_wgt,sumrcts,
     >                      sumtcts,chatter,errflg)

c ------------------- description -------------------------------------
c This routine calculates the counts/arcmin^2 from raw counts/pixel
c for both -- PSF and background   
c
c ------------------- variables ---------------------------------------
      implicit none
      character*(*) error
      integer accept(*),reject(*) 
      integer nbins
      integer minus_bck,minus_cts(*)
      real rad_hi(*),rad_lo(*),sumrcts,sumtcts
      real sumevt(*),area(nbins,1,1),area_wgt(nbins,1,1)
      real bckgrnd,bck_area,pix_size_min
      real cts(nbins,1,1),cts_err(nbins,1,1)
      integer chatter,errflg

c ------------------- internal variables ----------------------------
      character(100) subinfo
      real total_counts,minus_bckarea,active_bckarea
      real calcpois,calcpois2,calcpois3
      integer i

c ------------------- variable definitions ---------------------------
c sumevt     real input  no. of events in each bin
c accept     int  input  no. of accepted events
c reject     int  input  no. of rejected events
c bckgrnd    real input  no. of background counts in back_area
c area       real input  area of the annulus of each bin in RPSF
c back_area  real input  area of the annulus of the background  
c cts        real output counts in arc-min^2
c cts_err    real output error in counts in arc-min^2
c
c ------------------ authors/modifications ---------------------------  
c Banashree Mitra Seifert (1996 Jan) 1.0.0:
c --------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.0')
      character(11) subname
*-
      subname ='xrpsf_conv'
      subinfo  = 'using '//subname//'Ver '//version
      call wtinfo(chatter,10,1,subinfo)
 
c calculate the background = background/area

      if(bck_area .eq. 0.) then
         bckgrnd=0.0
      else
         minus_bckarea= minus_bck*pix_size_min*pix_size_min
         active_bckarea=bck_area - minus_bckarea
         bckgrnd = bckgrnd/active_bckarea
      endif


      i=1
      do while (i .le. nbins)
         area(i,1,1) = area(i,1,1)-
     >                    (minus_cts(i)*pix_size_min*pix_size_min)
         i=i+1
      enddo

      sumrcts = 0.0
      sumtcts = 0.0
      i=1
      do while(i .le. nbins)
         cts(i,1,1) = float(accept(i))/area(i,1,1)
         total_counts = cts(i,1,1) - bckgrnd 
         sumrcts = sumrcts + total_counts * area(i,1,1)
         sumtcts = sumtcts + total_counts * area(i,1,1)/
     >             area_wgt(i,1,1)

         if (accept(i) .eq. 0) then
             cts_err(i,1,1) = 0.
             i=i+1
         else
             if (error(1:5) .eq. 'GAUSS') then
                 cts_err(i,1,1) = 1./sqrt(float(accept(i))) 
                 cts_err(i,1,1) = cts_err(i,1,1)/area(i,1,1)
             elseif (error(1:7) .eq. 'POISS-1') then
                 cts_err(i,1,1) = calcpois(accept(i)) 
                 cts_err(i,1,1) = cts_err(i,1,1)/area(i,1,1)
             elseif (error(1:7) .eq. 'POISS-2') then
                 cts_err(i,1,1) = calcpois2(accept(i)) 
                 cts_err(i,1,1) = cts_err(i,1,1)/area(i,1,1)
             elseif (error(1:7) .eq. 'POISS-3') then
                 cts_err(i,1,1) = calcpois3(accept(i)) 
                 cts_err(i,1,1) = cts_err(i,1,1)/area(i,1,1)
             else
                 subinfo='type of error calculation is not defined:'
     >                    //error
                 call wterrm(subname,version,subinfo)
                 errflg=1
                 return
             endif
         i=i+1
         endif
      enddo
      
      errflg = 0
      return
      end
c -------------------------------------------------------------------
c                         end of xrpsf_conv
c -------------------------------------------------------------------


*+XRPSF_WT
c ---------------------------------------------------------------------+
      subroutine xrpsf_wt(outfil,infil,xrpsf_ver,nrad,rad_lo,
     >                   rad_hi,cts,cts_err,area_wgt,pix_size,
     >                   nevt,maxrad,maxtheta,telescope,instrument,
     >                   bckgrnd,chanmin,chanmax,
     >                   bck_per_pixel,
     >                   sumrcts,sumtcts,errflg,chatter,killit)

c ----------------- DESCRIPTION -------------------------------------
c
c This routine writes to FITS output file in CALRPSF format. 
c It opens the FITS file and passes the data to WTRPF1
c which writes the data in the form of a BINARY TABLE extension. 
c Finally it closes the FITS file.
c
c ---------------------- VARIABLES --------------------------------

      IMPLICIT NONE
      character*(*) infil,outfil
      character*(*) telescope,instrument
      character*(*) xrpsf_ver
      integer maxrad,nrad,nevt,chatter,maxtheta,chanmin,chanmax
      real cts(maxrad,maxtheta,*), cts_err(maxrad,maxtheta,*)
      real bckgrnd,bck_per_pixel,sumrcts,pix_size,sumtcts
      real rad_lo(*),rad_hi(*),area_wgt(maxrad,maxtheta,*)
      integer errflg
      logical killit

c -------------------------- INTERNAL VARIABLES ----------------------

      integer maxhist,maxcomm
      parameter (maxhist = 10)
      parameter (maxcomm = 10)
      character(80) hist(maxhist),comms(maxcomm),subinfo
      integer ounit,nk_hist,status,nk_comm
      real theta_lo(1), theta_hi(1),energ_lo(1),energ_hi(1)
      integer ntheta,nenerg
      character(8) extname
      character(16) hduclas3,taskname
      character(16) radunit,thetaunit,energunit,rpsfunit
      logical qarea,qerror

c ------------------------- VARIABLE DIRECTORY ------------------------
c
c Arguments ...
c
c outfil     char   : Output filename
c infil      char   : Input filename
c stver      char   : Main program version
c maxrad     int    : Array dimensions for radial profile data
c cts   real   : Array of radial profile in counts per arcmin^2
c cts_err   real   : Array of errors on cts (in arcmin^2)
c rad_lo     real   : Array of lower edge of radial bins (in arcmin)
c rad_hi     real   : Array of upper edge of radial bins (in arcmin)
c n_pix      int    : Array of No of pixels in bin
c Pixsize    real   : Pixsize in degrees
c nevt       int    : Counter for No. radial profile values
c telstw     char   : Mission/Telescope name
c inststw    char   : Instrument/Detector name
c chatter    int    : Chattines flag (>20 noisy)
c
c Internals ...
c
c ounit      int    : Fortran file unit number
c maxhist    int    : Dimension for history comment array
c maxcomm    int    : Dimension for comments array
c nk_comm    int    : No. of lines of comments
c nk_hist    int    : No of lines of history comments
c status     int    : Error flag
c hist       char   : History comment array
c comms      char   : Comment array
c version    char   : Subroutine version
c errstr     char   : Error string for this routine
c errmess    char   : Error message obtained from FITSIO call
c
c --------------------- CALLED ROUTINES ---------------------------------
c
c subroutine OP_NA        : (FITSIO) Opens FITS file,writes header and null
c                            primary array
c subroutine FTGERR       : (FITSIO) Obtains appropriate error text
c subroutine WTRPF1       :  Writes data extension
c subroutine FTCLOS       : (FITSIO) Closes FITS file
c subroutine WT_FERRMSG   :  Writes Error text if required
c
c ---------------- COMPILATION AND LINKING ----------------
c
c Link with FITSIO and FTOOLS
c
c ---------------- AUTHORS/MODIFICATION HISTORY ----------------
c
c Banashree Mitra Seifert (1996 Jan) 1.0.0:
c
c -----------------------------------------------------------------
        character(5) version
        parameter ( version = '1.0.0' )
        character(9) subname
*-
c --------------------------------------------------------------
        subname = 'extrpsf_wt'
        subinfo = 'using '//subname//'Ver '//version
        call wtinfo(chatter,10,1,subinfo)

c -------------------- OPENING FITS FILE ------------------------
        call cgetlun(ounit)
        status = 0
        call opnpa(outfil,chatter,ounit,killit,status)
        subinfo = ' setting up primary header'
        call wtfwrn(subname,version,chatter,15,status,subinfo)
        if (status .NE. 0) then
            goto 200 
        endif

c ----------------- SETTING UP HISTORY COMMENT FIELDS ----------------

        nk_hist = 2
        hist(1) = 'extrpsf converts from EVENT -> RPSF format'
        hist(2) = 'EVENT FILE : '//infil
        nk_comm = 0

c ------- SETTING UP UNDEFINED ARRAYS ! THETA AND ENERG VALUES -------

        theta_lo(1) = -99
        theta_hi(1) = -99
        energ_lo(1) = -99
        energ_hi(1) = -99
        ntheta = 1
        nenerg = 1

c ---------------------- WRITING DATA --------------------------------

        radunit = 'arcmin'
        thetaunit = 'arcmin'
        energunit = 'KeV'
        rpsfunit = 'count/arcmin**2'
        qarea = .true.
        qerror = .true.
        errflg = 0
        extname = 'OBS RPSF'
        if (bckgrnd .eq. 0.0) then 
            hduclas3 = 'NET'
        else
            hduclas3 = 'TOTAL'
        endif

        call wtrpf1(ounit,extname,hduclas3,nrad,rad_lo,rad_hi,
     >              radunit,ntheta,theta_lo,theta_hi,thetaunit,
     >              nenerg,energ_lo,energ_hi,energunit,cts,
     >              qerror,cts_err,rpsfunit,qarea,area_wgt,hist,
     >              nk_hist,comms,nk_comm,telescope,instrument,maxrad,
     >              maxtheta,errflg,chatter)
        if (errflg .ne. 0) then
            goto 200
        endif

c ----------------------- CREATOR KEYWORD ----------------------------

        taskname = 'EXTRPSF'//' '//xrpsf_ver
        status = 0
        call ftpkys(ounit,'CREATOR',taskname,
     >              's/w task which wrote this dataset',
     >               status)
        subinfo = ' writing CREATOR keyword'
        call wtfwrn(subname,version,chatter,15,status,subinfo)

c --------------- WRITE PIXELSIZE ----------------------------------

200     status = 0
        call ftpkye(ounit,'PIXSIZE',pix_size,8,'In decimal degrees',
     >              status)
        subinfo = ' writing PIXSIZE keyword'
        call wtferr(subname,version,status,subinfo)

c ------------------ WRITE BACKGROUND --------------------
        
        status = 0
        call ftpkye(ounit,'BACKGRND',bck_per_pixel,8,
     >              'In counts per pixel', status)      
        subinfo = ' writing BACKGRND keyword'
        call wtferr(subname,version,status,subinfo)

c ------------ WRITE CHANMIN and CHANMAX for ROSAT PSPC --------------

        status = 0
        call ftpkyj(ounit,'CHANMIN',chanmin,
     >            'Minimum PI channel for image',status)
        subinfo = ' writing CHANMIN keyword'
        call wtferr(subname,version,status,subinfo)
        status = 0
        call ftpkyj(ounit,'CHANMAX',chanmax,
     >            'Maximum PI channel for image',status)
        subinfo = ' writing CHANMAX keyword'
        call wtferr(subname,version,status,subinfo)

c --------------------- WRITE SUMRCTS -------------------------------

        status = 0
        call ftpkye(ounit,'SUMRCTS',sumrcts,8,
     >             'Sum of source counts under profile',status)
        subinfo = ' writing SUMRCTS'
        call wtferr(subname,version,status,subinfo)

c ------------------- WRITE SUMTCTS -----------------------------------+

        status = 0
        call ftpkye(ounit,'SUMTCTS',sumtcts,8,
     >              'Theoretical Sum source counts(corr for '
     >              //'excl regions)',status)
        subinfo = ' writing SUMTCTS'
        call wtferr(subname,version,status,subinfo)

c ----------------- CLOSE FITS FILE ---------------------------------

        status = 0
        call ftclos(ounit,status)
        subinfo='closing output file'
        call wtferr(subname,version,status,subinfo)
        call cfrelun(ounit)

        errflg = 0
        return
        end
c ---------------------------------------------------------------------+
c              END OF SUBROUTINE RPSF_WT                             
c ---------------------------------------------------------------------+

*+CELS2XY

      subroutine cels2xy(infil,celsx,celsy,xcol,ycol,x_in,y_in,
     >                 chatter,errflg)  

c ------------------ description ---------------------------------------
c
c This subroutine converts the sky coordinate ,i.e, RA and DEC, to
c pixel coordinates in -SIN, -TAN, -ARC, -NCP, -GLS, -MER, -AIT projections.
c returns   0 = good,
c         501 = angle too large for projection;
c         502 = bad values
c         503 = ???undocumented error - looks like an underflow???
c
c ------------------- internal variables --------------------------------

      implicit none
      character*(*) infil
      real celsx,celsy,x_in,y_in
      integer xcol,ycol
      integer chatter,errflg

c -------------------- variable directory ------------------------------
      character(4) type
      character(8) ctype
      character(100) subinfo,comm
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(9,50)
      character(6) kyword
      real tcrvl1,tcrvl2,tcrpix1,tcrpix2,tcdlt1,tcdlt2,tcrot
      integer iunit,nsearch, status,next(50),ninstr
      double precision xin,yin
   
c ------------------ routines called ------------------------------
c mvext   : move to desired location by extension no/name
c---------------- authors/modification ---------------------------------
c
c Banashree M Seifert (Jan. 24, 1996)
c
c Banashree M Seifert (Sept. 24, 1996)
c     . error while writing TCDELT(ycol) fixed
c     . some error while calling wtinfo fixed
c-----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.0')
      character(14) subname
*-
          
       subname ='xrpsf_cels2xy'
       subinfo = 'using '//subname//'Ver '//version
       call wtinfo(chatter,10,1,subinfo)

c ------------------------------------------------------------------- 
      status = 0
      extname = 'EVENTS'
      ninstr = 2
      instr(1) = 'EVENTS'
      instr(2) = 'ACCEPTED'
      nsearch = 50

      call mvext (0, infil, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, status, chatter)
      if (status .ne. 0) then
          subinfo='opening the input file'
          call wtferr(subname,version,status,subinfo)
          call ftclos(iunit,status)
          subinfo = 'closing input EVT file'
          call wtferr(subname,version,status,subinfo)
          errflg=1
          return
      endif

c read x-reference coordinate 

      write(kyword,'(a,i1)')'TCRVL',xcol
      status = 0
      call ftgkye(iunit,kyword,tcrvl1,comm,status)
      write(subinfo,'(a,a)')'reading x-reference coordinate ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'CRVAL',xcol
          write(subinfo,'(a,a)')'looking for keyword ',
     >                          kyword
          call wtinfo(chatter,9,1,subinfo)
          status=0
          call ftgkye(iunit,kyword,tcrvl1,comm,status)
          write(subinfo,'(a,a)')'reading x-reference coordinate ',
     >                          kyword
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
      endif

c read y-reference coordinate 

      write(kyword,'(a,i1)')'TCRVL',ycol
      status = 0
      call ftgkye(iunit,kyword,tcrvl2,comm,status)
      write(subinfo,'(a,a)')'reading y-reference coordinate ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'CRVAL',ycol
          write(subinfo,'(a,a)')'looking for keyword ',
     >                          kyword
          call wtinfo(chatter,9,1,subinfo)
          status=0
          call ftgkye(iunit,kyword,tcrvl2,comm,status)
          write(subinfo,'(a,a)')'reading y-reference coordinate ',
     >                          kyword
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
      endif

c      xref = dbl(tcrvl1)
c      yref = dbl(tcrvl2)

c read x-reference pixel 

      write(kyword,'(a,i1)')'TCRPX',xcol
      status = 0
      call ftgkye(iunit,kyword,tcrpix1,comm,status)
      write(subinfo,'(a,a)')'reading x-reference pixel ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'CRPIX',xcol
          write(subinfo,'(a,a)')'looking for keyword ',
     >                          kyword
          call wtinfo(chatter,9,1,subinfo)
          status=0
          call ftgkye(iunit,kyword,tcrpix1,comm,status)
          write(subinfo,'(a,a)')'reading x-reference pixel ',kyword
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
      endif

c read y-reference pixel 

      write(kyword,'(a,i1)')'TCRPX',ycol
      status = 0
      call ftgkye(iunit,kyword,tcrpix2,comm,status)
      write(subinfo,'(a,a)')'reading y-reference pixel ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'CRPIX',ycol
          write(subinfo,'(a,a)')'looking for keyword ',
     >                          kyword
          call wtinfo(chatter,9,1,subinfo)
          status=0
          call ftgkye(iunit,kyword,tcrpix2,comm,status)
          write(subinfo,'(a,a)')'reading y-reference pixel ',kyword
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
      endif
 
c      xrefpix = dbl(tcrpix1)
c      yrefpix = dbl(tcrpix2)

c read x-coordinate increment

      write(kyword,'(a,i1)')'TCDLT',xcol
      status = 0
      call ftgkye(iunit,kyword,tcdlt1,comm,status)
      write(subinfo,'(a,a)')'reading x-increment ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'CDELT',xcol
          write(subinfo,'(a,a)')'looking for keyword ',
     >                          kyword
          call wtinfo(chatter,9,1,subinfo)
          status=0
          call ftgkye(iunit,kyword,tcdlt1,comm,status)
          write(subinfo,'(a,a)')'reading x-increment ',kyword
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
      endif

c read y-coordinate increment

      write(kyword,'(a,i1)')'TCDLT',ycol
      status = 0
      call ftgkye(iunit,kyword,tcdlt2,comm,status)
      write(subinfo,'(a,a)')'reading y-increment ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'CDELT',ycol
          write(subinfo,'(a,a)')'looking for keyword ',
     >                          kyword
          call wtinfo(chatter,9,1,subinfo)
          status=0
          call ftgkye(iunit,kyword,tcdlt2,comm,status)
          write(subinfo,'(a,a)')'reading y-increment ',kyword
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
      endif

c      xinc = dbl(tcdlt1)
c      yinc = dbl(tcdlt2)

c read type of projection keyword

      write(kyword,'(a,i1)')'TCTYP',xcol
      status = 0
      call ftgkys(iunit,kyword,ctype,comm,status)
      write(subinfo,'(a,a)')'reading type of projection ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'CTYPE',xcol
          write(subinfo,'(a,a)')'looking for keyword ',
     >                          kyword
          call wtinfo(chatter,9,1,subinfo)
          status=0
          call ftgkys(iunit,kyword,ctype,comm,status)
          subinfo = 'reading type of projection CTYPE1'
          call wtferr(subname,version,status,subinfo)
          if (status .ne. 0) then
              errflg = 4
              return
          endif
      endif

      call ftupch(ctype)
      type = ctype(5:8)

c read rotation keyword

      write(kyword,'(a,i1)')'TCROT',xcol
      status = 0
      call ftgkye(iunit,kyword,tcrot,comm,status)
      write(subinfo,'(a,a)')'reading rotation in degrees ',kyword
      call wtfwrn (subname,version,chatter,9,status,subinfo)
      if (status .ne. 0) then
          write(kyword,'(a,i1)')'TCROT',ycol
          status = 0
          call ftgkye(iunit,kyword,tcrot,comm,status)
          write(subinfo,'(a,a)')'reading rotation in degrees ',
     >                           kyword
          call wtfwrn (subname,version,chatter,9,status,subinfo)
      
          if (status .ne. 0) then
              write(kyword,'(a,i1)')'CROTA',xcol
              write(subinfo,'(a,a)')'Looking for keyword ',kyword
              call wtinfo(chatter,9,1,subinfo)
              status=0
              call ftgkye(iunit,kyword,tcrot,comm,status)
              if(status .ne. 0) then
                 write(kyword,'(a,i1)')'CROTA',ycol
                 write(subinfo,'(a,a)')'Looking for keyword ',kyword
                 call wtinfo(chatter,9,1,subinfo)
                 status=0
                 call ftgkye(iunit,kyword,tcrot,comm,status)
                 if (status .ne. 0) then
                     subinfo='rotation keyword is not found'
                     call wtinfo(chatter,9,0,subinfo)
                     subinfo='Rotation is assumed to be 0 degree'
                     call wtinfo(chatter,9,0,subinfo)
                     tcrot=0.0000E0
                 endif
              endif
          endif
      endif

c      rot = dbl(tcrot)

c ---- converting from RA/DEC to pixels

      status = 0
 
      call ftxypx(dble(celsx),dble(celsy),dble(tcrvl1),dble(tcrvl2),
     >            dble(tcrpix1),dble(tcrpix2),dble(abs(tcdlt1)),
     >            dble(tcdlt2),dble(tcrot),type,xin,yin,status)
 
      if (status .ne. 0) then
          subinfo = 'returning from call to ftxypx'
          call wterrm(subname,version,subinfo)
          errflg = 4
          return
      endif

      x_in = sngl(xin)
      y_in = sngl(yin)

c ----- closing the file -----------------------------
      status = 0
      call ftclos(iunit,status)
      subinfo= 'closing input event file'
      call wtferr(subname,version,status,subinfo)

      call cfrelun(iunit)

      errflg = 0
      return
      end

c ---------------------------------------------------------------------
c                        end of cels2xy
c ---------------------------------------------------------------------

*+CALC_AREAWGT

      subroutine calc_areawgt_all(x_in,y_in,rad_length,rad_hi_sq,
     >                            nbins,pix_size_min,area,areawgt,
     >                            area_wgt,chatter,errflg)

c -------------------------------------------------------------------
c This routine calculates the area_wgt factor which is basically
c calculation of the no. of pixels supposed to be in one annulus 
c since 
c
c                   total pixel of the anulus * area of each pixel
c       area_wgt = --------------------------------------------------
c                                area of the anulus
c -------------------------------------------------------------------

      implicit none
      logical areawgt
      integer nbins
      real x_in,y_in,rad_hi_sq(*),rad_length,pix_size_min 
      real area(nbins,1,1),area_wgt(nbins,1,1)
      integer chatter,errflg

c ------------------ internal variables --------------------------
      character(80) subinfo
      integer maxbins
      parameter (maxbins=100)
      real pix_rad_length,pix_bin_length
      real pix_rad_lo(maxbins),pix_rad_hi(maxbins)
      real pix_rad_lo_sq(maxbins),pix_rad_hi_sq(maxbins)
      real x_min,x_max,y_min,y_max,xsq,ysq
      real theo_counts(maxbins),cal_area(maxbins)
      real xtemp,ytemp,rsq,pix_size_min_sq 
      integer i,k,center_x,center_y

c -------------------- variable explanation ---------------------------
c center_x       int   integer form of x_in
c center_y       int   integer form of y_in
c pix_rad_length real  total radial length in pixels
c pix_bin_length real  length of each bin
c x_min,xmax and real  the four corners of the square surrounding the
c ymin,ymax            circular region
c
c --------------- AUTHORS/MODIFICATION ---------------------------------
c
c Banashree Mitra Seifert (Feb., 1996)
c
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '1.0.0')
        character(17) subname
      
*-
        subname = 'calc_areawgt_all'
        subinfo  = 'using '//subname//'Ver '//version
        call wtinfo(chatter,10,1,subinfo)
c ------------------------------------------------------------------
    
      if(pix_size_min .lt. 0.) then
         pix_size_min = - pix_size_min
      endif

      pix_rad_length = rad_length/pix_size_min
      pix_bin_length = pix_rad_length/nbins

      center_x= int(x_in +0.5)
      center_y= int(y_in +0.5)
     
      x_min = center_x - pix_rad_length
      x_max = center_x + pix_rad_length
      y_min = center_y - pix_rad_length
      y_max = center_y + pix_rad_length 
      
      pix_rad_lo(1) = 0.
      pix_rad_hi(1) = pix_rad_lo(1) + pix_bin_length
      pix_rad_lo_sq(1) = 0.
      pix_rad_hi_sq(1) = pix_rad_hi(1) * pix_rad_hi(1)
      i = 2
      do while(i .le. nbins)
         pix_rad_lo(i) = pix_rad_hi(i-1)
         pix_rad_hi(i) = pix_rad_lo(i) + pix_bin_length
         pix_rad_lo_sq(i) = pix_rad_lo(i) * pix_rad_lo(i)
         pix_rad_hi_sq(i) = pix_rad_hi(i) * pix_rad_hi(i)
         i = i+1
      enddo

      pix_size_min_sq = pix_size_min*pix_size_min


      i=1
      do while(i .le. nbins) 
         theo_counts(i)= 0.
         i=i+1
      enddo

      xtemp = x_min - 1
      do while (xtemp .le. x_max)
         xtemp=xtemp+1
         xsq = (xtemp-center_x)**2
         ytemp = y_min - 1
         do while (ytemp .le. y_max)
            ytemp=ytemp+1
            ysq = (ytemp-center_y)**2
            rsq = xsq + ysq
            if(rsq .le. pix_rad_hi_sq(nbins)) then 
               do k=1,nbins
                  if(rsq .le. pix_rad_hi_sq(k)) then
                     theo_counts(k)= theo_counts(k) + 1
                     goto 2
                  endif
               enddo
            endif
 2       enddo
      enddo

      i=1
      do while(i .le. nbins) 
         cal_area(i) = theo_counts(i) * pix_size_min_sq
         area_wgt(i,1,1) = cal_area(i) / area(i,1,1)
         i=i+1
      enddo

      errflg = 0
      return
      end

c --------------------------------------------------------------------
c                        end of area_wgt calculation
c --------------------------------------------------------------------

*+CALC_RAD_BIN

       subroutine calc_rad_bin(rad_hi,rad_lo,bin_length,nbins,
     >                         rad_hi_sq,rad_lo_sq,
     >                         area,errflg,chatter)


      real rad_hi(*),rad_lo(*),rad_hi_sq(*),rad_lo_sq(*)
      real area(nbins,1,1),bin_length,pi
      integer errflg,chatter

c ----------------- internal variables --------------------------
      character(80) subinfo

c --------------- AUTHORS/MODIFICATION ---------------------------------
c
c Banashree Mitra Seifert (Feb., 1995)
c
c-----------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.0.0')
      character(13) subname
*-

      subname ='calc_rad_bin'
      subinfo  = 'using '//subname//'Ver '//version
      call wtinfo(chatter,10,1,subinfo)
 


      pi = 4.*atan(1.0)

      rad_lo(1) = 0.
      rad_hi(1) = rad_lo(1) + bin_length
      rad_lo_sq(1) = 0.
      rad_hi_sq(1) = rad_hi(1) * rad_hi(1)
      area(1,1,1) = pi* rad_hi_sq(1)

      i=2
      do while(i .le. nbins)
         rad_lo(i) = rad_hi(i-1)
         rad_hi(i) = rad_lo(i) + bin_length
         rad_lo_sq(i) = rad_lo(i) * rad_lo(i)
         rad_hi_sq(i) = rad_hi(i) * rad_hi(i)
         area(i,1,1) =pi*(rad_hi_sq(i) - rad_lo_sq(i))
         i=i+1
      enddo

      errflg = 0
      return
      end

c --------------------------------------------------------------------
c         end of calculation of radial bins in arcmin
c --------------------------------------------------------------------

*+CALC_AREAWGT_RAD

      subroutine calc_areawgt_rad(x_in,y_in,rad_length,rad_hi,
     >                          rad_hi_sq,
     >                         nbins,pix_size_min,area,areawgt,
     >                         wgt_rad,area_wgt,chatter,errflg )

c -------------------------------------------------------------------
c This routine does the area_wgt calculation using the outer radius 
c for calculation supplied by user. The area_wgt factor is basically 
c the ratio of pixels supposed to be in one annulus to the ratio
c of pixels found
c
c                   total pixel of the anulus * area of each pixel
c       area_wgt = --------------------------------------------------
c                                area of the anulus
c
c -------------------------------------------------------------------

      implicit none
      logical areawgt
      integer nbins
      real x_in,y_in,rad_hi_sq(*),rad_length,pix_size_min
      real rad_hi(*),wgt_rad
      real area(nbins,1,1),area_wgt(nbins,1,1)
      integer chatter,errflg

c ------------------ internal variables --------------------------
      character(500) subinfo
      integer maxbins
      parameter (maxbins=100)
      real pix_rad_length,pix_bin_length
      real pix_rad_lo(maxbins),pix_rad_hi(maxbins)
      real pix_rad_lo_sq(maxbins),pix_rad_hi_sq(maxbins)
      real x_min,x_max,y_min,y_max,xsq,ysq
      real theo_counts(maxbins),cal_area(maxbins)
      real xtemp,ytemp,rsq,pix_size_min_sq
      integer i,k,center_x,center_y,nbins_wgt

c -------------------- variable explanation ---------------------------
c center_x       int   integer form of x_in
c center_y       int   integer form of y_in
c pix_rad_length real  total radial length in pixels
c pix_bin_length real  length of each bin
c x_min,xmax and real  the four corners of the square surrounding the
c    ymin,ymax         circular region
c
c --------------- AUTHORS/MODIFICATION ---------------------------------
c
c Banashree Mitra Seifert (Feb., 1996)
c
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '1.0.0')
        character(17) subname

*-
        subname = 'calc_areawgt_rad'
        subinfo  = 'using '//subname//'Ver '//version
        call wtinfo(chatter,10,1,subinfo)
c ------------------------------------------------------------------

      nbins_wgt=0
      do i=1,nbins
         if (rad_hi(i) .gt. wgt_rad) then        
             nbins_wgt= i-1
             goto 1
         endif
      enddo

 1       write(subinfo,'(a,f5.1,a)')
     >    'Area weighting dist. input by user is ',
     >     wgt_rad, ' arc-min from the center.'
         call wtinfo(chatter,10,0,subinfo)
         write(subinfo,'(a,f5.1,a)')
     >    'Area weighting done through a dist. of',
     >     rad_hi(nbins_wgt), ' arc-min from the center.'
         call wtinfo(chatter,10,0,subinfo)
         if (wgt_rad .ne. rad_hi(nbins_wgt)) then
             subinfo='This difference in calculation from that of'
     >            //' user input value is due to binning since'
     >            //' bin no. is an integer and minimum bin'
     >            //' accepted for calculation which ever is less' 
             call wtinfo(chatter,10,0,subinfo)
         endif

      if(pix_size_min .lt. 0.) then
         pix_size_min = - pix_size_min
      endif

      pix_rad_length = rad_hi(nbins_wgt)/pix_size_min
      pix_bin_length = pix_rad_length/nbins_wgt

      center_x= int(x_in +0.5)
      center_y= int(y_in +0.5)

      x_min = center_x - pix_rad_length
      x_max = center_x + pix_rad_length
      y_min = center_y - pix_rad_length
      y_max = center_y + pix_rad_length

      pix_rad_lo(1) = 0.
      pix_rad_hi(1) = pix_rad_lo(1) + pix_bin_length
      pix_rad_lo_sq(1) = 0.
      pix_rad_hi_sq(1) = pix_rad_hi(1) * pix_rad_hi(1)
      i = 2
      do while(i .le. nbins_wgt)
         pix_rad_lo(i) = pix_rad_hi(i-1)
         pix_rad_hi(i) = pix_rad_lo(i) + pix_bin_length
         pix_rad_lo_sq(i) = pix_rad_lo(i) * pix_rad_lo(i)
         pix_rad_hi_sq(i) = pix_rad_hi(i) * pix_rad_hi(i)
         i = i+1
      enddo

      pix_size_min_sq = pix_size_min*pix_size_min

      i=1
      do while(i .le. nbins)
         theo_counts(i)= 0.
         i=i+1
      enddo

      xtemp = x_min - 1
      do while (xtemp .le. x_max)
         xtemp=xtemp+1
         xsq = (xtemp-center_x)**2
         ytemp = y_min - 1
         do while (ytemp .le. y_max)
            ytemp=ytemp+1
            ysq = (ytemp-center_y)**2
            rsq = xsq + ysq
            if(rsq .le. pix_rad_hi_sq(nbins_wgt)) then
               do k=1,nbins
                  if(rsq .le. pix_rad_hi_sq(k)) then
                     theo_counts(k)= theo_counts(k) + 1
                     goto 2
                  endif
               enddo
            endif
 2       enddo
      enddo

      i=1
      do while(i .le. nbins_wgt)
         cal_area(i) = theo_counts(i) * pix_size_min_sq
         area_wgt(i,1,1) = cal_area(i) / area(i,1,1)
         i=i+1
      enddo
      i=nbins_wgt+1
      do while(i .le. nbins)
         area_wgt(i,1,1) = 1.
         i=i+1
      enddo

      errflg=0
      return
      end

c --------------------------------------------------------------------
c                        end of area_wgt_rad calculation
c --------------------------------------------------------------------

*+calc_areawgt_no

      subroutine calc_areawgt_no(nbins,area_wgt,chatter,errflg)

c -------------------------------------------------------------------
c This subroutine if when area weighting is not required by user.
c i.e., when area_wgt = 1.
c
c -------------------------------------------------------------------

      implicit none
      character(100) subinfo
      integer i,nbins
      real area_wgt(nbins,1,1)
      integer chatter,errflg

c --------------- AUTHORS/MODIFICATION ---------------------------------
c
c Banashree Mitra Seifert (Feb., 1996)
c
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '1.0.0')
        character(16) subname

*-
        subname = 'calc_areawgt_no'
        subinfo  = 'using '//subname//'Ver '//version
        call wtinfo(chatter,10,1,subinfo)

        i=1
        do while (i .le. nbins)
           area_wgt(i,1,1) = 1.
           i=i+1
        enddo
        
        errflg=0

        return
        end

c -------------------------------------------------------------------
c            end of calc_areawgt_no
c -------------------------------------------------------------------

*+READ_REGION

      subroutine read_region_psf(iunit,char_no,maxpoints,line_no,
     >                       comm_line,shape_no,shape,npoints,
     >                       points,sign,chatter,errflg)

c ------------------------- description ------------------------------
c READ_REGION reads the input region file, extracts the information
c and passed to the called routine.
c
c ------------- variable definitions ---------------------------
       implicit none
       character*(*) comm_line(*)
       character*(*) shape(*)
       character*(*) sign(*)
       integer maxpoints
       real points(maxpoints,maxpoints)
       integer iunit,line_no,shape_no
       integer npoints(*),char_no(*)
       integer chatter,errflg

c ----------------------- internal variables ---------------------
       character(200) line,temp
       character(100) subinfo
       character(20) char1,char_points(100,100)
       integer i,count,end,start

c ----------------------- variable directory ---------------------
c iunit     int  i/p  unit no. of input file
c char_no   int  o/p  no. of letters in the particular shape
c maxpoints int  o/p  maximum number of shapes and maximum number of
c                     points for a particular shape
c line_no   int  o/p  no. of comment lines
c comm_line char o/p  content of comment lines
c shape_no  int  o/p  no. of shapes in the input region file
c shape     char o/p  the shape of a particular region
c                     six shapes are supported at present:
c                     1. CIRCLE
c                     2. BOX
c                     3. POLYGON
c                     4. POINT
c                     5. ELLIPSE
c                     6. ANNULUS
c npoints   int  o/p  no. of parameters inside shape
c points    real o/p  parameters for defining shape
c                     points(1,1),points(1,2) are the x and y coordinate
c                     of shape_no=1
c sign      char o/p  sign preceeding the shape -> " " , "!", or " "
c errflg    int  o/p  error flag defining error status
c
c ----------------- internal variable directory --------------------
c
c line        char  content of the line read
c temp        char  assign read character to temp
c char1       char  temporary character save
c char_points char  the parameter values read in character
c i           int   count for do loop
c count       int   count of the lines read
c end         int   end point of a character in the line read
c start       int start point of a character in the line read
c
c ----------------------- called routines -----------------------
c    none
c --------- author/modifications --------------------------------
c
c Banashree Mitra Seifert (Feb 1996) 1:0:0
c
c ---------------------------------------------------------------
       character(5) version
       parameter (version='1.0.0')
       character(12) subname

       subname = 'read_region_psf'
       subinfo='using '//subname//'Ver '//version
       call wtinfo(1,10,1,subinfo)

c ------------------------- initialisation -----------------------------
      line_no=0
      count = 0
      shape_no=0
 1    count=count+1
      read(iunit,'(a)',end=200) line
           call ftupch(line)
           if (line(1:1) .eq. '#') then
               line_no=line_no+1
               comm_line(line_no)= line
               goto 1
           elseif((line(1:1) .eq. '-') .or.
     >            (line(1:1) .eq. '!')) then
                  shape_no=shape_no+1
                  sign(shape_no) = line(1:1)
           elseif((line(1:1) .eq. ' ') .or.
     >            (line(1:1) .eq. '+')) then
                  shape_no=shape_no+1
                  sign(shape_no) = line(1:1)
           else
                  subinfo ='none of these "-","!", or " " is'
     >                     //' encountered'
                  call wterrm(subname,version,subinfo)
                  errflg = 1
                  return
           endif

c -----------------------------------------------------------------
c excluded or included region is found by line(1:1)
c Now to find out the first starting point after '('
c -----------------------------------------------------------------

      do i=1,20
         char1 = line(i:i)
         if (char1 .eq. '(' ) then
             goto 2
         endif
      enddo

c now we have region,ie, BOX,CIRCLE,POLYGON,POINTS,ELLIPSE, or ANNULUS

 2    end = i-1
      char_no(shape_no) = end-1
      if (line(2:end) .eq. 'CIRCLE') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'BOX') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'ELLIPSE') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'POINT') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'POLYGON') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'ANNULUS') then
               shape(shape_no)=line(2:end)

       else
               subinfo='Unknown region: '//line(1:end)
               call wterrm(subname,version,subinfo)
               errflg=1
               return
       endif


c Now get all the points

c move past '('

      start = end+1
      npoints(shape_no)=0
20    do i=1,10
         temp = line(start+i:start+i)
         if (temp .eq. ',') then
             npoints(shape_no)=npoints(shape_no)+1
             end=start+i-1
             char_points(shape_no,npoints(shape_no)) = line(start+1:end)
             start = end+1
             goto 20
         elseif (temp .eq. ')') then
                 npoints(shape_no)=npoints(shape_no)+1
                 end=start+i-1
                 char_points(shape_no,npoints(shape_no)) =
     >                                             line(start+1:end)
                 goto 6
         endif
      enddo


 6    do i=1,npoints(shape_no)
         read (char_points(shape_no,i),*,iostat=errflg)
     >                                          points(shape_no,i)
      enddo

      goto 1

 200  return
      end
c --------------------------------------------------------------
c             end of read_region_psf subroutine
c --------------------------------------------------------------
 
*+DO_REGION

      subroutine do_region(x,y,shape_no,shape,x_center,y_center,
     >                     pix_size_min,xmin,xmax,ymin,ymax,radiusmin,
     >                     radiusmax,radius,x1,x2,y1,y2,twice_a,minus,
     >                     chatter,errflg)
                                    
c ------------------------------------------------------------------
c this subroutine calculates the region file
c 1. calculates the region file
c 2. calculates whether the pixel falls inside the region 
c 3. if so, then it keeps no. of counts to be included/subtracted
c -----------------------------------------------------------------

      implicit none
      character*(*) shape(*)
      real pix_size_min
      integer x,y
      integer shape_no
      integer chatter,errflg
      real x_center(*),y_center(*),xmin(*),xmax(*),ymin(*),ymax(*)
      real radiusmin(*),radiusmax(*),radius(*)
      real twice_a(*),x1(*),y1(*),x2(*),y2(*)
      logical minus

c --------------------- internal variables --------------------

      character(120) subinfo
      real dist_sq,dist,sq_dist1,sq_dist2,sum_distance
      integer i
      real pix_sq

c --------------- authors/modificateions --------------------
c Banashree Mitra Seifert (1996 April) 1.0.0:
c ---------------------------------------------------------------
      character(10) subname
      parameter (subname='do_region')
      character(5) version
      parameter (version='1.0.0')
*-
c -----------------------------------------------------------------
c      subinfo='using '//subname //'Ver '//version
c      call wtinfo(chatter,10,2,subinfo)
c --------------------------------------------------------------------
      pix_sq=pix_size_min*pix_size_min

      errflg=0
      minus=.false.
      do i=1,shape_no
         if (shape(i) .eq. 'BOX') then
             if((x .ge. xmin(i)) .and. (x .le. xmax(i))) then 
                if((y .ge. ymin(i)) .and. (y .le. ymax(i))) then
                   minus=.true.
                   return
                endif
             endif
            
         elseif (shape(i) .eq. 'ANNULUS') then
             radiusmin(i)=radiusmin(i)
             radiusmax(i)=radiusmax(i)
             dist_sq=(x-x_center(i))*(x-x_center(i)) +
     >               (y-y_center(i))*(y-y_center(i))
             dist=sqrt(dist_sq)
             if(dist .gt. radiusmin(i)) then
                if(dist .le. radiusmax(i)) then
                   minus=.true.
                   return
                endif
             endif

         elseif (shape(i) .eq. 'CIRCLE') then
             dist_sq=(x-x_center(i))*(x-x_center(i)) +
     >               (y-y_center(i))*(y-y_center(i))
             dist=sqrt(dist_sq)
             if(dist .le. radius(i)) then
                minus=.true.
                return
             endif


         elseif (shape(i) .eq. 'ELLIPSE') then
c        in case of ellipse the sum of the distances of a point from the two
c        focii .le. 2a 
             sq_dist1 = (x-x1(i))*(x-x1(i)) + 
     >                 (y-y1(i))*(y-y1(i))
             sq_dist2 = (x-x2(i))*(x-x2(i)) +
     >                 (y-y2(i))*(y-y2(i))
             sum_distance = sqrt(sq_dist1) + sqrt(sq_dist2)
             if(sum_distance .le. twice_a(i)) then 
                minus = .true.
                return
             endif

c         elseif (shape(i) .eq. 'POLYGON') then


         else
              subinfo='shape of region is neither of' 
     >                //'BOX,ANNULUS,CIRCLE,ELLIPSE'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
         endif

      enddo


      return
      end

c ------------------------------------------------------------
c                         end of do_region
c -------------------------------------------------------------

*+REGION_BOUNDARY

      subroutine region_boundary(char_no,maxpoints,
     >                       shape_no,shape,npoints,points,sign,
     >                       x_center,y_center,xmin,xmax,ymin,ymax,
     >                       radiusmin,radiusmax,radius,x1,x2,y1,y2,
     >                       twice_a,chatter,errflg)

c -------------------------------------------------------------------
c this routine calculates the region boundaries
c e.g., it calculates for BOX xmin,xmax,ymin and ymax
c                     for CIRCLE center and radius
c                     for ELLIPSE -- 2*a
c                     for ANNULUS -- inner radius and outer radius
c -------------------------------------------------------------------- 
           

      implicit none
      integer maxpoints
      character*(*) shape(*)
      character*(*) sign(*)
      real points(maxpoints,maxpoints)
      real x_center(*),y_center(*)
      real xmin(*),xmax(*),ymin(*),ymax(*)
      real radiusmin(*),radiusmax(*),radius(*),a,b,phi,xfocii,yfocii
      real twice_a(*),x1(*),y1(*),x2(*),y2(*) 
      integer shape_no
      integer npoints(*),char_no(*)
      integer chatter,errflg
      character(100) subinfo
      integer i

c --------------------- variable directory ----------------------------
c maxpoints   int     maximum no. of points for a particular shape
c                     e.g. for circle it is 1
c shape       char    shape of region e.g. BOX,CIRCLE,ELLIPSE,ANNULUS
c sign        char*1  sign of the region - for exclusion and + for inclusion
c points      real    for CIRCLE it is radius, for BOX it is the lengths of
c                     the sides, for ANNULUS it is the inner and outer radii
c                     and for ELLIPSE it is a,b,phi
c x_,y_center real    center coords of the region     
c xmin,xmax   real    for BOX the min and max x-coord. of the corners
c ymin,ymax   real    for BOX the min and max y-coord. of the corners
c radiusmin   real    for ANNULUS inner radius of the annulus
c radiusmax   real    for ANNULUS outer radius of the annulus
c radius      real    for CIRCLE radius
c a,b         real    for ELLIPSE semi-major and semi minor axes
c phi         real    for ELLIPSE the angle with x-axis clockwise from x-axis
c xfocii      real    for ELLIPSE x-distance of focii from center
c yfocii      real    for ELLIPSE y-distance of focii from center
c twice_a     real    for ELLIPSE =2a
c x1,y1       real    for ELLIPSE x- and y-coord of one of the focii
c x2,y2       real    for ELLIPSE x- and y-coord of another focus
c shape_no    int     serial no. of the shape from region file
c npoints     int     no. of points supplied for a particular shape
c char_no     int     no. of characters for a particular shape
c                     e.g. for BOX char_no 3, CIRCLE =6 etc
c 
c --------------- authors/modificateions --------------------
c Banashree Mitra Seifert (1996 April) 1.0.0:
c ---------------------------------------------------------------
      character(16) subname
      parameter (subname='region_boundary')
      character(5) version
      parameter (version='1.0.0')
*-
c -----------------------------------------------------------------
      subinfo='using '//subname //' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c -----------------------------------------------------------------
 
      do i=1,shape_no
         if (shape(i) .eq. 'BOX') then
             x_center(i)=points(i,1)
             y_center(i)=points(i,2)
             xmin(i)=x_center(i) - points(i,3)/2.
             xmax(i)=xmin(i) + points(i,3)
             ymin(i)=y_center(i) - points(i,4)/2.
             ymax(i)=ymin(i) + points(i,4)

         elseif (shape(i) .eq. 'ANNULUS') then
             x_center(i)=points(i,1)
             y_center(i)=points(i,2)
             radiusmin(i)=points(i,3)
             radiusmax(i)=points(i,4)

         elseif (shape(i) .eq. 'CIRCLE') then
             x_center(i)=points(i,1)
             y_center(i)=points(i,2)
             radius(i)=points(i,3)

         elseif (shape(i) .eq. 'ELLIPSE') then
             x_center(i)=points(i,1)
             y_center(i)=points(i,2)
             a=points(i,3)
             b=points(i,4)
             phi=points(i,5)
c            convert phi from degree to radians
             phi=phi*3.14159265359/180.
c            xfocii and yfocii are the distances of the focii from the center
             if(a .lt. b) then
                 xfocii=sqrt(b*b - a*a)*sin(phi)
                 yfocii=sqrt(b*b - a*a)*cos(phi)
                 twice_a(i) = 2.*b
             else
                 xfocii=sqrt(a*a - b*b)*cos(phi)
                 yfocii=sqrt(a*a - b*b)*sin(phi)
                 twice_a(i) = 2.*a
             endif
c            x1,y1 and x2,y2 are the coords. of the two focii
             x1(i)=x_center(i) + xfocii
             y1(i)=y_center(i) + yfocii
             x2(i)=x_center(i) - xfocii
             y2(i)=y_center(i) - yfocii

c         elseif (shape(i) .eq. 'POLYGON') then
c         shape POLYGON is not supported due to complexity of shape

         else
              subinfo='shape of one of the regions is neither of'
     >                //'BOX,ANNULUS,CIRCLE,ELLIPSE'
              call wterrm(subname,version,subinfo)
              subinfo='shapes other than these is not supported'
              call wtinfo(chatter,0,1,subinfo)
              errflg=1
              return
         endif

      enddo

      return
      end
c ------------------------------------------------------------
c                 end of region_boundary
c -------------------------------------------------------------


