
*+DETECT

      subroutine detect

c ----------------------------------------------------------------
c Identify the point sources in the field covered by a single 
c observation and creates a point source mask for that field. 
c It uses SLIDING BOX algorithm with the slight improvement that the
c box is a circle wherew the radius is a function of off-axis angle.
c The source identification threshold can be set for both count rate 
c and significance, both at the same time if desired.
c The program sets a source radius at 90% encircled energy radius and 
c the background annulus at 2.5 times the 90% encircled energy radius.
c If fewer than 4 counts are found in the background annulus, the 
c average background for the observation is used.
c The significance is determined using POISSON statistics, but does not
c consider the number of trials.
c
c Energy bands:  the range of bands to be analysed for point sources. 
c
c Statistical significance: either Poisson probability 
c                          (i.e., 3xsigma=0.9973002) or the value of 
c                           sigma (e.g, 1, 1.5, 2, 2.5,....,8, 8.5)
c
c Count rate threshold: units of counts per sec (may be set to zero)
c
c inputs:
c   band select
c   count image and exposure map
c outputs:
c   (1)ascii file containing info on the detected source, source number,
c      x and y pixels, source count rate in (counts/sec) and source
c      significance
c   (2)FITS file of the point-source mask (image file)
c      accepted regions are set to 1 and masked ones are set to 0
c
c June 1997: ONLY PSPC is supported
c The HRI support id dropped for the first release (June 1997)
c since  need some more details to work out regarding makeing
c the image of the event file.
c ------------------ routines called ---------------------------------
c detect_gp        gets parameter
c
c -------------------------- variables ------------------------------+

      implicit none
      character(180) evtmapfil,expmapfil,hashfil,outfil
      double precision sig
      integer nx,ib1,ib2
      real ee(7),wt(7) 
      real c_rate
      integer nnx,errflg,chatter
      logical killit, qmask

      character(100) subinfo
      integer status,iget,ineed

      integer p_img_inr4, p_exp_inr4, p_new_inr4
      integer p_hash, p_listx, p_listy
      integer p_counts, p_expose, p_mask, p_circle


c ------------------ variable definitions ---------------------------
c evtmapfil   i   event image map file
c ib1     i   i   lower energy band 
c ib2     i   i   upper energy band
c ee      r data  average energy in each band
c wt      r data  weightage for each energy band
c c_rate  r   i   threshold count rate (counts/sec) 
c sig     double  significance of detection in sigma
c qmask   logical whether masking output file is required
c                 It is hidden parameter and by default it is "no"
c
c --------------- authors/modification -------------------------------
c
c Banashree M Seifert (Oct, 1996) 1.0.0:
c Peter D Wilson (June 30, 1998) 1.0.1:
c              . Updated for new FCPARS behavior
c
c --------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.1')
      character(7) subname
      parameter (subname='detect')
*-
c ------------- dynamic memory allocated array -------------------------


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

c ---------------------------------------------------------------------+
      data ee/180.0, 250.0, 525.0, 600.0, 800.0, 1050.0, 1500.0/
      data wt/   2.,    3.,    0.5,   1.,    1.,     1.,    0.5/ 
c ----------------- get parameters ------------------------------------+


      call detect_gp(evtmapfil,expmapfil,hashfil,outfil,ib1,ib2,sig,
     >               c_rate,qmask,errflg,chatter,killit)

      if (errflg .ne. 0) then
          subinfo='returning from detect_gp'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c ----------------- task begins from here -----------------------------+

      call wtbegm(subname,version,chatter)

      call findsize(evtmapfil,nx,chatter,errflg)
      
      if (errflg .ne. 0) then
          subinfo='returning from findsize'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_img_inr4 = 0
      p_exp_inr4 = 0
      p_new_inr4 = 0
      p_hash = 0
      p_listx = 0
      p_listy = 0
      p_counts = 0
      p_expose = 0
      p_mask = 0 
      p_circle = 0

      iget=0
      status = 0
      nnx=nx+100
      call udmget(nx*nx, 6, p_img_inr4, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nx*nx*4

      status = 0
      call udmget(512*512, 6, p_exp_inr4, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+512*512*4

  
      status = 0
      call udmget(nx*nx, 6, p_new_inr4, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nx*nx*4
 

      status = 0
ccc      call udmget(60, 4, p_hash, status)
      call udmget(240, 4, p_hash, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+240*4

      status = 0
      call udmget(180916, 4, p_listx, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+180916*4

      status = 0
      call udmget(180916, 4, p_listy, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+180916*4

      call udmget(nnx*nnx, 6, p_counts, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nnx*nnx*4

      call udmget(nnx*nnx, 6, p_expose, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nnx*nnx *4

      call udmget(nnx*nnx , 6, p_mask, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nnx*nnx*4

      call udmget(nnx*nnx, 6, p_circle, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+nnx*nnx*4
  

      ineed = nx*nx*2*4 + 240*4 + 2*180916*4 + nnx*nnx*4*4 +512*512*4

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
           call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory I get   =',iget
           call wtinfo(chatter,10,1,subinfo)

 50   if (status .ne. 0) then
          errflg = -1
          subinfo='failed to allocate dynamic memory '
          call wtinfo(chatter,1,1,subinfo)
          goto 100
      endif


      call do_detect(evtmapfil,expmapfil,hashfil,outfil,ib1,ib2,ee,wt,
     >               sig,c_rate, nx, MEMR(p_img_inr4), 
     >               MEMR(p_exp_inr4), MEMR(p_new_inr4), MEMI(p_hash), 
     >               MEMI(p_listx), MEMI(p_listy), MEMR(p_counts),
     >               MEMR(p_expose), MEMR(p_mask), MEMR(p_circle),
     >               qmask, errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from do_detect subroutine'
         call wterrm(subname,version,subinfo)
         goto 100
      endif

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_img_inr4, 6, status)
      if (status .ne. 0) goto 60

      call udmfre(p_exp_inr4, 6, status)
      if (status .ne. 0) goto 60

      call udmfre(p_new_inr4, 6, status)
      if (status .ne. 0) goto 60

      call udmfre(p_hash,     4, status)
      if (status .ne. 0) goto 60

      call udmfre(p_listx,    4, status)
      if (status .ne. 0) goto 60

      call udmfre(p_listy,    4, status)
      if (status .ne. 0) goto 60

      call udmfre(p_counts,   6, status)
      if (status .ne. 0) goto 60

      call udmfre(p_expose,   6, status)
      if (status .ne. 0) goto 60

      call udmfre(p_mask,     6, status)
      if (status .ne. 0) goto 60

      call udmfre(p_circle,   6, status)

 60   if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wterrm(subname,version,subinfo)
      endif

 100  call wtendm(subname,version,errflg,chatter)
      end
c ----------------------------------------------------------------------
c            **** END OF MAIN DETECT ****
c ----------------------------------------------------------------------

*+DETECT_GP

      subroutine detect_gp(evtmapfil,expmapfil,hashfil,outfil,bandl,
     >                     bandh,sig,count_rate,qmask,errflg,
     >                     chatter,killit)

c ----------------------------------------------------------------------
c This subroutine reads the user defined parameters such as input files
c and output file as well as chatter flag
c
c --------------- variables --------------------------------------------
      implicit none
      character*(*) evtmapfil,expmapfil,hashfil,outfil
      integer bandl,bandh
      real count_rate
      double precision sig

      integer errflg,chatter
      logical killit,qmask

c --------------- authors/modification ---------------------------------
c
c Banashree M Seifert (Dec, 1996) 1.0.0:
c Peter D Wilson (June 30, 1998) 1.0.1:
c       . Strip out INQUIRE calls for FITS input files
c
c-----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.1')
      character(10) subname
      parameter (subname ='detect_gp')
*-
c --------------- internal declarations --------------------------------
      character(180) filename,ill_file(10)
      character(100) subinfo, comm
      character(4) instrument
      integer iunit, chanmin,chanmax,exp_chanmin,exp_chanmax
      integer status, extnum, n_ill, block
      logical ext

*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(0,10,2,subinfo)

c ------------------------------------------------------------------
c     now read the required parameters from parameter file
c                read in chatter & clobber
c ------------------------------------------------------------------

c read in clobber 

      status = 0
      call uclgsb('clobber', killit, status)
      if (status .ne. 0) then
          subinfo = 'getting clobber parameter'
          call wterrm(subname,version,subinfo)
          killit =.false.
          status = 0
      endif

c chatter parameter 

      status = 0
      call uclgsi('chatter',chatter,status)
      if (status .ne. 0) then
          subinfo = 'getting chatter parameter'
          call wterrm(subname,version,subinfo)
          chatter = 9
          status = 0
      endif

c masking required or not

      status = 0
      call uclgsb('qmask', qmask, status)
      if (status .ne. 0) then
          subinfo = 'getting qmask parameter'
          call wterrm(subname,version,subinfo)
          qmask = .false.
          status = 0
      endif
  
c ----------------------------------------------------------------------
c                input file parameters                    
c ----------------------------------------------------------------------
c Source spectrum

      subinfo='  '
      call wtinfo(chatter,0,0,subinfo)
      subinfo=' --- Only PSPC is supported for FTOOLS4.0 (June, 1997) '
      call wtinfo(chatter,0,0,subinfo)
      subinfo=' ------ Working on HRI support for later release'
      call wtinfo(chatter,0,0,subinfo)

      n_ill = 0
      status = 0
      call uclgst('evtmapfil',evtmapfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for events map!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(evtmapfil)
      if (evtmapfil(1:2) .eq. '  ') then
          subinfo = 'events map file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't call INQUIRE. Call FTRTNM to strip off extension
      n_ill = n_ill+1
      call ftrtnm( evtmapfil, ill_file(n_ill), status )
C      call fcpars(evtmapfil, filename, extnum, status)
C      call crmvlbk(filename)
C      n_ill = n_ill + 1
C      ill_file(n_ill) = filename
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
C          subinfo= 'events map file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c exposure mapfile

      n_ill = 0
      status = 0
      call uclgst('expmapfil',expmapfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for exposure map!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(expmapfil)
      if (expmapfil(1:2) .eq. '  ') then
          subinfo = 'events map file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't call INQUIRE. Call FTRTNM to strip off extension
      n_ill = n_ill+1
      call ftrtnm( expmapfil, ill_file(n_ill), status )
C      call fcpars(expmapfil, filename, extnum, status)
C      call crmvlbk(filename)
C      n_ill = n_ill + 1
C      ill_file(n_ill) = filename
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
C          subinfo= 'exposure map file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif
C      expmapfil=filename  

c hash file
 
      status = 0
      call uclgst('hashfil',hashfil,status)
      if (status .ne. 0) then
          subinfo = 'getting hash filename'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif


c ------------------------------------------------------------------
cccc  since HRI need to be worked out, we released only PSPC support FTOOLS4.0
cccc      call uclgst('instr', instrument, status)
cccc      if (status .ne. 0) then
cccc          subinfo = 'getting instrument type parameter'
cccc          call wterrm(subname,version,subinfo)
cccc          errflg=1
cccc          return
cccc      endif

c ------------------------------------------------------------------
ccc      call ftupch(instrument) 

ccc      if (instrument(1:3) .eq. 'HRI') then
ccc          chanmin=3
ccc          chanmax=10
ccc          bandl=0
ccc          bandh=0
ccc      else

ccc   get chanmin and chanmax parameters for PSPC 
ccc   and subsequently calculate lower and upper bands -- bandl, bandh

          call uclgsi('chanmin',chanmin,status)
          if (status .ne. 0) then
              subinfo = 'getting lower PI parameter'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
          endif

          call uclgsi('chanmax',chanmax,status)
          if (status .ne. 0) then
              subinfo = 'getting upper PI parameter'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
          endif

ccc       check if chanmin < chanmax are consistence

          if(chanmin .gt. chanmax) then
             subinfo='chanmin > chanmax'
             call wterrm(subname,version,subinfo)
             errflg=1
             return
          endif

ccc       see if user input of chanmin/chanmax is consistence with
ccc       exposure map file CHANMIN/CHANMAX
ccc       also is checked to see if user uses HRI input file, then exit with error
ccc        this check should be taken out once HRI is supported

          status=0
          call ftgiou(iunit,status)
          call ftopen(iunit,expmapfil,0,block,status)
          if (status .ne. 0) then
              subinfo = 'opening exposure map file to read chan nos.'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
          endif

          call ftgkys(iunit,'INSTRUME', instrument, comm, status)
          if (status .ne. 0) then
              subinfo='reading INSTRUME keyword'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
          endif

          if(instrument(1:4) .ne. 'PSPC') then
             subinfo='  '
             call wterrm(subname,version,subinfo)
             subinfo='Sorry!! at present only PSPC is supported'
             call wtinfo(chatter,0,1,subinfo)
             subinfo='your input exposure mapfile INSTRUMENT is '
     >                //' not PSPC'
             call wtinfo(chatter,0,1,subinfo)
             errflg=1
             return
          endif


          call ftgkyj(iunit,'CHANMIN',exp_chanmin,comm,status)
          if (status .ne. 0) then
              subinfo='reading CHANMIN keyword'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
          endif

          call ftgkyj(iunit,'CHANMAX',exp_chanmax,comm,status)
          if (status .ne. 0) then
              subinfo='reading CHANMAX keyword'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
          endif

          call ftclos(iunit,status)
          call ftfiou(iunit,status)
   
ccc       
          if(chanmin .ne. exp_chanmin) then
             subinfo='your input chanmin does not match with '//
     >               'exposure map chanmin keyword'
             call wterrm(subname,version,subinfo)
             errflg=1
             return
          endif

ccc       calculate lower and upper bands -- bandl, bandh from chanmin/max
          if(chanmin .eq.   8) then
             bandl=1
          elseif(chanmin .eq.  11) then
             bandl=8
          elseif(chanmin .eq.  20) then
             bandl=2
          elseif(chanmin .eq.  42) then
             bandl=3
          elseif(chanmin .eq.  52) then
             bandl=4
          elseif(chanmin .eq.  70) then
             bandl=5
          elseif(chanmin .eq.  91) then
             bandl=6
          elseif(chanmin .eq. 132) then
             bandl=7
          else
             write(subinfo,'(a,i4)')'chanmin from your input is ',
     >                              chanmin
             call wtinfo(chatter,0,2,subinfo)
             subinfo='chanmin should be any of followings:'
             call wtinfo(chatter,0,2,subinfo)
             subinfo='8, 11, 20, 42, 52, 70, 91, 132'
             call wtinfo(chatter,0,2,subinfo)
             subinfo='try create a new exposure map and try again!'
             call wterrm(subname,version,subinfo)
             errflg=1
             return
          endif


          if(chanmax .eq. 19) then
             if(bandl .eq. 8) then
                bandh=8
             else
                bandh=1
             endif
          elseif(chanmax .eq.  41) then
             bandh=2
          elseif(chanmax .eq.  51) then
             bandh=3
          elseif(chanmax .eq.  69) then
             bandh=4
          elseif(chanmax .eq.  90) then
             bandh=5
          elseif(chanmax .eq. 131) then
             bandh=6
          elseif(chanmax .ge. 201) then
             bandh=7
          else
             write(subinfo,'(a,i4)')'chanmax from your input is ',
     >                              chanmax
             call wtinfo(chatter,0,2,subinfo)
             subinfo='CHANMAX should be any of followings:'
             call wtinfo(chatter,0,2,subinfo)
             subinfo='19, 41, 51, 69, 90, 131, >=201'
             call wtinfo(chatter,0,2,subinfo)
             subinfo='try create a new exposure map and try again!'
             call wterrm(subname,version,subinfo)
             errflg=1
             return
          endif

c         end of checking for PSPC

ccc      endif
c ------------------------------------------------------------------
 10   call uclgsd('sig',sig,status)
      if (status .ne. 0) then
          subinfo = 'getting significance parameter'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

c ------------------------------------------------------------------
      call uclgsr('count_rate',count_rate,status)
      if (status .ne. 0) then
          subinfo = 'getting count rate threshold parameter'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

c -------------------------------------------------------------------
c output file

      n_ill = 0
      status = 0
      call uclgst('outfil',outfil,status)
      if (status .ne. 0) then
          subinfo = 'getting output file name!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(outfil)

      ext = .true.
      INQUIRE(FILE=outfil, EXIST=ext)
      if (ext) then
          call delfil(outfil)
      endif
c ------------------------------------------------------------------
  
      errflg = 0
      return
      end
c ----------------------------------------------------------------------
c                       END OF DETECT_GP
c ----------------------------------------------------------------------

*+DO_DETECT
 
      subroutine do_detect(evtmapfil,expmapfil,hashfil,outfil,ib1,ib2,
     >                     ee,wt,sig,cr,nx,img_inr4,exp_inr4,new_inr4,
     >                     hash,listx,listy,counts,expose,mask,circle,
     >                     qmask, errflg,chatter)

c --------------------------------------------------------------------
c this subroutine actually does the detection
c This is ftoolisation of Steve's code DETECT.F
c purpose: To detect sources in a ROSAT image in conjunction with
c          the extended analysis software
c -------------------------------------------------------------------

      implicit none
      character*(*) evtmapfil,expmapfil,hashfil,outfil
      double precision sig
      integer ib1,ib2, nx
      integer hash(*),listx(*),listy(*)
      real wt(*),ee(*), cr
      real img_inr4(nx,*), exp_inr4(512,512), new_inr4(nx,*)
      real expose(nx+100,*), mask(nx+100,*), counts(nx+100,*)
      real circle(nx+100,*)
      integer errflg,chatter
      logical qmask

c -------------------------- internals ----------------------------
      character(100) subinfo
      character(20) outmask,diagfil
      integer totsrc
      parameter (totsrc=400)
      double precision averat, bckc,bckr,bckt,gammq,prob,rate,rate_err
      double precision srcc, srcmc,srcr,srct,srcx,srcy,sumx,sumy
ccc      double precision srcrs(100),srces(100),srcps(100)
      double precision srcrs(totsrc),srces(totsrc),srcps(totsrc)
      integer tsrcx,tsrcy
      real totcts,totexp,weit
      integer numsrc, i, j, nsrc, x, y, factor
      integer ib,ibb1,ibb2,npix,srcxs(totsrc),srcys(totsrc)
      integer ix,iy,maxx,maxy,ylim,radin,radin1,radin2,radou
      integer  radin_sav(totsrc) 
      integer isize,iflag,hunit,ih,ounit1
      real sca1,e,tot,enc, expave
      real pixrad,dist,rad,r,tstpnt,maxrad
ccc      integer hash(60), listx(11288), listy(11288)
ccc      real inr4(512,512)
ccc      real counts(612,612),expose(612,612),mask(612,612),circle(612,612)

c for reading mapfiles
      integer evt_nx,evt_ny,status
      integer exp_nx,exp_ny
      double precision evt_cdelt1,evt_cdelt2,evt_crval1,evt_crval2 
      double precision evt_crpix1,evt_crpix2,evt_crot 
      character(8) evt_ctype,exp_ctype
      character(4) type
      double precision exp_cdelt1,exp_cdelt2,exp_crval1,exp_crval2 
      double precision exp_crpix1,exp_crpix2,exp_crot 

      real pix_in_min

      double precision tmp_ra,tmp_dec,ra(totsrc),dec(totsrc)
      double precision ra_sec,dec_sec
      integer ra_hr,ra_min,dec_deg,dec_min
      real sx, sy, xdist,ydist
      integer x1, x2, y1, y2
      character(11) significance(totsrc)
      logical rot,ext1,ext2,write_file
      
      integer block,iunit,ounit,nxby2,nxby2_50

      real  dist_bet_two_src(totsrc), pixrad_out(totsrc)
      real offang(totsrc), radius(totsrc)

c ----------------- variable definitions ---------------------
c sca1   r  data  scaling factor for ??????????????????????
c sig    d  i/p   significance of detection in sigma
c e      r        mean energy in eV
c enc    r        total encircled energy percent
c cr     r        count rate threshold
c inr4   r  i/p   count image array (512 x 512)
c
c --------- authors/modifications ----------------------------------
c Banashree M Seifert (Oct 1996)1.0.0:
c
c Banashree M Seifert (May, 1997) 1.1.0:
c       . maxno. of sources to be detected = 400
c       . introduced call delfil so that old detect.img will be deleted
c ------------------------------------------------------------------
     
      character(10) subname
      parameter (subname='do_detect')
      character(5) version
      parameter (version='1.1.0')
*-
      subinfo='using '//subname//'Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c
       npix = 0
       pixrad = 0

c ------------------------------------------------------------------
      ibb1 = ib1
      if(ib1 .eq. 0) ibb1 = 1
      if(ib1 .eq. 8) ibb1 = 1
ccc added by me
      if(ib1 .gt. 8) ibb1 = 1

      ibb2 = ib2
      if(ib2 .eq. 0) ibb2 = 1
      if(ib2 .eq. 8) ibb2 = 1
ccc added by me
      if(ib2 .gt. 8) ibb2 = 1


      if(ib1 .eq. 0) then
         sca1 = 144.0
      else
         sca1 = 16.113
      endif

c Set the energy for the PSF evaluation, i.e., pick an energy in the
c middle of the band that you are looking at.

      e = 0.
      tot = 0.
      if(ib1 .eq. 0) then
         e = 800.
         enc = 0.5
      else
         do ib=ibb1,ibb2
            e = e + wt(ib)*ee(ib)
            tot = tot + wt(ib)
         enddo
         e = e/tot
         tot = 0.
         write(subinfo,'(a,f6.2)')'Weighted Energy =',e
         call wtinfo(chatter,10,0,subinfo)
c        The encircled energy for the PSF evaluation
         enc = 0.9
      endif
c
c  If SIG is greater than or equal to 1, the use the value for SIG sigma
c  (i.e., convert to probabilities)
c
      if(sig .ge. 1.d0) then
         if(sig .eq. 1.0d0) sig = 0.6826894921370858d0
         if(sig .eq. 1.5d0) sig = 0.8663855974622838d0
         if(sig .eq. 2.0d0) sig = 0.9544997361036416d0
         if(sig .eq. 2.5d0) sig = 0.9875806693484477d0
         if(sig .eq. 3.0d0) sig = 0.9973002039367398d0
         if(sig .eq. 3.5d0) sig = 0.9995347418419290d0
         if(sig .eq. 4.0d0) sig = 0.9999366575163338d0
         if(sig .eq. 4.5d0) sig = 0.9999932046537505d0
         if(sig .eq. 5.0d0) sig = 0.9999994266968563d0
         if(sig .eq. 5.5d0) sig = 0.9999999620208751d0
         if(sig .eq. 6.0d0) sig = 0.9999999980268247d0
         if(sig .eq. 6.5d0) sig = 0.9999999999196800d0
         if(sig .eq. 7.0d0) sig = 0.9999999999974403d0
         if(sig .eq. 7.5d0) sig = 0.9999999999999361d0
         if(sig .eq. 8.0d0) sig = 0.9999999999999988d0
         if(sig .eq. 8.5d0) sig = 1.0000000000000000d0

      endif
      write(subinfo,'(a,f14.11)')'Significance : ',sig
      call wtinfo(chatter,10,0,subinfo)


      do iy=1,nx+100
        do ix=1,nx+100
           counts(ix,iy)=0.0
           expose(ix,iy)=0.0
        enddo
      enddo

      do iy=1,nx
         do ix=1,nx
            img_inr4(ix,iy)= 0.0
            new_inr4(ix,iy)= 0.0
         enddo
      enddo

      do iy=1,512
         do ix=1,512
            exp_inr4(ix,iy)= 0.0
         enddo
      enddo
 

c copy the evtmap (*.img) file to output file
      call ftgiou(iunit,errflg)
      call ftgiou(ounit,errflg)

      call ftopen(iunit,evtmapfil,0,block,errflg)
      call ftinit(ounit,outfil,block,errflg)
      call ftcopy(iunit,ounit,1,errflg)

      call ftclos(iunit,errflg)
      call ftclos(ounit,errflg)
      call ftfiou(iunit,errflg)
      call ftfiou(ounit,errflg)
 
c read the evtmap file =inr4(i,j)
      evt_nx=0
      evt_ny=0
      evt_crval1=0.d0
      evt_crval2=0.d0
      evt_cdelt1=0.d0
      evt_cdelt2=0.d0
      evt_crpix1=0.d0
      evt_crpix2=0.d0
      evt_crot=0.d0
      evt_ctype=' '
      rot=.true.
      errflg=0
      
      call read_map(evtmapfil,evt_nx,evt_ny,evt_crval1,evt_crval2,
     >              evt_cdelt1,evt_cdelt2,evt_crpix1,evt_crpix2,
     >              rot,evt_crot,evt_ctype,img_inr4,chatter,errflg)

      if(errflg .ne. 0)then
         subinfo='returning from read_map for file '//evtmapfil
         call wterrm(subname,version,subinfo)
         return
      endif
         
c convert pixelsize to arcmin since evt_cdelt1 is in degree

       pix_in_min = abs(evt_cdelt1)*60.


c padding data to the array
      totcts=0.
      do iy=1,evt_nx
         do ix=1,evt_nx
            counts(ix+50,iy+50) = img_inr4(ix,iy)
            totcts=totcts+img_inr4(ix,iy)
         enddo
      enddo
      
      write(subinfo,'(a,f10.2)')'total events =',totcts
      call wtinfo(chatter,15,0,subinfo)

c -----------------------------------------------------------------
c  Read in and merge the exposure arrays
c -----------------------------------------------------------------

      exp_nx=0
      exp_ny=0
      exp_crval1=0.d0
      exp_crval2=0.d0
      exp_cdelt1=0.d0
      exp_cdelt2=0.d0
      exp_crpix1=0.d0
      exp_crpix2=0.d0
      exp_crot=0.d0
      exp_ctype=' '
      rot=.false.
      errflg=0
      
      call read_map(expmapfil,exp_nx,exp_ny,exp_crval1,exp_crval2,
     >              exp_cdelt1,exp_cdelt2,exp_crpix1,exp_crpix2,
     >              rot,exp_crot,exp_ctype,exp_inr4,chatter,errflg)

      factor = evt_nx/512

         call exp_remap(exp_inr4,new_inr4,evt_nx,factor,
     >                  chatter,errflg)

         if(errflg .ne. 0)then
            subinfo='returning from exp_remap'
            call wterrm(subname,version,subinfo)
            return
         endif


c     Add the data to the array with padding

      totexp=0.0
      tot = 0.
      

      do ix=1,evt_nx
         do iy=1,evt_nx
             if(new_inr4(ix,iy) .gt. 0) then
               expose(ix+50,iy+50) = new_inr4(ix,iy)
               totexp=totexp+new_inr4(ix,iy)
               tot=tot+1.
            endif
         enddo
      enddo

c  Find the average exposure value

      expave = totexp/tot

      write(subinfo,'(a,e15.10,a,f12.1)')'exposure= ',totexp,
     >                         '   &   total no.=',tot
      call wtinfo(chatter,10,0,subinfo)
      write(subinfo,'(a,f12.2)')'average exposure= ',expave
      call wtinfo(chatter,10,0,subinfo)

      averat = totcts/totexp
      expave = expave/10.

      nxby2=evt_nx/2
      nxby2_50=nxby2+0.5+50.

c  only consider data where the exposure is more than 10% of the
c  average exposure

      do ix=51,evt_nx+50
         do iy=51,evt_nx+50
            circle(ix-50,iy-50)=0.
            if(expose(ix,iy) .lt. expave) then
               expose(ix,iy) = 0.
               mask(ix,iy) = 0.
            else
               mask(ix,iy) = 1.
            endif
         enddo
      enddo

c set up output file names

      if(ib1 .eq. 0) then
         outmask = 'mask.fits'
         diagfil = 'diag.dat'
      elseif(ib1 .ne. ib2) then
         write(outmask,'(a,i1,a,i1,a)') 'mask_', ib1,'_', ib2, '.fits'
         write(diagfil,'(a,i1,a,i1,a)') 'diag_', ib1,'_', ib2, '.dat'
      else
         write(outmask,'(a,i1,a)') 'mask_', ib1, '.fits'
         write(diagfil,'(a,i1,a)') 'diag_', ib1, '.dat'
      endif
   
      call crmvlbk(diagfil)
      subinfo = 'source detection filename = '//diagfil
      call wtinfo(chatter,0,0,subinfo)
      ext1 = .false.
      INQUIRE(FILE=diagfil, EXIST=ext1)

      if(qmask) then
         call crmvlbk(outmask)
         subinfo = 'mask filename             = '//outmask
         call wtinfo(chatter,0,0,subinfo)
         ext2 = .false.
         INQUIRE(FILE=outmask, EXIST=ext2)
      endif
      
      if((ext1) .and. (ext2)) then
          subinfo='  '
          call wtinfo(chatter,0,1,subinfo)
          subinfo='Both diagnostic file= '//diagfil
          call wtinfo(chatter,0,1,subinfo)
          subinfo='and mask file= '//outmask//' exist!'
          call wtinfo(chatter,0,1,subinfo)
          call delfil(outmask)
          call delfil(diagfil)
      elseif (ext1) then
          subinfo='  '
          call wtinfo(chatter,0,1,subinfo)
          subinfo='diagnostic file '//diagfil//' exists!'
          call delfil(diagfil)
      elseif (ext2) then
          subinfo='  '
          call wtinfo(chatter,0,1,subinfo)
          subinfo='mask file '//outmask//' exists!'
          call wtinfo(chatter,0,1,subinfo)
          call delfil(outmask)
      else 
      endif


c Here's the real program starts

c first read the hash file
      errflg=0
      call ftgiou(hunit,errflg)
      open(unit=hunit,file=hashfil,form='formatted', status='old')

c ------------------------------------------------------------------
c Steve's prog had 
ccc         read(hunit,'(i5)')hash(ih)
ccc         read(hunit,'(i3,i3)') listx(ih),listy(ih)
c I replace it by
ccc         read(hunit,*)hash(ih)
ccc         read(hunit,*) listx(ih),listy(ih)
c these replacement needed for replacing the hash file with new hash 
c file with 1024X1024 instead of old one 512X512
c ------------------------------------------------------------------

      do ih=1,240
         read(hunit,*)hash(ih)
      enddo

      do ih=1,180916
         read(hunit,*) listx(ih),listy(ih)
      enddo
 
      call ftclos(hunit,errflg)
      call ftfiou(hunit,errflg)

c  listx is a list of the X coordinates of the closest pixels
c  listy is a list of the Y coordinates of the closest pixels
c  all of these lists are ordered by distance
c  HASH lists the number of pixels within integer pixel distances
c  Now determine size of PSF near edge

      numsrc = 0
      if(ib1 .eq. 0) then
         call psf_hri(20.,maxrad,chatter,errflg)
         maxrad = maxrad*8./5.
      else
         call psf_pspc(e,57.3,enc,maxrad,chatter,errflg)
         maxrad = maxrad*4.
      endif

c  set the edge avoidance

      ix = maxrad + 50
      maxx = evt_nx - maxrad + 50

c     loop over the X direction
      do while(ix .lt. maxx)
         iy = maxrad
c        set the Y limits of the detector area

         ylim = (ix-nxby2_50)**2
         if(ylim .ge. nxby2*nxby2) then
            ylim = 0.
         else
            ylim = sqrt(float(nxby2*nxby2) - ylim)
         endif

         if((nxby2+0.5)-ylim .gt. iy) iy = nxby2+0.5 - ylim
         if((nxby2+0.5)+ylim .lt. evt_nx-maxrad) then
            maxy = (nxby2+0.5) + ylim + 50
         else
            maxy = evt_nx - maxrad + 50
         endif
         iy = iy + 50

c        loop over the Y direction
         do while(iy .lt. maxy)

c           determine whether the point is worth looking at
            tstpnt = expose(ix,iy)*mask(ix,iy)
            if(tstpnt.gt.0) then

c           determine size of PSF in pixels

               if(ib1 .eq. 0) then
                  r = 0.08333333*
     +                 sqrt(float((ix-nxby2_50)**2+(iy-nxby2_50)**2))
                  call psf_hri(r,rad,chatter,errflg)
                  pixrad = rad/5.
               else
ccc                  r = 0.249122*
                  r = pix_in_min*
     +                 sqrt(float((ix-nxby2_50)**2+(iy-nxby2_50)**2))
                  call psf_pspc(e,r,enc,rad,chatter,errflg)
ccc                  pixrad = rad/0.249122
                  pixrad = rad/pix_in_min
               endif

c              Make certain point is not within another source

               i = 1
               do while(i .lt. numsrc)
                  dist = (ix-srcxs(i))**2+(iy-srcys(i))**2
                  dist = sqrt(dist)
                  i = i + 1
                  if(dist .lt. 0.90*pixrad) then
                     tstpnt = 0
                     i = numsrc
                  endif
               enddo

               if(tstpnt .gt. 0)then

c  sum the source and background counts to calculate
c  the probability of a real source
c  PSPC: Source deemed to be within 1.0 PSF radius
c  Background deemed to be within 2.5 PSF radii
c  HRI:  Source deemed to be within 1.0 PSF radius
c  Background deemed to be between 2.5 and 4.5 PSF radii

                 if(ib1 .eq. 0) then
                    radin1 = nint(pixrad)
                    if(radin1 .lt. 1) radin1 = 1
                    radin2 = 2.5*radin1
                    radou = nint(4.5*pixrad)
                    if(radou .lt. 6) radou = 6
                 else
                    radin1 = nint(pixrad)
                    if(radin1 .lt. 1) radin1 = 1
                    radin2 = radin1
                    radou = nint(2.5*pixrad)
                    if(radou .lt. 4) radou = 4
                 endif

                 srcc = counts(ix,iy)
                 srct = expose(ix,iy)
                 do i=1,hash(radin1)
                    x = ix + listx(i)
                    y = iy + listy(i)
                    srcc = srcc + counts(x,y)
                    srct = srct + expose(x,y)
                 enddo
                 if(srcc .gt. 0)then
                    bckc = 0
                    bckt = 0
                    do i=hash(radin2)+1,hash(radou)
                       x = ix + listx(i)
                       y = iy + listy(i)
                       bckc = bckc + counts(x,y)
                       bckt = bckt + expose(x,y)
                    enddo

c  check for the case where there are very few counts in the background
c  annulus.  If less than 4, use the average background for the entire
c  field to calculate the model counts.

                    if(bckc .lt. 4.) then
                       srcmc = srct*averat
                    else
                       srcmc = srct*bckc/bckt
                    endif

                    prob = gammq(srcc,srcmc)
                    if(prob .gt. sig)then

c  Measure the position (I)

                       if(ib1 .eq. 0) then
                          radin = nint(pixrad*2.0)
                       else
                          radin = nint(pixrad*1.3)
                       endif

                       sumx = ix*counts(ix,iy)*expose(ix,iy)
                       sumy = iy*counts(ix,iy)*expose(ix,iy)
                       weit = counts(ix,iy)*expose(ix,iy)

                       do i=1,hash(radin)
                          x = ix + listx(i)
                          y = iy + listy(i)
                          sumx = sumx+x*counts(x,y)*
     >                                      expose(x,y)
                          sumy = sumy+y*counts(x,y)*
     >                                      expose(x,y)
                          weit = weit+counts(x,y)*expose(x,y)
                       enddo
                       srcx = sumx/weit
                       srcy = sumy/weit

c  Measure the position even better
                       srcx = nint(srcx+0.5)
                       srcy = nint(srcy+0.5)
                       radin1 = nint(pixrad)
                       if(radin1 .lt. 1) radin1 = 1
                       if(ib1 .eq. 0) then
                          radin2 = 2.*radin1
                       else
                          radin2 = radin1
                       endif
ccc                       tsrcx=sngl(srcx)
ccc                       tsrcy=sngl(srcy)
                       tsrcx=srcx
                       tsrcy=srcy
                       sumx = srcx*counts(tsrcx,tsrcy)*
     +                                  expose(tsrcx,tsrcy)
                       sumy = srcy*counts(tsrcx,tsrcy)*
     +                                  expose(tsrcx,tsrcy)
                       weit = counts(tsrcx,tsrcy)*
     +                                  expose(tsrcx,tsrcy)


                       do i=1,hash(radin1)
                          x = srcx + listx(i)
                          y = srcy + listy(i)
                          sumx = sumx + x*counts(x,y)*
     +                                      expose(x,y)
                          sumy = sumy + y*counts(x,y)*
     +                                      expose(x,y)
                          weit = weit + counts(x,y)*
     +                                      expose(x,y)
                       enddo
                       srcx = sumx/weit
                       srcy = sumy/weit
                       srcx = nint(srcx+0.5)
                       srcy = nint(srcy+0.5)

c  Measure the count rate
                       if(expose(tsrcx,tsrcy) .gt. 0) then
                          srcc = counts(tsrcx,tsrcy)
                          srct = expose(tsrcx,tsrcy)
                          npix = 1
                       endif

                       do i=1,hash(radin1)
                          x = srcx + listx(i)
                          y = srcy + listy(i)
                          if(expose(x,y) .gt. 0) then
                             srcc = srcc + counts(x,y)
                             srct = srct + expose(x,y)
                             npix = npix + 1
                          endif
                       enddo
                       bckc = 0
                       bckt = 0

                       do i=hash(radin2)+1,hash(radou)
                          x = srcx + listx(i)
                          y = srcy + listy(i)
                          bckc = bckc + counts(x,y)
                          bckt = bckt + expose(x,y)
                       enddo
                       
                       srcmc = srct*bckc/bckt
                       prob = gammq(srcc,srcmc)

                       srcr = npix*srcc/srct
                       bckr = npix*bckc/bckt
                       rate = (srcr-bckr)/enc
                       rate_err = srcc/(srct*srct) + bckc/(bckt*bckt)
                       rate_err = sqrt(rate_err)*npix/enc
                       srcr = srcr*sca1
                       bckr = bckr*sca1

                      
                       if((prob .gt. sig) .and.
     +                               (rate .gt. cr)) then

c  Add to list of known sources

                      if((srcxs(numsrc) .eq. srcx) .and. 
     >                   (srcys(numsrc) .eq. srcy)) then
                      
                      else   
                          numsrc = numsrc + 1
                          if(numsrc .gt. totsrc) then
                             subinfo='Maximum no. of source to be '
     >                               //'detected is set to 400'
                             call wtinfo(chatter,0,1,subinfo)
                             subinfo='May be, you need to increase '
     >                              //'significance level'
                             call wtinfo(chatter,0,1,subinfo)
                             errflg=1
                             return
                          endif
                          srcxs(numsrc) = srcx
                          srcys(numsrc) = srcy
                          srcrs(numsrc) = rate
                          srces(numsrc) = rate_err
                          srcps(numsrc) = prob

                          pixrad_out(numsrc)= pixrad
                          radin_sav(numsrc) = radin

                      endif
c                     for srcx and srcy not coinciding with earlier one
                       endif
c                      above endif is for tstpnt > 0 loop

c  Point was probable

                    endif
                 endif

c  Point was not within another PSF

               endif

c  End of considering a valid point (I)

             endif

c  Increment y position by a fraction of a PSF

             if(ib1 .eq. 0) then
                radin = nint(0.5*pixrad)
             else
                radin = nint(0.33*pixrad)
             endif

             if(radin .lt. 1) radin = 1
                iy = iy + radin

c        End of the y direction loop

          call xclock(ix,maxx,10)
         enddo

30       continue
c  Increment the x postion by a fraction of the smallest pixel 

         if(ib1 .eq. 0) then
            dist = 0.083333333*abs(ix-nxby2_50)
            call psf_hri(dist,rad,chatter,errflg)
            pixrad = rad/5.0
            radin = nint(0.5*pixrad)
         else
            dist = pix_in_min * abs(ix-nxby2_50)
            call psf_pspc(e,dist,enc,rad,chatter,errflg)
            pixrad = rad/pix_in_min
            radin = nint(0.33*pixrad)
         endif

         if(radin .lt. 1) radin = 1
            ix = ix + radin

c     End of the x direction loop

      enddo

c Output

      isize = evt_nx
      if(ib1 .eq. 0) then
         iflag = -1
      else
         iflag = 1
      endif

c convert X and Y to RA and DEC
      type=evt_ctype(5:8)
      do i=1,numsrc
         tmp_ra=0.d0
         tmp_dec=0.d0
         status=0
         sx=real(srcxs(i)-50)
         sy=real(srcys(i)-50)
         
         call ftwldp(dble(sx),dble(sy),evt_crval1,evt_crval2,
     >               evt_crpix1,evt_crpix2,evt_cdelt1,evt_cdelt2,
     >               evt_crot,type,tmp_ra,tmp_dec,status)
      
            if(status .ne. 0)then
              subinfo='returning from ftwldp'
              call wterrm(subname,version,subinfo)
              errflg=1
              return
            endif

         ra(i)  = tmp_ra
         dec(i) = tmp_dec 


         if((srcps(i) .gt. 0.9999999999999988d0) .and. 
     >      (srcps(i) .le. 1.000000000d0)) then
             significance(i)='> 8  sigma'
         elseif((srcps(i) .gt. 0.9999999999999361d0) .and. 
     >          (srcps(i) .le. 0.9999999999999988d0)) then
                 significance(i)='> 7.5sigma'
         elseif((srcps(i) .gt. 0.9999999999974403d0) .and.
     >          (srcps(i) .le. 0.9999999999999361d0)) then
                 significance(i)='> 7  sigma'
         elseif((srcps(i) .gt. 0.9999999999196800d0) .and.
     >          (srcps(i) .le. 0.9999999999974403d0)) then
                 significance(i)='> 6.5sigma'
         elseif((srcps(i) .gt. 0.9999999980268247d0) .and.
     >          (srcps(i) .le. 0.9999999999196800d0)) then
                 significance(i)='> 6  sigma'
         elseif((srcps(i) .gt. 0.9999999620208751d0) .and.
     >          (srcps(i) .le. 0.9999999980268247d0)) then
                 significance(i)='> 5.5sigma'
         elseif((srcps(i) .gt. 0.9999994266968563d0) .and.
     >          (srcps(i) .le. 0.9999999620208751d0)) then
                 significance(i)='> 5  sigma'
         elseif((srcps(i) .gt. 0.9999932046537505d0) .and.
     >          (srcps(i) .le. 0.9999994266968563d0)) then
                 significance(i)='> 4.5sigma'
         elseif((srcps(i) .gt. 0.9999366575163338d0) .and.
     >          (srcps(i) .le. 0.9999932046537505d0)) then
                 significance(i)='> 4  sigma'
         elseif((srcps(i) .gt. 0.9995347418419290d0) .and.
     >          (srcps(i) .le. 0.9999366575163338d0)) then
                 significance(i)='> 3.5sigma'
         elseif((srcps(i) .gt. 0.9973002039367398d0) .and.
     >          (srcps(i) .le. 0.9995347418419290d0)) then
                 significance(i)='> 3  sigma'
         elseif((srcps(i) .gt. 0.9875806693484477d0) .and.
     >          (srcps(i) .le. 0.9973002039367398d0)) then
                 significance(i)='> 2.5sigma'
         elseif((srcps(i) .gt. 0.9544997361036416d0) .and.
     >          (srcps(i) .le. 0.9875806693484477d0)) then
                 significance(i)='> 2  sigma'
         elseif((srcps(i) .gt. 0.8663855974622838d0) .and.
     >          (srcps(i) .le. 0.9544997361036416d0)) then
                 significance(i)='> 1.5sigma'
         elseif((srcps(i) .gt. 0.6826894921370858d0) .and.
     >          (srcps(i) .le. 0.8663855974622838d0)) then
                 significance(i)='> 1  sigma'
         else
             significance(i)='< 1 sigma'
         endif

         radius(i) = pixrad_out(i) * pix_in_min
         if(i .eq. 1) then
            dist_bet_two_src(i) = 0.
         else
            dist_bet_two_src(i) =sqrt(float(srcxs(i-1) -srcxs(i))**2 + 
     >                float(srcys(i-1) -srcys(i))**2 )
            dist_bet_two_src(i) = pix_in_min * dist_bet_two_src(i)
         endif

         offang(i) = pix_in_min*sqrt(float(nxby2_50 - srcxs(i))**2 + 
     >                               float(nxby2_50 - srcys(i))**2)
      enddo
 
c     write to ascii file diag_b1_b2.dat

      call ftgiou(ounit1,errflg)
      open(unit=ounit1,file=diagfil, status='unknown')

      write(ounit1,'(1x,a)')
     >'no.  X    Y        RA            DEC    '    
     >//'    cts/s   error   sigma limit radial' 
      write(ounit1,'(16x,a,37x,a)') 'h  m  s       d  m  s',
     >                              'dist'

c     filter out duplicate sources
c     if distance between two sources > 0.9*PSF, then it is the same
c     source

      nsrc = 0
      do j=1,numsrc

         if((j .eq. 1) .or. 
     >                  (dist_bet_two_src(j) .gt. radius(j-1)))then
            nsrc = nsrc +1 
c           convert ra(i) to hr,min,sec and dec(i) to deg,min,sec for output 

            ra(j)  = ra(j)/15.d00
            ra_hr  = int(ra(j))
            tmp_ra = (ra(j) - ra_hr)*60.d00
            ra_min = int(tmp_ra)
            ra_sec = (tmp_ra - ra_min)*60.d00

            dec_deg = int(dec(j))
            tmp_dec= abs((dec(j) - dec_deg)*60.d00)
            dec_min = int(tmp_dec)
            dec_sec = (tmp_dec - dec_min)*60.d00


            if(ib1 .eq. 0) then
               radin_sav(j) = nint(5.0*pixrad_out(j))
            else
               radin_sav(j) = nint(1.5*pixrad_out(j))
            endif
c           if masking file is asked for as output, then do the masking
c           Mask out the area
            srcx=srcxs(j)
            srcy=srcys(j)
            if(qmask)then
               mask(int(srcx),int(srcy)) = 0.
               do i=1,hash(radin_sav(j))
                  x = srcx + listx(i)
                  y = srcy + listy(i)
                  mask(x,y) = 0.
               enddo
            endif

c           make a circle with 1.5*psf/5.0*psf around the source
            do i=0,radin_sav(j)
               ydist = sqrt(real(radin_sav(j)*radin_sav(j) - i*i))
               y1 = srcy + nint(ydist)
               y2 = srcy - nint(ydist)
               x1 = srcx + i
               x2 = srcx - i
               circle(x1,y1) = 1.
               circle(x1,y2) = 1.
               circle(x2,y1) = 1.
               circle(x2,y2) = 1.
            enddo
            do i=0,radin_sav(j)
               xdist = sqrt(real(radin_sav(j)*radin_sav(j) - i*i))
               x1=srcx + nint(xdist)
               x2=srcx - nint(xdist)
               y1=srcy + i
               y2=srcy - i
               circle(x1,y1) = 1.
               circle(x1,y2) = 1.
               circle(x2,y1) = 1.
               circle(x2,y2) = 1.
            enddo
     
            write_file=.true.
            if(j .gt. 1) then
               do i=j-1,1,-1
                  if(((srcxs(j)-50) .eq. (srcxs(i)-50)) .and.
     >               ((srcys(j)-50) .eq. (srcys(i)-50))) then
                     nsrc = nsrc -1
                     write_file=.false.
                     goto 200
                  endif
               enddo
            endif
200         if(write_file) then
               write(ounit1,'(i3,i5,i5,2x,i3,i3,f6.2,2x,i3,
     >                     i3,f6.2,1x,f8.4,a,f6.4,3x,a11,f5.1)') 
     >                nsrc,srcxs(j)-50,srcys(j)-50,ra_hr,ra_min,ra_sec,
     >                dec_deg,dec_min,dec_sec,srcrs(j),'+/-',srces(j),
     >                significance(j),offang(j)
            endif
         endif
      enddo 

      do iy=1,evt_nx
         do ix=1,evt_nx
            new_inr4(ix,iy) = mask(ix+50,iy+50)
         enddo
      enddo

      call ftclos(ounit1,errflg)
      call ftfiou(ounit1,errflg)

      write(subinfo,'(a,i4)') 'number of sources detected = ',nsrc
      call wtinfo(chatter,0,1,subinfo)
 
c --------------- write to output file ----------------------------

c write to mask file

      if (qmask) then
         call wt_detect(outmask,iflag,isize,new_inr4,SNGL(evt_cdelt1),
     >               SNGL(evt_cdelt2),SNGL(evt_crval1),SNGL(evt_crval2),
     >                  evtmapfil, expmapfil,errflg,chatter)

         if(errflg .ne. 0)then
           subinfo='returning from wt_detect'
           call wterrm(subname,version,subinfo)
           return
         endif
      endif

c write to image file

      call wt_image(outfil,evt_nx,evt_ny,circle,errflg,chatter)
      
      if(errflg .ne. 0)then
        subinfo='returning from wt_image'
        call wterrm(subname,version,subinfo)
        return
      endif
       
c --------------- end of writing output ---------------------------

      return
      end

c -----------------------------------------------------------------------
c                     END OF DO_DETECT
c -----------------------------------------------------------------------

*+WT_DETECT

      subroutine wt_detect(outfil,iflag,isize,inr4,cdelt1,cdelt2,
     >                     crval1,crval2,evtmapfil, expmapfil,
     >                     errflg,chatter)

c ----------------------------------------------------------------------
c this subroutine writes the fits output file
c ----------------------------------------------------------------------

      implicit none
      character*(*) outfil, evtmapfil, expmapfil
      integer iflag,isize,errflg,chatter
      real inr4(isize,isize)
      real cdelt1,cdelt2,crval1,crval2

c ------------------- internals ----------------------------------------
      
      character(100) subinfo, comments(8)
      character(8) telescop,instrum,detnam,filter
      character(8) ctype1,ctype2,cunit1,cunit2
      character(8) bunit,radecsys 
      real equinox,crpix1,crpix2   
      integer ounit2,block,nk_hist,nk_comm
      character(80) hist(10)
      logical deadapp,vignapp

c ---------------- authors/modifications ---------------------------------
c Banashree M Seifert (Oct 1996) 1.0.0:
c
c ------------------------------------------------------------------------
      character(10) subname
      parameter (subname='wt_detect')
      character(5) version
      parameter (version='1.0.0')
*-
c ------------------------------------------------------------------------
      subinfo= 'using '//subname//'Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c --------------------------------------------------------------------------

      errflg=0
      call ftgiou(ounit2,errflg)
      errflg=0
      call ftinit(ounit2,outfil,block,errflg)

c --- set values that are fixed for this task ---

      telescop= 'ROSAT'
      if(iflag .gt. 0) then
         instrum= 'PSPC'
      else
         instrum= 'HRI'
      endif

      detnam = '  '
      filter = '  '
      deadapp=.false.
      vignapp=.false.
      radecsys = 'FK5'
      equinox = 2.00000e+3
      crpix1 =isize/2. + 0.5      
      crpix2 =isize/2. + 0.5
      ctype1 = 'RA---TAN'
      ctype2 = 'DEC--TAN'
      cunit1 = 'deg'
      cunit2 = 'deg'
      bunit =' '

      hist(1) = 'Input eventmap file: '//evtmapfil
      hist(2) = 'Input exposure map file: '//expmapfil
      nk_hist = 2
      comments(1)='This file was produced using the ESAS software '
      comments(2)='written by S. L. Snowden'
      nk_comm = 2

      errflg=0
      call wtexm1(ounit2,telescop,instrum,detnam,filter,
     >                  deadapp,vignapp,radecsys,equinox,ctype1,
     >                  ctype2,crpix1,crpix2,cunit1,cunit2,bunit,
     >                  crval1,crval2,cdelt1,cdelt2,
     >                  isize,inr4,nk_hist,hist,
     >                  nk_comm,comments,chatter,errflg)

      if(errflg .ne. 0) then
         subinfo='returning from wtexm1'
         call wterrm(subname,version,subinfo)
         return
      endif

c  write some useful header keywords

      call ftpkys(ounit2,'OBS_MODE','pointing',
     >          'OBS MODE: POINTING,SLEW, OR SCAN',errflg)

      call timestamp(ounit2)

      errflg=0
      call ftclos(ounit2,errflg)
      call ftfiou(ounit2,errflg)

      return
      end

c -------------------------------------------------------------------
c                  END OF WT_DETECT
c --------------------------------------------------------------------

*+PSF_HRI

      subroutine psf_hri (ang,rad,chatter,errflg)

c -------------------------------------------------------------
c taken from Steves psf_hri.for
c this is just his program copied
c Banashree M Seifert (Oct 1996) 1.1.0:
c
c ------------------------------------------------------------
      implicit none
      real ang, rad
      integer chatter,errflg

c -------------- internals -------------------------------------

      character(100) subinfo
      integer i, ia, ic, ierr
      real aang, r50(161)

c ----------- authors/modifications --------------------------
c Banashree M. Seifert (Oct 1996) 1.0.0:
c
c ------------------------------------------------------------
      character(8) subname
      parameter (subname='psf_hri')
      character(5) version
      parameter (version='1.0.0')
 
      data ic /0/
*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,40,2,subinfo)

      ierr = 0

      if(ic .eq. 0) then
         ic = 1
         do i=1,161
            aang = 0.25*(i-1)
            r50(i) = 1.175*sqrt(0.74**2 + 1.00**2 + 
     +                  (1.30**2 + 0.0205*aang**2.349)**2)
         enddo
      endif

      ia = ang*4. + 1
      if(ia .gt. 161) ia = 161
      rad = r50(ia)

      errflg=0
      return
      end  

c ------------------------------------------------------------------
c                       END OF PSF_HRI
c -------------------------------------------------------------------------   

*+PSF_PSPC
      subroutine psf_pspc (e,ang,enc,rad,chatter,errflg)

c ---------------------------------------------------------------------
c This routine returns the radius (rad) of the PSF
c 
c This is taken from Steve Snowden's PSF.FOR
c Modification to Steves code that it calls the ROSLIB routine 
c PSFOFF to calculate the PSF
c e     r   i/p   energy in eV 
c ang   r         off axis angle in arcmin
c ene   r         energy [keV]
c eps   r      	  off-axis angle [arc min]
c x     r         angle from target position [arcsec]
c rad   r         in arcmin radius of 90%psf
c ierr  i   o/p   = 0 no error 
c                 = 1 energy outside bounds 0.07-3 keV
c                 = 3 
c
c
c This is modified by Banashree M Seifert (Oct 1996) from Steve Snowden's
c code to accommodate roslib function subroutine psfoff
c ----------------------------------------------------------------------
      implicit none
      real e,ang,enc,rad
      integer chatter,errflg

c ------------- internals ------------------------------------------
      character(100) subinfo
      real circ(61), ene, eps
      real pi, psf, psfa(605), psfoff
      real sum, tot, x 
      integer  ia, ic, ieps, ierr, j, kk
      data ic /0/
      SAVE circ

c --------------- authors/modifications -----------------------------
c Banashree M. Seifert (Oct 1996) 1.0.0:
c          . replaced Steves pi=3.1415927 by pi=4.*atan(1.)
c -----------------------------------------------------------------------
       
      character(9) subname
      parameter (subname='psf_pspc')
      character(5) version
      parameter (version='1.0.1')
*-

      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,40,2,subinfo)
c -----------------------------------------------------------------------
      pi=4.*atan(1.)
      ierr = 0
      if (ic .eq. 0) then
        do kk = 1,61
          circ(kk) = 0.
        enddo
      endif

      if((e .ge. 70.) .and. (e .le. 3000.) .and. (ic .eq. 0)) then
          ene = e/1000.
          do ieps = 0,60
             eps = ieps
             tot = 0.
             x = 0

c            calculate PSF at an interval of 5arcsec from the source
             do j=1,601
                psf=psfoff(ene,eps,x,ierr)

                if(j .eq. 1) then
                   psfa(j) = 1.e6*psf*(0.5*0.5)
                else
                   psfa(j) = 1.e6*psf*((j-0.5)*(j-0.5) - 
     +                          (j-1.5)*(j-1.5))
                endif
                tot = tot + psfa(j)
                x = x + 5.
             enddo
             sum = 0.
             j = 0
             tot = enc*tot
             do while (sum .lt. tot)
                j = j + 1
                sum = sum + psfa(j)
             enddo
             circ(ieps+1) = (j-1.)/12.
          enddo
          ic = 1
      endif
      ia = nint(ang) + 1
      if(ia .gt. 61) ia = 61
      rad = circ(ia)

      return
      end     
c ---------------------------------------------------------------
c                  END OF PSF_PSPC
c ---------------------------------------------------------------

*+READ_MAP
      subroutine read_map(mapfil,nx,ny,crval1,crval2,
     >                    cdelt1,cdelt2,crpix1,crpix2,rot,
     >                    crot,ctype,imgmap,chatter,errflg)

c --------------------------------------------------------------------
c This subroutine reads a mapfile (image file) and returns
c CRVAL*,CDEL*,nx,ny,and an array image
c ------------------------------------------------------------------------------
      implicit none
      character*(*) mapfil,ctype
      double precision cdelt1,cdelt2,crval1,crval2,crpix1,crpix2
      double precision crot
      integer nx,ny
      real imgmap(nx,*)
      integer chatter,errflg
      logical rot

c ----------------- internals ----------------------------------------
      character(100) subinfo
      integer iunit,block,status,group,nullval
      character(80) comm
      character(8) instrument
      logical anyfl

c ------------ authors/modifications -------------------------------
c Banashree M Seifert (Oct 1996) 1.0.0:
c
c -------------------------------------------------------------------
      character(9) subname
      parameter (subname='read_map')
      character(5) version
      parameter (version='1.0.0')

*-
c ------------------------------------------------------------------------
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c ------------------------------------------------------------------------

      status=0
      call ftgiou(iunit,status)
      call ftopen(iunit,mapfil,0,block,status)

ccc check for the instrument if PSPC -- go ahead
ccc                             HRI  -- bail out with error

       call ftgkys(iunit,'INSTRUME', instrument, comm, status)
       if (status .ne. 0) then
           subinfo='reading INSTRUME keyword'
           call wterrm(subname,version,subinfo)
           errflg=1
           return
       endif

       if(instrument(1:4) .ne. 'PSPC') then
          subinfo='  '
          call wterrm(subname,version,subinfo)
          subinfo='Sorry!! at present only PSPC is supported'
          call wtinfo(chatter,0,1,subinfo)
          subinfo='your input file INSTRUMENT is not PSPC'
          call wtinfo(chatter,0,1,subinfo)
          errflg=1
          return
       endif

c     get EVENTMAP sizes
      status=0
      call ftgkyj(iunit,'NAXIS1',nx,comm,status)
      if (status .ne. 0) then
          subinfo='reading NAXIS1 keyword'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif
      call ftgkyj(iunit,'NAXIS2',ny,comm,status)
      if (status .ne. 0) then
          subinfo='reading NAXIS2 keyword'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

c     read the eventmap data
      group = 0
      nullval = 0
      call ftg2de(iunit,group,nullval,nx,nx,ny,imgmap,anyfl,status)
      if (status.ne.0) then
          subinfo='reading expmap array from '//mapfil
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

c     read the required keywords CDELT*, CRVAL*, CRPIX*, CROT, CTYPE

       call ftgkyd(iunit,'CDELT1',cdelt1,comm,status)
       subinfo = 'reading CDELT1 keyword from '//mapfil
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif
 
       call ftgkyd(iunit,'CDELT2',cdelt2,comm,status)
       subinfo = 'reading CDELT2 keyword from '//mapfil
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif
 
       call ftgkyd(iunit,'CRPIX1',crpix1,comm,status)
       subinfo = 'reading CRPIX1 keyword from '//mapfil
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif
 
       call ftgkyd(iunit,'CRPIX2',crpix2,comm,status)
       subinfo = 'reading CRPIX2 keyword from '//mapfil
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif
 
       call ftgkyd(iunit,'CRVAL1',crval1,comm,status)
       subinfo = 'reading CRVAL1 keyword from '//mapfil
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif
 
       call ftgkyd(iunit,'CRVAL2',crval2,comm,status)
       subinfo = 'reading CRVAL2 keyword from '//mapfil
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif
 
       call ftgkys(iunit,'CTYPE1',ctype,comm,status)
       subinfo = 'reading CTYPE1 keyword from '//mapfil
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif
 
       crot=0.d0
       if(rot) then
          call ftgkyd(iunit,'CROTA2',crot,comm,status)
          subinfo = 'reading CROTA2 keyword from '//mapfil
          if (status .ne. 0) then
              call wtinfo(chatter,15,2,subinfo)
              subinfo='assuming rotation =0.'
              call wtinfo(chatter,15,2,subinfo)
              crot=0.d0
          endif
       endif
 
c --------------------------------------------------------------
      call ftclos(iunit,status)
      call ftfiou(iunit,status)

      errflg=0
      return
      end

c --------------------------------------------------------------------
c                    END OF READ_MAP
c --------------------------------------------------------------------


*+
      double precision function gammq(a,x)
      real*8 gamser
      real*8 a,x,gln
      if((x .lt. 0.) .or. (a .le. 0.)) then
          print *, 'error in gammq',x,a
          gammq = 0.
          return
      endif
      if(x .lt. a+1.)then
        call gser(gamser,a,x,gln)
        gammq = 1. - gamser
      else
        call gcf(gammq,a,x,gln)
      endif
      return
      end
c
c
c
      subroutine gcf(gammcf,a,x,gln)
      parameter (itmax=300,eps=3.e-7)
      real*8 dtemp,gammln,a,x,gln,a0,a1,b0,b1,fac,gold
      double precision gammcf
      real*8 an,ana,anf,g
      gln = gammln(a)
      gold = 0.
      a0 = 1.
      a1 = x
      b0 = 0.
      b1 = 1.
      fac = 1.
      g = 0.
      do 11 n=1,itmax
        an = float(n)
        ana = an - a
        a0 = (a1+a0*ana)*fac
        b0 = (b1+b0*ana)*fac
        anf = an*fac
        a1 = x*a0 + anf*a1
        b1 = x*b0 + anf*b1
        if(a1 .ne. 0.) then
          fac = 1./a1
          g = b1*fac
          if(dabs((g-gold)/g).lt.eps)go to 1
          gold = g
        endif
11    continue
      print *, 'a too large, itmax too small',a
1     dtemp = -x + a*dlog(x) - gln
      gammcf = dexp(dtemp)*g
      return
      end
c
c
c
      subroutine gser(gamser,a,x,gln)
      parameter (itmax=300,eps=3.e-7)
      real*8 dtemp, gammln, gamser, gln,a,x,ap,sum,del
c
      gln = gammln(a)
      if(x .le. 0.)then
        if(x .lt. 0.)pause
        gamser = 0.
        return
      endif
      ap=a
      sum=1./a
      del=sum
      do 11 n=1,itmax
        ap=ap+1.
        del=del*x/ap
        sum=sum+del
        if(dabs(del).lt.dabs(sum)*eps)go to 1
11    continue
      print *, 'a too large, itmax too small'
1     dtemp = -x + a*log(x) - gln
      gamser=sum*exp(dtemp)
      return
      end
c
c
c
      double precision function gammln(xx)
      real*8 cof(6),stp,half,one,fpf,x,tmp,ser,xx
      data cof,stp/76.18009173d0,-86.50532033d0,24.01409822d0,
     *    -1.231739516d0,.120858003d-2,-.536382d-5,2.50662827465d0/
      data half,one,fpf/0.5d0,1.0d0,5.5d0/
      x=xx-one
      tmp=x+fpf
      tmp=(x+half)*log(tmp)-tmp
      ser=one
      do 11 j=1,6
        x=x+one
        ser=ser+cof(j)/x
11    continue
      gammln = tmp + log(stp*ser)
      return
      end

c -----------------------------------------------------------------------
c               END OF FUNCTION SUBROUTINES
c -----------------------------------------------------------------------

*+WT_IMAGE

      subroutine wt_image(outfil,nx,ny,circle,errflg,chatter)

c -----------------------------------------------------------------------
c this subroutine puts a circle of 1.5* 90% PSF around a detected source
c the image file is same as input image file with circles drawn around
c the sources
c -----------------------------------------------------------------------

      implicit none
      character*(*) outfil
      integer nx,ny,errflg,chatter
      real circle(nx+100,*)

c ----------- internal variables ------------------------------------------
      character(120) subinfo
      integer block,ounit,status
      integer group,nullval,i,j
      real imgmap(2048,2048)
      logical anyfl

c ----------- authors/modifications ---------------------------------------
c Banashree M Seifert (1.0.0: Dec 1996)
c
c -------------------------------------------------------------------------

      character(9) subname
      parameter (subname='wt_image')
      character(5) version
      parameter (version='1.0.0')

      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)


c open the file copied from input image
      status=0
      call ftgiou(ounit,status)
      call ftopen(ounit,outfil,1,block,status) 

c     read the eventmap data
      status=0
      group = 0
      nullval = 0
      call ftg2de(ounit,group,nullval,nx,nx,ny,imgmap,anyfl,status)
      if (status.ne.0) then
          subinfo='reading image array from : '//outfil
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif


      do i=1,nx
         do j=1,ny
            if(circle(50+i,50+j) .eq. 1) then
               imgmap(i,j) = 1
            endif
         enddo
      enddo

c --- WRITE DATA ---

      status = 0
      call ftp2de(ounit,0,nx,nx,ny,imgmap,status)
      if (status .ne. 0) then
        subinfo = ' writing primary data (exp map)'
        call wterrm(subname,version,subinfo)
        errflg = 3
        return
      ENDIF
      subinfo = ' sources have been marked'
      call wtinfo(chatter,20,2,subinfo)

      call ftclos(ounit,status)
      call ftfiou(ounit,status)

      errflg=0
      return
      end

c ------------------------------------------------------------------------
c                  end of wt_image
c ------------------------------------------------------------------------

*+FIND_SIZE

      subroutine findsize(fil,nx,chatter,errflg)

c ---------------------------------------------------------------------
c subroutine to get the array size and allocate dynamic memory
c ---------------------------------------------------------------------
      implicit none
      character*(*) fil
      character(20) comm
      integer nx,chatter,errflg

C     Local variables
      character(80) subinfo
      integer iunit,status,block

c ------------------------------------------------------------------
      character(9) subname
      parameter (subname='findsize')
      character(5) version
      parameter (version='1.0.0')
c ------------------------------------------------------------------
      status=0
      call ftgiou(iunit,status)
      call ftopen(iunit,fil,0,block,status)

c     get EVENTMAP sizes
      status=0
      call ftgkyj(iunit,'NAXIS1',nx,comm,status)
      if (status .ne. 0) then
          subinfo='reading NAXIS1 keyword'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

      call ftfiou(iunit,status)
      call ftclos(iunit,status)

      return
      end

c ---------------------------------------------------------------------
c                 end of find_size
c ---------------------------------------------------------------------


*+EXP_REMAP

      subroutine exp_remap(expmap,newmap,nx,factor,chatter,errflg)

c ----------------------------------------------------------------------
c This subroutine remaps the exposure map (which is output from
c pcexpmap).  The output from pcexpmap is a 512x512 exposure map.
c Now, to run detect, we need the remap this 512x512 exposure map to
c the input image file, which could be 512X512, 1024x1024,2048x2048
c in case of 512x512  --> factor=1
c           1042x1024 --> factor=2
c           2048x2048 --> factor=3
c
c                  input image file dimension
c since,  factor= ----------------------------
c                             512
c ---------------------------------------------------------------------- 

      implicit none
      integer nx, factor
      real expmap(512,*),newmap(nx,*)
      integer chatter, errflg

c --------------- internal variables ------------------------------------
      character(80) subinfo
      integer start_x, start_y, end_x, end_y
      integer i,j,ii,jj,factor_sq 
  
c ------------- authors/modifications ------------------------------------
c
c Banashree M Seifert (Dec 1996; 1.0.0):
c -----------------------------------------------------------------------

      character(10) subname
      parameter (subname='exp_remap')
      character(5) version
      parameter (version='1.0.0')

c -----------------------------------------------------------------------

      subinfo='using '//subname// 'Ver '//version
      call wtinfo(chatter,20,1,subinfo)

c ----------------------------------------------------------------------
      factor_sq= factor*factor

      if(factor .eq. 1) then
         do i=1,512
            do j=1,512
               newmap(i,j) = expmap(i,j)
            enddo
         enddo

      else

         do j=1,512
            start_y= (j-1)*factor + 1
            end_y = j*factor

            do i=1,512
               start_x= (i-1)*factor + 1
               end_x =  i*factor
            
               do jj=start_y, end_y
                  do ii=start_x, end_x
                     newmap(ii,jj) = expmap(i,j)
                  enddo
               enddo

            enddo

         enddo

      endif

      errflg=0

      return
      end

c -----------------------------------------------------------------------
c                 end of exp_remap
c -----------------------------------------------------------------------





