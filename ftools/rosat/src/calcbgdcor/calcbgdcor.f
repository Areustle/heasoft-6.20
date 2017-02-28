
*+CALCBGDCOR

      subroutine calcbr

c ----------------------------------------------------------------
c This task remaps (resacles) the background spectra to the source 
c spectra which is to be used to read directly into XSPEC
c
c ------------------ routines called ---------------------------------
c bgdcor_gp        gets parameter
c get_array_size    gets array size of the input files
c calc_sky_bckgrnd  calculates the sky bckgrnd spectra from total 
c                   backgrnd spectra
c calc_arf_ratio    calculates the ratio of ARF's between source and 
c                   bckgrnd spectra
c arf_to_chan       converts the ARF energy range to channel no.
c
c sky_bckgrnd_arf_correction 
c                   corrects sky bckgrnd by multiplying the two ARFs 
c                   ratios
c
c -------------------------- variables ------------------------------+

      implicit none
      character(180) srcfil,bckfil,arf_srcfil,arf_bckfil,rmffil
      character(180) particle_srcfil,particle_bckfil,expmapfil
      character(180) outfil,regfil_sou,regfil_bck 
      character(100) subinfo
      integer phasize,arfsize,iget,ineed
      integer chatter,errflg,status
      real backscal_src,backscal_bck
      logical killit

c for DMA
      integer p_sky_bck,p_arf_lo,p_arf_hi,p_arf_ratio
      integer p_e_min,p_e_max,p_arf_remap,p_sky_bck_arf_corr
      integer p_sky_bck_arf_exp_corr
      integer p_src_bck_corr

c ------------------ variable definitions ---------------------------
c srcfil          char input   source spectrum
c bckfil          char input   bkgrnd spectrum
c arf_srcfil      char input   source ARF spectrum
c arf_bckfil      char input   bkgrnd ARF spectrum
c particle_srcfil char input   source particle spectrum
c particle_bckfil char input   bkgrnd particle spectrum
c outfil          char input   output filename 
c rmffil          char input   RMF filename to convert ARF e_lo and e_hi 
c                              to channel no.
c expmapfil       char input   exposure map filename(output from task
c                              PCEXPMAP
c phasize         int          size of input PHA
c arfsize         int          size of input ARF
c iget            int          memory size allocated for this task
c ineed           int          memory size needed for this task (iget and
c                              ineed should be equal)
c                              calling subroutine 'remap'
c sky_bck         real         sky background reading
c sky_bck_arf_corr real        sky background reading corrected for ARF 
c                              ratios
c arf_lo          real         e_lo for ARF 
c arf_hi          real         e_hi for ARF
c e_min           real         e_lo for RMF
c e_max           real         e_hi for RMF
c arf_remap       real         remapped arf_ratios acc. to channel no.
c                              of RMF instead of energies
c arf_ratio       real         ratio of the response matrices of two ARFs
c
c --------------- authors/modification -------------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c
c Banashree Mitra Seifert (Sept, 1996) 1.1.0:
c              . accuracy set to 0.001     
c
c Banashree Mitra Seifert (Sept, 1996) 1.2.0:
c              . initialised variable errflg=0 which gives problems in LINUX
c Peter D Wilson (June 29, 1998) 1.2.1:
c              . Added max_xflt parameter to rdpha1 function call
c Peter D Wilson (June 30, 1998) 1.2.2:
c              . Updated for new FCPARS behavior
c Ning Gan(July 2, 1998) 1.2.3:
c              . Modified the date string length to 68.
c toliver (July 9, 1999) 1.2.4:
c              . Correction in BGDCOR_GP
c Peter D Wilson (Aug 30, 1999) 1.3.0
c              . Add POINT, ELLIPSE, and ANNULUS region support
c --------------------------------------------------------------------
      character(5) version
      parameter (version = '1.3.0')
      character(11) subname
      parameter (subname='calcbgdcor')
      character(21) taskname
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

c ----------------- get parameters ------------------------------------+

      taskname=subname//'Ver '//version

      call bgdcor_gp(srcfil,bckfil,arf_srcfil,arf_bckfil,
     >               particle_srcfil,particle_bckfil,outfil,
     >               rmffil,expmapfil,regfil_sou,regfil_bck,
     >               errflg,chatter,killit)

      if (errflg .ne. 0) then
          subinfo='returning from bgdcor_gp'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c ----------------- task begins from here -----------------------------+

      call wtbegm(subname,version,chatter)

c ----------------- get array size for allocating DMA -----------------+
 
      call get_array_size(srcfil,arf_srcfil,phasize,arfsize,
     >                    backscal_src,errflg,chatter)
 
      if (errflg .ne. 0) then
          subinfo='returning from get_array_size'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_sky_bck = 0
      p_arf_lo = 0
      p_arf_hi = 0
      p_arf_ratio = 0
      p_e_min = 0
      p_e_max = 0
      p_arf_remap = 0
      p_sky_bck_arf_corr = 0
      p_sky_bck_arf_exp_corr = 0
      p_src_bck_corr = 0

      iget=0
      status = 0
      call udmget(phasize, 6, p_sky_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4

      status = 0
      call udmget(arfsize, 6, p_arf_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+arfsize*4

      status = 0
      call udmget(arfsize, 6, p_arf_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+arfsize*4

      status = 0
      call udmget(arfsize, 6, p_arf_ratio, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+arfsize*4

      status = 0
      call udmget(phasize, 6, p_e_min, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4

      status = 0
      call udmget(phasize, 6, p_e_max, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
   
      status = 0
      call udmget(phasize, 6, p_arf_remap, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4

      status = 0
      call udmget(phasize, 6, p_sky_bck_arf_corr, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4

      status = 0
      call udmget(phasize, 6, p_sky_bck_arf_exp_corr, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4

      status = 0
      call udmget(phasize, 6, p_src_bck_corr, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4


 50   ineed = 7*phasize*4 + 3*arfsize*4 

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

c --------------- calc sky bckgrnd spectrum ---------------------------+
c background spectra is assumed to contain two parts:
c 1. sky_bckgrnd spectrum 
c 2. particle_bckgrnd spectrum
c therefore,  
c sky_bckgrnd_spectrum = (bckgrnd_spectrum) - (particle_bckgrnd_spectrum)
c -------------------------------------------------------------------

      call calc_sky_bckgrnd(bckfil,particle_bckfil,phasize,backscal_bck,
     >                      MEMR(p_sky_bck),errflg,chatter)

      if (errflg .ne. 0) then
          subinfo='returning from calc_sky_bckgrnd'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c -------------- calc ratio of src_arf/bck_arf ------------------------+

      call calc_arf_ratio(arfsize,arf_srcfil,arf_bckfil,MEMR(p_arf_lo),
     >                    MEMR(p_arf_hi),MEMR(p_arf_ratio),
     >                    errflg,chatter)

      if (errflg .ne. 0) then
          subinfo='returning from calc_arf_ratio'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c ------- convert 729 ARF energy limits to 256 channel no. ------------+ 


      call arf_to_chan(rmffil,arfsize,MEMR(p_arf_lo),MEMR(p_arf_hi),
     >                 MEMR(p_arf_ratio),MEMR(p_e_min),MEMR(p_e_max),
     >                 MEMR(p_arf_remap),errflg,chatter)

      if (errflg .ne. 0) then
          subinfo='returning from arf_to_chan'
          call wterrm(subname,version,subinfo)
          goto 100
      endif
 
c ------------- sky background arf_correction --------------------------

      call sky_bckgrnd_arf_correction(MEMR(p_sky_bck),MEMR(p_arf_remap),
     >                            phasize,MEMR(p_sky_bck_arf_corr),
     >                            errflg, chatter)

      if (errflg .ne. 0) then
          subinfo='returning from sky_bckgrnd__arf_correction'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c ------------- sky background exposure map correction -----------------

      call sky_bckgrnd_exp_correction(expmapfil,regfil_sou,
     >                      regfil_bck,phasize,MEMR(p_sky_bck_arf_corr),
     >                      MEMR(p_sky_bck_arf_exp_corr),errflg,chatter)

      if (errflg .ne. 0) then
          subinfo='returning from sky_bckgrnd_exp_correction'
          call wterrm(subname,version,subinfo)
          goto 100
      endif

c -------- calculate source background and write output file --------------------
c source bckgrnd is calculated as
c src_bckgrnd= src_particle_spectrum + sky_bckgrnd_spectrum(corrected)

      call src_bck_spectrum(particle_srcfil,outfil,srcfil,bckfil,
     >              arf_srcfil,arf_bckfil,rmffil,phasize,taskname,
     >              MEMR(p_sky_bck_arf_exp_corr),MEMR(p_src_bck_corr),
     >              errflg,chatter,killit)

      if (errflg .ne. 0) then
          subinfo='returning from src_bck_spectrum'
          call wterrm(subname,version,subinfo)
          goto 100
      endif
 
c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_sky_bck, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_arf_lo, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_arf_hi, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_arf_ratio, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_e_min, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_e_max, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_arf_remap, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_sky_bck_arf_corr, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_sky_bck_arf_exp_corr, 6, status)
      if(status .ne. 0) goto 60

      call udmfre(p_src_bck_corr, 6, status)

 60   if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wterrm(taskname,version,subinfo)
      endif

 100  call wtendm(subname,version,errflg,chatter)
      end
c ----------------------------------------------------------------------
c            **** END OF FROSCON ****
c ----------------------------------------------------------------------

*+BGDCOR_GP

      subroutine bgdcor_gp(srcfil,bckfil,arf_srcfil,arf_bckfil,
     >                     particle_srcfil,particle_bckfil,outfil,
     >                     rmffil,expmapfil,regfil_sou,regfil_bck,
     >                     errflg,chatter,killit)

c ----------------------------------------------------------------------
c This subroutine reads the user defined parameters such as input files
c and output file as well as chatter flag
c
c --------------- variables --------------------------------------------
      implicit none
      character*(*) srcfil,bckfil,arf_srcfil,arf_bckfil,rmffil
      character*(*) particle_srcfil,particle_bckfil,outfil
      character*(*) expmapfil,regfil_sou,regfil_bck
      integer errflg,chatter
      logical killit

c --------------- authors/modification ---------------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c Peter D Wilson (June 30, 1998) 1.0.1:
c       . Strip out INQUIRE calls for FITS input files
c toliver (July 9, 1999) 1.0.2:
c       . Correct filename parameter in call to CHK_PHA
c
c-----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.2')
      character(10) subname
      parameter (subname ='bgdcor_gp')
*-
c --------------- internal declarations --------------------------------
      character(180) filename,ill_file(10)
      character(100) subinfo
      integer status, extnum, n_ill
      logical ext, valfil, fltflag

c The PHA common block variables
      character(8) pha_telescop,pha_instrume,pha_detnam,pha_filter
      integer pha_unit,typflag,nx_or_ntheta,ny
C      character(16) obs_date, obs_time
      character(68) obs_date, obs_time
      character(80) online
      integer extno,nfound,nret
      common /pha_block/ pha_unit,pha_telescop, pha_instrume,
     >     pha_detnam, pha_filter, typflag, obs_date, obs_time,
     >     nx_or_ntheta,ny
      logical quiet

*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(0,10,2,subinfo)

c ------------------------------------------------------------------
c     now go and read the required parameters from parameter file
c ------------------------------------------------------------------
c                read in chatter & clobber
c ------------------------------------------------------------------

c read in clobber 

      errflg=0
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
  
      quiet=.true.
      if(chatter .ge. 10) then
         quiet=.false.
      endif

c ----------------------------------------------------------------------
c                   get input file parameters                    
c ----------------------------------------------------------------------
c Source spectrum

      n_ill = 0
      status = 0
      call uclgst('srcfil',srcfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for source spectrum!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(srcfil)
      if (srcfil(1:2) .eq. '  ') then
          subinfo = 'src_spectrum file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't call INQURE. Call FTRTNM to strip off extension
      n_ill = n_ill+1
      call ftrtnm( srcfil, ill_file(n_ill), status )
C      call fcpars(srcfil, filename, extnum, status)
C      call crmvlbk(filename)
C      n_ill = n_ill + 1
C      ill_file(n_ill) = filename
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
C          subinfo= 'src_spectrum file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c     Check out the PHA file
      call ck_pha(chatter, ill_file(n_ill), errflg)
      if(errflg .ne. 0)  then
         subinfo = 'returning from ck_pha PHAFIL: '//filename
         call wterrm(subname,version,subinfo)
         return
      endif

c     do we need a filter...
      if(pha_filter(1:4).eq.'NONE'.or.pha_filter(1:7).eq.'UNKNOWN'
     >     .or.pha_filter(1:8).eq.'        ')then
         fltflag=.false.
      else
         fltflag=.true.
      endif

c ----------------------------------------------------------------
c background spectrum

      status = 0
      call uclgst('bckfil',bckfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for bckgrnd spectrum!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(bckfil)
      if (bckfil(1:2) .eq. '  ') then
          subinfo = 'bckgrnd_spectrum file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't bother!  Incompatible with new CFITSIO extended filenames
C      call fcpars(bckfil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
C          subinfo= 'bck_spectrum file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c ------------------------------------------------------------------
c Source ARF file

      status = 0
      call uclgst('arf_srcfil',arf_srcfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for src_arf spectrum!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(arf_srcfil)
      if (arf_srcfil(1:2) .eq. '  ') then
          subinfo = 'src_arf file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't bother!  Incompatible with new CFITSIO extended filenames
C      call fcpars(arf_srcfil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
C          subinfo= 'src_arf file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c ------------------------------------------------------------------
c Background ARF file

      status = 0
      call uclgst('arf_bckfil',arf_bckfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for bck_arf spectrum!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(arf_bckfil)
      if (arf_bckfil(1:2) .eq. '  ') then
          subinfo = 'bck_arf file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't bother!  Incompatible with new CFITSIO extended filenames
C      call fcpars(arf_bckfil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
C          subinfo= 'bck_arf file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif
         
c ----------------------------------------------------------------
c Source particle spectrum file

      status = 0
      call uclgst('particle_srcfil',particle_srcfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for src_particle spectrum!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(particle_srcfil)
      if (particle_srcfil(1:2) .eq. '  ') then
          subinfo = 'source_particle spectrum has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't bother!  Incompatible with new CFITSIO extended filenames
C      call fcpars(particle_srcfil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
C          subinfo= 'source_particle spectrum file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c ----------------------------------------------------------------
c Background particle spectrum file

      status = 0
      call uclgst('particle_bckfil',particle_bckfil,status)
      if (status .ne. 0) then
          subinfo = 'getting input file for bck_particle spectrum!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(particle_bckfil)
      if (particle_bckfil(1:2) .eq. '  ') then
          subinfo = 'bckgrnd_particle spectrum has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't bother!  Incompatible with new CFITSIO extended filenames
C      call fcpars(particle_bckfil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
C          subinfo= 'bckgrnd_particle spectrum file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c ----------------------------------------------------------------
c response matrix file

      status = 0
      call uclgst('rmffil',rmffil,status)
      if (status .ne. 0) then
          subinfo = 'getting RMF file'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(rmffil)
      if (rmffil(1:2) .eq. '  ') then
          subinfo = 'rmf file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

c     if rmffil=caldb or not
      
      if((rmffil(1:5).eq.'CALDB').or.(rmffil(1:5).eq.'caldb')) then
         call gtcalf(chatter,pha_telescop,pha_instrume,'-','-',
     >        'MATRIX',obs_date,obs_time,obs_date,obs_time,'-',
     >        1,rmffil,extno,online,nret,nfound,errflg)

         if(errflg .ne. 0) then
            subinfo='returning from gtcalf subroutine'
            call wterrm(subname,version,subinfo)
            return
         endif

C      else
C
C PDW 6/30/98: Don't bother!  Incompatible with new CFITSIO extended filenames
C         call fcpars(rmffil, filename, extnum, status)
C         call crmvlbk(filename)
C         ext = .true.
C         INQUIRE(FILE=filename, EXIST=ext)
C         if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
C             subinfo= 'rmf file does not exist!'
C             call wterrm(subname,version,subinfo)
C             subinfo = 'filename : '//filename
C             call wterrm(subname,version,subinfo)
C             errflg = 1
C             return
C         endif

      endif

c ----------------------------------------------------------------
c exposure map file   

      status = 0
      call uclgst('expmapfil',expmapfil,status)
      if (status .ne. 0) then
          subinfo = 'getting EXPMAP file'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(expmapfil)
      if (expmapfil(1:2) .eq. '  ') then
          subinfo = 'expmap file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 6/30/98: Don't bother!  Incompatible with new CFITSIO extended filenames
C      call fcpars(expmapfil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
C          subinfo= 'expmap file does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c ----------------------------------------------------------------
c region file for source

      status = 0
      call uclgst('regfil_sou',regfil_sou,status)
      if (status .ne. 0) then
          subinfo = 'getting source region file'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(regfil_sou)
      if (regfil_sou(1:2) .eq. '  ') then
          subinfo = 'source region has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call fcpars(regfil_sou, filename, extnum, status)
      call crmvlbk(filename)
      ext = .true.
      INQUIRE(FILE=filename, EXIST=ext)
      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
          subinfo= 'source region file does not exist!'
          call wterrm(subname,version,subinfo)
          subinfo = 'filename : '//filename
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif
  
c ----------------------------------------------------------------
c region file for background 

      status = 0
      call uclgst('regfil_bck',regfil_bck,status)
      if (status .ne. 0) then
          subinfo = 'getting background region file'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(regfil_bck)
      if (regfil_bck(1:2) .eq. '  ') then
          subinfo = 'background region has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call fcpars(regfil_bck, filename, extnum, status)
      call crmvlbk(filename)
      ext = .true.
      INQUIRE(FILE=filename, EXIST=ext)
      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
          subinfo= 'background region file does not exist!'
          call wterrm(subname,version,subinfo)
          subinfo = 'filename : '//filename
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

c ------------------------------------------------------------------
c Output file

      call uclgst('outfil', outfil, status)
      if (status .ne. 0) then
          subinfo = 'getting output file parameter'
          call wterrm(subname,version,subinfo)
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
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

c ---------------------------------------------------------------------

      errflg = 0
      return
      end
c ----------------------------------------------------------------------
c                       END OF FROSCON_GP
c ----------------------------------------------------------------------

*+GET_ARRAY_SIZE

      subroutine get_array_size(srcfil,arf_srcfil,phasize,arfsize,
     >                          backscal_src,errflg,chatter)

c -----------------------------------------------------------------------
c This subroutine gets the array size of the input files to allocate 
c memory dynamically in the main routine
c -----------------------------------------------------------------------

      implicit none
      character*(*) srcfil,arf_srcfil
      integer phasize,arfsize,errflg,chatter
      real backscal_src

c ------------------- internal variables --------------------------------
 
      integer status,iunit_src,iunit_arf
      character(20) instr(9),outhdu(9,50),extname,outver(9,50)
      character(20) extnames(9,50)
      character(100) subinfo,comm
      integer nsearch,next(50),ninstr
      
c --------------- authors/modifications ----------------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c
c -----------------------------------------------------------------------
      character(15) subname
      parameter (subname='get_array_size')
      character(5) version
      parameter(version='1.0.0')

*-
c -------------------- user info --------------------------------------

      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --------------------------------------------------------------------

      status=0
      extname='SPECTRUM'
      ninstr=1
      instr(1)='SPECTRUM'
      nsearch=50
 
      call mvext (0, srcfil, iunit_src, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, status, chatter)

      if (status .ne. 0) then
          subinfo='opening the input src PHA file'
          call wtferr(subname,version,status,subinfo)
          call ftclos(iunit_src,status)
          subinfo = 'closing input PHA file'
          call wtferr(subname,version,status,subinfo)
          errflg=1
          return
      endif

c ------------------- read no. of channels -----------------------------

       status = 0
       call ftgkyj(iunit_src,'NAXIS2',phasize,comm,status)
       subinfo = 'reading NAXIS2 keyword'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif

       write(subinfo,'(a,i10)')'number of PHA channels found = ',phasize
       call wtinfo(chatter,10,1,subinfo)

c ------------------- read backscal keyword to get area size -----------

       status = 0
       call ftgkye(iunit_src,'BACKSCAL',backscal_src,comm,status)
       subinfo = 'reading BACKSCAL keyword'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif


c ------------------- closing src PHA file -----------------------------

       call ftclos(iunit_src,status)
       if(status .ne. 0) then
          subinfo='closing input src PHA file'
          call wterrm(subname,version,subinfo)
       endif

c -------------------- open ARF file -----------------------------------

      status=0
      extname='SPECRESP'
      ninstr=2
      instr(1)='RESPONSE'
      instr(2)='SPECRESP'
      nsearch=50

      call mvext(0, arf_srcfil, iunit_arf, ninstr, instr, nsearch,
     >           next,
     >           outhdu,extnames,outver,extname,status,chatter)

      if (status .ne. 0) then
          subinfo='opening the input src ARF file'
          call wtferr(subname,version,status,subinfo)
          call ftclos(iunit_arf,status)
          subinfo = 'closing input ARF file'
          call wtferr(subname,version,status,subinfo)
          errflg=1
          return
      endif

c ------------------- read no. of Response matrices ------------------
       status = 0
       call ftgkyj(iunit_arf,'NAXIS2',arfsize,comm,status)
       subinfo = 'reading NAXIS2'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif

       write(subinfo,'(a,i10)')'number of ARF matrices found = ',arfsize
       call wtinfo(chatter,10,1,subinfo)

c ------------------ closing src ARF file -----------------------------

       call ftclos(iunit_arf,status)
       if(status .ne. 0) then
          subinfo='closing input src ARF file'
          call wterrm(subname,version,subinfo)
       endif

c ------------------------------------------------------------------

       errflg=0
       return
       end

c ----------------------------------------------------------------------
c                   END OF GET_ARRAY_SIZE
c ----------------------------------------------------------------------

*+CALC_SKY_BCKGRND

      subroutine calc_sky_bckgrnd(bckfil,particle_bckfil,phasize,
     >               backscal_bck,sky_bck,errflg,chatter)

c -----------------------------------------------------------------------
c This subroutine calculates the sky_background spectrum by subtracting
c particle background spectrum from total background spectrum
c Output is sky background spectrum
c Here we convert all data to be in counts/sec so that output background
c spectrum will be in counts/sec
c ---------------------------------------------------------------------

      implicit none
      character*(*) bckfil,particle_bckfil
      integer phasize,errflg,chatter
      real sky_bck(*)

c ----------------------- internal variables ----------------------------
      character(100) subinfo
      integer ineed,iget,status

c variables used to read total backgrnd spectra
      character(20) instr(9),outhdu(9,50),extname,outver(9,50)
      character(20) extnames(9,50)
      integer nsearch,next(50),ninstr

      character(20) tlscop_bck,instrum_bck,filter_bck
      character(20) chantyp_bck,xflt_bck(9)
      character(180) backfil_bck,corfil_bck,rmffil_bck
      character(5) dmode_bck,hduclas2_bck
      character(20) arffil_bck,detnam_bck,phaversn_bck
      integer iunit1,nchan_bck,dtype_bck,n_xflt_bck
      integer fchan_bck,detchan_bck,max_xflt
      real texpos_bck,areascal_bck,backscal_bck,corscal_bck
      logical qgroup_bck,qqual_bck,qerror_bck,qsys_bck,pois_bck
ccc      integer group_bck(phasize),qualty_bck(phasize)
ccc      integer chan_bck(phasize),ipha_bck(phasize)
ccc      real error_bck(phasize),sysfrc_bck(phasize),pha_bck(phasize)
      integer p_group_bck,p_qualty_bck 
      integer p_chan_bck,p_ipha_bck
      integer p_error_bck,p_sysfrc_bck,p_pha_bck

c variables used to read particle backgrnd spectra
      character(20) tlscop_part,instrum_part,filter_part
      character(20) chantyp_part,xflt_part(9)
      character(180) backfil_part,corfil_part,rmffil_part
      character(5) dmode_part,hduclas2_part
      character(20) arffil_part,detnam_part,phaversn_part
      integer iunit2,nchan_part,dtype_part,n_xflt_part
      integer fchan_part,detchan_part
      real texpos_part,areascal_part,backscal_part,corscal_part
      logical qgroup_part,qqual_part,qerror_part,qsys_part,pois_part
ccc      integer group_part(phasize),qualty_part(phasize)
ccc      integer chan_part(phasize),ipha_part(phasize)
ccc      real error_part(phasize),sysfrc_part(phasize),pha_part(phasize)
      integer p_group_part,p_qualty_part
      integer p_chan_part,p_ipha_part
      integer p_error_part,p_sysfrc_part,p_pha_part


c ------------------- authors/modifications ----------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c Peter D Wilson (June 29, 1998) 1.0.1:
c            . Added max_xflt parameter to rdpha1 function call
c 
c ----------------------------------------------------------------------
      character(17) subname
      parameter (subname='calc_sky_bckgrnd')
      character(5) version
      parameter (version='1.0.1')
*-
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

      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c ------------------ allocate DMA ------------------------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_error_bck = 0
      p_sysfrc_bck = 0
      p_pha_bck = 0
      p_group_bck = 0
      p_qualty_bck = 0
      p_chan_bck = 0
      p_ipha_bck = 0
      p_error_part = 0
      p_sysfrc_part = 0
      p_pha_part = 0
      p_group_part = 0
      p_qualty_part = 0
      p_chan_part = 0
      p_ipha_part = 0

      iget=0
      status = 0
      if(phasize .lt. 50) phasize=50

      call udmget(phasize, 6, p_error_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 6, p_sysfrc_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 6, p_pha_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_group_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_qualty_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_chan_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_ipha_bck, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 6, p_error_part, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 6, p_sysfrc_part, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 6, p_pha_part, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_group_part, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_qualty_part, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_chan_part, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4
 
      call udmget(phasize, 4, p_ipha_part, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phasize*4

 
 50   ineed = 14*phasize*4 

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
           call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory allocated =',iget
           call wtinfo(chatter,10,1,subinfo)

      if (status .ne. 0) then
          errflg = -1
          subinfo='failed to allocate dynamic memory '
          call wtinfo(chatter,0,1,subinfo)
          return
      endif

c ------------------------------------------------------------------
c DMA allocated and now open and read PHA file
c ---------------------------------------------------------------------

       status = 0
       extname='SPECTRUM'
       ninstr=1
       instr(1)='SPECTRUM'
       nsearch = 50

       call mvext (0, bckfil, iunit1, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, status, chatter)

       if (status .ne. 0) then
           subinfo='opening the input bckgrnd PHA file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit1,status)
           subinfo = 'closing input PHA file '//bckfil
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       endif

c read the total background spectrum
      max_xflt=9
      call rdpha1(iunit1,phasize,nchan_bck,tlscop_bck,
     >            instrum_bck,detnam_bck,filter_bck,detchan_bck,
     >            texpos_bck,areascal_bck,backscal_bck,corscal_bck,
     >            backfil_bck,corfil_bck,rmffil_bck,arffil_bck,
     >            xflt_bck,max_xflt,n_xflt_bck,dmode_bck,chantyp_bck,
     >            phaversn_bck,hduclas2_bck,fchan_bck,
     >            MEMI(p_chan_bck),dtype_bck,MEMI(p_ipha_bck),
     >            MEMR(p_pha_bck),qerror_bck,MEMR(p_error_bck),
     >            qsys_bck,MEMR(p_sysfrc_bck),qqual_bck,
     >            MEMI(p_qualty_bck),qgroup_bck,MEMI(p_group_bck),
     >            pois_bck,errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from reading total bckgrnd spectrum'
         call wterrm(subname,version,subinfo)
         return
      endif
      
c ---------------------------------------------------------------------+
c    open particle bckgrnd file
c ---------------------------------------------------------------------+

       status = 0
       extname='SPECTRUM'
       ninstr=1
       instr(1)='SPECTRUM'
       nsearch = 50

       call mvext (0,particle_bckfil,iunit2,ninstr,instr,nsearch,next,
     >             outhdu,extnames,outver,extname,status,chatter)

       if (status .ne. 0) then
           subinfo='opening the input particle bckgrnd PHA file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit2,status)
           subinfo = 'closing input PHA file '//particle_bckfil
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       endif

c read the particle background spectrum
      call rdpha1(iunit2,phasize,nchan_part,tlscop_part,
     >            instrum_part,detnam_part,filter_part,detchan_part,
     >            texpos_part,areascal_part,backscal_part,
     >            corscal_part,backfil_part,corfil_part,rmffil_part,
     >            arffil_part,xflt_part,max_xflt,n_xflt_part,dmode_part,
     >            chantyp_part,phaversn_part,hduclas2_part,fchan_part,
     >            MEMI(p_chan_part),dtype_part,MEMI(p_ipha_part),
     >            MEMR(p_pha_part),qerror_part,MEMR(p_error_part),
     >            qsys_part,MEMR(p_sysfrc_part),qqual_part,
     >            MEMI(p_qualty_part),qgroup_part,MEMI(p_group_part),
     >            pois_part,errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from reading particle bckgrnd spectrum'
         call wterrm(subname,version,subinfo)
         return
      endif
 
c -------------------------------------------------------------------
c now do the subtraction
c On return from subtract, the data is changed to rate (counts/exposure)
c if data were in counts.  But if they were in rate then it is same
c -------------------------------------------------------------------

      call subtract(dtype_bck,MEMI(p_ipha_bck),MEMR(p_pha_bck),
     >              dtype_part,MEMI(p_ipha_part),MEMR(p_pha_part),
     >              phasize,texpos_bck,texpos_part,sky_bck,
     >              errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from subtract'
         call wterrm(subname,version,subinfo)
         return
      endif
  
      errflg=0
      return
      end

c ---------------------------------------------------------------------
c                    END OF CALC_SKY_BCKGRND
c ---------------------------------------------------------------------

*+SUBTRACT

      subroutine subtract(dtype_bck,icounts,rate,dtype_part,ipart,
     >                    part,phasize,texpos_bck,texpos_part,sky,
     >                    errflg,chatter)

c --------------------------------------------------------------------
c This routine subtracts the particle spectra from total spectra
c to obtain the sky component of the spectra
c --------------------------------------------------------------------

      implicit none
      integer dtype_bck,dtype_part,phasize
      integer icounts(*),ipart(*)
      real rate(*),part(*),sky(*)
      real texpos_bck,texpos_part 
      integer errflg,chatter

c ----------------- internal variables --------------------------------
      character(100) subinfo
      integer i

c --------------- authors/modifications -------------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c 
c ---------------------------------------------------------------------

      character(9) subname
      parameter (subname='subtract')
      character(5) version
      parameter (version='1.0.0')
*-
c --------------------------------------------------------------------
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c ---------------------------------------------------------------------

      if(dtype_bck .eq. 1) then
         do i=1,phasize
            rate(i) = real(icounts(i))/texpos_bck
         enddo
         dtype_bck=2
      endif

      if(dtype_part .eq. 1) then
         do i=1,phasize
            part(i) = real(ipart(i))/texpos_part
         enddo
         dtype_part=2
      endif

      do i=1,phasize
         if(rate(i) .eq. 0.0) then
            sky(i) = 0.0
         else
            sky(i) = rate(i) - part(i)
         endif
       
      enddo
   
      errflg=0
      return
      end

c ---------------------------------------------------------------------
c                   END OF SUBTRACT
c ---------------------------------------------------------------------

*+CALC_ARF_RATIO

      subroutine calc_arf_ratio(arfsize,arf_srcfil,arf_bckfil,
     >                          arf_lo,arf_hi,arf_ratio,
     >                          errflg,chatter)

c ---------------------------------------------------------------------
c This subroutine calculates the ratio of the two ARFs, that is,
c ARF_RATIO=SOURCE ARF/BACKGRND ARF
c ---------------------------------------------------------------------

      implicit none
      character*(*) arf_srcfil,arf_bckfil
      integer arfsize
      integer errflg,chatter
      real arf_lo(*),arf_hi(*),arf_ratio(*)

c ------------------------- internal variables -------------------------
      character(100) subinfo

      character(20) instr(9),outhdu(9,50),extname,outver(9,50)
      character(20) extnames(9,50)
      integer status
      integer nsearch,next(50),ninstr

      integer iunit_src, iunit_bck
      character(20) telescop_src,instrume_src,detnam_src,filter_src
      character(20) telescop_bck,instrume_bck,detnam_bck,filter_bck
      character(5) arfversn
      integer iebound_src,iebound_bck,i
      real rspns_src(729),rspns_bck(729)
      
c ------------------ author/modifications -----------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c
c ---------------------------------------------------------------------
      character(15) subname
      parameter (subname='calc_arf_ratio')
      character(5) version
      parameter (version='1.0.0')
*-
c ---------------------------------------------------------------------
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subname)

c ---------------------------------------------------------------------
c Open source ARF file and read useful data and then close the file

       status = 0
       extname ='SPECRESP'
       ninstr = 2
       instr(1) = 'RESPONSE'
       instr(2) = 'SPECRESP'
       nsearch = 50

       call mvext(0,arf_srcfil,iunit_src,ninstr,instr,nsearch,next,
     >            outhdu,extnames,outver,extname,status,chatter)

       if (status .ne. 0) then
           subinfo='opening the input ARF file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit_src,status)
           subinfo = 'closing source ARF file'
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       endif

       call rdarf1(iunit_src,chatter,telescop_src,instrume_src,
     >             detnam_src,filter_src,iebound_src,arf_lo,arf_hi,
     >             rspns_src,arfversn,errflg)

       if (errflg .ne. 0) then
           subinfo='reading source ARF file '//arf_srcfil
           call wtferr(subname,version,errflg,subinfo)
           return
       endif

 
       call ftclos(iunit_src,status)
       if(status .ne. 0) then
          subinfo = 'closing source ARF file'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
       endif

       call ftfiou(iunit_src,status)

c ---------------------------------------------------------------------
c Open background ARF file and read useful data and then close the file

       status = 0
       extname ='SPECRESP'
       ninstr = 2
       instr(1) = 'RESPONSE'
       instr(2) = 'SPECRESP'
       nsearch = 50

       call mvext(0,arf_bckfil,iunit_bck,ninstr,instr,nsearch,next,
     >            outhdu,extnames,outver,extname,status,chatter)

       if (status .ne. 0) then
           subinfo='opening the background ARF file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit_bck,status)
           subinfo = 'closing background ARF file'
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       endif

       call rdarf1(iunit_bck,chatter,telescop_bck,instrume_bck,
     >             detnam_bck,filter_bck,iebound_bck,arf_lo,
     >             arf_hi,rspns_bck,arfversn,errflg)

       if (errflg .ne. 0) then
           subinfo='reading source ARF file '//arf_bckfil
           call wtferr(subname,version,errflg,subinfo)
           return
       endif


       call ftclos(iunit_bck,status)
       if(status .ne. 0) then
          subinfo = 'closing background ARF file'
          call wtferr(subname,version,status,subinfo)
          errflg=1
          return
       endif

       call ftfiou(iunit_bck,status)

c ----------------------------------------------------------------------
c now test if both the files are compatible before calculating ratio

       if(telescop_src .ne. telescop_bck) then
          subinfo='telescope mismatch in ARF files'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
       endif

       if(instrume_src .ne. instrume_bck) then
          subinfo='instrument mismatch in ARF files'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
       endif

       if(iebound_src .ne. iebound_bck) then
          subinfo='energy range mismatch in ARF files'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
       endif

c ----------------------------------------------------------------------
c now take the ratio of the two ARFs

       do i=1,iebound_src
          if(rspns_bck(i) .eq. 0.) then
             arf_ratio(i)= 0.
          else
             arf_ratio(i) = rspns_src(i)/rspns_bck(i)
          endif
       enddo
       
       errflg=0
       return
       end

c ---------------------------------------------------------------------
c                    END OF CALC_ARF_RATIO  
c ---------------------------------------------------------------------

*+ARF_TO_CHAN

      subroutine arf_to_chan(rmffil,arfsize,arf_lo,arf_hi,arf_ratio,
     >                       e_min,e_max,arf_remap,
     >                       errflg,chatter) 

c ---------------------------------------------------------------------
c This subroutine converts the arf_lo,arf_hi to chanel no. w.r.t
c according to RMF supplied by user
c ---------------------------------------------------------------------

      implicit none
      character*(*) rmffil
      integer arfsize
      real arf_lo(*),arf_hi(*),arf_ratio(*),arf_remap(*)
      real e_min(*),e_max(*)
      integer errflg,chatter

c --------------- internal variables ----------------------------------
      character(100) subinfo
      character(20) instr(9),outhdu(9,50),extname,outver(9,50)
      character(20) extnames(9,50)
      integer status
      integer nsearch,next(50),ninstr

      integer iunit_rmf,maxchan   
      parameter (maxchan=256)
      integer iebound,channel(maxchan)
      real areascal
      real accuracy
      character(8) rmfversn
      character(20) telescop, instrume, detnam, filter
      integer ebd_fchan
      character(4) ebdchantyp

c ------------------ author/modifications -----------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c
c ---------------------------------------------------------------------
      character(12) subname
      parameter (subname='arf_to_chan')
      character(5) version
      parameter (version='1.0.0')
*-
c ---------------------------------------------------------------------
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c ---------------------------------------------------------------------
c Open RMF EBOUNDS extension 

       status = 0
       extname ='EBOUNDS'
       ninstr = 2
       instr(1) = 'EBOUNDS'
       instr(2) = 'RESPONSE'
       nsearch = 50

       call mvext(0, rmffil, iunit_rmf, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, status, chatter)

       if (status .ne. 0) then
           subinfo='opening the input RMF file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit_rmf,status)
           subinfo = 'closing input RMF file'
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       endif

c ---------------------------------------------------------------------
c Read E_MIN and E_MAX from the RMF extension

      call rdebd3(iunit_rmf,chatter,maxchan,
     >            telescop,instrume,detnam,filter,areascal,
     >            ebdchantyp,ebd_fchan,
     >            iebound,channel,e_min,e_max,rmfversn,errflg)

      if (errflg .ne. 0) then
          subinfo='reading EBOUNDS extension'
          call wtferr(subname,version,status,subinfo)
          call ftclos(iunit_rmf,status)
          subinfo = 'closing input RMF file'
          call wtferr(subname,version,status,subinfo)
          return
      endif

c ---------------------------------------------------------------------
c Now do the conversion (subroutine REMAP does this )
c acc = accuray needed in fraction
c arf_remap = remapped response matrix
c ---------------------------------------------------------------------

      accuracy = 0.001
      call rmap1d(chatter,arfsize,arf_lo,arf_hi,arf_ratio,
     >           iebound,e_min,e_max,arf_remap,3,
     >           accuracy,errflg)

      if(errflg .lt. 0) errflg=0
      if (errflg .gt. 0) then
          subinfo='retruning from remap subroutine'
          call wtferr(subname,version,status,subinfo)
          return
      endif

      errflg=0
      return
      end

c ---------------------------------------------------------------------
c                    END OF ARF_TO_CHAN
c ---------------------------------------------------------------------

*+SKY_BCKGRND_ARF_CORRECTION

      subroutine sky_bckgrnd_arf_correction(sky_bck,arf_remap,phasize,
     >                                  sky_bck_arf_corr,errflg,chatter)
                                                       
c -----------------------------------------------------------------------
c This routine multiplies the sky background component by the
c correction factor sky_bck_arf_corr which is the ratio of the two 
c source to bckgrnd ARFs and mapped to channel no.
c -----------------------------------------------------------------------

      implicit none
      real sky_bck(*),arf_remap(*),sky_bck_arf_corr(*) 
      integer phasize,errflg,chatter

c ---------------------- internal variables -----------------------
      character(100) subinfo
      integer i

c ------------------- authors/modifications -------------------------
c
c Banashree Mitra Seifert (July, 1996) 1.0.0:
c
c ---------------------------------------------------------------------

      character(27) subname
      parameter (subname='sky_bckgrnd_arf_correction')
      character(5) version
      parameter (version='1.0.0')
*-
c --------------------------------------------------------------------
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c ---------------------------------------------------------------------
      do i=1,phasize
         sky_bck_arf_corr(i)= sky_bck(i) * arf_remap(i)
      enddo

      errflg=0
      return
      end

c --------------------------------------------------------------------
c         END OF SKY_BCKGRND_ARF_CORRECTION
c --------------------------------------------------------------------

*+SKY_BCKGRND_EXP_CORRECTION

      subroutine sky_bckgrnd_exp_correction(expmapfil,regfil_sou,
     >                      regfil_bck,phasize,sky_bck_arf_corr,
     >                      sky_bck_arf_exp_corr,errflg,chatter)

c --------------------------------------------------------------------
c This subroutine opens the exposure map file, reads in the
c image data and then performs the exposure correction by summing over
c the exposure map.  
c Input is sky_bck_arf_corr.  
c This, along with expmap give the exposure corrected sky_backgrnd. 
c Output is sky_bck_arf_exp_corr. 
c --------------------------------------------------------------------

      implicit none
      character*(*) expmapfil,regfil_sou,regfil_bck  
      real sky_bck_arf_corr(*),sky_bck_arf_exp_corr(*)
      integer phasize,errflg,chatter

c -------------- internal variables ----------------------------------
c variables to open file
      integer iunit,ninstr,nsearch,status
      integer next(50)
      character(20) outhdu(9,50),extnames(50),outver(9,50)
      character(20) instr(50),extname

c variables to read file
ccc      real expmap(1024,1024)
      real expmap(512,512)
      integer group,nx,ny
      real nullval
      logical anyfl
      character(100) subinfo,comm

c variables for region file
      integer iunitr
      character*(100) comm_line(40)
      character(10) shape(10),shape_sou,shape_bck
      character(1) sign(10)
      integer maxpoints

      parameter (maxpoints=20)
      real points(maxpoints,maxpoints)
      integer line_no,shape_no
      integer npoints(maxpoints),char_no(10)

      real center_x_sou,center_y_sou,rad1_sou,rad2_sou
      real xmin_sou,xmax_sou,ymin_sou,ymax_sou
      real center_x_bck,center_y_bck,rad1_bck,rad2_bck
      real xmin_bck,xmax_bck,ymin_bck,ymax_bck

      integer i,ix,iy
      real tot_exp_sou,tot_exp_bck,exp_corr
      real tminx,tmaxx,tminy,tmaxy,xsq,ysq,r
      integer xmin,xmax,ymin,ymax

c ---------- author/modifications ----------------------------------
c Banashree Mitra Seifert (July 1996) 1.0.0
c Peter D Wilson (Aug 30, 1999) 1.1.0: More regions supported
c ------------------------------------------------------------------
      character(27) subname
      parameter (subname='sky_bckgrnd_exp_correction')
      character(5) version
      parameter (version='1.1.0')
 
*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --------------------------------------------------------------
c open the exposure map file and read the data

      rad1_sou = 0.
      rad2_sou = 0.
      xmin_sou = 0.
      xmax_sou = 0.
      ymin_sou = 0.
      ymax_sou = 0.
      rad1_bck = 0.
      rad2_bck = 0.
      xmin_bck = 0.
      xmax_bck = 0.
      ymin_bck = 0.
      ymax_bck = 0.
    
     

      errflg=0
      extname='*'
      ninstr = 2
      instr(1) = 'IMAGE'
      instr(2) = 'EXPOSURE'
      nsearch = 50
      call mvext(0,expmapfil, iunit, ninstr, instr, nsearch, next,
     >           outhdu, extnames, outver,extname, errflg, chatter)

      if(errflg .ne. 0) then
         subinfo='IMAGE extension is not found'
         call wtinfo(chatter,10,2,subinfo)
         return
      endif

c     get EXPOSURE MAP sizes
      status=0
      call ftgkyj(iunit,'NAXIS1',nx,comm,status)
      if (status .ne. 0) then
          subinfo='reading NAXIS1 keyword'
          call wterrm(subname,version,subinfo)
          return
      endif
      call ftgkyj(iunit,'NAXIS2',ny,comm,status)
      if (status .ne. 0) then
          subinfo='reading NAXIS2 keyword'
          call wterrm(subname,version,subinfo)
          return
      endif
      
c     read the expmap data
      group = 0
      nullval = 0
      call ftg2de(iunit,group,nullval,nx,nx,ny,expmap,anyfl,status)
      if (status.ne.0) then
          subinfo='returning from reading 2d expmap array'
          call wterrm(subname,version,subinfo)
          return
      endif

c --------------------------------------------------------------
c     read the source region file
c     get lun for region file

      status = 0
      call ftgiou(iunitr,status)
      open(unit=iunitr,file=regfil_sou,status='old')

      call rdreg1(iunitr,char_no,maxpoints,line_no,comm_line,
     >            shape_no,shape,npoints,points,sign,
     >            chatter,errflg)

      call ftfiou(iunitr,status)

      if(errflg .ne. 0) then
         subinfo='returning from rdreg1'
         call wterrm(subname,version,subinfo)
         return
      endif
                              
      if(shape_no .gt. 1) then
         subinfo='more than one source region is found'
         call wterrm(subname,version,subinfo)
         errflg=1
         return
      endif

      shape_sou=shape(1)
      center_x_sou=points(1,1)
      center_y_sou=points(1,2)
      if(shape_sou .eq. 'POINT') then
C        Do nothing
      elseif(shape_sou .eq. 'CIRCLE') then
         rad1_sou=points(1,3)
         rad2_sou=points(1,3)
      elseif(shape_sou .eq. 'ELLIPSE') then
C        For ELLIPSE, rad1 is X radius, rad2 is Y RADIUS
         rad1_sou=points(1,3)
         rad2_sou=points(1,4)
      elseif(shape_sou .eq. 'ANNULUS') then
C        For ANNULUS, rad1 is interior radius, rad2 is exterior
         rad1_sou=points(1,3)
         rad2_sou=points(1,4)
      elseif(shape_sou .eq. 'BOX') then
         xmin_sou=center_x_sou - points(1,3)/2.
         xmax_sou=center_x_sou + points(1,3)/2.
         ymin_sou=center_y_sou - points(1,4)/2.
         ymax_sou=center_y_sou + points(1,4)/2.
      else
         subinfo='Unsupported region shape: ' // shape_sou
         call wterrm(subname,version,subinfo)
         errflg=1
         return
      endif


c --------------------------------------------------------------
c     read the background region file
c     get lun for region file

      status = 0
      call ftgiou(iunitr,status)
      open(unit=iunitr,file=regfil_bck,status='old')

      call rdreg1(iunitr,char_no,maxpoints,line_no,comm_line,
     >            shape_no,shape,npoints,points,sign,
     >            chatter,errflg)

      call ftfiou(iunitr,status)

      if(errflg .ne. 0) then
         subinfo='returning from rdreg1'
         call wterrm(subname,version,subinfo)
         return
      endif
                              
      if(shape_no .gt. 1) then
         subinfo='more than one bckgrnd region is found'
         call wterrm(subname,version,subinfo)
         errflg=1
         return
      endif

      shape_bck=shape(1)
      center_x_bck=points(1,1)
      center_y_bck=points(1,2)
      if(shape_bck .eq. 'POINT') then
C        Do nothing
      elseif(shape_bck .eq. 'CIRCLE') then
         rad1_bck=points(1,3)
         rad2_bck=points(1,3)
      elseif(shape_bck .eq. 'ELLIPSE') then
C        For ELLIPSE, rad1 is X radius, rad2 is Y radius
         rad1_bck=points(1,3)
         rad2_bck=points(1,4)
      elseif(shape_bck .eq. 'ANNULUS') then
C        For ANNULUS, rad1 is interior radius, rad2 is exterior
         rad1_bck=points(1,3)
         rad2_bck=points(1,4)
      elseif(shape_bck .eq. 'BOX') then
         xmin_bck=center_x_bck - points(1,3)/2.
         xmax_bck=center_x_bck + points(1,3)/2.
         ymin_bck=center_y_bck - points(1,4)/2.
         ymax_bck=center_y_bck + points(1,4)/2.
      else
         subinfo='Unsupported region shape: ' // shape_bck
         call wterrm(subname,version,subinfo)
         errflg=1
         return
      endif


c ----------------------------------------------------------------------
c now work with exposure map for source 

      tot_exp_sou=0.
      if(shape_sou.eq.'POINT') then
         ix = int( center_x_sou )
         iy = int( center_y_sou )
         tot_exp_sou = expmap(ix,iy)

      elseif(shape_sou.eq.'CIRCLE' .or. shape_sou.eq.'ELLIPSE') then
         xmin=int( center_x_sou - rad1_sou       )
         xmax=int( center_x_sou + rad1_sou + 0.5 )
         ymin=int( center_y_sou - rad2_sou       )
         ymax=int( center_y_sou + rad2_sou + 0.5 )

         do ix = xmin, xmax 
            xsq=( (ix-center_x_sou) / rad1_sou )**2 
            do iy = ymin, ymax
               ysq=( (iy-center_y_sou) / rad2_sou )**2
               r=sqrt(xsq+ysq)
               if(r .le. 1.0) then
                  tot_exp_sou = tot_exp_sou + expmap(ix,iy)
               endif
            enddo 
         enddo

      elseif(shape_sou .eq. 'ANNULUS') then
C        Only search region within OUTER radius
         xmin=int( center_x_sou - rad2_sou       )
         xmax=int( center_x_sou + rad2_sou + 0.5 )
         ymin=int( center_y_sou - rad2_sou       )
         ymax=int( center_y_sou + rad2_sou + 0.5 )

         do ix = xmin, xmax 
            xsq=(ix-center_x_sou)**2 
            do iy = ymin, ymax
               ysq=(iy-center_y_sou)**2
               r=sqrt(xsq+ysq)
               if(r .ge. rad1_sou .and. r .le. rad2_sou ) then
                  tot_exp_sou = tot_exp_sou + expmap(ix,iy)
               endif
            enddo 
         enddo

      elseif(shape_sou .eq. 'BOX') then
         do ix = int(xmin_sou), int(xmax_sou)
            do iy = int(ymin_sou), int(ymax_sou)
               tot_exp_sou = tot_exp_sou + expmap(ix,iy)
            enddo
         enddo
      endif
 
c ----------------------------------------------------------------------
c now work with exposure map for background

      tot_exp_bck=0.
      if(shape_bck.eq.'POINT') then
         ix = int( center_x_bck )
         iy = int( center_y_bck )
         tot_exp_bck = expmap(ix,iy)

      elseif(shape_bck.eq.'CIRCLE' .or. shape_bck.eq.'ELLIPSE') then
         xmin=int( center_x_bck - rad1_bck       )
         xmax=int( center_x_bck + rad1_bck + 0.5 )
         ymin=int( center_y_bck - rad2_bck       )
         ymax=int( center_y_bck + rad2_bck + 0.5 )

         do ix = xmin, xmax
            xsq=( (ix-center_x_bck) / rad1_bck )**2
            do iy = ymin, ymax
               ysq=( (iy-center_y_bck) / rad2_bck )**2
               r=sqrt(xsq+ysq)
               if(r .le. 1.0) then
                  tot_exp_bck = tot_exp_bck + expmap(ix,iy)
               endif
            enddo
         enddo

      elseif(shape_bck .eq. 'ANNULUS') then
C        Only search region within OUTER radius
         xmin=int( center_x_bck - rad2_bck       )
         xmax=int( center_x_bck + rad2_bck + 0.5 )
         ymin=int( center_y_bck - rad2_bck       )
         ymax=int( center_y_bck + rad2_bck + 0.5 )

         do ix = xmin, xmax
            xsq=(ix-center_x_bck)**2
            do iy = ymin, ymax
               ysq=(iy-center_y_bck)**2
               r=sqrt(xsq+ysq)
               if(r .ge. rad1_bck .and. r .le. rad2_bck) then
                  tot_exp_bck = tot_exp_bck + expmap(ix,iy)
               endif
            enddo
         enddo

      elseif(shape_bck .eq. 'BOX') then
         do ix = int(xmin_bck), int(xmax_bck)
            do iy = int(ymin_bck), int(ymax_bck)
               tot_exp_bck = tot_exp_bck + expmap(ix,iy)
            enddo
         enddo
      endif

c -----------------------------------------------------------------------
c exposure correction and area correction factor
c since source region may not be same as background region. so area
c correction is required. 
c mean exposure = total exposure/no. of pixels
c
c                       mean exposure for source
c exposure correction=--------------------------------
c                       mean exposure for background
c
c                     no. of pixels in source
c area correction = -----------------------------
c                     no. of pixels in bckgrnd
c therefore, 
c                                          total exposure for source
c exposure correction * area correction = ------------------------------ 
c                                          total exposure for background
c -----------------------------------------------------------------------

      exp_corr = tot_exp_sou/tot_exp_bck 

      write(subinfo,'(a,f15.2)')'total source exposure= ',tot_exp_sou
      call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,f15.2)')'total bkgrnd exposure= ',tot_exp_bck
      call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,f10.4)')
     >           'exposure*area correction factor= ',exp_corr 
      call wtinfo(chatter,10,1,subinfo)

      do i=1,phasize
         sky_bck_arf_exp_corr(i) = sky_bck_arf_corr(i)*exp_corr 
      enddo

      return
      end
c -------------------------------------------------------------------
c            END OF SKY_BCKGRND_EXP_CORRECTION
c --------------------------------------------------------------------

*+SRC_BCK_SPECTRUM

      subroutine src_bck_spectrum(particle_srcfil,outfil,srcfil,
     >           bckfil,arf_srcfil,arf_bckfil,rmffil,
     >           phasize,taskname,
     >           sky_bck_arf_exp_corr,src_bck_corr,
     >           errflg,chatter,killit)

c ----------------------------------------------------------------------
c This subroutine opens the source particle spectrum file, reads the
c particle spectrum, and adds the corrected sky background (corrected 
c for the source region).
c inputs are source particle spectra and sky_bck_arf_exp_corr
c output is src_bck_corr
c ----------------------------------------------------------------------

      implicit none
      character*(*) particle_srcfil,outfil,srcfil,bckfil 
      character*(*) arf_srcfil,arf_bckfil,taskname 
      real sky_bck_arf_exp_corr(*),src_bck_corr(*)
      integer phasize,errflg,chatter
      logical killit

c -------------- internal variables ----------------------------------
c variables to open file
      integer iunit,ninstr,nsearch,status
      integer next(50)
      character(20) outhdu(9,50),extnames(50),outver(9,50)
      character(20) instr(50),extname

c variables used to read particle backgrnd spectra
      integer phsz
      parameter (phsz=1024)
      character(20) tlscop_part,instrum_part,filter_part
      character(20) chantyp_part,xflt_part(9)
      character(180) backfil_part,corfil_part,rmffil_part
      character(5) dmode_part,hduclas2_part
      character(20) arffil_part,detnam_part,phaversn_part
      integer nchan_part,dtype_part,n_xflt_part
      integer fchan_part,detchan_part
      integer group_part(phsz),qualty_part(phsz)
      integer chan_part(phsz),ipha_part(phsz)
      real texpos_part,areascal_part,backscal_part,corscal_part
      real error_part(phsz),sysfrc_part(phsz),pha_part(phsz)
      logical qgroup_part,qqual_part,qerror_part,qsys_part,pois_part

      character(100) subinfo
      integer i

c variables to read the srcfile
      character(20) tlscop,instrum,filter
      character(20) chantyp,xflt(9)
      character(180) backfil,corfil,rmffil,arffil
      character(5) dmode,hduclas2
      character(20) detnam,phaversn
      integer nchan,dtype,n_xflt,max_xflt
      integer fchan,detchan
      integer group(phsz),qualty(phsz)
      integer chan(phsz),ipha(phsz)
      real texpos,areascal,backscal,corscal
      real error(phsz),sysfrc(phsz),pha(phsz)
      logical qgroup,qqual,qerror,qsys,pois


c variables to write data in ouput file
      integer ounit,nk_hist,nk_comm
      character(100) hist(10),comment(10)
 
c ---------- author/modifications ----------------------------------
c Banashree Mitra Seifert (July 1996) 1.0.0
c Peter D Wilson (June 29, 1998) 1.0.1:
c            . Added max_xflt parameter to rdpha1 function call
c ------------------------------------------------------------------
      character(17) subname
      parameter (subname='src_bck_spectrum')
      character(5) version
      parameter (version='1.0.1')

*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --------------------------------------------------------------
c open the source particle spectra file and read the data

      status=0
      extname='SPECTRUM'
      ninstr=1
      instr(1)='SPECTRUM'
      nsearch = 50
      call mvext(0,particle_srcfil, iunit, ninstr, instr, nsearch, next,
     >           outhdu, extnames, outver,extname, status, chatter)

      if(status .ne. 0) then
         subinfo='SPECTRUM extn for source particle not found'
         call wterrm(subname,version,subinfo)
         status=0
         call ftclos(iunit,status)
         errflg=1 
         return
      endif

c ------------------------------------------------------------------------
c read the particle background spectrum
      max_xflt=9
      call rdpha1(iunit,phasize,nchan_part,tlscop_part,
     >            instrum_part,detnam_part,filter_part,detchan_part,
     >            texpos_part,areascal_part,backscal_part,
     >            corscal_part,backfil_part,corfil_part,rmffil_part,
     >            arffil_part,xflt_part,max_xflt,n_xflt_part,dmode_part,
     >            chantyp_part,phaversn_part,hduclas2_part,fchan_part,
     >            chan_part,dtype_part,ipha_part,
     >            pha_part,qerror_part,error_part,
     >            qsys_part,sysfrc_part,qqual_part,
     >            qualty_part,qgroup_part,group_part,
     >            pois_part,errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from reading source particle spectrum'
         call wterrm(subname,version,subinfo)
         return
      endif

      call ftclos(iunit,status)
      call ftfiou(iunit,status)
      
c ------------------------------------------------------------------------

      if(dtype_part .eq. 1) then
         do i=1,phasize
            pha_part(i) = real(ipha_part(i))/texpos_part
         enddo
      endif

      do i=1,phasize
         src_bck_corr(i) = pha_part(i) + sky_bck_arf_exp_corr(i)  
      enddo

c to write the output, we need some info from source file

      status=0
      extname='SPECTRUM'
      ninstr=1
      instr(1)='SPECTRUM'
      nsearch = 50
      call mvext(0,srcfil, iunit, ninstr, instr, nsearch, next,
     >           outhdu, extnames, outver,extname, status, chatter)

      if(status .ne. 0) then
         subinfo='SPECTRUM extn for source particle not found'
         call wterrm(subname,version,subinfo)
         status=0
         call ftclos(iunit,status)
         errflg=1
         return
      endif
    
      call rdpha1(iunit,phasize,nchan,tlscop,instrum,detnam,filter,
     >            detchan,texpos,areascal,backscal,corscal,backfil,
     >            corfil,rmffil,arffil,xflt,max_xflt,n_xflt,dmode,
     >            chantyp,phaversn,hduclas2,fchan,chan,dtype,ipha,pha,
     >            qerror,error,qsys,sysfrc,qqual,qualty,qgroup,group,
     >            pois,errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from reading source particle spectrum'
         call wterrm(subname,version,subinfo)
         return
      endif

      call ftclos(iunit,status)
      call ftfiou(iunit,status)


c -----------------------------------------------------------------------
c Now write the corrected background for source
c opening and writing the header for outfil to be written ------
 
      status=0
      call ftgiou(ounit,status)
      status=0
      call opnpa(outfil,chatter,ounit,killit,status)

      if (status .ne. 0) then
          subinfo='error opening output file:'//outfil
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

c Add additional keywords to Primary Header
      call FTPKYS(ounit,'CONTENT','PI SPECTRUM',
     >           'SPECTRUM extension present', status)

      call FTPKYS(ounit,'CREATOR',taskname,
     >           's/w task which wrote this dataset',status)
 

      nk_hist=5
      hist(1)='background correction done by CALCBGDCOR'//' Ver '
     >        //version
      hist(2)='background filename: '//bckfil
      hist(3)='source filename: '//srcfil
      hist(4)='source ARF filename: '//arf_srcfil
      hist(5)='bckgrnd ARF filename: '//arf_bckfil

      nk_comm=0

      hduclas2='BCKGRND'
      phaversn='1.1.0'
      qgroup=.false.
      qqual=.false.
      qsys=.false.
      backfil = bckfil
      corfil = 'NONE'
      arffil = arf_bckfil //' & '//arf_srcfil
      areascal=1.0
      backscal=1.0
      corscal=1.0
      dtype=2


      call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,comment,
     >            tlscop,instrum,detnam,filter,
     >            phaversn,hduclas2,fchan,texpos,
     >            areascal,backfil,backscal,corfil,
     >            corscal,rmffil,arffil,detchan,
     >            chantyp,chan,src_bck_corr,dtype,qerror,
     >            error,qsys,sysfrc,qqual,qualty,
     >            qgroup,group,nchan,
     >            errflg)

      if(errflg .ne. 0) then
         subinfo='writing results to '//outfil
         call wterrm(subname,version,subinfo)
         return  
      endif

      call ftpkys(ounit,'CREATOR',taskname,
     >             's/w task which wrote this dataset',status)

      status=0
      call ftclos(ounit,status)
      call ftfiou(ounit,status)
      return
      end

c ---------------------------------------------------------------------
c            END OF SRC_BCK_SPECTRUM 
c -----------------------------------------------------------------------


