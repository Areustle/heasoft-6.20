*+RPSF2EEF
c ----------------------------------------------------------------------
       subroutine rpsf2f 

c ---------------- DESCRIPTION -----------------------------------------
c This subroutine reads observed RPSF and model RPSF extensions
c in FITS format.  It converts the Radial Point Spread Fraction RPSF 
c (data and model) to Encircled Energy Fraction EEF and subsequently
c it writes the EEF (data and model) in FITS format. This task is to 
c convert a radial profile of PSF per unit area to fraction of 
c encircled energy function 
c
c ------------------ ROUTINES CALLED -----------------------------------
c eef_gp    : gets parameters
c eef_rdat  : reads data
c eef_conv  : makes the conversion
c eef_wt    : writes the converted data in FITS format
c
c -------------------- DECLARE VARIABLES ------------------------------+
c
      implicit none
      character(120) subinfo
      character(16)  instrum
      character(20) telescop,instrume
      character(20) ohduclas3, ohduclas4, phduclas3, phduclas4
      character(10) outhdu_class
      character(16) radunit, thetaunit, energunit, rpsfunit, reefunit
      integer nenerg, ntheta  
      integer chatter, errflg, status
      integer ineed,iget
      real pix_size, backgrnd
      logical theo_pres,data_pres,killit
c
c ------------------------- VARIABLE DEFINITIONS -----------------------
c
c infil      char   : Name of PSF file (user defined)
c predfil    char   : Name of theoretical data file (user defined)
c outfil     char   : Name of EEF file (user defined)
c subinfo    char   : Subroutine information
c chatter    int    : Chatter flag (<5 quiet,>5normal,>20 noisy)
c maxrad     int    : Maximum array size for observed psf data
c maxpred    int    : Maximum array size for predicted psf data
c nrad       int    : Counter for observed psf data
c npred      int    : Counter for predicted psf data
c rad_lo     real   : Array of lower edge of observed radial bins
c rad_hi     real   : Array of upper edge of observed radial bins
c cts        real   : Array of observed radial profile, in counts
c err        real   : Array of errors on cts
c pred_lo    real   : Array of lower edge of predicted model bins
c pred_hi    real   : Array of upper edge of predicted model bins
c pred       real   : Array of theoretical PSF
c rad_mean   real   : Array of mean of observed radial bins
c del_rad    real   : Array of half-width of observed radial bins 
c pred_mean  real   : Array of mean predicted model bins
c pdel_rad   real   : Array of half-width predicted model bins
c errflg     int    : error flag, 0 is okay
c theo_pres  logical: true if theoretical extension is present
c 
C
c --------------- AUTHORS/MODIFICATION ---------------------------------
c
c Banashree Mitra Seifert (Nov. 24, 1995)
c Peter D Wilson (Jul 01, 1998) 1.0.1:
c       . Updated for new FCPARS behavior
c
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '1.0.1')
	character(40) taskname
        character(9) subname
cccc        common/task/taskname
c
c ---------------------------------------------------------------------+
*-
c -------------- variables for calculation -----------------------------

      integer maxrad,maxtheta,maxenerg,maxpred, nrad,npred
      character(180) infil, predfil, outfil
      integer p_rad_lo, p_rad_hi, p_energ_lo, p_energ_hi
      integer p_cts, p_err 
      integer p_theta_lo, p_theta_hi, p_pred_lo, p_pred_hi
      integer p_pred, p_reef, p_reef_err, p_pred_reef
      integer p_area_wgt, p_parea_wgt
  
c ------------- dynamic memory allocated array -------------------------

c     real*4 rad_lo(maxrad), rad_hi(maxrad)
c     real*4 cts(maxrad,maxtheta,maxenerg), err(maxrad,maxtheta,maxenerg)
c     real*4 theta_lo(maxtheta), theta_hi(maxtheta)
c     real*4 pred_lo(maxpred), pred_hi(maxpred)
c     real*4 pred(maxpred,maxtheta,maxenerg)
c     real*4 reef(maxrad,maxtheta,maxenerg)
c     real*4 reef_err(maxrad,maxtheta,maxenerg)
c     real*4 area_wgt(maxrad,maxtheta,maxenerg)
c     real*4 parea_wgt(maxpred,maxtheta,maxenerg)
c
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

c ----------------- INITIALISATION -------------------------------------+

      subname = 'rpsf2eef'
      taskname = 'rpsf2eef '

      call wtbegm(taskname,version,chatter)
c ----------------- GET PARAMETERS -------------------------------------+

      errflg   = 0
      call eef_gp (infil, predfil,outfil,instrum,chatter,killit,errflg)

      if (errflg .ne. 0) then
          subinfo='returning from eef_gp'  
          goto 100
      endif

c --------- get array size for dynamic memory allocation ---------------
c           before going to read the data files                         

      call get_dmasize(infil, predfil, theo_pres, data_pres, 
     >                 maxrad, maxpred, maxtheta, maxenerg, 
     >                 outhdu_class,chatter, errflg)

      if (errflg .ne. 0) then
          subinfo='returning from get_dmasize'  
          goto 100
      endif

c -------------- array size is obtained -------------------------------+ 
c                now go for dynamic memory allocation

      write(subinfo,'(a,4i4)')'maxrad,maxtheta,maxenerg,maxpred =',
     >                         maxrad,maxtheta,maxenerg,maxpred
      call wtinfo(chatter,30,3,subinfo)

c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      iget=0
      status = 0

      p_rad_lo = 0
      p_rad_hi = 0 
      p_theta_lo = 0
      p_theta_hi = 0
      p_energ_lo = 0
      p_energ_hi = 0
      p_cts = 0
      p_err = 0 
      p_pred_lo = 0
      p_pred_hi = 0
      p_pred = 0
      p_reef = 0 
      p_reef_err = 0
      p_area_wgt = 0
      p_pred_reef = 0
      p_parea_wgt = 0

      if(maxrad .lt. 50) maxrad = 50
      call udmget(maxrad, 6, p_rad_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad, 6, p_rad_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      if(maxtheta .lt. 50) maxtheta=50
      call udmget(maxtheta, 6, p_theta_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxtheta*4

      status = 0
      call udmget(maxtheta, 6, p_theta_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxtheta*4

      status = 0
      if(maxenerg .lt. 50) maxenerg=50
      call udmget(maxenerg, 6, p_energ_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxenerg*4

      status = 0
      call udmget(maxenerg, 6, p_energ_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxenerg*4

      status = 0
      maxtheta=1
      maxenerg=1
      call udmget(maxrad*maxtheta*maxenerg, 6, p_cts, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxenerg*4

      status = 0
      call udmget(maxrad*maxtheta*maxenerg, 6, p_err, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxenerg*4

      status = 0
      if(maxpred .lt. 50) maxpred=50
      call udmget(maxpred, 6, p_pred_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred, 6, p_pred_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred*maxtheta*maxenerg, 6, p_pred, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*maxenerg*4

      status = 0
      call udmget(maxrad*maxtheta*maxenerg, 6, p_reef, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxenerg*4

      status = 0
      call udmget(maxrad*maxtheta*maxenerg, 6, p_reef_err, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxenerg*4

      status = 0
      call udmget(maxrad*maxtheta*maxenerg, 6, p_area_wgt, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxenerg*4

      status = 0
      call udmget(maxpred*maxtheta*maxenerg, 6, p_pred_reef, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*maxenerg*4

      status = 0
      call udmget(maxpred*maxtheta*maxenerg, 6, p_parea_wgt, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*maxenerg*4

 50   ineed = 2*maxrad*4 + 2*50*4 + 2*50*4 + 2*maxpred*4 + 
     >        5*maxrad*maxtheta*maxenerg*4 +
     >        3*maxpred*maxtheta*maxenerg*4
      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed 
      call wtinfo(1,10,4,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory I get   =',iget
      call wtinfo(1,10,4,subinfo)

      if (status .ne. 0) then
          errflg = -1
          subinfo=' failed to allocate dynamic memory ' 
          call wterrm(subname,version,subinfo)
          goto 100
      endif
									
c ------------------ READ DATA FILE -----------------------------------+

      call eef_rdat(infil, predfil, MEMR(p_rad_lo), MEMR(p_rad_hi), 
     >              MEMR(p_theta_lo), MEMR(p_theta_hi),
     >              MEMR(p_energ_lo), MEMR(p_energ_hi),
     >              MEMR(p_cts), MEMR(p_err), MEMR(p_pred_lo), 
     >              MEMR(p_pred_hi), MEMR(p_pred), maxrad, maxpred, 
     >              maxtheta, maxenerg, nrad, npred, nenerg, ntheta, 
     >              MEMR(p_area_wgt), MEMR(p_parea_wgt),
     >              outhdu_class,
     >              ohduclas3, ohduclas4, phduclas3, phduclas4,
     >              theo_pres, data_pres, telescop, instrume,
     >              radunit, thetaunit, energunit, rpsfunit,
     >              pix_size, backgrnd, errflg, chatter)

      if (errflg .ne. 0) then
          subinfo='returning from eef_rdat'  
          goto 100 
      endif               

c ----------------- CONVERSION CALCULATION ----------------------------+

      call eef_conv (MEMR(p_rad_lo), MEMR(p_rad_hi), nrad, maxrad, 
     >               MEMR(p_pred_lo), maxtheta,maxenerg,
     >               MEMR(p_pred_hi), npred,maxpred, 
     >               MEMR(p_cts), MEMR(p_err), MEMR(p_pred),  
     >               nenerg, ntheta, MEMR(p_area_wgt), 
     >               MEMR(p_parea_wgt), backgrnd, pix_size, 
     >               MEMR(p_reef),MEMR(p_reef_err),MEMR(p_pred_reef),
     >               theo_pres, data_pres, errflg, chatter) 

      if (errflg .ne. 0) then
          subinfo='returning from eef_conv'  
          goto 100 
      endif               

c ---------------------- WRITE TO OUTPUT FILE -------------------------+


      call eef_wt(outfil, MEMR(p_rad_lo), MEMR(p_rad_hi), nrad, maxrad, 
     >            MEMR(p_pred_lo), MEMR(p_pred_hi),npred, maxpred,  
     >            MEMR(p_theta_lo), MEMR(p_theta_hi), ntheta, maxtheta, 
     >            MEMR(p_energ_lo), MEMR(p_energ_hi), nenerg, maxenerg, 
     >            MEMR(p_pred), MEMR(p_reef), MEMR(p_reef_err), 
     >            MEMR(p_pred_reef), backgrnd, MEMR(p_area_wgt), 
     >            MEMR(p_parea_wgt), instrume, telescop,
     >            ohduclas3, ohduclas4, phduclas3, phduclas4,
     >            radunit,thetaunit,energunit,rpsfunit,reefunit,
     >            data_pres, theo_pres, killit, errflg, chatter) 


      if (errflg .ne. 0) then
          subinfo='returning from eef_wt'  
      endif

 100  if (errflg .ne. 0) then 
          call wterrm(subname,version,subinfo)
      endif


c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_rad_lo, 6, status)
      status = 0
      call udmfre(p_rad_hi, 6, status)
      status = 0
      call udmfre(p_theta_lo, 6, status)
      status = 0
      call udmfre(p_theta_hi, 6, status)
      status = 0
      call udmfre(p_energ_lo, 6, status)
      status = 0
      call udmfre(p_energ_hi, 6, status)
      status = 0
      call udmfre(p_cts, 6, status)
      status = 0
      call udmfre(p_err, 6, status)
      status = 0
      call udmfre(p_pred_lo, 6, status)
      status = 0
      call udmfre(p_pred_hi, 6, status)
      status = 0
      call udmfre(p_pred, 6, status)
      status = 0
      call udmfre(p_area_wgt, 6, status)
      status = 0
      call udmfre(p_parea_wgt, 6, status)
     
      if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wterrm(subname,version,subinfo)
          errflg=99
      endif

      call wtendm(taskname,version,errflg,chatter)
      return
      end
c ----------------------------------------------------------------------
c            **** END OF RPSF2EEF **** 
c ----------------------------------------------------------------------   
c            **** START OF THE SUBROUTINES ****
c ----------------------------------------------------------------------   
c ---------------------------------------------------------------------+   
*+EEF_GP
c
      subroutine eef_gp(infil,predfil,outfil,instrum,chatter,killit,
     >                  errflg)

c --------------- DESCRIPTION OF EEF-GP --------------------------------
c
c This subroutine reads the user defined parameters as well as chatterflag.
c
c --------------- VARIABLES --------------------------------------------

      implicit none
      character*(*) infil, predfil, outfil, instrum 
      integer chatter, errflg
      logical killit
c
c --------------- VARIABLE DIRECTORY -----------------------------------
c
c infil    char : (input) FITS PSF file (user defined)
c outfil   char : (input) Output (EEF FITS FILE) filename
c chatter  int  : (input) chattiness flag (quiet<5, warning>5, verbose>20)
c errflg   int  : error flag (no error=0), returned to main
c
c --- CALLED ROUTINES --------------------------------------------------
c
c UCLGST     : (HOST) Get string input
c UCLGSI     : (HOST) Get integer input
c UCLGSB     : (HOST) Get boolean input
c FCECHO     : (FTOOLS) Screen write
c FTUPCH     : (FTOOLS/FITSIO) converts string to upper case
c 
c --------------- AUTHORS/MODIFICATION ---------------------------------
c
c Banashree Mitra Seifert (Nov. 24, 1995)
c Peter D Wilson (Jul 01, 1998) 1.0.1:
c       . Drop INQUIRE test. Use ftrtnm for stripping of extension number
c
c-----------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.0.1')
      character(7) subname
*-
c --------------- INTERNAL DECLARATIONS --------------------------------

      character(180) filename, ill_file(3)  
      character(120) subinfo, fileinfo
      integer  status, extnum, n_ill
      logical ext, valfil

c ----------------- GET INPUT FILE PARAMETERS --------------------------


      subname ='eef_gp'
      subinfo  = 'using '//subname//version
      call wtinfo(chatter,10,1,subinfo)

      n_ill = 0
      status = 0
      call uclgst('infil', infil, status)
      if (status .ne. 0) then
          subinfo = 'getting datafile parameters !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(infil) 
      if (infil .eq. '  ') then
          subinfo = 'input file has to be entered, or none !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif 

C PDW 7/1/98: Drop INQUIRE. Use ftrtnm to strip off extension number
C      call fcpars(infil, filename, extnum, status)
      call ftrtnm( infil, filename, status )
      call crmvlbk(filename)
      n_ill = n_ill + 1
      ill_file(n_ill) = filename
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
      if ( filename .eq. '  ' ) then
          subinfo= 'input file does not exist !'
          call wterrm(subname,version,subinfo)
          fileinfo = 'filename : '//filename
          call wterrm(subname,version,fileinfo)
          errflg = 1
          return
      endif                    

c ---------------- GET PRED_PSF FILENAME -------------------------------

      status = 0
      call uclgst('predfil', predfil, status)
      if (status .ne. 0) then
          subinfo = 'getting theo_psf parameters '
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif
      if (predfil(1:1) .eq. '%') then
          predfil = filename
      endif
        status = 0
C PDW 7/1/98: Drop INQUIRE. Use ftrtnm to strip off extension number
C        call fcpars(predfil, filename, extnum, status)
        call ftrtnm( predfil, filename, status )
        n_ill = n_ill +1
        ill_file(n_ill) = filename
C        ext = .true.
C        INQUIRE(FILE=filename, EXIST=ext)
C        if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
        if ( filename .eq. '  ' ) then
            subinfo = 'predfil file does not exist '
            call wterrm(subname,version,subinfo)
            fileinfo = 'filename : '//filename
            call wterrm(subname,version,fileinfo)
            errflg = 1
            return
        endif         

        call uclpst ('predfil','%',status)
        if (status .ne. 0) then
            subinfo ='putting default predfile parameter'
            call wterrm(subname,version,fileinfo)
        endif 

c ---------------- OUTFILE NAME ----------------------------------------

        call uclgst('outfil', outfil, status)
        if (status .ne. 0) then
            subinfo = 'getting outfile parameter'
            call wterrm(subname,version,fileinfo)
        endif
        call crmvlbk(outfil)

c ---- TERMINATE WITH AN ERROR MESG IF USER DOES NOT GIVE OUTFIL -------

        if (outfil  .eq.  '  ') then
            subinfo = 'output filename must be entered !'
            call wterrm(subinfo)
            errflg=1
            return
        endif

c ----------------- READ IN CLOBBER ------------------------------------
      
        status = 0
        call uclgsb('clobber', killit, status)
        if (status .ne. 0) then
            subinfo = 'getting clobber parameter........'
            call wterrm(subname,version,fileinfo)
            killit =.false.
            status = 0
        endif 

c ------------------ check validity of outfil --------------------------

      valfil = .true.
      call ck_file(outfil,ill_file,n_ill,valfil,killit,chatter)
      if (.not. valfil) then
          subinfo = ' outfil is not valid !'
          call wterrm(subname,version,fileinfo)
          errflg = 1
          return
      endif 

c ----------------- CHATTER PARAMETER ------------------------------

      status = 0
      call uclgsi('chatter',chatter,status)
      if (status .ne. 0) then
          subinfo = 'getting chatter parameter !'
          call wterrm(subname,version,fileinfo)
          chatter = 9
          status = 0
      endif               
      return
      end
c ----------------------------------------------------------------------
c                       END OF EEF_GP
c ----------------------------------------------------------------------      
c
*+GET_DMASIZE
c ---------------------------------------------------------------------+
      subroutine get_dmasize(infil, predfil, theo_pres, data_pres, 
     >                       maxrad, maxpred, maxtheta, maxenerg, 
     >                       outhdu_class,chatter, errflg)

c ----------------------- DESCRIPTION ----------------------------------
c This subroutine reads the data file (predicted and observed) and gets 
c the maximum array size required for dynamic memory allocation.

c ----------------------- ROUTINES CALLED ------------------------------
c    MVEXT, FGTCNO, CRMVBLK, FTGKYS, PAR_DIM

c ----------------------- DECLARE VARIABLES ----------------------------

       implicit none
       character*(*) infil, predfil
       character*(*) outhdu_class
       integer maxrad, maxpred, maxtheta, maxenerg, errflg
       logical theo_pres,data_pres
     
c -------------------- AUTHORS/MODIFICATIONS ---------------------------
c
c Banashree Mitra Seifert (Nov. 24, 1995)
c 
c ----------------------------------------------------------------------
        character(5) version
        parameter (version='1.0.0')
        character(12)  subname 
c ----------------------------------------------------------------------
*-
 
c ------------------- INTERNAL VARIABLES -------------------------------

       character(120) subinfo
       character(20) instr(4), extnames(50), outver(9,50), extname
       character(50) outhdu(9,50), comm , wrnstr
       character(20)  ch_dim, dim_str
       integer status, ninstr, nsearch, iunit, next(50), chatter 
       integer colnum, ierr, ndim, rpsf_dim(3)
       logical casesen

c ----------------------------------------------------------------------
       subname = 'get_dmasize'
       subinfo='using '//subname//version
       call wtinfo(chatter,10,3,subinfo)

       status = 0
       ninstr = 2
       instr(1) = 'RESPONSE'
       instr(2) = 'RPRF' 
       nsearch  = 50
       call mvext (0, infil, iunit, ninstr, instr, nsearch, next, 
     >            outhdu, extnames, outver, extname, errflg, chatter) 

       if ((errflg .ne. 0) .and. (errflg .ne. 2))then
            subinfo='returning from mvext subroutine'
            call wtinfo(chatter,10,3,subinfo)
            call ftclos(iunit,status)
            subinfo = ' closing PSF observed data file' 
            call wtferr(subname,version,status,subinfo)
            return
       endif            

c ----------------------------------------------------------------------

c  If there is observed data then get the parameters &
c  if there is no observed data, go looking for theoretical data

c ----------------------------------------------------------------------
c ----------------------------------------------------------------
c       case if one extension is found
c ----------------------------------------------------------------

      if(errflg .eq. 0) then
          outhdu_class = outhdu(3,1)
          if(outhdu_class .eq. 'PREDICTED') then
             theo_pres=.true.
             data_pres=.false.             
             subinfo='Only predicted data is found'
             call wtinfo(chatter,10,3,subinfo)
          elseif((outhdu_class .eq. 'NET') .or. 
     >           (outhdu_class .eq. 'TOTAL')) then 
             theo_pres=.false.
             data_pres=.true.
             subinfo='Only observed data is found'
             call wtinfo(chatter,10,3,subinfo)
          endif

          errflg = 0
          ninstr = 3 
          instr(1) = 'RESPONSE'
          instr(2) = 'RPRF'
          instr(3) = outhdu_class
          nsearch  = 50
          call mvext (0, infil, iunit, ninstr, instr, nsearch, next, 
     >               outhdu, extnames, outver, extname, errflg, chatter) 

          if (errflg .ne. 0) then
              subinfo='returning from mvext subroutine'
              call wtinfo(chatter,10,3,subinfo)
              call ftclos(iunit,status)
              subinfo = ' closing PSF observed data file'
              call wtferr(subname,version,status,subinfo)
              return
          endif

          casesen = .false.
          call ftgcno (iunit, casesen, 'RPSF', colnum, status) 
 
          write(ch_dim, '(a,i1)', iostat=ierr) 'TDIM', colnum
          if (ierr .ne. 0) then
              wrnstr= 'assume that pred RPSF col no. is 3'
              call wtwarm(subname,version,chatter,1,wrnstr)
          endif
          call crmvlbk(ch_dim)

          call ftgkys(iunit, ch_dim, dim_str, comm, status)

          call par_dim(dim_str, ndim, rpsf_dim, ierr)
          maxpred  = rpsf_dim(1)
          maxtheta = rpsf_dim(2)
          maxenerg = rpsf_dim(3)

          call ftclos(iunit,errflg)
          write(subinfo, '(2a,i6)') 'Max radius array dimension', 
     >                            ' for theoretical data=', maxpred 
          call wtinfo(chatter,30,3,subinfo)

      endif

c ----------------------------------------------------------------
c       case if more than one extension is found
c ----------------------------------------------------------------

      if(errflg .eq. 2) then
         subinfo='more than one extension found'
         call wtinfo(chatter,10,1,subinfo)

      if((outhdu(3,1) .eq. 'NET') .or. (outhdu(3,1) .eq. 'TOTAL')) then
          outhdu_class = outhdu(3,1)
          data_pres=.true.
          theo_pres=.false.

          subinfo='has observed data extension'
          call wtinfo(chatter,10,3,subinfo)

          status = 0
          ninstr = 3 
          instr(1) = 'RESPONSE'
          instr(2) = 'RPRF'
          instr(3) = outhdu_class
          nsearch  = 50
          call mvext (0, infil, iunit, ninstr, instr, nsearch, next, 
     >               outhdu, extnames, outver, extname, errflg, chatter) 

          if (errflg .ne. 0) then
              subinfo='returning from mvext subroutine'
              call wtinfo(chatter,10,3,subinfo)
              call ftclos(iunit,status)
              subinfo = ' closing PSF observed data file'
              call wtferr(subname,version,status,subinfo)
              return
          endif

          casesen = .false.
          call ftgcno (iunit, casesen, 'RPSF', colnum, status) 

          write(ch_dim, '(a,i1)', iostat=ierr) 'TDIM', colnum
          if (ierr .ne. 0) then
              wrnstr= 'assume that RPSF col no. is 3'
              call wterrm(subname,version,wrnstr)
          endif
          call crmvlbk(ch_dim)

          call ftgkys(iunit, ch_dim, dim_str, comm, status)
          call par_dim(dim_str, ndim, rpsf_dim, ierr)
          maxrad   = rpsf_dim(1)
          maxtheta = rpsf_dim(2)
          maxenerg = rpsf_dim(3)

          write(subinfo, '(a,i6)') 'max radius array dimension=', 
     >                               maxrad
          call wtinfo(chatter,30,3,subinfo)

          call ftclos(iunit,errflg)
      endif

      if (outhdu(3,2) .eq. 'PREDICTED') then
          theo_pres=.true.
          subinfo='has predicted data extension'
          call wtinfo(chatter,10,3,subinfo)

          status = 0
          ninstr = 3
          instr(1) = 'RESPONSE'
          instr(2) = 'RPRF'
          instr(3) = 'PREDICTED' 
          nsearch  = 50
          call mvext (0, infil, iunit, ninstr, instr, nsearch, next,
     >               outhdu, extnames, outver, extname, errflg, chatter)

          if (errflg .ne. 0) then
              subinfo='returning from mvext subroutine'
              call wtinfo(chatter,10,3,subinfo)
              call ftclos(iunit,status)
              subinfo = ' closing PSF observed data file'
              call wtferr(subname,version,status,subinfo)
              return
          endif

          casesen = .false.
          call ftgcno (iunit, casesen, 'RPSF', colnum, status) 
 
          write(ch_dim, '(a,i1)', iostat=ierr) 'TDIM', colnum
          if (ierr .ne. 0) then
              wrnstr= 'assume that pred RPSF col no. is 3'
              call wtwarm(subname,version,chatter,1,wrnstr)
          endif
          call crmvlbk(ch_dim)

          call ftgkys(iunit, ch_dim, dim_str, comm, status)

          call par_dim(dim_str, ndim, rpsf_dim, ierr)
          maxpred  = rpsf_dim(1)
          maxtheta = rpsf_dim(2)
          maxenerg = rpsf_dim(3)

          call ftclos(iunit,errflg)
          write(subinfo, '(2a,i6)') 'Max radius array dimension', 
     >                            ' for theoretical data=', maxpred 
          call wtinfo(chatter,30,3,subinfo)
      endif
      endif

      return
      end

c ---------------------------------------------------------------------+
c                  END OF GET_DMASIZE SUBROUTINE              
c          maxrad,maxtheta,maxenerg and maxpred are obtained 
c ---------------------------------------------------------------------+ 

*+EEF_RDAT

      subroutine eef_rdat(infil, predfil, rad_lo, rad_hi,theta_lo,
     >                    theta_hi, energ_lo, energ_hi, cts, err,   
     >                    pred_lo, pred_hi, pred, maxrad, maxpred, 
     >                    maxtheta, maxenerg, nrad, npred, nenerg, 
     >                    ntheta,area_wgt, parea_wgt,
     >                    outhdu_class,
     >                    ohduclas3, ohduclas4, phduclas3, phduclas4,
     >                    theo_pres, data_pres, telescop, instrume,
     >                    radunit, thetaunit, energunit, rpsfunit,
     >                    pix_size,backgrnd,errflg,chatter)

c ------------------ DESCRIPTION ---------------------------------------
c
c This subroutine reads the data inputs for the RPSF2EEF task. It reads
c the RPSF FITS file for data and theoretical model         
c
c ------------------- VARIABLES ----------------------------------------

      implicit none
      character*(*) infil, predfil
      integer maxrad, maxtheta, maxenerg, maxpred, nrad, npred
      character*(*) ohduclas3, ohduclas4, phduclas3, phduclas4
      character*(*) outhdu_class
c      real rad_lo(maxrad), rad_hi(maxrad)
c      real cts(maxrad,maxtheta,maxenerg), err(maxrad,maxtheta,maxenerg)
c      real pred_lo(maxpred), pred_hi(maxpred), 
c      real pred(maxpred,maxtheta,maxenerg)

      real rad_lo(*), rad_hi(*), theta_lo(*), theta_hi(*)
      real energ_lo(*), energ_hi(*)
      real cts(maxrad,maxtheta,*), err(maxrad,maxtheta,*)
      real pred_lo(*), pred_hi(*) 
      real pred(maxpred,maxtheta,*)
      integer errflg,chatter
      logical theo_pres, data_pres
 
c -------------------- VARIABLE DIRECTORY ------------------------------
c 
c infil      char   : Name of FITS format PSF file
c maxen      int    : Maximum array size for observed psf data
c maxpred    int    : Maximum array size for predicted psf data
c nrad       int    : Counter for observed psf data
c npred      int    : Counter for predicted psf data
c rad_lo     real   : Array of lower edge of observed radial bins
c rad_hi     real   : Array of upper edge of observed radial bins
c cts        real   : Array of observed radial profile, in counts
c err        real   : Array of statistical errors
c pred_lo    real   : Array of lower edge of predicted model bins
c pred_hi    real   : Array of upper edge of predicted model bins
c pred       real   : Array of theoretical PSF
c
c --------------------------- CALLED ROUTINES ---------------------------
c 
c subroutine FTOPEN       : FITSIO routine to open file
c subroutine FTCLOS       : FITSIO routine to close file
c subroutine RD_RPSF1992a : (CALLIB) Reads observed PSF extension in FITS file
c subroutine CQDP_RTHEO   : Reads theoretical PSF extension in FITS file
c subroutine WT_FERRMSG   : Writes FITSIO error text if neccesary
c
c---------------- AUTHORS/MODIFICATION ---------------------------------
c
c Banashree Mitra Seifert (Nov. 24, 1995)
c
c-----------------------------------------------------------------------
       character(5) version
       parameter (version = '1.0.0')
       character(10) subname
*-
c ------------------- INTERNAL VARIABLES -------------------------------

      character(40) comm
      character(120) subinfo, errinfo
      character(20) hduclas3
      character*(*) telescop, instrume
      real pix_size, backgrnd
      integer status, iunit
      character*(*) radunit, thetaunit, energunit, rpsfunit
      integer ntheta, nenerg
      real perr(9092,1,1)
      real parea_wgt(maxpred,maxtheta,maxenerg)
      real  area_wgt(maxrad,maxtheta,maxenerg)
      logical qerror, qarea
      integer nsearch, ninstr, next(50)
      character(20) instr(50), outhdu(9,50), outver(9,50), extname
      character(20) extnames(50)
 
c ---------------------- INTERNALS -------------------------------------
c
c errmesg    char   : Error message text
c version    char   : Subroutine version
c status     int    : Error flag for FITSIO call
c iunit      int    : Fortran unit number for file
c
c ----------------- USER INFORMATION -----------------------------------

       subname ='rpsf_rdat' 
       subinfo = 'using '//subname//version
       call wtinfo(chatter,10,1,subinfo)

c ----------- OPENING PSF FILE -----------------------------------------

       if (data_pres) then
           status = 0
           ninstr = 3
           instr(1) = 'RESPONSE'
           instr(2) = 'RPRF' 
           instr(3) = outhdu_class 
           nsearch  = 50
           call mvext (0, infil, iunit, ninstr, instr, nsearch, next, 
     >            outhdu, extnames, outver, extname, errflg, chatter) 
           if (errflg .ne. 0) then
               subinfo = 'reading observed data'
               call wterrm(subname,version,subinfo)
               call ftclos(iunit,status)
               subinfo = 'closing PSF file' 
               call wtferr(subname,version,status,subinfo)
               return
           endif            
       

c ------------------- READING OBSERVED PSF EXTENSION -------------------


       call rdrpf1(iunit,hduclas3,nrad,rad_lo,rad_hi,radunit,
     >             ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     >             energ_lo,energ_hi,energunit,cts,qerror,err,
     >             rpsfunit,qarea,area_wgt,telescop,instrume,
     >             maxrad,maxtheta,errflg,chatter)
       
       if ((hduclas3 .eq. 'NET') .or. (hduclas3 .eq. 'TOTAL')) then
           ohduclas4 = hduclas3
           ohduclas3 = 'OBSERVED'
       endif
       if (errflg .ne. 0) then
           call ftclos(iunit,status)
           subinfo = 'closing obs_psf file' 
           call wtferr(subname,version,status,subinfo)
           return
       endif            

c ------------------- READ PIXSIZE -------------------------------------

       status = 0
       call ftgkye(iunit,'PIXSIZE',pix_size,comm,status)
       subinfo ='reading PIXSIZE !'
       call wtferr(subname,version,status,subinfo)
       pix_size = pix_size *60.


c ------------------- READ BACKGROUND FOR PSF DATE ---------------------

       status = 0
       call ftgkye(iunit,'BACKGRND',backgrnd,comm,status)
       errinfo = ' reading BACKGRND'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           backgrnd = 0
           errinfo = 'background is not supplied'
           call wtinfo(chatter,10,1,errinfo)
       endif
       status = 0

c -------------- closing the input file --------------------------------

       call ftclos(iunit,status)
       subinfo = 'closing '//infil
       call wtferr(subname,version,status,subinfo)
       
       endif
c -------------------- READING OF OBSERVED DATA IS DONE ----------------
c -------------------- NOW READING THEORETICAL PSF EXTENSION -----------

  100   errflg = 0
        if(theo_pres) then
           ninstr = 3
           instr(1) ='RESPONSE'
           instr(2) = 'RPRF'
           instr(3) = 'PREDICTED'
           nsearch = 50

          call mvext (0, predfil, iunit, ninstr, instr, nsearch,
     >                next, outhdu, extnames, outver, extname, 
     >                errflg, chatter) 
          if (errflg .ne. 0) then
              subinfo = 'reading theoretical data'
              call wterrm(subname,version,subinfo)
              call ftclos(iunit,status)
              subinfo = 'closing PSF file' 
              call wtferr(subname,version,status,subinfo)
              return
          endif            

c --------------------- READ THEO DATA -------------------------------+

          call rdrpf1(iunit,hduclas3,npred,pred_lo,pred_hi,radunit,
     >                   ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     >                   energ_lo,energ_hi,energunit,pred,qerror,perr,
     >                   rpsfunit,qarea,parea_wgt,telescop,instrume,
     >                   maxpred,maxtheta,errflg,chatter)    

          phduclas3 = hduclas3
          phduclas4 = 'NET' 
          if (errflg .eq. 1) then
              theo_pres = .false.
              errflg = 0
          endif
       endif

c ------------------- READ PIXSIZE ------------------------------------+

       if (theo_pres) then
           status = 0
           call ftgkye(iunit,'PIXSIZE',pix_size,comm,status)
           subinfo = 'reading PIXSIZE !'
           call wtferr(subname,version,status,subinfo)
           pix_size = pix_size *60.

c ------------------- READ BACKGROUND FOR PSF DATE --------------------+

           status = 0
           call ftgkye(iunit,'BACKGRND',backgrnd,comm,status)
           errinfo = 'reading BACKGRND'
           call wtferr(subname,version,status,errinfo)
           if (status .ne. 0) then
               backgrnd = 0
               errinfo = 'background is not supplied'
               call wtinfo(chatter,10,2,errinfo)
           endif
           status = 0

       endif
c ----------------- CLOSING FILE --------------------------------------+
       status = 0
       call ftclos(iunit,status)
       subinfo = 'closing '//predfil
       call wtferr(subname,version,status,subinfo)
  200  if ((.NOT.theo_pres).AND.(.NOT.data_pres)) then
            errflg = 5
            subinfo = 'neither PREDICTED nor OBSERVED data present'
            call wtinfo(chatter,10,2,subinfo)
       endif 
       return
       end

c --------------- END OF SUBROUTINE EEF_RDAT --------------------------+
c ---------------------------------------------------------------------+
*+EEF_CONV
      subroutine eef_conv (rad_lo, rad_hi, nrad, maxrad, pred_lo, 
     >                     maxtheta, maxenerg, pred_hi, npred, maxpred, 
     >                     cts, err, pred, nenerg, ntheta, 
     >                     area_wgt, parea_wgt, backgrnd, pix_size, 
     >                     reef, reef_err, pred_reef,
     >                     theo_pres, data_pres, errflg, chatter) 

c -------------------- DESCRIPTION ------------------------------------+
c Converts data from RPSF to EEF in FITS format
c ---------------------------------------------------------------------+
c since the counts are in units of counts/unit area, first cts is 
c converted to cts = (cts - background)* area under the annulus and 
c then summed up bin by bin 
c --------------------- variables -------------------------------------+
      implicit none
      integer maxrad, maxtheta,maxenerg, ntheta, nenerg, nrad
      integer  npred, maxpred
      real rad_lo(*), rad_hi(*)
      real cts(maxrad,maxtheta,*), err(maxrad,maxtheta,*)
      real pred_lo(*), pred_hi(*)
      real pred(maxpred,maxtheta,*), reef(maxrad,maxtheta,*)
      real reef_err(maxrad,maxtheta,*)
      real area_wgt(maxrad,maxtheta,*)
      real parea_wgt(maxpred,maxtheta,*)
      real pred_reef(maxpred,maxtheta,*)
      real pix_size, backgrnd
      integer errflg,chatter
      logical theo_pres, data_pres

c --------------- AUTHORS/MODIFICATION --------------------------------+
c
c Banashree Mitra Seifert (Nov. 24, 1995)
c
c ---------------------------------------------------------------------+
      character(5) version
      parameter (version = '1.0.0')
      character(9) subname 
*- 
c ----------------------- Internal variables --------------------------+

        real pi, sumcts_tmp(600), tot_cts,sumtcts, sigma_tot
        real rsq, area, areasq, pix_sq, reefsq
        integer ien, itheta, irad
        character(120)  subinfo
        pi = 4*atan(1.)

c ---------------------------------------------------------------------+
        subname = 'eef_conv' 
        subinfo='using '//subname//version
        call wtinfo(chatter,10,1,subinfo)

c ---------------------------------------------------------------------+

        pix_sq = pix_size*pix_size

        if (data_pres) then     
            subinfo= 'observed data  ----> present(eef_conv)'
            call wtinfo(chatter,10,3,subinfo)

               do ien = 1, nenerg
                  do itheta = 1, ntheta
                     tot_cts = 0.
                     sigma_tot = 0
                     do irad = 1, nrad
                        rsq = rad_hi(irad)*rad_hi(irad) - 
     >                        rad_lo(irad)*rad_lo(irad) 
                        area = pi * rsq * area_wgt(irad,itheta,ien)
                        areasq = area*area
                        tot_cts = tot_cts + (cts(irad,itheta,ien)-
     >                                backgrnd/pix_sq) * area
                        sumcts_tmp(irad) = tot_cts
                        sigma_tot = sigma_tot + areasq *
     >                       (err(irad,itheta,ien)*err(irad,itheta,ien))
                        reef_err(irad,itheta,ien) = sigma_tot
                     enddo  

                     do irad = 1, nrad
                       reef(irad,itheta,ien) = sumcts_tmp(irad)/tot_cts
                       reefsq = reef(irad,itheta,ien)*
     >                          reef(irad,itheta,ien)
                        reef_err(irad,itheta,ien) = 
     >                         sqrt(reef_err(irad,itheta,ien) + 
     >                                reefsq * sigma_tot)/tot_cts 

                     enddo
                  enddo
               enddo
        else
           subinfo= 'observed data  ----> absent(eef_conv)'
           call wtinfo(chatter,10,3,subinfo)
        endif

c ---------------- Now go for theoretical data if present -------------+
        if (theo_pres) then 
            subinfo= 'theoretical data ----> present(eef_conv)'
            call wtinfo(chatter,10,3,subinfo)

           do ien = 1, nenerg
              do itheta = 1, ntheta
                 sumtcts = 0.
                 do irad = 1, npred
                    rsq = pred_hi(irad)*pred_hi(irad) - 
     >                    pred_lo(irad)*pred_lo(irad)
                    area = pi * rsq 
                    pred(irad,itheta,ien)=
     >                      (pred(irad,itheta,ien)-backgrnd) * area
                    sumtcts = sumtcts + pred(irad,itheta,ien)
                    sumcts_tmp(irad) = sumtcts
                 enddo  
                 do irad = 1, npred
                    pred_reef(irad,itheta,ien) = 
     >                              sumcts_tmp(irad)/sumtcts
                 enddo
              enddo
           enddo
        else
          subinfo =  ' theoretical data ----> absent(eef_conv)'
          call wtinfo(chatter,10,3,subinfo)
        endif
        
        return
        end

c ---------------------------------------------------------------------+
c                END OF SUBROUTINE EEF_CONV
c ---------------------------------------------------------------------+
*+EEF_WT
c ---------------------------------------------------------------------+

      subroutine eef_wt(outfil, rad_lo, rad_hi, nrad, maxrad, 
     >                  pred_lo, pred_hi, npred, maxpred,  
     >                  theta_lo, theta_hi, ntheta, maxtheta, 
     >                  energ_lo, energ_hi, nenerg, maxenerg, 
     >                  pred, reef, reef_err, pred_reef, backgrnd, 
     >                  area_wgt,  parea_wgt, instrume, telescop,
     >                  ohduclas3, ohduclas4, phduclas3, phduclas4,
     >                  radunit,thetaunit,energunit,rpsfunit,reefunit,
     >                  data_pres, theo_pres, killit, errflg, chatter) 

c ---------------- DESCRIPTION OF EEF_WT ------------------------------+
c This subroutines checks for the HDUCLAS3 (Observed or predicted)
c and writes a FITS extension the REEF (Observed or predicted) 

c ---------------------- VARIABLES -------------------------------------

      implicit none
      character(120) subinfo
      character(180) outfil
      integer nrad, nenerg, ntheta, ounit, maxrad, maxtheta
      integer maxenerg,maxpred,errflg, npred 
      real rad_lo(*),rad_hi(*),area_wgt(maxrad,maxtheta,*)
      real parea_wgt(maxpred,maxtheta,*)
      real theta_lo(5), theta_hi(5), energ_lo(5),energ_hi(5)
      real pred_lo, pred_hi, pred(*), backgrnd
      real reef(maxrad,maxtheta,*), reef_err(maxrad,maxtheta,*)
      real pred_reef(maxrad, maxtheta,*)
      integer nk_hist, chatter, nk_comm, status
      character(50) extname
      character(80) hist(20),comms(20)
      character*(*) ohduclas3, ohduclas4, phduclas3, phduclas4
      character*(*) radunit, thetaunit, energunit, rpsfunit, reefunit
      character*(*) telescop, instrume
      logical qarea, qerror
      logical data_pres, theo_pres, killit

c ------------------- CALLED ROUTINES ----------------------------------
c WTEEF1
c
c ----------------- AUTHORS/MODIFICATIONS ------------------------------
c
c Banashree Mitra Seifert
c
c ----------------------------------------------------------------------
      character(5) version
      parameter (version='1.0.0')
      character(7) subname
*-
c ----------------------------------------------------------------------
      subname = 'eef_wt' 
      subinfo='using '//subname//version
      call wtinfo(chatter,10,1,subinfo)

c ----- opening and writing the header for EEF file to be written ------ 
      status=0
      call opnpa(outfil,chatter,ounit,killit,status)
      if (status .ne. 0) then
          subinfo='error opening output EEF file'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
      endif

c ------- writing the observed data -----------------------------------+
      
      if (data_pres) then
          subinfo= 'observed data  ----> present(eef_wt)'
          call wtinfo(chatter,10,3,subinfo)

         qarea=.true.
         qerror=.true.

         call wteef1(ounit, extname, ohduclas3, ohduclas4,nrad,
     >               rad_lo, rad_hi, radunit,ntheta, theta_lo, theta_hi,
     >               thetaunit, nenerg, energ_lo, energ_hi, energunit, 
     >               reef,qerror,reef_err, reefunit, qarea,area_wgt, 
     >               hist,nk_hist,comms, nk_comm, telescop, instrume,
     >               maxrad, maxtheta, errflg, chatter)       

      else
         subinfo='observed data  ----> absent(eef_wt) '
         call wtinfo(chatter,10,3,subinfo)
      endif

      if (errflg .ne. 0) then
          subinfo='error in writing observed data'
          call wtferr(subname,version,errflg,subinfo)
      endif
    
c -------- writing the predicted data ---------------------------------+

      if (theo_pres) then
          subinfo='theoretical data ----> present(eef_wt)'
          call wtinfo(chatter,10,3,subinfo)

          qerror=.false.
          qarea=.false.
  
          call wteef1(ounit, extname, phduclas3, phduclas4, maxpred, 
     >                pred_lo,pred_hi,radunit,ntheta,theta_lo,theta_hi, 
     >                thetaunit, nenerg, energ_lo, energ_hi, energunit, 
     >                pred_reef, qerror, reef_err, reefunit, qarea,
     >                parea_wgt, hist,nk_hist,comms, nk_comm, telescop, 
     >                instrume, maxpred, maxtheta, errflg, chatter)       

      else
          subinfo= 'predicted data ----> absent(eef_wt) '
          call wtinfo(chatter,10,3,subinfo)
      endif

      if (errflg .ne. 0) then
          subinfo='error in writing predicted  data'
          call wtferr(subname,version,errflg,subinfo)
      endif
      call ftclos(ounit, status)
      if (status .ne. 0) then
          subinfo='cannot close the output file'
          call wtferr(subname,version,status,subinfo)
          return 
      endif

      return
      end

c ---------------------------------------------------------------------
c               END OF EEF_WT
c --------------------------------------------------------------------- 

