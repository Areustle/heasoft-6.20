
*+RPSFQDP
      subroutine rpsfqp
c
c -------------------- DESCRIPTION --------------------------------
c
c RPSFQDP reads the input file and looks for whether the file has RPSF data
c or the REEF data (observation and/or theoretical)
c If found to be RPSF data then it calls CQDP_RPSF to read input data
c If found to be REEF data then it calls CQDP_REEF to read input data
c CQDP_RPSF reads OBS RPSF and PRED PSF extensions in CALRPSF format FITS 
c file. Then it subsequently writes the RPSF/REEF data into an output file
c in QDP format.
c
c -------------------- VARIABLES --------------------------------
c
      implicit none
      character(180) fits_psf,qdp_psf,pred_psf
      character(150) subinfo
      integer maxrad,maxpred,maxtheta,maxenerg,nrad,npred
      integer chatter,ierr, status, iget, ineed
      integer nset, i
      character(20) hdu_sav(9,50)
      integer p_rad_lo,p_rad_hi, p_cts,p_err
      integer p_prad_lo, p_prad_hi, p_pred, p_rad_mean, p_del_rad
      integer p_prad_mean, p_pdel_rad
      integer p_reef, p_reef_err, p_pred_reef
      real rescale 
      real pix_size,backgrnd
      logical theo_pres,data_pres,killit
      logical rpsf_pres, reef_pres
c
c ------------------- VARIABLE DIRECTORY ------------------------------
c
c fits_psf   char   : Name of PSF file (user defined)
c qdp_psf    char   : Name of QDP file (user defined)
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
c prad_lo    real   : Array of lower edge of predicted model bins
c prad_hi    real   : Array of upper edge of predicted model bins
c pred       real   : Array of theoretical PSF
c rad_mean   real   : Array of mean of observed radial bins
c del_rad    real   : Array of half-width of observed radial bins 
c prad_mean  real   : Array of mean predicted model bins
c pdel_rad   real   : Array of half-width predicted model bins
c rescale    real   : Scalar rescaling factor for pred ONLY
c ierr       int    : error flag, 0 is okay
c thoe_pres  logical: true if theoretical extension is present
c 
c --------------------- CALLED ROUTINES ------------------------------
c
c subroutine CQDP_GP    : Reads user defined parameters using XPI
c subroutine CQDP_RPSF  : Reads FITS format RPSF file
c subroutine CQDP_REEF  : Reads FITS format REEF file
c subroutine CQDP_CONV  : Converts data into desired format
c subroutine CQDP_WT    : Writes data into QDP format output file
c
c ------------------ AUTHORS/MODifICATION ----------------------------
c
c Rehana Yusaf (1993 Feb)
c Rehana Yusaf (1993 May 27) : If theoretical ext not present,only
c                              OBS RPSF written
c Rehana Yusaf (1993 SEpt 21) : Change to read RPSFVER 1993a instead of 
c                               1992a
c Rehana Yusaf (1994 Jan 31) 1.0.2; Additional paramters ...
c                                   . pred_psf, filename for pred data
c                                   . data_pres, logical,true if obs data
c                                     present
c Rehana Yusaf (1994 Feb3) 1.0.3; CQDP_RPSF updated so that the program
c                                 is NOT terminated if pix_size not present
c Rehana Yusaf (1994 Sept 12) 1.0.4; Minor cosmetics to cqdp_rpsf and cqdp_wt
c Rehana Yusaf (1995 Jan 13) 1.0.5; update _gp to no longer read defval from parfile
c Ian M George (1995 Apr 05) 2.0.0; added rescale parameter to allow user to 
c				  rescale the theoretical ONLY
c Rehana Yusaf (1995 April 25) 2.0.1; add clobber
c Banashree Mitra Seifert (1995 December) 3.0.0; 
c                   . replaced calls to FNDHDU, FNDEXT, FTMRHD, FTMAHD by 
c                     CALL MVEXT
c                   . added reader for REEF data RDEEF1
c                   . added Dynamic Memory Allocation
c                   . screen display subroutines used  
c                     wtbegm,wtendm,wterrm,wtinfo
c Rehana Yusaf (1996 Feb 22) 3.0.1; bugfix  
c Peter D Wilson (1998 Jul 01) 3.0.2: Updated for new FCPARS behavior
c -----------------------------------------------------------------------
      character(5) version
      parameter (version = '3.0.2')
*-
c ----------------------------------------------------------------------

      character(40) taskname
      character(8) subname 
cccc      common/task/taskname

c ------------- dynamic memory allocated array -------------------------

c     real*4 rad_lo(maxrad), rad_hi(maxrad)
c     real*4 cts(maxrad,maxtheta,maxenerg), err(maxrad,maxtheta,maxenerg)
c     real*4 theta_lo(maxtheta), theta_hi(maxtheta)
c     real*4 pred_lo(maxpred), pred_hi(maxpred)
c     real*4 pred(maxpred,maxtheta,maxenerg)
c     real*4 reef(maxrad,maxtheta,maxenerg)
c     real*4 rad_mean(maxrad), del_rad(maxrad), pdel_rad(maxpred)
c     real*4 pred_mean(maxpred,maxtheta,maxenerg) 
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
      subname  = 'rpsfqdp'
      taskname = 'rpsfqdp'

c ----------------- Get parameter file --------------------------------+

      call cqdp_gp(fits_psf,pred_psf,qdp_psf,rescale,ierr,
     >             killit,chatter)

      call wtbegm(taskname,version,chatter)

      if (ierr .ne. 0) then
          goto 100
      endif

  
      maxrad   = 1000
      maxpred  = 1000
      maxtheta = 5
      maxenerg = 5

c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      iget=0
      status = 0

      p_rad_lo = 0
      p_rad_hi = 0
      p_rad_mean = 0
      p_del_rad = 0
      p_pdel_rad = 0
      p_prad_mean = 0
      p_cts = 0
      p_err = 0 
      p_prad_lo = 0
      p_prad_hi = 0
      p_pred = 0
      p_reef = 0 
      p_reef_err = 0
      p_pred_reef = 0

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
      call udmget(maxrad, 6, p_rad_mean, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad, 6, p_del_rad, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxpred, 6, p_pdel_rad, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred*maxtheta*maxenerg, 6, p_prad_mean, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*maxenerg*4

      status = 0
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
      call udmget(maxpred, 6, p_prad_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred, 6, p_prad_hi, status)
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
      call udmget(maxpred*maxtheta*maxenerg, 6, p_pred_reef, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*maxenerg*4


 50   ineed = 4*maxrad*4 + 3*maxpred*4 + 4*maxrad*maxtheta*maxenerg*4 +
     >        3*maxpred*maxtheta*maxenerg*4
      write(subinfo, '(a,i10)')'dmasize required for this task=', ineed
      call wtinfo(chatter,15,2,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory  =',iget
      call wtinfo(chatter,15,2,subinfo)

      if (status .ne. 0) then
         ierr = -1
         subinfo=' failed to allocate dynamic memory '
         call wterrm(subname,version,subinfo)
         goto 100
      endif

c --------------------------------------------------------------------
c Calling the subroutine decide to search for the type of input file
c e.g., type RPSF --> predicted or observed
c       type REEF --> predicted or observed
c nset  gives the no of such sets found
c --------------------- Call Decide ----------------------------------

      nset = 0
      call decide(fits_psf, nset, hdu_sav, chatter)   
      Do i=1,nset

      if(hdu_sav(i,2) .eq. 'RPRF') then

         rpsf_pres=.true.
         reef_pres=.false.
      call cqdp_rpsf(fits_psf, pred_psf, MEMR(p_rad_lo),
     >               MEMR(p_rad_hi), MEMR(p_cts),MEMR(p_err),
     >               MEMR(p_prad_lo), MEMR(p_prad_hi), MEMR(p_pred),
     >               maxrad, maxpred, maxtheta, nrad, npred,
     >               theo_pres, data_pres, pix_size, backgrnd,
     >               ierr, chatter)

          if (ierr .ne. 0) then
              goto 100
          endif  

      call cqdp_conv(MEMR(p_rad_lo), MEMR(p_rad_hi), MEMR(p_prad_lo),
     >               MEMR(p_prad_hi), MEMR(p_rad_mean), MEMR(p_del_rad),
     >               MEMR(p_prad_mean), MEMR(P_pdel_rad), maxrad, 
     >               maxpred, nrad, npred, theo_pres, data_pres, ierr, 
     >               chatter)
      if (ierr .ne. 0) then
          goto 100
      endif  

      call cqdp_wt(fits_psf, qdp_psf, MEMR(p_rad_mean), 
     >             MEMR(p_del_rad), MEMR(p_cts), MEMR(p_err), 
     >             MEMR(p_prad_mean), MEMR(p_pdel_rad), MEMR(p_pred),
     >             maxrad, maxpred, maxtheta, nrad, npred, theo_pres, 
     >             data_pres, rescale, pix_size, backgrnd, rpsf_pres,
     >             reef_pres, ierr, chatter,killit)

      if (ierr .ne. 0) then
          goto 100
      endif  

      elseif(hdu_sav(i,2) .eq. 'REEF') then

       rpsf_pres=.false.
       reef_pres=.true.
       call cqdp_reef(fits_psf,pred_psf,MEMR(p_rad_lo),
     >                MEMR(p_rad_hi),MEMR(p_reef), MEMR(p_reef_err),
     >                MEMR(p_prad_lo), MEMR(p_prad_hi),
     >                MEMR(p_pred_reef), maxrad, maxpred, maxtheta,
     >                nrad,npred,theo_pres, data_pres,pix_size,
     >                backgrnd,ierr,chatter)

          if (ierr .ne. 0) then
              goto 100
          endif

      call cqdp_conv(MEMR(p_rad_lo), MEMR(p_rad_hi), MEMR(p_prad_lo),
     >               MEMR(p_prad_hi), MEMR(p_rad_mean), MEMR(p_del_rad),
     >               MEMR(p_prad_mean), MEMR(P_pdel_rad), maxrad, 
     >               maxpred, nrad, npred, theo_pres, data_pres, ierr, 
     >               chatter)
      if (ierr .ne. 0) then
          goto 100
      endif  

      call cqdp_wt(fits_psf, qdp_psf, MEMR(p_rad_mean), 
     >             MEMR(p_del_rad), MEMR(p_reef), MEMR(p_reef_err), 
     >             MEMR(p_prad_mean), MEMR(p_pdel_rad), 
     >             MEMR(p_pred_reef),
     >             maxrad, maxpred, maxtheta, nrad, npred, theo_pres, 
     >             data_pres, 1., pix_size, 0., rpsf_pres,reef_pres, 
     >             ierr, chatter,killit)

      if (ierr .ne. 0) then
          goto 100
      endif  

      else
         subinfo='input file is not an RPSF nor a REEF file'
         call wterrm(subname,version,subinfo)
         goto 100
      endif

c ---------------------------------------------------------------------+
      ENDDO

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_rad_lo, 6, status)
      status = 0
      call udmfre(p_rad_hi, 6, status)
      status = 0
      call udmfre(p_rad_mean, 6, status)
      status = 0
      call udmfre(p_del_rad, 6, status)
      status = 0
      call udmfre(p_pdel_rad, 6, status)
      status = 0
      call udmfre(p_prad_mean, 6, status)
      status = 0
      call udmfre(p_cts, 6, status)
      status = 0
      call udmfre(p_err, 6, status)
      status = 0
      call udmfre(p_prad_lo, 6, status)
      status = 0
      call udmfre(p_prad_hi, 6, status)
      status = 0
      call udmfre(p_pred, 6, status)
      status = 0
      call udmfre(p_reef, 6, status)
      status = 0
      call udmfre(p_reef_err, 6, status)
      status = 0
      call udmfre(p_pred_reef, 6, status)
     
      if (status .ne. 0) then
          subinfo= ' failed to de-allocate memory '
          call wterrm(subname,version,subinfo) 
          ierr=99
      endif

100   call wtendm(taskname,version,ierr,chatter)
      return
      end
c ----------------------------------------------------------------------
c            end OF RPSFQDP         
c ------------------------------------------------------------------

*+CQDP_GP
      subroutine cqdp_gp(infile,pred_psf,outfile,rescale,ierr,
     &                   killit,chatter)
c ------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c This routine gets the user defined parameters, that is the input and
c output file names. As well as chatter flag.
c
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character(180) infile,outfile,ill_files(3)
      character(180) filename,pred_psf,tmpfile
      integer err,chatter,ierr,extnum,n_ill
      character(80) errmess, subinfo
      real rescale
      logical ext,killit,valfil
c
c --- VARIABLE DIRECTORY ---
c
c infile     char   : Name of FITS PSF file (user defined) 
c outfile    char   : Name of QDP output file (user defined)
c chatter    int    : Chattiness flag (<5quiet,>5normal,>20 noisy)   
c rescale    real   : Scalar rescaling factor for pred ONLY
c err        int    : Error flag
c ierr       int    : Error flag, returned to main
c version    char   : Version of subroutine
c errstr     char   : Error string for this routine
c errmess    char   : Error message
c
c --- COMPILATION AND LINKING ---
c
c Link with FTOOLS
c 
c --- AUTHORS/MODifICATION HISTORY ---
c
c Rehana Yusaf (1993 Feb)
c Rehana Yusaf (1993 May 27) : INQUIRE added
c Rehana Yusaf (1994 Jan 13) : remove defval
c Ian M George (2.0.0:95 Apr 05) added rescale parameter, removed header
c Rehana Yusaf (2.0.1:95 Apr 25); add clobber
c
c Banashree Mitra Seifert (1996, Jan) 2.1.0:
c             . Screen display subroutines used
c               wtinfo,wterrm
c Peter D Wilson (1998 Jul 01) 2.1.1:
c             . Drop INQUIRE tests.
c --------------------------------------------------------------------
      character(5) version
      parameter (version = '2.1.1')
      character(8) subname 
*-
c --------------------------------------------------------------------
       subname = 'cqdp_gp'
       subinfo= 'using ' //subname//' '//version
       call wtinfo(chatter,10,1,subinfo)

c -------------- READING INPUT FILE PARAMETER -----------------------

       err=0
       call uclgst('datafile',infile,err)
       if (err.NE.0) then
          errmess = 'getting datafile parameter'
          call wterrm(subname,version,errmess)
       endif
       call crmvlbk(infile)
       if (infile .eq. '  ') then
         errmess = ' Input file must be entered ,or NONE !'
         call wterrm(subname,version,errmess)
         ierr =1
         return
       endif
C PDW 7/1/98: Don't bother! Let FTOPEN determine if file exists
C       call fcpars(infile,filename,extnum,err)
C       call crmvlbk(filename)
C       tmpfile = filename
C       call ftupch(tmpfile)
C       if (tmpfile.NE.'NONE') then
C         INQUIRE(FILE=filename,EXIST=ext)
C         if (.NOT.ext) then
C          errmess = ' input file does not exist !'
C          call wterrm(subname,version,errmess)
C          ierr = 1
C          return
C         endif
C       endif

c
c -------------------- READ PRED_PSF ---------------------
c
       call uclgst('predfile',pred_psf,err)
       if (err.NE.0) then
          errmess = ' getting predfile parameter'
          call wterrm(subname,version,errmess)
       endif       
       call uclpst('predfile','%',err)
       if (err.NE.0) then
          errmess = ' Putting default predfile parameter'
          call wterrm(subname,version,errmess)
       endif    
       call crmvlbk(pred_psf)
       if ((pred_psf(1:1).EQ.'%').OR.(pred_psf.EQ.' ')) then
         if (infile.EQ.'NONE') then
          errmess = ' Either predfile or infile have to be entered'
          call wterrm(subname,version,errmess)
          ierr = 2
          return
         ELSE
          pred_psf = infile  
         endif
       ELSE
         ierr = 0
         call fcpars(pred_psf,filename,extnum,ierr) 
         tmpfile = filename
         call ftupch(tmpfile)
         if (tmpfile.EQ.'NONE') then
           if (infile.EQ.'NONE') then
             errmess = ' Either predfile or infile have to be entered'
             call wterrm(subname,version,errmess)
             ierr = 2
             return
           endif
C PDW 7/1/98: Don't bother! Let FTOPEN determine if file exists
C        ELSE
C          INQUIRE(FILE=filename,EXIST=ext)
C          if (.NOT.ext) then
C            errmess = ' input predfile does not exist !'
C            call wterrm(subname,version,errmess)
C            ierr = 1
C            return
C          endif      
         endif
       endif
          
c
c ------------- READING OUTPUT FILE PARAMETER -----------------------
c
       call uclgst('outfile',outfile,err)
       if (err.NE.0) then
          errmess = ' getting outfile parameter'
          call wterrm(subname,version,errmess)
       endif
c
c      <<<--- TERMINATE if USER DOES NOT ENTER OUTFILE--->>>
c
       call crmvlbk(outfile)
       if (outfile.EQ.'  ') then
         errmess=' must enter outfile name !'
         call wterrm(subname,version,errmess)
         ierr = 1
         return
       endif

c GET CLOBBER

       call uclgsb('clobber',killit,err)
       if (err.NE.0) then
         errmess = ' getting clobber'
         killit = .false.
         call wterrm(subname,version,errmess)
       endif
       n_ill = 0
       call ck_file(outfile,ill_files,n_ill,valfil,
     &              killit,chatter)
       if (.NOT.valfil) then
         errmess = ' Invalid outfile name'
         call wterrm(subname,version,errmess)
         ierr = 1
         return
       endif
c
c ----------------- READING CHATTER FLAG ------------------------
c
       call uclgsi('chatter',chatter,err)
       if (err.NE.0) then
          errmess = ' getting chatter parameter'
         call wterrm(subname,version,errmess)
       endif
c
c      <<<--- READING RESCALE PARAMETER --->>>
c
       call uclgsr('rescale',rescale,err)
       if (err.NE.0) then
          errmess = ' getting rescale parameter'
         call wterrm(subname,version,errmess)
	  errmess = 'setting Rescale = 1.0'
         call wterrm(subname,version,errmess)
	  err = 0 
	  rescale = 1.0
       endif
       return
       end

c -------------------------------------------------------------------
c             end OF SUBROUTINE CQDP_GP 
c -------------------------------------------------------------------

*+CQDP_RPSF
      subroutine cqdp_rpsf(fits_psf,pred_psf,rad_lo,rad_hi,cts,
     &     err,prad_lo,prad_hi,pred,maxrad,maxpred,maxtheta,nrad,
     &     npred,theo_pres,data_pres,pix_size,backgrnd,ierr,chatter)
c
c --- DESCRIPTION ---------------------------------------------------
c
c Reading RPSF FITS file using FITSIO.
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character(180) fits_psf,pred_psf
      integer maxrad,maxpred,nrad,npred,chatter,ierr,maxtheta
      real rad_lo(maxrad),rad_hi(maxrad)
      real cts(maxrad,maxtheta,1),err(maxrad,maxtheta,1)
      real prad_lo(maxpred),prad_hi(maxpred),pred(maxpred,maxtheta,1)
      logical theo_pres,data_pres
c
c --- INTERNAL VARIABLES ---
c
      character(40) errstr,comm
      character(80) subinfo,errinfo
      character(8) telescop,instrume
      real pix_size,backgrnd, tot_cts
      integer status,iunit
      character(16) radunit,thetaunit,energunit,rpsfunit
      character(16) hduclas3
      integer ntheta,nenerg
      real theta_lo(1),theta_hi(1),energ_lo(1),energ_hi(1)
      real perr(9092,1,1),parea_wgt(9092,1,1),area_wgt(300,1,1)
      logical qerror,qarea
      integer nsearch,ninstr,next(50)
      integer errflg
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(9,50)
c
c --- VARIABLE DIRECTORY ---
c
c Arguments ...
c 
c fits_psf   char   : Name of FITS format PSF file
c maxen      int    : Maximum array size for observed psf data
c maxpred    int    : Maximum array size for predicted psf data
c nrad       int    : Counter for observed psf data
c npred      int    : Counter for predicted psf data
c rad_lo     real   : Array of lower edge of observed radial bins
c rad_hi     real   : Array of upper edge of observed radial bins
c cts        real   : Array of observed radial profile, in counts
c err        real   : Array of statistical errors
c prad_lo    real   : Array of lower edge of predicted model bins
c prad_hi    real   : Array of upper edge of predicted model bins
c pred       real   : Array of theoretical PSF
c
c Internals ...
c
c errmess    char   : Error message text
c errstr     char   : Error text for this routine
c version    char   : Subroutine version
c status     int    : Error flag for FITSIO call
c iunit      int    : Fortran unit number for file
c
c --- CALLED ROUTINES ---
c 
c subroutine FTOPEN       : FITSIO routine to open file
c subroutine FTCLOS       : FITSIO routine to close file
c subroutine RD_RPSF1992a : (CALLIB) Reads observed PSF extension in FITS file
c subroutine CQDP_RTHEO   : Reads theoretical PSF extension in FITS file
c subroutine WT_FERRMSG   : Writes FITSIO error text if neccesary
c
c --- COMPILATION AND LINKING ---
c
c Link with FITSIO and FTOOLS
c
c --- AUTHORS/MODifICATION HISTORY ---
c
c Rehana Yusaf (1993 Feb)
c Rehana Yusaf (1994 Jan 31) 1.0.1; data_pres and pred_psf paramters added.
c                                   rd_rpsf1993a renamed tp rdrpf1
c Rehana Yusaf (1994 Sept 12) 1.0.2; only warn about theo not present at
c                                    chatter > 10 
c Banashree Mitra Seifert (1996, Jan) 1.1.0:
c             . Replaced by MVEXT
c             . Screen display subroutines used
c               wtinfo,wtferr
c Banashree Mitra Seifert (1996, Feb) 1.2.0:
c             . Corrected call for MVEXT for both obs and theo
c               RPSF file
c
c -----------------------------------------------------------------------
       character(5) version
       parameter (version = '1.2.0')
       character(10) subname
*-
c -----------------------------------------------------------------------
       subname = 'cqdp_rpsf'
       subinfo = 'using '//subname//' '//version
       call wtinfo(chatter,10,1,subinfo)
c
c ----------- OPENING PSF FILE -------------
c
          status = 0
          ninstr = 3
          instr(1) = 'RESPONSE'
          instr(2) = 'RPRF'
          instr(3) = 'TOTAL'
          extname = 'OBS RPSF'
          nsearch = 50

       call mvext (0, fits_psf, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, errflg, chatter)

       if (errflg .ne. 0) then
           data_pres = .false.
           call ftclos(iunit,status)
           subinfo = ' closing PSF file'
           call wtferr(subname,version,status,subinfo)
       else
           data_pres =.true.
       endif
c ----------- READING OBSERVED PSF EXTENSION -------------

       if(data_pres) then
          call rdrpf1(iunit,hduclas3,nrad,rad_lo,rad_hi,radunit,
     &                   ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     &                   energ_lo,energ_hi,energunit,cts,qerror,err,
     &                   rpsfunit,qarea,area_wgt,telescop,instrume,
     &                   maxrad,maxtheta,ierr,chatter)
          if (ierr.NE.0) then
              call ftclos(iunit,status)
              subinfo = ' closing obs_psf file'
              call wtferr(subname,version,status,subinfo)
              return
          endif            
c
c ------------------- READ PIXSIZE -------------------------
c
          status = 0
          call ftgkye(iunit,'PIXSIZE',pix_size,comm,status)
          errinfo = ' reading PIXSIZE !'
          call wtferr(subname,version,status,errinfo)
          pix_size = pix_size * 60
c ------------------- READ BACKGROUND --------------
          status = 0
          call ftgkye(iunit,'BACKGRND',backgrnd,comm,status)
          errinfo = errstr//' reading BACKGRND'
          call wtferr(subname,version,status,errinfo)
          if (status.NE.0) then
             errinfo = 'If rescaling theo curve, background required'
             call wtinfo(chatter,1,1,errinfo)
          endif
c ------------- Closing the input file -----------------------
          status = 0
          call ftclos(iunit,status)
          errinfo = ' closing'// fits_psf
          call wtferr(subname,version,status,errinfo)
       endif
c ----------------- closed the if data present statement

c --------------- READING THEORETICAL PSF EXTENSION --------
        errflg = 0
        ninstr = 3
        instr(1) ='RESPONSE'
        instr(2) = 'RPRF'
        instr(3) = 'PREDICTED'
        extname  ='THEO RPSF'
        nsearch = 50

        call mvext (0, pred_psf, iunit, ninstr, instr, nsearch,
     >              next, outhdu, extnames, outver, extname,
     >              errflg, chatter)

        if (errflg .ne. 0) then
            theo_pres=.false.
            status=0
            call ftclos(iunit,status)
            subinfo = ' closing PSF file'
            call wtferr(subname,version,status,subinfo)
            return
        else
            theo_pres = .true.
        endif

c --------------------- READ THEO DATA -------------------------------+
      if(theo_pres) then
         ierr = 0
         call rdrpf1(iunit,hduclas3,npred,prad_lo,prad_hi,radunit,
     &                   ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     &                   energ_lo,energ_hi,energunit,pred,qerror,perr,
     &                   rpsfunit,qarea,parea_wgt,telescop,instrume,
     &                   maxpred,maxtheta,ierr,chatter)    
         if (ierr.EQ.1) then
             theo_pres = .false.
             ierr = 0
         endif

c ------------------- READ PIXSIZE ------------------------------------+

         status = 0
         call ftgkye(iunit,'PIXSIZE',pix_size,comm,status)
         subinfo = ' reading PIXSIZE !'
         call wtferr(subname,version,status,subinfo)
         pix_size = pix_size *60

c ------------- READ THEORETICAL TOTAL PSF COUNTS ----------------------+

         status = 0
         call ftgkye(iunit,'SUMTCTS',tot_cts, comm,status)
         subinfo = ' reading SUMTCTS !'
         call wtferr(subname,version,status,subinfo)
         if (status .ne. 0) then
             tot_cts = 0
             errinfo = 'SUMTCTS is not found '
             call wtferr(subname,version,status,errinfo)
         endif

c ------------------- READ BACKGROUND FOR PSF DATA -------------------

         status = 0
         call ftgkye(iunit,'BACKGRND',backgrnd,comm,status)
         errinfo = ' reading BACKGRND'
         call wtferr(subname,version,status,errinfo)
         if (status .ne. 0) then
             backgrnd = 0
             errinfo = 'background is not supplied'
             call wtinfo(chatter,1,1,errinfo) 
         endif
         backgrnd = backgrnd * pix_size
         status = 0
       endif

c -------------------- CLOSING FILE ----------------------------------

       status = 0
       call ftclos(iunit,status)
       errinfo = ' closing '//pred_psf
       call wtferr(subname,version,status,errinfo)
  200  if ((.NOT.theo_pres).AND.(.NOT.data_pres)) then
         ierr = 5
         errinfo = ' predicted and observed data not present'
         call wtinfo(chatter,1,1,errinfo)
       endif 
       return
       end
c ----------------------------------------------------------------
c              end OF SUBROUTINE CQDP_RPSF 
c ----------------------------------------------------------------

*+CQDP_CONV
      subroutine cqdp_conv(rad_lo,rad_hi,prad_lo,prad_hi,rad_mean,
     &    del_rad,prad_mean,pdel_rad,maxrad,maxpred,nrad,
     &    npred,theo_pres,data_pres,ierr,chatter)
c
c --- DESCRIPTION --------------------------------------------------------
c
c Converts data read from FITS file into desired format for QDP file
c
c --- VARIABLES ----------------------------------------------------------
c
      IMPLICIT NONE
      integer maxrad,maxpred,nrad,npred,i,j,chatter,ierr
      character(40) subinfo
      real rad_lo(maxrad),rad_hi(maxrad),prad_lo(maxpred)
      real rad_mean(maxrad),del_rad(maxrad),prad_hi(maxpred)
      real prad_mean(maxpred),pdel_rad(maxpred)
      logical theo_pres,data_pres
c
c --- VARIABLE DIRECTORY ---
c
c maxrad     int    : Maximum array size for observed psf data
c maxpred    int    : Maximum array size for predicted psf data
c nrad       int    : Counter for observed psf data
c npred      int    : Counter for predicted psf data
c rad_lo     real   : Array of lower edge of observed radial bins
c rad_hi     real   : Array of upper edge of observed radial bins
c prad_lo    real   : Array of lower edge of predicted model bins
c prad_hi    real   : Array of upper edge of predicted model bins
c rad_mean   real   : Array of mean of observed radial bins
c del_rad    real   : Array of half-width of observed radial bins
c prad_mean  real   : Array of mean predicted model bins
c pdel_rad   real   : Array of half-width of observed radial bins
c
c --- AUTHORS/MODifICATION HISTORY ---
c
c Rehana Yusaf (Feb 1993)
c Rehana Yusaf (1994 Jan 31) 1.0.1; add data_pres
c
c Banashree Mitra Seifert (1996, Jan) 1.1.0:
c              . Screen display is used
c                wtinfo
c -------------------------------------------------------------------------
       character(5) version
       parameter (version ='1.1.0')
       character(10) subname
*-
c -------------- USER INFO --------------------------------------             

       subname = 'cqdp_conv'
       subinfo = 'using '//subname//' '//version
       call wtinfo(chatter,10,1,subinfo)

c     --- (OBSERVED) DATA MANIPULATION --

        if (data_pres) then
         do i=1,nrad
          rad_mean(i) = (rad_lo(i) + rad_hi(i))/float(2)
          del_rad(i) = (rad_hi(i) - rad_lo(i))/float(2)
         enddo
        endif
c
c      --- (THEORETICAL) DATA MANIPULATION --
c
        if (theo_pres) then
         do j=1,npred
            prad_mean(j) = (prad_lo(j) + prad_hi(j))/float(2)
            pdel_rad(j) = (prad_hi(j) - prad_lo(j))/float(2)
         enddo
        endif
        return
        end
c ---------------------------------------------------------------------
c              END OF SUBROUTINE CQDP_CONV 
c ---------------------------------------------------------------------

*+CQDP_WT
      subroutine cqdp_wt(fits_psf,qdp_psf,rad_mean,del_rad,cts,err,
     &    prad_mean,pdel_rad,pred,maxrad,maxpred,maxtheta,nrad,
     &    npred,theo_pres,data_pres,rescale,pix_size,backgrnd,
     &    rpsf_pres, reef_pres, ierr,chatter,killit)
c
c --- DESCRIPTION ----------------------------------------------------
c
c This routine writes to output file in QDP format
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character(180) qdp_psf,fits_psf
      integer maxrad,maxpred,maxtheta,nrad,npred,chatter,ierr
      real rad_mean(maxrad),del_rad(maxrad)
      real cts(maxrad,maxtheta,1),err(maxrad,maxtheta,1)
      real prad_mean(maxpred),pdel_rad(maxpred)
      real pred(maxpred,maxtheta,1)
      real rescale
      real pix_size,backgrnd
      logical theo_pres,data_pres,killit
      logical rpsf_pres, reef_pres
c
c --- INTERNALS ---
c
      character(40) subinfo
      character(80) header,desc1,desc2,desc3
      integer j
c
c --- VARIABLE DIRECTORY ---
c
c maxrad     int    : Maximum array size for observed psf data
c maxpred    int    : Maximum array size for predicted psf data
c nrad       int    : Counter for observed psf data
c npred      int    : Counter for predicted psf data
c cts        real   : Array of observed PSF in counts
c pred       real   : Array of theoretical PSF
c rad_mean   real   : Array of mean of observed radial bins
c del_rad    real   : Array of half-width of observed radial bins 
c prad_mean  real   : Array of mean predicted model bins
c pdel_rad   real   : Array of half-width predicted model bins
c rescale    real   : Scalar rescaling factor for pred ONLY
c chatter    int    : Chattiness flag (<5 quiet,>5 normal,>20 noisy)
c
c --- AUTHORS/MODifICATION HISTORY --->>>
c
c Rehana Yusaf (1993 Feb)
c Rehana Yusaf (1994 Jan 31) 1.0.1; add data_pres
c Rehana Yusaf (1994 Sept 12) 1.0.2; fix y-axis label
c Ian M George (1995 Apr 05) 2.0.0; added rescale to passed parameters
c Rehana Yusaf (1995 April 25) 2.0.1; add clobber
c Banashree Mitra Seifert(1995 December) 2.1.0; 
c           . modified to accomodate for REEF data, in which case,
c             label for y-axis is dimensionless
c           . added logical parameters rpsf_pres, reef_pres  
c           . Screen display used
c             wtinfo,
c --------------------------------------------------------------------- 
        character(5) version
        parameter (version = '2.1.0')
        character(8) subname
*-

c       --- USER INFORMATION ---

        subname='cqdp_wt'
        subinfo = 'using '//subname//' '//version 
        call wtinfo(chatter,10,1,subinfo)

c       --- OPENING OUTPUT FILE/WRITING HEADER ---

        call opasci(10,qdp_psf,2,80,killit,chatter,ierr)
        header = '! QDP FILE : '//qdp_psf
        write(10,100) header
        header = '! CONVERTED FROM FITS FILE : '//fits_psf        
        write(10,100) header
        desc1 = ' Read serr 1,2'
        desc2 = ' la x Radius (arcmin)'
        write(10,100) desc1
        write(10,100) desc2
        if (rpsf_pres) then
            desc3 = ' la y Counts per sq arcmin'
            write(10,100) desc3
        endif
        if (reef_pres) then
            desc3 = ' la y eef '
            write(10,100) desc3
        endif
        
c
c       <<<--- WRITING OBSERVED PSF --->>>
c
        if (data_pres) then
         header = '! ----------------------------'
         write (10,100) header
         header = '! Observed Radial Profile Data'
         write (10,100) header
         header = '! ----------------------------'
         write (10,100) header
         header='! Radial Mean, Delta Rad, Counts & Statistical Error :'
         write(10,100) header
         do j=1,nrad
          write (10,*) rad_mean(j),del_rad(j),cts(j,1,1),err(j,1,1)
         enddo
        endif
c
c       <<<--- WRITING THEORETICAL PSF --->>>
c
        if (theo_pres) then
          header = '! -------------------------------'
          write (10,100) header
          header = '! Theoretical Radial Profile Data'
          write (10,100) header
          header = '! -------------------------------'
          write (10,100) header
          write (10,*)' no no no no'
	  if(rescale.NE.1.0) then
            write(header,'(a,g12.5)') 
     &		'rescaling predicted curve by factor: ',rescale
            call wtinfo(header)
	    write(header,'(a,g12.5,a)')
     &		'! Theoretical PSF curve rescaled by factor: ',
     &		rescale, ' (at request of user)'
	    write (10,100) header
	  endif
          header = '! Radial Mean, Delta Rad, Theoretical PSF :'
          write (10,100) header
          do j=1,npred

c            write (10,*) prad_mean(j),pdel_rad(j),
c     &		pred(j,1,1)*rescale,' 0.0'
             if (backgrnd.NE.0.0) then
               pred(j,1,1) = pred(j,1,1) - backgrnd/pix_size**2
             endif
             pred(j,1,1) = pred(j,1,1) * rescale
             if (backgrnd.NE.0.0) then
               pred(j,1,1) = pred(j,1,1) + backgrnd/pix_size**2
             endif 
             write (10,*) prad_mean(j),pdel_rad(j),
     &         pred(j,1,1),' 0.0'
          enddo
        endif
        write(10,*)'log y'
        close(unit=10)
  100   format(A80)
        return
        end
c
c       <<<--- end OF SUBROUTINE CQDP_WT --->>>
c


*+WTCOLS
      subroutine wtcols(ncols,columns,maxcol,chatter)

c --- DESCRIPTION -------------------------------------------------
c
c This routine prints out a character array of column header names,
c using fcecho.
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character(100) subinfo
      character(80) info
      integer ncols,maxcol,i, chatter
      character(8) curcol,columns(maxcol)
c
c --- VARIABLE DIRECTORY ---
c
c info       char   : Comment string
c ncols      int    : No. of Columns
c maxcol     int    : Array dimension
c columns    char   : Array containing column names
c curcol     char   : current column name
c
c --- CALLED ROUTINES ---
c
c subroutine FCECHO : FTOOLS library routine to write to screen
c
c --- LINKING AND COMPILATION ---
c
c Link with FTOOLS 
c
c --- AUTHORS/MODIFICATIONS ---
c
c Rehana Yusaf ( Feb 1993) 1.0.0:
c
c Banashree Mitra Seifert (Jan 1996) 1.1.0:
c              . Introduced screen display subroutine
c                wtinfo 
c --------------------------------------------------------------------
      character(5) version
      parameter (version='1.1.0')
      character(7) subname
*-
      subname ='wtcols'
      subinfo='using'//subname//version
      call wtinfo(chatter,10,1,subinfo)
c
c     <<<------>>>
c
      info = 'The Following Columns are present in the input file :'
      call wtinfo(chatter,10,1,info)
      do i=1,ncols
        curcol = columns (i)
        call wtinfo(chatter,10,1,info)
      enddo
      return
      end
c ---------------------------------------------------------------------+
c            END OF SUBROUTINE WTCOLS 
c ---------------------------------------------------------------------+

*+RPSF_DECIDE
c ---------------------------------------------------------------------+
      subroutine decide(infile, nset, hdu_sav, chatter)   

c --------------- DESCRIPTION OF RPSF_DECIDE --------------------------+
c
c This subroutines reads the input file to be converted to QDP format
c and then determines which format is it in (that is, the RPSF format 
c or the REEF format.
c
c --------------- ROUTINES CALLED -------------------------------------+
c MVEXT  --> opens input file and moves to the desired extension either
c            by EXTNUM or by HDUCLAS/EXTNAME
c
c --------------- DECLARE VARIABLES -----------------------------------+

      implicit none 
      character(180) infile
      character(20) hdu_sav(9,50)
      integer nset, chatter

c --------------- INTERNAL VARIABLES ----------------------------------+

      integer iunit,ninstr,nsearch, status, next(50), errflg
      character(20) extname
      character(20) instr(50), outhdu(9,50),extnames(50),outver(9,50)
      character(120) subinfo
    
c --------------- AUTHORS/MODifICATIONS ------------------------------+
c 
c Banashree Mitra Seifert (Nov. 24, 1995)
c
c --------------------------------------------------------------------+
       character(5) version
       parameter (version='1.0.0')
       character(12) subname 
c --------------------------------------------------------------------+
*-  
      subname = 'rpsf_decide'
      subinfo ='using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)
  
      status   = 0
      ninstr   = 2    
      instr(1) = 'RESPONSE'
      instr(2) = 'RPRF'
      nsearch = 50 

      call mvext(0, infile, iunit, ninstr, instr, nsearch, next,
     >           outhdu, extnames, outver, extname, errflg, chatter)

      if ((errflg .eq. 0) .or. (errflg .eq. 2)) then
          subinfo='has psf extension'
          call wtinfo(chatter,20,4,subinfo)
          nset = nset +1
          hdu_sav(nset,1) = outhdu(1,1)
          hdu_sav(nset,2) = outhdu(2,1)
      else
          subinfo='does not have psf extension'
          call wtinfo(chatter,20,3,subinfo)
          call ftclos(iunit,status)
          subinfo = ' closing PSF file'
          call wtferr(subname,version,status,subinfo)
      endif
      
      status   = 0
      ninstr   = 2 
      instr(1) = 'RESPONSE'
      instr(2) = 'REEF'
      nsearch = 50 

      call mvext(0, infile, iunit, ninstr, instr, nsearch, next,
     >           outhdu, extnames, outver, extname, errflg, chatter)

      if ((errflg .eq. 0) .or. (errflg .eq. 2)) then
          subinfo='has reef extension'
          call wtinfo(chatter,1,4,subinfo)
          nset = nset +1
          hdu_sav(nset,1) = outhdu(1,1)
          hdu_sav(nset,2) = outhdu(2,1)
      else
          subinfo='does not have reef extension'
          call wtinfo(chatter,20,3,subinfo)
          call ftclos(iunit,status)
          subinfo = ' closing REEF file'
          call wtferr(subname,version,status,subinfo)
      endif
      
      return
      end
c ----------------------------------------------------------------------
c                             END OF DECIDE 
c ----------------------------------------------------------------------
 
*+CQDP_REEF
      subroutine cqdp_reef(fits_psf,pred_psf,rad_lo,rad_hi,reef,
     >                     reef_err,prad_lo,prad_hi,pred_reef,maxrad,
     >                     maxpred, maxtheta,nrad,npred,theo_pres,
     >                     data_pres,pix_size,backgrnd,ierr,chatter)
c
c -------------------------- DESCRIPTION --------------------------
c
c Reading REEF FITS file using FITSIO.
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character(180) fits_psf,pred_psf
      integer maxrad,maxpred,nrad,npred,chatter,ierr,maxtheta
      real rad_lo(maxrad),rad_hi(maxrad)
      real reef(maxrad,maxtheta,*),reef_err(maxrad,maxtheta,*)
      real prad_lo(maxpred),prad_hi(maxpred)
      real pred_reef(maxpred,maxtheta,*)
      logical theo_pres,data_pres
c
c ---------------------- INTERNAL VARIABLES --------------------------
c
      character(40) comm
      character(80) subinfo,errinfo
      character(20) instr(50),outhdu(9,50),outver(9,50),extname(50)
      character(20) extnames(9,50)
      character(8) telescop,instrume
      real pix_size,backgrnd
      integer status,iunit
      character(16) radunit,thetaunit,energunit,reefunit
      character(16) hduclas3
      integer ntheta,nenerg
      real theta_lo(1),theta_hi(1),energ_lo(1),energ_hi(1)
      real perr(9092,1,1),parea_wgt(9092,1,1),area_wgt(300,1,1)
      logical qerror,qarea
      integer nsearch,ninstr,next(50)
      integer errflg
c
c --- VARIABLE DIRECTORY ---
c
c Arguments ...
c 
c fits_psf   char   : Name of FITS format EEF file
c maxen      int    : Maximum array size for observed eef data
c maxpred    int    : Maximum array size for predicted eef data
c nrad       int    : Counter for observed eef data
c npred      int    : Counter for predicted eef data
c rad_lo     real   : Array of lower edge of observed radial bins
c rad_hi     real   : Array of upper edge of observed radial bins
c cts        real   : Array of observed radial profile, in counts
c err        real   : Array of statistical errors
c prad_lo    real   : Array of lower edge of predicted model bins
c prad_hi    real   : Array of upper edge of predicted model bins
c pred       real   : Array of theoretical EEF
c
c Internals ...
c
c errmess    char   : Error message text
c errstr     char   : Error text for this routine
c version    char   : Subroutine version
c status     int    : Error flag for FITSIO call
c iunit      int    : Fortran unit number for file
c
c --- CALLED ROUTINES ---
c 
c subroutine FTOPEN       : FITSIO routine to open file
c subroutine FTCLOS       : FITSIO routine to close file
c subroutine MVEXT        : moves to desired extension
c subroutine RDEEF1       : Reads observed EEF extension in FITS file
c subroutine WT_FERRMSG   : Writes FITSIO error text if neccesary
c
c ---------------- AUTHORS/MODifICATION HISTORY -----------------------
c
c Banashree Mitra Seifert (December 1995)
c
c -----------------------------------------------------------------------
       character(5) version
       parameter (version = '1.0.0')
       character(10) subname
*-
c -----------------------------------------------------------------------
       subname = 'cqdp_reef'
       subinfo = 'using '//subname//' '//version
       call wtinfo(chatter,10,1,subinfo)

c ----------- OPENING PSF FILE -------------
c
          status = 0
          ninstr = 3
          instr(1) = 'RESPONSE'
          instr(2) = 'REEF'
          instr(3) = 'OBSERVED'
          nsearch = 50

       call mvext (0, fits_psf, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, errflg, chatter)
       if (errflg .ne. 0) then
           data_pres = .false.
           call ftclos(iunit,status)
           subinfo = ' closing EEF file'
           call wtferr(subname, version,status,subinfo)
       else
           data_pres =.true.
       endif
c ----------- READING OBSERVED PSF EXTENSION -------------

       if(data_pres) then

       call rdeef1(iunit,hduclas3,nrad,rad_lo,rad_hi,radunit,
     >                   ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     >                   energ_lo,energ_hi,energunit,reef,qerror,
     >                   reef_err,
     >                   reefunit,qarea,area_wgt,telescop,instrume,
     >                   maxrad,maxtheta,ierr,chatter)

       if (ierr.NE.0) then
           call ftclos(iunit,status)
           subinfo = ' closing obs_eef file'
           call wtferr(subname, version,status,subinfo)
           return
       endif            
c
c ------------------- READ PIXSIZE -------------------------
c
       if(pix_size .ne. 0.) then
          status = 0
          call ftgkye(iunit,'PIXSIZE',pix_size,comm,status)
          errinfo = ' reading PIXSIZE !'
           call wtferr(subname, version,status,errinfo)
          pix_size = pix_size * 60
       endif
c ------------------- READ BACKGROUND --------------
       if(backgrnd .ne. 0.0) then
          status = 0
          call ftgkye(iunit,'BACKGRND',backgrnd,comm,status)
          errinfo = ' reading BACKGRND'
           call wtferr(subname, version,status,errinfo)
          if (status.NE.0) then
            errinfo='If rescaling theo curve, background required'
            call wtferr(subname, version,status,errinfo)
          endif
       endif
c ------------- Closing the input file -----------------------
       status = 0
       call ftclos(iunit,status)
       errinfo = ' closing '//fits_psf
           call wtferr(subname, version,status,errinfo)
       endif
c ----------------- closed the if data present statement
c --------------- READING THEORETICAL PSF EXTENSION --------
  100   ierr = 0

        ninstr = 3
        instr(1) ='RESPONSE'
        instr(2) = 'REEF'
        instr(3) = 'PREDICTED'
        nsearch = 50

      call mvext (0, pred_psf, iunit, ninstr, instr, nsearch,
     >             next, outhdu, extnames, outver, extname,
     >             errflg, chatter)
       if (errflg .ne. 0) then
           theo_pres=.false.
           call ftclos(iunit,status)
           subinfo = ' closing EEF file'
           call wtferr(subname,version,status,subinfo)
           return
       endif

c --------------------- READ THEO DATA -------------------------------+
       theo_pres = .true.
       status = 0

       call rdeef1(iunit,hduclas3,npred,prad_lo,prad_hi,radunit,
     >                   ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     >                   energ_lo,energ_hi,energunit,pred_reef,
     >                   qerror,perr,
     >                   reefunit,qarea,parea_wgt,telescop,instrume,
     >                   maxpred,maxtheta,ierr,chatter)    

       if (ierr.EQ.1) then
         theo_pres = .false.
         ierr = 0
       endif

c      <<<--- CLOSING FILE --->>>

       status = 0
       call ftclos(iunit,status)
       errinfo = ' closing '//pred_psf
       call wtferr(subname,version,status,errinfo)
  200  if ((.NOT.theo_pres).AND.(.NOT.data_pres)) then
         ierr = 5
         errinfo = ' predicted and observed data not present'
         call wtinfo(chatter,1,1,errinfo)
       endif 
       return
       end
c ----------------------------------------------------------------
c              end OF SUBROUTINE CQDP_REEF 
c ----------------------------------------------------------------
 
