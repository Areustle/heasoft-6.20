
*+RBNRPSF
        subroutine rbnrpf 

c --------------------- DESCRIPTION --------------------------------
c
c Main task routine for rebinning OGIP standard RPSF file with a specified
c amount of minimum counts in each bin
c Input : infile (RPSF format)
c         outfile
c         cmin (Minimum counts per pixel)
c         bkgd (% default, any other value overides this) If % bkgd in
c         infile is used.
c 
c --- VARIABLES -------------------------------------------------------

        IMPLICIT NONE
        character(180) infile,outfile, subinfo
        character(8) telrad,instrad
        character(16) radunit,thetaunit,energunit,rpsfunit
        integer npts,chatter, ierr
        integer maxrad,maxtheta,maxen,chanmin,chanmax
        real pix_size,c_min,bkgd,bkgd_rad, sumtcts, sumrcts
        integer p_rad_lo, p_rad_hi, p_cts_arc2, p_err_arc2
        integer p_area_wgt, p_energ_lo, p_energ_hi
        integer p_theta_lo, p_theta_hi
        integer ntheta,nenerg 
        integer iget, ineed, status
        logical qerror,qarea,calc,lbin,negpres,rpsf_bkgd,killit

c -------------------- VARIABLE DIRECTORY ------------------------
c
c infile     char   : Name of radial profile file (input) 
c outfile    char   : Name of results file (output)
c ierr       int    : Error flag, ierr = 0 okay
c telrad     char   : Telescope name for radial data (from infile)
c instrad    char   : Instrument name for radial data (from infile)
c maxrad     int    : Maximum size of arrays used for radial profile data
c maxen      int    : Maximum size of arrays used for pha data
c pix_size   real   : pixel size (from rpsf infile), converted to arcmins/pixel
c c_min      real   : Minimum No. counts/bin (user defined)
c bkgd       real   : Background count rate in counts/pixel (user defined)
c rad_lo     real   : Array of lower edge of radial bins ( in arcmins)
c rad_hi     real   : Array of upper edge of radial bins (in arcmins)
c theta_lo   real   : Array of theta low values of the observation
c theta_hi   real   : Array of theta high values of the observation
c energ_lo   real   : Array of energy low values of the observation
c energ_hi   real   : Array of energy high values of the observation
c ntheta     int    : No of theta values
c nenerg     int    : No of energy values
c qerror     logical: True if errors present in rad profile file
c qarea      logical: True if Area_weighting factors present  
c area_wgt   real   : Array of area weighting factors
c cts_arc2   real   : Array of radial profile in counts/arcmin^2
c err_arc2   real   : Array of errors on cts_arc2
c npts       int    : No. of observed psf data points
c lbin       logical: true if last bin has less than c_min counts
c negpres    logical: true if negative counts present in dataset
c
c ---------------- COMPILATION AND LINKING ----------------
c
c Link with XPI and with FITSIO
c
c ---------------- CALLED ROUTINES ----------------
c
c subroutine CPSF_GP       : Obtains user defined input
c subroutine CPSF_RINFILE  : Reads data from radial profile
c subroutine CPSF_RESULTS  : Writes rebinned Observed data
c
c ---------------- AUTHORS/MODIFICATION HISTORY ----------------
c 
c Rehana Yusaf (1993 January 15) : Modularisation and minor modifications
c Rehana Yusaf (1993 Febuary 24) : Changing code to read FITS format radial
c                                  profile input
c
c Rehana Yusaf (1993 March 3)    : Minor changes, more info in output
c Rehana Yusaf (1993 May 26)     : Minor changes, removing stops etc
c Rehana Yusaf (1993 June 8)     : Minor changes, lbin and negpres added
c
c Rehana Yusaf (1993 June 9) 1.0.1: Substitute off-axis function for on-axis
c Rehana Yusaf (1993 July 27) 1.0.2; Minor change to Off-Axis algorithm,
c                                    (comment in function)   
c Rehana Yusaf (1993 August 2) 1.0.3; Minor change to OFF-AXIS alg'
c                                    (comment in function)
c Rehana Yusaf (1993 Sept 13 1993) 1.0.4; RPSFVER 1993a format is used, 
c                                  instead of 1992a this task is changed
c                                  accordingly.        
c Rehana Yusaf (1994 Jan 20) 1.1.0; Stripped out code from PSPCRPSF as a 
c                                  basis for this 'new' task that only
c                                  rebins the data, PSPCRPSF now takes as
c                                  input the rebinned RPSF file 
c Rehana Yusaf (1994 Feb 3) 1.1.1; rpsf_bkgd is used correctly - that is
c				   if a user-defined bkgd is entered then
c                                  use that value NOT the value read from
c                                  file 
c Rehana Yusaf (1995 Jan 13) 1.1.2; update _gp so that defval is no longer
c                                   read from parfile
c Rehana Yusaf (1995 Feb 28) 1.1.3; Add more diagnostics in _rebin
c                                   bkgd can be calculated
c Rehana Yusaf (1995 April 25) 1.1.4; add clobber and update to _rebin
c
c Banashree Mitra Seifert (1996, Jan) 2.0.0: Modifications made
c                      . Introduced DMA
c                      . Replaced by call to MVEXT
c                      . Replaced filenames by character(180) 
c                      . maxtheta=1, maxen=1
c                      . Introduced screen display routines
c                        wtinfo,wtbegm,wtendm,wtferr
c
c Banashree Mitra Seifert (1996, Feb) 2.1.0: 
c                      . modification done in search for PSF file.
c                        earlier version (2.0.0) didn't look for 
c                        instr(3) = TOTAL. Instead it looked for 
c                        only instr(3) = NET.
c                      . introduced EXTNAME before call to MVEXT
c
c Banashree Mitra Seifert (1997, Oct 10) 2.2.0:
c     . In the subroutine cpsf_rebin,
c          The calculation of area_wgt was wrong. binning was ok, but 
c          while running pcrpsf/hrirpsf after binning, resulted in a 
c          mismatch between the psf model normalization and the data 
c          further down the data-analysis pipeline. This error happened 
c          when binning requires counts from more than one bin from 
c          unbinned input data. So replaced 
c              area_wgt(j,1,1) = sumarea/area
c          by          
c              area_wgt(j,1,1) = sumarea/k
c
c Peter D Wilson (1998 Jul 01) 2.2.1:
c     . Updated for new FCPARS behavior
c ------------------------------------------------------------------------
        character(5) version
        parameter (version = '2.2.1')
        character(8) subname
*- 
c ------------- dynamic memory allocated array -------------------------
c
c       real rad_lo(maxrad),rad_hi(maxrad),cts_arc2(maxrad,maxtheta,maxen)
c       real err_arc2(maxrad,maxtheta,maxen)
c       real area_wgt(maxrad,maxtheta,maxen)
c       real energ_lo(maxen),energ_hi(maxen)
c       real theta_lo(maxtheta),theta_hi(maxtheta)
c
c ---------------- DYNAMIC MEMORY ALLOCATION --------------------------
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

        character(40) taskname
ccc        COMMON /task/taskname
        subname  = 'rbnrpsf'
        taskname = 'rbnrpsf'
        subinfo='using'//subname//version
        call wtinfo(chatter,10,1,subinfo)
c -----------------------------------------------------------------------

        ierr = 0
        rpsf_bkgd = .false.
        call cpsf_gp(infile,outfile, c_min,bkgd,rpsf_bkgd,calc,
     >               bkgd_rad,ierr,chatter,killit)

        call wtbegm(taskname,version,chatter)

        IF (ierr.NE.0) THEN
            go to 100
        ENDIF

        maxrad  =1000
        maxtheta=5 
        maxen   =50
        
c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------
      iget=0
      status = 0

      p_rad_lo = 0
      p_rad_hi = 0 
      p_cts_arc2 = 0
      p_err_arc2 = 0
      p_area_wgt = 0
      p_energ_lo = 0
      p_energ_hi = 0
      p_theta_lo = 0
      p_theta_hi = 0

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
      call udmget(maxrad*maxtheta*maxen, 6, p_cts_arc2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxen*4

      status = 0
      call udmget(maxrad*maxtheta*maxen, 6, p_err_arc2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxen*4

      status = 0
      call udmget(maxrad*maxtheta*maxen, 6, p_area_wgt, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*maxen*4

      status = 0
      call udmget(maxen, 6, p_energ_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxen*4

      status = 0
      call udmget(maxen, 6, p_energ_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxen*4
  
      status = 0
      if(maxtheta .lt. 50)maxtheta=50
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
  
  50   ineed = 2*maxrad*4 + 3*maxrad*5*maxen*4 +
     >         2*maxtheta*4 + 2*maxen*4 
      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
      call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory obtained=', iget
      call wtinfo(chatter,10,1,subinfo)

      if (status .ne. 0) then
         ierr = -1
         subinfo='failed to allocate dynamic memory '
         call wtferr(subname,version,status,subinfo)
         goto 100
      endif
c ---------------------------------------------------------------------+
c                    end of DMA allocation
c ---------------------------------------------------------------------+

        call cpsf_rinfile(infile, pix_size, c_min, npts, MEMR(p_rad_lo),
     >                    MEMR(p_rad_hi), radunit, ntheta,
     >                    MEMR(p_theta_lo), MEMR(p_theta_hi), thetaunit,
     >                    nenerg, MEMR(p_energ_lo), MEMR(p_energ_hi), 
     >                    energunit, MEMR(p_cts_arc2), qerror, 
     >                    MEMR(p_err_arc2), rpsfunit, qarea, 
     >                    MEMR(p_area_wgt), maxrad,
     >                    maxtheta, telrad, instrad, negpres, lbin,
     >                    rpsf_bkgd, bkgd, chanmin, chanmax, calc, 
     >                    bkgd_rad, sumrcts, sumtcts, ierr, chatter)

        IF (ierr.NE.0) THEN
            goto 100
        ENDIF

c ------------ calling cpsf_results -----------------------------------+

        call cpsf_results(outfile, infile, version, npts, 
     >                    MEMR(p_rad_lo), MEMR(p_rad_hi), radunit, 
     >                    ntheta, MEMR(p_theta_lo), MEMR(p_theta_hi),
     >                    thetaunit, nenerg, MEMR(p_energ_lo), 
     >                    MEMR(p_energ_hi), energunit, MEMR(p_cts_arc2),
     >                    qerror, MEMR(p_err_arc2), rpsfunit, qarea,
     >                    MEMR(p_area_wgt), pix_size, maxrad, maxtheta, 
     >                    bkgd, c_min, telrad, instrad, negpres, lbin, 
     >                    chanmin, chanmax, sumrcts, sumtcts, 
     >                    ierr, chatter, killit)

      if (ierr .ne. 0) then
          goto 100
      endif

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_rad_lo, 6, status)
      status = 0
      call udmfre(p_rad_hi, 6, status)
      status = 0
      call udmfre(p_cts_arc2, 6, status)
      status = 0
      call udmfre(p_err_arc2, 6, status)
      status = 0
      call udmfre(p_energ_lo, 6, status)
      status = 0
      call udmfre(p_energ_hi, 6, status)
      status = 0
      call udmfre(p_theta_lo, 6, status)
      status = 0
      call udmfre(p_theta_hi, 6, status)
      status = 0
      call udmfre(p_area_wgt, 6, status)

      if (status .ne. 0) then
          subinfo= ' failed to de-allocate memory '
          call wtferr(subname,version,status,subinfo)
          ierr=99
      endif

100   call wtendm(taskname,version,ierr,chatter)
      return
      end
c --------------------------------------------------------------------
c              END OF RBNRPSF
c --------------------------------------------------------------------

*+CPSF_GP
        subroutine cpsf_gp(infile,outfile,c_min,bkgd,rpsf_bkgd,calc,
     >                     bkgd_rad,errflg,chatter,killit)
c --- DESCRIPTION ----------------------------------------------------
c
c This routine obtains user defined filenames and parameters.
c
c ---------------------- VARIABLES -----------------------------------

        IMPLICIT NONE
        character*(*) infile,outfile
        character(180) strbkgd, ill_files(5)
        real c_min,bkgd,bkgd_rad
        integer chatter,errflg,n_ill
        logical rpsf_bkgd,calc,killit,valfil

c ------------------ INTERNAL VARIABLES ------------------------------

        character(180) filename
        character(100) subinfo
        character(80) extinfo
        integer ierr, extnum, try
        logical ext

c -------------------- VARIABLE DIRECTORY ----------------------------
c
c infile     char   : Name of Radial profile file 
c outfile    char   : Name of results file
c c_min      real   : Minimum No. counts/bin
c bkgd       real   : Background count rate in counts/pixel
c chatter    int    : Chattines flag (<5 quiet,>5normal,>20 noisy)
c ierr       int    : Error flag
c errflg     int    : Error flag which returns to main, 0 ok
c errstr     char   : Error string
c
c ----------------- COMPILATION AND LINKING -------------------------
c
c Link with FTOOLS 
c
c ----------------- AUTHORS/MODIFICATION HISTORY -----------------
c
c Rehana Yusaf (1993 January)
c Rehana Yusaf (1993 Febuary):Change code to use fcecho for screen output
c Rehana Yusaf (1993 June 9) : Temporary measure, off_axis angle is 
c                              prompted for.
c Rehana Yusaf (1993 Sept 23) : Off Axis angle only prompted for if
c                               detector file not entered
c Rehana Yusaf (1994 Jan 20) 1.0.3; Adapt code for reading parameters
c                               for RBNRPSF (previously used for PSPCRPSF)
c Rehana Yusaf (1995 Jan 13) 1.0.4; defval no longer read from parfile
c Rehana Yusaf (1995 Feb 28) 1.0.5; bkgd can be calculated
c Rehana Yusaf (1995 April 25) 1.0.6; read in clobber
c Banashree Mitra Seifert (1996, Jan) 1.1.0; 
c              . Redefined the filenames to be character(180)
c              . Dimensions are carried from the calling subroutine
c              . Introduced screen display routines
c                wtinfo,wterrm
c Peter D Wilson (1998 Jul 01) 1.1.1:
c              . Drop INQUIRE test
c -----------------------------------------------------------------
        character(5) version
        parameter (version = '1.1.1')
        character(12) subname
*-
c ----------------- OBTAINING I/O FILENAMES -----------------------

        subname = 'cpsf_getpar '  
        subinfo = 'using'//subname//version
        call wtinfo(chatter,10,1,subinfo)
c -------------------------------------------------------------------

        call uclgst('infile',infile,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting infile parameter '
          call wterrm(subname,version,extinfo)
        ENDIF
        call crmvlbk(infile)
        IF (infile.EQ.'  ') THEN
          extinfo = 'infile must be entered !'
          call wterrm(subname,version,extinfo)
          errflg = 1
          return
        ENDIF
C PDW 7/1/98: Don't bother! Let FTOPEN determine if file exists
C        call fcpars(infile,filename,extnum,ierr)
C        INQUIRE(FILE=filename,EXIST=ext)
C        IF (.NOT.ext) THEN
C          extinfo = 'infile does not exist : '//filename
C          call wterrm(subname,version,extinfo)
C          errflg = 1
C          return
C        ENDIF

c ---------------- GET OUTFILE NAME ----------------------------

        call uclgst('outfile',outfile,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting outfile parameter' 
          call wterrm(subname,version,extinfo)
          errflg = 1
          return
        ENDIF
        call crmvlbk(outfile) 
        IF (outfile.EQ.'  ') THEN
          extinfo='must enter outfile name !!'
          call wterrm(subname,version,extinfo)
          errflg=1
          return
        ENDIF

c CLOBBER PARAMETER

      ierr = 0
      call uclgsb('clobber',killit,ierr)
      IF (ierr.NE.0) THEN
          extinfo = 'getting killit parameter !'
          call wterrm(subname,version,extinfo)
          errflg = 1
          return
      ENDIF

      n_ill = 0
      call ck_file(outfile,ill_files,n_ill,valfil,killit,chatter)
      IF (.NOT.valfil) THEN
          errflg = 2
          return
      ENDIF

c ------------ OBTAINING USER DEFINED PARAMETERS ----------------------+

        call uclgsi('chatter',chatter,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'getting chatter parameter'
            call wterrm(subname,version,extinfo)
        ENDIF  

        call uclgsr('c_min',c_min,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting minimum no. counts/bin,c_min parameter'
          call wterrm(subname,version,extinfo)
        ENDIF  

        calc = .false.
        try = 0
  90    call uclgst('bkgd',strbkgd,ierr)
        try = try + 1
        IF (ierr.NE.0) THEN
            extinfo = 'getting background count rate, bkgd parameter'
            call wterrm(subname,version,extinfo)
        ENDIF
        ierr=0
        call uclpst('bkgd','%',ierr)
        IF (ierr.NE.0) THEN
            extinfo = ' putting default bkgd parameter'
            call wterrm(subname,version,extinfo)
        ENDIF

        call crmvlbk(strbkgd)
        call ftupch(strbkgd)
        IF (strbkgd(1:1).EQ.'%') THEN
          rpsf_bkgd = .true.
        ELSEIF (strbkgd(1:1).EQ.'C') THEN
          calc = .true.
          call uclgsr('bkgd_rad',bkgd_rad,ierr)
          IF (ierr.NE.0) THEN
              extinfo = 'getting background radius parameter'
              call wterrm(subname,version,extinfo)
          ENDIF
        ELSE
          read(strbkgd,*,IOSTAT=ierr) bkgd
          If (ierr.NE.0) THEN
              extinfo = ' error in bkgd value '
              call wterrm(subname,version,extinfo)
              errflg = 1
              return
          ENDIF
          rpsf_bkgd = .false.
          IF (bkgd.LT.0) THEN
            IF (try.LT.3) THEN
                extinfo='negative bkgd entered, try again'
                call wterrm(subname,version,extinfo)
                goto 90
            ELSE
                errflg = 1
                return
            ENDIF
          ENDIF
        ENDIF

        return
        end 		
c --------------------------------------------------------------------
c              END OF SUBROUTINE CPSF_GP                 
c --------------------------------------------------------------------

*+CPSF_RINFILE
c ---------------------------------------------------------------------+
       subroutine cpsf_rinfile(infile,pix_size,c_min,npts,
     >                         rad_lo,rad_hi,radunit,ntheta,theta_lo,
     >                         theta_hi,thetaunit,nenerg,energ_lo,
     >                         energ_hi,energunit,cts_arc2,qerror,
     >                         err_arc2,rpsfunit,qarea,area_wgt,maxrad,
     >                         maxtheta,telrad,instrad,negpres,lbin,
     >                         rpsf_bkgd,bkgd,chanmin,chanmax,calc,
     >                         bkgd_rad,sumrcts,sumtcts,ierr,chatter)
c --- DESCRIPTION ---------------------------------------------------------
c
c Reads in Radial profile data from user defined input file. The input file
c is in FITS format. 
c  
c --- VARIABLES -----------------------------------------------------------

        IMPLICIT NONE
        character*(*) infile
        character*(*) telrad,instrad
        character(16) radunit,thetaunit,energunit,rpsfunit
        integer maxrad,ierr,maxtheta
        integer npts,chatter,ntheta,nenerg,chanmin,chanmax
        real pix_size,c_min,sumrcts,sumtcts,bkgd,bkgd_rad
        real rad_lo(*),rad_hi(*),cts_arc2(maxrad,maxtheta,1)
        real err_arc2(maxrad,maxtheta,1),area_wgt(maxrad,maxtheta,1)
        real energ_lo(*),energ_hi(*),theta_lo(*), theta_hi(*)
        logical negpres,lbin,qerror,qarea,rpsf_bkgd,calc

c ----------------- INTERNAL VARIABLES ----------------------------------

        character(180) subinfo,errinfo
        character(40)  comm
        character(20) instr(4),outhdu(9,50),extnames(50),outver(9,50)
        character(20) extname
        character(16) hduclas3
        integer maxin,iunit,status,ninstr,nsearch,next(50)
        integer extnum
        parameter (maxin = 1000)
        real incts(maxin,1,1),inrad_lo(maxin),inrad_hi(maxin)
        real inerr(maxin,1,1),pix

c ----------------- VARIABLE DIRECTORY --------------------------------
c
c Arguments ...
c
c infile     char   : Name of radial profile data file (user defined)
c pix        real   : Pixelsize in degrees (from infile)
c pix_size   real   : pixel size in arcmins per pixel
c c_min      real   : Minimum No. counts/bin (user defined)
c maxrad     int    : Maximum size of arrays for radial profile data
c rad_lo     real   : Array of lower edge of radial bins ( in arcmins)
c rad_hi     real   : Array of upper edge of radial bins (in arcmins)
c theta_lo   real   : Array of lower theta values
c theta_hi   real   ; Array of upper theta values
c energ_lo   real   : Lower Energy bounds
c energ_hi   real   : Upper Energy bounds
c area_wgt   real   : Array of area weighting factors
c cts_arc2   real   : Array of radial profile in counts/arcmin^2
c err_arc2   real   : Array of errors on cts_arc2
c npts       int    : Counter for No. of psf data points
c telrad     char   : Telescope name
c instrad    char   : Instrument name
c negpres    logical: true if negative counts present in data
c lbin       logical: true if last bin < c_min counts
c
c 
c maxin      int    : Array dimensions, this should be same as maxrad
c incts      real   : Array of radial profile in counts/arcmin^2,
c                     used to read from file
c inerr      int    : Array of errors on incts, used to read from file
c inrad_lo   real   : Array of minimum radial profile in arcmin
c                     used to read from file
c inrad_hi   real   : Array of max radial profile in arcmin
c subinfo    char   : User information 
c errstr     char   : Error string for this routine
c iunit      int    : Fortran i/o unit
c data_pres  logical: true if observational data is present
c theo_pres  logical: true if predicted data is present
c
c ---------------------- CALLED ROUTINES ----------------------------
c
c subroutine RD_RPSF1993a : (CALLIB) Reads OGIP FITS format radial profile
c subroutine CPSF_REBIN   : Rebins radial profile data if necessary. That
c                           is, each bin is checked ,and if it is less than
c                           the Minimum No. counts/bin defined by the user 
c                           then the data is rebinned.
c 
c -------------------- AUTHORS/MODIFICATION HISTORY --------------------
c 
c Rehana Yusaf (1993 January)
c Rehana Yusaf (1993 Febuary) : Change code to read FITS format RPSF data
c Rehana Yusaf (1993 Sept 13) 1.0.1; Change code to read FITS RPSFVER 1993a
c                              (previous read was 1992a)
c Rehana Yusaf (1994 Jan 20) 1.0.2; RD_RPSF1993a has been renamed RDRPF1,
c                              also bkgd is read from RPSF file if the user
c                              does not define a value. bkgd is passed to the
c                              cpsf_rebin routine.
c Rehana Yusaf (1995 Mar 7) 1.0.3; Calculate SUMRCTS and SUMTCTS in _rebin
c Banashree Mitra Seifert (December 1995) 2.0.0: modifications are
c                  . In the variable declaration so that the dimensions 
c                    are carried over from the parent subroutine.
c                  . Replaced by call to MVEXT
c                  . Modified some error messages
c                  . Introduced screen display routines
c                    wtinfo,wtferr,wterrm
c --------------------------------------------------------------------------

        character(5) version
        parameter (version = '2.0.0')
        character(14)  subname
        integer rwmode 
*-  
c -------------------- USER INFO ------------------------------

        subname = 'cpsf_rinfile'
        subinfo = 'using cpsf_rinfile Ver '//version
        call wtinfo(chatter,10,1,subinfo)

c ---------------------- FIND RPSF EXTENSION --------------------------+
 
        ierr = 0
        ninstr = 3
        instr(1) = 'RESPONSE'
        instr(2) = 'RPRF'
        instr(3) = 'NET'
        nsearch = 50
        rwmode = 0

       call mvext (rwmode, infile, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, ierr, chatter)
       
       if (ierr .ne. 0) then
           ierr = 0
           ninstr = 3
           instr(1) = 'RESPONSE'
           instr(2) = 'RPRF'
           instr(3) = 'TOTAL'
           nsearch = 50
           
           call mver (iunit, extnum,ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, ierr, chatter)
 

           if (ierr .ne. 0) then
               subinfo='error in searching for observed PSF extension'
               call wterrm(subname,version,subinfo)
               subinfo = 'RPSF observed data is not present'
               call wterrm(subname,version,subinfo)
               call ftclos(iunit,status)
               subinfo = ' closing PSF obs file'
               call wtferr(subname,version,status,subinfo)
               return
           else
               subinfo=' extension with observed PSF data found'
               call wtinfo(chatter,10,3,subinfo)
           endif
       else
           subinfo=' extension with observed PSF data found'
           call wtinfo(chatter,10,3,subinfo)
       endif

c --------- STARTS if block for READ OBSERVED DATA --------------------+

       call rdrpf1(iunit, hduclas3, npts, inrad_lo, inrad_hi, radunit,
     >             ntheta, theta_lo, theta_hi, thetaunit, nenerg,
     >             energ_lo, energ_hi, energunit, incts, qerror, inerr,
     >             rpsfunit, qarea, area_wgt, telrad, instrad, maxrad,
     >             maxtheta,ierr,chatter)

       IF (ierr.NE.0) THEN
           errinfo = ' reading RPSF file'
           call wterrm(subname,version,errinfo)
           return
       ENDIF

c ------------------- READ PIXEL SIZE----------------------------------

       status = 0
       call ftgkye(iunit,'PIXSIZE',pix,comm,status)
       errinfo = ' reading PIXSIZE '
       call wtferr(subname,version,status,errinfo)
       IF (pix.EQ.(0.0)) THEN
           errinfo = 
     &	   'The PIXSIZE keyword is set to 0.0 in radial profile'
           call wterrm(subname,version,errinfo)
           ierr = 1
           return
       ENDIF

c --------- Converting pixsize from degrees to arcmin/pixel ------------

       pix_size = pix * 60

c ---------- Read CHANMIN and CHANMAX values if present ---------------

       status = 0
       call ftgkyj(iunit,'CHANMIN',chanmin,comm,status)
       status = 0
       call ftgkyj(iunit,'CHANMAX',chanmax,comm,status)
       status = 0

c ------------ Read BACKGROUND if RPSF_BKGD is true ------------

       IF (rpsf_bkgd) THEN
           call ftgkye(iunit,'BACKGRND',bkgd,comm,status) 
           errinfo = 'reading BACKGRND '
           call wtferr(subname,version,status,errinfo)
       ENDIF

       status = 0  
       call ftclos(iunit,status)
       errinfo = 'closing RPSF file'
       call wtferr(subname,version,status,errinfo)

c ------------ CALLING REBINNING ROUTINE ---------------------------

       call cpsf_rebin(inrad_lo, inrad_hi, area_wgt, incts, inerr, npts,
     >                 pix_size, c_min, rad_lo, rad_hi, cts_arc2, 
     >                 err_arc2, maxin, maxrad, maxtheta, negpres, lbin,
     >                 chatter, bkgd, sumrcts, sumtcts, calc, bkgd_rad,
     >                 ierr)
        
        return
        end
c ---------------------------------------------------------------------+
c                     END OF CPSF_RINFILE 
c ---------------------------------------------------------------------+

*+CPSF_REBIN
        subroutine cpsf_rebin(inrad_lo,inrad_hi,area_wgt,incts,inerr,
     >                        npts,pix_size,c_min,rad_lo,rad_hi,
     >                        cts_arc2,err_arc2,maxin,maxrad,maxtheta,
     >                        negpres,lbin,chatter,bkgd,sumraw,sumtcts,
     >                        calc,bkgd_rad,ierr)

c ------------------ DESCRIPTION -------------------------------------
c
c This routine rebins the radial profile data by comparing each bin with
c the user defined Minimum counts/bin ,and rebinning if the bin is less
c than it. The arrays inrad_lo,inrad_hi,in_pix,incts,inerr have been used 
c to read data from radial file. After rebinning check the arrays rad_lo
c rad_hi,area_wgt,cts_arc2 and err_arc2 are used throughout the program.
c
c --- VARIABLES ---------------------------------------------------------

        IMPLICIT NONE
        integer maxin,maxrad,chatter,maxtheta,ierr, npts
        real inrad_lo(*),inrad_hi(*),incts(maxin,1,1),inerr(maxin,1,1)
        real rad_lo(*),rad_hi(*),cts_arc2(maxrad,1,1)
        real err_arc2(maxrad,1,1), area_wgt(maxrad,1,1),bkgd, bkgd_rad
        real pix_size,c_min,sumraw,sumtcts
        logical lbin,negpres,calc
c
c -------------------- INTERNAL VARIABLES --------------------------
c
        character(200) subinfo
        real sumrcts,rawcts,sumpix,absumr,area,sumarea,sumpix_bkgd
        real per_bkgd,sumcts ,pi,totpix,totpix2,sumrcts_bkgd
        integer i,j,k,onpts,ibin,step,remain, tmaxin
        parameter (tmaxin=1000)
        real in_pix(tmaxin)
        logical rebin
c
c ----------------------- VARIABLE DIRECTORY --------------------------
c
c Arguments ...
c
c maxin      int    : Maximum size of arrays
c pix_size   real   : pixel size in arcmin per pixel
c c_min      real   : Minimum counts/bin (user defined)
c                     after rebin
c inrad_lo   real   : Before rebin,lower edge of radial bins (in arcmins)
c inrad_hi   real   : Before rebin,upper edge of radial bins (in arcmins)
c in_pix     real   : Array of No. of pixels in each bin
c                     no longer argument 1.0.1; internal array
c incts      real   : Before rebin, array containing ctc/arcmin^2
c inerr      real   : Before rebin, array of errors on incts
c area_wgt   real   : Area weighting factors
c rad_lo     real   : After rebin, lower edge of radial bins (in arcmins)
c rad_hi     real   : After rebin, upper edge of radial bins (in arcmins)
c n_pix      real   : After rebin, Array of No. of pixels in bins
c cts_arc2   real   : After rebin, array of cts/arcmin^2
c err_arc2   real   : After rebin, error on cts/arcmin^2
c sumraw     real   : Sum of raw counts
c chatter    int    : Chattines flag (<5 quiet,>5 normal,>20 noisy)
c negpres    logical: true if negative counts present
c lbin       logical: true if last bin has < c_min counts
c 
c Internals ...
c
c rawcts     real   : Raw counts
c sumrcts    real   : Sum of raw counts that are rebinned for current rebin
c sumpix     real   : Sum of No. of pixels in a bin when rebin occurs
c onpts      int    : No. radial of data values after rebin
c ibin       int    : No. of bin which is currently in rebin process
c step       int    : Counter for No. of bin accumulated for new bin
c remain     int    : Counter for remaining No. of data values 
c i,j,k      int    : counters for loops
c rebin      logical: True if current bin requires rebinning
c absumr     real   : Absolute value of sumrcts
c tmaxin     integer: tmaxin=maxin
c                   : it was made this way since one cannot assign dimension
c                   : passed to an unpassed variable
c
c ------------------ AUTHORS/MODIFICATION HISTORY ------------------------
c
c Rehana Yusaf (1993 January)
c Rehana Yusaf (1993 Febuary) : Change code to rebin PSPCRPSF format data
c Rehana Yusaf (1993 June 8)  : Add negpres and lbin
c Rehana Yusaf (1993 Sept 13) 1.0.1; Arguments changed due to reading
c                             RPSF Ver 1993a in RINFILE instead of 1992a
c
c Rehana Yusaf (1994 Jan 20) 1.0.2; bkgd passed to this routine, and taken away
c                             from the sum of the raw counts.
c Rehana Yusaf (1994 Feb 3) 1.0.3; sumraw calculated correctly !
c Rehana Yusaf (1995 Feb 28) 1.0.4; add calc and bkgd_rad parameters
c Rehana Yusaf (1995 May 2) 1.0.5; If calc and calculated bkgd gives
c                                  a -ve sumtcts ( due to excl regions ?)
c                                  then calculate bkgd by setting sumtcts
c                                  to 1, bkgd = (sumcts - 1)/sumpix
c Banashree Mitra Seifert (1996, Jan) 1.1.0:
c                . Introduced tmaxin as integer tmaxin=maxin
c                  It was made this way since one cannot assign dimension
c                  passed to an unpassed variable
c                . Replaced pi = 3.14159.... 
c                  by pi = 4.*atan(1.)
c                . Introduced screen display routines
c                  wtinfo,wterrm,wtwarm
c
c Banashree Mitra Seifert (1997, Oct 10) 1.2.0: 
c     . The calculation of area_wgt was wrong. binning was ok, but while
c       running pcrpsf/hrirpsf after binning, resulted in a mismatch between 
c       the psf model normalization and the data further down the 
c       data-analysis pipeline. This error happened when binning requires 
c       counts from more than one bin from unbinned input data. So replaced 
c              area_wgt(j,1,1) = sumarea/area
c by          
c              area_wgt(j,1,1) = sumarea/k
c
c-------------------------------------------------------------------------

       character(5) version
       parameter (version='1.2.0')
       character(11) subname
*-

c ------------------- USER INFORMATION ------------------------------

        pi = 4. * atan(1.) 
        subname = 'cpsf_rebin'
        subinfo = 'using '//subname//' Ver '//version
        call wtinfo(chatter,10,1,subinfo)

c ------------------ INITIALISE VARIABLES ----------------------------

        IF (calc) THEN
            IF (bkgd_rad.GE.inrad_hi(npts)) THEN
               subinfo='Inner radius for bkd calculation is > '
     >                 //'Outer radius'
               call wterrm(subname,version,subinfo)
               ierr = 1
               return
            ENDIF

            per_bkgd = bkgd_rad/inrad_hi(npts)

            IF (per_bkgd.GE.(0.75)) THEN
                subinfo='The inner radius for bkgd calculation'
     >                   // ' is > 75% of the total radius'
                call wtwarm(subname,version,chatter,1,subinfo)
            ENDIF
        ENDIF

        sumrcts = 0
        sumpix_bkgd = 0
        sumrcts_bkgd = 0
        do i=1,npts
           area = pi * (inrad_hi(i)**2 - inrad_lo(i)**2)
           in_pix(i) = area_wgt(i,1,1) * area /pix_size**2
           IF (calc) THEN
               IF (inrad_lo(i).GE.bkgd_rad) THEN
                   sumpix_bkgd = sumpix_bkgd + in_pix(i)
                   rawcts = incts(i,1,1) * in_pix(i) * pix_size**2
                   sumrcts_bkgd = sumrcts_bkgd + rawcts
               ENDIF
           ENDIF
        enddo

        IF (calc) THEN
            IF (sumrcts_bkgd.GE.0.0) THEN
                bkgd = sumrcts_bkgd/sumpix_bkgd
            ELSE
                subinfo='the sum of the counts in the'
     >                   //' bkgd region is < 0'
                call wtwarm(subname,version,chatter,1,subinfo)
                subinfo='the bkgd has been set to 0'
                call wtinfo(chatter,10,1,subinfo) 
                bkgd = 0
            ENDIF
        ENDIF

        rebin = .false.
        negpres = .false.
        lbin = .false.
        step = 1
        onpts = 0
        i = 1
        j = 1
        totpix2=pi*(inrad_hi(npts)**2-inrad_lo(1)**2)/pix_size**2

c --------- A LOOP WHICH CHECKS EACH BIN AND REBINS IF NECESSARY ------

        sumraw = 0.0
        absumr = 0.0
        totpix = 0.0
        do WHILE (i.LE.npts)
           step=1
           rawcts = incts(i,1,1) * in_pix(i) * pix_size**2
           
           IF (rawcts.LT.(0.0)) THEN
               IF (.NOT.negpres) THEN
                   subinfo='input data contains -ve counts !'
                   call wtwarm(subname,version,chatter,1,subinfo)
                   negpres=.true.
               ENDIF
           ENDIF
           IF (rawcts.GE.c_min) THEN
               rad_lo(j) = inrad_lo(i)
               rad_hi(j) = inrad_hi(i)
               cts_arc2(j,1,1) = incts(i,1,1)
               err_arc2(j,1,1) = inerr(i,1,1)
               area_wgt(j,1,1) = area_wgt(i,1,1)
               sumraw = sumraw + rawcts
               totpix = totpix + in_pix(i)
           ELSE
               rebin = .true.
               ibin = i
               sumrcts = 0
               absumr = 0
               sumpix = 0
               sumarea = 0
               k = 0
               remain = npts - ibin
 
               do WHILE (rebin.AND.(k.LE.remain))
                  sumrcts = sumrcts + rawcts
                  absumr = absumr + ABS(rawcts)
                  sumpix = sumpix + in_pix(ibin + k)
                  sumarea = sumarea + area_wgt(ibin+k,1,1)
                  IF (sumrcts.GE.c_min) THEN
                      rebin = .false.
                  ENDIF
                  k = k + 1
                  rawcts=incts(ibin+k,1,1)*in_pix(ibin+k)*pix_size**2
                  IF (rawcts.LT.(0.0)) THEN
                     IF (.NOT.negpres) THEN
                         subinfo='input data contains -ve counts!'
                         call wtwarm(subname,version,chatter,1,subinfo)
                         negpres = .true.
                     ENDIF
                  ENDIF 
                enddo

c ---------- IF LAST BIN < C_MIN PRINT WARNING ----------------

              IF (rebin.AND.(k.GT.remain)) THEN
                  subinfo='last bin has less than min cnts/bin'
                  call wtwarm(subname,version,chatter,1,subinfo)
                  lbin=.true.
              ENDIF
              rad_lo(j) = inrad_lo(ibin)
              rad_hi(j) = inrad_hi(ibin + k - 1)
              cts_arc2(j,1,1) = sumrcts/(sumpix*pix_size**2)
              err_arc2(j,1,1) = SQRT(absumr)/(sumpix*pix_size**2)
              area = pi*(rad_hi(j)**2 - rad_lo(j)**2)

ccc this calculation of area_wgt was wrong. binning was ok, but while
ccc running pcrpsf/hrirpsf after binning, resulted in a mismatch between 
ccc the psf model normalization and the data further down the 
ccc data-analysis pipeline. This error happened when binning requires 
ccc counts from more than one bin from unbinned input data. So replaced 
ccc              area_wgt(j,1,1) = sumarea/area
ccc by          
ccc              area_wgt(j,1,1) = sumarea/k

              area_wgt(j,1,1) = sumarea/k
              step = k 
              totpix = totpix + sumpix
              sumraw = sumraw + sumrcts
           ENDIF
            j = j+ 1
            i = i + step
       enddo
        sumcts = sumraw
        sumraw = sumcts - (totpix * bkgd)
        sumtcts = sumcts - (totpix2 * bkgd)
      IF (calc) THEN
        IF (sumtcts.LE.0.0) THEN
          bkgd = (sumcts - 1)/totpix2
          sumtcts = 1
          sumraw = sumcts - totpix*bkgd
          subinfo ='the background value has been calculated such'
     >//' that the sum of the'
          call wtinfo(chatter,10,1,subinfo)
          subinfo='theoretical counts is set to 1. If the background'
     >//' is calculated'
          call wtinfo(chatter,10,1,subinfo)
          subinfo =' in the usual way then sumtcts is -ve, this may be'
     >//' due to excluded regions.'
          call wtinfo(chatter,10,1,subinfo)
        ENDIF
      ENDIF
      IF (calc) THEN
          write(subinfo,'(a,f20.9)') ' calculated bkgd value :',bkgd
          call wtinfo(chatter,10,2,subinfo)
      ENDIF

          write(subinfo,'(a,F20.9)')' sum of pixels calculated using'
     >//' area of circle:',totpix2
          call wtinfo(chatter,20,2,subinfo)
          write(subinfo,'(a,F20.9)')' actual sum of pixels :',totpix
          call wtinfo(chatter,20,2,subinfo)

         IF ((totpix/totpix2).LE.(0.9)) THEN
           subinfo='actual sum of pixels is < 90% of sum'
     >             //' of pixels calculated using area of circle, '
     >             // 'where the outer radius is used'
           call wtwarm(subname,version,chatter,9,subinfo) 
           subinfo = 'note: This may be due to regions being excluded'
           call wtinfo(chatter,10,1,subinfo)
         ENDIF

          write(subinfo,'(a,F20.9)')'total sum of counts :',sumcts
          call wtinfo(chatter,20,2,subinfo)
          write(subinfo,'(a,F20.9)')'sum of counts in source :',sumraw
          call wtinfo(chatter,20,2,subinfo)
          write(subinfo,'(a,a,F20.9)')'theoretical sum of counts'
     >,'(corrected for any excl regions):',sumtcts
          call wtinfo(chatter,20,2,subinfo)

        IF (sumrcts.LT.0) THEN
            subinfo = 'sum of counts is negative this may be due to'
     >                //' background dominating the source'
            call wtwarm(subname,version,chatter,1,subinfo)
            ierr = 1
            return
        ENDIF

        IF (sumtcts.LT.0) THEN
            subinfo = 'theoretical Sum of counts is negative, this'
     >              //' may be due to background dominating the source'
            call wtwarm(subname,version,chatter,1,subinfo)
            ierr = 1
            return
        ENDIF
        IF ((sumraw/sumcts).LE.(0.1)) THEN
             subinfo ='source is less than 10% of total counts !'
             call wtwarm(subname,version,chatter,1,subinfo)
        ENDIF

        onpts = j - 1
        npts = onpts

        return
        end
c ---------------------------------------------------------------------+
c              END OF SUBROUTINE CPSF_REBIN              
c ---------------------------------------------------------------------+

*+CPSF_RESULTS
       subroutine cpsf_results(outfile, infile, mnver, npts, rad_lo,
     >                         rad_hi, radunit, ntheta, theta_lo,
     >                         theta_hi, thetaunit, nenerg, energ_lo, 
     >                         energ_hi, energunit, cts_arc2, qerror, 
     >                         err_arc2, rpsfunit, qarea, area_wgt,
     >                         pix_size, maxrad, maxtheta, bkgd, c_min,
     >                         telrad, instrad, negpres, lbin, chanmin,
     >                         chanmax, sumrcts, sumtcts,
     >                         ierr, chatter, killit)
c --------------- DESCRIPTION ----------------------------------------
c This subroutine writes observed and thoeretical radial psf to output 
c file. The output file is in FITS format RPSFVER 1993a
c
c ------------------------ VARIABLES ----------------------------------

        IMPLICIT NONE
        character*(*) outfile,infile
        character*(*) telrad,instrad
        character(5) mnver
        character*(*) rpsfunit,radunit,thetaunit,energunit
        integer maxrad,maxtheta, ntheta,nenerg,chanmin,chanmax
        integer npts,chatter,ierr
        real rad_lo(*),rad_hi(*),cts_arc2(maxrad,maxtheta,1)
        real err_arc2(maxrad,maxtheta,1)
        real c_min,sumrcts,sumtcts, pix_size,bkgd
        real theta_lo(*),theta_hi(*), energ_lo(*),energ_hi(*)
        real area_wgt(maxrad,maxtheta,1)
        logical negpres,lbin,qerror,qarea,killit

c -------------------- INTERNAL VARIABLES-------------------------------

        character(80) subinfo
        character(16) hduclas3,creator
        character(8) cc_min,extname
        integer ounit,ierrstat,maxhist,nk_hist, nk_comm,maxcomm
        parameter (maxhist = 10, maxcomm = 10)
        character(80) hist(maxhist),comms(maxcomm)
        real pix

c ---------------------- VARIABLE DIRECTORY ---------------------------
c
c Arguments ...
c
c outfile    char   : Name of results file    
c infile     char   : Radial Profile input file
c mnver      char   : Main program version
c telrad     char   : Telescope name for radial data
c instrad    char   : Instrument name for radial data
c maxrad     int    : Size of arrays for radial profile data
c maxpred    int    : Size of arrays for predicted radial psf data
c rad_lo     real   : Array of lower edger of radial bins, in arcmin
c rad_hi     real   : Array of upper edge of radial bins, in arcmin
c theta_lo   real   : Lower theta values
c theta_hi   real   : Upper theta values
c energ_lo   real   : Lower energy boundary
c energ_hi   real   : Upper energy boundary
c ntheta     int    : No. of theta values
c nenerg     int    : No. of energy values
c qerror     logical: True if err_arc2 present
c qarea      logical: True if area weighting is present 
c cts_arc2   real   : Array of radial profile in counts/arcmin^2
c err_arc2   real   : Array of errors on cts_arc2
c pix_size   real   : Pixel size in arcmin per pixel
c c_min      real   : Minimum No. of counts/bin (user defined)
c npts       int    : No. of observed radial values
c chatter    int    : Chatter flag (<5 quiet,>5 normal, >20 noisy)
c negpres    logical: true if negative counts present in dataset
c lbin       logical: true if last has < c_min counts
c 
c Internals ...
c 
c errtxt     char   : Error text obtained from ftgerr
c errstr     char   : Error string
c rad_del    real   : Radius of predicted bin
c pix        real   : Pixel size in degrees
c ounit      int    : Output file number
c ierrstat   int    : Error flag
c i,j,k      int    : Counters for loops
c
c -------------------- COMPILATION AND LINKING ------------------------
c
c Link with FTOOLS, FITSIO and CALLIB
c
c -------------------- CALLED ROUTINES ----------------------------
c
c subroutine OP_NPA       : CALLIB routine which opens a FITS file, writes
c                           the header, and null primary array
c subroutine WTRPF1       : CALLIB,Writes observed radial psf data to fits file
c subroutine FTCLOS       : FITSIO routine which closes a FITS file
c subroutine WT_FERRMSG   : CALLIB routine which writes FITSIO and routine 
c                           error message
c
c -------------------- AUTHORS/MODIFICATION HISTORY --------------------
c
c Rehana Yusaf (1993 January 15)
c Rehana Yusaf (1993 March 4) : Adding more comments
c Rehana Yusaf (1993 June 8) : Adding negpres and lbin
c Rehana Yusaf (1993 Sept 13) 1.0.1; Use WT_RPSF1993a instead of 
c                            WT_RPSF1992a
c Rehana Yusaf (1994 Jan 20) 1.0.2; WT_RPSF1993a has been updated to WTRPF1
c                            this write HDUCLASS keywords
c Rehana Yusaf (1995 MAR 7) 1.0.3; sumtcts is also written to outfile
c Rehana Yusaf (1995 April 25) 1.0.4; add killit
c Banashree Mitra Seifert (1996, Jan) 1.1.0;
c                . Variables dimensions are carried from the 
c                  calling subroutine
c                . implemented screen display routines
c                  wtinfo,wtferr
c ---------------------------------------------------------------------------
        character(5) version
        parameter (version = '1.1.0')
        character(14) subname
*- 
c ------------------ USER INFO ----------------------------------------+

        subname = 'cpsf_results'
        subinfo = 'using '//subname//version
        call wtinfo(chatter,10,1,subinfo)

c --------------- MODIFYING OUTPUT INTO DESIRED FORM ------------------
c ------------------ WRITING TO FITS FILE -------------------------
c OBSERVED DATA ...

        call cgetlun(ounit)
        call opnpa(outfile,chatter,ounit,killit,ierrstat)
        nk_hist = 0
        write (cc_min,100) c_min
        nk_comm = 3 
        comms(1) = 'DATA OBTAINED FROM RADIAL PROFILE : '//infile
        comms(2) = 'THE RADIAL PROFILE DATA IS REBINNED '
        comms(3)='USING (USER DEFINED) MINIMUM COUNTS/BIN : '//cc_min
        IF (lbin) THEN
         nk_comm=nk_comm+1
         comms(nk_comm)='LAST BIN CONTAINS LESS THAN MINUMUM COUNTS/BIN'
        ENDIF
        IF (negpres) THEN
         nk_comm=nk_comm+1
         comms(nk_comm)='WARNING:INPUT DATASET CONTAINS NEGATIVE COUNTS'
        ENDIF
        extname = 'OBS RPSF'
        pix = pix_size/float(60)
        If (bkgd.EQ.(0.0)) THEN
          hduclas3 = 'NET'
        ELSE
          hduclas3 = 'TOTAL'
        ENDIF
c ------------------ USER INFO ----------------------------------------+
        call wtrpf1(ounit,extname,hduclas3,npts,rad_lo,rad_hi,radunit,
     >              ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     >              energ_lo,energ_hi,energunit,cts_arc2,qerror,
     >              err_arc2,rpsfunit,qarea,area_wgt,hist,nk_hist,
     >              comms,nk_comm,telrad,instrad,maxrad,maxtheta,
     >              ierr,chatter)
        IF (ierr.NE.0) THEN
            subinfo = 'error in writing RPSF extension '
            call wterrm(subname,version,subinfo)
          return
        ENDIF

c ---------------- Write Pixelsize -----------------------------
  
        ierrstat = 0
        call ftpkye(ounit,'PIXSIZE',pix,8,'pixelsize in deg',ierrstat)
        subinfo = 'writing PIXSIZE '
        call wtferr(subname,version,ierrstat,subinfo)

c ----------------- Write Backgrnd ----------------------------------

        ierrstat = 0
        call ftpkye(ounit,'BACKGRND',bkgd,8,
     >              'Background count rate in cts/pixel ',ierrstat)
        subinfo = 'writing BACKGRND '
        call wtferr(subname,version,ierrstat,subinfo)

c ------------ Write CHANMIN and CHANMAX if not zero ------------------

        ierrstat = 0
        IF (chanmin.NE.0) THEN
          call ftpkyj(ounit,'CHANMIN',chanmin,
     >             'Minimum PI channel for image ',ierrstat)
          subinfo ='writing CHANMIN'
          call wtferr(subname,version,ierrstat,subinfo)
        ENDIF

        ierrstat = 0
        IF (chanmax.NE.0) THEN
          call ftpkyj(ounit,'CHANMAX',chanmax,
     >              'Maximum PI channel for image ',ierrstat)
          subinfo = 'writing CHANMAX'
          call wtferr(subname,version,ierrstat,subinfo)
        ENDIF

c --------------- WRITE SUMRCTS ---------------------------------------+

        ierrstat = 0
        call ftpkye(ounit,'SUMRCTS',sumrcts,8,
     >             'Sum of source counts under profile',ierrstat)
        subinfo = 'writing SUMRCTS'
        call wtferr(subname,version,ierrstat,subinfo)

c --------------- WRITE SUMTCTS ---------------------------------------+

        ierrstat = 0
        call ftpkye(ounit,'SUMTCTS',sumtcts,8,
     >             'Theoretical Sum source counts(corr for any '
     >             //'excl regions)',ierrstat)
        subinfo = 'writing SUMTCTS'
        call wtferr(subname,version,ierrstat,subinfo)

c --------------- WRITE CREATOR KEYWORD -------------------------------+

        ierrstat = 0
        creator = 'rbnrpsf '//mnver
        call ftpkys(ounit,'CREATOR',creator,
     >              's/w task which wrote this dataset',ierrstat)
        subinfo = 'writing CREATOR keyword'
        call wtferr(subname,version,ierrstat,subinfo)

  100   format(F6.0)
        ierrstat = 0
        call ftclos(ounit,ierrstat)
        subinfo='closing output file'
        call wtferr(subname,version,ierrstat,subinfo)
        return
        end
c ---------------------------------------------------------------------
c       END OF SUBROUTINE RESULTS
c ---------------------------------------------------------------------
 
