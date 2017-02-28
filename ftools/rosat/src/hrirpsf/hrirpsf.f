*+HRIRPSF
        subroutine hrirpf

c ------------ DESCRIPTION -------------------------------------
c
c Main task routine for calculating theoretical HRI RPSF using ... 
c 
c Observed radial profile (user defined), OBS RPSF extension 
c Off_Axis histogram file, or user defined angle
c
c All of these input files are in FITS format. The output is written in
c FITS format. Function HRI written by Larry David and Jane Turner is
c used. 
c  
c ----------------- VARIABLES ---------------------------------

        IMPLICIT NONE
        character(180) infile,detfil,outfile
        character(32) theo_fn
	character(80) subinfo
        character(8) telrad,instrad
        character(16) radunit,thetaunit,energunit,rpsfunit
        integer maxrad,maxpred,maxtheta,ierr,ntheta,nenerg,nhist
        integer n_rad,npts,chatter,iget,status,ineed
        integer p_rad_lo,p_rad_hi,p_cts_arc2,p_err_arc2, p_x
        integer p_pred,p_pred_err, p_area_wgt,p_parea_wgt
        integer p_theta_lo,p_theta_hi,p_xmin,p_xmax
        real energ_lo(5),energ_hi(5),theta_min(10),theta_max(10)
        real sohist(10),sumtcts
        real pix_size,bkgd,rad_min,rad_max,bkgd_rad
        real sumrcts,off_ang 
        logical qerror,qarea,calc,killit
        logical lbin,negpres,off_hist,indata,file_bkgd

c --- VARIABLE DIRECTORY ---
c
c infile     char   : Name of radial profile file (input) 
c outfile    char   : Name of results file (output)
c ierr       int    : Error flag, ierr = 0 okay
c telrad     char   : Telescope name for radial data (from infile)
c instrad    char   : Instrument name for radial data (from infile)
c maxrad     int    : Maximum size of arrays used for radial profile data
c maxpred    int    : Maximum size of arrays for predicted psf
c pix_size   real   : pixel size (from rpsf infile), converted to arcmins/pixel
c bkgd       real   : Background count rate in counts/pixel (user defined)
c rad_min    real   : Minimum radius for predicted model in arcmin(user defined)
c rad_max    real   : Maximum radius for predicted model in arcmin(user defined)
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
c x          real   : Array of angles from target position
c pred       real   : Array of theoretical radial psf in counts/arcmin^2
c n_rad      int    : No. of predicted model bins (user defined)
c npts       int    : No. of observed psf data points
c lbin       logical: true if last bin has less than c_min counts
c negpres    logical: true if negative counts present in dataset
c off_ang    real   : Off_axis angle
c
c --- COMPILATION AND LINKING ---
c
c Link with XPI and with FITSIO
c
c --- CALLED ROUTINES ---
c
c subroutine HRI_GP       : Obtains user defined input
c subroutine HRI_RINFILE  : Reads data from radial profile
c subroutine HRI_RDHIST   : Reads off_axis histogram
c subroutine HRI_PRED     : Calculates theoretical PSF
c subroutine HRI_RESULTS  : Writes Observed and theoretical psf to output
c
c --- AUTHORS/MODIFICATION HISTORY ---
c 
c Rehana Yusaf (Feb 4 1994) 1.1.0; PSPCRSPF is used as a basis 
c Rehana Yusaf (Jan 17 1995) 1.1.1; pass bkgd back from _rinfile
c Rehana Yusaf (Mar 8 1995) 1.1.2; If bkgd value is changed recalculate
c                                    sumrcts and sumtcts.
c                                    sumtcts is introduced as the
c                                    normalisation factor. It is
c                                    sumtcts = sumcts - (sumpix*bkgd)
c                                    where sumpix is the 'theoretical'
c                                    sum of pixels using r where r is outer
c                                    radius (pi*r**2). 
c                                    sumrcts = sumcts - (sumpix2*bkgd)
c                                    where sumpix2 is the actual sum of
c                                    the pixels ( not correcting for any excl)
c Rehana Yusaf (1995 April 25) 1.1.3; add clobber, and if calc and bkgd
c                                     value gives a -ve sumtcts then
c                                     adjust bkgd so sumtcts is 1
c
c Banashree Mitra Seifert (1996, Jan) 2.0.0: 
c                . Introduced DMA
c                . Filename as character(180)
c                . Introduced MVEXT
c                . Introduced new screen display routine
c                  wtinfo,wtbegm,wtendm,wtferr
c
c Banashree Mitra Seifert (1997, Feb) 2.1.0:
c                . modification in HRI_GP to make the parameter 'bkgd'
c                  case insensitive
c Peter D Wilson (1998, June 30) 2.1.1:
c                . Updated for new FCPARS behavior
c
c B. Irby & M. Corcoran (2006 07 10) 2.1.2
c                 fixed uninitialized ierr error when getting INFILE 
c                 parameter
c                 write PIXSIZE, BACKGRND & SUMTCOUNTS keywords in
c                 theoretical profile
c 
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '2.1.2')
*- 
c _______________________________________________________________________

        character(8) subname
        character(40) taskname
cccc        COMMON /task/taskname

c ------------- dynamic memory allocated array -------------------------

c        real rad_lo(maxrad),rad_hi(maxrad),cts_arc2(maxrad,maxtheta,1)
c        real err_arc2(maxrad,maxtheta,1), x(maxpred)
c        real pred(maxpred,maxtheta,1),area_wgt(maxrad,maxtheta,1)
c        real theta_lo(maxtheta),theta_hi(maxtheta)
c        real parea_wgt(maxpred,maxtheta,1),pred_err(maxpred,maxtheta,1)
c        real xmin(maxpred),xmax(maxpred)
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
        taskname = 'HRIRPSF'
        subname  = 'hrirpsf' 
        subinfo='using'//subname//version
        call wtinfo(chatter,10,1,subinfo)

c GET PARAMETERS ...

        ierr = 0
        call hri_gp(infile, detfil, off_hist, outfile, indata, 
     >              file_bkgd, bkgd, pix_size, sumtcts, rad_min, 
     >              rad_max, n_rad, off_ang, calc, bkgd_rad, ierr, 
     >              chatter, killit)

        call wtbegm(taskname,version,1)

        IF (ierr.NE.0) THEN
            goto 100
        ENDIF

        maxrad   = 1000
        maxpred  = 1000
        maxtheta = 50
c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_rad_lo = 0
      p_rad_hi = 0
      p_theta_lo = 0
      p_theta_hi = 0
      p_cts_arc2 = 0
      p_err_arc2 = 0
      p_area_wgt = 0
      p_x = 0
      p_xmin = 0
      p_xmax = 0
      p_pred = 0
      p_pred_err = 0
      p_parea_wgt = 0

      iget=0
      status = 0
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
      call udmget(maxrad*maxtheta*1, 6, p_cts_arc2, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxrad*maxtheta*1*4

      status = 0
      call udmget(maxrad*maxtheta*1, 6, p_err_arc2, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxrad*maxtheta*1*4
  
      status = 0
      call udmget(maxrad*maxtheta*1, 6, p_area_wgt, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxrad*maxtheta*1*4

      status = 0
      call udmget(maxpred, 6, p_x, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred, 6, p_xmin, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred, 6, p_xmax, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred*maxtheta*1, 6, p_pred, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxpred*maxtheta*1*4

      status = 0
      call udmget(maxpred*maxtheta*1, 6, p_pred_err, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxpred*maxtheta*1*4

      status = 0
      call udmget(maxpred*maxtheta*1, 6, p_parea_wgt, status)
      if (status .ne. 0) then
         goto 50
      endif
      iget = iget+maxpred*maxtheta*1*4

 50   ineed = 2*maxrad*4 + 3*maxpred*4 + 2*maxtheta*4 + 
     >        3*maxrad*maxtheta*1*4 + 3*maxpred*maxtheta*1*4

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
      call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory I get   =', iget
      call wtinfo(chatter,10,1,subinfo)

      if (status .ne. 0) then
         ierr = -1
         subinfo='failed to allocate dynamic memory '
         call wtferr(subname,version,status,subinfo)
         goto 100
      endif

c READ RPSF FILE ...
        IF (indata) THEN 
        call hri_rinfile(infile, pix_size, file_bkgd, bkgd, npts, 
     >                   MEMR(p_rad_lo), MEMR(p_rad_hi), radunit, 
     >                   ntheta, MEMR(p_theta_lo), MEMR(p_theta_hi), 
     >                   thetaunit, nenerg, energ_lo, energ_hi, 
     >                   energunit, MEMR(p_cts_arc2), qerror, 
     >                   MEMR(p_err_arc2), rpsfunit, qarea, 
     >                   MEMR(p_area_wgt), maxrad, maxtheta, telrad, 
     >                   instrad, sumrcts, sumtcts, negpres, calc, 
     >                   bkgd_rad, lbin, ierr, chatter)

          IF (ierr.NE.0) THEN
              goto 100
          ENDIF
        ENDIF

c READ OFF_AXIS HISTOGRAM ...

        IF (off_hist) THEN
          call hri_rdhist(detfil,theta_min,theta_max,sohist,
     >                     nhist,ierr,chatter)
          IF (ierr.NE.0) THEN
              goto 100
          ENDIF
        ENDIF             

c CALCULATE THEORETICAL PSF ...

       call hri_pred(indata, MEMR(p_x), rad_min, rad_max, n_rad, 
     >               MEMR(p_pred), bkgd, pix_size, maxpred, maxtheta, 
     >               theta_min, theta_max, sohist, nhist, off_hist, 
     >               theo_fn, chatter, off_ang, ierr, sumtcts)

        IF (ierr.NE.0) THEN
            goto 100
        ENDIF

c WRITE RESULTS ...

c ----------------------------------------------------------------------
        call hri_results(outfile, infile, detfil, off_hist, indata, 
     >                   version, npts, MEMR(p_rad_lo), MEMR(p_rad_hi), 
     >                   radunit, ntheta, MEMR(p_theta_lo), 
     >                   MEMR(p_theta_hi), thetaunit, nenerg, 
     >                   energ_lo, energ_hi, energunit, 
     >                   MEMR(p_cts_arc2), qerror, MEMR(p_err_arc2), 
     >                   rpsfunit, qarea, MEMR(p_area_wgt), pix_size, 
     >                   MEMR(p_x), MEMR(p_pred), n_rad, rad_min, 
     >                   rad_max, maxrad, maxtheta, maxpred, theo_fn, 
     >                   bkgd, MEMR(p_xmin), MEMR(p_xmax), 
     >                   MEMR(p_parea_wgt), MEMR(p_pred_err), telrad,
     >                   instrad, negpres, lbin, sumrcts, sumtcts, 
     >                   off_ang, ierr, chatter, killit)


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
      call udmfre(p_cts_arc2, 6, status)
      status = 0
      call udmfre(p_err_arc2, 6, status)
      status = 0
      call udmfre(p_area_wgt, 6, status)
      status = 0
      call udmfre(p_x, 6, status)
      status = 0
      call udmfre(p_xmin, 6, status)
      status = 0
      call udmfre(p_xmax, 6, status)
      status = 0
      call udmfre(p_pred, 6, status)
      status = 0
      call udmfre(p_pred_err, 6, status)
      status = 0
      call udmfre(p_parea_wgt, 6, status)

      if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wtferr(subname,version,status,subinfo)
          ierr=99
      endif

100   call wtendm(taskname,version,ierr,1)
      return
      end
c ----------------------------------------------------------------
c              END OF HRIRPSF 
c ----------------------------------------------------------------

*+HRI_GP
       subroutine hri_gp(infile, detfil, off_hist, outfile, indata, 
     >                   file_bkgd, bkgd, pix_size, sumtcts, rad_min, 
     >                   rad_max, n_rad, off_ang, calc, bkgd_rad, 
     >                   errflg, chatter, killit)
c --- DESCRIPTION ----------------------------------------------------
c
c This routine obtains user defined filenames and parameters.
c
c --- VARIABLES ------------------------------------------------------

        IMPLICIT NONE
        character*(*) infile, detfil,outfile
        character(80) ill_files(4), tdet
        real bkgd,rad_min,rad_max,off_ang, sumtcts,pix_size,bkgd_rad
        integer n_rad,chatter,errflg
        logical off_hist,indata,file_bkgd,calc,killit

c --- INTERNAL VARIABLES ---

        character(180) filename, tmpfile
        character(80) defval,bkgd_str, extinfo, subinfo
        integer ierr,extnum,try,n_ill
        logical ext,valfil

c --- VARIABLE DIRECTORY ---
c
c infile     char   : Name of Radial profile file 
c outfile    char   : Name of results file
c bkgd       real   : Background count rate in counts/pixel
c rad_min    real   : Minimum radius for predicted model in arcmins
c rad_max    real   : Maximum radius for predicted model in arcmins
c n_rad      int    : No. of predicted model bins
c chatter    int    : Chattines flag (<5 quiet,>5normal,>20 noisy)
c ierr       int    : Error flag
c errflg     int    : Error flag which returns to main, 0 ok
c off_ang    real   : Off_axis angle
c
c --- COMPILATION AND LINKING ---
c
c Link with FTOOLS 
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (1994 Feb 4) 1.0.0; PSPC_GP used as a basis
c Rehana Yusaf (1995 Mar 8) 1.0.1; . Another option is added to bkgd,
c                                    if C(alc) then calculate bkgd
c Rehana Yusaf (1995 April 25) 1.0.2; read in clobber
c Banashree Mitra Seifert (1996, Jan) 1.1.0
c                 . Filename character(180)
c                 . Introduced screen display subroutines
c                   wtinfo,wterrm,wtferr
c
c Banashree Mitra Seifert (1997, Feb) 1.2.0
c                 . parameter for background count rate (bkgd) is made 
c                   case insensitive while it is calc/CALC
c                   [call FTUPCH(bkgd_str)]
c Peter D Wilson (1998, June 30) 1.2.1
c                 . Drop INQUIRE call
c ---------------------------------------------------------------------
        character(5) version
        parameter (version = '1.2.1')
        character(7) subname
*-
c ------------ OBTAINING I/O FILENAMES --------------------------

        subname = 'hri_gp'  
        subinfo ='using'//subname//version
        call wtinfo(chatter,10,1,subinfo)

c ---------------------------------------------------------------
        ierr = 0
        n_ill = 0
        ill_files(1) = ''
        ill_files(2) = ''
        ill_files(3) = ''
        ill_files(4) = ''

        call uclgst('infile',infile,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting infile parameter '
          call wtferr(subname,version,ierr,extinfo)
        ENDIF
        call crmvlbk(infile)
        ierr = 0
        call fcpars(infile,filename,extnum,ierr)
        call crmvlbk(filename)
        tmpfile = filename
        call ftupch(tmpfile)
        IF (tmpfile(1:2).EQ.'  ') THEN
          extinfo = 'enter input filename or NONE if no input '
          call wterrm(subname,version,extinfo)
          errflg = 1
          return
        ENDIF
        IF (tmpfile.EQ.'NONE') THEN
          indata = .false.
        ELSE
          indata = .true.
        ENDIF
C PDW 6/30/98: Don't bother! Must let FTOPEN decide if file exists
C        IF (indata) THEN
C          INQUIRE(FILE=filename,EXIST=ext)
C          IF (.NOT.ext) THEN
C            extinfo = 'infile does not exist : '//filename
C            call wterrm(subname,version,extinfo)
C            errflg = 1
C            return
C          ENDIF
C        ENDIF

c GET DETFIL NAME ...

        call uclgst('detfil',detfil,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting DETFIL parameter'
          call wterrm(subname,version,extinfo)
        ENDIF
        ierr = 0
        call crmvlbk(detfil)
        call fcpars(detfil,filename,extnum,ierr)
        tdet = filename 
        call FTUPCH(tdet)
        IF ((tdet(1:2).EQ.'  ').OR.(tdet.EQ.'NONE')) THEN
          off_hist = .false.
        ELSE
          off_hist = .true.
        ENDIF
        IF (off_hist) THEN
C PDW 6/30/98: Don't bother! Must let FTOPEN decide if file exists
C          INQUIRE (FILE=filename,EXIST=ext)
C          IF (.NOT.ext) THEN
C            extinfo = 'detfil does not exist :'//detfil
C            call wterrm(subname,version,extinfo)
C            errflg=1
C            return
C          ENDIF
          n_ill = 1
C PDW 6/30/98: Call ftrtnm to strip off extension
          call ftrtnm( detfil, filename, ierr )
          ill_files(n_ill) = filename
       ENDIF


        call uclgst('outfile',outfile,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting outfile parameter'
          call wterrm(subname,version,extinfo)
        ENDIF
        call crmvlbk(outfile)
        IF (outfile(1:2).EQ.'  ') THEN
          extinfo=' must enter outfile name !!'
          call wterrm(subname,version,extinfo)
          errflg=1
          return
        ENDIF

c
c --- GET CHATTER ---
c
        call uclgsi('chatter',chatter,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting chatter parameter'
          call wterrm(subname,version,extinfo)
        ENDIF  
c
c --- GET CLOBBER ---
c
      killit = .false.
      call uclgsb('clobber',killit,ierr)
      IF (ierr.NE.0) THEN
         extinfo = 'getting clobber parameter'
         call wterrm(subname,version,extinfo)
      ENDIF
      call ck_file(outfile,ill_files,n_ill,valfil,killit,chatter)
      IF (.NOT.valfil) THEN
          extinfo = 'invalid outfile name !'
          call wterrm(subname,version,extinfo)
          errflg = 1
          return
      ENDIF
                 
c ------------ OBTAINING USER DEFINED PARAMETERS --------------

        calc = .false.
        try = 0
        bkgd = 0
   90   call uclgst('bkgd',bkgd_str,ierr)
        call FTUPCH(bkgd_str)
        try = try + 1
        IF (ierr.NE.0) THEN
          extinfo = 'getting background count rate, bkgd parameter'
          call wterrm(subname,version,extinfo)
        ENDIF
        ierr = 0
        defval = '%'
        call uclpst('bkgd',defval,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'putting default detfil parameter'
          call wterrm(subname,version,extinfo)
        ENDIF
        call crmvlbk(bkgd_str)
        IF (bkgd_str(1:1).EQ.'%') THEN
          IF (.NOT.indata) THEN
           extinfo = 'background has to be supplied as '
     >//'infile not entered !' 
          call wterrm(subname,version,extinfo)
           errflg = 1
           return
         ENDIF
         file_bkgd = .true.
        ELSEIF (bkgd_str(1:1).EQ.'C') THEN
          IF (.NOT.indata) THEN
            extinfo='bckgrnd cannot be calculated without a dataset'
            call wterrm(subname,version,extinfo)
            errflg = 1
            return
          ENDIF
          calc = .true.
          call uclgsr('bkgd_rad',bkgd_rad,ierr)
          IF (ierr.NE.0) THEN
            extinfo ='getting background radius parameter'
            call wterrm(subname,version,extinfo)
          ENDIF
       ELSE
         read(bkgd_str,*,IOSTAT=errflg)bkgd
         If (errflg.NE.0) THEN
           extinfo = 'reading bkgd '
           call wterrm(subname,version,extinfo)
           return
         ENDIF
         file_bkgd = .false.
          IF (bkgd.LT.0) THEN
            IF (try.LT.3) THEN
              extinfo=' negative bkgd entered, try again'
              call wterrm(subname,version,extinfo)
              goto 90
            ELSE
              errflg = 1
              return
            ENDIF
          ENDIF
       ENDIF
      
c ... GET SOME ADDITIONAL PARAMETERS FROM USER IF NOT INDATA ... 

        IF (.NOT.indata) THEN
          call uclgsr('sumtcts',sumtcts,ierr)
          IF (ierr.NE.0) THEN
            extinfo = 'getting sum of counts '
            call wterrm(subname,version,extinfo)
            return
          ENDIF
        ENDIF
        IF (sumtcts.EQ.0) THEN
         sumtcts = 1
        ENDIF
        IF ((.NOT.indata).AND.(bkgd.NE.0)) THEN
          call uclgsr('pix_size',pix_size,ierr)
          IF (ierr.NE.0) THEN
            extinfo = 'getting pixelsize'
            call wterrm(subname,version,extinfo)
            return
          ENDIF          
        ENDIF        

c
        call uclgsr('rad_min',rad_min,ierr)
        IF (ierr.NE.0) THEN
          extinfo ='getting minimum radius for predicted bins,rad_min'
          call wterrm(subname,version,extinfo)
        ENDIF
c
        call uclgsr('rad_max',rad_max,ierr)
        IF (ierr.NE.0) THEN
          extinfo ='getting maximum radius for predicted bins,rad_max'
          call wterrm(subname,version,extinfo)
        ENDIF    
c
        call uclgsi('n_rad',n_rad,ierr)
        IF (ierr.NE.0) THEN
          extinfo='getting No. of predicted model bins,n_rad parameter'
          call wterrm(subname,version,extinfo)
        ENDIF    
c
        IF (.NOT.off_hist) THEN
          call uclgsr('off_ang',off_ang,ierr)
          IF (ierr.NE.0) THEN
            extinfo ='getting off_axis angle parameter'
            call wterrm(subname,version,extinfo)
          ENDIF
        ENDIF

        return
        end 		
c ---------------------------------------------------------------------+
c              END OF SUBROUTINE HRI_GP
c ---------------------------------------------------------------------+

*+HRI_RINFILE
        subroutine hri_rinfile(infile, pix_size, file_bkgd, bkgd, npts,
     >                         rad_lo, rad_hi, radunit, ntheta, 
     >                         theta_lo, theta_hi, thetaunit, nenerg, 
     >                         energ_lo, energ_hi, energunit, cts_arc2, 
     >                         qerror, err_arc2, rpsfunit, qarea, 
     >                         area_wgt, maxrad,maxtheta, telrad, 
     >                         instrad, sumrcts, sumtcts, negpres, 
     >                         calc, bkgd_rad, lbin, ierr, chatter)

c ------------------- DESCRIPTION -------------------------------
c
c Reads in Radial profile data from user defined input file. The input file
c is in FITS format. 
c  
c ---------------------------- VARIABLES ---------------------------------

        IMPLICIT NONE
        character*(*) infile,telrad,instrad
        character*(*) radunit,thetaunit,energunit,rpsfunit
        integer maxrad,ierr,maxtheta
        integer npts,chatter,ntheta,nenerg
        real pix_size,sumrcts,bkgd,sumtcts
        real rad_lo(*),rad_hi(*),cts_arc2(maxrad,maxtheta,*)
        real err_arc2(maxrad,maxtheta,*),area_wgt(maxrad,maxtheta,*)
        real energ_lo(*),energ_hi(*),theta_lo(*)
        real theta_hi(*),pix,bkgd_rad 
        logical negpres,lbin,qerror,qarea,file_bkgd,calc

c ------------------ INTERNAL VARIABLES --------------------------

        character(200) subinfo
        character(70)  errinfo
        character(32) comm
        character(20) instr(50),outhdu(9,50),extname(50),outver(9,50)
        character(20) extnames(9,50)
        character(16) hduclas3
        integer iunit,status,i, ninstr,nsearch,next(50)
        real pi,per_bkgd,sumcts,sumrcts_bkgd,sumpix,sumpix2
        real sumpix_bkgd,area,rawcts,pixels, pixsize_sq

c -------------------- VARIABLE DIRECTORY --------------------
c
c Arguments ...
c
c infile     char   : Name of radial profile data file (user defined)
c pix        real   : Pixelsize in degrees (from infile)
c pix_size   real   : pixel size in arcmins per pixel
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
c subinfo    char   : User information 
c iunit      int    : Fortran i/o unit
c
c --- CALLED ROUTINES ---
c
c subroutine RD_RPSF1993a : (CALLIB) Reads OGIP FITS format radial profile
c subroutine HRI_REBIN   : Rebins radial profile data if necessary. That
c                           is, each bin is checked ,and if it is less than
c                           the Minimum No. counts/bin defined by the user 
c                           then the data is rebinned.
c 
c --- AUTHORS/MODIFICATION HISTORY ---
c 
c Rehana Yusaf (1993 January)
c Rehana Yusaf (1993 Febuary) : Change code to read FITS format RPSF data
c Rehana Yusaf (1993 Sept 13) 1.0.1; Change code to read FITS RPSFVER 1993a
c                              (previous read was 1992a)
c Rehana Yusaf (1994 Jan 29) 1.0.2;  RD_RPSF1993a renamed to RDRPF1, this is
c                                 used to read the data
c                               . Additional values read from infile -
c                                 background,sumrcts,chanmin&chanmax
c Rehana Yusaf (1994 Feb 4) 1.0.3; Extracted from PSPCRPSF, chanmin
c                                  and chanmax no longer required. 
c Rehana Yusaf (1995 Mar 8) 1.0.4; If Calc then calculate bkgd and
c                                  recalculate sumrcts
c Rehana Yusaf (1995 May 2) 1.0.5; If calc, and bkgd value gives
c                                  -ve sumtcts then adjust bkgd
c                                  so that sumtcts is 1
c
c Banashree Mitra Seifert (1996, Jan) 1.1.0 :
c                   . modification in variable declaration so that
c                     dimensions are carried in by the calling routine 
c                   . pi = 4i.*atan(1.) instead of pi=3.141592654
c                   . Replaced by call to MVEXT
c                   . Introduced term pixsize_sq 
c                   . Introduced screen display routines
c                     wtinfo,wtwarm,wtferr
c
c Banashree Mitra Seifert (1997, Sept) 1.2.0 : 
c                   . one of the calls to wtinfo was wrong. Fixed
c ---------------------------------------------------------------------
        character(5) version
        parameter (version = '1.2.0')
        character(12) subname
*-  

c --------------- USER INFO -----------------------------

        pi = 4.*atan(1.)
        subname = 'hri_rinfile'
        subinfo = 'using '//subname//version
        call wtinfo(chatter,10,1,subinfo)

c ------------------- READ DATA -------------------------------

        ierr = 0
        ninstr = 2
        instr(1) ='RESPONSE'
        instr(2) = 'RPRF'
        nsearch = 50
       call mvext (0, infile, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, ierr, chatter)

       if(ierr .eq. 2) then
          subinfo='more than one extension found'
          call wtwarm(subname,version,chatter,1,subinfo)
       endif
       if ((ierr .eq. 0) .or. (ierr .eq. 2)) then
            subinfo='has psf extension'
            call wtinfo(chatter,1,3,subinfo)
       else
            subinfo='does not have psf extension'
            call wtwarm(subname,version,chatter,1,subinfo)
            call ftclos(iunit,status)
            subinfo = 'closing PSF file'
            call wtferr(subname,version,status,subinfo)
            return
       endif

c --------------------------- READ DATA ------------------------
       call rdrpf1(iunit, hduclas3, npts, rad_lo, rad_hi, radunit, 
     >             ntheta, theta_lo, theta_hi, thetaunit, nenerg, 
     >             energ_lo, energ_hi, energunit, cts_arc2, qerror, 
     >             err_arc2, rpsfunit, qarea, area_wgt, telrad, 
     >             instrad, maxrad, maxtheta, ierr, chatter)

        IF (ierr.NE.0) THEN
            errinfo = 'reading RPSF file'
            call wtferr(subname,version,ierr,errinfo)
            return
        ENDIF

c --------------- READ PIXEL SIZE ------------------------

        status = 0
        call ftgkye(iunit,'PIXSIZE',pix,comm,status)
        errinfo = 'reading PIXSIZE '
        call wtferr(subname,version,status,errinfo)
        IF (pix.EQ.(0.0)) THEN
            subinfo = 'pix_size not present in rad profile'
            call wterrm(subname,version,subinfo)
            ierr = 1
            return
        ENDIF

c ------ Converting pixsize from degrees to arcmin/pixel -------------

        pix_size = pix * 60

c ------------------- READ SUMRCTS -------------------------

        status = 0
        call ftgkye(iunit,'SUMRCTS',sumrcts,comm,status)
        errinfo = 'reading SUMRCTS'
        call wtferr(subname,version,status,errinfo)
        IF (status.NE.0) THEN
          ierr = 1
          return
        ENDIF        
       
c ------------ READ SUMTCTS -----------------------

        status = 0
        call ftgkye(iunit,'SUMTCTS',sumtcts,comm,status)
        errinfo = 'reading SUMTCTS'
        call wtferr(subname,version,status,errinfo)
        IF (status.NE.0) THEN
          ierr = 1
          return
        ENDIF

c ------------ READ BACKGROUND -----------------------------

        IF (file_bkgd) THEN
          status = 0
          call ftgkye(iunit,'BACKGRND',bkgd,comm,status)
          errinfo = 'reading BACKGRND value'
          call wtferr(subname,version,status,errinfo)
          IF (status.NE.0) THEN
            ierr = 1
            return
          ENDIF
        ENDIF 
        status = 0
        call ftclos(iunit,status)
        errinfo='closing RPSF file' 
        call wtferr(subname,version,status,errinfo)

c --- if calc then calculate background and recalculate SUMRCTS --

        pixsize_sq = pix_size*pix_size
        IF (calc) THEN
            IF (bkgd_rad.GE.rad_hi(npts)) THEN
                subinfo='inner radius for bkd calculation is >'
     >                  //' Outer radius'
                call wterrm(subname,version,subinfo)
                ierr = 1
                return
            ENDIF
            per_bkgd = bkgd_rad/REAL(rad_hi(npts))
            IF (per_bkgd.GE.(0.75)) THEN
                subinfo='The inner radius for bkgd calculation'
     >                  //' is > 75% of the total radius'
                call wtwarm(subname,version,chatter,1, subinfo)
            ENDIF
        ENDIF
c ---------------------------------------------------------------------+
        IF ((calc).OR.(.NOT.file_bkgd)) THEN
            sumcts = 0
            sumrcts_bkgd = 0
            sumpix = 0
            sumpix2 = 0
            sumpix_bkgd = 0
            do i=1,npts
              area = pi * (rad_hi(i)**2 - rad_lo(i)**2)
              pixels = area_wgt(i,1,1) * area /pixsize_sq
              sumpix = sumpix + pixels
              rawcts = cts_arc2(i,1,1) * pixels *pixsize_sq 
              sumcts = sumcts + rawcts
              IF ((rad_lo(i).GE.bkgd_rad).AND.(calc)) THEN
                  sumpix_bkgd = sumpix_bkgd + pixels
                  sumrcts_bkgd = sumrcts_bkgd + rawcts          
              ENDIF
            enddo
            IF (calc) THEN
                IF (sumrcts_bkgd.GE.0.0) THEN
                    bkgd = sumrcts_bkgd/sumpix_bkgd
                ELSE
                    subinfo='The sum of the counts in the'
     >                        //' bkgd region is < 0'
                    call wtwarm(subname,version,chatter,1,subinfo)
                    subinfo=' The bkgd has been set to 0'
                    call wtwarm(subname,version,chatter,1,subinfo)
                    bkgd = 0
                ENDIF
            ENDIF
c ---------------------------------------------------------------------+
          sumrcts = sumcts - (sumpix * bkgd)
          sumpix2 = (pi*(rad_hi(npts)**2-rad_lo(1)**2))/pixsize_sq
          sumtcts = sumcts - (sumpix2*bkgd)
      IF (calc) THEN
          if (sumtcts.LE.0.0) THEN
              bkgd = (sumcts - 1)/sumpix2
              sumtcts = 1
              sumrcts = sumcts - sumpix*bkgd
              subinfo =' the background value has been calculated such'
     >               //' that the sum of the theoretical counts is set'
     >               //' to 1. If the background is calculated in the'
     >               //' usual way then sumtcts is -ve' 
     
              call wtwarm(subname,version,chatter,1, subinfo)
              subinfo = 'this may be due to excluded regions.'
              call wtinfo(chatter,9,1,subinfo)
           ENDIF
      ENDIF

      write(subinfo,'(a,f20.9)') ' Calculated bkgd value :',bkgd
      IF (calc) THEN
          call wtinfo(chatter,9,1,subinfo)
      ENDIF

           write(subinfo,'(a,F20.9)')' Sum of pixels calculated using'
     >                   //' area of circle:',sumpix2
           call wtinfo(chatter,10,3,subinfo)
           write(subinfo,'(a,F20.9)')' Actual sum of pixels :',sumpix
           call wtinfo(chatter,10,3,subinfo)

          IF ((sumpix/sumpix2) .LE. (0.9)) THEN
           subinfo='Actual sum of pixels is < 90% of sum of pixels'
     >             //' calculated using area of circle, where the'
     >             //' outer radius is used'
           call wtwarm(subname,version,chatter,10,subinfo)
           subinfo = 'NOTE: This may be due to regions being excluded'
           call wtwarm(subname,version,chatter,10,subinfo)
          ENDIF
 

          write(subinfo,'(a,F20.9)')' Total sum of counts :',sumcts
          call wtinfo(chatter,20,3,subinfo)
          write(subinfo,'(a,F20.9)')' Counts in source :',sumrcts
          call wtinfo(chatter,20,3,subinfo)
          write(subinfo,'(a,a,F20.9)')' Theoretical sum of counts'
     >,'(corrected for any excl regions):',sumtcts
          call wtinfo(chatter,20,3,subinfo)

          IF (sumrcts.LT.0) THEN
              subinfo = ' Sum of counts is negative'
              call wtwarm(subname,version,chatter,10,subinfo)
              subinfo='this may be due to background dominating'
     >                 //' the source'
              call wtinfo(chatter,10,3,subinfo)
              ierr = 1
              return
           ENDIF

          IF ((sumrcts/sumcts).LE.(0.1)) THEN
               subinfo ='Source is less than 10% of total counts !'
               call wtwarm(subname,version,chatter,1,subinfo)
          ENDIF
        ENDIF
        return
        end
c ------------------------------------------------------------------
c              END OF SUBROUTINE HRI_RINFILE      
c ------------------------------------------------------------------

*+HRI_PRED
       subroutine hri_pred(indata, x, rad_min, rad_max, n_rad, pred,
     >                     bkgd, pix_size, maxpred, maxtheta, 
     >                     theta_min, theta_max, sohist, nhist, 
     >                     off_hist, theo_fn, chatter, off_ang, errf,
     >                     sumtcts)
c ---------------------------------------------------------------------+
c ------------------------ DESCRIPTION -------------------------
c
c This subroutine calculates the theoretical radial psf. The function 
c hri called by this routine essentially does the calculation. This 
c routine prepares the data for the function. 
c
c ---------------------- VARIABLES ----------------------

        character*(*) theo_fn
        integer maxpred,maxtheta
        integer n_rad,chatter,errf,nhist
        real rad_min,rad_max,bkgd,pix_size,sumtcts,off_ang
        real pred(maxpred,maxtheta,*), x(*)
        real theta_min(*),theta_max(*),sohist(*)
        logical off_hist,indata

c --- INTERNAL VARIABLES ---

        character(100) subinfo
        real rad_del,psf,psf_lo,psf_hi,hri
        integer i,j,ierr,k

c --- VARIABLE DIRECTORY ---
c
c Arguments ...
c
c maxpred    int    : Maximum size of arrays used for predicted radial psf
c x          real   : Array of angles from target position
c rad_min    real   : Minimum radius for predicted model (user defined)
c rad_max    real   : Maximum radius for predicted model (user defined)
c bkgd       real   : Background count rate (user defined),cts/pixel
c pix_size   real   : pixel size 
c n_rad      int    : No. of predicted model bins (user defined)
c off_ang    real   : off_axis angle
c errf       int    : Error flag
c
c Internals ...
c
c sum        real   : Sum of cts
c rad_del    real   : Radius for predicted model bins
c enst       int    : Starting channel for weighting pred, corresponds
c                     to en_min
c ierr       int    : error flag for psf function
c i,j        int    : Counters for loops
c
c --- CALLED ROUTINES ---
c 
c function HRI      : Calculates off-axis psf.
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (January 15)
c Rehana Yusaf (1993 June 9) : Substitute off_axis function for on_axis one
c Rehana Yusaf (1994 Jan 31) 1.0.2; Update so that if phafile not entered
c                              then uniform weighting.
c                              If .NOT.indata and not RMF then an energy step 
c                              of 10 eV is used from en_min to en_max     
c
c Rehana Yusaf (1994 Feb 4) 1.0.3; The HRI has no energy dependence and
c                                  is not weighted with the phafile 
c Rehana Yusaf (1995 Mar 8) 1.0.4; sumtcts is used to normalise the
c                                  predicted psf. sumtcts is the sum
c                                  of the counts under the profile
c                                  (correcting for excl regions)
c                                  before sumrcts was used, it inclu
c                                  the excl regions
c
c Banashree Mitra Seifert (1996, Jan) 1.1.0 :
c           . changed in variable declaration to get dimensions from 
c             calling routine
c           . Carry dimensions from calling routine           
c           . Introduced screen display routines
c             wtinfo 
c ------------------------------------------------------------------------
        character(5) version
        parameter (version = '1.1.0')
        character(9) subname
*- 
c --------------------- USER INFO ------------------------------

        subname ='hri_pred' 
        subinfo = 'using '//subname//version
        call wtinfo(chatter,10,1,subinfo)

c ------------ PREPARE DATA FOR PSF --------------------------

        rad_del = (rad_max - rad_min)/REAL(n_rad)
        do i=1,n_rad
           x(i) = rad_min + (i-1) * rad_del
        enddo
c
c       <<<--- CALCULATE PREDICTED PSF USING FUNCTION --->>>
c
        theo_fn = 'LDAVID (OFF-AXIS) PSF ,Nov 1993'
        do j=1,n_rad
           IF (off_hist) THEN
              do k=1,nhist
                 ierr = 0
                 psf_lo= hri(theta_min(k),x(j)*60)*3600
                 psf_hi=hri(theta_max(k),x(j)*60)*3600
                 psf=sumtcts*(psf_lo+psf_hi)/2.
                 pred(j,1,1)=pred(j,1,1)+psf*sohist(k)
              enddo
            ELSE
              pred(j,1,1)=sumtcts*hri(off_ang,x(j)*60)*3600   
            ENDIF
            IF (bkgd.NE.0) THEN
              pred(j,1,1) = pred(j,1,1) + bkgd/(pix_size**2)
            ENDIF
        enddo
        return
        end
c ------------------------------------------------------------------
c              END OF SUBROUTINE HRI_PRED 
c ------------------------------------------------------------------

*+HRI_RESULTS
       subroutine hri_results(outfile, infile, detfil, off_hist, 
     >                        indata, mnver, npts, rad_lo, rad_hi,
     >                        radunit,ntheta,theta_lo,theta_hi,
     >                        thetaunit, nenerg, energ_lo, energ_hi, 
     >                        energunit, cts_arc2, qerror, err_arc2, 
     >                        rpsfunit, qarea, area_wgt, pix_size, x, 
     >                        pred, n_rad, rad_min, rad_max, maxrad, 
     >                        maxtheta, maxpred, theo_fn, bkgd, xmin, 
     >                        xmax, parea_wgt, pred_err, telrad, 
     >                        instrad, negpres, lbin, sumrcts, sumtcts,
     >                        off_ang, ierr, chatter, killit)

c -------------------------- DESCRIPTION --------------------------------
c
c This subroutine writes observed and thoeretical radial psf to output 
c file. The output file is in FITS format RPSFVER 1993a
c
c ------------------------------ VARIABLES ------------------------------
        IMPLICIT NONE
        character*(*) outfile,infile,detfil
        character*(*) theo_fn
        character*(*) telrad,instrad
        character*(*) mnver
        character*(*) rpsfunit,radunit,thetaunit,energunit
        character(50)  creator
        integer maxrad,maxpred,maxtheta,ntheta,nenerg
        integer npts,n_rad,chatter,ierr
        real sumrcts, sumtcts, off_ang,rad_min,rad_max,pix_size,bkgd
        real energ_lo(*),energ_hi(*)
        real rad_lo(*),rad_hi(*),cts_arc2(maxrad,maxtheta,*)
        real err_arc2(maxrad,maxtheta,*), area_wgt(maxrad,maxtheta,*)
        real x(*),pred(maxpred,maxtheta,*)
        real pred_err(maxpred,maxtheta,*)
        real theta_lo(*),theta_hi(*)
        real xmin(*),xmax(*),parea_wgt(maxpred,maxtheta,*)
        logical negpres,lbin,qerror,qarea,off_hist,indata,killit
c ---------------------------------------------------------------------+
c ---------------INTERNAL VARIABLES ------------------------

        character(80) subinfo
        character(8) extname
        integer j,ounit,ierrstat,maxhist,nk_hist
        integer nk_comm,maxcomm
        parameter (maxhist = 10, maxcomm = 10)
        character(80) hist(maxhist),comms(maxcomm)
        character(16) hduclas3 
        real rad_del,pix
c
c ----------- VARIABLE DIRECTORY --------------------
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
c x          real   : Array of angles from target position
c pred       real   : Array of theoretical radial psf in counts/arcmin^2
c rad_min    real   : Minimum radius for predicted model (user defined)
c rad_max    real   : Maximum radius for predicted model (user defined)
c bkgd       real   : Background count rate, counts/pixel
c npts       int    : No. of observed radial values
c n_rad      int    : No. of predicted model bins (user defined)
c chatter    int    : Chatter flag (<5 quiet,>5 normal, >20 noisy)
c negpres    logical: true if negative counts present in dataset
c lbin       logical: true if last has < c_min counts
c off_ang    real   : Off_axis angle
c 
c Internals ...
c 
c errtxt     char   : Error text obtained from ftgerr
c xmin       real   : Array of lower edge of radial bins, in arcmin (predicted)
c xmax       real   : Array of upper edge of radial bins, in arcmin (predicted)
c rad_del    real   : Radius of predicted bin
c pix        real   : Pixel size in degrees
c ounit      int    : Output file number
c ierrstat   int    : Error flag
c i,j,k      int    : Counters for loops
c
c --- COMPILATION AND LINKING ---
c
c Link with FTOOLS, FITSIO and CALLIB
c
c --- CALLED ROUTINES ---
c
c subroutine OP_NPA       : CALLIB routine which opens a FITS file, writes
c                           the header, and null primary array
c subroutine WTRPF1       : CALLIB,Writes observed radial psf data to fits file
c subroutine FTCLOS       : FITSIO routine which closes a FITS file
c subroutine WT_FERRMSG   : CALLIB routine which writes FITSIO and routine 
c                           error message
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (1993 January 15)
c Rehana Yusaf (1993 March 4) : Adding more comments
c Rehana Yusaf (1993 June 8) : Adding negpres and lbin
c Rehana Yusaf (1993 Sept 13) 1.0.1; Use WT_RPSF1993a instead of 
c                            WT_RPSF1992a
c Rehana Yusaf (1994 Jan 31) 1.0.2; WT_RPSF1993a has been renamed to
c                            WTRPF1.
c                            IF RPSF data not input then it is not written !
c Rehana Yusaf (1994 Feb 4) 1.0.3; Modified for HRI case
c Rehana Yusaf (1995 Mar 10) 1.0.4; Add sumtcts
c Rehana Yusaf (1995 April 25) 1.0.5; add clobber
c
c Banashree Mitra Seifert (1996, Jan) 1.1.0 :
c              . Changed variable declaration to get dimensions 
c                from calling routines
c              . Introduced screen display routines
c                wtinfo,wtferr
c -------------------------------------------------------------------
        character(5) version
        parameter (version = '2.0.0')
        character(12) subname
*- 
c ----------------------- USER INFO ----------------------------

        subname = 'hri_results '
        subinfo = 'using '//subname//version
        call wtinfo(chatter,10,1,subinfo)

c ------------ MODIFYING OUTPUT INTO DESIRED FORM -------------------

        rad_del = (rad_max - rad_min)/(REAL(n_rad))
        do j=1,n_rad
           xmin(j) = x(j)
           xmax(j) = xmin(j)+rad_del
        enddo

c ------------ WRITING TO FITS FILE ---------------------------


c OBSERVED DATA ...

        call cgetlun(ounit)
        call opnpa(outfile,chatter,ounit,killit,ierrstat)
        IF (indata) THEN
            nk_hist=0
            nk_comm = 2 
        comms(1) = 'DATA OBTAINED FROM RADIAL PROFILE : '//infile
        comms(2) = 'THE RADIAL PROFILE DATA IS REBINNED IF NECCESARY'
            IF (lbin) THEN
                nk_comm=nk_comm+1
         comms(nk_comm)='LAST BIN CONTAINS LESS THAN MINUMUM COUNTS/BIN'
            ENDIF
            IF (negpres) THEN
                nk_comm=nk_comm+1
            comms(nk_comm)='WARNING: INPUT DATASET CONTAINS -VE COUNTS'
            ENDIF
            extname = 'OBS RPSF'
            pix = pix_size/REAL(60)
            IF (bkgd.EQ.0) THEN
                hduclas3 = 'NET'
            ELSE
                hduclas3 = 'TOTAL'
            ENDIF
        call wtrpf1(ounit,extname,hduclas3,npts,rad_lo,rad_hi,radunit,
     >              ntheta,theta_lo,theta_hi,thetaunit,nenerg,energ_lo,
     >              energ_hi,energunit,cts_arc2,qerror,err_arc2,
     >              rpsfunit,qarea,area_wgt,hist,nk_hist,comms,nk_comm,
     >              telrad,instrad,maxrad,maxtheta,ierr,chatter)

        ierrstat = 0
        call ftpkye(ounit,'PIXSIZE',pix,8,'pixelsize in deg',
     >               ierrstat)
        subinfo = 'writing PIXSIZE '
        call wtferr(subname,version,ierrstat,subinfo)

        ierrstat = 0
        call ftpkye(ounit,'SUMRCTS',sumrcts,8,
     >             'Sum of source counts under profile',ierrstat)
        subinfo = 'writing SUMRCTS'
        call wtferr(subname,version,ierrstat,subinfo)

        ierrstat = 0
        call ftpkye(ounit,'SUMTCTS',sumtcts,8,
     >'Theoretical Sum source counts(corr for '
     >//'excl regions)',ierrstat)
        subinfo = 'writing SUMTCTS'
        call wtferr(subname,version,ierrstat,subinfo)

        ierrstat = 0
        call ftpkye(ounit,'BACKGRND',bkgd,8,'Background in cts/pixel',
     >              ierrstat)
        subinfo = 'writing BACKGRND'
        call wtferr(subname,version,ierrstat,subinfo) 
        creator = 'HRIRPSF '//mnver
        call ftpkys(ounit,'CREATOR',creator,
     >       's/w task that wrote dataset',ierrstat)
        subinfo = 'writing CREATOR'
        call wtferr(subname,version,ierrstat,subinfo)
        ENDIF
        

c THEORETICAL DATA ...

        IF (.NOT.indata) THEN
          telrad = 'ROSAT'
          instrad = 'HRI'
          theta_lo(1) = -99
          theta_hi(1) = -99
          energ_lo(1) = -99
          energ_hi(1) = -99
          ntheta = 1
          nenerg = 1
          radunit = 'arcmin'
          thetaunit = 'arcmin'
          energunit = 'KeV'
          rpsfunit = 'count/arcmin**2'   
        ENDIF
        extname = 'THEO PSF'
        qerror = .false.
        qarea = .false.
        nk_hist = 1
        hist(1) ='THEORETICAL FUNCTION : '//theo_fn
        nk_comm = 1
        comms(1) = ' OBSERVED RADIAL PROFILE : '//infile
        IF (off_hist) THEN
          comms(2)=' OFF_AXIS HISTOGRAM FILE : '//detfil
          nk_comm =2
        ENDIF
        hduclas3 = 'PREDICTED'
        call wtrpf1(ounit,extname,hduclas3,n_rad,xmin,xmax,radunit,
     >                 ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     >                 energ_lo,energ_hi,energunit,pred,qerror,
     >                 pred_err,rpsfunit,qarea,parea_wgt,hist,nk_hist,
     >                 comms,nk_comm,telrad,instrad,maxpred,maxtheta,
     >                 ierr,chatter)   
     
     
       ierrstat = 0
       pix=pix_size/real(60)
       if (pix.gt.0.0) then
        call ftpkye(ounit,'PIXSIZE',pix,8,'pixelsize in deg',
     >               ierrstat)
        else
        call ftpkye(ounit,'PIXSIZE',-99.0,8,'pixelsize not defined',
     >               ierrstat)
        endif
        subinfo = 'writing PIXSIZE '
        call wtferr(subname,version,ierrstat,subinfo)


        ierrstat = 0
        call ftpkye(ounit,'SUMTCTS',sumtcts,8,
     >'Theoretical Sum source counts',ierrstat)
        subinfo = 'writing SUMTCTS'
        call wtferr(subname,version,ierrstat,subinfo)

        ierrstat = 0
        call ftpkye(ounit,'BACKGRND',bkgd,8,
     >'Assumed Background in cts/pixel',ierrstat)
        subinfo = 'writing BACKGRND'
        call wtferr(subname,version,ierrstat,subinfo) 

        
        creator = 'HRIRPSF '//mnver
        call ftpkys(ounit,'CREATOR',creator,
     >       's/w task that wrote dataset',ierrstat)
        subinfo = 'writing CREATOR'
        call wtferr(subname,version,ierrstat,subinfo)
  100   format(F6.0)
        ierrstat = 0
        call ftclos(ounit,ierrstat)
        call wtferr(subname,version,ierrstat,subinfo)
        return
        end
c ---------------------------------------------------------------------
c       END OF SUBROUTINE RESULTS
c ---------------------------------------------------------------------
 

*+HRI_RDHIST
c     ---------------------------------------------------------
      subroutine hri_rdhist(detfil,theta_min,theta_max,sohist,
     >                      nhist,ierr,chatter)   

c --- DESCRIPTION ----------------------------------------------------
c This subroutine opens the detector file and calls Rd_HRI1992a to
c read the DETECTOR extension, with the off_axis histogram.
c --------------------------------------------------------------------      
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) detfil
      integer ierr,chatter,nhist
      real theta_min(10),theta_max(10),sohist(10)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c theta_min    real   : Array of theta min for each bin
c theta_max    real   : Array of theta max for each bin 
c sohist       real   : Array of weighting factor for source region
c nhist        int    : Number of bins
c ierr         int    : Error flag
c chatter      int    : Chattiness flag (>20 verbose)
c
c --- CALLED ROUTINES ----------------------------------------------
c
c RDOHT1          : (CALLIB) General redaer for off_axis detector ext
c FTOPEN          : (FITSIO) Open FITS file
c FTCLOS          : (FITSIO) Close FITS file
c WT_FERRMSG      : (CALLIB) Writes ftsio, and Routine error message 
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf 1.0.0; Sept 23 1993
c Rehana Yusaf 1.0.1; Feb 7 1994, extension search now in this routine
c 
c Banashree Mitra Seifert (1996, Jan) 1.1.0:
c             . variable declaration to get dimension from calling 
c               routines
c             . Introduced call to MVEXT
c             . Introduced screen display routines
c               wtinfo,wtwarm,wtferr
c ------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.0')
      character(11) subname
*-
c ------------------------------------------------------------------
c
c --- INTERNALS ---
c
      character(70) errinfo,subinfo
      character(20) instr(50),outhdu(9,50),extname(50)
      character(20) outver(9,50),extnames(9,50)
      character(16) dettel,detint,detfilt,chantyp
      real texpos,areascal,bohist(10)
      integer iunit,status,detchans,ninstr,nsearch,next(50)

c --- USER INFO ---
c
      subname = 'hri_rdhist'
      subinfo ='using '//subname//version
      call wtinfo(chatter,10,1,subinfo)

c --- OPEN FILE ---

      status = 0
      ninstr = 2
      instr(1) ='SPECTRUM'
      instr(2) = 'DETECTOR'
      nsearch = 50

      call mvext (0, detfil, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, status, chatter)


      if(status .eq. 2) then
          subinfo='more than one extension found'
          call wtwarm(subname,version,chatter,1,subinfo)
       endif
       if ((status .eq. 0) .or. (status .eq. 2)) then
            subinfo='has det extension'
            call wtinfo(chatter,1,3,subinfo)
       else
            subinfo='does not have det extension'
            call wtwarm(subname,version,chatter,1,subinfo)
            call ftclos(iunit,status)
            subinfo = 'closing DET file'
            call wtferr(subname,version,status,subinfo)
            return
       endif
  
      call rdoht1(iunit,dettel,detint,detfilt,texpos,
     >                  areascal,detchans,chantyp,nhist,theta_min,
     >                  theta_max,sohist,bohist,ierr,chatter)
      IF (nhist.EQ.0) THEN
        errinfo = 'no data in DETECTOR file'
        ierr = 1
        call wtferr(subname,version,status,subinfo)
      ENDIF
      return
      end
c ---------------------------------------------------------------------
c     END OF HRI_RDHIST
c ---------------------------------------------------------------------
        

