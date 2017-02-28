    
*+PCRPSF
        subroutine pcrpsf 
c --- DESCRIPTION -----------------------------------------------------
c
c Main task routine for calculating theoretical PSPC RPSF using ... 
c 
c Observed radial profile (user defined), OBS RPSF extension 
c Pha file (user defined), SPECTRUM extension
c RMF file (user defined), EBOUNDS extension
c 
c All of these input files are in FITS format. The output is written in
c FITS format. Function PSFOFF, written by GRH is utilised. 
c  
c
c --- VARIABLES -------------------------------------------------------
c
        IMPLICIT NONE
        character(180) infile,phafile,rmffile,detfil,outfile
        character(30) theo_fn
	character(80) subinfo
        character(8) telrad,instrad,telpha,instpha,telrmf,instrmf
        character(16) radunit,thetaunit,energunit,rpsfunit
        integer maxrad,maxen,maxpred,maxtheta,maxwmap,ierr
        integer iget, ineed, status
        real sumrcts,off_ang,binsize_min
        real pix_size,bkgd,rad_min,rad_max,en_min,en_max,sumtcts
        integer p_rad_lo,p_rad_hi,p_cts_arc2, p_en,p_cts,p_err_arc2
        integer p_pred,p_x, p_area_wgt, p_theta_lo,p_theta_hi
        integer p_parea_wgt,p_pred_err, p_xmin,p_xmax
        integer p_energ_lo,p_energ_hi
        integer p_wmap,p_new_wmap
        real theta_min(10),theta_max(10),sohist(10),bkgd_rad
        real opticx,opticy,deltx,delty,pix_x,pix_y 
        real center_x(500),center_y(500)
        integer ntheta,nenerg,nhist,chanmin,chanmax
        integer iunit,nx,ny,binsize,new_nx,new_ny

        logical qerror,qarea,calc
        integer n_rad,npts,npha,chatter,typflag
        logical lbin,negpres,off_hist,indata,pha,rmf,file_bkgd,killit
c
c --- VARIABLE DIRECTORY ---
c
c infile     char   : Name of radial profile file (input) 
c phafile    char   : Name of PHA file (input)
c rmffile    char   : Name of RMF file (input)
c outfile    char   : Name of results file (output)
c ierr       int    : Error flag, ierr = 0 okay
c telrad     char   : Telescope name for radial data (from infile)
c instrad    char   : Instrument name for radial data (from infile)
c telpha     char   : Telescope name for PHA data (from phafile)
c instpha    char   : Instrument name for PHA data (from phafile)
c telrmf     char   : Telescope name for RMF data (from rmffile)
c instrmf    char   : Instrument name for RMF data (from rmffile)
c maxrad     int    : Maximum size of arrays used for radial profile data
c maxen      int    : Maximum size of arrays used for pha data
c maxpred    int    : Maximum size of arrays for predicted psf
c pix_size   real   : pixel size (from rpsf infile), converted to arcmins/pixel
c bkgd       real   : Background count rate in counts/pixel (user defined)
c rad_min    real   : Minimum radius for predicted model in arcmin(user defined)
c rad_max    real   : Maximum radius for predicted model in arcmin(user defined)
c en_min     real   : Minimum energy boundary in keV (user defined)
c en_max     real   : Maximum energy boundary in keV (user defined)
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
c en         real   : Array of Energies 
c cts        real   : Array of counts per chan from phafile
c x          real   : Array of angles from target position
c pred       real   : Array of theoretical radial psf in counts/arcmin^2
c n_rad      int    : No. of predicted model bins (user defined)
c npts       int    : No. of observed psf data points
c npha       int    : No. of pha data values
c lbin       logical: true if last bin has less than c_min counts
c negpres    logical: true if negative counts present in dataset
c off_ang    real   : Off_axis angle
c binsize_min real  : WMAP rebinned binsize in arcmin
c                     hidden parameter as 1 arcmin
c binsize    int    : WMAP rebinned binsize in no. of pixels
c                     =binsize_min/pixelsize
c ---------------------------------------------------------------
c
c --- COMPILATION AND LINKING ---
c
c Link with XPI and with FITSIO
c
c --- CALLED ROUTINES ---
c
c subroutine PSPC_GP       : Obtains user defined input
c subroutine PSPC_RINFILE  : Reads data from radial profile
c subroutine PSPC_RPHAFILE : Reads data from PHA file
c subroutine PSPC_RDHIST   : Reads off_axis histogram
c subroutine PSPC_PRED     : Calculates theoretical PSF
c subroutine PSPC_RESULTS  : Writes Observed and theoretical psf to output
c
c --- AUTHORS/MODIFICATION HISTORY ---
c 
c Rehana Yusaf (1993 January 15) : Modularisation and minor modifications
c Rehana Yusaf (1993 Febuary 24) : Changing code to read FITS format radial
c                                  profile input
c
c Rehana Yusaf (1993 March 3)    : Minor changes, more info in output
c Rehana Yusaf (1993 May 26)     : Minor changes, removing stops etc
c Rehana Yusaf (1993 June 8)     : Minor changes, lbin and negpres
c                                  added
c Rehana Yusaf (1993 June 9) 1.0.1: Substitute off-axis function for on-axis
c Rehana Yusaf (1993 July 27) 1.0.2; Minor change to Off-Axis algorithm,
c                                    (comment in function)                             
c Rehana Yusaf (1993 August 2) 1.0.3; Minor change to OFF-AXIS alg'
c                                    (comment in function)
c Rehana Yusaf (1993 Sept 13 1993) 1.0.4; RPSFVER 1993a format is used, 
c                                  instead of 1992a this task is changed
c                                  accordingly.        
c Rehana Yusaf (1994 Jan 28) 1.0.5; . Subroutines WT/RD_RPSF1993a have been
c                                     renamed to WTRPF1 and RDRPF1.
c                                   . The rebinning part of the task has been
c                                     removed and is now a seperate task as it
c                                     is useful as a general utility (RBNRPSF)
c                                   . The option of not entering the data and
c                                     only generating the theoretical PSF is
c                                     given.
c                                  
c Rehana Yusaf (1994 Feb 2 ) 1.0.6; . pixelsize is user defined if NOT indata
c				      pha,and bkgd is not zero 
c                                   . sumrcts is user defined if NOT indata 
c                                     and pha 
c Rehana Yusaf (1994 Feb 8) 1.0.7;  . If phafile NOT entered then weighting
c				      is done with sumrcts - bugfix
c Rehana Yusaf (1994 April 6);      . Rename task from PSPCRPSF
c Rehana Yusaf (1994 Oct 18) 2.0.8; . Bug fix in PSPC_PRED for theo curve
c                                     only
c Rehana Yusaf  (1995 Jan 12) 2.0.9; . bug fix, bkgd was not being passed
c                                      back from _rinfile correctly.
c Rehana Yusaf (1995 Mar 1) 2.1.0;   . If bkgd value is changed recalculate
c                                      sumrcts (sumrcts = sumcts - sumpix*bkgd)
c                                      sumtcts = sumcts - sumpix2*bkgd
c                                      where sumpix2 is the sum of pixels
c                                      using AWF
c                                      Add more diagnostics
c Rehana Yusaf (1995 Apr 25) 2.1.1;    add clobber and if calc then
c                                      if calculated bkgd gives -ve
c                                      sumtcts then adjust bkgd so that
c                                      sumtcts is 1
c Rehana Yusaf (1995 May 22) 2.1.2;    bug-fix, killit defined twice
c Rehana Yusaf (1995, Nov 3) 2.1.3;    bug-fix, add error flag to internal 
c                                      writes
c Banashree Mitra Seifert (1996, Jan ) 3.0.0:
c              . Introduced DMA 
c              . Filename made character(180)
c              . Replaced calls by MVEXT          
c              . Introduced screen display routines 
c                wtinfo,wtbegm,wtendm
c              . few general clean-ups
c Rehana Yusaf (1996 Feb 22) 3.0.1; bug-fix in calc_cts
c
c Banashree Mitra Seifert (1996, April) 3.1.0:
c              . introduced calculation with WMAP extension
c
c Banashree Mitra Seifert (1997, Feb5) 3.2.0:
c              . memory allocation was wrong for wmap and new_wmap
c                It is now corrected
c
c Banashree Mitra Seifert (1997, May) 3.3.0:
c              . wmap & new_wmap are made double precision instead of real
c              . maxwmap = 1000 instead of 500
c              . all subroutines passing wmap parameter also passed maxwmap
c                to called routine
c Banashree Mitra Seifert (1997, November) 3.4.0:
c              . Jane wants back the condition that user inputs offaxis 
c                histogram file as 'none'. This means, even if there is an
c                offaxis histogram extension or wmap primary extensionin the 
c                input file(i.e, typflag=1 or typflag=2)
c                user may opt for not use this offaxis histogram by typing 
c                'none' 
c                when prompted for the off-axis histogram filename.
c                To do this, the modification needed is, to add another 
c                clause when typflag=2 or 1 
c                 if((typflag .eq. 1) .and (off_hist)) then only read wmap
c                            or
c                 if((typflag .eq. 2) .and (off_hist)) then only read the
c                offaxis histogram file
c              . another bug I found here now that, if one enters phafile
c                as none, that is, pha=false, then it will not call ck_phafile
c                to get the typflag parameter.
c                This 'if' clause was not there. 
c Peter D. Wilson (1998, March) 3.5.0):
c              . Fixed some problems with the typflag/off_hist comparisons.
c                Not all permutations handled properly, plus when doing
c                WMAP calculations ended up also calling pspc_pred later.
c Peter D Wilson (June 29, 1998) 3.5.1:
c              . Added max_xflt parameter to rdpha1 function call
c Peter D Wilson (June 30, 1998) 3.5.2
c              . Updated for new FCPARS behavior
c Ning Gan  (June 30, 1998) 3.5.3
c              . Updated for the new format of date keywords. 
c
c B. Irby & M. Corcoran (Jul 11 2006) 3.5.4
c              . fixed uninitialized ierr value, corrected 
c                writing of PIXSIZE keyword in output file for
c                theoretical profile
c -----------------------------------------------------------------------   
        character(5) version
        parameter (version = '3.5.4')
*- 
c _______________________________________________________________________
c
        character(7) taskname
        character(7) subname

c ------------- dynamic memory allocated array -------------------------
c
c  real rad_lo(maxrad),rad_hi(maxrad),cts_arc2(maxrad,maxtheta,1)
c  real en(maxen),cts(maxen),err_arc2(maxrad,maxtheta,1)
c  real pred(maxpred,maxtheta,1),x(maxpred)
c  real area_wgt(maxrad,maxtheta,1)
c  real theta_lo(maxtheta),theta_hi(maxtheta)
c  real parea_wgt(maxpred,maxtheta,1),pred_err(maxpred,maxtheta,1)
c  real xmin(maxpred),xmax(maxpred)
c  real*8 wmap(500,500),new_wmap(500,500)
c
c --- DYNAMIC MEMORY ALLOCATION ---
c  the following MEM common block definition is in the system iraf77.inc
c  file
      LOGICAL               MEMB(100)
      INTEGER*2             MEMS(100)
      INTEGER*4             MEMI(100)
      INTEGER*4             MEML(100)
      REAL                  MEMR(100)
      DOUBLE PRECISION      MEMD(100)
      COMPLEX               MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ---------------------------------------------------------------------+

        taskname = 'pcrpsf'
        subname = 'pcrpsf'

        ierr = 0
        call pspc_gp(infile,phafile,detfil,off_hist,rmffile,outfile,
     >               indata,pha,rmf,file_bkgd,bkgd,pix_size,sumtcts,
     >               rad_min,rad_max,n_rad,en_min,en_max,off_ang,calc,
     >               bkgd_rad,ierr,binsize_min,chatter,killit)

        call wtbegm(taskname,version,chatter)

        IF (ierr.NE.0) THEN
            goto 100
        ENDIF

        maxrad  = 1000
        maxen   = 1000
        maxpred = 1000
        maxtheta= 50
        maxwmap = 1000
c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_rad_lo = 0
      p_rad_hi = 0
      p_theta_lo = 0
      p_theta_hi = 0
      p_energ_lo = 0
      p_energ_hi = 0
      p_en = 0
      p_cts = 0
      p_cts_arc2 = 0
      p_err_arc2 = 0
      p_area_wgt = 0
      p_x = 0
      p_xmax = 0
      p_xmin = 0
      p_pred = 0
      p_pred_err = 0
      p_parea_wgt = 0
      p_wmap = 0
      p_new_wmap = 0

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
      call udmget(maxen, 6, p_en, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxen*4

      status = 0
      call udmget(maxen, 6, p_cts, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxen*4
             
      status = 0
      call udmget(maxrad*maxtheta*5, 6, p_cts_arc2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*5*4

      status = 0
      call udmget(maxrad*maxtheta*5, 6, p_err_arc2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*5*4
             
      status = 0
      call udmget(maxrad*maxtheta*5, 6, p_area_wgt, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*5*4

      status = 0
      call udmget(maxpred, 6, p_x, status)
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
      call udmget(maxpred, 6, p_xmin, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*4

      status = 0
      call udmget(maxpred*maxtheta*5, 6, p_pred, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*5*4
             
      status = 0
      call udmget(maxpred*maxtheta*5, 6, p_pred_err, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*5*4

      status = 0
      call udmget(maxpred*maxtheta*5, 6, p_parea_wgt, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpred*maxtheta*5*4

      status = 0
      call udmget(maxwmap*maxwmap, 7, p_wmap, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxwmap*maxwmap*4

      status = 0
      call udmget(maxwmap*maxwmap, 7, p_new_wmap, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxwmap*maxwmap*4

 50   ineed = 2*maxrad*4 + 2*maxtheta*4 + 4*maxen*4 + 3*maxpred*4 +
     >        3*maxrad*maxtheta*5*4 + 3*maxpred*maxtheta*5*4 

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
           call wtinfo(chatter,10,1,subinfo) 
      write(subinfo,'(a,i10)')'total bytes of memory I get   =',iget
           call wtinfo(chatter,10,1,subinfo) 

      if (status .ne. 0) then
          ierr = -1
          subinfo='failed to allocate dynamic memory '
          call wtinfo(chatter,1,1,subinfo) 
          goto 100
      endif

        IF (indata) THEN 
            call pspc_rinfile(infile,pix_size,file_bkgd,npts,
     >                        MEMR(p_rad_lo),MEMR(p_rad_hi),radunit,
     >                        ntheta,MEMR(p_theta_lo),MEMR(p_theta_hi),
     >                        thetaunit,nenerg,MEMR(p_energ_lo),
     >                        MEMR(p_energ_hi),energunit,
     >                        MEMR(p_cts_arc2),qerror,MEMR(p_err_arc2),
     >                        rpsfunit,qarea,MEMR(p_area_wgt),maxrad,
     >                        maxtheta,telrad,instrad,sumrcts,sumtcts,
     >                        negpres,chanmin,chanmax,lbin,bkgd,calc,
     >                        bkgd_rad,ierr,chatter,killit)

            IF (ierr.NE.0) THEN
                goto 100
            ENDIF
        ENDIF
 
        IF (pha.OR.rmf) THEN
            call pspc_rphafile(phafile,rmffile,MEMR(p_en),MEMR(p_cts),
     >                         npha,maxen,telpha,instpha,telrmf,
     >                         instrmf,pha,rmf,ierr,chatter)

            IF (ierr.NE.0) THEN
                goto 100
            ENDIF
        ENDIF

c check here if the PHA file contains WMAP or OFF-AXIS HISTOGRAM
c extension   typflag = 1 -----> contains WMAP file
c             typflag = 2 -----> contains OFF-AXIS HISTOGRAM
c if off_hist = false, then no need to look for the existence of wmap or 
c offaxis histogram extension from the PHA file
c PDW 3/5/98: Check phafile for off-axis info only if detfil is the
c             same as the phafile 

        typflag = 0
        if( pha .and. off_hist .and. (phafile.eq.detfil) ) then
           call ck_phafile(iunit,phafile,typflag,ierr,chatter)
           if (ierr .ne. 0) then
              subinfo='checking out PHA file'
              goto 100
           endif
        endif

ccc        if (typflag .eq. 1) then
ccc   this changed now (Nov12,1997) since Jane wants back the condition that
ccc   one puts offaxis histogram file as 'none'
ccc
ccc   Change back to only testing typflag, since typflag=1 occurs only
ccc   if off_hist is true : PDW 3/5/98

        if (typflag .eq. 1)then
            call pspc_wmap(iunit,nx,ny,maxwmap,MEMD(p_wmap),opticx,
     >                     opticy,
     >                     deltx,delty,pix_x,pix_y,ierr,chatter)   
            if (ierr .ne. 0) then
                subinfo='returning from pspc_wmap'
                call wtinfo(chatter,10,2,subinfo)
                goto 100
            endif

c if binsize=1, then no need to go for rebinning and so set
c the variables of new binning as old(original) bins
c rebinned binsize=binsize in arcmin/size of one pixel
c binsize=binsize_min/deltx

            binsize=int(binsize_min/deltx)

            call wmap_rebin(nx,ny,maxwmap,MEMD(p_wmap),binsize,new_nx,
     >                      new_ny,MEMD(p_new_wmap),
     >                      center_x,center_y,ierr,chatter)
            if (ierr .ne. 0) then
                subinfo='returning from wmap_rebin'
                call wtinfo(chatter,10,2,subinfo)
                goto 100
            endif

C Locate min and max channels for min and max energies... PDW 3/5/98
            IF (.not.indata) THEN
               chanmin = 0
 75            chanmin = chanmin+1
               if ( chanmin.lt.npha .and. 
     >              MEMR(p_en+chanmin-1).lt.en_min ) goto 75
               chanmax = chanmin-1
 76            chanmax = chanmax+1
               if ( chanmax.lt.npha .and.
     >              MEMR(p_en+chanmax-0).le.en_max ) goto 76
            ENDIF

            call calc_wmap(npha,MEMR(p_en),n_rad,rad_min,rad_max,
     >                     MEMR(p_cts),new_nx,new_ny,opticx,opticy,
     >                     center_x,center_y,deltx,delty,maxwmap,
     >                     MEMD(p_new_wmap),sumtcts,bkgd,pix_size,
     >                     MEMR(p_pred),theo_fn,chanmin,chanmax,
     >                     ierr,chatter)

            if (ierr .ne. 0) then
                subinfo='returning from calc_wmap'
                call wtinfo(chatter,10,2,subinfo)
                goto 100
            endif

        else

c this is for typflag=2 
c        IF (off_hist) THEN
cccc        if(typflag .eq. 2)  then
ccc   this changed now (Nov12,1997) since Jane wants back the condition that
ccc one puts offaxis histogram file as 'none'
ccc
ccc PDW 3/5/98: Change again to allow for typflag=0 (ie, no PHA file or
ccc             different file off-axis), but off_hist=TRUE

           if(off_hist) then
              call pspc_rdhist(detfil,theta_min,theta_max,sohist,nhist,
     >             ierr,chatter)
              
              IF (ierr.NE.0) THEN
                 goto 100
              ENDIF
           endif

           call pspc_pred(indata,pha,rmf, MEMR(p_en),MEMR(p_cts),npha,
     >                 MEMR(p_x),rad_min,rad_max,n_rad,MEMR(p_pred),
     >                 en_min, en_max,bkgd,pix_size,maxen,maxpred,
     >                 maxtheta,theta_min,theta_max,sohist,nhist,
     >                 off_hist,theo_fn,chatter,off_ang,ierr,sumtcts,
     >                 chanmin,chanmax,ierr)

           IF (ierr.NE.0) THEN
               goto 100
           ENDIF

        endif

        call pspc_results(outfile,infile,phafile,detfil,off_hist,
     >                    indata,pha,rmf, rmffile,version,npts,
     >                    MEMR(p_rad_lo),MEMR(p_rad_hi),radunit,ntheta,
     >                    MEMR(p_theta_lo),MEMR(p_theta_hi),thetaunit,
     >                    nenerg,MEMR(p_energ_lo),MEMR(p_energ_hi),
     >                    energunit, MEMR(p_cts_arc2),qerror,
     >                    MEMR(p_err_arc2),rpsfunit,qarea,
     >                    MEMR(p_area_wgt),pix_size,MEMR(p_x),
     >                    MEMR(p_pred),n_rad,rad_min,rad_max,maxrad,
     >                    maxtheta,maxpred,theo_fn,bkgd,en_min,en_max,
     >                    chanmin,chanmax,MEMR(p_xmin),MEMR(p_xmax),
     >                    MEMR(p_parea_wgt),MEMR(p_pred_err),telrad,
     >                    instrad,negpres,lbin,sumrcts,sumtcts,
     >                    off_ang,calc,bkgd_rad,ierr,chatter,killit)

        IF (ierr.NE.0) THEN
            goto 100
        ENDIF

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_rad_lo, 6, status)
      status = 0
      call udmfre(p_rad_hi, 6, status)
      status = 0
      call udmfre(p_energ_lo, 6, status)
      status = 0
      call udmfre(p_energ_hi, 6, status)
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
      call udmfre(p_pred, 6, status)
      status = 0
      call udmfre(p_pred_err, 6, status)
      status = 0
      call udmfre(p_parea_wgt, 6, status)
      status = 0
      call udmfre(p_en, 6, status)
      status = 0
      call udmfre(p_cts, 6, status)
      status = 0
      call udmfre(p_x, 6, status)
      status = 0
      call udmfre(p_xmin, 6, status)
      status = 0
      call udmfre(p_xmax, 6, status)
      status = 0
      call udmfre(p_wmap, 7, status)
      status = 0
      call udmfre(p_new_wmap, 7, status)
       
      if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wterrm(taskname,version,subinfo)
          ierr=1
      endif

100   call wtendm(taskname,version,ierr,chatter)

      return
      end

c ----------------------------------------------------------------
c               End of PCRPSF 
c ----------------------------------------------------------------

*+PSPC_GP
        subroutine pspc_gp(infile,phafile,detfil,off_hist,rmffile,
     >                     outfile,indata,pha,rmf,file_bkgd,bkgd,
     >                     pix_size,sumtcts,rad_min,rad_max,n_rad,
     >                     en_min,en_max,off_ang,calc,bkgd_rad,errflg, 
     >                     binsize_min,chatter,killit)
c --- DESCRIPTION ----------------------------------------------------
c
c This routine obtains user defined filenames and parameters.
c
c --- VARIABLES ------------------------------------------------------
c
        IMPLICIT NONE
        character*(*) infile,phafile,rmffile
        character*(*) detfil,outfile
        character(180) ill_files(5), tdet, toutfile
        integer n_rad,chatter,errflg
        real bkgd,rad_min,rad_max,en_min,en_max,off_ang
        real sumtcts,pix_size,bkgd_rad,binsize_min
        logical off_hist,indata,pha,rmf,file_bkgd,calc,killit

c --- INTERNAL VARIABLES ---

        integer extnum,try,n_ill, ierr
        character(70) extinfo, subinfo
        character(180) defval,filename,tmpfile,strbkgd,tmppha
        logical ext,valfil
c
c --- VARIABLE DIRECTORY ---
c
c infile     char   : Name of Radial profile file 
c phafile    char   : Name of Pha file
c outfile    char   : Name of results file
c c_min      real   : Minimum No. counts/bin
c bkgd       real   : Background count rate in counts/pixel
c rad_min    real   : Minimum radius for predicted model in arcmins
c rad_max    real   : Maximum radius for predicted model in arcmins
c en_min     real   : Minimum energy boundary in  keV
c en_max     real   : Maximum energy boundary in keV
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
c Rehana Yusaf (1993 January)
c Rehana Yusaf (1993 Febuary):Change code to use fcecho for screen output
c Rehana Yusaf (1993 June 9) : Temporary measure, off_axis angle is 
c                              prompted for.
c Rehana Yusaf (1993 Sept 23) : Off Axis angle only prompted for if
c                               detector file not entered
c Rehana Yusaf (1994 Jan 28) 1.0.3; . c_min no longer prompted for.
c                                   . backgrnd read in as a string - %
c                                     denotes reading value from infile.
c                                   . sumrcts prompted for if infile entered
c                                     as NONE.
c                                   . phafil can be NONE
c                                   . If infile is NONE then RMFFILE is not
c                                     prompted for.
c                                   . en_min and en_max only prompted for
c                                     when infil is NONE.
c                                   . For all input files INPUT[extnum] is
c                                     allowed.
c Rehana Yusaf (1994 Feb 2) 1.0.4;  . sumrcts is prompted for if not indata
c                                     and pha
c				    . pix_size is prompted for if not indata
c                                     pha, and bkgd is not zero 
c Rehana Yusaf (1994 Feb 9) 1.0.5;  . detfil[extnum] is now allowed
c                                   . If the phafile is entered then
c                                     rmffile is required 
c Rehana Yusaf (1995 Mar 1) 1.0.6;  . Another option is added to bkgd,
c                                     if C(alc) then calculate bkgd
c Rehana Yusaf (1995 April 25) 1.0.7; read in clobber (killit)
c
c Bamashree Mitra Seifert (1996, Jan) 2.0.0:
c              . variable dimensions to carry from calling routine
c              . Filenames made character(180)
c              . Introduced screen display routines 
c                wtinfo,wtferr
c Peter D Wilson (1998, June 30) 2.0.1:
c              . Drop calls to INQUIRE
c -----------------------------------------------------------------------
        character(5) version
        parameter (version = '2.0.1')
        character(12) subname 
*-

c       <<<--- OBTAINING I/O FILENAMES --->>>
c
        subname = 'pspc_getpar '  
        subinfo = 'using'//subname//' '//version 
        call wtinfo(chatter,10,1,subinfo)
c --------------------------------------------------------------------
        n_ill = 0
        ierr = 0
        call uclgst('infile',infile,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'getting infile parameter '
            call wtferr(subname, version, ierr,extinfo) 
        ENDIF
        call crmvlbk(infile)
        ierr = 0
        call fcpars(infile,filename,extnum,ierr)
        call crmvlbk(filename)
        tmpfile = filename
        call ftupch(tmpfile)
        IF (tmpfile.EQ.'  ') THEN
            extinfo = 'enter input filename or none if no input '
            call wtferr(subname,version,errflg,extinfo) 
            errflg = 1
            return
        ENDIF
        IF (tmpfile.EQ.'NONE') THEN
            indata = .false.
        ELSE
            indata = .true.
        ENDIF
C PDW 6/30/98: Don't bother! Let FTOPEN determine if file exists
C        IF (indata) THEN
C            INQUIRE(FILE=filename,EXIST=ext)
C            IF (.NOT.ext) THEN
C                extinfo = 'infile does not exist : '//filename
C                call wtferr(subname, version, errflg,extinfo) 
C                errflg = 1
C                return
C            ENDIF
C        ENDIF

        call uclgst('phafile',phafile,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'getting phafile parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF
        call crmvlbk(phafile)
        ierr = 0
C PDW 6/30/98: Replace with ftrtnm
C        call fcpars(phafile,tmppha,extnum,ierr)
        call ftrtnm( phafile, tmppha, ierr )
        call crmvlbk(tmppha)
        tmpfile =tmppha 
        call ftupch(tmpfile)
        IF ((tmpfile.EQ.'NONE').OR.(tmpfile.EQ.'  ')) THEN
             pha = .false.
        ELSE
             pha = .true.
        ENDIF
        IF (pha) THEN
C PDW 6/30/98: Don't bother! Let FTOPEN determine if file exists
C            INQUIRE (FILE=tmppha,EXIST=ext)
C            IF (.NOT.ext) THEN
C                extinfo = 'PHAfile does not exist :'//tmppha
C                call wtferr(subname, version, errflg,extinfo) 
C                errflg=1
C                return
C            ENDIF
            n_ill = n_ill + 1
            ill_files(n_ill) = tmppha
        ENDIF

c GET DETFIL NAME ...

        call uclgst('detfil',detfil,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'getting detfil parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF
        ierr = 0
        defval = '%'
        call uclpst('detfil',defval,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'putting default detfil parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF        
        call crmvlbk(detfil)
        tdet = detfil
        call FTUPCH(tdet)
        IF ((tdet.EQ.'  ').OR.(tdet.EQ.'NONE')) THEN
             off_hist = .false.
        ELSEIF (tdet(1:1).EQ.'%') THEN
                off_hist = .true.
          IF (.NOT.pha) THEN
            extinfo = 'off_axis histogram cannot be the same '
     &//'as the PHAFILE is not entered !'
            call wtferr(subname, version, errflg,extinfo) 
            errflg = 1
            return
          ENDIF
          detfil = phafile
        ELSE
          off_hist = .true.
        ENDIF
        ierr = 0
C PDW 6/30/98: Replace with call to ftrtnm
C        call fcpars(detfil,filename,extnum,ierr)
        call ftrtnm( detfil, filename, ierr )
        IF (off_hist.AND.(filename.NE.tmppha)) THEN
C PDW 6/30/98: Don't bother! Let FTOPEN determine if file exists
C          INQUIRE (FILE=filename,EXIST=ext)
C          IF (.NOT.ext) THEN
C              extinfo = 'DETFIL does not exist :'//detfil
C              call wtferr(subname, version, errflg,extinfo) 
C              errflg=1
C              return
C          ENDIF
          n_ill = n_ill + 1
          ill_files(n_ill) = filename
       ENDIF

c RMFILE ...
      
        rmf = .false.  
        rmffile = 'none'
        IF ((indata).OR.((.NOT.indata).AND.(pha))) THEN 
            call uclgst('rmffile',rmffile,ierr)
            IF (ierr.NE.0) THEN
                extinfo = 'getting rmfile parameter'
                call wtferr(subname, version, errflg,extinfo) 
            ENDIF
            call crmvlbk(rmffile)
            ierr = 0
C PDW 6/30/98: Replace with call to ftrtnm
C            call fcpars(rmffile,filename,extnum,ierr)
            call ftrtnm( rmffile, filename, ierr )
            call crmvlbk(filename)
            tmpfile = filename
            call ftupch(tmpfile)
            IF ((tmpfile.eq.'  ').OR.(tmpfile.eq.'NONE')) THEN 
                extinfo = ' RMF FILE is required !'
                call wtferr(subname, version, errflg,extinfo) 
                errflg = 1
                return
            ENDIF
C PDW 6/30/98: Don't bother! Let FTOPEN determine if file exists
C            INQUIRE(FILE=filename,EXIST=ext)
C            IF (.NOT.ext) THEN
C                extinfo = 'RMF file does not exist :'//filename
C                call wtferr(subname, version, errflg,extinfo) 
C                errflg = 1
C                return
C            ENDIF
            rmf = .true.
            n_ill = n_ill + 1
            ill_files(n_ill) = filename
        ENDIF

        call uclgst('outfile',outfile,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'getting outfile parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF
        call rmvexsp(outfile,toutfile)
        IF (toutfile.EQ.'  ') THEN
            extinfo='must enter outfile name !!'
            call wtferr(subname, version, errflg,extinfo) 
            errflg=1
            return
        ENDIF

        killit = .false.
        ierr = 0
        call uclgsb('clobber',killit,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'getting clobber parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF
        call ck_file(outfile,ill_files,n_ill,valfil,killit,chatter)
        IF (.NOT.valfil) THEN
            extinfo = 'invalid outfile name!'
            call wtferr(subname, version, errflg,extinfo) 
            errflg = 1
            return
        ENDIF

c       <<<--- OBTAINING USER DEFINED PARAMETERS --->>>

        call uclgsi('chatter',chatter,ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'getting chatter parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF  
      

        calc = .false.
        try = 0
  90    call uclgst('bkgd',strbkgd,ierr)
        try = try + 1
        IF (ierr.NE.0) THEN
            extinfo ='getting background count rate, bkgd parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF
        ierr=0
        call uclpst('bkgd','%',ierr)
        IF (ierr.NE.0) THEN
            extinfo = 'putting default bkgd parameter'
            call wtferr(subname, version, errflg,extinfo) 
        ENDIF

        call crmvlbk(strbkgd)
        call ftupch(strbkgd)
        IF (strbkgd(1:1).EQ.'%') THEN
            file_bkgd = .true.
        ELSEIF (strbkgd(1:1).EQ.'C') THEN
                IF (.NOT.indata) THEN
            extinfo ='background cannot be calculated without a dataset'
                    ierr = 1
                    call wtferr(subname, version, ierr,extinfo) 
                    return
                ENDIF 
          calc = .true.
          call uclgsr('bkgd_rad',bkgd_rad,ierr)
          IF (ierr.NE.0) THEN
              extinfo ='getting background radius parameter'
              call wtferr(subname, version, ierr,extinfo) 
          ENDIF
        ELSE
          read(strbkgd,*,IOSTAT=ierr) bkgd
          If (ierr.NE.0) THEN
              extinfo = 'error in bkgd value '
              errflg = 1
              call wtferr(subname, version, ierr,extinfo) 
              return
          ENDIF
          file_bkgd = .false.
          IF (bkgd.LT.0) THEN
              IF (try.LT.3) THEN
                  extinfo='-ve bkgd entered, try again'
                  call wtferr(subname, version, errflg,extinfo) 
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
                call wtferr(subname, version, errflg,extinfo) 
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
                call wtferr(subname, version, errflg,extinfo) 
                return
            ENDIF          
        ENDIF        

        call uclgsr('rad_min',rad_min,ierr)
        IF (ierr.NE.0) THEN
          extinfo = 'getting minimum radius for predicted bins,rad_min'
          call wtferr(subname, version, errflg,extinfo) 
        ENDIF

        call uclgsr('rad_max',rad_max,ierr)
        IF (ierr.NE.0) THEN
          extinfo ='getting maximum radius for predicted bins,rad_max'
          call wtferr(subname, version, errflg,extinfo) 
        ENDIF    

        call uclgsi('n_rad',n_rad,ierr)
        IF (ierr.NE.0) THEN
          extinfo='getting No. of predicted model bins,n_rad parameter'
          call wtferr(subname, version, errflg,extinfo) 
        ENDIF    

        IF (.NOT.indata) THEN
          call uclgsr('en_min',en_min,ierr)
          IF (ierr.NE.0) THEN
            extinfo='getting minimum energy boundary,en_min parameter'
            call wtferr(subname, version, errflg,extinfo) 
          ENDIF

          call uclgsr('en_max',en_max,ierr)
          IF (ierr.NE.0)  THEN
           extinfo='getting maximum energy boundary,en_max parameter'
           call wtferr(subname, version, errflg,extinfo) 
          ENDIF
        ENDIF

        IF (.NOT.off_hist) THEN
            call uclgsr('off_ang',off_ang,ierr)
            IF (ierr.NE.0) THEN
                extinfo = 'getting off_axis angle parameter'
                call wtferr(subname, version, errflg,extinfo) 
            ENDIF
        ENDIF
          
        call uclgsr('binsize_min',binsize_min,ierr)
        if (ierr .ne. 0) then
            extinfo = 'getting pixel size for rebinning wmap'
            call wtferr(subname, version, errflg,extinfo) 
        endif  
 
        return
        end 		
c ------------------------------------------------------------------
c              END OF SUBROUTINE PSPC_GP  
c ------------------------------------------------------------------

*+PSPC_RINFILE

      subroutine pspc_rinfile(infile,pix_size,file_bkgd,npts,rad_lo,
     >                        rad_hi,radunit,ntheta,theta_lo,theta_hi,
     >                        thetaunit,nenerg,energ_lo,energ_hi,
     >                        energunit,cts_arc2,qerror,err_arc2,
     >                        rpsfunit,qarea,area_wgt,maxrad,maxtheta,
     >                        telrad,instrad,sumrcts,sumtcts,negpres,
     >                        chanmin,chanmax,lbin,bkgd,calc,bkgd_rad,
     >                        ierr,chatter,killit)
c --- DESCRIPTION ---------------------------------------------------------
c
c Reads in Radial profile data from user defined input file. The input file
c is in FITS format. 
c  
c --- VARIABLES -----------------------------------------------------------
c
        IMPLICIT NONE
        character*(*) infile
        character*(*) telrad,instrad
        character*(*) radunit,thetaunit,energunit,rpsfunit
        integer maxrad,maxtheta,ntheta,nenerg
        integer npts,chatter,chanmin,chanmax, ierr
        real pix_size,sumrcts,bkgd
        real rad_lo(*),rad_hi(*),cts_arc2(maxrad,maxtheta,*)
        real err_arc2(maxrad,maxtheta,*),area_wgt(maxrad,maxtheta,*)
        real energ_lo(*),energ_hi(*),theta_lo(*)
        real theta_hi(*),pix,bkgd_rad ,sumtcts
        logical negpres,lbin,qerror,qarea,file_bkgd,calc, killit
c
c --- INTERNAL VARIABLES ---
c
        character(200) subinfo
        character(30) comm
        character(20) instr(50),outhdu(9,50),extname(50),outver(9,50)
        character(20) extnames(9,50)
        character(16) hduclas3
        integer iunit,status,i,ninstr,nsearch,next(50),wterr
        real sumcts,sumpix,sumpix2,sumrcts_bkgd,rawcts
        real area,pixels,sumpix_bkgd,per_bkgd,pi,theocts
c
c --- VARIABLE DIRECTORY ---
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
c subroutine PSPC_REBIN   : Rebins radial profile data if necessary. That
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
c Rehana Yusaf (1995 Jan 12) 1.0.3; bkgd is passed back to main
c Rehana Yusaf (1995 Mar 1) 1.0.4; If Calc then calculate bkgd and
c                                  recalculate sumrcts
c Rehana Yusaf (1995 May 2) 1.0.5; If calc then if calculated bkgd value
c                                  gives a -ve sumtcts then adjust bkgd
c                                  so that sumtcts is 1
c Rehana Yusaf (1995 Nov 3) 1.0.6; add error flag to internal write
c
c Banashree Mitra Seifert (1996, Jan) 2.0.0:
c            . replaced by MVEXT
c            . to carry dimension from calling routine
c            . modification in calculation of SUMCTS
c            . Replaced pi by pi = 4.* atan(1.) instead of 3.14159...
c            . added conditions of 
c                          chanmin=0, chanmax=0 
c                                   or
c                          chanmin > chanmax
c            . Introduced screen display routines 
c              wtinfo,wtferr
c Rehana Yusaf (Jan 23 1996) 2.0.1; add theocts
c            . prev => sumtcts = sumcts - sumpix2*bkgd
c            . now  => sumtcts = theocts - sumpix2*bkgd
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '2.0.1')
        character(14) subname
*-  
        subname='pspc_rinfile'
        subinfo = 'using '//subname//' '//version
        call wtinfo(chatter,10,1,subinfo)

        pi = 4.* atan(1.)

        status = 0
        ninstr = 2
        instr(1) ='RESPONSE'
        instr(2) = 'RPRF'
        nsearch = 50
       call mvext (0, infile, iunit, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, ierr, chatter)

       if (ierr .ne. 0) then
           if (ierr .eq. 2) then
               subinfo='more than one extension is found'
               call wtferr(subname, version, ierr,subinfo) 
           else
               subinfo='error in searching for PSF extension'
               call wtferr(subname, version, ierr,subinfo) 
               subinfo = 'RPSF data is not present'
               call wtferr(subname, version, ierr,subinfo) 
               call ftclos(iunit,status)
               subinfo = 'closing PSF obs file'
               call wtferr(subname, version, ierr,subinfo) 
               return
           endif
       else
           subinfo='extension with PSF data found'
           call wtinfo(chatter, 10, 2, subinfo) 
       endif

c ------------------------ READ DATA  -----------------------------
       pix = 0.0
       call rdrpf1(iunit,hduclas3,npts,rad_lo,rad_hi,radunit,ntheta,
     >             theta_lo,theta_hi,thetaunit,nenerg,energ_lo,
     >             energ_hi,energunit,cts_arc2,qerror,err_arc2,
     >             rpsfunit,qarea,area_wgt,telrad,instrad,maxrad,
     >             maxtheta,ierr,chatter)

        IF (ierr.NE.0) THEN
            subinfo = 'reading RPSF file'
            call wtferr(subname, version, ierr,subinfo) 
            return
        ENDIF

c       <<<--- READ PIXEL SIZE--->>>

        status = 0
        call ftgkye(iunit,'PIXSIZE',pix,comm,status)
        if (status .ne. 0) then
            subinfo = 'reading PIXSIZE '
            call wtferr(subname, version,status,subinfo) 
            return
        endif
        IF (pix.EQ.(0.0)) THEN
            subinfo = 'PIX_SIZE not present in rad profile'
            call wtferr(subname, version,status,subinfo) 
            ierr = 1
            return
        ENDIF

c       <<<--- Converting pixsize from degrees to arcmin/pixel --->>>

         pix_size = pix * 60

c       <<<--- READ SUMRCTS --->>>

        status = 0
        call ftgkye(iunit,'SUMRCTS',sumrcts,comm,status)
        subinfo = 'reading SUMRCTS'
        IF (status.NE.0) THEN
            call wtferr(subname, version, status,subinfo) 
            ierr = 1
            return
        ENDIF        

c       <<<--- READ SUMTCTS --->>>

        status = 0
        call ftgkye(iunit,'SUMTCTS',sumtcts,comm,status)
        subinfo = 'reading SUMTCTS'
        IF (status.NE.0) THEN
            call wtferr(subname, version, status,subinfo) 
            ierr = 1
            return
        ENDIF

c       <<<--- READ CHANMIN AND CHANMAX --->>>

        status = 0
        call ftgkyj(iunit,'CHANMIN',chanmin,comm,status)
        subinfo = 'reading CHANMIN ,min PI channel'
        IF (status.NE.0) THEN
            call wtferr(subname, version, status,subinfo) 
            ierr = 1
            return
        ENDIF

        status = 0
        call ftgkyj(iunit,'CHANMAX',chanmax,comm,status)
        subinfo = 'reading CHANMAX,min PI channel'
        IF (status.NE.0) THEN
            call wtferr(subname, version, status,subinfo) 
            ierr = 1
            return
        ENDIF

        if((chanmin .eq. 0) .and. (chanmax .eq. 0)) then
            subinfo='values of both chanmin and chanmax are zero'
            call wtferr(subname, version, status,subinfo)
            subinfo='this program requires they have some values'
            call wtferr(subname, version, status,subinfo)
            return  
        endif

        if(chanmin .gt. chanmax) then
            subinfo='chanmin is > chanmax'
            call wtferr(subname, version, status,subinfo)
            return  
        endif

c       <<<--- READ BACKGROUND --->>>

        IF (file_bkgd) THEN
            status = 0
            call ftgkye(iunit,'BACKGRND',bkgd,comm,status)
            subinfo = ' reading BACKGRND value'
            IF (status.NE.0) THEN
                call wtferr(subname, version, status,subinfo) 
                ierr = 1
                return
            ENDIF
        ENDIF 
        status = 0
        call ftclos(iunit,status)

c  <<<--- IF calc then calculate background and recalculate SUMRCTS --->>>

        IF (calc) THEN
          IF (bkgd_rad.GE.rad_hi(npts)) THEN
              subinfo='ERROR:Inner radius for bkd calculation is >'
     &                 //' Outer radius' 
              ierr = 1
              call wtferr(subname, version, ierr,subinfo) 
              return
          ENDIF
          per_bkgd = bkgd_rad/REAL(rad_hi(npts))
          IF (per_bkgd.GE.(0.75)) THEN
              subinfo='the inner radius for bkgd calculation is at a'
     >                //' distance > 75% of the total radius from the'
     >               //' center'
              call wtwarm(subname, version,chatter,1,subinfo) 
          ENDIF
        ENDIF
c Here one line is shifted down in modification version 2.0.0
          sumcts = 0
          theocts = 0
          sumrcts_bkgd = 0
          sumpix = 0
          sumpix2 = 0
          sumpix_bkgd = 0
          do i=1,npts
            area = pi * (rad_hi(i)**2 - rad_lo(i)**2)
            pixels = area_wgt(i,1,1) * area * (1/(pix_size**2))
            sumpix = sumpix + pixels
            rawcts = cts_arc2(i,1,1) * pixels * pix_size**2
            sumcts = sumcts + rawcts
            theocts = theocts + rawcts/area_wgt(i,1,1)
            IF ((rad_lo(i).GE.bkgd_rad).AND.(calc)) THEN
                sumpix_bkgd = sumpix_bkgd + pixels
                sumrcts_bkgd = sumrcts_bkgd + rawcts
            ENDIF
          enddo
          sumpix2 = (pi * (rad_hi(npts)**2-rad_lo(1)**2))
c The next line is modification 
        IF ((calc).OR.(.NOT.file_bkgd)) THEN
          IF (calc) THEN
          IF (sumrcts_bkgd.GE.0.0) THEN
           bkgd = sumrcts_bkgd/sumpix_bkgd
          ELSE
           subinfo='the sum of the counts in the bkgd region is < 0'
           call wtwarm(subname, version,chatter,1,subinfo) 
           subinfo='the bkgd has been set to 0'
           call wtwarm(subname, version,chatter,1,subinfo) 
           bkgd = 0
          ENDIF
          ENDIF
          sumrcts = sumcts - (sumpix * bkgd)
          sumpix2 = (pi * (rad_hi(npts)**2-rad_lo(1)**2))
     &               *1/(pix_size**2)
          sumtcts = theocts - (sumpix2*bkgd)
      IF (calc) THEN
        IF (sumtcts.LE.0.0) THEN
          bkgd = (sumcts - 1)/sumpix2
          sumtcts = 1
          sumrcts = sumcts - sumpix*bkgd
          subinfo ='the background value has been calculated such'
     &              //' that the sum of the'
          call wtinfo(chatter,1,3,subinfo) 
          subinfo='theoretical counts is set to 1. If the background'
     &             //' is calculated'
          call wtinfo(chatter,1,3,subinfo) 
          subinfo =' in the usual way then sumtcts is -ve, this may be'
     &              //' due to excluded regions.'
          call wtinfo(chatter,1,3,subinfo) 
        ENDIF
      ENDIF
      write(subinfo,'(a,f20.9)',IOSTAT=wterr) 
     & ' Calculated bkgd value :',bkgd
      IF (calc) THEN
           call wtinfo(chatter,15,2,subinfo) 
      ENDIF
      ENDIF

      wterr = 0
      IF (chatter.GE.20) THEN
          write(subinfo,'(a,F20.9)',IOSTAT=wterr)
     &      'Sum of pixels calculated using area of circle:',sumpix2
          call wtinfo(chatter,9,3,subinfo) 
          write(subinfo,'(a,F20.9)',IOSTAT=wterr)
     &          ' Actual sum of pixels :',sumpix
          call wtinfo(chatter,9,3,subinfo) 
      ENDIF
      IF ((sumpix/sumpix2).LE.(0.9)) THEN
           subinfo='actual sum of pixels is < 90% of sum of pixels'
     >     //' calculated. So, using area of circle where the outer '//
     >             'radius is used'
           call wtwarm(subname,version,chatter,15,subinfo) 
           subinfo = 'NOTE: This may be due to regions being excluded'
           call wtinfo(chatter,15,3,subinfo) 
      ENDIF


           write(subinfo,'(a,F20.9)',IOSTAT=wterr)
     &           ' Total sum of counts :',sumcts
           call wtinfo(chatter,20,2,subinfo) 
           write(subinfo,'(a,F20.9)',IOSTAT=wterr)
     &           ' Counts in source :',sumrcts
           call wtinfo(chatter,20,2,subinfo) 
           write(subinfo,'(a,f20.9)',IOSTAT=wterr)
     &           ' Theoretical sum of counts,'
     >           //' (corrected for any excl regions):',sumtcts
           call wtinfo(chatter,20,2,subinfo) 


      IF (sumrcts.LT.0) THEN
          subinfo = 'Sum of counts is negative'
          call wtwarm(subname,version,chatter,20,subInfo) 
          subinfo=' this may be due to background dominating'
     &              //' the source'
          call wtwarm(subname,version,chatter,20,subInfo) 
          ierr = 1
          return
      ENDIF

      IF ((sumrcts/sumcts).LE.(0.1)) THEN
          subinfo ='source is less than 10% of total counts !'
          call wtwarm(subname,version,chatter,9,subinfo) 
      ENDIF

      return
      end

c -----------------------------------------------------------------------
c              END OF SUBROUTINE PSPC_RINFILE        
c -----------------------------------------------------------------------

*+PSPC_RPHAFILE

      subroutine pspc_rphafile(phafile,rmffile,en,cts,npha,maxen,
     >                         telpha,instpha,telrmf,instrmf,pha,rmf,
     >                         ierr,chatter)
c
c --- DESCRIPTION -----------------------------------------------------
c
c This routine obtains the PHA data : Channel and Counts/Channel, and
c also RMF data : Channel,e_min and e_max. It then finds corresponding
c energy values for channels in the desired format.
c 
c --- VARIABLES -------------------------------------------------------
c
        IMPLICIT NONE
        character*(*) phafile,rmffile
        character*(*) telpha,instpha,telrmf,instrmf
        integer maxen,wterr, npha,chatter,ierr
        real en(*),cts(*)
        logical pha,rmf

c --- INTERNAL VARIABLES ---

        character(100) subinfo
        character(20) instr(50),outhdu(9,50),outver(9,50),extname(50)
        character(20) extnames(9,50)
        integer ninstr, nsearch, next(50), errflg
        integer i,ncts
        integer rmfchan(1000),chan(1000)
        real e_min(1000),e_max(1000)

c ------------ variables for calling rdpha1 --------------------------
      character(80) comm        
      character(30) filter,chantyp 
      character(30) xflt(50)
      character(180) backfil,corfil,rmffil,dmode,hduclas2
      character(180) arffil,detnam,phaversn
      integer iunit,maxpha,dtype,n_xflt, status, fchan,detchan
      integer iget, ineed, max_xflt
      integer p_group,p_qualty,p_ipha, p_error,p_sysfrc,p_rpha
      real texpos,areascal,backscal,corscal
      logical qgroup,qqual,qerror,qsys,pois

c
c --- VARIABLE DIRECTORY ---
c
c Arguments ...
c
c phafile    char   : Name of phafile
c rmffile    char   : Name of rmf file
c telpha     char   : Telescope name from phafile
c instpha    char   : Instrument name from phafile
c telrmf     char   : Telescope name from rmffile
c instrmf    char   : Instrument name from rmffile
c maxen      int    : Maximum size of arrays for pha data
c en         real   : Array of energies
c cts        real   : Array of counts per channel
c ierr       int    : Error flag, 0 is okay
c
c Internals ...

c e_min      real   : Array of minimum energies for channels
c e_max      real   : Array of maximum energies for channels
c chan       int    : Array of channels from PHA file
c rmfchan    int    : Array of counts from RMF file
c curchan    int    : Used for current channel in search loop
c i,j        int    : Counters for loops
c ncts       int    : counter for No of rmf data values
c anyflg     logical: True if any data undefined in FITS file
c findchan   logical: False when corresponding energy for channnel found
c
c
c --- CALLED ROUTINES ---
c
c subroutine PSPC_RPHA    : Reads Pha data,from SPECTRUM extension in PHA 
c                           file that is channel, and counts/channel.
c subroutine PSPC_RRMF    : Reads EBOUNDS extension in RMF file, that is
c                           channel, and (nominal) energy boundaries for
c                           each of the channels.
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (January 15 1993)
c Rehana Yusaf (1994 Jan 29) 1.0.1; PHAFILE is NOT always used for 
c                                   weighting - this routine only reads
c                                   the phafile if pha.eq.true
c                                   If the user is only looking at the
c                                   theoretical PSF then the RMFFILE is
c                                   not needed - rmf file only read if
c                                   rmf.eq.true 
c
c Banashree Mitra Seifert (1996, Jan) 2.0.0:
c            . Introduced DMA
c            . Added another subroutine CALC_CTS for PHA calculation 
c            . Replaced by MVEXT
c            . To carry dimension from calling routine
c            . Replaced pspc_rpha subroutine by calling to RDPHA1
c            . Introduced screen display routines 
c              wtinfo,wterrm,wtferr
c Peter D Wilson (June 29, 1998) 2.0.1:
c            . Added max_xflt parameter to rdpha1 function call
c Peter D Wilson (June 30, 1998) 2.0.2:
c            . Added abort code when error encountered in opening phafile
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '2.0.2')
        character(14) subname 
*- 
c --- DYNAMIC MEMORY ALLOCATION ---
c      integer group(maxpha),qualty(maxpha),ipha(maxpha)
c      real error(maxpha),sysfrc(maxpha),rpha(maxpha)
c
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

      subname='pspc_rphafile'
      subinfo= 'using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)

      maxpha = 1000

c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_error = 0
      p_sysfrc = 0
      p_rpha = 0
      p_group = 0
      p_qualty = 0
      p_ipha = 0

      iget=0
      status = 0
      call udmget(maxpha, 6, p_error, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 6, p_sysfrc, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 6, p_rpha, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_group, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_qualty, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_ipha, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4


 50   ineed = 6*maxpha*4 
      write(subinfo, '(a,i6)')'more DMAsize needed for RPHAFILE=', 
     >                          ineed
      call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i6)')'total bytes of memory I get more=',iget
      call wtinfo(chatter,10,1,subinfo)

      if (status .ne. 0) then
         ierr = -1
         subinfo=' failed to allocate dynamic memory '
         call wterrm(subname,version,subinfo)
         return
      endif

        IF (pha) THEN

            status = 0
            ninstr= 1
            instr(1) = 'SPECTRUM'
            nsearch = 1
            call mvext(0, phafile,iunit, ninstr, instr, nsearch, next,
     >                 outhdu,extnames,outver,extname,errflg,chatter)

            if (errflg .ne. 0) then
                subinfo='locating SPECTRUM data'
                call wterrm(subname,version,subinfo)
                call ftclos(iunit, status)
                subinfo='closing PSF file'
                call wtferr(subname,version,status,subinfo)
C PDW 6/30/98: Abort if error
                ierr = 1
                return
            endif

c -------------------------- READ NAXIS2 ----------------------------

          call ftgkyj(iunit,'NAXIS2',npha,comm,status)
          subinfo = 'KEYWORD = NAXIS2'
          call wtferr(subname,version,status,subinfo)
          IF (status.NE.0) THEN
            ierr = 1
            return
          ENDIF
c ---------------------------------------------------------------------+
            max_xflt=50
            call rdpha1(iunit,maxpha,npha,telpha,instpha,detnam,
     >                  filter,detchan,texpos,areascal,backscal,
     >                  corscal,backfil,corfil,rmffil,arffil,xflt,
     >                  max_xflt,
     >                  n_xflt,dmode,chantyp,phaversn,hduclas2,fchan,
     >                  chan,dtype,MEMI(p_ipha),MEMR(p_rpha),qerror,
     >                  MEMR(p_error),qsys,MEMR(p_sysfrc),
     >                  qqual,MEMI(p_qualty),qgroup,MEMI(p_group),pois,
     >                  ierr,chatter)

            IF (ierr.NE.0) THEN
                return
            ENDIF

            call calc_cts(npha,MEMI(p_ipha),MEMR(p_rpha),cts,dtype,
     >                    texpos,chatter)

        ENDIF

        IF (rmf) THEN
            call pspc_rrmf(rmffile,rmfchan,e_min,e_max,ncts,maxen,
     >                     telrmf,instrmf,ierr,chatter)

            IF (ierr.NE.0) THEN
                return        
            ENDIF
        ENDIF

        wterr = 0
        IF (rmf.AND.pha) THEN
          IF (npha.NE.ncts) THEN
            subinfo = ' RMF/PHA Channel mismatch'
            call wterrm(subname,version,subinfo)
            write(subinfo,'(1X,a,i12)',IOSTAT=wterr)
     &            ' No of PHA channels :',npha
            call wtinfo(chatter,0,1,subinfo)
            write(subinfo,'(1X,a,i12)',IOSTAT=wterr)
     &            ' No of RMF channels :',ncts
            call wtinfo(chatter,0,1,subinfo)
            ierr = 1
            return
          ENDIF
        ENDIF

c       <<<--- FIND CORRESPONDING ENERGY VALUES FOR CHANNELS --->>>
c       <<<--- NOTE THIS MAY BE CHANGED TO USE FTOOL --->>> 
       
        IF (rmf) THEN
            do i=1,ncts
               en(i)=(e_max(i) + e_min(i))/REAL(2)
            enddo
        ENDIF


c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_error, 6, status)
      status = 0
      call udmfre(p_sysfrc, 6, status)
      status = 0
      call udmfre(p_rpha, 6, status)
      status = 0
      call udmfre(p_group, 4, status)
      status = 0
      call udmfre(p_qualty, 4, status)
      status = 0
      call udmfre(p_ipha, 4, status)

      if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wtferr(subname,version,status,subinfo)
          ierr=99
      endif
 
      return
      end
c ---------------------------------------------------------------------+
c              END OF SUBROUTINE PSPC_RPHAFILE             
c ---------------------------------------------------------------------+

*+PSPC_RRMF
       subroutine pspc_rrmf(rmffile,rmfchan,e_min,e_max,ncts,
     &                      maxen,telescop,instrume,ierr,chatter)
c
c --- DESCRIPTION -----------------------------------------------------
c
c Reading RMF file, in order to convert channel data read from phafile
c into energies. EBOUNDS extension is read, The following columns are
c read ...
c
c CHANNEL : Channels
c E_MIN   : Minimum energy for channel, in keV
c E_MAX   : Maximum energy for channel, in keV
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
        IMPLICIT NONE
        character*(*) telescop,instrume
        character*(*) rmffile
        integer maxen,ncts,chatter,ierr
        integer rmfchan(*)
        real e_min(*),e_max(*)
c
c --- INTERNAL VARIABLES ---
c
        integer snum,maxcol
        parameter (maxcol =8)
        character(70) text,errtxt,subinfo
        character(40) comm
        character(8) colomns(maxcol)
        character(20) instr(50),outhdu(9,50),outver(9,50), extname(50)
        character(20) extnames(9,50)
        integer ninstr,nsearch,next(50)
        integer status,iunit,errflg
        integer felem,inull,colnum,ncols,frow,j
        real enull
        logical anyflg,foundcol

c
c --- VARIABLE DIRECTORY ---
c
c Arguments ...
c
c rmffile    char   : Name of RMF file 
c maxen      int    : Maximum size of arrays
c e_min      real   : Array of minimum energies for channels in keV
c e_max      real   : Array of maximum energies for channels in keV
c rmfchan    int    : Array of channels from RMF file 
c ncts       int    : Counter for No. of relevant rows in RMF file
c telescop   char   : Mission/Telescope name
c instrume   char   : Instrument name
c ierr       int    : Error flag, 0 is okay
c
c Internals ...
c
c extname    char   : Used to read value of extension name in FITS header
c colomns    char   : Array containing colomn names in FITS file
c errtxt     char   : Error text obtained from FITSIO call
c enull      real   : Used to represent undefined values in FITS file
c status     int    : Error flag for FITSIO calls
c iunit      int    : Fortran unit number for file
c block      int    : FITSIO record blocking factor
c nhdu       int    : Current header number
c htype      int    : Type of header unit in FITS file
c felem      int    : First pixel of element array in FITS file
c frow       int    : Beginning row No. in FITS file
c inull      int    : Used to represent undefined values in FITS file
c colnum     int    : No. of colomn
c i,j        int    : Counters for loops
c anyflg     logical: True if any data undefined in FITS file
c foundcol   logical: True if desired colomn found in FITS file
c
c --- CALLED ROUTINES ---
c
c subroutine FTOPEN      : FITSIO routine to open a FITS file
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYs      : FITSIO routine to read extension header keyword,
c                          where the s ,is for string 
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c                          where the j, is for an integer
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c                          where a rootstring is given, thus an array of
c                          keywords can be read
c subroutine FTGCVj      : FITSIO routine to read colomns of data in an
c                          extension. The j indicates that it is int data.
c subroutine FTGCVe      : FITSIO routine to read colomns of data in an
c                          extension. The e is for real data
c subroutine FTGERR      : FITSIO routine, which gives error text for a 
c                          given error flag.
c subroutine FTCLOS      : FITSIO routine to close FITS file. 
c 
c
c --- COMPILATION AND LINKING ---
c
c Link with FITSIO
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (1993 January)
c Rehana Yusaf (1994 January 29) 1.0.1; Use fndhdu and fndext to locate
c                                       RMF extension
c Banashree Mitra Seifert (1996, Jan) 2.0.0:
c            . replaced by MVEXT
c            . to carry dimension from calling routine
c            . Introduced screen dislay routines
c              wtinfo,wterrm,wtferr
c ----------------------------------------------------------------------
c
        character(5) version
        parameter (version ='2.0.0')
        character(10) subname 
*-
c       <<<--- USER INFO --->>>

        subname = 'pspc_rrmf'
        subinfo = 'using '//subname//' '//version
        call wtinfo(chatter,10,1,subinfo)

c --- FIND EBOUNDS EXTENSION ---

      status = 0
      errflg=0
      ninstr=1
      extname(1)='EBOUNDS'
      nsearch = 50

       call mvext (0, rmffile, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, errflg, chatter)
       if (errflg .ne. 0) then
           subinfo='RMF extension not found'
           call wterrm(subname,version,subinfo)
           status=0
           call ftclos(iunit,status)
           subinfo = 'closing RMF file'
           call wterrm(subname,version,subinfo) 
           return
       endif

c ---------------------- READING RMF DATA -------------------------

           status=0
c --- READ NAXIS2
           call ftgkyj(iunit,'NAXIS2',ncts,comm,status)
           text = 'looking for NAXIS2 '
           IF (status.NE.0) THEN
           call wtferr(subname,version,status,text) 
               ierr = 1
               return
           ENDIF
           status=0
c --- READ TTYPE
           snum = 1
           call ftgkns(iunit,'TTYPE',snum,maxcol,colomns,ncols,status)
           text = 'looking for TTYPE'
           IF (status.NE.0) THEN
               call wtferr(subname,version,status,text) 
               ierr = 1
               return
           ENDIF
           status=0
c --- READ TELESCOPE
           call ftgkys(iunit,'TELESCOP',telescop,comm,status)
           text = 'KEYWORD = TELESCOP'
           IF (status.EQ.207) THEN
               call wtferr(subname,version,status,text) 
               telescop = 'UNKNOWN'
           ENDIF
c --- READ INSTRUME
           status = 0
           call ftgkys(iunit,'INSTRUME',instrume,comm,status)
           text = 'KEYWORD = INSTRUME'
           call wtferr(subname,version,status,text) 
           IF (status.EQ.207) THEN
               instrume = 'UNKNOWN'
           ENDIF

c       <<<--- CHECK TO FIND CHANNEL COLUMN IN RMF FILE --->>>
        
           foundcol=.false.
           do j=1,ncols
              IF (colomns(j).EQ.'CHANNEL ') THEN
                  colnum=j
                  foundcol=.true.
              ENDIF
           enddo    
           IF (.NOT.foundcol) THEN
               errtxt='channel column not present in rmf file'
               call wterrm(subname,version,errtxt) 
               errtxt = 'in EBOUNDS ext'
               call wterrm(subname,version,errtxt) 
               call wrcols(ncols,colomns,maxcol)
               ierr = 1
               return
           ENDIF
c
c       <<<--- READING CHANNEL DATA FROM RMF FILE --->>>
c 
           felem=1
           frow=1
           inull=0
           status=0
           call ftgcvj(iunit,colnum,frow,felem,ncts,inull,rmfchan,
     &                 anyflg,status)
           text= 'reading RMF channel data'
           IF (status.NE.0) THEN
               call wterrm(subname,version,text) 
               ierr = 1
               return
           ENDIF
           status=0

c      <<<--- CHECK TO FIND E_MIN COLUMN IN RMF FILE --->>>

           foundcol=.false.
           do j=1,ncols
             IF (colomns(j).EQ.'E_MIN   ') THEN
                 colnum=j
                 foundcol=.true.
             ENDIF
           enddo
           IF (.NOT.foundcol) THEN
               errtxt =' E_MIN column not present in rmf file'
               call wterrm(subname,version,errtxt) 
               errtxt = ' IN EBOUNDS EXT '
               call wterrm(subname,version,errtxt) 
               call wrcols(ncols,colomns,maxcol)
               ierr = 1 
               return
           ENDIF

c      <<<--- READING CORRESPONDING MINIMUM ENERGY --->>>

           felem=1
           frow=1
           enull=0
           status=0
           call ftgcve(iunit,colnum,frow,felem,ncts,enull,e_min,
     &                 anyflg,status)
           text = 'reading E_MIN column'
           IF (status.NE.0) THEN
           call wterrm(subname,version,text) 
             ierr = 1
             return
           ENDIF

c      <<<--- CHECK TO FIND E_MAX COLUMN IN RMF FILE --->>>

           foundcol=.false.
           do j=1,ncols
             IF (colomns(j).EQ.'E_MAX   ') THEN
               colnum=j
               foundcol=.true.
             ENDIF
           enddo
           IF (.NOT.foundcol) THEN
               errtxt  ='E_MAX column not present in rmf file'
               call wterrm(subname,version,errtxt) 
               errtxt ='in EBOUNDS ext '
               call wterrm(subname,version,errtxt) 
               ierr = 1
               return
           ENDIF           

c      <<<--- READING CORRESPONDING MAXIMUM ENERGY --->>>

           felem=1
           frow=1
           enull=0
           status=0 
           call ftgcve(iunit,colnum,frow,felem,ncts,enull,e_max,
     &                 anyflg,status)
           text = 'reading E_MAX column'
           IF (status.NE.0) THEN
           call wterrm(subname,version,text) 
             ierr = 1
             return
           ENDIF
        status=0
        call ftclos(iunit,status)
        text = 'closing RMF file'
        call wtferr(subname,version,status,text) 
        return
        end
c -------------------------------------------------------------------
c              END OF SUBROUTINE PSPC_RRMF        
c -------------------------------------------------------------------

*+PSPC_PRED
       subroutine pspc_pred(indata,pha,rmf,en,cts,npha,x,rad_min,
     >                      rad_max,n_rad,pred,en_min,en_max,bkgd,
     >                      pix_size,maxen,maxpred,maxtheta,
     >                      theta_min,theta_max,sohist,nhist,off_hist,
     >                      theo_fn,chatter,off_ang,errf,sumtcts,
     >                      chanmin,chanmax,ierr)
c --- DESCRIPTION --------------------------------------------------------
c
c This subroutine calculates the theoretical radial psf. The function 
c psfoff called by this routine essentially does the calculation. This 
c routine prepares the data for the function. 
c The counts, cts from Phafile are used as weighting factors for the 
c theoretical psf, within the user defined energy boundarys, en_min and
c en_max respectively.
c 
c
c ------------------------------------------------------------------------
c
c --- VARIABLES ---
c
        character*(*) theo_fn
        integer maxen,maxpred,maxtheta,chanmin,chanmax
        integer n_rad,npha,chatter,errf,nhist
        real en(*),cts(*),pred(maxpred,maxtheta,*),x(*)
        real rad_min,rad_max,en_min,en_max,bkgd,pix_size,sumtcts,off_ang
        real theta_min(*),theta_max(*),sohist(*)
        logical off_hist,indata,pha,rmf
c
c --- INTERNAL VARIABLES ---
c
        character(50) subinfo
        character(5) cnum1,cnum2
        integer i,j,enst,enfin,ierr,k,wterr
        real sum,rad_del,psfoff,psf,psf_lo,psf_hi,en_step,cur_en
        logical findlim,energ_lim
c
c --- VARIABLE DIRECTORY ---
c
c Arguments ...
c
c maxen      int    : Maximum size of arrays used for pha data
c maxpred    int    : Maximum size of arrays used for predicted radial psf
c en         real   : Array of Energies
c cts        real   : Array of counts/channel for pha data
c pred       real   : Array of predicted radial psf
c x          real   : Array of angles from target position
c rad_min    real   : Minimum radius for predicted model (user defined)
c rad_max    real   : Maximum radius for predicted model (user defined)
c en_min     real   : Minimum energy boundary (user defined)
c en_max     real   : Maximum energy boundary (user defined)
c bkgd       real   : Background count rate (user defined),cts/pixel
c pix_size   real   : pixel size 
c n_rad      int    : No. of predicted model bins (user defined)
c npha       int    : No. of pha data values
c off_ang    real   : off_axis angle
c errf       int    : Error flag
c
c Internals ...
c
c sum        real   : Sum of cts
c rad_del    real   : Radius for predicted model bins
c enst       int    : Starting channel for weighting pred, corresponds
c                     to en_min
c enfin      int    : Finishing channel for weighting pred,corresponds
c                     to en_max
c cnum1      char   : String used to contain enst, for o/p
c cnum2      char   : String used to contain enfin, for o/p 
c ierr       int    : error flag for psf function
c i,j        int    : Counters for loops
c
c --- CALLED ROUTINES ---
c 
c function PSFOFF           : Calculates off-axis psf.
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (January 15)
c Rehana Yusaf (1993 June 9) : Substitute off_axis function for on_axis one
c Rehana Yusaf (1994 Jan 31) 1.0.2; Update so that if phafile not entered
c                              then uniform weighting.
c                              If .NOT.indata and not RMF then an energy step 
c                              of 10 eV is used from en_min to en_max     
c Rehana Yusaf (1994 Feb 8) 1.0.3; If NOT pha still weight with sumrcts
c Rehana Yusaf (1994 Oct 18) 1.0.4; logical DO WHILE(energylim) for theo
c                                   curve only, should be DO WHILE(.NOT.
c Banashree Mitra seifert (1996, Jan) 1.1.0:
c             . Carried dimensions from calling routine
c             . Introduced new screen display routine 
c               wterrm,wtinfo 
c Peter D. Wilson (1998, Mar) 1.1.1:
c             . Use center of ring instead of inner edge for calculating
c               PSF function
c             . Changed condition for finding enfin:
c                      en(i).GE.en_max --> en(i+1).GT.en_max
c -----------------------------------------------------------------------
        character(5) version
        parameter (version = '1.1.1')
        character(10) subname 
*- 
c ----------------------------------------------------------------------   
c
c       <<<--- USER INFO --->>>
c
        subname='pspc_pred'
        subinfo = 'using '//subname//' '//version
        call wtinfo(chatter,10,1,subinfo)

c       <<<--- PREPARE DATA FOR PSF --->>>
        en_step =0.0

        rad_del = (rad_max - rad_min)/REAL(n_rad)
        do i=1,n_rad
           x(i) = rad_min + (i-0.5) * rad_del
        enddo

        IF (indata) THEN
          enst = chanmin
          enfin = chanmax
        ENDIF
c
c       <<<--- USING USER DEFINED ENERGY BOUNDARYS --->>>
c    
         IF (.NOT.indata.AND.rmf) THEN
         i=1
         findlim=.false.
         do WHILE (.NOT.findlim)
          IF (en(i).GE.en_min) THEN
            enst=i
            findlim=.true.
          ENDIF
          i=i+1
         enddo
         i=1
         findlim=.false.
         do WHILE (.NOT.findlim)
          IF (en(i+1).GT.en_max) THEN
            enfin=i
            findlim=.true.
          ENDIF
          i=i+1
          IF (i.EQ.npha) THEN
            findlim=.true.
            enfin=npha
          ENDIF
         enddo
        ENDIF
c
c       <<<--- MORE USER INFO FOR NOISY CHATTINES --->>>
c
        wterr = 0
        IF (pha) THEN
         IF (chatter.GE.20) THEN
          write (cnum1,100,IOSTAT=wterr) enst
          write (cnum2,100,IOSTAT=wterr) enfin
          subinfo = 'PHA Channels used for weighting pred '//cnum1//
     &              ':'//cnum2
          call wtinfo(chatter,20,2,subinfo)
         ENDIF
        ENDIF
c
c       <<<--- CALCULATE PREDICTED PSF USING FUNCTION --->>>
c
        theo_fn = 'GRH (OFF-AXIS) PSF ,Apr 1993'
        IF (rmf) THEN
          do i=enst,enfin
            IF ((en(i).GE.0.07).AND.(en(i).LE.3)) THEN
              do j=1,n_rad
                IF (off_hist) THEN
                  do k=1,nhist
                    ierr = 0
                    psf_lo= psfoff(en(i),theta_min(k),x(j)*60,
     &                             ierr)*3600
                    psf_hi=psfoff(en(i),theta_max(k),x(j)*60,
     &                             ierr)*3600
                    IF (ierr.NE.0) THEN
                      subinfo = 'calculating PSF'
                      call wterrm(subname,version,subinfo)
                      errf = ierr
                      return
                    ENDIF                 
                    psf=(psf_lo+psf_hi)/REAL(2)
                    IF (pha.AND.(cts(i).GT.0)) THEN
                     pred(j,1,1)=pred(j,1,1)+cts(i)*psf*sohist(k)
                    ELSE
                     pred(j,1,1)=pred(j,1,1)+psf*sohist(k)
                    ENDIF
                  enddo
                ELSE
                  IF (pha.AND.(cts(i).GT.0)) THEN
                    pred(j,1,1)=pred(j,1,1)+cts(i)*psfoff(en(i),
     &                        off_ang,x(j)*60,ierr)*3600
                  ELSE
                    pred(j,1,1)=pred(j,1,1)+psfoff(en(i),
     &                        off_ang,x(j)*60,ierr)*3600   
                  ENDIF
                  IF (ierr.NE.0) THEN
                   subinfo = 'calculating PSF'
                      call wterrm(subname,version,subinfo)
                   errf = ierr
                   return
                  ENDIF
                ENDIF
              enddo
            ENDIF
          enddo
        ELSE
          energ_lim = .false.
          en_step = 0.01
          cur_en = en_min
          do WHILE(.NOT.energ_lim)
           IF ((cur_en.GE.0.07).AND.(cur_en.LE.3)) THEN
            do j=1,n_rad
              IF (off_hist) THEN
                 do k=1,nhist
                    psf_lo= psfoff(cur_en,theta_min(k),x(j)*60,
     &                             ierr)*3600
                    psf_hi=psfoff(cur_en,theta_max(k),x(j)*60,
     &                             ierr)*3600
                    IF (ierr.NE.0) THEN
                      subinfo = 'calculating PSF'
                      call wterrm(subname,version,subinfo)
                    ENDIF
                    psf=(psf_lo+psf_hi)/REAL(2)   
                    pred(j,1,1)=pred(j,1,1)+psf*sohist(k)
                 enddo
              ELSE
                pred(j,1,1) = pred(j,1,1)+psfoff(cur_en,off_ang,
     &                      x(j)*60,ierr)*3600 
                IF (ierr.NE.0) THEN
                   subinfo = 'calculating PSF'
                      call wterrm(subname,version,subinfo)
                   errf = ierr
                   return
                ENDIF   
              ENDIF
            enddo
           ENDIF
           cur_en = cur_en + en_step
           IF (cur_en.GE.en_max) THEN
            energ_lim = .true.
           ENDIF
          enddo
        ENDIF
        sum=0.0
        IF (pha) THEN
          do i=enst,enfin
           IF (cts(i).GT.0) THEN
              sum = sum + cts(i)
           ENDIF
          enddo
        ELSE
          IF (.NOT.rmf) THEN
            sum = ((en_max-en_min)/en_step) + 1
          ELSE
            sum = enfin-enst+1
          ENDIF
        ENDIF
        do j=1,n_rad
          pred(j,1,1)= (sumtcts * pred(j,1,1)/sum) 
          IF (bkgd.NE.(0.0)) THEN
             pred(j,1,1) = pred(j,1,1) + bkgd/(pix_size**2)
          ENDIF
        enddo
 100    format (I4)
        return
        end
c -------------------------------------------------------------------
c              END OF SUBROUTINE PSPC_PRED 
c -------------------------------------------------------------------

*+PSPC_RESULTS
       subroutine pspc_results(outfile,infile,phafile,detfil,off_hist,
     >                         indata,pha,rmf, rmffile,mnver, npts,
     >                         rad_lo,rad_hi,radunit,ntheta,theta_lo,
     >                         theta_hi,thetaunit,nenerg,energ_lo,
     >                         energ_hi,energunit,cts_arc2,qerror,
     >                         err_arc2,rpsfunit,qarea,area_wgt,
     >                         pix_size,x,pred,n_rad,rad_min,rad_max,
     >                         maxrad,maxtheta,maxpred,theo_fn,bkgd,
     >                         en_min,en_max,chanmin,chanmax,xmin,xmax,
     >                         parea_wgt,pred_err,telrad,instrad,
     >                         negpres,lbin,sumrcts,sumtcts,
     >                    off_ang,calc,bkgd_rad,ierr,chatter,killit)
c --- DESCRIPTION ------------------------------------------------------
c
c This subroutine writes observed and thoeretical radial psf to output 
c file. The output file is in FITS format RPSFVER 1993a
c
c ----------------- VARIABLES ------------------------------------

        IMPLICIT NONE
        character*(*) outfile,infile,phafile,rmffile,detfil
        character*(*) theo_fn
        character*(*) telrad,instrad
        character(16) rpsfunit,radunit,thetaunit,energunit,creator
        character(5) mnver
        integer maxrad,maxpred,maxtheta,chanmin,chanmax
        integer ntheta,nenerg, npts,n_rad,chatter,ierr
        real rad_lo(*),rad_hi(*),cts_arc2(maxrad,maxtheta,*)
        real err_arc2(maxrad,maxtheta,*),sumrcts
        real x(*),pred(maxpred,maxtheta,*),off_ang
        real rad_min,rad_max,pix_size,bkgd,en_min,en_max
        real theta_lo(*),theta_hi(*),sumtcts
        real energ_lo(*),energ_hi(*)
        real area_wgt(maxrad,maxtheta,*)
        real xmin(*),xmax(*),parea_wgt(maxpred,maxtheta,*)
        real pred_err(maxpred,maxtheta,*),bkgd_rad
        logical negpres,lbin,qerror,qarea
        logical killit,off_hist,indata,pha,rmf,calc
c
c ---INTERNAL VARIABLES---
c
        character(80) subinfo
        character(16) hduclas3 
        character(8) extname
        integer j,ounit,ierrstat,maxhist,nk_hist
        integer nk_comm,maxcomm
        parameter (maxhist = 10)
        parameter (maxcomm = 10)
        character(80) hist(maxhist),comms(maxcomm)
        real rad_del,pix

c
c --- VARIABLE DIRECTORY ---
c
c Arguments ...
c
c outfile    char   : Name of results file    
c infile     char   : Radial Profile input file
c phafile    char   : Phafile (input file)
c rmffile    char   : Rmffile (input file)
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
c en_min     real   : Energy boundary in keV (user defined)
c en_max     real   : Energy boundary in keV (user defined)
c c_min      real   : Minimum No. of counts/bin (user defined)
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
c subroutine WT_RPSF1993a : CALLIB,Writes observed radial psf data to fits file
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
c
c Rehana Yusaf (1995 April 25) 1.0.3; add clobber (killit)
c         RY    (1995 May 22)  1.0.4; bug-fix, killit defined twice
c
c Banashree Mitra Seifert (1996, Jan) 1.1.0:
c            . Carry the dimension from calling routine
c            . Introduced new screen display routine
c              wtinfo,wtferr
c Banashree Mitra Seifert (1996, April)1.2.0:
c            . modified calculation of xmin(j) and xmax(j)
c ----------------------------------------------------------------------
        character(5) version
        parameter (version = '1.2.0')
        character(14) subname 
*- 

        subname='pspc_results'
        subinfo = 'using '//subname//' '//version
        call wtinfo(chatter,10,1,subinfo)

c       <<<--- MODIFYING OUTPUT INTO DESIRED FORM --->>>

        rad_del = (rad_max - rad_min)/(REAL(n_rad))
        xmin(1)=0.
        xmax(1)=xmin(1)+rad_del
        do j=2,n_rad
           xmin(j) = xmax(j-1)
           xmax(j) = xmin(j)+rad_del
        enddo
c
c       <<<--- WRITING TO FITS FILE --->>>
c

c OBSERVED DATA ...

        call cgetlun(ounit)
        call opnpa(outfile,chatter,ounit,killit,ierrstat)
      IF (indata) THEN
        nk_hist=0
        nk_comm = 1 
        comms(1) = 'data obtained from radial profile : '//infile
        extname = 'OBS RPSF'
        pix = pix_size/REAL(60)
        IF (bkgd.EQ.0) THEN
         hduclas3 = 'NET'
        ELSE
         hduclas3 = 'TOTAL'
        ENDIF

        call wtrpf1(ounit,extname,hduclas3,npts,rad_lo,rad_hi,radunit,
     &                  ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     &                  energ_lo,energ_hi,energunit,cts_arc2,qerror,
     &                  err_arc2,rpsfunit,qarea,area_wgt,hist,nk_hist,
     &                  comms,nk_comm,telrad,instrad,maxrad,maxtheta,
     &                  ierr,chatter)
        ierrstat = 0
        call ftpkye(ounit,'PIXSIZE',pix,8,'pixelsize in deg',
     &               ierrstat)
        subinfo = 'writing PIXSIZE '
        call wtferr(subname,version,ierrstat,subinfo)
        ierrstat = 0
        call ftpkye(ounit,'SUMRCTS',sumrcts,8,
     &             'Sum of source counts under profile',ierrstat)
        subinfo = 'writing SUMRCTS'
        call wtferr(subname,version,ierrstat,subinfo)
        ierrstat = 0
        call ftpkye(ounit,'SUMTCTS',sumtcts,8,
     &'Theoretical Sum source counts(corr for excl regions)',ierrstat)
        subinfo = 'writing SUMTCTS'
        call wtferr(subname,version,ierrstat,subinfo)
        ierrstat = 0
        call ftpkyj(ounit,'CHANMIN',chanmin,'Minimum PI channel',
     &              ierrstat)
        subinfo = 'writing CHANMIN'
        call wtferr(subname,version,ierrstat,subinfo)
        ierrstat = 0
        call ftpkyj(ounit,'CHANMAX',chanmax,'Maximum PI channel',
     &              ierrstat)
        subinfo = 'writing CHANMAX'
        call wtferr(subname,version,ierrstat,subinfo)
        ierrstat = 0
        call ftpkye(ounit,'BACKGRND',bkgd,8,'Background in cts/pixel',
     &               ierrstat)
        subinfo = 'writing BACKGRND'
        call wtferr(subname,version,ierrstat,subinfo)
        creator = 'pcrpsf '//mnver
        call ftpkys(ounit,'CREATOR',creator,
     &       's/w task that wrote dataset',ierrstat)
        subinfo = 'writing CREATOR'
        call wtferr(subname,version,ierrstat,subinfo)
      ENDIF

c THEORETICAL DATA ...

        IF (.NOT.indata) THEN
          telrad = 'ROSAT'
          instrad = 'PSPC'
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
        hist(1) ='Theoretical Function : '//theo_fn
        nk_comm = 3
        comms(1) = 'Observed Radial Profile: '//infile
        comms(2) = 'Phafile (SPECTRUM extension) : '//phafile
        comms(3) = 'RMF FILE (EBOUNDS extension) : '//rmffile
        IF (off_hist) THEN
          comms(4)=' OFF_AXIS Histogram file : '//detfil
          nk_comm =4
        ENDIF
        nk_comm = nk_comm + 1
        write(comms(nk_comm),'(a,f8.4)') 
     &  'minimum radius for predicted model :',rad_min
        nk_comm = nk_comm + 1
        write(comms(nk_comm),'(a,f8.4)') 
     &  'maximum radius for predicted model :',rad_max
        nk_comm = nk_comm + 1
        write(comms(nk_comm),'(a,i12)') 
     &  'No. of predicted model bins :',n_rad
        if (calc) then
          nk_comm = nk_comm + 1
          comms(nk_comm) = 'pcrpsf calculated the background'
          nk_comm = nk_comm + 1
          write(comms(nk_comm),'(a,f8.4)',IOSTAT=ierr)
     &  'Inner radius for background calculation :',bkgd_rad
        endif
        hduclas3 = 'PREDICTED'

        call wtrpf1(ounit,extname,hduclas3,n_rad,xmin,xmax,radunit,
     &                 ntheta,theta_lo,theta_hi,thetaunit,nenerg,
     &                 energ_lo,energ_hi,energunit,pred,qerror,
     &                 pred_err,rpsfunit,qarea,parea_wgt,hist,nk_hist,
     &                 comms,nk_comm,telrad,instrad,maxpred,maxtheta,
     &                 ierr,chatter) 
        ierrstat = 0
        
        pix = pix_size/REAL(60)

        
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
        call ftpkye(ounit,'BACKGRND',bkgd,8,'Background in cts/pixel',
     &               ierrstat)
        subinfo = 'writing BACKGRND'
        call wtferr(subname,version,ierrstat,subinfo)
        ierrstat = 0
        call ftpkye(ounit,'SUMTCTS',sumtcts,8,
     &'Theoretical Sum of source counts(corrected for any '
     &//'excl regions)',ierrstat)
        subinfo = 'writing SUMTCTS'
        call wtferr(subname,version,ierrstat,subinfo)
        ierrstat = 0
        creator = 'pcrpsf '//mnver
        call ftpkys(ounit,'CREATOR',creator,
     &       's/w task that wrote dataset',ierrstat)
        subinfo = 'writing CREATOR'
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
 
*+PSPC_RDHIST
c     ---------------------------------------------------------
      subroutine pspc_rdhist(detfil,theta_min,theta_max,sohist,
     >                       nhist,ierr,chatter)   
c -------------------- DESCRIPTION --------------------------
c This subroutine opens the detector file and calls Rd_PSPC1992a to
c read the DETECTOR extension, with the off_axis histogram.
c ----------------------- VARIABLES ------------------------

      IMPLICIT NONE
      character*(*) detfil
      integer ierr,chatter,nhist
      real theta_min(*),theta_max(*),sohist(*)

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
c RDOHT1          : (CALLIB) General redaer for PSPC detector ext
c FTOPEN          : (FITSIO) Open FITS file
c FTCLOS          : (FITSIO) Close FITS file
c WT_FERRMSG      : (CALLIB) Writes ftsio, and Routine error message 
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf 1.0.0; Sept 23 1993
c Rehana Yusaf 1.1.0; Feb 9 1994 . RD_PSPC1992a renamed to RDOHT1
c                                . DETECTOR extension search now
c                                  in this routine
c Banashree Mitra Seifert (1996, Jan) 2.0.0:
c              . Replaced by call to MVEXT
c              . Introduced variable decalation needed for this
c              . Carry dimension from calling routine
c              . Introduced screen dislay routines
c                wtinfo,wtferr 
c Peter D Wilson (1998, June 30) 2.0.1:
c              . Abort if fail to open detfil
c ------------------------------------------------------------------
      character(5) version
      parameter (version = '2.0.1')
*-
c ------------------------------------------------------------------
c
c --- INTERNALS ---
c
      character(70) errinfo,subinfo
      character(16) dettel,detint,detfilt,chantyp
      real texpos,areascal,bohist(10)
      integer iunit,status,detchans,errflg
      integer ninstr,nsearch,next(50)
      character(20) instr(50),extname(50),outhdu(9,50),outver(9,50)
      character(20) extnames(9,50)
      character(12) subname 

c --- USER INFO ---
      subname= 'pspc_rdhist' 
      subinfo ='using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)

c ----------------------- OPEN FILE ---------------------------

      status = 0
      ninstr = 2
      instr(1) ='SPECTRUM'
      instr(2) = 'DETECTOR'
      nsearch = 50
c ---------------------------------------------------------------------+
      call mvext (0, detfil, iunit, ninstr, instr, nsearch, next,
     >            outhdu, extnames, outver, extname, errflg, chatter)

      if (errflg .ne. 0) then
          subinfo='locating detector (off axis histogram) extension' 
          call wtferr(subname,version,errflg,subinfo)
          call ftclos(iunit,status)
          subinfo = 'closing DETECTOR file'
          call wtferr(subname,version,status,subinfo)
C PDW 6/30/98: Abort if error encountered
          ierr = 1
          return
      endif

c ---------------------------------------------------------------------+
      call rdoht1(iunit,dettel,detint,detfilt,texpos,areascal,detchans,
     >            chantyp,nhist,theta_min,theta_max,sohist,bohist,
     >            ierr,chatter)

      IF (nhist.EQ.0) THEN
          errinfo = ' no data in DETECTOR file'
          ierr = 1
          call wtferr(subname,version,ierr,errinfo)
      ENDIF
      return
      end
c ---------------------------------------------------------------------
c     END OF PSPC_RDHIST
c ---------------------------------------------------------------------
        

*+WRCOLS
      subroutine wrcols(ncols,columns,maxcol)
c
c --- DESCRIPTION -------------------------------------------------
c
c This routine prints out a character array of column header names,
c using fcecho.
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character(80) info
      integer ncols,maxcol,i
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
c --- AUTHORS ---
c
c Rehana Yusaf ( Feb 1993)
c
*-Version 1.0
c
c     <<<------>>>
c
      info = 'The Following Columns are present in the input file :'
      do i=1,ncols
        curcol = columns (i)
      enddo
      return
      end
c ---------------------------------------------------------------------
c            END OF SUBROUTINE WRCOLS       
c ---------------------------------------------------------------------

*+CALC_CTS

        subroutine calc_cts (npha, ipha, rpha, cts, dtype, 
     &                       texpos, chatter)

c ------------------- DESCRIPTION --------------------------------------
c
c This routine converts the pha rate to counts/arc min instead of 
c counts/arc sec 
c
c ------------------ VARIABLES -------------------------
c
      IMPLICIT NONE
      character(100) subinfo
      integer ipha(*), npha, dtype, i, chatter
      real rpha(*), cts(*) ,texpos
c
c --- VARIABLE DIRECTORY ---
c
c ipha       int    : counts of PHA file
c npha       int    : no. of PHA channel
c dtype      int    : type of data, i.e, dtype=1 for COUNTS
c                                        dtype=2 for RATE=COUNTS/SEC 
c rpha       real   : rate of PHA channel
c cts        real   : converted counts or rate
c
c ----------------------- AUTHORS -----------------------
c
c Banashree Mitra Seifert (1996 Jan) 1.0.0:
c Rehana Yusaf (1996 Feb 22) 1.0.1; bugfix, changed cts(i) = rpha(i) * 60
c                                   to cts(i) = rpha(i) * texpos
c ---------------------------------------------------------------- 

        character(5) version
        parameter (version='1.0.1')
        character(9) subname 
*-
        subname = 'calc_cts'
        subinfo ='using '//subname//' '//version
        call wtinfo(chatter,10,1,subinfo)

            if (dtype .eq. 1) then
                do i=1, npha
                   cts(i) = REAL(ipha(i))
                enddo
            elseif (dtype .eq. 2) then
                do i=1, npha
                   cts(i) = rpha(i) *  texpos
                enddo
            else
                 subinfo='data type is neither count nor rate'
                 call wterrm(subname,version,subinfo)
                 return
            endif
         return
         end

c --------------------------------------------------------------
c                       END OF CALC_CTS 
c ---------------------------------------------------------------


*+CK_PHAFILE

      subroutine ck_phafile(iunit,phafile,typflag,errflg,chatter)

c ---------------------------------------------------------------
c This subroutine checks the PHA file whether it has WMAP extension or 
c Off-AXIS  histogram extension returns typflag value
c 
c for WMAP extension typflag=1
c for Off-AXIS  histogram extension typflag=2
c ---------------------------------------------------------------
      implicit none
      character*(*) phafile
      integer iunit,typflag,errflg,chatter

c ----------------- internal variables --------------------------

      character(100) subinfo
      integer ninstr,nsearch,extnum
      integer next(50)
      character(20) outhdu(9,50),extnames(50),outver(9,50)
      character(20) instr(50),extname
c
c ----------- authors/modifications -----------------------------
c Banashree Mitra Seifert (April, 1996) 1.0.0
c -----------------------------------------------------------------

      character(11) subname
      parameter (subname='ck_phafile')
      character(5) version
      parameter (version='1.0.0')
*-

      subinfo= 'using '//subname//'Ver '//version
      call wtinfo(chatter,10,2,subinfo)
      
c     looking for the WMAP extension in the PHA file
      ninstr = 2
      extname='WMAP'
      instr(1) = 'IMAGE'
      instr(2) = 'WMAP'
      nsearch = 50 
      call mvext(0,phafile, iunit, ninstr, instr, nsearch, next,
     >           outhdu, extnames, outver,extname, errflg, chatter)

      if(errflg .eq. 0) then
         typflag = 1
         subinfo='WMAP extension is found'
         call wtinfo(chatter,10,2,subinfo)
         return
      endif
      if ((errflg .ne. 0) .and. (errflg .ne. 3)) then
           subinfo='returning from checking for WMAP'
           call wtinfo(chatter,10,2,subinfo)
           return
      endif
c     if errflg .eq. 3, look for DETECTOR extension
      if(errflg .eq. 3) then
         errflg=0
         subinfo = 'no WMAP found, looking for Off-axis histogram'
         call wtinfo(chatter,10,2,subinfo)
      endif

      ninstr = 2
      extname ='SPECTRUM' 
      instr(1) = 'SPECTRUM'
      instr(2) = 'DETECTOR'
      nsearch=50
      extnum=0
      call mver(iunit,extnum,ninstr, instr, nsearch,next, 
     >          outhdu, extnames, outver,extname, errflg, chatter)

      if(errflg .eq. 0) then
         typflag = 2
      else
         subinfo='searching for DETECTOR extension'
         call wtinfo(chatter,10,2,subinfo)
         return
      endif

      return
      end

c ------------------------------------------------------------------
c                     end of ck_phafile
c ------------------------------------------------------------------
 
*+PSPC_WMAP

       subroutine pspc_wmap(iunit,nx,ny,maxwmap,wmap,opticx,
     >                      opticy,deltx,delty,pix_x,pix_y,
     >                      status,chatter)

c --------------------------------------------------------------------
c This routine gets the WMAP and other keywords needed to use it:
c the optical axis of the instrument and the size of the "pixels"
c (deltx, delty)  It assumes that the CDELT keywords are in degrees
c and returns them as arcmin.  The formula for finding the x distance 
c from the optical axis to any given WMAP "pixel" is:
c dx=(ix-opticx)*deltx
c where dx is the (angular) distance in arcmin, ix is the x index
c number of the pixel.  Similarily for y.
c It assumes that file has been scrolled to the apropriate extension
c and the file is closed upon completion.
c --------------------------------------------------------------------

      implicit none
      integer iunit,status,nx,ny,maxwmap,chatter
      real opticx,opticy,deltx,delty,pix_x,pix_y
      double precision wmap(maxwmap,maxwmap)

c ----------------internal variables --------------------------------
      integer group,wmbin,ihdvrs
      real nullval,valx,valy
      logical anyfl
      character(256) comm,subinfo
C      character(8) telescop,instrume,detnam,filter,obs_date,obs_tim
      character(8) telescop,instrume,detnam,filter,hduvers
      character(68) obs_date,obs_tim

c ---------- author/modifications ----------------------------------
c Banashree Mitra Seifert (Mar 1996)1.0.0
c        . Introduced this subroutine to include WMAP extension in 
c          the PHA file
c
c Banashree Mitra Seifert (May 1997)1.1.0:
c        . wmap made double precision
c        . maxwmap passed by called routine
c
c kaa 1/11/02  1.2.0  Added support for HDUVERS=2 WMAPs
c ------------------------------------------------------------------
      character(10) subname
      parameter (subname='pspc_wmap')
      character(5) version
      parameter (version='1.2.0')

*-
      subinfo='using '//subname//'Ver '//version
      call wtinfo(chatter,10,2,subinfo) 
      

c     get WMAP sizes
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

c --------- Read the necessary keywords -----------------

c TELESCOP ...
      status = 0
      telescop = 'UNKNOWN'
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call ftupch(telescop)
      if (status .eq. 202) then
         telescop = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         subinfo = 'reading TELESCOP'
         call wterrm(subname,version,subinfo)
      endif

c INSTRUME ...
      status = 0
      instrume = 'UNKNOWN'
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call ftupch(instrume)
      if (status .eq. 202) then
         instrume = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status .ne. 0) then
         subinfo = 'reading INSTRUME'
         call wterrm(subname,version,subinfo)
      endif

C DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call ftupch(detnam)
      if (status .eq. 202) then
         detnam = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status .ne. 0) then
         subinfo = 'reading DETNAM'
         call wterrm(subname,version,subinfo)
      endif

C FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call ftupch(filter)
      if (status .eq. 202) then
         filter = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status .ne. 0) then
         subinfo = 'reading FILTER'
         call wterrm(subname,version,subinfo)
      endif

C OBS-DATE ...
      status = 0
      obs_date = 'UNKNOWN'
      call ftgkys(iunit,'DATE-OBS',obs_date,comm,status)
      if (status .eq. 202) then
         obs_date = 'UNKNOWN'
         status=0
      else if (status .ne. 0) then
         subinfo = 'reading OBS-DATE'
         call wterrm(subname,version,subinfo)
      endif


C OBS-TIME ...
      status = 0
      obs_tim = 'UNKNOWN'
      call ftgkys(iunit,'TIME-OBS',obs_tim,comm,status)
      if (status .eq. 202) then
         obs_tim = 'UNKNOWN'
         status=0
      else if (status .ne. 0) then
         subinfo = 'reading OBS-TIME'
         call wterrm(subname,version,subinfo)
      endif

      group=0
      nullval = 0

      call ftg2dd(iunit,group,nullval,nx,nx,ny,wmap,anyfl,status)
      if (status.ne.0) then
          subinfo='returning from 2d array'
          call wterrm(subname,version,subinfo)
          return
      endif

c Get the HDUVERS.

      CALL ftgkys(iunit,'HDUVERS',hduvers,comm,status)
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL ftgkys(iunit,'HDUVERS1',hduvers,comm,status)
      ENDIF
      IF ( status .NE. 0 ) THEN
         ihdvrs = 0
         status = 0
      ELSE
         READ(hduvers(1:1),'(i1)') ihdvrs
      ENDIF

c Get the rebin factor in the WMAP. pre HDUVERS=2 this is read from the
c WMREBIN keyword. With HDUVERS=2 it can be read from CDELT1P

      IF ( ihdvrs .LT. 2 ) THEN
         call ftgkyj(iunit,'WMREBIN',wmbin,comm,status)
      ELSE
         call ftgkyj(iunit,'CDELT1P',wmbin,comm,status)
      ENDIF
      if (status .ne. 0) then
         subinfo='Cant find rebinning parameter for WMAP, assuming 15.'
         call wtwarm(subname,version,chatter,10,subinfo)
         status=0
         call ftcmsg()
         wmbin=15
      endif

c CDELTS should be positive for our purposes and so take 
c absolute values
      status=0
      call ftgkye(iunit,'CDELT1',deltx,comm,status)
      if (status .ne. 0) then
          subinfo='reading X-axis increment'
          call wtinfo(chatter,10,1,subinfo)
          return
      endif
      deltx=abs(deltx)
      if(deltx/wmbin.lt.2.4e-4) then
         subinfo='US Rev0 data may have wrong value in PHA file.'
     >           //' Standard value is DELTX=2.595021E-4.'
         call wtinfo(chatter,10,1,subinfo) 
         write(subinfo,*) 'X pixel size of wmap is',
     >           deltx/wmbin,' degrees.  Are you sure about this?'
         call wtwarm(subname,version,chatter,0,subinfo)
         write(subinfo,*) 'If this is incorrect, change',
     >           ' DELTX parameter and run tool again'
         call wtwarm(subname,version,chatter,0,subinfo)
      endif

      status=0
      call ftgkye(iunit,'CDELT2',delty,comm,status)
      if (status .ne. 0) then
          subinfo='reading Y-axis increment'
          call wtinfo(chatter,10,1,subinfo)
          return
      endif

      delty=abs(delty)
      if(delty/wmbin.lt.2.4e-4) then
         subinfo='US Rev0 data may have wrong value in PHA file.'
     $           //' Standard value is DELTY=2.595021E-4.'
         call wtinfo(chatter,10,1,subinfo)
         write(subinfo,*) 'Y pixel size of wmap is',
     $           delty/wmbin,' degrees.  Are you sure about this?'
         call wtwarm(subname,version,chatter,0,subinfo)
         write(subinfo,*) 'If this is incorrect, change',
     $           ' DELTY parameter and run tool again'
         call wtwarm(subname,version,chatter,0,subinfo)
      endif

      status=0

c For pre-HDUVERS=2 WMAPs read the CRVAL/CRPIX keywords...

      IF ( ihdvrs .LT. 2 ) THEN

         call ftgkye(iunit,'CRVAL1',valx,comm,status)
         if(status .ne. 0) then
            subinfo='error reading X ref pixel coord.'   
            call wtinfo(chatter,10,1,subinfo)
            return
         endif
c convert the reference value to detector coordinate system
c since wmap does give CRVAL1 in converted form with
c wmrebin 15
         valx = valx * wmbin

         call ftgkye(iunit,'CRVAL2',valy,comm,status)
         if(status .ne. 0) then
            subinfo='error reading Y ref pixel coord.'
            call wtinfo(chatter,10,1,subinfo)
            return
         endif
         valy = valy * wmbin
 
         status=0
         call ftgkye(iunit,'CRPIX1',pix_x,comm,status)
         if(status .ne. 0) then
            subinfo='error reading X pixel size'   
            call wtinfo(chatter,10,1,subinfo)
            return
         endif

         call ftgkye(iunit,'CRPIX2',pix_y,comm,status)
         if(status .ne. 0) then
            subinfo='error reading Y pixel size'
            call wtinfo(chatter,10,1,subinfo)
            return
         endif

c For post-HDUVERS=2 WMAPs read the CRVAL#P/CRPIX#P keywords...

      ELSE

         call ftgkye(iunit,'CRVAL1P',valx,comm,status)
         if(status .ne. 0) then
            subinfo='error reading X ref pixel coord.'   
            call wtinfo(chatter,10,1,subinfo)
            return
         endif

         call ftgkye(iunit,'CRVAL2P',valy,comm,status)
         if(status .ne. 0) then
            subinfo='error reading Y ref pixel coord.'
            call wtinfo(chatter,10,1,subinfo)
            return
         endif
 
         call ftgkye(iunit,'CRPIX1P',pix_x,comm,status)
         if(status .ne. 0) then
            subinfo='error reading X pixel size'   
            call wtinfo(chatter,10,1,subinfo)
            return
         endif

         call ftgkye(iunit,'CRPIX2P',pix_y,comm,status)
         if(status .ne. 0) then
            subinfo='error reading Y pixel size'
            call wtinfo(chatter,10,1,subinfo)
            return
         endif

      ENDIF

      call ftgkye(iunit,'OPTIC1',opticx,comm,status)
      if((status .ne. 0) .or. (opticx .lt. 0)) then
         subinfo= 'Optical axis keyword for X absent or wrong'
         call wtwarm(subname,version,chatter,10,subinfo)
         subinfo= 'Using values from par file.'
         call wtinfo(chatter,10,1,subinfo)
         status=0
c        get the default base optical x axis
         call uclgsr('optaxisx',opticx, status)
         if(status.NE.0) then
            subinfo ='Couldn''t get OPTAXISX parameter. Using 4119.0'
            call wtinfo(chatter,10,1,subinfo)
            status = 0
            opticx=4119.0
         endif
c convert the optic axis(detector system) (pos1) with respect to 
c WMAP coordinate system (pos2)
c conversion formula is
c (pos1 - crpix1)*cdelt1 = (pos2 - crval1)*pixsize in system1--> x-coord
c (pos1 - crpix2)*cdelt2 = (pos2 - crval2)*pixsize in system1--> y-coord

         opticx = (opticx - valx)*2.595021e-4/deltx + pix_x

            
      endif
 
      call ftgkye(iunit,'OPTIC2',opticy,comm,status)
      if((status .ne. 0) .or. opticy .lt. 0) then
         subinfo= 'Optical axis keyword for Y absent or wrong'
         call wtwarm(subname,version,chatter,10,subinfo)
         subinfo= 'Using values from par file.'
         call wtinfo(chatter,10,1,subinfo)

c        get the default base optical y axis
         call uclgsr('optaxisy',opticy, status)
         if(status.NE.0) then
            subinfo='Couldn''t get OPTAXISY parameter. Using 3929.0'
            call wtinfo(chatter,10,1,subinfo)
            status = 0
            call ftcmsg()
            opticy=3929.0
         endif
c convert the optic axis with respect to WMAP coordinate system
         opticy = (opticy - valy)*2.595021e-4/delty + pix_y
      endif

c     we will assume that the CDELTS are degrees and convert
c     to arcmin

      deltx=deltx*60.0
      delty=delty*60.0
 
C -------------- End of keyword reading -----------------

      call ftclos(iunit, status)
      subinfo='unable to close WMAP file'
      call wtferr(subname,version,status,subinfo)

      return
      end

c ----------------------------------------------------------------
c             end of pspc_wmap 
c ----------------------------------------------------------------

*+WMAP_REBIN 

      subroutine wmap_rebin(nx,ny,maxwmap,wmap,binsize,tot_binx,
     >                      tot_biny,
     >                      new_wmap,center_x,center_y,ierr,chatter)

c -----------------------------------------------------------------
c This subroutine rebins the wmap grids in 10X10 wmap size
c and returns the rebinned grids as new_nx,new_ny and new_wmap
c according to binsize
c -----------------------------------------------------------------

      implicit none
      integer nx,ny,binsize,maxwmap
      integer ierr,chatter
      double precision wmap(maxwmap,maxwmap),new_wmap(maxwmap,maxwmap)
      real center_x(*),center_y(*) 
 
c -------------------- internal variables ------------------------

      character(100) subinfo
      integer nbinx,nbiny,rem_x,rem_y,tot_binx,tot_biny
      integer ix,iy,i,j,xmin,xmax,ymin,ymax
      
c ----------- author/modifications -------------------------------
c Banashree Mitra Seifert (April,1996) 1.0.0 
c
c Banashree Mitra Seifert (May 1997)1.1.0:
c        . wmap made double precision
c        . maxwmap passed by called routine
c ----------------------------------------------------------------
      character(11) subname
      character(5) version
      parameter (version='1.1.0')
*-
     
      subname='wmap_rebin'
      subinfo='using '//subname//'Ver '//version
      call wtinfo(chatter,10,2,subinfo) 

c --------------- variable definitions ---------------------------
c binsize input  int  no. of bins for rebinning
c nx      input  int  no. of bins before rebinning x direction
c ny      input  int  no. of bins before rebinning y direction
c rem_x          int  remainder bins after binning in x direction
c rem_y          int  remainder bins after binning in y direction
c tot_binx       int  total no. of rebinned bins in x direction
c tot_biny       int  total no. of rebinned bins in y direction
c xmax           int  maximum bin no for a particular bin
c ymin           int  minimum bin no for a particular bin
c ------------------------------------------------------------------

      if(binsize .eq. 1) then
         tot_binx = nx
         tot_biny = ny
         do ix=1,tot_binx 
            center_x(ix) = ix
            do iy=1,tot_biny
               new_wmap(ix,iy) = wmap(ix,iy)
            enddo
         enddo
         do iy=1,tot_biny
            center_y(iy)=iy
         enddo
         ierr = 0
         return
      endif

      nbinx = nx/binsize
      nbiny = ny/binsize
c to see if there are left over bins after rebinning it

      rem_x = mod(nx,binsize)
      rem_y = mod(ny,binsize)
c if there are left over few bins < binsize then increment the
c total no. of bins as 1 otherwise ok.
c so the last bin has bins < binsize

      if(rem_x .eq. 0) then
         tot_binx=nbinx
      else
         tot_binx=nbinx+1
      endif

      if(rem_y .eq. 0) then
         tot_biny=nbiny
      else
         tot_biny=nbiny+1
      endif

      xmax =0
      do ix=1,tot_binx
         xmin=xmax+1
         xmax=xmax+binsize
         if(xmax .gt. nx) then
            xmax = nx
         endif
         center_x(ix)=real(xmin+xmax)/2.
         ymax=0
         do iy=1,tot_biny
            new_wmap(ix,iy) = 0. 
            ymin=ymax+1
            ymax=ymax+binsize
            if(ymax .gt. ny) then
               ymax = ny
            endif
            center_y(iy)=real(ymin+ymax)/2.
            do i=xmin,xmax
            do j=ymin,ymax
            if(wmap(i,j) .gt. 0) then
                  new_wmap(ix,iy)=new_wmap(ix,iy)+wmap(i,j)
            endif    
            enddo
            enddo
         enddo
      enddo

      return
      end

c -----------------------------------------------------------------
c                   end of wmap_rebin
c -----------------------------------------------------------------

*+CALC_WMAP

c --------------------------------------------------------------------+
      subroutine calc_wmap(npha,en,n_rad,rad_min,rad_max,cts,nx,ny,
     >                     opticx,opticy,center_x,center_y,
     >                     deltx,delty,maxwmap,new_wmap,sumtcts,bkgd,
     >                     pix_size,
     >                     rpsf,theo_fn,chanmin,chanmax,
     >                     ierr,chatter)

c ---------------------------------------------------------------------
      implicit none
      character*(*) theo_fn
      integer npha,n_rad,nx,ny,chanmin,chanmax,maxwmap,ierr,chatter
      real rad_max, rad_min,rpsf(n_rad,1,1),cts(*),en(*)
      real bkgd,pix_size
      real center_x(*),center_y(*),sumtcts
      double precision new_wmap(maxwmap,maxwmap)
      real opticx,opticy,deltx,delty

c ------------------------ internal variables --------------------
      character(100) subinfo
      real dx,dy,theta,dx2,dy2
      real psf,tot_psf,rpsf_theta,rpsf_theta_tot
      real x(1000),rad_binsize,psfoff
      integer i,ix,iy,ien,tot_wmap,tot_cts

c ------------------- authors/modifications --------------------
c Banashree Mitra Seifert (Mar, 1996) 1.0.0:
c
c Banashree Mitra Seifert (May 1997)1.1.0:
c        . wmap made double precision
c        . maxwmap passed by called routine
c Peter D. Wilson (Mar 1998) 1.1.1:
c        . Calculate PSF using average radius of annulus instead of
c          the average of the PSF at the inner and outer edges
c        . Test bkgd for 0.0 to see if pix_siz is defined before
c          dividing by pix_siz
c -------------------------------------------------------------

      character(10) subname
      parameter (subname='calc_wmap')
      character(5) version
      parameter (version='1.1.1')

*-
      subinfo='using '//subname//'Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c divide the whole area into concentric radial bins with
c RAD_LO as inner radius and 
c RAD_HI as outer radius of each bin

      rpsf_theta =0.0

      rad_binsize = (rad_max - rad_min)/REAL(n_rad)
      do i = 1,n_rad
         x(i) = rad_min + (i-0.5)*rad_binsize
      enddo

c calculation for wmap distribution
      theo_fn = '(OFF-AXIS) PSF ,Apr 1993'
      do i=1,n_rad
         tot_wmap = 0
         rpsf_theta_tot = 0.
         do ix=1,nx
            dx = (center_x(ix)-opticx)*deltx
            dx2 = dx*dx
            do iy=1,ny
               if(new_wmap(ix,iy) .gt. 0) then
               dy = (center_y(iy)-opticy)*delty
               dy2 = dy*dy
               theta = sqrt(dx2+dy2)
               tot_cts = 0
               tot_psf =  0.
                  do ien = chanmin,chanmax 
                     ierr = 0
                     if((en(ien) .ge. 0.07) .and. (en(ien) .le. 3))then
                        if (cts(ien) .gt. 0) then
                           psf = psfoff(en(ien),theta,x(i)*60.,
     >                                     ierr)*3600.
                           if(ierr .ne. 0) then
                              subinfo='error calculating theo psf'
                              call wterrm(subname,version,subinfo)
                              return
                           endif
                           tot_psf = tot_psf + psf * cts(ien)
                           tot_cts = tot_cts + cts(ien)
                         endif
                     endif
                  enddo
                    
                  if (tot_cts .eq. 0) then
                      ierr = 1
                      subinfo='total counts for all pha channels is 0'
                      call wterrm(subname,version,subinfo)
                      return
                  else
                      rpsf_theta = tot_psf/tot_cts
                      rpsf_theta_tot = rpsf_theta_tot + 
     >                                 rpsf_theta * new_wmap(ix,iy)
                      tot_wmap = tot_wmap + new_wmap(ix,iy)
                  endif
               endif
            enddo
         enddo

         if (tot_wmap .eq. 0) then
             rpsf(i,1,1) = rpsf_theta
         else 
             rpsf(i,1,1) = sumtcts * rpsf_theta_tot/tot_wmap 
             if(bkgd.ne.0.0) 
     >            rpsf(i,1,1) = rpsf(i,1,1) + bkgd/pix_size**2
         endif

      enddo
 
      ierr = 0
      return
      end

c ---------------------------------------------------------------------+      
c                  end of calc_wmap 
c ---------------------------------------------------------------------+      
   
