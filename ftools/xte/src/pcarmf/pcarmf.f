
C*********************************************************************
C subroutine: PCARMF
C
C DESCRIPTION:
C       Creates response matrices for an XTE PCA observation
C
C AUTHOR:
C	Keith Jahoda (PCA Team)
C
C MODIFICATION HISTORY:       
C       version 0.0  5-31-95
C                      coded by Keith Jahoda;
C                      expect the occasional core dump
C                       (worse yet, sometimes it runs)
C       version 0.1  7-19-95 DETNAM keyword changed to PCUn  (n=0,4)
C                    8-11-95 hduclas3 changed to upper case (DETECTOR)
C	version 0.2  1-29-96  
C       version 0.3  2-02-96 Revision by J. Lochner
C                       include FITS format calls to chan2energy file,
C                       a call to caldb to retrieve chan2energy file,
C                       and clean-up of the error handling
C       version 1.0  2-02-96 changed dimension of energ_lo and energ_hi
C                       to (0:maxchan-1)  KJ
C       version 1.07 4-23-96 added common block for passing calibr
C                       file name, extension and description between
C                       chan2energy.f and rmffpar.f (J. Lochner)
C     version 1.08 10-08-96 changed the callib rmf & ebd writing
C                       routines from wtrmf1 to wtrmf3 and wtebd2 to
C                       wtebd3. 
C                     Also properly declared the variables in the
C                       "xsection" routines (e.g. hydrogen et al) - (J Lochner)
C       version 1.09 10-17-96 changed first_chan value for wtrmf3 from
C                       0 to 1 on recommendation of Keith Arnaud for
C                     compatibility with upcoming XSPEC changes
C       version 1.10 11-21-96 (MJT) changed first_chan back to 0 as XSPEC
C                       will now handle this. first_chan is now passed to
C                       wtrmf3(), wtebd3() and to grprmf() which replaces
C                       grp_rmf()
C       version 2.0  10-26-96 (KJ) updated /xenon_data/ (which doesn't appear
C                    in this element
C                    11-26-96 (KJ) scale_hack comes in here (not in effic)
C                      first_chan = 0, everywhere, and call to grp_rmf
C                      changed to grprmf. Note - KJ created v2.0 based on
C                      v1.07, and hence redid some of JCL's changes in
C                      v1.08 & v1.09 and MJT's changes of v1.10
C     version 2.01 12-12-96 (JCL) Fixed logic bug in chan2energy to correctly
C                      return parameters for lld's other than 63.
C     version 2.02 12-17-96 added new subroutines to attempt a scale correction
C               derived for Crab, epoch 3,
C               see http://lheawww.gsfc.nasa.gov/users/keith/energy/scale_hack.html
C               for details
c     01-01-97  added channel shift option
C     version 2.1.1 2-27-97  removed standard2 option, ishift option
C               changed interpretation of 63 so that 63 = 3+12+48
C     version 2.1.2 has removed scale_hack option.
C             restored, but you must supply your own scale file.
C        lld_code=63 now is explicitily 3+12+48
C        proapane matrix now created
c
c     version 3.0 - incorporates the energy scale of v2.2.1 (never released as
c         an ftool, but widely beta tested.
c                 - includes parameterization of change in gain,
c                 - change in xenon in propane layer.
c                 -  actual best constants could be scrubbed some more! (3-2-98)
c     version 3.1 - removes some keywords,
c                   more general energy2chan
C
C	7/6/98 - by ZG to change the string length of obs_date, obs_time, and
C		refdate to 68 for y2k new format.
c     version 3.5 - 8/27/98 by KJ to incorporate the time dependencies into the parameters
c           when read via the caldb route
c     version 3.6 - 9/2/98.  declared scale as external, added Jahoda and McCammon partial
c           charge collection
c      1/13/99 - added common block /dates/ and read dates in as character values;
c           modest changes in chan2energy.  Accomodates ftools Y2K compliance
c      1/19/99 - turned off gain drift for dates in epoch 1,2
c      4/30/99 - partial charge collection only evaluated if pcc > 1.e-6
c      v3.7
c      4/30/99 - w_xe changed to allow the version that does a polynomial between Xe_l and _k
c           with jumps at edges.
c      aug 18, 1999 v3.8 - replace gtcal with gtcalf (PDW)
c      12-5-99  v3.8 jumps to v7.0
c           added parameter for w_xe scaling
c      12-11-99 v7.01 adds use of par(4) to scale the gainshift parameters from epoch 3 to epoch 4
c      02-25-00 v7.10 changes sigmaf to allow fussing with the resolution parameter
c      04-03-00 v7.10 adds deadlayer to effic - (non sensitivie area between the
c           layers
c      08-17-00 v7.10 forces gm_cm2 = 0 in propane layer of PCU 0 for all dates > May 13 2000.
c      11-21-00 v7.10  changed location of updating xe_gmcm2_pr to rmffpar, added mjd_obs, mjd_obs0 to /dates/
c      06-04-01 v7.10 (MJT) writing pcarmf vrsn_no in a HISTORY keyword so it survives marfrmf
c      04-16-02 v8.0  corrected calculation of sigma
c                     made energy channels logarithmic
c                     made dead layer separate by detector (changes .par file)
c                     added opportunity for second xe_gm_cm2_prN law (changes .par file, now just used for PCU 0)
c      08-15-03 v10.0 corrected small errors in respch,  removed all reference to gaina
c               reduced range of energies evaluated, and reduced number of energy channels
c               fixed a small error in counting number of energy channels
c      10-15-03 v10.1 (MJT) final delivery for release with HEASOFT v5.3; only change 
c               is increase in filename variable sizes to accomodate long pathnames and
c               using 10.1 version name following KJ's notation.
C
C 
C      05-15-07 v11.0 Modified by K. Jahoda to account for non-vetoed K-escape events
C		through escape_width parameter.  
C      10-29-07 hardcoded escape_width to be 1.5 keV (removed escape_width from par file)
C      05-18-07 V11.1 Modified by N. Shaposhnikov for the escape_width parameter to be read 
C               from parameter file	
C      08-07-07  V 11.2 added L escape and Kb escape line. All escape lines are
C 		individually normalized with five new parameters EscNormL1-2(layers) 
C		and EscNormKb     
C      10-29-07  V 11.5 removed L escape line. 
C 	        EscNormL1-2(layers) are no more!    
c
c      08-15-07 Just Ka and Kb escape line are left
c      08-15-07 Found trend in xenon layer 1 consistent with leak to the propane.
c		Adding the term to xe_gmcm2_l1
c      02-16-08 Adding the La escape line in similar way as for Kb through parameter
c   EscNormLa. The La escape line norm is assumed to be the same for all layers which
c	worked for Kb line. 
c   
c      02-20-08 Now La escape line norm vary with layer
c      04-09-09 NS: Code cleaning for a final release of v11.6. Changing the 
c               gtcalf() first option specifying the chatter level from 5 to 0
c               by the request of Mike Tripicco.
c 
c      05-11-09 Removing the leak term from L1 into propane layer ( the one which 
c               corrects the amount of xe in L1 according to the leak 
c               rate, the term inceasing the xe amount in pr layer stays).
c               Model fits show no noticable difference with or
c               without the leak. However, model with the xe leak from
c               L1 reqiures negative leak after a propane layer loss -
c               unphysical. (v11.7)

C
C	NOTES:
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED SUBROUTINES:
C
C
C       
C *******************************************************************

	subroutine PCARMF
	
c  properties of the matrix
       integer*4  maxen, ienerg
       integer*4  maxchan, ichan
       integer*4  maxgrp

       parameter (maxen=300, ienerg=300)        ! could be any number
       parameter (maxchan=256, ichan=maxchan)   ! Must be 256 !
       parameter (maxgrp=10)

       real*4    energ_lo(0:maxchan-1), energ_hi(0:maxchan-1)
       real*4    e_min(maxen), e_max(maxen)
       integer*4 ngrp(maxen), f_chan(maxen*maxgrp), n_chan(maxen*maxgrp)

       real*4 RESP_MATRIX (0:MAXCHAN-1,MAXEN)
       integer*4	illd, lldm(3)
       data	lldm	/48,12,3/

c  control for whether chan2energy looks up new parameters
       integer*4	icall
       common  /chane_init/    icall

c******************************************************
c  documentation of the program
       character(70) vrsn_no(30)
       common    /version/    vrsn_no
c *****************************************************

c JCL Addition, v1.07 *********************************
c   documentation of calibration file
        character(160) filenm(1)
        character(80) cdes
        integer extno(1)
        common /calfile/ filenm, extno, cdes
C End JCL Addition ************************************	
       
c required keywords - OGIP Calibration Memo CAL/GEN/92-002
       character(5) rmfversn
       character(16) telescop, instrume, detnam, filter
       character(20) hduclas3
       character(80) msg
       character(160) rspfile, c2efile

       data    rmfversn /'1.2.0'/
       data    hduclas3 /'DETECTOR'/
       data    telescop /'XTE'/
       data    instrume /'PCA'/
       data    filter /'NONE'/
c       data    chantyp  /'PHA'/
c
c      detnam entered at run time
c
c*****************************************************
c  more input to wrtrmf1  
       integer*4    nk_hist, nk_comm, first_chan
       character(6) chan_type
       character(70) hist(10), comment(30)
       data  nk_hist, nk_comm  /0, 0/
       
       integer*4  chatter, ounit
C JCL  data       chatter       /10/
       real*4     areascal
       data       areascal      /1./
       real*4     thresh
       data       thresh        /1.e-6/     ! reasonable choice?

       integer*4 i, ierr, j
c*****************************************************
c  declarations to guide building the matrix, and fill in last
c  required keywords

c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

       real*4     energy, efficiency

c added up here 3-11-00 to have access to lbl
c detector parameters
        real*4     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3, xe_gmcm2_pr
        real*4     pr_gmcm2, my_gmcm2, al_gmcm2, del_xepr
        real*4     kEdge_veto, lbl, lEdge_veto, xe_gmcm2_dl
   
        common     /detector/     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3,
     &				      xe_gmcm2_pr, del_xepr,
     &                                pr_gmcm2, my_gmcm2, al_gmcm2,
     &                                kEdge_veto, lbl, lEdge_veto, 
     &				     xe_gmcm2_dl

c parameters to write out energy and sigma
	real*4	e0, ep0, sigma0, sigmaf

c  external functions
       real*4      scale
	external	scale
c******************************************************
        character(40) taskname
        common /task/ taskname

	real*4    sum
 
	real*4    r256(0:255)

	filenm(1) = ' '
	cdes = ' '
C  NS Modified 05-18-07 ****************************
        taskname = 'pcarmf11.7'

	do i = 1,30
	   vrsn_no(i) = ' '
	end do
	
        vrsn_no(1) = 'pcarmf     v11.7,  May  11, 2009 - caveat emptor'
C***************************************
	
	icall = 0

c   here we go!   response file opened in routine rmffpar (by call to
c     op_npa.  This hack needed to get name of response file out of the
c     parameter file.
c        this call reads parameters and writes key-words

	call fcecho(vrsn_no(1))

	call rmffpar (ounit, rspfile, c2efile, chatter, 1, ierr)
	if (ierr .ne. 0) then
	   msg = 'ERROR reading parameters'
	   call fcecho(msg)
	   go to 900
	endif
	
c
c  input parameters are passed around in common blocks /schain/
c                                                  and /detector/

c  set up response matrix energies
C     arf generator gets energies by reading .rmf ebounds extension

	call mat_energ (E_MIN, E_MAX, MAXEN)
	   

	if (lld .lt. 63 .or. lld .eq. 64) then
c  set up the energy boundaries of the channels (these are the values used
c  by XSPEC for plotting against an energy scale)

          call chan_energ (energ_lo, energ_hi, maxchan, c2efile, ierr)
	  if (ierr .ne. 0) go to 900

	  do i = 1, MAXEN

	   energy = (E_MIN(i)+E_MAX(i))/2
	   sum=0.

	   call respch(r256, energy, energ_lo, energ_hi)
	   do j = 0, MAXCHAN-1 
	      sum=sum+r256(j)
	      RESP_MATRIX(j,i) = r256(j) 
	   end do

	  end do
	  go to 100
	end if

c  arrive here if lld = 63;  make one matrix for each layer and add results
	do i = 1, MAXEN
	   do j = 0, MAXCHAN-1
	      RESP_MATRIX(j,i) = 0.0
	   end do
	end do

	do illd = 1,3      
c replaced 29 Sept 2000
c	do illd = 3,1,-1  
c          force new parameter read by chan2energy
	   icall = 0			
	   lld = lldm(illd)
           call chan_energ (energ_lo, energ_hi, maxchan, c2efile, ierr)
	   if (ierr .ne. 0) go to 900
	   do i = 1, MAXEN
	     energy = (E_MIN(i)+E_MAX(i))/2
c	     efficiency = effic(energy)
	     sum=0.
	     call respch(r256, energy, energ_lo, energ_hi)
	     do j = 0, MAXCHAN-1 
	        sum=sum+r256(j)
c	        RESP_MATRIX(j,i) = RESP_MATRIX(j,i) + r256(j) * efficiency
	        RESP_MATRIX(j,i) = RESP_MATRIX(j,i) + r256(j) 
	     end do
	   end do
	end do
C  reset value of lld;  note that last pass thorugh loop had lld=3 so the call to
C   chan_energ has this value.  RESP_MATRIX is the sum of 3 matrices
	lld = 63

 100	first_chan = 0
C no default available here
        if (scale_hack .eq. 1) then
          do i = 0,MAXCHAN-1
            energy = (energ_lo(i)+energ_hi(i))/2.
            efficiency = scale(energy)
            do j = 1,MAXEN
              RESP_MATRIX(i,j) = RESP_MATRIX(i,j)*efficiency
            enddo
          enddo
        end if

	if ( lbl .gt. 10.) then
	    do i = 0, MAXCHAN-1
		e0 = (energ_lo(i)+energ_hi(i))/2.
		ep0 = energy2energyprime (e0, model)
		sigma0 = sigmaf(ep0)
c	        write (*,*) 'ZZZ ',energ_lo(i),energ_hi(i),sigma0
	    end do
	end if

	  call grprmf(CHATTER, MAXCHAN, ICHAN, MAXEN, IENERG,
     &          RESP_MATRIX, THRESH, MAXGRP, first_chan, ngrp, f_chan, 
     &          N_chan, ierr)
	  if (ierr .ne. 0) then
	   msg = 'grm_rmf error '
	   call fcecho(msg)
	   go to 900
	  endif


c  write response matrix
c        do i=1, nvers
c            comment(i) = vrsn_no(i)
c        end do
c        nk_comm = nvers
	comment(1) = vrsn_no(1)
	nk_comm = 1

c PCA team members will note that the detnam actually identifies the SLOT #
c   except that detnam runs from PCU0 - PCU4.
        write (detnam,1100) pcuid
1100    format('PCU',I1)
	hist(1) = ' '

 	chan_type = 'PHA'
c	write (*,*) 'test ', ierr

 	call wtrmf3(OUNIT, CHATTER,
     &          nk_hist, hist,
     &          nk_comm, comment,rmfversn, hduclas3,
     &          telescop, instrume, detnam, filter, areascal,
     &  	chan_type,first_chan,
     &          MAXCHAN, ICHAN, MAXEN, IENERG, E_MIN, E_MAX,
     &          MAXGRP, ngrp, F_chan, N_chan,
     &          RESP_MATRIX, THRESH, ierr)
	if (ierr .ne. 0) then
	   msg = 'wtrmf3 error '
	   call fcecho(msg)
	   go to 900
	endif

C  this call to rmffpar just writes keywords
	call rmffpar (ounit, rspfile, c2efile, chatter, 2, ierr)
	if (ierr .ne. 0) then
	   msg = 'ERROR writing parameters'
	   call fcecho(msg)
	   go to 900
	endif

	call wtebd3(ounit, chatter,
     &          nk_hist, hist,
     &          nk_comm, comment,rmfversn,
     &          telescop, instrume, detnam, filter, areascal,
     &          chan_type,
     &          first_chan, MAXCHAN, ENERG_LO, ENERG_HI, ierr)
	if (ierr .ne. 0) then
	   msg = 'wtebd3 error '
	   call fcecho(msg)
	   go to 900
	endif

C  this call to rmffpar just writes keywords
	call rmffpar (ounit, rspfile, c2efile, chatter, 2, ierr)
	if (ierr .ne. 0) then
	   msg = 'ERROR writing parameters'
	   call fcecho(msg)
	   go to 900
	endif


	call ftclos (OUNIT, ierr)


900	continue
	return
	
	end



	function scale (e0)

c 11-11-96  generalized to read from any file of less than or equal to 129 energy/value
c            pairs

	real*4   e0, scale
	real*4   e(129), sss(129)
	integer*4  i, icall, imax
	data	icall	/0/
	character(80)  msg

c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
	character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &		scale_file

        imax=0

	if (icall .eq. 0 ) then
	  if (scale_hack .eq. 1) then
	   if (scale_file .ne. 'none') then
	    open (unit=35, file=scale_file)
	    do i=1,129
	      read (35,*,end=10) e(i),sss(i)
	    end do
  10	    imax=i-1
c	    write (*,*) 'imax = ',i
	    close (35)
	   end if
	   if (scale_file .eq. 'none') then
	      msg = 'Scale_hack .eq. 1 but no file - and therefore no'
	      call fcecho(msg)
	      msg = '   correction applied'
	      call fcecho(msg)
	      imax= 1
	   end if
          end if
	  icall=1
	end if

	scale = 1.0
	if (e0 .le. e(1))  return
	if (e0 .ge. e(imax)) return

	do i = 2,imax
	   if (e0 .le. e(i)) then
	      scale = sss(i-1) + ( (e0 - e(i-1))/(e(i)-e(i-1)) ) *
     &		(sss(i) - sss(i-1))
	      return
	   end if
	end do

	return
	end
C *************************************************************************
C SUBROUTINE:
C       chan_energ
C
C DESCRIPTION:       
C
C AUTHOR:
C       Keith Jahoda
C
C MODIFICATION HISTORY:
C  1-25-96 calls chan2energy with real argument
C          + hack for display (?)
C  1-32-96 calls to chan2energy now get channel edges directly
C  2-01-96 Added c2efile to pass to new version of chan2energy function.
C           Also added ierr for graceful error handling  (J. Lochner)
C  2-02-96 3 L edges into xenon_data
C  2-08-96  still a problem with channel widths as interpretted by
C            XSPEC with set energy (not believed to affect calculation,
C            but displays notch in model (and data))
C  2-12-96 Fixed calling sequence for second call to chan2energy - JCL
C  10-24-96  removed option 3 from v1.07.
C          (calls chan2energy, so e2c_model is irrelevant here)
C     
C NOTES:
C       original version  5-31-95   by Keith Jahoda
C       10-24-96.  from v1.7 to v2.0.
C       the channel boundaries are primarily useful for plotting in XSPEC, although
C         the energies may be used in interpolating from one epoch to another.
C
C USEAGE:
C     call chan_energ(energ_lo, energ_hi, maxchan, c2efile)
C
C ARGUMENTS:
C     energy_lo -
C     energy_hi -
C     maxchan   -
C     c2efile   - name of channel-to-energy calibration file
C     ierr      - error status
C     
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED SUBROUTINES:
C
C********************************************************************
         subroutine chan_energ (energ_lo, energ_hi, maxchan, c2efile,
     $     ierr) 

c calling sequence declarations
        character*(*) c2efile 
        integer*4 maxchan, ierr
        real*4    energ_lo(0:maxchan-1), energ_hi(0:maxchan-1)

c local declarations
        integer*4 i

c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

        real*4     par(10)
        integer*4  npar, noCalDB
        common   /c2epar/  par, npar, noCalDB

        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,EscFracL,
     & 			      EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef


c external function
        real*4    chan2energy

c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

c	real*4	a,b,c,el1,el2,chanemax
c	common	/etest/	a,b,c,el1,el2,chanemax

        vrsn_no(7) = 'chan_energ    v7.01,  12-11-99'
 
        do i = 0, maxchan-1

	   energ_hi(i) = chan2energy(i,1,c2efile,ierr)
	   energ_lo(i) = chan2energy(i,0,c2efile,ierr)

c	write (*,8000) a,b,c,el1,el2,energ_lo(i),energ_hi(i)
c8000	format (7F12.4)

        end do


999     continue
        return
        end

C ***********************************************************************
C FUNCTION:
C     chan2energy
C
C DESCRIPTION:
C
C
C AUTHOR:
C     Keith Jahoda
C
C MODIFICATION HISTORY:
C     Jan 26, 1996 - Change chan2energy input file to FITS format (J. Lochner)
C     Jan 31, 1996 - Inserted callib routines to fetch chan2energy file
C                      from caldb (J. Lochner)
C     Feb  2, 1996 - added flag to calling sequence to specify lower or
C                      upper edge of channel  KJ
C     Feb  2, 1996 - 3 L edges in xenon_data
C     Apr 18, 1996 - Changed to search for energy2chan file.  Also added
C                     further checks for proper extension for non-caldb
C                     retrieval of energy-to-channel FITS file. (J. Lochner)      
C     Apr 23, 1996 - search for correct columns in cal file using column names
C                     and put filenm, extno and cdes value in common block
c                     (J. Lochner)
C     Oct 24, 1996 - added dos Santos calculation of channels for option 3
C                      and removed previous cumbersome version
C                    both subroutines now calculate channel<->energy_prime
C                    conversion, and assume that program will take care of conversion
C                    from energy to energy_prime when necessary
C     Nov 26, 1996 - supports model 1 (linear - no data supplied to GOF)
C                                   2 (quadratic in energy)
C                                   3 (quadratic in eprime, doesn't work as well as
C                                       it should)
C                    e2c and c2e make no distinction between e and eprime;
C                    calling program must keep this staright.
C                    subroutine energy2energyprime is in this code element
C     Dec 12, 1996 - Fixed logic bug to correctly return parameters for lld's
C                    other than 63. (J. Lochner)
C     Feb 27, 1997 - added /chane_init/ so that pcarmf can control whether
C                     this function retrieves a new set of conversion parameters
c     Feb 13, 1998 - added gain strectch feature  (gainf parameter)
c     Mar 02, 1998 - added parameterization for gain change
c     Mar 11, 1998 - removed gainf in favor of second method
c     Mar 19, 1998 - allowed lld_code=1 to select data base entry for lld_code=3
c                      if this is found first, similarly for other layers
c     Apr  3, 1998 - removed iflag from energy2rchan (to compile with g77)
c     Aug 28, 1998 - v3.5 fixed so that caldb reading would also apply the time dependence
c     Jan 13, 1999 - now gets dates as character values.  Differences calculated
c     Jan 19, 1999 - turned off gain drift for dates in epoch 1,2
c		after conversion to MJD
c     Aug 18, 1999 - Replace gtcal with gtcalf (PDW)
c     Dec 12, 1999 - par(4) now scales epoch 3 to epoch 4 for gain shift
c     Mar 10, 2000 - added McMaster cross section option for xenon
c     Apr  6, 2000 - added cubic (par(9)) to energy to chan
c                 chan from energy still has just quadratic
c     Apr 15, 2002 - slightly cleaned up
C       
C     
C NOTES:
C     This routine reads the CALDB, or whatever, to get the parameters of the
C     channel energy relation.  Keith Jahoda reads from an ascii file of exceedingly
C     particular format.  Jim Lochner made this FITS compatible and translated the
C     particluar format to FITS files
C
C
C ARGUMENTS:
C     ich     - input channel number (integer)
C     iflag   - (integer)  0 ==> return energy of lower edge of channel
C                          1                      upper
C     c2efile - name of channel-to-energy calibration file (character)
C     ierr    - error status (integer*4)
C     
C PRIMARY LOCAL VARIABLES:
C
C CALLED SUBROUTINES:
C
C ***********************************************************************     

      function chan2energy(ich, iflag, c2efile, ierr)

c calling sequence declarations
        integer*4    ich, iflag, ierr
        real*4       chan2energy
        character(160) c2efile
        
c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

        real*4     par(10)
        integer*4  npar, noCalDB
        common   /c2epar/  par, npar, noCalDB

        character(160) filenm(1)
        character(80) cdes
        integer extno(1)
        common /calfile/ filenm, extno, cdes
        
        real*4          escale
        integer*4       date0
 
        common  /gainshift/     date0, escale

	character(20)	cdate, cdate0, cdate1
	real*8		mjd_obs, mjd_obs0, mjd_obs1
	common /dates/ mjd_obs, mjd_obs0, mjd_obs1, cdate, cdate0,cdate1

        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,
     &                        EscFracL,  EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        real*4     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3, xe_gmcm2_pr
        real*4     pr_gmcm2, my_gmcm2, al_gmcm2, del_xepr
        real*4     kEdge_veto, lbl, lEdge_veto, xe_gmcm2_dl
        common     /detector/     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3,
     &                               xe_gmcm2_pr, del_xepr,
     &                               pr_gmcm2, my_gmcm2, al_gmcm2,
     &                               kEdge_veto, lbl, lEdge_veto, 
     &				     xe_gmcm2_dl
 


c local declarations
	character(200)  record200
	character(1)    cc
	integer*4      mmodel, mmid
	character(66)   ccdate
        integer*4      mlld(11), convmeth
        integer*4      icall, iunit, ftstatus
        real*4 parm1(11), parm2(11), parm3(11), parm4(11), parm5(11)
        real*4 parm6(11), parm7(11), parm8(11), parm9(11), parm10(11) 
	common	/chane_init/	icall
        integer*4 id, i
        character(8) mpcuid
	real*4    rch
c	integer*4	iyr, imn, idy, mdy, mdy0

c stores chanemax from one visit to next
c 3-15-00 this was added to protect against non real solutions for energy
	real*4	el1,el2,a,b1,c1, chanemax
	common	/etest/	a,b1,c1,el1,el2,chanemax
        
C Additional declarations for fitsio subroutine calls - JCL
        double precision mjd_ref
        character(80) msg, comment
        character(20) cpcuid20, cmodel, parmnum
	character(68) obs_date, obs_time, refdate
	character(16) cbd2value
        character(8) detnam, ccnm0001
        character(6) online(1)
        character(5) parmname
        integer block, hdutype, nrows, ncols, frow, felem, nullval
        integer nret, nfound, j, n, parmcol, lldcol
        logical anyf, quiet, found

C Additional declaration for ftgcve calls - MJT 9Apr98
        real*4 nullvale
	real*4 e0, e1, tolit, tol
        integer itmax

        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

        vrsn_no(3) = 'chan2energy     v8.0  4-16-02'

        chan2energy=0.0
        rch=0.0

        ftstatus = 0
        if (icall .eq. 0) then  ! first time into function

	if (noCalDB .eq. 0) then
C** Begin JCL Addition

C     Convert PCU ID and date from integer to character
              call fti2c(pcuid,cpcuid20,ftstatus)
              detnam = 'PCU'//cpcuid20(20:20)
              
C       JCL May 5, 1999: assign input date (cdate) to obs_date
              obs_date = cdate
              obs_time = '00:00:00'
           
C Read channel-to-energy file from caldb 
           if (c2efile .eq. 'caldb' .or. c2efile .eq. 'CALDB') then
              
              quiet = .false.

              call fti2c(model,cmodel,ftstatus)
              cbd2value = 'METHOD.EQ.'//cmodel(20:20)

C             No chatter value here, so use 5 to report only errors
              call gtcalf(0,'XTE','PCA',detnam,'-','E2C_PARM',
     $             obs_date,obs_time,obs_date,obs_time,cbd2value,1,
     $             filenm,extno,online,nret,nfound,ierr)

              if (ierr .ne. 0) then
                 msg = 'ERROR - unable to get energy-to-channel file'//
     $                ' from caldb'
                 call fcecho(msg)
                 go to 999
              endif
              
C Or translate the name of the file (taking off any extension specified
c     by the user.
           else
              call fcpars(c2efile,filenm(1),extno(1),ftstatus)
              if (ftstatus .ne. 0) then
                 msg = 'WARNING - proble passing e2c file name'
                 call fcecho(msg)
                 msg = ' ... will search all extensions'
                 call fcecho(msg)
                 extno(1) = -99
              endif
           endif
           
C Open the channel-to-energy file
c           msg = 'energy-to-channel file used = '//filenm(1)
c           call fcecho(msg)
           ftstatus = 0
           call cgetlun(iunit)
           call ftopen(iunit,filenm(1),0,block,ftstatus)
           if (ftstatus .ne. 0) then
              msg = 'cannot open energy-to-channel file'
              call fcerr(msg)
              go to 999
           endif
           
C Move to the extension for the input pcuid
           if (extno(1) .ge. 0) then
C            - extension number already known
              call ftmahd(iunit,extno(1)+1,hdutype,ftstatus)
              if (ftstatus .ne. 0) then
                 msg = 'cannot move to desired extension'
                 call fcerr(msg)
                 go to 999
              endif
C     - grab the HDUCLAS values for reference
c              ninstr = 1
c              instr(1) = '*'
c              nsearch = 1
c             call fndhud(chatter, iunit, ninstr, instr, nsearch,
c     $             nfound, next, outhdu, outver, extnam, ierr)
c              nfound = 1
c              next(1) = 0
c              ftype = outhdu(2,1)

           else
              
C     - SEARCH for appropriate extension via the PCU ID, and method
              n = 2
              found = .false.
              do while (.not. found)
                 
                 call ftmahd(iunit,pcuid+n,hdutype,ftstatus)
                 if (ftstatus .ne. 0) then
                    msg = 'cannot move to correct extension in c2e file'
                    call fcerr(msg)
                    go to 999
                 endif

C     - Check that the extension is of the right type (CCNM0001 = 'E2C_PARM')
                 call ftgkys(iunit,'CCNM0001',ccnm0001,comment,ftstatus)
                 if (ftstatus .ne. 0) then
                    msg = 'cannot read CCNM0001 keyword'
                    call fcerr(msg)
                    go to 999
                 endif
                 if (ccnm0001 .ne. 'E2C_PARM') then
                    msg = 'Wrong type of calibration file '//
     $                   '- need e2c type file'
                    call fcecho(msg)
                    ierr = 1
                    go to 999
                 endif
                 
C     - Check the PCUID, the Method number and the time
                 call ftgkys(iunit,'DETNAM',mpcuid,comment,ftstatus)
                 if (ftstatus .ne. 0) then
                    msg = 'cannot get DETNAM value'
                    call fcerr(msg)
                    go to 999
                 endif

                 call ftc2i(mpcuid(4:4),id,ftstatus)

                 if (id .eq. pcuid) then
                    call ftgkyj(iunit,'CONVMETH',convmeth,comment,
     $                   ftstatus)
                    if (ftstatus .ne. 0) then
                       msg = 'cannot get CONVMETH value'
                       call fcerr(msg)
                       go to 999
                    endif

                    if (convmeth .eq. model) then
                       call dt2mjd(obs_date,quiet,mjd_obs,ftstatus)
                       if (ftstatus .ne. 0) then
                          msg = 'Unable to determine MJD from obs_date'
                          call fcecho(msg)
                          go to 999
                       endif
                       call ftgkys(iunit,'CVSD0001',refdate,comment,
     $                      ftstatus)
                       if (ftstatus .ne. 0) then
                          msg = 'cannot get CVSD0001 value'
                          call fcerr(msg)
                          go to 999
                       endif
                       call dt2mjd(refdate,quiet,mjd_ref,ftstatus)
                       if (ftstatus .ne. 0) then
                          msg = 'Unable to determine MJD from refdate'
                          call fcecho(msg)
                          go to 999
                       endif
                       if (mjd_obs .gt. mjd_ref) then
                          found = .true.
                          extno(1) = pcuid + n - 1
                       endif
                                              
                    endif

                 endif

                 n = n + 5
              end do
              
           endif
                   
C Read the pcuid from the channel-to-energy file header and double check
           
C Get the number of rows
           call ftgkyj(iunit,'NAXIS2',nrows,comment,ftstatus)
           if (ftstatus .ne. 0) then
              msg = 'cannot get number of rows'
              call fcerr(msg)
              go to 999
           endif

C Get the number of columns
           call ftgkyj(iunit,'TFIELDS',ncols,comment,ftstatus)
           if (ftstatus .ne. 0) then
              msg = 'cannot get number of columns'
              call fcerr(msg)
              go to 999
           endif
           if (ncols .gt. 10) then
              msg = 'too many parameter columns in FITS file'
              call fcerr(msg)
              ierr = 1
              go to 999
           endif

C Get the CDES0001 keyword value
           call ftgkys(iunit,'CDES0001',cdes,comment,ftstatus)
           if (ftstatus .ne. 0) then
              msg = 'cannot read CDES0001 keyword'
              call fcerr(msg)
              go to 999
           endif

C     Read the columns
           frow = 1
           felem = 1
           nullval = -99
           call ftgcno(iunit,.false.,'LLD',lldcol,ftstatus)
           if (ftstatus .ne. 0) then
              msg = 'cannot get LLD column number'
              call fcerr(msg)
              go to 999
           endif
           call ftgcvj(iunit,lldcol,frow,felem,nrows,nullval,mlld,
     $          anyf,ftstatus)
           if (ftstatus .ne. 0) then
              msg = 'cannot get LLD values'
              call fcerr(msg)
              go to 999
           endif

C     9Apr98 MJT: ftgcve expects a REAL for nullval!
           nullvale= -99.0
           do j = 1,ncols-1
              call fti2c(j,parmnum,ftstatus)
              parmname = 'PARM'//parmnum(20:20)
              call ftgcno(iunit,.false.,parmname,parmcol,ftstatus)
              if (ftstatus .ne. 0) then
                 msg = 'cannot get '//parmname//' column number'
                 call fcerr(msg)
                 go to 999
              endif

              if (j .eq. 1) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm1,anyf,ftstatus)
              if (j .eq. 2) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm2,anyf,ftstatus)
              if (j .eq. 3) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm3,anyf,ftstatus)
              if (j .eq. 4) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm4,anyf,ftstatus)              
              if (j .eq. 5) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm5,anyf,ftstatus)
              if (j .eq. 6) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm6,anyf,ftstatus)
              if (j .eq. 7) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm7,anyf,ftstatus)
              if (j .eq. 8) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm8,anyf,ftstatus)
              if (j .eq. 9) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm9,anyf,ftstatus)
              if (j .eq. 10) call ftgcve(iunit,parmcol,frow,felem,nrows,
     $             nullvale,parm10,anyf,ftstatus)
              if (ftstatus .ne. 0) then
                 msg = 'cannot get '//parmname//' values'
                 call fcerr(msg)
                 go to 999
              endif
           end do
           
              
C ** End JCL Addition
           
C Now match lld code.  Note that 63 (=xenon layer average) is accepted
C     for any signal layer.
           par(1) = -99.
	   j = 1
           do while (j .le. nrows .and. par(1) .eq. -99.)
              if (mlld(j) .eq. lld .or.
     &		   (lld .eq.  1 .and. mlld(j) .eq. 3) .or.
     &		   (lld .eq.  2 .and. mlld(j) .eq. 3) .or.
     &		   (lld .eq.  4 .and. mlld(j) .eq. 12) .or.
     &		   (lld .eq.  8 .and. mlld(j) .eq. 12) .or.
     &		   (lld .eq. 16 .and. mlld(j) .eq. 48) .or.
     &		   (lld .eq. 32 .and. mlld(j) .eq. 48) .or.
     $             (lld .le. 63 .and. mlld(j) .eq. 63) ) then
                 par(1) = parm1(j)
                 par(2) = parm2(j)
                 par(3) = parm3(j)
                 par(4) = parm4(j)
                 par(5) = parm5(j)
                 par(6) = parm6(j)
                 par(7) = parm7(j)
                 par(8) = parm8(j)
                 par(9) = parm9(j)
                 par(10) = parm10(j)
              endif              
	      j = j + 1
           end do

           if (par(1) .eq. -99.) then
              msg = 'Could not find parameters for channel-to-energy'
              call fcecho(msg)
              msg = '   conversion input calibration file.'
              call fcecho(msg)
              ierr = 1
              go to 999
           endif

 600    continue

        close (iunit)
        call ftfiou(iunit,ftstatus)

	end if

	if (noCalDB .eq. 1) then

        open (unit=35, file=c2efile, status='old',
     +        access='sequential', form='formatted')

        msg = ' '
        call fcecho(msg)
        msg = 'c2e Input outside FITS.  Format twitchy, be careful'
        call fcecho(msg)
	msg = 'patience is rewarded'
        call fcecho(msg)
        call fcecho(vrsn_no(3))
        call fcecho(' ')


	if (noCalDB .eq. 1) write (*,*) cdate0, mjd_obs0
	if (noCalDB .eq. 1) write (*,*) cdate,  mjd_obs


        do 500 i = 1,1000000

            read (35,8000,end=501) record200
8000        format (A200)
            read (record200,8001) cc
8001        format (A1)
            if (cc .eq. 'C') go to 500        !  skip over comments

            read (record200,8002) cc,ccdate,mmid,mlld(1),mmodel,par
8002        format(a1,1x,a10,i3,i4,i3,10(1x,1pe11.4))
c	    write (*,*) cc,mmid,mlld(1),mmodel,ccdate
	    call dt2mjd (ccdate,quiet,mjd_ref,ftstatus)

            if (mjd_ref .gt. mjd_obs) go to 500    ! skip over more recent history
            if (mmid .ne. pcuid) go to 500  ! skip over other slots
            if (mmodel .ne. model) go to 500  ! ship over other models

            if (mlld(1) .eq. lld) go to 601
            if (mlld(1) .eq. 3 .and.
     &          (lld .eq. 1 .or. lld .eq. 2) ) go to 601
            if (mlld(1) .eq. 12 .and.
     &          (lld .eq. 4 .or. lld .eq. 8) ) go to 601
            if (mlld(1) .eq. 48 .and. 
     &          (lld .eq. 16 .or. lld .eq. 32) ) go to 601
            if (lld .le. 63 .and. mlld(1) .eq. 63) go to 601
                                              ! above checks for exact lld code
                                              ! match, but accepts 63 (=xenon layer
                                              ! average) for any signal layer

 500    continue
 501    continue

        msg = 'ERROR - Could not find parameters for '
        call fcecho(msg)
        msg = 'channel-to-energy conversion in '//c2efile
        call fcecho(msg)
        ierr = 1
        go to 999

 601    continue
        close (35)

c 8-27-98  moved end if to here
	end if


c apply gain shift only for epoch 3 (i.e. dates beyond 1996-04-14)
c  3-07-00 apply correction in all epochs
c	if (mjd_obs .gt. 50187.0) then
	    par(1) = par(1) + (par(5) * (mjd_obs - mjd_obs0) + 
     $		par(7)*(mjd_obs - mjd_obs0)**2)
	    par(2) = par(2) + (par(6) * (mjd_obs - mjd_obs0) + 
     $		par(8)*(mjd_obs - mjd_obs0)**2)* par(4)
c energy scale shift - test added 08-29-00
	    par(2) = par(2)*escale
c	end if

	chanemax = 0.

        
        icall = 1
        if (model .eq. 1) npar=2
        if (model .eq. 2) npar=3
c 03-06-00 to get documentation of last parameter
	if (model .eq. 3) npar=8
        end if


c all subsequent visits to this function end up here, using parameters previously
c   stored
c
	if (iflag .eq. 0) rch=ich-0.5
	if (iflag .eq. 1) rch=ich+0.5


	if (model .eq. 1) then
	    chan2energy = (rch-par(1))/par(2)
	    return
	end if

	if (model .eq. 2 .or. model .eq. 3) then
	    a =par(3)
	    b1=par(2)
	    c1=par(1)-rch
	    xel = b1*b1 - 4*a*c1
	    if (xel .gt. 0.0) then
	       el1 = (-b1 + sqrt(b1*b1 - 4*a*c1))/(2.*a)
 	       el2 = (-b1 - sqrt(b1*b1 - 4*a*c1))/(2.*a)
	       if (a.gt.0) then
	          chan2energy = max(el1,el2)
	        else
	          chan2energy = min(el1,el2)
	       end if
	       if (chan2energy .gt. chanemax) chanemax=chan2energy
	     else
		chan2energy = chanemax
	     end if
	    if (model .eq. 2) return
	end if

c       04-09-2009  NS: solution of cubic equation through itterations

	 tol = 1.0e-6
	 tolit = 1.0
	 itmax = 20
	 e0 = chan2energy
	 i = 1
	 e1 =(rch-par(1)-par(3)*e0*e0-par(4)*e0*e0*e0)/par(2)

c	     write (*,*) par(1),par(2),par(3),par(4),e0,e1
 

	     
	     do while (i.lt.itmax.and.tolit.gt.tol) 
	     e1 =(rch-par(1)-par(3)*e0*e0-par(9)*e0*e0*e0)/par(2)
	     tolit = abs(e1-e0)/e0
	     e0 = e1
	     i = i+1
c	     write (*,*) i,e0,e1,tolit
	     enddo
             
        chan2energy = e1
	return
        
 999	icall = 1
	if (ftstatus .ne. 0) then
	   ierr = 1
	   call fcerrm(ftstatus)
	   call ftclos(iunit,ftstatus)
	   call ftfiou(iunit,ftstatus)
	endif
        return
        end



	function energy2Rchan(e)

C
c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

        real*4     par(10)
        integer*4  npar, noCalDB
        common   /c2epar/  par, npar, noCalDB

        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef

c local parameters
	real*4	 e
	real*4   energy2Rchan
	

	    energy2Rchan = par(1) + par(2)*e + par(3)*e*e 
     &		+ par(9)*e*e*e


c
	return 
	end
C *************************************************************************
C SUBROUTINE:
C       chanwidth
C
C DESCRIPTION:       
C       returns the width of the energy channels
C
C AUTHOR:
C       Keith Jahoda
C
C MODIFICATION HISTORY:
C
C NOTES:
C 5-30-95  at present this is just a placeholder in case it is necessary to
C       correct for unequal channel widths
C 1-20-96  measures channel widths in keV to get normalization right
C            for respch
C 1-24-96  added xenon_data so we can get effective channel widths
C            across Xenon edges
C           added c2epar block
C 1-31-96  updated call to chan2energy
C 2-02-96  3 L edges into xenon_data
C 2-08-96   chanwidth is returned as the slope of the chan2energy law
C            (i.e. par(2))  extraneous and unused code removed
C 11-25-96 added adjustment for edges using e2c_model=2
C            required small change in respch.f as well
C version 2.1.2 - corrected apparent error in calculateing width of
C   channels near L edge for model 3.  Model 2 uncertain and not recommended
c version 2.2  - uses 1/slope as chanwidth.  This gives channels equal
c   width - worth examining (3/2/98)
C
C USAGE:
C     x = chanwidth(channel)
C
C ARGUMENTS:
C
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED SUBROUTINES:
C
C********************************************************************
        function chanwidth(ich,eelo,eehi)

c calling sequence declarations
        real*4     chanwidth
        real*4     eelo(0:255), eehi(0:255) 
        integer*4  ich

c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

	real*4	   par(10)
	integer*4  npar, noCalDB
	common	   /c2epar/ par, npar, noCalDB

        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef


 

c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

        data	vrsn_no(4) /'chanwidth      v7.0   12-05-99'/


 
	chanwidth = eehi(ich) - eelo(ich)
	chanwidth = 1./par(2)

	return
	end


C *************************************************************************
C FUNCTION:
C       effic
C
C DESCRIPTION:       
C
C
C AUTHOR:
C       Keith Jahoda
C
C MODIFICATION HISTORY:
C  6-25-95 v0.0 will fail for lld .ne. 1,2,4,8,16,32,63, or 64
C  8-11-95 v0.1  corrected error in calculation for lld=63 (t(3)-t(6) was
C            t(6)-t(3) )
C  1-14-96  modified to include xenon in the propane layerC
C  1-20-96  added constant kEdge_veto to reduce efficiency above
C                   K edge
C  1-24-96  added DeltaE_L, _K to xenon_data
C  2-02-96  3 L edges into xenon_data
C  2-08-96  option to apply global correction comes in through
C             parameter scale_hack
C  3-13-96  hard coded a scale factor that corrects between 18 and 34.5 keV
C             (approximately) for photo-electron range and self vetoing.
C             can be overridden with scale_hack option
C  3-29-96  cleaned up set up in function mar96scale
C           Added common block /quantumef/ to import gloabal efficiency fudge
C  4-04-96  added functionality for lld_code=60 (L2+R2+L3+R3)
C  10-25-96 brought over to version 2.0
C  11-05-96  added methane to the mix
C  11-26-96 option to use a different set of xenon cross sections allowed
C        LBL_sigma = 0.   fits to Henke (paper) & Veigele pts between edges
C                           fits polynomials in log space
C                    1.   for energies below 30 keV, uses a table created from
C                           the LBL web site.  For energies above 30 keV, falls
C                           back to the 0 solution
C                           table very dense so interpolation doesn't matter
C                    2.   a table from McMaster that goes all the way to 100 keV.
C                           interpolation done in linear space
C
C  04-03-00 adds xenon boundary (i.e. dead) layer
C
C NOTES:
C
C USEAGE:
C     x = effic(energy)
C
C ARGUMENTS:
C
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED SUBROUTINES:
C
C********************************************************************
        function effic (energy)

c calling sequence declarations
        real*4      energy, effic

c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

c local parameters
        integer*4   j
        real*4      xsec, asec, msec, psec
        character(80) msg
        
c detector parameters
        real*4     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3, xe_gmcm2_pr
        real*4     pr_gmcm2, my_gmcm2, al_gmcm2, del_xepr
        real*4     kEdge_veto, lbl, lEdge_veto, xe_gmcm2_dl
   
        common     /detector/     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3,
     &				      xe_gmcm2_pr, del_xepr,
     &                                pr_gmcm2, my_gmcm2, al_gmcm2,
     &                                kEdge_veto, lbl, lEdge_veto, 
     &				     xe_gmcm2_dl

	real*4	   area_factor
	common	   /quantumef/    area_factor

c xenon parameters
        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef

c methane stuff
	real*4	    ch4sec, ch4_gmcm2

c local parameters
        real*4     t(8), scale

c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

        vrsn_no(12) = 'effic        v7.0   12-05-99'

        effic=0.0

c get cross sections
        call aluminum (energy, asec)
	if (asec .lt. 0) asec = 10./al_gmcm2
        call mylar (energy, msec)
	if (msec .lt. 0) msec = 10./my_gmcm2
        call propane (energy, psec)
	if (psec .lt. 0) psec = 10./pr_gmcm2
	if (lbl .gt. 0.5 .and. lbl .lt. 1.5) then
	    call xenonlbl (energy, xsec)
	  else if (lbl .gt. 1.5) then
	    call xenonmcm (energy,xsec)
	  else
	    call xenon (energy, xsec)
	end if
c        call xenon (energy, xsec)
	if (xsec .lt. 0) xsec = 10./xe_gmcm2_l1
	ch4_gmcm2 = xe_gmcm2_l1 * 0.123            * 0.1
c                             ratio of STP density,  ratio of pressures
	call ch4 (energy, ch4sec)
	if (ch4sec .lt. 0) ch4sec = 10./ch4_gmcm2

	scale=area_factor                        ! a bold faced fudge factor

C  transmission of first window (assumes half of al and half of mylar)
        t(1) = 1.0  * exp ( - asec*al_gmcm2/2. - msec*my_gmcm2/2.)

C  transmission of first window plus propane and xenon in fron layer
        t(2) = t(1) * exp ( - psec*pr_gmcm2 - xsec*xe_gmcm2_pr)

        if (lld .eq. 64) then
            effic = (t(1)-t(2)) * scale
            go to 100
            end if

C  transmission of ... second window
        t(3) = t(2) * exp ( - asec*al_gmcm2/2. - msec*my_gmcm2/2.)  

   
C  transmission of  ... first xenon layer
	ch4_gmcm2 = xe_gmcm2_l1 * 0.0123
        t(4) = t(3) * exp ( - xsec*xe_gmcm2_l1 - ch4sec*ch4_gmcm2 )

        if (lld .eq. 1 .or. lld .eq. 2) then
            effic = (t(3)-t(4)) * 0.5
     &		*scale
            go to 100
            end if
        if (lld .eq. 3) then
            effic = (t(3)-t(4)) 
     &		*scale
	t0 =  exp (-1.*psec*pr_gmcm2)
	t00= exp (-1.*xsec*xe_gmcm2_pr)
c	write (*,8000) energy,t(1),t(2),t(3),t(4),t0,t00
8000	format ('eff ',7(F12.5,1x))
c	write (*,8001) energy,xsec,asec,msec,psec,ch4sec
8001	format ('xsec',6(F12.5,1x))
	    go to 100
            end if

c transmission of boundary layer
	t(5) = t(4) * exp ( - xsec*xe_gmcm2_dl )

C  transmission of  ... second xenon layer
	ch4_gmcm2 = xe_gmcm2_l2 * 0.0123
        t(6) = t(5) * exp ( - xsec*xe_gmcm2_l2 - ch4sec*ch4_gmcm2 )

        if (lld .eq. 4 .or. lld .eq. 8) then
            j = lld/4
            effic = (t(5)-t(6)) * 0.5
     &		*scale
	    go to 100
            end if
        if (lld .eq. 12) then
            effic = (t(5)-t(6)) 
     &		*scale
	    go to 100
            end if
            
c transmission of boundary layer
	t(7) = t(6) * exp ( - xsec*xe_gmcm2_dl)

C  transmission of  ... third xenon layer
	ch4_gmcm2 = xe_gmcm2_l3 * 0.0123
        t(8) = t(7) * exp ( - xsec*xe_gmcm2_l3 - ch4sec*ch4_gmcm2 ) 

        if (lld .eq. 16 .or. lld .eq.32) then
            j = lld/16
            effic = (t(7)-t(8)) * 0.5
     &		*scale
	    go to 100
            end if
        if (lld .eq. 48) then
            effic = (t(7)-t(8)) 
     &		*scale
	    go to 100
            end if

        if (lld .eq. 60) then
            effic = (t(5)-t(6) + t(7)-t(8)) 
     &		*scale
	    go to 100
            end if

        if (lld .eq. 63) then
c            effic = (t(4)-t(3) + t(5)-t(6) + t(7)-t(8))  repaired sept 29, 2000
            effic = (t(3)-t(4) + t(5)-t(6) + t(7)-t(8)) 
     &		*scale
	    go to 100
         end if

         msg = 'ERROR - Unable to decode lld (vrsn_no(12)'
         call fcecho(msg)
 100	 continue
c	write (*,8002) xe_gmcm2_dl, t, energy
8002	format ('ttt',F6.4,1x,8(F7.3),2x,F8.2)

         return
         end

        function jlld(lld)
c
c  5-29-95 reduces lld code to 1,2,3 (layer) or 4 (total)
c       does not check for nonsensical values of lld,  returns jlld=0
c       for most non-expected values of lld
C  4-07-96  added case for lld=60
C  4-14-97  added hack to return jlld=1 for lld=64.  This allows
C        pcarmf to function above L edge.

c calling sequence declarations
        integer*4  jlld, lld

c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

        vrsn_no(9) = 'jlld         v2.1.2,  04-14-97'

        jlld = 0
        if (lld .le. 3) jlld = 1
        if (lld .ge. 4 .and. lld .le. 12) jlld=2
        if (lld .ge.16 .and. lld .le. 48) jlld=3
	if (lld .eq. 60) jlld=2
        if (lld .eq. 63) jlld = 4
c this final hack to make pcarmf work above Xe-L edge for proane layer
        if (lld .eq. 64) jlld = 1
        return
        end
C*********************************************************************
C SUBROUTINE:
C       mat_energ
C
C DESCRIPTION:       
C       sets up the energy channels for PCA RMF and ARF generators
C
C AUTHOR:
C       Keith Jahoda
C
C MODIFICATION HISTORY:
C   1-30-96  gets edge energies from xenon_data block
C   2-02-96  e L edges in xenon_data
C   2-08-96  uses half of the energies below 10 keV (assumes 3 edges)
C              and half above 10 keV
C   10-25-96 brought v1.07 to v2.0
C   04-15-20 changed lower limit to 0.23 keV (from 0.023) and changed channel spacing to logarithmic
C              channels that would encompass an edge are still split in two.
c   04-16-02 changed energy channels to logarithmic spacing
C   08-15-03 reduced energy range and reduced number of channels
C
C NOTES:
C       original version  5-31-95   by Keith Jahoda
C       Sets up equally spaced channels but with extra
C         channels near edges.  
C       Behaviour not guaranteed if edges are closer together than
C         typical channel spacing
C
C ARGUMENTS:
C
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED SUBROUTINES:
C
C********************************************************************
	subroutine mat_energ (E_MIN, E_MAX, MAXEN)

c calling sequence declarations
	integer*4 MAXEN, i
	real*4    E_MIN(*), E_MAX(*)

c global declarations
        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef


c local declarations
	integer*4 n_edge
        data      n_edge  /4/
        real*4    edges(0:4)            ! needs n_edge+1 elements
        real*4    e_lo, e_hi
        data      e_lo, e_hi   /1.5,  80./
        integer*4 j


c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

        vrsn_no(6) = 'mat_energ     v10.0   08-15-03'
 
	edges(0) = xeL3edge
	edges(1) = xeL2edge
	edges(2) = xeL1edge
	edges(3) = xeKedge
	edges(4) = 1.e6

	rlo = alog10(e_lo)
	rhi = alog10(e_hi)
	xxx = float(MAXEN - n_edge )
	delta = (rhi - rlo)/xxx
	j = 0

	rr = rlo
	e_min(1) = 10.0**rr
	do i = 1, MAXEN-1
	   rr = rr + delta
	   ee = 10.0**rr
	   if (ee .lt. edges(j)) then
	     e_max(i) = ee
	     e_min(i+1) = ee
	   end if
	   if (ee .gt. edges(j)) then
	     e_max(i) = edges(j)
	     e_min(i+1) = edges(j)
	     rr = rr - delta
	     j = j + 1
	   end if
	end do
	e_max(MAXEN) = e_hi

	return
	end
C *************************************************************************
C FUNCTION:
C       respch
C
C DESCRIPTION:       
C 	returns the normalized response in channel ICH
C      		to input energy                       E0
C      		for pcu                               PCUID
C      		and lld                               LLD
C
C AUTHOR:
C       Keith Jahoda
C
C MODIFICATION HISTORY:
C
C NOTES:
C  5-30-95
C      recognized values for PCUID 1,2,3,4,5 (correspond to slots)
C                               and 0        (generic or average)
C      recognized values for LLD   1,2,3     L1, R1, <L1,R1>
C                                  4,8,12    L2, R2, <L2,R2>
C                                  16,32,48  L3, R3, <L3,R3>
C                                  63        average for all layers
C                                  64        VP
C  1-24-96  changed calculation of ech
C             required adding /c2epar/
C  1-26-96  changed call to energy2Rchan for i0
C  2-04-96  corrected heinous error in calculation of delta_ch
C  2-27-96  added /xenon_data/
C  3-06-96  modified so that response calculation is done in
C             channel space (i.e. absorbed respe.f)
C  3-28-96  ensured that wch is initialized in each loop (sets the normalization
C             for each energy)
C  10-25-96 copied to v2.0
C             tries to distinguish which items need energy (e0)
C                                               and energy_prime (ep0)
C             escape energies now counted (consistently) in electrons
C             so the Xenon_data block has new parameters
C  version 2.1.1 - e2c_model is preferred.
c  version v2.2  - includes an attempt to deal with lld of each channel
c  version 3.0   - has a partial charge collection parameterization, although
c                   default calls this with a zero norm.  This could stand some
c                   investigation
c  version 3.1   - electron track exponent comes in through parameter file
c                - removed iflag from calls to energy2Rchan (4/3/98)
c  version 3.6   - added Jahoda and McCammon parameterization of partial charge
c                    collection
c  version 10.0  - repaired some small errors in the code
C
C
C USEAGE:
C
C ARGUMENTS:
C
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED SUBROUTINES:
C	chanwidth
C 	respe
C********************************************************************
	subroutine respch(r256,e0,energ_lo,energ_hi)

c  calling sequence declarations
        real*4     e0,escape_width
	real*4     r256(0:255)
	real*4	   energ_lo(0:255), energ_hi(0:255)
        integer*4  ich

c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

	real*4	par(10)
	integer*4  npar, noCalDB
	common    /c2epar/	par, npar, noCalDB

c detector parameters
        real*4     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3, xe_gmcm2_pr
        real*4     pr_gmcm2, my_gmcm2, al_gmcm2, del_xepr
        real*4     kEdge_veto, lbl, lEdge_veto, xe_gmcm2_dl
   
        common     /detector/     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3,
     &				      xe_gmcm2_pr, del_xepr,
     &                                pr_gmcm2, my_gmcm2, al_gmcm2,
     &                                kEdge_veto, lbl, lEdge_veto, 
     &				     xe_gmcm2_dl

        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef

c  parameterization for partial charge collection
        real*4  pcc, kappa, r256p(0:255)

        common  /partial/       pcc


c external functions
	real*4     energy2Rchan, sigmaf
	real*4     energy2energyprime, w_xe
	integer*4  jlld

c  local declarations
c        real*4     wch, chanwidth
	real*4     i0, i1
	real*4	   ibeta
	real*4	   sigma0, sigma1, sigma2, sigma_beta
	real*4     pi
	real*4     ep0, el0, ela, eka, ekb, epa, epb
	real*4     dnl0, dnl, dnka0, dnka, dnkb0, dnkb
	real*4     we0
	real*4	   unvetoed, unvetoedl, unvetoedka, unvetoedkb
	real*4	   sigxe,sig5,ratio,sqt2pi
	integer*4  i, j, k, imodel

        data pi                    /3.14159256/

c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 
        vrsn_no(2) = 'respch       v10.0   08-15-03'

	escape_width = 1.5
	efficiency = effic(e0)
	sqt2pi =  1.0/sqrt(2.0*pi)
        we0=0.0
  
c	if (model .eq. 3) ep0 = energy2energyprime(e0)
c	if (model .eq. 2) ep0 = e0
	imodel = model
	ep0 = energy2energyprime(e0, imodel)

	i0 = energy2Rchan(ep0) 

	if (i0 .lt. 0 .or. i0 .gt. 255) then
	    do i=0,255
	      r256(i)=0.
	    end do
c	    write (*,*) 'e0,i0 ',e0,i0
	    return
	end if
	sigma0 = sigmaf(ep0)
c	write (*,*) 'e, sig ',e0,ep0,sigma0
	if (imodel .eq. 3) we0 = w_xe(e0,imodel)
	if (imodel .eq. 2) we0 = w_xe(e0,imodel)

	el0 = ep0*1000./22.                      ! number of electrons in full peak
c	write (*,*) ep0, el0, we0, sigma0

	dnl0 = 1000.*EscEnerLa/we0
	dnl  = dnl0 + delta_el_L
	dnka0= 1000.*EscEnerKa/we0
	dnka = dnka0 + delta_el_Ka
	dnkb0= 1000.*EscEnerKb/we0
	dnkb = dnkb0 + delta_el_Kb

	k  = lld
	j  = jlld(k)
c

C no escape peaks because e0<xeL3edge
C notice normalization (11-25-96)
c
c    r0 = (kev/ch * ch)**-1
c    r0*wch = kev/ch * r0 =  1/ch  (compare to chanwidth.f for model 2)
c
	if (e0 .le. xeL3edge) then
	  call point (e0, unvetoed, j)
c	  ich = i0
c	  wch = 1.0/par(2)
c	  r0 = 1./(sqrt(2.*pi)*sigma0)
	  do ich=0,255
c	    wch = chanwidth(ich,energ_lo,energ_hi)
	    r256(ich) = sqt2pi*exp(-1.*(ich-i0)**2/(2.*sigma0*sigma0))
     &		*unvetoed*efficiency/sigma0
	  end do
c	write (*,9000) e0,ep0,unvetoed,dum,dum,dum,dum,dum,dum
9000	format (9F10.4)
	  go to 100
	end if


C L escape peak possible
	if (e0 .le. xeKedge) then
	    ela = el0 - dnl               ! equivalent electrons in esc peak
	    epa = ela*22./1000.
	    call point (epa, unvetoedl, j)
	    i1  = energy2Rchan(epa)
	    sigma1 = sigmaf(e0-EscEnerLa)
	    ich=i0
	    call point (e0-xeL3edge, unvetoed, j)
c	    wch = 1.0/par(2)
c	    r0     = 1./(sqrt(2.*pi)*sigma0)
c	    ich=i1
c	    wch = 1.0/par(2)
c	    r1     = 1./(sqrt(2.*pi)*sigma1)
	    do ich=0,255
c	     wch = chanwidth(ich,energ_lo,energ_hi)
	     r256(ich) =
     &        ((1.-EscFracL(j))*
     &             exp(-1.*(ich-i0)**2/(2.*sigma0*sigma0)) 
     &		*unvetoed/sigma0 +
     &	         EscFracL(j) *
     &             exp(-1.*(ich-i1)**2/(2.*sigma1*sigma1)) 
     &		*unvetoedl/sigma1)
     &			 *efficiency*lEdge_veto*sqt2pi


c	write (*,9000) e0,ep0,unvetoed,ela,epa,unvetoedl,dum,dum,dum
C add L escape lines back in
	      if (e0.ge.xeL3edge.and.e0.le.(xeL3edge+escape_width)) then
	         aa = efficiency*(1.-lEdge_veto)
c  assume that events that start on this layer (and in fact get distributed
c  among 5 other layers) actually end up on this layer.
	         ibeta  = energy2Rchan(EscEnerLa)
	         sigma_beta  = sigmaf(EscEnerLa)
		 rbeta = 1./(sqrt(2.*pi)*sigma_beta)
c	write (*,*) ich, ibeta, e0, r256(ich), efficiency, sigma_beta
c		r2 = exp(-1.*(ich-ibeta)**2/(2.*sigma_beta*sigma_beta))
                 r256(ich) = r256(ich) + aa*rbeta*
     &                       EscNormLa(j)*exp(-1.*(ich-ibeta)**2/
     &                       (2.*sigma_beta*sigma_beta))
     


	      endif

	    end do
	    go to 100
	end if

C K escape peak possible (L escape ignored, and blended in with main peak
C      anyway)
	eka = el0 - dnka
	epa = eka*22./1000.
	call point (epa, unvetoedka, j)
	i1  = energy2Rchan(epa)
	sigma1 = sigmaf(e0-EscEnerKa)
	ekb = el0 - dnkb
	epb = ekb*22./1000.
	call point (epb, unvetoedkb, j)
	i2  = energy2Rchan(epb)
	sigma2 = sigmaf(e0-EscEnerKb)
	ich=i0
	call point (e0-xeKedge, unvetoed, j)
c	wch = 1.0/par(2)
c	r0     = 1./(sqrt(2.*pi)*sigma0)
c	ich=i1
c	wch = 1.0/par(2)
c	r1     = 1./(sqrt(2.*pi)*sigma1)
c	ich=i2
c	wch = 1.0/par(2)
c	r2     = 1./(sqrt(2.*pi)*sigma2)
	do ich=0,255
c	     wch = chanwidth(ich,energ_lo,energ_hi)
	     r256(ich) =( (1.-EscFracKa-EscFracKb)*
     &		exp(-1.*(ich-i0)**2/(2.*sigma0*sigma0))
     &		*unvetoed/sigma0 +
     &	      EscFracKa*exp(-1.*(ich-i1)**2/(2.*sigma1*sigma1))
     &		*unvetoedka/sigma1 +
     &	      EscFracKb*exp(-1.*(ich-i2)**2/(2.*sigma2*sigma2))
     &		*unvetoedkb/sigma2 )
     &        *efficiency*kEdge_veto*sqt2pi


C add escape lines back in
	      if (e0.ge.xeKedge.and.e0.le.(xeKedge+escape_width)) then
	         aa = efficiency*(1.-kEdge_veto)
c  assume that events that start on this layer (and in fact get distributed
c  among 5 other layers) actually end up on this layer.
c	         ialpha = energy2Rchan(EscEnerKa)
c	         sigma_alpha = sigmaf(EscEnerKa)
c		 ralpha = 1./(sqrt(2.*pi)*sigma_beta)
	         ibeta  = energy2Rchan(EscEnerKb)
	         sigma_beta  = sigmaf(EscEnerKb)
		 rbeta = 1./(sqrt(2.*pi)*sigma_beta)
c	write (*,*) ich, ibeta, e0, r256(ich), efficiency, sigma_beta
c		r2 = exp(-1.*(ich-ibeta)**2/(2.*sigma_beta*sigma_beta))
                 r256(ich) = r256(ich) + aa*rbeta*
     &                       EscNormKb*exp(-1.*(ich-ibeta)**2/
     &                       (2.*sigma_beta*sigma_beta))
     
c     &		        EscNormKa*ralpha*exp(-1.*(ich-ialpha)**2/(2.*sigma_alpha*sigma_alpha)))
c     &			*DeltaE_K
c	         r256(ich) = r256(ich) + r1
c	write (*,*) ich, ibeta, e0, r1, aa, rbeta, r2, DeltaE_K
CCC  total fake here just to get a multiplier in

c     &			*EscFracKb
c     &			* 5.
c	write (*,*) ich, e0, r1, aa, ibeta, sigma_beta, ' b'

c
c     &			aa*EscFracKa*wch*rbeta*
c     &			exp(-1.*(ich-ialpha)**2/(2.*sigma_alpha*sigma_alpha)) +
	

	      endif

	end do


c	write (*,9000) e0,ep0,unvetoed,eka,epa,unvetoedka,ekb,epb,unvetoedkb

 100	continue

c correct for partial charge collection
c	call xenon (e0, sigxe)
c	call xenon (5.0, sig5)
c	ratio = sigxe/sig5
c v3.5 formulation - effectively turned off by small default for pcc
c	sum = 0.
c	sumt= 0.
c	ratio = ratio*pcc
c	if (ratio .ge. 0.10) ratio=0.10
c	do i=0,255
c	    sum=sum+ratio*r256(i)
c	    sumt=sumt+r256(i)
c	end do
c	do i=0,255
c	    r256(i)=r256(i)*(1.0-ratio)
c	end do
c	do i=0,i0
c	    r256(i)=r256(i)+sum/(1.0*(i0+1))
c	end do
c	write (*,*) e0,sum,sumt,pcc,ratio

c v3.6 9-2-98 adopted from Jahoda and McCammon
c   r256p = Q' in the JM formulation
c   ratio * pcc = kappa
	if (pcc .gt. 1.0e-06) then
	do i=0,255
	   r256p(i) = r256(i)
	end do
	call xenon (5.0, sig5)
	call xenon (e0, sigxe)
	ratio = sigxe/sig5
	kappa = ratio * pcc

        imax = i0*1.25
        if (imax .gt. 255) imax = 255

        do i = 1,imax-1
c         
c     losses to lower channels
           do m = 0,i-1
              r256(i) = r256(i) - r256p(i) * 
     &          kappa * (1. - (float(m)/float(i)))**(kappa - 1.) *
     &          (1./float(i))
           end do

c     gains from higher channels
           do n = i+1,imax
              r256(i) = r256(i) + r256p(n) *
     &          kappa * (1. - (float(i)/float(n)))**(kappa - 1.) *
     &          (1./float(n))
           end do
	end do
	end if
	
c	if (e0 .gt. 4.7 .and. e0 .lt. 5.0) then
c	    do i=0,255
c		write (*,8100) e0,i,r256(i),r256p(i)
c	    end do
c	end if
c 8100	format (f8.3,i5,2f10.5)
	
	call detlld(r256)


	return
        end



	subroutine point (e0, unvetoed, j)

	real*4	e0, unvetoed
	integer*4	j
c
	real*4	epoint, d5, d6
	real*4  dd


	common	/etracks/  epoint, d5, d6

c d6 used to be specified in data statement
c	data	eee	/1.86/
c
c       uses parameterization from HEAO/OSO programs
c

	dd=d5
	if (j .eq. 2 .or. j .eq. 3) dd=1.33*d5
	unvetoed = 1.0 - dd*(e0/epoint)**d6

	return
	end


	subroutine detlld (r256)

c  calling sequence declarations
	real*4     r256(0:255)

c global parameters 
        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

	integer*4	j, klld, jlld
	real*4	rlld (0:7,3,0:4)

	data  rlld   /  0.0,  0.0,  0.0,  0.0,  0.0,  0.2,  0.35,  1.0,
     &			0.0,  0.0,  0.0,  0.0,  0.0,  0.25, 0.75,  1.0,
     &			0.0,  0.0,  0.0,  0.0,  0.25, 0.5,  1.0,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.3,  0.08, 1.0,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.3,  0.7,  0.75,  1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.25, 0.8,  1.0,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.2,  0.2,  0.75,  1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.3,  0.03, 0.7,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.3,  0.25, 1.0,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.2,  0.2,  0.7,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.4,  0.3,  1.0,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.0,  0.3,  0.8,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.3,  0.15, 1.0,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.4,  0.3,  1.0,   1.0,
     &                  0.0,  0.0,  0.0,  0.0,  0.2,  0.3,  1.0,   1.0/



	j=lld
	klld=jlld(j)

	do j=0,7
	    r256(j)=r256(j)*rlld(j,klld,pcuid)
	end do

	return
	end






C***********************************************************************
C SUBROUTINE:
C     rmffpar
C
C DESCRIPTION:
C     Obtain parameters for pcarmf task.  Also write parameters to
C       output file
C
C AUTHOR:
C     Keith Jahoda
C
C MODIFICATION HISTORY:
C     7-19-95;  limit checking/core dumping on detector number added
C     8-11-95;  changed area keywords to 0-4 (from 1-5) scheme;
C                default values changed to 1 (from 700)
C     1-13-96;  changed dimension of pca_area from (5,2) to (0:4,2)
C                (should have done this with last change)
C     1-14-96;  added xenon thickness in propane layer to /detector/ block
C     1-20-96;  added kEdge_veto to /xenon_data/ block
C     1-21-96;  removed data statement for pca_area as this is read in
C                from parameter file.  Avoids confusion.  Maybe.
C     1-24-96;  added DeltaE_L and DeltaE_K to /xenon_data/
C     1-31-96;  added c2efile and chatter as parameters.  Also added
C                more graceful error exits through ierr (J. Lochner)
C     2-01-96   fixed error in computed goto (i.e. go to (1,2,3,4,5) pcuid+1)
C     2-02-96   added hidden parameter to allow by passing the CALDB for
C                chan2energy parameters
C               3 L edges into xenon_data
C     2-15-96   now reads in xe_gm_cm2_pr independently by pcuid
C     3-29-96   added common block /quantumef/ to carry global area fudge factor
C                 area_factor now appears in .par file
C     4-18-96   changed c2e labels in .par file to e2c.  LEFT VARIABLE NAMES
C                THEMSELVES UNCHANGED. (J. Lochner)
C     4-23-96   modified keywords to output file to reflect change to e2c.
C                Also write out name of calibr file, its extens and its CDES
C                value (J. Lochner)
C     10-26-96  modified /xenon_data/ block to read electron offsets,
C                brought to version 2.0
C     11-11-96  individual defaults for xenon thickness in various layers
C     12-23-96  added parameterization to do vetoing due to electron path length
C     01-01-97  added parameter to shift matrix channels
C     03-25-97  changed nonstandard keywords to history records
C     version 2.1.2  several non used parameters removed
C           also contains first Audio Ftool!
C     06-27-97  v2.2  adds sigf
C     08-07-97  mylar read in separately for each detector
c     02-27-98  added added 5 reserved keywords per detector
c		 (will allow pcarmf updates of code only, without new .par file;
c                 although new .par file will be needed to take advantage of additions)
c     03-02-98  used 3 keywords for date, offset, and slope for gain shift
c     03-11-98  removed EscFracLt, DeltaE_??, added track_exp
c     01-13-99  added cdate and cdate0 and associated common block;
c         changed input of date and date0 to character;
c         requires change to .par file
c     12-05-99  added w_xe scale factor
c     08-29-00  added fudge factor to channels per keV
c     02-11-02  added second xe_gmcm2_pr relationship
c     04-16-02  made dead layer individual to each detector
c
c     09-20-06  new parameters for EscLines above edge
C     
C NOTES:
C
C USEAGE:
C        call rmffpar (ounit, rspfile, c2efile, chatter, istatus, ierr)
C
C     ARGUMENTS:
C     ounit    - output unit number       
C     rspfile  - name of output response matrix file
C     c2efile  - name of channel-to-energy calibration file
C     chatter  - chatter parameter
C     istatus  - how to use this subroutine: 1 = read parameters
C                                            2 = write parameters
C
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED SUBROUTINES:
C
C************************************************************************
        subroutine rmffpar (ounit, rspfile, c2efile, chatter, istatus,
     $     ierr)
C
C   calling sequence declarations
        integer*4      ounit, chatter, istatus, ierr
        character(160)   rspfile, c2efile

C   local declarations
        integer*4      fstatus, i
        character(20)   extchar
	character(70)   char70
        character(80)   msg, histxt
	real*4		resp1, resp2
C JCL   data           chatter /25/
     
C  global declarations
        real*4     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3, xe_gmcm2_pr
        real*4     pr_gmcm2, my_gmcm2, al_gmcm2, del_xepr
	real*4     kEdge_veto, lbl, lEdge_veto, xe_gmcm2_dl
        common     /detector/     xe_gmcm2_l1, xe_gmcm2_l2, xe_gmcm2_l3,
     &				     xe_gmcm2_pr, del_xepr,
     &                               pr_gmcm2, my_gmcm2, al_gmcm2,
     &                               kEdge_veto, lbl, lEdge_veto, 
     &				     xe_gmcm2_dl

c	data	escape_width	/1.5/

	real*4	xe_gmcm2_pr_p, del_xepr_p
	common	/detector2/	xe_gmcm2_pr_p, del_xepr_p

        integer*4 pcuid, lld, date , model, scale_hack
        character(80)   scale_file
        common    /schain/   pcuid, lld, date , model, scale_hack,
     &          scale_file

	real*4		escale
	integer*4	date0

	common	/gainshift/	date0, escale

	character(20)	cdate, cdate0, cdate1
	real*8		mjd_obs, mjd_obs0, mjd_obs1
	common /dates/ mjd_obs, mjd_obs0, mjd_obs1, cdate, cdate0,cdate1 

        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
	real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     & 			      EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &			      delta_el_L, delta_el_Ka, delta_el_Kb, wxef


c parameterization for self vetoing losses of electron tracks
	real*4	epoint, d5, d6
	common	/etracks/ epoint, d5, d6

        real*4     par(10)
        integer*4  npar,    npar_l
	integer*4  noCalDB
        common   /c2epar/  par, npar, noCalDB

        real*4     area_factor 
        common     /quantumef/    area_factor 

        real*4  sigf
        common    /resolution/  sigf, resp1, resp2

c  parameterization for partial charge collection
	real*4	pcc

	common	/partial/	pcc

 
c  local declarations
        integer*4  status, ftstatus
	character(68)	obs_date, obs_date0, obs_date1
	logical		quiet

	data	quiet	/.false./
	data	ftstatus	/0/

c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no
        
c  documentation of the calibration file
        character(160) filenm(1)
        character(80) cdes
        integer extno(1)
        common /calfile/ filenm, extno, cdes

        vrsn_no(8) = 'rmffpar       v8.0,   04-16-02'
        ierr = 0
        status = 0

C istatus = 1 -> read the parameters
        if (istatus .eq. 1) then

        call uclgst ('outfile',rspfile,status)
        if (status .ne. 0) then
           msg = 'ERROR reading output file name'
           call fcerr(msg)
           ierr = 1
           go to 999
        endif
                    
        call uclgsi ('pcuid',pcuid,status)         ! should come from script
        if (pcuid .lt. 0 .or. pcuid .gt. 4) then
           msg = 'pcu number outside of allowed range of 0-4'
           call fcecho(msg)
           ierr = 1
           go to 999
        end if
        call uclgsi ('lld_code',lld,status)        ! should come from script
        call uclgsi ('e2c_model',model,status)
c	call system ('if -e /home/pcasrv2/keith/gunsmoke.au 
c     &      /home/lhea1/kcg/bin/play /home/pcasrv2/keith/gunsmoke.au &')
	if (model .le. 1 .or. model .ge. 4) then
	    msg = 'Only models 3 is supported by '
            call fcecho(msg)
	    msg = 'calibration data delivered with'
	    call fcecho(msg)
	    msg = 'pcarmf v8.0.  Please restart with e2c_model =  3'
	    call fcecho(msg)
	    ierr = 1
	    go to 999
	end if
        call uclgst ('e2cfile',c2efile,status)
	call uclgsi ('nofits',noCalDB,status)      !(this one not written to output)

        call uclgsi ('scale_hack',scale_hack,status)
        if (scale_hack .eq. 1)
     &      call uclgst('scale_file',scale_file,status)

        call uclgst ('cdate',cdate,status)           ! should come from script  
c	write (*,*) cdate, status

	chatter = 10
        call uclgsi ('chatter',chatter,status)
        if (status .ne. 0) then
           msg = 'ERROR reading chatter parameter'
           call fcerr(msg)
           ierr = 1
           go to 999
        endif
        
	call uclgsr ('xe_kedge_veto',kEdge_veto,status)
	call uclgsr ('xe_ledge_veto',lEdge_veto,status)
        call uclgsr ('pr_gmcm2',    pr_gmcm2,   status)
        call uclgsr ('al_gmcm2',    al_gmcm2,   status)


        call uclgsr ('xeKedge',      xeKedge,   status)
        call uclgsr ('xeL3edge',    xeL3edge,   status)
        call uclgsr ('xeL2edge',    xeL2edge,   status)
        call uclgsr ('xeL1edge',    xeL1edge,   status)
        call uclgsr ('EscFracKa',  EscFracKa,   status)
        call uclgsr ('EscFracKb',  EscFracKb,   status)
        call uclgsr ('EscFracL1',EscFracL(1),   status)
        call uclgsr ('EscFracL2',EscFracL(2),   status)
        call uclgsr ('EscFracL3',EscFracL(3),   status)
c       NS 08-07-07 adding escape line norms
c        call uclgsr ('EscNormKa',  EscNormKa,   status)
        call uclgsr ('EscNormKb',  EscNormKb,   status)
        call uclgsr ('EscNormLa1',  EscNormLa(1),   status)
        call uclgsr ('EscNormLa2',  EscNormLa(2),   status)
        call uclgsr ('EscNormLa3',  EscNormLa(3),   status)
c       NS	
        call uclgsr ('EscEnerKa',  EscEnerKa,   status)
        call uclgsr ('EscEnerKb',  EscEnerKb,   status)
        call uclgsr ('EscEnerLa',  EscEnerLa,   status)
	call uclgsr ('DeltaE_L3',    DeltaE_L3,   status)
	call uclgsr ('DeltaE_L2',    DeltaE_L2,   status)
	call uclgsr ('DeltaE_L1',    DeltaE_L1,   status)
	call uclgsr ('DeltaE_K',    DeltaE_K,   status)

	call uclgsr ('LBL_sigma',         lbl,    status)
	call uclgsr ('delta_el_L',  delta_el_L,   status)
	call uclgsr ('delta_el_Ka', delta_el_Ka,  status)
	call uclgsr ('delta_el_Kb', delta_el_Kb,  status)
  	call uclgsr ('area_factor', area_factor, status)
	call uclgsr ('epoint',       epoint,     status)
	call uclgsr ('track_coeff',      d5,     status)
	call uclgsr ('track_exp',        d6,     status)
	call uclgsr ('resolution_factor', sigf,  status)
	call uclgsr ('pcc_coeff', pcc, status)
	call uclgsr ('w_xe_fact', wxef, status)
	call uclgsr ('energy_scale', escale, status)
c       05-18-07 NS code !**************************************
c	call uclgsr ('escape_width', escape_width, status)
c       ********************************************************
    
        go to (1, 2, 3, 4, 5) pcuid+1

    1   call uclgsr ('xe_gm_cm2_l1_p0',xe_gmcm2_l1,status)   
        call uclgsr ('xe_gm_cm2_l2_p0',xe_gmcm2_l2,status) 
        call uclgsr ('xe_gm_cm2_l3_p0',xe_gmcm2_l3,status)
	call uclgsr ('xe_gm_cm2_pr0',xe_gmcm2_pr,status)
        call uclgsr ('my_gmcm2_p0',    my_gmcm2,   status)
        call uclgsr ('xe_gmcm2_dl0', xe_gmcm2_dl,   status)
	call uclgsr ('resp1_0',         resp1,   status)
	call uclgsr ('resp2_0',         resp2,   status)
	call uclgst ('cdate0_0',         cdate0,   status)
	call uclgsr ('xe_pr_daily_change0', del_xepr, status)
	call uclgst ('cdate1_0',         cdate1,   status)
	call uclgsr ('xe_gm_cm2_pr0_p',xe_gmcm2_pr_p,status)
	call uclgsr ('xe_pr_daily_change0_p', del_xepr_p, status)

        go to 10
    2   call uclgsr ('xe_gm_cm2_l1_p1',xe_gmcm2_l1,status)   
        call uclgsr ('xe_gm_cm2_l2_p1',xe_gmcm2_l2,status) 
        call uclgsr ('xe_gm_cm2_l3_p1',xe_gmcm2_l3,status)
	call uclgsr ('xe_gm_cm2_pr1',xe_gmcm2_pr,status)
        call uclgsr ('my_gmcm2_p1',    my_gmcm2,   status)
        call uclgsr ('xe_gmcm2_dl1', xe_gmcm2_dl,   status)
	call uclgsr ('resp1_1',         resp1,   status)
	call uclgsr ('resp2_1',         resp2,   status)
	call uclgst ('cdate0_1',         cdate0,   status)
	call uclgsr ('xe_pr_daily_change1', del_xepr, status)
	call uclgst ('cdate1_1',         cdate1,   status)
	call uclgsr ('xe_gm_cm2_pr1_p',xe_gmcm2_pr_p,status)
	call uclgsr ('xe_pr_daily_change1_p', del_xepr_p, status)
        go to 10
    3   call uclgsr ('xe_gm_cm2_l1_p2',xe_gmcm2_l1,status)   
        call uclgsr ('xe_gm_cm2_l2_p2',xe_gmcm2_l2,status) 
        call uclgsr ('xe_gm_cm2_l3_p2',xe_gmcm2_l3,status)
	call uclgsr ('xe_gm_cm2_pr2',xe_gmcm2_pr,status)
        call uclgsr ('my_gmcm2_p2',    my_gmcm2,   status)
        call uclgsr ('xe_gmcm2_dl2', xe_gmcm2_dl,   status)
	call uclgsr ('resp1_2',         resp1,   status)
	call uclgsr ('resp2_2',         resp2,   status)
	call uclgst ('cdate0_2',         cdate0,   status)
	call uclgsr ('xe_pr_daily_change2', del_xepr, status)
	call uclgst ('cdate1_2',         cdate1,   status)
	call uclgsr ('xe_gm_cm2_pr2_p',xe_gmcm2_pr_p,status)
	call uclgsr ('xe_pr_daily_change2_p', del_xepr_p, status)
        go to 10
    4   call uclgsr ('xe_gm_cm2_l1_p3',xe_gmcm2_l1,status)   
        call uclgsr ('xe_gm_cm2_l2_p3',xe_gmcm2_l2,status) 
        call uclgsr ('xe_gm_cm2_l3_p3',xe_gmcm2_l3,status)
	call uclgsr ('xe_gm_cm2_pr3',xe_gmcm2_pr,status)
        call uclgsr ('my_gmcm2_p3',    my_gmcm2,   status)
        call uclgsr ('xe_gmcm2_dl3', xe_gmcm2_dl,   status)
	call uclgsr ('resp1_3',         resp1,   status)
	call uclgsr ('resp2_3',         resp2,   status)
	call uclgst ('cdate0_3',         cdate0,   status)
	call uclgsr ('xe_pr_daily_change3', del_xepr, status)
	call uclgst ('cdate1_3',         cdate1,   status)
	call uclgsr ('xe_gm_cm2_pr3_p',xe_gmcm2_pr_p,status)
	call uclgsr ('xe_pr_daily_change3_p', del_xepr_p, status)
        go to 10
    5   call uclgsr ('xe_gm_cm2_l1_p4',xe_gmcm2_l1,status)   
        call uclgsr ('xe_gm_cm2_l2_p4',xe_gmcm2_l2,status) 
        call uclgsr ('xe_gm_cm2_l3_p4',xe_gmcm2_l3,status)
	call uclgsr ('xe_gm_cm2_pr4',xe_gmcm2_pr,status)
        call uclgsr ('my_gmcm2_p4',    my_gmcm2,   status)
        call uclgsr ('xe_gmcm2_dl4', xe_gmcm2_dl,   status)
	call uclgsr ('resp1_4',         resp1,   status)
	call uclgsr ('resp2_4',         resp2,   status)
	call uclgst ('cdate0_4',         cdate0,   status)
	call uclgsr ('xe_pr_daily_change4', del_xepr, status)
	call uclgst ('cdate1_4',         cdate1,   status)
	call uclgsr ('xe_gm_cm2_pr4_p',xe_gmcm2_pr_p,status)
	call uclgsr ('xe_pr_daily_change4_p', del_xepr_p, status)
        
10      continue

	obs_date = cdate
	call dt2mjd (obs_date,quiet,mjd_obs,ftstatus)
	obs_date0 = cdate0
	call dt2mjd (obs_date0,quiet,mjd_obs0,ftstatus)
	obs_date1 = cdate1
	call dt2mjd (obs_date1,quiet,mjd_obs1,ftstatus)
c
c  this section moved here 11-21-00

c adjust xenon in propane layer (this is not the obvious place, but we only come
c  here once, and we have the number of days calculated)

	xe_gmcm2_pr = xe_gmcm2_pr + del_xepr*(mjd_obs - mjd_obs0)
c	write (*,*) "RMFTESTOUT",xe_gmcm2_pr,del_xepr,mjd_obs,mjd_obs0
c 08-07-07	NS 	
c 08-15-07   putting the leak to propane term back
c	xe_gmcm2_l1 = xe_gmcm2_l1 - del_xepr*(mjd_obs - mjd_obs0)
c 02-11-02  added possibility for second relationship between xe_gmcm2_pr and time
	if (mjd_obs .gt. mjd_obs1) then 
c    	   xe_gmcm2_l1 = xe_gmcm2_l1 -  del_xepr*(mjd_obs1 - mjd_obs0)
c    	   xe_gmcm2_l1 = xe_gmcm2_l1 -  del_xepr_p*(mjd_obs - mjd_obs1)
	   xe_gmcm2_pr = xe_gmcm2_pr_p + del_xepr_p*(mjd_obs - mjd_obs1)
	   pr_gmcm2 = 0.
	end if


c 08-17-00  hard code values for PCU0 propane layer after loss of propane volume
c	if (pcuid .eq. 0 .and. mjd_obs .gt. 51677.0) then
c		xe_gmcm2_pr = 0.
c		pr_gmcm2 = 0.
c	end if
c 04-15-08 hard code values for PCU1 propane layer after loss of propane volume
c	if (pcuid .eq. 1 .and. mjd_obs .gt. 54094.0) then
c		xe_gmcm2_pr = 0.
c		pr_gmcm2 = 0.
c	end if
c 02-06-01 graceful exit if a matrix for propane layer, PCU 0, after failure is requested
        if (pcuid.eq.0 .and. mjd_obs.gt.51677.0 .and. lld.eq.64) then
          msg = 'Requested date falls after PCU0 propane loss...exiting'
          call fcerr(msg)
          ierr = 1
          go to 999
        endif

        if (status .ne. 0) then
           msg = 'ERROR reading parameter file'
           call fcerr(msg)
           ierr = 1
           go to 999
        endif
        
C     Open the output file (with graceful error handling - JCL)
         call op_npa(RSPFILE, CHATTER, OUNIT, fstatus)
         if (status .ne. 0) then
            msg = 'op_npa error'
            call fcecho(msg)
            ierr = 1
            go to 998
         endif

        end if

C Write parameters to output file
        if (istatus .eq. 1 .or. istatus .eq. 2) then
        fstatus = 0

        call ftphis (ounit,vrsn_no(1),fstatus)
        write (char70, 9001) pcuid
9001    format ('PCA Slot number ',I1,53x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9002) lld
9002    format ('Lower level discriminator code ',I2,37x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9003) cdate
9003    format ('date  ',A20,36X)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9004) xe_gmcm2_l1
9004    format ('thickness (gm/cm^2) of layer 1',1Pe10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9005) xe_gmcm2_l2
9005    format ('thickness (gm/cm^2) of layer 2',1Pe10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9006) xe_gmcm2_l3
9006    format ('thickness (gm/cm^2) of layer 3',1Pe10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9007) xe_gmcm2_pr 
9007    format ('xe thickness (gm/cm^2) of layer pr ',1Pe10.3,25x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9049) xe_gmcm2_dl 
9049    format ('xe thickness (gm/cm^2) of boundary layers',1Pe10.3,18x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9009) pr_gmcm2
9009    format ('Propane layer thickness (gm/cm^2) ',1pe10.3,26x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9010) my_gmcm2
9010    format ('Mylar window thickness (gm/cm^2)  ',1pe10.3,26x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9011) al_gmcm2
9011    format ('Aluminum window thickness (gm/cm^2) ',1pe10.3,24x)
        call ftphis (ounit,char70,fstatus)

        write (char70, 9008) kEdge_veto
9008    format ('1.-self veto above K edge ',F6.3,38x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9045) lEdge_veto
9045    format ('1.-self veto above L edge ',F6.3,38x)
        call ftphis (ounit,char70,fstatus)

        write (char70, 9012) xeKedge
9012    format ('Xenon K-edge (keV)  ',f10.3,40x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9013) xeL3edge
9013    format ('Xenon L3-edge (keV) ',f10.3,40x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9014) xeL2edge
9014    format ('Xenon L2-edge (keV) ',f10.3,40x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9015) xeL1edge
9015    format ('Xenon L1-edge (keV) ',f10.3,40x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9016) EscFracKa
9016    format ('Xenon K-alpha escape fraction ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9017) EscFracKb
9017    format ('Xenon K-beta  escape fraction ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9018) EscFracL(1)
9018    format ('Xenon L escape frac, layer 1  ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9019) EscFracL(2)
9019    format ('Xenon L escape frac, layer 2  ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9020) EscFracL(3)
9020    format ('Xenon L escape frac, layer 3  ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
c	NS adding escape lines and norms
c        write (char70, 9116) EscNormKa
c9116    format ('Xenon K-alpha escape line normalization ',f10.3,30x)
c        call ftphis (ounit,char70,fstatus)
        write (char70, 9117) EscNormKb
9117    format ('Xenon K-beta  escape line normalization ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9116) EscNormLa(1)
9116    format ('Xenon L-alpha  escape line norm ,layer 1',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9118) EscNormLa(2)
9118    format ('Xenon L-alpha  escape line norm , layer 2',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9119) EscNormLa(3)
9119    format ('Xenon L-alpha  escape line norm, layer 3',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
c	NS
        write (char70, 9021) EscEnerKa
9021    format ('Xenon K-alpha escape energy   ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9022) EscEnerKb
9022    format ('Xenon K-beta  escape energy   ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9023) EscEnerLa
9023    format ('Xenon L-alpha escape energy   ',f10.3,30x)
        call ftphis (ounit,char70,fstatus)
c	write (*,*) 'rmffpar 1'
        write (char70, 9024) DeltaE_L3
9024    format ('Energy step at Xe L3 edge (keV)    ',f10.3,25x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9025) DeltaE_L2
9025    format ('Energy step at Xe L2 edge (keV)    ',f10.3,25x)
        call ftphis (ounit,char70,fstatus)
        write (char70, 9026) DeltaE_L1
9026    format ('Energy step at Xe L1 edge (keV)    ',f10.3,25x)
        call ftphis (ounit,char70,fstatus)
        write (char70 ,9027) DeltaE_K
9027    format ('Energy step at Xe K  edge (keV)    ',f10.3,25x)
        call ftphis (ounit,char70,fstatus)
c	write (*,*) 'rmffpar 2'
        write (char70, 9028) lbl
9028    format ('LBL cross section flag        ',f10.3,30x)
        call ftphis (ounit,char70,fstatus) 
        write (char70, 9029) delta_el_L
9029    format ('electron offset for L escape peak  ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
        write (char70, 9030) delta_el_Ka
9030    format ('electron offset for Ka escape peak ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
        write (char70 ,9031) delta_el_Kb
9031    format ('electron offset for Kb escape peak ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
        write (char70, 9032) area_factor
9032    format ('Global area fudge factor       ',f10.3,30x)
        call ftphis (ounit,char70,fstatus) 
        write (char70, 9033) epoint
9033    format ('reference energy for elec. tracks  ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
        write (char70, 9034) d5
9034    format ('coefficient for electron tracks    ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
        write (char70, 9044) d6
9044    format ('exponent    for electron tracks    ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
C        call ftphis (ounit,'For a good answer, call 
C     &          301-286-7063 or xtehelp@athena',fstatus)
	write (char70, 9037) sigf
	write (*, 9037) sigf
9037	format ('resolution scale factor            ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
	write (char70, 9047) resp1
	write (*, 9047) resp1
9047	format ('fano resolution coeff              ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
	write (char70, 9048) resp2
	write (*, 9048) resp2
9048	format ('electronic resolution coeff        ',f10.3,25x)
        call ftphis (ounit,char70,fstatus) 
	write (char70, 9038) pcc
9038	format ('partial charge coefficient         ',f10.4,25x)
	call ftphis (ounit,char70,fstatus)
	write (char70, 9046) wxef
9046	format ('scale facter in w_xe               ',f10.4,25x)
	call ftphis (ounit,char70,fstatus)
	write (char70, 9040) cdate0
9040	format ('reference date for gain changes    ',A20,15x)
	call ftphis (ounit,char70,fstatus)
	write (char70, 9043) del_xepr
9043	format ('rate of xenon increase in propane  ',1pE12.3,23x)
	call ftphis (ounit,char70,fstatus)
	write (char70, 9050) escale
9050	format ('channels per keV adjustment        ',1pE12.3,23x)
	call ftphis (ounit,char70,fstatus)
	write (char70, 9051) cdate1
9051	format ('2nd reference date for xe_pr law   ',A20,15x)
	call ftphis (ounit,char70,fstatus)
        write (char70, 9052) xe_gmcm2_pr_p 
9052    format ('2nd xe thickness (gm/cm^2) of layer pr ',1Pe10.3,21x)
        call ftphis (ounit,char70,fstatus)
	write (char70, 9053) del_xepr_p
9053	format ('2nd rate of xenon increase in propane  ',1pE12.3,19x)
	call ftphis (ounit,char70,fstatus)
c       05-18-07 NS code ***************************************
c	write (char70, 9054) escape_width
c9054	format ('energy window in keV for non-vetoed K-escape events ',1pE12.3,23x)
c	call ftphis (ounit,char70,fstatus)
c ****************************************************************
	

c document calibration file and parameters used for
c     energy to channel relationship
20      continue
        histxt = 'Calibration file used: '
        call ftphis(ounit,histxt,fstatus)
        call ftphis(ounit,filenm(1),fstatus)
        if (fstatus .ne. 0) then
           msg = 'cannot write filenm(1) value as HISTORY'
           call fcecho(msg)
           go to 999
        endif
        call fti2c(extno(1),extchar,fstatus)
        histxt = 'Extension number: '//extchar(19:20)
        call ftphis(ounit,histxt,fstatus)
        call ftphis(ounit,cdes,fstatus)

        write (char70, 9035) model
9035    format ('energy-to-channel model     ',I2,40x)
        call ftphis (ounit,char70,fstatus) 

        npar_l = npar
        do i = 1,npar
          write (char70, 9036) i,par(i)
9036      format ('energy to channel parameter ',I2,' = ',1pe10.3,27x)
          call ftphis (ounit,char70,fstatus) 

        end do


        end if

        if (istatus .ne. 1 .and. istatus .ne. 2) then
           msg = 'istatus ne. 1 or 2 in rmffpar'
           call fcecho(msg)
        endif
            
998     if (fstatus .ne. 0) then
           call fcerrm(fstatus)
        endif
999     continue
        if (status .ne. 0) then
           call fcerrm(status)
        endif
        
        return
        end




        function sigmaf (e0)
c
c 5-29-95 returns the value of sigma, in keV, for input
c         energy e0.
c         pcuid ==> no pcu dependence as of 5-29-95
c         lld   ==> 2nd and 3rd layers assumed identical
c                   average of all layers assumed equal to
c                   values for first layer
C 2-23-96 ch2keV replaced by 1/par(2)
c 3-08-96 returns sigma in channels
c 4-07-96 added case for lld=60
C
C 10-26-96 copied to version 2.0;  width should be readdressed sometime
C           looks like width returned in channels for epoch 1.
c 11-17-96 answers previously were in channels of epoch 1;  new answer
c           is approximately channels of current epoch (scaled to fits to p0)
C version 2.1.2   sigma artificially reduced to 0.75 exected value to
C         "compensate" for sigma apparently being too big.  Physical widths
C         therefore uncertain.  Equivalent widths probably OK.
C 06-27-97 v2.2  sigf now a parameter that can modify (multiplicative) sigma
c 2000-02-29 v7.10 gives option of sigma = sqrt (ae^2 + b) 
c   read in through respN_M if resolution_factor < 0.50
c 2001-11-15 v8.0 actually does this right.  Old calculation left in for 
c   a reminder.  sigf now a non-used parameter and the better calculation
C   is forced.
c

c calling sequence declarations
        real*4      e0, sigmaf

c global parameters 

c   6-27-97, these apparently not used
c        integer*4 pcuid, lld, date , model, scale_hack
c        character(80)   scale_file
c        common    /schain/   pcuid, lld, date , model, scale_hack,
c     &          scale_file

	real*4	par(10)
	integer*4  npar, noCalDB
	common    /c2epar/	par, npar, noCalDB

	real*4	sigf, siga, sigb
	common    /resolution/  sigf, siga, sigb

c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

        vrsn_no(10) = 'sigmaf       v8.0,  04-15-02'

C  in channels, from p0 epoch 3, nov 17,1996
C	  sigmaf = 1.06 + 0.051*e0 + 2.4e-04*e0*e0
C
c  divide by slope for p0, epoch 3 to get keV
C	  sigmaf = sigmaf/2.66
C
c  in channels for whatever detector
C	  sigmaf = sigmaf * par(2)
C
Cc 3-28-97 test
CCCC	  sigmaf = sigmaf * sigf
C
C	  if (sigf .gt. 0.5) return
C
c 2-25-98  use sigma = sqrt (a E^2 + b)
c          coefficients come in as keV;  par(2) converts to channels
c	  sigmaf = sqrt ( siga*e0*e0 + sigb ) * par(2)
C
c  what was that about?  11-15-2001
c   siga = 0.121, sigb = 0.442 gives sigmaf(6 keV) ~ 1 keV, DeltaE/E ~ 0.08 at 22 keV
c	sigmaf = (sqrt (siga*e0 + sigb)) * par(2) / 2.35
c   NS 02-18-08 - test how the addition of quaratic term helps, use the unused sigf
c   variable for this

	sigmaf = (sqrt (sigf*e0*e0 + siga*e0 + sigb)) * par(2) / 2.35


c	write (*,*) 'sigmaf ',e0,sigmaf

        return
        end

 
        function energy2energyprime (e, imodel)
c
c  10-25-96.  The conversion in this direction is unambiguous, and relies
c     on data from dos Santos et al.
c
        real*4  e, energy2energyprime, wxe, w_xe
	integer*4	imodel
 
        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
        real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     &                        EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &                        delta_el_L, delta_el_Ka, delta_el_Kb, wxef

           wxe = w_xe(e, imodel)
           energy2energyprime = e*22./wxe
 
        return
        end

      function w_xe(ekev, imodel)
c
c  returns w (eV/pair) in Xenon for photon
c  energy ekev.
c   Data from dos Santos et al. NIM A307, 347 (1991)
c             Dias et al. Phys. Rev. A, 48, 2887 (1993)
c   data above 10 keV from Dias, personal communication and in prep.
c
c  the output is modified by the factor wxe.  If wxe=1, the values are
c  as in ref above;  if wxe=0, then wxe=22 and therefore ep=e
c
      real   ekev, w_xe, x
      real   e(30), w(30)
      integer    i, imodel

        real*4     xeKedge, xeL3edge, xeL2edge, xeL1edge
        real*4     EscFracKa, EscFracKb, EscFracL(4)
        real*4     EscNormKb, EscNormLa(4)
        real*4     EscEnerKa, EscEnerKb, EscEnerLa
        real*4     DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K
        real*4     delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        common  /xenon_data/  xeKedge, xeL3edge, xeL2edge, xeL1edge,
     &                        EscFracKa, EscFracKb,
     &                        EscFracL, EscNormKb, EscNormLa,
     &                        EscEnerKa, EscEnerKb, EscEnerLa,
     &                        DeltaE_L3, DeltaE_L2, DeltaE_L1, DeltaE_K,
     &                        delta_el_L, delta_el_Ka, delta_el_Kb, wxef

        data  e   /1.5,  2.0,   3.0,   4.0,   4.5,
     &             4.78, 4.785, 5.0,   5.1,   5.105,
     &             5.15, 5.2,   5.4,   5.452, 5.453,
     &             5.48, 6.0,   7.0,   8.0,   9.0,
     &            10.0, 14.2,  22.0,  34.5,  34.5,
     &            35.0, 36.0,  36.5, 100.0, 100.0/

        data  w   /22.72, 22.46, 22.20, 22.05, 22.01,
     &             21.98, 22.36, 22.38, 22.35, 22.42,
     &             22.43, 22.42, 22.39, 22.45, 22.45,
     &             22.46, 22.38, 22.24, 22.14, 22.06,
     &             21.983,21.833,21.75, 21.708,21.817,
     &             21.825,21.825,21.813,21.675,21.675/

        w_xe=0.0

	if (imodel .eq. 2) then
        x = ekev
        if (ekev .lt. xeL3edge) x = x + DeltaE_L3
        if (ekev .lt. xeL2edge) x = x + DeltaE_L2
        if (ekev .lt. xeL1edge) x = x + DeltaE_L1
        if (ekev .gt. xeKedge)  x = x - DeltaE_K

        w_xe = 22.0*ekev/x

        return
	end if

	if (imodel .eq. 3) then
      do i=1,30
          if (e(i) .gt. ekev) go to 10
      end do

  10      if (i .le. 1) then
            w_xe=22.72
          go to 20
      end if

      if (i .ge. 31) then
          w_xe=21.675
          go to 20
      end if

      w_xe = w(i-1) + (ekev-e(i-1))/(e(i)-e(i-1)) * (w(i)-w(i-1))
  20  w_xe = (w_xe - 22.)*wxef + 22.
      return
	end if

      end

C  version (for pcarmf) is documented in PROPANE subroutine (the one subroutine that
c    will always be called!
C
C**********************************************************************	
	SUBROUTINE	XENON ( EKEV, SIGMA)
c
c 6-8-95.  returns sigma=-9999 for E < 1.012 keV
c                       = 0.       E > 100.  keV
C
C*************************************************************************
	REAL*4		EDGE(20), EMIN, EMAX
	REAL*4		A(60), SIGMA, E, EKEV
	INTEGER*4	N(20), NEDGE, I, J, INDEX
C  1012 < E < 4782.2 eV;  quadratic fit to 14 points from Henke, weighted
C   by 1/deltaE where deltaE = (ALOG(E(I+1)-ALOG(E(I-1))
	DATA	(A(J),J=1,3)	/12.9, 0.94, -0.214/
C  E < 5103.7 eV; Slope from 6 Henke pts. with E > 5453 eV
C    offset to match 1 Henke pt.
	DATA	(A(J),J=4,6)	/28.85, -2.623, 0./
C  E < 5452.8 eV; Slope as above, offset to match 1 Henke pt.
	DATA	(A(J),J=7,9)	/29.16, -2.623, 0./
C  5453 < E < 34560 eV;  linear fit, 6 points from Henke, 6 pts from
C    Veigele (starting at 10 keV), 1/deltaE weight
	DATA	(A(J),J=10,12)	/30.25, -2.728, 0./
C  34560 < E < 100000 eV; linear fit, 6 points from Veigele
	DATA	(A(J),J=13,15)	/32.84, -2.803, 0./
	DATA	N	/3, 2, 2, 2, 2, 15*0/
	DATA	EMIN,EMAX	/1012., 100000./
	DATA	EDGE	/1012., 4782.2, 5103.7, 5452.8,
     +			34560., 15*1.E12/
	DATA	NEDGE	/5/

	E = EKEV*1.E3			! to eV
	IF ( (E.LT.EMIN) .OR. (E.GT.EMAX) ) THEN
	    SIGMA=-9999.
	    IF (E.GT.EMAX) sigma = 0.
	    RETURN 
	    END IF
	INDEX=0
	DO 100 I = 1,NEDGE
 100	    IF (E.GT.EDGE(I)) INDEX = INDEX + 1
	CALL POLSUM (E, A(3*(INDEX-1)+1), N(INDEX), SIGMA)
	RETURN 
	END
C***************************************************************************
	SUBROUTINE	PROPANE ( E, MUPROP)
C
C***************************************************************************
	REAL*4		MUPROP, MUC, MUH
	REAL*4		E
C  Propane is 3 Carbon and 8 Hydrogen atoms
C  The atomic weights are from Henke
C  E in keV
c  documentation of the program 
        character(70) vrsn_no(30)
        common    /version/    vrsn_no 

        vrsn_no(13) = 'xsection      v0.0,  6-25-95'
 
	CALL CARBON ( E, MUC)
  10	CALL HYDROGEN ( E, MUH)
	MUPROP = (36.03*MUC + 8.064*MUH) / 44.094
 101	if (muprop .lt. 0) MUPROP = -9999.
	RETURN 
	END
C
C***************************************************************************
	SUBROUTINE	MYLAR ( E, MUMYL)
C
C***************************************************************************
	REAL*4		MUMYL, MUO, MUC, MUH
	REAL*4		E
C  Mylar is 10 Carbon, 4 Oxygen, and 8 Hydrogen atoms
C  The atomic weights are from Henke
C  E in keV
	CALL OXYGEN ( E, MUO)
  10	CALL CARBON ( E, MUC)
  11	CALL HYDROGEN ( E, MUH)
	MUMYL = (120.1*MUC + 64.0*MUO + 8.064*MUH) / 192.164
 101	if (mumyl .lt. 0) MUMYL = -9999.
	RETURN 
	END
C
C***************************************************************************
	SUBROUTINE	CH4 ( E, MUCH4)
C
C***************************************************************************
	REAL*4		MUCH4, MUC, MUH
	REAL*4		E
C  Methane is 1 Carbon and 4 Hydrogen atoms
C  The atomic weights are from Henke
C  E in keV
	CALL CARBON ( E, MUC)
  10	CALL HYDROGEN ( E, MUH)
	MUCH4 = (12.01*MUC + 4.032*MUH) / 16.042
 101	if (much4 .lt. 0) MUCH4 = -9999.
	RETURN 
	END
C
C*************************************************************************
C HYDROGEN
C
C   30.5,   49.3,   72.4,   91.5,  108.5,	! Henke energies (50)
C  114.0,  132.8,  148.7,  151.7,  171.7,
C  183.3,  192.6,  212.2,  277.0,  311.7,
C  392.4,  395.3,  452.2,  511.3,  524.9,
C  556.3,  572.8,  637.4,  676.8,  705.0,
C  776.2,  851.5,  929.7, 1011.7, 1041.0,
C  1188.0, 1253.6, 1486.7, 1740.0, 2042.4,
C  2165.9, 2293.2, 2622.4, 2984.3, 3691.7,
C  4466.3, 4510.8, 4952.2, 5414.7, 5898.8,
C  6930.3, 7478.2, 8047.8, 8638.9, 9886.4
C
C  4.04E5, 9.68E4, 3.10E4, 1.52E4, 8.95E3,
C  7.68E3, 4.78E3, 3.35E3, 3.19E3, 2.13E3,
C  1.73E3, 1.47E3, 1.08E3, 4.62E2, 3.16E2,
C  1.49E2, 1.45E2, 9.35E1, 6.25E1, 5.73E1,
C  4.74E1, 4.31E1, 3.03E1, 2.48E1, 2.17E1,
C  1.57E1, 1.16E1, 8.64E0, 6.52E0, 5.93E0,
C  3.82E0, 3.20E0, 1.80E0, 1.06E0, 6.19E-1,
C  5.08E-1,4.18E-1,2.65E-1,1.70E-1,8.20E-2,
C  4.27E-2,4.13E-2,3.00E-2,2.22E-2,1.66E-2,
C  9.58E-3,7.40E-3,5.76E-3,4.53E-3,2.86E-3,
C
C  10000., 15000., 20000., 30000., 40000.,	! Veigele energies
C  50000., 60000., 80000.,100000.,     0.,	
C      0.,     0.,     0.,     0.,     0.,
C
C  0.00E1, 0.00E1, 0.00E1, 0.00E1, 0.00E1,	! cross-sections 
C  0.00E1, 0.00E1, 0.00E1, 0.00E1, 0.00E1,	! in barns/atom
C  0.00E1, 0.00E1, 0.00E1, 0.00E1, 0.00E1,
C
C  5.975E-1,					! conversion to cm**2/gm
C
C*************************************************************************
	SUBROUTINE	HYDROGEN ( EKEV, MUH)
C
C*************************************************************************
	REAL*4		E, MUH, EKEV
	REAL*4		EDGE(5), EMIN, EMAX
	REAL*4		A(15)
	INTEGER*4	N(5), INDEX, I, J
c
c 8-07-97  muh = 0.    E > 20.0  keV
c   fit not changed, but upper limit extended to allow extrapolation
c               -9999  E < 0.030 keV
C
C  30. < E < 10000. eV;  quadratic fit to 50 points from Henke, weighted
C      by 1/deltaE where deltaE = (ALOG(E(I+1)-ALOG(E(I-1))
	DATA	(A(J),J=1,3)	/23.002, -2.7824, -3.899E-2/
C E in keV
	DATA	N	/3, 4*0/
	DATA	EMIN,EMAX	/30.0, 20000.5/
	DATA	EDGE	/30.0, 4*1.E12/
c	DATA	NEDGE	/1/
	E = EKEV*1.E3
	IF ( (E.LT.EMIN) .OR. (E.GT.EMAX) ) THEN
	    MUH=-9999.
	    IF (E.GT.EMAX) MUH = 0.
	    RETURN 
	    END IF
	INDEX=0
	DO 100 I = 1,5
 100	    IF (E.GT.EDGE(I)) INDEX = INDEX + 1
	CALL POLSUM (E, A(3*(INDEX-1)+1), N(INDEX), MUH)
	RETURN 
	END
c
C*******************************************************************	
	SUBROUTINE	CARBON ( EKEV, SIGMA)
C
C*************************************************************************
	REAL*4		EDGE(5), EMIN, EMAX
	REAL*4		A(15), EKEV, SIGMA, E
	INTEGER*4	N(5), NEDGE, INDEX, I, J
c
c 8-07-97  sigma = 0.    E > 20keV
c          sigma =-9999  E < 0.070 keV
c
C  70 < E < 283.8 eV;  quadratic fit to 12 from Henke, weighted
C   by 1/deltaE where deltaE = (ALOG(E(I+1)-ALOG(E(I-1))
	DATA	(A(J),J=1,3)	/14.703, -2.72E-3, -0.2185/
C  283.8 < E < 10000 eV;  quadratic fit to 31 points from Henke
	DATA	(A(J),J=4,6)	/21.062, -1.111, -0.1193/
	DATA	N	/3, 3, 3*0/
	DATA	EMIN,EMAX	/70., 20000./
	DATA	EDGE	/70., 283.8, 3*1.E12/
	DATA	NEDGE	/2/
	E = EKEV*1.E3
	IF ( (E.LT.EMIN) .OR. (E.GT.EMAX) ) THEN
	    SIGMA=-9999.
	    IF (E.GT.EMAX) SIGMA=0.
	    RETURN 
	    END IF
	INDEX=0
	DO 100 I = 1,NEDGE
 100	    IF (E.GT.EDGE(I)) INDEX = INDEX + 1
	CALL POLSUM (E, A(3*(INDEX-1)+1), N(INDEX), SIGMA)
	RETURN 
	END
C
C*******************************************************************
	SUBROUTINE	OXYGEN ( EKEV, SIGMA)
C
C*************************************************************************
	REAL*4		EDGE(5), EMIN, EMAX
	REAL*4		A(15), EKEV, SIGMA, E
	INTEGER*4	N(5), NEDGE, INDEX, I, J
c
c  8-07-97    sigma = 0    E > 20.0   keV
c                   = -9999  <  0.108 keV
c
C  108.5 < E < 531.7 eV;  quadratic fit to 16 points from Henke, weighted
C      by 1/deltaE where deltaE = (ALOG(E(I+1)-ALOG(E(I-1))
	DATA	(A(J),J=1,3)	/18.51, -1.0226, -0.1279/
C  531.7 < E < 10000 eV;  quadratic fit to 30 from Henke, weighted
C      by 1/deltaE where deltaE = (ALOG(E(I+1)-ALOG(E(I-1))
	DATA	(A(J),J=4,6)	/20.108, -0.7673, -0.13365/
	DATA	N	/3, 3, 3*0/
	DATA	EMIN,EMAX	/100.0, 20000.5/
	DATA	EDGE	/100.0, 531.7, 3*1.E12/
	DATA	NEDGE	/2/
	E = EKEV*1.E3
	IF ( (E.LT.EMIN) .OR. (E.GT.EMAX) ) THEN
	    SIGMA=-9999.
	    IF (E.GT.EMAX) sigma = 0.
	    RETURN 
	    END IF
	INDEX=0
	DO 100 I = 1,NEDGE
 100	    IF (E.GT.EDGE(I)) INDEX = INDEX + 1
	CALL POLSUM (E, A(3*(INDEX-1)+1), N(INDEX), SIGMA)
	RETURN 
	END
C*************************************************************************
C ALUMINUM
C
C   30.5,   49.3,   72.4,   91.5,  108.5,	! Henke energies (50)
C  114.0,  132.8,  148.7,  151.7,  171.7,
C  183.3,  192.6,  212.2,  277.0,  311.7,
C  392.4,  395.3,  452.2,  511.3,  524.9,
C  556.3,  572.8,  637.4,  676.8,  705.0,
C  776.2,  851.5,  929.7, 1011.7, 1041.0,
C  1188.0, 1253.6, 1486.7, 1740.0, 2042.4,
C  2165.9, 2293.2, 2622.4, 2984.3, 3691.7,
C  4466.3, 4510.8, 4952.2, 5414.7, 5898.8,
C  6930.3, 7478.2, 8047.8, 8638.9, 9886.4
C
C  1.22E4, 9.64E3, 1.13E4, 1.17E3, 1.07E5,
C  1.14E5, 9.58E4, 8.51E4, 8.49E4, 7.43E4,
C  6.40E4, 6.02E4, 5.26E4, 3.10E4, 2.30E4,
C  1.38E4, 1.36E4, 9.75E3, 7.18E3, 6.72E3,
C  5.80E3, 5.37E3, 4.04E3, 3.41E3, 3.08E3,
C  2.38E3, 1.85E3, 1.45E3, 1.15E3, 1.07E3,
C  7.42E2, 6.39E2, 4.03E2, 3.22E3, 2.16E3,
C  1.86E3, 1.60E3, 1.12E3, 7.92E2, 4.40E2,
C  2.57E2, 2.50E2, 1.91E2, 1.48E2, 1.16E2,
C  7.25E1, 5.81E1, 4.68E1, 3.80E1, 2.55E1,
C
C  10000., 15000., 20000., 30000., 40000.,	! Veigele energies
C  50000., 60000., 80000.,100000.,     0.,	
C      0.,     0.,     0.,     0.,     0.,
C
C  0.00E1, 0.00E1, 0.00E1, 0.00E1, 0.00E1,	! cross-sections 
C  0.00E1, 0.00E1, 0.00E1, 0.00E1, 0.00E1,	! in barns/atom
C  0.00E1, 0.00E1, 0.00E1, 0.00E1, 0.00E1,
C
C  2.232E-2,					! conversion to cm**2/gm
C
C*************************************************************************
	SUBROUTINE	ALUMINUM (EKEV, MUH)
C
C*************************************************************************
	REAL*4		E, MUH, EKEV
	REAL*4		EDGE(5), EMIN, EMAX
	REAL*4		A(15)
	INTEGER*4	N(5), INDEX, I, J
c
c   returns muh = 0.         E <   0.100 keV
c               =-9999.  for E >  10.0   keV
c
C
C  100. < E < 1599.9 eV;  quadratic fit to 23 points from Henke, weighted
C      by 1/deltaE where deltaE = (ALOG(E(I+1)-ALOG(E(I-1)) (start at 114 eV)
	DATA	(A(J),J=1,3)	/10.837, 1.819, -0.3418/
C  1599.9 < E < 10000. eV;  quadratic fit to 17 points from Henke, weighted
C      by 1/deltaE where deltaE = (ALOG(E(I+1)-ALOG(E(I-1))
c extended upper range to 20 keV 11-23-98
	DATA	(A(J),J=4,6)	/29.035, -2.799, 0./
	DATA	N	/3, 2, 3*0/
	DATA	EMIN,EMAX	/100.0, 30000.5/
	DATA	EDGE	/100.0, 1599.9, 3*1.E12/
c	DATA	NEDGE	/2/
	E = EKEV*1.E3
	IF ( (E.LT.EMIN) .OR. (E.GT.EMAX) ) THEN
	    MUH=-9999.
	    IF (E.GT.EMAX) MUH=0.
            RETURN 
	    END IF
	INDEX=0
	DO 100 I = 1,5
 100	    IF (E.GT.EDGE(I)) INDEX = INDEX + 1
	CALL POLSUM (E, A(3*(INDEX-1)+1), N(INDEX), MUH)
	RETURN 
	END
C*************************************************************************
	SUBROUTINE POLSUM (E, A, N1, SIGMA)
C
C*************************************************************************
	REAL*4		A(1), E, SIGMA
	REAL*4		LOGE
	INTEGER*4	N1, I
	SIGMA = 0.
	LOGE = ALOG(E)
	DO 10 I = 1,N1
  10	    SIGMA = SIGMA + A(I)*LOGE**(FLOAT(I-1))
	SIGMA = EXP (SIGMA)
	RETURN
	END


	subroutine xenonlbl (energy, xsec)

	real*4		energy, xsec
	real*4		r0
	real*4		kevtoang, lambda
	real*4		ee(509), f2(509), f1(509)
	real*4		mxe, ff2
	integer*4	i, j

	data	r0	/2.817938e-5/  !  classical electron radius in angstrom
	data	kevtoang /12.398/     ! divide energy into this to get angstrom
	data	mxe	/2.18016e-22/ ! mass of xenon atom in gm*

	data ee(1),f1(1),f2(1)  /0.01 , -9999. , 0.312014E-14 /
	data ee(2),f1(2),f2(2)  /0.0101617 , -9999. , 0.317061E-14 /
	data ee(3),f1(3),f2(3)  /0.0103261 , -9999. , 0.322189E-14 /
	data ee(4),f1(4),f2(4)  /0.0104931 , -9999. , 0.327400E-14 /
	data ee(5),f1(5),f2(5)  /0.0106628 , -9999. , 0.332696E-14 /
	data ee(6),f1(6),f2(6)  /0.0108353 , -9999. , 0.338077E-14 /
	data ee(7),f1(7),f2(7)  /0.0110106 , -9999. , 0.343545E-14 /
	data ee(8),f1(8),f2(8)  /0.0111886 , -9999. , 0.349102E-14 /
	data ee(9),f1(9),f2(9)  /0.0113696 , -9999. , 0.354748E-14 /
	data ee(10),f1(10),f2(10)  /0.0115535 , -9999. , 0.360486E-14 /
	data ee(11),f1(11),f2(11)  /0.0117404 , -9999. , 0.366316E-14 /
	data ee(12),f1(12),f2(12)  /0.0119303 , -9999. , 0.372241E-14 /
	data ee(13),f1(13),f2(13)  /0.012 , -9999. , 0.374417E-14 /
	data ee(14),f1(14),f2(14)  /0.0122 , -9999. , 0.307739E-04 /
	data ee(15),f1(15),f2(15)  /0.0123193 , -9999. , 1.78551 /
	data ee(16),f1(16),f2(16)  /0.0125186 , -9999. , 1.86163 /
	data ee(17),f1(17),f2(17)  /0.012721 , -9999. , 7.89998 /
	data ee(18),f1(18),f2(18)  /0.0129268 , -9999. , 9.28777 /
	data ee(19),f1(19),f2(19)  /0.0131359 , -9999. , 10.1806 /
	data ee(20),f1(20),f2(20)  /0.0133483 , -9999. , 10.8450 /
	data ee(21),f1(21),f2(21)  /0.0135642 , -9999. , 11.4783 /
	data ee(22),f1(22),f2(22)  /0.0137836 , -9999. , 11.9987 /
	data ee(23),f1(23),f2(23)  /0.0140066 , -9999. , 12.5503 /
	data ee(24),f1(24),f2(24)  /0.0142331 , -9999. , 13.1464 /
	data ee(25),f1(25),f2(25)  /0.0144633 , -9999. , 13.3356 /
	data ee(26),f1(26),f2(26)  /0.0146973 , -9999. , 13.4705 /
	data ee(27),f1(27),f2(27)  /0.014935 , -9999. , 13.6068 /
	data ee(28),f1(28),f2(28)  /0.0151765 , -9999. , 13.7445 /
	data ee(29),f1(29),f2(29)  /0.015422 , -9999. , 13.7867 /
	data ee(30),f1(30),f2(30)  /0.0156714 , -9999. , 13.6926 /
	data ee(31),f1(31),f2(31)  /0.0159249 , -9999. , 13.5991 /
	data ee(32),f1(32),f2(32)  /0.0161825 , -9999. , 13.5063 /
	data ee(33),f1(33),f2(33)  /0.0164442 , -9999. , 13.4141 /
	data ee(34),f1(34),f2(34)  /0.0167102 , -9999. , 13.2302 /
	data ee(35),f1(35),f2(35)  /0.0169805 , -9999. , 12.9384 /
	data ee(36),f1(36),f2(36)  /0.0172551 , -9999. , 12.6530 /
	data ee(37),f1(37),f2(37)  /0.0175342 , -9999. , 12.3738 /
	data ee(38),f1(38),f2(38)  /0.0178178 , -9999. , 12.1009 /
	data ee(39),f1(39),f2(39)  /0.018106 , -9999. , 11.8340 /
	data ee(40),f1(40),f2(40)  /0.0183989 , -9999. , 11.5597 /
	data ee(41),f1(41),f2(41)  /0.0186964 , -9999. , 11.2620 /
	data ee(42),f1(42),f2(42)  /0.0189988 , -9999. , 10.9720 /
	data ee(43),f1(43),f2(43)  /0.0193061 , -9999. , 10.6895 /
	data ee(44),f1(44),f2(44)  /0.0196184 , -9999. , 10.4142 /
	data ee(45),f1(45),f2(45)  /0.0199357 , -9999. , 10.1460 /
	data ee(46),f1(46),f2(46)  /0.0202582 , -9999. , 9.88472 /
	data ee(47),f1(47),f2(47)  /0.0205858 , -9999. , 9.63017 /
	data ee(48),f1(48),f2(48)  /0.0209188 , -9999. , 9.38218 /
	data ee(49),f1(49),f2(49)  /0.0212571 , -9999. , 9.14057 /
	data ee(50),f1(50),f2(50)  /0.0216009 , -9999. , 8.90518 /
	data ee(51),f1(51),f2(51)  /0.0219503 , -9999. , 8.65326 /
	data ee(52),f1(52),f2(52)  /0.0223053 , -9999. , 8.24073 /
	data ee(53),f1(53),f2(53)  /0.0226661 , -9999. , 7.84788 /
	data ee(54),f1(54),f2(54)  /0.0230327 , -9999. , 7.47375 /
	data ee(55),f1(55),f2(55)  /0.0234053 , -9999. , 7.11745 /
	data ee(56),f1(56),f2(56)  /0.0237838 , -9999. , 6.77814 /
	data ee(57),f1(57),f2(57)  /0.0241685 , -9999. , 6.45500 /
	data ee(58),f1(58),f2(58)  /0.0245594 , -9999. , 6.14727 /
	data ee(59),f1(59),f2(59)  /0.0249566 , -9999. , 5.85421 /
	data ee(60),f1(60),f2(60)  /0.0253603 , -9999. , 5.57512 /
	data ee(61),f1(61),f2(61)  /0.0257705 , -9999. , 5.30934 /
	data ee(62),f1(62),f2(62)  /0.0261873 , -9999. , 5.05623 /
	data ee(63),f1(63),f2(63)  /0.0266109 , -9999. , 4.81518 /
	data ee(64),f1(64),f2(64)  /0.0270413 , -9999. , 4.58563 /
	data ee(65),f1(65),f2(65)  /0.0274786 , -9999. , 4.35879 /
	data ee(66),f1(66),f2(66)  /0.0279231 , -9999. , 4.14242 /
	data ee(67),f1(67),f2(67)  /0.0283747 , -9999. , 3.93679 /
	data ee(68),f1(68),f2(68)  /0.0288337 , -9999. , 3.74136 /
	data ee(69),f1(69),f2(69)  /0.0293 , 9.24787 , 3.55564 /
	data ee(70),f1(70),f2(70)  /0.0297739 , 9.15401 , 3.37914 /
	data ee(71),f1(71),f2(71)  /0.0302555 , 9.05665 , 3.21139 /
	data ee(72),f1(72),f2(72)  /0.0307449 , 8.95659 , 3.05198 /
	data ee(73),f1(73),f2(73)  /0.0312421 , 8.85495 , 2.90047 /
	data ee(74),f1(74),f2(74)  /0.0317475 , 8.75252 , 2.75649 /
	data ee(75),f1(75),f2(75)  /0.0322609 , 8.66863 , 2.61117 /
	data ee(76),f1(76),f2(76)  /0.0327827 , 8.56578 , 2.44432 /
	data ee(77),f1(77),f2(77)  /0.033313 , 8.43921 , 2.28813 /
	data ee(78),f1(78),f2(78)  /0.0338518 , 8.30286 , 2.14192 /
	data ee(79),f1(79),f2(79)  /0.0343993 , 8.15845 , 2.00505 /
	data ee(80),f1(80),f2(80)  /0.0349557 , 8.00686 , 1.87693 /
	data ee(81),f1(81),f2(81)  /0.0355211 , 7.84855 , 1.75699 /
	data ee(82),f1(82),f2(82)  /0.0360956 , 7.68358 , 1.64472 /
	data ee(83),f1(83),f2(83)  /0.0366794 , 7.50428 , 1.53963 /
	data ee(84),f1(84),f2(84)  /0.0372727 , 7.32127 , 1.45808 /
	data ee(85),f1(85),f2(85)  /0.0378755 , 7.14335 , 1.38499 /
	data ee(86),f1(86),f2(86)  /0.0384882 , 6.96445 , 1.31556 /
	data ee(87),f1(87),f2(87)  /0.0391107 , 6.77222 , 1.24961 /
	data ee(88),f1(88),f2(88)  /0.0397432 , 6.57704 , 1.20822 /
	data ee(89),f1(89),f2(89)  /0.0403861 , 6.39261 , 1.17650 /
	data ee(90),f1(90),f2(90)  /0.0410393 , 6.21168 , 1.14562 /
	data ee(91),f1(91),f2(91)  /0.0417031 , 6.02982 , 1.11554 /
	data ee(92),f1(92),f2(92)  /0.0423776 , 5.84214 , 1.08702 /
	data ee(93),f1(93),f2(93)  /0.043063 , 5.65382 , 1.06731 /
	data ee(94),f1(94),f2(94)  /0.0437595 , 5.46429 , 1.04795 /
	data ee(95),f1(95),f2(95)  /0.0444673 , 5.27081 , 1.02894 /
	data ee(96),f1(96),f2(96)  /0.0451865 , 5.07147 , 1.01028 /
	data ee(97),f1(97),f2(97)  /0.0459174 , 4.85931 , 0.991962 /
	data ee(98),f1(98),f2(98)  /0.04666 , 4.64081 , 0.985351 /
	data ee(99),f1(99),f2(99)  /0.0474147 , 4.41909 , 0.980027 /
	data ee(100),f1(100),f2(100)  /0.0481816 , 4.18957 , 0.974732 /
	data ee(101),f1(101),f2(101)  /0.0489609 , 3.94228 , 0.970576 /
	data ee(102),f1(102),f2(102)  /0.0497528 , 3.68733 , 0.979680 /
	data ee(103),f1(103),f2(103)  /0.0505576 , 3.42626 , 0.988870 /
	data ee(104),f1(104),f2(104)  /0.0513753 , 3.15172 , 0.998145 /
	data ee(105),f1(105),f2(105)  /0.0522062 , 2.85945 , 1.01157 /
	data ee(106),f1(106),f2(106)  /0.0530506 , 2.55235 , 1.02998 /
	data ee(107),f1(107),f2(107)  /0.0539087 , 2.22780 , 1.04873 /
	data ee(108),f1(108),f2(108)  /0.0547806 , 1.87892 , 1.06781 /
	data ee(109),f1(109),f2(109)  /0.0556667 , 1.49957 , 1.09067 /
	data ee(110),f1(110),f2(110)  /0.056567 , 1.08790 , 1.11783 /
	data ee(111),f1(111),f2(111)  /0.057482 , 0.635603 , 1.14567 /
	data ee(112),f1(112),f2(112)  /0.0584117 , 0.128885 , 1.17420 /
	data ee(113),f1(113),f2(113)  /0.0593564 , -0.469527 , 1.20343 /
	data ee(114),f1(114),f2(114)  /0.0603165 , -1.17074 , 1.27189 /
	data ee(115),f1(115),f2(115)  /0.0612921 , -2.01898 , 1.35847 /
	data ee(116),f1(116),f2(116)  /0.0622834 , -3.32953 , 1.54840 /
	data ee(117),f1(117),f2(117)  /0.0632908 , -4.78492 , 2.60003 /
	data ee(118),f1(118),f2(118)  /0.0643145 , -6.07324 , 4.36594 /
	data ee(119),f1(119),f2(119)  /0.0653547 , -4.99111 , 7.33114 /
	data ee(120),f1(120),f2(120)  /0.0664118 , -3.14174 , 7.31205 /
	data ee(121),f1(121),f2(121)  /0.0674859 , -2.75421 , 6.32607 /
	data ee(122),f1(122),f2(122)  /0.0685775 , -3.33890 , 5.47306 /
	data ee(123),f1(123),f2(123)  /0.0696867 , -4.96733 , 5.26332 /
	data ee(124),f1(124),f2(124)  /0.0708138 , -6.42927 , 6.13015 /
	data ee(125),f1(125),f2(125)  /0.0719591 , -7.44364 , 7.13975 /
	data ee(126),f1(126),f2(126)  /0.073123 , -8.36901 , 8.31560 /
	data ee(127),f1(127),f2(127)  /0.0743057 , -9.20709 , 9.68514 /
	data ee(128),f1(128),f2(128)  /0.0755076 , -9.88767 , 11.2802 /
	data ee(129),f1(129),f2(129)  /0.0767289 , -10.2734 , 12.9916 /
	data ee(130),f1(130),f2(130)  /0.0779699 , -10.5619 , 14.7116 /
	data ee(131),f1(131),f2(131)  /0.079231 , -10.5638 , 16.6594 /
	data ee(132),f1(132),f2(132)  /0.0805125 , -10.3859 , 18.4108 /
	data ee(133),f1(133),f2(133)  /0.0818147 , -10.1979 , 20.2492 /
	data ee(134),f1(134),f2(134)  /0.083138 , -9.86862 , 22.2711 /
	data ee(135),f1(135),f2(135)  /0.0844827 , -8.89494 , 24.3769 /
	data ee(136),f1(136),f2(136)  /0.0858491 , -7.87394 , 25.9997 /
	data ee(137),f1(137),f2(137)  /0.0872377 , -6.97134 , 27.7306 /
	data ee(138),f1(138),f2(138)  /0.0886487 , -5.91492 , 29.5766 /
	data ee(139),f1(139),f2(139)  /0.0900825 , -4.32097 , 31.5456 /
	data ee(140),f1(140),f2(140)  /0.0915395 , -2.54196 , 32.9879 /
	data ee(141),f1(141),f2(141)  /0.0930201 , -0.817933 , 34.3950 /
	data ee(142),f1(142),f2(142)  /0.0945246 , 1.09180 , 35.8621 /
	data ee(143),f1(143),f2(143)  /0.0960535 , 3.61712 , 37.0881 /
	data ee(144),f1(144),f2(144)  /0.0976071 , 6.04274 , 37.6383 /
	data ee(145),f1(145),f2(145)  /0.0991858 , 8.20609 , 38.1965 /
	data ee(146),f1(146),f2(146)  /0.10079 , 10.4756 , 38.7631 /
	data ee(147),f1(147),f2(147)  /0.10242 , 13.1326 , 39.1040 /
	data ee(148),f1(148),f2(148)  /0.104077 , 15.6864 , 38.9448 /
	data ee(149),f1(149),f2(149)  /0.10576 , 18.0665 , 38.7863 /
	data ee(150),f1(150),f2(150)  /0.107471 , 20.7633 , 38.6284 /
	data ee(151),f1(151),f2(151)  /0.109209 , 23.4217 , 37.6772 /
	data ee(152),f1(152),f2(152)  /0.110975 , 25.7865 , 36.6323 /
	data ee(153),f1(153),f2(153)  /0.11277 , 28.6833 , 35.6163 /
	data ee(154),f1(154),f2(154)  /0.114594 , 31.1490 , 33.2117 /
	data ee(155),f1(155),f2(155)  /0.116448 , 32.7183 , 30.9479 /
	data ee(156),f1(156),f2(156)  /0.118331 , 33.9623 , 28.8384 /
	data ee(157),f1(157),f2(157)  /0.120245 , 35.1408 , 26.8726 /
	data ee(158),f1(158),f2(158)  /0.12219 , 36.4739 , 24.7408 /
	data ee(159),f1(159),f2(159)  /0.124166 , 37.1409 , 21.9711 /
	data ee(160),f1(160),f2(160)  /0.126175 , 37.2229 , 19.5113 /
	data ee(161),f1(161),f2(161)  /0.128215 , 37.0163 , 17.3269 /
	data ee(162),f1(162),f2(162)  /0.130289 , 36.6180 , 15.3871 /
	data ee(163),f1(163),f2(163)  /0.132397 , 36.0981 , 13.6645 /
	data ee(164),f1(164),f2(164)  /0.134538 , 35.4897 , 12.1347 /
	data ee(165),f1(165),f2(165)  /0.136714 , 34.8270 , 10.7762 /
	data ee(166),f1(166),f2(166)  /0.138925 , 34.1190 , 9.56974 /
	data ee(167),f1(167),f2(167)  /0.141172 , 33.3983 , 8.51687 /
	data ee(168),f1(168),f2(168)  /0.143456 , 32.6755 , 7.58211 /
	data ee(169),f1(169),f2(169)  /0.145776 , 31.9591 , 6.74997 /
	data ee(170),f1(170),f2(170)  /0.148134 , 31.2477 , 6.00916 /
	data ee(171),f1(171),f2(171)  /0.15053 , 30.5484 , 5.34963 /
	data ee(172),f1(172),f2(172)  /0.152964 , 29.8693 , 4.75853 /
	data ee(173),f1(173),f2(173)  /0.155439 , 29.1829 , 4.20373 /
	data ee(174),f1(174),f2(174)  /0.157953 , 28.4795 , 3.71361 /
	data ee(175),f1(175),f2(175)  /0.160507 , 27.7506 , 3.30996 /
	data ee(176),f1(176),f2(176)  /0.163103 , 27.0554 , 3.00803 /
	data ee(177),f1(177),f2(177)  /0.165742 , 26.3968 , 2.73365 /
	data ee(178),f1(178),f2(178)  /0.168422 , 25.7007 , 2.50814 /
	data ee(179),f1(179),f2(179)  /0.171146 , 25.0692 , 2.42290 /
	data ee(180),f1(180),f2(180)  /0.173915 , 24.5010 , 2.34056 /
	data ee(181),f1(181),f2(181)  /0.176727 , 23.9171 , 2.29085 /
	data ee(182),f1(182),f2(182)  /0.179586 , 23.3883 , 2.32877 /
	data ee(183),f1(183),f2(183)  /0.182491 , 22.9277 , 2.36731 /
	data ee(184),f1(184),f2(184)  /0.185442 , 22.4983 , 2.42192 /
	data ee(185),f1(185),f2(185)  /0.188442 , 22.0995 , 2.48194 /
	data ee(186),f1(186),f2(186)  /0.191489 , 21.7114 , 2.54345 /
	data ee(187),f1(187),f2(187)  /0.194587 , 21.3291 , 2.64107 /
	data ee(188),f1(188),f2(188)  /0.197734 , 20.9806 , 2.76930 /
	data ee(189),f1(189),f2(189)  /0.200932 , 20.6692 , 2.90375 /
	data ee(190),f1(190),f2(190)  /0.204182 , 20.3805 , 3.04473 /
	data ee(191),f1(191),f2(191)  /0.207485 , 20.1117 , 3.19362 /
	data ee(192),f1(192),f2(192)  /0.21084 , 19.8625 , 3.35046 /
	data ee(193),f1(193),f2(193)  /0.214251 , 19.6316 , 3.51500 /
	data ee(194),f1(194),f2(194)  /0.217716 , 19.4184 , 3.68762 /
	data ee(195),f1(195),f2(195)  /0.221237 , 19.2267 , 3.86872 /
	data ee(196),f1(196),f2(196)  /0.224816 , 19.0517 , 4.05053 /
	data ee(197),f1(197),f2(197)  /0.228452 , 18.8896 , 4.24019 /
	data ee(198),f1(198),f2(198)  /0.232147 , 18.7442 , 4.43874 /
	data ee(199),f1(199),f2(199)  /0.235902 , 18.6171 , 4.64658 /
	data ee(200),f1(200),f2(200)  /0.239717 , 18.5106 , 4.86416 /
	data ee(201),f1(201),f2(201)  /0.243595 , 18.4460 , 5.09193 /
	data ee(202),f1(202),f2(202)  /0.247535 , 18.4112 , 5.29354 /
	data ee(203),f1(203),f2(203)  /0.251538 , 18.3738 , 5.47387 /
	data ee(204),f1(204),f2(204)  /0.255607 , 18.3395 , 5.66034 /
	data ee(205),f1(205),f2(205)  /0.259741 , 18.3173 , 5.85316 /
	data ee(206),f1(206),f2(206)  /0.263942 , 18.3114 , 6.05255 /
	data ee(207),f1(207),f2(207)  /0.268211 , 18.3514 , 6.25872 /
	data ee(208),f1(208),f2(208)  /0.272549 , 18.4216 , 6.42272 /
	data ee(209),f1(209),f2(209)  /0.276957 , 18.4756 , 6.54819 /
	data ee(210),f1(210),f2(210)  /0.281437 , 18.5193 , 6.67611 /
	data ee(211),f1(211),f2(211)  /0.285989 , 18.5652 , 6.80654 /
	data ee(212),f1(212),f2(212)  /0.290615 , 18.6195 , 6.93951 /
	data ee(213),f1(213),f2(213)  /0.295315 , 18.6897 , 7.07508 /
	data ee(214),f1(214),f2(214)  /0.300092 , 18.8057 , 7.21331 /
	data ee(215),f1(215),f2(215)  /0.304945 , 18.9234 , 7.26060 /
	data ee(216),f1(216),f2(216)  /0.309878 , 19.0080 , 7.29716 /
	data ee(217),f1(217),f2(217)  /0.31489 , 19.0713 , 7.33392 /
	data ee(218),f1(218),f2(218)  /0.319983 , 19.1248 , 7.37085 /
	data ee(219),f1(219),f2(219)  /0.325158 , 19.1713 , 7.40797 /
	data ee(220),f1(220),f2(220)  /0.330418 , 19.2124 , 7.44529 /
	data ee(221),f1(221),f2(221)  /0.335762 , 19.2493 , 7.48278 /
	data ee(222),f1(222),f2(222)  /0.341192 , 19.2829 , 7.52047 /
	data ee(223),f1(223),f2(223)  /0.346711 , 19.3138 , 7.55835 /
	data ee(224),f1(224),f2(224)  /0.352319 , 19.3426 , 7.59641 /
	data ee(225),f1(225),f2(225)  /0.358017 , 19.3701 , 7.63467 /
	data ee(226),f1(226),f2(226)  /0.363808 , 19.3974 , 7.67313 /
	data ee(227),f1(227),f2(227)  /0.369692 , 19.4263 , 7.71177 /
	data ee(228),f1(228),f2(228)  /0.375672 , 19.4655 , 7.75061 /
	data ee(229),f1(229),f2(229)  /0.381748 , 19.5031 , 7.76071 /
	data ee(230),f1(230),f2(230)  /0.387922 , 19.5253 , 7.76418 /
	data ee(231),f1(231),f2(231)  /0.394197 , 19.5345 , 7.76765 /
	data ee(232),f1(232),f2(232)  /0.400573 , 19.5356 , 7.77112 /
	data ee(233),f1(233),f2(233)  /0.407052 , 19.5294 , 7.77460 /
	data ee(234),f1(234),f2(234)  /0.413635 , 19.5164 , 7.77807 /
	data ee(235),f1(235),f2(235)  /0.420326 , 19.4972 , 7.78154 /
	data ee(236),f1(236),f2(236)  /0.427124 , 19.4722 , 7.78503 /
	data ee(237),f1(237),f2(237)  /0.434032 , 19.4422 , 7.78850 /
	data ee(238),f1(238),f2(238)  /0.441052 , 19.4082 , 7.79198 /
	data ee(239),f1(239),f2(239)  /0.448186 , 19.3783 , 7.79547 /
	data ee(240),f1(240),f2(240)  /0.455435 , 19.3645 , 7.78776 /
	data ee(241),f1(241),f2(241)  /0.462802 , 19.3252 , 7.71960 /
	data ee(242),f1(242),f2(242)  /0.470287 , 19.2438 , 7.65206 /
	data ee(243),f1(243),f2(243)  /0.477894 , 19.1344 , 7.58510 /
	data ee(244),f1(244),f2(244)  /0.485623 , 18.9992 , 7.51873 /
	data ee(245),f1(245),f2(245)  /0.493478 , 18.8383 , 7.45294 /
	data ee(246),f1(246),f2(246)  /0.501459 , 18.6506 , 7.38772 /
	data ee(247),f1(247),f2(247)  /0.50957 , 18.4343 , 7.32308 /
	data ee(248),f1(248),f2(248)  /0.517812 , 18.1867 , 7.25901 /
	data ee(249),f1(249),f2(249)  /0.526187 , 17.9046 , 7.19549 /
	data ee(250),f1(250),f2(250)  /0.534698 , 17.5839 , 7.13252 /
	data ee(251),f1(251),f2(251)  /0.543346 , 17.2149 , 7.07011 /
	data ee(252),f1(252),f2(252)  /0.552134 , 16.7959 , 7.01671 /
	data ee(253),f1(253),f2(253)  /0.561065 , 16.3253 , 6.96779 /
	data ee(254),f1(254),f2(254)  /0.570139 , 15.7908 , 6.91921 /
	data ee(255),f1(255),f2(255)  /0.579361 , 15.1775 , 6.87097 /
	data ee(256),f1(256),f2(256)  /0.588732 , 14.4668 , 6.82307 /
	data ee(257),f1(257),f2(257)  /0.598254 , 13.6367 , 6.77550 /
	data ee(258),f1(258),f2(258)  /0.60793 , 12.6493 , 6.72163 /
	data ee(259),f1(259),f2(259)  /0.617763 , 11.4392 , 6.65668 /
	data ee(260),f1(260),f2(260)  /0.627755 , 9.91544 , 6.59235 /
	data ee(261),f1(261),f2(261)  /0.637908 , 7.92458 , 6.52865 /
	data ee(262),f1(262),f2(262)  /0.648226 , 5.15405 , 6.46555 /
	data ee(263),f1(263),f2(263)  /0.658711 , 0.823389 , 6.40307 /
	data ee(264),f1(264),f2(264)  /0.669365 , -8.21883 , 6.34119 /
	data ee(265),f1(265),f2(265)  /0.6763 , -53.1265 , 6.30176 /
	data ee(266),f1(266),f2(266)  /0.6765 , -53.0765 , 40.0443 /
	data ee(267),f1(267),f2(267)  /0.680191 , -13.5912 , 39.7162 /
	data ee(268),f1(268),f2(268)  /0.691193 , 1.88287 , 38.7644 /
	data ee(269),f1(269),f2(269)  /0.702372 , 8.50978 , 37.8355 /
	data ee(270),f1(270),f2(270)  /0.713733 , 12.8348 , 36.9287 /
	data ee(271),f1(271),f2(271)  /0.725277 , 16.0447 , 36.0437 /
	data ee(272),f1(272),f2(272)  /0.737008 , 18.5805 , 35.1800 /
	data ee(273),f1(273),f2(273)  /0.748928 , 20.6559 , 34.3369 /
	data ee(274),f1(274),f2(274)  /0.761042 , 22.3906 , 33.5140 /
	data ee(275),f1(275),f2(275)  /0.773351 , 23.8460 , 32.7108 /
	data ee(276),f1(276),f2(276)  /0.785859 , 25.0739 , 31.9493 /
	data ee(277),f1(277),f2(277)  /0.79857 , 26.1402 , 31.2468 /
	data ee(278),f1(278),f2(278)  /0.811486 , 27.0729 , 30.5598 /
	data ee(279),f1(279),f2(279)  /0.824611 , 27.8731 , 29.8878 /
	data ee(280),f1(280),f2(280)  /0.837949 , 28.5435 , 29.2306 /
	data ee(281),f1(281),f2(281)  /0.851502 , 29.0794 , 28.5879 /
	data ee(282),f1(282),f2(282)  /0.865274 , 29.4623 , 27.9593 /
	data ee(283),f1(283),f2(283)  /0.879269 , 29.6403 , 27.3445 /
	data ee(284),f1(284),f2(284)  /0.893491 , 29.3691 , 26.7433 /
	data ee(285),f1(285),f2(285)  /0.907943 , 28.8453 , 27.1220 /
	data ee(286),f1(286),f2(286)  /0.922628 , 28.8239 , 27.9663 /
	data ee(287),f1(287),f2(287)  /0.937551 , 29.3445 , 28.8369 /
	data ee(288),f1(288),f2(288)  /0.952715 , 30.2131 , 29.7346 /
	data ee(289),f1(289),f2(289)  /0.968124 , 31.8753 , 30.4358 /
	data ee(290),f1(290),f2(290)  /0.983783 , 33.5306 , 29.9975 /
	data ee(291),f1(291),f2(291)  /0.999695 , 34.5075 , 29.5656 /
	data ee(292),f1(292),f2(292)  /1.01586 , 35.3891 , 29.1399 /
	data ee(293),f1(293),f2(293)  /1.03229 , 36.1745 , 28.7203 /
	data ee(294),f1(294),f2(294)  /1.04899 , 36.8886 , 28.3081 /
	data ee(295),f1(295),f2(295)  /1.06596 , 37.5503 , 27.9085 /
	data ee(296),f1(296),f2(296)  /1.0832 , 38.1725 , 27.5145 /
	data ee(297),f1(297),f2(297)  /1.10072 , 38.7608 , 27.1261 /
	data ee(298),f1(298),f2(298)  /1.11852 , 39.3204 , 26.7430 /
	data ee(299),f1(299),f2(299)  /1.13661 , 39.8555 , 26.3655 /
	data ee(300),f1(300),f2(300)  /1.155 , 40.3704 , 25.9932 /
	data ee(301),f1(301),f2(301)  /1.17368 , 40.8685 , 25.6260 /
	data ee(302),f1(302),f2(302)  /1.19266 , 41.3542 , 25.2644 /
	data ee(303),f1(303),f2(303)  /1.21195 , 41.8336 , 24.9077 /
	data ee(304),f1(304),f2(304)  /1.23155 , 42.3199 , 24.5560 /
	data ee(305),f1(305),f2(305)  /1.25147 , 42.8247 , 24.1689 /
	data ee(306),f1(306),f2(306)  /1.27172 , 43.3132 , 23.7415 /
	data ee(307),f1(307),f2(307)  /1.29229 , 43.7480 , 23.2653 /
	data ee(308),f1(308),f2(308)  /1.31319 , 44.1298 , 22.7985 /
	data ee(309),f1(309),f2(309)  /1.33443 , 44.4783 , 22.3412 /
	data ee(310),f1(310),f2(310)  /1.35601 , 44.7999 , 21.8932 /
	data ee(311),f1(311),f2(311)  /1.37794 , 45.0988 , 21.4540 /
	data ee(312),f1(312),f2(312)  /1.40023 , 45.3774 , 21.0235 /
	data ee(313),f1(313),f2(313)  /1.42288 , 45.6376 , 20.6017 /
	data ee(314),f1(314),f2(314)  /1.44589 , 45.8813 , 20.1883 /
	data ee(315),f1(315),f2(315)  /1.46928 , 46.1096 , 19.7834 /
	data ee(316),f1(316),f2(316)  /1.49304 , 46.3237 , 19.3868 /
	data ee(317),f1(317),f2(317)  /1.51719 , 46.5250 , 18.9992 /
	data ee(318),f1(318),f2(318)  /1.54173 , 46.7150 , 18.6195 /
	data ee(319),f1(319),f2(319)  /1.56667 , 46.8945 , 18.2476 /
	data ee(320),f1(320),f2(320)  /1.59201 , 47.0641 , 17.8828 /
	data ee(321),f1(321),f2(321)  /1.61776 , 47.2240 , 17.5254 /
	data ee(322),f1(322),f2(322)  /1.64392 , 47.3755 , 17.1753 /
	data ee(323),f1(323),f2(323)  /1.67051 , 47.5192 , 16.8318 /
	data ee(324),f1(324),f2(324)  /1.69753 , 47.6555 , 16.4953 /
	data ee(325),f1(325),f2(325)  /1.72499 , 47.7891 , 16.1656 /
	data ee(326),f1(326),f2(326)  /1.75289 , 47.9166 , 15.8343 /
	data ee(327),f1(327),f2(327)  /1.78124 , 48.0323 , 15.5070 /
	data ee(328),f1(328),f2(328)  /1.81005 , 48.1391 , 15.1843 /
	data ee(329),f1(329),f2(329)  /1.83932 , 48.2365 , 14.8680 /
	data ee(330),f1(330),f2(330)  /1.86907 , 48.3262 , 14.5584 /
	data ee(331),f1(331),f2(331)  /1.8993 , 48.4094 , 14.2557 /
	data ee(332),f1(332),f2(332)  /1.93002 , 48.4866 , 13.9588 /
	data ee(333),f1(333),f2(333)  /1.96124 , 48.5576 , 13.6681 /
	data ee(334),f1(334),f2(334)  /1.99296 , 48.6218 , 13.3835 /
	data ee(335),f1(335),f2(335)  /2.0252 , 48.6862 , 13.1109 /
	data ee(336),f1(336),f2(336)  /2.05795 , 48.7501 , 12.8333 /
	data ee(337),f1(337),f2(337)  /2.09124 , 48.8033 , 12.5599 /
	data ee(338),f1(338),f2(338)  /2.12506 , 48.8512 , 12.2914 /
	data ee(339),f1(339),f2(339)  /2.15943 , 48.8936 , 12.0278 /
	data ee(340),f1(340),f2(340)  /2.19436 , 48.9308 , 11.7689 /
	data ee(341),f1(341),f2(341)  /2.22985 , 48.9633 , 11.5149 /
	data ee(342),f1(342),f2(342)  /2.26592 , 48.9913 , 11.2654 /
	data ee(343),f1(343),f2(343)  /2.30257 , 49.0146 , 11.0201 /
	data ee(344),f1(344),f2(344)  /2.33981 , 49.0333 , 10.7797 /
	data ee(345),f1(345),f2(345)  /2.37766 , 49.0480 , 10.5433 /
	data ee(346),f1(346),f2(346)  /2.41611 , 49.0581 , 10.3112 /
	data ee(347),f1(347),f2(347)  /2.45519 , 49.0642 , 10.0837 /
	data ee(348),f1(348),f2(348)  /2.4949 , 49.0663 , 9.85962 /
	data ee(349),f1(349),f2(349)  /2.53526 , 49.0639 , 9.64058 /
	data ee(350),f1(350),f2(350)  /2.57626 , 49.0580 , 9.42503 /
	data ee(351),f1(351),f2(351)  /2.61793 , 49.0477 , 9.21347 /
	data ee(352),f1(352),f2(352)  /2.66027 , 49.0335 , 9.00620 /
	data ee(353),f1(353),f2(353)  /2.7033 , 49.0155 , 8.80231 /
	data ee(354),f1(354),f2(354)  /2.74703 , 48.9935 , 8.60254 /
	data ee(355),f1(355),f2(355)  /2.79146 , 48.9675 , 8.40617 /
	data ee(356),f1(356),f2(356)  /2.83661 , 48.9373 , 8.21365 /
	data ee(357),f1(357),f2(357)  /2.88249 , 48.9032 , 8.02469 /
	data ee(358),f1(358),f2(358)  /2.92911 , 48.8648 , 7.83922 /
	data ee(359),f1(359),f2(359)  /2.97648 , 48.8221 , 7.65729 /
	data ee(360),f1(360),f2(360)  /3.02463 , 48.7751 , 7.47879 /
	data ee(361),f1(361),f2(361)  /3.07355 , 48.7236 , 7.30363 /
	data ee(362),f1(362),f2(362)  /3.12326 , 48.6674 , 7.13165 /
	data ee(363),f1(363),f2(363)  /3.17378 , 48.6062 , 6.96295 /
	data ee(364),f1(364),f2(364)  /3.22511 , 48.5400 , 6.79748 /
	data ee(365),f1(365),f2(365)  /3.27727 , 48.4683 , 6.63504 /
	data ee(366),f1(366),f2(366)  /3.33028 , 48.3909 , 6.47567 /
	data ee(367),f1(367),f2(367)  /3.38415 , 48.3073 , 6.31930 /
	data ee(368),f1(368),f2(368)  /3.43888 , 48.2172 , 6.16590 /
	data ee(369),f1(369),f2(369)  /3.4945 , 48.1200 , 6.01544 /
	data ee(370),f1(370),f2(370)  /3.55102 , 48.0151 , 5.86774 /
	data ee(371),f1(371),f2(371)  /3.60846 , 47.9017 , 5.72276 /
	data ee(372),f1(372),f2(372)  /3.66682 , 47.7789 , 5.58056 /
	data ee(373),f1(373),f2(373)  /3.72613 , 47.6459 , 5.44120 /
	data ee(374),f1(374),f2(374)  /3.7864 , 47.5013 , 5.30424 /
	data ee(375),f1(375),f2(375)  /3.84764 , 47.3434 , 5.16996 /
	data ee(376),f1(376),f2(376)  /3.90987 , 47.1705 , 5.03820 /
	data ee(377),f1(377),f2(377)  /3.97311 , 46.9802 , 4.90890 /
	data ee(378),f1(378),f2(378)  /4.03738 , 46.7695 , 4.78203 /
	data ee(379),f1(379),f2(379)  /4.10268 , 46.5345 , 4.65756 /
	data ee(380),f1(380),f2(380)  /4.16903 , 46.2700 , 4.53550 /
	data ee(381),f1(381),f2(381)  /4.23646 , 45.9693 , 4.41575 /
	data ee(382),f1(382),f2(382)  /4.30498 , 45.6222 , 4.29816 /
	data ee(383),f1(383),f2(383)  /4.37462 , 45.2140 , 4.18271 /
	data ee(384),f1(384),f2(384)  /4.44537 , 44.7217 , 4.06953 /
	data ee(385),f1(385),f2(385)  /4.51727 , 44.1057 , 3.95854 /
	data ee(386),f1(386),f2(386)  /4.59033 , 43.2884 , 3.84952 /
	data ee(387),f1(387),f2(387)  /4.66458 , 42.0783 , 3.74260 /
	data ee(388),f1(388),f2(388)  /4.74003 , 39.6979 , 3.63771 /
	data ee(389),f1(389),f2(389)  /4.7871 , 24.7370 , 3.57457 /
	data ee(390),f1(390),f2(390)  /4.7873 , 24.7388 , 11.2300 /
	data ee(391),f1(391),f2(391)  /4.81669 , 38.5346 , 11.1152 /
	data ee(392),f1(392),f2(392)  /4.8946 , 41.4727 , 10.8198 /
	data ee(393),f1(393),f2(393)  /4.97377 , 42.3694 , 10.5324 /
	data ee(394),f1(394),f2(394)  /5.05421 , 42.2444 , 10.2525 /
	data ee(395),f1(395),f2(395)  /5.1071 , 35.5416 , 10.0749 /
	data ee(396),f1(396),f2(396)  /5.1073 , 35.5459 , 13.7396 /
	data ee(397),f1(397),f2(397)  /5.13596 , 42.2243 , 13.6172 /
	data ee(398),f1(398),f2(398)  /5.21903 , 44.2811 , 13.2720 /
	data ee(399),f1(399),f2(399)  /5.30344 , 45.2083 , 12.9357 /
	data ee(400),f1(400),f2(400)  /5.38922 , 45.6040 , 12.6079 /
	data ee(401),f1(401),f2(401)  /5.4527 , 42.4369 , 12.3739 /
	data ee(402),f1(402),f2(402)  /5.4529 , 42.4412 , 14.2726 /
	data ee(403),f1(403),f2(403)  /5.47639 , 45.7152 , 14.1809 /
	data ee(404),f1(404),f2(404)  /5.56497 , 47.2510 , 13.8442 /
	data ee(405),f1(405),f2(405)  /5.65498 , 48.1223 , 13.5140 /
	data ee(406),f1(406),f2(406)  /5.74644 , 48.7967 , 13.1897 /
	data ee(407),f1(407),f2(407)  /5.83939 , 49.3585 , 12.8715 /
	data ee(408),f1(408),f2(408)  /5.93383 , 49.8423 , 12.5592 /
	data ee(409),f1(409),f2(409)  /6.02981 , 50.2667 , 12.2530 /
	data ee(410),f1(410),f2(410)  /6.12733 , 50.6436 , 11.9527 /
	data ee(411),f1(411),f2(411)  /6.22644 , 50.9812 , 11.6584 /
	data ee(412),f1(412),f2(412)  /6.32715 , 51.2854 , 11.3700 /
	data ee(413),f1(413),f2(413)  /6.42948 , 51.5609 , 11.0876 /
	data ee(414),f1(414),f2(414)  /6.53348 , 51.8112 , 10.8106 /
	data ee(415),f1(415),f2(415)  /6.63915 , 52.0390 , 10.5397 /
	data ee(416),f1(416),f2(416)  /6.74654 , 52.2471 , 10.2746 /
	data ee(417),f1(417),f2(417)  /6.85565 , 52.4372 , 10.0151 /
	data ee(418),f1(418),f2(418)  /6.96654 , 52.6112 , 9.76125 /
	data ee(419),f1(419),f2(419)  /7.07922 , 52.7706 , 9.51281 /
	data ee(420),f1(420),f2(420)  /7.19372 , 52.9165 , 9.26990 /
	data ee(421),f1(421),f2(421)  /7.31007 , 53.0501 , 9.03234 /
	data ee(422),f1(422),f2(422)  /7.42831 , 53.1725 , 8.80037 /
	data ee(423),f1(423),f2(423)  /7.54845 , 53.2846 , 8.57347 /
	data ee(424),f1(424),f2(424)  /7.67054 , 53.3871 , 8.35171 /
	data ee(425),f1(425),f2(425)  /7.79461 , 53.4806 , 8.13507 /
	data ee(426),f1(426),f2(426)  /7.92068 , 53.5660 , 7.92366 /
	data ee(427),f1(427),f2(427)  /8.04879 , 53.6438 , 7.71695 /
	data ee(428),f1(428),f2(428)  /8.17898 , 53.7145 , 7.51516 /
	data ee(429),f1(429),f2(429)  /8.31126 , 53.7786 , 7.31810 /
	data ee(430),f1(430),f2(430)  /8.44569 , 53.8364 , 7.12583 /
	data ee(431),f1(431),f2(431)  /8.58229 , 53.8887 , 6.93824 /
	data ee(432),f1(432),f2(432)  /8.72111 , 53.9356 , 6.75508 /
	data ee(433),f1(433),f2(433)  /8.86216 , 53.9776 , 6.57633 /
	data ee(434),f1(434),f2(434)  /9.0055 , 54.0148 , 6.40187 /
	data ee(435),f1(435),f2(435)  /9.15116 , 54.0477 , 6.23183 /
	data ee(436),f1(436),f2(436)  /9.29917 , 54.0765 , 6.06593 /
	data ee(437),f1(437),f2(437)  /9.44958 , 54.1015 , 5.90408 /
	data ee(438),f1(438),f2(438)  /9.60242 , 54.1229 , 5.74633 /
	data ee(439),f1(439),f2(439)  /9.75773 , 54.1409 , 5.59257 /
	data ee(440),f1(440),f2(440)  /9.91555 , 54.1559 , 5.44270 /
	data ee(441),f1(441),f2(441)  /10.0759 , 54.1681 , 5.29650 /
	data ee(442),f1(442),f2(442)  /10.2389 , 54.1775 , 5.15401 /
	data ee(443),f1(443),f2(443)  /10.4045 , 54.1843 , 5.01514 /
	data ee(444),f1(444),f2(444)  /10.5728 , 54.1888 , 4.87979 /
	data ee(445),f1(445),f2(445)  /10.7438 , 54.1910 , 4.74792 /
	data ee(446),f1(446),f2(446)  /10.9176 , 54.1911 , 4.61942 /
	data ee(447),f1(447),f2(447)  /11.0942 , 54.1892 , 4.49423 /
	data ee(448),f1(448),f2(448)  /11.2736 , 54.1856 , 4.37228 /
	data ee(449),f1(449),f2(449)  /11.4559 , 54.1802 , 4.25349 /
	data ee(450),f1(450),f2(450)  /11.6412 , 54.1732 , 4.13778 /
	data ee(451),f1(451),f2(451)  /11.8295 , 54.1648 , 4.02508 /
	data ee(452),f1(452),f2(452)  /12.0208 , 54.1549 , 3.91534 /
	data ee(453),f1(453),f2(453)  /12.2153 , 54.1438 , 3.80846 /
	data ee(454),f1(454),f2(454)  /12.4128 , 54.1315 , 3.70439 /
	data ee(455),f1(455),f2(455)  /12.6136 , 54.1180 , 3.60307 /
	data ee(456),f1(456),f2(456)  /12.8176 , 54.1035 , 3.50441 /
	data ee(457),f1(457),f2(457)  /13.025 , 54.0880 , 3.40837 /
	data ee(458),f1(458),f2(458)  /13.2356 , 54.0716 , 3.31487 /
	data ee(459),f1(459),f2(459)  /13.4497 , 54.0543 , 3.22386 /
	data ee(460),f1(460),f2(460)  /13.6672 , 54.0363 , 3.13527 /
	data ee(461),f1(461),f2(461)  /13.8883 , 54.0175 , 3.04904 /
	data ee(462),f1(462),f2(462)  /14.1129 , 53.9980 , 2.96511 /
	data ee(463),f1(463),f2(463)  /14.3412 , 53.9779 , 2.88344 /
	data ee(464),f1(464),f2(464)  /14.5731 , 53.9572 , 2.80395 /
	data ee(465),f1(465),f2(465)  /14.8089 , 53.9360 , 2.72660 /
	data ee(466),f1(466),f2(466)  /15.0484 , 53.9142 , 2.65133 /
	data ee(467),f1(467),f2(467)  /15.2918 , 53.8919 , 2.57808 /
	data ee(468),f1(468),f2(468)  /15.5391 , 53.8692 , 2.50682 /
	data ee(469),f1(469),f2(469)  /15.7904 , 53.8461 , 2.43748 /
	data ee(470),f1(470),f2(470)  /16.0458 , 53.8225 , 2.37001 /
	data ee(471),f1(471),f2(471)  /16.3054 , 53.7986 , 2.30437 /
	data ee(472),f1(472),f2(472)  /16.5691 , 53.7743 , 2.24052 /
	data ee(473),f1(473),f2(473)  /16.8371 , 53.7497 , 2.17840 /
	data ee(474),f1(474),f2(474)  /17.1094 , 53.7247 , 2.11796 /
	data ee(475),f1(475),f2(475)  /17.3861 , 53.6995 , 2.05918 /
	data ee(476),f1(476),f2(476)  /17.6674 , 53.6739 , 2.00199 /
	data ee(477),f1(477),f2(477)  /17.9531 , 53.6480 , 1.94636 /
	data ee(478),f1(478),f2(478)  /18.2435 , 53.6218 , 1.89225 /
	data ee(479),f1(479),f2(479)  /18.5386 , 53.5952 , 1.83962 /
	data ee(480),f1(480),f2(480)  /18.8384 , 53.5684 , 1.78843 /
	data ee(481),f1(481),f2(481)  /19.1431 , 53.5413 , 1.73864 /
	data ee(482),f1(482),f2(482)  /19.4527 , 53.5138 , 1.69021 /
	data ee(483),f1(483),f2(483)  /19.7674 , 53.4861 , 1.64311 /
	data ee(484),f1(484),f2(484)  /20.0871 , 53.4579 , 1.59730 /
	data ee(485),f1(485),f2(485)  /20.412 , 53.4294 , 1.55275 /
	data ee(486),f1(486),f2(486)  /20.7421 , 53.4005 , 1.50942 /
	data ee(487),f1(487),f2(487)  /21.0776 , 53.3711 , 1.46727 /
	data ee(488),f1(488),f2(488)  /21.4185 , 53.3413 , 1.42629 /
	data ee(489),f1(489),f2(489)  /21.765 , 53.3110 , 1.38643 /
	data ee(490),f1(490),f2(490)  /22.117 , 53.2801 , 1.34767 /
	data ee(491),f1(491),f2(491)  /22.4747 , 53.2486 , 1.30998 /
	data ee(492),f1(492),f2(492)  /22.8382 , 53.2165 , 1.27332 /
	data ee(493),f1(493),f2(493)  /23.2076 , 53.1836 , 1.23767 /
	data ee(494),f1(494),f2(494)  /23.583 , 53.1499 , 1.20300 /
	data ee(495),f1(495),f2(495)  /23.9644 , 53.1153 , 1.16929 /
	data ee(496),f1(496),f2(496)  /24.352 , 53.0796 , 1.13650 /
	data ee(497),f1(497),f2(497)  /24.7459 , 53.0428 , 1.10462 /
	data ee(498),f1(498),f2(498)  /25.1462 , 53.0047 , 1.07361 /
	data ee(499),f1(499),f2(499)  /25.5529 , 52.9651 , 1.04346 /
	data ee(500),f1(500),f2(500)  /25.9662 , 52.9238 , 1.01414 /
	data ee(501),f1(501),f2(501)  /26.3861 , 52.8805 , 0.985625 /
	data ee(502),f1(502),f2(502)  /26.8129 , 52.8350 , 0.957896 /
	data ee(503),f1(503),f2(503)  /27.2466 , 52.7870 , 0.930929 /
	data ee(504),f1(504),f2(504)  /27.6873 , 52.7358 , 0.904706 /
	data ee(505),f1(505),f2(505)  /28.1351 , 52.6812 , 0.879202 /
	data ee(506),f1(506),f2(506)  /28.5902 , 52.6223 , 0.854402 /
	data ee(507),f1(507),f2(507)  /29.0526 , 52.5584 , 0.830283 /
	data ee(508),f1(508),f2(508)  /29.5225 , 52.4904 , 0.806827 /
	data ee(509),f1(509),f2(509)  /30 , 52.4028 , 0.784016 /

	j = 0

	if (energy .ge. 30.) then
	    call xenon (energy, xsec)
	    return
	end if

	if (energy .le. ee(1) ) then
	    xsec = -9999.
	    return
	end if

	do i=1,509
	    if (energy .gt. ee(i)) j=i
	end do
c  energy is between ee(j) and ee(j+1)
c
c	interpolate to get f2, use linear scale as grid is pretty fine

	ff2 = f2(j) + (f2(j+1)-f2(j)) * (energy-ee(j))/(ee(j+1)-ee(j))

	lambda = kevtoang/energy
	xsec = 2.*r0*lambda*ff2            ! cross section in angstrom^2/atom
	xsec = xsec*1.e-16                    !  cm^2/atom
	xsec = xsec/mxe			 !  cm^2/gm

	return
	end

	
	    
	subroutine xenonmcm (energy, xsec)

	real*4	ee(108), f1(108)
	real*4		energy, xsec
	real*4  ff2
	integer*4	i, j

      data ee(  1  ),f1(  1  ) / 1 , 7.1  /
      data ee(  2  ),f1(  2  ) / 2 , 2165.9  /
      data ee(  3  ),f1(  3  ) / 3 , 805.9  /
      data ee(  4  ),f1(  4  ) / 4 , 400.8  /
      data ee(  5  ),f1(  5  ) / 4.780 , 260.6  /
      data ee(  6  ),f1(  6  ) / 4.782 , 739.5  /
      data ee(  7  ),f1(  7  ) / 5 , 659.3  /
      data ee(  8  ),f1(  8  ) / 5.098 , 627.2  /
      data ee(  9  ),f1(  9  ) / 5.100 , 881.3  /
      data ee(  10  ),f1(  10  ) / 5.450 , 742.1  /
      data ee(  11  ),f1(  11  ) / 5.453 , 858.9  /
      data ee(  12  ),f1(  12  ) / 6 , 669.7  /
      data ee(  13  ),f1(  13  ) / 7 , 447.1  /
      data ee(  14  ),f1(  14  ) / 8 , 314.2  /
      data ee(  15  ),f1(  15  ) / 9 , 229.7  /
      data ee(  16  ),f1(  16  ) / 10 , 173.3  /
      data ee(  17  ),f1(  17  ) / 11 , 134.1  /
      data ee(  18  ),f1(  18  ) / 12 , 106.1  /
      data ee(  19  ),f1(  19  ) / 13 , 85.4  /
      data ee(  20  ),f1(  20  ) / 14 , 69.8  /
      data ee(  21  ),f1(  21  ) / 15 , 57.9  /
      data ee(  22  ),f1(  22  ) / 16 , 48.5  /
      data ee(  23  ),f1(  23  ) / 17 , 41.1  /
      data ee(  24  ),f1(  24  ) / 18 , 35.2  /
      data ee(  25  ),f1(  25  ) / 19 , 30.3  /
      data ee(  26  ),f1(  26  ) / 20 , 26.3  /
      data ee(  27  ),f1(  27  ) / 21 , 23.0  /
      data ee(  28  ),f1(  28  ) / 22 , 20.3  /
      data ee(  29  ),f1(  29  ) / 23 , 17.9  /
      data ee(  30  ),f1(  30  ) / 24 , 16.0  /
      data ee(  31  ),f1(  31  ) / 25 , 14.3  /
      data ee(  32  ),f1(  32  ) / 26 , 12.8  /
      data ee(  33  ),f1(  33  ) / 27 , 11.5  /
      data ee(  34  ),f1(  34  ) / 28 , 10.4  /
      data ee(  35  ),f1(  35  ) / 29 , 9.5  /
      data ee(  36  ),f1(  36  ) / 30 , 8.6  /
      data ee(  37  ),f1(  37  ) / 31 , 7.9  /
      data ee(  38  ),f1(  38  ) / 32 , 7.2  /
      data ee(  39  ),f1(  39  ) / 33 , 6.7  /
      data ee(  40  ),f1(  40  ) / 34 , 6.1  /
      data ee(  41  ),f1(  41  ) / 34.58 , 5.9  /
      data ee(  42  ),f1(  42  ) / 34.583 , 32.1  /
      data ee(  43  ),f1(  43  ) / 35 , 31.2  /
      data ee(  44  ),f1(  44  ) / 36 , 29.1  /
      data ee(  45  ),f1(  45  ) / 37 , 27.2  /
      data ee(  46  ),f1(  46  ) / 38 , 25.4  /
      data ee(  47  ),f1(  47  ) / 39 , 23.8  /
      data ee(  48  ),f1(  48  ) / 40 , 22.3  /
      data ee(  49  ),f1(  49  ) / 41 , 21.0  /
      data ee(  50  ),f1(  50  ) / 42 , 19.7  /
      data ee(  51  ),f1(  51  ) / 43 , 18.6  /
      data ee(  52  ),f1(  52  ) / 44 , 17.5  /
      data ee(  53  ),f1(  53  ) / 45 , 16.5  /
      data ee(  54  ),f1(  54  ) / 46 , 15.6  /
      data ee(  55  ),f1(  55  ) / 47 , 14.8  /
      data ee(  56  ),f1(  56  ) / 48 , 14.0  /
      data ee(  57  ),f1(  57  ) / 49 , 13.2  /
      data ee(  58  ),f1(  58  ) / 50 , 12.6  /
      data ee(  59  ),f1(  59  ) / 51 , 11.9  /
      data ee(  60  ),f1(  60  ) / 52 , 11.3  /
      data ee(  61  ),f1(  61  ) / 53 , 10.8  /
      data ee(  62  ),f1(  62  ) / 54 , 10.3  /
      data ee(  63  ),f1(  63  ) / 55 , 9.8  /
      data ee(  64  ),f1(  64  ) / 56 , 9.3  /
      data ee(  65  ),f1(  65  ) / 57 , 8.9  /
      data ee(  66  ),f1(  66  ) / 58 , 8.5  /
      data ee(  67  ),f1(  67  ) / 59 , 8.1  /
      data ee(  68  ),f1(  68  ) / 60 , 7.8  /
      data ee(  69  ),f1(  69  ) / 61 , 7.4  /
      data ee(  70  ),f1(  70  ) / 62 , 7.1  /
      data ee(  71  ),f1(  71  ) / 63 , 6.8  /
      data ee(  72  ),f1(  72  ) / 64 , 6.5  /
      data ee(  73  ),f1(  73  ) / 65 , 6.3  /
      data ee(  74  ),f1(  74  ) / 66 , 6.0  /
      data ee(  75  ),f1(  75  ) / 67 , 5.8  /
      data ee(  76  ),f1(  76  ) / 68 , 5.6  /
      data ee(  77  ),f1(  77  ) / 69 , 5.3  /
      data ee(  78  ),f1(  78  ) / 70 , 5.1  /
      data ee(  79  ),f1(  79  ) / 71 , 4.9  /
      data ee(  80  ),f1(  80  ) / 72 , 4.8  /
      data ee(  81  ),f1(  81  ) / 73 , 4.6  /
      data ee(  82  ),f1(  82  ) / 74 , 4.4  /
      data ee(  83  ),f1(  83  ) / 75 , 4.3  /
      data ee(  84  ),f1(  84  ) / 76 , 4.1  /
      data ee(  85  ),f1(  85  ) / 77 , 4.0  /
      data ee(  86  ),f1(  86  ) / 78 , 3.8  /
      data ee(  87  ),f1(  87  ) / 79 , 3.7  /
      data ee(  88  ),f1(  88  ) / 80 , 3.6  /
      data ee(  89  ),f1(  89  ) / 81 , 3.5  /
      data ee(  90  ),f1(  90  ) / 82 , 3.4  /
      data ee(  91  ),f1(  91  ) / 83 , 3.3  /
      data ee(  92  ),f1(  92  ) / 84 , 3.2  /
      data ee(  93  ),f1(  93  ) / 85 , 3.1  /
      data ee(  94  ),f1(  94  ) / 86 , 3.0  /
      data ee(  95  ),f1(  95  ) / 87 , 2.9  /
      data ee(  96  ),f1(  96  ) / 88 , 2.8  /
      data ee(  97  ),f1(  97  ) / 89 , 2.7  /
      data ee(  98  ),f1(  98  ) / 90 , 2.6  /
      data ee(  99  ),f1(  99  ) / 91 , 2.5  /
      data ee(  100  ),f1(  100  ) / 92 , 2.5  /
      data ee(  101  ),f1(  101  ) / 93 , 2.4  /
      data ee(  102  ),f1(  102  ) / 94 , 2.3  /
      data ee(  103  ),f1(  103  ) / 95 , 2.3  /
      data ee(  104  ),f1(  104  ) / 96 , 2.2  /
      data ee(  105  ),f1(  105  ) / 97 , 2.1  /
      data ee(  106  ),f1(  106  ) / 98 , 2.1  /
      data ee(  107  ),f1(  107  ) / 99 , 2.0  /
      data ee(  108  ),f1(  108  ) / 100 , 2.0  /

      j = 0
	if (energy .le. ee(1) ) then
	    xsec = -9999.
	    return
	end if

	if (energy .gt. 100.) then
	    xsec = 0.
	    return
	end if

	do i=1,108
	    if (energy .gt. ee(i)) j=i
	end do
c  energy is between ee(j) and ee(j+1)
c
c	interpolate to get f2, use linear scale as grid is pretty fine

	ff2 = f1(j) + (f1(j+1)-f1(j)) * (energy-ee(j))/(ee(j+1)-ee(j))

c	write (*,*) energy,i,j,ee(i),f1(i)

	xsec = ff2			 !  cm^2/gm

	return
	end

	
