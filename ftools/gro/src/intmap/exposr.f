CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(EXPOSR) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  EXPOSR
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: exposr.f,v 1.4 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1
CH1  Function: Generate exposure data from the exposure history file
CH1            and from the SENSTV routines results.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call EXPOSR
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  Called by:  INTMAP
CH2
CH2  Calls:
CH2   GETVAL : Get input values from the Namelist
CH2   NXTMOD : Find the next viewing mode and return the live time
CH2   BINANG : Compute the map bin angles if change in pointing dir
CH2   SENSTV1: First MPE Sensitivity subroutine
CH2   SENSTV2: Second MPE Sensitivity subroutine
CH2   SENSTV3: Third MPE Sensitivity subroutine
CH2   SMOOTH : Smoothing routine from MPE for the exposure values
CH2
CH3  COMMON Use:
CH3  COMMON Block Name: FITSDT (Holds the FITS file variables)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 bitpix     Integer   Number of bits per pixels
CH3 naxis      Integer   Number of axis in the map
CH3 naxis1     Integer   Number of bins on the 1st axis
CH3 naxis2     Integer   Number of bins on the 2nd axis
CH3 naxis3     Integer   Number of bins on the 3rd axis
CH3 bscale(3)  Real      Bin scaling factor (counts, exposure, intensty)
CH3 bzero(3)   Real      Bin offset value (counts, exposure, intensty)
CH3 ftparm     Real*4    5 parameters (200 groups). index 1 to 5 are:
CH3   (5,200)            1:number bins in group,   2:position on axis1,
CH3                      3:position on axis2,      4:increment on axis1,
CH3                      5:increment on axis 2
CH3 gridtp     Ch*4      Grid type ('RECT', 'POLA' or 'AITF')
CH3 headpf(2)  Real      Two pointers for header buffer
CH3 evclas     Integer   Event class
CH3 energy     Real      Energy level ranges
CH3  (2,10)
CH3 pcount     Integer   Number of parameters in FITS file
CH3 gcount     Integer   Number of groups in FITS data
CH3 naxs12(200)Integer   Number of bins on axis with variable # of bins
CH3 crval1     Real      Coordinate of reference point on axis 1
CH3 crpix1     Real      Array index of reference point on axis 1
CH3 cdelt1     Real      Increment of coordinate along axis 1
CH3 crval2     Real      Coordinate of reference point on axis 2
CH3 crpix2     Real      Array index of reference point on axis 2
CH3 cdelt2     Real      Increment of coordinate along axis 2
CH3 coords     Ch*4      Coordinate system used ('GALA' or 'CELE')
CH3 buffer(3)  Ch*2880   FITS record buffer (may hold up to 3 header rc)
CH3 cntbin     Integer   Counts map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3 expbin     Real      Exposure map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3 intbin     Real      Intensity map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3
CH3  COMMON Block Name: GLOBAL (Holds the main variables)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 strtim     Real*8    FITS data start time (TJD & MSD combined)
CH3 endtim     Real*8    FITS data end time (TJD & MSD combined)
CH3 retcod     Integer   Program return code (0 = normal)
CH3 specin(10) Real      Spectral indexes
CH3 walldi     Integer   Wall distance (from SAGE Namelist)
CH3 maxlev     Integer   Maximum number of energy levels (10)
CH3 tascco     Integer   TASC in coincidence flag
CH3 acs        Integer   ACS value
CH3 tunit      Integer   Unit number for timeline file
CH3 eunit      Integer   Unit number for exposure history file
CH3 calfil(2)  Ch*8      Names of the calibration files
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  exgbin     R*4        0      Exposure data for galactic disk maps
CH3  (594,66,10)
CH3  solidn(200)R*4        -      Solid angles
CH3  temp       R*4        -      Temporary buffer to save memory space
CH3  (400000)
CH3  effsar     R*4        -      Effective sensitivity area (SENSTV2)
CH3  (10,74,9,3)
CH3  flux(10)   R*4        -      Relative flux of assumed sky (SENSTV2)
CH3  efsens(10) R*4        -      Effective sensitivity area (SENSTV2)
CH3  polang     R*4        -      Polar angles for all map bins
CH3  (594,200)
CH3  azmang     R*4        -      Azimuth angles for all map bins
CH3  (594,200)
CH3  dir(4)     R*4        -      Instrument pointing direction
CH3  imod       I*4        -      Active viewing mode
CH3  typs       I*2        -      Type mode
CH3  oldtyp     I*2       -1      Previous value of typs
CH3  oldtsc     R*4       -1      Previous value of tascco
CH3  termin     L*4        -      Process terminated flag
CH3  calid(2)   Ch*2       -      Calibration file number
CH3  thr        Ch*4       -      TASC OK treshold from text file
CH3  oldthr     Ch*4      '-1'    Old value of thr
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       6              Printer report
CH4
CH4  Method:
CH4    Call GETVAL to get the ACS and wall distance values from Namelist
CH4    Call NXTMOD to get the next instrument mode from expo. hist. file
CH4    While (NXTMOD has not set the terminate flag) do
CH4       Call BINANG to compute the bin angles for this mode
CH4       If (tasc in coincidence or type or threshold changed) then
CH4          Call SENSTV1 to find the calibration files
CH4          Call SENSTV2 to generate the effective sensitivity area
CH4       End if
CH4       If (map is rectagular or Aitoff) then
CH4          For (j=1 to the number of bins on axis2) do
CH4             If (map type is Aitoff) compute # of bins on axis 1
CH4             For (i=1 to the number of bins on axis1) do
CH4                Call SENSTV3 to compute final effective sensitivit
CH4                Compute and accumulate bin i,j exposure value
CH4             End for
CH4          End for
CH4       Else if (map is polar)
CH4          For (i=1 to the number of bins on polar axis) do
CH4             For (j=1 to the number of bins on azimuth axis) do
CH4                Call SENSTV3 to compute final effective sensitivit
CH4                Compute and accumulate bin i,j exposure value
CH4             End for
CH4          End for
CH4       End if
CH4       Call NXTMOD to get the next instrument mode
CH4    End while
CH4  End EXPOSR
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Changed calid*2(2) to calid(2)*2.
CH5				Changed ! comments to C comments.
CH5				Merged changes from version 1.03 on IBM:
CH5				Used live time as an array
CH5                             instead of a scalar because of the
CH5                             earth shadow effect. Cut the exposure
CH5                             off at a selected angle from
CH5                             the detector axis. Changed call to
CH5                             NXTMOD due to earth shadow processing
CH5                             Printed values from the
CH5                             sensitivity routines and called
CH5                             the SMOOTH MPE routine. Passed
CH5                             angles to SENSTV3 in degrees.
CH5                             Printed warning message on negative exposures.
CH5     2.01    S. Bansal       Pass misc_dir to this function so that it can
CH5                             be passed to getval.
CH5 $Log: exposr.f,v $
CH5 Revision 1.4  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.3  2002/12/26 17:16:31  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.2  2002/04/18 19:37:56  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:02  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.8  1995/01/20  20:57:02  albert
c Saved the calibration table index to be written to the exposure file header
c
c Revision 2.7  1995/01/11  18:14:18  albert
c Corrected wrong dimension with effsar array.
c
c Revision 2.6  1995/01/09  21:22:49  albert
c Changed to call the smoothing routine only when the new calibration code is
c null
c
c Revision 2.5  1994/12/09  14:43:47  albert
c Modified the calling sequence to the sensitivity routines to pass the
c calibration table index supplied by NXTMOD.
c
c Revision 2.4  1992/10/14  16:01:22  albert
c Made the solid angle dependent on the energy level so as to include the
c sensitivity scale factor which depends on the energy level.
c
c Revision 2.3  1992/04/08  15:01:36  albert
c Changed polar angles and azimuth angles arrays to be variable dimensioned
c as the bin data arrays are.
c
c Revision 2.2  1992/04/01  21:17:06  albert
c Used variable dimension arrays to store the bin data.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CH5
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine EXPOSR(data_dir,calib_dir,cal_bin_dir,expbin,ltmap,
     *			polang,azmang,solidn)


      real	  expbin(naxis1,naxis2,naxis3),exp,conv
      real	  ltmap(naxis1,naxis2,naxis3)
      real        effsar(10,74,9,16),flux(10),efsens(10),dir(4)
      integer     imod,oldtsc,calcod,oldcal,ir,i,j,k,n
      integer*2   typs,oldtyp
      logical     termin
      character(2) calid(2)
      character(4) thr,oldthr
      character*(*)  data_dir, calib_dir, cal_bin_dir

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'
      real    polang(naxis1,naxis2),azmang(naxis1,naxis2)
      real    solidn(naxis2,naxis3)
      save

      data oldtsc /-1/, oldtyp/-1/, oldthr/'-1'/, oldcal /-1/
      data conv/57.295779513/

      character(80)	id
      common	/id/id
      id = '$Id: exposr.f,v 1.4 2013/05/21 19:08:24 irby Exp $'

      call GETVAL(acs,walldi,strtim,endtim,retcod)
      
C---> Max angle from detector axis in radians
      detmax = detmax / conv
      if (retcod .ne. 0) return
      calstr = ' '

C---> Call NXTMOD to get the first instrument mode with its live time
      call NXTMOD(tascco,imod,typs,dir,thr,calcod,ltmap,termin)
      if (retcod .ne. 0) return

C---> Loop until the last mode is found in the exposure history file
      do while (.not.termin)

C------> Skip loop processing if imod=1
	 if (imod.eq.1) goto 10

C------> Compute new sky bin angles if change in pointing direction
         call BINANG(dir,polang,azmang,solidn)

C------> Call the sensitivity routines if there is a change
	 if (tascco.ne.oldtsc.or.typs.ne.oldtyp.or.thr.ne.oldthr.or.
     +   calcod.ne.oldcal) then
            write(6,2000) evclas,typs,walldi,tascco
	    call SENSTV1(data_dir,7,evclas,typs,walldi,acs,tascco,calid,
     +					calfil,ir,calcod)
	   if (ir .ne. 0) goto 100
    	   write(6,3000) naxis3,calfil
	   if (calstr .eq. ' ') write(calstr,'("  CALTBL",i2.2)') calcod
           do i=1,naxis3
    	     write(6,'(23x,3f9.1)') energy(1,i),energy(2,i),specin(i)
    	   end do
    
           call SENSTV2(calib_dir,cal_bin_dir,energy,naxis3,specin,
     *			calid,effsar,flux,ir,calcod,calfil)

    	   if (calcod .eq. 0) call SMOOTH(tascco,effsar,naxis3,74,9,16)

    	   if (ir .ne. 0) goto 200
           oldtsc = tascco
    	   oldtyp = typs
    	   oldthr = thr
    	   oldcal = calcod
    	 end if

C------> Compute the rectangular or Aitoff map bins                             
         if (gridtp.eq.'RECT'.or.gridtp.eq.'AITF') then
            do j=1,naxis2
               n = naxis1
               if (gridtp.eq.'AITF') n = naxs12(j)
               do i=1,n
	          if (abs(polang(i,j)).le.detmax) then
                     theta = polang(i,j)*conv
                     azm = azmang(i,j)*conv
		     call SENSTV3(naxis3,imod,theta,
     &                            azm,effsar,efsens,calcod)
		     do k=1,naxis3
		     	exp = ltmap(i,j,k)*solidn(j,k)*efsens(k)
			expbin(i,j,k) = expbin(i,j,k) + exp
c			   if (exp.lt.0) write(6,1000) i,j,k,exp,
c    &                        ltmap(i,j,k),efsens(k),solidn(j)
		     end do
		  endif
	       end do
	    end do

C------> Do the polar map
         else if (gridtp.eq.'POLA') then

C---------> Loop over the polar bins
            do i=1,naxis1

C------------> Loop over the azimuth bins
               do j=1,naxs12(i)
		 if (abs(polang(i,j)).le.detmax) then
                     theta = polang(i,j)*conv
                     azm = azmang(i,j)*conv
                     call SENSTV3(naxis3,imod,theta,
     &				azm,effsar,efsens,calcod)
                   do k=1,naxis3
		     exp = ltmap(i,j,k)*solidn(i,k)*efsens(k)
		     expbin(i,j,k) = expbin(i,j,k) + exp
c		     if (exp.lt.0) write(6,1000) i,j,k,exp,
c    &                     ltmap(i,j,k),efsens(k),solidn(j)
                   end do
		 endif
               end do
            end do
         endif

C------> Call NXTMOD to get the next instrument mode with its live time
10       continue
         call NXTMOD(tascco,imod,typs,dir,thr,calcod,ltmap,termin)
         if (retcod .ne. 0) return

      end do

      return

C---> Bad return code from SENSTV routines
100   continue
      write(6,*) 'EXPOSR: bad return code from SENSTV1:',ir
      retcod = 8
      return
200   continue
      write(6,*) 'EXPOSR: bad return code from SENSTV2:',ir
      retcod = 8
      return

1000  format(1x,'Negative Exposure: I=',i3,' J=',i3,' K=',
     &       i3,' Exposure=',g13.8,' Live Time=',g13.8,' Eff Area=',
     &       g13.8,' Solid Angle=',g13.8)
2000  format(/1x,'Calling SENSTV1 with  Event class=,',i2,' Type mode=',
     &       i4,' Wall Distance=',i2,' TASC flag=',i2)
3000  format(1x,'Calling SENSTV2 with    LOW LEVEL HI LEVEL SPEC INX',
     &   5x,'Number of E levels=',i3,' Calibration file id=',2(a,2x),/)

CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(EXPOSR) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
