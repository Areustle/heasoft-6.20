CCCCCCCCCCCCCCCCCCCCCCCCCCC DETEFF_INTERP(SOURCE) CCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  DETEFF_INTERP
CH1
CH1  Version: 1.00                  Date: 6 August 1990
CH1
CH1  Programmer(s) and Completion Date:
CH1     Mark Fardal - Stanford University - 6 August 1990
CH1     Patrick Nolan - converted to SunOS - January 1991
CH1       Smoothing for theta=5,10 added for version 2.5, Sept 93
CH1        -- this required creating subroutine READSAR
CH1       Support for multiple sets of calib files added in ver 2.7, Dec 94
CH1       Smoothing limited to old cal. files only.  Ver 2.9, Feb 95
CH1
CH1  Function:  For a given tip and azimuth angle and operating
CH1     mode, extract the appropriate detector sensitive area
CH1     functions from the SARFIL data base and interpolate to
CH1     produce a single tabulated sensitive area function.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1
CH2  Calling Sequence:  Call DETEFF_INTERP(IMOD, ITHLO, IPHLO, U,
CH2     V, EFF_TABLE, ICLASS, TASCIN, CALSET)
CH2
CH2  Argument            Type   I/O                 Description
CH2  --------            ----   ---  ----------------------------------------
CH2  IMOD               Integer  I   Number of viewing mode
CH2  ITHLO              Integer  I   Index to true theta's lower bracket
CH2  IPHLO              Integer  I   Index to true phi's lower bracket
CH2  U                   Real    I   Extent in theta across bracket
CH2  V                   Real    I   Extent in phi across bracket
CH2  EFF_TABLE(20)       Real    O   Sensitive area
CH2  ICLASS             Integer  I   Energy resolution class
CH2  TASCIN             Logical  I   Is TASC in coincidence logic?
CH2  CALSET             char*2   I   Which calibration tables?
CH2
CH3  Significant Local Variables:
CH3  Variable      Type   Ini. Val.          Description
CH3  --------      ----   ---------  --------------------------------------
CH3  EFF*(20)      Real       -      Effective area (by true energy)
CH3  SARFIL     char*100      -      Name of SAR file
CH3
CH3  Logical Units Used:
CH3       Variable  I/O              Description
CH3       --------  ---  ------------------------------------------
CH3        LUSAR     I   Sensitive areas for classes A and C
CH3 
CH3  Calls:
CH3     CALFILENAME  to find name of appropriate SAR file.
CH3     READSAR to read portions of the file for two theta values.
CH3
CH4  Method:
CH4    Open file corresponding to event class
CH4    Read records bracketing the source theta and phi
CH4    Interpolate linearly in theta, phi, and energy
CH4
CH4  Requirements and Assumptions:
CH4    U and V range from 0 to 1 and indicate where source angles lie
CH4      within bracketing angles.
CH4    The calibration files follow the format in the CALFIL document
CH4      as of December 1989.
C
C   @(#) deteff_interp.f 1.4@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine deteff_interp(data_dir,cal_bin_dir,calib_dir,evclass,
     *     imod,ithlo,iphlo,u,v,eff_table,iclass,tascin,calset)

      implicit none

*     ARGUMENTS
      character*(*)  data_dir, cal_bin_dir, calib_dir, calset
      integer imod,ithlo,iphlo,iclass,evclass,clobber,status
      real u,v,eff_table(20)
      logical tascin, fexist
      character(2) fileid

      include '../SPECMAT_COMMON/lunits.inc'    ! for lusar

*     LOCAL VARIABLES
      integer i, ind1, ind2
      real eff1(20),eff2(20)
      character(80) sarfil, calfile, tmpfnm

      save

*-------------------------------------------------------------------*
      clobber = 1
      status  = 0

*     To do for the new calibration files
*     interpolation in readsar probably needs a new method


C Open the file 
      call calfilename(data_dir,evclass,tascin,iclass,calset,'sar',
     *     fileid,sarfil)
C 
C     write code to call fits2cal to convert input FITS sar file to binary sar file
C
      ind1 = index(calib_dir, ' ') - 1
      ind2 = index(sarfil, ' ') - 1
      calfile = calib_dir(1:ind1) // sarfil(1:ind2) // '.fits'
      
      ind1 = index(cal_bin_dir, ' ') - 1
      sarfil = cal_bin_dir(1:ind1) // sarfil

      inquire (file=calfile, exist=fexist)
      if (.not. fexist) then
	 write(*,*) 'deteff_interp::  ', calfile, ' does not exist'
c         stop
         return
      endif

      call fileexists(sarfil, clobber, status)
      call fits2cal(fileid, calfile, sarfil)
      
      open (unit=lusar,file=sarfil,status='old',
     >     access='direct',form='unformatted',recl=80,
     >     err=1001)
      
C     Read appropriate records and interpolate in phi
      call readsar(lusar,ithlo,  iphlo,imod,v,tascin,eff1,calset)
      call readsar(lusar,ithlo+1,iphlo,imod,v,tascin,eff2,calset)
      close (unit=lusar)
      tmpfnm = 'rm -f ' // sarfil
      call system(tmpfnm)

C     Linear interpolation in theta
      do i = 1,20
         eff_table(i) = (1.-u)*eff1(i) + u*eff2(i)
      end do

      RETURN

 1001 write (6,*) 'ERROR OPENING SENSITIVE AREA FILE ',sarfil
      STOP


      end

*******************************************************************

C READSAR
*
*  Read all the relevant SAR records for a given mode, theta, phi.
*  Interpolate in phi if necessary.

      subroutine readsar(lunit,ith,iphlo,imod,v,tascin,eff,calset)

* Arguments
      implicit none
      character*(*)  calset
      integer lunit,ith,iphlo,imod
      real v,eff(20)
      logical tascin

* Local variables
      real effy1(20),effy2(20),effy3(20),y(3)
      integer irec,i,nphi,iphhi,irec2

      save

      if (calset.eq.'00') then
         nphi = 3
      else
         nphi = 16
      end if

* The general record number is 1+(ith-1)+9*(iph-1)+nphi*9*(imod-1)

      if (ith.eq.1) then
*     Only one phi value at theta=0.  No interpolation needed.
         irec = 1+9*nphi*(imod-1)
         read(lunit,rec=irec) eff
*     Invert the byte order for Linux
c     call reflect(eff,4,80)
      else if ((ith.eq.2.or.ith.eq.3).and.calset.eq.'00') then
*     Special treatment for theta= 5 or 10. Smooth or fit all phi.
*     So far this applies only to old calibration files. Stay tuned.
         irec = 1+(ith-1)+nphi*9*(imod-1)
         read (lunit,rec=irec) effy1
         read (lunit,rec=irec+9) effy2
         read (lunit,rec=irec+18) effy3
*     After reading, reflect the byte order for Linux
c     call reflect( effy1,4,80)
c     call reflect( effy2,4,80)
c     call reflect( effy3,4,80)
         if (tascin) then       ! Smooth out all phi dependence
            do i = 1,20
               eff(i) = (effy1(i)*effy2(i)*effy3(i))**(1./3.)
            enddo
         else                   ! Fit a straight line to the 3 phi values
            do i = 1,20
               y(1) = log(effy1(i))
               y(2) = log(effy2(i))
               y(3) = log(effy3(i))
               call fitazi(y)
               eff(i) = (1.-v)*exp(y(iphlo)) + v*exp(y(iphlo+1))
            enddo
         endif
      else
*     Read the two records that bracket phi.  Linear interpolation.
	 if (iphlo.eq.16) then
	    iphhi = 1
         else
	    iphhi = iphlo+1
         end if
         irec = 1+(ith-1)+9*(iphlo-1)+nphi*9*(imod-1)
         irec2= 1+(ith-1)+9*(iphhi-1)+nphi*9*(imod-1)
         read (lunit,rec=irec) effy1
         read (lunit,rec=irec2) effy2
c     call reflect( effy1,4,80)
c     call reflect( effy2,4,80)
         do i = 1,20
            eff(i) = (1.-v)*effy1(i) + v*effy2(i)
         end do
      endif

      end

*************************************************************

      subroutine fitazi(y)
C Fit a straight line to 3 equally-spaced points.  
C Return the fitted values at the the points.
      real y(3)
      real t(3)
      integer i

      do i = 1,3
         t(i) = y(i)
      end do
      y(1) = (5.*t(1)+2.*t(2)-t(3))/6.
      y(2) = (t(1)+t(2)+t(3))/3.
      y(3) = (-t(1)+2.*t(2)+5.*t(3))/6.
      return
      end

      


