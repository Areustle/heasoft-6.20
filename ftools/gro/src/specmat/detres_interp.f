CCCCCCCCCCCCCCCCCCCCCCC DETRES_INTERP(SOURCE) CCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  DETRES_INTERP
CH1
CH1  Version: 1.00                  Date: 6 August 1990
CH1
CH1  Programmer(s) and Completion Date:
CH1     Mark Fardal - Stanford University - 6 August 1990
CH1     Patrick Nolan - converted to SunOs - January 1991
CH1
CH1  Function: For a given tip and azimuth angle and viewing mode,
CH1     extract the detector response functions from the energy
CH1     resolution file and interpolate to produce a single tabulated
CH1     energy response function.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1
CH2  Calling Sequence:  Call DETRES_INTERP (IMOD, ITHLO, IPHLO, U, V, 
CH2     RESOLN, EROW, OVERFL, TASCIN, CALSET)
CH2
CH2  Argument            Type   I/O                 Description
CH2  --------            ----   ---  ----------------------------------------
CH2  IMOD               Integer  I   Number of viewing mode
CH2  ITHLO              Integer  I   Index to theta lower bracket
CH2  IPHLO              Integer  I   Index to phi lower bracket
CH2  U                   Real    I   Pos'n within theta bracket (0<=u<=1)
CH2  V                   Real    I   Pos'n within phi bracket (0<=u<=1)
CH2  RESOLN(100,20)     Real*8   O   Energy resolution table
CH2  EROW(100)          Real*8   O   Measured energies in cal'n files
CH2  OVERFL(20)         Real*8   O   Tail (beyond 2*Etrue) of en. resln.
CH2  TASCIN             Logical  I   Is TASC used in coincidence logic?
CH2  CALSET             Char*2   I   Which set of calibration tables
CH2
CH3  COMMON Use: None 
CH3
CH3  Calls:
CH3    CALFILENAME  to get name of appropriate EDP file
CH3
CH3  Significant Local Variables:
CH3  Variable      Type   Ini. Val.          Description
CH3  --------      ----   ---------  --------------------------------------
CH3  NREC*        Integer    -       Record #s bracketing source theta, phi
CH3  RESP*         Real      -       Energy res functions bracketing " "
CH3
CH3  Logical Units Used:
CH3       Variable  I/O              Description
CH3       --------  ---  ------------------------------------------
CH3        LUEDP     I   Energy resolution calibration file
CH3
CH4  Method:
CH4    Open file and read appropriate records
CH4    Interpolate linearly in theta, phi, and energy
CH4
CH4  Requirements and Assumptions:
CH4    The file format is assumed to be as described in the CALFIL
CH4      document as of December 1989. 
C
C  @(#) detres_interp.f 1.3@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine detres_interp(data_dir,cal_bin_dir,calib_dir,evclass,
     *     imod,ithlo,iphlo,u,v,resoln,erow,overfl,tascin,calset)

      implicit none

      save

*     ARGUMENTS
      character*(*)  data_dir, cal_bin_dir, calib_dir
      integer imod,ithlo,iphlo,evclass, status
      real u,v
      real*8 resoln(100,20),erow(100),overfl(20)
      logical tascin
      character(2) calset,fileid

      include '../SPECMAT_COMMON/lunits.inc'   ! for luedp

*     LOCAL VARIABLES
      logical fexist
      integer j,k,nrec1,nrec2,nrec3,nrec4,nphi,iphhi,clobber
      integer ind1, ind2
      character(80) edpfil, calfile, tmpfnm
      real resp1(101,20),resp2(101,20),resp3(101,20),resp4(101,20),
     >      uc,vc
      real darr1(102),darr2(102),darr3(102),darr4(102)

*---------------------------------------------------------------------*

      clobber = 1
      status  = 0

      if (calset.eq.'00') then
         nphi = 3
      else
         nphi = 16
      end if
      if (iphlo.eq.16) then
	 iphhi = 1
      else
	 iphhi = iphlo+1
      end if
*     Decide which records to read from file.  Straddle the given theta,phi
      if (ithlo.gt.1) then
         nrec1 = (ithlo-1)*20 + (iphlo-1)*9*20  + (imod-1)*nphi*9*20
         nrec4 = (ithlo-1)*20 + (iphhi-1)*9*20  + (imod-1)*nphi*9*20
      else
         nrec1 = (imod-1)*nphi*9*20
         nrec4 = nrec1
      end if
      nrec2 =  ithlo   *20 + (iphlo-1)*9*20  + (imod-1)*nphi*9*20
      nrec3 =  ithlo   *20 + (iphhi-1)*9*20  + (imod-1)*nphi*9*20

*     Open the file and read the necessary records
      call calfilename(data_dir,evclass,tascin,3,calset,'edp',fileid,
     *     edpfil)

C 
C     write code to call fits2cal to convert input FITS sar file to binary sar file
C
      ind1 = index(calib_dir, ' ') - 1
      ind2 = index(edpfil, ' ') - 1
      calfile = calib_dir(1:ind1) // edpfil(1:ind2) // '.fits'
      
      ind1 = index(cal_bin_dir, ' ') - 1
      edpfil = cal_bin_dir(1:ind1) // edpfil
      
      inquire (file=calfile, exist=fexist)
      if (.not. fexist) then
	 write(*,*) 'DETRES_INTERP:  ', calfile, ' does not exist'
         return
      endif

      call fileexists(edpfil, clobber, status)
      call fits2cal(fileid, calfile, edpfil)
      
      open (unit=luedp,file=edpfil,status='old',access='direct',
     >     form='unformatted',recl=408,err=999)
      do j = 1,20
	 read (luedp,rec=nrec1+j) darr1
	 read (luedp,rec=nrec2+j) darr2
	 read (luedp,rec=nrec3+j) darr3
	 read (luedp,rec=nrec4+j) darr4
*              Invert the byte order for Linux
c            call reflect(darr1,4,408)
c            call reflect(darr2,4,408)
c            call reflect(darr3,4,408)
c            call reflect(darr4,4,408)
	 do k = 1,101
	   resp1(k,j) = darr1(k+1)
	   resp2(k,j) = darr2(k+1)
	   resp3(k,j) = darr3(k+1)
	   resp4(k,j) = darr4(k+1)
	 end do
      end do
      close (unit=luedp)
      tmpfnm = 'rm -f ' // edpfil
      call system(tmpfnm)

* Linear interpolation in theta and phi
      uc = 1.-u
      vc = 1.-v
      do j = 1,20
	 do k = 1,100
  	    resoln(k,j) = uc*vc*resp1(k,j) + u*vc*resp2(k,j)
     >        + u*v*resp3(k,j) + uc*v*resp4(k,j)
	 end do
	 overfl(j) = uc*vc*resp1(101,j) + u*vc*resp2(101,j)
     >     + u*v*resp3(101,j) + uc*v*resp4(101,j)
         overfl(j) = 0.02 * overfl(j)  ! want integrated tail of p.d.
      end do

      do k = 1,100
         erow(k) = dble(0.02*k - 0.01)
      end do

      RETURN

999   write (6,*) 'ERROR OPENING FILE ENERGY RESOLUTION FILE--'
      write (6,*) '  MATRIX GENERATION ABORTED.'
      STOP

      end



