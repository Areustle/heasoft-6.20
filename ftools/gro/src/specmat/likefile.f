CCCCCCCCCCCCCCCCCCCCCCCCCCC LIKEFILE(SOURCE) CCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  likefile
CH1
CH1  Version: 2.10                  Date: 9 July 1992
CH1
CH1  Programmer(s) and Completion Date:
CH1     Patrick Nolan 
CH1
CH1  Function:  Read the likelihood output file to get the energy bands,
CH1     time intervals, and source position.
CH1     
CH1
CH1  Software System and Spacecraft:  SPECTRAL, EGRET project
CH1
CH1  Computer and Language:  Sun Fortran 1.4
CH1
CH2  Calling Sequence:  Call LIKEFILE (EMIN, EMAX, 
CH2      INTTIME, NINTS, CELEQ, SRCLON, SRCLAT, NREGNS, 
CH2      EREGNS, CONETHETA, CUTANGLE)
CH2  
CH2  Argument            Type   I/O                 Description
CH2  --------            ----   ---  -----------------------------------
CH2  EMIN                Real    O   Minimum energy of range
CH2  EMAX                Real    O   Maximum energy of range
CH2  INTTIME(2,NINTMAX) Real*8   O   SELECTed time intervals
CH2  NINTS              Integer  O   Number of intervals
CH2  CELEQ              Logical  O   Coordinate system of source direction
CH2  SRCLON              Real    O   Right ascension or LII
CH2  SRCLAT              Real    O   Declination or BII
CH2  NREGNS             Integer  O   Number of energy bands
CH2  EREGNS(0:JMAX)      Real    O   Upper energy of each band
CH2  CONETHETA(0:JMAX)   Real    O   Size of acceptance cone for each band
CH2  CUTANGLE(0:JMAX)    Real    O   Zenith angle cutoff for each band
CH2
CH2  Calls:
CH2    BEHEAD: Chops non-numeric front off string
CH2    TCONV: Convert character date and time into Real*8 number
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable      Type   Ini. Val.          Description
CH3  --------      ----   ---------  -----------------------------------
CH3  CHARLINE    Char*130     -      Line of likelihood file
CH3
CH3  Logical Units Used:
CH3       Variable  I/O              Description
CH3       --------  ---  ------------------------------------------
CH3        LUSEL     I   SELECT file; main data for SPECTRAL
CH3
CH4  Method:
CH4  
CH4  Requirements and Assumptions:
CH4    handles both incompatible file formats produced by different
CH4    versions of PSRSPEC and by Skyutil
C
C  @(#) likefile.f 1.2@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine likefile(emin,emax,inttime,nints,
     >     celeq,srclon,srclat,nregns,eregn,
     >     conetheta,cutangle,data_dir,selfil)

      implicit none

      include '../SPECMAT_COMMON/spectral.inc'
      include '../SPECMAT_COMMON/lunits.inc'   ! for lusel

*     ARGUMENTS
      character*(*) data_dir, selfil
      integer nregns,nints
      real emin,emax,srclon,srclat
      real eregn(0:jmax),conetheta(0:jmax),cutangle(0:jmax),vp
      real*8 inttime(2,nintmax),startime,endtime
      logical celeq

*     LOCAL VARIABLES
c      character(100) selfil
      character(130) charline
      character(8) keywd1
      real e1,e2,dum1,dum2,angle
      logical strtchk,endchk,poschk,done
      integer j

*     STRUCTURE OF INPUT LINES
*     equivalence (charline(1:8),keywd1)

      save


*----------------------------------------------------------------------*


      nints = 0
      strtchk = .false.
      endchk = .false.
      poschk = .false.
      write (6,*) ' Reading likelihood file...'
csb-02/05      call getenv('SELECTFILE',selfil)


      j = index(data_dir, ' ') - 1
      selfil = data_dir(1:j) // selfil

      open (unit=lusel,file=selfil,status='old')
                            
      done = .false.
      do while (.not.done)
         read (lusel,'(a130)') charline
         keywd1=charline(1:8)
         if (keywd1.eq.'Source p') then    ! Source RA and Dec
            call behead(130,charline)
            read (charline,*) srclon,srclat
            celeq = .true.
            write (6,*) '   Obtained source position.'
            poschk = .true.
         else if (keywd1.eq.'Viewing ') then   ! Start and end times
            call behead(130,charline)
            read (charline,*,err=77,end=77) vp,startime,endtime
	    nints = nints+1
            if (nints.gt.nintmax) go to 666
            inttime(1,nints) = startime
            inttime(2,nints) = endtime
            write (6,*) '   Obtained times ',startime,endtime
            strtchk = .true.
	    endchk = .true.
77          continue	! old format has no times in this line
	 else if (keywd1.eq.'Start ti') then   ! Start time
	    call behead(130,charline)
	    nints = nints + 1
            if (nints.gt.nintmax) go to 666
	    read (charline,*) startime
	    inttime(1,nints) = startime
	    write (6,*) '   Obtained start time ',startime
	    strtchk = .true.
	 else if (keywd1.eq.'End time') then   ! End time
	    call behead(130,charline)
	    read(charline,*) endtime
	    inttime(2,1) = endtime
	    write (6,*) '   Obtained end time ',endtime
	    endchk = .true.
	    done = .true.
         else if (keywd1.eq.'.......e') then   ! sentinel
	    done = .true.
         end if
      end do

      nregns = 0
      do while (.true.)  ! read flux values from data table
         read (lusel,*,err=999,end=999) e1,e2,dum1,dum2,angle
         if (e1.le.0.) go to 999
         if (nregns.ge.jmax) go to 999
         nregns = nregns + 1
         if (nregns.eq.1) then
            eregn(0) = e1
            cutangle(0) = angle
         end if
         eregn(nregns) = e2
         cutangle(nregns) = angle
      end do

999   close (unit=lusel)
      do j = 0,jmax
         conetheta(j) = 20.
      end do
      emin = eregn(0)
      emax = eregn(nregns)
      if (.not.(strtchk.and.endchk.and.poschk)) 
     >     then
         write (6,*) 'LIKEFILE failed to get parameters from file ',
     >        selfil
         write (6,*) 'Program aborted.'
         STOP
      end if

      return

 666  write (*,*) 'Number of intervals exceeds NINTMAX.'
      STOP

      end  
