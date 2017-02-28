CCCCCCCCCCCCCCCCCCCCCCCCCCCCC EXHIST(SOURCE) CCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  EXHIST
CH1
CH1  Version: 1.00                  Date: 3 August 1990
CH1
CH1  Programmer and Completion Date:
CH1     Mark Fardal - Stanford University - 3 August 1990
CH1     Patrick Nolan - converted to SunOS - January 1991
CH1       - time is beginning of interval - July 1991
CH1       - moved call to main loop; removed iorn loop here  Dec 94
CH1
CH1  Function:  Takes a list of intervals in which the orientation
CH1     of the instrument is constant (and does not ovelap viewing
CH1     periods).  For each interval produces a list of the total
CH1     live time spent in each viewing mode.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1			     Sun Sparc -SunOS 4.1.1 - Sun Fortran 1.3.1
CH1
CH2  Calling Sequence:  Call EXHIST(ORNTIME,ORNVIEW,NORNS,TMODE,
CH2                       NORNMAX,CUTANGLE,SRA,SDEC,NREGNS,IORN)
CH2
CH2  Argument            Type   I/O                 Description
CH2  --------            ----   ---  ----------------------------------------
CH2  ORNTIME(2,NORNMAX)  Real*8  I   Start and end times of orientations
CH2  ORNVIEW(NORNMAX)    Char*4  I   Viewing periods of orientations
CH2  NORNS               Integer I   Number of orientation periods
CH2  TMODE(MODEMAX,0:JMAX) Real  O   Time in (viewing mode, energy band)
CH2  NORNMAX             Integer I   Dimension of orientation arrays
CH2  CUTANGLE(0:JMAX)    Real    I   Zenith angle cutoff for energy bins
CH2  SRA                 Real    I   Right ascension of source (degrees)
CH2  SDEC                Real    I   Declination of source (degrees)
CH2  NREGNS              Real    I   Number of energy bands
CH2  IORN                Int     I   Which orientation to process
CH2
CH2  Calls:
CH2   MCONV: Converts type and direction modes into viewing modes
CH2   CONVRT: Converts julian day and milliseconds to a real*8 number
CH2
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.          Description
CH3  --------   ----   ---------  --------------------------------------
CH3  LINE      Char*130   -       Input line from file
CH3  MODE      Integer    -       Number of viewing mode
CH3  OPEN      Logical    -       T = in middle of orientation period
CH3  N         Integer    -       Number of orientation period
CH3  FLAGS(3)  Logical    -       SAA, pointing deviation, exclude flags
CH3
CH4  Logical Units Used:
CH4       Variable  I/O              Description
CH4       --------  ---  ------------------------------------------
CH4        LUEXH     I   Exposure History File(s)
CH4
CH4  Method:
CH4   As input it takes a time interval.
CH4   It then reads the Exposure History file to get the total live 
CH4   time spent in each  viewing mode.
CH4   The file is opened or closed only as necessary; multiple calls
CH4   to this routine may be needed to handle a single file.
CH4   There is much consistency checking on the file contents.
CH4
C    Notes:
C     Perhaps it would be better if this subroutine extracted the
C     S/C orientation from the exposure history file.  Now, this
C     information is taken from the timeline file.  The former,
C     being machine-generated, is possibly more reliable.

C  @(#) exhist.f 1.4@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine exhist(orntime,ornview,norns,tmode,
     >     cutangle,sra,sdec,nregns,iorn,data_dir,filebase)

      IMPLICIT NONE
      include '../SPECMAT_COMMON/spectral.inc'
      include '../SPECMAT_COMMON/lunits.inc'   ! for luexh

*     ARGUMENTS
      character*(*)  data_dir, filebase
      integer norns, nregns, iorn
      real tmode(modemax,0:jmax),cutangle(0:jmax),sra,sdec
      real*8 orntime(2,nornmax)
      character(4) ornview(nornmax)

*     PARAMETER
      integer secday, ind
      parameter (secday=86400)

*     LOCAL VARIABLES
      integer n,j,itjd,imsd,hexmode,mode,len_trim,coincmode
      logical flags(3),allowed,metlive,btest
      real*8 time,convrt,ti,tf,tlive,telaps,oldtime,oldtelaps
      real epos(4),tmap(0:jmax)
      character(100) filename
      character(130) line

      save

*     CALLS: CONVRT, MCONV

*----------------------------------------------------------------------*

*   CHECK INPUT FOR CONSISTENCY
      time = orntime(1,1)
      do n = 1,norns
         if (orntime(1,n).lt.time) go to 665
         if (orntime(2,n).lt.orntime(1,n)) go to 671
         time = orntime(2,n)
      end do

*   INITIALIZE MATRIX
*      do n = 1,norns
*         do k = 1,nmodes
*            do j = 0,jmax
*               tmode(k,n,j) = 0.0
*            end do
*         end do
*      end do


c      call getenv('EXHIST',filebase)
*      do n = 1,norns
*      READ EXPOSURE HISTORY FILES
	


         time = orntime(1,iorn)
	 if (iorn.eq.1.or.ornview(iorn).ne.ornview(iorn-1)) then
	   ind = index(data_dir, ' ') - 1
	   filename = data_dir(1:ind) // 
     *         filebase(1:len_trim(filebase))// '.' // ornview(iorn)
	   write (6,*) 'EXHIST::OPENING EXHIST FILE: ',filename
           open (unit=luexh,file=filename,status='old',err=666)
	   oldtime = 0.0
	 end if

*         READ INPUT LINES
 10      line(1:1) = '*'
         do while (line(1:1).eq.'*')
            read (luexh,'(a)',end=5,err=667) line
         end do
         read (line,60) itjd,imsd,flags,hexmode,coincmode,tlive,telaps,
     >        epos
 60      format (1x,i6,i9,1x,3l1,z5,z2,36x,f7.1,f10.3,4f8.4)
         time = convrt(itjd,imsd)
	 allowed = .not.flags(3)		! Interval not excluded
	 metlive = .not.btest(coincmode,0)	! MET enabled

*        CHECK FOR ERRORS
         if (oldtime.gt.0..and.time.lt.oldtime) go to 668
         if (telaps.lt.tlive) go to 669
         if (oldtime.gt.0..and.
     >      abs((time-oldtime)*secday-oldtelaps).gt.1.) go to 670
	 oldtelaps = telaps
         oldtime = time

*        ADD LIVE TIME CONTRIBUTIONS
         if (allowed.and.metlive) then	! Interval OK to use
            call mconv(hexmode,mode)
            if (mode.le.0.and.tlive.gt.0.)
     >           write (6,'(''Invalid viewing mode '',z4)') hexmode
            ti = max(orntime(1,iorn),time)
            tf = min(orntime(2,iorn),time+telaps/secday)

            if (mode.gt.0.and.telaps.gt.0..and.tf.gt.ti) then
               call shadow(real(tlive),real(telaps),epos,10.,cutangle,
     >              tmap,nregns,sra,sdec)
               do j = 0,jmax
                  tmode(mode,j) = tmode(mode,j)+
     >              secday*(tf-ti)*tmap(j)/telaps
               end do
            end if
         end if
	 if (time+telaps/secday.lt.orntime(2,iorn)) go to 10
 5       if (iorn.eq.norns.or.ornview(iorn+1).ne.ornview(iorn)) 
     >        close(luexh)
*      end do
      
      return

665   write (6,*) 'Orientation start times out of order:'
      write (6,*) iorn,time,orntime(1,iorn)
      STOP
671   write (6,*) 'Orientation start, end out of order:'
      write (6,*) iorn,orntime(1,iorn),orntime(2,iorn)
      STOP
666   write (6,*) 'Error while opening exposure file.'
      STOP
667   write (6,*) 'Read error in exposure history file.'
      STOP
668   write (6,*) 'Times in exposure file out of order:'
      write (6,*)'TJD: ',itjd,'   MSD:',imsd
      STOP
669   write (6,*) 'Live time too small:'
      write (6,*) 'Live: ',tlive,'   Elapsed: ',telaps
      STOP
670   write (6,*) 'Error in elapsed time.'
      write (6,*) 'time,oldtime,oldtelaps:',time,oldtime,oldtelaps
      STOP

      end
