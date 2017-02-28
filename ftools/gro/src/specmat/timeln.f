CCCCCCCCCCCCCCCCCCCCCCCCCCC TIMELN.SOURCE(TIMELN) CCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  TIMELN
CH1
CH1  Version: 1.00                  Date: 3 August 1990
CH1
CH1  Programmer and Completion Date:
CH1     Mark Fardal - Stanford University - 3 August
CH1     Patrick Nolan - converted to SunOS - January 1991
CH1	P.N. - complete rewrite to handle real files - July 1991
CH1      - move code to remove 0 length intervals here from main  Dec 94
CH1      - handle CALTBLxx keywords.  Dec 94
CH1
CH1  Function:  Given a list of time intervals, read the Timeline
CH1      History file(s) to turn the original list into a possibly 
CH1      longer list of time intervals in which the orientation of 
CH1      the instrument was constant in each one.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1			     Sun Sparc - Sun Fortran 1.3.1
CH1
CH2  Calling Sequence:  CALL TIMELN(INTTIME, NINTS, ORNTIME, ORNVIEW,
CH2      ORNTASC, ORN, CALSET, NORNS)
CH2  Argument            Type    I/O                 Description
CH2  --------            ----    ---  ----------------------------------------
CH2  INTTIME(2,NINTS+1)  Real*8   I   Start, end times of selected intervals
CH2  NINTS               Integer  I   Number of time intervals
CH2  ORNTIME(2,NORNMAX)  Real*8   O   Start,end of constant-orientation ints
CH2  ORNVIEW(NORNMAX)    Char*4   O   Viewing period of orientation ints
CH2  ORNTASC(NORNMAX)    Logical  O   TASC in coincidence mode?
CH2  ORN(2,2,NORNMAX)    Real     O   Orientations: ra/dec of z and x axes
CH2  CALSET(NORNMAX)     Char*2   O   Which calibration data set
CH2  NORNS               Integer  O   Number of constant-orientations ints
CH2
CH2  Calls:
CH2   TCONV: Converts string date and time to real*8 number
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable       Type   Ini. Val.          Description
CH3  --------       ----   ---------  --------------------------------------
CH3  LINE         Char*70      -      Input line from file
CH3  CHDATE       Char*8       -      Time in mm/dd/yy format
CH3  CHTIME       Char*12      -      Time in hh/mm/ss.fff format
CH3  TIME         Real*8       -      Time of Timeline file events
CH3  KEYWD        Char*8       -      Reason for Timeline line write
CH3  ACTIVE       Logical      -      Data is usable
CH3  WORKING      Logical      -      An 'orientation' has been started
CH3
CH4  Logical Units Used:
CH4      Variable  I/O              Description
CH4      --------  ---  ------------------------------------------
CH4       LUTML     I   Timeline History File(s)
CH4
CH4  Method:
CH4   This subroutine is part of the program MATRIX; it reads the 
CH4   Timeline History file(s).  Given a list of time intervals, it comes
CH4   up with another, possibly longer, list in which the orientation
CH4   of the instrument is constant over each interval.  Orientation
CH4   intervals will not overlap multiple viewing periods.  The orientations
CH4   are also recorded.  The format of the input files is assumed to
CH4   follow the specification in EGRET TELEMETRY AND DATA FORMATS,
CH4   March 1990.  In VMS Fortran,
CH4   TIMELN must be compiled with d_floating (precise) option.
CH4   Note the distinction between "orientation" and
CH4   "aspect"; a change in orientation denotes a very large change in the 
CH4   instrument direction, while aspect changes can be small deviations.
CH4   All orientation are aspect changes as well.
CH4
C  @(#) timeln.f 1.3@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      subroutine timeln(inttime,nints,orntime,ornview,orntasc,orn,
     >      calset,norns,misc_dir)

      IMPLICIT NONE

      include '../SPECMAT_COMMON/spectral.inc'
      include '../SPECMAT_COMMON/lunits.inc'  ! for lutml

*     ARGUMENTS
      character*(*)  misc_dir
      integer nints,norns
      real orn(2,2,nornmax)    ! orientations: ra/dec of z and x axes
      logical orntasc(nornmax)
      real*8 inttime(2,nints+1), ! start, end time of selected intervals
     >       orntime(2,nornmax)  ! start, end time of orientation ints
      character(4) ornview(nornmax)
      character(2) calset(nornmax)

*     LOCAL VARIABLES
      logical active,oldactive,oldtascin
      logical noacc,working,tascin
      integer n,i,j,k,l
      real xra,xdec,zra,zdec
      real*8 time,oldtime
      character(80) tlfile
      character(70) line
      character(12) chtime  !	Time in hh/mm/ss.fff format
      character(8) chdate
      character(5) keywd   
      character(4) observ,obs
      character(2) caltabl,caltabl1

*     STRUCTURE OF INPUT LINE
*     equivalence (line(2:5),observ)
*     equivalence (line(8:15),chdate)
*     equivalence (line(18:29),chtime)
*     equivalence (line(32:36),keywd)
*     equivalence (line(38:39),caltabl1)

      save

*     CALLS: TCONV

*------------------------------------------------------------------*


csb-02/05      call getenv('TIMELINE',tlfile)	! Name of timeline file
Csb   Read misc_dir:timeline
      i = index(misc_dir, ' ') - 1
      tlfile = misc_dir(1:i) // '/timeline'

*     CHECK INTERVALS FOR CONSISTENCY
      time = inttime(1,1)
      do n = 1,nints
         if (inttime(1,n).lt.time) go to 666
         if (inttime(2,n).lt.inttime(1,n)) go to 670
         time = inttime(2,n)
C         write (6,*) n,real(inttime(1,n)),real(inttime(2,n))
      end do

      norns = 0

C Loop over all observation intervals.  Open and read timeline file
C again for each one.  They should not overlap.
      do n = 1,nints
	obs = '    '
        time = -1.0
        active = .false.
	noacc = .true.
	working = .false.
	tascin = .true.
        caltabl = '00'

        open (unit=lutml,file=tlfile,status='old')
10	read (lutml,'(a)',err=668,end=900) line	! Read a line from file
        observ=line(2:5)
        chdate=line(8:15)
        chtime=line(18:29)
        keywd=line(32:36)
        caltabl1=line(38:39)
	if (line(1:1).eq.'*') go to 10	! Skip comments
	if (keywd.eq.'     '.and.chdate.eq.'        '.and.
     >    chtime.eq.'            ') go to 10	! Skip blank lines
	oldtime = time
	oldtascin = tascin
	oldactive = active
	call tconv(chdate,chtime,time)	! Convert time format
	if (oldtime.gt.0..and.time.lt.oldtime) go to 667
	if (observ.ne.'    ') obs=observ
	do i = 1,4
	  if (obs(i:i).eq.' ') obs(i:i) = '0'
	end do

C Check for all possible keywords
	call sp_upcase(keywd,5)
	if (keywd.eq.'     ') then
	   continue
	else if (keywd.eq.'TASCI') then
	   tascin = .true.
	else if (keywd.eq.'TASCO') then
	   tascin = .false.
	else if (keywd.eq.'EXCLU') then
           continue
	else if (keywd.eq.'END E') then
           continue
	else if (keywd.eq.'ALBED') then 
           continue
	else if (keywd.eq.'END A') then
           continue
	else if (keywd.eq.'TEST ') then
           continue
	else if (keywd.eq.'END T') then
           continue
	else if (keywd.eq.'CALIB') then
           continue
	else if (keywd.eq.'END C') then
           continue
        else if (keywd.eq.'CALTB') then
           caltabl = caltabl1
C  Axis change
	else if (keywd.eq.'SC-X-') then
           read (line(40:70),*) xra,xdec
	else if (keywd.eq.'SC-Z-') then
           read (line(40:70),*) zra,zdec
C  Start or end of observation
	else if (keywd.eq.'START') then
           noacc = .false.
	else if (keywd.eq.'END V') then
           noacc = .true.
	else if (keywd.eq.'STOP ') then
           noacc = .true.
	else
           go to 669
	end if

C  'active' means data is acceptable
	active = .not.noacc

C Decide if this event begins or ends an 'orientation'

C   See if the first orientation started before this event
	if (oldactive.and.oldtime.le.inttime(1,n).and.
     >      time.gt.inttime(1,n)) then
	  working = .true.
	  norns = norns + 1
          if (norns.gt.nornmax) go to 720
	  orntime(1,norns) = inttime(1,n)
	  ornview(norns) = obs
	  orntasc(norns) = oldtascin
	  orn(1,1,norns) = zra
	  orn(2,1,norns) = zdec
	  orn(1,2,norns) = xra
	  orn(2,2,norns) = xdec
          calset(norns) = caltabl
	end if

C   All timeline events will end an orientation
	if (working) then
	  working = .false.
	  orntime(2,norns) = min(time,inttime(2,n))
	  if (norns.gt.0.and.orntime(2,norns).le.orntime(1,norns))
     >      norns = norns - 1	! remove runts
	end if

C   This event starts an orientation
	if (active.and.time.ge.inttime(1,n).and.
     >      time.lt.inttime(2,n)) then
	  working = .true.
	  norns = norns + 1
          if (norns.gt.nornmax) go to 720
	  orntime(1,norns) = time
	  ornview(norns) = obs
	  orntasc(norns) = tascin
	  orn(1,1,norns) = zra
	  orn(2,1,norns) = zdec
	  orn(1,2,norns) = xra
	  orn(2,2,norns) = xdec
          calset(norns) = caltabl
	end if

	go to 10

C  Clean up at end of file
900     close (unit=lutml)
	if (working) orntime(2,norns) = min(time,inttime(2,n))
	if (norns.gt.0.and.orntime(2,norns).le.orntime(1,norns))
     >      norns = norns - 1	! Remove runts

      end do

* remove any intervals of length zero.  shift down.
      i = 1
      do while (i.le.norns)
         if (orntime(2,i).le.orntime(1,i)) then
            write (*,*) 'Eliminating orientation #',i,': Length zero.'
            do j = i+1,norns
               ornview(j-1) = ornview(j)
	       orntasc(j-1) = orntasc(j)
               calset(j-1)  = calset(j)
               do k = 1,2
                  orntime(k,j-1) = orntime(k,j)
                  do l = 1,2
                     orn(k,l,j-1) = orn(k,l,j)
                  end do
               end do
            end do
            norns = norns - 1
         else
            i = i + 1
         end if
      end do

      return

C Fatal errors
666   write (6,*) 'Inconsistency in time intervals.'
      write (6,*) 'Start time(',n,')',inttime(1,n),' less than ',time
      STOP
670   write (6,*) 'Inconsistency in time intervals.'
      write (6,*) 'Stop time ',inttime(2,n),' before start time ',
     >  inttime(1,n)
      STOP 
667   write (6,*) 'Times in timeline files are out of order.'
      write (6,*) line
      STOP
668   write (6,*) 'Error while reading timeline file.'
      STOP
669   write (6,*) 'Unrecognized keyword in timeline file:',keywd
      write (6,*) line
      STOP
 720  write (6,*) norns,' orientations exceeds NORNMAX=',nornmax
      STOP

      end


C Convert a string to upper case.  There may be mixed case text
C in the timeline file.  This works only for ASCII.
      subroutine sp_upcase(str,len)
      character*(*) str
      integer len,i,j,ichar

      do i = 1,len
	j = ichar(str(i:i))
	if (j.ge.97.and.j.le.122) str(i:i) = char(j-32)
      end do
      return
      end
