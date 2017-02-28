C ***************************************************************************
C SELECTOR TASK
C      XTECOL
C
C FILE:
C      XTECOL.f
C
C DESCRIPTION:      
C     Apply the HEXTE collimator correction to a light curve
C      
C AUTHOR:
C     James Lochner  5/95
C     with Tod Strohmeyer (PCA) and Ian George (HEASARC)
C      
C MODIFICATION HISTORY:
C Oct 12, 1995 - change 'coord out of range' from an error to a warning
C     when collimator correction applied to each time stamp.
C Feb 29, 1996 - Allow user to specify either alternate start or stop
C     time, instead of acting only when start time specified
C Mar  5, 1996 - Write INDEF value instead of 0.0 when object outside
C     of field of view.  Applies only when collimator correction calculated
C     for each time stamp.
C Oct 17, 1996 - Change acsestq(1-4) into doubles instead of reals
C     This, with change in ck_quats, should solve the jitter=0.0 problem.      
C Jul 08, 1998 - "UNKNOWN" for detector name confuses gtcal(), use "-" instead
C 
C	7/6/98 - by ZG to change the string length of obs_date and obs_time
C		to 68 for y2k string format.
C Aug 18, 1999 - Replace gtcal with gtcalf (PDW)
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      lcfil	 - name of input XTE light curve file
C      outfil    - name of output light curve file
C      xtefilt   - name of XTE filter file
C      collcube  - name fo XTE collimator cube file
C      detnam       - desired detector
C      jitter    - acceptable pointing jitter
C      ra        - alternate source ra
C      dec       - alternate source dec
C      start     - alternate start time for observation
C      stop      - alternate stop time for observation
C      chatter   - how much to tell user
C
C CALLED ROUTINES:
C     subroutine gxtecol  - gets parameters from environment
c     subroutine doxtecol - constructs and outputs the .arf file
C ************************************************************************** 

      Subroutine XTECOL

c start with initial declarations
      character(160) lcfil, outfil, xtefilt
      character(160) collcube
      character(12) ra, dec
      character(10) det_str
      real jitter
      integer chatter
      double precision start, stop
      logical abort
        
      character(40) taskname
      common /task/ taskname
      character(40) rcsname

C First set up the software version strings  

      rcsname = ' $Revision: 1.8 $ '
      READ(rcsname, 5) taskname
    5 FORMAT(11X, A40)
      taskname = 'xtecol'// taskname(1:INDEX(taskname, '$')-1)

      abort = .false.
        
c get the parameters from the par file
        call gXTECOL(lcfil, outfil, xtefilt, collcube,
     $     det_str, jitter, ra, dec, start, stop, chatter)


c Perform the Algorithm:   
        call doXTECOL(lcfil, outfil, xtefilt, collcube,
     $       det_str, jitter, ra, dec, start, stop, chatter)


c  Exit subroutine

	return
	end
C*****************************************************************
C SUBROUTINE:
C      gXTECOL
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  5/95
C
C MODIFICATION HISTORY:
C     Oct 3, 1995 - Enhanced detnam specification so that either a single
C                  detector may be specified or "all" may be specified
C     
C NOTES:
C      gXTECOL uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C      call gXTECOL(phafil, rmffil, arffil, xtefilt, collcube, det_str,
C                   jitter, ra, dec, tstart, tstop, chatter)
C      
C ARGUMENTS:
C     lcfil	 - name of input XTE light curve file
C     outfil	 - name of output light curve file
C     xtefilt    - name of XTE filter file
C     collcube   - name of XTE collimator cube file
C     det_str    - desired detector
C     jitter     - acceptable pointing jitter
C     ra         - alternate source ra
C     dec        - alternate source dec
C     tstart     - alternate start time for observation
C     tstop      - alternate stop time for observation
C     chatter    - how much to tell user (for CALLIB routines)
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgsd - get real*8 parameter
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsr - get real*4 parameter
C      subroutine uclgst - get string parameter
C      
C *******************************************************************************

      SUBROUTINE gXTECOL(lcfile, outfile, xtefilt, collcube,
     $     det_str, jitter, ra, dec, tstart, tstop, chatter)


c start with the declarations
      character*(*) lcfile, outfile, xtefilt
      character*(*)  ra, dec, det_str
      character(160) collcube
      real jitter
      integer chatter
      double precision tstart, tstop
      
      character(80) context
      integer  status
      real rstart, rstop
      
      status = 0

c get the name of the input light curve file
	call uclgst('lcfile',lcfile,status)
	if (status .ne. 0) then
	    context = 'could not get LCFILE parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the name of the output light curve file
	call uclgst('outfile',outfile,status)
	if (status .ne. 0) then
	    context = 'could not get OUTFILE parameter'
	    call fcerr(context)
	    go to 999
	endif
        
c get the name of the XTE filter file
	call uclgst('xtefilt',xtefilt,status)
	if (status .ne. 0) then
	    context = 'could not get XTEFILT parameter'
	    call fcerr(context)
	    go to 999
	endif
        
c get the name of the collimator cube file
	call uclgst('collcube',collcube,status)
	if (status .ne. 0) then
	    context = 'could not get COLLCUBE parameter'
	    call fcerr(context)
	    go to 999
	endif
        
C get the desired detector (ALL should be acceptable input)
        call uclgst('detnam',det_str,status)
        if (status .ne. 0) then
           context = 'could not get DETNAM parameter'
           call fcerr(context)
	endif

C get the value of the acceptable pointing jitter (hidden)
        call uclgsr('jitter',jitter,status)
        if (status .ne. 0) then
           context = 'could not get JITTER parameter'
           call fcerr(context)
           go to 999
        endif
        
c get desired source position (hidden parameter)
c     (need routine to accept/decode either hh mm ss or decimal degrees)
        call uclgst('ra',ra,status)
	if (status .ne. 0) then
	    context = 'could not get Alternate RA parameter'
	    call fcerr(context)
	    go to 999
	endif
        call uclgst('dec',dec,status)
	if (status .ne. 0) then
	    context = 'could not get Alternate DEC parameter'
	    call fcerr(context)
	    go to 999
	endif

C get the value of the alternate start and stop times (hidden)
        call uclgsr('start',rstart,status)
        if (status .ne. 0) then
           context = 'could not get START parameter'
           call fcerr(context)
           go to 999
        endif
        call uclgsr('stop',rstop,status)
        if (status .ne. 0) then
           context = 'could not get STOP parameter'
           call fcerr(context)
           go to 999
        endif
        tstart = rstart
        tstop = rstop

C get the chattiness flag
        call uclgsi('chatter',chatter,status)
        if (status .ne. 0) then
           context = 'could not get CHATTER parameter'
           call fcerr(context)
           status = 0
           context = 'setting CHATTER = 10'
           call fcerr(context)
           chatter = 10
	endif

c Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end

C*****************************************************************
C SUBROUTINE:
C     doXTECOL
C
C DESCRIPTION:      
C      
C      
C AUTHOR:
C      James Lochner  5/95
C     with Tod Strohmayer (PCA) & Ian George (HEASARC)
C      
C MODIFICATION HISTORY:
C  Feb 27, 1996 - read input light curve looking for either RATE or COUNTS.
C                 chatter coll. cube file name to screen
C  Feb 29, 1996 - allow to specify either tstart or tstop parameter (in
C                 addition to both)
C  May 16, 1996 - add tzero to TSTART and TSTOP values read from lc header
C                  (This allows for change in v3.5.1 of extractor tools)
C  Aug 18, 1999 - Replace gtcal with gtcalf (PDW)
C
C NOTES:
C
C USEAGE:      
C      call doXTECOL(phafil, rmffil, arffil, xtefilt, path,
C              det_str, jitter, ra_str, dec_str, tstart, tstop, chatter)
C      
C ARGUMENTS:
C     lcfil	 - name of input XTE light curve file
C     outfil	 - name of output light curve file
C     xtefilt    - name of XTE filter file
C     collcube    - name of XTE collimator cube file
C     det_str        - desired detector
C     jitter     - acceptable pointing jitter
C     ra_str     - alternate source ra
C     dec_str    - alternate source dec
C     tstart     - alternate start time for observation
C     tstop      - alternate stop time for observation
C     chat       - how much to tell user 
C
C PRIMARY LOCAL VARIABLES (selected):
C     tstart_lc - default TSTART value in light curve file
C     tstop_lc  - default TSTOP value in light curve file
C     N.B. e = energy, c1 = coord. 1 (i.e. x), c2 = coord. 2 (i.e. y)
C     collcube    - file name extracted from input string collcube
C     x_lo,x_hi - limits on the spatial grid in coll. cube
C     y_lo,y_hi -      "                  "
C     cresp3d   - coll. cube in energy, x, and y position
C     tmpimg    - 2 x 2 subset of cresp3d
C     cresp1d   - 1-d coll. resp. on coll cube's energy grid
C     context   - error message
C     status    - error number
C
C CALLED ROUTINES:
C     subroutine onequat   - procedure for using an avg quaternion
c     subroutine manyquat1 - procedure for light curve bins < quaternion sampling
c     subroutine manyquat2 - procedure for light curve bins > quaternion sampling
C     subroutine fcerr     - echo message to terminal
C      subroutine fcerrm   - echo fitsio error message to terminal 
C      
C *******************************************************************************

      SUBROUTINE doXTECOL(lcfil, outfil, xtefilt, collcube,
     $     det_str, jitter, ra_str, dec_str, tstart,
     $     tstop, chat)


C start with the declarations
      character*(*) lcfil, outfil, xtefilt
      character*(*) ra_str, dec_str
      character*(*) collcube, det_str
      real jitter
      integer chat
      double precision tstart, tstop

      character(80) context, message
      character(80) comment(10)
C      character(16) obs_date, obs_time
	character(68) obs_date, obs_time
      character(8) telescop, instrume, detnam, filter
      character(8) runit, timeunit
      character(8) hduclas(9), hduvers(9,2), crspvers
      character(6) online(1)
      character(40) taskname
      COMMON /task/ taskname
      
      integer i, nquats
      integer  iunit1, iunit2, equinox
      integer frow, felem, nrows
      integer nsearch, block, ftstatus, htype, ierr
      integer extno(1), nfound, nret
      
      integer maxpts
      parameter (maxpts = 500000)
      integer collresp, rate, collrate, error, collerror
      integer qtime
      real quats(250000,4), coor(3), pi
      double precision ra, dec, time(maxpts), mjdref
      double precision mintime, maxtime, timedel, tzero, delquat
      double precision tstart_lc, tstop_lc
      
      logical abort, inopen, timecol, quiet
      
c %%% Start of IMG Addition -------------
        integer maxextn, maxdim, maxe, maxc1, maxc2, maxe2
        parameter (maxextn=10, maxdim=3, maxe=10, maxe2=256, 
     &		maxc1=30, maxc2=30)
	integer status, ninstr, icol
        character(20) instr(9), ftype
	character(25) wrnstr, errstr2
	character(160) colfil(1)
        integer next(maxextn), ndim
        character(20) outhdu(9,maxextn), outver(9,maxextn)
        character(20) extnam(maxextn)
	character(20) coordtyp(maxdim), coordnam(2,maxdim)
	integer coordnpts(maxdim), coordcol(2,maxdim)
	logical qok
	real x_lo, x_hi, y_lo, y_hi
	integer edim, c1dim, c2dim
	integer incol(2)
	character(20) x_units, y_units

c %%% End of IMG Addition -------------

      
C******************************************************************************
C   the following MEM common block definition is in the system iraf77.inc file
C   and is used for dynamic memory allocation 
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
      
C     note:
C      datatype      value
C      logical        1
C      integer*2      3
C      Integer        4
C      Long Integer   5
C      Real           6
C      Double         7
C      Complex        8
C******************************************************************************      

c %%% Start of IMG Addition -------------
c Initalization
	wrnstr = '** DOXTECOL WARNING:'
	errstr2 = '** DOXTECOL ERROR:'
c %%% End of IMG Addition -------------

      ierr = 0
      ftstatus = 0
      equinox = 2000
      pi = 3.14159
      frow = 1
      felem = 1

c shut up compiler warnings
      c1dim=0
      c2dim=0

C Allocate the Dynamic Memory

      rate = 0
      collrate = 0
      error = 0
      collerror = 0
      qtime = 0
      collresp = 0

C allocate the dynamic memory for input intensity
      call udmget (maxpts, 6, rate, status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for input rate'
         call fcerr (context)
         goto 998
      endif

C allocate the dynamic memory for output intensity
      call udmget (maxpts, 6, collrate, status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for output rate'
         call fcerr (context)
         goto 998
      endif

C allocate the dynamic memory for input errors
      call udmget (maxpts, 6, error, status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for input errors'
         call fcerr (context)
         goto 998
      endif
      
C allocate the dynamic memory for output errors
      call udmget (maxpts, 6, collerror, status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for output errors'
         call fcerr (context)
         goto 998
      endif
      
C allocate the dynamic memory for quaternion times
      call udmget (maxpts, 7, qtime, status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for quatrn times'
         call fcerr (context)
         goto 998
      endif
      
C allocate the dynamic memory for output collimator correction
      call udmget (maxpts, 6, collresp, status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for collimator'
         call fcerr (context)
         goto 998
      endif

C     Parse the detector name
      
      detnam = det_str
C Open the light curve file, getting certain header info
      call ck_xtelc(chat, lcfil, iunit1, telescop, instrume, detnam,
     $     filter, obs_date, obs_time, ierr)
      if (ierr .ne. 0) then
         context = errstr2//'Could not get light curve info'
         go to 999
      endif
      call ftclos(iunit1,ftstatus)
      
C Read the light curve file 
      call readlc2(iunit1,lcfil,inopen,block,nrows,mintime,maxtime,
     $     'TIME','RATE','ERROR',timecol,time,MEMR(rate),MEMR(error),
     $     timedel,runit,timeunit,tzero,mjdref,maxpts,abort)
      if (abort) then
         context = ' ... trying again ...'
         call fcecho(context)
         call readlc2(iunit1,lcfil,inopen,block,nrows,mintime,
     $    maxtime,'TIME','COUNTS','ERROR',timecol,time,MEMR(rate),
     $    MEMR(error),timedel,runit,timeunit,tzero,mjdref,maxpts,abort)
         if (abort)  then
            ierr = 1
            context = 'Unable to find RATE or COUNTS in input file'
            go to 999
         else
            context = '... success, continuing ...'
            call fcecho(context)     
         endif
      endif
            
C Get TSTART and TSTOP times from keywords, or override with inputs
      call startnstop(iunit1, lcfil, tstart_lc, tstop_lc, mjdref,
     $     ierr)
      if (tstart .eq. -1.0) tstart = tstart_lc + tzero
      if (tstop .eq. -1.0) tstop = tstop_lc + tzero

C reckon light curve times using tzero
      do i = 1,nrows
         time(i) = time(i) + tzero
      end do
      
C From the lc file, obtain the source position [and the active dets]
      if (ra_str .eq. 'INDEF') then
         call ftgkyd(iunit1,'RA_OBJ',ra,comment,ftstatus)         
         if (ftstatus .eq. 202) then
            ftstatus = 0
            call ftgkyd(iunit1,'RA ',ra,comment,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to obtain object RA on 2nd attempt'
               call fcerr(context)
               goto 999
            endif
         else if (ftstatus .ne. 0) then
            context = 'unable to obtain object RA'
            call fcerr(context)
            goto 999
         endif
      else
         call parsera(ra_str,equinox,ra,ierr)
      endif
      
      if (dec_str .eq. 'INDEF') then
         call ftgkyd(iunit1,'DEC_OBJ',dec,comment,ftstatus)
         if (ftstatus .eq. 202) then
            ftstatus = 0
            call ftgkyd(iunit1,'DEC',dec,comment,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to obtain object DEC'
               call fcerr(context)
               goto 999
            endif
         else if (ftstatus .ne. 0) then
            context = 'unable to obtain object DEC'
            call fcerr(context)
            goto 999
         endif
      else
         call parsedec(dec_str,equinox,dec,ierr)
      endif
      
C Convert source RA & Dec into unit vector in inertial coord.
      coor(1) = cos(pi*dec/180.) * cos(pi*ra/180.)
      coor(2) = cos(pi*dec/180.) * sin(pi*ra/180.)
      coor(3) = sin(pi*dec/180.)


c ----------------- Start of COLLIMATOR RESPONSE stuff -----------------
c     Open the COLLIMATOR RESPONSE file and perform initial checks
c ... Check whether i/p string contains special chatacters denoting 
c     COLLIMATOR RESPONSE filename to be determined from the local CALDB, 
c     and if so call caldb access software 

      if (collcube(1:5) .eq. 'CALDB' .or. collcube .eq. 'caldb') then
C
         filter = '-'
         quiet = .false.
c     08 July 1998: (MJT) gtcal doesn't know anything about "UNKNOWN"
c     so let's change it to "-" which will cause gtcal to ignore it
         if (detnam .eq. 'UNKNOWN') then
            detnam='-'
         endif
         call gtcalf(chat, telescop, instrume,detnam,filter,'COLLRESP',
     $        obs_date, obs_time, obs_date, obs_time, '-', 1, colfil,
     $        extno, online, nret, nfound, ierr)
         if(ierr.NE.0)then 
            context = errstr2//'unable to get collimator cube'//
     $           ' from caldb'
            go to 999
         endif

c... Otherwise, Translate the name of the input file (taking off any 
c     extension specified by the user).
         
      else

         call fcpars(collcube,colfil(1),extno(1),status)
         if(status.NE.0) then
            message = wrnstr // 
     &           ' Problem passing collcube expression'
            call fcecho(message)
            message = ' ... will search all extensions'
            call fcecho(message)
            extno(1) = -99
         endif
      endif

C Chatter the name of the collimator cube to the screen
      if (chat .ge. 15) then
         message = 'collimator cube = '//colfil(1)
         call fcecho(message)
      endif      
      
c ... Now
c 	colfil(1) - name of file to be opened 
c	extno(1)  - extension no. of dataset (or -99 indicating search reqd)

c Open i/p file
        status = 0
      	call cgetlun(iunit2)
      	call ftopen(iunit2,colfil(1),0,block,status)
      	IF (status.NE.0) THEN
      		message = errstr2//' opening file: '//colfil(1)
      		call wt_ferrmsg(status,message)
		ierr = 1        
		goto 999
      	ENDIF


c GO FIND THE EXTENSION
	if(extno(1).GE.0) then
c  	   - Extension number IS already known
	   call ftmahd(iunit2,extno(1)+1,htype,status)
	   message = wrnstr // ' Problem moving to specified xtens'
           call wt_ferrmsg(status, message)
c 	   ... grab the HDUCLAS values for reference
	   ninstr = 1
	   instr(1) = '*'
	   nsearch = 1
           call fndhdu(chat, iunit2, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
	   nfound = 1
	   next(1) = 0
	   ftype = outhdu(2,1)
	else
c  	   - Extension SEARCHING required 
c 	   .... Search for COLLRESP 
	   nsearch = maxextn
	   call fndcol(chat, iunit2, nsearch,
     &          crspvers, hduclas, hduvers,
     &          ierr)
	   if(ierr.EQ.0) then
	    ftype = hduclas(2)
	   else 
	    message = errstr2//' Unable to locate acceptable extn'
	    call fcecho(message)
	    goto 999
	   endif
	endif

c ... Now
c	Assuming everything's OK, we should be at the desired extension

c Find the column number of the coll.cube dataset
c ... (a good check that we're at the right place in the file)
	call ftgcno(iunit2, .false., 'COLLRESP', icol, status)
      	IF (status.NE.0) THEN
      		message = errstr2//' Locating COLLRESP column'
      		call wt_ferrmsg(status,message)
		ierr = 1        
		goto 999
      	ENDIF

c Get all the info regarding the storage of the coll.cube dataset
	call gcrefs (iunit2, chat, icol, maxdim, ndim,
     &		coordtyp, coordnam, coordcol, coordnpts, ierr)
      	IF(ierr.NE.0) then
		goto 999
	endif

c ... Now
c	Assuming everything's OK, we know a whole bunch of stuff regarding 
c       the column number & ordering of the COLLRESP dataset, plus 
c       the column number, number of elements etc of the corresponding 
c	energy & spatial "grids"

c Cycle through the dimensions found, assigning pointers to the relevant dims
	do i = 1, ndim
	   if(coordtyp(i).EQ.'ENERGY') then
		edim = i
	   elseif(coordtyp(i).EQ.'COORD-1') then
		c1dim = i
	   elseif(coordtyp(i).EQ.'COORD-2') then
		c2dim = i
	   endif
	enddo

c Go and get the limits on the spatial coord grids (for later checks)
c ... First coordinate
	incol(1) = coordcol(1,c1dim)
	incol(2) = coordcol(2,c1dim)
      	call gcsylt(iunit2, chat, incol, x_lo, x_hi, x_units, ierr)
      	IF(ierr.NE.0) then
		goto 999
	endif
c ... Second coordinate
	incol(1) = coordcol(1,c2dim)
	incol(2) = coordcol(2,c2dim)
      	call gcsylt(iunit2, chat, incol, y_lo, y_hi, y_units, ierr)
      	IF(ierr.NE.0) then
		goto 999
	endif

c -------------------------------------------------------------------
c -------- Start Checking & Preparation of Spatial Grid -------------

c ... Perform checks
c     (For now, simple/crude checks & similar error messages; later messages
c      should be improved, and perhaps coordinate transformations
c      might even be performed here if necessary instead of erroring out)
	qok = .true.
	if(coordnam(1,c1dim).NE.coordnam(2,c1dim)) then
	   qok = .false.
	   message = errstr2//' Coord-1 across 2 columns'
	endif
	if(coordnam(1,c2dim).NE.coordnam(2,c2dim)) then
	   qok = .false.
	   message = errstr2//' Coord-2 across 2 columns'
	endif
	if((coordnam(1,c1dim).NE.'ALPHA').OR.
     &		coordnam(1,c2dim).NE.'BETA') then
	   qok = .false.
	   message = errstr2//' Mixed coord system'
	endif
c -------------   END of Opening of COLLIMATOR RESPONSE
      
C Open the xtefilter file and get the pointing info
c     nquats = number of quaternions returned
c        1 = measured jitter is acceptable - need only use "avg" quat
c     qtime is array of times at which quaternions given
c     quats is array of the quaternions, dimmed (nquats,4)
        nquats = maxpts
        call rd_quatrns2(tstart,tstop,mjdref,xtefilt, jitter,
     $          MEMD(qtime), quats, nquats, delquat, chat, ierr)
        if (ierr .ne. 0) go to 999

        
C  Now choose means for correcting intensity values
        if (nquats .eq. 1) then
           call onequat(iunit2,coor,quats,x_lo,x_hi,x_units,
     $          y_lo,y_hi,y_units,nrows,tstart, tstop, time,
     $          MEMR(rate),MEMR(collrate),MEMR(error),
     $          MEMR(collerror),MEMR(collresp),chat,ierr)
        else if (timedel .le. delquat) then
           call manyquat1(iunit2, coor, x_lo, x_hi, x_units, y_lo, y_hi,
     $          y_units, nquats, MEMD(qtime), quats, nrows, tstart,
     $          tstop, time, MEMR(rate), MEMR(error),MEMR(collrate),
     $          MEMR(collerror), MEMR(collresp), chat, ierr)
        else
           call manyquat2(iunit2, coor, x_lo, x_hi, x_units, y_lo, y_hi,
     $          y_units, nquats, MEMD(qtime), quats, nrows, timedel,
     $          tstart, tstop, time, MEMR(rate), MEMR(error),
     $          MEMR(collrate), MEMR(collerror), MEMR(collresp), chat,
     $          ierr)
        endif
        if (ierr .ne. 0) then
           ierr = 0
           go to 999
        endif
        
        
      
C Create the Output light curve
C     Output a null primary header
C     replace the rate values with the coll_rate values,
c     put collresp value(s) in header if nquats = 1, or otherwise in a column

        call wt_clrsplc(iunit1,lcfil,inopen,colfil(1),outfil,nrows,
     $       MEMR(collrate),MEMR(collerror),nquats,MEMR(collresp),abort)
        
876     continue

        
C Exit subroutine
C report possible alternatives when memory allocation fails
      goto 999
998   context = '  When memory allocation falls you can:'
      call fcecho(context)
      context = '  1) try again when system load is lighter'
      call fcecho(context)
      context = '  2) try again with smaller sigma and/or nsigma'
      call fcecho(context)
      context = '  3) try again with smaller image'
      call fcecho(context)

C free the dynamic memory
      call udmfre (rate, 6, status)
      call udmfre (collrate, 6, status)
      call udmfre (error, 6, status)
      call udmfre (collerror, 6, status)
      call udmfre (collresp, 6, status)
      call udmfre (qtime, 7, status)

999     continue 
        if (ierr .ne. 0) call fcecho(context)
        if (ftstatus .ne. 0) call fcerrm(ftstatus) 
	return
	end

