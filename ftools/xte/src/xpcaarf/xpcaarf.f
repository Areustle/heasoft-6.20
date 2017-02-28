C ***************************************************************************
C SELECTOR TASK
C      xpcaarf
C
C FILE:
C      xpcaarf.f
C
C DESCRIPTION:      
C     create a .arf file for a given PCA observation
C      
C AUTHOR:
C      James Lochner  5/95
C     with Tod Strohmeyer (PCA) and Ian George (HEASARC)
C      
C MODIFICATION HISTORY:
C     Dec. 1, 1995 - Convert jitter parameter to minutes of arc rather
C              than quaternion units
C     Feb 12, 1996 - Use seconds of arc for jitter parameter     
C     Dec 16, 1996 - Bug fix in doxpcaarf for reading times from GTI
C
C	7/6/98 - by ZG to change the string length of obs_date and obs_time
C		to 68 for new y2k string format.
C     Aug 18, 1999 - Replace gtcal with gtcalf (PDW)
C     Mar 23, 2011 - increment version number concurrent with parameter file
C                    changes to PCU geometric areas. (MJT)
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      phafil	 - name of input PCA .pha file
C      rmffil	 - name of input PCA .rmf file
C      arffil    - name of output .arf file
C      xtefilt   - name of XTE filter file
C      collcube  - name fo PCA collimator cube file
C      pcu       - desired pcu (0 for sum)
C      geomarea  - geometric area of each pcu
C      jitter    - acceptable jitter in spacecraft pointing
C      ra        - alternate source ra
C      dec       - alternate source dec
C      start     - alternate start time for observation
C      stop      - alternate stop time for observation
C      arfversn  - version number of the ARF file to be written
C      chatter   - how much to tell user
C
C CALLED ROUTINES:
C     subroutine gxpcaarf  - gets parameters from environment
c     subroutine doxcpaarf - constructs and outputs the .arf file
C ************************************************************************** 

      Subroutine XPCAAF

c start with initial declarations
      character(160) phafil, rmffil, arffil, xtefilt
      character(160) collcube
      character(5) arfversn
      character(12) ra, dec
      real geomarea(5), jitter
      double precision start, stop
      integer pcu, chatter
      
      logical abort
        
      character(40) taskname
      common /task/ taskname

      taskname = 'xpcaarf 3.9'
      abort = .false.
        
c get the parameters from the par file
        call gxpcaarf(phafil, rmffil, arffil, xtefilt, collcube,
     $     pcu, geomarea, jitter, ra, dec, start, stop, arfversn,
     $     chatter)


c Perform the Algorithm:   
        call doxpcaarf(phafil, rmffil, arffil, xtefilt, collcube,
     $     pcu, geomarea, jitter, ra, dec, start, stop, arfversn,
     $     chatter)


c  Exit subroutine

	return
	end
C*****************************************************************
C SUBROUTINE:
C      gxpcaarf
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  5/95
C
C MODIFICATION HISTORY:
C      
C NOTES:
C      gxpcaarf uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C      call gxpcaarf(phafil, rmffil, arffil, xtefilt, collcube, pcu,
C                     geomarea, jitter, ra, dec, start, stop, arfversn,
C                     chatter)
C      
C ARGUMENTS:
C     phafil	 - name of input PCA pha file
C     rmffil     - name of input PCA rmf file
C     arffil	 - name of output .arf file
C     xtefilt    - name of XTE filter file
C     collcube   - name of PCA collimator cube file
C     pcu        - desired pcu (0 for sum)
C     geomarea   - geometric area of each pcu
C     jitter     - acceptable jitter in the spacecraft pointing
C     ra         - alternate source ra
C     dec        - alternate source dec
c     start      - alternate start time for observation
c     stop       - alternate stop time for observation
C     chatter    - how much to tell user (for CALLIB routines)
C     arfversn   - version number of the ARF file to be written
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

      SUBROUTINE gxpcaarf(phafil, rmffil, arffil, xtefilt, collcube,
     $     pcu, geomarea, jitter, ra, dec, start, stop, arfversn,
     $     chatter)

c start with the declarations
      character*(*) phafil, rmffil, arffil, xtefilt
      character*(*) collcube, arfversn, ra, dec
      real geomarea(0:4), jitter
      double precision start, stop
      integer pcu, chatter

      
      character(80) context
      character(9) area_pcu(0:4)
      integer  status
      real rstart, rstop
      
      status = 0

      area_pcu(0) = 'area_pcu0'
      area_pcu(1) = 'area_pcu1'
      area_pcu(2) = 'area_pcu2'
      area_pcu(3) = 'area_pcu3'
      area_pcu(4) = 'area_pcu4'

c get the name of the input pha file
	call uclgst('phafil',phafil,status)
	if (status .ne. 0) then
	    context = 'could not get PHAFIL parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the name of the input rmf file
	call uclgst('rmffil',rmffil,status)
	if (status .ne. 0) then
	    context = 'could not get RMFFIL parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the name of the output arf file
	call uclgst('arffil',arffil,status)
	if (status .ne. 0) then
	    context = 'could not get ARFFIL parameter'
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
c        if ((collcube(1:5).eq.'CALDB'.or.collcube(1:5).eq.'caldb')) then
c     call gtcal(
        
C get the desired pcu
        call uclgsi('pcu',pcu,status)
        if (status .ne. 0) then
           context = 'could not get PCU parameter'
           call fcerr(context)
	endif

C get the value of the geometric area of the desired pcu (hidden)
        call uclgsr(area_pcu(pcu),geomarea(pcu),status)
        if (status .ne. 0) then
           context = 'could not get GEOM AREA parameter'
           call fcerr(context)
           go to 999
        endif

C get the value of the acceptable s/c pointing jitter (hidden)
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
        start = rstart
        stop = rstop

c get the arfversn to be used
        call uclgst('arfversn',arfversn,status)
	if (status .ne. 0) then
	    context = 'could not get ARFVERSN parameter'
	    call fcerr(context)
	    go to 999
	endif

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

c     get the min & max times for intervals to be analyzed
C        call uclgst('intervals',intervals,status)
C        if (status .ne. 0) then
C           context = 'could not get INTERVALS parameter'
C           call fcerr(context)
C           go to 999
C        endif


c Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end
C*****************************************************************
C SUBROUTINE:
C     doxpcaarf
C
C DESCRIPTION:      
C      
C      
C AUTHOR:
C      James Lochner  5/95
C     with Tod Strohmayer (PCA) & Ian George (HEASARC)
C      
C MODIFICATION HISTORY:
C     Dec. 1, 1995 - Use .pha GTI times rather than TSTART/TSTOP for
C                  deciding times in the quaternion file
C     Mar. 8, 1996 - allow value of 'none' for name of xtefilter/quaternion
C                  file.  Source position in s/c coord then defaults to
C                  (0.0,0.0).  Also incresed maxe2 from 256 to 2048.
C     Mar. 22, 1996 - Read boresite values from parameter file.  (Use 0.0
C                  if not found in parameter file)
C     Dec. 16, 1996 - Bug fix for reading times from GTI extension
C     Aug. 18, 1999 - Replace gtcal with gtcalf (PDW)
C
C NOTES:
C
C USEAGE:      
C      CALL doxpcaarf(phafil, rmffil, arffil, xtefilt, collcube,
C     $     pcu, geomarea, jitter, ra_str, dec_str, tstart_pha,
C     $     tstop_pha, arfversn, chat)
C      
C ARGUMENTS:
C     phafil	 - name of input PCA pha file
C     rmffil     - name of input PCA rmf file
C     arffil	 - name of output .arf file
C     xtefilt    - name of XTE filter file
C     collcube   - name of PCA collimator cube file
C     pcu        - desired pcu 
C     geomarea   - geometric area of each pcu
C     jitter     - acceptable jitter in the spacecraft pointing
C     ra_str     - alternate source ra
C     dec_str    - alternate source dec
C     start_pha  - alternate start time for observation
C     stop_pha   - alternate stop time for observation
C     chat       - how much to tell user (for CALLIB routines)
C     arfversn   - version number of the ARF file to be written
C
C     PRIMARY LOCAL VARIABLES (selected):
C     N.B. e = energy, c1 = coord. 1 (i.e. x), c2 = coord. 2 (i.e. y)
C     quat      - average quaternions for the observation
C     colfil    - file name extracted from input string collcube
C     x_lo,x_hi - limits on the spatial grid in coll. cube
C     y_lo,y_hi -      "                  "
C     c1lo, c1hi, c2lo, c2hi
C               - subcube to extract from the coll. cube
C     cresp3d   - coll. cube in energy, x, and y position
C     tmpimg    - 2 x 2 subset of cresp3d
C     cresp1d   - 1-d coll. resp. on coll cube's energy grid
C     cresp_out - 1-d coll. resp. on rmf's energy grid
C     context   - error message
C     status    - error number
C
C CALLED ROUTINES:
C     subroutine fcerr  - echo message to terminal
C     subroutine fcerrm - echo fitsio error message to terminal 
c     subroutine gcsynm - callib
C     subroutine gcrefs - callib
c     subroutine gcsylt - callib
c     subroutine xtcol1 - callib
c     subroutine rmap1d - callib
C *******************************************************************************

      SUBROUTINE doxpcaarf(phafil, rmffil, arffil, xtefilt, collcube,
     $     pcu, geomarea, jitter, ra_str, dec_str, tstart_pha,
     $     tstop_pha, arfversn, chat)


C start with the declarations
      character*(*) phafil, rmffil, arffil, xtefilt
      character*(*) collcube, ra_str, dec_str, arfversn
      real geomarea(0:4), jitter
      integer pcu, chat
      double precision tstart_pha, tstop_pha
      
      integer ounit, iunit1, iunit2
      character(80) context, msg
      character(80) comment(10), hist(10)
C      character(16) obs_date, obs_time
	character(68) obs_date, obs_time
      character(8) pha_telescop,pha_instrume,pha_detnam,pha_filter
      character(8) rmf_telescop,rmf_instrume,rmf_detnam,rmf_filter
      character(8) taskname, rmfclas3
      character(8) hduclas(9), hduvers(9,2), crspvers, csys
      character(20) cpcu20
      character(6) online(1)
      character(1) cpcu
      real coor(3), sccoor(3)
      real pi, acc
      double precision ra, dec, mjdref_pha
      double precision nullval
      integer gtistarts, gtistops
      integer rmf_ienerg, rmf_lo_energy, rmf_hi_energy
      integer o_vector, equinox
      integer nsearch, block, ftstatus, htype, ierr
      integer ngti, frow, felem
      integer extno(1), nfound, nret, ireadwrite
      integer nk_hist, nk_comm
      parameter (nk_hist=0, nk_comm=0)
      integer ien, icx, icy
      integer i, j, m, n
      logical anyf

      integer maxextn, maxdim, maxe, maxc1, maxc2, maxe2
      parameter (maxextn=10, maxdim=3, maxe=10, maxe2=2048, 
     &     maxc1=30, maxc2=30)
      integer status, ninstr, icol
      character(20) instr(9), ftype
      character(30) wrnstr, errstr2
      character(160) colfil(1)
      integer next(maxextn), ndim
      character(20) outhdu(9,maxextn), outver(9,maxextn)
      character(20) extnam(maxextn)
      character(20) coordtyp(maxdim), coordnam(2,maxdim)
      integer coordnpts(maxdim), coordcol(2,maxdim)
      logical qok, quiet
      real e_lo, e_hi, x_lo, x_hi, y_lo, y_hi
      integer edim, c1dim, c2dim
      integer incol(2)
      character(20) x_units, y_units
      real c1lo, c1hi, c2lo, c2hi
      character(20) c1unt, c2unt, enunt
      real egrd(2,maxe), c1grd(2,maxc1), c2grd(2,maxc2)
      real egrd_lo(maxe2), egrd_hi(maxe2)
      real cresp3d(maxe,maxc1,maxc2)
      real cresp1d(maxe)
      real cresp_out(maxe2)
      real work1d(maxe*maxc1*maxc2)
      real tmpimg(2,2)
      real x(2), y(2)
      real temp
      
c     real energ_lo(256), energ_hi(256)
      integer mode

      
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

c Initalization
      wrnstr = '** DOXPCAARF WARNING:'
      errstr2 = '** DOXPCAARF ERROR:'

      ierr = 0
      ftstatus = 0
      taskname = 'XPCAARF'
      pi = 3.14159
      frow = 1
      felem = 1
      nullval = -99.0

c shut up compiler warnings
      c1dim=0
      c2dim=0

C Check the pha file
      ireadwrite = 0
      call CK_XTEPHA(chat, phafil, iunit1, ireadwrite,
     $     pha_telescop, pha_instrume,
     $     pha_detnam, pha_filter, obs_date, obs_time, ierr)
      if (ierr .ne. 0) then
         context = errstr2//'Could not get pha file info'
         go to 999
      endif
      if (obs_time .eq. 'UNKNOWN') obs_time = '00:00:00'
      if (pha_detnam .eq. 'UNKNOWN') pha_detnam = 'ALL'
C Note that setting pha_detnam to ALL is a bit of a cheat, since
C it can't be determined exactly what pcu's are in the pha file.
C However, setting it to ALL avoids the Warning msg from compkey     

      
C Obtain further info from the PHA file only if the quaternions
C will be read      

      if (xtefilt .ne. 'none' .and. xtefilt .ne. 'NONE'
     $     .and. xtefilt .ne. 'None') then
      
C From the pha file, obtain the source position
         if (ra_str .eq. 'INDEF') then
            call ftgkyd(iunit1,'RA_OBJ',ra,comment,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to obtain object RA'
               call fcerr(context)
               goto 999
            endif
         else
C  N.B. parsedec & parsera are in ftools/develop/xanlib/coords         
            call parsera(ra_str,equinox,ra,ierr)
         endif
      
         if (dec_str .eq. 'INDEF') then
            call ftgkyd(iunit1,'DEC_OBJ',dec,comment,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to obtain object Dec'
               call fcerr(context)
               goto 999
            endif
         else
            call parsedec(dec_str,equinox,dec,ierr)
         endif

C Chatter the source position to the screen
         if (chat .ge. 10) then
            write(msg,'(a,2(f8.4,2x))') 'Source R.A. & Dec.: ',
     $           ra,dec
            call fcecho(msg)
         endif

C Convert source RA & Dec into unit vector in inertial coord.
         coor(1) = cos(pi*dec/180.) * cos(pi*ra/180.)
         coor(2) = cos(pi*dec/180.) * sin(pi*ra/180.)
         coor(3) = sin(pi*dec/180.)
      
C Get TSTART and TSTOP times from the GTI extension of the pha file,
c     or override with inputs
C Dec 16, 1996 - bug fix for GTI's with many rows: read just row 1 for start,
C     read just row = ngti for stop
C April 3, 1997 - bug fix now irrelevant since entire gti table now read    
         if (tstart_pha .eq. -1.) then
            call ftmrhd(iunit1,1,htype,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to move to GTI extension'
               call fcerr(context)
               goto 999
            endif
            call ftgkyj(iunit1,'NAXIS2',ngti,context,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to get number of GTI time values'
               call fcerr(context)
               goto 999
            endif
         
C Allocate memory for the GTIs: 
            GTIstarts = 0
            GTIstops = 0
            CALL udmget(ngti,7,GTIstarts,ftstatus)
            CALL udmget(ngti,7,GTIstops,ftstatus)

            CALL ftgcvd(iunit1,1,1,felem,ngti,nullval,MEMD(GTIstarts),
     &           anyf,ftstatus)
            CALL ftgcvd(iunit1,2,1,felem,ngti,nullval,MEMD(GTIstops),
     &           anyf,ftstatus)
            
            IF (ftstatus .NE. 0) THEN
               context = 'unable to obtain GTI time values'
               CALL fcerr(context)
               GOTO 999
            ENDIF

         endif

C N.B.  For convenience, we set the value of mjdref_pha.  This variable
c      is included in the call to rd_quatrns, but rd_quatrns doesn't
c      actually do anything with it.  At a future date, you may eliminate
c     mjdref_pha entirely, if you wish.
      
         mjdref_pha = 49352.000696574074

C endif on if xtefilt .ne. none         
      endif
      
      
C close the input pha file
      call ftclos(iunit1,ftstatus)
      call ftfiou(iunit1,ftstatus)

C If xtefilt ne none, Open the xtefilter file or quaternion file
c     and get the pointing info
      if (xtefilt .ne. 'none' .and. xtefilt .ne. 'NONE'
     $     .and. xtefilt .ne. 'None') then
        IF (tstart_pha .eq. -1) THEN
          CALL rd_quatrns(ngti, MEMD(GTIstarts), MEMD(GTIstops),
     &      mjdref_pha, xtefilt, coor, jitter, sccoor, chat, ierr)
        ELSE  
          CALL rd_quatrns(1, tstart_pha, tstop_pha, mjdref_pha,
     &      xtefilt, coor, jitter, sccoor, chat, ierr)
        ENDIF
        IF (ierr .NE. 0) GOTO 999
      ELSE
C      IF there is no filter/quaternion file (xtefilt='none') THEN
C      set the source position to the boresite (read from the par file)
        CALL uclgsr('y_boresite', sccoor(2),ftstatus)
        IF (ftstatus .NE. 0) THEN
          msg = 'boresite values not in parameter file, '//
     &      'using (0.0,0.0)'
          CALL fcecho(msg)
          sccoor(2) = 0.0
          sccoor(3) = 0.0
          ftstatus = 0
        ELSE
          CALL uclgsr('z_boresite',sccoor(3),ftstatus)
        ENDIF
      ENDIF
            
C Checkout the rmf file (from CALLIB)
      call CK_RMF(chat, rmffil, iunit1, rmf_telescop, rmf_instrume,
     $     rmf_detnam, rmf_filter, rmf_ienerg, rmfclas3, ierr)

C Compare rmf keywords with pha keywords
      call compkey(chat, rmffil, rmf_telescop, rmf_instrume,
     $     rmf_detnam, rmf_filter, 'PHA', pha_telescop, pha_instrume,
     $     pha_detnam, pha_filter, ierr)
C  if telescop don't match (ierr = 1) then quit
      if (ierr .ne. 0) go to 999
      
C allocate rmf_lo_energy & rmf_hi_energy arrays
      m = max(rmf_ienerg,100)
      rmf_lo_energy = 0
      rmf_hi_energy = 0
      call udmget(m,6,rmf_lo_energy,ftstatus)
      call udmget(m,6,rmf_hi_energy,ftstatus)
      
C get the energy grids   (GT_RMF_GRIDS is from CALLIB)
      call GT_RMF_GRIDS(chat, iunit1, rmf_ienerg,
     $     MEMR(rmf_lo_energy), MEMR(rmf_hi_energy), ierr)
      if (ierr .NE. 0) go to 999
      
      call ftfiou(iunit1,ftstatus)

C allocate the rest of the arrays
      m = max(rmf_ienerg,100)
      o_vector = 0
      call udmget(m,6,o_vector,ftstatus)
c      call udmget(m,6,collresp,ftstatus)

     
      
c %%% Start of IMG Addition -------------
c ----------------- Start of COLLIMATOR RESPONSE stuff -----------------
c ... Check whether i/p string contains special chatacters denoting 
c     COLLIMATOR RESPONSE filename to be determined from the local CALDB, 
c     and if so call caldb access software 

      if (collcube(1:5) .eq. 'CALDB' .or. collcube .eq. 'caldb') then
C
C ... From the PCU number, determine the appropriate collimator cube
         if (pcu .lt. 0 .or. pcu .gt. 4) then
            ierr = 1
            context = errstr2//'invalid PCU value'
            call fcecho(context)
            go to 999
         endif
         call fti2c(pcu,cpcu20,ftstatus)
         cpcu = cpcu20(20:20)
         pha_detnam = 'PCU'// cpcu

         quiet = .false.
         call gtcalf(chat, pha_telescop, pha_instrume, pha_detnam, '-',
     $        'COLLRESP', obs_date, obs_time, obs_date, obs_time, '-',
     $        1, colfil, extno, online, nret, nfound, ierr)
         if(ierr.NE.0)then 
            msg = errstr2//'unable to get collimator cube'//
     $           ' from caldb'
            call fcecho(msg)
            go to 999
         endif
                     
c ... Otherwise, Translate the name of the input file (taking off any 
c     extension specified by the user).

      else
         call fcpars(collcube,colfil(1),extno(1),status)
         if(status.NE.0) then
            msg = wrnstr // 
     &           ' Problem passing collcube expression'
            call fcecho(msg)
            msg = ' ... will search all extensions'
            call fcecho(msg)
            extno(1) = -99
         endif	
      endif

C Chatter the name of the collimator cube to the screen
      if (chat .ge. 10) then
         msg = 'collimator cube = '//colfil(1)
         call fcecho(msg)
      endif
      
c ... Now
c 	colfil(1) - name of file to be opened 
c	extno(1)  - extension no. of dataset (or -99 indicating search reqd)

c Open i/p file
        status = 0
      	call cgetlun(iunit2)
      	call ftopen(iunit2,colfil(1),0,block,status)
      	IF (status.NE.0) THEN
      		msg = errstr2//' opening file: '//colfil(1)
      		call wt_ferrmsg(status,msg)
		ierr = 1        
		goto 999
      	ENDIF


c GO FIND THE EXTENSION
	if(extno(1).GE.0) then
c  	   - Extension number IS already known
	   call ftmahd(iunit2,extno(1)+1,htype,status)
	   msg = wrnstr // ' Problem moving to specified xtens'
           call wt_ferrmsg(status, msg)
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
	    msg = errstr2//' Unable to locate acceptable extn'
	    call fcecho(msg)
	    goto 999
	   endif
	endif

c ... Now
c	Assuming everything's OK, we should be at the desired extension

c Find the column number of the coll.cube dataset
c ... (a good check that we're at the right place in the file)
	call ftgcno(iunit2, .false., 'COLLRESP', icol, status)
      	IF (status.NE.0) THEN
      		msg = errstr2//' Locating COLLRESP column'
      		call wt_ferrmsg(status,msg)
		ierr = 1        
		goto 999
      	ENDIF

c Get all the info regarding the storage of the coll.cube dataset
	call gcrefs(iunit2, chat, icol, maxdim, ndim,
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
	   msg = errstr2//' Coord-1 across 2 columns'
	endif
	if(coordnam(1,c2dim).NE.coordnam(2,c2dim)) then
	   qok = .false.
	   msg = errstr2//' Coord-2 across 2 columns'
	endif
	if((coordnam(1,c1dim).NE.'ALPHA').OR.
     &		coordnam(1,c2dim).NE.'BETA') then
	   qok = .false.
	   msg = errstr2//' Mixed coord system'
	endif
	if((sccoor(2).LT.x_lo).OR.(sccoor(2).GT.x_hi)) then
	   qok = .false.
	   msg = errstr2//' Coord-1 out-of-range'
	elseif((sccoor(3).LT.y_lo).OR.(sccoor(3).GT.y_hi)) then
	   qok = .false.
	   msg = errstr2//' Coord-2 out-of-range'
	endif	   
	if(.NOT.qok) then
	  call fcecho(msg)
	  goto 999
	endif
c ... Now
c	Assuming everything's OK, we believe that the position for which the 
c       the COLLRESP is required IS indeed included within the spatial 
c       coordinate grid of the calibration dataset.

c -------- End Checking & Preparation of Spatial Grid -------------
c -------------------------------------------------------------------


c -------------------------------------------------------------------
c ------------- Extract nearest Spatial elements from Cube ----------
c Extract appropriate elements from the collimator cube
c ... to do this, we set up the limits for the 3-dimensional sub-cube we 
c     want extracted. In our case we want a sub-cube which represents the
c     four nearest points in the spatial dimensions, and all the points in
c     the energy dimension. 
c ... This is achieved by setting 
c     c1lo=c1hi=sccoor(2), 
c     c2lo=c2hi=sccoor(3)
c     e_lo>e_hi (actual values dont matter)

c ... Set up "dummy" Energy i/ps (to return ALL values in energy dimension)
	e_lo = 1
	e_hi = 0
	enunt = 'keV' 
c ... Set up coordsystem
	csys = 'XMA_CART'
c ... Set up Coord-1 (ALPHA) values
	c1lo = sccoor(2)
	c1hi = sccoor(2)
	c1unt = x_units
c ... Set up Coord-2 (BETA) values
	c2lo = sccoor(3)
	c2hi = sccoor(3)
	c2unt = y_units
c ... Just do it
      	call xtcol1(iunit2, chat, maxe, maxc1, maxc2, 
     &		e_lo, e_hi, enunt,
     & 		csys, c1lo, c1hi, c1unt, c2lo, c2hi, c2unt,
     &		ien, egrd, icx, c1grd, icy, c2grd, 
     &		cresp3d, work1d, ierr)
      	IF(ierr.NE.0) then
		goto 999
             endif
             if (chat .ge. 20) then
                msg = 'xtcol1 completed'
                call fcecho(msg)
             endif
             
c OK, so now it is believed that we have the sub-cude in the cresp3d
c array, along with the relevant grid-point arrays in engrd, cxgrd, cygrd
	if(icx.NE.2) then
	   write(msg,'(a,a,i12)') errstr2,' icx =', icx
	   call fcecho(msg)
	   ierr = 1
	   goto 998
	elseif(icy.NE.2) then
	   write(msg,'(a,a,i12)') errstr2,' icy =', icy
	   call fcecho(msg)
	   ierr = 1
	   goto 998
	endif

c -------------------------------------------------------------------
c ------------- Perform Bi-linear interpolation ---------------------
C We do this for each energy-slice of the sub-cube
c ... fill in the grid points
	x(1) = c1grd(1,1)
	x(2) = c1grd(1,2)
	y(1) = c2grd(1,1)
	y(2) = c2grd(1,2)

c ... Loop over the energies, putting the result in tmpimg
      	do n = 1,ien
          do j = 1, icy
            do i = 1, icx
              tmpimg(i,j) = cresp3d(n,i,j)
            end do
           end do
c          ... interpolate to the source position (not allowing extrapolation)
	   call bilint(chat, sccoor(2), sccoor(3), 2, 2, 
     &		2, 2, x, y, tmpimg, .false., temp, ierr) 
	cresp1d(n) = temp 
      	   IF(ierr.NE.0) then
		goto 999
	   endif
	enddo
 
c So now we have a 1-dimension array cresp1d, with ien elements, and 
c energies specified by the 2-dimensional egrd array.

c Optional dump ...
	if (chat .ge. 20) then
           msg = ' ... after bilint (energ_lo, energ_hi, cresp1d)'
           call fcecho(msg)
        endif
        
c -------------------------------------------------------------------
c ------------- Perform Remapping onto rmf energy grid --------------
	do i = 1, ien
	   egrd_lo(i) = egrd(1,i)
	   egrd_hi(i) = egrd(2,i)
           if (chat .ge. 20) then
              write(msg,'(4x,f8.4,3x,f8.4,3x,f9.7)')
     $             egrd_lo(i), egrd_hi(i), cresp1d(i)
              call fcecho(msg)
           endif
	enddo
	acc = 999
	mode = 3
	call rmap1d(chat, ien, egrd_lo, egrd_hi,  cresp1d,
     &	  rmf_ienerg, MEMR(rmf_lo_energy), MEMR(rmf_hi_energy), 
     &	  cresp_out, mode, acc, ierr)
      	IF(ierr.NE.0) then
		goto 999
	endif

C Multiply the extraction from the coll. cube by the geometric area
        call multcons(geomarea(pcu),rmf_ienerg,cresp_out,MEMR(O_vector))
	
        if (chat .ge. 20) then
           msg = 'done with multiplication'
           call fcecho(msg)
        endif
        
      
C Output the .arf file (inherited from ROSAT tool PCARF)
C     Output a null primary header

      if(arfversn(1:1).EQ.'1')then
C             ... Open the FITS file and write a null primary header

c %%% Start of IMG Addition -------------
         call op_nPA(arffil, chat, ounit, ierr)
c %%% End of IMG Addition -------------

            if(ierr.ne.0) then
                  ierr = 5
                  goto 998
            endif
C             ... Add additional keywords to Primary Header
              call FTPKYS(ounit,'CREATOR', taskname,
     &             's/w task which wrote this dataset', ierr)

              call FTPKYS(ounit,'CONTENT','ANCILLARY RESP',
     &             'SPECRESP xtension', ierr)
        else
              msg = errstr2 // 'Unknown format: '// arfversn
              call fcecho(msg)
              ierr = 1
              goto 998
        endif

C  Now do SPECRESP Extentsion

      if(arfversn(1:1).EQ.'1')then
C     ... Write the SPECRESP extension within ARF file
         call fti2c(pcu,cpcu20,ftstatus)
         cpcu = cpcu20(20:20)
         pha_detnam = 'PCU'//cpcu
         call wtarf1(ounit, chat,
     &        nk_hist, hist,
     &        nk_comm, comment,arfversn,phafil,
     &        pha_telescop, pha_instrume, pha_detnam, pha_filter, 
     &        rmf_ienerg, rmf_ienerg, MEMR(rmf_lo_energy),
     &        MEMR(rmf_hi_energy), MEMR(o_vector), ierr)
         if(ierr.NE.0) goto 876
         
         call FTPKYS(ounit,'CREATOR', taskname,
     &        's/w task which wrote this dataset',ierr)
         call FTPKYS(ounit,'RESPFILE', rmffil,
     &        'RMF file',ierr)
         msg = 'Collimator Cube file '//colfil(1)
         call FTPCOM(ounit,msg,ftstatus)
         write(msg,'(a,2(f8.4,2x))') 'Source R.A. & Dec.: ',
     $        ra,dec
         call FTPCOM(ounit,msg,ftstatus)
         call ftpcks(ounit,ftstatus)
      else
         msg = errstr2 // 'Unknown format: '// arfversn
         call fcecho(msg)
         ierr = 1
         goto 998
      endif

C ----------------- finished SPECRESP EXTENSION ----------------------------

876            continue

C Close the FITS file
        call ftclos(ounit, ierr) 
      if(ierr.ne.0) then
            ierr = 6
            goto 998
      endif

        
C Exit subroutine
998   continue
999   continue 
      if (ierr .ne. 0) call fcerrm(ftstatus)
      if (ftstatus .ne. 0) call fcerrm(ftstatus) 
	return
	end
c           
C *******************************************************************
C SUBROUTINE: 
C     multcons
C
C DESCRIPTION:      
C     multiplies an array by a real*4 constant value
C
C AUTHOR:
C     James Lochner 5/95
C
C MODIFICATION HISTORY:
C
C NOTES:
C     actual size of output array is the smaller of the input arrays
C
C USEAGE:
C     call multcons(cons, n1, array1, prodct)
C
C ARGUMENTS:
C     cons	- constant
C     n1        - size of input array
C     array     - array to be multiplied
C     prodct    - cons * array1
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED SUBROUTINES:
C
C ********************************************************************

      SUBROUTINE multcons(cons, n1, array, prodct)

      integer n1
      real cons, array(n1), prodct(n1)

      integer i

      do i = 1,n1
         prodct(i) = cons * array(i)
      end do

      return
      end
