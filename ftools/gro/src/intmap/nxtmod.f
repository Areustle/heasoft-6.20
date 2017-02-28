CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(NXTMOD) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  NXTMOD
CH1
CH1  Version: 1.02                  Date: 05/03/91
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: nxtmod.f,v 1.4 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Get the next instrument mode from the exposure history
CH1            file and accumulate the live time for it.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call NXTMOD(tscco,imod,typs,savdir,
CH2                                 savthr,ltmap,term)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     tscco       L*4     O   TASC in Coincidence flag
CH2     imod        I*4     O   Active viewing mode
CH2     typs        I*2     O   Type mode bit mask
CH2     savdir(4)   R*4     O   Instrument Pointing direction
CH2     savthr      Ch*4    O   Threshold for the current mode (as read)
CH2	ltmap       R*4     O   Array of live times
CH2     term        L*4     O   Determines if no more modes
CH2
CH2  Called by:  EXPOSR
CH2
CH2  Calls:
CH2     CONVRT: Utility routine to convert a TJD/MSD time in a real*8
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
CH3 tsco       Integer   TASC in coincidence flag
CH3 acs        Integer   ACS value
CH3 tunit      Integer   Unit number for timeline file
CH3 eunit      Integer   Unit number for exposure history file
CH3 calfil(2)  Ch*8      Names of the calibration files
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  maxsiz     I*4      162199   Maximum number of entries in file
CH3  eltim      R*4        -      Elapsed time
CH3  xstime     R*8        -      Exposure history file start time
CH3                               within data time range
CH3  xetime     R*8        -      Exposure history file end time within
CH3                               data time range
CH3  p5         R*8        -      0.5 degrees in radians
CH3  ltim       R*4        -      Live time read
CH3  dir(4)     R*4        -      Instrument pointing direction
CH3  ltimes     R*4        -      Array of adjusted live times
CH3  (maxsiz)
CH3  telmod(87) I*4     see code  Array of direction modes
CH3  coinc      I*4        -      Coincidence read from file
CH3  tnd        I*4        -      Type and direction mode read from file
CH3  tjd        I*4        -      Current truncated Julian day
CH3  msd        I*4        -      Current millisecond of day
CH3  top        I*4        -      Points to current 1st entry in file
CH3  bot        I*4        -      Points to last valid entry in file
CH3  cur        I*4        -      Points to current entry in file
CH3  savtnd     I*4        -      Type and direction mode for cur mode
CH3  savcoi     I*4        -      Coincidence for current mode
CH3  first      L*4        -      First time routine called flag
CH3  thr        Ch*4       -      TASC OK threshold as read from file
CH3  line       Ch*112     -      file entry
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       6              Printer report
CH4       8    eunit     Exposure history file
CH4
CH4  Method:
CH4    If (this is the first time the routine is called) then
CH4       While (the last valid file entry has not been found) do
CH4          Read the next exposure history file entry (skip comments)
CH4          Save the live time for the entry in an array
CH4          Get the start and end times for the entry
CH4          If (no 1st valid entry found & entry is in data time) then
CH4             Set the top entry pointer to that entry
CH4             If (data time range starts in the entry) then
CH4                Adjust entry live time to only portion in data range
CH4             End if
CH4          End if
CH4          If (entry end time > data end time) then
CH4             Set the bottom pointer entry to that entry
CH4             If (data time range ends in the entry) then
CH4                Adjust entry live time to only portion in data range
CH4             End if
CH4          End if
CH4       End while
CH4       If (no top entry pointer) terminate with an error message
CH4       If (no bottom entry pointer) set it to last entry in the file
CH4    End if
CH4    If (the top pointer was not found) set terminate flag & return
CH4    Start at the beginning of the exposure history file
CH4    Read entries up to the top pointer
CH4    Save the values for the top entry and accumulate the live time
CH4    Mark the entry as already used
CH4    Set the current pointer to the next entry
CH4    Clear the top pointer
CH4    While (the current pointer is not at the bottom pointer) do
CH4       Read the line at the current pointer (skip comments)
CH4       If (the entry is marked as already been used) skip the line
CH4       If (the entry fields are similar to the values saved) then
CH4          Add the live time to the accumulated live time
CH4          Mark the entry as already used
CH4       Else if (top pointer is cleared) then
CH4          Set the top pointer to this entry
CH4       End if
CH4    End while
CH4    Put entry values to be returned in the appropriate format
CH4  End NXTMOD
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5     1.02    A.Etienne 05/03/91    Increased the buffer to read the
CH5                                   exposure history file (line) from
CH5                                   80 to 112 bytes to match the new
CH5                                   exposure history file format.
CH5
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Changed ! comments to C comments.
CH5				Changed Z constants (IBM) to x constants (f77).
CH5				Defined varaibles undefined on IBM.
CH5				Renamed tscco to tsco, an integer paramter.
CH5				Tscco converted to integer in parameter tsco.
CH5				Merged changes of version 1.03 on IBM.
CH5     			Changed subroutine parameters to
CH5                             pass an array of live times
CH5                             instead of a scalar for the earth
CH5                             shadow processing. Read new format
CH5                             of the exposure history file with
CH5                             the earth positions in it. Called
CH5                             SHADOW routine to process the earth shadow.
CH5
CH5 $Log: nxtmod.f,v $
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
CH5 Revision 1.1  2002/04/16 20:24:03  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.8  1996/08/15  20:15:44  dlb
c Extended telmod table to include strip mode configurations
c and increased the do-loop range from 75 to 87 to account for
c the increased table size.
c
c Revision 2.7  1994/12/09  14:47:21  albert
c Read the calibration table index using the new format of the exposure
c history files and passed it to the calling routine. Allowed for the new
c mode of verticals only.
c
c Revision 2.6  1993/11/15  20:07:12  albert
c Added code to initialize the livetime array to 0 when there is a new mode
c being processed. This corrects a program bug where the array was not
c zeroed out for a few mode changes and the initialization was done in
c routine shadow.
c
c Revision 2.5  1993/05/06  13:08:45  albert
c Added livetime array more efficiently
c
c Revision 2.4  1992/10/14  16:22:15  albert
c Modified to skip excluded times from the counts map header in addition to
c the excluded times in the exposure history file. Wrote a warning message
c if the times don't match. Corrected a minor problem in the computation of
c the eartth shadow for the first and last intervals.
c
c Revision 2.3  1992/06/01  18:19:06  albert
c Modified to skip all exposure history entries with the excluded or the SAA
c flags on instead of testing only the live time.
c
c Revision 2.2  1992/04/01  21:50:51  albert
c Used variable dimesion bin data arrays. Corrected error when the input map
c start and end times occured in between an exposure history time interval.
c Wrote a warning message when the user start time was before the exposure
c history file start time or when the end time was after the file end time.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine NXTMOD(tsco,imod,typs,savdir,savthr,savcal,ltmap,term)

      integer	maxsiz, iand
      parameter (maxsiz = 8000)
      real      livtim
      real*8    xstime,xetime,time1,time2,dtime,CONVRT,p5
      real      ltmap(naxis1,naxis2,naxis3),slat,clat,rlon
      real      ltim,savdir(4),dir(4),ltimes(maxsiz),eltim
      real      epos(4),tstep,totime
      integer*2 typs
      integer   imod,i,j,k,telmod(87),ntotl,tsco,izcnt,calcod
      integer   coinc,tnd,tjd,msd,top,bot,cur,savtnd,savcoi,nstep,savcal
      integer	ni,ix,itop,icur
      logical   first,term,tscco,qnew
      character(4) thr,savthr
      character(112) line

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'
      save

      data first/.true./, tstep/10.0/

Cesp  ! 0.5 degrees in radians
      data p5 /8.72664626E-3/

      data telmod /
     &   x'0000', x'1000', x'4000', x'0100', x'0400', x'1400', x'5000',
     &   x'4100', x'0500', x'1C00', x'1800', x'3000', x'7000', x'6000',
     &   x'4080', x'4180', x'0180', x'0300', x'0700', x'0600', x'0C00',
     &   x'3800', x'6080', x'0380', x'0E00', x'3C00', x'7800', x'7080',
     &   x'6180', x'4380', x'0780', x'0F00', x'1E00', x'7C00', x'7180',
     &   x'4780', x'1F00', x'3E00', x'7880', x'6380', x'0F80', x'3E03',
     &   x'3C03', x'7C03', x'7803', x'7883', x'7083', x'7183', x'6183',
     &   x'6383', x'4383', x'4783', x'0783', x'0F83', x'0F03', x'1F03',
     &   x'1E03', x'7E03', x'7C83', x'7983', x'7383', x'6783', x'4F83',
     &   x'1F83', x'3F03', x'7E83', x'7B83', x'6F83', x'3F83', x'7F03',
     &   x'7D83', x'7783', x'5F83', x'7F83', x'0003', x'0080', x'0083',
     &   x'0800', x'0803', x'0880', x'0883', x'0200', x'0203', x'2000',
     &   x'2003', x'2200', x'2203'/

      character(80)	id
      common	/id/id
      id = '$Id: nxtmod.f,v 1.4 2013/05/21 19:08:24 irby Exp $'

C---> Test if this is the first time
      if (first) then
         top = 0
         bot = 0
         cur = 0
         ni  = 0
         ix  = 1
         itop= 0
         term = .false.
         write(6,*) ' '

C------> Find the first and last entries within the time range
         do while (bot.eq.0)
            read(eunit,'(a112)',end=20) line
            if (line(1:1).eq.'*') then
               if (line(2:2).eq.'*') read(line(38:39),'(i2)') calcod
               goto 10
            end if
            read(line,1000) tjd,msd,tnd,coinc,thr,(dir(j),j=1,4),
     &           ltim,eltim,(epos(j),j=1,4)
            cur = cur + 1
            if (cur .gt. maxsiz) goto 200
            ltimes(cur) = ltim
	    if (line(18:18).eq.'T'.or.line(20:20).eq.'T')ltimes(cur)=0
            xstime = CONVRT(tjd,msd)
            xetime = xstime + eltim/86400D0

C---------> Find the 1st entry within the time range
            if (top.eq.0 .and. xetime.gt.strtim .and. ltimes(cur).gt.0 
     &           .and. xstime.lt.endtim) then
               top = cur
               if (xstime .lt. strtim) then
                  ni = ni + 1
                  indexn(ni) = cur
                  ltimes(cur)=ltimes(cur)*(xetime-strtim)/
     *                 (xetime-xstime)
                  eltimn(ni) = (xetime - strtim) * 86400D0
                  ntotl = (xetime-xstime)*86400D0 / tstep + 1
                  nstep = (strtim-xstime)*86400D0 / tstep + 1
                  qnew = .true.
                  eposn(ni,3) = epos(3)
                  eposn(ni,4) = epos(4)
                  call earthp(nstep,ntotl,'CELE',epos,qnew,slat,clat,
     &                 rlon)
                  eposn(ni,1) = rlon
                  eposn(ni,2) = atan2(slat,clat)
	       else if (cur .eq. 1) then
	          write(6,*)'WARNING: the counts map start time is ',
     &                 'before the exposure history file start time'
	          write(6,*) '         map start time  = ',strtim
	          write(6,*) '         file start time = ',xstime
	       end if
            end if

C---------> Find if the current entry is within the current excluded time range
            if (top.ne.0 .and. ix.le.numexc .and. ltimes(cur).ne.0) then

C------------> Get the next excluded time interval
	       do while(ix.le.numexc .and. xstime.ge.endexc(ix))
                  ix = ix + 1
	       end do
	       if (ix.le.numexc .and. strexc(ix).lt.xetime) then
		  if (strexc(ix).le.xstime.and.xetime.le.endexc(ix))then
		     ltimes(cur) = 0
		  else
		     ni = ni + 1
		     indexn(ni) = cur
		     time1 = max(xstime,strexc(ix))
		     time2 = min(xetime,endexc(ix))
		     dtime = (time1 - xstime) +  (xetime - time2)
		     ltimes(cur) = ltimes(cur) * dtime/(xetime-xstime)
		     eltimn(ni) = dtime * 86400D0
		     ntotl = (xetime-xstime)*86400D0 / tstep + 1
		     qnew = .true.
		     do i=1,4
			eposn(ni,i) = epos(i)
		     end do
		     if (strexc(ix).gt.xstime) then
		        nstep = (strexc(ix)-xstime)*86400D0 / tstep + 1       
			call earthp(nstep,ntotl,'CELE',epos,qnew,slat,
     &                       clat,rlon)
		 	eposn(ni,3) = rlon
		 	eposn(ni,4) = atan2(slat,clat)
		     end if
		     if (endexc(ix).lt.xetime) then
		 	nstep = (endexc(ix)-xstime)*86400D0 / tstep + 1
		 	call earthp(nstep,ntotl,'CELE',epos,qnew,slat,
     &                       clat,rlon)
		 	eposn(ni,1) = rlon
		 	eposn(ni,2) = atan2(slat,clat)
		     end if
		     write(6,2000) ix,eltim-eltimn(ni),cur,line(3:81)
		  end if
	       end if
	    end if

C---------> Find the last entry within the time range
            if (xetime .ge. endtim) then
               bot = cur
               if (xstime .lt. endtim) then
                  ni = ni + 1
                  indexn(ni) = cur
                  ltimes(cur)=ltimes(cur)*(endtim-xstime)/
     *                 (xetime-xstime)
                  eltimn(ni) = (endtim - xstime) * 86400D0
                  ntotl = (xetime-xstime)*86400D0 / tstep + 1
                  nstep = (endtim-xstime)*86400D0 / tstep + 1
                  qnew = .true.
                  eposn(ni,1) = epos(1)
                  eposn(ni,2) = epos(2)
                  call earthp(nstep,ntotl,'CELE',epos,qnew,slat,clat,
     &                 rlon)
                  eposn(ni,3) = rlon
                  eposn(ni,4) = atan2(slat,clat)
               else
                  bot = bot - 1
               end if
            end if
 10         continue
         end do

 20      continue
         first = .false.
         if (top .lt. 1) goto 100
         if (bot .eq. 0) then
	    write(6,*) 'WARNING: the end time of the counts map is ',
     &           'later than the exposure history file end time'
	    write(6,*) '         map end time  = ',endtim
	    write(6,*) '         file end time = ',xetime
	    bot = cur
	 end if

      end if

C---> Verify if there are any more entries to process
      if (top .eq. 0) then
         term = .true.
         goto 90
      end if

C---> Read the entry at position top
      i = 0
      calcod = 0
      rewind(eunit)
      do while (i .lt. top)
         read(eunit,'(a112)') line
         if (line(1:1).eq.'*') then
            if (line(2:2).eq.'*') read(line(38:39),'(i2)') calcod
         else
            i = i + 1
         end if
      end do
      read(line,1000) tjd,msd,tnd,coinc,thr,(dir(j),j=1,4),ltim,eltim,
     &     (epos(j),j=1,4)
      write(6,*) 'Processing modes similar to:',line
      
C---> Save the values for this entry
      savtnd = tnd
      savcoi = coinc
      savthr = thr
      savcal = calcod
      do i=1,4
         savdir(i) = dir(i)
      end do
      livtim = ltimes(top)
      totime = livtim
      ltimes(top) = 0
      cur = top + 1
      i = itop + 1
      do while (i.le.ni .and. indexn(i).le.top)
         if (indexn(i) .eq. top) then
	    eltim = eltimn(i)
	    do j=1,4
	       epos(j) = eposn(i,j)
	    end do
	    itop = i
         end if
	 i = i + 1
      end do
      top = 0
      icur = itop

C---> Clear the livetime array
      do k = 1, naxis3
         do j = 1, naxis2
            do i = 1, naxis1
               ltmap(i,j,k) = 0.0
            end do
         end do
      end do
      
      call SHADOW(tnd,livtim,eltim,epos,tstep,zenmax,ltmap)

C---> Accumulate the live times for all similar entries
      do while (cur .le. bot)
         read(eunit,'(a112)') line
         if (line(1:1).eq.'*') then
            if (line(2:2).eq.'*') read(line(38:39),'(i2)') calcod
            goto 40
         end if
         read(line,1000)tjd,msd,tnd,coinc,thr,(dir(j),j=1,4),ltim,eltim,
     &        (epos(j),j=1,4)
         if (ltimes(cur) .eq. 0) goto 30
         if (tnd.eq.savtnd.and.coinc.eq.savcoi.and.thr.eq.savthr.and.
     &        calcod.eq.savcal.and.abs(savdir(1)-dir(1)).le.p5 .and.
     &        abs(savdir(2)-dir(2)).le.p5 .and.
     &        abs(savdir(3)-dir(3)).le.p5 .and.
     &        abs(savdir(4)-dir(4)).le.p5) then
            livtim = ltimes(cur)
	    totime = totime + livtim
            ltimes(cur) = 0
            i = icur + 1
            do while (i.le.ni .and. indexn(i).le.cur)
               if (indexn(i) .eq. cur) then
	          eltim = eltimn(i)
	          do j=1,4
	             epos(j) = eposn(i,j)
	          end do
	          icur = i
               end if
	       i = i + 1
            end do
    	    call SHADOW(tnd,livtim,eltim,epos,tstep,zenmax,ltmap)
         else if (top .eq. 0) then
            top = cur
         end if
 30      continue
         cur = cur + 1
 40      continue
      end do

c---> Add the total live time to the ltmap array.  Since shadow
c---> accumulates the time shadowed as a negative number, this
c---> will give us the actual live time at each point.

      izcnt = 0
      do k=1, naxis3
         do j=1, naxis2
            do i=1, naxis1
               ltmap(i,j,k) = ltmap(i,j,k) + totime
               if (ltmap(i,j,k) .lt. 0) then
                  ltmap(i,j,k) = 0
                  izcnt = izcnt + 1
               end if
            enddo
         enddo
      enddo
      if (izcnt .ne. 0) write(6,*) 'Number of negative pixels =',izcnt

C---> Save the values that need to be returned
      tscco = btest(savcoi,2)
C     Convert logical to integer to pass in paramter !
      if(tscco) then
         tsco = 1
      else
         tsco = 0
      endif

      typs = IAND(savtnd,124)

C---> Find the type and direction mode index
      imod = 0
      do i=1,87
         if (savtnd .eq. telmod(i)+typs) imod = i
      end do
      if (imod .eq. 0) then
         write(6,'('' NXTMOD: No entry found for mode '',z4)') savtnd
         imod = 1
      else if (imod .eq. 75) then
         imod = 74
      end if

 90   continue
      return

C---> Time range not matched in exposure history file
100   continue
      write(6,*)'NXTMOD:data time not matched in exposure history file'
      retcod = 8
      return

C---> Too many entries in file
200   continue
      write(6,*)'NXTMOD: More than',maxsiz,' lines in expo history file'
      retcod = 8
      return

1000  format(1x,i6,i9,5x,z4,1x,z1,1x,a3,4f8.4,f7.1,f10.3,4f8.4)
2000  format(80('*'),/,'* WARNING: The exclude interval number',i3,
     & ' in the counts map header excludes',/,'*',f10.3,' seconds',
     & ' from entry number',i6,':',/,'*',a,/,'* which were not',
     & ' excluded by the entry itself. Discrepancies > 2.048 sec may',
     & /,'* indicate a problem.',/,80('*'),/)
CCCCCCCCCCCCCCCCCCCC END EXPHST.SOURCE(NXTMOD) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
