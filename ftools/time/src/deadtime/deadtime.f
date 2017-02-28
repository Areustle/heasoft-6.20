C******************************************************************************
C SELECTOR TASK:
C      deadtime
C
C FILE:
C      deadtime.f
C
C DESCRIPTION:
C	This routine reads a deadtime file and a gti file, and uses it
C	to update an exposure/ontime keyword in another file
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	21 December, 1993
C
C MODIFICATION HISTORY:
C
C  Banashree M Seifert (May 1997) V2.0  modifications are:
C       . to make the gtifile parmeter hidden (%)
C       . gti is searched for in the infile when the gtifile is %.
C       . allow for more than one MKF files
C
C  Banashree M Seifert (August, 1997) V2.1  modifications are:
C       . in subroutine DEADTM --> nullval is made double precision instead
C                                  of integer
C  Peter D Wilson (Dec 1997) V2.2
C       . in subroutine DEADTM --> allow extra memory to be allocated if
C                                  there is more data than array can hold
C
C  Ken Ebisawa (2000-3-3) v2.3 
C       . fcerrm is called after the line 999        
C       . fixed a bug in deadwr
C       . other improvement in deadwr
C
C  Ning Gan(2000-7-5) v2.4 
C       .increased max number of files from 10 to 32.
C NOTES:
C      deadtime supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
	subroutine deadte
	character(180) infile, gtifile
	character(80) gtistart, gtistop, exposure, timecol, deadcol
        character(80) contxt
	logical sensecase,samegti

	integer status
	integer iunit, gunit, dunit
	integer startno, stopno, tcolno, dcolno
	
	double precision ontime

        integer maxfiles,nfiles,lstu
        character(400) deadlist,lstfil
        parameter (maxfiles=32)
        character(180) deadfile(maxfiles)
        logical flag

	character(40) taskname
	common /task/ taskname

	taskname = 'deadtime2.4'

	infile = ' '
	gtifile = ' '
        do i=1,maxfiles
	   deadfile(i) = ' '
        enddo
	gtistart = ' '
	gtistop = ' '
	exposure = ' '
	timecol = ' '
	deadcol = ' '
	iunit = 15
	gunit = 16
	dunit = 17
        lstu  = 18
	status = 0

C  get the parameters from the par file
	call gdeadtime (infile, gtifile, deadlist, gtistart, gtistop,
     &                  exposure, timecol, deadcol, sensecase, status)
	if (status .ne. 0) goto 999

C if deadlist has '@' as first character, then
C read list file to load the names of RMF files and nfiles
C if not,
C  parse the deadfile string and get deadfile names and number of
C  deadfiles by calling fcgcls

        if(deadlist(1:1) .eq. '@') then
           lstfil=deadlist(2:)
           call rdlistfile(lstfil, lstu, maxfiles, nfiles,
     >                     deadfile, status)
           if(status .ne. 0) then
              contxt = 'error returning from rdlistfile'
              goto 999
           endif
        else
           flag=.false.
           call fcgcls(deadlist, deadfile, nfiles, flag)
           if(nfiles .gt. maxfiles) then
	      write (contxt, 899) maxfiles
899	      format (' I can only deal with ',i4, ' deadfiles')
              call fcerr(contxt)
              goto 999
           endif
        endif

        if(gtifile(1:1) .eq. '%') then
           samegti=.true.
           gunit=iunit
           gtifile=infile
        else
           samegti=.false.
        endif

C  check for reasonable inputs, open files, etc.
C  on return the pointer is at the GTI extension

	call deadck (infile, iunit, gtifile, gunit, deadfile(1),dunit,
     &              gtistart, startno, gtistop, stopno, exposure,
     &              timecol, tcolno, deadcol, dcolno, sensecase,
     >              samegti, status)

	if (status .ne. 0) goto 999

C  calculate the exposure based on deadtime and gtis

	call deadtm (gunit, startno, stopno, deadfile, nfiles, tcolno,
     >               dcolno, ontime, status)

        if(status .ne. 0) then
           contxt='error returning from deadtm'
           call fcerr(contxt)
           status = 0
           call ftclos (gunit, status)
           goto 999
        endif

        if(.not. samegti) then
    	   status = 0
	   call ftclos (gunit, status)
        endif

C don't skip to 999 on error here since the files are open

C  update the ontime keyword, and close all the files

	call deadwr (iunit, samegti, gtifile, nfiles, deadfile,
     >               exposure, ontime, taskname, status)
999	continue
        if(status.ne.0) call fcerrm(status)
	return
	end


C******************************************************************************
C SUBROUTINE:
C      gdeadtime
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	21 December, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gdeadtime uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C	call gdeadtime (infile, gtifile, deadfile, gtistart, gtistop,
C                       exposure, timecol, deadcol, sensecase, status)
C
C ARGUMENTS:
C	infile   - input FITS file and extension number
C	gtifile  - input GTI file and extension number
C	deadfile - input deadtime file and extension number
C	gtistart - name of start column in GTI file
C	gtistop  - name of stop column in GTI file
C	exposure - name of keyword in infile to update
C	timecol  - name of time column in deadtime file
C	deadcol  - name of fractional deadtime column
C	sensecase - whether to be case sensitive about column names
C	status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutiie fcerr - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C
C******************************************************************************
	subroutine gdeadtime (infile, gtifile, deadfile, gtistart,
     &           gtistop, exposure, timecol, deadcol, sensecase, status)

	character*(*) infile, gtifile, deadfile
	character*(*) gtistart, gtistop, exposure, timecol, deadcol
	logical sensecase
	integer status

	character(80) context

C  get the name of the input FITS file
	call uclgst ('infile', infile, status)
	if (status .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the input GTI file
	call uclgst ('gtifile', gtifile, status)
	if (status .ne. 0) then
	    context = 'could not get GTIFILE parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the input fractional deadtime file
	call uclgst ('deadfile', deadfile, status)
	if (status .ne. 0) then
	    context = 'could not get DEADFILE parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the GTI start column
	call uclgst ('gtistart', gtistart, status)
	if (status .ne. 0) then
	    context = 'could not get GTISTART parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the GTI stop column
	call uclgst ('gtistop', gtistop, status)
	if (status .ne. 0) then
	    context = 'could not get GTISTOP parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the exposure keyword
	call uclgst ('exposure', exposure, status)
	if (status .ne. 0) then
	    context = 'could not get EXPOSURE parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the deadtime file time column
	call uclgst ('timecol', timecol, status)
	if (status .ne. 0) then
	    context = 'could not get TIMECOL parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the deadtime file fractional deadtime column
	call uclgst ('deadcol', deadcol, status)
	if (status .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get whether to be case sensitive about column names
	call uclgsb ('sensecase', sensecase, status)
	if (status .ne. 0) then
	    context = 'could not get SENSECASE parameter'
	    call fcerr (context)
	    goto 999
	endif

999	return
	end


C **************************************************************************
C SUBROUTINE:
C      rdlistfile
C
C DESCRIPTION:
C      read the ascii file containing filenames and get the no. of files read
C
C AUTHOR:
C  Banashree M Seifert (May 1997) new provision added
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C   call rdlistfile(lstfil, lstu, maxfiles, nfiles, deadfile, errflg)
C *************************************************************************
      subroutine rdlistfile(lstfil, lstu, maxfiles, nfiles,
     >                      deadfile, errflg)

c -----------------------------------------------------------------
c  this routine opens the listfile and then reads them and returns
c  the filenames and the no. of files (nfiles)
c -----------------------------------------------------------------

      implicit none
      character*(*) lstfil, deadfile(*)
      integer lstu, maxfiles, nfiles, errflg

c -------------- internals --------------------------------------
      character(180) line
      character(80) contxt
      integer last, ierr

      errflg=0
      nfiles=0

c Open the list file
      open(unit=lstu, file=lstfil, status='old')

 10   read(lstu, '(a)', IOSTAT=ierr) line
      if(ierr .eq. 0) then
          if(line(1:2) .eq. '  ') goto 10
          call crmvlbk(line)
          last = index(line(1:),' ')
          nfiles = nfiles + 1
          deadfile(nfiles) = line(1:last -1)
          if(nfiles .gt. maxfiles) then
	      write (contxt, 19) maxfiles
 19	      format (' I can only deal with ',i4, ' MKF files.')
C             contxt = 'I can only deal with 10 MKF files'
             call fcerr(contxt)
             errflg=1
             goto 999
          endif
      goto 10
      endif

      if(nfiles .eq. 0) then
         contxt = 'Failed to read any input deadfile'
         call fcecho(contxt)
         errflg=1
         goto 999
      endif

      contxt ='... read all the files from '//lstfil
      call fcecho(contxt)

 999  close(lstu)
      return
      end

C******************************************************************************
C SUBROUTINE:
C      deadck
C
C DESCRIPTION:
C      Check the input parameter for reasonable-ness, open files, get column
C	numbers
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	21 December, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call deadck (infile, iunit, gtifile, gunit, deadfile, dunit,
C                    gtistart, startno, gtistop, stopno, exposure,
C                    timecol, tcolno, deadcol, dcolno, sensecase, status)
C
C ARGUMENTS:
C	infile   - input FITS file and extension number
C	iunit    - input file unit number
C	gtifile  - input GTI file and extension number
C	gunit    - GTI file unit number
C	deadfile - input deadtime file and extension number
C	dunit    - deadtime file unit number
C	gtistart - name of start column in GTI file
C	startno  - GTI start time column number
C	gtistop  - name of stop column in GTI file
C	stopno   - GTI stop time column number
C	exposure - name of keyword in infile to update
C	timecol  - name of time column in deadtime file
C	tcolno   - deadtime time column number
C	deadcol  - name of fractional deadtime column
C	dcolno   - deadtime fractional deadtime column number
C	sensecase - whether to be case sensitive about column names
C	status   - status of operation

C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C******************************************************************************
	subroutine deadck (infile, iunit, gtifile, gunit, deadfile,
     &             dunit, gtistart, startno, gtistop, stopno, exposure,
     &             timecol, tcolno, deadcol, dcolno, sensecase, samegti,
     >             status)

	character*(*) infile, gtifile, deadfile
	character*(*) gtistart, gtistop, exposure, timecol, deadcol
	integer iunit, gunit, dunit
	integer startno, stopno, tcolno, dcolno
	logical sensecase,samegti
	integer status

	integer extnum, block, htype, fstatus,gtiextn
	double precision value
	character(160) filename
	character(80) context,comment
        character(10) extname

C open the input file
	call fcpars (infile, filename, extnum, status)
	if (extnum .eq. -99) extnum = 1
	call ftopen (iunit, filename, 1, block, status)
	if (status .ne. 0) then
	    context = ' Error opening input file ' // filename
	    goto 999
	endif

C go to the extension
	call ftmahd (iunit, extnum+1, htype, status)
	if (status .ne. 0) then
	    write (context, 1000) extnum
1000	    format (' Error moving to infile extension number:  ',i4)
	    goto 998
	endif

C  and check that the keyword exists
	call ftgkyd (iunit, exposure, value, context, status)
	if (status .ne. 0) then
	    context = ' Could not find keyword ' // exposure
	    goto 998
	endif

c if GTI extension is in the input file, then go to the proper extension
c else open the gti file and go to the proper extension
c        if (samegti) then  go to the GTI extension
c        else open the GTI file and go to the GTI extension

        if (samegti) then
            call ftmahd(iunit,1,htype,status)
            gtiextn=0
 1          call ftmrhd(iunit,1,htype,status)
            gtiextn=gtiextn+1
            call ftgkys(iunit,'EXTNAME',extname,comment,status)
            if(extname(1:3) .ne. 'GTI') go to 1

        else
	      call fcpars (gtifile, filename, extnum, status)
	      if (extnum .eq. -99) extnum = 1
	      call ftopen (gunit, filename, 0, block, status)
	      if (status .ne. 0) then
	          context = ' Error opening GTI file ' // filename
	          goto 998
	      endif

	      call ftmahd (gunit, extnum+1, htype, status)
	      if (status .ne. 0) then
	          write (context, 1001) extnum
1001	          format (' Error moving to GTI extension ', i4)
	          goto 997
	      endif
        endif

C and check for the GTI start and stop columns
	call ftgcno (gunit, sensecase, gtistart, startno, status)
	call ftgcno (gunit, sensecase, gtistop, stopno, status)
	if (status .ne. 0) then
	    context = ' Error finding GTI start or stop column'
	    goto 997
	endif

C and finally open the deadtime file
C one file is opened to find out the column numbers assuming that all
C files are identical

	call fcpars (deadfile, filename, extnum, status)
	if (extnum .eq. -99) extnum = 1
	call ftopen (dunit, filename, 0, block, status)
	if (status .ne. 0) then
	    context = ' Error opening deadtime file ' // filename
	    goto 997
	endif

C move to the appropriate extension
	call ftmahd (dunit, extnum+1, htype, status)
	if (status .ne. 0) then
	    write (context, 1002) extnum+1
1002	    format (' Error moving to deadtime file extension: ', i4)
	    goto 996
	endif

C and get the column numbers
	call ftgcno (dunit, sensecase, timecol, tcolno, status)
	call ftgcno (dunit, sensecase, deadcol, dcolno, status)
	if (status .ne. 0) then
	    context = ' Error finding columns in deadtime file'
	    goto 996
	endif

	call ftclos (dunit, fstatus)

	return

C Error return, close all open files

996	fstatus = 0
	call ftclos (dunit, fstatus)

997	fstatus = 0
	call ftclos (gunit, fstatus)

998	fstatus = 0
	call ftclos (iunit, fstatus)

999	call fcerr (context)

	return
	end

C******************************************************************************
C SUBROUTINE:
C      deadtm
C
C DESCRIPTION:
C	Determine the exposure based on gti and deadtime fraction
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	22 December, 1993
C
C MODIFICATION HISTORY:
C  Banashree M Seifert (May 1997)
C     . subroutine call ADD_DEADTIME added so that deadtimes from multiple
C       MKF files are appended one after another.
C  Banashree M Seifert (August, 1997)
C     . nullval was defined as integer, but was reading DOUBLE PRECISION
C       values
C       So, nullval is made DOUBLE PRECISION instead of INTEGER
C  Jeff Guerber (Sept. 1997) Incr. maxdead to 50000, added check on total
C       number.  Need to revisit whole issue of fixed-size arrays!
C  Peter Wilson (Dec. 1997) Array treatment changed so that more memory
C       gets allocated if needed to hold next batch of data.
C
C NOTES:
C
C USAGE:
C	call deadtm (gunit, startno, stopno, deadfile, nfiles, tcolno,
C                    dcolno, ontime, status)
C
C ARGUMENTS:
C	gunit    - GTI file unit number
C	startno  - GTI start time column number
C	stopno   - GTI stop time column number
C	dunit    - deadtime file unit number
C	tcolno   - deadtime time column number
C	dcolno   - deadtime fractional deadtime column number
C	ontime   - the time of the exposure
C	status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C******************************************************************************
	subroutine deadtm (gunit, startno, stopno, deadfiles, nfiles,
     >                     tcolno, dcolno, ontime, status)

        character*(*) deadfiles(*)
	integer gunit, status, nfiles
	integer startno, stopno, tcolno, dcolno
	double precision ontime

	integer ngtis, ndead, felem, nelem, drow, grow
	double precision tstart, tstop, dstart, dstop, deadfrac, deltat
	logical anyf
	character(80) context

        character(180) filename
        integer p_d_start, p_d_deadfrac
        integer p_dstart_tmp, p_deadfrac_tmp
        integer maxdead, i,tdead, unit,extnum, block,htype
        double precision nullval

c ---------- DM allocation ----------------------------------
C Dynamic memory allocation stuff
C the following MEM common block definition is in the system iraf77.inc file
C Get IRAF MEM common into main program.

      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,  4 = Integer*4
C     1 is boolean
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 complex
c -------------------------------------------------------------------

      maxdead = 20000
      p_d_start = 0
      p_d_deadfrac = 0
      call udmget(maxdead, 7, p_d_start, status)
      call udmget(maxdead, 7, p_d_deadfrac, status)

C  find how many gtis and deadtimes there are

      status=0
      call ftgkyj (gunit, 'NAXIS2', ngtis, context, status)
      if (status .ne. 0) then
          context = ' Unable to determine number of GTIs'
          goto 999
      endif

c Now read all the deatime files and collect the TIME and deadfraction
c columns into one column
c that is, join all the deadtimes in time col and fraction also in another
c column

      drow=1
      felem=1
      ndead = 0
      unit = 21

      anyf= .false.
      do i=1,nfiles
         extnum=0
         htype=0
         tdead=0
         status=0
         call fcpars (deadfiles(i), filename, extnum, status)
	 if (extnum .eq. -99) extnum = 1
	 call ftopen (unit, filename, 0, block, status)
	 if (status .ne. 0) then
	     context = ' Error opening deadtime file ' // filename
	     goto 999
	 endif

C        move to the appropriate extension
	 call ftmahd (unit, extnum+1, htype, status)
	 if (status .ne. 0) then
	     write (context, 1002) extnum+1
1002	     format (' Error moving to deadtime file extension: ', i4)
	     goto 999
	 endif
	 call ftgkyj (unit, 'NAXIS2', tdead, context, status)
	 if (status .ne. 0) then
	     context = ' Unable to get NAXIS2 keyword from '//filename
	     goto 999
      	 endif

         p_dstart_tmp = 0
         p_deadfrac_tmp = 0
         call udmget(tdead, 7, p_dstart_tmp, status)
         call udmget(tdead, 7, p_deadfrac_tmp, status)

         status=0
         nullval=0.d0
	 call ftgcvd (unit, tcolno, drow, felem, tdead,nullval,
     >                MEMD(p_dstart_tmp), anyf, status)
         if (status .ne. 0) then
             context = ' Error reading deadtime file'//filename
             goto 999
         endif

         call ftgcvd (unit, dcolno, drow, felem, tdead, nullval,
     >                MEMD(p_deadfrac_tmp), anyf, status)
         if (status .ne. 0) then
             context = ' Error reading deadfrac file'//filename
             goto 999
         endif

	 if ( (ndead+tdead) .gt. maxdead ) then
	     maxdead = max(maxdead+10000,ndead+tdead)
             p_d_start = 0
             p_d_deadfrac = 0
	     call udmget(maxdead, 7, p_d_start, status)
	     if( status.eq.0 ) 
     >             call udmget(maxdead, 7, p_d_deadfrac, status)
	     if( status.ne.0 ) then
		context = ' Error reallocating p_d_xxx arrays'
		goto 999
	     endif
	 endif

         call add_deadtimes(ndead, tdead, MEMD(p_dstart_tmp),
     >                      MEMD(p_d_start), MEMD(p_deadfrac_tmp),
     >                      MEMD(p_d_deadfrac))

         call udmfre(p_dstart_tmp, 7, status)
         call udmfre(p_deadfrac_tmp, 7, status)
         call ftclos(unit, status)
         unit = unit+1
      enddo


      felem = 1
      nelem = 1
      drow = 1
      ontime = 0.D0

C  loop over all GTIs
      do 500 grow = 1, ngtis

C         get the next gti
          call ftgcvd (gunit, startno, grow, felem, nelem, nullval,
     &                 tstart, anyf, status)
          if (status .ne. 0) then
              context = ' Error reading tstart from GTI file'
              goto 999
          endif

          call ftgcvd (gunit, stopno, grow, felem, nelem, nullval,
     &                 tstop, anyf, status)

       	  if (status .ne. 0) then
              context = ' Error reading tstop from GTI file'
	      goto 999
          endif

C         find the deadtime interval which contains tstart

100	  call getdeadtime(tstart, drow, ndead, MEMD(p_d_start),
     >                     MEMD(p_d_deadfrac), dstart, dstop, deadfrac,
     >                     status)

          if(status .ne. 0) then
             context='error returning from getdeadtime '
             goto 999
          endif

	  if (status .ne. 0) then
C             tstart is not included in deadtime file
	      write (context, 1000) tstart
1000	      format (' Deadtime file does not contain time:',1PE23.15)
	      goto 999
	  endif

C         calculate the time for this interval
	  if (tstop .le. dstop) then
	      deltat = tstop - tstart
	      tstart = tstop
	  else
	      deltat = dstop - tstart
	      tstart = dstop
	  endif

C         add with appropriate deadtime fraction to the accumulated time
	  ontime = ontime + (1.D0 - deadfrac) * deltat

C         if more time is in this GTI, loop back for next deadtime
	  if (tstart .lt. tstop) goto 100

C         else go back for next GTI
500   continue

      call udmfre(p_d_start, 7, status)
      call udmfre(p_d_deadfrac, 7, status)

      return

999   call fcerr (context)
      call udmfre(p_d_start, 7, status)
      call udmfre(p_d_deadfrac, 7, status)

      return
	end

C*****************************************************************************
C SUBROUTINE:
C     add_deadtimes
C
C DESCRIPTION:
C    adds all the deatimes and deadfractions from nfiles into one column
C
C AUTHOR:
C    Banashree M Seifert
C    May, 1997
C
C MODIFICATION HISTORY
C    This subroutine is introduced when user asked for provision to input
C    more than one MKF files
C
C
C USAGE:
C      call add_deadtimes(ndead, tdead, dstart_tmp, dstart,
C                         deadfrac_tmp, deadfrac)
C
C ARGUMENTS:
C   ndead        - total no. of deadtimes
C   tdead        - current no. of dead times
C   dstart_tmp   - curremt start time for deadtime correction
C   deadfrac_tmp - curremt fractional deadtime
C   dstart       - total deadtime added in columns
C   deadfrac     - total fractional deadtimes added in columns
C
C******************************************************************************

      subroutine add_deadtimes(ndead, tdead, dstart_tmp, dstart,
     >                         deadfrac_tmp, deadfrac)


      implicit none
      integer tdead, ndead
      double precision dstart_tmp(*), deadfrac_tmp(*)
      double precision dstart(*), deadfrac(*)

      integer j


      do j=1,tdead
         dstart(ndead+j) = dstart_tmp(j)
         deadfrac(ndead+j) = deadfrac_tmp(j)
      enddo

      ndead = ndead+tdead

      return
      end


C******************************************************************************
C SUBROUTINE:
C      getdeadtime
C
C DESCRIPTION:
C	Get the deadtime record which contains TIME
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	22 December, 1993
C
C MODIFICATION HISTORY:
C  Banashree M Seifert (May, 1997)
C       . modified to accomodate more than one MKF input files
C
C NOTES:
C
C USAGE:
C       call getdeadtime (time, drow, ndead, d_start, d_deadfrac,
C                               dstart, dstop, deadfrac, status)
C
C ARGUMENTS:
C	time     - the time for which the deadtime fraction is requested
C	dunit    - deadtime file unit number
C	tcolno   - deadtime time column number
C	dcolno   - deadtime fractional deadtime column number
C	drow     - the current row in the deadtime file
C	ndead    - total number of rows in deadtime file
C	dstart   - the returned start time of the deadtime interval
C	dstop    - the returned stop time of the deadtime interval
C	deadfrac - the returned deadtime fraction for this interval
C	status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C******************************************************************************
	subroutine getdeadtime (time, drow, ndead, d_start, d_deadfrac,
     >                          dstart, dstop, deadfrac, status)

	double precision time, deadfrac
	integer drow, ndead
	double precision dstart, dstop, d_start(*), d_deadfrac(*)
	integer status

	integer felem, nelem
	character(80) context

	felem = 1
	nelem = 1
C get the dstart and dstop corresponding to the input drow
100	if (drow .ge. ndead) then
	    status = 10
	    context = ' Deadtime file does not include all GTIs'
	    call fcerr (context)
	    goto 999
	endif

        dstart=d_start(drow)
        dstop =d_start(drow+1)

C check if time is in this interval

        if (time .ge. dstop) then
            drow = drow + 1
            goto 100
        else if ((time .ge. dstart) .and. (time .lt. dstop)) then
            deadfrac = d_deadfrac(drow)
        else
C           file is assumed to be time ordered, as are gtis
            status = 10
        endif

999	return
	end

C******************************************************************************
C SUBROUTINE:
C      deadwr
C
C DESCRIPTION:
C	Get the deadtime record updated
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	22 December, 1993
C
C MODIFICATION HISTORY:
C Banashree M Seifert (May 1997)
C    . modified when GTI file == infile
C    . few more history keywords added
C AM (March 1999)
C    . updated to change the exposure time keyword in all extensions
C
C Ken Ebisawa (2000-3-3)
C    .Fixed the bug that the program does not run on the spectral
C     file whose primary does not have the EXTNAME keyword.
C    .Old ontime values are written in the history keyword
C    .Name of the keyword modified is written in the history keyword
C NOTES:
C
C USAGE:
C   call deadwr (iunit, samegti, gtifile, nfiles, deadfile, exposure,
C                ontime, taskname, status)
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C******************************************************************************
	subroutine deadwr (iunit, samegti, gtifile, nfiles, deadfile,
     >                     exposure, ontime, taskname, status)

        implicit none
	integer iunit, status, nfiles
	character*(*) gtifile, deadfile(*), exposure, taskname
	double precision ontime, original_ontime
        logical samegti

        character(8) extname
	character(80) context, comment
        character(300) hist
	integer i, j, htype, HDUNUM
	

C       Get the number of HDUs of the file
        call ftthdu(iunit,HDUNUM,status)
	if (status .ne. 0) then
	   context = 'Error getting HDUNUM'
	   call fcerr (context)
	   return
	endif

C       Update the keywords in all the extensions
        DO I = 1, HDUNUM
	   status = 0
	   call ftmahd(iunit,I,htype,status)
	   context = '&'
	   call ftgkyd (iunit,exposure,original_ontime,context,status)
	   if(status.eq.0) then
	      call ftmkyd (iunit, exposure, ontime, 14, context, status)
C       write history keywords
	      hist = 'DEADTIME correction done by:'
	      call ftphis(iunit,hist,status)
	      call timestamp(iunit)
	      hist = 'GTI file used for the correction '//gtifile
	      call ftphis(iunit,hist,status)
	      hist = 'The keyword modified is '//exposure
	      call ftphis(iunit,hist,status)
	      write(hist,'(a,1pe11.5)') 'Original exposure value = ',
     1              original_ontime
	      call ftphis(iunit,hist,status)
	      hist = 'MKF files used are:'
	      call ftphis(iunit,hist,status)
	      do j=1,nfiles
		 write(hist,100) j, deadfile(j)
 100		 format(i2,'.  ', a)
		 call ftphis(iunit,hist,status)
	      enddo
	   endif
        ENDDO

C       close the input file
	call ftclos (iunit, status)
	return
	end
c --------------------------------------------------------------------------












