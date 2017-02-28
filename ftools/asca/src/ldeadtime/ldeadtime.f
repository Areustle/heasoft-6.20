C ***************************************************************************
C SELECTOR TASK
C      ldeadtime
C
C FILE:
C      ldeadtime.f
C
C DESCRIPTION:
C     Exposure column of FITS light curves are corrected
C     based on deadtime values in the mkf file.
C
C AUTHOR:
C      Srilal Weera, Version 1.0 03/96
C
C MODIFICATION HISTORY:
C     Modified by Ken Ebisawa, Version 1.1 May-20-1996
C     The name is change from 'lexpcor' to 'ldeadtime'
C     In addition to filling the FRACEXP column, the RATE and
C     ERROR columns are divided by the FRACEXP values; i.e.
C     deadtime correction is carried out within this task.
C
C NOTES:
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C       mkffile   	- Name of mkf FITS file and [ext#]
C       lcfile	 	- Name of light curve FITS file and [ext#]
C	mkfbnwidth	- Bin width from mkffile
C 	g2,3deadt	- Column names for G2,3DEAD_T
C	status  	- error number
C	history		- History keyword is written
C
C CALLED ROUTINES:
C     subroutine gldeadtime  - Gets parameters from environment
C     subroutine ldeadtime  - Calculates the exposure correction for lcfile
C     subroutine fcerrm   - echo fitsio error message to terminal
C
C **************************************************************************

      Subroutine ldeade

      character(160) lcfile, mkffile
      character(80)   mkfbnwidth, g2deadt, g3deadt

      integer status
      logical history

      character(40) taskname
      common /task/ taskname

      taskname = 'ldeadtime v1.1'
      status=0

C get the parameters from the par file
      call gldeadtime(lcfile,mkffile,mkfbnwidth,g2deadt,g3deadt
     & 			,history, status)

      if (status .ne. 0) goto 999

C calculate the exposure correction
      call ldeadtime(lcfile,mkffile, mkfbnwidth, g2deadt, g3deadt
     &		,history, status)

999   if (status .ne. 0) call fcerrm(status)
      end

C **************************************************************************
C SUBROUTINE:
C      gldeadtime
C
C DESCRIPTION:
C      Gets parameters from parameter file
C
C AUTHOR:
C      Srilal Weera, Version 1.0 03/96
C
C MODIFICATION HISTORY:
C      Ken Ebisawa, Version 1.1 May-20-1996
C     The name was changed from 'gexpcor' to 'gldeadtime'
C NOTES:
C      uses F77/VOS like calls to read parameters from .par file
C
C USAGE:
C     call gldeadtime(lcfile,mkffile,mkfbnwidth,g2deadt,g3deadt
C    & 			,history, status)
C
C ARGUMENTS:
C       mkffile   	- Name of mkf FITS file and [ext#]
C       lcfile	 	- Name of light curve FITS file and [ext#]
C	mkfbnwidth	- Bin width from mkffile
C 	g2,3deadt	- Column names for G2,3DEAD_T
C	status  	- error number
C	history		- History keyword is written
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine uclgsb - get logical parameter
C      subroutine uclgst - get string parameter
C      subroutine uclgsd - get double precision parameter
C      subroutine uclgsr - get logical parameter
C
C******************************************************************************
      SUBROUTINE gldeadtime(lcfile,mkffile,mkfbnwidth,g2deadt,g3deadt
     & 			,history, status)

      character*(*) lcfile, mkffile,mkfbnwidth, g2deadt, g3deadt
      character(80) context
      integer  status
      logical history


C get the name of the light curve fits file to be modified
	call uclgst('lcfile',lcfile,status)
 	if (status .ne. 0) then
	    context = 'could not get lcfile parameter'
	    call fcerr(context)
	    go to 999
         endif

C get the name of the mkf fits file
	call uclgst('mkffile',mkffile,status)
	if (status .ne. 0) then
	    context = 'could not get mkffile parameter'
	    call fcerr(context)
	    go to 999
         endif


C get the name of the BN_WIDTH column in the mkf file
 	call uclgst('mkfbnwidth',mkfbnwidth,status)
	if (status .ne. 0) then
	    context = 'could not get BN_WIDTH parameter in mkffile'
	    call fcerr(context)
	    go to 999
	endif

C get the name of the G2_DEADT column in the mkf file
 	call uclgst('g2deadt',g2deadt,status)
	if (status .ne. 0) then
	    context = 'could not get G2_DEADT  parameter'
	    call fcerr(context)
	    go to 999
	endif

C get the name of the G3_DEADT column in the mkf file
 	call uclgst('g3deadt',g3deadt,status)
	if (status .ne. 0) then
	    context = 'could not get G3_DEADT parameter'
	    call fcerr(context)
	    go to 999
	endif

C  get the history record flag
      call uclgsb('history',history,status)
      if (status .ne. 0) then
         context = 'could not get history flag'
         call fcerr(context)
         goto 999
      endif

999	continue
	end
C*****************************************************************************
C     SUBROUTINE:
C     ldeadtime
C
C     DESCRIPTION:
C     Calculate the exposure correction
C
C     AUTHOR:
C     Srilal Weera, Version 1.0 03/96
C
C     MODIFICATION HISTORY:
C     AUTHOR:
C     Ken Ebisawa, Version 1.1 May-=20-1996
C     The name is change from 'lexpcor' to 'ldeadtime'
C     In addition to filling the FRACEXP column, the RATE and
C     ERROR columns are divided by the FRACEXP values; i.e.
C     deadtime correction is carried out within this task.
C
C     NOTES:
C
C     USAGE:
C     call ldeadtime(lcfile,mkffile, mkfbnwidth, g2deadt, g3deadt
C     &		,history, status)
C
C     ARGUMENTS:
C     mkffile   	- Name of mkf FITS file and [ext#]
C     lcfile	 	- Name of light curve FITS file and [ext#]
C     mkfbnwidth	- Bin width from mkffile
C     g2,3deadt	- Column names for G2,3DEAD_T
C     status  	- error number
C     history		- History keyword is written
C
C
C     PRIMARY LOCAL VARIABLES:
C
C     timezero	- Start of light curve
C     binwidth  	- Light curve binwidth
C     fracexp   	- Fractional exposure column
C     instrume   	- GIS2 or GIS3
C     mkftime		- Time from mkffile
C     lctime		- Time from lcfile
C     col1,2,3,4mkf	- columns read from the mkffile (defined in frcexp)
C     col1,4lc	- columns read from the lcfile (defined in frcexp)
C     nrows1		- number of rows in mkffile extension
C     nrows2		- number of rows in lcfile extension
C     iunit1		- unit number assigned to mkffile
C     iunit2		- unit number assigned to lcfile
C     context   	- error message context string
C     status   	- error number
C
C     CALLED ROUTINES:
C     function   fcstln - return length of character string (integer)
C     function   frcexp - calculate the exposure correction
C     subroutine fcerr  - report error to STDERR
C     subroutine fcerrm - report an error number to terminal
C     subroutine fcpars - parse off filename and extension number
C     subroutine ftclos - close a FITS file
C     subroutine ftfiou - deallocate an unused I/O unit number
C     subroutine ftgiou - allocate an unused I/O unit number
C     subroutine ftgkys - get string keyword value
C     subroutine ftgkyj - get integer keyword value
C     subroutine ftgcno - get column number corresponding to the column name
C     subroutine ftmahd - absolute move to FITS header
C     subroutine ftopen - open a FITS file
C     subroutine ftphis - put history keyword record
C     subroutine timestamp - put timestamp in the history keyword
C
C     NOTES
C     1 refers to the mkffile
C     2 refers to the lcfile
C     e.g. 'nrows2' is the number of rows from lcfile.
C
C*****************************************************************************

      SUBROUTINE  ldeadtime(lcfile,mkffile, mkfbnwidth, g2deadt, g3deadt
     &     ,history, status)

      character*(*) lcfile,mkffile,mkfbnwidth, g2deadt, g3deadt
      character(80)   mkftime, lctime, fracexp, instrume
      integer col1mkf, col2mkf, col3mkf, col4mkf
      integer col1lc, col4lc
      integer rate_col, error_col
      double precision timezero, binwidth
      integer  status
      character context*80
      character(160) filename

      integer iunit1,iunit2,extnum,block,htype, nrows1, nrows2
      integer flen, fcstln
      logical history, exact
      logical rate_exist, error_exist

      character(40) taskname
      common /task/ taskname

      status = 0

      mkftime  = 'TIME'
      lctime   = 'TIME'
      fracexp  = 'FRACEXP'
      instrume = 'INSTRUME'

C     open the mkffile, with read-only access
      call ftgiou(iunit1,status)
      call fcpars(mkffile, filename, extnum, status)
      if (extnum .eq. -99) extnum = 1
      call ftopen(iunit1, filename, 0, block, status)
      if (status .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         go to 900
      endif

C     move to the extension (assume first extension if none specified)
      if (extnum .eq. -99) extnum = 1
      call ftmahd(iunit1,extnum+1,htype,status)
      if (status .ne. 0) then
         context = 'unable to move to extension in infile'
         call fcerr(context)
         go to 999
      endif

C     make sure this is an ASCII or binary table
      if (htype .eq. 0)then
         context = 'input extension is not a table'
         call fcerr(context)
         go to 999
      endif

C     get the number of rows  from naxis2 keyword
      call ftgkyj (iunit1, 'NAXIS2', nrows1, context, status)
      if (status .ne. 0) then
         context = ' Error determining number of rows in input file'
         call fcerr (context)
         goto 999
      endif

C     get column numbers corresponding to the column names
C     for variables read from mkffile and pass them on to the
C     FRCEXP subroutine

C     get the column number corresponding to the column name
      exact=.false.
      call ftgcno(iunit1,exact,mkftime,col1mkf,status)
      if (status .gt. 0 .or. col1mkf .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 999
      end if

C     get the column number corresponding to the column name
      exact=.false.
      call ftgcno(iunit1,exact,mkfbnwidth,col2mkf,status)
      if (status .gt. 0 .or. col2mkf .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 999
      end if

C     get the column number corresponding to the column name
      exact=.false.
      call ftgcno(iunit1,exact,g2deadt,col3mkf,status)
      if (status .gt. 0 .or. col3mkf .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 999
      end if

C     get the column number corresponding to the column name
      exact=.false.
      call ftgcno(iunit1,exact,g3deadt,col4mkf,status)
      if (status .gt. 0 .or. col4mkf .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 999
      end if

C     Next open the  lcfile, with read write access
      call ftgiou(iunit2,status)
      call fcpars(lcfile, filename, extnum, status)
      if (extnum .eq. -99) extnum = 1
      call ftopen(iunit2, filename, 1, block, status)
      if (status .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         go to 900
      endif

C     move to the extension (assume first extension if none specified)
      if (extnum .eq. -99) extnum = 1
      call ftmahd(iunit2,extnum+1,htype,status)
      if (status .ne. 0) then
         context = 'unable to move to extension in infile'
         call fcerr(context)
         go to 999
      endif

C     make sure this is an ASCII or binary table
      if (htype .eq. 0)then
         context = 'input extension is not a table'
         call fcerr(context)
         go to 999
      endif

C     get the start time (timezero) from TIMEZERO keyword
      call ftgkyd (iunit2, 'TIMEZERO', timezero , context, status)
      if (status .ne. 0) then
         context = ' Error getting TIMEZERO keyword'
         call fcerr (context)
         goto 999
      endif

C     get the binning factor (binwidth) from TIMEDEL keyword
      call ftgkyd (iunit2, 'TIMEDEL', binwidth , context, status)
      if (status .ne. 0) then
         context = ' Error getting TIMEDEL keyword'
         call fcerr (context)
         goto 999
      endif

C     get the number of rows  from naxis2 keyword
      call ftgkyj (iunit2, 'NAXIS2', nrows2, context, status)
      if (status .ne. 0) then
         context = ' Error determining number of rows in input file'
         call fcerr (context)
         goto 999
      endif

C     get column numbers corresponding to the column names
C     for variables read from lcfile and pass them on to the
C     FRCEXP subroutine

C     get the column number corresponding to the column name
      exact=.false.
      call ftgcno(iunit2,exact,lctime,col1lc,status)
      if (status .gt. 0 .or. col1lc .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 999
      end if

C     get the column number corresponding to the column name
      exact=.false.
      call ftgcno(iunit2,exact,fracexp,col4lc,status)
      if (status .gt. 0 .or. col4lc .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 999
      end if

C     get the RATE and ERROR column numbers.
      exact=.false.
      call ftgcno(iunit2,exact,'RATE',rate_col,status)
      if (status .eq. 0 .and. rate_col .ne. 0)then
         rate_exist=.true.
      end if

      exact=.false.
      call ftgcno(iunit2,exact,'ERROR',error_col,status)
      if (status .eq. 0 .and. error_col .ne. 0)then
         error_exist=.true.
      end if

C     get the instrument name from INSTRUME keyword (GIS2 or GIS3)
      exact=.false.
      call ftgkys(iunit2,'INSTRUME',instrume,context,status)
      if (status .ne. 0) then
         context = ' Error reading INSTRUME keyword in lcfile'
         call fcerr(context)
         go to 999
      end if

C     call FRCEXP subroutine to calculate the exposure correction
C     and write it back to the FRACEXP column in the original lcfile.
C     In addition, the RATE and ERROR columns, if any, are divided by the
C     FRACEXP values (deadtime correction).
      call frcexp(iunit1, iunit2, nrows1, nrows2,
     &     timezero, binwidth, instrume, col1mkf, col2mkf,
     $     col3mkf, col4mkf, col1lc, col4lc, rate_exist, error_exist,
     $     rate_col, error_col)

C     Write the HISTORY keyword to the lcfile
      flen = fcstln(filename)
      context = 'TASK: '//taskname(1:14)//
     &     ' on FILENAME: '//filename(1:flen)
      if (history) then
         call ftphis(iunit2,context,status)
         call timestamp(iunit2)
         write(context,'(a,a,a)')
     $        'FRACEXP column was filled by ', taskname(1:14),'.'
         call ftphis(iunit2,context,status)
         context='FRACEXP values tell the livetime fractions'//
     $        ' for each time-bin.'
         call ftphis(iunit2,context,status)
         if (rate_exist) then
            context='RATE columns were overwritten so that'
            call ftphis(iunit2,context,status)
            context='new RATE values = (old RATE values)/FRACEXP.'
            call ftphis(iunit2,context,status)
         endif
         if (error_exist) then
            context='ERROR columns were overwritten so that'
            call ftphis(iunit2,context,status)
            context='new ERROR values = (old ERROR values)/FRACEXP.'
            call ftphis(iunit2,context,status)
         endif
      endif

C     Close the  files
 999  continue
      call ftclos(iunit1,status)
      call ftclos(iunit2,status)

 900  continue
C     Deallocate the unit numbers assigned
      call ftfiou(iunit1,status)
      call ftfiou(iunit2,status)

      return
      end

C*****************************************************************************
C     SUBROUTINE:
C     frcexp
C
C     DESCRIPTION:
C     Calculate the exposure correction and write it back to the FRACEXP
C     column in the lcfile.
C
C     AUTHORS:
C     Srilal Weera, Ken Ebisawa
C
C     MODIFICATION HISTORY:
C     Ken Ebisawa May-20-1996
C     In addition to filling the FRACEXP column, the RATE and
C     ERROR columns are divided by the FRACEXP values; i.e.
C     deadtime correction is carried out within this task.
C     There were several arguments which were passed but not used, and
C     these were removed.

C     NOTES:
C     This routine calculates deadtime correction for both GIS2 and GIS3.
C     The FRACEXP column is replaced with either dead2 or dead3 array
C     based on the value of `instrume' keyword.
C
      SUBROUTINE frcexp(iunit1, iunit2, nrows1, nrows2,
     &     timezero, binwidth, instrume, col1mkf, col2mkf, col3mkf,
     $     col4mkf, col1lc, col4lc, rate_exist, error_exist,
     $     rate_col, error_col)
      integer iunit1,iunit2,nrows1, nrows2
      double precision timezero, binwidth
      character*(*)	 instrume
      integer col1mkf, col2mkf, col3mkf, col4mkf
      integer col1lc,  col4lc
      logical rate_exist, error_exist
      integer rate_col, error_col

C     EXPLANATION OF ARGUMENTS:
C     All the valuables are 'IN' valuables:
C     iunit1		- unit number assigned to mkffile
C     iunit2		- unit number assigned to lcfile
C     nrows1		- number of rows in mkffile extension
C     nrows2		- number of rows in lcfile extension
C     timezero	        - Start time of the light curve
C     binwidth  	- Light curve binwidth
C     instrume   	- GIS2 or GIS3
C     col1mkf, col2mkf, col3mkf,col4mkf
C     - column numbers in the mkffile for
C     - TIME, BN_WIDTH, G2_DEADT and G3_DEADT, respectively.
C     col1lc,col4lc     - column numbers in the light curve file for
C     - TIME, FRACEXP respectively.
C     rate_exist, error_exist
C     - logical variables telling if RATE and ERROR columns
C     exists in the light curve file, respectively.
c     rate_col, error_col
C     - column numbers in the light curve file for
C     RATE and ERROR, respectively.
C
C     PRIMARY LOCAL VARIABLES:
C
C     dead2,3		- array to hold G2,3_DEADT column mkffile
C     start  		- array to hold TIME column from mkffile
C     widthmkf	- array to hold BN_WIDTH column from mkffile
C     dead2		- array to hold G2_DEADT column mkffile
C     dead3		- array to hold G3_DEADT column mkffile
C     fracd2		- array containing corrected FRACEXP values for GIS2
C     fracd3		- array containing corrected FRACEXP values for GIS3
C     bintime		- array to hold TIME column from lcfile
C     exposure	- array to hold FRACEXP column from lcfile
C     binstart	- start time array
C     binend  	- end   time array
C     st_bin		- start binwidth number
C     en_bin		- end binwidth number
C     context   	- error message context string
C     status   	- error number
C
C     CALLED ROUTINES:
C     subroutine fcerr  - report error to STDERR
C     subroutine ftgcvd - read double precision data column
C     subroutine ftgcve - read real data column
C     subroutine ftpcle - write real data column
C
C     **************************************************************************
      character context*80
      integer max_array_size
      parameter (max_array_size=5000)
      double precision start(max_array_size), endt(max_array_size)
      double precision bintime(max_array_size),binstart(max_array_size),
     $     binend(max_array_size)
      real dead2(max_array_size), dead3(max_array_size),
     $     widthmkf(max_array_size)
      real exposure(max_array_size), rate(max_array_size),
     $     error(max_array_size)
      real fracd2(max_array_size), fracd3(max_array_size),
     $     fracd(max_array_size)


      integer  status, i, j
      integer st_bin, en_bin
      logical anyf

      status = 0

C     Added initializations of st_bin and en_bin - KM, 2001 December
      st_bin = 0
      en_bin = 0

C     Read TIME column from mkffile into the array
      call ftgcvd(iunit1,col1mkf,1,1,nrows1,0.0d0,start,anyf,status)
      if (status .ne. 0) then
         context =
     &        ' Error finding TIME column in  mkffile'
         call fcerr (context)
         goto 999
      endif

C     Read BN_WIDTH column from mkffile into the array
      call ftgcve(iunit1,col2mkf,1,1,nrows1,0.,widthmkf,anyf,status)
      if (status .ne. 0) then
         context =
     &        ' Error finding BN_WIDTH column in  mkffile'
         call fcerr (context)
         goto 999
      endif

C     Read G2_DEADT column from mkffile into the array
      call ftgcve(iunit1,col3mkf,1,1,nrows1,0.,dead2,anyf,status)
      if (status .ne. 0) then
         context =
     &        ' Error finding G2_DEADT  column in  mkffile'
         call fcerr (context)
         goto 999
      endif

C     Read G3_DEADT column from mkffile into the array
      call ftgcve(iunit1,col4mkf,1,1,nrows1,0.,dead3,anyf,status)
      if (status .ne. 0) then
         context =
     &        ' Error finding G3_DEADT  column in  mkffile'
         call fcerr (context)
         goto 999
      endif

      do 15 i = 1, nrows1
         endt(i) = start(i) + dble(widthmkf(i))
 15   continue

C     Read TIME column from lcfile into the array
      call ftgcvd(iunit2,col1lc,1,1,nrows2,0.0d0,bintime,anyf,status)
      if (status .ne. 0) then
         context =
     &        ' Error finding TIME column in lcfile'
         call fcerr (context)
         goto 999
      endif

C     Read FRACEXP column from lcfile into the array
      call ftgcve(iunit2,col4lc,1,1,nrows2,0.,exposure,anyf,status)
      if (status .ne. 0) then
         context =
     &        ' Error finding FRACEXP column in lcfile'
         call fcerr (context)
         goto 999
      endif

C     Read RATE column from lcfile into the array
      if (rate_exist) then
         call ftgcve(iunit2,rate_col,1,1,nrows2,0.,rate,anyf,status)
         if (status .ne. 0) then
            context =
     &           ' Error finding RATE column in lcfile'
            call fcerr (context)
            goto 999
         endif
      endif

C     Read ERROR column from lcfile into the array
      if(error_exist) then
         call ftgcve(iunit2,error_col,1,1,nrows2,0.,error,anyf,status)
         if (status .ne. 0) then
            context =
     &           ' Error finding ERROR column in lcfile'
            call fcerr (context)
            goto 999
         endif
      endif

C     Index 'j' is for the no. of records in the lcfile (nrows2).
      do  25 j = 1, nrows2
         binstart(j) = timezero + bintime(j)
         binend(j)   = timezero + bintime(j) + binwidth


C     Index 'i' is for the no. of records in the mkffile (nrows1).
         do  35 i = 1, nrows1
            if(binstart(j) .gt. start(i).and.binstart(j) .le.endt(i))
     &           st_bin=i
            if(binend(j).gt.start(i).and.binend(j).le.endt(i))
     &           en_bin = i
 35      continue

C     calculate average deadtime
         if(st_bin .eq. en_bin) then
            fracd2(j) = 1.0 - dead2(st_bin)
            fracd3(j) = 1.0 - dead3(st_bin)
         elseif (en_bin .eq. st_bin +1) then
            fracd2(j) =  1.0 - ((
     &           (binend(j) - start(en_bin))*dead2(en_bin) +
     &           (endt(st_bin) - binstart(j))*dead2(st_bin))
     &           /(binend(j)-binstart(j)))

            fracd3(j) = 1.0 - ((
     &           (binend(j)-start(en_bin))*dead3(en_bin) +
     &           (endt(st_bin) - binstart(j))*dead3(st_bin))
     &           /(binend(j)-binstart(j)))

C     print*,'frac2j = ',fracd2(j)
C     print*,'frac3j = ',fracd3(j)
         else
            context =  ' Error in calculating deadtime fraction'
            call fcerr (context)
            goto 999
         endif

 25   continue

C     Depending on whether GIS2 or GIS3 is specified,
C     calculate the new RATE and ERROR values.
      if (instrume .eq. 'GIS2') then
         do i = 1, nrows2
            fracd(i)=fracd2(i)
         end do
      elseif (instrume .eq. 'GIS3') then
         do i = 1, nrows2
            fracd(i)=fracd3(i)
         end do
      else
         context =  ' Error reading INSTRUME keyword'
         call fcerr (context)
         goto 999
      endif

      do i = 1, nrows2
         if(fracd(i).gt.0.0) then
            rate(i) = rate(i)/fracd(i)
            error(i) = error(i)/fracd(i)
         else
            write(context,'(a,i12,a)')
     $           'WARNING: FRACEXP=0.0!! at the row', i,
     $           ' in the lcurve file.'
            call fcecho(context)
            write(context,'(a)')
     $           'WARNING: RATE and ERROR are set to -999.0.'
            call fcecho(context)
            rate(i) = -999.0
            error(i) = -999.0
         endif
      end do

C     Replace the  FRACEXP, RATE and ERROR columns with new values.
      call ftpcle(iunit2,col4lc,1,1,nrows2,fracd,status)
      if (status .ne. 0) then
         context =
     &        ' Error modifying FRACEXP column in the lcfile'
         call fcerr (context)
         goto 999
      endif

      if(rate_exist) then
         call ftpcle(iunit2,rate_col,1,1,nrows2,rate,status)
         if (status .ne. 0) then
            context =
     &           ' Error modifying RATE column in the lcfile'
            call fcerr (context)
            goto 999
         endif
      endif
      if(error_exist) then
         call ftpcle(iunit2,error_col,1,1,nrows2,error,status)
         if (status .ne. 0) then
            context =
     &           ' Error modifying ERROR column in the lcfile'
            call fcerr (context)
            goto 999
         endif
      endif

 999  continue
      return
      end
