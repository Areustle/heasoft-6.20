C******************************************************************************
C SELECTOR TASK:
C      fsort
C
C FILE:
C      fsort.f
C
C DESCRIPTION:
C      sorts rows from a FITS file for a particular expression
C
C AUTHOR/DATE:
C      Kent Blackburn  9/1/92
C
C MODIFICATION HISTORY:
C      9/9/94 EAG 3.0a - clear FITSIO stack, etc.
C     11/30/1995 JRG 3.0c - ffsort: Fixed TTYPE.
C      2/25/96 (Srilal) 3.0d - timestamp added
C      9/23/97 PDW 3.1a - unique option added, maxcl->999
C     10/15/97 PDW 3.1b - Replace old get header routines
C     11/13/97 PDW 3.1c - Logical error comparing complex fixed in doinsort
C     10/21/98 NG  3.1d - redefined the null values(jnul,dnul,cnul,mnul) 
C      6/13/11 BKI 3.1e - Allow for a column with a single row, i.e only call fcb2s (which wraps
C                         to fits_read_colnull) a second time if there are rows beyond the first.
C      1/27/12 BKI 3.1f - Do not call sorting routines if nrows < 2
C
C NOTES:
C      fsort supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fsort
C      IRAF: task fsort
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      columns - column name for first sort index
C      method  - method for sorting
C      ascend  - boolean flag for ascend or desend sort
C      unique  - boolean flag for purging rows with identical keys
C
C CALLED ROUTINES:
C      subroutine gsort - gets parameters from parameter file
C      subroutine ffsort - sort out rows
C
C******************************************************************************

      subroutine fsort
      character(160) infile
      character(80)  columns
      character(40)  method
      logical       ascend
      logical       unique
      character(40) taskname
      common /task/ taskname

      call ftcmsg

      taskname = 'fsort3.1f'
C  get parameters from parameter file
      call gsort(infile,columns,method,ascend,unique)

C  extract data to new FITS file
      call ffsort(infile,columns,method,ascend,unique)

      return
      end


C*****************************************************************************
C SUBROUTINE:
C      gsort
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Kent Blackburn  9/1/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       gsort uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gsort(infile,columns,method,ascend,unique)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      columns - column name for first sort index
C      method  - method for sorting
C      ascend  - boolean flag for ascend or desend sort
C      unique  - boolean flag for purging rows with identical keys
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************

      subroutine gsort(infile,columns,method,ascend,unique)
      character*(*) infile, columns, method
      logical       ascend,unique

      character(80) context
      integer status

C  initialize variables
      status = 0

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the columns
      call uclgst('columns',columns,status)
      if (status .ne. 0) then
         context = 'could not get COLUMNS parameter'
         call fcerr(context)
         goto 999
      endif

C  get the method for sorting
      call uclgst('method',method,status)
      if (status .ne. 0) then
         context = 'could not get Method parameter'
         call fcerr(context)
         goto 999
      endif

C  get the value of the ascend flag
      call uclgsb('ascend',ascend,status)
      if (status .ne. 0) then
         context = 'could not get ASCEND parameter'
         call fcerr(context)
         goto 999
      endif

C  get the value of the unique flag
      call uclgsb('unique',unique,status)
      if (status .ne. 0) then
         context = 'could not get UNIQUE parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         stop
      endif

      return
      end

C******************************************************************************
C SUBROUTINE:
C      ffsort
C
C DESCRIPTION:
C      sort rows from a FITS file extension
C
C AUTHOR/DATE:
C      J. Kent Blackburn 2/3/92
C
C MODIFICATION HISTORY:
C
C   Jeff Guerber 11/30/1995 - Changed TTYPE to char*40.
C   Peter Wilson 09/23/1997 - Replaced sort code and added
C                                 unique option (ie, sort -u)
C
C NOTES:
C
C
C USAGE:
C      call ffsort(infile,columns,method,ascend,unique)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      columns - column name for first sort index
C      method  - method for sorting
C      ascend  - boolean flag for ascending sort
C      unique  - boolean flag for purging rows with identical keys
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      ftstat  - fitsio library call return status
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      history - history string
C      maxcl   - maximum number of columns supported by software
C      simple  - FITS primary header flag
C      extend  - FITS extension header flag
C      exact   - FITS keyword match flag
C      inopen  - input file open flag
C      bool    - expression evaluates to boolean flag
C      extnum  - extension number in input FITS file
C      iunit   - input file unit number
C      record  - string containing contents of one line from eunit file
C      block   - fits file block size
C      htype   - fits file header type
C      bitpix  - number of bits per pixel in primary array
C      naxis   - number of dimensions of in array
C      naxes   - number of points along each dimension
C      pcount  - value of pcount keyword
C      gcount  - value of gcount keyword
C      rowlen  - length of FITS table in bytes
C      nrows   - number of records in FITS table
C      tfields - total number of columns in FITS table
C      varidat - size in bytes of variable data area
C      clnum   - column number
C      ttype   - array of column names
C      tform   - array of column formats
C      tunit   - array of column units
C      tbcol   - column number of first char in each field
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine ffsort(infile,columns,method,ascend,unique)

      character*(*) infile,columns,method
      logical ascend,unique
      integer maxcl
      parameter ( maxcl = 999 )
      character(160) fname
      character(80) context,errstr,comment
      character(80) history
      logical exact,inopen,negflag,goodlist
      integer extnum,ftstat,iunit,block,htype
      integer i,j,rowlen,nrows
      integer tfields,varidat,wd,tcols,rstart,rstop(maxcl),rstop1
      character(40) ttype(maxcl)
      character(16) tform(maxcl)
      character(25) tunit(maxcl)
      character(70) extname
      character(40) colist(maxcl)
      integer tbcol(maxcl)

C   initialize variables
      ftstat = 0
      iunit = 15
      block = 0
      exact = .true.
      inopen = .false.
      context = ' '
      errstr = ' '

C   make the case of the method upper case and verify validity
      call ftupch(method)
      if ((method .ne. 'HEAP') .and.
     &     (method .ne. 'INSERT')) then
         context = 'Sorting method must be HEAP or INSERT'
         call fcerr(context)
         goto 999
      endif

C   get the filename and extension
      call fcpars(infile,fname,extnum,ftstat)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

C   if the extension is less than 1 then give an error and exit
      if (extnum .lt. 1) then
         context = 'Primary extension not supported'
         call fcerr(context)
         goto 999
      endif

C   open the input FITS file
      call ftopen(iunit,fname,1,block,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'Unable to open infile'
         call fcerr(context)
         goto 999
      endif
      inopen = .true.

C   move to the extension in the input file
      call ftmrhd(iunit,extnum,htype,ftstat)
      if ( ftstat .ne. 0 ) then
         errstr = 'Error moving to extension number '
         write(context,1000) errstr, extnum
 1000    format(A34,I3)
         call fcerr(context)
         goto 999
      endif

C   get extension header's keywords depending on extension type
      if ( htype .eq. 1 ) then
         call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,ftstat)
      else if ( htype .eq. 2 ) then
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstat)
      else
         context = 'Extension type not supported'
         call fcerr(context)
         goto 999
      endif

      if (columns .eq. ' ') then
         context = 'error column list names blank'
         call fcerr(context)
         goto 999
      else
         call fcgcls(columns,colist,tcols,negflag)
         call fccmpl(tcols,tfields,colist,ttype,negflag,goodlist)
         if (.not. goodlist) then
            context = 'error in column list names'
            call fcerr(context)
            goto 999
         endif
      endif

C       get the input row width
      call ftgkyj(iunit,'NAXIS1',wd,comment,ftstat)
      if ((wd .gt. 100000).and.(method .eq. 'INSERT')) then
         context = 'table width too large for this sorting task'
         call fcerr(context)
         goto 999
      endif

C   Skip ahead if nrows<2 (no sorting necessary)
      if ( nrows .lt. 2 ) goto 25

C
C     PDW--Begin replacement source code (9/23/97)
C
      i = 1
      rstart = 1
      rstop(1) = nrows

C        Sort current column and row range

 10   if( method .eq. 'INSERT' ) then
         call doinsort(iunit,wd,rstart,rstop(i),colist(i),htype,
     &        tfields,ttype,tform,tbcol,ascend,ftstat)
      else
         call dohpsort(iunit,wd,rstart,rstop(i),colist(i),htype,
     &        tfields,ttype,tform,tbcol,ascend,ftstat)
      endif
      if( tcols.eq.1 .and. .not.unique ) goto 25

C        Check for duplicate rows in current column

 15   rstop1 = rstop(i)
      call fdstop(iunit,rstart,colist(i),htype,tfields,ttype,
     &     tform,tbcol,rstop1,ftstat)

      if( rstop1.gt.rstart ) then

C           Duplicate range found.  Do we need to move to the next
C                column (and sort) or delete rows?

         if( i.lt.tcols ) then
            i = i+1
            rstop(i) = rstop1
            goto 10
         elseif (unique) then
            call ftdrow( iunit, rstart+1, rstop1-rstart, ftstat )
            do 16 j=1,i
               rstop(j) = rstop(j) - (rstop1-rstart)
 16         continue
            nrows = nrows - (rstop1-rstart)
            rstop1 = rstart
         endif
      endif
      rstart = rstop1+1

C        Do we need to move to a previous column?

 17   if( (rstart.ge.rstop(i)) .and. (i.gt.1) ) then
         rstart = rstop(i)+1
         i = i-1
         goto 17
      endif

C        Repeat until we've searched the entire first column
C              for duplicates

      if( rstart.lt.rstop(1) ) goto 15
 25   continue
C
C     PDW--End replacement source code (9/23/97)
C

C   write out a history record
      history = 'TASK: FSORT on FILENAME: '//fname
      call ftphis(iunit,history,ftstat)
      call timestamp(iunit)

 999  continue
      if ( ftstat .ne. 0 ) then
         call fcerrm(ftstat)
         ftstat = 0
      endif
      if ( inopen ) then
         call ftclos(iunit,ftstat)
         ftstat = 0
      endif
      return
      end
C******************************************************************************
C SUBROUTINE:
C      fdstop
C
C DESCRIPTION:
C      finds last row to have same value as specified start row in a column
C
C AUTHOR/DATE:
C      J. Kent Blackburn 9/2/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C
C USAGE:
C      call fdstop(iunit,rstart,clname,htype,tfields,ttype,
C                  tform,tbcol,rstop,ftstat)
C
C ARGUMENTS:
C      iunit    - input file unit number
C      rstart  - starting row number in search
C      clname  - column name to search in
C      htype   - header type
C      tfields - number of columns in table
C      ttype   - names of columns in table
C      tform   - array of tforms for all columns
C      tbcol   - array of table column start values for columns
C      rstop   - last sequential row to have same value as rstart
C      ftstat  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      string1 - string containing the first value
C      string2 - srting containing the second value
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine fdstop(iunit,rstart,clname,htype,tfields,ttype,
     &     tform,tbcol,rstop,ftstat)

      integer iunit,rstart,rstop,htype,tfields,ftstat
      character*(*) clname
      character(80)  tmpnam1, tmpnam2
      integer maxcl
      parameter ( maxcl = 999 )
      integer tbcol(maxcl),bcol
      character*(*) tform(maxcl),ttype(maxcl)
      integer clnumb,elem,i,l,nchars,fcafml
      character(70) form, blank
      character(1024) string1,string2

      elem = 1
      l = rstart
      string1 = ' '
      string2 = ' '

C   determine the column number from column name
      do 5 i = 1, tfields
         tmpnam1 = clname
         call ftupch(tmpnam1)
         tmpnam2 = ttype(i)
         call ftupch(tmpnam2)
c         if (clname .eq. ttype(i)) then
         if (tmpnam1 .eq. tmpnam2) then
            clnumb = i
            goto 6
         endif
 5    continue
 6    continue

C   determine the form of the data
      form = tform(clnumb)
      bcol = tbcol(clnumb)

C   get the value of the first row element in this search
      blank = ' '
      if (htype .eq. 1) then
         nchars = fcafml(form)
         call ftgtbs(iunit,l,bcol,nchars,string1,ftstat)
      else if ( htype .eq. 2 ) then
         call fcb2s(iunit,clnumb,l,elem,form,blank,string1)
      endif

C   loop until next value differs, or reach end of table

 10   continue
      if (htype .eq. 1) then
         nchars = fcafml(form)
         call ftgtbs(iunit,l+1,bcol,nchars,string2,ftstat)
      else if ( htype .eq. 2 ) then
         call fcb2s(iunit,clnumb,l+1,elem,form,blank,string2)
      endif

      if (string2 .eq. string1) then
         l = l + 1
         if (l .lt. rstop) goto 10
      else
         rstop = l
      end if

      return
      end

C******************************************************************************
C SUBROUTINE:
C      doinsort
C
C DESCRIPTION:
C      sorts through all rows within a range
C
C AUTHOR/DATE:
C      J. Kent Blackburn 9/3/92
C
C MODIFICATION HISTORY:
C      JKB: Modified 11/20/92 to fix bug in floating point data type
C           comparisons
C      PDW 11/13/97: Fixed logical bug (ge/le->gt/lt) in complex comparisons
C
C NOTES:
C
C
C USAGE:
C      call doinsort(iunit,width,rstart,rstop,clname,htype,
C   &              tfields,ttype,tform,tbcol,ascend,ftstat)
C
C ARGUMENTS:
C      iunit   - input file unit number
C      width   - width of row in FITS file
C      rstart  - starting row number in search
C      rstop   - stopping row number in search
C      clname  - column name to search in
C      htype   - header type
C      tfields - number of columns in table
C      ttype   - names of columns in table
C      tform   - array of tforms for all columns
C      tbcol   - array of table column start values for columns
C      ascend  - ascending sort flag
C      ftstat  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      repeat  - repeat factor for column element, only 1 supported
C      width   - width of column
C      fchar   - first char of row to move
C      buffer  - buffer for transferring bytes in row
C      context - error message
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine doinsort(iunit,width,rstart,rstop,clname,htype,
     &     tfields,ttype,tform,tbcol,ascend,ftstat)

      integer iunit,width,rstart,rstop,htype,tfields,ftstat
      character*(*) clname
      logical ascend
      integer maxcl,dattyp,repeat,wid,jnul,stjj,stij
      parameter ( maxcl = 999 )
      integer tbcol(maxcl),bcol
      character*(*) tform(maxcl),ttype(maxcl)
      character(16) form
      double precision dnul,stjd,stid
      integer i,j,l,elem,clnumb,fchar
      logical igej,ilej,anyfj,anyfi,stjl,stil
      character(1024) stj,sti,snul
      real stjc(2),stic(2),cnul(2)
      double precision stjm(2),stim(2),mnul(2)
      integer bufferj(25000),bufferi2l(25000)
      character(80)  tmpnam1, tmpnam2

C   initialize variables
      elem = 1
      jnul = -2147483647
      dnul = -9.11912913E-36 
      cnul(1) = -9.11E-36
      cnul(2) = -9.11E-36
      mnul(1) = -9.11912913E-36
      mnul(2) = -9.11912913E-36
      snul = ' '
      fchar = 1

C   determine the column number from column name
      do 5 i = 1, tfields
         tmpnam1 = clname
         call ftupch(tmpnam1)
         tmpnam2 = ttype(i)
         call ftupch(tmpnam2)
c         if (clname .eq. ttype(i)) then
         if (tmpnam1 .eq. tmpnam2) then
            clnumb = i
            goto 6
         endif
 5    continue
 6    continue


C   determine the form of the data
      form = tform(clnumb)
      bcol = tbcol(clnumb)
      if ( htype .eq. 1 ) then
         call fcasfm(form,dattyp,ftstat)
         repeat = 1
      else if ( htype .eq. 2 ) then
         call ftbnfm(form,dattyp,repeat,wid,ftstat)
      endif

      do 30 j = rstart+1, rstop

C   ************ GET THE STARTING VALUE ***************

         call ftgtbb(iunit,j,fchar,width,bufferj,ftstat)

C   Data Types that are logical
         if (dattyp .eq. 14) then
            call ftgcl(iunit,clnumb,j,elem,elem,stjl,ftstat)
         endif

C   Data Types that are ascii
         if (dattyp .eq. 16) then
            call ftgcvs(iunit,clnumb,j,elem,elem,snul,stj,
     &           anyfj,ftstat)
         endif

C   Data Types that can be considered integers
         if ((dattyp .eq. 1) .or. (dattyp .eq. 11) .or.
     &        (dattyp .eq. 21) .or. (dattyp .eq. 41)) then
            call ftgcvj(iunit,clnumb,j,elem,elem,jnul,stjj,
     &           anyfj,ftstat)
         endif

C   Data Types that can be considered doubles
         if ((dattyp .eq. 42) .or. (dattyp .eq. 82)) then
            call ftgcvd(iunit,clnumb,j,elem,elem,dnul,stjd,
     &           anyfj,ftstat)
         endif

C   Data Types are complex
         if (dattyp .eq. 83) then
            call ftgcvc(iunit,clnumb,j,elem,elem,cnul,stjc,
     &           anyfj,ftstat)
         endif

C   Data Types are double complex
         if (dattyp .eq. 163) then
            call ftgcvm(iunit,clnumb,j,elem,elem,mnul,stjm,
     &           anyfj,ftstat)
         endif

         do 20 i = j - 1, rstart, -1
            ilej = .false.
            igej = .false.

C   ************** GET THE COMPARE VALUE *****************

C   Data Types that are logical
            if (dattyp .eq. 14) then
               call ftgcl(iunit,clnumb,i,elem,elem,stil,ftstat)
               if ( stjl .and. (.not. stil) ) ilej = .true.
               if ( (.not. stjl) .and. stil ) igej = .true.
            endif

C   Data Types that are ascii
            if (dattyp .eq. 16) then
               call ftgcvs(iunit,clnumb,i,elem,elem,snul,sti,
     &              anyfi,ftstat)
               if ( anyfj .and. (.not. anyfi)) then
                  igej = .true.
               else if ((.not. anyfj) .and. anyfi ) then
                  ilej = .true.
               else if ((.not. anyfj) .and. (.not. anyfi)) then
                  if ( sti .le. stj ) ilej = .true.
                  if ( sti .ge. stj ) igej = .true.
               endif
            endif

C   Data Types that can be considered integers
            if ((dattyp .eq. 1) .or. (dattyp .eq. 11) .or.
     &           (dattyp .eq. 21) .or. (dattyp .eq. 41)) then
               call ftgcvj(iunit,clnumb,i,elem,elem,jnul,stij,
     &              anyfi,ftstat)
               if ( anyfj .and. (.not. anyfi)) then
                  igej = .true.
               else if ((.not. anyfj) .and. anyfi ) then
                  ilej = .true.
               else if ((.not. anyfj) .and. (.not. anyfi)) then
                  if ( stij .le. stjj ) ilej = .true.
                  if ( stij .ge. stjj ) igej = .true.
               endif
            endif

C   Data Types that can be considered doubles
            if ((dattyp .eq. 42) .or. (dattyp .eq. 82)) then
               call ftgcvd(iunit,clnumb,i,elem,elem,dnul,stid,
     &              anyfi,ftstat)
               if ( anyfj .and. (.not. anyfi)) then
                  igej = .true.
               else if ((.not. anyfj) .and. anyfi ) then
                  ilej = .true.
               else if ((.not. anyfj) .and. (.not. anyfi)) then
                  if ( stid .le. stjd ) ilej = .true.
                  if ( stid .ge. stjd ) igej = .true.
               endif
            endif

C   Data Types are complex
            if (dattyp .eq. 83) then
               call ftgcvc(iunit,clnumb,i,elem,elem,cnul,stic,
     &              anyfi,ftstat)
               if ( anyfj .and. (.not. anyfi)) then
                  igej = .true.
               else if ((.not. anyfj) .and. anyfi ) then
                  ilej = .true.
               else if ((.not. anyfj) .and. (.not. anyfi)) then
                  if ( stic(1) .lt. stjc(1) ) then
                     ilej = .true.
                  else if ( stic(1) .eq. stjc(1) ) then
                     if ( stic(2) .le. stjc(2) ) ilej = .true.
                  endif
                  if ( stic(1) .gt. stjc(1) ) then
                     igej = .true.
                  else if ( stic(1) .eq. stjc(1) ) then
                     if ( stic(2) .ge. stjc(2) ) igej = .true.
                  endif
               endif
            endif

C   Data Types are double complex
            if (dattyp .eq. 163) then
               call ftgcvm(iunit,clnumb,i,elem,elem,mnul,stim,
     &              anyfi,ftstat)
               if ( anyfj .and. (.not. anyfi)) then
                  igej = .true.
               else if ((.not. anyfj) .and. anyfi ) then
                  ilej = .true.
               else if ((.not. anyfj) .and. (.not. anyfi)) then
                  if ( stim(1) .lt. stjm(1) ) then
                     ilej = .true.
                  else if ( stim(1) .eq. stjm(1) ) then
                     if ( stim(2) .le. stjm(2) ) ilej = .true.
                  endif
                  if ( stim(1) .gt. stjm(1) ) then
                     igej = .true.
                  else if ( stim(1) .eq. stjm(1) ) then
                     if ( stim(2) .ge. stjm(2) ) igej = .true.
                  endif
               endif
            endif

C   *****************************************************

            if (ascend) then
C             sort values in ascending order
               l = i + 1
               if (ilej) then
                  goto 10
               else
                  call ftgtbb(iunit,i,fchar,width,bufferi2l,ftstat)
                  call ftptbb(iunit,l,fchar,width,bufferi2l,ftstat)
               endif
            else
C             sort values in descending order
               l = i + 1
               if (igej) then
                  goto 10
               else
                  call ftgtbb(iunit,i,fchar,width,bufferi2l,ftstat)
                  call ftptbb(iunit,l,fchar,width,bufferi2l,ftstat)
               endif
            endif
 20      continue
         l = rstart
 10      call ftptbb(iunit,l,fchar,width,bufferj,ftstat)
 30   continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      dohpsort
C
C DESCRIPTION:
C      sorts through all rows within a range
C
C AUTHOR/DATE:
C      J. Kent Blackburn 9/3/92
C
C MODIFICATION HISTORY:
C      JKB: Modified 11/20/92 to fix bug in floating point data type
C           comparisons
C
C NOTES:
C
C
C USAGE:
C      call dohpsort(iunit,width,rstart,rstop,clname,htype,
C   &              tfields,ttype,tform,tbcol,ascend,ftstat)
C
C ARGUMENTS:
C      iunit   - input file unit number
C      width   - width of row in FITS file
C      rstart  - starting row number in search
C      rstop   - stopping row number in search
C      clname  - column name to search in
C      htype   - header type
C      tfields - number of columns in table
C      ttype   - names of columns in table
C      tform   - array of tforms for all columns
C      tbcol   - array of table column start values for columns
C      ascend  - ascending sort flag
C      ftstat  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      repeat  - repeat factor for column element, only 1 supported
C      width   - width of column
C      fchar   - first char of row to move
C      buffer  - buffer for transferring bytes in row
C      context - error message
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine dohpsort(iunit,width,rstart,rstop,clname,htype,
     &     tfields,ttype,tform,tbcol,ascend,ftstat)

      integer iunit,width,rstart,rstop,htype,tfields,ftstat
      character*(*) clname
      logical ascend
      integer maxcl,dattyp,repeat,wid,jnul,stlj,stij
      parameter ( maxcl = 999 )
      integer tbcol(maxcl),bcol
      character*(*) tform(maxcl),ttype(maxcl)
      character(16) form
      real aln2i,tiny
      double precision dnul,stld,stid
      parameter (aln2i = 1.0/0.69314718, tiny = 1.0e-05)
      integer i,ii,j,k,l,ll,m,n,nn,elem,clnumb,lognb2
      logical lgti,llti,anyfl,anyfi,stll,stil
      character(1024) stl,sti,snul
      real stlc(2),stic(2),cnul(2)
      double precision stlm(2),stim(2),mnul(2)
      character(80)  tmpnam1, tmpnam2


C   initialize variables
      elem = 1
      jnul = -2147483647
      dnul = -9.11912913E-36 
      cnul(1) = -9.11E-36
      cnul(2) = -9.11E-36
      mnul(1) = -9.11912913E-36
      mnul(2) = -9.11912913E-36
      snul = ' '
      n = rstop - rstart + 1
      m = n
      lognb2 = int(alog(float(n))*aln2i+tiny)

C   determine the column number from column name
C       call ftgcno(iunit,exact,clname,clnumb,ftstat)
      do 5 i = 1, tfields
         tmpnam1 = clname
         call ftupch(tmpnam1)
         tmpnam2 = ttype(i)
         call ftupch(tmpnam2)
c         if (clname .eq. ttype(i)) then
         if (tmpnam1 .eq. tmpnam2) then
            clnumb = i
            goto 6
         endif
 5    continue
 6    continue

C   determine the form of the data
      form = tform(clnumb)
      bcol = tbcol(clnumb)
      if ( htype .eq. 1 ) then
         call fcasfm(form,dattyp,ftstat)
         repeat = 1
      else if ( htype .eq. 2 ) then
         call ftbnfm(form,dattyp,repeat,wid,ftstat)
      endif

      do 20 nn = 1,lognb2
         m = m / 2
         k = n - m
         do 10 j = 1, k
            i = j
 30         continue
            lgti = .false.
            llti = .false.
            l = i + m
            ll = rstart + l - 1
            ii = rstart + i - 1

C   *****************************************************

C   Data Types that are logical
            if (dattyp .eq. 14) then
               call ftgcl(iunit,clnumb,ll,elem,elem,stll,ftstat)
               call ftgcl(iunit,clnumb,ii,elem,elem,stil,ftstat)
               if ( (.not. stll) .and. stil ) llti = .true.
               if ( stll .and. (.not. stil) ) lgti = .true.
            endif

C   Data Types that are ascii
            if (dattyp .eq. 16) then
               call ftgcvs(iunit,clnumb,ll,elem,elem,snul,stl,
     &              anyfl,ftstat)
               call ftgcvs(iunit,clnumb,ii,elem,elem,snul,sti,
     &              anyfi,ftstat)
               if ( anyfl .and. (.not. anyfi)) then
                  llti = .true.
               else if ((.not. anyfl) .and. anyfi ) then
                  lgti = .true.
               else if ((.not. anyfl) .and. (.not. anyfi)) then
                  if ( stl .lt. sti ) llti = .true.
                  if ( stl .gt. sti ) lgti = .true.
               endif
            endif

C   Data Types that can be considered integers
            if ((dattyp .eq. 1) .or. (dattyp .eq. 11) .or.
     &           (dattyp .eq. 21) .or. (dattyp .eq. 41)) then
               call ftgcvj(iunit,clnumb,ll,elem,elem,jnul,stlj,
     &              anyfl,ftstat)
               call ftgcvj(iunit,clnumb,ii,elem,elem,jnul,stij,
     &              anyfi,ftstat)
               if ( anyfl .and. (.not. anyfi)) then
                  llti = .true.
               else if ((.not. anyfl) .and. anyfi ) then
                  lgti = .true.
               else if ((.not. anyfl) .and. (.not. anyfi)) then
                  if ( stlj .lt. stij ) llti = .true.
                  if ( stlj .gt. stij ) lgti = .true.
               endif
            endif

C   Data Types that can be considered doubles
            if ((dattyp .eq. 42) .or. (dattyp .eq. 82)) then
               call ftgcvd(iunit,clnumb,ll,elem,elem,dnul,stld,
     &              anyfl,ftstat)
               call ftgcvd(iunit,clnumb,ii,elem,elem,dnul,stid,
     &              anyfi,ftstat)
               if ( anyfl .and. (.not. anyfi)) then
                  llti = .true.
               else if ((.not. anyfl) .and. anyfi ) then
                  lgti = .true.
               else if ((.not. anyfl) .and. (.not. anyfi)) then
                  if ( stld .lt. stid ) llti = .true.
                  if ( stld .gt. stid ) lgti = .true.
               endif
            endif

C   Data Types are complex
            if (dattyp .eq. 83) then
               call ftgcvc(iunit,clnumb,ll,elem,elem,cnul,stlc,
     &              anyfl,ftstat)
               call ftgcvc(iunit,clnumb,ii,elem,elem,cnul,stic,
     &              anyfi,ftstat)
               if ( anyfl .and. (.not. anyfi)) then
                  llti = .true.
               else if ((.not. anyfl) .and. anyfi ) then
                  lgti = .true.
               else if ((.not. anyfl) .and. (.not. anyfi)) then
                  if ( stlc(1) .lt. stic(1) ) then
                     llti = .true.
                  else if ( stlc(1) .eq. stic(1) ) then
                     if ( stlc(2) .lt. stic(2) ) llti = .true.
                  endif
                  if ( stlc(1) .gt. stic(1) ) then
                     lgti = .true.
                  else if ( stlc(1) .eq. stic(1) ) then
                     if ( stlc(2) .gt. stic(2) ) lgti = .true.
                  endif
               endif
            endif

C   Data Types are double complex
            if (dattyp .eq. 163) then
               call ftgcvm(iunit,clnumb,ll,elem,elem,mnul,stlm,
     &              anyfl,ftstat)
               call ftgcvm(iunit,clnumb,ii,elem,elem,mnul,stim,
     &              anyfi,ftstat)
               if ( anyfl .and. (.not. anyfi)) then
                  llti = .true.
               else if ((.not. anyfl) .and. anyfi ) then
                  lgti = .true.
               else if ((.not. anyfl) .and. (.not. anyfi)) then
                  if ( stlm(1) .lt. stim(1) ) then
                     llti = .true.
                  else if ( stlm(1) .eq. stim(1) ) then
                     if ( stlm(2) .lt. stim(2) ) llti = .true.
                  endif
                  if ( stlm(1) .gt. stim(1) ) then
                     lgti = .true.
                  else if ( stlm(1) .eq. stim(1) ) then
                     if ( stlm(2) .gt. stim(2) ) lgti = .true.
                  endif
               endif
            endif

C   *****************************************************

            if (ascend) then
C             sort values in ascending order
               if (llti) then
                  call fswapr(iunit,ll,ii,width,ftstat)
                  i = i - m
                  if (i .ge. 1) goto 30
               endif
            else
C             sort values in descending order
               if (lgti) then
                  call fswapr(iunit,ll,ii,width,ftstat)
                  i = i - m
                  if (i .ge. 1) goto 30
               endif
            endif
 10      continue
 20   continue

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fswapr
C
C DESCRIPTION:
C      swaps two rows in a fits file
C
C AUTHOR/DATE:
C      J. Kent Blackburn 9/3/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C
C USAGE:
C      call fswapr(iunit,row1,row2,width,ftstat)
C
C ARGUMENTS:
C      iunit   - input file unit number
C      row1    - starting row number in search
C      row2    - column name to search in
C      width   - width of row in bytes
C      ftstat  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      repeat  - repeat factor for column element, only 1 supported
C      fchar   - first char of row to move
C      buffer  - buffer for transferring bytes in row
C      context - error message
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine fswapr(iunit,row1,row2,width,ftstat)

      integer iunit,row1,row2,width,ftstat
      integer buffer1(512),buffer2(512)
      integer mxbyte,fchar,remain,nchar
      parameter ( mxbyte = 2048 )

C   don't overflow buffer
      buffer1(1) = 32
      buffer2(1) = 32
      fchar = 1
      remain = width
 10   continue
      if (remain .gt. 0) then
         if (remain .gt. mxbyte) then
            nchar = mxbyte
         else
            nchar = remain
         endif
      endif

C   read in the 2 rows from the fits file in blocks
      call ftgtbb(iunit,row1,fchar,nchar,buffer1,ftstat)
      call ftgtbb(iunit,row2,fchar,nchar,buffer2,ftstat)

C   write out the 2 rows from the fits into swapped rows
      call ftptbb(iunit,row1,fchar,nchar,buffer2,ftstat)
      call ftptbb(iunit,row2,fchar,nchar,buffer1,ftstat)

      remain = remain - nchar
      fchar = fchar + nchar
      if (remain .gt. 0) goto 10

      return
      end

