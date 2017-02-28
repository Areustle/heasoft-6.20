C****************************************************************************** 
C SELECTOR TASK:
C      fltime
C
C FILE:
C      fltime.f 
C
C DESCRIPTION: 
C      Filters a FITS file using a Good Time Intervals file to create
C      a new FITS file.
C
C AUTHOR:  
C      Janice Tarrant  3/24/92
C
C MODIFICATION HISTORY:
C      February, 1993 J. Ingham
C      2/22/93 EAG - Make all times double precision
C Banashree M Seifert (2.0.0; Oct 1996)
C      . fifilt is changed and another subroutine filter is introduced
C      . DMA added
c Banashree M Seifert (2.1.0; Oct28, 1996)
c      . modified the filter subroutine so that it writes the
c        naxis2 keyword correctly and clobber parameter is taken care of
c        by changing ftinit for ffinit
c
c Banashree M Seifert (2.2.0; Mar 3, 1997)
c      . TSTART and TSTOP are inserted into the output file
c        (for this, the subroutine FILTER is changed)
c
c Banashree M Seifert (2.3.0; Oct 6, 1997)
c      . do 30 and do 40 ended with '30 enddo'. but old compilers want '30 continue'
c        similarly for 40 enddo
C
C James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 4 March, 1998
C      1. Restored standard task common block, to prevent this tool
C         from seg-faulting whenever it tries to write an error message.
C      2. Added implicit none to each subroutine, and declared any
C         variables which had been left out (such as status).
C      3. Changed a couple of fcerr calls to use errstr instead of
C         context, in places where the former had been assigned but
C         the latter had not.
C      4. Just to be paranoid, changed call to wtendm to pass
C         task_tmp, a local copy of the common variable taskname.
C         This was done to prevent any possible conflict because
C         wtendm also uses the task common block.
C
C toliver (2.5.0; 03/18/1998)
C      Revised subroutine firfwf by replacing calls to obsolete fitsio
C      routine 'ftgbnh' with calls to new cfitsio function 'ftghbn', updated
C      version number
C Ning Gan (2.6.0; 04/09/1998)
C       Increased the max number of gti from 100 to 10000.
C       Added the error check for the number of rows in gti file.
C
C Ning Gan (2.6.1; 11/02/1998)
C       Before update the TSTART and TSTOP, check whether they exist.
C       if they do, then preserve the old keywords and do nothing. 
C       Added checks for the new date format. 
C       Defined the "EXPOSURE" as the default value of the exposure parameter.
C Ning Gan (2.6.2; 11/02/1998)
C       Restored the updates of TSTART and TSTOP.    
C       Fixed the bug of calculating the TSTOP (The TSTOP was defined 
C       as the latest gti_start and should be the latest gti_stop).
C       Updated the TELAPSE for consistency.
C
C
C NOTES:
C      fltime supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      column  - column name for time
C      obsdate - observation start date keyword
C      obstime - observation start time keyword
C      gtifile - GTI file name
C      gticols - column names for start and stop times
C      gtidate - GTI observation start date keyword
C      gtitime - GTI observation start time keyword
C      outfile - output file name
C      exposure - exposure keyword name
C      copyall - copy all other extensions flag
C
C CALLED ROUTINES:
C      subroutine gfltie - gets parameters from environment
C      subroutine firfwf - read input FITS file and write filtered file
C
C****************************************************************************** 
        subroutine fltime
      implicit none
        character(160) infile,outfile, gtifile
      character(80) column, obsdate, obstime,
     &               gticols,gtidate,gtitime,exposure
      logical copyall

C jp 4-mar-1998: Declare task CB and local copy of taskname (task_tmp).
C                Also declare status, which was implicitly a real.
      character(40) taskname
      common /task/ taskname
      character(40) task_tmp
      integer status

      character(5) version

      taskname = 'fltime'
      version = '2.6.2'
      infile = ' '
      column = ' '
      obsdate = ' '
      obstime = ' '
      gtifile = ' '
      gticols = ' '
      gtidate = ' '
      gtitime = ' '
      outfile = ' '
      exposure = ' '
      status = 0

C  get the parameters from the par file
      call gfltie(infile,column,obsdate,obstime,gtifile,gticols,
     &              gtidate,gtitime,outfile,exposure,copyall)

C  read in the FITS file and write out the filtered file
      call firfwf(infile,column,obsdate,obstime,gtifile,gticols,
     &              gtidate,gtitime,outfile,exposure,copyall)

C jp 4-mar-1998: Pass task_tmp, not taskname, since wtendm uses
C                the task CB, and may not like the same address
C                also being passed as an argument.
      task_tmp = taskname
      call wtendm(task_tmp,version,status,0)

      return
      end



C****************************************************************************** 
C SUBROUTINE:
C      gfltie
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Janice Tarrant  3/24/92
C
C MODIFICATION HISTORY:
C Banashree M Seifert (2.0.0; Oct 1996)
C      . added clobber parameter
C
C NOTES:
C      gfltie uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gfltie(infile,column,obsdate,obstime,gtifile,gticols,gtidate,
C                  gtitime,outfile,exposure,copyall)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      column  - column name for time
C      obsdate - observation start date keyword
C      obstime - observation start time keyword
C      gtifile - GTI file name
C      gticols - column names for start and stop times
C      gtidate - GTI observation start date keyword
C      gtitime - GTI observation start time keyword
C      outfile - output file name
C      exposure - exposure keyword name
C      copyall - copy all other extensions flag
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsb - get boolean parameter
C
C****************************************************************************** 
      subroutine gfltie(infile,column,obsdate,obstime,gtifile,
     &                        gticols,gtidate,gtitime,outfile,exposure,
     &                    copyall)

      implicit none

      character*(*) infile, column, obsdate, obstime, gtifile,
     &             gticols, gtidate, gtitime, outfile, exposure

      logical copyall,killit
      character(80) context
      integer status

c --------------------- internals ---------------------------------------

      character(7) subname
      parameter (subname='gfltie')
      character(5) version
      parameter (version='2.0.0')

C  initialize variables
      status = 0

c read in clobber
      call uclgsb('clobber',killit,status)
      if (status .ne. 0) then
          context = 'Getting clobber parameter'
          call wterrm(subname,version,context)
          goto 999
      endif

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
          context = 'could not get INFILE parameter'
          call fcerr(context)
          goto 999
      endif

C  get the name of the GTI file
      call uclgst('gtifile',gtifile,status)
      if (status .ne. 0) then
          context = 'could not get GTIFILE parameter'
          call fcerr(context)
          goto 999
      endif

C  get the name of the output ASCII file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
          context = 'could not get OUTFILE parameter'
          call fcerr(context)
          goto 999
      endif

C  get the column name for the time
      call uclgst('column',column,status)
      if (status .ne. 0) then
          context = 'could not get COLUMN parameter'
          call fcerr(context)
          goto 999
      endif

C  get the observation date keyword
      call uclgst('obsdate',obsdate,status)
      if (status .ne. 0) then
          context = 'could not get OBSDATE parameter'
          call fcerr(context)
          goto 999
      endif

C  get the observation time keyword
      call uclgst('obstime',obstime,status)
      if (status .ne. 0) then
          context = 'could not get OBSTIME parameter'
          call fcerr(context)
          goto 999
      endif

C  get the column names for start and stop times
      call uclgst('gticols',gticols,status)
      if (status .ne. 0) then
          context = 'could not get GTICOLS parameter'
          call fcerr(context)
          goto 999
      endif

C  get the GTI observation date keyword
      call uclgst('gtidate',gtidate,status)
      if (status .ne. 0) then
          context = 'could not get GTIDATE parameter'
          call fcerr(context)
          goto 999
      endif

C  get the GTI observation time keyword
      call uclgst('gtitime',gtitime,status)
      if (status .ne. 0) then
          context = 'could not get GTITIME parameter'
          call fcerr(context)
          goto 999
      endif

C  get the EXPOSURE time keyword
      call uclgst('exposure',exposure,status)
      if (status .ne. 0) then
          context = 'could not get EXPOSURE parameter'
          call fcerr(context)
          goto 999
      endif

C  get the copy all extensions flag
      call uclgsb('copyall',copyall,status)
      if (status .ne. 0) then
          context = 'could not get COPYALL parameter'
          call fcerr(context)
          goto 999
      endif

999      continue
      if (status .ne. 0)  call fcerrm(status)

      return
      end



C****************************************************************************** 
C SUBROUTINE:
C      firfwf
C
C DESCRIPTION: 
C      Reads the FITS file and writes the GTI filtered values to a new
C      FITS file
C
C AUTHOR:  
C      Janice Tarrant  3/24/92
C
C MODIFICATION HISTORY:
C
C toliver (03/18/1998)
C      Replaced calls to obsolete fitsio routine 'ftgbnh' with calls to new
C      cfitsio function 'ftghbn'
C
C NOTES:
C      firfwf uses FITSIO calls to read FITS file
C      vector elements in tables not supported
C
C USAGE:
C      call firfwf(infile,column,obsdate,obstime,gtifile,gticols,gtidate,
C                  gtitime,outfile,exposure,copyall)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      column  - column name for time
C      obsdate - observation start date keyword
C      obstime - observation start time keyword
C      gtifile - GTI file name
C      gticols - column names for start and stop times
C      gtidate - GTI observation start date keyword
C      gtitime - GTI observation start time keyword
C      outfile - output file name
C      exposure - exposure keyword name
C      copyall - copy all other extensions flag
C
C PRIMARY LOCAL VARIABLES:
C      maxcl    - maximum number of columns
C      filename - name of FITS file
C      extnum   - FITS file extension number
C      ftstatus - FITSIO error number
C      context  - error message 
C      negflag  - exclude name flag
C      goodlist - good name list flag
C      gtilist  - GTI column names list
C      tcols    - number of columns in list
C      odate    - observation start date
C      otime    - observation start time
C      gdate    - GTI observation start date
C      gtime    - GTI observation start time
C      ooffs    - observation time offset
C      goffs    - GTI observation time offset
C
C CALLED ROUTINES:
C      subroutine fccmpl - compare two lists for a subset
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcgcls - get column list based on parameter
C      subroutine fcpars - parse off filename and extension number
C      subroutine fctofs- get the observation and GTI time offsets
C      subroutine fifilt - filter FITS file using GTI
C      subroutine ftadef - define ASCII extension
C      subroutine ftbdef - define binary extension
C      subroutine ftclos - close a FITS file
C      subroutine ftcrhd - create a new header
C      subroutine ftghbn - get BINARY table header
C      subroutine ftgcno - get column number associated with name
C      subroutine ftgkys - get string keyword value
C      subroutine ftgprh - get primary header keywords from CHU
C      subroutine ftgtbh - get ASCII table header
C      subroutine ftinit - create a new FITS file
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftmrhd - relative move to FITS header
C      subroutine ftopen - open a FITS file
C      subroutine ftphbn - put binary table header keywords into CHU
C      subroutine ftpdef - define structure of primary array
C      subroutine ftphis - put history keyword record
C      subroutine ftphpr - put primary header keywords into CHU
C      subroutine ftphtb - put ASCII table header keywords into CHU
C
C****************************************************************************** 
      subroutine firfwf(infile,column,obsdate,obstime,gtifile,
     &     gticols,gtidate,gtitime,outfile,
     &     exposure,copyall)
      implicit none
      character*(*) infile, column, obsdate, obstime, gtifile,
     &     gticols,gtidate,gtitime,outfile,exposure
      logical copyall

      integer maxcl
      parameter (maxcl = 512)
      character(160) filename, context, gtiname
      character(80) errstr
      character(70) ttype(maxcl), tform(maxcl), tunit(maxcl),extname,
     &     odate, otime, comment, gtype(maxcl), gform(maxcl), 
     &     gunit(maxcl), gextname, gtilist(2), gdate, gtime,
     &     ndate, ntime
      logical inopen, gtiopen, outopen, exact, negflag, goodlist,
     &     simple, extend, dates, julian
      integer i, ftstatus, iunit, gtiunit, ounit,block,extnum,htype,
     &     nrows, rowlen, tfields, tbcol(maxcl), varidat, tcols,
     &     colnum, ghtype, gnrows, growlen, gfields, gbcol(maxcl), 
     &     gvaridat, gtinum(2), bitpix, naxis, naxes(99), pcount, 
     &     gcount, gtiext, mkywd, length, fcstln
      double precision ooffs, goffs
      real tmpnum

C  initialize variables
      ftstatus = 0
      iunit = 15
      gtiunit = 16
      ounit = 17
      inopen = .false.
      gtiopen = .false.
      outopen = .false.
      negflag = .false.
      exact = .false.

C  get the filename and extension number
      call fcpars(infile,filename,extnum,ftstatus)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

C  if extension is 0 then give error and exit
      if (extnum .eq. 0) then
         context = 'primary extension not supported for INPUT file'
         call fcerr(context)
         goto 999
      endif

C  get the GTI filename and extension number
      call fcpars(gtifile,gtiname,gtiext,ftstatus)

C EAG 8/25/93 default to 1st extension
      if (gtiext .eq. -99) gtiext = 1

C  if extension is 0 then give error and exit
      if (extnum .eq. 0) then
         context = 'primary extension not supported for GTI file'
         call fcerr(context)
         goto 999
      endif

C  open the input FITS file
      call ftopen(iunit,filename,0,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         goto 999
      endif
      inopen = .true.

C  move to the extension number
      call ftmrhd(iunit,extnum,htype,ftstatus)
      if (ftstatus .ne. 0) then
         errstr = 'error moving to extension in INPUT file # '
         write(context,1000) errstr, extnum
         call fcerr(context)
         goto 999
      endif

C  get the header depending on the extension type
      if (htype .eq. 1) then
         call ftgtbh(iunit,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,ftstatus)
      else
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstatus)
      endif
C  check that column name exists and get its position in the input file
      if (column .eq. ' ') then
         errstr = 'error in input column name'
         call fcerr(errstr)
         goto 999
      endif
      call fccmpl(1,tfields,column,ttype,negflag,goodlist)
      if (.not. goodlist) then
         errstr = 'error in input column name'
         call fcerr(errstr)
         goto 999
      endif
      call ftgcno(iunit,exact,column,colnum,ftstatus)

C  get the reference date and time
      ftstatus=0
      if ((obsdate(1:1) .eq. ' ') .or. (obsdate(1:1) .eq. '-')) then
         dates = .false.
      else
         dates = .true.
         call ftgkys(iunit,obsdate,odate,comment,ftstatus)
         if (ftstatus .ne. 0) then
            errstr = 'error getting reference date'
            call fcerr(errstr)
            goto 999
         endif
         if (index(odate,'/') .gt. 0 .or. index(odate,'-') .gt. 0) then
            julian = .false.
         else
            julian = .true.
         endif
         if (.not. julian) then
            call ftgkys(iunit,obstime,otime,comment,ftstatus)
            if (ftstatus .ne. 0) then
               errstr = 'error getting reference time'
               call fcerr(errstr)
               goto 999
            endif
         endif
      endif

C  open the GTI FITS file
      call ftopen(gtiunit,gtiname,0,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open GTI file'
         call fcerr(context)
         goto 999
      endif
      gtiopen = .true.

C  move to the GTI extension
      call ftmrhd(gtiunit,gtiext,ghtype,ftstatus)
      if (ftstatus .ne. 0) then
         errstr = 'error moving to extension in GTI file # '
         write(context,1000) errstr, 1
         call fcerr(context)
         goto 999
      endif

C  get the header depending on the extension type
      if (ghtype .eq. 1) then
         call ftgtbh(gtiunit,growlen,gnrows,gfields,gtype,gbcol,
     &        gform,gunit,gextname,ftstatus)
      else
         call ftghbn(gtiunit,maxcl,gnrows,gfields,gtype,gform,gunit,
     &        gextname,gvaridat,ftstatus)
      endif

C  check that column names exist and get their position in the GTI file
      if (gticols .eq. ' ') then
         errstr = 'error in GTI column names'
         call fcerr(errstr)
         goto 999
      endif
      call fcgcls(gticols,gtilist,tcols,negflag)
      call fccmpl(2,gfields,gtilist,gtype,negflag,goodlist)
      if (.not. goodlist) then
         errstr = 'error in GTI column names'
         call fcerr(errstr)
         goto 999
      endif
      call ftgcno(gtiunit,exact,gtilist(1),gtinum(1),ftstatus)
      call ftgcno(gtiunit,exact,gtilist(2),gtinum(2),ftstatus)

C  get the GTI reference date and time
      if (dates) then
         call ftgkys(gtiunit,gtidate,gdate,comment,ftstatus)
         if (ftstatus .ne. 0) then
            errstr = 'error getting GTI reference date'
            call fcerr(errstr)
            goto 999
         endif
         if((index(gdate,'/') .gt. 0 .or. index(gdate,'-') .gt.0)
     &	    .and. julian) then
            context = ' GTI file has different style reference'
            call fcerr (context)
            ftstatus = 1
            goto 999
         endif
         if (.not. julian) then
            call ftgkys(gtiunit,gtitime,gtime,comment,ftstatus)
            if (ftstatus .ne. 0) then
               errstr = 'error getting GTI reference time'
               call fcerr(errstr)
               goto 999
            endif
         endif

C  get the time offset for the observation and GTI file
         call fctofs(odate,otime,gdate,gtime,ndate,ntime,ooffs,goffs,
     &        julian, ftstatus)
      else
         ooffs = 0.D0
         goffs = 0.D0
      endif
      
C  open the new FITS file
c  changed from ftinit to ffinit so that it overwrites the existing file 
c  if any
ccc      call ftinit(ounit,outfile,block,ftstatus)
      call ffinit(ounit,outfile,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open outfile'
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

C  construct a simple primary header or copy for the new file
      if (copyall) then
         call ftmahd(iunit,1,htype,ftstatus)
         mkywd = 0
         call ftcopy(iunit,ounit,mkywd,ftstatus)
      else
         call ftmahd(iunit,1,htype,ftstatus)
         call ftgprh(iunit,simple,bitpix,naxis,naxes,pcount,
     &        gcount,extend,ftstatus)
         simple = .true.
         naxis = 0
         pcount = 0
         gcount = 1
         call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,
     &        gcount,extend,ftstatus)
         call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,ftstatus)
      endif

C  create a new extension
      i = 1
 500  call ftmrhd(iunit,1,htype,ftstatus)

      if (ftstatus .ne. 0)  goto 600
      if (i .eq. extnum) then
         call ftcrhd(ounit,ftstatus)
         if (htype .eq. 1) then
            call ftphtb(ounit,rowlen,0,tfields,ttype,tbcol,
     &           tform,tunit,extname,ftstatus)
            call xcopyscale(iunit,ounit,ftstatus)

C and write the needed keywords
C  write a history keyword to the header
            context = 'TASK:fltime on file '
            context(22:) = filename
            call ftphis(ounit,context,ftstatus)
            context = 'With GTI file '
            context(16:) = gtifile
            call ftphis(ounit,context,ftstatus)
            if (dates) then
               write(ndate,*,err=665) tmpnum
               call ftmkye (ounit, obsdate, tmpnum, 6,'&',ftstatus)
               goto 667
 665           call ftmkys (ounit, obsdate, ndate, '&', ftstatus)
 667           if (.not. julian) then
                  write(ntime,*,err=675) tmpnum
                  call ftmkye (ounit, obsdate, tmpnum, 6,'&',ftstatus)
                  goto 677
 675              call ftmkys (ounit, obstime, 
     &              ntime, '&',ftstatus)
 677              continue
               endif
            endif

            call ftadef(ounit,rowlen,tfields,tbcol,tform,0,
     &           ftstatus)
         else
            call ftphbn(ounit,0,tfields,ttype,tform,tunit,
     &           extname,varidat,ftstatus)

            call xcopyscale(iunit,ounit,ftstatus)

C and write the needed keywords
C  write a history keyword to the header
            context = 'TASK:fltime on file '
            context(22:) = filename
            call ftphis(ounit,context,ftstatus)
           length = fcstln(gtifile)
            context = 'With GTI file '
            context(16:) = gtifile
            call ftphis(ounit,context,ftstatus)
            if (dates) then
               read(ndate,*,err=685) tmpnum
               call ftmkye (ounit, obsdate, tmpnum, 6,'&',ftstatus)
               goto 687
 685           call ftmkys (ounit, obsdate, ndate, '&', ftstatus)
 687           if (.not. julian) then
                  write(ntime,*,err=695) tmpnum
                  call ftmkye (ounit, obsdate, tmpnum, 6,'&',ftstatus)
                  goto 697
 695              call ftmkys (ounit, obstime, 
     &              ntime, '&',ftstatus)
 697              continue
               endif
            endif

            call ftbdef(ounit,tfields,tform,varidat,0,ftstatus)

         endif
C  filter the input FITS file using the GTI file
         call fifilt(iunit,colnum,nrows,ooffs,gtiunit,gtinum,gnrows,
     &        goffs,ounit,exposure)

      else if (copyall) then
         call ftcrhd(ounit,ftstatus)
         call ftcopy(iunit,ounit,0,ftstatus)
      endif
      i = i + 1
      goto 500
 600  ftstatus = 0

C  close the infile and the outfile
      call ftclos(iunit,ftstatus)

      call ftclos(gtiunit,ftstatus)
      call ftclos(ounit,ftstatus)

C  close files and return on error
 999  continue
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ftstatus = 0
         if (inopen)  call ftclos(iunit,ftstatus)
         if (gtiopen)  call ftclos(gtiunit,ftstatus)
         if (outopen)  call ftclos(ounit,ftstatus)
      endif

 1000 format(A34,I3)
      return
      end


C******************************************************************************
C SUBROUTINE:
C      fifilt
C
C DESCRIPTION:
C      Filters a FITS file using Good Time Interval values 
C
C AUTHOR/DATE:
C      Janice Tarrant 3/25/92
C
C MODIFICATION HISTORY:
C Banashree M Seifert (2.0.0, Oct 1996) 
C       . rewritten to allocate DMA and so added one more subroutine
C         filter
C         goes for release 3.6
C
C NOTES:
C
C USAGE:
C      call fifilt(iunit,colnum,nrows,ooffs,gtiunit,gtinum,gnrows,
C                   goffs,ounit,exposure)
C
C ARGUMENTS:
C      iunit   - input file logical unit number
C      colnum  - column number in input file
C      nrows   - number of rows in input file
C      ooffs   - observation time offset
C      gtiunit - GTI file logical unit number
C      gtinum  - column number in GTI file
C      gnrows  - number of rows in GTI file
C      goffs   - GTI time offset
C      ounit   - output file logical unit number
C      exposure - exposure keyword
C
C PRIMARY LOCAL VARIABLES:
C      found      - found time in GTI flag
C      width      - width of table
C      orow       - output row number
C      frow       - beginning row number
C      remain     - number of rows remaining
C      nelem      - number of data elements
C      gfrow      - GTI beginning row number
C      gremain    - GTI number of rows remaining
C      gnelem     - GTI number of data elements
C      gin        - GTI time is in
C      rownum     - input row number
C      nnrows     - new number of rows
C      time       - observation times
C      start_time - GTI start time
C      stop_time  - GTI stop time
C
C CALLED ROUTINES:
C      subroutine ftgcfe - get real column values
C      subroutine ftgkyj - get integer keyword value
C      subroutine ftmkyj - modify integer keyword value
C
C******************************************************************************
      subroutine fifilt(iunit,colnum,nrows,ooffs,gtiunit,gtinum,
     &                        gnrows,goffs,ounit,exposure)
      implicit none
      integer maxgti
      parameter (maxgti = 10000)
      character*(*) exposure
      integer iunit, colnum, nrows, gtiunit, gtinum(2), gnrows, ounit
      double precision ooffs, goffs

      character(70) comment,subinfo
      integer j, width, length, ftstatus, gfrow, gfelem, gnelem
      double precision start_time(maxgti), stop_time(maxgti)
      double precision intgr, dummy
      logical anyf, flagvals(maxgti)

      integer p_time,p_flvals
      integer status

c --- DYNAMIC MEMORY ALLOCATION ---
c  the following MEM common block definition is in the system iraf77.inc
c  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ----------------------------------------------------------------------+
      character(8) subname
      parameter (subname='fifilt')
      character(5) version
      parameter(version='2.0.0')
c ----------------------------------------------------------------------

      ftstatus=0
C  get the input row width
      call ftgkyj(iunit,'NAXIS1',width,comment,ftstatus)
      call ftgkyj(iunit,'NAXIS2',length,comment,ftstatus)

C set the integration time to zero
      intgr = 0.0d0
      dummy=0.0d0


C determine the total integration time
      gfrow = 1
      gfelem = 1
      gnelem=gnrows
      ftstatus=0
      

C
C Check that the number of gti does not exceed the maxgti.
C
      If (gnrows.gt.maxgti) then
         write(subinfo,19)gnrows,maxgti
19       format('The number of gti ',i7, ' > the maximum(maxgti)',
     *          i7, ' Increase maxgti in fltime.f.')
         call wterrm(subname,version,subinfo)
         return
      endif

      call ftgcfd(gtiunit,gtinum(1),gfrow,gfelem,gnelem,start_time,
     &                  flagvals,anyf,ftstatus)
      call ftgcfd(gtiunit,gtinum(2),gfrow,gfelem,gnelem,stop_time,
     &                  flagvals,anyf,ftstatus)
 
      do 200 j = 1, gnelem
        intgr = intgr + stop_time(j) - start_time(j)
 200  continue

C  update the exposure keyword
      call ftgkyd(iunit,exposure,dummy,comment,ftstatus)
      if (ftstatus .eq. 0) then
        call ftmkyd(ounit,'EXPOSURE',intgr,8,comment,ftstatus)
      else
        ftstatus = 0
        comment = 'Total Integration Time'
        call ftukyd(ounit,'EXPOSURE',intgr,8,comment,ftstatus)
      endif

c ------------------------- DMA ---------------------------------
      p_time = 0
      p_flvals = 0
      status = 0

      call udmget(length, 7, p_time, status)
      if (status .ne. 0) then
          goto 50
      endif

      call udmget(length, 1, p_flvals, status)
      if (status .ne. 0) then
          goto 50
      endif
 
 50   if(status .ne. 0) then
         subinfo='unable to allocate memory'
         call wterrm(subname,version,subinfo)
         return
      endif

      call filter(iunit,colnum,nrows,ooffs,gnelem,goffs,
     >            start_time,stop_time,ounit,width,MEMD(p_time),
     >            MEMB(p_flvals), status)

      if(status .ne. 0) then
         subinfo='returning from filter'
         call wterrm(subname,version,subinfo)
         return
      endif
          
      status = 0
      call udmfre(p_time, 7, status)
      if (status .ne. 0) then
          goto 60
      endif
      
      call udmfre(p_flvals, 1, status)
      if (status .ne. 0) then
          goto 60
      endif

 
 60   if(status .ne. 0) then
         subinfo='unable to de-allocate memory'
         call wterrm(subname,version,subinfo)
         return
      endif

      return
      end

C******************************************************************************

*+FILTER
      subroutine filter (iunit,colnum,nrows,ooffs,gnelem,goffs,
     >                   start_time,stop_time,ounit,width,time,
     >                   flvals,status)

c ----------------------------------------------------------------------
c This subroutine does the actual filtering of the GTIs
c Rewritten from fifilt 
c ------------------------------------------------------------------

      implicit none
      integer iunit,ounit,colnum,nrows,gnelem,width,status
      double precision ooffs, goffs
      double precision time(*),start_time(*),stop_time(*)
      logical flvals(*)

c --------------------- internals --------------------------------
      character(70) comment,subinfo
      integer nelem,orow,frow,felem,rownum,nnrows
      integer i,j
      logical anyf, found

      double precision data_first,data_last
      double precision gti_first,gti_last
      double precision start_key, end_key

c ------------------ authors/modifications ---------------------
c Banashree M Seifert (1.0.0, Oct 1996)
c        . created for reported bug and made calculations simple
c
c Banashree M Seifert (1.1.0, Oct28 1996)
c        . replaced so that it writes naxis2 keyword correctly
c
c Banashree M Seifert (1.2.0, Nov27, 1996)
c        . do loop is corrected
c
c Banashree M Seifert (1.3.0, Mar 3, 1997)
c        . TSTART and TSTOP keywords are included in outfile
c
c Banashree M Seifert (1.4.0, Oct 6, 1997)
c        . do 30 & do 40 does not go along with enddo for old compilers
c          corrected to be
c             30 continue, 40 continue
c
c -----------------------------------------------------------------
      character(7) subname
      parameter (subname='filter')
      character(5) version
      parameter (version='1.4.0') 
      double precision tdiff
*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(0,40,1,subinfo)
 
      nelem = nrows
      nnrows=nrows
      orow = 0
      frow = 1
      felem = 1
      status=0
      call ftgcfd(iunit,colnum,frow,felem,nelem,time,flvals,anyf,
     &                  status)

c write the TSTART and TSTOP keyword
c this is included as the 
c TSTART is GTI time start or data time start whichever is later in time 
c TSTOP  is GTI time start or data time start whichever is earlier in time
c March 3, 1997 (Banashree M Seifert)

      data_first=time(1)
      data_last=time(1)
      do 30 i=1,nelem
         if(time(i) .gt. data_last) data_last=time(i)
         if(time(i) .lt. data_first) data_first=time(i)
  30  continue

      gti_first=start_time(1)
      gti_last=stop_time(1)
      do 40 i=1,gnelem
         if(stop_time(i) .gt. gti_last) gti_last=stop_time(i)
         if(start_time(i) .lt. gti_first) gti_first=start_time(i)
 40   continue
      
      start_key=data_first
      end_key  = data_last 
      if(gti_first .gt. data_first) start_key=gti_first
      if(gti_last  .lt. data_last)  end_key= gti_last 
 

       comment = 'Data starting time in secs'
       status = 0 
       call ftukyd(ounit,'TSTART',start_key,15,comment,status)
       status = 0 
       comment = 'Data ending time in secs'
       call ftukyd(ounit,'TSTOP',end_key,15,comment,status)
       status = 0 
       tdiff = end_key - start_key
       comment = 'Elapsed Time'
       call ftukyd(ounit,'TELAPSE',tdiff,15,comment,status)
       status = 0 


C  determine if the times are in the GTI times
      do 10 i = 1, nelem
          time(i) = time(i) + ooffs
          found = .false.
          do j=1,gnelem
             if(.not. found) then
                start_time(j) = start_time(j) + goffs
                stop_time(j) = stop_time(j) + goffs
                if ((time(i) .ge. start_time(j)) .and.
     &              (time(i) .le. stop_time(j))) then
                    found = .true.
                endif
             endif
          enddo
          if (found) then
              rownum = frow + i - 1
              orow = orow + 1
              status = 0
              call fcopyr (iunit, rownum, ounit, orow, width,status)
              if(status .ne. 0) then
                 subinfo='returning from fcopyr'
                 call wterrm(subname,version,subinfo)
                 return 
              endif
          else
              nnrows = nnrows - 1
          endif
 10      continue

C  correct the number of rows keyword
      status=0
      comment='number of rows in table'
      call ftmkyj(ounit,'NAXIS2',nnrows,comment,status)
      call ftddef (ounit, width*nnrows, status)


      return
      end

c ---------------------------------------------------------------
c               END OF FILTER
c ----------------------------------------------------------------

