c****************************************************************************
C SELECTOR TASK:  
C      asmappend
C
C FILE:
C      asmappend.f 
C
C DESCRIPTION: 
c      Reads in an ASM file (or list of files) and sorts them according
c      to time then reads the information and appends that information
c      to a MASTER file in time ordered form, updating the necessary
c      files. 
C     
C AUTHOR:  
C      Brian K. Elza 6/95
C
C MODIFICATION HISTORY:
C      10/30/97 PDW 4.0a - Changed FTGCLx to FTGCFx and FTGCVx so as to
C                          only make calls to standard FITSIO interface
C                          described in FITSIO manual... hide internals
C
C      18June1998 (MJT) 4.0b - fixed ftopen call, which ignored output
C                              from fcpars re: masterfile
C
C      25Mar2008 (MJT) 4.0c - modified to match new ASM file formats
C                             having two new columns (BAND/A_BKGND)
C
C      30Oct2009 (MJT) 4.0d - masterv13 changes: one fewer column
C                             (dropped TIME_ANL, was col 17) and
C                             type changes for DATE_ANL and CAL_REF
C                             from "J" to "20A"
C NOTES:
C      
C      
C ARGUMENTS:
C      
C
C PRIMARY LOCAL VARIABLES:  
C
C***************************************************************************
      subroutine asmapd()
c      implicit none

      integer isiz,nb
      parameter (isiz=100)
      parameter (nb = 100)
      integer inofiles, inomasfiles,status,imjdref,
     &   masterextnum,xtend,masterunit,block,islaveunit,
     &   extnum,istimecol,imtimecol,isnssc,imnssc,isminchan,
     &   imminchan,ismaxchan,immaxchan
      logical abort, lmultiple
      character(160) masterfile, infile, infiles(isiz),
     &   masterinfile(2),slavefile
      character(80) objects(isiz),obstart,obstop,obsdate
      double precision tmjds(nb),tmjde(nb),mjdref,
     &   mtmjds(2),mtmjde(2),dummy
      character(40) taskname
      
      integer i
      
      common /task/ taskname

c     These two common values are for chktime's usage, they tell it NOT
c     to do anything. 
      common/multiple/lmultiple
      common/timeres/dummy
      
      taskname = 'asmappend_4.0d'
      lmultiple=.FALSE.
      dummy=0.0d0
      infile = ' '
      masterfile = ' '

c     Since the subroutine appendfile is generic we have to
c tell it which columns are the TIME columns in the files being
c processed...
      istimecol = 1
      imtimecol = 1
      isnssc = 3
      imnssc = 3
      isminchan = 4
      imminchan = 4
      ismaxchan = 5
      immaxchan = 5
      
      status=0
      abort=.FALSE.
      
      call fcecho(' ')
      call fcecho('Running ASMAPPEND version 4.0c')
      call fcecho('==============================================')
        
C     get the parameters from the par file
      call gasmpar(masterfile,infile,status)

c     See if the infile is a list of files and if so separate them.
      call fcgcls(infile,infiles,inofiles,abort)
      if(abort)then
        call fcecho(' ')
        call fcecho('Could not separate INFILE into filenames.')
        goto 999
      endif

c     See if the masterfile is a list of files, if so give error
c and abort if more than onefile. 
      call fcgcls(masterfile,masterinfile,inomasfiles,abort)
      if(abort)then
        call fcecho(' ')
        call fcecho('Could not separate MASTERFILE into filenames.')
        goto 999
      endif


c     Since it is possible that the MASTERFILE actually is a list
c of files and that these must all be sorted and appended with the
c appended files what we will do is to put all of them together into
c one long array of files.
      
      do 110 i=1,inomasfiles
        infiles(inofiles+i)=masterinfile(i)
110   continue
      
      inofiles=inofiles+inomasfiles

      
c      print*,'inofiles is',inofiles
c      print*,'inomasfiles is',inomasfiles

      obstart='TSTART'
      obstop='TSTOP'
      obsdate='MJDREF'
      imjdref=0
      mjdref=0.0d0

c      do 10 i=1,inofiles
c        print*,'infiles are ',i,infiles(i),tmjds(i),tmjde(i)
c10    continue

c     Load the arrays tmjds and tmjde with the start and end times
c located in each of these files so that we know what is needed.
c This will facilitate our checking if we can just append this file to
c the masterfile, or if we are going to have to insert it between rows
c already in the masterfile or replace some information.
      call chktime(infiles,objects,obstart,obstop,obsdate,
     &   mjdref,imjdref,tmjds,tmjde,inofiles,status)

c      do 20 i=1,inofiles
c        print*,'infiles are ',i,infiles(i),
c     &     tmjds(i)/86400.0d0,tmjde(i)/86400.0d0
c20    continue

c     Now the MASTERFILE should be the earliest possible file
c so we will make it be the earliest by simply assigning it that way...
c Of course this means that we will have to start looping through our
c files to be appended beginning at the second file, since the first
c has been force to be the MASTER.
      masterinfile(1)=infiles(1)
      inomasfiles=1
      mtmjds(1)=tmjds(1)
      mtmjde(1)=tmjde(1)

c      print*,'Mastertime info is ',masterinfile(1),
c     &   mtmjds(1)/86400.0d0,mtmjde(1)/86400.0d0

c      Assign a unit file number used in inputting file.
      call ftgiou(masterunit,status)
      if(status.ne.0)then
        call fcecho('Error getting input unit number')
        call fcecho('Setting to logical masterunit 10')
        status=0
        masterunit=10
      endif

      call fcecho(' ')
      call fcecho('Processing MASTERFILE ')
      call fcecho(masterinfile(1))

      call fcpars(masterinfile(1),masterfile,masterextnum,status)
      if (status .ne. 0) then
        call fcerrm(status)
        call fcecho('Unable to parse masterfile name')
        call fcecho('aborting...')
        goto 999
      endif
      
c      If no extension is found set the extension to number 2
      if (masterextnum .eq. -99) masterextnum = 1
      
c     call ftopen(masterunit,masterinfile(1),1,block,status)
c 18Jun98 (MJT) ironically, this wouldn't have worked with file+ext
c in the old (preCFITSIO) days, though its OK now. Fixing it anyway.
      call ftopen(masterunit,masterfile,1,block,status)
      if (status .ne. 0) then
        call fcerrm(status)
        call fcerr('unable to open first file in infile')
        call ftclos(masterunit,status)
        if(status.ne.0)then
          call fcecho('Error closing input number')
          status=0
        endif
           
        call ftfiou(masterunit,status)
        if(status.ne.0)then
          call fcecho('Error freeing output unit number')
          status=0
        endif
        goto 999
      endif

c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status).
c      Move to the correct extension number.
      do 115 i=1, masterextnum+1
        call ftmahd(masterunit,i,xtend,status)
        call ftpcks(masterunit,status)
115   continue
      
c      print*,'Moved to extension number',masterextnum+1

c----------------------------------------------------------------------      

c      Assign a unit file number used in inputting file.
      call ftgiou(islaveunit,status)
      if(status.ne.0)then
        call fcecho('Error getting input unit number')
        call fcecho('Setting to logical islaveunit 11')
        status=0
        islaveunit=11
      endif

      do 100 i=2,inofiles

        call fcpars(infiles(i),slavefile,extnum,status)
        if (status .ne. 0) then
          call fcerrm(status)
          call fcerr('unable to parse filename')
          return
        endif

        call fcecho('----------------------------------------------')
        call fcecho(' ')
        call fcecho('Processing APPEND FILE:')
        call fcecho(slavefile)

c      If no extension is found set the extension to number 2
        if (extnum .eq. -99) extnum = 1
      
        call ftopen(islaveunit,slavefile,0,block,status)
        if (status .ne. 0) then
          call fcerrm(status)
          call fcerr('unable to open first file in infile')
          call ftclos(islaveunit,status)
          if(status.ne.0)then
            call fcecho('Error closing input number')
            status=0
          endif
           
          call ftfiou(islaveunit,status)
          if(status.ne.0)then
            call fcecho('Error freeing output unit number')
            status=0
          endif
          return
        endif
        
c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status).
c      Move to the correct extension number.

        call ftmahd(islaveunit,extnum+1,xtend,status)

c       Since there are several things that can be required in appending
c the secondary file to the master we will handle them in different
c subroutines. This subroutine will deal with all cases necessary for
c appending one file to another.
        call appendfile(masterunit,islaveunit,
     &       tmjds(i),tmjde(i),mtmjds(1),mtmjde(1),
     &       istimecol,imtimecol,abort)

        if(abort)goto 999

        call ftclos(islaveunit,status)
        if(status.ne.0)then
          call fcecho('Could not close APPEND file')
          call fcerrm(status)
          status=0
        endif
        
100   continue

      call ftpcks(masterunit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to update CHKSUM values')
        status=0
      endif
      
      call ftclos(masterunit,status)
      if(status.ne.0)then
        call fcecho('Could not close MASTERFILE file')
        call fcerrm(status)
        status=0
      endif
      
      
999   continue

      call fcecho('----------------------------------------------')
      call fcecho(' ')
      if(.not.abort)then
        call fcecho('Modification of MASTERFILE was successful!')
        call fcecho(' ')
        call fcecho('All events from APPEND files:')
        
        do 101 i=2,inofiles
          call fcecho(infiles(i))
101     continue

        call fcecho(' ')
        call fcecho('Were added to the MASTERFILE:')
        call fcecho(masterfile)
      else
        call fcecho('Run was ABORTED due to error.')
        call fcecho('See messages above for pertinent information.')
      endif
      
      return
      end


c**********************************************************************
c
c PRIMARY LOCAL VARIABLES:
C      context - error messages
C      status - error number
c
c CALLED ROUTINES:
C      subroutine fcerr  - report error to STDERR
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsd - get double precision real parameter
C      subroutine uclgst - get string parameter
C
c Get the input parameters from the *.par file. 
c**********************************************************************

      subroutine gasmpar(masterfile,infile,status)

c      implicit none

      integer status
      character(80) context
      character(160) masterfile, infile

C      initialize variables
      status=0

C      get the name of the input FITS file
      call uclgst('masterfile',masterfile,status)
      if (status .ne. 0) then
        context = 'could not get MASTERFILE parameter'
        call fcerr(context)
        goto 999
      endif

C      get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
        context = 'could not get INFILE parameter'
        call fcerr(context)
        goto 999
      endif

c     Update the variable telling the latest time that is found in
c the master file to agree with the last time that existed in the file
c that was appended. 
      
999   continue
      return
      end
      

c**********************************************************************
c     This is the subroutine which deals with scanning the files to
c be appended and figuring out what needs to be done. It scans the master
c file as well as the append file to determine exactly HOW to insert the
c information in the append file into the master file. This subroutine
c determines if all of the information in the append file is to be appended
c to the masterfile or if some data must be inserted between existing rows
c or if it must replace information that exists. 

      subroutine appendfile(masterunit,islaveunit,tmjds,tmjde,
     &   mtmjds,mtmjde,istimecol,imtimecol,abort)

c      implicit none
      integer maxfld,iaxis
      parameter (maxfld = 100)
      parameter (iaxis = 2)
      double precision tmjds,tmjde,mtmjds,mtmjde
      integer masterunit,islaveunit,pcount,status,istimecol,
     &   imtimecol
      integer isnrows,isnfields,
     &   imnrows,imnfields
      character(40) sttype(maxfld),stform(maxfld),stunit(maxfld),
     &   sextnam,mttype(maxfld),mtform(maxfld),mtunit(maxfld),
     &   mextnam,stimeunit,mtimeunit,contxt,strbuffer

      integer itzero
      double precision stzero,mtzero,difftzero,sconversion,
     &   mconversion,timef,mtimeval,stimeval,stime,mtime,fmtime
      logical abort
      integer itime,i,sfelem,slelem,mfelem,nrows,j
      
      logical flgval,anynul
      flgval = .FALSE.
      anynul = .FALSE.

      
      status=0

c      print*,' '
c      print*,'masterunit, islaveunit',masterunit, islaveunit
c      print*,'tmjds,tmjde,mtmjds,mtmjde',tmjds,tmjde,mtmjds,mtmjde
c      print*,' '
      
c----------------------------------------------------------------------
c     Let's read in the pertinent information about both of these
c files that are being processed.
      call ftghbn(masterunit,maxfld,imnrows,imnfields,mttype,mtform,
     &   mtunit,mextnam,pcount,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR reading MASTER table information!')
        call fcecho('Cannot continue - check MASTERFILE!')
        return
      endif

      call ftghbn(islaveunit,maxfld,isnrows,isnfields,sttype,stform,
     &   stunit,sextnam,pcount,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR reading APPENDED table information!')
        call fcecho('Cannot continue - check FILE being appended!')
        return
      endif

c----------------------------------------------------------------------      
c     Now read the OFFSET times that apply to the TIME column for both
c files.
      
      call ftgkyd(masterunit,'TIMEZERO',mtzero,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(masterunit,'TIMEZERI',itzero,contxt,status)
        call ftgkyd(masterunit,'TIMEZERF',mtzero,contxt,status)
        if(status.eq.0)then
          mtzero = mtzero+dfloat(itzero)
          itzero = 0
        endif
      endif

      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Warning on reading MASTERFILE')
        contxt='Could not find keyword TIMEZERO, or TZERO'
        call fcecho(contxt)
        call fcecho('Assuming TIME column has NO offset!!!')
        mtzero = 0.0d0
        status = 0
      endif
      
      call ftgkyd(islaveunit,'TIMEZERO',stzero,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(islaveunit,'TIMEZERI',itzero,contxt,status)
        call ftgkyd(islaveunit,'TIMEZERF',stzero,contxt,status)
        if(status.eq.0)then
          stzero = stzero+dfloat(itzero)
          itzero=0
        endif
      endif

      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Warning on reading APPENDED FILE')
        contxt='Could not find keyword TIMEZERO, or TZERO'
        call fcecho(contxt)
        call fcecho('Assuming TIME column has NO offset!!!')
        stzero=0.0d0
        status=0
      endif

      call ftgkys(masterunit,'TIMEUNIT',mtimeunit,contxt,status)
      if(status.ne.0)then
        contxt='Could not find keyword TIMEUNIT in MASTERFILE'
        call fcecho(contxt)
        contxt='Assuming all inputs in SECONDS'
        call fcecho(contxt)
        mtimeunit='s'
        status=0
      endif

      call timeconvert(mtimeunit,mconversion,status)
      if(status.ne.0)then
        contxt='Could not determine TIMEUNIT conversion'
        call fcecho(contxt)
        contxt='Assuming all inputs are in seconds'
        call fcecho(contxt)
        mconversion=1.0d0
        status=0
      endif

      call ftgkys(islaveunit,'TIMEUNIT',stimeunit,contxt,status)
      if(status.ne.0)then
        contxt='Could not find keyword TIMEUNIT in Append file'
        call fcecho(contxt)
        contxt='Assuming all inputs in SECONDS'
        call fcecho(contxt)
        stimeunit='s'
        status=0
      endif

      call timeconvert(stimeunit,sconversion,status)
      if(status.ne.0)then
        contxt='Could not determine TIMEUNIT conversion'
        call fcecho(contxt)
        contxt='Assuming all inputs are in seconds'
        call fcecho(contxt)
        sconversion=1.0d0
        status=0
      endif

      if(mconversion.ne.sconversion)then
        call fcecho(' ')
        call fcecho('TIMEUNIT in each file is different!')
        call fcecho('Are these two files compatible???')
        call fcecho('Check files and if necessary modify them.')
        call fcecho('Cannot continue')
        abort=.TRUE.
        goto 999
      endif

      
c     Let's compare the offsets for the two files to figure out
c the additive factor that must be applied to the TIME's in the file
c to be appended to the MASTER file.

      difftzero = stzero - mtzero

c      print*,'mconversion and sconversio',mconversion,sconversion
c      print*,'mtzero, stzero, and difftzero are',mtzero,stzero,difftzero
      
c----------------------------------------------------------------------
c     Let's check the rest of the information in these two files to
c make sure that they are compatible!

c      print*,'imnfields and isnfields are ',imnfields,isnfields
      if(imnfields.ne.isnfields)then
        call fcecho(' ')
        call fcecho('Number of COLUMNS in the two files differ!')
        call fcecho('Cannot continue... aborting')
        abort=.TRUE.
        goto 999
      endif

c     Let's check to make sure that the columns have the same names
c and lets check the units of those columns as well.
      do 100 i=1,imnfields
        if(mttype(i).ne.sttype(i))then
          call fcecho(' ')
          call fcecho('TTYPEs do not match for these files')
          call fcecho('Cannot continue... aborting...')
          abort = .TRUE.
          goto 999
        endif

        if(mtunit(i).ne.stunit(i))then
          call fcecho(' ')
          call fcecho('TUNITs do not match for these files')
          call fcecho('Cannot continue... aborting...')
          abort = .TRUE.
          goto 999
        endif

        if(mtform(i).ne.stform(i))then
          call fcecho(' ')
          call fcecho('TFORMs do not match for these files')
          call fcecho('Cannot continue... aborting...')
          abort = .TRUE.
          goto 999
        endif

100   continue

      call fcecho(' ')
      call fcecho('File passed checks to be sure it is compatible.')
      call fcecho('Modifying MASTERFILE!')

c======================================================================
c     Now that we have checked the files to be sure that everything
c is compatible we can actually begin sorting them and adding them.
c======================================================================
      
c      print*,'IMNROWS and ISNROWS are',imnrows,isnrows,status

c======================================================================
c     Since we have sorted the files so that the MASTERFILE is the
c earliest file and each APPENDFILE is now sorted in time we can deal
c with all of the HEADER keywords that deal with the LAST time in the
c APPENDFILE.
c======================================================================
      
c       Let's change the variable containing the last time in
c the MASTERFILE. We will have to modify some of the keywords as well
c and that is what we will do at this point.

      if(tmjde.gt.mtmjde)then

        call ftgkyd(islaveunit,'TSTOP',timef,contxt,status)
        call ftmkyd(masterunit,'TSTOP',timef,14,contxt,status)

        if(status.ne.0)then
          status=0
          call ftgkyj(islaveunit,'TSTOPI',itime,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Unable to get TSTOPI from APPENDFILE')
            call fcerrm(status)
            abort=.TRUE.
            goto 999
          endif

          call ftmkyj(masterunit,'TSTOPI',itime,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Unable to put TSTOPI into MASTERFILE')
            call fcerrm(status)
            abort=.TRUE.
            goto 999
          endif

          call ftgkyd(islaveunit,'TSTOPF',timef,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Unable to get TSTOPF from APPENDFILE')
            call fcerrm(status)
            abort=.TRUE.
            goto 999
          endif

          call ftmkyd(masterunit,'TSTOPF',timef,14,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Unable to put TSTOPF into MASTERFILE')
            call fcerrm(status)
            abort=.TRUE.
            goto 999
          endif
        endif

        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to get TSTOP from APPENDFILE or')
          call fcecho('update TSTOP keyword in MASTERFILE aborting')
          call fcerrm(status)
          abort=.TRUE.
          goto 999
        endif

        call ftgkys(islaveunit,'DATE-END',strbuffer,contxt,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to get DATE-END from APPENDFILE')
          call fcecho('Aborting...')
          abort = .TRUE.
          goto 999
        endif

        call ftmkys(masterunit,'DATE-END',strbuffer,contxt,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to put DATE-END into APPENDFILE')
          call fcecho('Aborting...')
          abort=.TRUE.
          goto 999
        endif

        call ftgkys(islaveunit,'TIME-END',strbuffer,contxt,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to get TIME-END from APPENDFILE')
          call fcecho('Aborting...')
          abort=.TRUE.
          goto 999
        endif

        call ftmkys(masterunit,'TIME-END',strbuffer,contxt,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to put TIME-END into APPENDFILE')
          call fcecho('Aborting...')
          abort=.TRUE.
          goto 999
        endif

      endif
      
c======================================================================
c     This section checks the TSTART and TSTOP of the files that have
c been input and if the TSTART of the file to be appended comes after
c the TSTOP of the MASTERFILE then that file is simply appended to the
c MASTERFILE as one long bulk read. No other work is done on the file!
c======================================================================
      
      if((tmjds+1.0d-9).gt.mtmjde)then

c        print*,'No overlap in files just appending one to the other',
c     &     tmjds/86400.0d0,mtmjde/86400.0d0
c        print*,'IMNROWS is ',imnrows,isnrows
        
        call ftirow(masterunit,imnrows,isnrows,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not insert space for rows in MASTERFILE')
          call fcecho('Cannot continue... Aborting...')
          call fcerrm(status)
          abort=.TRUE.
          goto 999
        endif

        mfelem=imnrows+1
        sfelem=1
        slelem=isnrows

        call fcecho('Calling asminsert')

        call asminsert(masterunit,mtzero,
     &     islaveunit,stzero,
     &     mfelem,sfelem,slelem,
     &     imtimecol,istimecol,maxfld,abort)
        call fcecho('Out of asminsert')
        imnrows = imnrows+isnrows
        
        if(abort)then
          call fcecho(' ')
          call fcecho('Aborting from error in ASMINSERT')
        endif

      else
        
c        print*,'Overlap in files has been found...',
c     &     tmjds/86400.0d0,mtmjde/86400.0d0
        
c======================================================================
c       Since TSTART from APPENDFILE occurs before TSTOP in MASTERFILE
c we will have to scan through the MASTERFILE looking for matching
c TIMESTAMPS and if they exist we will have to sort on other columns,
c to determine if we should replace this data or add another row.
c======================================================================
        
        nrows=1

        call ftgcfd(masterunit,imtimecol,nrows,1,1,mtimeval,
     &     flgval,anynul,status)
        fmtime=mtimeval+mtzero
c        print*,'fmtime is on first',fmtime
        
        do 200 i=1,isnrows

c  Get the time from the file to be appended and calculate the
c total time for that timestamp.
          call ftgcfd(islaveunit,istimecol,i,1,1,stimeval,
     &       flgval,anynul,status)
          stime=stimeval+stzero
c          print*,'STIME VALUE IS ',stime


c  Check the timestamp from the append file with the first
c timestamp that is in the masterfile - fmtime = first master time...
          if((stime.lt.(fmtime+1.0d-9)).and.
     &       (stime.gt.(fmtime-1.0d-9)).and.nrows.eq.1)then

            mfelem=1
            sfelem=i
            slelem=1

            call asmsort(masterunit,imnrows,mtzero,
     &         islaveunit,stzero,mfelem,sfelem,slelem,
     &         maxfld,abort)
c            print*,'Out of ASMSORT',mfelem,sfelem,slelem,abort     
c            print*,' '
            
c            print*,'calling asminsert 2',mfelem,sfelem,slelem
            call asminsert(masterunit,mtzero,
     &         islaveunit,stzero,
     &         mfelem,sfelem,slelem,
     &         imtimecol,istimecol,maxfld,abort)

c  After inserting a row we jump to the end of this loop and
c start again.
            goto 200

          endif

203       if(nrows+1.le.imnrows)then

c  Increment the row in the Masterfile that is being read
            j=nrows+1
c            print*,'nrows and imnrows are',j,nrows,imnrows

          else
            
c            print*,'Going to 201'
            goto 201

          endif

c  Get the TIMESTAMP from the master file, and calculate the total
c time by adding the ZERO time to that quantity.
          call ftgcfd(masterunit,imtimecol,j,1,1,mtimeval,
     &       flgval,anynul,status)
          mtime=mtimeval+mtzero

c          print*,'MTIME and j is ',mtime,j

          if((stime.lt.(mtime+1.0d-9)).and.
     &       (stime.gt.(mtime-1.0d-9)))then

c  When the timestamp in the Masterfile and the appendfile are
c equal then we have to be VERY careful since it is possible that
c several of the columns may have quantities that differ. So we cannot
c simply insert another row into the Masterfile, since it is possible for
c there to be several rows with equivalent timestamp values. So we will
c have to call a subroutine which will do various comparisons to determine
c exactly where to insert the row from the appendfile into the Masterfile.

            mfelem=j
            sfelem=i
            slelem=i

c            print*,'Calling ASMSORT',mfelem,sfelem,slelem,abort
            
            call asmsort(masterunit,imnrows,mtzero,
     &         islaveunit,stzero,mfelem,sfelem,slelem,
     &         maxfld,abort)
c            print*,'Out of ASMSORT',mfelem,sfelem,slelem,abort     
c            print*,' '
c            mfelem=j
c            sfelem=i
c            slelem=i

            call asminsert(masterunit,mtzero,
     &         islaveunit,stzero,mfelem,sfelem,slelem,
     &         imtimecol,istimecol,maxfld,abort)

c            print*,'Out of ASMINSERT',mfelem,
c     &         sfelem,slelem,abort
            
            goto 200
            
          elseif(fmtime.lt.stime.and.mtime.gt.stime)then
c            print*,'FMTIME.LT.STIME.AND.MTIME.GT.STIME',
c     &         fmtime,stime,mtime,j,i,stime-mtzero

            mfelem=j
            sfelem=i
            slelem=i

c            print*,'CALLING ASMNESORT',fmtime,stime,mtime,mfelem
c            call asmnesort(masterunit,imnrows,mtzero,
c     &         stime,mfelem,maxfld,abort)
c            print*,'OUT OF ASMNESORT',mtime,stime,mfelem            

c            print*,'ATTEMPTING TO ADD ROW TO FILE',mfelem-1,
c     &         slelem,status
            call ftirow(masterunit,mfelem-1,1,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Could not add space to MASTERFILE')
              abort=.TRUE.
              goto 999
            endif

            fmtime=mtime
            
c            print*,'calling asminsert 4',mfelem,sfelem,slelem            
            call asminsert(masterunit,mtzero,
     &         islaveunit,stzero,mfelem,sfelem,slelem,
     &         imtimecol,istimecol,maxfld,abort)

            imnrows=imnrows+1

            goto 200

          endif

          nrows=nrows+1
          goto 203
            
200     continue

201     continue
        
        if(i.lt.isnrows)then
c          print*,'I is less than or equal isnrows...',i,
c     &       isnrows,nrows
          
          call ftirow(masterunit,imnrows,isnrows-i+1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not insert space for rows in MASTERFILE')
            call fcecho('Cannot continue... Aborting...')
            call fcerrm(status)
            abort=.TRUE.
            goto 999
          endif
        endif

        imnrows=imnrows+1

        mfelem=imnrows
        sfelem=i
        slelem=isnrows
        
c        print*,'calling asminsert 5',mfelem,sfelem,slelem        
        call asminsert(masterunit,mtzero,
     &     islaveunit,stzero,mfelem,sfelem,slelem,
     &     imtimecol,istimecol,maxfld,abort)
        
        imnrows=imnrows+(isnrows-i)
        
      endif

      if(tmjde.gt.mtmjde)mtmjde = tmjde

999   continue
      return
      end

c**********************************************************************
c     This subroutine actually deals with inserting the information
c from the appendfile into the masterfile. It MUST know the format of
c the file and at the moment it is hardwired for the particular format
c of the ASM data. 

      subroutine asminsert(masterunit,mtzero,
     &   islaveunit,stzero,mfelem,sfelem,slelem,
     &   imtimecol,istimecol,maxfld,abort)

c      implicit none
      integer masterunit,maxfld,
     &   islaveunit,mfelem,sfelem,
     &   slelem,imtimecol,istimecol,status
      double precision mtzero,stzero
      logical abort

      double precision doubleval
      integer n,intval
      character(70) strbuf
      real realval
      integer i,j
      logical flgval,anynul
      flgval = .FALSE.
      anynul = .FALSE.

c      print*,'INTO ASMINSERT',abort
      status = 0
      j=-1
      
      do 100 n=sfelem,slelem

        j=j+1
        
        do 200 i=1,3
          call ftgcfd(islaveunit,i,n,1,1,doubleval,
     &       flgval,anynul,status)
          if(status.ne.0)then
            call fcecho(' ')
            if(i.eq.1)then
              call fcecho('Unable to get TIME value.')
            elseif(i.eq.2)then
              call fcecho('Unable to get T_START_OBS value.')
            elseif(i.eq.3)then
              call fcecho('Unable to get T_STOP_OBS value.')
            endif
            call fcerrm(status)
            abort = .TRUE.
            goto 999
          endif

          if(i.eq.imtimecol)then
            if((stzero.lt.(mtzero+1.0d-9)).and.
     &         (stzero.gt.(mtzero-1.0d-9)))then
            else
              doubleval = doubleval + (stzero-mtzero)
            endif
          endif
          
          call ftpcld(masterunit,i,mfelem+j,1,1,doubleval,
     &       status)
          if(status.ne.0.and.i.eq.1)then
            call fcecho(' ')
            if(i.eq.1)then
              call fcecho('Unable to write TIME value.')
            elseif(i.eq.2)then
              call fcecho('Unable to write T_START_OBS value.')
            elseif(i.eq.3)then
              call fcecho('Unable to write T_STOP_OBS value.')
            endif
            call fcerrm(status)
            abort = .TRUE.
            goto 999
          endif

c          PRINT*,'IN ASMINSERT AND DOUBLEVVAL IS',doubleval+mtzero
c     &       ,sfelem,
c     &       slelem,n,j,mfelem
          
200     continue

c        print*,'Out of 200 loop entering 210 '        

        do 210 i=1,4
          call ftgcfj(islaveunit,3+i,n,1,1,intval,
     &       flgval,anynul,status)
          if(status.ne.0)then
            call fcecho(' ')
            if(i.eq.1)then
              call fcecho('Unable to get N_DWELL value.')
            elseif(i.eq.2)then
              call fcecho('Unable to get SSC_NUMBER value.')
            elseif(i.eq.3)then
              call fcecho('Unable to get MINCHAN value.')
            elseif(i.eq.4)then
              call fcecho('Unable to get MAXCHAN value.')
            endif
            call fcerrm(status)
            abort = .TRUE.
            goto 999
          endif

          call ftpclj(masterunit,3+i,mfelem+j,1,1,intval,
     &       status)
          if(status.ne.0)then
            call fcecho(' ')
            if(i.eq.1)then
              call fcecho('Unable to write N_DWELL value.')
            elseif(i.eq.2)then
              call fcecho('Unable to write SSC_NUMBER value.')
            elseif(i.eq.3)then
              call fcecho('Unable to write MINCHAN value.')
            elseif(i.eq.4)then
              call fcecho('Unable to write MAXCHAN value.')
            endif
            call fcerrm(status)
            abort = .TRUE.
            goto 999
          endif

210     continue

        call ftgcvs(islaveunit,8,n,1,1,'0',strbuf,
     &     anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to get BAND buffer string.')
          call fcerrm(status)
          abort = .TRUE.
          goto 999
        endif

        call ftpcls(masterunit,8,mfelem+j,1,1,strbuf,
     &     status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to write BAND buffer string.')
          call fcerrm(status)
          abort = .TRUE.
          goto 999
        endif

c        print*,'Out of 210 loop entering 220'        

        do 220 i=1,5
          call ftgcfe(islaveunit,8+i,n,1,1,realval,
     &       flgval,anynul,status)
          if(status.ne.0)then
            call fcecho(' ')
            if(i.eq.1)then
              call fcecho('Unable to get RATE value.')
            elseif(i.eq.2)then
              call fcecho('Unable to get ERROR value.')
            elseif(i.eq.3)then
              call fcecho('Unable to get TIMEDEL value.')
            elseif(i.eq.4)then
              call fcecho('Unable to get A_BKGND value.')
            elseif(i.eq.5)then
              call fcecho('Unable to get RDCHI_SQ value.')
            endif
            call fcerrm(status)
            abort = .TRUE.
            goto 999
          endif

          call ftpcle(masterunit,8+i,mfelem+j,1,1,realval,
     &       status)
          if(status.ne.0)then
            call fcecho(' ')
            if(i.eq.1)then
              call fcecho('Unable to write RATE value.')
            elseif(i.eq.2)then
              call fcecho('Unable to write ERROR value.')
            elseif(i.eq.3)then
              call fcecho('Unable to write TIMEDEL value.')
            elseif(i.eq.4)then
              call fcecho('Unable to write A_BKGND value.')
            elseif(i.eq.5)then
              call fcecho('Unable to write RDCHI_SQ value.')
            endif
            call fcerrm(status)
            abort = .TRUE.
            goto 999
          endif

220     continue

c        print*,'Out of 220 loop1 '
        
        call ftgcfj(islaveunit,14,n,1,1,intval,
     &     flgval,anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to get DOF value.')
          call fcerrm(status)
          abort = .TRUE.
          goto 999
        endif

        call ftpclj(masterunit,14,mfelem+j,1,1,intval,
     &     status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Unable to write DOF value.')
          call fcerrm(status)
          abort = .TRUE.
          goto 999
        endif

c        print*,'Out of 220 loop2 '        
        
        do 230 i=1,4
           call ftgcvs(islaveunit,14+i,n,1,1,'0',strbuf,
     &          anynul,status)
           if(status.ne.0)then
              call fcecho(' ')
              if(i.eq.1)then
                 call fcecho('Unable to get SOLUTN buffer string.')
              elseif(i.eq.2)then
                 call fcecho('Unable to get DATE_ANL value.')
              elseif(i.eq.3)then
                 call fcecho('Unable to get CAL_REF value.')
              elseif(i.eq.4)then
                 call fcecho('Unable to get BARYCENTER value.')
              endif
              call fcerrm(status)
              abort = .TRUE.
              goto 999
           endif

           call ftpcls(masterunit,14+i,mfelem+j,1,1,strbuf,
     &          status)
           if(status.ne.0)then
              call fcecho(' ')
              if(i.eq.1)then
                 call fcecho('Unable to write SOLUTN buffer string.')
              elseif(i.eq.2)then
                 call fcecho('Unable to write DATE_ANL value.')
              elseif(i.eq.3)then
                 call fcecho('Unable to write CAL_REF value.')
              elseif(i.eq.4)then
                 call fcecho('Unable to write BARYCENTER value.')
              endif
              call fcerrm(status)
              abort = .TRUE.
              goto 999
           endif

c        print*,'In 230 loop'
        
230     continue

100   continue

999   continue
      if(abort)call fcecho('Abort flag set... aborting ASMINSERT')
      
      return
      end

      
c**********************************************************************
c     This subroutine's functions is to deal with the instances when
c the TIMESTAMP in the APPENDFILE and in the MASTERFILE are the same.
c In those instances it is necessary to search other columns and to
c see if those columns are the same and if they aren't to insert
c the APPENDFILE row at the correct place in the MASTERFILE.
c**********************************************************************

      subroutine asmsort(masterunit,imnrows,mtzero,
     &   islaveunit,stzero,mfelem,sfelem,slelem,
     &   maxfld,abort)

      integer masterunit,maxfld,
     &   islaveunit,mfelem,sfelem,
     &   slelem,imnrows,status
      double precision mtzero,stzero
      logical abort

      integer ispace
      parameter (ispace = 30)

      double precision stimeval,fmtimeval,mtimeval,diff
      integer n, sndwell,mndwell(ispace),sssc,mssc(ispace),
     &   sminchan,mminchan(ispace),smaxchan,mmaxchan(ispace)
      integer j,ielements,k
      logical flgval,anynul,ltime,lndwell(ispace),
     &   lssc(ispace),lminchan(ispace),lmaxchan(ispace)

c      print*,'Into ASMSORT',imnrows,mtzero,
c     &   stzero,mfelem,sfelem,slelem
      
      flgval = .FALSE.
      anynul = .FALSE.
      fmtimeval=0.0d0

      do 50 j=1,ispace
        lndwell(j)=.FALSE.
        lssc(j)=.FALSE.
        lminchan(j)=.FALSE.
        lmaxchan(j)=.FALSE.
50    continue
      
      status = 0
      j=-1

      n=sfelem
      ielements=0
      diff=0.0d0
      
      diff=(stzero-mtzero)
c      print*,'STZERO, MTZERO, DIFF is ',stzero,mtzero,diff

c     Get all of the necessary information from the APPEND file and
c store these values into variables which will be used later.
      
      call ftgcfd(islaveunit,1,n,1,1,stimeval,
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get TIME APPEND value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif
      
      call ftgcfj(islaveunit,4,n,1,1,sndwell,
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get N_DWELL APPEND value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif
      
      call ftgcfj(islaveunit,5,n,1,1,sssc,
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get SSC_NUMBER APPEND value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif
      
      call ftgcfj(islaveunit,6,n,1,1,sminchan,
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get MINCHAN APPEND value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif
      
      call ftgcfj(islaveunit,7,n,1,1,smaxchan,
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get MAXCHAN APPEND value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif

c     We begin at the element in the MASTER file that we found
c was equal to the value in the APPENDFILE and proceed from there.

100   continue

      ltime=.FALSE.
      
      j=j+1
      ielements=ielements+1

c      print*,'Into SEARCH routine',imnrows,mtzero,
c     &   stzero,mfelem,sfelem,j

      
      if((mfelem+j).gt.imnrows)then
        mtimeval=fmtimeval+1.1d-9
        ielements=ielements-1
        goto 101
      endif
      
      call ftgcfd(masterunit,1,mfelem+j,1,1,mtimeval,
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get TIME MASTER value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif

c     Since we have to allow the TIMESTAMP to change before we know that
c we have read to the end of a unit we must store the value of
c the original timestamp.
      if(j.eq.0)fmtimeval=mtimeval
      
      call ftgcfj(masterunit,4,mfelem+j,1,1,mndwell(j+1),
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get N_DWELL MASTER value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif
      
      call ftgcfj(masterunit,5,mfelem+j,1,1,mssc(j+1),
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get SSC_NUMBER MASTER value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif
      
      call ftgcfj(masterunit,6,mfelem+j,1,1,mminchan(j+1),
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get MINCHAN MASTER value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif
      
      call ftgcfj(masterunit,7,mfelem+j,1,1,mmaxchan(j+1),
     &   flgval,anynul,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to get MAXCHAN MASTER value.')
        call fcerrm(status)
        abort = .TRUE.
        goto 999
      endif

c      Print*,'Comparing to',sfelem,stimeval,sndwell,sssc,
c     &   sminchan,smaxchan
      
c      print*,'Read row',mfelem+j,mtimeval,mndwell(j+1),mssc(j+1),
c     &   mminchan(j+1),mmaxchan(j+1)

c      print*,' '
c      print*,'COMPARING',j
c      print*,'ROWS',mfelem+j,sfelem
      
c      print*,'TIMES',mtimeval,stimeval,
c     &   mtimeval+mtzero,stimeval+stzero
      
c      print*,'N_DWELL',mndwell(j+1),sndwell
c      print*,'SSC_NUMBER',mssc(j+1),sssc
c      print*,'MINCHAN',mminchan(j+1),sminchan
c      print*,'MAXCHAN',mmaxchan(j+1),smaxchan

      if((mtimeval.lt.((stimeval+diff)+1.0d-9)).and.
     &   (mtimeval.gt.((stimeval+diff)-1.0d-9)))ltime=.TRUE.
      if(sndwell.eq.mndwell(j+1))lndwell(j+1)=.TRUE.
      if(sssc.eq.mssc(j+1))lssc(j+1)=.TRUE.
      if(sminchan.eq.mminchan(j+1))lminchan(j+1)=.TRUE.
      if(smaxchan.eq.mmaxchan(j+1))lmaxchan(j+1)=.TRUE.

c      print*,'LOGICALS are',ltime,lndwell(j+1),lssc(j+1),
c     &   lminchan(j+1),lmaxchan(j+1),j+1,fmtimeval,mtimeval,
c     &   stimeval+diff
c      print*,' COMPARING ',fmtimeval+mtzero,mtimeval+mtzero,
c     &   stimeval+stzero
c      print*,' '

      if(ltime.and.lndwell(j+1).and.lssc(j+1).and.
     &   lminchan(j+1).and.lmaxchan(j+1))then
c        print*,'EVERYTHING MATCHES'
c        print*,' '
c        call fcecho('Replacing values in MASTERFILE')
        mfelem=mfelem+j
        goto 999
      endif

c     Continue searching all TIMESTAMPS until you hit a new group of
c TIMESTAMPS. If a complete matching group hasn't been found then
c we have to go to the next step of trying to determine where to
c insert the information from the APPENDFILE to the MASTERFILE.
      if(.not.ltime)then
c        print*,'Times do not match', fmtimeval,mtimeval,ielements
        ielements=ielements-1
        goto 101
      endif
      
      goto 100

101   continue

c      print*,'INTO ASMSORT and ielements is ',ielements
      
c     Reset our counter variable
      j=ielements

c     Instead of counting upwards, count downwards (assuming all data
c is in ascending order) so as to preserve the form of the data
c in the MASTERFILE.
      do 200 k=ielements,1,-1

c       Increment counter variable to tell us how far from
c mfelem we are. We begin with J=0... 
        j=j-1

        if(sndwell.gt.mndwell(k))then
          mfelem=mfelem+j+1
c          print*,'SNDWELL GT MNDWELL',sndwell,mndwell(k),k,mfelem
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999
          
        elseif(sndwell.lt.mndwell(k))then
          mfelem=mfelem+j
c          print*,'SNDWELL LT MNDWELL',sndwell,mndwell(k),k,mfelem   
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999
          
        endif

        if(sssc.gt.mssc(k))then
          mfelem=mfelem+j+1
c          print*,'SSSC GT MSSC',sssc,mssc(k),k,mfelem
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999

        elseif(sssc.lt.mssc(k))then
          mfelem=mfelem+j
c          print*,'SSSC LT MSSC',sssc,mssc(k),k,mfelem
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999

        endif

        if(sminchan.gt.mminchan(k))then
          mfelem=mfelem+j+1
c          print*,'SMINCHAN GT MMINCHAN',sminchan,mminchan(k),k,mfelem          
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999

        elseif(sminchan.lt.mminchan(k))then
          mfelem=mfelem+j
c          print*,'SMINCHAN LT MMINCHAN',sminchan,mminchan(k),k,mfelem   
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999

        endif

        if(smaxchan.gt.mmaxchan(k))then
          mfelem=mfelem+j+1
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999

        elseif(smaxchan.lt.mmaxchan(k))then
          mfelem=mfelem+j
          call ftirow(masterunit,mfelem-1,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not add space to MASTERFILE')
            abort=.TRUE.
          endif
          imnrows=imnrows+1
          goto 999

        endif
        
200   continue

c     If we have gotten through all of the "if's" above we will
c insert a row after them. Note that we should never get to this
c point but if for some reason we do, this will handle this anomaly. 

c      mfelem=mfelem+ielements-1
      
c      call ftirow(masterunit,mfelem-1,1,status)
c      if(status.ne.0)then
c        call fcecho(' ')
c        call fcecho('Could not add space to MASTERFILE')
c        abort=.TRUE.
c        goto 999
c      endif
      
999   continue

c      print*,'Out of ASMSORT',imnrows,
c     &   mfelem,sfelem,j
      
      if(abort)then
        call fcecho(' ')
        call fcecho('Abort flag set... aborting ASMSORT')
      endif
       
      return
      end


      
