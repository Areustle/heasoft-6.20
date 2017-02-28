
*****************************************************************************
C      chktime
C
C DESCRIPTION: 
C     Checks time range for multiple files and sorts them such that
c     they are in ascending order for TSTART value as read in. 
C     Assumes that we are scanning a BINTABLE file.
C
C AUTHOR:  
C      Brian K. Elza 12/93 
C
C MODIFICATION HISTORY:
C	None
C
C
C ARGUMENTS:
C     files   c- array containing the file names must < 100
c     objects a- output list of OBJECTS from that keyword in each file
c     obstart c- input alternative TSTART keyword with starting time
c     obstop  c- input alternative TSTOP keyword with starting time
c     obsdate c- input observation start date keyword
c     mjdref  d- output MJDREF value from the files - should be identical.
c     imjdref i- output MJDREFI value from the files - should be identical.
c     tmjds  da- output contains that start times of the files sorted in
c                ascending order in seconds. The files are rearranged
c                in this order.
c     tmjde  da- output contains that stops times in seconds of the files 
C     no      i- number of files in files
C     status  i- output status of results, did everything work in fitsio
C     
C
C PRIMARY LOCAL VARIABLES:
c      file1    - first file in list
c      filen    - 2nd, 3rd,... or nth file in list

c      extnum     - data unit number to check, 1 = primary, etc
c      xtend    - data type of unit number checked
C      word1    - returned word1 value for KEYWORD of the first file
c      wordn    - returned wordn value for KEYWORD of the nth file
c      contxt   - error message
c      cval     - conversion of integer to *20 sting to print
C
c SUBROUTINE CALLS
c     ftgiou - get a free logical unit number from a file
c     fcpars - parse a file name into file name and extension
c     ftopen - open a FITS file
c     ftclos - close a FITS file
c     ftmahd - skip to a particular FITS extension
c     ftgkys - read a particular KEYWORD string value
c     ftgkyd - read a particular KEYWORD double precision value      
c     ftfiou - free up a logical unit number for reuse
c     fcgcls - parse input to get actual input file names
c     fcecho - echo out the string to STDOUT
c     fcerr  - echo out error message to STDERR
c     fcerrm - write out the error message associated with a status
c     timeconvert - convert all times into seconds
***************************************************************************** 

      subroutine chktime(files,objects,obstart,obstop,obsdate,
     &   mjdref,imjdref,tmjds,tmjde,no,status)
c      implicit none
      integer isiz
      parameter (isiz=1000)
      integer no,extnum,xtend,i,j,k,imjdref,istart,istop,
     &   itimezero
      character*(*) objects(isiz),obstart,obstop,obsdate
      double precision fulls(isiz),fulle(isiz),mjdref,
     &   conversion, timezero, timeresolution
        
      character(160) files(*),file1,filen,filetemp(isiz)
      character(80) contxt,timeunit,timesys1,timesysn
      
      double precision big,maxit, 
     &   tmjds(*),tmjde(*)

      integer status,iunit1,iunitn,block

      logical ok,lfxbary(isiz),lfxtime(isiz),abort, lmultiple

      common/multiple/lmultiple
      common/timeres/timeresolution

      timeresollution=0.0d0
      abort=.FALSE.
      istart=0
      istop=0
      mjdref=0.0d0
      itimezero=0
      timezero=0.0d0
      imjdref=0
      ok = .FALSE.
      status = 0
      k = 0

      do i=1,isiz
        lfxbary(i)=.FALSE.
        lfxtime(i)=.FALSE.
        fulle(i)=0.0d0
        fulls(i)=0.0d0
      enddo
      
c      Assign a unit file number used in inputting file.
      call ftgiou(iunit1,status)
      if(status.ne.0)then
        contxt='Error getting input unit number'
        call fcecho(contxt)
        contxt='Setting to logical unit 10'
        call fcecho(contxt)
        status=0
        iunit1=10
      endif

c      Assign a unit file number used in inputting file.
      call ftgiou(iunitn,status)
      if(status.ne.0)then
        contxt='Error getting input unit number'
        call fcecho(contxt)
        contxt='Setting to logical unitn 11'
        call fcecho(contxt)
        status=0
        iunitn=11
      endif
        
      call fcpars(files(1),file1,extnum,status)
      if (status .ne. 0) then
        contxt = 'unable to parse filename'
        call fcerrm(status)
        call fcerr(contxt)
        return
      endif

c      If no extension is found set the extension to number 2
      if (extnum .eq. -99) extnum = 1
      
      call ftopen(iunit1,file1,0,block,status)
      if (status .ne. 0) then
        contxt = 'unable to open first file in infile'
        call fcerrm(status)
        call fcerr(contxt)
        call ftclos(iunit1,status)
        if(status.ne.0)then
          contxt='Error closing input number'
          call fcecho(contxt)
          status=0
        endif
           
        call ftfiou(iunit1,status)
        if(status.ne.0)then
          contxt='Error freeing output unit number'
          call fcecho(contxt)
          status=0
        endif
        return
      endif

c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status).
c      Move to the correct extension number.
      call ftmahd(iunit1,extnum+1,xtend,status)

c     Read in the value that goes with the assigned keyword
      call ftgkys(iunit1,'TIMESYS',timesys1,contxt,status)
      if(status.ne.0)then
        contxt='Could not find keyword TIMESYS'
        call fcecho(contxt)
        contxt='This file was not created properly.'
        call fcecho(contxt)
        contxt='Please check this file. TIMESYS is missing:'
        call fcecho(contxt)
        call fcecho(file1)
        contxt='Cannot continue with invalid file... Aborting'
        call fcecho(contxt)
        stop
      endif

      call ftgkyd(iunit1,'TIMEDEL',timeresolution,contxt,status)
      if(status.ne.0)then
        call fcecho('Could not find reference for TIMEDEL')
        status=0
        if(.not.lmultiple)then
          call fcecho('Proceeding with TIMERESOLUTION set to 0.0d0')
          timeresolution=0.0d0
        else
          call fcecho('You specified that each light-bin')
          call fcecho('is to be an even multiple')
          call fcecho('of the minimum time resolution')
          call fcecho('but this file does not contain this')
          call fcecho('information. Cannot continue!')
          call fcecho(' ')
          call fcecho('Aborting...Enter BINSIZE by hand!')
          abort=.TRUE.
          goto 999
        endif
      endif

      
      call ftgkys(iunit1,'TIMEUNIT',timeunit,contxt,status)
      if(status.ne.0)then
        contxt='Could not find keyword TIMEUNIT'
        call fcecho(contxt)
        contxt='Assuming all inputs in SECONDS'
        call fcecho(contxt)
        timeunit='s'
        status=0
      endif

      call timeconvert(timeunit,conversion,status)
      if(status.ne.0)then
        contxt='Could not determine TIMEUNIT conversion'
        call fcecho(contxt)
        contxt='Assuming all inputs are in seconds'
        call fcecho(contxt)
        conversion=1.0d0
        status=0
      endif

      call ftgkyd(iunit1,'TSTART',fulls(1),contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(iunit1,'TSTARTI',istart,contxt,status)
        call ftgkyd(iunit1,'TSTARTF',fulls(1),contxt,status)
        if(status.eq.0)then
          fulls(1) = fulls(1)+dfloat(istart)
          istart=0
        endif

        if(status.ne.0)then
          status=0
          call ftgkyd(iunit1,obstart,fulls(1),contxt,status)
        endif
        
      endif

      if(status.ne.0)then
        contxt='Could not find keyword TSTARTn'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftgkyd(iunit1,'TSTOP',fulle(1),contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(iunit1,'TSTOPI',istop,contxt,status)
        call ftgkyd(iunit1,'TSTOPF',fulle(1),contxt,status)
        if(status.eq.0)then
          fulle(1) = fulle(1)+dfloat(istop)
          istop=0
        endif

        if(status.ne.0)then
          status=0
          call ftgkyd(iunit1,obstop,fulle(1),contxt,status)
        endif
        
      endif
c      print*,'fulle(1) is ',fulle(1)

      if(status.ne.0)then
        contxt='Could not find keyword TSTOPn'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftgkyd(iunit1,'TIMEZERO',timezero,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(iunit1,'TIMEZERI',itimezero,contxt,status)
        call ftgkyd(iunit1,'TIMEZERF',timezero,contxt,status)
        if(status.eq.0)then
          timezero = timezero+dfloat(itimezero)
          itimezero = 0
        endif
      endif

      if(status.ne.0)then
        contxt='No keyword TIMEZERO, TIMEZERI, or TIMEZERF found'
        call fcecho(contxt)
        call fcecho('Setting TIMEZERO offset to 0.0d0')
        timezero=0.0d0
        status=0
      endif

c     Now that we have the TIMEZERO offset add it to the TSTART and TSTOP
c values. 
      fulls(1)=fulls(1)+timezero
      fulle(1)=fulle(1)+timezero
      
c      print*,'fulle(1) is ',fulle(1)

      call ftgkys(iunit1,'OBJECT',objects(1),contxt,status)
      if(status.ne.0)then
c        contxt='Could not find keyword OBJECT
c        call fcecho(contxt)
        status=0
      endif
c      print*,'fulle(1) is ',fulle(1)

      call ftgkyd(iunit1,'MJDREF',mjdref,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(iunit1,'MJDREFI',imjdref,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for MJDREFI')
          call fcecho('Proceeding with MJDREFI set to 0')
          imjdref=0
          status=0
        endif
        call ftgkyd(iunit1,'MJDREFF',mjdref,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for MJDREFF')
          call fcecho('Proceeding with MJDREFF set to 0.0d0')
          mjdref=0.0d0
          status=0
        endif
      endif

      call ftgkyl(iunit1,'FXBARY',lfxbary(1),contxt,status)
      if(status.ne.0)then
        lfxbary(1)=.FALSE.
        status=0
      endif

      call ftgkyl(iunit1,'FXTIME',lfxtime(1),contxt,status)
      if(status.ne.0)then
        lfxtime(1)=.FALSE.
        status=0
      endif
      
c      Close the FITS file
      call ftclos(iunit1,status)
      if(status.ne.0)then
        contxt='Could not close first file'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

c      Make use of the timing information in there files to
c      figure out how to rearrange them into sequential time order
      fulls(1)=fulls(1)*conversion
      fulle(1)=fulle(1)*conversion

      if (no.eq.1)then
        if(lfxbary(1))then
          call fcecho(' ')
          call fcecho('BARYCENTER corrected file being processed.')
          if(lfxtime(1))then
            call fcecho('Time column contains CORRECTED times.')
            call fcecho(' ')
            call fcecho('*****************************************')
            call fcecho('             WARNING                   ')
            call fcecho('Processing an SA file with the TIME')
            call fcecho('column overwritten with barycenter times')
            call fcecho('can cause a loss of accuracy, if a CDLT')
            call fcecho('is present in these files!')
            call fcecho(' ')
            call fcecho('There is NO WAY to correct for this!')
            call fcecho('If this is a problem, you will have to')
            call fcecho('process uncorrected files first.')
            call fcecho(' ')
            call fcecho('No farther warnings will be given!')
            call fcecho('*****************************************')
          else
            call fcecho('BARYTIME column contains CORRECTED times.')
          endif
        endif
      endif
      
      if (no .gt. 1) then
        do 10 i = 2, no
          
c     Parse the filename
          call fcpars(files(i),filen,extnum,status)
          if(status.ne.0)then
            contxt='Could not parse the first file name'
            call fcerr(contxt)
            call fcerrm(status)
            status=0
          endif

c      print*,'files(i) and filen is ',files(i),filen,
c     &             extnum,status

c      If the extnum is not included then set it to the second
          if (extnum .eq. -99) extnum = 1

c      Open the file
c          print*,'Iunitn is ',iunitn,filen
          
          call ftopen(iunitn,filen,0,block,status)
          if (status .ne. 0) then
            call fcerrm(status)
            call fcerr('Unable to open file number')
            call ftclos(iunitn,status)
            if(status.ne.0)then
              contxt='Error closing input number'
              call fcecho(contxt)
              status=0
            endif
      
            call ftfiou(iunitn,status)
            if(status.ne.0)then
              contxt='Error freeing input unit number'
              call fcecho(contxt)
              status=0
            endif
            return
          endif

c      Get the other strings assigned to specific keywords.
          call ftmahd(iunitn,extnum+1,xtend,status)           
          if(status.ne.0)then
            contxt='Error moving to extnum'
            call fcerr(contxt)
            call fcerrm(status)
            status=0
          endif

          call ftgkys(iunitn,'TIMEUNIT',timeunit,contxt,status)
          if(status.ne.0)then
            contxt='Could not find keyword TIMEUNIT'
            call fcecho(contxt)
            contxt='Assuming all inputs in SECONDS'
            call fcecho(contxt)
            timeunit='s'
            status=0
          endif

          call ftgkys(iunitn,'TIMESYS',timesysn,contxt,status)
          if(status.ne.0)then
            contxt='Could not find keyword TIMESYS'
            call fcecho(contxt)
            contxt='This file was not created properly.'
            call fcecho(contxt)
            contxt='Please check this file TIMESYS is missing:'
            call fcecho(contxt)
            call fcecho(filen)
            contxt='Cannot continue with invalid file... Aborting'
            call fcecho(contxt)
            stop
          endif

c     Check to be sure that timesys's are equal
          if(timesys1.ne.timesysn)then
            contxt='************** ERROR **************'
            call fcecho(contxt)
            contxt='TIMESYS keywords DO NOT MATCH!!!'
            call fcecho(contxt)
            contxt='Your files are not compatible!'
            call fcecho(contxt)
            contxt='Please check file:'
            call fcecho(contxt)
            call fcecho(filen)
            contxt='Not compatible with:'
            call fcecho(contxt)
            call fcecho(file1)
            contxt='Cannot continue.... Aborting'
            call fcecho(contxt)
            stop
          endif
          
          call timeconvert(timeunit,conversion,status)
          if(status.ne.0)then
            contxt='Could not determine TIMEUNIT conversion'
            call fcecho(contxt)
            contxt='Assuming all inputs are in seconds'
            call fcecho(contxt)
            conversion=1.0d0
            status=0
          endif

          call ftgkyd(iunitn,'TSTART',fulls(i),contxt,status)
          if(status.ne.0)then
            status=0
            call ftgkyj(iunitn,'TSTARTI',istart,contxt,status)
            call ftgkyd(iunitn,'TSTARTF',fulls(i),contxt,status)
            if(status.eq.0)then
              fulls(i) = fulls(i)+dfloat(istart)
              istart=0
            endif

            if(status.ne.0)then
              status=0
              call ftgkyd(iunitn,obstart,fulls(i),contxt,status)
            endif
            
          endif

          if(status.ne.0)then
            contxt='Could not find keyword TSTARTn'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif

          call ftgkyd(iunitn,'TSTOP',fulle(i),contxt,status)
          if(status.ne.0)then
            status=0
            call ftgkyj(iunitn,'TSTOPI',istop,contxt,status)
            call ftgkyd(iunitn,'TSTOPF',fulle(i),contxt,status)
            if(status.eq.0)then
              fulle(i) = fulle(i)+dfloat(istop)
              istop=0
            endif

            if(status.ne.0)then
              status=0
              call ftgkyd(iunitn,obstop,fulle(i),contxt,status)
            endif
            
          endif

          if(status.ne.0)then
            contxt='Could not find keyword TSTOPn'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif

          timezero=0.0d0

          call ftgkyd(iunitn,'TIMEZERO',timezero,contxt,status)
          if(status.ne.0)then
            status=0
            call ftgkyj(iunitn,'TIMEZERI',itimezero,contxt,status)
            call ftgkyd(iunitn,'TIMEZERF',timezero,contxt,status)
            if(status.eq.0)then
              timezero = timezero+dfloat(itimezero)
              itimezero = 0
            endif
          endif

          if(status.ne.0)then
            contxt='No keyword TIMEZERO, TIMEZERI, or TIMEZERF found'
            call fcecho(contxt)
            call fcecho('Setting TIMEZERO offset to 0.0d0')
            timezero=0.0d0
            status=0
          endif

          call ftgkyl(iunitn,'FXBARY',lfxbary(i),contxt,status)
          if(status.ne.0)then
            lfxbary(i)=.FALSE.
            status=0
          endif

          call ftgkyl(iunitn,'FXTIME',lfxtime(i),contxt,status)
          if(status.ne.0)then
            lfxtime(i)=.FALSE.
            status=0
          endif

c     Now that we have the TIMEZERO offset add it to the TSTART and TSTOP
c values. 
          fulls(i)=fulls(i)+timezero
          fulle(i)=fulle(i)+timezero

          if(i.le.100)then
            call ftgkys(iunitn,'OBJECT',objects(i),contxt,status)
            if(status.ne.0)then
c              contxt='Could not find keyword OBJECT'
c              call fcecho(contxt)
              status=0
            endif
          endif

c      Close up all files that were opened to obtain this
c      initial information
          call ftclos(iunitn,status)
          if(status.ne.0)then
            contxt='Error closing other input files'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif

c      Make use of the timing information in there files to
c      figure out how to rearrange them into sequential time order
          fulls(i)=fulls(i)*conversion
          fulle(i)=fulle(i)*conversion
              
10      continue

c     Since it is now possible to BARYCENTER correct RAW XTE data,
c we have to check to be sure that people aren't doing stupid things,
c like running corrected and uncorrected files through this code at
c the same time - thus producing garbage. Don't laugh, the reason we
c are doing this is because someone already did it!!! And that was when
c you had to "trick" the barycenter code to work on RAW data files.
c So you had to KNOW that you just changed the Time column to be the
c BARYTIME column and then attempt to process that file with other
c unmodified files. It is now much easier for such foolishness... 
c Kind of reaffirms your faith in the stupidity of Mankind (or Womankind
c if we are to be PC).

        do i=2,no
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
          if(lfxbary(i).neqv.lfxbary(1))then
            call fcecho(' ')
            call fcecho('Check on FXBARY fails!')
            call fcecho('BARYCENTER keywords differ!!!')
            call fcecho('You are attempting to mix corrected')
            call fcecho('and uncorrected files. This cannot be')
            call fcecho('allowed... Aborting.. ')
            abort=.TRUE.
            goto 999
          else
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
            if(lfxtime(i).neqv.lfxtime(1))then
              call fcecho(' ')
              call fcecho('Check on FXTIME fails!')
              call fcecho('BARYCENTER Time keywords differ!!!')
              call fcecho('You are attempting to mix corrected')
              call fcecho('and uncorrected TIME columns. This ')
              call fcecho('cannot be allowed... Aborting.. ')
              abort=.TRUE.
              goto 999
            endif
          endif
        enddo

        if(lfxbary(1))then
          call fcecho(' ')
          call fcecho('All input files have been BARYCENTER')
          call fcecho('corrected.')
          if(lfxtime(1))then
            call fcecho('Time column contains CORRECTED times')
          else
            call fcecho('BARYTIME column contains CORRECTED times.')
          endif
        endif

      endif
      
c      call fcecho('After rearrangement we have')

c      Arrange the observation times in ascending order so that
c      when we are dealing with specifically input time intervals we
c      can search files in ascending order. Note we arrange all files
c      into order according to the beginning of the observation time.
           
      big=1.0D+14
      
      do 20 i=1,no
        maxit=big
        
        do 30 j=1,no

          if(maxit.ge.fulls(j))then
            maxit=fulls(j)
            k=j
          endif
          
30      continue

c      Store things into temporary arrays such that they are now
c      in the proper time order.

        filetemp(i)=files(k)
        tmjds(i)=fulls(k)
        tmjde(i)=fulle(k)
        fulls(k)=big
        
20    continue

c      Now move all of the files back into the original arrays.
      do 40 i=1,no
        
        files(i)=filetemp(i)

c        print*,'Start time and end times are',tmjds(i),tmjde(i)
              
40    continue
           

c      Check to see if any of the files have overlapping time
c      intervals and farther check that the object being observed
c      is different. If the time intervals of the files overlap
c      and the object being observed the same we will have deal with
c      this at a later time. If the time intervals overlap and the
c      objects observed are the same, the total number of counts
c      during that time interval MUST be the same. We will check for
c      this when we begin to process that data from these files...
        
      if(no.gt.1)then
        do 71 j=1,no
          do 70 i=j,no
            if(i.ne.j)then
              if(no.le.100)then
                if (tmjds(j).gt.tmjde(i).and.
     &             objects(j).eq.objects(i)) then 
                  call fcecho('WARNING - time intervals overlap')
                endif
              else
                if (tmjds(j).gt.tmjde(i))then 
                  call fcecho('WARNING - time intervals overlap')
                endif
              endif
            endif
70        continue
71      continue
      endif

999   continue
      
      call ftfiou(iunit1,status)
      if(status.ne.0)then
        contxt='Error freeing output unit number'
        call fcecho(contxt)
        status=0
      endif

      if(no.gt.1)then
        call ftfiou(iunitn,status)
        if(status.ne.0)then
          contxt='Error freeing output unit number'
          call fcecho(contxt)
          status=0
        endif
      endif

      if(abort)then
        status=9999
      endif
      
      return
      end
        
