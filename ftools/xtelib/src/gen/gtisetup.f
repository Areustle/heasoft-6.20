c     This subroutine reads a specified GTI file.
      
      subroutine gtisetup(gapplyfiles,gfiles,
     &   nogtiorfiles,extval,abort)
      implicit none

      integer isiz

c      Define all of the common parameters used in the arrays.
      parameter (isiz = 999)

      character*(*) gapplyfiles(isiz),gfiles(isiz)
      character(160) file1,tempfile
      character(80) contxt
      character(40) extnam, hduclas1, hduclas2
      integer block,xtend,nogtiorfiles,fcstln,outlin,
     &   nultyp,i,j,extnum,status,iunit,extval
      logical abort
      
      character(1) dataval(10)

      data (dataval(i),i=1,10)/'0','1','2','3','4','5',
     &     '6','7','8','9'/
      
      status=0
      j=0

c      print*,'About to assign iunit'
      
c      Assign a unit file number used in inputting file.
      call ftgiou(iunit,status)
      if(status.ne.0)then
        contxt='Error getting input unit number'
        call fcecho(contxt)
        contxt='Setting to logical unit 10'
        call fcecho(contxt)
        status=0
        iunit=9
      endif

      i=0
10    continue
      
      i=i+1
      
      extnam=' '
      hduclas1=' '
      hduclas2=' '
      file1=' '
      tempfile=' '
      nultyp=0
      
c######################################################################
c      Okay now that we have the information we can actually start
c      processing the GTI files and getting the information that is
c      contained in each one. 

c      print*,'About to call fcpars'
c      Parse the input file name to extract information from it.
      call fcpars(gapplyfiles(i),file1,extnum,status)

c      print*,'gapplyfiles are ',gapplyfiles(i)
      
      if (status.ne.0)then
         contxt='Could not parse file name for GTI file'
         call fcecho(contxt)
         call fcerrm(status)
         status=0
      endif

c     Since the GTI information can be in any extension
c     we check the "extnum" and if it is less than 1 we
c     force it to 2.
c      print*,'Value from fcpars is ',extnum
      
      if(extval.gt.0)then
c        print*,'Going to extension',extval
        extnum=extval
      else
        if(extnum.lt.1)extnum=2
      endif

c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.

      call ftopen(iunit,file1,0,block,status)
      if(status.ne.0)then
         contxt='Failure to open input file for GTI.'
         call fcerr(contxt)
         call fcerrm(status)
         status=0
         call ftclos(iunit,status)
         if(status.ne.0)then
           contxt='Error closing input number'
           call fcecho(contxt)
           status=0
         endif
         call ftfiou(iunit,status)
         if(status.ne.0)then
           contxt='Error freeing logical input number'
           call fcecho(contxt)
           status=0
         endif
      endif

c      Skip the primary header and go to the second (or extnum)
c      to read all pertinent GTI processing information - skipping the
c      first extension to go to the GTI extension.

100   continue

c      print*,'Moving to extnum value ',extnum+1
      
      call ftmahd(iunit,extnum+1,xtend,status)
      if(status.ne.0)then
         contxt='Error moving to extnum in GTI files'
         call fcecho(contxt)
         abort=.TRUE.
         status=0
         goto 999
      endif

      call ftgkys(iunit,'EXTNAME',extnam,contxt,status)
      if(status.ne.0)then
c        call fcecho(' ')
c        contxt='Could not find keyword EXTNAM'
c        call fcecho(contxt)
        status=0
      endif

      call ftgkys(iunit,'HDUCLAS1',hduclas1,contxt,status)
      if(status.ne.0)then
c        call fcecho(' ')
c        contxt='Could not find keyword HDUCLAS1'
c        call fcecho(contxt)
        status=0
      endif

      call ftgkys(iunit,'HDUCLAS2',hduclas2,contxt,status)
      if(status.ne.0)then
c        call fcecho(' ')
c        contxt='Could not find keyword HDUCLAS2'
c        call fcecho(contxt)
        status=0
      endif

c      print*,'extnam is ',extnam
c      print*,'hduclas1 is ',hduclas1
c      print*,'hduclas2 is ',hduclas2
      
      if(extnam.ne.'GTI'.and.hduclas1.ne.'GTI')then
        call fcecho(' ')
        call fcecho('Failed to find GTI extension as HDUCLAS1 value.')
        call fcecho('This is probably NOT XTE data. Continuing without')
        call fcecho('applying GTI information for any input file.')
        call fcecho(' ')
        call fcecho('You can over-ride this by entering the GITORFILE')
        call fcecho('parameter explicitly on the command-line.')
        call fcecho('Entering GTIORFILEs explicitly is recommended.')
        call fcecho(' ')
        abort=.TRUE.
        goto 999
      else
        j=j+1
        tempfile=gapplyfiles(i)
        outlin=fcstln(tempfile)
        gfiles(j)=tempfile(:outlin) // '+2'
      endif
      
      if(status.ne.0)then
         status=0
      endif

999   continue
      
      call ftclos(iunit,status)
      if(status.ne.0)then
        contxt='Error closing GTI file'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

c      print*,'abort value is ',abort
c      print*,'nogtiorfiles and j are',nogtiorfiles,j

      if(i.lt.nogtiorfiles.and.(.not.abort))goto 10
      if(.not.abort)nogtiorfiles=j
      
      call ftfiou(iunit,status)
      if(status.ne.0)then
        contxt='Error freeing logical output number'
        call fcecho(contxt)
        status=0
      endif

      call fcecho(' ')
      call fcecho('The files input with GTI (ORFILE) information are:')
      do i=1,nogtiorfiles
        call fcecho(gapplyfiles(i))
      enddo
      
      return
      end
