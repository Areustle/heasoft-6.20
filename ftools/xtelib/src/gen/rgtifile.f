c     This subroutine reads a specified GTI file.
      
      subroutine rgtifile(gfile,obstart,obstop,gtistart,
     &     gtistop,tstart,tstop,gstart,gstop,igtino)
      implicit none

      integer nf,nb,isiz,igtino

c      Define all of the common parameters used in the arrays.
      parameter (nb = 512)
      parameter (nf = 258)
      parameter (isiz = 100)

      character*(*) gfile,obstart,obstop,gtistart,gtistop
      character(160) file1
      character(80) contxt,timeunit
      character(40) ttype(nb),tform(nb),tunit(nb),extnam
      integer block,xtend,nrows,nfield,pcount,jrows,
     &   startpos,stoppos,lrow,jrow,nultyp,j,extnum,status,iunit,
     &   itstart,itstop,itimezero,lcount
      character(20) cval
      logical exact,anynul,flgval,lstartrange, lstoprange
      double precision nulvald,gstart(*),gstop(*),tstart,tstop,
     &   conversion,timezero,early,late

      status=0
      tstart=0.0d0
      tstop=0.0d0
      timezero=0.0d0
      early=0.0d0
      late=0.0d0
      itstart=0
      itstop=0
      itimezero=0
      lstartrange=.FALSE.
      lstoprange=.FALSE.
      exact=.FALSE.
      igtino=0
      nrows=0
      jrows=0
      jrow=0
      lrow=0
      
      cval=' '
      
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
      
      nulvald=0.0d0
      nultyp=0
      
c######################################################################
c      Okay now that we have the information we can actually start
c      processing the GTI files and getting the information that is
c      contained in each one. 

      j=0
      
c      Parse the character input file name 
      call fcpars(gfile,file1,extnum,status)
      if (status.ne.0)then
         contxt='Could not parse file name for GTI file'
         call fcecho(contxt)
         call fcerrm(status)
         status=0
      endif

c      Since the GTI information can be in any extension
c      we check the "extnum" and if it is less than 1 we
c      force it to 1. 
      if (extnum.lt.1) extnum=1

c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.
      call ftopen(iunit,file1,0,block,status)
      if(status.ne.0)then
         contxt='Failure to open input GTI file.'
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
c      to read all pertinent processing information.
      call ftmahd(iunit,extnum+1,xtend,status)
      if(status.ne.0)then
         contxt='Error moving to extnum in GTI files'
         call fcerr(contxt)
         call fcerrm(status)
         status=0
      endif

      call ftgkys(iunit,'TIMEUNIT',timeunit,contxt,status)
      if(status.ne.0)then
        call fcecho(' ')
        contxt='Could not find keyword TIMEUNIT'
        call fcecho(contxt)
        contxt='Assuming all inputs in SECONDS'
        call fcecho(contxt)
        timeunit='s'
        status=0
      endif

      call timeconvert(timeunit,conversion,status)
      if(status.ne.0)then
        call fcecho(' ')
        contxt='Could not determine TIMEUNIT conversion'
        call fcecho(contxt)
        contxt='Assuming all inputs are in seconds'
        call fcecho(contxt)
        conversion=1.0d0
        status=0
      endif
      
c      Read the information about how the data is stored - see the
c      fitsio.for file for a description of this call. 
      call ftghbn(iunit,nb,jrows,nfield,ttype,
     &     tform,tunit,extnam,pcount,status)
      
c      Print out any error information about accessing the files
      if (status.ne.0)then
        call fcecho(' ')
        contxt='Could not find TFIELDS from GTI'
        call fcecho(contxt)
        call fcerrm(status)
        return
      endif

      if(jrows.le.0)then
        call fcecho(' ')
        call fcecho('This GTI file contains NO data!')
        call fcecho('Aborting processing of this GTI file.')
        return
      endif
        
      call ftgkyd(iunit,'TSTART',tstart,contxt,status)
      if(status.ne.0)then
        
        status=0
        call ftgkyj(iunit,'TSTARTI',itstart,contxt,status)
        call ftgkyd(iunit,'TSTARTF',tstart,contxt,status)
        if(status.eq.0)then
          tstart = tstart+dfloat(itstart)
          itstart=0
        endif
      
        if(status.ne.0)then
          status=0
          call ftgkyd(iunit,obstart,tstart,contxt,status)
        endif
      endif
      
           
      if(status.ne.0)then
c         contxt='Could not find keyword TSTART'
c         call fcecho(contxt)
c         call fcerrm(status)
         status=0
      endif

      call ftgkyd(iunit,'TSTOP',tstop,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(iunit,'TSTOPI',itstop,contxt,status)
        call ftgkyd(iunit,'TSTOPF',tstop,contxt,status)
        if(status.eq.0)then
          tstop = tstop+dfloat(itstop)
          itstop=0
        endif
      
        if(status.ne.0)then
          status=0
          call ftgkyd(iunit,obstop,tstop,contxt,status)
        endif
      endif
                 
      if(status.ne.0)then
c         contxt='Could not find keyword TSTOP'
c         call fcecho(contxt)
c         call fcerrm(status)
         status=0
      endif

      call ftgkyd(iunit,'TIMEZERO',timezero,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(iunit,'TIMEZERI',itimezero,contxt,status)
        call ftgkyd(iunit,'TIMEZERF',timezero,contxt,status)
        if(status.eq.0)then
          timezero = timezero+dfloat(itimezero)
          itimezero=0
        endif
      endif

c     Add TIMEZERO correction to TSTART and TSTOP
      tstart=tstart+timezero
      tstop=tstop+timezero
      
c      Find out the column number that is associated with the
c      time column (timecol) and counts column (columns) in the next
c      two subroutine calls

      call ftgcno(iunit,exact,'START',startpos,status)
      if(status.ne.0)then
         status=0
         call ftgcno(iunit,exact,gtistart,startpos,status)
      endif
           
      if(status.ne.0)then
        call fcecho(' ')
        contxt='Could not find START column number'
        call fcecho(contxt)
        contxt='aborting... cannot continue'
        call fcecho(contxt)
        call fcerrm(status)
      endif

      call ftgcno(iunit,exact,'STOP',stoppos,status)
      if(status.ne.0)then
         status=0
         call ftgcno(iunit,exact,gtistop,stoppos,status)
      endif
           
      if(status.ne.0)then
        call fcecho(' ')
        contxt='Could not find STOP column number'
        call fcecho(contxt)
        contxt='Check input data'
        call fcecho(contxt)
        contxt='aborting... cannot continue'
        call fcecho(contxt)
        call fcerrm(status)
      endif

      jrow=0
      
      do jrow=1,jrows
        lrow=lrow+1
c      Read in a double precision time element stored in
c      the input file into "timearay" 
c       call ftgcld(iunit,startpos,lrow,1,1,1,
c    &     nultyp,nulvald,gstart(lrow),flgval,anynul,status)
c      MJT -- 14Jan98:
c      Changing these to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(iunit,startpos,lrow,1,1,
     &       nulvald,gstart(lrow),anynul,status)
         
c       call ftgcld(iunit,stoppos,lrow,1,1,1,
c    &     nultyp,nulvald,gstop(lrow),flgval,anynul,status)
        call ftgcvd(iunit,stoppos,lrow,1,1,
     &       nulvald,gstop(lrow),anynul,status)

c     Since so many people create time intervals with START=STOP
c     we have to check for that separately and FIX it.

        if(gstart(lrow).eq.gstop(lrow))then

          lrow=lrow-1
          gstart(lrow)=0.0d0
          gstop(lrow)=0.0d0
          
        else
          
          gstart(lrow)=gstart(lrow)+timezero
          gstart(lrow)=gstart(lrow)*conversion

          gstop(lrow)=gstop(lrow)+timezero
          gstop(lrow)=gstop(lrow)*conversion

        endif
         
      enddo

c     Since we have "high-jacked" nrows to mean the number of VALID
c     rows we have to set it equal to the number of rows that
c     actually passed our check. 
      nrows=lrow

      if(lrow.le.0)then
        call fcecho(' ')
        call fcecho('This GTI file contains NO data!')
        call fcecho('Aborting processing of this GTI file.')
        return
      endif
      
      if(status.ne.0)then
        call fcecho(' ')
        contxt='Error in reading in GTI values'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif
           
      call ftclos(iunit,status)
      if(status.ne.0)then
        contxt='Error closing GTI file'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftfiou(iunit,status)
      if(status.ne.0)then
        contxt='Error freeing logical output number'
        call fcecho(contxt)
        status=0
      endif

      do lrow=1,nrows

        if(gstop(lrow).le.gstart(lrow))then
          call fcecho(' ')
          call fcecho('Your GTIs are out of order!')
          call fcecho('This GTI file was NOT properly')
          call fcecho('created. Aborting.')
          stop
        endif
        
        if(lrow.gt.1)then
          if(gstart(lrow).lt.gstop(lrow-1))then
            call fcecho(' ')
            call fcecho('Your GTIs have overlapping time')
            call fcecho('intervals. This GTI file was NOT')
            call fcecho('properly created! Cannot continue.')
            call fcecho('Aborting.')
            stop
          endif
          
        endif
        
        if((tstart.gt.0.0d0).and.
     &     (tstart.gt.gstart(lrow)))then
          lstartrange=.TRUE.
        endif

        if((tstop.gt.0.0d0).and.
     &     (tstop.lt.gstop(lrow)))then
          lstoprange=.TRUE.
        endif
      enddo

      if(lstartrange.or.lstoprange)then
        call fcecho(' ')

        if(lstartrange)then
          call fcecho('GTI time-range is outside of TSTART+TIMEZERO!')
          cval=' '
          contxt=' '
          contxt(1:50)='Specified TSTART + TIMEZERO for this file is:'
          call ftd2f(tstart,8,cval,status)
          contxt(51:71)=cval(1:20)
        endif
        
        if(lstoprange)then
          call fcecho('GTI time-range is outside of TSTOP+TIMEZERO!')
          cval=' '
          contxt=' '
          contxt(1:50)='Specified TSTOP + TIMEZERO for this file is:'
          call ftd2f(tstop,8,cval,status)
          contxt(51:71)=cval(1:20)
        endif

        call fcecho('Check your GTI file! ')

        call fcecho('The GTIs should be braketed by ')
        call fcecho('TSTART + TIMEZERO and TSTOP + TIMEZERO,')
        call fcecho('but the GTIs in your file do not obey')
        call fcecho('this convention!')
        call fcecho(' ')
        call fcecho('The code will attempt to compensate')
        call fcecho('for this problem, by REMOVING GTIs')
        call fcecho('which fall outside of the specified range:')
        call fcecho('  TSTART + TIMEZERO        TSTOP + TIMEZERO:')

        cval=' '
        contxt=' '
        call ftd2f(tstart,8,cval,status)
        contxt(1:20)=cval(1:20)
        cval=' '
        call ftd2f(tstop,8,cval,status)
        contxt(25:45)=cval(1:20)
        call fcecho(contxt)
        cval=' '
        call fcecho(' ')
        call fcecho('CHECK YOUR RESULTS RIGOROUSLY!!!')
        call fcecho(' ')
        call fcecho('If no data passes your filtering criteria,')
        call fcecho('it was because your GTI files are not correct.')
      endif

      if(lstartrange.or.lstoprange)then
        lcount=0
        do lrow = 1,nrows

c         Is this GTI earlier than TSTART value?
          if(tstart.gt.gstart(lrow).and.
     &       tstart.gt.gstop(lrow))then

            cval=' '
            contxt=' '
            call ftd2f(gstart(lrow),8,cval,status)
            contxt(1:20)=cval(1:20)
            cval=' '
            call ftd2f(gstop(lrow),8,cval,status)
            contxt(25:45)=cval(1:20)
            call fcecho(' ')
            call fcecho('Processing GTI TIME-RANGE:')
            call fcecho(contxt)
            cval=' '
            
            call fcecho('Entire GTI range is earlier than TSTART.')
            call fcecho('REMOVING this GTI time range!')
            goto 100
          endif
          

c         Is this GTI later than TSTOP value?
          if(tstop.lt.gstop(lrow).and.
     &       tstop.lt.gstart(lrow))then

            cval=' '
            contxt=' '
            call ftd2f(gstart(lrow),8,cval,status)
            contxt(1:20)=cval(1:20)
            cval=' '
            call ftd2f(gstop(lrow),8,cval,status)
            contxt(25:45)=cval(1:20)
            call fcecho(' ')
            call fcecho('Processing GTI TIME-RANGE:')
            call fcecho(contxt)
            cval=' '
            
            call fcecho('Entire GTI range is later than TSTOP.')
            call fcecho('REMOVING this GTI time-range!')
            goto 100
          endif

c         Is the start greater than GTI's start time and
c the stop time less than GTI's stop time, if so set the
c GTI to be TSTART and TSTOP and jump out of this iteration.
          if(tstart.gt.gstart(lrow).and.
     &       tstop.lt.gstop(lrow))then
            lcount=lcount+1
            gstart(lcount)=tstart
            gstop(lcount)=tstop

            cval=' '
            contxt=' '
            call ftd2f(gstart(lrow),8,cval,status)
            contxt(1:20)=cval(1:20)
            cval=' '
            call ftd2f(gstop(lrow),8,cval,status)
            contxt(25:45)=cval(1:20)
            call fcecho(' ')
            call fcecho('Processing GTI TIME-RANGE:')
            call fcecho(contxt)
            cval=' '

            call fcecho('GTI range brackets TSTART and TSTOP.')
            call fcecho('Setting GTI range to be TSTART and TSTOP.')
            call fcecho('Jumping out of loop.')
            goto 200
          endif

c         Is the start greater than the GTI's start time
c and less than the stop time? If so set GTI's start to be the
c same as TSTART and continue.
          if(tstart.gt.gstart(lrow).and.
     &       tstart.lt.gstop(lrow))then
            lcount=lcount+1
            gstart(lcount)=tstart
            gstop(lcount)=gstop(lrow)

            cval=' '
            contxt=' '
            call ftd2f(gstart(lrow),8,cval,status)
            contxt(1:20)=cval(1:20)
            cval=' '
            call ftd2f(gstop(lrow),8,cval,status)
            contxt(25:45)=cval(1:20)
            call fcecho(' ')
            call fcecho('Processing GTI TIME-RANGE:')
            call fcecho(contxt)
            cval=' '

            call fcecho('GTI start is earlier than TSTART.')
            call fcecho('Setting GTI start to equal TSTART.')
            goto 100
          endif

c         Is the TSTOP value greater than the start time,
c and less than the GTI stop time? If so set the GTI's stop time
c to be the same as TSTOP and jump out of this loop. 
          if(tstop.gt.gstart(lrow).and.
     &       tstop.lt.gstop(lrow))then
            lcount=lcount+1
            gstart(lcount)=gstart(lrow)
            gstop(lcount)=tstop

            cval=' '
            contxt=' '
            call ftd2f(gstart(lrow),8,cval,status)
            contxt(1:20)=cval(1:20)
            cval=' '
            call ftd2f(gstop(lrow),8,cval,status)
            contxt(25:45)=cval(1:20)
            call fcecho(' ')
            call fcecho('Processing GTI TIME-RANGE:')
            call fcecho(contxt)
            cval=' '

            call fcecho('GTI stop is later than TSTOP.')
            call fcecho('Setting GTI stop to equal TSTOP.')
            call fcecho('Jumping out of loop.')
            goto 200
          endif

          lcount=lcount+1
          gstart(lcount)=gstart(lrow)
          gstop(lcount)=gstop(lrow)
          
100       continue
          
        enddo

200     continue

        igtino=lcount

      else
          
        igtino=nrows

      endif

c      do lrow=1,igtino
c        print*,'GSTART and GSTOP are ',gstart(lrow),gstop(lrow)
c      enddo

      if(tstart.eq.0.0d0)then
        tstart=gstart(1)
        call fcecho(' ')
        call fcecho('No TSTART keyword found in this GTI file.')
        call fcecho('Setting TSTART to first START time.')
        cval=' '
        call ftd2f(tstart,8,cval,status)
        call fcecho(cval)
      endif
      
      if(tstop.eq.0.0d0)then
        tstop=gstop(igtino)
        call fcecho(' ')
        call fcecho('No TSTOP keyword found in this GTI file.')
        call fcecho('Setting TSTOP to last STOP time.')
        cval=' '
        call ftd2f(tstop,8,cval,status)
        call fcecho(cval)
      endif
      
      return
      end
