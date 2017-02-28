      subroutine gtiminfo(gettmin,gettmax,timemin,timemax,timeint,
     &     tmjds,tmjde,itimno,itimnoall,files,no,gofile
     &       ,gafile,gticols,
     &     gtidate,gtitime,obstart,obstop,obsdate,ipermno)
      implicit none

      integer ntime,ngti,ichan,isiz,ipermno

c      Define all of the common parameters used in the arrays.
      parameter (isiz = 999)
      
      character*(*) files(isiz),gofile,gafile,gticols,gtidate,
     &   obstart,obstop,obsdate,timeint,gtitime
      integer no,itimno(2,*),status
      
      double precision tmjds(*),tmjde(*),timemin,timemax
      logical gettmin,gettmax
      integer itimnoall
      
      integer itimerange1,itimerange2,
     &   itimerange3,itimerange4,itimeranges,
     &   itimerangee,iholds,iholde,
     &   igstart,igstop

      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
      
      common/gtiinfo/ichan,ngti

      ntime=ngti
      status=0

C     5Jan99 (MJT) Inserting hideous kludge to 'fix' the bug which causes
C                  user-specified timemin parameter to be totally ignored
C                  when gtiorfile=APPLY. Since I found and fixed the bug
C                  which was causing timeint to drop the first good-time
C                  interval (see below) I'm simply going to transform any 
C                  user-specified timemin/max values into the equivalent 
C                  timeint string and continue with processing as if that
C                  was how the user input was done! If, however, *both*
C                  timemin/max AND timeint are specified then the latter
C                  will take precedence and the former will be ignored
C                  (since timeint is more flexible in allowing comma-separated
C                  lists of intervals).
C
C                  Note that I must supply defaults in cases where only one
C                  of timemin/max were specified: using 0.0 and 999999999.0
C                  which should be good until 2025 for RXTE (secs since 1994.0)
C                  or any mission whose times fall inside that range...

      if (((.not.gettmin).or.(.not.gettmax))
     &     .and.(timeint.eq.' '.or.timeint.eq.'0.0-0.0')) then
         if (gettmin) timemin=0.0d0
         if (gettmax) timemax=999999999.0d0
c        write(timeint,'(d15.10,a1,d15.10)') timemin,'-',timemax
c      More precision needed for this:
         write(timeint,'(d21.16,a1,d21.16)') timemin,'-',timemax
c         print '(a22,a43)','FAKE TIMEINT (d21.16): ',timeint
         timemin=-1.0d0
         timemax=-1.0d0
         gettmin=.TRUE.
         gettmax=.TRUE.
      else if (((.not.gettmin).or.(.not.gettmax))
     &        .and.(timeint.ne.' '.and.timeint.ne.'0.0-0.0')) then   
         call fcecho(' ')
         call fcecho('WARNING: You have supplied both a TIMEINT')
         call fcecho('value or file and a TIMEMIN and/or TIMEMAX.')
         call fcecho('*TIMEINT will be used; TIMEMIN/MAX ignored*')
         timemin=-1.0d0
         timemax=-1.0d0
         gettmin=.TRUE.
         gettmax=.TRUE.
      endif

C      Read in the FITS file and write out the light curve file.
c     But let's initialize "pointer" arguments to udmget to avoid
c     unwitting reallocs! MJT 29Jan2002

      itimerange1 = 0
      itimerange2 = 0
      itimerange3 = 0
      itimerange4 = 0
      itimeranges = 0
      itimerangee = 0
      iholds = 0
      iholde = 0
      igstart = 0
      igstop = 0

      call udmget(ngti,7,itimerange1,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE1')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif

      call udmget(ngti,7,itimerange2,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE2')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif

      call udmget(ngti,7,itimerange3,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE3')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif

      call udmget(ngti,7,itimerange4,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE4')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif

      call udmget(ngti,7,itimerangee,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGEE')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif

      call udmget(ngti,7,itimeranges,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGES')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif
            
      call udmget(ngti,7,iholds,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for HOLDE')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif
      
      call udmget(ngti,7,iholde,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for HOLDE')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif

      call udmget(ngti,7,igstart,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for GSTART')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif
      
      call udmget(ngti,7,igstop,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for GSTOP')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif

      call gtiminfo_root(gettmin,gettmax,timemin,
     &   timemax,timeint,
     &   tmjds,tmjde,itimno,itimnoall,files,no,gofile,
     &   gafile,gticols,
     &   gtidate,gtitime,obstart,obstop,obsdate,ipermno,ntime,
     &   memd(itimerange1),memd(itimerange2),
     &   memd(itimerange3),memd(itimerange4),memd(itimeranges),
     &   memd(itimerangee),memd(iholds),memd(iholde),
     &   memd(igstart),memd(igstop))   

      call udmfre(itimerange1,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE1')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itimerange2,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE2')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itimerange3,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE3')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itimerange4,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE4')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itimerangee,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGEE')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itimeranges,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGES')
        call fcerrm(status)
        status=0
      endif
            
      call udmfre(iholds,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for HOLDS')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(iholde,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for HOLDE')
        call fcerrm(status)
        status=0
      endif

      call udmfre(igstart,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for GSTART')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(igstop,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for GSTOP')
        call fcerrm(status)
        status=0
        return
      endif
      
      
      
      return
      end
      
      
c**********************************************************************
c      This subroutine reads in and sorts all of the time filtering
c      information so that the code knows which time interval values to
c      process and which to ignore. In order to save on allocation space
c      initially "tmjds" and "tmjde" store the start and stop times
c      that are associated with each file in files. Several actions are
c      performed within this subroutine.
c  1.) The GTI and PHA files are read and that information processed
c      with the final information sorted so as to arrive as a set of

c**********************************************************************
c      This subroutine reads in and sorts all of the time filtering
c      information so that the code knows which time interval values to
c      process and which to ignore. In order to save on allocation space
c      initially "tmjds" and "tmjde" store the start and stop times
c      that are associated with each file in files. Several actions are
c      performed within this subroutine.
c  1.) The GTI and PHA files are read and that information processed
c      with the final information sorted so as to arrive as a set of
c      good time intervals that are to be processed.
c  2.) This listing is compared against the time values stored in TMJD
c      to see if any of the files fall outside of the time ranges so that
c      we can simply ignore those from the beginning.
c  3.) Once we have a full listing of valid time intervals and files
c      to be processed, the values in TMJD and FILES are modified to reflect
c      this information and returned in those variable names. This
c      information will be used in subsequent file processing....
c**********************************************************************
c      
      subroutine gtiminfo_root(gettmin,gettmax,timemin,
     &   timemax,timeint,
     &   tmjds,tmjde,itimno,itimnoall,files,no,gofile,
     &   gafile,gticols,
     &   gtidate,gtitime,obstart,obstop,obsdate,ipermno,ntime,
     &   timerange1,timerange2,
     &   timerange3,timerange4,timeranges,
     &   timerangee,holds,holde,
     &   gstart,gstop)
      implicit none

      integer nf,ntime,itemp,isiz,ipermno

c      Define all of the common parameters used in the arrays.
      parameter (nf = 258)
      parameter (itemp = 100)
      parameter (isiz = 999)
      
      character*(*) files(isiz),gofile,gafile,gticols,gtidate,
     &   obstart,obstop,obsdate,timeint,gtitime
      character(1) cmatch
      character(160) gfiles(isiz),timefile,gapplyfiles(isiz)
      character(80) contxt,gtistart,gtistop
      character(20) cval
      integer no,itimno(2,*),igtino,status,ifile,
     &   it1,it2,it3
      
      double precision tmjds(*),tmjde(*),timemin,timemax
      logical chkit,abort,fstchar,gettmin,gettmax,lorf,landf,
     &   lmatch, landranges
      integer i,j,k,l,numranges,igtinof,itimnoall,itot,
     &   iranges,igtiors,igtiands,ipos,ilen,fcstln,
     &   itimeunit, iomode, recl, igtiorf, igtiandf, extval,
     &   nogtiorfiles
      double precision tstart,tstop,small,large
      double precision timerange1(*),timerange2(*),
     &   timerange3(*),timerange4(*),timeranges(*),
     &   timerangee(*),holds(*),holde(*),
     &     gstart(*),gstop(*)

      character(1) dataval(10), testchar
      logical lbailout

      common/bail/lbailout
      common/gtistuff/gtistart,gtistop
      common/gtiorfiles/gapplyfiles,nogtiorfiles

      data (dataval(i),i=1,10)/'0','1','2','3','4','5',
     &     '6','7','8','9'/

      testchar = ' '
      itot=0
      landranges=.FALSE.
      gtistart=' '
      gtistop=' '
      timefile=' '
      itimnoall=0
      status=0
      ipermno=0
      small=0.0d0
      large=0.0d0
      lorf=.FALSE.
      landf=.FALSE.
      lmatch=.FALSE.
      cmatch='@'
      ipos=0
      iomode=1
      recl=133
      igtiors=0
      igtiands=0
      extval=0
      l=0
      iranges=0
      
      call dinitial(ntime,timerange1)
      call dinitial(ntime,timerange2)                
      call dinitial(ntime,timerange3)
      call dinitial(ntime,timerange4)        
      call dinitial(ntime,timeranges)        
      call dinitial(ntime,timerangee)        

      if((.not.gettmin).and.
     &   (timeint.ne.' '.and.timeint.ne.'0.0-0.0'))then
        call fcecho(' ')
        call fcecho('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
        call fcecho('WARNING!!!! You have entered both a ')
        call fcecho('TIMEMIN and a TIMEINT file or value!')
        call fcecho('TIMEINT will be used. Ignoring TIMEMIN.')
        call fcecho('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
        if(lbailout)then
          call fcecho(' ')
          call fcecho('You have the bailout option set!')
          call fcecho('Aborting...')
          stop
        endif
         
      endif

      if((.not.gettmax).and.
     &   (timeint.ne.' '.and.timeint.ne.'0.0-0.0'))then
        call fcecho(' ')
        call fcecho('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
        call fcecho('WARNING!!!! You have entered both a ')
        call fcecho('TIMEMAX and a TIMEINT file or value!')
        call fcecho('TIMEINT will be used. Ignoring TIMEMAX.')
        call fcecho('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
        if(lbailout)then
          call fcecho(' ')
          call fcecho('You have the bailout option set!')
          call fcecho('Aborting...')
          stop
        endif
      endif
       
      
      
      do i=1,no
c         print*,'tmjds and tmjde are ',i,tmjds(i),tmjde(i)         
        holds(i)=tmjds(i)
        holde(i)=tmjde(i)
      enddo
       
      j=0
      
c      Set the variable itimno equal to the number of time elements that
c      are left after setting up timemin and timemax. Note that it is
c      possible for one 1 file or none to be left...
      
c      Set the starting point equal to timemin and the last point equal
c      to timemax.

c----------------------------------------------------------------------      
c      Okay, now that we have all of the input files to be read sitting
c      in the array GFILES we have to parse through the input information
c      and extract the column TITLES such that we can set up a list of
c      time bins. So we will have the START time stored in GTISTART and
c      the STOP time in GTISTOP
      
      j=0
      i=1
      fstchar = .FALSE.
      
550   if(gticols(i:i).eq.' '.and.(.not.fstchar))then
         i=i+1
         goto 550
      else if (gticols(i:i).ne.' ')then
         fstchar=.true.
      endif
      
551   if(gticols(i:i).ne.' ')then
         j=j+1
         gtistart(j:j)=gticols(i:i)
         i=i+1
         goto 551
      endif

552   if(gticols(i:i).eq.' ')then
         i=i+1
         goto 552
      endif

      j=0

553   if(gticols(i:i).ne.' ')then
         j=j+1
         gtistop(j:j)=gticols(i:i)
         i=i+1
         goto 553
      endif
         
c      print*,'gticols,gtistart, and gtistop'
c      print*,gticols,gtistart,gtistop
c      Okay now we have the COLUMN header information.
c----------------------------------------------------------------------      

c      Now that we have dealt with timemin and timemax it is necessary
c      to go take care of the time intervals that were specified.

c      If no GTI files are input then skip the '$' section.
      
499   continue
      
      if(gofile.eq.' '.or.gofile.eq.'INDEF')then

c      If no TIMEMIN or TIMEMAX was set in the PAR file than we will
c      set them to the first and last times from TSTART and TSTOP. However
c      if there are GTIORFILES and TIMEMIN and TIMEMAX were not set then
c      we will set them to the first time specified and the last time
c      specified in all of the files. This is necessary because TSTART and
c      TSTOP may not have the precision necessary so that the "may"
c      erroneously exclude either the first or last point in a file,
c      if they have been rounded. This loss of precision is thus avoided
c      by this approach.

        if(gettmin)timemin=holds(1)
        if(gettmax)timemax=holde(no)

        do i=1,no
          itimno(1,i)=1
          tmjds(1)=timemin
          itimno(2,i)=1
          tmjde(no)=timemax
          itimnoall=1

          itimno(1,i)=i
          itimno(2,i)=i
          tmjds(i)=holds(i)
          tmjde(i)=holde(i)
          tmjds(1)=timemin
          tmjde(no)=timemax

c          print*,'TMJDS and TMJDE is',tmjds(i),tmjde(i)
          
        enddo

        itimnoall=no

        goto 599

      endif
      
      
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c      We will first acquire a list of files that are to be OR'ed
c      together for each of the input files to build up a list of
c      acceptable GTI's for each file
      
c      We have to allow for the inputting of files and file
c      extensions which contain GTI information. So let's take care of
c      getting all of that set up at this point in time. 

      abort = .FALSE.
      fstchar=.FALSE.
      chkit = .FALSE.
      status=0

c      Read in the input file GOFILE and see if it is one file or a list of
c      GTI files with extensions that must be processed, store that file
c      or list of files in GFILES and the number of files in GTINOF
      if(gofile(1:5).eq.'APPLY')then
        if(gofile.eq.'APPLY')then
          extval=2
          call gtisetup(gapplyfiles,gfiles,nogtiorfiles,
     &       extval,abort)
        else

c     We have to extract the extension from the gofile input.
          ilen=fcstln(gofile)
c          print*,'The length of the file is ',ilen,gofile(6:ilen)
          
          k=0
          do i=6,ilen
            
            testchar=gofile(i:i)

            do j=1,10
              if(dataval(j).eq.gofile(i:i))l=j-1
c              print*,'We had a match for ', gofile(i:i), j, l
            enddo
            
            if(k.ne.0)then
              j=k*10
              l=l*j
            endif

            extval=extval+l
c            print*,'We are have built up to',extval

            k=k+1
            
          enddo

c          print*,'We are going to try to go to',extval

          call gtisetup(gapplyfiles,gfiles,nogtiorfiles,
     &       extval,abort)
          
        endif
        

c       If when trying to find out which extensions are GTI's we find
c that one or more of the files do no meet the qualifications for a GTI
c or do not contain GTI information, then will will not apply GTI
c information to ANY of the input files. This should take care of any
c cases where we are attempting to process ASCA, EINSTEIN, or GINGA data
c and no valid GTI extensions are found.
        if(abort)then
          call fcecho(' ')
          call fcecho('**********************************************')
          call fcecho('Could not find a VALID GTI extension in one or')
          call fcecho('more input files. Proceeding without using')
          call fcecho('GTI information provided in each input file.')
          call fcecho(' ')
          call fcecho('If you enter GTIORFILE extensions')
          call fcecho('at the GTIORFILE prompt. This check is not')
          call fcecho('performed so your input MUST be correct.')
          call fcecho('**********************************************')
          if(lbailout)then
            call fcecho(' ')
            call fcecho('You have the bailout option set!')
            call fcecho('Aborting...')
            stop
          endif
            
          gofile=' '
          goto 499
        else
          call fcecho(' ')
          call fcecho('Found GTI extensions in all input files.')
          call fcecho('Proceeding to extract and apply GTIs to input.')
          igtinof=nogtiorfiles
        endif

      else
        call fcgcls(gofile,gfiles,igtinof,abort)
      endif
      
      if(status.ne.0)then
        call fcecho(' ')
        contxt='Error extracting GTI information from GTIORFILE'
        call fcecho(contxt)
        call fcecho('Check input... Aborting...')
        call fcerrm(status)
        status=0
        stop
      endif

c      Set up a loop to cycle over all of the GTI files that contain
c      values that are to be OR'ed for each individual file. This means
c      that we will have a list of acceptable time values for each file
c      that is being processed. Thus if a time interval acceptable for
c      one file is unacceptable for another, there will be no overlap
c      of time intervals.

      j=0

      do ifile=1,igtinof
        call fcecho(' ')
        call fcecho('Processing GTIORFILE')
        call fcecho(gfiles(ifile))
        
        call rgtifile(gfiles(ifile),obstart,obstop,gtistart,
     &     gtistop,tstart,tstop,gstart,gstop,igtino)

        if(gettmin.and.ifile.eq.1)timemin=gstart(1)
        if(gettmin.and.gstart(1).lt.timemin)timemin=gstart(1)
        if(gettmax.and.ifile.eq.1)timemax=gstop(igtino)
        if(gettmax.and.gstop(igtino).gt.timemax)timemax=gstop(igtino)

c        print*,' '
c        print*,'File being processed is file number',ifile
c        do i=1,igtino
c          print*,'Gstart and Gstop are',gstart(i),gstop(i)
c        enddo
c        
c        print*,'timemin and timemax at GTIOR is '
c     &     ,timemin,timemax,igtino
c        print*,'GTIorstart is',igtino,(gstart(i),i=1,igtino),tstart
c        print*,'GTIorstop is',igtino,(gstop(i),i=1,igtino),tstop

        do i=1,no
          
          if((holds(i).eq.tstart).or.(holde(i).eq.tstop))then
            lorf=.TRUE.
            itimno(1,i)=j+1
               
            do k=1,igtino
              j=j+1
              tmjds(j)=gstart(k)
              tmjde(j)=gstop(k)
            enddo
            
            itimno(2,i)=j
            itimnoall=itimnoall+j
            
          endif
          
        enddo
        
        if(.not.lorf)then
          call fcecho(' ')
          call fcecho('**************WARNING****************')
          call fcecho('GTIORFILES have been input which do not')
          call fcecho('correspond to ONE specific input file!')
          call fcecho(' ')
          call fcecho('GTI files that apply to ALL files should')
          call fcecho('be input as a GTIANDFILE!. Aborting...')
          call fcecho('*************************************')
          stop

        endif
        
        lorf=.FALSE.
          
      enddo

      
      gettmin=.FALSE.
      gettmax=.FALSE.
      igtiorf=igtinof
      
599   continue

c     Reinitialize the variables that keep track of the number of
c GTI files as well as the number of elements in each. 
      igtino=0
      igtinof=0

c      At this point TMJDS and TMJDE contain start and stop times
c      that are acceptable for each individual file. ITIMNO(1,N)
c      contains the array element which contains the first acceptable
c      start time for the Nth file and ITIMNO(2,N) contains the
c      last acceptable array element stored in TMJD which is associated
c      with this file.


c     Check to see if a user specified BOTH a GTIANDFILE and a TIMEINT
c value, and issue appropriate warnings is that is the case...

C     5Jan99 (MJT) a bit shrill (and unnecessary!)...
C
c      if((gafile.ne.' '.and.gafile.ne.'INDEF').and.(
c     &   timeint.ne.' '.and.timeint.ne.'0.0-0.0'))then
c        call fcecho(' ')
c        call fcecho('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
c        call fcecho('WARNING!!!! You have entered both a ')
c        call fcecho('GTIANDFILE and a TIMEINT file or value!')
c        call fcecho(' ')
c        call fcecho('The code will attempt to combine them!')
c        call fcecho('This may or may not be successful.')
c        call fcecho('In general this is a bad idea since the')
c        call fcecho('extractor assumes you have created a GTI')
c        call fcecho('file using MGTIME,or MAKETIME.')
c        call fcecho('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
c        call fcecho(' ')
c      endif
      
c----------------------------------------------------------------------      
c      Call the subroutine that will parce the input string of
c      good time intervals that are to be AND'ed performing the
c      calculations which were supplied by the user in the PAR file.

c      print*,'GAFILE is ',gafile
      
      if(gafile.ne.' '.and.gafile.ne.'INDEF')then

c      We have to allow for the inputting of files and file
c      extensions which contain GTI information. So let's take care of
c      getting all of that set up at this point in time. 

        abort = .FALSE.
        fstchar=.FALSE.
        chkit = .FALSE.

c      Read in the input file GAFILE and see if it is one file or a list of
c      GTI files with extensions that must me processed, store that file
c      or list of files in GFILES and the number of files in GTINOF
      
        call fcgcls(gafile,gfiles,igtinof,abort)
        
        if(status.ne.0)then
          call fcecho(' ')
          contxt='Error extracting GTI information from GTIANDFILE'
          call fcecho(contxt)
          call fcecho('Check input!!! Aborting.')
          call fcerrm(status)
          status=0
          stop
        endif

        igtiandf=igtinof
        
c        print*,'gfiles is ',igtinof,gfiles(1),gfiles(2),gfiles(3)

c      Set up a loop to cycle over all of the GTI files that contain
c      values that are to be OR'ed for each individual file. This means
c      that we will have a list of acceptable time values for each file
c      that is being processed. Thus if a time interval acceptable for
c      one file is unacceptable for another, there will be no overlap
c      of time intervals.

c        print*,'igtinof is ',igtinof
        
        j=itimnoall

        do ifile=1,igtinof
          call fcecho(' ')
          call fcecho('Processing GTIANDFILE')
          call fcecho(gfiles(ifile))

          call rgtifile(gfiles(ifile),obstart,obstop,gtistart,
     &       gtistop,tstart,tstop,timerange1,timerange2,numranges)
        
          if(numranges.le.0)then
            call fcecho(' ')
            call fcecho('The GTIANDFILE(s) contains NO pertinent data')
            call fcecho('within the TSTART+TIMEZERO and TSTOP+TIMEZERO')
            call fcecho('specified.')
            if((igtinof.gt.1).and.
     &         (ifile.lt.igtinof))then
              call fcecho('Continuing to next file.')
            else
              call fcecho('No valid data. Aborting processing')
              call fcecho('of GTIANDFILE information. Continuing.')
              if(lbailout)then
                call fcecho(' ')
                call fcecho('You have the bailout option set!')
                call fcecho('Aborting...')
                stop
              endif
            
            endif
            
            goto 299
          endif

          landranges=.TRUE.
          
c          print*,'timemin and timemax at gtiand is',timemin,timemax
          if(gettmin.and.ifile.eq.1)timemin=timerange1(1)
          if(gettmin.and.timerange1(1).lt.timemin)
     &       timemin=timerange1(1)
          if(gettmax.and.ifile.eq.1)timemax=timerange2(numranges)
          if(gettmax.and.(timerange2(numranges).gt.timemax))
     &       timemax=timerange2(numranges)
          
          itot=0
          
          if(ifile.eq.1)then
            do it1=1,numranges
              timerange3(it1)=timerange1(it1)
              timerange4(it1)=timerange2(it1)
              timerange1(it1)=0.0d0
              timerange2(it1)=0.0d0
              iranges=numranges
            enddo
          endif

          if(igtinof.eq.1)then
            itot=iranges
            goto 301
          endif
          
          gettmin=.FALSE.
          gettmax=.FALSE.
          
          do it1=1,iranges
            do it3=1,numranges

              small=timerange1(it3)
              large=timerange2(it3)

              if(small.gt.timerange4(it1))goto 219
              if(large.lt.timerange3(it1))goto 219
              
              if(small.lt.timerange3(it1).and.
     &           small.lt.timerange4(it1))small=timerange3(it1)
              if(large.gt.timerange4(it1))large=timerange4(it1)
              itot=itot+1
              timeranges(itot)=small
              timerangee(itot)=large

219           continue
                            
            enddo
          enddo
          
          do it1=1,itot
            timerange3(it1)=timeranges(it1)
            timerange4(it1)=timerangee(it1)
            iranges=itot
          enddo

299       continue
          
        enddo
        
301     continue

        if(.not.landranges)then
          call fcecho(' ')
          call fcecho('No GTI time-ranges were found in ANY of your')
          call fcecho('GTIANDFILES! Resetting GTIANDFILES parameter')
          call fcecho('and aborting GTIAND sorting and comparisons.')
          if(lbailout)then
            call fcecho(' ')
            call fcecho('You have the bailout option set!')
            call fcecho('Aborting...')
            stop
          endif
                      
          gafile=' '
          goto 599
        endif

        call dinitial(ntime,timerange1)
        call dinitial(ntime,timerange2)

        it2=0

c       Let's look for time intervals that have the same start and
c stop times - this happens if several of the input times overlap.
c Ideally, this should never happen, but more than a few people have
c input several time-ranges which have the stop time of one time range
c equal to the start time of another and they have done it in several
c GTI's so that each will result in a separte time range with the start
c     time equal to the stop time.
        do j=1,itot
          if(timerange3(j).ne.timerange4(j))then
            it2=it2+1
            timerange1(it2)=timerange3(j)
            timerange2(it2)=timerange4(j)
          endif
        enddo
           
        itot=it2
           
        call dinitial(ntime,timerange3)
        call dinitial(ntime,timerange4)

        it3=0

c          Now we have to check for overlapping time intervals... This
c happens when someone inputs a stop time equal to a start time in the
c SAME GTI file that was input - don't laugh, people do this sort of
c thing and they do it ALL the time... So since they aren't careful the
c code has to try to be...

        if(itot.gt.1)then
          
          do j=1,itot-1

c            print*,'it3 is ',it3,j

            if(it3.gt.0)then
              if(timerange3(it3).eq.0.0d0)then
                it3=it3+1
                timerange3(it3)=timerange1(j)
c                print*,'IF Set timerange3 to ',it3,j,timerange3(it3)
              endif
            else
              it3=it3+1
              timerange3(it3)=timerange1(j)
c              print*,'ELSE Set timerange3 to ',it3,j,timerange3(it3)
            endif
            

            if(timerange2(j).eq.timerange1(j+1))then
c              print*,'The two were equal'
            else
              timerange4(it3)=timerange2(j)
c              print*,'Set timerange4 to ',it3,j,timerange4(it3)
              it3=it3+1
              timerange3(it3)=timerange1(j+1)
c              print*,'Set timerange3 to ',it3,j+1,timerange3(it3)
            endif

            if(j.eq.itot-1)then
              timerange4(it3)=timerange2(j+1)
c              print*,'End - Setting timerange4(it3) to ',it3,j+1
c     $           ,timerange2(j+1)

            endif
            
          enddo

c          print*,'Entered with itot ', itot, '  left with itot ',it3
          
          itot=it3
          
        else

          timerange3(1)=timerange1(1)
          timerange4(1)=timerange2(1)

        endif

c        print*,'ITOT is ',itot
        
c        do j=1,itot
c          print*,'In GTIM timerange3 and 4 are ',j,
c     &       timerange3(j),timerange4(j)
c        enddo

c**********************************************************************
        
        if(timeint.eq.' '.or.timeint.eq.'0.0-0.0')then        
          j=itimnoall

          if(itot.eq.0.and.iranges.ne.0)itot=iranges
          numranges=itot

c          print*,'IN TIMEINT.eq.blank',ipermno,numranges
          ipermno=ipermno+numranges
        
          do k=1,numranges
            j=j+1
            tmjds(j)=timerange3(k)
            tmjde(j)=timerange4(k)
          enddo
          
        endif
c**********************************************************************

      endif

c      print*,'TIMEINT value is ',timeint
      
      if(timeint.ne.' '.and.timeint.ne.'0.0-0.0')then
c        print*,'TIMEINT NE blank and NE 0.0-0.0'

        ilen=fcstln(timeint)
        call scnstrforchar(timeint,ilen,cmatch,lmatch,ipos)

        if(lmatch)then
c         A match was found for the character to denote that
c a file was input rather than a character string. So we will have
c to open this input file - assumed to be ASCII and in a format
c such that it contains N rows with:
c  Time-start       Time-stop

          numranges=0
          
c      Assign a unit file number to the input file.
          call ftgiou(itimeunit,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error getting input itimeunit number')
            call fcecho('Setting to logical unit 15')
            status=0
            itimeunit=15
          endif

c      Open the ASCII file containing time ranges created by TIMETRANS.
          call fcecho(' ')
          call fcecho('Attempting to read TIMEINT file:')
          call fcecho(timeint(ipos+1:ilen))
          
          call faopen(itimeunit,timeint(ipos+1:ilen)
     &       ,iomode,recl,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not open TIMEINT input time file.')
            call fcerr('Cannot continue....')
            if(lbailout)then
              call fcecho(' ')
              call fcecho('You have the bailout option set!')
              call fcecho('Aborting...')
              stop
            endif
            
          endif

c         Read in all of the time ranges from the input file. 
799       numranges = numranges+1
          read(itimeunit,*,err=800,end=800)
     &       timerange1(numranges),timerange2(numranges)
          goto 799
800       continue
          numranges = numranges-1
        
c      Close the ASCII file created by TIMETRANS and continue
          close(itimeunit)

c      Free the logical unit number used. 
          call ftfiou(itimeunit,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error freeing input itimeunit number')
            call fcecho('Setting to logical unit 15')
            status=0
          endif

        else

c         The time interval was input as one only string of times
c and we have to parse that to get the start and stop values.

          call str2real(timeint,1,numranges,timerange1,
     &       timerange2)

c          print*,'timerange1 and timerange2 is ',timerange1(1),
c     &       timerange2(1)
        endif

        j=itimnoall         
        
        if(timerange1(1).eq.timerange2(1).and.numranges.eq.1)then
          timerange1(1)=timemin
          timerange2(1)=timemax
        endif

        if(itot.eq.0)then
          do k=1,numranges
            j=j+1
            tmjds(j)=timerange1(k)
            tmjde(j)=timerange2(k)
          enddo

c          print*,'ITOT IPERMNO+NUMRANGES',
c     &       ipermno,numranges,itot
          
          ipermno=ipermno+numranges

        else

c          print*,'Iranges is ',itot,iranges,numranges
          iranges=itot
          itot=0

          do it1=1,iranges

c            print*,'IRANGES is ',iranges,it1
            
            do it3=1,numranges

c              print*,'NUMRANGES is ',numranges,it3
              
              small=timerange1(it3)
              large=timerange2(it3)

c              print*,'small and large',small,large
              
              if(small.gt.timerange4(it1))goto 319
              if(large.lt.timerange3(it1))goto 319

c              print*,'Past gotos'
c              print*,'3',timerange3(it1)
c              print*,'3',timerange4(it1)
              
              if(small.lt.timerange3(it1).and.
     &           small.lt.timerange4(it1))small=timerange3(it1)
              if(large.gt.timerange4(it1))large=timerange4(it1)

c              print*,'small and large after',small,large
              
              itot=itot+1
              timeranges(itot)=small
              timerangee(itot)=large

319           continue
                            
            enddo
          enddo

c     print*,'ITOT is ',itot
c     Some users cannot tell that they are entering mutually
c     exculsive GTIs so the code has to add this option. Don't laugh,
c     this check was added BECAUSE people were doing this. 
          if(itot.le.0)then
            call fcecho(' ')
            call fcecho('Merging GTIANDFILES and TIMEINT resulted in ')
            call fcecho('zero GTIs left. There are NO GOOD TIMES!')
            call fcecho('Cannot continue aborting!!!')
            stop
          endif

          do it1=1,itot
            timerange3(it1)=timeranges(it1)
            timerange4(it1)=timerangee(it1)
            iranges=itot
          enddo

          itot=iranges
          
          call dinitial(ntime,timerange1)
          call dinitial(ntime,timerange2)

c       Let's look for time intervals that have the same start and
c stop times - this happens if several of the input times overlap.
c Ideally, this should never happen, but more than a few people have
c input several time-ranges which have the stop time of one time range
c equal to the start time of another and they have done it in several
c GTI's so that each will result in a separte time range with the start
c time equal to the stop time. 
          it2=0
          do j=1,itot
            if(timerange3(j).ne.timerange4(j))then
              it2=it2+1
              timerange1(it2)=timerange3(j)
              timerange2(it2)=timerange4(j)
            endif
          enddo
           
          itot=it2
           
          call dinitial(ntime,timerange3)
          call dinitial(ntime,timerange4)

          it3=0

c     Now we have to check for overlapping time intervals... This
c     happens when someone inputs a stop time equal to a start time in the
c     SAME GTI file that was input - don't laugh, people do this sort of
c     thing and they do it ALL the time... So since they aren't careful the
c     code has to try to be...

          if(itot.gt.1)then
             do j=1,itot-1
                
C     5Jan99 (MJT) BUG SQUASHED!
C     if(timerange3(it3).eq.0.0d0)then
                if(timerange3(j).eq.0.0d0)then
                   it3=it3+1
                   timerange3(it3)=timerange1(j)
                endif
                
                if(timerange2(j).eq.timerange1(j+1))then
              
                else
                   timerange4(it3)=timerange2(j)
                   it3=it3+1
                   timerange3(it3)=timerange1(j+1)
                endif
                
                if(j.eq.itot-1)
     &               timerange4(it3)=timerange2(j+1)
             enddo
             
             itot=it3
          else
             timerange3(1)=timerange1(1)
             timerange4(1)=timerange2(1)
          endif
          
          
c     print*,'ITOT is ',itot
          
c     do j=1,itot
c     print*,'In TIMEINT timerange3 and 4 are ',j,
c     &         timerange3(j),timerange4(j)
c     enddo
          
          j=itimnoall
          
          numranges=itot

          if(numranges.le.0)then
            call fcecho(' ')
            call fcecho('Your TIMEINT contains NO valid time-ranges!')
            call fcecho('Please check you input! Aborting parsing.')
            call fcecho('We must start the entire process or sorting')
            call fcecho('time-intervals and time-ranges over again.')
            if(lbailout)then
              call fcecho(' ')
              call fcecho('You have the bailout option set!')
              call fcecho('Aborting...')
              stop
            endif
            
            timeint='0.0-0.0'
            goto 599
          endif
          
c          print*,'IPERMNO and numranges',ipermno,numranges,itimnoall
          ipermno=ipermno+numranges

          do k=1,numranges
            j=j+1
            tmjds(j)=timerange3(k)
            tmjde(j)=timerange4(k)
          enddo

        endif
        

c----------------------------------------------------------------------
c      Now that we have all of the GTI values we have to check to see
c      if there are any overlap intervals or duplicate intervals. 

c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$      
c      If we are this point neither TIMEINT nor GAFILE was set so
c      we will set the AND'ed time interval to TIMEMIN and TIMEMAX

      else if((gafile.eq.' '.or.gafile.eq.'INDEF').and.
     &     (timeint.eq.' '.or.timeint.eq.'INDEF'.or.
     &     timeint.eq.'0.0-0.0'))then

c        print*,'We are in INDEF, INDEF'
        
        j=itimnoall+1
        ipermno=1
        tmjds(j)=timemin
        tmjde(j)=timemax
        
      endif

c      Print*,'GAFILE and TIMEINT is',gafile,timeint
      
c**********************************************************************
c      print*,'Time start and stop is ',itimnoall,ipermno
c     &   ,(tmjds(i),tmjde(i),i=1,itimnoall+ipermno)

c      print*,'itimnoall and ipermno are',itimnoall,ipermno

      if(itimnoall+ipermno.gt.ntime)then
        contxt=' '
        call fcecho(' ')
        call fcecho('A dimensioning problem has occured!')
        call fcecho('Redimension the parameter gtiarray to')
        call fcecho('a value larger than 30000 (the default).')
        call fcecho(' ')
        call fcecho('Your GTI filtering presently results in')
        call fti2c(itimnoall+ipermno,cval,status)
        contxt(1:11)=cval(10:20)
        contxt(13:)='time-ranges. You will have to either pre-filter'
        call fcecho(contxt)
        call fcecho('your data using GROSSTIMEFILT or FSELECT')
        call fcecho('and re-run this code, or resize GTIARRAY.')
        call fcecho('Cannot continue... Aborting...')
        stop
      endif
      
c      do 995 i=1,itimnoall+ipermno
c        print*,'Time is',i,tmjds(i),tmjde(i)
c995   continue
      
c      print*,'timemin and timemax is ',timemin,timemax
      
      return
      end

