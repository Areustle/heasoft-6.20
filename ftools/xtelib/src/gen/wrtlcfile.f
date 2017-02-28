
C**********************************************************************
c
c      wrtlcfile
c
c
c DESCRIPTION:
c     Writes a Light Curve out in the proper FITS format such that
c     XRONOS can process the data. This routine is set up to write out
c     a single light curve or multiple light curves into different
c     columns. 
c
c AUTHOR:
c      Brian K. Elza
c
c MODIFICATION HISTORY:
c     15June2000 (MJT) modified handling of TIMEREF kwd:
c                      reads value from i/p file, default 'LOCAL'
c                      writes value w/comment 'No path length corrections' *unless*
c                        FXTIME kwd exists in i/p file in which case it writes
c                        value of 'SOLARSYSTEM' w/ comment 'bary. corr. applied to times'
c
c     ARGUMENTS:
C     files   c- input array containing the file names must <= 999
c     outroot c- input prepended root to output file name
c     ounit   i- ouput unit number associated with the output fits file
c     nchan   i- input total number of elements in the array to be scanned.
c     no      i- input number of files that are in array FILES
c     lcmode  c- input mode is either SUM, RATE, or MEAN - what to calculate
c     extenlc c- input suffix that is appended to the root to construct outfile
c     timemin d- input Earliest time that was processed for this file
c     timemax d- input Latest time that was processed for this file
c     totsecs d- input Total elapsed time in all files processed.
c     binsz   d- input Bin size in seconds for this light curve.
c     inocols i- input Number of COLUMNS to be written out. This is ONE
c                if ACCCUMULATE=ONE and the number of columns that was
c                processed to create this light curve if ACCUMULATE=MANY.
c                Note that COLUMNS is written out as 3*inocols+1 separate
c                columns.
c     icolfset i-input Number of offset bins set asside for each column,
c                so if 50 bins are necessary to process each column than
c                icolfet would equal 50. This is done so that only one
c                array will be input although it may have N vectors put
c                together in a serial manner.
c     lcbin   l- input Logical that tells if elements in the Light
c                Curve has been set to TNULL
c     tnull   r- input Real value that is used as a NULL value -300.0e0 is the
c                generic value used.
c     bin    ra- Input Real array containing the total number of counts
c                the fall into each bin. 
c     realbin ad- input array containing the amount of time that
c                 falls into each bin. 
c     ibincount ai-input array containing the number of events that that were
c                 each bin array, so that the MEAN could be calculated,
c                 i.e., these are incremented by 1 each time something
c                 falls within this bin.
c     ioffset  ai-input 2D array which contains the number of bins that
c                 fall inbetween each file processed, thus ensuring that
c                 bins are not allocated for times that do not exist and
c                 minimizing the number of elements necessary in each
c                 array. This array contains an offset for each input file.
c     iarray   ai-input array containing the bin that is associated with
c                 each time bin. Thus the number of double precision bins
c                 were minimized at the expense of having more integer bins.
c     icount   i-input maximum number of counts in all columns that were
c                processed.
c     clobber  l-input logical that tells is we are to over write any
c                existing output files. 
c
c CALLED ROUTINES:
c     ftgiou - get a free logical unit number from a file
c     fcpars - parse a file name into file name and extension
c     ftopen - open a FITS file
c     ftclos - close a FITS file
c     ftmahd - skip to a particular FITS extension
c     ftgkys - read a particular KEYWORD string value
c     ftgkyd - read a particular KEYWORD double precision value
c     ftgkye - read a particular KEYWORD real value            
c     ftfiou - free up a logical unit number for reuse
c     fcgcls - parse input to get actual input file names
c     fcecho - echo out the string to STDOUT
c     fcerr  - echo out error message to STDERR
c     fcerrm - write out the error message associated with a status
c     ffinit - open an output file or delete it if it exists.
c     ftpdef - define the KEYWORD space in and output file
c     ftphpr - write out initial keywords describing this extension
c     ftpkys - write a particular KEYWORD string value
c     ftpkyd - write a particular KEYWORD double precision value
c     ftpkye - write a particular KEYWORD real value
c     ftpkyf - write out a KEYWORD in a fixed format
c     ftpdat - write out the creation date into this file
c     ftcrhd - create another data extension
c     ftphbn - write out binary information keywords for extension
c     ftpkyj - write out a particular KEYWORD integer value
c     ftpkyl - write out a particular KEYWORD logical value
c     ftpcld - write out a double precision value in a column
c     ftpcle - write out a real value in a column      
c     
c PRIMARY LOCAL VARIABLE: 
c      nfield      - number of columns that will be written out
c      tform(s/r)  - form of the information stored
c                    (s = EVENT_SUM, r=EVENT_RATE)
c      ttype(s/r)  - Column title that is to be written out
c      tunit(s/r)  - units that go with each column
c      extnam      - extension name BINTABLE in this instance
c      chanaray    - counting array of bin channels
c      pcount      - size of special data area following the table (0).
c      gcount      - number of 'random groups' usually 1.
c      nrows       - number of rows in the file (number of bins)
c      nfield      - number of columns that are in the outfile
c
c**********************************************************************
        
        subroutine wrtlcfile(files,outroot,ounit,nchan,
     &   no,lcmode,extenlc,tfirst,
     &   timemin,timemax,totsecs,binsz,
     &   inocols,icolfset,lcbin,tnull,isave,cols,
     &   dbin,realbin,dbincount,dpernoarray,
     &   inobd,ilbd,iubd,cpixold,chbin,
     &   icount,clobber,icycles,dbegin,irowsave,
     &   labort)
        
        implicit none

        integer nc,icols,isiz
        parameter (nc = 255)
        parameter (icols = 40)
        parameter (isiz = 999)
      
        character*(*) files(isiz),lcmode,extenlc,outroot,
     &     cols(icols),cpixold,chbin

        integer ounit,nfield,gcount,pcount,status,
     &     izero,no,ione,nchan,imjdref,
     &     itest,isave,inobd,ilbd,iubd
        character(40) ttypes(nc),tforms(nc),tunits(nc),extnamr,
     &     ttyper(nc),tformr(nc),tunitr(nc),contxt,
     &     ttypem(nc),tformm(nc),tunitm(nc),
     &     timesys,timeunit,extnams,timeref,tassign
        character(20) tempname
        character(160) file1,file2
        double precision realbin(*),mjdref,dpernoarray(*),dbin(*),
     &     dbincount(*)
        
        real tnull
        double precision totsecs,timemin,timemax,tfirst,
     &     fzero,binsz,lasttime,dbegin,dtruc,
     &     startf,stopf,timearay,timeval,tierrela,tierabso,
     &     timeval2
        
        character(80) origin,telescop,instrume,
     &     radecsys,observer,object,dateobs,timeobs,
     &     dateend,timeend,ctimezero,commentlines(128)
        character(20) cval
        real equino,ra,dec,deltat,timepixr
        double precision realval,error,fracexp,timetemp,timezero
        integer extnum,iunit,iunitn,xtend,bitpix,i,j,k,block,
     &     naxis,istarti,istopi,icount,irow,
     &     kval,outlen,fcstln,ival,icvalen,
     &     icval,icval2,icval3,kcol,irowsave,
     &     inocols,icolfset,icycles, noofcomments
c MJT 15July96 (g77/linux) extend used as logical -- fixed declaration
        logical simple,lor,land,pand,clobber,lcbin,labort,
     &     gainapp,clockapp,lfirstgo,lfxbary,lfxtime,extend

        character(40) taskname        
        common /task/ taskname
        common/offset_value/ctimezero
        
        common/gtinfo/mjdref,imjdref,ra,
     &     dec,equino,dateobs,timeobs
     &     ,dateend,timeend,
     &     instrume,object,radecsys,timesys,timeunit  

        common/comments/commentlines,noofcomments
        
c       We have added a new feature which will allow Light Curves to be
c accumulated indefinitely, and to this end we have incorporated a feature
c which will separate the FIRST time run through with all of the others.
c So that in the first run all of the initial information is written into
c the header. The logical controlling this is "icycles". If ICYCLES is 1
c then we write out all of the header information. 

        
c      Set the values such that they are defined for a light curve
c      file - the options are for EVENT_SUM and EVENT_RATE and
c      for equispaced bins or non-equispaced bins.

        data extnamr/'RATE'/
        data extnams/'COUNTS'/

        lfirstgo = .FALSE.
        timezero=0.0d0
        tierrela=0.0d0
        tierabso=0.0d0
        lfxbary=.FALSE.
        lfxtime=.FALSE.

        status = 0

        if(ctimezero.eq.'INDEF')then
          timezero=0.0d0
        elseif(ctimezero.eq.'FLOAT')then
          timezero=0.0d0
        else
c          print*,'About to try to read in timezero!'
          timezero=0.0d0
          outlen=fcstln(ctimezero)
          i=1
          read(ctimezero(:outlen),*,err=900)timezero
c          print*,'TIMEZERO read in was ',timezero
900       continue
c          print*,'The value of TIMEZERO is ',timezero
        endif

c        print*,'TIMEZERO is',timezero
        
        if(icycles.eq.1.or.ounit.eq.0)then
          lfirstgo=.TRUE.
          mjdref=0.0d0
          imjdref=0
        endif

        if(labort)return
          
        land=.FALSE.
        lor=.FALSE.
        pand=.FALSE.

        gainapp=.FALSE.
        clockapp=.FALSE.
        
        nfield=inocols*3+1

        ttypes(1)='TIME'
        tforms(1)='1D'
        tunits(1)='s'

        ttyper(1)='TIME'
        tformr(1)='1D'
        tunitr(1)='s'

        ttypem(1)='TIME'
        tformm(1)='1D'
        tunitm(1)='s'

        k=0
        cval=' '
        icvalen=20
        
c        print*,'inocols is ',inocols

        k=0

        do 10 i=2,(3*inocols)+1,3
          k=k+1

          if(k.gt.1)then
            call fti2c(k,cval,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Error converting integer to character')
              call fcecho('Column names in LC file may be corrupted.')
              call fcecho('Continuing...')
              status=0
            endif

            j=0

20          continue
            j=j+1
            if(cval(20-j:20-j).ne.' '.and.j.ne.icvalen)goto 20
            j=j-1
          else
            cval=' '
            j=0
          endif
          
          tempname='COUNTS'
          tempname(7:7+j)=cval(20-j:20)
          
          ttypes(i)=tempname
          tforms(i)='1D'
          tunits(i)='counts'

          tempname='ERROR'
          tempname(6:6+j)=cval(icvalen-j:icvalen)
          
          ttypes(i+1)=tempname
          tforms(i+1)='1D'
          tunits(i+1)='counts'

          ttyper(i+1)=tempname
          tformr(i+1)='1D'
          tunitr(i+1)='counts/s'

          ttypem(i+1)=tempname
          tformm(i+1)='1D'
          tunitm(i+1)='counts'
          
          tempname='FRACEXP'
          tempname(8:8+j)=cval(icvalen-j:icvalen)
          
          ttypes(i+2)=tempname
          tforms(i+2)='1D'
          tunits(i+2)=' '

          ttyper(i+2)=tempname
          tformr(i+2)='1D'
          tunitr(i+2)=' '

          ttypem(i+2)=tempname
          tformm(i+2)='1D'
          tunitm(i+2)=' '

          tempname='RATE'
          tempname(5:5+j)=cval(icvalen-j:icvalen)          

          ttyper(i)=tempname
          tformr(i)='1D'
          tunitr(i)='counts/s'

          tempname='MEAN'
          tempname(5:5+j)=cval(icvalen-j:icvalen)          

          ttypem(i)=tempname
          tformm(i)='1D'
          tunitm(i)='counts'

10      continue

c        print*,'TFORMS is ',(tforms(i),i=1,(3*inocols)+1)
c        print*,'TTYPES is ',(ttypes(i),i=1,(3*inocols)+1)
c        print*,'TUNITS is ',(tunits(i),i=1,(3*inocols)+1)        
c        print*,'nfield is ',nfield

        lasttime=0.0d0
        status=0
        
c       If we are going through more than one cycle of writing out the light
c curve than we will just jump to the part where we are writing out more
c rows and skip everything else.

c        print*,'ICYCLES IN WRTLCFILE IS ',icycles,ounit
        
        if((.not.lfirstgo))then

          goto 980

        else
          irowsave=0
        endif

c        print*,'We are to the creation stage'
        
        origin= ' '
        telescop= ' '
        instrume= ' '
        radecsys= ' '
        observer= ' '
        object= ' '
        dateobs= ' '
        timeobs= ' '
        dateend= ' '
        timeend= ' '
        equino=0.0d0
        timepixr=0.0
        dec=0.0d0
        ra=0.0d0

c      Assign a unit file number to the input file.

        call ftgiou(iunit,status)
        if(status.ne.0)then
           call fcecho('Error getting input unit number')
           call fcecho('Setting to logical unit 9')
           status=0
           iunit=9
        endif

c----------------------------------------------------------------------
c      Since we now have to be able to print out an Error column and
c      a column which describes the percentage of a bin that is filled.
c      We can no longer simply write everything out at once but have
c      to perform certain calculations before writing it out.
c      We will now perform those calculations within this subroutine
c      rather than above where it used to be performed...

        
        status=0
        if(no.gt.1)then
        
c      Assign a unit file number to the output file.

          call ftgiou(iunitn,status)
          if(status.ne.0)then
            call fcecho('Error getting input unit number')
            call fcecho('Setting to logical unit 8')
            status=0
            iunitn=8
          endif
          
c----------------------------------------------------------------------        
c      Read in the information from the last file which will be needed
c      for writing out the FITS file to be input into XSPEC.

c      Parse the original file and store it in file1 along with its
c      extension number.
          call fcpars(files(no),file1,extnum,status)
          if (status .ne. 0) then
            contxt = 'unable to parse last file name in infile'
            call fcerrm(status)
            call fcerr(contxt)
            status=0
          endif

          if (extnum .eq. -99) extnum = 1
        
c      Open the final file 
          call ftopen(iunitn,file1,0,block,status)
          if (status .ne. 0) then
            contxt = 'unable to open last file in infile'
            call fcerr(contxt)
            call fcecho(file1)
            call fcerrm(status)
            call ftclos(iunitn,status)
            call ftfiou(iunitn,status)            
            return
          endif

c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status)
c      where data# is the data unit number to go to (here the second)
c      and where type 0= primary HDU, 1= ASCII table, 2=Binary table.

          call ftmahd(iunitn,extnum+1,xtend,status)
          if(status.ne.0)then
            call fcerr('Error moving to extnum+1')
            call fcerrm(status)
            status=0
          endif

        else
          iunitn=iunit
        endif

c----------------------------------------------------------------------
c      Read in the information from the first file which will be needed
c      for writing out the FITS file to be input into XSPEC.

c      Parse the original file and store it in file1 along with its
c      extension number.
        call fcpars(files(1),file1,extnum,status)
        if(status.ne.0)then
          call fcerr('Could not parse the first file name')
          call fcerrm(status)
          status=0
        endif

	if (extnum .eq. -99) extnum = 1

c      Open the initial file 
	call ftopen(iunit,file1,0,block,status)
        if(status.ne.0)then
          call fcerr('Failure to open initial input file - aborting')
          call fcecho(file1)          
          call fcerrm(status)
          status=0
          call ftclos(iunit,status)
          call ftfiou(iunit,status)          
          labort=.TRUE.
          return
        endif

c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status)
c      where data# is the data unit number to go to (here the second)
c      and where type 0= primary HDU, 1= ASCII table, 2=Binary table.

        call ftmahd(iunit,extnum+1,xtend,status)
        if(status.ne.0)then
          call fcerr('Error moving to extnum+1')
          call fcerrm(status)
          status=0
         endif

        call ftgkys(iunit,'ORIGIN',origin,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkys(iunit,'TELESCOP',origin,contxt,status)
        endif
        if(status.ne.0)then
          call fcecho('Could not find keyword ORIGIN')
          call fcecho('Setting ORIGIN to UNKNOWN')
          origin='UNKNOWN'
          status=0
        endif
        
        call ftgkys(iunit,'DATE-OBS',dateobs,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword DATE-OBS')
          call fcerrm(status)
          status=0
        endif
        
        call ftgkys(iunit,'TIME-OBS',timeobs,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword TIME-OBS')
          call fcerrm(status)
          status=0
        endif
        
        call ftgkys(iunit,'TIMESYS',timesys,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword TIMESYS')
          call fcerrm(status)
          status=0
        endif

        timeref=' '
        call ftgkys(iunit,'TIMEREF',timeref,contxt,status)
        if(status.ne.0)then
          timeref='LOCAL'
          status=0
        endif

        tassign=' '
        call ftgkys(iunit,'TASSIGN',tassign,contxt,status)
        if(status.ne.0)then
          tassign='SATELLITE'
          status=0
        endif

        call ftgkyl(iunit,'GAINAPP',gainapp,contxt,status)
        if(status.ne.0)then
          gainapp=.FALSE.
          status=0
        endif

        call ftgkyl(iunit,'CLOCKAPP',clockapp,contxt,status)
        if(status.ne.0)then
          clockapp=.FALSE.
          status=0
        endif

        call ftgkys(iunit,'TIMEUNIT',timeunit,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword TIMEUNIT')
          call fcerrm(status)
          status=0
        endif

        call ftgkyd(iunit,'TIERRELA',tierrela,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for TIERRELA')
          call fcecho('Proceeding with TIERRELA set to 0.0d0')
          tierrela=0
          status=0
        endif

        call ftgkyd(iunit,'TIERABSO',tierabso,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for TIERABSO')
          call fcecho('Proceeding with TIERABSO set to 0.0d0')
          tierabso=0
          status=0
        endif
        
        call ftgkyd(iunit,'MJDREF',mjdref,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkyj(iunit,'MJDREFI',imjdref,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for MJDREF')
            call fcecho('Proceeding with MJDREFI set to 0')
            imjdref=0
            status=0
          endif
          call ftgkyd(iunit,'MJDREFF',mjdref,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for MJDREF')
            call fcecho('Proceeding with MJDREFF set to 0.0d0')
            mjdref=0.0d0
            status=0
          endif
        endif

        call ftgkyl(iunit,'FXBARY',lfxbary,contxt,status)
        if(status.ne.0)then
          lfxbary=.FALSE.
          status=0
        endif

        call ftgkyl(iunit,'FXTIME',lfxtime,contxt,status)
        if(status.ne.0)then
          lfxtime=.FALSE.
          status=0
        endif
        
        call ftgkys(iunitn,'DATE-END',dateend,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword DATE-END')
          status=0
        endif
        
        call ftgkys(iunitn,'TIME-END',timeend,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword TIME-END')
          status=0
        endif

        call ftgkys(iunit,'OBJECT',object,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword OBJECT')
          call fcerrm(status)
          status=0
        endif
        
        call ftgkye(iunit,'RA_PNT',ra,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkye(iunit,'RA_NOM',ra,contxt,status)
        endif
        if(status.ne.0)then
           status=0
           call ftgkye(iunit,'RA_OBJ',ra,contxt,status)
        endif

C       The following if statement is added by Zhiyu Guo on 5/12/98 to
C       enable the program to check for RA_OBS keyword.

	if(status.ne.0)then
           status=0
           call ftgkye(iunit,'RA_OBS',ra,contxt,status)
        endif
        
        if(status.ne.0)then
	   contxt='Could not find RA_NOM, RA_PNT, RA_OBJ, or RA_OBS'
C           call fcecho('Could not find RA_NOM, RA_PNT, RA_OBJ, or RA_OBS')
	   call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftgkye(iunit,'DEC_PNT',dec,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkye(iunit,'DEC_NOM',dec,contxt,status)
        endif
        if(status.ne.0)then
           status=0
           call ftgkye(iunit,'DEC_OBJ',dec,contxt,status)
        endif

C	The following if statement is added by Zhiyu Guo on 5/12/98 to
C	enable the program to check for DEC_OBS keyword.

        if(status.ne.0)then
           status=0
           call ftgkye(iunit,'DEC_OBS',dec,contxt,status)
        endif

        if(status.ne.0)then
	   contxt='Could not find DEC_NOM, DEC_PNT, DEC_OBJ, or DEC_OBS'
C           call fcecho('Could not find DEC_NOM, DEC_PNT, DEC_OBJ, or DEC_OBS')
	   call fcecho(contxt)
	   call fcerrm(status)
           status=0
        endif

        call ftgkye(iunit,'EQUINOX',equino,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find EQUINOX setting to 0.0')
          equino=0.0d0
          status=0
         endif
        
        if(status.ne.0)then
          call fcerrm(status)
          status=0
        endif
        
        call ftgkys(iunit,'RADECSYS',radecsys,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find RADECSYS setting to FK5')
          status=0
          radecsys='FK5'
        endif
        
        if(status.ne.0)then
          call fcerrm(status)
          status=0
        endif
        
        call ftgkys(iunit,'OBSERVER',observer,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkys(iunit,'TELESCOP',observer,contxt,status)
        endif
        if(status.ne.0)then
          call fcecho('Could not find keyword OBSERVER')
          call fcerrm(status)
          status=0
        endif
        
        call ftgkys(iunit,'TELESCOP',telescop,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword TELESCOP')
          call fcerrm(status)
          status=0
        endif
        
        call ftgkys(iunit,'INSTRUME',instrume,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword INSTRUME')
          call fcerrm(status)
          status=0
        endif

        call ftgkye(iunit,'TIMEDEL',deltat,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword TIMEDEL')
          call fcerrm(status)
          status=0
        endif
        
        call ftclos(iunit,status)
        if(status.ne.0)then
          call fcecho('Error closing initial input unit')
          call fcerrm(status)
          status=0
        endif
        
        call ftfiou(iunit,status)
        if(status.ne.0)then
          contxt='Error freeing output iunit number'
          call fcecho(contxt)
          status=0
        endif

        
        if(no.gt.1)then
          call ftclos(iunitn,status)
          if(status.ne.0)then
            call fcecho('Error closing last input unit')
            call fcerrm(status)
            status=0
          endif

          call ftfiou(iunitn,status)
          if(status.ne.0)then
            contxt='Error freeing output iunitn number'
            call fcecho(contxt)
            status=0
          endif
          
        endif

c        print*,'**********closed file1***********'
c----------------------------------------------------------------------

        izero = 0
        fzero=0.0d0
        ione = 1
        
c      Assign a unit file number to the output file.

        call ftgiou(ounit,status)
        if(status.ne.0)then
           call fcecho('Error getting output unit number')
           call fcecho('Setting to logical unit 9')
           status=0
           ounit=12
        endif

        
c----------------------------------------------------------------------
c      Print out the Primary Data Header 
c      
c      Parse the character output root file name 
        call fcpars(outroot,file1,extnum,status)
        if(status.ne.0)then
          call fcecho('Error parsing output root file name')
          call fcerrm(status)
          status=0
        endif

c      Parse the character output file extension name 
        call fcpars(extenlc,file2,extnum,status)
        if(status.ne.0)then
          contxt='Error parsing output extention light curve file name'
          call fcerr(contxt)
          call fcerrm(status)
          status=0
        endif
        
c      Now since the outroot only contains the prefix of the
c      file's actual name we have to parse through it and extract
c      that information and then tack on the suffix
        j=0
        k=0

        do 110 i=1,160
          if(file1(i:i).ne.' ')j=i
          if(file2(i:i).ne.' ')k=i
110     continue

        file1(j+1:j+k+1)=file2(1:k)
111     continue
              
c      Create output file and overwrite it if it exists.
        outlen=fcstln(file1)

        call fcecho(' ')
        call fcecho('Creating output light curve file:')
        call fcecho(file1(:outlen))
        
c      Open and initialize output FITS file...
        call ffinit(ounit,file1(:outlen),status)
        if(status.ne.0)then
          call fcecho('Error cannot write  LIGHTCURVE file')
          call fcerrm(status)
          return
        endif


c     Initialize variables for writing out the primary header

        simple = .TRUE.
        bitpix = 8
        naxis = 0
        pcount = 0
        gcount = 1
        extend = .TRUE.

c       call ftpdef(ounit,bitpix,naxis,0,pcount,gcount,status)
c       if(status.ne.0)then
c         contxt='Error in defining KEYWORD header space for outfile'
c         call fcecho(contxt)
c         call fcerrm(status)
c         status=0
c       endif
c 3Dec97 - No longer necessary and conflicts w/FITSIO v5.04 (MJT)

        call ftphpr(ounit,simple,bitpix,naxis,0,
     &       pcount,gcount,extend,status)
        if(status.ne.0)then
          call fcecho('Error writing initial KEYWORDS for outfile')
          call fcerrm(status)
          status=0
        endif
        
c      Write out that this file contains a LIGHT CURVE.
         call ftpkys(ounit,'CONTENT','LIGHT CURVE',
     &     'light curve file',
     &     status)
         if(status.ne.0)then
           call fcecho('Error writing keyword CONTENT')
           call fcerrm(status)
           status=0
         endif
         
c      Write out the origin of this file
         CALL FTPKYS(ounit,'ORIGIN','NASA/GSFC/XTE/GOF',
     &      'origin of fits file',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword ORIGIN')
           call fcerrm(status)
           status=0
         endif
         
c      Write out the creation date of this file...
         call ftpdat(ounit,status)
         if(status.ne.0)then
           call fcecho('Error in writing out creation date')
           call fcerrm(status)
           status=0
         endif
         
         CALL FTPKYS(ounit,'TELESCOP',telescop,
     &      'Telescope (mission) name',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TELESCOP')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkys(ounit,'INSTRUME',instrume,
     &      'Instrument used for observation',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword INSTRUME')
           call fcerrm(status)
           status=0
         endif

         if(imjdref.eq.0)then
           call ftpkyd(ounit,'MJDREF',mjdref,16,'1993.0',status)
           if(status.ne.0)then
             call fcecho('Error writing keyword MJDREF')
             call fcerrm(status)
             status=0
           endif
         else
           call ftpkyj(ounit,'MJDREFI',imjdref,
     &        'Integer part of MJDREF',status)
           if(status.ne.0)then
             call fcecho('Error writing keyword MJDREFI')
             status=0
           endif
           call ftpkyd(ounit,'MJDREFF',mjdref,15,
     &        'Fractional part of MJDREF',status)
           if(status.ne.0)then
             call fcecho('Error writing keyword MJDREFF')
             call fcerrm(status)
             status=0
           endif
         endif

         call ftpkys(ounit,'DATE-OBS',dateobs,
     &      'EARLIEST observation date of files',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword DATE-OBS')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkys(ounit,'TIME-OBS',timeobs,
     &      'EARLIEST time of all input files',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIME-OBS')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkys(ounit,'DATE-END',dateend,
     &      'LATEST observation date of files',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword DATE-END')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkys(ounit,'TIME-END',timeend,
     &      'LATEST time of all input files',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIME-END')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkys(ounit,'TIMESYS',timesys,
     &      'The time system used was',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMESYS')
           call fcerrm(status)
           status=0
         endif

         call ftpkys(ounit,'TIMEUNIT','s',
     &      'unit for TSTARTI/F and TSTOPI/F, TIMEZERO',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMEUNIT')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyd(ounit,'TSTART',timemin,14,
     &      'Observation start time',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TSTART')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyd(ounit,'TSTOP',timemax,14,
     &      'Observation stop time',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TSTOP')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkys(ounit,'OBJECT',object,
     &      'OBJECT from the FIRST input file',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword OBJECT')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkye(ounit,'RA_PNT',ra,8,
     &      'RA of First input object',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword RA_PNT')
           call fcerrm(status)
           status=0
         endif

         call ftpkye(ounit,'DEC_PNT',dec,8,
     &      'DEC of First input object',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword DEC_PNT')
           call fcerrm(status)
           status=0
         endif

         call ftpkyf(ounit,'EQUINOX',equino,2,
     &      'Equinox of the FIRST object',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword EQUINOX')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkys(ounit,'RADECSYS',radecsys,
     &      'Co-ordinate frame used for equinox',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword RADECSYS')
           call fcerrm(status)
           status=0
         endif

         call ftpkys(ounit,'TIMVERSN','OGIP/93-003',
     &      'OGIP memo number for file format',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMVERSN')
           call fcerrm(status)
           status=0
         endif

c       Write out the name of the code and its version number         
         call ftpkys(ounit,'CREATOR',taskname,
     &      'Program name that produced this file',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword CREATOR')
           call fcerrm(status)
           status=0
         endif

         call ftpcks(ounit,status)
         if(status.ne.0)then
           call fcecho(' ')
           call fcecho('ERROR creating/updating CHKSUM keyword.')
           status=0
         endif
         
c     Finished writing out the Primary Data header so get ready to
c     create another Header Data Unit

c     Creating second header unit
         call ftcrhd(ounit,status)
         if(status.ne.0)then
           call fcerr('ERROR creating second header extension....')
           call fcerrm(status)
           status=0
         endif

c      Define the information about the number of columns and the
c      like. This information will have to change at a later date as
c      the options for calculating errors and other information are
c      included. Actually the above will have to be expanded
c      as well... 

        gcount = 1
        pcount = 0

c      See what option has been chosen - either EVENT_SUM
c      or EVENT_RATE is supported.
c----------------------------------------------------------------------

c     Initialize variables before we use them. 
        itest=0
        irow=0
        dbegin=dtruc(dpernoarray(1)-1.0d0)
        if(dbegin.lt.1.0d0)dbegin=0.0d0
          
C----------------------------------------------------------------------

        
c----------------------------------------------------------------------        
        if ( lcmode.eq.'EVENT_SUM'.or.
     &     lcmode.eq.'SUM' ) then

c      Write out the header information standard keywords for
c      writing a binary table extension.
          call ftphbn(ounit,icount,nfield,ttypes,tforms,tunits,
     &       extnams,pcount,status)
          if(status.ne.0)then
            call fcerr('Error in writing header information')
            call fcerrm(status)
            status=0
          endif
          
        else if ( lcmode.eq.'EVENT_RATE'.or.
     &       lcmode.eq.'RATE') then

c      Write out the header information standard keywords for
c      writing a binary table extension.
          call ftphbn(ounit,icount,nfield,ttyper,tformr,tunitr,
     &       extnamr,pcount,status)
          if(status.ne.0)then
            call fcerr('Error in writing header information')
            call fcerrm(status)
            status=0
          endif

        else if ( lcmode.eq.'MEAN') then

c      Write out the header information standard keywords for
c      writing a binary table extension.
          call ftphbn(ounit,icount,nfield,ttypem,tformm,tunitm,
     &       extnams,pcount,status)
          if(status.ne.0)then
            call fcerr('Error in writing header information')
            call fcerrm(status)
            status=0
          endif

        endif
c----------------------------------------------------------------------
c      Now that we have the storage information entered. We will enter
c      the pertinent information with regard to HDUCLASS.

        call ftpkys(ounit,'HDUCLASS','OGIP',
     &     'format conforms to OGIP/GSFC standards',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword HDUCLASS')
           call fcerrm(status)
           status=0
         endif

        call ftpkys(ounit,'HDUCLAS1','LIGHTCURVE',
     &      'Extension contains a Light Curve',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword HDUCLAS1')
          call fcerrm(status)
          status=0
        endif

        call ftpkys(ounit,'HDUCLAS2','TOTAL',
     &     'Extension contains a Light Curve',status)        
        if(status.ne.0)then
          call fcecho('Error writing keyword HDUCLAS2')
          call fcerrm(status)
          status=0
        endif
        
        if(lcmode.eq.'SUM'.or.lcmode.eq.'EVENT_SUM')
     &     call ftpkys(ounit,'HDUCLAS3','COUNT',
     &     'Extension contains counts',status)        

         if(lcmode.eq.'RATE'.or.lcmode.eq.'EVENT_RATE')
     &     call ftpkys(ounit,'HDUCLAS3','RATE',
     &     'Extension contains rate',status)        

         if(lcmode.eq.'MEAN')
     &      call ftpkys(ounit,'HDUCLAS3','MEAN',
     &      'Extension contains mean',status)        

        call ftpkys(ounit,'HDUVERS1','1.1.0',
     &       'Version number of the format',status)        
         if(status.ne.0)then
            call fcecho('Error writing keyword HDUVERS1')
            call fcerrm(status)
            status=0
         endif

C----------------------------------------------------------------------
c      Let's write out additional information that is required for
c      XSPEC to function.

         call ftpkys(ounit,'TIMVERSN','OGIP/93-003',
     &      'OGIP memo number for file format',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMVERSN')
           call fcerrm(status)
           status=0
         endif

         call ftpkys(ounit,'TIMESYS',timesys,
     &      'The time system used was',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMESYS')
           call fcerrm(status)
           status=0
         endif

        call ftpkys(ounit,'DATE-OBS',dateobs,
     &     'EARLIEST observation date of files',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword DATE-OBS')
          call fcerrm(status)
          status=0
        endif
        
        call ftpkys(ounit,'TIME-OBS',timeobs,
     &     'EARLIEST time of all input files',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword TIME-OBS')
          call fcerrm(status)
          status=0
        endif
         
        call ftpkys(ounit,'DATE-END',dateend,
     &     'LATEST observation date of files',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword DATE-END')
          call fcerrm(status)
          status=0
        endif
         
        call ftpkys(ounit,'TIME-END',timeend,
     &     'LATEST time of all input files',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword TIME-END')
          call fcerrm(status)
          status=0
        endif
         
         call ftpkys(ounit,'TIMEUNIT','s',
     &      'unit for TSTARTI/F and TSTOPI/F, TIMEZERO',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMEUNIT')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyf(ounit,'TIMEPIXR',timepixr,1,
     &      'Timestamps give leading edge of bin',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMEPIXR')
           call fcerrm(status)
           status=0
         endif
         
c         call ftpkys(ounit,'TIMEREF',timeref,
c     &      'barycentric correction applied to times',status)
c   (MJT) 15Jun00 changed treatment of TIMEREF (see also below and fxbary.f)
         if (.not.lfxtime)then
            call ftpkys(ounit,'TIMEREF',timeref,
     &           'No path length corrections',status)
         endif
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMEREF')
           call fcerrm(status)
           status=0
         endif

         if(imjdref.eq.0)then
           call ftpkyd(ounit,'MJDREF',mjdref,16,'1993.0',status)
           if(status.ne.0)then
             call fcecho('Error writing keyword MJDREF')
             call fcerrm(status)
             status=0
           endif
         else
           call ftpkyj(ounit,'MJDREFI',imjdref,
     &        'Integer part of MJDREF',status)
           if(status.ne.0)then
             call fcecho('Error writing keyword MJDREFI')
             status=0
           endif
           call ftpkyd(ounit,'MJDREFF',mjdref,15,
     &        'Fractional part of MJDREF ',status)
           if(status.ne.0)then
             call fcecho('Error writing keyword MJDREFF')
             call fcerrm(status)
             status=0
           endif
         endif
         
c     LCBIN contains the logical that tells if any values have been
c     set to TNULL. If that is the case then we want to write out
c     the TNULL keyword so that this data isn't processed but can
c     be stripped out of the output file
         if(lcbin)then
           call ftpkye(ounit,'TNULL',tnull,8,
     &        'TNULL value',status)
           if(status.ne.0)then
             call fcecho('Error writing keyword TNULL')
             call fcerrm(status)
             status=0
           endif
         endif
         
         call ftpkys(ounit,'TASSIGN',tassign,
     &      'where time is assigned',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TASSIGN')
           call fcerrm(status)
           status=0
         endif


c        Since we have to allow for subtracting TIMEZERO we need to
c rework the algorithm
         

         if(ctimezero.eq.'INDEF')then
c           print*,'In TIMEZERO INDEF'
           timezero=0.0d0
           istarti=dint(tfirst+(dbegin*binsz))
           startf=((tfirst+(dbegin*binsz))-
     &        dfloat(istarti))
           
         elseif(ctimezero.eq.'FLOAT')then
c           print*,'In TIMEZERO FLOAT'           
           timezero=0.0d0
           istarti=dint((-0.5d0)*binsz)
           istarti=0.0d0
           startf=(-0.5d0*binsz)-dfloat(istarti)
           startf=0.0d0
           
         elseif(timezero.ne.0.0d0)then
c           print*,'In TIMEZERO NE 0.0d0'
           istarti=dint(tfirst+(dbegin*binsz)-timezero)
           startf=((tfirst+(dbegin*binsz)-timezero)-
     &        dfloat(istarti))
         endif
         
         call ftpkyj(ounit,'TSTARTI',istarti,
     &      'integer portion of start time',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TSTARTI')
           call fcerrm(status)
           status=0
         endif

c        Unfortunately due to the limited accuracy of the machine
c we have to use a rounding function to remove the garbage that
c accumulates at the end of the double precisions number, thus we limit our
c accuracy to 1.0d-8 seconds which should be accurate enough for our
c applications. So we first multiply by 1.0d8 and then round our value
c and then expand it back out.
         
         startf=startf*1.0d8
         startf=(dnint(startf))
         startf=(startf)/1.0d8
         
         call ftpkyd(ounit,'TSTARTF',startf,14,
     &      'fractional observation start time',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TSTARTF')
           call fcerrm(status)
           status=0
         endif


c        Since we have to allow for subtracting TIMEZERO we need to
c rework the algorithm

         if(ctimezero.eq.'INDEF')then
           istopi=dint(timemax+(binsz))
           stopf=(timemax+(binsz))-dfloat(istopi)
           timezero=0.0d0
           
         elseif(ctimezero.eq.'FLOAT')then
           timetemp=(timemax+(binsz))-
     &        (tfirst+((dbegin+1.0d0)*binsz))
           istopi=dint(timetemp)
           stopf=(timetemp-dfloat(istopi))
           
         elseif(timezero.ne.0.0d0)then
           istopi=dint(timemax+(binsz)-timezero)
           stopf=(timemax+(binsz)-timezero)-dfloat(istopi)
         endif
       

c         print*,'TIMETEMP is ',timetemp,(timemax+(0.5d0*binsz)),
c     &      (tfirst+((dbegin+0.5d0)*binsz))

c         print*,'STOP is ',istopi,stopf
         
         call ftpkyj(ounit,'TSTOPI',istopi,
     &      'integer observation stop time',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TSTOPI')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyd(ounit,'TSTOPF',stopf,14,
     &      'fractional observation stop time',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TSTOPF')
           call fcerrm(status)
           status=0
         endif

         if(ctimezero.eq.'INDEF')then
           istarti=0.0d0
           startf=0.0d0
           timezero=0.0d0

         elseif(ctimezero.eq.'FLOAT')then
           istarti=dint(tfirst+((dbegin)*binsz))
           startf=((tfirst+((dbegin)*binsz))-
     &        dfloat(istarti))

         elseif(timezero.ne.0.0d0)then
           istarti=dint(timezero)
           startf=timezero-istarti
         endif
         
         call ftpkyj(ounit,'TIMEZERI',istarti,
     &      'integer zerotime to calculate t(n) bin',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMEZERI')
           call fcerrm(status)
           status=0
         endif

c        Unfortunately due to the limited accuracy of the machine
c we have to use a rounding function to remove the garbage that
c accumulates at the end of the double precisions number, thus we limit our
c accuracy to 1.0d-8 seconds which should be accurate enough for our
c applications. So we first multiply by 1.0d8 and then round our value
c and then expand it back out.
         
         startf=startf*1.0d8
         startf=(dnint(startf))
         startf=(startf)/1.0d8
         
         call ftpkyd(ounit,'TIMEZERF',startf,14,
     &      'fractional zerotime to calculate t(n) bin',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMEZERF')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyd(ounit,'TIMEDEL',binsz,14,
     &      'integration time',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIMEDEL')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyd(ounit,'TIERRELA',tierrela,14,
     &      'relative time error',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIERRELA')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyd(ounit,'TIERABSO',tierabso,14,
     &      'absolute time error',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TIERABSO')
           call fcerrm(status)
           status=0
         endif

c         print*,'TOTSEC's before 1 is ",totsecs
c        Unfortunately due to the limited accuracy of the machine when
c performing additions to accumulate the total time we have to perform
c a truncation such that we chop off all of the noise at the end.
         totsecs=totsecs*1.0d8
         totsecs=(dnint(totsecs))
         totsecs=(totsecs)/1.0d8

c         print*,'TOTSEC's after 1 is ",totsecs
         
         call ftpkyd(ounit,'ONTIME',totsecs,14,         
     &      'time on source',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword ONTIME')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyl(ounit,'BACKAPP',.FALSE.,
     &      'background is subtracted',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword BACKAPP')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyl(ounit,'DEADAPP',.FALSE.,
     &      'deadtime correction applied',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword DEADAPP')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyl(ounit,'VIGNAPP',.FALSE.,
     &      'vignetting or collimator applied',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword VIGNAPP')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyl(ounit,'GAINAPP',gainapp,
     &      'Gain all ready subracted',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword GAINAPP')
           call fcerrm(status)
           status=0
         endif

         call ftpkyl(ounit,'CLOCKAPP',clockapp,
     &      'Clock correction applied',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword CLOCKAPP')
           call fcerrm(status)
           status=0
         endif

         if(lfxbary)then
           call ftpkyl(ounit,'FXBARY',lfxbary,
     &        'BARYCENTER corrected file',status)
           if(status.ne.0)then
             call fcecho(' ')
             call fcecho('Error echoing FXBARY keyword')
             status=0
           endif
           
           if(lfxtime)then
             call ftpkyl(ounit,'FXTIME',lfxtime,
     &          'TIME column was barycenter corrected',status)
             if(status.ne.0)then
               call fcecho(' ')
               call fcecho('Error echoing FXTIME keyword')
               status=0
             endif
C   (MJT) 15Jun00 modified treatment of TIMEREF kwd (see also above)
             call ftpkys(ounit,'TIMEREF','SOLARSYSTEM',
     &            'barycentric correction applied to times',status)
             if(status.ne.0)then
                call fcecho(' ')
                call fcecho('Error writing TIMEREF keyword')
                status=0
             endif
           endif
         endif

         do i=1,noofcomments
           call ftpcom(ounit,commentlines(i),status)
           if(status.ne.0)then
             call fcecho(' ')
             call fcecho('Unable to write comment')
             status=0
           endif
         enddo

         if(chbin.eq.'INDEF')then
           if(cpixold.ne.' ')then
             call ftplsw(ounit,status)
             if(status.ne.0)then
               call fcecho(' ')
               call fcecho('Unable to define LONGSTRN')
               status=0
             endif
             call ftpkls(ounit,'CPIX',cpixold,
     &          'Channel binning of CHANNEL column',status)
c         print*,'wrote OBJECT',status
             if(status.ne.0)then
               contxt='Error writing keyword CPIX'
               call fcecho(contxt)
               status=0
             endif
           endif
         else
           if(chbin.ne.' ')then
             call ftplsw(ounit,status)
             if(status.ne.0)then
               call fcecho(' ')
               call fcecho('Unable to define LONGSTRN')
               status=0
             endif
             call ftpkls(ounit,'CPIX',chbin,
     &          'Channel binning of CHANNEL column',status)
c         print*,'wrote OBJECT',status
             if(status.ne.0)then
               contxt='Error writing keyword CPIX'
               call fcecho(contxt)
               status=0
             endif
           endif
         endif
         
         call ftpkyj(ounit,'MINCHAN',ilbd,
     &      'minimum channel included in bin',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword MINCHAN')
           call fcerrm(status)
           status=0
         endif
         
         call ftpkyj(ounit,'MAXCHAN',iubd,
     &      'maximum channel include in bin',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword MAXCHAN')
           call fcerrm(status)
           status=0
         endif
         
c      Write out the mission name.
         call ftpkys(ounit,'TELESCOP',telescop,
     &      'Telescope (mission) name',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword TELESCOP')
           call fcerrm(status)
           status=0
        endif
        
c      Write out the name of the instrument
        call ftpkys(ounit,'INSTRUME',instrume,
     &     'Instrument name',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword INSTRUME')
          call fcerrm(status)
          status=0
        endif
        
        call ftpkys(ounit,'OBJECT',object,
     &     'OBJECT from the FIRST input file',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword OBJECT')
          call fcerrm(status)
          status=0
        endif
        
        call ftpkye(ounit,'RA',ra,8,
     &     'RA of First input object',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword RA')
          call fcerrm(status)
          status=0
        endif
         
        call ftpkye(ounit,'DEC',dec,8,
     &     'DEC of First input object',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword DEC')
          call fcerrm(status)
          status=0
        endif
         
        call ftpkyf(ounit,'EQUINOX',equino,2,
     &     'Equinox of the FIRST object',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword EQUINOX')
          call fcerrm(status)
          status=0
        endif
         
        call ftpkys(ounit,'RADECSYS',radecsys,
     &     'Co-ordinate frame used for equinox',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword RADECSYS')
          call fcerrm(status)
          status=0
        endif
        

        call ftpkns(ounit,'ROWID',1,isave,cols,
     &     'Column Name processed&',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword ROWID')
          call fcecho('Continuing...')
          status=0
        endif

        call xteftpklns(ounit,'FILEN',1,no,files,
     &     'Input files input to produce this file&',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword FILEN')
          call fcecho('Continuing...')
          status=0
        endif
        
c      Write out the origin of this file
        CALL FTPKYS(ounit,'ORIGIN','NASA/GSFC',
     &     'origin of fits file',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword ORIGIN')
          call fcerrm(status)
          status=0
        endif

c       Write out the name of the code and its version number
        call ftpkys(ounit,'CREATOR',taskname,
     &     'Program name that produced this file',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword CREATOR')
          call fcerrm(status)
          status=0
        endif
         
c      Write out the creation date of this file...
        call ftpdat(ounit,status)
        if(status.ne.0)then
          call fcecho('Error in writing out creation date')
          call fcerrm(status)
          status=0
        endif

c      Define the structure of the binary table date unit so that
c      we can actually write out the informtion.
c 3Dec97 - No longer necessary and conflicts w/FITSIO v5.04 (MJT)

c       if ( lcmode.eq.'EVENT_SUM'
c    &     .or.lcmode.eq.'SUM' ) then
c          
c         call ftbdef(ounit,nfield,tforms,pcount,icount,status)
c         if(status.ne.0)then
c           call fcerr('Error in allocating output space for file')
c           call fcerrm(status)
c           status=0
c         endif
c         
c       else if ( lcmode.eq.'EVENT_RATE'.or.
c    &       lcmode.eq.'RATE') then
c          
c         call ftbdef(ounit,nfield,tformr,pcount,icount,status)            
c         if(status.ne.0)then
c           call fcerr('Error in allocating output space for file')
c           call fcerrm(status)
c           status=0
c         endif
c           
c       else if ( lcmode.eq.'MEAN') then
c          
c         call ftbdef(ounit,nfield,tformm,pcount,icount,status)
c         if(status.ne.0)then
c           call fcerr('Error in allocating output space for file')
c           call fcerrm(status)
c           status=0
c         endif
c
c       endif

980     continue
        
c      Write out the r*4 value to the second column of the binary
c      file.

        if(lfirstgo)then
          irow=0
          itest=0
        else
          irow=0
          itest=0
        endif

        do 290 k=1,nchan
 
c          print*,'Doing loop',k,itest,irow,icount
          
          itest=itest+1

c          print*,'IROW and IROWSAVE and Icount are',
c     &       irow,irowsave,irow+irowsave,icount
          
c          if((irow+irowsave).eq.icount)goto 291

          kval=itest

          timearay=(dtruc(dpernoarray(kval)-1.0d0-dbegin))*binsz

222       continue

c          if(realbin(kval).le.0.0d0)
c     &       print*,'ZERO in realbin',kval
          if(realbin(kval).gt.0.0d0)then
            irow=irow+1

            timeval=timearay
          
c            print*,'timeval ',timearay,timeval,itest,
c     &         itemp,dbegin

c           Check to see if irow+irowsave is positive
            if(irow+irowsave.lt.0)then
              call fcecho(' ')
              call fcecho('    ********Error**********    ')
              call fcecho(' Attempting to move to a negative row! ')
              call fcecho(' Cannot continue aborting ')
              labort=.TRUE.
            endif
            
c     Write out the double precision timestamp that is associated
c     with this bin.

            if(ctimezero.eq.'INDEF')then
              timezero=0.0d0
              timeval2=timearay+tfirst+(dbegin*binsz)
            elseif(ctimezero.eq.'FLOAT')then
              timeval2=timearay
            elseif(timezero.ne.0.0d0)then
              timeval2=timearay+tfirst+(dbegin*binsz)-timezero
            endif

c            print*,'TIMEVAL and TIMEVAL2 are',
c     &         timeval,timeval2,timezero,
c     &         ctimezero,dbegin,tfirst
            call ftpcld(ounit,1,irow+irowsave,1,1
     &         ,timeval2,status)

            lasttime=timeval

c            print*,'IROW + IROWSAVE is',irow,irowsave,irow+irowsave
c     Since we may have multiple light curves to be writing, we will have
c     one sequence of columns associated with each input column (INOCOLS)
            
            do 210 kcol=1,inocols
              ival=(kcol-1)*icolfset
c              print*,'IVAL+KVAL is',ival,kval,ival+kval,
c     &           realbin(kval),binsz
              
              if(lcmode.eq.'RATE'.or.
     &           lcmode.eq.'EVENT_RATE')then
                realval=(dbin(kval+ival)/(realbin(kval)))
                error=dsqrt(dabs(dbin(kval+ival)))/
     &             (realbin(kval))
                if(realbin(kval).ne.0.0d0.and.kval.ne.0)then
                  fracexp=((realbin(kval))/binsz)
                  
                else
                  fracexp=0.0e0
                endif
                      
              elseif(lcmode.eq.'SUM'.or.
     &             lcmode.eq.'EVENT_SUM')then
                realval=(dbin(kval+ival))
                error=dsqrt(dabs(dbin(kval+ival)))
                if(realbin(kval).ne.0.0d0.and.kval.ne.0)then
                  fracexp=((realbin(kval))/binsz)
                else
                  fracexp=0.0d0
                endif
                      
              elseif(lcmode.eq.'MEAN')then
                realval=(dbin(kval+ival)/(dbincount(kval)))
                error=dsqrt(dabs(realval))
                if(realbin(kval).ne.0.0d0.and.kval.ne.0)then
                  fracexp=((realbin(kval))/binsz)
                else
                  fracexp=0.0d0
                endif

c                print*,'FRACEXP is ',realbin(kval),binsz,fracexp
              endif

c     Since we may be writing out multiple columns we have to make sure
c     that we write each column in proper sequence and then increment by
c     a specific value. So here we are determining the actual column
c     number that that we will be writing to in each row. We are doing
c     this in such a way that each row is filled in the most efficient
c     way possible. 
              icval=3*kcol-1
              icval2=3*kcol
              icval3=3*kcol+1

              fracexp=fracexp*1.0d9
              fracexp=(dnint(fracexp))
              fracexp=(fracexp)/1.0d9
              
c     Actually write out the information to the proper column
              call ftpcld(ounit,icval,irow+irowsave,1,1
     &           ,realval,status)
              call ftpcld(ounit,icval2,irow+irowsave,1,1
     &           ,error,status)
              call ftpcld(ounit,icval3,irow+irowsave,1,1
     &           ,fracexp,status)                          

210         continue

          endif
          
290     continue
291     continue

        if(status.ne.0)then
          call fcerr('Error in writing information to file')
          call fcerrm(status)
          status=0
        endif

c       Since we may be doing multiple cycles we have to store the initial
c starting offset for each iteration. This value is stored in DBEGIN and is
c primarily used in completing the timing information. 
c        print*,'DBEGIN and ITEST are',dbegin,irowsave,
c     &     timearay,timeval

        irowsave=irow+irowsave

c        print*,'NAXIS2 is ',irow, irowsave

        call ftmkyj(ounit,'NAXIS2',irowsave,
     &     'number of rows in table',status)
        if(status.ne.0)then
          call fcecho('ERROR in updating number of rows in LIGHTCURVE')
          status=0
        endif

c        print*,'TOTSEC's before is ",totsecs
c        Unfortunately due to the limited accuracy of the machine when
c performing additions to accumulate the total time we have to perform
c a truncation such that we chop off all of the noise at the end.
        totsecs=totsecs*1.0d8
        totsecs=(dnint(totsecs))
        totsecs=(totsecs)/1.0d8

c        print*,'TOTSEC's after is ",totsecs
        call ftmkyd(ounit,'ONTIME',totsecs,14,         
     &     'time on source',status)
        if(status.ne.0)then
          call fcecho('ERROR in updating ONTIME in LIGHTCURVE')
          status=0
        endif
          
        call ftrdef(ounit,status)
        if(status.ne.0)then
          call fcecho('ERROR in Redefining the LIGHTCURVE output file')
          status=0
        endif
          
c        endif
        
c     Since we have to append the GTI extension we do not close the
c     file that we opened within this subroutine. We output the proper
c     logical unit number so that we can write out the GTI's

        if(lasttime.ne.0.0)then
c          print*,'tfirst, dbegin, lasttime',tfirst,dbegin,lasttime
          timearay=tfirst+((dtruc(dbegin+1.0d0))*binsz)+lasttime
c          print*,'timearay is ',timearay

            

c        Since we have to allow for subtracting TIMEZERO we need to
c rework the algorithm

          if(ctimezero.eq.'INDEF'.and.timezero.eq.0.0d0)then
            istopi=dint(timearay)
            stopf=(timearay-dfloat(istopi))
            
          elseif(ctimezero.eq.'FLOAT')then
            timetemp=(timearay)-
     &         (tfirst+((dbegin)*binsz))
            istopi=dint(timetemp)
            stopf=(timetemp-dfloat(istopi))
            
          elseif(timezero.ne.0.0d0)then
            timearay=tfirst+((dtruc(dbegin+1.0d0))*binsz)
     &         +lasttime-timezero
            istopi=dint(timearay)
            stopf=(timearay-dfloat(istopi))
          endif

c         print*,'TIMETEMP 2 is',timetemp,(timemax+(0.5d0*binsz)),
c     &      (tfirst+((dbegin+0.5d0)*binsz))
         
c         istopi=dint(timetemp)
c         stopf=(timetemp-dfloat(istopi))

c         print*,'STOP 2 is ',istopi,stopf
            
          call ftmkyj(ounit,'TSTOPI',istopi,
     &       'integer observation stop time',status)
          if(status.ne.0)then
            call fcecho('Error updating keyword TSTOPI')
            call fcerrm(status)
            status=0
          endif
         
          stopf=stopf*1.0d8
          stopf=(dnint(stopf))
          stopf=(stopf)/1.0d8
        
          call ftmkyd(ounit,'TSTOPF',stopf,14,
     &       'fractional observation stop time',status)
          if(status.ne.0)then
            call fcecho('Error updating keyword TSTOPF')
            call fcerrm(status)
            status=0
          endif
        endif
        
        return
        end
