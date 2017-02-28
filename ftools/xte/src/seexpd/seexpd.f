c******************************************************************************
C SELECTOR TASK:
C      seexpd
C
C FILE:
C      seexpd.f 
C
C DESCRIPTION: 
C      Read in a 
C     
C AUTHOR:  
C      Brian K. Elza 1/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      
C
C      
C ARGUMENTS:
C      
C
C PRIMARY LOCAL VARIABLES:
C      infile     - input FITS file and extension number
C      outfile    - output fil
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
C      outimecol  - column name for bin centers      
C      outcol     - column name for binned parameter(s) values
C      sensecase  - whether values should be case sensitive
C      
C CALLED ROUTINES:
C      subroutine gfcolv - gets parameters from environment
C      subroutine ffcolv - read input FITS file and writes light curve file
C
C***************************************************************************
        subroutine seexpd
c        implicit none
	character(160) infile, outfile
	character(80) timecol, columns, outimecol, outcol
        integer status
        logical sensecase
	character(40) taskname
	common /task/ taskname

	taskname = 'seexpd_3.3'
	infile = ' '
	outfile = ' '
	timecol = ' '
	columns = ' '
	outimecol = ' '
	outcol = ' '

C     get the parameters from the par file
        call seinpar(infile,outfile,timecol,columns,
     &       outimecol,outcol,sensecase,status)

c        print*,'Values are infile,outfile,timecol,columns,
c     &       outcol,outimecol,sensecase,status'
c        print*,infile,outfile,timecol,columns,
c     &       outcol,outimecol,sensecase,status

        if (status .ne. 0) goto 999

C      Read inthe FITS file and write out the light curve file.

        call seinpfil(infile,outfile,timecol,columns,
     &       outimecol,outcol,sensecase,status)

999     return
        end

c**********************************************************************
c
c SUBROUTINE:
C      seinpar
c
c DESCRIPTION:
C      Get parameters from parameter file
C      
c AUTHOR:
C      Brian K. Elza 1/94
c
c MODIFICATION HISTORY:
C      None, yet...
c
c NOTES:
C      seinpar uses F77/VOS like calls to read parameters from the .par file
C      
c USAGE:
c        call seinpar(infile,outfile,timecol,columns,
c     &       outimecol,outcol,status)
c
c
c ARGUMENTS:
C      infile     - input FITS file and extension number
C      outfile    - output file name
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
C      outimecol  - column name for bin centers      
C      outcol     - column name for binned parameter(s) values
C      sensecase  - whether values should be case sensitive
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
c**********************************************************************

        subroutine seinpar(infile,outfile,timecol,columns,
     &       outimecol,outcol,sensecase,status)

c        implicit none
	character*(*) infile, outfile
	character*(*) timecol, columns, outimecol, outcol
        character(80) context
	integer status
        logical sensecase,clobber
        
C      initialize variables
        status=0

C      get the name of the input FITS file
	call uclgst('infile',infile,status)
	if (status .ne. 0) then
           context = 'could not get INFILE parameter'
           call fcerr(context)
           goto 999
	endif
        
C  get the name of the output file
	call uclgst('outfile',outfile,status)
	if (status .ne. 0) then
           context = 'could not get OUTFILE parameter'
           call fcerr(context)
           goto 999
	endif

C  get the time column string
	call uclgst('timecol',timecol,status)
        if(timecol.eq.' ')then
           context = 'Time column header blank useing TIME'
           call fcecho(context)
           timecol='TIME'
        endif
        
	if (status .ne. 0) then
           context = 'could not get TIMECOL parameter'
           call fcerr(context)
           goto 999
	endif

C  get the columns
	call uclgst('columns',columns,status)
        if(columns.eq.' ')then
           context = 'Column header blank! Must have value'
           call fcecho(context)
           status=1
        endif

	if (status .ne. 0) then
           context = 'could not get COLUMNS parameter'
           call fcerr(context)
           goto 999
	endif

C  get the title for the output time column
	call uclgst('outimecol',outimecol,status)
	if (status .ne. 0) then
           context = 'could not get OUTIMECOL parameter'
           call fcerr(context)
           goto 999
	endif
        
C  get the title for the output column
	call uclgst('outcol',outcol,status)
	if (status .ne. 0) then
           context = 'could not get OUTCOL parameter'
           call fcerr(context)
           goto 999
	endif

C  get whether to be case sensitive
	call uclgsb('sensecase', sensecase,status)
	if (status .ne. 0) then
           context = 'could not get SENSECASE parameter'
           call fcerr(context)
           goto 999
	endif

c      Take care of all of the case sensitive issues... 
        if(.not.sensecase)then
           call ftupch(timecol)
           call ftupch(columns)
        endif

C  get whether to be case sensitive
	call uclgsb('clobber', clobber,status)
	if (status .ne. 0) then
           context = 'could not get CLOBBER parameter'
           call fcerr(context)
           goto 999
	endif

        
999	continue

	if (status .ne. 0)  call fcerrm(status)

	return
	end

c****************************************************************************
c
c      seinpfil
c
c DESCRIPTION:
c      Gets the information necessary to begin processing files.
c      We will need to acquire all of the information about how the
c      data is stored in the FITS files. This routine does not
c      actually process any of the files, but simply gets all of the
c      information stored in the header which will be used in reading
c      in and processing the information. 

C INPUT PARAMETERS
C      iunit   i  Fortran i/o unit number
c      outfile c  Outfile
c      timecol c  Character string used for TIME header.
c      columns c  Character string used for count header.
c      
C MAJOR LOCAL VARIABLES:
C      nrows   i  number of rows in the table
C      nfield  i  number of fields in the table
C      ttype   c  name of each field (array)
C      tform   c  Fortran-77 format of each field (array)
C      tunit   c  units of each field (array)
C      extnam  c  name of table (optional)
C      status  i  returned error status (0=ok)
c      
C OUTPUT PARAMETERS:
c      coltime i  column number which has timecol as the header (array)
c      colcnt  i  column number which has columns as the header (array)
c      
c CALLED ROUTINES:
c      fcpars    - parses a string to extract a file name
c      fcerr     - print error message associated with status
c      ftopen    - assign a unit number to a filename and open it
c      ftmahd    - move to absolute header data unit
c      ftghbn    - read required header keywords from a binary table ext.
c      ftgcno    - determine col. num. corresponding to column name
c      ftgkys    - get string value associated with keyword from header.
c      ftgcx     - read in a logical array of elements
c      ftgcld    - read in a double precsion array of elements        
c      ftclos    - close up the file that was read in.
c      
c**********************************************************************
      
        subroutine seinpfil(infile,outfile,timecol,columns,
     &       outimecol,outcol,sensecase,status)        
c        implicit none

c      Define all of the common parameters used in the arrays.
        logical anynul,flgval,sensecase,exact,parflg,foundchn
        integer ikey2,ikey3,ikey4,isml
        integer nf,nb,ne,siz,ichan
        integer nrows,nfield,pcount,status,
     &       ikeyval,i,j

        parameter (ikey2=19)
        parameter (ikey3=4)
        parameter (ikey4=6)
        parameter (isml = 8)
        parameter (nb = 128)
        parameter (nf = 256)
        parameter (ne = 1024)
        parameter (siz = 50)
        parameter (ichan = 1000000)

        integer*2 itotal,isize
        logical array(nb)
        integer block,coltime,colcnt,
     &       naxis,colpos,
     &       lrow,colpost,extnum,
     &       ounit,iunit,iextnum,oextnum,xtend
        integer nulval,incr,jsize,
     &       nultyp,iword,iword2,nchan,
     &       ftstatus,iskip,outlen,fcstln,
     &       rcount,dtype,width,fbit,istart,istop,outvali
        real keyval
        double precision timearay,nulvald

        character*(*) infile,outfile,timecol,columns,
     &       outimecol,outcol
        character(1) cnum(10)
        character(6) keyword
        character(8) keyword2(ikey2),keyword3(ikey3),keyword4(ikey4)
        character(20) outval
        character(80) ttype(nb),tform(nb),tunit(nb),extnam,
     &       filein,fileout,context,otype(nb),otform(nb),otunit(nb)
        character(80) ctyp(isml),tpack(nb),comm,coltemp
        character(240) tddes(nb),tmpval
c      Set up an array where an integer value is assigned an
c      ascii character value
        data (cnum(i),i=1,10)/'0','1','2','3','4','5',
     &       '6','7','8','9'/
        data (keyword2(i),i=1,ikey2)/'ORIGIN','CREATOR','HDUCLASS',
     &       'HDUCLAS1','HDUCLAS2','TIMESYS','TIMEUNIT',
     &       'DATE','DATE-OBS','TIME-OBS','DATE-END',
     &       'TIME-END','OBJECT','RADECSYS','OBSERVER','OBSID',
     &       'TELESCOP','INSTRUME','DATAMODE'/
        data (keyword3(i),i=1,ikey3)/'EXTVER','EXTLEVEL','APPID',
     &       'TIMEDEL'/
        data (keyword4(i),i=1,ikey4)/'RA_PNT','DEC_PNT','EQUINOX',
     &       'TSTART','TSTOP','MJDREF'/

c        print*,'infile,outfile,no'
c        print*,infile,outfile,no
        
c      Assign a unit file number used in inputting file.
        iunit=10
        
c      Assign a unit file number to the output file.
        ounit=11

        status=0
        foundchn=.FALSE.
        parflg=.FALSE.
        exact=.FALSE.
        coltemp=' '
        coltime=0
        colcnt=0
        iskip=0
        istart=0
        if(sensecase)exact=.TRUE.
c        print*,'sensecase is ',sensecase
        
c**********************************************************************
c      initialize variables:
        do 12 j=1,nb
           otform(j)=' '
           otype(j)=' '
           otunit(j)=' '
12      continue

        do 13 iword=1,isml
           ctyp(iword)='$'
           tddes(iword)='$'
13      continue

c        print*,'done initializing'
        
c      Set up unit number for reading in values. This really isn't
c      necessary since we successively open and close each file, but 
        
c      Parse the character input file name 
        call fcpars(infile,filein,iextnum,status)

c        print*,'parse filename',infile,filein,iextnum
        
c      Open/initialize the correct input file
	call ftopen(iunit,filein,0,block,status)
c        print*,'Opened filein',iunit,filein,status   

c      Parse the character output filename
        call fcpars(outfile,fileout,oextnum,status)
c        print*,'parsed outfile ',outfile,fileout

        outlen=fcstln(fileout)
c      Open and initialize output FITS file...
c        CALL FTINIT(ounit,fileout,block,status)
        call ffinit(ounit,fileout(:outlen),status)
c        print*,'initialialized outfile ',ounit,fileout,status
           
	if (status .ne. 0) then
           context = 'Output file exists. Cannot continue'
           call fcerrm(status)
           call fcerr(context)
           goto 999
        endif

c        print*,'About to call copy header command'
c      Copy the primary header from the input file to the output file
        call ftcopy(iunit,ounit,0,ftstatus)
c        print*,'Copied header',ftstatus
        
        if(ftstatus.ne.0)then
           context='Unable to copy primary header from table'
           call fcerr(context)
           goto 999
        endif
           
c      Print out any error information about accessing the files
        if (status.ne.0)then
           call fcerrm(status)
        endif
        
c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the "extnum" and if it is less than 2 we
c      force it to 2. 
        if (iextnum.lt.2) extnum=2
c        print*,'assigned extnum to ',extnum

c      Since we are finished copying the primary header we can 
c      simply go to the second extension in the file (or iextnum)
c      to read all pertinent processing information. xtend tells
c      the type of information within this extension - in our case it
c      should be in binary form.
        call ftmahd(iunit,extnum,xtend,status)
c        print*,'iunit,extnum,xtend,status',iunit,extnum,xtend,status
        if (status.ne.0)then
           call fcerrm(status)
        endif
           
        if(xtend.ne.2) call fcerr ('Extension is not binary')
           
c      Read the information about how the data is stored - see the
c      fitsio.for file for a description of this call. 
        call ftghbn(iunit,nb,nrows,nfield,ttype,
     &          tform,tunit,extnam,pcount,status)

c      Print out any error information about accessing the files
        if (status.ne.0)then
           call fcerrm(status)
        endif

c        print*,'nrows, nfield,pcount is',nrows,nfield,pcount
        
c      Store useful information for the processing of data
           
c      Store the information about which column the information we
c      want is stored in (coltime and colcnt)
        
        do 20 j=1,nfield
           if(.not.sensecase)call ftupch(ttype(j))
           if(ttype(j).eq.timecol)then
              coltime=j
           endif
           if(ttype(j).eq.columns)then
              colcnt=j
           endif
c           print*,'j type and unit are ',j,ttype(j),tunit(j),
c     &          coltime,colcnt
20      continue

c      Find out the column number that is associated with the
c      time column (timecol) and counts column (columns) in the next
c      two subroutine calls
        
        call ftgcno(iunit,exact,timecol,colpost,ftstatus)            
        call ftgcno(iunit,exact,columns,colpos,ftstatus)

c      Print out any error information about accessing the files
        if (ftstatus.ne.0)then
           call fcerrm(ftstatus)
           print*,'ftstatus is ',ftstatus
        endif

c      Parse the TFORM value to find out the type of data storage
c      and the number of 8 byte values that there are. The type
c      is returned in "dtype" where 11 = 'X', and the rcount value
c      is the number of bits/8. We have to parse the proper TDDES
c      field to find out where the bits which tell which channel detected
c      the photon resides at a later point.
            call ftbnfm(tform(colpos),dtype,rcount,width,status)

c            print*,'tform(colpos),dtype,rcount,width,colpos'
c            print*,tform(colpos),dtype,rcount,width,colpos

c      Print out any error information about accessing the files
            if (status.ne.0)then
               call fcerrm(status)
            endif

c      We need to get the information from the Data Descriptor fields.
            
c      Create the 6 character string to find the TDDES
           keyword(1:5)='TDDES'
           keyword(6:6)=' '
c           print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in keyval
           status=0
           call ftgkys(iunit,keyword,tddes(1),comm,status)
c           print*,'tddes is ',tddes(1)           
           status=0
        
        do 72 iword=1,nfield

c      iword2 will take on the value of the column which contains
c      this information - i.e. 2 --> NFIELD (number of the column
c      being looked at)
           iword2=colpos
        
c      Create the 6 character string to find the TDDES
           keyword(1:5)='TDDES'
           keyword(6:6)=cnum(iword+1)
c           print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in tddes(iword+1)
           status=0
           call ftgkys(iunit,keyword,tddes(iword+1),comm,status)
c           print*,'tddes is ',iword,tddes(iword+1)
        
c      Create the 6 character string to find the TPACK
           keyword(1:5)='TPACK'
           keyword(6:6)=cnum(iword+1)
c           print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in tpack(iword+1)
           status=0
           call ftgkys(iunit,keyword,tpack(iword+1),comm,status)
c           print*,'tpack is ',iword,tpack(iword+1)
              
72      continue

c----------------------------------------------------------------------        
c      Now that we have the information stored in the DDL we have to
c      make use of it to determine where the information that we want
c      is stored. 
        j=0
        tmpval=tddes(colcnt+1)
        itotal=0

c      The purpose of the following loop is to skip all of the
c      telemetry information which doesn't interest us. 
        do 73 jsize=1,240
           j=j+1
           if(tmpval(j:j+1).eq.'>>')foundchn=.TRUE.
           if(foundchn.and.tmpval(j:j).eq.'C')then
              j=999
           endif
           if(foundchn.and.tmpval(j:j).eq.'{'.and.j.ne.999)then
              istart=j
           endif
           if(foundchn.and.tmpval(j:j).eq.'}'.and.j.ne.999)then
              istop=j
              outval=tmpval(istart+1:istop-1)
              call ftc2i(outval,outvali,status)
c              print*,'outval and outvali is ',outval,outvali
              iskip=iskip+outvali
           endif
           
73      continue

c        print*,'iskip is ',iskip
        
c      We have stored the number of element to skip in ISKIP.
c----------------------------------------------------------------------        
        status=0
        
c**********************************************************************
c
c      Now that we have read in all of the information necessary for
c      actually processing the information lets get all of the rest of
c      the header information necessary and print that information out

c       Update the CHKSUM keywords in the output file.
        call ftpcks(ounit,status)
        
c      Create another Header Data Unit this is independent of chosen
c      options.
        call ftcrhd(ounit,status)

c      Since the number of rows to be written doesn't change we simply
c      set the number of output rows equal to the number of input rows.
        nchan=nrows

c      We are writing out a standard 2D binaray file and will store
c      the proper values.
        naxis=2

c      If we gave an alternative for the output Time column then use it.
        if(outimecol.eq.' ')then
           otype(1)=ttype(coltime)
        else
           otype(1)=outimecol
        endif
        
        otunit(1)=tunit(coltime)
        otform(1)=tform(coltime)

c      If we gave an alternative for the output column then use it.
        if(outcol.eq.' ')then
           otype(2)=ttype(colcnt)
        else
           otype(2)=outcol
        endif
        
        otunit(2)='channel'
        otform(2)='1I'
        
c      Write out the header information standard keywords for
c      writing a binary table extension.
        call ftphbn(ounit,nchan,naxis,otype,otform,otunit,
     &       extnam,pcount,status)
           
c      Now we will read and write out the rest of the information
c      that is contained within this extension. Since some of the
c      KEYWORDS searched for may not exist we will have to reset status
c      after each attempt. However if the KEYWORD is found we will print
c      that information out to the output file that we are creating.

        do 1010 i=1,ikey2
           call ftgkys(iunit,keyword2(i),comm,context,status)

           if(keyword2(i).eq.'CREATOR')comm=
     &          'SEEXPD - Version 0.1'
           if(keyword2(i).eq.'DATE')then
              call ftpdat(ounit,status)
           else
              call ftpkys(ounit,keyword2(i),comm,context,status)
           endif
           
           status=0
1010    continue

        do 1011 i=1,ikey3
           call ftgkyj(iunit,keyword3(i),ikeyval,context,status)
           call ftpkyj(ounit,keyword3(i),ikeyval,context,status)
           status=0
1011    continue

        do 1012 i=1,ikey4,3
           call ftgkye(iunit,keyword4(i),keyval,context,status)
           call ftpkye(ounit,keyword4(i),keyval,8,context,status)

           call ftgkye(iunit,keyword4(i+1),keyval,context,status)
           call ftpkye(ounit,keyword4(i+1),keyval,8,context,status)

           call ftgkye(iunit,keyword4(i+2),keyval,context,status)
           call ftpkyf(ounit,keyword4(i+2),keyval,2,context,status)
           status=0
1012    continue

c      Define the structure of the binary table date unit so that
c      we can actually write out the informtion.

        call ftbdef(ounit,2,otform,pcount,nchan,status)
        
        status=0
        
c----------------------------------------------------------------------

c      Loop over all of the rows that are stored in each file
c     
        do 50 lrow=1,nrows

           fbit=1
           nulvald=0.0d0

c      Increment value used in performing the read of the array.
           incr=1

c      What to do with null values? And what is the increment?
           nultyp=0
           incr=1
           
c      What to do with null values?
           nulval=0

c      Read in the timestamp for each row - there is usually only
c      one timestamp per row.
c          call ftgcld(iunit,colpost,lrow,1,1,incr,
c    &          nultyp,nulvald,timearay,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
           call ftgcvd(iunit,colpost,lrow,1,1,nulvald,
     &          timearay,anynul,status)

c      Print out any error information about accessing the files
           if (status.ne.0)then
              call fcerrm(status)
           endif

c      Read in the telemetry string as if it were a string of logicals
c      of a certain length. We will make use of ISKIP (the number of elements
c      to skip to get to the beginning of the proper place) to
c      determine which channel received the photon.
           call ftgcx(iunit,colpos,lrow,fbit,rcount*8,array,status)

c      Print out any error information about accessing the files
           if (status.ne.0)then
              call fcerrm(status)
           endif
c----------------------------------------------------------------------
c      This is where we will determine the channel which received the
c      photon. That value will be stored in ITOTAL. ISIZE maintains 
c      the value of each bit as we move through the logical. 
        itotal=0
        isize=128

c      Since we are dealing with 8 bit integers we will have to move
c      through 8 elements within the logical array. If that element
c      within the logical is true then we increment ITOTAL by the value
c      within the place holder ISIZE. Then we divide ISIZE by 2 and proceed
c      to the next element
        do 74 jsize=1,8
           if(array(iskip+jsize))itotal=itotal+isize
           isize=isize/2
74      continue

c        print*,'itotal is ',itotal
c      ITOTAL now contains the channel which detected the photon...
c      Since ITOTAL takes on the values from 0 --> 255 
c----------------------------------------------------------------------        
c      Since we now have all of the information necessary we will write
c      out the timestamp and channel.

        call ftpcld(ounit,1,lrow,1,1,timearay,status)
        call ftpcli(ounit,2,lrow,1,1,itotal,status)

50    continue



c       Update the CHKSUM keyword in the output file created.
      call ftpcks(ounit,status)
      

c      Close up the input and output file.
      call ftclos(ounit,status)
      call ftclos(iunit,status)

c      Print out any error information about accessing the files
      if (status.ne.0)then
        call fcerrm(status)
      endif

999   continue
      return
      end

