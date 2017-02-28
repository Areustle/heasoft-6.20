c******************************************************************************
C SELECTOR TASK:
C      saexpd
C
C FILE:
C      saexpd.f 
C
C DESCRIPTION: 
C      Read in a XTE/SA file 
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
        subroutine saexpd
c        implicit none
	character(160) infile, outfile
	character(80) timecol, columns, element,
     &       outimecol, outcol

        integer status
        logical sensecase,colexpd

	character(40) taskname
	common /task/ taskname

	taskname = 'saexpd_3.3'
	infile = ' '
	outfile = ' '
	timecol = ' '
	columns = ' '
        colexpd=.FALSE.
        element = ' '
	outimecol = ' '
	outcol = ' '


C     get the parameters from the par file
        call gcolv(infile,outfile,timecol,columns,
     &       colexpd,element,outimecol,outcol,sensecase,status)

c        print*,'Values are infile,outfile,timecol,columns,
c     &       outcol,outimecol,sensecase,status'
c        print*,infile,outfile,timecol,columns,
cc     &       outcol,outimecol,sensecase,status

        if (status .ne. 0) goto 999

C      Read inthe FITS file and write out the light curve file.

        call gcolvfil(infile,outfile,timecol,columns,
     &       colexpd,element,outimecol,outcol,sensecase,status)

999     return
        end

c**********************************************************************
c
c SUBROUTINE:
C      gcolv
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
C      gcolv uses F77/VOS like calls to read parameters from the .par file
C      
c USAGE:
c        call gcolv(infile,outfile,timecol,columns,
c     &       colexpd,element,outimecol,outcol,status)
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

        subroutine gcolv(infile,outfile,timecol,columns,
     &       colexpd,element,outimecol,outcol,sensecase,status)

c        implicit none
	character*(*) infile, outfile
	character*(*) timecol, columns, element,
     &       outimecol, outcol

        character(80) context, operation
	integer status
        logical sensecase,colexpd
        
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

C  get the operation
	call uclgst('operation',operation,status)
        if(operation.eq.' ')then
           context = 'Column header blank! Must have value'
           call fcecho(context)
           status=1
        endif

	if (status .ne. 0) then
           context = 'could not get OPERATION parameter'
           call fcerr(context)
           goto 999
	endif

        call ftupch(operation)
        if(operation.eq.'COLEXPD')then
           colexpd=.TRUE.
        elseif(operation.eq.'VECEXPD') then
           colexpd=.FALSE.
        elseif(operation.ne.'COLEXPD'.or.operation.ne.'VECEXPD') then
           context='ERROR - only VECEXPD or COLEXPD are supported'
           call fcerr(context)
           context='Change value of OPERATION to a supported function'
           call fcerr(context)
           status=1
           goto 999
        endif
        
C  get the element
	call uclgst('element',element,status)
        if(element.eq.' ')then
           context = 'Column header blank! Must have value'
           call fcecho(context)
           status=1
        endif

	if (status .ne. 0) then
           context = 'could not get ELEMENT parameter'
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
           call ftupch(element)
        endif
c        print*,'timecol and columns is...', timecol,columns,
c     &       operation,colexpd,element
        
999	continue

	if (status .ne. 0)  call fcerrm(status)

	return
	end

c****************************************************************************
c
c      gcolvfil
c
c DESCRIPTION:
c      Gets the information necessary to begin processing files.
c      We will need to acquire all of the information about how the
c      data is stored in the FITS files. This routine gets all of the
c      information to process the file, and uses it in the manipulation
c      of the information and creates the output file and writes all
c      of the processed information to that file.

C INPUT PARAMETERS
C      iunit   i   - Fortran i/o unit number
c      outfile c   - Outfile
c      operation c - Operation to perform; either COLEXPD or VECEXPD
c      timecol c   - Character string used for TIME header.
c      columns c   - Character string used for count header.
C      outimecol c - column name for bin centers      
C      outcol    c - column name for binned parameter(s) values
C      sensecase l - whether values should be case sensitive
c

c      
C MAJOR LOCAL VARIABLES:
C      ncols   i  number of columns in the table
C      nrows   i  number of rows in the table
C      nfield  i  number of fields in the table
C      ttype   c  name of each field (array)
C      tform   c  Fortran-77 format of each field (array)
C      tunit   c  units of each field (array)
C      extnam  c  name of table (optional)
C      status  i  returned error status (0=ok)
c      clbd    s  string giving lower boundary for each column (2D array)
c      cubd    s  string giving upper boundary for each column (2D array)
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
c      ftgtdm    - parse teh TDIMnnn keywork to get dimensionality of col.
c      ftgcno    - determine col. num. corresponding to column name
c      ftgkys    - get string value associated with keyword from header.
c      ftgcli    - read in an integer*2 array of elements
c      ftclos    - close up the file that was read in.
c      
c**********************************************************************
      
        subroutine gcolvfil(infile,outfile,timecol,columns,
     &       colexpd,element,outimecol,outcol,sensecase,status)        
c        implicit none

c      Define all of the common parameters used in the arrays.
        logical anynul,flgval,sensecase,exact,timeflg,
     &       fstcolt,colexpd,onedima
        integer ikey2,ikey3,ikey4,isml
        integer nf,nb,ne,siz
        integer nrows,nfield,pcount,status,
     &       ikeyval,i,j,irun,ifelem

        parameter (ikey2=19)
        parameter (ikey3=4)
        parameter (ikey4=6)
        parameter (isml = 8)
        parameter (nb = 257)

c      The following two parameters determine the size of the 1D array
c      that will "hold" all of the information after it is read into
c      temparay. So the product of nf*siz determines the size of the array
c      used to store the information, and ne determines the maximum number
c      of elements that can be read in at one pass. If you have memory
c      problems then try decreasing nf and ne, or if you want more speed
c      than increase the values of these three parameters. The code is
c      constructed in such a way so as to make the most efficient use of
c      whatever amount of memory you give it.
        
        parameter (nf = 256)
        parameter (siz = 50)
        parameter (ne = 1280)

        integer*2 array(nf*siz),temparay(ne)
        integer block,coltime,colcnt,
     &       naxis,naxes(nb),colpos,onaxes(nb),
     &       lrow,inchan,colpost,extnum,ichn,ielemm,ielemn,
     &       ounit,iunit,iextnum,oextnum,xtend,naxist,naxest(nb)
        integer nulval,lfelem,incr,icol,
     &       nultyp,iword,iword2,nchan,iperiod,
     &       ielem,ileft,isize,jsize,lelem,ftstatus
        real cdlt(isml),crsv(isml),timedel,keyval
        double precision timearay(9),
     &       nulvald,fasthold,
     &       tint

        character*(*) infile,outfile,timecol,columns,
     &       element, outimecol,outcol
        character(1) cnum(10)
        character(6) keyword
        character(8) keyword2(ikey2),keyword3(ikey3),keyword4(ikey4)
        character(20) outval
        character(80) ttype(nb),tform(nb),tunit(nb),extnam,
     &       filein,fileout,context,otype(nb),otform(nb),otunit(nb),
     &       outvalf
        character(80) clbd(isml),cubd(isml)
     &       ,ctyp(isml),tddes(nb),tpack(nb),stval,
     &       cuni(isml),crsf(isml),comm,coltemp
        
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

c      Assign a unit file number to the output file.
        ounit=11
        
c      Assign a unit file number used in inputting file.
        iunit=10

        status=0
        icol=0
        timeflg=.FALSE.
        fstcolt=.FALSE.
        exact=.FALSE.
        onedima=.FALSE.
        coltemp=' '
        if(sensecase)exact=.TRUE.
c        print*,'sensecase is ',sensecase
c shut up compiler warnings
        ielemm=0
        ielemn=0
        
c**********************************************************************
c      initialize variables:
        do 12 j=1,999
           otform(j)=' '
           otype(j)=' '
           otunit(j)=' '
12      continue

        do 13 iword=1,isml
           if(columns(iword:iword).eq.' '.and.icol.eq.0)icol=iword
           ctyp(iword)='$'
           clbd(iword)='$'
           cubd(iword)='$'
           cuni(iword)='$'
           crsf(iword)='$'
           cdlt(iword)=0.0e0
           crsv(iword)=0.0e0
13      continue

        if(icol.gt.5)icol=5
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
c      Open and initialize output FITS file...
c        call ftinit(ounit,fileout,block,status)
        call ffinit(ounit,fileout,status)
c        print*,'initialialized outfile ',ounit,fileout,status
           
	if (status .ne. 0) then
           context = 'Output file exists. Cannot continue'
           call fcerrm(status)
           call fcerr(context)
           goto 999
        endif

c        print*,'About to call copy header command'
c      Copy the primary header from the input file to the output file
        ftstatus=0
        call ftcopy(iunit,ounit,0,ftstatus)
c        print*,'Copied header',ftstatus
        
        if(ftstatus.ne.0)then
           context='Unable to write primary header '
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

c      Get the information pertaining to the time length.
        call ftgkye(iunit,'TIMEDEL',timedel,context,status)
        if (status.ne.0)then
           call fcerrm(status)
        endif

c        print*,'delta is ',timedel,context,status
           
c      Read the information about how the data is stored - see the
c      fitsio.for file for a description of this call. 
        call ftghbn(iunit,nb,nrows,nfield,ttype,
     &          tform,tunit,extnam,pcount,status)

c      Print out any error information about accessing the files
        if (status.ne.0)then
           call fcerrm(status)
        endif

c        print*,'nrows is',nrows,nfield,pcount
        
c      Store the information about which column the information we
c      want is stored in (coltime and colcnt) and the units for
c      that information (nunit).
        
        do 20 j=1,nfield
           if(.not.sensecase)call ftupch(ttype(j))
           if(ttype(j).eq.timecol)then
              if(outimecol.ne.' ')ttype(j)=outimecol
              coltime=j
           endif
           if(ttype(j).eq.columns)then
              if(outcol.ne.' ')ttype(j)=outcol
              colcnt=j
           endif
c           print*,'type and unit are ',ttype(j),tunit(j),
c     &          coltime,colcnt
20      continue
c      

c      Parce the TDIMnnn keyword to get the dimensionality of the time
c      column (coltime), and store that info in naxist, and naxest. 
        call ftgtdm(iunit,coltime,nb,naxist,naxest,status)

c      Parce the TDIMnnn keywork to get the dimensionality of the counts
c      column (colcnt) and store that info in naxis, and naxes.
        call ftgtdm(iunit,colcnt,nb,naxis,naxes,status)

        if(naxis.gt.2)then
           context='ERROR: This code only supports 1D and 2D arrays'
           call fcerr(context)
           return
        endif
        if(naxis.eq.1)onedima=.TRUE.

c      Since naxes contains the dimensioned size of the array according
c      to (time,channels) we can access the time interval of increase by
c      dividing timedel by naxes(1) and store this interval in TINT. This
c      should agree with the stored values.
        tint = dble(timedel)/(float(naxes(1)))

c        print*,'coltime,colcnt and tint is ',coltime,colcnt,tint

c      Print out any error information about accessing the files
        if (status.ne.0)then
           call fcerrm(status)
        endif

c      Find out the column number that is associated with the
c      time column (timecol) and counts column (columns) in the next
c      two subroutine calls

        
        call ftgcno(iunit,exact,timecol,colpost,ftstatus)            
        call ftgcno(iunit,exact,columns,colpos,ftstatus)

c      Print out any error information about accessing the files
        if (ftstatus.ne.0)then
           call fcerrm(ftstatus)
c           print*,'ftstatus is ',ftstatus
        endif

c        print*,'columns timecol, colpos colpost is '
c     &       ,columns,timecol,colpos,colpost

c
c      Since we are reworking the structure of the file it is necessary
c      that we record all of the information about the upper and lower
c      bounds as well as all of the information about the columns in
c      the array. We have to rearrange this information so that the
c      result again agrees with the definition of our SA data. 
c      
c      Since modification to fitsio routines would be required to
c      generalize the following and the final form of the keywords
c      may/will change, I opted to create the exact string that will
c      exist in SA data based upon the returns from the TDIM calls.

c      iword will take on the value of the dimension of the array.
c      i.e. we are dealing with 2D to ND arrays where NAXIS gives the
c      dimension. This is usually written as (X,Y) so in this instance
c      NAXIS = 2. For SA data we know that the first column is time so
c      all we have to deal with are the other elements. Each of these
c      elements have an upper and lower bound for the detector. 
        do 72 iword=1,naxis

c      iword2 will take on the value of the column which contains
c      this information - i.e. 2 --> NFIELD (number of the column
c      being looked at)
           iword2=colpos

c           print*,'iword and iword2',iword,iword2
           status=0
        
c      Create the 6 character string to find the TYPE
           keyword(1:1)=cnum(iword+1)
           keyword(2:5)='CTYP'
           keyword(6:6)=cnum(iword2+1)
c           print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in ctyp(iword)
           status=0
           call ftgkys(iunit,keyword,ctyp(iword),comm,status)
c              print*,'ctyp is ',iword,iword2,ctyp(iword)

              if(.not.sensecase)then
                 call ftupch(ctyp(iword))
              endif

c      If one of the elements of the 2D array is DELTA_TIME than we will
c      expand the number of timestamps so that no data is lost. 
              if(ctyp(1).eq.'DELTA_TIME')then
                 timeflg=.TRUE.
                 fstcolt=.TRUE.
                 ielemm=1
              else if(ctyp(2).eq.'DELTA_TIME')then
                 timeflg=.TRUE.
                 ielemm=2
              endif

c      Since the 1D case is stored differently we have to deal with it
c      as an exception. 
              if(onedima.and.fstcolt)then
                 ielemn=2
                 ttype(ielemn)=element
                 naxes(ielemn)=1
                 if(colexpd)then
                    colexpd=.FALSE.
                    comm='changing option COLEXPD to VECEXPD'
                 endif
              elseif(onedima.and.(.not.fstcolt))then
                 ielemm=1
                 ttype(ielemm)='TIME'
                 naxes(ielemm)=1
                 if(.not.colexpd)then
                    colexpd=.TRUE.
                    comm='changing option VECEXPD to VECEXPD'
                 endif
                 
              endif
              
           if(ctyp(iword).eq.element)ielemn=iword
c              print*,'ctyp(iword) is ',iword,ctyp(iword)
c              print*,'iword,timeflg,fstcolt,ielemm,ielemn',iword,
c     &          timeflg,fstcolt,
c     &          ielemm,ielemn
c              print*,'element is ',element

c      Create the 6 character string to find the CUNI
              keyword(1:1)=cnum(iword+1)
              keyword(2:5)='CUNI'
              keyword(6:6)=cnum(iword2+1)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in cuni
              status=0
              call ftgkys(iunit,keyword,cuni(iword),comm,status)
c              print*,'cuni is ',cuni(iword)

c      Create the 6 character string to find the CDLT
              keyword(1:1)=cnum(iword+1)
              keyword(2:5)='CDLT'
              keyword(6:6)=cnum(iword2+1)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in cd
              status=0
              call ftgkye(iunit,keyword,cdlt(iword),comm,status)
c              print*,'cd is ',cdlt(iword)
              
c      Create the 6 character string to find the CRSF
              keyword(1:1)=cnum(iword+1)
              keyword(2:5)='CRSF'
              keyword(6:6)=cnum(iword2+1)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in crsf(i
              status=0
              call ftgkys(iunit,keyword,crsf(iword),comm,status)
c              print*,'crsf is ',crsf(iword)
               
c      Create the 6 character string to find the CRSV
              keyword(1:1)=cnum(iword+1)
              keyword(2:5)='CRSV'
              keyword(6:6)=cnum(iword2+1)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in crsv
              status=0
              call ftgkye(iunit,keyword,crsv(iword),comm,status)
c              print*,'crsv is ',crsv(iword)
              
c      Create the 6 character string to find the LOWER BOUND
              keyword(1:1)=cnum(iword+1)
              keyword(2:5)='CLBD'
              keyword(6:6)=cnum(iword2+1)
c              print*,'keyword is ',keyword
c      Search the HEADER to find this keyword and return the
c      character string found in clbd(iword)
              status=0
              call ftgkys(iunit,keyword,
     &             clbd(iword),comm,status)
              
c              print*,'clbd is ',clbd(iword)

c      Create the 6 character string to find the UPPER BOUND 
              keyword(1:1)=cnum(iword+1)
              keyword(2:5)='CUBD'
              keyword(6:6)=cnum(iword2+1)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in clbd(iword)
              status=0
              call ftgkys(iunit,keyword,cubd(iword),comm,status)
c              print*,'cubd is ',cubd(iword)

71         continue
           
72      continue

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

c         print*,'Created second data header',status

c      If one of the elements is TIME than we have to expand the number
c      of timestamps. So we will write out a new number of timestamps
c      which is the (number of elements of time)*(the number of timestamps)
        if(timeflg)then
           nchan=naxes(ielemm)*nrows
        else

c      If not we will only deal with the timestamps that are stored in the
c      file.
           nchan=nrows
        endif

c        print*,'naxes(ielemn) is ',naxes(ielemn),naxes(ielemm)

        onaxes(1)=naxes(ielemn)
        onaxes(2)=1        
        if(colexpd)then
c           otype(2)=ttype(ielemn)
c           otunit(2)=tunit(ielemn)

           otype(2)=ttype(colcnt)
           otunit(2)=tunit(colcnt)

           otform(1)='1D'
           otype(1)='TIME'
           otunit(1)='s'

c      If alternative column titles were selected then we much use those.
           if(outcol.ne.' ')otype(2)=outcol
           if(outimecol.ne.' ')otype(1)=outimecol
           
c      Form the TFORM keyword such that it is correct for the
c      new length of this element.
           j=0

           if(naxes(ielemn).gt.1)then
              call fti2c(naxes(ielemn),outval,status)
              do 1001 i=1,20
                 if(outval(i:i).ne.' ')then
                    j=j+1
                    outvalf(j:j)=outval(i:i)
                 endif
1001          continue

              j=j+1
              outvalf(j:j)='I'
              
              do 1002 i=j+1,80
                 outvalf(i:i)=' '
1002          continue

              otform(2)=outvalf
           else
              otform(2)='1I'
           endif
           
c           print*,'otform is ',otform(1),otform(2)
        
c      Write out the header information standard keywords for
c      writing a binary table extension.
           call ftphbn(ounit,nchan,naxis,otype,otform,otunit,
     &          extnam,pcount,status)

c      This else statement considers the condition where we are NOT
c      doing a column expansion but instead are going to be doing a
c      vector expansion. In this case the final output will NOT be in
c      XTE/SA format but a standard FITS format.
        else
           do 110 j=1,naxes(ielemn)

c      If an alternative outcol title was selected than we will use that.
              if(outcol.eq.' ')then
                 coltemp=columns
              else
                 coltemp=outcol
              endif
              
              if(j.lt.10)then
                 coltemp(icol:icol)=cnum(j+1)
              elseif (j.lt.100)then
                 coltemp(icol:icol)=cnum(j/10+1)
                 coltemp(icol+1:icol+1)=cnum(mod(j,10)+1)
              elseif (j.lt.1000)then
                 coltemp(icol:icol)=cnum(j/100+1)
                 coltemp(icol+1:icol+1)=cnum((mod(j,100)/10)+1)
                 coltemp(icol+2:icol+2)=cnum(mod(mod(j,100),10)+1)
              elseif (j.ge.1000)then
                 context='Truncating to fields 999 elements'
                 call fcerr(context)
                 naxes(ielemn)=999
                 goto 110
              endif

c              print*,'coltemp is',coltemp
              otype(j+1)=coltemp
              otunit(j+1)=tunit(colcnt)
              otform(j+1)='1I'
110        continue

           otype(1)=ttype(coltime)
           otunit(1)=tunit(coltime)
           otform(1)='1D'

c           print*,'ttype time',colcnt,ttype(colcnt),tunit(colcnt)
c           print*,'ttype count',coltime,ttype(coltime),tunit(coltime) 

c      If an alternative output time column title was selected we will use
c      that instead of the input Time stamp column
           if(outimecol.ne.' ')otype(1)=outimecol
           
           if(.not.colexpd)extnam='SAEXPD'
           call ftphbn(ounit,nchan,naxes(ielemn)+1,otype,otform,
     &          otunit,extnam,pcount,status)

c           print*,'ounit,nchan,naxes(ielemn)+1,status'
c           print*,ounit,nchan,naxes(ielemn)+1,status
           
        endif
           
c      Now we will read and write out the rest of the information
c      that is contained within this extension. Since some of the
c      KEYWORDS searched for may not exist we will have to reset status
c      after each attempt. However if the KEYWORD is found we will print
c      that information out to the output file that we are creating.

        do 1010 i=1,ikey2
           call ftgkys(iunit,keyword2(i),comm,context,status)
           if(keyword2(i).eq.'CREATOR')comm=
     &          'SAEXPD - Version 0.1'
           if(keyword2(i).eq.'DATE')then
              call ftpdat(ounit,status)
           else
              call ftpkys(ounit,keyword2(i),comm,context,status)
           endif
           
c           print*,keyword2(i),comm
           status=0
1010    continue

        do 1011 i=1,ikey3
           call ftgkyj(iunit,keyword3(i),ikeyval,context,status)
           call ftpkyj(ounit,keyword3(i),ikeyval,context,status)
c           print*,keyword3(i),ikeyval
           status=0
1011    continue

        do 1012 i=1,ikey4,3
c           print*,'about to do real reads',i,keyword4(i)
           call ftgkye(iunit,keyword4(i),keyval,context,status)
           call ftpkye(ounit,keyword4(i),keyval,8,context,status)

           call ftgkye(iunit,keyword4(i+1),keyval,context,status)
           call ftpkye(ounit,keyword4(i+1),keyval,8,context,status)

           call ftgkye(iunit,keyword4(i+2),keyval,context,status)
           call ftpkyf(ounit,keyword4(i+2),keyval,2,context,status)
c           print*,'in key4',keyword4(i+2),keyval,status
           status=0
1012    continue

        if(colexpd)then
        
           status=0
           call ftgkys(iunit,'TDDMAC1',stval,comm,status)
           call ftpkys(ounit,'TDDMAC1',stval,comm,status)
           status=0
c           print*,'tddes is ',iword2,tddes(1)
        
c      Create the 6 character string to find the TDDES
           keyword(1:5)='TDDES'
           keyword(6:6)=' '
c           print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in keyval
           status=0
           call ftgkys(iunit,keyword,tddes(1),comm,status)
           call ftpkys(ounit,keyword,tddes(1),comm,status)
           status=0
c        print*,'tddes is ',iword2,tddes(1)
              
c      Create the 6 character string to find the TDDES
           keyword(1:5)='TDDES'
           keyword(6:6)=cnum(colpos+1)
c        print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in tddes(iword2)
           status=0
           call ftgkys(iunit,keyword,tddes(2),comm,status)
           call ftpkys(ounit,'TDDES2',tddes(2),comm,status)
c        print*,'tddes is ',iword2,tddes(2)

           if(naxes(colpos).gt.1)then
              call fti2c(naxes(colpos),outval,status)
              outvalf(1:2)='1,'
              j=0
              do 1021 i=1,20
                 if(outval(i:i).ne.' ')then
                    j=j+1
                    outvalf(j:j)=outval(i:i)
                 endif
1021          continue
              j=j+1
              outvalf(j:j+1)=',1'
              do 1022 i=j+2,80
                 outvalf(i:i)=' '
1022          continue
              tpack(2)=outvalf
           else
              tpack(2)='1,1'
           endif

c      Search the HEADER to find this keyword and return the
c      character string found in tpack(iword2)
           status=0
           call ftpkys(ounit,'TPACK2',tpack(2),comm,status)
c        print*,'tpack is ',iword2,tpack(2)
        
c      Write out the TDIM information
           call ftptdm(ounit,2,2,onaxes,status)

c----------------------------------------------------------------------
c----------------------------------------------------------------------
c      Now that we have read in all of the information necessary we
c      will write out the appropriate keywords for the new format that
c      this file will be written out in

c        print*,'***********************************************'
        
           if(naxis.eq.2)then
              i=3

              do 73 iword=1,naxis
                 i=i-1
c      iword2 will take on the value of the column which contains
c      this information - i.e. 2 --> NFIELD (number of the column
c      being looked at)
                 iword2=colpos

                 status=0
        
c      Create the 6 character string to find the TYPE
                 keyword(1:1)=cnum(iword+1)
                 keyword(2:5)='CTYP'
                 keyword(6:6)=cnum(3)
c      print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in ctyp(iword,iwor2)
                 status=0
                 if(ctyp(i).ne.'$')
     &                call ftpkys(ounit,keyword,ctyp(i)
     &                ,comm,status)
c              print*,'ctyp is ,i,iword,iword2 '
c     &             ,i,iword,iword2,ctyp(i)
               
c      Create the 6 character string to find the CUNI
                 keyword(1:1)=cnum(iword+1)
                 keyword(2:5)='CUNI'
                 keyword(6:6)=cnum(3)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in cuni
                 status=0
                 if(cuni(i).ne.'$')
     &                call ftpkys(ounit,keyword,cuni(i)
     &                ,comm,status)
c              print*,'cuni is ',cuni(i)

c      Create the 6 character string to find the CDLT
                 keyword(1:1)=cnum(iword+1)
                 keyword(2:5)='CDLT'
                 keyword(6:6)=cnum(3)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in cd
                 status=0
                 if(cdlt(i).ne.0.0.and.iword.ne.2)
     &                call ftpkyf(ounit,keyword,cdlt(i),
     &                8,comm,status)
c              print*,'cd is ',cdlt1(i)
              
c      Create the 6 character string to find the CRSF
                 keyword(1:1)=cnum(iword+1)
                 keyword(2:5)='CRSF'
                 keyword(6:6)=cnum(3)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in crsf(i)
                 status=0
                 if(crsf(i).ne.'$')
     &                call ftpkys(ounit,keyword,crsf(i),
     &                comm,status)
c              print*,'crsf is ',crsf(i)
               
c      Create the 6 character string to find the CRSV
                 keyword(1:1)=cnum(iword+1)
                 keyword(2:5)='CRSV'
                 keyword(6:6)=cnum(3)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in crsv
                 status=0
                 if(crsv(i).ne.0.0)
     &                call ftpkyf(ounit,keyword,crsv(i),
     &                8,comm,status)
c              print*,'crsv is ',crsv(i)
              
c      Create the 6 character string to find the LOWER BOUND
                 keyword(1:1)=cnum(iword+1)
                 keyword(2:5)='CLBD'
                 keyword(6:6)=cnum(3)
c              print*,'keyword is ',keyword
c      Search the HEADER to find this keyword and return the
c      character string found in clbd(i)
                 status=0
                 if(clbd(i).ne.'$')
     &                call ftpkys(ounit,keyword,
     &                clbd(i),comm,status)
              
c              print*,'clbd is ',clbd(i)

c      Create the 6 character string to find the UPPER BOUND 
                 keyword(1:1)=cnum(iword+1)
                 keyword(2:5)='CUBD'
                 keyword(6:6)=cnum(3)
c              print*,'keyword is ',keyword

c      Search the HEADER to find this keyword and return the
c      character string found in clbd(i)
                 status=0
                 if(cubd(i).ne.'$')
     &                call ftpkys(ounit,keyword,cubd(i)
     &                ,comm,status)
c              print*,'cubd is ',cubd(i)

74               continue
           
73            continue

           endif

        endif

c      Define the structure of the binary table date unit so that
c      we can actually write out the informtion.
c        print*,'About to set up data form'
        if(colexpd)then
           call ftbdef(ounit,2,otform,pcount,nchan,status)
        else
           call ftbdef(ounit,naxes(ielemn)+1,otform,
     &          pcount,nchan,status)
        endif
        
c        print*,'Set up data form',naxes(ielemm)+1,otform
c        print*,'***********status is ',status

        status=0
        
c        call ftclos(ounit,status)
c        return

        
c------ ----------------------------------------------------------------

c      Loop over all of the rows that are stored in each file
c     
        do 50 lrow=1,nrows

           nulvald=0.0d0

c      Increment value used in performing the read of the array.
           incr=1

c      What to do with null values?
           nultyp=0

c      Reinitialize element number and channel number for each row.
c      Each row is a different channel.
           iperiod=0
           lfelem=0
           incr=1
           
c      What to do with null values?
           nulval=0

c      Read in all of the timestamp for each row - there is usually only
c      one timestamp per row.
c          call ftgcld(iunit,colpost,lrow,1,1,incr,
c    &          nultyp,nulvald,timearay,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
           call ftgcvd(iunit,colpost,lrow,1,1,nulvald,
     &          timearay,anynul,status)

c           print*,'**Time array contains and status***',
c     &          timearay(1),status

c      Print out any error information about accessing the files
           if(status.ne.0)then
              call fcerrm(status)
           endif

c      The following section is only pertinent if ONE of the elements
c      in the 2D array is TIME... 

c           print*,'timeflg is and onaxes is',timeflg,onaxes(1)
           if(timeflg)then

c**********************************************************************
c      Now that we have read in all of the information which is
c      necessary to process this file we have to rearrange it into a
c      form which is in agreement with XTE/SA data 

c      In order to make this code as efficient as possible on each
c      access to the input file we have to do some manipulation of the
c      information to determine the optimum size to read in for each
c      access of the input file. Since we have allocated space for a
c      maximum of nf*siz elements we will optimize the reads to
c      the input file for maximum efficiency.

c      IELEM = the number of elements to read on each access to the file
           ielem=(nf*siz)/onaxes(1)
c           print*,'ielem is ',ielem

c      IRUN = the number of loops in which we read IELEM elemnts
           irun=naxes(ielemm)/ielem
c           print*,'irun is ',irun
           
c      ILEFT = the number of elements to read on the last access to the file
           ileft=naxes(ielemm)-(irun*ielem)
           if(ileft.eq.0)then
c              irun=naxes(ielemm)
              ileft=0
           endif
           
c      Sort through all of the information in one data unit and
c      separate it into its constituent components and rewrite it
c      into a differing form.

c           print*,'Finish one row loop'

c      Set up the timearray with all of the times that are going to be
c      printed out for each row...

           tint=cdlt(ielemm)
c           print*,'tint from cdlt is ',tint,ielemn
c      Write out the time stamps that were calculated for this row...
           
           inchan=((lrow-1)*naxes(ielemm))+1
c           print*,'INCHAN naxes status is ',
c     &          inchan,naxes(ielemm),status
c           call ftpcld(ounit,1,inchan,1,naxes(ielemm),fasthold,status)
c           print*,'Wrote out the time values',
c     &          fasthold(1),fasthold(1024),status
c      We will read in ielem elements for each read.
           
           do 80 ichn=1,irun

c      Read into "temparay," ielem elements of I*2 values.
c      If we do more than 1 loop each read will skip over all of the
c      values already read in.
              ifelem=((ichn-1)*(ielem))+1

              do 82 isize=1,onaxes(1)
                 
c                 print*,'About to do a I*2 read and status is ',status

c                call ftgcli(iunit,colpos,lrow,ifelem,ielem,incr
c    &                ,nultyp,nulval,temparay,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvi since ftgcli should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
                 call ftgcvi(iunit,colpos,lrow,ifelem,ielem,nulval,
     &                temparay,anynul,status)

c                 print*,'Did a I*2 read and status is ',status
c      Print out any error information about accessing the files
                 if(status.ne.0)then
                    call fcerrm(status)
                 endif
           
c      We have read in ielem elements from the first access to the input
c      file. We have to rearrange this information into the proper positions
c      so that we can print out all of the array elements for each print...

c      lelem acts as a variable which allows us to put the values stored
c      in temparary into array. Since array is a 1D array we have to
c      successively begin at higher values. So we have to increase
c      lelem by the correct number "onaxes" on each increment.
                 
                    lelem=((ichn-1)*ielem)+isize
                 
                    do 81 j=1,ielem
                       array(lelem)=temparay(j)
                       lelem=lelem+onaxes(1)
81                  continue
                 
                 ifelem=ifelem+naxes(ielemm)
                 lelem=((isize-1)*ielem)+1                 

82            continue


c              print*,'Finished reading in array and ready to write'
c              print*,array(1),array(2),array(3)
c      Now we have read in a certain number of complete elements. So we
c      have to calculate the starting point which we put in lfelem. And
c      then write out all of the elements into the output file.


              if(colexpd)then
                 do 70 jsize=1,ielem
                    fasthold=(timearay(1)+
     &                   (float(iperiod)*tint))

                    call ftpcld(ounit,1,inchan+iperiod,1,1,
     &                   fasthold,status)
                    lfelem=((lrow-1)*naxes(ielemm))+
     &                   ((ichn-1)*(ielem))+jsize
                    call ftpcli(ounit,2,lfelem,1,onaxes(1)
     &                   ,array((jsize-1)*onaxes(1)+1),status)
                    iperiod=iperiod+1
70               continue
              else
                 j=0
                 lfelem=((lrow-1)*naxes(ielemm))+((ichn-1)*(ielem))+1

                 do 183 jsize=1,ielem
                    fasthold=(timearay(1)+
     &                   (float(iperiod)*tint))

                    call ftpcld(ounit,1,inchan+iperiod,1,1,
     &                   fasthold,status)
                    iperiod=iperiod+1
                    
                    do 182 isize=1,onaxes(1)
                       j=j+1
                       call ftpcli(ounit,isize+1,(lfelem-1)+jsize,1,1
     &                      ,array(j),status)

182                 continue
183              continue

              endif

              
80            continue

c           print*,'***********DID all CYCLIC reads***************'
c**********************************************************************
c      Now that we have completed all of the reads that we can
c      perform using ielem elements, we now have to go pick up any
c      that are remaining in the row which need to be processed.

c      If there are no elements that were missed ileft will equal 0
           if(ileft.ne.0)then

c      Read in an integer array of elements into "array" 
c      So calculate where we have to move to. So we skip over all of the
c      values already read in above and go to the first which hasn't. This
c      value is stored in ifelem
              ifelem=((ichn-1)*(ielem))+1

c              print*,'Did a read of ',ileft,' elements at ',ifelem
c              print*,'The last first element to read is',ifelem
c              print*,'ichn is ',ichn,' irun is ',irun, ' ielem ',ielem
c              print*,'lfelem is',lfelem
              
c      So now we will actually read in all of the elements missed. We will
c      loop over the number of elements
              do 85 isize=1,onaxes(1)
c                call ftgcli(iunit,colpos,lrow,ifelem,ileft,incr
c    &                ,nultyp,nulval,temparay,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvi since ftgcli should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
                 call ftgcvi(iunit,colpos,lrow,ifelem,ileft,nulval,
     &                temparay,anynul,status)
                 
c      Print out any error information about accessing the files
                 if(status.ne.0)then
                    call fcerrm(status)
                 endif

c      We have read in ielem elements from the first access to the input
c      file. We have to rearrange this information into the proper positions
c      so that we can print out all of the array elements for each print...
c      We are again moving all of the values from the input array "temparay"
c      into the output array "array". So we have to move from the evenly
c      incremented "temparay" array into the output array - "array"

                    lelem=isize

                    do 86 j=1,ileft
                       array(lelem)=temparay(j)
                       lelem=lelem+onaxes(1)
                       
86                  continue

c      Increment the first element position
                    ifelem=ifelem+naxes(ielemm)

85            continue

c              print*,'Did a read of ',ielem,' elements at ',ifelem
c              print*,'lrow,ifelem,
c     &             ,temparay(1),temparay(2)'
c              print*,lrow,ifelem
c     &             ,temparay(1),temparay(2)
c              print*,'lelem after final read is  ',lelem

c      Now we have read in a certain number of complete elements. In
c      order to calculate this we have to take into account which
c      row we are in and increment lfelem accordingly.
              
              if(colexpd)then
                 do 75 jsize=1,ileft
                    fasthold=(timearay(1)+
     &                   (float(iperiod)*tint))

                    call ftpcld(ounit,1,inchan+iperiod,1,1,
     &                   fasthold,status)
                    
                    lfelem=((lrow-1)*naxes(ielemm))+
     &                   ((ichn-1)*(ielem))+jsize
                    call ftpcli(ounit,2,lfelem,1,onaxes(1)
     &                   ,array((jsize-1)*onaxes(1)+1),status)
                    iperiod=iperiod+1
75               continue
              else
                 lfelem=((lrow-1)*naxes(ielemm))+((ichn-1)*(ielem))+1
                 j=0
                 do 188 jsize=1,ileft
                    fasthold=(timearay(1)+
     &                   (float(iperiod)*tint))
                    call ftpcld(ounit,1,inchan+iperiod,1,1,
     &                   fasthold,status)
                    iperiod=iperiod+1

                    do 187 isize=1,onaxes(1)
                       j=j+1
                       call ftpcli(ounit,isize+1,(lfelem-1)+jsize,1,1
     &                   ,array(j),status)
187                 continue

188              continue
              
              endif

           endif

c      This last section should have picked up all of the remaining
c      elements in a row.
c**********************************************************************

        endif   

50      continue

c       Update the CHKSUM keyword in the output file created.
        call ftpcks(ounit,status)
        
c      Close up each file after extracting the useful information.
        call ftclos(ounit,status)
            
c10      continue

c      Print out any error information about accessing the files
        if(status.ne.0)then
           call fcerrm(status)
        endif

999     continue
        return
        end

