C**********************************************************************
c
c      wrtphafile
c
c
c DESCRIPTION:
c      Writes the array in REALCHAN out to the unit given in
c      ounit.
c
c AUTHOR:
c      Brian K. Elza
c
c MODIFICATION HISTORY:
c      None yet.
c
c ARGUMENTS:
c      ounit      i- unit number associated with the output fits file
c      realchan   e- array containing the values to be written
c      naxes      i- 2D array containing elements x channel
c      spmode    c- write out a EVENT_SUM or EVENT_RATE file???
c
c CALLED ROUTINES:
c      ftphbn      - write out the primary binary header keywords
c      ftbdef      - define the structure of the binary table unit
c      ftpcli      - write out an array of i*2 elements to a column
c      ftpcle      - write out an array of r*4 elements to a column
c      ftclos      - close up the output file
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
        
        subroutine wrtphafile(files,outfile,ounit,nchan,
     &   no,spmode,extenpha,lspbin,tnull,
     &   timemin,timemax,binsz,totsecs,dchannel,realchan,
     &   mspinten,iconoffsettotal,
     &   isave,cols,chbin,cpixold,
     &   rstor,iwrt,cwrite,lwrite,clobber,
     &   lext)
        
        implicit none

        integer nb,isiz,nc,icols
        parameter (nb = 512)
        parameter (nc = 5)
        parameter (isiz = 999)
        parameter (icols=40)
       
        character*(*) files(isiz),spmode,outfile,cwrite(nb),
     &     extenpha,cols(icols),cpixold,chbin
        character(20) tempname,cval
        
        integer ounit,nfield,gcount,pcount,status,
     &       izero,no,ione,nchan,iwrt,j,isave,icvalen
        character(40) ttypes(nc),tforms(nc),tunits(nc),extnam,
     &     ttyper(nc),tformr(nc),tunitr(nc),contxt,
     &     ttypem(nc),tformm(nc),tunitm(nc),
     &     timesys,timeunit
        character(160) file1,file2
        character(80) none
        double precision realchan(*),mjdref,dchannel(*)
        integer*2 chanaray
        real rstor(nb),tnull
        double precision totsecs,timemin,timemax,
     &       binsz,mspinten
        
        character(80) origin,telescop,instrume,
     &     radecsys,observer,object,dateobs,timeobs,dateend,
     &     timeend,commentlines(128)
        real equino,ra,dec,deltat
        double precision realval,error
        integer extnum,iunit,iunitn,xtend,bitpix,i,k,block,bitpix1,
     &     naxis,outlen,fcstln,iput,imjdref,
     &     iconoffsettotal, noofcomments
c MJT 15July96 (g77/linux) extend used as logical -- fixed declaration
        logical simple,lwrite(nb),clobber,lext,lspbin,gainapp,
     &     lfxbary,lfxtime,extend

        character(40) taskname        
        common /task/ taskname

        common/gtinfo/mjdref,imjdref,ra,dec,equino,dateobs,timeobs
     &       ,dateend,timeend,
     &       instrume,object,radecsys,timesys,timeunit

        common/comments/commentlines,noofcomments
        
c      Set the values such that they are defined for a light curve
c      file - the options are for EVENT_SUM and EVENT_RATE and
c      for equispaced bins or non-equispaced bins.

        mjdref=0.0d0
        imjdref=0
        lfxbary=.FALSE.
        lfxtime=.FALSE.
        
        data extnam/'SPECTRUM'/
        
        ttypes(1)='CHANNEL'
        tforms(1)='1I'
        tunits(1)=' '

        ttypes(2)='COUNTS'
        tforms(2)='1D'
        tunits(2)='count'

        ttypes(3)='STAT_ERR'
        tforms(3)='1D'
        tunits(3)='count'

        ttyper(1)='CHANNEL'
        tformr(1)='1I'
        tunitr(1)=' '

        ttyper(2)='RATE'
        tformr(2)='1D'
        tunitr(2)='count/s'

        ttyper(3)='STAT_ERR'
        tformr(3)='1D'
        tunitr(3)='count/s'

        ttypem(1)='CHANNEL'
        tformm(1)='1I'
        tunitm(1)=' '

        ttypem(2)='MEAN'
        tformm(2)='1D'
        tunitm(2)='count'

        ttypem(3)='STAT_ERR'
        tformm(3)='1D'
        tunitm(3)='count'

        nfield=3
           
c        print*,'Input values into wrtphafile'
c        print*,files(1),outfile,realchan(1),nchan,no,spmode
c     &       dateobs(1),timeobs(1),dateend(no),timeend(no),totsecs


        none='NONE    '   
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
        dec=0.0d0
        ra=0.0d0
        status=0
        
c      Assign a unit file number to the output file.

        call ftgiou(iunitn,status)
        if(status.ne.0)then
           contxt='Error getting input unit number'
           call fcecho(contxt)
           contxt='Setting to logical unit 8'
           call fcecho(contxt)
           status=0
           iunitn=8
        endif

                
c      Assign a unit file number to the input file.

        call ftgiou(iunit,status)
        if(status.ne.0)then
           contxt='Error getting input unit number'
           call fcecho(contxt)
           contxt='Setting to logical unit 9'
           call fcecho(contxt)
           status=0
           iunit=9
        endif

c        print*,'about to read in fits info'

        status=0
        if(no.gt.1)then
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
        
c        print*,'out of parse'

          if (extnum .eq. -99) extnum = 1
c        print*,'extnum is ',extnum,files(no),file1

c      Open the final file 
          call ftopen(iunitn,file1,0,block,status)
c        print*,'opened file to read'

          if (status .ne. 0) then
            contxt = 'unable to open last file in infile'
            call fcerrm(status)
            call fcerr(contxt)
            call ftclos(iunitn,status)
            if(status.ne.0)then
              contxt='Error closing input file'
              call fcecho(contxt)
              status=0
            endif
           
            call ftfiou(iunitn,status)
            if(status.ne.0)then
              contxt='Error freeing output unit number'
              call fcecho(contxt)
              status=0
            endif
           
            return
          endif

c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status)
c      where data# is the data unit number to go to (here the second)
c      and where type 0= primary HDU, 1= ASCII table, 2=Binary table.

c        print*,'about to attempt to move to extnum+1'
          call ftmahd(iunitn,extnum+1,xtend,status)
          if(status.ne.0)then
            contxt='Error moving to extnum+1'
            call fcerr(contxt)
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

c        print*,'out of parse'
	if (extnum .eq. -99) extnum = 1
c        print*,'extnum is ',<extnum,files(1),file1

c      Open the initial file 
	call ftopen(iunit,file1,0,block,status)
c        print*,'opened file to read'

        if(status.ne.0)then
           contxt='Failure to open initial input file - aborting'
           call fcerr(contxt)
           call fcerrm(status)
           status=0
           call ftclos(iunit,status)
           call ftfiou(iunit,status)
           return
        endif

c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status)
c      where data# is the data unit number to go to (here the second)
c      and where type 0= primary HDU, 1= ASCII table, 2=Binary table.

c        print*,'about to attempt to move to extnum+1'
        call ftmahd(iunit,extnum+1,xtend,status)
        if(status.ne.0)then
           contxt='Error moving to extnum+1'
           call fcerr(contxt)
           call fcerrm(status)
           status=0
         endif

c      print*,'opened iunit and moved to second extension'
c        print*,'outfile is ',outfile
c      Read in the value that goes with the assigned keyword

         call ftgkys(iunit,'ORIGIN',origin,contxt,status)
         if(status.ne.0)then
           status=0
           call ftgkys(iunit,'TELESCOP',origin,contxt,status)
         endif
         if(status.ne.0)then
           contxt='Could not find keyword ORIGIN'
           call fcecho(contxt)
           contxt='Setting ORIGIN to UNKNOWN'
           call fcecho(contxt)
           origin='UNKNOWN'
           status=0
         endif
        
        call ftgkys(iunit,'DATE-OBS',dateobs,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword DATE-OBS'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
        call ftgkys(iunit,'TIME-OBS',timeobs,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword TIME-OBS'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
        call ftgkys(iunit,'TIMESYS',timesys,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword TIMESYS'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftgkyl(iunit,'GAINAPP',gainapp,contxt,status)
        if(status.ne.0)then
          gainapp=.FALSE.
          status=0
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

        
        call ftgkys(iunit,'TIMEUNIT',timeunit,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword TIMEUNIT'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
        call ftgkyd(iunit,'MJDREF',mjdref,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkyj(iunit,'MJDREFI',imjdref,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for MJDREFI')
            call fcecho('Proceeding with MJDREFI set to 0')
            imjdref=0
            status=0
          endif
          call ftgkyd(iunit,'MJDREFF',mjdref,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for MJDREFF')
            call fcecho('Proceeding with MJDREFF set to 0.0d0')
            mjdref=0.0d0
            status=0
          endif
        endif
        
c        print*,'mjdref is ',mjdref,imjdref
        
        call ftgkys(iunitn,'DATE-END',dateend,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword DATE-END'
           call fcecho(contxt)
           status=0
        endif
        
        call ftgkys(iunitn,'TIME-END',timeend,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword TIME-END'
           call fcecho(contxt)
           status=0
        endif
        
        call ftgkys(iunit,'OBJECT',object,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword OBJECT'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
c        print*,'out of obj'
        call ftgkye(iunit,'RA_PNT',ra,contxt,status)
        if(status.ne.0)then
           status=0
           call ftgkye(iunit,'RA_NOM',ra,contxt,status)
        endif
        if(status.ne.0)then
           status=0
           call ftgkye(iunit,'RA_OBJ',ra,contxt,status)
        endif

C	The following if statement is added by Zhiyu Guo on 5/12/98 to 
C	enable the program to check for RA_OBS keyword

        if(status.ne.0)then
           status=0
           call ftgkye(iunit,'RA_OBS',ra,contxt,status)
        endif

        
        if(status.ne.0)then
           contxt='Could not find RA_NOM, RA_PNT, RA_OBJ, or RA_OBS'
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

C       The following if statement is added by Zhiyu Guo on 5/12/98 to
C       enable the program to check for RA_OBS keyword

        if(status.ne.0)then
           status=0
           call ftgkye(iunit,'DEC_OBS',dec,contxt,status)
        endif

        if(status.ne.0)then
           contxt='Could not find DEC_NOM, DEC_PNT, DEC_OBJ, or DEC_OBS'
           call fcecho(contxt)
           status=0
        endif
        
        call ftgkye(iunit,'EQUINOX',equino,contxt,status)
        if(status.ne.0)then
           contxt='Could not find EQUINOX setting to 0.0'
           call fcecho(contxt)
           equino=0.0d0
           status=0
        endif
        
        if(status.ne.0)then
           call fcerrm(status)
           status=0
        endif
        
c        print*,'out of equinox'
c        print*,'contxt is',contxt
        call ftgkys(iunit,'RADECSYS',radecsys,contxt,status)
        if(status.ne.0)then
           contxt='Could not find RADECSYS setting to FK5'
           call fcecho(contxt)
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
          contxt='Could not find keyword OBSERVER'
          call fcecho(contxt)
          call fcerrm(status)
          status=0
        endif
        
        call ftgkys(iunit,'TELESCOP',telescop,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword TELESCOP'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
c        print*,'out of tele'
        call ftgkys(iunit,'INSTRUME',instrume,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword INSTRUME'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
        call ftgkye(iunit,'TIMEDEL',deltat,contxt,status)
        if(status.ne.0)then
           contxt='Could not find keyword TIMEDEL'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
        call ftclos(iunit,status)
        if(status.ne.0)then
           contxt='Error closing initial input unit'
           call fcerr(contxt)
           call fcerrm(status)
           status=0
        endif

        if(no.gt.1)then        
          call ftclos(iunitn,status)
          if(status.ne.0)then
            contxt='Error closing last input iunitn'
            call fcerr(contxt)
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
        
c        print*,'closed file1'
c----------------------------------------------------------------------

        izero = 0
        ione = 1

        if(lext)goto 810
        
        call ftgiou(ounit,status)
        if(status.ne.0)then
           contxt='Error getting output unit number'
           call fcecho(contxt)
           contxt='Setting to logical unit 12'
           call fcecho(contxt)
           status=0
           ounit=12
        endif

c----------------------------------------------------------------------
c      Print out the Primary Data Header 
c      
c      Parse the character output root file name 
        call fcpars(outfile,file1,extnum,status)
        if(status.ne.0)then
          contxt='Error parsing output root file name'
          call fcerr(contxt)
          call fcerrm(status)
          status=0
        endif

c      Parse the character output file extension name 
        call fcpars(extenpha,file2,extnum,status)
        if(status.ne.0)then
          contxt='Error parsing output extention PHA file name'
          call fcerr(contxt)
          call fcerrm(status)
          status=0
        endif

c      Now since the outfile only contains the prefix of the
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
c        if(clobber)call delfil(file1(:outlen))
              
c      Create output file and overwrite it if it exists.

        call fcecho(' ')
        call fcecho('Creating output spectral file:')
        call fcecho(file1(:outlen))
        
c        print*,'file1 is',file1
c      Open and initialize output FITS file...
        call ffinit(ounit,file1(:outlen),status)
c        call ftclos(ounit,status)
c        call ftinit(ounit,file1(:outlen),block,status)
c        print*,'Have initialized outfile'
        if(status.ne.0)then
           contxt='Error cannot write SPECTRUM file'
           call fcerr(contxt)
           call fcerrm(status)
           return
        endif

        simple = .TRUE.
        bitpix = 8
        bitpix1 = -32
        naxis = 0
        pcount = 0
        gcount = 1
        extend = .TRUE.

        call ftphpr(ounit,simple,bitpix1,naxis,0,
     &       pcount,gcount,extend,status)
        if(status.ne.0)then
           contxt='Error writing initial KEYWORDS for outfile'
           call fcerr(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out that this file contains a LIGHT CURVE.
         call ftpkys(ounit,'CONTENT','SPECTRUM',
     &       'light spectrum file',
     &       status)
         if(status.ne.0)then
            contxt='Error writing keyword CONTENT'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the origin of this file
         CALL FTPKYS(ounit,'origin','NASA/GSFC/XTE/GOF',
     &        'origin of fits file',status)
         if(status.ne.0)then
            contxt='Error writing keyword ORIGIN'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c       Write out the name of the code and its version number        
         call ftpkys(ounit,'CREATOR',taskname,
     &        'Program name that produced this file',status)
c         print*,'wrote CREATOR'
         if(status.ne.0)then
            contxt='Error writing keyword CREATOR'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the creation date of this file...
        call ftpdat(ounit,status)
        if(status.ne.0)then
           contxt='Error in writing out creation date'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
 
         CALL FTPKYS(ounit,'TELESCOP',telescop,
     &        'Telescope (mission) name',status)
         if(status.ne.0)then
            contxt='Error writing keyword TELESCOP'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'INSTRUME',instrume,
     &        'Instrument used for observation',status)
         if(status.ne.0)then
            contxt='Error writing keyword INSTRUME'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        if(imjdref.eq.0)then
          call ftpkyd(ounit,'MJDREF',mjdref,16,'1993.0',status)
          if(status.ne.0)then
            contxt='Error writing keyword MJDREF'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif
        else
          call ftpkyj(ounit,'MJDREFI',imjdref,
     &       'Integer part of MJDREF',status)
          if(status.ne.0)then
            contxt='Error writing keyword MJDREFI'
            call fcecho(contxt)
            status=0
          endif
          call ftpkyd(ounit,'MJDREFF',mjdref,15,
     &       'Fractional part of MJDREF ',status)
          if(status.ne.0)then
            contxt='Error writing keyword MJDREFF'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif
        endif

c        print*,'wrote MJDREF'
         
        call ftpkyd(ounit,'TSTART',timemin,14,
     &       'Observation start time',status)
c      print*,' wrote TSTART'
        if(status.ne.0)then
           contxt='Error writing keyword TSTART'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c         print*,'calculated tstopi ',timemax

        call ftpkyd(ounit,'TSTOP',timemax,14,
     &       'Observation stop time',status)
c         print*,' wrote TSTOP'
        if(status.ne.0)then
           contxt='Error writing keyword TSTOP'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
        
         call ftpkys(ounit,'OBJECT',object,
     &        'OBJECT from the FIRST input file',status)
         if(status.ne.0)then
            contxt='Error writing keyword OBJECT'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkye(ounit,'RA_OBJ',ra,8,
     &        'RA of First input object',status)
         if(status.ne.0)then
            contxt='Error writing keyword RA_OBJ'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkye(ounit,'DEC_OBJ',dec,8,
     &        'DEC of First input object',status)
         if(status.ne.0)then
            contxt='Error writing keyword DEC_OBJ'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkyf(ounit,'EQUINOX',equino,2,
     &        'Equinox of the FIRST object',status)
c         print*,'wrote EQUINOX'
         if(status.ne.0)then
            contxt='Error writing keyword EQUINOX'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'RADECSYS',radecsys,
     &        'Co-ordinate frame used for equinox',status)
c        print*,'wrote RADECSYS'
         if(status.ne.0)then
            contxt='Error writing keyword RADECSYS'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c         call ftpkyj(ounit,'OBS-MODE',ione,
c     &        'Observation mode 1=point, 2=slew,3=calibration',status)
c         print*,'wrote obs-mode',ione
        if(status.ne.0)then
           contxt='Error writing keyword OBS-MODE'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'DATE-OBS',dateobs,
     &        'EARLIEST observation date of files',status)
c         print*,'wrote date-obs',dateobs
         if(status.ne.0)then
            contxt='Error writing keyword DATE-OBS'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'TIME-OBS',timeobs,
     &        'EARLIEST time of all input files',status)
c         print*,'wrote time-obs',timeobs
         if(status.ne.0)then
            contxt='Error writing keyword TIME-OBS'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'DATE-END',dateend,
     &        'LATEST observation date of files',status)
c         print*,'wrote date-end',dateend
         if(status.ne.0)then
            contxt='Error writing keyword DATE-END'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'TIME-END',timeend,
     &        'LATEST time of all input files',status)
c         print*,'wrote time-end',timeend
         if(status.ne.0)then
            contxt='Error writing keyword TIME-END'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkys(ounit,'TIMESYS',timesys,
     &     'The time system',status)
c         print*,' wrote TIMESYS'
        if(status.ne.0)then
          contxt='Error writing keyword TIMESYS'
          call fcecho(contxt)
          call fcerrm(status)
          status=0
        endif

c      print*,'Out of first data header'
         call ftpkys(ounit,'PHAVERSN','1992a',
     &        'OGIP memo number for file format',status)
c         print*,'wrote PHAVERSN'
         if(status.ne.0)then
            contxt='Error writing keyword PHAVERSN'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c       call ftpdef(ounit,bitpix1,naxis,0,pcount,gcount,status)
c       if(status.ne.0)then
c          contxt='Error in defining KEYWORD header space for outfile'
c          call fcecho(contxt)
c          call fcerrm(status)
c          status=0
c       endif
c 3Dec97 - No longer necessary and conflicts w/FITSIO v5.04 (MJT)

c      print*,'wrote out end'
c      Wrote out primary data unit
c----------------------------------------------------------------------

c      This is where we jump to, to create another data header unit
        
810     continue

c        print*,'Creating ANOTHER data header'

        call ftpcks(ounit,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('ERROR creating/updating primary CHKSUM keyword.')
          status=0
        endif
        
c      Create another Header Data Unit
        call ftcrhd(ounit,status)

        if(status.ne.0)then
           contxt='ERROR creating second header extension....'
           call fcerr(contxt)
           call fcerrm(status)
           status=0
        endif

c         print*,'Created second data header',status

c      Define the information about the number of columns and the
c      like. This information will have to change at a later date as
c      the options for calculating errors and other information are
c      included. Actually the above will have to be expanded
c      as well... 

        gcount = 1
        pcount = 0

c        nrows = nchan

c      See what option has been chosen - either EVENT_SUM
c      or EVENT_RATE is supported.

c        print*,'tforms(1),tforms(2),tunits(1)
c     &       ,tunits(2),ttypes(1),ttypes(2)'

c        print*,tforms(1),tforms(2),tunits(1)
c     &       ,tunits(2),ttypes(1),ttypes(2),extnam
        
        if ( spmode.eq.'EVENT_SUM'.or.
     &       spmode.eq.'SUM') then

c      Write out the header information standard keywords for
c      writing a binary table extension.
           call ftphbn(ounit,nchan,nfield,ttypes,tforms,tunits,
     &          extnam,pcount,status)
           if(status.ne.0)then
              contxt='Error in writing header information'
              call fcerr(contxt)
              call fcerrm(status)
              status=0
           endif

c           print*,'wrote out count information header',status

        else if ( spmode.eq.'EVENT_RATE'.or.
     &          spmode.eq.'RATE') then
c           print*,'In EVENT_RATE section'
c      Write out the header information standard keywords for
c      writing a binary table extension.
           call ftphbn(ounit,nchan,nfield,ttyper,tformr,tunitr,
     &          extnam,pcount,status)
           if(status.ne.0)then
              contxt='Error in writing header information'
              call fcerr(contxt)
              call fcerrm(status)
              status=0
           endif

c           print*,'successfully wrote initial header information.'

        else if ( spmode.eq.'MEAN') then
           
c           print*,'In MEAN section'
c      Write out the header information standard keywords for
c      writing a binary table extension.
           call ftphbn(ounit,nchan,nfield,ttypem,tformm,tunitm,
     &          extnam,pcount,status)
           if(status.ne.0)then
              contxt='Error in writing header information'
              call fcerr(contxt)
              call fcerrm(status)
              status=0
           endif

c           print*,'successfully wrote initial header information.'

        endif

c----------------------------------------------------------------------
c      Now that we have the storage information entered. We will enter
c      the pertinent information with regard to HDUCLASS.

        call ftpkys(ounit,'HDUCLASS','OGIP',
     &       'format conforms to OGIP/GSFC standards',status)
         if(status.ne.0)then
            contxt='Error writing keyword HDUCLASS'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
         endif

        call ftpkys(ounit,'HDUCLAS1','SPECTRUM',
     &       'Extension contains a Spectrum',status)
         if(status.ne.0)then
            contxt='Error writing keyword HDUCLAS1'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
         endif

        call ftpkys(ounit,'HDUCLAS2','TOTAL',
     &       'Extension contains a Spectrum',status)        
         if(status.ne.0)then
            contxt='Error writing keyword HDUCLAS2'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
         endif

         if(spmode.eq.'SUM'.or.spmode.eq.'EVENT_SUM')
     &        call ftpkys(ounit,'HDUCLAS3','COUNT',
     &       'Extension contains counts',status)        

         if(spmode.eq.'RATE'.or.spmode.eq.'EVENT_RATE')
     &        call ftpkys(ounit,'HDUCLAS3','RATE',
     &       'Extension contains rate',status)        

         if(spmode.eq.'MEAN')
     &        call ftpkys(ounit,'HDUCLAS3','MEAN',
     &       'Extension contains mean',status)        

         if(status.ne.0)then
            contxt='Error writing keyword HDUCLAS3'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
         endif

        call ftpkys(ounit,'HDUVERS1','1.1.0',
     &       'Version number of the format',status)        
         if(status.ne.0)then
            contxt='Error writing keyword HDUVERS1'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
         endif
         
         if(iwrt.gt.0)then
           do 800 j=1,iwrt
             if(lwrite(j))then
               contxt='MEAN value for this column'
             else
               contxt='SUM of values for this column'
             endif
             call ftpkye(ounit,cwrite(j),rstor(j),10,contxt,status)
800        continue
         endif
         
C----------------------------------------------------------------------
c      Let's write out additional information that is required for
c      XSPEC to function.

c      Write out the statistical error form used
c        call ftpkyj(ounit,'STAT_ERR',izero,
c     &       'No statistical error specified',status)
c        print*,'wrote stat-err',status
c      Poisson error???
c        if(status.ne.0)then
c           contxt='Error writing keyword STAT_ERR'
c           call fcecho(contxt)
c           call fcerrm(status)
c           status=0
c        endif

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
           endif
         endif
         
         call ftpkyl(ounit,'POISSERR',.FALSE.,
     &      'Are Poisson Distribution errors assumed.',status)
c        print*,'wrote poisserr',status
c      Any systematic error specified?
         if(status.ne.0)then
           contxt='Error writing keyword POISSERR'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
         endif

c        if(ttyper(3).ne.'ERROR'.and.nfield.ne.3)
         call ftpkyj(ounit,'SYS_ERR',izero,
     &      'No systematic error was specified',status)
c        print*,'wrote sys-err',status
c      Any grouping information
        if(status.ne.0)then
           contxt='Error writing header keyword SYS_ERR'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkyj(ounit,'GROUPING',izero,
     &       'No grouping data has been specified',status)
c        print*,'wrote grouping info',status
        if(status.ne.0)then
           contxt='Error writing keyword GROUPING'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Any Data Quality information of the date?
        call ftpkyj(ounit,'QUALITY',izero,
     &       'No data quality information specified',status)
c        print*,'wrote quality',status
        if(status.ne.0)then
           contxt='Error writing keyword QUALITY'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the mission name.
        call ftpkys(ounit,'TELESCOP',telescop,
     &       'Telescope (mission) name',status)
c        print*,'wrote telescop',status
        if(status.ne.0)then
           contxt='Error writing keyword TELESCOP'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the name of the instrument
        call ftpkys(ounit,'INSTRUME',instrume,
     &       'Instrument name',status)
c        print*,'wrote instrument',status
        if(status.ne.0)then
           contxt='Error writing keyword INSTRUME'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Any filter information?
        call ftpkys(ounit,'FILTER',none,
     &       'Instrument filter in use',status)
c        print*,'wrote filer',status
        if(status.ne.0)then
           contxt='Error writing keyword FILTER'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the total exposure time
        call ftpkyd(ounit,'EXPOSURE',totsecs,14,
     &       'Exposure time',status)
c        print*,'wrote exposure',status
        if(status.ne.0)then
           contxt='Error writing keyword EXPOSURE'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the area scale 
        call ftpkye(ounit,'AREASCAL',1.0e0,8,
     &       'Nominal effective area',status)
c        print*,'wrote areascal',status
        if(status.ne.0)then
           contxt='Error writing keyword AREASCAL'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out background scaling factor
        call ftpkye(ounit,'BACKSCAL',1.0e0,8,
     &       'Background scale factor',status)
c        print*,'wrote backscal',status
        if(status.ne.0)then
           contxt='Error writing keyword BACKSCAL'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkye(ounit,'CORRSCAL',0.0e0,8,
     &       'Correlation scale factor',status)
c        print*,'wrote corrscal',status
        if(status.ne.0)then
           contxt='Error writing keyword CORRSCAL'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out other important information
        call ftpkys(ounit,'BACKFILE',none,
     &       'Background FITS file for this object',status)
c        print*,'wrote BACKFILE',status
        if(status.ne.0)then
           contxt='Error writing keyword BACKFILE'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkys(ounit,'CORRFILE',none,
     &       'Correlation FITS file for this object',status)
c        print*,'wrote CORRFILE',status
        if(status.ne.0)then
           contxt='Error writing keyword CORRFILE'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkys(ounit,'RESPFILE',none,
     &       'Redistribution matrix file (RMF)',status)
c        print*,'wrote RESPFILE',status
        if(status.ne.0)then
           contxt='Error writing keyword RESPFILE'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkys(ounit,'ANCRFILE',none,
     &       'Ancillary response file (ARF)',status)
c        print*,'wrote ANCRFILE',status
        if(status.ne.0)then
           contxt='Error writing keyword ANCRFILE'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkys(ounit,'XFLT0001',none,
     &       'XSPEC selection filter description',status)
c        print*,'wrote XFLT0001',status
        if(status.ne.0)then
           contxt='Error writing keyword XFLT0001'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkys(ounit,'CHANTYPE','PHA',
     &       'Channels assigned by detector electronics',status)
c        print*,'wrote CHANTYPE',status
        if(status.ne.0)then
           contxt='Error writing keyword CHANTYPE'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the total number of detector channels available.
        call ftpkyj(ounit,'DETCHANS',nchan,
     &       'Total number of detector channels available',status)
c        print*,'wrote DETCHANS',status
        if(status.ne.0)then
           contxt='Error writing keyword DETCHANS'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkyj(ounit,'TLMIN1',izero,
     &       'Lowest Legal channel number',status)
        if(status.ne.0)then
           contxt='Error writing keyword TLMIN1'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkyj(ounit,'TLMAX1',nchan-1,
     &       'Highest Legal channel number',status)
        if(status.ne.0)then
           contxt='Error writing keyword TLMAX1'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
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
            call ftpkls(ounit,'CPIX1',cpixold,
     &         'Channel binning of CHANNEL column',status)
c         print*,'wrote OBJECT',status
            if(status.ne.0)then
              contxt='Error writing keyword CPIX1'
              call fcecho(contxt)
              call fcerrm(status)
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
            call ftpkls(ounit,'CPIX1',chbin,
     &         'Channel binning of CHANNEL column',status)
c         print*,'wrote OBJECT',status
            if(status.ne.0)then
              contxt='Error writing keyword CPIX1'
              call fcecho(contxt)
              call fcerrm(status)
              status=0
            endif
          endif
        endif
        
        call ftpkys(ounit,'OBJECT',object,
     &        'OBJECT from the FIRST input file',status)
c         print*,'wrote OBJECT',status
        if(status.ne.0)then
           contxt='Error writing keyword OBJECT'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c**********************************************************************

        k=0
        cval=' '
        icvalen=20

c        print*,'inocols is ',inocols,

        do 10 i=1,isave
          k=k+1

          if(k.gt.0)then
            call fti2c(k,cval,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Error converting integer to character')
              call fcecho('ROWID values in PHA file may be corrupted.')
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
          
          tempname='ROWID'
          tempname(6:6+j)=cval(20-j:20)

          call ftpkys(ounit,tempname,cols(i),
     &       'Column Name processed',status)
          if(status.ne.0)then
            contxt='Error writing keyword ROWID'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif
          
10      continue

        call xteftpklns(ounit,'FILEN',1,no,files,
     &     'Input files input to produce this file&',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword FILEN')
          call fcecho('Continuing...')
          status=0
        endif
        
        
c**********************************************************************
        
c      Write out the origin of this file

         CALL FTPKYS(ounit,'ORIGIN','NASA/GSFC',
     &        'origin of fits file',status)
c      print*,'wrote origin',status
         if(status.ne.0)then
            contxt='Error writing keyword ORIGIN'
            call fcecho(contxt)
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
           contxt='Error in writing out creation date'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkye(ounit,'RA_OBJ',ra,8,
     &       'RA of First input object',status)
c        print*,'wrote RA_OBJ',status
        if(status.ne.0)then
           contxt='Error writing keyword RA_OBJ'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkye(ounit,'DEC_OBJ',dec,8,
     &        'DEC of First input object',status)
c         print*,'wrote DEC_OBJ',status
         if(status.ne.0)then
            contxt='Error writing keyword DEC_OBJ'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkyf(ounit,'EQUINOX',equino,2,
     &        'Equinox of the FIRST object',status)
c         print*,'wrote EQUINOX',status
         if(status.ne.0)then
            contxt='Error writing keyword EQUINOX'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'RADECSYS',radecsys,
     &        'Co-ordinate frame used for equinox',status)
c        print*,'wrote RADECSYS',status
         if(status.ne.0)then
            contxt='Error writing keyword RADECSYS'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkye(ounit,'DROLLANG',0.0e0,8,
     &        'Mean roll angle',status)
c        print*,'wrote DROLLANG',status
         if(status.ne.0)then
            contxt='Error writing keyword DROLLANG'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

C----------------------------------------------------------------------
c      Let's write out additional information that is required for
c      XSPEC to function.

         call ftpkys(ounit,'PHAVERSN','1992a',
     &        'OGIP memo number for file format',status)
c         print*,'wrote PHAVERSN'
         if(status.ne.0)then
            contxt='Error writing keyword PHAVERSN'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c      Write out the creation date of this file...
        call ftpdat(ounit,status)
        if(status.ne.0)then
           call fcerrm(status)
           status=0
        endif
         
         call ftpkys(ounit,'DATE-OBS',dateobs,
     &        'EARLIEST observation date of files',status)
c         print*,'wrote DATE-OBS'
         if(status.ne.0)then
            contxt='Error writing keyword DATE-OBS'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'TIME-OBS',timeobs,
     &        'EARLIEST time of all input files',status)
c         print*,'wrote TIME-OBS'
         if(status.ne.0)then
            contxt='Error writing keyword TIME-OBS'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'DATE-END',dateend,
     &        'LATEST observation date of files',status)
c         print*,'wrote DATE-END'
         if(status.ne.0)then
            contxt='Error writing keyword DATE-END'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'TIME-END',timeend,
     &        'LATEST time of all input files',status)
c         print*,'wrote TIME-END'
         if(status.ne.0)then
            contxt='Error writing keyword TIME-END'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
         
         call ftpkys(ounit,'TIMESYS',timesys,
     &        'The time system is MJD',status)
c         print*,' wrote TIMESYS'
         if(status.ne.0)then
            contxt='Error writing keyword TIMESYS'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        call ftpkyl(ounit,'GAINAPP',gainapp,
     &     'Gain all ready subracted',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword GAINAPP')
          call fcerrm(status)
          status=0
        endif
        
         call ftpkys(ounit,'TIMEUNIT','s',
     &        'unit for TSTARTI/F and TSTOPI/F, TIMEZERO',status)
c         print*,' wrote TIMEUNIT'
         if(status.ne.0)then
            contxt='Error writing keyword TIMEUNIT'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        if(imjdref.eq.0)then
          call ftpkyd(ounit,'MJDREF',mjdref,16,'1993.0',status)
          if(status.ne.0)then
            contxt='Error writing keyword MJDREF'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif
        else
          call ftpkyj(ounit,'MJDREFI',imjdref,
     &       'Integer part of MJDREF',status)
          if(status.ne.0)then
            contxt='Error writing keyword MJDREFI'
            call fcecho(contxt)
            status=0
          endif
          call ftpkyd(ounit,'MJDREFF',mjdref,15,
     &       'Fractional part of MJDREF ',status)
          if(status.ne.0)then
            contxt='Error writing keyword MJDREFF'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif
        endif

c         print*,'wrote MJDREF'
         
         call ftpkyd(ounit,'TSTART',timemin,14,
     &        'Observation start time',status)
c         print*,' wrote TSTART'
         if(status.ne.0)then
            contxt='Error writing keyword TSTART'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

c         print*,'calculated tstopi ',timemax

         call ftpkyd(ounit,'TSTOP',timemax,14,
     &        'fractional observation stop time',status)
c         print*,' wrote TSTOP'
         if(status.ne.0)then
            contxt='Error writing keyword TSTOP'
            call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif
         
        call ftpkyd(ounit,'TIMEDEL',binsz,14,
     &       'integration time',status)
c         print*,' wrote TIMEDEL'
        if(status.ne.0)then
           contxt='Error writing keyword TIMEDEL'
           call fcecho(contxt)
           call fcerrm(status)
           status=0
        endif

        if(lspbin)then
          call ftpkye(ounit,'TNULL',tnull,8,
     &       'TNULL value',status)
          if(status.ne.0)then
            contxt='Error writing keyword TNULL'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif
        endif
        
C----------------------------------------------------------------------

c      Define the structure of the binary table date unit so that
c      we can actually write out the informtion.
c 3Dec97 - No longer necessary and conflicts w/FITSIO v5.04 (MJT)
c
c       if ( spmode.eq.'EVENT_SUM'.or.
c    &       spmode.eq.'SUM') then
c         
c          call ftbdef(ounit,nfield,tforms,pcount,nchan,status)
c          if(status.ne.0)then
c             contxt='Error in allocating output space for file'
c             call fcerr(contxt)
c             call fcerrm(status)
c             status=0
c          endif
c
c       else if ( spmode.eq.'EVENT_RATE'.or.
c    &          spmode.eq.'RATE') then
c
c          call ftbdef(ounit,nfield,tformr,pcount,nchan,status)
c          if(status.ne.0)then
c             contxt='Error in allocating output space for file'
c             call fcerr(contxt)
c             call fcerrm(status)
c             status=0
c          endif
c            
c       else if ( spmode.eq.'MEAN')then
c
c          call ftbdef(ounit,nfield,tformm,pcount,nchan,status)
c          if(status.ne.0)then
c             contxt='Error in allocating output space for file'
c             call fcerr(contxt)
c             call fcerrm(status)
c             status=0
c          endif
c            
c       endif
         
c      Set up the channel array with a numerical value I*2 which will
c      be written out to the fits file.

        chanaray=-1
        iput=0
        do 200 i=1,nchan
          chanaray=chanaray+1

           if(spmode.eq.'RATE'.or.
     &          spmode.eq.'EVENT_RATE')then
              realval=(dchannel(i+iconoffsettotal))/totsecs
              error=dsqrt(dabs(dchannel(i+iconoffsettotal)))/totsecs

              if(realval.gt.mspinten)then
                realval=dble(tnull)
                error=dble(tnull)
              endif
              
           elseif(spmode.eq.'SUM'.or.
     &             spmode.eq.'EVENT_SUM')then
             error=dsqrt(dabs(dchannel(i+iconoffsettotal)))
             realval=(dchannel(i+iconoffsettotal))

             if(realval.gt.mspinten)then
               realval=dble(tnull)
               error=dble(tnull)
             endif
             
           elseif(spmode.eq.'MEAN')then
             realval=(((dchannel(i+iconoffsettotal)))/
     &          (realchan(i+iconoffsettotal)))             
             error=dsqrt(dabs(realval))

             if(realval.gt.mspinten)then
               realval=dble(tnull)
               error=dble(tnull)
             endif
             
           endif

           call ftpcli(ounit,1,i,1,1,chanaray,status)
           call ftpcld(ounit,2,i,1,1,realval,status)
           call ftpcld(ounit,3,i,1,1,error,status)
           
200     continue

        if(status.ne.0)then
           contxt='Error in writing information to file'
           call fcerr(contxt)
           call fcerrm(status)
           status=0
        endif

        lext=.TRUE.

        return
        end
