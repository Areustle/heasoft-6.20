C****************************************************************************
C SELECTOR TASK:  
C      bemerge
C
C FILE:
C      bemerge.f 
C
C DESCRIPTION: 

C     
C AUTHOR:  
C      Brian K. Elza 10/95
C
C MODIFICATION HISTORY:
C
C     v3.6 (20Jan99) changing ftgkys to ftgkls (since CPIX kwds are long and CFITSIO version
C                    will not automatically handle long string kwds).
C
C NOTES:
C      
C     (20Jan99) Must update fitsfilecopy.f in xtelib/src/gen to get the recorded version number
C               for BEMERGE to appear in output file!!!!!
C      
C ARGUMENTS:
C      
C
C PRIMARY LOCAL VARIABLES:  
C      in_fil1    - input first FITS file and extension number
C      in_fil2    - input second FITS file and extension number
C      ou_fil     - output file
C      slop       - fractional allowable error
C      timeint_fil- output file to be created IF GTI's don't match.
C
C      
C CALLED ROUTINES:
C
C***************************************************************************
      subroutine bemere()
      implicit none

      character(160) in_fil1, in_fil2, ou_fil, timeint_fil
      double precision slop
      integer status

      character(40) taskname
      logical abort
      
      common /task/ taskname

      taskname = 'bemerge_3.6'
      in_fil1 = ' '
      in_fil2 = ' '
      ou_fil = ' '
      timeint_fil = ' '
      abort=.FALSE.
      slop = 0.0d0
      status=0

      call fcecho(' ')
      call fcecho('Running BEMERGE version 3.6')
      call fcecho('==============================================')
        
C     get the parameters from the par file
      call gbemergeparm(in_fil1, in_fil2, ou_fil, slop,
     &   timeint_fil, abort, status)
      if (status .ne. 0)then
        call fcecho(' ')
        call fcecho('Error reading information from Parameter file')
        call fcecho('Check parameter file, and fix error')
        call fcecho('Aborting....')
        abort=.TRUE.
        goto 999
      endif

      if(abort)then
        call fcecho(' ')
        call fcecho('Error reading information from Parameter file')
        call fcecho('Check parameter file, and fix error')
        call fcecho('Aborting....')
        goto 999
      endif
      
      call fcecho(' ')
      call fcecho('###########################################')
      call fcecho(' ')
      call fcecho('Begin processing...')

      call bemergeadd(in_fil1, in_fil2, ou_fil, slop,
     &   timeint_fil, abort, status)
      if(abort)then
        call fcecho(' ')
        call fcecho('Aborting error from BEMERGEADD routine!')
        goto 999
      endif

      call fcecho(' ')
      call fcecho('Finished BEMERGE. Merged file created was:')
      call fcecho(ou_fil)

999   continue
      return
      end


c**********************************************************************
c This subroutine reads in all of the input parameters and does
c some checks... 
c**********************************************************************
      
      subroutine gbemergeparm(in_fil1, in_fil2, ou_fil, slop,
     &   timeint_fil, abort, status)
      
      implicit none
      integer status,no
      character(160) infiles
      character(160) infile(2)
      character*(*) in_fil1, in_fil2 ,ou_fil, timeint_fil
      double precision slop
      logical abort
      abort=.FALSE.

C      initialize variables
      status=0
      no=0
      infiles = ' '

C      get the name of the first input FITS file
      call uclgst('in_fil1',infiles,status)
      if (status .ne. 0) then
        call fcerr('could not get IN_FIL1 parameter')
        goto 999
      endif

      if(infiles .eq. ' ')then
        call fcecho('IN_FIL1 value NOT SET! Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call fcgcls(infiles,infile,no,abort)
      if(no.eq.2)then
        in_fil1=infile(1)
        in_fil2=infile(2)
      else if(no.gt.2)then
        call fcecho(' ')
        call fcecho('ERROR!!! Can only accept 2 input files!')
        call fcecho('Aborting....')
        abort=.TRUE.
        goto 999
      else if(no.eq.1)then
        in_fil1=infiles

C      get the name of the second input FITS file
        call uclgst('in_fil2',in_fil2,status)
        if (status .ne. 0) then
          call fcerr('could not get IN_FIL2 parameter')
          goto 999
        endif
        if(in_fil2 .eq. ' ')then
          call fcecho('IN_FIL2 value NOT SET!')
        endif
          
      endif
      
C  get the name of the output root file
      call uclgst('out_file',ou_fil,status)
      if (status .ne. 0) then
        call fcerr('could not get OUT_FILE prefix parameter')
        goto 999
      endif
      if(ou_fil .eq. ' ')then
        call fcecho('OUT_FILE value NOT SET! Aborting')
        abort=.TRUE.
        goto 999
      endif

C  get the slop parameter
      call uclgsd('slop',slop,status)
      if(status .eq. 3)then
        status=0
        slop=0.01d0
      else if (status .ne. 0) then
        call fcerr('could not get SLOP parameter')
        abort = .TRUE.
        goto 999
      endif

C  get the name of the gti file to be created if needed
      call uclgst('timeint_fil',timeint_fil,status)
      if (status .ne. 0) then
        call fcerr('could not find TIMEINT_FIL parameter')
        goto 999
      endif
      if(timeint_fil .eq. ' ')then
        call fcecho('TIMEINT_FIL value NOT SET!')
        call fcecho('Setting to default value: timeint.fil')
        timeint_fil='timeint.fil'
      endif
      
999   continue

      return
      end

c**********************************************************************
c
c
c
c
c
c**********************************************************************

      subroutine bemergeadd(in_fil1, in_fil2, ou_fil, slop,
     &   timeint_fil, abort, status)

      character*(*) in_fil1, in_fil2, ou_fil,timeint_fil
      character*(2000)cpixtotal
      double precision slop
      integer status
      logical abort,phatype2

      integer iunit1,iunit2,ounit,ichars,extoufile,
     &   irow1,irow2,irowsadded,xtend,extinfile1,extinfile2

      phatype2=.FALSE.

c     ichars tells the size of the character string 'cpixtotal'
      ichars=2000
      irowsadded=0
      irow1=0
      irow2=0
      
c      print*,'Bemerge in_fil1, in_fil2, ou_fil, slop',
c     &   in_fil1, in_fil2, ou_fil, slop
      
c     Let's assign some logical unit numbers for each file.
c      Assign a unit file number used in inputting the first file.
      call ftgiou(iunit1,status)
      if(status.ne.0)then
        call fcecho('Error getting IN_FIL1 unit number')
        call fcecho('Setting to logical unit 10')
        status=0
        iunit=10
      endif

c      Assign a unit file number used in inputting the second file.
      call ftgiou(iunit2,status)
      if(status.ne.0)then
        call fcecho('Error getting IN_FIL2 unit number')
        call fcecho('Setting to logical unit 11')
        status=0
        iunit=11
      endif

c      Assign a unit file number used in outputting file.
      call ftgiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error getting OU_FIL unit number')
        call fcecho('Setting to logical unit 12')
        status=0
        ounit=12
      endif

c**********************************************************************      
      
      call fits_file_open(iunit1,in_fil1,extinfile1,abort,status)
      if(abort)then
        call fcecho(' ')
        call fcecho('ERROR trying to parse and open input file1.')
        goto 999
      endif

      call fits_file_open(iunit2,in_fil2,extinfile2,abort,status)
      if(abort)then
        call fcecho(' ')
        call fcecho('ERROR trying to parse and open input file2.')
        goto 999
      endif

      call bemerge_sortby_CPIX(iunit1,extinfile1,
     &   iunit2,extinfile2,cpixtotal,ichars,irow1,irow2,
     &   irowsadded,phatype2,abort,status)
      if(abort)then
        call fcecho(' ')
        call fcecho('ERROR trying to sort IN_FIL1 and IN_FIL2 by CPIX')
        goto 999
      endif

      call fits_file_init(ounit,ou_fil,abort,status)
      if(abort)then
        call fcecho(' ')
        call fcecho('ERROR trying to initialize OU_FIL')
        goto 999
      endif

      call fits_file_copy(iunit1,extinfile1,
     &   ounit,extoufile,abort,status)
      if(abort)then
        call fcecho(' ')
        call fcecho('ERROR trying to copy initial extensions from')
        call fcecho('IN_FIL1 to OU_FIL and moving to the extension')
        call fcecho('to be worked on.')
        goto 999
      endif

      call xtepha_merge_data(iunit1,extinfile1,
     &   iunit2,extinfile2,ounit,extoufile,
     &   cpixtotal,ichars,irow1,irow2,irowsadded,phatype2,
     &   slop,abort,status)

      if(abort)then
        call fcecho(' ')
        call fcecho('Error in fits_file_copy_data')
      endif

      call xtepha_merge_gtis(iunit1,extinfile1,
     &   iunit2,extinfile2,ounit,extoufile,
     &   timeint_fil,abort,status)

      if(abort)then
        call fcecho(' ')
        call fcecho('Error in fits_file_merge_gtis')
        goto 999
      endif

      
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c     Copy all of the remaining extensions if any from IN_FIL1 to OU_FIL
32    call ftmrhd(iunit1,1,xtend,status)

      if(status.eq.0)then
        call ftpcks(ounit,status)
        if(status.ne.0)status=0
      endif
      call ftcrhd(ounit,status)
      call ftcopy(iunit1,ounit,0,status)
      if(status.ne.0)then
        goto 33
      endif
      goto 32

33    continue
      status=0
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      call ftclos(iunit1,status)
      if(status.ne.0)then
        call fcecho('Error closing IN_FIL1.')
        abort=.TRUE.
        status=0
      endif

      call ftclos(iunit2,status)
      if(status.ne.0)then
        call fcecho('Error closing IN_FIL2.')
        abort=.TRUE.
        status=0
      endif
      
999   continue

c**********************************************************************
      
      call ftfiou(iunit1,status)
      if(status.ne.0)then
        call fcecho('Error freeing IN_FIL1 unit number')
        status=0
      endif

      call ftfiou(iunit2,status)
      if(status.ne.0)then
        call fcecho('Error freeing IN_FIL2 unit number')
        status=0
      endif

      call ftpcks(ounit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not update chksum keyword in output file')
        status=0
      endif
      
      call ftclos(ounit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcerr('Error closing output file.')
      endif
      
      call ftfiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error freeing OU_FIL unit number')
        status=0
      endif

      
      return
      end
      
c**********************************************************************
c
c
c
c
c
c**********************************************************************

      subroutine bemerge_sortby_CPIX(iunit1,extinfile1,
     &   iunit2,extinfile2,cpixtotal,ichars,irow1,irow2,
     &   irowsadded,phatype2,abort,status)

      implicit none
      integer iunit1,extinfile1,iunit2,extinfile2,status,
     &   xtend,ne,i,iunit,ichars,irow1,irow2,irowsadded
      character*(*)cpixtotal
      
      parameter (ne = 512)
      
      integer ilbd1(ne),iubd1(ne),inobd1,
     &   ilbd2(ne),iubd2(ne),inobd2,inobdhold,
     &   ilbdhold(ne),iubdhold(ne),fcstln,ioutlen1,
     &   ioutlen2,ilowlen,iuplen,itotal

      
      logical abort,ldryrun,phatype2,
     &   pha1type2,pha2type2
      character(1000) cpix1,cpix2,cpixhold
      character(80) comment
      character(10) clower,cupper,cinsert

      ldryrun=.FALSE.
      abort=.FALSE.
      pha1type2=.FALSE.
      pha2type2=.FALSE.
      status=0
      clower=' '
      cupper=' '
      cpix1=' '
      cpix2=' '
      cpixhold=' '

c     This subroutine will first search for CPIX and sort the input
c files so that the file with the lower channels will be in iunit1
c and the upper channels will be in iunit2. 
      
c**********************************************************************
      
c      Move to the second CHDU (or to extnum+1)
c      to read all pertinent processing information.
      call ftmahd(iunit1,extinfile1+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extinfile1')
        call fcerrm(status)
        status=0
      endif

C Read the CPIX1 or CPIX2 keyward to get the channel bins for iunit1
c      call ftgkys(iunit1, 'CPIX1', cpix1, comment, status)
      call ftgkls(iunit1, 'CPIX1', cpix1, comment, status)
      if (status .ne. 0) then
        status = 0
c        call ftgkys(iunit1, 'CPIX2', cpix1, comment, status)
        call ftgkls(iunit1, 'CPIX2', cpix1, comment, status)
        if(status.eq.0)pha1type2=.TRUE.
        if (status .ne. 0) then
          status=0
c          call ftgkys(iunit1, 'CPIX', cpix1, comment, status)
          call ftgkls(iunit1, 'CPIX', cpix1, comment, status)
          if (status .ne. 0) then
            call fcerr('unable to obtain CPIX keyword value')
            abort=.TRUE.
            go to 999
          endif
        endif
      endif

      call parsebd(cpix1,inobd1,ne,ilbd1,iubd1,ldryrun,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to PARSE CPIX for first input file!')
        call fcecho('Cannot continue CPIX MUST be present!!!!')
        abort=.TRUE.
        goto 999
      endif
      
c     Getting all necessary channel information from the first file.
      
c      Move to the second CHDU (or to extnum+1)
c      to read all pertinent processing information.
      call ftmahd(iunit2,extinfile2+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extinfile2')
        call fcerrm(status)
        status=0
      endif

C Read the CPIX1 or CPIX2 keyward to get the channel bins for iunit1
c      call ftgkys(iunit2, 'CPIX1', cpix2, comment, status)
      call ftgkls(iunit2, 'CPIX1', cpix2, comment, status)
      if (status .ne. 0) then
        status = 0
c        call ftgkys(iunit2, 'CPIX2', cpix2, comment, status)
        call ftgkls(iunit2, 'CPIX2', cpix2, comment, status)
        if(status.eq.0)pha2type2=.TRUE.
        if (status .ne. 0) then
          status=0
c          call ftgkys(iunit2, 'CPIX', cpix2, comment, status)
          call ftgkls(iunit2, 'CPIX', cpix2, comment, status)
          if (status .ne. 0) then
            call fcerr('unable to obtain CPIX keyword value')
            abort=.TRUE.
            go to 999
          endif
        endif
      endif
      
      call parsebd(cpix2,inobd2,ne,ilbd2,iubd2,ldryrun,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to PARSE CPIX for first input file!')
        call fcecho('Cannot continue CPIX MUST be present!!!!')
        abort=.TRUE.
        goto 999
      endif

      if(pha1type2)then
        if(.not.pha2type2)then
          call fcecho(' ')
          call fcecho('Error. Files are of different types!')
          call fcecho('The first file contains MANY columns result')
          call fcecho('and the second contains ONE column result.')
          call fcecho(' ')
          call fcecho('CANNOT MERGE FILES! Aborting...')
          abort=.TRUE.
          goto 999
        else
          call fcecho(' ')
          call fcecho('Files contain MANY column accumulation result.')
          phatype2=.TRUE.
        endif
      else
        if(pha2type2)then
          call fcecho(' ')
          call fcecho('Error. Files are of different types!')
          call fcecho('The first file contains ONE column result')
          call fcecho('and the second contains MANY column result.')
          call fcecho(' ')
          call fcecho('CANNOT MERGE FILES! Aborting...')
          abort=.TRUE.
          goto 999
        else
          call fcecho(' ')
          call fcecho('Files contain ONE column accumulation result.')
          phatype2=.FALSE.
        endif
      endif
      
c     Test to see which file contains the lowest channels
      if(ilbd2(1).lt.iubd1(inobd1))then
        call fcecho(' ')
        call fcecho('Input files were in the wrong order')
        call fcecho('Switching them so that file with lower channels')
        call fcecho('acts as the primary file to be copied to output.')
        call fcecho(' ')
        call fcecho('Continuing to process files.')

        iunit=iunit1
        iunit1=iunit2
        iunit2=iunit

        cpixhold=cpix1
        cpix1=cpix2
        cpix2=cpixhold
        inobdhold=inobd1

        do 10 i=1,inobd1
          ilbdhold(i)=ilbd1(i)
          iubdhold(i)=iubd1(i)
10      continue

        do 20 i=1,inobd2
          ilbd1(i)=ilbd2(i)
          iubd1(i)=iubd2(i)
20      continue

        do 30 i=1,inobdhold
          ilbd2(i)=ilbdhold(i)
          iubd2(i)=iubdhold(i)
30      continue

        inobd1=inobd2
        inobd2=inobdhold
        
      endif

      do 40 i=1,inobd1
        if(ilbd1(i).gt.ilbd2(1).and.ilbd1(i).lt.iubd2(inobd2))then
          call fcecho(' ')
          call fcecho('ERROR...')
          call fcecho('Files overlap in channel ranges!')
          call fcecho('This should NEVER occur!')
          call fcecho('Aborting... Check original input files.')
          abort=.TRUE.
          goto 999
        endif

        if(iubd1(i).gt.ilbd2(1).and.iubd1(i).lt.iubd2(inobd2))then
          call fcecho(' ')
          call fcecho('ERROR...')
          call fcecho('Files overlap in channel ranges!')
          call fcecho('This should NEVER occur!')
          call fcecho('Aborting... Check original input files.')
          abort=.TRUE.
          goto 999
        endif

40    continue
        
c**********************************************************************

      ioutlen1=fcstln(cpix1)
      ioutlen2=fcstln(cpix2)

      call fcecho(' ')
      call fcecho('CPIX containing lower channels:')
      call fcecho(cpix1(:ioutlen1))

      call fcecho(' ')
      call fcecho('CPIX containing upper channels:')
      call fcecho(cpix2(:ioutlen2))

      if(iubd1(inobd1)+1.eq.ilbd2(1))then

        call fcecho(' ')
        call fcecho('Channel specifications are continuous.')
        call fcecho('CPIXs being merged to form new CPIX keyword:')

c       Check to see if the character string is long enough.
        if((ioutlen1+2+ioutlen2).gt.ichars)then
          call fcecho(' ')
          call fcecho('Error! Character string CPIXTOTAL too small!')
          call fcecho('CPIXtotal dimensioned to ichar (2000)')
          call fcecho('Check your CPIX keywords!')
          call fcecho('Aborting...')
          abort=.TRUE.
          goto 999
        endif
        
        cpixtotal(1:ioutlen1)=cpix1
        cpixtotal(ioutlen1+1:ioutlen1+1)=','
        cpixtotal(ioutlen1+2:ioutlen1+2+ioutlen2)=cpix2

        itotal = ioutlen1+ioutlen2+2
        
      else

        call fcecho(' ')
        call fcecho('Channel specfications are NOT continuous.')
        call fcecho('There is a GAP between the CPIXs, we are adding:')

        call cint2char(iubd1(inobd1)+1,clower,ilowlen,abort)
        if(abort)then
          call fcecho(' ')
          call fcecho('Error converting lower boundary to char.')
          goto 999
        endif
        
        call cint2char(ilbd2(1)-1,cupper,iuplen,abort)
        if(abort)then
          call fcecho(' ')
          call fcecho('Error converting upper boundary to char.')
          goto 999
        endif

        cpixtotal(1:ioutlen1)=cpix1
        cpixtotal(ioutlen1+1:ioutlen1+1)=','
        itotal=ioutlen1+2

c       Check to see if the character string is long enough.
        if((itotal+ilowlen+iuplen+ioutlen2+2).gt.ichars)then
          call fcecho(' ')
          call fcecho('Error! Character string CPIXTOTAL too small!')
          call fcecho('CPIXtotal dimensioned to ichar (2000)')
          call fcecho('Check your CPIX keywords!')
          call fcecho('Aborting...')
          abort=.TRUE.
          goto 999
        endif
        
        cpixtotal(itotal:itotal+ilowlen)=clower(:ilowlen)
        itotal=itotal+ilowlen
        cpixtotal(itotal:itotal)='~'
        itotal=itotal+1
        cpixtotal(itotal:itotal+iuplen)=cupper(:iuplen)
        itotal=itotal+iuplen
        cpixtotal(itotal:itotal)=','
        itotal=itotal+1
        cpixtotal(itotal:itotal+ioutlen2)=cpix2

        itotal = itotal+ioutlen2
        irowsadded = irowsadded+1
        
        cinsert(1:ilowlen)=clower(:ilowlen)
        cinsert(ilowlen+1:ilowlen+1)='~'
        cinsert(ilowlen+2:ilowlen+2+iuplen)=cupper(:iuplen)

        call fcecho(cinsert(:ilowlen+2+iuplen))

        call fcecho(' ')
        call fcecho('The final CPIX keyword created is:')

      endif

c     How many characters are in CPIXTOTAL?
      ichars=itotal

      call fcecho(cpixtotal(:ichars))

      irow1=inobd1
      irow2=inobd2
      irowsadded=irowsadded+irow2
      
c      print*,'inobd1 is ',inobd1
c      do 50 i=1,inobd1
c        print*,ilbd1(i),iubd1(i)
c50    continue

999   continue
      
      status=0

      return
      end
      

c**********************************************************************
c
c
c
c
c
c**********************************************************************

      subroutine xtepha_merge_data(iunit1,extinfile1,
     &   iunit2,extinfile2,ounit,extoufile,
     &   cpixtotal,ichars,irow1,irow2,irowsadded,phatype2,
     &   slop,abort,status)

      implicit none
      
      integer iunit1, extinfile1, iunit2, extinfile2, ichars,
     &   ounit, extoufile, irow1, irow2, irowsadded, status
      character*(*) cpixtotal
      character(160) file1(100),file2(100)
      character(70) comm
      character(8) keyname
      logical phatype2, abort, lrate
      double precision slop,tempval2,tempval3,
     &   exposure1,exposure2,exposure,exposlop,
     &   tstart1,tstart2,tstrslop,tstop1,tstop2,
     &   tstpslop,tstart,tstop
      integer irowval,itemp,colnum1,colnum2,xtend
      logical flgval, anynul, lexact,
     &   lagree

      integer i, j, nultyp, ir,
     &   nulval, ival1, ival2, naxis2

      flgval=.FALSE.
      anynul=.FALSE.
      lrate=.FALSE.
      nultyp=1
      nulval=0
      irowval=irow1
      exposlop=0.0D0
      tstrslop=0.0D0
      tstpslop=0.0D0
      
c      print*,'Into fits_file_merge_data',irow1,irow2,irowsadded,irowval

c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(iunit1,extinfile1+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extinfile1')
        call fcerrm(status)
        abort=.TRUE.
        status=0
      endif
      
c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(iunit2,extinfile2+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extinfile2')
        call fcerrm(status)
        abort=.TRUE.
        status=0
      endif

c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(ounit,extoufile+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extoufile')
        call fcerrm(status)
        abort=.TRUE.
        status=0
      endif
      
      
c**********************************************************************

      call filen_find(iunit1,'FILEN',ival1,file1)
      call filen_find(iunit2,'FILEN',ival2,file2)

      call fthdef(ounit,ival2,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not add FILEN space to outfile')
        call fcecho('Continuing...')
        status=0
      endif
      
      call xteftpklns(ounit,'FILEN',ival1+1,ival2,file2
     &   ,'Input files input to produce this file&',status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not add FILEN values to outfile')
        call fcecho('Continuing...')
        status=0
      endif

      if(phatype2)then
c
c         call ftmkys(ounit,'CPIX2',cpixtotal(:ichars),'&',status)
c
c     MJT (20Jan99) modify keyword routine in CFITSIO is limited to
c     writing 68-characters so to write a long keyword we have to
c     delete the existing keywords and then write long keywords.
         call ftdkey(ounit,'CPIX2',status)
         call ftpkls(ounit,'CPIX2',cpixtotal(:ichars),
     &        'Channel binning of CHANNEL column',status)
         call ftplsw(ounit,status)
         if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not copy new CPIX2 to outfile')
            call fcecho('Continuing...')
            status=0
         endif
      else
c         call ftmkys(ounit,'CPIX1',cpixtotal(:ichars),'&',status)
         call ftdkey(ounit,'CPIX1',status)
         call ftpkls(ounit,'CPIX1',cpixtotal(:ichars),
     &        'Channel binning of CHANNEL column',status)
         call ftplsw(ounit,status)
         if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not copy new CPIX1 to outfile')
            call fcecho('Continuing...')
            status=0
         endif
      endif

c**********************************************************************
      call ftgkyd(iunit1,'EXPOSURE',exposure1,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error. Could not read EXPOSURE from input file1.')
        call fcecho('Your file was NOT created by SA(SE)EXTRCT or')
        call fcecho('or has been modified so this task cannot work!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call ftgkyd(iunit2,'EXPOSURE',exposure2,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error. Could not read EXPOSURE from input file2.')
        call fcecho('Your file was NOT created by SA(SE)EXTRCT or')
        call fcecho('or has been modified so this task cannot work!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      if(exposure1.ne.exposure2)then
        if(exposure1.lt.exposure2)then
          exposlop=exposure1*slop
        else
          exposlop=exposure2*slop
        endif
      endif

      if((exposure1.le.(exposure2+exposlop)).and.
     &   (exposure1.ge.(exposure2-exposlop)))then

        exposure=exposure1
        
      else
        call fcecho(' ')
        call fcecho('EXPOSURE differs by more than SLOP value.')
        call fcecho('Using the smaller of the two for EXPOSURE!')
        call fcecho('Check your input files to be sure they are')
        call fcecho('compatible!!!')

        call fthdef(ounit,1,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error alloting space for EXPOSURE warning!')
          abort=.TRUE.
          goto 999
        endif

        call ftphis(ounit,'EXPOSURES for input files differed'
     &     ,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error writing EXPOSURE warning history keyword!')
          abort=.TRUE.
          goto 999
        endif
        
        if(exposure1.lt.exposure2)then
          exposure=exposure1
        else
          exposure=exposure2
        endif
        
      endif
      
      call ftmkyd(ounit,'EXPOSURE',exposure,14,'&',status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not modify EXPOSURE value in output.')
        abort=.TRUE.
        goto 999
      endif
c**********************************************************************
c**********************************************************************

      call ftgkyd(iunit1,'TSTART',tstart1,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error. Could not read TSTART from input file1.')
        call fcecho('Your file was NOT created by SA(SE)EXTRCT or')
        call fcecho('or has been modified so this task cannot work!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call ftgkyd(iunit2,'TSTART',tstart2,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error. Could not read TSTART from input file2.')
        call fcecho('Your file was NOT created by SA(SE)EXTRCT or')
        call fcecho('or has been modified so this task cannot work!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      if(tstart1.ne.tstart2)then
        if(tstart1.lt.tstart2)then
          tstrslop=tstart1*slop
        else
          tstrslop=tstart2*slop
        endif
      endif

      if((tstart1.le.(tstart2+tstrslop)).and.
     &   (tstart1.ge.(tstart2-tstrslop)))then

        tstart=tstart1
        
      else
        call fcecho(' ')
        call fcecho('TSTART differs by more than SLOP value.')
        call fcecho('Using the smaller of the two for TSTART!')
        call fcecho('Check your input files to be sure they are')
        call fcecho('compatible!!!')

        call fthdef(ounit,1,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error alloting space for TSTART warning!')
          abort=.TRUE.
          goto 999
        endif

        call ftphis(ounit,'TSTART for input files differed'
     &     ,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error writing TSTART warning history keyword!')
          abort=.TRUE.
          goto 999
        endif
        
        if(tstart1.lt.tstart2)then
          tstart=tstart1
        else
          tstart=tstart2
        endif
        
      endif

c**********************************************************************      

      call ftgkyd(iunit1,'TSTOP',tstop1,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error. Could not read TSTOP from input file1.')
        call fcecho('Your file was NOT created by SA(SE)EXTRCT or')
        call fcecho('or has been modified so this task cannot work!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call ftgkyd(iunit2,'TSTOP',tstop2,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error. Could not read TSTOP from input file2.')
        call fcecho('Your file was NOT created by SA(SE)EXTRCT or')
        call fcecho('or has been modified so this task cannot work!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      if(tstop1.ne.tstop2)then
        if(tstop1.lt.tstop2)then
          tstpslop=tstop1*slop
        else
          tstpslop=tstop2*slop
        endif
      endif

      if((tstop1.le.(tstop2+tstpslop)).and.
     &   (tstop1.ge.(tstop2-tstpslop)))then

        tstop=tstop1
        
      else
        call fcecho(' ')
        call fcecho('TSTOP differs by more than SLOP value.')
        call fcecho('Using the smaller of the two for TSTOP!')
        call fcecho('Check your input files to be sure they are')
        call fcecho('compatible!!!')

        call fthdef(ounit,1,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error alloting space for TSTOP warning!')
          abort=.TRUE.
          goto 999
        endif

        call ftphis(ounit,'TSTOP for input files differed'
     &     ,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error writing TSTOP warning history keyword!')
          abort=.TRUE.
          goto 999
        endif
        
        if(tstop1.lt.tstop2)then
          tstop=tstop1
        else
          tstop=tstop2
        endif
        
      endif

c**********************************************************************      

      lagree=.FALSE.
      
      call chkkeywrds(iunit1,iunit2,'MISSION',lagree)
      if(.not.lagree)then
        call fcecho(' ')
        call fcecho('MISSION keywords values disagree for files!')
        call fcecho('Check input files for compatibility!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      lagree=.FALSE.
      
      call chkkeywrds(iunit1,iunit2,'TELESCOP',lagree)
      if(.not.lagree)then
        call fcecho(' ')
        call fcecho('TELESCOP keywords values disagree for files!')
        call fcecho('Check input files for compatibility!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif
      
      lagree=.FALSE.
      
      call chkkeywrds(iunit1,iunit2,'INSTRUME',lagree)
      if(.not.lagree)then
        call fcecho(' ')
        call fcecho('INSTRUME keywords values disagree for files!')
        call fcecho('Check input files for compatibility!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      lagree=.FALSE.
      
      call chkkeywrds(iunit1,iunit2,'OBJECT',lagree)
      if(.not.lagree)then
        call fcecho(' ')
        call fcecho('OBJECT keywords values disagree for files!')
        call fcecho('Check input files for compatibility!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      lagree=.FALSE.
      
      call chkkeywrds(iunit1,iunit2,'RA_OBJ',lagree)
      if(.not.lagree)then
        call fcecho(' ')
        call fcecho('RA_OBJ keywords values disagree for files!')
        call fcecho('Check input files for compatibility!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      lagree=.FALSE.
      
      call chkkeywrds(iunit1,iunit2,'DEC_OBJ',lagree)
      if(.not.lagree)then
        call fcecho(' ')
        call fcecho('DEC_OBJ keywords values disagree for files!')
        call fcecho('Check input files for compatibility!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      
c**********************************************************************      
      
      lexact=.FALSE.
      call ftgcno(iunit1,lexact,'RATE',colnum1,status)
      if(status.eq.0)then
        lrate=.TRUE.
      else
        status=0
      endif
      
      call ftgcno(iunit2,lexact,'RATE',colnum2,status)
      if(status.eq.0)then
        if(.not.lrate)then
          call fcecho(' ')
          call fcecho('Error. One input file is RATE, other is not!!!')
          call fcecho('Aborting....')
          abort=.TRUE.
          goto 999
        endif
      else
        if(lrate)then
          call fcecho(' ')
          call fcecho('Error. One input file is RATE, other is not!!!')
          call fcecho('Aborting....')
          abort=.TRUE.
          goto 999
        endif
      endif

      status=0
      
      if(phatype2)then

        call fcecho(' ')
        call fcecho('Spectral files are TYPE II')
        call fcecho(' ')
        call fcecho('Modifying keywords and setting up outfile.')
        
        if(.not.lrate)then

          call ftgcno(iunit1,lexact,'COUNTS',colnum1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('FILE1 not a RATE file and')
            call fcecho('COUNTS column not found')
            call fcecho('Cannot continue. Aborting...')
            abort=.TRUE.
            goto 999
          endif
      
          call ftgcno(iunit2,lexact,'COUNTS',colnum2,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('FILE2 not a RATE file and')
            call fcecho('COUNTS column not found')
            call fcecho('Cannot continue. Aborting...')
            abort=.TRUE.
            goto 999
          endif
        endif
        
        call ftdcol(ounit,2,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error deleting CHANNEL column from outfile')
          abort=.TRUE.
          goto 999
        endif

        call ftnkey(irow1+irowsadded,'I',keyname,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error creating new TFORM for outfile')
          abort=.TRUE.
          goto 999
        endif

        call fticol(ounit,2,'CHANNEL',keyname,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error inserting new column 2 in outfile')
          abort=.TRUE.
          goto 999
        endif

        call fthdef(ounit,1,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Creating space for TUNIT2 keyword')
          abort=.TRUE.
          goto 999
        endif

        call ftpkys(ounit,'TUNIT2','channel',
     &     'physical unit of field',status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error inserting new TUNIT2 in outfile')
          abort=.TRUE.
          goto 999
        endif
        
        if(lrate)then
          keyname=' '
        
          call ftdcol(ounit,3,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error deleting COUNTS column from outfile')
            abort=.TRUE.
            goto 999
          endif
          
          call ftnkey(irow1+irowsadded,'D',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error creating new TFORM for outfile')
            abort=.TRUE.
            goto 999
          endif

          call fticol(ounit,3,'COUNTS',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new column 3 in outfile')
            abort=.TRUE.
            goto 999
          endif

          call fthdef(ounit,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Creating space for TUNIT3 keyword')
            abort=.TRUE.
            goto 999
          endif

          call ftpkys(ounit,'TUNIT3','count',
     &       'physical unit of field',status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new TUNIT3 in outfile')
            abort=.TRUE.
            goto 999
          endif

          keyname=' '
        
          call ftdcol(ounit,4,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error deleting STAT_ERR column from outfile')
            abort=.TRUE.
            goto 999
          endif
          
          call ftnkey(irow1+irowsadded,'D',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error creating new TFORM for outfile')
            abort=.TRUE.
            goto 999
          endif

          call fticol(ounit,4,'STAT_ERR',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new column 4 in outfile')
            abort=.TRUE.
            goto 999
          endif

          call fthdef(ounit,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Creating space for TUNIT4 keyword')
            abort=.TRUE.
            goto 999
          endif

          call ftpkys(ounit,'TUNIT4','count',
     &       'physical unit of field',status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new TUNIT4 in outfile')
            abort=.TRUE.
            goto 999
          endif
          
        else
          keyname=' '
        
          call ftdcol(ounit,3,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error deleting RATE column from outfile')
            abort=.TRUE.
            goto 999
          endif
          
          call ftnkey(irow1+irowsadded,'D',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error creating new TFORM for outfile')
            abort=.TRUE.
            goto 999
          endif

          call fticol(ounit,3,'RATE',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new column 3 in outfile')
            abort=.TRUE.
            goto 999
          endif

          call fthdef(ounit,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Creating space for TUNIT3 keyword')
            abort=.TRUE.
            goto 999
          endif

          call ftpkys(ounit,'TUNIT3','count/s',
     &       'physical unit of field',status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new TUNIT3 in outfile')
            abort=.TRUE.
            goto 999
          endif

          keyname=' '
        
          call ftdcol(ounit,4,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error deleting STAT_ERR column from outfile')
            abort=.TRUE.
            goto 999
          endif
          
          call ftnkey(irow1+irowsadded,'D',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error creating new TFORM for outfile')
            abort=.TRUE.
            goto 999
          endif

          call fticol(ounit,4,'STAT_ERR',keyname,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new column 4 in outfile')
            abort=.TRUE.
            goto 999
          endif

          call fthdef(ounit,1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Creating space for TUNIT4 keyword')
            abort=.TRUE.
            goto 999
          endif

          call ftpkys(ounit,'TUNIT4','count/s',
     &       'physical unit of field',status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error inserting new TUNIT4 in outfile')
            abort=.TRUE.
            goto 999
          endif

        endif

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        

        call fcecho('Finished setting up new columns and keywords.')
        call fcecho(' ')
        call fcecho('Copying data from input files to output file.')
        
        call ftgkyj(ounit,'NAXIS2',naxis2,comm,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error.. Could not get NAXIS2 value.')
          call fcecho('Aborting...')
          abort=.TRUE.
          goto 999
        endif

        irowval=-1
        
        do 50 i=1,naxis2

          irowval=-1
          
          do 110 j=1,irow1

            irowval=irowval+1
            
c           call ftgcli(iunit1,2,i,j,1,1,nultyp,nulval,
c    &         itemp,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvi since ftgcli should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvi(iunit1,2,i,j,1,nulval,itemp,anynul,status)

            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting irowval, aborting.')
              abort=.TRUE.
              goto 999
            endif
            
c           call ftgcld(iunit1,3,i,j,1,1,nultyp,nulval,
c    &         tempval2,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvd(iunit1,3,i,j,1,0.0d0,tempval2,anynul,status)

            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting tempval2 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

c           call ftgcld(iunit1,4,i,j,1,1,nultyp,nulval,
c    &         tempval3,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvd(iunit1,4,i,j,1,0.0d0,tempval3,anynul,status)

            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting tempval3 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

            if(lrate)then
              tempval2=(tempval2*exposure1)
              tempval3=(dsqrt(tempval2))/exposure
              tempval2=tempval2/exposure
            endif

            call ftpclj(ounit,2,i,j,1,irowval,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding irowval values, aborting.')
              abort=.TRUE.
              goto 999
            endif
            
            call ftpcld(ounit,3,i,j,1,tempval2,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding tempval2 values, aborting.')
              abort=.TRUE.
              goto 999
            endif
            
            call ftpcld(ounit,4,i,j,1,tempval3,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding tempval3 values, aborting.')
              abort=.TRUE.
              goto 999
            endif
            
110       continue

          ir=0
          
          if(irow2.ne.irowsadded)then
            tempval2=0.0d0
            irowval=irowval+1
            ir=1
            call ftpclj(ounit,2,i,ir+irow1,1,irowval,status)
            call ftpcld(ounit,3,i,ir+irow1,1,tempval2,status)
            call ftpcld(ounit,4,i,ir+irow1,1,tempval2,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Error adding row of zero values, aborting.')
              abort=.TRUE.
              goto 999
            endif
          endif


          do 210 j=1,irow2
            irowval=irowval+1
          
c           call ftgcld(iunit2,3,i,j,1,1,nultyp,nulval,
c    &         tempval2,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvd(iunit2,3,i,j,1,0.0d0,tempval2,anynul,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting tempval2 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

c           call ftgcld(iunit2,4,i,j,1,1,nultyp,nulval,
c    &         tempval3,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvd(iunit2,4,i,j,1,0.0d0,tempval3,anynul,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting tempval3 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

            call ftpclj(ounit,2,i,j+irow1+ir,1,irowval,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding irowval, aborting.')
              call fcerrm(status)
              abort=.TRUE.
              goto 999
            endif

            if(lrate)then
              tempval2=(tempval2*exposure2)
              tempval3=(dsqrt(tempval2))/exposure
              tempval2=tempval2/exposure
            endif
          
            call ftpcld(ounit,3,i,j+irow1+ir,1,tempval2,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding tempval2 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

            call ftpcld(ounit,4,i,j+irow1+ir,1,tempval3,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding tempval3 values, aborting.')
              abort=.TRUE.
              goto 999
            endif
210       continue

50      continue
          


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
      else

        if(.not.lrate)then
          call ftgcno(iunit1,lexact,'COUNTS',colnum1,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('FILE1 not a RATE file and')
            call fcecho('COUNTS column not found')
            call fcecho('Cannot continue. Aborting...')
            abort=.TRUE.
            goto 999
          endif
      
          call ftgcno(iunit2,lexact,'COUNTS',colnum2,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('FILE2 not a RATE file and')
            call fcecho('COUNTS column not found')
            call fcecho('Cannot continue. Aborting...')
            abort=.TRUE.
            goto 999
          endif
        endif

c       Spectral files processed are type I.

        call fcecho(' ')
        call fcecho('Spectral files are TYPE I')
        call fcecho(' ')
        call fcecho('Copying data to output file.')

c       Add the correct number of rows to allow data to be input.
        call ftirow(ounit,irow1,irowsadded,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error allocating space for data in outfile.')
          call fcecho('May be out of space on device. Aborting...')
          call fcerr('Cannot continue. Closing output file.')
          status=0
          call ftpcks(ounit,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not update chksum keyword')
            status=0
          endif
          call ftclos(ounit,status)
          abort=.TRUE.
          goto 999
        endif

        if(lrate)then
          do 100 j=1,irow1

c           call ftgcli(iunit1,1,j,1,1,1,nultyp,nulval,
c    &       itemp,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvi since ftgcli should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvi(iunit1,1,j,1,1,nulval,itemp,anynul,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting irowval, aborting.')
              abort=.TRUE.
              goto 999
            endif
          
c           call ftgcld(iunit1,2,j,1,1,1,nultyp,nulval,
c    &         tempval2,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvd(iunit1,2,j,1,1,0.0d0,tempval2,anynul,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting tempval2 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

c           call ftgcld(iunit1,3,j,1,1,1,nultyp,nulval,
c    &         tempval3,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
            call ftgcvd(iunit1,3,j,1,1,0.0d0,tempval3,anynul,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error getting tempval3 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

            tempval2=(tempval2*exposure1)
            tempval3=(dsqrt(tempval2))/exposure
            tempval2=tempval2/exposure
            
c            tempval2=(tempval3*exposure1)/exposure   
            
            call ftpcld(ounit,2,j,1,1,tempval2,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding tempval2 values, aborting.')
              abort=.TRUE.
              goto 999
            endif

            call ftpcld(ounit,3,j,1,1,tempval3,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcerr('Error adding tempval3 values, aborting.')
              abort=.TRUE.
              goto 999
            endif
            
100       continue

        endif
        
        if(irow2.ne.irowsadded)then
          irowval=irowval+1
          tempval2=0.0d0
          call ftpclj(ounit,1,irowval,1,1,irowval-1,status)
          call ftpcld(ounit,2,irowval,1,1,tempval2,status)
          call ftpcld(ounit,3,irowval,1,1,tempval2,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error adding row of zero values, aborting.')
            abort=.TRUE.
            goto 999
          endif
          
        endif
        
        do 200 j=1,irow2
          irowval=irowval+1

c         call ftgcli(iunit2,1,j,1,1,1,nultyp,nulval,
c    &       itemp,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvi since ftgcli should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
          call ftgcvi(iunit2,1,j,1,1,nulval,itemp,anynul,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcerr('Error getting irowval, aborting.')
            abort=.TRUE.
            goto 999
          endif
          
c         call ftgcld(iunit2,2,j,1,1,1,nultyp,nulval,
c    &       tempval2,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
          call ftgcvd(iunit2,2,j,1,1,0.0d0,tempval2,anynul,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcerr('Error getting tempval2 values, aborting.')
            abort=.TRUE.
            goto 999
          endif

c         call ftgcld(iunit2,3,j,1,1,1,nultyp,nulval,
c    &       tempval3,flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
          call ftgcvd(iunit2,3,j,1,1,0.0d0,tempval3,anynul,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcerr('Error getting tempval3 values, aborting.')
            abort=.TRUE.
            goto 999
          endif

          itemp=irowval-1
          
          call ftpclj(ounit,1,irowval,1,1,itemp,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcerr('Error adding irowval, aborting.')
            call fcerrm(status)
            abort=.TRUE.
            goto 999
          endif

          if(lrate)then
            tempval2=(tempval2*exposure2)
            tempval3=(dsqrt(tempval2))/exposure
            tempval2=tempval2/exposure
          endif
          
          call ftpcld(ounit,2,irowval,1,1,tempval2,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcerr('Error adding tempval2 values, aborting.')
            abort=.TRUE.
            goto 999
          endif

          call ftpcld(ounit,3,irowval,1,1,tempval3,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcerr('Error adding tempval3 values, aborting.')
            abort=.TRUE.
            goto 999
          endif
200     continue

      endif



c**********************************************************************
      
c     Let's modify the DETCHANS, and TLMAX1 to the correct values.
      call ftmkyj(ounit,'DETCHANS',irow1+irowsadded,'&',status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not modify DETCHANS value in output.')
        abort=.TRUE.
        goto 999
      endif

      if(phatype2)then
        call fthdef(ounit,2,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho
     &       ('Error allocating space to write TLMIN2 and TLMAX2')
        endif
        
        call ftpkyj(ounit,'TLMIN2',0,
     &     'Lowest Legal channel number',status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not modify TLMIN2 value in output.')
          abort=.TRUE.
          goto 999
        endif

        call ftpkyj(ounit,'TLMAX2',irow1+irowsadded-1,
     &     'Highest Legal channel number',status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not modify TLMAX2 value in output.')
          abort=.TRUE.
          goto 999
        endif
      else
        call ftmkyj(ounit,'TLMAX1',irow1+irowsadded-1,'&',status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not modify TLMAX1 value in output.')
          abort=.TRUE.
          goto 999
        endif
      endif
      
c**********************************************************************      

      call fcecho('Completed creating main data extension.')
      call fcecho(' ')
      call fcecho('Now proceeding to deal with the GTI information.')
      
999   continue
      
      return
      end
      
      
      subroutine xtepha_merge_gtis(iunit1,extinfile1,
     &   iunit2,extinfile2,ounit,extoufile,
     &   timeint_fil,abort,status)

      implicit none

      character*(*) timeint_fil
      integer iunit1, extinfile1, iunit2, extinfile2,
     &   ounit, extoufile, status, ne
      logical abort, lerror
      parameter (ne = 100)

      character(80) comm
      integer xtend, fcstln, outlen, ounit2,
     &   naxis1, naxis2, nultyp, nulval, i, j, igti
      double precision tstart1(ne), tstart2(ne),
     &   tstop1(ne), tstop2(ne), tstart(ne), tstop(ne),
     &   tdlt
      logical flgval, anynul


      flgval=.FALSE.
      anynul=.FALSE.
      nultyp=1
      nulval=0
      igti=1

      call dinitial(ne,tstart1)
      call dinitial(ne,tstart2)
      call dinitial(ne,tstop1)
      call dinitial(ne,tstop2)
      
      outlen=fcstln(timeint_fil)

      lerror=.FALSE.
      
c     First let's move through the first input file by incrementing
c the extension number by 1.
      call ftmrhd(iunit1,1,xtend,status)

      if(status.eq.0)then
        call ftpcks(ounit,status)
        if(status.ne.0)status=0
      endif
      
c     Create a new extension in the output file.
      call ftcrhd(ounit,status)

c     Copy the extension from IN_FIL1 to the OU_FIL
      call ftcopy(iunit1,ounit,0,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('No GTI information in IN_FIL1.')
        call fcecho('Terminating GTI work.')
        status=0
        return
      endif

c     Move through the second input file by incrementing the extension
c number by 1.
      call ftmrhd(iunit2,1,xtend,status)

c     Now that we have copied the GTI extension from the first infile to
c the outfile, we have to perform some checks to be sure that the GTI's
c in the input files were the same. If they weren't we will have to do
c more work.  

      call ftgkyj(iunit1,'NAXIS2',naxis1,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error.. Could not get NAXIS2 value from IN_FIL1.')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call ftgkyj(iunit2,'NAXIS2',naxis2,comm,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error.. Could not get NAXIS2 value from IN_FIL2.')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      if(naxis1.ne.naxis2)then
        lerror=.TRUE.
        call fcecho(' ')
        call fcecho('Error in merging GTIs for input files.')
        call fcecho('The number of GTIs in the files differ!')
        call fcecho('This should NOT occur. They should be equal')
        call fcecho('as should the values for START and STOP.')

      endif
      
      do 100 i=1,naxis1

c       call ftgcld(iunit1,1,i,1,1,1,nultyp,nulval,
c    &     tstart1(i),flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(iunit1,1,i,1,1,0.0d0,tstart1(i),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcerr('Error getting TSTART1 values, aborting.')
          abort=.TRUE.
          goto 999
        endif

c       call ftgcld(iunit1,2,i,1,1,1,nultyp,nulval,
c    &     tstop1(i),flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(iunit1,2,i,1,1,0.0d0,tstop1(i),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcerr('Error getting TSTOP1 values, aborting.')
          abort=.TRUE.
          goto 999
        endif
        
100   continue

      do 200 i=1,naxis2

c       call ftgcld(iunit2,1,i,1,1,1,nultyp,nulval,
c    &     tstart2(i),flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(iunit2,1,i,1,1,0.0d0,tstart2(i),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcerr('Error getting TSTART2 values, aborting.')
          abort=.TRUE.
          goto 999
        endif

c       call ftgcld(iunit2,2,i,1,1,1,nultyp,nulval,
c    &     tstop2(i),flgval,anynul,status)
c      MJT -- 02Jan98:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(iunit2,2,i,1,1,0.0d0,tstop2(i),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcerr('Error getting TSTOP2 values, aborting.')
          abort=.TRUE.
          goto 999
        endif
        
200   continue

      
c     If an error occurs in processing the above information we will have
c to create a new output ASCII file that can be input at the "timeint"
c prompt for SA(SE)EXTRCT.
      if(lerror)then

        call fcecho(' ')
        call fcecho
     &     ('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        call fcecho(' ')        
        call fcecho
     &     ('An ambiguity was detected in processing the GTI info.')
        call fcecho(' ')
        call fcecho('The GTI in the output file is just a copy of')
        call fcecho('the GTI in the first input file!')
        call fcecho(' ')
        call fcecho('Creating an ASCII output file:')
        call fcecho(timeint_fil(:outlen))
        call fcecho(' ')
        call fcecho('The extractor code SA(SE)EXTRCT')
        call fcecho('should be re-run on the original data with')
        call fcecho('this file as input for the TIMEINT parameter')
        call fcecho('using @filename to input these values. ')
        call fcecho(' ')        
        call fcecho('See the pertinent HELP file for more information')
        call fcecho('on SA(SE)EXTRCT.')
        call fcecho(' ')        
        call fcecho
     &     ('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

        
c      Assign a unit file number used in outputting file.
        call ftgiou(ounit2,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error getting TIMEINT_FIL unit number')
          call fcecho('Setting to logical unit 14')
          status=0
          ounit2=14
        endif

        call faopen(iunit2,timeint_fil(:outlen),2,133,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error opening TIMEINT_FIL!')
          call fcecho('Check hidden parameter timeint_fil.')
          call fcecho('Cannot continue. Aborting...')
          abort=.TRUE.
          goto 999
        endif

        tdlt=1.0d-9
        
        do 300 i=1,naxis1
          do 400 j=1,naxis2
            if(j.eq.naxis2)then
              tstart2(j+1)=tstop2(j)+1.0d0
              tstop2(j+1)=tstop2(j)+2.0d0
            endif

            print*,'tstart1(i) and tstart2(j)',
     &         tstart1(i),tstart2(j),tstart2(j+1),
     &         tstart1(i)-tstart2(j),' garf '
            
            if((tstart1(i)+tdlt).ge.tstart2(j).and.
     &         tstart1(i).lt.tstart2(j+1).and.
     &         tstart1(i).lt.tstop2(j))then
              tstart(igti)=tstart1(i)
            elseif(tstart1(i).le.(tstart2(j)+tdlt).and.
     &           tstart1(i).lt.tstart2(j+1).and.
     &           tstart1(i).lt.tstop2(j))then
              tstart(igti)=tstart2(j)
            endif

            print*,'tstop1(i) and tstop2(j)',
     &         tstop1(i),tstop2(j),tstop2(j+1),
     &         tstop1(i)-tstop2(j),' garf '
            
            if((tstop1(i)+tdlt).ge.tstop2(j).and.
     &         tstop1(i).lt.tstop2(j+1).and.
     &         tstop1(i).lt.tstart(j+1))then
              tstop(igti)=tstop2(j)
              igti=igti+1
            elseif(tstop1(i).le.(tstop2(j)+tdlt).and.
     &           tstop1(i).lt.tstop2(j+1).and.
     &           tstop1(i).lt.tstart2(j+1))then
              tstop(igti)=tstop1(j)
              igti=igti+1
            endif
400       continue
300     continue
        
        do 500 i=1,igti-1
          write(iunit2,1001)tstart(i),tstop(i)
500     continue
        
        close(iunit2)

      endif
      

999   continue

      if(lerror)then
        call ftfiou(ounit2,status)
        if(status.ne.0)then
          call fcecho('Error freeing TIMEINT_FIL unit number')
          status=0
        endif
      endif

      if(.not.lerror)then
        call fcecho('Processed and stored GTI information.')
      endif
      
      return
      
1001  format(2x,1PE21.15,5x,1PE21.15)

      end
