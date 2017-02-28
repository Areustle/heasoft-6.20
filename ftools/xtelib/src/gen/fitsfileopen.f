c**********************************************************************
c
c
c
c
c
c**********************************************************************

      subroutine fits_file_open(iunit, infile, extinfile, abort,
     &   status)

      character*(*) infile
      character(160) infile1
      integer iunit, extinfile, status
      logical abort

      integer outlen, fcstln, block

      call fcpars(infile,infile1,extinfile1,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error parsing IN_FILE!')
        call fcecho(infile)
        call fcecho('Check your parameter file!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

c     Return the same file input minus any extension information.
      infile = infile1
      
      call fcecho(' ')
      call fcecho('File opened is:')
      outlen=fcstln(infile)
      infile=infile(:outlen)
      call fcecho(infile)
      
c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the 'extnum' and if it is less than 2 we
c      force it to 2. 
      if (extinfile.lt.1) extinfile=1
            
c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.
      call ftopen(iunit,infile1,0,block,status)
      if(status.ne.0)then
        call fcerr('Failure to open file - aborting')
        call fcerrm(status)
        call fcecho(infile)
        status=0
        call ftclos(iunit,status)
        abort = .TRUE.
        return
      endif

999   continue
      
      return
      end

