c**********************************************************************
c
c
c
c
c
c**********************************************************************

      subroutine fits_file_init(ounit,ou_fil,abort,status)

      character*(*) ou_fil
      integer status,ounit
      logical abort

      character*(160) tmpfile(2),oufile
      integer outlen,fcstln,extoufile

c     Let's get everything set up for the output file.
      no=0
      call fcgcls(ou_fil,tmpfile,no,abort)
      if(no.ne.1)then
        call fcecho(' ')
        call fcecho('Error!!! More than ONE ou_fil specified!')
        call fcecho('Cannot handle more than one file!!!')
        call fcecho('Aborting....')
        abort=.TRUE.
        goto 999
      endif

      call fcpars(tmpfile(1),oufile,extoufile,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error parsing OU_FIL parameter!')
        call fcecho('Check your parameter file!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      ou_fil = oufile
      
      if (extoufile.lt.1) extoufile=1
      outlen=fcstln(oufile)
      
      call ffinit(ounit,oufile(:outlen),status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error cannot create OU_FIL')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call fcecho(' ')
      call fcecho('Output file successfully initialized was:')
      call fcecho(oufile(:outlen))

999   continue
      
      return
      end

      
