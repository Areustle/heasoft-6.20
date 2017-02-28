      subroutine fheader(unit,knam,atcredate,mdlname,status)

C
C   File Name:    fheader.f
C   Author:       W.T. Bridgman
C   Date:         January 1999
C   Abstract:     Routines for writing a stardard primary FITS
C                 header for XSTAR output files.
C
C     Create a FITS file with an empty primary header
C     nrhs columns and nrows row
C
C     Parameters:
C        unit    integer            File unit number
C        knam    char*16            File name to create
C        mdlname char*30            Model name for this run
C        status  integer            Returned status code
C
      implicit none
c
      character(16) knam, filename
      character(30) mdlname
c     the atomic data creation date
      character(63) atcredate
      integer unit, status
      integer lun11

      integer bitpix,naxis,naxes(2),group
      logical simple,extend
      integer blocksize

      status=0
c
      filename=knam

C     Delete the file if it already exists, so we can recreate it
      call deletefile(filename,status)
c      write (6,*)'in fheader after deletefile',filename,status

C     Get an unused Logical Unit Number to use to open the FITS file
c      call ftgiou(unit,status)
      call getlunx(unit)
c      write (6,*)'in fheader after getlun',unit,status

C     open the FITS file, with write access
      lun11=6
c      write (lun11,*)'opening fits unit ',unit, filename
      blocksize=1
      call ftinit(unit,filename,blocksize,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c      if (status .gt. 0)stop


C     initialize parameters for primary array
      simple=.true.
      bitpix=16
      naxis=0
      naxes(1)=0
      naxes(2)=0
      extend=.true.
      group=1
C     write the required primary header keywords
      call ftphpr(unit,simple,bitpix,naxis,naxes,0,group,extend,status)
      if (status .gt. 0)call printerror(lun11,status)

C     now add additional keywords
      call ftpcom(unit,'***********************************',status)
      call ftpkys(unit,'CREATOR','XSTAR version 2.39',
     $ 'Program which generated this file',status)
      if (status .gt. 0)call printerror(lun11,status)

C     Extract the system date
      call ftpdat(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

C     Save run-specific information
      call ftpkys(unit,'MODEL',mdlname,'Model name for this run',status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpkys(unit,'ATDATA',atcredate,
     $  'Atomic data creation date',status)
      if (status .gt. 0)call printerror(lun11,status)


      return
      end
