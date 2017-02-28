      subroutine fparmlist(unit,hdunum,mdlname,npar,parname,partype,
     $                    parval,parcomm,nloopctl,status,lun11)
c
c     author: T. Bridgman
c     parameters:
c        unit    integer            file unit number
c        hdunum  integer            number of last hdu written
c        mdlname char*30            model name for this run
c        npar    integer            number of parameters passed
c        parname char*20(999)       parameter name
c        partype char*10(999)       parameter type
c        parval  real(999)          parameter values converted to reals
c        parcomm char*30(999)       parameter comments & string values
c        nloopctl integer           loop control parameter
c        status  integer            returned status code
c
      implicit none
c     passed parameters
      character(30) mdlname
      integer unit, status, hdunum, npar, nloopctl
      character(20) parname(55)
      character(10) partype(55)
      real*8 parval(55)
      real parval4(55)
      character(30) parcomm(55)
c     parameter info
      integer idat1(6000000)   !jg
      integer lun11
      integer tfields,nrows,varidat
      character(16) ttype(5),tform(5),tunit(5)
      integer colnum,frow,felem,hdutype
      integer ll,mm
      character(30) extname
      character(4) ktmp2
c
      data tform/'1I','20A','1E','10A','30A'/
      data ttype/'index','parameter','value','type','comment'/
      data tunit/' ',' ',' ',' ',' '/
c
      nrows=npar
      varidat=0
c
      do mm=1,55
        parval4(mm)=parval(mm)
        enddo
c
c     move to the last hdu (hdunum) in the file
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c     append a new empty extension after the last hdu
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c     define parameters for the binary table (see the above data statements)
      tfields=5
c
c     build extension name
      extname='PARAMETERS'
      if(nloopctl.gt.0) then
          write(ktmp2,'(i4.4)')nloopctl
c          extname='parameters_' // ktmp2
          endif
c
c     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c     save run-specific information
      call ftpkys(unit,'MODEL',mdlname,'model name for this run',status)
      if (status .gt. 0)call printerror(lun11,status)
c
c     set 'global' parameters for writing fits columns
      frow=1
      felem=1
c
c     column  1  (index)
      colnum=1
      do ll=1,nrows
         idat1(ll)=ll
         enddo
      call ftpclj(unit,colnum,frow,felem,nrows,idat1,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c     column  2  (parameter name)
      colnum=2
      call ftpcls(unit,colnum,frow,felem,nrows,parname,status)
      if (status .gt. 0)call printerror(lun11,status)

c     column  3  (parameter value)
      colnum=3
      call ftpcle(unit,colnum,frow,felem,nrows,parval4,status)
      if (status .gt. 0)call printerror(lun11,status)

c     column  4 (parameter type)
      colnum=4
      call ftpcls(unit,colnum,frow,felem,nrows,partype,status)
      if (status .gt. 0)call printerror(lun11,status)

c     column  5 (parameter comment)
      colnum=5
      call ftpcls(unit,colnum,frow,felem,nrows,parcomm,status)
      if (status .gt. 0)call printerror(lun11,status)

c----------------------------------------------------------------
c     compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)
c
      return
      end
