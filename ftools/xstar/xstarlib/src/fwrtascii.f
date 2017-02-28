      subroutine fwrtascii(unit,extname,rdati,ncol,
     $                      nidat1j,klabs, kform, kunits,lun11)

c     write an ascii table extension containing
c     ncol columns and nidat1 rows
c     author: T. Bridgman
c
c     parameters:
c        unit    integer            file unit number
c        extname char*30            name of the ascii extension
c        rdati   real(ncol*nidat1j)  data array
c        ncol    integer            number of columns
c        nrhdim  integer            maximum number of rows & columns
c        nidat1j  integer            actual number of rows
c        klabs   char*16(ncol)      column labels
c        kform   char*16(ncol)      column numeric format
c        kunits  char*15(ncol)      column units
c
c     modifications:
c        1998/12/17, wtb: fix fits keyword format problem.  enhanced
c                    parameter list for more flexibility.
c        1999/01/04, wtb: added file creation date, model name, creator
c                    code & checksum
c        1999/01/25, wtb: convert this routine so it just writes an
c                    ascii table extension.
c
      implicit none
      include './PARAM'
      integer nrhmx,nrhmx1

      parameter (nrhmx1=999)
      parameter (nrhmx=3999)

c     passed parameters
      real*8 rdati(nrhmx1,nrhmx)
      real rdat(nrhmx)
      character(16) klabs(nrhmx), kform(nrhmx), kunits(nrhmx)
      integer ncol, nidat1j
!      character(30) extname  !jg
      character(10) extname

      integer unit, status, tfields, nrows, rowlen, verbose,lun11
      integer tbcol(nrhmx),felem,frow,colnum,kk,ll
c
      status=0
      verbose=0
      tfields=ncol
      nrows=nidat1j
      rowlen=0
      tbcol(1)=0
c     append a new empty extension onto the end of the primary array
      call ftcrhd(unit,status)

      if(verbose.gt.0) write(6,*)'fwrtascii: writing header table'
c     write the required header parameters for the ascii table
      call ftphtb(unit,rowlen,nrows,tfields,klabs,tbcol,kform,kunits,
     &            extname,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c     map each column to a 1-d array before writing to the file
      do kk=1,tfields
        if(verbose.gt.0) write(6,*)'fwrtascii: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nidat1j
          rdat(ll)=sngl(rdati(kk,ll))
          enddo
        if(verbose.gt.0) write(6,*)'fwrtascii: writing column ',kk
        call ftpcle(unit,colnum,frow,felem,nrows,rdat,status)
        enddo
      if (status .gt. 0)call printerror(lun11,status)

c     compute checksums
      if(verbose.gt.0) write(6,*)'fwrtascii: writing checksum'
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)
c
      end
