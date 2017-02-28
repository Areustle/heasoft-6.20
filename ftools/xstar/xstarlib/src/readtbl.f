      subroutine readtbl(nptrs,np1r,np1i,np1k,np2,npdat2,
     &           rdat1,idat1,kdat1,nidat1,filename,credate,lpri,lun11)
c
c     reads in atomic data
c       written by Ke Zhang, Nov. 9, 2001
c     NB this routine assumes that nrdat1=nidat1=nkdat1
c
      implicit none
c
      integer nidatt
      parameter (nidatt=37000000)
c
      integer nidat1,npdat2,lpri,lun11,np1r,np1i,np1k,np2
      integer idat1(nidat1)
      real rdat14(nidatt)
      real*8 rdat1(nidat1)
      character(1) kdat1(nidat1)
      integer nptrs(10,npdat2),ntptr(nidatt),mm

      real nulle
      integer nullj
      character filename*256,nullstr*1
      character credate*63, comment*50
      logical anynull

      integer status,unit,readwrite,blocksize,hdutype
      integer row,col,j,i,lenact

      status=0
      nullstr=' '
      nullj=0
      nulle=0.

      if (lpri.ne.0) write (lun11,*)'in readtbl'
c
c get an unused logical unit number and open the fits file
c      call ftgiou(unit,status)
      call getlunx(unit)
      readwrite=0
      call ftopen(unit,filename,readwrite,blocksize,status)

c Read the primary header & get the file creation date
      call ftmahd(unit,1,hdutype,status)
      call ftgkys(unit,'DATE',credate,comment,status)
      if(status .gt.0) call printerror(lun11,status)
      write(lun11,*)'Atomic Data Version: ',credate(1:lenact(credate))

      col=1
      row=1

c move to the next extension : POINTERS
      call ftmrhd(unit,1,hdutype,status)

c read LENGTH keywords: total records #
      call ftgkyj(unit,'LENGTH',np2,comment,status)

c read POINTERS data
      call ftgcvj(unit,col,row,1,10*np2,nullj,
     &               ntptr,anynull,status)
      do i=1,np2
        do j=1,10
          nptrs(j,i)=ntptr(10*(i-1)+j)
        enddo
      enddo


c move to the next extension : REALS
      call ftmrhd(unit,1,hdutype,status)

c read LENGTH keywords: total reals #
      call ftgkyj(unit,'LENGTH',np1r,comment,status)

c read REAL data
      call ftgcve(unit,col,row,1,np1r,nulle,
     &               rdat14,anynull,status)
      do mm=1,nidat1
        rdat1(mm)=rdat14(mm)
        enddo


c move to the next extension : INTEGERS
      call ftmrhd(unit,1,hdutype,status)

c read LENGTH keywords: total integers #
      call ftgkyj(unit,'LENGTH',np1i,comment,status)

c read INTEGER data
      call ftgcvj(unit,col,row,1,np1i,nullj,
     &               idat1,anynull,status)


c move to the next extension : CHARS
      call ftmrhd(unit,1,hdutype,status)

c read LENGTH keywords: total chars #
      call ftgkyj(unit,'LENGTH',np1k,comment,status)

c read CHAR data
      call ftgcvb(unit,col,row,1,np1k,nullstr,
     &             kdat1,anynull,status)


c close the file and free the unit number
      call ftclos(unit, status)
c      call ftfiou(unit, status)
      close(unit)

c check for any error, and if so print out error messages
      if (status .gt. 0) call printerror(lun11,status)

      return
      end
