      subroutine deletefile(filename,status)
c
c     a simple little routine to delete a fits file
c     author:  T. Bridgman
c
      implicit none
      integer status,unit,blocksize
      character*(*) filename
      character*50 kcom
      character*1 ktmp
      integer mm,ll,lenact
c
      data kcom/'rm -f                                             '/
c
      mm=lenact(filename)

      do ll=1,mm
        read (filename(ll:ll),'(a1)')ktmp
        write (kcom(6+ll:6+ll),'(a1)')ktmp
        enddo
      do ll=mm+7,50
        write (kcom(ll:ll),'(a1)')' '
        enddo
c       write (6,*)'executing:',kcom
c      this is slow but it works
c      call system(kcom)
c      return
c
c     simply return if status is greater than zero
c      write (6,*)'in deletefile',unit,filename,status
      if (status .gt. 0)return
c
c     get an unused logical unit number to use to open the fits file
      call ftgiou(unit,status)
      call getlunx(unit)
c      write (6,*)'after ftgiou',unit,status
c
c     try to open the file, to see if it exists
      call ftopen(unit,filename,1,blocksize,status)
c      write (6,*)'after ftopen',unit,status
c
      if (status .eq. 0)then
c         file was opened;  so now delete it
          call ftdelt(unit,status)
c      write (6,*)'after ftdelt 1',unit,status
      else if (status .eq. 103)then
c         file doesn't exist, so just reset status to zero and clear errors
          status=0
          call ftcmsg
      else
c         there was some other error opening the file; delete the file anyway
          status=0
          call ftcmsg
          call ftdelt(unit,status)
c      write (6,*)'after ftdelt 2',unit,status
      end if


c     free the unit number for later reuse
c      call ftfiou(unit, status)
      call frelunx(unit)
      close(unit)
c      write (6,*)'after ftfiou',unit,status
c
      end
