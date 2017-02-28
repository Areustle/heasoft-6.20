      subroutine errmess(lun11,nlen,str1)
      character*(*) str1
      integer lun11
      write (lun11,*) 'in errmess:',str1
      return
      end
