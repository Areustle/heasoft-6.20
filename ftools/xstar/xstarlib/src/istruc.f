      subroutine istruc(zeff,alpha,xitp,nnz,lpri,lun11)
c
      implicit none
c
      integer nnz,lpri,lun11
c
      real*8 zeff(31),alpha(31),xitp(31),xisum
      real*8  z8(31),a8(31),x8(31)
      integer mm,nnzp1,ill
c
      if (lpri.ne.0)
     $ write (lun11,*)'ion rates:',nnz
      do mm=1,nnz
          z8(mm)=dble(zeff(mm))
          a8(mm)=dble(alpha(mm))
          if (lpri.ne.0)
     $     write (lun11,9901)zeff(mm),alpha(mm)
9901      format (1x,2(1pe11.3))
          enddo
c
      nnzp1 = nnz + 1
      ill=1
      call ioneqm(z8,a8,x8,nnzp1,nnz,ill,lpri,lun11)
c
      xisum=0.
      do mm=1,nnz
          xitp(mm)=sngl(x8(mm))
          xisum=xisum+xitp(mm)
          enddo
      xitp(nnz+1)=max(0.,1.-xisum)
c
      return
      end
