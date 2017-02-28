      function nbinc(e,epi,ncn2)
      implicit none
c
c     this function bins the continuum
c     lines between   epi(i) and   epi(i+1) are put in bin number i.
c     energies between 0 and   epi(1) are put in bin number 50.
c     author:  T. Kallman
c
      include './PARAM'
c
      real*8 e
      integer jlo, lun11,nbinc, ncn2, numcon, numcon2,
     &        numcon3
      real*8 epi(ncn)
c
      lun11=6
      numcon=ncn2
      numcon2=max0(2,ncn2/50)
      numcon3=numcon-numcon2
      call huntf(epi,numcon3,e,jlo,0,lun11)
c      call hunt3(epi,numcon3,e,jlo,0,lun11)
c      if (abs(e-epi(jlo+1)).lt.abs(e-epi(jlo))) jlo=jlo+1
      nbinc=jlo
c
      return
      end
