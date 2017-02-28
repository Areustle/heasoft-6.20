      subroutine enxt(eth,nb1,lpri,epi,ncn2,t,lfast,lun11,
     $                  jk,nskp,nphint,lrcalc)
c
c     finds next energy bin for photoionizaion rate integrations
c     author: T. Kallman
c
      implicit none
c
      include './PARAM'
c
      real*8 epi(ncn)
      real*8 ergsev,bk,tm,t,eth,bktm,exptst,epii
      integer nb1,ncn2,lfast,lun11,jk,nphint,lrcalc,lpri
      integer nskp,numcon2,nbinc,numcon3,nskp1,numcon,nskp2
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
       if (lpri.gt.2)
     $  write (lun11,*)'in enxt:',eth,nb1,t,lfast,jk,lpri,
     $                    epi(1),epi(ncn2),ncn2
      tm=t*1.e4
      bktm=bk*tm/ergsev
      if (lfast.le.2) then
         numcon2=max(2,ncn2/50)
         nphint=ncn2-numcon2
         nskp=1
         nskp2=1
      elseif (lfast.eq.3) then
         nphint=nbinc(max(3.*eth,eth+3.*bktm),epi,ncn2)
         nphint=max(nphint,nb1+1)
         nskp=max(1,int((nphint-nb1)/16))
         nskp2=nskp
      else
        nphint=nbinc(1.d+4,epi,ncn2)
        nskp=1
        nskp2=1
        endif
      nskp1=nskp
      epii=epi(jk)
      exptst=(epii-eth)/bktm
      if (exptst.lt.3.) then
         lrcalc=1
         nskp=nskp1
       else
         lrcalc=0
         nskp=nskp2
       endif
       nphint=max(nphint,nb1+nskp)
       numcon=ncn2
       numcon2=max(2,ncn2/50)
       numcon3=numcon-numcon2
       nphint=min(nphint,numcon3)
       if (lpri.gt.2)
     $  write (lun11,*)'in enxt:',eth,nb1,t,lfast,jk,nskp,
     $   nphint,lrcalc
c
      return
      end
