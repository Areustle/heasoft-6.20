      subroutine calt71(temp,den,ic,m,np1r,np1i,rdat1,idat1,wav,aij,
     $                  lun11,lpri)
c
c  This rutine takes the coefficients in data type 71 (dtype71 reals
c  in itype71 integers) and returns the radiative transition prbability
c  (in s-1) from the superlevels to the spectroscopic level given by
c  itype71(3).
c  The wavelength for the transition is also given in wav
c  temp, den, and ic are the temperature, electron density
c  and effective charge of the ion respectively.
c      author: M. Bautista
c
       implicit none
      include './PARAM'
c
      integer m
      real*8 rdat1(nrdat1)
      integer idat1(nidat1)
      real*8 wav,aij,temp,den,rne,rte,dtmp,rm,rec1,
     $     rec,rec2
      integer lun11,lpri,nden,ntem,in,it,kt1,ic
      integer javi,np1r,np1i
c
      javi=m
      m=javi
c
      rne=log10(den)
      rte=log10(temp)
      nden=idat1(np1i-1+1)
      ntem=idat1(np1i-1+2)
      if (lpri.gt.1) write (lun11,*)'in calt71:',nden,ntem

      if (nden.eq.1 .and. ntem.eq.1) then
        if (rdat1(np1r-1+3).gt.30.) then
          dtmp=log10(rdat1(np1r-1+3))
        else
          dtmp=rdat1(np1r-1+3)
        endif
       wav=rdat1(np1r-1+4)
       aij=10.**dtmp
c       aij=min(aij,1.e+12)
       if (lpri.gt.1) write (lun11,*)'early return',aij,wav
       return
      endif
      if (rne.gt.rdat1(np1r-1+nden)) then
        if (lpri.gt.1) then
          write (lun11,*)'DENSITY TOO HIGH AT CALT71'
          write (lun11,*)'z=',ic,' temp=',temp,' Ne=',den,nden,
     $                   rdat1(np1r-1+nden)
          endif
        rne=min(rne,rdat1(np1r-1+nden))
      endif
      if (rte.gt.(rdat1(np1r-1+nden+ntem)+1.)) then
       rte=rdat1(np1r-1+nden+ntem)+1.
      endif
      if (rte.lt.(rdat1(np1r-1+nden+1)-1.)) then
       rte=rdat1(np1r-1+nden+1)-1.
      endif
c
      wav=rdat1(np1r-1+nden*ntem+nden+ntem+1)
      if (rne.le.rdat1(np1r-1+1)) then
       in=1
      else
       in=0
 5     in=in+1
       if (rne.ge.rdat1(np1r-1+in+1).and.in.lt.nden) goto 5
      endif
      if (rte.lt.rdat1(np1r-1+nden+1)) then
       it=1
      else
       it=0
 6     it=it+1
       if (it.ge.ntem) then
        it=ntem-1
       else
        if (rte.ge.rdat1(np1r-1+nden+it+1)) goto 6
       endif
      endif
      kt1=nden+ntem+(in-1)*ntem+it
      rm=(rdat1(np1r-1+kt1+1)-rdat1(np1r-1+kt1))
     $   /(rdat1(np1r-1+nden+it+1)-
     #    rdat1(np1r-1+nden+it))
      rec1=rdat1(np1r-1+kt1)+rm*(rte-rdat1(np1r-1+nden+it))
      kt1=kt1+ntem
      rm=(rdat1(np1r-1+kt1+1)-rdat1(np1r-1+kt1))
     $    /(rdat1(np1r-1+nden+it+1)-
     #    rdat1(np1r-1+nden+it))
      rec2=rdat1(np1r-1+kt1)+rm*(rte-rdat1(np1r-1+nden+it))
c
      rm=(rec2-rec1)/(rdat1(np1r-1+in+1)-rdat1(np1r-1+in))
      rec=rec1+rm*(rne-rdat1(np1r-1+in))
      aij=10.**rec
c      aij=min(aij,1.e+12)
      if (lpri.gt.1) write (lun11,*)'late return',rm,rec2,
     $       rec1,rec,aij,wav
c
      return
      end
