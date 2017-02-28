      subroutine calt70(temp,den,eth,ic,m,np1r,np1i,rdat1,idat1,
     1                  nx,xe,xs,rec,al,lun11,lpri)
c
c  This routine takes the coefficients in data type 70 (dtype70 reals
c  in itype70 integers) and returns the recombination rate (in s-1cm-3)
c  and the correstpondent phot. x-section for the superlevel. m is the
c  dimension of dtype70. nx is the number of points in the x-section
c  xe() contains the photon energy in Ry and xx() is the x-section
c  in Mb.
c  temp, den, and ic are the temperature, electron density
c  and effective charge of the ion respectively.
c  eth is the threshold energy for the superlevel in Ry.
c      author: M. Bautista
c

       implicit none
      integer nptmpdim
      parameter (nptmpdim=200000)
      include './PARAM'
c
      integer m
      real*8 rdat1(nrdat1)
      integer idat1(nidat1)
      real*8 xe(nptmpdim),xs(nptmpdim),rne,rte,rm,
     $      rec1,rec2,rec,scale,al,crit,temp,den,eth,dt
      integer nden,ntem,nxs,in,it,kt1,i1,imax,nx,
     $      lun11,lpri,lprim,ic,i,np1r,np1i
c
c     alpf: fitting coef. for hydrogenic recombination n=12,l=0
c      dimension alpf(3)
c      data alpf/-7.1094841E-02,-9.0274535E-02,-14.26129/
c
      if (lpri.gt.1)
     $  write (lun11,*)'in calt70:',temp,den,eth,ic,m,rdat1(np1r),
     $                  idat1(np1i-1+1)
      rne=log10(den)
      rte=log10(temp)
      nden=idat1(np1i-1+1)
      ntem=idat1(np1i-1+2)
      nxs=idat1(np1i-1+3)
      if (nden.gt.1) then
      if (rne.gt.rdat1(np1r-1+nden)) then
c       write (lun11,*)'DENSITY TOO HIGH AT SUPREC'
c       write (lun11,*)'z=',ic,' temp=',temp,' Ne=',den
c       return
       rne=min(rne,rdat1(np1r-1+nden))
      endif
      if (rne.le.rdat1(np1r-1+1)) then
       in=1
      else
       in=int(rne/rdat1(np1r-1+nden)*nden)-1
       if (in.ge.nden) in=in-1
 5     in=in+1
       if (in.lt.nden .and. rne.ge.rdat1(np1r-1+in+1)) goto 5
       if (rne.lt.rdat1(np1r-1+in)) then
        in=in-2
        goto 5
       endif
      endif
      else
       in=1
      endif
      if (rte.lt.rdat1(np1r-1+nden+1)) then
       it=1
      else
       dt=(rdat1(np1r-1+nden+ntem)-rdat1(np1r-1+nden+1))/float(ntem)
       it=int((rte-rdat1(np1r-1+nden+1))/dt)
 6     it=it+1
       if (it.ge.ntem) then
        it=ntem-1
       else
        if (rte.ge.rdat1(np1r-1+nden+it+1)) goto 6
        if (rte.lt.rdat1(np1r-1+nden+it)) then
         it=it-2
         goto 6
        endif
       endif
      endif
      kt1=nden+ntem+(in-1)*ntem+it
      rm=(rdat1(np1r-1+kt1+1)-rdat1(np1r-1+kt1))
     $    /(rdat1(np1r-1+nden+it+1)-
     #    rdat1(np1r-1+nden+it))
      rec1=rdat1(np1r-1+kt1)+rm*(rte-rdat1(np1r-1+nden+it))
      if (nden.gt.1) then
       kt1=kt1+ntem
       rm=(rdat1(np1r-1+kt1+1)-rdat1(np1r-1+kt1))
     $    /(rdat1(np1r-1+nden+it+1)-
     #    rdat1(np1r-1+nden+it))
       rec2=rdat1(np1r-1+kt1)+rm*(rte-rdat1(np1r-1+nden+it))
       rm=(rec2-rec1)/(rdat1(np1r-1+in+1)-rdat1(np1r-1+in))
       rec=rec1+rm*(rne-rdat1(np1r-1+in))
      else
       rec=rec1
      endif
      if (lpri.gt.1) write (lun11,*)nden,ntem,it,in,kt1,
     $             rdat1(np1r-1+kt1),rm,rec2,rec1,rec
      rec=10.**rec
c
      i1=ntem*nden+ntem+nden
      do i=1,nxs
       xe(i)=rdat1(np1r-1+i1+(i-1)*2+1)
       xs(i)=rdat1(np1r-1+i1+(i-1)*2+2)
      if (lpri.gt.1)
     $  write (lun11,*)i,xe(i),xs(i)
      enddo
      lprim=0
      call milne(temp,nxs,xe,xs,eth,al,lun11,lprim)
      scale=rec/(1.e-24+al)
      if (lpri.gt.1)
     $ write (lun11,*)'in calt70:',rec,al,scale,xs(1),nxs,eth
      crit=1.e-6
      imax=nxs
      do i=1,nxs
       xs(i)=xs(i)*scale
       xs(i)=min(xs(i),1000000.)
       if (xs(i).gt.xs(1)*crit) imax=i
      if (lpri.gt.1)
     $  write (lun11,*)i,xe(i),xs(i)
      enddo
      nxs=imax
      nx=nxs
c
      return
      end
