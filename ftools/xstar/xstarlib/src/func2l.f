      subroutine func2l(jkk,lpri,lun11,t,xee,xpx,
     $       idat1,rdat1,kdat1,nptrs,
     $       npar,npnxt,npfi,
     $       rniss,rlev,ilev,
     $       nlpt,iltp,nlev,klev)
c
c     this routine calculates rates affecting level populations
c     author: T. Kallman
c

      implicit none
c
      include './PARAM'
c
c     master data
c     global xstar data
c     master data
      integer idat1(nidat1)
      real*8 rdat1(nrdat1)
      integer nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data
      integer npar(ndat2),npnxt(ndat2)
      integer npfi(ntyp,nni)
      real*8 rlev(10,nd)
      integer ilev(10,nd),nlpt(nd),iltp(nd)
      integer np1i,np1r,np1k
      character(1) klev(100,nd),kblnk
c
      integer nlevmx,mltype,ml,mllz,nlev,nidt,nrdt,nkdt,lun11,
     $        lpri,ltyp,lrtyp,lcon,jkk,mm,lk,mlpar,mlm
      real*8 xee,xpx,t,bb
      real*8 rniss(nnml)
c
      data kblnk/' '/
c
      if (lpri.gt.1)
     $  write (lun11,*)'in func2l, inputs:',t,
     $          xee,xpx
c
c     now find level data
c     step thru types
      nlevmx=0
      mltype=13
      ml=npfi(mltype,jkk)
      if (lpri.gt.1) write (lun11,*)'jkk=',jkk,ml,npar(ml)
      mllz=npar(ml)
c     step thru records of this type
      mlpar=npar(ml)
      do while ((ml.ne.0).and.(mlpar.eq.mllz))
         mlm=ml-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         nlev=idat1(np1i+nidt-2)
         nlevmx=max(nlevmx,nlev)
         if ((nlev.gt.0).and.(nlev.le.nd)) then
           nlpt(nlev)=ml
           iltp(nlev)=ltyp
 9101      format (1x,'level quantities:',4i6,4(1pe12.5),3i6,8a1)
           if (lpri.gt.1) write (lun11,9101)
     $       ml,nlev,ltyp,lrtyp,(rdat1(np1r+mm-1),mm=1,4),
     $       idat1(np1i),idat1(np1i+1),
     $       idat1(np1i+2),(kdat1(np1k+mm-1),mm=1,8)
           do  lk=1,nrdt
             rlev(lk,nlev)=rdat1(np1r+lk-1)
             enddo
           do lk=1,nidt
             ilev(lk,nlev)=idat1(np1i+lk-1)
             enddo
           do lk=1,nkdt
             klev(lk,nlev)=kdat1(np1k+lk-1)
             enddo
           do lk=nkdt+1,100
             klev(lk,nlev)=kblnk
             enddo
           endif
         ml=npnxt(ml)
         if (ml.ne.0) mlpar=npar(ml)
         enddo
      nlev=nlevmx
      call levwk(rniss,bb,lpri,rlev,ilev,
     $   nlpt,iltp,nlev,klev,t,xee,xpx,lun11)
c
      return
      end
