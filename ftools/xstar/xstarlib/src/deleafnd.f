      subroutine deleafnd(jkk,lup,ml,
     $   nrdti,np1ri,nidti,np1ii,nkdti,np1ki,
     $   idat1,rdat1,kdat1,nptrs,np2,
     $   npar,npnxt,npfi,npfirst,
     $   nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $   npconi2,ncsvn,delea,lfnd,lpri,lun11)
c
      implicit none
c
      include './PARAM'
c
      character(49) kdesc(ntyp),kdesc2
      character(29) krdesc(ntyp)
c     master data 
      integer idat1(nidat1),nptrs(nptt,ndat2)
      integer np2
      real*8 rdat1(nrdat1)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      integer npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data 
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
      integer jkk,mllz,lup,lfnd,iltmp,lcon,ltyp,lrtyp,
     $   nrdt,np1r,nidt,np1i,nkdt,np1k,ml,nilin,
     $   nrdti,np1ri,nidti,np1ii,nkdti,np1ki,
     $   nlsvn,ncsvn,lpri,lun11,nitmp,iion,mlm,ndtmp
      real*8 delea
c
c     find associated type 86 data
c
c     this is not needed
      go to 9092
      iion=0
c      nilin=npar(ml)
      nilin=npfirst(13)
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $           nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $           nptrs,0,lun11)
      if (lpri.ge.1)
     $   write (lun11,*)'searching for ion',jkk,mlm,
     $   idat1(np1i+nidt-1)
       do while ((idat1(np1i+nidt-1).ne.jkk)
     $     .and.(iion.lt.nni))
        iion=iion+1
        nitmp=npfi(13,iion)
        mlm=nitmp-1
        call drd(ltyp,lrtyp,lcon,
     $           nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $           nptrs,0,lun11)
        if (lpri.ge.1)
     $    write (lun11,*)iion,idat1(np1i+nidt-1),
     $           nitmp
        enddo
 9092   continue
      iion=jkk
c     get damping parameter for iron auger damped lines  
      ndtmp=npfi(41,iion)
      if (lpri.gt.1) write (lun11,*)'ndtmp=',iion,ndtmp
      if (ndtmp.eq.0) then
          lfnd=0
          return
        else
          if (lpri.gt.1)
     $    write (lun11,*)'  found ion',lup,ndtmp
          mllz=npar(ndtmp)
          mlm=ndtmp-1
          call drd(ltyp,lrtyp,lcon,
     $           nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $           nptrs,0,lun11)
          iltmp=idat1(np1i+1)
          do while ((ndtmp.ne.0).and.(lup.ne.iltmp)
     $            .and.(npar(ndtmp).eq.mllz)) 
            mlm=ndtmp-1
            call drd(ltyp,lrtyp,lcon,
     $             nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $             nptrs,0,lun11)
            if (lpri.ge.2)
     $        call dprinto(ltyp,lrtyp,lcon,
     $                   nrdt,np1r,nidt,np1i,nkdt,np1k,
     $                   rdat1,idat1,kdat1,lun11)
            iltmp=idat1(np1i+1)
            if (lpri.gt.1)
     $       write (lun11,*)'   ',nidt,
     $       iltmp,ndtmp,lup
            ndtmp=npnxt(ndtmp)     
            enddo
        endif
      if (lpri.gt.1) write (lun11,*)'lup,iltmp',
     $                     lup,iltmp
      if (lup.eq.iltmp) then
          lfnd=1
          delea=rdat1(np1r+2)*(4.136e-15)
          if (lpri.gt.1) write (lun11,*)rdat1(np1r+2),delea
        else
          lfnd=0
        endif
     
c
      return
      end
