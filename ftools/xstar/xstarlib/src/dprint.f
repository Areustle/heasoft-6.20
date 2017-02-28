      subroutine dprint(ltyp,lrtyp,lcon,
     $  lrdat,rdat,lidat,idat,lkdat,kdat,
     $  np1r,np1i,np1k,np2,
     $  idat1,rdat1,kdat1,nptrs,lpri,lun11)
c
c     this  routine prints one element of the database
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
      integer nptmpdim
      parameter (nptmpdim=200000)
c
      integer idat1(nidat1)
      real*8 rdat1(nrdat1)
      character(1) kdat1(nkdat1)
      integer nptrs(nptt,ndat2)
      integer ltyp,lrtyp,lcon,lrdat,lidat,lkdat,
     $  np1r,np1i,np1k,np2,ml,lpri,lun11,lprisv
      real*8 rdat(nptmpdim)
      integer idat(nptmpdim)
      character(1) kdat(nptmpdim)
cc
      lprisv=lpri
      if (lpri.ne.0)
     $ write (lun11,*)'in dprint, np2=',np2
      if (np2.ge.ndat2) then
          write (6,*) 'data index error'
          return
          endif
      nptrs(1,np2)=1
      nptrs(2,np2)=ltyp
      nptrs(3,np2)=lrtyp
      nptrs(4,np2)=lcon
      nptrs(5,np2)=lrdat
      nptrs(6,np2)=lidat
      nptrs(7,np2)=lkdat
      nptrs(8,np2)=np1r
      nptrs(9,np2)=np1i
      nptrs(10,np2)=np1k
      if (lpri.ne.0) then
        write (lun11,*)'in dprint:',np2,ltyp,lrtyp,lrdat,lidat,lkdat
        write (lun11,*)'          ',lcon,np1r,np1i,np1k
        endif
      np2=np2+1
      if (lrdat.gt.0) then
        do ml=1,lrdat
           rdat1(np1r)=rdat(ml)
           np1r=np1r+1
           enddo
        endif
      if (lidat.gt.0) then
        do  ml=1,lidat
           idat1(np1i)=idat(ml)
           np1i=np1i+1
           enddo
        endif
      if (lkdat.eq.0) then
        do ml=1,lkdat
            kdat1(np1k)=kdat(ml)
            np1k=np1k+1
            enddo
        endif
c
      if ((np1k.gt.nkdat1).or.(np1i.gt.nidat1).or.(np1r.gt.nrdat1)
     $   .or.(np2.gt.ndat2)) then
        write (lun11,*)'dprint index error,',np1k,np1i,np1r,np2
        return
        endif
c
      lpri=lprisv
c
      return
      end
