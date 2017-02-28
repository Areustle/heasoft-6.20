      subroutine func2i(jkk,
     $       idat1,rdat1,kdat1,nptrs,
     $       npfi,npar,npnxt,nlev)
c
c     this routine counts the levels for each ion
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
      integer np1i,np1r,np1k
c
      integer nlevmx,mltype,ml,mllz,nlev,nidt,nrdt,nkdt,
     $        jkk,ltyp,lrtyp,lcon,mlpar,lun11,mlm
c
c          now find level data
c          step thru types
           nlevmx=0
           mltype=13
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
c          step thru records of this type
           mlpar=npar(ml)
           do while ((ml.ne.0).and.(mlpar.eq.mllz))
              mlm=ml-1
              call drd(ltyp,lrtyp,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $          nptrs,0,lun11)
              nlevmx=nlevmx+1
              nlev=idat1(np1i+nidt-2)
              ml=npnxt(ml)
              if (ml.ne.0) mlpar=npar(ml)
              enddo
           nlev=nlevmx
c
      return
      end
