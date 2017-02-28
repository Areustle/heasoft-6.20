      subroutine dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                 np2,idat1,rdat1,kdat1,nptrs,lpri,lun11)
c
c     this routine reads one element from the database
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
      integer nptmpdim
      parameter (nptmpdim=200000)
c
c     master data
      integer idat1(nidat1) , nptrs(nptt,ndat2)
      real*8  rdat1(nrdat1)
      character(1) kdat1(nkdat1)
      real*8 rdat(nptmpdim)
      integer idat(nptmpdim)
      character(1) kdat(nptmpdim)
      integer mlr,mli,mlk,nrd,lcon,ltyp,lrtyp,lrdat,lidat,
     $        lkdat,np2,lpri,lun11,np1,np1r,np1i,np1k,ml,mm
c
      if ( lpri.ne.0 ) write (lun11,*) 'in dread, np2=' , np2,
     &                                  ntyp
c      if ((ltyp.le.0).or.(ltyp.gt.ntyp))
c     $    stop 'data typing error'
      mlr = 0
      mli = 0
      mlk = 0
      nrd = 0
      lcon=1
      do while (lcon.ne.0)
        np2 = np2 + 1
        nrd = nrd + 1
        np1 = nptrs(1,np2)
        ltyp = nptrs(2,np2)
        lrdat = nptrs(5,np2)
        lidat = nptrs(6,np2)
        lkdat = nptrs(7,np2)
        lrtyp = nptrs(3,np2)
        lcon = nptrs(4,np2)
        np1r = nptrs(8,np2)
        np1i = nptrs(9,np2)
        np1k = nptrs(10,np2)
        if ( lpri.ne.0 ) write (lun11,*) 'in dread:' , np2 , np1 ,ltyp,
     &                                 lrtyp , lrdat , lidat
        if ( lpri.ne.0 ) write (lun11,99001) lkdat , lcon , np1r ,np1i,
     &                        np1k
        if ( lrdat.ne.0 ) then
          do ml = 1 , lrdat
            rdat(mlr+ml) = rdat1(np1r+ml-1)
            if ( lpri.ne.0 ) write (lun11,*) mlr , np1r , rdat1(np1r) ,
     &                              rdat(mlr+ml)
            enddo
          np1r=np1r+lrdat-1
          mlr=mlr+lrdat
          if ( lpri.ne.0 ) write (lun11,*) 'rdat=' ,
     &                           (rdat(mm),mm=1,lrdat) , np2
          endif
        if ( lidat.ne.0 ) then
          do ml = 1 , lidat
            idat(mli+ml) = idat1(np1i+ml-1)
            if ( lpri.ne.0 ) write (lun11,*) mli , np1i , idat1(np1i) ,
     &                              idat1(np1i-1+mli+ml) , np2
            enddo
          mli = mli + lidat
          np1i = np1i + lidat-1
          if ( lpri.ne.0 ) write (lun11,*) 'idat=' ,
     &                           (idat1(np1i-1+mm),mm=1,lidat)
          endif
        if ( lkdat.ne.0 ) then
          do ml = 1 , lkdat
            kdat(mlk+ml) = kdat1(np1k+ml-1)
c           write (lun11,*)mlk,np1k,kdat1(np1k),kdat(mlk),np2
            enddo
          mlk = mlk + lkdat
          np1k = np1k + lkdat-1
          if ( lpri.ne.0 ) write (lun11,*) 'kdat=' ,
     &                           (kdat(mm),mm=1,lkdat)
          endif
        enddo
c      np2=np2-nrd+1
      lidat = mli
      lrdat = mlr
      lkdat = mlk
      lcon = 0
c
c     the last pointer has to point to the next empty space.
c      nptr1(np2+1)=np1+1
c
c      call remtms(tt1)
c      tread = tread + abs(tt1-tt0)
c
      if ( lpri.ne.0 ) write (lun11,*) 'leaving dread' , np2
c
      return
99001 format (8x,5i8)
      end
