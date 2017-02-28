      subroutine drd(ltyp,lrtyp,lcon,lrdat,np1r,lidat,np1i,lkdat,np1k,
     &                 np2,nptrs,lpri,lun11)
c
c     this routine reads one element from the database
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
c
c     master data
      integer  nptrs(nptt,ndat2)
      integer nrd,lcon,ltyp,lrtyp,lrdat,lidat,
     $        lkdat,np2,lpri,lun11,np1,np1r,np1i,np1k
c
      if ( lpri.ne.0 ) write (lun11,*) 'in drd, np2=' , np2,
     &                                  ntyp
c      if ((ltyp.le.0).or.(ltyp.gt.ntyp))
c     $    stop 'data typing error'
      nrd = 0
      lcon=1
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
      lcon = 0
      if ( lpri.ne.0 ) write (lun11,*) 'leaving drd' , np2
c
      return
99001 format (8x,5i8)
      end
