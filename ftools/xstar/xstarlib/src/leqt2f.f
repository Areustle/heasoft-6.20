      subroutine leqt2f(a,m,n,np,b,idgt,wkarea,ier,lun11,lpri)
c
      implicit none
c
      include './PARAM'
c
      integer indx(nd),ier,lun11,lpri,n,np,m,npp
      real*8 a(np,np),b(np),wkarea(1)
      real*8  ao(ndss,ndss),bo(ndss),btmp,tmp,sum,errmx,err
      real*8  an(ndss,ndss),bn(ndss),d,tmpmx
      integer mm,ll2,mmmx,mmmxo,jk,idgt,kl
c
c     n had better be less than nd
c
c     Not used
      integer javi
      real*8 javir
      javi=m
c      m=javi
      javi=ier
      javir=wkarea(1)
c      wkarea(1)=javir
      javi=idgt
c
      do jk=1,n
        bo(jk)=dble(b(jk))
        bn(jk)=dble(b(jk))
        do kl=1,n
           an(jk,kl)=dble(a(jk,kl))
           ao(jk,kl)=dble(a(jk,kl))
           enddo
        enddo
c
      npp=ndss
      if (lpri.gt.1)
     $ write (lun11,*)'before ludcmp',n,npp,np
      call ludcmp(an,n,npp,indx,d,lun11,lpri)
      npp=ndss
      if (lpri.gt.1)
     $ write (lun11,*)'after ludcmp',n,npp
      call lubksb(an,n,npp,indx,bn,lun11,lpri)
      if (lpri.gt.1)
     $ write (lun11,*)'after lubksb'
      npp=ndss
      call mprove(ao,an,n,npp,indx,bo,bn,lun11,lpri)
      if (lpri.gt.2)
     $ write (lun11,*)'after mprove',n,npp,np
c
c        check the solution
         if (lpri.gt.2) write (lun11,*)'checking the solution'
         errmx=0.d0
         do  ll2=1,n
          sum=0.d0
          tmpmx=0.d0
          mmmx=0
          mmmxo=0
          do  mm=1,n
            btmp=bn(mm)
            tmp=dble(a(ll2,mm))*max(0.d0,btmp)
            if (abs(tmp).ge.tmpmx) then
              mmmxo=mmmx
              mmmx=mm
              tmpmx=max(tmpmx,abs(tmp))
              endif
            sum=sum+tmp
            enddo
          sum=sum-dble(b(ll2))
          err=sum/max(1.d-24,tmpmx)
          errmx=max(errmx,abs(err))
          if (lpri.gt.2) write (lun11,9246)ll2,bn(ll2),tmpmx,sum,err,
     $                                     mmmx,mmmxo
 9246     format (1h ,i4,4e12.4,2i4)
          enddo
c
      do jk=1,n
         if (lpri.gt.2)
     $    write (lun11,*)jk,b(jk)
         if (bn(jk).lt.1.d-36) bn(jk)=0.d0
         if (bn(jk).gt.1.d+36) bn(jk)=1.d+36
         b(jk)=sngl(bn(jk))
         enddo
c
         if (lpri.gt.2)
     $    write (lun11,*)'leaving leqt'
c      ier=0
c      wkarea(1)=0.
c      idgt=0
c
      return
      end
