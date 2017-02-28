      subroutine ludcmp(a,n,np,indx,d,lun11,lpri)
c
c
      implicit none
c
      real*8  tiny
      parameter (tiny=1.0d-20)
      include './PARAM'
      integer np, n
c
c
      real*8  a(np,np),vv(nd),d,aamax,sum,dum
      integer indx(n),lun11,lpri,j,i,imax,k
c
      if (lpri.gt.1)
     $ write (lun11,*)'in ludcmp:'
      d=1.d0
      do 12 i=1,n
        aamax=0.d0
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
          if (lpri.gt.1) write (lun11,*)i,j,a(i,j),aamax
11      continue
        if (aamax.eq.0.d0) then
           if (lpri.gt.1)
     $      write (lun11,9902)
           return
9902       format (1h ,'singular matrix.' )
         end if
        vv(i)=1.d0/aamax
12    continue
      do 19 j=1,n
        if (j.gt.1) then
          do 14 i=1,j-1
            sum=a(i,j)
            if (i.gt.1)then
              do 13 k=1,i-1
                sum=sum-a(i,k)*a(k,j)
13            continue
              a(i,j)=sum
            endif
14        continue
        endif
        aamax=0.d0
        imax=0
        do 16 i=j,n
          sum=a(i,j)
          if (j.gt.1)then
            do 15 k=1,j-1
              sum=sum-a(i,k)*a(k,j)
15          continue
            a(i,j)=sum
          endif
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        imax=max(imax,1)
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        if (lpri.gt.1)
     $   write (lun11,*)'j,imax:',j,imax
        indx(j)=imax
        if(j.ne.n)then
          if(a(j,j).eq.0.d0)a(j,j)=tiny
          dum=1.d0/a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      if(a(n,n).eq.0.d0)a(n,n)=tiny
c
      return
      end
