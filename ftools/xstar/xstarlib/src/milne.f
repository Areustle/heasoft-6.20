C------------------------------------------------------------------------------
      subroutine milne(temp4,nt,x4,y4,eth4,alpha4,lun11,lpri)
C
c     this routine calculates the Milne relation for type 53 data
c       x    = array of energies in Ry with respect to the threshold
c       y    = array of cross sections in Mb
c       nt   = number of points in topbase arrays
c       eth  = threshold energy in Ry
c       alpha= recombination rate for level n,lo
c     author:  M. Bautista
c
      implicit none
c
      integer nptmpdim
      parameter (nptmpdim=500000)
c
      integer lun11,lpri,nt,i
      real*8 x4(nt),y4(nt),temp4,alpha4,eth4
      real*8   temp,x(nptmpdim),y(nptmpdim),eth,alpha,st,ry,
     $    sum,s1,s2,v1,v2,rb,ra,ri2,ri3,sumo,crit
c
c      lpri=2
c
      ry=2.17896d-11
c
       temp=(temp4)
       eth=(eth4)
       do i=1,nt
         x(i)=(x4(i))
         y(i)=(y4(i))
         enddo
c
       st=(x(1)+eth)*ry
       sumo=1.d0
       sum=0.d0
       if (lpri.gt.1)
     $   write (lun11,*)'in milne:',temp,nt,eth,x(1),y(1)
       i=1
       crit=0.01d0
       do while ((abs(sum-sumo).gt.crit*sum).and.(i.lt.nt))
         i=i+1
         s1=(x(i-1)+eth)*ry
         s2=(x(i)+eth)*ry
         if (s2.lt.s1) return
         v1=y(i-1)
         v2=y(i)
         if (lpri.gt.1) write (lun11,*)'i=',i,x(i),y(i),
     $     s1,s2,v1,v2
         if ((v1.ne.0.d0).or.(v2.ne.0.d0)) then
           rb=(v2-v1)/(s2-s1+1.d-24)
           ra=v2-rb*s2
           call intin(s1,s2,st,temp,ri2,ri3,lpri,lun11)
           sumo=sum
           sum = sum + (ra*ri2 + rb*ri3)
           if (lpri.gt.1) write (lun11,*)i,x(i),y(i),
     $     s1,s2,v1,v2,ra,rb,ri2,ri3,sum
           endif
         enddo
c      alpha=sum*.79788*2.4917e+25/(temp**1.5)
       alpha=sum*.79788d0*40.4153d0
       if (lpri.gt.1)
     $    write (lun11,*)'alpha=',alpha,sum
       alpha4=sngl(alpha)
c
      return
      end
