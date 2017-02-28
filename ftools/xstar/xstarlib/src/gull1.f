      subroutine gull1(n,rs,gus,gls,lpri,lun11)
c
c this subroutine calculates the value of |g(n,l;r,l')|**2
c given n the principal qn and r for all l=[o,n-1] and
c l'=l+1 or l'=l-1.  ref burgess (1964), brockelhurst (1971)
c      author: m. bautista
c
      implicit none
c
      integer n,lpri,lun11
      real*8  cn,clu,cll,g0,gu(100),gl(100),fn,pi,s,r
      real*8  dn,dl
      real*8 f1,rs
      real*8 gus(100),gls(100)
      integer n1,l
c
      dn=dfloat(n)
      r=dble(rs)
      pi=2.d0*dacos(0.d+0)
      n1=2*n-1
      call fact(n1,f1)
      g0=0.5d0*dlog(pi/2.d0)+dlog(8.d0*dn)
     $   +dn*dlog(4.d0*dn)-dble(f1)
        if(r.eq.0.d0) then
        gu(n)=g0-2.d0*dn
        else
      s=dsqrt(r)
      gu(n)=g0-2.d0*datan(dfloat(n)*s)/s
     $  -0.5d0*dlog(1.d0-dexp(-2.d0*pi/s))
        end if
      gu(n)=dexp(gu(n))
c
      fn=1.d-300/gu(n)
      gu(n)=gu(n)*fn
c
      if(n.eq.1) go to 40
      gu(n-1)=(2.d0*dn-1.d0)*(1.d0+dn*dn*r)*dn*gu(n)
      gl(n)=(1.d0+dn*dn*r)*gu(n)/(2.d0*dn)
      gl(n-1)=(2.d0*dn-1.d0)*(4.d0+(dn-1.d0)*(1.d0+dn*dn*r))*gl(n)
c
      do 10 l=n-1,3,-1
      dl=dfloat(l)
      gu(l-1)=(4.d0*dn*dn
     $  -4.d0*dl*dl+dl*(2.d0*dl-1.d0)*(1.d0+dn*dn*r))*gu(l)
      gu(l-1)=gu(l-1)-4*dn*dn*(dn-dl)
     $        *(dn+dl)*(1+(dl+1)*(dl+1)*r)*gu(l+1)
      gl(l-1)=(4*dn*dn-4*(dl-1)*(dl-1)
     $        +(dl-1)*(2*dl-1)*(1+dn*dn*r))*gl(l)
      gl(l-1)=gl(l-1)-4*dn*dn*(dn-dl)
     $        *(dn+dl)*(1+(dl-1)*(dl-1)*r)*gl(l+1)
 10    continue
      gl(1)=0.d0
      gu(1)=(4.d0*dn*dn-16.d0+6.d0*(1.d0+dn*dn*r))*gu(2)
      gu(1)=gu(1)-4.d0*dn*dn*(dn-2.d0)*(dn+2.d0)*(1.d0+9.d0*r)*gu(3)
c
      cn=dlog(dn)-dn*dlog(4.d0*dn*dn)
     $    -(2.d0*dn+4.d0)*dlog(1.d0+dn*dn*r)
      gu(1)=cn+dlog(1.d0+r)+2.d0*dlog(gu(1))-2.d0*dlog(fn)
      clu=cn+dlog(1.d0+r)
      cll=cn
c
      do 30 l=1,n-1
      dl=dfloat(l)
      clu=clu+dlog(4.d0*dn*dn*(dn-dl)
     $    *(dn+dl)*(1.d0+(dl+1.d0)*(dl+1.d0)*r))
      cll=cll+dlog(4.d0*dn*dn*(dn-dl)
     $    *(dn+dl)*(1.d0+(dl-1.d0)*(dl-1.d0)*r))
      gu(l+1)=clu+2.d0*dlog(gu(l+1))-2.d0*dlog(fn)
      gl(l+1)=cll+2.d0*dlog(gl(l+1))-2.d0*dlog(fn)
 30    continue
      go to 60
c
 40    gl(1)=0.d0
      gu(1)=2.d0*dlog(gu(1))-dlog(4.d0)
     $      -5.d0*dlog(1.d0+r)-2.d0*dlog(fn)
      if (lpri.ne.0)
     $ write (lun11,*)'in gull1:',r,fn,gu(1),gu(2),gl(1),gl(2)
c converts results to single precision to give in retudn
       do l=1,100
        gus(l)=sngl(gu(l))
        gls(l)=sngl(gl(l))
       enddo

 60    return
      end
