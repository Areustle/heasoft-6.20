      subroutine hunt3(xx,n,x,jlo,lpri,lun11)
c
      implicit none
c
      integer n,jlo,lpri,lun11,nint,jhi,inc,jm,nintmx
      real*8 xx(n),x
      logical ascnd
c
      data nintmx/1000/
c
      nint=0
      if (lpri.gt.1) write (lun11,*)'in hunt',n,x
c      do m=1,n
c        write (lun11,*)m,xx(m)
c        enddo
      jlo=1
      ascnd=.false.
      if (xx(n).gt.xx(1)) ascnd=.true.
      if(jlo.le.0.or.jlo.gt.n)then
        jlo=1
        jhi=n+1
        if (lpri.gt.1) write (lun11,*)'initializing',jlo,jhi
        go to 3
      endif
      inc=1
      if(x.ge.xx(jlo).eqv.ascnd)then
1       jhi=jlo+inc
        if (lpri.gt.1) write (lun11,*)'hunt up ',jlo,jhi,xx(jlo),xx(jhi)
        if(jhi.gt.n)then
          jhi=n+1
        else if(x.ge.xx(jhi).eqv.ascnd)then
          jlo=jhi
          inc=inc+inc
          if (lpri.gt.1) write (lun11,*)'double the increment',inc
          go to 1
        endif
      else
        jhi=jlo
2       jlo=jhi-inc
        if (lpri.gt.1) write (lun11,*)'hunt down ',jlo,jhi,xx(jlo),
     $                        xx(jhi)
        if(jlo.lt.1)then
          jlo=0
        else if(x.lt.xx(jlo).eqv.ascnd)then
          jhi=jlo
          inc=inc+inc
          if (lpri.gt.1) write (lun11,*)'double the increment',inc
          go to 2
        endif
      endif
      jm=n/2
3     continue
      if (lpri.gt.1) write (lun11,*)'bisection phase',jlo,jhi,jm,xx(jm)
      jlo=min(jlo,n)
      jlo=max(jlo,1)
      nint=nint+1
      if (lpri.gt.1) write (lun11,*)'bisection phase',jlo,jhi,jm,xx(jm)
      if ((jhi-jlo.eq.1).or.(nint.gt.nintmx)) return
      if (lpri.gt.1) write (lun11,*)'bisection phase',jlo,jhi,jm,xx(jm)
      jm=(jhi+jlo)/2
      if(x.gt.xx(jm).eqv.ascnd)then
        jlo=jm
      else
        jhi=jm
      endif
      go to 3
c
      end
