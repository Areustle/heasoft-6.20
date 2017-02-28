      subroutine hgf(ia,ib,ic,x,hyp)
c
c     subroutine hgf calculates the value, hyp, of the
c     hypergeometric fn at x for constants ia,ib,ic
c     real*8  ser,hyp
c     author:  M. Bautista
c
      implicit none
      integer ia,ib,ic,i,j,n
      real*8 x,hyp,ser
c
      ser=1.
      hyp=1.
      i=-ia
      j=-ib
      i=min(i,j)
      do 10 n=0,i
      ser=ser*(ia+n)*(ib+n)*x/((n+1.)*(ic+n))
      hyp=hyp+ser
 10    continue
c
      return
      end
