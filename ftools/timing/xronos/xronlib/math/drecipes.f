c
c Contained in this file are Numerical Recipes routines for
c gamma functions.  These have been converted to double
c precision and adapted to HEASARC standard.
c
c Eric Lufkin  January, 1994.
c
c*********
c*********
      double precision function gammln(xx)

c Return the log of the gamma function for xx > 1.
c This routine is taken from Numerical Recipes, p. 157.

      implicit none
      integer j
      double precision cof(6), stp, haf, one, fpf, x, tmp, ser, xx
      data cof,stp /76.18009173d0, -86.50532033d0, 24.01409822d0
     &            ,-1.23739516d0, .120858003d-2, -.5356382d-5
     &            ,2.50662827465d0/
      data haf,one,fpf /0.5d0, 1.d0, 5.5d0/

      x=xx-one
      tmp=x+fpf
      tmp=(x+haf)*dlog(tmp)-tmp
      ser=one
      do j=1,6
         x=x+one
         ser=ser+cof(j)/x
      enddo
      gammln=tmp+dlog(stp*ser)

      return
      end
c*********
c*********
      double precision FUNCTION GAMMP(A,X)
      implicit none
      double precision a, x, gln, gammcf

      IF(X.LT.0.d0.OR.A.LE.0.d0) then
         call xwrite(' Warning bad argument to gammp', 10)
c         call xaerror('You should not see this.', 5)
c         stop
      endif
      IF(X.LT.A+1.d0)THEN
        CALL GSER(GAMMP,A,X,GLN)
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMP=1.d0-GAMMCF
      ENDIF
      RETURN
      END
c*********
c*********
      double precision FUNCTION GAMMQ(A,X)
      implicit none
      double precision a, x, gamser, gln
      character(80) context


      IF(X.LT.0.d0.OR.A.LE.0.d0) then
         call xwrite('bad argument to gammq', 10)
         write(context,'(''A='',g22.17,''X='',g22.17)')a,x
         call xwrite(context, 10)
         call xwrite(' Warning the rms is not correct',10)
C         call xaerror('You should not see this.', 5)
C     stop
      endif
      IF(X.LT.A+1.d0)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMQ=1.d0-GAMSER
      ELSE
        CALL GCF(GAMMQ,A,X,GLN)
      ENDIF
      RETURN
      END
c*********
c*********
      subroutine gcf(gammcf,a,x,gln)

      implicit  none
      integer itmax, n
      character(80) context
      double precision eps, gln, a, gammln
     &   , gold, a0, a1, x, b0, b1, fac, an, ana, anf, g, gammcf
c      parameter(itmax=100,eps=3.d-7)
      parameter(itmax=5000,eps=3.d-7)

      
c  Initialize to avoid warning
      g = 0.d0
c  --
      gold=0.d0
      a0=1.d0
      a1=x
      b0=0.d0
      b1=1.d0
      fac=1.d0
      do n=1,itmax
         an=dble(n)
         ana=an-a
         a0=(a1+a0*ana)*fac
         b0=(b1+b0*ana)*fac
         anf=an*fac
         a1=x*a0+anf*a1
         b1=x*b0+anf*b1
         if(a1.ne.0.d0) then
            fac=1.d0/a1
            g=b1*fac
            if(dabs((g-gold)/g).lt.eps) go to 1
            gold=g
         endif
      enddo
      call xwrite('A is too large, ITMAX is too small in gammcf.', 10)
      write(context,'(''A='',g22.17,''X='',g22.17)')a,x
      call xwrite(context,10)
      call xwrite(' Warning the rms is not correct',10)
c      call xaerror('You should not see this.', 5)
1     gln=gammln(a)
      gammcf=dexp(-x+a*dlog(x)-gln)*g

      return
      end
c*********
c*********
      subroutine gser(gamser,a,x,gln)

      implicit none
      integer itmax, n
      character(80) context
      double precision eps, x, a, gamser, gammln, ap, sum, del, gln
c      parameter (itmax=100,eps=3.d-7)
      parameter (itmax=5000,eps=3.d-7)

      if(x.le.0.d0) then
         gamser=0.d0
         return
      endif
      ap=a
      sum=1.d0/a
      del=sum
      do n=1,itmax
         ap=ap+1.d0
         del=del*x/ap
         sum=sum+del
         if(dabs(del).lt.dabs(sum)*eps) go to 1
      enddo
      call xwrite('A is too large, ITMAX is too small in gser.',10)
      write(context,'(''A='',g22.17,''X='',g22.17)')a,x
      call xwrite(context,10)
      call xwrite(' Warning the rms is not correct',10)
c      call xaerror('You should not see this.', 5)
c      stop
1     gln=gammln(a)
      gamser=sum*dexp(-x+a*dlog(x)-gln)

      return
      end




