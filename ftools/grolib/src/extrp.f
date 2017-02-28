C=======================================================================
      REAL FUNCTION EXTRP(EN1,EN2,GAMMA)
C=======================================================================
C*
C* Returns result of numerical integration of the extrapolation
C* function for high energies f(E)=exp(-E/36 GeV)*E^GAMMA.  Method
C* based on procedures in NUMERICAL RECIPES.
C*
C* Input:
C*   EN1              - real*4; lower energy limit
C*   EN1              - real*4; upper energy limit
C*   GAMMA            - real*4; spectral index
C=======================================================================
C+ $Log: extrp.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:07  irby
C+ Additions to libgro - previously these codes existed in the following
C+ libraries:
C+
C+   libsenstv
C+   libsysutil
C+   libutil
C+   libftio
C+
c Revision 1.1  1996/08/15  17:27:23  programs
c Initial revision
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      real*4 en1, en2, gamma
      real*4 eps, e0
      integer i, k, m
      parameter(eps=1.0e-6, e0=36000.0, k=5)
      real*4 e, de, sum, den
      real*4 s(k), h(k), c(k), d(k)

      character(80)	id

      save

      common	/id/	id
      id = '$Id: extrp.f,v 3.2 2013/05/21 19:08:27 irby Exp $'


      extrp=0.0
      d(1)=1.0
      do i=1,k-1
         h(i)=0.0
      enddo

      de=en2-en1
      s(k)=0.5*de*(exp(-en1/e0)*en1**gamma+exp(-en2/e0)*en2**gamma)
      h(k)=1.0

      do while (abs(d(1)).gt.eps*abs(extrp))
         do i=1,k-1
            s(i)=s(i+1)
            h(i)=h(i+1)
         enddo

         sum=0.0
         e=en1+0.5*de
         do while (e.lt.en2)
            sum=sum+exp(-e/e0)*e**gamma
            e=e+de
         enddo
         de=0.5*de
         s(k)=0.5*s(k-1)+de*sum
         h(k)=0.25*h(k-1)

         if (h(1).gt.0.0) then
            do i=1,k
               c(i)=s(i)
               d(i)=s(i)
            enddo
            extrp=s(k)
            do m=1,k-1
               do i=1,k-m
                  den=(c(i+1)-d(i))/(h(i)-h(i+m))
                  c(i)=h(i)*den
                  d(i)=h(i+m)*den
               enddo
               extrp=extrp+d(k-m)
            enddo
         endif
      enddo

C-----------------------------------------------------------------------
      return
      end
