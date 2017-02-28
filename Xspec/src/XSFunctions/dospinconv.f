      SUBROUTINE dospinconv (ear, ne, param, ifl, photar, tmp)

      INTEGER ne, ifl
      REAL ear(0:ne), param(7), photar(ne), tmp(ne)


c Subroutine to smooth the model spectrum by relativistic effects from a
c disk - xsdili. 
c ACF and RMJ May/June 1998
c
cc Updated by LB, Feb. 2006
c  Updated Mar 2009 - renamed from spinconv to dospinconv, tmp array
c       is now allocated in outer c++ wrapper spinconv.cxx (CG)

c Arguments :
c    ear      r        i: Energy ranges
c    ne       i        i: Number of elements in photar array
c    param    r        i: Model parameters

c  parameters :
c       1        power law index for emissivity (10 for disk)
c       2        inner radius (GM/c**2)
c       3        outer radius (GM/c**2)
c       4        inclination  (degrees)

c    ifl      i        i: Data set
c    photar   r      i/r: Model flux
c    tmp      r        i: Work array


      INTEGER MAXB
      PARAMETER (MAXB=600)

      REAL earb(0:MAXB),photarb(MAXB),photerb(MAXB)
      REAL paramb(9)

      INTEGER i

      LOGICAL first

      SAVE first,earb
      DATA first/.true./
      if (first) then
         do i=0,MAXB
           earb(i)=20.0*float(i)/float(MAXB)
         end do
         first=.false.
      end if
      paramb(1)=10.0
      paramb(2)=param(1)
      paramb(3)=param(2)
      paramb(4)=param(3)
      paramb(5)=param(4)
      paramb(6)=param(5)
      paramb(7)=param(6)
      paramb(8)=param(7)
      paramb(9)=0.0
      call spin(earb,MAXB,paramb,ifl,photarb,photerb)

      CALL doblur(earb, photarb, MAXB, ear, photar, ne, tmp)

      RETURN
      END

