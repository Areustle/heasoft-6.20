      SUBROUTINE dokdblur (ear, ne, param, ifl, photar, tmp)

      IMPLICIT NONE

      INTEGER ne, ifl
      REAL ear(0:ne), param(4), photar(ne), tmp(ne)

c Subroutine to smooth the model spectrum by relativistic effects from a
c disk in the presence of a spinning black hole - users xslaor 
c ACF and RMJ May/June 1998

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
c    photer   r        i: Dynamically allocated workspace array
c                         (This was added Mar 09, removed udmget)


      INTEGER MAXB
      PARAMETER (MAXB=1000)

      REAL earb(0:MAXB), photarb(MAXB), photerb(MAXB)
      REAL paramb(5)

      INTEGER i

      LOGICAL first

      save first,earb
      DATA first/.true./

      if (first) then
         do i=0,MAXB
           earb(i)=20.0*float(i)/float(MAXB)
         end do
         first=.false.
      end if
      paramb(1)=10.
      paramb(2)=param(1)
      paramb(3)=param(2)
      paramb(4)=param(3)
      paramb(5)=param(4)
      call xslaor(earb,MAXB,paramb,ifl,photarb,photerb)

      CALL doblur(earb, photarb, MAXB, ear, photar, ne, tmp)

      RETURN
      END

