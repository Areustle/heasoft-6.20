      SUBROUTINE dordblur (ear, ne, param, ifl, photar, tmp)

      IMPLICIT NONE

      INTEGER ne, ifl
      REAL ear(0:ne), param(4), photar(ne), tmp(ne)

c Subroutine to smooth the model spectrum by relativistic effects from a
c disk in the presence of a non-spinning black hole - uses xsdili. 
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
c    tmp      r        i: Dynamically allocated workspace array
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
      call xsdili(earb,MAXB,paramb,ifl,photarb,photerb)

      CALL doblur(earb, photarb, MAXB, ear, photar, ne, tmp)

      RETURN
      END

c ************************************************************************
c General routine for blurring by a line given by earb, photarb. Used by 
c rdblur, kdblur, kdblur2.

      SUBROUTINE doblur(earb, photarb, maxb, ear, photar, ne, tmp)

      IMPLICIT NONE

      INTEGER maxb, ne

      REAL earb(0:maxb), photarb(maxb)
      REAL ear(0:ne), photar(ne), photer(ne), tmp(ne)

c earb and photarb  are energies and fluxes for line
c ear and photar are input energies and fluxes to be blurred

c FRACT is the fraction of the line shape integral used in the convolution
      
      REAL FRACT
      PARAMETER (FRACT=0.99999)

      REAL energy, sum
      REAL r1, r2, f1, f2
      REAL eratlow, erathigh, estart, estop

      INTEGER halfmaxb
      INTEGER ie, je, i
      INTEGER if, if1, if2
      INTEGER ilow, ihigh, jlow, jhigh

      halfmaxb = maxb/2

c normalize line to an integral of 1

      sum=0.
      do i=1,maxb
         sum=sum+photarb(i)
      enddo
      IF ( sum .NE. 0.0 .AND. sum .NE. 1.0 ) THEN
         do i=1,maxb
            photarb(i)=photarb(i)/sum
         enddo
      ENDIF

c trap out the pathological case of sum = 0. In this case don't alter the
c input spectrum

      IF ( sum .EQ. 0.0 ) RETURN

c find range which encompasses FRACT of the area under
c the line symmetrically from the center

      sum = 0.0
      ilow = MAXB/2-1
      ihigh = MAXB/2
      DO WHILE ( sum .LE. FRACT )
         IF ( ilow .GE. 0 ) THEN
            sum = sum + photarb(ilow)
            ilow = ilow - 1
         ENDIF
         IF ( ihigh .LE. MAXB ) THEN
            sum = sum + photarb(ihigh)
            ihigh = ihigh + 1
         ENDIF
      ENDDO

      IF ( ilow .LT. 0 ) ilow = 0
      IF ( ihigh .GT. MAXB) ihigh = MAXB

      eratlow = earb(ilow)/10.0
      erathigh = earb(ihigh)/10.0

c Initialize the convolution output

      do ie=1,ne
         tmp(ie)=0.0
      enddo

c Loop over energy ranges for the current dataset

      jlow = 1
      jhigh = 1

      do ie=1,ne

         energy=0.5*(ear(ie-1)+ear(ie))

c find first and last bins required in the convolution

         estart = energy * eratlow
         estop = energy * erathigh

         DO WHILE ( ear(jlow) .LT. estart .AND. jlow .LT. ne )
            jlow = jlow + 1
         ENDDO
         IF ( jlow .GT. 1 ) jlow = jlow - 1
         DO WHILE ( ear(jhigh) .LT. estop .AND. jhigh .LT. ne )
            jhigh = jhigh + 1
         ENDDO

         do je = jlow, jhigh
            f1=ear(je-1)/energy*float(halfmaxb)*
     :           (1.0+1.0/float(halfmaxb))
            f2=ear(je)/energy*float(halfmaxb)*
     :           (1.0+1.0/float(halfmaxb))
            if1=INT(f1)
            r1=f1-if1
            if2=INT(f2)
            r2=f2-if2
            sum=0.0

            do if=if1+1,if2-1
               if(if.ge.1.and.if.le.maxb)then
                  sum=sum+photarb(if)
               endif
            end do
             
            if(if1.ge.1.and.if1.le.maxb)then
               sum=sum+(1.0-r1)*photarb(if1)
            end if
             
            if(if2.ge.1.and.if2.le.maxb)then
               sum=sum+(r2)*photarb(if2)
            end if
             
            if (if1 .eq. if2) then
               if(if1.ge.1.and.if1.le.maxb)then
                  sum=sum-photarb(if1)
               end if
            end if
             
            tmp(je)=tmp(je)+photar(ie)*sum
         enddo
      enddo

      DO ie = 1, ne
         photar(ie) = tmp(ie)
         photer(ie) = photer(ie)
      ENDDO

      RETURN
      END

