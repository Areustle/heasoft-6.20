C..

      SUBROUTINE mlmekl(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(6), photar(ne), photer(ne)

c
c XSPEC model subroutine to calculate a MEKAL model with multiple temperatures
c according to Mike L's models for A496.
c 
c Parameters:
c    param(1) = option (1,2,3)
c    param(2) = nH (cm^-3)  Fixed at 1 for most applications
c    param(3) = metallicity used in the Mewe-Kaastra plasma model
c    param(4) = redshift used in the Mewe-Kaastra plasma model
c    param(5) = switch(0=calculate MEKAL model, 1=interpolate MEKAL model)
c
c
c Declare variables:
c
 
      REAL pparam(18)

      INTEGER i


      DO i = 1, 2
         pparam(i) = param(i)
      ENDDO
      pparam(3) = 1.
      DO i = 4, 16
         pparam(i) = param(3)
      ENDDO
      pparam(17) = param(4)
      pparam(18) = param(5)

      CALL mlvmkl(ear, ne, pparam, ifl, photar)

      RETURN
      END







