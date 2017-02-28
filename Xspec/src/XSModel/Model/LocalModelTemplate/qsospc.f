      SUBROUTINE qsospc(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(4), photar(ne), photer(ne)

C---
C QSO spectrum.
C---
C Parameters
C	1	PHOTON power law index for optical-IR (2500 A - )
C	2	PHOTON power law index from X-ray band to optical (2 keV - 2500 A)
C	3	PHOTON power law index for X-ray band (> 2 keV)
C       4       Redshift

C Normalization is at 2 keV.


C model N(E) = ((2500A)**(par1-par2)) (E**-par1) [E < 2500A]
C	     = (2**par2) (E**-par2)              [2500 A < E < 2 keV]
C            = (2**par3) (E**-par3)              [E>2 keV]
C where E is the emitted frame energy and 2500A indicates the energy that
C corresponds to 2500A = 4.9594E-03 keV.

C--
      REAL ebreak1, ebreak2, norm, zfac
      INTEGER i, ibreak1, ibreak2, tne
C---

C Shift the energies to the emitted frame

      zfac = 1. + param(4)
      DO i = 0, ne
         ear(i) = ear(i) * zfac
      ENDDO

C Find the energy ranges that correspond to the break energies

      ebreak1 = 4.9594E-03
      ebreak2 = 2.0

      ibreak1 = 0
      ibreak2 = 0

      DO i = 1, ne

         IF ( ear(i-1) .LT. ebreak1 .AND. ear(i) .GE. ebreak1 ) THEN
            ibreak1 = i
         ENDIF

         IF ( ear(i-1) .LT. ebreak2 .AND. ear(i) .GE. ebreak2 ) THEN
            ibreak2 = i
         ENDIF

         photar(i) = 0.
         photer(i) = 0.

      ENDDO

      IF ( ear(ne) .LT. ebreak1 ) ibreak1 = ne + 1
      IF ( ear(ne) .LT. ebreak2 ) ibreak2 = ne + 1

c Calculate the three sections of the spectrum by calling xszplw for each
c part. First the optical-IR section.

      IF ( ibreak1 .GT. 0 ) THEN

         tne = MIN(ibreak1, ne)
         norm = 2**param(2) * ebreak1**(param(1)-param(2))

         CALL xspwlw(ear, tne, param(1), ifl, photar, photer)

         DO i = 1, tne
            photar(i) = photar(i) * norm
            photer(i) = photer(i) * norm
         ENDDO

      ENDIF

c Now the 2500A to 2 keV section

      IF ( ibreak2 .GT. 0 ) THEN

         tne = MIN(ibreak2-ibreak1, ne-ibreak1)
         norm = 2.0**param(2)

         CALL xspwlw(ear(ibreak1), tne, param(2), ifl, 
     &               photar(ibreak1+1), photer(ibreak1+1))

         DO i = ibreak1+1, ibreak1+tne
            photar(i) = photar(i) * norm
            photer(i) = photer(i) * norm
         ENDDO

      ENDIF

c Finally the > 2 keV section

      IF ( ibreak2 .LE. ne ) THEN

         tne = ne-ibreak2
         norm = 2.0**param(3)

         CALL xspwlw(ear(ibreak2), tne, param(3), ifl, 
     &               photar(ibreak2+1), photer(ibreak2+1))

         DO i = ibreak2+1, ibreak2+tne
            photar(i) = photar(i) * norm
            photer(i) = photer(i) * norm
         ENDDO

      ENDIF

C Shift the energies back to the observed frame and correct flux by the
C time dilation factor

      DO i = 0, ne
         ear(i) = ear(i) / zfac
      ENDDO
      DO i = 1, ne
         photar(i) = photar(i) / zfac
      ENDDO

      RETURN
      END
