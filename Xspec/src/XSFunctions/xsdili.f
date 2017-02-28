
      SUBROUTINE xsdili(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(5), photar(ne), photer(ne)

c  model to calculate the line shape for a rotating accretion
c  disk. does not include GR effects. note that if param(2) is
c  set to 10 then do the special case of an accretion disk
c  emissivity.

c  parameters :
c	1        line energy
c       2        power law index for emissivity (10 for disk)
c       3        inner radius (GM/c**2)
c       4        outer radius (GM/c**2)
c       5        inclination  (degrees)

      INTEGER nr
      PARAMETER (nr=100)

      INTEGER n, k, ie
      REAL ri, ro, dlogr, dera, alp,
     &     tgcsi2, cosdel, fra, ra, dra,
     &     rd(nr), enel, spm, rilog10
      REAL beta, betal, betah, zpo, zpol, zpoh
      REAL sincl, cincl, rafact, flux, total
      REAL enobl, enobh

      DATA dera /57.295779/

c suppress a warning message from the compiler
      ie = ifl

c this model does not calculate errors
      DO ie = 1, ne
         photer(ie) = 0.0
      ENDDO

c convert input parameters - inclination set to radians.

      enel = param(1)
      alp = param(2)
      ri = param(3)
      rilog10 = alog10(ri)
      ro = param(4)

      sincl = sin(param(5)/dera)
      cincl = cos(param(5)/dera)

c set spectrum to 0

      DO n = 1, ne
         photar(n) = 0.
      ENDDO

c trap case where inner radius is greater than outer

      IF ( ri .GE. ro ) THEN
         CALL xaerror(
     &     'Inner radius > outer radius  -  model is invalid', 5)
         RETURN
      ENDIF

c trap case of inner radius being less than the last stable orbit.

      IF  (ri .LT. 6.) THEN
         CALL xaerror('Inner radius < 6 R_g  -  model is invalid', 5)
         RETURN
      ENDIF

c calculate log step in radius

      dlogr = (alog10(ro)-rilog10)/float(nr-1)

c calculate radii

      rd(1) = ri
      DO n = 2, nr
         rd(n) = 10.**(rilog10+float(n-1)*dlogr)
      ENDDO

c big loop for radii. note that the radius cannot reach 3 else
c the metric goes singular

      DO n = 1, nr - 1

         ra = (rd(n)+rd(n+1))/2.

         dra = rd(n+1) - rd(n)

         rafact = sqrt(1.-3./ra)

c if power-law index is less than ten use to calculate emissivity

         IF (alp.LT.9.9) THEN

            fra = ra**(alp)

c else use the accretion disk emissivity law.

         ELSE

            fra = (1.-sqrt(6./ra))/ra**3

         ENDIF

c loop over azimuthal angles

         DO k = 1, 179, 2

            beta = float(k)/dera
            betal = float(k-1)/dera
            betah = float(k+1)/dera

c calculate mean redshift (1+z = zpo) for the bin

            tgcsi2 = (sincl*sin(beta))
     &               **2/(1.-(sincl*sin(beta))**2)
            cosdel = sincl*cos(beta)
     &               /sqrt((sincl*cos(beta))**2+cincl**2)
            zpo = (1.+cosdel/sqrt(ra*(1.+tgcsi2)-2.))/rafact

c and the low and high redshifts for the bin. note the traps for
c the case of an inclination of 90 degrees.


            IF ( param(5) .GT. 89.9 .AND. k .EQ. 91 ) THEN

               zpol = 1./rafact

            ELSE

               tgcsi2 = (sincl*sin(betal))
     &                  **2/(1.-(sincl*sin(betal))**2)
               cosdel = sincl*cos(betal)
     &                  /sqrt((sincl*cos(betal))**2+cincl**2)
               zpol = (1.+cosdel/sqrt(ra*(1.+tgcsi2)-2.))/rafact

            ENDIF

            IF ( param(5) .GT. 89.9 .AND. k .EQ. 89 ) THEN

               zpoh = 1./rafact

            ELSE

               tgcsi2 = (sincl*sin(betah))
     &                  **2/(1.-(sincl*sin(betah))**2)
               cosdel = sincl*cos(betah)
     &                  /sqrt((sincl*cos(betah))**2+cincl**2)
               zpoh = (1.+cosdel/sqrt(ra*(1.+tgcsi2)-2.))/rafact

            ENDIF

c  enobl and enobh are the lower and upper observed energy from
c  this azimuthal and radial bin

            enobl = min(enel/zpol, enel/zpoh)
            enobh = max(enel/zpol, enel/zpoh)
            total = enobh - enobl

c  calculate total emission from this bin

            flux = fra*ra*dra*2./dera/zpo**3

c  find fractions of emission from this bin to place in each energy
c  range.

            IF (enobh .LT. ear(0) .OR. enobl .GT. ear(ne) ) GOTO 10

            DO ie = 1, ne
               IF ( ear(ie) .GE. enobh ) THEN
                  IF ( ear(ie-1) .LE. enobl ) THEN
                     photar(ie) = photar(ie) + flux
                     GOTO 10
                  ELSEIF ( ear(ie-1) .GE. enobh ) THEN
                     GOTO 10
                  ELSE
                     IF ( total .GT. 0. ) THEN
                        photar(ie) = photar(ie) 
     &                     + flux*(enobh-ear(ie-1))/total
                     ENDIF
                  ENDIF
               ELSEIF ( ear(ie) .GE. enobl ) THEN
                  IF ( ear(ie-1) .GE. enobl ) THEN
                     IF ( total .GT. 0. ) THEN
                        photar(ie) = photar(ie) 
     &                     + flux*(ear(ie)-ear(ie-1))/total
                     ENDIF
                  ELSE
                     IF ( total .GT. 0. ) THEN
                        photar(ie) = photar(ie) 
     &                     + flux*(ear(ie)-enobl)/total
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO

 10         CONTINUE

         ENDDO
      ENDDO

c normalise values to total

      spm = 0.
      DO n = 1, ne
         spm = spm + photar(n)
      ENDDO

c write values

      IF ( spm .NE. 0 ) THEN
         DO n = 1, ne
            photar(n) = photar(n)/spm
         ENDDO
      ENDIF

      RETURN
      END
