
      SUBROUTINE clceff(imsiz1, imsiz2, wmsiz1, wmsiz2, wmoff, wbinfac,
     &                  nbin, middl, lower, hiher, siseff, sensor,
     &                  theta, phi, binsze, abnsze, chpmap, grdmap, 
     &                  bemap, work, wmap,pha, pha_lo, pha_hi, 
     &                  xrteff, gispsf, efmap, avgeff, weight)

      INTEGER imsiz1, imsiz2, wmsiz1, wmsiz2, wmoff(2), wbinfac
      INTEGER nbin, sensor, pha_lo, pha_hi
      DOUBLE PRECISION work(imsiz1,imsiz2)
      REAL middl(nbin), lower(nbin), hiher(nbin)
      REAL pha(nbin), siseff(nbin), avgeff(nbin), weight(nbin)
      REAL binsze, abnsze
      REAL theta(imsiz1,imsiz2), phi(imsiz1,imsiz2)
      REAL efmap(imsiz1,imsiz2)
      REAL grdmap(imsiz1,imsiz2), bemap(imsiz1,imsiz2)
      REAL wmap(wmsiz1,wmsiz2)
      INTEGER*2 chpmap(imsiz1,imsiz2)
      LOGICAL xrteff, gispsf

c This routine actually calculates the EFF detector image map
*    sensor        sensor ID (0-3)
*    imsiz1        size of image
*    imsiz2        size of image
*    wmsiz1        size of WMAP
*    wmsiz2        size of WMAP
*    wmoff         WMAP offset in detector coordinate
*    wbinfac       WMAP bin factor
*    nbin          number of pha bins
*    theta         the angle from the optical axes for pixel (i,j) in WMAP
*    phi           the azimuthal angle for pixel (i,j) in WMAP
*    bemap         Be thickness map for GIS
*    grdmap        Grid map for GIS
*    wmap          WMAP
*    binsze        the size (mm) of each WMAP bin
*    abnsze        the size (arcmin) of each WMAP bin
*    middl         mid energies
*    lower         lower energies
*    hiher         higher energies
*    pha           pulse height spectrum
*    siseff        SIS efficiency
*    avgeff        averaged efficiency for each pha bin
*    weight        weight for each pha bin

      INTEGER i, j, k, l, m, ix, iy
      DOUBLE PRECISION deteff, mean, weit, avge, avgnorm, sum
      CHARACTER wrtstr*255

* External functions
*    xrt_eff_area   telescope effective area
*    get_gis_eff    GIS efficiency including thermal shield

      REAL xrt_eff_area
      DOUBLE PRECISION get_gis_eff, trns

      EXTERNAL xrt_eff_area, trns, get_gis_eff

* clear efmap, avgeff, weight array

      sum = 0.D0
      DO k = 1, imsiz2
         DO j = 1, imsiz1
            efmap(j,k) = 0.
         ENDDO
      ENDDO

      DO i = 1, nbin
         avgeff(i) = 0.
         weight(i) = 0.
      ENDDO

* calculate avgnorm

      avgnorm = 0.D0
      DO k = 1, wmsiz2
         DO j = 1, wmsiz1
            IF ( wmap(j,k) .GT. 0. ) THEN
               avgnorm = avgnorm + DBLE(wmap(j,k))
            ENDIF
         ENDDO
      ENDDO
      IF ( avgnorm .GT. 0 ) THEN
         avgnorm = 1.D0 / avgnorm / DBLE(wbinfac) / DBLE(wbinfac)
      ENDIF

* Loop round the energies. First initialize the ARF array

      CALL fcecho(' ')
      DO i = pha_lo, pha_hi

         WRITE(wrtstr, '(a,i4,a,f6.3,a,f6.3,a,i4,a,i4,a)')
     &        'pha bin = ', i,
     &        ', E = ', lower(i), ' - ', hiher(i), ' keV  (',
     &        i-pha_lo+1,' / ',pha_hi-pha_lo+1,
     &        ')   calculating ...'
         CALL fcecho(wrtstr)

* if weight of this energy are 0, do nothing.

         IF ( pha(i) .eq. 0. ) GOTO 100

* calculate the detector efficiency at each position

         IF ( sensor .GE. 2 ) THEN

            DO k = 1, imsiz2
               DO j = 1, imsiz1

* Calculate the detector efficiency for this pixel
* Note that get_gis_eff() includes thermal shield efficiency

* 25Jul97 MJT: using ascaarf version of get_gis_eff with
*              extra parameter=0 is identical to original

                  work(j,k) = 
     &                 DBLE(grdmap(j,k)) *
     &                 get_gis_eff(DBLE(lower(i)),
     &                                   DBLE(hiher(i)),
     &                                   DBLE(bemap(j,k)), 0)

               ENDDO
            ENDDO

            IF ( gispsf ) THEN

               CALL giscnv(DBLE(middl(i)), imsiz1, imsiz2, work,
     &                     DBLE(binsze), DBLE(abnsze))

            ENDIF

         ELSE

            deteff = DBLE( siseff(i) ) * TRNS(DBLE(middl(i)), sensor)

            DO k = 1, imsiz2
               DO j = 1, imsiz1

* Calculate the detector efficiency for this pixel
* SIS efficiency must be multiplied here, but I don't know the function.

                  work(j,k) =  deteff * chpmap(j,k)

               ENDDO
            ENDDO

         ENDIF

* calculate the XRT efficiency at each position

         IF ( xrteff ) THEN

            DO k = 1, imsiz2
               DO j = 1, imsiz1

                  IF ( work(j,k) .GT. 0. ) THEN
                     work(j,k) = work(j,k) * DBLE(
     &                           xrt_eff_area(lower(i), hiher(i),
     &                                        theta(j,k), phi(j,k)))
                  ENDIF

               ENDDO
            ENDDO
         ENDIF

* Loop round the pixels in the WMAP

         avge = 0.D0
         DO k = 1, wmsiz2
            iy = wmoff(2) + (k-1)*wbinfac - 1
            DO j = 1, wmsiz1

               IF ( wmap(j,k) .GT. 0. ) THEN

                  mean = 0.D0
                  ix = wmoff(1) + (j-1)*wbinfac - 1

                  DO l = 1, wbinfac
                     DO m = 1, wbinfac
                        mean = mean + work(ix+l, iy+m)
                     ENDDO
                  ENDDO

                  avge = avge + DBLE(wmap(j,k)) * mean

ccc                  IF ( mean .GT. 0.D0 ) THEN
ccc                     weit = weit + DBLE(wmap(j,k)) / mean
ccc                  ENDIF

               ENDIF

            ENDDO
         ENDDO

* multiply the resultant by the PHA weight at this energy

ccc         weit = weit * DBLE(pha(i))
         IF ( avge .NE. 0. ) THEN
            weit = DBLE(pha(i)) / avge
         ELSE
            weit = 0.
         ENDIF
         sum = sum + weit

* sum up work to efmap with weight

         DO k = 1, imsiz2
            DO j = 1, imsiz1
               IF ( work(j,k) .GT. 0. ) 
     &               efmap(j,k) = efmap(j,k) + REAL( weit * work(j,k) )
            ENDDO
         ENDDO

* set weight and avgeff array

         weight(i) = REAL(weit)
         avgeff(i) = REAL(avgnorm * avge)
         WRITE(wrtstr,'(a,1pg14.7,a,1pg14.7)')
     &         ' <efficiency> = ', avgeff(i), ' weight = ', weight(i)
         CALL fcecho(wrtstr)

* end loop over energies

 100     CONTINUE

      ENDDO

* scale efmap, weight array

      IF ( sum .gt. 0 ) THEN

         DO k = 1, imsiz2
            DO j = 1, imsiz1
               efmap(j,k) = efmap(j,k) / sum
            ENDDO
         ENDDO

         DO i = pha_lo, pha_hi
            weight(i) = weight(i) / sum
         ENDDO

      ENDIF

      RETURN
      END
