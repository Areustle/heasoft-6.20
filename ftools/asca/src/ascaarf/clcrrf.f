
      SUBROUTINE clcrrf(wmsiz1, wmsiz2, wmoff, wbinfac, nenerg, 
     &                  middl, lower, hiher, instrum, psf, binsze, 
     &                  abnsze, wmap, bkgmap, grdmap, bemap, optaxs,
     &                  arf, fudge, fillist, nrayen, rayene, nraymp1, 
     &                  nraymp2, raymp1, raymp2, raympi, rbnsze, 
     &                  raycen)

      INTEGER wmsiz1, wmsiz2, wmoff(2), wbinfac, nenerg, instrum
      INTEGER nrayen, nraymp1, nraymp2
      REAL arf(nenerg), optaxs(2), raycen(2)
      REAL middl(nenerg), lower(nenerg), hiher(nenerg)
      REAL binsze, abnsze, rbnsze
      DOUBLE PRECISION psf(wmsiz1,wmsiz2)
      REAL wmap(wmsiz1,wmsiz2), grdmap(wmsiz1,wmsiz2)
      REAL bemap(wmsiz1,wmsiz2), bkgmap(wmsiz1,wmsiz2)
      REAL rayene(nrayen), raymp1(nraymp1,nraymp2)
      REAL raymp2(nraymp1,nraymp2), raympi(nraymp1,nraymp2)
      LOGICAL fudge
      CHARACTER fillist*(*)

c This routine actually calculates the ARF for a raytraced source.
c Based on CLCPRF.
*    instrum       instrument (0-3)
*    wmsiz1        size of WMAP
*    wmsiz2        size of WMAP
c    wmoff         first detector image pixel in WMAP
c    wbinfac       WMAP bin factor
*    nenerg        number of energies
*    wmap          the WMAP
*    bkgmap        the optional background map
*    bemap         Be thickness map for GIS
*    grdmap        Grid map for GIS
*    psf           The PSF map
*    binsze        the size (mm) of each WMAP bin
*    abnsze        the size (arcmin) of each WMAP bin
*    middl         mid energies
*    lower         lower energies
*    hiher         higher energies
*    optaxs        optical axis in detector coordinates
*    arf           effective areas
*    fudge         If true the gaussian effective area fudge will be applied
*    fillist       File containing list of raytrace files
*    nrayen        Number of raytrace energies
*    rayene        Vector of raytrace energies
*    nraymp1       X-axis size of raytrace image
*    nraymp2       Y-axis size of raytrace image
*    raymp1        Raytrace image
*    raymp2        Raytrace image
*    raympi        Raytrace image
*    rbnsze        The size of the raytrace image bins (in mm)
*    raycen        The optical axis position in raytrace image coordinates

      INTEGER i, j, k, ifudge
      REAL deteff, xrteff, x, y, xoff, yoff, ratio, ratio2
      DOUBLE PRECISION dlo, dhi, dbe, dgis, sum

      character(255) wrtstr

* External functions
*    rayres      gets the raytracing result
*    trns        thermal precollimator transmission
*    get_gis_eff GIS efficiency including thermal shield
*    sisfdg      the SIS efficiency fudge

      REAL rayres
      DOUBLE PRECISION get_gis_eff, sisfdg, trns

      EXTERNAL trns, get_gis_eff, rayres

* Set the fudge correction flag

      IF ( fudge ) THEN
         ifudge = 0
      ELSE
         ifudge = 1
      ENDIF

c These somewhat unwieldy expressions convert from WMAP coordinates to
c raytrace map coordinates via the optical axis positions.

      xoff = wmoff(1) - wbinfac - 0.5*(wbinfac-1) - optaxs(1)
      xoff = xoff * binsze/rbnsze/wbinfac + raycen(1)
      yoff = wmoff(2) - wbinfac - 0.5*(wbinfac-1) - optaxs(2)
      yoff = yoff * binsze/rbnsze/wbinfac + raycen(2)

c The ratio of the WMAP binsize to the raytrace image binsize

      ratio = binsze / rbnsze
      ratio2 = ratio**2

* Loop round the energies. First initialize the ARF array

      DO i = 1, nenerg

         arf(i) = 0.

* If the energy has been set to < 0 then outside the valid range

         IF ( middl(i) .LT. 0. ) GOTO 100

* Since the raytrace includes the XRT efficiency we set xrteff to 1.

         xrteff = 1.0

* calculate the PSF map here. Loop round the WMAP bins. x and y are the
* positions in raytrace map coordinates.

         sum = 0.d0
         DO k = 1, wmsiz2
            y = k * ratio + yoff
            DO j = 1, wmsiz1
               x = j * ratio + xoff

* get the raytrace result for this WMAP bin (for present assume that
* the raytrace map is in WMAP bins)

               psf(j,k) = DBLE(rayres(middl(i), x, y, fillist, ratio,
     &                                nrayen, rayene, nraymp1, 
     &                                nraymp2, raymp1, raymp2, raympi)
     &                         )

               sum = sum + psf(j,k)

            ENDDO
         ENDDO

* If the instrument is the GIS then convolve the XRT PSF with the
* GIS PSF

         IF ( instrum .GE. 2 ) THEN

            DO k = 1, wmsiz2
               DO j = 1, wmsiz1
                  
* Calculate the detector efficiency for this pixel
* Note that get_gis_eff() includes thermal shield efficiency

                  dlo = DBLE(lower(i))
                  dhi = DBLE(hiher(i))
                  dbe = DBLE(bemap(j,k))
                  dgis = get_gis_eff(dlo, dhi, dbe, ifudge)
                  deteff = grdmap(j,k) * dgis
                  psf(j,k) = deteff * psf(j,k)

               ENDDO
            ENDDO
            CALL giscnv(DBLE(middl(i)), wmsiz1, wmsiz2, psf, 
     &                  DBLE(binsze), DBLE(abnsze))
         ELSE

* If the SIS is in use then include the thermal shield and, if required,
* the efficiency fudge factor.

            xrteff = xrteff * SNGL(TRNS(DBLE(middl(i)), instrum))
            IF ( fudge ) xrteff = xrteff
     &            * SNGL(SISFDG(DBLE(lower(i)), DBLE(hiher(i))))

         ENDIF

* Loop round the pixels in the WMAP

         DO k = 1, wmsiz2
            DO j = 1, wmsiz1

               IF ( wmap(j,k) .GE. 0. ) THEN

* If the pixel was part of the selected region then the contribution to the 
* ARF for this pixel is the product of the detector efficiency and the PSF

                  arf(i) = arf(i) + psf(j,k)

               ENDIF

            ENDDO
         ENDDO

* multiply the resultant by the efficiency factor.

         arf(i) = arf(i) * xrteff

* end loop over energies

 100     CONTINUE

      ENDDO

      RETURN
      END


