
      SUBROUTINE clcerf(wmsiz1, wmsiz2, nenerg, middl, lower, hiher, 
     &                  instrum, theta, phi, psf, binsze, abnsze, wmap, 
     &                  bkgmap, grdmap, bemap, work, waoaa, arf, fudge)

      INTEGER wmsiz1, wmsiz2, nenerg, instrum
      REAL waoaa, arf(nenerg), middl(nenerg), lower(nenerg)
      REAL hiher(nenerg)
      REAL binsze, abnsze
      REAL theta(wmsiz1,wmsiz2), phi(wmsiz1,wmsiz2)
      DOUBLE PRECISION psf(wmsiz1,wmsiz2)
      REAL work(wmsiz1,wmsiz2)
      REAL wmap(wmsiz1,wmsiz2), grdmap(wmsiz1,wmsiz2)
      REAL bemap(wmsiz1,wmsiz2), bkgmap(wmsiz1,wmsiz2)
      LOGICAL fudge

c This routine actually calculates the ARF for an extended source. This 
c probably works OK for flattish surface brightness profiles.
*    instrum       instrument (0-3)
*    wmsiz1        size of WMAP
*    wmsiz2        size of WMAP
*    nenerg        number of energies
*    wmap          the WMAP
*    theta         the angle from the optical axes for pixel (i,j) in WMAP
*    phi           the azimuthal angle for pixel (i,j) in WMAP
*    bkgmap        the optional background map
*    bemap         Be thickness map for GIS
*    grdmap        Grid map for GIS
*    psf           The PSF map
*    binsze        the size (mm) of each WMAP bin
*    abnsze        the size (arcmin) of each WMAP bin
*    middl         mid energies
*    lower         lower energies
*    hiher         higher energies
*    arf           effective areas
*    waoaa         weighted off-axis angle
*    fudge         if true then apply efficiency fudge

      INTEGER i, j, k, ifudge
      REAL sumwmap, deteff, xrteff
      DOUBLE PRECISION dlo, dhi, dbe, dgis

      character(255) wrtstr

* External functions
*    xrtefa   telescope effective area
*    trns     thermal precollimator transmission
*    get_gis_eff   GIS efficiency including thermal window
*    sisfdg   the fudge to the SIS efficiency

      REAL xrtefa
      DOUBLE PRECISION get_gis_eff, sisfdg, trns

      EXTERNAL xrtefa, trns, get_gis_eff, sisfdg

* set the fudge correction flag

      ifudge = 1
      IF ( fudge ) ifudge = 0

* Calculate the WMAP sum and the weighted mean off-axis angle

      waoaa = 0.
      sumwmap = 0.
      DO k = 1, wmsiz2
         DO j = 1, wmsiz1
            IF ( wmap(j,k) .GE. 0. ) THEN
               sumwmap = sumwmap + wmap(j,k) - bkgmap(j,k)
               waoaa = waoaa + theta(j,k) * (wmap(j,k) - bkgmap(j,k))
            ENDIF
         ENDDO
      ENDDO
      IF ( sumwmap .NE. 0. ) THEN
         waoaa = waoaa/sumwmap
      ELSE
         waoaa = 0.
      ENDIF

      CALL fcecho(' ')
      WRITE(wrtstr,'(a,1pe11.5)') ' Total counts in region = ', sumwmap
      CALL fcecho(wrtstr)
      WRITE(wrtstr,'(a,f6.3,a)') 
     &   ' Weighted mean angle from optical axis  = ', waoaa, ' arcmin'
      CALL fcecho(wrtstr)
      CALL fcecho(' ')

* Additional initilization of deteff inserted here (KM, 2001 November)
      deteff = 0.

* Loop round the energies. First initialize the ARF array

      DO i = 1, nenerg

         arf(i) = 0.
* If the energy has been set to < 0 then outside the valid range

         IF ( middl(i) .LT. 0. ) GOTO 100

* If this is an SIS then can calculate the detector efficiency here.
* This efficiency includes the thermal shield and, if required, the fudge.

         IF ( instrum .LE. 1 ) THEN
            deteff = SNGL(TRNS(DBLE(middl(i)), instrum))
            IF ( fudge ) deteff = deteff 
     &           * SNGL(SISFDG(DBLE(lower(i)), DBLE(hiher(i))))
         ENDIF

* Loop round the pixels in the WMAP

         DO k = 1, wmsiz2
            DO j = 1, wmsiz1

               IF ( wmap(j,k) .GE. 0. ) THEN

* IF the GIS then calculate the detector efficiency for this pixel. 

                  IF ( instrum .GE. 2 ) THEN
                     dlo = DBLE(lower(i))
                     dhi = DBLE(hiher(i))
                     dbe = DBLE(bemap(j,k))
                     dgis = get_gis_eff(dlo, dhi, dbe, ifudge)
                     deteff = grdmap(j,k) * dgis
                  ENDIF

* calculate the XRT efficiency at the pixel position - this uses all efficiency 
* within 12mm diameter which is probably not correct. Need to think about this 
* some more.

                  xrteff = xrtefa(lower(i), hiher(i), theta(j,k), 
     &                            phi(j,k))

* and the contribution to the ARF is the product of the detector efficiency,
* the XRT efficiency and the weighted map.

                  arf(i) = arf(i) + deteff * xrteff 
     &                              * (wmap(j,k) - bkgmap(j,k))

               ENDIF

            ENDDO
         ENDDO

* divide by the sum of the WMAP 

         IF ( sumwmap .NE. 0. ) arf(i) = arf(i)/sumwmap

* end loop over energies

 100     CONTINUE

      ENDDO

      RETURN
      END
