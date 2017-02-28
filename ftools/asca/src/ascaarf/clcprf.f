
      SUBROUTINE clcprf(wmsiz1, wmsiz2, nenerg, middl, lower, hiher,
     &                  source, instrum, theta, phi, psf, binsze, 
     &                  abnsze, wmap, bkgmap, grdmap, bemap,
     &                  waoaa, arf, fudge)

      INTEGER wmsiz1, wmsiz2, nenerg, instrum
      REAL waoaa, arf(nenerg)
      REAL middl(nenerg), lower(nenerg), hiher(nenerg)
      REAL source(2), binsze, abnsze
      REAL theta(wmsiz1,wmsiz2), phi(wmsiz1,wmsiz2)
      DOUBLE PRECISION psf(wmsiz1,wmsiz2)
      REAL wmap(wmsiz1,wmsiz2), grdmap(wmsiz1,wmsiz2)
      REAL bemap(wmsiz1,wmsiz2), bkgmap(wmsiz1,wmsiz2)
      LOGICAL fudge

c This routine actually calculates the ARF for a point source.
*    point         is a point source
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
*    source        Source position in (theta,phi) for a point source

*    fudge         If true the gaussian effective area fudge will be applied

      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979323846264338327950d0)

      INTEGER i, j, k, m, n, ifudge, nrep, prmpt
      REAL sumwmap, deteff, xrteff
      DOUBLE PRECISION dlo, dhi, dbe, dgis, doff, xoff, yoff, sum
      DOUBLE PRECISION am2mm, dphi, xmms, ymms, xmmb, ymmb, delmm

      character(255) wrtstr

* External functions
*    xrtefa      telescope effective area
*    xrtpsf95b   calculate the XRT PSF
*    trns        thermal precollimator transmission
*    get_gis_eff GIS efficiency including thermal shield
*    sisfdg      the SIS efficiency fudge

      REAL xrtefa
      DOUBLE PRECISION get_gis_eff, sisfdg, trns, xrtpsf95b

      EXTERNAL xrtefa, trns, get_gis_eff, xrtpsf95b

      DOUBLE PRECISION odlo, odhi, odbe 

* Set the fudge correction flag

      IF ( fudge ) THEN
         ifudge = 0
      ELSE
         ifudge = 1
      ENDIF

* Calculate the WMAP sum and the weighted mean off-axis angle

      waoaa = 0
      sumwmap = 0.
      DO k = 1, wmsiz2
         DO j = 1, wmsiz1
            IF ( wmap(j,k) .GE. 0 ) THEN
               sumwmap = sumwmap + wmap(j,k) - bkgmap(j,k)
               waoaa = waoaa + theta(j,k) * (wmap(j,k) - bkgmap(j,k))
            ENDIF
         ENDDO
      ENDDO
      waoaa = waoaa/sumwmap

      CALL fcecho(' ')
      WRITE(wrtstr,'(a,1pe11.5)') ' Total counts in region = ', sumwmap
      CALL fcecho(wrtstr)
      WRITE(wrtstr,'(a,f6.3,a)') 
     &   ' Weighted mean angle from optical axis  = ', waoaa, ' arcmin'
      CALL fcecho(wrtstr)
      CALL fcecho(' ')

* When we calculate the PSF we will want to integrate on at most 0.1mm bins so
* calculate the number of subdivisions within each WMAP bin

      nrep = INT(binsze/0.1 + 0.99)
      delmm = binsze/nrep

* Calculate the source position in Cartesian coordinates in units of mm.

      am2mm = DBLE( binsze / abnsze )
      dphi = DBLE(source(2)) * PI / 180.d0
      xmms = DBLE(source(1)) * COS(dphi) * am2mm
      ymms = DBLE(source(1)) * SIN(dphi) * am2mm

c Calculate the position of the center of the WMAP and issue a warning
c if the source position is too far from the WMAP center.

      j = wmsiz1/2
      k = wmsiz2/2
      dphi = DBLE(phi(j,k)) * PI / 180.d0
      xmmb = DBLE(theta(j,k)) * COS(dphi) * am2mm
      ymmb = DBLE(theta(j,k)) * SIN(dphi) * am2mm

      IF ( (xmms-xmmb)**2 + (ymms-ymmb)**2 .GT. (10./am2mm)**2 ) THEN
         CALL fcecho(' ')
         CALL fcecho(
     & 'Warning : source position is > 10 arcmin from WMAP center')
         CALL fcecho(
     & 'You may have entered the source position incorrectly')
         CALL fcecho(' ')
      ENDIF

* Loop round the energies. First initialize the ARF array

      odlo=0.0
      odhi=0.0 
      odbe=0.0
      
      prmpt = nenerg / 10
      
      DO i = 1, nenerg

         If ((i / prmpt) * prmpt .EQ. i) THEN
	   write(wrtstr,'(a,i4,a,i4,a)')
     &     '  calculated ', i, ' /', nenerg, ' energies...'
           CALL fcecho(wrtstr)
         ENDIF
	 
         arf(i) = 0.

* If the energy has been set to < 0 then outside the valid range

         IF ( middl(i) .LT. 0. ) GOTO 100

* calculate the XRT efficiency at the source position (this is normalized 
* at 12mm diameter on the focal plane)

         xrteff = xrtefa(lower(i), hiher(i), source(1), source(2))

* calculate the PSF map here. Loop round the WMAP bins

         sum = 0.d0
         DO k = 1, wmsiz2
            DO j = 1, wmsiz1

* for this WMAP bin calculate the bin position in Cartesian coordinates in
* mm.

               dphi = DBLE(phi(j,k)) * PI / 180.d0
               xmmb = DBLE(theta(j,k)) * COS(dphi) * am2mm
               ymmb = DBLE(theta(j,k)) * SIN(dphi) * am2mm

* integrate the PSF over bins of at most 0.1mm square. Note that xrtpsf95b
* returns the PSF in units of 1/mm^2 for the given source position
* and offset

               doff = delmm*(nrep+1)/2.d0

               psf(j,k) = 0.d0
               DO n = 1, nrep
                  yoff = ymmb-doff+delmm*n - ymms

                  DO m = 1, nrep
                     xoff = xmmb-doff+delmm*m - xmms

                     psf(j,k) = psf(j,k) + xrtpsf95b(DBLE(middl(i)),
     &                          DBLE(source(1)), DBLE(source(2)),
     &                          xoff, yoff)

                  ENDDO
               ENDDO

               psf(j,k) = psf(j,k) * delmm * delmm
               sum = sum + psf(j,k)

            ENDDO
         ENDDO

* If the instrument is the GIS then convolve the XRT PSF with the
* GIS PSF

         IF ( instrum .GE. 2 ) THEN

            dlo = DBLE(lower(i))
            dhi = DBLE(hiher(i))

            DO k = 1, wmsiz2
               DO j = 1, wmsiz1
                  
* Calculate the detector efficiency for this pixel
* Note that get_gis_eff() includes thermal shield efficiency
 
                  dbe = DBLE(bemap(j,k))
		  
         if ( odlo.ne.dlo.or.odhi.ne.dhi.or.odbe.ne.dbe) then
                  dgis = get_gis_eff(dlo, dhi, dbe, ifudge)

                  odlo =dlo
                  odhi =dhi
                  odbe =dbe
         endif

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

* multiply the resultant by the XRT efficiency at the source position

         arf(i) = arf(i) * xrteff

* end loop over energies

 100     CONTINUE

      ENDDO

      RETURN
      END


