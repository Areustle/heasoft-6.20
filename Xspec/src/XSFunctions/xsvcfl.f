
      SUBROUTINE XSVCFL(ear, ne, tlow, thigh, slope, abun, dens, z, 
     &                  itype, switch, ifl, cflver, nsteps, nmtval, 
     &                  bolo, ttab, tval, dem, photar, photer)

      IMPLICIT NONE

      INTEGER ne, ifl, itype, switch, cflver, nsteps
      REAL ear(0:ne), abun(*), ttab(*), bolo(*), tval(*), dem(*)
      REAL photar(ne), photer(ne)
      REAL tlow, thigh, slope, dens, z

c  XSPEC model subroutine to calculate cooling flow spectrum
c  Arguments :
c       ear    R        i: model energy ranges
c       ne     I        i: number of model energies
c       tlow   R        i: low end of temperature range
c       thigh  R        i: high end of temperature range
c       slope  R        i: slope of emission measure distribution
c       abun   R        i: abundances
c       dens   R        i: density (cm^-3)
c       z      R        i: redshift
c       itype  I        i: type of plasma emission file
c                            1 = R-S
c                            2 = Mekal
c                            3 = Meka
c       switch I        i: 0==calculate, 1==interpolate
c       ifl    I        i: dataset number (unused at present) 
C       cflver I        i: cflow version number
C       nsteps I        i: number of temperature steps
C       nmtval I        i: 
C       bolo   R        i: Bolometric luminosity for each temperature
C       ttab   R        i: 
C       tval   R        r: Tabulated temperatures
C       dem    R        r: Emission measure for each temperature
c       photar R        r: spectrum

c  Norm is mass accretion rate in units of Msun/yr

c  kaa  3/24/88
c  kaa 12/21/92    attempt to speed up and fix intermittent errors.
c                  NB the workphot array is used to store the R-S data
c                  as read in and not after rebinning to the ear array
c                  as in other routines.
c                  Following a couple of bug fixes from daw this now gives
c                  results for slope=0 consistent with rmj's model.
c  kaa   9/3/94    modified for FITS format files
C  kaa   9/6/96    use of dynamic memory - shares loadrs routine with R-S model
C  kaa  10/3/96    replaced loadrs by more general ldpfil call
C  kaa  10/4/96    generalized to handle both R-S and Mekal based CF models
C                  - assumes that we are not going to be running both at the
C                    same time.
C  kaa  10/11/96   uses SUMDEM routine to do actual summation and rebinning
C                  now does calculate option as well but calculation is done
C                  on temperatures in tabulated grid and bolometric luminosity
C                  is calculated using tabulated data

C  cg   2/9/09     udmget elimination: moved the memory allocation code into new 
C                  xscflw.cxx and xsvmcf.cxx files.  The corresponding array 
C                  pointers are now passed into here.


      REAL norm, tstep, factor, tmin, tmax, altlow, althigh
      REAL deltat, tbot, h0, q0, Lambda0, boloi, tkeV

      INTEGER i, j, status, nmtval, itmin, itmax

      character(72) contxt
      character(255) wrtstr

      INTEGER lenact
      REAL csmgh0, csmgq0, csmgl0, fzsq
      character(255) fgmstr
      EXTERNAL csmgh0, csmgq0, csmgl0, fzsq, lenact, fgmstr

      status = 0

      IF ( z .LE. 0. ) THEN
         CALL xwrite('z must be non-zero for cooling flow models', 10)
         RETURN
      ENDIF

      IF ( switch .EQ. 1 .AND. itype .EQ. 3 ) THEN
         CALL xwrite('XSVCFL: This error should not occur', 2)
         RETURN
      ENDIF


C Set up the temperature array

      altlow = log(tlow)
      althigh = log(thigh)

      IF ( cflver .EQ. 1 ) THEN
         DO i = 1, nsteps
            tval(i) = ttab(i)
         ENDDO
      ELSEIF ( cflver .EQ. 2 ) THEN
         DO i = 1, nsteps
            tval(i) = exp(altlow + 
     &                         (i-1)*(althigh-altlow)/(nsteps-1))
         ENDDO
      ENDIF

c  Get the cosmology parameters

      q0 = csmgq0()
      h0 = csmgh0()
      Lambda0 = csmgl0()

c  Fix up the norm. The numerical constant assumes distance linearly depends
c  on redshift with H0=50 hence cosmology factors correct this.

      norm = 3.16e-15 * (h0/50.)**2 / fzsq(z, q0, Lambda0)

c  norm is C/4piD^2 in Mushotzky & Szymkowiak - uses :
c	L = (5/2) (Mdot/mu/m(H)) k dT
c  and :
c   3.16e-15 = (5/2) (1.989e33/3.15e7) (1/0.6) (1/1.66e-24)
c              (1/4pi) (6000 3.09e24)^-2 (1.38e-16/1.60e-9)

C  Now calculate the DEMs. The integral is an extended trapezium rule 
c  over the temperatures. The integration is performed in log T space.

      DO i = 1, nsteps
         dem(i) = 0.
      ENDDO

c  Now loop over the temperatures. In the old version we perform the
c  function evaluations at tabulated temperatures. The integral
c  is an extended trapezium rule over the tabulated temperatures
c  within the range then simple trapezium rules to handle the bits
c  left over at the end. The integration is performed in log T space.

c  Find the index for the tabulated temperatures immediately above the 
c  minimum and below the maximum. tmin and tmax are the log T corresponding
c  to these. tbot is the log of the lowest tabulated T.

c  NB this assumes that the temperatures are logarithmically evenly 
C  distributed

      IF ( cflver .EQ. 1 ) THEN

         deltat = (log(tval(nmtval))-log(tval(1)))
     &            /(nmtval-1)
         tbot = log(tval(1))
         itmin = MAX(INT((altlow-tbot)/deltat + 2),2)
         itmax = MIN(INT((althigh-tbot)/deltat + 1),nmtval-1)
         tmin = tbot + deltat*(itmin-1)
         tmax = tbot + deltat*(itmax-1)

         DO i = (itmin-1), (itmax+1)

c  Calculate the weighting factor in the integral (this is
c  a bit messy !).

            factor = deltat
            IF (i .EQ. itmin-1) THEN
               factor = factor*(1.+(tmin-altlow)/deltat)/2
            ELSEIF (i .EQ. itmin) THEN
               factor = factor*(1.+(altlow-tmin+deltat)/deltat)/2
            ELSEIF (i .EQ. itmax) THEN
               factor = factor*(1.+(tmax+deltat-althigh)/deltat)/2
            ELSEIF (i .EQ. itmax+1) THEN
               factor = factor*(1.+(althigh-tmax)/deltat)/2
            ENDIF

c  calculate the emission-weighting for this temperature. This
c  includes the division by the bolometric emissivity and an
c  extra factor of T since we are integrating in log space.

            tstep = 1.16e7*tval(i)
            dem(i) = (tstep/1.e7)**slope * tstep * factor
     &                      * norm / bolo(i)

         ENDDO

c  In the new version just loop over all the temperatures

      ELSEIF ( cflver .EQ. 2 ) THEN

         DO i = 1, nsteps
            tkeV = tval(i)

            factor = (althigh-altlow)/(nsteps-1)
            IF ( i .EQ. 1 .OR. i .EQ. nsteps ) factor = factor/2.

c  Interpolate on the tabulated array of bolometric luminosities to
c  get the value for this temperature

            IF ( tkeV .LE. ttab(1) ) THEN
               boloi = bolo(1)
            ELSEIF ( tkeV .GE. ttab(nmtval) ) THEN
               boloi = bolo(nmtval)
            ELSE
               j = 1
               DO WHILE ( tkeV .GT. ttab(j+1) )
                  j = j + 1
               ENDDO
               boloi = ( bolo(j)*(ttab(j+1)-tkeV) +
     &                  bolo(j+1)*(tkeV-ttab(j)) ) /
     &                (ttab(j+1)-ttab(j))
            ENDIF

c  calculate the emission-weighting for this temperature. This
c  includes the division by the bolometric emissivity and an
c  extra factor of T since we are integrating in log space.

            tstep = 1.16e7*tval(i)
            dem(i) = (tstep/1.e7)**slope * tstep * factor
     &                      * norm / boloi

         ENDDO

      ENDIF

C  Now call the routine that does the actual calculation of the
C  spectrum

      CALL sumdem(itype, switch, ear, ne, abun, dens, z, nsteps, 
     &            tval, dem, ifl, .FALSE., 0., 
     &            photar, photer, status)
      IF ( status .NE. 0 ) GOTO 100


 100  CONTINUE
      IF ( status .NE. 0 ) THEN
         WRITE(wrtstr,'(''Error '', i3, '' in XSVCFL'')') status
         CALL xwrite(wrtstr, 10)
         CALL xwrite(contxt, 10)
      ENDIF

      END
