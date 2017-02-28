
      SUBROUTINE specread(MAXBIN, MAXREC, nenerg, numrec, energy, 
     &                    spectra, parval, parivl, parmth, parnam, 
     &                    qaddtv, redshift, modlnm)

      IMPLICIT NONE

      INTEGER MAXBIN, MAXREC

      REAL energy(MAXBIN), spectra(MAXBIN, MAXREC)
      REAL parval(MAXREC), parivl(6)

      INTEGER nenerg, numrec, parmth

      LOGICAL qaddtv, redshift

      CHARACTER parnam*12, modlnm*12

c Subroutine to read in the spectra and associated data to be written into
c a FITS table file
c Arguments :
c     MAXBIN     i        i: Max number of energy bins allowed
c     MAXREC     i        i: Max number of spectra allowed
c     nenerg     i        r: Actual number of energies 
c                            (= number of spectral bins + 1)
c     numrec     i        r: Actual number of spectra
c     energy     r        r: Energies 
c     spectra    r        r: A 2-D array of spectra
c     parval     r        r: The parameter value associated with each spectrum
c     parivl     r        r: The parameter info required by XSPEC
c     parmth     r        r: The interpolation method to be used by XSPEC
c     parnam     c*12     r: The parameter name
c     qaddtv     l        r: True if this is an additive model
c     redshift   l        r: True if redshift is to be included as a parameter
c                            by XSPEC
c     modlnm     c*12     r: Model name

      INTEGER i, j


c This is an additive model (change to .FALSE. if multiplicative)

      qaddtv = .TRUE.

c Include redshift as a parameter (change to .FALSE. if you don't want this)

      redshift = .TRUE.

c Put the name of the model and parameter here

      modlnm = 'mymodel'
      parnam = 'mypar'

c This is the interpolation method that XSPEC should use (0=linear, 1=log)

      parmth = 0

c This is the parameter info used by XSPEC : the default value, delta, min,
c bottom, top, max. Set min=bottom=first parameter value and top=max=last
c parameter value 

      parivl(1) = 0.5
      parivl(2) = 0.01
      parivl(3) = 0.
      parivl(4) = 0.
      parivl(5) = 1.
      parivl(6) = 1.

c These are the number of energies and the number of spectra

      nenerg = 100
      numrec = 2

c These are the parameter values for the tabulated spectra

      parval(1) = 0.0
      parval(2) = 1.0

c This are the energies on which spectra are calculated. There are nenerg
c values for the nenerg-1 bins in the spectra.

      DO i = 1, nenerg
         energy(i) = i*0.01
      ENDDO

c and these are the spectra

      DO j = 1, numrec
         DO i = 1, nenerg-1
            spectra(i,j) = i*0.01 + j
         ENDDO
      ENDDO

      RETURN
      END

