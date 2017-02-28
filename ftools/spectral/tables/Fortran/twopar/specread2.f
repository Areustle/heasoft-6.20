
      SUBROUTINE specread2(MAXBIN, MAXREC, nenerg, parntb, energy, 
     &                     spectra, parval, parivl, parmth, parnam, 
     &                     qaddtv, redshift, modlnm)

      IMPLICIT NONE

      INTEGER MAXBIN, MAXREC

      REAL energy(MAXBIN), spectra(MAXBIN, MAXREC)
      REAL parval(MAXREC,2), parivl(6,2)

      INTEGER nenerg, parntb(2), parmth(2)

      LOGICAL qaddtv, redshift

      character(12) parnam(2), modlnm

c Subroutine to read in the spectra and associated data to be written into
c a FITS table file
c Arguments :
c     MAXBIN     i        i: Max number of energy bins allowed
c     MAXREC     i        i: Max number of values for each parameter
c     nenerg     i        r: Actual number of energies 
c                            (= number of spectral bins + 1)
c     parntb     i        r: Actual number of values for each parameter
c     energy     r        r: Energies 
c     spectra    r        r: A 2-D array of spectra
c     parval     r        r: The parameter values associated with each spectrum
c     parivl     r        r: The parameter info required by XSPEC
c     parmth     r        r: The interpolation method to be used by XSPEC
c     parnam     c*12     r: The parameter name
c     qaddtv     l        r: True if this is an additive model
c     redshift   l        r: True if redshift is to be included as a parameter
c                            by XSPEC
c     modlnm     c*12     r: Model name

      INTEGER i, j, k, inum


c This is an additive model (change to .FALSE. if multiplicative)

      qaddtv = .TRUE.

c Include redshift as a parameter (change to .FALSE. if you don't want this)

      redshift = .TRUE.

c Put the name of the model and parameters here

      modlnm = 'mymodel'
      parnam(1) = 'mypar1'
      parnam(2) = 'mypar2'

c This is the interpolation method that XSPEC should use (0=linear, 1=log)

      parmth(1) = 0
      parmth(2) = 0

c This is the parameter info used by XSPEC : the default value, delta, min,
c bottom, top, max. Set min=bottom=first parameter value and top=max=last
c parameter value 

      parivl(1,1) = 0.5
      parivl(2,1) = 0.01
      parivl(3,1) = 0.
      parivl(4,1) = 0.
      parivl(5,1) = 1.
      parivl(6,1) = 1.

      parivl(1,2) = 1.0
      parivl(2,2) = 0.01
      parivl(3,2) = 0.
      parivl(4,2) = 0.
      parivl(5,2) = 5.
      parivl(6,2) = 5.

c These are the number of energies and the number of spectra

      nenerg = 100
      parntb(1) = 2
      parntb(2) = 3

c These are the parameter values for the tabulated spectra

      parval(1,1) = 0.0
      parval(2,1) = 1.0
      parval(1,2) = 0.0
      parval(2,2) = 1.0
      parval(3,2) = 2.0

c These are the energies on which spectra are calculated. There are nenerg
c values for the nenerg-1 bins in the spectra.

      DO i = 1, nenerg
         energy(i) = i*0.01
      ENDDO

c and these are the spectra

      inum = 0
      DO k = 1, parntb(1)
         DO j = 1, parntb(2)
            inum = inum + 1
            DO i = 1, nenerg-1
               spectra(i,inum) = i*0.01 + j + 2*k
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END

