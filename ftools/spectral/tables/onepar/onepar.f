c This program writes a one-parameter series of model spectra into
c an XSPEC table model file.

c The spectra and associated information are read using the routine 
c specread which should be modified by the user for their own data.

      IMPLICIT NONE

c MAXBIN should be > the number of bins in the spectra
c MAXREC should be > the number of spectra

      INTEGER MAXBIN, MAXREC
      PARAMETER(MAXBIN=2000, MAXREC=100)

      REAL energy(MAXBIN), spectra(MAXBIN, MAXREC)
      REAL parval(MAXREC), intivl(6), junk

      INTEGER intmth, ierr, ounit, i
      INTEGER nenerg, numrec, status

      CHARACTER(255) outfil
      CHARACTER(12) intnam, modlnm
      CHARACTER(14) cunits
      CHARACTER(72) contxt
      CHARACTER cjunk

      LOGICAL redshift, qaddtv

      DATA cunits /'photons/cm^2/s'/

c Read the spectra

      CALL specread(MAXBIN, MAXREC, nenerg, numrec, energy, spectra,
     &              parval, intivl, intmth, intnam, qaddtv, redshift,
     &              modlnm)

c Get the output filename

      WRITE(*,'('' Output file : '')', advance="no")
      READ(*,'(a)',iostat=ierr) outfil
      IF ( ierr .NE. 0 ) CALL exit(1)

c Now write out the FITS table model file down to the point of actually
c writing the model spectra

      CALL wftbmd (outfil, ' ', modlnm, cunits, 1, 0, redshift,
     &             qaddtv, cjunk, junk, intnam, intivl, numrec, intmth,
     &             MAXREC, parval, nenerg, energy, ounit, ierr, status, 
     &             contxt)

      IF ( ierr .NE. 0 ) GOTO 999

c Loop round writing the spectra
c NB the actual size of the spectra are nenerg-1 because the energies are
c the beginning and end values for each bin.

      DO i = 1, numrec

         CALL ftpcle(ounit, 1, i, 1, 1, parval(i), status)
         contxt = 'Failed to write PARAMVAL column'
         IF ( status .NE. 0 ) GOTO 999

         CALL ftpcle(ounit, 2, i, 1, nenerg-1, spectra(1,i), status)
         contxt = 'Failed to write INTPSPEC column'
         IF ( status .NE. 0 ) GOTO 999

      ENDDO

c close the file

      CALL ftclos(ounit, status)
      contxt = 'Failed to close FITS file'
      IF (status .NE. 0) GOTO 999
      CALL frelun(ounit)

c error wrap-up

 999  CONTINUE
      IF ( ierr .NE. 0 .OR. status .NE. 0 ) THEN
         WRITE(*,*) ierr, status
         WRITE(*,'(a)') contxt
         CALL leave()
      ENDIF

      CALL leave()
      END











