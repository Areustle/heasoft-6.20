c This program writes a two-parameter series of model spectra into
c an XSPEC table model file.

c The spectra and associated information are read using the routine 
c specread2 which should be modified by the user for their own data.

      IMPLICIT NONE

c MAXBIN should be > the number of bins in the spectra
c MAXREC should be > the number of spectra

      INTEGER MAXBIN, MAXREC
      PARAMETER(MAXBIN=2000, MAXREC=100)

      REAL energy(MAXBIN), spectra(MAXBIN, MAXREC)
      REAL parval(MAXREC,2), intivl(6,2), junk

      INTEGER intmth(2), ierr, ounit, i, j, i1
      INTEGER nenerg, intntb(2), status

      CHARACTER outfil*255 
      character(12) intnam(2), modlnm
      CHARACTER cunits*14, contxt*72, cjunk*1

      LOGICAL redshift, qaddtv

      DATA cunits /'photons/cm^2/s'/

c Read the spectra

      CALL specread2(MAXBIN, MAXREC, nenerg, intntb, energy, spectra,
     &               parval, intivl, intmth, intnam, qaddtv, redshift,
     &               modlnm)

c Get the output filename

      WRITE(*,'('' Output file : '', $)')
      READ(*,'(a)',iostat=ierr) outfil
      IF ( ierr .NE. 0 ) CALL exit(1)

c Now write out the FITS table model file down to the point of actually
c writing the model spectra

      CALL wftbmd (outfil, ' ', modlnm, cunits, 2, 0, redshift,
     &             qaddtv, cjunk, junk, intnam, intivl, intntb, intmth,
     &             MAXREC, parval, nenerg, energy, ounit, ierr, status, 
     &             contxt)

      IF ( ierr .NE. 0 ) GOTO 999

c Loop round writing the spectra
c NB the actual size of the spectra are nenerg-1 because the energies are
c the beginning and end values for each bin.

      i1 = 0
      DO j = 1, intntb(2)
         DO i = 1, intntb(1)
            i1 = i1 + 1

            CALL ftpcle(ounit, 1, i1, 1, 1, parval(i,1), status)
            CALL ftpcle(ounit, 1, i1, 2, 1, parval(j,2), status)
            contxt = 'Failed to write PARAMVAL column'
            IF ( status .NE. 0 ) GOTO 999

            CALL ftpcle(ounit, 2, i1, 1, nenerg-1, spectra(1,i1), 
     &                  status)
            contxt = 'Failed to write INTPSPEC column'
            IF ( status .NE. 0 ) GOTO 999

         ENDDO
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











