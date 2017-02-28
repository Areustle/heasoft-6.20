c This program converts a table model file into FITS.

      INTEGER MAXBIN, MAXREC, MXINTP
      PARAMETER(MAXBIN=2000, MAXREC=20000, MXINTP=30)

      REAL energy(MAXBIN), phot(MAXBIN)
      REAL parval(MAXREC, MXINTP), par(MXINTP)
      REAL intivl(6, MXINTP)

      REAL junk, checkreal, eend

      INTEGER intntb(MXINTP), intmth(MXINTP)

      INTEGER ierr, iunit, lenrec, ounit
      INTEGER i, j, ipar, nintpm
      INTEGER nenerg, numrec, status
      INTEGER checkint, nbytes, addhead

      INTEGER*2 i2temp(2)

      character(255) infile, outfil
      character(12) intnam(MXINTP)
      CHARACTER cunits*14, modlnm*12, contxt*72, mach*4, cjunk*1

      LOGICAL redshift, qaddtv

      INTEGER gtrcln, lenact
      EXTERNAL gtrcln, lenact

      DATA cunits /'photons/cm^2/s'/

c Get the filenames and the number of energy ranges in the file

      WRITE(*,'('' Input file : '', $)')
      READ(*,'(a)',iostat=ierr) infile
      IF ( ierr .NE. 0 ) CALL exit(1)

      WRITE(*,'('' Output file : '', $)')
      READ(*,'(a)',iostat=ierr) outfil
      IF ( ierr .NE. 0 ) CALL exit(1)

c Ask whether the model is additive

      WRITE(*,'('' Is this an additive model ? (y/n) '', $)')
      READ(*, '(a)', iostat=ierr) cjunk
      IF ( ierr .NE. 0 ) CALL exit(1)
      IF (cjunk .EQ. 'y' .OR. cjunk .EQ. 'Y') THEN
         qaddtv = .true.
      ELSE
         qaddtv = .false.
      ENDIF

c Open the input file

      CALL getlun(iunit)
      lenrec = gtrcln(infile)
      CALL openwr(iunit, infile, 'old', 'd', ' ', lenrec, 1, ierr)
      IF ( ierr .NE. 0 ) THEN
         WRITE(*,*) 'Failed to open input file, ierr = ', ierr
         CALL exit(2)
      ENDIF

c read in the header record. Note that bperno is stored as an I*2 in
c the file but we put it in the structure as an I.

      READ (iunit, REC=1, IOSTAT=ierr) mach, checkint, checkreal,
     &    nbytes, i2temp(1), nenerg, nintpm, modlnm, energy(1),
     &    eend, addhead, redshift

cd      WRITE(*,'(i4,a)') nintpm, ' interpolation parameters'
cd      WRITE(*,'(i4,a)') nenerg, ' energies'
cd      IF ( redshift ) WRITE(*, '(a)') 'redshift flag set'
cd      WRITE(*,*) 
      WRITE(*,*) 'start energy : ', energy(1)
cd
 
      nenerg = nenerg + 1

      modlnm = modlnm(:6)//'        '
      WRITE(*,'(a)') modlnm

c read in the parameter information.

      DO ipar = 1, nintpm

         READ (iunit, REC=ipar+1) intnam(ipar), 
     &       (intivl(i,ipar), i=1, 6), i2temp(1), i2temp(2),
     &       (parval(i,ipar), i=1, i2temp(1))
         intntb(ipar) = i2temp(1)
         intmth(ipar) = i2temp(2)
         junk = intivl(6,ipar)
         DO i = 6, 3, -1
            intivl(i,ipar) = intivl(i-1,ipar)
         ENDDO
         intivl(2,ipar) = junk

cd         WRITE(*, '(a,i4,a)') ' interpolation parameter ', ipar, 
cd     &                        ' ... '
cd         WRITE(*, '(i5,a)') intntb(ipar), ' values : '
cd         WRITE(*,*) (parval(i,ipar), i=1, i2temp(1))

      ENDDO

c Read in the energies

      READ (iunit, rec=nintpm+2, iostat=ierr) 
     &    (energy(i),i=2,nenerg)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) 'Failed to read energies'
         CALL exit(3)
      ENDIF

c Get the total number of model spectra

      numrec = 1
      DO i = 1, nintpm
         numrec = numrec * intntb(i)
      ENDDO

c Now write out the FITS table model file down to the point of actually
c writing the model spectra

      CALL wftbmd (outfil, infile, modlnm, cunits, nintpm, 0, redshift,
     &             qaddtv, cjunk, junk, intnam, intivl, intntb, intmth,
     &             MAXREC, parval, nenerg, energy, ounit, ierr, status, 
     &             contxt)

      IF ( ierr .NE. 0 ) GOTO 999

               
c Loop round reading and writing the spectra

      DO i = 1, numrec

         READ(iunit, rec=nintpm+2+i) (par(j),j=1,nintpm),
     &      (phot(j),j=1,nenerg-1)

         CALL ftpcle(ounit, 1, i, 1, nintpm, par, status)
         contxt = 'Failed to write PARAMVAL column'
         IF ( status .NE. 0 ) GOTO 999

         CALL ftpcle(ounit, 2, i, 1, nenerg-1, phot, status)
         contxt = 'Failed to write INTPSPEC column'
         IF ( status .NE. 0 ) GOTO 999

      ENDDO

c close the files

      CLOSE(iunit)
      CALL frelun(iunit)

      CALL ftclos(ounit, status)
      contxt = 'Failed to close FITS file'
      IF (status .NE. 0) GOTO 999
      CALL frelun(ounit)

c error wrap-up

 999  CONTINUE
      IF ( ierr .NE. 0 ) THEN
         WRITE(*,*) ierr, status
         WRITE(*,'(a)') contxt
         CALL leave()
      ENDIF

      CALL leave()
      END











