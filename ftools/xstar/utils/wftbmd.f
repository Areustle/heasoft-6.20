      SUBROUTINE wftbmd(outfil, infile, modlnm, modunt, nintpm, naddpm,
     &                  qrdshf, qaddtv, addnam, addivl, intnam, intivl, 
     &                  intntb, intmth, maxtab, inttab, nenerg, energy, 
     &                  model, maxbin, numrec, work, ierr, status, 
     &                  contxt)

      INTEGER nintpm, naddpm, maxtab, nenerg, maxbin, numrec
      INTEGER ierr, status
      INTEGER intntb(nintpm), intmth(nintpm)
      REAL addivl(6, naddpm), intivl(6, nintpm)
      REAL inttab(maxtab, nintpm), energy(nenerg)
      REAL model(maxbin, numrec), work(nintpm)
      CHARACTER*(*) outfil, infile, modlnm, modunt, contxt
      CHARACTER*(*) addnam(naddpm), intnam(nintpm)
      LOGICAL qrdshf, qaddtv

c Arguments :
c    outfil      c*(*)     i: FITS filename
c    infile      c*(*)     i: Name of original file
c    modlnm      c*(*)     i: The name of the model
c    modunt      c*(*)     i: The model units
c    nintpm      i         i: The number of interpolation parameters
c    naddpm      i         i: The number of additional parameters
c    qrdshf      l         i: Redshift flag
c    qaddtv      l         i: If true this is an additive table model
c    addnam      c*(*)     i: Names of the additional parameters
c    addivl      r         i: Initial values of the additional parameters
c    intnam      c*(*)     i: Names of the interpolation parameters
c    intivl      r         i: Initial values of the interpolation parameters
c    intntb      i         i: Number of tabulated values for interpolation params.
c    intmth      i         i: Interpolation method for interpolation parameters
c    maxtab      i         i: The size of the first dimension in inttab
c    inttab      r         i: The tabulated parameter values.
c    nenerg      i         i: Number of energies (one more than spectral bins)
c    energy      r         i: Energies
c    model       r         i: The model spectra
c    maxbin      i         i: The size of the first dimension in model
c    numrec      i         i: The number of model spectra
c    work        r         w: Workspace array of size nintpm
c    ierr        i         o: error
c                             1 = failed to open FITS file
c                             2 = failed to write primary header
c                             3 = failed to write parameter extension
c                             4 = failed to write energy extension
c                             5 = failed to write model spectra
c                             6 = failed to close FITS file
c    status      i         o: FITSIO status
c    contxt      c*(*)     o: Error diagnostic string

c  HDUVERS1 1.0.0

c Local variables

      INTEGER ounit, i, j, k, tfields, lenact, nrows
      INTEGER nvals, irec, idiv, ipt
      character(20) ttype(100), tunit(100)
      character(20) tform(100)

c ------------------------------------------------------------
c Open the output FITS file.
c ------------------------------------------------------------
c      lpri=3
      if (lpri.ge.3) write (6,*)'enetring wftbmd:'
      CALL getlun(ounit)
      if (lpri.ge.3) write (6,*)'after getlun'
      CALL ftinit(ounit, outfil, 1, status) 
      if (lpri.ge.3) write (6,*)'after ftinit'
      IF (status .NE. 0) THEN
         ierr = 1
         contxt = 'Failed to open output file'
         RETURN
      ENDIF

c ------------------------------------------------------------
c Write the primary header
c ------------------------------------------------------------

      CALL ftpdef(ounit, 8, 0, 0, 0, 1, status)
      if (lpri.ge.3) write (6,*)'after ftpdef'
      contxt = 'Failed to define primary header'
      IF ( status .NE. 0 ) GOTO 200

c Write the basic primary array keywords

      CALL ftphpr(ounit, .TRUE., 8, 0, 0, 0, 1, .TRUE., status)
      if (lpri.ge.3) write (6,*)'after ftphpr'
      contxt = 'Failed to write basic primary keywords'
      IF ( status .NE. 0 ) GOTO 200

c Write out the additional keywords about the creation of the
c FITS file.

      CALL ftpkys(ounit, 'CONTENT', 'MODEL',
     & 'spectrum file contains time intervals and event', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write CONTENT keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'FILENAME', infile,
     & 'File that FITS was produced from', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write FILENAME keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'ORIGIN', 'NASA/GSFC',
     & 'origin of FITS file', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write ORIGIN keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

c Write the model name and units

      CALL ftpkys(ounit,'MODLNAME', modlnm,
     &            'Model name',status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write MODLNAME keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit,'MODLUNIT', modunt,
     &            'Model units',status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write MODLUNIT keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

c Write the redshift flag

      CALL ftpkyl(ounit,'REDSHIFT', qrdshf,
     &    'If true then redshift will be included as a parameter',
     &            status)
      if (lpri.ge.3) write (6,*)'after ftpkyl'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write REDSHIFT keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

c Write the additive table model flag

      CALL ftpkyl(ounit,'ADDMODEL', qaddtv,
     &    'If true then this is an additive table model',
     &            status)
      if (lpri.ge.3) write (6,*)'after ftpkyl'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write ADDMODEL keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

c Write the OGIP keywords

      CALL ftpkys(ounit, 'HDUCLASS', 'OGIP',
     &            'format conforms to OGIP standard', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLASS keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUCLAS1', 'XSPEC TABLE MODEL',
     &            'model spectra for XSPEC', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLAS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUVERS1', '1.0.0',
     &            'version of format', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUVERS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

 200  CONTINUE
      IF (status .NE. 0) THEN
         ierr = 2
         RETURN
      ENDIF

c ----------------------------------------------------------------------------
c Write the data extension containing the parameter definitions
c ----------------------------------------------------------------------------

c Create the extension

      CALL ftcrhd(ounit, status)
      if (lpri.ge.3) write (6,*)'after ftcrhd'
      contxt = 'Failed to create extension'
      IF ( status .NE. 0 ) GOTO 300

c Set the header keywords for a binary extension

      nvals = 0
      DO i = 1, nintpm
         nvals = MAX(nvals, intntb(i))
         if (lpri.ge.3) write (6,*)i,intntb(i),nvals
      ENDDO
      if (lpri.ge.3) write (6,*)'after nvals=',nvals

      tfields = 10
      ttype(1) = 'NAME'
      tform(1) = '12A'
      tunit(1) = ' '
      ttype(2) = 'METHOD'
      tform(2) = 'J'
      tunit(2) = ' '
      ttype(3) = 'INITIAL'
      tform(3) = 'E'
      tunit(3) = ' '
      ttype(4) = 'DELTA'
      tform(4) = 'E'
      tunit(4) = ' '
      ttype(5) = 'MINIMUM'
      tform(5) = 'E'
      tunit(5) = ' '
      ttype(6) = 'BOTTOM'
      tform(6) = 'E'
      tunit(6) = ' '
      ttype(7) = 'TOP'
      tform(7) = 'E'
      tunit(7) = ' '
      ttype(8) = 'MAXIMUM'
      tform(8) = 'E'
      tunit(8) = ' '
      ttype(9) = 'NUMBVALS'
      tform(9) = 'J'
      tunit(9) = ' '
      ttype(10) = 'VALUE'
      WRITE(tform(10),'(i5,a1)') nvals, 'E'
      tunit(10) = ' '
         
c Write the main header keywords.

      CALL ftphbn(ounit, (nintpm+naddpm), tfields, ttype, tform, tunit,
     &            'PARAMETERS', 0, status)
      if (lpri.ge.3) write (6,*)'after ftphbn'
      contxt = 'Failed to write main header keywords for extension'
      IF ( status .NE. 0 ) GOTO 300


c Write the OGIP keywords

      CALL ftpkys(ounit, 'HDUCLASS', 'OGIP',
     &            'format conforms to OGIP standard', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLASS keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUCLAS1', 'XSPEC TABLE MODEL',
     &            'model spectra for XSPEC', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLAS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUCLAS2', 'PARAMETERS',
     &            'extension containing parameter info', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLAS2 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUVERS1', '1.0.0',
     &            'version of format', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUVERS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

c Write the number of interpolated and additional parameters

      CALL ftpkyj(ounit,'NINTPARM', nintpm, 
     &            'Number of interpolation parameters', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      contxt = 'Failed to write NINTPARM keyword'
      IF ( status .NE. 0 ) GOTO 300

      CALL ftpkyj(ounit,'NADDPARM', naddpm, 
     &            'Number of additional parameters', status)
      if (lpri.ge.3) write (6,*)'after ftpkyj'
      contxt = 'Failed to write NADDPARM keyword'
      IF ( status .NE. 0 ) GOTO 300

c define the binary table

      CALL ftbdef(ounit, tfields, tform, 0, (nintpm+naddpm), status)
      if (lpri.ge.3) write (6,*)'after ftpdef'
      contxt = 'Failed to define binary table'
      IF ( status .NE. 0 ) GOTO 300

c loop round the interpolation parameters

      DO i = 1, nintpm

c write the parameter name

         CALL ftpcls(ounit, 1, i, 1, 1, intnam(i), status)
         if (lpri.ge.3) write (6,*)'after ftpcls'
         contxt = 'Failed to write NAME column'
         IF ( status .NE. 0 ) GOTO 300

c write the parameter method

         CALL ftpclj(ounit, 2, i, 1, 1, intmth(i), status)
         if (lpri.ge.3) write (6,*)'after ftpclj'
         contxt = 'Failed to write METHOD column'
         IF ( status .NE. 0 ) GOTO 300

c write the initial value

         CALL ftpcle(ounit, 3, i, 1, 1, intivl(1,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle 1'
         contxt = 'Failed to write INITIAL column'
         IF ( status .NE. 0 ) GOTO 300

c write the delta value

         CALL ftpcle(ounit, 4, i, 1, 1, intivl(2,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle 2'
         contxt = 'Failed to write DELTA column'
         IF ( status .NE. 0 ) GOTO 300

c write the minimum value

         CALL ftpcle(ounit, 5, i, 1, 1, intivl(3,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle min'
         contxt = 'Failed to write MINIMUM column'
         IF ( status .NE. 0 ) GOTO 300

c write the bottom value

         CALL ftpcle(ounit, 6, i, 1, 1, intivl(4,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle bot'
         contxt = 'Failed to write BOTTOM column'
         IF ( status .NE. 0 ) GOTO 300

c write the top value

         CALL ftpcle(ounit, 7, i, 1, 1, intivl(5,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle top'
         contxt = 'Failed to write TOP column'
         IF ( status .NE. 0 ) GOTO 300

c write the maximum value

         CALL ftpcle(ounit, 8, i, 1, 1, intivl(6,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle max'
         contxt = 'Failed to write MAXIMUM column'
         IF ( status .NE. 0 ) GOTO 300

c write the number of tabulated parameter values

         CALL ftpclj(ounit, 9, i, 1, 1, intntb(i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle numb',intntb(i)
         contxt = 'Failed to write NUMBVALS column'
         IF ( status .NE. 0 ) GOTO 300

c write the tabulated parameter values

         CALL ftpcle(ounit, 10, i, 1, intntb(i), inttab(1,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle val',intntb(i),
     $        inttab(1,i)
         contxt = 'Failed to write VALUE column'
         IF ( status .NE. 0 ) GOTO 300

      ENDDO

c loop round the additional parameters

      DO i = 1, naddpm

c write the parameter name

         CALL ftpcls(ounit, 1, i+nintpm, 1, 1, addnam(i), status)
         if (lpri.ge.3) write (6,*)'after ftpcls'
         contxt = 'Failed to write NAME column'
         IF ( status .NE. 0 ) GOTO 300

c write the initial value

         CALL ftpcle(ounit, 3, i+nintpm, 1, 1, addivl(1,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle init'
         contxt = 'Failed to write INITIAL column'
         IF ( status .NE. 0 ) GOTO 300

c write the delta value

         CALL ftpcle(ounit, 4, i+nintpm, 1, 1, addivl(2,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle delt'
         contxt = 'Failed to write DELTA column'
         IF ( status .NE. 0 ) GOTO 300

c write the minimum value

         CALL ftpcle(ounit, 5, i+nintpm, 1, 1, addivl(3,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle min'
         contxt = 'Failed to write MINIMUM column'
         IF ( status .NE. 0 ) GOTO 300

c write the bottom value

         CALL ftpcle(ounit, 6, i+nintpm, 1, 1, addivl(4,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle bot'
         contxt = 'Failed to write BOTTOM column'
         IF ( status .NE. 0 ) GOTO 300

c write the top value

         CALL ftpcle(ounit, 7, i+nintpm, 1, 1, addivl(5,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle top'
         contxt = 'Failed to write TOP column'
         IF ( status .NE. 0 ) GOTO 300

c write the maximum value

         CALL ftpcle(ounit, 8, i+nintpm, 1, 1, addivl(6,i), status)
         if (lpri.ge.3) write (6,*)'after ftpcle max'
         contxt = 'Failed to write MAXIMUM column'
         IF ( status .NE. 0 ) GOTO 300

      ENDDO

 300  CONTINUE
      IF (status .NE. 0) THEN
         ierr = 3
         RETURN
      ENDIF

c ---------------------------------------------------------------
c Write the data extension containing the energies
c ---------------------------------------------------------------

c Create the extension

      CALL ftcrhd(ounit, status)
      if (lpri.ge.3) write (6,*)'after ftcrhd'
      contxt = 'Failed to create extension'
      IF ( status .NE. 0 ) GOTO 400

c Set the header keywords for a binary extension

      tfields = 2
      ttype(1) = 'ENERG_LO'
      tform(1) = 'E'
      tunit(1) = ' '
      ttype(2) = 'ENERG_HI'
      tform(2) = 'E'
      tunit(2) = ' '

c Write the main header keywords.

      CALL ftphbn(ounit, nenerg-1, tfields, ttype, tform, tunit,
     &            'ENERGIES', 0, status)
      if (lpri.ge.3) write (6,*)'after ftphbn'
      contxt = 'Failed to write main header keywords for extension'
      IF ( status .NE. 0 ) GOTO 400


      CALL ftpkys(ounit, 'HDUCLAS1', 'XSPEC TABLE MODEL',
     &            'model spectra for XSPEC', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLAS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUCLAS2', 'ENERGIES',
     &            'extension containing energy bin info', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLAS2 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUVERS1', '1.0.0',
     &            'version of format', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUVERS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

c define the binary table

      CALL ftbdef(ounit, tfields, tform, 0, nenerg, status)
      if (lpri.ge.3) write (6,*)'after ftbdef'
      contxt = 'Failed to define binary table'
      IF ( status .NE. 0 ) GOTO 400

c write the energies into the table

      CALL ftpcle(ounit, 1, 1, 1, nenerg-1, energy, status)
      if (lpri.ge.3) write (6,*)'after ftpcle energlo'
      contxt = 'Failed to write ENERG_LO column'
      IF ( status .NE. 0 ) GOTO 400

      CALL ftpcle(ounit, 2, 1, 1, nenerg-1, energy(2), status)
      if (lpri.ge.3) write (6,*)'after ftpcle energhi'
      contxt = 'Failed to write ENERG_HI column'
      IF ( status .NE. 0 ) GOTO 400

 400  CONTINUE
      IF (status .NE. 0) THEN
         ierr = 4
         RETURN
      ENDIF

c ---------------------------------------------------------------
c Now write out all the model spectra
c ---------------------------------------------------------------

c Create the extension

      CALL ftcrhd(ounit, status)
      if (lpri.ge.3) write (6,*)'after ftcrhd'
      contxt = 'Failed to create extension'
      IF ( status .NE. 0 ) GOTO 500

c Set the header keywords for a binary extension

      tfields = 2 + naddpm
      ttype(1) = 'PARAMVAL'
      WRITE(tform(1),'(i5,a)') nintpm, 'E'
      tunit(1) = ' '
      ttype(2) = 'INTPSPEC'
      WRITE(tform(2),'(i5,a)') nenerg-1, 'E'
      tunit(2) = modunt
      DO i = 1, naddpm
         ttype(i+2) = 'ADDSP'
         WRITE(ttype(i+2)(6:8),'(i3.3)') i
         WRITE(tform(i+2),'(i5,a)') nenerg-1, 'E'
         tunit(i+2) = modunt
      ENDDO

      nrows = 1
      DO i = 1, nintpm
         nrows = nrows * intntb(i)
         if (lpri.ge.3) write (6,*)i,intntb(i),nrows
      ENDDO

c Write the main header keywords.

      CALL ftphbn(ounit, nrows, tfields, ttype, tform, tunit,
     &            'SPECTRA', 0, status)
      if (lpri.ge.3) write (6,*)'after ftphbn'
      contxt = 'Failed to write main header keywords for extension'
      IF ( status .NE. 0 ) GOTO 500

      CALL ftpkys(ounit, 'HDUCLAS1', 'XSPEC TABLE MODEL',
     &            'model spectra for XSPEC', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLAS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUCLAS2', 'MODEL SPECTRA',
     &            'extension containing model spectra', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUCLAS2 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

      CALL ftpkys(ounit, 'HDUVERS1', '1.0.0',
     &            'version of format', status)
      if (lpri.ge.3) write (6,*)'after ftpkys'
      IF ( status .NE. 0 ) THEN
         WRITE(*,'(a)') 'Warning : Failed to write HDUVERS1 keyword'
         WRITE(*,'(a,i)') '          FITSIO error ', status
         status = 0
      ENDIF

c define the binary table

      CALL ftbdef(ounit, tfields, tform, 0, nrows, status)
      if (lpri.ge.3) write (6,*)'after ftbdef'
      contxt = 'Failed to define binary table'
      IF ( status .NE. 0 ) GOTO 500

c Loop round the rows writing the model spectra into the table.

      k = 0
      DO i = 1, nrows

c First the parameter values

         irec = i
         idiv = nrows
         DO j = 1, nintpm
            idiv = idiv/intntb(j)
            ipt = (irec-1)/idiv + 1
            work(j) = inttab(ipt, j)
            irec = irec - (ipt-1)*idiv
            if (lpri.ge.3) write (6,*)
     $       i,intntb(j),idiv,i,ipt,work(j),irec
         ENDDO

         CALL ftpcle(ounit, 1, i, 1, nintpm, work, status)
         if (lpri.ge.3) write (6,*)'after ftpcle paramval'
         contxt = 'Failed to write PARAMVAL column'
         IF ( status .NE. 0 ) GOTO 500

c Then the interpolation spectrum

         k = k + 1
         if (lpri.ge.3) write (6,*)k,model(1,k)
         CALL ftpcle(ounit, 2, i, 1, nenerg-1, model(1,k), status)
         if (lpri.ge.3) write (6,*)'after ftpcle intpspec'
         contxt = 'Failed to write INTPSPEC column'
         IF ( status .NE. 0 ) GOTO 500

c Then the additional spectra

         DO j = 1, naddpm
            k = k + 1
            CALL ftpcle(ounit, j+2, i, 1, nenerg-1, model(1,k), status)
            if (lpri.ge.3) write (6,*)'after ftpcle model'
            contxt = 'Failed to write '//ttype(j+1)//' column'
            IF ( status .NE. 0 ) GOTO 500
         ENDDO

      ENDDO

 500  CONTINUE
      IF (status .NE. 0) THEN
         ierr = 5
         RETURN
      ENDIF

      
c ---------------------------------------------------------------
c Close the FITS file
c ---------------------------------------------------------------

      CALL ftclos(ounit, status)
      if (lpri.ge.3) write (6,*)'after ftclos'

      IF (status .NE. 0) THEN
         ierr = 6
         RETURN
      ENDIF

      RETURN
      END
