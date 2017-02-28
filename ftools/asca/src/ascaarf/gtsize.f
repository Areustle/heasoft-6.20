
      SUBROUTINE gtsize(spu, spfil, rsu, rsfil, wmsize, wmstrt, wmend,
     &                  wmrep, nenerg, status)

      IMPLICIT NONE

      INTEGER spu, rsu, wmsize(2), wmstrt(2), wmend(2), wmrep
      INTEGER nenerg, status
      CHARACTER*(*) spfil, rsfil

c Routine to read the spectrum and get the size of the WMAP and read
c the RMF file and get the number of energies.

      INTEGER MAXDIM
      PARAMETER(MAXDIM=99)

      INTEGER spsize, spbpp, spnax, sppax(maxdim), sppc, spgc
      INTEGER rssize, rstype, iext, ipow, i
      INTEGER wmrebin, ihdvrs

      CHARACTER rstnam*64, instr*4, comment*80, contxt*72
      CHARACTER hduvers*10

      LOGICAL spxt, spsimp
      LOGICAL found

* open the spectral file

      CALL ftopen(spu,spfil,0,spsize,status)
      contxt = 'Error opening spectrum file'
      IF ( status .NE. 0 ) GOTO 999

* get the size of the primary array

      CALL ftghpr(spu,maxdim,spsimp,spbpp,spnax,sppax,sppc,spgc,
     -            spxt,status)
      contxt = 'Error reading primary array description'
      IF ( status .NE. 0 ) GOTO 999

      IF ( spnax .NE. 2 ) THEN
         contxt = 'This spectrum file does not have a primary image.'
         CALL fcecho(contxt)
         contxt = 'ASCAARF cannot be used.'
         status = 1
         GOTO 999
      ENDIF

c Get the HDUVERS.

      CALL ftgkys(spu,'HDUVERS',hduvers,comment,status)
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL ftgkys(spu,'HDUVERS1',hduvers,comment,status)
      ENDIF
      IF ( status .NE. 0 ) THEN
         ihdvrs = 0
         status = 0
      ELSE
         READ(hduvers(1:1),'(i1)') ihdvrs
      ENDIF

* Get the rebin factor in the WMAP for subsequent comparison with
* the image map bin factor

      IF ( ihdvrs .LT. 2 ) THEN
         CALL ftgkyj(spu,'WMREBIN',wmrebin,comment,status)
         contxt = 'Cannot find WMREBIN keyword'
      ELSE
         CALL ftgkyj(spu,'CDELT1P',wmrebin,comment,status)
         contxt = 'Cannot find CDELT1P keyword'
      ENDIF
      IF ( status .NE. 0 ) GOTO 999

c Get the instrument

      CALL ftgkys(spu,'INSTRUME',instr,comment,status)
      contxt = 'Cannot find INSTRUME keyword'
      IF ( status .NE. 0 ) GOTO 999

      if (instr.eq.'SIS0' .or. instr.eq.'SIS1') then

         DO i = 1, 2
            wmsize(i) = sppax(i)
            wmstrt(i) = 1
            wmend(i) = sppax(i)
         ENDDO
         wmrep = 1

c For GIS need to ensure that wmsize is a power of 2 so
c the fft works. The extra 20 is added on to avoid edge
c effects in the convolution.

      elseif (instr.eq.'GIS2' .or. instr.eq.'GIS3') then

         DO i = 1, 2
            sppax(i) = sppax(i) * wmrebin
            ipow = 0
            DO WHILE ( sppax(i)+20 .GE. 2**ipow )
               ipow = ipow + 1
            ENDDO
            wmsize(i) = 2**ipow
            IF ( wmsize(i) .GT. 256 ) wmsize(i) = 256
            wmstrt(i) = (wmsize(i)-sppax(i))/2
            wmend(i) = wmstrt(i) + sppax(i) - 1
         ENDDO
         wmrep = wmrebin
         wmrebin = 1

      else

         call fcerr('Unrecognised INSTRUME keyword')
         status = 3000
         RETURN

      endif

* Close the spectral file

      CALL ftclos(spu,status)
      contxt = 'Error closing spectrum file'
      IF ( status .NE. 0 ) GOTO 999

* Open the RMF file

      CALL ftopen(rsu,rsfil,0,rssize,status)
      contxt = 'Error opening response file'
      IF ( status .NE. 0 ) GOTO 999

c need to find the extension containing the energies used in the RMF.

      iext = 1
      found = .FALSE.

      DO WHILE (.NOT. found)

         CALL ftmahd(rsu,iext,rstype,status)
         contxt = 'Failed to find extension with RMF energies'
         IF ( status .NE. 0 ) GOTO 999

         CALL ftgkys(rsu,'EXTNAME',rstnam,comment,status)
         IF (rstnam .EQ. 'SPECRESP MATRIX') found = .TRUE.
         IF (rstnam .EQ. 'MATRIX') found = .TRUE.
         status = 0
         CALL ftgkys(rsu,'HDUCLAS1',rstnam,comment,status)
         IF ( status.EQ.0 .AND. rstnam.EQ.'RESPONSE' ) THEN
            CALL ftgkys(rsu,'HDUCLAS2',rstnam,comment,status)
            IF ( rstnam .EQ. 'RSP_MATRIX' ) found = .TRUE.
         ENDIF
         status = 0
         iext = iext + 1

      ENDDO

* get the number of energy bins

      CALL ftgkyj(rsu,'NAXIS2',nenerg,comment,status)
      contxt = 'Failed to read NAXIS2 keyword'
      IF ( status .NE. 0 ) GOTO 999

* Close the RMF file

      CALL ftclos(rsu, status)
      contxt = 'Error closing response file'
      IF ( status .NE. 0 ) GOTO 999

      RETURN

 999  CONTINUE
      CALL fcerr(contxt)
      CALL fcerrm(status)

      RETURN
      END
