
      SUBROUTINE gtsize1(spu, spfil, wmu, wmfil, rsu, rsfil,
     &                  npha, nbin, nchan, imsize, wmsize,
     &                  cgroup, sensor, status)

      implicit none
      INTEGER spu, wmu, rsu
      CHARACTER*(*) spfil, wmfil, rsfil
      INTEGER npha, nbin, nchan, imsize(2), wmsize(2), sensor, status
      CHARACTER*(*) cgroup

c Routine to read the spectrum and get the size of the WMAP and read
c the RMF file and get the number of energies. If an image file is
c read then get its size.

      INTEGER MAXDIM
      PARAMETER(MAXDIM=2)

      INTEGER wmbpp, wmnax, wmpax(MAXDIM), wmpc, wmgc
      INTEGER instrum, block

      CHARACTER comment*80, contxt*72, cinput*80

      LOGICAL wmxt, wmsimp

      character(4) cinstr(0:3)
      DATA cinstr / 'SIS0', 'SIS1', 'GIS2', 'GIS3' /

      INTEGER  fcstln
      EXTERNAL fcstln

* open the spectral file and go the SPECTRUM extension

      CALL ftopen(spu, spfil, 0, block, status)
      contxt = 'Error opening spectrum file'
      IF ( status .NE. 0 ) GOTO 999

      CALL fndext(spu, 'SPECTRUM', ' ', comment, status)
      contxt = 'Failed to find the SPECTRUM extension'
      IF ( status .NE. 0 ) GOTO 999

* get the INSTRUME and check against the sensor number

      CALL ftgkys(spu, 'INSTRUME', cinput, comment, status)
      contxt = 'Failed to read the INSTRUME keyword'
      IF ( status .NE. 0 ) GOTO 999

c Get the instrument

      IF ( index(cinput, 'SIS0') .GT. 0 ) THEN
         instrum = 0
      ELSEIF ( index(cinput, 'SIS1') .GT. 0 ) THEN
         instrum = 1
      ELSEIF ( index(cinput, 'GIS2') .GT. 0 ) THEN
         instrum = 2
      ELSEIF ( index(cinput, 'GIS3') .GT. 0 ) THEN
         instrum = 3
      ELSE
         instrum = -1
         CALL fcecho('Unrecognised INSTRUME keyword')
      ENDIF

* check instrument with sensor

      IF ( instrum .NE. sensor ) THEN
         CALL fcecho('Warning : file seems to be for wrong instrument')
         comment = 'File : '//spfil(:fcstln(spfil))
         CALL fcecho(comment)
         comment = 'Found '//cinput(:fcstln(cinput))//
     &             ' but expected '//cinstr(sensor)
         CALL fcecho(comment)
      ENDIF

c Now find the number of bins after grouping

      CALL ftgkyj(spu, 'NAXIS2', nchan, comment, status)
      contxt = 'Failed to read NAXIS2 keyword'
      IF (status .NE. 0) GOTO 999

      CALL rdgrpc(spu, 1, nchan, LEN(cgroup), cgroup, nbin, status)
      contxt = 'Failed to find the number of grouped bins'
      IF (status .NE. 0) GOTO 999

* Close the spectrum file

      CALL ftclos(spu, status)
      contxt = 'Error closing spectrum file'
      IF ( status .NE. 0 ) GOTO 999

* Open the WMAP file

      CALL ftopen(wmu,wmfil,0,block,status)
      contxt = 'Error opening WMAP file'
      IF ( status .NE. 0 ) GOTO 999

* get the size of the primary array

      CALL ftghpr(wmu,maxdim,wmsimp,wmbpp,wmnax,wmpax,wmpc,wmgc,
     &            wmxt,status)
      contxt = 'Error reading WMAP primary array description'
      IF ( status .NE. 0 ) GOTO 999

      IF ( wmnax .NE. 2 ) THEN
         contxt = 'This WMAP file does not have a primary image.'
         CALL fcecho(contxt)
         contxt = 'ASCEFFMAP cannot be used.'
         CALL fcecho(contxt)
         STOP
      ENDIF

      wmsize(1) = wmpax(1)
      wmsize(2) = wmpax(2)

c Set image size.  For SIS always use 1280x1280; for GIS, try to read
c the RAWXBINS and RAWYBINS keywords

      if ( sensor .LT. 2 ) then
         imsize(1) = 1280
         imsize(2) = 1280
      else
          call ftgkyj(wmu, 'RAWXBINS', imsize(1), comment, status)
          call ftgkyj(wmu, 'RAWYBINS', imsize(2), comment, status)
          if (status .ne. 0) then
              contxt = 'ASCAEFFMAP: Warning: Could not read GIS image'//
     &            ' size from WMAP header, using 256x256'
              call fcecho(contxt)
              imsize(1) = 256
              imsize(2) = 256
              status = 0
          endif
      endif

* Close the WMAP file

      CALL ftclos(wmu,status)
      contxt = 'Error closing WMAP file'
      IF ( status .NE. 0 ) GOTO 999

* Open the RMF file

      CALL ftopen(rsu,rsfil,0,block,status)
      contxt = 'Error opening response file'
      IF ( status .NE. 0 ) GOTO 999

c need to find the extension containing the energies used in the RMF.

      CALL fndext(rsu, 'RESPONSE', 'EBOUNDS', contxt, status)
      contxt = 'Failed to find extension with RMF energies'
      IF ( status .NE. 0 ) GOTO 999

* get the number of pha bins

      CALL ftgkyj(rsu,'NAXIS2', npha, comment, status)
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
