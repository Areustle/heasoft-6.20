      Subroutine wrteff(efu, effil, spfil, wmfil, rsfil,
     &                  sensor, taskname,
     &                  imsize1, imsize2, efmap,
     &                  nbin, lower, hiher, pha, avgeff, weight,
     &                  status)

      Implicit none

c input
      CHARACTER*(*) effil, spfil, wmfil, rsfil, taskname
      INTEGER efu, sensor, imsize1, imsize2, nbin, status
      INTEGER*2 i
      REAL efmap(imsize1,imsize2)
      REAL lower(nbin), hiher(nbin)
      REAL pha(nbin), avgeff(nbin), weight(nbin)

c local
      character(8) instr
      CHARACTER comment*64, wrtstr*512
      INTEGER bitpix, naxes(2)
      REAL value

      INTEGER fcstln
      EXTERNAL fcstln

c constant
      INTEGER tfields
      PARAMETER (tfields = 8)
      character(10) ttype(tfields), tfrmt(tfields), tunit(tfields)
      DATA ttype / 'CHANNEL', 'ENERG_LO', 'ENERG_HI', 'COUNTS',
     &             'COUNTS/E', 'AVGEFF', 'WEIGHT', 'WEIGHT/E' /
      DATA tfrmt / '1I', '1E', '1E', '1E',
     &             '1E', '1E', '1E', '1E' /
      DATA tunit / ' ', 'keV', 'keV', 'count',
     &             'count/keV', 'cm**2', ' ', ' ' /

* write FITS primary header keywords

      bitpix = -32
      naxes(1) = imsize1
      naxes(2) = imsize2
      CALL ftphpr(efu, .TRUE., bitpix, 2, naxes, 0, 1, .TRUE., status)

* write in OGIP standard (?) keywords

      comment = 'Telescope (mission) name'
      CALL ftpkys(efu, 'TELESCOP', 'ASCA', comment, status)
      IF (sensor .EQ. 0) instr = 'SIS0'
      IF (sensor .EQ. 1) instr = 'SIS1'
      IF (sensor .EQ. 2) instr = 'GIS2'
      IF (sensor .EQ. 3) instr = 'GIS3'
      comment = 'Instrument name'
      CALL ftpkys(efu, 'INSTRUME', instr, comment, status)
      comment = 's/w task which wrote this dataset'
      CALL ftpkys(efu, 'CREATOR', taskname, comment, status)
      comment = 'PHA file name'
      CALL ftpkys(efu, 'PHAFILE', spfil, comment, status)
      comment = 'WMAP file name'
      CALL ftpkys(efu, 'WMAPFILE', wmfil, comment, status)
      comment = 'RMF file name'
      CALL ftpkys(efu, 'RMFFILE', rsfil, comment, status)

* define & write EFF image

      CALL ftpdef(efu, bitpix, 2, naxes, 0, 1, status)
      CALL ftppre(efu, 1, 1, imsize1*imsize2, efmap, status)

      IF ( status .NE. 0 ) THEN
         CALL fcerr('Error writing EFF image')
         CALL fcerrm(status)
         GOTO 999
      ENDIF

* create the extension for weighting information 

      CALL ftcrhd(efu, status)

* write extension table header

      CALL ftphbn(efu, nbin, tfields, ttype, tfrmt, tunit,
     &            'WEIGHTING', 0, status)

* define the structure of the table we're going to write

      CALL ftbdef(efu, tfields, tfrmt, 0, nbin, status)

* write the data into the table.

      DO i = 1, nbin
         CALL ftpcli(efu, 1, INT(i), 1, 1, i, status)
      ENDDO
      CALL ftpcle(efu, 2, 1, 1, nbin, lower, status)
      CALL ftpcle(efu, 3, 1, 1, nbin, hiher, status)
      CALL ftpcle(efu, 4, 1, 1, nbin, pha, status)      
      DO i = 1, nbin
         value = pha(i) / ( hiher(i) - lower(i) )
         CALL ftpcle(efu, 5, INT(i), 1, 1, value, status)
      ENDDO
      CALL ftpcle(efu, 6, 1, 1, nbin, avgeff, status)      
      CALL ftpcle(efu, 7, 1, 1, nbin, weight, status)
      DO i = 1, nbin
         value = weight(i) / ( hiher(i) - lower(i) )
         CALL ftpcle(efu, 8, INT(i), 1, 1, value, status)
      ENDDO

      IF ( status .NE. 0 ) THEN
         CALL fcerr('Error writing weighting to EFF image file')
         CALL fcerrm(status)
         GOTO 999
      ENDIF

c close EFF image file

      CALL ftclos(efu, status)
      IF ( status .NE. 0 ) THEN
         CALL fcerr('Error closing EFF image file')
         CALL fcerrm(status)
         GOTO 999
      ENDIF

      CALL fcecho(' ')
      WRITE(wrtstr, '(a,a)') 'wrote ', effil(:fcstln(effil))
      CALL fcecho(wrtstr)

 999  CONTINUE
      RETURN
      END
