
      SUBROUTINE gtwmap1(wmu, wmfil, sensor, wmap, wmsiz1, wmsiz2,
     &                  wmoff, wbinfac, binsze, abnsze, optaxs, status)

      implicit none
      INTEGER wmu, sensor, wmsiz1, wmsiz2
      INTEGER wmoff(2), status
      INTEGER wbinfac
      REAL wmap(wmsiz1, wmsiz2)
      REAL optaxs(2)
      REAL binsze, abnsze
      CHARACTER*(*) wmfil

c Subroutine to get the WMAP from the spectral file. Returns the map values
c and keyword values from the spectral file. Note that the WMAP is read into
c the array leaving three bins on either end so that the XRT PSF can be
c convolved with the GIS PSF later on without having to worry about edge
c effects.

c Arguments :
c wmu       i    i: I/O unit for spectrum file
c wmfil     c    i: name of spectrum file
c sensor    i    i: The instrument in use
c wmap      r    r: WMAP
c wmsiz1    i    r: Actual size of WMAP array
c wmsiz2    i    r: Actual size of WMAP array
c wmoff     i    r: First pixel in WMAP in detector coordinates
c wbinfac   i    r: Number of pixels per WMAP bin (1-D)
c binsze    r    r: The size (mm) of each WMAP bin
c abnsze    r    r: The size (arcmin) of each WMAP bin
c optaxs    i    r: Optical axis in detector coordinates
c status    i    r: Status 0=OK

c  kaa  12/29/93    rearrangement of Yan's code in NPSARF
c  Jeff Guerber 1999-04-16:   Get pixel sizes from keywords CDELTn/WMREBIN,
c     instead of relying on hardcoded values (loses for eg. GIS 64^2 images)
c  kaa  1/10/02     added support for HDUVERS=2 wmaps

*    efl       effective focal length in mm

      DOUBLE PRECISION efl, pi
      PARAMETER (efl=3500.0)
      PARAMETER (pi=3.14159265358979323846264338327950)

      REAL xpsiz, ypsiz, axpsiz, aypsiz, val(2)

      INTEGER spsize
      INTEGER i, j, fpotc, ihdvrs

      CHARACTER comment*80, contxt*72, hduvers*10

      LOGICAL anyf

*position of the instruments optical axes, in pixels, unbinned
*this is current as of 18 nov 93. if the OPTIC# keywords are set
*in the file then these are used.

      REAL oax(0:3),oay(0:3)

      DATA oax /662.7222, 618.2777, 133.00, 119.36/
      DATA oay /559.0185, 773.8333, 130.96, 134.44/


c  open the WMAP file

      CALL ftopen(wmu,wmfil,0,spsize,status)
      contxt = 'Error opening spectrum file'
      IF ( status .NE. 0 ) GOTO 999

c Set the optical axis for this instrument. If the OPTIC# keywords
c are set then use these.

      CALL ftgkye(wmu,'OPTIC1',optaxs(1),comment,status)
      IF ( status .NE. 0 .OR. optaxs(1) .LT. 0 ) THEN
         status = 0
         optaxs(1) = oax(sensor)
      ENDIF
      CALL ftgkye(wmu,'OPTIC2',optaxs(2),comment,status)
      IF ( status .NE. 0 .OR. optaxs(2) .LT. 0 ) THEN
         status = 0
         optaxs(2) = oax(sensor)
      ENDIF

c Get the HDUVERS.

      CALL ftgkys(wmu,'HDUVERS',hduvers,comment,status)
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL ftgkys(wmu,'HDUVERS1',hduvers,comment,status)
      ENDIF
      IF ( status .NE. 0 ) THEN
         ihdvrs = 0
         status = 0
      ELSE
         READ(hduvers(1:1),'(i1)') ihdvrs
      ENDIF

c Get the rebin factor in the WMAP. pre HDUVERS=2 this is read from the
c WMREBIN keyword. With HDUVERS=2 it can be read from CDELT1P

      IF ( ihdvrs .LT. 2 ) THEN
         CALL ftgkyj(wmu,'WMREBIN',wbinfac,comment,status)
         contxt = 'Cannot find WMREBIN keyword'
      ELSE
         CALL ftgkyj(wmu,'CDELT1P',wbinfac,comment,status)
         contxt = 'Cannot find CDELT1P keyword'
      ENDIF
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL fcecho('Warning: Cannot find WMREBIN keyword, '//
     &       'defaulting to 1')
         wbinfac = 1
      ENDIF

c Get the keywords that give the WMAP offset in the detector
c    CRVAL1    -> wmoff(1) =   x position of first WMAP bin in image coords.
c    CRVAL2    -> wmoff(2) =   y position of first WMAP bin in image coords.
c and convert to WMAP offsets. Note in HDUVERS=1 the CRVAL were not really
c correct with binning factors not equal to 1 since they gave the position
c of the start of the first WMAP bin in the detector image in binned
c coordinates.

      IF ( ihdvrs .GE. 2 ) THEN

         CALL ftgkye(wmu,'CRVAL1P',val(1),comment,status)
         contxt = 'Cannot find CRVAL1P keyword'
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgkye(wmu,'CRVAL2P',val(2),comment,status)
         contxt = 'Cannot find CRVAL2P keyword'
         IF ( status .NE. 0 ) GOTO 999
         wmoff(1) = NINT(val(1)-(wbinfac-1)/2.)
         wmoff(2) = NINT(val(2)-(wbinfac-1)/2.)

      ELSE

         CALL ftgkye(wmu,'CRVAL1',val(1),comment,status)
         contxt = 'Cannot find CRVAL1 keyword'
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgkye(wmu,'CRVAL2',val(2),comment,status)
         contxt = 'Cannot find CRVAL2 keyword'
         IF ( status .NE. 0 ) GOTO 999
         wmoff(1) = NINT(val(1)*wbinfac-(wbinfac-1))
         wmoff(2) = NINT(val(2)*wbinfac-(wbinfac-1))

      ENDIF

c Set the pixel size for the instrument.  CDELT1/2 are sizes of the
c rebinned pixels.  The keywords should be correct for all image sizes, but
c the 0.25 default for GIS is only correct for 256^2 images.

      xpsiz = 0.0
      ypsiz = 0.0
      call ftgkye(wmu, 'CDELT1', xpsiz, comment, status)
      call ftgkye(wmu, 'CDELT2', ypsiz, comment, status)
      xpsiz = xpsiz / float(wbinfac)
      ypsiz = ypsiz / float(wbinfac)

      if (status .ne. 0) then
          IF (sensor .LE. 1) THEN
              xpsiz = 0.027
              ypsiz = 0.027
          ELSE
              xpsiz = 0.25
              ypsiz = 0.25
          ENDIF
          write (contxt,'(a,f6.3,a)')  'Warning: Cannot read ' //
     &        'CDELT1/2 kwds, using default pixel size = ', xpsiz, ' mm'
          call fcecho(contxt)
          status = 0
      endif

* calculate the WMAP bin size in mm

      binsze = xpsiz

*Now calculate the angular size of a WMAP bin.
*GIS: 256x256 picture. each pixel .234'. maxtheta=29.95'
*SIS: 840x844 picture. each pixel .0265'. maxtheta=11.13'
*NB: axpsiz is calculated to be .0265 indeed from the numbers given in
*the SIS spec files, but is calculated to be .246 from the numbers given in
*the GIS spec files. Right now, I am using the numbers from the spec.
*[This refers to the default values.]

*The angular size of the pixel in radians is:

      axpsiz = atan(xpsiz/efl)
      aypsiz = atan(ypsiz/efl)

*These should be the same -- I'll assume from now on that this is the case.
*Giving the angular size of an (unrebinned) WMAP bin in arcminutes as:

      abnsze = axpsiz * (180./pi) * 60.

* Now read in the spectrum's primary array (wmap). Note that it is read
* into the central (4,wmsize-3) region of the array and the ends are filled
* with zeroes.

      fpotc=1
      anyf=.false.

      DO i = 1, wmsiz2
         DO j = 1, wmsiz1
            wmap(j,i) = 0.
         ENDDO
      ENDDO

      DO i = 1, wmsiz2

         CALL ftgpve(wmu, 0, fpotc, wmsiz1, 0, wmap(1,i), anyf, status)
         WRITE(contxt,'(a,i5)') 'Error reading WMAP row ', i
         IF ( status .NE. 0 ) GOTO 999

         fpotc = fpotc + wmsiz1

      ENDDO

* that's all we need from WMAP so close

      CALL ftclos(wmu,status)
      contxt = 'Error closing spectrum file'
      IF ( status .NE. 0 ) GOTO 999

      RETURN

 999  CONTINUE
      CALL fcerr(contxt)
      CALL fcerrm(status)

      RETURN
      END
