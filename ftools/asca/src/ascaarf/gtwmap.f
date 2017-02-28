
      SUBROUTINE gtwmap(spu, spfil, wmap, wmsiz1, wmsiz2, wmstrt, wmend,
     &                  wmrep, wmoff, wbinfac, instrum, binsze, 
     &                  abnsze, optaxs, regsze, qnorti, qunif, status)

      IMPLICIT NONE

      INTEGER spu, wmsiz1, wmsiz2, wmstrt(2), wmend(2), wmrep
      INTEGER instrum, status
      INTEGER wmoff(2), wbinfac
      REAL optaxs(2)
      REAL wmap(wmsiz1, wmsiz2)
      REAL binsze, abnsze, regsze
      CHARACTER*(*) spfil
      LOGICAL qnorti, qunif

c Subroutine to get the WMAP from the spectral file. Returns the map values
c and keyword values from the spectral file. Note that the WMAP is read into
c the array leaving three bins on either end so that the XRT PSF can be
c convolved with the GIS PSF later on without having to worry about edge
c effects.

c Arguments :
c     spu     i        i: I/O unit for spectrum file
c     spfil   c        i: name of spectrum file 
c     wmap    r        r: WMAP values
c     wmsiz1  i        r: Actual size of WMAP array
c     wmsiz2  i        r: Actual size of WMAP array
c     wmstrt  i        r: First WMAP bin for input map
c     wmend   i        r: Last WMAP bin for input map
c     wmoff   i        r: Start of first bin in WMAP in detector coordinates
c     wbinfac i        r: Number of pixels per WMAP bin (1-D)
c     instrum i        r: ASCA instrument (0-3)
c     binsze  r        r: The size (mm) of each WMAP bin
c     abnsze  r        r: The size (arcmin) of each WMAP bin
c     regsze  r        r: The angular size (arcmin^2) of the selected region
c     optaxs  i        r: Optical axis in detector coordinates
c     qnorti  l        r: True if there is no RTI column available in GIS data
c     qunif   l        r: If true then assume a uniform input surface brightness
c     status  i        r: Status   0=OK

c  kaa  12/29/93    rearrangement of Yan's code in NPSARF
c       1/7/02      added support for HDUVERS=2 WMAPs

*    efl       effective focal length in mm

      DOUBLE PRECISION efl, pi
      PARAMETER (efl=3500.0)
      PARAMETER (pi=3.14159265358979323846264338327950)
      
      REAL xpsiz, ypsiz, axpsiz, aypsiz, pixsze, bckscl, factor
      REAL pix(2), val(2)

      INTEGER spsize
      INTEGER i, j, k, fpotc, irise, npxsou, wmsize(2), wmlast
      INTEGER ihdvrs
 
      CHARACTER instr*4, comment*80, contxt*72, hduvers*10

      LOGICAL anyf, usenul

*position of the instruments optical axes, in pixels, unbinned
*this is current as of 18 nov 93. if the OPTIC# keywords are set
*in the file then these are used.

      REAL oax(0:3),oay(0:3)

      DATA oax /662.72, 618.28, 133.00, 119.36/
      DATA oay /559.02, 773.83, 130.96, 134.44/

* open the spectral file

      CALL ftopen(spu,spfil,0,spsize,status)
      contxt = 'Error opening spectrum file'
      IF ( status .NE. 0 ) GOTO 999

c Get the instrument

      CALL ftgkys(spu,'INSTRUME',instr,comment,status)
      contxt = 'Cannot find INSTRUME keyword'
      IF ( status .NE. 0 ) GOTO 999

      if (instr.eq.'SIS0') then
         instrum=0
      elseif (instr.eq.'SIS1') then
         instrum=1
      elseif (instr.eq.'GIS2') then
         instrum=2
      elseif (instr.eq.'GIS3') then
         instrum=3
      else
         call fcerr('Unrecognised INSTRUME keyword')
         status = 3000
         RETURN
      endif

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

c Get the rebin factor in the WMAP. pre HDUVERS=2 this is read from the
c WMREBIN keyword. With HDUVERS=2 it can be read from CDELT1P

      IF ( ihdvrs .LT. 2 ) THEN
         CALL ftgkyj(spu,'WMREBIN',wbinfac,comment,status)
         contxt = 'Cannot find WMREBIN keyword'
      ELSE
         CALL ftgkyj(spu,'CDELT1P',wbinfac,comment,status)
         contxt = 'Cannot find CDELT1P keyword'
      ENDIF
      IF ( status .NE. 0 ) GOTO 999
      wbinfac = wbinfac / wmrep

c Set the bin size for the instrument. If the HDUVERS or HDUVERS1 keywords
c are set then get the bin sizes from the CDELT# keywords.

      IF ( ihdvrs .GT. 0 ) THEN
         CALL ftgkye(spu,'CDELT1',xpsiz,comment,status)
         contxt = 'Cannot read CDELT1 keyword'
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgkye(spu,'CDELT2',ypsiz,comment,status)
         contxt = 'Cannot read CDELT2 keyword'
         IF ( status .NE. 0 ) GOTO 999
         ypsiz = ABS(ypsiz)
         usenul = .TRUE.
      ELSE
         status = 0
         IF (instrum .LE. 1) THEN
            xpsiz = 0.027 * wbinfac
            ypsiz = 0.027 * wbinfac
         ELSE
            xpsiz = 0.25 * wbinfac
            ypsiz = 0.25 * wbinfac
         ENDIF
         usenul = .FALSE.
      ENDIF

c Set the optical axis for this instrument. If the OPTIC# keywords
c are set then use these.

      CALL ftgkye(spu,'OPTIC1',optaxs(1),comment,status)
      IF ( status .NE. 0 .OR. optaxs(1) .LT. 0 ) THEN
         status = 0
         optaxs(1) = oax(instrum)
      ENDIF
      CALL ftgkye(spu,'OPTIC2',optaxs(2),comment,status)
      IF ( status .NE. 0 .OR. optaxs(2) .LT. 0 ) THEN
         status = 0
         optaxs(2) = oay(instrum)
      ENDIF

c Get the keywords that give the WMAP offset in the detector
c    CRVAL1    -> wmoff(1) =   x position of first WMAP bin in detector image.
c    CRVAL2    -> wmoff(2) =   y position of first WMAP bin in detector image.
c and convert to WMAP offsets. Note in HDUVERS=1 the CRVAL were not really
c correct with binning factors not equal to 1 since they gave the position
c of the start of the first WMAP bin in the detector image in binned
c coordinates.


      IF ( ihdvrs .GE. 2 ) THEN

         CALL ftgkye(spu,'CRVAL1P',val(1),comment,status)
         contxt = 'Cannot find CRVAL1P keyword'
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgkye(spu,'CRVAL2P',val(2),comment,status)
         contxt = 'Cannot find CRVAL2P keyword'
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgkye(spu,'CRPIX1P',pix(1),comment,status)
         contxt = 'Cannot find CRVAL1P keyword'
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgkye(spu,'CRPIX2P',pix(2),comment,status)
         contxt = 'Cannot find CRVAL2P keyword'
         IF ( status .NE. 0 ) GOTO 999
         wmoff(1) = NINT(val(1) - (pix(1)-1.0)*wmrep*wbinfac
     &                   -(wmrep*wbinfac-1)/2.)
         wmoff(2) = NINT(val(2) - (pix(2)-1.0)*wmrep*wbinfac
     &                   -(wmrep*wbinfac-1)/2.)

      ELSE

         CALL ftgkye(spu,'CRVAL1',val(1),comment,status)
         contxt = 'Cannot find CRVAL1 keyword'
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgkye(spu,'CRVAL2',val(2),comment,status)
         contxt = 'Cannot find CRVAL2 keyword'
         IF ( status .NE. 0 ) GOTO 999
         wmoff(1) = NINT(val(1)*wmrep*wbinfac-(wmrep*wbinfac-1))
         wmoff(2) = NINT(val(2)*wmrep*wbinfac-(wmrep*wbinfac-1))

      ENDIF

c include any extra bins

      wmoff(1) = wmoff(1) - (wmstrt(1)-1)*wbinfac
      wmoff(2) = wmoff(2) - (wmstrt(2)-1)*wbinfac

c trap out the case where for the GIS the expanded WMAP actually falls off
c the edge of the detector.

      IF ( instrum .GE. 2 ) THEN

         DO i = 1, 2
            IF ( wmoff(i) .LE. 0 ) THEN
               wmstrt(i) = wmstrt(i) - (1-wmoff(i))/wbinfac
               wmend(i) = wmend(i) - (1-wmoff(i))/wbinfac
               wmoff(i) = 1
            ENDIF
         ENDDO

         wmsize(1) = wmsiz1
         wmsize(2) = wmsiz2
         DO i = 1, 2
            wmlast = wmoff(i) + wmsize(i) - 1
            IF ( wmlast .GT. 256 ) THEN
               wmstrt(i) = wmstrt(i) + (wmlast-256)/wbinfac
               wmend(i) = wmend(i) + (wmlast-256)/wbinfac
               wmoff(i) = 256 - wmsize(i) + 1
            ENDIF
         ENDDO

      ENDIF

* set the WMAP bin size

      binsze = xpsiz

*Now calculate the angular size of a WMAP bin.
*GIS: 256x256 picture. each pixel .234'. maxtheta=29.95'
*SIS: 840x844 picture. each pixel .0265'. maxtheta=11.13'
*NB: axpsiz is calculated to be .0265 indeed from the numbers given in
*the SIS spec files, but is caluculted to be .246 from the numbers given in
*the GIS spec files. Right now, I am using the numbers from the spec.

*The angular size of the pixel is:

      axpsiz=atan(xpsiz/efl)
      aypsiz=atan(ypsiz/efl)

*These should be the same -- I'll assume from now on that this is the case.
*Giving the angular size of a WMAP bin as :

      abnsze = axpsiz * (180./pi) * 60.
      
* Now read in the spectrum's primary array (wmap). Note that it is read
* into the central region of the array starting at (wmstrt(1),wmstrt(2)) 
* and the ends are filled with null values (-99).

      fpotc=1
      anyf=.false.

      DO i = 1, wmsiz2
         DO j = 1, wmsiz1
            wmap(j,i) = -99.
         ENDDO
      ENDDO

      DO i = wmstrt(2), wmend(2), wmrep

         CALL ftgpve(spu,0,fpotc,(wmend(1)-wmstrt(1)+1)/wmrep,-99.,
     &               wmap(wmstrt(1),i),anyf,status)
         WRITE(contxt,'(a,i5)') 'Error reading WMAP row ', i
         IF ( status .NE. 0 ) GOTO 999

         IF ( wmrep .gt. 1 ) THEN

* split wmap to smaller bin by wmrep

            DO j = 1, (wmend(1)-wmstrt(1)+1)/wmrep
               DO k = 1, wmrep
                  wmap(wmstrt(1)-1+(j-1)*wmrep+k,i+wmrep-1) 
     &                    = wmap(wmstrt(1)-1+j,i)
               ENDDO
            ENDDO
            DO j = i, i+wmrep-2
               DO k = wmstrt(1), wmend(1)
                  wmap(k, j) = wmap(k, i+wmrep-1)
               ENDDO
            ENDDO

         ENDIF

         fpotc=fpotc+(wmend(1)-wmstrt(1)+1)/wmrep

      ENDDO

* if we are not using the newer versions of the WMAP that use FITS nulls
* for bins outside the region then we need to make sure to set bins with
* zero counts to -99. This may lead to some bins within the region being
* ignored but we can't do anything about this.

      IF ( .NOT. usenul ) THEN
         DO i = 1, wmsiz2
            DO j = 1, wmsiz1
               IF ( wmap(j,i) .LE. 0. ) wmap(j,i) = -99.
            ENDDO
         ENDDO
      ENDIF

* if the WMAP has been expanded then ensure that the total flux stayed
* the same

      IF ( wmrep .GT. 1 ) THEN
         factor = wmrep * wmrep
         DO i = 1, wmsiz2
            DO j = 1, wmsiz1
               IF ( wmap(j,i) .GE. 0. ) wmap(j,i) = wmap(j,i)/factor
            ENDDO
         ENDDO
      ENDIF


* If the qunif flag has been set then all non-negative wmap values
* are set to one.

      IF ( qunif ) THEN
         DO i = 1, wmsiz2
            DO j = 1, wmsiz1
               IF ( wmap(j,i) .GE. 0. ) wmap(j,i) = 1.
            ENDDO
         ENDDO
      ENDIF

* if this is GIS then check the RISEBINS keyword to check whether RTI
* is available.

      qnorti = .FALSE.
      IF ( instrum .GE. 2 ) THEN
         CALL ftgkyj(spu,'RISEBINS',irise,comment,status)
         IF ( status .EQ. 0 .AND. irise .LE. 1 ) qnorti = .TRUE.
         status = 0
      ENDIF

* If the NPIXSOU and PIXSIZE keywords exist then read them and calculate
* size of the selected region. If they don't exist then use the BACKSCAL
* keyword to get the size.

      CALL ftgkyj(spu, 'NPIXSOU', npxsou, comment, status)
      CALL ftgkye(spu, 'PIXSIZE', pixsze, comment, status)
      IF ( status .EQ. 0 ) THEN
         regsze = npxsou * (pixsze**2) * 3600
      ELSE
         status = 0
         CALL ftgkye(spu, 'BACKSCAL', bckscl, comment, status)
         IF ( status .NE. 0 ) THEN
            regsze = -1.
            status = 0
         ELSE
            IF ( instrum .GE. 2 ) THEN
               regsze = bckscl * (256**2) * (0.24558**2)
            ELSE
               regsze = bckscl * (320**2) * (0.10608**2)
            ENDIF
         ENDIF
      ENDIF

* that's all we need from spec so close

      CALL ftclos(spu,status)
      contxt = 'Error closing spectrum file'
      IF ( status .NE. 0 ) GOTO 999

      RETURN

 999  CONTINUE
      CALL fcerr(contxt)
      CALL fcerrm(status)

      RETURN
      END

