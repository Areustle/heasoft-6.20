      
      SUBROUTINE gtimap(imgu, imgfil, immap, imsiz1, imsiz2, 
     &                  wbinfac, row, status)

      INTEGER imgu, imsiz1, imsiz2, status
      INTEGER wbinfac
      REAL immap(imsiz1, imsiz2), row(imsiz1)
      CHARACTER*(*) imgfil

c Subroutine to get the IMMAP from the image file. The image is binned up
c to match the WMAP. NB we assume that the image covers the entire detector.

c Arguments :
c     imgu     i        i: I/O unit for spectrum file
c     imgfil   c        i: name of spectrum file 
c     immap    r        r: IMMAP values
c     imsiz1   i        r: Actual size of IMMAP array
c     imsiz2   i        r: Actual size of IMMAP array
c     wbinfac  i        i: Number of pixels per WMAP bin (1-D)
c     row      r        w: Workspace vector used in reading the image
c     status   i        r: Status   0=OK

c  kaa  5/19/94    Based on GTWMAP and GTGMAP

      INTEGER spsize, imgbin, ibnfac
      INTEGER i, j, k, l, fpixel
 
      CHARACTER comment*80, contxt*72

      LOGICAL anyf

      INTEGER fcstln
      EXTERNAL fcstln

* open the image file

      CALL ftopen(imgu,imgfil,0,spsize,status)
      contxt = 'Error opening image file'
      IF ( status .NE. 0 ) GOTO 999

c Get the image binning factor

      imgbin = 1
      CALL ftgkyj(imgu,'IMGBIN',imgbin,comment,status)
      IF ( status .NE. 0 ) status = 0

* now set the relative binning for the image

      ibnfac = wbinfac/imgbin

* read in the image and bin it up as we go along

      anyf = .FALSE.
      DO i = 1, imsiz2
         DO j = 1, imsiz1
            immap(j,i) = 0.
         ENDDO
      ENDDO

      fpixel = 1
      DO i = 1, imsiz2
         DO l = 1, ibnfac
            CALL ftgpve(imgu, 0, fpixel, (imsiz1*ibnfac), 0, row, 
     &                  anyf, status)
            IF ( status .NE. 0 ) THEN
               CALL fcerr('Error reading image file')
               CALL fcecho(imgfil(:fcstln(imgfil)))
               CALL fcerrm(status)
               GOTO 999
            ENDIF
            DO j = 1, imsiz1
               DO k = 1, ibnfac
                  immap(j,i) = immap(j,i) + row((j-1)*ibnfac+k)
               ENDDO
            ENDDO
            fpixel = fpixel + imsiz1*ibnfac
         ENDDO
      ENDDO

* that's all we need from the image file so close.

      CALL ftclos(imgu,status)
      contxt = 'Error closing image file'
      IF ( status .NE. 0 ) GOTO 999

      RETURN

 999  CONTINUE
      CALL fcerr(contxt)
      CALL fcerrm(status)

      RETURN
      END
