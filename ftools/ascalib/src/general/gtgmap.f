
      SUBROUTINE gtgmap(spu, filenm, wmsiz1, wmsiz2, wbinfac,
     &                  wmoff, cinstr, ccnm0001, outmap, status)

      INTEGER spu, wmsiz1, wmsiz2
      INTEGER wmoff(2), wbinfac, status
      REAL outmap(wmsiz1, wmsiz2)
      CHARACTER*(*) filenm, cinstr, ccnm0001

c Subroutine to read the file GIS calibration file FILENM and return the
c values in the region defined by the WMAP.

c Arguments :
c      spu     i        i: I/O unit for file
c      filenm  c        i: filename
c      wmsiz1  i        i: actual size of WMAP
c      wmsiz2  i        i: actual size of WMAP
c      wbinfac i        i: WMAP rebinning factor
c      wmoff   i        i: \ (x,y) position on detector of (1,1) in the WMAP.
c      cinstr  c        i: Instrument - should be INSTRUME keyword
c      ccnm0001 c        i: Type of file - should be CCNM0001 keyword
c      outmap  r        r: output map
c      status  i        r: status   0=OK
      
c  kaa  12/30/93
c        4/19/94   Added check for WMAP being partly outside image (for
c                  people who are lazy about defining regions).
c  pdw   3/09/98   Close file before returning
c  ng    8/21/98   Added check for the boundary of the primary array.  

      INTEGER size, i, j, k, l, fpixel, nelem
      REAL row(256)

      character(64) kytest, comment

      LOGICAL anyf

      INTEGER fcstln
      EXTERNAL fcstln

c open the file

      CALL ftopen(spu, filenm, 0, size, status)
      IF ( status .NE. 0 ) THEN
         CALL fcerr('Error opening GIS calibration file')
         CALL fcecho(filenm(:fcstln(filenm)))
         CALL fcerrm(status)
         GOTO 999
      ENDIF

c check that this really is the correct file

      CALL ftgkys(spu, 'INSTRUME', kytest, comment, status)
      IF ( status .NE. 0 ) THEN
         CALL fcerr('Error reading INSTRUME keyword from cal file')
         comment = 'File : '//filenm(:fcstln(filenm))
         CALL fcecho(comment)
         CALL fcerrm(status)
      ENDIF
      IF ( cinstr(:fcstln(cinstr)) .NE. kytest(:fcstln(cinstr)) ) THEN
         CALL fcecho('Warning : file seems to be for wrong instrument')
         comment = 'File : '//filenm(:fcstln(filenm))
         CALL fcecho(comment)
         comment = 'Found '//kytest(:fcstln(cinstr))//
     &             ' but expected '//cinstr(:fcstln(cinstr))
         CALL fcecho(comment)
      ENDIF

      CALL ftgkys(spu, 'CCNM0001', kytest, comment, status)
      IF ( status .NE. 0 ) THEN
         CALL fcerr('Error reading CCNM0001 keyword from cal file')
         comment = 'File : '//filenm(:fcstln(filenm))
         CALL fcecho(comment)
         CALL fcerrm(status)
      ENDIF
      IF ( ccnm0001(:fcstln(ccnm0001)) .NE. kytest(:fcstln(ccnm0001))
     &       ) THEN
         CALL fcecho('Warning : cal. file seems to be wrong type')
         comment = 'File : '//filenm(:fcstln(filenm))
         CALL fcecho(comment)
         comment = 'Found '//kytest(:fcstln(ccnm0001))//
     &             ' but expected '//ccnm0001(:fcstln(ccnm0001))
         CALL fcecho(comment)
      ENDIF

c read in the appropriate section of the array

      anyf = .FALSE.

      nelem = wmsiz1*wbinfac

      fpixel = (wmoff(2)-1)*256 + wmoff(1)

      DO i = 1, wmsiz2
         DO j = 1, wmsiz1
            outmap(j,i) = 0.
         ENDDO
      ENDDO

      DO i = 1, wmsiz2
         DO l = 1, wbinfac
            IF ( fpixel .GT. 0 .AND. (fpixel+nelem) .LE. 256*256 ) THEN
               CALL ftgpve(spu, 0, fpixel, nelem, 0, row, anyf, status)
               IF ( status .NE. 0 ) THEN
                  CALL fcerr('Error reading GIS calibration file')
                  CALL fcecho(filenm(:fcstln(filenm)))
                  CALL fcerrm(status)
                  GOTO 999
               ENDIF
               DO j = 1, wmsiz1
                  DO k = 1, wbinfac
                     outmap(j,i) = outmap(j,i) + row((j-1)*wbinfac+k)
                  ENDDO
               ENDDO
            ELSE
	       GOTO 900 
            ENDIF
            fpixel = fpixel + 256
         ENDDO
      ENDDO
900   CONTINUE

      DO i = 1, wmsiz2
         DO j = 1, wmsiz1
            outmap(j,i) = outmap(j,i)/wbinfac**2
         ENDDO
      ENDDO

      CALL ftclos(spu, status)

 999  CONTINUE
      RETURN
      END



