
      SUBROUTINE gtsmap(sensor, cchip, imsiz1, imsiz2, chpmap)

      INTEGER sensor, imsiz1, imsiz2
      INTEGER*2 chpmap(imsiz1, imsiz2)
      CHARACTER cchip*4

c Subroutine to make SIS chip map

c Arguments :
c      sensor  i        i: sensor
c      cchip   i        i: chips
c      imsiz1  i        i: actual size of image
c      imsiz2  i        i: actual size of image
c      chpmap  i*2      r: sets image pixels in the valid chips to 1
c                          and 0 otherwise


      INTEGER i, j, c
      REAL*8 rawx, rawy
      INTEGER ichip(0:3)

      INTEGER fcstln
      EXTERNAL fcstln

      DO i = 0, 3
         ichip(i) = 0
      ENDDO
      DO i = 1, fcstln(cchip)
         READ(cchip(i:i),'(i1)') j
         ichip(j) = 1
      ENDDO

      DO j = 1, imsiz2
         DO i = 1, imsiz1
            CALL jc_sis_detch2raw(sensor, DBLE(i), DBLE(j),
     &                                  c, rawx, rawy)
            IF ( c .GE. 0 .AND. c .LE. 3 ) THEN
               chpmap(i,j) = ichip(c)
            ELSE
               chpmap(i,j) = 0
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END
