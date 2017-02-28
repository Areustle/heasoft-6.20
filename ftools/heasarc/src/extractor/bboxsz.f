
      SUBROUTINE bboxsz(Imszh1, Imszh2, Imageh, Wtmapfix, Bbox)

      IMPLICIT NONE

      INTEGER Imszh1, Imszh2

      REAL Imageh(Imszh1, Imszh2)

      INTEGER Bbox(4)

      LOGICAL Wtmapfix

c Subroutine to find the bounding box around the parts of Imageh > 0

c Arguments :
c       Imszh1       i        i: Dimension 1 of Imageh
c       Imszh2       i        i: Dimension 2 of Imageh
c       Imageh       r        i: Array
c       Wtmapfix     l        i: If true then pixels outside the selected
c                                region have been set to -1.
c       Bbox         i        r: Array > 0 lies within the box with lower left
c                                corner (Bbox(1),Bbox(2)) and upper right
c                                corner (Bbox(3),Bbox(4))

      INTEGER i, j

      bbox(1) = Imszh1
      bbox(2) = Imszh2
      bbox(3) = 1
      bbox(4) = 1

c If wtmapfix has been set then define the bounding box as all pixels with
c values not -1.

      IF ( wtmapfix ) THEN

         DO j = 1 , Imszh2
            DO i = 1 , Imszh1
               IF ( Imageh(i,j) .NE. -1 ) THEN
                  bbox(1) = MIN(bbox(1),i)
                  bbox(2) = MIN(bbox(2),j)
                  bbox(3) = MAX(bbox(3),i)
                  bbox(4) = MAX(bbox(4),j)
               ENDIF
            ENDDO
         ENDDO

      ELSE

c If wtmapfix has not been set then define the bounding box as all pixels with
c values greater than zero

         DO j = 1 , Imszh2
            DO i = 1 , Imszh1
               IF ( Imageh(i,j).GT.0 ) THEN
                  bbox(1) = MIN(bbox(1),i)
                  bbox(2) = MIN(bbox(2),j)
                  bbox(3) = MAX(bbox(3),i)
                  bbox(4) = MAX(bbox(4),j)
               ENDIF
            ENDDO
         ENDDO

      ENDIF

      RETURN
      END

