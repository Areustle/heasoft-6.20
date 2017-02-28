
      SUBROUTINE rayres_init(fillist, nrayen, nraymp, rbnsze, raycen, 
     &                       status)

      REAL rbnsze, raycen(2)
      INTEGER status, nrayen, nraymp(2)
      CHARACTER*(*) fillist

c Routine to initialize the reading of raytracing results. Also allocates
c memory for arrays and sets up maps with 2 lowest energies.
c Arguments :
c    fillist      c      i: Input list of filenames
c    nrayen       i      r: Number of energy bins in raytrace data
c    nraymp       i      r: Size of raytrace images
c    rbnsze       r      r: Bin size of raytrace images (in mm)
c    raycen       r      r: The optical axis position in raytrace image coords
c    status       i      r: Status   0==OK

c Local variables

      REAL ein

      INTEGER ilun, i, size, istart

      CHARACTER filenm*255, comment*72, string*255

      INTEGER  lenact, lchop
      EXTERNAL lenact, lchop

      status = 0

c Read the filenames to get the number of energies available

      CALL getlun(ilun)
      OPEN(unit=ilun, file=fillist, status='old', iostat=status)
      IF ( status .NE. 0 ) THEN
         CALL fcecho('Failed to read list of available files')
         RETURN
      ENDIF

c Loop round once to find the number of energies

      i = 0
      READ(ilun, *, iostat=status) ein
      DO WHILE ( status .EQ. 0 )

         i = i + 1
         READ(ilun, *, iostat=status) ein

      ENDDO
      status = 0
      nrayen = i

      WRITE(string,'(a,i5)') 'Number of raytrace energies = ', nrayen
      CALL fcecho(string)

c Reread the first line to get an image filename

      REWIND(ilun)
      READ(ilun, '(a)') string
      istart = lchop(string)
      filenm = string(index(string(istart:),' ')+istart:)

      CLOSE(ilun)
      CALL frelun(ilun)

c Find the size of the map arrays and the binsize (assuming square pixels)

      CALL getlun(ilun)
      CALL ftopen(ilun, filenm, 0, size, status)
      CALL ftgkyj(ilun, 'NAXIS1', nraymp(1), comment, status)
      CALL ftgkyj(ilun, 'NAXIS2', nraymp(2), comment, status)
      CALL ftgkye(ilun, 'CRPIX1', raycen(1), comment, status)
      CALL ftgkye(ilun, 'CRPIX2', raycen(2), comment, status)
      CALL ftgkye(ilun, 'CDELT1', rbnsze, comment, status)
      CALL ftclos(ilun, status)
      CALL frelun(ilun)
      IF ( status .NE. 0 ) THEN
         CALL fcerr('Failed to read raytrace file')
         CALL fcerrm(status)
         RETURN
      ENDIF

      WRITE(string,'(a,i4,a,i4)') 'Size of raytrace images = ', 
     &                            nraymp(1), ' x ', nraymp(2)
      CALL fcecho(string)
      WRITE(string,'(a,f9.5,a)') '                binsize = ', 
     &                            rbnsze, ' mm'
      CALL fcecho(string)

      RETURN
      END

c ************************************************************************

      FUNCTION rayres(energy, x, y, fillist, ratio, nrayen, rayene, 
     &                nraymp1, nraymp2, raymp1, raymp2, raympi)
      REAL rayres, energy, x, y, ratio
      INTEGER nrayen, nraymp1, nraymp2
      REAL rayene(nrayen), raymp1(nraymp1,nraymp2)
      REAL raymp2(nraymp1,nraymp2), raympi(nraymp1,nraymp2)
      CHARACTER*(*) fillist

c Function to interpolate in energy on the available maps and return
c the map value at (imap, jmap).
c Arguments :
c    energy         r       i: Interpolation energy
c    x, y           r       i: Map position
c    fillist        c       i: File containing list of raytrace files
c    ratio          r       i: WMAP binsize / Raytrace map binsize
c    nrayen         i       i: Number of raytrace energies
c    rayene         r       i: Vector of raytrace energies
c    nraymp1        i       i: X-axis size of raytrace image
c    nraymp2        i       i: Y-axis size of raytrace image
c    raymp1         r       i: Raytrace image
c    raymp2         r       i: Raytrace image
c    raympi         r       i: Raytrace image
c    rayres         r       r: Returned raytrace value

      REAL oenerg, f1, f2, xbox(2), ybox(2), xedge, yedge
      REAL areasum

      INTEGER i, j, ie1, ie2, ilun, status
      INTEGER ienerg1, ienerg2

      LOGICAL first

      SAVE oenerg, ienerg1, ienerg2, first

      DATA oenerg / 0.0 /
      DATA first /.TRUE./

c If the first time through then need to set up the array of energies
c and read in the first two maps

      IF ( first ) THEN

         first = .FALSE.
         CALL getlun(ilun)
         OPEN(unit=ilun, file=fillist, status='old', iostat=status)
         DO i = 1, nrayen
            READ(ilun, *) rayene(i)
         ENDDO
         CLOSE(ilun)
         CALL frelun(ilun)

         ienerg1 = 1
         ienerg2 = 2
         CALL rdrayt(fillist, ienerg1, nraymp1, nraymp2, raymp1)
         CALL rdrayt(fillist, ienerg2, nraymp1, nraymp2, raymp2)


      ENDIF

c Check whether we need to recalculate the interpolated map

      IF ( energy .NE. oenerg ) THEN

         oenerg = energy

c Check whether we need to read in new maps

         IF ( energy .LT. rayene(ienerg1) .OR. 
     &        energy .GT. rayene(ienerg2) ) THEN

c Find the energies immediately above and immediately below those
c chosen. This algorithm is not efficient but since it is only called
c once per energy the overhead is small.

            DO i = 1, nrayen
               IF ( energy .GE. rayene(i) ) ie1 = i
            ENDDO
            DO i = nrayen, 1, -1
               IF ( energy .LT. rayene(i) ) ie2 = i
            ENDDO

c Find out which maps we need to read. There are three possibilities.
c If energy > energ(ienerg2) = energ(ie1) then transfer raymp2 to raymp1 
c and read a new raymp2

            IF ( ie1 .EQ. ienerg2 ) THEN

               DO j = 1, nraymp2
                  DO i = 1, nraymp1
                     raymp1(i,j) = raymp2(i,j)
                  ENDDO
               ENDDO

               CALL rdrayt(fillist, ie2, nraymp1, nraymp2, raymp2)

c If energy < energ(energ1) = energ(ie2) then transfer raymp1 to raymp2
c and read a new raymp1

            ELSEIF ( ie2 .EQ. ienerg1 ) THEN

               DO j = 1, nraymp2
                  DO i = 1, nraymp1
                     raymp2(i,j) = raymp1(i,j)
                  ENDDO
               ENDDO

               CALL rdrayt(fillist, ie1, nraymp1, nraymp2, raymp1)

c Otherwise read new maps for both energies

            ELSE

               CALL rdrayt(fillist, ie1, nraymp1, nraymp2, raymp1)
               CALL rdrayt(fillist, ie2, nraymp1, nraymp2, raymp2)

            ENDIF

            ienerg1 = ie1
            ienerg2 = ie2

         ENDIF

c Create a new interpolated map. Note the trap for energies above or
c below those tabulated.

         IF ( energy .GT. rayene(ienerg2) ) THEN
            f1 = 0.0
            f2 = 1.0
         ELSEIF ( energy .LT. rayene(ienerg1) ) THEN
            f1 = 1.0
            f2 = 0.0
         ELSE
            f1 = (rayene(ienerg2) - energy)/
     &            (rayene(ienerg2)-rayene(ienerg1))
            f2 = (energy - rayene(ienerg1))/
     &            (rayene(ienerg2)-rayene(ienerg1))
         ENDIF

         DO j = 1, nraymp2
            DO i = 1, nraymp1
               raympi(i,j) = f1*raymp1(i,j) + f2*raymp2(i,j)
            ENDDO
         ENDDO

      ENDIF

c Define the corners of the WMAP bin that needs to be filled

      xbox(1) = x - ratio/2.
      xbox(2) = x + ratio/2.
      ybox(1) = y - ratio/2.
      ybox(2) = y + ratio/2.

c Loop over raytrace image bins that encapsulate the WMAP bin and
c sum contributions weighting by area of intersection of WMAP bin
c and raytrace image bin

      rayres = 0.0
      areasum = 0.0

      DO j = MAX(NINT(ybox(1)),1), MIN(NINT(ybox(2)),nraymp2)

         yedge = MIN(ybox(2),j+0.5) - MAX(ybox(1),j-0.5)

         DO i = MAX(NINT(xbox(1)),1), MIN(NINT(xbox(2)),nraymp1)

            xedge = MIN(xbox(2),i+0.5) - MAX(xbox(1),i-0.5)

            rayres = rayres + xedge * yedge * raympi(i,j)
            areasum = areasum + xedge * yedge

         ENDDO

      ENDDO

      RETURN
      END

c ************************************************************************

      SUBROUTINE rdrayt(fillist, ie, naxis1, naxis2, map)

      INTEGER ie, naxis1, naxis2
      REAL map(naxis1, naxis2)
      CHARACTER fillist*(*)

c Subroutine to read the raytrace image from the ie th energy map.
c Argument :
c    fillist      c         i: File listing names of raytrace image files
c    ie           i         i: Index into list
c    naxis1       i         i: Size of map
c    naxis2       i         i: Size of map
c    map          r         r: Raytrace map


      REAL rjunk
      INTEGER ilun, size, status, i, istart

      CHARACTER filenm*255, string*255

      LOGICAL qanyf

      INTEGER lchop
      EXTERNAL lchop

c Open the list of filenames and read the appropriate name

      CALL getlun(ilun)
      OPEN(unit=ilun, file=fillist, status='old', iostat=status)
      DO i = 1, ie-1
         READ(ilun, *)
      ENDDO
      READ(ilun, '(a)') string
      istart = lchop(string)
      filenm = string(index(string(istart:),' ')+istart:)
      CLOSE(ilun)
      CALL frelun(ilun)

c Open the FITS file and read the image

      CALL getlun(ilun)
      CALL ftopen(ilun, filenm, 0, size, status)
      CALL ftgpve(ilun, 0, 1, naxis1*naxis2, 0., map, qanyf, status)
      CALL ftclos(ilun, status)
      CALL frelun(ilun)
      IF ( status .NE. 0 ) THEN
         CALL fcerr('Failed to read raytrace file in RDRAYT')
         CALL fcerrm(status)
      ENDIF

      RETURN
      END

