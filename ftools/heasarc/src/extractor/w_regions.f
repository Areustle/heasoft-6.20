
      SUBROUTINE w_regions(Unit, Status)

      IMPLICIT NONE

      INTEGER Unit, Status

c Subroutine to write the regions in use to an ASC region extension. This
c routine assumes that the FITS file has been created and opened on Unit.
c In order to include any regions included in the event file in a regions
c extension we reread the regions info and reset the arrays.
c Arguments :
c     Unit          i      i: i/o channel for output FITS file
c     Status        i      r: 0==OK

      INCLUDE 'extractor.inc'

      INTEGER NFIELDS, MAXPTS
      PARAMETER (NFIELDS=6, MAXPTS=10000)

      DOUBLE PRECISION points(MAXPTS)

      INTEGER i, j, npos, ipt, nshapes, npoints, isign, icomp
      INTEGER nrvec, nrotang, ncopied

      character(30) ttype(NFIELDS)
      character(10) tform(NFIELDS)
      character(20) tunit(NFIELDS)
 
      CHARACTER(8) extnam
      CHARACTER(20) shape, mform
      CHARACTER(25) ctemp
      CHARACTER(72) contxt
      CHARACTER(255) infile1

      character(40) taskname
      COMMON / task / taskname

      INTEGER lenact, combine_all_regions, number_shapes
      CHARACTER(255) exevfl
      EXTERNAL lenact, exevfl, combine_all_regions, number_shapes

      DATA ttype /'X','Y','SHAPE','R','ROTANG','COMPONENT'/
      DATA tform /'1D','1D','16A','1D','1D','1I'/
      DATA tunit /'pixel','pixel',' ','pixel','degree',' '/

      Status = 0

c If there is no regionfile defined then just copy the region files from
c the event file and return

      IF ( regionfile .EQ. ' ' ) THEN
         CALL COPY_REGION_EXT(Unit, ncopied, Status)
         RETURN
      ENDIF

c Check for any region extension or data subspace keyword in the first event 
c filename. We assume that all input event files have been filtered in the same 
c way. If they have not been then the output is meaningless anyway.

      infile1 = exevfl(1, .FALSE., Status)

      Status = COMBINE_ALL_REGIONS(infile1, eventname)

c Copy any region extensions with coordinates different from the current
c image coordinates. This ensures that they get propagated.

      CALL COPY_REGION_EXT(Unit, ncopied, Status)

c Set the number of shapes to be written

      nshapes = number_shapes()

c Set the ttype for the X and Y fields from the image coordinates defined

      ttype(1) = Xcolf
      ttype(2) = Ycolf

c First loop through the regions finding the sizes of the arrays required.

      npos = 1
      nrvec = 1
      nrotang = 1

      DO i = 1, nshapes

         CALL GET_SHAPE_DATA(i, isign, shape, icomp, npoints, points)

         IF ( shape .EQ. "POLYGON" ) npos = npoints/2

         IF ( shape .EQ. "BOX" .OR. shape .EQ. "DIAMOND" .OR.
     &        shape .EQ. "ROTBOX" .OR. shape .EQ. "ELLIPSE" .OR. 
     &        shape .EQ. "ANNULUS" .OR.shape .EQ. "SECTOR" .OR.
     &        shape .EQ. "PANDA" ) nrvec = 2

         IF ( shape .EQ. "BOXANNULUS" .OR. shape .EQ. "ELLIPTANNULUS" 
     &        .OR. shape .EQ. "EPANDA" .OR. shape .EQ. "BPANDA")
     &        nrvec = 4

         IF ( shape .EQ. "BOXANNULUS" .OR. shape .EQ. "ELLIPTANNULUS" 
     &        .OR. shape .EQ. "SECTOR" .OR. shape .EQ. "PANDA" ) 
     &        nrotang = 2

         IF ( shape .EQ. "EPANDA" .OR. shape .EQ. "BPANDA" ) nrotang = 3

      ENDDO

      IF ( npos .LT. 10 ) THEN
         WRITE(tform(1),'(i1,a)') npos, 'D'
      ELSEIF ( npos .LT. 100 ) THEN
         WRITE(tform(1),'(i2,a)') npos, 'D'
      ELSEIF ( npos .LT. 1000 ) THEN
         WRITE(tform(1),'(i3,a)') npos, 'D'
      ELSE
         WRITE(tform(1),'(i4,a)') npos, 'D'
      ENDIF
      tform(2) = tform(1)

      WRITE(tform(4),'(i1,a)') nrvec, 'D'

      WRITE(tform(5),'(i1,a)') nrotang, 'D'

c Create the new HDU

      CALL FTCRHD(Unit, Status)

c Set the EXTNAME parameter

      extnam = regextname

c Initialize the number of rows and define the fields.

      CALL FTPHBN(Unit, nshapes, NFIELDS, ttype, tform, tunit, extnam, 
     &            0, Status)
      CALL FTRDEF(Unit, Status)

      CALL FTUKYJ(Unit, 'EXTVER', ncopied+1, ' ', Status)
      CALL FTUKYJ(Unit, 'EXTLEVEL', 1, 
     &            'Level i DB hierarchy: Data table', Status)
      CALL FTUKYS(Unit, 'HDUNAME', 'REGION', 'ASCDM block name', Status)
      CALL FTUKYS(Unit, 'HDUCLASS', 'ASC', 'Region extension', Status)
      CALL FTUKYS(Unit, 'HDUCLAS1', 'REGION', 'Region extension', 
     &            Status)
      CALL FTUKYS(Unit, 'HDUCLAS2', 'STANDARD', 'Region extension', 
     &            Status)
      CALL FTUKYS(Unit, 'HDUVERS', '1.0.0', ' ', Status)
      CALL FTUKYS(Unit, 'HDUDOC', 
     & 'ASC-FITS-REGION-1.0: McDowell, Rots:'//
     & ' FITS REGION Binary Table Design', ' ', Status)
      CALL FTUKYS(Unit, 'CONTENT', 'REGION', 'CXC Content Key', 
     &            Status)

      CALL wstdky(Unit, Status)
      contxt = 'Failed to write standard keywords'
      IF ( Status .NE. 0 ) GOTO 999

c Write the DM keywords. If we didn't read an MFORM1/MTYPE1 then construct
c one from the image column names.

      IF ( fmtype(1) .NE. ' ' ) THEN
         CALL FTPKYS(Unit,'mtype1',fmtype(1),
     &               'DM Keyword: Descriptor name.', Status)
         CALL FTPKYS(Unit,'mform1',fmform(1),
     &               'DM Keyword: Descriptor value.', Status)
      ELSE
         CALL FTPKYS(Unit,'mtype1','pos',
     &               'DM Keyword: Descriptor name.', Status)
         mform = xcolf(:lenact(xcolf))//','//ycolf(:lenact(ycolf))
         CALL FTPKYS(Unit,'mform1',mform,
     &               'DM Keyword: Descriptor value.', Status)
      ENDIF
      IF ( fmtype(2) .NE. ' ' ) THEN
         CALL FTPKYS(Unit,'mtype2',fmtype(2),
     &               'DM Keyword: Descriptor name.', Status)
         CALL FTPKYS(Unit,'mform2',fmform(2),
     &               'DM Keyword: Descriptor value.', Status)
      ENDIF

c Write the WCS keywords - note that the region positions and sizes will
c be written out in unbinned units.

      CALL FTUKYS(Unit,'tctyp1',fctype(1),' ',Status)
      IF ( fcname(1).NE.' ' ) CALL FTUKYS(Unit,'tcnam1',fcname(1),
     &                                    ' ',Status)
      CALL FTUKYD(Unit, 'tcrpx1', fcrpix(1), 15,
     &            'X axis reference pixel', Status)
      CALL FTUKYD(Unit,'tdrpx1',fcrpix(1),15,
     &            'original X axis reference pixel',Status)
      CALL FTUKYD(Unit,'tcrvl1',fcrval(1),15,
     &              'Coord of X ref pixel',Status)
      CALL FTUKYD(Unit,'tcdlt1',fcrdelt(1),15,
     &              'X axis increment',Status)
      CALL FTUKYD(Unit,'tddlt1',fcrdelt(1),15,
     &              'original X axis increment',Status)

      CALL FTUKYS(Unit,'tctyp2',fctype(2),'projection',Status)
      IF ( fcname(2).NE.' ' ) CALL FTUKYS(Unit,'tcnam2',fcname(2),
     &                                    ' ',Status)
      CALL FTUKYD(Unit, 'tcrpx2', fcrpix(2), 15,
     &            'Y axis reference pixel', Status)
      CALL FTUKYD(Unit,'tdrpx2',fcrpix(2),15,
     &            'original Y axis reference pixel',Status)
      CALL FTUKYD(Unit,'tcrvl2',fcrval(2),15,
     &              'Coord of Y ref pixel',Status)
      CALL FTUKYD(Unit,'tcdlt2',fcrdelt(2),15,
     &              'Y axis increment',Status)
      CALL FTUKYD(Unit,'tddlt2',fcrdelt(2),15,
     &              'original Y axis increment',Status)

      IF ( fcrota .GT. -999.D0 ) THEN 
         CALL FTUKYD(Unit,'tcrot2',fcrota,15,'Sky coord rotation angle',
     &               Status)
      ENDIF

c Write the binary table with the region descriptions

      CALL FTBDEF(Unit, NFIELDS, tform, 0, nshapes, Status)

c Loop round shapes writing the output arrays

      DO i = 1, nshapes

c Initialize the output arrays to zero

         DO j = 1, npos
            CALL FTPCLD(Unit, 1, i, j, 1, 0.0d0, Status)
            CALL FTPCLD(Unit, 2, i, j, 1, 0.0d0, Status)
         ENDDO
         DO j = 1, nrvec
            CALL FTPCLD(Unit, 4, i, j, 1, 0.0d0, Status)
         ENDDO
         DO j = 1, nrotang
            CALL FTPCLD(Unit, 5, i, j, 1, 0.0d0, Status)
         ENDDO

         CALL GET_SHAPE_DATA(i, isign, shape, icomp, npoints, points)

c Temporary fix for Swift XRT which doesn't recognize ROTBOX shape

         IF ( shape .EQ. "ROTBOX" ) shape = "BOX"

         IF ( isign .EQ. 0 ) THEN
            ctemp = "!"//shape
         ELSE
            ctemp = shape
         ENDIF

         IF ( shape .NE. "POLYGON" ) THEN
            CALL FTPCLD(Unit, 1, i, 1, 1, points(1), Status)
            CALL FTPCLD(Unit, 2, i, 1, 1, points(2), Status)
         ELSE 
            DO ipt = 1, npoints/2
               CALL FTPCLD(Unit, 1, i, ipt, 1, points(2*ipt-1), Status)
               CALL FTPCLD(Unit, 2, i, ipt, 1, points(2*ipt), Status)
            ENDDO
         ENDIF
               
         CALL FTPCLS(Unit, 3, i, 1, 1, ctemp, Status)

         IF ( shape .EQ. "PANDA" ) THEN
            CALL FTPCLD(Unit, 4, i, 1, nrvec, points(6), Status)
            CALL FTPCLD(Unit, 5, i, 1, nrotang, points(3), Status)
         ELSEIF ( shape .EQ. "EPANDA" .OR. shape .EQ. "BPANDA" ) THEN
            CALL FTPCLD(Unit, 4, i, 1, nrvec, points(6), Status)
            CALL FTPCLD(Unit, 5, i, 1, nrotang-1, points(3), Status)
            CALL FTPCLD(Unit, 5, i, 3, 1, points(11), Status)
         ELSEIF ( shape .NE. "POLYGON" ) THEN
            CALL FTPCLD(Unit, 4, i, 1, nrvec, points(3), Status)
            IF ( shape .NE. "CIRCLE" .AND. shape .NE. "ANNULUS" .AND. 
     &           shape .NE. "POINT" ) THEN
               CALL FTPCLD(Unit, 5, i, 1, nrotang, points(3+nrvec), 
     &                     Status)
            ENDIF
         ENDIF

         CALL FTPCLJ(Unit, 6, i, 1, 1, icomp, Status)

         WRITE(contxt,'(a,i4)') 'Failed to write region ', i
         IF ( Status .NE. 0 ) GOTO 999

      ENDDO

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF
 
      RETURN
      END

c ************************************************************************

      SUBROUTINE COPY_REGION_EXT(Unit, Ncopied, Status)

      IMPLICIT NONE

      INTEGER Unit, Ncopied, Status

c Copy any region extensions with coordinates different from the current
c image coordinates. Assumes that the output file is opened on Unit.
c     Unit          i      i: i/o channel for output FITS file
c     Ncopied       i      r: Number copied
c     Status        i      r: 0==OK

      INCLUDE 'extractor.inc'

      INTEGER evunit, i, blocksize, hdutype, icol

      CHARACTER(255) infile1
      CHARACTER(80)  contxt
      CHARACTER(8) chdunm
      CHARACTER(20) comment

      LOGICAL qcopy

      CHARACTER(255) exevfl
      INTEGER lenact
      EXTERNAL exevfl, lenact

c Get the name of the first input event file and open it. If we can't
c open it then give up and jump to closing the output event file

      infile1 = exevfl(1, .FALSE., Status)
      CALL getlun(evunit)
      CALL ftopen(evunit, infile1, 0, blocksize, Status)
      contxt = 'Failed to open '//infile1(:lenact(infile1))
      IF ( Status.NE.0 ) GOTO 999

c Loop round the input file extensions

      i = 2
      Ncopied = 0
      CALL FTMAHD(evunit,i,hdutype,Status)
      DO WHILE ( status .EQ. 0 )

c Check for HDUNAME='REGION' and then look for columns named xcolf and ycolf
c If the extension is region but xcolf or ycolf are not present then copy it.

         qcopy = .FALSE.

         CALL ftgkys(evunit, 'HDUNAME', chdunm,  comment, Status)
         IF ( status .EQ. 0 ) THEN
            CALL UPC(chdunm)
            IF ( chdunm(1:6) .EQ. 'REGION' ) THEN
               qcopy = .TRUE.
               CALL FTGCNO(evunit, .FALSE., xcolf, icol, Status)
               CALL FTGCNO(evunit, .FALSE., ycolf, icol, Status)
               IF ( Status .EQ. 0 ) qcopy = .FALSE.
            ENDIF
         ENDIF
         Status = 0

         IF ( qcopy ) THEN

            CALL ftcrhd(unit, Status)
            CALL ftcopy(evunit, unit, 0, Status)
            contxt = 'Failed to copy HDU to new event file'
            IF ( status .NE. 0 ) GOTO 999
            Ncopied = Ncopied + 1

         ENDIF

c Move onto next input HDU

         i = i + 1
         CALL FTMAHD(evunit,i,hdutype,Status)            

      ENDDO

      Status = 0

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcecho(contxt)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END
