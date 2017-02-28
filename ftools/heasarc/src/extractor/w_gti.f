
      SUBROUTINE W_GTI(Lun, Imaxgti, Maxgti, Gti, Gtihdunm1,
     &                 expos1, Iccd, Gtiext, Status)

      IMPLICIT NONE

      INCLUDE 'extractor.inc'
 
      INTEGER Maxgti
      DOUBLE PRECISION Gti(3,Maxgti),expos1
      INTEGER Imaxgti, Lun, Iccd, Status
      CHARACTER*(*) Gtiext
      CHARACTER*(*) Gtihdunm1
      
 
C Write out the GTI extension
C Arguments :
C    Lun         i        i: i/o channel for output FITS file
C    Imaxgti     i        i: number of GTIs
C    Maxgti      i        i: maximum size of GTI array
C    Gti         d        i: GTIs
C    Gtihdunm1   c        i: HDUNAME keyword to output
C    Expos1      d        i: Exposure
C    Iccd        i        i: CCD number for this GTI
C    Gtiext      c        i: Name of GTI extension to use if one is not
C                            specified in the data subspace keywords
C    Status      i        r: 0==OK

      DOUBLE PRECISION temp
 
      INTEGER i, tfields, ngti

      character(30) ttype(3)
      character(10) tform(3)
      character(20) tunit(3)
 
      CHARACTER(30) extnam
      CHARACTER(72) contxt

      INTEGER lenact
      EXTERNAL lenact

      character(40) taskname
      COMMON / task / taskname
 
      DO 100 i = 1 , 3
         ttype(i) = ' '
         tform(i) = ' '
         tunit(i) = ' '
 100  CONTINUE

C If there is only one interval and it ran from 0.0 to 0.0 then this
C was used as a place-holder so don't write it out.

      IF ( Imaxgti .EQ. 1 .AND. Gti(1,1) .EQ. 0.0d0 .AND. 
     &     Gti(2,1) .EQ. 0.0d0 ) THEN
         ngti = 0
      ELSE
         ngti = Imaxgti
      ENDIF

      Status = 0 

      CALL FTCRHD(Lun,Status)
 
C     Initialize the number of rows and define
c     the fields....
 
      tfields = 2
 
      ttype(1) = 'START'
      tform(1) = '1D'
      tunit(1) = timeunit(:lenact(timeunit))
 
      ttype(2) = 'STOP'
      tform(2) = '1D'
      tunit(2) = timeunit(:lenact(timeunit))

C     Set the extension name 

      CALL g1tblrf(Iccd, 'TIME', extnam, Status)
      IF ( Status .NE. 0 ) THEN
         extnam = Gtiext
         Status = 0
      ENDIF

      CALL FTPHBN(Lun,ngti,tfields,ttype,tform,tunit,extnam,0,
     &            Status)
      CALL FTRDEF(Lun,Status)
 
      CALL FTUKYS(Lun,'hduclass','OGIP',
     &              'File conforms to OGIP/GSFC conventions',Status)
      CALL FTUKYS(Lun,'hduclas1','GTI',
     &              'File contains Good Time Intervals',Status)
      CALL FTUKYS(Lun,'hduclas2','STANDARD',
     &              'File contains Good Time Intervals',Status)

      temp=expos
      expos=expos1
      CALL wstdky(Lun, Status)
      expos=temp
      contxt = 'Failed to write standard keywords'
      IF ( Status .NE. 0 ) GOTO 999

C Write out the ASC data model keywords

      CALL FTUKYS(Lun,'HDUNAME',Gtihdunm1,'ASCDM block name',Status)
      CALL FTUKYS(Lun,'MTYPE1','TIME','Data type',Status)
      CALL FTUKYS(Lun,'MFORM1','START,STOP',
     &            'names of the start and stop columns',Status)
      CALL FTUKYS(Lun,'METYP1','R',
     &            'data descriptor type: Range, binned data',Status)
      contxt = 'Failed to write ASC data model keywords'
      IF ( Status .NE. 0 ) GOTO 999

C Write the GTIs

      CALL FTBDEF(Lun,tfields,tform,0,ngti,Status)

      DO i = 1 , ngti
 
         CALL FTPCLD(Lun,1,i,1,1,Gti(1,i),Status)
         CALL FTPCLD(Lun,2,i,1,1,Gti(2,i),Status)
 
      ENDDO

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF
 
      RETURN
      END
