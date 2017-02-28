      SUBROUTINE DO_BACK( MAP, SZX, SZY, MAPID,
     &                    OPTIMIZE, IBBACE, STATUS )
C*************************************************************
C
C  Does the background calculation
C
C  I  MAP        (R)  Image map
C  I  SZX/Y      (I)  Size of image map
C  I  MAPID      (S)  Map id string
C  I  OPTIMIZE   (L)  Whether to look for optimal boxsize
C I/O IBBACE     (I)  Box size in image pixels
C  O  STATUS     (I)  Error flag  (0 = OK)
C*************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      LOGICAL OPTIMIZE

      CHARACTER*(*) MAPID

      INTEGER*4 SZX
      INTEGER*4 SZY
      INTEGER*4 IBBACE
      INTEGER*4 STATUS

      REAL*4 MAP(SZX, SZY)

C***************
C Include files
C***************
      INCLUDE '../include/io.inc'
      INCLUDE 'backgd.inc'

C*****************
C Local variables
C*****************
      character(1) MAPTYPE

      INTEGER*4 IBMIN
      INTEGER*4 IBMAX
      INTEGER*4 IBX
      INTEGER*4 IBEST

      REAL*4 BACKBEST
      REAL*4 BACKGRO
      REAL*4 RBMIN

      REAL*8 ZMX
      REAL*8 ZMY

C************
C Initialize
C************
      STATUS = 0

C**************
C Get map info
C**************
      CALL GHEADD( MAPID, 'ZMX', ZMX, 0, STATUS )
      CALL GHEADD( MAPID, 'ZMY', ZMY, 0, STATUS )
      CALL GHEADS( MAPID, 'MAPTYPE', MAPTYPE, 0, STATUS )

C*******************************************
C Determine which kind of statistics to use
C*******************************************
      IF ( MAPTYPE.EQ.'I' ) THEN
         CALL XWRITE( ' Calculating background: '//
     &                'Poisson statistics assumed' ,10 )
         POISSON = .TRUE.
      ELSE
         CALL XWRITE( '  Calculating background ', 10 )
C        CALL XWRITE( '  Gaussian statistics assumed ...', 10 )
C        CALL XWRITE( ' ** Warning: Not working properly yet ! ', 10 )
         POISSON = .FALSE.
      ENDIF

C*****************************************************************
C If optimizing and box size not specified loop over box sizes to
C determine the best one (lowest background)
C*****************************************************************
      IF ( OPTIMIZE .AND. IBBACE.EQ.0 ) THEN
         RBMIN = SQRT( FLOAT( SZX * SZY ) / FLOAT( MAXBOX ) )

C        Minimum of 8 pixels on a side per box
         IBMIN = 8
         DO WHILE ( RBMIN.GT.FLOAT( IBMIN ) ) 
            IBMIN = IBMIN * 2
         ENDDO

C        Maximum of 4 box sizes
C        IBMAX = SQRT( FLOAT( SZX * SZY ) / 16.0 )
         IBMAX = MAX( INT( MIN( SZX, SZY ) / 4.0 ), IBMIN )
         IBX   = IBMIN
         IBEST = 0
         BACKBEST = 0.0

C        Loop over box sizes, doubling the size each time
         DO WHILE ( IBX.LE.IBMAX )
            IBBAC = IBX

C           Call the background estimation routine
            CALL DETECT_BGR( MAP, SZX, SZY, MAPID, STATUS )

C           Check the results
            IF ( STATUS.EQ.0 ) THEN
               BACKGRO = BNEW / ( ZMX * ZMY )
               WRITE( ZWRITE, * ) IBX, BACKGRO
               CALL XWRITE( ZWRITE, 10 )
               IF ( IBEST.EQ.0 .OR. BACKBEST.GT.BACKGRO ) THEN
                  BACKBEST = BACKGRO
                  IBEST    = IBBAC
               ENDIF
            ELSE
               WRITE( ZWRITE, * ) IBX, '  failed'
               CALL XWRITE( ZWRITE, 10 )
            ENDIF

C           Double the size for next iteration
            IBX = FLOAT( IBX ) * 2.0
         ENDDO

C        Write out the best one we found
         WRITE( ZWRITE, 99001 ) IBEST
         CALL XWRITE( ZWRITE, 10 )
         IBBAC = IBEST

C********************************************
C Otherwise just set the background box size
C based on inputs and image size
C********************************************
      ELSE

C        If input size is zero, set to the smallest 
C        image dimension divided by 8
         IF ( IBBACE.EQ.0 ) THEN
            IBBAC = MIN( SZX, SZY ) / 8.0
            IF ( IBBAC.LT.8 ) THEN
               IBBAC = 8
            ENDIF
            IF ( IBBAC.GT.64 ) THEN
               IBBAC = 64
            ENDIF
            WRITE( ZWRITE, 99002 ) IBBAC
         ELSE
            IBBAC = IBBACE
            WRITE( ZWRITE, 99003 ) IBBAC
         ENDIF
      ENDIF

C****************************************************************
C Call the background estimation routine for the chosen box size
C****************************************************************
      CALL DETECT_BGR( MAP, SZX, SZY, MAPID, STATUS )

C     If this fails, reset the number of background boxes to zero
C     so that various tasks will try again, instead of using an
C     erroneous background "optimal" box size
      IF ( STATUS.NE.0 ) THEN
         NBOXES = 0
         RETURN
      ENDIF

C*************************
C Write out what we found
C*************************
      CALL PRBACK( MAPID, STATUS )

C******
C Done
C******
      RETURN

C************************************
C Formatted output FORMAT statements
C************************************
99001 FORMAT( ' >>> Optimum box size =', i4 )
99002 FORMAT( ' >>> Calculated box size =', i4 )
99003 FORMAT( ' >>> Specified box size =', i4 )

      END

