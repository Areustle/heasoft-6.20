      SUBROUTINE PSF( CMDID, MAP, SZX, SZY, MAPID, STATUS )
C**********************************************************************
C Calculate the number of counts in concentric circles,
C unless BOX option is given.
C
C Produces a file contaning the total number of counts C in each box,
C the differential counts in two concentric boxes and the
C differential counts divided by the area in two concentric boxes
C and the percentage of the counts in each box obtained normalizing 
C for the total number of counts in that region.
C
C The input parameters are:
C a) the X and Y pixels position in the image,
C b) the number concentric circles or boxes over calculate the counts
C c) the background level (background/pixels)
C
C The space in arcsec between the consecutive circles or boxes is 
C determined by the image resolution. The increment is one image pixel 
C at a time.
C
C  I  cmdid  (i)  Command id
C  I  map    (r)  Image map
C  I  szx/y  (i)  Size of map
C  I  mapid  (s)  Map id string
C  O  status (i)  Error flag (0=OK)
C**********************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      CHARACTER*(*) MAPID

      INTEGER*4 CMDID
      INTEGER*4 SZX
      INTEGER*4 SZY
      INTEGER*4 STATUS
      
      REAL*4 MAP(SZX, SZY)

C***************
C Include files
C***************
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/dynmem.inc'
      INCLUDE 'backgd.inc'
      INCLUDE 'ximpsf.inc'

C******************
C Input parameters
C******************
      LOGICAL CURSOR
      LOGICAL CALFILE
      LOGICAL PRINT
      LOGICAL NOPLOT
      LOGICAL SQUARE
      LOGICAL PILEUP
      LOGICAL EXPOWGT

      character(20) XBUF
      character(20) YBUF

      CHARACTER*(MAX_FILELEN) FILEPLOT
      CHARACTER*(MAX_FILELEN) MULTPSF
      CHARACTER*(MAX_FILELEN) ENGFILE
      
      CHARACTER*(MAX_IDSTR) EXMAPID

      INTEGER*4 COLOR
      INTEGER*4 LSTYLE
      INTEGER*4 LWIDTH

      REAL*4 BACKGROUND
      REAL*4 RADIUS
      REAL*4 ENERGY

C*****************
C Local variables
C*****************
      LOGICAL ISDISPLAY
      LOGICAL ISLOADED
      LOGICAL ISRNULL
      LOGICAL ISCPMAPID
      LOGICAL DRAW
      LOGICAL FIRST
      LOGICAL UNKNOWN
      LOGICAL SKYCOR
      LOGICAL BADEXPO
      LOGICAL THERE
      LOGICAL READONLY
      LOGICAL GLOBAL
      LOGICAL OPTIMIZE
      LOGICAL PLOT
      LOGICAL HASPILEUP

      character(10) BGMAPID
      character(20) UNITSTR
      character(20) AREAUNIT
      character(80) STRING
      character(80) OBJECT
      character(80) TELSTR
      character(80) CTYPE
      character(80) COMMENT
      character(100) DS1
      character(100) DS2
      CHARACTER*(MAX_FILELEN) DEVICE
      CHARACTER*(MAX_FILELEN) TEMPLATE

      INTEGER SLEN1
      INTEGER SLEN2
      INTEGER SIGDIG

      INTEGER*4 PSFBXS
      INTEGER*4 IMAX
      INTEGER*4 IXPOS
      INTEGER*4 IYPOS
      INTEGER*4 P_HALF
      INTEGER*4 IXPO1
      INTEGER*4 IYPO1
      INTEGER*4 I
      INTEGER*4 IHOLE
      INTEGER*4 IRADIU
      INTEGER*4 IMIN
      INTEGER*4 JMIN
      INTEGER*4 JMAX
      INTEGER*4 IXPI
      INTEGER*4 IYPI
      INTEGER*4 IX
      INTEGER*4 IY
      INTEGER*4 ARGC
      INTEGER*4 IERR
      INTEGER*4 LENACT
      INTEGER*4 J
      INTEGER*4 IN
      INTEGER*4 ITEL
      INTEGER*4 DI
      INTEGER*4 ZOOMPSF
      INTEGER*4 IBBACE
      INTEGER*4 NCOLS
      INTEGER*4 NROWS

      INTEGER*4 NUPIX(0:MAXPSF)

      REAL*4 AIXP1
      REAL*4 AIYP1
      REAL*4 WGT
      REAL*4 XX
      REAL*4 YY
      REAL*4 ADIFF
      REAL*4 XPOS
      REAL*4 YPOS
      REAL*4 XPIX
      REAL*4 YPIX
      REAL*4 XPIX2
      REAL*4 YPIX2
      REAL*4 DELX
      REAL*4 DELY
      REAL*4 DEL
      REAL*4 PIXEL_SIZE
      REAL*4 RATIO
      REAL*4 AIR
      REAL*4 AREA
      REAL*4 OTOT
      REAL*4 TOT
      REAL*4 TOTCOR
      REAL*4 HOLEFRAC
      REAL*4 FRACDIF
      REAL*4 UNIT
      REAL*4 RING
      REAL*4 PI
      REAL*4 HALF_PIX
      REAL*4 XOUT
      REAL*4 YOUT
      REAL*4 SFA
      REAL*4 BGX
      REAL*4 BGY
      REAL*4 XIMG
      REAL*4 YIMG
      REAL*4 XP
      REAL*4 YP
      REAL*4 OFFSET
      REAL*4 DR

      REAL*4 OFFAXIS(MXQCOL)
      REAL*4 XPSF(0:MAXPSF)
      REAL*4 PSFDIF1(0:MAXPSF)
      REAL*4 PSFDIF2(0:MAXPSF)
      REAL*4 PSFINT(0:MAXPSF)
      REAL*4 PSFDIST(0:MAXPSF)
      REAL*4 PSFDIF2_ERR(0:MAXPSF)
      REAL*4 PSFINT_ERR(0:MAXPSF)
      REAL*4 PSFDIF1_ERR(0:MAXPSF)
      REAL*4 PSF_BACK(0:MAXPSF)
      REAL*4 PSFAREA(0:MAXPSF)
      REAL*4 XPSFINT(0:MAXPSF)
      REAL*4 XPSFDIF2(0:MAXPSF)
      REAL*4 PSFCT(0:MAXPSF)
      REAL*4 PSFCT_ERR(0:MAXPSF)
      REAL*4 BOX_STORE(0:MAXPSF)

      REAL*8 EXPOSURE
      REAL*8 EXPOMAX
      REAL*8 XCEN
      REAL*8 YCEN
      REAL*8 BGXCEN
      REAL*8 BGYCEN
      REAL*8 DD
      REAL*8 XOFF
      REAL*8 YOFF
      REAL*8 ZMX
      REAL*8 ZMY
      REAL*8 BGZMX
      REAL*8 BGZMY

      REAL*8 PIXSIZE(2)

C*************************************
C Parameters, DATA and SAVE variables
C*************************************
      PARAMETER (PI = 3.141592654, SIGDIG = 10)

      DATA XPIX/0.0/
      DATA YPIX/0.0/
      DATA XPIX2/0.0/
      DATA YPIX2/0.0/

      SAVE XPIX
      SAVE YPIX
      SAVE XPIX2
      SAVE YPIX2

C************
C Initialize
C************
      BGMAPID    = 'BGMAP'
      CURSOR     = .FALSE.
      BACKGROUND = -1.0
      XBUF       = ' '
      YBUF       = ' '
      PSFBXS     = 0
      ENERGY     = -1.0
      ENGFILE    = ' '
      FILEPLOT   = ' '
      MULTPSF    = ' '
      PRINT      = .FALSE.
      SQUARE     = .FALSE.
      CALFILE    = .FALSE.
      PILEUP     = .FALSE.
      RADIUS     = 0.0
      COLOR      = 3
      LWIDTH     = 1
      LSTYLE     = 1
      NOPLOT     = .FALSE.
      STATUS     = 0

C****************************************************************
C Check number of parameters, retrieve them and do sanity checks
C****************************************************************
      CALL NUMCARG( CMDID, ARGC, STATUS )
      IF ( ARGC.NE.0 ) THEN
         CALL WRONGARGS( CMDID, STATUS )
      ENDIF

      CALL GPARL( CMDID, 'CURSOR', CURSOR, STATUS )
      CALL GPARL( CMDID, 'CALFILEGEN', CALFILE, STATUS )
      CALL GPARL( CMDID, 'PRINT', PRINT, STATUS )
      CALL GPARS( CMDID, 'XPIX', XBUF, STATUS )
      CALL GPARS( CMDID, 'YPIX', YBUF, STATUS )
      CALL GPARR( CMDID, 'BACK', BACKGROUND, STATUS )
      CALL GPARR( CMDID, 'RADIUS', RADIUS, STATUS )
      CALL GPARR( CMDID, 'ENERGY', ENERGY, STATUS )
      CALL GPARS( CMDID, 'FILEPLOT', FILEPLOT, STATUS )
      CALL GPARS( CMDID, 'MULTPSFFILE', MULTPSF, STATUS )
      CALL GPARS( CMDID, 'ENGFILE', ENGFILE, STATUS )
      CALL GPARL( CMDID, 'NOPLOT', NOPLOT, STATUS )
      CALL GPARL( CMDID, 'BOX', SQUARE, STATUS )
      CALL GPARI( CMDID, 'COLOR', COLOR, STATUS )
      CALL GPARI( CMDID, 'LSTYLE', LSTYLE, STATUS )
      CALL GPARI( CMDID, 'LWIDTH', LWIDTH, STATUS )
      CALL GPARL( CMDID, 'PILEUP', PILEUP, STATUS )
      CALL GPARL( CMDID, 'EXPOWGT', EXPOWGT, STATUS )
      IF ( STATUS.NE.0 ) THEN
         RETURN
      ENDIF

C     Strip quotes from filenames
      CALL QUSTRIP( FILEPLOT )
      CALL QUSTRIP( MULTPSF )
      CALL QUSTRIP( ENGFILE )

C     Check if we have a map loaded - bail if not
      IF ( .NOT.ISLOADED( MAPID ) ) THEN
         CALL XWRITE( ' Image not loaded', 10 )
         STATUS = -1
         RETURN
      ENDIF

C     If doing exposure weighting, check if expomap exists
      IF ( EXPOWGT ) THEN
         EXMAPID = ' ';
         CALL GHEADS( MAPID, 'EXMAPID', EXMAPID, 0, STATUS )
         IF ( STATUS.NE.0 .OR. EXMAPID.EQ.' ' ) THEN
            CALL XWRITE( 'ERROR: Cannot exposure weight with '//
     &                   'no exposure map', 5 )
            STATUS = -1
            RETURN
         ENDIF
      ENDIF

C     Check energy file and energy params
      IF ( ENGFILE.NE.' ' .AND. ENERGY.GT.0.0 ) THEN
         CALL XWRITE( ' ENGFILE and ENERGY are incompatible', 10 )
         RETURN
      ENDIF

C     If the image is not displayed, CURSOR param must be false
      IF ( .NOT.ISDISPLAY( ) .AND. CURSOR ) THEN
         CALL XWRITE( ' No display: CURSOR ignored', 10 )
         CURSOR = .FALSE.
      ENDIF

C     Check that the displayed map is the current map
      IF ( CURSOR .AND. .NOT.ISCPMAPID( 'DIS', MAPID ) ) THEN
         CALL XWRITE( ' Current map is not display map', 10 )
         CALL MAPRCMD( 1 )
         RETURN
      ENDIF

C     Do we want to plot?
      PLOT = .NOT.NOPLOT

C************************
C Read image header info
C************************

C     Telescope
      CALL GET_ITEL( MAPID, ITEL )

C     Map zoom and center
      CALL GET_REFRAM( MAPID, DI, DI, ZMX, ZMY, XCEN, YCEN, STATUS )

C     Get offset of map center
      CALL GET_OPTAX( ITEL, XCEN, YCEN, XOFF, YOFF )

C     Read WCS keys
      CALL GHEADS( MAPID, 'CTYPE1', CTYPE, 0, STATUS )
      CALL GHEADD( MAPID, 'CDELT1', PIXSIZE(1), 0, STATUS )
      CALL GHEADD( MAPID, 'CDELT2', PIXSIZE(2), 0, STATUS )

C     Check the WCS type - if TAN coords, assume WCS is valid
C     and use sky coordinates
      I = LENACT( CTYPE )
      CALL UPC( CTYPE )
      SKYCOR = CTYPE(I-2:I).EQ.'TAN'

C     Total exposure
      BADEXPO = .FALSE.
      CALL GHEADD( MAPID, 'EXPOSURE', EXPOSURE, 0, STATUS )
      IF ( EXPOSURE.LE.0.0 ) THEN
         EXPOSURE = 1.0
         BADEXPO  = .TRUE.
      ENDIF

C********************************
C Get the background map, if any
C********************************
      CALL GETBGMAP( MAPID, BGMAP, BGSZ, STATUS )
      IF ( STATUS.NE.0 ) THEN
         RETURN
      ENDIF

C     Get zoom and center
      CALL GET_REFRAM( BGMAPID, DI, DI, BGZMX, BGZMY,
     &                 BGXCEN, BGYCEN, STATUS )

C***********************************************************
C Calculate the pixel size and check "squareness" of pixels
C***********************************************************
      PIXEL_SIZE = ABS( PIXSIZE(1) )

C     --TAN CDELT keywords are in degrees, convert to arcseconds
      IF ( SKYCOR ) THEN
         PIXEL_SIZE = PIXEL_SIZE * 3600.0
      ENDIF

C     Warn about non-square pixels
      IF ( ABS( PIXSIZE(1) ).NE.ABS( PIXSIZE(2) ) ) THEN
         WRITE( ZWRITE, * ) 'Pixels are not square, ',
     &                      'scale may be incorrect'
         CALL XWARN( ZWRITE, 10 )
      ENDIF

C***************************************************************
C Get the detector coordinates about which to calculate the PSF
C These are prompted for, or selected with the cursor if not
C already specified at the prompt
C***************************************************************
      IF ( XBUF.EQ.' ' .AND. YBUF.EQ.' ' ) THEN

C        Prompt for them or get from cursor
C        INXYPIX( ) handles conversion from frame to detector coords
         CALL XWRITE( ' Select center ', 5 )
         CALL INXYPIX( CURSOR, XPIX, YPIX )
         
      ELSE

C        Use the specified values, converted to doubles
         CALL STRNUM( XBUF, 4, DD, STATUS )
         XPIX = DD
         CALL STRNUM( YBUF, 4, DD, STATUS )
         YPIX = DD

      ENDIF

C*******************************************************************
C Calculate the distance (in detector pixels) from the optical axis
C*******************************************************************
      OFFSET = SQRT( ( XPIX - XOFF )**2 + ( YPIX - YOFF )**2 )
      DD = OFFSET

C     Tell the user a little
      CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
      WRITE( ZWRITE, '(3A)' ) ' Distance from optical axis: ',
     &                        DS1(:SLEN1), ' pixels'
      CALL XWRITE( ZWRITE, 10 )

C****************************************************************
C Check that the detector coordinates are contained in the image
C****************************************************************
      CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                XPIX, YPIX, XPOS, YPOS, 2 )
 
      IF ( XPOS.LT.1.0 .OR. XPOS.GT.SZX .OR. 
     &     YPOS.LT.1.0 .OR. YPOS.GT.SZY ) THEN
         CALL XWRITE( ' Requested coordinates are outside image', 10 )
         STATUS = 1
         RETURN
      ENDIF

C*****************************************************
C Get the radius out to which to calculate the PSF
C This is also prompted for, or taken from the cursor
C if not specified at the prompt
C*****************************************************

C     Not specified, so get the radius
      IF ( RADIUS.EQ.0.0 ) THEN

C        Get a coordinate that lies at the radius
         CALL XWRITE( ' Select outer radius', 5 )
         CALL INXYPIX( CURSOR, XPIX2, YPIX2 )

C        Calculate the radius
         CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                   XPIX2, YPIX2, XOUT, YOUT, 2 )

         DELX = ABS( XPIX2 - XPIX )
         DELY = ABS( YPIX2 - YPIX )
         DEL  = MAX( DELX, DELY )

         RADIUS = DEL * PIXEL_SIZE / ZMX

C        Tell the user what we found
         IF ( SKYCOR ) THEN
            DD = RADIUS / 60.0
            CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
            WRITE( ZWRITE, '(2A)' ) ' radius(arcmin) = ' , DS1(:SLEN1)
         ELSE
            DD = RADIUS
            CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
            WRITE( ZWRITE, '(2A)' ) ' radius(pixels) = ' , DS1(:SLEN1)
         ENDIF
         CALL XWRITE(ZWRITE, 10)

C        Check if we've gone off the map
         IX = NINT( XOUT )
         IY = NINT( YOUT )
         IF ( IX.LT.1 .OR. IX.GT.SZX .OR. 
     &        IY.LT.1 .OR. IY.GT.SZY ) THEN
            CALL XWRITE( ' Requested coordinates are outside image',
     &                   10 )
            STATUS = 1
            RETURN
         ENDIF

C     Radius specified, so just tell the user what they put in
      ELSE
         IF ( SKYCOR ) THEN
            DD = RADIUS
            CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
            WRITE( ZWRITE, '(2A)' ) ' radius(arcmin) = ', DS1(:SLEN1)
            CALL XWRITE( ZWRITE, 15 )

C           Convert to arcseconds
            RADIUS = RADIUS * 60.0

         ELSE
            CALL XWRITE(' RADIUS assumed to be in original pixels', 10)
         ENDIF
      ENDIF

C*****************************************************************
C Calculate the number of "boxes" to use -- this is the number of
C rings" in the case that the "box" parameter is not set
C*****************************************************************
      IF ( SKYCOR ) THEN
         PSFBXS   = RADIUS / PIXEL_SIZE
         UNITSTR  = 'arcsec'
         AREAUNIT = 'sq arcmin'
      ELSE
         PSFBXS   = RADIUS / ZMX
         UNITSTR  = 'pixels'
         AREAUNIT = 'pixel'
      ENDIF
      CALL XISTR( PSFBXS, DS1, SLEN1 )
      WRITE( ZWRITE, '(2A)' ) 'Number of circles or boxes ', DS1(:SLEN1)
      CALL XWRITE( ZWRITE, 15 )

C******************************************************
C Check that we don't go off the map given this radius
C******************************************************
      IMIN = XPOS - PSFBXS
      JMIN = YPOS - PSFBXS
      IMAX = XPOS + PSFBXS
      JMAX = YPOS + PSFBXS
      IF ( IMIN.LT.1 .OR. IMAX.GT.SZX .OR.
     &     JMIN.LT.1 .OR. JMAX.GT.SZY ) THEN
         CALL XWRITE( ' Requested radius outside image bounds', 10 )
         STATUS = 1
         RETURN
      ENDIF

C******************************************************************
C Get the map background. If not specified, nor already calculated
C then calculate it
C******************************************************************
      IF ( BACKGROUND.LT.0.0 ) THEN
         CALL XWRITE( ' BACK not specified', 10 )
         CALL XWRITE( ' calculating mean background from all image ',
     &                10 )

         IF ( NBOXES.GT.0 ) THEN

C           Use the existing calculated background
            CALL XWRITE( ' Using existing background calculation', 10 )
            CALL PRBACK( MAPID, STATUS )
            BACKGROUND = BNEW

         ELSE

C           Run the background calculation with default values for the
C           sigma multiplier, barycenteric distance limit, half-box 
C           minimum probability.
C
C           The box size is set to the maximum of:
C             32 pixels 
C           or
C             32 times the map size mod 256
C
C           Otherwise the background calculation might fail
C
            SIGMULT = DEFSIGM
            BARYLIM = DEFBARL
            BXHPROB = DEFHPROB
            
            IBBACE  = MIN( FLOAT( SZX ) / 256.0 * 32.0, 32.0 )

            OPTIMIZE = .FALSE.

            CALL DO_BACK( MAP, SZX, SZY, MAPID, 
     &                    OPTIMIZE, IBBACE, STATUS )
            IF ( STATUS.NE.0 ) THEN
               RETURN
            ENDIF

            BACKGROUND = BNEW

C           Reset NBOXES to zero, so that later tasks don't think the
C           background has already been calculated
            NBOXES = 0

         ENDIF
      ELSE

C        Scale the user input background by the zoom
         BACKGROUND = BACKGROUND * ( ZMX * ZMY )
      ENDIF

C********************************************************
C Warn if we have too many "boxes", and reset the number
C********************************************************
      IF ( PSFBXS.GT.MAXPSF ) THEN
         PSFBXS = MAXPSF
         WRITE( ZWRITE, 99001 ) MAXPSF
         CALL XWARN( ZWRITE, 10 )
      ENDIF

C**********************************************
C Get background map coordinates of psf center
C**********************************************
      CALL CALIMGPIX( BGSZ, BGSZ, BGZMX, BGZMY, BGXCEN, BGYCEN,
     &                XPIX, YPIX, BGX, BGY, 2 )

C*************************************************************
C Get integer truncated source and background map coordinates
C*************************************************************
      IXPO1 = BGX
      IYPO1 = BGY
      IXPOS = XPOS
      IYPOS = YPOS

C****************************
C Initialize various vectors
C****************************
      DO I = 0, MAXPSF
         XPSF(I)     = 0.0
         PSF_BACK(I) = 0.0
         NUPIX(I)    = 0.0
      ENDDO

C*********************************************
C If we are PRINTing then write a header line
C*********************************************
      IF ( PRINT ) THEN
         WRITE( ZWRITE, 99002 )
         CALL XWRITE( ZWRITE, 10 )
      ENDIF

C----------------------------------------------------------------------

C***************************************************************
C Accumulate image counts in either box annuli or circle annuli
C Either type, the annulus is 1 image pixel wide
C***************************************************************

C     Can we draw the boxes?
      DRAW = ISCPMAPID( 'DIS', MAPID ).AND.ISDISPLAY( )

C     Are we using boxes or circles?
      IF ( SQUARE ) THEN

C        Boxes.

C        Setup PGPLOT to start drawing them if possible
         IF ( DRAW ) THEN
            CALL PGSAVE
            CALL LINE_PGSTATE( COLOR, LWIDTH, LSTYLE )
         ENDIF

C        Loop on number of boxes
         DO I = 1, PSFBXS + 1

C           Get the current radius
            IRADIU = I - 1
            AIR    = ( FLOAT( IRADIU ) + 0.5 ) * ZMX

C           Draw the current box
            IF ( DRAW ) THEN
               IF ( MOD( I, 4 ).EQ.0 ) THEN
                  CALL JRNBOX( XPIX, YPIX, AIR * 2.0, AIR * 2.0,
     &                         0.0, -1 , -1, -1 )
               ENDIF
            ENDIF

C           Get the min and max map indices for this radius
            IMIN = IXPOS - IRADIU
            JMIN = IYPOS - IRADIU
            IMAX = IXPOS + IRADIU
            JMAX = IYPOS + IRADIU

C           Accumulate counts in this box
            DO IX = IMIN, IMAX
               DO IY = JMIN, JMAX
                  IF ( IX.EQ.IMIN .OR. IX.EQ.IMAX .OR.
     &                 IY.EQ.JMIN .OR. IY.EQ.JMAX ) THEN

C                    Get the source and background map coords
                     XIMG = IX
                     YIMG = IY
                     CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                               XP, YP, XIMG, YIMG, 1 )
                     CALL CALIMGPIX( BGSZ, BGSZ, BGZMX, BGZMY, 
     &                               BGXCEN, BGYCEN, XP, YP,
     &                               AIXP1, AIYP1, 2)
                     IXPI = AIXP1
                     IYPI = AIYP1

C                    If the map is valid, get the background weighted
C                    counts
                     IF ( .NOT.ISRNULL( MAP(IX,IY) ) ) THEN

C                       The background weight
                        WGT = BGMAP(IXPI, IYPI) / BGMAP(IXPO1, IYPO1)

C                       The psf
                        XPSF(I) = XPSF(I) + MAP(IX, IY)

C                       The psf background
                        PSF_BACK(I) = PSF_BACK(I) + BACKGROUND * WGT

C                       Keep running total of the number of pixels in
C                       this annulus
                        NUPIX(I) = NUPIX(I) + 1

                     ENDIF
                  ENDIF

               ENDDO
            ENDDO
C           END loops over coords
            
         ENDDO
C        END loop over boxes

C        Restore the PGPLOT settings from before this box
         IF ( DRAW ) THEN
            CALL PGUNSA
         ENDIF

C        END box accumulation

      ELSE
         
C        Circles

C        Get the min and max map indices to loop over
         IMIN = IXPOS - PSFBXS
         JMIN = IYPOS - PSFBXS
         IMAX = IXPOS + PSFBXS
         JMAX = IYPOS + PSFBXS

C        Loop through each pixel and determine which annulus it's in
         DO IX = IMIN, IMAX
            DO IY = JMIN, JMAX

C              Get the source and background map coords
               XIMG = IX
               YIMG = IY
               CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN, 
     &                         XP, YP, XIMG, YIMG, 1 )
               CALL CALIMGPIX( BGSZ, BGSZ, BGZMX, BGZMY, 
     &                         BGXCEN, BGYCEN, XP, YP, AIXP1, AIYP1, 2 )
               IXPI = AIXP1
               IYPI = AIYP1

C              Get the background weight
               WGT = BGMAP(IXPI, IYPI) / BGMAP(IXPO1, IYPO1)

C              Determine which ring this pixel belongs to
               XX = FLOAT( IX - IXPOS ) - 0.5
               YY = FLOAT( IY - IYPOS ) - 0.5
               RING = SQRT( XX**2.0 + YY**2.0 ) + 1.0
               I = RING

C              If the ring and map value are valid, accumulate counts
               IF ( I.GE.1 .AND. I.LE.MAXPSF ) THEN
                  IF ( .NOT.ISRNULL( MAP(IX, IY) ) ) THEN

C                    The psf
                     XPSF(I) = XPSF(I) + MAP(IX, IY)

C                    The psf background
                     PSF_BACK(I) = PSF_BACK(I) + BACKGROUND * WGT

C                    Keep running total of the number of pixels in
C                    this annulus
                     NUPIX(I) = NUPIX(I) + 1 
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C        END loop over pixels

C        END circle accumulation

      ENDIF

C----------------------------------------------------------------------

C******************************************
C Init FIRST for NPSF routine outside loop
C******************************************
      FIRST = .TRUE.

C********************************************************************
C Loop over number of boxes, calculating various parameters for each
C********************************************************************
      DO I = 1, PSFBXS

C        Get the area within each box/circle
C        Use the vector NUPIX(I) which contains the number of 
C        pixels in the ring (either box or circle)
         AREA = NUPIX(I)

C        Convert to sqare arc min/sec if necessary
         IF ( SKYCOR ) THEN
            AREA = AREA * PIXEL_SIZE**2.0 / 3600.0
         ENDIF

C        Get the net counts in each ring and the error
         PSFDIF1(I) = XPSF(I) - PSF_BACK(I)

C        Trap imaginaries in error calc
         IF ( PSFDIF1(I).LT.0.0 ) THEN
            PSFDIF1(I) = 0.0
         ENDIF
         PSFDIF1_ERR(I) = SQRT( XPSF(I) )

C        Calculate the EEF for this ring
         IF ( I.GT.1 ) THEN
            XPSF(I) = XPSF(I) + XPSF(I - 1)
            PSF_BACK(I) = PSF_BACK(I) + PSF_BACK(I - 1)
         ENDIF
         PSFCT_ERR(I) = SQRT( XPSF(I) )
         PSFCT(I) = XPSF(I) - PSF_BACK(I)
         IF ( PSFCT(I).LT.0.0 ) THEN
            PSFCT(I) = 0.0
         ENDIF

C        Calculate the counts per area (per second)
         PSFAREA(I)     = AREA 
         PSFDIF2(I)     = PSFDIF1(I) / ( PSFAREA(I) * EXPOSURE )
         PSFDIF2_ERR(I) = PSFDIF1_ERR(I) / ( PSFAREA(I) * EXPOSURE )

C        Print out the results if required
         IF ( PRINT ) THEN
            ADIFF = XPSF(I) 
            IF ( I.NE.1 ) THEN
               ADIFF = XPSF(I) - XPSF(I - 1)
            ENDIF
            WRITE( ZWRITE, 99003 ) I, XPSF(I), PSFCT(I),
     &                             PSFAREA(I), PSFDIF2(I)
            CALL XWRITE( ZWRITE, 10 )
         ENDIF

      ENDDO
C*********************
C END loop over boxes
C*********************

C************************************************
C Call NPSF( ) routine to calculate expected PSF
C************************************************
      CALL NPSF( MAPID, XPIX, YPIX, ITEL, SFA, ENERGY, ENGFILE,
     &           FIRST, EXPOWGT, UNKNOWN, STATUS )

C     Get the zoom of the FRAC vector
      IF ( STATUS.EQ.0 ) THEN
         ZOOMPSF = NINT( SQRT( SFA ) )
      ELSE
         ZOOMPSF = 1.0
         STATUS = 0
      ENDIF

C**************************************************
C Check that the maximum exposure contained within
C this radius is non-zero and get the max exposure
C**************************************************
      IF ( EXPOWGT .AND. EXPOMAXS(PSFBXS).LE.0.0D0 ) THEN
         CALL XERROR( 'Maximum exposure from expomap <= 0.0!', 5 )
         STATUS = 1
         RETURN
      ENDIF
      EXPOMAX = EXPOMAXS(PSFBXS)

C**************************************************************
C Initial (uncorrected) count totals, both expected and actual
C**************************************************************
      TOT  = PSFCT(PSFBXS)
      OTOT = XPSF(PSFBXS)

C********************************************************************
C  If we are correcting for source pileup, find the hole boundary
C  (include first nonzero ring/box in hole, removing it from totals)
C********************************************************************
      HOLEFRAC = 0.0
      IF ( PILEUP ) THEN
         IHOLE     = 1
         HASPILEUP = .FALSE.
         DO WHILE ( IHOLE.LT.PSFBXS .AND. XPSF(IHOLE).LE.0.0 ) 
            IHOLE     = IHOLE + 1
            HASPILEUP = .TRUE.
         ENDDO
         IF ( HASPILEUP ) THEN
            OTOT = OTOT - XPSF(IHOLE)
            TOT  = TOT  - PSFCT(IHOLE)
            I    = INT( IHOLE * ZMX ) - 1
            IF ( I.GT.( MAXPSF - 1 ) ) THEN
               I = MAXPSF - 1
            ENDIF
            IF ( EXPOWGT ) THEN
               HOLEFRAC = FRACW(I) / EXPOMAX
            ELSE
               HOLEFRAC = FRAC(I)
            ENDIF
         ENDIF
      ENDIF

C*************************************************
C Get the max box/ring radius, in detector pixels
C*************************************************
      I = INT( PSFBXS * ZMX ) - 1
      IF ( I.GT.( MAXPSF - 1 ) ) THEN
         I = MAXPSF - 1
      ENDIF

C********************************
C Get the total corrected counts
C********************************
      IF ( UNKNOWN ) THEN
         TOTCOR = TOT
      ELSE

C        Get the fraction of the PSF that we have, 
C        removing the piled-up counts
         IF ( EXPOWGT ) THEN
            FRACDIF = FRACW(I) / EXPOMAX - HOLEFRAC
         ELSE
            FRACDIF = FRAC(I) - HOLEFRAC
         ENDIF

C        Calculate the corrected counts
         IF ( FRACDIF.GT.0.0 ) THEN
            TOTCOR = TOT / FRACDIF
         ELSE
            TOTCOR = TOT
         ENDIF
      ENDIF

C*****************************
C Write out the counts values
C*****************************
      WRITE( ZWRITE, 99004 ) ' Total counts (original)      = ', OTOT
      CALL XWRITE( ZWRITE, 10 )

      WRITE( ZWRITE, 99004 ) ' Total counts (bg subtracted) = ', TOT
      CALL XWRITE( ZWRITE, 10 )

      WRITE( ZWRITE, 99004 ) ' Total counts (psf corrected) = ', TOTCOR
      CALL XWRITE( ZWRITE, 10 )

C----------------------------------------------------------------------

C***********************************************
C Calculate the predicted psf for this position
C***********************************************
      
C     Half the pixel size
      HALF_PIX = PIXEL_SIZE / 2.0

C***************************
C Loop over number of boxes
C***************************
      DO I = 1, PSFBXS

C        Box/ring radius (center) for plotting
         BOX_STORE(I) = ( FLOAT( I ) - 0.5 ) * PIXEL_SIZE

C        Corrected/normalized measured psf (and error) for this radius
         PSFINT(I)     = PSFCT(I) / TOTCOR
         PSFINT_ERR(I) = PSFCT_ERR(I) / TOTCOR

C        Get the proper index into the FRAC( ) array
C        It depends on the zoom of FRAC
         J = NINT( FLOAT( I ) * ZMX / FLOAT( ZOOMPSF ) - 1.0 )

         IF ( J.GT.( MAXPSF - 1 ) ) THEN
            J = MAXPSF - 1
         ENDIF

C        Expected psf at this radius (possibly weighted by expomap)
         IF ( EXPOWGT ) THEN
            XPSFINT(I) = FRACW(J) / EXPOMAX
         ELSE
            XPSFINT(I) = FRAC(J)
         ENDIF

C        Unit conversion factor for /area/second units
         UNIT = PSFAREA(I) * EXPOSURE

C        Expected psf in this ring only
C        FRAC( ) contains integrated PSF
         IF ( I.GT.1 ) THEN

C           Un-integrate
            XPSFDIF2(I) = ( XPSFINT(I) - XPSFINT(I - 1 ) ) * TOTCOR
            
C           Proper units
            XPSFDIF2(I) = XPSFDIF2(I) / UNIT

C           If the expected psf is zero for this ring, 
C           just make it the same as the previous ring,
C           scaled by the difference in area 
            IF ( XPSFDIF2(I).EQ.0.0 ) THEN
               RATIO       = PSFAREA(I - 1) / PSFAREA(I)
               XPSFDIF2(I) = XPSFDIF2(I - 1) * RATIO
            ENDIF
         ELSE

C           Inner-most ring needs no un-integration
            XPSFDIF2(I) = XPSFINT(I) * TOTCOR / UNIT

         ENDIF

      ENDDO
C*********************
C END loop over boxes
C*********************

C----------------------------------------------------------------------

C*****************************
C Write QDP file for plotting
C*****************************

C     Get telescope string
      CALL GET_TELSTR( MAPID, TELSTR )

C     Get object string
      CALL GHEADS( MAPID, 'OBJECT', OBJECT, 0, STATUS )

C     Lookup short header template
      TEMPLATE = 'shortid'
      CALL LKUPFILE( TEMPLATE, 'hdr', 'header template', STATUS )
      IF ( STATUS.NE.0 ) THEN
         RETURN
      ENDIF

C     Set the output filename
      IF ( FILEPLOT.NE.' ' ) THEN

C        Append a '.psf' extension
         IN = INDEX( FILEPLOT, '.' )
         IF ( IN.EQ.0 ) THEN
            CALL XTEND( FILEPLOT, 'psf' )
         ENDIF
      ELSE
         FILEPLOT = 'psf.qdp'
      ENDIF

C     Write the output file, starting with a nice comment header
      CALL TXINIT( STATUS )
      CALL TXWRCOM( FILEPLOT, '!', STATUS )
      CALL TXWRCOM( FILEPLOT, '! Ximage psf output', STATUS )
      CALL TXWRCOM( FILEPLOT, '!', STATUS )

C     Put in the map short header
      CALL TXWRHDR( FILEPLOT, MAPID, TEMPLATE, STATUS)

C     Put in the coordinates
      DD = XPIX
      CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
      DD = YPIX
      CALL XDSTR( DD, SIGDIG, DS2, SLEN2 )
      WRITE( ZWRITE, '(5A)' ) '! PSF calculated from x ', DS1(:SLEN1), 
     &                        ' and y ', DS2(:SLEN2), ' pixels'
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )

C     Put in the number of boxes/rings used
      CALL XISTR( PSFBXS, DS1, SLEN1 )
      IF ( .NOT.SQUARE ) THEN
         WRITE( ZWRITE, '(2A)' ) '! Number of concentric circles ',
     &                           DS1(:SLEN1)
      ELSE
         WRITE( ZWRITE, '(2A)' ) '! Number of concentric boxes ',
     &                           DS1(:SLEN1)
      ENDIF
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )

C     Put in information about the background
      DD = BACKGROUND
      CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
      DD = TOT
      CALL XDSTR( DD, SIGDIG, DS2, SLEN2 )
      WRITE( ZWRITE, '(4A)' ) '! Background/pixels ', DS1(:SLEN1), 
     &                        ' Total Counts ', DS2(:SLEN2)
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )

C     Put in psf correction info
      DD = TOTCOR
      CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
      WRITE( ZWRITE, '(2A)' ) '! Corrected total Counts ', DS1(:SLEN1)
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )

C     Put in energy info
      IF ( ENERGY.GT.0.0 ) THEN
         WRITE( ZWRITE, * ) '! Energy of psf ', ENERGY
         CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )
      ENDIF

C     Put in info about each column
      WRITE( ZWRITE, '(2A)' ) '! Column 1: radius of circle or box in ',
     &                        UNITSTR(:LENACT(UNITSTR))
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )
      WRITE( ZWRITE, '(2A)' ) '! Column 2: half size of annulus in ',
     &                        UNITSTR(:LENACT(UNITSTR))
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 3: Total number of Counts'//
     &              ' in circle/box', STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 4: Error on no of counts',
     &              STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 5: EEF within box/circles',
     &              STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 6: Error on EEF', STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 7: Predicted EEF for '//
     &              'nominal PSF', STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 8: Diff./area', STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 9: Error on diff/area', STATUS )
      CALL TXWRCOM( FILEPLOT, '! Column 10: Predicted diff/area for'//
     &              ' nominal psf', STATUS )

C     Put in various QDP/PLT commands
      CALL TXWRCOM( FILEPLOT, 'read serr 1 2 3 5', STATUS )
      CALL TXWRCOM( FILEPLOT, 'window 1', STATUS )
      CALL TXWRCOM( FILEPLOT, 'color off 2', STATUS )
      CALL TXWRCOM( FILEPLOT, 'yplot 3 4', STATUS )
      CALL TXWRCOM( FILEPLOT, 'line step 4', STATUS )
      CALL TXWRCOM( FILEPLOT, 'loc 0.0 0.50 1.0 0.95', STATUS )
      WRITE( ZWRITE, '(2A)' ) 'la ot ', TELSTR(:LENACT(TELSTR))
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )

      DD = XPIX
      CALL XDSTR( DD, SIGDIG, DS1, SLEN1 )
      DD = YPIX
      CALL XDSTR( DD, SIGDIG, DS2, SLEN2 )
      WRITE( ZWRITE, '(7A)' ) 'la t ', OBJECT(:LENACT(OBJECT)),
     &                        ' PSF at X=', DS1(:SLEN1),
     &                        ' Y=', DS2(:SLEN2), ' pixel'
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )
      CALL TXWRCOM( FILEPLOT, 'la y Encircled Energy Function (EEF)', 
     &              STATUS )
      CALL TXWRCOM( FILEPLOT, 'la nx off', STATUS )
      CALL TXWRCOM( FILEPLOT, 'window 2', STATUS )
      CALL TXWRCOM( FILEPLOT, 'yplot 5 6', STATUS )
      CALL TXWRCOM( FILEPLOT, 'line step 6', STATUS )
      CALL TXWRCOM( FILEPLOT, 'loc 0.0 0.05 1.0 0.50', STATUS )
      IF ( BADEXPO ) THEN
         WRITE( ZWRITE, '(3A)' ) 'la y PSF (ct/', 
     &                           AREAUNIT(:LENACT(AREAUNIT)), ')'
      ELSE
         WRITE( ZWRITE, '(3A)' ) 'la y PSF (ct/',
     &                           AREAUNIT(:LENACT(AREAUNIT)), '/s)'
      ENDIF
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )
      WRITE( ZWRITE, '(3A)' ) 'la x radius (', 
     &                        UNITSTR(:LENACT(UNITSTR)), ')'
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )
      CALL TXWRCOM( FILEPLOT, 'window all', STATUS )
      CALL TXWRCOM( FILEPLOT, 'log x on 1 2', STATUS )
      CALL TXWRCOM( FILEPLOT, 'log y on 1 2', STATUS )

C     QDP doesn't support /xtk device, use /xw
      CALL TCLRESS( 'set default(device)', DEVICE, MAX_FILELEN, STATUS )
      SLEN1 = LENACT( DEVICE )
      DS1 = DEVICE(SLEN1 - 3:SLEN1)
      CALL UPC( DS1 )
      IF ( DS1(1:4).EQ.'/XTK' ) THEN
         ZWRITE = 'dev /XW'
      ELSE
         WRITE( ZWRITE, '(2A)' ) 'dev ', DEVICE(:LENACT(DEVICE))
      ENDIF
      CALL TXWRCOM( FILEPLOT, ZWRITE, STATUS )
      
C     Write the psf info into the buffer
      CALL TXWRCOL( FILEPLOT, BOX_STORE(1), PSFBXS, STATUS )
      CALL RALLOC( 1, PSFBXS, 1, P_HALF, STATUS )
      IF ( STATUS.NE.0 ) THEN
         CALL XWRITE( ' Failed to allocate for psf writing', 10 )
         RETURN
      ENDIF
      DO I = 1, PSFBXS
         MEMR(P_HALF + I - 1) = HALF_PIX
      ENDDO
      CALL TXWRCOL( FILEPLOT, MEMR(P_HALF), PSFBXS, STATUS )
      CALL RALLOC(0, PSFBXS, 1, P_HALF, STATUS )
      CALL TXWRCOL( FILEPLOT, PSFCT(1), PSFBXS, STATUS )
      CALL TXWRCOL( FILEPLOT, PSFCT_ERR(1), PSFBXS, STATUS )
      CALL TXWRCOL( FILEPLOT, PSFINT(1), PSFBXS, STATUS )
      CALL TXWRCOL( FILEPLOT, PSFINT_ERR(1), PSFBXS, STATUS )
      CALL TXWRCOL( FILEPLOT, XPSFINT(1), PSFBXS, STATUS )
      CALL TXWRCOL( FILEPLOT, PSFDIF2(1), PSFBXS, STATUS )
      CALL TXWRCOL( FILEPLOT, PSFDIF2_ERR(1), PSFBXS, STATUS )
      CALL TXWRCOL( FILEPLOT, XPSFDIF2(1), PSFBXS, STATUS )
      CALL TXWRFILE( FILEPLOT, STATUS )

C************************
C Run QDP to do the plot
C************************
      IF ( PLOT ) THEN
         STRING = 'qdp '//FILEPLOT(:LENACT(FILEPLOT))
         CALL SPAWN( STRING, LENACT( STRING ), IERR )
      ENDIF

C**************************************
C Write an ximage psf file if asked to
C**************************************
      IF ( CALFILE ) THEN
         MULTPSF = 'ximage_psf.dat'
         CALL XWRITE( ' Calibration file: ximage_psf.dat', 10 )
      ENDIF
      
      IF ( MULTPSF.NE.' ' ) THEN

C        See if we are appending or not
         INQUIRE( FILE=MULTPSF, EXIST=THERE )

         CALL TXINIT( STATUS )

         IF ( THERE .AND. .NOT.CALFILE ) THEN
            CALL XWRITE( ' Appending columns to existing calibration'//
     &                   ' file', 10 ) 
            CALL XWRITE( MULTPSF, 15 )

C           Check that the file is a valid psf file before writing
            CALL TXRDFILE( MULTPSF, STATUS )
            CALL TXINFO( MULTPSF, NCOLS, NROWS, STATUS )
            IF ( STATUS.NE.0 ) THEN
               GOTO 700
            ENDIF

            DO I = 2, NCOLS
               CALL TXRDKEY( MULTPSF, 'OFFAXIS', I, OFFAXIS(I), STATUS )
            ENDDO
            IF ( STATUS.NE.0 ) THEN
               CALL XWRITE( ' Missing OFFAXIS keywords', 10 )
               GOTO 700
            ENDIF
            CALL TXINITC( MULTPSF, STATUS )
            CALL TXRDCOL( MULTPSF, 1, MAXPSF, 
     &                    PSFDIST(1), NROWS, STATUS )
            IF ( STATUS.NE.0 ) THEN
               GOTO 700
            ENDIF

C           Check that the zoom matches
            DR = ABS( ( ZMX - 1.0 ) * 0.5 - PSFDIST(1) )
            IF ( DR.GT.0.1 ) THEN
               CALL XWRITE( ' Zoom in file different from image', 10 )
               STATUS = -1
               GOTO 700
            ENDIF

C           Pad our results with 1's, or truncate our results
C           to match rows in existing file
            IF ( NROWS.LT.PSFBXS ) THEN
               CALL XWARN( 'Truncating PSF to match file', 10 )
            ELSEIF ( NROWS.GT.PSFBXS ) THEN
               CALL XWARN( 'Filling out PSF with 1.0 to match file',
     &                     10 )
               DO I = PSFBXS + 1, NROWS
                  PSFINT(I) = 1.0
               ENDDO
            ENDIF

         ELSE

C           Otherwise, create a new file
            CALL XWRITE( ' Writing calibration file', 10 )
            CALL XWRITE( MULTPSF, 15 )
            NROWS = PSFBXS
            DO I = 1, NROWS
               PSFDIST(I) = ZMX * ( FLOAT( I ) - 0.5 ) - 0.5
            ENDDO
            CALL TXWRCOL( MULTPSF, PSFDIST(1), NROWS, STATUS )
            NCOLS = 1
         ENDIF

C        Write comments and current column
         CALL TXWRCOM( MULTPSF, '!', STATUS )
         CALL TXWRCOM( MULTPSF, 
     &            '!  PSF calibration file for use as PSFFILE', STATUS )
         CALL TXWRCOM( MULTPSF, '!', STATUS )

C        Short map header
         TEMPLATE = 'shortid'
         CALL LKUPFILE( TEMPLATE, 'hdr', 'header template', STATUS )
         IF ( STATUS.EQ.0 ) THEN
            CALL TXWRHDR( MULTPSF, MAPID, TEMPLATE, STATUS )
         ELSE
            STATUS = 0
         ENDIF
         
         CALL TXWRCOM( MULTPSF, '!', STATUS )
         CALL TXWRCOM( MULTPSF, '!  Column 1: Distance from source'
     &                 //' center (det coords)', STATUS )
         CALL TXWRCOM( MULTPSF, '!  Columns 2+: Fractional energy', 
     &                 STATUS )
         CALL TXWRCOM( MULTPSF, '!', STATUS )
         CALL TXWRIKEY( MULTPSF, 'UNITTYPE', 0, 1, 
     &                  'Unit for OFFAXIS (1=pix, 2=arcmin)', STATUS )

C        Check if we've too much to write
         IF ( ( NCOLS + 1 ).GT.MXQCOL ) THEN
            CALL XWRITE( ' Offaxis buffer exceeded, too many columns',
     &                   10 )
            STATUS = -1
            GOTO 700
         ENDIF

C        Write the OFFAXIS angle for each column
         OFFAXIS(NCOLS + 1) = OFFSET
         DO I = 2, NCOLS + 1
            WRITE( COMMENT, * ) 'Offaxis distance for column ', i
            CALL RMVXBK( COMMENT )
            CALL TXWRKEY( MULTPSF, 'OFFAXIS', I, OFFAXIS(I), COMMENT,
     &                    STATUS )
         ENDDO

         CALL TXWRCOM( MULTPSF, '!', STATUS )

C        Write the psf data
         CALL TXWRCOL( MULTPSF, PSFINT(1), NROWS, STATUS )
         CALL TXWRFILE( MULTPSF, STATUS )

C        Jump point for various errors
  700    CONTINUE

         IF ( STATUS.NE.0 ) THEN
            CALL XWRITE( ' Failed to write PSF file', 10 )
         ENDIF
      ENDIF

C***************************
C Export psf results to Tcl
C***************************
      READONLY = .FALSE.
      GLOBAL   = .FALSE.
      CALL TCLVARR( 'psf(cnt)', OTOT, READONLY, GLOBAL, STATUS )
      CALL TCLVARR( 'psf(cntbg)', TOT, READONLY, GLOBAL, STATUS )
      CALL TCLVARR( 'psf(cntpsf)', TOTCOR, READONLY, GLOBAL, STATUS )

C************
C We're done
C************
      RETURN

C********************************************
C Various FORMAT strings for pretty printing
C********************************************

C Resetting of PSFBXS
99001 FORMAT( ' Number of psf boxes reset to allowed max of', I5 )

C IF ( PRINT ) header line
99002 FORMAT( 6X, 'Total Cnts', 2X, 'Corr. Cnts', 5X, 'Area', 2X,
     &        ' Difference Counts/area ' )

C IF ( PRINT ) format string
99003 FORMAT( 1X, I4, 4(1X, G12.5) )

C Counts values format string
99004 FORMAT( A, F11.1 )

      END
