      SUBROUTINE RESOLUTION( MAPID, IXS, SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                       ITEL, DMINI, DMIN2, NPSF1ST )
C**********************************************************************
C  I  MAPID   (S)  Map id string
C  I  IXS     (I)  Excess index
C  I  SZX/Y   (I)  Size of image
C  I  ZMX/Y   (D)  Zoom of image
C  I  X/YCEN  (D)  Center of image
C  I  ITEL    (I)  Telescope index
C  O  DMINI   (R)  Min distance between distinct sources
C  O  DMIN2   (R)  dmini*1.5
C I/O NPSF1ST (L)  Set to true before contig_srch loop, so npsf
C                  knows when to do one-time lookup
C**********************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      LOGICAL NPSF1ST
      
      CHARACTER*(*) MAPID
      
      INTEGER*4 IXS
      INTEGER*4 SZX
      INTEGER*4 SZY
      INTEGER*4 ITEL

      REAL*4 DMINI
      REAL*4 DMIN2

      REAL*8 ZMX
      REAL*8 ZMY
      REAL*8 XCEN
      REAL*8 YCEN

C***************
C Include files
C***************
      INCLUDE 'backgd.inc'
      INCLUDE 'excess.inc'
      INCLUDE 'ximpsf.inc'

C*****************
C Local variables
C*****************
      LOGICAL UNKNOWN
      
      INTEGER*4 IR
      INTEGER*4 IFRA
      INTEGER*4 IFLAT
      INTEGER*4 STATUS
      
      REAL*4 LAST_RESIDUAL
      REAL*4 C
      REAL*4 AINTE
      REAL*4 SIGNAL_PIXEL
      REAL*4 BACKGROUND_PIXEL
      REAL*4 RESIDUAL_PSF
      REAL*4 XP
      REAL*4 YP
      REAL*4 SFA

C************************************
C Get the PSF at the source position
C************************************
      CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN, XP, YP, 
     &                SOURX(NBROS(IXS)), SOURY(NBROS(IXS)), 1 )

      CALL NPSF( MAPID, XP, YP, ITEL, SFA, -1.0, ' ', NPSF1ST,
     &           .FALSE., UNKNOWN, STATUS )

C************************************************
C Find minimum distance between distinct sources
C************************************************
      C     = 3.0
      IR    = 2
      IFLAT = 0
      LAST_RESIDUAL = 0.0
      IFRA  = FLOAT( BOXSIZ ) / 2.0 * ZMX / SQRT( SFA )
      AINTE = RBOX(IXS)/FRAC(MIN( IFRA, MAXPSF ))
      SIGNAL_PIXEL = AINTE
      BACKGROUND_PIXEL = BNEW / C
      DO WHILE ( SIGNAL_PIXEL.GT.BACKGROUND_PIXEL .AND.
     &           IR.LT.( MAXPSF - 1 ) )
 
         RESIDUAL_PSF = ( FRAC(IR + 1) - FRAC(IR) )
         IF ( RESIDUAL_PSF.EQ.0.0 ) THEN
            RESIDUAL_PSF = LAST_RESIDUAL
            IFLAT = IFLAT + 1
         ENDIF
         IF ( IFLAT.GE.3 .OR. FRAC(IR).GT.0.9999 ) THEN
            GOTO 100
         ENDIF
         LAST_RESIDUAL = RESIDUAL_PSF
         SIGNAL_PIXEL  = RESIDUAL_PSF * AINTE / ( 8.0 * FLOAT( IR ) )
         IR = IR + 1
      ENDDO
 
 100  DMINI = FLOAT( IR ) * SQRT( SFA ) / ZMX
      DMIN2 = DMINI * 1.5
      RETURN
      END
