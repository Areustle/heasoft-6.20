      SUBROUTINE NPSF( MAPID, XPIX, YPIX, ITEL, SFA, ENERGYIN,
     &                 ENGINFILE, FIRST, DOEXPWGT, UNKNOWN, STATUS )

      IMPLICIT NONE
C******************************************************************
C This subroutine returns (in a common block) the appropriate
C point spread function for a given position in the FOV
C
C Outputs psf via FRAC/FRACW in COMMON block in ximpsf.inc
C
C FRAC is the fractional encircled energy fraction as a function
C of distance (in detector pixels) from the point (XPIX, YPIX).
C
C FRACW is the exposure weighted version
C
C  Parameters:
C
C  I  MAPID     (S)  Map ID string
C  I  X/YPIX    (R)  Source position in units of elemental pixels
C  I  ITEL      (I)  Telescope ID
C  O  SFA       (R)  ?Area
C  I  ENERGYIN  (R)  Input Energy value
C  I  ENGINFILE (S)  Input Energy file
C I/O FIRST     (L)  Should be true on first call, 
C                    set to false for subsequent calls
C  I  DOEXPWGT  (L)  Should exposure map weighting be applied?
C I/O UNKNOWN   (L)  Whether using unknown psf
C  O  STATUS    (I)  Error flag (0=OK)
C
C******************************************************************

C*****************
C Input arguments
C*****************
      LOGICAL FIRST
      LOGICAL DOEXPWGT
      LOGICAL UNKNOWN

      CHARACTER*(*) MAPID
      CHARACTER*(*) ENGINFILE

      INTEGER*4 ITEL
      INTEGER*4 STATUS

      REAL*4 XPIX
      REAL*4 YPIX
      REAL*4 SFA
      REAL*4 ENERGYIN

C***************
C Include files
C***************
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/dynmem.inc'
      INCLUDE '../include/pi.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE '../include/sitedef.inc'
      INCLUDE '../include/io.inc'
      INCLUDE 'ximpsf.inc'

C*****************
C Local variables
C*****************
      LOGICAL THERE
      LOGICAL WGTCALCD
      LOGICAL INTEGRATE

      character(10) ENGSRC
      character(80) TELESCOP
      character(80) INSTRUME
      character(80) DETNAM

      INTEGER EMIN
      INTEGER EMAX
      INTEGER CHSTAT
      INTEGER JETXMAX
      INTEGER*4 LENACT
      INTEGER*4 I
      INTEGER*4 J
      INTEGER*4 ISIS

C     psf calculation vars
      REAL*4 DDEL
      REAL*4 TMPOFF
      REAL*4 TMPPSF
      REAL*4 TMPRAD
      REAL*4 TMPFRAC(0:MAXPSF)
      REAL*4 TMPFRACW(0:MAXPSF)
      REAL*4 OFFAXIS
      REAL*4 RAD
      REAL*4 TPI
      REAL*4 TPIX

      REAL*8 EXPOWGT

C     ASCA SIS psf vars/functions
      REAL*4 ASCA_EEF
      REAL*4 DIAM
      REAL*4 AZIM

C     ROSAT HRI psf
      REAL*4 HRIPSF

C     ROSAT PSPC psf
      REAL*4 PSFOFF

C     Beppo-SAX psf
      REAL*4 SAX_PSF

C     JET-X psf
      REAL*4 JETX_PSF

C     XMM psf (PN, MOS1, MOS2)
      REAL*4 XMM_PSF

C     Swift XRT psf
      REAL*4 SWIFT_PSF

C     Suzaku XIS psf and normalization
      REAL*4 SUZAKU_PSF
      REAL*4 SUZAKU_NORM

C     Normalization vars
      REAL*4 NORMFACT
      REAL*4 TMPNORM

C     Dummy vars
      INTEGER*4 DI
      REAL*8 DD

C*************************
C For variable energy psf
C*************************
      INTEGER MAXCHAN
      PARAMETER (MAXCHAN=2048)
      
      INTEGER*4 NPHA
      INTEGER*4 NBIN
      INTEGER*4 BINNING
      INTEGER*4 BEGCHAN
      INTEGER*4 ENDCHAN
      INTEGER*4 NCOLS
      INTEGER*4 NROWS

      REAL*4 ENG(MAXCHAN)
      REAL*4 PHA(MAXCHAN)
      REAL*4 ESTEP
      REAL*4 EOFFSET
      REAL*4 ENERGY

C*********************
C Saved between calls
C*********************
      LOGICAL PSFCALC
      
      CHARACTER*(MAX_FILELEN) ENGFILE
      CHARACTER*(MAX_FILELEN) PSFFILE
      CHARACTER*(MAX_IDSTR) EXMAPID

      INTEGER*4 SAV_ITEL

      INTEGER EXSZX
      INTEGER EXSZY
      INTEGER EXMAPPTR

      REAL*8 XOFF
      REAL*8 YOFF
      REAL*8 XCEN
      REAL*8 YCEN
      REAL*8 PIXSIZE

C*******************
C DATA declarations
C*******************
      DATA SAV_ITEL/ -999 /

C***********
C Save them
C***********
      SAVE SAV_ITEL
      SAVE XOFF
      SAVE YOFF
      SAVE PIXSIZE
      SAVE ENERGY
      SAVE PSFCALC
      SAVE ENGFILE
      SAVE PSFFILE
      SAVE EXMAPID
      SAVE EXMAPPTR
      SAVE EXSZX
      SAVE EXSZY

C*****************
C Initializations
C*****************
      THERE = .FALSE.

      INTEGRATE = .TRUE.

C     2*pi
      TPI = 2.0 * PI

C     Initialize the exposure weight to 1.0 (no weight)
      EXPOWGT = 1.0

C********************************************
C Get telescope ID strings from MAPID number
C********************************************
      IF ( FIRST ) THEN
         UNKNOWN = .FALSE.
      ENDIF

      IF ( ITEL.GT.0 ) THEN
         TELESCOP = ZTELESCOP(ITEL)
         INSTRUME = ZINSTRUME(ITEL)
         DETNAM   = ZDETNAM(ITEL)
          
         CALL LOCASE( TELESCOP )
         CALL LOCASE( INSTRUME )
         CALL LOCASE( DETNAM )
      ELSE
         IF ( FIRST ) THEN
            UNKNOWN = .TRUE.
         ENDIF
          
         XOFF = XPIX
         YOFF = YPIX
         TELESCOP = 'unknown'
         INSTRUME = 'unknown'
         DETNAM   = ' '
      ENDIF

C******************************************************
C Look up header/mdb information for the mission
C If we've already done it then don't look it up again
C******************************************************
      IF ( FIRST ) THEN
         SAV_ITEL = -999
      ENDIF

      IF ( SAV_ITEL.NE.ITEL ) THEN
         PSFCALC = .FALSE.
         ENERGY  = ENERGYIN
         CALL GET_REFRAM( MAPID, DI, DI, DD, DD, XCEN, YCEN, STATUS )
         CALL GET_OPTAX( ITEL, XCEN, YCEN, XOFF, YOFF )
         CALL GHEADI( MAPID, 'EMIN', EMIN, 0, STATUS )
         CALL GHEADI( MAPID, 'EMAX', EMAX, 0, STATUS )

C        Get PIXSIZE in arcsec
         CALL GMDBD( ITEL, 'PIXSIZE', PIXSIZE, 0, STATUS )
         IF ( PIXSIZE.EQ.0.0 ) THEN
            PIXSIZE = 1.0
         ENDIF

C        Get the PSF file and energy file
         CALL GMDBS( ITEL, 'PSFFILE', PSFFILE, 0, STATUS )

         IF ( ENGINFILE.EQ.' ' .AND. ENERGYIN.LT.0.0 ) THEN
            CALL GMDBS( ITEL, 'ENGFILE', ENGFILE, 0, STATUS )
            ENGSRC = 'mdb'
         ELSE
            ENGFILE = ENGINFILE
            ENGSRC  = 'input'
         ENDIF

C*****************************
C Check for psf file from MDB
C*****************************
         IF ( PSFFILE.NE.' ' ) THEN
            INQUIRE( FILE=PSFFILE, EXIST=THERE )
            IF ( THERE ) THEN

C              Override psf calculation with MDB file
               WRITE( ZWRITE, * ) ' Using psf file from mdb: ',
     &                            PSFFILE(:LENACT(PSFFILE))
               CALL XWRITE( ZWRITE, 15 )

               ENGFILE = ' '
            ELSE

C              No PSF file found in MDB 
               WRITE( ZWRITE, * ) ' Psf file from mdb not found: ',
     &                            PSFFILE(:LENACT(PSFFILE))
               CALL XWRITE( ZWRITE, 10 )
               PSFFILE = ' '
            ENDIF
         ENDIF

C*****************************************
C Find energy file (PI distribution file)
C from the MDB or input as ENGINFILE
C*****************************************
         IF ( ENGFILE.NE.' ' ) THEN
            INQUIRE( FILE=ENGFILE, EXIST=THERE )
            IF ( THERE ) THEN
               WRITE( ZWRITE, * ) ' Using energy file from ', ENGSRC,
     &                            ': ', ENGFILE(:LENACT(ENGFILE))
               CALL XWRITE( ZWRITE, 15 )
            ELSE
               WRITE( ZWRITE, * ) ' Energy file from ', ENGSRC,
     &                            ' not found: ',
     &                            ENGFILE(:LENACT(ENGFILE))
               CALL XWRITE( ZWRITE, 10 )
               ENGFILE = ' '
            ENDIF
         ENDIF

C*********************************************************************
C If no energy file in MDB or input as ENGINFILE and no psf file in
C the MDB look in calibration directory under mission for energy file
C*********************************************************************
         IF ( ENGFILE.EQ.' ' .AND. ENERGY.LT.0.0 .AND.
     &        PSFFILE.EQ.' ' ) THEN
            CALL LKUPCAL( ITEL, 'eng_*.psf', ENGFILE, STATUS )
            IF ( STATUS.NE.0 ) THEN
               CALL LKUPCAL( ITEL, 'eng_*.dat', ENGFILE, STATUS )
            ENDIF
            IF ( STATUS.EQ.0 ) THEN
               WRITE( ZWRITE, * ) ' Using energy file from cal : ',
     &                             ENGFILE(:LENACT(ENGFILE))
               CALL XWRITE( ZWRITE, 15 )
            ELSE 
               STATUS = 0
            ENDIF
         ENDIF

C*************************************
C Get the exposure map ID if required
C*************************************
         EXMAPID = ' ';
         IF ( DOEXPWGT ) THEN
            CALL GHEADS( MAPID, 'EXMAPID', EXMAPID, 0, STATUS )
            IF ( STATUS.NE.0 .OR. EXMAPID.EQ.' ' ) THEN
               CALL XWARN( 'No exposure weighting possible', 5 )
               EXMAPID = ' '
            ELSE
               CALL GHEADI( EXMAPID, 'MAPPTR', EXMAPPTR, 0, STATUS )
               CALL GHEADI( EXMAPID, 'SZX', EXSZX, 0, STATUS )
               CALL GHEADI( EXMAPID, 'SZY', EXSZY, 0, STATUS )
            ENDIF
         ENDIF

      ENDIF
C********************************
C End of IF ( SAV_ITEL.NE.ITEL ) 
C********************************

C************************************
C Calculate off-axis angle in arcmin
C************************************
      OFFAXIS = SQRT( ( XPIX - XOFF )**2.0 + 
     &                ( YPIX - YOFF )**2.0 )
     &          * PIXSIZE / 60.0

C*****************************************************************
C PSF function
C
C If no psf file in MDB, assume a function to calculate it exists
C If the function is not found, the calibration directory is 
C searched later
C*****************************************************************
      IF ( PSFFILE.EQ.' ' ) THEN
         PSFCALC = .TRUE.
      ENDIF
      
      IF ( ENGFILE.NE.' ' .OR. ENERGY.GE.0.0 .OR. PSFCALC ) THEN

C*******************************
C Read energy distribution file
C*******************************
         IF ( ENGFILE.NE.' ' ) THEN
            IF ( FIRST ) THEN
               CALL TXINIT( STATUS )
            ENDIF
            CALL TXRDFILE( ENGFILE, STATUS )
            CALL TXRDIKEY( ENGFILE, 'BINNING', 0, BINNING, STATUS )
            CALL TXRDKEY( ENGFILE, 'EOFFSET', 0, EOFFSET, STATUS )
            CALL TXRDKEY( ENGFILE, 'ESTEP', 0, ESTEP, STATUS )

            CHSTAT = 0
            CALL TXRDIKEY( ENGFILE, 'BEGCHAN', 0, BEGCHAN, CHSTAT )
            CALL TXRDIKEY( ENGFILE, 'ENDCHAN', 0, ENDCHAN, CHSTAT )
            IF ( CHSTAT.NE.0 ) THEN
               BEGCHAN = EMIN
               ENDCHAN = EMAX
            ENDIF
            CALL TXINFO( ENGFILE, NCOLS, NROWS, STATUS )

C           Check for problems
            IF ( STATUS.NE.0 ) THEN
               WRITE( ZWRITE, * ) ' Energy file incomplete: ',
     &                            ENGFILE(:LENACT(ENGFILE))
               CALL XWRITE( ZWRITE, 10 )
               ENGFILE = ' '
               PSFCALC = .FALSE.
               GOTO 400
            ENDIF

C           Get the energy distribution
            IF ( NCOLS.GT.0 ) THEN
               CALL TXRDCOL( ENGFILE, 1, MAXCHAN, PHA, NPHA, STATUS )
            ELSE
               IF ( FIRST ) THEN
                  ENERGY = EOFFSET
                  ENGFILE = ' '
                  WRITE( ZWRITE, * ) ' Using average energy for PSF: ',
     &                               EOFFSET
                  CALL XWRITE( ZWRITE, 10 )
               ENDIF
               NPHA = 1
            ENDIF
              
C*******************************************************************
C Translate channels into energies based on EOFFSET, ESTEP keywords
C*******************************************************************
            DO I = 1, NPHA
               ENG(I) = ESTEP * FLOAT( I - 1 ) + EOFFSET
            ENDDO
              
C**********************************************************
C Bin channels based on BINNING, BEGCHAN, ENDCHAN keywords
C**********************************************************
            I = MAX( EMIN, BEGCHAN )
            J = MIN( EMAX, ENDCHAN )
            IF ( J.LT.I ) THEN
               I = 1
               J = 1
            ENDIF
            CALL BINCHAN( I, J, BINNING, NPHA, ENG, PHA, NBIN )
         ELSE
              
C*********************************************************************
C If no energy file, use input energy (ENERGYIN)
C If energy not given (i.e. <= 0) then psf function probably isn't 
C energy-dependent, otherwise an energy file should have been present
C*********************************************************************
            IF ( FIRST .AND. ENERGY.GT.0.0 ) THEN
               WRITE( ZWRITE, * ) ' Using average energy for PSF: ',
     &                            ENERGY
               CALL XWRITE( ZWRITE, 10 )
            ENDIF
            NBIN = 1
            PHA(NBIN) = 1.0
            ENG(NBIN) = ENERGY
         ENDIF
          
C*****************#*********************************************
C Init FRAC arrays (the main product of this routine), an array
C containing fractional encircled energies for each pixel away 
C from the off axis location
C******************#********************************************
         TPIX = TPI * PIXSIZE
         DO I = 0, MAXPSF
            FRAC(I)  = 0.0
            FRACW(I) = 0.0
         ENDDO

C****************************************************
C Call the exposure weighting function here, so that 
C weighting can be used in the case that we don't have 
C an analytical psf function
C
C This will calculate the PSF weights only once, and
C save them for later calls
C****************************************************
         WGTCALCD = .FALSE.

         IF ( DOEXPWGT .AND. EXMAPID.NE.' ') THEN

C           Call the weight function
            CALL GET_EXPOWGT( MEMR(EXMAPPTR), EXSZX, EXSZY, 
     &                        EXMAPID, XPIX, YPIX, 0,
     &                        EXPOWGT, WGTCALCD, STATUS )

            IF ( STATUS.NE.0 ) THEN
               CALL XWRITE( ' Failed to get exposure weight', 10 )
               GOTO 400
            ENDIF
         ENDIF
          
C*********************
C Loop on energy bins
C*********************
         DO I = 1, NBIN

C           Initialize EXPOWGT just in case
            EXPOWGT = 1.0
         
C*************************************************************
C Calculate normalization factor so that Integral 2*PI*r*dr*f
C from 0 to infinity = 1 
C*************************************************************
            IF ( TELESCOP.EQ.'jetx' ) THEN
               NORMFACT = 0.0
               TMPNORM  = 0.0
               JETXMAX  = 150
               DO J = 1, JETXMAX
                  TMPNORM  = JETX_PSF( ENG(I), OFFAXIS, 
     &                                 FLOAT( J ), STATUS )
                  TMPNORM  = TMPNORM * TPIX * FLOAT( J ) * PIXSIZE
                  NORMFACT = NORMFACT + TMPNORM
               ENDDO
            ELSE IF ( TELESCOP.EQ.'suzaku' ) THEN
               NORMFACT = SUZAKU_NORM( INSTRUME, STATUS )
            ELSE
               NORMFACT = 1.0
            ENDIF

            IF ( STATUS.NE.0 ) THEN
               CALL XWRITE( ' Failed to calculate normalization', 10 )
               PSFCALC = .FALSE.
               GOTO 400
            ENDIF

C***************
C Calculate PSF
C***************
            ISIS = 0
            DDEL = 0.0
            DIAM = 0.0
            DO WHILE ( ISIS.LT.MAXPSF ) 

C              Get the current radius in arcsec
               RAD = ( FLOAT( ISIS ) + 0.5 ) * PIXSIZE

C**********************************************************************
C Mission-specific PSF functions:
C   * Energy units of eng (from engfile) should be same as in function
C   * Offaxis is in arcmin, convert if function requires different
C   * For exposure weighting to work, the functions should output
C     the fraction of source counts contributed by a 1-pixel wide
C     ring. If it returns instead the fraction inside the given radius,
C     this routine should re-create the ring by subtracting the
C     integrated psf from the previous radius.
C**********************************************************************

C*********************************************
C EXOSAT - calculate expected PSF all at once
C          No exposure map weighting possible
C          (does EXOSAT have exposure maps?)
C*********************************************
               IF ( TELESCOP.EQ.'exosat' ) THEN
                  TMPOFF = OFFAXIS / ( PIXSIZE / 60.0 )
                  CALL PEPSI( TMPOFF, MAXPSF, TMPFRAC )
                  ISIS = MAXPSF + 1
                  IF ( DOEXPWGT .AND. EXMAPID.NE.' ' ) THEN
                     CALL XWARN( ' Cannot do exposure map wieghting'//
     &                           ' for EXOSAT', 10 );
                     EXMAPID = ' '
                  ENDIF
                  GOTO 100

C******************************************************************
C ASCA SIS - ASCA_EEF expects diameter not radius (in millimeters)
C******************************************************************
               ELSEIF ( TELESCOP.EQ.'asca' .AND. 
     &                  INSTRUME(1:3).EQ.'sis' ) THEN

C                 Stop once we're outside of 12 
C                 millimeters in diameter
                  IF ( DIAM.GT.12.0 ) THEN
                     GOTO 100
                  ENDIF
                  AZIM = 0

C                 Convert radius (arcseconds) to
C                 diameter (millimeters)
                  DIAM = 0.9823 * 2.0 * RAD / 60.0

C                 ASCA_EEF returns integrated psf,
C                 so un-integrate it
                  TMPPSF = ASCA_EEF( ENG(I), DIAM, OFFAXIS, AZIM )
     &                     - DDEL

C                 Set the integrate flag to .FALSE., since the area of
C                 this ring is already accounted for
                  INTEGRATE = .FALSE.

C************
C ROSAT PSPC 
C************
               ELSEIF ( TELESCOP(1:5).EQ.'rosat' .AND.
     &                  INSTRUME(1:4).EQ.'pspc' ) THEN

C                 Stop once we've integrated > 98%
                  IF ( DDEL.GE.0.981 ) THEN
                     GOTO 100
                  ENDIF
                  TMPPSF = PSFOFF( ENG(I), OFFAXIS, RAD, STATUS )
                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' ROSAT PSF failed', 10 )
                     PSFCALC = .FALSE.
                     GOTO 400
                  ENDIF

C                 Do the integral for this one
                  INTEGRATE = .TRUE.

C***********
C ROSAT HRI 
C***********
              
               ELSEIF ( TELESCOP(1:5).EQ.'rosat' .AND.
     &                  INSTRUME(1:3).EQ.'hri' ) THEN

C                 Stop once we've integrated > 98%
                  IF ( DDEL.GE.0.981 ) THEN
                     GOTO 100
                  ENDIF
                  TMPOFF = 0.0
                  TMPPSF = HRIPSF( TMPOFF, RAD )

C                 Do the integral for this one
                  INTEGRATE = .TRUE.

C********************************************************
C JETX - This mission was never launched (it may yet be)
C********************************************************
               ELSEIF ( TELESCOP.EQ.'jetx' ) THEN

C                 Stop if the energy is below 0.01 keV
                  IF ( ENG(I).LE.0.01 ) THEN
                     GOTO 200
                  ENDIF

C                 Stop once we've integrated > 98%
                  IF ( DDEL.GE.0.981 ) THEN
                     GOTO 100
                  ENDIF

C                 Do the calculation using pixels
                  TMPRAD = RAD / PIXSIZE
                  TMPPSF = JETX_PSF( ENG(I), OFFAXIS, TMPRAD, STATUS )
                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' JETX PSF failed', 10 )
                     PSFCALC = .FALSE.
                     GOTO 400
                  ENDIF

C                 Do the integral for this one
                  INTEGRATE = .TRUE.

C***************
C BeppoSAX MECS 
C***************
               ELSEIF ( TELESCOP.EQ.'sax' .AND.
     &                  INSTRUME.EQ.'mecs' ) THEN
                   
C                 Stop if the energy is below 0.01 keV
                  IF ( ENG(I).LE.0.01 ) THEN
                     GOTO 200
                  ENDIF

C                 Stop once we've integrated > 98%
                  IF ( DDEL.GE.0.981 ) THEN
                     GOTO 100
                  ENDIF

C                 Do the calculation using pixels
                  TMPRAD = RAD / PIXSIZE
                  TMPPSF = SAX_PSF( ENG(I), OFFAXIS, TMPRAD, STATUS )
                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' SAX PSF failed', 10 )
                     PSFCALC = .FALSE.
                     GOTO 400
                  ENDIF

C                 Do the integral for this one
                  INTEGRATE = .TRUE.

C*****
C XMM 
C*****
               ELSEIF ( TELESCOP.EQ.'xmm' ) THEN

C                 Stop once we've integrated > 98%
                  IF ( DDEL.GE.0.981 ) THEN
                     GOTO 100
                  ENDIF

C                 Do the proper instrument
                  IF ( INSTRUME.EQ.'emos1' ) THEN
                     TMPPSF = XMM_PSF( 1, ENG(I), OFFAXIS,
     &                                 RAD, STATUS )
                  ELSE IF ( INSTRUME.EQ.'emos2' ) THEN
                     TMPPSF = XMM_PSF( 2, ENG(I), OFFAXIS,
     &                                 RAD, STATUS )
                  ELSE IF ( INSTRUME.EQ.'epn' ) THEN
                     TMPPSF = XMM_PSF( 0, ENG(I), OFFAXIS,
     &                                 RAD, STATUS )
                  ELSE
                     IF ( FIRST .AND. ISIS.EQ.0 ) THEN
                        CALL XWARN( ' Undefined XMM instrument,'//
     &                              ' using EMOS1 PSF', 10 )
                     ENDIF
                     TMPPSF = XMM_PSF( 1, ENG(I), OFFAXIS,
     &                                 RAD, STATUS )
                  ENDIF

C                 XMM_PSF returns the integrated psf
C                 so un-integrate it
                  TMPPSF = TMPPSF - DDEL

C                 Set the integrate flag to .FALSE., since the area of
C                 this ring is already accounted for
                  INTEGRATE = .FALSE.

                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' XMM PSF failed', 10 )
                     PSFCALC = .FALSE.
                     GOTO 400
                  ENDIF

C***********
C Swift XRT 
C***********
               ELSEIF ( TELESCOP.EQ.'swift' .AND.
     &                  INSTRUME(1:3).EQ.'xrt' ) THEN

C                 Stop once we've integrated > 98%
                  IF ( DDEL.GE.0.981 ) THEN
                     GOTO 100
                  ENDIF

C                 SWIFT_PSF returns an integrated psf
C                 so un-integrate it
                  TMPPSF = SWIFT_PSF( ENG(I), OFFAXIS, RAD, STATUS )
     &                     - DDEL

C                 Set the integrate flag to .FALSE., since the area of
C                 this ring is already accounted for
                  INTEGRATE = .FALSE.

                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' SWIFT XRT PSF failed', 10 )
                     PSFCALC = .FALSE.
                     GOTO 400
                  ENDIF

C************
C Suzaku XIS 
C************
               ELSEIF ( TELESCOP.EQ.'suzaku' .AND.
     &                  INSTRUME(1:3).EQ.'xis' ) THEN

C                 Stop once we've integrated > 98%
                  IF ( DDEL.GE.0.981 ) THEN
                     GOTO 100
                  ENDIF
                  TMPPSF = SUZAKU_PSF( INSTRUME, OFFAXIS, RAD, STATUS )

                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' SUZAKU XRT PSF failed', 10 )
                     PSFCALC = .FALSE.
                     GOTO 400
                  ENDIF

C                 Do the integral for this one
                  INTEGRATE = .TRUE.

C*************************************
C No psf function for this mission
C A psf file will be looked for later
C*************************************
               ELSE
                  CALL XWRITE( ' No PSF function defined '//
     &                         'for mission', 5 )
                  ENGFILE = ' '
                  ENERGY  = -1.0
                  PSFCALC = .FALSE.
                  GOTO 400
               ENDIF

C*****************************************
C Get the exposure weight for this radius 
C*****************************************
               IF ( DOEXPWGT .AND. EXMAPID.NE.' ') THEN

C                 Call the weight function
                  CALL GET_EXPOWGT( MEMR(EXMAPPTR), EXSZX, EXSZY, 
     &                              EXMAPID, XPIX, YPIX, ISIS,
     &                              EXPOWGT, WGTCALCD, STATUS )

                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' Failed to get exposure weight', 10 )
                     GOTO 400
                  ENDIF

               ELSE

C                 Use 1.0 as the "weight" if not weighting
                  EXPOWGT = 1.0
               ENDIF

C***************************************
C Integrate the PSF as we move outward,
C applying exposure weighting as we go
C***************************************
               IF ( ISIS.EQ.0 ) THEN

C                 Do the integral if needed
                  IF ( INTEGRATE ) THEN

C                    Save the current (unweighted) integral 
C                    for the next iteration
                     DDEL = TMPPSF * TPIX * RAD / NORMFACT

                  ELSE

C                    Save the current (unweighted) integral 
C                    for the next iteration
                     DDEL = TMPPSF / NORMFACT

                  ENDIF

C                 Accumulate the unweighted integral
                  TMPFRAC(ISIS) = DDEL

C                 Apply the weight to FRACW
                  TMPFRACW(ISIS) = DDEL * EXPOWGT

               ELSE

C                 Do the integral if needed
                  IF ( INTEGRATE ) THEN

C                    Save the current (unweighted) integral 
C                    for the next iteration
                     DDEL = DDEL + TMPPSF * TPIX * RAD / NORMFACT

C                    Accumulate the unweighted integral
                     TMPFRAC(ISIS) = TMPFRAC(ISIS - 1)
     &                               + TMPPSF * TPIX * RAD / NORMFACT

C                    Apply the weight to get the weighted fraction
                     TMPFRACW(ISIS) = TMPFRACW(ISIS - 1)
     &                                + TMPPSF * TPIX * RAD
     &                                * EXPOWGT / NORMFACT
                  ELSE

C                    Save the current (unweighted) integral 
C                    for the next iteration
                     DDEL = DDEL + TMPPSF / NORMFACT

C                    Accumulate the unweighted integral
                     TMPFRAC(ISIS) = TMPFRAC(ISIS - 1) 
     &                               + TMPPSF / NORMFACT

C                    Apply the weight to get the weighted fraction
                     TMPFRACW(ISIS) = TMPFRACW(ISIS - 1) 
     &                                + TMPPSF * EXPOWGT / NORMFACT
                  ENDIF
               ENDIF

C              Move out one pixel
               ISIS = ISIS + 1

C*********************************
C End DO WHILE ( ISIS.LT.MAXPSF ) 
C*********************************
            ENDDO

C*******************************
C Early exit point for PSF loop
C*******************************
 100        CONTINUE

C************************************************************
C Fill remaining radii with value for last calculated radius
C Weight by the exposure map if required
C************************************************************
            DO J = ISIS, MAXPSF

               TMPFRAC(J)  = TMPFRAC(J - 1)
               TMPFRACW(J) = TMPFRACW(J - 1)

               IF ( DOEXPWGT .AND. EXMAPID.NE.' ' ) THEN

C                 Re-weight this radius
                  IF ( EXPOWGT.NE.0.0 ) THEN
                     TMPFRACW(J) = TMPFRACW(J) / EXPOWGT
                  ENDIF

C                 Call the weight function
                  CALL GET_EXPOWGT( MEMR(EXMAPPTR), EXSZX, EXSZY, 
     &                              EXMAPID, XPIX, YPIX, J,
     &                              EXPOWGT, WGTCALCD, STATUS )

                  IF ( STATUS.NE.0 ) THEN
                     CALL XWRITE( ' Failed to get exposure weight', 10 )
                     GOTO 400
                  ENDIF
                      
C                 Apply the weight
                  TMPFRACW(J) = TMPFRACW(J) * EXPOWGT

               ENDIF
            ENDDO

C********************************************************************
C  Scale contribution of calculated psf based on energy distribution
C  before adding to final psf
C********************************************************************
            DO J = 0, MAXPSF
               FRAC(J)  = FRAC(J)  + TMPFRAC(J)  * PHA(I)
               FRACW(J) = FRACW(J) + TMPFRACW(J) * PHA(I)

C              Debugging output
               WRITE( ZWRITE, * ) ' FRAC(', J, ') = ', FRAC(J), ' ',
     &                            ' FRACW(', J, ') = ', FRACW(J)
               CALL XWRITE( ZWRITE, 50 )
            ENDDO

C*************************
C End Loop on energy bins
C*************************
         ENDDO

C*******************************************
C Early exit point from loop on energy bins
C*******************************************
 200     CONTINUE

         SFA = 1.0

C********************
C End IF ( PSFCALC )
C********************
      ENDIF

C*********************************
C Jump point for various failures
C*********************************
 400  CONTINUE
      
      STATUS = 0

      IF ( FIRST .AND. PSFCALC ) THEN
         CALL XWRITE( ' Internal PSF function', 15 )
      ENDIF

C********************************************************************
C If no internal PSF function look in calibration directory for file
C Weighting is still applied if requested
C********************************************************************
      IF ( .NOT.PSFCALC ) THEN
         DO I = 0, MAXPSF
            FRAC(I)  = 1.0
            IF ( DOEXPWGT .AND. EXMAPID.NE.' ' ) THEN

C              Call the weight function
               CALL GET_EXPOWGT( MEMR(EXMAPPTR), EXSZX, EXSZY, 
     &                           EXMAPID, XPIX, YPIX, J,
     &                           EXPOWGT, WGTCALCD, STATUS )

               FRACW(I) = EXPOWGT
            ELSE
               FRACW(I) = 1.0
            ENDIF
         ENDDO
         IF ( PSFFILE.EQ.' ' ) THEN
            CALL LKUPCAL( ITEL, 'psf_*.dat', PSFFILE, STATUS )
            IF ( STATUS.NE.0 ) THEN
               STATUS = 0
               CALL XWARN( ' looking for a local psf file:'//
     &                     ' ximage_psf.dat', 10 )
               PSFFILE = 'ximage_psf.dat'
               INQUIRE( FILE=PSFFILE, EXIST=THERE )
               IF ( .NOT.THERE ) THEN
                  STATUS = -1
               ENDIF
            ENDIF

C**********************************************************************
C If no PSF calibration file, use generic unknown psf and give warning
C The unknown psf is the ROSAT HRI psf.
C**********************************************************************
            IF ( STATUS.NE.0 ) THEN
               UNKNOWN = .TRUE.
               CALL LKUPCAL( -2, 'psf_unknown.dat', PSFFILE, STATUS )
               IF ( STATUS.NE.0 ) THEN
                  CALL XWARN( ' Failed to find any psf ', 10 )
                  PSFFILE = ' '
               ELSEIF ( FIRST ) THEN
                  CALL XWARN( ' No psf file for mission, '//
     &                        'using unknown psf', 10 )
               ENDIF
            ENDIF
         ENDIF

      ELSE

C*********************************************************
C Look for additional psf correction file (e.g. SAX/MECS)
C*********************************************************
         IF ( PSFFILE.EQ.' ' .AND. FIRST ) THEN
            CALL LKUPCAL( ITEL, 'psf_*.dat', PSFFILE, STATUS )
            IF ( STATUS.EQ.0 ) THEN
               CALL XWRITE( ' PSF function + correction file', 10 )
            ELSE
               STATUS = 0
            ENDIF
         ENDIF

      ENDIF

C****************************
C Read the psf file if found
C****************************
      IF ( PSFFILE.NE.' ' ) THEN

         STATUS = 0
         CALL RDPSF( PSFFILE, OFFAXIS, PIXSIZE, FIRST, 
     &               MAXPSF, TMPFRAC, SFA, STATUS )
         IF ( STATUS.EQ.0 ) THEN
            DO I = 0, MAXPSF
               FRAC(I)  = FRAC(I)  * TMPFRAC(I)
               FRACW(I) = FRACW(I) * TMPFRAC(I)
            ENDDO
         ELSE
            IF ( FIRST ) THEN
               CALL XWRITE( ' Failed to determine a PSF', 10 )
               DO I = 0, MAXPSF
                  FRAC(I)  = 0.0
                  FRACW(I) = 0.0
               ENDDO
            ENDIF
            RETURN
         ENDIF
      ENDIF

C*****************************************************************
C Remember the last telescope used, to avoid reading psf in twice
C*****************************************************************
      SAV_ITEL = ITEL
      FIRST    = .FALSE.

C******
C Done
C******
      RETURN
      END
