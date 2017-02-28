        SUBROUTINE PUTFIT(LUN,INFILE,RARRAY,I2ARRAY,I4ARRAY,IIFLAG,
     +      UNITS,COMM,ISIZE,BZERO,BSCALE,CRVAL1,CDELT1,CRVAL2,
     +      CDELT2,STATUS)
C
C  LUN       logical unit number
C  INFILE    file name of output file
C  RARRAY    REAL*4 array for output
C  I2ARRAY   INTEGER*2 array for output
C  I4ARRAY   INTEGER*4 array for output
C  IFLAG     flag for input file type selection
C            1 => R*4 array, PSPC
C            2 => I*2 array, PSPC
C            3 => I*4 array, PSPC
C            -1 => R*4 array, HRI
C            -2 => I*2 array, HRI
C            -3 => I*4 array, HRI
C  UNITS     character string showing the units of the array
C  COMM      comment character string
C  ISIZE     array (square) dimension
C  BZERO     offset value for array
C  BSCALE    slope value for array
C  CRVAL1    reference coordinate for first axis (RA)
C  CDELT1    pixel size for first axis
C  CRVAL2    reference coordinate for second axis (Dec)
C  CDELT2    pixel size for second axis
C  STATUS    FITSIO status flag
C            on input 0 => equatorial
C                     1 => ecliptic
C                     2 => equatorial
C                     3 => galactic
C
        IMPLICIT NONE
C
        INTEGER*4 ISIZE
        REAL*4 BZERO, BSCALE, CDELT1, CDELT2, CRPIX1, CRPIX2, CRVAL1, 
     +      CRVAL2, RARRAY(ISIZE,ISIZE)
        INTEGER*2 I2ARRAY(ISIZE,ISIZE)
        INTEGER*4 I4ARRAY(ISIZE,ISIZE)
        character(5)  COM
        character(8)  UNITS
        character(60) COMM, COMMENT
        character(80) INFILE
        INTEGER*4 BITPIX, COORD, ERR, FPIXEL, IFLAG, IIFLAG, 
     +      GCOUNT, GROUP, NAXES(10), NAXIS, LUN, PCOUNT, STATUS
        LOGICAL EXTEND, SIMPLE
C
        EQUIVALENCE (COMMENT,COM)
C
	ERR =0
	COORD = 0
        IF(STATUS .LT. 4) THEN
            COORD = STATUS
            IF(COORD .EQ. 0) COORD = 2
            STATUS = 0
        ENDIF
        IFLAG = IABS(IIFLAG)
        COMMENT = COMM
C
C  Write out the array
C
        PRINT *, 'Writing output file'
        PRINT *, INFILE
C
        SIMPLE = .TRUE.
        NAXIS = 2
        NAXES(1) = ISIZE
        NAXES(2) = ISIZE
        PCOUNT = 0
        GCOUNT = 1
        EXTEND = .FALSE.
        IF(IFLAG .EQ. 1) THEN
            BITPIX = -32
        ELSEIF(IFLAG .EQ. 2) THEN
            BITPIX = 16
        ELSEIF(IFLAG .EQ. 3) THEN
            BITPIX = 32
        ENDIF
        GROUP = 1
        FPIXEL = 1
C
C  Open the output file
C
        CALL FTINIT(LUN,INFILE,2880,STATUS)
        IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
            ERR = 1
            PRINT *, 'STATUS FTINIT =', STATUS
            PRINT *, 'POSITION 1'
            PRINT *, INFILE
        ENDIF
C
C  Set the output file header
C
        CALL FTPHPR(LUN,SIMPLE,BITPIX,NAXIS,NAXES,PCOUNT,GCOUNT,
     +              EXTEND,STATUS)
        IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
            ERR = 1
            PRINT *, 'STATUS FTPHPR =', STATUS
            PRINT *, 'POSITION 2'
            PRINT *, INFILE
        ENDIF
C
C  Write some useful header keywords
C
        CALL FTPCOM(LUN,COMMENT,STATUS)
        CALL FTPKYS(LUN,'CONTENT','IMAGE','  ',STATUS)
            CALL FTPKYS(LUN,'TELESCOP','ROSAT','mission name',STATUS)
        IF(IIFLAG .GT. 0) THEN
            CALL FTPKYS(LUN,'INSTRUME','PSPC','instrument name',STATUS)
        ELSE
            CALL FTPKYS(LUN,'INSTRUME','HRI','instrument name',STATUS)
        ENDIF
        CALL FTPKYS(LUN,'OBS_MODE','POINTING',
     +          'obs mode: POINTING,SLEW, OR SCAN',STATUS)
        CALL FTPKYS(LUN,'RADECSYS','FK5',
     +          'Equatorial system reference',STATUS)
        CALL FTPKYJ(LUN,'EQUINOX',2000,'Equinox',STATUS)
        CALL FTPKYF(LUN,'BZERO',BZERO,4,' ',STATUS)
        CALL FTPKYF(LUN,'BSCALE',BSCALE,8,' ',STATUS)
        CALL FTPKYS(LUN,'BUNIT',UNITS,'Units of data',STATUS)
        CRPIX1 = ISIZE/2. + 0.5
        CALL FTPKYF(LUN,'CRPIX1',CRPIX1,4,'Reference pixel',STATUS)
        IF(COM .EQ. 'Merge') then
            IF(COORD .EQ. 1) THEN
                CALL FTPKYS(LUN,'CTYPE1','ELON-ZEA','Projection',STATUS)
                CALL FTPKYF(LUN,'CRVAL1',CRVAL1,4,
     +              'Ecliptic longitude',STATUS)
            ELSEIF(COORD .EQ. 2) THEN
                CALL FTPKYS(LUN,'CTYPE1','RA---ZEA','Projection',STATUS)
                CALL FTPKYF(LUN,'CRVAL1',CRVAL1,4,'Right Ascension',
     +              STATUS)
            ELSEIF(COORD .EQ. 3) THEN
                CALL FTPKYS(LUN,'CTYPE1','GLON-ZEA','Projection',STATUS)
                CALL FTPKYF(LUN,'CRVAL1',CRVAL1,4,
     +              'Galactic longitude',STATUS)
            ENDIF
        ELSE
            CALL FTPKYS(LUN,'CTYPE1','RA---TAN','Projection',STATUS)
            CALL FTPKYF(LUN,'CRVAL1',CRVAL1,4,'Right Ascension',STATUS)
        ENDIF
        CALL FTPKYF(LUN,'CDELT1',CDELT1,8,'Pixel size',STATUS)
        CALL FTPKYS(LUN,'CUNIT1','deg','Units of coordinate',STATUS)
C
        CRPIX2 = ISIZE/2. + 0.5
        CALL FTPKYF(LUN,'CRPIX2',CRPIX2,4,'Reference pixel',STATUS)
        IF(COM .EQ. 'Merge') then
            IF(COORD .EQ. 1) THEN
                CALL FTPKYS(LUN,'CTYPE2','ELAT-ZEA','Projection',STATUS)
                CALL FTPKYF(LUN,'CRVAL2',CRVAL2,4,
     +              'Ecliptic latitude',STATUS)
            ELSEIF(COORD .EQ. 2) THEN
                CALL FTPKYS(LUN,'CTYPE2','DEC--ZEA','Projection',STATUS)
                CALL FTPKYF(LUN,'CRVAL2',CRVAL2,4,'Declination',STATUS)
            ELSEIF(COORD .EQ. 3) THEN
                CALL FTPKYS(LUN,'CTYPE2','GLAT-ZEA','Projection',STATUS)
                CALL FTPKYF(LUN,'CRVAL2',CRVAL2,4,
     +              'Galactic latitude',STATUS)
            ENDIF
        ELSE
            CALL FTPKYS(LUN,'CTYPE2','DEC--TAN','Projection',STATUS)
            CALL FTPKYF(LUN,'CRVAL2',CRVAL2,4,'Declination',STATUS)
        ENDIF
        CALL FTPKYF(LUN,'CDELT2',CDELT2,8,'Pixel size',STATUS)
        CALL FTPKYS(LUN,'CUNIT2','deg','Units of coordinate',STATUS)
C
        CALL FTPCOM(LUN,
     +  'This file was produced using the ESAS software written by',
     +  STATUS)
        CALL FTPCOM(LUN,
     +  'S. L. Snowden, and written using the FITSIO package of ',
     +  STATUS)
        CALL FTPCOM(LUN,'W. D. Pence.',STATUS)
C
C  Define the file format
C
        CALL FTPDEF(LUN,BITPIX,NAXIS,NAXES,PCOUNT,GCOUNT,EXTEND,
     +              STATUS)
        IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
            ERR = 1
            PRINT *, 'STATUS FTPDEF =', STATUS
            PRINT *, 'POSITION 3'
            PRINT *, INFILE
        ENDIF
C
C  Write the file
C
        IF(IFLAG .EQ. 1) THEN
            CALL FTP2DE(LUN,GROUP,ISIZE,ISIZE,ISIZE,RARRAY,STATUS)
            IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
                ERR = 1
                PRINT *, 'STATUS FTP2DE =', STATUS
                PRINT *, 'POSITION 4'
                PRINT *, INFILE
            ENDIF
        ELSEIF(IFLAG .EQ. 2) THEN
            CALL FTP2DI(LUN,GROUP,ISIZE,ISIZE,ISIZE,I2ARRAY,STATUS)
            IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
                ERR = 1
                PRINT *, 'STATUS FTP2DI =', STATUS
                PRINT *, 'POSITION 5'
                PRINT *, INFILE
            ENDIF
        ELSEIF(IFLAG .EQ. 3) THEN
            CALL FTP2DJ(LUN,GROUP,ISIZE,ISIZE,ISIZE,I4ARRAY,STATUS)
            IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
                ERR = 1
                PRINT *, 'STATUS FTP2DJ =', STATUS
                PRINT *, 'POSITION 6'
                PRINT *, INFILE
            ENDIF
        ENDIF
C
C  Close the file
C
        CALL FTCLOS(LUN,STATUS)
        IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
            ERR = 1
            PRINT *, 'STATUS FTOPEN =', STATUS
            PRINT *, 'POSITION 7'
            PRINT *, INFILE
        ENDIF
C
        RETURN
        END
