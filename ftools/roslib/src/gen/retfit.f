        SUBROUTINE RETFIT(LUN,INFILE,RARRAY,I2ARRAY,I4ARRAY,
     +      IFLAG,ISIZE,CRVAL1,CDELT1,CRVAL2,CDELT2,STATUS)
C
C  LUN       logical unit number
C  INFILE    file name of input file
C  RARRAY    REAL*4 array for input
C  I2ARRAY   INTEGER*2 array for input
C  I4ARRAY   INTEGER*4 array for input
C  IFLAG     flag for input file type selection
C            1 => R*4 array
C            2 => I*2 array
C            3 => I*4 array
C  ISIZE     array (square) dimension
C  CRVAL1    reference coordinate for first axis (RA)
C  CDELT1    pixel size for first axis
C  CRVAL2    reference coordinate for second axis (Dec)
C  CDELT2    pixel size for second axis
C  STATUS    FITSIO status flag
C
        IMPLICIT NONE
C
        INTEGER*4 ISIZE
C
        REAL*4    RARRAY(ISIZE,ISIZE)
        INTEGER*2 I2ARRAY(ISIZE,ISIZE)
        INTEGER*4 I4ARRAY(ISIZE,ISIZE)
        character(80) COM, INFILE
        INTEGER*4 BITPIX, BLOCK, ERR, IFLAG, GCOUNT, NAXES(10), 
     +      NAXIS, LUN, PCOUNT, STATUS
        REAL*4 CDELT1, CDELT2, CRVAL1, CRVAL2
        LOGICAL ANYF, EXTEND, SIMPLE
C
C  Initialize
C
        ERR = 0
C
C  Read the array
C
        PRINT *, 'Reading input file'
        PRINT *, INFILE
C
C  Open the FITS file for the first observation exposure
C
        CALL FTOPEN(LUN,INFILE,0,BLOCK,STATUS)
        IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
            ERR = 1
            PRINT *, 'STATUS FTOPEN =', STATUS
            PRINT *, 'POSITION 1'
            PRINT *, INFILE
        ENDIF
C
C  Read the FITS file header information
C
        CALL FTGHPR (LUN,10,SIMPLE,BITPIX,NAXIS,NAXES,
     +                      PCOUNT,GCOUNT,EXTEND,STATUS)
        IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
            ERR = 1
            PRINT *, 'STATUS FTGHPR =', STATUS
            PRINT *, 'POSITION 2'
            PRINT *, INFILE
        ENDIF
C
C  Get the useful keywords
C
        CALL FTGKYE(LUN,'CRVAL1',CRVAL1,COM,STATUS)
        IF(STATUS .NE. 0) THEN
            CRVAL1 = 0
            STATUS = 0
        ENDIF
        CALL FTGKYE(LUN,'CDELT1',CDELT1,COM,STATUS)
        IF(STATUS .NE. 0) THEN
            CDELT1 = 0
            STATUS = 0
        ENDIF
        CALL FTGKYE(LUN,'CRVAL2',CRVAL2,COM,STATUS)
        IF(STATUS .NE. 0) THEN
            CRVAL2 = 0
            STATUS = 0
        ENDIF
        CALL FTGKYE(LUN,'CDELT2',CDELT2,COM,STATUS)
        IF(STATUS .NE. 0) THEN
            CDELT2 = 0
            STATUS = 0
        ENDIF

C
C  Read in an R*4 file
C
        IF(IFLAG .EQ. 1) THEN
            CALL FTG2DE (LUN,0,0,ISIZE,NAXES(1),NAXES(2),RARRAY,
     +              ANYF,STATUS)
            IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
                ERR = 1
                PRINT *, 'STATUS FTG2DE =', STATUS
                PRINT *, 'POSITION 3'
                PRINT *, INFILE
            ENDIF
C
C  Read in an I*2 file
C
        ELSEIF(IFLAG .EQ. 2) THEN
            CALL FTG2DI (LUN,0,0,ISIZE,NAXES(1),NAXES(2),I2ARRAY,
     +              ANYF,STATUS)
            IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
                ERR = 1
                PRINT *, 'STATUS FTG2DI =', STATUS
                PRINT *, 'POSITION 4'
                PRINT *, INFILE
            ENDIF
C
C  Read in an I*4 file
C
        ELSEIF(IFLAG .EQ. 3) THEN
            CALL FTG2DJ (LUN,0,0,ISIZE,NAXES(1),NAXES(2),I4ARRAY,
     +              ANYF,STATUS)
            IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
                ERR = 1
                PRINT *, 'STATUS FTG2DJ =', STATUS
                PRINT *, 'POSITION 5'
                PRINT *, INFILE
            ENDIF
        ENDIF
C
C  Close the FITS file
C
        CALL FTCLOS(LUN,STATUS)
        IF((STATUS .NE. 0) .AND. (ERR .EQ. 0)) THEN
            ERR = 1
            PRINT *, 'STATUS FTCLOS =', STATUS
            PRINT *, 'POSITION 6'
            PRINT *, INFILE
        ENDIF
C
        RETURN
        END
