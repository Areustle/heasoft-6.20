C******************************************************************************
C FTOOL TASK:
C      fbdrm2rmf
C
C FILE:
C      fbdrm2rmf.f
C
C FTOOL USAGE:
C      fbdrm2rmf drmfile rmffile
C
C DESCRIPTION:
C      Main FORTRAN routine for FTOOL to convert Compton Gamma Ray Observatory
C      (CGRO) Burst and Transient Source Experiment (BATSE) DRM files into
C      Office of Guest Investigator Programs (OGIP) standard FITS format
C      "Redistribution Matrix File" (RMF) format, compatible with XSPEC x-ray
C      spectral fitting package
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   v1.0 - Original version
C       1 July 1997 NG   minor changes for date keyword comments. 
C                        Changed the ddmmyy/hhmmss length to 68.
C
C NOTES:
C      RMF files are intended for use with corresponding BATSE "Pulse Height
C      Analyzer" (PHA) files, generated with FTOOL fbft2pha.  PHA file is
C      assumed to have same name as RMF file, but with matching-case extension,
C      .pha or .PHA.  Expected name of PHA file appears in RMF keyword PHAFILE.
C
C   +-----------------------------------------------------------
C   |
C   |  Outline of BATSE RMF FITS file format:
C   |
C   |      Primary  HDU:  Null data array
C   |
C   |      Extension #1:  BINTABLE  'SPECRESP MATRIX'
C   |                     Col    TTYPE       TFORM      TUNIT
C   |                     ^^^    ^^^^^^^^    ^^^^^^^    ^^^^^
C   |                       1    ENERG_LO    1E         keV
C   |                       2    ENERG_HI    1E         keV
C   |                       3    N_GRP       1I         none
C   |                       4    F_CHAN      PI(126)    chan
C   |                       5    N_CHAN      PI(126)    chan
C   |                       6    MATRIX      PE(252)    cm**2
C   |
C   |      Extension #2:  BINTABLE  'EBOUNDS'
C   |                     Col    TTYPE       TFORM      TUNIT
C   |                     ^^^    ^^^^^^^^    ^^^^^^^    ^^^^^
C   |                       1    CHANNEL     1I         chan
C   |                       2    E_MIN       1E         keV
C   |                       3    E_MAX       1E         keV
C   |
C   +-----------------------------------------------------------
C
C MAKE:
C      make fbdrm2rmf
C
C USAGE:
C      [result] = fbdrm2rmf ( )
C
C ARGUMENTS:
C    Input  ...  none
C    Output ...
C      [result] - status code
C
C PRIMARY LOCAL VARIABLES:
C      drmfile  - name of input BATSE DRM file
C      rmffile  - name of output RMF file
C      status   - status code
C      taskname - FTOOL identifier
C
C CALLED ROUTINES:
C      subroutine convert_bdrm - performs file conversion
C      subroutine getfilenames - gets filename parameters from environment
C      subroutine FTCMSG       - clears FITSIO error message stack
C
C******************************************************************************

      INTEGER FUNCTION fbdrmf ( )
      IMPLICIT NONE
      character(180) drmfile, rmffile
      CHARACTER taskname*40
      INTEGER status
      COMMON /TASK/ taskname

      status=0
C
C --- Set function return value to error until completion
      fbdrmf = -1
C
      taskname = 'fbdrm2rmf v1.0'
      CALL FTCMSG
C
C --- Get parameters from .par file
      CALL getfilenames ( 'drmfile', 'rmffile',
     &  drmfile, rmffile, status )
C --- Now do everything else
      CALL convert_bdrm ( drmfile, rmffile, status )
C
C --- Set function return value to status code
      fbdrmf = status
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      convert_bdrm
C
C DESCRIPTION:
C      creates RMF file from BATSE DRM file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C      This is the routine that will normally detect erroneous attempts to
C      process input files that are not in FITS format, because in such
C      cases the FTOPEN request will fail and return an error status code.
C
C USAGE:
C      CALL convert_bdrm ( drmfile, rmffile, status )
C
C ARGUMENTS:
C    Input  ...
C      drmfile - name of input BATSE DRM file
C      rmffile - name of output RMF file
C      status  - status code
C    Output ...  none
C
C PRIMARY LOCAL VARIABLES:
C      blocksize - FITSIO logical record blocking factor
C      cbuf      - card image buffer containing keyword records
C      ch_edges  - DRM output-side channel energy edges
C      edges     - DRM input-side photon energy edges
C      icard     - positional index into card image buffer
C      intstat   - interim status code
C      iunit     - FITSIO unit number for input file
C      n_col     - number of output energy channels
C      ounit     - FITSIO unit number for output file
C      rwmode    - FITSIO read/write access mode for input file
C      MAX_CHAN  - maximum possible number of spectral channels
C      MAX_EDGES - maximum possible number of input-side photon energy edges
C
C CALLED ROUTINES:
C      subroutine hdu0_hdr_brmf - write primary HDU in output file
C      subroutine make_hdu1     - write 1st extension HDU in output file
C      subroutine make_hdu2     - write 2nd extension HDU in output file
C      subroutine FCERRM        - display FITSIO error status & message stack
C      subroutine FTCLOS        - close FITS file
C      subroutine FTFIOU        - deallocate I/O unit number(s)
C      subroutine FTGIOU        - get unused I/O unit number
C      subroutine FTINIT        - open and initialize new empty FITS file
C      subroutine FTOPEN        - open existing FITS file
C
C******************************************************************************

      SUBROUTINE convert_bdrm ( drmfile, rmffile, status )
      IMPLICIT NONE
      CHARACTER*(*) drmfile, rmffile
      INTEGER MAX_EDGES, MAX_CHAN
      PARAMETER ( MAX_EDGES = 276, MAX_CHAN = 252 )
      REAL edges(MAX_EDGES+1), ch_edges(MAX_CHAN+1)
      CHARACTER cbuf(108)*80
      INTEGER iunit, ounit, n_col
      INTEGER rwmode, blocksize, intstat, status
      INTEGER icard
      DATA icard /0/
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Assign input and output unit numbers
      CALL FTGIOU ( iunit, status )
      CALL FTGIOU ( ounit, status )
C
C --- Open the input DRM file, with readonly access
      rwmode = 0
      CALL FTOPEN ( iunit, drmfile, rwmode, blocksize, status )
C
C --- Create the new empty output RMF file, with standard block size
      blocksize = 1
      CALL FTINIT ( ounit, rmffile, blocksize, status )
C
C --- Generate primary HDU of the output RMF file
      CALL hdu0_hdr_brmf ( iunit, drmfile, ounit, rmffile,
     &  cbuf, icard, status )
C
C --- Generate HDU 1 of the output RMF file
      CALL make_hdu1 ( iunit, ounit, cbuf, icard,
     &  edges, ch_edges, n_col, status )
C
C --- Generate HDU 2 of the output RMF file
      CALL make_hdu2 ( ounit, ch_edges, n_col, cbuf, icard, status )
C
C --- Save current status code before going on to cleanup section
      intstat = status
C
C --- Output error messages, if any
      CALL FCERRM ( status )
C
      IF ( status .NE. 0 ) THEN
C ---     If there was an error, delete the output file
          status = 0
          CALL FTDELT ( ounit, status )
      ELSE
C ---     Close output file
          CALL FTCLOS ( ounit, status )
      END IF
C
C --- Close input file and free unit numbers
      CALL FTCLOS ( iunit, status )
      CALL FTFIOU ( -1, status )
C
C --- If interim status indicated error, return interim status instead
      IF ( intstat .NE. 0 ) status = intstat
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      hdu0_hdr_brmf
C
C DESCRIPTION:
C      Generates primary HDU of the output RMF file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C      This is the routine that will normally detect erroneous attempts to
C      process input FITS files that are not in the BATSE DRM format.
C      Often this will occur when attempting to delete the FILETYPE keyword,
C      since this keyword often differentiates BATSE DRM files from other
C      FITS file types.  If a BATSE BFITS file is provided as input, this
C      routine will detect an error when attempting to modify the name of
C      the DET_MODE keyword, whose presence is one feature that can be used
C      to differentiate BATSE DRM files from BATSE BFITS files.
C
C USAGE:
C      CALL hdu0_hdr_brmf ( iunit, drmfile, ounit, rmffile,
C                           cbuf, icard, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit   - FITSIO unit number for input file
C      drmfile - name of input BATSE DRM file
C      ounit   - FITSIO unit number for output file
C      rmffile - name of output RMF file
C    Output ...
C      cbuf    - card image buffer containing all primary HDU keyword records
C      icard   - positional index of last card in image buffer
C      status  - status code
C
C PRIMARY LOCAL VARIABLES:
C      cment    - buffer to hold comment portion of FITS card
C      detstr   - detector identifier label string
C      ext      - flag to indicate whether fname should return file extension
C      file     - buffer to hold various file names
C      inkey    - name of keyword to be changed
C      morekeys - number of keywords to add to original HDU
C      outkey   - name of new keyword to put in output PHA flie
C      taskname - FTOOL identifier
C      FILE_VER - version number of RMF file
C
C CALLED ROUTINES:
C      subroutine chcard  - Transfer date or time keyword with format change
C      function   ddmmyy  - Convert date from yyyy.ddd to dd/mm/yy format
C      function   detnam_brmf - Form DETNAM keyword from component keywords
C      function   fname   - Extract filename from full file specification
C      function   hhmmss  - Convert seconds-of-day to time-of-day format
C      subroutine savhdr  - Save all cards in current HDU in card image buffer
C      subroutine xferkey - Transfer keyword record from input to output file
C      subroutine FTCOPY  - Copy entire HDU from input to output file
C      subroutine FTDKEY  - Delete existing keyword record
C      subroutine FTMCOM  - Modify comment field of existing keyword
C      subroutine FTMKYS  - Modify value and comment fields of existing keyword
C      subroutine FTMNAM  - Modify name of existing keyword
C      subroutine FTPCOM  - Append COMMENT keyword
C      subroutine FTPDAT  - Append/update DATE keyword
C      subroutine FTPHIS  - Append HISTORY keyword
C      subroutine FTPKYS  - Append keyword of string type
C      subroutine FTRDEF  - Reinitialize structure of HDU
C
C******************************************************************************

      SUBROUTINE hdu0_hdr_brmf ( iunit, drmfile, ounit, rmffile,
     &  cbuf, icard, status )
      IMPLICIT NONE
      EXTERNAL ddmmyy, hhmmss
      character(68) ddmmyy, hhmmss
      CHARACTER*(*) drmfile, rmffile
      character(180) fname, file
      CHARACTER cbuf(*)*(*)
      CHARACTER cment*70
      CHARACTER taskname*40
      CHARACTER detnam_brmf*12, detstr*12
      character(8) inkey, outkey
      character(8) FILE_VER
      PARAMETER ( FILE_VER = '1.0' )
      INTEGER iunit, ounit
      INTEGER morekeys
      INTEGER icard
      INTEGER status
      LOGICAL ext
      COMMON /TASK/ taskname
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Copy the primary HDU from the input file to the output file
      morekeys = 22
      CALL FTCOPY ( iunit, ounit, morekeys, status )
C
C --- Define the structure of the output primary HDU
      CALL FTRDEF ( ounit, status )
C
C --- Change the values of some keywords
      CALL FTMKYS ( ounit, 'TELESCOP', 'CGRO',
     &  'Mission identifier label', status )
C
      CALL FTMKYS ( ounit, 'ORIGIN', 'NASA/GSFC/COSSC',
     &  'Institution that produced this dataset', status )
C
C --- Delete the FILETYPE keyword inherited from the input DRM file
      CALL FTDKEY ( ounit, 'FILETYPE', status )
C
C --- Rename and convert format of some date/time keywords:
C --- STRT-DAY -> DATE-OBS
C --- STRT-TIM -> TIME-OBS
C --- END-DAY  -> DATE-END
C --- END-TIM  -> TIME-END
C --- TRIG-DAY -> TRIG_DAY
C --- TRIG-TIM -> TRIG_TIM
C
      inkey = 'STRT-DAY'
      outkey = 'DATE-OBS'
      cment = 'Start date of data (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment, status )
C
      inkey = 'STRT-TIM'
      outkey = 'TIME-OBS'
      cment = 'Start time of data (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment, status )
C
      inkey = 'END-DAY'
      outkey = 'DATE-END'
      cment = 'End date of data (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment, status )
C
      inkey = 'END-TIM'
      outkey = 'TIME-END'
      cment = 'End time of data (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment, status )
C
      inkey = 'TRIG-DAY'
      outkey = 'TRIG_DAY'
      cment = 'Date of burst trigger (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment, status )
C
      inkey = 'TRIG-TIM'
      outkey = 'TRIG_TIM'
      cment = 'Time of burst trigger (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment, status )
C
C --- Change the names of some keywords
C
      CALL FTMNAM ( ounit, 'SC-Z-RA' , 'RA_SCZ' ,  status )
      CALL FTMNAM ( ounit, 'SC-Z-DEC', 'DEC_SCZ',  status )
      CALL FTMNAM ( ounit, 'SC-X-RA' , 'RA_SCX' ,  status )
      CALL FTMNAM ( ounit, 'SC-X-DEC', 'DEC_SCX',  status )
      CALL FTMNAM ( ounit, 'OBJCTRA' , 'RA_OBJ' ,  status )
      CALL FTMNAM ( ounit, 'OBJCTDEC', 'DEC_OBJ',  status )
C
      CALL FTMNAM ( ounit, 'SC-X-POS', 'SC_X_POS', status )
      CALL FTMNAM ( ounit, 'SC-Y-POS', 'SC_Y_POS', status )
      CALL FTMNAM ( ounit, 'SC-Z-POS', 'SC_Z_POS', status )
C
C --- Rename and modify the DET_MODE keyword
      CALL FTMNAM ( ounit, 'DET_MODE', 'DETNAM',   status )
      detstr = detnam_brmf ( iunit )
      CALL FTMKYS ( ounit, 'DETNAM', detstr,
     &  'Sub-instrument identifier label', status )
C
C --- Remove N_E_CHAN keyword in favor of DETCHANS
      CALL FTDKEY ( ounit, 'N_E_CHAN', status )
C
C --- Rename and modify the FILE-ID keyword
      CALL FTMNAM ( ounit, 'FILE-ID',  'FILE_ID',  status )
      ext = .TRUE.
      file = fname ( rmffile, ext )
      CALL FTMKYS ( ounit, 'FILE_ID', file, '&', status )
C
C --- Rename and modify the FILE-VER keyword
      CALL FTMNAM ( ounit, 'FILE-VER', 'FILE_VER', status )
      CALL FTMKYS ( ounit, 'FILE_VER', FILE_VER, '&', status )
C
C --- Rename and modify comment of the FILENAME keyword
      CALL FTMNAM ( ounit, 'FILENAME', 'BFITSFIL', status )
      CALL FTMCOM ( ounit, 'BFITSFIL', 'Ancestral BFITS file', status )
C
C --- Update the FITS file creation date keyword
      CALL FTPDAT ( ounit, status )
C
      CALL FTMNAM ( ounit, 'MNEMONIC', 'CREATOR', status )
      CALL FTMCOM ( ounit, 'CREATOR',
     &  'Program that created BATSE DRM input file', status )
C
C --- Generate and write PHAFILE keyword
      ext = .FALSE.
      file = fname ( rmffile, ext )
C
      IF ( LGT (file, '_') ) THEN
          file = file(:INDEX(file,' ')-1) // '.pha'
      ELSE
          file = file(:INDEX(file,' ')-1) // '.PHA'
      END IF
C
      CALL FTPKYS ( ounit, 'PHAFILE', file,
     &  'Name of corresponding PHA file', status )
C
C --- Write DRM input file name keyword
      ext = .TRUE.
      file = fname ( drmfile, ext )
      CALL FTPKYS ( ounit, 'DRMFILE', file,
     &  'Name of BATSE DRM input file', status )
C
C --- Transfer FILE-ID keyword record, renamed to DRMORI
      CALL xferkey ( iunit, 'FILE-ID', ounit, 'DRMORI', status )
      CALL FTMCOM ( ounit, 'DRMORI',
     &  'Original name of BATSE DRM input file', status )
C
C --- Write FTOOL identifier keyword
      CALL FTPKYS ( ounit, 'FTOOL', taskname,
     &  'FTOOL identifier label', status )
C
C --- Write HISTORY keyword
      cment = 'BATSE DRM file reworked by FTOOL ' // taskname
      CALL FTPHIS ( ounit, cment, status )
C
C --- Write standard FITS 4-line comment block
      cment = 'FITS (Flexible Image Transport System) '
     &     // 'format defined in Astronomy and'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Astrophysics Supplement Series '
     &     // 'v44/p363, v44/p371, v73/p359, v73/p365.'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Contact the NASA Science Office '
     &     // 'of Standards and Technology for the'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'FITS Definition document #100 '
     &     // 'and other FITS information.'
      CALL FTPCOM ( ounit, cment, status )
C
C --- Write block of descriptive comments specific to BATSE RMF files
      cment = '-----------------------------------'
     &     // '-----------------------------------'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'This file is a derived data product '
     &     // 'of the CGRO BATSE instrument.'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'It consists of data from a '
     &     // 'BATSE DRM file, reorganized for'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'compatibility with the XSPEC spectral '
     &     // 'analysis program.  Some of the'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'principal identifying keywords for '
     &     // 'the files and software that are'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'associated with the production and '
     &     // 'use of this file are as follows:'
      CALL FTPCOM ( ounit, cment, status )
      cment = '-------  Item  -------------------------  '
     &     // 'Keyword  -------------------'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Original name of this file                FILE_ID'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Software that generated this file         FTOOL'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Name of DRM input file                    DRMFILE'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Original name of DRM input file           DRMORI'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Software that generated DRM file          CREATOR'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Name of ancestral BFITS file              BFITSFIL'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Name of corresponding PHA file            PHAFILE'
      CALL FTPCOM ( ounit, cment, status )
      cment = '-----------------------------------'
     &     // '-----------------------------------'
      CALL FTPCOM ( ounit, cment, status )
C
C --- Save all the primary header keyword records in a character buffer
      CALL savhdr ( ounit, cbuf, icard, status )
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      make_hdu1
C
C DESCRIPTION:
C      Generates header and data table of first extension HDU of the output
C      RMF file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL make_hdu1 ( iunit, ounit, cbuf, icard,
C                       edges, ch_edges, n_col, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit    - FITSIO unit number for input file
C      ounit    - FITSIO unit number for output file
C      cbuf     - card image buffer containing keyword records
C      icard    - positional index into card image buffer
C    Output ...
C      edges    - DRM input-side photon energy edges, averaged over detectors
C      ch_edges - DRM output-side channel energy edges, averaged over detectors
C      n_col    - number of output energy channels
C      status   - status code
C
C PRIMARY LOCAL VARIABLES:
C      colnum   - column number of FITS binary table
C      decimals - number of decimal places to write with FTPKYD
C      drmtime  - time DRM is calculated for, from TIME column of DRM file
C      f_chan   - first channel in each subset for row of matrix
C      i_row    - row of matrix to operate on
C      intval   - work variable of type integer
C      mat_row  - vector of nonzero elements for row of matrix
C      mat_type - matrix type code, from MAT_TYPE column of DRM file
C      matrix   - uncompressed DRM, summed over detectors
C      n_chan   - number of channels in each subset for row of matrix
C      n_grp    - number of channel subsets for row of matrix
C      n_row    - number of input energy bins
C      n_tot    - number of nonzero elements in one row of matrix
C      pcount   - size in bytes of heap holding F_CHAN, N_CHAN, and MATRIX
C      tform    - data types of FITS table columns, for TFORM keywords
C      ttype    - names of FITS table columns, for TTYPE keywords
C      tunit    - physical units of FITS table columns, for TUNIT keywords
C      HDUVERS  - version number of file format document
C      MAX_CHAN - maximum possible number of spectral channels
C      TFIELDS  - Number of FITS table columns
C
C CALLED ROUTINES:
C      function   cardimg   - Retrieve specified card from card image buffer
C      subroutine matchar   - Get RMF grouping parameters for row of matrix
C      subroutine read_bdrm - Read major parameters and matrix from DRM file
C      subroutine savcard   - Save single specified card into card image buffer
C      subroutine FTCRHD    - Append new, empty HDU
C      subroutine FTMKYJ    - Modify value & comment fields of existing keyword
C      subroutine FTPCL|E/I - Write column elements of specified data type
C      subroutine FTPHBN    - Write principal binary table header keywords
C      subroutine FTPKY|D/J/S - Append keyword of specified data type
C      subroutine FTPREC    - Append card to HDU
C      subroutine FTRDEF    - Reinitialize structure of HDU
C
C******************************************************************************

      SUBROUTINE make_hdu1 ( iunit, ounit, cbuf, icard,
     &  edges, ch_edges, n_col, status )
      IMPLICIT NONE
      INTEGER MAX_EDGES, MAX_CHAN
      PARAMETER ( MAX_EDGES = 276, MAX_CHAN = 252 )
      REAL matrix(MAX_EDGES*MAX_CHAN), mat_row (MAX_CHAN)
      INTEGER TFIELDS
      PARAMETER ( TFIELDS = 6 )
      character(16) ttype(TFIELDS), tform(TFIELDS), tunit(TFIELDS)
      DATA ttype
     &         / 'ENERG_LO', 'ENERG_HI', 'N_GRP', 'F_CHAN' ,
     &           'N_CHAN'  , 'MATRIX'  /
      DATA tform
     &         / '1E'      , '1E'      , '1I'   , 'PI(126)',
     &           'PI(126)' , 'PE(252)' /
      DATA tunit
     &         / 'keV'     , 'keV'     , 'none' , 'chan'   ,
     &           'chan'    , 'cm**2'   /
      character(8) HDUVERS
      PARAMETER ( HDUVERS  = '1.0.0' )
      DOUBLE PRECISION drmtime
      REAL edges(*), ch_edges(*)
      CHARACTER cbuf(*)*(*)
      CHARACTER cardimg*80
      INTEGER iunit, ounit, status
      INTEGER i_row, n_row, n_col
      INTEGER n_tot, n_grp
      INTEGER colnum, decimals, pcount
      INTEGER icard, intval, tmp_mat_type
      INTEGER*2 f_chan(126), n_chan(126), mat_type
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
      CALL read_bdrm ( iunit, edges, ch_edges, matrix, n_row, n_col,
     &  mat_type, drmtime, status )
C
C --- Set up first extension output HDU header
      CALL FTCRHD ( ounit, status )
      CALL FTPHBN ( ounit, n_row, TFIELDS, ttype, tform, tunit,
     &  'SPECRESP MATRIX', 0, status )
      CALL FTRDEF ( ounit, status )
C
C --- Write the EXTVER keyword record
      intval = 1
      CALL FTPKYJ ( ounit, 'EXTVER', intval,
     &  'Version of this extension type', status )
C
C --- Write the MAT_TYPE keyword record
      tmp_mat_type = mat_type
      CALL FTPKYJ ( ounit, 'MAT_TYPE', tmp_mat_type,
     &  '0=Direct, 1=Scattered, 2=Both, 3=Summed', status )
C
C --- Write the LO_THRES keyword record
      intval = 0
      CALL FTPKYJ ( ounit, 'LO_THRES', intval,
     &  'Lower threshold used to construct RMF', status )
C
C --- Write the TIME, TIMESYS, and TIMEUNIT keyword records
      decimals = 13
      CALL FTPKYD ( ounit, 'TIME', drmtime, decimals,
     &  'Time DRM is calculated for', status )
      CALL FTPKYS ( ounit, 'TIMESYS',  'TJD',
     &  'Time frame system for TIME keyword', status )
      CALL FTPKYS ( ounit, 'TIMEUNIT', 'd',
     &  'Time unit for TIME keyword', status )
C
C --- Write various keywords from primary HDU
      CALL FTPREC ( ounit, cardimg('TELESCOP',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('INSTRUME',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('ORIGIN  ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('OBJECT  ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DATE-OBS',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('TIME-OBS',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DATE-END',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('TIME-END',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('RA_SCZ  ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DEC_SCZ ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('RA_SCX  ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DEC_SCX ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('RA_OBJ  ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DEC_OBJ ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DETNAM  ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('N_E_BINS',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('EQUINOX ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('BFITSFIL',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DATE    ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('CREATOR ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DRMFILE ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('DRMORI  ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('FTOOL   ',cbuf,icard), status )
      CALL FTPREC ( ounit, cardimg('HISTORY ',cbuf,icard), status )
C
C --- Write various new keywords
      CALL FTPKYS ( ounit, 'FILTER', 'none',
     &  'No filter for BATSE instrument', status )
      CALL FTPKYS ( ounit, 'HDUCLASS', 'OGIP',
     &  'File format origin', status )
      CALL FTPKYS ( ounit, 'HDUDOC',
     &  'George et al. 1992, Legacy 2, p. 51.',
     &  'Format specification', status )
      CALL FTPKYS ( ounit, 'HDUVERS', HDUVERS,
     &  'Version of format', status )
      CALL FTPKYS ( ounit, 'HDUCLAS1', 'RESPONSE',
     &  'Redistribution matrix file', status )
      CALL FTPKYS ( ounit, 'HDUCLAS2', 'RSP_MATRIX',
     &  'Extension header contains redistribution matrix', status )
      CALL FTPKYS ( ounit, 'HDUCLAS3', 'REDIST',
     &  'Pure redistribution matrix, nothing folded in', status )
      CALL FTPKYS ( ounit, 'RADECSYS', 'FK5',
     &  'Coordinate frame used for EQUINOX', status )
C
C --- Write PHAFILE keyword from primary HDU
      CALL FTPREC ( ounit, cardimg('PHAFILE ',cbuf,icard), status )
C
C --- Write various new keywords
      CALL FTPKYS ( ounit, 'CHANTYPE', 'PHA',
     &  'Channel correction type', status )
      CALL FTPKYJ ( ounit, 'DETCHANS', n_col,
     &  'Total number of detector channels', status )
C
C --- Write the RMFVERSN keyword
      CALL FTPKYS ( ounit, 'RMFVERSN', '1992a',
     &  'OGIP version number of FITS format', status )
C
C --- Finished writing keywords for 1st extension header.  Now do data.
C
C --- Write the input energy edges in TTYPE1 & TTYPE2 (ENERG_LO & ENERG_HI)
      colnum = 1
      CALL FTPCLE ( ounit, colnum, 1, 1, n_row, edges(1), status )
      colnum = 2
      CALL FTPCLE ( ounit, colnum, 1, 1, n_row, edges(2), status )
C
      pcount = 0
C
C --- Determine N_GRP, F_CHAN, and N_CHAN values for matrix, row by row
      DO i_row=1,n_row
          CALL matchar ( matrix, n_row, i_row, n_col,
     &      mat_row, n_tot, n_grp, f_chan, n_chan )
C
C ---     Write the TTYPE3 (N_GRP) column
          colnum = 3
          CALL FTPCLJ ( ounit, colnum, i_row, 1, 1, n_grp, status )
C
C ---     Write the TTYPE4 (F_CHAN) column
          colnum = 4
          CALL FTPCLI ( ounit, colnum, i_row, 1, n_grp, f_chan, status )
C
C ---     Write the TTYPE5 (N_CHAN) column
          colnum = 5
          CALL FTPCLI ( ounit, colnum, i_row, 1, n_grp, n_chan, status )
C
C ---     Write the TTYPE6 (MATRIX) column
          colnum = 6
          CALL FTPCLE ( ounit, colnum, i_row, 1, n_tot, mat_row,
     &      status )
C
C ---     Update the value of pcount, the size in bytes of the heap holding
C ---     F_CHAN, N_CHAN, and MATRIX.  Each increment in the value of n_grp
C ---     requires another value of F_CHAN (short integer, 2 bytes) and N_CHAN
C ---     (short integer, 2 bytes).  Each increment in the value of n_tot
C ---     requires another value of MATRIX (real, 4 bytes).
          pcount = pcount + (2+2)*n_grp + 4*n_tot
      END DO
C
C --- Now modify PCOUNT keyword to reflect actual size of heap area used.
      CALL FTMKYJ ( ounit, 'PCOUNT', pcount, '&', status )
C
C --- The change to PCOUNT _must_ be followed by an HDU structure redefinition.
      CALL FTRDEF ( ounit, status )
C
C --- Save cards that will be used again later
      CALL savcard ( ounit, 'MAT_TYPE', cbuf, icard, status )
      CALL savcard ( ounit, 'TIME    ', cbuf, icard, status )
      CALL savcard ( ounit, 'TIMESYS ', cbuf, icard, status )
      CALL savcard ( ounit, 'TIMEUNIT', cbuf, icard, status )
      CALL savcard ( ounit, 'FILTER  ', cbuf, icard, status )
      CALL savcard ( ounit, 'HDUCLASS', cbuf, icard, status )
      CALL savcard ( ounit, 'HDUDOC  ', cbuf, icard, status )
      CALL savcard ( ounit, 'HDUVERS ', cbuf, icard, status )
      CALL savcard ( ounit, 'HDUCLAS1', cbuf, icard, status )
      CALL savcard ( ounit, 'RADECSYS', cbuf, icard, status )
      CALL savcard ( ounit, 'CHANTYPE', cbuf, icard, status )
      CALL savcard ( ounit, 'DETCHANS', cbuf, icard, status )
      CALL savcard ( ounit, 'RMFVERSN', cbuf, icard, status )
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      make_hdu2
C
C DESCRIPTION:
C      Generates header and data table of second extension HDU of the output
C      RMF file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL make_hdu2 ( ounit, ch_edges, n_col, cbuf, ncard, status )
C
C ARGUMENTS:
C    Input  ...
C      ounit    - FITSIO unit number for output file
C      ch_edges - DRM output-side channel energy edges, averaged over detectors
C      n_col    - number of output energy channels
C      cbuf     - card image buffer containing keyword records
C      ncard    - highest card number filled in card image buffer
C    Output ...
C      status   - status code
C
C PRIMARY LOCAL VARIABLES:
C      chan     - vector of channel numbers used
C      colnum   - column number of FITS binary table
C      i        - work variable to loop through channel numbers used
C      intval   - work variable of type integer
C      tform    - data types of FITS table columns, for TFORM keywords
C      tlmax1   - maximum legal value for column 1 (CHANNEL)
C      tlmin1   - minimum legal value for column 1 (CHANNEL)
C      ttype    - names of FITS table columns, for TTYPE keywords
C      tunit    - physical units of FITS table columns, for TUNIT keywords
C      MAX_CHAN - maximum possible number of spectral channels
C      TFIELDS  - Number of FITS table columns
C
C CALLED ROUTINES:
C      function   cardimg   - Retrieve specified card from card image buffer
C      subroutine FTCRHD    - Append new, empty HDU
C      subroutine FTPCL|E/I - Write column elements of specified data type
C      subroutine FTPHBN    - Write principal binary table header keywords
C      subroutine FTPKY|J/S - Append keyword of specified data type
C      subroutine FTPREC    - Append card to HDU
C      subroutine FTRDEF    - Reinitialize structure of HDU
C
C******************************************************************************

      SUBROUTINE make_hdu2 ( ounit, ch_edges, n_col, cbuf, ncard,
     &  status )
      IMPLICIT NONE
      CHARACTER cbuf(*)*(*)
      CHARACTER cardimg*80
      REAL ch_edges (*)
      INTEGER MAX_CHAN
      PARAMETER ( MAX_CHAN = 252 )
      INTEGER TFIELDS
      PARAMETER ( TFIELDS = 3 )
      character(16) ttype(TFIELDS), tform(TFIELDS), tunit(TFIELDS)
      DATA ttype / 'CHANNEL', 'E_MIN', 'E_MAX' /
      DATA tform / '1I'     , '1E'   , '1E'    /
      DATA tunit / 'chan'   , 'keV'  , 'keV'   /
      INTEGER status, ounit
      INTEGER n_col, colnum, ncard
      INTEGER tlmin1, tlmax1
      INTEGER intval, i
      INTEGER*2 chan(255)

      DO i = 1, 255
          chan(i) = 0
      END DO
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
      CALL FTCRHD ( ounit, status )
C
      CALL FTPHBN ( ounit, n_col, TFIELDS, ttype, tform, tunit,
     &  'EBOUNDS', 0, status )
      CALL FTRDEF ( ounit, status )
C
C --- Write the EXTVER keyword record
      intval = 1
      CALL FTPKYJ ( ounit, 'EXTVER', intval,
     &  'Version of this extension type', status )
C
C --- Write various keywords from preceding HDUs
      CALL FTPREC ( ounit, cardimg('MAT_TYPE',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('TIME    ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('TIMESYS ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('TIMEUNIT',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('TELESCOP',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('INSTRUME',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('ORIGIN  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('OBJECT  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DATE-OBS',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('TIME-OBS',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DATE-END',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('TIME-END',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('RA_SCZ  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DEC_SCZ ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('RA_SCX  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DEC_SCX ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('RA_OBJ  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DEC_OBJ ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DETNAM  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('N_E_BINS',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('EQUINOX ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('BFITSFIL',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DATE    ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('CREATOR ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DRMFILE ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DRMORI  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('FTOOL   ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('HISTORY ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('FILTER  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('HDUCLASS',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('HDUDOC  ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('HDUVERS ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('HDUCLAS1',cbuf,ncard), status )
C
C --- Write the HDUCLAS2 keyword
      CALL FTPKYS ( ounit, 'HDUCLAS2', 'EBOUNDS',
     &  'Extension header contains energy boundaries', status )
C
C --- Write various keywords from preceding HDUs
      CALL FTPREC ( ounit, cardimg('RADECSYS',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('PHAFILE ',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('CHANTYPE',cbuf,ncard), status )
      CALL FTPREC ( ounit, cardimg('DETCHANS',cbuf,ncard), status )
C
C --- For the BATSE DRM files, n_col = 4, 16, 128, or 252 channels
      IF ( n_col .EQ. 4 ) THEN
          tlmin1 = 1
      ELSE IF ( n_col .EQ. 252 ) THEN
          tlmin1 = 4
      ELSE
          tlmin1 = 0
      END IF
C
C --- Write TLMIN1 & TLMAX1 from knowledge of n_col
      CALL FTPKYJ ( ounit, 'TLMIN1', tlmin1,
     &  'Minimum legal value for column 1', status )
      tlmax1 = n_col + tlmin1 - 1
      CALL FTPKYJ ( ounit, 'TLMAX1', tlmax1,
     &  'Maximum legal value for column 1', status )
C
C --- Write RMFVERSN keyword from preceding HDU
      CALL FTPREC ( ounit, cardimg('RMFVERSN',cbuf,ncard), status )
C
C --- Finished writing keywords for 2nd extension header.  Now do data.
C
C --- Assign the channel numbers
      DO i = tlmin1, tlmax1
          chan(i) = i
      END DO
C
C --- Write the channel numbers in TTYPE1 (CHANNEL)
      colnum = 1
      CALL FTPCLI ( ounit, colnum, 1, 1, n_col, chan(tlmin1), status )
C
C --- Write the input energy edges in TTYPE2 & TTYPE3 (E_MIN & E_MAX)
      colnum = 2
      CALL FTPCLE ( ounit, colnum, 1, 1, n_col, ch_edges(1), status )
      colnum = 3
      CALL FTPCLE ( ounit, colnum, 1, 1, n_col, ch_edges(2), status )
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      read_bdrm
C
C DESCRIPTION:
C      gets binary table column data and selected keywords from input file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL read_bdrm ( iunit, edges, ch_edges, matrix,
C                       n_row, n_col, mat_type, drmtime, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit    - FITSIO unit number for input file
C    Output ...
C      edges    - DRM input-side photon energy edges, averaged over detectors
C      ch_edges - DRM output-side channel energy edges, averaged over detectors
C      i_det    - index number of instrument detector
C      matrix   - uncompressed DRM, summed over detectors
C      n_row    - number of input energy bins (number of rows in DRM)
C      n_col    - number of output energy channels (number of columns in DRM)
C      matcol   - TTYPE number of binary table column holding compressed DRM
C      mat_type - matrix type code, from MAT_TYPE column of DRM file
C      drmtime  - time DRM is calculated for, from TIME column of DRM file
C      status  - status code
C
C PRIMARY LOCAL VARIABLES:
C      ch_edges_in - DRM output-side channel energy edges for specific detector
C      cment     - buffer to hold comment portion of FITS card
C      colnum    - column number of FITS binary table
C      drm_mat   - vector holding compressed DRM elements for specific detector
C      edges_in  - DRM input-side photon energy edges for specific detector
C      i         - work variable of type integer
C      intval    - work variable of type integer
C      logval    - work variable of type logical
C      mat_in    - uncompressed DRM for specific detector
C      matind    - subscript index to appropriate matrix type in matsiz array
C      matsiz    - size of compressed DRM for each detector/matrix type pairing
C      msg       - text message buffer
C      n_det     - number of instrument detectors used
C      numzero   - number of matrix columns, from NUMZERO column of DRM file
C      taskname  - FTOOL identifier
C      zeros     - vector holding number of zeroes at top of each DRM column
C      MAX_CHAN  - maximum possible number of spectral channels
C      MAX_NDET  - maximum possible number of instrument detectors used
C      MAX_EDGES - maximum possible number of input-side photon energy edges
C
C CALLED ROUTINES:
C      subroutine matfill     - Reconstitute matrix from compressed DRM format
C      subroutine matsum      - Add array to cumulative sum array
C      subroutine FCECHO      - Write text to terminal
C      subroutine FTGCV|D/E/I/J - Get binary table column of specified type
C      subroutine FTGKYJ      - Get keyword of integer type
C      subroutine FTMAHD      - Move to specified absolute HDU
C
C******************************************************************************

      SUBROUTINE read_bdrm ( iunit, edges, ch_edges, matrix,
     &  n_row, n_col, mat_type, drmtime, status )
      IMPLICIT NONE
      INTEGER MAX_EDGES, MAX_CHAN, MAX_NDET
      PARAMETER ( MAX_EDGES = 276, MAX_CHAN = 252, MAX_NDET = 8 )
      CHARACTER cment*70, msg*80, taskname*40
      DOUBLE PRECISION drmtime
      REAL matrix(*)
      REAL edges(*), ch_edges(*)
      REAL edges_in(MAX_EDGES+1), ch_edges_in(MAX_CHAN+1)
      REAL drm_mat (MAX_EDGES*MAX_CHAN), mat_in (MAX_EDGES*MAX_CHAN)
      INTEGER intval, status, iunit
      INTEGER n_row, n_col, n_det, i_det
      INTEGER matsiz(MAX_NDET,3)
      INTEGER matcol, matind, colnum
      INTEGER numzeroj
      INTEGER i
      INTEGER*2 zeros(MAX_CHAN), numzero, mat_type
      LOGICAL logval
      COMMON /TASK/ taskname

      matind = 0
      matcol = 0
C
C --- Ascertain the number of input energy bins (number of DRM rows)
      CALL FTGKYJ ( iunit, 'N_E_BINS', n_row, cment, status )
C
C --- Ascertain the number of output energy channels (number of DRM columns)
      CALL FTGKYJ ( iunit, 'N_E_CHAN', n_col, cment, status )
C
C --- Write the single informational message that is output during a normal run
      WRITE ( msg,
     & '(A,'': ...'',I4,'' input bins x'',I4,'' output channels ...'')')
     & taskname ( 1 : INDEX(taskname,'   ') ), n_row, n_col
      CALL FCECHO ( msg )
C
C --- Go to the DRM data table extension (Extension #1)
      CALL FTMAHD ( iunit, 2, intval, status )
C
C --- Now get number of detectors used (n_det)
      CALL FTGKYJ ( iunit, 'NAXIS2', n_det, cment, status )
C
C --- Get MAT_TYPE (TTYPE2)
      colnum = 2
      CALL FTGCVI ( iunit, colnum, 1, 1, 1, 0,
     &  mat_type, logval, status )
C
C --- Get TIME (TTYPE3)
      colnum = 3
      CALL FTGCVD ( iunit, colnum, 1, 1, 1, 0.D0,
     &  drmtime, logval, status )
C
      DO i_det=1,n_det
C ---     Read input-side photon energy edges from TTYPE10=PHT_EDGE
          colnum = 10
          CALL FTGCVE ( iunit, colnum, i_det, 1, n_row+1, 0,
     &      edges_in, logval, status )
C
C ---     Compute cumulative sum of photon energy edges over detectors
          CALL matsum ( n_row+1, edges_in, edges )
C
C ---     Read output-side channel energy edges from TTYPE11=E_EDGES
          colnum = 11
          CALL FTGCVE ( iunit, colnum, i_det, 1, n_col+1, 0,
     &      ch_edges_in, logval, status )
C
C ---     Compute cumulative sum of channel energy edges over detectors
          CALL matsum ( n_col+1, ch_edges_in, ch_edges )
      END DO
C
C --- Average the input-side photon energy edges over all detectors used
      DO i=1,n_row+1
          edges(i) = edges(i) / n_det
      END DO
C
C --- Average the output-side channel energy edges over all detectors used
      DO i=1,n_col+1
          ch_edges(i) = ch_edges(i) / n_det
      END DO
C
C --- Get value of TTYPE6=NUMZERO (should equal n_col)
      colnum = 6
      CALL FTGCVI ( iunit, colnum, 1, 1, 1, 0, numzero,
     &  logval, status )
C
C --- Set longword version of numzero for certain FITSIO calls
      numzeroj = numzero
C
C --- Read TTYPE7, TTYPE8, TTYPE9 (DIRDRM, SCTDRM, SUMDRM)
      colnum = 7
      CALL FTGCVJ ( iunit, colnum, 1, 1, n_det, 0, matsiz(1,1),
     &  logval, status )
      colnum = 8
      CALL FTGCVJ ( iunit, colnum, 1, 1, n_det, 0, matsiz(1,2),
     &  logval, status )
      colnum = 9
      CALL FTGCVJ ( iunit, colnum, 1, 1, n_det, 0, matsiz(1,3),
     &  logval, status )
C
C --- Determine which matrix type we have (DIR, SCT, or SUM)
      DO i=1,3
          IF (matsiz(1,i) .GT. 0) THEN
              matcol = i+12
              matind = i
          END IF
      END DO
C
C --- Loop over the detectors and sum their response matrices
      DO i=1,n_det
C ---     Get indices of first nonzero element in columns of DRM
C ---     (TTYPE12=N_ZEROS)
          colnum = 12
          CALL FTGCVI ( iunit, colnum, i, 1, numzeroj, 0,
     &      zeros, logval, status )
C
C ---     Read compressed matrix elements from DRM file into vector drm_mat
          CALL FTGCVE ( iunit, matcol, i, 1, matsiz(i,matind), 0,
     &      drm_mat, logval, status )
C
C ---     Now reconstitute the original matrix
          CALL matfill ( n_row, numzero, zeros, drm_mat, mat_in )
C
C ---     Now sum the matrices
          CALL matsum ( n_row*numzeroj, mat_in, matrix )
      END DO
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      matfill
C
C DESCRIPTION:
C      Reconstitutes uncompressed, rectangular matrix from compressed DRM
C      matrix format
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL matfill ( n_row, n_col, zeros, drm_mat, matrix )
C
C ARGUMENTS:
C    Input  ...
C      n_row   - number of rows for output matrix
C      n_col   - number of columns for output matrix
C      zeros   - vector holding number of 0 elements at top of each DRM column
C      drm_mat - vector holding concatenated nonzero elements of entire DRM
C    Output ...
C      matrix  - uncompressed, rectangular DRM
C
C PRIMARY LOCAL VARIABLES:
C      i     - work variable of type integer
C      index - position within drm_mat as elements are transferred into matrix
C      j     - work variable of type integer
C
C CALLED ROUTINES:
C      none
C
C******************************************************************************

      SUBROUTINE matfill ( n_row, n_col, zeros, drm_mat, matrix )
      IMPLICIT NONE
      INTEGER index, n_row
      REAL drm_mat(*), matrix(n_row,*)
      INTEGER i, j
      INTEGER*2 zeros(*), n_col
C
C --- Initialize position index within drm_mat
      index = 1
C
C --- Fill matrix, column by column
      DO j=1,n_col
C ---     Remember, zeros is actually index of first NONZERO element
          DO i=1,zeros(j)-1
              matrix(i,j) = 0.
          END DO
C ---     Now read in the nonzero elements of the column from drm_mat
          DO i=zeros(j),n_row
              matrix(i,j) = drm_mat(index)
              index = index + 1
          END DO
      END DO
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      matchar
C
C DESCRIPTION:
C      Returns RMF grouping parameters and column values N_GRP, F_CHAN, N_CHAN,
C      and MATRIX (TTYPE3-6) for a specified row of the uncompressed,
C      rectangular DRM generated by matfill
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL matchar ( matrix, n_row, i_row, n_col,
C                     mat_row, n_tot, n_grp, f_chan, n_chan )
C
C ARGUMENTS:
C    Input  ...
C      matrix  - uncompressed, rectangular DRM generated by matfill
C      n_row   - used only for variable array dimensioning of matrix
C      i_row   - row of matrix to operate on
C      n_col   - number of columns in matrix
C    Output ...
C      mat_row - vector of nonzero elements for entire row of matrix
C      n_tot   - number of nonzero elements in mat_row (= sum of n_chan's)
C      n_grp   - number of channel subsets for row of matrix
C      f_chan  - first channel in each subset for row of matrix
C      n_chan  - number of channels in each subset for row of matrix
C
C PRIMARY LOCAL VARIABLES:
C      icol   - work variable to loop through columns of matrix
C      in_grp - flag indicating whether or not element is within channel subset
C
C CALLED ROUTINES:
C      none
C
C******************************************************************************

      SUBROUTINE matchar ( matrix, n_row, i_row, n_col,
     &  mat_row, n_tot, n_grp, f_chan, n_chan )
      IMPLICIT NONE
      INTEGER i_row, n_row, i_col
      REAL matrix(n_row,*), mat_row(*)
      INTEGER n_col
      INTEGER n_tot, n_grp
      INTEGER*2 f_chan(*), n_chan(*)
      LOGICAL in_grp
C
      in_grp = .FALSE.
      n_grp = 0
      n_tot = 0
C
C --- Go through row i_row of matrix, element by element
      DO i_col=1,n_col
          IF (in_grp) THEN
              IF (matrix(i_row,i_col) .EQ. 0.) THEN
C ---             Finished a group of nonzero elements
                  in_grp = .FALSE.
              ELSE
C ---             Another nonzero element in current group
                  n_chan(n_grp) = n_chan(n_grp) + 1
                  n_tot = n_tot + 1
                  mat_row(n_tot) = matrix(i_row,i_col)
              END IF
          ELSE IF (matrix(i_row,i_col) .NE. 0.) THEN
C ---         Started a new group of nonzero elements
              in_grp = .TRUE.
              n_grp = n_grp + 1
              n_tot = n_tot + 1
              n_chan(n_grp) = 1
              f_chan(n_grp) = i_col
              mat_row(n_tot) = matrix(i_row,i_col)
          END IF
      END DO
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      matsum
C
C DESCRIPTION:
C      Adds array of elements to a cumulative sum array
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL matsum ( n_elem, in_mat, sum )
C
C ARGUMENTS:
C    Input  ...
C      n_elem - number of array elements to sum
C      in_mat - input array addend
C      sum    - cumulative sum array
C    Output ...
C      sum    - cumulative sum array with in_mat now added to it
C
C PRIMARY LOCAL VARIABLES:
C      i - work variable to loop through elements of arrays
C
C CALLED ROUTINES:
C      none
C
C******************************************************************************

      SUBROUTINE matsum ( n_elem, in_mat, sum )
      IMPLICIT NONE
      REAL in_mat(*), sum(*)
      INTEGER n_elem
      INTEGER i
C
      DO i=1,n_elem
          sum(i) = sum(i) + in_mat(i)
      END DO
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      savhdr
C
C DESCRIPTION:
C      Saves all cards in current HDU into a card image buffer for later recall
C      with cardimg function
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL savhdr ( unit, cbuf, ncard, status )
C
C ARGUMENTS:
C    Input  ...
C      unit   - FITSIO unit number of input file
C    Output ...
C      cbuf   - card image buffer
C      ncard  - number of cards to read into card image buffer from current HDU
C      status - status code
C
C PRIMARY LOCAL VARIABLES:
C      i      - work variable to loop through cards in current HDU
C      intval - work variable of type integer
C
C CALLED ROUTINES:
C      subroutine FTGHSP - Return number of keywords in current HDU
C      subroutine FTGREC - Get header record from HDU
C
C******************************************************************************

      SUBROUTINE savhdr ( unit, cbuf, ncard, status )
      IMPLICIT NONE
      CHARACTER cbuf(*)*(*)
      INTEGER unit, status
      INTEGER intval
      INTEGER ncard, i
C
C --- Find number of keywords in HDU
      CALL FTGHSP ( unit, ncard, intval, status )
C
C --- Read all header records into card image buffer
      DO i = 1, ncard
          CALL FTGREC ( unit, i, cbuf(i), status )
      END DO
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      savcard
C
C DESCRIPTION:
C      Saves a single specified card from the current HDU into a card image
C      buffer for later recall with the cardimg function
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL savcard ( unit, keyword, cbuf, icard, status )
C
C ARGUMENTS:
C    Input  ...
C      unit    - FITSIO unit number of input file
C      keyword - keyword whose header card is to be saved in card image buffer
C      cbuf    - card image buffer
C      icard   - positional index into card image buffer
C    Output ...
C      cbuf    - card image buffer with specified card added into it
C      icard   - updated positional index into card image buffer
C      status  - status code
C
C PRIMARY LOCAL VARIABLES:
C      none
C
C CALLED ROUTINES:
C      subroutine FTGCRD - Get header record for specified keyword from HDU
C
C******************************************************************************

      SUBROUTINE savcard ( unit, keyword, cbuf, icard, status )
      IMPLICIT NONE
      CHARACTER cbuf(*)*(*)
      CHARACTER keyword*(*)
      INTEGER unit, status
      INTEGER icard
C
C --- Increment pointer into the card image buffer
      icard = icard + 1
C
C --- Add specified header record to the card image buffer
      CALL FTGCRD ( unit, keyword, cbuf(icard), status )
C
      RETURN
      END


C******************************************************************************
C FUNCTION:
C      detnam_brmf
C
C DESCRIPTION:
C      Generates DETNAM keyword from the DRM DET_MODE keyword and DET_NUM
C      column
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      [result] = detnam_brmf ( iunit )
C
C ARGUMENTS:
C    Input  ...
C      iunit    - FITSIO unit number for input file
C    Output ...
C      [result] - sub-instrument identifier showing specific detector(s) used
C
C PRIMARY LOCAL VARIABLES:
C      chdu   - position index of initial HDU
C      cment  - buffer to hold comment portion of FITS card
C      colnum - column number of FITS binary table
C      detflg - vector of positional flags indicating detectors used
C      detpos - position index within detnam_brmf string
C      dets   - vector holding detector numbers used
C      i      - work variable of type integer
C      ihdu   - position index of desired HDU
C      intval - work variable of type integer
C      logval - work variable of type logical
C      ndet   - number of detectors used
C      status - status code
C
C CALLED ROUTINES:
C      subroutine FTGCVI    - Get elements of binary table integer column
C      subroutine FTGHDN    - Get number of current HDU
C      subroutine FTGKY|J/S - Get keyword of specified data type
C      subroutine FTMAHD    - Move to specified absolute HDU
C
C******************************************************************************

      CHARACTER*(*) FUNCTION detnam_brmf ( iunit )
      IMPLICIT NONE
      CHARACTER cment*70
      INTEGER*2 dets(8)
      INTEGER chdu, ihdu
      INTEGER iunit, colnum, status
      INTEGER ndet, detpos
      INTEGER intval
      INTEGER i
      LOGICAL detflg(8)
      LOGICAL logval

      status=0
C
      CALL FTGKYS ( iunit, 'DET_MODE', detnam_brmf, cment, status )
C
C --- Save current location in input file
      CALL FTGHDN ( iunit, chdu )
C
C --- Go to the data extension, which has the DET_NUM column
      ihdu = 2
      CALL FTMAHD ( iunit, ihdu, intval, status )
C
C --- Determine the number of detectors used
      CALL FTGKYJ ( iunit, 'NAXIS2', ndet, cment, status )
C
C --- Read the detector numbers used
      colnum = 1
      DO i = 1, ndet
          CALL FTGCVI ( iunit, colnum, i, 1, 1, 0,
     &      dets(i), logval, status )
      END DO
C
C --- Blank out detflg
      DO i = 1, 8
          detflg(i) = .FALSE.
      END DO
C
C --- Now sort the detectors in order
      DO i = 1, ndet
          detflg(dets(i)+1) = .TRUE.
      END DO
C
      detpos = INDEX(detnam_brmf,' ')
      detnam_brmf(detpos:detpos) = '-'
C
C --- Now create the DETNAM keyword
      DO i = 1, 8
          IF (detflg(i)) THEN
              detpos = detpos+1
              WRITE ( detnam_brmf(detpos:detpos), '(I1)' ) i-1
          END IF
      END DO
C
C --- Return to saved location in input file
      CALL FTMAHD ( iunit, chdu, intval, status )
C
      RETURN
      END


C******************************************************************************
C FUNCTION:
C      cardimg
C
C DESCRIPTION:
C      Finds and returns card image stored in a card image buffer that has
C      initial characters matching a specified string
C
C AUTHOR/DATE:
C      R. Nakatsuka / 12 May 1995
C
C MODIFICATION HISTORY:
C      12 May 1995  RN   Original version
C
C NOTES:
C      If matching card image is not found in buffer, cardimg returns ' '
C
C USAGE:
C      [result] = cardimg ( string, cbuf, ncard )
C
C ARGUMENTS:
C    Input  ...
C      string   - string for which matching card image is desired
C      cbuf     - the card image buffer to search
C      ncard    - highest card number to search in card image buffer
C    Output ...
C      [result] - card image beginning with the specified search string
C
C PRIMARY LOCAL VARIABLES:
C      i - work variable to increment through search of card image buffer
C
C CALLED ROUTINES:
C      none
C
C******************************************************************************

      CHARACTER*(*) FUNCTION cardimg ( string, cbuf, ncard )
      IMPLICIT NONE
      CHARACTER cbuf(*)*(*)
      CHARACTER string*(*)
      INTEGER ncard, i
C
      i = 1
C
      DO WHILE ( cbuf(i)(1:LEN(string)) .NE. string )
          i = i + 1
          IF ( i .GT. ncard ) THEN
              cardimg = ' '
              GO TO 999
          END IF
      END DO
C
      cardimg = cbuf(i)
C
  999 CONTINUE
      RETURN
      END
