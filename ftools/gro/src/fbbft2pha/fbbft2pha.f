C******************************************************************************
C FTOOL TASK:
C      fbbft2pha
C
C FILE:
C      fbbft2pha.f
C
C FTOOL USAGE:
C      fbbft2pha bftfile phafile
C
C DESCRIPTION:
C      Main FORTRAN routine for FTOOL to convert Compton Gamma Ray Observatory
C      (CGRO) Burst and Transient Source Experiment (BATSE) BFITS files into
C      Office of Guest Investigator Programs (OGIP) standard FITS format "Pulse
C      Height Analyzer" (PHA) output files, compatible with XSPEC x-ray
C      spectral fitting package
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   v1.0 - Original version
C      01 Jul 1998  NG   v1.1 - cosmetic changes for y2k.
C
C NOTES:
C      Returns status code 0 for successful completion, nonzero for error.
C
C      Software assumes that user will generate a corresponding Redistribution
C      Matrix File (RMF) from the associated BATSE Detector Response Matrix
C      (DRM) file, using the FTOOL fdrm2rmf.  RMF file is assumed to have same
C      name as PHA file, but with matching-case extension, .rmf or .RMF.  Name
C      of RMF file is written into keyword RESPFILE.
C
C   +--------------------------------------------------------------
C   |
C   |  Outline of BATSE PHA FITS file format:
C   |
C   |      Primary  HDU:  Null data array
C   |
C   |      Extension #1:  BINTABLE  'SPECTRUM'
C   |                     Col    TTYPE       TFORM    TUNIT
C   |                     ^^^    ^^^^^^^^    ^^^^^    ^^^^^^^^
C   |                       1    SPEC_NUM    1I       none
C   |                       2    CHANNEL     nI       chan
C   |                       3    TSTART      1E       s
C   |                       4    TSTOP       1E       s
C   |                       5    RATE        nE       count /s
C   |                       6    STAT_ERR    nE       count /s
C   |                     where n is number of spectral channels
C   |
C   +--------------------------------------------------------------
C
C MAKE:
C      make fbbft2pha
C
C USAGE:
C      [result] = fbbft2pha ( )
C
C ARGUMENTS:
C    Input  ...  none
C    Output ...
C      [result] - status code
C
C PRIMARY LOCAL VARIABLES:
C      bftfile  - name of input BATSE BFITS file
C      phafile  - name of output PHA file
C      status   - status code
C      taskname - FTOOL identifier
C
C CALLED ROUTINES:
C      subroutine convert_bbft - performs file conversion
C      subroutine getfilenames - gets filename parameters from environment
C      subroutine FTCMSG       - clears FITSIO error message stack
C
C******************************************************************************

      INTEGER FUNCTION fbbfta ( )
      IMPLICIT NONE
      character(180) bftfile, phafile
      CHARACTER taskname*40
      INTEGER status
      COMMON /TASK/ taskname

      status=0
C
C --- Set function return value to error until completion
      fbbfta = -1
C
      taskname = 'fbbft2pha v1.0'
      CALL FTCMSG
C
C --- Get parameters from .par file
      CALL getfilenames ( 'bftfile', 'phafile',
     &  bftfile, phafile, status )
C --- Now do everything else
      CALL convert_bbft ( bftfile, phafile, status )
C
C --- Set function return value to status code
      fbbfta = status
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      convert_bbft
C
C DESCRIPTION:
C      creates PHA file from BATSE BFITS file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C      This is the routine that will normally detect erroneous attempts to
C      process input files that are not in FITS format, because in such
C      cases the FTOPEN request will fail and return an error status code.
C
C USAGE:
C      CALL convert_bbft ( bftfile, phafile, status )
C
C ARGUMENTS:
C    Input  ...
C      bftfile - name of input BATSE BFITS file
C      phafile - name of output PHA file
C      status  - status code
C    Output ...  none
C
C PRIMARY LOCAL VARIABLES:
C      blocksize - FITSIO logical record blocking factor
C      errors    - array of statistical errors, in units of count rate
C      intstat   - interim status code
C      iunit     - FITSIO unit number for input file
C      lchan     - lower channel number (from LO_CHAN keyword)
C      n_spec    - number of spectra in file
C      ounit     - FITSIO unit number for output file
C      rates     - array of measured count rates
C      rwmode    - FITSIO read/write access mode for input file
C      tstart    - array of spectra start times
C      tstop     - array of spectra stop times
C      uchan     - upper channel number (from UP_CHAN keyword)
C      MAX_CHAN  - maximum possible number of spectral channels
C      MAX_SPEC  - maximum number of spectra in BFITS input file
C
C CALLED ROUTINES:
C      subroutine hdu0_hdr_bpha  - write primary HDU in output file
C      subroutine make_hdu1_data - write data of extension 1 in output file
C      subroutine make_hdu1_hdr  - write header of extension 1 in output file
C      subroutine FCERRM         - display FITSIO error status & message stack
C      subroutine FTCLOS         - close FITS file
C      subroutine FTFIOU         - deallocate I/O unit number(s)
C      subroutine FTGIOU         - get unused I/O unit number
C      subroutine FTINIT         - open and initialize new empty FITS file
C      subroutine FTOPEN         - open existing FITS file
C
C******************************************************************************

      SUBROUTINE convert_bbft ( bftfile, phafile, status )
      IMPLICIT NONE
      CHARACTER*(*) bftfile, phafile
      INTEGER MAX_CHAN, MAX_SPEC
      PARAMETER ( MAX_CHAN = 252, MAX_SPEC = 10000 )
      REAL rates(MAX_CHAN*MAX_SPEC), errors(MAX_CHAN*MAX_SPEC)
      REAL tstart(MAX_SPEC), tstop(MAX_SPEC)
      INTEGER iunit, ounit, lchan, uchan, n_spec
      INTEGER rwmode, blocksize, intstat, status
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Assign input and output unit numbers
      CALL FTGIOU ( iunit, status )
      CALL FTGIOU ( ounit, status )
C
C --- Open the input BFITS file, with readonly access
      rwmode = 0
      CALL FTOPEN ( iunit, bftfile, rwmode, blocksize, status )
C
C --- Create the new empty output PHA file, with standard block size
      blocksize = 1
      CALL FTINIT ( ounit, phafile, blocksize, status )
C
C --- Generate primary HDU of the output PHA file
      CALL hdu0_hdr_bpha ( iunit, bftfile, ounit, phafile, status )
C
C --- Generate header of first extension HDU of the output PHA file
      CALL make_hdu1_hdr ( iunit, bftfile, ounit, phafile,
     &  n_spec, lchan, uchan, tstart, tstop, rates, errors, status )
C
C --- Generate data table of first extension HDU of the output PHA file
      CALL make_hdu1_data ( ounit,
     &  n_spec, lchan, uchan, tstart, tstop, rates, errors, status )
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
C      hdu0_hdr_bpha
C
C DESCRIPTION:
C      Generates primary HDU of the output PHA file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C      This is the routine that will normally detect erroneous attempts to
C      process input FITS files that are not in the BATSE BFITS format.
C      Often this will occur when attempting to delete the FILETYPE keyword,
C      since this keyword often differentiates BATSE BFITS files from other
C      FITS file types.  If a BATSE DRM file is provided as input, this
C      routine will detect an error when attempting to modify the name of
C      the SRCE-AZ keyword, whose presence is one feature that can be used
C      to differentiate BATSE BFITS files from BATSE DRM files.
C
C USAGE:
C      CALL hdu0_hdr_bpha ( iunit, bftfile, ounit, phafile, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit   - FITSIO unit number for input file
C      bftfile - name of input BATSE BFITS file
C      ounit   - FITSIO unit number for output file
C      phafile - name of output PHA file
C    Output ...
C      status  - status code
C
C PRIMARY LOCAL VARIABLES:
C      cment    - buffer to hold comment portion of FITS card
C      ext      - flag to indicate whether fname should return file extension
C      file     - buffer to hold name of BATSE BFITS input file
C      ihdu     - position index of desired HDU
C      inkey    - name of keyword to be changed
C      intval   - work variable of type integer
C      morekeys - number of keywords to add to original HDU
C      outkey   - name of new keyword to put in output PHA flie
C      taskname - FTOOL identifier
C      FILE_VER - version number of PHA file
C      HDUVERS  - version number of file format document
C      PHAVERSN - OGIP version number of FITS format
C
C CALLED ROUTINES:
C      subroutine chcard  - Transfer date or time keyword with format change
C      function   ddmmyy  - Convert date from yyyy.ddd to yyyy-mm-dd format
C      function   detnam_bpha - Form DETNAM keyword from component keywords
C      subroutine e_keys  - Write energy characterization keywords
C      function   fname   - Extract filename from full file specification
C      function   hhmmss  - Convert seconds-of-day to time-of-day format
C      subroutine xferkey - Transfer keyword record from input to output file
C      subroutine FTCOPY  - Copy entire HDU from input to output file
C      subroutine FTDKEY  - Delete existing keyword record
C      subroutine FTMAHD  - Move to specified absolute HDU
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

      SUBROUTINE hdu0_hdr_bpha ( iunit, bftfile, ounit, phafile,
     &  status )
      IMPLICIT NONE
      EXTERNAL ddmmyy, hhmmss
C      character(12) ddmmyy, hhmmss
       character(68) ddmmyy, hhmmss
      CHARACTER*(*) bftfile, phafile
      character(180) fname, file
      CHARACTER cment*70
      CHARACTER taskname*40
      CHARACTER detnam_bpha*12
      character(8) inkey, outkey
      character(8) HDUVERS, PHAVERSN, FILE_VER
      PARAMETER ( HDUVERS  = '1.0.0' )
      PARAMETER ( PHAVERSN = '1992a' )
      PARAMETER ( FILE_VER = '1.0'   )
      INTEGER status, iunit, ounit
      INTEGER morekeys
      INTEGER ihdu, intval
      LOGICAL ext
      COMMON /TASK/ taskname
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Copy the primary HDU from the input file to the output file
      morekeys = 27
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
C --- Delete the FILETYPE keyword inherited from the input BFITS file
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
C      cment = 'Start date of data (dd/mm/yy)'
      cment = 'Start date of data (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment,
     &  status )
C
      inkey = 'STRT-TIM'
      outkey = 'TIME-OBS'
      cment = 'Start time of data (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment,
     &  status )
C
      inkey = 'END-DAY'
      outkey = 'DATE-END'
c      cment = 'End date of data (dd/mm/yy)'
      cment = 'End date of data (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment,
     &  status )
C
      inkey = 'END-TIM'
      outkey = 'TIME-END'
      cment = 'End time of data (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment,
     &  status )
C
      inkey = 'TRIG-DAY'
      outkey = 'TRIG_DAY'
C      cment = 'Date of burst trigger (dd/mm/yy)'
      cment = ' Date of burst trigger (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment,
     &  status )
C
      inkey = 'TRIG-TIM'
      outkey = 'TRIG_TIM'
      cment = 'Time of burst trigger (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment,
     &  status )
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
      CALL FTMNAM ( ounit, 'SRCE-AZ',  'SRCE_AZ',  status )
      CALL FTMNAM ( ounit, 'SRCE-EL',  'SRCE_EL',  status )
      CALL FTMNAM ( ounit, 'GEOC-AZ',  'GEOC_AZ',  status )
      CALL FTMNAM ( ounit, 'GEOC-EL',  'GEOC_EL',  status )
C
C --- Rename and modify the FILE-ID keyword
      CALL FTMNAM ( ounit, 'FILE-ID', 'FILE_ID', status )
      ext = .TRUE.
      file = fname ( phafile, ext )
      CALL FTMKYS ( ounit, 'FILE_ID', file, '&', status )
C
C --- Rename and modify the FILE-VER keyword
      CALL FTMNAM ( ounit, 'FILE-VER', 'FILE_VER', status )
      CALL FTMKYS ( ounit, 'FILE_VER', FILE_VER, '&', status )
C
C --- Update the FITS file creation date keyword
      CALL FTPDAT ( ounit, status )
C
      CALL FTMNAM ( ounit, 'MNEMONIC', 'CREATOR', status )
      CALL FTMCOM ( ounit, 'CREATOR',
     &  'Program that created BATSE BFITS input file', status )
C
C --- Get and write the energy parameters E_MIN, E_MAX, EUNIT
      CALL e_keys ( iunit, ounit, status )
C
C --- Go to the BFITS data table extension (Extension #2)
      ihdu = 3
      CALL FTMAHD ( iunit, ihdu, intval, status )
C
C --- Transfer various keyword records with renaming
      CALL xferkey ( iunit, 'LO_CHAN', ounit, 'CHANMIN', status )
      CALL xferkey ( iunit, 'UP_CHAN', ounit, 'CHANMAX', status )
C
C --- Write the DETNAM keyword record
      CALL FTPKYS ( ounit, 'DETNAM', detnam_bpha(iunit),
     &  'Sub-instrument identifier label', status )
C
C --- Write input BFITS file name keyword
      ext = .TRUE.
      file = fname ( bftfile, ext )
      CALL FTPKYS ( ounit, 'BFITSFIL', file,
     &  'Name of BATSE BFITS input file', status )
C
C --- Go to the BFITS primary HDU
      ihdu = 1
      CALL FTMAHD ( iunit, ihdu, intval, status )
C
C --- Transfer FILE-ID keyword record, renamed to BFITSORI
      CALL xferkey ( iunit, 'FILE-ID', ounit, 'BFITSORI', status )
      CALL FTMCOM ( ounit, 'BFITSORI',
     &  'Original name of BATSE BFITS input file', status )
C
C --- Write FTOOL identifier keyword
      CALL FTPKYS ( ounit, 'FTOOL', taskname,
     &  'FTOOL identifier label', status )
C
C --- Write HISTORY keyword
      cment = 'BATSE BFITS file reworked by FTOOL ' // taskname
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
C --- Write block of descriptive comments specific to BATSE PHA files
      cment = '-----------------------------------'
     &     // '-----------------------------------'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'This file is a derived data product '
     &     // 'of the CGRO BATSE instrument.'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'It consists of data from a '
     &     // 'BATSE BFITS file, reorganized for'
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
      cment = 'Name of BFITS input file                  BFITSFIL'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Original name of BFITS input file         BFITSORI'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Software that generated BFITS file        CREATOR'
      CALL FTPCOM ( ounit, cment, status )
      cment = 'Name of corresponding RMF file            RESPFILE'
      CALL FTPCOM ( ounit, cment, status )
      cment = '-----------------------------------'
     &     // '-----------------------------------'
      CALL FTPCOM ( ounit, cment, status )
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      make_hdu1_hdr
C
C DESCRIPTION:
C      Generates header of first extension HDU of the output PHA file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL make_hdu1_hdr ( iunit, bftfile, ounit, phafile,
C                           n_spec, lchan, uchan,
C                           tstart, tstop, rates, errors, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit   - FITSIO unit number for input file
C      bftfile - name of input BATSE BFITS file
C      ounit   - FITSIO unit number for output file
C      phafile - name of output PHA file
C    Output ...
C      n_spec  - number of spectra in file
C      lchan   - lower channel number (from LO_CHAN keyword)
C      uchan   - upper channel number (from UP_CHAN keyword)
C      tstart  - array of spectra start times
C      tstop   - array of spectra stop times
C      rates   - array of measured count rates
C      errors  - array of statistical errors, in units of count rate
C      status  - status code
C
C PRIMARY LOCAL VARIABLES:
C      cment    - buffer to hold comment portion of FITS card
C      decimals - number of decimal places to write with FTPKYE
C      exposure - total duration of observation, in seconds
C      ext      - flag to indicate whether fname should return file extension
C      file     - buffer to hold variants of file names
C      i_chan   - spectral channel number
C      i_spec   - index number of spectrum in FITS file
C      ihdu     - position index of desired HDU
C      inkey    - name of keyword to be changed
C      intval   - work variable of type integer
C      logval   - work variable of type logical
C      n_chan   - number of spectral channels measured
C      outkey   - name of new keyword to put in output PHA flie
C      numform  - character buffer used in generating TFORM keywords
C      taskname - FTOOL identifier
C      tform    - data types of FITS table columns, for TFORM keywords
C      ttype    - names of FITS table columns, for TTYPE keywords
C      tunit    - physical units of FITS table columns, for TUNIT keywords
C      HDUVERS  - version number of file format document
C      PHAVERSN - OGIP version number of FITS format
C      TFIELDS  - Number of FITS table columns
C
C CALLED ROUTINES:
C      subroutine chcard    - Transfer date or time keyword with format change
C      function   ddmmyy    - Convert date from yyyy.ddd to dd/mm/yy format
C      function   detnam_bpha - Form DETNAM keyword from component keywords
C      subroutine e_keys    - Write energy characterization keywords
C      function   fname     - Extract filename from full file specification
C      function   hhmmss    - Convert seconds-of-day to time-of-day format
C      subroutine read_bbft - Get selected data and keywords from input file
C      subroutine xferkey   - Transfer keyword record from input to output file
C      subroutine FTCRHD    - Append new, empty HDU
C      subroutine FTGKYL    - Get keyword of logical type
C      subroutine FTMAHD    - Move to specified absolute HDU
C      subroutine FTMCOM    - Modify comment field of existing keyword
C      subroutine FTPDAT    - Append/update DATE keyword
C      subroutine FTPHBN    - Write principal binary table header keywords
C      subroutine FTPHIS    - Append HISTORY keyword
C      subroutine FTPKY|E/F/J/S - Append keyword of specified data type
C      subroutine FTRDEF    - Reinitialize structure of HDU
C
C******************************************************************************

      SUBROUTINE make_hdu1_hdr ( iunit, bftfile, ounit, phafile,
     &  n_spec, lchan, uchan, tstart, tstop, rates, errors, status )
      IMPLICIT NONE
      EXTERNAL ddmmyy, hhmmss
      INTEGER TFIELDS
      PARAMETER ( TFIELDS = 6 )
      character(16) ttype(TFIELDS), tform(TFIELDS), tunit(TFIELDS)
      DATA ttype
     &         / 'SPEC_NUM', 'CHANNEL' , 'TSTART', 'TSTOP',
     &           'RATE'    , 'STAT_ERR' /
      DATA tform
     &         / '1I'      , ' '       , '1E'    , '1E'   ,
     &           ' '       , ' '        /
      DATA tunit
     &         / 'none'    , 'chan'    , 's'     , 's'    ,
     &           'count /s', 'count /s' /
      CHARACTER*(*) bftfile, phafile
      CHARACTER fname*180, file*180
      CHARACTER cment*70
      CHARACTER taskname*40
      CHARACTER detnam_bpha*12, numform*8
C      character(12) ddmmyy, hhmmss
      character(68) ddmmyy, hhmmss
      character(8) inkey, outkey
      character(8) HDUVERS, PHAVERSN
      PARAMETER ( HDUVERS  = '1.0.0' )
      PARAMETER ( PHAVERSN = '1992a' )
      REAL tstart(*), tstop(*), rates(*), errors(*)
      REAL exposure
      INTEGER iunit, ounit, status
      INTEGER lchan, uchan, n_chan
      INTEGER i_chan, i_spec, n_spec, decimals, ihdu
      INTEGER intval
      LOGICAL ext, logval
      COMMON /TASK/ taskname
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Read and pass back data, including TIMES (TTYPE1), RATES (TTYPE2), and
C --- ERRORS (TTYPE3)
      CALL read_bbft ( iunit, n_spec, lchan, uchan,
     &  tstart, tstop, rates, errors, status )
C
      n_chan = uchan - lchan + 1
C
C --- Generate TFORM2, TFORM5, TFORM6 keywords with appropriate number of
C --- elements
      WRITE (numform, '(I8)') n_chan
c MJT 09July96 changing JINT/FLOATJ to INT/FLOAT (for linux/g77)
c     tform(2) = numform ( 8 - JINT(ALOG10(FLOATJ(n_chan))) : 8 ) // 'I'
      tform(2) = numform ( 8 - INT(ALOG10(FLOAT(n_chan))) : 8 ) // 'I'
      tform(5) = tform(2)(1:INDEX(tform(2),'I')-1) // 'E'
      tform(6) = tform(5)
C
C --- Append new, empty HDU at end of output PHA file
      CALL FTCRHD ( ounit, status )
C --- Insert mandatory keywords into binary table header of output PHA file
      CALL FTPHBN ( ounit, n_spec, TFIELDS, ttype, tform, tunit,
     &  'SPECTRUM', 0, status )
C --- Define structure of binary table
      CALL FTRDEF ( ounit, status )
C
C --- Write the EXTVER keyword record
      intval = 1
      CALL FTPKYJ ( ounit, 'EXTVER', intval,
     &  'Version of this extension type', status )
C
C --- Write null keywords indicating absence of various columns in binary table
      intval = 0
      CALL FTPKYJ ( ounit, 'SYS_ERR', intval,
     &  'No SYS_ERR column in binary table', status )
      CALL FTPKYJ ( ounit, 'QUALITY', intval,
     &  'No QUALITY column in binary table', status )
      CALL FTPKYJ ( ounit, 'GROUPING', intval,
     &  'No GROUPING column in binary table', status )
C
C --- Transfer various keyword records with possible renaming
      CALL xferkey ( iunit, 'DSELECT', ounit, 'DSELECT', status )
      CALL xferkey ( iunit, 'LO_CHAN', ounit, 'CHANMIN', status )
      CALL xferkey ( iunit, 'UP_CHAN', ounit, 'CHANMAX', status )
      CALL xferkey ( iunit, 'RF_511',  ounit, 'RF_511',  status )
      CALL xferkey ( iunit, 'R_EXP',   ounit, 'R_EXP',   status )
C
C --- Write the DETNAM keyword record
      CALL FTPKYS ( ounit, 'DETNAM', detnam_bpha(iunit),
     &  'Sub-instrument identifier label', status )
C
C --- Transfer various keyword records with possible renaming
      CALL xferkey ( iunit, 'DATATYPE', ounit, 'DATATYPE', status )
      CALL xferkey ( iunit, 'IS_SPEC',  ounit, 'IS_SPEC',  status )
      CALL xferkey ( iunit, 'IS_ERROR', ounit, 'IS_ERROR', status )
      CALL xferkey ( iunit, 'OVERFLW',  ounit, 'OVERFLW',  status )
      CALL xferkey ( iunit, 'LTIMECOR', ounit, 'LTIMECOR', status )
      CALL xferkey ( iunit, 'BCKGSUBT', ounit, 'BCKGSUBT', status )
      CALL xferkey ( iunit, 'BSTACC',   ounit, 'BSTACC',   status )
C
C --- Write TIMEZERO, TIMESYS, TIMEUNIT, TIMEREF, TASSIGN keywords
      CALL xferkey ( iunit, 'BASETIME', ounit, 'TIMEZERO', status )
      CALL FTMCOM ( ounit, 'TIMEZERO', 'Trigger time', status )
      CALL FTPKYS ( ounit, 'TIMESYS',  'TJD',
     &  'Time frame system for TIMEZERO keyword', status )
      CALL FTPKYS ( ounit, 'TIMEUNIT', 'd',
     &  'Time unit for TIMEZERO keyword', status )
      CALL FTPKYS ( ounit, 'TIMEREF',  'LOCAL',
     &  'Frame of reference for times', status )
      CALL FTPKYS ( ounit, 'TASSIGN',  'SATELLITE',
     &  'Where time assignment was performed', status )
C
C --- Transfer BASETIME keyword record
      CALL xferkey ( iunit, 'BASETIME', ounit, 'BASETIME', status )
C
C --- Write TELESCOP, INSTRUME, ORIGIN keywords
      CALL FTPKYS ( ounit, 'TELESCOP', 'CGRO',
     &  'Mission identifier label', status )
      CALL FTPKYS ( ounit, 'INSTRUME', 'BATSE',
     &  'Instrument identifier label', status )
      CALL FTPKYS ( ounit, 'ORIGIN', 'NASA/GSFC/COSSC',
     &  'Institution that produced this dataset', status )
C
C --- Go back to Primary HDU in BFITS input file for additional keywords.
      ihdu = 1
      CALL FTMAHD ( iunit, ihdu, intval, status )
C
C --- Transfer various keyword records with possible renaming
      CALL xferkey ( iunit, 'OBJECT',   ounit, 'OBJECT',   status )
      CALL xferkey ( iunit, 'BATSE_TR', ounit, 'BATSE_TR', status )
      CALL xferkey ( iunit, 'OBSERVER', ounit, 'OBSERVER', status )
C
C --- Rename and convert format of some date/time keywords:
C --- STRT-DAY -> DATE-OBS
C --- STRT-TIM -> TIME-OBS
C --- END-DAY  -> DATE-END
C --- END-TIM  -> TIME-END
C
      inkey = 'STRT-DAY'
      outkey = 'DATE-OBS'
C      cment  = 'Start date of data (dd/mm/yy)'
      cment  = 'Start date of data (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment,
     &  status )
C
      inkey = 'STRT-TIM'
      outkey = 'TIME-OBS'
      cment  = 'Start time of data (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment,
     &  status )
C
      inkey = 'END-DAY'
      outkey = 'DATE-END'
C      cment  = 'End date of data (dd/mm/yy)'
      cment  = 'End date of data (yyyy-mm-dd)'
      CALL chcard ( iunit, inkey, ounit, outkey, ddmmyy, cment,
     &  status )
C
      inkey = 'END-TIM'
      outkey = 'TIME-END'
      cment  = 'End time of data (hh:mm:ss.sss)'
      CALL chcard ( iunit, inkey, ounit, outkey, hhmmss, cment,
     &  status )
C
C --- Transfer various keyword records with possible renaming
      CALL xferkey ( iunit, 'EQUINOX' , ounit, 'EQUINOX', status )
      CALL xferkey ( iunit, 'SC-Z-RA' , ounit, 'RA_SCZ' , status )
      CALL xferkey ( iunit, 'SC-Z-DEC', ounit, 'DEC_SCZ', status )
      CALL xferkey ( iunit, 'SC-X-RA' , ounit, 'RA_SCX' , status )
      CALL xferkey ( iunit, 'SC-X-DEC', ounit, 'DEC_SCX', status )
      CALL xferkey ( iunit, 'OBJCTRA' , ounit, 'RA_OBJ' , status )
      CALL xferkey ( iunit, 'OBJCTDEC', ounit, 'DEC_OBJ', status )
C
C --- Write FITS file creation date keyword
      CALL FTPDAT ( ounit, status )
C
C --- Transfer MNEMONIC keyword record to CREATOR record
      CALL xferkey ( iunit, 'MNEMONIC', ounit, 'CREATOR', status )
      CALL FTMCOM ( ounit, 'CREATOR',
     &  'Program that created BATSE BFITS input file', status )
C
C --- Write input BFITS file name keyword
      ext = .TRUE.
      file = fname ( bftfile, ext )
      CALL FTPKYS ( ounit, 'BFITSFIL', file,
     &  'Name of BATSE BFITS input file', status )
C
C --- Transfer FILE-ID keyword record, renamed to BFITSORI
      CALL xferkey ( iunit, 'FILE-ID', ounit, 'BFITSORI', status )
      CALL FTMCOM ( ounit, 'BFITSORI',
     &  'Original name of BATSE BFITS input file', status )
C
C --- Write FTOOL identifier keyword
      CALL FTPKYS ( ounit, 'FTOOL', taskname,
     &  'FTOOL identifier label', status )
C
C --- Write HISTORY keyword
      cment = 'BATSE BFITS file reworked by FTOOL ' // taskname
      CALL FTPHIS ( ounit, cment, status )
C
C --- Write FILTER, HDUCLASS, HDUDOC, HDUVERS, HDUCLAS1 keywords
      CALL FTPKYS ( ounit, 'FILTER', 'none',
     &  'No filter for BATSE instrument', status )
      CALL FTPKYS ( ounit, 'HDUCLASS', 'OGIP',
     &  'File format origin', status )
      CALL FTPKYS ( ounit, 'HDUDOC',
     &  'Arnaud et al. 1992, Legacy 2, p. 65.',
     &  'Format specification', status )
      CALL FTPKYS ( ounit, 'HDUVERS', HDUVERS,
     &  'Version of format', status )
      CALL FTPKYS ( ounit, 'HDUCLAS1', 'SPECTRUM',
     &  'Extension header contains spectral data', status )
C
C --- Go to data table extension to get BCKGSUBT
      ihdu = 3
      CALL FTMAHD ( iunit, ihdu, intval, status )
C
      CALL FTGKYL ( iunit, 'BCKGSUBT', logval, cment, status )
C
C --- Write the appropriate HDUCLAS2 keyword, depending on BCKGSUBT
      IF ( logval ) THEN
          CALL FTPKYS ( ounit, 'HDUCLAS2', 'NET',
     &      'Background-subtracted spectrum', status )
      ELSE
          CALL FTPKYS ( ounit, 'HDUCLAS2', 'TOTAL',
     &      'Gross spectrum (source + background)', status )
      END IF
C
C --- Write HDUCLAS3, RADECSYS keywords
C
      CALL FTPKYS ( ounit, 'HDUCLAS3', 'RATE',
     &  'Data stored as rate in units of [count /s]', status )
      CALL FTPKYS ( ounit, 'RADECSYS', 'FK5',
     &  'Coordinate frame used for EQUINOX', status )
C
C --- Get and write the energy parameters E_MIN, E_MAX, EUNIT
      CALL e_keys ( iunit, ounit, status )
C
C --- Compute EXPOSURE keyword
      exposure = 0.
      DO i_spec = 1, n_spec
          DO i_chan = 1, n_chan
              IF ( errors((i_spec-1)*n_chan+i_chan) .GT. 0. ) THEN
                  exposure = exposure +
     &            rates ((i_spec-1)*n_chan+i_chan) /
     &            errors((i_spec-1)*n_chan+i_chan) ** 2
                  GO TO 110
              END IF
          END DO
  110     CONTINUE
      END DO
C
C --- Write EXPOSURE keyword
      CALL FTPKYF ( ounit, 'EXPOSURE', exposure, 3,
     &  'Corrected integration time [s]', status )
C
C --- Write AREASCAL, BACKSCAL, CORRSCAL, BACKFILE, CORRFILE keywords
C
      decimals = 4
      CALL FTPKYE ( ounit, 'AREASCAL', 1., decimals,
     &  'Area scaling factor', status )
      CALL FTPKYE ( ounit, 'BACKSCAL', 1., decimals,
     &  'Background scaling factor', status )
      CALL FTPKYE ( ounit, 'CORRSCAL', 0., decimals,
     &  'Correction scaling factor', status )
      CALL FTPKYS ( ounit, 'BACKFILE', 'none',
     &  'Name of background file', status )
      CALL FTPKYS ( ounit, 'CORRFILE', 'none',
     &  'Name of correction file', status )
C
C --- Generate and write RESPFILE keyword
      ext = .FALSE.
      file = fname ( phafile, ext )
C
      IF ( LGT (file, '_') ) THEN
          file = file(:INDEX(file,' ')-1) // '.rmf'
      ELSE
          file = file(:INDEX(file,' ')-1) // '.RMF'
      END IF
C
      CALL FTPKYS ( ounit, 'RESPFILE', file,
     &  'Name of redistribution matrix file', status )
C
C --- Write ANCRFILE, CHANTYPE, DETCHANS, PHAVERSN keywords
C
      CALL FTPKYS ( ounit, 'ANCRFILE', 'none',
     &  'Name of ancillary response file', status )
      CALL FTPKYS ( ounit, 'CHANTYPE', 'PHA',
     &  'Channel correction type', status )
      CALL FTPKYJ ( ounit, 'DETCHANS', n_chan,
     &  'Total number of detector channels', status )
      CALL FTPKYS ( ounit, 'PHAVERSN', PHAVERSN,
     &  'OGIP version number of FITS format', status )
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      make_hdu1_data
C
C DESCRIPTION:
C      Generates data table of first extension HDU of the output PHA file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL make_hdu1_data ( ounit, n_spec, lchan, uchan,
C                            tstart, tstop, rates, errors, status )
C
C ARGUMENTS:
C    Input  ...
C      ounit  - FITSIO unit number for output file
C      n_spec - number of spectra in file
C      lchan  - lower channel number (from LO_CHAN keyword)
C      uchan  - upper channel number (from UP_CHAN keyword)
C      tstart - array of spectra start times
C      tstop  - array of spectra stop times
C      rates  - array of measured count rates
C      errors - array of statistical errors, in units of count rate
C    Output ...
C      status - status code
C
C PRIMARY LOCAL VARIABLES:
C      chan_num - vector of channel numbers in observed spectrum
C      colnum   - column number of FITS binary table
C      i        - work variable of type integer
C      i_chan   - spectral channel number
C      i_spec   - index number of spectrum in FITS file
C      n_chan   - number of spectral channels measured
C      n_elem   - number of data elements to write to binary table column
C      MAX_CHAN - maximum possible number of spectral channels
C
C CALLED ROUTINES:
C      subroutine FTPCL|E/J - Write column elements of specified data type
C
C******************************************************************************

      SUBROUTINE make_hdu1_data ( ounit, n_spec, lchan, uchan,
     &  tstart, tstop, rates, errors, status )
      IMPLICIT NONE
      INTEGER MAX_CHAN
      PARAMETER ( MAX_CHAN = 252 )
      INTEGER chan_num(MAX_CHAN)
      REAL tstart(*), tstop(*), rates(*), errors(*)
      INTEGER i_chan, lchan, uchan, i_spec, n_spec, n_chan
      INTEGER n_elem, i
      INTEGER ounit, colnum, status
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
      n_chan = uchan - lchan + 1
C
C --- Create the CHANNEL data
      i = 0
      DO i_chan = lchan, uchan
          i = i + 1
          chan_num(i) = i_chan
      END DO
C
C --- Write TTYPE1 (SPEC_NUM), TTYPE2 (CHANNEL), TTYPE3 (TSTART),
C --- TTYPE4 (TSTOP), TTYPE5 (RATE), TTYPE6 (STAT_ERR) columns,
C --- row by row for efficiency.
C
      DO i_spec = 1, n_spec
C
C ---     Write TTYPE1 (SPEC_NUM)
          colnum = 1
          n_elem = 1
          CALL FTPCLJ ( ounit, colnum, i_spec, 1,
     &      n_elem, i_spec, status )
C
C ---     Write TTYPE2 (CHANNEL)
          colnum = 2
          n_elem = n_chan
          CALL FTPCLJ ( ounit, colnum, i_spec, 1,
     &      n_elem, chan_num, status )
C
C ---     Write TTYPE3 (TSTART)
          colnum = 3
          n_elem = 1
          CALL FTPCLE ( ounit, colnum, i_spec, 1,
     &      n_elem, tstart(i_spec), status )
C
C ---     Write TTYPE4 (TSTOP)
          colnum = 4
          n_elem = 1
          CALL FTPCLE ( ounit, colnum, i_spec, 1,
     &      n_elem, tstop(i_spec), status )
C
C ---     Write TTYPE5 (RATE)
          colnum = 5
          n_elem = n_chan
          CALL FTPCLE ( ounit, colnum, i_spec, 1,
     &      n_elem, rates((i_spec-1)*n_chan+1), status )
C
C ---     Write TTYPE6 (STAT_ERR)
          colnum = 6
          n_elem = n_chan
          CALL FTPCLE ( ounit, colnum, i_spec, 1,
     &      n_elem, errors((i_spec-1)*n_chan+1), status )
C
      END DO
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      read_bbft
C
C DESCRIPTION:
C      gets binary table column data and selected keywords from input file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL read_bbft ( iunit, n_spec, lchan, uchan,
C                       tstart, tstop, rates, errors, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit  - FITSIO unit number for input file
C    Output ...
C      n_spec - number of spectra in file
C      lchan  - lower channel number (from LO_CHAN keyword)
C      uchan  - upper channel number (from UP_CHAN keyword)
C      tstart - array of spectra start times
C      tstop  - array of spectra stop times
C      rates  - array of measured count rates
C      errors - array of statistical errors, in units of count rate
C      status - status code
C
C PRIMARY LOCAL VARIABLES:
C      cment    - buffer to hold comment portion of FITS card
C      colnum   - column number of FITS binary table
C      i_spec   - index number of spectrum in FITS file
C      ihdu     - position index of desired HDU
C      intval   - work variable of type integer
C      logval   - work variable of type logical
C      msg      - terminal message buffer
C      n_chan   - number of spectral channels measured
C      taskname - FTOOL identifier
C
C CALLED ROUTINES:
C      subroutine FCECHO - Write text to terminal
C      subroutine FTGCVE - Get elements of binary table column of real type
C      subroutine FTGKYJ - Get keyword of integer type
C
C******************************************************************************

      SUBROUTINE read_bbft ( iunit, n_spec, lchan, uchan,
     &  tstart, tstop, rates, errors, status )
      IMPLICIT NONE
      CHARACTER cment*70, msg*80, taskname*40
      REAL tstart(*), tstop(*), rates(*), errors(*)
      INTEGER iunit, colnum, status
      INTEGER i_spec, n_spec, n_chan
      INTEGER ihdu, intval
      INTEGER lchan, uchan
      LOGICAL logval
      COMMON /TASK/ taskname
C
C --- Go to the BFITS data table extension (Extension #2)
      ihdu = 3
      CALL FTMAHD ( iunit, ihdu, intval, status )
C
C --- Ascertain the number of rows (spectra) in the input BFITS file
      CALL FTGKYJ ( iunit, 'NAXIS2', n_spec, cment, status )
C
C --- Ascertain the lower and upper channel limits in the input BFITS file
      CALL FTGKYJ ( iunit, 'LO_CHAN', lchan, cment, status )
      CALL FTGKYJ ( iunit, 'UP_CHAN', uchan, cment, status )
      n_chan = uchan - lchan + 1
C
C --- Write the single informational message that is output during a normal run
      WRITE ( msg,
     &  '( A, '': ...'', I4, '' channels x'', I5, '' spectra ...'')' )
     &  taskname ( 1 : INDEX(taskname,'   ') ), n_chan, n_spec
      CALL FCECHO ( msg )
C
C --- Get TIMES, RATES, ERRORS out of binary table, row by row for efficiency.
      DO i_spec = 1, n_spec
C
C ---     Read TTYPE1 (TIMES)
          colnum = 1
          CALL FTGCVE ( iunit, colnum, i_spec, 1, 1, 0,
     &      tstart(i_spec), logval, status )
          CALL FTGCVE ( iunit, colnum, i_spec, 2, 1, 0,
     &      tstop(i_spec), logval, status )
C
C ---     Read TTYPE2 (RATES)
          colnum = 2
          CALL FTGCVE ( iunit, colnum, i_spec, 1, n_chan, 0,
     &      rates((i_spec-1)*n_chan+1), logval, status )
C
C ---     Read TTYPE3 (ERRORS)
          colnum = 3
          CALL FTGCVE ( iunit, colnum, i_spec, 1, n_chan, 0,
     &      errors((i_spec-1)*n_chan+1), logval, status )
C
      END DO
C
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      e_keys
C
C DESCRIPTION:
C      Computes mean over detectors of minimum and maximum output data channel
C      energy edges, using E_EDGES data from BFITS calibration data extension,
C      and writes energy characterization keywords E_MIN, E_MAX, and EUNIT to
C      output file
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      CALL e_keys ( iunit, ounit, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit  - FITSIO unit number for input file
C      ounit  - FITSIO unit number for output file
C    Output ...
C      status - status code
C
C PRIMARY LOCAL VARIABLES:
C      chdu     - position index of initial HDU
C      cment    - buffer to hold comment portion of FITS card
C      colnum   - column number of FITS binary table
C      decimals - number of decimal places to write with FTPKYE
C      e_max    - maximum output channel energy edge, averaged over detectors
C      e_min    - minimum output channel energy edge, averaged over detectors
C      edges    - vector of energy edges, from E_EDGES table column
C      i_det    - index number of instrument detector
C      ihdu     - position index of desired HDU
C      intval   - work variable of type integer
C      logval   - work variable of type logical
C      n_det    - number of instrument detectors used
C      n_edge   - number of output channel energy edges
C      strval   - buffer to hold string keyword value
C      MAX_CHAN - maximum possible number of spectral channels
C
C CALLED ROUTINES:
C      subroutine xferkey - Transfer keyword record from input to output file
C      subroutine FTBNFM  - Parse TFORM binary table column format string
C      subroutine FTGCVE  - Get elements of binary table column of real type
C      subroutine FTGHDN  - Get number of current HDU
C      subroutine FTGKY|J/S - Get keyword of specified data type
C      subroutine FTMAHD  - Move to specified absolute HDU
C      subroutine FTPKYE  - Append keyword of real type
C
C******************************************************************************

      SUBROUTINE e_keys ( iunit, ounit, status )
      IMPLICIT NONE
      CHARACTER cment*70, strval*20
      INTEGER MAX_CHAN
      PARAMETER ( MAX_CHAN = 252 )
      REAL edges(MAX_CHAN+1)
      REAL e_min, e_max
      INTEGER iunit, ounit, n_det, n_edge, chdu, ihdu
      INTEGER i_det, decimals, colnum, intval, status
      LOGICAL logval
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Save current location in input file
      CALL FTGHDN ( iunit, chdu )
C
C --- Go to the BFITS calibration data extension (Extension #1)
      ihdu = 2
      CALL FTMAHD ( iunit, ihdu, intval, status )
C
C --- Determine the number of detectors
      CALL FTGKYJ ( iunit, 'NAXIS2', n_det, cment, status )
C
C --- Determine the number of channel edges
      CALL FTGKYS ( iunit, 'TFORM4', strval, cment, status )
      CALL FTBNFM ( strval, intval, n_edge, intval, status )
C
C --- Determine values for the E_MIN and E_MAX keywords
      e_min = 0.
      e_max = 0.
C
C --- The energy edges are stored in TTYPE4
      colnum = 4
      DO i_det = 1,n_det
          CALL FTGCVE ( iunit, colnum, i_det, 1, n_edge, 0,
     &      edges, logval, status )
          e_min = e_min + edges(1)
          e_max = e_max + edges(n_edge)
      END DO
C
C --- Average the minimum and maximum energy edges over all detectors
      e_min = e_min / n_det
      e_max = e_max / n_det
C
C --- Write the E_MIN and E_MAX keywords
      decimals = 7
      CALL FTPKYE ( ounit, 'E_MIN', e_min, decimals,
     &  'Lower energy boundary', status )
      CALL FTPKYE ( ounit, 'E_MAX', e_max, decimals,
     &  'Upper energy boundary', status )
C
C --- Transfer the physical units of energy from keyword TUNIT4 to EUNIT
      CALL xferkey ( iunit, 'TUNIT4', ounit, 'EUNIT', status )
C
C --- Return to saved location in input file
      CALL FTMAHD ( iunit, chdu, intval, status )
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C FUNCTION:
C      detnam_bpha
C
C DESCRIPTION:
C      Generates DETNAM keyword from the BFITS DET_MODE and Detectors NOTE
C      keywords
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C
C USAGE:
C      [result] = detnam_bpha ( iunit )
C
C ARGUMENTS:
C    Input  ...
C      iunit    - FITSIO unit number for input file
C    Output ...
C      [result] - sub-instrument identifier showing specific detector(s) used
C
C PRIMARY LOCAL VARIABLES:
C      cment  - buffer to hold comment portion of FITS card
C      detnam_bpha - sub-instrument identifier showing specific detector(s) used
C      detpos - position index of detector string in Detectors NOTE keyword
C      status - status code
C      strval - buffer to hold string keyword value
C
C CALLED ROUTINES:
C      subroutine FTGKYS - Get keyword of string type
C
C******************************************************************************

      CHARACTER*(*) FUNCTION detnam_bpha ( iunit )
      IMPLICIT NONE
      CHARACTER cment*70, strval*24
      INTEGER iunit, detpos, status

      status=0
C
      CALL FTGKYS ( iunit, 'DET_MODE', detnam_bpha, cment, status )
C --- Go to the second NOTE keyword, which has the Detectors string
      CALL FTGKYS ( iunit, 'NOTE', strval, cment, status )
      CALL FTGKYS ( iunit, 'NOTE', strval, cment, status )
C
C --- detpos is position index of detector string in Detectors NOTE keyword
      detpos = INDEX(strval,' ') + 2
      detnam_bpha = detnam_bpha(1:INDEX(detnam_bpha,' ')-1) // '-' //
     &  strval(detpos:)
C
      RETURN
      END
