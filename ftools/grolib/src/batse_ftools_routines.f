C******************************************************************************
C SUBROUTINE:
C      getfilenames
C
C DESCRIPTION:
C      Gets input and output file names from the parameter file or from the
C      user
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
C      CALL getfilenames ( inpar, outpar, infile, outfile, status )
C
C ARGUMENTS:
C    Input  ...
C      inpar   - name of input file parameter
C      outpar  - name of output file parameter
C    Output ...
C      infile  - name of input file
C      outfile - name of output file
C      status  - status code
C
C PRIMARY LOCAL VARIABLES:
C      msg - text message buffer
C
C CALLED ROUTINES:
C      subroutine FCERR  - write text to stderr
C      subroutine UCLGST - get string parameter
C
C******************************************************************************

      SUBROUTINE getfilenames ( inpar, outpar, infile, outfile, status )
      IMPLICIT NONE
      CHARACTER*(*) inpar, outpar, infile, outfile
      CHARACTER msg*80
      INTEGER status
C
C --- Get name of input file
      CALL UCLGST ( inpar, infile, status )
      IF ( status .NE. 0 ) THEN
          msg = 'Unable to obtain ' // inpar // ' parameter'
          CALL FCERR ( msg )
          GO TO 999
      END IF
C
C --- Get name of output file
      CALL UCLGST ( outpar, outfile, status )
      IF ( status .NE. 0 ) THEN
          msg = 'Unable to obtain ' // outpar // ' parameter'
          CALL FCERR ( msg )
          GO TO 999
      END IF
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      chcard
C
C DESCRIPTION:
C      Transfers date or time keyword from input file to output file, with
C      format conversion and renaming
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C      01 JUL 1998  NG   Use the ftukys call to write the new keyword.
C      07 Jul 1998  PDW  Define func and instring as C*68
C
C NOTES:
C
C USAGE:
C      CALL chcard ( iunit, inkey, ounit, outkey, func,
C                    newcment, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit    - FITSIO unit number for input file
C      inkey    - name of old keyword
C      ounit    - FITSIO unit number for output file
C      outkey   - name of new keyword
C      func     - data format conversion function (either ddmmyy or hhmmss)
C      newcment - comment string for new keyword record
C    Output ...
C      status   - status code
C
C PRIMARY LOCAL VARIABLES:
C      card     - new card to write to output file
C      cment    - buffer to hold comment portion of FITS card
C      instring - original date or time string from input file
C      pos      - position index of substring within string
C
C CALLED ROUTINES:
C      subroutine FTGKEY - Get keyword as string
C      subroutine FTMCRD - Overwrite specified FITS keyword record
C      subroutine FTPREC - Append card to HDU
C
C******************************************************************************

      SUBROUTINE chcard ( iunit, inkey, ounit, outkey, func,
     &  newcment, status )
      IMPLICIT NONE
      CHARACTER card*80, instring*68
      character(68) keyvalue
      character(70) cment, newcment
      character(68) func
      character(8) inkey, outkey
      INTEGER iunit, ounit
      INTEGER pos
      INTEGER status
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Read the keyword record for inkey
      CALL FTGKEY ( iunit, inkey, instring, cment, status )
C
C --- A missing keyword generates a nonzero status on return from FTGKEY.
C --- Thus, this check traps out most non-.BFT FITS files as input.
      IF ( status .NE. 0 ) GO TO 999

C --- Update the keyword (Ning Gan)
      keyvalue = func(instring)  
      call ftukys(ounit, outkey, keyvalue, newcment,status)
C
C --- Set new keyword and its value as ASCII string
C      card(1:31) = outkey // '= ''' // func ( instring )
C
C --- Determine position for trailing apostrophe at end of keyword string value
C      pos = INDEX ( card(11:), ' ' )
C
C --- Set trailing apostrophe at end of keyword string value
C      card(10+pos:10+pos) = ''''
C
C --- Set the slash and comment field of the keyword record
C      card(32:80) = '/ ' // newcment
C
C --- Overwrite old keyword record with new card
C      CALL FTMCRD ( ounit, inkey, card, status )
C
C --- If keyword did not already exist, append the new card instead.
C      IF ( status .EQ. 202 ) THEN
C ---     Reset the status value to OK
C          status = 0
C          CALL FTPREC ( ounit, card, status )
C      END IF
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C SUBROUTINE:
C      xferkey
C
C DESCRIPTION:
C      Transfers keyword record from input to output file, with possible
C      renaming of keyword
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
C      CALL xferkey ( iunit, inkey, ounit, outkey, status )
C
C ARGUMENTS:
C    Input  ...
C      iunit  - FITSIO unit number for input file
C      inkey  - name of input keyword
C      ounit  - FITSIO unit number for output file
C      outkey - name of output keyword
C    Output ...
C      status - status code
C
C PRIMARY LOCAL VARIABLES:
C      card - FITS keyword record from input file
C
C CALLED ROUTINES:
C      subroutine FTGCRD - Get specified FITS keyword record
C      subroutine FTMCRD - Overwrite specified FITS keyword record
C      subroutine FTMNAM - Modify name of existing keyword
C      subroutine FTPREC - Append card to HDU
C
C******************************************************************************

      SUBROUTINE xferkey ( iunit, inkey, ounit, outkey, status )
      IMPLICIT NONE
      INTEGER iunit, ounit, status
      CHARACTER card*80, inkey*(*), outkey*(*)
C
C --- If previous error, just return
      IF ( status .NE. 0 ) GO TO 999
C
C --- Read the input record with key inkey from the input file iunit
      CALL FTGCRD ( iunit, inkey, card, status )
C
C --- Overwrite old keyword record with new card
      CALL FTMCRD ( ounit, outkey, card, status )
C
C --- If keyword did not already exist, append the new card instead.
      IF ( status .EQ. 202 ) THEN
C ---     Reset the status value to OK
          status = 0
          CALL FTPREC ( ounit, card, status )
      END IF
C
C --- Rename the keyword in the output file from inkey to outkey, if necessary
      IF ( outkey .NE. inkey )
     &  CALL FTMNAM ( ounit, inkey, outkey, status )
C
  999 CONTINUE
      RETURN
      END


C******************************************************************************
C FUNCTION:
C      ddmmyy
C
C DESCRIPTION:
C      Converts year+day-of-year date string (yyyy.ddd) to day-month-year
C(OBSOLETE)      string format (dd/mm/yy)
C      string format (yyyy-mm-dd)
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C      01 JUL 1998  NG   Changed the result to yyyy-mm-dd. Better
C                        leapyear treatment.
C      07 Jul 1998  PDW  Fix order of parameters in ftdt2s call
C
C NOTES:
C
C USAGE:
C      [result] = ddmmyy ( datstr )
C
C ARGUMENTS:
C    Input  ...
C      datstr   - date string in yyyy.ddd format
C    Output ...
C(OBSOLETE)      [result] - date string in dd/mm/yy format
C      [result] - date string in yyyy-mm-dd format
C
C PRIMARY LOCAL VARIABLES:
C      daymo  - array holding number of days per month
C      dd     - day of month, as an integer
C      ddmmyy - date string in yyyy-mm-dd format
C      mm     - month of year, as an integer
C      yy     - year, as an integer
C
C CALLED ROUTINES:
C      none
C
C******************************************************************************

      CHARACTER*(*) FUNCTION ddmmyy ( datstr )
      IMPLICIT NONE
      CHARACTER datstr*(*)
      INTEGER dd, mm, yy
      INTEGER daymo(12)
      INTEGER status
      DATA daymo / 31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
C
C --- convert year from string to integer
      READ ( datstr(1:4), '(I4)' ) yy
C
C --- set length of February (this works for years 1901 through 2099)
c      IF ( JMOD(yy,4) .EQ. 0 ) THEN
      IF ( (MOD(yy,4) .EQ. 0 .AND. MOD(yy,100).NE.0.)
     *      .OR. MOD(yy,400).EQ.0 ) THEN
          daymo(2) = 29
      ELSE
          daymo(2) = 28
      END IF
C
C --- convert DOY from string to integer
      READ ( datstr(6:8), '(I3)' ) dd
C
      mm = 1
C
      DO WHILE ( dd .GT. daymo(mm) )
          dd = dd - daymo(mm)
          mm = mm + 1
      END DO

      status = 0
      call ftdt2s(yy, mm, dd, ddmmyy, status)
C
C --- now insert time values into ASCII string with leading zeroes
C      WRITE ( ddmmyy(1:2), '(I2.2)' ) dd
C      WRITE ( ddmmyy(4:5), '(I2.2)' ) mm
C      ddmmyy(7:) = datstr(3:4)
C
C --- insert field dividers into output string
C      ddmmyy(3:3) = '/'
C      ddmmyy(6:6) = '/'
C
      RETURN
      END


C******************************************************************************
C FUNCTION:
C      hhmmss
C
C DESCRIPTION:
C      Converts seconds-of-day time string in F or E format (sssss.sss or
C      s.sssE+nn, for example) to time-of-day string format (hh:mm:ss.sss,
C      for example)
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C      11 Jun 1999  toliver    Initialize function output to blanks, since
C                              Linux fails to do this implicitly
C
C NOTES:
C      Provides precision to nanosecond
C
C USAGE:
C      [result] = hhmmss ( timstr )
C
C ARGUMENTS:
C    Input  ...
C      timstr   - seconds-of-day time string in F or E format
C    Output ...
C      [result] - time string in time-of-day format
C
C PRIMARY LOCAL VARIABLES:
C      hh     - hour of day, as an integer
C      hhmmss - time string in time-of-day format
C      mm     - minute of hour, as an integer
C      ms     - millisecond of second, as an integer
C      ss     - second of minute, as an integer
C      tim    - seconds-of-day time as number in internal format
C
C CALLED ROUTINES:
C      none
C
C******************************************************************************

      CHARACTER*(*) FUNCTION hhmmss ( timstr )
      IMPLICIT NONE
      CHARACTER timstr*(*)
      DOUBLE PRECISION tim
      INTEGER hh, mm, ss, ms
C
C --- convert input string to number, ignoring embedded and trailing blanks
      READ ( timstr, '(BN, D20.3)' ) tim
C
c      ss = JIDINT ( tim )
      ss = IDINT ( tim )
C
C --- compute nanoseconds, hours, minutes, seconds
c      ms = JIDNNT (1.D+03 * (tim-ss))
      ms = NINT (1.D+03 * (tim-ss))
      hh = ss / 3600
c     ss = JMOD ( ss, 3600 )
      ss = MOD ( ss, 3600 )
      mm = ss / 60
c     ss = JMOD ( ss, 60 )
      ss = MOD ( ss, 60 )
C
C --- now insert time values into ASCII string with leading zeroes
      hhmmss = ' '
      WRITE ( hhmmss(01:02), '(I2.2)' ) hh
      WRITE ( hhmmss(04:05), '(I2.2)' ) mm
      WRITE ( hhmmss(07:08), '(I2.2)' ) ss
      WRITE ( hhmmss(10:12), '(I3.3)' ) ms
C
C --- insert field dividers into output string
      hhmmss(3:3) = ':'
      hhmmss(6:6) = ':'
      hhmmss(9:9) = '.'
C
      RETURN
      END


C******************************************************************************
C FUNCTION:
C      fname
C
C DESCRIPTION:
C      Extracts file name, with optional extension, from UNIX or OpenVMS full
C      file specification string
C
C AUTHOR/DATE:
C      R. Nakatsuka / 06 Apr 1995
C
C MODIFICATION HISTORY:
C      06 Apr 1995  RN   Original version
C
C NOTES:
C      UNIX example:
C      fname ( '/arch/batse/burst00/BURST_00105/mer_bfits_105.fits', .FALSE. )
C          returns 'mer_bfits_105'
C      OpenVMS example:
C      fname ( 'GROVX3$DKA400:[DATA.B00105]MR_00105.BFT;1', .TRUE. )
C          returns 'MR_00105.BFT'
C
C USAGE:
C      [result] = fname ( fullname, ext )
C
C ARGUMENTS:
C    Input  ...
C      fullname - full file specification string
C      ext      - flag, to include file extension (.TRUE.), or not (.FALSE.)
C    Output ...
C      [result] - extracted file name, with optional extension
C
C PRIMARY LOCAL VARIABLES:
C      fname - extracted file name, with optional extension
C      pos1  - work variable to determine start index of file name substring
C      pos2  - work variable to determine end index of file name substring
C
C CALLED ROUTINES:
C      none
C
C******************************************************************************

      CHARACTER*(*) FUNCTION fname ( fullname, ext )
      IMPLICIT NONE
      CHARACTER fullname*(*)
      INTEGER pos1, pos2
      LOGICAL ext
C
      pos1 = 1
C
C --- Find the last '/' in the file specification  (for UNIX)
      DO WHILE (INDEX(fullname(pos1:),'/') .GT. 0 )
          pos1 = pos1 + INDEX (fullname(pos1:), '/')
      END DO
C
C --- Find the last ':' in the file specification  (for OpenVMS)
      DO WHILE (INDEX(fullname(pos1:),':') .GT. 0 )
          pos1 = pos1 + INDEX (fullname(pos1:), ':')
      END DO
C
C --- Find the right bracket in the file specification  (for OpenVMS)
      pos1 = pos1 + INDEX (fullname(pos1:), ']')
C
C --- Found the starting position.  Now find the ending position.
      pos2 = pos1
C
      IF ( .NOT. ext ) THEN
C ---     Find the last '.' in the file specification  (for UNIX & OpenVMS)
          DO WHILE (INDEX(fullname(pos2:),'.') .GT. 0 )
              pos2 = pos2 + INDEX (fullname(pos2:), '.')
          END DO
      END IF
C
C --- Find the ';' in the file specification  (for OpenVMS)
      IF (pos2 .EQ. pos1)  pos2 = pos2 + INDEX(fullname(pos2:),';')
C
C --- Use the entire remaining portion of fullname if no '.' or ';' exists
      IF (pos2 .EQ. pos1)  pos2 = pos2 + INDEX(fullname(pos2:),' ')
C
      fname = fullname ( pos1 : pos2-2 )
      RETURN
      END
