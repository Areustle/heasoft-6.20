
      SUBROUTINE gpaeffm(sensor, cchip, phafile, wmapfile, rmffile,
     &                   outfile, teldef, pha_lo, pha_hi, xrteff, 
     &                   gispsf, clobber, gbfil, ggfil, xrtrsp, 
     &                   status)

      INTEGER sensor
      CHARACTER*(*) phafile, wmapfile, rmffile, outfile, teldef
      CHARACTER*(*) gbfil, ggfil, xrtrsp, cchip
      INTEGER pha_lo, pha_hi, status
      LOGICAL xrteff, gispsf, clobber

*------------------------------------------------------------------------------
* Description: Gets the parameters from the ascaeffmap par file.
*
* Arguments:
*              sensor  (r) : sensor ID (0-3)
*              cchip   (r) : chip ID string (eg 012)
*              phafile (r) : The name of the pha file from which to read the
*                            spectrum
*              wmapfile(r) : The name of the WMAP file from which to read the
*                            weighting map
*              rmffile (r) : The name of the rmf file from which to read the
*                            binning grid
*              outfile (r) : The name of the output EFF image file
*              teldef  (r) : The name of the teldef file
*              pha_lo  (r) : Lower channel of spectrum to use
*              pha_hi  (r) : Upper channel of spectrum to use
*              xrteff  (r) : If xrteff = .true.
*                            then Multiply XRT effective area
*              gispsf  (r) : If gispsf = .true.
*                            then Convolve GIS Point Spread Function
*              clobber (r) : If clobber = .true. and outfile exists, then
*                            delete outfile before writing it.  Otherwise,
*                            exit with error.
*              gbfil   (r) : The GIS Be map filename
*              ggfil   (r) : The GIS grid filename
*              xrtrsp  (r) : The XRT effective area file
*              status  (r) : The success status for this routine
*                            0 = OK
*                            else = NOT OK
*
* Origin:      Written for ASCA mission data analysis
*
* Authors/Modification History:
*              Keith Arnaud  Dec 29, 1993 - Based on Ron Zellar and 
*                                           Yan Fernandez' routines.
*                            May 19, 1994 - Added imgfile argument.
*                            Jun  7, 1994 - Added psffil argument.
*                            Sep 22, 1995 - Added xrtrsp argument.
*                            Oct 12, 1995 - Trapped use of CALDB.
*                            May 31, 1996 - Reads the PHA file to get the
*                                            sensor number and only returns 
*                                            GIS calibration files as 
*                                            required. All cal file parameters
*                                            support the AUTO argument which 
*                                            first checks the CALDB and then 
*                                            refdata
*                            Oct 10, 1996 - Fixed incorrect telscp being passed
*                                           to gtcalf for xrtrsp
*                            Dec 20, 1996 - Allowed multiple SIS chips
*                            Nov 25, 1997 - Added expression to get correct RMF
*                                           from CALDB for SIS case
*                            Dec  8, 1998 - Initialize the errstat 
*
*------------------------------------------------------------------------------

      INTEGER errstat, extno, nret, nfound, chatter, nchan, hdutype
      INTEGER ilun, rwmode, block, i, istr
      CHARACTER context*80, online*80, instru*4, telscp*4, expr*128
      CHARACTER detnam*4, target*30, wrtstr*512, ctype*3
      LOGICAL qauto

      INTEGER fcstln
      EXTERNAL fcstln

*  the parameter file has been opened by the c-wrapper (hascaarf.c). that's 
*  how gpasca knows what the parameter file is.

*  Set status = OK

      status = 0
      chatter = 0
      errstat = 0

*  Get the phafile parameter

      CALL uclgst('phafile', phafile, errstat)
      context = 'Cannot get the phafile parameter'
      IF ( errstat .NE. 0 ) GOTO 999

*  Test that the pha file can be opened and get the instrument

      rwmode = 0
      block = 1
      ilun = 15

      call FTOPEN(ilun, phafile, rwmode, block, errstat)
      context = 'Unable to find FITS file: '//phafile
      IF ( errstat .EQ. 103 ) GOTO 999
      context = 'Unable to open FITS file: '//phafile
      IF ( errstat .NE. 0 ) GOTO 999

C Go to the extension containing the spectrum information

      CALL ftmrhd(ilun, 1, hdutype, errstat)
      context = 'Unable to move to SPECTRUM extension of '//phafile
      IF ( errstat .NE. 0 ) GOTO 999

C Read the instrument, number of spectral channels, 
C and channel type

      CALL FTGKYS(ilun, 'INSTRUME', instru, context, errstat)
      context = 'Unable to find INSTRUME keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999

      CALL FTGKYJ(ilun, 'DETCHANS', nchan, context, errstat)
      context = 'Unable to find DETCHANS keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999

      CALL FTGKYS(ilun, 'CHANTYPE', ctype, context, errstat)
      context = 'Unable to find CHANTYPE keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999

      CALL FTCLOS(ilun, errstat)
      context = 'Unable to close input file'
      IF ( errstat .NE. 0 ) GOTO 999

      READ(instru(4:4), '(i1)') sensor
      context = 'Bad sensor ID'
      IF ( (sensor .LT. 0) .OR. (sensor .GT. 3) ) GOTO 999
      WRITE(telscp, '(a,i1)') 'XRT', sensor+1

* Get the chip parameter

      IF ( sensor .LT. 2 ) THEN
         CALL uclgst('chip', cchip, errstat)
         context = 'Cannot get the chip parameter'
         IF ( errstat .NE. 0 ) GOTO 999
         context = 'Bad chip ID'
         DO i = 1, fcstln(cchip)
            IF ( (cchip(i:i) .NE. '0') .AND. 
     &           (cchip(i:i) .NE. '1') .AND. 
     &           (cchip(i:i) .NE. '2') .AND. 
     &           (cchip(i:i) .NE. '3') ) GOTO 999
         ENDDO
         WRITE(detnam, '(a,a)') 'CCD', cchip(1:1)
      ELSE
         cchip = ' '
         detnam = '-'
      ENDIF

* Get the rmffile parameter

      IF ( nchan .GT. 999 ) THEN
         WRITE(expr, '(a12,i4)') 'DETCHANS.EQ.', nchan
      ELSE
         WRITE(expr, '(a12,i3)') 'DETCHANS.EQ.', nchan
      ENDIF
      istr = fcstln(expr)
      expr(istr+1:) = '.AND.CHANTYPE.EQ."'//ctype(:fcstln(ctype))//'"'

      CALL uclgst('rmffile', rmffile, errstat)
      context = 'Cannot get the rmffile parameter'
      IF ( errstat .NE. 0 ) GOTO 999

      IF ( rmffile .EQ. 'CALDB' ) THEN
         CALL gtcalf(chatter, 'ASCA', instru, detnam, '-', 
     &               'MATRIX', 'now', 'now', 'now', 'now', 
     &               expr, 1, rmffile, extno, online, nret, nfound, 
     &               errstat)
         context = 'Cannot find CALDB data for rmffile'
         IF ( errstat .NE. 0 ) GOTO 999
         IF ( nfound .GT. 1 ) THEN
            WRITE(wrtstr,'(i3,a)') nfound, 
     &        ' matches were found in the CALDB, using...'
            CALL fcecho(wrtstr)
         ENDIF
         WRITE(wrtstr,'(a,a)') 'rmffile : ', rmffile(:fcstln(rmffile))
         CALL fcecho(wrtstr)
      ENDIF
         
* Get the outfile parameter

      CALL uclgst('outfile', outfile, errstat)
      context = 'Cannot get the outfile parameter'
      IF ( errstat .NE. 0 ) GOTO 999

* Get the pha_lo parameter

      CALL uclgsi('pha_lo', pha_lo, errstat)
      context = 'Cannot get the pha_lo parameter'
      IF ( errstat .NE. 0 ) GOTO 999

* Get the pha_hi parameter

      CALL uclgsi('pha_hi', pha_hi, errstat)
      context = 'Cannot get the pha_hi parameter'
      IF ( errstat .NE. 0 ) GOTO 999

* Get the wmapfile parameter

      CALL uclgst('wmapfile', wmapfile, errstat)
      context = 'Cannot get the wmapfile parameter'
      IF ( errstat .NE. 0 ) GOTO 999
      IF ( wmapfile .EQ. 'default' ) wmapfile = phafile

* If SIS data then get the teldef parameter

      IF ( sensor .LT. 2 ) then

         CALL uclgst('teldef', teldef, errstat)
         context = 'Cannot get the teldef parameter'
         IF ( errstat .NE. 0 ) GOTO 999

         qauto = .FALSE.
         IF ( teldef .EQ. 'AUTO' ) qauto = .TRUE.

         IF ( teldef .EQ. 'CALDB' .OR. qauto ) THEN
            CALL gtcalf(chatter, 'ASCA', instru, '-', '-', 
     &                  'ASCALIN', 'now', 'now', 'now', 'now', '-', 
     &                  1, teldef, extno, online, nret, nfound, 
     &                  errstat)
            context = 'Cannot find CALDB data for teldef'
            IF ( errstat .NE. 0 .AND. .NOT.qauto ) GOTO 999
            IF ( errstat .NE. 0 .AND. qauto ) THEN
               errstat = 0
               target = 's'//instru(4:4)//'_teldef_ascalin.fits'
               CALL fgfcal(teldef, target, errstat)
               context = 'Failed to find '//target(:22)//
     &                   ' in refdata area'
               IF ( errstat .NE. 0 ) GOTO 999
            ENDIF
         ENDIF

      ENDIF

* Get the xrteff parameter

      CALL uclgsb('xrteff', xrteff, errstat)
      context = 'Cannot get the xrteff parameter'
      IF ( errstat .NE. 0 ) GOTO 999

* Get the gispsf parameter

      CALL uclgsb('gispsf', gispsf, errstat)
      context = 'Cannot get the gispsf parameter'
      IF ( errstat .NE. 0 ) GOTO 999
        
* Get the clobber parameter

      CALL uclgsb('clobber', clobber, errstat)
      context = 'Cannot get the clobber parameter'
      IF ( errstat .NE. 0 ) GOTO 999

      IF ( sensor .GE. 2 ) THEN

* Get the Be file parameter

         CALL uclgst('bethick', gbfil, errstat)
         context = 'Cannot get the bethick parameter'
         IF ( errstat .NE. 0 ) GOTO 999

         qauto = .FALSE.
         IF (gbfil .EQ. 'AUTO' ) qauto = .true.

         IF ( gbfil .eq. 'CALDB' .OR. qauto ) THEN
            CALL gtcalf(chatter, 'ASCA', instru, '-', '-', 'WINTHICK', 
     &                  'now', 'now', 'now', 'now', '-', 1, gbfil, 
     &                  extno, online, nret, nfound, errstat)
            context = 'Cannot find CALDB data for bethick'
            IF ( errstat .NE. 0 .AND. .NOT.qauto ) GOTO 999
            IF ( errstat .NE. 0 .AND. qauto ) THEN
               errstat = 0
               target = 's'//instru(4:4)//'bev1.fits'
               CALL fgfcal(gbfil, target, errstat)
               context = 'Failed to find '//target(:11)//
     &                   ' in refdata area'
               IF ( errstat .NE. 0 ) GOTO 999
            ENDIF
            IF ( nfound .GT. 1 ) THEN
               WRITE(wrtstr,'(i3,a)') nfound, 
     &           ' matches were found in the CALDB, using...'
               CALL fcecho(wrtstr)
            ENDIF
            WRITE(wrtstr,'(a,a)') 'bethick : ', gbfil(:fcstln(gbfil))
            CALL fcecho(wrtstr)
         ENDIF           

* Get the grid file parameter

         CALL uclgst('grid', ggfil, errstat)
         context = 'Cannot get the grid parameter'
         IF ( errstat .NE. 0 ) GOTO 999

         qauto = .FALSE.
         IF (ggfil .EQ. 'AUTO' ) qauto = .true.

         IF ( ggfil .EQ. 'CALDB' .OR. qauto ) THEN
            CALL gtcalf(chatter, 'ASCA', instru, '-', '-', 'GRIDTRNS', 
     &                  'now', 'now', 'now', 'now', '-', 1, ggfil, 
     &                  extno, online, nret, nfound, errstat)
            context = 'Cannot find CALDB data for grid'
            IF ( errstat .NE. 0 .AND. .NOT.qauto ) GOTO 999
            IF ( errstat .NE. 0 .AND. qauto ) THEN
               errstat = 0
               target = 's'//instru(4:4)//'gridv3.fits'
               CALL fgfcal(ggfil, target, errstat)
               context = 'Failed to find '//target(:13)//
     &                   ' in refdata area'
               IF ( errstat .NE. 0 ) GOTO 999
            ENDIF
            IF ( nfound .GT. 1 ) THEN
               WRITE(wrtstr,'(i3,a)') nfound, 
     &           ' matches were found in the CALDB, using...'
               CALL fcecho(wrtstr)
            ENDIF
            WRITE(wrtstr,'(a,a)') 'grid : ', ggfil(:fcstln(ggfil))
            CALL fcecho(wrtstr)
         ENDIF           

      ENDIF

* Get the XRT effective area file parameter

      CALL uclgst('xrtrsp', xrtrsp, errstat)
      context = 'Cannot get the xrtrsp parameter'
      IF ( errstat .NE. 0 ) GOTO 999

      qauto = .FALSE.
      IF (xrtrsp .EQ. 'AUTO' ) qauto = .true.

      IF ( xrtrsp .EQ. 'CALDB' .OR. qauto ) THEN
         CALL gtcalf(chatter, 'ASCA', telscp, '-', '-', 'EFFAREA', 
     &               'now', 'now', 'now', 'now', '-', 1, xrtrsp, 
     &               extno, online, nret, nfound, errstat)
         context = 'Cannot find CALDB data for xrtrsp'
         IF ( errstat .NE. 0 .AND. .NOT.qauto ) GOTO 999
         IF ( errstat .NE. 0 .AND. qauto ) THEN
            errstat = 0
            target = 'xrt_ea_v2_0.fits'
            CALL fgfcal(xrtrsp, target, errstat)
            context = 'Failed to find '//target(:16)//
     &                ' in refdata area'
            IF ( errstat .NE. 0 ) GOTO 999
         ENDIF
         IF ( nfound .GT. 1 ) THEN
            WRITE(wrtstr,'(i3,a)') nfound, 
     &        ' matches were found in the CALDB, using...'
            CALL fcecho(wrtstr)
         ENDIF
         WRITE(wrtstr,'(a,a)') 'xrtrsp : ', xrtrsp(:fcstln(xrtrsp))
         CALL fcecho(wrtstr)
      ENDIF          

      RETURN

999   CONTINUE
      CALL fcerr(context)
      status = errstat
      RETURN

      END

