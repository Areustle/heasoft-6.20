
      SUBROUTINE gpasca(phafile, rmffile, outfile, raytrace, point, 
     &                  simple, source, fudge, qarffl, bkgfile, 
     &                  qunif, qclobber, gbfil, ggfil, xrtrsp, xrtpsf, 
     &                  rayfile, status)

      CHARACTER*(*) phafile, rmffile, outfile, bkgfile
      CHARACTER*(*) gbfil, ggfil, xrtrsp, xrtpsf, rayfile
      INTEGER status
      REAL source(2)
      LOGICAL point, qclobber, qarffl, simple, fudge, qunif, raytrace

*------------------------------------------------------------------------------
* Description: Gets the parameters from the ascaarf par file.
*
* Arguments:   phafile (r) : The name of the pha file from which to read the
*                            WMAP extension
*              rmffile (r) : The name of the rmf file from which to read the
*                            binning grid
*              outfile (r) : The name of the output ARF file
*              raytrace(r) : If raytrace=.true. then the area and psf information
*                            is obtained from the output of a raytrace.
*              point   (r) : If point=.true. then the spectrum is from a point
*                            source otherwise assume an extended source
*              simple  (r) : Asked if point=.true. then if simple=.true. take
*                            source position as center of WMAP.
*              source  (r) : Source position (in detector coords) if point and
*                            not simple.
*              fudge   (r) : If fudge = .true. the effective area fudge will be
*                            applied.
*              qarffl  (r) : If qarffl = .true. the arf filter correction will
*                            be applied.
*              bkgfile (r) : Optional background map filename
*              qunif   (r) : If qunif = .true. the input surface brightness is
*                            assumed to be uniform.
*              qclobber (r): If qclobber = .true. and outfile exists, then
*                            delete outfile before writing it.  Otherwise,
*                            exit with error.
*              gbfil   (r) : The GIS Be map filename
*              ggfil   (r) : The GIS grid filename
*              xrtrsp  (r) : The XRT effective area filename
*              xrtpsf  (r) : The XRT PSF filename
*              rayfile (r) : The root name for the raytrace files
*              status  (r) : The success status for this routine
*                            0 = OK
*                            else = NOT OK
*
* Origin:      Written for ASCA mission data analysis
*
* Authors/Modification History:
*              Keith Arnaud  Dec  29, 1993 - Based on Ron Zellar and 
*                                           Yan Fernandez' routines.
*                            May  19, 1994 - Added imgfile argument.
*                            June  7, 1994 - Added psffil argument.
*                            June 28, 1994 - Added fudge argument.
*                            Sept  5, 1995 - Added xrtrsp, xrtpsf arguments.
*                                            Removed imgfile argument.
*                            Dec   5, 1995 - Added CALDB support.
*                            Feb   7, 1996 - New caldb routine used.
*                            Apr  10, 1996 - Removed psffil argument and added
*                                            qarffl argument.
*                            May  31, 1996 - Reads the PHA file to get the
*                                            instrument and only returns GIS
*                                            calibration files as required.
*                                            All cal file parameters support
*                                            the AUTO argument which first
*                                            checks the CALDB and then refdata
*                            July 31, 1996 - Added the uniform parameter
*                            Aug  13, 1996 - Used chatter=-1 in call to gtcalf
*                                            to suppress error messages when
*                                            using AUTO.
*                            Feb   4, 1997 - Added optional background map
*            Peter D Wilson  July 20, 1998 - Set errstat if not ASCA mission
*                       kaa  Oct   5, 1998 - Added raytrace option
*------------------------------------------------------------------------------

      INTEGER errstat, extno, nret, nfound, chatter, hdutype
      INTEGER ilun, rwmode, block, chipno, sensor, nchan, istr
      CHARACTER context*80, online*80, target*20, instru*4, detnam*4
      CHARACTER telscp*4, mission*20, expr*128, wrtstr*512, ctype*3
      CHARACTER dmode*10
      LOGICAL qauto

      INTEGER clenact
      EXTERNAL clenact

C  the parameter file has been opened by the c-wrapper (hascaarf.c). that's 
C  how gpasca knows what the parameter file is.

C  Set status = OK

      status = 0
      chatter = 0
      errstat = 0


C  Get the phafile parameter

      CALL uclgst('phafile', phafile, errstat)
      context = 'Cannot get the phafile parameter'
      IF ( errstat .NE. 0 ) GOTO 999

C  Open the pha file

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

C Check that this really is an ASCA data file

      CALL FTGKYS(ilun, 'TELESCOP', mission, context, errstat)
      context = 'Unable to find TELESCOP keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999
      context = 'This spectrum file is not from ASCA !'
      IF ( mission(1:4) .NE. 'ASCA' ) then
         errstat = 1
         GOTO 999
      ENDIF

C Read the instrument

      CALL FTGKYS(ilun, 'INSTRUME', instru, context, errstat)
      context = 'Unable to find INSTRUME keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999
      READ(instru(4:4), '(i1)') sensor
      WRITE(telscp, '(a,i1)') 'XRT', sensor+1

C Read the datamode

      CALL FTGKYS(ilun, 'DATAMODE', dmode, context, errstat)
      context = 'Unable to find DATAMODE keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999

C Read the number of spectral channels

      CALL FTGKYJ(ilun, 'DETCHANS', nchan, context, errstat)
      context = 'Unable to find DETCHANS keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999

C Read the channel type

      CALL FTGKYS(ilun, 'CHANTYPE', ctype, context, errstat)
      context = 'Unable to find CHANTYPE keyword in spectrum file'
      IF ( errstat .NE. 0 ) GOTO 999

      CALL FTCLOS(ilun, errstat)
      context = 'Unable to close input file'
      IF ( errstat .NE. 0 ) GOTO 999

      IF ( nchan .GT. 999 ) THEN
         WRITE(expr, '(a12,i4)') 'DETCHANS.EQ.', nchan
      ELSE
         WRITE(expr, '(a12,i3)') 'DETCHANS.EQ.', nchan
      ENDIF
      istr = clenact(expr)
      expr(istr+1:) = '.AND.CHANTYPE.EQ."'//ctype(:clenact(ctype))//'"'

C  Get the rmffile parameter

      CALL uclgst('rmffile', rmffile, errstat)
      context = 'Cannot get the rmffile parameter'
      IF ( errstat .NE. 0 ) GOTO 999

      IF ( rmffile .eq. 'CALDB' ) THEN
         IF ( instru(1:3) .EQ. 'GIS' ) THEN
            detnam = '-'
         ELSE
            CALL uclgsi('chipno', chipno, errstat)
            context = 'Cannot get the chipno parameter'
            IF ( errstat .NE. 0 ) GOTO 999
            WRITE(detnam,'(a,i1)') 'CCD', chipno
         ENDIF
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
         WRITE(wrtstr,'(a,a)') 'rmffile : ', rmffile(:clenact(rmffile))
         CALL fcecho(wrtstr)
      ENDIF           

*  Get the outfile parameter

      CALL uclgst('outfile', outfile, errstat)
      context = 'Cannot get the outfile parameter'
      IF ( errstat .NE. 0 ) GOTO 999

*  Get the raytrace parameter

      CALL uclgsb('raytrace', raytrace, errstat)
      context = 'Cannot get the raytrace parameter'
      IF ( errstat .NE. 0 ) GOTO 999

*  Get the point parameter

      IF ( raytrace ) THEN
         point = .FALSE.
      ELSE
         CALL uclgsb('point', point, errstat)
         context = 'Cannot get the point parameter'
         IF ( errstat .NE. 0 ) GOTO 999
      ENDIF

* if point is true then get the simple parameter

      IF ( point ) THEN

         CALL uclgsb('simple', simple, errstat)
         context = 'Cannot get the simple parameter'
         IF ( errstat .NE. 0 ) GOTO 999

* if simple is not true then get the source positions

         IF ( .NOT. simple ) THEN

            CALL uclgsr('Xposition', source(1), errstat)
            context = 'Cannot get the Xposition parameter'
            IF ( errstat .NE. 0 ) GOTO 999

            CALL uclgsr('Yposition', source(2), errstat)
            context = 'Cannot get the Yposition parameter'
            IF ( errstat .NE. 0 ) GOTO 999

         ENDIF

      ENDIF
        
* Get the fudge parameter

      CALL uclgsb('fudge', fudge, errstat)
      context = 'Cannot get the fudge parameter'
      IF ( errstat .NE. 0) GOTO 999

* Get the arffil parameter

      CALL uclgsb('arffil', qarffl, errstat)
      context = 'Cannot get the arffil parameter'
      IF ( errstat .NE. 0 ) GOTO 999

* Get the optional background map filename

      CALL uclgst('bkgfile', bkgfile, errstat)
      context = 'Cannot get the bkgfile parameter'
      IF ( errstat .NE. 0 ) GOTO 999

* Get the uniform parameter

      CALL uclgsb('uniform', qunif, errstat)
      context = 'Cannot get the uniform parameter'
      IF ( errstat .NE. 0 ) GOTO 999

* Get the clobber parameter

      CALL uclgsb('clobber', qclobber, errstat)
      context = 'Cannot get the clobber parameter'
      IF (errstat .ne. 0) GOTO 999

      IF ( sensor .GE. 2 ) THEN

* Get the Be file parameter

         CALL uclgst('bethick', gbfil, errstat)
         context = 'Cannot get the bethick parameter'
         IF ( errstat .NE. 0 ) GOTO 999

         qauto = .FALSE.
         IF (gbfil .EQ. 'AUTO' ) qauto = .true.
         chatter = 0
         IF ( qauto ) chatter = -1

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
            WRITE(wrtstr,'(a,a)') 'bethick : ', gbfil(:clenact(gbfil))
            CALL fcecho(wrtstr)
         ENDIF           

* Get the grid file parameter

         CALL uclgst('grid', ggfil, errstat)
         context = 'Cannot get the grid parameter'
         IF ( errstat .NE. 0 ) GOTO 999

         qauto = .FALSE.
         IF (ggfil .EQ. 'AUTO' ) qauto = .true.
         chatter = 0
         IF ( qauto ) chatter = -1

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
            WRITE(wrtstr,'(a,a)') 'grid : ', ggfil(:clenact(ggfil))
            CALL fcecho(wrtstr)
         ENDIF           

      ENDIF

* Get the XRT effective area file parameter

      CALL uclgst('xrtrsp', xrtrsp, errstat)
      context = 'Cannot get the xrtrsp parameter'
      IF ( errstat .NE. 0 ) GOTO 999

      qauto = .FALSE.
      IF (xrtrsp .EQ. 'AUTO' ) qauto = .true.
      chatter = 0
      IF ( qauto ) chatter = -1

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
         WRITE(wrtstr,'(a,a)') 'xrtrsp : ', xrtrsp(:clenact(xrtrsp))
         CALL fcecho(wrtstr)
      ENDIF          

* Get the XRT PSF file parameter

      CALL uclgst('xrtpsf', xrtpsf, errstat)
      context = 'Cannot get the xrtpsf parameter'
      IF ( errstat .NE. 0 ) GOTO 999

      qauto = .FALSE.
      IF (xrtpsf .EQ. 'AUTO' ) qauto = .true.
      chatter = 0
      IF ( qauto ) chatter = -1

      IF ( xrtpsf .EQ. 'CALDB' .OR. qauto ) THEN
         CALL gtcalf(chatter, 'ASCA', telscp, '-', '-', 'RPSF', 
     &               'now', 'now', 'now', 'now', '-', 1, xrtpsf, 
     &               extno, online, nret, nfound, errstat)
         context = 'Cannot find CALDB data for xrtrsp'
         IF ( errstat .NE. 0 .AND. .NOT.qauto ) GOTO 999
         IF ( errstat .NE. 0 .AND. qauto ) THEN
            errstat = 0
            target = 'xrt_psf_v2_0.fits'
            CALL fgfcal(xrtpsf, target, errstat)
            context = 'Failed to find '//target(:17)//
     &                ' in refdata area'
            IF ( errstat .NE. 0 ) GOTO 999
         ENDIF
         IF ( nfound .GT. 1 ) THEN
            WRITE(wrtstr,'(i3,a)') nfound, 
     &        ' matches were found in the CALDB, using...'
            CALL fcecho(wrtstr)
         ENDIF
         WRITE(wrtstr,'(a,a)') 'xrtpsf : ', xrtpsf(:clenact(xrtpsf))
         CALL fcecho(wrtstr)
      ENDIF           

* Get the parameter giving the root filename for the raytrace files

      IF ( raytrace ) THEN
         CALL uclgst('rayfile', rayfile, errstat)
         context = 'Cannot get the rayfile parameter'
         IF ( errstat .NE. 0 ) GOTO 999
      ENDIF

      RETURN

999   CONTINUE
      CALL fcerr(context)
      status = errstat
      RETURN

      END



