c
c ************************************************************
c SUBROUTINES FOLLOW
c ************************************************************
c
c
C $Id: xsel_extract.f,v 3.65 2016/02/22 21:19:08 kaa Exp $
C 
C $Log: xsel_extract.f,v $
C Revision 3.65  2016/02/22 21:19:08  kaa
C Improved diagnostic output and changed to handle tabs in xselect.mdb.
C
C Revision 3.64  2015/10/27 19:30:43  kaa
C Changed CHARACTER declaration to remove those in obsolete format.
C
C Revision 3.63  2015/10/20 19:24:18  kaa
C The various size entries in the MDB now default to TLMAX since this is what all
C modern event files should use. This can still be overridden by settings in the
C xselect.mdb file.
C
C Revision 3.62  2013/05/21 19:08:48  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.61  2012/05/01 18:14:09  kaa
C Converted sigma for gaussian or lorentzian smoothing to a real.
C
C Revision 3.60  2012/04/05 20:05:36  kaa
C Improved plot device code so that filenames can be given for appropriate
C devices (eg set device myplot.ps/cps).
C
C Revision 3.59  2012/03/23 19:20:15  kaa
C Fixes for the problem that xselect has a tendency to go into an infinite loop
C if a command is given the wrong argument. Now it nicely passes the user back
C to the main prompt so they can try again.
C
C Revision 3.58  2011/08/18 20:01:08  kaa
C Lots of tidying up, mainly to remove compiler warnings.
C
C Revision 3.57  2011/06/18 12:48:28  kaa
C Removed spaces around = when defining parameters in ftools calls.
C
C Revision 3.56  2011/02/11 23:35:44  kaa
C Added code to pass to extractor the setting of the adjustgti parameter.
C
C Revision 3.55  2010/07/16 19:26:44  kaa
C Added the timepixr keyword to the xselect.mdb and set this to 0.5 for ASCA
C SIS0 and SIS1. This keyword should only be used in the xselect.mdb in the case
C where the TIMEPIXR keyword is not included in event files and it needs to be
C set to something other than 0.0.
C
C Revision 3.54  2008/03/14 23:41:12  kaa
C Now allows the xselect.mdb to contain a wild card at the end of names of
C submissions, instruments, and datamodes. The wild card is a * ie XIS* will
C match to any of XIS0, XIS1, XIS2, XIS3.
C
C Revision 3.53  2006/05/17 16:49:26  kaa
C Modified mkfbin/hkbin so that if an event lightcurve has been extracted then the
C mkf/hk parameters will be binned to match the event lightcurve and the RATE will
C be added to the mkfbin/hkbin output file. plot mkf/hk then enables the user to
C plot correlations between housekeeping parameters and the event rate.
C
C Added filter mkf and filter hk options as aliases for select mkf and select hk.
C Added save mkfbin and save hkbin options.
C
C Revision 3.52  2006/05/09 17:43:51  kaa
C Minor improvements in diagnostic output.
C
C Revision 3.51  2005/11/16 23:53:58  kaa
C Split XSL_CLEAR code out of xsel_extract.f and placed in its own file.
C Corrected failure to propagate MJDREFI/F everywhere it was required which
C led to incorrect label at the top of lightcurve plots.
C
C Revision 3.50  2004/12/09 22:13:15  kaa
C Added filter column option which allows filtering on event attributes which
C are included in columns in the event file. At the moment allows filters of
C the form "NAME=val:val val:val..." because these are handled by the extractor
C key filtering. Note that this filter can be used on Astro-E2 XRS data to
C get events from subsets of pixels.
C
C Revision 3.48  2003/08/07 17:06:45  kaa
C Rationalized the reading of the MDB standard parameters when setting mission,
C instrument, or datamode. This ensures that any standard parameter can be changed
C at any level. This does require that all standard parameters are defined at the
C mission level in the MDB file.
C
C Revision 3.47  2003/07/11 15:48:44  kaa
C Added filter grade option and updated documentation.
C
C Revision 3.45  2002/03/27 20:04:25  kaa
C Added support for the extractor ccol parameter. Also removed error if the GTI cannot
C be found because if data subspace keywords are in use we can find GTIs that way.
C Probably need to improve this. Also still need to rework the select chip command.
C
C Revision 3.44  2001/12/21 17:20:02  kaa
C Fixed possible seg fault under Linux when using an instrument with the image
C and/or wmap coords set to NONE.
C
C Revision 3.43  2001/01/24 22:23:50  kaa
C Fixed plot spectrum and curve so they recognise the qdp_commands parameter.
C
C Revision 3.42  2000/05/28 00:09:34  kaa
C Improved handling of instruments where users might want to switch between
C energy columns (eg PHA to PI). Set phaname now resets the lower and upper
C limits on the column values. If the MDB specifies spectrum truncation xselect
C now only resets the limits if they lie outside the specified truncated range.
C
C Revision 3.41  2000/05/12 01:00:02  kaa
C Added a new parameter rebinregion which determines whether the region files
C will be rebinned. This is added because ds9 region files don't need rebinning
C and fregcon currently doesn't handle them correctly.
C
C Revision 3.40  2000/03/15 19:45:46  kaa
C Image size was not being reset as claimed
C
C Revision 3.39  1999/12/28 03:25:15  kaa
C Suppressed prompt for the size keyword after set XYNAME to a coordinate
C system other than the standard three in the mdb. Assume that the TLMAX
C keywords will be used, which should always be the case now.
C
C Revision 3.38  1999/10/04 19:19:10  kaa
C Created new XSL_LCPLOT which just runs fplot. Renamed the routine that does
C cursor selection to XSL_TIME_CURSOR.
C
C Revision 3.37  1999/09/30 23:10:56  kaa
C Fixed bug that it tries to edit region files even when they do not exist
C
C Revision 3.36  1999/09/27 17:25:05  kaa
C Modified "set xybinsize" so that it alters any defined region filters so
C that they are still valid with the new binning.
C
C Revision 3.35  1999/09/20 21:38:17  kaa
C Switched to Y2K version of date string and allowed MJDREF as alternate to TIMESTR
C
C Revision 3.34  1999/07/17 20:57:21  kaa
C Added WTMAPFIX logical to control extractor WMAP fixing
C
C Revision 3.33  1999/05/10 23:30:10  ngan
C Set the HAVEMKF variable in mkfbin.
C
c Revision 3.32  1999/05/08  01:28:02  ngan
c Change some of the xsl_fdump to xsl_fdump2 to incorporate fv.
c
c Revision 3.31  1999/05/07  22:25:00  kaa
c Fixed it so HAVEMKF is only TRUE if there really is an MKF file
c
C Revision 3.30  1999/05/02 00:16:30  ngan
C New xselect(put a & at the end of plotting commands).
C
c Revision 3.29  1999/04/26  19:45:45  kaa
c Replaced use of mkf_reldir parameter with mkf_rel_dir keyword in the mission database
c
C Revision 3.28  1999/04/13 16:57:27  peachey
C First round of changes associated with new xselect, which uses tcl/tk
C
c Revision 3.26  1998/08/27  19:33:33  kaa
c Changed version number to 1.4b. Removed unused variables.
c
C Revision 3.25  1998/06/03 02:15:07  kaa
C Attempt to make slightly more logical distribution of routines among files
C
C Revision 3.24  1998/06/03 01:42:01  kaa
C This file is now redundant since it is identical to xsel_ver.inc
C
C Revision 3.23  1998/06/02 21:16:10  kaa
C Improved readability of the intensity filter
C
C Revision 3.22  1998/06/02 18:46:28  kaa
C Added a sort on DATE-OBS and TIME-OBS when making obscat. Added the pha_cut
C filters to "show status". Simplified the message written if no mkf files are
C found during a "set datadir".
C
C Revision 3.21  1998/05/11 22:08:51  kaa
C No longer require an @ in front of the filename of a list of files for the
C read command. Also tidied up command matching so it works through a single
C subroutine.
C
C Revision 3.20  1998/05/08 16:34:30  kaa
C Initial changes for XTE support
C
C Revision 3.19  1998/02/26 22:52:17  kaa
C Removed the fullimage keyword and replaced by test of whether the user
C has set the xysize.
C
C Revision 3.18  1997/11/24 23:42:20  kaa
C Major overhaul to create v1.4. Most of the mission-dependence is switched out
C into the ascii file xselect.mdb. All other mission-dependent code is in the
C file xsel_mission.f.
C
C Revision 3.17  1997/06/11 21:45:01  kaa
C Added fullimage parameter to extract image command - mainly for benefit of ROSAT HRI
C
C Revision 3.16  1997/06/03 19:25:09  kaa
C Added raw option to set image
C
C Revision 3.15  1997/05/16 21:57:29  kaa
C ROSAT_RDF_phamax was never read
C
C Revision 3.14  1997/04/17 23:00:32  kaa
C Cosmetic changes to eliminate g77 compilation warnings
C
C Revision 3.13  1997/04/15 22:22:41  kaa
C Fixing inopportune commit
C
C Revision 3.11  1997/02/07 00:12:22  kaa
C Added fmemsort after extract events when inputting multiple event files
C
c Revision 3.10  1997/02/06  21:15:53  kaa
c Added in SAX changes brought back by Lorella
c
c Revision 3.9  1996/12/23  18:11:47  kaa
c Added improved error checking and fixed erroneous running of fixrev0pha for ASCA MPC mode
c
c Revision 3.8  1996/11/02  21:55:35  kaa
c Fixed typo in previous change
c
c Revision 3.7  1996/10/29  20:02:43  kaa
c Added warning for set wmapname
c
c Revision 3.6  1996/09/04  22:56:59  cvsuser
c Updated behavior of 'select fast' using new information from ISAS
c
c Revision 3.5.1.1  1996/04/16  01:54:28  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.7  1995/09/29  18:50:24  oneel
c set chatter to 0 on the fixrev0pha line
c
c Revision 1.6  1995/09/25  18:41:50  oneel
c renamed fix_rev0_pha to fixrev0pha
c
c Revision 1.5  1995/09/13  16:17:14  oneel
c Try again, we were calling xsl_wrtcf wrong.
c
c Revision 1.4  1995/09/13  16:11:53  oneel
c let's try a 0 as the last parameter in xsl_wrtcf with the fix_rev0_pha
c tool
c
c Revision 1.3  1995/09/13  15:09:10  oneel
c Opps, forget the final parameter to xsl_wrtcf with fix_rev0_pha
c
c Revision 1.2  1995/09/13  13:06:43  oneel
c add fix_rev0_pha for spectal files
c
c
c
c ---------------------------------------------
      subroutine XSL_CPD()
c ---------------------------------------------
c
c This is the same as set device. 

      status = 0
      call XSL_GETDEV()
      IF(status.NE.0) status = 0

      return
      end
      
c ---------------------------------------------
      subroutine XSL_ECHO(ECHO)
c ---------------------------------------------
c  Toggles the ECHO parameter on and off
      logical ECHO
      
      ECHO = .NOT. ECHO
      IF(ECHO) THEN
C        make sure the term is on
C         call XSELSETTERM(6)
         call XWRITE('Echo is on',5)
      ELSE
         call XWRITE('Echo is off',5)
      ENDIF

      
      return
      end

c ---------------------------------------------
      subroutine XSL_HKBIN()
c ---------------------------------------------

      CALL xsl_mkforhkbin(1)

      RETURN
      END

c ---------------------------------------------
      subroutine XSL_MKFBIN()
c ---------------------------------------------

      CALL xsl_mkforhkbin(0)

      RETURN
      END
      
c
c ---------------------------------------------
      subroutine XSL_MKFORHKBIN(Mode)
c ---------------------------------------------

      IMPLICIT NONE

      INTEGER Mode

c Routine to bin filter file (mode=0) or HK (mode=1) variables
c Called by XSELECT main
c
c     Jim Ingham 2/17/93
c     kaa        5/17/06

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c the command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(512) str1, str2
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1
c ---------------------------------------------
      integer LENACT
      double precision dtime
      character(255) param
      character(512) contxt

      status=0

      IF ( Mode .NE. 0 .AND. Mode .NE. 1 ) THEN
         CALL xwrite('Invalid Mode in XSL_MKFORHKBIN', 5)
         RETURN
      ENDIF

      IF ( Mode .EQ. 1 .AND. .NOT.HKREAD ) THEN
         CALL xwrite('No HK files have been read.', 5)
         RETURN
      ENDIF
      
c If the datadir has not yet been entered, prompt for it

      call XSL_SET_DIR('data_dir',datdir,status)
      if (status.ne.0) return

c  Now get the parameter(s) to be binned

      IF ( Mode .EQ. 0 ) THEN
         call xsl_uclgst('mkf_param',param,status)
         contxt = 'Failed to get mkf_param'
      ELSE
         call xsl_uclgst('param',param,status)
         contxt = 'Failed to get param'
      ENDIF
      IF ( status .NE. 0 ) GOTO 99999

c  Set up a list of the parameters binned.

      IF ( Mode .EQ. 0 ) THEN
         CALL XSL_PARSE(param,ffplis,nffpar,MAXCUR,status)
         contxt = 'Too many parameters for MKF binning'
      ELSE
         CALL XSL_PARSE(param,parlis,npar,MAXCUR,status)
         contxt = 'Too many parameters for HK binning'
      ENDIF
      if(status.ne.0) then
         call XWRITE(contxt,5)
         status = 0
         return
      endif

c Get the filter file(s)

      IF ( Mode .EQ. 0 ) THEN
         call xsl_uclgst('mkf_name',ffilin,status)
         contxt = 'Failed to get mkf_name'
         IF ( status .NE. 0 ) GOTO 99999
      ENDIF

c If there is no lightcurve extracted ask for the dtime variable 

      IF ( .NOT.CURV ) THEN
         call xsl_uclgsd('dtime',dtime,status)
         contxt = 'Failed to get dtime'
         IF ( status .NE. 0 ) GOTO 99999
      ENDIF
      
c Now merge the input files if necessary

      IF ( Mode .EQ. 0 ) THEN

         call XSL_FINDMKF(.TRUE.)
         IF(status.ne.0) THEN
            call XWRITE('Error in MKFBIN from FINDMK',5)
            return
         ENDIF

      ELSE
      
         IF(MANYHK.AND. .NOT. MERGHK) THEN
            IF( WORKHK )then
               call XSL_MERGE(hkwrk1,nhkfil,lstfil,mrghkf,'HK',cmdfil,
     +                 str1,ECHO,ierr)
            ELSE
               call XSL_MERGE(hkflnm,nhkfil,lstfil,mrghkf,'HK',cmdfil,
     +                 str1,ECHO,ierr)
            ENDIF
            if(ierr.eq.0)then
               MERGHK=.TRUE.
            else
               call XWRITE('Error in HKBIN from fmerge',5)
               return
            endif
         ENDIF

      ENDIF

c  Then check whether the HK files are expanded, expand if not.  Also if
c  the original files were unexpanded, we need to reexpand with the 
c  new parameters.

      IF ( Mode .EQ. 1 ) THEN

         IF(.NOT.EXPAND) THEN
             CALL XSL_HKEXPAND(param,dtime,ierr)
             if(ierr.ne.0) then
                call XWRITE('Error from HKEXPAND in HKBIN',5)
                return
             endif
         ELSE IF (WORKHK) then
             WORKHK = .FALSE.
             EXPAND = .FALSE.
             CALL XSL_HKEXPAND(param,dtime,ierr)
             if(ierr.ne.0) then
                call XWRITE('Error from HKEXPAND in HKBIN',5)
                return
             endif

c    and check that the required column(s) exists if they are:

         ELSE

            IF( MERGHK ) THEN
                CALL XSL_CHKCOL(mrghkf,parlis,npar,status)
            ELSE
                CALL XSL_CHKCOL(hkflnm(1),parlis,npar,status)         
            ENDIF
            IF(status.gt.1) then
               call XWRITE('Rerun HKBIN with valid parameters',5)
               return
            ENDIF
            IF(status.lt.0) then
               call XWRITE('Rerun HKBIN with valid filenames',5)
               return
            ENDIF 
         ENDIf
               
      ENDIF

c Construct the string to run the perl script which runs fcurve and faddcol

      comlin = 'xsl_mkf_or_hk_bin '
      len1 = LENACT(comlin) + 1

c First put in the appropriate mkf or hk file name

      IF ( Mode .EQ. 0 ) THEN
         IF(nmkf.eq.1) THEN
            comlin(len1+1:) = mkfnam(1)(1:LENACT(mkfnam(1)))
         ELSE
            comlin(len1+1:) = mermkf(1:LENACT(mermkf))
         ENDIF
      ELSE
         IF( WORKHK ) THEN
            comlin(len1+1:) = hkwrk1(1)(1:LENACT(hkwrk1(1)))
        ELSE IF ( MERGHK ) THEN
            comlin(len1+1:) = mrghkf(1:LENACT(mrghkf))
        ELSE
            comlin(len1+1:) = hkflnm(1)(1:LENACT(hkflnm(1)))
        ENDIF
      ENDIF
      len1 = LENACT(comlin ) + 1

c Add in the lightcurve file if it exists

      IF ( CURV ) THEN
         comlin(len1+1:) = curfits(:LENACT(curfits))
      ELSE
         comlin(len1+1:) = 'none'
      ENDIF
      len1 = LENACT(comlin ) + 1

c The output filename

      IF ( Mode .EQ. 0 ) THEN
         comlin(len1+1:) = ffcurf(:LENACT(ffcurf))
      ELSE
         comlin(len1+1:) = hkcurf(:LENACT(hkcurf))
      ENDIF
      len1 = LENACT(comlin ) + 1

c The columns

      comlin(len1+1:) = '"'//param(:LENACT(param))//'"'
      len1 = LENACT(comlin ) + 1

c If not using a lightcurve but there is a filter file gti then set that

      IF ( .NOT.CURV .AND. FFTFL ) THEN
         comlin(len1+1:) = ffflt(:LENACT(ffflt))
      ELSE
         comlin(len1+1:) = 'none'
      ENDIF
      len1 = LENACT(comlin ) + 1

c If not using a lightcurve then convert the time bin to a string and add

      IF ( .NOT.CURV ) THEN
         write(str2,'(e15.7)') dtime
         comlin(len1+1:) = str2(:LENACT(str2))
      ELSE
         comlin(len1+1:) = '0.0'
      ENDIF
      len1 = LENACT(comlin ) + 1

c Add the appropriate echo argument

      IF ( ECHO ) THEN
         comlin(len1+1:) = 'yes'
      ELSE
         comlin(len1+1:) = 'no'
      ENDIF
      
c Now set up and run the command file
      
      call XSL_OPCF(cmdfil,ilun)
      call XSL_WRTCF(ilun,comlin,1)
      call XSL_CLCF(ilun)

      call XSL_RUNCF(cmdfil,ECHO,status)
      
c  Now set the logicals

      if(status.eq.0) then
         IF ( Mode .EQ. 0 ) THEN
            FFCURV = .TRUE.
         ELSE
            HKCURV = .TRUE.
         ENDIF
      else
         str1 = 'Error in fcurve '
         CALL XWRITE(str1,5)
         RETURN
      endif

c  If a lightcurve is in use we have included RATE in the output file so add
c  this to the list of parameters so PLOT MKF and PLOT HK will know about it

      IF ( CURV ) THEN
         IF ( Mode .EQ. 0 ) THEN
            ffplis(nffpar+1) = 'RATE'
            nffpar = nffpar + 1
         ELSE
            parlis(npar+1) = 'RATE'
            npar = npar + 1
         ENDIF
      ENDIF

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_MKFORHKBIN: Status = ', status
         CALL XWRITE(contxt, 5)
      ELSE 
         IF ( Mode .EQ. 0 ) HAVEMKF = .TRUE.
      ENDIF


      return
      end
c
c ---------------------------------------------
      subroutine XSL_PLOT()
c ---------------------------------------------
c 
c Called by XSELECT main
c
c     Alan Smale 1992 Nov

      INCLUDE 'xsel.inc'
      INCLUDE 'xselplt.inc'
        
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(512) str1
c ---------------------------------------------
      integer MXCMDS
      parameter (MXCMDS = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      logical PLTQDP
      integer nwhats,myindex
      character(512) contxt

      data commad /'CURVE',
     &             'HK',
     &             'IMAGE',
     &             'MKF',
     &             'SPECTRUM',
     &             15*'NA'/
      data comdes /'plot the light curve, enter selections',
     &             'plot the binned hk parameters',
     &             'plot the extracted image, using SAOIMAGE',
     &             'plot the binned filter file parameters',
     &             'plot the accumulated spectrum',
     &             15*'NA'/
      data nwhats / 5 /

      CALL xsl_match_cmd('plot_what', commad, comdes, nwhats, 
     &                   MXCMDS, comno, str1, myindex, status)

      IF ( status .NE. 0 ) THEN
         status = 0
         return
      ENDIF

c If plotdv has not been set, set it:          

      IF((str1.ne.'IMAGE').and.(plotdv.eq. ' '
     &     .or. plotdv .eq.'NONE'.or.plotdv.eq.'/NONE')) THEN
         call XSL_GETDEV()
         IF(status.ne.0) THEN
            status = 0
            return
         ENDIF
      ENDIF

c ---------------------------------------------
c PLOT SPECTRUM (plot the spcfil)

      IF(str1.eq.'SPECTRUM')then
         IF( SPEC )then
            CALL XSL_SPPLOT()
         ELSE
            call XWRITE(' No spectrum has been accumulated yet.',
     &           5)
         ENDIF

c ---------------------------------------------
c PLOT CURVE (plot the curfil)
      ELSE IF(str1.eq.'CURVE')then
         IF( CURV )then
            IF(USEQDP) THEN
               call xsl_uclgsb('plot_qdp',PLTQDP,status)
               contxt = 'Failed to get plot_qdp'
               IF ( status .NE. 0 ) GOTO 99999
            ELSE
               PLTQDP = .FALSE.
            ENDIF
            IF ( .NOT.PLTQDP ) THEN
               call XSL_LCPLOT()
            ELSE
               call XWRITE(
     &           ' Plotting of QDP files is no longer supported', 5)
               call XWRITE(
     &           ' Please use FITS files', 5)
            ENDIF
         ELSE
            call XWRITE(' No light curve has been '//
     &           'accumulated yet.',5)
         ENDIF
c ----------------------------------------------
c PLOT FFCURVE (plot whichever of the ffcurf is desired)
       ELSEIF(str1.eq.'MKF')then
       
         call XSL_FFORHK(nffpar,FFCURV,ffplis,ffcurf,plotdv,
     &                    keytim,tempfl,ECHO,MAXCUR)
c ----------------------------------------------
c PLOT HKCURVE (plot whichever of the hkcurf is desired)
       ELSEIF(str1.eq.'HK')then
       
         call XSL_FFORHK(npar,HKCURV,parlis,hkcurf,plotdv,
     &                    keytim,tempfl,ECHO,MAXCUR)
c ---------------------------------------------
c PLOT IMAGE (plot the raw data as an image)
      ELSEIF(str1.eq.'IMAGE')then
         call XSL_SAOIMAGE(0)
      ELSE
         call XWRITE(' XSELECT: Option not found',5)
      ENDIF

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_PLOT: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end


c
c
c ---------------------------------------------
      subroutine XSL_SAOIMAGE(mode)
c ---------------------------------------------
c 
c Called by XSELECT main
c
c     Alan Smale 1992 Nov

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(512) str1
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1
c ---------------------------------------------
      integer LENACT,mode

      status = 0
      IF(IMAGE)then
         call XSL_OPCF(cmdfil,ilun)
         IF(SMOOTH) THEN
            call XSL_COMSAO(str1,simfil,mode)
         ELSE
            call XSL_COMSAO(str1,imfil,mode)
         ENDIF
         len1 = LENACT( str1 )
         write(ilun,53) str1(1:len1)
         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)
      ELSE
         call XWRITE(' No image has been created yet.',5)
      ENDIF
      
 53   format(a)   
      return
      end

c ---------------------------------------------
      subroutine XSL_SET(mode)
c ---------------------------------------------
c Sets many things in xselect
c Mode = 0 is user driven mode
c Mode = 1 does set instrument
c Mode = 2 does set mission to the default mission
c Mode = 3 means don't reset xcolf and xcolh
c Mode = 4 means set mission to current value of keymis
c Mode = 5 means set inst to current value of instru
c Mode = 6 means set datamode to the current value of datamode
c
c   Jim Ingham 5/93

      include 'xsel.inc'
      include 'xselvar.inc'

c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(512) str1
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1
c ---------------------------------------------
      character(512) contxt
      character(16) skyord(3)
      integer MXCMDS
      parameter (MXCMDS = 19 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      integer nwhats,myindex,mode,xbinfold
      integer LENACT
      integer i
      logical DEFAUL, found, qregreb
      
      data (skyord(i),i=1,3)    /'SKY','DETECTOR','RAW'/
c These are the set commands:
      data commad /'BINSIZE',
     &     'DATA*DIR',
     &     'DATAMODE',
     &     'DEVICE',
     &     'DUMPCAT',
     &     'IMAGE',
     &     'INSTRUMENT',
     &     'MISSION',
     &     'MKFDIR',
     &     'OBSDIR',
     &     'PHAREBIN',
     &     'PAGEWIDTH',
     &     'PHANAME',
     &     'WMAPNAME',
     &     'WMAPBINSIZE',
     &     'XYBINSIZE',
     &     'XYNAME',
     &     'XYCENTER',
     &     'XYSIZE'/
      data comdes /'set the binsize for light curves',
     &     'set the data directory',
     &     'setup Xselect for particular data modes',
     &     'set the plot device',
     &     'set make obscat to display or not',
     &     'set the coordinates for the image to sky or det.',
     &     'set the instrument name',
     &     'set the mission name',
     &     'set the directory for the MKF file',
     &     'set the observation catalogue directory (default is cwd)',
     &     'set the pha bin size',
     &     'set the pagewidth for show commands',
     &     'set the column name for PHA',
     &     'set the column for the WMAP',
     &     'set the bin size for the weighted map',
     &     'set the bin size for the image',
     &     'sets the column for images',
     &     'set the center for the image',
     &     'sets the size of the image'/
      data nwhats / 19 /

      status = 0

      IF(mode.eq.1.or.mode.eq.5) THEN
         str1 = 'INSTRUMENT'
      ELSEIF(mode.eq. 2.or.mode.eq.3.or.mode.eq.4) THEN
         str1 = 'MISSION'
      ELSE IF(mode.eq.6) THEN
         str1 = 'DATAMODE'
      ELSE
         CALL xsl_match_cmd('set_what', commad, comdes, nwhats, 
     &                      MXCMDS, comno, str1, myindex, status)
         IF ( status .NE. 0 ) THEN
            status = 0
            return
         ENDIF
      ENDIF 
      status = 0
      IF(str1.eq.'BINSIZE') THEN
c -------------------------------------------------
c SET BINSIZE
         call XSL_GETDUMMYPARRT('binsize','set_str',binsiz,status)
         contxt = 'Failed to get binsize'
         IF ( status .NE. 0 ) GOTO 99999
c Now put it in the stored value
         call XSL_UCLPSR('stored_binsize',binsiz,status)
c If data has been read in, check that the binsize is not smaller than the
c TIMEDEL keyword.
         call XSL_CHKBINSIZE()
c Set the status back to 0 so this is not a fatal error:
         status = 0
      ELSE IF(str1.eq.'DATADIR') THEN
c -----------------------------------------------
c SET DATADIR
         call XSL_GET_DIR('data_dir','set_str',datdir,status)
         if(status.eq.0.and.datdir.ne.'NONE') THEN
c Echo the change:
            call XWRITE(' ',5)
            str1 = 'Setting data directory to '//datdir
            call XWRITE(str1,5)
c Now set the HKDIR:
            call xsl_uclgst('hk_dir',str1,status)
            contxt = 'Failed to get hk_dir'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_RELDIR(datdir,str1,hkdir,status)
c Finally set the MKF directory
            IF(mkfdir.eq.'NONE'.and.keymis.ne.'NONE'
     &           .and. mkfdnm.ne.'NONE') then
               mkfdir = datdir
               HAVEMKF = .TRUE.
c This is a simple way to do this, namely it is assumed that the MKF 
c files can be uniquely determined by some universal regular 
c expression, stored in the par file.
               call XSL_LIST(lstfil,mkfdnm,mkfdir,wrkdir,status)
               
c Now read in the list file:
               call GETLUN(ilun)
               call XSL_OPEN(ilun,lstfil,'OLD',' ',' ',0,0,status)
               read(ilun,'(a)',END=599) 
C I found one! So use datadir:
               str1 = 'Setting mkf directory to '//mkfdir
               call XWRITE(str1,5)
               call XWRITE(' ',5)
               close(ilun)
               call FRELUN(ilun)
               return

C Didn't find one, so try the relative path:         
 599           close(ilun)
               call FRELUN(ilun)
               call XSL_RELDIR(datdir,mkreld,mkfdir,status)
               if ( status.ne.0) then
                  call XWRITE('No MKF directory set',5)
                  call XWRITE(' ',5)
                  mkfdir = 'NONE'
                  HAVEMKF = .FALSE.
                  status = 0
                  return
               else
                  call XSL_LIST(lstfil,mkfdnm,mkfdir,wrkdir,status)
c Now read in the list file:
                  call GETLUN(ilun)
                  call XSL_OPEN(ilun,lstfil,'OLD',' ',' ',0,0,status)
                  read(ilun,'(a)',END=600) 
C I found one! So use datadir:
                  str1 = 'Setting mkf directory to '//mkfdir
                  call XWRITE(str1,5)
                  call XWRITE(' ',5)
                  close(ilun)
                  call FRELUN(ilun)
                  return
               endif
c I didn't find one here either...
 600           close(ilun)
               call FRELUN(ilun)
               call XWRITE('Could not find MKF files in '//
     &              'the datadir,'//
     &              'or in the default relative directory:',5)
               str1 = '    '//mkfdir
               call XWRITE(str1,5)
               call XWRITE('No MKF directory set',5)
               call XWRITE(' ',5)
               mkfdir = 'NONE'
               HAVEMKF = .FALSE.
               return               
            endif
         endif
      ELSE IF(str1 .eq. 'DEVICE') THEN
c ------------------------------------------------
c SET DEVICE      
c     Alan Smale 1993 March
         
         call XSL_GETDEV()
         IF(status.ne.0) THEN
            status = 0
            return
         ENDIF
c -------------------------------------------------------
c SET DUMPCAT
      ELSE IF(str1.eq.'DUMPCAT') THEN
         SHOWOC = .NOT. SHOWOC
         IF(SHOWOC) THEN
            call XWRITE('Obscat listing on',5)
         ELSE
            call XWRITE('Obscat listing off',5)
         ENDIF
c ------------------------------------------------------------
c SET IMAGE
      ELSE IF( str1 .eq. 'IMAGE') THEN
         status = 0
         call XSL_GETDUMMYPARST('image_coord','set_str',str1,status)
         contxt = 'Failed to get image_coord'
         IF ( status .NE. 0 ) GOTO 99999
         status = 0
         call UPC(str1)
         call gmatch(skyord,3,str1,myindex,status)
         IF(status.eq.1) THEN
            call XWRITE('Allowed values are SKY, DETECTOR, or RAW',
     &                      5)
            return
         ENDIF
c If you are actually switching coordinates, 
c then we should clear the region filters
         if(REGION.and.(xcolf.ne.keyx(myindex).or.
     &        ycolf.ne.keyy(myindex))) THEN
c Clear the region filters:
            call XSL_RMFILE( regfil )
            REGION = .FALSE.
            do i=1,numreg
               regvec(i) = ' '
            enddo
            numreg = 0
            call XWRITE(' ',5)
            call XWRITE('The IMAGE coordinates have changed.',5)
            call XWRITE
     &           ('This will invalidate your region filters,',5)
            call XWRITE('so I am clearing them.',5)
            call XWRITE(' ',5)
         endif
c set the appropriate keywords:
         xcolf = keyx(myindex)
         ycolf = keyy(myindex)
c Now set the appropriate size keyword:
         xfkey = keyxsz(myindex)
         yfkey = keyysz(myindex)
c -----------------------------------------------------------
c SET INSTRUMENT
      ELSE IF (str1 .eq. 'INSTRUMENT') THEN
         call XSL_INST_SET(mode)
         IF ( status .NE. 0 ) contxt = 'Failure in XSL_INST_SET'
c ---------------------------------------------------------------
c SET MISSION
      ELSE IF(str1 .eq. 'MISSION') THEN
         call XSL_MISSION_SET(mode)
         IF ( status .NE. 0 ) contxt = 'Failure in XSL_MISSION_SET'
c ------------------------------------------------------------
c SET MKF DIRECTORY
      ELSEIF( str1.eq.'MKFDIR') then
         call XSL_GET_DIR('mkf_dir','set_str',mkfdir,status)
         if(status.ne.0) then
            str1 = 'Setting MKF Directory to '//mkfdir
            call XWRITE(str1,5)
         ENDIF
c ------------------------------------------------------------
c SET DATAMODE
      ELSEIF( str1.eq.'DATAMODE') then
         call XSL_DATAMODE_SET(mode)
         IF ( status .NE. 0 ) contxt = 'Failure in XSL_DATAMODE_SET'
c ------------------------------------------------------------
c SET OBSDIR
      ELSE IF( str1.eq.'OBSDIR') THEN
         status = 0
c Get the catalogue directory

         call XSL_GETDUMMYPARST('obscat_dir','set_str',catdir,status)
         contxt = 'Failed to get obscat_dir'
         IF ( status .NE. 0 ) GOTO 99999
         IF(catdir.eq.'WORK'.or.catdir.eq.'work') THEN
            catdir = wrkdir
         ELSE
            call XSL_CHKDIR(catdir,status)
            if( status.eq.-10) then
               status = 0
 26            call xsl_uclgst('obscat_dir',catdir,status)
               contxt = 'Failed to get obscat_dir'
               IF ( status .NE. 0 ) GOTO 99999
               call XSL_CHKDIR(catdir,status)
               if( status.eq.-10) then
                  status = 0
                  goto 26
               else if ( status .ne. 0) then
                  len1 = LENACT(catdir)
                  str1 = 'OBSCAT Directory '//catdir(1:len1)//
     &                 ' doesn''t exist'
                  call XWRITE(str1,5)
                  return
               endif
            else if ( status .ne. 0) then
               len1 = LENACT(catdir)
               str1 = 'Obscat Directory '//catdir(1:len1)//
     &              ' doesn''t exist'
               call XWRITE(str1,5)
               return
            endif
         ENDIF

         call XSL_SET_OBSCATNAME()

         call xsl_uclgsb('make_default',DEFAUL,status)
         contxt = 'Failed to get make_default'
         IF ( status .NE. 0 ) GOTO 99999
         IF(DEFAUL) THEN
            call XSL_UCLPST('def_obscat_dir',catdir,status)
         ENDIF

c ------------------------------------------------------------
c SET PAGEWIDTH
      ELSE IF(str1 .eq. 'PAGEWIDTH') THEN
         status = 0
         call XSL_GETDUMMYPARIT('pagewidth','set_str',pgwth,status)
         contxt = 'Failed to get pagewidth'
         IF ( status .NE. 0 ) GOTO 99999
c ------------------------------------------------------------
c SET PHAREBIN
      ELSE IF(str1 .eq. 'PHAREBIN') THEN
         status = 0
         call XSL_GETDUMMYPARIT('pharebin','set_str',phabin,status)
         contxt = 'Failed to get pharebin'
         IF ( status .NE. 0 ) GOTO 99999
c Now put in the stored value:
         call XSL_UCLPSI('stored_pharebin',phabin,status)
c ------------------------------------------------------------
c SET PHANAME
      ELSE IF(str1 .eq. 'PHANAME') THEN
         status = 0
         call XSL_GETDUMMYPARST('phaname','set_str',keypha,status)
         contxt = 'Failed to get phaname'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(keypha)
         call XSL_CHKPHASIZE(0)
c ------------------------------------------------------------
c SET XYBINSIZE
      ELSE IF(str1 .eq. 'XYBINSIZE') THEN
         status = 0
         xbinfold = xbinf
         call XSL_GETDUMMYPARIT('xybinsize','set_str',xbinf,status) 
         contxt = 'Failed to get xybinsize'
         IF ( status .NE. 0 ) GOTO 99999
         IF ( REGION ) THEN
            CALL XSL_UCLGSB('rebinregion', qregreb, status)
            contxt = 'Failed to get rebinregion'
            IF ( status .NE. 0 ) GOTO 99999
            IF ( qregreb ) CALL XSL_REG_REBIN(xbinfold)
         ENDIF
c ------------------------------------------------------------
c SET XYNAME
      ELSE IF(str1 .eq. 'XYNAME') THEN
         status = 0
         call XSL_GETDUMMYPARST('xname','set_str',xcolf,status)
         contxt = 'Failed to get xname'
         IF ( status .NE. 0 ) GOTO 99999
         call XSL_GETDUMMYPARST('yname','set_str1',ycolf,status)
         contxt = 'Failed to get yname'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(xcolf)
         call UPC(ycolf)

c Now set the appropriate size keywords. If we don't know them assume
c that TLMAX works.

         found = .FALSE.
         DO i=1,MXCORD
            IF(xcolf.eq.keyx(i)) THEN
               xfkey = keyxsz(i)
               found = .TRUE.
            ENDIF
         ENDDO

         IF ( .NOT.found ) xfkey = 'TLMAX'

         found = .FALSE.
         DO i=1,MXCORD
            IF(ycolf.eq.keyy(i)) THEN
               yfkey = keyysz(i)
               found = .TRUE.
            ENDIF
         ENDDO

         IF ( .NOT.found ) yfkey = 'TLMAX'

c ------------------------------------------------------------
c SET XYCENTER
      ELSE IF (str1.eq.'XYCENTER') THEN
         len1 = xcf
         call XSL_GETDUMMYPARIT('xcenter','set_str',xcf,status)
         if(status.ne.0) then
            call XWRITE('Error reading xcenter',5)
            xcf = len1
            return
         endif
         len1 = ycf
         call XSL_GETDUMMYPARIT('ycenter','set_str1',ycf,status)
         if(status.ne.0) then
            call XWRITE('Error reading ycenter',5)
            ycf = len1
            return
         endif

         if(sizef.eq.-1) then
            call XWRITE(' ',5)
            call XWRITE('Remember to SET XYSIZE, or '//
     &           'the extractor will ignore the centering',5)
            call XWRITE(' ',5)
         endif

c ------------------------------------------------------------
c SET XYSIZE
      ELSE IF (str1.eq.'XYSIZE') THEN
         len1 = sizef
         call XSL_GETDUMMYPARIT('xysize','set_str',sizef,status)
         if(status.ne.0) then
            call XWRITE('Error reading xysize',5)
            sizef = len1
            return
         endif
         if(xcf.eq.0.and.ycf.eq.0) then
            call XSL_SET_XYCENTER()
         endif

c ------------------------------------------------------------
c SET WMAPBINSIZE
      ELSE IF(str1 .eq. 'WMAPBINSIZE') THEN
         status = 0
         call XSL_GETDUMMYPARIT('wmapbinsize','set_str',extrbinh,status)
         contxt = 'Failed to get wmapbinsize'
         IF ( status .NE. 0 ) GOTO 99999
c ------------------------------------------------------------
c SET WMAPNAME
      ELSE IF(str1 .eq. 'WMAPNAME') THEN
         CALL XWRITE(
     & 'Warning : downstream software assumes that the WMAP is', 5)
         CALL XWRITE(
     & '          in detector coordinates', 5)
         status = 0
         call XSL_GETDUMMYPARST('wxname','set_str',xcolh,status)
         contxt = 'Failed to get wxname'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(xcolh)
         status = 0
         call XSL_GETDUMMYPARST('wyname','set_str1',ycolh,status)
         contxt = 'Failed to get wyname'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(ycolh)
c Now set the appropriate size keyword:
         do i=1,MXCORD
            IF(xcolh.eq.keyx(i)) THEN
               xhkey = keyxsz(i)
               yhkey = keyysz(i)
               return
            ENDIF
         enddo
         call XWRITE('Coordinate keyword not found in '//
     &        'default list',5)
         call xsl_uclgst('set_new_xsiz',xhkey,status)
         contxt = 'Failed to get set_new_xsiz'
         IF ( status .NE. 0 ) GOTO 99999
         IF(xhkey .eq. 'quit') THEN
            call XWRITE('The coordinate keywords are no '//
     &           'longer set',5)
            call XWRITE('Try again, or do SET MISSION '//
     &           'to reset them',5)
         ENDIF
         call xsl_uclgst('set_new_ysiz',yhkey,status)
         contxt = 'Failed to get set_new_ysiz'
         IF ( status .NE. 0 ) GOTO 99999
         IF(yhkey .eq. 'quit') THEN
            call XWRITE('The coordinate keywords are no '//
     &           'longer set',5)
            call XWRITE('Try again, or do SET MISSION to '//
     &           'reset them',5)
         ENDIF
c ------------------------------------------------------------
c SET WHAT?
      ELSE
         call XWRITE('Unknown set command',5)
      ENDIF


99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i8)') ' XSL_SET: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end         

c     
c ---------------------------------------------
      subroutine XSL_SHOW()
c ---------------------------------------------
c     
c     Called by XSELECT main
c     
c     Alan Smale 1992 Nov

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(512) str1, str2, str3, str4
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2, len3, len4
c ---------------------------------------------      
      integer LENACT,htype
      integer i,j,max1,max2,max3,nwrite,datlen,hklen,rwmode,block
      character(512) contxt
      character(255) dashes,spaces,displi,hkext
      character(255) showdir,showfil,usenam,dumstr
      integer MXCMDS
      parameter (MXCMDS = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      integer nwhats,myindex
      character(6) rows
      logical dummy,answ,LIST
c     These are the show commands
      data commad /'DATA',
     &     'DIFF',
     &     'EVENT',
     &     'F*ILTERS',
     &     'GOODTIME',
     &     'GTI',
     &     'MKF',
     &     'OBSCAT',
     &     'PARAMETERS',
     &     'PRIMARY',
     &     'SELECT',
     &     'STATUS',
     &     8*'NA'/      
      data comdes /'show the data read in so far',
     &     'show the obscat in diff form',
     &     'display EVENT extensions of data files',
     &     'list the filters to be applied',
     &     'show accumulated timing selections from bin',
     &     'display GTI extensions from data files',
     &     'display the auxillary filter files (e.g. MKF for ASCA)',
     &     'display the catalogue',
     &     'list column names in event, obscat or mkf file',
     &     'display PRIMARY extensions of data files',
     &     'show selection expressions applied',
     &     'show current status of Xselect',
     &     8*'NA'/      
      data nwhats / 12 /

      CALL xsl_match_cmd('show_what', commad, comdes, nwhats, 
     &                   MXCMDS, comno, str1, myindex, status)
      IF ( status .NE. 0 ) THEN
         status = 0
         return
      ENDIF

c ---------------------------------------------
c SHOW DATA
      IF(str1.eq.'DATA')then

c     We must remove the directory strings, if they exist.  The + 1 is
c     because we put in the final '/' by hand.  So get the lengths:

         IF(BININ) THEN
            len2 = LENACT(evnin)
            call XSL_FILENAME(evnin,str2,len2)
            call XWRITE('Data is currently being read '//
     &           'from the file:',5)
            call XWRITE(' ',5)
            call XWRITE(str2(:len2),5)
            call XWRITE(' ',5)
            call XWRITE('The original data is:',5)
            call XWRITE(' ',5)
         ENDIF
         if(datdir.ne.'NONE') then
            datlen = LENACT(datdir) + 1
         else
            datlen = 0
         endif
         if(hkdir.ne.'NONE') then
            hklen = LENACT(hkdir) + 1
         else
            hklen = 0
         endif
         do i=1,255
            dashes(i:i) = '-'
            spaces(i:i) = ' '
         enddo

         IF ( READ.AND. .NOT. USEGTI )then
            IF(.NOT. HKREAD) THEN
C     If there are only Event files
               str3 = 'Data Directory is: '//datdir(1:datlen)
               call XWRITE(str3,5)
               call XWRITE(' ',5)
               call XWRITE(' Files currently in use: ',5)
               call XWRITE(' ',5)
               do i=1,NFILES
                  len1 = LENACT( filenm(i) )
                  call XSL_FILENAME(filenm(i),str2,len1)
                  write(str3,110) str2(:len1)//'+'//evtnum
 110              format(1x,80a)
                  call XWRITE(str3,5)
               end do
            ELSE
C     If there are Event and HK files           
               str3 = 'Data Directory is: '//datdir(1:datlen)
               call XWRITE(str3,5)
               str3 = 'HK Directory is: '//hkdir(1:hklen)
               call XWRITE(str3,5)
               call XWRITE(' ',5)
               call XWRITE(' Files currently in use: ',5)
               call XWRITE(' ',5)
               max1 = 12
               max2 = 12
c Find the longest file name.
               do i=1,nfiles
                  len1 = LENACT( filenm(i) ) - datlen
                  len2 = LENACT(hkflnm(i) ) - hklen
                  max1 = max(max1,len1)
                  max2 = max(max2,len2)
               enddo
c Add on space for the +evtnum...
               max1 = max1 + LENACT(evtnum) + 1
               if(max1.gt.10) then
                  len1 = (max1 - 10)/2
               else
                  len1 = 1
               endif
               if(max2.gt.8) then
                  len2 = (max2 - 8)/2
               else
                  len2 = 2
               endif
               write(str3,111) spaces(1:len1),spaces(1:len1),
     /              spaces(1:len2)
 111           format(1x,a,'Event Files',a,4x,a,'HK Files')
               call XWRITE(str3,5)
               write(str3,112) dashes(1:max1),dashes(1:max2)
 112           format(1x,a,4x,a)
               call XWRITE(str3,5)
               nwrite = max(nfiles,nhkfil)    
               do i = 1, nwrite
                  len1 = LENACT(filenm(i))
                  len2 = LENACT(hkflnm(i))
                  call XSL_FILENAME(filenm(i),str2,len1)
                  str2 = str2(:len1)//'+'//evtnum
                  call XSL_FILENAME(hkflnm(i),str4,len2)
                  write(str3,113) str2(:max1),str4(:max2)
 113              format(1x,a,4x,a)
                  call XWRITE(str3,5)
                  call XWRITE(' ',5)
               end do
            ENDIF
         ELSE IF (READ .AND. USEGTI) THEN
            IF(.NOT. HKREAD) THEN
C     If there are Event files and GTI files           
               str3 = 'Data Directory is: '//datdir(1:datlen)
               call XWRITE(str3,5)
               call XWRITE(' ',5)
               call XWRITE(' Files currently in use: ',5)
               call XWRITE(' ',5)
               max1 = 12
               max2 = 12
               do i=1,nfiles
                  len1 = LENACT( filenm(i) ) - datlen + 1
                  len2 = LENACT(gtifnm(i) ) - datlen + 1
                  max1 = max(max1,len1)
                  max2 = max(max2,len2)
               enddo
c Again, add on the evtnum (don't forget the '+':
               max1 = max1 + LENACT(evtnum) + 1
               if(max1.gt.10) then
                  len1 = (max1 - 10)/2
               else
                  len1 = 1
               endif
               if(max2.gt.9) then
                  len2 = (max2 - 9)/2
               else
                  len2 = 2
               endif
               write(str3,131) spaces(1:len1),spaces(1:len1),
     &              spaces(1:len2)
 131           format(1x,a,'Event Files',a,4x,a,'GTI Files')
               call XWRITE(str3,5)
               write(str3,132) dashes(1:max1),dashes(1:max2)
 132           format(1x,a,4x,a)
               call XWRITE(str3,5)    
               do i = 1, nfiles
                  len1 = LENACT(filenm(i))
                  len2 = LENACT(gtifnm(i))
                  call XSL_FILENAME(filenm(i),str2,len1)
                  str2 = str2(:len1)//'+'//evtnum
                  call XSL_FILENAME(gtifnm(i),str4,len2)
                  write(str3,133) str2(:max1),str4(:max2)
 133              format(1x,a,4x,a)
                  call XWRITE(str3,5)
               end do
               call XWRITE(' ',5)
            ELSE
C     If there are Event files, GTI files and HK files           
               str3 = 'Data Directory is: '//datdir(1:datlen)
               call XWRITE(str3,5)
               str3 = ' HK Directory is: '//hkdir(1:hklen)
               call XWRITE(str3,5)
               call XWRITE(' ',5)
               call XWRITE(' Files currently in use: ',5)
               call XWRITE(' ',5)
               max1 = 12
               max2 = 12
               max3 = 12
               do i=1,nfiles
                  len1 = LENACT( filenm(i) ) - datlen
                  len3 = LENACT(gtifnm(i) ) - datlen
                  len2 = LENACT(hkflnm(i) ) - hklen
                  max1 = max(max1,len1)
                  max2 = max(max2,len2)
                  max3 = max(max3,len3)
               enddo
               max1 = max1 + LENACT(evtnum) + 1
               if(max1.gt.10) then
                  len1 = (max1 - 10)/2
               else
                  len1 = 1
               endif
               if(max2.gt.8) then
                  len2 = (max2 - 8)/2
               else
                  len2 = 2
               endif
               if(max3.gt.9) then
                  len3 = (max3 - 9)/2
               else
                  len3 = 3
               endif
               nwrite = max(nfiles,nhkfil)
               if((7+max1+max2+max3+9) .le. 80) then
                  write(str3,141) spaces(1:len1),spaces(1:len1),
     &                 spaces(1:len2),spaces(1:len2),
     &                 spaces(1:len3)
 141              format(' Index ',a,'Event Files',a,4x,a,'HK files',
     &                 a,4x,a,'GTI Files')
                  call XWRITE(str3,5)
                  write(str3,142) dashes(1:max1),dashes(1:max2),
     &                 dashes(1:max3)
 142              format(' ----- ',a,4x,a,4x,a)
                  call XWRITE(str3,5)    
                  do i = 1, nwrite
                     len1 = LENACT(filenm(i))
                     len2 = LENACT(hkflnm(i))
                     len3 = LENACT(gtifnm(i))
                     call XSL_FILENAME(filenm(i),str2,len1)
                     str2 = str2(:len1)//'+'//evtnum
                     call XSL_FILENAME(gtifnm(i),str4,len3)
                     call XSL_FILENAME(hkflnm(i),str1,len2)
                     write(str3,143) catidx(i),str2(:max1),
     &                    str1(:max2),str4(:max3)
c                     write(str3,143)catidx(i),
c     &                    filenm(i)(datlen+1:datlen+max1),
c     &                    hkflnm(i)(hklen+1:hklen+max2),
c     &                    gtifnm(i)(datlen+1:datlen+max3)
 143                 format(' ',i5,' ',a,4x,a,4x,a)
                     call XWRITE(str3,5)
                  end do
               else
                  write(str3,171) spaces(1:len1),spaces(1:len1),
     /                 spaces(1:len2)
 171              format(' Index ',a,'Event Files',a,4x,a,'HK Files')
                  call XWRITE(str3,5)
                  write(str3,172) dashes(1:max1),dashes(1:max2)
 172              format(' ----- ',a,4x,a)
                  call XWRITE(str3,5)    
                  do i = 1, nfiles
                     len1 = LENACT(filenm(i))
                     len2 = LENACT(hkflnm(i))
                     call XSL_FILENAME(filenm(i),str2,len1)
                     str2 = str2(:len1)//'+'//evtnum
                     call XSL_FILENAME(hkflnm(i),str1,len2)
                     write(str3,173) catidx(i),str2(:max1),
     &                    str1(:max2)
c                     write(str3,173)catidx(i),
c     &                    filenm(i)(datlen+1:datlen+max1),
c     &                    hkflnm(i)(hklen+1:hklen+max2)
 173                 format(' ',i5,' ',a,4x,a)
                     call XWRITE(str3,5)
                  end do
                  call XWRITE(' ',5)
                  write(str3,151) spaces(1:len3)
 151              format(' Index ',a,'GTI Files')
                  call XWRITE(str3,5)
                  write(str3,153) dashes(1:max3)
 153              format(' ----- ',a)
                  call XWRITE(str3,5)
                  do i=1,nfiles
                     len1 = LENACT(gtifnm(i))
                     call XSL_FILENAME(gtifnm(i),str2,len1)
                     write(str3,155) catidx(i),str2(:max3)
 155                 format(' ',I5,' ',a)
                     call XWRITE(str3,5)
                  enddo
               endif  
               call XWRITE(' ',5)
            ENDIF
         ELSE IF ( HKREAD )then
C     If there are only HK files         
            call XWRITE(' HK Files currently in use: ',5)
            str3 = 'HK Directory is: '//hkdir(1:hklen)
            call XWRITE(str3,5)
            do i=1,nhkfil
               len1 = LENACT( hkflnm(i) )
               call XSL_FILENAME(hkflnm(i),str2,len2)
               write(str3,120) str2(:len2)
 120           format(1x,80a)
               call XWRITE(str3,5)
            end do
            call XWRITE(' ',5)
         ELSE
            call XWRITE(' No data has been read in yet.',5)
         ENDIF

c---------------------------------------------
c SHOW MKF
      ELSEIF(str1.eq.'MKF') THEN
c Get the list and rows parameters:
         call xsl_uclgst('show_str',str3,status)
         contxt = 'Failed to get show_str'
         IF ( status .NE. 0 ) GOTO 99999
 77      IF(str3.eq.'NOT_ENTERED') THEN
            LIST = .FALSE.
         ELSE IF(index(str3,'l').ne.0.or.index(str3,'L').ne.0) THEN
            LIST = .TRUE.
         ELSEIF(index(str3,'d').ne.0.or.index(str3,'D').ne.0) THEN
            LIST = .FALSE.
         ELSE IF(str3(1:1).eq.'q'.or.str3(1:1).eq.'Q')THEN
            return
         ELSE
            str3 = 'Error getting show method value, '//
     &           'try again (or type q to quit)'
            call XWRITE(str3,5)
            call xsl_uclgst('show_method',str3,status)
            contxt = 'Failed to get show_method'
            IF ( status .NE. 0 ) GOTO 99999
            goto 77
         ENDIF
         call xsl_uclgst('show_rows',rows,status)
         contxt = 'Failed to get show_rows'
         IF ( status .NE. 0 ) GOTO 99999
         
c  This is for the MKFILTER file
c     First deal with getting the filename of the mkf file:
c Check to make sure some data directory has been entered:

         call XSL_SET_DIR('data_dir',datdir,status)
         if (status.ne.0) then
            return
         endif
         call xsl_uclgst('mkf_name',ffilin,status)
         contxt = 'Failed to get mkf_name'
         IF ( status .NE. 0 ) GOTO 99999
         call XSL_FINDMKF(.FALSE.)
         if(status.ne.0) then
            status = 0
            call XWRITE('Error in SHOW FILE MKF from FIND MKF',5)
            return
         endif            
c Now get the list of parameters to show in the MKF file
         call XSL_GETDUMMYPARST('show_mkflist','show_str2',str3,status)
         contxt = 'Failed to get show_mkflist'
         IF ( status .NE. 0 ) GOTO 99999
         len1 = LENACT(str3)
c put quotes around the string, unless it is indirection:
         IF(str3(1:1).eq.'@') THEN
            continue
         ELSEIF(str3(1:1).ne.'"') THEN
            IF(str3(len1:len1).eq.'"') THEN
               str3 = '"'//str3(:len1)
            ELSE
               str3 = '"'//str3(:len1)//'"'
            ENDIF
         ELSEIF(str3(len1:len1).ne.'"') THEN
            str3 = str3(:len1)//'"'
         ENDIF
c Now dump the file
         IF(nmkf.eq.1) THEN
            call XWRITE(' ',5)
            call XWRITE('**** FILE '//
     &           mkfnam(1)(:LENACT(mkfnam(1)))//' ****',5)
            call XWRITE(' ',5)
            IF(LIST) THEN
               call XSL_FLIST(mkfnam(1),rows,'no')
            ELSE
               call XSL_FDUMP2(mkfnam(1),str3,rows,pgwth,'no')
            ENDIF
         ELSE
            write(str2,76) nmkf
 76         format('There are ',I2,' mkf files')
            call XWRITE(str2,5)
            do i=1,nmkf
               call XWRITE(' ',5)
               call XWRITE('**** FILE: '//
     &              mkfnam(i)(:LENACT(mkfnam(i)))//' ****',5)
               call XWRITE(' ',5)
               IF(LIST) THEN
                  call XSL_FLIST(mkfnam(i),rows,'no')
               ELSE
                  call XSL_FDUMP2(mkfnam(i),str3,rows,pgwth,'no')
               ENDIF
               IF(i.lt.nmkf) THEN
                  call xsl_uclgsb('show_answ',answ,status)
                  IF(.not.answ.or.status.ne.0) THEN
                     status = 0
                     call XWRITE(' ',5)
                     call XWRITE('         *** Done ***',5)
                     call XWRITE(' ',5)
                     return
                  ENDIF       
               ENDIF
            enddo
         ENDIF

c ---------------------------------------------
c SHOW EVENT, PRIMARY, GTI
      ELSEIF(str1.eq.'EVENT'.or.str1.eq.'PRIMARY'
     &        .or.str1.eq.'GTI') then
c Now any of the data files
         call XSL_GETDUMMYPARST('show_from','show_str',str2,status)
         contxt = 'Failed to get show_from'
         IF ( status .NE. 0 ) GOTO 99999
         CALL UPC(str2)
C Get the instrument name, quit if none has been chosen
         if(str2(1:1).eq.'O') then
            IF(.NOT.LOADED.OR. .NOT. MADE) THEN
               call xsl_uclgst('show_inst',str3,status)
               contxt = 'Failed to get show_inst'
               IF ( status .NE. 0 ) GOTO 99999
               if (str3.eq.'NONE') then
                  str3 = instru
               endif
               if (str3.eq.'NONE') then
                  call XWRITE('No instrument chosen',5)
                  return 
               endif
               CALL XSL_MDB_INST(keymis,submis,str3,obsno,status)
            ENDIF
         endif
         call XSL_GETDUMMYPARIT('show_which','show_str2',len1,status)
         contxt = 'Failed to get show_which'
         IF ( status .NE. 0 ) GOTO 99999
         call xsl_uclgst('show_str3',str4,status)
 78      IF(index(str4,'l').ne.0.or.index(str4,'L').ne.0) THEN
            LIST = .TRUE.
         ELSEIF(index(str4,'d').ne.0.or.index(str4,'D').ne.0) THEN
            LIST = .FALSE.
         ELSE IF(str4(1:1).eq.'q'.or.str4(1:1).eq.'Q')THEN
            return
         ELSE
            str4 = 'Error getting show_method value, '//
     &           'try again (or type q to quit)'
            call XWRITE(str4,5)
            call xsl_uclgst('show_method',str4,status)
            contxt = 'Failed to get show_method'
            IF ( status .NE. 0 ) GOTO 99999
            goto 78
         ENDIF
         call xsl_uclgst('show_rows',rows,status)
         contxt = 'Failed to get show_rows'
         IF ( status .NE. 0 ) GOTO 99999

C     Now get the catalogue name, if the data is from the obscat.
         IF(str2(1:1).eq.'O') then
            IF(LOADED .OR. MADE) THEN
               usenam = catnam
            ELSE
               CALL XSL_MDB_INST(keymis,submis,str3,obsno,status)
               if(obsno.lt.1) then
                  call XWRITE('No such instrument',5)
                  return
               endif
               usenam = obscat(obsno)
            ENDIF
c     Get and display the Datadir and Hkdir keywords:
            rwmode = 0
            status = 0
            call GETLUN(ilun)
            call FTOPEN(ilun, usenam, rwmode, block, status)
            if (status .eq. 103) then
               contxt='Unable to find Obscat file '//usenam
               call XWRITE(contxt,5)
               call FRELUN(ilun)
               return
            else if(status.ne.0) then
               contxt='Unable to open Obscat file '//usenam
               call XWRITE(contxt,5)
               write(contxt,51) status
 51            format(' FITSIO error no.: ',i3)
               call XWRITE(contxt,5)
c Don't use status here, we don't want to change its value on return
               len1 = 0
               call FTCLOS(ilun,len1)
               call FRELUN(ilun)
               return
            endif

c     go to first extension
            call FTMRHD(ilun, 1, htype, status)
            if(status .gt. 0) then
               contxt='Unable to move to ext#1 in '//usenam
               call XWRITE(contxt,5)
               len1=0
               call FTCLOS(ilun,len1)
               call FRELUN(ilun)
               return
            endif

c     Get the data or HK directories from the obscat:
            IF(str1(1:1).eq.'E'.or. str1(1:1).eq.'G'
     &           .or. str1(1:1).eq.'P') then
               call FTGKYS(ilun,'DATADIR',showdir,dumstr,status)
               if(status.ne.0) then
c
c     Catalogues need not have DATADIR in them
c     If not the data must be in the directory where the catalogue lives.
c     So go on.
c              
                  status = 0
                  IF ( datdir .eq. 'NONE' ) then
                     showdir = wrkdir
                  ELSE
                     showdir = datdir
                  ENDIF
               endif
            ELSEIF ( str1(1:1).eq.'H') then
               call FTGKYS(ilun,'HKDIR',showdir,dumstr,status)
               if(status.ne.0) then
                  status = 0
               endif
            ENDIF
            call FTGCNO(ilun,.TRUE.,'FILENAME',len2,status)
            if(status.ne.0) then
               status = 0
               call XWRITE(
     &              'FILENAME column not present in catalogue',5)
               len1=0
               call FTCLOS(ilun,len1)
               call FRELUN(ilun)
               return
            endif
            call FTGCVS(ilun,len2,len1,1,1,'NONE',str4,dummy,status)
            if(status.ne.0) then
               call XWRITE(
     &              'Could not get specified filename from catalogue',5)
               status = 0
               len1=0
               call FTCLOS(ilun,len1)
               call FRELUN(ilun)
               return
            endif
c Okay, now we are done with the obscat
c
            call FTCLOS(ilun,status)
            call FRELUN(ilun)

            call XSL_DATDIR(str4,showdir,0)

            IF(str1(1:1).eq.'E'.or.str1(1:1).eq.'P') THEN
               showfil = str4(:min(lenact(str4),len(showfil)))
            ELSE IF(str1(1:1).eq.'G') then
               showfil = str4(1:LENACT(str4))//'+'//gtinum
            ELSEIF(str4(1:1).eq.'H') then
               call xsl_uclgst('hkext',hkext,status)
               contxt = 'Failed to get hkext'
               IF ( status .NE. 0 ) GOTO 99999
               call xsl_uclgsi('hkdifflen',datlen,status)
               contxt = 'Failed to get hkdifflen'
               IF ( status .NE. 0 ) GOTO 99999
               len1 = LENACT(str4)
               showfil = str4(1:LENACT(str4)-datlen)//hkext
            ENDIF
         ELSE IF (str2(1:1).eq.'D') then
            if((str1(1:1).eq.'E'.or.str1(1:1).eq.'P').and.READ) then
               showfil = filenm(len1)
            elseif(str1(1:1).eq.'G'.and.USEGTI) then
               showfil = gtifnm(len1)
            elseif(str1(1:1).eq.'H'.and.HKREAD) then
               showfil = hkflnm(len1)
            else
               call XWRITE
     &              ('No files of the requested type read in',5)
               return
            endif 
         ENDIF
         IF(str1(1:1).eq.'E') then
c Use the evtnam keyword to find the event evtension
            showfil = showfil(:LENACT(showfil))//'+'//evtnum
         ELSE IF (str1(1:1).eq.'P') then
c This is always the zeroth extension.
            showfil = showfil(:LENACT(showfil))//'+0'
C               write(*,*) 'showfil = ',showfil
         ENDIF
         IF(LIST) THEN
            call XSL_FLIST(showfil,rows,'yes') 
         ELSE
            call XSL_FDUMP2(showfil,'-',rows,pgwth,'yes')
         ENDIF 

c ---------------------------------------------
c SHOW OBSCAT
      ELSEIF(str1.eq.'OBSCAT')then
c If you say show obscat list, you get the list output, but just
c show obscat still gets you the dump output.
         call xsl_uclgst('show_str',str3,status)
         contxt = 'Failed to get show_str'
         IF ( status .NE. 0 ) GOTO 99999
 81      IF(str3.eq.'NOT_ENTERED') THEN
            LIST = .FALSE.
         ELSE IF(index(str3,'l').ne.0.or.index(str3,'L').ne.0) THEN
            LIST = .TRUE.
         ELSEIF(index(str3,'d').ne.0.or.index(str3,'D').ne.0) THEN
            LIST = .FALSE.
         ELSE IF(str3(1:1).eq.'q'.or.str3(1:1).eq.'Q')THEN
            return
         ELSE
            str3 = 'Error getting list value, '//
     &           'try again (or type q to quit)'
            call XWRITE(str3,5)
            call xsl_uclgst('show_method',str3,status)
            contxt = 'Failed to get show_method'
            IF ( status .NE. 0 ) GOTO 99999
            goto 81
         ENDIF
         call xsl_uclgst('show_rows',rows,status)
         contxt = 'Failed to get show_rows'
         IF ( status .NE. 0 ) GOTO 99999
         IF(.NOT.LOADED) THEN
            call xsl_uclgst('show_inst',str3,status)
            contxt = 'Failed to get show_inst'
            IF ( status .NE. 0 ) GOTO 99999
            if(str3.eq.'NONE') then
               str3 = instru
            endif
            if (str3.eq.'NONE') then
               call XWRITE('No instrument chosen',5)
               return 
            endif
            call XSL_MDB_INST(keymis,submis,str3,obsno,status)
            if(obsno.lt.1) then
               call XWRITE('No such instrument',5)
               return
            endif
         ENDIF
         call XSL_DUMPCAT(LIST,rows,'no')

c ---------------------------------------------
c SHOW OBSCAT IN DIFF FORM
      ELSEIF(str1.eq.'DIFF')THEN
         IF(.NOT.MADE.AND..NOT. LOADED) then
            call xsl_uclgst('show_inst',str3,status)
            contxt = 'Failed to get show_inst'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_MDB_INST(keymis,submis,str3,obsno,status)
            if(str3.eq.'NONE') then
               str3 = instru
            endif
            if(str3.eq.'NONE') then
               call XWRITE('No instrument chosen',5)
               return 
            endif
            if(obsno.lt.1) then
               call XWRITE('No such instrument',5)
               return
            endif
            catnam = obscat(obsno)
         ENDIF
         call XSL_EXIST(catnam,ierr)

         IF (MADE .or. ierr.eq.0 )then
C     Set up FCATDIFF
c     First get the compare parameter
            call xsl_uclgst('compare',str2,status)
            contxt = 'Failed to get compare'
            IF ( status .NE. 0 ) GOTO 99999
c     If nothing, use the default:
            if(str2.eq.' ') then
               str2 = '"BIT_RATE DATAMODE"'
c     If indirect, make sure the file exists:
            else if (str2(1:1).eq.'@') then
               len2 = LENACT(str2)
               call XSL_EXIST(str2(2:len2),ierr)
               if(ierr.ne.0) then
                  call XWRITE('Compare file does not exist',5)
                  return
               endif
c     If a list, make sure it is surrounded by " "
            else if (str2(1:1).ne.'"') then
               len2 = LENACT(str2)
               if(str2(len2:len2).ne.'"') then
                  str2 = '"'//str2(:len2)//'"'
               else
                  str2 = '"'//str2(:len2)
               endif
            endif
c     Now get the copyover parameter, same comments as for compare
            call xsl_uclgst('copyover',str3,status)
            contxt = 'Failed to get copyover'
            IF ( status .NE. 0 ) GOTO 99999
            if(str3.eq.' ') then
               str3 = '"DATE-OBS TIME-OBS"'
            else if (str3(1:1).eq.'@') then
               len3 = LENACT(str3)
               call XSL_EXIST(str3(2:len3),ierr)
               if(ierr.ne.0) then
                  call XWRITE('Copyover file does not exist',5)
                  return
               endif
            else if (str3(1:1).ne.'"') then
               len2 = LENACT(str3)
               if(str3(len2:len2).ne.'"') then
                  str3 = '"'//str3(:len2)//'"'
               else
                  str3 = '"'//str3(:len2)
               endif
            endif
            
            len1 = LENACT(str2)
            len2 = LENACT(work2(1))
            len3 = LENACT(catnam)
            len4 = LENACT(str3)
            
            call XSL_RMFILE(work2(1))
            
            comlin = 'fcatdiff '//catnam(1:len3)//' '//
     &           work2(1)(1:len2)//' '//
     &           str2(1:len1)//' '//
     &           'copyover='//str3(1:len4)//' '//
     &           'exclude="-"'
            
            call XSL_OPCF(cmdfil,ilun)
C            len1 = LENACT(comlin)
            call XSL_WRTCF(ilun,comlin,1)
            call XSL_CLCF(ilun)
            call XSL_RUNCF(cmdfil,ECHO,status)
            IF(ierr.ne.0) THEN
               call XWRITE('Error in Fcatdiff',5)
               return
            ENDIF
            
c     First get the display list:

            displi = '-'
            call XSL_FDUMP(work2(1),displi,rows,pgwth,'no')
         ELSE
            call XWRITE(' No obs. cat. has been created.',5)
         ENDIF
         
c ---------------------------------------------
c SHOW PARAMS
      ELSEIF(str1.eq.'PARAMETERS')then
         call XSL_GETDUMMYPARST('show_param','show_str',str1,status)
         contxt = 'Failed to get show_param'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(str1)
         IF(str1(1:1).eq.'E') THEN         
c     Set up and run the FLCOL command to show the column names

            IF ( READ ) then
               IF(WORK) THEN
                  len1 = LENACT( work1(1) )
                  
                  str1='flcol '//
     +                 'infile = '//work1(1)(1:len1)//' '//
     +                 'nlist = 3 '//
     +                 'outfile = STDOUT'
               ELSE
                  len1 = LENACT( filenm(1) )
                  
                  str1='flcol '//
     +                 'infile = '//filenm(1)(1:len1)//' '//
     +                 'nlist = 3 '//
     +                 'outfile = '//'STDOUT' 
               ENDIF
            ELSE
               call XWRITE(' No data has been read in yet.',5)
            ENDIF
         ELSEIF(str1(1:1) .eq. 'M') THEN
            call XSL_FINDMKF(.FALSE.) 
            IF(status.ne.0) THEN
               call 
     &              XWRITE('Error in PARAM MKF from FIND MKF',5)
               status = 0
               return
            ENDIF
            len1 = LENACT( mkfnam(1) )
            
            str1='flcol '//
     +           'infile = '//mkfnam(1)(1:len1)//' '//
     +           'nlist = 3 '//
     +           'outfile = '//'STDOUT' 
            
         ELSEIF(str1(1:1) .eq. 'O') THEN
            IF(.not.LOADED) THEN
               IF(instru.eq.'NONE') THEN
                  call xsl_uclgst('show_inst',str3,status)
                  contxt = 'Failed to get show_inst'
                  IF ( status .NE. 0 ) GOTO 99999
                  call XSL_MDB_INST(keymis,submis,str3,obsno,status)
               ENDIF
               call XSL_EXIST(obscat(obsno),status)
               IF(status.ne.0) THEN
                  call XWRITE('Catalogue not found, use make '//
     &                 'obscat', 5)
                  status = 0
                  return
               ENDIF
               str1 = 'flcol infile = '//obscat(obsno)
            ELSE
               call XSL_EXIST(catnam,status)
               IF(status.ne.0) THEN
                  call XWRITE('Loaded catalogue not found',5)
                  status = 0
                  return
               ENDIF
               str1 = 'flcol infile = '//catnam
            ENDIF
            len1 = LENACT(str1)
            str1 = str1(:len1)//' '//
     +           'nlist = 3 '//
     +           'outfile = '//'STDOUT'
         ELSE
            call XWRITE('Options supported are:',5)
            call XWRITE('Events, Mkfilter and Obscat',5)
            return
         ENDIF
c     Now open the command file, write to it, and run it:	     
         call XSL_OPCF(cmdfil,ilun)
         len1 = LENACT( str1 )
         write(ilun,'(A)') str1(1:len1)
         call XSL_CLCF(ilun)
         
         call XSL_RUNCF(cmdfil,ECHO,status)
         
c ---------------------------------------------
c SHOW SELECT

      ELSEIF(str1.eq.'SELECT')then
         IF( SELCT )then
            call XWRITE(' The last event selection was: ',5)
            call XWRITE(strsel(nstrsel),5)
         ELSE
            call XWRITE(' No event selection has been made yet.',5)
         ENDIF
         IF( HKSEL )then
            call XWRITE(' The last hk selection was: ',5)
            call XWRITE( hkstr,5)
         ELSE
            call XWRITE(' No hk selection has been made yet.',5)
         ENDIF
         
c ---------------------------------------------
c SHOW STATUS

      ELSEIF(str1.eq.'STATUS'.or.str1.eq.'FILTERS')then
         IF(str1.eq.'STATUS') THEN
            call XWRITE(' ',5)
            call XWRITE
     &           ('              *** Status of XSELECT ***',5)
            call XWRITE(' ',5)            
            str2 = 'Plot device is '//plotdv(1:LENACT(plotdv))
            call XWRITE(str2,5)
            call XWRITE(' ',5)
            call XWRITE(' *** MISSION ***',5)
            call XWRITE(' ',5)
            str2 = keymis(1:LENACT(keymis))//' '//
     &             submis(1:LENACT(submis))//' '//
     &             instru(1:LENACT(instru))//' '//
     &             datamode(1:LENACT(datamode))
            call XWRITE(str2,5)

            WRITE(str2,'(a,a10,a,a10)') 'Time keyword is ', 
     &                 keytim(1:10), ' in units of ', keyuni(1:10)
            call XWRITE(str2,5)
            write(str2,260) binsiz
 260        format('Default timing binsize = ',g12.5)
            call XWRITE(str2,5)
            if(timedel.gt.0.0) then
               write(str1,557) timedel
 557           format('Minimum timing resolution: ',g12.5)
               call XWRITE(str1,5)
            endif

            IF ( keypha .NE. 'NONE' ) THEN
               WRITE(str2,'(a,a10,a,i5)') 'Energy keyword is ', 
     &                 keypha(1:10), ' with binning ', phabin
               call XWRITE(str2,5)
               if(READ .AND. phamin.LE.phamax) then
                  write(str2,266) keypha(1:10),phamin,phamax
 266              format('Max and min for ',a,': ',i5,'   ',i5)
                  call XWRITE(str2,5)
               endif
            ENDIF

            IF ( xcolf .NE. 'NONE' .AND. ycolf .NE. 'NONE' ) THEN
               write(str2,'(a,a10,1x,a10,1x,a,i4)') 
     &        ' Image keywords   = ', xcolf(1:10), ycolf(1:10), 
     &        ' with binning = ', xbinf
               call XWRITE(str2,5)
               if(sizef.gt.0) then
                  write(str2,'(a,I6,a,I6,a,I6)')
     &              'The image is centered on: ( ',xcf,' , ',ycf,
     &              ' ) with size ',sizef
                  call XWRITE(str2,5)
               endif
            ENDIF
            IF ( xcolh .NE. 'NONE' .AND. ycolh .NE. 'NONE' ) THEN
               write(str2,'(a,a10,1x,a10,1x,a,i4)') 
     &        ' WMAP  keywords   = ', xcolh(1:10), ycolh(1:10), 
     &        ' with binning = ', extrbinh
               call XWRITE(str2,5)
            ENDIF

            call XWRITE(' ',5)
            call XWRITE(' *** DATA ***',5)
            call XWRITE(' ',5)
            DUMMY = .TRUE.
            IF(datdir.ne.'NONE') THEN
               DUMMY = .FALSE.
               str2 = 'The data directory is '
     &              //datdir(:LENACT(datdir))
               call XWRITE(str2,5)
            ENDIF
            IF(mkfdir.ne.'NONE') THEN
               DUMMY = .FALSE.
               str2 = 'The mkf directory is '
     &              //mkfdir(:LENACT(mkfdir))
               call XWRITE(str2,5)
            ENDIF
            if( MADE ) THEN
               DUMMY = .FALSE.
               call XWRITE(' ',5)
               len1 = lenact(catnam)
               call XSL_FILENAME(catnam,str2,len1)
               str2 = 'The obscat '//str2(:len1)//
     &              ' has been made.'
               call XWRITE(str2,5)
               if (catflt.ne.'NONE') then
                  IF(catflt.ne.'DEF') THEN
                     len1 = LENACT(catflt)
                     str3 = ' Default selection expression : '
     &                    //catflt(:len1)
                  ELSE
                     call XSL_GET_DEFOBSEL(instru,datamode,submis,
     &                                     keymis,str1)
                     str3 = '  Default selection expression : '
     &                    //str1(:LENACT(str1))
                  ENDIF
                  call XWRITE(str3,5)
               endif
               IF(catsel.ne.'NONE') THEN
                  len1 = LENACT(catsel)
                  call XWRITE(' The following selection '//
     &                 'expression has been applied:',5)
                  call XWRITE('    '//catsel(:len1),5)
               ENDIF
            else if(LOADED) then
               DUMMY = .FALSE.
               call XWRITE(' ',5)
               len1 = LENACT(catnam)
               call XWRITE('The catalogue: ',5)
               str3 = '   '//catnam(:len1)
               call XWRITE(str3,5)
               call XWRITE('has been loaded.',5)
               IF(catsel.ne.'NONE') THEN
                  len1 = LENACT(catsel)
                  call XWRITE(' The following selection '//
     &                 'expression has been applied:',5)
                  call XWRITE('    '//catsel(:len1),5)
               ENDIF
            ENDIF
            if( READ )THEN
               call XWRITE(' ',5)
               IF(MANY) THEN
                  write(str1,57) nfiles
 57               format('   ',i3,' data files have been read in.')
                  call XWRITE(str1,5)
               ELSE
                  call XWRITE('One data file has been read in.',5)
               ENDIF
               
            ENDIF
            if(MERGED)call XWRITE('Files have been merged. ',5)
            if( WORK )call XWRITE('Workspace files are in use.',5)
            
            if( WRKGTI ) call XWRITE(
     &           'GTI workspace files are in use.',5)
            
            if( USEGTI ) call XWRITE('GTI files are in use.',5)
            
            if( FILTER ) THEN
               call XWRITE('The data has been filtered.',5)
            ENDIF
            if( HKREAD ) THEN
               call XWRITE('HK files have been read in. ',5)
               DUMMY = .FALSE.
            endif
            if( EXPAND )call XWRITE('HK files are expanded.  ',5)
            if( MANYHK )call XWRITE('More than one HK file. ',5)
            IF(DUMMY) THEN
               call XWRITE('     NONE',5)
            ENDIF
            call XWRITE(' ',5)

            call XWRITE(' *** PRODUCTS ***',5)
            DUMMY = .TRUE.
            call XWRITE(' ',5)
            if(HKCURV) then
               call XWRITE('HK curves have been accumulated',5)
            endif
            if(FFCURV) then
               call XWRITE(' MKF curves have been '//
     &              'accumulated for:',5)
               do i=1,nffpar
                  write(str1,79) ffplis(i)(:LENACT(ffplis(i)))
 79               format('      ',a)
                  call XWRITE(str1,5)
               enddo     
            endif
            if(FAST) then
               call XWRITE('Fast timing corrections performed.',5)
               DUMMY = .FALSE.
            endif
            if(FAINT) then
               call XWRITE
     &              ('Faint to Bright conversion performed.',5)
               if( usrdfe.ne.'-') then
                  len1 = lenact(usrdfe)
                  call XSL_FILENAME(usrdfe,str1,len1)
                  str1 = '   DFE file used: '//str1(:len1)
                  call XWRITE(str1,5)
               endif
            endif
            if( SPEC )then
               call XWRITE(' A spectrum has been accumulated. ',5)
               DUMMY = .FALSE.
            endif
            if( CURV )then
               call XWRITE(' A curve has been accumulated. ',5)
               DUMMY = .FALSE.
            endif
            IF(UNBIN) THEN
               call XWRITE('An unbinned light curve has been '//
     &              'accumulated',5)
               DUMMY = .FALSE.
            ENDIF
            if( IMAGE )then
               call XWRITE(' An image has been accumulated. ',5)
               DUMMY = .FALSE.
            endif
            if(SMOOTH)then
               str1 = ' The image has been smoothed using a '
     &              //smethod
               call XWRITE(str1,5)
               if(smethod(1:1).eq.'G') then
                  write(str1,558) sigma,nsigma
 558              format('    sigma = ',f7.2,';  nsigma = ',i3)
                  call XWRITE(str1,5)
               else if(smethod(1:1).eq.'B') then
                  write(str1,559) xwindow,ywindow
 559              format('    xwindow = ',F7.2,';  ywindow = ',F7.2)
                  call XWRITE(str1,5)
               endif
               str1 = '    Boundary method: '//bound
               if(bound(1:1).eq.'c') then
                  write(str1,569) const
 569              format('    Boundary method: constant; const = ',i4)
                  call XWRITE(str1,5)
               endif
            endif
            if(BINOUT) then
               call XWRITE(' An output event list has '//
     &              'been written',5)
            endif

            IF(BININ) THEN
               len1 = LENACT(evnin)
               call XSL_FILENAME(evnin,str1,len1)
               call XWRITE('Data being read in from '//
     &              str1(:len1),5)
            ENDIF
            IF(DUMMY) THEN
               call XWRITE('     NONE',5)
            ENDIF
         ENDIF
         call XWRITE(' ',5)
         call XWRITE(' *** SELECTIONS ***',5)
         call XWRITE(' ',5)
         DUMMY = .TRUE.
         if( SELCT) THEN
            IF(nstrsel.eq.1) THEN
               call XWRITE(' An event selection has been made. ',5)
               call XWRITE(' The selection expression is:',5)
               str3 = '    '//strsel(1)(:min(lenact(strsel(1)),
     &                                       len(str3)-4))
               call XWRITE(str3,5)
            ELSE
                call XWRITE
     &              (' Event selections have been made. ',5)
                call XWRITE(' The selection expressions were:',5)   
                do i=1,nstrsel
                   str3 = '    '//strsel(i)(:min(lenact(strsel(i)),
     &                                       len(str3)-4))
                   call XWRITE(str3,5)
                enddo
             ENDIF
            DUMMY = .FALSE.
         endif
         if( HKSEL) THEN
            call XWRITE(' An HK selection has been made. ',5)
            len1 = LENACT(hkstr)
            str3 = 'The selection expression is : '//hkstr(:len1)
            call XWRITE(str3,5)
            DUMMY = .FALSE.
         endif
         if(FFTFL) THEN
            len2 = LENACT(ffilin)
            if(nffstr.eq.1) THEN
               str3 =
     &              ' A filter file selection has been made from file:'
            else
               str3 =
     &              ' Filter file selections have been made from file:'
            endif
            call XWRITE(str3,5)
            do i=1,nmkf
               len1 = lenact(mkfnam(i))
               call XSL_FILENAME(mkfnam(i),str1,len1)
               str1 = '   '//str1(:len1)
               call XWRITE(str1,5)
            enddo
            if(nffstr.eq.1)then
               call XWRITE(' The selection expression was: ',5)
               str3 ='   '//ffstr(1)(:min(lenact(ffstr(1)),
     &                                       len(str3)-4))
               call XWRITE(str3,5)
            else
               call XWRITE(' The selection expressions were:',5)
               do i=1,nffstr
                  str3 = '   '//ffstr(i)(:min(lenact(ffstr(i)),
     &                                       len(str3)-4))
                  call XWRITE(str3,5)
               enddo
            endif
            DUMMY = .FALSE.
         endif
         IF(DUMMY) THEN
            call XWRITE('     NONE',5)
         ENDIF
         call XWRITE(' ',5)
c     You need to check for numgti.gt.o because FITTFL=TRUE may be from
c     HKSEL or FFTFL.
         call XWRITE(' *** FILTERS ***',5)
         call XWRITE(' ',5)
         DUMMY = .TRUE.
         IF(FITTFL.and.numgti.gt.0) THEN
            call XWRITE(' Fits timing files have been read from:',
     &           5)
            do i = 1,numgti
               len1 = LENACT(gtivec(i))
               call XSL_FILENAME(gtivec(i),str1,len1)
               call XWRITE('    '//str1(:len1),5)
            enddo
            DUMMY = .FALSE.
         ENDIF
         if(FITTFL.and.numcti.gt.0) then
            call XWRITE
     &           (' Cursor selections have been written to:',5)
            if(numcti.gt.1) then
               call XWRITE
     &              ('  The list is in reverse order of entry',5)
            endif
            do i = numcti,1,-1
               do j=1,MXNSEL
                  if(ctindx(j).eq.i) then
                     len1 = LENACT(ctivec(j))
                     call XSL_FILENAME(ctivec(j),str1,len1)
                     call XWRITE('    '//str1(:len1),5)
                  endif
               enddo
            enddo
            DUMMY = .FALSE.
         endif
         if(FITTFL.and.numhnd.gt.0) then
            call XWRITE
     &           (' Keyboard timing selections have been written to:',
     &           5)
            if(numhnd.gt.1) then
               call XWRITE
     &              ('The list is in reverse order of entry',5)
            endif
            do i = numhnd,1,-1
               do j=1,MXNSEL
                  if(hndndx(j).eq.i) then
                     len1 = LENACT(hndvec(j))
                     call XSL_FILENAME(hndvec(j),str1,len1)
                     call XWRITE('    '//str1(:len1),5)
                  endif
               enddo
            enddo
            DUMMY = .FALSE.
         endif
                  
         IF(ASCTFL.and.numasc.gt.0) THEN
            call XWRITE(' Ascii timing selections '//
     &           'have been read from:',5)
            do i = 1,numasc
               len1 = LENACT(ascvec(i))
               call XSL_FILENAME(ascvec(i),str1,len1)
               call XWRITE('    '//str1(:len1),5)
            enddo
            DUMMY = .FALSE.
         ENDIF
         IF(XWNTFL) THEN
            call XWRITE(' A Xronos window file has been read in:',
     &           5)
            len1 = LENACT(xwnflt)
            call XSL_FILENAME(xwnflt,str1,len1)
            call XWRITE('    '//str1(:len1),5)
            DUMMY = .FALSE.
         ENDIF
         IF(XPHTFL) THEN
            call XWRITE(' A phase selection has been entered:',5)
            write(str1,*) '   Epoch:  ',epoch
            call XWRITE(str1,5)
            write(str1,*) '   Period: ',period
            call XWRITE(str1,5)
            str1 = '       START      STOP'
            call XWRITE(str1,5)
            str1 = '     --------   --------'
            call XWRITE(str1,5)
            do i=1,nphase
               write(str1,311) phase(1,i),phase(2,i)
               call XWRITE(str1,5)
            enddo
 311        format('     ',f8.5,'   ',f8.5)
            call XWRITE(' ',5)
            DUMMY = .FALSE.
         ENDIF
         IF(INTENS) THEN
            if(numint.eq.1) THEN
               call XWRITE('An intensity filter has been entered'//
     &              ' using the expression: ',5)
            else
               call XWRITE('Intensity filters have been entered'//
     &              ' using the expressions: ',5)
            endif
            do i = 1, numint
               str1 = ' * '//intexp(i)
               call XWRITE(str1,5)
            enddo
            DUMMY = .FALSE.
         ENDIF
         IF ( phalcut .NE. -20 .AND. phahcut .NE. -20 ) THEN
            WRITE(str1,'(a,i5,a,i5,a)') 'A PHA filter from ', phalcut,
     &             ' to ', phahcut, ' has been entered'
            CALL xwrite(str1, 5)
            CALL xwrite(' ', 5)
            DUMMY = .FALSE.
         ENDIF
         IF ( gfilter .NE. 'NONE' ) THEN
            str1 = 'A grade filter of '//gfilter(:lenact(gfilter))//
     &             ' has been entered'
            CALL xwrite(str1, 5)
            CALL xwrite(' ', 5)
            DUMMY = .FALSE.
         ENDIF
         IF ( colfilter .NE. 'NONE' .AND. 
     &        lenact(colfilter) .NE. 0 ) THEN
            str1 = 'A column filter of '//
     &             colfilter(:lenact(colfilter))//' has been entered'
            CALL xwrite(str1, 5)
            CALL xwrite(' ', 5)
            DUMMY = .FALSE.
         ENDIF

         IF( CLEAN ) then
            if(index(instru,'SIS').ne.0) THEN
               IF (.not. DIRTY) THEN
                  call XWRITE(
     &                 ' A Hot pixel removal has  been done',5)
               ELSE
                  call XWRITE('A Hot Pixel events list has'//
     &                 ' been extracted',5)
               ENDIF
               IF(clnmet.eq.1) THEN
                  write(str3,121) bkgthr
 121              format(' threshold = ',i3)
                  call XWRITE(str3,5)
               ELSE IF (clnmet.eq.2) THEN
                  write(str3,122) cellsz,logprb,bkgthr
 122              format('cellsz = ',i3,' log prob = ',
     &                 f8.3,' threshold = ',i3)
                  call XWRITE(str3,5)
               ENDIF
               if(cphal.gt.phamin.or.cphah.lt.phamax) then
                  write(str3,123) cphal,cphah
 123              format('SISCLEAN PHA lower cutoff = ',i4,
     &                 ' PHA upper cutoff = ',i4)
                  call XWRITE(str3,5)
               endif
               DUMMY = .FALSE.
            ELSE IF(index(instru,'SIS').ne.0) THEN
               call XWRITE('GIS Rise Time background subtraction'//
     &              ' has been effected.',5)
            ENDIF
         ENDIF 
         IF(REGION) then
            call XWRITE(
     &           ' Spatial selections have been read in from:',5)
            do i = 1,numreg
               len1 = LENACT(regvec(i))
               call XSL_FILENAME(regvec(i),str1,len1)
               call XWRITE('    '//str1(:len1),5)
            enddo
            DUMMY = .FALSE.
         ENDIF
         IF(DETFL) then
            call XWRITE(
     &           ' Detector filtering:',5)
            do i = 1,numdet
               len1 = LENACT(detvec(i))
               call XWRITE('    '//detvec(i)(:len1),5)
            enddo
            DUMMY = .FALSE.
         ENDIF
         IF(phalcut.gt.phamin.or.
     &        (phahcut.gt.0.and.phahcut.lt.phamax)) THEN
            write(str3,20) phalcut, phahcut
 20         format('PHA Lower cutoff = ',i4,' PHA Upper cutoff = ',i4)
            call XWRITE(str3,5)
            DUMMY = .FALSE.
         ENDIF
         IF(DUMMY) THEN
            call XWRITE('     NONE',5)
         ENDIF
         call XWRITE('   ',5)
c ---------------------------------------------
c SHOW timing selection               
      ELSE IF (str1.eq.'GOODTIME') THEN
         call XSL_EXIST(ascout,ierr)
         IF(ierr.ne.0) THEN
            call XWRITE('Run bin to see the accumulated timing '
     &           //'selection',5) 
            return
         ELSE
            call XSL_MORE(ascout)
         ENDIF
c ---------------------------------------------
c SHOW huh?
      ELSE
         call XWRITE(' XSELECT: Option not found',5)
      ENDIF

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i8)') ' XSL_SHOW: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end

c
c
c ---------------------------------------------
      subroutine XSL_SMOOTH()
c ---------------------------------------------
c This uses the fgauss task to smooth images.
c J. Ingham 3/94
c
      include 'xsel.inc'
      include 'xselvar.inc'

      character(3) old,new
      character(1024) comlin
      character(16) smwhat
      character(1) outtype
      character(512) str1, contxt
      integer MXCMDS
      parameter (MXCMDS = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      integer nwhats,myindex,LENACT,ilun

c     These are the smooth commands - at the moment IMAGE is the only
c     possibility
      data commad /'IMAGE',
     &     19*'NA'/      
      data comdes /'smooth the current image',
     &     19*'NA'/      
      data nwhats / 1 /

      CALL xsl_match_cmd('smooth_what', commad, comdes, nwhats, 
     &                   MXCMDS, comno, smwhat, myindex, status)
      IF ( status .NE. 0 ) THEN
         status = 0
         return
      ENDIF
      
      IF(smwhat .eq.'IMAGE') THEN
         if(.NOT.IMAGE) THEN
            call XWRITE('No image accumulated',5)
            return
         endif
         call XSL_UCLGOT('smooth_method',status)
         IF(status.ne.0) THEN
            status = 0
            call XWRITE('The available methods are:',5)
            call XWRITE('   Boxcar',5)
            call XWRITE('   Gaussian',5)
            call XWRITE('   Lorentzian',5)
            call XWRITE(' ',5)
         ENDIF
         call xsl_uclgst('smooth_method',smethod,status)
         contxt = 'Failed to get smooth_method'
         IF ( status .NE. 0 ) GOTO 99999
        

         IF(smethod(1:1).eq.'G'.or.smethod(1:1).eq.'g') THEN
            smethod = 'Gaussian'
            call XSL_GETDUMMYPARIT('sigma','smooth_str',sigma,status)
            contxt = 'Failed to get sigma'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_GETDUMMYPARIT('nsigma','smooth_str2',nsigma,status)
            contxt = 'Failed to get nsigma'
            IF ( status .NE. 0 ) GOTO 99999
            write(comlin,117) imfil(:LENACT(imfil)),
     &           simfil(:LENACT(simfil)),sigma,nsigma
 117        format('fgauss infile=',a,' outfile=',a,' sigma=',1pg11.4,
     &           ' nsigma=',i4,' ratio=1.0 theta=0.0')
         ELSE IF(smethod(1:1).eq.'L'.or.smethod(1:1).eq.'l') THEN
            smethod = 'Lorentzian'
            call XSL_GETDUMMYPARIT('sigma','smooth_str',sigma,status)
            contxt = 'Failed to get sigma'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_GETDUMMYPARIT('nsigma','smooth_str2',nsigma,status)
            contxt = 'Failed to get nsigma'
            IF ( status .NE. 0 ) GOTO 99999
            write(comlin,118) imfil(:LENACT(imfil)),
     &           simfil(:LENACT(simfil)),sigma,nsigma
 118        format('florentz infile=',a,' outfile=',a,' sigma=',
     &           1pg11.4,' nsigma=',i4,' ratio=1.0 theta=0.0')
         ELSEIF(smethod(1:1).eq.'B'.or.smethod(1:1).eq.'b') THEN
            smethod = 'Boxcar'
            call XSL_GETDUMMYPARRT('xwindow','smooth_str',xwindow,
     &                             status)
            contxt = 'Failed to get xwindow'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_GETDUMMYPARRT('ywindow','smooth_str2',ywindow,
     &                             status)
            contxt = 'Failed to get ywindow'
            IF ( status .NE. 0 ) GOTO 99999
            write(comlin,119) imfil(:LENACT(imfil)),
     &           simfil(:LENACT(simfil)),xwindow,ywindow
 119        format('fboxcar infile=',a,' outfile=',a,
     &           ' xwindow=',F7.2,' ywindow=',F7.2)
         ELSE
            call XWRITE('Unrecognized option in smooth_method.',5)
            status = -12
            return
         ENDIF
 544     call xsl_uclgst('boundary',bound,status)
         contxt = 'Failed to get boundary'
         IF ( status .NE. 0 ) GOTO 99999
         if(bound.eq.'?') then
            call XWRITE('The options are:',5)
            call XWRITE('   nearest  - Use the value of the '//
     &           'nearest boundary pixel',5)
            call XWRITE('   reflect  - Generate a value by '//
     &           'reflecting around the boundary',5)
            call XWRITE('   constant - Use a constant value',5)
            call XWRITE('   wrap     - Generate a value by '//
     &           'wrapping the image',5)
            goto 544
         endif
         IF(bound(1:1).eq.'n'.or.bound(1:1).eq.'N') THEN
            bound = 'nearest'
            str1 = ' boundary = nearest'
         ELSE IF(bound(1:1).eq.'c'.or.bound(1:1).eq.'C') THEN
            bound = 'constant'
            call xsl_uclgsi('smooth_constant',const,status)
            contxt = 'Failed to get smooth_constant'
            IF ( status .NE. 0 ) GOTO 99999
            write(str1,127) const
 127        format(' boundary = constant constant = ',I4)
         ELSE IF(bound(1:1).eq.'r'.or.bound(1:1).eq.'R') THEN
            bound = 'reflect'
            str1 = ' boundary = reflect'
         ELSE IF(bound(1:1).eq.'w'.or.bound(1:1).eq.'W') THEN
            str1 = ' boundary = wrap'
            bound = 'wrap'
         ELSE
            call XWRITE('Unrecognized boundary method',5)
            status = -11
            return
         ENDIF
         call xsl_uclgst('smooth_outtype',outtype,status)
         contxt = 'Failed to get smooth_outtype'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(outtype)
         if(outtype.ne.'-'.and.outtype.ne.' '.and.outtype.ne.'I'
     &        .and.outtype.ne.'J'.and.outtype.ne.'B'.and.outtype.ne.'E'
     &        .and.outtype.ne.'D') THEN
            call XWRITE('Unrecognized output type',5)
            new = 'ql'
            call XPIPARMODE('smooth_outtype',new,old,status)
            call xsl_uclgst('smooth_outtype',outtype,status)
            contxt = 'Failed to get smooth_outtype'
            IF ( status .NE. 0 ) RETURN
            call XPIPARMODE('smooth_outtype',old,new,status)
         ENDIF
         
         comlin = comlin(:LENACT(comlin))//str1(:LENACT(str1))//
     &        ' datatype='//outtype//' nullval=0.0 '//
     &        'copyprime=yes copyall=yes'
         
         
         call XSL_OPCF(cmdfil,ilun)
         call XSL_WRTCF(ilun,comlin,1)
         call XSL_CLCF(ilun)

C Now warn about overwriting: Nick says no.
c         IF(SMOOTH.and. .NOT. SAVSMOOTH) THEN
c            call XWRITE('The new smoothed image will '//
c     &           'overwrite the previous smoothed image.',5)
c            call xsl_uclgsb('save_file',ANSW,status)
c            IF(ANSW) THEN
c               call xsl_uclgst('outfile',str1,status)
c               call XSL_XTEND(str1, '-smooth',status)
c               call XSL_COPY(simfil,str1,status)
c            ENDIF
c         ENDIF
         call XSL_RMFILE(simfil)
         SMOOTH = .FALSE.
         call XSL_RUNCF(cmdfil,ECHO,status)

         IF(status.eq.0) THEN
            SMOOTH = .TRUE.
            SAVSMOOTH = .FALSE.
         ELSE
            call XWRITE('Error in smoothing image',5)
            status = -10
            return
         ENDIF

      ELSE
         call XWRITE('Unrecognized option for smooth',5)
         status = -10
         return
      ENDIF

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i8)') ' XSL_SMOOTH: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_INST_SET(mode)
c ---------------------------------------------
c This sets the instrument, querying the par file and all...
c J Ingham 5/94
c Mode = 0 is user driven mode
c Mode = 1 does set instrument
c Mode = 3 means don't reset xcolf and xcolh
c Mode = 5 means set inst to current value of instru

      include 'xsel.inc'
      include 'xselvar.inc'

      integer mode,LENACT
      character(512) str1,str3,contxt
      character(255) str2

      str2 = instru
      IF(mode.ne.5) THEN
         IF(mode.eq.0) then
            call xsl_uclgst('set_str',instru,status)
            contxt = 'Failed to get set_str parameter'
            IF ( status .NE. 0 ) GOTO 99999
            IF(instru .eq. 'NOT_ENTERED')THEN
               CALL XSL_MDBS(keymis, ' ', ' ', ' ', 'instruments', str1,
     &                        status)
               call XWRITE('The available instruments are:',5)
               CALL XWRITE(str1, 5)
               call xsl_uclgst('instrument',instru,status)
               contxt = 'Failed to get instrument parameter'
               IF ( status .NE. 0 ) GOTO 99999
            ELSE
               call XSL_UCLPST('instrument',instru,status)
               contxt = 'Failed to put instrument parameter'
               IF ( status .NE. 0 ) GOTO 99999
            ENDIF
         ELSE if ( mode.eq.1 ) THEN
            call xsl_uclgst('instrument',instru,status)
            contxt = 'Failed to get instrument parameter'
            IF ( status .NE. 0 ) GOTO 99999
         ENDIF
         CALL XSL_MDB_INST(keymis, submis, instru, obsno, status)
         contxt = 'Failed to identify instrument in MDB'
         IF ( status .NE. 0 ) GOTO 99999
      ENDIF

c Make sure instrument can be matched and set the obsno (index for detector)

      CALL XSL_MDB_INST(keymis, submis, instru, obsno, status)
      contxt = 'Failed to identify instrument in MDB'
      IF ( status .NE. 0 ) GOTO 99999

c If you are changing instruments, do a clear, but don't unset datdir:
c But if no instrument is set, don't do this...
      IF(str2.ne.instru.and.str2.ne.'NONE') THEN
         str2 = instru
         str1 = datdir
         str3 = mkfdir
         call XSL_CLEAR(1)
         instru = str2
         datdir = str1(:min(lenact(str1),len(datdir)))
         mkfdir = str3(:min(lenact(str3),len(mkfdir)))
      ENDIF

C set all the parameters from the MDB. Note that status is set in the common
C block by xsl_mdb_set

      CALL XSL_MDB_SET()
      contxt = 'Failure in XSL_MDB_SET'
      IF ( status .NE. 0 ) GOTO 99999

C Set the xselect prompt

      prompt = prefix(:LENACT(prefix))
      IF(keymis.ne.'NONE') THEN
         prompt = prompt(:LENACT(prompt))//':' 
     &        //keymis(:LENACT(keymis))
      ENDIF
      IF(instru.ne.'NONE') then
         prompt = prompt(:LENACT(prompt))//'-'//
     &        instru(:LENACT(instru))
         IF(datamode.ne.'NONE') then
            prompt = prompt(:LENACT(prompt))//'-'//
     &           datamode(:LENACT(datamode))
         ENDIF          
      ENDIF          
      prompt = prompt(:LENACT(prompt))//' >'

      MADE = .FALSE.

99999 IF ( status .NE. 0 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt,'(a,i4)') 'XSL_INST_SET: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end
c

c ------------------------------------------------------------
      SUBROUTINE XSL_SET_OBSCATNAME()
c ------------------------------------------------------------
c This sets the default obscat names.
c J. Ingham 10/94

      include 'xsel.inc'
      include 'xselvar.inc'

      integer len1, len2, LENACT
      INTEGER iflag, idelim, lenn, lenb, lene, idet

      CHARACTER(255) detstr

      LOGICAL qdone, qskip

c Get the instruments string for the current mission

      CALL XSL_MDBS(keymis, ' ', ' ', ' ', 'instruments', detstr, 
     &              status)
      IF ( status .NE. 0 ) THEN
         CALL XWRITE('Failed to get instruments from MDB', 5)
         RETURN
      ENDIF

      CALL locase(detstr)

      idet = 0
      lenn = 0
      qdone = .FALSE.
      len1 = LENACT(prefix)
      len2 = LENACT(catdir)

c Loop around the instruments

      DO WHILE ( .NOT.qdone )
c
         CALL xgtarg(detstr, lenn, lenb, lene, qskip, iflag, idelim)
         IF ( iflag .NE. 0 ) RETURN
         idet = idet + 1

         obscat(idet) = catdir(:len2)//prefix(:len1)//'_'//
     &                  detstr(lenb:lene)//'.cat'

      ENDDO

      end

c
c
c ---------------------------------------------
      subroutine XSL_DUMPCAT(LIST,rows,prhead)
c ---------------------------------------------
c This routine uses fdump to display the currently loaded catalogue
c onto the screen.
c
      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(512) str1, str2, str3, dispstr
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2
c ---------------------------------------------
      logical LIST
      character*(*) rows,prhead
      character(255) displi
      character(512) contxt
      integer block,htype,rwmode,LENACT,datlen,hklen,tmpstat
      integer lenn, lenb, lene, iflag, idelim
      logical qdone, qskip
      
c Get and display the Datadir and Hkdir keywords:
      rwmode = 0
      status = 0


      call GETLUN(ilun)
      call FTOPEN(ilun, catnam, rwmode, block, status)
      if (status .eq.103) then
         contxt='Unable to find FITS file '//catnam
         call XWRITE(contxt,5)
         call FRELUN(ilun)
         return 
      else if (status .gt. 0) then
         contxt='Unable to open FITS file '//catnam
         call XWRITE(contxt,5)
         write(contxt,51) status
 51      format('Fitsio error no.: ',i3)
         call XWRITE(contxt,5)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         return 
      endif
      
c go to first extension
      call FTMRHD(ilun, 1, htype, status)
      if(status .gt. 0) then
         contxt='Unable to move to ext#1 '
         call XWRITE(contxt,5)
         tmpstat=0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         return 
      endif
      
c Reset the data and HK directories from the obscat:
      call FTGKYS(ilun,'DATADIR',str2,str1,status)
      if(status.ne.0) then
c For a user loaded catalogue, the DATDIR keyword may not be present
c In this case, the catalogue must be in the datdir or wrkdir == datdir!
         status = 0
         IF ( datdir.eq.'NONE' ) then
            datdir = wrkdir
         ENDIF
      else
         datdir = str2(:min(lenact(str2),len(datdir)))    
      endif
      
      call FTGKYS(ilun,'HKDIR',str2,str1,status)
      if(status.ne.0) then
         status = 0
         IF ( datdir.eq.'NONE' ) then
            hkdir = datdir
         ELSE
            hkdir = wrkdir
         ENDIF
      else
         hkdir = str2(:min(lenact(str2),len(hkdir)))    
      endif
      call FTCLOS(ilun,status)
      call FRELUN(ilun)
      
      
c Next get the display list:
      
      call xsl_uclgst('displist',displi,status)
      IF ( status .NE. 0 ) RETURN
 125  if(displi(1:3).eq.'def') then
         call GETLUN(ilun)
         len1 = LENACT(prefix)
         str2 = prefix(:len1)//'_display.def'
         call XSL_RMFILE(str2)
c This is necessary to stop openwr from expanding the filename.
         str3 = str2
         call XSL_OPEN(ilun,str2,'NEW',' ',' ',0,0,ierr)

c  Get the mission display list

         CALL XSL_MDBS(keymis, submis, instru, datamode, 'dispcol', 
     &                 dispstr, status)
         IF ( status .NE. 0 ) THEN
            CALL XWRITE('Unable to find dispcol in MDB', 5)
            RETURN
         ENDIF

c Now parse dispstr and write out the keywords

         qdone = .FALSE.
         lenn = 0
         CALL xgtarg(dispstr, lenn, lenb, lene, qskip, iflag, idelim)
         IF ( iflag .NE. 0 ) qdone = .TRUE.

         DO WHILE ( .NOT.qdone )

            WRITE(ilun, 53) dispstr(lenb:lene)

            CALL xgtarg(dispstr, lenn, lenb, lene, qskip, iflag, idelim)
            IF ( iflag .NE. 0 ) qdone = .TRUE.

         ENDDO

         close(ilun)
         call FRELUN(ilun)
         displi = '@'//str3(:min(lenact(str3),len(displi)-1))
      else if(displi(1:1).eq.'@') then
         len2 = LENACT(displi)
         str1 = displi(2:len2)
         call XSL_EXIST(str1,status)
         if(status.ne.0) then
            call XWRITE
     +           ('Display list not found; assuming default',5)
            displi = 'def'
            goto 125 
         endif
      else 
         len2 = LENACT(displi)
         if(displi(1:1).ne.'"') then
            displi = '"'//displi(:len2)
            len2 = len2 + 1
         endif
         if(displi(len2:len2).ne.'"') then
            displi = displi(:len2)//'"'
         endif
      endif

      str2 ='******************** Observation Catalogue '
     &     //'********************'
      call XWRITE(' ',5)
      call XWRITE(str2,5)
      call XWRITE(' ',5)
      datlen = LENACT(datdir)
      hklen = LENACT(hkdir)
      str3 = 'Data Directory is: '//datdir(1:datlen)
      call XWRITE(str3,5)
      str3 = 'HK Directory is: '//hkdir(1:hklen)
      call XWRITE(str3,5)
      IF(LIST) THEN
         call XSL_FLIST(catnam,rows,prhead)
      ELSE
         call XSL_FDUMP(catnam,displi,rows,pgwth,prhead)
      ENDIF
      call XWRITE(' ',5)
 53   format(a)
      return
      end
c
c ---------------------------------------------
      subroutine XSL_MISSION_SET(mode)
c ---------------------------------------------

      include 'xsel.inc'
      include 'xselvar.inc'

c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(512) str1,str2
c General reusable integers for lengths of strings 
      integer len1, len2
c ---------------------------------------------
      character(255) keepms
      character(512) contxt
      integer mode
      integer LENACT
c ---------------------------------------------------------------
c  SET MISSION
c Philosophy:
c The default mission etc is set in the XSEL.PAR file, with
c keyword_mission, keyword_time, units_time etc. The MISSION
c option allows you to change mission to another that may
c be hardwired in. 
c
c The over mission concept really didn't work...
c
c     Alan Smale 1992 Dec

      status = 0
      keepms = keymis

c Mode = 2 means set to default mission, used at startup.
c Mode = 3 means don't reset xcolf and xcolh..., used if there is a 
c Mode = 4 means set it to the vaule of keymis, used in XSL_LOAD
c          saved session.

      IF(mode.eq.2) THEN
         call XSL_UCLGSTD('default_mission',str1,status)
         contxt = ' Failed to get default_mission'
         IF ( status .NE. 0 ) GOTO 99999
         call xsl_uclgst('default_mission',keymis,status)
         contxt = ' Failed to get default_mission'
         IF ( status .NE. 0 ) GOTO 99999
c Save the new value of 'default_mission' if necessary.
         IF(str1(:LENACT(str1)).ne.keymis(:LENACT(keymis)))THEN
            call xpisavepar(status)
            call XWRITE('Saving Par File',5)
         ENDIF
      ELSE IF(mode.eq.0) THEN
         call XSL_GETDUMMYPARST('mission','set_str',keymis,status)
         contxt = 'Failed to get mission'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(keymis)
c Resetting the mission should clear all:
         call XSL_CLEAR(1)
      ELSE IF(mode.eq.4) THEN
         str1 = datdir
         str2 = mkfdir
         call XSL_CLEAR(1)
         datdir = str1(:min(lenact(str1),len(datdir)))    
         mkfdir = str2(:min(lenact(str2),len(mkfdir)))    
      ENDIF

C By default the catalogue gets its info. from the primary header.
      catnum = '0'

c Now get the rate column name, and error column name.  This is for XRONOS,
c so it should not be mission dependent

      call xsl_uclgsr('time_reference',timref,status)
      contxt = 'Failed to get time_reference'
      IF ( status .NE. 0 ) GOTO 99999
      call xsl_uclgst('Xronos_rate_extnam',ratext,status)
      contxt = 'Failed to get Xronos_rate_extnam'
      IF ( status .NE. 0 ) GOTO 99999
      call xsl_uclgst('Xronos_rate_col',keyrat,status)
      contxt = 'Failed to get Xronos_rate_col'
      IF ( status .NE. 0 ) GOTO 99999
      call xsl_uclgst('Xronos_rate_error',keyrte,status)
      contxt = 'Failed to get Xronos_rate_error'
      IF ( status .NE. 0 ) GOTO 99999
      status = 0

c Check whether the mission specified is one supported otherwise try the
c "over" mission (which I don't think works).

      CALL XSL_MDBS(keymis, ' ', ' ', ' ', 'instruments', str1, status)

      IF ( status .NE. 0 ) THEN

         status = 0
         call xsl_uclgst('over_mission',keymis2,status)
         contxt = 'Failed to get over_mission'
         IF ( status .NE. 0 ) GOTO 99999
            
         len1 = LENACT( keymis  )
         len2 = LENACT( keymis2 )
            
         IF( keymis(1:len1) .eq. keymis2(1:len2) )THEN
            keymis = 'over'
            str1 = ' This is '//keymis2(:len2)
            call XWRITE(str1, 20)
         ELSE
            str1 =' Unidentified mission? : '//keymis(1:len1)
            call XWRITE(str1,5)
            keymis = keepms(:min(lenact(keepms),len(keymis)))
            return
         ENDIF

      ELSE

         str1 = ' This is '//keymis(:lenact(keymis))
         call XWRITE(str1, 20)

      ENDIF

c Now get the mission dependent stuff - first do all the standard keywords

      CALL XSL_MDB_SET()
      contxt = 'Failure in XSL_MDB_SET'
      IF ( status .NE. 0 ) GOTO 99999
            
c Now reset the prompt

      prompt = prefix(:LENACT(prefix))
      IF(keymis.ne.'NONE') THEN
         prompt = prompt(:LENACT(prompt))//':' 
     &        //keymis(:LENACT(keymis))
      ENDIF
      IF(instru.ne.'NONE') then
         prompt = prompt(:LENACT(prompt))//'-'//
     &        instru(:LENACT(instru))
      ENDIF          
      IF(datamode.ne.'NONE') then
         prompt = prompt(:LENACT(prompt))//'-'//
     &        datamode(:LENACT(datamode))
      ENDIF          
      prompt = prompt(:LENACT(prompt))//' >'

c Set the time conversion:

      if ( index(keyuni,'d') .ne.0) then
         day2uni = 1D0
         sec2uni = 1D0/day2sec
      elseif ( index(keyuni,'h') .ne. 0) then
         day2uni = 24D0
         sec2uni = 1D0/(60D0**2)
      elseif (index(keyuni,'s') .ne. 0) then
         day2uni = day2sec
         sec2uni = 1D0
      else
         call XWRITE('Unrecognized time unit',5)
         status = -10
         return
      endif

C Set the obscat name:

      call XSL_SET_OBSCATNAME()
         
99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i8)') ' XSL_MISSION_SET: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end         

c ---------------------------------------------
      subroutine XSL_MDB_SET()
c ---------------------------------------------

      IMPLICIT NONE

c Wrap-up routine to set all the parameters from the MDB for the current
c mission, submission, instrument, and datamode

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

      INTEGER ikey

      CHARACTER(255) keystr
      CHARACTER(512) str1, contxt
      ChARACTER(16) str2

      INTEGER lenact
      EXTERNAL lenact

c Set a string specifying all the keywords

      keystr = keymis(:lenact(keymis))
      IF ( lenact(submis) .GT. 0 .AND. submis .NE. 'NONE' ) THEN
         keystr = keystr(:lenact(keystr))//':'//submis(:lenact(submis))
      ENDIF
      IF ( lenact(instru) .GT. 0 .AND. instru .NE. 'NONE' ) THEN
         keystr = keystr(:lenact(keystr))//':'//instru(:lenact(instru))
      ENDIF
      IF ( lenact(datamode) .GT. 0 .AND. datamode .NE. 'NONE' ) THEN
         keystr = keystr(:lenact(keystr))//':'//
     &            datamode(:lenact(datamode))
      ENDIF


      status = 0

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'mkf_def_expr', 
     &              mkfdnm, status)
      contxt = 'Failed to get '//
     &     keystr(:lenact(keystr))//':mkf_def_expr'
      IF ( status .NE. 0 ) GOTO 99999
      IF ( ECHO ) THEN
         contxt = keystr(:lenact(keystr))//':mkf_def_expr = '//
     &        mkfdnm(:lenact(mkfdnm))
         CALL xwrite(contxt, 5)
      ENDIF

      IF (mkfdnm.ne.'NONE') then
c Get the default relative directory for the filter file
         CALL XSL_MDBS(keymis, submis, instru, datamode, 'mkf_rel_dir', 
     &                 mkreld, status)
         contxt = 'Failed to get '//
     &        keystr(:lenact(keystr))//':mkf_rel_dir'
         IF ( status .NE. 0 ) GOTO 99999
         IF ( ECHO ) THEN
            contxt = keystr(:lenact(keystr))//':mkf_rel_dir = '//
     &           mkreld(:lenact(mkreld))
            CALL xwrite(contxt, 5)
         ENDIF
      ENDIF


      status=0
      CALL XSL_MDBS(keymis, submis, instru, datamode, 'time', 
     &              keytim, status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':time'
      IF ( status .NE. 0 ) GOTO 99999
      IF ( ECHO ) THEN
         contxt = keystr(:lenact(keystr))//':time = '//
     &        keytim(:lenact(keytim))
         CALL xwrite(contxt, 5)
      ENDIF

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'tunits', 
     &              keyuni, status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':tunits'
      IF ( status .NE. 0 ) GOTO 99999
      IF ( ECHO ) THEN
         contxt = keystr(:lenact(keystr))//':tunits = '//
     &        keyuni(:lenact(keyuni))
         CALL xwrite(contxt, 5)
      ENDIF

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'timeorder', 
     &              TORDER, status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':timeorder'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBE(keymis, submis, instru, datamode, 'timepixr', 
     &              timepixr, status)
      IF ( status .NE. 0 ) THEN
         timepixr = -999.9
         status = 0
      ENDIF

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'events', 
     &              evtnam, status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':events'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'wtmapb', 
     &              WTMAPB, status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':wtmapb'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'wtmapfix', 
     &              WTMAPFIX, status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':wtmapfix'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'swmapx', 
     &              SWMAPX, status)
      IF ( status .NE. 0 ) THEN
         SWMAPX = .FALSE.
         status = 0
      ENDIF

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'swmapy', 
     &              SWMAPY, status)
      IF ( status .NE. 0 ) THEN
         SWMAPY = .FALSE.
         status = 0
      ENDIF

      CALL XSL_MDBE(keymis, submis, instru, datamode, 'binsize', 
     &              binsiz, status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':binsize'
      IF ( status .NE. 0 ) GOTO 99999

c Set the event list columns used for SKY, DETECTOR, and RAW coordinates

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'x', 
     &              keyx(1), status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':x'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'y', 
     &              keyy(1), status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':y'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'xsiz', 
     &              keyxsz(1), status)
      IF ( status .NE. 0 ) keyxsz(1) = 'TLMAX'

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'ysiz', 
     &              keyysz(1), status)
      IF ( status .NE. 0 ) keyysz(1) = 'TLMAX'

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'detx', 
     &              keyx(2), status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':detx'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'dety', 
     &              keyy(2), status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':dety'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'detxsiz', 
     &              keyxsz(2), status)
      IF ( status .NE. 0 ) keyxsz(2) = 'TLMAX'

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'detysiz', 
     &              keyysz(2), status)
      IF ( status .NE. 0 ) keyysz(2) = 'TLMAX'

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'rawx', 
     &              keyx(3), status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':rawx'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'rawy', 
     &              keyy(3), status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':rawy'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'rawxsiz', 
     &              keyxsz(3), status)
      IF ( status .NE. 0 ) keyxsz(3) = 'TLMAX'

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'rawysiz', 
     &              keyysz(3), status)
      IF ( status .NE. 0 ) keyysz(3) = 'TLMAX'

c and the keyword to specify the number of PHA bins

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'phamax', 
     &              phamxkwd, status)
      IF ( status .NE. 0 ) phamxkwd = 'TLMAX'

c Set the instrument-specific variables - the image binning, wmap binning,
c spectral binning, column for spectra, and image and wmap keywords

      CALL XSL_MDBJ(keymis, ' ', instru, datamode, 'fbin', xbinf, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':fbin'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBJ(keymis, ' ', instru, datamode, 'hbin', extrbinh, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':hbin'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBJ(keymis, ' ', instru, datamode, 'spbn', phabin, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':spbn'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, ' ', instru, datamode, 'ecol', keypha, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':ecol'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, ' ', instru, datamode, 'ccol', keyccd, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':ccol'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, ' ', instru, datamode, 'gcol', keygrd, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':gcol'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, ' ', instru, datamode, 'catnum', catnum, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':catnum'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_MDBS(keymis, ' ', instru, datamode, 'imagecoord', str2, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':imagecoord'
      IF ( status .NE. 0 ) GOTO 99999

      IF ( str2 .EQ. 'SKY' ) THEN
         ikey = 1
      ELSEIF ( str2 .EQ. 'DETECTOR' ) THEN
         ikey = 2
      ELSEIF ( str2 .EQ. 'RAW' ) THEN
         ikey = 3
      ELSE
         ikey = 0
      ENDIF
      IF ( ikey .EQ. 0 ) THEN
         xcolf = 'NONE'
         ycolf = 'NONE'
         xfkey = 'TLMAX'
         yfkey = 'TLMAX'
      ELSE
         xcolf = keyx(ikey)
         ycolf = keyy(ikey)
         xfkey = keyxsz(ikey)
         yfkey = keyysz(ikey)
      ENDIF

      CALL XSL_MDBS(keymis, ' ', instru, datamode, 'wmapcoord', str2, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':wmapcoord'
      IF ( status .NE. 0 ) GOTO 99999

      IF ( str2 .EQ. 'SKY' ) THEN
         ikey = 1
      ELSEIF ( str2 .EQ. 'DETECTOR' ) THEN
         ikey = 2
      ELSEIF ( str2 .EQ. 'RAW' ) THEN
         ikey = 3
      ELSE
         ikey = 0
      ENDIF

      IF ( ikey .EQ. 0 ) THEN
         xcolh = 'NONE'
         ycolh = 'NONE'
         xhkey = 'TLMAX'
         yhkey = 'TLMAX'
      ELSE
         xcolh = keyx(ikey)
         ycolh = keyy(ikey)
         xhkey = keyxsz(ikey)
         yhkey = keyysz(ikey)
      ENDIF

C Set the name of the GTI extension

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'gti', 
     &              gtinam, status)
      contxt = 'Failed to get gti'
      IF ( status .NE. 0 ) GOTO 99999

C Set the adjustgti logical for extractor

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'adjustgti', 
     &              ADJUSTGTI, status)
      IF ( status .NE. 0 ) THEN
         ADJUSTGTI = .FALSE.
         status = 0
      ENDIF

C Set the lststr in the parameter file - we do this so it is set up as a
C default that the user can override in the "make obscat" command.

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'lststr', str1, 
     &              status)
      contxt = 'Failed to get '//keystr(:lenact(keystr))//':lststr'
      IF ( status .NE. 0 ) GOTO 99999

      CALL XSL_UCLPST('lststr', str1, status)
      contxt = 'Failed to put lststr parameter'
      IF ( status .NE. 0 ) GOTO 99999


99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i8)') ' XSL_MDB_SET: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      RETURN
      END
