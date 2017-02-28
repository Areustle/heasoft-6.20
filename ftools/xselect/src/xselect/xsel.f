       PROGRAM XSELECT
c --------------------------------------------------------------------
c
c This is a XANADU-type interface to the ASCA SELECTOR tasks. 
c Designed to be mission-independent, there are currently explicit
c hooks for ASCA and ROSAT. A number of other missions are supported
c through the mission database file.

c It uses the XPI and XANLIB libraries.

c
c The interface works as follows. 
c   The program sets up the standard XPI interface
c   The user enters a command on the commandline
c   The program pulls in parameters from this command line, 
c       prompting for values not given.
c   The program gets a LUN number and opens a new CMDFIL. It
c       generates the relevant command or sequence of commands
c       in SELECTOR-speak and puts them in the CMDFIL. Finally
c       it spawns the CMDFIL and runs the appropriate host-
c       interface Selector commands.
c
c Note that all system dependency is (or should be) layered out
c into the small routines in XSL_aaa.f, where aaa = 'sun', etc.,
c or be handled by XANLIB.
c
c
c My convention: all subroutine names, all function names, major 
c IF THEN ELSEs, and all logicals are in upper case. The standard
c XANADU GTCOM section of code has also been left in upper case.
c All else is lower case.
c
c Filename conventions: All filenames will be stored with their 
c FULL PATH.  XSL_FILENAME extracts just the name.    
c
c     Authors: Alan Smale, 1992 June-Nov (Version 0.5 and before)
c              Alan Smale and Jim Ingham, 1993 (since Version 0.5)
c              Jim Ingham, May, 1993 (since version .80)
c              Keith Arnaud, Nov, 1997 (version 1.4)
c
c              APS, Code 668, NASA/GSFC   alan@ros6.gsfc.nasa.gov
c              JCI, Code 664, NASA/GSFC   ingham@lheavx.gsfc.nasa.gov
c
c --------------------------------------------------------------------
c   Version History - since .99e
c   .99e  -- Check TIMEDEL keyword during SET BINSIZE.
c         -- Fixed set mission and set inst, and keywords for BBXRT
c         -- XSL_GETKW* takes extnum as well as file name.
c         -- Run sisclean clean=1 from cleansis, not sisclean ftool
c         -- Remove references to sisreg and cleanim  
c         -- Turn GTBUF_EDIT off so it will not mess up readline
c   .99f  -- Put in history buffers for strsel and ffstr.
c   .99g  -- Fixed XSL_GTIFILTER to not merge output, and use all FITS GTI's
c         -- Changed XSL_MORE to do ECHO through XWRITE.
c         -- Don't turn GTBUF_EDIT off, new Readline fixes this
c         -- Datamode now in prompt, puts a selection expression in the
c            def obscat expression, changes SIS rbnpha.  Faint sets datamode.
c            Put datamode in obscat, load datamode from obscat if present.
c         -- Added XSL_PUTKWST
c         -- Offer to fix datadir keyword of catalogues in the datadir.
c         -- Put sis_plot2 and saoimage2 in sisclean
c         -- Set datamode in CHOOSE, and in READ
c         -- Added xpicloselog & xpiopenlog to XSL_RUNCF
c         -- Prompt for outtype in XSL_SMOOTH
c   .99h  -- Set keypha and evtnam seperately for MPC MODE
c         -- Start from extension 1 to find mission and inst in READ EVENTS
c         -- Added XSL_TLIMITS so I can pass start and stop to saextrct
c         -- Added XSL_APPEND to append gti extensions to the saextrct output
c         -- Broke up xsel_utils.f, and xsel.f
c         -- Restore sets instru and datamode
c         -- Bug in XSL_QDP2FITS wasn't writing Label Top line right
c         -- Check has an image been made in smooth
c         -- use newval(1)==ASCA to decide to check for datamode in read
c         -- Added RAWXBINS and POS_DET to the ASCA GIS OBSCAT
c         -- Removed EXTRACT LCEVENTS and SAVE LCEVENTS
c         -- Set the X and Y in set datamode: fast screws them
c         -- clear datamode when resetting instrument
c         -- set chatty to 7 for Xselect error number
c   1.0a  -- Added xsel_mission.inc, and XSL_INST_SET
c         -- Prompt for inst. if not set in SET DATAMODE
c         -- Fixed XSL_CMDLIST
c         -- Moved xsl_chkdir and xsl_getcwd to C routines
c         -- Choose now checks TIMEDEL and RAWXBINS
c         -- SET IMAGE clears region filters if coordinates have changed
c         -- Added SET WMAPBINSIZE
c         -- Added XSL_CHKPHASIZE, read & choose use it to get phamax, min.
c         -- BIN uses phamin,max to set phalow,phahi, & pha[hl]cut if none
c            has ben entered.
c         -- Added assign/user sys$command sys$input to VMS XSL_WRTCF
c         -- Build up rti filename list dynamically in XSL_GISCLEAN
c         -- Check RISEBINS, PHA_BINS.  Allow different values, but prohibit
c            senseless operations
c         -- Read makes an OBSCAT, there are definitions for ROSAT RDF.
c         -- Clear datamode resets prompt
c         -- Fix bug with load obscat if datadir no longer exists
c         -- Fast spectrum is binned like BRIGHT on  saving
c   1.0b  -- New SAEXTRCT uses the GTI and PHASE files.
c         -- XSL_FAINTDFE, use this in XSL_FAINT, SAVE DFE added.
c         -- XSL_NUMSORT for numerical sorts on ascii files.
c         -- Changed XSL_CATREG -> XSL_CAT, removed sisreg, use a system.
c         -- XSL_EXTRACT now removes the target file...
c         -- Made CHOOSE no files abort a script
c         -- Made SELECT MKF finds no MKF files abort a script
c         -- Lengthened CTMP in xsl_open to accomodate long directories
c         -- Added SAVE ALL
c         -- Added XSL_CHKPHASE to check the phase filter against TIMEDEL
c         -- Added COORDPRO!='ERROR' to ASCA default selection expression.
c         -- Added XSL_GET_DIR to do the querying for directories.
c         -- Added SET MKFDIR, and FINDMKF now uses this, or queries if NONE.
c         -- Added more parameters to the ASCA standard OBSCAT.
c   1.0c  -- Added mfracexp (i.e. exposure) to extract for MPC mode
c         -- Converted the ASCII cursor selected GTI files to FITS.
c         -- Fixed bug in CLEAR SELECTION, old selections were still listed.
c         -- Added FILTER INTENSITY, CLEAR INTENSITY, and put in SHOW STATUS.
c         -- Handle the case when INSTRUME or DATAMODE are blank in input file
c   1.0d  -- Run FASTWMAP from xsl_bin for FAST mode spectra.
c         -- Added XSL_FAST_SETUP to CHOOSE and READ.
c         -- Changed the mission startup so that don't do SET INST, DATAMODE.
c         -- Fixed up SISCLEAN so if there is a cleaned evt file, it gets
c            used first.  Also merged the .pxl files from multiple runs.
c         -- Made the EXTRACT EVENTS command pass the HOT_PIXEL extension 
c            from evnin to evnout if BINOUT and CLEAN.
c         -- Added the SELECT FAST command.
c         -- Changed XSL_COL2INT to optionally return the value.
c         -- Fix TIMEDEL for FAST mode.
c   1.0e  -- Fix the HOT_PIXEL extension again, now save it from EXTRACT 
c            EVENTS, but don't need to extract it.
c         -- Initialize in_or_out for FAST mode.
c         -- Fixed XSL_SAVE_EVENTS, the part with use_events was never reached.
c         -- Save some flags in XSL_SAVE_EVENTS use_events=yes
c         -- added HAVEMKF to set mission and set datadir
c         -- Look in datadir for MKF files, if not found look in mkf_reldir.
c   1.0f  -- Pass the clobber to SAVE ALL by using "!", fix grpsav to use this
c   1.0g  -- Accept different RDF versions if the major number agrees
c         -- Fixed a bug in save spec; it would not save unless clobberit==yes!
c         -- SAVE ALL now only saves the SIS clean file, not the binout file
c            and the GIS cleaned files only if there is no output events list.
c         -- Fixed the XSL_EXPAND_DIR on the vax, to handle logical disks
c         -- Added make_obscat=no to READ
c         -- Fixed VMS version of XSL_WRTCF, XSL_CLCF and XSL_RUNCF to get
c         -- Error reporting to work correctly.
c         -- Added tdisp option to obscat-display.
c         -- In CHOOSE, check for RISE_BINS, ... not in data files: TFORM=9A
c   1.0h  -- In XSL_CHKPHASIZE throw out files w/TLMIN > TLMAX.
c         -- Close lstfil in XSL_CHKVERS if FHELP dies...
c         -- Fixed bug in SET IMAGE
c         -- Added DIRTY flag to sisclean, and dirtysis
c         -- In XSL_FAINT, change datamode only after successful conversion
c         -- Fixed format error for xmax in XSL_FITS_2_QDP
c   1.0i  -- Check RISEBINS, RAWXBINS and PHA_BINS in READ, 
c            reject RISEBINS=(0,1), not just 0.
c         -- Use the last time in the file, if TSTOP == 0, in XSL_FITS_2_QDP
c   1.1  -- Totally revised the TIME filter entry.  Now use FILTER TIME
c         -- {CURSOR,FILE,SCC,MJD,UT}, ditto for the CLEARs.
c         -- Cleaned up the show status a little
c         -- Fixed a bug in mkfbin, cd to mkfdir, not datadir...
c         -- Added truncate spectrum for ROSAT PSPC
c         -- Added mission specific MKF default names
c         -- Added SET XYSIZE and XYCENTER, and CLEAR XYSIZE and XYCENTER
c   1.1a  -- Fixed ILUN leak in XSL_PARSE
c         -- Change TLMAX on outputting ROSAT PSPC spectra.
C         -- Group spectrum to 1 channel for ROSAT HRI
c         -- CLEAR HAND -> CLEAR KEYBOARD..., FILTER UT, MJD now use plot
c         -- 'c' on first click now cancels filter time cursor selection
c         -- Fixed a bug in xsl_cat
c   1.2   -- Added EINSTEIN HRI and PSPC, SAX, and JET-X (beta version...)
c         -- Broadened make obscat to work for ROSAT, and EINSTEIN...
c         -- Added a warning to remove the light curve in CLEAR INTENSITY
c         -- Removed offer to save evnout when CLEAN=.TRUE. in select MKF -
c            it was just confusing...
c         -- Check for nint>MXNINT in XSL_LCPLOT...
c   1.3   -- Default keypha for ASCA SIS -> PI.
C         -- If there is an output events file from extract, make the 
C            next extract call use it...
C         -- To get this to work, had to warn to do CLEAR EVENTS if BINOUT
C            in all the CLEAR commands that might effect a EXTRACT EVENTS
C         -- Fixed Bug in XSL_READ, catidx was getting set wrong.
C         -- ROSAT: Truncate PSPCB, PSPCC -> PSPC for instru variable
C         -- Added SISPI command for ASCA, run it from FAINT
c   1.3a  -- Changed XSL_OBSNO to query for lststr for ASCA - so it would
c            work better with tkascascreen...
c   1.3b  -- Fixed bug in XSL_GTI_FILTER, checking for new files when all new
c            files are just files to be removed
c   1.3c  -- Updated calling sequences for ASCA FTOOLS, treat FAST mode
c            keywords correctly when 'select fast' is done
c   1.3d  -- Incorporated new information from ISAS on area discrimination
c            for ASCA SIS FAST mode data
c   1.3e  -- Improved error trapping, fixed a bug when extracting MPC mode
c            spectra and added the batchmode keyword which when set causes
c            xselect to exit on an ftool failure (kaa)
c   1.3f  -- Added in SAX changes passed on by Lorella. Now supports SAX
c            MECS as well as LECS. Do an fmemsort on event files output
c            from extractor when multiple event files are read in. No 
c            longer worries about ASCA processing version since we can
c            safely assume that everyone is running frfread 3. (kaa)
c   1.3g  -- Changed xsel_extract to accomodate new parameters for 
c            extractor v2.0. (kaa)
c   1.4   -- Many mission-specifics isolated in the Mission DataBase (kaa)
c            Only missions that appear by name in the code are ASCA and
c            ROSAT. ASCA is there because of its datamodes which require
c            special handling and ROSAT because of the issue of old-style
c            and RDF files. (kaa)
c   1.4b  -- Added support for XTE PCA Standard2 data. New filter on detector
c            with mission-specific code to translate PCA names. Now use find
c            rather than ls to get lists of files since XTE data is organised
c            in directory trees.
c -------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
      INCLUDE 'xselplt.inc'

c ---------------------------------------------
c DECLARATIONS
c ---------------------------------------------
      CHARACTER(255) prefix_str
      INTEGER prefixlen

c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1
      
c ---------------------------------------------
c Things required by GTCOM2
      character(255) string, commnd, tmpstr
      integer idone,parse
c This is now in the common block
      integer comnum
c ---------------------------------------------
c To avoid meaningless compilation errors on the Decstation
      logical MAINLP
c ---------------------------------------------
c True if the batchmode parameter is set
C      logical QBATCH

      INTEGER APE_TRAD_CLOSE

c Do the common set-up
      CALL xsel_common_init(prefix_str, prefixlen)

      prompt = prefix_str(:prefixlen)
      
C 
C     reset the plot window and use fdump to look at the fitsfile.
c
      fpltwin = " "
      fpltbkg = " "

      MAINLP = .TRUE.
      DO WHILE ( MAINLP )

         string = 'show'

c This is to stop scripts on failure. If batchmode is set then exit

         IF(status.ne.0) THEN
            write(str1,'(a,i4)') 'Error in Xselect, no. ',status
            call XWRITE(str1,7)
            call GTBUFSTOPSCRIPT()
            IF ( QBATCH ) call EXIT(1)
         ENDIF

c using tmpstr here because GTCOM2 sets xsldir to xsldsk/xsldir and
c I don't want xsldir to be changed.

         tmpstr = xsldir

         CALL GTCOM2(string,parse,prompt,commnd,comnum,
     &        xsldsk,tmpstr,xslnam,' ',idone)

c comno is a flag to indicate whether any parameters have been 
c entered on the command line.  0 means that only the command 
c was entered.

         call UCLGNO(comno)

c Now process the command line

         call XSEL_PROC_COMMAND(commnd, idone, .false.)

      ENDDO

      status = APE_TRAD_CLOSE(1)
      if (status .ne. 0) call XSL_EXIT(status)
 
      STOP
      END
c
c ---------------------------------------------
c Code ends.
c ---------------------------------------------



