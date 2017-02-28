**==a2soue.spg  processed by SPAG 4.50J  at 11:08 on 28 Oct 1998
C*****************************************************************************
C SELECTOR TASK:
C   a2source
C
C FILE:
C   a2source.f
C
C DESCRIPTION:
C Takes user provided celestial source position and determines which raw
C data files the user will need to ultimately generate a light curve.
C Output consists of 2 listfiles each containing a list of the raw datafiles
C (one listfile for each detector - MED and HED). One of the listfiles must
C then be input to the FTOOL A2LCURVE to generate the light curve.
C
C AUTHOR/DATE:
C   pre-FTOOLS  Unknown (Frank Marshall?)
C   FTOOLS      Jesse Allen
C
C MODIFICATION HISTORY:
C   20 Nov 1997  SOURCE code modified to accept only B1950 R.A. and Dec.
C                and to generate file names instead of scan angles and
C                day visibilities.
C   28 Jan 1998  Uses SLA routines for coordinate conversions
C   15 Apr 1998  Accepts multiple equinoxes for R.A. and Dec (Assumes FK4)
CH   Jesse Allen (1.0.0 15 Apr 1998) Original working version
C
C
CH   25 May 1998              Output is now 2 listfiles (one for the MED
CH   Lorraine Breedon         detector and one for the HED detectors). Each
C                             listfile containes a list of the raw data files
C                             corresponding to when the source was most likely
C                             to have been scanned.

CH   28 Oct 1998              
CH   Lorraine Breedon         Tidy up code \
C                             Move code to subroutines create_filelist.f,
C                             process_coords.f, write_filelist.f \
 
C MAKE:
C   HOST: make a2source
C   IRAF:
C
C USAGE:
C   HOST: a2source
C   IRAF:
C
C ARGUMENTS:
C   none
C
C VARIABLES :
C
C Common block declarations
C
C   taskname     A shared string used by FTOOLS.  Contains the name and
C                version of this software.
C
C Local variables
C
C   status      Flag for checking error status of various operations.  A
C               non-zero value generally indicates something has not worked
C               as expected (e.g. string input given to an integer parameter)
C   parse, imessage, ichat, lchat, tchat
C               Variables used for XWRITE and XAERROR to control the
C               types of screen and log file output from this program
C   bday, prevday, startfile, stopfile
C               Specific the range of days and matching file names for
C               a particular source
C   radeg, decdeg, equinox, equi, rastr, decstr
C               Variables for input of the R.A. and Dec. of a source and
C               the equinox of those coordinates.  Software is hardcoded
C               to assume these values are in FK4 (Besselian) coordinates
C   rardb50, decrdb50, rardb78, decrdb78, rardj20, decrdj20, lIIrd, bIIrd,
C   radgb50, decdgb50, lIIdg, bIIdg, elgrd78, eltrd78, elgdg78, eltdg78
C               Co-ordinates for the source in a variety of units
C               ("rd" for radians, "dg" for degrees)
C   idmsflg, idmsflt, ihmsf
C               Arrays used by SLA routines to get sexidecimal output
C   rlong, rlat Temporary variables used to handle in routines which need
C               real variable I/O rather than double precision numbers
C   sign        SLA variable to indicate +/- values
C   rtd         Parameter to convert between degrees and radians
C   message     String for passing messages to the screen and/or log files
C   logfile     Name of the log file to write and log output from the program
C   program     Name of this program, used as a root name for the log file
C   td          Array for holding observation days and times of a source.
C   i, j        Looping variables used to perform repeated functions
C   jtd, jtmax  Temporary variables used to holding times of observations
C                in looping cycles
C   sunlong     Ecliptic longitude of the Sun at mid-orbit
C   scana       Scan angle at which the source is closest to the scan of the
C                instrument
C   docsav, doca, doc
C               Variables for storing the angular seperating between the source
C                and scan angle of closest approach (degrees off center)
C   mjdref      Modified Julian Date for time reference
C
C CALLED ROUTINES:
C  External library routines
C
C      SLA routines        Starlink's Subroutine Library A (SLA) which
C                           perform a variety of coordinate system conversions
C      parsera, parsedec   XANLIB routines to convert sexidecimal inputs into
C                           fractional degrees
C      XWRITE, XAERROR     XANLIB routines for sending program output to
C      SETLOG               the terminal and/or log files with user set chatter
C                           levels
C
C  Local routines
C      getpars             Gets source parameters
C      daytofile           Converts the range of days into file numbers
C      create_filelist     Create output listing files
C      process_coords      Process coord information and determine
C                          which days source will be in MED, HED FOV 
C                          and thence the raw data file numbers (names)
C                          corresponding to these days.
C      write_filelist      Write the raw data filenames to output listing. 
C      EPHEM2              Calculates the longitude of the Sun at mid-orbit
C      DEGOFF              Calculates the scan angle at which the A2
C                          instrument is closest to the source, and the
C                          angular seperating between the source and the center
C                          of the instrument response function at the scan
C                          angle of closest approach.
C
C     EPHEM2 and DEGOFF are heritage code from the original VAX program
C     SOURCE
C
C*****************************************************************************
 
      SUBROUTINE A2SOUE
 
      IMPLICIT NONE
 
C Common block declarations
 
      COMMON /TASK  / TASkname
 
      character(40) TASkname
 
C Local variables
 
      INTEGER parse , ichat , lchat , tchat , status
      INTEGER start1 , stop1 , start2 , stop2,lenact
      LOGICAL clobber 
      INTEGER ounit1 , ounit2 ,  errstat,i
      character(40) hed_outlist , med_outlist
 
      DOUBLE PRECISION rtd , equinox 
 
      character(80) message , program , logfile , imessage , rastr , 
     &             decstr
      character(80) source
      character(160) dayfile
      DATA imessage , parse/' ' , 0/
      character(7) VERSION
      PARAMETER (VERSION='1.1.0')
 
 
      rtd = 57.29577951D0
 
 
C Begin program
 
      program = 'a2source '
      TASkname = 'A2SOURCE '//VERSION
      status = 0
      errstat = 0
      CALL FTCMSG
 
C Get parameters and set terminal and log chat levels
 
      CALL GETPARS(rastr,decstr,equinox,source,dayfile,lchat,tchat,
     &     clobber,status)
      IF ( status.NE.0 ) THEN
         message = ' Failure in attempting to retrieve input parameters'
         CALL XAERROR(message,1)
         return
      ENDIF
 
c open the log file if necessary tchat>=lchat
c reset the internal chatness to lchat
C also open the file for the output file listing
 
      CALL XCHATY(tchat,lchat)
      ichat = lchat
      i = INDEX(TASkname,' ')
      message = TASkname(:LENACT(TASkname))//' start calculations'
      CALL XWRITE(message,ichat)
      logfile = '+'//TASkname(1:i-1)//'.log'
      IF ( lchat.GE.tchat ) CALL SETLOG(imessage,parse,logfile,' ')
 
 
C Create the output file listings

      CALL CREATE_FILELIST(source,clobber,ounit1,ounit2,
     &            hed_outlist,med_outlist,status)
      IF ( status.NE.0 ) THEN
         message = ' Failure to create output listings '
         CALL XAERROR(message,1)
         return
      ENDIF

C Process coord information and determine which days of mission
C the source will be in the MED and HED FOV (`good'
C observing days).
C Then, via the internal subroutine call to daytofile.f
C determine the raw data file numbers corresponding to these
C `good' days.
C Write some coord information to output files

      CALL PROCESS_COORDS(equinox,rastr,decstr,rtd,ichat,
     &     ounit1,ounit2,start1,stop1,start2,stop2,dayfile,status)
      IF ( status.NE.0 ) THEN
         message = ' Failure in coord processing '
         CALL XAERROR(message,1)
         return
      ENDIF
 
C Now write MED and HED output file listings of the raw datafiles

      CALL WRITE_FILELIST(ounit1,ounit2,hed_outlist,med_outlist,
     &                 start1,stop1,start2,stop2,status)
      IF ( status.NE.0 ) THEN
         message = ' Failure writing to output listings '
         CALL XAERROR(message,1)
         return
      ENDIF
 
      RETURN
 
 
      END
