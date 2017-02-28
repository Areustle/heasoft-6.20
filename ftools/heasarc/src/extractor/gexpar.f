
      SUBROUTINE gexpar(status)

      IMPLICIT NONE

      INTEGER status

      INCLUDE 'extractor.inc'

c Get the extractor parameters.

      LOGICAL exitnow

      CHARACTER(72) contxt

      CHARACTER(255) xupc
      EXTERNAL xupc

c Initialize error flag

      status = 0

c Check whether we want to give up

      CALL UCLGSB('exitnow',exitnow,status)
      IF ( status.EQ.0 .AND. exitnow ) THEN
         status = -1
         RETURN
      ENDIF

c Track whether we will need to know about WMAP coords or the energy column. 
c We need to know about the energy column if a spectrum is being produced and 
c about WMAP coords if the wtmapb flag is set.

      needwmco = .FALSE.
      needener = .FALSE.

c Get the input event filename

      CALL UCLGST('filename',infile,status)
      contxt = 'Failed to get filename parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the output event filename

      CALL UCLGST('eventsout',eventsfile,status)
      contxt = 'Failed to get eventsout parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(eventsfile) .EQ. 'NONE' ) eventsfile = ' '

c Get the output image filename

      CALL UCLGST('imgfile',imagefile,status)
      contxt = 'Failed to get imgfile parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(imagefile) .EQ. 'NONE' ) imagefile = ' '
      buildimage = (imagefile .NE. ' ')

c Find binning factor for the coordinates used for filtering and building 
c the image

      CALL UCLGSI('binf',binf,status)
      contxt = 'Failed to get binf parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( binf.EQ.0 ) binf = 1

c If writing out an image find out whether a full-size image should be
c written or just the bounding box of the selected events.

      IF ( buildimage ) THEN

         CALL UCLGSB('fullimage',fullimage,status)
         contxt = 'Failed to get fullimage parameter'
         IF ( status .NE. 0 ) GOTO 999

      ENDIF

c Get the output spectrum filename

      CALL UCLGST('phafile',phafile,status)
      contxt = 'Failed to get phafile parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(phafile) .EQ. 'NONE' ) phafile = ' '

      buildspec = ( phafile .NE. ' ' )
      IF ( buildspec ) needener = .TRUE.

c If an output spectrum is required then get the spectrum binning factor,
c whether a WMAP should be built, and if so its binning factor

      IF ( buildspec ) THEN

         CALL UCLGSI('specbin',specbin,status)
         contxt = 'Failed to get specbin parameter'
         IF ( status .NE. 0 ) GOTO 999
         IF ( specbin .EQ. 0 ) specbin = 1

         CALL UCLGSB('wtmapb',buildwmap,status)
         contxt = 'Failed to get wtmapb parameter'
         IF ( status .NE. 0 ) GOTO 999

         IF ( buildwmap ) THEN

            CALL UCLGSI('binh',binh,status)
            contxt = 'Failed to get binh parameter'
            IF ( status .NE. 0 ) GOTO 999
            IF ( binh.EQ.0 ) binh = 1

            CALL UCLGSB('wtmapfix',wtmapfix,status)
            contxt = 'Failed to get wtmapfix parameter'
            IF ( status .NE. 0 ) GOTO 999

            CALL UCLGSB('swmapx',swmapx,status)
            contxt = 'Failed to get swmapx parameter'
            IF ( status .NE. 0 ) GOTO 999

            CALL UCLGSB('swmapy',swmapy,status)
            contxt = 'Failed to get swmapy parameter'
            IF ( status .NE. 0 ) GOTO 999

            CALL UCLGSI('wmapver',wmapver,status)
            contxt = 'Failed to get wmapver parameter'
            IF ( status .NE. 0 ) GOTO 999

            needwmco = .TRUE.

         ENDIF

      ELSE

         buildwmap = .FALSE.

      ENDIF

c Get the filename for a FITS format output light curve
      
      CALL UCLGST('fitsbinlc',fitsbinlc,status)
      contxt = 'Failed to get fitsbinlc parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(fitsbinlc) .EQ. 'NONE' ) fitsbinlc = ' '

c Get the filename for a QDP format output light curve

      CALL UCLGST('qdpfile',qdpfile,status)
      contxt = 'Failed to get qdpfile parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(qdpfile) .EQ. 'NONE' ) qdpfile = ' '

      IF ( qdpfile .NE. ' ' ) THEN
         CALL fcecho('Warning : QDP files should no longer be used')
      ENDIF

      buildlc = (fitsbinlc .NE. ' ') .OR. (qdpfile .NE. ' ')

c If a binned light curve is required then get the binning factor,
c the threshold fraction for whether a bin should be output, and the
c threshold warning fraction (if the number of counts in a bin rejected
c due to thresholding exceeds this fraction times the average number
c of counts in a bin then a warning is issued).

      IF ( buildlc ) THEN

         CALL UCLGSD('binlc',binlc,status)
         contxt = 'Failed to get binlc parameter'
         IF ( status .NE. 0 ) GOTO 999

         IF ( binlc .LE. 0.d0 ) THEN
            CALL fcecho(
     &         'Warning: Light curve binsize =< 1., setting to 1.')
            binlc = 1.d0
         ENDIF

         CALL UCLGSD('lcthresh',lcthresh,status)
         contxt = 'Failed to get lcthresh parameter'
         IF ( status .NE. 0 ) GOTO 999
   
         CALL UCLGSD('lcthwarn',lcthwarn,status)
         contxt = 'Failed to get lcthwarn parameter'
         IF ( status .NE. 0 ) GOTO 999

         CALL UCLGSD('lcstart',lcstart,status)
         contxt = 'Failed to get lcstart parameter'
         IF ( status .NE. 0 ) GOTO 999

         CALL UCLGSB('lctzero',lctzero,status)
         contxt = 'Failed to get lctzero parameter'
         IF ( status .NE. 0 ) GOTO 999

c Note that we need the energy info to write the PHALCUT and PHAHCUT
c keywords in the output file

         needener = .TRUE.

      ENDIF

c Get the filename for a QDP format unbinned output light curve

      CALL UCLGST('unbinlc',unbinlc,status)
      contxt = 'Failed to get unbinlc parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(unbinlc) .EQ. 'NONE' ) unbinlc = ' '
      wunbinlc = ( unbinlc .NE. ' ' )

c Get the filename for any region selection

      CALL UCLGST('regionfile',regionfile,status)
      contxt = 'Failed to get regionfile parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(regionfile) .EQ. 'NONE' ) regionfile = ' '

c Get the filename for any time selection

      CALL UCLGST('timefile',timefile,status)
      contxt = 'Failed to get timefile parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(timefile) .EQ. 'NONE' ) timefile = ' '

c set whether to adjust output GTI to match frame boundaries

      CALL UCLGSB('adjustgti',adjustgti,status)
      contxt = 'Failed to get adjustgti parameter'
      IF ( status .NE. 0 ) GOTO 999

c if a GTI file is in use for time selection then get the name of the
c extension to use.

      CALL UCLGST('gtinam',tfilgtinm,status)
      contxt = 'Failed to get gtinam parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(tfilgtinm) .EQ. 'NONE' ) tfilgtinm = ' '

c Get the columns to be used for X and Y

      CALL UCLGST('xcolf',xcolf,status)
      contxt = 'Failed to get xcolf parameter'
      IF ( status .NE. 0 ) GOTO 999
      CALL upc(xcolf)

      CALL UCLGST('ycolf',ycolf,status)
      contxt = 'Failed to get ycolf parameter'
      IF ( status .NE. 0 ) GOTO 999
      CALL upc(ycolf)

      detimage = ( xcolf .EQ. 'DETX' ) .OR. ( xcolf .EQ. 'DX' ) 

c Check for valid X and Y columns. If either of these are NONE and we are making
c an image or selecting by region then generate an error.

      haveimco = .TRUE.
      IF (xcolf .EQ. 'NONE' .OR. ycolf .EQ. 'NONE') THEN
         haveimco = .FALSE.
         IF (buildimage .OR. regionfile .NE. ' ') THEN
            contxt = 
     &     'xcolf and/or ycolf is NONE but image coordinates required'
            status = -1
            GOTO 999
         ENDIF
      ENDIF

c If an image is being extracted get the column for z and the digitization
c for X and Y - if not then make sure that the digitization is set to 1.

      xint = 1.0d0
      yint = 1.0d0

      IF ( buildimage ) THEN

         CALL UCLGST('zcolf', zcolf, status)
         contxt = 'Failed to get zcolf parameter'
         IF ( status .NE. 0 ) GOTO 999
         CALL upc(zcolf)

         IF ( zcolf .EQ. 'NONE' ) zcolf = ' '
         zimage = ( zcolf .NE. ' ' )

         CALL UCLGSD('xint', xint, status)
         contxt = 'Failed to get xint parameter'
         IF ( status .NE. 0 ) GOTO 999
         CALL UCLGSD('yint', yint, status)
         contxt = 'Failed to get xint parameter'
         IF ( status .NE. 0 ) GOTO 999

      ENDIF

c Get the column to be used for time

      CALL UCLGST('tcol',tcol,status)
      contxt = 'Failed to get tcol parameter'
      IF ( status .NE. 0 ) GOTO 999
      CALL upc(tcol)

C Get the column to be used for chip id

      CALL UCLGST('ccol',ccol,status)
      contxt = 'Failed to get ccol parameter'
      IF ( status .NE. 0 ) GOTO 999
      CALL upc(ccol)

C Get the column to be used for grade/pattern

      CALL UCLGST('gcol',gcol,status)
      contxt = 'Failed to get gcol parameter'
      IF ( status .NE. 0 ) GOTO 999
      CALL upc(gcol)
      IF ( gcol .EQ. ' ' ) gcol = 'NONE'
      needgrade = .TRUE.
      IF ( gcol .EQ. 'NONE' ) needgrade = .FALSE.

      IF ( needgrade ) THEN
         CALL UCLGST('gstring',gstring,status)
         contxt = 'Failed to get gstring parameter'
         IF ( status .NE. 0 ) GOTO 999
         CALL upc(gstring)
         IF ( gstring .EQ. 'NONE' .OR. gstring .EQ. ' ' ) 
     &     needgrade = .FALSE.
      ENDIF

c Get the column to be used for energy (ie PHA/PI)

      IF ( needener ) THEN

         CALL UCLGST('ecol',ecol,status)
         contxt = 'Failed to get ecol parameter'
         IF ( status .NE. 0 ) GOTO 999
         CALL upc(ecol)

c If this has been set to NONE then turn off needener and hope the user
c knew what they were doing !

         IF ( ecol .EQ. 'NONE' ) needener = .FALSE.

      ENDIF

c If a WMAP is required then get the columns for WMAP axes

      IF ( buildwmap ) THEN

         CALL UCLGST('xcolh',xcolh,status)
         contxt = 'Failed to get xcolh parameter'
         IF ( status .NE. 0 ) GOTO 999
         CALL upc(xcolh)

         CALL UCLGST('ycolh',ycolh,status)
         contxt = 'Failed to get ycolh parameter'
         IF ( status .NE. 0 ) GOTO 999
         CALL upc(ycolh)

      ENDIF

c Get the name for any ascii output file giving the final gtis

      CALL UCLGST('gtitxt',gtitxt,status)
      contxt = 'Failed to get gtitxt parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(gtitxt) .EQ. 'NONE' ) gtitxt = ' '

c and any output xronos window file giving the final gtis

      CALL UCLGST('xronwn',xronoutfile,status)
      contxt = 'Failed to get xronwn parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(xronoutfile) .EQ. 'NONE' ) xronoutfile = ' '

c Get the names for the events and gti extensions in the events file(s)

      CALL UCLGST('events',eventname,status)
      contxt = 'Failed to get eventname parameter'
      IF ( status .NE. 0 ) GOTO 999

      CALL UCLGST('gti',evgtinm,status)
      contxt = 'Failed to get gti parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the flag specifying whether the events are time-ordered

      CALL UCLGSB('timeorder',timeorder,status)
      contxt = 'Failed to get timeorder parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the MJD reference to be used instead of that in the events
c file

      CALL UCLGSD('timeref',mjdref,status)
      contxt = 'Failed to get mjdref parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the TIMEPIXR value to be used if it is not given in the input event file(s)

      CALL UCLGSD('tpixrpar',tpixrpar,status)
      contxt = 'Failed to get tpixrpar parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the file containing a list of keywords to ignore when copying
c headers

      CALL UCLGST('eventkey',eventkey,status)
      contxt = 'Failed to get eventkey parameter'
      IF ( status .NE. 0 ) GOTO 999
      IF ( xupc(eventkey) .EQ. 'NONE' ) eventkey = '  '

c Get the header parameters to read to get the maximum for various
c columns - these are only used if TLMAX# are absent

      IF ( needener ) THEN

         CALL UCLGST('phamax',mphakey,status)
         contxt = 'Failed to get mphakey parameter'
         IF ( status .NE. 0 ) GOTO 999

      ENDIF

      CALL UCLGST('xfkey',xfkey,status)
      contxt = 'Failed to get xfkey parameter'
      IF ( status .NE. 0 ) GOTO 999

      CALL UCLGST('yfkey',yfkey,status)
      contxt = 'Failed to get yfkey parameter'
      IF ( status .NE. 0 ) GOTO 999

      IF ( needwmco ) THEN

         CALL UCLGST('xhkey',xhkey,status)
         contxt = 'Failed to get xhkey parameter'
         IF ( status .NE. 0 ) GOTO 999

         CALL UCLGST('yhkey',yhkey,status)
         contxt = 'Failed to get yhkey parameter'
         IF ( status .NE. 0 ) GOTO 999

      ENDIF

c Get the copyall parameter

      IF ( eventsfile .NE. ' ' ) THEN
         CALL UCLGSB('copyall',copyall,status)
         contxt = 'Failed to get copyall parameter'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

c Get the clobber parameter

      CALL UCLGSB('clobber',clobber,status)
      contxt = 'Failed to get clobber parameter'
      IF ( status .NE. 0 ) GOTO 999


 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF

      RETURN
      END

c **********************************************************************

      FUNCTION xupc(string)

      CHARACTER*(*) xupc, string

c Function to return the upper case version of the input string

c Arguments
c       string      c        i: input string
c       xupc        c        r: upper case version of string

      xupc = ' '
      xupc = string(:MIN(len(string),len(xupc)))

      CALL upc(xupc)

      END







