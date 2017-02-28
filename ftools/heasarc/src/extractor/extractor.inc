C Ning Gan 6/23/1998: changed the length of csdate... from 8 to 68.
c Global variables for extractor

c  infile            input filename
c  eventsfile        output events file
c  imagefile         output image file
c  phafile           output pha file
c  fitsbinlc         output FITS format light curve file
c  qdpfile           output QDP format light curve file
c  unbinlc           output QDP format unbinned light curve file
c  regionfile        region selection file
c  timefile          time selection file
c  gtitxt            output ascii file with final gtis
c  xronoutfile       output xronos window file with final gtis
c  eventkey          file containing list of keywords to ignore when copying

      character(255)        infile, eventsfile, imagefile, phafile, 
     &                     fitsbinlc, qdpfile, unbinlc, regionfile, 
     &                     timefile, gtitxt, xronoutfile, eventkey

      COMMON / ext_files / infile, eventsfile, imagefile, phafile, 
     &                     fitsbinlc, qdpfile, unbinlc, regionfile, 
     &                     timefile, gtitxt, xronoutfile, eventkey

c  needwmco          Need to process WMAP coordinate info
c  needener          Need to process energy (PHA) info
c  needgrade         Need to process grade info
c  haveimco          Have image coordinates which are not NONE
c  buildimage        Extracting an image
c  buildspec         Extracting a spectrum
c  buildwmap         Making a WMAP for the spectrum
c  wtmapfix          Set WMAP pixels outside selected region to -1
c  swmapx            Invert the WMAP X-axis when mapping to IMAGE coordinates
c  swmapy            Invert the WMAP Y-axis when mapping to IMAGE coordinates
c  buildlc           Extracting a binned lightcurve
c  wunbinlc          Extracting an unbinned lightcurve
c  detimage          Image is in detector coordinates
c  fullimage         Image output is full-size not bounding box
c  zimage            Image has a z-axis ie not just number of counts
c  dophase           Do phase selection
c  copyall           If extracting events file copy all extensions
c  clobber           Overwrite existing files
c  adjustgti         Adjust GTI files to match frame boundaries

      LOGICAL              needwmco, needener, needgrade, haveimco,
     &                     buildimage, buildspec, buildwmap, wtmapfix, 
     &                     swmapx, swmapy, buildlc, wunbinlc, detimage, 
     &                     fullimage, zimage, dophase, copyall, clobber,
     &                     adjustgti
      COMMON / ext_flags / needwmco, needener, needgrade, haveimco,
     &                     buildimage, buildspec, buildwmap, wtmapfix, 
     &                     swmapx, swmapy, buildlc, wunbinlc, detimage,
     &                     fullimage, zimage, dophase, copyall, clobber,
     &                     adjustgti

c  tfilgtinm         GTI extension name in gtifile
c  eventname         events extension name in the input events file
c  evgtinm           GTI extension name in the input events file
c  gtihdunm          GTI HDUNAME in the input events file
c  xcolf, ycolf      Columns to be used for X, Y in the output image
c  zcolf             Image will be mean of this column
c  xcolh, ycolh      Columns to be used for X, Y in the output WMAP
c  tcol              Column to be used for time
c  ecol              Column to be used for energy (PHA/PI)
c  ccol              Column to be used for CCD ID
c  gcol              Column to be used for the event grade/pattern
c  gstring           Grade filter string
c  mphakey           Keyword containing maximum PHA/PI
c  xfkey, xykey      Keywords containing maximum X, Y
c  xhkey, yhkey      Keywords containing maximum in columns used in WMAP


      character(30)         tfilgtinm, eventname, evgtinm, gtihdunm
      character(20)         xcolf, ycolf, zcolf, xcolh, ycolh, tcol, 
     &                     ecol, mphakey, xhkey, yhkey, xfkey, yfkey,
     &                     ccol, gcol
      character(72)         gstring

      COMMON / ext_names / tfilgtinm, eventname, evgtinm, gtihdunm, 
     &                     xcolf, ycolf, zcolf, xcolh, ycolh, tcol, 
     &                     ecol, mphakey, xhkey, yhkey, xfkey, yfkey,
     &                     ccol, gcol, gstring

c  timeorder         Flag specifying whether input events are time-ordered
c  origbin           Time resolution of input events

      DOUBLE PRECISION      origbin
      LOGICAL               timeorder

      COMMON / ext_evfile / origbin, timeorder
      
c  binf              binning factor for image
c  xint              Digitization for X axis of image (unbinned coordinates)
c  yint              Digitization for Y axis of image (unbinned coordinates)
c  specbin           binning factor for spectrum
c  binh              binning factor for WMAP
c  binlc             binsize for light curve
c  lcthresh          threshold fraction for keeping light curve
c  lcthwarn          threshold fraction for warning about lost counts
c  lctzero           if true then offset lightcurve times by time of first bin
c  lcstart           if > 0 then gives the time for the first light curve bin
c  mjdref            MJD reference to be used
c  equinox           Equinox to be used
c  radecsys          System used for RA and DEC
c  wmapver           The version number of the WMAP to be created

      DOUBLE PRECISION     binlc, lcthresh, lcthwarn, lcstart, mjdref
      DOUBLE PRECISION     xint, yint

      REAL                 equinox

      INTEGER              binf, specbin, binh, wmapver

      LOGICAL              lctzero

      CHARACTER(20)        radecsys

      COMMON / ext_input / binlc, lcthresh, lcthwarn, lcstart, mjdref, 
     &                     xint, yint, equinox, binf, specbin, binh, 
     &                     wmapver, lctzero, radecsys

c  csdate            Observation start date in the formats of  yyyy-dd-mm or
c                    dd/mm/yy
c  cstime            Observation start time
c  cedate            Observation stop date in the formats of  yyyy-dd-mm or
c                    dd/mm/yy
c  cetime            Observation stop time
c  scseqend          End time in spacecraft seconds (ROSAT-specific)
c  numobi            Number of OBIs (ROSAT-specific)
c  timedel           Maximum TIMEDEL of input event files
c  timepixr          TIMEPIXR of input event files
c  tpixrpar          TIMEPIXR value from parameter
c  fctype            CTYPE for image
c  fcrpix            CRPIX for image
c  fcrval            CRVAL for image
c  fcrdelt           CRDELT for image
c  fcrota            CROTA for image
c  fcname            CNAME for image
c  fmtype            MTYPE for image (2 allowed)
c  fmform            MFORM for image (2 allowed)
c  skypix            Pixel size in sky coordinates (assumed square).
c  nompnt            Nominal pointing position (ra, dec, roll)
c  foptic            Optical axis position for image
c  hctype            CTYPE for WMAP
c  hcrpix            CRPIX for WMAP
c  hcrval            CRVAL for WMAP
c  hcrdelt           CRDELT for WMAP
c  hcname            CNAME for WMAP
c  hoptic            Optical axis position for WMAP
c  hmtype            MTYPE for WMAP (2 allowed)
c  hmform            MFORM for WMAP (2 allowed)
c  emjdrfi           Reference time in event file (days)
c  emjdrff           Reference time in event file (fraction of a day)
c  qmjdref           If true then MJDREF was read rather than I/F split.
c  deadc             Dead-time correction
c  ebound            Lower and upper bounds on event file PHA/PI
c  gbound            Lower and upper bounds on event file grade/pattern
c  fbound            (Xlow,Ylow,Xhigh,Yhigh) for event file image coordinates
c  hbound            (Xlow,Ylow,Xhigh,Yhigh) for event file WMAP coordinates
c  instrument        Instrument
c  telescope         Telescope
c  datamode          Datamode
c  detector          Detector
c  filter            Filter
c  object            Object name
c  timeref           Time reference system (whether barycenter-corrected)
c  timesys           Reference time
c  timeunit          Time unit
c  regextname        EXTNAME to be used for the region extension if region filtering

      DOUBLE PRECISION     timedel, timepixr, tpixrpar, fcrpix(2), 
     &                     fcrval(2), fcrdelt(2), nompnt(3), foptic(2),
     &                     hcrpix(2), hcrval(2), hcrdelt(2), hoptic(2),
     &                     fcrota, skypix, emjdrff
      REAL                 deadc
      INTEGER              ebound(2), gbound(2), fbound(4), hbound(4)
      INTEGER              emjdrfi, scseqend, numobi
      LOGICAL              qmjdref
      character(40)        instrument, telescope, filter, datamode, 
     &                     detector, object
      character(68)        csdate, cstime, cedate, cetime
      character(20)        fctype(2), hctype(2), fcname(2), hcname(2),
     &                     fmtype(2), fmform(2), hmtype(2), hmform(2),
     &                     timeref, timesys, timeunit
      character(8)          regextname

      COMMON / ext_eread / timedel, timepixr, tpixrpar, fcrpix, fcrval,
     &                     fcrdelt, nompnt, foptic, hcrpix, hcrval, 
     &                     hcrdelt, hoptic, fcrota, skypix, emjdrff, 
     &                     deadc, ebound, gbound, fbound, hbound, 
     &                     emjdrfi, scseqend, numobi, qmjdref, 
     &                     instrument, telescope, filter, datamode, 
     &                     detector, object, csdate, cstime, cedate, 
     &                     cetime, fctype, hctype, fcname, hcname, 
     &                     fmtype, fmform, hmtype, hmform, timeref, 
     &                     timesys, timeunit, regextname

c  tstart            Data start time
c  tstop             Data stop time
c  expos             Calculated exposure time including phase binning
c  fbbox             Bounding box for output image (unbinned coordinates)
c  hbbox             Bounding box for output WMAP (unbinned coordinates)
c  foffset           Offset for region specified in pixels

      DOUBLE PRECISION    tstart, tstop, expos
      INTEGER             fbbox(4), hbbox(4), foffset(2)

      COMMON / ext_calc / tstart, tstop, expos, fbbox, hbbox, foffset

c  Coefficients from fixwmp mapping detector to sky coordinates
c  wfxf0, wfyf0          Image zero
c  wfxh0, wfyh0          Detector zero
c  xsign, ysign      Orientation swaps
c  wftheta             Rotation angle

      DOUBLE PRECISION    wfxf0, wfyf0, wfxh0, wfyh0
      DOUBLE PRECISION    xsign, ysign, wftheta

      COMMON / ext_fix / wfxf0, wfyf0, wfxh0, wfyh0, 
     &                   xsign, ysign, wftheta











