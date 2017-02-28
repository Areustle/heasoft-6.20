
      SUBROUTINE ascaaf

*  kaa 12/29/93   based on yan's npsarf routine
*       20 Nov 1993. yan fernandez.
*
*      basic structure: read parameter file. open spectrum and response
*      files. Get WMAP from spectrum, and get energy channels from response
*      file. Convert every pixel in the WMAP to a (theta,phi) with origin
*      at the optical axis. This is necessary because the function that 
*      calculates effective area requires a position in polar coordinates.
*      For an extended source use a weighted average over the WMAP to get 
*      the effective area for an energy channel. For a point source just use
*      the WMAP to define the bounds of the selected region. Create the ARF 
*      file and write this effective area information to it.
*
* Modifications :
*      v1.01 kaa 2/23/94  Removed read of DET_{X,Y}SCL from GTWMAP and
*                         hardcoded pixel size until the keyword situation
*                         stabilises.
*      v2.00 kaa 3/29/94  New BETRNS from GIS team. Added GISEFF to calculate
*                         the GIS instrument efficiency for consistency with
*                         the operation of jbldarf. Added GISCNV to include
*                         GIS PSF for point source case. Dynamic memory used
*                         for main arrays.
*      v2.01 kaa 4/19/94  Trapped WMAP extending outside the field. Reads
*                         OPTIC# keywords if available.
*      v2.02 kaa 4/19/94  Had included thermal shield twice - removed from GIS
*                         efficiency.
*      v2.10 kaa 5/19/94  Optional image map can be read. If so this is convolved
*                         with the PSF to make the WMAP used for extended sources.
*      v2.20 kaa  6/7/94  Option to use Maxim Markevitch's interpolation on ray trace
*                         results to get the mirror PSF.
*      v2.30 kaa 6/28/94  Changes by Y. Ishisaki to ensure consistency with jBLDARF.
*                         Also reads new EXTRACTOR output with the HDUVERS# keywords
*                         in the WMAP - should handle the case of low position
*                         resolution modes in the GIS. The gaussian fudge to the
*                         effective areas is applied if the fudge parameter
*                         is set.
*      v2.31 kaa 8/11/94  Increased string lengths for filenames.
*      v2.40 kaa 3/7/95   Added No-RTI option for GIS data with RISEBINS<2.
*      v2.50 kaa 9/5/95   Uses v4.0 of gisres and v95a of the XRT code. Removed
*                         imgfile option since it is probably not the way to do
*                         this.
*      v2.51 kaa 11/22/95 Improved the diagnostic information that is written.
*                         Bug fix - the fudge option for GIS only allowed the
*                         area fudge to be included.
*      v2.52 kaa 12/5/95  Added CALDB support for parameters.
*      v2.53 kaa 2/26/96  Uses v95b of the XRT code.
*      v2.60 kaa 4/10/96  Included new arffilter code to correct XRT effective
*                         areas. Removed Maxim's PSF option since now XRT PSF
*                         file available.
*      v2.61 kaa 5/6/96   New version of arffilter code with slight changes to
*                         relative normalizations. Added output of names of
*                         calibration files used.
*      v2.62 kaa 5/31/96  Now only requires bethick and grid parameters - reads
*                         instrument from input spectrum to find out whether to
*                         get gis2 or gis3 cal files. Also supports AUTO option
*                         on all cal files.
*      v2.63 kaa 7/31/96  Added hidden parameter to calculate the ARF given a
*                         uniform input surface brightness within the region 
*                         defined by the WMAP.
*      v2.64 kaa 9/17/96  Fixed bus error generated under Solaris for GIS data
*                         with the extended source option. Added expr argument
*                         to gtcalf call which tries to find an appropriate rmf
*                         - this should help pick between those available.
*      v2.65 kaa 1/21/97  Improved descriptive output - includes size of 
*                         selected region
*      v2.70 kaa 2/4/97   Added option of reading a background map that is
*                         subtracted from the WMAP.
*      v2.71 kaa 2/11/97  New fft version of giscnv from Ping H. and Rich F.
*      v2.72 kaa 3/14/97  Updated fft routine from Ping. Fixed bug in CLCPPS
*                         which calculated source position as 0.5 WMAP bins off
*                         when using the simple option.
*      v2.80 kaa 8/21/98  Removed clcpsf routine and included in clcprf. Forced
*                         calculation of the psf array on bins smaller than
*                         0.1x0.1 mm^2 to avoid a bug that arises using GIS
*                         data in mode with 1mm pixels. This has slowed the
*                         code down slightly.
*      v2.81 kaa 8/28/98  Fixed bug that caused a crash if the GIS WMAP region
*                         was expanded to the whole detector (256 bins).
*      v3.00 kaa 10/5/98  Added ability to use raytrace output for the effective
*                         area and PSF information
*      v3.10 kaa 1/10/02  Added support for HDUVERS=2 WMAPs. This corrects a
*                         slight error in the handling of binned WMAPs. The
*                         off-axis angles were wrong by (binning-1) 
*                         detector pixels. For default binnings this makes
*                         no difference for the GIS and introduces an error
*                         of 11.1 arcsec for the SIS.
*
* Parameters required by ASCAARF are :
*   phafile   s     Input spectrum
*   rmffile   s     Input RMF file
*   outfile   s     Output ARF file
*   raytrace  s     Use raytrace input ?
*   point     b     Is this a point source ?
*   simple    b     Is the point source the center of the WMAP ?
*   Xposition r     \ Source position in detector coordinates if .not.simple
*   Yposition r     /
*   fudge     b     Fudge the effective areas with the gaussian.
*   arffil    b     Use the ARF filter modification
*   bkgfil    s     Optional background map file
*   uniform   b     Assume that the input surface brightness is uniform
*   qclobber  b     Overwrite output file ?
*   bethick   s     Cal. file with GIS Be thickness
*   grid      s     Cal. file with GIS grid
*   xrtrsp    s     XRT response file
*   xrtpsf    s     XRT PSF file
*   rayfile   s     Rootname for raytrace files
*   mode      s     Mode



* Local variables
*    qclobber      overwrite ARF file
*    point         is a point source
*    simple        point source is assumed in center of WMAP
*    spfil         spectrum filename
*    rsfil         RMF filename
*    arffil        ARF filename
*    instrum       instrument (0-3)
*    wmsize(2)     size of WMAP
*    wmstrt(2)     position in WMAP to start reading image from file
*    wmend(2)      position in WMAP to end reading image from file
*    wmoff(2)      first detector image pixel in the WMAP
*    wbinfac       linear binning factor in WMAP
*    optaxs(2)     optical axis for instrument in detector coordinates
*    nenerg        number of energies
*    wmap          the WMAP
*    theta         the angle from the optical axes for pixel (i,j) in WMAP
*    phi           the azimuthal angle for pixel (i,j) in WMAP
*    bemap         Be thickness map for GIS
*    grdmap        Grid map for GIS
*    psf           The PSF map
*    bkgmap        Optional background map
*    binsze        the size (mm) of each WMAP bin
*    abnsze        the size (arcmin) of each WMAP bin
*    regsze        the size of the selected region in arcmin^2
*    lower         lower energies
*    hiher         upper energies
*    middl         mid energies
*    arf           effective areas
*    waoaa         weighted off-axis angle
*    gbfil         GIS Be thickness file
*    ggfil         GIS grid file
*    srcdet        Source position in detector coords for a point source
*    source        Source position in (theta,phi) for a point source
*    hist          Set of history records to be written to ARF
*    qnorti        Logical for whether there is RTI
*    rbnsze        Binsize of raytrace images
*    raycen        The optical axis position in raytrace image coordinates

* Pointers for dynamic arrays

      INTEGER wmap, psf, theta, phi, bemap, grdmap, work
      INTEGER lower, hiher, middl, arf, bkgmap
      INTEGER raymp1, raymp2, raympi, rayene


      REAL binsze, abnsze, waoaa, regsze, rbnsze
      REAL source(2), srcdet(2), raycen(2)

      INTEGER outlen, status
      INTEGER spu ,rsu, arfu, arfsize, nhist
      INTEGER instrum, wmsize(2), wmstrt(2), wmend(2), wmrep, nenerg
      INTEGER wbinfac, wmoff(2)
      INTEGER nrayen, nraymp(2)
      REAL optaxs(2)

      LOGICAL fudge, qarffl, qclobber, point, simple, qnorti, qunif
      LOGICAL raytrace

      character(255) spfil, rsfil, arffil, bkgfil
      character(255) gbfil, ggfil, xrtrsp, xrtpsf, rayfile
      character(72) contxt
      character(80) hist(50)
      character(4) cinstr

* Dynamic memory allocation stuff
C  the following MEM common block definition is in the system iraf77.inc file
C
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,  4 = Integer*4
C     1 is boolean
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 complex


* External functions
*    fcstln   the length of a string

      INTEGER  fcstln

      EXTERNAL fcstln

*necessary for some ftools

      character(40) taskname
      common /task/ taskname

      call fcecho('ASCAARF vers 3.10  10 Jan 2001.')

      taskname = 'ascaarf v3.10'

* Get the parameters from "ascaarf.par" file using gpasca.

      call gpasca(spfil, rsfil, arffil, raytrace, point, simple, 
     &            srcdet, fudge, qarffl, bkgfil, qunif, qclobber, 
     &            gbfil, ggfil, xrtrsp, xrtpsf, rayfile, status)

      contxt = 'Error in getting parameters'
      if (status.ne.0) goto 999

c initialize the reading of raytrace information and get the number of
c energies and map sizes of the raytrace data

      IF ( raytrace ) THEN

         CALL rayres_init(rayfile, nrayen, nraymp, rbnsze, raycen, 
     &                    status)
         contxt = 'Error reading the raytrace files'
         IF ( status .NE. 0 ) goto 999

      ELSE

* initialize the XRT effective area and PSF file calculations

         CALL xrtea95b_init(xrtrsp, status)
         contxt = 'Error in finding the XRT effective area file'
         IF ( status .NE. 0 ) goto 999

         CALL xrtpsf95b_init(xrtpsf, status)
         contxt = 'Error in finding the XRT PSFfile'
         IF ( status .NE. 0 ) goto 999

      ENDIF

*unit numbers for spectrum and response and arffil.

      spu=12
      rsu=13
      arfu=14

* create arf fits file - if it already exists and qclobber is set then
* overwrite

      outlen=fcstln(arffil)
      if ( qclobber ) call clobber(arffil(:outlen),status)

      call ftinit(arfu,arffil(:outlen),arfsize,status)

      contxt = 'Error creating arf file'
      if (status.ne.0) goto 999

* Read the spectrum and RMF files to get the sizes of the arrays
* that have to be allocated

      CALL gtsize(spu, spfil, rsu, rsfil, wmsize, wmstrt, wmend,
     &            wmrep, nenerg, status)
      contxt = 'Failed to get sizes of arrays'
      IF ( status .NE. 0 ) GOTO 999

* Get the memory for map and energy arrays required

      wmap = 0
      psf = 0
      theta = 0
      phi = 0
      bemap = 0
      grdmap = 0
      work = 0
      bkgmap = 0
      lower = 0
      hiher = 0
      middl = 0
      arf = 0
      rayene = 0
      raymp1 = 0
      raymp2 = 0
      raympi = 0

      CALL udmget(wmsize(1)*wmsize(2), 6, wmap, status)
      contxt = 'Insufficient memory for wmap array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(wmsize(1)*wmsize(2), 7, psf, status)
      contxt = 'Insufficient memory for psf array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(wmsize(1)*wmsize(2), 6, theta, status)
      contxt = 'Insufficient memory for theta array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(wmsize(1)*wmsize(2), 6, phi, status)
      contxt = 'Insufficient memory for phi array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(wmsize(1)*wmsize(2), 6, bemap, status)
      contxt = 'Insufficient memory for bemap array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(wmsize(1)*wmsize(2), 6, grdmap, status)
      contxt = 'Insufficient memory for grdmap array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(wmsize(1)*wmsize(2), 6, work, status)
      contxt = 'Insufficient memory for work array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(wmsize(1)*wmsize(2), 6, bkgmap, status)
      contxt = 'Insufficient memory for bkgmap array'
      IF ( status .NE. 0 ) goto 999

      CALL udmget(nenerg, 6, lower, status)
      contxt = 'Insufficient memory for lower array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(nenerg, 6, hiher, status)
      contxt = 'Insufficient memory for hiher array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(nenerg, 6, middl, status)
      contxt = 'Insufficient memory for middl array'
      IF ( status .NE. 0 ) goto 999
      CALL udmget(nenerg, 6, arf, status)
      contxt = 'Insufficient memory for arf array'
      IF ( status .NE. 0 ) goto 999

      IF ( raytrace ) THEN
         CALL udmget(nrayen, 6, rayene, status)
         contxt = 'Insufficient memory for rayene array'
         CALL udmget(nraymp(1)*nraymp(2), 6, raymp1, status)
         contxt = 'Insufficient memory for raymp1 array'
         CALL udmget(nraymp(1)*nraymp(2), 6, raymp2, status)
         contxt = 'Insufficient memory for raymp2 array'
         CALL udmget(nraymp(1)*nraymp(2), 6, raympi, status)
         contxt = 'Insufficient memory for raympi array'
      ENDIF

* Read WMAP and its description from the spectrum - also sets the qnorti
* flag for GIS data with no RTI.

      CALL gtwmap(spu, spfil, MEMR(wmap), wmsize(1), wmsize(2), wmstrt, 
     &            wmend, wmrep, wmoff, wbinfac, instrum, binsze, abnsze, 
     &            optaxs, regsze, qnorti, qunif, status)
      contxt = 'Failed to read WMAP'
      IF ( status .NE. 0 ) GOTO 999

* If required read the background map file and subtract from the current
* wmap

      CALL gtbmap(bkgfil, MEMR(bkgmap), wmsize(1), wmsize(2), wmstrt, 
     &            wmend, status)
      contxt = 'Failed to read background map'
      IF ( status .NE. 0 ) GOTO 999

* write information to the history records

      hist(1) = 'ARF created by '//taskname
      hist(2) = 'from '//spfil(:fcstln(spfil))
      hist(3) = 'using '//rsfil(:fcstln(rsfil))
      IF ( bkgfil .NE. 'none' ) THEN
         hist(4) = 'and background '//bkgfil(:fcstln(bkgfil))
      ELSE
         hist(4) = ' '
      ENDIF
      IF ( point ) THEN
         hist(5) = 'with point source algorithm'
      ELSE
         IF ( raytrace ) THEN
            hist(5) = 'using raytrace data'
         ELSE
            hist(5) = 'with extended source algorithm'
         ENDIF
      ENDIF
      hist(6) = 'XRT effec area from '//xrtrsp(:fcstln(xrtrsp))
      hist(7) = '    PSF from        '//xrtpsf(:fcstln(xrtpsf))
      nhist = 7
      IF ( instrum .GE. 2 ) THEN
         hist(nhist+1) = 'GIS Be map from    '//gbfil(:fcstln(gbfil))
         hist(nhist+2) = 'GIS grid map from  '//ggfil(:fcstln(ggfil))
         nhist = nhist + 2
      ENDIF
      IF ( qunif ) THEN
         nhist = nhist + 1
         hist(nhist) = 'uniform input surface brightness assumed'
      ENDIF
        
      hist(nhist+1) = ' '
      CALL fcecho(hist(nhist+1))
      WRITE(hist(nhist+2),'(a,i4,a,i4,a)') 
     &       ' Input WMAP array has size ', (wmend(1)-wmstrt(1)+1),
     &       ' by ', (wmend(2)-wmstrt(2)+1),  ' bins'
      CALL fcecho(hist(nhist+2))
      WRITE(hist(nhist+3),'(a,i4,a,i4,a)') 
     &       '               expanded to ', wmsize(1), ' by ', 
     &                       wmsize(2), ' bins'
      CALL fcecho(hist(nhist+3))
      WRITE(hist(nhist+4),'(a,i4,1x,i4)') 
     &              ' First detector image pixel in the WMAP is ', 
     &              wmoff(1), wmoff(2)
      CALL fcecho(hist(nhist+4))
      WRITE(hist(nhist+5),'(i4,a)') wbinfac, 
     &              ' detector pixels per WMAP bin'
      CALL fcecho(hist(nhist+5))
      WRITE(hist(nhist+6),'(a,f9.5,a)') ' WMAP bin size is ', binsze, 
     &              ' mm'
      CALL fcecho(hist(nhist+6))
      WRITE(hist(nhist+7),'(a,f9.5,a)') '                  ', abnsze, 
     &              ' arcmin'
      CALL fcecho(hist(nhist+7))
      nhist = nhist + 7
      IF ( regsze .GT. 0. ) THEN
         WRITE(hist(nhist+1),'(a,1pg12.5,a)') 
     &              ' Selected region size is ', regsze, ' arcmin^2'
         CALL fcecho(hist(nhist+1))
         nhist = nhist + 1
      ENDIF
      WRITE(hist(nhist+1),'(a,f6.2,1x,f6.2)') 
     &              ' Optical axis is detector pixel ', optaxs(1), 
     &              optaxs(2)
      CALL fcecho(hist(nhist+1))
      hist(nhist+2) = ' '
      CALL fcecho(hist(nhist+2))
      nhist = nhist + 2
      IF ( qnorti ) THEN
         WRITE(hist(nhist+1),'(a)') ' Making correction for no RTI'
         CALL fcecho(hist(nhist+1))
         nhist = nhist + 1
      ENDIF

* Read the RMF file to get the energies to be used when calculating
* the ARF

      CALL gtenrg(rsu, rsfil, nenerg, MEMR(lower), MEMR(hiher), 
     &                  MEMR(middl), status)
      contxt = 'Failed to get energies from RMF file'
      IF ( status .NE. 0 ) GOTO 999

      WRITE(hist(nhist+1), '(i5,a)') nenerg, ' energies from RMF file'
      CALL fcecho(hist(nhist+1))
      nhist = nhist + 1

      IF ( fudge ) THEN
         WRITE(hist(nhist+1), '(a)') ' Effective area fudge applied'
         CALL fcecho(hist(nhist+1))
         nhist = nhist + 1
      ENDIF
      IF ( qarffl ) THEN
         WRITE(hist(nhist+1), '(a)') ' Arf filter applied'
         CALL fcecho(hist(nhist+1))
         nhist = nhist + 1
      ENDIF
      IF ( qunif ) THEN
         WRITE(hist(nhist+1), '(a)') 
     &      ' Uniform input surface brightness assumed'
         CALL fcecho(hist(nhist+1))
         nhist = nhist + 1
      ENDIF

* If the instrument is a GIS then get the Be and grid maps

      IF ( instrum .GE. 2 ) THEN
         WRITE(cinstr,'(a,i1)') 'GIS', instrum
         CALL gtgmap(spu, gbfil, wmsize(1), wmsize(2), wbinfac,
     &               wmoff, cinstr, 'WINTHICK', MEMR(bemap),
     &               status)
         contxt = 'Failed to get Be window thickness map'
         IF (status .NE. 0) GOTO 999
         CALL gtgmap(spu, ggfil, wmsize(1), wmsize(2), wbinfac,
     &               wmoff, cinstr, 'GRIDTRNS', MEMR(grdmap),
     &               status)
         contxt = 'Failed to get grid transmission map'
         IF (status .NE. 0) GOTO 999
      ENDIF

* Set up the theta and phi arrays giving the WMAP positions in polar
* coordinates wrt the optical axis.

      CALL clcang(wmsize(1), wmsize(2), wmoff, wbinfac, optaxs, abnsze, 
     &            MEMR(theta), MEMR(phi))

* If a point source has been specified then get its position in polar
* coordinates

      IF ( point ) THEN
         CALL clcpps(wmoff, wbinfac, wmstrt, wmend, optaxs, 
     &               abnsze, srcdet, source, simple, hist(nhist+1))
         CALL fcecho(' ')
         CALL fcecho(hist(nhist+1))
         CALL fcecho(hist(nhist+2))
         CALL fcecho(hist(nhist+3))
         nhist = nhist + 3
      ENDIF

* Now the routines to actually calculate the ARF.

      IF ( point ) THEN

         CALL clcprf(wmsize(1), wmsize(2), nenerg, MEMR(middl),
     &               MEMR(lower), MEMR(hiher), source, 
     &               instrum, MEMR(theta), MEMR(phi), MEMD(psf), 
     &               binsze, abnsze, MEMR(wmap), MEMR(bkgmap),
     &               MEMR(grdmap), MEMR(bemap), waoaa, MEMR(arf), fudge)

      ELSE

         IF ( raytrace ) THEN

            CALL clcrrf(wmsize(1), wmsize(2), wmoff, wbinfac, nenerg, 
     &                  MEMR(middl), MEMR(lower), MEMR(hiher), instrum,
     &                  MEMD(psf), binsze, abnsze, MEMR(wmap), 
     &                  MEMR(bkgmap), MEMR(grdmap), MEMR(bemap), 
     &                  optaxs, MEMR(arf), fudge, rayfile, nrayen, 
     &                  MEMR(rayene), nraymp(1), nraymp(2), 
     &                  MEMR(raymp1), MEMR(raymp2), MEMR(raympi),
     &                  rbnsze, raycen)

         ELSE

            CALL clcerf(wmsize(1), wmsize(2), nenerg, MEMR(middl),
     &                  MEMR(lower), MEMR(hiher), instrum, MEMR(theta), 
     &                  MEMR(phi), MEMD(psf), binsze, abnsze, 
     &                  MEMR(wmap), MEMR(bkgmap), MEMR(grdmap), 
     &                  MEMR(bemap), MEMR(work), waoaa, MEMR(arf), 
     &                  fudge)

         ENDIF

      ENDIF

c Modify the ARF by the arffilter correction and if the qnorti flag is
c set and the detector is one of the GIS' then correct for no RTI rejection.

      CALL corarf(instrum, qnorti, qarffl, nenerg, MEMR(middl), 
     &            MEMR(arf))

c Write the ARF file

      CALL wrtarf(arfu, nenerg, instrum, waoaa, MEMR(lower),
     &            MEMR(hiher), MEMR(middl), MEMR(arf), hist, nhist, 
     &            rsfil, status)
      contxt = 'Failed to write the ARF'
      IF ( status .NE. 0 ) GOTO 999
                  
 999  continue
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF
      return

      end
************************************************************************





