
      SUBROUTINE ascaep

*  kaa 20/06/94   based on kaa's ascaarf routine
*
*      basic structure: read parameter file. open spectrum and response
*      files. Get WMAP from spectrum, and get energy channels from response
*      file. Convert every pixel in the WMAP to a (theta,phi) with origin
*      at the optical axis. This is necessary because the function that
*      calculates effective area requires a position in polar coordinates.
*      Use a weighted average over the WMAP to get
*      the effective area for an energy channel. Create the EFF image
*      file and write this effective area information to it.
*
* Modifications :
*     v1.00 Y.Ishisaki 15/06/94
*               both SIS and GIS
*               write weighting information as BIN table
*
*     v2.00 Y.Ishisaki 20/06/94
*               for GIS only
*
*     v2.01 EAG 10/21/94 removed sf read ability, make portable to all
*                        systems supported by FTOOLS, included in FTOOLS
*
*     v2.10 kaa 9/22/95  updated for new XRT library routines for the effective
*                        area. Also used v4.0 of the GIS detector efficiency.
*                        Tidied up the use of routines lifted from xspec.
*
*     v2.11 kaa 6/4/96   Replaced separate GIS2 and GIS3 parameters by unified
*                        scheme for the GIS. Also replaced all the individual
*                        RMF parameters by one parameter. All appropriate
*                        parameters now accept the AUTO argument.
*     v2.20 Ken Ebisawa, Y. Ishisaki, kaa 12/13/96
*                        Fixed the dynamic allocation memory error changing
*                        the allocation size for the spectral file. Trapped
*                        possible error reading SIS matrix.
*                        Released with a new parameter file and a help file
*     v2.21 kaa 12/20/96 Allowed multiple chips
*     v2.22 kaa 2/12/97  Added FFT code for GIS convolution as supplied by
*                        Ping and Rich F.
*     v2.23 kaa 3/14/97  New version of FFT code from Ping.
*     v2.24 kaa 6/22/98  Fixed not enough dynamic memory being allocated when
*                        the spectrum is grouped.
*     v2.25 kaa 7/7/98   Memory leak fixed in jcoord.c by Peter Wilson.
*                        Cosmetic changes to warning messages.
*     v2.26 kaa 7/18/98  Replaced calls to C routines to read rmf and calculate
*                        the efficiency by a few lines of Fortran in gtenrg1.f.
*                        This fixes the bug causing the SIS effmap to have
*                        values too low by a factor of ~1000.
*     v2.27 Jeff Guerber 1999-05-05. Call gtgmap with bin factor=256/imsize(1)
*                        instead of 1, so the calibration images get rebinned
*                        to the correctpixel sizes.
*     v2.28 kaa 8/7/01   Fixed error in calculating the SIS efficiencies from
*                        the RMF. Was giving efficiency of response energy 
*                        range I to channel I which is wrong.
*     v2.29 kaa 1/10/02  Added support for HDUVERS=2 WMAPs. Parallel changes to
*                        those implemented in ascaarf v3.10.
*
* Parameters required by ASCAARF are :
*   phafile   s     Input spectrum
*   pha_lo    i     Lower channel of spectrum to use
*   pha_hi    i     Upper channel of spectrum to use
*   wmapfile  s     Wmap file
*   rmffile   s     Input RMF file
*   outfile   s     Output EFF image file ( DET coordinate )
*   xrteff    b     Multiply XRT effective area ?
*   gispsf    b     Convolve GIS Point Spread Function ?
*   killit    b     Overwrite output file ?
*   bethick   s     Cal. file with GIS Be thickness
*   grid      s     Cal. file with GIS grid
*   xrtrsp    s     XRT effective area file
*   mode      s     Mode

* Local variables
*    killit        overwrite ARF file
*    spfil         spectrum filename
*    wmfil         WMAP filename
*    rsfil         RMF filename
*    effil         EFF filename
*    sensor        sensor (0-3)
*    optaxs(2)     optical axis for instrument in detector coordinates
*    npha          number of pha channels
*    nbin          number of pha bins (grouped channels)
*    bemap         Be thickness map for GIS
*    grdmap        Grid map for GIS
*    chpmap        Chip map for SIS (0 for no chip, 1 for chip)
*    lower         lower energies
*    hiher         upper energies
*    middl         mid energies
*    arf           effective areas
*    gbfil         GIS Be thickness files
*    ggfil         GIS grid files

* Pointers for dynamic arrays

      INTEGER efmap, wmap, bemap, grdmap, chpmap, phabuf
      INTEGER theta, phi, work
      INTEGER lower, hiher, middl, siseff, avgeff, weight

      INTEGER MAXPHS
      PARAMETER (MAXPHS=4096)
      CHARACTER*(MAXPHS) cgroup

      REAL binsze, abnsze

      INTEGER outlen, status
      INTEGER spu, wmu, rsu, efu, efsize, itype
      INTEGER sensor, npha, nbin, nchan, pha_lo, pha_hi
      INTEGER imsize(2), imoff(2)
      INTEGER wmsize(2), wmoff(2), wbinfac
      REAL optaxs(2)

      LOGICAL xrteff, gispsf, killit, qsysdt

      character(255) spfil, wmfil, rsfil, effil, teldef, contxt
      character(255) gbfil, ggfil, xrtrsp
      character(255) wrtstr
      character(4) cinstr, cchip

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

      INTEGER  fcstln, jc_init

      EXTERNAL fcstln, jc_init

*necessary for some ftools

      character(40) taskname
      common /task/ taskname

      call fcecho('ASCAEFFMAP vers 2.29  11 Jan 2002.')

      taskname = 'ascaeffmap v2.29'

* Get the parameters from "ascaeffmap.par" file using gpaeffm.

      call gpaeffm(sensor, cchip, spfil, wmfil, rsfil, effil, teldef,
     &             pha_lo, pha_hi, xrteff, gispsf, killit, gbfil,
     &             ggfil, xrtrsp, status)

      contxt = 'Error in getting parameters'
      if (status.ne.0) goto 999

* initialize the XRT effective area and PSF file calculations

      CALL xrtea95b_init(xrtrsp, status)

      contxt = 'Error in finding the XRT effective area file'
      if (status.ne.0) goto 999

* unit numbers for spectrum and response and arffil.

      spu=11
      wmu=12
      rsu=13
      efu=14

* jcoord initialize

      if ( sensor.lt.2 ) THEN
         status = jc_init(15, sensor, teldef)
         contxt = 'Error in reading teldef file'
         if ( status .NE. 0 ) goto 999
      endif

* create eff image fits file - if it already exists and clobber is set then
* overwrite

      outlen=fcstln(effil)
      if ( killit ) call clobber (effil(:outlen), status)
      call ftinit(efu,effil(:outlen),efsize,status)

      contxt = 'Error creating eff file'
      if (status.ne.0) goto 999

* Read the spectrum and RMF files to get the sizes of the arrays
* that have to be allocated

      CALL gtsize1(spu, spfil, wmu, wmfil, rsu, rsfil,
     &            npha, nbin, nchan, imsize, wmsize, cgroup,
     &            sensor, status)
      contxt = 'Failed to get size of spectrum or response'
      IF ( status .NE. 0 ) GOTO 999

* Get the memory for map and energy arrays required

      efmap = 0
      theta = 0
      phi = 0
      work = 0
      chpmap = 0
      bemap = 0
      grdmap = 0
      wmap = 0
      lower = 0
      hiher = 0
      middl = 0
      siseff = 0
      avgeff = 0
      weight = 0
      phabuf = 0

      CALL udmget(imsize(1)*imsize(2), 6, efmap, status)
      contxt = 'Insufficient memory for efmap array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(imsize(1)*imsize(2), 6, theta, status)
      contxt = 'Insufficient memory for theta array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(imsize(1)*imsize(2), 6, phi, status)
      contxt = 'Insufficient memory for phi array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(imsize(1)*imsize(2), 7, work, status)
      contxt = 'Insufficient memory for work array'
      IF ( status .NE. 0 ) GOTO 999

      IF ( sensor .LT. 2 ) THEN
         CALL udmget(imsize(1)*imsize(2), 3, chpmap, status)
         contxt = 'Insufficient memory for chpmap array'
         IF ( status .NE. 0 ) GOTO 999
      ELSE
         CALL udmget(imsize(1)*imsize(2), 6, bemap, status)
         contxt = 'Insufficient memory for bemap array'
         IF ( status .NE. 0 ) GOTO 999
         CALL udmget(imsize(1)*imsize(2), 6, grdmap, status)
         contxt = 'Insufficient memory for grdmap array'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

      CALL udmget(wmsize(1)*wmsize(2), 6, wmap, status)
      contxt = 'Insufficient memory for wmap array'
      IF ( status .NE. 0 ) GOTO 999

      CALL udmget(npha, 6, lower, status)
      contxt = 'Insufficient memory for lower array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(npha, 6, hiher, status)
      contxt = 'Insufficient memory for hiher array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(npha, 6, middl, status)
      contxt = 'Insufficient memory for middl array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(npha, 6, siseff, status)
      contxt = 'Insufficient memory for siseff array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nbin, 6, avgeff, status)
      contxt = 'Insufficient memory for avgeff array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nbin, 6, weight, status)
      contxt = 'Insufficient memory for weight array'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nchan*3, 6, phabuf, status)
      contxt = 'Insufficient memory for phabuf array'
      IF ( status .NE. 0 ) GOTO 999

* Read the spectrum

      CALL rddtog(spfil, nbin, npha, MEMR(phabuf), status, itype,
     &            qsysdt)
      contxt = 'Failed to read the spectrum'
      IF ( status .NE. 0 ) GOTO 999

      CALL fcecho(' ')
      WRITE(wrtstr, '(i5,a)') nbin,
     &                        ' grouped channels from the PHA file'
      CALL fcecho(wrtstr)
      CALL fcecho(' ')


* Read WMAP and its description from the WMAP

      CALL gtwmap1(wmu, wmfil, sensor, MEMR(wmap), wmsize(1), wmsize(2),
     &            wmoff, wbinfac, binsze, abnsze, optaxs, status)
      contxt = 'Failed to read WMAP'
      IF ( status .NE. 0 ) GOTO 999

      WRITE(wrtstr,'(a,f6.4,1x,a,f6.4,a)') ' Bin size is ', binsze,
     &                                 '(mm) = ', abnsze, '(arcmin)'
      CALL fcecho(wrtstr)
      WRITE(wrtstr,'(a,f6.2,1x,f6.2)') ' Optical axis is ', optaxs(1),
     &                             optaxs(2)
      CALL fcecho(wrtstr)

      WRITE(wrtstr,'(a,i5,1x,a,i5)') ' WMAP size is ',
     &                               wmsize(1), 'x', wmsize(2)
      CALL fcecho(wrtstr)
      WRITE(wrtstr,'(a,i5,1x,i5)') 
     &  ' First detector image pixel in WMAP is ', wmoff(1), wmoff(2)
      CALL fcecho(wrtstr)
      WRITE(wrtstr,'(a,i5)') ' WMAP bin factor is ',
     &                       wbinfac
      CALL fcecho(wrtstr)

      CALL fcecho(' ')

* Read the RMF file to get the energies to be used when calculating
* the EFF map

      CALL gtenrg1(rsu, rsfil, sensor, npha, MEMR(lower), MEMR(hiher),
     &                  MEMR(middl), MEMR(siseff), status)
      contxt = 'Failed to get energies from RMF'
      IF ( status .NE. 0 ) GOTO 999

      WRITE(wrtstr, '(i5,a)') npha,
     &                        ' ungrouped channels from the RMF file'
      CALL fcecho(wrtstr)
      CALL fcecho(' ')

* Rebin the energy bounds according to PHA bins

      IF ( npha .LT. nbin ) THEN
         CALL fcerr('Incompatible PHA and RMF files')
         GOTO 999
      ENDIF

      IF ( npha .GT. nbin ) THEN
         CALL rbenrg(npha, nbin, cgroup, MEMR(lower), MEMR(hiher),
     &               MEMR(middl), MEMR(siseff), status)
         contxt = 'Failed to group spectrum'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

* If the instrument is a GIS then get the Be and grid maps

      imoff(1) = 1
      imoff(2) = 1

      IF ( sensor .LT. 2 ) THEN
         CALL gtsmap(sensor, cchip, imsize(1), imsize(2), MEMS(chpmap))
      ELSE
         WRITE(cinstr, '(a,i1)') 'GIS', sensor
         CALL gtgmap(spu, gbfil, imsize(1), imsize(2), 256/imsize(1),
     &               imoff, cinstr, 'WINTHICK', MEMR(bemap),
     &               status)
         CALL gtgmap(spu, ggfil, imsize(1), imsize(2), 256/imsize(1),
     &               imoff, cinstr, 'GRIDTRNS', MEMR(grdmap),
     &               status)
      ENDIF
      contxt = 'Failed to get instrument map'
      IF (status .NE. 0) GOTO 999

* Set up the theta and phi arrays giving the image positions in polar
* coordinates wrt the optical axis.

      CALL clcang1(imsize(1), imsize(2), imoff, 1, optaxs, abnsze,
     &             MEMR(theta), MEMR(phi))

      pha_lo = max(1, pha_lo)
      pha_hi = min(nbin, pha_hi)

* Now the routines to actually calculate the EFF.

      CALL clceff(imsize(1), imsize(2), wmsize(1), wmsize(2),
     &            wmoff, wbinfac, nbin, MEMR(middl),
     &            MEMR(lower), MEMR(hiher), MEMR(siseff),
     &            sensor, MEMR(theta), MEMR(phi),
     &            binsze, abnsze, MEMS(chpmap), MEMR(grdmap),
     &            MEMR(bemap), MEMD(work), MEMR(wmap),
     &            MEMR(phabuf), pha_lo, pha_hi, xrteff, gispsf,
     &            MEMR(efmap), MEMR(avgeff), MEMR(weight))

c Write the EFF file

      CALL wrteff(efu, effil, spfil, wmfil, rsfil,
     &            sensor, taskname,
     &            imsize(1), imsize(2), MEMR(efmap),
     &            nbin, MEMR(lower), MEMR(hiher),
     &            MEMR(phabuf), MEMR(avgeff), MEMR(weight),
     &            status)

 999  continue

      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF

      return

      end
c ************************************************************************
