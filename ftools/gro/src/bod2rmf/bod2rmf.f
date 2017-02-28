C*******************************************************************************
C TASK NAME: bod2rmf
C
C FILE NAME: bod2rmf.f
C
C DESCRIPTION: FTOOL for creating RMF FITS files in XSPEC format for BATSE
C     occultation data (NHIS type).  A separate RMF file is required for each
C     CGRO orientation (constant within a given CGRO viewing period), source
C     direction, and BATSE detector (LAD).  The response matrix is generated
C     using LAD matrix integrator software (Version 3.0).  The corresponding
C     PHA FITS files in XSPEC format are created by FTOOL bod2pha.
C
C AUTHOR/DATE: Peter J.T. Leonard, RITSS, 08/99
C
C NOTES:
C
C USAGE:
C     Host: bod2rmf
C     IRAF: bod2rmf
C
C ROUTINES IMPLEMENTED IN THIS FILE:
C     subroutine bodrmf             - top level subroutine, called by IRAF
C                                     or host wrapper in hbod2rmf.c
C     subroutine get_chan_edges     - provides energies of channel edges for LAD
C     subroutine pre_cont_matrix    - oversees reading in of response matrix data
C     subroutine response_matrix    - produces response matrices for LAD data
C     subroutine some_angle_stuff   - angular transforms and scattering corrections
C     subroutine direct_angle       - puts direction vectors in CGRO coordinates
C     subroutine j2000_to_gro       - converts r.a. and dec. to CGRO coordinates
C     subroutine integrator_reader  - reads in LAD matrix data
C     subroutine transform          - transforms source r.a. and dec. to source
C                                     altitude and azimuth in CGRO coordinates
C     subroutine get_directions     - converts source and geocenter angles to
C                                     direction vectors in CGRO coordinates
C     subroutine gro_to_equatorial  - calculates Cartesian unit vectors in
C                                     equatorial coords in direction of CGRO axes
C     subroutine u_loader           - loads detector normals in CGRO coordinates
C     subroutine load_correct_data  - loads corrections to current LAD matrices
C     subroutine integrator_atscat  - integrates differential matrices
C     subroutine evaluate_matrix    - evaluates matrix at specific angle to normal
C     subroutine newmatrix          - energy rebinning of compressed matrix elements
C                                     into energy-binned matrix elements
C     subroutine transpose_matrix   - transposes matrix a to matrix b
C     subroutine unit_vector        - transforms spherical coordinates to
C                                     Cartesian unit vector
C     subroutine read_inst_resp     - reads in inst_resp.fits, which contains
C                                     detector-specific response matrix data
C     subroutine rsp_write2         - prepares for writing out of response matrix
C     subroutine write_rsp2         - writes out RMF FITS file in XSPEC format
C     subroutine cmprsp2            - compresses response matrix to XSPEC format
C
C MODIFICATION HISTORY:
C     Sandhia Bansal, 13 December, 2001:  The code was trying to access 0th
C         element of LOOKUP record.  LOOKUP was declared as 'LOOKUP(dim)' - so
C         no 0-th element existed.  This causes seg faults on linux, even though
C         it goes unnoticed on solaris.  Also 1-d ichanb and fchan when passed
C         to wtrmf1 in 2-d arrays, do not have the correct numbers in them.
C         This also causes seg fault on linux.  Fixed.
C
C     James Peachey, 27 October, 1999: changed routine name from
C         transpose to transpose_matrix to eliminate spurious warning
C         when compiled on Linux.
C*******************************************************************************
 
C*******************************************************************************
C SUBROUTINE: bodrmf
C
C DESCRIPTION: Program for creating RMF FITS files in XSPEC format for BATSE
C     occultation data (NHIS type).  A separate RMF file is required for each
C     CGRO orientation (constant within a given CGRO viewing period), source
C     direction, and BATSE detector (LAD).  The response matrix is generated
C     using LAD matrix integrator software (Version 3.0).  The corresponding
C     PHA FITS files in XSPEC format are created by FTOOL bod2pha.
C
C AUTHOR/DATE: Based on code from many authors, including G.N. Pendleton
C     (Marshall Space Flight Center), M. Brock (Marshall Space Flight Center),
C     Robert Radocinski (Jet Propulsion Laboratory), Chris Shrader (Goddard
C     Space Flight Center), Keith A. Arnaud (Goddard Space Flight Center),
C     and John R. Mattox (Boston University).
C
C NOTES:
C
C HIDDEN ARGUMENTS:
C     inrfil - name of input FITS file containing detector-specific response
C              matrix data
C
C ARGUMENTS:
C     ladnum - BATSE LAD number (0 to 7)
C     sradeg - r.a. of source in degrees (r*4)
C     sdecdg - dec. of source in degrees (r*4)
C     zradeg - r.a. of CGRO z-axis in degrees (r*4)
C     zdecdg - dec. of CGRO z-axis in degrees (r*4)
C     xradeg - r.a. of CGRO x-axis in degrees (r*4)
C     xdecdg - dec. of CGRO x-axis in degrees (r*4)
C     rmffil - name of output XSPEC RMF FITS file
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine pre_cont_matrix - oversees reading in of response matrix data
C     subroutine get_chan_edges  - provides energies of channel edges for LAD
C     subroutine j2000_to_gro    - converts r.a. and dec. to CGRO coordinates
C     subroutine response_matrix - produces response matrices for LAD data
C     subroutine apply_attenuation_correction - corrects response for HEXEL
C      entrance window. See Harmon et al. 2002, ApJS, 138, 149.
C     subroutine rsp_write2      - prepares for writing out of response matrix
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C     03/04, 1. Bug Fixes to evaluate matrix. 
C            2. Added code to correct for HEXEL entrance window.
C            3. Disabled viewper option because spacecraft angles are suspect.
C*******************************************************************************
 
      subroutine bodrmf ()
 

      double precision pi
      parameter (pi=3.141592653589793)
 
      double precision sdec_deg, sra_deg, xdec_deg, xra_deg, zdec_deg,
     &     zra_deg
 
      real azimuth_deg, chan_edges(130), direct_matrix(129,16),
     &     eazimuth_deg, eelev_deg, elev_deg, e_edges(129),
     &     sradeg, sdecdg, zradeg, zdecdg, xradeg, xdecdg
 
      integer det, nc_edges, ne_edges, max_recs, status, clobber
      integer vpnum
      integer*2 idet, nchan, nebin, nmaxebin
 
      character(100)  inrfil, rmffil, output_dir, data_dir, msg
      character(11)   mat_vers
 
      data nc_edges / 17 /
      data ne_edges / 66 /


c *****replace w/new code

C Define energy bin edges.
      data e_edges / 10.000,  12.000,  14.000,  16.000,  18.000,
     &  20.000,  23.000,  26.000,  30.000,  34.000,  39.000,  44.000,
     &  50.000,  57.000,  64.000,  72.000,  81.000,  91.000,  102.00,
     &  114.00,  128.00,  143.00,  160.00,  179.00,  199.00,  221.00,
     &  246.00,  273.00,  302.00,  334.00,  369.00,  407.00,  449.00,
     &  494.00,  543.00,  596.00,  654.00,  717.00,  785.00,  858.00,
     &  937.00,  1020.0,  1110.0,  1200.0,  1300.0,  1410.0,  1530.0,
     &  1650.0,  1780.0,  1920.0,  2070.0,  2230.0,  2400.0,  2590.0,
     &  3190.0,  3910.0,  4770.0,  5790.0,  6990.0,  8400.0,  10000.,
     &  11900.,  14100.,  16600.,  19500.,  22800.,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
     &  0.0000,  0.0000,  0.0000,  0.0000,  0.0000 /
 
C     Set up common block for taskname - necessary for fcerr.
      character(40) taskname
      common/tasknm/taskname
      taskname = 'bod2rmf'
 
C     Read in name of input file containing detector-specific response matrix data.
      status = 0
      call uclgst ('inrfil', inrfil, status)
      if (status.ne.0) then
         msg = 'Problem getting name of input file containing'
         call fcerr (msg)
         msg = 'detector-specific response matrix data!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
      inrfil = inrfil(1:len_trim(inrfil)) // '.fits'
 
      call pre_cont_matrix (inrfil)
 
C     Read in BATSE LAD number.
      call uclgsi ('ladnum', det, status)
      if (status.ne.0) then
         msg = 'Problem getting BATSE LAD number!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

C     Read in VP number (string).
      call uclgsi ('viewper', vpnum, status)
      if (status.ne.0) then
         msg = 'Problem getting viewper number!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

C     Read in r.a. of source (degrees).
      call uclgsr ('sradeg', sradeg, status)
      if (status.ne.0) then
         msg = 'Problem getting r.a. of source!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

C     Read in dec. of source (degrees).
      call uclgsr ('sdecdg', sdecdg, status)
      if (status.ne.0) then
         msg = 'Problem getting dec. of source!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

      if (vpnum .eq. 0) then
C     Read in x-ra, x-dec, z-ra, z-dec, tjd-start and tjd-stop
C     Read in r.a. of CGRO z-axis (degrees).
         call uclgsr ('zradeg', zradeg, status)
         if (status.ne.0) then
            msg = 'Problem getting r.a. of CGRO z-axis!'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif

C     Read in dec. of CGRO z-axis (degrees).
         call uclgsr ('zdecdg', zdecdg, status)
         if (status.ne.0) then
            msg = 'Problem getting dec. of CGRO z-axis!'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif

C     Read in r.a. of CGRO x-axis (degrees).
         call uclgsr ('xradeg', xradeg, status)
         if (status.ne.0) then
            msg = 'Problem getting r.a. of CGRO x-axis!'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif

C     Read in dec. of CGRO x-axis (degrees).
         call uclgsr ('xdecdg', xdecdg, status)
         if (status.ne.0) then
            msg = 'Problem getting dec. of CGRO x-axis!'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif

C     Read in TJD (start) of observation
         call uclgsr ('tjd_start', tjd_start, status)
         if (status.ne.0) then
            msg = 'Problem getting obs tjd_start'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif

C     Read in TJD (stop) of observation
         call uclgsr ('tjd_stop', tjd_stop, status)
         if (status.ne.0) then
            msg = 'Problem getting obs tjd_stop'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif
      else
c         msg = '**************WARNING**********************************'
c        call fcerr (msg)
c        msg = 'Spacecraft orientation angles suspect in vp_list.fits'
c        call fcerr (msg)
c        msg = 'Use of this option not recommended!'
c        call fcerr (msg)
c        msg = '**************WARNING**********************************'
         call fcerr (msg)
c        call exit (1)
                
C     Read in name of data directory.
         status = 0
         call uclgst ('datadir', data_dir, status)
         if (status.ne.0) then
            msg = 'Problem getting name of data directory!'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif

	 call readtable(data_dir, vpnum, tjd_start, tjd_stop, 
     *        xradeg, xdecdg, zradeg, zdecdg, 1, status)
c

         if (status.ne.0) then
            msg = 'Aborting program!'
            call fcerr (msg)
            return
         endif
      endif

C     Read in name of output directory.
      status = 0
      call uclgst ('outputdir', output_dir, status)
      if (status.ne.0) then
         msg = 'Problem getting name of output directory!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         return
      endif

C     Read in name of output RMF file.
      status = 0
      call uclgst ('rmffil', rmffil, status)
      if (status.ne.0) then
         msg = 'Problem getting name of output RMF file!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
      rmffil = rmffil(1:len_trim(rmffil)) // '.rmf'

C     Read in clobber flag.
      status = 0
      call uclgsi ('clobber', clobber, status)
      if (status.ne.0) then
         msg = 'Problem getting clobber flag!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

Csb Create full name (including path name) of the output RMF file
      rmffil = output_dir(1:len_trim(output_dir)) // rmffil

C     Check to see whether the output file already exists
      call FileExists(rmffil, clobber, status)
      if (status .ne. 0) return
      
c     get detector channel energy boundaries
      
      max_recs = 8
      call get_chan_edges (det, tjd_start, tjd_stop, chan_edges)

C     Convert coordinates to double precision.
      sra_deg = dble (sradeg)
      sdec_deg = dble (sdecdg)
      xra_deg = dble (xradeg)
      xdec_deg = dble (xdecdg)
      zra_deg = dble (zradeg)
      zdec_deg = dble (zdecdg)
 
C     Get spacecraft coordinates.
      call j2000_to_gro (sra_deg, sdec_deg, xra_deg, xdec_deg, zra_deg,
     &     zdec_deg, elev_deg, azimuth_deg)
      eelev_deg = - elev_deg
      if (azimuth_deg.lt.180.0) then
         eazimuth_deg = azimuth_deg + 180.0
      else
         eazimuth_deg = azimuth_deg - 180.0
      end if
c
c


C     Modify Earth position so that it is not exactly 180 degrees from
C     source position.
      if (eelev_deg.ge.0.0) then
         eelev_deg = eelev_deg - 5.0
      else
         eelev_deg = eelev_deg + 5.0
      endif

      idet = det
      nchan = nc_edges - 1
      nebin = ne_edges - 1
      nmaxebin = 129
      call response_matrix (azimuth_deg, elev_deg, eazimuth_deg, 
     *     eelev_deg, idet, mat_vers, e_edges, ne_edges, nebin,
     *     chan_edges, nc_edges, nchan, nmaxebin, direct_matrix)
      call apply_attenuation_correction(det, elev_deg, azimuth_deg, 
     &     e_edges,ne_edges,direct_matrix, nmaxebin,nchan) !cawh 2004-03-10
      call rsp_write2 (rmffil, det, chan_edges, nc_edges-1, e_edges,
     &     ne_edges-1, direct_matrix)




      return 
      end


C*******************************************************************************
C SUBROUTINE: pre_cont_matrix
C
C DESCRIPTION: Routine that oversees reading in of response matrix data.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 06/93
C
C NOTES:
C
C ARGUMENTS:
C     inrfil - name of input FITS file containing detector-specific response
C              matrix data
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine u_loader          - loads detector normals in CGRO coordinates
C     subroutine load_correct_data - loads corrections to current LAD matrices
C     subroutine integrator_reader - reads in LAD matrix data
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine pre_cont_matrix (inrfil)
 
      implicit none
 
C Spectral index for interpolation.
      real*4 ALFA,
C Input (source) energy bin edges.
     &  EBIN_EDGE_IN(276),
C Input (source) energy bin edges.
     &  EBIN_EDGE_OUT(276),
     &  FMATLOW(4,4,4), FMATMED(4,16,16), FMATHIGH(4,275,275)
 
      integer*4 IDET,
C Array size: 4x4, 16x16 or 275x275.
     &  IMATRIX,
C Number of bins along a matrix row or column.
     &  NOBINS_IN,
C Number of bins along a matrix row or column.
     &  NOBINS_OUT
 
      character*(*) inrfil
 
      COMMON/NEWMAT/NOBINS_IN, EBIN_EDGE_IN, NOBINS_OUT, EBIN_EDGE_OUT,
     &  FMATLOW, FMATMED, FMATHIGH, ALFA
      COMMON/INDPAS/IDET, IMATRIX
 
C Set up common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common/tasknm/taskname
      taskname = 'bod2rmf'
 
      nobins_in = 16
      imatrix = 2
 
      call u_loader
      call load_correct_data
 
      msg = 'Starting to read the input file inst_resp.fits....'
      call fcerr (msg)
 
      call integrator_reader (inrfil)
 
      msg = '...finished reading the input file inst_resp.fits.'
      call fcerr (msg)
 
      return
      end


C*******************************************************************************
C SUBROUTINE: response_matrix
C
C DESCRIPTION: Routine that produces response matrices for LAD data.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 04/93
C
C NOTES:
C
C ARGUMENTS:
C     srce_az             - source azimuth in degrees in CGRO coords (r*4)
C     srce_el             - source elevation in degrees in CGRO coords (r*4)
C     geo_az              - geocenter azimuth in degrees in CGRO coords (r*4)
C     geo_el              - geocenter elevation in degrees in CGRO coords (r*4)
C     detector            - BATSE LAD number (0 to 7) (i*2)
C     mat_vers            - integrator subroutine version number (c*11)
C     nebin               - number of matrix input bins (i*2)
C     e_edges(nebin+1)    - input bin edges: keV (r*4)
C     nchan               - number of matrix output bins (i*2)
C     chan_edges(nchan+1) - output bin edges keV (r*4)
C     nmaxebin            - size of external array's first dimension used for
C                           first dimension of variable matrix arrays (i*2)
C     direct_matrix(nmaxebin,nchan)    - variable matrix for direct matrix
C                                        transfer (r*4)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine get_directions    - converts source and geocenter angles to
C                                    direction vectors in CGRO coordinates
C     subroutine some_angle_stuff  - angular transforms and scattering corrections
C     subroutine direct_angle      - puts direction vectors in CGRO coordinates
C     subroutine integrator_atscat - integrates differential matrices
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine response_matrix (srce_az, srce_el, geo_az, geo_el,
     &                            detector, mat_vers, e_edges, ne_edges,
     &                            nebin, chan_edges, nc_edges, nchan,
     &                            nmaxebin, direct_matrix)
 
      character(11) mat_vers
 
      integer*4 i, IDET, IMATRIX, i1, i2,
C Number of bins along a matrix row or column.
     &     NOBINS_IN,
C Number of bins along a matrix row or column.
     &     NOBINS_OUT
 
      integer*2 detector, nebin, nchan, nmaxebin
 
      real*4 srce_az, srce_el, geo_az, geo_el,
C Spectral index for interpolation.
     &     ALFA,
C Input (source) energy bin edges.
     &     EBIN_EDGE_IN(276),
C Input (source) energy bin edges.
     &     EBIN_EDGE_OUT(276),
     &     FMATLOW(4,4,4), FMATMED(4,16,16), FMATHIGH(4,275,275),
     &     FMATLOWEVAL(4,4), FMATMEDEVAL(16,16), FMATHIGHEVAL(275,275),
C Direction to source in CGRO coordinates.
     &     phibin,
C Direction to geocenter in CGRO coordinates.
     &     phigin,
     &     thetbin, thetgin
 
C Put in variable dimension matrix definitions here.
      dimension e_edges(ne_edges)
      dimension chan_edges(nc_edges)
      dimension direct_matrix(nmaxebin,nchan)
 
      COMMON/NEWMAT/NOBINS_IN, EBIN_EDGE_IN, NOBINS_OUT, EBIN_EDGE_OUT,
     &     FMATLOW, FMATMED, FMATHIGH, ALFA
      COMMON/INDPAS/IDET, IMATRIX
      COMMON/MATEVL/FMATLOWEVAL, FMATMEDEVAL, FMATHIGHEVAL
 
      thetbin = srce_el
      phibin = srce_az
      thetgin = geo_el
      phigin = geo_az
      nobins_out = nchan
      nobins_in = nebin
      imatrix = 3
      IDET = detector
      mat_vers = 'version_2.1'
 
      DO I = 1, NOBINS_IN+1
         EBIN_EDGE_IN(I) = e_edges(I)
      END DO
 
      DO I = 1, NOBINS_OUT+1
         EBIN_EDGE_OUT(I) = chan_edges(I)
      END DO
 
      call get_directions (thetbin, phibin, thetgin, phigin)
      call some_angle_stuff
      call direct_angle
      call integrator_atscat
 
      do i1 = 1, NOBINS_IN
         do i2 = 1, NOBINS_OUT
            direct_matrix(i1,i2) = FMATHIGHEVAL(i1,i2)
         end do
      end do
      
      return
      end
 


C*******************************************************************************
C SUBROUTINE: some_angle_stuff
C
C DESCRIPTION: Routine that does angular transforms and calculates
C     scattering corrections.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS: None.
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************

      subroutine some_angle_stuff
 
      implicit none
 
      real*4 GEOX, GEOY, GEOZ, BURSTX, BURSTY, BURSTZ, BXGX, BXGY, BXGZ,
     &     BXGXGX, BXGXGY, BXGXGZ, BXGLEN, BXGXGLEN
 
      COMMON/DIRCOS/GEOX, GEOY, GEOZ, BURSTX, BURSTY, BURSTZ, BXGX,
     &     BXGY, BXGZ, BXGXGX, BXGXGY, BXGXGZ
 
      BXGX = BURSTY * GEOZ - BURSTZ * GEOY
      BXGY = BURSTZ * GEOX - BURSTX * GEOZ
      BXGZ = BURSTX * GEOY - BURSTY * GEOX
      BXGLEN = SQRT (BXGX**2 + BXGY**2 + BXGZ**2)
      BXGX = BXGX / BXGLEN
      BXGY = BXGY / BXGLEN
      BXGZ = BXGZ / BXGLEN
      BXGXGX = BXGY * GEOZ - BXGZ * GEOY
      BXGXGY = BXGZ * GEOX - BXGX * GEOZ
      BXGXGZ = BXGX * GEOY - BXGY * GEOX
      BXGXGLEN = SQRT (BXGXGX**2 + BXGXGY**2 + BXGXGZ**2)
      BXGXGX = BXGXGX / BXGXGLEN
      BXGXGY = BXGXGY / BXGXGLEN
      BXGXGZ = BXGXGZ / BXGXGLEN
 
      RETURN
      END
 

C*******************************************************************************
C SUBROUTINE: direct_angle
C
C DESCRIPTION: Routine that puts direction vectors in CGRO coordinates.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 1993?
C
C NOTES:
C
C ARGUMENTS: None.
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine direct_angle
 
      COMMON/DIRCOS/GEOX, GEOY, GEOZ, BURSTX, BURSTY, BURSTZ, BXGX,
     &     BXGY, BXGZ, BXGXGX, BXGXGY, BXGXGZ
      COMMON/INDPAS/IDET, IMATRIX
      COMMON/DETGRO/UD(3,3,0:7)
      COMMON/MATAVL/ADETBST

      ADETBST = 360.0 / (2.0 * 3.1415926) * ACOS (BURSTX * UD(1,1,IDET)
     &     + BURSTY * UD(1,2,IDET) + BURSTZ * UD(1,3,IDET))

      RETURN
      END
 


C*******************************************************************************
C SUBROUTINE: j2000_to_gro
C
C DESCRIPTION: Routine that converts r.a. and dec. of source to CGRO
C     coordinates using orientation of CGRO axes in ra. and dec.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS:
C     obj_ra        - source r.a. in degrees (r*8)
C     obj_dec       - source dec. in degrees (r*8)
C     gro_x_ra_deg  - r.a. of CGRO x-axis in degrees (r*8)
C     gro_x_dec_deg - dec. of CGRO x-axis in degrees (r*8)
C     gro_z_ra_deg  - r.a. of CGRO z-axis in degrees (r*8)
C     gro_z_dec_deg - dec. of CGRO z-axis in degrees (r*8)
C     theta         - source elevation in degrees in CGRO coords (r*4)
C     phi           - source azimuth in degrees in CGRO coords (r*4)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine gro_to_equatorial - calculates Cartesian unit vectors in
C                                    equatorial coords in direction of CGRO axes
C     subroutine transpose_matrix  - transposes matrix a to matrix b
C     subroutine transform         - transforms source r.a. and dec. to source
C                                    altitude and azimuth in CGRO coordinates
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine j2000_to_gro (obj_ra, obj_dec, gro_x_ra_deg,
     *     gro_x_dec_deg, gro_z_ra_deg,gro_z_dec_deg, theta, phi)
 
      real*8 equat_to_gro(3,3), gro_to_equat(3,3), gro_x_dec_rad,
     &     gro_x_ra_rad, gro_z_dec_rad, gro_z_ra_rad, gro_x_dec_deg,
     &     gro_x_ra_deg, gro_z_dec_deg, gro_z_ra_deg, pi,
     &     srce_az_el_gro(2), srce_az_el_gro_deg(2), obj_ra,
     &     srce_ra_dec_earth(2), obj_dec
 
      real*4 theta, phi
 
      pi = datan (1.0d0) * 4.0d0
 
      gro_x_dec_rad = gro_x_dec_deg * (pi / 180.0d0)
      gro_x_ra_rad = gro_x_ra_deg * (pi / 180.0d0)
      gro_z_dec_rad = gro_z_dec_deg * (pi / 180.0d0)
      gro_z_ra_rad = gro_z_ra_deg * (pi / 180.0d0)
      srce_ra_dec_earth(1) = obj_ra * (pi / 180.0d0)
      srce_ra_dec_earth(2) = obj_dec * (pi / 180.0d0)
 
      call gro_to_equatorial (gro_z_ra_rad, gro_z_dec_rad, gro_x_ra_rad, 
     &     gro_x_dec_rad, gro_to_equat)
      call transpose_matrix (gro_to_equat, equat_to_gro)
      call transform (srce_ra_dec_earth, srce_az_el_gro, equat_to_gro)

      srce_az_el_gro_deg(1) = srce_az_el_gro(1) * (180.0 / pi)
      srce_az_el_gro_deg(2) = srce_az_el_gro(2) * (180.0 / pi)
      theta = srce_az_el_gro_deg(2)
      phi = srce_az_el_gro_deg(1)
 
      return
      end
 

C*******************************************************************************
C SUBROUTINE: integrator_reader
C
C DESCRIPTION: Routine that reads in LAD matrix data.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS:
C     inrfil - name of input FITS file containing detector-specific response
C              matrix data
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine read_inst_resp - reads in inst_resp.fits, which contains
C                                 detector-specific response matrix data
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************

      subroutine integrator_reader (inrfil)

      real*4 edif_edge(65), ein(275), diff_matrix(64,4,64,8)
 
      character*(*) inrfil
 
      common/difmat/edif_edge, diff_matrix, ein
 
      call read_inst_resp (inrfil, edif_edge, ein, diff_matrix)
 
      return
      end
 


C*******************************************************************************
C SUBROUTINE: transform
C
C DESCRIPTION: Routine that transforms source r.a. and dec. to source
C     altitude and azimuth in CGRO coordinates.
C
C AUTHOR/DATE: Martin Brock and G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS:
C     u - source r.a. and dec. in radians (r*8)
C     v - source azimuth and altitude in radians in CGRO coordinates (r*8)
C     t - transformation matrix from Earth to CGRO coordinates (r*8)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine unit_vector - transforms spherical coordinates to
C                              Cartesian unit vector
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine transform (u, v, t)
 
C Source r.a. and dec. (before transformation).
      real*8 u(2)
C Source azimuth and altitude in CGRO coordinates (after transformation).
      real*8 v(2)
C Transformation matrix from Earth to CGRO coordinates.
      real*8 t(3,3)
 
C Cartesian coordinates of unit vector.
      real*8 unit(3)
      real*8 x, y, z
      real*8 pi, pi_over_2
 
      pi = datan (1.0d0) * 4.0d0
      pi_over_2 = pi / 2.0d0
 
C Compute components of Cartesian unit vector in u direction.
      call unit_vector (u, unit)
 
C Compute cosine of angle between u direction and z-axis.
      z = t(1,3) * unit(1) + t(2,3) * unit(2) + t(3,3) * unit(3)
      if (z.ge.1) then
         v(1) = 0
         v(2) = pi_over_2
         return
      else if (z.le.-1) then
         v(1) = 0
         v(2) = - pi_over_2
         return
      end if
      v(2) = dasin (z)
 
      x = t(1,1) * unit(1) + t(2,1) * unit(2) + t(3,1) * unit(3)
      y = t(1,2) * unit(1) + t(2,2) * unit(2) + t(3,2) * unit(3)
      v(1) = datan2 (y, x)
 
      return
      end
 


C*******************************************************************************
C SUBROUTINE: get_directions
C
C DESCRIPTION: Routine that converts source and geocenter angles to
C     direction vectors in CGRO coordinates.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS:
C     thetbin - source elevation in degrees in CGRO coords (r*4)
C     phibin  - source azimuth in degrees in CGRO coords (r*4)
C     thetgin - geocenter elevation in degrees in CGRO coords (r*4)
C     phigin  - geocenter azimuth in degrees in CGRO coords (r*4)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine get_directions (thetbin, phibin, thetgin, phigin)
 
      implicit none
 
      real*4 thetbin, phibin, thetgin, phigin
      real*4 GEOX, GEOY, GEOZ, BURSTX, BURSTY, BURSTZ, BXGX, BXGY
      real*4 BXGZ, BXGXGX, BXGXGY, BXGXGZ, UD(3,3,0:7), ADETBST
      real*4 DTGANG(8), DTBANG(8)
 
      integer*4 IDET, IMATRIX, IDT(8)
 
      COMMON/DIRCOS/GEOX, GEOY, GEOZ, BURSTX, BURSTY, BURSTZ,
     &     BXGX, BXGY, BXGZ, BXGXGX, BXGXGY, BXGXGZ
      COMMON/INDPAS/IDET, IMATRIX
      COMMON/DETGRO/UD
      COMMON/MATAVL/ADETBST
 
      DATA IDT / 0, 1, 2, 3, 4, 5, 6, 7 /
 
      integer*4 i
 
      real*4 Thetab, Thetag, PHIB, PHIG, THETABR, THETAGR, PHIBR, PHIGR
 
C Set up common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common/tasknm/taskname
      taskname = 'bod2rmf'
 
      THETAB = 90.0 - thetbin
      THETAG = 90.0 - thetgin
      PHIB = phibin
      PHIG = phigin
      THETABR = THETAB * 2.0 * 3.1415926 / 360.0
      THETAGR = THETAG * 2.0 * 3.1415926 / 360.0
      PHIBR = PHIB * 2.0 * 3.1415926 / 360.0
      PHIGR = PHIG * 2.0 * 3.1415926 / 360.0
      GEOX = SIN (THETAGR) * COS (PHIGR)
      GEOY = SIN (THETAGR) * SIN (PHIGR)
      GEOZ = COS (THETAGR)
      BURSTX = SIN (THETABR) * COS (PHIBR)
      BURSTY = SIN (THETABR) * SIN (PHIBR)
      BURSTZ = COS (THETABR)
      ADETBST = 360.0 / (2.0 * 3.1415926) * ACOS (BURSTX * UD(1,1,IDET)
     &     + BURSTY * UD(1,2,IDET) + BURSTZ * UD(1,3,IDET))
 
      write (msg, 10) thetbin, phibin
10    format ('THETBIN =',f10.5,'    PHIBIN =',f11.5)
      call fcerr (msg)
      write (msg, 20) thetgin, phigin
20    format ('THETGIN =',f10.5,'    PHIGIN =',f11.5)
      call fcerr (msg)

      DO I = 1, 8
         DTBANG(I) = 360.0 / (2.0 * 3.1415926) * ACOS (BURSTX *
     &        UD(1,1,IDT(I)) + BURSTY * UD(1,2,IDT(I)) +
     &        BURSTZ * UD(1,3,IDT(I)))
         DTGANG(I) = 360.0 / (2.0 * 3.1415926) * ACOS (GEOX *
     &        UD(1,1,IDT(I)) + GEOY * UD(1,2,IDT(I)) +
     &        GEOZ * UD(1,3,IDT(I)))
         write (msg, 30) IDT(I), DTBANG(I), DTGANG(I)
 30      format ('IDET =',i3,'    DTB =',f10.4,'    DTG =',f10.4)
         call fcerr (msg)
      END DO
 
      RETURN
      END
 

C*******************************************************************************
C SUBROUTINE: gro_to_equatorial
C
C DESCRIPTION: Routine that calculates Cartesian unit vectors in
C     equatorial coordinates in direction of CGRO axes.
C
C AUTHOR/DATE: Martin Brock and G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS:
C     z_ra         - r.a. of CGRO z-axis in radians (r*8)
C     z_dec        - dec. of CGRO z-axis in radians (r*8)
C     x_ra         - r.a. of CGRO x-axis in radians (r*8)
C     x_dec        - dec. of CGRO x-axis in radians (r*8)
C     gro_to_equat - Cartesian unit vectors in equatorial coords
C                    in direction of CGRO axes (r*8)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine unit_vector - transforms spherical coordinates to
C                              Cartesian unit vector
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine gro_to_equatorial (z_ra, z_dec, x_ra, x_dec,
     &                              gro_to_equat)
 
C The orientation of CGRO z-axis and x-axis in r.a. and dec. (radians).
      real*8 z_ra, z_dec, x_ra, x_dec
C Cartesian unit vectors in equatorial coords in direction of CGRO axes.
      real*8 gro_to_equat(3,3)
C The orientation of CGRO x-axis and z-axis in r.a. and dec. (radians).
      real*8 sphere_x(2), sphere_z(2)
C Cartesian unit vectors in equatorial coords in direction of CGRO axes.
      real*8 gro_x(3), gro_y(3), gro_z(3)

C Find Cartesian unit vector in equatorial coords in direction of CGRO z-axis.
      sphere_z(1) = z_ra
      sphere_z(2) = z_dec
      call unit_vector (sphere_z, gro_z)

C Find Cartesian unit vector in equatorial coords in direction of CGRO x-axis.
      sphere_x(1) = x_ra
      sphere_x(2) = x_dec
      call unit_vector (sphere_x, gro_x)

C Find Cartesian unit vector in equatorial coords in direction of CGRO y-axis.
      gro_y(1) = gro_z(2) * gro_x(3) - gro_z(3) * gro_x(2)
      gro_y(2) = gro_z(3) * gro_x(1) - gro_z(1) * gro_x(3)
      gro_y(3) = gro_z(1) * gro_x(2) - gro_z(2) * gro_x(1)

      gro_to_equat(1,1) = gro_x(1)
      gro_to_equat(2,1) = gro_y(1)
      gro_to_equat(3,1) = gro_z(1)

      gro_to_equat(1,2) = gro_x(2)
      gro_to_equat(2,2) = gro_y(2)
      gro_to_equat(3,2) = gro_z(2)

      gro_to_equat(1,3) = gro_x(3)
      gro_to_equat(2,3) = gro_y(3)
      gro_to_equat(3,3) = gro_z(3)
 
      return
      end
 

C*******************************************************************************
C SUBROUTINE: u_loader
C
C DESCRIPTION: Routine that loads detector normals in CGRO coordinates.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 1993?
C
C NOTES:
C
C ARGUMENTS: None.
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine u_loader
 
      COMMON/DETGRO/UD(3,3,0:7)
 
C Detector 0.
      UD(1,1,0) = 1.0 / SQRT (3.0)
      UD(1,2,0) = 1.0 / SQRT (3.0)
      UD(1,3,0) = 1.0 / SQRT (3.0)
      UD(2,1,0) = - 1.0 / SQRT (2.0)
      UD(2,2,0) = 1.0 / SQRT (2.0)
      UD(2,3,0) = 0.0
      UD(3,1,0) = - 1.0 / SQRT (6.0)
      UD(3,2,0) = - 1.0 / SQRT (6.0)
      UD(3,3,0) = SQRT (2.0) / SQRT (3.0)

C Detector 2.
      UD(1,1,2) = 1.0 / SQRT (3.0)
      UD(1,2,2) = - 1.0 / SQRT (3.0)
      UD(1,3,2) = 1.0 / SQRT (3.0)
      UD(2,1,2) = 1.0 / SQRT (2.0)
      UD(2,2,2) = 1.0 / SQRT (2.0)
      UD(2,3,2) = 0.0
      UD(3,1,2) = - 1.0 / SQRT (6.0)
      UD(3,2,2) = 1.0 / SQRT (6.0)
      UD(3,3,2) = SQRT (2.0) / SQRT (3.0)

C Detector 4.
      UD(1,1,4) = - 1.0 / SQRT (3.0)
      UD(1,2,4) = 1.0 / SQRT (3.0)
      UD(1,3,4) = 1.0 / SQRT (3.0)
      UD(2,1,4) = - 1.0 / SQRT (2.0)
      UD(2,2,4) = - 1.0 / SQRT (2.0)
      UD(2,3,4) = 0.0
      UD(3,1,4) = 1.0 / SQRT (6.0)
      UD(3,2,4) = - 1.0 / SQRT (6.0)
      UD(3,3,4) = SQRT (2.0) / SQRT (3.0)

C Detector 6.
      UD(1,1,6) = - 1.0 / SQRT (3.0)
      UD(1,2,6) = - 1.0 / SQRT (3.0)
      UD(1,3,6) = 1.0 / SQRT (3.0)
      UD(2,1,6) = 1.0 / SQRT (2.0)
      UD(2,2,6) = - 1.0 / SQRT (2.0)
      UD(2,3,6) =  0.0
      UD(3,1,6) = 1.0 / SQRT (6.0)
      UD(3,2,6) = 1.0 / SQRT (6.0)
      UD(3,3,6) = SQRT (2.0) / SQRT (3.0)
 
      DO IDET = 1, 3
         DO IGRO = 1, 3
            IF (IGRO.LT.3) UD(IDET,IGRO,1) = UD(IDET,IGRO,0)
            IF (IGRO.LT.3) UD(IDET,IGRO,3) = UD(IDET,IGRO,2)
            IF (IGRO.LT.3) UD(IDET,IGRO,5) = UD(IDET,IGRO,4)
            IF (IGRO.LT.3) UD(IDET,IGRO,7) = UD(IDET,IGRO,6)
            IF (IGRO.EQ.3) UD(IDET,IGRO,1) = - UD(IDET,IGRO,0)
            IF (IGRO.EQ.3) UD(IDET,IGRO,3) = - UD(IDET,IGRO,2)
            IF (IGRO.EQ.3) UD(IDET,IGRO,5) = - UD(IDET,IGRO,4)
            IF (IGRO.EQ.3) UD(IDET,IGRO,7) = - UD(IDET,IGRO,6)
         ENDDO
      ENDDO

      RETURN
      END
 

C*******************************************************************************
C SUBROUTINE: load_correct_data
C
C DESCRIPTION: Routine that loads corrections to current LAD matrices.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 1993?
C
C NOTES:
C
C ARGUMENTS: None.
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine load_correct_data
 
      dimension v1(12), v2(12), v3(12), v4(12), v5(12), v6(12),
     &     v7(12), v8(12), v9(12), v10(12), v11(12), v12(12), v13(12),
     &     v14(12), v15(12)
 
      COMMON/CORMAT/CMAT(15,12), ECOR(15), ACOR(12)
 
      data ECOR / 23.0, 32.0, 42.0, 56.0, 75.0, 98.0, 127.0, 170.0,
     &     236.0, 322.0, 438.0, 585.0, 793.0, 1191.0, 1669.0 /
      data ACOR / 0.0, 15.0, 30.0, 45.0, 60.0, 65.0, 70.0, 75.0,
     &     80.0, 85.0, 90.0, 95.0 /
      data v1 / 1.10, 1.03, 0.94, 0.88, 1.76, 1.04, 0.90, 0.43,
     &     0.155, 0.269, -0.02, -0.02 /
      data v2 / 1.06, 1.03, 0.95, 0.98, 1.105, 0.87, 0.75, 0.55,
     &     0.36, 0.30, -0.11, -0.11 /
      data v3 / 1.043, 1.014, 0.982, 0.976, 1.075, 0.87, 0.75, 0.62,
     &     0.45, 0.37, 0.80, 0.80 /
      data v4 / 1.033, 1.015, 0.978, 0.995, 1.048, 0.87, 0.75, 0.62,
     &     0.49, 0.42, 0.36, 0.36 /
      data v5 / 1.032, 1.013, 0.981, 0.998, 1.034, 0.87, 0.75, 0.62,
     &     0.50, 0.43, 0.37, 0.37 /
      data v6 / 1.023, 1.013, 0.989, 0.992, 1.042, 0.88, 0.75, 0.62,
     &     0.52, 0.47, 0.45, 0.45 /
      data v7 / 1.026, 1.015, 0.981, 1.0, 1.037, 0.88, 0.76, 0.64,
     &     0.56, 0.53, 0.51, 0.992 /
      data v8 / 1.013, 1.013, 0.988, 1.011, 1.019, 0.92, 0.80, 0.71,
     &     0.66, 0.63, 0.61, 0.967 /
      data v9 / 1.01, 1.01, 0.99, 1.00, 1.03, 0.94, 0.89, 0.83,
     &     0.76, 0.73, 0.75, 0.967 /
      data v10 / 1.01, 1.01, 1.0, 1.0, 1.02, 0.96, 0.94, 0.92,
     &     0.90, 0.87, 0.85, 0.892 /
      data v11 / 0.99, 1.0, 1.02, 1.01, 1.0, 1.0, 1.0, 1.0,
     &     1.0, 1.0, 1.0, 0.857 /
      data v12 / 0.982, 1.002, 1.002, 1.036, 0.981, 1.0, 1.0, 1.0,
     &     1.0, 1.0, 1.0, 0.796 /
      data v13 / 0.94, 1.02, 1.0, 1.0, 0.998, 1.0, 1.0, 1.0,
     &     1.0, 1.0, 1.0, 1.119/
      data v14 / 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &     1.0, 1.0, 1.0, 1.15 /
      data v15 / 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &     1.0, 1.0, 1.0, 1.07 /
 
      DO J = 1, 12
         CMAT(1,J) = V1(J)
         CMAT(2,J) = V2(J)
         CMAT(3,J) = V3(J)
         CMAT(4,J) = V4(J)
         CMAT(5,J) = V5(J)
         CMAT(6,J) = V6(J)
         CMAT(7,J) = V7(J)
         CMAT(8,J) = V8(J)
         CMAT(9,J) = V9(J)
         CMAT(10,J) = V10(J)
         CMAT(11,J) = V11(J)
         CMAT(12,J) = V12(J)
         CMAT(13,J) = V13(J)
         CMAT(14,J) = V14(J)
         CMAT(15,J) = V15(J)
      ENDDO
 
      RETURN
      END
 

C*******************************************************************************
C SUBROUTINE: integrator_atscat
C
C DESCRIPTION: Routine that integrates differential matrices.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS: None.
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************

      subroutine integrator_atscat

      COMMON/NEWMAT/NOBINS_IN, EBIN_EDGE_IN(276), NOBINS_OUT,
     &     EBIN_EDGE_OUT(276), FMATLOW(4,4,4), FMATMED(4,16,16),
     &     FMATHIGH(4,275,275), ALFA
      COMMON/MATAVL/ADETBST

C First get ALFA-less ATSCAT matrix for scattered input rows.
      ALFA_OLD = ALFA
      ALFA = 0.0

      call newmatrix

      ALFA = ALFA_OLD

C By this point we have matrix bin edges in energy units EEDGE(275).
C Call integrator with appropriate information to make integrated matrix.
      call newmatrix

      angle = ADETBST

      call evaluate_matrix (angle)

      RETURN
      END
 


C*******************************************************************************
C SUBROUTINE: evaluate_matrix
C
C DESCRIPTION: Routine that evaluates matrix at specific angle to normal.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 1993?
C
C NOTES:
C
C ARGUMENTS: None.
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************

      subroutine evaluate_matrix (angle)

      COMMON/INDPAS/IDET, IMATRIX
      COMMON/CORMAT/CMAT(15,12), ECOR(15), ACOR(12)
      COMMON/DIFMAT/EDIF_EDGE(65), DIFF_MATRIX(64,4,64,8), EIN(275)
      COMMON/NEWMAT/NOBINS_IN, EBIN_EDGE_IN(276), NOBINS_OUT,
     &     EBIN_EDGE_OUT(276), FMATLOW(4,4,4), FMATMED(4,16,16),
     &     FMATHIGH(4,275,275), ALFA
      COMMON/MATEVL/FMATLOWEVAL(4,4), FMATMEDEVAL(16,16),
     &     FMATHIGHEVAL(275,275)
      
      REAL ANGLE_BACK(13)
      DATA ANGLE_BACK / 90.0, 95.0, 100.0, 105.0, 110.0, 115.0,
     &     120.0, 125.0, 130.0, 135.0, 150.0, 165.0, 180.0 /

      REAL RESPONSE_BACK1(13,8)
      DATA RESPONSE_BACK1 / -0.02, -0.0175, -0.0166, -0.0144,
     &  -0.006, -0.0034, -0.003, -0.001, -0.0004, -0.0002, -0.00012,
     &  -0.00006, -0.00002, -0.11, -0.098, -0.096, -0.0803, -0.066,
     &  -0.0506, -0.0429, -0.0286, -0.0231, -0.0132, -0.0088, -0.0044,
     &  -0.001, 0.8, 0.744, 0.728, 0.592, 0.496, 0.384, 0.320, 0.232,
     &  0.184, 0.120, 0.08, 0.04, 0.008, 0.36, 0.338, 0.331, 0.270,
     &  0.226, 0.190, 0.147, 0.111, 0.0864, 0.0576, 0.0396, 0.018,
     &  0.0036, 0.37, 0.351, 0.325, 0.277, 0.236, 0.199, 0.159, 0.122,
     &  0.092, 0.070, 0.048, 0.022, 0.037, 0.45, 0.432, 0.405, 0.342,
     &  0.288, 0.247, 0.184, 0.157, 0.126, 0.094, 0.063, 0.031, 0.0045,
     &  0.992, 0.962, 0.942, 0.813, 0.744, 0.674, 0.595, 0.486, 0.347,
     &  0.248, 0.168, 0.0793, 0.0099, 0.967, 0.957, 0.947, 0.928, 0.918,
     &  0.908, 0.850, 0.686, 0.493, 0.251, 0.174, 0.087, 0.0967 /

      REAL RESPONSE_BACK2(13,7)
      DATA RESPONSE_BACK2 / 0.967, 0.937, 0.986, 1.121, 1.27,
     &  1.38, 1.30, 1.01, 0.620, 0.251, 0.174, 0.087, 0.0967, 0.892,
     &  0.900, 1.02, 1.186, 1.409, 1.569, 1.480, 1.168, 0.677, 0.240,
     &  0.169, 0.089, 0.00892, 0.857, 0.865, 0.994, 1.216, 1.45, 1.64,
     &  1.56, 1.23, 0.719, 0.239, 0.162, 0.0857, 0.00857, 0.796, 0.803,
     &  0.915, 1.146, 1.393, 1.552, 1.488, 1.154, 0.676, 0.230, 0.151,
     &  0.0796, .00796, 1.119, 1.152, 1.432, 1.734, 2.081, 2.349, 2.338,
     &  1.779, 1.007, 0.324, 0.223, 0.1119, 0.0111, 1.15, 1.54, 1.920,
     &  2.30, 2.49, 2.44, 1.96, 1.219, 0.356, 0.341, 0.126, 0.0115,
     &  0.00115, 1.07, 1.487, 1.829, 2.054, 2.364, 2.311, 1.883, 1.209,
     &  0.406, 0.267, 0.139, 0.0107, 0.00107 /
 
      REAL RESPONSE_BACK(13,15)

 
      DO I = 1, 15
         DO J = 1, 13
            IF (I.LE.8) RESPONSE_BACK(J,I) = RESPONSE_BACK1(J,I)
            IF (I.GT.8) RESPONSE_BACK(J,I) = RESPONSE_BACK2(J,I-8)
         ENDDO
      ENDDO

      IF (angle.LE.90.0) THEN
         AC2 = COS ((angle * 2.0 * 3.1415926 / 360.0)**2 + 0.2617)
      END IF

      IF (angle.GT.90.0) THEN
         AC2 = COS ((90.0 * 2.0 * 3.1415926 / 360.0)**2 + 0.2617)
      END IF

C Calculate angular dependence of weight factor for front facing detector.
      IF (angle.LT.ACOR(12)) THEN
         DO IAN = 1, 11
            IF (angle.GT.ACOR(IAN).AND.angle.LE.ACOR(IAN+1)) THEN
               IA_LOW = IAN
               AW_LOW = (ACOR(IAN+1) - angle) /
     &              (ACOR(IAN+1) - ACOR(IAN))
               IA_HIGH = IAN + 1
               AW_HIGH = (angle - ACOR(IAN)) /
     &              (ACOR(IAN+1) - ACOR(IAN))
            ENDIF
         ENDDO
      ENDIF

C Same for back facing detector.
      IF (angle.gt.ACOR(12)) THEN
         IF (angle.gt.ANGLE_BACK(13)) THEN
            IA_LOW = 12
            AW_LOW = 0
            IA_HIGH = 13
            AW_HIGH = 1
         ELSE
            I = 2
            DO WHILE (angle.GT.ANGLE_BACK(I))
               I = I + 1
            END DO
            IA_LOW = I - 1
            AW_LOW = (ANGLE_BACK(I+1) - angle) /
     &           (ANGLE_BACK(I+1) - ANGLE_BACK(I))
            IA_HIGH = I
            AW_HIGH = 1 - AW_LOW
         ENDIF
      ENDIF

C Calculate energy dependence of weight factor.
      DO IVV = 1, NOBINS_IN
         IF (IMATRIX.EQ.1.OR.IMATRIX.EQ.2) THEN !changed cawh-2004-03-04
            ECOR_CEN = (EBIN_EDGE_IN(IVV) + EBIN_EDGE_IN(IVV+1)) / 2.0

            DO IEN = 1, 14
               IF (ECOR_CEN.GT.ECOR(IEN).AND.
     &              ECOR_CEN.LE.ECOR(IEN+1)) THEN
                  IE_LOW = IEN
                  EW_LOW = (ECOR(IEN+1) - ECOR_CEN) /
     &                 (ECOR(IEN+1) - ECOR(IEN))
                  IE_HIGH = IEN + 1
                  EW_HIGH = (ECOR_CEN - ECOR(IEN)) /
     &                 (ECOR(IEN+1) - ECOR(IEN))
               ENDIF
            ENDDO

            IF (ECOR_CEN.LE.ECOR(1)) THEN
               IE_LOW = 1
               EW_LOW = 1.0
               IE_HIGH = 1
               EW_HIGH = 0.0
            ENDIF

            IF (ECOR_CEN.GT.ECOR(15)) THEN
               IE_LOW = 1
               EW_LOW = 0.0
               IE_HIGH = 15
               EW_HIGH = 1.0
            ENDIF

C Calculate correction factor.
            if (angle.lt.ACOR(12)) then
               CORFACT =
     &              CMAT(IE_LOW,IA_LOW) * EW_LOW * AW_LOW +
     &              CMAT(IE_LOW,IA_HIGH) * EW_LOW * AW_HIGH +
     &              CMAT(IE_HIGH,IA_LOW) * EW_HIGH * AW_LOW +
     &              CMAT(IE_HIGH,IA_HIGH) * EW_HIGH * AW_HIGH
            ENDIF

              if (angle.ge.ACOR(12)) then
                 CORFACT =
     &                RESPONSE_BACK(IA_LOW,IE_LOW) * EW_LOW * AW_LOW +
     &                RESPONSE_BACK(IA_LOW,IE_HIGH) * AW_LOW * EW_HIGH +
     &                RESPONSE_BACK(IA_HIGH,IE_LOW) * AW_HIGH * EW_LOW +
     &                RESPONSE_BACK(IA_HIGH,IE_HIGH) * EW_HIGH * AW_HIGH
              ENDIF
           ENDIF

           DO IHH = 1, NOBINS_OUT
              IF (IMATRIX.EQ.0) THEN
                 FMATLOWEVAL(IVV,IHH) = (FMATLOW(1,IVV,IHH) +
     &                FMATLOW(2,IVV,IHH) * AC2 +
     &                FMATLOW(3,IVV,IHH) * AC2**2 +
     &                FMATLOW(4,IVV,IHH) * AC2**3)
              ENDIF

              IF (IMATRIX.eq.1.or.IMATRIX.eq.2) THEN
                 FMATMEDEVAL(IVV,IHH) = (FMATMED(1,IVV,IHH) +
     &                FMATMED(2,IVV,IHH) * AC2 +
     &                FMATMED(3,IVV,IHH) * AC2**2 +
     &                FMATMED(4,IVV,IHH) * AC2**3) 
                 FMATMEDEVAL(IVV,IHH) = FMATMEDEVAL(IVV,IHH) * CORFACT
              ENDIF

              IF (IMATRIX.EQ.3) THEN
                 FMATHIGHEVAL(IVV,IHH) = (FMATHIGH(1,IVV,IHH) +
     &                FMATHIGH(2,IVV,IHH) * AC2 +
     &                FMATHIGH(3,IVV,IHH) * AC2**2 +
     &                FMATHIGH(4,IVV,IHH) * AC2**3)
              ENDIF

c              FMATHIGHEVAL(IVV,IHH) = FMATHIGHEVAL(IVV,IHH) * CORFACT

              IF (FMATHIGHEVAL(IVV,IHH).LT.0.0)
     &             FMATHIGHEVAL(IVV,IHH) = 0.0
              
          ENDDO
 
       ENDDO
 
       RETURN
       END



 

C*******************************************************************************
C SUBROUTINE: newmatrix
C
C DESCRIPTION: Routine that does energy rebinning of compressed matrix
C     elements into energy-binned matrix elements.
C
C AUTHOR/DATE: G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS: None.
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************

      subroutine newmatrix

      REAL VCHUNK, EDIF_EDGEH(65), EDIF_CENT(64), ROW_TOT(276),
     &     BINNED_MATRIX(4,275,275), DIFF_MATRIX_VEC(4,64,8)
 
      COMMON/DIFMAT/EDIF_EDGE(65), DIFF_MATRIX(64,4,64,8), EIN(275)
      COMMON/NEWMAT/NOBINS_IN, EBIN_EDGE_IN(276), NOBINS_OUT,
     &     EBIN_EDGE_OUT(276), FMATLOW(4,4,4), FMATMED(4,16,16),
     &     FMATHIGH(4,275,275), ALFA
      COMMON/INDPAS/IDET, IMATRIX

C Power-law index is ALFA.  Assume bins not over matrix edges.
C All energies must be in keV.
      DO ICOEF = 1, 4
          IVLOW = 0
          DO IV = 1, NOBINS_IN
             VLOW = EBIN_EDGE_IN(IV)
             VHIGH = EBIN_EDGE_IN(IV+1)
             VWIDE = VHIGH - VLOW
C Jump over bins that underflow compressed matrix elements.
             IF (VLOW.LT.EIN(1).AND.VHIGH.LE.EIN(1)) GOTO 1234
             IF (VLOW.LT.EIN(1).AND.VHIGH.GT.EIN(1)) IVLOW = 1

             IF (IVLOW.EQ.0) THEN
 
C This is first vertical bin, so search for IVLOW.
                DO IVFIND = 1, 64
                   IF ((VLOW.GE.EIN(IVFIND)).AND.
     &                  (VLOW.LE.EIN(IVFIND+1))) THEN
                      IVLOW = IVFIND
                      GOTO 100
                   ENDIF
                ENDDO
 100            CONTINUE
             ENDIF
 
C Next find IVHIGH.
             DO IVFIND = IVLOW, 64
                IF ((VHIGH.GT.EIN(IVFIND)).AND.
     &               (VHIGH.LE.EIN(IVFIND+1))) THEN
                   IVHIGH = IVFIND
                   GOTO 200
                ENDIF
             ENDDO

 200         CONTINUE

             NVPOINTS = IVHIGH - IVLOW + 2
             VCHUNK = VWIDE / FLOAT (NVPOINTS-1)

C Element creation loop.
             DO IVVECT = 1, NVPOINTS
                EVUSE = VLOW + VCHUNK * FLOAT (IVVECT-1)
                DO JCDIF = IVLOW, IVHIGH
                   IF ((EVUSE.GT.EIN(JCDIF)).AND.
     &                  (EVUSE.LE.EIN(JCDIF+1))) THEN
                      DO IVH = 1, 64
                         DIFF_MATRIX_VEC(ICOEF,IVH,IDET+1) =
     &                        DIFF_MATRIX(JCDIF,ICOEF,IVH,IDET+1) +
     &                        (DIFF_MATRIX(JCDIF+1,ICOEF,IVH,IDET+1) -
     &                        DIFF_MATRIX(JCDIF,ICOEF,IVH,IDET+1)) *
     &                        (EVUSE - EIN(JCDIF)) /
     &                        (EIN(JCDIF+1) - EIN(JCDIF))
                      ENDDO
                      GOTO 300
                   ENDIF
                ENDDO
 
 300            CONTINUE

C DIFF_MATRIX_VEC loaded.  Ready for horizontal summation.
                HDW = EVUSE

                DO ILOAD = 1, 64
                   EDIF_EDGEH(ILOAD) = EVUSE * EDIF_EDGE(ILOAD)
                   EDIF_CENT(ILOAD) = EVUSE * 0.5 *
     &                  (EDIF_EDGE(ILOAD+1) + EDIF_EDGE(ILOAD))
                ENDDO

                EDIF_EDGEH(65) = EVUSE * EDIF_EDGE(65)

                IHLOW = 0

                DO IHBIN = 1, NOBINS_OUT
                   HLOW = EBIN_EDGE_OUT(IHBIN)
                   HHIGH = EBIN_EDGE_OUT(IHBIN+1)
                   HWIDE = HHIGH - HLOW

C This is first horizontal bin, so search for IHLOW.
                   IF (IHLOW.EQ.0) THEN
                      DO IHLFIND = 1, 63
                         IF ((HLOW.GT.EDIF_CENT(IHLFIND)).AND.
     &                        (HLOW.LE.EDIF_CENT(IHLFIND+1))) THEN
                            IHLOW = IHLFIND
                            GOTO 150
                         ENDIF
                      ENDDO
 150                  CONTINUE
                   ENDIF

C If IHLOW is still zero after this tremendous effort to non-zero it,
C then HLOW must be less than EDIF_CENT(1).  Assume IHHIGH on scale.
                   IF (HLOW.LE.EDIF_CENT(1)) THEN
                      IF (HHIGH.GT.EDIF_CENT(1)) THEN
C Locate IHHIGH.
                         DO IHFIND = 1, 63
                            IF ((HHIGH.GT.EDIF_CENT(IHFIND)).AND.
     &                           (HHIGH.LE.EDIF_CENT(IHFIND+1))) THEN
                               IHHIGH = IHFIND
                            ENDIF
                         ENDDO

                         NHPOINTS = IHHIGH + 2
                         HCHUNK = HWIDE / FLOAT (NHPOINTS-1)

                         DO ICBIN = 1, NHPOINTS
                            EUSE = HLOW + HCHUNK * FLOAT (ICBIN-1)

                            IF (EUSE.LE.EDIF_CENT(1)) THEN
                               ROW_ENTRY =
     &                              DIFF_MATRIX_VEC(ICOEF,1,IDET+1) *
     &                              EUSE / EDIF_CENT(1)
                               GOTO 1500
                            ENDIF

                            IF (EUSE.GT.EDIF_CENT(1)) THEN
                               DO ICDIF = 1, IHHIGH
C********************** Change in indentation starts here. *********************
                                  IF ((EUSE.GT.EDIF_CENT(ICDIF)).AND.
     &                                (EUSE.LE.EDIF_CENT(ICDIF+1))) THEN
                                     ROW_ENTRY =
     &                                    DIFF_MATRIX_VEC(ICOEF,ICDIF,
     *                                    IDET+1) +
     &                                    (DIFF_MATRIX_VEC(ICOEF,
     *                                    ICDIF+1,IDET+1) -
     &                                    DIFF_MATRIX_VEC(ICOEF,
     *                                    ICDIF,IDET+1)) *
     &                                    (EUSE - EDIF_CENT(ICDIF)) /
     &                                    (EDIF_CENT(ICDIF+1) - 
     *                                    EDIF_CENT(ICDIF))
                                     GOTO 1500
                                  ENDIF
C********************** Change in indentation ends here. ***********************
                               ENDDO
                            ENDIF
 
 1500                       CONTINUE

                            ROW_TOT(IHBIN) = ROW_ENTRY +
     &                           ROW_TOT(IHBIN)
                            ROW_ENTRY = 0.0

                         ENDDO

                         ROW_TOT(IHBIN) = ROW_TOT(IHBIN) * HWIDE /
     &                        FLOAT (NHPOINTS)
                      ENDIF

                      IF (HHIGH.LE.EDIF_CENT(1)) THEN
                         ROW_TOT(IHBIN) =
     &                        DIFF_MATRIX_VEC(ICOEF,1,IDET+1) *
     &                        ((HLOW + HHIGH) / 2.0) /
     &                        EDIF_CENT(1) * HWIDE
                      ENDIF
                   ENDIF

C IHLOW equals last lower EDIF_CENT index below previous edge.
                   IF (IHLOW.GE.64) THEN
                      IF (HLOW.GT.EDIF_EDGEH(65)) THEN
                         ROW_TOT(IHBIN) = - 1.0
                         IHOVER = IHBIN
                         GOTO 600
                      ENDIF

C Could still be below this edge.
                      IF (HLOW.LE.EDIF_EDGEH(65)) THEN
 
C Both bin edges greater than EDIF_CENT(64) and less than EDIF_EDGEH(65).
                         IF (HHIGH.LE.EDIF_EDGEH(65)) THEN
                            ROW_TOT(IHBIN) =
     &                           DIFF_MATRIX_VEC(ICOEF,64,IDET+1) *
     &                           (EDIF_EDGEH(65) - (HLOW + HHIGH) /
     &                           2.0) / (EDIF_EDGEH(65) -
     &                           EDIF_CENT(64)) * HWIDE
                            GOTO 600
                         ENDIF

C One bin edge less, the other greater.
                         IF (HHIGH.GT.EDIF_EDGEH(65)) THEN
                            ROW_TOT(IHBIN) =
     &                           ((EDIF_EDGEH(65) - HLOW)**2) *
     &                           DIFF_MATRIX_VEC(ICOEF,64,IDET+1) /
     &                           (2.0 * (EDIF_EDGEH(65) -
     &                           EDIF_CENT(64)))
                            GOTO 600
                         ENDIF
                      ENDIF
                   ENDIF

                   IF ((IHLOW.LT.64).AND.(IHLOW.GE.1)) THEN
                      IF (HHIGH.GT.EDIF_EDGEH(65)) THEN
                         HWIDE = EDIF_EDGEH(65) - HLOW
                         NHPOINTS = 64 - IHLOW + 2
                         HCHUNK = HWIDE / FLOAT (NHPOINTS-1)

                         DO ICBIN = 1, NHPOINTS
                            EUSE = HLOW + HCHUNK * FLOAT (ICBIN-1)
                                  
                            DO ICDIF = IHLOW, 63
C********************** Change in indentation starts here. *********************
                               IF ((EUSE.GT.EDIF_CENT(ICDIF)).AND.
     &                              (EUSE.LE.EDIF_CENT(ICDIF+1))) THEN
                                  ROW_ENTRY =
     &                                 DIFF_MATRIX_VEC(ICOEF,ICDIF,
     *                                 IDET+1) +
     &                                 (DIFF_MATRIX_VEC(ICOEF,ICDIF+1,
     *                                 IDET+1) -
     &                                 DIFF_MATRIX_VEC(ICOEF,ICDIF,
     *                                 IDET+1)) *
     &                                 (EUSE - EDIF_CENT(ICDIF)) /
     &                                 (EDIF_CENT(ICDIF+1) - 
     *                                 EDIF_CENT(ICDIF))
                                  GOTO 400
                               ENDIF
C********************** Change in indentation ends here. ***********************
                            ENDDO

 400                        ROW_TOT(IHBIN) = ROW_ENTRY +
     &                           ROW_TOT(IHBIN)
                            ROW_ENTRY = 0.0
                         ENDDO

                         ROW_TOT(IHBIN) = ROW_TOT(IHBIN) *
     &                        HWIDE / FLOAT (NHPOINTS)
                         IHLOW = 64

                      ENDIF

C Here we are still inside compressed row, so this is basic summation,
C and HWIDE is not violated.
                      IF (HHIGH.LE.EDIF_EDGEH(65)) THEN
                         DO IHFIND = IHLOW, 63
                            IF ((HHIGH.GT.EDIF_CENT(IHFIND)).AND.
     &                           (HHIGH.LE.EDIF_CENT(IHFIND+1))) THEN
                               IHHIGH = IHFIND
                            ENDIF
                         ENDDO
 
                         IF (HHIGH.GT.EDIF_CENT(64)) IHHIGH = 64
                         NHPOINTS = IHHIGH - IHLOW + 2
                         HCHUNK = HWIDE / FLOAT (NHPOINTS-1)

C********************** Change in indentation starts here. *********************
                         DO ICBIN = 1, NHPOINTS
                            EUSE = HLOW + HCHUNK * FLOAT (ICBIN-1)
                            DO ICDIF = IHLOW, IHHIGH
                               IF (ICDIF.LE.63) THEN
                                  IF ((EUSE.GT.EDIF_CENT(ICDIF)).AND.
     &                                (EUSE.LE.EDIF_CENT(ICDIF+1))) THEN
                                     ROW_ENTRY =
     &                                    DIFF_MATRIX_VEC(ICOEF,ICDIF,
     *                                    IDET+1) +
     &                                    (DIFF_MATRIX_VEC(ICOEF,
     *                                    ICDIF+1,IDET+1) -
     &                                    DIFF_MATRIX_VEC(ICOEF,ICDIF,
     *                                    IDET+1)) *
     &                                    (EUSE - EDIF_CENT(ICDIF)) /
     &                                    (EDIF_CENT(ICDIF+1) - 
     *                                    EDIF_CENT(ICDIF))
                                     GOTO 500
                                  ENDIF
                               ENDIF

                               IF (ICDIF.GT.63) THEN
                                  ROW_ENTRY =
     &                                 DIFF_MATRIX_VEC(ICOEF,ICDIF,
     *                                 IDET+1) *
     &                                 (HHIGH - EDIF_CENT(64)) /
     &                                 (EDIF_CENT(64) - EDIF_CENT(63))
                                  GOTO 500
                               ENDIF
                            ENDDO

 500                        ROW_TOT(IHBIN) = ROW_ENTRY + ROW_TOT(IHBIN)
                            ROW_ENTRY = 0.0

                         ENDDO
C********************** Change in indentation ends here. ***********************

                         ROW_TOT(IHBIN) = ROW_TOT(IHBIN) *
     &                        HWIDE / FLOAT (NHPOINTS)
                         IHLOW = IHHIGH
                      ENDIF
                   ENDIF

 600               CONTINUE

                   IF (ROW_TOT(IHBIN).EQ.-1.0) GOTO 700
                   IF (IHBIN.EQ.NOBINS_OUT) IHOVER = NOBINS_OUT + 1
                ENDDO

 700            CONTINUE

C Now in vertical part, only go to IHOVER.  Remember to reset ROW_TOT.
                DO IVHSUM = 1, IHOVER-1
                   BINNED_MATRIX(ICOEF,IV,IVHSUM) =
     &                  BINNED_MATRIX(ICOEF,IV,IVHSUM) +
     &                  ROW_TOT(IVHSUM) * EVUSE**(-ALFA)
                   ROW_TOT(IVHSUM) = 0.0
                ENDDO

                ROW_TOT(IHOVER) = 0.0
                DENOM = DENOM + EVUSE**(-ALFA)
             ENDDO

C Normalize matrix elements correctly.
             DO INORMAL = 1, NOBINS_OUT
C Bin elements assuming incident flux is in photons/cm^2 integrated across
C bin.  Would multiply by VWIDE if incident flux expressed in photons/cm^2/keV
C (i.e., divided by bin width).
                BINNED_MATRIX(ICOEF,IV,INORMAL) =
     &               (BINNED_MATRIX(ICOEF,IV,INORMAL) / DENOM)
             ENDDO

             IVLOW = IVHIGH
             DENOM = 0.0

C Jump for lower end underflow.
 1234        CONTINUE

          ENDDO

C Next load into the specific coefficient matrix the BINNED_MATRIX.
          DO IVV = 1, NOBINS_IN
             DO IHH = 1, NOBINS_OUT
                IF (IMATRIX.EQ.0) FMATLOW(ICOEF,IVV,IHH) =
     &               BINNED_MATRIX(ICOEF,IVV,IHH)
                IF (IMATRIX.EQ.2.OR.IMATRIX.EQ.1)
     &               FMATMED(ICOEF,IVV,IHH) =
     &               BINNED_MATRIX(ICOEF,IVV,IHH)
                IF (IMATRIX.EQ.3) FMATHIGH(ICOEF,IVV,IHH) =
     &               BINNED_MATRIX(ICOEF,IVV,IHH)
                BINNED_MATRIX(ICOEF,IVV,IHH) = 0.0
             ENDDO
          ENDDO
       ENDDO

       RETURN
       END
 


C*******************************************************************************
C SUBROUTINE: transpose_matrix
C
C DESCRIPTION: Routine that transposes matrix a to matrix b.
C
C AUTHOR/DATE: Martin Brock and G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS:
C     a - 3 by 3 input matrix (r*8)
C     b - 3 by 3 output matrix (r*8)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************
 
      subroutine transpose_matrix (a, b)
 
      real*8 a(3,3), b(3,3)

      integer i, j

      do i = 1, 3
         do j = 1, 3
            b(j,i) = a(i,j)
         end do
      enddo
 
      return
      end
 


C*******************************************************************************
C SUBROUTINE: unit_vector
C
C DESCRIPTION: Routine that transforms spherical coordinates to
C     Cartesian unit vector.
C
C AUTHOR/DATE: Martin Brock and G.N. Pendleton, MSFC, 03/93
C
C NOTES:
C
C ARGUMENTS:
C     sphere - spherical coordinates (r*8)
C     cartes - Cartesian unit vector (r*8)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     08/99, cosmetic changes, Peter J.T. Leonard, RITSS
C*******************************************************************************

      subroutine unit_vector (sphere, cartes)
 
      real*8 cos_dec, sphere(2), cartes(3)
 
      cos_dec = dcos (sphere(2))
      cartes(1) = cos_dec * cos (sphere(1))
      cartes(2) = cos_dec * sin (sphere(1))
      cartes(3) = dsin (sphere(2))
 
      return
      end
 

C*******************************************************************************
C SUBROUTINE: read_inst_resp
C
C DESCRIPTION: Reads in inst_resp.fits, which contains the detector-specific
C     response matrix data edif_edge(65), ein(275) and diff_matrix(64,4,64,8).
C
C AUTHOR/DATE: Peter J.T. Leonard, RITSS, 08/99
C
C NOTES:
C
C ARGUMENTS:
C     inrfil      - name of input FITS file containing detector-specific
C                   response matrix data
C     edif_edge   - vector (r*4)
C     ein         - vector (r*4)
C     diff_matrix - 4-d array (r*4)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: Several FITS I/O routines are called.
C
C MODIFICATION HISTORY:
C*******************************************************************************
 
      subroutine read_inst_resp (inrfil, edif_edge, ein, diff_matrix)

      implicit none
 
      integer*4 status, unit, rwmode, blocksize
      integer*4 group, fpixel, nelements, nullval, anyf
      integer*4 nmove, hdutype
      integer*4 naxes(4)
 
      real*4 edif_edge(65), ein(275)
      real*4 diff_matrix(64,4,64,8)
 
      character*(*) inrfil
 
C Set up common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common/tasknm/taskname
      taskname = 'bod2rmf'
 
C Find unused logical unit number.
      status = 0
      call ftgiou (unit, status)
 
C Open FITS file containing diff_matrix array and edif_edge and ein vectors.
      rwmode = 0
      blocksize = 1
      call ftopen (unit, inrfil, rwmode, blocksize, status)
 
C Read in diff_matrix array from FITS file.
      group = 1
      fpixel = 1
      naxes(1) = 64
      naxes(2) = 4
      naxes(3) = 64
      naxes(4) = 8
      nelements = naxes(1) * naxes(2) * naxes(3) * naxes(4)
      nullval = 0
      anyf = 0
      call ftgpve (unit, group, fpixel, nelements, nullval, diff_matrix,
     &     anyf, status)
 
C Move to extension of FITS file.
      nmove = 1
      hdutype = 0
      call ftmrhd (unit, nmove, hdutype, status)
 
C Read in edif_edge vector from FITS file extension.
      group = 1
      fpixel = 1
      naxes(1) = 65
      nelements = naxes(1)
      nullval = 0
      anyf = 0
      call ftgpve (unit, group, fpixel, nelements, nullval, edif_edge,
     &     anyf, status)
 
C Move to second extension of FITS file.
      nmove = 1
      hdutype = 0
      call ftmrhd (unit, nmove, hdutype, status)
 
C Read in ein vector from FITS file second extension.
      group = 1
      fpixel = 1
      naxes(1) = 275
      nelements = naxes(1)
      nullval = 0
      anyf = 0
      call ftgpve (unit, group, fpixel, nelements, nullval, ein,
     &     anyf, status)
 
C Close FITS file, and free logical unit.
      call ftclos (unit, status)
      call ftfiou (unit, status)
 
      if (status.ne.0) then
         msg = 'Problem reading inst_resp.fits!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif
 
      return
      end


C*******************************************************************************
C SUBROUTINE: rsp_write2
C
C DESCRIPTION: Prepares for the writing out of the BATSE direct response
C     matrix into an XSPEC RMF FITS file.
C
C AUTHOR/DATE: Peter J.T. Leonard, RITSS, 08/99
C
C NOTES:
C
C ARGUMENTS:
C     rmffil     - name of response matrix file (c*)
C     det        - BATSE LAD number (0 to 7) (i*4)
C     chan_edges - channel edges of PHA count rate spectrum (r*4)
C     nchan      - number of PHA channels channels (i*4)
C     e_edges    - energy edges of incident photon spectrum (r*4)
C     nenerg     - number of energy bins in photon spectrum (i*4)
C     response   - direct response matrix (r*4)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine write_rsp2 - writes out RMF FITS file in XSPEC format
C
C MODIFICATION HISTORY:
C*******************************************************************************
 
      subroutine rsp_write2 (rmffil, det, chan_edges, nchan,
     &                       e_edges, nenerg, response)
 
      implicit none
 
      integer*4 ierr, i, j, det, nenerg, nchan, nk_hist, nk_comm
      integer*4 mxener, mxchan, mxelem, mxgrps, mxtgrp
      parameter (mxener=300, mxchan=64, mxgrps=6)
      parameter (mxelem=mxener*mxchan, mxtgrp=mxener*mxgrps)
      integer*4 ngroup(mxener), ichanb(mxener,mxtgrp)
      integer*4 nchang(mxener,mxtgrp)
c     integer*4 ngroup(mxener), ichanb(mxtgrp), nchang(mxtgrp)
 
      real*4 response(129,16), e_edges(0:129), chan_edges(0:16)
      real*4 e_min(mxchan), e_max(mxchan), rsp_min, matrix(64,300)
      real*4 rspmatrix(64,300), rspmat(mxelem), energies(0:mxener)
      parameter (rsp_min=1.0e-6)
 
      character*(*) rmffil
      character(80) hist, comment
      character(20) tlscpe, instrm, detnam, filter, hduclas3

      data tlscpe / 'BATSE' /
      data instrm / 'BATSE' /
      data filter / 'NONE' /
      data hduclas3 / 'FULL' /

C Set up common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common/tasknm/taskname
      taskname = 'bod2rmf'
c
      if (det .eq. 0) instrm = 'LAD0'
      if (det .eq. 1) instrm = 'LAD1'
      if (det .eq. 2) instrm = 'LAD2'
      if (det .eq. 3) instrm = 'LAD3'
      if (det .eq. 4) instrm = 'LAD4'
      if (det .eq. 5) instrm = 'LAD5'
      if (det .eq. 6) instrm = 'LAD6'
      if (det .eq. 7) instrm = 'LAD7'
c
C BATSE LAD number.
      write (detnam, 10) det
10    format ('LAD',i1)
 
C History and comments.
      nk_hist = 1
      hist = 'XSPEC RMF file produced from BATSE response matrix.'
      nk_comm = 0
      comment = hist
 
C Direct response matrix.
      do i = 1, mxchan
         do j = 1, mxener
            if ((i.le.nchan).and.(j.le.nenerg)) then
               rspmatrix(i,j) = response(j,i)
            else
               rspmatrix(i,j) = 0.0
            endif
         enddo
      enddo
 
C Energy edges of photon spectrum.
      do j = 0, mxener
         if (j.le.nenerg) then
            energies(j) = e_edges(j)
         else
            energies(j) = 0.0
         endif
      enddo

C Channel edges of detector.
      do i = 1, mxchan
         if (i.le.nchan) then
            e_min(i) = chan_edges(i-1)
            e_max(i) = chan_edges(i)
         else
            e_min(i) = 0.0
            e_max(i) = 0.0
         endif
      enddo

C Write XSPEC response matrix.
      ierr = 0
      call write_rsp2 (nchan, nenerg, mxelem, mxgrps, mxtgrp,rsp_min, 
     *     rspmatrix, matrix, energies, e_min, e_max, tlscpe, instrm, 
     *     detnam, filter, hduclas3, rmffil, nk_hist, hist, nk_comm, 
     *     comment, rspmat, ngroup, ichanb, nchang, ierr)

      if (ierr.ne.0) then
         msg = 'Problem writing output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif
 
      return
      end
 

C*******************************************************************************
C SUBROUTINE: write_rsp2
C
C DESCRIPTION: Writes out an RMF FITS file in XSPEC format.
C
C AUTHOR/DATE: Keith A. Arnaud, 10/94
C              John R. Mattox, 08/96
C
C NOTES:
C
C ARGUMENTS:
C     nchan    - number of channels in response matrix
C     nenerg   - number of energy bins in response matrix
C     mxelem   - maximum number of non-zero response elements
C     mxgrps   - maximum number of groups at a given energy
C     mxtgrp   - maximum number of total groups
C     rsp_min  - minimum value of response that is stored
C     matrix   - response matrix
C     energies - energy bins for the response
C     e_min    - nominal lower energy bounds for channels
C     e_max    - nominal upper energy bounds for channels
C     tlscpe   - telescope name 
C     instrm   - instrument name 
C     detnam   - detector name 
C     filter   - filter name 
C     hduclas3 - if = REDIST   => photon redistribution matrix (only)
C                   = DETECTOR => convolved with detector effects (only)
C                   = FULL     => convolved with all effects (det+optic)
C     rmffil   - output filename
C     nk_hist  - number of history records
C     hist     - history records (optional)
C     nk_comm  - number of comment records
C     comment  - comment records (optional)
C     rspmat   - non-zero response elements
C     ngroup   - number of contiguous channel sets
C     ichanb   - start channel of a group
C     nchang   - number of channels in a group
C     ierr     - error code (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine cmprsp2 - compresses response matrix to XSPEC format
C     Several FITS I/O routines are also called.
C
C MODIFICATION HISTORY:
C     03/97, cosmetic changes, PJTL and JMSS, HSTX
C*******************************************************************************

      subroutine write_rsp2 (nchan, nenerg, mxelem, mxgrps, mxtgrp,
     *     rsp_min, rspmatrix, matrix, energies, e_min, e_max, tlscpe, 
     *     instrm, detnam, filter, hduclas3, rmffil, nk_hist, hist,
     &     nk_comm, comment, rspmat, ngroup, ichanb, nchang, ierr)
 
      implicit none

      integer*4 nchan, nenerg, mxelem, mxgrps, mxtgrp
      integer*4 nk_hist, nk_comm, ierr, i, j
      integer*4 ngroup(nenerg), ichanb(nenerg,mxtgrp)
      integer*4 nchang(nenerg,mxtgrp)
c      integer*4 ngroup(nenerg), ichanb(mxtgrp), nchang(mxtgrp)

      real*4 rspmatrix(64,300), matrix(nchan,nenerg), rspmat(mxelem)
      real*4 rsp_min, energies(0:nenerg), e_min(nchan), e_max(nchan)

      character*(*) rmffil, hist(nk_hist), comment(nk_comm)
      character*(*) tlscpe, instrm, detnam, filter, hduclas3

      integer*4 ounit, chatter, nelem, ntgrps
 
C Set up common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common/tasknm/taskname
      taskname = 'bod2rmf'

      chatter = 10

      do i = 1, nchan
         do j = 1, nenerg
            matrix(i,j) = rspmatrix(i,j)
         enddo
      enddo

C Calculate the compressed matrix.
      call cmprsp2 (nchan, nenerg, mxelem, mxgrps, mxtgrp, rsp_min,
     &     matrix, rspmat, ngroup, ichanb, nchang, nelem, ntgrps)

C Open the output FITS file.
      ounit = 15
      ierr = 0
      call ftinit (ounit, rmffil, 1, ierr) 

      if (ierr.ne.0) then
         msg = 'Problem opening output file!'
         call fcerr (msg)
         msg = 'Does output file already exist?'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif

C Write the primary header.
      call ftpdef (ounit, 8, 0, 0, 0, 1, ierr)

      if (ierr.ne.0) then
         msg = 'Problem writing primary header in output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif

C Write the basic primary array keywords.
      call ftphpr (ounit, .true., 8, 0, 0, 0, 1, .true., ierr)

      if (ierr.ne.0) then
         msg = 'Problem writing primary array keywords in output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif

C Write out the additional keywords for the creation of the FITS file.
      call ftpkys (ounit, 'CONTENT', 'RESPONSE',
     & 'spectral response matrix', ierr)

      if (ierr.ne.0) then
         msg = 'Problem writing additional keywords in output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif
 
      call ftpkys (ounit, 'ORIGIN', 'NASA/GSFC',
     & 'origin of FITS file', ierr)

      if (ierr.ne.0) then
         msg = 'Problem writing additional keywords in output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif

C Write the energy bounds extensions.
      call wtebd2 (ounit, chatter, nk_hist, hist, nk_comm, comment, 
     &     '1.1.0', tlscpe, instrm, detnam, filter, 1.0, 1, nchan, 
     *     e_min, e_max, ierr) 

      if (ierr.ne.0) then
         msg =
     &    'Problem writing energy bounds extensions in output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif

C Write the response matrix extensions.  Note that wtrmf1 wants the
C uncompressed matrix passed and not the compressed matrix.


      call wtrmf1 (ounit, chatter, nk_hist, hist, nk_comm, comment,
     *     '1.1.0', hduclas3, tlscpe, instrm, detnam, filter, 1.0, 
     *     nchan,  nchan, nenerg, nenerg, energies(0), energies(1), 
     *     mxgrps, ngroup, ichanb, nchang, matrix, rsp_min, ierr)

      if (ierr.ne.0) then
         msg =
     &    'Problem writing response matrix extensions in output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif

C Close the output file.
      call ftclos (ounit, ierr)

      if (ierr.ne.0) then
         msg = 'Problem closing output file!'
         call fcerr (msg)
         msg = 'Aborting program.'
         call fcerr (msg)
         call exit (1)
      endif
 
      return
      end
 

C*******************************************************************************
C SUBROUTINE: cmprsp2
C
C DESCRIPTION: Routine that compresses a response matrix to XSPEC format.
C
C AUTHOR/DATE: Keith A. Arnaud, 03/89
C
C NOTES:
C
C ARGUMENTS:
C     nchan   - number of channels in response matrix
C     nenerg  - number of energy bins in response matrix
C     mxelem  - maximum number of non-zero response elements
C     mxgrps  - maximum number of groups at a given energy
C     mxtgrp  - maximum number of total groups
C     rsp_min - minimum value of response that is stored
C     matrix  - response matrix
C     rspmat  - non-zero response elements
C     ngroup  - number of contiguous channel sets
C     ichanb  - start channel of a group
C     nchang  - number of channels in a group
C     nelem   - number of non-zero response elements
C     ntgrps  - number of groups
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C     03/97, cosmetic changes, PJTL and JMSS, HSTX
C*******************************************************************************
 
      subroutine cmprsp2 (nchan, nenerg, mxelem, mxgrps, mxtgrp, 
     *     rsp_min, matrix, rspmat, ngroup, ichanb, nchang, nelem, 
     *     ntgrps)
 
      implicit none

      integer nchan, nenerg, mxelem, mxgrps, mxtgrp
      integer ngroup(nenerg), ichanb(nenerg,mxtgrp)
      integer nchang(nenerg,mxtgrp)
c      integer ngroup(nenerg), ichanb(mxtgrp), nchang(mxtgrp)
      integer nelem, ntgrps, i, j, igroup, iresp

      real rsp_min, effic, rsp
      real matrix(nchan,nenerg), rspmat(mxelem)

      logical counting
 
C Set up common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common/tasknm/taskname
      taskname = 'bod2rmf'

C Create the response matrix - start by looping over energies.
      igroup = 0
      iresp = 0

      do i = 1, nenerg

C Loop over channels.
         effic = 0
         ngroup(i) = 0
         counting = .false.
         do j = 1, nchan
            rsp = matrix(j, i)

C If response greater than minimum acceptable, then include.
            if (rsp.gt.rsp_min) then
               iresp = iresp + 1
 
               if (iresp.gt.mxelem) then
                  msg = 'Too many response elements - increase mxelem!'
                  call fcerr (msg)
                  msg = 'Aborting program.'
                  call fcerr (msg)
                  call exit (1)
               endif
 
               rspmat(iresp) = rsp
               effic = effic + rsp

C If not currently in a group, then start one.
               if (.not.counting) then
                  igroup = igroup + 1
 
                  if (igroup.gt.mxtgrp) then
                     msg = 'Too many response groups - increase mxtgrp!'
                     call fcerr (msg)
                     msg = 'Aborting program.'
                     call fcerr (msg)
                     call exit (1)
                  endif
 
c                  ichanb(igroup) = j
                  counting = .true.
                  ngroup(i) = ngroup(i) + 1
		  ichanb(i,ngroup(i)) = j
 
                  if (ngroup(i).gt.mxgrps) then
                     msg = 'Too many response groups - increase mxgrps!'
                     call fcerr (msg)
                     msg = 'Aborting program.'
                     call fcerr (msg)
                     call exit (1)
                  endif
               endif

C If response not greater than acceptable minimum and in group, then end group.
            else
               if (counting) then
                  counting = .false.
                  nchang(i,ngroup(i)) = j - ichanb(i,ngroup(i)) + 1
c                  nchang(igroup) = j - ichanb(igroup) + 1
               endif
            endif
         enddo

         if (counting) then
            nchang(i,ngroup(i)) = nchan - ichanb(i,ngroup(i)) + 1
         endif
      enddo

      nelem = iresp
      ntgrps = igroup
 
      return
      end

C*******************************************************************************
C SUBROUTINE: get_chan_edges
C
C DESCRIPTION: Routine that provides energies of channel edges for BATSE LAD.
C
C AUTHOR/DATE: Chris Shrader, Code 661  NASA/GSFC  03/01
C
C NOTES: Replaces an earlier version, which assumed static detecotr gains
C
C ARGUMENTS:
C     tjd_start  - trunccated julian date of observation start
C     tjd_stop   - trunccated julian date of observation end
C     det        - BATSE LAD number (0 to 7)
C     chan_edges - energies of channel edges for BATSE LAD
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES: None.
C
C MODIFICATION HISTORY:
C        replaces earlier version; now takes into account time dependencies of
C        BATSE LAD calibration; usilizes "cont_calib" subroutines and
C        parameters provided by C. Wilson-Hodge, NASA/MSFC (individual MSFC
c        programmers are noted in appropriate subroutines)
c 2005 April 15 Further modification by C. Wilson-Hodge to correct erroneous
c usage of cont_calib subroutines that resulted in incorrect channel edges.
c
C*******************************************************************************
      subroutine get_chan_edges (det, tjd_start, tjd_stop, chan_edges)

      implicit none
c input variables
      integer   det      
      real*4     tjd_start, tjd_stop
c output variables
      real*4 chan_edges(17) 
c local variables needed for call to c_read_lad_lookup
      integer*4 max_entries     !maximum number of entries in the lookup table
      parameter (max_entries = 17)
      real*8     start_time(max_entries), stop_time(max_entries)  
                                !interval of validity (TJD)
      integer*4  tab(max_entries,0:15) !lad lookup table entry
      integer*4 n_entries, ltstatus !for lad lookup table
c local variables for call to load_geoff
      real*4 geoff_edges(0:7,0:16)
      common /pendleton/ geoff_edges
c local variables for call to cont_calib_eedges
c     integer det  (declared as an input variable. Listed here for completeness)
      integer*4 lookup_tab(0:15)
      real*4 e_edges(0:16) !channel boundaries for the 16 CONT channels
                                !e_edges(k) = Lower edge (keV) of channel k
                                !and upper edge (keV) of channel k-1.
     
c other local variables
      integer*4 i_entry, i


      ltstatus=0
c     read in the lad lookup table
      call c_read_lad_lookup(max_entries, start_time, 
     *     stop_time, tab, n_entries, ltstatus)

c
      if (ltstatus .lt. 0) then
         write(6,*) 'Parameter max_entries too small in CONT_CALIB'
         call exit (1)
      endif
       
c     load pendletons edges
      call load_geoff
c     loop through the entries in the lookup table
       do i_entry = 1,n_entries
         do i=0, 15 
            lookup_tab(i) = tab(i_entry,i)
         enddo
c when the appropriate lookup entry is found, generate the edges         
         if (start_time(i_entry) .lt. tjd_stop .and.
     &	      stop_time(i_entry)  .gt. tjd_start) then
           call cont_calib_eedges(det, lookup_tab, e_edges)
           if (lookup_tab(0) .ge. lookup_tab(15)) then
             e_edges(0) = e_edges(1)-1.0
           endif  
         endif
      enddo  
      do i=0,16
        chan_edges(i+1) = e_edges(i)
      enddo  
      return
      end

       subroutine cont_calib_eedges(detector,lookup_tab,e_edges)
c      calculate the cont energy edges. Uses Pendletons edges interpolated
c      in linear channels.
       implicit none
c  input:
       integer*4 detector
       integer*4 lookup_tab(0:15)
c  ouput:
       real*4 e_edges(0:16)  !channel edges in keV
c  local:                    
       real*4 geoff_edges(0:7,0:16)
       common /pendleton/ geoff_edges
       integer*4 lc  !linear channels of input lookup table
       integer*4 lc0(0:15) !linear channels used in making Pendletons edges.
       data lc0/0,7,9,11,14,18,23,28,36,49,65,85,115,143,207,335/
       integer*4 chan,hchan,lchan,i
       do chan = 0, 15
          hchan = lookup_tab(chan)
          if(hchan.lt.65) then
             lc = hchan
          else if(hchan.lt.96) then
             lc = 2*hchan-65
          else
             lc = 8*hchan-641
          endif
          i = 0
          if(lc .ge. lc0(8))  i = 8
          if(lc .ge. lc0(i+4)) i = i+4
          if(lc .ge. lc0(i+2)) i = i+2
          if(lc .ge. lc0(i+1)) i = i+1
          if(i .eq. 15) i = 14
          e_edges(chan) = geoff_edges(detector,i)
     &          +(geoff_edges(detector,i+1)-geoff_edges(detector,i))
     &           *(lc-lc0(i))/real(lc0(i+1)-lc0(i))
       enddo
       e_edges(16) = geoff_edges(detector,16)
       return
       end   

      subroutine load_geoff
c     load geoff_edges with data
      real*4 geoff_edges(0:7,17) !note difference from declaration in cont
                                !calib_eedges
      common /pendleton/ geoff_edges
      geoff_edges(0,1) = 9.88152
      geoff_edges(0,2) = 22.7307
      geoff_edges(0,3) = 33.3997
      geoff_edges(0,4) = 43.3015
      geoff_edges(0,5) = 58.4717
      geoff_edges(0,6) = 77.3767
      geoff_edges(0,7) = 101.916
      geoff_edges(0,8) = 126.448
      geoff_edges(0,9) = 165.523
      geoff_edges(0,10) = 227.594
      geoff_edges(0,11) = 310.361
      geoff_edges(0,12) = 405.015
      geoff_edges(0,13) = 600.988
      geoff_edges(0,14) = 725.978
      geoff_edges(0,15) = 1009.82
      geoff_edges(0,16) = 1822.39
      geoff_edges(0,17) = 8469.54
      geoff_edges(1,1) = 14.3909
      geoff_edges(1,2) = 21.7234
      geoff_edges(1,3) = 29.4773
      geoff_edges(1,4) = 38.2654
      geoff_edges(1,5) = 52.0801
      geoff_edges(1,6) = 70.4377
      geoff_edges(1,7) = 93.9625
      geoff_edges(1,8) = 118.376
      geoff_edges(1,9) = 158.347
      geoff_edges(1,10) = 224.636
      geoff_edges(1,11) = 314.599
      geoff_edges(1,12) = 421.479
      geoff_edges(1,13) = 584.750
      geoff_edges(1,14) = 720.702
      geoff_edges(1,15) = 1071.17
      geoff_edges(1,16) = 1799.97
      geoff_edges(1,17) = 8922.53
      geoff_edges(2,1) = 14.4930
      geoff_edges(2,2) = 22.8370
      geoff_edges(2,3) = 33.8832
      geoff_edges(2,4) = 44.7875
      geoff_edges(2,5) = 58.7856
      geoff_edges(2,6) = 77.4970
      geoff_edges(2,7) = 102.292
      geoff_edges(2,8) = 127.548
      geoff_edges(2,9) = 168.072
      geoff_edges(2,10) = 237.354
      geoff_edges(2,11) = 330.926
      geoff_edges(2,12) = 448.746
      geoff_edges(2,13) = 612.848
      geoff_edges(2,14) = 772.944
      geoff_edges(2,15) = 1149.10
      geoff_edges(2,16) = 1887.83
      geoff_edges(2,17) = 8490.77
      geoff_edges(3,1) = 13.3458
      geoff_edges(3,2) = 21.3761
      geoff_edges(3,3) = 33.1016
      geoff_edges(3,4) = 44.3154
      geoff_edges(3,5) = 56.8231
      geoff_edges(3,6) = 75.1088
      geoff_edges(3,7) = 100.314
      geoff_edges(3,8) = 125.524
      geoff_edges(3,9) = 164.494                     
      geoff_edges(3,10) = 235.404
      geoff_edges(3,11) = 306.370
      geoff_edges(3,12) = 434.191
      geoff_edges(3,13) = 594.244
      geoff_edges(3,14) = 745.486
      geoff_edges(3,15) = 1105.48
      geoff_edges(3,16) = 1805.78
      geoff_edges(3,17) = 8691.89
      geoff_edges(4,1) = 9.86876
      geoff_edges(4,2) = 20.7733
      geoff_edges(4,3) = 28.9396
      geoff_edges(4,4) = 38.2588
      geoff_edges(4,5) = 52.3798
      geoff_edges(4,6) = 70.1647
      geoff_edges(4,7) = 94.1696
      geoff_edges(4,8) = 119.672
      geoff_edges(4,9) = 156.187                     
      geoff_edges(4,10) = 219.342
      geoff_edges(4,11) = 301.997
      geoff_edges(4,12) = 417.745
      geoff_edges(4,13) = 592.604
      geoff_edges(4,14) = 758.880
      geoff_edges(4,15) = 1121.00
      geoff_edges(4,16) = 1798.39
      geoff_edges(4,17) = 8454.14
      geoff_edges(5,1) = 17.3184
      geoff_edges(5,2) = 22.0365
      geoff_edges(5,3) = 31.1646
      geoff_edges(5,4) = 39.6137
      geoff_edges(5,5) = 53.0077
      geoff_edges(5,6) = 71.1232
      geoff_edges(5,7) = 95.7904
      geoff_edges(5,8) = 121.892
      geoff_edges(5,9) = 164.162                     
      geoff_edges(5,10) = 238.622
      geoff_edges(5,11) = 324.528
      geoff_edges(5,12) = 455.241
      geoff_edges(5,13) = 612.680
      geoff_edges(5,14) = 784.340
      geoff_edges(5,15) = 1172.46
      geoff_edges(5,16) = 1812.71
      geoff_edges(5,17) = 8739.07         
      geoff_edges(6,1) = 11.4300
      geoff_edges(6,2) = 21.2083
      geoff_edges(6,3) = 33.6429
      geoff_edges(6,4) = 45.2910
      geoff_edges(6,5) = 58.0456
      geoff_edges(6,6) = 75.5492
      geoff_edges(6,7) = 99.7253
      geoff_edges(6,8) = 124.962
      geoff_edges(6,9) = 164.436
      geoff_edges(6,10) = 235.560                     
      geoff_edges(6,11) = 317.121
      geoff_edges(6,12) = 439.893
      geoff_edges(6,13) = 581.732
      geoff_edges(6,14) = 817.943
      geoff_edges(6,15) = 1109.46
      geoff_edges(6,16) = 1799.21
      geoff_edges(6,17) = 8384.32           
      geoff_edges(7,1) = 12.7402
      geoff_edges(7,2) = 21.4884
      geoff_edges(7,3) = 27.6048
      geoff_edges(7,4) = 36.1036
      geoff_edges(7,5) = 51.8152
      geoff_edges(7,6) = 70.2859
      geoff_edges(7,7) = 94.9515
      geoff_edges(7,8) = 120.532
      geoff_edges(7,9) = 157.946                     
      geoff_edges(7,10) = 222.366
      geoff_edges(7,11) = 300.658
      geoff_edges(7,12) = 407.180
      geoff_edges(7,13) = 581.978
      geoff_edges(7,14) = 802.225
      geoff_edges(7,15) = 1094.40
      geoff_edges(7,16) = 1821.62
      geoff_edges(7,17) = 8406.58

      return
      end                       !CONT_CALIB subroutine!


      subroutine c_read_lad_lookup(max_entries, start_time,
     .     stop_time, tab, n_entries, status)
c      read the lookup table and stuff it into an array of structures
      implicit none
c
c modifed to used data initialization statements instead of ascii file
c input   CRS 03/01
c
c    input:
      integer*4 max_entries, max_recs !maximum number of entries
c    output:

      real*8     start_time(max_entries), stop_time(max_entries) !interval of validity (TJD)
      integer*4  tab(max_entries,0:15) !lad lookup table entry


c       structure /lookup_entry/  
c          real*8 start_time, stop_time     !interval of validity
c          integer*4 tab(0:15)
c       endstructure
c
c       record /lookup_entry/lookup(max_entries)

      integer*4 n_entries       !number of table entries
      integer*4 status          !0 if ok, -1 if max_entries too small
c
      integer*4    tjd_interval(2, 0:16)
      common /table_data/ tab_dat
      integer*4    tab_dat(16,17), i, i1, i2, j

c
c eliminate ascii input file

      data tjd_interval /
     .     8361,         0,
     .     8367,         0,
     .     8406,     81139,
     .     8807,      8279,
     .     8809,     64511,
     .     8812,     65817,
     .     8812,     66103,
     .     9400,     59319,
     .     9419,     60041,
     .     10062,     71275,
     .     10178,     85302,
     .     10465,     83335,
     .     10504,     80991,
     .     10983,     71907,
     .     11053,     51166,
     .     11055,     54394,
     .     11085,     74663 /

c
      data tab_dat /
     .  0, 13, 15, 17, 19, 22, 25, 30, 37, 49, 64, 74, 89, 100, 111,125,
     .  0,  9, 11, 14, 18, 22, 26, 30, 38, 49, 65, 75, 90, 98, 106, 122,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106, 122,
     .  0, 13, 15, 17, 19, 22, 25, 30, 37, 49, 64, 74, 89, 100, 111,125,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106, 122,
     .  0, 13, 15, 17, 19, 22, 25, 30, 37, 49, 64, 74, 89, 100, 111,125,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106, 122,
     .  7,  9, 11, 14, 18, 20, 23, 25, 28, 32, 36, 49, 65, 75, 90, 122,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106, 122,
     .  122, 7,  8,  9, 10, 11, 12, 14, 16, 18, 23, 28, 36, 49, 75, 90,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106, 122,
     .  0,  7,  8,  9, 10, 11, 12, 14, 16, 18, 23, 28, 36, 49, 75, 122,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106, 122,
     .  0,  7,  8,  9, 10, 11, 12, 14, 16, 18, 23, 28, 36, 49, 75, 122,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106, 122,
     .  0,  7,  8,  9, 10, 11, 12, 14, 16, 18, 23, 28, 36, 49, 75, 122,
     .  0,  7,  9, 11, 14, 18, 23, 28, 36, 49, 65, 75, 90, 98, 106,122 /
c
      n_entries = 0 
      do while(n_entries.lt.max_entries)
         i1 = tjd_interval(1,n_entries)
         i2 = tjd_interval(2,n_entries)
         n_entries = n_entries+1

         start_time(n_entries) = i1+i2/8.64D+04

c     lookup(n_entries).start_time = i1+i2/8.64D+04
c     if(n_entries .ge. 1) then
c
c  sb-12/01 fixed bug: stop_time should be calculated for 1-max_entries
c
         if (n_entries .gt. 1) then
	    stop_time(n_entries-1) = start_time(n_entries)
c     lookup(n_entries-1).stop_time=lookup(n_entries).start_time
         endif
      enddo

      stop_time(n_entries) = 1.0D+30

c     lookup(n_entries).stop_time = 1.0D+30
c     
c     do 10 i=1,max_recs
c     do 10 j=1,16
c     10	   tab(i-1,j-1) = tab_dat(j,i)
c     c 10      lookup(i-1).tab(j-1)   = tab_dat(j,i)

c
c sb-12/01  fixed a bug : tab-1st dim was indexed from 0 to max_recs-1
c it should be 1-max_recs
c
      do i = 1, 17
	 do j = 1, 16
	    tab(i,j-1) = tab_dat(j,i)
         enddo
      enddo

c 10      lookup(i-1).tab(j-1)   = tab_dat(j,i)
c
      return 
      end       


      subroutine apply_attenuation_correction (det, elev_deg, 
     & azimuth_deg, e_edges,ne_edges,directmatrix,nmaxebin,nchan)
c This subroutine applies corrections to the response matrix described in 
c Section 3.5 of Harmon et al. 2002, ApJS, 138, 149
      implicit none
      real*4 azimuth_deg, elev_deg, detangles(8), 
     & effective_area(8,2),correction, ang, midenergy
      integer det,ii,jj,kk,ierror, ne_edges
      integer*2 nmaxebin,nchan
      real*4 directmatrix(nmaxebin,nchan),e_edges(ne_edges)
c Constant A in Eqn. 9 of Harmon et al. 2002, ApJS, 138, 149
c This constant is different for each LAD
      effective_area(1,1) = 0.15
      effective_area(2,1) = 0.08
      effective_area(3,1) = 0.10
      effective_area(4,1) = 0.13
      effective_area(5,1) = 0.15
      effective_area(6,1) = 0.05
      effective_area(7,1) = 0.03
      effective_area(8,1) = 0.17
c Constant B in Eqn. 9 of Harmon et al. 2002, ApJS, 138, 149
c This constant is the same for all 8 LADs.
      do ii=1,8 
        effective_area(ii,2) = 0.02
      enddo   
c get source aspect angles, i.e. the angle between the detector normal and 
c the source direction. 
      call get_directions_angles(elev_deg,azimuth_deg, detangles)
      ang=detangles(det+1)
      
      do jj = 1, 70
!..... get the attenuation correction factor
    	  midenergy = (e_edges(jj+1) + e_edges(jj) ) / 2.0
          call attenc(midenergy, ang, correction,ierror)      
	  if (ierror .eq. 1) stop         
          do kk=1,16
            directmatrix(jj,kk) = directmatrix(jj,kk)*correction
            directmatrix(jj,kk) = directmatrix(jj,kk)+
     .         (directmatrix(jj,kk)*effective_area(det+1,1)*
     .         exp(detangles(det+1)*(-1.0)*effective_area(det+1,2)))    
	  enddo	! do kk = 1, 16
        enddo	! do jj = 1, 70      
        return
        end 

	subroutine attenc(ene, angin, correction, ierror)

	implicit none
        double precision pi
        parameter (pi=3.141592653589793)
!	Get correction factors for LAD response matrix
!	This is an adaptation of Alan Harmon's IDL code by the same name.
	
	real*4		
     >			almuval,	! the value returned by al_mu
     >			ang(0:12),
     >			angin,		! the input angle of a detector
     >			correction,		! the correction factor computed
     >			ene,		! the input energy, midpt of a bin
     >			sl,
     >			teff(0:12),
     >			tint,
     >			val

	integer*4	
     >			i,
     >			ierror,
     >			jhi,
     >			jlo

	data teff /0.206404, 0.214839, 0.222772, 0.234522,
     >             0.241863, 0.250841, 0.263986, 0.279524,
     >             0.301512, 0.327302, 0.362299, 0.402002, 0.460953/
	do i = 0, 12
	  ang(i) = i * 5.0
	enddo

	jlo = -1 	! just an initialization for loop control
	do i = 12, 0, -1
	  if (ang(i) .le. angin)  jlo = i
	  if (jlo .ne. -1) goto 100	! end loop
	enddo

100	continue	! break from loop above

	if (jlo .eq. -1) then
c	  write(6, 900)
900	  format(' attenc --- ')
c          write(6, 600)  
600	  format(5x,'Angle range invalid low, returning...')
	  correction = 1
	  ierror = 0
	  return
	endif
	
	jhi = -1
	do i = 0, 12
	  if (ang(i) .ge. angin) jhi = i
	  if (jhi .ne. -1) goto 200	! end loop
	enddo

200	continue	! break from loop above

	if ((jhi .eq. -1) .and. (angin .gt. 90.)) then
c	  write(6, 900)
c	  write(6, 610)
610	  format(5x,'angle range invalid hi, returning...')
	  correction = 1
	  ierror = 0
	  return
	endif

	if ((jhi .eq. -1) .and. (angin .gt. 60) .and.
     &     (angin .lt. 90)) then
	  correction = 1.0
	  return
	endif

	if (jlo .eq. jhi) then
	  tint = teff(jlo)
	else
	  sl = (teff(jhi) - teff(jlo)) / (ang(jhi) - ang(jlo))
	  tint = teff(jlo) + sl * (angin - ang(jlo))
	endif

	call al_mu(ene, almuval)
	val = cos(angin*pi/180.)
	correction = exp((-1.0) * almuval * tint) / 
     >           exp((-1.0) * almuval * .23 / val)
	correction = correction * correction

	return
	end

	subroutine al_mu(er, almuval)

	implicit none

!	This program is an adaptation of an IDL program of like name that Alan
!	Harmon wrote.
!	Interpolates a value for the mass attenuation coefficient of Aluminum
!       given energy in keV between 15 and 3--- keV
!	From Storm and Israel, Sec A, Nucl Data Tables 7, No. 6, 1970, p. 580
!	energy in keV


	real*4
     >			almuval,
     >			en(0:30),
     >			er,
     >			sl,
     >			tot(0:30)
	integer*4	
     >			i,
     >			ihi,
     >			ilo

	en(0) =  5.
	en(1) =  6.
	en(2) =  8.
	en(3) =  10.
	en(4) =  15.
	en(5) =  20.
	en(6) =  30.
	en(7) =  40.
	en(8) =  50.
	en(9) =  60.
	en(10) =  80.
	en(11) =  100.
	en(12) =  150.
	en(13) =  200.
	en(14) =  300.
	en(15) =  400.
	en(16) =  500.
	en(17) =  600.
	en(18) =  800.
	en(19) =  1000.
	en(20) =  1500.
	en(21) =  2000.
	en(22) =  3000.
	en(23) =  4000.
	en(24) =  5000.
	en(25) =  6000.
	en(26) =  8000.
	en(27) =  10000.
	en(28) =  15000.
	en(29) =  20000.
	en(30) =  30000.

	tot(0) = 8510.
	tot(1) =  5070.
	tot(2) =  2210.
	tot(3) = 1180.
	tot(4) =  349.
	tot(5) =  152.
	tot(6) =  49.7
	tot(7) =  25.2
	tot(8) =  16.4
	tot(9) =  12.4
	tot(10) =  9.03
	tot(11) =  7.62
	tot(12) =  6.17
	tot(13) =  5.48
	tot(14) =  4.67
	tot(15) =  4.15
	tot(16) =  3.78
	tot(17) =  3.49
	tot(18) =  3.06
	tot(19) =  2.75
	tot(20) =  2.24
	tot(21) =  1.93
	tot(22) =  1.59
	tot(23) =  1.39
	tot(24) =  1.27
	tot(25) =  1.19
	tot(26) =  1.09
	tot(27) =  1.04
	tot(28) =  .981
	tot(29) =  .968
	tot(30) =  .980


!	..... .022320 converts from barns/atom to cm^2/g
	do i = 0, 30
	  tot(i) = tot(i) * .022320
	enddo


	ilo = -1 	! just an initialization for loop control
	do i = 30, 0, -1
	  if (en(i) .le. er)  ilo = i
	  if (ilo .ne. -1) goto 100	! end loop
	enddo

100	continue	! break from loop above

	if (ilo .eq. -1) then
c	  write(6, 900)
900	  format(' al_mu --- ')
c          write(6, 600)  
600	  format(5x,'invalid energy low, returning...')
	  almuval = -1
	  return
	endif

	ihi = -1
	do i = 0, 30
	  if (en(i) .ge. er) ihi = i
	  if (ihi .ne. -1) goto 200	! end loop
	enddo

200	continue	! break from loop above

	if (ihi .eq. -1)  then
c	  write(6, 900)
c	  write(6, 610)
610	  format(5x,'invalid energy high, returning...')
	  almuval = -1
	  return
	endif

	if (ihi .eq. ilo) then
	  almuval = tot(ihi)
	else
	  sl = (alog(tot(ihi)) - alog(tot(ilo))) / 
     >           (alog(en(ihi)) - alog(en(ilo)))
	  almuval = exp(alog(tot(ilo)) + sl * (alog(er) - alog(en(ilo))))
        endif


	return
	end
        
C ROUTINE TO DEAL WITH BURST AND GEOCENTER ANGLES
C IN THEIR CURRENT MODE OF EXPRESSION 
C AND TO CONVERT THEM TO DIRECTION VECTORS IN GRO COORDS
C version 2.0 of the matrix integrator code
C G.N.Pendleton 3/24/93
      SUBROUTINE GET_DIRECTIONS_angles(THETBIN, PHIBIN, dtbang)
!	This is an adaptation of Geoff Pendleton's code that computes the 
!	angles of the detectors in the response_matrix suite of code. 
      implicit none
      real*4 THETBIN,PHIBIN

      real*4 BURSTX,BURSTY,BURSTZ,
     *   UD(3,3,0:7), DTBANG(8) 
      integer*4   IDT(8) 

      DATA IDT/0,1,2,3,4,5,6,7/
      COMMON/DETGRO/UD
c local variables:
      integer*4 i
      real*4 Thetab,PHIB,THETABR,PHIBR
c      IDET=0
      THETAB=90.-THETBIN
      PHIB=PHIBIN
      THETABR=THETAB*2.*3.1415926/360.
      PHIBR=PHIB*2.*3.1415926/360.
      BURSTX=SIN(THETABR)*COS(PHIBR)
      BURSTY=SIN(THETABR)*SIN(PHIBR)
      BURSTZ=COS(THETABR)

c      WRITE(6,*) '          '
c      WRITE(6,*) ' THETBIN=',THETBIN,' PHIBIN=',PHIBIN
c      WRITE(6,*) '          '

      DO I=1,8
         DTBANG(I)=360./(2.*3.1415926)*ACOS(BURSTX*UD(1,1,IDT(I))+
     *               BURSTY*UD(1,2,IDT(I))+BURSTZ*UD(1,3,IDT(I)))
      ENDDO
      RETURN
      END                              
