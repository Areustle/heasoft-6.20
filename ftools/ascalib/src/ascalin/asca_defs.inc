C
C       ASCA_DEFS.INC:  ASCALIN DEFINITION INCLUDE FILE
C
C       ERIC GOTTHELF 1992,
C
C       LAST UPDATE JUN 1994.
C       Mod. Dec 1998: fixed gmapc_coeff dimension (5, not 4). JRG
C       Mod. Mar 2001: added an extra row to gh_cN and added gain_cNs
C                      for the new CTI forumla.  KM.
C

C       GENERAL CONSTANTS

        double precision pi, halfpi, twopi, epsilon, deg_to_rad
        parameter( pi = 3.1415926535898D0)
        parameter( halfpi = 1.5707963267949D0)
        parameter( twopi = 6.2831853071796D0)
        parameter( epsilon = 1.0D-12)
        parameter( deg_to_rad = 0.0174532925199D0)

C       GENERAL VARIABLES

        integer nhdr
        double precision tstart, tstop

C       FITS COLUMN VARIABLES

        integer x_raw_col, y_raw_col, c_raw_col, x_det_col,
     &          y_det_col, x_sky_col, y_sky_col, pha_col,
     &          pi_col, time_col, rt_col, rti_col, spread_col,
     &          gis_cols, sis_cols, tfields

        integer event_x_raw, event_y_raw, event_x_scl, event_y_scl
        integer event_x_det, event_y_det, event_pha, event_ph
        integer event_pi

C       TRANSFORMATION VARIABLES

        integer det_pi_size, rti_size
        real tr(6), itr(6)
        real tr0(6), tr1(6), tr2(6), tr3(6)
        real xyscale, image_radius, x_offset, y_offset
        real det_x_scale, det_y_scale
        real det_x_size, det_x_center, det_x_pix1
        real det_y_size, det_y_center, det_y_pix1
        real fov_x_size, fov_y_size

C       SKY VARIABLES

        integer axlen1, axlen2, q_col, atime_col, qstat_col
        integer nattitude
        real ea_phi, ea_theta, ea_psi, ra_nom, dec_nom, roll_nom
        real cdelt1, cdelt2, crpix1, crpix2, crval1, crval2
        real equinox, euler(3), sun_long, sun_source
        real ramean, rasig, decmean, decsig, rollmean
        real raavg, decavg, rollavg, rollsig
        real offsetavg, offsetsig, offsetmean
        double precision astart, astop

C       TELESCOPE CALIBRATION VARIABLES

        real optical_x_sky, optical_y_sky
        real optical_x_axis, optical_y_axis
        real plate_x_scale, plate_y_scale, focal_length
        real det_rotation
        double precision misalign(3,3), imisalign(3,3)
	real gdetxoff, gdetyoff, sdetxoff, sdetyoff

C       GAIN HISTORY CALIBRATION VARIABLES

C       max number of GH rec for an evt file

        integer ghmax
        parameter (ghmax = 5000)

        integer gmapc_dim, gh_records, gh_hvl(ghmax), gh_hvh(ghmax)
        integer n_ph2pi
        real gh_iron(ghmax), gh_rise(ghmax), gh_temp(ghmax)
        real gh_c0(ghmax,8), gh_c1(ghmax,8), gh_c2(ghmax,8)
        real gh_c3(ghmax,8), gmapc_coeff(5), gain_chips( 0: 3 )
        real gain_renorm, gain_offset
        double precision gh_stop(ghmax), gh_start(ghmax)

C       GIS CALIBRATION FILE VARIABLES

        integer gis_size, gis_chan

C       maximum number of gis spectral bins

        parameter (gis_chan = 1024)

C       interpolation image size isize X size

        parameter (gis_size = 256)

        real deltax(gis_size, gis_size), deltay(gis_size, gis_size)
        real gis_gain(gis_size, gis_size), rt_map(gis_size, gis_size)

        real cal_kev, cal_adu, cal_norm, cal_ratio
        real rti_off, rt_off, rt_scale, ran_seed
        integer cal_size, iseed, cal_xo, cal_yo

C       SIS CALIBRATION FILE VARIABLES

        integer sis_size, sis_chan, sis_num

C       maximum number of sis spectral bins

        parameter (sis_chan = 4096)

C       default sis address space size

        parameter (sis_size = 1280)

        real chip_gain(0:3), chip_gain_off(0:3)
        real sis_off, sis_gain
c (sis_size, sis_size)
        real sis_b2l(2)
        integer sis_l2b(2)
        real nrows, ncols, row_width, col_width, rgap, cgap
        real r_width, c_width, rdead, cdead

C       MODE VARIABLES

        integer pos_meth, pos_size, pha_size, rise_size, spread_size
        integer rise_scale, ccd_mode
        integer dettype, detector, bit_rate, datamode, n_events

C       integer inum

C       maximum number of calibration pts

C       parameter (inum = 1000)

C       INSTRUMENT INDEX

        integer GIS, SIS, SIS0, SIS1, SIS2, GIS2, GIS3

        parameter (GIS = 1)
        parameter (SIS = 2)

        parameter (SIS0 = 1)
        parameter (SIS1 = 2)
        parameter (SIS2 = 3)
        parameter (GIS2 = 4)
        parameter (GIS3 = 5)

C       POS_DET INDEX

        integer FLF, POW2

        parameter(FLF  = 1)
        parameter(POW2 = 2)

        parameter(gis_cols = 20)
        parameter(sis_cols = 20)

C       EVENT RECORD INDEX

        integer x_raw, y_raw, c_raw, x_det, y_det, x_sky, y_sky
        integer pha, phai, rt, rti, sp, time

        parameter(x_raw = 1)
        parameter(y_raw = 2)
        parameter(c_raw = 3)
        parameter(x_det = 4)
        parameter(y_det = 5)
        parameter(x_sky = 6)
        parameter(y_sky = 7)
        parameter(pha = 8)
        parameter(phai = 9)
        parameter(time = 10)
        parameter(rt = 11)
        parameter(rti = 12)
        parameter(sp = 13)

C       GIS DATA MODE INDEX

        integer PH_mode, PH2_mode, MPC_mode, MEMORY_mode, PCAL_mode

        parameter (PH_mode = 1)
        parameter (PH2_mode = 2)
        parameter (MPC_mode = 3)
        parameter (MEMORY_mode = 4)
        parameter (PCAL_mode = 5)

C       SIS MODE INDEX

        integer BRIGHT_mode, BRIGHT2_mode, FAINT_mode
        integer FAST_mode, FAST2_mode, FRAME_mode
        integer HISTOGRAM_mode, DARK_IMAGE_mode, INTEGRATION_mode
        integer NONOBS_mode

        parameter (FAINT_mode = 1)
        parameter (BRIGHT_mode = 2)
        parameter (BRIGHT2_mode = 3)
        parameter (FAST_mode = 4)
        parameter (FAST2_mode = 5)
        parameter (FRAME_mode = 6)
        parameter (HISTOGRAM_mode = 7)
        parameter (DARK_IMAGE_mode = 8)
        parameter (INTEGRATION_mode = 9)
        parameter (NONOBS_mode = 10)

        integer NA_mode
        parameter (NA_mode = 100)

C       BIT RATE INDEX

        integer HIGH_bit_rate, MEDIUM_bit_rate, LOW_bit_rate

        parameter (LOW_bit_rate = 1)
        parameter (MEDIUM_bit_rate = 16)
        parameter (HIGH_bit_rate = 32)

        integer NA_bit_rate
        parameter (NA_bit_rate = 1024)

        integer nsisob, nccd, ngrade, nsisfrf
        parameter (nccd = 4)
        parameter (ngrade = 8)
        parameter (nsisob = 44)
        parameter (nsisfrf = 40)

        integer ccds(nccd), grade(ngrade), sis_ob(nsisob)
        character(8) sisob(nsisob)

        integer AEPOW,AEANL,DESTA,BIASH,ARENA,LVENA
        integer LVDU0,LVDU1,LVDU2,LVDU3,LVDL0
        integer ARIO0,ARIO1,ARIO2,ARIO3,STAH0,STAH1
        integer STAH2,STAH3,LVDL1,LVDL2,LVDL3
        integer ENDH0,ENDH1,ENDH2,ENDH3,BIASO,CCDMO
        integer STAV0,STAV1,STAV2,STAV3,ENDV0
        integer ENDV1,ENDV2,ENDV3,EVTR0,EVTR1,EVTR2
        integer EVTR3,SPTR0,SPTR1,SPTR2,SPTR3

        parameter (AEPOW = 1)
        parameter (AEANL = 2)
        parameter (DESTA = 3)
        parameter (BIASH = 4)
        parameter (ARENA = 5)
        parameter (LVENA = 6)
        parameter (LVDU0 = 7)
        parameter (LVDU1 = 8)
        parameter (LVDU2 = 9)
        parameter (LVDU3 = 10)
        parameter (LVDL0 = 11)
        parameter (LVDL1 = 12)
        parameter (LVDL2 = 13)
        parameter (LVDL3 = 14)
        parameter (ARIO0 = 15)
        parameter (ARIO1 = 16)
        parameter (ARIO2 = 17)
        parameter (ARIO3 = 18)
        parameter (STAH0 = 19)
        parameter (STAH1 = 20)
        parameter (STAH2 = 21)
        parameter (STAH3 = 22)
        parameter (ENDH0 = 23)
        parameter (ENDH1 = 24)
        parameter (ENDH2 = 25)
        parameter (ENDH3 = 26)
        parameter (BIASO = 27)
        parameter (CCDMO = 28)
        parameter (STAV0 = 29)
        parameter (STAV1 = 30)
        parameter (STAV2 = 31)
        parameter (STAV3 = 32)
        parameter (ENDV0 = 33)
        parameter (ENDV1 = 34)
        parameter (ENDV2 = 35)
        parameter (ENDV3 = 36)
        parameter (EVTR0 = 37)
        parameter (EVTR1 = 38)
        parameter (EVTR2 = 39)
        parameter (EVTR3 = 40)
        parameter (SPTR0 = 41)
        parameter (SPTR1 = 42)
        parameter (SPTR2 = 43)
        parameter (SPTR3 = 44)
