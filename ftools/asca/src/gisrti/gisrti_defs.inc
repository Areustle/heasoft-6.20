C  $Header: /headas/headas/ftools/asca/src/gisrti/gisrti_defs.inc,v 3.6 1996/04/16 23:34:11 dunfee Exp $
C          
C
C       ASCA_DEFS.INC:  ASCALIN DEFINITION INCLUDE FILE
C
C       ERIC GOTTHELF 1992, 
C
C       LAST UPDATE APR 1994.
C

C       FITS COLUMN VARIABLES 

        integer x_raw_col, y_raw_col, c_raw_col, x_det_col, y_det_col, 
     &          x_sky_col, y_sky_col, pha_col, pi_col, time_col, 
     &          rt_col, rti_col, spread_col, gis_cols, sis_cols

        integer event_x_raw, event_y_raw, event_x_scl, event_y_scl
        integer event_x_det, event_y_det, event_pha, event_ph, event_pi

C       GENERAL VARIABLES

        integer nhdr
        double precision tstart, tstop

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

        integer axlen1, axlen2, q_col, atime_col, nattitude
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
        real plate_x_scale, plate_y_scale, focal_length, det_rotation
        double precision misalign(3,3), imisalign(3,3)

C       GAIN HISTORY CALIBRATION VARIABLES

C       max number of GH rec for an evt file

        integer ghmax
        parameter (ghmax = 5000)

        integer gh_records, gh_hvl(ghmax), gh_hvh(ghmax)
        real gh_iron(ghmax), gh_rise(ghmax), gh_temp(ghmax)
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
        real sis_gain
c (sis_size, sis_size)
        real nrows, ncols, row_width, col_width, rgap, cgap
        real r_width, c_width, rdead, cdead

C       MODE VARIABLES

        integer pos_meth, pos_size, pha_size, rise_size, spread_size
	integer rise_scale 
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








