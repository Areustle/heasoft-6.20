C
C       ASCA_COMMON.INC:  ASCALIN COMMON BLOCK INCLUDE FILE
C
C       ERIC GOTTHELF 1992, 
C
C       LAST UPDATE DEC 1994.
C
        common /operation_mode/ pos_meth, pos_size, pha_size, 
     &       rise_size, spread_size, dettype, detector, 
     &       bit_rate, datamode, n_events, time_col, x_det_col, 
     &       y_det_col, x_raw_col, y_raw_col, x_sky_col, y_sky_col, 
     &       c_raw_col, pha_col, pi_col, spread_col, rt_col, 
     &       rti_col, tfields, ccd_mode

        common /telescope/ optical_x_axis, optical_y_axis, 
     &       optical_x_sky, optical_y_sky, plate_x_scale, 
     &       plate_y_scale, focal_length, det_rotation

        common /misalignment/ misalign, imisalign,
     &       gdetxoff, gdetyoff, sdetxoff, sdetyoff

        common /transform/ tr0, tr1, tr2, tr3, 
     &       xyscale, image_radius, x_offset, y_offset, 
     &       det_x_scale, det_x_size, det_x_center, det_x_pix1,
     &       det_y_scale, det_y_size, det_y_center, det_y_pix1,
     &       fov_x_size, fov_y_size, rti_size, det_pi_size

        common /aspect/ axlen1, axlen2, nattitude, q_col, atime_col, 
     &	     qstat_col, crpix1, crpix2, crval1, crval2, cdelt1, cdelt2, 
     &       equinox, euler, sun_long, sun_source, ea_phi, ea_theta,
     &	     ea_psi, ra_nom, dec_nom, roll_nom, ramean, rasig, decmean,
     &	     decsig, rollmean, rollsig, raavg, decavg, rollavg, 
     &       offsetavg, offsetsig, offsetmean
     
        common /aspect_time/ astart, astop

        common /start_stop/ tstart, tstop

        common /cal/ iseed, ran_seed, cal_kev, cal_adu, cal_norm, 
     &       cal_ratio, cal_xo, cal_yo, rti_off, rt_off, rt_scale,
     &       tr, itr, cal_size, gis_gain, deltax, deltay, rt_map,
     &       sis_gain, chip_gain, chip_gain_off, sis_off, sis_b2l,
     &	     sis_l2b, n_ph2pi

        common /gain_history/ gh_records, 
     &       gh_hvl, gh_hvh, gh_temp, gh_iron, gh_rise,
     &       gh_c0, gh_c1, gh_c2, gh_c3, gain_chips,
     &       gain_renorm, gain_offset, gmapc_dim, gmapc_coeff


        common /gain_history_time/ gh_stop, gh_start

        common /sis_ccd/ nrows, ncols, row_width, col_width, rgap, cgap, 
     &        r_width, c_width, rdead, cdead

        common /sis_ob_params/ ccds, grade, sis_ob

	data sisob /'_AEPOW','_AEANL','_DESTA','BIASHV','_ARENA',
     &	'_LVENA','_LVDU0','_LVDU1','_LVDU2','_LVDU3','_LVDL0','_LVDL1',
     &		 '_LVDL2','_LVDL3','_ARIO0','_ARIO1','_ARIO2','_ARIO3',
     &		 '_STAH0','_STAH1','_STAH2','_STAH3','_ENDH0','_ENDH1',
     &		 '_ENDH2','_ENDH3','BIASOF','CCDMOD','_STAV0','_STAV1',
     &		 '_STAV2','_STAV3','_ENDV0','_ENDV1','_ENDV2','_ENDV3',
     &		 '_EVTR0','_EVTR1','_EVTR2','_EVTR3','_SPTR0','_SPTR1',
     &		 '_SPTR2','_SPTR3'/





