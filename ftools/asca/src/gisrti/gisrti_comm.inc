C  $Header: /headas/headas/ftools/asca/src/gisrti/gisrti_comm.inc,v 3.6 1996/04/16 23:34:08 dunfee Exp $
C          
C
C       ASCA_COMMON.INC:  ASCALIN COMMON BLOCK INCLUDE FILE
C
C       ERIC GOTTHELF 1992, 
C
C       LAST UPDATE APR 1994.
C
        common /operation_mode/ pos_meth, pos_size, pha_size, 
     &       rise_size, spread_size, dettype, detector, 
     &       bit_rate, datamode, n_events, time_col, x_det_col, 
     &       y_det_col, x_raw_col, y_raw_col, x_sky_col, y_sky_col, 
     &       c_raw_col, pha_col, pi_col, spread_col, rt_col, rti_col

        common /telescope/ optical_x_axis, optical_y_axis, 
     &       optical_x_sky, optical_y_sky, plate_x_scale, 
     &       plate_y_scale, focal_length, det_rotation

        common /misalignment/ misalign, imisalign

        common /transform/ tr0, tr1, tr2, tr3, 
     &       xyscale, image_radius, x_offset, y_offset, 
     &       det_x_scale, det_x_size, det_x_center, det_x_pix1,
     &       det_y_scale, det_y_size, det_y_center, det_y_pix1,
     &       fov_x_size, fov_y_size, rti_size, det_pi_size

        common /aspect/ axlen1, axlen2, nattitude, q_col, atime_col,
     &       crpix1, crpix2, crval1, crval2, cdelt1, cdelt2, equinox,
     &       euler, sun_long, sun_source, ea_phi, ea_theta, ea_psi, 
     &	     ra_nom, dec_nom, roll_nom, ramean, rasig, decmean, decsig, 
     &	     rollmean, rollsig, raavg, decavg, rollavg, offsetavg, 
     &       offsetsig, offsetmean
     
        common /aspect_time/ astart, astop

        common /start_stop/ tstart, tstop

        common /cal/ iseed, ran_seed, cal_kev, cal_adu, cal_norm, 
     &       cal_ratio, cal_xo, cal_yo, rti_off, rt_off, rt_scale,
     &       tr, itr, cal_size, gis_gain, deltax, deltay, rt_map,
     &       sis_gain, chip_gain, chip_gain_off

        common /gain_history/ gh_records, 
     &       gh_hvl, gh_hvh, gh_temp, gh_iron, gh_rise

        common /gain_history_time/ gh_stop, gh_start

        common /sis_ccd/ nrows, ncols, row_width, col_width, rgap, cgap, 
     &        r_width, c_width, rdead, cdead
        



