        
        subroutine parse_sis_cal_hdr(iunit, cal_time, status)

        implicit none
        
        integer iunit, status
        character(80) cal_time

        integer i, j
        character(80) key, comment

        include 'asca_defs.inc'
        include 'asca_common.inc'
        
        call ftgkys(iunit, 'DATE', cal_time, comment, status)
        if (status .eq. 202) then
           status = 0
           cal_time = 'NOT AVAILABLE'
        end if

        call ftgkye(iunit, 'FC_X_SCL', plate_x_scale, comment, status)
        call ftgkye(iunit, 'FC_Y_SCL', plate_y_scale, comment, status)
        call ftgkye(iunit, 'DET_ROTD', det_rotation, comment, status)
        call ftgkye(iunit, 'FOCALLEN', focal_length, comment, status)
        call ftgkye(iunit, 'OPTAXISX', optical_x_axis, comment, status)
        call ftgkye(iunit, 'OPTAXISY', optical_y_axis, comment, status)

        do i = 1, 3
           do j = 1, 3
              write(key, '(a6,i1,i1)') 'ALIGNM', i, j
              call ftgkyd(iunit, key, misalign(i, j), comment, status)
              imisalign(j, i) = misalign(i, j)
           end do
        end do
        
        call ftgkye(iunit, 'DET_SCAL', xyscale, comment, status)
        call ftgkye(iunit, 'DET_XSIZ', det_x_size, comment, status)
        call ftgkye(iunit, 'DET_XCEN', det_x_center, comment, status)
        call ftgkye(iunit, 'DETXPIX1', det_x_pix1, comment, status)
        call ftgkye(iunit, 'DET_XSCL', det_x_scale, comment, status)
        call ftgkye(iunit, 'DET_YSIZ', det_y_size, comment, status)
        call ftgkye(iunit, 'DET_YCEN', det_y_center, comment, status)
        call ftgkye(iunit, 'DETYPIX1', det_y_pix1, comment, status)
        call ftgkye(iunit, 'DET_YSCL', det_y_scale, comment, status)

        call ftgkye(iunit, 'FOV_X_MM', fov_x_size, comment, status)
        call ftgkye(iunit, 'FOV_Y_MM', fov_y_size, comment, status)

        call ftgkye(iunit, 'N_ROWS', nrows, comment, status)
        call ftgkye(iunit, 'N_COLS', ncols, comment, status)
        call ftgkye(iunit, 'ROW_SIZE', row_width , comment, status)
        call ftgkye(iunit, 'COL_SIZE', col_width , comment, status)
        call ftgkye(iunit, 'ROW_GAP', rgap , comment, status)
        call ftgkye(iunit, 'COL_GAP', cgap , comment, status)
        call ftgkye(iunit, 'R_WIDTH', r_width , comment, status)
        call ftgkye(iunit, 'C_WIDTH', c_width , comment, status)
        call ftgkye(iunit, 'R_DEAD', rdead , comment, status)
        call ftgkye(iunit, 'C_DEAD', cdead , comment, status)

        call ftgkye(iunit, 'COE_X0_A', tr0(1), comment, status)
        call ftgkye(iunit, 'COE_X0_B', tr0(2), comment, status)
        call ftgkye(iunit, 'COE_X0_C', tr0(3), comment, status)
        call ftgkye(iunit, 'COE_Y0_A', tr0(4), comment, status)
        call ftgkye(iunit, 'COE_Y0_B', tr0(5), comment, status)
        call ftgkye(iunit, 'COE_Y0_C', tr0(6), comment, status)

        call ftgkye(iunit, 'COE_X1_A', tr1(1), comment, status)
        call ftgkye(iunit, 'COE_X1_B', tr1(2), comment, status)
        call ftgkye(iunit, 'COE_X1_C', tr1(3), comment, status)
        call ftgkye(iunit, 'COE_Y1_A', tr1(4), comment, status)
        call ftgkye(iunit, 'COE_Y1_B', tr1(5), comment, status)
        call ftgkye(iunit, 'COE_Y1_C', tr1(6), comment, status)

        call ftgkye(iunit, 'COE_X2_A', tr2(1), comment, status)
        call ftgkye(iunit, 'COE_X2_B', tr2(2), comment, status)
        call ftgkye(iunit, 'COE_X2_C', tr2(3), comment, status)
        call ftgkye(iunit, 'COE_Y2_A', tr2(4), comment, status)
        call ftgkye(iunit, 'COE_Y2_B', tr2(5), comment, status)
        call ftgkye(iunit, 'COE_Y2_C', tr2(6), comment, status)

        call ftgkye(iunit, 'COE_X3_A', tr3(1), comment, status)
        call ftgkye(iunit, 'COE_X3_B', tr3(2), comment, status)
        call ftgkye(iunit, 'COE_X3_C', tr3(3), comment, status)
        call ftgkye(iunit, 'COE_Y3_A', tr3(4), comment, status)
        call ftgkye(iunit, 'COE_Y3_B', tr3(5), comment, status)
        call ftgkye(iunit, 'COE_Y3_C', tr3(6), comment, status)
        call ftgkye(iunit, 'RAN_SEED', ran_seed, comment, status)
        iseed = int(ran_seed)

        end
