        
        
        subroutine parse_giscal_header(iunit, cal_time, calxo, calyo,
     &       status)
        
        implicit none
        
        include 'asca_defs.inc'
        
        integer calxo, calyo, iunit, status
        character*(*) cal_time
        
        integer i, j
        real det_x_offset, det_y_offset
        
        real num
        character(80) comment, key
        
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
        call ftgkye(iunit, 'OPTAXISX', optical_x_axis, comment,status)
        call ftgkye(iunit, 'OPTAXISY', optical_y_axis, comment,status)
        
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

        call ftgkyj(iunit, 'CAL_XO', calxo, comment, status)
        call ftgkyj(iunit, 'CAL_YO', calyo, comment, status)
        call ftgkye(iunit, 'CAL_KEV', cal_kev, comment, status)
        call ftgkye(iunit, 'CAL_ADU', cal_adu, comment, status)
        call ftgkye(iunit, 'CAL_NORM', cal_norm, comment, status)
        call ftgkye(iunit, 'CALRATIO', cal_ratio, comment, status)
        call ftgkye(iunit, 'RAN_SEED', ran_seed, comment, status)
        iseed = int(ran_seed)

        rti_off = 0.0
        rti_size = 0
        if (status .eq. 0) then
           rti_size = 256
           call ftgkye(iunit, 'RTI_OFF', rti_off, comment, status)
           if (status .eq. 202) then 
              status = 0
              rti_off = 119.0
           end if
        end if
        
        det_x_offset = det_x_center * det_x_scale
        det_y_offset = det_y_center * det_y_scale
        
        if (status .eq. 0) then
           
           if (pos_meth .eq. FLF) then
              
              do i= 1, 6
                 tr(i) = tr2(i)
              end do
              
           else
              
              do i= 1, 6
                 tr(i) = tr1(i)
              end do
              
           end if
           
C calculate inverse matrix:
           
           num = tr(6)*tr(2) - tr(3)*tr(5)
           
           if (num .ne. 0.0) then
              
              itr(1) = (tr(4)*tr(3) - tr(1)*tr(6)) / num
              itr(2) = tr(6) / num
              itr(3) = -tr(3) / num
              
              itr(4) = (tr(1)*tr(5) - tr(4)*tr(2)) / num
              itr(5) = -tr(5) / num
              itr(6) = tr(2) / num

           else
              
              call fcerr (
     &             ' ERROR: PARSE_CAL_DATA: Determinant is singular')
              
              status = 1
              
              itr(1) = det_x_offset
              itr(2) = det_x_scale
              itr(3) = 0.0
              
              itr(4) = det_y_offset
              itr(5) = 0.0
              itr(6) = det_y_scale
              
           end if

        end if

        end
