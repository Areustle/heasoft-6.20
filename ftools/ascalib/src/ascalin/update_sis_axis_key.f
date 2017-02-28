C******************************************************************************
C SUBROUTINE:
C
C     update_sis_axis_key
C
C DESCRIPTION:
C     Update Event file SIS keywords following successfull completion of
C     ASCALIN processing.
C
C AUTHOR/DATE:
C       Eric Gotthelf    Aug 1993
C	NASA/GSFC
C
C MODIFICATION HISTORY:
C       Jeff Guerber, RITSS/GSFC, 1999-03-02.  Enabled new-format date
C           in PI column history keyword.
C
C USAGE:
C     call update_sis_axis_key(data_unit, cal_name, cal_time,
C     &            [gain_name, gain_time,] oldfrf, status)
C
C ARGUMENTS:
C   input:
C	data_unit       - data file unit
C	cal_name        - name of calibration file used to proccess data
C	cal_time        - time stamp of calibration file used to proccess data
C[	gain_name       - name of temporal file used to proccess data]
C[	gain_time       - time stamp of temporal file used to proccess data]
C	oldfrf          - is this data file created with an old FRFread
C
C   output:
C       status         - fitsio status
C
C******************************************************************************

        subroutine update_sis_axis_key(data_unit, cal_name, cal_time,
     &       gain_name, gain_time, oldfrf, status)

        implicit none

        include 'asca_defs.inc'

        integer data_unit, status
        character*(*) cal_name, cal_time, gain_name, gain_time
        logical oldfrf

        integer decimals, timeref
        real gain_ev
        character(160) comment
        character(80) cdate
        character(40) taskname

        common /task/ taskname

        include 'asca_common.inc'

        decimals = 8

        if (oldfrf) then

           write(comment,'(a11, a12, a12, a125)') 'HISTORY    ',
     &          taskname, ': CAL FILE: ', cal_name(1:125)
           call ftmcrd(data_unit, 'RESERVE1', comment, status)
           if (status .eq. 202) then
              status = 0
              call ftphis (data_unit, comment(12:80), status)
           end if
           write(comment,'(a11, a12, a23, a34)') 'HISTORY    ',
     &          taskname, ': CAL FILE DATE STAMP: ', cal_time(1:36)
           call ftmcrd(data_unit, 'RESERVE2', comment, status)
           if (status .eq. 202) then
              status = 0
              call ftphis (data_unit, comment(12:80), status)
           end if

           write(comment,'(a11, a12, a13, a124)') 'HISTORY    ',
     &          taskname, ': GAIN FILE: ', gain_name(1:124)
           call ftmcrd(data_unit, 'RESERVE3', comment, status)
           if (status .eq. 202) then
              status = 0
              call ftphis (data_unit, comment(12:80), status)
           end if
           write(comment,'(a11, a12, a24, a33)') 'HISTORY    ',
     &          taskname, ': GAIN FILE DATE STAMP: ', gain_time(1:35)
           call ftmcrd(data_unit, 'RESERVE4', comment, status)
           if (status .eq. 202) then
              status = 0
              call ftphis (data_unit, comment(12:80), status)
           end if

        else

           write(comment,'(a14, a12, a134)') taskname,
     &          ': CAL FILE: ', cal_name(1:135)
           call ftmkys (data_unit, 'SISLIN1', comment, ' ', status)
           if (status .eq. 202) then
              status = 0
              call ftpkls (data_unit, 'SISLIN1', comment, ' ', status)
           end if

           write(comment,'(a14, a23, a43)') taskname,
     &          ': CAL FILE DATE STAMP: ', cal_time(1:43)
           call ftmkys (data_unit, 'SISLIN2', comment, ' ', status)
           if (status .eq. 202) then
              status = 0
              call ftpkys (data_unit, 'SISLIN2', comment, ' ', status)
           end if

c           write(comment,'(a14, a13, a133)') taskname,
c     &          ': GAIN FILE: ', gain_name(1:133)
c           call ftmkys (data_unit, 'SISLIN3', comment, ' ', status)
c           if (status .eq. 202) then
c              status = 0
c              call ftpkls (data_unit, 'SISLIN3', comment, ' ', status)
c           end if

c           write(comment,'(a14, a24, a42)') taskname,
c     &          ': GAIN FILE DATE STAMP: ', gain_time(1:42)
c           call ftmkys (data_unit, 'SISLIN4', comment, ' ', status)
c           if (status .eq. 202) then
c              status = 0
c              call ftpkys (data_unit, 'SISLIN4', comment, ' ', status)
c           end if

c           call ftmkys (data_unit, 'SISPI', taskname, ' ', status)
c           if (status .eq. 202) then
c              status = 0
c           end if

        end if

c        if (history) then
c        call GETDAT( cdate )
c        write(comment,'(a26,a11,a18)') 'PI column last updated on ',
c     &       cdate(1:11), ' using sis_pi v1.1'
c Uncomment the following 3 lines to change to new FITSIO...
        call ftgstm( cdate, timeref, status )
        write(comment,'(a26,a19,a18)') 'PI column last updated on ',
     &       cdate(1:19), ' using sis_pi v1.2'
*                                        updated, 2001 Mar.
        call FTPHIS( data_unit, comment, status )
c        end if

        if (status .ne. 0) then
           comment = 'Error writing history cards'
           call fcerr(comment)
           status = 0
        end if

        if (oldfrf) then

           call modify_j_kw(data_unit, 'AXLEN1', int(det_x_size),
     &          'Detector X address space size', status)

           call modify_j_kw(data_unit, 'AXLEN2', int(det_y_size),
     &          'Detector X address space size', status)

           call modify_j_kw(data_unit, 'DET_YSIZ', int(det_y_size),
     &          'Detector Y address space size', status)

           call modify_j_kw(data_unit, 'DET_XSIZ', int(det_x_size),
     &          'Detector X address space size', status)

           call modify_j_kw(data_unit, 'DET_YSIZ', int(det_y_size),
     &          'Detector Y address space size', status)

           call modify_f_kw(data_unit, 'DET_XCEN', det_x_center,
     &          decimals,
     &          'Detector X ref pixel (center of address space)',
     &          status)

           call modify_f_kw(data_unit, 'DET_YCEN', det_y_center,
     &          decimals,
     &          'Detector Y ref pixel (center of address space)',
     &          status)

           call modify_j_kw(data_unit, 'DETXPIX1', int(det_x_pix1),
     &          'Detector X first pixel enumeration', status)

           call modify_j_kw(data_unit, 'DETYPIX1', int(det_y_pix1),
     &          'Detector Y first pixel enumeration', status)

           call modify_f_kw(data_unit, 'DET_XSCL', det_x_scale,
     &          decimals, 'Detector X pixel scale (mm/pixel)', status)

           call modify_f_kw(data_unit, 'DET_YSCL', det_y_scale,
     &          decimals, 'Detector Y pixel scale (mm/pixel)', status)

        end if

        call modifyn_f_kw(data_unit, 'OPTIC', x_det_col,
     &       optical_x_axis, decimals,
     &       'Optical axis X in detector coords (pixels)', status)

        call modifyn_f_kw(data_unit, 'OPTIC', y_det_col,
     &       optical_y_axis, decimals,
     &       'Optical axis Y in detector coords (pixels)', status)

        call modifyn_j_kw(data_unit, 'TLMIN', x_det_col,
     &       int(det_x_pix1), 'Detector X first pixel enumeration',
     &       status)

        call modifyn_j_kw(data_unit, 'TLMIN', y_det_col,
     &       int(det_y_pix1), 'Detector Y first pixel enumeration',
     &       status)

        call modifyn_j_kw(data_unit, 'TLMIN', pi_col, 0,
     &       'First PI channel enumeration', status)

        call modifyn_j_kw(data_unit, 'TLMAX', x_det_col,
     &       int(det_x_size), 'Detector X address space size', status)

        call modifyn_j_kw(data_unit, 'TLMAX', y_det_col,
     &       int(det_y_size), 'Detector Y address space size', status)

        call modifyn_j_kw(data_unit, 'TLMAX', pi_col, det_pi_size-1,
     &       'Last  PI channel enumeration', status)

        call modifyn_f_kw(data_unit, 'TCRPX', x_det_col, det_x_center,
     &       decimals, 'Detector X ref pixel (center of address space)',
     &       status)

        call modifyn_f_kw(data_unit, 'TCRPX', y_det_col, det_y_center,
     &       decimals, 'Detector Y ref pixel (center of address space)',
     &       status)

        call modifyn_f_kw(data_unit, 'TCRVL', x_det_col, 0.0,
     &       decimals, 'Detector X ref pixel value (pixels)', status)

        call modifyn_f_kw(data_unit, 'TCRVL', y_det_col, 0.0,
     &       decimals, 'Detector Y ref pixel value (pixels)', status)

        call modifyn_f_kw(data_unit, 'TCDLT', x_det_col, det_x_scale,
     &       decimals, 'Detector X pixel scale (mm/pixel)', status)

        call modifyn_f_kw(data_unit, 'TCDLT', y_det_col, det_y_scale,
     &       decimals, 'Detector Y pixel scale (mm/pixel)', status)

        call modify_f_kw(data_unit, 'FOV_X_MM', fov_x_size, decimals,
     &       'Detector X field of view (mm)', status)

        call modify_f_kw(data_unit, 'FOV_Y_MM', fov_y_size, decimals,
     &       'Detector Y field of view (mm)', status)

        call modify_j_kw(data_unit, 'RAN_SEED', int(ran_seed),
     &       'Random number generator seed', status)

        gain_ev = sis_gain * 1000.0
        call modify_f_kw(data_unit, 'GAIN_NOM', gain_ev, decimals,
     &        'Nominal gain value (eV/channel) for PI column', status )

        if (status .ne. 0)
     &       call fcerr('Error updating detector keywords')

        end
