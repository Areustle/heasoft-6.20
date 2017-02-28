      
        subroutine put_sis_event (iunit, k, event_time, 
     &       event_record, status)

        implicit none

        integer iunit, k
        double precisionevent_time
        integer event_record(*)
        integer ival, inull, anyf, status
        
        double precision dval

        include 'asca_defs.inc'
        include 'asca_common.inc'
        
C     ival = event_record(x_raw)
C     call ftpcli(iunit, x_raw_col, k, 1, 1, ival, status)
        
C     ival = event_record(y_raw)
C     call ftpclj(iunit, y_raw_col, k, 1, 1, ival, status)
        
        ival = event_record(x_det)
        call ftpclj(iunit, x_det_col, k, 1, 1, ival, status)
        
        ival = event_record(y_det)
        call ftpclj(iunit, y_det_col, k, 1, 1, ival, status)
        
        ival = event_record(x_sky)
        call ftpclj(iunit, x_sky_col, k, 1, 1, ival, status)
        
        ival = event_record(y_sky)
        call ftpclj(iunit, y_sky_col, k, 1, 1, ival, status)
        
c      ival = event_record(pha)
c      call ftpclj(iunit, pha_col, k, 1, 1, ival, status)
        
        if (datamode .ne. FAINT_mode) then
           ival = event_record(phai)
           call ftpclj(iunit, pi_col, k, 1, 1, ival, status)
        end if

C     dval = event_time
C     call ftpcld(iunit, time_col, k, 1, 1, dval, status)
        
        end
