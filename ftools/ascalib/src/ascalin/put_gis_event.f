           
        subroutine put_gis_event (iunit, k, event_time, 
     &       event_record, status)
        
        integer iunit, k
        double precision event_time
        integer event_record(*)
        integer ival, ival2, status
        
        double precision dval
        
        include 'asca_defs.inc'
        include 'asca_common.inc'
        
C     ival = event_record(x_raw)
C     call ftpcli(iunit, x_raw_col, k, 1, 1, ival, status)
        
C     ival = event_record(y_raw)
C     call ftpclj(iunit, y_raw_col, k, 1, 1, ival, status)
        
        ival = event_record(phai)
        call ftpclj(iunit, pi_col, k, 1, 1, ival, status)

        ival = event_record(x_det)
        call ftpclj(iunit, x_det_col, k, 1, 1, ival, status)
        
        ival2 = event_record(y_det)
        call ftpclj(iunit, y_det_col, k, 1, 1, ival2, status)
        
        ival = event_record(x_sky)
        call ftpclj(iunit, x_sky_col, k, 1, 1, ival, status)
        
        ival = event_record(y_sky)
        call ftpclj(iunit, y_sky_col, k, 1, 1, ival, status)
        
        ival = event_record(rti)
        call ftpclj(iunit, rti_col, k, 1, 1, ival, status)
                
C     dval = event_time
C     call ftpcld(iunit, time_col, k, 1, 1, dval, status)
        
        end
