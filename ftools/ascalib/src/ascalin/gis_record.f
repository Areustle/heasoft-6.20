C******************************************************************************
C SUBROUTINE:
C
C      gis_record
C        
C DESCRIPTION:
C
C      get gis record from open fits file using buffer I/O
C     
C AUTHOR/DATE:
C
C       Eric Gotthelf, Aug 1993
C       ASCA GOF, NASA/GSFC.
C
C MODIFICATION HISTORY:
C       
C NOTES:
C
C USAGE:
C
C     call gis_record (iunit, first, row, event_time, event_record, status)
C
C ARGUMENTS:
C
C     iunit     - I/O unit for opened data file
C     verbose   - whether to print out diagnostic info
C     status    - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C     fitsio variables - hdtype, rows, filenm, xtensn, rwmode, block, rowlen
C                        vardat, fcstln, crpix1, crpix2, tfield, exact
C                        tbcol, ttype, tform, tunit, extnam, comment,  keywd
C
C CALLED ROUTINES:
C
C      subroutine fcecho - echo message to terminal
C      subroutine ftxxxx - FITSIO calls
C
C******************************************************************************

        subroutine gis_record (iunit, first, row, event_time, 
     &       event_record, status)
        
        implicit none
        
        include 'asca_defs.inc'

        integer iunit, first, row, status
        integer event_record(gis_cols)
        double precision event_time 
        
c     Local:
        
        integer block_size, record_size, buffered_recs, records
        parameter(block_size     = 2880)
        parameter(record_size    = 30)
        parameter(buffered_recs  = 96)
        parameter(records = buffered_recs)
        
        double precision dnull, evt_atime(records)
        integer evt_x_sky(records)
        integer evt_y_sky(records)
        integer evt_pha(records)
        integer evt_phai(records)
        integer evt_rt(records)
        integer evt_rti(records)
        integer evt_sp(records)
        integer evt_x_raw(records)
        integer evt_y_raw(records)
        integer evt_x_det(records)
        integer evt_y_det(records)
        
        integer count, total, index, inull
        integer nrecords
        logical anyf
        save count, total, nrecords,evt_atime
	save evt_x_sky, evt_y_sky,evt_pha,evt_phai,evt_rt,evt_rti
	save evt_sp,evt_x_raw,evt_y_raw,evt_x_det,evt_y_det
        
        include 'asca_common.inc'
        
c     Start:
        
c     for the first call, set the pointer for the file:
        
        if (first .eq. 0) then
           first = 1
           count = records
           total = records
           nrecords = records
        end if
        
c     at the end of buffer, read next one:
        
        if (count .eq. total) then

c     partial buffer means the end of file:
           
           if (total .lt. records .or. row .gt. n_events) then
              first = -1
              return
           end if
           
           count = 0
           
           if (row+records-1.gt.n_events) 
     &          nrecords = mod(n_events,records)
           
c     otherwise, reset COUNT, and read the buffer:
           
           call ftgcvd(iunit, time_col, row, 1, nrecords, dnull, 
     &          evt_atime, anyf, status)
           
           call ftgcvj(iunit, x_sky_col, row, 1, nrecords, inull, 
     &          evt_x_sky, anyf, status)
           
           call ftgcvj(iunit, y_sky_col, row, 1, nrecords, inull, 
     &          evt_y_sky, anyf, status)
           
           call ftgcvj(iunit, pha_col, row, 1, nrecords, inull, 
     &          evt_pha,anyf, status)
           
           call ftgcvj(iunit, pi_col, row, 1, nrecords, inull, 
     &          evt_phai, anyf, status)

           call ftgcvj(iunit, rt_col, row, 1, nrecords, inull, 
     &          evt_rt, anyf, status)
           
           call ftgcvj(iunit, spread_col, row, 1, nrecords, inull, 
     &          evt_sp, anyf, status)

           call ftgcvj(iunit, x_raw_col, row, 1, nrecords, inull, 
     &          evt_x_raw, anyf, status)
           
           call ftgcvj(iunit, y_raw_col, row, 1, nrecords, inull, 
     &          evt_y_raw, anyf, status)
           
           call ftgcvj(iunit, x_det_col, row, 1, nrecords, inull, 
     &          evt_x_det, anyf, status)
           
           call ftgcvj(iunit, y_det_col, row, 1, nrecords, inull, 
     &          evt_y_det, anyf, status)
           
           call ftgcvj(iunit, rti_col, row, 1, nrecords, inull, 
     &          evt_rti, anyf, status)
           
c     for the last buffer, START is the number of words read:
           
           total = nrecords
           
        end if
        
c     if there is something left in the buffer, fill the record:

        count = count + 1
        row = row + 1
        
        event_time          = evt_atime(count)
        event_record(x_sky) = evt_x_sky(count)
        event_record(y_sky) = evt_y_sky(count)
        event_record(pha)   = evt_pha(count)
        event_record(phai)  = evt_phai(count)
        event_record(rt)    = evt_rt(count)
        event_record(sp)    = evt_sp(count)
        event_record(x_raw) = evt_x_raw(count)
        event_record(y_raw) = evt_y_raw(count)
        event_record(x_det) = evt_x_det(count)
        event_record(y_det) = evt_y_det(count)
        event_record(rti)   = evt_rti(count)
        
        return
        
        end
      
      
