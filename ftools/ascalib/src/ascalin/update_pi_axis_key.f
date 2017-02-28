C******************************************************************************
C SUBROUTINE:
C
C     update_pi_axis_key
C
C DESCRIPTION:
C     Update Event file PI keywords following successfull completion of 
C     ASCALIN/SISLIN processing.
C
C AUTHOR/DATE:
C       Eric Gotthelf    Aug 1993
C	NASA/GSFC
C
C MODIFICATION HISTORY:
C       Updated by Koji Mukai, April 2001.
C       Last updated by Koji Mukai, May 2002 --- the TLMIN/MAX keywords
C                for the PI column was not being added in.
C
C USAGE:
C     call update_sis_axis_key(data_unit, cal_name, cal_time,
C     &            gain_name, gain_time, launch, status)
C
C ARGUMENTS:
C   input:
C	data_unit       - data file unit
C	cal_name        - name of calibration file used to proccess data 
C	cal_time        - time stamp of calibration file used to proccess data 
C	gain_name       - name of temporal file used to proccess data
C	gain_time       - time stamp of temporal file used to proccess data
C	launch          - (for SIS) is the gain normalized to value at launch?
C
C   output:
C       status         - fitsio status
C
C******************************************************************************

        subroutine update_pi_axis_key(data_unit, cal_name, cal_time,
     &       gain_name, gain_time, launch, status)
        
        implicit none
        
        include 'asca_defs.inc'

        integer data_unit, status
        character*(*) cal_name, cal_time, gain_name, gain_time
        logical launch

        integer decimals, i
        character(160) comment
        character(40) taskname, cdate
        character(6) siscards(6), giscards(6), kword(6)
        
        common /task/ taskname

        include 'asca_common.inc'
        
        data siscards / 'SISPI1', 'SISPI2', 'SISPI1', 'SISPI2', 
     &       'SISPI3', 'SISPI4' /
        data giscards / 'GISPI1', 'GISPI2', 'GISPI3', 'GISPI4', 
     &       'GISPI5', 'GISPI6' /

        decimals = 8

        if (dettype .eq. GIS) then
           do i = 1, 6
              kword(i) = giscards(i)
           end do
        else
           do i = 1, 6
              kword(i) = siscards(i)
           end do           
        end if

        if (dettype .eq. GIS) then
          write(comment,'(a14, a12, a134)') taskname,
     &       ': CAL FILE: ', cal_name(1:135)
          call ftmkys (data_unit, kword(1), comment, ' ', status)
          if (status .eq. 202) then
             status = 0
             call ftpkls (data_unit, kword(1), comment, ' ', status)
          end if
        
          write(comment,'(a14, a23, a43)') taskname, 
     &       ': CAL FILE DATE STAMP: ', cal_time(1:43)
          call ftmkys (data_unit, kword(2), comment, ' ', status)
          if (status .eq. 202) then
             status = 0
             call ftpkys (data_unit, kword(2), comment, ' ', status)
          end if
        end if
        
        write(comment,'(a14, a13, a133)') taskname,
     &       ': GAIN FILE: ', gain_name(1:133)
        call ftmkys (data_unit, kword(3), comment, ' ', status)
        if (status .eq. 202) then
           status = 0
           call ftpkls (data_unit, kword(3), comment, ' ', status)
        end if
        
        write(comment,'(a14, a24, a42)') taskname,
     &       ': GAIN FILE DATE STAMP: ', gain_time(1:42)
        call ftmkys (data_unit, kword(4), comment, ' ', status)
        if (status .eq. 202) then
           status = 0
           call ftpkys (data_unit, kword(4), comment, ' ', status)
        end if
        
        comment = 'Overall gain scale renorm factor (unity)'
        call modify_f_kw(data_unit, kword(5), gain_renorm, 
     &       decimals, comment, status)
        
        comment = 'Overall gain scale offset correction (keV)'
        call modify_f_kw(data_unit, kword(6), gain_offset,
     &       decimals, comment, status)

        call modifyn_j_kw(data_unit, 'TLMIN', pi_col, 0, 
     &       'First PI channel enumeration', status)
        
        call modifyn_j_kw(data_unit, 'TLMAX', pi_col, det_pi_size-1,
     &       'Last  PI channel enumeration', status)

        if (dettype .eq. SIS) then
           
           if (launch) then
              comment = 'SIS CTI/Gain corrected to launch value'
           else
              comment = 'SIS CTI/Gain corrected to observation epoch'
           end if
           
           call ftmkys (data_unit, 'SISPI5', comment, ' ', status)
           if (status .eq. 202) then
              status = 0
              call ftpkys (data_unit, 'SISPI5', comment, ' ', status)
           end if
           
           if( n_ph2pi .gt. 0 ) then
              comment =
     &         'New formula for SIS  -CCD mode CTI/gain correction used'
              comment( 21: 21 ) = char( 48 + n_ph2pi )
              call ftmkys (data_unit, 'SISPI6', comment, ' ', status)
              if (status .eq. 202) then
                 status = 0
                 call ftpkys (data_unit, 'SISPI6', comment, ' ', status)
              end if
           else
              call ftdkey(data_unit, 'SISPI6', status)
              if (status .eq. 202) status = 0
           end if

        end if

        if (status .ne. 0) then 
           comment = 'Error writing history cards'
           call fcerr(comment)
           status = 0
        end if
           
        if (status .ne. 0)
     &       call fcerr('Error updating PI keywords')

        if (status .eq. 0) then
           if (dettype .eq. SIS) then
              call ftdkey(data_unit, 'SISPI', status)
              if (status .eq. 202) status = 0
              call ftdkey(data_unit, 'SISLIN3', status)
              if (status .eq. 202) status = 0
              call ftdkey(data_unit, 'SISLIN4', status)
              if (status .eq. 202) status = 0
              call ftdkey(data_unit, 'SISPI7', status)
              if (status .eq. 202) status = 0
           else
              call ftdkey(data_unit, 'GISLIN3', status)
              if (status .eq. 202) status = 0
              call ftdkey(data_unit, 'GISLIN4', status)
              if (status .eq. 202) status = 0
           end if
        end if

        end

