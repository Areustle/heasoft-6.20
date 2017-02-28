C******************************************************************************
C SUBROUTINE:
C
C     update_sky_keywords
C
C DESCRIPTION:
C     Update Event file SKY keywords following successfull completion of 
C     ASCALIN processing.
C
C AUTHOR/DATE:
C       Eric Gotthelf    Aug 1993
C	NASA/GSFC
C
C MODIFICATION HISTORY:
C
C USAGE:
C     call update_sky_keywords(data_unit, cal_name, cal_time,
C     &     asp_name, asp_time, fixedasp, c_standard, t_standard, 
C     &     oldfrf, status)
C
C ARGUMENTS:
C   input:
C	data_unit       - data file unit
C	cal_name        - name of calibration file used to proccess data 
C	cal_time        - time stamp of calibration file used to proccess data 
C	asp_name        - name of attitude file used to proccess data]
C	asp_time        - time stamp of attitude file used to proccess data] 
C	fixedasp        - apply aspect solution to sky image?
C       c/t_standard    - which FITS keyword convention to use?
C	oldfrf          - is this data file created with an old FRFread
C
C   output:
C       status         - fitsio status
C
C******************************************************************************

      subroutine update_sky_keywords(iunit, cal_name, cal_time, 
     &     asp_name, asp_time, fixedasp, c_standard, t_standard, 
     &     oldfrf, status)

      implicit none

      character*(*) cal_name, cal_time, asp_name, asp_time
      integer iunit, status
      logical fixedasp, t_standard, c_standard, oldfrf

      include 'asca_defs.inc'

      character(160) contxt, keywd
      character(40) taskname, root

      include 'asca_common.inc'

      common /task/ taskname

      if (.not. oldfrf) then

         if (fixedasp) then

            write(contxt,'(a14, a45)')
     &        taskname, ': FIXED ASPECT PERFORMED USING EULER ANGLES: ' 
            call ftmkys (iunit, 'ASPECT1', contxt, ' ', status)
            if (status .eq. 202) then
               status = 0
               call ftpkys (iunit, 'ASPECT1', contxt, ' ', status)
            end if
            
            write(contxt, '(a14,a1, 10x,3(1x,f9.4))') taskname, ':',
     &           ea_phi, ea_theta, ea_psi
            call ftmkys (iunit, 'ASPECT2', contxt, ' ', status)
            if (status .eq. 202) then
               status = 0
               call ftpkys (iunit, 'ASPECT2', contxt, ' ', status)
               
            end if
            
         else
            
            write(contxt,'(a14, a12, a134)')
     &           taskname, ': ATT FILE: ', asp_name(1:134)
            call ftmkys (iunit, 'ASPECT1', contxt, ' ', status)
            if (status .eq. 202) then
               status = 0
               call ftpkls (iunit, 'ASPECT1', contxt, ' ', status)
            end if
            write(contxt,'(a14, a23, a43)')  
     &           taskname, ': ATT FILE DATE STAMP: ', asp_time(1:43)
            call ftmkys (iunit, 'ASPECT2', contxt, ' ', status)
            if (status .eq. 202) then
               status = 0
               call ftpkys (iunit, 'ASPECT2', contxt, ' ', status)
            end if
            
         end if
         
         call ftmkys (iunit, 'RADECSYS', 'FK5', '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = 'World Coordinate System'
            call ftpkys (iunit, 'RADECSYS', 'FK5', contxt, status)
         endif        
         
         call ftmkyf (iunit, 'EQUINOX', equinox, 6, '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = 'Equinox for coordinate system'
            call ftpkyf (iunit, 'EQUINOX', equinox, 6, contxt, status)
         endif        
         
         root = 'TCTYP'
         call catnum(keywd, root, x_sky_col)
         call ftmkys (iunit, keywd, 'RA---TAN', '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = 'Coordinate projection'
            call ftpkys (iunit, keywd, 'RA---TAN', contxt, status)
         endif        
         call catnum(keywd, root, y_sky_col)
         call ftmkys (iunit, keywd, 'DEC--TAN', '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Coordinate projection'
            call ftpkys (iunit, keywd, 'DEC--TAN', contxt, status)
         endif        
         
         call modifyn_f_kw(iunit, 'TCRPX', x_sky_col, crpix1, 6, 
     &        'Sky X axis reference pixel', status)
         
         call modifyn_f_kw(iunit, 'TCRPX', y_sky_col, crpix2, 6, 
     &        'Sky Y axis reference pixel', status)
         
         call modifyn_f_kw(iunit, 'TCRVL', x_sky_col, crval1, 6, 
     &        'Sky X coordinate at reference pixel (degrees)', status)
         
         call modifyn_f_kw(iunit, 'TCRVL', y_sky_col, crval2, 6,
     &        'Sky Y coordinate at reference  pixel (degrees)', status)
         
         call modifyn_f_kw(iunit, 'TCDLT', x_sky_col, cdelt1, 6,
     &        'Sky X pixel scale (degrees/pixel)', status)
         
         call modifyn_f_kw(iunit, 'TCDLT', y_sky_col, cdelt2, 6,
     &        'Sky Y pixel scale (degrees/pixel)', status)
         
         call modifyn_j_kw(iunit, 'TLMIN', x_sky_col, int(det_x_pix1),
     &        'Sky X first pixel enumeration', status)
         
         call modifyn_j_kw(iunit, 'TLMIN', y_sky_col, int(det_y_pix1),
     &        'Sky Y first pixel enumeration', status)
         
         call modifyn_j_kw(iunit, 'TLMAX', x_sky_col, axlen1, 
     &        'Sky X address space size', status)
         
         call modifyn_j_kw(iunit, 'TLMAX', y_sky_col, axlen2,
     &        'Sky Y address space size', status)
         
         keywd = 'RA_PNT'
         contxt = '&'
         call ftmkyf (iunit, keywd, raavg, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'File average value of RA (degrees)'
            call ftpkyf (iunit, keywd, raavg, 6, contxt, status)
         endif
         
         keywd = 'DEC_PNT'
         contxt = '&'
         call ftmkyf (iunit, keywd, decavg, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'File average value of DEC (degrees)'
            call ftpkyf (iunit, keywd, decavg, 6, contxt, status)
         endif
         
         keywd = 'PA_PNT'
         contxt = '&'
         call ftmkyf (iunit, keywd, rollavg, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'File average value of ROLL (degrees)'
            call ftpkyf (iunit, keywd, rollavg, 6, contxt, status)
         endif
         
         keywd = 'RA_PNTE'
         contxt = '&'
         call ftmkyf (iunit, keywd, rasig, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'File standard deviation of RA (degrees)'
            call ftpkyf (iunit, keywd, rasig, 6, contxt, status)
         endif
         
         keywd = 'DEC_PNTE'
         contxt = '&'
         call ftmkyf (iunit, keywd, decsig, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'File standard deviation of DEC (degrees)'
            call ftpkyf (iunit, keywd, decsig, 6, contxt, status)
         endif
         
         keywd = 'PA_PNTE'
         contxt = '&'
         call ftmkyf (iunit, keywd, rollsig, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'File standard deviation of ROLL (degrees)'
            call ftpkyf (iunit, keywd, rollsig, 6, contxt, status)
         endif

         keywd = 'RA__MEAN'
         contxt = '&'
         call ftmkyf (iunit, keywd, ramean, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'Mean pointing RA (degrees)'
            call ftpkyf (iunit, keywd, ramean, 6, contxt, status)
         endif
         
         keywd = 'DEC_MEAN'
         contxt = '&'
         call ftmkyf (iunit, keywd, decmean, 6, contxt, status)
         if (status .eq. 202) then
            status = 0
            contxt = 'Mean pointing DEC (degrees)'
            call ftpkyf (iunit, keywd, decmean, 6, contxt, status)
         endif
         
         call catnum(keywd, root, y_sky_col)
         call ftmkys (iunit, keywd, 'DEC--TAN', '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Coordinate projection'
            call ftpkys (iunit, keywd, 'DEC--TAN', contxt, status)
         endif

         call modifyn_f_kw(iunit, 'OPTIC', x_sky_col, 
     &        optical_x_sky, 6, 
     &        'File mean optical axis X in sky coords (pixels)', 
     &        status)
         
         call modifyn_f_kw(iunit, 'OPTIC', y_sky_col, 
     &        optical_y_sky, 6, 
     &        'File mean optical axis Y in sky coords (pixels)', 
     &        status)

         if (gdetxoff .ne. 0.0 .and. gdetyoff .ne. 0.0) then
            if (dettype .eq. GIS) then
               keywd = 'GDETXOFF'
               contxt  = 'GIS detX offset (arcmin)'
               call ftpkyf( iunit, keywd, gdetxoff, 5, contxt, status )
               keywd = 'GDETYOFF'
               contxt  = 'GIS detY offset (arcmin)'
               call ftpkyf( iunit, keywd, gdetyoff, 5, contxt, status )
            else if (dettype .eq. SIS) then
               keywd = 'SDETXOFF'
               contxt  = 'SIS detX offset (arcmin)'
               call ftpkyf( iunit, keywd, sdetxoff, 5, contxt, status )
               keywd = 'SDETYOFF'
               contxt  = 'SIS detY offset (arcmin)'
               call ftpkyf( iunit, keywd, sdetyoff, 5, contxt, status )
            end if
            contxt = 'Attitude correction (Gotthelf et al ' //
     &                                      '2000 ApJ 543 417) applied'
            call FTPHIS( iunit, contxt, status )
         end if
         
      else
         
         call ftmkys (iunit, 'TCTYP1 ', 'RA---TAN', '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Coordinate projection'
            call ftpkys (iunit, 'TCTYP1', 'RA---TAN', contxt, status)
         endif        
         call ftmkys (iunit, 'TCTYP2', 'DEC--TAN', '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Coordinate projection'
            call ftpkys (iunit, 'TCTYP2', 'DEC--TAN', contxt, status)
         endif        
         
         call ftmkyf (iunit, 'TCRPX1', crpix1, 6, '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' X axis ref. pixel'
            call ftpkyf (iunit, 'TCRPX1', crpix1, 6, contxt, status)
         endif
         call ftmkyf (iunit, 'TCRPX2', crpix2, 6, '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Y axis ref. pixel'
            call ftpkyf (iunit, 'TCRPX2', crpix2, 6, contxt, status)
         endif
         
         call ftmkyf (iunit, 'TCRVL1', crval1, 6, '&', status)
         if (status .eq. 202) then
            status = 0

            contxt = ' Sky coordinate (degrees) at X axis ref. pixel'
            call ftpkyf (iunit, 'TCRVL1', crval1, 6, contxt, status)
         endif
         call ftmkyf (iunit, 'TCRVL2', crval2, 6, '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Sky coordinate at Y axis ref. pixel (degrees) '
            call ftpkyf (iunit, 'TCRVL2', crval2, 6, contxt, status)
         endif
         
         call ftmkyf (iunit, 'TCRDLT1', cdelt1, 6, '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Increment at X axis ref. pixel (degrees) '
            call ftpkyf (iunit, 'TCRDLT1', cdelt1, 6, contxt, status)
         endif
         call ftmkyf (iunit, 'TCRDLT2', cdelt2, 6, '&', status)
         if (status .eq. 202) then
            status = 0
            contxt = ' Increment (degrees) at Y axis ref. pixel'
            call ftpkyf (iunit, 'TCRDLT2', cdelt2, 6, contxt, status)
         endif

         write(contxt,'(a11, a14, a12, a123)') 'HISTORY    ', 
     &        taskname, ': CAL FILE: ', cal_name(1:123)
         call ftmcrd(iunit, 'RESERVE3', contxt, status)
         if (status .eq. 202) then
            status = 0
            call ftmcrd(iunit, 'RESERVE5', contxt, status)
            if (status .eq. 202) then
               status = 0
               call ftphis (iunit, contxt(12:80), status)
            end if
         end if
         write(contxt,'(a11, a14, a23, a32)') 'HISTORY    ', 
     &        taskname, ': CAL FILE DATE STAMP: ', cal_time(1:32)
         call ftmcrd(iunit, 'RESERVE4', contxt, status)
         if (status .eq. 202) then
            status = 0
            call ftmcrd(iunit, 'RESERVE6', contxt, status)
            if (status .eq. 202) then
               status = 0
               call ftphis (iunit, contxt(12:80), status)
            end if
         end if

      end if
      
      end






