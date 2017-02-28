C******************************************************************************
C SUBROUTINE:
C
C     gis_ele2det
C
C DESCRIPTION:
C     Calibrates spatial, spectral, and risetime raw data. Creates linearize 
C     GIS spatial image, gain corrects spectral data, applies spatial 
C     corrections for risetime channels. Creates aspected sky image.
C
C AUTHOR/DATE:
C       Eric Gotthelf    Aug 1993
C	NASA/GSFC
C
C MODIFICATION HISTORY:
C       Added a term for the attitude correction: KM, March 2005
C
C USAGE:
C     call gis_ele2det(iunit, aunit, fixed_asp, nobgd, verbose, status)
C
C ARGUMENTS:
C   input:
C	iunit           - data file unit
C	aunit           - attitude file unit
C	pointing        - user selected method to compute pointing
C	fixedasp        - apply aspect solution to sky image?
C	nobgd           - diagnostic (should always be .TRUE.)
C       verbose         - flag to write out information
C
C   output:
C       status         - fitsio status
C
C******************************************************************************

      subroutine gis_ele2det(iunit, aunit, fixed_asp, nobgd, verbose, 
     &     status)
        
        implicit none
        
        include 'asca_defs.inc'
        
        integer iunit, aunit, status
        logical fixed_asp, nobgd, verbose

        integer i, j, ip, jp, id, jd
        integer pibin, gis_pi, pha_bin, rt_bin, rt2rti, rti_bin
        integer x_pix1, y_pix1, event_record(gis_cols)
        integer asp, quality1, quality2
        integer event_start, asp_start, event, count, counts, skycnt
        real x, y, dx, dy, dx2, dy2, sx, sy, tx, ty, fx, fy, rx, ry
        real ax, ay, cx, cy, cx_off, cy_off
        real det_x_offset, det_y_offset, xyoffset, phascale, random, g
        real rtscale, x_flip, y_flip, cos_ang, sin_ang
        real roll, rolldeg, angle, cos_roll, sin_roll, theta, phi
        real delx, dely, ra_off, dec_off, active2
        real min_x_size, min_y_size, max_x_size, max_y_size
        real ra, dec, delxavg, delyavg, delxsig, delysig
        real delta_ra, delta_dec, ra_nom_rad, dec_nom_rad
        double precision tscale, tzero, rollsum, rollsum2
        double precision delxsum, delysum, delxsum2, delysum2
        double precision event_time, asp_time, asp_time1, asp_time2
        double precision q(4), q1(4), q2(4), slope, last_event
        character(80) info
        logical error

        include 'asca_common.inc'
        
C     intialize local variables:
        

        asp = 1
        event = 1
        count = 0
        skycnt = 0
        status = 0
        asp_start = 0
        error = .FALSE.
        event_start = 0
        last_event = -1.0d0

        delxsum = 0.0D0
        delysum = 0.0D0
        delxsum2 = 0.0D0
        delysum2 = 0.0D0
        rollsum = 0.0D0
        rollsum2 = 0.0D0

        equinox = 2000.0

        g = 4.0

        tzero  = 0.0D0
        tscale = 1.0D0

        x_flip = +1.0
        y_flip = -1.0

        x_pix1 = int(det_x_pix1)
        y_pix1 = int(det_y_pix1)
        
        axlen1 = int(det_x_size)
        axlen2 = int(det_y_size)

        crpix1 = det_x_center
        crpix2 = det_y_center

        ra_nom_rad = ra_nom * deg_to_rad
        dec_nom_rad = dec_nom * deg_to_rad
        
        cdelt1 = -plate_x_scale * det_x_scale / 60.0
        cdelt2 =  plate_y_scale * det_y_scale / 60.0

        cos_ang = cos(det_rotation * deg_to_rad) 
        sin_ang = sin(det_rotation * deg_to_rad) 
        
        det_x_offset = det_x_center * det_x_scale
        det_y_offset = det_y_center * det_y_scale
        
        xyscale =  float(gis_size) / float(pos_size)
        xyoffset = 0.5 * xyscale
        phascale = float(gis_chan) /  float(pha_size)

        max_x_size = int(det_x_size)
        max_y_size = int(det_y_size)

        min_x_size = int(det_x_pix1)
        min_y_size = int(det_y_pix1)
        
        active2 = fov_x_size*fov_y_size/det_x_scale/det_y_scale/4.0

c       Added three new initializations - KM, 2001 Dec
        cos_roll = 0.0
        sin_roll = 0.0
        asp_time1 = 0.0D0
        asp_time2 = 0.0D0

        if (aunit .le. 0) fixed_asp = .TRUE.

C     start:
        
C     check if valid file:
        
        if (.not.(datamode.eq.PH_mode .or. datamode.eq.PH2_mode)) then
           call fcerr('Error: data mode not supported')
           status = 1
           return
        end if
        
        do i=1, tfields
           call fttscl(iunit, i, tscale, tzero, status)
        end do
        if (status .ne. 0) call 
     &       fcerr('Error resetting TSCALE/TZERO values')

        call gis_record (iunit, event_start, event, event_time, 
     &       event_record, status)
        
        if (fixed_asp) then

           call euler2xrt(misalign, ea_phi, ea_theta, ea_psi, crval1, 
     &          crval2, rolldeg, error)
           roll = rolldeg * deg_to_rad
           cos_roll = cos(roll) 
           sin_roll = sin(roll)            
           if (error) then 
              call fcerr('Error: EULER2XRT: phi/psi inconsistency')
              status = 1
              return
           end if
           nattitude = n_events
           asp_time2 = tstop + 32.0d0
           quality2 = 0
        else
           
           call asp_record (aunit, asp_start, asp, asp_time2, q2, 
     &          quality2, status)          
           do while (asp_time2 .lt. event_time .and. asp_start .ne. -1
     &          .and. status .eq. 0)
              asp_time1 = asp_time2
              quality1 = quality2
              do i = 1, 4
                 q1(i) = q2(i)
              end do
              call asp_record (aunit, asp_start, asp, asp_time2, q2, 
     &             quality2, status)
              
           end do
           
        end if
           
        do while (event_start .ne. -1 .and. status .eq. 0)

c           do while (asp_start .ne. -1 .and. event_start .ne. -1
c     &          .and. status .eq. 0)
              
           do while (event_time .lt. asp_time2 .and. event_start .ne.
     &          -1 .and. status .eq. 0)
              
              i=max(min(event_record(x_raw),gis_size-1),0)
              j=max(min(event_record(y_raw),gis_size-1),0)

              x=(float(i)+random(iseed)) * xyscale
              y=(float(j)+random(iseed)) * xyscale

              tx=tr(1)+tr(2)*x+tr(3)*y
              ty=tr(4)+tr(5)*x+tr(6)*y
              
              sx=(tx+det_x_offset)/det_x_scale
              sy=(ty+det_y_offset)/det_y_scale
              
              ip=max(min(int(sx),gis_size),1)
              jp=max(min(int(sy),gis_size),1)
              
              dx=deltax(ip,jp)
              dy=deltay(ip,jp)
              
              id=max(min(ip+1,gis_size),1)
              jd=max(min(jp+1,gis_size),1)
              
              if(tx.lt.0.)id=max(min(ip-1,gis_size),1)
              if(ty.lt.0.)jd=max(min(jp-1,gis_size),1)
              
              dx2=g*(abs(deltax(id,jp)-dx))*(0.5-random(iseed))
              dy2=g*(abs(deltay(ip,jd)-dy))*(0.5-random(iseed))
              
              rx=(tx+dx+dx2)*cos_ang-(ty+dy+dy2)*sin_ang
              ry=(tx+dx+dx2)*sin_ang+(ty+dy+dy2)*cos_ang
              
              fx=((rx*x_flip/det_x_scale)+det_x_center)
              fy=((ry*y_flip/det_y_scale)+det_y_center)
              
              if (aunit .gt. 0) then
                 
              event_record(x_det)=max(min(int(fx/xyscale)+x_pix1,
     &                gis_size),x_pix1)
              event_record(y_det)=max(min(int(fy/xyscale)+y_pix1,
     &                gis_size),y_pix1)
                 
                 rt_bin=max(min(event_record(rt),rise_size-1),0)
                 event_record(rti)=rt_bin
                 if(rti_size.gt.1)event_record(rti)=
     &                max(min(int(rt2rti(sx,sy,rt_bin)),
     &                rti_size-1),0)
                 
              end if
              
              pha_bin=max(min(event_record(pha),pha_size-1),0)
              event_record(phai)=gis_pi(pha_bin,ip,jp,event_time,fx,fy)
              
c             write(*,'(6(1x,i5),2(1x,f10.2))') 
c     &             event_record(x_raw),event_record(y_raw),i,j,
c     &             event_record(x_det),event_record(y_det), fx, fy
              
              cx_off=fx-crpix1
              cy_off=fy-crpix2
              
              ax=fx-det_x_center
              ay=fy-det_y_center

              if (.not. nobgd .or. ax*ax+ay*ay .le. active2) then
                 
                 if (fixed_asp) then
                    
                    cx=cx_off*cos_roll+cy_off*sin_roll+crpix1
                    cy=-cx_off*sin_roll+cy_off*cos_roll+crpix2
                    
                 else
                    
                    do i = 1, 4
                       slope = (q2(i)-q1(i))/(asp_time2-asp_time1)
                       q(i) = (event_time-asp_time1)*slope+q1(i)
                    end do
                    
                    call aberration (event_time,ra_nom_rad,dec_nom_rad,
     &                   delta_ra, delta_dec, sun_long)
                    
                    ra = ra_nom_rad + delta_ra
                    dec = dec_nom_rad + delta_dec
                    
C                   Added the term for the attitude correction: KM, March 2005
                    call rd2sat (ra, dec, q, theta, phi, roll, error)
                    delta_ra =
     &          (gdetxoff * cos(roll) + gdetyoff * sin(roll)) / cos(dec)
                    delta_dec = gdetxoff * sin(roll)
     &                                            - sdetyoff * cos(roll)
                    ra = ra + delta_ra
                    dec = dec + delta_dec
                    
                    call rd2sat (ra, dec, q, theta, phi, roll, error)
                    
                    delx=theta*cos(phi)/cdelt1/deg_to_rad
                    dely=theta*sin(phi)/cdelt2/deg_to_rad
                    
                    cx_off = cx_off - delx
                    cy_off = cy_off - dely
                    
                    cx= cx_off*cos(roll)+cy_off*sin(roll)+crpix1
                    cy=-cx_off*sin(roll)+cy_off*cos(roll)+crpix2
                    
                    delxsum = delxsum+delx 
                    delysum = delysum+dely
                    rollsum = rollsum+roll
                    delxsum2= delxsum2+dble(delx)*dble(delx)
                    delysum2= delysum2+dble(dely)*dble(dely)
                    rollsum2= rollsum2+dble(roll)*dble(roll)
                 
                    skycnt = skycnt + 1
                    
                 end if
                 
                 if (aunit .gt. 0) then
                    event_record(x_sky)=max(min(int(cx/xyscale)+x_pix1,
     &                   axlen1),x_pix1)
                    event_record(y_sky)=max(min(int(cy/xyscale)+y_pix1,
     &                   axlen2),y_pix1)
                 end if
                 
              else
                 
                 if (aunit .gt. 0) then
                    event_record(x_sky)=1
                    event_record(y_sky)=1
                 end if

              end if
              
              call put_gis_event (iunit, event-1, event_time, 
     &             event_record, status)
              
              last_event = event_time
              
              call gis_record (iunit, event_start, event, event_time, 
     &             event_record, status)
              
              if (last_event .gt. event_time) then
                 write(info, '(a16,i8,a20,f20.8)') 
     &                'WARNING: event# ', event-1, 
     &                ' out of time order: ',
     &                event_time
                 call fcerr(info)
              end if
              
           end do
                
           if (.not. fixed_asp) then
              if (asp_start .ne. -1) then
                 asp_time1 = asp_time2
                 quality1 = quality2
                 do i = 1, 4
                    q1(i) = q2(i)
                 end do
                 call asp_record (aunit, asp_start, asp, asp_time2, q2, 
     &                quality2, status)
              else
                 asp_time1 = asp_time2
                 asp_time2 = event_time + 1.0d0
              end if
           end if
        end do

C     calculate and echo means and standard deviations

        count = event - 1

        if (aunit .gt. 0 .and. skycnt .gt. 1 .and. 
     &       delxsum .ne. 0.0d0 .and. delysum .ne. 0.0d0 .and. 
     &          rollsum .ne. 0.0d0 ) then

           delxavg = delxsum  / skycnt
           delyavg = delysum  / skycnt
           rollmean = rollsum  / deg_to_rad  / skycnt 
           delxsig =sqrt(abs(delxsum2-delxsum*delxsum/skycnt) /
     &          (skycnt - 1))
           delysig =sqrt(abs(delysum2-delysum*delysum/skycnt) /
     &          (skycnt - 1))
           rollsig =sqrt(abs(rollsum2-rollsum*rollsum/skycnt) /
     &          (skycnt - 1)) / deg_to_rad

        else

           rollmean = roll / deg_to_rad
           delxavg = 0.0
           delyavg = 0.0
           delxsig = 0.0
           delysig = 0.0
           rollsig = 0.0

        end if

        if (rollmean .le. 0.0) rollmean = rollmean + 360.0

        roll = rollmean * deg_to_rad
        decmean = (-delxavg*sin(roll) + delyavg*cos(roll)) * 
     &       cdelt2 + crval2
        if ( abs(cos(decmean*deg_to_rad)) .gt. 0.0001) then
           ramean  = ( delxavg*cos(roll) + delyavg*sin(roll)) * 
     &          cdelt1 / cos(decmean*deg_to_rad) + crval1
        else
           ramean = 0.0
        end if
        rasig  = abs(( delxsig*cos(roll) + delysig*sin(roll))*cdelt1)
        decsig = abs((-delxsig*sin(roll) + delysig*cos(roll))*cdelt2)

        optical_x_axis=x_flip*optical_x_axis/det_x_scale+det_x_center
        optical_y_axis=y_flip*optical_y_axis/det_y_scale+det_y_center
        cx_off=optical_x_axis-crpix1-delxavg
        cy_off=optical_y_axis-crpix2-delyavg
        optical_x_sky= cx_off*cos(roll)+cy_off*sin(roll)+crpix1
        optical_y_sky=-cx_off*sin(roll)+cy_off*cos(roll)+crpix2
        
c     check whether to do this before or after converting to sky mean!

        det_x_size   = det_x_size / xyscale
        det_y_size   = det_y_size / xyscale
        det_x_scale  = det_x_scale * xyscale
        det_y_scale  = det_y_scale * xyscale
        det_x_center = det_x_center / xyscale
        det_y_center = det_y_center / xyscale
        det_x_offset = det_x_offset / xyscale
        det_y_offset = det_y_offset / xyscale

        optical_y_sky  = optical_y_sky  / xyscale
        optical_x_sky  = optical_x_sky  / xyscale
        optical_y_axis = optical_y_axis / xyscale
        optical_x_axis = optical_x_axis / xyscale

        axlen1 = int(det_x_size)
        axlen2 = int(det_y_size)
        crpix1 = det_x_center
        crpix2 = det_y_center 
        cdelt1 = cdelt1 * xyscale
        cdelt2 = cdelt2 * xyscale

        if (verbose) then 

           if (aunit .gt. 0) then

              if (fixed_asp) then
                 
                 call fcecho (' ')
                 write (info, 1009) ramean, decmean, rollmean
                 call fcecho (info)
              
              else 
              
                 call fcecho (' ')
                 call fcecho
     &                (' MEAN ASPECT AND OFFSET FOR FILE EVENTS:')
                 call fcecho (' ')      
                 write (info, 1007) delxavg, delyavg
                 call fcecho (info)
                 write (info, 1008) delxsig, delysig
                 call fcecho (info)
                 call fcecho (' ')      
                 write (info, 1009) ramean, decmean, rollmean
                 call fcecho (info)
                 write (info, 1010) rasig, decsig, rollsig
                 call fcecho (info)
              
              end if   

 1000         format (' Number of EVENT records processed  = ', I10)
              
 1007         format (' Binned RA/DEC offset means (pix) : ', 
     &             2(1x,f14.8))
 1008         format (' Binned RA/DEC offset sigma (pix) : ', 
     &             2(1x,f14.8))
 1009         format (' Binned RA/DEC/ROLL (degs) : ', 3(1x,f14.8))
 1010         format (' Binned RA/DEC/ROLL (sigma): ', 3(1x,f14.8))
                            
           end if   
           
           call fcecho (' ')
           write(info, 1000) count
           call fcecho (info)
           
        end if
        
        end

