c Date: Tue, 30 Jul 1996 16:46:30 -0400
c From: Eric Gotthelf <evg@venus.gsfc.nasa.gov>
c To: dunfee@rosserv.gsfc.nasa.gov, evg@venus.gsfc.nasa.gov
c Subject: updated ascalib routine: sis_ele2det.f (I coulb not copy it into the dropbox)
c 
c $Id: sis_ele2det.f,v 3.12 2013/05/21 19:08:08 irby Exp $
c
c
        subroutine sis_ele2det(iunit, aunit, fixed_asp, verbose, status)
        
        implicit none
        
        include 'asca_defs.inc'
        
        integer iunit, aunit, status
        logical fixed_asp, verbose

        integer event_record(sis_cols)
        integer i, j, c, g, ip, jp, id, jd, pi_bin, pha_bin, counts
        integer event_start, asp_start, event, count, sis_pi
        integer asp, quality1, quality2
        integer x_pix1, y_pix1, skycnt
c       integer min_x_size, min_y_size, max_x_size, max_y_size,
        real x, y, dx, dy, dx2, dy2, sx, sy, tx, ty, fx, fy, rx, ry
        real cx, cy, cx_off, cy_off
        real det_x_offset, det_y_offset, xyoffset, phascale, random
        real x_flip, y_flip, cos_ang, sin_ang
        real roll, rolldeg, angle, cos_roll, sin_roll, theta, phi
        real delx, dely, ra_off, dec_off
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

        tzero  = 0.0D0
        tscale = 1.0D0

        x_flip =  +1.0
        y_flip =  -1.0

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

c        max_x_size = int(det_x_size)
c        max_y_size = int(det_y_size)

c        min_x_size = int(det_x_pix1)
c        min_y_size = int(det_y_pix1)
        
c       Added three new initializations - KM, 2001 Dec
        fx = 0.0
        fy = 0.0
        cos_roll = 0.0
        sin_roll = 0.0
        asp_time1 = 0.0D0
        asp_time2 = 0.0D0

C     start:
        
C     check if valid file:

        if (aunit .le. 0) fixed_asp = .TRUE.

        if (.not. (datamode .eq. FAINT_mode .or. datamode .eq. 
     &       BRIGHT_mode .or. datamode .eq. BRIGHT2_mode)) then
           call fcerr('ERROR: SIS_ELE2DET: non-imaging data mode!')
          status = 1
           return
        end if

        do i=1, tfields
           call fttscl(iunit, i, tscale, tzero, status)
        end do
        if (status .ne. 0) call 
     &       fcerr('Error resetting TSCALE/TZERO values')

        call sis_record (iunit, event_start, event, event_time, 
     &       event_record, status)
        
        if (fixed_asp) then
           
           call euler2xrt(misalign, ea_phi, ea_theta, ea_psi, crval1, 
     &          crval2, rolldeg, error)
           roll = rolldeg * deg_to_rad
           cos_roll = cos(roll) 
           sin_roll = sin(roll)            
           if (error) then 
              call fcerr('ERROR: EULER2XRT: phi/psi inconsistency')
              status = 1
              return
           end if
           asp_time2 = tstop + 32.0d0
           nattitude = n_events
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
        
c     do while (asp_start .ne. -1 .and. event_start .ne. -1
c     &       .and. status .eq. 0)

        do while (event_start .ne. -1 .and. status .eq. 0)
           
           do while (event_time .lt. asp_time2 .and. event_start .ne.
     &          -1 .and. status .eq. 0)
              
              i = event_record(x_raw)
              j = event_record(y_raw)
              c = event_record(c_raw)
                 
              call four_chip_image(i, j, c, x, y)
              
              sx = (x + 0.5 - random(iseed)) * xyscale - 
     &             det_x_center
              sy = (y + 0.5 - random(iseed)) * xyscale - 
     &             det_y_center 

              if (aunit .gt. 0) then
                 
                 rx = sx * cos_ang - sy * sin_ang
                 ry = sx * sin_ang + sy * cos_ang
                 
                 fx = (rx * x_flip) + det_x_center
                 fy = (ry * y_flip) + det_y_center
                 
                 event_record(x_det) = max(min(int(fx)+x_pix1,
     &                sis_size),x_pix1)
                 event_record(y_det) = max(min(int(fy)+y_pix1,
     &                sis_size),y_pix1)
                 
              end if

              if (datamode .ne. FAINT_mode) then
                 pha_bin = max(min(event_record(pha),pha_size-1),0)
                 event_record(phai)= sis_pi(pha_bin, i, j, c, g,
     &                event_time)
              end if

              cx_off = fx - crpix1
              cy_off = fy - crpix2
              
c     cx_off = optical_x_axis  - crpix1
c     cy_off = optical_y_axis  - crpix2
              
c     cx_off = fx - optical_x_axis 
c     cy_off = fy - optical_y_axis
              
              if (fixed_asp) then
                 
                 cx =  cx_off * cos_roll + cy_off * sin_roll + crpix1
                 cy = -cx_off * sin_roll + cy_off * cos_roll + crpix2
                 
              else                 
                 
                 do i = 1, 4
                    slope = (q2(i)-q1(i))/(asp_time2-asp_time1)
                    q(i) = (event_time-asp_time1)*slope+q1(i)
                 end do
                 
                 call aberration (event_time, ra_nom_rad, dec_nom_rad,
     &                delta_ra, delta_dec, sun_long)
                 
                 ra = ra_nom_rad + delta_ra
                 dec = dec_nom_rad + delta_dec
                 
C                Added the term for the attitude correction: KM, March 2005
                 call rd2sat (ra, dec, q, theta, phi, roll, error)
                 delta_ra =
     &          (sdetxoff * cos(roll) + sdetyoff * sin(roll)) / cos(dec)
                 delta_dec = sdetxoff * sin(roll) - sdetyoff * cos(roll)
                 ra = ra + delta_ra
                 dec = dec + delta_dec
                    
                 call rd2sat (ra, dec, q, theta, phi, roll, error)
                 
                 delx=theta*cos(phi)/cdelt1/deg_to_rad
                 dely=theta*sin(phi)/cdelt2/deg_to_rad

                 cx_off = cx_off - delx
                 cy_off = cy_off - dely
                 
                 cx=cx_off*cos(roll)+cy_off*sin(roll)+crpix1
                 cy=-cx_off*sin(roll)+cy_off*cos(roll)+crpix2

                 delxsum = delxsum+delx 
                 delysum = delysum+dely
                 rollsum = rollsum+roll
                 delxsum2= delxsum2+delx*delx
                 delysum2= delysum2+dely*dely
                 rollsum2= rollsum2+roll*roll
                 
                 skycnt = skycnt + 1

              end if
              
              if (aunit .gt. 0) then
                 event_record(x_sky)=max(min(int(cx)+x_pix1, 
     &                axlen1),x_pix1)
                 event_record(y_sky)=max(min(int(cy)+y_pix1, 
     &                axlen2),y_pix1)
              end if

              call put_sis_event (iunit, event-1, event_time, 
     &             event_record, status)
              
              last_event = event_time
              
              call sis_record (iunit, event_start, event, event_time, 
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

        if (aunit .gt. 0. .and. skycnt .gt. 1 .and. 
     &       delxsum .ne. 0.0d0 .and. delysum .ne. 0.0d0 .and. 
     &       rollsum .ne. 0.0d0 ) then
           
              delxavg = delxsum  / skycnt
              delyavg = delysum  / skycnt
              rollmean = rollsum / deg_to_rad / skycnt
              delxsig =sqrt(abs(delxsum2-delxsum*delxsum/skycnt) / 
     &             (skycnt - 1))
              delysig =sqrt(abs(delysum2-delysum*delysum/skycnt) / 
     &             (skycnt - 1))
              rollsig =sqrt(abs(rollsum2-rollsum*rollsum/skycnt) 
     &             / (skycnt - 1)) / deg_to_rad

        else

           rollavg = roll /  deg_to_rad
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
        if ( abs(cos(decmean*deg_to_rad) ) .gt. 0.0001) then
           ramean  = ( delxavg*cos(roll) + delyavg*sin(roll)) * 
     &         cdelt1 / cos(decmean*deg_to_rad) + crval1
        else
           ramean = 0.0
        end if
        
        rasig  = abs(( delxsig*cos(roll) + delysig*sin(roll))*cdelt1)
        decsig = abs((-delxsig*sin(roll) + delysig*cos(roll))*cdelt2)
        
        optical_x_axis=x_flip*optical_x_axis/det_x_scale+det_x_center
        optical_y_axis=y_flip*optical_y_axis/det_y_scale+det_y_center
        cx_off=optical_x_axis-delxavg-crpix1
        cy_off=optical_y_axis-delyavg-crpix2
        optical_x_sky= cx_off*cos(roll)+cy_off*sin(roll)+crpix1
        optical_y_sky=-cx_off*sin(roll)+cy_off*cos(roll)+crpix2
        
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
