C******************************************************************************
C SUBROUTINE:
C      mean_aspect
C
C DESCRIPTION:
C      compute attitude file averages.
C
C AUTHOR/DATE:
C       Eric Gotthelf    Aug 1993
C	NASA/GSFC
C
C MODIFICATION HISTORY:
C       Koji Mukai, 2002 April 23 - redirected ">15 min gap in attitude file"
C                                   message to stdout using fcecho
C
C USAGE:
C      call nominal_aspect(aunit, rauser, decuser, pointing, verbose, status)
C
C ARGUMENTS:
C   input:
C	aunit           - attitude file unit
C	rauser, decuser - user selected ra, dec pointing (degrees)
C	pointing        - user selected method to compute pointing
C       verbose         - flag to write out information
C
C   output:
C       status         - fitsio status
C       statistical information put into common block
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     
C******************************************************************************

        subroutine nominal_aspect(aunit, ounit, rauser, decuser, 
     &       slewmax, pointing, verbose, outfile, acmflag, status)
        
        implicit none
                
        integer aunit, ounit, status
        real rauser, decuser, slewmax
        character*(*) pointing
        logical verbose, outfile, acmflag
        
        include 'asca_defs.inc'

        integer arow, arowstart, start, count, i
        integer e1pcount,e1ncount,e2pcount,e2ncount,e3pcount,e3ncount
        integer rapcount, rancount, decpcount, decncount
        integer rollpcount, rollncount, offpcount, offncount
        integer quality, year, month, day, hour, min, acm, getbit
        real roll, rollmin, rollmax, sec, pointed, manuver, ratemax
        real delta_ra, delta_dec, delx, dely
        real x_sat(3),y_sat(3),z_sat(3),sun(3),aber(3)
        real theta, phi, ra, dec, ra_nom_rad, dec_nom_rad
        real sep, sepmin, lastra, lastdec, sep_ang_rad, rate, dt
        real ramin, ramax, decmin, decmax, offmin, offmax
        double precision e1sum, e2sum, e3sum
        double precision q(4), atime, atimelast, firsttime, lasttime
        double precision tstop_minus, tstop_plus
        double precision tstart_minus, tstart_plus
        double precision xyz(3,3), xrt(3,3)
        double precision boresight(3,3), euler1, euler2, euler3
        double precision rasum, decsum, rasum2, decsum2
        double precision rollsum, rollsum2
        double precision offset, offset2, offsig, offavg
        
        logical flgval, anyf, histry, exact, error, error2
        logical zerotime, gap

        character(160) contxt, datestr, timestr
        
        include 'asca_common.inc'


        
        if (verbose) then
           call fcecho (' ')
           write (contxt, 3001) astart, astop
           call fcecho (contxt)
           write (contxt, 3003) tstart, tstop
           call fcecho (contxt)
        end if

        acm = 0
        arow = 1
        start = 0
        atimelast = -1.0D0
        tstop_plus  = tstop + 32.0D0
        tstart_plus  = tstart + 32.0D0
        tstop_minus = tstop - 32.0D0
        tstart_minus = tstart - 32.0D0

C     check if this attitude file spans data file (allow some slack)
        
        if ( astart .gt. tstart_plus .or. astop .lt. tstop_minus ) then
           if ( astart .gt. tstart_plus) then
              contxt = 'Event start time preceeds attitude file'
           else
              contxt =
     &             'Event end time is not covered by attitude file'
           endif
           call fcerr (contxt)
           status = 1
           return
        end if
        
c     skip to starting record for this event file:
        
        call asp_record (aunit, start, arow, atime, q, quality, status)
        
        do while (atime .le. tstart .and. start .ne. -1 .and.
     &       status .eq. 0 .and. atime .lt. tstop)
           
           atimelast = atime
           call asp_record (aunit, start, arow, atime, q, quality, 
     &          status)
           
        end do
        
        if (status .ne. 0) then
           contxt = 'Error reading attitude file (1)'
           call fcerr (contxt)
        end if
        if (start .eq. -1 .and. atime .gt. tstop_plus) then
           contxt = 'End of attitude file before data end (1)'
           call fcerr (contxt)
           status = 1
        endif
        if (atimelast .gt. tstop) then
           contxt = 'Beginning of attitude file after data (1)'
           call fcerr (contxt)
           status = 1
        end if
        
        if (status .ne. 0) return
        
C     found the first time
        
        ratemax = slewmax * deg_to_rad

        count = 0
        phi = 0.0
        theta = 0.0
        
        e1sum = 0.0d0
        e2sum = 0.0d0
        e3sum = 0.0d0

        e1ncount = 0
        e1pcount = 0
        e2pcount = 0 
        e2ncount = 0
        e3pcount = 0
        e3ncount = 0
        e3pcount = 0

        rapcount = 0
        rancount = 0
        decpcount = 0
        decncount = 0

        rollpcount = 0
        rollncount = 0
        offpcount = 0
        offncount = 0

        rasum = 0.0d0
        decsum = 0.0d0
        rasum2 = 0.0d0
        decsum2 = 0.0d0

        ramin = 360.0
        ramax = -360.0
        decmin = 360.0
        decmax = -360.0

        start = 0
        gap = .FALSE.
        error = .FALSE.
        error2 = .FALSE.
        zerotime = .FALSE.
        arowstart = max(arow - 2, 1)
        arow = arowstart
        atime = -1.0D0
        
        call asp_record (aunit, start, arow, atime, q, quality,  
     &       status)


        call sat2rd (ra, dec, q, theta, phi, error)
        call dir_cos(q, xyz)
        call cosine2euler (xyz, euler1, euler2, euler3,
     &       error2)

        euler(1) = euler1
        euler(2) = euler2
        euler(3) = euler3
        
        pointed = 0.0
        manuver = 0.0
        lasttime  = atime
        firsttime = atime
        lastra = ra
        lastdec = dec


        do while (atime .le. tstop .and. start .ne. -1 .and.
     &       status .eq. 0)
           
           if (atime .ne. 0.0d0) then

              if (acmflag) then

                 phi = 0.0
                 theta = 0.0
                 call sat2rd (ra, dec, q, theta, phi, error)
                 sep=sep_ang_rad(ra,dec,lastra,lastdec)
                 dt = atime - lasttime

                 if (dt .gt. 0.0) then
                    rate = sep / dt
                    if (rate .gt. ratemax) then
                       acm=1
                    else
                       acm=getbit(quality, 11)
                    end if
                 else
                    acm=getbit(quality, 11)
                 end if
                 
                 lastra=ra
                 lastdec=dec
                 
              end if

              if (acm .eq. 0 .or. .not. acmflag) then
                 
c     write(104,*) arow, atime, status
                 
                 call dir_cos (q, boresight)
                 call cosine2euler (boresight, euler1, euler2, euler3,
     &                error2)
                 
c                 write(105,'(i5,5(1x,f))') arow, ra, dec, 
c     &                euler1, euler2, euler3
                 
                 e1sum  = e1sum + euler1
                 e2sum  = e2sum + euler2
                 e3sum  = e3sum + euler3

                 rasum  = rasum + dble(ra)
                 decsum = decsum + dble(dec)
                 rasum2  = rasum2 + dble(ra)*dble(ra)
                 decsum2 = decsum2 + dble(dec)*dble(dec)
                 
                 if (ra .lt. ramin) ramin = ra 
                 if (ra .gt. ramax) ramax = ra 
                 if (dec .lt. decmin) decmin = dec
                 if (dec .gt. decmax) decmax = dec 
                 
                 if (euler1 .gt. 0.0) then
                    e1pcount = e1pcount + 1
                 else
                    e1ncount = e1ncount + 1
                 end if
                 if (euler2 .gt. 0.0) then
                    e2pcount = e2pcount + 1
                 else
                    e2ncount = e2ncount + 1
                 end if
                 if (euler3 .gt. 0.0) then
                    e3pcount = e3pcount + 1
                 else
                    e3ncount = e3ncount + 1
                 end if
                 if (ra .gt. 0.0) then
                    rapcount = rapcount + 1
                 else
                    rancount = rancount + 1
                 end if
                 if (dec .gt. 0.0) then
                    decpcount = decpcount + 1
                 else
                    decncount = decncount + 1
                 end if

                 count = count + 1
                 
                 pointed = pointed + atime - lasttime
                 
              else
                 
                 manuver = manuver + atime - lasttime
                 
              end if

C     check the next time
              
              if (atime .gt. lasttime + 900.0d0) then
                 gap = .TRUE.
                 write(contxt,*) 
     &                'Detected gap > 15min in attitude file:', 
     &                lasttime 
                 call fcecho (contxt)
              end if
              
              lasttime = atime

           else

              zerotime = .TRUE.

           end if
           
           call asp_record (aunit, start, arow, atime, q, quality, 
     &          status)

        end do
        
        if (verbose) then
           write (contxt, 3002) firsttime, lasttime
           call fcecho (contxt)
        end if
        
        if (gap) then
           call fcecho (' ')
           contxt='Detected gap > 15min in attitude file:' 
           call fcecho (contxt)
c           contxt = 'aspect is suspect: result may not be valid'
c           call fcerr (contxt)
c          status = 1
        end if
        if (zerotime) then
           call fcecho (' ')
           contxt='Detected time value of zero in attitude file:' 
           call fcerr (contxt)
           contxt = 'aspect is suspect: result may not be valid'
           call fcerr (contxt)
c          status = 1
        end if
        if (error .or. error2) then
           contxt='Error in aspect calc (phi/psi inconsitency)' 
           call fcerr (contxt)
           if (error) then
              contxt = 'during RA/DEC computation: '//
     &             'result may not be valid'
           end if
           if (error2) then
              contxt = 'during Euler angle computation: '//
     &             'result may not be valid'
           end if
           call fcerr (contxt)
c          status = 1
        end if
        if (start .eq. -1 .and. atime .gt. tstop_plus) then
           contxt = 'End of attitude file before data end (2)'
           call fcerr (contxt)
           status = 1
        endif
        if (status .ne. 0) then
           contxt= 'Error reading attitude record (2)' 
           call fcerr (contxt)
        end if
        if (lasttime .gt. tstop) then
           contxt = 'Beginning of attitude file after data (2)'
           call fcerr (contxt)
           status = 1
        endif
        if (status .ne. 0)  return
        
C     and calculate the averages
        
        ramin = ramin / deg_to_rad
        ramax = ramax / deg_to_rad
        decmin = decmin / deg_to_rad
        decmax = decmax / deg_to_rad

        if (count .gt. 1) then
           if (e1pcount .ne. e1ncount) then
              euler(1) = e1sum / abs(e1pcount - e1ncount)
           else
              euler(1) = e1sum
           end if
           if (e2pcount .ne. e2ncount) then
              euler(2) = e2sum / abs(e2pcount - e2ncount)
           else
              euler(2) = e2sum
           end if
           if (e3pcount .ne. e3ncount) then
              euler(3) = e3sum / abs(e3pcount - e3ncount)
           else
              euler(3) = e3sum
           end if

           if (rapcount .ne. rancount) then
              raavg  = rasum / deg_to_rad / abs(rapcount - rancount) 
		
           else
              raavg  = rasum / deg_to_rad 
           end if

           if (decpcount .ne. decncount) then
              decavg = decsum / deg_to_rad / abs(decpcount - decncount)
           else
              decavg = decsum / deg_to_rad
           end if
           rasig  = dsqrt(abs(rasum2-rasum*rasum/count) / deg_to_rad / 
     &          (count-1))
           decsig = sqrt(abs(decsum2-decsum*decsum/count) / deg_to_rad /
     &          (count-1))
        else
           euler(1) = euler1
           euler(2) = euler2
           euler(3) = euler3
           raavg  = ra / deg_to_rad
           decavg = dec / deg_to_rad
        end if

        if (euler(1) .lt. 0.0) euler(1) = euler(1) + twopi
        if (euler(3) .lt. 0.0) euler(3) = euler(3) + twopi
        if (raavg  .lt. 0.0) raavg = raavg + 360.0D0
        if (ramin  .lt. 0.0) ramin = ramin + 360.0D0
        if (ramax  .lt. 0.0) ramax = ramax + 360.0D0

C     DETERMINE RANOM, DECNOM BASED ON THE USER'S POINTING REQUEST
        
        if (pointing .eq. 'ATT' .or. pointing .eq. 'att' .or.
     &       pointing .eq. 'MEAN' .or. pointing .eq. 'mean') then
           
C     reset the nominal ra and dec and write to file
           
           ra_nom = raavg
           dec_nom = decavg
           crval1 = raavg
           crval2 = decavg
           
        else if (pointing .eq. 'USER' .or. pointing .eq. 'user') then
           
C     user the rauser, decuser values
           
           ra_nom = rauser
           dec_nom = decuser
           crval1 = rauser
           crval2 = decuser

        else if (pointing .eq. 'KEY' .or. pointing .eq. 'key') then
           
C     user the ra_nom, dec_nom values
           
           crval1 = ra_nom
           crval2 = dec_nom

        endif
        
        atime = tstart+(tstop-tstart)/2.0d0
        call sun_sat_ang(atime, euler, x_sat, y_sat, z_sat, sun, aber)

        sun_long   = sun(3)
        sun_source = z_sat(3)

        if (verbose .and. status .eq. 0) then
           
           call fcecho (' ')
           write (contxt, 1001) lasttime - firsttime
           call fcecho (contxt)
           if (acmflag) then
              write (contxt, 1011) pointed, manuver
              call fcecho (contxt)
           end if

           if (euler(1) .lt. 0.0) euler(1) = euler(1) + twopi
           if (euler(3) .lt. 0.0) euler(3) = euler(3) + twopi
           if (sun(1) .lt. 0.0) sun(1) = sun(1) + twopi
           if (x_sat(1) .lt. 0.0) x_sat(1) = x_sat(1) + twopi
           if (y_sat(1) .lt. 0.0) y_sat(1) = y_sat(1) + twopi
           if (z_sat(1) .lt. 0.0) z_sat(1) = z_sat(1) + twopi
           
           call fcecho (' ')
           write (contxt, 1003) euler(1)/deg_to_rad,
     &          euler(2)/deg_to_rad, euler(3)/deg_to_rad
           call fcecho (contxt)

          call fcecho (' ')
          write (contxt, 5000) 
          call fcecho (contxt)
          call fcecho (' ')
          write (contxt, 5002) sun(1)/deg_to_rad,sun(2)/deg_to_rad
          call fcecho (contxt)
          write (contxt, 5003) aber(1)/deg_to_rad * 3600.0, 
     &          aber(2)/deg_to_rad * 3600.0
          call fcecho (contxt)
          call fcecho (' ')
          write (contxt, 5004) x_sat(1)/deg_to_rad, 
     &         x_sat(2)/deg_to_rad, x_sat(3)/deg_to_rad
          call fcecho (contxt)
          write (contxt, 5005) y_sat(1)/deg_to_rad, 
     &         y_sat(2)/deg_to_rad, y_sat(3)/deg_to_rad
          call fcecho (contxt)
          write (contxt, 5006) z_sat(1)/deg_to_rad, 
     &         z_sat(2)/deg_to_rad, z_sat(3)/deg_to_rad
          call fcecho (contxt)
          
           call fcecho (' ')
           write (contxt, 2000) 
           call fcecho (contxt)
           write (contxt, 2001) 
           call fcecho (contxt)

        end if
        
C     NOW FIGURE OUT MEAN OFFSET FOR THE NOMINAL POINTING AND GET ROLL ANGLE:
        
        count = 0
        start = 0
        error = .FALSE.
        atime = -1.0D0
        arow  = arowstart

        rollmin = 360.0 * deg_to_rad
        rollmax = -360.0 * deg_to_rad
        offmin  = 360.0 * deg_to_rad
        offmax  = -360.0 * deg_to_rad

        offset   = 0.0D0
        offset2  = 0.0D0
        rollsum  = 0.0D0
        rollsum2 = 0.0D0

        ra_nom_rad  = ra_nom * deg_to_rad
        dec_nom_rad = dec_nom * deg_to_rad
     
        if (outfile) then

           call ascatout(firsttime, year, month, day, hour,min,sec)
           write(datestr, 333) year, month, day
           write(timestr, 444) hour, min, int(sec)
 333       format(i4.4,'-',i2.2,'-',i2.2)
 444       format(i2.2,':',i2.2,':',i2.2)

           write(ounit, '(a,f8.3,a,f8.3,a17,f14.4,a6,a10,a1,a8)') 
     &          'LA T RA (J2000 deg): ', ra_nom, ' DEC: ', dec_nom,
     &          '; START TIME: SC ', firsttime,
     &          ' = UT ',datestr,' ',timestr
           write(ounit, '(a)') '!'
c           write(ounit, '(a)') '@plotasp
           write(ounit, '(a)') 'co on 7'
           write(ounit, '(a)') 'cs 0.5'
           write(ounit, '(a)') 'la y1 time'
           write(ounit, '(a)') 'la y2 EULER1 (deg)'
           write(ounit, '(a)') 'la y3 EULER2 (deg)'
           write(ounit, '(a)') 'la y4 EULER3 (deg)'
           write(ounit, '(a16,a1,a1)') 'la y5 X offset (',char(39),')'
           write(ounit, '(a16,a1,a1)') 'la y6 Y offset (',char(39),')'
           write(ounit, '(a)') 'la y7 SENSOR'
           write(ounit, '(a)') 'ti'
           write(ounit, '(a)') 'cs 0.75'
           write(ounit, '(a)') 'p v'
           write(ounit, '(a)') '!'
           write(ounit, '(a42,a29)') 
     &          '!    OFFSET TIME  EULER1   EULER2   EULER3 ',
     &          '   X offset   Y offset SENSOR'
           write(ounit, '(a42,a29)') 
     &          '!        (sec)     (deg)    (deg)    (deg) ',
     &          '   (arcmin)   (arcmin)  (hex)'
           write(ounit, '(a)') '!'

        end if

        call asp_record (aunit, start, arow, atime, q, quality, 
     &       status)
        
        phi = 0.0
        theta = 0.0
        call sat2rd (ra, dec, q, theta, phi, error)
        lastra=ra
        lastdec=dec
        lasttime=atime
        
        call asp_record (aunit, start, arow, atime, q, quality, 
     &       status)
        
        do while (atime .le. tstop .and. start .ne. -1 .and.  
     &       status .eq. 0 .and. .not. error) 
                      
           if (atime .ne. 0.0d0) then

              phi = 0.0
              theta = 0.0
              call sat2rd (ra, dec, q, theta, phi, error)
              sep=sep_ang_rad(ra,dec,lastra,lastdec)
              dt = atime - lasttime
              if (dt .gt. 0.0) then
                 rate = sep / dt
                 if (rate .ge. ratemax) then
                    call setbit(quality, 11)
                 end if
              end if

              lastra=ra
              lastdec=dec
              
              call rd2sat (ra_nom_rad, dec_nom_rad, q, theta, phi, 
     &             roll, error)

c              if (outfile .and. (acm .eq. 0 .or. .not.acmflag)) then
              if (outfile) then
                 
                 delx=dble(theta)*60.0d0*cos(dble(phi))/deg_to_rad
                 dely=dble(theta)*60.0d0*sin(dble(phi))/deg_to_rad
                 
                 call dir_cos (q, boresight)
                 call cosine2euler (boresight, euler1, euler2, euler3,
     &                error2)

c                 call sat2rd (ra, dec, q, theta, phi, error)
c                 write(110, '(6(1x,f10.4),i4)') ra_nom_rad, 
c     &                dec_nom_rad, ra, dec, delx, dely
                 
                 if (euler1 .lt. 0.0) euler1 = euler1 + twopi
                 if (euler3 .lt. 0.0) euler3 = euler3 + twopi
                 call write_aspect(ounit, real(atime-tstart)
     &                , real(euler1/deg_to_rad) 
     &                , real(euler2/deg_to_rad)
c     &                , theta, phi
     &                , real(euler3/deg_to_rad), delx, dely, quality)
                 
              end if

              offset = offset + theta
              offset2 = offset2 + dble(theta)*dble(theta)
              
              rollsum = rollsum + roll
              rollsum2 = rollsum2 + dble(roll)*dble(roll)
              
              if (roll .lt. rollmin) rollmin = roll
              if (roll .gt. rollmax) rollmax = roll
              if (theta .lt. offmin) offmin = theta
              if (theta .gt. offmax) offmax = theta
              
              if (roll .gt. 0.0) then
                 rollpcount = rollpcount + 1
              else
                 rollncount = rollncount + 1
              end if
              if (theta .gt. 0.0) then
                 offpcount = offpcount + 1
              else
                 offncount = offncount + 1
              end if

              count = count + 1

              lasttime=atime
              
           end if
           
C     check the next time
           
           call asp_record (aunit, start, arow, atime, q, quality, 
     &          status)
           
        end do
        

        if (error) then
           contxt='Error in aspect calc (phi/psi inconsitency)' 
           call fcerr (contxt)
           contxt = 'during ROLL/OFFSET computation: '//
     &          'result may not be valid'
           call fcerr (contxt)
        end if
        if (start .eq. -1 .and. atime .gt. tstop_plus) then
           contxt = 'End of attitude file before data end (3)'
           call fcerr (contxt)
           status = 1
        endif
        if (status .ne. 0) then
           contxt= 'Error reading attitude record (3)' 
           call fcerr (contxt)
        end if
        if (lasttime .gt. tstop ) then
           contxt = 'Beginning of attitude file after data (3)'
           call fcerr (contxt)
           status = 1
        endif
        if (count .eq. 0) then
           contxt = 'No overlapping attitude records for this data file'
           call fcerr (contxt)
           contxt = 'Problem with attitude file?'
           call fcerr (contxt)
           status = 1
        endif
        if (status .ne. 0) return
        
C     and calculate the averages
        
        rollmin = rollmin / deg_to_rad
        rollmax = rollmax / deg_to_rad
        offmin = 60.0 * offmin / deg_to_rad
        offmax = 60.0 * offmax / deg_to_rad

        if (count .gt. 1) then

           rollavg = rollsum / deg_to_rad / 
     &          abs(rollpcount - rollncount)
           rollsig = sqrt(abs(rollsum2 - rollsum*rollsum/count) / 
     &          (count-1)) / deg_to_rad 
           offsetavg = 60.0 * offset / deg_to_rad / 
     &          abs(offpcount - offncount)
           offsetsig = 60.0 * sqrt(abs(offset2 - offset*offset/count) / 
     &          (count - 1)) / deg_to_rad
           
        else
        
           rollavg = roll / deg_to_rad
           rollsig = 0.0
           offsetavg = 0.0
           offsetsig = 0.0

        end if

        if (rollavg .lt. 0.0) rollavg = rollavg + 360.0
        if (rollmin .lt. 0.0) rollmin = rollmin + 360.0
        if (rollmax .lt. 0.0) rollmax = rollmax + 360.0
        
        if (verbose .and. status .eq. 0) then

           call fcecho (' ')
           write (contxt, 2002) raavg, decavg, rollavg, offsetavg
           call fcecho (contxt)
           write (contxt, 2003) ramin, decmin, rollmin, offmin
           call fcecho (contxt)
           write (contxt, 2004) ramax, decmax, rollmax, offmax
           call fcecho (contxt)
           write (contxt, 2005) rasig, decsig, rollsig, offsetsig
           call fcecho (contxt)
           call fcecho (' ')
           write (contxt, 1006) count
           call fcecho (contxt)
           call fcecho (' ')
           write (contxt, 1007) ra_nom, dec_nom
           call fcecho (contxt)

        end if

 3001   format (' Attitude file start and stop ascatime :', 2(1x,f16.6))
 3003   format (' Data     file start and stop ascatime :', 2(1x,f16.6))
 3002   format (' Aspecting run start and stop ascatime :', 2(1x,f16.6))

 2000   format (13x,'         RA            DEC           ',
     &       ' ROLL          OFFSET     ')
 2001   format (13x,'       (deg)          (deg)         ',
     &       ' (deg)         (arcmin)    ')
 2002   format (' Average     ', 1x, 4(1x,f14.6)) 
 2003   format (' Minimum     ', 1x, 4(1x,f14.6)) 
 2004   format (' Maximum     ', 1x, 4(1x,f14.6)) 
 2005   format (' Sigma (RMS) ', 1x, 4(1x,f14.6)) 

 1001   format (' Time interval averaged over (seconds) :', 1x,f16.6)
 1011   format (' Total pointing and manuver time (sec) :', 2(1x,f16.6))
 1002   format (' Mean solar long & sun-src angle (deg) :', 2(1x,f16.6))
 1004   format (' Mean RA/DEC aberration corr. (arcsec) :', 2(1x,f16.6))
 1009   format (' Mean solar panel-sun angle      (deg) :', 1(1x,f16.6))

 5000   format (31x,'     RA             DEC         SUN ANGLE')
c 5001   format (31x,'     (deg)          (deg)        ',
c     &       '   (deg)      ')
 5002   format (' Mean solar position   (deg) :', 2(f10.2,5x))
 5003   format (' Mean aberration    (arcsec) :', 2(f10.2,5x))
 5004   format (' Mean sat X-axis       (deg) :', 2(f14.6,1x),f10.2)
 5005   format (' Mean sat Y-axis       (deg) :', 2(f14.6,1x),f10.2)
 5006   format (' Mean sat Z-axis       (deg) :', 2(f14.6,1x),f10.2)

 1003   format (' Mean boresight Euler angles :', 3(f14.6,1x))
 1007   format (' Aspecting to RA/DEC                   :', 2(1x,f16.8))
 1006   format (' Number of ASPECT records processed = ', i10)
        
        end
        
        subroutine write_aspect(ounit, time, ra, dec, roll, delta_ra, 
     &       delta_dec, quality)
        
        integer ounit, quality
        real time
        real ra, dec, roll, delta_ra, delta_dec
        
        write(ounit,12) time, ra, dec, roll, delta_ra, delta_dec, 
     &       quality
        
 12     format ((1x, f14.6), 3(1x, f8.3), 2(1x, f10.3), 1x, z6)
        
        end

        integer function getbit(word, bitnum)
        integer word, bitnum

        getbit = iand(word, 2**bitnum) / 2**bitnum

        end

        subroutine setbit(word, bitnum)
        integer word, bitnum

        word = ior(word, 2**bitnum)

        end


