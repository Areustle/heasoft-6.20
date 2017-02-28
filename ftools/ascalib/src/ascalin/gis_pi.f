C******************************************************************************
C FUNCTION:
C
C      gis_pi
C
C DESCRIPTION:
C
C      get pi bin value
C
C AUTHOR/DATE:
C
C       Yasushi Ikebe, April 1993.
C       University of Toyko
C
C       Eric Gotthelf, GSFC/NASA
C
C MODIFICATION HISTORY:
C
C       Feb 1997. Added code to correct for radial dependent long-term
C                 secular gain drift (see Gain History File, V4+, for
C                 details). E.Gotthelf/K.Ebisawa/T.Ishisaki.
C       Dec 1998. gmapc_dim is supposed to be the order of the radial
C                 polynomial, so start loop at gmapc_dim+1 to get all coeffs.
C                 Jeff Guerber, GSFC/Raytheon ITSS.
C
C USAGE:
C
C     call gis_pi(pha_bin, ix, iy, event_time, fx, fy)
C
C ARGUMENTS:
C
C     pha_bin    - uncorrected PHA value of event
C     ix, iy     - gain map location of event
C     event_time - ascatime of event
C     fx, fy     - DETX/Y location of event
C
C CALLED ROUTINES:
C
C      subroutine get_cal_peak - returns the interpolated cal peak for
C                                the given time.
C
C******************************************************************************

        integer function gis_pi(pha_bin, ix, iy, event_time, fx, fy)

        implicit none

        include 'asca_defs.inc'

        integer pha_bin, ix, iy
        real fx, fy
        double precision  event_time

        integer i
        real fph, random, get_cal_peak, cal_peak, drift_factor, r

        include 'asca_common.inc'

        cal_peak = get_cal_peak(event_time)

        if (gis_gain(ix,iy) .gt. 0.0 .and. cal_peak .ne. 0.0) then

           fph = (float(pha_bin) + random(iseed)) *
     &          (cal_norm / gis_gain(ix, iy)) * (cal_adu / cal_peak)

           fph = (fph - gain_offset*cal_adu/cal_kev) / gain_renorm

           if (gmapc_dim .gt. 0) then

              r = sqrt((fx-det_x_center)**2 + (fy-det_y_center)**2)

              drift_factor = 0.0
              do i = gmapc_dim+1, 1, -1
                 drift_factor = drift_factor * r + gmapc_coeff(i)
              end do

              if (drift_factor .gt. 0.0001) fph = fph/drift_factor

           end if

        else

           fph = 0.0

        end if

        gis_pi = max(min(int(fph), pha_size-1), 0)

        end
