*+SIS_PI
        integer function sis_pi(pha_bin, i, j, chip, graded, 
     &     event_time)
        
        implicit none
        
        include 'asca_defs.inc'
        
        integer pha_bin, i, j, chip, graded
        double precision event_time
        logical error

*       Calculates the SIS Bright/Bright2 mode PI channels
*       given pha, rawx, rawy, ccdid, (grade --- unused at the moment),
*       and event_time information.
*
*       Arguments
*         pha_bin       (i) : integer, pha of event
*         i, j          (i) : integers, raw coordinates of event
*         chip, graded  (i) : integers, chip ID and event grade
*         event_time    (i) : double precision, ASCAtime of event
*         SIS_PI        (r) : Returns the pi bin
*
*       Requires previous call to read_sis_gain_file, which would
*       have set up the common block variables; calls get_sis_gain
*       to figure out the appropriate gain & offset values.
*       
*       History
*         Original code written by Koji Mukai and made into a function
*         by Eric Gotthelf; a modified version (1996 Feb) by Koji Mukai
*         to be shared between sispi and ascalin.
*
*         Added independent energy scale renormalization for ascapi. Apr 15
*         1996 (EVG).
*
*         Significant modification: the gain & offset paradigm for
*         CTI correction is dead.  There is now a sqrt(fph) term
*         as well.  Old format retained for use with old calibration
*         file, now added a new method.
*                                  Koji Mukai, 2001 March
*-SIS_PI
        real fph, random, get_sis_gain_coeff
        real gain_fact, offset, gain_norm
        integer pi_b1, pi_b2
        character(80) warning

        include 'asca_common.inc'
        
        if (sis_gain .gt. 0.0 ) then

          if( pha_bin .eq. pha_size - 1 ) then
*           PHA/PI number assumed to start from channel 0
*           Thus pha_size-1 (2047 in Bright, 4095 in Bright2)
*           is the overflow channel --- don't touch it.
            SIS_PI = det_pi_size - 1
          else
           
            fph = (float(pha_bin) + random(iseed))

*           Neat formula for bright/bright2 to linear conversion,
*           works without an if statement provided sis_b2l have been
*           initialized to 1024.0, 1536.0 for Bright, 2x4096.0 for Bright2
            fph = fph + max( fph - sis_b2l( 1 ), 0.0 )
     &                            + max( fph - sis_b2l( 2 ), 0.0 ) * 2.0

            if( n_ph2pi .eq. 0 ) then
*             Use the old "gain and offset" formula
              call get_sis_gain(i, j, chip, graded, event_time, 
     &          gain_norm, offset, error)
              fph = fph * gain_norm + offset
            else if( n_ph2pi .gt. 0 ) then
*             Use the new method
              call adjust_sis_gain(i, j, chip, graded, event_time,
     &                       fph, error )
            end if

*           Now fph is the photon energy in eV
            fph = ( fph / 1000.0 - sis_off ) / sis_gain
           
*           Now set the gain scale and offset - convert PH to Energy 
*           Note - gain_norm and offset are in eV, 
*                  sis_off and sis_gain are in keV.


*           Now includes renorm'ed gain and offset --- findings from XSPEC 
*           gain command or other external info can be folded back
*           into the PI channel

            pi_b1 = int( (fph - gain_offset/sis_gain ) 
     &           / gain_renorm)

*           Neat formula for linear to bright/bright2 conversion,
*           works without an if statement provided sis_l2b have been
*           initialized to 1023, 2046 for Bright, 2x4095 for Bright2

            pi_b2 = pi_b1 - max( pi_b1 - sis_l2b( 1 ), 0 ) / 2
     &                              - max( pi_b1 - sis_l2b( 2 ), 0 ) / 4
            SIS_PI = max( min( pi_b2, det_pi_size - 1 ), 0 )
          end if 

        else
          sis_pi = 0
        end if
        
        return
        
        end

