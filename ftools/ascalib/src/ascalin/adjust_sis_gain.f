C******************************************************************************
C FUNCTION:
C
C      adjust_sis_gain
C        
C DESCRIPTION:
C
C      adjust_sis_gain
C
C AUTHOR/DATE:
C
C      Koji Mukai, 2001 March based on EVG/KM's get_sis_gain
C
C MODIFICATION HISTORY:
C
C      Koji Mukai, 2005 March, increased the number of coefficients
C                   to 8 to match Dotani-san's new Type 3 formula
C       
C NOTES:
C
C USAGE:
C
C     call adjust_sis_gain(h, v, chip, graded, event_time, fph, error )
C
C ARGUMENTS:
C
C     h, v      - RAWX and RAWY coordinates of the SIS event
C     chip      - CCDID of the event
C     graded    - grade of the event
C     event_time - when it was recorded
C     fph       - PHA channel in 0-4096 scale, I/O
C     error     - will be flagged
C
C PRIMARY LOCAL VARIABLES:
C
C
C******************************************************************************

      subroutine adjust_sis_gain(h, v, chip, graded, event_time,
     &     fph, error)
      integer h, v, chip, graded
      double precision event_time
      double precision temp
      real fph
      logical error

      include 'asca_defs.inc'
      
      integer i, j, k
      real coeff( 8, 0: 3 )
      real frac1, frac2, delta_t
      character(80) warning

      include 'asca_common.inc'

*     Derive the correct set of coefficients (by interpolation if necessary)

      error = .FALSE.
      if (gh_records .eq. 1) then

         do k = 1, 8
            coeff(k, 0) = gh_c0(1,k)
            coeff(k, 1) = gh_c1(1,k)
            coeff(k, 2) = gh_c2(1,k)
            coeff(k, 3) = gh_c3(1,k)
         end do

      else

         if (gh_start(1) .le. event_time .and. 
     &        event_time .lt. gh_start(gh_records)) then
            
            i = 1
            do while (gh_start(i) .lt. event_time)
               i = i + 1
            end do
            
            if (event_time .ge. gh_start(i)) then
               
               do k = 1, 8
                  coeff(k, 0) = gh_c0(i,k)
                  coeff(k, 1) = gh_c1(i,k)
                  coeff(k, 2) = gh_c2(i,k)
                  coeff(k, 3) = gh_c3(i,k)
               end do
               
            else
               
c     Interpolate between two successive rows
               
               i = i - 1
               delta_t =  gh_start(i+1) - gh_start(i)
               frac1   = (gh_start(i+1) - event_time) / delta_t
               frac2   = (event_time - gh_start(i)) / delta_t
               
               do k = 1, 8
                  coeff(k, 0) = gh_c0(i,k)*frac1+gh_c0(i+1,k)*frac2
                  coeff(k, 1) = gh_c1(i,k)*frac1+gh_c1(i+1,k)*frac2
                  coeff(k, 2) = gh_c2(i,k)*frac1+gh_c2(i+1,k)*frac2
                  coeff(k, 3) = gh_c3(i,k)*frac1+gh_c3(i+1,k)*frac2
               end do
               
            end if
               
         else

            write(warning, '(a,e25.16)') 
     &'WARNING: TIME is out of range of gain history file: ', 
     &           event_time
            call fcecho(warning)
            error = .TRUE.
            
         end if
         
      end if

C     don't do pi for special case.
         
      if (error) then

         fph = fph
         error = .FALSE.

      else

*       This piece of code matches Dotani-san's animal software
*       (although he does it in double precision, which probably
*       isn't necessary).
*       Modified for Type 3 - new parameter 3, so old params 3-7 became
*             new params 4-8 (Type 2 files are now read to have new param
*             3 = 1.0) 2005 March
*       Convert to eV
        fph = fph * gain_chips( chip )
*       Correct for Serial CTI
*        if( fph .gt. coeff( 4, chip ) ) then
*          fph = fph
*     &       + ( h - 1 ) * coeff( 3, chip ) * ( fph - coeff( 4, chip ) )
*        end if
        if( fph .gt. coeff( 5, chip ) ) then
          fph = fph
     &       + ( h - 1 ) * coeff( 4, chip ) * ( fph - coeff( 5, chip ) )
        end if
*       Now correct for parallel CTI
*        if( fph .gt. 0.0 ) then
*          fph = fph + v * ( coeff( 5, chip )
*     &        + ( h - 1 ) * coeff( 6, chip ) ) * fph ** coeff( 7, chip )
*        end if
        if( fph .gt. 0.0 ) then
          fph = fph + v * ( coeff( 6, chip )
     &        + ( h - 1 ) * coeff( 7, chip ) ) * fph ** coeff( 8, chip )
        end if
*       Now correct for frame-store CTI
*        if( fph .gt. coeff( 2, chip ) ) then
*          fph = fph
*     &             + 422 * coeff( 1, chip ) * ( fph - coeff( 2, chip ) )
*        end if
        if( fph .gt. coeff( 2, chip ) ) then
          fph = fph + 422 * coeff( 1, chip )
     &                  * ( fph - coeff( 2, chip ) ) ** coeff( 3, chip )
        end if
         
      end if
      
      
      end




