C******************************************************************************
C FUNCTION:
C
C      get_sis_gain
C        
C DESCRIPTION:
C
C      get_sis_gain
C
C AUTHOR/DATE:
C
C       Eric Gotthelf/Koji Mukai GSFC/NASA
C
C MODIFICATION HISTORY:
C       
C NOTES:
C
C USAGE:
C
C     call get_sis_gain(...)
C
C ARGUMENTS:
C
C     iunit     - FORTRAN I/O unit for opened data file
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

      subroutine get_sis_gain(h, v, chip, graded, event_time, gain, 
     &     offset, error)
      integer h, v, chip, graded
      double precision event_time
      real gain, offset
      logical error

      include 'asca_defs.inc'
      
      integer i, j, k
      real coeff( 6, 0: 3)
      real frac1, frac2, delta_t
      character(80) warning

      include 'asca_common.inc'

      error = .FALSE.
      if (gh_records .eq. 1) then

         do k = 1, 6
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
               
               do k = 1, 6
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
               
               do k = 1, 6
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

         gain = 0.0
         offset = 0.0
         error = .FALSE.

      else

         gain   = coeff(1, chip) + h * coeff(2, chip)
     &        + v * coeff(3, chip)
         offset = coeff(4, chip) + h * coeff(5, chip)
     &        + v * coeff(6, chip)
         
      end if
      
      
      end




