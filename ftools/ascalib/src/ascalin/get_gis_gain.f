C******************************************************************************
C FUNCTION:
C
C      get_cal_peak
C        
C DESCRIPTION:
C
C      get_cal_peak
C
C AUTHOR/DATE:
C
C       Ikebe-san
C       University of Toyko
C
C MODIFICATION HISTORY:
C       
C NOTES:
C
C USAGE:
C
C     call get_cal_peak(event_time)
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

      real function get_cal_peak(event_time)
      double precision event_time
      
      include 'asca_defs.inc'
      
      real cal_pi, cal_peak
      integer i
      character(80) warning
      
      include 'asca_common.inc'
      
      if( gh_start(1).le.event_time .and. 
     &     event_time.lt.gh_stop(gh_records)) then
         i = 1
Cebi  do while (gh_stop(i).lt.event_time)
 2       continue
         if (.not.(gh_stop(i).lt.event_time)) go to 1
         i = i + 1
         go to 2
 1       continue
Cebi  end do
         
         if(event_time.ge.gh_start(i)) then
            cal_peak = gh_iron(i)
         else 
            cal_peak = gh_iron(i-1)+(gh_iron(i)-gh_iron(i-1))* 
     &           (2*event_time - gh_start(i-1) - gh_stop(i-1)) /
     &           (gh_start(i)+gh_stop(i)-gh_start(i-1)-gh_stop(i-1))
         end if

      else

         write(warning, '(a,f20.8)') 
     &        'CAUTION: TIME is out of range of gain history file: ', 
     &        event_time
         call fcecho(warning)
         cal_peak = 1
         cal_pi = 0
      end if
      
      get_cal_peak = cal_peak
      
      end
