C  $Header: /headas/headas/ftools/ascalib/src/ascalin/get_gain_norm.f,v 3.8 2013/05/21 19:08:08 irby Exp $
C          
        
C******************************************************************************
C FUNCTION:
C
C      get_gain_norm
C        
C DESCRIPTION:
C
C      get_gain_norm
C
C AUTHOR/DATE:
C
C       Ikebe-san
C       UNiversity of Toyko
C
C MODIFICATION HISTORY:
C       
C NOTES:
C
C USAGE:
C
C     call get_gain_norm(event_time)
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

      real function get_gain_norm(event_time)
      double precision event_time
      
      include 'asca_defs.inc'
      
      real cal_pi, cal_peak
      integer i
      character(80) warning
      
      include 'asca_common.inc'
      
      cal_pi = 500
C     cal_pi = 590
      
C     write(*,*) 'gh_records=',gh_records
C     write(*,*) 'START,END=',gh_start(1),gh_stop(gh_records)
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
Cevg more informative warning message
         write(warning, '(a,e25.16)') 
     &        'WARNING: TIME is out of range of gain history file: ', 
     &        event_time
         call fcecho(warning)
         cal_peak = 1
         cal_pi = 0
      end if
      
Cevg  don't do pi for special case.
      if (cal_peak .eq. 0) then
         get_gain_norm = 0.0
      else
Cevg  line too long for F77      
         get_gain_norm = cal_pi/cal_peak*gis_gain(cal_xo,cal_yo) / 
     &        cal_ratio
      end if

C     write(*,*) 'cal_xo,cal_yo=(',cal_xo,',',cal_yo,')'
C     write(*,*) 'i,cal_peak = ',i,cal_peak
C     write(*,*) 'cal_pi = ',cal_pi
C     write(*,*) 'cal_ratio =',cal_ratio
C     write(*,*) 'gis_gain(cal_xo,cal_yo)=',gis_gain(cal_xo,cal_yo)
      
      end
