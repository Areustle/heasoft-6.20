CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     standalone: stub subroutines to allow ascatape to be run as
C     a standalone program.
C
C     Author:  Don Jennings, NASA/GSFC/HSTX
C     Date:    04/04/94
C     Version: 1.0
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     The following takes the place of the FTOOL C main binding routine
C
      program ascatape
C
      call ascate
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     The following takes the place of the FTOOL fcecho subroutine call
C
      subroutine fcecho(context)
C
      character(80) context
C
      print *,context
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     The following takes the place of the FTOOL fcerr subroutine call
C
      subroutine fcerr(context)
C
      character(80) context
      character(90) str
      integer*4    i
      logical*4    done
C
      str = 'ERROR: ' // context
      i = len(str)
      done = .false.
      do while(i .gt. 0 .and. done .eq. .false.)
         if(str(i:i) .ne. ' ')then
            done = .true.
         else
            i = i - 1
         endif
      enddo
C
      print *,str(1:i)
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     The following takes the place of the HOST uclgst call
C
      subroutine uclgst(string1,string2,int)
C
      character*(*) string1,string2
      integer*4     int
C
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     The following takes the place of the HOST uclgsb call
C
      subroutine uclgsb(string,boolean,int)
C
      character*(*) string
      integer*4     int
      logical*4     boolean
C
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
