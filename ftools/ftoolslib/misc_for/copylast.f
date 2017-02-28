
C*****************************************************************************
C SUBROUTINE:
C       copylast
C
C DESCRIPTION:
C       copy extensions following current one from input to output files
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       21 July, 1993
C
C MODIFICATION HISTORY:
C     Lawrence Brown 6/14/94
C     fixed up so that any error other than the necessary 107
C     (end of file)  will be treated as an error and its status
C     returned
C
C NOTES:
C
C USAGE:
C        call copylast (iunit, ounit, status)
C ARGUMENTS:
C       iunit   - input unit number
C       ounit   - output unit number
C       status  - status of operation
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C*****************************************************************************
      subroutine copylast (iunit, ounit, status)

      integer iunit, ounit, status

      integer htype

      if(status.ne.0) goto 999

C copy all additional extension, if so requested
 900  call ftmrhd (iunit, 1, htype, status)
      call ftcrhd (ounit, status)
      call ftcopy (iunit, ounit, 0, status)
      if (status .eq. 0) goto 900
      if (status .ne. 107) goto 999
      status = 0

 999  return
      end
