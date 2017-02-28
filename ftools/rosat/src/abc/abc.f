c
      subroutine abc

c Apply Barycentric Corrections to event data.

c For ROSAT only.

c This program corrects the times in a file -- both the entries in the
c TIME column of an event list and the Good Time intervals in a 
c separate extension, as well as their header timing keywords -- for
c the ROSAT spacecraft clock and for the time delay given in a
c specified correction table.  The original file is NOT overwritten.  It
c is copied to a new file and the relevant entries are corrected in place.

c See subroutine abcmake for a detailed description of the algorithm.

c Author:  eal  February 1994, NASA/Goddard Space Flight Center
c 
c Modification History:
c     2006-08-27  Version 1.1 
c                 MFC fixed uninitialized variables in abcmake; first
c                 create a temporary file, then rename as output file
c                 if program completes successfully
c

      IMPLICIT NONE
c      INCLUDE 'abcdef.inc'
c
      character(80) rastr,decstr,istring
      character(256) in_fil,bc_fil,ou_fil,scc_fil
      INTEGER*4 tchat,lchat,ierr,parse

      character(40) taskname
      common /task/ taskname

      DATA istring,parse /' ',0/


      taskname = 'abc1.1'
      
c Get parameters.
      taskname = 'abc 1.0'
      ierr = 0

      CALL abcinit(in_fil,bc_fil,ou_fil,rastr,decstr
     &            ,tchat,lchat,scc_fil,ierr)

      IF(ierr.eq.0) THEN

c Set chattiness.

         CALL xchaty(tchat,lchat)

c Open log file. (The '+' is necessary to open append.)
c open log file using environment variable FLOGFILE

c         IF(lchat.ge.tchat) THEN
c            log_fil = '+' // program(:lenact(program)) // '.log'
c            CALL setlog(istring,parse,log_fil,' ')
c         ENDIF

c Execute program.

         CALL abcmake(in_fil,bc_fil,ou_fil,rastr,decstr
     &               ,scc_fil,ierr)

      ENDIF

      END
