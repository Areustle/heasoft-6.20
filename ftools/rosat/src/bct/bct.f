c
      subroutine bct

c Produce a Barycenter Correction Table from orbit data.

c For ROSAT only.

c BCT reads a ROSAT orbit file and a JPL2000 ephemeris file to produce a
c Barycenter Correction Table that can be used to conver times in an
c event list (or anywhere else) to the solar system barycenter.
c The output table is in 4-column format, with input and corrected times
c in integer and frational parts.  For details see subroutine bctmake.

c Author:  Eric A. Lufkin   December 1993, NASA/Goddard Space Flight Center

      IMPLICIT NONE

c      INCLUDE 'bctdef.inc'
c
      character(80) rastr,decstr,istring
      character(256) in_fil,ou_fil,eph_fil
      INTEGER*4 ierr,tchat,lchat,parse

      character(40) taskname
      common /task/ taskname

      DATA istring,parse /' ',0/

      taskname = 'bct 1.0'
      ierr = 0

c Get parameters.

      CALL bctinit(in_fil,ou_fil,rastr,decstr,tchat,lchat,eph_fil,ierr)

      IF(ierr.eq.0) THEN
 
c Set chattiness.
 
         CALL xchaty(tchat,lchat)
 
c Open log file. (The '+' is necessary to open append.)
c open log file using environment variable FLOGFILE
 
c         IF(lchat.ge.tchat) THEN
c            log_fil = '+' // program(:lenact(program)) // '.log'
c            CALL setlog(istring,parse,log_fil,' ')
c         ENDIF

c Make the Barycenter Correction TabLe.

         CALL bctmake(in_fil,ou_fil,rastr,decstr,eph_fil,ierr)

      ENDIF

      END
