*+ CSPAWN
      SUBROUTINE CSPAWN(CBUF, LBUF, IER)

	IMPLICIT NONE
      CHARACTER CBUF*(*)
      INTEGER   LBUF, IER
C-----------------------------------------------------------------------
C Description: Spawn to operating system.  If LBUF=0 then this routine 
C              should spawn a shell and leave the user in the shell 
C              until the user logs out or exits the shell.  If LBUF<>0 
C              then the system should only execute that one command and 
C              immediately return to the calling routine.
C
C Arguments:   CBUF  (i):  The system command to execute
C              LBUF  (i):  The number of valid characters in CBUF 
C                          (can be zero)
C              IER   (r):  =0 spawn was successful, <>0 otherwise
c 	       !!! NOTE !!!
c	       ... I *think* IER=1 is OK for VMS, thus IER is reset 
c			to zero when IER=1 below. !!! BEWARE !!!!
c			[IMG]
C
C Origin:      Swiped from the Xanadu Library for Calibration Library
C
C Authors/Modification History:
C              Xanadu Library
C              Ron Zellar (1993 Feb 3) Modified for inclusion in
C                   Calibration Library
c	       Ian M George (1.1.0: 1993 Aug 10) if(ier=1) ier=0 botch
C-----------------------------------------------------------------------
*-Version 1.1.0

      INTEGER   LIB$SPAWN
C---
      IER=LIB$SPAWN(CBUF(:LBUF))

c Iffy guess
	if(ier.EQ.1) ier = 0

      RETURN
      END
