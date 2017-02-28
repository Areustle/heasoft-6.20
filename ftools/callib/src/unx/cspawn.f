*+CSPAWN
      SUBROUTINE CSPAWN(CBUF, LBUF, IER)
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
C
C Origin:      Swiped from the Xanadu Library for Calibration Library
C
C Authors/Modification History:
C              Xanadu Library
C              Ron Zellar (1993 Feb 3) Modified for inclusion in
C                   Calibration Library
c	       Ian M George (1.1.0: 1993 Aug 10), replaced write(*,*)
c              Jeff Guerber (1.1.1: 1999-02-17)  Incr. CSHELL size, in case.
C-----------------------------------------------------------------------
*-Version 1.1.0

      CHARACTER CSHELL*255, message*80
      INTEGER   IFIRST, LSHELL
      SAVE CSHELL, IFIRST, LSHELL
      DATA IFIRST/1/
C---
      IER=0

      IF(IFIRST.NE.0) THEN
         IFIRST=0
         CALL CTRLOG('SHELL', 5, CSHELL, LSHELL)
         IF(LSHELL.EQ.0) THEN
            CSHELL='/bin/csh'
            LSHELL=8
         END IF
      END IF
      IF(LBUF.GT.0) THEN
         CALL system(CBUF(:LBUF),IER)
      ELSE
         message = 'Type exit to return.'
         call fcecho(message)
         CALL system(CSHELL(:LSHELL),IER)
      END IF
      RETURN
      END
