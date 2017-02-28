      SUBROUTINE GTHELP(Clib, Ctopic)
      CHARACTER Clib*(*), Ctopic*(*)
C---
C Display the help text from the Clib help library on topic Ctopic.
C---
C Clib    I    Location of help libary.
C Ctopic  I    The topic of interest.
C---
C 2002-May-24 - New routine [AFT]
C----
      INTEGER   LENACT
C
      INTEGER   ier, itmp, llib
C
      CHARACTER chelp*256
      SAVE      chelp
      INTEGER   ifirst, lhelp
      SAVE      ifirst, lhelp
      DATA ifirst/1/, lhelp/0/
C
      IF ( ifirst.NE.0 ) THEN
         CALL TRLOG('XANHTML',7,chelp,lhelp)
         IF ( lhelp.LE.0 ) THEN
            CALL PTBUF('Problem with setup, Variable XANHTML is not '//
     &         'defined.',0)
            RETURN
         END IF
      END IF
      llib = LENACT(Clib)
      chelp(lhelp+1:) = ' '//Clib(:llib)//'/index.html &'
      itmp = LENACT(chelp)
      WRITE(*,*) 'chelp=',chelp(:itmp)
      CALL SPAWN(chelp, itmp, ier)
      RETURN
      END
