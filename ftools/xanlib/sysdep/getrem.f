C*********
      SUBROUTINE GETREM(REMUSER, IERR)
      CHARACTER REMUSER*(*)
      INTEGER   IERR
C---
C Return the name of the current remote user
C Author : Andy Pollock
C Original UNIX dummy : 20 September 1992
C---
C REMUSER   O  The remote id
C IERR      O  =1 if valid, <>0 could not generate user id
C---
C---
      IERR=0
      REMUSER='Unknown'
      RETURN
      END
