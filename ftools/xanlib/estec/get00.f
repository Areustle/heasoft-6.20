**==GET00.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE GET00(String,Length,Status)
* 20 November 1990 : code moved to XANLIB
      INCLUDE 'status.codes'
      CHARACTER*(*) String
      INTEGER Length
      INTEGER Status
*-
      IF ( Status.NE.OK__ ) RETURN
      CALL RDFORN(String,Length)
      RETURN
      END
