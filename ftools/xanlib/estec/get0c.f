**==GET0C.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE GET0C(Parameter,Value,Status)
      INCLUDE 'status.codes'
      CHARACTER*(*) Parameter
      CHARACTER*(*) Value
      INTEGER Status
 
      IF ( Status.NE.OK__ ) RETURN
 
      CALL PRMPT(Parameter,Status)
      READ (*,'(a)',IOSTAT=Status) Value
      RETURN
      END
