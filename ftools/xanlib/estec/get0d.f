**==GET0D.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE GET0D(Parameter,Value,Status)
      INCLUDE 'status.codes'
      CHARACTER*(*) Parameter
      REAL*8 Value
      INTEGER Status
 
      IF ( Status.NE.OK__ ) RETURN
      CALL PRMPT(Parameter,Status)
      READ (*,*,IOSTAT=Status) Value
      RETURN
      END
