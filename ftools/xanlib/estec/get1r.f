**==GET1R.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
      SUBROUTINE GET1R(Parameter,Nvals,Values,Status)
      INCLUDE 'status.codes'
      CHARACTER*(*) Parameter
      INTEGER Nvals
      REAL Values(Nvals)
      INTEGER Status
 
      IF ( Status.NE.OK__ ) RETURN
      CALL PRMPT(Parameter,Status)
      READ (*,*,IOSTAT=Status) Values
      RETURN
      END
