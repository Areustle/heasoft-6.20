**==GET1D.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE GET1D(Parameter,Nvals,Values,Status)
      INCLUDE 'status.codes'
      CHARACTER*(*) Parameter
      INTEGER Nvals
      REAL*8 Values(Nvals)
      INTEGER Status
 
      IF ( Status.NE.OK__ ) RETURN
      CALL PRMPT(Parameter,Status)
      READ (*,*,IOSTAT=Status) Values
      RETURN
      END
