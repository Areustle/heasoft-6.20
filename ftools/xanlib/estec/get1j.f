**==GET1J.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE GET1J(Parameter,Nvals,Values,Status)
      INCLUDE 'status.codes'
      CHARACTER*(*) Parameter
      INTEGER Nvals
      INTEGER*2 Values(Nvals)
      INTEGER Status
 
      IF ( Status.NE.OK__ ) RETURN
      CALL PRMPT(Parameter,Status)
      READ (*,*,IOSTAT=Status) Values
      RETURN
      END