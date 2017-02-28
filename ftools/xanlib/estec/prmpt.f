**==PRMPT.spg  processed by SPAG 3.09I  at 09:46 on 20 Aug 1992
      SUBROUTINE PRMPT(Parameter,Status)
 
      INCLUDE 'status.codes'
      CHARACTER*(*) Parameter
      INTEGER*4 Status
      character(80) string
* External reference :
      INTEGER*4 LENACT
 
      IF ( Status.NE.OK__ ) RETURN
      string = Parameter // '>'
      CALL PROMPT(string,LENACT(string))
      RETURN
      END
