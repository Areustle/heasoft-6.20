**==xgtarg1.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
*
* subroutine to return a command or parameter
*
      SUBROUTINE XGTARG1(Zstring,Zparse,Zbeg,Zend,Qskip,Iflag,Delim,*,*,
     &                   *)
*
*
* zstring, returned command name
* zparse, set to length of command
* zbeg, set to 1
* zend, set to length of command
* qskip, true if command is empty
* iflag, -1 = EOF, 0 ok, 1 fell off end, 2 terminal skip char
* delim, always 0
* returns, not used
*
      IMPLICIT NONE
      CHARACTER*(*) Zstring
      INTEGER*4 Zparse , Zbeg , Zend
      LOGICAL Qskip
      INTEGER*4 LENACT
      INTEGER*4 Iflag
      INTEGER*4 Delim
      INCLUDE 'yaccfor.inc'
 
      Delim = 0
 
 
      Qskip = .FALSE.
      IF ( COMval ) THEN
         COMval = .FALSE.
         Zstring = SCOm
         Zbeg = 1
         Zend = LENACT(SCOm)
         Zparse = Zend + 1
         RETURN
      ENDIF
 
      IF ( CPAr.GT.NPArs ) THEN
         Zstring = ' '
         Zbeg = 1
         Zend = 1
         Zparse = 2
         Qskip = .TRUE.
         Iflag = 1
         RETURN 1
      ENDIF
 
      Zstring = SPArs(CPAr)
      Zbeg = 1
      Zend = LENACT(Zstring)
      Zparse = Zend + 1
      Iflag = 0
      RETURN
      END
