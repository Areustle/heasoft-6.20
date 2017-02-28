C
C File: clecho.f
C Description: Equivalent with CLPUTL(6, string)
C
C Public:  CLECHO, CLIERR
C
C History:
C     30-Jul-1996 Y.ISHISAKI, CLECHO Created
C     21-Feb-2005 Y.ISHISAKI, CLIerr() moved from clgetl.f
C     22-Feb-2005 Y.ISHISAKI, remove using IFTSS()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
      Subroutine CLecho( String )
      Implicit None
C input
      Character * (*)  String
C begin
      Call CLputL( 6, String )
C
      Return
      End
C
C ----------
C   CLIERR   ... print error message
C ----------
      Subroutine CLIERR( LEVEL,MESAGE )
      Implicit NONE
C ARG
C     (Input)
      Integer * 4  LEVEL
      Character * ( * )  MESAGE
C
C Common
      include 'clidef.inc'
      include 'clunit.inc'
C
C VAR
      Integer  LMSG
      Character * 8  PREFIX
      Logical   * 4  YES
      Character * (LINSIZ)  MSG
C
C FUNC
      Integer Lenrd
C
C BEGIN
      If( LEVEL .eq. 0 ) Then
        PREFIX = '%CLI-I, '
      Else If( LEVEL .eq. 1 ) Then
        PREFIX = '%CLI-W, '
      Else If( LEVEL .eq. 2 ) Then
        PREFIX = '%CLI-E, '
      End If
C
      LMSG = LENRD( MESAGE )
      MSG = MESAGE(:LMSG)
      Call CLputL(6,Prefix//MSG(:Lmsg))
C
      If( LEVEL.le.0 ) Return
      If( LUNIN.eq.5 .or. LUNPTR.eq.0 ) Return
C
      YES = .TRUE.
      Call AFFIRM( 'Continue to execute command procedure ?', YES )
      If( .not.YES ) Call CLSTOP()
C
      Return
      End
