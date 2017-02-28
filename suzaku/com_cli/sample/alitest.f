C
C File: CLITEST.FOR
C Description: Test program for CLI
C
      Character * 16  key
      Character * 2  table(5)
      Integer * 4  it
C data
      Data table / 'aa','ab','c','d','e' /
C
C
      Call CLalii( 'clear','@$CLEAR' )
      Call CLalii( 'command','C O M A N D')
C ... test for KEYRD
 100  Call Keyrd(-2,'?KEY>',key,table,5,it)
      Print *, it,',',key
      Goto 100
C
      Stop
      End
C
