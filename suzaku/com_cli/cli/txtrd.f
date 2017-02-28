C
C File: txtrd.f
C Descriotion: Collection of character input routines
C Date: 15-Jun-1992
C
C History:
C      4-Sep-1992, Titrd "title"/'title'
C     18-Jan-1993, add Optrd
C      2-Feb-1993, IntrdX: call CLitoa
C      4-Feb-1993, Txtrd rewritten: allow " for quotation
C     15-Feb-1993, Txtrd: skip preceding spaces before next text
C     09-Jul-1997 Y.ISHISAKI, delete IntrdX, FltrdX, FdprdX
C     12-Nov-2005 Y.ISHISAKI, check size of ANSWER in TitRD
C
C Public: Txtrd, Titrd, Optrd
C
C Subroutine: Txtrd
C Description: Read one word from input token stream
C
      Subroutine Txtrd( quest,answer )
      Implicit None
C constant
      include 'clidef.inc'
C input
      Character * (*)  quest
C input/output
      Character * (*)  answer
C common
      include 'clflag.inc'
C local
      Character * 1  K, K2, Q
      Integer   Nch, Ich
C begin
      If( quest(1:1) .eq. '?' ) answer = ' '
      Nch = Len( answer )
      Ich = 0
      Call ARGRD( quest,answer )
C
 10   Continue
      Call Chard( K,K2 )
      IF( K.eq.' ' .or. K.eq.TAB ) Goto 10
C
      IF( K.eq.CRET ) Return
C
      If( K.eq.QUOTE .or. K.eq.'"' ) Then
        Q = K
      Else
        Q = ' '
        Ich = Ich + 1
        If( Ich.le.Nch ) answer(Ich:Ich) = K
      End If
C
 20   Continue
      Call Chard( K,K2 )
      If( Q.eq.' ' .and.
     &    (K.eq.CRET .or. K.eq.SPACE .or. K.eq.TAB .or.
     &     K.eq.',' .or. K.eq.'=')
     &  ) Then
        Goto 30
      End If
      If( Q.ne.' ' .and. (K.eq.Q .or. K.eq.CRET ) ) Then
        Goto 30
      End If
      Ich = Ich + 1
      If( Ich.le.Nch ) answer(Ich:Ich) = K
      Goto 20
C
 30   Continue
      If( Ich.lt.Nch ) answer(Ich+1:) = ' '
 40   Continue
        If( K2.ne.' ' .and. K2.ne.TAB ) Goto 50
        Call Chard( K,K2 )
      Goto 40
 50   Continue
      Return
      End
C
C
C Subroutine: Titrd
C Description: Read a whole line from input stream
C
      Subroutine TITRD(QUEST,ANSWER)
      Implicit None
C constant
      include 'clidef.inc'
C input
      Character * (*)  QUEST
C input/output
      Character * (*)  ANSWER
C local
      Character * 1  K, K2, Q
      Integer  Nch, Ich
C begin
      If (QUEST(1:1).eq.'?') ANSWER = ' '
      Nch = Len( ANSWER )
      Ich = 0
      Q = ' '
      Call ARGRD(QUEST,ANSWER)
C
      Call Chard( K,K2 )
      If( K.eq.'"' .or. K.eq.QUOTE ) Then
        Q = K
      Else If( K.eq.CRET ) Then
        Return
      Else
        Ich = Ich + 1
        If( Ich.le.Nch ) ANSWER(Ich:Ich) = K
      End If
10    Continue
        Call CHARD( K,K2 )
        If( K.eq.CRET ) Goto 20
        If( Q.ne.' ' .and. K.eq.Q ) Goto 20
        Ich = Ich + 1
        If( Ich.le.Nch ) ANSWER(Ich:Ich) = K
      Goto 10
20    Continue
      If( Ich.gt.0 .and. Ich.lt.Nch ) ANSWER(Ich+1:) = ' '
      Return
      End
C
C
      Subroutine Optrd( quest,text )
      Implicit None
C input
      Character * (*)  quest
C input/output
      Character * (*)  text
C local
      Integer  L
C function
      Integer  Lenrd
C begin
      Call Txtrd( quest,text )
      L = Lenrd( text )
C
      If( Index('#0123456789+-.',text(1:1)).gt.0 ) Then
        Call Ugetrd( text,L )
        text = ' '
      Else
        Call CLstrupc( L,text )
      End If
C
      Return
      End
