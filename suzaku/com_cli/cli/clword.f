C
C File: clword.for
C Description: Collection of string manipulation routines
C Author: A.Shirahashi, Univ. of Tokyo
C
C History:
C     07-Oct-1992, CLword, CLpart: quotation
C     21-Feb-2005 Y.ISHISAKI, add CLsubp2() and CLpart() call it
C     21-Feb-2005 Y.ISHISAKI, check TAB in CLpart(), CLsubp(), and CLsubp2()
C     21-Feb-2005 Y.ISHISAKI, add CLpart2(), which returns word length
C
C Public: CLword, CLpart, CLsubp, CLsubp2
C
C Note: CLsubp & CLpart is obsolete, use CLsubp2 & CLpart2, instead
C
C ----------------------------------------------------------
C divede the string 'S' into the words delimiter by any one
C character in the string 'D' into the array of strings 'W'.
C ----------------------------------------------------------
      Subroutine CLword( S,D,N,W )
      Implicit None
C
      Character * 1  SQ, DQ
      Parameter( SQ = "'", DQ = '"' )
C input
      Character * (*)  S, D
C output
      Character * (*)  W(*)
      Integer  N
C local
      Integer  L, I, J
      Character * 1  quote
C begin
      L = Len( S )
      I = 0
      N = 0
 100  Continue
      I = I + 1
      If( I.gt.L ) Return
      If( Index( D,S(I:I) ).ne.0 ) Goto 100
      J = I
      If( S(J:J).eq.SQ .or. S(J:J).eq.DQ ) Then
        quote = S(J:J)
        J = J + 1
      Else
        quote = ' '
      End If
 300  Continue
      I = I + 1
      If( I.gt.L ) Goto 310
      If( quote.eq.' ' ) Then
        If( Index( D,S(I:I) ).eq.0 ) Goto 300
      Else
        If( S(I:I).ne.quote ) Goto 300
      End If
 310  N = N + 1
      If( I-1.ge.J ) Then
        W(N) = S(J:I-1)
      Else
        W(N) = ' '
      End If
      Goto 100
C
      End
C
C ----------------------------------------------------------
C return the 'N'-th word in the string 'S' into 'W' (obsolete)
C ----------------------------------------------------------
      Subroutine CLpart( S,N,W )
      Implicit None
C input
      Character * (*)  S
      Integer  N
C output
      Character * (*)  W
C local
      Integer I0, I1
C begin
      Call CLsubp2(S, N, I0, I1)
      If ( 0.eq.I0 ) Then
        W = ' '
      Else
        W = S(I0:I1)
      End If
      Return
C
      End
C
C --------------------------------------------------------------------
C return the 'N'-th word in the string 'S' into 'W' with it lentgh 'L'
C --------------------------------------------------------------------
      Subroutine CLpart2( S,N,W,L )
      Implicit None
C input
      Character * (*)  S
      Integer  N
C output
      Character * (*)  W
      Integer  L
C local
      Integer I0, I1
C begin
      Call CLsubp2(S, N, I0, I1)
      If ( 0.eq.I0 ) Then
        W = ' '
        L = 0
      Else
        W = S(I0:I1)
        L = I1 - I0 + 1
      End If
      Return
C
      End
C
C --------------------------------------------------------------------
C return the offset of the first character of 'N'-th word
C in the string 'S' into 'I'.
C --------------------------------------------------------------------
      Subroutine CLsubp( S,N,I )
      Implicit None
C common
      include 'clidef.inc'
C input
      Character * (*)  S
      Integer  N
C output
      Integer  I
C local
      Integer  L, J, N1
C begin
      I = 0
      L = Len( S )
      N1 = 0
 100  Continue
      I = I + 1
      If( I.gt.L ) Return
      If ( SPACE .eq. S(I:I) .or. TAB .eq. S(I:I) ) Goto 100
      J = I
 300  Continue
      I = I + 1
      If( I.gt.L ) Goto 310
      If ( SPACE .ne. S(I:I) .and. TAB .ne. S(I:I) ) Goto 300
 310  N1 = N1 + 1
      If( N1.eq.N ) Then
        I = J
        Return
      End If
      Goto 100
C
      End
C
C --------------------------------------------------------------------
C return the offset of the first character of 'N'-th word
C              in the string 'S' into 'I0' and 'I1'.
C --------------------------------------------------------------------
      Subroutine CLsubp2( S,N,I0,I1 )
      Implicit None
C common
      include 'clidef.inc'
C const
      Character * 1  SQ, DQ
      Parameter( SQ = "'", DQ = '"' )
C input
      Character * (*)  S
      Integer  N
C output
      Integer  I0, I1
C local
      Integer L, I, J, N1
      Character * 1  quot
C begin
      L = Len( S )
      I = 0
      N1 = 0
C ... skip head SPACES & TABS
 100  Continue
      I = I + 1
      If( I.gt.L ) Then
        I0 = 0
        I1 = 0
        Return
      End If
      If ( SPACE .eq. S(I:I) .or. TAB .eq. S(I:I) ) Goto 100
C ... escape single & double quotations
      J = I
      If( S(J:J).eq.SQ .or. S(J:J).eq.DQ ) Then
        quot = S(J:J)
        J = J + 1
      Else
        quot = ' '
      End If
C ... search word end
 300  Continue
      I = I + 1
      If( I.gt.L ) Goto 310
      If( quot.eq.' ' ) Then
        If ( SPACE .ne. S(I:I) .and. TAB .ne. S(I:I) ) Goto 300
      Else
        If( S(I:I).ne.quot ) Goto 300
      End If
C ... finish
 310  N1 = N1 + 1
      If( N1.eq.N ) Then
        If( I-1.ge.J ) Then
          I0 = J
          I1 = I-1
        Else
          I0 = 0
          I1 = 0
        End If
ccc        Write (*,*) 'LINE=',S,' N=',N,' I0=',I0,' I1=',I1,' W=',S(I0:I1)
        Return
      End If
      Goto 100
C
      End
