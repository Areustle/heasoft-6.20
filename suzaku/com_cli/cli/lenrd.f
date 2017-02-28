C
C File: lenrd.f
C Desscriotion: Collection of utility routines for string manipulation
C Date: 15-Jun-1992
C
C Public: Lenrd, Lkbrd, CLrchrn
C
C Subroutine: Lenrd
C Description: return non-blank length of character
C
      Integer Function LENRD(CH)
      Implicit None
C input
      Character * (*)  CH
C local
      Integer L, M, I
C begin
      L = LEN(CH)
      M = L
      Do 1 I = 1,L
        If (CH(M:M).ne.' ') Goto 2
        M = M-1
1     Continue
2     LENRD = M
      Return
      End
C
C Subrotuine: Lkbrd
C Description: collapse unusefull blanks
C
      Integer Function LKBRD(STR,N)
C
      Implicit None
C input
      Character * (*)  STR
      Integer N
C local
      Integer L, M, K, I
C begin
      L = LEN(STR)
      M = 0
      K = N
      Do 10 I = 1,L
        If (STR(I:I).eq.' ') Then
          If (K.lt.N) Then
            K = K+1
            M = M+1
            STR(M:M) = STR(I:I)
          End If
        Else
          K = 0
          M = M+1
          If ( M.ne.I ) STR(M:M) = STR(I:I)
        End If
10    Continue
      If ( M+1 .le. L ) STR(M+1:L) = ' '
      LKBRD = MAX(0,M-K)
      Return
      End
C
C
      Integer Function CLrchr( String, C )
C
      Implicit None
C input
      Character * (*)  String
      Character * 1  C
C local
      Integer  i
C begin
      Do i = Len( String ), 1, -1
        If( String(i:i).eq.C ) Then
          CLrchr = i
          Return
        End If
      End Do
      CLrchr = 0
      Return
      End

