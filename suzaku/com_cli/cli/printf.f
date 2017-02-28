C
C File: printf.f
C Description: Emulate C's printf
C Author: A.Shirahashi, Univ. of Tokyo
C
C Public : PRINTF,
C Private: CLVTOS, CLVFMT
C
C History:
C     22-Jan-1992, do not use NARGET
C                       support for '%d' format
C     20-Feb-2005 Y.ISHISAKI, insert 'Implicit None'
C
      Subroutine PRINTF( LUN, S, V1,V2,V3,V4,V5,V6,V7,V8,V9 )
      Implicit None
C
C ARG
      Integer * 4  LUN
      Character * (*)  S
      Real * 4  V1, V2, V3, V4, V5, V6, V7, V8, V9
C
C VAR
      Integer * 4  I, L, N, LENS, IS, IO, IV, IP, IN, IN0, NW, ND, INP
      Real * 4  ARGV(9)
      Integer * 4  ARGI(9)
      Equivalence( ARGV,ARGI )
      Character * 132  SO
      Character * 16  STRING
C
C FUNC
      Integer * 4  Lenrd, Lkbrd
C
C BEGIN
      I = 1
      N = 0
   10 Continue
        If( S(I:I).eq.'$' ) Goto 20
        If( S(I:I).eq.'%' ) Then
          N = N + 1
          I = I + 2
        Else
          I = I + 1
        End If
      Goto 10
   20 Continue
      LENS = I - 1
C
C     N = MIN( NARGET()-2,9 )
      If( N.ge.1 ) ARGV(1) = V1
      If( N.ge.2 ) ARGV(2) = V2
      If( N.ge.3 ) ARGV(3) = V3
      If( N.ge.4 ) ARGV(4) = V4
      If( N.ge.5 ) ARGV(5) = V5
      If( N.ge.6 ) ARGV(6) = V6
      If( N.ge.7 ) ARGV(7) = V7
      If( N.ge.8 ) ARGV(8) = V8
      If( N.ge.9 ) ARGV(9) = V9
C
      IS = 0
      IO = 0
      IV = 0
100   Continue
C     If( IS.ge.LEN(S) ) Goto 800
      If( IS.ge.LENS ) Goto 800
      IP = INDEX( S(IS+1:LENS),'%' )
      If( IP.eq.0 ) Goto 800
      If( IP.ge.2 ) Then
        SO(IO+1:IO+IP-1) = S(IS+1:IS+IP-1)
        IO = IO + IP - 1
      End If
      IN = IS+IP+1
      IN0 = IN
200   Continue
      If( INDEX( '.0123456789',S(IN:IN) ) .gt. 0 ) Then
        IN = IN + 1
        Goto 200
      End If
      If( Index( 'dDfF',S(IN:IN) ) .gt. 0 ) Then
        If( IN.eq.IN0 ) Then
          NW = 0
          ND = 6
        Else
          INP = INDEX( S(IN0:IN-1),'.' )
          If( INP.eq.0 ) Then
            Read( S(IN0:IN-1),* ) NW
            ND = 6
          Else
            If( INP.eq.1 ) Then
              NW = 0
            Else
              Read( S(IN0:IN0+INP-2),* ) NW
            End If
            Read( S(IN0+INP:IN-1),* ) ND
          End If
        End If
        IV = IV + 1
        If( S(IN:IN).eq.'d' .or. S(IN:IN).eq.'D' ) Then
          Write(String,*) ARGI(IV)
          L = Lkbrd( String,0 )
        Else
          Call CLVTOS( ARGV(IV),STRING,NW,ND )
          L = Lenrd( String )
        End If
        If( NW.eq.0 ) Then
          NW = L
        End If
        If( NW.gt.L ) Then
          SO(IO+1:IO+NW-L) = ' '
          SO(IO+NW-L+1:IO+NW) = STRING(:L)
        Else
          SO(IO+1:IO+L) = STRING(:L)
        End If
        IO = IO + NW
      Else
        IO = IO + 1
        SO(IO:IO) = S(IN:IN)
      End If
      IS = IN
      Goto 100
800   Continue
C     L = LEN( S ) - IS
      L = LENS - IS
      If( L.gt.0 ) Then
        SO(IO+1:IO+L) = S(IS+1:IS+L)
        IO = IO + L
      End If
      Write( LUN,* ) SO(:IO)
C
      Return
      End
C
C Subroutine: CLVTOS
C Description: Convert value to string
C Author: A.Shirahashi, Univ. of Tokyo
C
       Subroutine CLVTOS( VAL,STR,NW,ND )
       Implicit None
C
C ARG
       Real      *  4   VAL
       Character * (*)  STR
       Integer   *  4   NW, ND
C
C VAR
       Integer * 4  I, LS, IE, NI, NZ, NP, NE
C
C FUNC
       Integer * 4  CLVFMT
C
C BEGIN
       STR = ' '
       LS = CLVFMT( 'G',LEN(STR),ND,VAL,STR )
       IE = INDEX( STR,'E' )
       If( IE .eq. 0 ) IE = LS + 1
       I = IE - 1
       NI = I
100    Continue
         If( STR(I:I) .ne. '0' ) Goto 110
         I = I - 1
       Goto 100
110    Continue
       If( STR(I:I+1) .eq. '.0' ) I = I + 1
       NZ = NI - I
       If( IE .gt. LS ) Then
         STR = STR(1:I)
         If( STR(I:I) .eq. '.' ) Then
           LS = CLVFMT( 'F',LEN(STR),1,VAL,STR )
         End If
         Return
       End If
       Read( STR(IE+1:LS),* ) NP
       NE = I - INDEX( STR,'.' )
       If( NP.gt.0 .and. NP.le.+4 ) Then
         LS = CLVFMT( 'F',LEN(STR),MAX(1,ND-NP-NZ),VAL,STR )
       Else If( NP.eq.0 ) Then
         STR = STR(1:I)
       Else If( NP.lt.0 .and. NP.ge.-4 ) Then
         LS = CLVFMT( 'F',LEN(STR),MAX(1,ND-NP-NZ),VAL,STR )
       Else
         STR = STR(1:I)//STR(IE:)
       End If
       Return
       End
C
C Function: CLVFMT
C
       Integer Function CLVFMT( EDIT,NW,ND,VAL,STR )
       Implicit None
C
C ARG
       Character * (*)  EDIT
       Integer   *  4   NW,ND
       Real      *  4   VAL
       Character * (*)  STR
C
C VAR
       Integer * 4  L
       Character * 16  FORM
       Character * 16  STRING
C
C FUNC
       Integer * 4  LKBRD
C
C BEGIN
       STR = ' '
       If( EDIT.eq.'G' ) Then
         If( VAL.ge.0.1 .and. VAL.lt.10000.0 ) Then
           Write( FORM,100 ) ND
100        Format( '(F16.',I1,')' )
           Write( STRING,FORM ) VAL
         Else
           Write( FORM,110 ) ND
110        Format( '(E16.',I1,')' )
           Write( STRING,FORM ) VAL
         End If
       Else If( EDIT.eq.'F' ) Then
           Write( FORM,120 ) ND
120        Format( '(F16.',I1,')' )
         Write( STRING,FORM ) VAL
       End If
       L = LKBRD( STRING,0 )
       STR(:L) = STRING(:L)
       CLVFMT = L
       Return
       End
C
C
