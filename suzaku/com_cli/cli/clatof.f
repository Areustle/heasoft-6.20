C
C File: clatof.f
C Description: Collection of conversion routines between value and string
C Author: A.Shirahashi, KEK
C
C History:
C     07-Aug-1992, add CLdtoa
C     10-Aug-1992, Ivform,Ivfrmd -> CLfrmt
C     02-Feb-1992, add CLitoa
C     24-Feb-1993, bug fix: CLitoa
C
C Public : CLatof, CLatoi, CLftoa, CLdtoa, CLitoa
C Private: CLxtoa, CLfrmt
C
C Subroutine: CLatof
C Description: Convert string to Real value
C
      Subroutine CLatof( STRING,VALUE )
      Implicit None
C ARG
      Character * (*)  STRING
      Real * 4  VALUE
C
C VAR
      Real * 4  BODY, FPOWER
      Character * 1  C
      Integer * 4  L, K, IBSIGN, IPSIGN, IPOWER
C
C BEGIN
      L = LEN( STRING )
      IBSIGN = 1
      BODY = 0.0E0
      IPSIGN = 1
      IPOWER = 0
      K = 0
100   Continue
      K = K + 1
      If( K.gt.L ) Goto 9000
      If( STRING(K:K).eq.' ' ) Goto 100
      If( STRING(K:K).eq.'+' ) Then
        IBSIGN = 1
        K = K + 1
      Else If( STRING(K:K).eq.'-' ) Then
        IBSIGN = -1
        K = K + 1
      End If
110   If( K.gt.L ) Goto 9000
      If( STRING(K:K).ge.'0'.and.STRING(K:K).le.'9' ) Then
        BODY = BODY * 10.0E0 + FLOAT( ICHAR(STRING(K:K))-ICHAR('0') )
        K = K + 1
        Goto 110
      End If
      If( STRING(K:K).eq.' ' ) Goto 9000
      If( STRING(K:K).ne.'.' ) Goto 200
      FPOWER = 1.0E0
120   K = K + 1
      If( K.gt.L ) Goto 9000
      If( STRING(K:K).ge.'0'.and.STRING(K:K).le.'9' ) Then
        FPOWER = FPOWER / 10.0E0
        BODY = BODY + FLOAT( ICHAR(STRING(K:K))-ICHAR('0') ) * FPOWER
        Goto 120
      End If
      If( STRING(K:K).eq.' ' ) Goto 9000
200   Continue
      C = STRING(K:K)
      If( C.ne.'D'.and.C.ne.'E'.and.C.ne.'d'.and.C.ne.'e' ) Goto 9000
      K = K + 1
      If( K.gt.L ) Goto 9000
      If( STRING(K:K).eq.'+' ) Then
        IPSIGN = 1
        K = K + 1
      Else If( STRING(K:K).eq.'-' ) Then
        IPSIGN = -1
        K = K + 1
      End If
220   If( K.gt.L ) Goto 9000
      If( STRING(K:K).ge.'0'.and.STRING(K:K).le.'9' ) Then
        IPOWER = IPOWER * 10 + ICHAR(STRING(K:K)) - ICHAR('0')
        K = K + 1
        Goto 220
      End If
C
9000  VALUE = IBSIGN * BODY * ( 10 ** FLOAT( IPSIGN * IPOWER ) )
C     PRINT *,'IBSIGN,BODY,IPSIGN,IPOWER=',
C    &         IBSIGN,BODY,IPSIGN,IPOWER
      Return
      End
C
C Subroutine: CLatod
C Description: Convert string to Real*8 value
C
      Subroutine CLatod( STRING,VALUE )
      Implicit None
C ARG
      Character * (*)  STRING
      Real * 8  VALUE
C
C VAR
      Real * 8  BODY, FPOWER
      Character * 1  C
      Integer * 4  L, K, IBSIGN, IPSIGN, IPOWER
C
C BEGIN
      L = LEN( STRING )
      IBSIGN = 1
      BODY = 0.0D0
      IPSIGN = 1
      IPOWER = 0
      K = 0
100   Continue
      K = K + 1
      If( K.gt.L ) Goto 9000
      If( STRING(K:K).eq.' ' ) Goto 100
      If( STRING(K:K).eq.'+' ) Then
        IBSIGN = 1
        K = K + 1
      Else If( STRING(K:K).eq.'-' ) Then
        IBSIGN = -1
        K = K + 1
      End If
110   If( K.gt.L ) Goto 9000
      If( STRING(K:K).ge.'0'.and.STRING(K:K).le.'9' ) Then
        BODY = BODY * 10.0D0 + DBLE( ICHAR(STRING(K:K))-ICHAR('0') )
        K = K + 1
        Goto 110
      End If
      If( STRING(K:K).eq.' ' ) Goto 9000
      If( STRING(K:K).ne.'.' ) Goto 200
      FPOWER = 1.0D0
120   K = K + 1
      If( K.gt.L ) Goto 9000
      If( STRING(K:K).ge.'0'.and.STRING(K:K).le.'9' ) Then
        FPOWER = FPOWER / 10.0D0
        BODY = BODY + DBLE( ICHAR(STRING(K:K))-ICHAR('0') ) * FPOWER
        Goto 120
      End If
      If( STRING(K:K).eq.' ' ) Goto 9000
200   Continue
      C = STRING(K:K)
      If( C.ne.'D'.and.C.ne.'E'.and.C.ne.'d'.and.C.ne.'e' ) Goto 9000
      K = K + 1
      If( K.gt.L ) Goto 9000
      If( STRING(K:K).eq.'+' ) Then
        IPSIGN = 1
        K = K + 1
      Else If( STRING(K:K).eq.'-' ) Then
        IPSIGN = -1
        K = K + 1
      End If
220   If( K.gt.L ) Goto 9000
      If( STRING(K:K).ge.'0'.and.STRING(K:K).le.'9' ) Then
        IPOWER = IPOWER * 10 + ICHAR(STRING(K:K)) - ICHAR('0')
        K = K + 1
        Goto 220
      End If
C
9000  VALUE = IBSIGN * BODY * ( 10 ** DBLE( IPSIGN * IPOWER ) )
C     PRINT *,'IBSIGN,BODY,IPSIGN,IPOWER=',
C    &         IBSIGN,BODY,IPSIGN,IPOWER
      Return
      End
C
C Subroutine: CLatoi
C Description: Convert string to Integer value
C
      Subroutine CLatoi( STRING,VALUE )
      Implicit None
C ARG
      Character * (*)  STRING
      Integer  VALUE
C
C VAR
      Integer  SIGN
      Integer * 4  L, K
C
C BEGIN
      L = LEN( STRING )
      SIGN = 1
      VALUE = 0
      K = 0
100   Continue
      K = K + 1
      If( K.gt.L ) Goto 9000
      If( STRING(K:K).eq.' ' ) Goto 100
      If( STRING(K:K).eq.'+' ) Then
        SIGN = 1
        K = K + 1
      Else If( STRING(K:K).eq.'-' ) Then
        SIGN = -1
        K = K + 1
      End If
110   If( K.gt.L ) Goto 9000
      If( STRING(K:K).ge.'0'.and.STRING(K:K).le.'9' ) Then
        VALUE = VALUE * 10 + ICHAR(STRING(K:K)) - ICHAR('0')
        K = K + 1
        Goto 110
      End If
C
9000  VALUE = SIGN * VALUE
      Return
      End
C
C
C Subroutine: CLitoa
C Description: Convert Integer value to string
C
      Subroutine CLitoa( val,str )
      Implicit None
C input
      Integer  val
C output
      Character*(*)  str
C local
      Character * 12  str12
      Integer  L
C funcion
      Integer  Lkbrd
C begin
      str = ' '
      str12 = ' '
      Write(str12,*) val
      L = Lkbrd(str12,0)
      If( L.le.Len(str) ) Then
        str(1:L) = str12(1:L)
      Else
        str = '************'
      End If
C
      Return
      End
C
C
C Subroutine: CLftoa
C Description: Convert Real value to string
C
      Subroutine CLftoa( val,str )
      Implicit None
C input
      Real * 4   val
C output
      Character * (*)  str
C local
      Character * 12  strin
C function
      Integer  Lenrd
C begin
      str = ' '
      strin = ' '
C
      Call CLxtoa( Dble(val),6,strin )
C
      If( Lenrd(strin).gt.Len(str) ) Then
        strin = '************'
      End If
C
      If( Len(str).le.Len(strin) ) Then
        str = strin(:Len(str))
      Else
        str(:Len(strin)) = strin
      End If
C
      Return
      End
C
C
      Subroutine CLdtoa( val,str )
      Implicit None
C input
      Real * 8  val
C output
      Character * (*)  str
C local
      Character * 21  strin
C function
      Integer  Lenrd
C begin
      str = ' '
      strin = ' '
C
      Call CLxtoa( val,15,strin )
C
      If( Lenrd(strin).gt.Len(str) ) Then
        strin = '*********************'
      End If
C
      If( Len(str).le.Len(strin) ) Then
        str = strin(:Len(str))
      Else
        str(:Len(strin)) = strin
      End If
C
      Return
      End
C
C
      Subroutine CLxtoa( val,nd,str )
      Implicit None
C ARG
      Real * 8   val
      Integer * 4  nd
      Character * (*)  str
C
C VAR
      Integer * 4  I, LS, IE, NE, NP
C
C FUNC
      Integer * 4  CLfrmt
C
C BEGIN
       LS = CLfrmt( 'G',LEN(str),nd,val,str )
       IE = INDEX( str,'E' )
       If( IE .eq. 0 ) IE = LS + 1
       I = IE - 1
100    Continue
         If( str(I:I) .ne. '0' ) Goto 110
         I = I - 1
       Goto 100
110    Continue
       If( str(I:I+1) .eq. '.0' ) I = I + 1
       If( IE .gt. LS ) Then
         str = str(1:I)
         If( str(I:I) .eq. '.' ) Then
           LS = CLfrmt( 'F',LEN(str),1,val,str )
         End If
         Return
       End If
       Read( str(IE+1:LS),* ) NP
       If( NP .eq. 0 ) Then
         str = str(1:I)
         Return
       End If
       NE = I - INDEX( str,'.' )
       If( NP .lt. 0 .and. 6 - NE .ge. -NP ) Then
         LS = CLfrmt( 'F',LEN(str),NE-NP,val,str )
         Return
       End If
       str = str(1:I)//str(IE:)
       Return
       End

C
C Function: CLfrmt
C
      Integer Function CLfrmt( EDIT,NW,ND,val,str )
C
      Implicit None
C ARG
      Character * (*)  EDIT
      Integer   *  4   NW,ND
      Real      *  8   val
      Character * (*)  str
C
C VAR
      Character * 16  form
      Integer  Lform
C
C FUNC
      Integer * 4  Lkbrd
C
C BEGIN
       Write( FORM,1000 ) EDIT,NW,Min(NW-5,ND)
1000   Format( '(',A,I2,'.',I2,')' )
       Lform = Lkbrd(form,0)
       Write( str,form(:Lform) ) val
       CLfrmt = Lkbrd( str,0 )
       Return
       End
C
C
