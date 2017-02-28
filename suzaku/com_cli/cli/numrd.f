C
C File: numrd.f
C Description: Convert string to double value
C History:
C     09-Jul-1997, Y.ISHISAKI, add TEXT arg, regard TAB as SPACE
C     09-Jul-1998, Y.ISHISAKI, check QUOTE
C
      Subroutine NUMRD(F,IRAD,K,TEXT)
      Implicit None
C constatnt
      include 'clidef.inc'
C input
      Integer  IRAD
C output
      Real * 8  F
      Character * 1  K
      Character * (*)  TEXT
C local
      Real * 8  RES, UNIT, BASE
      Real  SIG
      Integer  IRES, ISIG, M, ITEXT
      Logical START, CHANGD, ICHANGD, TERMIN, EXPON, FRCTN
      Character * 1  K1, K2, QUOTED
C data
      Character * 16  DIGIT
      Data DIGIT/'0123456789ABCDEF'/
C begin
      START = .FALSE.
      CHANGD = .FALSE.
      ICHANGD = .FALSE.
      TERMIN = .FALSE.
      EXPON = .FALSE.
      FRCTN = .FALSE.
      SIG = 1.0
      RES = 0.0D0
      IRES = 0
      ISIG = 1
      UNIT = 1.0D0
      ITEXT = 1
      TEXT = ' '
      QUOTED = ' '
C
 10   Continue
      Call CHARD(K1,K2)
      If ( QUOTED.eq.' ' ) Then
         If (K1.eq.'"' .or. K1.eq.QUOTE) Then
            QUOTED = K1
            Goto 10
         End If
      Else If ( K1.eq.QUOTED ) Then
         QUOTED = ' '
         Goto 10
      End If
      K = K1
      Call CLstrupc(1,K)
      If (K.eq.BSLA) Then
         Return
      Else If (K.eq.',' .or. K.eq.CRET) Then
         Goto 20                ! immediate end
      Else If (K.eq.SPACE .or. K.eq.TAB) Then
         If (START) Then
            TERMIN = .TRUE.
            If ( QUOTED.eq.' ' ) Then
               Goto 30          ! end if something is input
            Else If ( ITEXT .le. LEN(TEXT) ) Then
               TEXT(ITEXT:ITEXT) = K1
               ITEXT = ITEXT + 1
            End If
         End If
      Else
         START = .TRUE.         ! something is input
         If ( ITEXT .le. LEN(TEXT) ) Then
            TEXT(ITEXT:ITEXT) = K1
            ITEXT = ITEXT + 1
         End If
         If (TERMIN) Goto 10    ! ignore following char if already terminate
         M = INDEX(DIGIT(1:IRAD),K) - 1
         If (EXPON) Then        ! 1.0E**
            If (M.ge.0) Then    ! 1.0E[0-9]
               ICHANGD = .TRUE.
               IRES = IRES*IRAD + M
            Else If (ICHANGD) Then  ! 1.0E1x
               TERMIN = .TRUE.
            Else If (K.eq.'+') Then ! 1.0E+
               ISIG = + ISIG
            Else If (K.eq.'-') Then ! 1.0E-
               ISIG = - ISIG
            Else                ! 1.0Ex
               TERMIN = .TRUE.
            End If
         Else
            If (M.ge.0) Then    ! [0-9]
               CHANGD = .TRUE.
               If (FRCTN) Then  ! 1.[0-9]
                  UNIT = UNIT/IRAD
                  RES = RES + UNIT*M
               Else             ! 1[0-9]
                  RES = RES*IRAD + M
               End If
            Else If (K.eq.'.') Then             ! 1.
               If (FRCTN) TERMIN = .TRUE.       ! 1..
               FRCTN = .TRUE.
            Else If (CHANGD) Then
               If (K.eq.'D' .or. K.eq.'E') Then ! 1.0E
                  EXPON = .TRUE.
               Else
                  TERMIN = .TRUE.               ! 1.0x
               End If
            Else If (K.eq.'+') Then             ! +
               SIG = + SIG
            Else If (K.eq.'-') Then             ! -
               SIG = - SIG
            End If
         End If
      End If
      Goto 10
C
 30   Continue
C
      If (K2.eq.SPACE .or. K2.eq.TAB) Then
         Call CHARD(K,K2)
         Goto 30
      End If
C
 20   BASE = IRAD
      RES = SIG*RES*BASE** (ISIG*IRES)
      If (CHANGD) F = RES
C
      Return
      End
