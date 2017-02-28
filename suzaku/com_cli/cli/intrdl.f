C
C File: intrdl.f
C Description: Collection of "data input and conversion" routins
C
C Public: Intrd,  Fltrd,  Fdprd,  Hexrd
C         IntrdX, FltrdX, FdprdX, HexrdX
C         IntrdL, FltrdL, FdprdL, HexrdL
C
C Target: UN*X (also portable on VMS)
C
C Note: This version is portable because it does not use NARGET.
C
C History:
C     09-Jul-1997, Y.ISHISAKI, add IntrdX, FltrdX, FdprdX, HexrdX
C     11-Nov-2005, Y.ISHISAKI, answer*16 -> (LINSIZ) in Intrd,Fltrd,Fdprd,Hexrd
C
      Subroutine Intrd( QUEST,VALUE )
      Implicit None
C
      Include 'clidef.inc'
C
C input
      Character * (*)  quest
C (IntrdL)
      Integer * 4  minim, maxim
C input/output
      Integer * 4  value
C output (IntrdX)
      Character * (*) text
C local
      Character * (LINSIZ)  answer
      Character * 1  K
      Integer  Narg
      Real * 8  F
C
C..... CHANGE INTEGER
C
      Narg = 2
      Goto 1
C
      Entry IntrdX( quest,value,text )
C
      Narg = 3
      Goto 1
C
      Entry IntrdL( quest,value,minim,maxim )
C
      Narg = 4
      Goto 1
C
 1    Continue
C
      If (quest(1:1).eq.'?') VALUE = 0
      If (Narg.ge.4) VALUE = MAX( MINIM,VALUE )
      If (Narg.ge.4) VALUE = MIN( MAXIM,VALUE )
C      Write ( ANSWER,* ) VALUE
      Call CLitoa(value,answer)
 3    Call ARGRD(quest,ANSWER)
      F = VALUE
      If (Narg.eq.3) Then
         Call NUMRD(F,10,K,TEXT)
      Else
         Call NUMRD(F,10,K,ANSWER)
      End If
      If (K.eq.BSLA) Goto 3
      VALUE = MIN(MAX(-2147483648.D0,F),2147483647.D0)
      If (Narg.ge.4) VALUE = MAX(MINIM,VALUE)
      If (Narg.ge.4) VALUE = MIN(MAXIM,VALUE)
      Return
      End
C
C
      Subroutine Fltrd(QUEST,VALUE)
      Implicit None
C
      Include 'clidef.inc'
C
      Character * (*)  QUEST, TEXT
      Character * (LINSIZ)  ANSWER
      Character * 1  K
      Real*4 VALUE, MINIM, MAXIM
C local
      Integer  Narg
      Real * 8  F
C
C..... CHANGE REAL*4 FLOATING
C
      Narg = 2
      Goto 1
C
      Entry FltrdX(QUEST,VALUE,TEXT)
C
      Narg = 3
      Goto 1
C
      Entry FltrdL(QUEST,VALUE,MINIM,MAXIM)
C
      Narg = 4
      Goto 1

 1    Continue
C
      If (QUEST(1:1).eq.'?') VALUE = 0
      If (Narg.ge.4) VALUE = MAX(MINIM,VALUE)
      If (Narg.ge.4) VALUE = MIN(MAXIM,VALUE)
C     Write (ANSWER,101) VALUE
C101  Format (1PG16.6)
C      If (ANSWER(12:16).eq.'0E+00') ANSWER(12:16) = ' '
      Call CLftoa( value, answer )
 3    Call ARGRD(QUEST,ANSWER)
      F = VALUE
      If (Narg.eq.3) Then
         Call NUMRD(F,10,K,TEXT)
      Else
         Call NUMRD(F,10,K,ANSWER)
      End If
      If (K.eq.BSLA) Goto 3
      VALUE = F
      If (Narg.ge.4) VALUE = MAX(MINIM,VALUE)
      If (Narg.ge.4) VALUE = MIN(MAXIM,VALUE)
      Return
      End
C
C

      Subroutine FDPRD( QUEST,VALUE )
      Implicit None
C
      Include 'clidef.inc'
C
      Character * (*)  QUEST, TEXT
      Character * 1  K
      Character * (LINSIZ)  ANSWER
      Real * 8  VALUE, MINI, MAXI
      Real * 8  F
      Integer  Narg
C
C..... CHANGE REAL*8 FLOATING
C
      Narg = 2
      Goto 1
C
      Entry FDPRDX( QUEST,VALUE,TEXT )
C
      Narg = 3
      Goto 1
C
      Entry FDPRDL( QUEST,VALUE,MINI,MAXI )
C
      Narg = 4
      Goto 1
C
 1    Continue
C
      If (QUEST(1:1).eq.'?') VALUE = 0.0D0
      If (Narg.ge.4) VALUE = MAX(MINI,VALUE)
      If (Narg.ge.4) VALUE = MIN(MAXI,VALUE)
C     Write (ANSWER,101) VALUE
C101  Format (1PG16.6)
C     If (ANSWER(12:16).eq.'0E+00') ANSWER(12:16) = ' '
      Call CLdtoa(value,answer)
 3    Call ARGRD(QUEST,ANSWER)
      F = VALUE
      If (Narg.eq.3) Then
         Call NUMRD(F,10,K,TEXT)
      Else
         Call NUMRD(F,10,K,ANSWER)
      End If
      If (K.eq.BSLA) Goto 3
      VALUE = F
      If (Narg.ge.4) VALUE = MAX(MINI,VALUE)
      If (Narg.ge.4) VALUE = MIN(MAXI,VALUE)
C     CALL SET_NO_HELP
      Return
      End
C
C
      Subroutine HEXRD( QUEST,VALUE )
      Implicit None
C
      Include 'clidef.inc'
C
      Character * (*)  QUEST, TEXT
      Character * 1  K
      Character * (LINSIZ)  ANSWER
      Integer*4 VALUE, MINIM, MAXIM
      Real*8 F
      Integer  Narg
C
C..... CHANGE INTEGER
C
      Narg = 2
      Goto 1
C
      Entry HEXRDX( QUEST,VALUE,TEXT )
C
      Narg = 3
      Goto 1
C
      Entry HEXRDL( QUEST,VALUE,MINIM,MAXIM )
C
      Narg = 4
      Goto 1
C
 1    Continue
C
      If (QUEST(1:1).eq.'?') VALUE = 0
      If (Narg.ge.4) VALUE = MAX( MINIM,VALUE )
      If (Narg.ge.4) VALUE = MIN( MAXIM,VALUE )
      Write ( ANSWER,101 ) VALUE
 101  Format (I16)
 3    Call ARGRD(QUEST,ANSWER)
      F = VALUE
      If (Narg.eq.3) Then
         Call NUMRD(F,16,K,TEXT)
      Else
         Call NUMRD(F,16,K,ANSWER)
      End If
      If (K.eq.BSLA) Goto 3
      VALUE = MIN(MAX(-2147483648.D0,F),2147483647.D0)
      If (Narg.ge.4) VALUE = MAX(MINIM,VALUE)
      If (Narg.ge.4) VALUE = MIN(MAXIM,VALUE)
      Return
      End
