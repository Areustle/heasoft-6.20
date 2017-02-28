C
C File: logrd.f
C Description: Routines to ask yes or no.
C
C History:
C     29-Sep-1994, PROMPT -> CLprom
C     14-Oct-1998 Y.ISHISAKI, just call LOGRD() when non-TTY in AFFIRM()
C     22-Feb-2005 Y.ISHISAKI, remove using 'tvdisp.inc', LTVLOC in AFFIRM()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C Public: Logrd, Affirm
C
      Subroutine LOGRD(QUEST,L)
      Implicit None
C common
      include 'clidef.inc'
      include 'clunit.inc'
C ARG
      Logical L
      Character QUEST*(*)
C local
      Character * 1  M
      Character * (LINSIZ) WORK
      Integer  lq
C BEGIN
      LQ = LEN(QUEST)
      WORK = QUEST
C
C..... accept Logical value
C
 10   Continue
      If( L ) Then
        M = 'Y'
      Else
        M = 'N'
      End If
C
      Call TXTRD(WORK(1:LQ)//' (Y/N)',M)
      Call CLstrupc(1,M)
      If (M.eq.'Y' .or. M.eq.'T' .or. M.eq.'1') Then
        L = .TRUE.
      Else If (M.eq.'N' .or. M.eq.'F' .or. M.eq.'0') Then
        L = .FALSE.
      Else
        Call CLEROK()           ! discard remaining command line
        Goto 10
      End If
      Return
      End
C
C
      Subroutine AFFIRM( QUEST,L )
      Implicit None
C common
      include 'clidef.inc'
C
C .... Show prompt on console and inquire yes/no
C      3-DEC-1986, A.Shirahashi
C
C ARG
      Logical   *  4   L
      Character * (*)  QUEST
C
C VAR
      Character *  1  M
      Character * (LINSIZ)  WORKS
      Integer   *  4  LQ, LM
C FUNC
      Logical * 4  CLITTY
C
C BEGIN
      If ( .not. CLITTY() ) Then
        Call Logrd( QUEST, L )
        Return
      End If
C
      LQ = LEN(QUEST)
      WORKS = QUEST
C
 10   Continue
      If( L ) Then
        M = 'Y'
      Else
        M = 'N'
      End If
C
      Call CLrdln(WORKS(1:LQ)//' (Y/N): '//M//' ? ', M, LM)
C
      Call CLstrupc(1,M)
      If (M.eq.'Y' .or. M.eq.'T' .or. M.eq.'1') Then
        L = .TRUE.
      Else If (M.eq.'N' .or. M.eq.'F' .or. M.eq.'0') Then
        L = .FALSE.
      Else
        Goto 10
      End If
C
      Return
      End
