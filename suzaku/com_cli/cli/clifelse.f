C
C File: clifelse.f
C Description: CLI if/elif/else/endif Support Routines
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     20-Feb-2005 Y.ISHISAKI, created for if/elif/else/endif
C
C Public: CLifexpr
C
C ---------------------------------------------------------
C ... check keyword for if/elif condition
C     supported keywords are NOT EQ NE GT GE LT LE EQS NES
C ---------------------------------------------------------
      Subroutine CLifkey( WORD, KEY )
      Implicit None
C input
      Character * (*)  WORD
C output
      Character * (*)  KEY
C begin
      KEY = WORD
      Call CLstrupc( LEN(KEY), KEY )
C
      If ( 'NOT' .eq. KEY ) Return
      If ( 'EQ'  .eq. KEY ) Return
      If ( 'NE'  .eq. KEY ) Return
      If ( 'GT'  .eq. KEY ) Return
      If ( 'GE'  .eq. KEY ) Return
      If ( 'LT'  .eq. KEY ) Return
      If ( 'LE'  .eq. KEY ) Return
      If ( 'EQS' .eq. KEY ) Return
      If ( 'NES' .eq. KEY ) Return
C
      KEY = ' '         ! no match
C
      Return
      End
C
C
C ---------------------------------------------------------
C ... eval expression for if/elif condition
C     supported keywords are NOT EQ NE GT GE LT LE EQS NES
C ---------------------------------------------------------
      Logical Function CLifexpr( LINE )
      Implicit None
C common
      include 'clidef.inc'
      Include 'clunit.inc'
C input
      Character * (*)  LINE
C local
      Character * 8  KEY, OPER
      Logical  RESULT, FLAG_NOT
      Integer  I, LAST, IPOS, IEND, LPOS, RPOS, LEND, REND
      Real * 8  Lval, Rval
C begin
      LAST = LEN(LINE)
ccc      Write (*,*) 'LINE=', LINE
C
      I = 1
      FLAG_NOT = .FALSE.
C
C cut out left value
C
 10   Continue
      Call CLsubp2(LINE(:LAST), I, IPOS, IEND)
      I = I + 1
C
      If ( IPOS.le.0 ) Then             ! no arguments, assumed as FALSE
        RESULT = .FALSE.
        If ( FLAG_NOT ) RESULT = .NOT. RESULT
        CLifexpr = RESULT
        Return
      End If
C
ccc      Write (*,*) 'I=', I, ' S1=', LINE(IPOS:IEND)
      Call CLifkey(LINE(IPOS:IEND), KEY)
C
      If ( 'NOT' .eq. KEY ) Then
        FLAG_NOT = .NOT. FLAG_NOT
        Goto 10
      Else If ( ' ' .ne. KEY ) Then
        Call CLIerr(0,
     &       'reserved keyword found at wrong position, ignored')
        Goto 10
      End If
C
      LPOS = IPOS
      LEND = IEND
      Call CLatod( LINE(LPOS:LEND), Lval )      ! get left value
C
C cut out operator
C
      Call CLsubp2(LINE(:LAST), I, IPOS, IEND)
      I = I + 1
C
      If ( IPOS.le.0 ) Goto 80          ! use only left value
C
ccc      Write (*,*) 'I=', I, ' S2=', LINE(IPOS:IEND)
      Call CLifkey(LINE(IPOS:IEND), KEY)
C
      If ( 'NOT' .eq. KEY ) Then
        FLAG_NOT = .NOT. FLAG_NOT
      Else If ( ' ' .ne. KEY ) Then
        OPER = KEY
      Else If ( ' ' .eq. KEY ) Then
ccc        Call CLIerr(0, 'no comparison operator found, ignored')
        Goto 80                         ! use only left value
      End If
C
C cut out right value
C
 30   Continue
      Call CLsubp2(LINE(:LAST), I, IPOS, IEND)
      I = I + 1
C
      If ( IPOS.le.0 ) Then             ! only left value & operator
        Call CLIerr(0, 'right value no found, using left value only')
        Goto 80
      End If
C
ccc      Write (*,*) 'I=', I, ' S3=', LINE(IPOS:IEND)
      Call CLifkey(LINE(IPOS:IEND), KEY)
C
      If ( 'NOT' .eq. KEY ) Then
        FLAG_NOT = .NOT. FLAG_NOT
        Goto 30
      Else If ( ' ' .ne. KEY ) Then
        Call CLIerr(0, 'duplicated operator found, ignored')
        Goto 80                         ! use only left value
      End If
C
      RPOS = IPOS
      REND = IEND
      Call CLatod( LINE(RPOS:REND), Rval )      ! get left value
C
C eval operator
C
ccc      Write (*,*) 'Lval=', Lval, '  Rval=', Rval, '  OPER='//OPER
      If ( 'EQ' .eq. OPER ) Then
        RESULT = (Lval .eq. Rval)
      Else If ( 'NE' .eq. OPER ) Then
        RESULT = (Lval .ne. Rval)
      Else If ( 'GT' .eq. OPER ) Then
        RESULT = (Lval .gt. Rval)
      Else If ( 'GE' .eq. OPER ) Then
        RESULT = (Lval .ge. Rval)
      Else If ( 'LT' .eq. OPER ) Then
        RESULT = (Lval .lt. Rval)
      Else If ( 'LE' .eq. OPER ) Then
        RESULT = (Lval .le. Rval)
      Else If ( 'EQS' .eq. OPER ) Then
        RESULT = ( LINE(LPOS:LEND) .eq. LINE(RPOS:REND) )
      Else If ( 'NES' .eq. OPER ) Then
        RESULT = ( LINE(LPOS:LEND) .ne. LINE(RPOS:REND) )
      Else
        Call CLIerr(0, 'unknown operator found, may be bug')
        RESULT = .FALSE.
      End If
C
      If ( FLAG_NOT ) RESULT = .NOT. RESULT
      CLifexpr = RESULT
      Return
C
 80   Continue                          ! use only left value
ccc      Write (*,*) 'Lval=', Lval
      RESULT = ( 0.D0 .ne. Lval )
      If ( FLAG_NOT ) RESULT = .NOT. RESULT
      CLifexpr = RESULT
      Return
      End
