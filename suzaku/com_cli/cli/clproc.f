C
C File: clproc.f (formerly in clgetl.f)
C Description: Various support routines for command procedure
C Author: A.Shirahashi, Univ. of Tokyo
C
C History:
C     22-Feb-2005 Y.ISHISAKI, created for command procedure
C     22-Feb-2005 Y.ISHISAKI, CLCURL,CLIEOF,CLSTOP,CLSUSP,CLRESM from clgetl.f
C     27-Feb-2005 Y.ISHISAKI, set back current arguments in CLeof & CLstop
C     02-Mar-2005 Y.ISHISAKI, check for if in CLIEOF by checking IF_LUN
C     09-Mar-2005 Y.ISHISAKI, use CLeixt() instead of Exit()
C     11-Nov-2005 Y.ISHISAKI, Character * 8 -> 16  LUNSTR in CLIEOF
C
C Public:
C     CLCURL            ... return current filename, line number and contents
C     CLIEOF            ... End of File detected
C     CLSTOP            ... abort command procedure
C     CLSUSP            ... suspend the execuation of the command procedure
C     CLRESM            ... resume the execuation of the command procedure
C     Logical CLBREAK   ... set/clear/return Ctrl-C break status
C
C ----------
C   CLCURL   ... return current filename, line number and contents
C ----------
      Subroutine CLCURL( LUNP, LINUMB, LINBUF )
      Implicit NONE
C ARG
      Integer * 4  LUNP, LINUMB
      Character * (*)  linbuf
C Common
      include 'clidef.inc'
      include 'clunit.inc'
C BEGIN
      LUNP = LUNPTR
      If( LUNP.gt.0 ) Then
        LINUMB = FILINE(LUNPTR)
        LINBUF = FILBUF
      End If
C
      Return
      End
C
C ----------
C   CLIEOF   ... End of File detected
C ----------
      Subroutine CLIEOF
      Implicit NONE
C
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
      include 'clitxt.inc'
C
      Character * 16   LUNSTR
      Integer  I, LUN, LQ, IERR, LL
      Logical  FLAG_SUB
C
      Logical  CLItty
      Integer  Lenrd
C
C BEGIN
      If( LUNPTR .eq. 0 ) Then
        If( .not.CLItty() ) Then
          Call CLIerr( 2,'End of File on console input, aborted' )
          Call CLstop
          Call CLexit(1)
        Else
          Call CLIerr( 1,'End of File on console input, why ?' )
        End If
        Return
      End If
ccc        Call CLARGP
      LUN = LUNLST(LUNPTR)
      Call CLitoa(LUNLST(LUNPTR), LUNSTR)
      LL = Lenrd(LUNSTR)
C ... check for subroutine
      FLAG_SUB = .FALSE.
      Do I = 1, NSUB
        If ( LUN.eq.SUB_LUN(I) ) Then
          FLAG_SUB = .TRUE.
        End if
      End Do
C ... close file unless subroutine
      IERR = 0
      If ( .not. FLAG_SUB ) Then
        Call CLclos( LUN,IERR )
C ... free lun
        If ( IERR.eq.0 ) Call CLfreelun( LUNLST(LUNPTR) )
      End If
      If( IERR .ne. 0 ) Then
        Call CLIERR( 0,'can''t close command file (lun='//
     &       LUNSTR(:LL)//'), ignored' )
      Else If( Opt_ECHO.ne.0 ) Then
        Call CLMKPR( cQUEST,cANSWE,cQUECOM,cQUES,LQ )
        If( cQUES(LQ:LQ).eq.' ' ) LQ = LQ - 1
C          Write( 6,'(1H ,A,A,A)') QUES(1:LQ),' ','(End OF FILE)'
        If ( LUNLST(LUNPTR) .ge. LUNMIN ) Then
          Call CLputL(6,
     &         cQUES(:LQ)//' ('//LUNSTR(:LL)//': End of File)')
        End If
      End If
C ... check for if
      Do While ( IF_DEPTH.gt.0 .and. IF_LUN(IF_DEPTH).eq.LUN )
        If ( 'x' .ne. ISKIP_BEFORE_IF(IF_DEPTH) ) Then
          ISKIP = 0
        Else
          ISKIP = 1
        End If
        IF_DEPTH = IF_DEPTH - 1
      End Do
C ... check for loop
      If ( LOOP_DEPTH.gt.0 .and. LOOP_LUN(LOOP_DEPTH).eq.LUN ) Then
        LOOP_DEPTH = LOOP_DEPTH - 1
        LOOP_READ_DEPTH = LOOP_DEPTH
      End If
      LUNPTR = LUNPTR - 1
C ... set back current arguments
      Call CLargs( ARGLST(LUNPTR)(:LARGS(LUNPTR)),' ',LL )
      Call CLlabF               ! flush label
      If ( LUNPTR.eq.LPTRSV ) Then
        LUNPTR = 0              ! back to console at previous suspended level
      End If
C
       Return
       End
C
C ----------
C   CLSTOP   ... abort command procedure
C ----------
      Subroutine CLstop
      Implicit None
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C local
      Integer  I, Ierr
C begin
C ... unset skip mode for comment & if block
      ISKIP = 0
C ... check for suspended command procedure
      If ( LPTRSV.gt.LUNPTR ) Then
        LUNPTR = LPTRSV
        LPTRSV = 0
      End If
C ... close all command files
      Do I = LUNPTR, 1, -1
        Call CLIEOF
      End Do
C ... cancel all loops (actually, this is already done in CLIEOF)
      LOOP_DEPTH = 0
      LOOP_READ_DEPTH = 0
C ... cancel IF/ELIF/ELSE/ENDIF block
      IF_DEPTH = 0
C ... restore input stream
      LUNPTR = 0
      LPTRSV = 0
C ... set back current arguments
      Call CLargs( ARGLST(0)(:LARGS(0)),' ',I )
C ... flush all labels
      Call CLlabF
C ... end of redirection
      Call CLsyso(5,'SYS$INPUT','UNKNOWN',Ierr)
C ... discard remaining command line
      Call CLerok
C
      Return
      End
C
C ----------
C   CLSUSP   ... suspend the execuation of the command procedure
C ----------
      Subroutine CLSUSP
      Implicit NONE
C Common
      include 'clidef.inc'
      include 'clunit.inc'
C
C BEGIN
      LPTRSV = LUNPTR
      LUNPTR = 0
C
      Return
      End
C
C ----------
C   CLRESM   ... resume the execuation of the command procedure
C ----------
      Subroutine CLRESM
      Implicit NONE
C Common
      include 'clidef.inc'
      include 'clunit.inc'
C
C BEGIN
      LUNPTR = LPTRSV
      LPTRSV = 0
C
      Return
      End
C
C ----------
C   CLBREAK   ... set/clear/return Ctrl-C break status
C ----------
      Logical  Function CLBREAK( FLAG_ON_OFF, FLAG_CLEAR )
      Implicit NONE
C input
      Integer * 4  FLAG_ON_OFF          ! 1:ON, 0:OFF, -1:READ ONLY
      Integer * 4  FLAG_CLEAR           ! 0: DO NOT CLEAR
C common
      include 'clflag.inc'
C function
      Logical  CLsigC
C begin
      If ( 1.eq.FLAG_ON_OFF ) Then
        If ( 0.ne.Opt_BREAK ) Then
          Call CLIerr(0, 'break is already ON')
        Else
          Call CLsigI( 2 )      ! capture ^C
          Opt_BREAK = 1
        End If
      Else If ( 0.eq.FLAG_ON_OFF ) Then
        If ( 0.eq.Opt_BREAK ) Then
          Call CLIerr(0, 'break is not ON')
        Else
          Call CLsigX( 2 )      ! restore original signal hander
          Opt_BREAK = 0
        End If
      End If
C
      If ( 0.eq.Opt_BREAK ) Then
        CLBREAK = .FALSE.
        Return
      End If
C
      CLBREAK = CLsigC( 2 )     ! check if ^C is pressed
      If ( 0.ne.FLAG_CLEAR .and. CLBREAK ) Then
        Call CLsigR( 2 )        ! clear ^C signal flag
      End If
C
      Return
      End
