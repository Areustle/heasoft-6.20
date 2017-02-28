C
C File: clgetl.f
C Description: Read one logical line from terminal or input stream
C Author: A.Shirahashi, Univ. of Tokyo
C
C History:
C     27-Jul-1992, continuation mark '-' -> '\'
C     10-Aug-1992, #define sym exp -> @sym=exp
C     21-Oct-1992, ! at first column is not treated as comment
C     22-Oct-1992, do not call CLhins if ! at first column
C     27-Oct-1992, pseude CR code '\\'
C     05-Dec-1992, export CLilvl, CLilun to flgrd.f
C     26-Apr-1993, support for GNU readline (QUICK HACK!)
C     27-Apr-1993, CLgets -> CLgets, CLargr, CLinco
C     24-Feb-1994, @! bug fixed, remove CLtget
C     12-Mar-1994, line1: 80 -> 256, buffer: 80 -> *
C     29-Sep-1994, PROMPT -> CLprom
C     30-Sep-1994, \!, !!
C     11-Jan-1995, symbol replacement in @$
C     30-Nov-1995, don't replace '%' if no argument found
C     30-Jul-1996 Y.ISHISAKI, CLSUSP, CLRESM
C     11-Feb-1997 Y.ISHISAKI, support '@/*', '@*/' for commenting
C     11-Feb-1997 Y.ISHISAKI, support '@+'/'@-' or '@++'/'@--'
C     11-Feb-1997 Y.ISHISAKI, support '@%%' and '@##'
C     22-Feb-1997 Y.ISHISAKI, add @. commands
C               alias, cd, chdir, exit, resume, setenv, suspend, unalias
C     25-Feb-1997 Y.ISHISAKI, support '@ command |'
C     25-Feb-1997 Y.ISHISAKI, check LPTRSV in CLIEOF & CLICMD
C     25-Feb-1997 Y.ISHISAKI, resume only when console input by '@<'
C     25-Feb-1997 Y.ISHISAKI, count FILINE & set FILBUF for console input
C     26-Feb-1997 Y.ISHISAKI, change prompt for debug mode
C     26-Feb-1997 Y.ISHISAKI, add 'C' & '?' command for debug mode
C     26-Feb-1997 Y.ISHISAKI, add '@.status','@.abort','@.debug' command
C     26-Feb-1997 Y.ISHISAKI, set LPTRSV=0 in CLstop
C     26-Feb-1997 Y.ISHISAKI, call CLIERR if CLI command not found
C     26-Feb-1997 Y.ISHISAKI, add '@.help' command
C     26-Feb-1997 Y.ISHISAKI, regards '@./test' (have '/') as com file
C     08-Jul-1998 Y.ISHISAKI, remove QTEMP variable in CLMKPR
C     12-Jul-1998 Y.ISHISAKI, expand CLI variables in @ file / filter |
C     16-Jul-1998 Y.ISHISAKI, MSG*32 -> 256 in CLIEOF()
C     18-Jul-1998 Y.ISHISAKI, print @*/ in com file
C     14-Oct-1998 Y.ISHISAKI, accept only "@./file" or "@../file"
C     14-Oct-1998 Y.ISHISAKI, remove duplicated message of "Can't open 'a.com'"
C     16-Feb-2005 Y.ISHISAKI, add '@.sleep' command
C     17-Feb-2005 Y.ISHISAKI, expand CLI variables first, except @$ command
C     20-Feb-2005 Y.ISHISAKI, use Fsystem() for @$ instead of IPFCMD()
C     20-Feb-2005 Y.ISHISAKI, set CLI variable "?" after Fsystem(), Fexecp()
C     21-Feb-2005 Y.ISHISAKI, enable echo for "@*"
C     21-Feb-2005 Y.ISHISAKI, do not call CLhins if empty line
C     21-Feb-2005 Y.ISHISAKI, calling CLinco() moved from CLgetl() to ARGRD()
C     21-Feb-2005 Y.ISHISAKI, do not check LUNPTR in CLinco(), instead in ARGRD
C     21-Feb-2005 Y.ISHISAKI, show '-' on top when ISKIP in CLgetl()
C     21-Feb-2005 Y.ISHISAKI, CLIerr() moved to clecho.f
C     21-Feb-2005 Y.ISHISAKI, CLTRML() deleted, and stop using ITRML in CLGETS
C     21-Feb-2005 Y.ISHISAKI, CLWLOG & CLILOG moved to cllog.f
C     21-Feb-2005 Y.ISHISAKI, Call CLMKLOG, CLCLLOG, CLRdOUT for @(, @), @>
C     21-Feb-2005 Y.ISHISAKI, handling COMMENTS moved from CLGETL to CLICMD
C     22-Feb-2005 Y.ISHISAKI, CLICMD moved to clicmd.f
C     22-Feb-2005 Y.ISHISAKI, change EOF message condition, LUNBASE -> LUNMIN
C     22-Feb-2005 Y.ISHISAKI, CLargR moved to clargs.f
C     22-Feb-2005 Y.ISHISAKI, remove LMOD in CLGETL()
C     22-Feb-2005 Y.ISHISAKI, remove using 'tvdisp.inc', LTVLOC in CLGETL()
C     22-Feb-2005 Y.ISHISAKI, remove using LASK in CLGETL(), instead LTSSIN
C     22-Feb-2005 Y.ISHISAKI, call CLerok in CLstop, instead of IFLQ,IPNT,IFCR
C     22-Feb-2005 Y.ISHISAKI, LAST = LEN(LINE) for truncated too long line
C     22-Feb-2005 Y.ISHISAKI, CLstar() moved to clflag.f
C     22-Feb-2005 Y.ISHISAKI, CLCURL,CLIEOF,CLSTOP,CLSUSP,CLRESM to clproc.f
C     23-Feb-2005 Y.ISHISAKI, remember last console input for console arguments
C     25-Feb-2005 Y.ISHISAKI, call CLhins() only when LTSSIN=T, console mode
C     25-Feb-2005 Y.ISHISAKI, insert '-' to the prompt when ISKIP, in CLMKPR()
C     26-Feb-2005 Y.ISHISAKI, rename ISNGL -> Opt_DEBUG, check for 0.ne.Opt
C     26-Feb-2005 Y.ISHISAKI, CLINCO moved to chard.f
C     27-Feb-2005 Y.ISHISAKI, use Fgetc() in CLgets
C     27-Feb-2005 Y.ISHISAKI, add CLputs() which use Fputc()
C     02-Mar-2005 Y.ISHISAKI, replace arguments moved to chard.f
C     02-Mar-2005 Y.ISHISAKI, disable console mode arguments
C     02-Mar-2005 Y.ISHISAKI, check history command in console mode
C     02-Mar-2005 Y.ISHISAKI, check FLAG_SUB_READING for 'sub:' prompt
C     09-Mar-2005 Y.ISHISAKI, special treatment when LUN=5 in CLgets()
C     09-Mar-2005 Y.ISHISAKI, use CLfgetc/CLfputc/CLfseek not Fgetc/Fputc/Fseek
C     09-Mar-2005 Y.ISHISAKI, CLGETS(), CLPUTS() moved to miscunix.f
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C     21-Oct-2005 Y.ISHISAKI, check LAST1.gt.0 before reading LINE1 in CLGETL()
C
C Public:
C     CLGETL            ... read one logical line
C     CLMKPR            ... build prompt string
C
C ----------
C   CLGETL   ... read one logical line
C ----------
      Subroutine CLGETL( LINE,LAST )
      Implicit None
C ARG
C     (Ouput)
      Character * (*)  LINE
      Integer   *  4   LAST
C
C Common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
      include 'clitxt.inc'
C
C VAR
      Logical * 4  LECHO, LTSSIN
      Character * (LINSIZ)  LINE1
      Character * 80  ans
      Integer * 4  Last1, LQ, Lans, Ists
C
C FUNC
      Integer * 4  CLgets, Lenrd
      Logical * 4  CLITTY, CLBREAK
C
C BEGIN
10    Continue
      LUNIN = LUNLST(LUNPTR)
      If( ICOM .eq. 1 ) LUNIN = 5
      LTSSIN = LUNIN.eq.5 .and. CLITTY()
      LECHO = (.not.LTSSIN) .and. (Opt_ECHO.ne.0)
C
      LAST = 0
C
 100  Continue
C
      If ( FLAG_SUB_READING.ne.0 ) Then
        cQUES = 'sub: '
        LQ = 5
      Else If ( LOOP_READ_DEPTH.gt.LOOP_DEPTH ) Then
        cQUES = 'loop: '
        LQ = 6
      Else
        Call CLMKPR( cQUEST,cANSWE,cQUECOM,cQUES,LQ )
      End If
C
      Call CLsigUnsetMask       ! for DFARTL360 bug (maybe)
C ... read one line
      If( LTSSIN ) Then
        Call CLrdln( cQUES(1:LQ),Line1,Last1 )
C       Write(*,*) '1:',Last1,'[',Line1(:Last1),']'
        If( Last1.lt.0 ) Then
          Last = -1
          Return
        End If
      Else
        Ists = CLGETS( LUNIN,Line1,Last1 )
        If( Ists.lt.0 ) Then
          Last = -1
          Return
        End If
      End If
      FILINE(LUNPTR) = FILINE(LUNPTR) + 1       ! count up line number
      FILBUF = Line1(:Last1)                    ! remember current line
C ... local echo of command procedure
      If ( LECHO .and.
     &     .not.(LUNPTR.ne.0 .and. Opt_DEBUG.ne.0) .and.
ccc  21-Feb-2005 Y.ISHISAKI, enable echo for "@*" & "@*/"
ccc     &     .not.(LAST1.ge.2 .and. LINE1(1:2).eq.'@*') .and.
     &     .not.(LAST1.ge.2 .and. LINE1(1:2).eq.'@-') ) Then
        LQ = LENRD( cQUES )
        If ( ISKIP.ne.0 ) Then
          Write ( *, '(A)' ) cQUES(:LQ)//' '//LINE1(:LAST1)
        Else
          Call CLputL( 6, cQUES(:LQ)//' '//LINE1(:LAST1) )
        End If
      End If
C ... check ^C
      If ( ISKIP.eq.0 .and. CLBREAK(-1,1) ) Then
        If ( 0.eq.LUNPTR ) Then
          Call CLIerr( 0, 'Ctrl-C detected (ignored)' )
        Else
          Call CLsusp           ! suspend command execution
          Call CLIerr(0,'*********************************************')
          Call CLIerr(0,'  Command procedure interrupted by Ctrl-C. ')
          Call CLIerr(0,'  You can resume it by the CLI command "@<".')
          Call CLIerr(0,'*********************************************')
        End If
      End If
C
ccc  21-Feb-2005 Y.ISHISAKI, this line must be comment out to enable history
ccc      If ( Line1(1:1) .EQ. '!' .OR. Line1(1:2) .EQ. '@!' ) Goto 100
C ... remove inline comment
ccc  21-Feb-2005 Y.ISHISAKI, this line is moved to ARGRD(), after CLgetl()
ccc      Call CLinco( Line1,Last1 )
C     Write(*,*) '2:',Last1,'[',Line1(:Last1),']'
C ... replace arguments
ccc  02-Mar-2005 Y.ISHISAKI, following lines are moved to ARGRD()
ccc      Call CLargR( Line1,Last1,Line2,Last2 )
ccc      If ( Last1.ne.Last2 .or. Line1(:Last1).ne.Line2(:Last2) ) Then
ccc        Line1 = Line2
ccc        Last1 = Last2
ccc        If ( 0.eq.LUNPTR ) Then
ccc          Call CLputL(6,Line1(:Last1))
ccc        End If
ccc      End If

C     Write(*,*) '3:',Last1,'[',Line1(:Last1),']'
C
      If( LUNPTR.ne.0 .and. Opt_DEBUG.ne.0 ) Then
 500    Continue
        Call CLprom( cQUES(1:LQ)//' '//Line1(:Last1)
     &       //' => (G,S,R,A,C) ? ' )
        ISTS = CLGETS( 5,ans,LANS )
        Call CLstrupc( 1,ans )
        If( LANS.eq.0 .or. ans(1:1).eq.'G' ) Then
        Else If( ans(1:1).eq.'S' ) Then
          Goto 100
        Else If( ans(1:1).eq.'R' ) Then
          Call CLprom( cQUES(1:LQ) )
          ISTS = CLGETS( 5,LINE1,LAST1 )
        Else If( ans(1:1).eq.'A' ) Then
          Call CLSTOP()
          Goto 10
        Else If( ans(1:1).eq.'C' ) Then
          Opt_DEBUG = 0
        Else If( ans(1:1).eq.'?' .or. ans(1:1).eq.'H' ) Then
          Write (*,*) 'Go(default), Skip, Read, Abort, Continue'
          Goto 500
        End If
      End If
C ... write command input into log file
      If( INHLOG.eq.0 .and. LAST1.gt.0 .and. LINE1(1:1).ne.'@' ) Then
        Call CLWLOG( LINE1(:LAST1) )
      End If
C... check for concatenation with '\' at the line end
      ICONCAT = 0
      If ( Opt_BSLASH.ne.0 .and. LAST1.gt.0 .and.
     &     LINE1(LAST1:LAST1).eq.CONTMARK ) Then
        ICONCAT = 1
        LAST1 = LAST1 - 1
      End If
      If( LAST+LAST1 .gt. LEN(LINE) ) Then
        Call CLIerr(2, 'too long line, truncated')
        LINE(LAST+1:) = LINE1(1:LAST1)
        LAST = LEN(LINE)
        Return
      End If
      If( LAST1 .ge. 1 ) Then
        LINE(LAST+1:LAST+LAST1) = LINE1(:LAST1)
        LAST = LAST + LAST1
      End If
      If( ICONCAT .eq. 1 ) Goto 100
C ... check history command in console mode
      If ( LTSSIN ) Then
        If ( LINE(:LAST).eq.'!' ) Then
          Call CLhall
          LAST = 0
          Goto 100
        Else If ( Index(LINE(:LAST), '!').gt.0 ) Then
          Call CLheval( LINE(:LAST),LINE1,LAST )
          LINE = LINE1
          Call CLputL(6,LINE(:LAST))
        End If
C ... put command history
        If( LAST.gt.0 ) Then
          Call CLhins( LINE(:LAST) )
        End if
      End If
C
      Return
      End
C
C ----------
C   CLMKPR   ... build prompt string
C ----------
      Subroutine CLMKPR( QUEST,ANSWER,QUECOM,QUES,LQ )
      Implicit NONE
C
C ARG
      Character * (*)  QUEST, ANSWER, QUECOM
      Character * (*)  QUES
      Integer   *  4   LQ
C
C Common
      include 'clflag.inc'
C
C VAR
      Integer I, LL, LC
C
C FUNCTION
      Integer Lenrd, Lkbrd
C
C BEGIN
      LQ = LENRD( QUEST )
      If( QUEST(1:1).eq.'?' ) Then
        If( INDEX(QUEST,'>')+INDEX(QUEST(2:),'?').eq.0 ) Then
          QUES = QUEST(2:LQ)//' ? '
        Else
          QUES = QUEST(2:LQ)
        End If
        ANSWER = ' '
      Else
        LL = LKBRD( ANSWER,LEN(ANSWER) )
        If( INDEX( QUEST,'>' )+INDEX( QUEST,':' ) .eq. 0 ) Then
          QUES = QUEST(1:LQ)//': '//ANSWER(1:LL)//' ? '
        Else
          QUES = QUEST(1:LQ)//' '//ANSWER(1:LL)//' ? '
        End If
      End If
C
      LQ = LENRD( QUES )
      LQ = LQ + 1
      QUES(LQ:LQ) = ' '
C insert '*' on temporary reading from console
      If( ICOM .eq. 1 ) Then
        Do I=Len(QUES)-1, 1, -1
          QUES(I+1:I+1) = QUES(I:I)
        End Do
        QUES(1:1) = '*'
        LQ = LQ + 1
        LC = LENRD( QUECOM )
        If( LC .gt. 0 ) Then
          QUES(LQ+1:LQ+LC) = QUECOM(1:LC)
          LQ = LQ + LC
          If( QUES(LQ:LQ) .ne. ' ' ) Then
            LQ = LQ + 1
            QUES(LQ:LQ) = ' '
          End If
        End If
      End If
C insert '-' on @/* or @.if skip
      If( ISKIP .eq. 1 ) Then
        Do I=Len(QUES)-1, 1, -1
          QUES(I+1:I+1) = QUES(I:I)
        End Do
        QUES(1:1) = '-'
        LQ = LQ + 1
      End If
C insert '_' on concatenation
      If( ICONCAT .eq. 1 ) Then
        Do I=Len(QUES)-1, 1, -1
          QUES(I+1:I+1) = QUES(I:I)
        End Do
        QUES(1:1) = '_'
        LQ = LQ + 1
      End If
C
      Return
      End
