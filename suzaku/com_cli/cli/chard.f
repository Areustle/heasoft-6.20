C
C File: chard.f
C Description: Chard and related routines.
C
C Public:
C     CHARD     ... get one Character
C     ARGRD     ... get a new line
C     UGETRD    ... put string into input queue
C     CLinco    ... deprive inline comment
C
C Historty:
C     27/JUL/89  A.Shirahashi, PF4 as 'resume execution'
C     16/SEP/89  A.Shirahashi, add Subroutine OPNRD
C     18/SEP/89  A.Shirahashi, @% as '%output off' and @# as '%output on'
C
C     26/SEP/89  A.Shirahashi, add CLIMEM Function
C     16/NOV/89  A.Shirahashi, add CLCURL Subroutine
C
C     10-Jun-92  A.Shirahashi, fix a bug around LOPNRD
C                              move LINRD,FLARD,FLGRD to FLGRD.FOR
C     15-Jun-92, A.Shirahashi, rename ISATTY -> CLITTY
C                              move CLHINS,CLHREC,CLHALL to CLHINS.FOR
C     16-Jun-92, A.Shirahashi, add LUNRD, rename CLINSQ to UGETRD
C                              split into 2 files: CHARD.FOR and CLGETL.FOR
C                              remove many unused variables
C     26-Jun-92, A.Shirahashi, fix a bug in UGETRD
C     11-Aug-92, A.Shirahahsi, do not tread '\' as input cancel.
C     20-Oct-92, Call CLhcmd if ! at first column
C     26-Oct-92, Call CLmacr in Argrd
C     01-Apr-94, Add Opt_AT,Opt_ECHO,Opt_EOF in CLIBLK, Check OPT_AT in ARGRD
C     27-May-1998 Y.ISHISAKI, add CLIDEF block data
C     21-Feb-2005 Y.ISHISAKI, call CLinco() here, after CLgetl()
C     21-Feb-2005 Y.ISHISAKI, check LUNPTR before calling CLhcmd()
C     21-Feb-2005 Y.ISHISAKI, delete initialization for 'cliio.inc'
C     22-Feb-2005 Y.ISHISAKI, delete IRD,IWR,HELSET,TTCHAN,TLUN,KLUN initialize
C     22-Feb-2005 Y.ISHISAKI, delete Entry CTZRD()
C     23-Feb-2005 Y.ISHISAKI, remove using IFATN(),RSTATN() in ARGRD(),UGETRD()
C     23-Feb-2005 Y.ISHISAKI, block data CLIBLK -> CLDATA moved to cldata.f
C     23-Feb-2005 Y.ISHISAKI, include 'cldata.f', got a problem with OSF1
C     25-Feb-2005 Y.ISHISAKI, CLinco() [deprive inline comments] after CLICMD()
C     25-Feb-2005 Y.ISHISAKI, ignore @.if/elif/else/endif inside comment
C     26-Feb-2005 Y.ISHISAKI, rename LINE/LAST -> c*/c*, moved into clitxt.inc
C     26-Feb-2005 Y.ISHISAKI, change ARGRD,UGETRD,CLlast Entry -> Subroutine
C     26-Feb-2005 Y.ISHISAKI, CLINCO moved from clgetl.f
C     26-Feb-2005 Y.ISHISAKI, CLlast moved to clflag.f
C     27-Feb-2005 Y.ISHISAKI, call CLputs() instead of Write (LUN,'(A)')
C     02-Mar-2005 Y.ISHISAKI, replace arguments moved from clgetl.f
C     02-Mar-2005 Y.ISHISAKI, history command interpretation moved to clgetl.f
C     02-Mar-2005 Y.ISHISAKI, add @.sub, @.endsub support
C     09-Mar-2005 Y.ISHISAKI, use CLfseek() instead of Fseek()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C *********************************************************************
C
C ---------
C   CHARD   ... get one Character
C ---------
      Subroutine ChaRD( K,K2 )
      Implicit None
C constant
      include 'clidef.inc'
C common
      include 'clflag.inc'
      include 'clitxt.inc'
C arg
C (Chard)
      Character *  1   K, K2
C local
      Integer  L
C begin
      If( IPNT.gt.cLAST ) Goto 880
C
C ... replace a symbol $(symbol)
      If( Opt_DOLLAR.ne.0 .and. cLINE(IPNT:IPNT).eq.'$' ) Then
        L = cLAST - IPNT + 1
        Call CLmacS(cLINE(IPNT:),L)
        cLAST = IPNT + L - 1
      End If
C
      K = cLINE(IPNT:IPNT)
      IPNT = IPNT + 1
      K2 = cLINE(IPNT:IPNT)
C
      Return
C
 880  Continue
      K = CRET
      K2 = K
C
      Return
      End
C
C ---------
C   ARGRD   ... get a new line
C ---------
      Subroutine ARGRD( QUESTN,ANSWER )
      Implicit None
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
      include 'clitxt.inc'
C input
      Character * (*)  QUESTN, ANSWER
C local
      Integer  Last1, Ldsn, L, Ierr
      Character * (LINSIZ)  Line1, DSNAME
C function
      Integer  Lunrd, CLputs
      Logical  CLifexpr
C begin
 10   Continue
      cQUEST = QUESTN
      cANSWE = ANSWER
C
C ... check if the buffer is exausted.
      If( IPNT.ne.0 .and. IPNT.gt.cLAST .and. IFCR.ne.0 ) Then
        IPNT = 0
        IFLQ = 1
      End If
C
C ... If buffer is not empty or empty but IFCR != 0, then return.
      If( IPNT.ne.0 ) Then
        Return
      End If
C
 100  Continue
C ... get one Logical line
      Call CLgetL( cLINE,cLAST )
C ... check if reading subroutine
      If ( FLAG_SUB_READING.ne.0 ) Then
        Call CLeval( cLINE(:cLAST), LINE1, LAST1 )
        If ( Line1(1:2).eq.'@.' ) Then
          Call CLpart2(LINE1(3:LAST1), 1, DSNAME, Ldsn)
          Call CLstrupc(Ldsn, DSNAME)
          If ( 'SUB'.eq.DSNAME ) Then
            Call CLIerr( 0, 'not usable in subroutine, ignored' )
            Goto 10
          Else If ( 'ENDSUB'.eq.DSNAME ) Then
            FLAG_SUB_READING = 0
            Goto 100
          End If
        End If
        If ( 0.ne.CLputs(SUB_LUN(NSUB), cLINE(:cLAST)) ) Then
          Call CLstop
          Call CLIerr( 2, 'error writing a temporary file' )
          Goto 10
        End If
        Goto 100
      End If
C ... check if reading loop body
      If ( LOOP_READ_DEPTH.gt.LOOP_DEPTH ) Then
ccc        Write (LOOP_LUN(LOOP_DEPTH), '(A)') cLINE(:cLAST)
        If ( 0.ne.CLputs(LOOP_LUN(LOOP_DEPTH), cLINE(:cLAST)) ) Then
          Call CLstop
          Call CLIerr( 2, 'error writing a temporary file' )
          Goto 10
        End If
        Call CLeval( cLINE(:cLAST), LINE1, LAST1 )
        If ( Line1(1:2).eq.'@.' ) Then
          Call CLpart2(LINE1(3:LAST1), 1, DSNAME, Ldsn)
          Call CLstrupc(Ldsn, DSNAME)
          If ( 'WHILE'.eq.DSNAME ) Then
            If ( LOOP_READ_DEPTH.ge.MAXLOOP ) Then
              Call CLstop
              Call CLIerr( 2, 'too many loop nested' )
              Goto 10
            End If
            LOOP_READ_DEPTH = LOOP_READ_DEPTH + 1
            LOOP_CMD(LOOP_READ_DEPTH) = 'W'
          Else If ( 'REPEAT'.eq.DSNAME ) Then
            If ( LOOP_READ_DEPTH.ge.MAXLOOP ) Then
              Call CLstop
              Call CLIerr( 2, 'too many loop nested' )
              Goto 10
            End If
            LOOP_READ_DEPTH = LOOP_READ_DEPTH + 1
            LOOP_CMD(LOOP_READ_DEPTH) = 'R'
          Else If ( 'END'.eq.DSNAME ) Then
            If ( 'W'.ne.LOOP_CMD(LOOP_READ_DEPTH) ) Then
              Call CLstop
              Call CLIerr(2, 'while/end mismatch')
              Goto 10
            Else
              LOOP_READ_DEPTH = LOOP_READ_DEPTH - 1
            End If
          Else If ( 'UNTIL'.eq.DSNAME ) Then
            If ( 'R'.ne.LOOP_CMD(LOOP_READ_DEPTH) ) Then
              Call CLstop
              Call CLIerr(2, 'repeat/until mismatch')
              Goto 10
            Else
              LOOP_READ_DEPTH = LOOP_READ_DEPTH - 1
            End If
          End If
          If ( LOOP_READ_DEPTH.eq.LOOP_DEPTH ) Then
            LOOP_STAT(LOOP_DEPTH) = 's'                 ! loop start
            Call CLfseek(LOOP_LUN(LOOP_DEPTH), 0, 0)
            Ierr = Lunrd( LOOP_LUN(LOOP_DEPTH) )
          End If
        End If
        Goto 100
      End If
C
      If( cLAST.ge.3 .and. cLINE(1:3) .eq. '@/*' ) Then
C ... Start comment by "@/*"
        If ( 0.ne.FLAG_COMMENT ) Then
          Call CLIerr(0, 'found nested comment, ignored')
        End If
        ISKIP_BEFORE_COMMENT = ISKIP
        ISKIP = 1
        FLAG_COMMENT = 1
        Goto 100                ! read again
      End If
C
      If ( ISKIP.ne.0 ) Then
        Call CLeval( cLINE(:cLAST), LINE1, LAST1 )
        If ( FLAG_COMMENT.ne.0 ) Then
C ... check if inside comment block
          If ( Line1(1:3).eq.'@*/' ) Then
C ... End comment by "@*/"
            FLAG_COMMENT = 0
            ISKIP = ISKIP_BEFORE_COMMENT
          End If
          Goto 100              ! read again
        Else If ( IF_DEPTH.ne.0 .and. Line1(1:2).eq.'@.' ) Then
          Call CLpart2(LINE1(3:LAST1), 1, DSNAME, Ldsn)
          Call CLstrupc(Ldsn, DSNAME)
C ... if/elif/else/endif commands with ISKIP=1
          If ( 'IF'.eq.DSNAME ) Then
            If ( IF_DEPTH.ge.MAXIF ) Then
              Call CLstop
              Call CLIerr( 2, 'too many if blocks nested' )
              Goto 10
            Else
              IF_DEPTH = IF_DEPTH + 1
              IF_LUN(IF_DEPTH) = LUNLST(LUNPTR)
              FLAG_IGN_ELSE(IF_DEPTH) = 'x'     ! not evaluate elif/else
              ISKIP_BEFORE_IF(IF_DEPTH) = 'x'   ! ISKIP=1
            End If
          Else If ( 'ELIF'.eq.DSNAME ) Then
            If ( 'e' .eq. FLAG_IGN_ELSE(IF_DEPTH) ) Then
              Call CLIerr( 0, 'elif after else, ignored' )
              Goto 10
            Else If ( 'z' .eq. FLAG_IGN_ELSE(IF_DEPTH) ) Then
              Call CLsubp(LINE1(3:LAST1), 2, L)
              L = L + 2
              If ( CLifexpr( LINE1(L:LAST1) ) ) Then
                FLAG_IGN_ELSE(IF_DEPTH) = 'x'   ! not evaluate elif/else
                ISKIP = 0
                Goto 100        ! read again
              End If
            End If
          Else If ( 'ELSE'.eq.DSNAME ) Then
            If ( 'e' .eq. FLAG_IGN_ELSE(IF_DEPTH) ) Then
              Call CLIerr( 0, 'else after else, ignored' )
              Goto 10
            Else If ( 'z' .eq. FLAG_IGN_ELSE(IF_DEPTH) ) Then
              FLAG_IGN_ELSE(IF_DEPTH) = 'e'     ! else comes, wait for endif
              ISKIP = 0
              Goto 100          ! read again
            Else
              FLAG_IGN_ELSE(IF_DEPTH) = 'e'     ! else comes, wait for endif
            End If
          Else If ( 'ENDIF'.eq.DSNAME ) Then
            If ( 'x' .ne. ISKIP_BEFORE_IF(IF_DEPTH) ) Then
              ISKIP = 0
            End If
            IF_DEPTH = IF_DEPTH - 1
            Goto 100            ! read again
          End If
        End If
        Goto 100                ! read again
      End If                    ! (ISKIP.ne.0)
C
      ICOM = 0                  ! stop temporary reading from console
C
C ... check If this line is EOF
      If( cLAST .lt. 0 ) Then
        Call CLIEOF
        Goto 300                ! get next line
      End If
C
ccc 200  Continue
      IPNT = 1
      IFLQ = 0                  ! reading from internal memory
C
C ... replace arguments (obsolete)
ccc  02-Mar-2005 Y.ISHISAKI, following lines are moved from CLgetl()
      If ( Opt_PERCENT.ne.0 ) Then
        Call CLargR( cLINE, cLAST, LINE1, Last1 )
        If ( cLAST.ne.LAST1 .or. cLINE(:cLAST).ne.LINE1(:LAST1) ) Then
          cLINE = LINE1
          cLAST = LAST1
          If ( 0.eq.LUNPTR ) Then
            Call CLputL(6, cLINE(:cLAST))
          End If
        End If
      End If
C ... parse CLI command
      If( cLAST.gt.0 .and. cLINE(1:1).eq.'@' .and. Opt_AT.ne.0 ) Then
        Call CLICMD( cLINE(:cLAST),cLAST )
        Goto 300                ! get next line
C ... expand command line history
ccc  02-Mar-2005 Y.ISHISAKI, history command interpretation moved to clgetl.f
ccc      Else If( cLAST.gt.0 .and. cLINE(1:1).eq.'!' ) Then
ccc  21-Feb-2005 Y.ISHISAKI, this line is moved from CLinco()
ccc   command line history is only usable in cosole mode
ccc   otherwise, treated as a comment
ccc        If ( LUNPTR .ne. 0 ) Then
ccc          Goto 300              ! get next line
ccc        End If
ccc        Call CLhcmd( cLINE,cLAST )
ccc        If ( cLAST.le.0 ) Then
ccc          If ( cLAST.lt.0 ) Then ! just '!' is OK for history list
ccc            Call CLIerr(0, 'no matching history')
ccc          End If
ccc          Goto 300              ! get next line
ccc        Else
ccc          Call CLputL(6,cLINE(:cLAST))
ccc          Goto 200              ! eval again
ccc        End If
      End If
C
C ... remove inline comment here
ccc  21-Feb-2005 Y.ISHISAKI, this line is moved from CLgetl()
ccc  25-Feb-2005 Y.ISHISAKI, moved after evaluating '@',
ccc    otherwise, '@!' will be interpreted as carriage return '\n'
      If ( Opt_INCO.ne.0 ) Then
        Call CLinco( cLINE, cLAST )
      End if
C
C ... Return to caller
      Return
C
C ... get next line
 300  Continue
      IPNT = 0
      IFLQ = 1
      cLAST = 0
      Goto 100
      End
C
C ----------
C   UGETRD   ... put string into input queue
C ----------
      Subroutine UGETRD( LINEIQ, LENIQ )
      Implicit None
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clitxt.inc'
C input
      Character * (*)  LINEIQ
      Integer   *  4   LENIQ
C local
      Integer  L
C begin
      If( IPNT.gt.0 .and. cLAST.ge.IPNT ) Then
        L = Leniq + 1 + cLAST - IPNT + 1
        cLINE(cLAST+1:cLAST+L) = Lineiq(1:Leniq)//' '//cLine(IPNT:cLAST)
        cLINE(1:L) = cLINE(cLAST+1:cLAST+L)
        cLAST = L
      Else
        cLINE(1:Leniq) = Lineiq(1:Leniq)
        cLast = Leniq
      End If
C
      If ( cLAST+1.lt.LEN(cLINE) ) Then
        cLINE(cLAST+1:) = ' '
      End If
C
      IPNT = 1
      IFLQ = 0
C
      Return
      End
C
C --------
C  CLinco  ... deprive inline comment
C --------
      Subroutine CLinco( buffer,length )
      Implicit None
C common
      Include 'clidef.inc'
      Include 'clunit.inc'
C input/output
      Character * (*)  buffer
      Integer  length
C local
      Integer  i
C function
ccc      Integer  Lenrd
C begin
      If( length.eq.0 ) Return
C
      i = Index( buffer(:length),'@!' )
      If( i.gt.0 ) Then
        buffer(i:) = ' '
        length = i - 1
      End If
C
      i = 0
 100  Continue
        i = i + 1
        If( i.gt.length ) Goto 200
        If( buffer(i:i).ne.'!' ) Goto 100
        If( i.gt.1 ) Then
          If( buffer(i-1:i-1).eq.BSLA ) Then
            buffer(i-1:length-1) = buffer(i:length)
            buffer(length:length) = ' '
            length = length - 1
            Goto 100
          End If
        End If
        If( i.lt.length ) Then
           If( buffer(i+1:i+1).eq.'!' ) Then
              buffer(i:length-1) = buffer(i+1:length)
              buffer(length:length) = ' '
              length = length - 1
              Goto 100
           End If
        End If
ccc
ccc 21-Feb-2005 Y.ISHISAKI, check LUNPTR in ARGRD
ccc        If( i.gt.1 .or. lunptr.gt.0 ) Then
        If( i.gt.1 ) Then
          buffer(i:) = ' '
          length = i - 1
        End If
 200  Continue
C
ccc      length = Lenrd( buffer )
      Return
      End
C
C ----------
C   CLDATA   ... include block data for initialization of common variables
C ----------
      include 'cldata.f'
C
