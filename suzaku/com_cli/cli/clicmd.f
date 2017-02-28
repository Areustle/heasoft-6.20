C
C File: clicmd.f (formerly in clgetl.f)
C Description: Execute CLI Local Command
C Author: A.Shirahashi, Univ. of Tokyo
C
C Routines: CLICMD
C
C History:
C     22-Feb-2005 Y.ISHISAKI, CLICMD moved from clgetl.f
C     23-Feb-2005 Y.ISHISAKI, show status with sole '@'
C     26-Feb-2005 Y.ISHISAKI, rename ISNGL -> Opt_DEBUG
C     26-Feb-2005 Y.ISHISAKI, add information message on "@?" (toggle debug)
C     26-Feb-2005 Y.ISHISAKI, allow setting $? by user
C     26-Feb-2005 Y.ISHISAKI, do not change Opt_DEBUG status on "@ file"
C     27-Feb-2005 Y.ISHISAKI, call CLputs() instead of Write (LUN,'(A)')
C     01-Mar-2005 Y.ISHISAKI, print message when symbol contains ' ' or '!'
C     02-Mar-2005 Y.ISHISAKI, add '@.path' command
C     02-Mar-2005 Y.ISHISAKI, use CLfindpath() for '@ file'
C     02-Mar-2005 Y.ISHISAKI, add '@.return [VAL]' command
C     02-Mar-2005 Y.ISHISAKI, add '@.sub/call' support
C     06-Mar-2005 Y.ISHISAKI, set Ierr & error message when CLfindpath() fails
C     09-Mar-2005 Y.ISHISAKI, use CLgetenv/CLfseek/CLexit not Getenv/Fseek/Exit
C     26-Mar-2005 Y.ISHISAKI, call CLinco in normal symbol assign
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C ----------
C   CLICMD   ... Execute CLI Local Command
C ----------
      Subroutine CLICMD( LINE0,LAST0 )
      Implicit None
C constant
      include 'clidef.inc'
C common
      include 'clflag.inc'
      include 'clunit.inc'
      include 'clitxt.inc'
C input
      Character * (*)  LINE0
      Integer   *  4   LAST0
C local
      Logical  LOGON
      Character * (LINSIZ)  LINE1, DSNAME, path
      Integer  LUN, Lcmd, Ldsn, Lpath, Ierr, status
      Integer  LAST1, pos1, pos2
      Real * 8  DSEC
C function
      Integer  Lenrd, CLexitcode, CLputs
      Logical * 4  CLITTY, CLifexpr
C begin
C ... set flag to log this line (basically ON, bug OFF when command failed)
      LOGON = .TRUE.
C ... must start with '@'
      If ( '@'.ne.LINE0(1:1) ) Return
C ... show status with sole '@'
      If ( LAST0.lt.2 ) Then
        Call CLstatus(' ')
        Return
      End If
C ... expand CLI variables
      If( LINE0(2:2).ne.'$' ) Then
        Call CLeval( LINE0(:LAST0), LINE1, LAST1 )
      Else
C ... "@$" need special treatment
        LINE0(2:2) = ' '
        Call CLeval( LINE0(:LAST0), LINE1, LAST1 )
        LINE0(2:2) = '$'
        LINE1(2:2) = '$'
      End If
C ... Execute TSS Command
      If( LINE1(2:2).eq.'$' ) Then
        Call cli__Fsystem( LINE1(3:LAST1) )
        Call CLseti( '?', CLexitcode() )
        Goto 999
      End If
C ... get end position of the @xxx command
      Call CLsubp2( LINE1(:LAST1), 1, pos1, Lcmd )
ccc      LOGON = .FALSE.
C
C%%   If( ICOM .eq. 1 ) Then
C%%   Call CLIERR( 2,'unexpected CLI command' )
C%%   Else If( LINE1(2:2) .eq. '(' ) Then
      If( LAST1.ge.3 .and. LINE1(2:3) .eq. '//' ) Then
C ... Show message
        Write (*,'(A)') LINE1(4:LAST1)
      Else If( LINE1(2:2) .eq. '(' ) Then
C ... Open Log File
        Call CLpart2( LINE1(3:LAST1), 1, path, Lpath )
        Call CLmklog( path(:Lpath), IERR )
      Else If( LINE1(2:2) .eq. ')' ) Then
C ... Close Log File
        Call CLcllog( IERR )
      Else If( LINE1(2:3) .eq. '*/' ) Then
        Call CLIerr ( 0, 'end of comment without ''@/*'', ignored' )
      Else If( LINE1(2:2) .eq. '*' ) Then
C ... Inquire String from Console in Command File
        If( LUNPTR.eq.0 .and. CLITTY() ) Then
          Call CLIERR( 1,'inquire in direct mode, why ?' )
        Else
          If( LAST1 .ge. 3 ) Then
            cQUECOM = LINE1(3:LAST1)
          Else
            cQUECOM = ' '
          End If
          ICOM = 1
        End If
      Else If( LINE1(2:2).eq.'<' ) Then
C ... suspend/resume the execuation of the command procedure
        If( LUNPTR.eq.0 ) Then  ! console
          Call CLresm
        Else                    ! command file
          Call CLsusp
        End If
      Else If( LINE1(2:2).eq.'!' ) Then
C ... Comment
C ... Do Nothing
ccc        LOGON = .TRUE.
      Else If( LINE1(2:2) .eq. '>' ) Then
C ... Redirect standard output to file
ccc        LOGON = .TRUE.
        Call CLpart2( LINE1(3:LAST1), 1, path, Lpath )
        Call CLRdOut( path(:Lpath), IERR )
        If( IERR .ne. 0 ) Then
          LOGON = .FALSE.
        End If
      Else If( LINE1(2:2).eq.'?' .and. LINE1(3:3).ne.'=' ) Then
C ... Debug On/Off
ccc        LOGON = .TRUE.
        If ( 0.ne.Opt_DEBUG ) Then
          Opt_DEBUG = 0
          Call CLIerr(0, "debug off")
        Else
          Opt_DEBUG = 1
          Call CLIerr(0, "debug on")
        End If
      Else If( LINE1(2:2) .eq. '#' ) Then
C ... Turn outputs on with/without info message.
        Call CLsyso(6,'SYS$OUTPUT','NEW',status)
        If ( LAST1.eq.2 .or. LINE1(3:3) .ne. '#' ) Then
          Call CLIerr(0, 'output on')
        End If
      Else If( LINE1(2:2) .eq. '%'
C ... allow user to modify $%n
     &       .and. 0.eq.Index(LINE1(:Lcmd), '=') ) Then
C ... Turn outputs off with/without info message.
        If ( LAST1.eq.2 .or. LINE1(3:3) .ne. '%' ) Then
          Call CLIerr(0, 'output off, use "@#" to on')
        End If
        Call CLsyso(6,'NLA0:','NEW',status)
      Else If( LINE1(2:2) .eq. '-' ) Then
C ... Turn echo off with/without info message.
        Call CLIopt('ECHO', 0)
        If ( LAST1.eq.2 .or. LINE1(3:3) .ne. '-' ) Then
          Call CLIerr(0, 'echo off, use "@+" to on')
        End If
      Else If( LINE1(2:2) .eq. '+' ) Then
C ... Turn echo on with/without info message.
        Call CLIopt('ECHO', 1)
        If ( LAST1.eq.2 .or. LINE1(3:3) .ne. '+' ) Then
          Call CLIerr(0, 'echo on')
        End If
      Else If( LINE1(2:2) .eq. ':' ) Then
C ... Set label         "@: label:"
        Call CLlabR( LINE1(3:LAST1), 1 )
      Else If( LINE1(2:2) .eq. '=' .and. LAST1.gt.2 ) Then
C ... Set argument      "@=arg0 arg1 arg2 ..."
        Call CLargs( LINE1(3:LAST1),' ',Ldsn )
      Else If ( (LINE1(2:2) .eq. '.')
     &       .and. (LINE1(2:3).ne.'./')
     &       .and. (LINE1(2:4).ne.'../') ) Then
C ... CLI special command
        Call CLpart2(LINE1(3:LAST1), 1, DSNAME, Ldsn)
        Call CLstrupc(Ldsn, DSNAME)
        If ( 'ABORT'.eq.DSNAME ) Then
          Call CLstop
        Else If ( 'ALIAS'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          If ( 0.eq.Ldsn ) Then
            Call CLaliP
          Else
            Call CLpart2(LINE1(3:LAST1), 3, path, Lpath)
            If ( 0.eq.Lpath ) Then
              Ldsn = Lenrd(DSNAME)
              Call CLaliL( DSNAME(:Ldsn),path,Lpath )
              If ( Lpath.le.0 ) Then
                Call CLIerr(1, 'unknown alias: '//DSNAME(:Ldsn))
              Else
                Call CLecho(path(:Lpath))
              End If
            Else
              Lpath = Lenrd(path)
              Call CLaliI(DSNAME(:Ldsn), path(:Lpath))
            End If
          End If
        Else If ( 'CD'.eq.DSNAME .or. 'CHDIR'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, path, Lpath)
          Call CLchdir(path(:Lpath))
        Else If ( 'EXIT'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, path, Lpath)
          Call CLatoi(path(:Lpath), status)
          Call CLstop
          Call CLexit(status)
        Else If ( 'GOTO'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          Call CLlabJ(DSNAME(:Ldsn))
        Else If ( 'HELP'.eq.DSNAME ) Then
          Call CLhelp
        Else If ( 'HISTORY'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, path, Lpath)
          Call CLphis(path(:Lpath))
        Else If ( 'PATH'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          If ( 0.eq.Ldsn ) Then
            Call CLstatus('PATH')
          Else
            Call CLsetpath( DSNAME(:Ldsn) )
          End If
        Else If ( 'RESUME'.eq.DSNAME ) Then
          Call CLresm
        Else If ( 'RETURN'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          If ( Ldsn.gt.0 ) Then
            Call CLsetv('?', DSNAME(:Ldsn))
          End if
          Call CLIEOF
        Else If ( 'SET'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          Call CLpart2(LINE1(3:LAST1), 3, path, Lpath)
          Call CLIset( DSNAME(:Ldsn), path(:Lpath) )
        Else If ( 'SHOW'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          Call CLstatus( DSNAME(:Ldsn) )
        Else If ( 'STATUS'.eq.DSNAME ) Then
          Call CLstatus(' ')
        Else If ( 'SETENV'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          If ( 0.eq.Ldsn ) Then
            Call cli__Fsystem('printenv')
          Else
            Call CLpart2(LINE1(3:LAST1), 3, path, Lpath)
            If ( ' '.eq.path ) Then
              Call CLgetenv( DSNAME(:Ldsn), path )
              Lpath = Lenrd(path)
              DSNAME(Ldsn+1:) = '=' // path(:Lpath)
              Call CLecho(DSNAME(:Ldsn+Lpath+1))
            Else
              Lpath = Lenrd(path)
              DSNAME(Ldsn+1:) = '=' // path(:Lpath)
              Call cli__Fputenv(DSNAME(:Ldsn+Lpath+1))
            End If
          End If
        Else If ( 'SLEEP'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          Call CLatod(DSNAME(:Ldsn), DSEC)
          Call CLsleep(DSEC)
        Else If ( 'SUSPEND'.eq.DSNAME ) Then
          Call CLsusp
        Else If ( 'UNALIAS'.eq.DSNAME ) Then
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          If ( 0.eq.Ldsn ) Then
            Call CLaliP
          Else
            Call CLaliL( DSNAME(:Ldsn),path,Lpath )
            If ( Lpath.le.0 ) Then
              Call CLIerr(1, 'unknown alias: '//DSNAME(:Ldsn))
            Else
              Call CLaliU(DSNAME(:Ldsn))
            End If
          End If
        Else If ( 'VERSION'.eq.DSNAME ) Then
          Call CLvers
C ... call command
        Else If ( 'CALL'.eq.DSNAME ) Then
          Call CLsubp( LINE1(3:LAST1), 2, pos1 )
          pos1 = pos1 + 2
          Call CLsubrC( LINE1(pos1:LAST1) )
        Else If ( 'SUB'.eq.DSNAME ) Then
C ... sub/endsub commands with FLAG_SUB_READING=0
          Call CLpart2(LINE1(3:LAST1), 2, DSNAME, Ldsn)
          Call CLsubrI( DSNAME(:Ldsn) )
        Else If ( 'ENDSUB'.eq.DSNAME ) Then
          Call CLIerr(0, 'endsub without sub, ignored')
C ... if/elif/else/endif commands with ISKIP=0
        Else If ( 'IF'.eq.DSNAME ) Then
          If ( IF_DEPTH.ge.MAXIF ) Then
            Call CLstop
            Call CLIerr( 2, 'too many if blocks nested' )
          Else
            Call CLsubp(LINE1(3:LAST1), 2, pos1)
            pos1 = pos1 + 2
            IF_DEPTH = IF_DEPTH + 1
            IF_LUN(IF_DEPTH) = LUNLST(LUNPTR)
            ISKIP_BEFORE_IF(IF_DEPTH) = 'z'
            If ( CLifexpr( LINE1(pos1:LAST1) ) ) Then
              FLAG_IGN_ELSE(IF_DEPTH) = 'x'     ! not evaluate elif/else
            Else
              FLAG_IGN_ELSE(IF_DEPTH) = 'z'     ! evaluate elif/else
              ISKIP = 1
            End If
          End If
        Else If ( 'ELIF'.eq.DSNAME ) Then
          If ( 0 .eq. IF_DEPTH ) Then
            Call CLIerr(0, 'elif without if, ignored')
          Else If ( 'e'.eq.FLAG_IGN_ELSE(IF_DEPTH) ) Then
            Call CLIerr(0, 'elif after else, ignored')
            ISKIP = 1
          Else
            ISKIP = 1
          End If
        Else If ( 'ELSE'.eq.DSNAME ) Then
          If ( 0 .eq. IF_DEPTH ) Then
            Call CLIerr(0, 'else without if, ignored')
          Else If ( 'e'.eq.FLAG_IGN_ELSE(IF_DEPTH) ) Then
            Call CLIerr(0, 'else after else, ignored')
            ISKIP = 1
          Else
            FLAG_IGN_ELSE(IF_DEPTH) = 'e'       ! else comes, wait for endif
            ISKIP = 1
          End If
        Else If ( 'ENDIF'.eq.DSNAME ) Then
          If ( 0 .eq. IF_DEPTH ) Then
            Call CLIerr(0, 'endif without if, ignored')
          Else
            IF_DEPTH = IF_DEPTH - 1
          End If
C ... while/end/repeat/until command
        Else If ( 'WHILE'.eq.DSNAME ) Then
          If ( LOOP_DEPTH.gt.0
     &         .and. LUNPTR.ne.0
     &         .and. LUNLST(LUNPTR).eq.LOOP_LUN(LOOP_DEPTH)
     &         .and. 's'.eq.LOOP_STAT(LOOP_DEPTH) ) Then
C ... running loop
            LOOP_STAT(LOOP_DEPTH) = 'r'                 ! running loop
            Call CLsubp(LINE1(3:LAST1), 2, pos1)
            pos1 = pos1 + 2
            If ( CLifexpr( LINE1(pos1:LAST1) ) ) Then
              Continue          ! continue loop
            Else
ccc              Write (*,*) 'loop end, lun=', LOOP_LUN(LOOP_DEPTH)
              Call CLIEOF
            End If
          Else
C ... start new loop
            Call CLgetlun( LUN )
            Call CLtempnam('/tmp','cli',path)
            Lpath = Lenrd(path)
            Call CLopen( path(:Lpath),LUN,'A',Ierr,' ' )
            Call CLfdel( path(:Lpath) )
            If ( 0.ne.Ierr ) Then
              Call CLstop
              Call CLIerr( 2, 'can''t open temporary file' )
            Else If ( LOOP_DEPTH.ge.MAXLOOP ) Then
              Call CLstop
              Call CLIerr( 2, 'too many loop nested' )
            Else If ( 0.ne.CLputs(LUN, LINE0(:LAST0)) ) Then
              Call CLstop
              Call CLIerr( 2, 'error writing a temporary file' )
            Else
ccc              Write (*,*) 'path='//path(:Lpath)
ccc              Write (LUN, '(A)') LINE0(:LAST0)
              LOOP_DEPTH = LOOP_DEPTH + 1
              LOOP_CMD(LOOP_DEPTH) = 'W'
              LOOP_READ_DEPTH = LOOP_DEPTH + 1
              LOOP_CMD(LOOP_READ_DEPTH) = 'W'
              LOOP_LUN(LOOP_DEPTH) = LUN
            End If
          End If
        Else If ( 'END'.eq.DSNAME ) Then
          If ( LOOP_DEPTH.gt.0
     &         .and. LUNPTR.ne.0
     &         .and. LUNLST(LUNPTR).eq.LOOP_LUN(LOOP_DEPTH) ) Then
C ... running loop
            LOOP_STAT(LOOP_DEPTH) = 's'                 ! loop start
            Call CLfseek(LOOP_LUN(LOOP_DEPTH), 0, 0)    ! seek to file top
          Else
C ... end without while
            Call CLIerr( 0, 'end without while, ignored' )
          End If
        Else If ( 'REPEAT'.eq.DSNAME ) Then
          If ( LOOP_DEPTH.gt.0
     &         .and. LUNPTR.ne.0
     &         .and. LUNLST(LUNPTR).eq.LOOP_LUN(LOOP_DEPTH)
     &         .and. 's'.eq.LOOP_STAT(LOOP_DEPTH) ) Then
C ... running loop
            LOOP_STAT(LOOP_DEPTH) = 'r'                 ! running loop
          Else
C ... start new loop
            Call CLgetlun( LUN )
            Call CLtempnam('/tmp','cli',path)
            Lpath = Lenrd(path)
            Call CLopen( path(:Lpath),LUN,'A',Ierr,' ' )
            Call CLfdel( path(:Lpath) )
            If ( 0.ne.Ierr ) Then
              Call CLstop
              Call CLIerr( 2, 'can''t open temporary file' )
            Else If ( LOOP_DEPTH.ge.MAXLOOP ) Then
              Call CLstop
              Call CLIerr( 2, 'too many loop nested' )
            Else If ( 0.ne.CLputs(LUN, LINE0(:LAST0)) ) Then
              Call CLstop
              Call CLIerr( 2, 'error writing a temporary file' )
            Else
ccc              Write (*,*) 'path='//path(:Lpath)
ccc              Write (LUN, '(A)') LINE0(:LAST0)
              LOOP_DEPTH = LOOP_DEPTH + 1
              LOOP_CMD(LOOP_DEPTH) = 'R'
              LOOP_READ_DEPTH = LOOP_DEPTH + 1
              LOOP_CMD(LOOP_READ_DEPTH) = 'R'
              LOOP_LUN(LOOP_DEPTH) = LUN
            End If
          End If
        Else If ( 'UNTIL'.eq.DSNAME ) Then
          If ( LOOP_DEPTH.gt.0
     &         .and. LUNPTR.ne.0
     &         .and. LUNLST(LUNPTR).eq.LOOP_LUN(LOOP_DEPTH) ) Then
C ... running loop
            Call CLsubp(LINE1(3:LAST1), 2, pos1)
            pos1 = pos1 + 2
            If ( CLifexpr( LINE1(pos1:LAST1) ) ) Then
ccc              Write (*,*) 'loop end, lun=', LOOP_LUN(LOOP_DEPTH)
              Call CLIEOF
            Else
              LOOP_STAT(LOOP_DEPTH) = 's'               ! loop start
              Call CLfseek(LOOP_LUN(LOOP_DEPTH), 0, 0)  ! seek to file top
            End If
          Else
C ... until without repeat
            Call CLIerr( 0, 'until without repeat, ignored' )
          End If
        Else            ! If ( 'ABORT'.eq.DSNAME )
          Call CLIERR( 2, 'command not found: @.'// DSNAME(:Ldsn) )
        End If
      Else              ! If ( (LINE1(2:2) .eq. '.') ..
        pos1 = Index(LINE1(:LAST1),'=')
        pos2 = Index(LINE1(:LAST1),' ')
        If( pos1.gt.0 .and. (pos2.eq.0 .or. pos2.gt.pos1) ) Then
C ... symbol assign                     @symbol=...
          If( LINE1(pos1+1:pos1+1) .eq. BQUOTE ) Then
C ... command substition                @symbol=`command args ...`
            pos2 = Index(LINE1(pos1+2:),BQUOTE)
            If ( pos2.eq.0 ) Then
              pos2 = LAST1
            Else
              pos2 = pos1 + pos2
            End If
ccc            Write(*,*) 'LINE1(:LAST1)=[',LINE1(:LAST1),']'
ccc            Write(*,*) 'LINE1(pos1+2:pos2)=[',LINE1(pos1+2:pos2),']'
            Call cli__Fexecp( LINE1(pos1+2:pos2), DSNAME, Ldsn )
            Call CLseti( '?', CLexitcode() )
          Else If ( LINE1(pos1+1:pos1+1) .eq. '?' ) Then
C ... read from terminal input          @symbol=?prompt
            Call CLrdln( LINE1(pos1+2:LAST1+1), DSNAME, Ldsn )
            if ( Ldsn .le. 0 ) Ldsn = -1 ! ignore null input
          Else
C ... normal symbol assign              @symbol=value, or print list for @=
            DSNAME = LINE1(pos1+1:LAST1)
            Ldsn = LAST1 - pos1
          End If
          If ( Ldsn.ge.0 ) Then
            If ( Index(DSNAME(:Ldsn), '!').gt.0 ) Then
              Call CLinco( DSNAME,Ldsn )
ccc                Call CLIerr(0,"symbol contains an exclamation mark '!'")
            End if
            Call CLsetv( LINE1(2:pos1-1), DSNAME(:Ldsn) )
            If ( Ldsn.gt.0 ) Then
              If ( ' '.eq.DSNAME(Ldsn:Ldsn) ) Then
                Call CLIerr(0, 'symbol contains a space at the tail')
              End If
            End If
          End If
        Else
C ... redirect input to indirect file   @ file
          If( LAST1.gt.2 .and. LINE1(2:2).eq.'@' ) Then
C ... single step execution             @@ file
            DSNAME = LINE1(3:LAST1)
            LAST1 = LAST1 - 1
            LINE1(2:LAST1) = DSNAME
            Opt_DEBUG = 1
          End If
          If ( LPTRSV.gt.0 ) LUNPTR = LPTRSV
          If ( LUNPTR .ge. MAXNST ) Then
            Call CLIERR( 2,'command files nest too deep' )
ccc            Call CLstop
            LOGON = .FALSE.
            Goto 999
          End If
ccc            LOGON = .TRUE.
          Call CLsubp( LINE1(2:LAST1), 1, pos1 )
          pos1 = pos1 + 1
          LUNPTR = LUNPTR + 1
          Call CLgetlun( LUN )
          LUNLST(LUNPTR) = LUN
ccc            FILIST(LUNPTR) = LINE1(pos1:LAST1)
          FILINE(LUNPTR) = 0
C ... execute command output            @ command |
          If ( LINE1(LAST1:LAST1).eq.'|' ) Then
            Call CLargs( LINE1(pos1:LAST1),' ',Ldsn )
            Call CLtempnam('/tmp','cli',path)
            Lpath = Lenrd(path)
            DSNAME = LINE1(pos1:LAST1-1)
            Ldsn = Lenrd(DSNAME)
ccc              Write (*,*) 'exec "' // DSNAME(:Ldsn) // '"'
            Call CLexec( DSNAME(:Ldsn),' ',path(:Lpath) )
            Call CLopen( path(:Lpath),LUN,'R',Ierr,' ' )
            Call CLfdel( path(:Lpath) )
          Else                  ! @ file
            Call CLargs( LINE1(pos1:LAST1),DSNAME,Ldsn )
            Call CLfindpath( DSNAME(:Ldsn), '.com', path, Lpath )
            If ( Lpath.gt.0 ) Then
              Call CLopen( path(:Lpath),LUN,'R',Ierr,' ' )
              If ( Ierr.eq.0 .and. DSNAME(:Ldsn).ne.path(:Lpath) ) Then
                Call CLIerr(0, "entering '"//path(:Lpath)//"'")
              End If
            Else
              Ierr = -1
              Call CLIerr( 2, "can't open '"//DSNAME(:Ldsn)//"'")
            End if
          End If
          If( Ierr .ne. 0 ) Then
            Call CLfreelun( LUN )
            If( LUNPTR .gt. 0 ) Then
              LUNPTR = LUNPTR - 1
            End If
            LOGON = .FALSE.
            Goto 999
          End If
        End If
      End If
C
 999  Continue
C ... Write log when success
      If( LOGON .and. INHLOG.eq.0 ) Then
        Call CLWLOG( LINE1(1:LAST1) )
      End If
C
      Return
      End
