C
C File: cllab.f
C Description: CLI Label Support Routines
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     20-Feb-2005 Y.ISHISAKI, created for label & goto support
C     22-Feb-2005 Y.ISHISAKI, ignore labels inside comments in CLlabJ
C     02-Mar-2005 Y.ISHISAKI, cancel if blocks only inside current proc.
C     09-Mar-2005 Y.ISHISAKI, use CLftell/Fseek() instead of Ftell/Fseek()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C Public: CLlabP, CLlabR, CLlabF, CLlabJ
C
C ... print label
      Subroutine CLlabP
      Implicit None
C common
      include 'clidef.inc'
      include 'clunit.inc'
C local
      Integer  I, L, LPOS
      Character * 16  POSVAL
C function
      Integer  Lenrd
C begin
      If ( 0.eq.NLABEL ) Then
        Call CLIerr( 0, 'no label is defined' )
      Else
        Do I = 1, NLABEL        ! show label info for @:
          L = LENLBL(I)
          Call CLitoa(POSLBL(I), POSVAL)
          LPOS = LENRD(POSVAL)
          Write (*,'(A,I2,A,A)') ':'//LABELS(I)(:L)//'  LUN=',LUNLBL(I),
     &         '  POS=',POSVAL(:LPOS)
        End Do
      End If
C
      Call CLstatus('LABEL')
C
      Return
      End
C
C
C ... register label
      Subroutine CLlabR( LINE, FLAG_LIST )
      Implicit None
C input
      Character * (*)  LINE
      Integer  FLAG_LIST
C common
      Include 'clidef.inc'
      Include 'clunit.inc'
C local
      Character * 64  label
      Integer  I, LL, L, LUN, POS, LAST
C function
      Integer  Lenrd, CLftell
C begin
      LL = 0
      LAST = LEN(LINE)
      Do I = 1, LAST            ! skip head SPACES & TABS
        If ( SPACE .ne. LINE(I:I) .and. TAB .ne. LINE(I:I) ) Then
          Call CLpart(LINE(I:LAST), 1, label)
          LL = Lenrd(label)
          Goto 10
        End If
      End Do
 10   Continue
C
      If ( ':' .eq. label(1:1) ) Then
        LL = 0                  ! show label info for @:
      End If
C
ccc      Write (*,*) 'CLlabr: LL=', LL, ', label=', label(:LL)
      If ( 0 .eq. LL ) Then
        If ( 0 .ne. FLAG_LIST ) Call CLlabP
        Return
      End If
C
      LUN = LUNLST(LUNPTR)
      If ( 0.eq.LUN ) Then
        Call CLIerr(0, 'labels are ignored in console mode')
        Return
      End If
      POS = CLftell(LUN)
C
      If ( NLABEL .ge. MAXLABEL ) Then
        Call CLIerr(0, 'too many labels, ignored')
        Return
      End If
C
      Do I = 1, NLABEL
        L = LENLBL(I)
ccc        Write (*,*) 'label=', label, ', LABELS(I)=', LABELS(I)(:L)
        If ( label(:LL).eq.LABELS(I)(:L) .and. LUN.eq.LUNLBL(I) ) Then
          If ( POS .eq. POSLBL(I) ) Return      ! already marked
          Call CLIerr(0, 'found duplicated label, replace it')
          POSLBL(I) = POS
          Return
        End If
      End Do
C
      NLABEL = NLABEL + 1
      LABELS(I) = label(:LL)
      LENLBL(I) = LL
      LUNLBL(I) = LUN
      POSLBL(I) = POS
C
      Return
      End
C
C
C ... flush label
      Subroutine CLlabF
      Implicit None
C common
      include 'clidef.inc'
      include 'clunit.inc'
C local
      Integer  I, J, LUN
C begin
      If ( 0.eq.NLABEL ) Return
C
      If ( 0.eq.LUNPTR ) Then
        NLABEL = 0
        Return
      End If
C
      Do I = NLABEL, 1, -1
        LUN = LUNLBL(I)
        Do J = 1, LUNPTR
          If ( LUN .eq. LUNLST(J) ) Then
            Return
          End If
        End Do
        NLABEL = NLABEL - 1
      End Do
C
      Return
      End
C
C
C ... jump to specified label
      Subroutine CLlabJ( label )
      Implicit None
C input
      Character * (*)  label
C common
      Include 'clidef.inc'
      Include 'clflag.inc'
      Include 'clunit.inc'
C local
      Integer  I, L, LUN, POS, LAST, PASS, IERR, LPOS
      Character * 16  POSVAL
      Character * (LINSIZ)  LINE
      Logical  INSIDE_COMMENT
C function
      Integer  CLGETS, CLftell, Lenrd
C begin
 5    Continue
      If ( 0.eq.LUNPTR ) Then
        Call CLIerr(0, 'not usable in console mode, ignored')
        Return
      End If
cccC ... check for while/repeat loop
ccc      If ( LOOP_DEPTH.gt.0 ) Then
cccC ... exit from loop
ccc         Do I = LOOP_DEPTH, 1, -1
ccc            Call CLIEOF
ccc            Call CLlabF
ccc         End Do
ccc      End If
C
      PASS = 1
      INSIDE_COMMENT = .FALSE.
      L = LEN(label)
C
      LUN = LUNLST(LUNPTR)
 10   Continue
C
      Do I = NLABEL, 1, -1
        L = LENLBL(I)
        If ( LUN.eq.LUNLBL(I) .and. label.eq.LABELS(I)(:L) ) Then
          Call CLfseek(LUN, POSLBL(I), 0)
          Goto 20
        End If
      End Do
C
      If ( 1.ne.PASS ) Goto 999
      PASS = PASS + 1
      POS = CLftell(LUN)
      Call CLfseek(LUN, 0, 0)
      Do While (.true.)
C ... get one Logical line
        IERR = CLGETS( LUN, LINE, LAST )
ccc        Write (*,*) 'LINE='//LINE(:LAST)
C ... check If this line is EOF
        If( IERR .lt. 0 ) Goto 10       ! search label again
        If ( LINE(1:3) .eq. '@*/' ) Then        ! check comment end
          INSIDE_COMMENT = .FALSE.
        Else If ( INSIDE_COMMENT ) Then
          Continue                              ! skip line inside comment
        Else If ( LINE(1:3) .eq. '@/*' ) Then   ! check comment start
          INSIDE_COMMENT = .TRUE.
        Else If ( LINE(1:2) .eq. '@:' ) Then
ccc          Write (*,*) 'LINE='//LINE(:LAST)
          Call CLlabR( LINE(3:LAST), 0 )
          I = NLABEL
          L = LENLBL(I)
          If ( LUN.eq.LUNLBL(I) .and. label.eq.LABELS(I)(:L) ) Then
            Goto 20
          End If
        End If
      End Do
C
 20   Continue                  ! label found
      Call CLitoa(POSLBL(I), POSVAL)
      LPOS = Lenrd(POSVAL)
      Write (*,'(A,I2,A,A)') ':'//LABELS(I)(:L)//'  LUN=',LUNLBL(I),
     &     '  POS=',POSVAL(:LPOS)
C
      Do While ( IF_DEPTH.gt.0 .and. IF_LUN(IF_DEPTH).eq.LUN )
        IF_DEPTH = IF_DEPTH - 1         ! cancel IF/ELIF/ELSE/ENDIF block
      End Do
C
      Return
C
 999  Continue
ccc
ccc 22-Feb-2005 Y.ISHISAKI
ccc Fseek() after reaching EOF do not work properly on Linux,
ccc and probably also other system, e.g. OSF1
ccc Next READ() will always return EOF
ccc
ccc      Call Fseek(LUN, 0, POS)
ccc      IERR = CLGETS( LUN,LINE,LAST )
ccc      Write (*,*) 'POS=', POS, ' LAST=', LAST, ' IERR=', IERR
ccc      Write (*,*) 'LINE='//LINE(:LAST)
      Call CLIerr(0, 'label not found, reaching end')
      Call CLIEOF
      If ( 0.ne.LUNPTR ) Then
        Goto 5                  ! try again
      End If
C
      Return
C
      Return
      End
