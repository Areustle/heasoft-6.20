C
C File: cllog.f
C Description: CLI Log File Support Routines
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     21-Feb-2005 Y.ISHISAKI, CLWLOG & CLILOG moved from clgetl.f
C     21-Feb-2005 Y.ISHISAKI, CLIMEM deleted, CLMKLOG,CLCLLOG,CLRdOUT created
C     22-Feb-2005 Y.ISHISAKI, use CLgetlun(), CLfreelun() for KLUN
C     26-Feb-2005 Y.ISHISAKI, LOGFILE declaration move to unitrd.inc
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C Public: CLMKLOG, CLCLLOG, CLWLOG, CLILOG, CLRdOUT
C
C ----------
C   CLMKLOG   ... make log file
C ----------
      Subroutine CLMKLOG( FILE, IERR )
      Implicit NONE
C input
      Character * (*)  FILE
C output
      Integer  IERR
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C local
      Integer  L
C function
      Integer  Lenrd
C begin
      IERR = 0
      If ( 0.eq.LEN( FILE ) ) Then
        If ( 0.eq.ILOG ) Then
          Call CLIERR( 0,'log file is not opend' )
        Else
          L = Lenrd( LOGFILE )
          Call CLIERR( 0, "log file '"//LOGFILE(:L)//"' is being opend")
        End If
        Return
      End If
C
      If( 0.ne.ILOG ) Then
        Call CLIERR( 2,'log file already opened' )
        Return
      End If
C
      LOGFILE = FILE
      L = Lenrd( LOGFILE )
      Call CLgetlun( KLUN )
      Call CLopen( LOGFILE(:L),KLUN,'W',IERR,' ' )
      If( IERR .eq. 0 ) Then
        Call CLIerr( 0, "log file '"//LOGFILE(:L)//"' is created" )
        ILOG = 1
      Else
        Call CLfreelun( KLUN )
      End If
C
      Return
      End
C
C ----------
C   CLCLLOG   ... close log file
C ----------
      Subroutine CLCLLOG( IERR )
      Implicit NONE
C output
       Integer  IERR
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C local
      Integer  L
C function
      Integer  Lenrd
C
      If ( 0.eq.ILOG ) Then
        Call CLIERR( 1,'log file is not opend' )
        Return
      End If
C
      L = Lenrd( LOGFILE )
      Call CLclos( KLUN,IERR )
      Call CLfreelun( KLUN )
      If( IERR .ne. 0 ) Then
        Call CLIerr( 2,"can't close log file '"//LOGFILE(:L)//'"' )
      Else
        Call CLIerr( 0, "log file '"//LOGFILE(:L)//"' closed")
      End If
      ILOG = 0
      LOGFILE = ' '
C
       Return
       End
C
C ----------
C   CLWLOG   ... write command input on log file
C ----------
       Subroutine CLWLOG( LINE )
       Implicit NONE
C input
       Character * (*)  LINE
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C begin
      If( ILOG.eq.1 .and. LUNIN.eq.5 .and. ICOM.eq.0 ) Then
        Write( KLUN,'(A)' ) LINE
      End If
C
      Return
      End
C
C ----------
C   CLILOG ... temporary enable/disable logging command input
C ----------
       Integer Function CLILOG( SWITCH )
C input
       Integer * 4  SWITCH
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C begin
      If( SWITCH .eq. 0 ) Then
        INHLOG = 1
      Else
        INHLOG = 0
      End If
      If( ( ILOG.eq.1 ) .and. ( LUNPTR.eq.0 .or. ICOM.eq.1 ) ) Then
        CLILOG = 1
      Else
        CLILOG = 0
      End If
      Return
      End
C
C ----------
C   CLRdOUT ... redirect subsequent output to file
C ----------
       Subroutine CLRdOUT( FILE, IERR )
       Implicit NONE
C input
       Character * (*)  FILE
C output
       Integer  IERR
C common
       include 'clidef.inc'
       include 'clflag.inc'
       include 'clunit.inc'
C local
       Integer  status
       Character * (LINSIZ)  MSG
C function
      Integer  CLstricmp
C begin
      IERR = 0
      If ( 0.eq.LEN( FILE ) .or. FILE.eq.'*' .or.
     &     0.eq.CLstricmp('TERMINAL',FILE) ) Then
        Call CLsyso(6,'SYS$OUTPUT','UNKNOWN',status)
      Else If ( 0.eq.CLstricmp('NULLFILE', FILE) ) Then
        Call CLsyso(6,'NLA0:','UNKNOWN',status)
      Else
        MSG = "stdout redirected to '"//FILE//"', type '@>' to back"
        Call CLIERR( 0, MSG )
        Call CLopen( FILE,6,'W',IERR,' ' )
        If( IERR .ne. 0 ) Then
          Call CLsyso(6,'SYS$OUTPUT','UNKNOWN',status)
          Call CLIERR( 2,'can''t redirect standard output' )
        End If
      End If
C
      Return
      End
