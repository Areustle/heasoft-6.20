C
C File: cliset.f
C Description: set CLI options
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     22-Feb-2005 Y.ISHISAKI, cliset.f created for @.set
C     26-Feb-2005 Y.ISHISAKI, rename ISNGL -> Opt_DEBUG
C
C Public: CLiset
C
C ----------
C   CLIset   ... set CLI options
C ----------
       Subroutine CLIset( OPT, VAL )
       Implicit NONE
C input
       Character * (*)  OPT, VAL
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C local
      Logical  LBREAK
C function
      Logical  CLbreak
C begin
      If ( ' '.eq.OPT ) Then
        Call CLstatus( 'OPT' )
        Return
      End If
C
      Call CLstrupc( LEN(OPT), OPT )
      Call CLstrupc( LEN(VAL), VAL )
C
      If ( 'AT'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_AT = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_AT = 0
        Else If ( 0.ne.Opt_AT ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'BREAK'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          LBREAK = CLbreak( 1, 0 )
        Else If ( 'OFF'.eq.VAL ) Then
          LBREAK = CLbreak( 0, 0 )
        Else If ( 0.ne.Opt_BREAK ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'DEBUG'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_DEBUG = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_DEBUG = 0
        Else If ( 0.ne.Opt_DEBUG ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'ECHO'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_ECHO = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_ECHO = 0
        Else If ( 0.ne.Opt_ECHO ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'LOG'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Write (*,*) 'Try "@(file" or "@>" to start logging'
        Else If ( 'OFF'.eq.VAL ) Then
          Write (*,*) 'Try "@)" or "@>" to end logging'
        Else If ( 0.ne.ILOG ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'DOLLAR'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_DOLLAR = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_DOLLAR = 0
        Else If ( 0.ne.Opt_DOLLAR ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'PERCENT'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_PERCENT = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_PERCENT = 0
        Else If ( 0.ne.Opt_PERCENT ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'BSLASH'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_BSLASH = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_BSLASH = 0
        Else If ( 0.ne.Opt_BSLASH ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'INCO'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_INCO = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_INCO = 0
        Else If ( 0.ne.Opt_INCO ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      Else If ( 'HISTORY'.eq.OPT ) Then
        If ( 'ON'.eq.VAL ) Then
          Opt_HISTORY = 1
        Else If ( 'OFF'.eq.VAL ) Then
          Opt_HISTORY = 0
        Else If ( 0.ne.Opt_HISTORY ) Then
          Write (*,'(A,A)') OPT, '=ON'
        Else
          Write (*,'(A,A)') OPT, '=OFF'
        End If
      End If
C
      Return
      End
