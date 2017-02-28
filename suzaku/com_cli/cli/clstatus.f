C
C File: clstatus.f
C Description: show current CLI status
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     22-Feb-2005 Y.ISHISAKI, clstatus.f created for @.status & @.show
C     02-Mar-2005 Y.ISHISAKI, add 'PATH' support
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C Public: CLstatus
C
C ----------
C   CLstatus   ... show current status inside CLI
C ----------
       Subroutine CLstatus( OPT )
       Implicit NONE
C input
       Character * (*)  OPT
C common
       include 'clidef.inc'
       include 'clflag.inc'
       include 'clunit.inc'
       include 'clalir.inc'
       include 'clmacr.inc'
C local
      Integer  I, L, LL
      Character * (MAXIF)  S1
      Character * (MAXLOOP)  S2
      Character * (LINSIZ)  MSG
C function
      Integer  Lenrd
C begin
      Call CLstrupc( LEN(OPT), OPT )
C
      If ( ' '.eq.OPT .or. 'VERS'.eq.OPT(:4) ) Then
        Call CLvers
      End If
C
      If ( ' '.eq.OPT .or. 'PROC'.eq.OPT(:4) ) Then
 10     Format(' [',I1,'] (',A,') LUN=',I2,', line',I4,': ',A)
        Do I = 1, LPTRSV
          L = Lenrd(ARGLST(I))
          Write (*, 10) I, 'Suspend', LUNLST(I), FILINE(I),
     &         ARGLST(I)(:L)
        End Do
        Write (*, 10)   0, 'Running', LUNLST(0), FILINE(0),
     &       '*** console input ***'
        Do I = LPTRSV+1, LUNPTR
          L = Lenrd(ARGLST(I))
          Write (*, 10) I, 'Running', LUNLST(I), FILINE(I),
     &         ARGLST(I)(:L)
        End Do
      End If
C
      If ( ' '.eq.OPT ) Write (*, *)
C
      If ( ' '.eq.OPT .or. 'FLOW'.eq.OPT(:4) ) Then
        MSG = ' '
        If ( IF_DEPTH.le.0 ) Then
          Write (MSG,'(A,I3)') 'IF depth =', IF_DEPTH
        Else
          Do I = 1, IF_DEPTH
            S1(I:I) = FLAG_IGN_ELSE(I)
          End Do
          Do I = IF_DEPTH+1, MAXIF
            S1(I:I) = '-'
          End Do
          Write (MSG,'(1X,A,I3,4X,A)')
     &         'IF depth =', IF_DEPTH, S1
        End If
        If ( LOOP_DEPTH.le.0 ) Then
          Write (MSG(33:),'(A,I3)') 'LOOP depth =', LOOP_DEPTH
        Else
          Do I = 1, LOOP_DEPTH
            S2(I:I) = LOOP_CMD(I)
          End Do
          Do I = LOOP_DEPTH+1, MAXLOOP
            S2(I:I) = '-'
          End Do
          Write (MSG(33:),'(1X,A,I3,4X,A)')
     &         'LOOP depth =', LOOP_DEPTH, S2
        End If
        L = Lenrd(MSG)
        Write (*,*) MSG(:L)
      End If
C
      If ( ' '.eq.OPT ) Write (*, *)
C
      If ( ' '.eq.OPT .or. 'ALIAS'.eq.OPT(:5) ) Then
 20     Format(I5,A,I4,A)
        Write (*, 20) NALI, '/', MAXALI,
     &       ' aliases are defined.     Try "@.alias" to see.'
      End If
C
      If ( ' '.eq.OPT .or. 'VAR'.eq.OPT(:3) ) Then
        Write (*, 20) NMAC, '/', MAXMAC,
     &       ' variables are defined.   Try "@=" to see.'
      End If
C
      If ( ' '.eq.OPT .or. 'LABEL'.eq.OPT(:5) ) Then
        Write (*, 20) NLABEL, '/', MAXLABEL,
     &       ' labels are defined.      Try "@:" to see.'
      End If
C
      If ( ' '.eq.OPT .or. 'SUB'.eq.OPT(:3) ) Then
        Write (*, 20) NSUB, '/', MAXSUB,
     &       ' subroutines are defined. Try "@.sub" to see.'
      End if
C
      If ( ' '.eq.OPT ) Write (*, *)
C
      If ( ' '.eq.OPT .or. 'OPT'.eq.OPT(:3) ) Then
        L = 1
C
        MSG(L:) = 'ALIAS='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_ALIAS ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'AT='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_AT ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'BREAK='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_BREAK ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'DEBUG='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_DEBUG ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'ECHO='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_ECHO ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'LOG='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.ILOG ) Then
          LL = Lenrd( LOGFILE )
          MSG(L:) = 'ON ['//LOGFILE(:LL)//']'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG )
C
        Write (*,*) MSG(:L)
        L = 1
C
        MSG(L:) = 'DOLLAR($symbol)='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_DOLLAR ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'PERCENT(%*,%0-%9)='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_PERCENT ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'BSLASH(concat by '''//BSLA//''')='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_BSLASH ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG )
C
        Write (*,*) MSG(:L)
        L = 1
C
        MSG(L:) = 'INCO(! or @! ...)='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_INCO ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG ) + 3
C
        MSG(L:) = 'HISTORY(![-]n[:n])='
        L = Lenrd( MSG ) + 1
        If ( 0.ne.Opt_HISTORY ) Then
          MSG(L:) = 'ON'
        Else
          MSG(L:) = 'OFF'
        End If
        L = Lenrd( MSG )
C
        Write (*,*) MSG(:L)
        L = 1
C
      End If
C
      If ( ' '.eq.OPT .or. 'PATH'.eq.OPT(:4) ) Then
        Write (*, '(1X,A,A,A)') "PATH='",
     &       SEARCH_PATH(:LSEARCH_PATH), "'"
      End if
C
      If ( ' '.eq.OPT ) Then
        Write (*, *)
        Write (*,*) 'Try "@.help" for detail.'
      End If
C
      Return
      End
