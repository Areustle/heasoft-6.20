C
C File: clsubr.f
C Description: CLI subroutine support routines
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     02-Mar-2005 Y.ISHISAKI, created for subroutine support
C     09-Mar-2005 Y.ISHISAKI, use CLfseek() instead of Fseek()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C Public: CLsubrP
C
C ----------
C   CLsubrP     ... print subroutine status
C ----------
      Subroutine CLsubrP
      Implicit NONE
C common
      include 'clidef.inc'
      Include 'clunit.inc'
C local
      Integer  I, L
C begin
      If ( 0.eq.NSUB ) Then
        Call CLIerr( 0, 'no subroutine is defined' )
      Else
        Do I = 1, NSUB
          L = LSUB_NAME(I)
          Write (*, '(1X,I2,A,A,A,I2,A,A)')
     &         I, ': sub ', SUB_NAME(I)(:L), ' (LUN=', SUB_LUN(I),
     &         '), defined in ', SUB_DEFARG(I)(:LSUB_DEFARG(I))
        End do
      End If
C
      Call CLstatus('SUB')
C
      Return
      End
C
C ----------
C   CLsubrI     ... start subroutine install
C ----------
      Subroutine CLsubrI( NAME )
      Implicit NONE
C common
      Include 'clidef.inc'
      Include 'clunit.inc'
C input
      Character * (*)  NAME
C local
      Integer  I, J, Lpath, Ierr, LUN
      Character * (LINSIZ)  path
C function
      Integer  Lenrd
C begin
      If ( Len(NAME).eq.0 ) Then
        Call CLsubrP
        Return
      Else If ( NSUB.ge.MAXSUB ) Then
        Call CLstop
        Call CLIerr( 2, 'too many subroutines' )
        Return
      End if
C
      Do I = 1, NSUB
        If ( SUB_NAME(I)(:LSUB_NAME(I)).eq.NAME ) Then
          Call CLIerr(0, 'duplicated routine name, replace it')
          Call CLclos( SUB_LUN(I), Ierr )
          Call CLfreelun( SUB_LUN(I) )
          Do J = I, NSUB - 1
            SUB_LUN(J) = SUB_LUN(J+1)
            SUB_DEFLUN(J) = SUB_DEFLUN(J+1)
            SUB_NAME(J) = SUB_NAME(J+1)
            LSUB_NAME(J) = LSUB_NAME(J+1)
            SUB_DEFARG(J) = SUB_DEFARG(J+1)
            LSUB_DEFARG(J) = LSUB_DEFARG(J+1)
          End Do
          NSUB = NSUB - 1
        End If
      End Do
C
      Call CLgetlun( LUN )
      Call CLtempnam('/tmp','cli',path)
      Lpath = Lenrd(path)
      Call CLopen( path(:Lpath),LUN,'A',Ierr,' ' )
      Call CLfdel( path(:Lpath) )
      If ( 0.ne.Ierr ) Then
        Call CLstop
        Call CLIerr( 2, 'can''t open temporary file' )
        Return
      End If
C
      NSUB = NSUB + 1
      SUB_LUN(NSUB) = LUN
      FLAG_SUB_READING = 1
      SUB_DEFLUN(NSUB) = LUNLST(LUNPTR)
      SUB_NAME(NSUB) = NAME
      LSUB_NAME(NSUB) = Len(NAME)
      If ( LUNPTR.eq.0 ) Then
        SUB_DEFARG(NSUB) = '*** console input ***'
        LSUB_DEFARG(NSUB) = Lenrd( SUB_DEFARG(NSUB) )
      Else
        SUB_DEFARG(NSUB) = ARGLST(LUNPTR)
        LSUB_DEFARG(NSUB) = LARGS(LUNPTR)
      End If
C
      Return
      End
C
C ----------
C   CLsubrC     ... call subroutine
C ----------
      Subroutine CLsubrC( ARG )
      Implicit NONE
C common
      include 'clidef.inc'
      Include 'clunit.inc'
C input
      Character * (*)  ARG
C local
      Integer  I, L, I0, I1, Ierr
C function
      Integer  Lunrd
C begin
      Call CLsubp2( ARG, 1, I0, I1 )
      If ( I0.eq.0 ) Then
        Call CLIerr( 0, 'no subroutine name is specified, ignored' )
        Return
      End If
C
      Do I = NSUB, 1, -1
        If ( SUB_NAME(I)(:LSUB_NAME(I)).eq.ARG(I0:I1) ) Then
          Call CLfseek( SUB_LUN(I), 0, 0 )
          Ierr = Lunrd( SUB_LUN(I) )
          If ( Ierr.eq.0 ) Then
            Call CLargs( ARG, ' ', L )
          End if
          Return
        End If
      End Do
C
      Call CLIerr( 2, 'no matching subroutine name' )
C
      Return
      End
