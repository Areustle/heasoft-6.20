C   31/03/87 806250022  MEMBER NAME  CLARGS   (FORT)     M  FORTRAN
C+
C File: clargs.f (formerly cfargs.f)
C Description: Routines to handle command procedure arguments
C Author: A.Shirahashi, Univ. of Tokyo
C Date: 31-Mar-1987
C
C History:
C     22-Feb-2005 Y.ISHISAKI, renamed from 'cfargs.f' into 'clargs.f'
C     22-Feb-2005 Y.ISHISAKI, routine name also CFarg* -> CLarg*
C     22-Feb-2005 Y.ISHISAKI, CLargR moved from clgetl.f
C     23-Feb-2005 Y.ISHISAKI, CLargS just remember line in ARGLST
C     23-Feb-2005 Y.ISHISAKI, CLARGP,CLARGI,CLPUSH,CLPOP removed
C     23-Feb-2005 Y.ISHISAKI, add new args LINE, L for CLargR()
C     25-Feb-2005 Y.ISHISAKI, change spec of command line arguments,
C               Now, command arguments are only set to CLI variables,
C               $%0, $%1, ..., $%9   for each words,
C               $%* for the whole line,
C               $%# for number of arguments.
C
C Public:
C     CLargS   ... set arguments for command procedure
C     CLargR   ... replace arguments (obsolete)
C
C ----------
C   CLargS   ... set arguments for command procedure
C ----------
      Subroutine CLargS( LINE,DSNAME,LDSN )
      Implicit None
C input
      Character * (*)  LINE
C output
      Character * (*)  DSNAME
      Integer   *  4   LDSN
C common
      include 'clidef.inc'
      include 'clunit.inc'
C local
      Integer * 4  I, N, L, I0, I1
      Character * 16  KEY, NUM
C begin
      N = 0
      KEY = '%*'
      Call CLsetv( KEY, LINE )
C
      Do I = 0, 9
        Call CLsubp2( LINE, I+1, I0, I1 )       ! cut out each words
C
        If ( 0.eq.I .and. Len(DSNAME).gt.1 ) Then
          DSNAME = LINE(I0:I1)          ! return 1st argument = $%0
          If ( 0.eq.I0 ) Then
            LDSN = 0
          Else
            LDSN = I1 - I0 + 1
          End if
ccc          Write (*,*) 'I0=', I0, ' I1=', I1, ' DSNAME=',DSNAME(:Ldsn)
        End if
C
        Call CLitoa( I, KEY(2:) )       ! make a string, '%0' - '%9'
        If ( 0.eq.I0 ) Then
          L = 0
          Call CLsetv( KEY, LINE(:L) )  ! unset variable, with null string
        Else
          N = I + 1
          Call CLsetv( KEY, LINE(I0:I1) )
        End If
      End Do
C
      Call CLitoa( N, NUM )
      Call CLsetv( '%#', NUM )
C
      ARGLST(LUNPTR) = LINE
      LARGS(LUNPTR) = Len(LINE)
ccc      Write (*,*) 'LINE=', LINE
C
      Return
      End
C
C --------
C  CLargR   ... replace arguments (obsolete)
C --------
      Subroutine CLargR( LINE0, L0, LINE, L )
      Implicit None
C common
      Include 'clidef.inc'
      Include 'clunit.inc'
C input
      Character * (*)  LINE0, LINE
      Integer  L0, L
C local
      Integer  I, IARG, LARG, IS, IE
C begin
      I = 0
      L = 0
ccc      Write (*,10) 'L0=', L0, ' LINE0="', LINE0(:L0), '"'
ccc 10   Format(A,I4,A,A,A)
      Do While ( I.lt.L0 )
        I = I + 1
        L = L + 1
        If ( L.gt.Len(LINE) ) Then
          Goto 999
        End If
        LINE(L:L) = LINE0(I:I)
        If ( '%'.eq.LINE0(I:I) ) Then
          IARG = INDEX( '0123456789', LINE0(I+1:I+1) )
          If ( '%'.eq.LINE0(I+1:I+1) ) Then
            I = I + 1
          Else If ( '*'.eq.LINE0(I+1:I+1) ) Then
            LARG = LARGS(LUNPTR)
            If ( 0.ne.LARG ) Then
              LINE(L:) = ARGLST(LUNPTR)(:LARG)
            End If
            I = I + 1
            L = L + LARG - 1
          Else If ( IARG.gt.0 ) Then
            Call CLsubp2( ARGLST(LUNPTR), IARG, IS, IE )
            If ( 0.ne.IS ) Then
ccc       Write (*,*) 'IARG=', IARG, ' ARG="'//ARGLST(LUNPTR)(IS:IE)//'"'
              LARG = IE - IS + 1
              LINE(L:) = ARGLST(LUNPTR)(IS:IE)
              I = I + 1
              L = L + LARG - 1
              If ( L.ge.Len(LINE) ) Then
                L = Len(LINE)
                Goto 999
              End If
ccc              Write (*,*) 'L=', L, ' LINE="'//LINE(:L)//'"'
            End If
          End If
        End If
      End Do
C
 999  Continue
C
ccc      Write (*,10) ' L=', L, '  LINE="', LINE(:L), '"'
C
      Return
      End
